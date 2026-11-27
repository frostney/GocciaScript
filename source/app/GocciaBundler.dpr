program GocciaBundler;
{ GocciaBundler: the command line, configs, output plan and collision checks (the
  base class and GocciaBundler.dpr's planning), each file compiled by a
  TGocciaEngine on a thread of its own (Goccia.Embed CompileArtifact) into a
  bytecode file (Goccia.Artifact), which GocciaRunner runs as an entry or
  imports with --prefer-bytecode. `./build.pas bundler` builds it as
  build/GocciaBundler. }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  HostOutputFiles,
  TextSemantics,
  TimingUtils,

  CLI.ConfigFile,
  CLI.Options,
  Goccia.Application,
  Goccia.Capabilities,
  Goccia.CLI.Application,
  Goccia.CLI.FileSettings,
  Goccia.CLI.FileEngine,
  Goccia.CLI.Options,
  Goccia.CLI.Permissions,
  Goccia.CLI.SourceMaps,
  Goccia.CLI.Stdin,
  Goccia.FileExtensions,
  Goccia.JSX.Transformer,
  Goccia.ScriptLoader.Input,
  Goccia.Terminal.Colors,
  Goccia.TextFiles,
  Goccia.Embed,
  Goccia.EngineFault,

  FileUtils,
  Goccia.ThreadCleanupRegistry;

type
  TBundlerOutput = record
    InputFile: string;
    OutputPath: string;
    MapOutputPath: string;
    CreateParents: Boolean;
  end;

  { a file that did not compile: Message the report as GocciaRunner prints it, Detail the error alone (a
    parallel run's "Error in" line) }
  EGocciaBundleFailure = class(Exception)
  public
    Detail: string;
  end;

  TBundlerApp = class(TGocciaCLIApplication)
  private
    FOutputPath: TStringOption;
    FSourceMap: TStringOption;
    FPlan: array of TBundlerOutput;

    procedure WriteSourceMapIfEnabled(const ASource: TStringList; const AOutput: TBundlerOutput);
    procedure EmitBytecode(const ASource: TStringList; const AOutput: TBundlerOutput; AQuiet: Boolean);
    function ResolveOutputPath(const AInputFile, AInputRoot: string): string;
    procedure AddPlannedFiles(const AFiles: TStringList;
      const AInputRoot: string; const AStdin: Boolean);
    procedure ValidateOutputPlan;
    procedure EmitPlannedFile(const AIndex: Integer; AQuiet: Boolean);
    procedure EmitFiles(AJobCount: Integer);
  protected
    function HonoredSettings: TGocciaHonoredSettings; override;
    function HonorsUnsafeRequests: Boolean; override;
    procedure Configure; override;
    function UsageLine: string; override;
    function StdinUsage: TGocciaStdinUsage; override;
    procedure HandleError(const AException: Exception); override;
    procedure ExecuteWithPaths(const APaths: TStringList); override;
  end;

  { one file compiled on a thread of its own: an engine's state is its thread's }
  TBundleThread = class(TThread)
  private
    FApp: TBundlerApp;
    FIndex: Integer;
    FQuiet: Boolean;
    FErrorClass: ExceptClass;
    FErrorMessage, FErrorDetail: string;
  protected
    procedure Execute; override;
  end;

procedure TBundleThread.Execute;
begin
  { a file that would start while another thread reports a fault does not (ADR 0196) }
  StopIfEngineFaultReported;
  try
    try
      FApp.EmitPlannedFile(FIndex, FQuiet);
    except
      on E: Exception do
      begin
        { the run stops here, on this file's thread (ADR 0196) }
        if IsEngineFault(E) then
          AbortOnEngineFault(FApp.FPlan[FIndex].InputFile, E);
        FErrorClass := ExceptClass(E.ClassType);
        FErrorMessage := E.Message;
        if E is EGocciaBundleFailure then FErrorDetail := EGocciaBundleFailure(E).Detail
        else FErrorDetail := E.Message;
      end;
    end;
  finally
    { this thread's engine state: FPC does not finalize managed threadvars }
    RunThreadvarCleanupsOrAbort(FApp.FPlan[FIndex].InputFile);
  end;
end;

{ TBundlerApp - Configure }

function TBundlerApp.UsageLine: string;
begin
  Result := '[file|directory|-] [options]';
end;

function TBundlerApp.StdinUsage: TGocciaStdinUsage;
begin
  Result := suStdinDefault;
end;

{ The bundler runs no code, so it grants no capability and applies no
  limit; --timeout and the --max-* limits are usage errors. }
function TBundlerApp.HonoredSettings: TGocciaHonoredSettings;
begin
  Result := [];
end;

{ Nor does it apply unsafe-* keys, so a config requesting them needs no
  trust here; they are reported as ignored. }
function TBundlerApp.HonorsUnsafeRequests: Boolean;
begin
  Result := False;
end;

procedure TBundlerApp.Configure;
begin
  AddEngineOptions;
  FOutputPath := AddString('output',
    'Output path (single file) or output directory (multiple files)');
  FSourceMap := TStringOption(Add(TOptionalStringOption.Create('source-map',
    'Write a .map source map file (optional: explicit path)')));
  FOutputPath.WritesHostFile := True;
  FSourceMap.WritesHostFile := True;
end;

{ a file that did not compile prints its report, as GocciaBundler prints a compile error }
procedure TBundlerApp.HandleError(const AException: Exception);
begin
  if AException is EGocciaBundleFailure then
    WriteLn(AException.Message)
  else
    inherited HandleError(AException);
end;

{ TBundlerApp - Core logic }

{ --source-map: the JSX transform's map of the file (GocciaBundler's map is its source pipeline's, the same
  transform), at the planned path; nothing for a file the transform leaves alone }
procedure TBundlerApp.WriteSourceMapIfEnabled(const ASource: TStringList; const AOutput: TBundlerOutput);
var
  SourceName, Text: string;
  Transformed: TGocciaJSXTransformResult;
begin
  if AOutput.MapOutputPath = '' then
    Exit;
  if IsJSXExcludedExtension(ExtractFileExt(AOutput.InputFile)) then
    Exit;
  SourceName := AOutput.InputFile;
  if (SourceName = STDIN_FILE_NAME) and FOutputPath.Present then
    SourceName := AOutput.OutputPath;
  Text := StringListToSourceText(ASource);
  Transformed := TGocciaJSXTransformer.Transform(Text, AOutput.InputFile);
  try
    if Assigned(Transformed.SourceMap) then
      Transformed.SourceMap.SetSourceContent(0, Text);
    WriteSourceMapIfAvailable(Transformed.SourceMap, AOutput.MapOutputPath,
      AOutput.OutputPath, SourceName, False);
  finally
    Transformed.SourceMap.Free;
    Transformed.OriginMap.Free;
  end;
end;

{ runs on the file's own thread: a fresh engine under the file's settings compiles it }
procedure TBundlerApp.EmitBytecode(const ASource: TStringList; const AOutput: TBundlerOutput; AQuiet: Boolean);
var
  Engine: TGocciaEngine;
  FileConfig: TConfigEntryArray;
  FileConfigPath: string;
  NoUnsafe: TGocciaUnsafeRequests;
  Bytes: TBytes;
  R: TGocciaRunResult;
  StartTime, EndTime: Int64;
  Failure: EGocciaBundleFailure;
  Stream: TFileStream;
  SnapshotErr: UnicodeString;
begin
  if not AQuiet then
    WriteLn('Compiling: ', AOutput.InputFile);
  StartTime := GetNanoseconds;
  { CLI flag > per-file config > root config > default }
  FileConfigPath := DiscoverFileConfigPath(AOutput.InputFile);
  if FileConfigPath <> '' then
    FileConfig := LoadFileConfig(FileConfigPath)
  else
    SetLength(FileConfig, 0);
  { The bundler grants nothing, so a permissions block is only validated and
    any grant it requests reported as ignored. }
  FilePermissionRequest(FileConfigPath, AOutput.InputFile);
  NoUnsafe := [];
  Engine := TGocciaEngine.Create;
  try
    { build.pas snapshots the compiled preludes from a run of an empty file with GOCCIA_SNAPSHOT_OUT set: the host's
      are compiled too, so the snapshot holds what the other apps' have }
    if GetEnvironmentVariable('GOCCIA_SNAPSHOT_OUT') <> '' then
      Engine.PrepareHost(SnapshotErr);
    ApplySettings(Engine, ResolveFileSettings(EngineOptions, FileConfig, AOutput.InputFile, NoUnsafe));
    R := Default(TGocciaRunResult);
    if not Engine.CompileArtifact(AOutput.InputFile, UnicodeString(StringListToSourceText(ASource)), Bytes, R) then
    begin
      Failure := EGocciaBundleFailure.Create(FailureText(AOutput.InputFile, R, ASource, Engine, '', nil, True, True,
        IsColorTerminal, 0, 0));
      Failure.Detail := UTF8Encode(R.ErrorText);
      raise Failure;
    end;
  finally
    Engine.Free;
  end;
  if AOutput.CreateParents then
    EnsureHostOutputParent(AOutput.OutputPath);
  Stream := TFileStream.Create(AOutput.OutputPath, fmCreate);
  try
    if Length(Bytes) > 0 then
      Stream.WriteBuffer(Bytes[0], Length(Bytes));
  finally
    Stream.Free;
  end;
  WriteSourceMapIfEnabled(ASource, AOutput);
  EndTime := GetNanoseconds;
  if not AQuiet then
    WriteLn(SysUtils.Format('  Compiled to %s (%s)',
      [AOutput.OutputPath, FormatDuration(EndTime - StartTime)]));
end;

function TBundlerApp.ResolveOutputPath(const AInputFile,
  AInputRoot: string): string;
var
  RelativePath: string;
begin
  if FOutputPath.Present then
  begin
    if DirectoryExists(FOutputPath.Value) then
    begin
      if AInputRoot <> '' then
        RelativePath := ExtractRelativePath(
          IncludeTrailingPathDelimiter(ExpandFileName(AInputRoot)),
          ExpandFileName(AInputFile))
      else
        RelativePath := ExtractFileName(AInputFile);
      Result := IncludeTrailingPathDelimiter(FOutputPath.Value) +
        ChangeFileExt(RelativePath, EXT_GBC);
    end
    else
      Result := FOutputPath.Value;
  end
  else
    Result := ChangeFileExt(AInputFile, EXT_GBC);
end;

procedure TBundlerApp.EmitPlannedFile(const AIndex: Integer; AQuiet: Boolean);
var
  Source: TStringList;
begin
  Source := SourceRegistry.Load(FPlan[AIndex].InputFile);
  try
    EmitBytecode(Source, FPlan[AIndex], AQuiet);
  finally
    Source.Free;
  end;
end;

{ Replace characters that are illegal in filenames on Windows (< > : " /
  \ | * ?) and ASCII control characters with '_'.  Used when synthesizing
  on-disk paths from multifile section names whose synthetic basename
  contains the angle brackets of the stdin marker (e.g.
  '<stdin>[part1]').  The section name itself stays intact for diagnostic
  output; only the disk path is sanitized. }
function SanitizeFileBaseName(const ABaseName: string): string;
var
  I: Integer;
  C: Char;
begin
  SetLength(Result, Length(ABaseName));
  for I := 1 to Length(ABaseName) do
  begin
    C := ABaseName[I];
    if (C = '<') or (C = '>') or (C = ':') or (C = '"') or
       (C = '/') or (C = '\') or (C = '|') or (C = '?') or (C = '*') or
       (Ord(C) < 32) then
      Result[I] := '_'
    else
      Result[I] := C;
  end;
end;

procedure TBundlerApp.AddPlannedFiles(const AFiles: TStringList;
  const AInputRoot: string; const AStdin: Boolean);
var
  I, Index: Integer;
begin
  for I := 0 to AFiles.Count - 1 do
  begin
    if LowerCase(ExtractFileExt(AFiles[I])) = EXT_GBC then
      raise Exception.CreateFmt('Cannot compile bytecode file: %s', [AFiles[I]]);
    Index := Length(FPlan);
    SetLength(FPlan, Index + 1);
    FPlan[Index].InputFile := AFiles[I];
    if AStdin and MultifileEnabled then
      FPlan[Index].OutputPath := IncludeTrailingPathDelimiter(FOutputPath.Value) +
        ChangeFileExt(SanitizeFileBaseName(ExtractFileName(AFiles[I])), EXT_GBC)
    else if AStdin then
      FPlan[Index].OutputPath := FOutputPath.Value
    else
      FPlan[Index].OutputPath := ResolveOutputPath(AFiles[I], AInputRoot);
    FPlan[Index].CreateParents := (AInputRoot <> '') and
      FOutputPath.Present and DirectoryExists(FOutputPath.Value);
    if FSourceMap.Present then
    begin
      FPlan[Index].MapOutputPath := FSourceMap.ValueOr('');
      if FPlan[Index].MapOutputPath = '' then
        FPlan[Index].MapOutputPath := ChangeFileExt(
          FPlan[Index].OutputPath, EXT_MAP);
    end;
  end;
end;

procedure TBundlerApp.ValidateOutputPlan;
var
  Destinations, Sources: TStringList;
  I, Index: Integer;
  ParentPath, NextParent: string;

  procedure ReserveDestination(const APath, ASource: string);
  var
    Canonical: string;
    Index: Integer;
  begin
    if APath = '' then
      Exit;
    Canonical := CanonicalCapabilityPath(APath);
    if Destinations.Find(Canonical, Index) then
      raise TParseError.CreateFmt(
        'Output collision: %s and %s both write to %s',
        [Sources[PtrInt(Destinations.Objects[Index])], ASource, APath]);
    Destinations.AddObject(Canonical, TObject(PtrInt(Sources.Add(ASource))));
  end;

begin
  Destinations := TStringList.Create;
  Sources := TStringList.Create;
  try
    Destinations.Sorted := True;
    {$IF DEFINED(DARWIN) OR DEFINED(MSWINDOWS)}
    Destinations.CaseSensitive := False;
    {$ELSE}
    Destinations.CaseSensitive := True;
    {$IFEND}
    for I := 0 to High(FPlan) do
    begin
      ReserveDestination(FPlan[I].OutputPath, FPlan[I].InputFile + ' (bytecode)');
      ReserveDestination(FPlan[I].MapOutputPath, FPlan[I].InputFile + ' (source map)');
    end;
    // A planned file cannot also be a directory needed by another output.
    // Check after collecting all destinations so input order does not matter.
    for I := 0 to Destinations.Count - 1 do
    begin
      ParentPath := ExtractFileDir(Destinations[I]);
      while ParentPath <> '' do
      begin
        if Destinations.Find(ParentPath, Index) then
          raise TParseError.CreateFmt(
            'Output collision: %s and %s require %s as both a file and a directory',
            [Sources[PtrInt(Destinations.Objects[Index])],
             Sources[PtrInt(Destinations.Objects[I])], ParentPath]);
        NextParent := ExtractFileDir(ParentPath);
        if NextParent = ParentPath then
          Break;
        ParentPath := NextParent;
      end;
    end;
  finally
    Sources.Free;
    Destinations.Free;
  end;
end;

{ every planned file, each on its own thread, at most AJobCount at once. One at a time (AJobCount 1), a failure
  stops the run as GocciaBundler's does; in parallel each failed file is reported and the exit code is 1. }
procedure TBundlerApp.EmitFiles(AJobCount: Integer);
var
  I, Next, Oldest, Started, Running: Integer;
  Workers: array of TBundleThread;
  Parallel: Boolean;
  ErrorClass: ExceptClass;
  ErrorMessage: string;
  Failure: EGocciaBundleFailure;
begin
  Parallel := AJobCount > 1;
  SetLength(Workers, Length(FPlan));
  Running := 0;
  Oldest := 0;
  Started := 0;
  ErrorClass := nil;
  ErrorMessage := '';
  try
    for Next := 0 to High(FPlan) do
    begin
      if Running = AJobCount then
      begin
        Workers[Oldest].WaitFor;
        StopIfEngineFaultReported;
        Dec(Running);
        Inc(Oldest);
        if not Parallel and (Workers[Oldest - 1].FErrorClass <> nil) then
          Break;
      end;
      if not Parallel and (Next > 0) then
        WriteLn;
      Workers[Next] := TBundleThread.Create(True, FILE_THREAD_STACK);
      Workers[Next].FApp := Self;
      Workers[Next].FIndex := Next;
      Workers[Next].FQuiet := Parallel;
      Workers[Next].Start;
      Inc(Started);
      Inc(Running);
    end;
  finally
    for I := 0 to Started - 1 do
    begin
      if I >= Oldest then
        Workers[I].WaitFor;
      StopIfEngineFaultReported;
      if Workers[I].FErrorClass <> nil then
      begin
        if Parallel then
        begin
          WriteLn('Error in ', FPlan[I].InputFile, ': ', Workers[I].FErrorDetail);
          ExitCode := 1;
        end
        else if ErrorClass = nil then
        begin
          ErrorClass := Workers[I].FErrorClass;
          ErrorMessage := Workers[I].FErrorMessage;
        end;
      end;
      Workers[I].Free;
    end;
  end;
  if ErrorClass <> nil then
  begin
    if ErrorClass = EGocciaBundleFailure then
    begin
      Failure := EGocciaBundleFailure.Create(ErrorMessage);
      raise Failure;
    end;
    raise ErrorClass.Create(ErrorMessage);
  end;
end;

{ TBundlerApp - ExecuteWithPaths }

procedure TBundlerApp.ExecuteWithPaths(const APaths: TStringList);
var
  I, Jobs: Integer;
  Inputs, Found, Files, StdinNames: TStringList;
  InputRoot: string;
begin
  if FOutputPath.Present and (FOutputPath.Value <> '') and
     not DirectoryExists(FOutputPath.Value) and
     ((APaths.Count > 1) or
      ((APaths.Count = 1) and DirectoryExists(APaths[0]))) then
    raise TParseError.Create(
      '--output must be a directory when compiling multiple files.');

  if MultifileEnabled and FOutputPath.Present and
     (FOutputPath.Value <> '') and not DirectoryExists(FOutputPath.Value) then
    raise TParseError.Create(
      '--output=<file> cannot be combined with --multifile (an input '
      + 'may expand to multiple sections); pass a directory or omit '
      + '--output.');

  // Use Present rather than the value to catch the bare --source-map
  // form too — see ScriptLoader for the rationale.
  if FSourceMap.Present and MultifileEnabled then
    raise TParseError.Create(
      '--source-map cannot be combined with --multifile (an input '
      + 'may expand to multiple sections).');

  { Freeze the complete invocation before a worker may write: directory
    discovery, multifile expansion, configs and every output destination. }
  FPlan := nil;
  Inputs := TStringList.Create;
  StdinNames := nil;
  try
    if APaths.Count = 0 then
      Inputs.Add(STDIN_FILE_NAME)
    else
      Inputs.AddStrings(APaths);
    for I := 0 to Inputs.Count - 1 do
    begin
      if (APaths.Count = 0) or IsStdinPath(Inputs[I]) then
      begin
        if not FOutputPath.Present then
          raise TParseError.Create(
            '--output=<path> is required when compiling from stdin.');
        if MultifileEnabled and not DirectoryExists(FOutputPath.Value) then
          raise TParseError.Create(
            '--output must be a directory when --multifile is set with stdin.');
        if not MultifileEnabled and DirectoryExists(FOutputPath.Value) then
          raise TParseError.Create(
            '--output must be a file when compiling from stdin.');
        ValidateFileConfig(STDIN_FILE_NAME);
        if StdinNames = nil then
          StdinNames := SplitStdinMultifile(ReadSourceFromText(Input));
        AddPlannedFiles(StdinNames, '', True);
        Continue;
      end;
      InputRoot := '';
      if DirectoryExists(Inputs[I]) then
      begin
        InputRoot := Inputs[I];
        Found := FindAllFiles(InputRoot, ScriptExtensions);
      end
      else
      begin
        if not FileExists(Inputs[I]) then
          raise Exception.Create('Path not found: ' + Inputs[I]);
        Found := TStringList.Create;
        Found.Add(Inputs[I]);
      end;
      try
        Files := ExpandMultifileFiles(Found);
        try
          AddPlannedFiles(Files, InputRoot, False);
        finally
          Files.Free;
        end;
      finally
        Found.Free;
      end;
    end;
    ValidateOutputPlan;
    Jobs := GetJobCount(Length(FPlan));
    if Jobs > 1 then
      WriteLn(SysUtils.Format('Compiling %d files with %d workers', [Length(FPlan), Jobs]));
    EmitFiles(Jobs);
  finally
    StdinNames.Free;
    Inputs.Free;
  end;
end;

{ Entry point }

var
  RunResult: Integer;
begin
  RunResult := TGocciaApplication.RunApplication(TBundlerApp, 'GocciaBundler');
  if RunResult <> 0 then
    ExitCode := RunResult;
end.
