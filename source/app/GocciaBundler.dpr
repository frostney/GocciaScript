program GocciaBundler;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  HostOutputFiles,
  TimingUtils,

  Goccia.Application,
  Goccia.Bytecode.Binary,
  Goccia.Bytecode.Module,
  Goccia.Bytecode.Profile.Options,
  CLI.ConfigFile,
  Goccia.CLI.Application,
  Goccia.CLI.Stdin,
  Goccia.CLI.SourceMaps,
  Goccia.CLI.SourcePipelineResult,
  Goccia.CLI.Options,
  CLI.Options,
  Goccia.Capabilities,
  Goccia.Compiler,
  Goccia.Engine,
  Goccia.FileExtensions,
  Goccia.FloatingPoint,
  Goccia.GarbageCollector,
  Goccia.ScriptLoader.Input,
  Goccia.SourcePipeline,
  Goccia.SourceMap,
  Goccia.TextFiles,
  Goccia.Threading,
  Goccia.Threading.Flags,
  Goccia.Threading.Init,

  FileUtils;

type
  TBundlerOutput = record
    InputFile: string;
    OutputPath: string;
    MapOutputPath: string;
    CreateParents: Boolean;
  end;

  TBundlerApp = class(TGocciaCLIApplication)
  private
    FOutputPath: TStringOption;
    FSourceMap: TStringOption;
    FPlan: array of TBundlerOutput;

    function CompileSource(const ASource: TStringList;
      const AFileName: string;
      out ASourceMap: TGocciaSourceMap): TGocciaBytecodeModule;
    procedure WriteSourceMapIfEnabled(const ASourceMap: TGocciaSourceMap;
      const AOutput: TBundlerOutput);
    procedure EmitBytecode(const ASource: TStringList;
      const AOutput: TBundlerOutput);
    function ResolveOutputPath(const AInputFile, AInputRoot: string): string;
    procedure AddPlannedFiles(const AFiles: TStringList;
      const AInputRoot: string; const AStdin: Boolean);
    procedure ValidateOutputPlan;
    procedure EmitPlannedFile(const AIndex: Integer);
    procedure EmitWorkerProc(const AFileName: string; const AIndex: Integer;
      out AConsoleOutput: string; out AErrorMessage: string; AData: Pointer);
    procedure EmitParallel(const AFiles: TStringList;
      const AJobCount: Integer);

  protected
    function HonoredSettings: TGocciaHonoredSettings; override;
    function HonorsUnsafeRequests: Boolean; override;
    procedure Configure; override;
    function UsageLine: string; override;
    function StdinUsage: TGocciaStdinUsage; override;
    procedure Validate; override;
    procedure ExecuteWithPaths(const APaths: TStringList); override;
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

{ TBundlerApp - Validate }

procedure TBundlerApp.Validate;
begin
  inherited Validate;
end;

{ TBundlerApp - Core logic }

procedure TBundlerApp.WriteSourceMapIfEnabled(
  const ASourceMap: TGocciaSourceMap; const AOutput: TBundlerOutput);
var
  SourceName: string;
begin
  if AOutput.MapOutputPath = '' then
    Exit;
  SourceName := AOutput.InputFile;
  if (SourceName = STDIN_FILE_NAME) and FOutputPath.Present then
    SourceName := AOutput.OutputPath;
  WriteSourceMapIfAvailable(ASourceMap, AOutput.MapOutputPath,
    AOutput.OutputPath, SourceName, not GIsWorkerThread);
end;

function TBundlerApp.CompileSource(const ASource: TStringList;
  const AFileName: string;
  out ASourceMap: TGocciaSourceMap): TGocciaBytecodeModule;
var
  SourcePipelineResult: TGocciaCLISourcePipelineResult;
  Compiler: TGocciaCompiler;
  CompiledModule: TGocciaBytecodeModule;
  FileConfig: TConfigEntryArray;
  FileConfigPath: string;
  EffectiveStrictTypes: Boolean;
  EffectiveWarningUnsupportedFeatures: Boolean;
  EffectiveSourceType: TGocciaSourceType;
  EffectiveCompatibility: TGocciaCompatibilityFlags;
  EffectiveLabelStatementsEnabled: Boolean;
  EffectiveForInLoopsEnabled: Boolean;
  EffectiveExperimentalJSModuleSourceEnabled: Boolean;
  PipelineOptions: TGocciaSourcePipelineOptions;
begin
  ASourceMap := nil;
  { Resolve source pipeline flags: CLI flag > per-file config >
    root config > default. }
  FileConfigPath := DiscoverFileConfigPath(AFileName);
  if FileConfigPath <> '' then
    FileConfig := LoadFileConfig(FileConfigPath)
  else
    SetLength(FileConfig, 0);
  { The bundler grants nothing, so a permissions block is only validated and
    any grant it requests reported as ignored. }
  FilePermissionRequest(FileConfigPath, AFileName);
  ResolveCompatibilityFlags(EngineOptions, FileConfig, EffectiveCompatibility);
  EffectiveLabelStatementsEnabled := ResolveFlagOption(
    EngineOptions.CompatibilityFlagOption(cfLabel), FileConfig);
  EffectiveForInLoopsEnabled := ResolveFlagOption(
    EngineOptions.CompatibilityFlagOption(cfForIn), FileConfig);
  EffectiveExperimentalJSModuleSourceEnabled := ResolveFlagOption(
    EngineOptions.CompatibilityFlagOption(cfExperimentalJSModuleSource),
    FileConfig);
  EffectiveSourceType := ResolveSourceTypeOption(EngineOptions.SourceType,
    FileConfig, AFileName);
  EffectiveStrictTypes := ResolveFlagOption(
    EngineOptions.StrictTypes, FileConfig);
  EffectiveWarningUnsupportedFeatures := ResolveFlagOption(
    EngineOptions.WarningUnsupportedFeatures, FileConfig);

  CompiledModule := nil;
  PipelineOptions := TGocciaSourcePipeline.DefaultOptions;
  PipelineOptions.Preprocessors := TGocciaEngine.DefaultPreprocessors;
  PipelineOptions.Compatibility := EffectiveCompatibility;
  PipelineOptions.LabelStatementsEnabled := EffectiveLabelStatementsEnabled;
  PipelineOptions.ForInLoopsEnabled := EffectiveForInLoopsEnabled;
  PipelineOptions.ExperimentalJSModuleSourceEnabled :=
    EffectiveExperimentalJSModuleSourceEnabled;
  PipelineOptions.WarningUnsupportedFeatures :=
    EffectiveWarningUnsupportedFeatures;
  PipelineOptions.SourceType := EffectiveSourceType;
  SourcePipelineResult := TGocciaCLISourcePipelineResult.Parse(ASource, AFileName,
    PipelineOptions, False);
  try
    Compiler := TGocciaCompiler.Create(AFileName);
    try
      Compiler.StrictTypes := EffectiveStrictTypes;
      { The goal the parse settled for a detected entry (ADR 0140). }
      Compiler.NonStrictMode := (cfNonStrictMode in EffectiveCompatibility) and
        (SourcePipelineResult.SourceType = stScript);
      Compiler.ArgumentsObjectEnabled :=
        cfArgumentsObject in EffectiveCompatibility;
      if SourcePipelineResult.SourceType = stModule then
      begin
        Compiler.GlobalBackedTopLevel := True;
        Compiler.PreinitializedTopLevelFunctions := True;
        Compiler.AsyncTopLevel := SourcePipelineResult.ProgramNode.HasTopLevelAwait;
      end;
      CompiledModule := Compiler.Compile(SourcePipelineResult.ProgramNode);
      PipelineOptions.SourceType := SourcePipelineResult.SourceType;
      CompiledModule.CompileProfile := BuildBytecodeCompileProfile(
        PipelineOptions, EffectiveStrictTypes);
      ASourceMap := SourcePipelineResult.TakeSourceMap;
      Result := CompiledModule;
      CompiledModule := nil;
    finally
      Compiler.Free;
    end;
  finally
    SourcePipelineResult.Free;
    CompiledModule.Free;
  end;
end;

procedure TBundlerApp.EmitBytecode(const ASource: TStringList;
  const AOutput: TBundlerOutput);
var
  Module: TGocciaBytecodeModule;
  SourceMap: TGocciaSourceMap;
  StartTime, EndTime: Int64;
  FloatingPointState: TGocciaFloatingPointState;
begin
  if not GIsWorkerThread then
    WriteLn('Compiling: ', AOutput.InputFile);
  StartTime := GetNanoseconds;

  SourceMap := nil;
  { Constant folding is IEEE-754 arithmetic and needs floating-point
    exceptions masked, as TGocciaEngine does around its compile and run
    steps. The scope is per call because the mask is per thread. }
  EnterGocciaFloatingPointScope(FloatingPointState);
  try
    Module := CompileSource(ASource, AOutput.InputFile, SourceMap);
  finally
    LeaveGocciaFloatingPointScope(FloatingPointState);
  end;
  try
    if AOutput.CreateParents then
      EnsureHostOutputParent(AOutput.OutputPath);
    Goccia.Bytecode.Binary.SaveModuleToFile(Module, AOutput.OutputPath);
    WriteSourceMapIfEnabled(SourceMap, AOutput);
    EndTime := GetNanoseconds;
    if not GIsWorkerThread then
      WriteLn(SysUtils.Format('  Compiled to %s (%s)',
        [AOutput.OutputPath, FormatDuration(EndTime - StartTime)]));
  finally
    SourceMap.Free;
    Module.Free;
  end;
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

procedure TBundlerApp.EmitPlannedFile(const AIndex: Integer);
var
  Source: TStringList;
begin
  Source := SourceRegistry.Load(FPlan[AIndex].InputFile);
  try
    EmitBytecode(Source, FPlan[AIndex]);
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

procedure TBundlerApp.EmitWorkerProc(const AFileName: string;
  const AIndex: Integer; out AConsoleOutput: string;
  out AErrorMessage: string; AData: Pointer);
begin
  AConsoleOutput := '';
  AErrorMessage := '';
  try
    EmitPlannedFile(AIndex);
  except
    on E: Exception do
      AErrorMessage := E.Message;
  end;
end;

procedure TBundlerApp.EmitParallel(const AFiles: TStringList;
  const AJobCount: Integer);
var
  Pool: TGocciaThreadPool;
  I: Integer;
begin
  EnsureSharedPrototypesInitialized;

  Pool := TGocciaThreadPool.Create(AJobCount);
  try
    if (TGarbageCollector.Instance <> nil) then
      Pool.MaxBytes := TGarbageCollector.Instance.MaxBytes;
    Pool.RunAll(AFiles, EmitWorkerProc);

    for I := 0 to AFiles.Count - 1 do
      if Pool.Results[I].ErrorMessage <> '' then
      begin
        WriteLn('Error in ', Pool.Results[I].FileName, ': ', Pool.Results[I].ErrorMessage);
        ExitCode := 1;
      end;
  finally
    Pool.Free;
  end;
end;

{ TBundlerApp - ExecuteWithPaths }

procedure TBundlerApp.ExecuteWithPaths(const APaths: TStringList);
var
  I: Integer;
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
    Inputs.Clear;
    for I := 0 to High(FPlan) do
      Inputs.Add(FPlan[I].InputFile);
    if GetJobCount(Inputs.Count) > 1 then
    begin
      WriteLn(SysUtils.Format('Compiling %d files with %d workers',
        [Inputs.Count, GetJobCount(Inputs.Count)]));
      EmitParallel(Inputs, GetJobCount(Inputs.Count));
    end
    else
      for I := 0 to High(FPlan) do
      begin
        if I > 0 then
          WriteLn;
        EmitPlannedFile(I);
      end;
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
