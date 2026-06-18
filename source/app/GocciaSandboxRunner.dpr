program GocciaSandboxRunner;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,
  base64,

  CLI.ConfigFile,
  CLI.Options,
  FileUtils,
  TextSemantics,

  Goccia.Application,
  Goccia.Builtins.Console,
  Goccia.CLI.Application,
  Goccia.CLI.Options,
  Goccia.Engine,
  Goccia.Error,
  Goccia.Error.Detail,
  Goccia.Executor,
  Goccia.Executor.Bytecode,
  Goccia.Executor.Interpreter,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.JSON,
  Goccia.Modules.Loader,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.Console,
  Goccia.RuntimeExtensions.FFI,
  Goccia.RuntimeProfiles.Loader,
  Goccia.RuntimeExtensions.Sandbox,
  Goccia.Sandbox.Context,
  Goccia.Sandbox.Modules,
  Goccia.SourcePipeline,
  Goccia.TextFiles,
  Goccia.Timeout,
  Goccia.Values.ArrayValue,
  Goccia.Values.Error,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  SandboxVirtualFileSystem;

type
  TSandboxRunnerApp = class(TGocciaCLIApplication)
  private
    FContext: TGocciaSandboxContext;
    FSeedPaths: TRepeatableOption;
    FSeedConfigFiles: TRepeatableOption;
    FDiff: TFlagOption;
    FDiffFormat: TStringOption;
    FDiffOutput: TStringOption;
    FPrint: TFlagOption;
    FCurrentOutputLines: TStrings;

    procedure SeedHostPathSpec(const ASpec, ABaseDirectory: string);
    procedure SeedHostPath(const AHostPath, ASandboxPath: string);
    procedure SeedConfigFile(const APath: string);
    procedure SeedInlineFile(const APath: string; const AText: string;
      const ABytes: TBytes; const AHasBytes: Boolean);
    procedure ImportDirectoryContents(const AHostDirectory,
      ASandboxDirectory: string);
    procedure ImportFile(const AHostPath, ASandboxPath: string);
    function ReadHostBytes(const APath: string): TBytes;
    function DecodeBase64Bytes(const AText: string): TBytes;
    function JsonStringProperty(const AObject: TGocciaObjectValue;
      const AName: string; const ARequired: Boolean = False): string;
    function JsonHasProperty(const AObject: TGocciaObjectValue;
      const AName: string): Boolean;
    procedure ApplySeedConfigEntry(const AEntry: TGocciaObjectValue;
      const ABaseDirectory: string);
    procedure LoadSeeds;

    procedure ConfigureEngineForSandbox(const AEngine: TGocciaEngine;
      const AFileName: string);
    procedure CaptureConsoleLine(const AMethod, ALine: string);
    function ExecuteSandboxPath(const AEntryPath: string):
      TGocciaSandboxRunResult;
    function ClonePrimitiveResult(const AValue: TGocciaValue): TGocciaValue;
    procedure WriteDiffIfRequested;
  protected
    procedure Configure; override;
    function UsageLine: string; override;
    procedure Validate; override;
    procedure ExecuteWithPaths(const APaths: TStringList); override;
  public
    destructor Destroy; override;
  end;

function EmptyConfigEntries: TConfigEntryArray;
begin
  SetLength(Result, 0);
end;

function IsDirectoryPath(const APath: string): Boolean;
begin
  Result := DirectoryExists(APath);
end;

function EnsureSandboxAbsolute(const APath: string): string;
begin
  if APath = '' then
    Exit('/');
  if APath[1] = '/' then
    Result := APath
  else
    Result := '/' + APath;
end;

function HostPathFromConfig(const ABaseDirectory, APath: string): string;
begin
  if ExtractFileDrive(APath) <> '' then
    Exit(ExpandFileName(APath));
  if (APath <> '') and (APath[1] = PathDelim) then
    Exit(ExpandFileName(APath));
  Result := ExpandFileName(IncludeTrailingPathDelimiter(ABaseDirectory) + APath);
end;

destructor TSandboxRunnerApp.Destroy;
begin
  FContext.Free;
  inherited;
end;

procedure TSandboxRunnerApp.Configure;
begin
  AddEngineOptions;
  FSeedPaths := AddRepeatable('seed',
    'Import a host path into the sandbox, optionally host=/sandbox/path');
  FSeedConfigFiles := AddRepeatable('seed-config',
    'Import sandbox seed entries from a JSON configuration file');
  FDiff := AddFlag('diff', 'Print sandbox filesystem changes after execution');
  FDiffFormat := AddString('diff-format',
    'Diff format: json or unified (default: json)');
  FDiffOutput := AddString('diff-output', 'Write diff output to a host file');
  FPrint := AddFlag('print', 'Print the script result value');
end;

function TSandboxRunnerApp.UsageLine: string;
begin
  Result := '<sandbox-entry-path> [options]';
end;

procedure TSandboxRunnerApp.Validate;
begin
  inherited Validate;
  if EngineOptions.Timeout.Present and (EngineOptions.Timeout.Value < 0) then
    raise TParseError.Create('--timeout must be 0 or greater.');
  if FDiffFormat.Present and (FDiffFormat.Value <> 'json') and
     (FDiffFormat.Value <> 'unified') then
    raise TParseError.Create('--diff-format must be json or unified.');
end;

function TSandboxRunnerApp.ReadHostBytes(const APath: string): TBytes;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, Stream.Size);
    if Stream.Size > 0 then
      Stream.ReadBuffer(Result[0], Stream.Size);
  finally
    Stream.Free;
  end;
end;

function TSandboxRunnerApp.DecodeBase64Bytes(const AText: string): TBytes;
var
  Decoded: string;
begin
  Decoded := DecodeStringBase64(AText);
  SetLength(Result, Length(Decoded));
  if Length(Decoded) > 0 then
    Move(Decoded[1], Result[0], Length(Decoded));
end;

function TSandboxRunnerApp.JsonHasProperty(const AObject: TGocciaObjectValue;
  const AName: string): Boolean;
var
  Value: TGocciaValue;
begin
  Value := AObject.GetProperty(AName);
  Result := Assigned(Value) and not (Value is TGocciaUndefinedLiteralValue);
end;

function TSandboxRunnerApp.JsonStringProperty(const AObject: TGocciaObjectValue;
  const AName: string; const ARequired: Boolean): string;
var
  Value: TGocciaValue;
begin
  Value := AObject.GetProperty(AName);
  if (not Assigned(Value)) or (Value is TGocciaUndefinedLiteralValue) or
     (Value is TGocciaNullLiteralValue) then
  begin
    if ARequired then
      raise Exception.CreateFmt('seed config entry requires "%s"', [AName]);
    Exit('');
  end;
  if not (Value is TGocciaStringLiteralValue) then
    raise Exception.CreateFmt('seed config "%s" must be a string', [AName]);
  Result := TGocciaStringLiteralValue(Value).Value;
end;

procedure TSandboxRunnerApp.SeedInlineFile(const APath: string;
  const AText: string; const ABytes: TBytes; const AHasBytes: Boolean);
var
  SandboxPath: string;
  ParentPath: string;
  SlashIndex: Integer;
begin
  SandboxPath := FContext.Fs.Normalize(EnsureSandboxAbsolute(APath));
  ParentPath := '/';
  SlashIndex := Length(SandboxPath);
  while (SlashIndex > 1) and (SandboxPath[SlashIndex] <> '/') do
    Dec(SlashIndex);
  if SlashIndex > 1 then
    ParentPath := Copy(SandboxPath, 1, SlashIndex - 1);
  FContext.Fs.MakeDirectory(ParentPath, True);
  if AHasBytes then
    FContext.Fs.WriteAllBytes(SandboxPath, ABytes)
  else
    FContext.Fs.WriteAllText(SandboxPath, AText);
end;

procedure TSandboxRunnerApp.ImportFile(const AHostPath, ASandboxPath: string);
var
  SandboxPath: string;
  ParentPath: string;
  SlashIndex: Integer;
begin
  SandboxPath := FContext.Fs.Normalize(EnsureSandboxAbsolute(ASandboxPath));
  ParentPath := '/';
  SlashIndex := Length(SandboxPath);
  while (SlashIndex > 1) and (SandboxPath[SlashIndex] <> '/') do
    Dec(SlashIndex);
  if SlashIndex > 1 then
    ParentPath := Copy(SandboxPath, 1, SlashIndex - 1);
  FContext.Fs.MakeDirectory(ParentPath, True);
  FContext.Fs.WriteAllBytes(SandboxPath, ReadHostBytes(AHostPath));
end;

procedure TSandboxRunnerApp.ImportDirectoryContents(const AHostDirectory,
  ASandboxDirectory: string);
var
  SearchRec: TSearchRec;
  HostChild: string;
  SandboxChild: string;
begin
  FContext.Fs.MakeDirectory(EnsureSandboxAbsolute(ASandboxDirectory), True);
  if FindFirst(IncludeTrailingPathDelimiter(AHostDirectory) + '*',
     faAnyFile, SearchRec) <> 0 then
    Exit;
  try
    repeat
      if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
        Continue;
      HostChild := IncludeTrailingPathDelimiter(AHostDirectory) + SearchRec.Name;
      if ASandboxDirectory = '/' then
        SandboxChild := '/' + SearchRec.Name
      else
        SandboxChild := IncludeTrailingPathDelimiter(ASandboxDirectory) +
          SearchRec.Name;
      SandboxChild := StringReplace(SandboxChild, PathDelim, '/', [rfReplaceAll]);
      if (SearchRec.Attr and faDirectory) <> 0 then
        ImportDirectoryContents(HostChild, SandboxChild)
      else
        ImportFile(HostChild, SandboxChild);
    until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;
end;

procedure TSandboxRunnerApp.SeedHostPath(const AHostPath,
  ASandboxPath: string);
var
  HostPath: string;
  TargetPath: string;
begin
  HostPath := ExpandFileName(AHostPath);
  if not FileExists(HostPath) and not DirectoryExists(HostPath) then
    raise Exception.Create('Seed path does not exist: ' + HostPath);

  TargetPath := EnsureSandboxAbsolute(ASandboxPath);
  if TargetPath = '' then
    TargetPath := '/';

  if IsDirectoryPath(HostPath) then
    ImportDirectoryContents(HostPath, TargetPath)
  else
  begin
    if (ASandboxPath = '') or (ASandboxPath = '/') then
      TargetPath := '/' + ExtractFileName(HostPath);
    ImportFile(HostPath, TargetPath);
  end;
end;

procedure TSandboxRunnerApp.SeedHostPathSpec(const ASpec,
  ABaseDirectory: string);
var
  SeparatorIndex: Integer;
  HostPath: string;
  SandboxPath: string;
begin
  SeparatorIndex := Pos('=', ASpec);
  if SeparatorIndex > 0 then
  begin
    HostPath := Copy(ASpec, 1, SeparatorIndex - 1);
    SandboxPath := Copy(ASpec, SeparatorIndex + 1, MaxInt);
  end
  else
  begin
    HostPath := ASpec;
    SandboxPath := '/';
  end;
  SeedHostPath(HostPathFromConfig(ABaseDirectory, HostPath), SandboxPath);
end;

procedure TSandboxRunnerApp.ApplySeedConfigEntry(
  const AEntry: TGocciaObjectValue; const ABaseDirectory: string);
var
  FromPath: string;
  ToPath: string;
  InlinePath: string;
  Text: string;
  Bytes: TBytes;
  SourceCount: Integer;
begin
  FromPath := JsonStringProperty(AEntry, 'from');
  Text := JsonStringProperty(AEntry, 'text');
  SourceCount := 0;
  if FromPath <> '' then Inc(SourceCount);
  if JsonHasProperty(AEntry, 'text') then Inc(SourceCount);
  if JsonHasProperty(AEntry, 'base64') then Inc(SourceCount);
  if SourceCount <> 1 then
    raise Exception.Create(
      'seed file entries must specify exactly one of from, text, or base64');

  if FromPath <> '' then
  begin
    ToPath := JsonStringProperty(AEntry, 'to');
    if ToPath = '' then
      ToPath := '/';
    SeedHostPath(HostPathFromConfig(ABaseDirectory, FromPath), ToPath);
    Exit;
  end;

  InlinePath := JsonStringProperty(AEntry, 'path', True);
  if JsonHasProperty(AEntry, 'base64') then
  begin
    Bytes := DecodeBase64Bytes(JsonStringProperty(AEntry, 'base64', True));
    SeedInlineFile(InlinePath, '', Bytes, True);
  end
  else
    SeedInlineFile(InlinePath, Text, nil, False);
end;

procedure TSandboxRunnerApp.SeedConfigFile(const APath: string);
var
  ConfigPath: string;
  BaseDirectory: string;
  Parser: TGocciaJSONParser;
  Parsed: TGocciaValue;
  Root: TGocciaObjectValue;
  FilesValue: TGocciaValue;
  FilesArray: TGocciaArrayValue;
  EntryValue: TGocciaValue;
  I: Integer;
begin
  ConfigPath := ExpandFileName(APath);
  BaseDirectory := ExtractFilePath(ConfigPath);
  Parser := TGocciaJSONParser.Create;
  try
    Parsed := Parser.Parse(ReadUTF8FileText(ConfigPath));
  finally
    Parser.Free;
  end;

  if not (Parsed is TGocciaObjectValue) then
    raise Exception.Create('seed config must be a JSON object');
  Root := TGocciaObjectValue(Parsed);
  FilesValue := Root.GetProperty('files');
  if not (FilesValue is TGocciaArrayValue) then
    raise Exception.Create('seed config requires a "files" array');
  FilesArray := TGocciaArrayValue(FilesValue);
  for I := 0 to FilesArray.GetLength - 1 do
  begin
    EntryValue := FilesArray.GetElement(I);
    if not (EntryValue is TGocciaObjectValue) then
      raise Exception.Create('seed config files entries must be objects');
    ApplySeedConfigEntry(TGocciaObjectValue(EntryValue), BaseDirectory);
  end;
end;

procedure TSandboxRunnerApp.LoadSeeds;
var
  I: Integer;
begin
  for I := 0 to FSeedPaths.Values.Count - 1 do
    SeedHostPathSpec(FSeedPaths.Values[I], GetCurrentDir);
  for I := 0 to FSeedConfigFiles.Values.Count - 1 do
    SeedConfigFile(FSeedConfigFiles.Values[I]);
end;

procedure TSandboxRunnerApp.ConfigureEngineForSandbox(
  const AEngine: TGocciaEngine; const AFileName: string);
var
  Runtime: TGocciaRuntimeCore;
  ConsoleExtension: TGocciaConsoleRuntimeExtension;
  EmptyConfig: TConfigEntryArray;
begin
  EmptyConfig := EmptyConfigEntries;
  AEngine.SourceType := ResolveSourceTypeOption(EngineOptions.SourceType,
    EmptyConfig, AFileName);
  AEngine.Compatibility := ResolveCompatibilityFlags(EngineOptions,
    EmptyConfig);
  AEngine.StrictTypes := ResolveFlagOption(EngineOptions.StrictTypes,
    EmptyConfig);
  AEngine.FunctionConstructor.Enabled := ResolveFlagOption(
    EngineOptions.UnsafeFunctionConstructor, EmptyConfig);

  Runtime := AttachRuntime(AEngine);
  ApplyLoaderRuntimeProfile(Runtime);
  Runtime.Install(TGocciaSandboxRuntimeExtension.Create(FContext));
  if ResolveFlagOption(EngineOptions.UnsafeFFI, EmptyConfig) then
    Runtime.Install(TGocciaFFIRuntimeExtension.Create);
  if EngineOptions.AllowedHosts.Present then
    AEngine.SetAllowedFetchHosts(EngineOptions.AllowedHosts.Values);

  ConsoleExtension := TGocciaConsoleRuntimeExtension(
    Runtime.FindRuntimeExtension(TGocciaConsoleRuntimeExtension));
  if Assigned(ConsoleExtension) and Assigned(ConsoleExtension.BuiltinConsole) then
    ConsoleExtension.BuiltinConsole.OutputCallback := CaptureConsoleLine;
end;

procedure TSandboxRunnerApp.CaptureConsoleLine(const AMethod, ALine: string);
begin
  if Assigned(FCurrentOutputLines) then
    FCurrentOutputLines.Add(ALine);
end;

function TSandboxRunnerApp.ClonePrimitiveResult(
  const AValue: TGocciaValue): TGocciaValue;
begin
  if not Assigned(AValue) or (AValue is TGocciaUndefinedLiteralValue) then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  if AValue is TGocciaNullLiteralValue then
    Exit(TGocciaNullLiteralValue.NullValue);
  if AValue is TGocciaBooleanLiteralValue then
    Exit(TGocciaBooleanLiteralValue.Create(
      TGocciaBooleanLiteralValue(AValue).Value));
  if AValue is TGocciaNumberLiteralValue then
    Exit(TGocciaNumberLiteralValue.Create(
      TGocciaNumberLiteralValue(AValue).Value));
  if AValue is TGocciaStringLiteralValue then
    Exit(TGocciaStringLiteralValue.Create(
      TGocciaStringLiteralValue(AValue).Value));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TSandboxRunnerApp.ExecuteSandboxPath(const AEntryPath: string):
  TGocciaSandboxRunResult;
var
  Source: TStringList;
  OutputLines: TStringList;
  Executor: TGocciaExecutor;
  InterpreterExecutor: TGocciaInterpreterExecutor;
  BytecodeExecutor: TGocciaBytecodeExecutor;
  Resolver: TGocciaSandboxModuleResolver;
  Provider: TGocciaSandboxModuleContentProvider;
  Engine: TGocciaEngine;
  ScriptResult: TGocciaScriptResult;
  PreviousOutputLines: TStrings;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Ok := False;
  Result.ExitCode := 1;

  if not FContext.Fs.IsFile(AEntryPath) then
  begin
    Result.ErrorMessage := 'sandbox entry file not found: ' + AEntryPath;
    Exit;
  end;

  Source := CreateUTF8FileTextLines(
    UTF8String(FContext.Fs.ReadAllText(AEntryPath)));
  OutputLines := TStringList.Create;
  Resolver := TGocciaSandboxModuleResolver.Create(FContext.Fs, '/');
  Provider := TGocciaSandboxModuleContentProvider.Create(FContext.Fs);
  Engine := nil;
  Executor := nil;
  PreviousOutputLines := FCurrentOutputLines;
  FCurrentOutputLines := OutputLines;
  try
    if EngineOptions.Mode.Matches(emBytecode) then
    begin
      BytecodeExecutor := TGocciaBytecodeExecutor.Create;
      BytecodeExecutor.GlobalBackedTopLevel := True;
      Executor := BytecodeExecutor;
    end
    else
    begin
      InterpreterExecutor := TGocciaInterpreterExecutor.Create;
      Executor := InterpreterExecutor;
    end;

    Engine := TGocciaEngine.Create(AEntryPath, Source, Resolver, Executor);
    Engine.ModuleLoader.SetContentProvider(Provider, True);
    Provider := nil;
    ConfigureEngineForSandbox(Engine, AEntryPath);

    try
      StartExecutionTimeout(EngineOptions.Timeout.ValueOr(0));
      StartInstructionLimit(EngineOptions.MaxInstructions.ValueOr(0));
      ScriptResult := Engine.Execute;
      Result.ResultValue := ClonePrimitiveResult(ScriptResult.Result);
      Result.Ok := True;
      Result.ExitCode := 0;
    finally
      ClearExecutionTimeout;
      ClearInstructionLimit;
    end;
  except
    on E: TGocciaError do
      Result.ErrorMessage := E.GetDetailedMessage(False);
    on E: TGocciaThrowValue do
      Result.ErrorMessage := FormatThrowDetail(E.Value, AEntryPath, Source,
        False, E.Suggestion);
    on E: Exception do
      Result.ErrorMessage := E.Message;
  end;

  Result.Output := OutputLines.Text;
  if Result.ErrorMessage <> '' then
    Result.ErrorOutput := Result.ErrorMessage + LineEnding;
  Engine.Free;
  Executor.Free;
  Resolver.Free;
  Provider.Free;
  FCurrentOutputLines := PreviousOutputLines;
  OutputLines.Free;
  Source.Free;
end;

procedure TSandboxRunnerApp.WriteDiffIfRequested;
var
  DiffText: string;
  OutFile: TextFile;
begin
  if not FDiff.Present and not FDiffOutput.Present then
    Exit;
  if FDiffFormat.ValueOr('json') = 'unified' then
    DiffText := FContext.DiffUnified
  else
    DiffText := FContext.DiffJson;

  if FDiffOutput.Present then
  begin
    AssignFile(OutFile, FDiffOutput.Value);
    Rewrite(OutFile);
    try
      Write(OutFile, DiffText);
    finally
      CloseFile(OutFile);
    end;
  end
  else
    Write(DiffText);
end;

procedure TSandboxRunnerApp.ExecuteWithPaths(const APaths: TStringList);
var
  EntryPath: string;
  RunResult: TGocciaSandboxRunResult;
begin
  if APaths.Count <> 1 then
  begin
    WriteLn(StdErr, 'Error: expected one sandbox entry path.');
    ExitCode := 1;
    Exit;
  end;

  FContext.Free;
  FContext := TGocciaSandboxContext.Create;
  FContext.RunScriptCallback := ExecuteSandboxPath;
  LoadSeeds;
  FContext.CaptureBaseline;

  EntryPath := FContext.Fs.Normalize(APaths[0]);
  RunResult := ExecuteSandboxPath(EntryPath);
  if RunResult.Output <> '' then
    Write(RunResult.Output);
  if RunResult.ErrorOutput <> '' then
    Write(StdErr, RunResult.ErrorOutput);
  if FPrint.Present and Assigned(RunResult.ResultValue) and
     not (RunResult.ResultValue is TGocciaUndefinedLiteralValue) then
    WriteLn(RunResult.ResultValue.ToStringLiteral.Value);
  if not RunResult.Ok then
    ExitCode := RunResult.ExitCode;
  WriteDiffIfRequested;
end;

var
  RunResult: Integer;
begin
  RunResult := TGocciaApplication.RunApplication(TSandboxRunnerApp,
    'GocciaSandboxRunner');
  if RunResult <> 0 then
    ExitCode := RunResult;
end.
