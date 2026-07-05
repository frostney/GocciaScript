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
  Goccia.Builtins.GlobalShadowRealm,
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
  Goccia.Realm,
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

    procedure EnsureSandboxParentDirectory(const AContext:
      TGocciaSandboxContext; const APath: string);
    procedure ImportVirtualFile(const ASourceFs,
      ADestinationFs: TSandboxVirtualFileSystem; const ASourcePath,
      ADestinationPath: string; const ADestinationIsDirectory: Boolean);
    procedure ImportVirtualDirectoryContents(const ASourceFs,
      ADestinationFs: TSandboxVirtualFileSystem; const ASourceDirectory,
      ADestinationDirectory: string);
    procedure ImportVirtualPath(const ASourceFs,
      ADestinationFs: TSandboxVirtualFileSystem; const ASourcePath,
      ADestinationPath: string; const ADestinationIsDirectory: Boolean);
    procedure SeedNestedContext(const AParentContext,
      AChildContext: TGocciaSandboxContext;
      const AOptions: TGocciaSandboxRunOptions);
    procedure ConfigureSandboxResolver(
      const AResolver: TGocciaSandboxModuleResolver);
    procedure ConfigureEngineForSandbox(const AEngine: TGocciaEngine;
      const AContext: TGocciaSandboxContext; const AFileName: string);
    procedure CaptureConsoleLine(const AMethod, ALine: string);
    function ExecuteSandboxPathInContext(const AContext: TGocciaSandboxContext;
      const AEntryPath: string): TGocciaSandboxRunResult;
    function ExecuteSandboxPath(const AContext: TGocciaSandboxContext;
      const AEntryPath: string; const AOptions: TGocciaSandboxRunOptions):
      TGocciaSandboxRunResult;
    function CloneResultValue(const AValue: TGocciaValue): TGocciaValue;
    function CloneResultValueRecursive(const AValue: TGocciaValue;
      const ASeen: TList): TGocciaValue;
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
var
  Path: string;
begin
  Path := NormalizeSandboxPathSeparators(APath);
  if Path = '' then
    Exit('/');
  if Path[1] = '/' then
    Result := Path
  else
    Result := '/' + Path;
end;

function HostPathFromConfig(const ABaseDirectory, APath: string): string;
begin
  if ExtractFileDrive(APath) <> '' then
    Exit(ExpandFileName(APath));
  if (APath <> '') and (APath[1] = PathDelim) then
    Exit(ExpandFileName(APath));
  Result := ExpandFileName(IncludeTrailingPathDelimiter(ABaseDirectory) + APath);
end;

function SandboxPathParent(const APath: string): string;
var
  Path: string;
  SlashIndex: Integer;
begin
  Path := NormalizeSandboxPathSeparators(APath);
  Result := '/';
  SlashIndex := Length(Path);
  while (SlashIndex > 1) and (Path[SlashIndex] <> '/') do
    Dec(SlashIndex);
  if SlashIndex > 1 then
    Result := Copy(Path, 1, SlashIndex - 1);
end;

function SandboxPathName(const APath: string): string;
var
  Path: string;
  SlashIndex: Integer;
begin
  Path := NormalizeSandboxPathSeparators(APath);
  SlashIndex := Length(Path);
  while (SlashIndex > 0) and (Path[SlashIndex] <> '/') do
    Dec(SlashIndex);
  Result := Copy(Path, SlashIndex + 1, MaxInt);
end;

function SandboxPathHasTrailingSeparator(const APath: string): Boolean;
var
  Path: string;
begin
  Path := NormalizeSandboxPathSeparators(APath);
  Result := (Path <> '') and (Path[Length(Path)] = '/');
end;

function SandboxJoinPath(const ABase, AName: string): string;
var
  Base: string;
begin
  Base := NormalizeSandboxPathSeparators(ABase);
  if Base = '/' then
    Result := '/' + AName
  else
    Result := Base + '/' + AName;
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
  SandboxDirectory: string;
  SandboxChild: string;
begin
  SandboxDirectory := FContext.Fs.Normalize(EnsureSandboxAbsolute(
    ASandboxDirectory));
  FContext.Fs.MakeDirectory(SandboxDirectory, True);
  if FindFirst(IncludeTrailingPathDelimiter(AHostDirectory) + '*',
     faAnyFile, SearchRec) <> 0 then
    Exit;
  try
    repeat
      if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
        Continue;
      HostChild := IncludeTrailingPathDelimiter(AHostDirectory) + SearchRec.Name;
      if HostPathIsSymlink(HostChild) then
        raise Exception.Create('Seed path is a symlink (not supported): ' + HostChild);
      SandboxChild := SandboxJoinPath(SandboxDirectory, SearchRec.Name);
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
  // Strip any trailing separator before the symlink check: POSIX lstat()
  // follows a final symlink when the path ends in '/', so without this a
  // symlinked seed leaf would be rejected as `linkdir` but dereferenced as
  // `linkdir/`. Downstream directory/file imports are delimiter-agnostic.
  HostPath := ExcludeTrailingPathDelimiter(ExpandFileName(AHostPath));
  if HostPathIsSymlink(HostPath) then
    raise Exception.Create('Seed path is a symlink (not supported): ' + HostPath);
  if not FileExists(HostPath) and not DirectoryExists(HostPath) then
    raise Exception.Create('Seed path does not exist: ' + HostPath);

  TargetPath := EnsureSandboxAbsolute(ASandboxPath);

  if IsDirectoryPath(HostPath) then
    ImportDirectoryContents(HostPath, TargetPath)
  else
  begin
    if (TargetPath = '/') or SandboxPathHasTrailingSeparator(ASandboxPath) or
       FContext.Fs.IsDirectory(TargetPath) then
      TargetPath := FContext.Fs.Normalize(SandboxJoinPath(TargetPath,
        ExtractFileName(HostPath)));
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
  SourceCount := 0;
  if JsonHasProperty(AEntry, 'from') then Inc(SourceCount);
  if JsonHasProperty(AEntry, 'text') then Inc(SourceCount);
  if JsonHasProperty(AEntry, 'base64') then Inc(SourceCount);
  if SourceCount <> 1 then
    raise Exception.Create(
      'seed file entries must specify exactly one of from, text, or base64');

  if JsonHasProperty(AEntry, 'from') then
  begin
    FromPath := JsonStringProperty(AEntry, 'from', True);
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
  begin
    Text := JsonStringProperty(AEntry, 'text', True);
    SeedInlineFile(InlinePath, Text, nil, False);
  end;
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

procedure TSandboxRunnerApp.EnsureSandboxParentDirectory(
  const AContext: TGocciaSandboxContext; const APath: string);
begin
  AContext.Fs.MakeDirectory(SandboxPathParent(APath), True);
end;

procedure TSandboxRunnerApp.ImportVirtualFile(const ASourceFs,
  ADestinationFs: TSandboxVirtualFileSystem; const ASourcePath,
  ADestinationPath: string; const ADestinationIsDirectory: Boolean);
var
  SourceStat: TSandboxFsStat;
  TargetPath: string;
begin
  SourceStat := ASourceFs.Stat(ASourcePath);
  TargetPath := ADestinationFs.Normalize(EnsureSandboxAbsolute(
    ADestinationPath));
  if ADestinationIsDirectory or (TargetPath = '/') or
     SandboxPathHasTrailingSeparator(ADestinationPath) or
     ADestinationFs.IsDirectory(TargetPath) then
    TargetPath := ADestinationFs.Normalize(SandboxJoinPath(TargetPath,
      SourceStat.Name));
  ADestinationFs.MakeDirectory(SandboxPathParent(TargetPath), True);
  ADestinationFs.WriteAllBytes(TargetPath, ASourceFs.ReadAllBytes(
    ASourcePath));
end;

procedure TSandboxRunnerApp.ImportVirtualDirectoryContents(const ASourceFs,
  ADestinationFs: TSandboxVirtualFileSystem; const ASourceDirectory,
  ADestinationDirectory: string);
var
  Entries: TSandboxFsStatArray;
  TargetDirectory: string;
  TargetChild: string;
  I: Integer;
begin
  TargetDirectory := ADestinationFs.Normalize(EnsureSandboxAbsolute(
    ADestinationDirectory));
  ADestinationFs.MakeDirectory(TargetDirectory, True);
  Entries := ASourceFs.List(ASourceDirectory);
  for I := 0 to High(Entries) do
  begin
    TargetChild := SandboxJoinPath(TargetDirectory, Entries[I].Name);
    if Entries[I].Kind = nkDirectory then
      ImportVirtualDirectoryContents(ASourceFs, ADestinationFs,
        Entries[I].Path, TargetChild)
    else
      ImportVirtualFile(ASourceFs, ADestinationFs, Entries[I].Path,
        TargetChild, False);
  end;
end;

procedure TSandboxRunnerApp.ImportVirtualPath(const ASourceFs,
  ADestinationFs: TSandboxVirtualFileSystem; const ASourcePath,
  ADestinationPath: string; const ADestinationIsDirectory: Boolean);
var
  SourceStat: TSandboxFsStat;
  TargetPath: string;
begin
  SourceStat := ASourceFs.Stat(ASourcePath);
  TargetPath := ADestinationFs.Normalize(EnsureSandboxAbsolute(
    ADestinationPath));
  if SourceStat.Kind = nkDirectory then
    ImportVirtualDirectoryContents(ASourceFs, ADestinationFs, ASourcePath,
      TargetPath)
  else
    ImportVirtualFile(ASourceFs, ADestinationFs, ASourcePath, TargetPath,
      ADestinationIsDirectory);
end;

procedure TSandboxRunnerApp.SeedNestedContext(const AParentContext,
  AChildContext: TGocciaSandboxContext;
  const AOptions: TGocciaSandboxRunOptions);
var
  Seed: TGocciaSandboxSeedSpec;
begin
  for Seed in AOptions.Seeds do
  begin
    case Seed.Kind of
      sskParentPath:
        ImportVirtualPath(AParentContext.Fs, AChildContext.Fs, Seed.FromPath,
          Seed.ToPath, Seed.ToDirectory);
      sskText:
      begin
        EnsureSandboxParentDirectory(AChildContext, Seed.Path);
        AChildContext.Fs.WriteAllText(Seed.Path, Seed.Text);
      end;
      sskBytes:
      begin
        EnsureSandboxParentDirectory(AChildContext, Seed.Path);
        AChildContext.Fs.WriteAllBytes(Seed.Path, Seed.Bytes);
      end;
    end;
  end;
end;

procedure TSandboxRunnerApp.ConfigureEngineForSandbox(
  const AEngine: TGocciaEngine; const AContext: TGocciaSandboxContext;
  const AFileName: string);
var
  Runtime: TGocciaRuntimeCore;
  ConsoleExtension: TGocciaConsoleRuntimeExtension;
  EmptyConfig: TConfigEntryArray;
  Compatibility: TGocciaCompatibilityFlags;
begin
  EmptyConfig := EmptyConfigEntries;
  AEngine.SourceType := ResolveSourceTypeOption(EngineOptions.SourceType,
    EmptyConfig, AFileName);
  ResolveCompatibilityFlags(EngineOptions, EmptyConfig, Compatibility);
  AEngine.Compatibility := Compatibility;
  AEngine.WarningUnsupportedFeatures := ResolveFlagOption(
    EngineOptions.WarningUnsupportedFeatures, EmptyConfig);
  AEngine.StrictTypes := ResolveFlagOption(EngineOptions.StrictTypes,
    EmptyConfig);
  AEngine.FunctionConstructor.Enabled := ResolveFlagOption(
    EngineOptions.UnsafeFunctionConstructor, EmptyConfig);
  if ResolveFlagOption(EngineOptions.UnsafeShadowRealm, EmptyConfig) then
    EnableShadowRealm(AEngine);

  Runtime := AttachRuntime(AEngine);
  ApplyLoaderRuntimeProfile(Runtime);
  Runtime.Install(TGocciaSandboxRuntimeExtension.Create(AContext));
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

procedure TSandboxRunnerApp.ConfigureSandboxResolver(
  const AResolver: TGocciaSandboxModuleResolver);
var
  AliasSpec, AliasKey, AliasValue: string;
  I, SeparatorIndex: Integer;
begin
  if not Assigned(AResolver) then
    Exit;

  if EngineOptions.ImportMap.Present then
    AResolver.LoadImportMap(EngineOptions.ImportMap.Value);

  for I := 0 to EngineOptions.Aliases.Values.Count - 1 do
  begin
    AliasSpec := EngineOptions.Aliases.Values[I];
    SeparatorIndex := Pos('=', AliasSpec);
    if SeparatorIndex <= 1 then
      raise Exception.Create('Invalid --alias argument. Use --alias key=value.');

    AliasKey := Copy(AliasSpec, 1, SeparatorIndex - 1);
    AliasValue := Copy(AliasSpec, SeparatorIndex + 1, MaxInt);
    if AliasValue = '' then
      raise Exception.Create('Invalid --alias argument. Use --alias key=value.');

    AResolver.AddAlias(AliasKey, AliasValue);
  end;
end;

function TSandboxRunnerApp.CloneResultValue(
  const AValue: TGocciaValue): TGocciaValue;
var
  Seen: TList;
begin
  Seen := TList.Create;
  try
    Result := CloneResultValueRecursive(AValue, Seen);
  finally
    Seen.Free;
  end;
end;

function TSandboxRunnerApp.CloneResultValueRecursive(
  const AValue: TGocciaValue; const ASeen: TList): TGocciaValue;
var
  ArrayValue, ClonedArray: TGocciaArrayValue;
  ObjectValue, ClonedObject: TGocciaObjectValue;
  Key: string;
  I: Integer;
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

  if ASeen.IndexOf(AValue) >= 0 then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);

  if AValue is TGocciaArrayValue then
  begin
    ASeen.Add(AValue);
    try
      ArrayValue := TGocciaArrayValue(AValue);
      ClonedArray := TGocciaArrayValue.Create(nil, ArrayValue.GetLength);
      for I := 0 to ArrayValue.GetLength - 1 do
        ClonedArray.SetElement(I,
          CloneResultValueRecursive(ArrayValue.GetElement(I), ASeen));
      Exit(ClonedArray);
    finally
      ASeen.Remove(AValue);
    end;
  end;

  if AValue is TGocciaObjectValue then
  begin
    ASeen.Add(AValue);
    try
      ObjectValue := TGocciaObjectValue(AValue);
      ClonedObject := TGocciaObjectValue.Create(
        TGocciaObjectValue.SharedObjectPrototype, 8);
      for Key in ObjectValue.GetOwnPropertyKeys do
        ClonedObject.SetProperty(Key,
          CloneResultValueRecursive(ObjectValue.GetProperty(Key), ASeen));
      Exit(ClonedObject);
    finally
      ASeen.Remove(AValue);
    end;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TSandboxRunnerApp.ExecuteSandboxPathInContext(
  const AContext: TGocciaSandboxContext; const AEntryPath: string):
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
  CloneRealm, ExecutionRealm: TGocciaRealm;
  PreviousOutputLines: TStrings;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Ok := False;
  Result.ExitCode := 1;

  if not AContext.Fs.IsFile(AEntryPath) then
  begin
    Result.ErrorMessage := 'sandbox entry file not found: ' + AEntryPath;
    Exit;
  end;

  Source := CreateUTF8FileTextLines(
    UTF8String(AContext.Fs.ReadAllText(AEntryPath)));
  OutputLines := TStringList.Create;
  Resolver := TGocciaSandboxModuleResolver.Create(AContext.Fs, '/');
  Provider := TGocciaSandboxModuleContentProvider.Create(AContext.Fs);
  Engine := nil;
  Executor := nil;
  CloneRealm := CurrentRealm;
  PreviousOutputLines := FCurrentOutputLines;
  FCurrentOutputLines := OutputLines;
  try
    ConfigureSandboxResolver(Resolver);

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
    ConfigureEngineForSandbox(Engine, AContext, AEntryPath);

    try
      StartExecutionTimeout(EngineOptions.Timeout.ValueOr(0));
      StartInstructionLimit(EngineOptions.MaxInstructions.ValueOr(0));
      ScriptResult := Engine.Execute;
      ExecutionRealm := CurrentRealm;
      try
        SetCurrentRealm(CloneRealm);
        Result.ResultValue := CloneResultValue(ScriptResult.Result);
      finally
        SetCurrentRealm(ExecutionRealm);
      end;
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

function TSandboxRunnerApp.ExecuteSandboxPath(
  const AContext: TGocciaSandboxContext; const AEntryPath: string;
  const AOptions: TGocciaSandboxRunOptions): TGocciaSandboxRunResult;
var
  ChildContext: TGocciaSandboxContext;
begin
  if not AOptions.Isolated then
    Exit(ExecuteSandboxPathInContext(AContext, AEntryPath));

  FillChar(Result, SizeOf(Result), 0);
  Result.Ok := False;
  Result.ExitCode := 1;
  ChildContext := TGocciaSandboxContext.Create;
  try
    ChildContext.RunScriptCallback := ExecuteSandboxPath;
    SeedNestedContext(AContext, ChildContext, AOptions);
    ChildContext.CaptureBaseline;
    Result := ExecuteSandboxPathInContext(ChildContext,
      ChildContext.Fs.Normalize(AEntryPath));
    if AOptions.IncludeDiff then
    begin
      Result.DiffRequested := True;
      if AOptions.DiffFormat = 'unified' then
        Result.Diff := ChildContext.DiffUnified
      else
        Result.Diff := ChildContext.DiffJson;
    end;
  except
    on E: Exception do
    begin
      Result.Ok := False;
      Result.ExitCode := 1;
      Result.ErrorMessage := E.Message;
      Result.ErrorOutput := E.Message + LineEnding;
    end;
  end;
  ChildContext.Free;
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
  RunResult := ExecuteSandboxPathInContext(FContext, EntryPath);
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
