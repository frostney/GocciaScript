unit Goccia.CLI.FileEngine;

{ What the CLIs that run files (GocciaRunner, GocciaTestRunner) share for one file's engine: the console
  capture, the file's settings and host config, the file thread's stack, and the import capability over the file's
  module resolution (the node_modules and provider package grants, their audit, and the provider package checks the
  read policy and the module loader ask for, ADR 0122). }

{$I Goccia.inc}

interface

uses
  Classes,
  SysUtils,

  CLI.ConfigFile,
  CLI.Options,
  SandboxVirtualFileSystem,

  Goccia.Capabilities,
  Goccia.CapabilityAudit,
  Goccia.CLI.FileSettings,
  Goccia.CLI.JSON.Reporter,
  Goccia.CLI.Options,
  Goccia.Embed,
  Goccia.Modules.Resolver,
  Goccia.Packages.Address;

type
  { The console of a JSON run, as GocciaRunner's TScriptLoaderConsoleCapture: every line in output, and each
    method's line, without the prefix the console gave it, on its own stream }
  TGocciaConsoleCapture = class
  private
    FOutputLines: TStringList;
    FStdoutLines: TStringList;
    FStderrLines: TStringList;
  public
    FileName: string;
    { the console's lines go to the capture (a JSON report), are dropped (--silent), or go to stdout; and to the
      --log file through LogLine when it is open }
    Capturing, Silent: Boolean;
    LogLine: procedure(const AMethod, ALine: string) of object;
    procedure Emit(const AMethod, ALine: UnicodeString);
    constructor Create;
    destructor Destroy; override;
    procedure CaptureOutput(const AMethod, ALine: UnicodeString);
    procedure Discard(const AMethod, ALine: UnicodeString);
    { a parse warning as GocciaRunner prints it (PrintPipelineWarnings); FileName set by the runner }
    procedure PrintWarning(const AMessage, ASuggestion: UnicodeString; ALine, AColumn: Integer);
    { an imported module's parse warning, printed as GocciaRunner's module loader prints it }
    procedure PrintModuleWarning(const AFileName, AMessage, ASuggestion: UnicodeString; ALine, AColumn: Integer);
    function OutputText: string;
    function StdoutText: string;
    function StderrText: string;
  end;

  { the import capability over a file's resolution, as TGocciaEngine grants it: node_modules (GrantNodeModules: the
    walk's ceiling when import grants it, a refusal when it is denied, sealed otherwise) and provider packages
    (GrantProviderPackage: an import scope covering the repository, ADR 0122), whose files are checked against
    their pins as they load }
  TGocciaImportGrant = class
  private
    FCapabilities: TGocciaCapabilities;
    procedure AuditOrFail(AKind: TGocciaCapabilityKind; AAllowed: Boolean; const ASubject, AReason: string);
  public
    { the file's engine, which audits each decision (nil: none audited) }
    Engine: TGocciaEngine;
    { the file's resolver, which materializes the provider packages }
    Resolver: TGocciaModuleResolver;
    constructor Create(const ACapabilities: TGocciaCapabilities);
    function Grant(const ASpecifier, AImportingDirectory: string; out ACeiling: string): Boolean;
    procedure GrantProvider(const ASpecifier: string; const AAddress: TGocciaProviderAddress);
    procedure AuditProvider(const AAllowed: Boolean; const ASubject, AReason: string);
    procedure CheckContent(const APath: UnicodeString; const B: RawByteString);
    function ProviderGraph(const APath, AImportingFile, APackageRoot: string): Boolean;
  end;

const
  { reserved, not committed: room for native recursion as deep as the main thread's }
  FILE_THREAD_STACK = 64 * 1024 * 1024;

var
  { 1 while a running file thread compiles with the JIT: one compiles at a time (its emitter is shared) }
  JitOwned: LongInt = 0;

{ the host config the preludes read (Goccia.Embed ConfigJson): experimental-ast installs goccia:ast }
function HostConfigJson(const AEngineOptions: TGocciaEngineOptions; const AFileConfig: TConfigEntryArray): UnicodeString;
{ a file's resolved settings on its engine: the language profile, the source type and the per-file limits }
procedure ApplySettings(const AEngine: TGocciaEngine; const ASettings: TGocciaFileSettings);

{ a path a config names, against the config's directory (Goccia.CLI.Application's ConfigInputPath) }
function ConfigInputPath(const AWritten, AConfigPath: string): string;
{ a file's globals (GocciaRunner's ApplyDataGlobalsToEngine): the root config's globals files (AConfigEntries, read
  as the script's), then the command line's, then each --global name=value }
function InjectGlobals(const AEngine: TGocciaEngine; const AConfigEntries: TConfigEntryArray;
  const AGlobalFiles, AInlineGlobals: TRepeatableOption; out AError: string): Boolean;
{ a file's virtual modules (GocciaRunner's ApplyConfiguredVirtualModules): the root config's, the file's own
  config's when it has another, then the command line's }
function DefineVirtualModules(const AEngine: TGocciaEngine; const ARootConfigPath, AFileConfigPath: string;
  const AEngineOptions: TGocciaEngineOptions; out AError: string): Boolean;
{ the command line's --modules manifests and --module definitions, the user's own }
function DefineCommandLineVirtualModules(const AEngine: TGocciaEngine; const AEngineOptions: TGocciaEngineOptions;
  out AError: string): Boolean;

{ one file's memory report from its engine's counters and the memory manager's, before and after the file }
function FileMemoryStats(const AStart, AEnd: TGocciaMemoryStats; const AHeapStart, AHeapEnd: THeapStatus):
  TCLIJSONMemoryStats;
{ a multi-file report's top-level memory: the files' collections and allocations summed. At once (AParallel) the
  engines' heaps coexist, so their live and peak bytes add up, as GocciaRunner combines its workers'; one after
  another each heap is freed before the next, so the peak is the largest and the live bytes are the last file's.
  memory.heap is the main thread's memory manager over the run. }
function AggregateMemoryStats(const AStats: array of TCLIJSONMemoryStats; AParallel: Boolean;
  const AHeapStart, AHeapEnd: THeapStatus; AWorkers: Integer = 0): TCLIJSONMemoryStats;

{ GocciaScript's suggestion for a runtime error's message, '' for none }
function RuntimeSuggestion(const AMessage: string): string;
{ a failed run's report as text, as GocciaRunner prints it: the error, its suggestion, where it was thrown and a code
  frame from the file it is in. AEngine says which files the run loaded; AWithheld names a file whose source no
  report shows; AFrameSource: the virtual filesystem a sandbox run's modules come from (nil: the host's files);
  AFrames: a code frame at all; ASuggestions: the host's suggestions; ATimeoutMs and AMaxInstructions: the limits
  a spent one names }
function FailureText(const AFileName: string; const AResult: TGocciaRunResult; const ASource: TStringList;
  AEngine: TGocciaEngine; const AWithheld: string; const AFrameSource: TSandboxVirtualFileSystem;
  AFrames, ASuggestions, AColor: Boolean; ATimeoutMs, AMaxInstructions: Int64): string;

implementation

uses
  Math,

  ConsoleStreams,
  TextEncoding,
  TextSemantics,

  Goccia.Core,
  Goccia.Error,
  Goccia.Error.Suggestions,
  Goccia.HostAPI,
  Goccia.Packages.Store,
  Goccia.ScriptLoader.Globals,
  Goccia.SourcePipeline;

function HostConfigJson(const AEngineOptions: TGocciaEngineOptions; const AFileConfig: TConfigEntryArray): UnicodeString;
begin
  if ResolveFlagOption(AEngineOptions.ExperimentalAST, AFileConfig) then
    Result := '{"experimental-ast":"true"}'
  else
    Result := '{}';
end;

procedure ApplySettings(const AEngine: TGocciaEngine;
  const ASettings: TGocciaFileSettings);
var
  Profile: TLangProfile;
begin
  Profile := RecommendedProfile;
  Profile.Asi := cfASI in ASettings.Compatibility;
  Profile.VarDecl := cfVar in ASettings.Compatibility;
  Profile.FuncDecl := cfFunction in ASettings.Compatibility;
  Profile.ForLoop := cfTraditionalFor in ASettings.Compatibility;
  Profile.WhileLoop := cfWhileLoops in ASettings.Compatibility;
  Profile.LooseEq := cfLooseEquality in ASettings.Compatibility;
  Profile.NonStrict := cfNonStrictMode in ASettings.Compatibility;
  Profile.ArgumentsObject := cfArgumentsObject in ASettings.Compatibility;
  Profile.Labels := ASettings.LabelStatements;
  Profile.ForIn := ASettings.ForInLoops;
  Profile.WarningUnsupported := ASettings.WarningUnsupportedFeatures;
  Profile.StrictTypes := ASettings.StrictTypes;
  Profile.FunctionCtor := ASettings.FunctionConstructor;
  Profile.ShadowRealm := ASettings.ShadowRealm;
  AEngine.Profile := Profile;
  case ASettings.SourceType of
    fstScript: AEngine.SourceKind := gskScript;
    fstModule: AEngine.SourceKind := gskModule;
  else
    AEngine.SourceKind := gskDetect;
  end;
  AEngine.FetchMaxResponseBytes := ASettings.FetchMaxResponseBytes;
  AEngine.InspectDepth := ASettings.InspectDepth;
  AEngine.JSModuleSources := ASettings.ExperimentalJSModuleSource;
  if ASettings.MaxMemorySet then
    AEngine.MaxMemory := ASettings.MaxMemory
  else
    AEngine.MaxMemory := 0;
end;

constructor TGocciaConsoleCapture.Create;
begin
  inherited Create;
  FOutputLines := TStringList.Create;
  FStdoutLines := TStringList.Create;
  FStderrLines := TStringList.Create;
end;

destructor TGocciaConsoleCapture.Destroy;
begin
  FStderrLines.Free;
  FStdoutLines.Free;
  FOutputLines.Free;
  inherited Destroy;
end;

function UnformatLine(const AMethod, ALine: string): string;
var
  Prefix: string;
  PrefixStart: Integer;
begin
  Prefix := '';
  if AMethod = 'warn' then
    Prefix := 'Warning: '
  else if AMethod = 'error' then
    Prefix := 'Error: '
  else if AMethod = 'info' then
    Prefix := 'Info: '
  else if AMethod = 'debug' then
    Prefix := 'Debug: '
  else if AMethod = 'trace' then
    Prefix := 'Trace: ';
  if Prefix = '' then
    Exit(ALine);
  PrefixStart := 1;
  while (PrefixStart <= Length(ALine)) and (ALine[PrefixStart] = ' ') do
    Inc(PrefixStart);
  if Copy(ALine, PrefixStart, Length(Prefix)) = Prefix then
    Result := Copy(ALine, 1, PrefixStart - 1) +
      Copy(ALine, PrefixStart + Length(Prefix), MaxInt)
  else
    Result := ALine;
end;

procedure TGocciaConsoleCapture.CaptureOutput(const AMethod, ALine: UnicodeString);
var
  Method, Line: string;
begin
  { as they are: an explicit UTF8Encode drops an unpaired surrogate, which the output's encoder
    (ProcessEncoding) writes as U+FFFD }
  Method := AMethod;
  Line := ALine;
  FOutputLines.Add(Line);
  if ConsoleMethodWritesToStderr(Method) then
    FStderrLines.Add(UnformatLine(Method, Line))
  else
    FStdoutLines.Add(UnformatLine(Method, Line));
end;

{ --silent }
procedure TGocciaConsoleCapture.Discard(const AMethod, ALine: UnicodeString);
begin
end;

procedure TGocciaConsoleCapture.Emit(const AMethod, ALine: UnicodeString);
begin
  { the line as it is: the --log writer and the output's encoder write an unpaired surrogate as U+FFFD, which an
    explicit UTF8Encode would drop }
  if Assigned(LogLine) then
    LogLine(AMethod, ALine);
  if Capturing then
    CaptureOutput(AMethod, ALine)
  else if not Silent then
    WriteConsoleLine(ConsoleMethodWritesToStderr(string(AMethod)), ALine);
end;

procedure TGocciaConsoleCapture.PrintWarning(const AMessage, ASuggestion: UnicodeString;
  ALine, AColumn: Integer);
begin
  WriteLn('Warning: ', UTF8Encode(AMessage));
  if ASuggestion <> '' then
    WriteLn('  Suggestion: ', UTF8Encode(ASuggestion));
  WriteLn(Format('  --> %s:%d:%d', [FileName, ALine, AColumn]));
end;

procedure TGocciaConsoleCapture.PrintModuleWarning(const AFileName, AMessage, ASuggestion: UnicodeString;
  ALine, AColumn: Integer);
begin
  WriteLn('Warning: ', UTF8Encode(AMessage));
  if ASuggestion <> '' then
    WriteLn('  Suggestion: ', UTF8Encode(ASuggestion));
  WriteLn(Format('  --> %s:%d:%d', [UTF8Encode(AFileName), ALine, AColumn]));
end;

function TGocciaConsoleCapture.OutputText: string;
begin
  Result := FOutputLines.Text;
end;

function TGocciaConsoleCapture.StdoutText: string;
begin
  Result := FStdoutLines.Text;
end;

function TGocciaConsoleCapture.StderrText: string;
begin
  Result := FStderrLines.Text;
end;

constructor TGocciaImportGrant.Create(const ACapabilities: TGocciaCapabilities);
begin
  inherited Create;
  FCapabilities := ACapabilities;
end;

{ every decision is audited, as GocciaRunner's grants do; a log that cannot be written ends the run }
procedure TGocciaImportGrant.AuditOrFail(AKind: TGocciaCapabilityKind; AAllowed: Boolean;
  const ASubject, AReason: string);
begin
  if Assigned(Engine) and not Engine.Audit(AKind, AAllowed, ASubject, AReason) then
    raise EGocciaCapabilityAuditDeliveryError.Create('capability audit delivery failed');
end;

function TGocciaImportGrant.Grant(const ASpecifier, AImportingDirectory: string;
  out ACeiling: string): Boolean;
var
  Reason: string;
begin
  Result := FCapabilities.NodeModulesCeiling(AImportingDirectory, ACeiling);
  if Result then
  begin
    if ACeiling = '' then
      Reason := 'node_modules walk is unbounded'
    else
      Reason := 'node_modules walk is bounded by ' + ACeiling;
    AuditOrFail(gckImportNodeModules, True, ASpecifier, Reason);
    Exit;
  end;
  AuditOrFail(gckImportNodeModules, False, ASpecifier, 'the import capability does not grant node_modules here');
  { an outright deny refuses; a capability never granted keeps the sealed resolution's message }
  if FCapabilities.DeniesNodeModules(AImportingDirectory) then
    raise EGocciaPermissionDenied.CreateDenied(CapabilityName(gcImport), ASpecifier,
      'node_modules resolution is denied for ' + AImportingDirectory);
end;

{ a provider import needs an import scope covering its repository: github, github:<owner> or
  github:<owner>/<repo>; the PermissionDenied names the package, the suggestion the grants that would cover it or
  the deny that refused it }
procedure TGocciaImportGrant.GrantProvider(const ASpecifier: string; const AAddress: TGocciaProviderAddress);
var
  DenyScope, Owner, Suggestion: string;
begin
  if FCapabilities.AllowsProviderPackage(IMPORT_PROVIDER_GITHUB, AAddress.Owner, AAddress.Repository) then
  begin
    AuditOrFail(gckImportProvider, True, AAddress.PackageKey, 'the import capability covers ' + AAddress.ScopeText);
    Exit;
  end;
  if FCapabilities.ProviderPackageDenyScope(IMPORT_PROVIDER_GITHUB, AAddress.Owner, AAddress.Repository,
     DenyScope) then
  begin
    if DenyScope = '' then
      Suggestion := 'refused by an unscoped import deny'
    else
      Suggestion := 'refused by the import deny ' + DenyScope;
    AuditOrFail(gckImportProvider, False, AAddress.PackageKey, Suggestion);
  end
  else
  begin
    Owner := LowerCase(GITHUB_PROVIDER_PREFIX + AAddress.Owner);
    Suggestion := Format(SSuggestProviderImportNotGranted, [AAddress.ScopeText, AAddress.ScopeText, Owner]);
    AuditOrFail(gckImportProvider, False, AAddress.PackageKey,
      'the import capability does not cover ' + AAddress.ScopeText);
  end;
  raise EGocciaPermissionDenied.CreateDenied(CapabilityName(gcImport), AAddress.PackageKey, Suggestion);
end;

{ the package store's decisions: a pin it verified or refused }
procedure TGocciaImportGrant.AuditProvider(const AAllowed: Boolean; const ASubject, AReason: string);
begin
  AuditOrFail(gckImportProvider, AAllowed, ASubject, AReason);
end;

{ GocciaScript's IsProviderGraphPath: a file of a materialized package, reached through the package's import-map
  entry or imported by one of its own files }
function TGocciaImportGrant.ProviderGraph(const APath, AImportingFile, APackageRoot: string): Boolean;
var
  Package: TGocciaMaterializedPackage;
  RelativePath: string;
begin
  Result := False;
  if (APath = '') or not Assigned(Resolver) or not Assigned(Resolver.ProviderPackages) then
    Exit;
  Package := Resolver.ProviderPackages.FindPackage(APath, RelativePath);
  if not Assigned(Package) then
    Exit;
  Result := ((APackageRoot <> '') and (ExcludeTrailingPathDelimiter(APackageRoot) = Package.Root)) or
    ((AImportingFile <> '') and (Resolver.ProviderPackages.FindPackage(AImportingFile, RelativePath) = Package));
end;

{ verify on load: the bytes compiled are the bytes hashed }
procedure TGocciaImportGrant.CheckContent(const APath: UnicodeString; const B: RawByteString);
var
  HostPath: string;
  Bytes: TBytes;
begin
  if not Assigned(Resolver) then
    Exit;
  HostPath := UTF8Encode(APath);
  if not Resolver.IsProviderPackagePath(HostPath) then
    Exit;
  SetLength(Bytes, Length(B));
  if Length(B) > 0 then
    Move(B[1], Bytes[0], Length(B));
  Resolver.VerifyProviderContent(HostPath, Bytes);
end;

function IsAbsoluteFilePath(const APath: string): Boolean;
begin
  Result := (APath <> '') and ((APath[1] = PathDelim) or
    ((Length(APath) >= 2) and (APath[2] = ':')));
end;

{ a path a config names, against the config's directory (Goccia.CLI.Application's ConfigInputPath) }
function ConfigInputPath(const AWritten, AConfigPath: string): string;
begin
  if IsAbsoluteFilePath(AWritten) then
    Result := ExpandFileName(AWritten)
  else
    Result := ExpandFileName(IncludeTrailingPathDelimiter(
      ExtractFileDir(AConfigPath)) + AWritten);
end;

function ReadFileUnicode(const APath: string): UnicodeString;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(APath);
    Result := UTF8DecodeKeepingNoncharacters(Lines.Text);
  finally
    Lines.Free;
  end;
end;

{ GocciaRunner's globals (ApplyDataGlobalsToEngine): the root config's globals files, then the command line's, then
  each --global name=value. Globals modules (a .js file's exports) are not supported yet. }
function InjectGlobals(const AEngine: TGocciaEngine; const AConfigEntries: TConfigEntryArray;
  const AGlobalFiles, AInlineGlobals: TRepeatableOption; out AError: string): Boolean;
var
  Entries: TConfigEntryArray;
  I: Integer;
  Path: string;
  Pair: TScriptLoaderGlobalPair;
  Err: UnicodeString;
begin
  Result := False;
  AError := '';
  Err := '';
  Entries := AConfigEntries;
  { a config's globals are read as the script's: under its read capability, a module evaluated in its engine }
  AEngine.BeginGuestLoads;
  try
    for I := 0 to High(Entries) do
    begin
      Path := ConfigInputPath(Entries[I].Value, Entries[I].SourcePath);
      { judged against the config that names it (an extends base names its own files) }
      AEngine.GuestScope(ExtractFileDir(ExpandFileName(Entries[I].SourcePath)));
      if IsStructuredGlobalsFile(Path) then
      begin
        if not AEngine.GuestReadAllowed(UnicodeString(Path), Err) or
           not AEngine.InjectDataGlobals(ReadFileUnicode(Path), UnicodeString(ExtractFileExt(Path)),
           UnicodeString(Path), Err) then
        begin
          AError := UTF8Encode(Err);
          Exit;
        end;
      end
      else if not AEngine.InjectModuleGlobals(UTF8Decode(Path), Err) then
      begin
        AError := UTF8Encode(Err);
        Exit;
      end;
    end;
  finally
    AEngine.EndGuestLoads;
  end;
  if AGlobalFiles.FromCommandLine then
    for I := 0 to AGlobalFiles.Values.Count - 1 do
    begin
      Path := AGlobalFiles.Values[I];
      if IsStructuredGlobalsFile(Path) then
      begin
        if not AEngine.InjectDataGlobals(ReadFileUnicode(Path), UnicodeString(ExtractFileExt(Path)),
           UnicodeString(Path), Err) then
        begin
          AError := UTF8Encode(Err);
          Exit;
        end;
      end
      { a globals module: its named exports, evaluated in the script's engine }
      else if not AEngine.InjectModuleGlobals(UTF8Decode(Path), Err) then
      begin
        AError := UTF8Encode(Err);
        Exit;
      end;
    end;
  for I := 0 to AInlineGlobals.Values.Count - 1 do
  begin
    Pair := ParseGlobalPair(AInlineGlobals.Values[I]);
    if not AEngine.InjectInlineGlobal(UTF8Decode(Pair.Key), UTF8Decode(Pair.ValueText), Err) then
    begin
      AError := UTF8Encode(Err);
      Exit;
    end;
  end;
  Result := True;
end;

{ GocciaRunner's virtual modules (ApplyConfiguredVirtualModules): the root config's, the file's own config's when
  it has another, then the command line's --modules manifests and --module definitions. JavaScript and TypeScript
  manifests are not supported yet. }
function DefineVirtualModules(const AEngine: TGocciaEngine; const ARootConfigPath, AFileConfigPath: string;
  const AEngineOptions: TGocciaEngineOptions; out AError: string): Boolean;
var
  Err: UnicodeString;
begin
  Result := False;
  AError := '';
  Err := '';
  { a config's manifests are read as the script's, under its read capability }
  AEngine.BeginGuestLoads;
  try
    if (ARootConfigPath <> '') and not AEngine.DefineConfigVirtualModules(UTF8Decode(ARootConfigPath), Err) then
    begin
      AError := UTF8Encode(Err);
      Exit;
    end;
    if (AFileConfigPath <> '') and (ExpandFileName(AFileConfigPath) <> ExpandFileName(ARootConfigPath)) and
       not AEngine.DefineConfigVirtualModules(UTF8Decode(AFileConfigPath), Err) then
    begin
      AError := UTF8Encode(Err);
      Exit;
    end;
  finally
    AEngine.EndGuestLoads;
  end;
  Result := DefineCommandLineVirtualModules(AEngine, AEngineOptions, AError);
end;

{ the command line's --modules manifests and --module definitions, the user's own (sandbox mode applies these and
  not a config's) }
function DefineCommandLineVirtualModules(const AEngine: TGocciaEngine; const AEngineOptions: TGocciaEngineOptions;
  out AError: string): Boolean;
var
  I: Integer;
  Path, Ext: string;
  Err: UnicodeString;
begin
  Result := False;
  AError := '';
  Err := '';
  if AEngineOptions.ModuleManifests.FromCommandLine then
    for I := 0 to AEngineOptions.ModuleManifests.Values.Count - 1 do
    begin
      Path := ExpandFileName(AEngineOptions.ModuleManifests.Values[I]);
      Ext := LowerCase(ExtractFileExt(Path));
      if (Ext = '.js') or (Ext = '.mjs') or (Ext = '.ts') or (Ext = '.mts') then
      begin
        if not AEngine.DefineModuleManifestVirtualModules(UTF8Decode(Path), Err) then
        begin
          AError := UTF8Encode(Err);
          Exit;
        end;
      end
      else if not AEngine.DefineManifestVirtualModules(UTF8Decode(Path), Err) then
      begin
        AError := UTF8Encode(Err);
        Exit;
      end;
    end;
  if AEngineOptions.ModuleDefinitions.FromCommandLine then
    for I := 0 to AEngineOptions.ModuleDefinitions.Values.Count - 1 do
      if not AEngine.DefineInlineVirtualModule(UTF8Decode(AEngineOptions.ModuleDefinitions.Values[I]),
         UTF8Decode(GetCurrentDir), Err) then
      begin
        AError := UTF8Encode(Err);
        Exit;
      end;
  Result := True;
end;

function FileMemoryStats(const AStart, AEnd: TGocciaMemoryStats;
  const AHeapStart, AHeapEnd: THeapStatus): TCLIJSONMemoryStats;
begin
  Result := DefaultCLIJSONMemoryStats;
  Result.Enabled := True;
  Result.GCStartBytes := AStart.LiveBytes;
  Result.GCEndBytes := AEnd.LiveBytes;
  Result.GCLiveBytes := AEnd.LiveBytes;
  Result.GCPeakBytes := AEnd.PeakLiveBytes;
  Result.GCDeltaBytes := AEnd.LiveBytes - AStart.LiveBytes;
  Result.GCAllocatedDuringRunBytes := AEnd.AllocatedBytes - AStart.AllocatedBytes;
  Result.GCMaxBytes := AEnd.LimitBytes;
  Result.GCCollections := AEnd.Collections - AStart.Collections;
  Result.HeapStartAllocatedBytes := Int64(AHeapStart.TotalAllocated);
  Result.HeapEndAllocatedBytes := Int64(AHeapEnd.TotalAllocated);
  Result.HeapDeltaAllocatedBytes := Result.HeapEndAllocatedBytes - Result.HeapStartAllocatedBytes;
  Result.HeapStartFreeBytes := Int64(AHeapStart.TotalFree);
  Result.HeapEndFreeBytes := Int64(AHeapEnd.TotalFree);
  Result.HeapDeltaFreeBytes := Result.HeapEndFreeBytes - Result.HeapStartFreeBytes;
end;

{ a multi-file report's top-level memory: the files' collections and allocations summed. At once (AParallel) the
  engines' heaps coexist, so their live and peak bytes add up, as GocciaRunner combines its workers'; one after
  another each heap is freed before the next, so the peak is the largest and the live bytes are the last file's.
  memory.heap is the main thread's memory manager over the run. }
{ the sum of the K largest peaks among the enabled stats }
function LargestSum(const AStats: array of TCLIJSONMemoryStats; K: Integer): Int64;
var
  Peaks: array of Int64;
  I, J, N: Integer;
  T: Int64;
begin
  SetLength(Peaks, Length(AStats));
  N := 0;
  for I := 0 to High(AStats) do
    if AStats[I].Enabled then
    begin
      Peaks[N] := AStats[I].GCPeakBytes;
      Inc(N);
    end;
  for I := 1 to N - 1 do
  begin
    T := Peaks[I];
    J := I - 1;
    while (J >= 0) and (Peaks[J] < T) do
    begin
      Peaks[J + 1] := Peaks[J];
      Dec(J);
    end;
    Peaks[J + 1] := T;
  end;
  Result := 0;
  for I := 0 to Min(K, N) - 1 do
    Inc(Result, Peaks[I]);
end;

function AggregateMemoryStats(const AStats: array of TCLIJSONMemoryStats; AParallel: Boolean;
  const AHeapStart, AHeapEnd: THeapStatus; AWorkers: Integer = 0): TCLIJSONMemoryStats;
var
  I: Integer;
  First: Boolean;
begin
  Result := DefaultCLIJSONMemoryStats;
  First := True;
  for I := 0 to High(AStats) do
  begin
    if not AStats[I].Enabled then
      Continue;
    if First or AParallel then
    begin
      if First then
        Result := AStats[I]
      else
        Result := CombineCLIJSONMemoryStats(Result, AStats[I], True);
    end
    else
    begin
      Inc(Result.GCAllocatedDuringRunBytes, AStats[I].GCAllocatedDuringRunBytes);
      Inc(Result.GCCollections, AStats[I].GCCollections);
      if AStats[I].GCPeakBytes > Result.GCPeakBytes then
        Result.GCPeakBytes := AStats[I].GCPeakBytes;
      if AStats[I].GCMaxBytes > Result.GCMaxBytes then
        Result.GCMaxBytes := AStats[I].GCMaxBytes;
      Result.GCEndBytes := AStats[I].GCEndBytes;
      Result.GCLiveBytes := AStats[I].GCLiveBytes;
      Result.GCDeltaBytes := Result.GCEndBytes - Result.GCStartBytes;
    end;
    First := False;
  end;
  { at most AWorkers engines live at once: the peak is the largest AWorkers files' peaks together, not every
    file's }
  if AParallel and (AWorkers > 0) and (Length(AStats) > AWorkers) then
    Result.GCPeakBytes := LargestSum(AStats, AWorkers);
  Result.Enabled := True;
  Result.HeapStartAllocatedBytes := Int64(AHeapStart.TotalAllocated);
  Result.HeapEndAllocatedBytes := Int64(AHeapEnd.TotalAllocated);
  Result.HeapDeltaAllocatedBytes := Result.HeapEndAllocatedBytes - Result.HeapStartAllocatedBytes;
  Result.HeapStartFreeBytes := Int64(AHeapStart.TotalFree);
  Result.HeapEndFreeBytes := Int64(AHeapEnd.TotalFree);
  Result.HeapDeltaFreeBytes := Result.HeapEndFreeBytes - Result.HeapStartFreeBytes;
end;

{$I GocciaRunner.Suggestions.inc}

{ Msg is Template with each %s, %d or %x standing for any text }
function TemplateMatches(const ATemplate, AMsg: string): Boolean;
var
  T, M, StarT, StarM: Integer;
begin
  T := 1;
  M := 1;
  StarT := 0;
  StarM := 0;
  while M <= Length(AMsg) do
  begin
    if (T < Length(ATemplate)) and (ATemplate[T] = '%') and (ATemplate[T + 1] in ['s', 'd', 'x']) then
    begin
      Inc(T, 2);
      StarT := T;
      StarM := M;
    end
    else if (T <= Length(ATemplate)) and (ATemplate[T] = AMsg[M]) then
    begin
      Inc(T);
      Inc(M);
    end
    else if StarT > 0 then
    begin
      Inc(StarM);
      M := StarM;
      T := StarT;
    end
    else
      Exit(False);
  end;
  while (T < Length(ATemplate)) and (ATemplate[T] = '%') and (ATemplate[T + 1] in ['s', 'd', 'x']) do
    Inc(T, 2);
  Result := T > Length(ATemplate);
end;

{ GocciaScript's suggestion for a runtime error's message, '' for none }
function RuntimeSuggestion(const AMessage: string): string;
var
  I: Integer;
begin
  for I := 0 to RUNTIME_SUGGESTION_COUNT - 1 do
    if TemplateMatches(RUNTIME_SUGGESTIONS[I, 0], AMessage) then
      Exit(RUNTIME_SUGGESTIONS[I, 1]);
  Result := '';
end;

{ the key a module of APath is loaded under: a sandbox run's paths (AFrameSource assigned) are its own, rooted at /,
  as the sandbox's resolver keeps them; ExpandFileName would put a Windows drive in front of one }
function PathKey(const APath: string; const AFrameSource: TSandboxVirtualFileSystem): string;
begin
  if Assigned(AFrameSource) then
    Result := APath
  else
    Result := ExpandFileName(APath);
end;

{ The failed run's report as text (WriteLn prints it as GocciaRunner does). AEngine: the run's engine, which says
  which files it loaded; AWithheld: a file whose source no report shows (the host environment's provider);
  AFrameSource: the virtual filesystem a sandbox run's modules are read from (nil: the host's files); AFrames: a
  code frame from the source (a sandbox child's error shows only the location); ASuggestions: the host's
  suggestions (meant for the host, not for a sandbox child's caller). }
function FailureText(const AFileName: string; const AResult: TGocciaRunResult; const ASource: TStringList;
  AEngine: TGocciaEngine; const AWithheld: string; const AFrameSource: TSandboxVirtualFileSystem;
  AFrames, ASuggestions, AColor: Boolean; ATimeoutMs, AMaxInstructions: Int64): string;
var
  Name, Suggestion, ErrorPath, FramePath: string;
  Source, Loaded: TStringList;
begin
  case AResult.Aborted of
    gaTimeout:
      Exit(Format('Fatal error: file timed out after %dms', [ATimeoutMs]));
    gaInstructionLimit:
      Exit(Format('Fatal error: Execution exceeded instruction limit of %d', [AMaxInstructions]));
    gaMemoryLimit, gaHostAbort:
      Exit('Fatal error: ' + UTF8Encode(AResult.ErrorText));
  end;
  if not AResult.ErrorIsError then
    Exit(UTF8Encode(AResult.ErrorMessage));
  Name := UTF8Encode(AResult.ErrorName);
  if Name = '' then
    Name := 'Error';
  { a static import that did not load, located at its declaration (FormatHostErrorDiagnostic: no code frame) }
  if AResult.ErrorResolvedPath <> '' then
    Exit(FormatErrorWithSourceContext(Name, UTF8Encode(AResult.ErrorText), UTF8Encode(AResult.ErrorFile),
      AResult.ErrorLine, AResult.ErrorColumn, nil, AColor, '') +
      '  Resolved to: ' + UTF8Encode(AResult.ErrorResolvedPath) + sLineBreak);
  Suggestion := '';
  if ASuggestions then
  begin
    Suggestion := UTF8Encode(AResult.ErrorSuggestion);
    if Suggestion = '' then
      Suggestion := RuntimeSuggestion(UTF8Encode(AResult.ErrorText));
  end;
  if AResult.ErrorLine > 0 then
  begin
    { the code frame from the file the error is in: an imported module's own source, never a file the entry's run
      did not load as a module (a thrown value's stack can name any path; a module the host evaluated first, such
      as a globals module or the host environment's provider, keeps its source) }
    Source := nil;
    if AFrames then
      Source := ASource;
    Loaded := nil;
    ErrorPath := UTF8Encode(AResult.ErrorFile);
    if (ErrorPath <> '') and (ErrorPath <> AFileName) and
       (PathKey(ErrorPath, AFrameSource) <> PathKey(AFileName, AFrameSource)) then
    begin
      Source := nil;
      FramePath := '';
      if AFrames then
        FramePath := UTF8Encode(AEngine.ModuleSourcePath(UnicodeString(PathKey(ErrorPath, AFrameSource))));
      if (FramePath <> '') and ((AWithheld = '') or (PathKey(ErrorPath, AFrameSource) <> AWithheld)) then
      begin
        try
          if Assigned(AFrameSource) then
            Loaded := CreateFileTextLines(AFrameSource.SnapshotReadAllText(FramePath))
          else
          begin
            Loaded := TStringList.Create;
            Loaded.LoadFromFile(FramePath);
          end;
          Source := Loaded;
        except
          on E: Exception do
          begin
            FreeAndNil(Loaded);
            Source := nil;
          end;
        end;
      end;
    end;
    try
      Result := FormatErrorWithSourceContext(Name, UTF8Encode(AResult.ErrorText),
        ErrorPath, AResult.ErrorLine, AResult.ErrorColumn,
        Source, AColor, Suggestion);
    finally
      Loaded.Free;
    end;
  end
  else if AResult.ErrorText <> '' then
    Result := Name + ': ' + UTF8Encode(AResult.ErrorText)
  else
    Result := Name;
end;

end.
