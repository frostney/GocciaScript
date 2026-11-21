unit Goccia.CLI.G2FileEngine;

{ What the CLIs that run files on G2 (GocciaRunner, GocciaTestRunner) share for one file's engine: the console
  capture, the file's settings and host config, the file thread's stack, and the import capability over the file's
  module resolution (the node_modules and provider package grants TGocciaEngine makes on the old engine, their
  audit, and the provider package checks the read policy and the module loader ask for, ADR 0122). }

{$I Goccia.inc}

interface

uses
  Classes,
  SysUtils,

  CLI.ConfigFile,
  CLI.Options,
  G2.Embed,

  Goccia.Capabilities,
  Goccia.CapabilityAudit,
  Goccia.CLI.FileSettings,
  Goccia.CLI.Options,
  Goccia.Modules.Resolver,
  Goccia.Packages.Address;

type
  { The console of a JSON run, as GocciaRunner's TScriptLoaderConsoleCapture: every line in output, and each
    method's line, without the prefix the console gave it, on its own stream }
  TG2ConsoleCapture = class
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
    function OutputText: string;
    function StdoutText: string;
    function StderrText: string;
  end;

  { the import capability over a file's resolution, as TGocciaEngine grants it: node_modules (GrantNodeModules: the
    walk's ceiling when import grants it, a refusal when it is denied, sealed otherwise) and provider packages
    (GrantProviderPackage: an import scope covering the repository, ADR 0122), whose files are checked against
    their pins as they load }
  TG2ImportGrant = class
  private
    FCapabilities: TGocciaCapabilities;
    procedure AuditOrFail(AKind: TGocciaCapabilityKind; AAllowed: Boolean; const ASubject, AReason: string);
  public
    { the file's engine, which audits each decision (nil: none audited) }
    Engine: TG2Engine;
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

{ the host config the preludes read (G2.Embed ConfigJson): experimental-ast installs goccia:ast }
function HostConfigJson(const AEngineOptions: TGocciaEngineOptions; const AFileConfig: TConfigEntryArray): UnicodeString;
{ a file's resolved settings on its engine: the language profile, the source type and the per-file limits }
procedure ApplySettings(const AEngine: TG2Engine; const ASettings: TGocciaFileSettings);

{ a path a config names, against the config's directory (Goccia.CLI.Application's ConfigInputPath) }
function ConfigInputPath(const AWritten, AConfigPath: string): string;
{ a file's globals (GocciaRunner's ApplyDataGlobalsToEngine): the root config's globals files (AConfigEntries, read
  as the script's), then the command line's, then each --global name=value }
function InjectGlobals(const AEngine: TG2Engine; const AConfigEntries: TConfigEntryArray;
  const AGlobalFiles, AInlineGlobals: TRepeatableOption; out AError: string): Boolean;
{ a file's virtual modules (GocciaRunner's ApplyConfiguredVirtualModules): the root config's, the file's own
  config's when it has another, then the command line's }
function DefineVirtualModules(const AEngine: TG2Engine; const ARootConfigPath, AFileConfigPath: string;
  const AEngineOptions: TGocciaEngineOptions; out AError: string): Boolean;
{ the command line's --modules manifests and --module definitions, the user's own }
function DefineCommandLineVirtualModules(const AEngine: TG2Engine; const AEngineOptions: TGocciaEngineOptions;
  out AError: string): Boolean;

implementation

uses
  G2.Core,
  G2.HostAPI,

  Goccia.Error.Suggestions,
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

procedure ApplySettings(const AEngine: TG2Engine;
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
    fstScript: AEngine.SourceKind := g2skScript;
    fstModule: AEngine.SourceKind := g2skModule;
  else
    AEngine.SourceKind := g2skDetect;
  end;
  AEngine.FetchMaxResponseBytes := ASettings.FetchMaxResponseBytes;
  AEngine.JSModuleSources := ASettings.ExperimentalJSModuleSource;
  if ASettings.MaxMemorySet then
    AEngine.MaxMemory := ASettings.MaxMemory
  else
    AEngine.MaxMemory := 0;
end;

constructor TG2ConsoleCapture.Create;
begin
  inherited Create;
  FOutputLines := TStringList.Create;
  FStdoutLines := TStringList.Create;
  FStderrLines := TStringList.Create;
end;

destructor TG2ConsoleCapture.Destroy;
begin
  FStderrLines.Free;
  FStdoutLines.Free;
  FOutputLines.Free;
  inherited Destroy;
end;

function MethodWritesToStderr(const AMethod: string): Boolean;
begin
  Result := (AMethod = 'warn') or (AMethod = 'error') or
    (AMethod = 'assert') or (AMethod = 'trace');
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

procedure TG2ConsoleCapture.CaptureOutput(const AMethod, ALine: UnicodeString);
var
  Method, Line: string;
begin
  Method := UTF8Encode(AMethod);
  Line := UTF8Encode(ALine);
  FOutputLines.Add(Line);
  if MethodWritesToStderr(Method) then
    FStderrLines.Add(UnformatLine(Method, Line))
  else
    FStdoutLines.Add(UnformatLine(Method, Line));
end;

{ --silent }
procedure TG2ConsoleCapture.Discard(const AMethod, ALine: UnicodeString);
begin
end;

procedure TG2ConsoleCapture.Emit(const AMethod, ALine: UnicodeString);
begin
  if Assigned(LogLine) then
    LogLine(UTF8Encode(AMethod), UTF8Encode(ALine));
  if Capturing then
    CaptureOutput(AMethod, ALine)
  else if not Silent then
    WriteLn(UTF8Encode(ALine));
end;

procedure TG2ConsoleCapture.PrintWarning(const AMessage, ASuggestion: UnicodeString;
  ALine, AColumn: Integer);
begin
  WriteLn('Warning: ', UTF8Encode(AMessage));
  if ASuggestion <> '' then
    WriteLn('  Suggestion: ', UTF8Encode(ASuggestion));
  WriteLn(Format('  --> %s:%d:%d', [FileName, ALine, AColumn]));
end;

function TG2ConsoleCapture.OutputText: string;
begin
  Result := FOutputLines.Text;
end;

function TG2ConsoleCapture.StdoutText: string;
begin
  Result := FStdoutLines.Text;
end;

function TG2ConsoleCapture.StderrText: string;
begin
  Result := FStderrLines.Text;
end;

constructor TG2ImportGrant.Create(const ACapabilities: TGocciaCapabilities);
begin
  inherited Create;
  FCapabilities := ACapabilities;
end;

{ every decision is audited, as GocciaRunner's grants do; a log that cannot be written ends the run }
procedure TG2ImportGrant.AuditOrFail(AKind: TGocciaCapabilityKind; AAllowed: Boolean;
  const ASubject, AReason: string);
begin
  if Assigned(Engine) and not Engine.Audit(AKind, AAllowed, ASubject, AReason) then
    raise EGocciaCapabilityAuditDeliveryError.Create('capability audit delivery failed');
end;

function TG2ImportGrant.Grant(const ASpecifier, AImportingDirectory: string;
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
    raise EG2PermissionDenied.CreateDenied(CapabilityName(gcImport), ASpecifier,
      'node_modules resolution is denied for ' + AImportingDirectory);
end;

{ a provider import needs an import scope covering its repository: github, github:<owner> or
  github:<owner>/<repo>; the PermissionDenied names the package, the suggestion the grants that would cover it or
  the deny that refused it }
procedure TG2ImportGrant.GrantProvider(const ASpecifier: string; const AAddress: TGocciaProviderAddress);
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
  raise EG2PermissionDenied.CreateDenied(CapabilityName(gcImport), AAddress.PackageKey, Suggestion);
end;

{ the package store's decisions: a pin it verified or refused }
procedure TG2ImportGrant.AuditProvider(const AAllowed: Boolean; const ASubject, AReason: string);
begin
  AuditOrFail(gckImportProvider, AAllowed, ASubject, AReason);
end;

{ GocciaScript's IsProviderGraphPath: a file of a materialized package, reached through the package's import-map
  entry or imported by one of its own files }
function TG2ImportGrant.ProviderGraph(const APath, AImportingFile, APackageRoot: string): Boolean;
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
procedure TG2ImportGrant.CheckContent(const APath: UnicodeString; const B: RawByteString);
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
    Result := UTF8Decode(Lines.Text);
  finally
    Lines.Free;
  end;
end;

{ GocciaRunner's globals (ApplyDataGlobalsToEngine): the root config's globals files, then the command line's, then
  each --global name=value. Globals modules (a .js file's exports) are not on G2 yet. }
function InjectGlobals(const AEngine: TG2Engine; const AConfigEntries: TConfigEntryArray;
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
  manifests are not on G2 yet. }
function DefineVirtualModules(const AEngine: TG2Engine; const ARootConfigPath, AFileConfigPath: string;
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
function DefineCommandLineVirtualModules(const AEngine: TG2Engine; const AEngineOptions: TGocciaEngineOptions;
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

end.
