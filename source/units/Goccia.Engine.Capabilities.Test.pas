program Goccia.Engine.Capabilities.Test;

{ Engine and runtime behavior driven by the capability set (ADR 0122): which
  host reads are exempt, granted, or denied; how PermissionDenied reaches the
  guest; FFI gating; node_modules; and inheritance by child realms. }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  BaseUnix,
  Sockets,
  {$ENDIF}
  Classes,
  SysUtils,

  FileUtils,
  HTTPTypes,
  TestingPascalLibrary,

  Goccia.Arguments.Collection,
  Goccia.Builtins.GlobalShadowRealm,
  Goccia.Capabilities,
  Goccia.CapabilityAudit,
  Goccia.Engine,
  Goccia.Error,
  Goccia.Executor,
  Goccia.GarbageCollector,
  Goccia.Executor.Bytecode,
  Goccia.Executor.Interpreter,
  Goccia.FetchManager,
  Goccia.Modules,
  Goccia.Modules.ContentProvider,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.Fetch,
  Goccia.RuntimeExtensions.FFI,
  Goccia.TestSetup,
  Goccia.Values.Error,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.PromiseValue,
  Goccia.Values.ResponseValue,
  Goccia.VM.Exception;

type
  TRunOutcome = record
    Result: string;
    ErrorName: string;
    ErrorMessage: string;
    ErrorCapability: string;
    ErrorScope: string;
    Suggestion: string;
    Location: string;
    SuggestionIsHostOnly: Boolean;
  end;

  TEngineCapabilitiesTests = class(TTestSuite)
  private
    FRoot: string;
    FProject: string;
    FOutside: string;
    FEvents: TStringList;
    FEventSources: TStringList;
    FEventReasons: TStringList;
    FAuditedHopIndex: Integer;
    FVirtualModuleName: string;
    FAliasPattern: string;
    FAliasTarget: string;
    FInstallFetchAndFFI: Boolean;
    FVirtualModuleSource: string;
    function PumpUntilAudited(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure RecordEvent(const AEvent: TGocciaCapabilityAuditEvent);
    function ProjectPath(const AName: string): string;
    function OutsidePath(const AName: string): string;
    function Run(const ASource: string;
      const ACapabilities: TGocciaCapabilities;
      const ABytecode: Boolean = False;
      const AShadowRealm: Boolean = False;
      const AEntry: string = ''): TRunOutcome;
    function EventsOfKind(const AKind: string): Integer;
  protected
    procedure BeforeAll; override;
    procedure AfterAll; override;
    procedure BeforeEach; override;
  private
    procedure TestProviderFollowsReadCapability;
    procedure TestStaticImportInsideProjectIsExempt;
    procedure TestStaticImportOutsideProjectIsDenied;
    procedure TestDenialSuggestionsNameTheGrant;
    function ComputedImportSuggestion(const ASpecifier: string): string;
    procedure TestHostLoadedModuleOutsideProjectIsExempt;
    procedure TestReadGrantCoversOutsidePath;
    procedure TestMissingOutsideFileIsDeniedBeforeProbing;
    procedure TestExtensionProbingCannotRevealDeniedFiles;
    procedure TestComputedDynamicImportNeedsRead;
    procedure TestLiteralDynamicImportIsExempt;
    procedure TestComputedImportInBytecodeNeedsRead;
    procedure TestDenyScopeRemovesExemption;
    procedure TestUnscopedDenyRemovesExemption;
    procedure TestBytesImportOutsideProjectIsDenied;
    procedure TestFFIRefusesWithoutGrant;
    procedure TestFFIOpenChecksLibraryScopes;
    procedure TestFFIBareNamesNeedAnUnscopedGrant;
    procedure TestImportMetaResolveDoesNotProbeOutsideTheGrant;
    procedure TestAbortedFetchStillAuditsItsHops;
    procedure TestAbortedFetchAuditEndsWithItsEngine;
    procedure TestDenyScopeHidesExistenceOfProbedFiles;
    procedure TestHostLoadedModuleIsCheckedForTheGuest;
    function OpenLibrary(const ALibrary: string;
      const ACapabilities: TGocciaCapabilities): TRunOutcome;
    procedure TestNodeModulesDenyThrowsPermissionDenied;
    procedure TestShadowRealmInheritsCapabilities;
    procedure TestFetchPolicyTravelsWithEachEngine;
    procedure TestSymlinkOutOfProjectNeedsRead;
    procedure TestGrantedNodeModulesArePartOfTheGraph;
    procedure TestNodeModulesInAPathIsNoGrant;
    procedure TestLinkedPackageIsPartOfTheGraph;
    procedure TestDenyScopesJudgeTheResolvedPath;
    procedure TestDenyScopeHidesExistenceInsideProject;
    procedure TestImportMetaResolveHonoursDenyScopes;
    procedure TestVirtualBareModuleAuditsNoNodeModules;
    procedure TestAbortAtScriptEndRecordsAbandonment;
    procedure TestAliasCandidatesAreJudged;
    procedure TestPackageProbesAreJudged;
    procedure TestCallDenialSitesMatchAcrossExecutors;
    procedure TestStaticImportDenialSitesMatchAcrossExecutors;
  public
    procedure SetupTests; override;
  end;

procedure TEngineCapabilitiesTests.SetupTests;
begin
  Test('The runtime installs the filesystem provider whatever the read ' +
    'capability says', TestProviderFollowsReadCapability);
  Test('A static literal import inside the project needs no grant',
    TestStaticImportInsideProjectIsExempt);
  Test('A static import outside the project is denied without a host path',
    TestStaticImportOutsideProjectIsDenied);
  Test('Read and ffi denials suggest the flag that grants them',
    TestDenialSuggestionsNameTheGrant);
  Test('A module the host loads itself is exempt outside the project',
    TestHostLoadedModuleOutsideProjectIsExempt);
  Test('A read grant covers a path outside the project',
    TestReadGrantCoversOutsidePath);
  Test('A missing file outside the project is denied before probing',
    TestMissingOutsideFileIsDeniedBeforeProbing);
  Test('Extension probing cannot reveal whether a denied file exists',
    TestExtensionProbingCannotRevealDeniedFiles);
  Test('A computed dynamic import needs a read grant',
    TestComputedDynamicImportNeedsRead);
  Test('A literal dynamic import inside the project needs no grant',
    TestLiteralDynamicImportIsExempt);
  Test('A computed dynamic import in bytecode needs a read grant',
    TestComputedImportInBytecodeNeedsRead);
  Test('A scoped read deny removes the module-graph exemption',
    TestDenyScopeRemovesExemption);
  Test('An unscoped read deny removes the module-graph exemption',
    TestUnscopedDenyRemovesExemption);
  Test('A bytes import outside the project is denied',
    TestBytesImportOutsideProjectIsDenied);
  Test('The FFI extension refuses to attach without the ffi grant',
    TestFFIRefusesWithoutGrant);
  Test('FFI.open checks library path scopes',
    TestFFIOpenChecksLibraryScopes);
  Test('A bare library name needs an unscoped ffi grant and no deny scope',
    TestFFIBareNamesNeedAnUnscopedGrant);
  Test('import.meta.resolve does not probe the host outside the read grant',
    TestImportMetaResolveDoesNotProbeOutsideTheGrant);
  Test('An aborted fetch still audits its hops, attributed to its fetch() call',
    TestAbortedFetchStillAuditsItsHops);
  Test('An aborted fetch''s audit is dropped when its engine discards its ' +
    'requests', TestAbortedFetchAuditEndsWithItsEngine);
  Test('A deny scope the resolver could probe into refuses before probing',
    TestDenyScopeHidesExistenceOfProbedFiles);
  Test('A module the host loaded is still read-checked for the guest',
    TestHostLoadedModuleIsCheckedForTheGuest);
  Test('A node_modules deny throws PermissionDenied',
    TestNodeModulesDenyThrowsPermissionDenied);
  Test('A ShadowRealm child inherits its creator''s capability set',
    TestShadowRealmInheritsCapabilities);
  Test('Two in-flight requests on one thread keep their own engine''s policy',
    TestFetchPolicyTravelsWithEachEngine);
  Test('A symlink inside the project to a file outside it needs read',
    TestSymlinkOutOfProjectNeedsRead);
  Test('Packages reached through a granted node_modules scope need no read',
    TestGrantedNodeModulesArePartOfTheGraph);
  Test('A path through a node_modules directory is no import grant',
    TestNodeModulesInAPathIsNoGrant);
  Test('A package symlinked out of the project is part of the graph',
    TestLinkedPackageIsPartOfTheGraph);
  Test('Deny scopes judge the path a specifier resolves to',
    TestDenyScopesJudgeTheResolvedPath);
  Test('A deny scope inside the project hides whether its file exists',
    TestDenyScopeHidesExistenceInsideProject);
  Test('import.meta.resolve does not probe into a deny scope',
    TestImportMetaResolveHonoursDenyScopes);
  Test('A bare specifier served by a virtual module audits no node_modules ' +
    'decision', TestVirtualBareModuleAuditsNoNodeModules);
  Test('A fetch aborted as the script ends records that its later hops go ' +
    'unaudited', TestAbortAtScriptEndRecordsAbandonment);
  Test('Every path an alias or import map rewrites to is judged before ' +
    'probing', TestAliasCandidatesAreJudged);
  Test('File probes inside a granted package are judged too',
    TestPackageProbesAreJudged);
  Test('fetch() and FFI.open() denials are located alike in both executors',
    TestCallDenialSitesMatchAcrossExecutors);
  Test('Static import and export-from denials are located at the ' +
    'declaration in both executors',
    TestStaticImportDenialSitesMatchAcrossExecutors);
end;

procedure WriteFile(const APath, AText: string);
begin
  ForceDirectories(ExtractFileDir(APath));
  FileUtils.WriteUTF8FileText(APath, AText);
end;

procedure DeleteTree(const APath: string);
var
  SearchRec: TSearchRec;
  EntryPath: string;
begin
  if not DirectoryExists(APath) then
    Exit;
  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*', faAnyFile,
     SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
        Continue;
      EntryPath := IncludeTrailingPathDelimiter(APath) + SearchRec.Name;
      if (SearchRec.Attr and faDirectory) = faDirectory then
        DeleteTree(EntryPath)
      else
        DeleteFile(EntryPath);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
  RemoveDir(APath);
end;

procedure TEngineCapabilitiesTests.BeforeAll;
begin
  inherited BeforeAll;
  FRoot := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'goccia-engine-capabilities-' + IntToStr(GetProcessID);
  FProject := IncludeTrailingPathDelimiter(FRoot) + 'project';
  FOutside := IncludeTrailingPathDelimiter(FRoot) + 'outside';
  WriteFile(ProjectPath('goccia.json'), '{}');
  WriteFile(ProjectPath('lib.js'), 'export const value = "inside";');
  WriteFile(OutsidePath('secret.js'), 'export const value = "outside";');
  WriteFile(OutsidePath('data.bin'), 'bytes');
  WriteFile(ProjectPath('node_modules/pkg/package.json'),
    '{"name":"pkg","type":"module","exports":"./index.js"}');
  WriteFile(ProjectPath('node_modules/pkg/index.js'),
    'import { detail } from "./detail.js"; export const value = detail;');
  WriteFile(ProjectPath('node_modules/pkg/detail.js'),
    'export const detail = "package";');
  WriteFile(ProjectPath('nested/goccia.json'), '{}');
  WriteFile(ProjectPath('sub/index.js'), 'export const value = "sub";');
  WriteFile(ProjectPath('sub/secret.json'), '{}');
  WriteFile(ProjectPath('hidden.js'), 'export const value = "hidden";');
  WriteFile(ProjectPath('shadow/index.js'), 'export const value = "shadow";');
  WriteFile(ProjectPath('node_modules/probe/package.json'),
    '{"name":"probe","type":"module","exports":"./main"}');
  WriteFile(ProjectPath('node_modules/probe/main/index.js'),
    'export const value = "directory";');
  WriteFile(IncludeTrailingPathDelimiter(FRoot) +
    'other/node_modules/pkg/secret.json', '{"secret":"other"}');
  WriteFile(IncludeTrailingPathDelimiter(FRoot) + 'linkedpkg/package.json',
    '{"name":"linked","type":"module","exports":"./index.js"}');
  WriteFile(IncludeTrailingPathDelimiter(FRoot) + 'linkedpkg/index.js',
    'import { detail } from "./detail.js"; export const value = detail;');
  WriteFile(IncludeTrailingPathDelimiter(FRoot) + 'linkedpkg/detail.js',
    'export const detail = "linked";');
  FEvents := TStringList.Create;
  FEventSources := TStringList.Create;
  FEventReasons := TStringList.Create;
end;

procedure TEngineCapabilitiesTests.AfterAll;
begin
  FEvents.Free;
  FEventSources.Free;
  FEventReasons.Free;
  DeleteTree(FRoot);
  inherited AfterAll;
end;

procedure TEngineCapabilitiesTests.BeforeEach;
begin
  inherited BeforeEach;
  FEvents.Clear;
  FEventSources.Clear;
  FEventReasons.Clear;
  FVirtualModuleName := '';
  FVirtualModuleSource := '';
  FAliasPattern := '';
  FAliasTarget := '';
  FInstallFetchAndFFI := False;
end;

procedure TEngineCapabilitiesTests.RecordEvent(
  const AEvent: TGocciaCapabilityAuditEvent);
begin
  FEvents.Add(CapabilityKindName(AEvent.Kind) + '|' +
    CapabilityDecisionName(AEvent.Decision) + '|' + AEvent.Subject);
  FEventReasons.Add(AEvent.Reason);
  FEventSources.Add(ExtractFileName(AEvent.Source.FilePath) + ':' +
    IntToStr(AEvent.Source.Line));
end;

function TEngineCapabilitiesTests.ProjectPath(const AName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FProject) +
    StringReplace(AName, '/', PathDelim, [rfReplaceAll]);
end;

function TEngineCapabilitiesTests.OutsidePath(const AName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FOutside) +
    StringReplace(AName, '/', PathDelim, [rfReplaceAll]);
end;

function TEngineCapabilitiesTests.EventsOfKind(const AKind: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FEvents.Count - 1 do
    if Copy(FEvents[I], 1, Length(AKind) + 1) = AKind + '|' then
      Inc(Result);
end;

procedure CaptureThrown(const AValue: TGocciaValue; var AOutcome: TRunOutcome);
var
  ErrorObject: TGocciaObjectValue;
begin
  if not (AValue is TGocciaObjectValue) then
  begin
    AOutcome.ErrorMessage := AValue.ToStringLiteral.Value;
    Exit;
  end;
  ErrorObject := TGocciaObjectValue(AValue);
  AOutcome.ErrorName := ErrorObject.GetProperty('name').ToStringLiteral.Value;
  AOutcome.ErrorMessage :=
    ErrorObject.GetProperty('message').ToStringLiteral.Value;
  AOutcome.ErrorCapability :=
    ErrorObject.GetProperty('capability').ToStringLiteral.Value;
  AOutcome.ErrorScope := ErrorObject.GetProperty('scope').ToStringLiteral.Value;
  if (ErrorObject is TGocciaErrorObjectValue) and
     TGocciaErrorObjectValue(ErrorObject).HasErrorSourceLocation then
    AOutcome.Location := Format('%s:%d:%d', [
      ExtractFileName(TGocciaErrorObjectValue(ErrorObject).ErrorSourcePath),
      TGocciaErrorObjectValue(ErrorObject).ErrorSourceLine,
      TGocciaErrorObjectValue(ErrorObject).ErrorSourceColumn]);
end;

function TEngineCapabilitiesTests.Run(const ASource: string;
  const ACapabilities: TGocciaCapabilities; const ABytecode: Boolean;
  const AShadowRealm: Boolean; const AEntry: string): TRunOutcome;
var
  Source: TStringList;
  Executor: TGocciaExecutor;
  Runtime: TGocciaRuntimeCore;
  Engine: TGocciaEngine;
  ResultValue: TGocciaValue;
begin
  Result := Default(TRunOutcome);
  Source := TStringList.Create;
  Source.Text := ASource;
  if ABytecode then
    Executor := TGocciaBytecodeExecutor.Create
  else
    Executor := TGocciaInterpreterExecutor.Create;
  if AEntry <> '' then
    Engine := TGocciaEngine.Create(AEntry, Source, Executor, ACapabilities)
  else
    Engine := TGocciaEngine.Create(ProjectPath('app.mjs'), Source, Executor,
      ACapabilities);
  try
    Engine.CapabilityAuditSink := RecordEvent;
    Runtime := AttachRuntime(Engine);
    if FInstallFetchAndFFI then
    begin
      Runtime.Install(TGocciaFetchRuntimeExtension.Create);
      InstallFFIIfGranted(Runtime);
    end;
    if FVirtualModuleName <> '' then
      Engine.InjectModule(FVirtualModuleName, FVirtualModuleSource);
    if FAliasPattern <> '' then
      Engine.ModuleLoader.Resolver.AddAlias(FAliasPattern, FAliasTarget);
    if AShadowRealm then
      EnableShadowRealm(Engine);
    try
      Engine.Execute;
      Engine.WaitForRuntimeIdle;
    except
      on E: TGocciaThrowValue do
      begin
        CaptureThrown(E.Value, Result);
        Result.Suggestion := E.Suggestion;
        Result.SuggestionIsHostOnly := E.SuggestionIsHostOnly;
      end;
      on E: EGocciaBytecodeThrow do
      begin
        CaptureThrown(E.ThrownValue, Result);
        Result.Suggestion := E.Suggestion;
        Result.SuggestionIsHostOnly := E.SuggestionIsHostOnly;
      end;
      on E: Exception do
        Result.ErrorMessage := E.Message;
    end;
    ResultValue := TGocciaObjectValue(Engine.Realm.GlobalObject)
      .GetProperty('result');
    if Assigned(ResultValue) then
      Result.Result := ResultValue.ToStringLiteral.Value;
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

procedure TEngineCapabilitiesTests.TestProviderFollowsReadCapability;
var
  Source: TStringList;
  Executor: TGocciaInterpreterExecutor;
  Engine: TGocciaEngine;
begin
  Source := TStringList.Create;
  Executor := TGocciaInterpreterExecutor.Create;
  try
    Engine := TGocciaEngine.Create(ProjectPath('app.js'), Source, Executor);
    try
      AttachRuntime(Engine);
      Expect<Boolean>(Engine.ContentProvider.ReadsHostFileSystem).ToBe(True);
    finally
      Engine.Free;
    end;
    { Under an outright deny the provider stays, so the loader can refuse
      each read with an audited PermissionDenied. }
    Engine := TGocciaEngine.Create(ProjectPath('app.js'), Source, Executor,
      TGocciaCapabilities.None.Deny(gcRead));
    try
      AttachRuntime(Engine);
      Expect<Boolean>(Engine.ContentProvider.ReadsHostFileSystem).ToBe(True);
    finally
      Engine.Free;
    end;
  finally
    Executor.Free;
    Source.Free;
  end;
end;

procedure TEngineCapabilitiesTests.TestStaticImportInsideProjectIsExempt;
var
  Outcome: TRunOutcome;
begin
  Outcome := Run('import { value } from "./lib.js"; globalThis.result = value;',
    TGocciaCapabilities.None);
  Expect<string>(Outcome.ErrorMessage).ToBe('');
  Expect<string>(Outcome.Result).ToBe('inside');
  Expect<Integer>(EventsOfKind('read.file')).ToBe(0);
end;

procedure TEngineCapabilitiesTests.TestStaticImportOutsideProjectIsDenied;
var
  Outcome: TRunOutcome;
begin
  Outcome := Run(
    'import { value } from "../outside/secret.js"; globalThis.result = value;',
    TGocciaCapabilities.None);
  Expect<string>(Outcome.ErrorName).ToBe('PermissionDenied');
  Expect<string>(Outcome.ErrorMessage).ToBe('read: ../outside/secret.js');
  Expect<string>(Outcome.ErrorCapability).ToBe('read');
  Expect<string>(Outcome.ErrorScope).ToBe('../outside/secret.js');
  { The guest never sees the host path; the host-side suggestion does. }
  Expect<Boolean>(Pos(FRoot, Outcome.ErrorMessage) > 0).ToBe(False);
  Expect<Boolean>(Pos('secret.js', Outcome.Suggestion) > 0).ToBe(True);
  Expect<string>(Outcome.Result).ToBe('undefined');
  Expect<Integer>(EventsOfKind('read.file')).ToBe(1);
  Expect<Boolean>(Pos('read.file|deny|', FEvents[FEvents.Count - 1]) = 1)
    .ToBe(True);
end;

function TEngineCapabilitiesTests.ComputedImportSuggestion(
  const ASpecifier: string): string;
var
  Source: TStringList;
  Executor: TGocciaInterpreterExecutor;
  Engine: TGocciaEngine;
  Caught: TGocciaValue;
begin
  Result := '';
  Source := TStringList.Create;
  Source.Text := 'const name = "' + ASpecifier + '";' + sLineBreak +
    'import(name).catch((e) => { globalThis.caught = e; });';
  Executor := TGocciaInterpreterExecutor.Create;
  Engine := TGocciaEngine.Create(ProjectPath('app.js'), Source, Executor,
    TGocciaCapabilities.None);
  try
    AttachRuntime(Engine);
    Engine.Execute;
    Engine.WaitForRuntimeIdle;
    Caught := TGocciaObjectValue(Engine.Realm.GlobalObject)
      .GetProperty('caught');
    if Caught is TGocciaErrorObjectValue then
      Result := TGocciaErrorObjectValue(Caught).ErrorHostSuggestion;
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

procedure TEngineCapabilitiesTests.TestDenialSuggestionsNameTheGrant;
var
  Outcome: TRunOutcome;
  Suggestion: string;
begin
  Outcome := Run(
    'import { value } from "../outside/secret.js"; globalThis.result = value;',
    TGocciaCapabilities.None);
  Expect<Boolean>(Pos('--allow-read=' + CanonicalCapabilityPath(FOutside),
    Outcome.Suggestion) > 0).ToBe(True);
  Expect<Boolean>(Pos('"allow-read"', Outcome.Suggestion) > 0).ToBe(True);

  Expect<Boolean>(Pos('computed', Outcome.Suggestion) > 0).ToBe(False);

  Outcome := Run(
    'import { value } from "../outside/secret.js"; globalThis.result = value;',
    TGocciaCapabilities.None.Allow(gcRead).Deny(gcRead, FOutside));
  Expect<Boolean>(Pos('a read deny (--deny-read', Outcome.Suggestion) = 1)
    .ToBe(True);

  { A computed import() names why the module graph does not cover it. The
    rejection reaches the guest, so the suggestion is read from the error
    object it carries. }
  Suggestion := ComputedImportSuggestion('../outside/secret.js');
  Expect<Boolean>(Pos('a computed import() specifier is not part of the ' +
    'module graph', Suggestion) = 1).ToBe(True);
  Expect<Boolean>(Pos('--allow-read=' + CanonicalCapabilityPath(FOutside),
    Suggestion) > 0).ToBe(True);

  Outcome := OpenLibrary('../outside/lib.so',
    TGocciaCapabilities.None.Allow(gcFFI, FProject));
  Expect<Boolean>(Pos('--allow-ffi=', Outcome.Suggestion) > 0).ToBe(True);
  Outcome := OpenLibrary('libgoccia-capability-probe.so',
    TGocciaCapabilities.None.Allow(gcFFI, FProject));
  Expect<Boolean>(Pos('--allow-ffi', Outcome.Suggestion) > 0).ToBe(True);
end;

procedure TEngineCapabilitiesTests.TestHostLoadedModuleOutsideProjectIsExempt;
var
  Source: TStringList;
  Executor: TGocciaInterpreterExecutor;
  Engine: TGocciaEngine;
  Module: TGocciaModule;
begin
  Source := TStringList.Create;
  Executor := TGocciaInterpreterExecutor.Create;
  try
    Engine := TGocciaEngine.Create(ProjectPath('app.mjs'), Source, Executor,
      TGocciaCapabilities.None);
    try
      Engine.CapabilityAuditSink := RecordEvent;
      AttachRuntime(Engine);
      { --globals, --modules, and host environment providers load this way. }
      Module := Engine.ModuleLoader.LoadHostModule(OutsidePath('secret.js'),
        ProjectPath('app.mjs'));
      Expect<Boolean>(Assigned(Module)).ToBe(True);
      Expect<Boolean>(Module.IsHostOwned).ToBe(True);
      Expect<Integer>(EventsOfKind('read.file')).ToBe(0);
    finally
      Engine.Free;
    end;
  finally
    Executor.Free;
    Source.Free;
  end;
end;

procedure TEngineCapabilitiesTests.TestReadGrantCoversOutsidePath;
var
  Outcome: TRunOutcome;
begin
  Outcome := Run(
    'import { value } from "../outside/secret.js"; globalThis.result = value;',
    TGocciaCapabilities.None.Allow(gcRead, FOutside));
  Expect<string>(Outcome.ErrorMessage).ToBe('');
  Expect<string>(Outcome.Result).ToBe('outside');
  Expect<Boolean>(FEvents.IndexOf('read.file|allow|' +
    CanonicalCapabilityPath(OutsidePath('secret.js'))) >= 0).ToBe(True);
end;

procedure TEngineCapabilitiesTests.TestMissingOutsideFileIsDeniedBeforeProbing;
var
  Outcome: TRunOutcome;
begin
  Outcome := Run('import "../outside/missing.js";', TGocciaCapabilities.None);
  Expect<string>(Outcome.ErrorName).ToBe('PermissionDenied');
  Expect<string>(Outcome.ErrorMessage).ToBe('read: ../outside/missing.js');
end;

procedure TEngineCapabilitiesTests.TestExtensionProbingCannotRevealDeniedFiles;
var
  Existing, Missing: TRunOutcome;
  Capabilities: TGocciaCapabilities;
begin
  { lib.js exists and absent.js does not; both are denied. An extensionless
    import of either must fail the same way, before the resolver probes. }
  Capabilities := TGocciaCapabilities.None
    .Deny(gcRead, ProjectPath('lib.js'))
    .Deny(gcRead, ProjectPath('absent.js'));
  Existing := Run('import "./lib";', Capabilities);
  Missing := Run('import "./absent";', Capabilities);
  Expect<string>(Existing.ErrorName).ToBe('PermissionDenied');
  Expect<string>(Missing.ErrorName).ToBe('PermissionDenied');
  Expect<string>(Missing.ErrorMessage).ToBe('read: ./absent');
end;

procedure TEngineCapabilitiesTests.TestComputedDynamicImportNeedsRead;
const
  SOURCE_TEXT =
    'const name = "./lib" + ".js";' + sLineBreak +
    'globalThis.result = "pending";' + sLineBreak +
    'import(name).then((m) => { globalThis.result = m.value; },' +
    ' (e) => { globalThis.result = (e instanceof PermissionDenied) + "|" +' +
    ' e.message; });';
var
  Outcome: TRunOutcome;
begin
  Outcome := Run(SOURCE_TEXT, TGocciaCapabilities.None);
  Expect<string>(Outcome.Result).ToBe('true|read: ./lib.js');
  Outcome := Run(SOURCE_TEXT,
    TGocciaCapabilities.None.Allow(gcRead, FProject));
  Expect<string>(Outcome.Result).ToBe('inside');
end;

procedure TEngineCapabilitiesTests.TestLiteralDynamicImportIsExempt;
var
  Outcome: TRunOutcome;
begin
  Outcome := Run('globalThis.result = "pending";' + sLineBreak +
    'import("./lib.js").then((m) => { globalThis.result = m.value; },' +
    ' (e) => { globalThis.result = e.message; });',
    TGocciaCapabilities.None);
  Expect<string>(Outcome.Result).ToBe('inside');
end;

procedure TEngineCapabilitiesTests.TestComputedImportInBytecodeNeedsRead;
const
  SOURCE_TEXT =
    'const name = "./lib" + ".js";' + sLineBreak +
    'globalThis.result = "pending";' + sLineBreak +
    'import(name).then((m) => { globalThis.result = m.value; },' +
    ' (e) => { globalThis.result = e.name + "|" + e.message; });' + sLineBreak +
    'import("./lib.js").then((m) => { globalThis.literal = m.value; });';
var
  Outcome: TRunOutcome;
begin
  Outcome := Run(SOURCE_TEXT, TGocciaCapabilities.None, True);
  Expect<string>(Outcome.Result).ToBe('PermissionDenied|read: ./lib.js');
  Outcome := Run(SOURCE_TEXT, TGocciaCapabilities.None.Allow(gcRead,
    FProject), True);
  Expect<string>(Outcome.Result).ToBe('inside');
end;

procedure TEngineCapabilitiesTests.TestDenyScopeRemovesExemption;
var
  Outcome: TRunOutcome;
begin
  Outcome := Run('import { value } from "./lib.js"; globalThis.result = value;',
    TGocciaCapabilities.None.Allow(gcRead).Deny(gcRead, ProjectPath('lib.js')));
  Expect<string>(Outcome.ErrorName).ToBe('PermissionDenied');
  Expect<string>(Outcome.ErrorMessage).ToBe('read: ./lib.js');
end;

{ An outright read deny — the replacement for --no-host-filesystem — refuses
  every host read with a catchable, audited PermissionDenied: static,
  dynamic, and bytes imports alike. }
procedure TEngineCapabilitiesTests.TestUnscopedDenyRemovesExemption;
var
  Outcome: TRunOutcome;
begin
  Outcome := Run('import { value } from "./lib.js"; globalThis.result = value;',
    TGocciaCapabilities.None.Deny(gcRead));
  Expect<string>(Outcome.ErrorName).ToBe('PermissionDenied');
  Expect<string>(Outcome.ErrorMessage).ToBe('read: ./lib.js');
  Expect<string>(Outcome.Result).ToBe('undefined');
  Expect<Boolean>(FEvents.IndexOf('read.file|deny|' +
    CanonicalCapabilityPath(ProjectPath('lib.js'))) >= 0).ToBe(True);

  Outcome := Run('globalThis.result = "pending";' + sLineBreak +
    'Promise.all([import("./lib.js"), import("./li" + "b.js"),' +
    ' import("./lib.js", { with: { type: "bytes" } })].map((p) =>' +
    ' p.then(() => "loaded", (e) => e.name + ":" + e.message)))' +
    '.then((r) => { globalThis.result = r.join("|"); });',
    TGocciaCapabilities.None.Deny(gcRead));
  Expect<string>(Outcome.Result).ToBe(
    'PermissionDenied:read: ./lib.js|PermissionDenied:read: ./lib.js|' +
    'PermissionDenied:read: ./lib.js');
end;

procedure TEngineCapabilitiesTests.TestBytesImportOutsideProjectIsDenied;
var
  Outcome: TRunOutcome;
begin
  Outcome := Run('import data from "../outside/data.bin" with ' +
    '{ type: "bytes" }; globalThis.result = data.length;',
    TGocciaCapabilities.None);
  Expect<string>(Outcome.ErrorName).ToBe('PermissionDenied');
  Expect<string>(Outcome.ErrorMessage).ToBe('read: ../outside/data.bin');
  Outcome := Run('import data from "../outside/data.bin" with ' +
    '{ type: "bytes" }; globalThis.result = data.length;',
    TGocciaCapabilities.None.Allow(gcRead, FOutside));
  Expect<string>(Outcome.Result).ToBe('5');
end;

procedure TEngineCapabilitiesTests.TestFFIRefusesWithoutGrant;
var
  Source: TStringList;
  Executor: TGocciaInterpreterExecutor;
  Engine: TGocciaEngine;
  Runtime: TGocciaRuntimeCore;
  Raised: Boolean;
begin
  Source := TStringList.Create;
  Executor := TGocciaInterpreterExecutor.Create;
  Engine := TGocciaEngine.Create(ProjectPath('app.js'), Source, Executor);
  try
    Runtime := AttachRuntime(Engine);
    Expect<Boolean>(InstallFFIIfGranted(Runtime)).ToBe(False);
    Raised := False;
    try
      Runtime.Install(TGocciaFFIRuntimeExtension.Create);
    except
      on E: EGocciaFFINotGranted do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
    Expect<Boolean>(Assigned(Runtime.FindRuntimeExtension(
      TGocciaFFIRuntimeExtension))).ToBe(False);
    Expect<Boolean>(Engine.Interpreter.GlobalScope.ContainsOwnLexicalBinding(
      'FFI')).ToBe(False);
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;

  Source := TStringList.Create;
  Executor := TGocciaInterpreterExecutor.Create;
  Engine := TGocciaEngine.Create(ProjectPath('app.js'), Source, Executor,
    TGocciaCapabilities.None.Allow(gcFFI));
  try
    Runtime := AttachRuntime(Engine);
    Expect<Boolean>(InstallFFIIfGranted(Runtime)).ToBe(True);
    Expect<Boolean>(Engine.Interpreter.GlobalScope.ContainsOwnLexicalBinding(
      'FFI')).ToBe(True);
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

procedure TEngineCapabilitiesTests.TestFFIOpenChecksLibraryScopes;
var
  Source: TStringList;
  Executor: TGocciaInterpreterExecutor;
  Engine: TGocciaEngine;
  Outcome: TRunOutcome;
begin
  Outcome := Default(TRunOutcome);
  Source := TStringList.Create;
  Source.Text := 'FFI.open("../outside/lib.so");';
  Executor := TGocciaInterpreterExecutor.Create;
  Engine := TGocciaEngine.Create(ProjectPath('app.js'), Source, Executor,
    TGocciaCapabilities.None.Allow(gcFFI, FProject));
  try
    Engine.CapabilityAuditSink := RecordEvent;
    InstallFFIIfGranted(AttachRuntime(Engine));
    try
      Engine.Execute;
    except
      on E: TGocciaThrowValue do
        CaptureThrown(E.Value, Outcome);
    end;
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
  Expect<string>(Outcome.ErrorName).ToBe('PermissionDenied');
  Expect<string>(Outcome.ErrorMessage).ToBe('ffi: ../outside/lib.so');
  Expect<Boolean>(FEvents.IndexOf('ffi.open|deny|../outside/lib.so') >= 0)
    .ToBe(True);
end;

function TEngineCapabilitiesTests.OpenLibrary(const ALibrary: string;
  const ACapabilities: TGocciaCapabilities): TRunOutcome;
var
  Source: TStringList;
  Executor: TGocciaInterpreterExecutor;
  Engine: TGocciaEngine;
begin
  Result := Default(TRunOutcome);
  Source := TStringList.Create;
  Source.Text := 'FFI.open("' + ALibrary + '");';
  Executor := TGocciaInterpreterExecutor.Create;
  Engine := TGocciaEngine.Create(ProjectPath('app.js'), Source, Executor,
    ACapabilities);
  try
    Engine.CapabilityAuditSink := RecordEvent;
    InstallFFIIfGranted(AttachRuntime(Engine));
    try
      Engine.Execute;
    except
      on E: TGocciaThrowValue do
      begin
        CaptureThrown(E.Value, Result);
        Result.Suggestion := E.Suggestion;
      end;
    end;
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

{ A name with no directory part is found by the platform loader's search
  path, not the working directory, so a path scope can never describe where
  it loads from: only an unscoped grant with no deny scope covers it. }
procedure TEngineCapabilitiesTests.TestFFIBareNamesNeedAnUnscopedGrant;
const
  BARE_NAME = 'libgoccia-capability-probe.so';
var
  Outcome: TRunOutcome;
begin
  Outcome := OpenLibrary(BARE_NAME,
    TGocciaCapabilities.None.Allow(gcFFI, GetCurrentDir));
  Expect<string>(Outcome.ErrorName).ToBe('PermissionDenied');
  Expect<string>(Outcome.ErrorMessage).ToBe('ffi: ' + BARE_NAME);

  Outcome := OpenLibrary(BARE_NAME,
    TGocciaCapabilities.None.Allow(gcFFI).Deny(gcFFI, FOutside));
  Expect<string>(Outcome.ErrorName).ToBe('PermissionDenied');

  { Unscoped and undenied: the capability allows it, and the load itself
    fails without naming any host path. }
  Outcome := OpenLibrary(BARE_NAME, TGocciaCapabilities.None.Allow(gcFFI));
  Expect<string>(Outcome.ErrorName).ToBe('TypeError');
  Expect<Boolean>(Pos(GetCurrentDir, Outcome.ErrorMessage) > 0).ToBe(False);

  { A path with a directory part is judged, and loaded, where it resolves. }
  Outcome := OpenLibrary('./' + BARE_NAME,
    TGocciaCapabilities.None.Allow(gcFFI, GetCurrentDir));
  Expect<string>(Outcome.ErrorName).ToBe('TypeError');
  Expect<Boolean>(Pos(GetCurrentDir, Outcome.ErrorMessage) > 0).ToBe(False);
end;

{ Resolution probes the host for extensions and index files, so resolving an
  existing file differs from resolving a missing one. Outside what the
  engine may read, import.meta.resolve must answer without probing, so the
  two are indistinguishable. Inside the project it still probes. }
procedure TEngineCapabilitiesTests.TestImportMetaResolveDoesNotProbeOutsideTheGrant;
const
  SOURCE_TEXT =
    'globalThis.result = [' +
    'import.meta.resolve("../outside/secret"),' +
    'import.meta.resolve("../outside/missing"),' +
    'import.meta.resolve("./lib")].map((u) => u.split("/").pop()).join("|");';
var
  Outcome: TRunOutcome;
begin
  Outcome := Run(SOURCE_TEXT, TGocciaCapabilities.None);
  Expect<string>(Outcome.ErrorMessage).ToBe('');
  Expect<string>(Outcome.Result).ToBe('secret|missing|lib.js');
  Outcome := Run(SOURCE_TEXT, TGocciaCapabilities.None.Allow(gcRead,
    FOutside));
  Expect<string>(Outcome.Result).ToBe('secret.js|missing|lib.js');
end;

{ The worker still resolves and checks an aborted request's destination;
  its decisions must reach the audit sink when the completion arrives while
  the engine is still running, even though the abort already settled the
  promise, and be attributed to the fetch() call that started it. The script
  hands control to PumpUntilAudited, standing in for any later work that
  drains fetch completions. }
function TEngineCapabilitiesTests.PumpUntilAudited(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
const
  SETTLE_DEADLINE_MS = 5000;
var
  Waited: Integer;
begin
  FAuditedHopIndex := -1;
  Waited := 0;
  while (FAuditedHopIndex < 0) and (Waited < SETTLE_DEADLINE_MS) do
  begin
    TGocciaFetchManager.Instance.PumpCompletions;
    if (FEvents.Count > 4) and (Pos('net.fetch|', FEvents[4]) = 1) then
      FAuditedHopIndex := 4
    else
    begin
      Sleep(1);
      Inc(Waited);
    end;
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TEngineCapabilitiesTests.TestAbortedFetchStillAuditsItsHops;
var
  Source: TStringList;
  Executor: TGocciaInterpreterExecutor;
  Engine: TGocciaEngine;
begin
  Source := TStringList.Create;
  Source.Text :=
    'const controller = new AbortController();' + sLineBreak +
    'const request = fetch("http://localhost:1/", ' +
    '{ signal: controller.signal });' + sLineBreak +
    'request.catch(() => {});' + sLineBreak +
    'controller.abort();' + sLineBreak +
    'pumpUntilAudited();';
  Executor := TGocciaInterpreterExecutor.Create;
  Engine := TGocciaEngine.Create(ProjectPath('abort.mjs'), Source, Executor,
    TGocciaCapabilities.None.Allow(gcNet, 'localhost')
      .Allow(gcNet, NET_PRIVATE_SCOPE));
  try
    Engine.CapabilityAuditSink := RecordEvent;
    AttachRuntime(Engine).Install(TGocciaFetchRuntimeExtension.Create);
    Engine.RegisterGlobal('pumpUntilAudited',
      TGocciaNativeFunctionValue.Create(PumpUntilAudited, 'pumpUntilAudited',
        0));
    Engine.Execute;
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
  { capabilities.effective, the name check, net.dispatch, the abandonment
    the abort records, then the replayed address check. }
  Expect<Integer>(FAuditedHopIndex).ToBe(4);
  { The abandonment names the host, like every net.fetch event. }
  Expect<string>(FEvents[3]).ToBe('net.fetch|allow|localhost');
  Expect<Boolean>(Pos('abandoned: ', FEventReasons[3]) = 1).ToBe(True);
  Expect<string>(FEventSources[4]).ToBe('abort.mjs:2');
end;

{ Execute discards the engine's requests when it returns, so a completion
  that arrives afterwards — here pumped by another engine still using the
  thread's fetch manager — must find nothing to report to: the first engine
  and its audit sink are gone. }
procedure TEngineCapabilitiesTests.TestAbortedFetchAuditEndsWithItsEngine;
const
  DRAIN_MS = 300;
var
  KeeperSource, Source: TStringList;
  KeeperExecutor, Executor: TGocciaInterpreterExecutor;
  Keeper, Engine: TGocciaEngine;
  EventsAfterFree, Waited: Integer;
begin
  KeeperSource := TStringList.Create;
  KeeperExecutor := TGocciaInterpreterExecutor.Create;
  Keeper := TGocciaEngine.Create(ProjectPath('keeper.js'), KeeperSource,
    KeeperExecutor);
  try
    AttachRuntime(Keeper).Install(TGocciaFetchRuntimeExtension.Create);
    Source := TStringList.Create;
    Source.Text :=
      'const controller = new AbortController();' + sLineBreak +
      'fetch("http://localhost:1/", { signal: controller.signal })' +
      '.catch(() => {});' + sLineBreak +
      'controller.abort();';
    Executor := TGocciaInterpreterExecutor.Create;
    Engine := TGocciaEngine.Create(ProjectPath('gone.mjs'), Source, Executor,
      TGocciaCapabilities.None.Allow(gcNet, 'localhost')
        .Allow(gcNet, NET_PRIVATE_SCOPE));
    try
      Engine.CapabilityAuditSink := RecordEvent;
      AttachRuntime(Engine).Install(TGocciaFetchRuntimeExtension.Create);
      Engine.Execute;
    finally
      Engine.Free;
      Executor.Free;
      Source.Free;
    end;
    EventsAfterFree := FEvents.Count;
    Waited := 0;
    while Waited < DRAIN_MS do
    begin
      TGocciaFetchManager.Instance.PumpCompletions;
      Sleep(1);
      Inc(Waited);
    end;
    Expect<Integer>(FEvents.Count).ToBe(EventsAfterFree);
  finally
    Keeper.Free;
    KeeperExecutor.Free;
    KeeperSource.Free;
  end;
end;

{ The resolver probes `<candidate>.js` and `<candidate>/index.js`. When a
  deny scope names one of those, whether the file exists must not decide
  between PermissionDenied and "Module not found". }
procedure TEngineCapabilitiesTests.TestDenyScopeHidesExistenceOfProbedFiles;
const
  SOURCE_TEXT =
    'globalThis.result = "pending";' + sLineBreak +
    'const names = ["../outside/secret", "../outside/absent"];' + sLineBreak +
    'Promise.all(names.map((n) => import(n).then(() => "loaded",' +
    ' (e) => e.name))).then((r) => { globalThis.result = r.join("|"); });';
var
  Outcome: TRunOutcome;
begin
  Outcome := Run(SOURCE_TEXT, TGocciaCapabilities.None.Allow(gcRead, FOutside)
    .Deny(gcRead, OutsidePath('secret.js'))
    .Deny(gcRead, OutsidePath('absent.js')));
  Expect<string>(Outcome.Result).ToBe('PermissionDenied|PermissionDenied');
end;

{ A module the host enrolled itself (globals, host environment, manifests)
  is cached under its address. A guest import of the same address is still a
  guest read and must be judged, not served from the cache. }
procedure TEngineCapabilitiesTests.TestHostLoadedModuleIsCheckedForTheGuest;
var
  Source: TStringList;
  Executor: TGocciaInterpreterExecutor;
  Engine: TGocciaEngine;
  Outcome: TRunOutcome;
begin
  Outcome := Default(TRunOutcome);
  Source := TStringList.Create;
  Source.Text :=
    'import { value } from "../outside/secret.js";' + sLineBreak +
    'globalThis.result = value;';
  Executor := TGocciaInterpreterExecutor.Create;
  Engine := TGocciaEngine.Create(ProjectPath('app.mjs'), Source, Executor);
  try
    AttachRuntime(Engine);
    Engine.InjectGlobalsFromModule(OutsidePath('secret.js'));
    try
      Engine.Execute;
    except
      on E: TGocciaThrowValue do
        CaptureThrown(E.Value, Outcome);
    end;
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
  Expect<string>(Outcome.ErrorName).ToBe('PermissionDenied');
  Expect<string>(Outcome.ErrorMessage).ToBe('read: ../outside/secret.js');
end;

procedure TEngineCapabilitiesTests.TestNodeModulesDenyThrowsPermissionDenied;
var
  Outcome: TRunOutcome;
begin
  Outcome := Run('import { value } from "pkg"; globalThis.result = value;',
    TGocciaCapabilities.None.Allow(gcImport, IMPORT_NODE_MODULES_SCOPE));
  Expect<string>(Outcome.Result).ToBe('package');
  Outcome := Run('import { value } from "pkg"; globalThis.result = value;',
    TGocciaCapabilities.None.Allow(gcImport, IMPORT_NODE_MODULES_SCOPE)
      .Deny(gcImport, IMPORT_NODE_MODULES_SCOPE + '=' + FProject));
  Expect<string>(Outcome.ErrorName).ToBe('PermissionDenied');
  Expect<string>(Outcome.ErrorMessage).ToBe('import: pkg');
end;

procedure TEngineCapabilitiesTests.TestShadowRealmInheritsCapabilities;
const
  SOURCE_TEXT =
    'globalThis.result = "pending";' + sLineBreak +
    'new ShadowRealm().importValue("../outside/secret.js", "value").then(' +
    '(v) => { globalThis.result = v; }, (e) => { globalThis.result = ' +
    '"rejected"; });';
var
  Outcome: TRunOutcome;
begin
  Outcome := Run(SOURCE_TEXT, TGocciaCapabilities.None, False, True);
  Expect<string>(Outcome.Result).ToBe('rejected');
  Outcome := Run(SOURCE_TEXT, TGocciaCapabilities.None.Allow(gcRead, FOutside),
    False, True);
  Expect<string>(Outcome.Result).ToBe('outside');
end;

{ A one-shot HTTP responder on the loopback interface: accepts up to
  AConnections connections and answers each with 200 "ok". }
{$IFDEF UNIX}
type
  TLoopbackResponder = class(TThread)
  private
    FListener: TSocket;
    FConnections: Integer;
    FPort: Word;
  protected
    procedure Execute; override;
  public
    constructor Create(const AConnections: Integer);
    destructor Destroy; override;
    property Port: Word read FPort;
  end;

constructor TLoopbackResponder.Create(const AConnections: Integer);
var
  Address: TInetSockAddr;
  AddressLength: TSockLen;
begin
  FConnections := AConnections;
  FListener := fpSocket(AF_INET, SOCK_STREAM, 0);
  FillChar(Address, SizeOf(Address), 0);
  Address.sin_family := AF_INET;
  Address.sin_port := 0;
  Address.sin_addr := StrToNetAddr('127.0.0.1');
  fpBind(FListener, @Address, SizeOf(Address));
  fpListen(FListener, AConnections);
  AddressLength := SizeOf(Address);
  fpGetSockName(FListener, @Address, @AddressLength);
  FPort := NToHs(Address.sin_port);
  inherited Create(False);
end;

destructor TLoopbackResponder.Destroy;
begin
  CloseSocket(FListener);
  inherited;
end;

procedure TLoopbackResponder.Execute;
const
  RESPONSE: AnsiString = 'HTTP/1.1 200 OK'#13#10'Content-Length: 2'#13#10 +
    'Connection: close'#13#10#13#10'ok';
var
  Client: TSocket;
  Buffer: array[0..4095] of Byte;
  I: Integer;
begin
  for I := 1 to FConnections do
  begin
    Client := fpAccept(FListener, nil, nil);
    if Client < 0 then
      Exit;
    fpRecv(Client, @Buffer[0], SizeOf(Buffer), 0);
    fpSend(Client, PAnsiChar(RESPONSE), Length(RESPONSE), 0);
    CloseSocket(Client);
  end;
end;
{$ENDIF}

{ Two engines share this thread's fetch manager and have a request in flight
  at the same time. `localhost` passes both engines' name check; only the
  engine that names `private` may connect to the loopback address it resolves
  to. Each request is judged by the set it carries, not by whichever engine
  happens to pump the completions. }
procedure TEngineCapabilitiesTests.TestFetchPolicyTravelsWithEachEngine;
{$IFDEF UNIX}
const
  SETTLE_DEADLINE_MS = 10000;
var
  Responder: TLoopbackResponder;
  SourceA, SourceB: TStringList;
  ExecutorA, ExecutorB: TGocciaInterpreterExecutor;
  EngineA, EngineB: TGocciaEngine;
  PromiseA, PromiseB: TGocciaPromiseValue;
  URL: string;
  Waited: Integer;

  function StartRequest(const AEngine: TGocciaEngine): TGocciaPromiseValue;
  var
    Policy: TGocciaFetchPolicy;
    Headers: THTTPHeaders;
  begin
    Policy := Default(TGocciaFetchPolicy);
    Policy.Capabilities := AEngine.Capabilities;
    SetLength(Headers, 0);
    Result := TGocciaPromiseValue.Create;
    TGarbageCollector.Instance.AddTempRoot(Result);
    TGocciaFetchManager.Instance.StartFetch(URL, 'GET', Headers, Policy,
      AEngine.Realm, Result);
  end;

begin
  Responder := TLoopbackResponder.Create(1);
  SourceA := TStringList.Create;
  SourceB := TStringList.Create;
  ExecutorA := TGocciaInterpreterExecutor.Create;
  ExecutorB := TGocciaInterpreterExecutor.Create;
  EngineA := TGocciaEngine.Create(ProjectPath('a.mjs'), SourceA, ExecutorA,
    TGocciaCapabilities.None.Allow(gcNet, 'localhost')
      .Allow(gcNet, NET_PRIVATE_SCOPE));
  EngineB := TGocciaEngine.Create(ProjectPath('b.mjs'), SourceB, ExecutorB,
    TGocciaCapabilities.None.Allow(gcNet, 'localhost'));
  PromiseA := nil;
  PromiseB := nil;
  try
    AttachRuntime(EngineA).Install(TGocciaFetchRuntimeExtension.Create);
    AttachRuntime(EngineB).Install(TGocciaFetchRuntimeExtension.Create);
    URL := 'http://localhost:' + IntToStr(Responder.Port) + '/';
    PromiseA := StartRequest(EngineA);
    PromiseB := StartRequest(EngineB);
    Expect<Boolean>(TGocciaFetchManager.Instance.HasPendingFor(EngineA.Realm))
      .ToBe(True);
    Expect<Boolean>(TGocciaFetchManager.Instance.HasPendingFor(EngineB.Realm))
      .ToBe(True);
    Waited := 0;
    while ((PromiseA.State = gpsPending) or (PromiseB.State = gpsPending)) and
          (Waited < SETTLE_DEADLINE_MS) do
      if TGocciaFetchManager.Instance.PumpCompletions = 0 then
      begin
        Sleep(1);
        Inc(Waited);
      end;
    Expect<Boolean>(PromiseA.State = gpsFulfilled).ToBe(True);
    Expect<Integer>(TGocciaResponseValue(PromiseA.PromiseResult).Status)
      .ToBe(200);
    Expect<Boolean>(PromiseB.State = gpsRejected).ToBe(True);
    Expect<string>(TGocciaObjectValue(PromiseB.PromiseResult)
      .GetProperty('message').ToStringLiteral.Value)
      .ToBe('net: localhost:' + IntToStr(Responder.Port));
  finally
    if Assigned(PromiseA) then
      TGarbageCollector.Instance.RemoveTempRoot(PromiseA);
    if Assigned(PromiseB) then
      TGarbageCollector.Instance.RemoveTempRoot(PromiseB);
    EngineB.Free;
    EngineA.Free;
    ExecutorB.Free;
    ExecutorA.Free;
    SourceB.Free;
    SourceA.Free;
    Responder.WaitFor;
    Responder.Free;
  end;
{$ELSE}
begin
  Expect<Boolean>(True).ToBe(True);
{$ENDIF}
end;

{ A symbolic link inside the project that names a file outside it is judged
  where it resolves: the static import is not exempt, so it needs a read
  grant covering the target. }
procedure TEngineCapabilitiesTests.TestSymlinkOutOfProjectNeedsRead;
{$IFDEF UNIX}
var
  LinkPath: string;
  Outcome: TRunOutcome;
{$ENDIF}
begin
  {$IFDEF UNIX}
  LinkPath := ProjectPath('link.js');
  DeleteFile(LinkPath);
  if fpSymlink(PAnsiChar(AnsiString(OutsidePath('secret.js'))),
     PAnsiChar(AnsiString(LinkPath))) <> 0 then
    Fail('could not create the test symlink');
  try
    Outcome := Run(
      'import { value } from "./link.js"; globalThis.result = value;',
      TGocciaCapabilities.None);
    Expect<string>(Outcome.ErrorName).ToBe('PermissionDenied');
    Expect<string>(Outcome.ErrorMessage).ToBe('read: ./link.js');
    Outcome := Run(
      'import { value } from "./link.js"; globalThis.result = value;',
      TGocciaCapabilities.None.Allow(gcRead, FOutside));
    Expect<string>(Outcome.Result).ToBe('outside');
  finally
    DeleteFile(LinkPath);
  end;
  {$ELSE}
  Expect<Boolean>(True).ToBe(True);
  {$ENDIF}
end;

{ The nested project's root is <project>/nested, so the package in
  <project>/node_modules lies outside it. Reached through the import grant,
  the package and its own literal imports belong to the module graph. }
procedure TEngineCapabilitiesTests.TestGrantedNodeModulesArePartOfTheGraph;
const
  SOURCE_TEXT = 'import { value } from "pkg"; globalThis.result = value;';
var
  Outcome: TRunOutcome;
begin
  Outcome := Run(SOURCE_TEXT,
    TGocciaCapabilities.None.Allow(gcImport, IMPORT_NODE_MODULES_SCOPE),
    False, False, ProjectPath('nested/app.mjs'));
  Expect<string>(Outcome.ErrorMessage).ToBe('');
  Expect<string>(Outcome.Result).ToBe('package');
  Expect<Integer>(EventsOfKind('read.file')).ToBe(0);

  { A ceiling below the package's node_modules does not cover it. }
  Outcome := Run(SOURCE_TEXT,
    TGocciaCapabilities.None.Allow(gcImport,
      IMPORT_NODE_MODULES_SCOPE + '=' + ProjectPath('nested')),
    False, False, ProjectPath('nested/app.mjs'));
  Expect<Boolean>(Outcome.Result <> 'package').ToBe(True);

  { A deny still wins over the exemption. }
  Outcome := Run(SOURCE_TEXT,
    TGocciaCapabilities.None.Allow(gcImport, IMPORT_NODE_MODULES_SCOPE)
      .Deny(gcRead, ProjectPath('node_modules/pkg/detail.js')),
    False, False, ProjectPath('nested/app.mjs'));
  Expect<string>(Outcome.ErrorName).ToBe('PermissionDenied');
end;

{ A lexical node_modules segment proves nothing: the exemption belongs to
  what bare-specifier resolution produced. A symlink named like a package, or
  a node_modules directory in an unrelated tree, is an ordinary read. }
procedure TEngineCapabilitiesTests.TestNodeModulesInAPathIsNoGrant;
var
  Grant: TGocciaCapabilities;
  Outcome: TRunOutcome;
{$IFDEF UNIX}
  LinkPath: string;
{$ENDIF}
begin
  Grant := TGocciaCapabilities.None.Allow(gcImport, IMPORT_NODE_MODULES_SCOPE);
  Outcome := Run('import s from "' + IncludeTrailingPathDelimiter(FRoot) +
    'other/node_modules/pkg/secret.json" with { type: "json" };' +
    ' globalThis.result = s.secret;', Grant);
  Expect<string>(Outcome.ErrorName).ToBe('PermissionDenied');
  Expect<string>(Outcome.Result).ToBe('undefined');
  {$IFDEF UNIX}
  LinkPath := ProjectPath('node_modules/evil');
  DeleteFile(LinkPath);
  if fpSymlink(PAnsiChar(AnsiString(FOutside)),
     PAnsiChar(AnsiString(LinkPath))) <> 0 then
    Fail('could not create the test symlink');
  try
    Outcome := Run('import k from "./node_modules/evil/data.bin" with ' +
      '{ type: "text" }; globalThis.result = k;', Grant);
    Expect<string>(Outcome.ErrorName).ToBe('PermissionDenied');
    Expect<string>(Outcome.Result).ToBe('undefined');
    Outcome := Run('import { value } from "./node_modules/evil/secret.js";' +
      ' globalThis.result = value;', Grant, True);
    Expect<string>(Outcome.ErrorName).ToBe('PermissionDenied');
  finally
    DeleteFile(LinkPath);
  end;
  {$ENDIF}
end;

{ Workspaces and pnpm link packages from elsewhere. Resolved through the
  grant, the package's canonical root is part of the graph: its own literal
  imports need no read grant while they stay inside it. }
procedure TEngineCapabilitiesTests.TestLinkedPackageIsPartOfTheGraph;
{$IFDEF UNIX}
var
  LinkPath: string;
  Outcome: TRunOutcome;
{$ENDIF}
begin
  {$IFDEF UNIX}
  LinkPath := ProjectPath('node_modules/linked');
  DeleteFile(LinkPath);
  if fpSymlink(PAnsiChar(AnsiString(IncludeTrailingPathDelimiter(FRoot) +
     'linkedpkg')), PAnsiChar(AnsiString(LinkPath))) <> 0 then
    Fail('could not create the test symlink');
  try
    Outcome := Run('import { value } from "linked"; globalThis.result = value;',
      TGocciaCapabilities.None.Allow(gcImport, IMPORT_NODE_MODULES_SCOPE));
    Expect<string>(Outcome.ErrorMessage).ToBe('');
    Expect<string>(Outcome.Result).ToBe('linked');
    Expect<Integer>(EventsOfKind('read.file')).ToBe(0);
    { Without the grant the same files are an ordinary read. }
    Outcome := Run('import { value } from "./node_modules/linked/index.js";' +
      ' globalThis.result = value;', TGocciaCapabilities.None);
    Expect<string>(Outcome.ErrorName).ToBe('PermissionDenied');
  finally
    DeleteFile(LinkPath);
  end;
  {$ELSE}
  Expect<Boolean>(True).ToBe(True);
  {$ENDIF}
end;

{ A deny scope only refuses what a request actually reads or probes: a
  sibling sharing the stem, a file next to the resolved one, or a file inside
  the directory an index import resolves through is no reason to refuse. }
procedure TEngineCapabilitiesTests.TestDenyScopesJudgeTheResolvedPath;
var
  Outcome: TRunOutcome;
begin
  Outcome := Run('import { value } from "./lib"; globalThis.result = value;',
    TGocciaCapabilities.None.Deny(gcRead, ProjectPath('lib-private')));
  Expect<string>(Outcome.ErrorMessage).ToBe('');
  Expect<string>(Outcome.Result).ToBe('inside');
  Outcome := Run('import { value } from "./lib.js"; globalThis.result = value;',
    TGocciaCapabilities.None.Deny(gcRead, ProjectPath('lib.js.map')));
  Expect<string>(Outcome.ErrorMessage).ToBe('');
  Expect<string>(Outcome.Result).ToBe('inside');
  Outcome := Run('import { value } from "./sub"; globalThis.result = value;',
    TGocciaCapabilities.None.Deny(gcRead, ProjectPath('sub/secret.json')));
  Expect<string>(Outcome.ErrorMessage).ToBe('');
  Expect<string>(Outcome.Result).ToBe('sub');
  Outcome := Run('import { value } from "../outside/secret";' +
    ' globalThis.result = value;', TGocciaCapabilities.None
    .Allow(gcRead, FOutside).Deny(gcRead, OutsidePath('secret-notes')));
  Expect<string>(Outcome.ErrorMessage).ToBe('');
  Expect<string>(Outcome.Result).ToBe('outside');
end;

{ The resolver tries `<stem>`, `<stem>.<ext>`, then `<stem>/index.<ext>`.
  Reaching a denied candidate refuses on the spot, so whether the denied
  file exists can never pick between PermissionDenied, "Module not found",
  or a later candidate loading. }
procedure TEngineCapabilitiesTests.TestDenyScopeHidesExistenceInsideProject;
const
  SOURCE_TEXT =
    'globalThis.result = "pending";' + sLineBreak +
    'Promise.all([import("./hidden"), import("./gone"), import("./shadow")]' +
    '.map((p) => p.then(() => "loaded", (e) => e.name)))' +
    '.then((r) => { globalThis.result = r.join("|"); });';
var
  Outcome: TRunOutcome;
  Denies: TGocciaCapabilities;
begin
  Denies := TGocciaCapabilities.None.Deny(gcRead, ProjectPath('hidden.js'))
    .Deny(gcRead, ProjectPath('gone.js'))
    .Deny(gcRead, ProjectPath('shadow.js'));
  Outcome := Run(SOURCE_TEXT, Denies);
  Expect<string>(Outcome.Result)
    .ToBe('PermissionDenied|PermissionDenied|PermissionDenied');
  WriteFile(ProjectPath('shadow.js'), 'export const value = "file";');
  try
    Outcome := Run(SOURCE_TEXT, Denies, True);
    Expect<string>(Outcome.Result)
      .ToBe('PermissionDenied|PermissionDenied|PermissionDenied');
  finally
    DeleteFile(ProjectPath('shadow.js'));
  end;
end;

{ Like an import, import.meta.resolve may not tell a guest whether a denied
  file exists; refused, it answers lexically. }
procedure TEngineCapabilitiesTests.TestImportMetaResolveHonoursDenyScopes;
const
  SOURCE_TEXT =
    'globalThis.result = [' +
    'import.meta.resolve("./hidden"),' +
    'import.meta.resolve("./gone"),' +
    'import.meta.resolve("../outside/secret"),' +
    'import.meta.resolve("./lib")].map((u) => u.split("/").pop()).join("|");';
var
  Outcome: TRunOutcome;
begin
  Outcome := Run(SOURCE_TEXT, TGocciaCapabilities.None
    .Allow(gcRead, FOutside)
    .Deny(gcRead, ProjectPath('hidden.js'))
    .Deny(gcRead, ProjectPath('gone.js'))
    .Deny(gcRead, OutsidePath('secret.js'))
    .Deny(gcRead, ProjectPath('lib-private')));
  Expect<string>(Outcome.ErrorMessage).ToBe('');
  Expect<string>(Outcome.Result).ToBe('hidden|gone|secret|lib.js');
end;

{ A bare specifier a virtual module serves never reaches node_modules, so no
  node_modules decision belongs in the audit log. }
procedure TEngineCapabilitiesTests.TestVirtualBareModuleAuditsNoNodeModules;
const
  SOURCE_TEXT = 'import { x } from "virt"; globalThis.result = x;';
begin
  FVirtualModuleName := 'virt';
  FVirtualModuleSource := 'export const x = 7;';
  Expect<string>(Run(SOURCE_TEXT, TGocciaCapabilities.None).Result).ToBe('7');
  Expect<Integer>(EventsOfKind('import.node-modules')).ToBe(0);
  Expect<string>(Run(SOURCE_TEXT, TGocciaCapabilities.None.Allow(gcImport,
    IMPORT_NODE_MODULES_SCOPE)).Result).ToBe('7');
  Expect<Integer>(EventsOfKind('import.node-modules')).ToBe(0);
  { A real bare import still records its decision. }
  FVirtualModuleName := '';
  Run('import { value } from "pkg"; globalThis.result = value;',
    TGocciaCapabilities.None.Allow(gcImport, IMPORT_NODE_MODULES_SCOPE));
  Expect<Boolean>(EventsOfKind('import.node-modules') > 0).ToBe(True);
end;


{ Execute discards the engine's requests when the script ends, so an
  aborted request's worker may report its address and redirect decisions
  too late to audit. The abort itself records that, attributed to the
  fetch() call, so the gap is explicit in the log. }
procedure TEngineCapabilitiesTests.TestAbortAtScriptEndRecordsAbandonment;
var
  Source: TStringList;
  Executor: TGocciaInterpreterExecutor;
  Engine: TGocciaEngine;
  EventIndex, Index: Integer;
begin
  Source := TStringList.Create;
  Source.Text :=
    'const controller = new AbortController();' + sLineBreak +
    'fetch("http://localhost:1/private?token=secret",' +
    ' { signal: controller.signal })' +
    '.catch(() => {});' + sLineBreak +
    'controller.abort();';
  Executor := TGocciaInterpreterExecutor.Create;
  Engine := TGocciaEngine.Create(ProjectPath('end.mjs'), Source, Executor,
    TGocciaCapabilities.None.Allow(gcNet, 'localhost')
      .Allow(gcNet, NET_PRIVATE_SCOPE));
  try
    Engine.CapabilityAuditSink := RecordEvent;
    AttachRuntime(Engine).Install(TGocciaFetchRuntimeExtension.Create);
    Engine.Execute;
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
  EventIndex := -1;
  for Index := 0 to FEvents.Count - 1 do
    if Pos('abandoned: ', FEventReasons[Index]) = 1 then
      EventIndex := Index;
  Expect<Boolean>(EventIndex >= 0).ToBe(True);
  if EventIndex >= 0 then
  begin
    { The subject is the host, never the URL with its path and query. }
    Expect<string>(FEvents[EventIndex]).ToBe('net.fetch|allow|localhost');
    Expect<string>(FEventSources[EventIndex]).ToBe('end.mjs:2');
  end;
end;


{ An alias (or import-map entry) rewrites a specifier to a host path; every
  candidate the resolver then probes is judged first, so a refused request
  cannot tell an existing file from a missing one, and `..` in the tail
  cannot probe past the alias target unjudged. import.meta.resolve answers
  with the alias-applied path when refused. }
procedure TEngineCapabilitiesTests.TestAliasCandidatesAreJudged;
const
  IMPORTS =
    'globalThis.result = "pending";' + sLineBreak +
    'Promise.all([import("@x/secret.js"), import("@x/nothere.js"),' +
    ' import("@x/../other/node_modules/pkg/secret.json",' +
    ' { with: { type: "json" } })]' +
    '.map((p) => p.then(() => "loaded", (e) => e.name)))' +
    '.then((r) => { globalThis.result = r.join("|"); });';
  RESOLVES =
    'globalThis.result = [import.meta.resolve("@x/secret"),' +
    ' import.meta.resolve("@x/nothere")]' +
    '.map((u) => u.slice(u.lastIndexOf("/outside/"))).join("|");';
var
  Outcome: TRunOutcome;
begin
  FAliasPattern := '@x/';
  FAliasTarget := IncludeTrailingPathDelimiter(FOutside);
  Outcome := Run(IMPORTS, TGocciaCapabilities.None);
  Expect<string>(Outcome.Result)
    .ToBe('PermissionDenied|PermissionDenied|PermissionDenied');
  Outcome := Run(RESOLVES, TGocciaCapabilities.None);
  Expect<string>(Outcome.ErrorMessage).ToBe('');
  Expect<string>(Outcome.Result).ToBe('/outside/secret|/outside/nothere');
  { A read grant over the alias target lets the same imports through. }
  Outcome := Run('import { value } from "@x/secret.js";' +
    ' globalThis.result = value;',
    TGocciaCapabilities.None.Allow(gcRead, FOutside));
  Expect<string>(Outcome.Result).ToBe('outside');
end;

{ The package's exports target "./main" is probed as main, main.js, ...,
  then main/index.js. A deny on main.js must refuse whether or not main.js
  exists, as it does for a relative import. }
procedure TEngineCapabilitiesTests.TestPackageProbesAreJudged;
const
  SOURCE_TEXT = 'import { value } from "probe"; globalThis.result = value;';
var
  Grant: TGocciaCapabilities;
  Outcome: TRunOutcome;
begin
  Grant := TGocciaCapabilities.None.Allow(gcImport, IMPORT_NODE_MODULES_SCOPE);
  Outcome := Run(SOURCE_TEXT, Grant);
  Expect<string>(Outcome.Result).ToBe('directory');
  Outcome := Run(SOURCE_TEXT,
    Grant.Deny(gcRead, ProjectPath('node_modules/probe/main.js')));
  Expect<string>(Outcome.ErrorName).ToBe('PermissionDenied');
  Expect<string>(Outcome.ErrorMessage).ToBe('read: probe');
end;


{ ADR 0014: a denial raised by a native call is located at the call
  expression's own position in both executors, as import() is. }
procedure TEngineCapabilitiesTests.TestCallDenialSitesMatchAcrossExecutors;
const
  FETCH_SOURCE = 'const x = 1;' + sLineBreak +
    '  globalThis.result = fetch("http://example.com/");';
  FFI_SOURCE = 'const x = 1;' + sLineBreak +
    '  globalThis.result = FFI.open("../outside/lib.so");';
var
  Interpreted, Bytecode: TRunOutcome;
  Grant: TGocciaCapabilities;
begin
  FInstallFetchAndFFI := True;
  Grant := TGocciaCapabilities.None.Allow(gcFFI, FProject);
  Interpreted := Run(FETCH_SOURCE, Grant);
  Bytecode := Run(FETCH_SOURCE, Grant, True);
  Expect<string>(Interpreted.ErrorName).ToBe('PermissionDenied');
  Expect<string>(Bytecode.ErrorName).ToBe('PermissionDenied');
  Expect<string>(Interpreted.Location).ToBe('app.mjs:2:28');
  Expect<string>(Bytecode.Location).ToBe(Interpreted.Location);
  { The throw marks the denial's suggestion host-only in both executors,
    through the VM unwind too, so guest-bound output can drop it. }
  Expect<Boolean>(Interpreted.Suggestion <> '').ToBe(True);
  Expect<Boolean>(Interpreted.SuggestionIsHostOnly).ToBe(True);
  Expect<Boolean>(Bytecode.SuggestionIsHostOnly).ToBe(True);
  Interpreted := Run(FFI_SOURCE, Grant);
  Bytecode := Run(FFI_SOURCE, Grant, True);
  Expect<string>(Interpreted.ErrorName).ToBe('PermissionDenied');
  Expect<string>(Bytecode.ErrorName).ToBe('PermissionDenied');
  Expect<Boolean>(Interpreted.Location <> '').ToBe(True);
  Expect<string>(Bytecode.Location).ToBe(Interpreted.Location);
end;


{ ADR 0014: a static import or export-from denial is located at its
  declaration in both executors, whether the entry runs as a script or a
  module. }
procedure TEngineCapabilitiesTests.TestStaticImportDenialSitesMatchAcrossExecutors;

  procedure ExpectLocatedAlike(const ASource, AEntry, AExpected: string;
    const ACapabilities: TGocciaCapabilities);
  var
    Interpreted, Bytecode: TRunOutcome;
  begin
    Interpreted := Run(ASource, ACapabilities, False, False, AEntry);
    Bytecode := Run(ASource, ACapabilities, True, False, AEntry);
    Expect<string>(Interpreted.ErrorName).ToBe('PermissionDenied');
    Expect<string>(Bytecode.ErrorName).ToBe('PermissionDenied');
    Expect<string>(Interpreted.Location).ToBe(AExpected);
    Expect<string>(Bytecode.Location).ToBe(AExpected);
  end;

const
  OUTSIDE_IMPORT = 'const x = 1;' + sLineBreak +
    'import { value } from "../outside/secret.js";';
  OUTSIDE_REEXPORT = 'const x = 1;' + sLineBreak +
    'export { value } from "../outside/secret.js";';
  BARE_IMPORT = 'const x = 1;' + sLineBreak +
    'import { value } from "pkg";';
var
  Entry: string;
begin
  for Entry in [ProjectPath('app.js'), ProjectPath('app.mjs')] do
  begin
    ExpectLocatedAlike(OUTSIDE_IMPORT, Entry,
      ExtractFileName(Entry) + ':2:1', TGocciaCapabilities.None);
    ExpectLocatedAlike(OUTSIDE_IMPORT, Entry,
      ExtractFileName(Entry) + ':2:1', TGocciaCapabilities.None.Deny(gcRead));
    ExpectLocatedAlike(BARE_IMPORT, Entry, ExtractFileName(Entry) + ':2:1',
      TGocciaCapabilities.None.Deny(gcImport, IMPORT_NODE_MODULES_SCOPE));
  end;
  { export-from belongs to modules. }
  ExpectLocatedAlike(OUTSIDE_REEXPORT, ProjectPath('app.mjs'), 'app.mjs:2:1',
    TGocciaCapabilities.None);
end;

begin
  TestRunnerProgram.AddSuite(
    TEngineCapabilitiesTests.Create('Engine capabilities'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
