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
  end;

  TEngineCapabilitiesTests = class(TTestSuite)
  private
    FRoot: string;
    FProject: string;
    FOutside: string;
    FEvents: TStringList;
    FEventSources: TStringList;
    FAuditedHopIndex: Integer;
    function PumpUntilAudited(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure RecordEvent(const AEvent: TGocciaCapabilityAuditEvent);
    function ProjectPath(const AName: string): string;
    function OutsidePath(const AName: string): string;
    function Run(const ASource: string;
      const ACapabilities: TGocciaCapabilities;
      const ABytecode: Boolean = False;
      const AShadowRealm: Boolean = False): TRunOutcome;
    function EventsOfKind(const AKind: string): Integer;
  protected
    procedure BeforeAll; override;
    procedure AfterAll; override;
    procedure BeforeEach; override;
  private
    procedure TestProviderFollowsReadCapability;
    procedure TestStaticImportInsideProjectIsExempt;
    procedure TestStaticImportOutsideProjectIsDenied;
    procedure TestReadGrantCoversOutsidePath;
    procedure TestMissingOutsideFileIsDeniedBeforeProbing;
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
  Test('A read grant covers a path outside the project',
    TestReadGrantCoversOutsidePath);
  Test('A missing file outside the project is denied before probing',
    TestMissingOutsideFileIsDeniedBeforeProbing);
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
    'export const value = "package";');
  FEvents := TStringList.Create;
  FEventSources := TStringList.Create;
end;

procedure TEngineCapabilitiesTests.AfterAll;
begin
  FEvents.Free;
  FEventSources.Free;
  DeleteTree(FRoot);
  inherited AfterAll;
end;

procedure TEngineCapabilitiesTests.BeforeEach;
begin
  inherited BeforeEach;
  FEvents.Clear;
  FEventSources.Clear;
end;

procedure TEngineCapabilitiesTests.RecordEvent(
  const AEvent: TGocciaCapabilityAuditEvent);
begin
  FEvents.Add(CapabilityKindName(AEvent.Kind) + '|' +
    CapabilityDecisionName(AEvent.Decision) + '|' + AEvent.Subject);
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
end;

function TEngineCapabilitiesTests.Run(const ASource: string;
  const ACapabilities: TGocciaCapabilities; const ABytecode: Boolean;
  const AShadowRealm: Boolean): TRunOutcome;
var
  Source: TStringList;
  Executor: TGocciaExecutor;
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
  Engine := TGocciaEngine.Create(ProjectPath('app.mjs'), Source, Executor,
    ACapabilities);
  try
    Engine.CapabilityAuditSink := RecordEvent;
    AttachRuntime(Engine);
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
      end;
      on E: EGocciaBytecodeThrow do
      begin
        CaptureThrown(E.ThrownValue, Result);
        Result.Suggestion := E.Suggestion;
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
        CaptureThrown(E.Value, Result);
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
    if (FEvents.Count > 3) and (Pos('net.fetch|', FEvents[3]) = 1) then
      FAuditedHopIndex := 3
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
  { capabilities.effective, the name check, net.dispatch, then the replayed
    address check for the aborted request. }
  Expect<Integer>(FAuditedHopIndex).ToBe(3);
  Expect<string>(FEventSources[3]).ToBe('abort.mjs:2');
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

begin
  TestRunnerProgram.AddSuite(
    TEngineCapabilitiesTests.Create('Engine capabilities'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
