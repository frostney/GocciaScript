program Goccia.Engine.Capabilities.Test;

{ Engine and runtime behavior driven by the capability set (ADR 0122): which
  host reads are exempt, granted, or denied; how PermissionDenied reaches the
  guest; FFI gating; node_modules; and inheritance by child realms. }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  FileUtils,
  TestingPascalLibrary,

  Goccia.Builtins.GlobalShadowRealm,
  Goccia.Capabilities,
  Goccia.CapabilityAudit,
  Goccia.Engine,
  Goccia.Error,
  Goccia.Executor,
  Goccia.Executor.Bytecode,
  Goccia.Executor.Interpreter,
  Goccia.Modules,
  Goccia.Modules.ContentProvider,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.Fetch,
  Goccia.RuntimeExtensions.FFI,
  Goccia.TestSetup,
  Goccia.Values.Error,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
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
    procedure TestHostLoadedModuleOutsideProjectIsExempt;
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
    procedure TestNodeModulesDenyThrowsPermissionDenied;
    procedure TestShadowRealmInheritsCapabilities;
    procedure TestFetchPolicyTravelsWithEachEngine;
  public
    procedure SetupTests; override;
  end;

procedure TEngineCapabilitiesTests.SetupTests;
begin
  Test('The filesystem provider follows the read capability',
    TestProviderFollowsReadCapability);
  Test('A static literal import inside the project needs no grant',
    TestStaticImportInsideProjectIsExempt);
  Test('A static import outside the project is denied without a host path',
    TestStaticImportOutsideProjectIsDenied);
  Test('A module the host loads itself is exempt outside the project',
    TestHostLoadedModuleOutsideProjectIsExempt);
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
  Test('A node_modules deny throws PermissionDenied',
    TestNodeModulesDenyThrowsPermissionDenied);
  Test('A ShadowRealm child inherits its creator''s capability set',
    TestShadowRealmInheritsCapabilities);
  Test('Two engines on one thread keep their own fetch policy',
    TestFetchPolicyTravelsWithEachEngine);
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
end;

procedure TEngineCapabilitiesTests.AfterAll;
begin
  FEvents.Free;
  DeleteTree(FRoot);
  inherited AfterAll;
end;

procedure TEngineCapabilitiesTests.BeforeEach;
begin
  inherited BeforeEach;
  FEvents.Clear;
end;

procedure TEngineCapabilitiesTests.RecordEvent(
  const AEvent: TGocciaCapabilityAuditEvent);
begin
  FEvents.Add(CapabilityKindName(AEvent.Kind) + '|' +
    CapabilityDecisionName(AEvent.Decision) + '|' + AEvent.Subject);
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
    Engine := TGocciaEngine.Create(ProjectPath('app.js'), Source, Executor,
      TGocciaCapabilities.None.Deny(gcRead));
    try
      AttachRuntime(Engine);
      Expect<Boolean>(Engine.ContentProvider is
        TGocciaUnavailableModuleContentProvider).ToBe(True);
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

procedure TEngineCapabilitiesTests.TestUnscopedDenyRemovesExemption;
var
  Outcome: TRunOutcome;
begin
  Outcome := Run('import { value } from "./lib.js"; globalThis.result = value;',
    TGocciaCapabilities.None.Deny(gcRead));
  Expect<Boolean>(Outcome.ErrorMessage <> '').ToBe(True);
  Expect<string>(Outcome.Result).ToBe('undefined');
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

{ Two engines live on this thread at once and share its fetch manager, but
  each request carries its own engine's capability set. `localhost` passes
  both engines' name check; only the engine that names `private` may connect
  to the loopback address it resolves to. Port 1 refuses the connection, so
  the permitted request settles with a network TypeError instead. }
procedure TEngineCapabilitiesTests.TestFetchPolicyTravelsWithEachEngine;
const
  SOURCE_TEXT =
    'globalThis.result = "pending";' + sLineBreak +
    'fetch("http://localhost:1/").then(() => { globalThis.result = "ok"; },' +
    ' (e) => { globalThis.result = e.name + "|" + e.message; });';
var
  SourceA, SourceB: TStringList;
  ExecutorA, ExecutorB: TGocciaInterpreterExecutor;
  EngineA, EngineB: TGocciaEngine;
  ResultA, ResultB: string;
begin
  SourceA := TStringList.Create;
  SourceB := TStringList.Create;
  SourceA.Text := SOURCE_TEXT;
  SourceB.Text := SOURCE_TEXT;
  ExecutorA := TGocciaInterpreterExecutor.Create;
  ExecutorB := TGocciaInterpreterExecutor.Create;
  EngineA := TGocciaEngine.Create(ProjectPath('a.mjs'), SourceA, ExecutorA,
    TGocciaCapabilities.None.Allow(gcNet, 'localhost')
      .Allow(gcNet, NET_PRIVATE_SCOPE));
  EngineB := TGocciaEngine.Create(ProjectPath('b.mjs'), SourceB, ExecutorB,
    TGocciaCapabilities.None.Allow(gcNet, 'localhost'));
  try
    AttachRuntime(EngineA).Install(TGocciaFetchRuntimeExtension.Create);
    AttachRuntime(EngineB).Install(TGocciaFetchRuntimeExtension.Create);
    EngineB.Execute;
    EngineB.WaitForRuntimeIdle;
    EngineA.Execute;
    EngineA.WaitForRuntimeIdle;
    ResultA := TGocciaObjectValue(EngineA.Realm.GlobalObject)
      .GetProperty('result').ToStringLiteral.Value;
    ResultB := TGocciaObjectValue(EngineB.Realm.GlobalObject)
      .GetProperty('result').ToStringLiteral.Value;
  finally
    EngineB.Free;
    EngineA.Free;
    ExecutorB.Free;
    ExecutorA.Free;
    SourceB.Free;
    SourceA.Free;
  end;
  Expect<string>(ResultB).ToBe('PermissionDenied|net: localhost:1');
  Expect<Boolean>(Pos('TypeError|', ResultA) = 1).ToBe(True);
end;

begin
  TestRunnerProgram.AddSuite(
    TEngineCapabilitiesTests.Create('Engine capabilities'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
