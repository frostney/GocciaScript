program Goccia.CapabilityAudit.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  SandboxVirtualFileSystem,
  TestingPascalLibrary,

  Goccia.Capabilities,
  Goccia.CapabilityAudit,
  Goccia.Engine,
  Goccia.Executor,
  Goccia.Executor.Bytecode,
  Goccia.Executor.Interpreter,
  Goccia.Modules,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.Sandbox,
  Goccia.Sandbox.Context,
  Goccia.TestSetup;

type
  ECapabilityAuditSinkFailure = class(Exception);

  TFailingRootClampRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FContext: TGocciaSandboxContext;
    FPreviousRootClampCallback: TSandboxRootClampCallback;
    procedure ReplacementRootClamp(const APath, ABase,
      ACanonicalPath: string);
  public
    constructor Create(const AContext: TGocciaSandboxContext);
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
    procedure Detach; override;
  end;

  TCapabilityAuditTests = class(TTestSuite)
  private
    FRootClampCount: Integer;
    FSinkInvocationCount: Integer;
    FRecordedEvents: TStringList;
    procedure FailingSink(const AEvent: TGocciaCapabilityAuditEvent);
    procedure RecordingSink(const AEvent: TGocciaCapabilityAuditEvent);
    procedure RootClampSentinel(const APath, ABase,
      ACanonicalPath: string);
    procedure TestSerializesStructuredEvent;
    procedure TestSerializesMissingCoordinatesAsNull;
    procedure TestSinkFailurePropagates;
    procedure TestSandboxPromiseCannotCatchSinkFailure;
    procedure TestVMAsyncIteratorCannotCatchSinkFailure;
    procedure TestFailedRuntimeInstallRestoresRootClampCallback;
    procedure TestSandboxDetachRestoresExistingModules;
    procedure TestEffectiveCapabilitiesEmittedOnce;
    procedure TestEffectiveCapabilitiesPrecedeFirstEvent;
    procedure TestChildContextDoesNotRepeatEffectiveSet;
    procedure TestNodeModulesResolutionEmitsImportAudit;
    procedure TestUngrantedNodeModulesEmitsDeny;
  public
    procedure SetupTests; override;
  end;

procedure TCapabilityAuditTests.SetupTests;
begin
  Test('Serializes the versioned structured event',
    TestSerializesStructuredEvent);
  Test('Serializes unavailable coordinates as null',
    TestSerializesMissingCoordinatesAsNull);
  Test('Sink failures propagate to the host', TestSinkFailurePropagates);
  Test('Sandbox promises cannot catch sink failures',
    TestSandboxPromiseCannotCatchSinkFailure);
  Test('VM async iterators cannot catch sink failures',
    TestVMAsyncIteratorCannotCatchSinkFailure);
  Test('Failed runtime installation restores the root clamp callback',
    TestFailedRuntimeInstallRestoresRootClampCallback);
  Test('Sandbox detach restores existing runtime modules',
    TestSandboxDetachRestoresExistingModules);
  Test('capabilities.effective is emitted once per engine',
    TestEffectiveCapabilitiesEmittedOnce);
  Test('capabilities.effective precedes the first other event',
    TestEffectiveCapabilitiesPrecedeFirstEvent);
  Test('A child context does not repeat capabilities.effective',
    TestChildContextDoesNotRepeatEffectiveSet);
  Test('Each granted node_modules resolution emits import.node-modules',
    TestNodeModulesResolutionEmitsImportAudit);
  Test('A bare specifier without the import grant emits a deny',
    TestUngrantedNodeModulesEmitsDeny);
end;

constructor TFailingRootClampRuntimeExtension.Create(
  const AContext: TGocciaSandboxContext);
begin
  inherited Create;
  FContext := AContext;
end;

procedure TFailingRootClampRuntimeExtension.Attach(
  const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);
  FPreviousRootClampCallback := FContext.Fs.RootClampCallback;
  FContext.Fs.RootClampCallback := ReplacementRootClamp;
  raise Exception.Create('attachment failed');
end;

procedure TFailingRootClampRuntimeExtension.Detach;
begin
  FContext.Fs.RootClampCallback := FPreviousRootClampCallback;
  FPreviousRootClampCallback := nil;
  inherited;
end;

procedure TFailingRootClampRuntimeExtension.ReplacementRootClamp(
  const APath, ABase, ACanonicalPath: string);
begin
end;

procedure TCapabilityAuditTests.FailingSink(
  const AEvent: TGocciaCapabilityAuditEvent);
begin
  Inc(FSinkInvocationCount);
  raise ECapabilityAuditSinkFailure.Create(AEvent.Subject);
end;

procedure TCapabilityAuditTests.RecordingSink(
  const AEvent: TGocciaCapabilityAuditEvent);
begin
  FRecordedEvents.Add(CapabilityKindName(AEvent.Kind) + '|' +
    CapabilityDecisionName(AEvent.Decision) + '|' + AEvent.Subject);
end;

procedure TCapabilityAuditTests.RootClampSentinel(
  const APath, ABase, ACanonicalPath: string);
begin
  Inc(FRootClampCount);
end;

procedure TCapabilityAuditTests.TestSerializesStructuredEvent;
var
  AuditEvent: TGocciaCapabilityAuditEvent;
begin
  AuditEvent.Kind := gckNetFetch;
  AuditEvent.Decision := gcdDeny;
  AuditEvent.Subject := 'https://blocked.test/"quoted"';
  AuditEvent.Reason := 'host is not allowed';
  AuditEvent.Source.FilePath := 'app.js';
  AuditEvent.Source.Line := 7;
  AuditEvent.Source.Column := 3;

  Expect<string>(AuditEvent.ToJSON).ToBe(
    '{"schemaVersion":1,"kind":"net.fetch","decision":"deny",' +
    '"subject":"https://blocked.test/\"quoted\"",' +
    '"reason":"host is not allowed",' +
    '"source":{"file":"app.js","line":7,"column":3}}');
end;

procedure TCapabilityAuditTests.TestSerializesMissingCoordinatesAsNull;
var
  AuditEvent: TGocciaCapabilityAuditEvent;
begin
  AuditEvent.Kind := gckShadowRealm;
  AuditEvent.Decision := gcdAllow;
  AuditEvent.Subject := 'ShadowRealm';
  AuditEvent.Reason := 'enabled';
  AuditEvent.Source.FilePath := '<shadow-realm>';
  AuditEvent.Source.Line := 0;
  AuditEvent.Source.Column := 0;

  Expect<string>(AuditEvent.ToJSON).ToBe(
    '{"schemaVersion":1,"kind":"shadow-realm.construct",' +
    '"decision":"allow","subject":"ShadowRealm","reason":"enabled",' +
    '"source":{"file":"<shadow-realm>","line":null,"column":null}}');
end;

procedure TCapabilityAuditTests.TestSinkFailurePropagates;
var
  Source: TStringList;
  Executor: TGocciaInterpreterExecutor;
  Engine: TGocciaEngine;
  ErrorMessage: string;
  Raised: Boolean;
begin
  Source := TStringList.Create;
  Executor := TGocciaInterpreterExecutor.Create;
  Engine := TGocciaEngine.Create('audit-test.js', Source, Executor);
  try
    Engine.CapabilityAuditSink := FailingSink;
    FSinkInvocationCount := 0;
    ErrorMessage := '';
    Raised := False;
    try
      { The first delivery is the engine's capabilities.effective event,
        emitted ahead of the event being reported. }
      Engine.EmitCapabilityAudit(gckFFIOpen, gcdAllow, 'library',
        'enabled');
    except
      on E: EGocciaCapabilityAuditDeliveryError do
      begin
        Raised := True;
        ErrorMessage := E.Message;
      end;
    end;
    Expect<Integer>(FSinkInvocationCount).ToBe(1);
    Expect<Boolean>(Raised).ToBe(True);
    Expect<Boolean>(Pos('"layers"', ErrorMessage) > 0).ToBe(True);
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

procedure TCapabilityAuditTests.TestSandboxPromiseCannotCatchSinkFailure;
  procedure RunWithExecutor(const AExecutor: TGocciaExecutor);
  var
    Source: TStringList;
    Engine: TGocciaEngine;
    Runtime: TGocciaRuntimeCore;
    Context: TGocciaSandboxContext;
    Raised: Boolean;
  begin
    Source := TStringList.Create;
    Source.Text := 'import fs from "fs";' + sLineBreak +
      'try {' + sLineBreak +
      '  await fs.promises.readFile("../../missing.txt", "utf8");' +
      sLineBreak +
      '} catch (e) {}';
    Engine := TGocciaEngine.Create('/audit-promise-test.js', Source, AExecutor);
    Context := TGocciaSandboxContext.Create;
    try
      Engine.SourceType := stModule;
      Engine.CapabilityAuditSink := FailingSink;
      FSinkInvocationCount := 0;
      Runtime := AttachRuntime(Engine);
      Runtime.Install(TGocciaSandboxRuntimeExtension.Create(Context));

      Raised := False;
      try
        Engine.Execute;
      except
        on E: EGocciaCapabilityAuditDeliveryError do
          Raised := True;
      end;
      Expect<Integer>(FSinkInvocationCount).ToBe(1);
      Expect<Boolean>(Raised).ToBe(True);
    finally
      Engine.Free;
      Context.Free;
      AExecutor.Free;
      Source.Free;
    end;
  end;
begin
  RunWithExecutor(TGocciaInterpreterExecutor.Create);
  RunWithExecutor(TGocciaBytecodeExecutor.Create);
end;

procedure TCapabilityAuditTests.TestVMAsyncIteratorCannotCatchSinkFailure;
var
  Source: TStringList;
  Executor: TGocciaBytecodeExecutor;
  Engine: TGocciaEngine;
  Raised: Boolean;
begin
  Source := TStringList.Create;
  Source.Text := 'const iterable = {' + sLineBreak +
    '  [Symbol.iterator]: () => ({' + sLineBreak +
    '    next: () => {' + sLineBreak +
    '      Function("return 1");' + sLineBreak +
    '      return { done: true };' + sLineBreak +
    '    },' + sLineBreak +
    '  }),' + sLineBreak +
    '};' + sLineBreak +
    'try {' + sLineBreak +
    '  for await (const value of iterable) {}' + sLineBreak +
    '} catch (e) {}';
  Executor := TGocciaBytecodeExecutor.Create;
  Engine := TGocciaEngine.Create('/audit-async-iterator-test.js', Source,
    Executor);
  try
    Engine.SourceType := stModule;
    Engine.CapabilityAuditSink := FailingSink;
    FSinkInvocationCount := 0;

    Raised := False;
    try
      Engine.Execute;
    except
      on E: EGocciaCapabilityAuditDeliveryError do
        Raised := True;
    end;
    Expect<Integer>(FSinkInvocationCount).ToBe(1);
    Expect<Boolean>(Raised).ToBe(True);
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

procedure TCapabilityAuditTests.TestFailedRuntimeInstallRestoresRootClampCallback;
var
  Source: TStringList;
  Executor: TGocciaInterpreterExecutor;
  Engine: TGocciaEngine;
  Runtime: TGocciaRuntimeCore;
  Context: TGocciaSandboxContext;
  NormalizedPath: string;
  Raised: Boolean;
begin
  Source := TStringList.Create;
  Executor := TGocciaInterpreterExecutor.Create;
  Engine := TGocciaEngine.Create('audit-attach-test.js', Source, Executor);
  Context := TGocciaSandboxContext.Create;
  try
    Runtime := AttachRuntime(Engine);
    Context.Fs.RootClampCallback := RootClampSentinel;

    Raised := False;
    try
      Runtime.Install(TFailingRootClampRuntimeExtension.Create(Context));
    except
      on E: Exception do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);

    FRootClampCount := 0;
    NormalizedPath := Context.Fs.Normalize('../../probe', '/work');
    Expect<string>(NormalizedPath).ToBe('/probe');
    Expect<Integer>(FRootClampCount).ToBe(1);
  finally
    Engine.Free;
    Context.Free;
    Executor.Free;
    Source.Free;
  end;
end;

procedure TCapabilityAuditTests.TestSandboxDetachRestoresExistingModules;
var
  Source: TStringList;
  Executor: TGocciaInterpreterExecutor;
  Engine: TGocciaEngine;
  Runtime: TGocciaRuntimeCore;
  Context: TGocciaSandboxContext;
  Extension: TGocciaSandboxRuntimeExtension;
  ExistingFsModule: TGocciaModule;
  ExistingGocciaModule: TGocciaModule;
  CurrentModule: TGocciaModule;
begin
  Source := TStringList.Create;
  Executor := TGocciaInterpreterExecutor.Create;
  Engine := TGocciaEngine.Create('audit-detach-test.js', Source, Executor);
  Context := TGocciaSandboxContext.Create;
  ExistingFsModule := TGocciaModule.Create('existing-fs');
  ExistingGocciaModule := TGocciaModule.Create('existing-goccia');
  try
    Runtime := AttachRuntime(Engine);
    Engine.ModuleLoader.GlobalModules.Add('fs', ExistingFsModule);
    Engine.ModuleLoader.GlobalModules.Add('goccia', ExistingGocciaModule);

    Extension := TGocciaSandboxRuntimeExtension(Runtime.Install(
      TGocciaSandboxRuntimeExtension.Create(Context)));
    Extension.Detach;

    Expect<Boolean>(Engine.ModuleLoader.GlobalModules.TryGetValue(
      'fs', CurrentModule) and (CurrentModule = ExistingFsModule)).ToBe(True);
    Expect<Boolean>(Engine.ModuleLoader.GlobalModules.TryGetValue(
      'goccia', CurrentModule) and
      (CurrentModule = ExistingGocciaModule)).ToBe(True);
  finally
    Engine.ModuleLoader.GlobalModules.Remove('goccia');
    Engine.ModuleLoader.GlobalModules.Remove('fs');
    Engine.Free;
    ExistingGocciaModule.Free;
    ExistingFsModule.Free;
    Context.Free;
    Executor.Free;
    Source.Free;
  end;
end;

procedure TCapabilityAuditTests.TestEffectiveCapabilitiesEmittedOnce;
var
  Source: TStringList;
  Executor: TGocciaInterpreterExecutor;
  Engine: TGocciaEngine;
  Capabilities: TGocciaCapabilities;
begin
  FRecordedEvents := TStringList.Create;
  Source := TStringList.Create;
  Executor := TGocciaInterpreterExecutor.Create;
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, 'example.com');
  Engine := TGocciaEngine.Create('audit-effective.js', Source, Executor,
    Capabilities);
  try
    Engine.CapabilityAuditSink := RecordingSink;
    Engine.AuditEffectiveCapabilities;
    Engine.AuditEffectiveCapabilities;
    Expect<Integer>(FRecordedEvents.Count).ToBe(1);
    Expect<string>(FRecordedEvents[0]).ToBe('capabilities.effective|allow|' +
      Capabilities.ToJSON);
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
    FRecordedEvents.Free;
    FRecordedEvents := nil;
  end;
end;

procedure TCapabilityAuditTests.TestEffectiveCapabilitiesPrecedeFirstEvent;
var
  Source: TStringList;
  Executor: TGocciaInterpreterExecutor;
  Engine: TGocciaEngine;
begin
  FRecordedEvents := TStringList.Create;
  Source := TStringList.Create;
  Executor := TGocciaInterpreterExecutor.Create;
  Engine := TGocciaEngine.Create('audit-effective-first.js', Source, Executor);
  try
    Engine.CapabilityAuditSink := RecordingSink;
    Engine.EmitCapabilityAudit(gckFFIOpen, gcdDeny, 'lib', 'no grant');
    Engine.EmitCapabilityAudit(gckFFIOpen, gcdDeny, 'lib', 'no grant');
    Expect<Integer>(FRecordedEvents.Count).ToBe(3);
    Expect<string>(FRecordedEvents[0]).ToBe('capabilities.effective|allow|' +
      TGocciaCapabilities.None.ToJSON);
    Expect<string>(FRecordedEvents[1]).ToBe('ffi.open|deny|lib');
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
    FRecordedEvents.Free;
    FRecordedEvents := nil;
  end;
end;

procedure TCapabilityAuditTests.TestChildContextDoesNotRepeatEffectiveSet;
var
  Source, ChildSource: TStringList;
  Executor, ChildExecutor: TGocciaInterpreterExecutor;
  Engine, Child: TGocciaEngine;
begin
  FRecordedEvents := TStringList.Create;
  Source := TStringList.Create;
  ChildSource := TStringList.Create;
  Executor := TGocciaInterpreterExecutor.Create;
  ChildExecutor := TGocciaInterpreterExecutor.Create;
  Engine := TGocciaEngine.Create('audit-parent.js', Source, Executor);
  Child := nil;
  try
    Engine.CapabilityAuditSink := RecordingSink;
    Engine.AuditEffectiveCapabilities;
    Child := TGocciaEngine.Create('audit-child.js', ChildSource,
      ChildExecutor, Engine.Capabilities);
    Child.ConfigureCapabilityAuditAsChildOf(Engine);
    Child.AuditEffectiveCapabilities;
    Child.EmitCapabilityAudit(gckFFIOpen, gcdDeny, 'lib', 'no grant');
    Expect<Integer>(FRecordedEvents.Count).ToBe(2);
    Expect<string>(FRecordedEvents[1]).ToBe('ffi.open|deny|lib');
  finally
    Child.Free;
    Engine.Free;
    ChildExecutor.Free;
    Executor.Free;
    ChildSource.Free;
    Source.Free;
    FRecordedEvents.Free;
    FRecordedEvents := nil;
  end;
end;

{ Writes a package.json and index.js for "pkg" under <root>/node_modules and
  returns the path of <root>/app.js. }
function WriteNodeModulesProject(const ARoot: string): string;
var
  PackageDirectory: string;
begin
  PackageDirectory := IncludeTrailingPathDelimiter(ARoot) + 'node_modules' +
    PathDelim + 'pkg';
  ForceDirectories(PackageDirectory);
  with TStringList.Create do
  try
    Text := '{"name":"pkg","type":"module","exports":"./index.js"}';
    SaveToFile(PackageDirectory + PathDelim + 'package.json');
    Text := 'export const value = 42;';
    SaveToFile(PackageDirectory + PathDelim + 'index.js');
  finally
    Free;
  end;
  Result := IncludeTrailingPathDelimiter(ARoot) + 'app.js';
end;

procedure RemoveNodeModulesProject(const ARoot: string);
var
  PackageDirectory: string;
begin
  PackageDirectory := IncludeTrailingPathDelimiter(ARoot) + 'node_modules' +
    PathDelim + 'pkg';
  DeleteFile(PackageDirectory + PathDelim + 'package.json');
  DeleteFile(PackageDirectory + PathDelim + 'index.js');
  RemoveDir(PackageDirectory);
  RemoveDir(IncludeTrailingPathDelimiter(ARoot) + 'node_modules');
  RemoveDir(ARoot);
end;

function TestProjectRoot(const AName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'goccia-audit-' + AName + '-' + IntToStr(GetProcessID);
end;

procedure TCapabilityAuditTests.TestNodeModulesResolutionEmitsImportAudit;
var
  Root, EntryPath: string;
  Source: TStringList;
  Executor: TGocciaInterpreterExecutor;
  Engine: TGocciaEngine;
begin
  Root := TestProjectRoot('node-modules');
  EntryPath := WriteNodeModulesProject(Root);
  FRecordedEvents := TStringList.Create;
  Source := TStringList.Create;
  Source.Text := 'import { value } from "pkg"; value;';
  Executor := TGocciaInterpreterExecutor.Create;
  Engine := TGocciaEngine.Create(EntryPath, Source, Executor,
    TGocciaCapabilities.None.Allow(gcImport, IMPORT_NODE_MODULES_SCOPE));
  try
    Engine.SourceType := stModule;
    AttachRuntime(Engine);
    Engine.CapabilityAuditSink := RecordingSink;
    Engine.Execute;
    Expect<Boolean>(FRecordedEvents.IndexOf(
      'import.node-modules|allow|pkg') >= 0).ToBe(True);
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
    FRecordedEvents.Free;
    FRecordedEvents := nil;
    RemoveNodeModulesProject(Root);
  end;
end;

procedure TCapabilityAuditTests.TestUngrantedNodeModulesEmitsDeny;
var
  Root, EntryPath, ErrorMessage: string;
  Source: TStringList;
  Executor: TGocciaInterpreterExecutor;
  Engine: TGocciaEngine;
begin
  Root := TestProjectRoot('node-modules-denied');
  EntryPath := WriteNodeModulesProject(Root);
  FRecordedEvents := TStringList.Create;
  Source := TStringList.Create;
  Source.Text := 'import { value } from "pkg"; value;';
  Executor := TGocciaInterpreterExecutor.Create;
  Engine := TGocciaEngine.Create(EntryPath, Source, Executor);
  try
    Engine.SourceType := stModule;
    AttachRuntime(Engine);
    Engine.CapabilityAuditSink := RecordingSink;
    ErrorMessage := '';
    try
      Engine.Execute;
    except
      on E: Exception do
        ErrorMessage := E.Message;
    end;
    Expect<Boolean>(Pos('Cannot resolve bare module specifier "pkg"',
      ErrorMessage) > 0).ToBe(True);
    Expect<Boolean>(FRecordedEvents.IndexOf(
      'import.node-modules|deny|pkg') >= 0).ToBe(True);
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
    FRecordedEvents.Free;
    FRecordedEvents := nil;
    RemoveNodeModulesProject(Root);
  end;
end;

begin
  TestRunnerProgram.AddSuite(
    TCapabilityAuditTests.Create('Capability Audit'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
