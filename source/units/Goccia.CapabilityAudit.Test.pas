program Goccia.CapabilityAudit.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  SandboxVirtualFileSystem,
  TestingPascalLibrary,

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
    procedure FailingSink(const AEvent: TGocciaCapabilityAuditEvent);
    procedure RootClampSentinel(const APath, ABase,
      ACanonicalPath: string);
    procedure TestSerializesStructuredEvent;
    procedure TestSerializesMissingCoordinatesAsNull;
    procedure TestSinkFailurePropagates;
    procedure TestSandboxPromiseCannotCatchSinkFailure;
    procedure TestVMAsyncIteratorCannotCatchSinkFailure;
    procedure TestFailedRuntimeInstallRestoresRootClampCallback;
    procedure TestSandboxDetachRestoresExistingModules;
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

procedure TCapabilityAuditTests.RootClampSentinel(
  const APath, ABase, ACanonicalPath: string);
begin
  Inc(FRootClampCount);
end;

procedure TCapabilityAuditTests.TestSerializesStructuredEvent;
var
  AuditEvent: TGocciaCapabilityAuditEvent;
begin
  AuditEvent.Kind := gckFetchHost;
  AuditEvent.Decision := gcdDeny;
  AuditEvent.Subject := 'https://blocked.test/"quoted"';
  AuditEvent.Reason := 'host is not allowed';
  AuditEvent.Source.FilePath := 'app.js';
  AuditEvent.Source.Line := 7;
  AuditEvent.Source.Column := 3;

  Expect<string>(AuditEvent.ToJSON).ToBe(
    '{"schemaVersion":1,"kind":"fetch.host","decision":"deny",' +
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
    Expect<Boolean>(Pos('library', ErrorMessage) > 0).ToBe(True);
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
    Source.Text := 'import fs from "fs";' + LineEnding +
      'try {' + LineEnding +
      '  await fs.promises.readFile("../../missing.txt", "utf8");' +
      LineEnding +
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
  Source.Text := 'const iterable = {' + LineEnding +
    '  [Symbol.iterator]: () => ({' + LineEnding +
    '    next: () => {' + LineEnding +
    '      Function("return 1");' + LineEnding +
    '      return { done: true };' + LineEnding +
    '    },' + LineEnding +
    '  }),' + LineEnding +
    '};' + LineEnding +
    'try {' + LineEnding +
    '  for await (const value of iterable) {}' + LineEnding +
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

begin
  TestRunnerProgram.AddSuite(
    TCapabilityAuditTests.Create('Capability Audit'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
