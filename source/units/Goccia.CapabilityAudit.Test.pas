program Goccia.CapabilityAudit.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.CapabilityAudit,
  Goccia.Engine,
  Goccia.Executor.Interpreter,
  Goccia.TestSetup;

type
  ECapabilityAuditSinkFailure = class(Exception);

  TCapabilityAuditTests = class(TTestSuite)
  private
    procedure FailingSink(const AEvent: TGocciaCapabilityAuditEvent);
    procedure TestSerializesStructuredEvent;
    procedure TestSerializesMissingCoordinatesAsNull;
    procedure TestSinkFailurePropagates;
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
end;

procedure TCapabilityAuditTests.FailingSink(
  const AEvent: TGocciaCapabilityAuditEvent);
begin
  raise ECapabilityAuditSinkFailure.Create(AEvent.Subject);
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
  Raised: Boolean;
begin
  Source := TStringList.Create;
  Executor := TGocciaInterpreterExecutor.Create;
  Engine := TGocciaEngine.Create('audit-test.js', Source, Executor);
  try
    Engine.CapabilityAuditSink := FailingSink;
    Raised := False;
    try
      Engine.EmitCapabilityAudit(gckFFIOpen, gcdAllow, 'library',
        'enabled');
    except
      on E: ECapabilityAuditSinkFailure do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    Engine.Free;
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
