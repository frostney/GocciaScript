program HTTPClientTest;

{$I Shared.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}

  SysUtils,

  HTTPClient,
  TestingPascalLibrary,
  TimingUtils;

type
  THTTPClientTests = class(TTestSuite)
  public
    procedure SetupTests; override;
    procedure TestCanonicalAuthorityParsing;
    procedure TestAuditAuthorityOmitsUserInfo;
    procedure TestConnectionDeadlineBoundsBlockingWork;
    procedure TestRejectsAmbiguousAuthority;
    procedure TestRejectsRequestTargetControls;
  end;

procedure THTTPClientTests.SetupTests;
begin
  Test('Canonical authority parsing', TestCanonicalAuthorityParsing);
  Test('Audit authority omits userinfo', TestAuditAuthorityOmitsUserInfo);
  Test('Connection deadline bounds blocking work',
    TestConnectionDeadlineBoundsBlockingWork);
  Test('Rejects ambiguous authority', TestRejectsAmbiguousAuthority);
  Test('Rejects request-target controls', TestRejectsRequestTargetControls);
end;

procedure THTTPClientTests.TestCanonicalAuthorityParsing;
begin
  Expect<string>(HTTPURLHost(
    'http://API.Example.COM:8080/path?q=1')).ToBe('api.example.com');
  Expect<string>(HTTPURLHost(
    'https://[2001:db8::1]/resource')).ToBe('2001:db8::1');
end;

procedure THTTPClientTests.TestAuditAuthorityOmitsUserInfo;
begin
  Expect<string>(HTTPURLAuditHost(
    'http://user:password@Blocked.Test/private?token=secret')).ToBe(
    'blocked.test');
end;

procedure THTTPClientTests.TestConnectionDeadlineBoundsBlockingWork;
const
  DEADLINE_MILLISECONDS = 25;
  MAX_TEST_DURATION_MILLISECONDS = 2000;
var
  ElapsedMilliseconds: Int64;
  Headers: THTTPHeaders;
  Raised: Boolean;
  StartNanoseconds: Int64;
begin
  SetLength(Headers, 0);
  Raised := False;
  StartNanoseconds := GetNanoseconds;
  try
    HTTPGet('http://203.0.113.1/', Headers, nil,
      DEADLINE_MILLISECONDS);
  except
    on E: EHTTPError do
      Raised := True;
  end;
  ElapsedMilliseconds := (GetNanoseconds - StartNanoseconds) div 1000000;
  Expect<Boolean>(Raised).ToBe(True);
  Expect<Boolean>(
    ElapsedMilliseconds < MAX_TEST_DURATION_MILLISECONDS).ToBe(True);
end;

procedure THTTPClientTests.TestRejectsAmbiguousAuthority;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    HTTPURLHost('http://allowed.example@127.0.0.1/');
  except
    on E: EHTTPError do
      Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
end;

procedure THTTPClientTests.TestRejectsRequestTargetControls;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    HTTPURLHost('http://example.com/path' + #13 + #10 + 'Injected: yes');
  except
    on E: EHTTPError do
      Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
end;

begin
  TestRunnerProgram.AddSuite(THTTPClientTests.Create('HTTP client'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
