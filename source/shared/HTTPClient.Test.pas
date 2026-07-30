program HTTPClientTest;

{$I Shared.inc}

uses
  SysUtils,

  HTTPClient,
  TestingPascalLibrary;

type
  THTTPClientTests = class(TTestSuite)
  public
    procedure SetupTests; override;
    procedure TestCanonicalAuthorityParsing;
    procedure TestRejectsAmbiguousAuthority;
    procedure TestRejectsRequestTargetControls;
  end;

procedure THTTPClientTests.SetupTests;
begin
  Test('Canonical authority parsing', TestCanonicalAuthorityParsing);
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
