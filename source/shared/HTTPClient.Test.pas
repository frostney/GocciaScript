program HTTPClientTest;

{$I Shared.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}

  SysUtils,

  HTTPClient,
  HTTPTypes,
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
    procedure TestAcceptsValidIPv6Literals;
    procedure TestRejectsNonIPv6BracketedAuthority;
    procedure TestAcceptsValidSchemes;
    procedure TestRejectsMalformedScheme;
    procedure TestRejectsRequestTargetControls;
    procedure TestClassifiesPrivateAddressRanges;
    procedure TestClassifiesPublicAddressesAsRoutable;
    procedure TestRejectsObfuscatedLoopbackForms;
    procedure TestResolvesLiteralAddressWithoutLookup;
    procedure TestDefaultPolicyPreservesHistoricalBehavior;
  end;

procedure THTTPClientTests.SetupTests;
begin
  Test('Canonical authority parsing', TestCanonicalAuthorityParsing);
  Test('Audit authority omits userinfo', TestAuditAuthorityOmitsUserInfo);
  Test('Connection deadline bounds blocking work',
    TestConnectionDeadlineBoundsBlockingWork);
  Test('Rejects ambiguous authority', TestRejectsAmbiguousAuthority);
  Test('Accepts valid IPv6 literals', TestAcceptsValidIPv6Literals);
  Test('Rejects non-IPv6 bracketed authority',
    TestRejectsNonIPv6BracketedAuthority);
  Test('Accepts valid URI schemes', TestAcceptsValidSchemes);
  Test('Rejects malformed URI scheme', TestRejectsMalformedScheme);
  Test('Rejects request-target controls', TestRejectsRequestTargetControls);
  Test('Classifies private address ranges',
    TestClassifiesPrivateAddressRanges);
  Test('Classifies public addresses as routable',
    TestClassifiesPublicAddressesAsRoutable);
  Test('Rejects obfuscated loopback forms',
    TestRejectsObfuscatedLoopbackForms);
  Test('Resolves literal address without lookup',
    TestResolvesLiteralAddressWithoutLookup);
  Test('Default policy preserves historical behavior',
    TestDefaultPolicyPreservesHistoricalBehavior);
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
  WORKER_DRAIN_TIMEOUT_MILLISECONDS = 120000;
var
  ElapsedMilliseconds: Int64;
  Headers: THTTPHeaders;
  Raised: Boolean;
  StartNanoseconds: Int64;
  WorkersDrained: Boolean;
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
  WorkersDrained := WaitForHTTPConnectionWorkers(
    WORKER_DRAIN_TIMEOUT_MILLISECONDS);
  Expect<Boolean>(Raised).ToBe(True);
  Expect<Boolean>(
    ElapsedMilliseconds < MAX_TEST_DURATION_MILLISECONDS).ToBe(True);
  Expect<Boolean>(WorkersDrained).ToBe(True);
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

{ Valid IPv6 literals must survive bracket stripping unchanged — including a
  compressed loopback and a percent-encoded zone id (RFC 6874). Regression
  guard for the bracket-content plausibility check. }
procedure THTTPClientTests.TestAcceptsValidIPv6Literals;
begin
  Expect<string>(HTTPURLHost('http://[2001:db8::1]/')).ToBe('2001:db8::1');
  Expect<string>(HTTPURLHost('http://[::1]/')).ToBe('::1');
  Expect<string>(HTTPURLHost(
    'http://[fe80::1%25eth0]/')).ToBe('fe80::1%25eth0');
end;

{ A bracketed authority whose content is not a plausible IPv6 literal must
  raise, so a hostname like [allowed.example] can never masquerade as a valid
  host and slip past the invalid-URL reject+audit path. }
procedure THTTPClientTests.TestRejectsNonIPv6BracketedAuthority;

  function Rejects(const AURL: string): Boolean;
  begin
    Result := False;
    try
      HTTPURLHost(AURL);
    except
      on E: EHTTPError do
        Result := True;
    end;
  end;

begin
  Expect<Boolean>(Rejects('http://[allowed.example]/')).ToBe(True);
  Expect<Boolean>(Rejects('http://[example]/')).ToBe(True);
  Expect<Boolean>(Rejects('http://[]/')).ToBe(True);
  Expect<Boolean>(Rejects('http://[fe80::1%]/')).ToBe(True);
end;

{ A syntactically valid RFC 3986 scheme must parse even when it is not http(s):
  HTTPURLHost passes ARequireSupportedScheme=False, so a non-HTTP scheme like
  `file` or `custom+v1.0` canonicalizes to its host rather than being rejected. }
procedure THTTPClientTests.TestAcceptsValidSchemes;
begin
  Expect<string>(HTTPURLHost('http://allowed.example/')).ToBe('allowed.example');
  Expect<string>(HTTPURLHost(
    'https://allowed.example/')).ToBe('allowed.example');
  Expect<string>(HTTPURLHost('file://allowed.example/')).ToBe('allowed.example');
  Expect<string>(HTTPURLHost(
    'custom+v1.0://allowed.example/')).ToBe('allowed.example');
end;

{ A scheme that violates RFC 3986 syntax — empty, a non-letter first character,
  an illegal character, or absent entirely — must raise before host extraction,
  so a malformed URL can never leak a bare host past the fetch invalid-URL
  reject+audit path. }
procedure THTTPClientTests.TestRejectsMalformedScheme;

  function Rejects(const AURL: string): Boolean;
  begin
    Result := False;
    try
      HTTPURLHost(AURL);
    except
      on E: EHTTPError do
        Result := True;
    end;
  end;

begin
  Expect<Boolean>(Rejects('://allowed.example')).ToBe(True);
  Expect<Boolean>(Rejects('ht*tp://allowed.example')).ToBe(True);
  Expect<Boolean>(Rejects('1http://x')).ToBe(True);
  Expect<Boolean>(Rejects('allowed.example')).ToBe(True);
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

{ The ranges --fetch-deny-private-ranges exists to keep a script away from.
  169.254.169.254 is called out explicitly because the cloud
  instance-metadata endpoint is the payload SSRF is usually aiming at. }
procedure THTTPClientTests.TestClassifiesPrivateAddressRanges;
begin
  Expect<Boolean>(IsPrivateNetworkAddress('10.0.0.1')).ToBe(True);
  Expect<Boolean>(IsPrivateNetworkAddress('10.255.255.255')).ToBe(True);
  Expect<Boolean>(IsPrivateNetworkAddress('172.16.0.1')).ToBe(True);
  Expect<Boolean>(IsPrivateNetworkAddress('172.31.255.254')).ToBe(True);
  Expect<Boolean>(IsPrivateNetworkAddress('192.168.1.1')).ToBe(True);
  Expect<Boolean>(IsPrivateNetworkAddress('127.0.0.1')).ToBe(True);
  Expect<Boolean>(IsPrivateNetworkAddress('169.254.169.254')).ToBe(True);
  Expect<Boolean>(IsPrivateNetworkAddress('100.64.0.1')).ToBe(True);
  Expect<Boolean>(IsPrivateNetworkAddress('0.0.0.0')).ToBe(True);
  Expect<Boolean>(IsPrivateNetworkAddress('224.0.0.1')).ToBe(True);
  Expect<Boolean>(IsPrivateNetworkAddress('::1')).ToBe(True);
  Expect<Boolean>(IsPrivateNetworkAddress('fd00::1')).ToBe(True);
  Expect<Boolean>(IsPrivateNetworkAddress('fe80::1')).ToBe(True);
  Expect<Boolean>(IsPrivateNetworkAddress('[::1]')).ToBe(True);
end;

{ The boundaries matter as much as the ranges: 172.15 and 172.32 sit just
  outside 172.16/12, and 11.x / 192.169.x are the neighbours of 10/8 and
  192.168/16. An off-by-one here silently blocks legitimate traffic. }
procedure THTTPClientTests.TestClassifiesPublicAddressesAsRoutable;
begin
  Expect<Boolean>(IsPrivateNetworkAddress('8.8.8.8')).ToBe(False);
  Expect<Boolean>(IsPrivateNetworkAddress('1.1.1.1')).ToBe(False);
  Expect<Boolean>(IsPrivateNetworkAddress('172.15.255.255')).ToBe(False);
  Expect<Boolean>(IsPrivateNetworkAddress('172.32.0.1')).ToBe(False);
  Expect<Boolean>(IsPrivateNetworkAddress('11.0.0.1')).ToBe(False);
  Expect<Boolean>(IsPrivateNetworkAddress('192.169.0.1')).ToBe(False);
  Expect<Boolean>(IsPrivateNetworkAddress('100.63.255.255')).ToBe(False);
  Expect<Boolean>(IsPrivateNetworkAddress('100.128.0.1')).ToBe(False);
  Expect<Boolean>(IsPrivateNetworkAddress('2606:4700::1111')).ToBe(False);
end;

{ Shortened and hex IPv4 forms are the classic way to smuggle loopback past
  a textual filter. The classifier must not silently reinterpret them as
  routable — anything it cannot parse as four decimal octets is refused. }
procedure THTTPClientTests.TestRejectsObfuscatedLoopbackForms;
begin
  Expect<Boolean>(IsPrivateNetworkAddress('127.1')).ToBe(True);
  Expect<Boolean>(IsPrivateNetworkAddress('0x7f.0.0.1')).ToBe(True);
  Expect<Boolean>(IsPrivateNetworkAddress('2130706433')).ToBe(True);
  Expect<Boolean>(IsPrivateNetworkAddress('localhost')).ToBe(True);
  Expect<Boolean>(IsPrivateNetworkAddress('')).ToBe(True);
  Expect<Boolean>(IsPrivateNetworkAddress('999.1.1.1')).ToBe(True);
  Expect<Boolean>(IsPrivateNetworkAddress('1.2.3.4.5')).ToBe(True);
end;

{ A literal target must round-trip unchanged and perform no lookup, so
  pinning never introduces DNS traffic for a request that had none. }
procedure THTTPClientTests.TestResolvesLiteralAddressWithoutLookup;
begin
  Expect<string>(ResolveHostToAddress('93.184.216.34')).ToBe('93.184.216.34');
  Expect<string>(ResolveHostToAddress('127.0.0.1')).ToBe('127.0.0.1');
end;

{ A host that never sets a policy must see exactly the previous behavior:
  private ranges reachable, 8 MiB body ceiling. }
procedure THTTPClientTests.TestDefaultPolicyPreservesHistoricalBehavior;
var
  Policy: THTTPRequestPolicy;
begin
  Policy := DefaultHTTPPolicy;
  Expect<Boolean>(Policy.DenyPrivateRanges).ToBe(False);
  Expect<Integer>(Policy.MaxResponseBytes).ToBe(8 * 1024 * 1024);
end;

begin
  TestRunnerProgram.AddSuite(THTTPClientTests.Create('HTTP client'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
