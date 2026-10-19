program HTTPClientTest;

{$I Shared.inc}

uses
  {$IFDEF UNIX}cthreads, BaseUnix, Sockets,{$ENDIF}
  {$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}ctypes, OpenSSL,{$IFEND}

  Classes,
  SysUtils,

  HTTPClient,
  HTTPTypes,
  NetworkAddress,
  TestingPascalLibrary,
  TimingUtils;

type
  THTTPClientTests = class(TTestSuite)
  public
    procedure SetupTests; override;
    procedure TestCanonicalAuthorityParsing;
    procedure TestAuditAuthorityOmitsUserInfo;
    procedure TestConnectionDeadlineBoundsBlockingWork;
    {$IFDEF UNIX}
    procedure TestReadCutShortByDeadlineIsATimeout;
    procedure TestReadsAWellFormedChunkedBody;
    procedure TestChunkCutShortIsAnError;
    procedure TestMissingLastChunkIsAnError;
    procedure TestMalformedChunkedBodyIsAnError;
    procedure TestContentLengthCutShortIsAnError;
    {$ENDIF}
    {$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
    procedure TestSilentTLSPeerTimesOut;
    {$IFEND}
    procedure TestRejectsAmbiguousAuthority;
    procedure TestAcceptsValidIPv6Literals;
    procedure TestRejectsNonIPv6BracketedAuthority;
    procedure TestAcceptsValidSchemes;
    procedure TestRejectsMalformedScheme;
    procedure TestRejectsRequestTargetControls;
    procedure TestNormalizesRequestURL;
    procedure TestResolvesRedirectLocation;
    procedure TestClassifiesPrivateAddressRanges;
    procedure TestClassifiesPublicAddressesAsRoutable;
    procedure TestRejectsObfuscatedLoopbackForms;
    procedure TestResolvesLiteralAddressWithoutLookup;
    procedure TestResolvesLocalhostNamesToLoopback;
    procedure TestResolvesAHostNameToDottedQuad;
    {$IFDEF UNIX}
    procedure TestResolvesAHostsFileName;
    {$ENDIF}
    procedure TestDefaultPolicyPreservesHistoricalBehavior;
  end;

{$IFDEF UNIX}
var
  GHostsFileName: string;
  GHostsFileAddresses: TStringList;

{ A loopback name in /etc/hosts other than localhost (which the client
  answers itself), and every address the file lists for it. Debian and
  Ubuntu list the machine's own hostname under 127.0.1.1. }
function FindLoopbackHostsFileName(out AName: string;
  const AAddresses: TStringList): Boolean;
var
  Lines, Fields: TStringList;
  Address, Candidate, Line: string;
  CommentAt, I, J: Integer;
  Octets: array[0..3] of Byte;

  function IsLocalhost(const AValue: string): Boolean;
  begin
    Result := (AValue = 'localhost') or (AValue = 'localhost.') or
      (Pos('.localhost', AValue) > 0);
  end;

  function SplitLine(const AValue: string): Boolean;
  begin
    Fields.Clear;
    Line := AValue;
    CommentAt := Pos('#', Line);
    if CommentAt > 0 then
      Line := Copy(Line, 1, CommentAt - 1);
    Line := StringReplace(Trim(Line), #9, ' ', [rfReplaceAll]);
    Fields.DelimitedText := Line;
    Result := Fields.Count > 1;
  end;
begin
  Result := False;
  AName := '';
  if not FileExists('/etc/hosts') then
    Exit;
  Lines := TStringList.Create;
  Fields := TStringList.Create;
  try
    Fields.Delimiter := ' ';
    Fields.StrictDelimiter := False;
    Lines.LoadFromFile('/etc/hosts');
    for I := 0 to Lines.Count - 1 do
    begin
      if not SplitLine(Lines[I]) then
        Continue;
      Address := Fields[0];
      if not TryParseIPv4(Address, Octets) or (Octets[0] <> 127) then
        Continue;
      for J := 1 to Fields.Count - 1 do
      begin
        Candidate := LowerCase(Fields[J]);
        if (Candidate <> '') and not IsLocalhost(Candidate) then
        begin
          AName := Candidate;
          Break;
        end;
      end;
      if AName <> '' then
        Break;
    end;
    if AName = '' then
      Exit;
    for I := 0 to Lines.Count - 1 do
      if SplitLine(Lines[I]) then
        for J := 1 to Fields.Count - 1 do
          if LowerCase(Fields[J]) = AName then
            AAddresses.Add(Fields[0]);
    Result := True;
  finally
    Fields.Free;
    Lines.Free;
  end;
end;

{ A TCP listener on an ephemeral loopback port. }
function CreateLoopbackListener(out APort: Word): cint;
var
  Address: TInetSockAddr;
  AddressLength: TSockLen;
begin
  Result := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Result < 0 then
    raise Exception.Create('socket failed');
  FillChar(Address, SizeOf(Address), 0);
  Address.sin_family := AF_INET;
  Address.sin_port := 0;
  Address.sin_addr := StrToNetAddr('127.0.0.1');
  AddressLength := SizeOf(Address);
  if (fpBind(Result, @Address, SizeOf(Address)) <> 0) or
     (fpListen(Result, 1) <> 0) or
     (fpGetSockName(Result, @Address, @AddressLength) <> 0) then
  begin
    CloseSocket(Result);
    raise Exception.Create('loopback listener failed');
  end;
  APort := ntohs(Address.sin_port);
end;

{ Reads one request head from AConnection. }
procedure ReceiveRequestHead(const AConnection: cint);
var
  Buffer: array[0..4095] of Byte;
  Received: RawByteString;
  Count: ssize_t;
begin
  Received := '';
  repeat
    Count := fpRecv(AConnection, @Buffer[0], SizeOf(Buffer), 0);
    if Count <= 0 then
      Exit;
    SetLength(Received, Length(Received) + Count);
    Move(Buffer[0], Received[Length(Received) - Count + 1], Count);
  until Pos(#13#10#13#10, Received) > 0;
end;

type
  { Accepts one connection, reads its request, writes a fixed answer and
    closes the connection: a server that ends a response wherever the test
    says. }
  TCannedResponseServer = class(TThread)
  private
    FListener: cint;
    FResponse: RawByteString;
  protected
    procedure Execute; override;
  public
    Port: Word;
    constructor Create(const AResponse: RawByteString);
    destructor Destroy; override;
  end;

constructor TCannedResponseServer.Create(const AResponse: RawByteString);
begin
  FResponse := AResponse;
  FListener := CreateLoopbackListener(Port);
  inherited Create(False);
end;

destructor TCannedResponseServer.Destroy;
begin
  CloseSocket(FListener);
  inherited Destroy;
end;

procedure TCannedResponseServer.Execute;
var
  Connection: cint;
begin
  Connection := fpAccept(FListener, nil, nil);
  if Connection < 0 then
    Exit;
  try
    ReceiveRequestHead(Connection);
    if FResponse <> '' then
      fpSend(Connection, @FResponse[1], Length(FResponse),
        {$IFDEF LINUX}MSG_NOSIGNAL{$ELSE}0{$ENDIF});
  finally
    CloseSocket(Connection);
  end;
end;

{ Fetches http://127.0.0.1/ from a server that answers ARawResponse, and
  returns the body as text, or the client's error message after 'error: '. }
function FetchCanned(const ARawResponse: RawByteString): string;
var
  Body: RawByteString;
  Headers: THTTPHeaders;
  Response: THTTPResponse;
  Server: TCannedResponseServer;
begin
  SetLength(Headers, 0);
  Server := TCannedResponseServer.Create(ARawResponse);
  try
    try
      Response := HTTPGet(Format('http://127.0.0.1:%d/', [Server.Port]),
        Headers, nil, 5000);
      SetLength(Body, Length(Response.Body));
      if Length(Response.Body) > 0 then
        Move(Response.Body[0], Body[1], Length(Response.Body));
      Result := string(Body);
    except
      on E: EHTTPError do
        Result := 'error: ' + E.Message;
    end;
    Server.WaitFor;
  finally
    Server.Free;
  end;
end;

const
  CHUNKED_HEAD = 'HTTP/1.1 200 OK'#13#10 +
    'Transfer-Encoding: chunked'#13#10 +
    'Connection: close'#13#10#13#10;
  TRUNCATED_BODY = 'error: HTTP response body ended before its declared length';
  MALFORMED_CHUNKED_BODY = 'error: Invalid HTTP response: malformed chunked body';
{$ENDIF}

{$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
{ A self-signed certificate for the name localhost and its key, used only by
  the loopback TLS test below, which trusts it through SSL_CERT_FILE. }
const
  TEST_CERTIFICATE =
    '-----BEGIN CERTIFICATE-----'#10 +
    'MIIDQjCCAiqgAwIBAgIUI0gsQc8cnyWxwWtIsgACcbZoZT4wDQYJKoZIhvcNAQEL'#10 +
    'BQAwFDESMBAGA1UEAwwJbG9jYWxob3N0MCAXDTI2MTAxOTE2NTc1MVoYDzIxMjYw'#10 +
    'OTI1MTY1NzUxWjAUMRIwEAYDVQQDDAlsb2NhbGhvc3QwggEiMA0GCSqGSIb3DQEB'#10 +
    'AQUAA4IBDwAwggEKAoIBAQCuinMenuiQID5kibcniRxTBa+EtJLjCJIckBPihDOl'#10 +
    'hT81WiNU7aFApq67YbAyahuh/frqX6UVQIa/r86epWVGK5v2wghPHUZL3YNqKpnQ'#10 +
    'B5XhoHMlqSLhCMfEALAwfwSCk5e26viN7pviR5TK4gmpqNEctoeaWnNKEdgdbGML'#10 +
    'JJj7TOW0ouA+/HswaQSAqTkBCKOB3CBIaVifS2iXFGsvS1xS/msUWT9QeyJhqBHZ'#10 +
    'AoqKdlr/8bGmo8KcmRGZqVlC9pyBqVt9UpsoHnQyw+Z7YeroWmVLXcAsCB205nwv'#10 +
    'IkqJ1Xc9/v0GpnCeSGfao9PvptkaYa4uUKAgG8EMc0+XAgMBAAGjgYkwgYYwHQYD'#10 +
    'VR0OBBYEFOcKRG8bTau3gzzukPiPolHj6duGMB8GA1UdIwQYMBaAFOcKRG8bTau3'#10 +
    'gzzukPiPolHj6duGMBQGA1UdEQQNMAuCCWxvY2FsaG9zdDAMBgNVHRMBAf8EAjAA'#10 +
    'MAsGA1UdDwQEAwIFoDATBgNVHSUEDDAKBggrBgEFBQcDATANBgkqhkiG9w0BAQsF'#10 +
    'AAOCAQEADw+DTaDBzapIgViL02R4sXEWkjVSF17O+Oj9PJupS16Nc1CUaIxCPQJC'#10 +
    'qAyTdQ9SKxueyr93hBeLxTBOmE9YG6EaKSX0KYu45bvb4IC79Z6DRryrk4jbk+3h'#10 +
    '2uuoH8dO2l8PQ5Lqb09nSPBX+C2NqJq8OxJyHEppiVJxBSIxhlgi+8L1BU0NtqBI'#10 +
    '0OqDlFFP4vxJhXpPXPIg/YmcYTGZZPJriBMe4tg8FiKCJxpjs630t3BfAS5+uBE9'#10 +
    'AFgSWj0NyE/XWiQQJq+s37w9lx+u/+CLltYwR/nLcEixcwpSqEUGH3Hzqc0ZEwM5'#10 +
    'ANhrWTY2cjksKbkOn8qfrsLixzGjsw=='#10 +
    '-----END CERTIFICATE-----'#10;
  TEST_PRIVATE_KEY =
    '-----BEGIN PRIVATE KEY-----'#10 +
    'MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQCuinMenuiQID5k'#10 +
    'ibcniRxTBa+EtJLjCJIckBPihDOlhT81WiNU7aFApq67YbAyahuh/frqX6UVQIa/'#10 +
    'r86epWVGK5v2wghPHUZL3YNqKpnQB5XhoHMlqSLhCMfEALAwfwSCk5e26viN7pvi'#10 +
    'R5TK4gmpqNEctoeaWnNKEdgdbGMLJJj7TOW0ouA+/HswaQSAqTkBCKOB3CBIaVif'#10 +
    'S2iXFGsvS1xS/msUWT9QeyJhqBHZAoqKdlr/8bGmo8KcmRGZqVlC9pyBqVt9Upso'#10 +
    'HnQyw+Z7YeroWmVLXcAsCB205nwvIkqJ1Xc9/v0GpnCeSGfao9PvptkaYa4uUKAg'#10 +
    'G8EMc0+XAgMBAAECggEAOrdquM9ejzW5p5rIma0/cRKFlFAm5ed5N9CkT9yHf1RP'#10 +
    'CkSzhBuY1qX8PB9F5f/aIenOIlBgucCGVC/xg19CrZN4GU/2+Z5wio8PaYTBT8Ft'#10 +
    'g4S1l+LJUAjjIyXUH8xrGP5L2IY9bYnquPm5rXLTK1cWWeFxxtRM2oscN/HBc/qi'#10 +
    'Ckjw4G6xFINF2FYqFDmd4CQYeAtp/Q6uYbaQrJ+GbCeH+Kr84+51NFC4xUeLrtu8'#10 +
    'FW63wBHOJt52SeDFgwQhUN/mkfdin3TKTqPiQqylQRPOrRSauN9FZrbK34+JUkz+'#10 +
    'lKB7ZoD0oyNO8zbOudkCBK+A0wvbVFy58NCH59jwCQKBgQDnZZnSW3d+5Ky2/Eyw'#10 +
    'ZfJcD8voXyk7xPHiV0DEcDVnpAZtivpc4P+8wS2ZZnQv+tqoBWtU+K2FOBcOY8yg'#10 +
    'bN8UHz4fp5ex0+o3JqpQoRfJiMD0T8V7GUX4oBXUNozJ65LjjEq5yiI55akk5fFz'#10 +
    'XVdQv3nzpmmq0/2crRiQovMUYwKBgQDBGUgqhUlp1qTf/prAFloV5luHhUJRdZVD'#10 +
    'iMnJQpvdGhSRs2TpoAefrC6CDJTcyfJkHUXbcI2vCtt2nl070oOw8PDYVEf5ihwd'#10 +
    '+KxRGmNMPq8KEeVfpBtaSDbG3v0VfI7A4T5tt1g28/vR8ZBxzi0spptxqvQGpeFJ'#10 +
    'T0oycJL8PQKBgQDdwRqW450y+Eck3clIy4pl+6V4PaowY19DgEbLepVTi3e1hxjD'#10 +
    '5cRvydm/RtZWHNtrZjYkKmVwY/6EhdPWcj+e8woAIAZaHzI4RE+4EYnQU8RG3iPt'#10 +
    'Ap1x1UjWJKjr4VzVwN7kCfie758Xg13PJ0bEVZuziml9ksdbXO9LvT2i7QKBgHBg'#10 +
    'QjtcrkjF/dXN2kmHuI2eLg5EYz/86g0Pgj96YDLghq/0cvnzg/xMvBQ7Njb33/Sb'#10 +
    'hJgcfJgt/WKe7ytta+6Bieibw/dZhNt8aOwaK0eoAz4dBBwQ6Uo1VIuQIkFq57kg'#10 +
    'XLCFTvVMMHUR3Doe/vNJUpVoI/PqfZ2oY0nPcfsRAoGAOXkllTJbcmy27tINnzZY'#10 +
    'S0lszZf8BBMA0CDrYzleSFKoMqtpTwoNq4EudP5apb4sD0849xy7V+ZmtpMDayr/'#10 +
    'GwZo15b9OMd6kjX8hxX/qur4hLbRU58s3y1J1/E79o9zl7xPXA4fW3TnDjt7CcKy'#10 +
    'kX40r/v9DFO7lVURC0cx+bM='#10 +
    '-----END PRIVATE KEY-----'#10;

function Setenv(AName, AValue: PAnsiChar; AOverwrite: cint): cint; cdecl;
  external 'c';

type
  { Accepts one connection, completes a TLS handshake on it and then sends
    nothing until Release: a peer that goes silent after the handshake. }
  TSilentTLSServer = class(TThread)
  private
    FListener: cint;
    FCertificateFile: string;
    FKeyFile: string;
    FRelease: PRTLEvent;
  protected
    procedure Execute; override;
  public
    Port: Word;
    HandshakeCompleted: Boolean;
    Failure: string;
    constructor Create(const ACertificateFile, AKeyFile: string);
    destructor Destroy; override;
    procedure Release;
  end;

  { Runs one HTTPGet off the test thread, so a client that never returns
    fails the test instead of hanging it. }
  TClientRequest = class(TThread)
  private
    FURL: string;
    FTimeoutMilliseconds: Integer;
    FDone: PRTLEvent;
  protected
    procedure Execute; override;
  public
    ErrorMessage: string;
    constructor Create(const AURL: string;
      const ATimeoutMilliseconds: Integer);
    destructor Destroy; override;
    function WaitDone(const AMilliseconds: Cardinal): Boolean;
  end;

constructor TSilentTLSServer.Create(const ACertificateFile, AKeyFile: string);
begin
  FCertificateFile := ACertificateFile;
  FKeyFile := AKeyFile;
  FRelease := RTLEventCreate;
  FListener := CreateLoopbackListener(Port);
  inherited Create(False);
end;

destructor TSilentTLSServer.Destroy;
begin
  CloseSocket(FListener);
  RTLEventDestroy(FRelease);
  inherited Destroy;
end;

procedure TSilentTLSServer.Release;
begin
  RTLEventSetEvent(FRelease);
end;

procedure TSilentTLSServer.Execute;
const
  LOAD_WAIT_NANOSECONDS = Int64(10) * 1000 * 1000 * 1000;
var
  Connection: cint;
  Context: PSSL_CTX;
  Deadline: Int64;
  Session: PSSL;
begin
  Connection := fpAccept(FListener, nil, nil);
  if Connection < 0 then
    Exit;
  Context := nil;
  Session := nil;
  try
    { The client loads OpenSSL before it sends its hello, so the hello waits
      in the socket until the library this server shares is ready. }
    Deadline := GetNanoseconds + LOAD_WAIT_NANOSECONDS;
    while not IsSSLloaded and (GetNanoseconds < Deadline) do
      Sleep(5);
    if not IsSSLloaded then
    begin
      Failure := 'the client never loaded OpenSSL';
      Exit;
    end;
    { SslCtxUsePrivateKeyFile binds SSL_CTX_use_RSAPrivateKey_file, hence
      the RSA key. }
    Context := SslCtxNew(SslMethodTLSV1_2);
    if not Assigned(Context) or
       (SslCtxUseCertificateFile(Context, FCertificateFile,
         SSL_FILETYPE_PEM) <> 1) or
       (SslCtxUsePrivateKeyFile(Context, FKeyFile, SSL_FILETYPE_PEM) <> 1) then
    begin
      Failure := 'the server could not load its certificate and key';
      Exit;
    end;
    Session := SslNew(Context);
    SslSetFd(Session, Connection);
    HandshakeCompleted := SslAccept(Session) = 1;
    if not HandshakeCompleted then
      Failure := 'the TLS handshake failed';
    RTLEventWaitFor(FRelease, 60000);
  finally
    if Assigned(Session) then
      SslFree(Session);
    if Assigned(Context) then
      SslCtxFree(Context);
    CloseSocket(Connection);
  end;
end;

constructor TClientRequest.Create(const AURL: string;
  const ATimeoutMilliseconds: Integer);
begin
  FURL := AURL;
  FTimeoutMilliseconds := ATimeoutMilliseconds;
  FDone := RTLEventCreate;
  inherited Create(False);
end;

destructor TClientRequest.Destroy;
begin
  RTLEventDestroy(FDone);
  inherited Destroy;
end;

procedure TClientRequest.Execute;
var
  Headers: THTTPHeaders;
begin
  SetLength(Headers, 0);
  try
    try
      HTTPGet(FURL, Headers, nil, FTimeoutMilliseconds);
      ErrorMessage := '(no error)';
    except
      on E: Exception do
        ErrorMessage := E.Message;
    end;
  finally
    RTLEventSetEvent(FDone);
  end;
end;

function TClientRequest.WaitDone(const AMilliseconds: Cardinal): Boolean;
var
  Deadline: Int64;
begin
  Deadline := GetNanoseconds + Int64(AMilliseconds) * 1000000;
  repeat
    RTLEventWaitFor(FDone, 50);
    if Finished then
      Exit(True);
  until GetNanoseconds >= Deadline;
  Result := Finished;
end;
{$IFEND}

procedure THTTPClientTests.SetupTests;
begin
  Test('Canonical authority parsing', TestCanonicalAuthorityParsing);
  Test('Audit authority omits userinfo', TestAuditAuthorityOmitsUserInfo);
  Test('Connection deadline bounds blocking work',
    TestConnectionDeadlineBoundsBlockingWork);
  {$IFDEF UNIX}
  Test('A read cut short by the deadline is reported as a timeout',
    TestReadCutShortByDeadlineIsATimeout);
  Test('Reads a well-formed chunked body', TestReadsAWellFormedChunkedBody);
  Test('A chunk cut short by the connection closing is an error',
    TestChunkCutShortIsAnError);
  Test('A chunked body without its last chunk is an error',
    TestMissingLastChunkIsAnError);
  Test('A malformed chunked body is an error',
    TestMalformedChunkedBodyIsAnError);
  Test('A body shorter than its Content-Length is an error',
    TestContentLengthCutShortIsAnError);
  {$ENDIF}
  {$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
  Test('A TLS peer that goes silent after the handshake times out',
    TestSilentTLSPeerTimesOut);
  {$IFEND}
  Test('Rejects ambiguous authority', TestRejectsAmbiguousAuthority);
  Test('Accepts valid IPv6 literals', TestAcceptsValidIPv6Literals);
  Test('Rejects non-IPv6 bracketed authority',
    TestRejectsNonIPv6BracketedAuthority);
  Test('Accepts valid URI schemes', TestAcceptsValidSchemes);
  Test('Rejects malformed URI scheme', TestRejectsMalformedScheme);
  Test('Rejects request-target controls', TestRejectsRequestTargetControls);
  Test('Normalizes a request URL by the URL Standard',
    TestNormalizesRequestURL);
  Test('Resolves a redirect Location against the response URL',
    TestResolvesRedirectLocation);
  Test('Classifies private address ranges',
    TestClassifiesPrivateAddressRanges);
  Test('Classifies public addresses as routable',
    TestClassifiesPublicAddressesAsRoutable);
  Test('Rejects obfuscated loopback forms',
    TestRejectsObfuscatedLoopbackForms);
  Test('Resolves localhost names to loopback without a lookup',
    TestResolvesLocalhostNamesToLoopback);
  Test('Resolves a host name to a dotted-quad address',
    TestResolvesAHostNameToDottedQuad);
  Test('Resolves literal address without lookup',
    TestResolvesLiteralAddressWithoutLookup);
  {$IFDEF UNIX}
  if FindLoopbackHostsFileName(GHostsFileName, GHostsFileAddresses) then
    Test('Resolves a name only the hosts file lists',
      TestResolvesAHostsFileName)
  else
    Skip('Resolves a name only the hosts file lists',
      TestResolvesAHostsFileName,
      'no loopback name other than localhost in /etc/hosts');
  {$ENDIF}
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

{$IFDEF UNIX}
{ A listener that is never accepted: the kernel completes the connection and
  takes the request, and nothing ever answers. The read is then ended by the
  request's deadline, which used to be reported as a malformed response
  ("no header terminator"). Sockets are created here directly, so the test is
  compiled where that API is the same as the client's. }
procedure THTTPClientTests.TestReadCutShortByDeadlineIsATimeout;
const
  DEADLINE_MILLISECONDS = 150;
var
  Address: TInetSockAddr;
  AddressLength: TSockLen;
  ErrorMessage: string;
  Headers: THTTPHeaders;
  Listener: cint;
begin
  SetLength(Headers, 0);
  ErrorMessage := '';
  Listener := fpSocket(AF_INET, SOCK_STREAM, 0);
  Expect<Boolean>(Listener >= 0).ToBe(True);
  try
    FillChar(Address, SizeOf(Address), 0);
    Address.sin_family := AF_INET;
    Address.sin_port := 0;
    Address.sin_addr := StrToNetAddr('127.0.0.1');
    Expect<Integer>(fpBind(Listener, @Address, SizeOf(Address))).ToBe(0);
    Expect<Integer>(fpListen(Listener, 1)).ToBe(0);
    AddressLength := SizeOf(Address);
    Expect<Integer>(fpGetSockName(Listener, @Address,
      @AddressLength)).ToBe(0);
    try
      HTTPGet(Format('http://127.0.0.1:%d/', [ntohs(Address.sin_port)]),
        Headers, nil, DEADLINE_MILLISECONDS);
    except
      on E: EHTTPError do
        ErrorMessage := E.Message;
    end;
  finally
    CloseSocket(Listener);
  end;
  Expect<string>(ErrorMessage).ToBe('HTTP request timed out');
end;
{$ENDIF}

{$IFDEF UNIX}
procedure THTTPClientTests.TestReadsAWellFormedChunkedBody;
begin
  Expect<string>(FetchCanned(CHUNKED_HEAD +
    '5;name=value'#13#10'hello'#13#10 +
    '6'#13#10' world'#13#10 +
    '0'#13#10#13#10)).ToBe('hello world');
end;

{ The chunk declares 4,000 bytes and one arrives before the connection
  closes. The client used to copy the declared size out of a buffer holding
  only what arrived, returning its own heap memory as the body. }
procedure THTTPClientTests.TestChunkCutShortIsAnError;
begin
  Expect<string>(FetchCanned(CHUNKED_HEAD + 'fa0'#13#10'A')).ToBe(
    TRUNCATED_BODY);
  Expect<string>(FetchCanned(CHUNKED_HEAD + '4000'#13#10'A')).ToBe(
    TRUNCATED_BODY);
  Expect<string>(FetchCanned(CHUNKED_HEAD + '5'#13#10'hello')).ToBe(
    TRUNCATED_BODY);
end;

procedure THTTPClientTests.TestMissingLastChunkIsAnError;
begin
  Expect<string>(FetchCanned(CHUNKED_HEAD + '5'#13#10'hello'#13#10)).ToBe(
    TRUNCATED_BODY);
  Expect<string>(FetchCanned(CHUNKED_HEAD + '5'#13#10'hello'#13#10'6')).ToBe(
    TRUNCATED_BODY);
end;

procedure THTTPClientTests.TestMalformedChunkedBodyIsAnError;
begin
  Expect<string>(FetchCanned(CHUNKED_HEAD + 'zz'#13#10'hello'#13#10 +
    '0'#13#10#13#10)).ToBe(MALFORMED_CHUNKED_BODY);
  Expect<string>(FetchCanned(CHUNKED_HEAD + #13#10'0'#13#10#13#10)).ToBe(
    MALFORMED_CHUNKED_BODY);
  Expect<string>(FetchCanned(CHUNKED_HEAD + '-1'#13#10'0'#13#10#13#10)).ToBe(
    MALFORMED_CHUNKED_BODY);
  Expect<string>(FetchCanned(CHUNKED_HEAD + '3'#13#10'hello'#13#10 +
    '0'#13#10#13#10)).ToBe(MALFORMED_CHUNKED_BODY);
  Expect<string>(FetchCanned(CHUNKED_HEAD + 'ffffffffffff'#13#10'A')).ToBe(
    'error: HTTP response body exceeds 8388608 byte limit');
end;

{ The client used to return a body of the declared length padded with
  zeros. }
procedure THTTPClientTests.TestContentLengthCutShortIsAnError;
begin
  Expect<string>(FetchCanned('HTTP/1.1 200 OK'#13#10 +
    'Content-Length: 10'#13#10'Connection: close'#13#10#13#10'abc')).ToBe(
    TRUNCATED_BODY);
  Expect<string>(FetchCanned('HTTP/1.1 200 OK'#13#10 +
    'Content-Length: 3'#13#10'Connection: close'#13#10#13#10'abc')).ToBe(
    'abc');
end;
{$ENDIF}

{$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
{ The peer completes the handshake and then sends nothing. The socket's
  receive timeout ended each read, OpenSSL asked to be called again, and the
  client did, for ever: the request never returned. }
procedure THTTPClientTests.TestSilentTLSPeerTimesOut;
const
  DEADLINE_MILLISECONDS = 1000;
  WAIT_MILLISECONDS = 10000;
var
  CertificateFile, KeyFile: string;
  Client: TClientRequest;
  Finished: Boolean;
  Server: TSilentTLSServer;

  procedure WriteText(const AFileName: string; const AText: RawByteString);
  var
    Stream: TFileStream;
  begin
    Stream := TFileStream.Create(AFileName, fmCreate);
    try
      Stream.WriteBuffer(AText[1], Length(AText));
    finally
      Stream.Free;
    end;
  end;
begin
  CertificateFile := GetTempFileName('', 'goccia-tls-cert');
  KeyFile := GetTempFileName('', 'goccia-tls-key');
  WriteText(CertificateFile, TEST_CERTIFICATE);
  WriteText(KeyFile, TEST_PRIVATE_KEY);
  Setenv('SSL_CERT_FILE', PAnsiChar(AnsiString(CertificateFile)), 1);
  try
    Server := TSilentTLSServer.Create(CertificateFile, KeyFile);
    try
      Client := TClientRequest.Create(
        Format('https://localhost:%d/', [Server.Port]),
        DEADLINE_MILLISECONDS);
      try
        Finished := Client.WaitDone(WAIT_MILLISECONDS);
        Server.Release;
        Client.WaitFor;
        Server.WaitFor;
        Expect<string>(Server.Failure).ToBe('');
        Expect<Boolean>(Server.HandshakeCompleted).ToBe(True);
        Expect<Boolean>(Finished).ToBe(True);
        Expect<string>(Client.ErrorMessage).ToBe('HTTP request timed out');
      finally
        Client.Free;
      end;
    finally
      Server.Free;
    end;
  finally
    DeleteFile(CertificateFile);
    DeleteFile(KeyFile);
  end;
end;
{$IFEND}

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

procedure THTTPClientTests.TestNormalizesRequestURL;
begin
  Expect<string>(NormalizeHTTPRequestURL('HTTP://Example.COM/a/./b/../c?x=1#f'))
    .ToBe('http://example.com/a/c?x=1');
  Expect<string>(NormalizeHTTPRequestURL('http://h/a/%2e/%2E%2e/b'))
    .ToBe('http://h/b');
  // What the client's own checks decide stays as given.
  Expect<string>(NormalizeHTTPRequestURL('ftp://h/a/./b'))
    .ToBe('ftp://h/a/./b');
  Expect<string>(NormalizeHTTPRequestURL('http://h/a/./b' + #10 + 'x'))
    .ToBe('http://h/a/./b' + #10 + 'x');
  Expect<string>(NormalizeHTTPRequestURL('not a url')).ToBe('not a url');
  // Non-ASCII path and query code points are percent-encoded as UTF-8.
  Expect<string>(NormalizeHTTPRequestURL(
    'http://h/caf' + WideChar($00E9) + '/../x/' + WideChar($0100) +
    '?q=' + WideChar($00E9) + '#f')).ToBe('http://h/x/%C4%80?q=%C3%A9');
  // A non-ASCII host stays the client's own to decide.
  Expect<string>(NormalizeHTTPRequestURL('http://h' + WideChar($00E9) +
    '/a/./b')).ToBe('http://h' + WideChar($00E9) + '/a/./b');
end;

procedure THTTPClientTests.TestResolvesRedirectLocation;
const
  BASE = 'http://h:81/dir/page?q=1';
var
  Raised: Boolean;
begin
  Expect<string>(ResolveHTTPRedirectURL(BASE, 'sibling'))
    .ToBe('http://h:81/dir/sibling');
  Expect<string>(ResolveHTTPRedirectURL(BASE, './sibling'))
    .ToBe('http://h:81/dir/sibling');
  Expect<string>(ResolveHTTPRedirectURL(BASE, '../up'))
    .ToBe('http://h:81/up');
  Expect<string>(ResolveHTTPRedirectURL(BASE, '/abs/./p'))
    .ToBe('http://h:81/abs/p');
  Expect<string>(ResolveHTTPRedirectURL(BASE, '//other/x'))
    .ToBe('http://other/x');
  Expect<string>(ResolveHTTPRedirectURL(BASE, '?k=v'))
    .ToBe('http://h:81/dir/page?k=v');
  Expect<string>(ResolveHTTPRedirectURL(BASE, 'https://o/a/../b#f'))
    .ToBe('https://o/b');
  // A header value's bytes: UTF-8 for e-acute.
  Expect<string>(ResolveHTTPRedirectURL(BASE, 'caf' + Chr($C3) + Chr($A9)))
    .ToBe('http://h:81/dir/caf%C3%A9');
  Raised := False;
  try
    ResolveHTTPRedirectURL(BASE, 'http://');
  except
    on E: EHTTPError do
      Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
end;

{ The ranges the net capability refuses unless `private` or an explicit
  address names them (ADR 0122).
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
{ RFC 6761 section 6.3: `localhost` and every name under it are loopback,
  answered without asking the platform resolver, whose view of them differs
  per platform (and is absent for subdomains). }
procedure THTTPClientTests.TestResolvesLocalhostNamesToLoopback;
begin
  Expect<string>(ResolveHostToAddress('localhost')).ToBe('127.0.0.1');
  Expect<string>(ResolveHostToAddress('LOCALHOST.')).ToBe('127.0.0.1');
  Expect<string>(ResolveHostToAddress('app.localhost')).ToBe('127.0.0.1');
  Expect<Boolean>(IsPrivateNetworkAddress(
    ResolveHostToAddress('app.localhost'))).ToBe(True);
end;

{ A name the platform resolver answers comes back as dotted-quad text. The
  Windows lane formats the octets itself; this is what exercises it. }
procedure THTTPClientTests.TestResolvesAHostNameToDottedQuad;
var
  Address, Name: string;
  Dots, I: Integer;
begin
  {$IFDEF MSWINDOWS}
  Name := GetEnvironmentVariable('COMPUTERNAME');
  {$ELSE}
  Name := '';
  {$ENDIF}
  if Name = '' then
  begin
    Expect<Boolean>(True).ToBe(True);
    Exit;
  end;
  Address := ResolveHostToAddress(Name);
  Dots := 0;
  for I := 1 to Length(Address) do
    if Address[I] = '.' then
      Inc(Dots);
  Expect<Integer>(Dots).ToBe(3);
end;

{$IFDEF UNIX}
{ Resolution goes through the platform resolver, which consults the hosts
  file; FPC's NetDB DNS client did not, so this name failed to resolve
  (issue #1565). }
procedure THTTPClientTests.TestResolvesAHostsFileName;
begin
  Expect<Boolean>(GHostsFileAddresses.IndexOf(
    ResolveHostToAddress(GHostsFileName)) >= 0).ToBe(True);
end;
{$ENDIF}

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
  {$IFDEF UNIX}
  { The loopback servers close connections the client may still write to
    (a TLS close_notify); a test that regresses should fail, not end the
    process with SIGPIPE. }
  fpSignal(SIGPIPE, SignalHandler(SIG_IGN));
  GHostsFileAddresses := TStringList.Create;
  try
  {$ENDIF}
    TestRunnerProgram.AddSuite(THTTPClientTests.Create('HTTP client'));
    TestRunnerProgram.Run;
    ExitCode := TestResultToExitCode;
  {$IFDEF UNIX}
  finally
    GHostsFileAddresses.Free;
  end;
  {$ENDIF}
end.
