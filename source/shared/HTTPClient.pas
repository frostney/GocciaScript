unit HTTPClient;

// Minimal HTTP/1.1 client built on raw BSD sockets.
// Supports GET and HEAD over HTTP and HTTPS.
// Cross-platform: Unix (macOS, Linux) and Windows.
// Designed for synchronous use today with a path to non-blocking I/O later.

{$I Shared.inc}

interface

uses
  Classes,
  SysUtils,

  HTTPTypes;

type
  // Re-exported wire types — the definitions moved to HTTPTypes so
  // socket-free layers can import them without this unit's platform
  // closure; existing consumers keep naming them through here.
  THTTPHeader = HTTPTypes.THTTPHeader;
  THTTPHeaders = HTTPTypes.THTTPHeaders;
  THTTPResponse = HTTPTypes.THTTPResponse;
  EHTTPError = HTTPTypes.EHTTPError;
  EHTTPDestinationDenied = HTTPTypes.EHTTPDestinationDenied;
  THTTPHostCheck = HTTPTypes.THTTPHostCheck;
  THTTPRequestPolicy = HTTPTypes.THTTPRequestPolicy;

const
  DEFAULT_MAX_RESPONSE_BODY_BYTES = HTTPTypes.DEFAULT_MAX_RESPONSE_BODY_BYTES;

function HTTPGet(const AURL: string;
  const AHeaders: THTTPHeaders; const AAllowedHosts: TStrings = nil;
  const ATimeoutMilliseconds: Integer = 0): THTTPResponse; overload;
function HTTPGet(const AURL: string;
  const AHeaders: THTTPHeaders; const AAllowedHosts: TStrings;
  const ATimeoutMilliseconds: Integer;
  const APolicy: THTTPRequestPolicy): THTTPResponse; overload;
function HTTPHead(const AURL: string;
  const AHeaders: THTTPHeaders; const AAllowedHosts: TStrings = nil;
  const ATimeoutMilliseconds: Integer = 0): THTTPResponse; overload;
function HTTPHead(const AURL: string;
  const AHeaders: THTTPHeaders; const AAllowedHosts: TStrings;
  const ATimeoutMilliseconds: Integer;
  const APolicy: THTTPRequestPolicy): THTTPResponse; overload;

{ Exposed for testing: the address-classification and resolution steps that
  DoRequest performs between validating a destination and connecting to it. }
function IsPrivateNetworkAddress(const AAddressText: string): Boolean;
function ResolveHostToAddress(const AHost: string): string;
function HTTPURLHost(const AURL: string): string;
function HTTPURLAuditHost(const AURL: string): string;
function WaitForHTTPConnectionWorkers(
  const ATimeoutMilliseconds: Integer): Boolean;

{ The request URL the client sends (WHATWG Fetch, https://fetch.spec.whatwg.org/:
  a request's URL is what the URL Standard's parser returns): an absolute
  http(s) URL parsed and serialized without its fragment, so the scheme and
  host are lowercase and dot segments are removed (https://h/a/./b/../c is
  https://h/a/c). Anything the URL Standard's parser refuses, a URL with a
  control character and a scheme other than http and https come back as given,
  for the client's own checks to refuse or accept as before. Host parsing is
  the engine's (Goccia.URL.Parser). Every request and every redirect hop goes
  through it, and so does a request handed to an embedder's transport. }
function NormalizeHTTPRequestURL(const AURL: string): string;

{ A redirect's target (WHATWG Fetch, HTTP-redirect fetch): the Location value
  parsed by the URL Standard's parser with the redirecting response's URL as
  the base, so a relative, ./, ../, absolute-path, query-only and
  scheme-relative Location are resolved against that URL, then serialized
  without its fragment. Raises EHTTPError when the value does not parse (a
  network error). }
function ResolveHTTPRedirectURL(const ABaseURL, ALocation: string): string;

{ True when two absolute http(s) URLs have the same origin: the same scheme,
  host and port. The comparison the client makes for a redirect hop, which
  sends the caller's Authorization, Cookie and Proxy-Authorization only while
  every hop so far stayed on the request's origin. Exposed for testing. }
function IsSameHTTPOrigin(const AFromURL, AToURL: string): Boolean;

implementation

uses
  SyncObjs,
  Goccia.URL.Parser,

  {$IFDEF UNIX}
  Sockets, BaseUnix,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  WinSock2,
  {$ENDIF}
  CriticalSections,
  NetworkAddress,
  TextEncoding,
  TimingUtils,
  TransportSecurity;

const
  HTTP_CONNECTION_WORKER_STACK_SIZE = 1024 * 1024;
  MAX_HTTP_CONNECTION_WORKERS = 16;
  MAX_REDIRECTS = 20;
  HTTP_STATUS_MOVED_PERMANENTLY = 301;
  HTTP_STATUS_FOUND = 302;
  HTTP_STATUS_SEE_OTHER = 303;
  HTTP_STATUS_TEMPORARY_REDIRECT = 307;
  HTTP_STATUS_PERMANENT_REDIRECT = 308;
  REDIRECT_MODE_ERROR_MESSAGE =
    'redirect refused: the request''s redirect mode is "error"';
  REDIRECT_LIMIT_ERROR_MESSAGE =
    'redirect limit exceeded: more than %d redirects';
  MAX_RESPONSE_HEADER_BYTES = 64 * 1024;
  CRLF = #13#10;
  RECV_BUF_SIZE = 8192;

type
  // THTTPParsedURL and ParseHTTPURL now live in HTTPTypes (socket-free) so the
  // fetch builtin's host validation and the Lakon WASM lane can canonicalize a
  // URL without this unit's socket closure. Named here through the used unit.
  THTTPConnectionState = class
  private
    FAbandoned: Boolean;
    FCompleted: Boolean;
    FErrorMessage: string;
    FEvent: TEvent;
    FLock: TGocciaCriticalSection;
    FRefCount: Integer;
    FSocket: TSocket;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddRef;
    procedure Complete(const ASocket: TSocket; const AErrorMessage: string);
    procedure Release;
    function TakeResult(out ASocket: TSocket;
      out AErrorMessage: string): Boolean;
    function TryAbandon: Boolean;
    function WaitFor(const ATimeoutMilliseconds: Integer): TWaitResult;
  end;

  THTTPConnectionWorker = class(TThread)
  private
    FHost: string;
    FPort: Integer;
    FState: THTTPConnectionState;
    FTimeoutMilliseconds: Integer;
    FWorkerSlotAcquired: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(const AHost: string; const APort,
      ATimeoutMilliseconds: Integer; const AState: THTTPConnectionState);
    destructor Destroy; override;
  end;

var
  GHTTPConnectionWorkerCount: Integer = 0;

{$IFDEF MSWINDOWS}
type
  PAddrInfo = ^TAddrInfo;
  TAddrInfo = record
    ai_flags: LongInt;
    ai_family: LongInt;
    ai_socktype: LongInt;
    ai_protocol: LongInt;
    ai_addrlen: NativeUInt;
    ai_canonname: PAnsiChar;
    ai_addr: PSockAddr;
    ai_next: PAddrInfo;
  end;

function Getaddrinfo(ANodeName, AServName: PAnsiChar;
  AHints: PAddrInfo; out ARes: PAddrInfo): LongInt; stdcall;
  external 'ws2_32.dll' name 'getaddrinfo';
procedure Freeaddrinfo(AI: PAddrInfo); stdcall;
  external 'ws2_32.dll' name 'freeaddrinfo';

var
  GWinSockInitialized: Boolean = False;

procedure EnsureWinSockInit;
var
  WSAData: TWSAData;
begin
  if GWinSockInitialized then Exit;
  if WSAStartup($0202, WSAData) <> 0 then
    raise EHTTPError.Create('WSAStartup failed');
  GWinSockInitialized := True;
end;
{$ENDIF}

{$IFDEF UNIX}
{ The platform resolver: getaddrinfo(3) from libc, which the binaries already
  link through cthreads. It follows the system's resolver configuration
  (nsswitch, /etc/hosts, stub resolvers, IPv6 nameservers), as every other
  program on the host does. FPC's NetDB resolves names itself instead: it
  reads /etc/resolv.conf, queries only its IPv4 nameservers, and never
  consults /etc/hosts or nsswitch. getaddrinfo is thread-safe in glibc, musl
  and the BSD libcs, which matters because fetch workers resolve on their own
  threads. }
type
  PHostAddrInfo = ^THostAddrInfo;
  {$PUSH}
  {$PACKRECORDS C}
  THostAddrInfo = record
    ai_flags: cint;
    ai_family: cint;
    ai_socktype: cint;
    ai_protocol: cint;
    ai_addrlen: TSockLen;
    { glibc and musl put the address before the canonical name; Darwin and
      FreeBSD put the canonical name first. }
    {$IF DEFINED(LINUX)}
    ai_addr: PSockAddr;
    ai_canonname: PAnsiChar;
    {$ELSEIF DEFINED(DARWIN) OR DEFINED(FREEBSD)}
    ai_canonname: PAnsiChar;
    ai_addr: PSockAddr;
    {$ELSE}
      {$ERROR Declare this host's struct addrinfo field order in HTTPClient}
    {$IFEND}
    ai_next: PHostAddrInfo;
  end;
  {$POP}

function HostGetAddrInfo(ANodeName, AServName: PAnsiChar;
  AHints: PHostAddrInfo; out ARes: PHostAddrInfo): cint; cdecl;
  external 'c' name 'getaddrinfo';
procedure HostFreeAddrInfo(AInfo: PHostAddrInfo); cdecl;
  external 'c' name 'freeaddrinfo';
function HostGaiStrError(ACode: cint): PAnsiChar; cdecl;
  external 'c' name 'gai_strerror';

{ The first IPv4 address the platform resolver gives for AHost, as
  dotted-quad text. IPv4 only: the connect path opens AF_INET sockets. }
function ResolveIPv4WithSystemResolver(const AHost: string): string;
var
  Hints: THostAddrInfo;
  Res, Cur: PHostAddrInfo;
  HostBytes: TBytes;
  ErrorOffset: Integer;
  Status: cint;
begin
  if not TryEncodeASCIINullTerminated(AHost, HostBytes, ErrorOffset) then
    raise EHTTPError.CreateFmt(
      'HTTP host contains a non-ASCII code unit at offset %d',
      [ErrorOffset]);
  FillChar(Hints, SizeOf(Hints), 0);
  Hints.ai_family := AF_INET;
  Hints.ai_socktype := SOCK_STREAM;
  Hints.ai_protocol := IPPROTO_TCP;
  Res := nil;
  Status := HostGetAddrInfo(PAnsiChar(@HostBytes[0]), nil, @Hints, Res);
  if Status <> 0 then
    raise EHTTPError.CreateFmt('Failed to resolve host: %s (%s)',
      [AHost, string(AnsiString(HostGaiStrError(Status)))]);
  Result := '';
  try
    Cur := Res;
    while Assigned(Cur) do
    begin
      if (Cur^.ai_family = AF_INET) and Assigned(Cur^.ai_addr) and
         (Cur^.ai_addrlen >= SizeOf(TInetSockAddr)) then
      begin
        Result := NetAddrToStr(PInetSockAddr(Cur^.ai_addr)^.sin_addr);
        Break;
      end;
      Cur := Cur^.ai_next;
    end;
  finally
    if Assigned(Res) then
      HostFreeAddrInfo(Res);
  end;
  if Result = '' then
    raise EHTTPError.CreateFmt('Failed to resolve host: %s', [AHost]);
end;
{$ENDIF}

// ---------------------------------------------------------------------------
// URL parsing (ParseHTTPURL) lives in HTTPTypes so socket-free layers can use
// it; named here through the used unit.
// ---------------------------------------------------------------------------

// Re-exported from HTTPTypes so existing HTTPClient consumers keep naming
// these unchanged; the socket-free implementations live there.
function HTTPURLHost(const AURL: string): string;
begin
  Result := HTTPTypes.HTTPURLHost(AURL);
end;

function HTTPURLAuditHost(const AURL: string): string;
begin
  Result := HTTPTypes.HTTPURLAuditHost(AURL);
end;

// ---------------------------------------------------------------------------
// Socket connect (cross-platform)
// ---------------------------------------------------------------------------

{$IFDEF UNIX}
{ Darwin and the BSDs raise SIGPIPE for a write to a reset connection unless
  the socket says not to, and SecureTransport and OpenSSL write to the
  socket themselves. Linux has no such option; it sends with MSG_NOSIGNAL
  and runs OpenSSL with SIGPIPE blocked on the thread (TransportSecurity). }
{$IF DEFINED(DARWIN) OR DEFINED(FREEBSD)}
const
  {$IFDEF DARWIN}
  SO_NOSIGPIPE = $1022;
  {$ELSE}
  SO_NOSIGPIPE = $800;
  {$ENDIF}
{$IFEND}

procedure DisableSocketSigPipe(const ASocket: TSocket);
{$IF DEFINED(DARWIN) OR DEFINED(FREEBSD)}
var
  Enabled: Integer;
begin
  Enabled := 1;
  fpSetSockOpt(ASocket, SOL_SOCKET, SO_NOSIGPIPE, @Enabled, SizeOf(Enabled));
end;
{$ELSE}
begin
end;
{$IFEND}

procedure ConfigureSocketTimeout(const ASocket: TSocket;
  const ATimeoutMilliseconds: Integer);
var
  Timeout: TTimeVal;
begin
  if ATimeoutMilliseconds <= 0 then
    Exit;
  Timeout.tv_sec := ATimeoutMilliseconds div 1000;
  Timeout.tv_usec := (ATimeoutMilliseconds mod 1000) * 1000;
  fpSetSockOpt(ASocket, SOL_SOCKET, SO_RCVTIMEO, @Timeout,
    SizeOf(Timeout));
  fpSetSockOpt(ASocket, SOL_SOCKET, SO_SNDTIMEO, @Timeout,
    SizeOf(Timeout));
end;

function ConnectSocketBlocking(const AHost: string; const APort,
  ATimeoutMilliseconds: Integer): TSocket;
var
  SockAddr: TInetSockAddr;
  Octets: array[0..3] of Byte;
  Addr: in_addr;
  ConnectResult: Integer;
  OriginalFlags: Integer;
  PollDescriptor: TPollFD;
  SocketError: Integer;
  SocketErrorLength: TSockLen;
  NoDelay: Integer;
begin
  { DoRequest passes the literal ResolveHostToAddress pinned, so the
    numeric path is the one taken and nothing is resolved twice. A name
    goes to the same system resolver. }
  if not TryParseIPv4(AHost, Octets) and
     not TryParseIPv4(ResolveIPv4WithSystemResolver(AHost), Octets) then
    raise EHTTPError.CreateFmt('Failed to resolve host: %s', [AHost]);
  Addr.s_addr := htonl((Cardinal(Octets[0]) shl 24) or
    (Cardinal(Octets[1]) shl 16) or (Cardinal(Octets[2]) shl 8) or
    Cardinal(Octets[3]));

  Result := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Result < 0 then
    raise EHTTPError.Create('Failed to create socket');
  DisableSocketSigPipe(Result);
  { A request is written whole, right after the TLS handshake's last
    flight: Nagle would hold it until that flight is acknowledged, a
    delayed ACK on TLS 1.3. }
  NoDelay := 1;
  fpSetSockOpt(Result, IPPROTO_TCP, TCP_NODELAY, @NoDelay,
    SizeOf(NoDelay));

  FillChar(SockAddr, SizeOf(SockAddr), 0);
  SockAddr.sin_family := AF_INET;
  SockAddr.sin_port := htons(APort);
  SockAddr.sin_addr := Addr;

  try
    if ATimeoutMilliseconds > 0 then
    begin
      OriginalFlags := fpFcntl(Result, F_GETFL, 0);
      if (OriginalFlags < 0) or
         (fpFcntl(Result, F_SETFL, OriginalFlags or O_NONBLOCK) < 0) then
        raise EHTTPError.Create('Failed to configure non-blocking socket');
      try
        ConnectResult := fpConnect(Result, @SockAddr, SizeOf(SockAddr));
        if (ConnectResult <> 0) and (fpGetErrNo <> ESysEINPROGRESS) then
          raise EHTTPError.CreateFmt('Failed to connect to %s:%d',
            [AHost, APort]);
        if ConnectResult <> 0 then
        begin
          FillChar(PollDescriptor, SizeOf(PollDescriptor), 0);
          PollDescriptor.fd := Result;
          PollDescriptor.events := POLLOUT;
          if fpPoll(@PollDescriptor, 1, ATimeoutMilliseconds) <= 0 then
            raise EHTTPError.CreateFmt('Failed to connect to %s:%d',
              [AHost, APort]);
          SocketError := 0;
          SocketErrorLength := SizeOf(SocketError);
          if (fpGetSockOpt(Result, SOL_SOCKET, SO_ERROR, @SocketError,
              @SocketErrorLength) <> 0) or (SocketError <> 0) then
            raise EHTTPError.CreateFmt('Failed to connect to %s:%d',
              [AHost, APort]);
        end;
      finally
        fpFcntl(Result, F_SETFL, OriginalFlags);
      end;
    end
    else if fpConnect(Result, @SockAddr, SizeOf(SockAddr)) <> 0 then
      raise EHTTPError.CreateFmt('Failed to connect to %s:%d',
        [AHost, APort]);
    ConfigureSocketTimeout(Result, ATimeoutMilliseconds);
  except
    CloseSocket(Result);
    raise;
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure ConfigureSocketTimeout(const ASocket: TSocket;
  const ATimeoutMilliseconds: Integer);
var
  Timeout: LongInt;
begin
  if ATimeoutMilliseconds <= 0 then
    Exit;
  Timeout := ATimeoutMilliseconds;
  WinSock2.setsockopt(ASocket, SOL_SOCKET, SO_RCVTIMEO,
    PAnsiChar(@Timeout), SizeOf(Timeout));
  WinSock2.setsockopt(ASocket, SOL_SOCKET, SO_SNDTIMEO,
    PAnsiChar(@Timeout), SizeOf(Timeout));
end;

function ConnectSocketBlocking(const AHost: string; const APort,
  ATimeoutMilliseconds: Integer): TSocket;
var
  Hints, Res, Cur: PAddrInfo;
  HostBytes, PortBytes: TBytes;
  ErrorOffset: Integer;
  Sock: TSocket;
begin
  EnsureWinSockInit;

  FillChar(Hints, SizeOf(Hints), 0);
  New(Hints);
  try
    FillChar(Hints^, SizeOf(TAddrInfo), 0);
    Hints^.ai_family := AF_INET;
    Hints^.ai_socktype := SOCK_STREAM;
    Hints^.ai_protocol := IPPROTO_TCP;
    if not TryEncodeASCIINullTerminated(AHost, HostBytes,
      ErrorOffset) then
      raise EHTTPError.CreateFmt(
        'HTTP host contains a non-ASCII code unit at offset %d',
        [ErrorOffset]);
    if not TryEncodeASCIINullTerminated(IntToStr(APort), PortBytes,
      ErrorOffset) then
      raise EHTTPError.Create('HTTP port could not be encoded as ASCII');
    Res := nil;

    if Getaddrinfo(PAnsiChar(@HostBytes[0]), PAnsiChar(@PortBytes[0]),
                   Hints, Res) <> 0 then
      raise EHTTPError.CreateFmt('Failed to resolve host: %s', [AHost]);
  finally
    Dispose(Hints);
  end;

  try
    Cur := Res;
    Sock := INVALID_SOCKET;
    while Assigned(Cur) do
    begin
      Sock := WinSock2.socket(Cur^.ai_family, Cur^.ai_socktype,
                               Cur^.ai_protocol);
      if Sock = INVALID_SOCKET then
      begin
        Cur := Cur^.ai_next;
        Continue;
      end;
      ConfigureSocketTimeout(Sock, ATimeoutMilliseconds);

      if WinSock2.connect(Sock, Cur^.ai_addr^,
        Integer(Cur^.ai_addrlen)) = 0 then
        Break;

      WinSock2.closesocket(Sock);
      Sock := INVALID_SOCKET;
      Cur := Cur^.ai_next;
    end;

    if Sock = INVALID_SOCKET then
      raise EHTTPError.CreateFmt('Failed to connect to %s:%d', [AHost, APort]);

    Result := Sock;
  finally
    Freeaddrinfo(Res);
  end;
end;
{$ENDIF}

// ---------------------------------------------------------------------------
// Platform-neutral socket I/O wrappers
// ---------------------------------------------------------------------------

function SocketSend(const ASock: TSocket; const ABuf: Pointer;
  const ALen: Integer): Integer; {$IFDEF FPC}inline;{$ENDIF}
begin
  {$IFDEF UNIX}
  { MSG_NOSIGNAL (Linux) makes a send to a peer that reset the connection
    fail with EPIPE instead of ending the process with SIGPIPE; Darwin and
    the BSDs get SO_NOSIGPIPE on the socket in ConnectSocketBlocking. }
  {$IFDEF LINUX}
  Result := fpSend(ASock, ABuf, ALen, MSG_NOSIGNAL);
  {$ELSE}
  Result := fpSend(ASock, ABuf, ALen, 0);
  {$ENDIF}
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Result := WinSock2.send(ASock, ABuf^, ALen, 0);
  {$ENDIF}
end;

function SocketRecv(const ASock: TSocket; const ABuf: Pointer;
  const ALen: Integer): Integer; {$IFDEF FPC}inline;{$ENDIF}
begin
  {$IFDEF UNIX}
  Result := fpRecv(ASock, ABuf, ALen, 0);
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Result := WinSock2.recv(ASock, ABuf^, ALen, 0);
  {$ENDIF}
end;

type
  TSocketReadFailure = (srfOther, srfInterrupted, srfTimedOut);

{ Why the SocketRecv that just returned a negative count failed. Call it
  before anything else can replace the thread's last socket error. }
function LastSocketReadFailure: TSocketReadFailure;
begin
  Result := srfOther;
  {$IFDEF UNIX}
  case fpGetErrNo of
    ESysEINTR:
      Result := srfInterrupted;
    ESysEAGAIN:
      Result := srfTimedOut;
  end;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  case WSAGetLastError of
    WSAEINTR:
      Result := srfInterrupted;
    WSAETIMEDOUT:
      Result := srfTimedOut;
  end;
  {$ENDIF}
end;

procedure SocketClose(const ASock: TSocket); {$IFDEF FPC}inline;{$ENDIF}
begin
  {$IFDEF UNIX}
  CloseSocket(ASock);
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  WinSock2.closesocket(ASock);
  {$ENDIF}
end;

function InvalidHTTPSocket: TSocket; {$IFDEF FPC}inline;{$ENDIF}
begin
  {$IFDEF UNIX}
  Result := -1;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Result := INVALID_SOCKET;
  {$ENDIF}
end;

function IsValidHTTPSocket(const ASocket: TSocket): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  {$IFDEF UNIX}
  Result := ASocket >= 0;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Result := ASocket <> INVALID_SOCKET;
  {$ENDIF}
end;

function TryAcquireHTTPConnectionWorker: Boolean;
begin
  Result := AtomicIncrementInt32(GHTTPConnectionWorkerCount) <=
    MAX_HTTP_CONNECTION_WORKERS;
  if not Result then
    AtomicDecrementInt32(GHTTPConnectionWorkerCount);
end;

procedure ReleaseHTTPConnectionWorker;
begin
  AtomicDecrementInt32(GHTTPConnectionWorkerCount);
end;

function WaitForHTTPConnectionWorkers(
  const ATimeoutMilliseconds: Integer): Boolean;
var
  DeadlineNanoseconds: Int64;
begin
  DeadlineNanoseconds := GetNanoseconds +
    Int64(ATimeoutMilliseconds) * 1000000;
  repeat
    ReadMemoryBarrier;
    if GHTTPConnectionWorkerCount = 0 then
      Exit(True);
    Sleep(1);
  until GetNanoseconds >= DeadlineNanoseconds;
  ReadMemoryBarrier;
  Result := GHTTPConnectionWorkerCount = 0;
end;

{ THTTPConnectionState }

constructor THTTPConnectionState.Create;
begin
  inherited;
  FSocket := InvalidHTTPSocket;
  CriticalSectionInit(FLock);
  FEvent := TEvent.Create(nil, True, False, '');
  FRefCount := 1;
end;

destructor THTTPConnectionState.Destroy;
begin
  if IsValidHTTPSocket(FSocket) then
    SocketClose(FSocket);
  FEvent.Free;
  CriticalSectionDone(FLock);
  inherited;
end;

procedure THTTPConnectionState.AddRef;
begin
  AtomicIncrementInt32(FRefCount);
end;

procedure THTTPConnectionState.Release;
begin
  if AtomicDecrementInt32(FRefCount) = 0 then
    Free;
end;

procedure THTTPConnectionState.Complete(const ASocket: TSocket;
  const AErrorMessage: string);
var
  CloseSocketAfterCompletion: Boolean;
begin
  CloseSocketAfterCompletion := False;
  CriticalSectionEnter(FLock);
  try
    FCompleted := True;
    if FAbandoned then
      CloseSocketAfterCompletion := IsValidHTTPSocket(ASocket)
    else
    begin
      FSocket := ASocket;
      FErrorMessage := AErrorMessage;
    end;
  finally
    CriticalSectionLeave(FLock);
  end;
  if CloseSocketAfterCompletion then
    SocketClose(ASocket);
  FEvent.SetEvent;
end;

function THTTPConnectionState.TakeResult(out ASocket: TSocket;
  out AErrorMessage: string): Boolean;
begin
  ASocket := InvalidHTTPSocket;
  AErrorMessage := '';
  CriticalSectionEnter(FLock);
  try
    Result := FCompleted and not FAbandoned;
    if Result then
    begin
      ASocket := FSocket;
      FSocket := InvalidHTTPSocket;
      AErrorMessage := FErrorMessage;
    end;
  finally
    CriticalSectionLeave(FLock);
  end;
end;

function THTTPConnectionState.TryAbandon: Boolean;
begin
  CriticalSectionEnter(FLock);
  try
    Result := not FCompleted;
    if Result then
      FAbandoned := True;
  finally
    CriticalSectionLeave(FLock);
  end;
end;

function THTTPConnectionState.WaitFor(
  const ATimeoutMilliseconds: Integer): TWaitResult;
begin
  Result := FEvent.WaitFor(ATimeoutMilliseconds);
end;

{ THTTPConnectionWorker }

constructor THTTPConnectionWorker.Create(const AHost: string; const APort,
  ATimeoutMilliseconds: Integer; const AState: THTTPConnectionState);
begin
  {$IFDEF FPC}
  inherited Create(True, HTTP_CONNECTION_WORKER_STACK_SIZE);
  {$ELSE}
  inherited Create(True);
  {$ENDIF}
  FreeOnTerminate := True;
  if not TryAcquireHTTPConnectionWorker then
    raise EHTTPError.Create('HTTP connection worker limit exceeded');
  FWorkerSlotAcquired := True;
  FHost := AHost;
  FPort := APort;
  FTimeoutMilliseconds := ATimeoutMilliseconds;
  FState := AState;
  FState.AddRef;
end;

destructor THTTPConnectionWorker.Destroy;
begin
  if Assigned(FState) then
    FState.Release;
  if FWorkerSlotAcquired then
    ReleaseHTTPConnectionWorker;
  inherited;
end;

procedure THTTPConnectionWorker.Execute;
var
  ErrorMessage: string;
  Socket: TSocket;
begin
  ErrorMessage := '';
  Socket := InvalidHTTPSocket;
  try
    try
      Socket := ConnectSocketBlocking(FHost, FPort, FTimeoutMilliseconds);
    except
      on E: Exception do
        ErrorMessage := E.Message;
    end;
    FState.Complete(Socket, ErrorMessage);
  except
    if IsValidHTTPSocket(Socket) then
      SocketClose(Socket);
  end;
end;

function ConnectSocket(const AHost: string; const APort,
  ATimeoutMilliseconds: Integer): TSocket;
var
  ErrorMessage: string;
  State: THTTPConnectionState;
  WaitResult: TWaitResult;
  Worker: THTTPConnectionWorker;
begin
  if ATimeoutMilliseconds <= 0 then
    Exit(ConnectSocketBlocking(AHost, APort, ATimeoutMilliseconds));

  State := THTTPConnectionState.Create;
  Worker := nil;
  try
    Worker := THTTPConnectionWorker.Create(AHost, APort,
      ATimeoutMilliseconds, State);
    Worker.Start;
    Worker := nil;

    WaitResult := State.WaitFor(ATimeoutMilliseconds);
    if WaitResult = wrTimeout then
    begin
      if State.TryAbandon then
        raise EHTTPError.Create('HTTP request timed out');
    end
    else if WaitResult <> wrSignaled then
      raise EHTTPError.Create('HTTP connection wait failed');

    if not State.TakeResult(Result, ErrorMessage) then
      raise EHTTPError.Create('HTTP request timed out');
    if ErrorMessage <> '' then
      raise EHTTPError.Create(ErrorMessage);
    if not IsValidHTTPSocket(Result) then
      raise EHTTPError.Create('HTTP connection failed');
  finally
    Worker.Free;
    State.Release;
  end;
end;

// ---------------------------------------------------------------------------
// Send / Receive wrappers (unified TLS + plain)
// ---------------------------------------------------------------------------

procedure SendAll(const ASock: TSocket;
  var ATransport: TTransportSecurityConnection;
  const AData: TBytes);
var
  Sent, Total, Len, N: Integer;
begin
  Total := Length(AData);
  Sent := 0;
  while Sent < Total do
  begin
    Len := Total - Sent;
    if ATransport.Active then
      N := TransportSecurityWrite(ATransport, @AData[Sent], Len)
    else
      N := SocketSend(ASock, @AData[Sent], Len);
    if N <= 0 then
      raise EHTTPError.Create('Send failed');
    Inc(Sent, N);
  end;
end;

procedure AppendBytes(var ADestination: TBytes; const ASource: Pointer;
  const ALength: Integer; const AMaxLength: Integer);
var
  DestinationLength: Integer;
begin
  if ALength <= 0 then
    Exit;
  DestinationLength := Length(ADestination);
  if (DestinationLength > AMaxLength - ALength) then
    raise EHTTPError.CreateFmt('HTTP response exceeds %d byte limit',
      [AMaxLength]);
  SetLength(ADestination, DestinationLength + ALength);
  Move(ASource^, ADestination[DestinationLength], ALength);
end;

function FindCRLF(const ABytes: TBytes): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(ABytes) - 2 do
    if (ABytes[I] = 13) and (ABytes[I + 1] = 10) then
      Exit(I);
  Result := -1;
end;

function FindHeaderTerminator(const ABytes: TBytes): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(ABytes) - 4 do
    if (ABytes[I] = 13) and (ABytes[I + 1] = 10) and
       (ABytes[I + 2] = 13) and (ABytes[I + 3] = 10) then
      Exit(I);
  Result := -1;
end;

function IsomorphicDecode(const ABytes: TBytes; const AOffset,
  ALength: Integer): string;
var
  I: Integer;
begin
  if ALength <= 0 then
    Exit('');
  SetLength(Result, ALength);
  for I := 0 to ALength - 1 do
    Result[I + 1] := Char(ABytes[AOffset + I]);
end;

procedure RemoveLeadingBytes(var ABytes: TBytes; const ACount: Integer);
var
  Remaining: Integer;
begin
  if ACount <= 0 then
    Exit;
  if ACount >= Length(ABytes) then
  begin
    SetLength(ABytes, 0);
    Exit;
  end;
  Remaining := Length(ABytes) - ACount;
  Move(ABytes[ACount], ABytes[0], Remaining);
  SetLength(ABytes, Remaining);
end;

function RecvBytes(const ASock: TSocket;
  var ATransport: TTransportSecurityConnection;
  var ABuf: array of Byte; const ALen: Integer): Integer;
begin
  if ATransport.Active then
    Result := TransportSecurityRead(ATransport, ABuf, ALen)
  else
    Result := SocketRecv(ASock, @ABuf[0], ALen);
end;

// ---------------------------------------------------------------------------
// HTTP response parsing
// ---------------------------------------------------------------------------

type
  TRawHTTPResponse = record
    StatusCode: Integer;
    StatusText: string;
    Headers: THTTPHeaders;
    Body: TBytes;
    { The message ended where its framing said and nothing followed it, on
      an HTTP/1.1 connection the server did not close: the connection may
      carry the next request. }
    Reusable: Boolean;
  end;

  { On a reused connection: no byte of a response arrived before it ended,
    or what arrived is no status line. The server closed it, or the
    previous response left bytes behind. }
  EHTTPNoResponse = class(EHTTPError);

function FindHeaderValue(const AHeaders: THTTPHeaders;
  const AName: string): string;
var
  I: Integer;
  Lower: string;
begin
  Result := '';
  Lower := LowerCase(AName);
  for I := 0 to High(AHeaders) do
    if AHeaders[I].Name = Lower then
    begin
      Result := AHeaders[I].Value;
      Exit;
    end;
end;

const
  TRUNCATED_BODY_ERROR = 'HTTP response body ended before its declared length';
  MALFORMED_CHUNK_ERROR = 'Invalid HTTP response: malformed chunked body';

const
  { What a size larger than any Integer body limit parses to. }
  OVERSIZED_LENGTH = Int64(High(Integer)) + 1;

{ A chunk-size line's size: one or more hexadecimal digits and nothing else.
  A size larger than any Integer parses to OVERSIZED_LENGTH, which every body
  limit refuses. }
function TryParseChunkSize(const AText: string; out ASize: Int64): Boolean;
var
  Digit, I: Integer;
begin
  ASize := 0;
  Result := AText <> '';
  for I := 1 to Length(AText) do
  begin
    case AText[I] of
      '0'..'9': Digit := Ord(AText[I]) - Ord('0');
      'a'..'f': Digit := Ord(AText[I]) - Ord('a') + 10;
      'A'..'F': Digit := Ord(AText[I]) - Ord('A') + 10;
    else
      Exit(False);
    end;
    if ASize < OVERSIZED_LENGTH then
      ASize := ASize * 16 + Digit;
    if ASize > OVERSIZED_LENGTH then
      ASize := OVERSIZED_LENGTH;
  end;
end;

{ A Content-Length value: decimal digits and nothing else. A value larger
  than any Integer parses to OVERSIZED_LENGTH, which every body limit
  refuses. }
function TryParseContentLength(const AText: string;
  out ALength: Int64): Boolean;
var
  I: Integer;
begin
  ALength := 0;
  Result := AText <> '';
  for I := 1 to Length(AText) do
  begin
    if not (AText[I] in ['0'..'9']) then
      Exit(False);
    if ALength < OVERSIZED_LENGTH then
      ALength := ALength * 10 + Ord(AText[I]) - Ord('0');
    if ALength > OVERSIZED_LENGTH then
      ALength := OVERSIZED_LENGTH;
  end;
end;

{ A trailer field line (RFC 9112 section 7.1.2, a field line as in section
  5): a non-empty token, a colon, and a value without CR, LF or NUL. }
function IsFieldLine(const ALine: string): Boolean;
var
  ColonAt, I: Integer;
begin
  ColonAt := Pos(':', ALine);
  if ColonAt <= 1 then
    Exit(False);
  for I := 1 to ColonAt - 1 do
    if not (ALine[I] in ['!', '#'..'''', '*', '+', '-', '.', '0'..'9',
      'A'..'Z', '^'..'z', '|', '~']) then
      Exit(False);
  for I := ColonAt + 1 to Length(ALine) do
    if (ALine[I] = #0) or (ALine[I] = #10) or (ALine[I] = #13) then
      Exit(False);
  Result := True;
end;

function HasHeader(const AHeaders: THTTPHeaders; const AName: string): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AHeaders) do
    if AHeaders[I].Name = AName then
      Exit(True);
  Result := False;
end;

{ The last coding a Transfer-Encoding value lists, lowercased, such as
  'chunked' for 'gzip, chunked'. }
function FinalTransferCoding(const AValue: string): string;
var
  I: Integer;
begin
  Result := AValue;
  for I := Length(AValue) downto 1 do
    if AValue[I] = ',' then
    begin
      Result := Copy(AValue, I + 1, MaxInt);
      Break;
    end;
  Result := LowerCase(Trim(Result));
end;

{ WHATWG Fetch: a redirect status is 301, 302, 303, 307 or 308. }
function IsRedirectStatus(const AStatus: Integer): Boolean;
begin
  Result := (AStatus = HTTP_STATUS_MOVED_PERMANENTLY) or
    (AStatus = HTTP_STATUS_FOUND) or (AStatus = HTTP_STATUS_SEE_OTHER) or
    (AStatus = HTTP_STATUS_TEMPORARY_REDIRECT) or
    (AStatus = HTTP_STATUS_PERMANENT_REDIRECT);
end;

function ReadResponse(const ASock: TSocket;
  var ATransport: TTransportSecurityConnection;
  const AIsHead: Boolean; const ADeadlineNs: Int64;
  const AMaxBodyBytes: Integer;
  const ASkipRedirectBody: Boolean;
  const AReused: Boolean = False): TRawHTTPResponse;
var
  Buf: array[0..RECV_BUF_SIZE - 1] of Byte;
  RawHeader: TBytes;
  N, HeaderEnd, I, J, ContentLen, ChunkSize: Integer;
  DeclaredLength, FieldLength: Int64;
  TrailerBytes: Integer;
  Line, HeaderBlock: string;
  Lines: array of string;
  ColonPos: Integer;
  TransferEncoding: string;
  BodyBytes: TBytes;
  BodyLen: Integer;
  ChunkBuf: TBytes;
  ChunkBufLimit: Integer;
  Remaining: Integer;
  Persistent: Boolean;

  function RemainingTimeoutMilliseconds: Integer;
  var
    RemainingNs: Int64;
  begin
    if ADeadlineNs = 0 then
      Exit(0);
    RemainingNs := ADeadlineNs - GetNanoseconds;
    if RemainingNs <= 0 then
      raise EHTTPError.Create('HTTP request timed out');
    Result := Integer((RemainingNs + 999999) div 1000000);
  end;

  function Receive(var ABuffer: array of Byte;
    const ALength: Integer): Integer;
  var
    Failure: TSocketReadFailure;
  begin
    { A read that fails returns nothing, exactly like a peer that closed the
      connection, and every caller takes "nothing" for the end of the
      response. Two failures are not that. An interrupted read is tried
      again. A read the socket timeout cut short is the request timing out,
      and is reported as that rather than as whatever a short response would
      be taken for: a missing header terminator, or a complete body. }
    repeat
      ConfigureSocketTimeout(ASock, RemainingTimeoutMilliseconds);
      try
        Result := RecvBytes(ASock, ATransport, ABuffer, ALength);
      except
        { A TLS backend that cannot tell a timeout from a broken
          connection raises its read error when the socket timeout ends
          the read. Past the deadline that is the request timing out. }
        if (ADeadlineNs <> 0) and (GetNanoseconds >= ADeadlineNs) then
          raise EHTTPError.Create('HTTP request timed out');
        raise;
      end;
      Failure := srfOther;
      if Result < 0 then
        Failure := LastSocketReadFailure;
    until Failure <> srfInterrupted;
    if (Result <= 0) and
       ((Failure = srfTimedOut) or
        ((ADeadlineNs <> 0) and (GetNanoseconds >= ADeadlineNs))) then
      raise EHTTPError.Create('HTTP request timed out');
    { A failed read (a reset connection) is not the peer closing it, which
      is what ends a body without a declared length. }
    if Result < 0 then
      raise EHTTPError.Create('HTTP connection failed while reading the response');
  end;
begin
  Result.StatusCode := 0;
  Result.StatusText := '';
  SetLength(Result.Headers, 0);
  SetLength(Result.Body, 0);
  Result.Reusable := False;

  // Read until we find the end of headers (CRLFCRLF)
  SetLength(RawHeader, 0);
  HeaderEnd := -1;
  repeat
    try
      N := Receive(Buf, RECV_BUF_SIZE);
    except
      on E: Exception do
      begin
        { On a reused connection, a failed read before the first byte (a
          reset, or the TLS backend's read error) is no response at all; a
          timeout stays a timeout. }
        if AReused and (Length(RawHeader) = 0) and
           not ((ADeadlineNs <> 0) and (GetNanoseconds >= ADeadlineNs)) then
          raise EHTTPNoResponse.Create(E.Message);
        raise;
      end;
    end;
    if (N <= 0) and (Length(RawHeader) = 0) then
      raise EHTTPNoResponse.Create(
        'Invalid HTTP response: no header terminator');
    if N <= 0 then Break;
    AppendBytes(RawHeader, @Buf[0], N, MAX_RESPONSE_HEADER_BYTES);
    HeaderEnd := FindHeaderTerminator(RawHeader);
  until HeaderEnd >= 0;

  if HeaderEnd < 0 then
    raise EHTTPError.Create('Invalid HTTP response: no header terminator');

  // Split headers from any body bytes already received
  HeaderBlock := IsomorphicDecode(RawHeader, 0, HeaderEnd);
  I := HeaderEnd + 4;
  if I < Length(RawHeader) then
  begin
    SetLength(BodyBytes, Length(RawHeader) - I);
    Move(RawHeader[I], BodyBytes[0], Length(BodyBytes));
  end
  else
    SetLength(BodyBytes, 0);

  // Parse status line: "HTTP/1.1 200 OK"
  I := Pos(CRLF, HeaderBlock);
  if I > 0 then
    Line := Copy(HeaderBlock, 1, I - 1)
  else
    Line := HeaderBlock;

  { HTTP/1.1 connections persist unless a Connection field says close
    (RFC 9112 section 9.3); an HTTP/1.0 one is never reused here. }
  Persistent := Copy(Line, 1, 9) = 'HTTP/1.1 ';
  { On a reused connection, bytes the previous response left behind are no
    status line: the connection is given up before any of them is read as
    a body. }
  if AReused and not ((Length(Line) >= 12) and
    (Copy(Line, 1, 7) = 'HTTP/1.') and CharInSet(Line[8], ['0'..'9']) and
    (Line[9] = ' ') and CharInSet(Line[10], ['0'..'9']) and
    CharInSet(Line[11], ['0'..'9']) and CharInSet(Line[12], ['0'..'9']) and
    ((Length(Line) = 12) or (Line[13] = ' '))) then
    raise EHTTPNoResponse.Create('Invalid HTTP response: malformed status line');
  J := Pos(' ', Line);
  if J > 0 then
  begin
    Delete(Line, 1, J);
    J := Pos(' ', Line);
    if J > 0 then
    begin
      Result.StatusCode := StrToIntDef(Copy(Line, 1, J - 1), 0);
      Result.StatusText := Copy(Line, J + 1, Length(Line));
    end
    else
      Result.StatusCode := StrToIntDef(Line, 0);
  end;

  // Parse header lines
  HeaderBlock := Copy(HeaderBlock, Pos(CRLF, HeaderBlock) + 2, Length(HeaderBlock));
  SetLength(Lines, 0);
  while Length(HeaderBlock) > 0 do
  begin
    I := Pos(CRLF, HeaderBlock);
    if I > 0 then
    begin
      SetLength(Lines, Length(Lines) + 1);
      Lines[High(Lines)] := Copy(HeaderBlock, 1, I - 1);
      Delete(HeaderBlock, 1, I + 1);
    end
    else
    begin
      if HeaderBlock <> '' then
      begin
        SetLength(Lines, Length(Lines) + 1);
        Lines[High(Lines)] := HeaderBlock;
      end;
      Break;
    end;
  end;

  SetLength(Result.Headers, Length(Lines));
  for I := 0 to High(Lines) do
  begin
    ColonPos := Pos(':', Lines[I]);
    if ColonPos > 0 then
    begin
      Result.Headers[I].Name := LowerCase(Trim(Copy(Lines[I], 1, ColonPos - 1)));
      Result.Headers[I].Value := Trim(Copy(Lines[I], ColonPos + 1, Length(Lines[I])));
    end
    else
    begin
      Result.Headers[I].Name := LowerCase(Trim(Lines[I]));
      Result.Headers[I].Value := '';
    end;
  end;

  if Persistent then
    for I := 0 to High(Result.Headers) do
      if (Result.Headers[I].Name = 'connection') and
         (Pos('close', LowerCase(Result.Headers[I].Value)) > 0) then
        Persistent := False;

  // Don't read body for HEAD requests or 1xx/204/304 responses
  if AIsHead or (Result.StatusCode div 100 = 1) or
     (Result.StatusCode = 204) or (Result.StatusCode = 304) then
  begin
    Result.Reusable := Persistent and (Result.StatusCode div 100 <> 1) and
      (Length(BodyBytes) = 0);
    Exit;
  end;
  { A redirect the caller will refuse whatever its body holds (redirect
    mode "error") is refused from its status line and headers, without
    waiting for a body. }
  if ASkipRedirectBody and IsRedirectStatus(Result.StatusCode) then
    Exit;

  // Read body
  TransferEncoding := LowerCase(FindHeaderValue(Result.Headers, 'transfer-encoding'));

  if Pos('chunked', TransferEncoding) > 0 then
  begin
    { Chunked transfer coding (RFC 9112 section 7.1). A connection that
      ends before the last chunk is an incomplete message, which fetch
      reports as a network error rather than as a shorter body, and only
      bytes that arrived are ever copied into the body. }
    ChunkBuf := Copy(BodyBytes, 0, Length(BodyBytes));
    SetLength(Result.Body, 0);
    { Room for a whole chunk plus one receive; kept within an Integer for a
      body limit near High(Integer). }
    if AMaxBodyBytes > High(Integer) - RECV_BUF_SIZE then
      ChunkBufLimit := High(Integer)
    else
      ChunkBufLimit := AMaxBodyBytes + RECV_BUF_SIZE;

    repeat
      while FindCRLF(ChunkBuf) < 0 do
      begin
        N := Receive(Buf, RECV_BUF_SIZE);
        if N <= 0 then
          raise EHTTPError.Create(TRUNCATED_BODY_ERROR);
        AppendBytes(ChunkBuf, @Buf[0], N, ChunkBufLimit);
      end;

      I := FindCRLF(ChunkBuf);
      Line := IsomorphicDecode(ChunkBuf, 0, I);
      RemoveLeadingBytes(ChunkBuf, I + 2);

      J := Pos(';', Line);
      if J > 0 then
        Line := Copy(Line, 1, J - 1);

      if not TryParseChunkSize(Trim(Line), DeclaredLength) then
        raise EHTTPError.Create(MALFORMED_CHUNK_ERROR);
      if DeclaredLength > AMaxBodyBytes - Length(Result.Body) then
        raise EHTTPError.CreateFmt('HTTP response body exceeds %d byte limit',
          [AMaxBodyBytes]);
      ChunkSize := Integer(DeclaredLength);
      if ChunkSize = 0 then
      begin
        { The last chunk is followed by the trailer section: field lines,
          then an empty line. The fields are checked and dropped, and the
          section is held to the header size limit. }
        TrailerBytes := 0;
        repeat
          while FindCRLF(ChunkBuf) < 0 do
          begin
            if Length(ChunkBuf) > MAX_RESPONSE_HEADER_BYTES - TrailerBytes then
              raise EHTTPError.Create(MALFORMED_CHUNK_ERROR);
            N := Receive(Buf, RECV_BUF_SIZE);
            if N <= 0 then
              raise EHTTPError.Create(TRUNCATED_BODY_ERROR);
            AppendBytes(ChunkBuf, @Buf[0], N, ChunkBufLimit);
          end;
          I := FindCRLF(ChunkBuf);
          Inc(TrailerBytes, I + 2);
          if TrailerBytes > MAX_RESPONSE_HEADER_BYTES then
            raise EHTTPError.Create(MALFORMED_CHUNK_ERROR);
          if (I > 0) and not IsFieldLine(IsomorphicDecode(ChunkBuf, 0, I)) then
            raise EHTTPError.Create(MALFORMED_CHUNK_ERROR);
          RemoveLeadingBytes(ChunkBuf, I + 2);
        until I = 0;
        Break;
      end;

      while Length(ChunkBuf) < Int64(ChunkSize) + 2 do
      begin
        N := Receive(Buf, RECV_BUF_SIZE);
        if N <= 0 then
          raise EHTTPError.Create(TRUNCATED_BODY_ERROR);
        AppendBytes(ChunkBuf, @Buf[0], N, ChunkBufLimit);
      end;
      if (ChunkBuf[ChunkSize] <> 13) or (ChunkBuf[ChunkSize + 1] <> 10) then
        raise EHTTPError.Create(MALFORMED_CHUNK_ERROR);

      BodyLen := Length(Result.Body);
      SetLength(Result.Body, BodyLen + ChunkSize);
      Move(ChunkBuf[0], Result.Body[BodyLen], ChunkSize);
      RemoveLeadingBytes(ChunkBuf, ChunkSize + 2);
    until False;
    { RFC 9112 section 6.3: chunked must be the final coding, and a
      Content-Length beside it is a framing error a later message could hide
      behind: neither connection is reused. }
    Result.Reusable := Persistent and (Length(ChunkBuf) = 0) and
      (FinalTransferCoding(TransferEncoding) = 'chunked') and
      not HasHeader(Result.Headers, 'content-length');
    { A second Transfer-Encoding field could name a coding after chunked. }
    J := 0;
    for I := 0 to High(Result.Headers) do
      if Result.Headers[I].Name = 'transfer-encoding' then
        Inc(J);
    if J > 1 then
      Result.Reusable := False;
  end
  else
  begin
    ContentLen := -1;
    if HasHeader(Result.Headers, 'content-length') then
    begin
      { Every Content-Length field must hold the same value (RFC 9112
        section 6.3; Fetch's "extract a length"). }
      DeclaredLength := -1;
      for I := 0 to High(Result.Headers) do
        if Result.Headers[I].Name = 'content-length' then
        begin
          if not TryParseContentLength(Result.Headers[I].Value,
             FieldLength) or
             ((DeclaredLength >= 0) and (FieldLength <> DeclaredLength)) then
            raise EHTTPError.Create(
              'Invalid HTTP response: malformed Content-Length');
          DeclaredLength := FieldLength;
        end;
      if DeclaredLength > AMaxBodyBytes then
        raise EHTTPError.CreateFmt('HTTP response body exceeds %d byte limit',
          [AMaxBodyBytes]);
      ContentLen := Integer(DeclaredLength);
    end;

    if ContentLen >= 0 then
    begin
      if ContentLen > AMaxBodyBytes then
        raise EHTTPError.CreateFmt('HTTP response body exceeds %d byte limit',
          [AMaxBodyBytes]);
      SetLength(Result.Body, ContentLen);
      BodyLen := 0;

      { Bytes past the declared length would belong to no request, and a
        Transfer-Encoding field beside Content-Length leaves the length in
        doubt (RFC 9112 section 6.3). }
      Result.Reusable := Persistent and (Length(BodyBytes) <= ContentLen) and
        not HasHeader(Result.Headers, 'transfer-encoding');

      // Copy bytes already read with headers
      if Length(BodyBytes) > 0 then
      begin
        if Length(BodyBytes) >= ContentLen then
        begin
          Move(BodyBytes[0], Result.Body[0], ContentLen);
          BodyLen := ContentLen;
        end
        else
        begin
          Move(BodyBytes[0], Result.Body[0], Length(BodyBytes));
          BodyLen := Length(BodyBytes);
        end;
      end;

      // Read remaining; a connection that ends first is an incomplete
      // message (RFC 9112 section 8), not a body padded with zeros.
      while BodyLen < ContentLen do
      begin
        N := Receive(Buf, RECV_BUF_SIZE);
        if N <= 0 then
          raise EHTTPError.Create(TRUNCATED_BODY_ERROR);
        Remaining := ContentLen - BodyLen;
        if N > Remaining then
        begin
          N := Remaining;
          Result.Reusable := False;
        end;
        Move(Buf[0], Result.Body[BodyLen], N);
        Inc(BodyLen, N);
      end;
    end
    else
    begin
      // Read until connection close
      Result.Body := Copy(BodyBytes);
      repeat
        N := Receive(Buf, RECV_BUF_SIZE);
        if N <= 0 then Break;
        BodyLen := Length(Result.Body);
        if BodyLen > AMaxBodyBytes - N then
          raise EHTTPError.CreateFmt(
            'HTTP response body exceeds %d byte limit',
            [AMaxBodyBytes]);
        SetLength(Result.Body, BodyLen + N);
        Move(Buf[0], Result.Body[BodyLen], N);
      until False;
    end;
  end;
end;

// ---------------------------------------------------------------------------
// Destination resolution and address policy
// ---------------------------------------------------------------------------

{ The address classification lives in NetworkAddress, which carries no socket
  dependency; this unit re-exports it under its original name. }
function IsPrivateNetworkAddress(const AAddressText: string): Boolean;
begin
  Result := NetworkAddress.IsPrivateNetworkAddress(AAddressText);
end;

{ Resolves a hostname to a single numeric address, once.

  This is the fix for the check-then-use window: DoRequest used to validate a
  *hostname* and then hand that same hostname to connect, which resolved it
  again. An allowlisted name under attacker DNS control could answer the first
  lookup with a public address and the second with 169.254.169.254. Resolving
  here and connecting to the returned literal means the address that was
  checked is the address that is used.

  An input that is already a literal is returned unchanged, so no lookup
  happens for numeric targets. }
const
  LOCALHOST_ADDRESS = '127.0.0.1';

{ `localhost`, or any name under it, in any case and with one trailing
  dot. }
function IsLocalhostName(const AHost: string): Boolean;
const
  LOCALHOST_NAME = 'localhost';
var
  Host: string;
begin
  Host := LowerCase(AHost);
  if (Length(Host) > 1) and (Host[Length(Host)] = '.') then
    Delete(Host, Length(Host), 1);
  Result := (Host = LOCALHOST_NAME) or
    ((Length(Host) > Length(LOCALHOST_NAME) + 1) and
     (Copy(Host, Length(Host) - Length(LOCALHOST_NAME), MaxInt) =
      '.' + LOCALHOST_NAME));
end;

function ResolveHostToAddress(const AHost: string): string;
{$IFDEF UNIX}
var
  Octets: array[0..3] of Byte;
{$ENDIF}
{$IFDEF MSWINDOWS}
var
  Octets: array[0..3] of Byte;
  Hints, Res: PAddrInfo;
  HostBytes: TBytes;
  ErrorOffset: Integer;
  SockAddr: PSockAddrIn;
{$ENDIF}
begin
  if AHost = '' then
    raise EHTTPError.Create('Failed to resolve host: (empty)');
  if TryParseIPv4(AHost, Octets) then
    Exit(AHost);
  { RFC 6761 section 6.3: localhost names are loopback, answered here rather
    than by a platform resolver that may not know them (macOS's) or may
    forward them to DNS (every name under localhost). }
  if IsLocalhostName(AHost) then
    Exit(LOCALHOST_ADDRESS);

  {$IFDEF UNIX}
  Result := ResolveIPv4WithSystemResolver(AHost);
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  EnsureWinSockInit;
  New(Hints);
  try
    FillChar(Hints^, SizeOf(TAddrInfo), 0);
    Hints^.ai_family := AF_INET;
    Hints^.ai_socktype := SOCK_STREAM;
    Hints^.ai_protocol := IPPROTO_TCP;
    if not TryEncodeASCIINullTerminated(AHost, HostBytes, ErrorOffset) then
      raise EHTTPError.CreateFmt(
        'HTTP host contains a non-ASCII code unit at offset %d',
        [ErrorOffset]);
    Res := nil;
    if Getaddrinfo(PAnsiChar(@HostBytes[0]), nil, Hints, Res) <> 0 then
      raise EHTTPError.CreateFmt('Failed to resolve host: %s', [AHost]);
    try
      if not Assigned(Res) or not Assigned(Res^.ai_addr) then
        raise EHTTPError.CreateFmt('Failed to resolve host: %s', [AHost]);
      SockAddr := PSockAddrIn(Res^.ai_addr);
      { WinSock2 declares the octets as u_char, a character type Format's
        %d refuses; their ordinals are the octet values. }
      Result := Format('%d.%d.%d.%d', [
        Ord(SockAddr^.sin_addr.S_un_b.s_b1),
        Ord(SockAddr^.sin_addr.S_un_b.s_b2),
        Ord(SockAddr^.sin_addr.S_un_b.s_b3),
        Ord(SockAddr^.sin_addr.S_un_b.s_b4)]);
    finally
      Freeaddrinfo(Res);
    end;
  finally
    Dispose(Hints);
  end;
  {$ENDIF}

  if Result = '' then
    raise EHTTPError.CreateFmt('Failed to resolve host: %s', [AHost]);
end;

{ HTML Standard, same origin: two tuple origins with the same scheme, host
  and port. }
function SameHTTPOrigin(const AFrom, ATo: THTTPParsedURL): Boolean;
begin
  Result := SameText(AFrom.Scheme, ATo.Scheme) and
    SameText(AFrom.Host, ATo.Host) and (AFrom.Port = ATo.Port);
end;

function IsSameHTTPOrigin(const AFromURL, AToURL: string): Boolean;
begin
  Result := SameHTTPOrigin(ParseHTTPURL(AFromURL), ParseHTTPURL(AToURL));
end;

// ---------------------------------------------------------------------------
// Core request logic
// ---------------------------------------------------------------------------

function DoRequest(const AMethod, AURL: string;
  const AHeaders: THTTPHeaders;
  const AMaxRedirects: Integer; const AAllowedHosts: TStrings;
  const ATimeoutMilliseconds: Integer;
  const APolicy: THTTPRequestPolicy): THTTPResponse;
var
  Parsed, NextParsed: THTTPParsedURL;
  ResolvedAddress: string;
  MaxBodyBytes: Integer;
  Sock: TSocket;
  Transport: TTransportSecurityConnection;
  Request: TBytes;
  RequestText: string;
  Raw: TRawHTTPResponse;
  I, Redirects: Integer;
  CurrentURL, Location, HostHeader: string;
  HasUserAgent: Boolean;
  SendCredentials: Boolean;
  IsHead: Boolean;
  Method: string;
  HeaderName, HeaderNameLower, HeaderValue: string;
  DeadlineNs: Int64;
  { A connection is open (Sock, Transport); Reused: it carried the previous
    hop, a redirect within the same origin. }
  Connected, Reused, Stale: Boolean;

  function RemainingTimeoutMilliseconds: Integer;
  var
    RemainingNs: Int64;
  begin
    if DeadlineNs = 0 then
      Exit(0);
    RemainingNs := DeadlineNs - GetNanoseconds;
    if RemainingNs <= 0 then
      raise EHTTPError.Create('HTTP request timed out');
    Result := Integer((RemainingNs + 999999) div 1000000);
  end;

  { Resolves the destination once and validates both the name and the address
    it resolved to, returning the address the caller must connect to.

    Order matters. The name check runs first so an off-allowlist host is
    rejected without a DNS lookup, which keeps a denied request from becoming
    an observable side effect. The address check runs on the resolved value,
    because that is the only form in which "is this target internal" is a
    meaningful question. }
  function DestinationScope(const AParsed: THTTPParsedURL): string;
  begin
    if Pos(':', AParsed.Host) > 0 then
      Result := '[' + AParsed.Host + ']'
    else
      Result := AParsed.Host;
    if not (((AParsed.Scheme = 'http') and (AParsed.Port = 80)) or
            ((AParsed.Scheme = 'https') and (AParsed.Port = 443))) then
      Result := Result + ':' + IntToStr(AParsed.Port);
  end;

  procedure ValidateName(const AParsed: THTTPParsedURL);
  var
    Reason: string;
  begin
    if Assigned(AAllowedHosts) and
       (AAllowedHosts.IndexOf(AParsed.Host) < 0) then
      raise EHTTPError.CreateFmt('fetch host not allowed: %s',
        [AParsed.Host]);
    if Assigned(APolicy.HostCheck) and
       not APolicy.HostCheck(AParsed.Host, AParsed.Port, '', Reason) then
      raise EHTTPDestinationDenied.CreateDenied(DestinationScope(AParsed),
        Reason);
  end;

  procedure ValidateAddress(const AParsed: THTTPParsedURL;
    const AAddress: string);
  var
    Reason: string;
  begin
    if Assigned(APolicy.HostCheck) and
       not APolicy.HostCheck(AParsed.Host, AParsed.Port, AAddress, Reason) then
      raise EHTTPDestinationDenied.CreateDenied(DestinationScope(AParsed),
        Reason);

    if APolicy.DenyPrivateRanges and IsPrivateNetworkAddress(AAddress) then
      raise EHTTPError.CreateFmt(
        'fetch destination not allowed: %s resolves to private address %s',
        [AParsed.Host, AAddress]);
  end;

  function ResolveAndValidateDestination(
    const AParsed: THTTPParsedURL): string;
  begin
    ValidateName(AParsed);
    Result := ResolveHostToAddress(AParsed.Host);
    ValidateAddress(AParsed, Result);
  end;

  procedure CloseConnection;
  begin
    if Connected then
    begin
      Connected := False;
      try
        CloseTransportSecurity(Transport);
      finally
        SocketClose(Sock);
      end;
    end;
  end;

begin
  if ATimeoutMilliseconds < 0 then
    raise EHTTPError.Create('Invalid HTTP timeout');
  MaxBodyBytes := APolicy.MaxResponseBytes;
  if MaxBodyBytes <= 0 then
    MaxBodyBytes := DEFAULT_MAX_RESPONSE_BODY_BYTES;
  if ATimeoutMilliseconds > 0 then
    DeadlineNs := GetNanoseconds +
      Int64(ATimeoutMilliseconds) * 1000000
  else
    DeadlineNs := 0;
  CurrentURL := NormalizeHTTPRequestURL(AURL);
  SendCredentials := True;
  Redirects := 0;
  Result.Redirected := False;
  Method := UpperCase(AMethod);
  IsHead := (Method = 'HEAD');

  Connected := False;
  Reused := False;
  try
    while True do
    begin
      Parsed := ParseHTTPURL(CurrentURL);
      if Reused then
      begin
        { A redirect within the origin rides the connection that answered
          it: the name is checked again, and the address it was pinned to
          is checked again, without a second lookup. }
        ValidateName(Parsed);
        ValidateAddress(Parsed, ResolvedAddress);
      end
      else
        { Runs on every pass of this loop that opens a connection, so a
          redirect hop is resolved, validated, and pinned exactly like the
          initial request. }
        ResolvedAddress := ResolveAndValidateDestination(Parsed);
      ValidateHTTPRequestText(Parsed.Path, 'request target', False);
      if not Reused then
      begin
        FillChar(Transport, SizeOf(Transport), 0);
        { Connect to the address that was just validated, not to the name.
          Both platform connect paths take a numeric fast path for a
          literal, so this performs no second lookup and there is no window
          in which DNS can answer differently. }
        Sock := ConnectSocket(ResolvedAddress, Parsed.Port,
          RemainingTimeoutMilliseconds);
        Connected := True;
        ConfigureSocketTimeout(Sock, RemainingTimeoutMilliseconds);
        { TLS still verifies against the hostname. Pinning changes which
          address we dial, never which identity we require the peer to
          prove — handing the literal here would break certificate
          validation and silently downgrade the connection's guarantees. }
        if Parsed.Scheme = 'https' then
          try
            StartTransportSecurity(Transport, Sock, Parsed.Host, DeadlineNs);
          except
            { A handshake the deadline ended is the request timing out. }
            if (DeadlineNs <> 0) and (GetNanoseconds >= DeadlineNs) then
              raise EHTTPError.Create('HTTP request timed out');
            raise;
          end;
      end
      else
        ConfigureSocketTimeout(Sock, RemainingTimeoutMilliseconds);

      // Build Host header value
      if ((Parsed.Scheme = 'http') and (Parsed.Port = 80)) or
         ((Parsed.Scheme = 'https') and (Parsed.Port = 443)) then
        HostHeader := Parsed.Host
      else
        HostHeader := Parsed.Host + ':' + IntToStr(Parsed.Port);

      RequestText := Method + ' ' + Parsed.Path + ' HTTP/1.1' + CRLF;
      RequestText := RequestText + 'Host: ' + HostHeader + CRLF;
      { HTTP/1.1 connections persist by default (RFC 9112 section 9.3), so
        a redirect within the origin can reuse this one. When no redirect
        can follow, the server is asked to close it, so the closed socket
        waits out TIME-WAIT on its side, not here. }
      if (APolicy.RedirectMode <> hrmFollow) or
         (Redirects >= AMaxRedirects) then
        RequestText := RequestText + 'Connection: close' + CRLF;

      // Check if user provided User-Agent
      HasUserAgent := False;
      for I := 0 to High(AHeaders) do
        if LowerCase(AHeaders[I].Name) = 'user-agent' then
          HasUserAgent := True;

      if not HasUserAgent then
        RequestText := RequestText + 'User-Agent: GocciaScript/1.0' + CRLF;

      // Add custom headers (skip Host since we already set it)
      ValidateHTTPRequestHeaders(AHeaders);
      for I := 0 to High(AHeaders) do
      begin
        HeaderName := AHeaders[I].Name;
        HeaderValue := AHeaders[I].Value;
        HeaderNameLower := LowerCase(HeaderName);
        if HeaderNameLower = 'host' then Continue;
        if not SendCredentials and
           ((HeaderNameLower = 'authorization') or
            (HeaderNameLower = 'cookie') or
            (HeaderNameLower = 'proxy-authorization')) then Continue;
        RequestText := RequestText + HeaderName + ': ' +
          HeaderValue + CRLF;
      end;

      RequestText := RequestText + CRLF;
      Request := EncodeUTF8WithReplacement(RequestText);

      Stale := False;
      try
        try
          SendAll(Sock, Transport, Request);
        except
          { A send on a reused connection that fails (plain or TLS): the
            server closed it. }
          Stale := Reused;
          raise;
        end;
        Raw := ReadResponse(Sock, Transport, IsHead, DeadlineNs,
          MaxBodyBytes, APolicy.RedirectMode = hrmError, Reused);
      except
        on E: Exception do
        begin
          CloseConnection;
          { A send or a read the deadline ended is the request timing out:
            the TLS backends end a send by it, as they end a read. }
          if (DeadlineNs <> 0) and (GetNanoseconds >= DeadlineNs) then
            raise EHTTPError.Create('HTTP request timed out');
          { A reused connection the server closed between two hops, before
            any response arrived: the hop (a GET or HEAD) is made again, once,
            on a connection of its own. A response that did arrive is never
            requested twice. }
          if Reused and (Stale or (E is EHTTPNoResponse)) then
          begin
            Reused := False;
            Continue;
          end;
          raise;
        end;
      end;
      Reused := False;

      { WHATWG Fetch, HTTP fetch: a redirect status is handled by the
        request's redirect mode. "error" is a network error, "manual"
        answers with the redirect response itself, and "follow" runs
        HTTP-redirect fetch, which answers with the response when it has
        no Location. }
      if IsRedirectStatus(Raw.StatusCode) and
         (APolicy.RedirectMode <> hrmManual) then
      begin
        if APolicy.RedirectMode = hrmError then
          raise EHTTPError.Create(REDIRECT_MODE_ERROR_MESSAGE);
        { HTTP-redirect fetch: no Location header answers with the
          response; a present one, even empty, is parsed against the
          response's URL. }
        if HasHeader(Raw.Headers, 'location') then
        begin
          Location := FindHeaderValue(Raw.Headers, 'location');
          // WHATWG Fetch: Location parsed against the response's URL.
          CurrentURL := ResolveHTTPRedirectURL(CurrentURL, Location);
          NextParsed := ParseHTTPURL(CurrentURL);
          { HTTP-redirect fetch: a request that has already been
            redirected AMaxRedirects (20) times fails with a network
            error. }
          if Redirects >= AMaxRedirects then
            raise EHTTPError.CreateFmt(REDIRECT_LIMIT_ERROR_MESSAGE,
              [AMaxRedirects]);
          Inc(Redirects);
          Result.Redirected := True;

          { Fetch removes Authorization once a redirect changes origin.
            Keep caller credentials off every later hop, even if it
            returns here. }
          if not SameHTTPOrigin(Parsed, NextParsed) then
            SendCredentials := False;

          { HTTP-redirect fetch: a 303 turns a request into GET unless it
            is already GET or HEAD, so a HEAD request stays HEAD. }
          if (Raw.StatusCode = HTTP_STATUS_SEE_OTHER) and
             (Method <> 'GET') and not IsHead then
            Method := 'GET';

          { The next hop reuses this connection only within the origin
            (scheme, host and port), on a message that ended cleanly. }
          if Raw.Reusable and SameHTTPOrigin(Parsed, NextParsed) then
            Reused := True
          else
            CloseConnection;
          Continue;
        end;
      end;

      // No redirect — build final response
      Result.StatusCode := Raw.StatusCode;
      Result.StatusText := Raw.StatusText;
      Result.Headers := Raw.Headers;
      Result.Body := Raw.Body;
      Result.FinalURL := CurrentURL;
      Break;
    end;
  finally
    CloseConnection;
  end;
end;

{ A header value read off the wire is a byte sequence held one byte per
  character, not a string of code points, so the URL parser (which encodes
  code points as UTF-8) would encode each byte again: the bytes past 0x7F are
  percent-encoded here, as they are, before the parser sees the value. }
function PercentEncodeHeaderBytes(const AText: string): string;
const
  HEX_DIGITS = '0123456789ABCDEF';
var
  I: Integer;
  B: Byte;
begin
  Result := '';
  for I := 1 to Length(AText) do
  begin
    B := Byte(Ord(AText[I]) and $FF);
    if Ord(AText[I]) > 127 then
      Result := Result + '%' + HEX_DIGITS[(B shr 4) + 1] +
        HEX_DIGITS[(B and $0F) + 1]
    else
      Result := Result + AText[I];
  end;
end;

function NormalizeHTTPRequestURL(const AURL: string): string;
var
  Parsed: TGocciaURLRecord;
  I, AuthorityStart: Integer;
begin
  Result := AURL;
  for I := 1 to Length(AURL) do
    if (Ord(AURL[I]) < 32) or (Ord(AURL[I]) = 127) then
      Exit;
  { A non-ASCII host is the client's own to accept or refuse: the URL parser
    has no IDNA, so it would keep the host as written. }
  AuthorityStart := Pos('://', AURL);
  if AuthorityStart = 0 then
    Exit;
  I := AuthorityStart + 3;
  while (I <= Length(AURL)) and (AURL[I] <> '/') and (AURL[I] <> '?') and
        (AURL[I] <> '#') do
  begin
    if Ord(AURL[I]) > 127 then
      Exit;
    Inc(I);
  end;
  { The parser percent-encodes the path's and the query's non-ASCII code
    points as UTF-8. }
  Parsed := ParseURL(AURL);
  if Parsed.IsValid and ((Parsed.Scheme = 'http') or
     (Parsed.Scheme = 'https')) then
    Result := SerializeURL(Parsed, True);
end;

function ResolveHTTPRedirectURL(const ABaseURL, ALocation: string): string;
var
  Base, Target: TGocciaURLRecord;
  Location: string;
begin
  Location := PercentEncodeHeaderBytes(ALocation);
  Base := ParseURL(ABaseURL);
  if Base.IsValid then
    Target := ParseURLWithBase(Location, Base)
  else
    Target := ParseURL(Location);
  if not Target.IsValid then
    raise EHTTPError.Create('Invalid URL: malformed redirect location');
  Result := SerializeURL(Target, True);
end;

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

function HTTPGet(const AURL: string;
  const AHeaders: THTTPHeaders; const AAllowedHosts: TStrings;
  const ATimeoutMilliseconds: Integer): THTTPResponse;
begin
  Result := DoRequest('GET', AURL, AHeaders, MAX_REDIRECTS, AAllowedHosts,
    ATimeoutMilliseconds, DefaultHTTPPolicy);
end;

function HTTPGet(const AURL: string;
  const AHeaders: THTTPHeaders; const AAllowedHosts: TStrings;
  const ATimeoutMilliseconds: Integer;
  const APolicy: THTTPRequestPolicy): THTTPResponse;
begin
  Result := DoRequest('GET', AURL, AHeaders, MAX_REDIRECTS, AAllowedHosts,
    ATimeoutMilliseconds, APolicy);
end;

function HTTPHead(const AURL: string;
  const AHeaders: THTTPHeaders; const AAllowedHosts: TStrings;
  const ATimeoutMilliseconds: Integer;
  const APolicy: THTTPRequestPolicy): THTTPResponse;
begin
  Result := DoRequest('HEAD', AURL, AHeaders, MAX_REDIRECTS, AAllowedHosts,
    ATimeoutMilliseconds, APolicy);
end;

function HTTPHead(const AURL: string;
  const AHeaders: THTTPHeaders; const AAllowedHosts: TStrings;
  const ATimeoutMilliseconds: Integer): THTTPResponse;
begin
  Result := DoRequest('HEAD', AURL, AHeaders, MAX_REDIRECTS, AAllowedHosts,
    ATimeoutMilliseconds, DefaultHTTPPolicy);
end;

end.
