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

implementation

uses
  SyncObjs,

  {$IFDEF UNIX}
  Sockets, BaseUnix, NetDB,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  WinSock2,
  {$ENDIF}
  CriticalSections,
  TextEncoding,
  TimingUtils,
  TransportSecurity;

const
  HTTP_CONNECTION_WORKER_STACK_SIZE = 1024 * 1024;
  MAX_HTTP_CONNECTION_WORKERS = 16;
  MAX_REDIRECTS = 20;
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
  HostEntry: THostEntry;
  Addr: in_addr;
  ConnectResult: Integer;
  OriginalFlags: Integer;
  PollDescriptor: TPollFD;
  SocketError: Integer;
  SocketErrorLength: TSockLen;
begin
  // Try as numeric IP first
  Addr := StrToNetAddr(AHost);
  if Addr.s_addr = 0 then
  begin
    // DNS lookup via netdb
    if not ResolveHostByName(AHost, HostEntry) then
      raise EHTTPError.CreateFmt('Failed to resolve host: %s', [AHost]);
    Addr := HostEntry.Addr;
  end;

  Result := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Result < 0 then
    raise EHTTPError.Create('Failed to create socket');

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
  Result := fpSend(ASock, ABuf, ALen, 0);
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
  end;

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

function ReadResponse(const ASock: TSocket;
  var ATransport: TTransportSecurityConnection;
  const AIsHead: Boolean; const ADeadlineNs: Int64;
  const AMaxBodyBytes: Integer): TRawHTTPResponse;
var
  Buf: array[0..RECV_BUF_SIZE - 1] of Byte;
  RawHeader: TBytes;
  N, HeaderEnd, I, J, ContentLen, ChunkSize: Integer;
  Line, HeaderBlock: string;
  Lines: array of string;
  ColonPos: Integer;
  TransferEncoding: string;
  BodyBytes: TBytes;
  BodyLen: Integer;
  ChunkBuf: TBytes;
  Done: Boolean;
  Remaining: Integer;

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
  begin
    ConfigureSocketTimeout(ASock, RemainingTimeoutMilliseconds);
    Result := RecvBytes(ASock, ATransport, ABuffer, ALength);
  end;
begin
  Result.StatusCode := 0;
  Result.StatusText := '';
  SetLength(Result.Headers, 0);
  SetLength(Result.Body, 0);

  // Read until we find the end of headers (CRLFCRLF)
  SetLength(RawHeader, 0);
  HeaderEnd := -1;
  repeat
    N := Receive(Buf, RECV_BUF_SIZE);
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

  // Don't read body for HEAD requests or 1xx/204/304 responses
  if AIsHead or (Result.StatusCode div 100 = 1) or
     (Result.StatusCode = 204) or (Result.StatusCode = 304) then
    Exit;

  // Read body
  TransferEncoding := LowerCase(FindHeaderValue(Result.Headers, 'transfer-encoding'));

  if Pos('chunked', TransferEncoding) > 0 then
  begin
    // Chunked transfer encoding
    ChunkBuf := Copy(BodyBytes, 0, Length(BodyBytes));
    SetLength(Result.Body, 0);
    Done := False;

    while not Done do
    begin
      while FindCRLF(ChunkBuf) < 0 do
      begin
        N := Receive(Buf, RECV_BUF_SIZE);
        if N <= 0 then begin Done := True; Break; end;
        AppendBytes(ChunkBuf, @Buf[0], N,
          AMaxBodyBytes + RECV_BUF_SIZE);
      end;
      if Done then Break;

      I := FindCRLF(ChunkBuf);
      Line := IsomorphicDecode(ChunkBuf, 0, I);
      RemoveLeadingBytes(ChunkBuf, I + 2);

      J := Pos(';', Line);
      if J > 0 then
        Line := Copy(Line, 1, J - 1);

      ChunkSize := StrToIntDef('$' + Trim(Line), 0);
      if ChunkSize = 0 then Break;
      if (ChunkSize < 0) or
         (Length(Result.Body) > AMaxBodyBytes - ChunkSize) then
        raise EHTTPError.CreateFmt('HTTP response body exceeds %d byte limit',
          [AMaxBodyBytes]);

      while Length(ChunkBuf) < ChunkSize + 2 do
      begin
        N := Receive(Buf, RECV_BUF_SIZE);
        if N <= 0 then begin Done := True; Break; end;
        AppendBytes(ChunkBuf, @Buf[0], N,
          AMaxBodyBytes + RECV_BUF_SIZE);
      end;

      BodyLen := Length(Result.Body);
      SetLength(Result.Body, BodyLen + ChunkSize);
      Move(ChunkBuf[0], Result.Body[BodyLen], ChunkSize);
      RemoveLeadingBytes(ChunkBuf, ChunkSize + 2);
    end;
  end
  else
  begin
    ContentLen := StrToIntDef(FindHeaderValue(Result.Headers, 'content-length'), -1);

    if ContentLen >= 0 then
    begin
      if ContentLen > AMaxBodyBytes then
        raise EHTTPError.CreateFmt('HTTP response body exceeds %d byte limit',
          [AMaxBodyBytes]);
      SetLength(Result.Body, ContentLen);
      BodyLen := 0;

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

      // Read remaining
      while BodyLen < ContentLen do
      begin
        N := Receive(Buf, RECV_BUF_SIZE);
        if N <= 0 then Break;
        Remaining := ContentLen - BodyLen;
        if N > Remaining then N := Remaining;
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

{ Parses dotted-quad IPv4 text. Deliberately strict: anything that is not
  exactly four decimal octets is not an IPv4 literal, so shortened forms
  ("10.1", "0x7f.1") and octal-looking octets are rejected rather than
  reinterpreted. Those forms are a classic way to smuggle a loopback address
  past a naive textual filter, and both platform resolvers here are AF_INET
  so we never have to accept them. }
function TryParseIPv4(const AValue: string; out AOctets: array of Byte): Boolean;
var
  I, Part, Digits, Value: Integer;
  Ch: Char;
begin
  Part := 0;
  Value := 0;
  Digits := 0;
  for I := 1 to Length(AValue) do
  begin
    Ch := AValue[I];
    if (Ch >= '0') and (Ch <= '9') then
    begin
      Inc(Digits);
      if Digits > 3 then
        Exit(False);
      Value := Value * 10 + (Ord(Ch) - Ord('0'));
      if Value > 255 then
        Exit(False);
    end
    else if Ch = '.' then
    begin
      if (Digits = 0) or (Part > 2) then
        Exit(False);
      AOctets[Part] := Byte(Value);
      Inc(Part);
      Value := 0;
      Digits := 0;
    end
    else
      Exit(False);
  end;
  if (Digits = 0) or (Part <> 3) then
    Exit(False);
  AOctets[3] := Byte(Value);
  Result := True;
end;

{ True when the address belongs to a range that is not routable on the public
  internet and is therefore reachable only from inside the host's own network
  position — which is exactly what an SSRF payload is after. The cloud
  instance-metadata endpoint (169.254.169.254) falls under link-local. }
function IsPrivateNetworkAddress(const AAddressText: string): Boolean;
var
  Octets: array[0..3] of Byte;
  Normalized: string;
begin
  Normalized := LowerCase(Trim(AAddressText));
  if Normalized = '' then
    Exit(True);

  { Strip the brackets an IPv6 authority carries in a URL. }
  if (Length(Normalized) >= 2) and (Normalized[1] = '[') and
     (Normalized[Length(Normalized)] = ']') then
    Normalized := Copy(Normalized, 2, Length(Normalized) - 2);

  if TryParseIPv4(Normalized, Octets) then
  begin
    Result :=
      (Octets[0] = 10) or                                        // 10/8
      (Octets[0] = 127) or                                       // loopback
      (Octets[0] = 0) or                                         // this host
      ((Octets[0] = 172) and (Octets[1] >= 16) and
       (Octets[1] <= 31)) or                                     // 172.16/12
      ((Octets[0] = 192) and (Octets[1] = 168)) or               // 192.168/16
      ((Octets[0] = 169) and (Octets[1] = 254)) or               // link-local
      ((Octets[0] = 100) and (Octets[1] >= 64) and
       (Octets[1] <= 127)) or                                    // CGNAT
      ((Octets[0] = 192) and (Octets[1] = 0) and
       (Octets[2] = 0)) or                                       // IETF proto
      (Octets[0] >= 224);                                        // multicast +
    Exit;
  end;

  { IPv6. Neither platform connect path requests AF_INET6 today, so this is
    defensive: it keeps the classifier correct if a literal reaches it, and
    stays deny-biased for anything it cannot parse. }
  if Pos(':', Normalized) > 0 then
  begin
    Result :=
      (Normalized = '::1') or                                    // loopback
      (Normalized = '::') or                                     // unspecified
      (Copy(Normalized, 1, 2) = 'fc') or                         // ULA fc00::/7
      (Copy(Normalized, 1, 2) = 'fd') or
      (Copy(Normalized, 1, 4) = 'fe80') or                       // link-local
      (Copy(Normalized, 1, 7) = '::ffff:');                      // v4-mapped
    Exit;
  end;

  { Not an address literal at all. Callers pass a resolved address here, so
    reaching this means resolution produced something unexpected; refuse it
    rather than let an unclassifiable target through. }
  Result := True;
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
function ResolveHostToAddress(const AHost: string): string;
{$IFDEF UNIX}
var
  Octets: array[0..3] of Byte;
  HostEntry: THostEntry;
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

  {$IFDEF UNIX}
  if not ResolveHostByName(AHost, HostEntry) then
    raise EHTTPError.CreateFmt('Failed to resolve host: %s', [AHost]);
  Result := NetAddrToStr(HostEntry.Addr);
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
      Result := Format('%d.%d.%d.%d', [
        SockAddr^.sin_addr.S_un_b.s_b1, SockAddr^.sin_addr.S_un_b.s_b2,
        SockAddr^.sin_addr.S_un_b.s_b3, SockAddr^.sin_addr.S_un_b.s_b4]);
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

// ---------------------------------------------------------------------------
// Core request logic
// ---------------------------------------------------------------------------

function DoRequest(const AMethod, AURL: string;
  const AHeaders: THTTPHeaders;
  const AMaxRedirects: Integer; const AAllowedHosts: TStrings;
  const ATimeoutMilliseconds: Integer;
  const APolicy: THTTPRequestPolicy): THTTPResponse;
var
  Parsed: THTTPParsedURL;
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
  IsHead: Boolean;
  Method: string;
  HeaderName, HeaderValue: string;
  DeadlineNs: Int64;

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
  function ResolveAndValidateDestination(
    const AParsed: THTTPParsedURL): string;
  begin
    if Assigned(AAllowedHosts) and
       (AAllowedHosts.IndexOf(AParsed.Host) < 0) then
      raise EHTTPError.CreateFmt('fetch host not allowed: %s',
        [AParsed.Host]);

    Result := ResolveHostToAddress(AParsed.Host);

    if APolicy.DenyPrivateRanges and IsPrivateNetworkAddress(Result) then
      raise EHTTPError.CreateFmt(
        'fetch destination not allowed: %s resolves to private address %s',
        [AParsed.Host, Result]);
  end;

  procedure ValidateRequestText(const AValue, AKind: string;
    const AAllowTab: Boolean);
  var
    K, Code: Integer;
  begin
    for K := 1 to Length(AValue) do
    begin
      Code := Ord(AValue[K]);
      if (Code = 127) or (Code = 0) or (Code = 13) or (Code = 10) or
         ((Code < 32) and not (AAllowTab and (Code = 9))) then
        raise EHTTPError.CreateFmt('Invalid HTTP %s', [AKind]);
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
  CurrentURL := AURL;
  Redirects := 0;
  Result.Redirected := False;
  Method := UpperCase(AMethod);
  IsHead := (Method = 'HEAD');

  while True do
  begin
    Parsed := ParseHTTPURL(CurrentURL);
    { Runs on every pass of this loop, so a redirect hop is resolved,
      validated, and pinned exactly like the initial request. }
    ResolvedAddress := ResolveAndValidateDestination(Parsed);
    ValidateRequestText(Parsed.Path, 'request target', False);
    FillChar(Transport, SizeOf(Transport), 0);
    { Connect to the address that was just validated, not to the name. Both
      platform connect paths take a numeric fast path for a literal, so this
      performs no second lookup and there is no window in which DNS can
      answer differently. }
    Sock := ConnectSocket(ResolvedAddress, Parsed.Port,
      RemainingTimeoutMilliseconds);
    try
      ConfigureSocketTimeout(Sock, RemainingTimeoutMilliseconds);
      { TLS still verifies against the hostname. Pinning changes which
        address we dial, never which identity we require the peer to prove —
        handing the literal here would break certificate validation and
        silently downgrade the connection's guarantees. }
      if Parsed.Scheme = 'https' then
        StartTransportSecurity(Transport, Sock, Parsed.Host);

      try
        // Build Host header value
        if ((Parsed.Scheme = 'http') and (Parsed.Port = 80)) or
           ((Parsed.Scheme = 'https') and (Parsed.Port = 443)) then
          HostHeader := Parsed.Host
        else
          HostHeader := Parsed.Host + ':' + IntToStr(Parsed.Port);

        RequestText := Method + ' ' + Parsed.Path + ' HTTP/1.1' + CRLF;
        RequestText := RequestText + 'Host: ' + HostHeader + CRLF;
        RequestText := RequestText + 'Connection: close' + CRLF;

        // Check if user provided User-Agent
        HasUserAgent := False;
        for I := 0 to High(AHeaders) do
          if LowerCase(AHeaders[I].Name) = 'user-agent' then
            HasUserAgent := True;

        if not HasUserAgent then
          RequestText := RequestText + 'User-Agent: GocciaScript/1.0' + CRLF;

        // Add custom headers (skip Host since we already set it)
        for I := 0 to High(AHeaders) do
        begin
          HeaderName := AHeaders[I].Name;
          HeaderValue := AHeaders[I].Value;
          ValidateRequestText(HeaderName, 'header name', False);
          ValidateRequestText(HeaderValue, 'header value', True);
          if (HeaderName = '') or (Pos(':', HeaderName) > 0) or
             (Pos(' ', HeaderName) > 0) or (Pos(#9, HeaderName) > 0) then
            raise EHTTPError.Create('Invalid HTTP header name');
          if LowerCase(HeaderName) = 'host' then Continue;
          RequestText := RequestText + HeaderName + ': ' +
            HeaderValue + CRLF;
        end;

        RequestText := RequestText + CRLF;
        Request := EncodeUTF8WithReplacement(RequestText);

        SendAll(Sock, Transport, Request);
        Raw := ReadResponse(Sock, Transport, IsHead, DeadlineNs,
          MaxBodyBytes);
      finally
        CloseTransportSecurity(Transport);
      end;
    finally
      SocketClose(Sock);
    end;

    // Handle redirects
    if (Raw.StatusCode >= 301) and (Raw.StatusCode <= 308) and
       (Raw.StatusCode <> 304) and (Raw.StatusCode <> 305) then
    begin
      Location := FindHeaderValue(Raw.Headers, 'location');
      if (Location <> '') and (Redirects < AMaxRedirects) then
      begin
        Inc(Redirects);
        Result.Redirected := True;

        // Handle relative URLs
        if Copy(Location, 1, 2) = '//' then
          CurrentURL := Parsed.Scheme + ':' + Location
        else if (Length(Location) > 0) and (Location[1] = '/') then
          CurrentURL := Parsed.Scheme + '://' + HostHeader + Location
        else if Pos('://', Location) = 0 then
          CurrentURL := Parsed.Scheme + '://' + HostHeader + '/' + Location
        else
          CurrentURL := Location;

        // 303: change method to GET per RFC 7231
        if Raw.StatusCode = 303 then
        begin
          Method := 'GET';
          IsHead := False;
        end;

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
