unit TransportSecurity;

// Cross-platform TLS transport for blocking sockets.
// macOS uses SecureTransport, Windows uses SChannel, Unix uses OpenSSL.

{$I Shared.inc}

interface

uses
  SysUtils
{$IFNDEF LAKON}
  {$IFDEF UNIX}
  , Sockets
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  , WinSock2
  {$ENDIF}
{$ENDIF}
  ;

{$IFDEF LAKON}
type
  // Lakon's WASM lane has no sockets (its capability contract stops
  // at stdio/argv), so no platform socket unit exists to import; the
  // alias keeps TTransportSecurityConnection and the signatures below
  // compiling for consumers that only need the TYPES at compile time
  // (HTTPClient's unit closure). The bodies raise if ever reached.
  TSocket = Integer;
{$ENDIF}

type
  TTransportSecurityConnection = record
  public
    Active: Boolean;
  private
    Backend: Integer;
    Socket: TSocket;
    BackendData: Pointer;
    DeadlineNanoseconds: Int64;
  end;

{ ADeadlineNanoseconds is the request's deadline on the GetNanoseconds clock,
  or 0 (the default) for none. The handshake and every later read and write
  end by it, however the peer paces its bytes. A handshake past the deadline
  raises. Past it, OpenSSL's read and write return -1 with EAGAIN in errno, as
  a plain receive that timed out leaves it, and SecureTransport's and
  SChannel's raise their read or write error. Otherwise both return the byte
  count, or 0 when the peer closed the connection. The OpenSSL backend puts
  the socket in non-blocking mode for the rest of the connection. }
procedure StartTransportSecurity(var AConnection: TTransportSecurityConnection;
  const ASocket: TSocket; const AHost: string;
  const ADeadlineNanoseconds: Int64 = 0);
procedure CloseTransportSecurity(var AConnection: TTransportSecurityConnection);
function TransportSecurityRead(var AConnection: TTransportSecurityConnection;
  var ABuffer: array of Byte; const ALength: Integer): Integer;
function TransportSecurityWrite(var AConnection: TTransportSecurityConnection;
  const ABuffer: Pointer; const ALength: Integer): Integer;

{$IFNDEF LAKON}
{ A send on the blocking socket that ends by ADeadlineNanoseconds (0 for none,
  which leaves the socket's own send timeout), so a peer that stops reading, or
  reads slowly, cannot hold a send past the deadline. Returns the bytes sent,
  which may be fewer than ALength, or -1 once the deadline has passed or the
  send failed. On Windows the socket's send timeout is set to the time left
  before the call. Elsewhere the call waits for room with poll and sends without
  blocking, because a blocking send's timeout restarts each time the peer makes
  room (Darwin), so a slowly draining peer could keep one send running past it.
  It sends with the flags SocketSend does, so it keeps the protection against
  SIGPIPE that send carries (issue #1815). }
function SocketSendBefore(const ASock: TSocket; const ABuffer: Pointer;
  const ALength: Integer; const ADeadlineNanoseconds: Int64): Integer;

{ Sends until all ALength bytes have gone or the deadline passes or a send
  fails, each send as SocketSendBefore does. Returns how many bytes went out,
  from 0 to ALength: fewer than ALength only when it stopped early. }
function SocketSendAllBefore(const ASock: TSocket; const ABuffer: Pointer;
  const ALength: Integer; const ADeadlineNanoseconds: Int64): Integer;

{ Settles SChannel's encrypted input after a call that reads from it. SChannel
  decrypts and reads in place and returns what it did not consume as a
  SECBUFFER_EXTRA buffer of AExtraLength bytes: the last AExtraLength bytes of
  the input (Microsoft, "Extra Buffers Returned by Schannel"). They move to the
  front of AInput while it is still allocated, and AInput shrinks to them, so no
  byte is read from memory that was freed; zero clears it. A message that was
  incomplete (SEC_E_INCOMPLETE_MESSAGE) consumed nothing: AIncomplete leaves the
  input as it is for the next receive to extend. Raises when AExtraLength is
  negative or longer than AInput. }
procedure ConsumeEncryptedInput(var AInput: TBytes; const AIncomplete: Boolean;
  const AExtraLength: Integer);
{$ENDIF}

implementation

{$IFDEF LAKON}

type
  ETransportSecurityError = class(Exception);

const
  NO_TLS_ERROR = 'TLS transport is not available on this platform';

procedure StartTransportSecurity(var AConnection: TTransportSecurityConnection;
  const ASocket: TSocket; const AHost: string;
  const ADeadlineNanoseconds: Int64);
begin
  AConnection.Active := False;
  AConnection.Backend := 0;
  AConnection.Socket := ASocket;
  AConnection.BackendData := nil;
  raise ETransportSecurityError.Create(NO_TLS_ERROR);
end;

procedure CloseTransportSecurity(var AConnection: TTransportSecurityConnection);
begin
  AConnection.Active := False;
end;

function TransportSecurityRead(var AConnection: TTransportSecurityConnection;
  var ABuffer: array of Byte; const ALength: Integer): Integer;
begin
  Result := -1;
end;

function TransportSecurityWrite(var AConnection: TTransportSecurityConnection;
  const ABuffer: Pointer; const ALength: Integer): Integer;
begin
  Result := -1;
end;

{$ELSE}

uses
  {$IFDEF UNIX}
  BaseUnix,
  {$IFNDEF DARWIN}
  ctypes,
  CriticalSections,
  DynamicLibraries,
  OpenSSL,
  {$ENDIF}
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Math,
  TextEncoding,
  TimingUtils;

const
  TSB_NONE = 0;
  TSB_OPENSSL = 1;
  TSB_SECURE_TRANSPORT = 2;
  TSB_SCHANNEL = 3;
  OPENSSL_LOAD_ERROR = 'HTTPS requires OpenSSL but it could not be loaded';
  TLS_HANDSHAKE_ERROR = 'TLS handshake failed';
  TLS_READ_ERROR = 'TLS read failed';
  TLS_WRITE_ERROR = 'TLS write failed';
  TLS_TIMEOUT_ERROR = 'TLS handshake timed out';

type
  ETransportSecurityError = class(Exception);

function EncodeTLSHostName(const AHost: string): TBytes;
var
  ErrorOffset: Integer;
begin
  if not TryEncodeASCIINullTerminated(AHost, Result, ErrorOffset) then
    raise ETransportSecurityError.CreateFmt(
      'TLS host name contains a non-ASCII code unit at offset %d',
      [ErrorOffset]);
end;

procedure ConsumeEncryptedInput(var AInput: TBytes; const AIncomplete: Boolean;
  const AExtraLength: Integer);
var
  ConsumedLength: Integer;
begin
  if AIncomplete then
    Exit;
  if (AExtraLength < 0) or (AExtraLength > Length(AInput)) then
    raise ETransportSecurityError.Create(TLS_READ_ERROR);
  ConsumedLength := Length(AInput) - AExtraLength;
  if (AExtraLength > 0) and (ConsumedLength > 0) then
    Move(AInput[ConsumedLength], AInput[0], AExtraLength);
  SetLength(AInput, AExtraLength);
end;

{ A write to a peer that reset the connection must fail the write with EPIPE
  or ECONNRESET, not end the process with SIGPIPE. Linux takes MSG_NOSIGNAL
  per send. Darwin and the BSDs take SO_NOSIGPIPE on the socket instead
  (HTTPClient sets it where it creates the socket), which also covers
  SecureTransport's writes through the callback below. }
function SocketSend(const ASock: TSocket; const ABuffer: Pointer;
  const ALength: Integer): Integer; {$IFDEF FPC}inline;{$ENDIF}
begin
  {$IFDEF UNIX}
  {$IFDEF LINUX}
  Result := fpSend(ASock, ABuffer, ALength, MSG_NOSIGNAL);
  {$ELSE}
  Result := fpSend(ASock, ABuffer, ALength, 0);
  {$ENDIF}
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Result := WinSock2.send(ASock, ABuffer^, ALength, 0);
  {$ENDIF}
end;

function SocketReceive(const ASock: TSocket; const ABuffer: Pointer;
  const ALength: Integer): Integer; {$IFDEF FPC}inline;{$ENDIF}
begin
  {$IFDEF UNIX}
  Result := fpRecv(ASock, ABuffer, ALength, 0);
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Result := WinSock2.recv(ASock, ABuffer^, ALength, 0);
  {$ENDIF}
end;

{ Milliseconds left before ADeadlineNanoseconds, rounded up; 0 when it has
  passed. Only called with a deadline set. }
function MillisecondsBefore(const ADeadlineNanoseconds: Int64): Int64;
var
  Remaining: Int64;
begin
  Remaining := ADeadlineNanoseconds - GetNanoseconds;
  if Remaining <= 0 then
    Result := 0
  else
    Result := (Remaining + 999999) div 1000000;
end;

{$IF DEFINED(DARWIN) OR DEFINED(MSWINDOWS)}
{ Sets option AOption (SO_RCVTIMEO or SO_SNDTIMEO) of ASock to AMilliseconds,
  which is above 0: a timeout of 0 means none. }
procedure SetSocketTimeout(const ASock: TSocket; const AOption: Integer;
  const AMilliseconds: Int64);
var
  {$IFDEF UNIX}
  Timeout: TTimeVal;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Timeout: LongInt;
  {$ENDIF}
begin
  {$IFDEF UNIX}
  Timeout.tv_sec := AMilliseconds div 1000;
  Timeout.tv_usec := (AMilliseconds mod 1000) * 1000;
  fpSetSockOpt(ASock, SOL_SOCKET, AOption, @Timeout, SizeOf(Timeout));
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Timeout := LongInt(Min(AMilliseconds, Int64(High(LongInt))));
  WinSock2.setsockopt(ASock, SOL_SOCKET, AOption, PAnsiChar(@Timeout),
    SizeOf(Timeout));
  {$ENDIF}
end;
{$IFEND}

{$IFDEF UNIX}
{ One send that never blocks: it takes what fits and returns, or returns -1
  with EAGAIN. Linux takes that per send. Darwin and the BSDs ignore
  MSG_DONTWAIT on a send, so the socket is made non-blocking around the call
  (the connection is used by one thread; a send that SO_NOSIGPIPE covers
  raises no signal). errno is the send's. }
function SocketSendNoWait(const ASock: TSocket; const ABuffer: Pointer;
  const ALength: Integer): Integer;
{$IFNDEF LINUX}
var
  Flags: LongInt;
  SendError: LongInt;
{$ENDIF}
begin
  {$IFDEF LINUX}
  Result := fpSend(ASock, ABuffer, ALength, MSG_NOSIGNAL or $40);
  {$ELSE}
  Flags := fpFcntl(ASock, F_GETFL, 0);
  if (Flags < 0) or (fpFcntl(ASock, F_SETFL, Flags or O_NONBLOCK) < 0) then
    Exit(-1);
  Result := fpSend(ASock, ABuffer, ALength, 0);
  SendError := fpGetErrNo;
  fpFcntl(ASock, F_SETFL, Flags);
  fpSetErrNo(SendError);
  {$ENDIF}
end;

function SocketSendBefore(const ASock: TSocket; const ABuffer: Pointer;
  const ALength: Integer; const ADeadlineNanoseconds: Int64): Integer;
const
  MAX_POLL_MILLISECONDS = 1 shl 30;
var
  Descriptor: TPollFd;
  Milliseconds: Int64;
  Outcome: LongInt;
  Ready: Boolean;
begin
  if ADeadlineNanoseconds = 0 then
    Exit(SocketSend(ASock, ABuffer, ALength));
  repeat
    Ready := False;
    repeat
      Milliseconds := MillisecondsBefore(ADeadlineNanoseconds);
      if Milliseconds = 0 then
        Exit(-1);
      if Milliseconds > MAX_POLL_MILLISECONDS then
        Milliseconds := MAX_POLL_MILLISECONDS;
      Descriptor.fd := ASock;
      Descriptor.events := POLLOUT;
      Descriptor.revents := 0;
      Outcome := fpPoll(@Descriptor, 1, Milliseconds);
      if Outcome > 0 then
        Ready := True
      else if (Outcome < 0) and (fpGetErrNo <> ESysEINTR) then
        Exit(-1);
    until Ready;
    Result := SocketSendNoWait(ASock, ABuffer, ALength);
    { Another thread's send can take the room poll reported: wait again, for
      the time that is left. }
  until (Result >= 0) or
        ((fpGetErrNo <> ESysEAGAIN) and (fpGetErrNo <> ESysEINTR));
  if Result < 0 then
    Result := -1;
end;
{$ENDIF}
{$IFDEF MSWINDOWS}
function SocketSendBefore(const ASock: TSocket; const ABuffer: Pointer;
  const ALength: Integer; const ADeadlineNanoseconds: Int64): Integer;
var
  Milliseconds: Int64;
begin
  if ADeadlineNanoseconds <> 0 then
  begin
    Milliseconds := MillisecondsBefore(ADeadlineNanoseconds);
    if Milliseconds = 0 then
      Exit(-1);
    SetSocketTimeout(ASock, SO_SNDTIMEO, Milliseconds);
  end;
  Result := SocketSend(ASock, ABuffer, ALength);
end;
{$ENDIF}

function SocketSendAllBefore(const ASock: TSocket; const ABuffer: Pointer;
  const ALength: Integer; const ADeadlineNanoseconds: Int64): Integer;
const
  {$IFDEF MSWINDOWS}
  { A Windows send that times out says nothing of how much of its buffer went
    out, so no call is given more than a chunk and the count stays within one
    chunk of the truth. }
  MAX_SEND_LENGTH = 65536;
  {$ELSE}
  MAX_SEND_LENGTH = High(Integer);
  {$ENDIF}
var
  Written: Integer;
begin
  Result := 0;
  while Result < ALength do
  begin
    Written := SocketSendBefore(ASock,
      Pointer(NativeUInt(ABuffer) + NativeUInt(Result)),
      Min(ALength - Result, MAX_SEND_LENGTH), ADeadlineNanoseconds);
    if Written <= 0 then
      Exit;
    Inc(Result, Written);
  end;
end;

{$IF DEFINED(DARWIN) OR DEFINED(MSWINDOWS)}
{ A receive on the blocking socket that ends by ADeadlineNanoseconds (0 for
  none): the socket's receive timeout is set to the time left before every
  call, so a peer that trickles bytes cannot stretch a TLS read made of many
  receives past the deadline. Returns -1 once the deadline has passed. }
function SocketReceiveBefore(const ASock: TSocket; const ABuffer: Pointer;
  const ALength: Integer; const ADeadlineNanoseconds: Int64): Integer;
var
  Milliseconds: Int64;
begin
  if ADeadlineNanoseconds <> 0 then
  begin
    Milliseconds := MillisecondsBefore(ADeadlineNanoseconds);
    if Milliseconds = 0 then
      Exit(-1);
    SetSocketTimeout(ASock, SO_RCVTIMEO, Milliseconds);
  end;
  Result := SocketReceive(ASock, ABuffer, ALength);
end;
{$IFEND}

{$IFDEF MSWINDOWS}
{ Sends all ALength bytes by ADeadlineNanoseconds (0 for none). False when the
  deadline passed or a send failed first; how many bytes went out is then
  unknown. }
function SendSocketAll(const ASocket: TSocket; const ABuffer: Pointer;
  const ALength: Integer; const ADeadlineNanoseconds: Int64): Boolean;
begin
  Result := SocketSendAllBefore(ASocket, ABuffer, ALength,
    ADeadlineNanoseconds) = ALength;
end;
{$ENDIF}

{$IFDEF DARWIN}
{$linkframework Security}
{$linkframework CoreFoundation}

type
  OSStatus = LongInt;
  CFAllocatorRef = Pointer;
  SSLContextRef = Pointer;
  SSLConnectionRef = Pointer;
  SSLProtocolSide = Integer;
  SSLConnectionType = Integer;
  SSLProtocol = Integer;

const
  ERR_SEC_SUCCESS = 0;
  ERR_SSL_WOULD_BLOCK = -9803;
  ERR_SSL_CLOSED_GRACEFUL = -9805;
  ERR_SSL_CLOSED_ABORT = -9806;
  K_SSL_CLIENT_SIDE = 1;
  K_SSL_STREAM_TYPE = 0;
  K_TLS_PROTOCOL_12 = 8;

type
  TSecureTransportData = class
  public
    Socket: TSocket;
    Context: SSLContextRef;
    DeadlineNanoseconds: Int64;
  end;

  TSecureTransportReadFunc = function(AConnection: SSLConnectionRef;
    AData: Pointer; var ADataLength: NativeUInt): OSStatus; cdecl;
  TSecureTransportWriteFunc = function(AConnection: SSLConnectionRef;
    AData: Pointer; var ADataLength: NativeUInt): OSStatus; cdecl;

function SSLCreateContext(AAllocator: CFAllocatorRef;
  AProtocolSide: SSLProtocolSide;
  AConnectionType: SSLConnectionType): SSLContextRef; cdecl;
  external name 'SSLCreateContext';
function SSLSetIOFuncs(AContext: SSLContextRef;
  AReadFunc: TSecureTransportReadFunc;
  AWriteFunc: TSecureTransportWriteFunc): OSStatus; cdecl;
  external name 'SSLSetIOFuncs';
function SSLSetConnection(AContext: SSLContextRef;
  AConnection: SSLConnectionRef): OSStatus; cdecl;
  external name 'SSLSetConnection';
function SSLSetPeerDomainName(AContext: SSLContextRef; APeerName: PAnsiChar;
  APeerNameLength: NativeUInt): OSStatus; cdecl;
  external name 'SSLSetPeerDomainName';
function SSLSetProtocolVersionMin(AContext: SSLContextRef;
  AVersion: SSLProtocol): OSStatus; cdecl;
  external name 'SSLSetProtocolVersionMin';
function SSLHandshake(AContext: SSLContextRef): OSStatus; cdecl;
  external name 'SSLHandshake';
function SSLRead(AContext: SSLContextRef; AData: Pointer;
  ADataLength: NativeUInt; var AProcessed: NativeUInt): OSStatus; cdecl;
  external name 'SSLRead';
function SSLWrite(AContext: SSLContextRef; AData: Pointer;
  ADataLength: NativeUInt; var AProcessed: NativeUInt): OSStatus; cdecl;
  external name 'SSLWrite';
function SSLClose(AContext: SSLContextRef): OSStatus; cdecl;
  external name 'SSLClose';
procedure CFRelease(ARef: Pointer); cdecl; external name 'CFRelease';

function SecureTransportSocketRead(AConnection: SSLConnectionRef;
  AData: Pointer; var ADataLength: NativeUInt): OSStatus; cdecl;
var
  Data: TSecureTransportData;
  RequestedLength: NativeUInt;
  ReadCount: Integer;
begin
  Data := TSecureTransportData(AConnection);
  RequestedLength := ADataLength;
  ReadCount := SocketReceiveBefore(Data.Socket, AData, ADataLength,
    Data.DeadlineNanoseconds);
  if ReadCount > 0 then
  begin
    ADataLength := ReadCount;
    if NativeUInt(ReadCount) = RequestedLength then
      Result := ERR_SEC_SUCCESS
    else
      Result := ERR_SSL_WOULD_BLOCK;
  end
  else if ReadCount = 0 then
  begin
    ADataLength := 0;
    Result := ERR_SSL_CLOSED_GRACEFUL;
  end
  else
  begin
    ADataLength := 0;
    Result := ERR_SSL_CLOSED_ABORT;
  end;
end;

function SecureTransportSocketWrite(AConnection: SSLConnectionRef;
  AData: Pointer; var ADataLength: NativeUInt): OSStatus; cdecl;
var
  Data: TSecureTransportData;
  RequestedLength: NativeUInt;
  Written: Integer;
begin
  Data := TSecureTransportData(AConnection);
  RequestedLength := ADataLength;
  { SSLWrite counts what this callback accepts as written, so it takes all of
    it unless the deadline or the connection stops it. A send that moves part
    of the buffer, as a non-blocking one does under backpressure, would
    otherwise leave the rest queued while the request looks sent. }
  Written := SocketSendAllBefore(Data.Socket, AData, ADataLength,
    Data.DeadlineNanoseconds);
  if Written > 0 then
  begin
    ADataLength := Written;
    if NativeUInt(Written) = RequestedLength then
      Result := ERR_SEC_SUCCESS
    else
      Result := ERR_SSL_WOULD_BLOCK;
  end
  else
  begin
    ADataLength := 0;
    Result := ERR_SSL_CLOSED_ABORT;
  end;
end;

procedure StartSecureTransport(var AConnection: TTransportSecurityConnection;
  const AHost: string);
var
  Data: TSecureTransportData;
  HostName: TBytes;
  Status: OSStatus;
begin
  Data := TSecureTransportData.Create;
  Data.Socket := AConnection.Socket;
  Data.DeadlineNanoseconds := AConnection.DeadlineNanoseconds;
  Data.Context := SSLCreateContext(nil, K_SSL_CLIENT_SIDE, K_SSL_STREAM_TYPE);
  if Data.Context = nil then
  begin
    Data.Free;
    raise ETransportSecurityError.Create('Failed to create SecureTransport context');
  end;

  try
    Status := SSLSetIOFuncs(Data.Context, SecureTransportSocketRead,
      SecureTransportSocketWrite);
    if Status <> ERR_SEC_SUCCESS then
      raise ETransportSecurityError.Create('Failed to set SecureTransport I/O callbacks');

    Status := SSLSetConnection(Data.Context, SSLConnectionRef(Data));
    if Status <> ERR_SEC_SUCCESS then
      raise ETransportSecurityError.Create('Failed to bind SecureTransport socket');

    HostName := EncodeTLSHostName(AHost);
    Status := SSLSetPeerDomainName(Data.Context, PAnsiChar(@HostName[0]),
      Length(HostName) - 1);
    if Status <> ERR_SEC_SUCCESS then
      raise ETransportSecurityError.Create('Failed to set TLS server name');

    Status := SSLSetProtocolVersionMin(Data.Context, K_TLS_PROTOCOL_12);
    if Status <> ERR_SEC_SUCCESS then
      raise ETransportSecurityError.Create('Failed to set minimum TLS version');

    repeat
      Status := SSLHandshake(Data.Context);
    until Status <> ERR_SSL_WOULD_BLOCK;

    if Status <> ERR_SEC_SUCCESS then
      raise ETransportSecurityError.CreateFmt('%s: %d',
        [TLS_HANDSHAKE_ERROR, Status]);

    AConnection.BackendData := Data;
    AConnection.Backend := TSB_SECURE_TRANSPORT;
    AConnection.Active := True;
  except
    CFRelease(Data.Context);
    Data.Free;
    raise;
  end;
end;

procedure CloseSecureTransport(var AConnection: TTransportSecurityConnection);
var
  Data: TSecureTransportData;
begin
  Data := TSecureTransportData(AConnection.BackendData);
  if Assigned(Data) then
  begin
    if Data.Context <> nil then
    begin
      SSLClose(Data.Context);
      CFRelease(Data.Context);
    end;
    Data.Free;
  end;
end;

function ReadSecureTransport(var AConnection: TTransportSecurityConnection;
  var ABuffer: array of Byte; const ALength: Integer): Integer;
var
  Data: TSecureTransportData;
  Processed: NativeUInt;
  Status: OSStatus;
begin
  Data := TSecureTransportData(AConnection.BackendData);
  Processed := 0;
  repeat
    Status := SSLRead(Data.Context, @ABuffer[0], ALength, Processed);
  until (Status <> ERR_SSL_WOULD_BLOCK) or (Processed > 0);
  if (Status <> ERR_SEC_SUCCESS) and (Status <> ERR_SSL_CLOSED_GRACEFUL) and
     (Status <> ERR_SSL_WOULD_BLOCK) then
    raise ETransportSecurityError.CreateFmt('%s: %d', [TLS_READ_ERROR, Status]);
  Result := Processed;
end;

function WriteSecureTransport(var AConnection: TTransportSecurityConnection;
  const ABuffer: Pointer; const ALength: Integer): Integer;
var
  Data: TSecureTransportData;
  Processed: NativeUInt;
  Status: OSStatus;
begin
  Data := TSecureTransportData(AConnection.BackendData);
  Processed := 0;
  repeat
    Status := SSLWrite(Data.Context, ABuffer, ALength, Processed);
  until (Status <> ERR_SSL_WOULD_BLOCK) or (Processed > 0);
  if (Status <> ERR_SEC_SUCCESS) and (Status <> ERR_SSL_WOULD_BLOCK) then
    raise ETransportSecurityError.CreateFmt('%s: %d', [TLS_WRITE_ERROR, Status]);
  Result := Processed;
end;
{$ENDIF}

{$IFDEF UNIX}
{$IFNDEF DARWIN}
type
  TOpenSSLData = class
  public
    Context: PSSL_CTX;
    SSL: PSSL;
  end;

  TSSLSetDefaultVerifyPaths = function(AContext: PSSL_CTX): cint; cdecl;
  TSSLSetHostName = function(ASSL: PSSL; AHost: PAnsiChar): cint; cdecl;
  TSSLMethodGetter = function: Pointer; cdecl;

const
  OPENSSL_VERSION_THREE = '.3';
  SSL_CTRL_SET_MIN_PROTO_VERSION = 123;
  TLS1_2_VERSION = $0303;

var
  // OpenSSL is loaded on the first handshake, which runs on whichever fetch
  // or package worker thread gets there first, so several can arrive at once.
  // EnsureOpenSSLLoaded serializes that load under this lock and records its
  // outcome so the load runs once per process.
  GOpenSSLLoadLock: TGocciaCriticalSection;
  GOpenSSLLoadAttempted: Boolean = False;
  GOpenSSLLoaded: Boolean = False;

procedure PreferOpenSSLVersionThree;
var
  I: Integer;
begin
  for I := High(DLLVersions) downto Low(DLLVersions) + 1 do
    DLLVersions[I] := DLLVersions[I - 1];
  DLLVersions[Low(DLLVersions)] := OPENSSL_VERSION_THREE;
end;

function TryUseOpenSSLPair(const ADirectory, AVersion: string): Boolean;
var
  SSLBase: string;
  CryptoBase: string;
begin
  SSLBase := IncludeTrailingPathDelimiter(ADirectory) + 'libssl';
  CryptoBase := IncludeTrailingPathDelimiter(ADirectory) + 'libcrypto';
  Result := FileExists(SSLBase + '.so' + AVersion) and
    FileExists(CryptoBase + '.so' + AVersion);
  if Result then
  begin
    {$IFDEF GOCCIA_FPC_OPENSSL_DLL_NAMES}
    DLLSSLName := SSLBase;
    DLLUtilName := CryptoBase;
    {$ELSE}
    BaseSSLName := SSLBase;
    BaseCryptoName := CryptoBase;
    {$ENDIF}
    DLLVersions[Low(DLLVersions)] := AVersion;
  end;
end;

procedure ConfigureOpenSSLLoading;
const
  DIRECTORIES: array[0..7] of string = (
    '/lib/x86_64-linux-gnu',
    '/usr/lib/x86_64-linux-gnu',
    '/lib/aarch64-linux-gnu',
    '/usr/lib/aarch64-linux-gnu',
    '/lib64',
    '/usr/lib64',
    '/lib',
    '/usr/lib'
  );
  VERSIONS: array[0..2] of string = (
    '.3',
    '',
    '.1.1'
  );
var
  DirectoryIndex: Integer;
  VersionIndex: Integer;
begin
  PreferOpenSSLVersionThree;
  for DirectoryIndex := Low(DIRECTORIES) to High(DIRECTORIES) do
    for VersionIndex := Low(VERSIONS) to High(VERSIONS) do
      if TryUseOpenSSLPair(DIRECTORIES[DirectoryIndex],
        VERSIONS[VersionIndex]) then
        Exit;
end;

// FPC 3.2.2's InitSSLInterface is not safe to race: it reads SSLloaded into
// Result before entering its own critical section, so a thread that waited on
// that section while another loaded the library exits with Result still False
// and reports a failed load of a loaded library. ConfigureOpenSSLLoading also
// rewrites the openssl unit's DLLVersions and library-name globals,
// shifting DLLVersions on every call. Both therefore run once, under a lock
// this unit owns, and the result is read from IsSSLloaded rather than from
// InitSSLInterface's return value. A failed load is remembered too, so every
// later handshake reports the same missing-library error.
function EnsureOpenSSLLoaded: Boolean;
begin
  CriticalSectionEnter(GOpenSSLLoadLock);
  try
    if not GOpenSSLLoadAttempted then
    begin
      GOpenSSLLoadAttempted := True;
      ConfigureOpenSSLLoading;
      InitSSLInterface;
      GOpenSSLLoaded := IsSSLloaded;
    end;
    Result := GOpenSSLLoaded;
  finally
    CriticalSectionLeave(GOpenSSLLoadLock);
  end;
end;

procedure ConfigureOpenSSLVerification(const AContext: PSSL_CTX;
  const ASSL: PSSL; const AHost: string);
var
  SetDefaultVerifyPaths: TSSLSetDefaultVerifyPaths;
  SetHostName: TSSLSetHostName;
  HostName: TBytes;
begin
  SetDefaultVerifyPaths := TSSLSetDefaultVerifyPaths(GetProcedureAddress(
    SSLLibHandle, 'SSL_CTX_set_default_verify_paths'));
  if Assigned(SetDefaultVerifyPaths) and (SetDefaultVerifyPaths(AContext) <> 1) then
    raise ETransportSecurityError.Create('Failed to load OpenSSL default certificate paths');

  SslCtxSetVerify(AContext, SSL_VERIFY_PEER, TSSLCTXVerifyCallback(nil));

  HostName := EncodeTLSHostName(AHost);
  SetHostName := TSSLSetHostName(GetProcedureAddress(SSLLibHandle,
    'SSL_set1_host'));
  if not Assigned(SetHostName) then
    raise ETransportSecurityError.Create('OpenSSL library does not provide SSL_set1_host; hostname verification unavailable');
  if SetHostName(ASSL, PAnsiChar(@HostName[0])) <> 1 then
    raise ETransportSecurityError.Create('Failed to configure OpenSSL host verification');
end;

function CreateOpenSSLContext: PSSL_CTX;
var
  GetMethod: TSSLMethodGetter;
begin
  GetMethod := TSSLMethodGetter(GetProcedureAddress(SSLLibHandle,
    'TLS_client_method'));
  if not Assigned(GetMethod) then
    GetMethod := TSSLMethodGetter(GetProcedureAddress(SSLLibHandle,
      'TLS_method'));
  if not Assigned(GetMethod) then
    raise ETransportSecurityError.Create('OpenSSL library does not provide a version-flexible TLS client method');

  Result := SslCtxNew(GetMethod());
  if not Assigned(Result) then
    raise ETransportSecurityError.Create('Failed to create OpenSSL context');

  if SslCTXCtrl(Result, SSL_CTRL_SET_MIN_PROTO_VERSION, TLS1_2_VERSION, nil) <= 0 then
  begin
    SslCtxFree(Result);
    raise ETransportSecurityError.Create('Failed to set minimum OpenSSL TLS version');
  end;
end;

{ The OpenSSL backend drives its socket without blocking: after the TCP
  connection it puts the socket in non-blocking mode, and whenever OpenSSL
  wants to read or write it waits with poll for the time left before the
  request's deadline. A blocking socket with a receive timeout bounded each
  receive but not one OpenSSL call, which makes as many receives as a record
  needs, and a timed-out receive came back as SSL_ERROR_WANT_READ, which used
  to be retried for ever. }
procedure SetSocketNonBlocking(const ASocket: TSocket);
var
  Flags: LongInt;
begin
  Flags := FpFcntl(ASocket, F_GETFL);
  if (Flags < 0) or (FpFcntl(ASocket, F_SETFL, Flags or O_NONBLOCK) < 0) then
    raise ETransportSecurityError.Create(
      'Failed to configure the TLS socket');
end;

{ Waits until ASocket is ready for AEvents (POLLIN or POLLOUT), or reports
  False once ADeadlineNanoseconds (0 for none) has passed. An error or hang-up
  on the socket counts as ready: the OpenSSL call made next reports it. }
function WaitForSocket(const ASocket: TSocket; const AEvents: cshort;
  const ADeadlineNanoseconds: Int64): Boolean;
var
  Descriptor: TPollFd;
  Milliseconds: Int64;
  Outcome: cint;
begin
  repeat
    if ADeadlineNanoseconds = 0 then
      Milliseconds := -1
    else
    begin
      Milliseconds := MillisecondsBefore(ADeadlineNanoseconds);
      if Milliseconds = 0 then
        Exit(False);
      if Milliseconds > High(cint) then
        Milliseconds := High(cint);
    end;
    Descriptor.fd := ASocket;
    Descriptor.events := AEvents;
    Descriptor.revents := 0;
    Outcome := FpPoll(@Descriptor, 1, Milliseconds);
    if Outcome > 0 then
      Exit(True);
    if (Outcome < 0) and (FpGetErrNo <> ESysEINTR) then
      raise ETransportSecurityError.Create('TLS socket wait failed');
  until False;
end;

{ OpenSSL's socket BIO writes with plain write(2), so MSG_NOSIGNAL cannot reach
  it, and a write to a peer that reset the connection raises SIGPIPE: the
  handshake, a read (a key update), a write and SSL_shutdown's close_notify
  all write. On Linux each of those calls therefore runs with SIGPIPE blocked
  on the calling thread, and a SIGPIPE it raised is taken off the pending set
  before the mask is restored, so the write fails with EPIPE
  (SSL_ERROR_SYSCALL) and the process, the embedder's own handlers and every
  other thread are left alone. A caller that blocks SIGPIPE itself keeps the
  block, and a SIGPIPE it already held pending; one sent to this thread by
  another party while an unblocked caller's call runs is indistinguishable
  from ours and is taken too. Darwin and the BSDs set SO_NOSIGPIPE on the
  socket instead. }
type
  TSigPipeGuard = record
    Active: Boolean;
    WasBlocked: Boolean;
    PendingBefore: Boolean;
    Previous: TSigSet;
  end;

{$IFDEF LINUX}
type
  { glibc's sigset_t is 128 bytes, FPC's TSigSet 16: sigpending is called
    through libc with room for the larger one, since FPC's own fpSigPending
    passes the kernel a wrong set size and fails with EINVAL. }
  TLibcSigSet = array[0..127] of Byte;

function LibcSigPending(var ASet: TLibcSigSet): cint; cdecl;
  external 'c' name 'sigpending';

function SigPipePending: Boolean;
var
  Pending: TLibcSigSet;
begin
  FillChar(Pending, SizeOf(Pending), 0);
  Result := (LibcSigPending(Pending) = 0) and
    ((Pending[(SIGPIPE - 1) div 8] and (1 shl ((SIGPIPE - 1) mod 8))) <> 0);
end;
{$ENDIF}

procedure BeginSigPipeGuard(out AGuard: TSigPipeGuard);
{$IFDEF LINUX}
var
  Block: TSigSet;
begin
  FillChar(AGuard, SizeOf(AGuard), 0);
  fpSigEmptySet(Block);
  fpSigAddSet(Block, SIGPIPE);
  AGuard.Active := fpSigProcMask(SIG_BLOCK, @Block, @AGuard.Previous) = 0;
  if AGuard.Active then
  begin
    AGuard.WasBlocked := fpSigIsMember(AGuard.Previous, SIGPIPE) = 1;
    { A caller that blocks SIGPIPE itself may already hold one pending; that
      one is its own and stays. }
    if AGuard.WasBlocked then
      AGuard.PendingBefore := SigPipePending;
  end;
end;
{$ELSE}
begin
  FillChar(AGuard, SizeOf(AGuard), 0);
end;
{$ENDIF}

procedure EndSigPipeGuard(const AGuard: TSigPipeGuard);
{$IFDEF LINUX}
var
  Wanted: TSigSet;
  NoWait: TimeSpec;
begin
  if not AGuard.Active then
    Exit;
  { Only a SIGPIPE the caller did not already hold pending is taken: a zero
    timeout takes the thread's pending SIGPIPE, if any, and fails with EAGAIN
    when there is none. Taken even when the caller blocks SIGPIPE, or
    unblocking it would end the process for a write that failed with an
    error. }
  if not AGuard.PendingBefore then
  begin
    fpSigEmptySet(Wanted);
    fpSigAddSet(Wanted, SIGPIPE);
    FillChar(NoWait, SizeOf(NoWait), 0);
    fpSigTimedWait(Wanted, nil, @NoWait);
  end;
  if not AGuard.WasBlocked then
    fpSigProcMask(SIG_SETMASK, @AGuard.Previous, nil);
end;
{$ELSE}
begin
end;
{$ENDIF}

{ The OpenSSL calls that can write, each under the guard, so the window in
  which a SIGPIPE sent to the thread for another reason would be taken is
  the call itself and not the waits between calls. }
function GuardedSslConnect(const ASSL: PSSL): Integer;
var
  Guard: TSigPipeGuard;
begin
  BeginSigPipeGuard(Guard);
  try
    Result := SslConnect(ASSL);
  finally
    EndSigPipeGuard(Guard);
  end;
end;

function GuardedSslRead(const ASSL: PSSL; const ABuffer: Pointer;
  const ALength: Integer): Integer;
var
  Guard: TSigPipeGuard;
begin
  BeginSigPipeGuard(Guard);
  try
    Result := SslRead(ASSL, ABuffer, ALength);
  finally
    EndSigPipeGuard(Guard);
  end;
end;

function GuardedSslWrite(const ASSL: PSSL; const ABuffer: Pointer;
  const ALength: Integer): Integer;
var
  Guard: TSigPipeGuard;
begin
  BeginSigPipeGuard(Guard);
  try
    Result := SslWrite(ASSL, ABuffer, ALength);
  finally
    EndSigPipeGuard(Guard);
  end;
end;

function GuardedSslShutdown(const ASSL: PSSL): Integer;
var
  Guard: TSigPipeGuard;
begin
  BeginSigPipeGuard(Guard);
  try
    Result := SslShutdown(ASSL);
  finally
    EndSigPipeGuard(Guard);
  end;
end;

procedure StartOpenSSL(var AConnection: TTransportSecurityConnection;
  const AHost: string);
var
  Data: TOpenSSLData;
  HostName: TBytes;
  Outcome: Integer;
  Ready: Boolean;
begin
  if not EnsureOpenSSLLoaded then
    raise ETransportSecurityError.Create(OPENSSL_LOAD_ERROR);

  Data := TOpenSSLData.Create;
  Data.Context := nil;
  Data.SSL := nil;
  try
    Data.Context := CreateOpenSSLContext;

    Data.SSL := SslNew(Data.Context);
    if not Assigned(Data.SSL) then
      raise ETransportSecurityError.Create('Failed to create OpenSSL session');

    ConfigureOpenSSLVerification(Data.Context, Data.SSL, AHost);

    HostName := EncodeTLSHostName(AHost);
    SslCtrl(Data.SSL, SSL_CTRL_SET_TLSEXT_HOSTNAME,
      TLSEXT_NAMETYPE_host_name, PAnsiChar(@HostName[0]));

    SslSetFd(Data.SSL, AConnection.Socket);
    { Without auto-retry, SSL_read returns after each record that carries no
      application data (a TLS 1.3 session ticket, say) instead of reading
      on, so a peer that streams such records cannot keep one call running
      past the deadline: every return comes back through WaitForSocket. }
    SslCtrl(Data.SSL, SSL_CTRL_CLEAR_MODE, SSL_MODE_AUTO_RETRY, nil);
    SetSocketNonBlocking(AConnection.Socket);
    repeat
      Outcome := GuardedSslConnect(Data.SSL);
      if Outcome = 1 then
        Break;
      case SslGetError(Data.SSL, Outcome) of
        SSL_ERROR_WANT_READ:
          Ready := WaitForSocket(AConnection.Socket, POLLIN,
            AConnection.DeadlineNanoseconds);
        SSL_ERROR_WANT_WRITE:
          Ready := WaitForSocket(AConnection.Socket, POLLOUT,
            AConnection.DeadlineNanoseconds);
      else
        raise ETransportSecurityError.Create(TLS_HANDSHAKE_ERROR);
      end;
      if not Ready then
        raise ETransportSecurityError.Create(TLS_TIMEOUT_ERROR);
    until False;

    if SSLGetVerifyResult(Data.SSL) <> X509_V_OK then
      raise ETransportSecurityError.Create('OpenSSL certificate verification failed');

    AConnection.BackendData := Data;
    AConnection.Backend := TSB_OPENSSL;
    AConnection.Active := True;
  except
    if Assigned(Data.SSL) then
      SslFree(Data.SSL);
    if Assigned(Data.Context) then
      SslCtxFree(Data.Context);
    Data.Free;
    raise;
  end;
end;

procedure CloseOpenSSL(var AConnection: TTransportSecurityConnection);
var
  Data: TOpenSSLData;
begin
  Data := TOpenSSLData(AConnection.BackendData);
  if Assigned(Data) then
  begin
    if Assigned(Data.SSL) then
    begin
      GuardedSslShutdown(Data.SSL);
      SslFree(Data.SSL);
    end;
    if Assigned(Data.Context) then
      SslCtxFree(Data.Context);
    Data.Free;
  end;
end;

{ OpenSSL asked to be called again once the socket is ready for AEvents.
  Waits for that; past the deadline, reports False with the socket timeout's
  error in errno, as a plain receive that timed out leaves it. }
function AwaitOpenSSLSocket(const AConnection: TTransportSecurityConnection;
  const AEvents: cshort): Boolean;
begin
  Result := WaitForSocket(AConnection.Socket, AEvents,
    AConnection.DeadlineNanoseconds);
  if not Result then
    FpSetErrNo(ESysEAGAIN);
end;

function ReadOpenSSL(var AConnection: TTransportSecurityConnection;
  var ABuffer: array of Byte; const ALength: Integer): Integer;
var
  Data: TOpenSSLData;
  ErrorCode: Integer;
  Events: cshort;
begin
  Data := TOpenSSLData(AConnection.BackendData);
  repeat
    Result := GuardedSslRead(Data.SSL, @ABuffer[0], ALength);
    if Result > 0 then
      Exit;

    ErrorCode := SslGetError(Data.SSL, Result);
    case ErrorCode of
      SSL_ERROR_ZERO_RETURN:
        begin
          Result := 0;
          Exit;
        end;
      SSL_ERROR_WANT_READ,
      SSL_ERROR_WANT_WRITE:
        begin
          if ErrorCode = SSL_ERROR_WANT_READ then
            Events := POLLIN
          else
            Events := POLLOUT;
          if not AwaitOpenSSLSocket(AConnection, Events) then
          begin
            Result := -1;
            Exit;
          end;
        end;
    else
      raise ETransportSecurityError.CreateFmt('%s: %d',
        [TLS_READ_ERROR, ErrorCode]);
    end;
  until False;
end;

function WriteOpenSSL(var AConnection: TTransportSecurityConnection;
  const ABuffer: Pointer; const ALength: Integer): Integer;
var
  Data: TOpenSSLData;
  ErrorCode: Integer;
  Events: cshort;
begin
  Data := TOpenSSLData(AConnection.BackendData);
  repeat
    Result := GuardedSslWrite(Data.SSL, ABuffer, ALength);
    if Result > 0 then
      Exit;

    ErrorCode := SslGetError(Data.SSL, Result);
    case ErrorCode of
      SSL_ERROR_ZERO_RETURN:
        begin
          Result := 0;
          Exit;
        end;
      SSL_ERROR_WANT_READ,
      SSL_ERROR_WANT_WRITE:
        begin
          if ErrorCode = SSL_ERROR_WANT_READ then
            Events := POLLIN
          else
            Events := POLLOUT;
          if not AwaitOpenSSLSocket(AConnection, Events) then
          begin
            Result := -1;
            Exit;
          end;
        end;
    else
      raise ETransportSecurityError.CreateFmt('%s: %d',
        [TLS_WRITE_ERROR, ErrorCode]);
    end;
  until False;
end;
{$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}
type
  SECURITY_STATUS = LongInt;
  SECURITY_INTEGER = Int64;
  PSecurityInteger = ^SECURITY_INTEGER;
  ULONG_PTR = NativeUInt;

  PSecHandle = ^TSecHandle;
  TSecHandle = record
    Lower: ULONG_PTR;
    Upper: ULONG_PTR;
  end;

  PCredHandle = PSecHandle;
  PCtxtHandle = PSecHandle;

  PSecBuffer = ^TSecBuffer;
  TSecBuffer = record
    cbBuffer: LongWord;
    BufferType: LongWord;
    pvBuffer: Pointer;
  end;

  PSecBufferDesc = ^TSecBufferDesc;
  TSecBufferDesc = record
    ulVersion: LongWord;
    cBuffers: LongWord;
    pBuffers: PSecBuffer;
  end;

  PSecPkgContextStreamSizes = ^TSecPkgContextStreamSizes;
  TSecPkgContextStreamSizes = record
    cbHeader: LongWord;
    cbTrailer: LongWord;
    cbMaximumMessage: LongWord;
    cBuffers: LongWord;
    cbBlockSize: LongWord;
  end;

  PSchannelCred = ^TSchannelCred;
  TSchannelCred = record
    dwVersion: LongWord;
    cCreds: LongWord;
    paCred: Pointer;
    hRootStore: Pointer;
    cMappers: LongWord;
    aphMappers: Pointer;
    cSupportedAlgs: LongWord;
    palgSupportedAlgs: Pointer;
    grbitEnabledProtocols: LongWord;
    dwMinimumCipherStrength: LongWord;
    dwMaximumCipherStrength: LongWord;
    dwSessionLifespan: LongWord;
    dwFlags: LongWord;
    dwCredFormat: LongWord;
  end;

  TSChannelData = class
  public
    Socket: TSocket;
    DeadlineNanoseconds: Int64;
    Credential: TSecHandle;
    Context: TSecHandle;
    HasContext: Boolean;
    StreamSizes: TSecPkgContextStreamSizes;
    EncryptedInput: TBytes;
    DecryptedInput: TBytes;
    DecryptedOffset: Integer;
  end;

const
  SECPKG_CRED_OUTBOUND = 2;
  SECBUFFER_VERSION = 0;
  SECBUFFER_EMPTY = 0;
  SECBUFFER_DATA = 1;
  SECBUFFER_TOKEN = 2;
  SECBUFFER_EXTRA = 5;
  SECBUFFER_STREAM_TRAILER = 6;
  SECBUFFER_STREAM_HEADER = 7;
  SECPKG_ATTR_STREAM_SIZES = 4;
  SEC_E_OK = SECURITY_STATUS($00000000);
  SEC_I_CONTINUE_NEEDED = SECURITY_STATUS($00090312);
  SEC_I_CONTEXT_EXPIRED = SECURITY_STATUS($00090317);
  SEC_E_INCOMPLETE_MESSAGE = SECURITY_STATUS($80090318);
  SEC_I_INCOMPLETE_CREDENTIALS = SECURITY_STATUS($00090320);
  SEC_I_RENEGOTIATE = SECURITY_STATUS($00090321);
  ISC_REQ_SEQUENCE_DETECT = $00000008;
  ISC_REQ_REPLAY_DETECT = $00000004;
  ISC_REQ_CONFIDENTIALITY = $00000010;
  ISC_REQ_EXTENDED_ERROR = $00004000;
  ISC_REQ_USE_SUPPLIED_CREDS = $00000080;
  ISC_REQ_ALLOCATE_MEMORY = $00000100;
  ISC_REQ_STREAM = $00008000;
  SCHANNEL_CRED_VERSION = 4;
  SCH_USE_STRONG_CRYPTO = $00400000;
  SCHANNEL_SHUTDOWN = 1;
  SECURITY_NATIVE_DREP = $00000010;
  UNISP_NAME = 'Microsoft Unified Security Protocol Provider';

function AcquireCredentialsHandleW(APrincipal: PWideChar; APackage: PWideChar;
  ACredentialUse: LongWord; ALogonId: Pointer; AAuthData: Pointer;
  AGetKeyFn: Pointer; AGetKeyArgument: Pointer; ACredential: PCredHandle;
  AExpiry: PSecurityInteger): SECURITY_STATUS; stdcall;
  external 'secur32.dll' name 'AcquireCredentialsHandleW';
function InitializeSecurityContextW(ACredential: PCredHandle;
  AContext: PCtxtHandle; ATargetName: PWideChar; AContextRequirements: LongWord;
  AReserved: LongWord; ATargetDataRepresentation: LongWord;
  AInput: PSecBufferDesc; AReservedTwo: LongWord; ANewContext: PCtxtHandle;
  AOutput: PSecBufferDesc; AContextAttributes: PLongWord;
  AExpiry: PSecurityInteger): SECURITY_STATUS; stdcall;
  external 'secur32.dll' name 'InitializeSecurityContextW';
function QueryContextAttributesW(AContext: PCtxtHandle; AAttribute: LongWord;
  ABuffer: Pointer): SECURITY_STATUS; stdcall;
  external 'secur32.dll' name 'QueryContextAttributesW';
function EncryptMessage(AContext: PCtxtHandle; AFQualityOfProtection: LongWord;
  AMessage: PSecBufferDesc; AMessageSequenceNumber: LongWord): SECURITY_STATUS; stdcall;
  external 'secur32.dll' name 'EncryptMessage';
function DecryptMessage(AContext: PCtxtHandle; AMessage: PSecBufferDesc;
  AMessageSequenceNumber: LongWord; AQualityOfProtection: PLongWord): SECURITY_STATUS; stdcall;
  external 'secur32.dll' name 'DecryptMessage';
function ApplyControlToken(AContext: PCtxtHandle; AInput: PSecBufferDesc): SECURITY_STATUS; stdcall;
  external 'secur32.dll' name 'ApplyControlToken';
function FreeContextBuffer(ABuffer: Pointer): SECURITY_STATUS; stdcall;
  external 'secur32.dll' name 'FreeContextBuffer';
function DeleteSecurityContext(AContext: PCtxtHandle): SECURITY_STATUS; stdcall;
  external 'secur32.dll' name 'DeleteSecurityContext';
function FreeCredentialsHandle(ACredential: PCredHandle): SECURITY_STATUS; stdcall;
  external 'secur32.dll' name 'FreeCredentialsHandle';

procedure AppendBytes(var ATarget: TBytes; const ASource: Pointer;
  const ALength: Integer);
var
  PreviousLength: Integer;
begin
  if ALength <= 0 then
    Exit;
  PreviousLength := Length(ATarget);
  SetLength(ATarget, PreviousLength + ALength);
  Move(ASource^, ATarget[PreviousLength], ALength);
end;

function ReceiveIntoBuffer(const AData: TSChannelData;
  var ABuffer: TBytes): Integer;
var
  Temporary: array[0..8191] of Byte;
begin
  Result := SocketReceiveBefore(AData.Socket, @Temporary[0],
    Length(Temporary), AData.DeadlineNanoseconds);
  if Result > 0 then
    AppendBytes(ABuffer, @Temporary[0], Result);
end;

{ Sends the token SChannel produced, by ADeadlineNanoseconds. False when the
  deadline passed or the send failed first; true when there was nothing to
  send. }
function SendSChannelToken(const ASocket: TSocket; const ABuffer: TSecBuffer;
  const ADeadlineNanoseconds: Int64): Boolean;
begin
  if (ABuffer.cbBuffer > 0) and Assigned(ABuffer.pvBuffer) then
    Result := SendSocketAll(ASocket, ABuffer.pvBuffer, ABuffer.cbBuffer,
      ADeadlineNanoseconds)
  else
    Result := True;
end;

function SChannelRequestFlags: LongWord;
begin
  Result := ISC_REQ_SEQUENCE_DETECT or ISC_REQ_REPLAY_DETECT or
    ISC_REQ_CONFIDENTIALITY or ISC_REQ_EXTENDED_ERROR or
    ISC_REQ_USE_SUPPLIED_CREDS or ISC_REQ_ALLOCATE_MEMORY or ISC_REQ_STREAM;
end;

procedure StartSChannel(var AConnection: TTransportSecurityConnection;
  const AHost: string);
var
  Data: TSChannelData;
  Credential: TSchannelCred;
  Status: SECURITY_STATUS;
  Expiry: SECURITY_INTEGER;
  ContextAttributes: LongWord;
  OutputBuffer: TSecBuffer;
  OutputDesc: TSecBufferDesc;
  InputBuffers: array[0..1] of TSecBuffer;
  InputDesc: TSecBufferDesc;
  TargetName: WideString;
  InputDescPointer: PSecBufferDesc;
  ExistingContext: PCtxtHandle;
  ReceiveCount: Integer;
  TokenSent: Boolean;
  ExtraLength: Integer;
begin
  Data := TSChannelData.Create;
  FillChar(Data.Credential, SizeOf(Data.Credential), 0);
  FillChar(Data.Context, SizeOf(Data.Context), 0);
  FillChar(Data.StreamSizes, SizeOf(Data.StreamSizes), 0);
  Data.Socket := AConnection.Socket;
  Data.DeadlineNanoseconds := AConnection.DeadlineNanoseconds;
  Data.HasContext := False;

  FillChar(Credential, SizeOf(Credential), 0);
  Credential.dwVersion := SCHANNEL_CRED_VERSION;
  Credential.dwFlags := SCH_USE_STRONG_CRYPTO;

  Status := AcquireCredentialsHandleW(nil, PWideChar(WideString(UNISP_NAME)),
    SECPKG_CRED_OUTBOUND, nil, @Credential, nil, nil, @Data.Credential,
    @Expiry);
  if Status <> SEC_E_OK then
  begin
    Data.Free;
    raise ETransportSecurityError.CreateFmt('Failed to acquire SChannel credentials: 0x%x',
      [LongWord(Status)]);
  end;

  TargetName := WideString(AHost);
  try
    repeat
      FillChar(OutputBuffer, SizeOf(OutputBuffer), 0);
      OutputBuffer.BufferType := SECBUFFER_TOKEN;
      FillChar(OutputDesc, SizeOf(OutputDesc), 0);
      OutputDesc.ulVersion := SECBUFFER_VERSION;
      OutputDesc.cBuffers := 1;
      OutputDesc.pBuffers := @OutputBuffer;

      InputDescPointer := nil;
      if Length(Data.EncryptedInput) > 0 then
      begin
        FillChar(InputBuffers, SizeOf(InputBuffers), 0);
        InputBuffers[0].BufferType := SECBUFFER_TOKEN;
        InputBuffers[0].cbBuffer := Length(Data.EncryptedInput);
        InputBuffers[0].pvBuffer := @Data.EncryptedInput[0];
        InputBuffers[1].BufferType := SECBUFFER_EMPTY;
        InputDesc.ulVersion := SECBUFFER_VERSION;
        InputDesc.cBuffers := 2;
        InputDesc.pBuffers := @InputBuffers[0];
        InputDescPointer := @InputDesc;
      end;

      if Data.HasContext then
        ExistingContext := @Data.Context
      else
        ExistingContext := nil;

      Status := InitializeSecurityContextW(@Data.Credential, ExistingContext,
        PWideChar(TargetName), SChannelRequestFlags, 0,
        SECURITY_NATIVE_DREP, InputDescPointer, 0, @Data.Context, @OutputDesc,
        @ContextAttributes, @Expiry);
      Data.HasContext := True;

      TokenSent := SendSChannelToken(Data.Socket, OutputBuffer,
        Data.DeadlineNanoseconds);
      if Assigned(OutputBuffer.pvBuffer) then
        FreeContextBuffer(OutputBuffer.pvBuffer);
      if not TokenSent then
        raise ETransportSecurityError.Create(TLS_WRITE_ERROR);

      { A message split across receives consumed nothing: the input stays
        and the next receive is appended to it. Otherwise SChannel consumed
        all of it but the extra bytes, the start of the next message, which
        it left in place at the end of the input. }
      ExtraLength := 0;
      if (InputDescPointer <> nil) and
         (InputBuffers[1].BufferType = SECBUFFER_EXTRA) then
        ExtraLength := Integer(InputBuffers[1].cbBuffer);
      ConsumeEncryptedInput(Data.EncryptedInput,
        Status = SEC_E_INCOMPLETE_MESSAGE, ExtraLength);

      if Status = SEC_E_INCOMPLETE_MESSAGE then
      begin
        ReceiveCount := ReceiveIntoBuffer(Data, Data.EncryptedInput);
        if ReceiveCount < 0 then
          raise ETransportSecurityError.Create(TLS_READ_ERROR);
        if ReceiveCount = 0 then
          raise ETransportSecurityError.Create(TLS_HANDSHAKE_ERROR);
        Continue;
      end;

      if Status = SEC_I_INCOMPLETE_CREDENTIALS then
        raise ETransportSecurityError.Create(TLS_HANDSHAKE_ERROR);

      if Status = SEC_I_CONTINUE_NEEDED then
      begin
        if Length(Data.EncryptedInput) = 0 then
        begin
          ReceiveCount := ReceiveIntoBuffer(Data, Data.EncryptedInput);
          if ReceiveCount < 0 then
            raise ETransportSecurityError.Create(TLS_READ_ERROR);
          if ReceiveCount = 0 then
            raise ETransportSecurityError.Create(TLS_HANDSHAKE_ERROR);
        end;
        Continue;
      end;

      if Status <> SEC_E_OK then
        raise ETransportSecurityError.CreateFmt('%s: 0x%x',
          [TLS_HANDSHAKE_ERROR, LongWord(Status)]);
    until Status = SEC_E_OK;

    Status := QueryContextAttributesW(@Data.Context, SECPKG_ATTR_STREAM_SIZES,
      @Data.StreamSizes);
    if Status <> SEC_E_OK then
      raise ETransportSecurityError.CreateFmt('Failed to query SChannel stream sizes: 0x%x',
        [LongWord(Status)]);

    AConnection.BackendData := Data;
    AConnection.Backend := TSB_SCHANNEL;
    AConnection.Active := True;
  except
    if Data.HasContext then
      DeleteSecurityContext(@Data.Context);
    FreeCredentialsHandle(@Data.Credential);
    Data.Free;
    raise;
  end;
end;

procedure CloseSChannel(var AConnection: TTransportSecurityConnection);
var
  Data: TSChannelData;
  ShutdownToken: LongWord;
  ShutdownBuffer: TSecBuffer;
  ShutdownDesc: TSecBufferDesc;
  InputBuffer: TSecBuffer;
  InputDesc: TSecBufferDesc;
  OutputBuffer: TSecBuffer;
  OutputDesc: TSecBufferDesc;
  Status: SECURITY_STATUS;
  ContextAttributes: LongWord;
  Expiry: SECURITY_INTEGER;
  TokenSent: Boolean;
begin
  Data := TSChannelData(AConnection.BackendData);
  if Assigned(Data) then
  begin
    if Data.HasContext then
    begin
      ShutdownToken := SCHANNEL_SHUTDOWN;
      ShutdownBuffer.cbBuffer := SizeOf(ShutdownToken);
      ShutdownBuffer.BufferType := SECBUFFER_TOKEN;
      ShutdownBuffer.pvBuffer := @ShutdownToken;
      ShutdownDesc.ulVersion := SECBUFFER_VERSION;
      ShutdownDesc.cBuffers := 1;
      ShutdownDesc.pBuffers := @ShutdownBuffer;
      ApplyControlToken(@Data.Context, @ShutdownDesc);

      repeat
        FillChar(InputBuffer, SizeOf(InputBuffer), 0);
        InputBuffer.BufferType := SECBUFFER_EMPTY;
        InputDesc.ulVersion := SECBUFFER_VERSION;
        InputDesc.cBuffers := 1;
        InputDesc.pBuffers := @InputBuffer;

        FillChar(OutputBuffer, SizeOf(OutputBuffer), 0);
        OutputBuffer.BufferType := SECBUFFER_TOKEN;
        FillChar(OutputDesc, SizeOf(OutputDesc), 0);
        OutputDesc.ulVersion := SECBUFFER_VERSION;
        OutputDesc.cBuffers := 1;
        OutputDesc.pBuffers := @OutputBuffer;

        Status := InitializeSecurityContextW(@Data.Credential, @Data.Context,
          nil, SChannelRequestFlags, 0, SECURITY_NATIVE_DREP, @InputDesc, 0,
          @Data.Context, @OutputDesc, @ContextAttributes, @Expiry);

        { The close_notify is a courtesy: one the deadline or a broken
          connection stops is dropped, and the close goes on to release the
          context. }
        TokenSent := SendSChannelToken(Data.Socket, OutputBuffer,
          Data.DeadlineNanoseconds);
        if Assigned(OutputBuffer.pvBuffer) then
          FreeContextBuffer(OutputBuffer.pvBuffer);
      until not TokenSent or (Status = SEC_E_OK) or
            (Status = SEC_I_CONTEXT_EXPIRED) or
            (Status <> SEC_I_CONTINUE_NEEDED);

      DeleteSecurityContext(@Data.Context);
    end;
    FreeCredentialsHandle(@Data.Credential);
    Data.Free;
  end;
end;

function ReadSChannel(var AConnection: TTransportSecurityConnection;
  var ABuffer: array of Byte; const ALength: Integer): Integer;
var
  Data: TSChannelData;
  Available: Integer;
  Buffers: array[0..3] of TSecBuffer;
  BufferDesc: TSecBufferDesc;
  Status: SECURITY_STATUS;
  QualityOfProtection: LongWord;
  I: Integer;
  ReceiveCount: Integer;
  ExtraLength: Integer;
begin
  Data := TSChannelData(AConnection.BackendData);

  Available := Length(Data.DecryptedInput) - Data.DecryptedOffset;
  if Available > 0 then
  begin
    Result := Min(Available, ALength);
    Move(Data.DecryptedInput[Data.DecryptedOffset], ABuffer[0], Result);
    Inc(Data.DecryptedOffset, Result);
    if Data.DecryptedOffset >= Length(Data.DecryptedInput) then
    begin
      SetLength(Data.DecryptedInput, 0);
      Data.DecryptedOffset := 0;
    end;
    Exit;
  end;

  while True do
  begin
    if Length(Data.EncryptedInput) = 0 then
    begin
      ReceiveCount := ReceiveIntoBuffer(Data, Data.EncryptedInput);
      if ReceiveCount < 0 then
        raise ETransportSecurityError.Create(TLS_READ_ERROR);
      if ReceiveCount = 0 then
      begin
        Result := 0;
        Exit;
      end;
    end;

    FillChar(Buffers, SizeOf(Buffers), 0);
    Buffers[0].BufferType := SECBUFFER_DATA;
    Buffers[0].cbBuffer := Length(Data.EncryptedInput);
    Buffers[0].pvBuffer := @Data.EncryptedInput[0];
    Buffers[1].BufferType := SECBUFFER_EMPTY;
    Buffers[2].BufferType := SECBUFFER_EMPTY;
    Buffers[3].BufferType := SECBUFFER_EMPTY;
    BufferDesc.ulVersion := SECBUFFER_VERSION;
    BufferDesc.cBuffers := 4;
    BufferDesc.pBuffers := @Buffers[0];
    QualityOfProtection := 0;

    Status := DecryptMessage(@Data.Context, @BufferDesc, 0,
      @QualityOfProtection);
    if Status = SEC_E_INCOMPLETE_MESSAGE then
    begin
      ReceiveCount := ReceiveIntoBuffer(Data, Data.EncryptedInput);
      if ReceiveCount < 0 then
        raise ETransportSecurityError.Create(TLS_READ_ERROR);
      if ReceiveCount = 0 then
      begin
        Result := 0;
        Exit;
      end;
      Continue;
    end;
    if Status = SEC_I_CONTEXT_EXPIRED then
    begin
      Result := 0;
      Exit;
    end;
    if Status = SEC_I_RENEGOTIATE then
      raise ETransportSecurityError.Create('SChannel renegotiation is not supported');
    if Status <> SEC_E_OK then
      raise ETransportSecurityError.CreateFmt('%s: 0x%x',
        [TLS_READ_ERROR, LongWord(Status)]);

    SetLength(Data.DecryptedInput, 0);
    Data.DecryptedOffset := 0;
    for I := 0 to High(Buffers) do
      if Buffers[I].BufferType = SECBUFFER_DATA then
        AppendBytes(Data.DecryptedInput, Buffers[I].pvBuffer,
          Buffers[I].cbBuffer);

    { DecryptMessage decrypted in place: the extra buffer points into
      EncryptedInput, so the bytes move to its front while it is allocated. }
    ExtraLength := 0;
    for I := 0 to High(Buffers) do
      if Buffers[I].BufferType = SECBUFFER_EXTRA then
        ExtraLength := Integer(Buffers[I].cbBuffer);
    ConsumeEncryptedInput(Data.EncryptedInput, False, ExtraLength);

    Available := Length(Data.DecryptedInput);
    if Available > 0 then
    begin
      Result := Min(Available, ALength);
      Move(Data.DecryptedInput[0], ABuffer[0], Result);
      Data.DecryptedOffset := Result;
      Exit;
    end;
  end;
end;

function WriteSChannel(var AConnection: TTransportSecurityConnection;
  const ABuffer: Pointer; const ALength: Integer): Integer;
var
  Data: TSChannelData;
  ChunkLength: Integer;
  PlainOffset: Integer;
  Message: TBytes;
  Buffers: array[0..3] of TSecBuffer;
  BufferDesc: TSecBufferDesc;
  Status: SECURITY_STATUS;
  TotalLength: Integer;
begin
  Data := TSChannelData(AConnection.BackendData);
  Result := 0;
  PlainOffset := 0;
  while PlainOffset < ALength do
  begin
    ChunkLength := Min(ALength - PlainOffset,
      Integer(Data.StreamSizes.cbMaximumMessage));
    TotalLength := Data.StreamSizes.cbHeader + ChunkLength +
      Data.StreamSizes.cbTrailer;
    SetLength(Message, TotalLength);
    Move(Pointer(NativeUInt(ABuffer) + NativeUInt(PlainOffset))^,
      Message[Data.StreamSizes.cbHeader], ChunkLength);

    FillChar(Buffers, SizeOf(Buffers), 0);
    Buffers[0].BufferType := SECBUFFER_STREAM_HEADER;
    Buffers[0].cbBuffer := Data.StreamSizes.cbHeader;
    Buffers[0].pvBuffer := @Message[0];
    Buffers[1].BufferType := SECBUFFER_DATA;
    Buffers[1].cbBuffer := ChunkLength;
    Buffers[1].pvBuffer := @Message[Data.StreamSizes.cbHeader];
    Buffers[2].BufferType := SECBUFFER_STREAM_TRAILER;
    Buffers[2].cbBuffer := Data.StreamSizes.cbTrailer;
    Buffers[2].pvBuffer := @Message[Data.StreamSizes.cbHeader + ChunkLength];
    Buffers[3].BufferType := SECBUFFER_EMPTY;
    BufferDesc.ulVersion := SECBUFFER_VERSION;
    BufferDesc.cBuffers := 4;
    BufferDesc.pBuffers := @Buffers[0];

    Status := EncryptMessage(@Data.Context, 0, @BufferDesc, 0);
    if Status <> SEC_E_OK then
      raise ETransportSecurityError.CreateFmt('%s: 0x%x',
        [TLS_WRITE_ERROR, LongWord(Status)]);

    TotalLength := Buffers[0].cbBuffer + Buffers[1].cbBuffer +
      Buffers[2].cbBuffer;
    if not SendSocketAll(Data.Socket, @Message[0], TotalLength,
        Data.DeadlineNanoseconds) then
      raise ETransportSecurityError.Create(TLS_WRITE_ERROR);
    Inc(PlainOffset, ChunkLength);
    Inc(Result, ChunkLength);
  end;
end;
{$ENDIF}

procedure StartTransportSecurity(var AConnection: TTransportSecurityConnection;
  const ASocket: TSocket; const AHost: string;
  const ADeadlineNanoseconds: Int64);
begin
  FillChar(AConnection, SizeOf(AConnection), 0);
  AConnection.Socket := ASocket;
  AConnection.DeadlineNanoseconds := ADeadlineNanoseconds;
  AConnection.Backend := TSB_NONE;

  {$IFDEF DARWIN}
  StartSecureTransport(AConnection, AHost);
  {$ELSE}
  {$IFDEF MSWINDOWS}
  StartSChannel(AConnection, AHost);
  {$ELSE}
  StartOpenSSL(AConnection, AHost);
  {$ENDIF}
  {$ENDIF}
end;

procedure CloseTransportSecurity(var AConnection: TTransportSecurityConnection);
begin
  if not AConnection.Active then
    Exit;

  case AConnection.Backend of
    {$IFDEF DARWIN}
    TSB_SECURE_TRANSPORT:
      CloseSecureTransport(AConnection);
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    TSB_SCHANNEL:
      CloseSChannel(AConnection);
    {$ENDIF}
    {$IFDEF UNIX}
    {$IFNDEF DARWIN}
    TSB_OPENSSL:
      CloseOpenSSL(AConnection);
    {$ENDIF}
    {$ENDIF}
  end;

  AConnection.Active := False;
  AConnection.Backend := TSB_NONE;
  AConnection.BackendData := nil;
end;

function TransportSecurityRead(var AConnection: TTransportSecurityConnection;
  var ABuffer: array of Byte; const ALength: Integer): Integer;
begin
  if ALength <= 0 then
  begin
    Result := 0;
    Exit;
  end;

  case AConnection.Backend of
    {$IFDEF DARWIN}
    TSB_SECURE_TRANSPORT:
      Result := ReadSecureTransport(AConnection, ABuffer, ALength);
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    TSB_SCHANNEL:
      Result := ReadSChannel(AConnection, ABuffer, ALength);
    {$ENDIF}
    {$IFDEF UNIX}
    {$IFNDEF DARWIN}
    TSB_OPENSSL:
      Result := ReadOpenSSL(AConnection, ABuffer, ALength);
    {$ENDIF}
    {$ENDIF}
  else
    Result := 0;
  end;
end;

function TransportSecurityWrite(var AConnection: TTransportSecurityConnection;
  const ABuffer: Pointer; const ALength: Integer): Integer;
begin
  if ALength <= 0 then
  begin
    Result := 0;
    Exit;
  end;

  case AConnection.Backend of
    {$IFDEF DARWIN}
    TSB_SECURE_TRANSPORT:
      Result := WriteSecureTransport(AConnection, ABuffer, ALength);
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    TSB_SCHANNEL:
      Result := WriteSChannel(AConnection, ABuffer, ALength);
    {$ENDIF}
    {$IFDEF UNIX}
    {$IFNDEF DARWIN}
    TSB_OPENSSL:
      Result := WriteOpenSSL(AConnection, ABuffer, ALength);
    {$ENDIF}
    {$ENDIF}
  else
    Result := 0;
  end;
end;

{$ENDIF}

{$IFNDEF LAKON}
{$IFDEF UNIX}
{$IFNDEF DARWIN}
initialization
  CriticalSectionInit(GOpenSSLLoadLock);

finalization
  CriticalSectionDone(GOpenSSLLoadLock);
{$ENDIF}
{$ENDIF}
{$ENDIF}

end.
