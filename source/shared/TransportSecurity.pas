unit TransportSecurity;

// Cross-platform TLS transport for blocking sockets.
// macOS uses SecureTransport, Windows uses SChannel, Unix uses OpenSSL.

{$I Shared.inc}

interface

uses
  SysUtils,
  {$IFDEF UNIX}
  Sockets
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  WinSock2
  {$ENDIF}
  ;

type
  TTransportSecurityConnection = record
  public
    Active: Boolean;
  private
    Backend: Integer;
    Socket: TSocket;
    BackendData: Pointer;
  end;

procedure StartTransportSecurity(var AConnection: TTransportSecurityConnection;
  const ASocket: TSocket; const AHost: string);
procedure CloseTransportSecurity(var AConnection: TTransportSecurityConnection);
function TransportSecurityRead(var AConnection: TTransportSecurityConnection;
  var ABuffer: array of Byte; const ALength: Integer): Integer;
function TransportSecurityWrite(var AConnection: TTransportSecurityConnection;
  const ABuffer: Pointer; const ALength: Integer): Integer;

implementation

uses
  {$IFDEF UNIX}
  BaseUnix,
  {$IFNDEF DARWIN}
  ctypes,
  DynLibs,
  OpenSSL,
  {$ENDIF}
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Math;

const
  TSB_NONE = 0;
  TSB_OPENSSL = 1;
  TSB_SECURE_TRANSPORT = 2;
  TSB_SCHANNEL = 3;
  OPENSSL_LOAD_ERROR = 'HTTPS requires OpenSSL but it could not be loaded';
  TLS_HANDSHAKE_ERROR = 'TLS handshake failed';
  TLS_READ_ERROR = 'TLS read failed';
  TLS_WRITE_ERROR = 'TLS write failed';

type
  ETransportSecurityError = class(Exception);

function SocketSend(const ASock: TSocket; const ABuffer: Pointer;
  const ALength: Integer): Integer; inline;
begin
  {$IFDEF UNIX}
  Result := fpSend(ASock, ABuffer, ALength, 0);
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Result := WinSock2.send(ASock, ABuffer^, ALength, 0);
  {$ENDIF}
end;

function SocketReceive(const ASock: TSocket; const ABuffer: Pointer;
  const ALength: Integer): Integer; inline;
begin
  {$IFDEF UNIX}
  Result := fpRecv(ASock, ABuffer, ALength, 0);
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Result := WinSock2.recv(ASock, ABuffer^, ALength, 0);
  {$ENDIF}
end;

procedure SendSocketAll(const ASocket: TSocket; const ABuffer: Pointer;
  const ALength: Integer);
var
  Sent: Integer;
  Written: Integer;
begin
  Sent := 0;
  while Sent < ALength do
  begin
    Written := SocketSend(ASocket, Pointer(PtrUInt(ABuffer) + PtrUInt(Sent)),
      ALength - Sent);
    if Written <= 0 then
      raise ETransportSecurityError.Create(TLS_WRITE_ERROR);
    Inc(Sent, Written);
  end;
end;

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
  end;

  TSecureTransportReadFunc = function(AConnection: SSLConnectionRef;
    AData: Pointer; var ADataLength: PtrUInt): OSStatus; cdecl;
  TSecureTransportWriteFunc = function(AConnection: SSLConnectionRef;
    AData: Pointer; var ADataLength: PtrUInt): OSStatus; cdecl;

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
  APeerNameLength: PtrUInt): OSStatus; cdecl;
  external name 'SSLSetPeerDomainName';
function SSLSetProtocolVersionMin(AContext: SSLContextRef;
  AVersion: SSLProtocol): OSStatus; cdecl;
  external name 'SSLSetProtocolVersionMin';
function SSLHandshake(AContext: SSLContextRef): OSStatus; cdecl;
  external name 'SSLHandshake';
function SSLRead(AContext: SSLContextRef; AData: Pointer;
  ADataLength: PtrUInt; var AProcessed: PtrUInt): OSStatus; cdecl;
  external name 'SSLRead';
function SSLWrite(AContext: SSLContextRef; AData: Pointer;
  ADataLength: PtrUInt; var AProcessed: PtrUInt): OSStatus; cdecl;
  external name 'SSLWrite';
function SSLClose(AContext: SSLContextRef): OSStatus; cdecl;
  external name 'SSLClose';
procedure CFRelease(ARef: Pointer); cdecl; external name 'CFRelease';

function SecureTransportSocketRead(AConnection: SSLConnectionRef;
  AData: Pointer; var ADataLength: PtrUInt): OSStatus; cdecl;
var
  Data: TSecureTransportData;
  RequestedLength: PtrUInt;
  ReadCount: Integer;
begin
  Data := TSecureTransportData(AConnection);
  RequestedLength := ADataLength;
  ReadCount := SocketReceive(Data.Socket, AData, ADataLength);
  if ReadCount > 0 then
  begin
    ADataLength := ReadCount;
    if PtrUInt(ReadCount) = RequestedLength then
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
  AData: Pointer; var ADataLength: PtrUInt): OSStatus; cdecl;
var
  Data: TSecureTransportData;
  RequestedLength: PtrUInt;
  Written: Integer;
begin
  Data := TSecureTransportData(AConnection);
  RequestedLength := ADataLength;
  Written := SocketSend(Data.Socket, AData, ADataLength);
  if Written > 0 then
  begin
    ADataLength := Written;
    if PtrUInt(Written) = RequestedLength then
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
  HostName: AnsiString;
  Status: OSStatus;
begin
  Data := TSecureTransportData.Create;
  Data.Socket := AConnection.Socket;
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

    HostName := AnsiString(AHost);
    Status := SSLSetPeerDomainName(Data.Context, PAnsiChar(HostName),
      Length(HostName));
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
  Processed: PtrUInt;
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
  Processed: PtrUInt;
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
    DLLSSLName := SSLBase;
    DLLUtilName := CryptoBase;
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

procedure ConfigureOpenSSLVerification(const AContext: PSSL_CTX;
  const ASSL: PSSL; const AHost: string);
var
  SetDefaultVerifyPaths: TSSLSetDefaultVerifyPaths;
  SetHostName: TSSLSetHostName;
  HostName: AnsiString;
begin
  SetDefaultVerifyPaths := TSSLSetDefaultVerifyPaths(GetProcedureAddress(
    SSLLibHandle, 'SSL_CTX_set_default_verify_paths'));
  if Assigned(SetDefaultVerifyPaths) and (SetDefaultVerifyPaths(AContext) <> 1) then
    raise ETransportSecurityError.Create('Failed to load OpenSSL default certificate paths');

  SslCtxSetVerify(AContext, SSL_VERIFY_PEER, TSSLCTXVerifyCallback(nil));

  HostName := AnsiString(AHost);
  SetHostName := TSSLSetHostName(GetProcedureAddress(SSLLibHandle,
    'SSL_set1_host'));
  if not Assigned(SetHostName) then
    raise ETransportSecurityError.Create('OpenSSL library does not provide SSL_set1_host; hostname verification unavailable');
  if SetHostName(ASSL, PAnsiChar(HostName)) <> 1 then
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

procedure StartOpenSSL(var AConnection: TTransportSecurityConnection;
  const AHost: string);
var
  Data: TOpenSSLData;
begin
  if not IsSSLloaded then
  begin
    ConfigureOpenSSLLoading;
    if not InitSSLInterface then
      raise ETransportSecurityError.Create(OPENSSL_LOAD_ERROR);
  end;

  Data := TOpenSSLData.Create;
  Data.Context := nil;
  Data.SSL := nil;
  try
    Data.Context := CreateOpenSSLContext;

    Data.SSL := SslNew(Data.Context);
    if not Assigned(Data.SSL) then
      raise ETransportSecurityError.Create('Failed to create OpenSSL session');

    ConfigureOpenSSLVerification(Data.Context, Data.SSL, AHost);

    SslCtrl(Data.SSL, SSL_CTRL_SET_TLSEXT_HOSTNAME,
      TLSEXT_NAMETYPE_host_name, PAnsiChar(AnsiString(AHost)));

    SslSetFd(Data.SSL, AConnection.Socket);
    if SslConnect(Data.SSL) <= 0 then
      raise ETransportSecurityError.Create(TLS_HANDSHAKE_ERROR);

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
      SslShutdown(Data.SSL);
      SslFree(Data.SSL);
    end;
    if Assigned(Data.Context) then
      SslCtxFree(Data.Context);
    Data.Free;
  end;
end;

function ReadOpenSSL(var AConnection: TTransportSecurityConnection;
  var ABuffer: array of Byte; const ALength: Integer): Integer;
var
  Data: TOpenSSLData;
  ErrorCode: Integer;
begin
  Data := TOpenSSLData(AConnection.BackendData);
  repeat
    Result := SslRead(Data.SSL, @ABuffer[0], ALength);
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
        Continue;
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
begin
  Data := TOpenSSLData(AConnection.BackendData);
  repeat
    Result := SslWrite(Data.SSL, ABuffer, ALength);
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
        Continue;
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
  ULONG_PTR = PtrUInt;

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

procedure PreserveExtraBytes(var ATarget: TBytes; const ASource: Pointer;
  const ALength: Integer);
begin
  SetLength(ATarget, 0);
  AppendBytes(ATarget, ASource, ALength);
end;

function ReceiveIntoBuffer(const ASocket: TSocket; var ABuffer: TBytes): Integer;
var
  Temporary: array[0..8191] of Byte;
begin
  Result := SocketReceive(ASocket, @Temporary[0], Length(Temporary));
  if Result > 0 then
    AppendBytes(ABuffer, @Temporary[0], Result);
end;

procedure SendSChannelToken(const ASocket: TSocket; const ABuffer: TSecBuffer);
begin
  if (ABuffer.cbBuffer > 0) and Assigned(ABuffer.pvBuffer) then
    SendSocketAll(ASocket, ABuffer.pvBuffer, ABuffer.cbBuffer);
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
begin
  Data := TSChannelData.Create;
  FillChar(Data.Credential, SizeOf(Data.Credential), 0);
  FillChar(Data.Context, SizeOf(Data.Context), 0);
  FillChar(Data.StreamSizes, SizeOf(Data.StreamSizes), 0);
  Data.Socket := AConnection.Socket;
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
        PWideChar(TargetName), ISC_REQ_SEQUENCE_DETECT or ISC_REQ_REPLAY_DETECT or
        ISC_REQ_CONFIDENTIALITY or ISC_REQ_EXTENDED_ERROR or
        ISC_REQ_ALLOCATE_MEMORY or ISC_REQ_STREAM, 0, SECURITY_NATIVE_DREP,
        InputDescPointer, 0, @Data.Context, @OutputDesc, @ContextAttributes,
        @Expiry);
      Data.HasContext := True;

      SendSChannelToken(Data.Socket, OutputBuffer);
      if Assigned(OutputBuffer.pvBuffer) then
        FreeContextBuffer(OutputBuffer.pvBuffer);

      if (InputDescPointer <> nil) and (InputBuffers[1].BufferType = SECBUFFER_EXTRA) then
        PreserveExtraBytes(Data.EncryptedInput, InputBuffers[1].pvBuffer,
          InputBuffers[1].cbBuffer)
      else
        SetLength(Data.EncryptedInput, 0);

      if Status = SEC_E_INCOMPLETE_MESSAGE then
      begin
        ReceiveCount := ReceiveIntoBuffer(Data.Socket, Data.EncryptedInput);
        if ReceiveCount < 0 then
          raise ETransportSecurityError.Create(TLS_READ_ERROR);
        if ReceiveCount = 0 then
          raise ETransportSecurityError.Create(TLS_HANDSHAKE_ERROR);
        Continue;
      end;

      if (Status = SEC_I_CONTINUE_NEEDED) or
         (Status = SEC_I_INCOMPLETE_CREDENTIALS) then
      begin
        if Length(Data.EncryptedInput) = 0 then
        begin
          ReceiveCount := ReceiveIntoBuffer(Data.Socket, Data.EncryptedInput);
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
      ReceiveCount := ReceiveIntoBuffer(Data.Socket, Data.EncryptedInput);
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
      ReceiveCount := ReceiveIntoBuffer(Data.Socket, Data.EncryptedInput);
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

    SetLength(Data.EncryptedInput, 0);
    for I := 0 to High(Buffers) do
      if Buffers[I].BufferType = SECBUFFER_EXTRA then
        PreserveExtraBytes(Data.EncryptedInput, Buffers[I].pvBuffer,
          Buffers[I].cbBuffer);

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
    Move(Pointer(PtrUInt(ABuffer) + PtrUInt(PlainOffset))^,
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
    SendSocketAll(Data.Socket, @Message[0], TotalLength);
    Inc(PlainOffset, ChunkLength);
    Inc(Result, ChunkLength);
  end;
end;
{$ENDIF}

procedure StartTransportSecurity(var AConnection: TTransportSecurityConnection;
  const ASocket: TSocket; const AHost: string);
begin
  FillChar(AConnection, SizeOf(AConnection), 0);
  AConnection.Socket := ASocket;
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

end.
