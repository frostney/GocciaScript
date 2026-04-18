unit HTTPClient;

// Minimal HTTP/1.1 client built on raw BSD sockets.
// Supports GET and HEAD over HTTP and HTTPS (via OpenSSL).
// Cross-platform: Unix (macOS, Linux) and Windows.
// Designed for synchronous use today with a path to non-blocking I/O later.

{$I Shared.inc}

interface

uses
  SysUtils;

type
  THTTPHeader = record
    Name: string;
    Value: string;
  end;

  THTTPHeaders = array of THTTPHeader;

  THTTPResponse = record
    StatusCode: Integer;
    StatusText: string;
    Headers: THTTPHeaders;
    Body: TBytes;
    FinalURL: string;
    Redirected: Boolean;
  end;

  EHTTPError = class(Exception);

function HTTPGet(const AURL: string;
  const AHeaders: THTTPHeaders): THTTPResponse;
function HTTPHead(const AURL: string;
  const AHeaders: THTTPHeaders): THTTPResponse;

implementation

uses
  {$IFDEF UNIX}
  Sockets, BaseUnix, NetDB,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  WinSock2,
  {$ENDIF}
  OpenSSL;

const
  MAX_REDIRECTS   = 20;
  CRLF            = #13#10;
  RECV_BUF_SIZE   = 8192;

type
  THTTPParsedURL = record
    Scheme: string;
    Host: string;
    Port: Integer;
    Path: string;
  end;

{$IFDEF MSWINDOWS}
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
// Minimal URL parsing (self-contained, no engine dependencies)
// ---------------------------------------------------------------------------

function ParseHTTPURL(const AURL: string): THTTPParsedURL;
var
  S, Rest: string;
  I: Integer;
begin
  Result.Scheme := '';
  Result.Host := '';
  Result.Port := 0;
  Result.Path := '/';

  S := AURL;

  // Scheme
  I := Pos('://', S);
  if I > 0 then
  begin
    Result.Scheme := LowerCase(Copy(S, 1, I - 1));
    Rest := Copy(S, I + 3, Length(S));
  end
  else
    raise EHTTPError.Create('Invalid URL: missing scheme');

  if (Result.Scheme <> 'http') and (Result.Scheme <> 'https') then
    raise EHTTPError.Create('Unsupported scheme: ' + Result.Scheme);

  // Split host from path
  I := Pos('/', Rest);
  if I > 0 then
  begin
    Result.Path := Copy(Rest, I, Length(Rest));
    Rest := Copy(Rest, 1, I - 1);
  end;

  // Strip userinfo if present
  I := Pos('@', Rest);
  if I > 0 then
    Rest := Copy(Rest, I + 1, Length(Rest));

  // Parse host:port
  if (Length(Rest) > 0) and (Rest[1] = '[') then
  begin
    // IPv6
    I := Pos(']', Rest);
    if I > 0 then
    begin
      Result.Host := Copy(Rest, 1, I);
      Rest := Copy(Rest, I + 1, Length(Rest));
      if (Length(Rest) > 0) and (Rest[1] = ':') then
        Result.Port := StrToIntDef(Copy(Rest, 2, Length(Rest)), 0);
    end
    else
      Result.Host := Rest;
  end
  else
  begin
    I := Pos(':', Rest);
    if I > 0 then
    begin
      Result.Host := Copy(Rest, 1, I - 1);
      Result.Port := StrToIntDef(Copy(Rest, I + 1, Length(Rest)), 0);
    end
    else
      Result.Host := Rest;
  end;

  if Result.Host = '' then
    raise EHTTPError.Create('Invalid URL: empty host');

  // Default ports
  if Result.Port = 0 then
  begin
    if Result.Scheme = 'https' then
      Result.Port := 443
    else
      Result.Port := 80;
  end;

  if Result.Path = '' then
    Result.Path := '/';
end;

// ---------------------------------------------------------------------------
// Socket connect (cross-platform)
// ---------------------------------------------------------------------------

{$IFDEF UNIX}
function ConnectSocket(const AHost: string; const APort: Integer): TSocket;
var
  SockAddr: TInetSockAddr;
  HostEntry: THostEntry;
  Addr: in_addr;
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

  if fpConnect(Result, @SockAddr, SizeOf(SockAddr)) <> 0 then
  begin
    CloseSocket(Result);
    raise EHTTPError.CreateFmt('Failed to connect to %s:%d', [AHost, APort]);
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function ConnectSocket(const AHost: string; const APort: Integer): TSocket;
var
  Hints, Res, Cur: PAddrInfo;
  PortStr: AnsiString;
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
    PortStr := AnsiString(IntToStr(APort));
    Res := nil;

    if getaddrinfo(PAnsiChar(AnsiString(AHost)), PAnsiChar(PortStr),
                   Hints, @Res) <> 0 then
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

      if WinSock2.connect(Sock, Cur^.ai_addr, Cur^.ai_addrlen) = 0 then
        Break;

      WinSock2.closesocket(Sock);
      Sock := INVALID_SOCKET;
      Cur := Cur^.ai_next;
    end;

    if Sock = INVALID_SOCKET then
      raise EHTTPError.CreateFmt('Failed to connect to %s:%d', [AHost, APort]);

    Result := Sock;
  finally
    freeaddrinfo(Res);
  end;
end;
{$ENDIF}

// ---------------------------------------------------------------------------
// Platform-neutral socket I/O wrappers
// ---------------------------------------------------------------------------

function SocketSend(const ASock: TSocket; const ABuf: Pointer;
  const ALen: Integer): Integer; inline;
begin
  {$IFDEF UNIX}
  Result := fpSend(ASock, ABuf, ALen, 0);
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Result := WinSock2.send(ASock, ABuf^, ALen, 0);
  {$ENDIF}
end;

function SocketRecv(const ASock: TSocket; const ABuf: Pointer;
  const ALen: Integer): Integer; inline;
begin
  {$IFDEF UNIX}
  Result := fpRecv(ASock, ABuf, ALen, 0);
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Result := WinSock2.recv(ASock, ABuf^, ALen, 0);
  {$ENDIF}
end;

procedure SocketClose(const ASock: TSocket); inline;
begin
  {$IFDEF UNIX}
  CloseSocket(ASock);
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  WinSock2.closesocket(ASock);
  {$ENDIF}
end;

// ---------------------------------------------------------------------------
// SSL wrapper
// ---------------------------------------------------------------------------

type
  TSSLConnection = record
    Context: PSSL_CTX;
    SSL: PSSL;
    Active: Boolean;
  end;

function InitSSL(const ASock: TSocket; const AHost: string): TSSLConnection;
begin
  Result.Active := False;
  Result.Context := nil;
  Result.SSL := nil;

  if not IsSSLloaded then
  begin
    if not InitSSLInterface then
      raise EHTTPError.Create('HTTPS requires OpenSSL but it could not be loaded');
  end;

  Result.Context := SslCtxNew(SslMethodTLSV1_2);
  if not Assigned(Result.Context) then
    raise EHTTPError.Create('Failed to create SSL context');

  Result.SSL := SslNew(Result.Context);
  if not Assigned(Result.SSL) then
  begin
    SslCtxFree(Result.Context);
    raise EHTTPError.Create('Failed to create SSL session');
  end;

  // Set SNI hostname via SSL_ctrl
  SslCtrl(Result.SSL, SSL_CTRL_SET_TLSEXT_HOSTNAME,
    TLSEXT_NAMETYPE_host_name, PAnsiChar(AnsiString(AHost)));

  SslSetFd(Result.SSL, ASock);
  if SslConnect(Result.SSL) <= 0 then
  begin
    SslFree(Result.SSL);
    SslCtxFree(Result.Context);
    raise EHTTPError.Create('SSL handshake failed');
  end;

  Result.Active := True;
end;

procedure FreeSSL(var AConn: TSSLConnection);
begin
  if AConn.Active then
  begin
    SslShutdown(AConn.SSL);
    SslFree(AConn.SSL);
    SslCtxFree(AConn.Context);
    AConn.Active := False;
  end;
end;

// ---------------------------------------------------------------------------
// Send / Receive wrappers (unified SSL + plain)
// ---------------------------------------------------------------------------

procedure SendAll(const ASock: TSocket; const ASSL: TSSLConnection;
  const AData: AnsiString);
var
  Sent, Total, Len, N: Integer;
begin
  Total := Length(AData);
  Sent := 0;
  while Sent < Total do
  begin
    Len := Total - Sent;
    if ASSL.Active then
      N := SslWrite(ASSL.SSL, @AData[Sent + 1], Len)
    else
      N := SocketSend(ASock, @AData[Sent + 1], Len);
    if N <= 0 then
      raise EHTTPError.Create('Send failed');
    Inc(Sent, N);
  end;
end;

function RecvBytes(const ASock: TSocket; const ASSL: TSSLConnection;
  var ABuf: array of Byte; const ALen: Integer): Integer;
begin
  if ASSL.Active then
    Result := SslRead(ASSL.SSL, @ABuf[0], ALen)
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

function ReadResponse(const ASock: TSocket; const ASSL: TSSLConnection;
  const AIsHead: Boolean): TRawHTTPResponse;
var
  Buf: array[0..RECV_BUF_SIZE - 1] of Byte;
  RawHeader: AnsiString;
  N, HeaderEnd, I, J, ContentLen, ChunkSize: Integer;
  Line, HeaderBlock: string;
  Lines: array of string;
  ColonPos: Integer;
  TransferEncoding: string;
  BodyBytes: TBytes;
  BodyLen: Integer;
  ChunkBuf: AnsiString;
  Done: Boolean;
  Remaining: Integer;
begin
  Result.StatusCode := 0;
  Result.StatusText := '';
  SetLength(Result.Headers, 0);
  SetLength(Result.Body, 0);

  // Read until we find the end of headers (CRLFCRLF)
  RawHeader := '';
  HeaderEnd := 0;
  repeat
    N := RecvBytes(ASock, ASSL, Buf, RECV_BUF_SIZE);
    if N <= 0 then Break;
    RawHeader := RawHeader + Copy(PAnsiChar(@Buf[0]), 1, N);
    HeaderEnd := Pos(CRLF + CRLF, string(RawHeader));
  until HeaderEnd > 0;

  if HeaderEnd = 0 then
    raise EHTTPError.Create('Invalid HTTP response: no header terminator');

  // Split headers from any body bytes already received
  HeaderBlock := Copy(string(RawHeader), 1, HeaderEnd - 1);
  I := HeaderEnd + 4; // skip CRLFCRLF
  if I <= Length(RawHeader) then
  begin
    SetLength(BodyBytes, Length(RawHeader) - I + 1);
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
    ChunkBuf := AnsiString(BodyBytes);
    SetLength(Result.Body, 0);
    Done := False;

    while not Done do
    begin
      while Pos(CRLF, string(ChunkBuf)) = 0 do
      begin
        N := RecvBytes(ASock, ASSL, Buf, RECV_BUF_SIZE);
        if N <= 0 then begin Done := True; Break; end;
        ChunkBuf := ChunkBuf + Copy(PAnsiChar(@Buf[0]), 1, N);
      end;
      if Done then Break;

      I := Pos(CRLF, string(ChunkBuf));
      Line := Copy(string(ChunkBuf), 1, I - 1);
      Delete(ChunkBuf, 1, I + 1);

      J := Pos(';', Line);
      if J > 0 then
        Line := Copy(Line, 1, J - 1);

      ChunkSize := StrToIntDef('$' + Trim(Line), 0);
      if ChunkSize = 0 then Break;

      while Length(ChunkBuf) < ChunkSize + 2 do
      begin
        N := RecvBytes(ASock, ASSL, Buf, RECV_BUF_SIZE);
        if N <= 0 then begin Done := True; Break; end;
        ChunkBuf := ChunkBuf + Copy(PAnsiChar(@Buf[0]), 1, N);
      end;

      BodyLen := Length(Result.Body);
      SetLength(Result.Body, BodyLen + ChunkSize);
      Move(ChunkBuf[1], Result.Body[BodyLen], ChunkSize);
      Delete(ChunkBuf, 1, ChunkSize + 2);
    end;
  end
  else
  begin
    ContentLen := StrToIntDef(FindHeaderValue(Result.Headers, 'content-length'), -1);

    if ContentLen >= 0 then
    begin
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
        N := RecvBytes(ASock, ASSL, Buf, RECV_BUF_SIZE);
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
        N := RecvBytes(ASock, ASSL, Buf, RECV_BUF_SIZE);
        if N <= 0 then Break;
        BodyLen := Length(Result.Body);
        SetLength(Result.Body, BodyLen + N);
        Move(Buf[0], Result.Body[BodyLen], N);
      until False;
    end;
  end;
end;

// ---------------------------------------------------------------------------
// Core request logic
// ---------------------------------------------------------------------------

function DoRequest(const AMethod, AURL: string;
  const AHeaders: THTTPHeaders;
  const AMaxRedirects: Integer): THTTPResponse;
var
  Parsed: THTTPParsedURL;
  Sock: TSocket;
  SSL: TSSLConnection;
  Request: AnsiString;
  Raw: TRawHTTPResponse;
  I, Redirects: Integer;
  CurrentURL, Location, HostHeader: string;
  HasUserAgent: Boolean;
  IsHead: Boolean;
begin
  CurrentURL := AURL;
  Redirects := 0;
  Result.Redirected := False;
  IsHead := (UpperCase(AMethod) = 'HEAD');

  while True do
  begin
    Parsed := ParseHTTPURL(CurrentURL);
    SSL.Active := False;
    Sock := ConnectSocket(Parsed.Host, Parsed.Port);
    try
      if Parsed.Scheme = 'https' then
        SSL := InitSSL(Sock, Parsed.Host);

      try
        // Build Host header value
        if ((Parsed.Scheme = 'http') and (Parsed.Port = 80)) or
           ((Parsed.Scheme = 'https') and (Parsed.Port = 443)) then
          HostHeader := Parsed.Host
        else
          HostHeader := Parsed.Host + ':' + IntToStr(Parsed.Port);

        Request := AnsiString(AMethod + ' ' + Parsed.Path + ' HTTP/1.1' + CRLF);
        Request := Request + AnsiString('Host: ' + HostHeader + CRLF);
        Request := Request + AnsiString('Connection: close' + CRLF);

        // Check if user provided User-Agent
        HasUserAgent := False;
        for I := 0 to High(AHeaders) do
          if LowerCase(AHeaders[I].Name) = 'user-agent' then
            HasUserAgent := True;

        if not HasUserAgent then
          Request := Request + AnsiString('User-Agent: GocciaScript/1.0' + CRLF);

        // Add custom headers (skip Host since we already set it)
        for I := 0 to High(AHeaders) do
        begin
          if LowerCase(AHeaders[I].Name) = 'host' then Continue;
          Request := Request + AnsiString(AHeaders[I].Name + ': ' + AHeaders[I].Value + CRLF);
        end;

        Request := Request + AnsiString(CRLF);

        SendAll(Sock, SSL, Request);
        Raw := ReadResponse(Sock, SSL, IsHead);
      finally
        FreeSSL(SSL);
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
        if (Length(Location) > 0) and (Location[1] = '/') then
          CurrentURL := Parsed.Scheme + '://' + HostHeader + Location
        else if Pos('://', Location) = 0 then
          CurrentURL := Parsed.Scheme + '://' + HostHeader + '/' + Location
        else
          CurrentURL := Location;

        // 303: change method to GET
        if Raw.StatusCode = 303 then
          IsHead := False;

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
  const AHeaders: THTTPHeaders): THTTPResponse;
begin
  Result := DoRequest('GET', AURL, AHeaders, MAX_REDIRECTS);
end;

function HTTPHead(const AURL: string;
  const AHeaders: THTTPHeaders): THTTPResponse;
begin
  Result := DoRequest('HEAD', AURL, AHeaders, MAX_REDIRECTS);
end;

end.
