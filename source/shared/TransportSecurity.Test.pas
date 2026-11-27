program TransportSecurityTest;

{$I Shared.inc}

uses
  {$IFDEF UNIX}cthreads, BaseUnix, Sockets,{$ENDIF}
  {$IFDEF MSWINDOWS}Windows, WinSock2,{$ENDIF}

  Classes,
  SysUtils,

  TestingPascalLibrary,
  TimingUtils,
  TransportSecurity;

type
  TTransportSecurityTests = class(TTestSuite)
  private
    procedure TestExtraInputMovesToTheFront;
    procedure TestExtraInputOfTheWholeInputStays;
    procedure TestNoExtraInputClearsTheInput;
    procedure TestExtraInputBeyondTheInputIsRefused;
    procedure TestExtraInputOfALargeRecordSurvivesDecryptingInPlace;
    procedure TestIncompleteMessageKeepsTheInputForTheNextReceive;
    procedure TestCoalescedRecordsAreConsumedOneAtATime;
    procedure TestHeldRestOfAResponseIsReadWithoutTheSocket;
    procedure TestHeldPlaintextLongerThanARequestIsReadInRequests;
    procedure TestReadWithNothingHeldReadsOneRecord;
    procedure TestReadThatWouldBlockBeforeAnyByteIsTriedAgain;
    procedure TestSendEndsAtTheDeadlineWhateverTheSocketTimeout;
    procedure TestSendToASlowlyDrainingPeerEndsAtTheDeadline;
    procedure TestSendAllKeepsSendingToAPeerThatMakesRoom;
    procedure TestSendAllStopsAtTheDeadlineWithTheBytesSent;
    procedure TestSendAfterTheDeadlineFailsAtOnce;
    procedure TestSendWithoutADeadlineSends;
  public
    procedure SetupTests; override;
  end;

procedure TTransportSecurityTests.SetupTests;
begin
  Test('Extra input moves to the front of the input',
    TestExtraInputMovesToTheFront);
  Test('Extra input that is the whole input stays',
    TestExtraInputOfTheWholeInputStays);
  Test('No extra input clears the input', TestNoExtraInputClearsTheInput);
  Test('Extra input beyond the input is refused',
    TestExtraInputBeyondTheInputIsRefused);
  Test('Extra input of a large record survives decrypting in place',
    TestExtraInputOfALargeRecordSurvivesDecryptingInPlace);
  Test('An incomplete message keeps the input for the next receive',
    TestIncompleteMessageKeepsTheInputForTheNextReceive);
  Test('Records that arrive in one receive are consumed one at a time',
    TestCoalescedRecordsAreConsumedOneAtATime);
  Test('The rest of a response held from a decrypted record is read without the socket',
    TestHeldRestOfAResponseIsReadWithoutTheSocket);
  Test('Held plaintext longer than a request is read one request at a time',
    TestHeldPlaintextLongerThanARequestIsReadInRequests);
  Test('A read with nothing held reads one record',
    TestReadWithNothingHeldReadsOneRecord);
  Test('A read that would block before any byte is tried again',
    TestReadThatWouldBlockBeforeAnyByteIsTriedAgain);
  Test('A send ends at the deadline whatever the socket timeout',
    TestSendEndsAtTheDeadlineWhateverTheSocketTimeout);
  Test('A send to a slowly draining peer ends at the deadline',
    TestSendToASlowlyDrainingPeerEndsAtTheDeadline);
  Test('A send of everything continues while the peer makes room',
    TestSendAllKeepsSendingToAPeerThatMakesRoom);
  Test('A send of everything stops at the deadline and reports what went out',
    TestSendAllStopsAtTheDeadlineWithTheBytesSent);
  Test('A send after the deadline fails at once',
    TestSendAfterTheDeadlineFailsAtOnce);
  Test('A send without a deadline sends', TestSendWithoutADeadlineSends);
end;

function PatternBytes(const ALength: Integer; const ASeed: Integer): TBytes;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, ALength);
  for I := 0 to ALength - 1 do
    Result[I] := Byte((I * 7 + ASeed) and $FF);
end;

const
  SECURE_TRANSPORT_SUCCESS = 0;
  SECURE_TRANSPORT_WOULD_BLOCK = -9803;
  { What HTTPClient asks each TLS read for (RECV_BUF_SIZE). }
  RESPONSE_READ_LENGTH = 8192;

type
  { A record layer that reads as SecureTransport's SSLRead does (Apple's
    sslTransport.c): it copies the plaintext it holds from a record it has
    already decrypted, and while the caller's buffer still has room it reads
    one more record from the socket, holding back whatever of that record does
    not fit. Records lists the plaintext of the records the server sent; once
    they are all read, the server sends nothing more but keeps the connection
    open, where SecureTransport's blocking socket read would wait forever. This
    one records that it went to the socket then, and returns as a read that
    would block. WouldBlockFirst is how many reads report a record still on its
    way (would block, nothing read) before the records are read. }
  TFakeRecordLayer = class
  public
    Records: array of TBytes;
    NextRecord: Integer;
    Held: TBytes;
    HeldOffset: Integer;
    WouldBlockFirst: Integer;
    ReadsFromTheSocket: Integer;
    WaitedForARecordThatNeverCame: Boolean;
  end;

function FakeRecordRead(AContext: Pointer; AData: Pointer;
  ADataLength: NativeUInt; var AProcessed: NativeUInt): LongInt; cdecl;
var
  Layer: TFakeRecordLayer;
  Target: PByte;
  Count, Remaining: NativeUInt;
  RecordBytes: TBytes;
begin
  Layer := TFakeRecordLayer(AContext);
  Target := PByte(AData);
  AProcessed := 0;
  Remaining := ADataLength;
  if Length(Layer.Held) - Layer.HeldOffset > 0 then
  begin
    Count := Length(Layer.Held) - Layer.HeldOffset;
    if Count > Remaining then
      Count := Remaining;
    Move(Layer.Held[Layer.HeldOffset], Target^, Count);
    Inc(Layer.HeldOffset, Count);
    Inc(Target, Count);
    Inc(AProcessed, Count);
    Dec(Remaining, Count);
    if Layer.HeldOffset >= Length(Layer.Held) then
    begin
      Layer.Held := nil;
      Layer.HeldOffset := 0;
    end;
  end;
  if Remaining = 0 then
    Exit(SECURE_TRANSPORT_SUCCESS);

  Inc(Layer.ReadsFromTheSocket);
  if Layer.WouldBlockFirst > 0 then
  begin
    Dec(Layer.WouldBlockFirst);
    Exit(SECURE_TRANSPORT_WOULD_BLOCK);
  end;
  if Layer.NextRecord >= Length(Layer.Records) then
  begin
    Layer.WaitedForARecordThatNeverCame := True;
    Exit(SECURE_TRANSPORT_WOULD_BLOCK);
  end;
  RecordBytes := Layer.Records[Layer.NextRecord];
  Inc(Layer.NextRecord);
  Count := Length(RecordBytes);
  if Count <= Remaining then
    Move(RecordBytes[0], Target^, Count)
  else
  begin
    Move(RecordBytes[0], Target^, Remaining);
    Layer.Held := RecordBytes;
    Layer.HeldOffset := Remaining;
    Count := Remaining;
  end;
  Inc(AProcessed, Count);
  Result := SECURE_TRANSPORT_SUCCESS;
end;

function FakeBufferedSize(AContext: Pointer;
  var ABufferSize: NativeUInt): LongInt; cdecl;
var
  Layer: TFakeRecordLayer;
begin
  Layer := TFakeRecordLayer(AContext);
  ABufferSize := Length(Layer.Held) - Layer.HeldOffset;
  Result := SECURE_TRANSPORT_SUCCESS;
end;

{ Reads from ALayer as HTTPClient reads a response with a declared length:
  requests of RESPONSE_READ_LENGTH bytes until AExpected bytes have come, and
  checks they are AExpected's. }
procedure ReadResponseOf(const ALayer: TFakeRecordLayer;
  const AExpected: TBytes);
var
  Buffer: array[0..RESPONSE_READ_LENGTH - 1] of Byte;
  Received: TBytes;
  Count, Status, I: Integer;
begin
  Received := nil;
  while Length(Received) < Length(AExpected) do
  begin
    Count := ReadSecureTransportRecords(ALayer, FakeRecordRead,
      FakeBufferedSize, Buffer, RESPONSE_READ_LENGTH, Status);
    Expect<Integer>(Status).ToBe(SECURE_TRANSPORT_SUCCESS);
    Expect<Boolean>(Count > 0).ToBe(True);
    if Count <= 0 then
      Break;
    SetLength(Received, Length(Received) + Count);
    Move(Buffer[0], Received[Length(Received) - Count], Count);
  end;
  Expect<Integer>(Length(Received)).ToBe(Length(AExpected));
  for I := 0 to Length(AExpected) - 1 do
    if Received[I] <> AExpected[I] then
    begin
      Expect<Integer>(Received[I]).ToBe(AExpected[I]);
      Break;
    end;
end;

procedure TTransportSecurityTests.TestExtraInputMovesToTheFront;
var
  Input: TBytes;
begin
  Input := PatternBytes(10, 0);
  ConsumeEncryptedInput(Input, False, 3);
  Expect<Integer>(Length(Input)).ToBe(3);
  Expect<Integer>(Input[0]).ToBe(49);
  Expect<Integer>(Input[1]).ToBe(56);
  Expect<Integer>(Input[2]).ToBe(63);
end;

procedure TTransportSecurityTests.TestExtraInputOfTheWholeInputStays;
var
  Expected, Input: TBytes;
  I: Integer;
begin
  Input := PatternBytes(10, 3);
  Expected := Copy(Input);
  ConsumeEncryptedInput(Input, False, 10);
  Expect<Integer>(Length(Input)).ToBe(10);
  for I := 0 to 9 do
    Expect<Integer>(Input[I]).ToBe(Expected[I]);
end;

procedure TTransportSecurityTests.TestNoExtraInputClearsTheInput;
var
  Input: TBytes;
begin
  Input := PatternBytes(10, 0);
  ConsumeEncryptedInput(Input, False, 0);
  Expect<Integer>(Length(Input)).ToBe(0);
end;

procedure TTransportSecurityTests.TestExtraInputBeyondTheInputIsRefused;
var
  Input: TBytes;
  RaisedForLength, RaisedForNegative: Boolean;
begin
  Input := PatternBytes(10, 0);
  RaisedForLength := False;
  RaisedForNegative := False;
  try
    ConsumeEncryptedInput(Input, False, 11);
  except
    on E: Exception do
      RaisedForLength := True;
  end;
  try
    ConsumeEncryptedInput(Input, False, -1);
  except
    on E: Exception do
      RaisedForNegative := True;
  end;
  Expect<Boolean>(RaisedForLength).ToBe(True);
  Expect<Boolean>(RaisedForNegative).ToBe(True);
  Expect<Integer>(Length(Input)).ToBe(10);
end;

{ SChannel decrypts the first record in place and reports the second as the
  extra buffer, a pointer into the same array. The extra bytes must be moved
  while the array is allocated: a record this size is a block of its own that
  freeing returns to the system, so a copy from it after the free reads
  unmapped memory. }
procedure TTransportSecurityTests.TestExtraInputOfALargeRecordSurvivesDecryptingInPlace;
const
  FIRST_RECORD_LENGTH = 1024 * 1024;
  EXTRA_LENGTH = 64 * 1024;
var
  Expected, Input: TBytes;
  I, Mismatches: Integer;
begin
  Input := PatternBytes(FIRST_RECORD_LENGTH + EXTRA_LENGTH, 1);
  Expected := Copy(Input, FIRST_RECORD_LENGTH, EXTRA_LENGTH);
  for I := 0 to FIRST_RECORD_LENGTH - 1 do
    Input[I] := Input[I] xor $5A;
  ConsumeEncryptedInput(Input, False, EXTRA_LENGTH);
  Expect<Integer>(Length(Input)).ToBe(EXTRA_LENGTH);
  Mismatches := 0;
  for I := 0 to EXTRA_LENGTH - 1 do
    if Input[I] <> Expected[I] then
      Inc(Mismatches);
  Expect<Integer>(Mismatches).ToBe(0);
end;

{ A handshake message split across receives: SChannel reports it incomplete
  and consumed nothing, so what arrived stays and the next receive extends it
  (issue #1814 cleared it first). }
procedure TTransportSecurityTests.TestIncompleteMessageKeepsTheInputForTheNextReceive;
var
  Expected, Input, Next: TBytes;
  I: Integer;
begin
  Input := PatternBytes(5, 0);
  Next := PatternBytes(4, 9);
  Expected := Copy(Input);
  ConsumeEncryptedInput(Input, True, 0);
  Expect<Integer>(Length(Input)).ToBe(5);
  for I := 0 to 4 do
    Expect<Integer>(Input[I]).ToBe(Expected[I]);
  { The receive's bytes are appended, and the message then completes with
    the last two bytes left over. }
  SetLength(Input, 9);
  Move(Next[0], Input[5], 4);
  ConsumeEncryptedInput(Input, False, 2);
  Expect<Integer>(Length(Input)).ToBe(2);
  Expect<Integer>(Input[0]).ToBe(Next[2]);
  Expect<Integer>(Input[1]).ToBe(Next[3]);
end;

{ Two records in one receive: the first is consumed and the second stays at
  the front of the input, to be consumed by the next call. }
procedure TTransportSecurityTests.TestCoalescedRecordsAreConsumedOneAtATime;
const
  FIRST_LENGTH = 21;
  SECOND_LENGTH = 13;
var
  Input, Second: TBytes;
  I: Integer;
begin
  Input := PatternBytes(FIRST_LENGTH + SECOND_LENGTH, 2);
  Second := Copy(Input, FIRST_LENGTH, SECOND_LENGTH);
  ConsumeEncryptedInput(Input, False, SECOND_LENGTH);
  Expect<Integer>(Length(Input)).ToBe(SECOND_LENGTH);
  for I := 0 to SECOND_LENGTH - 1 do
    Expect<Integer>(Input[I]).ToBe(Second[I]);
  ConsumeEncryptedInput(Input, False, 0);
  Expect<Integer>(Length(Input)).ToBe(0);
end;

procedure TTransportSecurityTests.TestHeldRestOfAResponseIsReadWithoutTheSocket;
const
  { A last record of 12,204 bytes of plaintext, as the issue's server sent:
    a request of 8,192 takes the first part, and the 4,012 that are left are
    the end of the response. }
  LAST_RECORD_LENGTH = 12204;
var
  Layer: TFakeRecordLayer;
  Expected: TBytes;
begin
  Layer := TFakeRecordLayer.Create;
  try
    Expected := PatternBytes(LAST_RECORD_LENGTH, 3);
    SetLength(Layer.Records, 1);
    Layer.Records[0] := Expected;
    ReadResponseOf(Layer, Expected);
    Expect<Boolean>(Layer.WaitedForARecordThatNeverCame).ToBe(False);
    Expect<Integer>(Layer.ReadsFromTheSocket).ToBe(1);
  finally
    Layer.Free;
  end;
end;

procedure TTransportSecurityTests.TestHeldPlaintextLongerThanARequestIsReadInRequests;
const
  { Each record is longer than a request, so every one is held in part and
    the last 808 bytes of the response are held when the request before them
    returns. }
  RECORD_LENGTH = 16384;
  LAST_RECORD_LENGTH = 9000;
var
  Layer: TFakeRecordLayer;
  Expected: TBytes;
begin
  Layer := TFakeRecordLayer.Create;
  try
    Expected := PatternBytes(RECORD_LENGTH + LAST_RECORD_LENGTH, 5);
    SetLength(Layer.Records, 2);
    Layer.Records[0] := Copy(Expected, 0, RECORD_LENGTH);
    Layer.Records[1] := Copy(Expected, RECORD_LENGTH, LAST_RECORD_LENGTH);
    ReadResponseOf(Layer, Expected);
    Expect<Boolean>(Layer.WaitedForARecordThatNeverCame).ToBe(False);
    Expect<Integer>(Layer.ReadsFromTheSocket).ToBe(2);
  finally
    Layer.Free;
  end;
end;

procedure TTransportSecurityTests.TestReadWithNothingHeldReadsOneRecord;
const
  RECORD_LENGTH = 239;
var
  Layer: TFakeRecordLayer;
  Buffer: array[0..RESPONSE_READ_LENGTH - 1] of Byte;
  Status: Integer;
begin
  Layer := TFakeRecordLayer.Create;
  try
    SetLength(Layer.Records, 1);
    Layer.Records[0] := PatternBytes(RECORD_LENGTH, 7);
    Expect<Integer>(ReadSecureTransportRecords(Layer, FakeRecordRead,
      FakeBufferedSize, Buffer, RESPONSE_READ_LENGTH, Status))
      .ToBe(RECORD_LENGTH);
    Expect<Integer>(Status).ToBe(SECURE_TRANSPORT_SUCCESS);
    Expect<Integer>(Layer.ReadsFromTheSocket).ToBe(1);
    Expect<Boolean>(Layer.WaitedForARecordThatNeverCame).ToBe(False);
  finally
    Layer.Free;
  end;
end;

procedure TTransportSecurityTests.TestReadThatWouldBlockBeforeAnyByteIsTriedAgain;
const
  RECORD_LENGTH = 4000;
var
  Layer: TFakeRecordLayer;
  Expected: TBytes;
begin
  Layer := TFakeRecordLayer.Create;
  try
    Expected := PatternBytes(RECORD_LENGTH, 11);
    SetLength(Layer.Records, 1);
    Layer.Records[0] := Expected;
    Layer.WouldBlockFirst := 2;
    ReadResponseOf(Layer, Expected);
    Expect<Integer>(Layer.ReadsFromTheSocket).ToBe(3);
    Expect<Boolean>(Layer.WaitedForARecordThatNeverCame).ToBe(False);
  finally
    Layer.Free;
  end;
end;

{ A TCP connection on loopback, made in one thread: the listener's backlog
  holds the connection until it is accepted. }
procedure CreateConnectedPair(out AClient, AServer: TSocket);
{$IFDEF UNIX}
var
  Address: TInetSockAddr;
  AddressLength: TSockLen;
  Listener: TSocket;
begin
  Listener := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Listener < 0 then
    raise Exception.Create('socket failed');
  FillChar(Address, SizeOf(Address), 0);
  Address.sin_family := AF_INET;
  Address.sin_port := 0;
  Address.sin_addr := StrToNetAddr('127.0.0.1');
  AddressLength := SizeOf(Address);
  if (fpBind(Listener, @Address, SizeOf(Address)) <> 0) or
     (fpListen(Listener, 1) <> 0) or
     (fpGetSockName(Listener, @Address, @AddressLength) <> 0) then
  begin
    CloseSocket(Listener);
    raise Exception.Create('loopback listener failed');
  end;
  AClient := fpSocket(AF_INET, SOCK_STREAM, 0);
  if (AClient < 0) or
     (fpConnect(AClient, @Address, SizeOf(Address)) <> 0) then
  begin
    CloseSocket(Listener);
    raise Exception.Create('loopback connect failed');
  end;
  AServer := fpAccept(Listener, nil, nil);
  CloseSocket(Listener);
  if AServer < 0 then
    raise Exception.Create('loopback accept failed');
end;
{$ENDIF}
{$IFDEF MSWINDOWS}
var
  Address: TSockAddrIn;
  AddressLength: LongInt;
  Listener: TSocket;
  StartupData: TWSAData;
begin
  if WSAStartup($0202, StartupData) <> 0 then
    raise Exception.Create('WSAStartup failed');
  Listener := WinSock2.socket(AF_INET, SOCK_STREAM, 0);
  if Listener = INVALID_SOCKET then
    raise Exception.Create('socket failed');
  FillChar(Address, SizeOf(Address), 0);
  Address.sin_family := AF_INET;
  Address.sin_port := 0;
  Address.sin_addr.S_addr := htonl(INADDR_LOOPBACK);
  AddressLength := SizeOf(Address);
  if (WinSock2.bind(Listener, PSockAddr(@Address), SizeOf(Address)) <> 0) or
     (WinSock2.listen(Listener, 1) <> 0) or
     (WinSock2.getsockname(Listener, TSockAddr(Address), AddressLength) <> 0) then
  begin
    WinSock2.closesocket(Listener);
    raise Exception.Create('loopback listener failed');
  end;
  AClient := WinSock2.socket(AF_INET, SOCK_STREAM, 0);
  if (AClient = INVALID_SOCKET) or
     (WinSock2.connect(AClient, PSockAddr(@Address), SizeOf(Address)) <> 0) then
  begin
    WinSock2.closesocket(Listener);
    raise Exception.Create('loopback connect failed');
  end;
  AServer := WinSock2.accept(Listener, nil, nil);
  WinSock2.closesocket(Listener);
  if AServer = INVALID_SOCKET then
    raise Exception.Create('loopback accept failed');
end;
{$ENDIF}

procedure CloseOneSocket(const ASocket: TSocket);
begin
  {$IFDEF UNIX}
  CloseSocket(ASocket);
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  WinSock2.closesocket(ASocket);
  {$ENDIF}
end;

procedure CloseSockets(const AClient, AServer: TSocket);
begin
  CloseOneSocket(AClient);
  CloseOneSocket(AServer);
end;

{ Sets a send or receive timeout (SO_SNDTIMEO or SO_RCVTIMEO). Used to leave
  what HTTPClient leaves on its socket before a request's reads, a timeout
  counted from then and not from the request's deadline, and to keep a test
  that goes wrong from waiting for ever. }
procedure SetSocketTimeoutOption(const ASocket: TSocket;
  const AOption: Integer; const AMilliseconds: Integer);
{$IFDEF UNIX}
var
  Timeout: TTimeVal;
begin
  Timeout.tv_sec := AMilliseconds div 1000;
  Timeout.tv_usec := (AMilliseconds mod 1000) * 1000;
  fpSetSockOpt(ASocket, SOL_SOCKET, AOption, @Timeout, SizeOf(Timeout));
end;
{$ENDIF}
{$IFDEF MSWINDOWS}
var
  Timeout: LongInt;
begin
  Timeout := AMilliseconds;
  WinSock2.setsockopt(ASocket, SOL_SOCKET, AOption, PAnsiChar(@Timeout),
    SizeOf(Timeout));
end;
{$ENDIF}

procedure SetSendTimeout(const ASocket: TSocket; const AMilliseconds: Integer);
begin
  SetSocketTimeoutOption(ASocket, SO_SNDTIMEO, AMilliseconds);
end;

function ReceiveSome(const ASocket: TSocket; var ABuffer: array of Byte): Integer;
begin
  {$IFDEF UNIX}
  Result := fpRecv(ASocket, @ABuffer[0], Length(ABuffer), 0);
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Result := WinSock2.recv(ASocket, ABuffer[0], Length(ABuffer), 0);
  {$ENDIF}
end;

type
  { Reads 4 KiB at a time, pausing between reads, until it is told to stop
    or its socket is closed: a peer that drains the connection at its own
    pace. }
  TSlowReader = class(TThread)
  private
    FSocket: TSocket;
    FDelayMilliseconds: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(const ASocket: TSocket;
      const ADelayMilliseconds: Integer);
  end;

constructor TSlowReader.Create(const ASocket: TSocket;
  const ADelayMilliseconds: Integer);
begin
  FSocket := ASocket;
  FDelayMilliseconds := ADelayMilliseconds;
  inherited Create(False);
end;

procedure TSlowReader.Execute;
var
  Buffer: array[0..4095] of Byte;
begin
  { A receive that waits too long ends the reader, so a test that goes wrong
    does not wait for ever. }
  SetSocketTimeoutOption(FSocket, SO_RCVTIMEO, 10000);
  while not Terminated and (ReceiveSome(FSocket, Buffer) > 0) do
    if FDelayMilliseconds > 0 then
      Sleep(FDelayMilliseconds);
end;

{ The peer accepts the connection and never reads, so sending fills the
  socket's buffers and then blocks. The socket's own send timeout is long: only
  the deadline can end the send in time. Without the deadline the send waited
  out the socket's timeout, counted from before the request (issue #1818). }
procedure TTransportSecurityTests.TestSendEndsAtTheDeadlineWhateverTheSocketTimeout;
const
  DEADLINE_MILLISECONDS = 500;
  SOCKET_TIMEOUT_MILLISECONDS = 8000;
  MAX_ELAPSED_MILLISECONDS = 4000;
  EARLIEST_END_MILLISECONDS = 400;
  GIVE_UP_BYTES = Int64(1024) * 1024 * 1024;
var
  Chunk: array[0..65535] of Byte;
  Client, Server: TSocket;
  Deadline, ElapsedMilliseconds, Start, Total: Int64;
  Sent: Integer;
begin
  FillChar(Chunk, SizeOf(Chunk), $41);
  CreateConnectedPair(Client, Server);
  try
    SetSendTimeout(Client, SOCKET_TIMEOUT_MILLISECONDS);
    Start := GetNanoseconds;
    Deadline := Start + Int64(DEADLINE_MILLISECONDS) * 1000000;
    Total := 0;
    repeat
      Sent := SocketSendBefore(Client, @Chunk[0], SizeOf(Chunk), Deadline);
      if Sent > 0 then
        Inc(Total, Sent);
    until (Sent <= 0) or (Total >= GIVE_UP_BYTES);
    ElapsedMilliseconds := (GetNanoseconds - Start) div 1000000;
    Expect<Boolean>(Sent <= 0).ToBe(True);
    Expect<Boolean>(ElapsedMilliseconds < MAX_ELAPSED_MILLISECONDS).ToBe(True);
    { The send did not fail for another reason before the deadline. }
    Expect<Boolean>(ElapsedMilliseconds >= EARLIEST_END_MILLISECONDS).ToBe(True);
  finally
    CloseSockets(Client, Server);
  end;
end;

{ The peer drains the connection, a little at a time, and so makes room
  again and again. A blocking send's timeout restarts each time it does (on
  Darwin), so one send of a large buffer could outlast any deadline: the single
  call here must return by it. }
procedure TTransportSecurityTests.TestSendToASlowlyDrainingPeerEndsAtTheDeadline;
const
  DEADLINE_MILLISECONDS = 500;
  SOCKET_TIMEOUT_MILLISECONDS = 8000;
  MAX_ELAPSED_MILLISECONDS = 4000;
  BUFFER_LENGTH = 16 * 1024 * 1024;
var
  Buffer: TBytes;
  Client, Server: TSocket;
  ElapsedMilliseconds, Start: Int64;
  Reader: TSlowReader;
begin
  Buffer := nil;
  SetLength(Buffer, BUFFER_LENGTH);
  FillChar(Buffer[0], BUFFER_LENGTH, $41);
  CreateConnectedPair(Client, Server);
  Reader := TSlowReader.Create(Server, 20);
  try
    SetSendTimeout(Client, SOCKET_TIMEOUT_MILLISECONDS);
    Start := GetNanoseconds;
    SocketSendBefore(Client, @Buffer[0], BUFFER_LENGTH,
      Start + Int64(DEADLINE_MILLISECONDS) * 1000000);
    ElapsedMilliseconds := (GetNanoseconds - Start) div 1000000;
    Expect<Boolean>(ElapsedMilliseconds < MAX_ELAPSED_MILLISECONDS).ToBe(True);
  finally
    { Closing the client ends a receive the reader is waiting in. }
    Reader.Terminate;
    CloseOneSocket(Client);
    Reader.WaitFor;
    Reader.Free;
    CloseOneSocket(Server);
  end;
end;

{ SecureTransport counts the bytes its write callback accepts as written, so the
  callback sends all of its buffer: a peer that makes room (here a fast reader)
  gets every byte, however many sends it takes, where one non-blocking send
  takes only what fits. }
procedure TTransportSecurityTests.TestSendAllKeepsSendingToAPeerThatMakesRoom;
const
  BUFFER_LENGTH = 24 * 1024 * 1024;
var
  Buffer: TBytes;
  Client, Server: TSocket;
  Reader: TSlowReader;
  Sent: Integer;
begin
  Buffer := nil;
  SetLength(Buffer, BUFFER_LENGTH);
  FillChar(Buffer[0], BUFFER_LENGTH, $41);
  CreateConnectedPair(Client, Server);
  Reader := TSlowReader.Create(Server, 0);
  try
    SetSendTimeout(Client, 10000);
    Sent := SocketSendAllBefore(Client, @Buffer[0], BUFFER_LENGTH,
      GetNanoseconds + Int64(30) * 1000000000);
    Expect<Integer>(Sent).ToBe(BUFFER_LENGTH);
  finally
    Reader.Terminate;
    CloseOneSocket(Client);
    Reader.WaitFor;
    Reader.Free;
    CloseOneSocket(Server);
  end;
end;

{ The peer drains slowly, so the buffer cannot all go by the deadline: the call
  returns at the deadline with the count that did, more than nothing and less
  than all of it. }
procedure TTransportSecurityTests.TestSendAllStopsAtTheDeadlineWithTheBytesSent;
const
  DEADLINE_MILLISECONDS = 500;
  EARLIEST_END_MILLISECONDS = 400;
  MAX_ELAPSED_MILLISECONDS = 4000;
  BUFFER_LENGTH = 64 * 1024 * 1024;
var
  Buffer: TBytes;
  Client, Server: TSocket;
  ElapsedMilliseconds, Start: Int64;
  Reader: TSlowReader;
  Sent: Integer;
begin
  Buffer := nil;
  SetLength(Buffer, BUFFER_LENGTH);
  FillChar(Buffer[0], BUFFER_LENGTH, $41);
  CreateConnectedPair(Client, Server);
  Reader := TSlowReader.Create(Server, 20);
  try
    SetSendTimeout(Client, 10000);
    Start := GetNanoseconds;
    Sent := SocketSendAllBefore(Client, @Buffer[0], BUFFER_LENGTH,
      Start + Int64(DEADLINE_MILLISECONDS) * 1000000);
    ElapsedMilliseconds := (GetNanoseconds - Start) div 1000000;
    Expect<Boolean>(Sent > 0).ToBe(True);
    Expect<Boolean>(Sent < BUFFER_LENGTH).ToBe(True);
    Expect<Boolean>(ElapsedMilliseconds >= EARLIEST_END_MILLISECONDS).ToBe(True);
    Expect<Boolean>(ElapsedMilliseconds < MAX_ELAPSED_MILLISECONDS).ToBe(True);
  finally
    Reader.Terminate;
    CloseOneSocket(Client);
    Reader.WaitFor;
    Reader.Free;
    CloseOneSocket(Server);
  end;
end;

procedure TTransportSecurityTests.TestSendAfterTheDeadlineFailsAtOnce;
var
  Buffer: array[0..15] of Byte;
  Client, Server: TSocket;
begin
  FillChar(Buffer, SizeOf(Buffer), $41);
  CreateConnectedPair(Client, Server);
  try
    Expect<Integer>(SocketSendBefore(Client, @Buffer[0], SizeOf(Buffer),
      GetNanoseconds - 1000000)).ToBe(-1);
  finally
    CloseSockets(Client, Server);
  end;
end;

procedure TTransportSecurityTests.TestSendWithoutADeadlineSends;
var
  Buffer: array[0..15] of Byte;
  Client, Server: TSocket;
  Received: Integer;
  Echo: array[0..15] of Byte;
begin
  Received := 0;
  FillChar(Buffer, SizeOf(Buffer), $41);
  FillChar(Echo, SizeOf(Echo), 0);
  CreateConnectedPair(Client, Server);
  try
    Expect<Integer>(SocketSendBefore(Client, @Buffer[0], SizeOf(Buffer),
      0)).ToBe(SizeOf(Buffer));
    Received := ReceiveSome(Server, Echo);
    Expect<Integer>(Received).ToBe(SizeOf(Buffer));
    Expect<Integer>(Echo[0]).ToBe($41);
  finally
    CloseSockets(Client, Server);
  end;
end;

begin
  TestRunnerProgram.AddSuite(TTransportSecurityTests.Create('TransportSecurity'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
