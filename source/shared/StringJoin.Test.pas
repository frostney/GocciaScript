program StringJoin.Test;

{$I Shared.inc}

uses
  SysUtils,

  AllocationCounter,
  StringBuffer,
  StringJoin,
  TestingPascalLibrary;

type
  TStringJoinTests = class(TTestSuite)
  private
    procedure TestNoElementsJoinToEmpty;
    procedure TestElementsJoinWithTheSeparator;
    procedure TestEmptyElementsKeepTheirSeparators;
    procedure TestEveryElementEmpty;
    procedure TestEmptySeparator;
    procedure TestLongerSeparator;
    procedure TestOneStringIsReturnedItself;
    procedure TestManyElements;
    procedure TestResultIsAllocatedOnce;
    procedure TestChunksAndLongStrings;
    procedure TestFinalCopyPolls;
    procedure TestGateIsAskedBeforeTheResult;
    procedure TestARaisingGateOrPollLeavesTheBufferEmpty;
  public
    procedure SetupTests; override;
  end;

procedure TStringJoinTests.SetupTests;
begin
  Test('no elements join to the empty string', TestNoElementsJoinToEmpty);
  Test('elements join with the separator between each two',
    TestElementsJoinWithTheSeparator);
  Test('an empty element keeps the separators on either side',
    TestEmptyElementsKeepTheirSeparators);
  Test('empty elements join to their separators alone',
    TestEveryElementEmpty);
  Test('an empty separator concatenates the elements', TestEmptySeparator);
  Test('a separator longer than one character is written whole',
    TestLongerSeparator);
  Test('a join with nothing between its strings returns the one string itself',
    TestOneStringIsReturnedItself);
  Test('more elements than the first part capacity join in order',
    TestManyElements);
  Test('Join allocates the result once, at its exact length',
    TestResultIsAllocatedOnce);
  Test('short strings across many chunks and long strings between them join in order',
    TestChunksAndLongStrings);
  Test('the final copy polls once per poll interval of characters',
    TestFinalCopyPolls);
  Test('the gate is asked about the result before it is allocated',
    TestGateIsAskedBeforeTheResult);
  Test('a gate or poll that raises leaves the buffer empty and usable',
    TestARaisingGateOrPollLeavesTheBufferEmpty);
end;

function JoinOf(const ASeparator: UnicodeString;
  const AElements: array of UnicodeString): UnicodeString;
var
  Joined: TStringJoinBuffer;
  I: Integer;
begin
  Joined := TStringJoinBuffer.Create(ASeparator);
  try
    for I := 0 to High(AElements) do
      Joined.Add(AElements[I]);
    Expect<Integer>(Joined.ElementCount).ToBe(Length(AElements));
    Result := Joined.Join;
  finally
    Joined.Free;
  end;
end;

procedure TStringJoinTests.TestNoElementsJoinToEmpty;
begin
  Expect<string>(JoinOf(',', [])).ToBe('');
end;

procedure TStringJoinTests.TestElementsJoinWithTheSeparator;
begin
  Expect<string>(JoinOf(',', ['a'])).ToBe('a');
  Expect<string>(JoinOf(',', ['a', 'bc', 'def'])).ToBe('a,bc,def');
end;

procedure TStringJoinTests.TestEmptyElementsKeepTheirSeparators;
begin
  Expect<string>(JoinOf(',', ['', 'a', '', 'b', ''])).ToBe(',a,,b,');
  Expect<string>(JoinOf(',', ['', 'a'])).ToBe(',a');
  Expect<string>(JoinOf(',', ['a', ''])).ToBe('a,');
end;

procedure TStringJoinTests.TestEveryElementEmpty;
begin
  Expect<string>(JoinOf(',', [''])).ToBe('');
  Expect<string>(JoinOf(',', ['', '', ''])).ToBe(',,');
  Expect<string>(JoinOf('', ['', '', ''])).ToBe('');
end;

procedure TStringJoinTests.TestEmptySeparator;
begin
  Expect<string>(JoinOf('', ['a', '', 'bc', 'd'])).ToBe('abcd');
end;

procedure TStringJoinTests.TestLongerSeparator;
begin
  Expect<string>(JoinOf(', ', ['a', 'b', '', 'c'])).ToBe('a, b, , c');
end;

procedure TStringJoinTests.TestOneStringIsReturnedItself;
var
  Text, Short: UnicodeString;
begin
  Text := StringOfChar('x', 1000);
  Short := StringOfChar('s', 3);
  Expect<Boolean>(Pointer(JoinOf(',', [Text])) = Pointer(Text)).ToBe(True);
  Expect<Boolean>(Pointer(JoinOf('', ['', Text, ''])) = Pointer(Text))
    .ToBe(True);
  Expect<Boolean>(Pointer(JoinOf(',', [Short])) = Pointer(Short)).ToBe(True);
  Expect<Boolean>(Pointer(JoinOf('', ['', Short])) = Pointer(Short))
    .ToBe(True);
end;

procedure TStringJoinTests.TestManyElements;
var
  Joined: TStringJoinBuffer;
  Expected: UnicodeString;
  I: Integer;
begin
  Joined := TStringJoinBuffer.Create('-');
  try
    Expected := '';
    for I := 0 to 999 do
    begin
      if I > 0 then
        Expected := Expected + '-';
      if I mod 3 <> 0 then
      begin
        Joined.Add(UnicodeString(IntToStr(I)));
        Expected := Expected + UnicodeString(IntToStr(I));
      end
      else
        Joined.Add('');
    end;
    Expect<string>(Joined.Join).ToBe(Expected);
  finally
    Joined.Free;
  end;
end;

{ Join writes the result once, into a string of the exact length: it asks
  for the result's bytes and a little bookkeeping, never a second copy or
  spare capacity for the result to keep. }
procedure TStringJoinTests.TestResultIsAllocatedOnce;
const
  RESULT_LENGTH = 3889;
  BOOKKEEPING_BYTES = 512;
var
  Joined: TStringJoinBuffer;
  Text: UnicodeString;
  I: Integer;
  Bytes: Int64;
begin
  Joined := TStringJoinBuffer.Create(',');
  try
    for I := 0 to 999 do
      Joined.Add(UnicodeString(IntToStr(I)));
    StartCountingAllocations;
    try
      Text := Joined.Join;
    finally
      StopCountingAllocations;
      Bytes := CountedAllocationBytes;
    end;
    Expect<Integer>(Length(Text)).ToBe(RESULT_LENGTH);
    Expect<Boolean>(Bytes >= RESULT_LENGTH * SizeOf(WideChar)).ToBe(True);
    Expect<Boolean>(Bytes <= RESULT_LENGTH * SizeOf(WideChar) +
      BOOKKEEPING_BYTES).ToBe(True);
  finally
    Joined.Free;
  end;
end;

procedure TStringJoinTests.TestChunksAndLongStrings;
var
  Joined: TStringJoinBuffer;
  Expected: TStringBuffer;
  Long, Text: UnicodeString;
  I: Integer;
begin
  Long := StringOfChar('L', LONG_TEXT_LENGTH);
  Joined := TStringJoinBuffer.Create(', ');
  Expected := TStringBuffer.Create;
  try
    for I := 0 to 49999 do
    begin
      if I > 0 then
        Expected.Append(', ');
      if I mod 997 = 0 then
        Text := Long
      else if I mod 13 = 0 then
        Text := ''
      else
        Text := UnicodeString(IntToStr(I));
      Joined.Add(Text);
      Expected.Append(Text);
    end;
    Expect<Boolean>(Expected.Length > 8 * MAXIMUM_CHUNK_LENGTH).ToBe(True);
    Expect<string>(Joined.Join).ToBe(Expected.ToString);
    Expect<Integer>(Joined.ElementCount).ToBe(0);
  finally
    Joined.Free;
  end;
end;

var
  PollCount: Integer;

procedure CountPoll;
begin
  Inc(PollCount);
end;

procedure TStringJoinTests.TestFinalCopyPolls;
const
  PART_COUNT = 5;
var
  Joined: TStringJoinBuffer;
  Part: UnicodeString;
  I: Integer;
  Text: UnicodeString;
begin
  Part := StringOfChar('p', POLL_INTERVAL_LENGTH);
  PollCount := 0;
  Joined := TStringJoinBuffer.Create(',', CountPoll);
  try
    for I := 1 to PART_COUNT do
      Joined.Add(Part);
    Expect<Integer>(PollCount).ToBe(0);
    Text := Joined.Join;
  finally
    Joined.Free;
  end;
  Expect<Integer>(Length(Text))
    .ToBe(PART_COUNT * POLL_INTERVAL_LENGTH + PART_COUNT - 1);
  Expect<Integer>(PollCount).ToBe(Length(Text) div POLL_INTERVAL_LENGTH);
end;

var
  GateCalls: Integer;
  LastGateBytes, GateRefusesAbove: Int64;

procedure RecordGate(const AResultBytes: Int64);
begin
  Inc(GateCalls);
  LastGateBytes := AResultBytes;
  if AResultBytes > GateRefusesAbove then
    raise Exception.Create('refused');
end;

procedure RaisingPoll;
begin
  raise Exception.Create('aborted');
end;

procedure TStringJoinTests.TestGateIsAskedBeforeTheResult;
var
  Joined: TStringJoinBuffer;
  Text: UnicodeString;
  I: Integer;
begin
  GateCalls := 0;
  GateRefusesAbove := High(Int64);
  Joined := TStringJoinBuffer.Create(',', nil, RecordGate);
  try
    for I := 0 to 9999 do
      Joined.Add(UnicodeString(IntToStr(I)));
    Expect<Integer>(GateCalls).ToBe(0);
    Text := Joined.Join;
    Expect<Integer>(GateCalls).ToBe(1);
    Expect<Boolean>(LastGateBytes = Length(Text) * SizeOf(WideChar))
      .ToBe(True);
    // A lone string is the result itself: nothing is allocated, nothing asked.
    Joined.Add(Text);
    Expect<Boolean>(Pointer(Joined.Join) = Pointer(Text)).ToBe(True);
    Expect<Integer>(GateCalls).ToBe(1);
  finally
    Joined.Free;
  end;
end;

procedure TStringJoinTests.TestARaisingGateOrPollLeavesTheBufferEmpty;
var
  Joined: TStringJoinBuffer;
  Part: UnicodeString;
  I: Integer;
  Raised: Boolean;
begin
  GateRefusesAbove := 4 * MAXIMUM_CHUNK_LENGTH;
  Joined := TStringJoinBuffer.Create(',', nil, RecordGate);
  try
    for I := 0 to 49999 do
      Joined.Add(UnicodeString(IntToStr(I)));
    Raised := False;
    try
      Joined.Join;
    except
      Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
    Expect<Integer>(Joined.ElementCount).ToBe(0);
    Joined.Add('a');
    Joined.Add('b');
    Expect<string>(Joined.Join).ToBe('a,b');
  finally
    Joined.Free;
  end;

  Part := StringOfChar('p', POLL_INTERVAL_LENGTH);
  Joined := TStringJoinBuffer.Create(',', RaisingPoll);
  try
    Joined.Add(Part);
    Joined.Add(Part);
    Raised := False;
    try
      Joined.Join;
    except
      Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
    Expect<Integer>(Joined.ElementCount).ToBe(0);
    Joined.Add('c');
    Joined.Add('d');
    Expect<string>(Joined.Join).ToBe('c,d');
  finally
    Joined.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TStringJoinTests.Create('StringJoin'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
