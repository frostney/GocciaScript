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
    procedure TestSmallJoinAllocatesOnlyItsResult;
    procedure TestRepeatedSeparatorsTakeNoScratch;
    procedure TestFinalCopyPolls;
    procedure TestGateIsAskedBeforeTheResult;
    procedure TestARaisingGateOrPollLeavesTheBufferEmpty;
    procedure TestACopyRefusesToAdd;
    procedure TestGateCountsTablesAndStringsHeldOnlyHere;
    procedure TestEmptySeparatorKeepsNoGaps;
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
  Test('more elements than the record holds, with runs of empty ones, join in order',
    TestManyElements);
  Test('Join allocates the result once, at its exact length',
    TestResultIsAllocatedOnce);
  Test('a join of a few strings allocates nothing but its result',
    TestSmallJoinAllocatesOnlyItsResult);
  Test('separators between empty elements take no memory before the result',
    TestRepeatedSeparatorsTakeNoScratch);
  Test('the final copy polls once per poll interval of characters',
    TestFinalCopyPolls);
  Test('the gate is asked about the tables and the result before they are allocated',
    TestGateIsAskedBeforeTheResult);
  Test('after a gate or poll raises, Release empties the buffer for another Init',
    TestARaisingGateOrPollLeavesTheBufferEmpty);
  Test('a copy of the buffer refuses to add or join',
    TestACopyRefusesToAdd);
  Test('the gate is asked about the tables and the strings held only here as well',
    TestGateCountsTablesAndStringsHeldOnlyHere);
  Test('with an empty separator, empty elements need no bookkeeping',
    TestEmptySeparatorKeepsNoGaps);
end;

function JoinOf(const ASeparator: UnicodeString;
  const AElements: array of UnicodeString): UnicodeString;
var
  Joined: TStringJoinBuffer;
  I: Integer;
begin
  Joined.Init(ASeparator);
  try
    for I := 0 to High(AElements) do
      Joined.Add(AElements[I]);
    Expect<Integer>(Joined.ElementCount).ToBe(Length(AElements));
    Result := Joined.Join;
  finally
    Joined.Release;
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
end;

procedure TStringJoinTests.TestManyElements;
var
  Joined: TStringJoinBuffer;
  Expected: TStringBuffer;
  Text: UnicodeString;
  I: Integer;
begin
  Joined.Init(', ');
  Expected := TStringBuffer.Create;
  for I := 0 to 49999 do
  begin
    if I > 0 then
      Expected.Append(', ');
    if I mod 997 = 0 then
      Text := StringOfChar('L', 300)
    else if (I mod 13 = 0) or (I mod 101 < 3) then
      Text := ''
    else
      Text := UnicodeString(IntToStr(I));
    Joined.Add(Text);
    Expected.Append(Text);
  end;
  Expect<string>(Joined.Join).ToBe(Expected.ToString);
  Expect<Integer>(Joined.ElementCount).ToBe(0);
  Joined.Release;
end;

{ The strings are kept as they are and written once, into a result of the
  exact length: the bytes of the result and nothing as large besides. }
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
  Joined.Init(',');
  for I := 0 to 999 do
    Joined.Add(UnicodeString(IntToStr(I)));
  StartCountingAllocations;
  try
    Text := Joined.Join;
  finally
    StopCountingAllocations;
    Bytes := CountedAllocationBytes;
  end;
  Joined.Release;
  Expect<Integer>(Length(Text)).ToBe(RESULT_LENGTH);
  Expect<Boolean>(Bytes >= RESULT_LENGTH * SizeOf(WideChar)).ToBe(True);
  Expect<Boolean>(Bytes <= RESULT_LENGTH * SizeOf(WideChar) +
    BOOKKEEPING_BYTES).ToBe(True);
end;

procedure TStringJoinTests.TestSmallJoinAllocatesOnlyItsResult;
var
  Joined: TStringJoinBuffer;
  First, Second, Text: UnicodeString;
  Allocations: Int64;
begin
  First := StringOfChar('a', 2);
  Second := StringOfChar('b', 2);
  StartCountingAllocations;
  try
    Joined.Init(',');
    Joined.Add(First);
    Joined.Add(Second);
    Text := Joined.Join;
    Joined.Release;
  finally
    Allocations := StopCountingAllocations;
  end;
  Expect<string>(Text).ToBe('aa,bb');
  Expect<Integer>(Integer(Allocations)).ToBe(1);
end;

procedure TStringJoinTests.TestRepeatedSeparatorsTakeNoScratch;
const
  ELEMENT_COUNT = 60000;
  SEPARATOR_LENGTH = 255;
var
  Joined: TStringJoinBuffer;
  Text: UnicodeString;
  I: Integer;
  Bytes: Int64;
begin
  StartCountingAllocations;
  try
    Joined.Init(StringOfChar('x', SEPARATOR_LENGTH));
    for I := 1 to ELEMENT_COUNT do
      Joined.Add('');
    Text := Joined.Join;
    Joined.Release;
  finally
    StopCountingAllocations;
    Bytes := CountedAllocationBytes;
  end;
  Expect<Integer>(Length(Text))
    .ToBe((ELEMENT_COUNT - 1) * SEPARATOR_LENGTH);
  Expect<Boolean>(Bytes <= (Int64(Length(Text)) + 2 * SEPARATOR_LENGTH) *
    SizeOf(WideChar) + 1024).ToBe(True);
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
  Part, Text, Separator: UnicodeString;
  I: Integer;
begin
  // A separator longer than the interval is polled within, as a string is.
  Separator := StringOfChar('s', POLL_INTERVAL_LENGTH * 2 + 7);
  PollCount := 0;
  Joined.Init(Separator, CountPoll);
  for I := 1 to 3 do
    Joined.Add('x');
  Text := Joined.Join;
  Joined.Release;
  Expect<Integer>(Length(Text)).ToBe(2 * Length(Separator) + 3);
  Expect<Integer>(PollCount).ToBe(Length(Text) div POLL_INTERVAL_LENGTH);

  Part := StringOfChar('p', POLL_INTERVAL_LENGTH);
  PollCount := 0;
  Joined.Init(',', CountPoll);
  for I := 1 to PART_COUNT do
    Joined.Add(Part);
  Expect<Integer>(PollCount).ToBe(0);
  Text := Joined.Join;
  Joined.Release;
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
  Joined.Init(',', nil, RecordGate);
  for I := 0 to INLINE_PART_COUNT - 1 do
    Joined.Add(UnicodeString(IntToStr(I)));
  // The record holds these: nothing allocated, nothing asked.
  Expect<Integer>(GateCalls).ToBe(0);
  Joined.Add('x');
  Expect<Integer>(GateCalls).ToBe(1);
  Expect<Boolean>(LastGateBytes = 2 * INLINE_PART_COUNT * SizeOf(Pointer))
    .ToBe(True);
  Text := Joined.Join;
  Expect<Integer>(GateCalls).ToBe(2);
  // The result, beside the table it is copied from.
  Expect<Boolean>(LastGateBytes = Length(Text) * SizeOf(WideChar) +
    2 * INLINE_PART_COUNT * SizeOf(Pointer)).ToBe(True);
  // A lone string is the result itself: nothing is allocated, nothing asked.
  Joined.Add(Text);
  Expect<Boolean>(Pointer(Joined.Join) = Pointer(Text)).ToBe(True);
  Expect<Integer>(GateCalls).ToBe(2);
  Joined.Release;
end;

function JoinRaises(var AJoined: TStringJoinBuffer): Boolean;
begin
  Result := False;
  try
    AJoined.Join;
  except
    Result := True;
  end;
end;

procedure TStringJoinTests.TestARaisingGateOrPollLeavesTheBufferEmpty;
var
  Joined: TStringJoinBuffer;
  Part: UnicodeString;
  I: Integer;
begin
  // Above the last table growth's ask, the old table and the new, below the
  // result's 17,778 bytes beside the new table, whatever a pointer's size.
  GateRefusesAbove := 3 * 1024 * SizeOf(Pointer) + 1;
  Joined.Init(',', nil, RecordGate);
  for I := 0 to 1999 do
    Joined.Add(UnicodeString(IntToStr(I)));
  Expect<Boolean>(JoinRaises(Joined)).ToBe(True);
  Joined.Release;
  Joined.Init(',', nil, RecordGate);
  Expect<Integer>(Joined.ElementCount).ToBe(0);
  Joined.Add('a');
  Joined.Add('b');
  Expect<string>(Joined.Join).ToBe('a,b');
  Joined.Release;

  Part := StringOfChar('p', POLL_INTERVAL_LENGTH);
  Joined.Init(',', RaisingPoll);
  Joined.Add(Part);
  Joined.Add(Part);
  Expect<Boolean>(JoinRaises(Joined)).ToBe(True);
  Joined.Release;
  Joined.Init(',');
  Expect<Integer>(Joined.ElementCount).ToBe(0);
  Joined.Add('c');
  Joined.Add('d');
  Expect<string>(Joined.Join).ToBe('c,d');
  Joined.Release;
end;

procedure TStringJoinTests.TestACopyRefusesToAdd;
var
  Joined, Copied: TStringJoinBuffer;
  Raised: Boolean;
begin
  Joined.Init(',');
  Joined.Add('a');
  Copied := Joined;
  Raised := False;
  try
    Copied.Add('b');
  except
    on EStringJoinCopied do
      Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
  Joined.Add('c');
  Expect<string>(Joined.Join).ToBe('a,c');
  Joined.Release;
end;

procedure TStringJoinTests.TestGateCountsTablesAndStringsHeldOnlyHere;
var
  Joined: TStringJoinBuffer;
  Text: UnicodeString;
  I: Integer;
  TableBytes: Int64;
begin
  GateCalls := 0;
  GateRefusesAbove := High(Int64);
  Joined.Init(',', nil, RecordGate);
  for I := 0 to INLINE_PART_COUNT do
    Joined.Add(StringOfChar('h', 10), True);
  // The first table, for the part past the record's.
  TableBytes := 2 * INLINE_PART_COUNT * SizeOf(Pointer);
  Expect<Boolean>(LastGateBytes = INLINE_PART_COUNT * 10 * SizeOf(WideChar) +
    TableBytes).ToBe(True);
  Text := Joined.Join;
  // The result, beside the table and every string held only here.
  Expect<Boolean>(LastGateBytes = (INLINE_PART_COUNT + 1) * 10 *
    SizeOf(WideChar) + TableBytes + Length(Text) * SizeOf(WideChar))
    .ToBe(True);
  Joined.Release;
end;

procedure TStringJoinTests.TestEmptySeparatorKeepsNoGaps;
var
  Joined: TStringJoinBuffer;
  I: Integer;
begin
  GateCalls := 0;
  GateRefusesAbove := High(Int64);
  Joined.Init('', nil, RecordGate);
  for I := 0 to INLINE_PART_COUNT - 1 do
  begin
    Joined.Add('x');
    Joined.Add('');
  end;
  // The parts fit the record, and the empty elements took nothing.
  Expect<Integer>(GateCalls).ToBe(0);
  Expect<string>(Joined.Join).ToBe(StringOfChar('x', INLINE_PART_COUNT));
  Joined.Release;
end;

begin
  TestRunnerProgram.AddSuite(TStringJoinTests.Create('StringJoin'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
