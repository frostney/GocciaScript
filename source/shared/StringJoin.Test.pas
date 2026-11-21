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
  Text: UnicodeString;
begin
  Text := StringOfChar('x', 1000);
  Expect<Boolean>(Pointer(JoinOf(',', [Text])) = Pointer(Text)).ToBe(True);
  Expect<Boolean>(Pointer(JoinOf('', ['', Text, ''])) = Pointer(Text))
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

begin
  TestRunnerProgram.AddSuite(TStringJoinTests.Create('StringJoin'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
