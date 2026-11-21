program StringJoin.Test;

{$I Shared.inc}

uses
  SysUtils,

  AllocationCounter,
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
  Test('Join allocates the result once, at its length',
    TestResultIsAllocatedOnce);
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

{ The strings are kept as they are and written once, into a result of the
  exact length: one allocation, and no spare capacity for the result to keep. }
procedure TStringJoinTests.TestResultIsAllocatedOnce;
var
  Joined: TStringJoinBuffer;
  Text: UnicodeString;
  I: Integer;
  Allocations: Int64;
begin
  Joined := TStringJoinBuffer.Create(',');
  try
    for I := 0 to 999 do
      Joined.Add(UnicodeString(IntToStr(I)));
    StartCountingAllocations;
    try
      Text := Joined.Join;
    finally
      Allocations := StopCountingAllocations;
    end;
    Expect<Integer>(Integer(Allocations)).ToBe(1);
    Expect<Integer>(Length(Text)).ToBe(3889);
  finally
    Joined.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TStringJoinTests.Create('StringJoin'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
