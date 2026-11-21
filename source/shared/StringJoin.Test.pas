program StringJoin.Test;

{$I Shared.inc}

uses
  SysUtils,

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
    procedure TestLongJoinIsNotCopiedOut;
    procedure TestAddAfterToString;
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
  Test('ToString hands out the buffer, shrunk to the result, not a copy of it',
    TestLongJoinIsNotCopiedOut);
  Test('ToString can be called again, and the join goes on after it',
    TestAddAfterToString);
end;

function JoinOf(const ASeparator: UnicodeString;
  const AElements: array of UnicodeString): UnicodeString;
var
  Joined: TStringJoinBuffer;
  I: Integer;
begin
  Joined := TStringJoinBuffer.Create(ASeparator);
  for I := 0 to High(AElements) do
    Joined.Add(AElements[I]);
  Expect<Integer>(Joined.ElementCount).ToBe(Length(AElements));
  Result := Joined.ToString;
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
  Text, Joined: UnicodeString;
  Buffer: TStringJoinBuffer;
begin
  Text := StringOfChar('x', 1000);
  Buffer := TStringJoinBuffer.Create(',');
  Buffer.Add(Text);
  Joined := Buffer.ToString;
  Expect<Boolean>(Pointer(Joined) = Pointer(Text)).ToBe(True);

  Buffer := TStringJoinBuffer.Create('');
  Buffer.Add('');
  Buffer.Add(Text);
  Buffer.Add('');
  Joined := Buffer.ToString;
  Expect<Boolean>(Pointer(Joined) = Pointer(Text)).ToBe(True);
end;

procedure TStringJoinTests.TestManyElements;
var
  Joined: TStringJoinBuffer;
  Expected: UnicodeString;
  I: Integer;
begin
  Joined := TStringJoinBuffer.Create('-');
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
  Expect<string>(Joined.ToString).ToBe(Expected);
end;

{ ToString hands out the buffer itself, shrunk to the result, so a second
  call gives the same string and no copy of it exists. }
procedure TStringJoinTests.TestLongJoinIsNotCopiedOut;
var
  Joined: TStringJoinBuffer;
  Text: UnicodeString;
  I: Integer;
begin
  Joined := TStringJoinBuffer.Create(',');
  for I := 0 to 999 do
    Joined.Add(UnicodeString(IntToStr(I)));
  Text := Joined.ToString;
  Expect<Integer>(Length(Text)).ToBe(3889);
  Expect<Boolean>(Pointer(Joined.ToString) = Pointer(Text)).ToBe(True);
end;

procedure TStringJoinTests.TestAddAfterToString;
var
  Joined: TStringJoinBuffer;
  First: UnicodeString;
begin
  Joined := TStringJoinBuffer.Create(',');
  Joined.Add('a');
  Joined.Add('b');
  First := Joined.ToString;
  Expect<string>(Joined.ToString).ToBe('a,b');
  Joined.Add('c');
  Expect<string>(Joined.ToString).ToBe('a,b,c');
  Expect<string>(First).ToBe('a,b');
end;

begin
  TestRunnerProgram.AddSuite(TStringJoinTests.Create('StringJoin'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
