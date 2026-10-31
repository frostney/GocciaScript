program UnicodeStringList.Test;

{$I Shared.inc}

uses
  SysUtils,

  TestingPascalLibrary,
  TimingUtils,
  UnicodeStringList;

const
  { Names in the timed test: a scan of every earlier name per name took
    about 0.5 s here to keep this many unique, and four times as long for
    twice as many. }
  UNIQUE_NAME_COUNT = 20000;
  UNIQUE_NAME_TRIALS = 3;
  UNIQUE_DOUBLING_PERCENT = 250;
  UNIQUE_SLACK_NANOSECONDS = 20 * 1000 * 1000;

type
  TUnicodeStringListTests = class(TTestSuite)
  private
    function NamedList(const ACount: Integer): TUnicodeStringList;
    procedure TestShortListAnswersByScan;
    procedure TestLongListFollowsAdditionsAndRemovals;
    procedure TestRepeatedStringStaysUntilItsLastCopyGoes;
    procedure TestReplacedItemIsForgotten;
    procedure TestComparisonIsExact;
    procedure TestEmptyStringIsFoundAfterCountGrows;
    procedure TestKeepingNamesUniqueIsLinear;
  public
    procedure SetupTests; override;
  end;

procedure TUnicodeStringListTests.SetupTests;
begin
  Test('a short list answers HasString', TestShortListAnswersByScan);
  Test('a long list''s HasString follows additions and removals',
    TestLongListFollowsAdditionsAndRemovals);
  Test('a repeated string stays until its last copy is removed',
    TestRepeatedStringStaysUntilItsLastCopyGoes);
  Test('a replaced item is no longer found', TestReplacedItemIsForgotten);
  Test('HasString compares UTF-16 code units exactly',
    TestComparisonIsExact);
  Test('an empty string added by growing Count is found',
    TestEmptyStringIsFoundAfterCountGrows);
  Test('keeping names unique costs time linear in them',
    TestKeepingNamesUniqueIsLinear);
end;

function TUnicodeStringListTests.NamedList(
  const ACount: Integer): TUnicodeStringList;
var
  I: Integer;
begin
  Result := TUnicodeStringList.Create;
  for I := 0 to ACount - 1 do
    Result.Add('n' + IntToStr(I));
end;

procedure TUnicodeStringListTests.TestShortListAnswersByScan;
var
  List: TUnicodeStringList;
begin
  List := NamedList(3);
  try
    Expect<Boolean>(List.HasString('n0')).ToBe(True);
    Expect<Boolean>(List.HasString('n2')).ToBe(True);
    Expect<Boolean>(List.HasString('n3')).ToBe(False);
    List.Delete(0);
    Expect<Boolean>(List.HasString('n0')).ToBe(False);
  finally
    List.Free;
  end;
end;

procedure TUnicodeStringListTests.TestLongListFollowsAdditionsAndRemovals;
var
  List: TUnicodeStringList;
begin
  List := NamedList(100);
  try
    // The first test on a long list builds its index; the rest follow it.
    Expect<Boolean>(List.HasString('n99')).ToBe(True);
    List.Add('added');
    List.Insert(0, 'inserted');
    Expect<Boolean>(List.HasString('added')).ToBe(True);
    Expect<Boolean>(List.HasString('inserted')).ToBe(True);
    List.Delete(List.IndexOf('n5'));
    List.Remove('n6');
    List.ExtractIndex(List.IndexOf('n7'));
    List.DeleteRange(List.IndexOf('n10'), 3);
    Expect<Boolean>(List.HasString('n5')).ToBe(False);
    Expect<Boolean>(List.HasString('n6')).ToBe(False);
    Expect<Boolean>(List.HasString('n7')).ToBe(False);
    Expect<Boolean>(List.HasString('n10')).ToBe(False);
    Expect<Boolean>(List.HasString('n12')).ToBe(False);
    Expect<Boolean>(List.HasString('n13')).ToBe(True);
    List.Count := 20;
    Expect<Boolean>(List.HasString('n50')).ToBe(False);
    List.Clear;
    Expect<Boolean>(List.HasString('n0')).ToBe(False);
    Expect<Boolean>(List.HasString('added')).ToBe(False);
    List.Add('n0');
    Expect<Boolean>(List.HasString('n0')).ToBe(True);
  finally
    List.Free;
  end;
end;

procedure TUnicodeStringListTests.TestRepeatedStringStaysUntilItsLastCopyGoes;
var
  List: TUnicodeStringList;
begin
  List := NamedList(40);
  try
    List.Add('twice');
    List.Add('twice');
    Expect<Boolean>(List.HasString('twice')).ToBe(True);
    List.Remove('twice');
    Expect<Boolean>(List.HasString('twice')).ToBe(True);
    List.Remove('twice');
    Expect<Boolean>(List.HasString('twice')).ToBe(False);
  finally
    List.Free;
  end;
end;

procedure TUnicodeStringListTests.TestReplacedItemIsForgotten;
var
  List: TUnicodeStringList;
begin
  List := NamedList(40);
  try
    Expect<Boolean>(List.HasString('n3')).ToBe(True);
    List[3] := 'replacement';
    Expect<Boolean>(List.HasString('n3')).ToBe(False);
    Expect<Boolean>(List.HasString('replacement')).ToBe(True);
  finally
    List.Free;
  end;
end;

procedure TUnicodeStringListTests.TestComparisonIsExact;
const
  // Below and above the length at which the list starts its index.
  COUNTS: array[0..1] of Integer = (1, 40);
var
  List: TUnicodeStringList;
  I: Integer;
begin
  for I := Low(COUNTS) to High(COUNTS) do
  begin
    List := NamedList(COUNTS[I]);
    try
      List.Add('caf' + #$00E9);
      Expect<Boolean>(List.HasString('caf' + #$00E9)).ToBe(True);
      // Case and canonically equivalent forms are other names.
      Expect<Boolean>(List.HasString('N0')).ToBe(False);
      Expect<Boolean>(List.HasString('cafe' + #$0301)).ToBe(False);
      Expect<Boolean>(List.HasString('n0' + #$200C)).ToBe(False);
    finally
      List.Free;
    end;
  end;
end;

procedure TUnicodeStringListTests.TestEmptyStringIsFoundAfterCountGrows;
var
  List: TUnicodeStringList;
begin
  List := NamedList(40);
  try
    Expect<Boolean>(List.HasString('')).ToBe(False);
    Expect<Boolean>(List.HasString('n0')).ToBe(True);
    List.Count := 45;
    Expect<Boolean>(List.HasString('')).ToBe(True);
  finally
    List.Free;
  end;
end;

{ Keeping a list unique by testing each name before adding it, as the
  binding-name collectors do: twice the names may take at most 2.5 times as
  long (plus the slack), where a scan per name takes four times as long. }
procedure TUnicodeStringListTests.TestKeepingNamesUniqueIsLinear;

  function UniqueNanoseconds(const ACount: Integer): Int64;
  var
    List: TUnicodeStringList;
    Name: UnicodeString;
    I: Integer;
    Start: Int64;
  begin
    List := TUnicodeStringList.Create;
    try
      Start := GetNanoseconds;
      for I := 0 to ACount - 1 do
      begin
        Name := 'n' + IntToStr(I);
        if not List.HasString(Name) then
          List.Add(Name);
      end;
      Result := GetNanoseconds - Start;
      Expect<Integer>(List.Count).ToBe(ACount);
    finally
      List.Free;
    end;
  end;

var
  Shorter, Longer, Elapsed: Int64;
  Trial: Integer;
begin
  Shorter := High(Int64);
  Longer := High(Int64);
  for Trial := 1 to UNIQUE_NAME_TRIALS do
  begin
    Elapsed := UniqueNanoseconds(UNIQUE_NAME_COUNT);
    if Elapsed < Shorter then
      Shorter := Elapsed;
    Elapsed := UniqueNanoseconds(2 * UNIQUE_NAME_COUNT);
    if Elapsed < Longer then
      Longer := Elapsed;
  end;
  if Longer > Shorter * UNIQUE_DOUBLING_PERCENT div 100 +
     UNIQUE_SLACK_NANOSECONDS then
    Fail(Format('%d unique names took %d ms against %d ms for %d', [
      2 * UNIQUE_NAME_COUNT, Longer div 1000000, Shorter div 1000000,
      UNIQUE_NAME_COUNT]));
  Expect<Boolean>(Longer <= Shorter * UNIQUE_DOUBLING_PERCENT div 100 +
    UNIQUE_SLACK_NANOSECONDS).ToBe(True);
end;

begin
  TestRunnerProgram.AddSuite(
    TUnicodeStringListTests.Create('UnicodeStringList'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
