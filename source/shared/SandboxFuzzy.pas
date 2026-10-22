unit SandboxFuzzy;

{ Fuzzy path matching for the sandbox's fuzzyFind: fzf's version 1
  algorithm with its path scheme, over UTF-16 code units and with ASCII-only
  case folding. docs/built-ins-sandbox-search.md#scoring is the
  specification and fixtures/sandbox-search/vectors.json pins it. }

{$I Shared.inc}

interface

type
  TSandboxFuzzyPositions = array of Integer;

  TSandboxFuzzyMatch = record
    Path: string;
    Score: Integer;
    { Zero-based UTF-16 code unit indexes into Path. }
    Positions: TSandboxFuzzyPositions;
  end;

  TSandboxFuzzyMatchArray = array of TSandboxFuzzyMatch;

{ Scores APath against a non-empty AQuery; False when the query's
  characters do not all appear in order. }
function TrySandboxFuzzyMatch(const APath, AQuery: string;
  out AMatch: TSandboxFuzzyMatch): Boolean;

{ Highest score first, then the shorter path, then path order. }
procedure SortSandboxFuzzyMatches(var AMatches: TSandboxFuzzyMatchArray);

implementation

uses
  SandboxGlob;

const
  SCORE_MATCH = 16;
  SCORE_GAP_START = -3;
  SCORE_GAP_EXTENSION = -1;
  BONUS_BOUNDARY = 8;
  BONUS_NON_WORD = 8;
  BONUS_CAMEL_123 = 7;
  BONUS_CONSECUTIVE = 4;
  BONUS_FIRST_CHARACTER_MULTIPLIER = 2;
  BONUS_BOUNDARY_WHITE = 8;
  BONUS_BOUNDARY_DELIMITER = 9;
  ASCII_LIMIT = $80;
  ASCII_CASE_OFFSET = Ord('a') - Ord('A');

type
  { Ordered as in fzf: the bonus rules compare against charNonWord. }
  TSandboxFuzzyCharacterClass = (fccWhite, fccNonWord, fccDelimiter, fccLower,
    fccUpper, fccLetter, fccNumber);

function CharacterClassOf(const ACharacter: Char): TSandboxFuzzyCharacterClass;
begin
  case ACharacter of
    'a'..'z':
      Result := fccLower;
    'A'..'Z':
      Result := fccUpper;
    '0'..'9':
      Result := fccNumber;
    '/':
      Result := fccDelimiter;
    ' ', #9..#13:
      Result := fccWhite;
  else
    if Ord(ACharacter) >= ASCII_LIMIT then
      Result := fccLetter
    else
      Result := fccNonWord;
  end;
end;

function BonusFor(const APrevious,
  ACurrent: TSandboxFuzzyCharacterClass): Integer;
begin
  if ACurrent > fccNonWord then
    case APrevious of
      fccWhite:
        Exit(BONUS_BOUNDARY_WHITE);
      fccDelimiter:
        Exit(BONUS_BOUNDARY_DELIMITER);
      fccNonWord:
        Exit(BONUS_BOUNDARY);
    end;
  if ((APrevious = fccLower) and (ACurrent = fccUpper)) or
     ((APrevious <> fccNumber) and (ACurrent = fccNumber)) then
    Exit(BONUS_CAMEL_123);
  case ACurrent of
    fccNonWord, fccDelimiter:
      Result := BONUS_NON_WORD;
    fccWhite:
      Result := BONUS_BOUNDARY_WHITE;
  else
    Result := 0;
  end;
end;

function FoldCase(const ACharacter: Char): Char; inline;
begin
  if (ACharacter >= 'A') and (ACharacter <= 'Z') then
    Result := Char(Ord(ACharacter) + ASCII_CASE_OFFSET)
  else
    Result := ACharacter;
end;

function Max3(const A, B, C: Integer): Integer;
begin
  Result := A;
  if B > Result then
    Result := B;
  if C > Result then
    Result := C;
end;

function TrySandboxFuzzyMatch(const APath, AQuery: string;
  out AMatch: TSandboxFuzzyMatch): Boolean;
var
  Query: string;
  QueryIndex, Index, StartIndex, EndIndex, Score, Consecutive, FirstBonus,
    Bonus, Count: Integer;
  InGap: Boolean;
  Previous, Current: TSandboxFuzzyCharacterClass;
begin
  AMatch := Default(TSandboxFuzzyMatch);
  if AQuery = '' then
    Exit(False);
  Query := AQuery;
  for Index := 1 to Length(Query) do
    Query[Index] := FoldCase(Query[Index]);

  { Forwards: the first position where every query character was seen. }
  QueryIndex := 1;
  StartIndex := 0;
  EndIndex := 0;
  for Index := 1 to Length(APath) do
    if FoldCase(APath[Index]) = Query[QueryIndex] then
    begin
      if StartIndex = 0 then
        StartIndex := Index;
      Inc(QueryIndex);
      if QueryIndex > Length(Query) then
      begin
        EndIndex := Index;
        Break;
      end;
    end;
  if EndIndex = 0 then
    Exit(False);

  { Backwards: the latest start that still holds the whole query. }
  QueryIndex := Length(Query);
  for Index := EndIndex downto StartIndex do
    if FoldCase(APath[Index]) = Query[QueryIndex] then
    begin
      Dec(QueryIndex);
      if QueryIndex = 0 then
      begin
        StartIndex := Index;
        Break;
      end;
    end;

  { Score the range, matching greedily forwards. }
  SetLength(AMatch.Positions, Length(Query));
  Count := 0;
  QueryIndex := 1;
  Score := 0;
  InGap := False;
  Consecutive := 0;
  FirstBonus := 0;
  if StartIndex > 1 then
    Previous := CharacterClassOf(APath[StartIndex - 1])
  else
    Previous := fccDelimiter;
  for Index := StartIndex to EndIndex do
  begin
    Current := CharacterClassOf(APath[Index]);
    if (QueryIndex <= Length(Query)) and
       (FoldCase(APath[Index]) = Query[QueryIndex]) then
    begin
      AMatch.Positions[Count] := Index - 1;
      Inc(Count);
      Inc(Score, SCORE_MATCH);
      Bonus := BonusFor(Previous, Current);
      if Consecutive = 0 then
        FirstBonus := Bonus
      else
      begin
        if (Bonus >= BONUS_BOUNDARY) and (Bonus > FirstBonus) then
          FirstBonus := Bonus;
        Bonus := Max3(Bonus, FirstBonus, BONUS_CONSECUTIVE);
      end;
      if QueryIndex = 1 then
        Inc(Score, Bonus * BONUS_FIRST_CHARACTER_MULTIPLIER)
      else
        Inc(Score, Bonus);
      InGap := False;
      Inc(Consecutive);
      Inc(QueryIndex);
    end
    else
    begin
      if InGap then
        Inc(Score, SCORE_GAP_EXTENSION)
      else
        Inc(Score, SCORE_GAP_START);
      InGap := True;
      Consecutive := 0;
      FirstBonus := 0;
    end;
    Previous := Current;
  end;
  SetLength(AMatch.Positions, Count);
  AMatch.Path := APath;
  AMatch.Score := Score;
  Result := True;
end;

function CompareMatches(const A, B: TSandboxFuzzyMatch): Integer;
begin
  if A.Score <> B.Score then
  begin
    if A.Score > B.Score then
      Exit(-1);
    Exit(1);
  end;
  if Length(A.Path) <> Length(B.Path) then
  begin
    if Length(A.Path) < Length(B.Path) then
      Exit(-1);
    Exit(1);
  end;
  Result := CompareSandboxPaths(A.Path, B.Path);
end;

procedure MergeSortMatches(var AMatches, AScratch: TSandboxFuzzyMatchArray;
  const ALow, AHigh: Integer);
var
  Middle, Left, Right, Target: Integer;
begin
  if AHigh <= ALow then
    Exit;
  Middle := (ALow + AHigh) div 2;
  MergeSortMatches(AMatches, AScratch, ALow, Middle);
  MergeSortMatches(AMatches, AScratch, Middle + 1, AHigh);
  Left := ALow;
  Right := Middle + 1;
  Target := ALow;
  while (Left <= Middle) and (Right <= AHigh) do
  begin
    if CompareMatches(AMatches[Right], AMatches[Left]) < 0 then
    begin
      AScratch[Target] := AMatches[Right];
      Inc(Right);
    end
    else
    begin
      AScratch[Target] := AMatches[Left];
      Inc(Left);
    end;
    Inc(Target);
  end;
  while Left <= Middle do
  begin
    AScratch[Target] := AMatches[Left];
    Inc(Left);
    Inc(Target);
  end;
  while Right <= AHigh do
  begin
    AScratch[Target] := AMatches[Right];
    Inc(Right);
    Inc(Target);
  end;
  for Target := ALow to AHigh do
    AMatches[Target] := AScratch[Target];
end;

procedure SortSandboxFuzzyMatches(var AMatches: TSandboxFuzzyMatchArray);
var
  Scratch: TSandboxFuzzyMatchArray;
begin
  Scratch := nil;
  SetLength(Scratch, Length(AMatches));
  MergeSortMatches(AMatches, Scratch, 0, High(AMatches));
end;

end.
