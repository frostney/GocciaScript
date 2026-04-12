unit Goccia.RegExp.Engine;

{$I Goccia.inc}

interface

type
  TGocciaRegExpMatchGroup = record
    Matched: Boolean;
    Value: string;
  end;

  TGocciaRegExpMatchGroups = array of TGocciaRegExpMatchGroup;

  TGocciaRegExpNamedGroup = record
    Name: string;
    Index: Integer;
    DisjunctionPath: array of Integer;
  end;

  TGocciaRegExpNamedGroups = array of TGocciaRegExpNamedGroup;

  TGocciaRegExpMatchResult = record
    Found: Boolean;
    MatchIndex: Integer;
    MatchEnd: Integer;
    NextIndex: Integer;
    Groups: TGocciaRegExpMatchGroups;
    NamedGroups: TGocciaRegExpNamedGroups;
  end;

function NormalizeRegExpSource(const APattern: string): string;
function HasRegExpFlag(const AFlags: string; const AFlag: Char): Boolean;
procedure ValidateRegExpFlags(const AFlags: string);
procedure ValidateRegExpPattern(const APattern, AFlags: string);
function CanonicalizeRegExpFlags(const AFlags: string): string;
function RegExpToString(const APattern, AFlags: string): string;
function ExecuteRegExp(const APattern, AFlags, AInput: string;
  const AStartIndex: Integer; const ARequireStart: Boolean;
  out AResult: TGocciaRegExpMatchResult): Boolean;
function PreprocessRegExpPattern(const APattern: string;
  out ANamedGroups: TGocciaRegExpNamedGroups): string;

implementation

uses
  Math,
  StrUtils,
  SysUtils,

  RegExpr,

  Goccia.RegExp.Unicode;

const
  EMPTY_REGEX = '(?:)';
  REGEXP_FLAG_ORDER = 'dgimsuvy';

function NormalizeRegExpSource(const APattern: string): string;
begin
  if APattern = '' then
    Result := EMPTY_REGEX
  else
    Result := APattern;
end;

function GetExecutableRegExpPattern(const APattern: string): string;
begin
  if APattern = EMPTY_REGEX then
    Result := ''
  else
    Result := APattern;
end;

function HasRegExpFlag(const AFlags: string; const AFlag: Char): Boolean;
begin
  Result := Pos(AFlag, AFlags) > 0;
end;

procedure ValidateRegExpFlags(const AFlags: string);
var
  Seen: string;
  I: Integer;
begin
  Seen := '';
  for I := 1 to Length(AFlags) do
  begin
    if not CharInSet(AFlags[I], ['d', 'g', 'i', 'm', 's', 'u', 'v', 'y']) then
      raise EConvertError.Create('Invalid regular expression flags');
    if Pos(AFlags[I], Seen) > 0 then
      raise EConvertError.Create('Invalid regular expression flags');
    Seen := Seen + AFlags[I];
  end;
  // ES2026 §22.2.2.1: u and v flags are mutually exclusive
  if HasRegExpFlag(AFlags, 'u') and HasRegExpFlag(AFlags, 'v') then
    raise EConvertError.Create('Invalid regular expression flags');
end;

// ES2026 §22.2.3.1 RegExp ( pattern, flags ) — validation step
procedure ValidateRegExpPattern(const APattern, AFlags: string);
var
  Matcher: TRegExpr;
  NormalizedPattern: string;
  ConvertedPattern: string;
  DiscardedGroups: TGocciaRegExpNamedGroups;
  IsUnicode: Boolean;
begin
  ValidateRegExpFlags(AFlags);
  NormalizedPattern := NormalizeRegExpSource(APattern);
  if NormalizedPattern = EMPTY_REGEX then
    Exit;
  IsUnicode := HasRegExpFlag(AFlags, 'u');
  ConvertedPattern := PreprocessRegExpPattern(
    GetExecutableRegExpPattern(NormalizedPattern), DiscardedGroups);
  // ES2026 §22.2.2.9: Apply Unicode pattern preprocessing when u flag is set
  if IsUnicode then
    ConvertedPattern := PreprocessUnicodePattern(ConvertedPattern);
  Matcher := TRegExpr.Create;
  try
    Matcher.Expression := ConvertedPattern;
    Matcher.ModifierI := HasRegExpFlag(AFlags, 'i');
    Matcher.ModifierM := HasRegExpFlag(AFlags, 'm');
    Matcher.ModifierS := HasRegExpFlag(AFlags, 's');
    if IsUnicode then
      Matcher.ModifierR := False;
    Matcher.Compile;
  finally
    Matcher.Free;
  end;
end;

function CanonicalizeRegExpFlags(const AFlags: string): string;
var
  I: Integer;
begin
  ValidateRegExpFlags(AFlags);
  Result := '';
  for I := 1 to Length(REGEXP_FLAG_ORDER) do
  begin
    if HasRegExpFlag(AFlags, REGEXP_FLAG_ORDER[I]) then
      Result := Result + REGEXP_FLAG_ORDER[I];
  end;
end;

function RegExpToString(const APattern, AFlags: string): string;
begin
  Result := '/' + StringReplace(APattern, '/', '\/', [rfReplaceAll]) + '/' +
    CanonicalizeRegExpFlags(AFlags);
end;

// ES2026 §22.2.7.2 AdvanceStringIndex ( S, index, unicode )
function AdvanceStringIndex(const AInput: string; const AIndex: Integer;
  const AUnicode: Boolean): Integer;
var
  LeadByte: Byte;
begin
  if AIndex >= Length(AInput) then
  begin
    Result := AIndex + 1;
    Exit;
  end;
  if not AUnicode then
  begin
    Result := AIndex + 1;
    Exit;
  end;
  LeadByte := Ord(AInput[AIndex + 1]);
  if LeadByte < $80 then
    Result := AIndex + 1
  else if (LeadByte and $E0) = $C0 then
    Result := Min(AIndex + 2, Length(AInput))
  else if (LeadByte and $F0) = $E0 then
    Result := Min(AIndex + 3, Length(AInput))
  else if (LeadByte and $F8) = $F0 then
    Result := Min(AIndex + 4, Length(AInput))
  else
    Result := AIndex + 1;
end;

function FindNamedGroupIndex(const ANamedGroups: TGocciaRegExpNamedGroups;
  const AName: string): Integer;
var
  I: Integer;
begin
  for I := 0 to High(ANamedGroups) do
    if ANamedGroups[I].Name = AName then
      Exit(ANamedGroups[I].Index);
  Result := -1;
end;

// ES2025 §22.2.1 Static Semantics: Early Errors — duplicate GroupSpecifier
// Two disjunction paths share a branch if they agree at every common depth.
// When they share a branch, both groups can participate in the same match —
// making duplicate names a SyntaxError.
function PathsShareBranch(const APathA, APathB: array of Integer): Boolean;
var
  MinLength, I: Integer;
begin
  MinLength := Min(Length(APathA), Length(APathB));
  for I := 0 to MinLength - 1 do
    if APathA[I] <> APathB[I] then
      Exit(False);
  Result := True;
end;

// ES2025 §22.2.2 Runtime Semantics: CompileAtom — \k GroupName
// Resolve \k<name> backreference when multiple groups share the same name.
// Returns the TRegExpr-compatible backreference string.
//
// When the backreference is outside the disjunction containing the duplicate
// groups (CompatCount = 0 or > 1), we emit (?:\N1|\N2|...) — an alternation
// of all candidate backreferences. This is correct because TRegExpr fails
// (rather than matching empty) when a backreference targets a non-participating
// group, so the alternation falls through to the participating group's backref.
// Concatenation (\N1\N2) would be wrong: the non-participating backref would
// fail and abort the entire match.
function ResolveNamedBackreference(
  const ANamedGroups: TGocciaRegExpNamedGroups;
  const AName: string; const ACurrentPath: array of Integer): string;
var
  AllIndices: array of Integer;
  CompatibleIndices: array of Integer;
  AllCount, CompatCount, I: Integer;
begin
  Result := '';
  // Collect all group indices with this name
  AllCount := 0;
  for I := 0 to High(ANamedGroups) do
    if ANamedGroups[I].Name = AName then
      Inc(AllCount);
  if AllCount = 0 then
    Exit;
  if AllCount = 1 then
  begin
    // Single group — simple backreference (ES2018 behavior)
    Result := '\' + IntToStr(FindNamedGroupIndex(ANamedGroups, AName));
    Exit;
  end;
  // ES2025: Multiple groups with same name — resolve via disjunction path
  SetLength(CompatibleIndices, AllCount);
  CompatCount := 0;
  for I := 0 to High(ANamedGroups) do
    if (ANamedGroups[I].Name = AName) and
       PathsShareBranch(ANamedGroups[I].DisjunctionPath, ACurrentPath) then
    begin
      CompatibleIndices[CompatCount] := ANamedGroups[I].Index;
      Inc(CompatCount);
    end;
  if CompatCount = 1 then
  begin
    // Exactly one compatible group — resolve directly
    Result := '\' + IntToStr(CompatibleIndices[0]);
    Exit;
  end;
  if CompatCount = 0 then
  begin
    // Backreference outside the disjunction — collect all groups with this name
    SetLength(AllIndices, AllCount);
    AllCount := 0;
    for I := 0 to High(ANamedGroups) do
      if ANamedGroups[I].Name = AName then
      begin
        AllIndices[AllCount] := ANamedGroups[I].Index;
        Inc(AllCount);
      end;
    // Emit alternation: (?:\1|\2|...) — the participating group's backreference
    // succeeds while non-participating ones either match empty or fail through
    Result := '(?:';
    for I := 0 to AllCount - 1 do
    begin
      if I > 0 then
        Result := Result + '|';
      Result := Result + '\' + IntToStr(AllIndices[I]);
    end;
    Result := Result + ')';
    Exit;
  end;
  // Multiple compatible groups — emit alternation of compatible ones
  Result := '(?:';
  for I := 0 to CompatCount - 1 do
  begin
    if I > 0 then
      Result := Result + '|';
    Result := Result + '\' + IntToStr(CompatibleIndices[I]);
  end;
  Result := Result + ')';
end;

// Pass 1: collect all named groups and their capture indices without modifying
// the pattern, so that forward \k<name> backreferences can be resolved.
// ES2025: Also tracks disjunction paths and validates duplicate named groups.
function CollectNamedGroups(const APattern: string): TGocciaRegExpNamedGroups;
var
  I, J, K, L, PatternLength, GroupIndex, CloseAngle: Integer;
  InCharClass: Boolean;
  GroupName: string;
  AltStack: array of Integer;
  AltStackDepth: Integer;
begin
  SetLength(Result, 0);
  PatternLength := Length(APattern);
  I := 1;
  GroupIndex := 0;
  InCharClass := False;
  // ES2025: Initialize disjunction path stack with top-level scope
  SetLength(AltStack, 64);
  AltStackDepth := 0;
  AltStack[0] := 0;
  while I <= PatternLength do
  begin
    if APattern[I] = '\' then
    begin
      if I + 1 <= PatternLength then
        Inc(I, 2)
      else
        Inc(I);
      Continue;
    end;
    if APattern[I] = '[' then
    begin
      InCharClass := True;
      Inc(I);
      Continue;
    end;
    if (APattern[I] = ']') and InCharClass then
    begin
      InCharClass := False;
      Inc(I);
      Continue;
    end;
    if InCharClass then
    begin
      Inc(I);
      Continue;
    end;
    // ES2025: Track disjunction alternatives
    if APattern[I] = '|' then
    begin
      Inc(AltStack[AltStackDepth]);
      Inc(I);
      Continue;
    end;
    if APattern[I] = ')' then
    begin
      if AltStackDepth > 0 then
        Dec(AltStackDepth);
      Inc(I);
      Continue;
    end;
    if APattern[I] = '(' then
    begin
      // Push disjunction level for all group types
      Inc(AltStackDepth);
      if AltStackDepth >= Length(AltStack) then
        SetLength(AltStack, AltStackDepth * 2 + 4);
      AltStack[AltStackDepth] := 0;
      if (I + 1 <= PatternLength) and (APattern[I + 1] = '?') then
      begin
        if (I + 2 <= PatternLength) and (APattern[I + 2] = '<') then
        begin
          // (?<= lookbehind, (?<! negative lookbehind — skip
          if (I + 3 <= PatternLength) and
             ((APattern[I + 3] = '=') or (APattern[I + 3] = '!')) then
          begin
            Inc(I, 3);
            Continue;
          end;
          // Named capture group (?<name>...)
          CloseAngle := I + 3;
          while (CloseAngle <= PatternLength) and
                (APattern[CloseAngle] <> '>') do
            Inc(CloseAngle);
          if CloseAngle <= PatternLength then
          begin
            Inc(GroupIndex);
            GroupName := Copy(APattern, I + 3, CloseAngle - I - 3);
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)].Name := GroupName;
            Result[High(Result)].Index := GroupIndex;
            // ES2025: Record disjunction path for duplicate name validation
            SetLength(Result[High(Result)].DisjunctionPath, AltStackDepth + 1);
            for J := 0 to AltStackDepth do
              Result[High(Result)].DisjunctionPath[J] := AltStack[J];
            I := CloseAngle + 1;
            Continue;
          end;
        end;
        // Non-capturing or other (?...) group — skip without incrementing index
        Inc(I, 2);
        Continue;
      end;
      // Plain capturing group
      Inc(GroupIndex);
    end;
    Inc(I);
  end;
  // ES2025 §22.2.1.1: Validate duplicate named capture groups are in different
  // alternatives. Two groups with the same name that share a disjunction branch
  // can both participate in a single match — that is a SyntaxError.
  for K := 0 to High(Result) - 1 do
    for L := K + 1 to High(Result) do
      if (Result[K].Name = Result[L].Name) and
         PathsShareBranch(Result[K].DisjunctionPath,
           Result[L].DisjunctionPath) then
        raise EConvertError.CreateFmt(
          'Duplicate named capture group: %s', [Result[K].Name]);
end;

// Pass 2: convert named groups to plain capturing groups and resolve \k<name>
// backreferences using the complete group map from pass 1.
// ES2025: Tracks disjunction paths for correct \k<name> resolution with
// duplicate named capture groups.
function PreprocessRegExpPattern(const APattern: string;
  out ANamedGroups: TGocciaRegExpNamedGroups): string;
var
  I, J, PatternLength: Integer;
  InCharClass: Boolean;
  GroupName: string;
  CloseAngle: Integer;
  BackrefResult: string;
  AltStack: array of Integer;
  AltStackDepth: Integer;
begin
  // Pass 1: collect all named groups so forward backreferences resolve
  ANamedGroups := CollectNamedGroups(APattern);
  PatternLength := Length(APattern);
  if PatternLength = 0 then
  begin
    Result := '';
    Exit;
  end;
  // Pass 2: emit converted pattern with disjunction path tracking
  Result := '';
  I := 1;
  InCharClass := False;
  // ES2025: Track disjunction path for \k<name> resolution
  SetLength(AltStack, 64);
  AltStackDepth := 0;
  AltStack[0] := 0;
  while I <= PatternLength do
  begin
    if APattern[I] = '\' then
    begin
      if I + 1 <= PatternLength then
      begin
        // \k<name> backreference: convert to numeric backreference(s)
        if (APattern[I + 1] = 'k') and (I + 2 <= PatternLength) and
           (APattern[I + 2] = '<') then
        begin
          CloseAngle := I + 3;
          while (CloseAngle <= PatternLength) and
                (APattern[CloseAngle] <> '>') do
            Inc(CloseAngle);
          if CloseAngle <= PatternLength then
          begin
            GroupName := Copy(APattern, I + 3, CloseAngle - I - 3);
            // ES2025: Resolve with duplicate named group awareness
            BackrefResult := ResolveNamedBackreference(ANamedGroups,
              GroupName, Copy(AltStack, 0, AltStackDepth + 1));
            if BackrefResult = '' then
              raise EConvertError.CreateFmt(
                'Invalid named backreference: %s', [GroupName]);
            Result := Result + BackrefResult;
            I := CloseAngle + 1;
            Continue;
          end;
        end;
        Result := Result + APattern[I] + APattern[I + 1];
        Inc(I, 2);
      end
      else
      begin
        Result := Result + APattern[I];
        Inc(I);
      end;
      Continue;
    end;
    if APattern[I] = '[' then
    begin
      InCharClass := True;
      Result := Result + APattern[I];
      Inc(I);
      Continue;
    end;
    if (APattern[I] = ']') and InCharClass then
    begin
      InCharClass := False;
      Result := Result + APattern[I];
      Inc(I);
      Continue;
    end;
    if InCharClass then
    begin
      Result := Result + APattern[I];
      Inc(I);
      Continue;
    end;
    // ES2025: Track disjunction alternatives
    if APattern[I] = '|' then
    begin
      Inc(AltStack[AltStackDepth]);
      Result := Result + '|';
      Inc(I);
      Continue;
    end;
    if APattern[I] = ')' then
    begin
      if AltStackDepth > 0 then
        Dec(AltStackDepth);
      Result := Result + ')';
      Inc(I);
      Continue;
    end;
    if APattern[I] = '(' then
    begin
      // Push disjunction level for all group types
      Inc(AltStackDepth);
      if AltStackDepth >= Length(AltStack) then
        SetLength(AltStack, AltStackDepth * 2 + 4);
      AltStack[AltStackDepth] := 0;
      if (I + 1 <= PatternLength) and (APattern[I + 1] = '?') then
      begin
        if (I + 2 <= PatternLength) and (APattern[I + 2] = '<') then
        begin
          // (?<= lookbehind, (?<! negative lookbehind
          if (I + 3 <= PatternLength) and
             ((APattern[I + 3] = '=') or (APattern[I + 3] = '!')) then
          begin
            Result := Result + '(?<';
            Inc(I, 3);
            Continue;
          end;
          // Named capture group (?<name>...) -> plain capturing group (...)
          CloseAngle := I + 3;
          while (CloseAngle <= PatternLength) and
                (APattern[CloseAngle] <> '>') do
            Inc(CloseAngle);
          if CloseAngle <= PatternLength then
          begin
            // Strip the name, emit plain capturing group
            Result := Result + '(';
            I := CloseAngle + 1;
            Continue;
          end;
        end;
        Result := Result + '(?';
        Inc(I, 2);
        Continue;
      end;
      Result := Result + APattern[I];
      Inc(I);
      Continue;
    end;
    Result := Result + APattern[I];
    Inc(I);
  end;
end;

// ES2026 §22.2.7.1 RegExpExec ( R, S )
function ExecuteRegExp(const APattern, AFlags, AInput: string;
  const AStartIndex: Integer; const ARequireStart: Boolean;
  out AResult: TGocciaRegExpMatchResult): Boolean;
var
  Matcher: TRegExpr;
  I: Integer;
  ConvertedPattern: string;
  NamedGroups: TGocciaRegExpNamedGroups;
  IsUnicode: Boolean;
begin
  AResult.Found := False;
  AResult.MatchIndex := -1;
  AResult.MatchEnd := -1;
  AResult.NextIndex := -1;
  SetLength(AResult.Groups, 0);
  SetLength(AResult.NamedGroups, 0);
  ValidateRegExpFlags(AFlags);
  IsUnicode := HasRegExpFlag(AFlags, 'u');
  if AStartIndex > Length(AInput) then
    Exit(False);
  if APattern = EMPTY_REGEX then
  begin
    AResult.Found := True;
    AResult.MatchIndex := AStartIndex;
    AResult.MatchEnd := AStartIndex;
    AResult.NextIndex := AdvanceStringIndex(AInput, AStartIndex,
      IsUnicode or HasRegExpFlag(AFlags, 'v'));
    SetLength(AResult.Groups, 1);
    AResult.Groups[0].Matched := True;
    AResult.Groups[0].Value := '';
    Exit(True);
  end;
  ConvertedPattern := PreprocessRegExpPattern(
    GetExecutableRegExpPattern(APattern), NamedGroups);
  // ES2026 §22.2.2.9: Apply Unicode pattern preprocessing when u flag is set
  if IsUnicode then
    ConvertedPattern := PreprocessUnicodePattern(ConvertedPattern);
  Matcher := TRegExpr.Create;
  try
    Matcher.Expression := ConvertedPattern;
    Matcher.ModifierI := HasRegExpFlag(AFlags, 'i');
    Matcher.ModifierM := HasRegExpFlag(AFlags, 'm');
    Matcher.ModifierS := HasRegExpFlag(AFlags, 's');
    if IsUnicode then
      Matcher.ModifierR := False;
    Matcher.Compile;
    Matcher.InputString := AInput;
    Result := Matcher.ExecPos(AStartIndex + 1);
    if Result and ARequireStart and
       (Matcher.MatchPos[0] <> AStartIndex + 1) then
      Result := False;
    if not Result then
      Exit(False);
    AResult.Found := True;
    AResult.MatchIndex := Matcher.MatchPos[0] - 1;
    AResult.MatchEnd := AResult.MatchIndex + Matcher.MatchLen[0];
    AResult.NextIndex := AResult.MatchEnd;
    if Matcher.MatchLen[0] = 0 then
      AResult.NextIndex := AdvanceStringIndex(AInput, AResult.NextIndex,
        IsUnicode or HasRegExpFlag(AFlags, 'v'));
    SetLength(AResult.Groups, Matcher.SubExprMatchCount + 1);
    for I := 0 to Matcher.SubExprMatchCount do
    begin
      AResult.Groups[I].Matched := Matcher.MatchPos[I] > 0;
      if AResult.Groups[I].Matched then
        AResult.Groups[I].Value := Matcher.Match[I]
      else
        AResult.Groups[I].Value := '';
    end;
    AResult.NamedGroups := NamedGroups;
  finally
    Matcher.Free;
  end;
end;

end.
