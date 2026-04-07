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

  RegExpr;

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

procedure ValidateRegExpPattern(const APattern, AFlags: string);
var
  Matcher: TRegExpr;
  NormalizedPattern: string;
  ConvertedPattern: string;
  DiscardedGroups: TGocciaRegExpNamedGroups;
begin
  ValidateRegExpFlags(AFlags);
  NormalizedPattern := NormalizeRegExpSource(APattern);
  if NormalizedPattern = EMPTY_REGEX then
    Exit;
  ConvertedPattern := PreprocessRegExpPattern(
    GetExecutableRegExpPattern(NormalizedPattern), DiscardedGroups);
  Matcher := TRegExpr.Create;
  try
    Matcher.Expression := ConvertedPattern;
    Matcher.ModifierI := HasRegExpFlag(AFlags, 'i');
    Matcher.ModifierM := HasRegExpFlag(AFlags, 'm');
    Matcher.ModifierS := HasRegExpFlag(AFlags, 's');
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

function PreprocessRegExpPattern(const APattern: string;
  out ANamedGroups: TGocciaRegExpNamedGroups): string;
var
  I, PatternLength: Integer;
  GroupIndex: Integer;
  InCharClass: Boolean;
  GroupName: string;
  CloseAngle: Integer;
  BackrefIndex: Integer;
begin
  SetLength(ANamedGroups, 0);
  PatternLength := Length(APattern);
  if PatternLength = 0 then
  begin
    Result := '';
    Exit;
  end;
  Result := '';
  I := 1;
  GroupIndex := 0;
  InCharClass := False;
  while I <= PatternLength do
  begin
    if APattern[I] = '\' then
    begin
      if I + 1 <= PatternLength then
      begin
        // \k<name> backreference: convert to \N numeric backreference
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
            BackrefIndex := FindNamedGroupIndex(ANamedGroups, GroupName);
            if BackrefIndex > 0 then
              Result := Result + '\' + IntToStr(BackrefIndex)
            else
              Result := Result + '\1';
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
    if APattern[I] = '(' then
    begin
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
            Inc(GroupIndex);
            GroupName := Copy(APattern, I + 3, CloseAngle - I - 3);
            SetLength(ANamedGroups, Length(ANamedGroups) + 1);
            ANamedGroups[High(ANamedGroups)].Name := GroupName;
            ANamedGroups[High(ANamedGroups)].Index := GroupIndex;
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
      Inc(GroupIndex);
      Result := Result + APattern[I];
      Inc(I);
      Continue;
    end;
    Result := Result + APattern[I];
    Inc(I);
  end;
end;

function ExecuteRegExp(const APattern, AFlags, AInput: string;
  const AStartIndex: Integer; const ARequireStart: Boolean;
  out AResult: TGocciaRegExpMatchResult): Boolean;
var
  Matcher: TRegExpr;
  I: Integer;
  ConvertedPattern: string;
  NamedGroups: TGocciaRegExpNamedGroups;
begin
  AResult.Found := False;
  AResult.MatchIndex := -1;
  AResult.MatchEnd := -1;
  AResult.NextIndex := -1;
  SetLength(AResult.Groups, 0);
  SetLength(AResult.NamedGroups, 0);
  ValidateRegExpFlags(AFlags);
  if AStartIndex > Length(AInput) then
    Exit(False);
  if APattern = EMPTY_REGEX then
  begin
    AResult.Found := True;
    AResult.MatchIndex := AStartIndex;
    AResult.MatchEnd := AStartIndex;
    AResult.NextIndex := AdvanceStringIndex(AInput, AStartIndex,
      HasRegExpFlag(AFlags, 'u') or HasRegExpFlag(AFlags, 'v'));
    SetLength(AResult.Groups, 1);
    AResult.Groups[0].Matched := True;
    AResult.Groups[0].Value := '';
    Exit(True);
  end;
  ConvertedPattern := PreprocessRegExpPattern(
    GetExecutableRegExpPattern(APattern), NamedGroups);
  Matcher := TRegExpr.Create;
  try
    Matcher.Expression := ConvertedPattern;
    Matcher.ModifierI := HasRegExpFlag(AFlags, 'i');
    Matcher.ModifierM := HasRegExpFlag(AFlags, 'm');
    Matcher.ModifierS := HasRegExpFlag(AFlags, 's');
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
        HasRegExpFlag(AFlags, 'u') or HasRegExpFlag(AFlags, 'v'));
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
