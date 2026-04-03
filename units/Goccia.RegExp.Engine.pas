unit Goccia.RegExp.Engine;

{$I Goccia.inc}

interface

type
  TGocciaRegExpMatchGroup = record
    Matched: Boolean;
    Value: string;
  end;

  TGocciaRegExpMatchGroups = array of TGocciaRegExpMatchGroup;

  TGocciaRegExpMatchResult = record
    Found: Boolean;
    MatchIndex: Integer;
    MatchEnd: Integer;
    NextIndex: Integer;
    Groups: TGocciaRegExpMatchGroups;
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

implementation

uses
  Math,
  StrUtils,
  SysUtils,

  RegExpr;

const
  EMPTY_REGEX = '(?:)';
  REGEXP_FLAG_ORDER = 'gimsuy';

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
    if not CharInSet(AFlags[I], ['g', 'i', 'm', 's', 'u', 'y']) then
      raise EConvertError.Create('Invalid regular expression flags');
    if Pos(AFlags[I], Seen) > 0 then
      raise EConvertError.Create('Invalid regular expression flags');
    Seen := Seen + AFlags[I];
  end;
end;

procedure ValidateRegExpPattern(const APattern, AFlags: string);
var
  Matcher: TRegExpr;
  NormalizedPattern: string;
begin
  ValidateRegExpFlags(AFlags);
  NormalizedPattern := NormalizeRegExpSource(APattern);
  if NormalizedPattern = EMPTY_REGEX then
    Exit;
  Matcher := TRegExpr.Create;
  try
    Matcher.Expression := GetExecutableRegExpPattern(NormalizedPattern);
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

function ExecuteRegExp(const APattern, AFlags, AInput: string;
  const AStartIndex: Integer; const ARequireStart: Boolean;
  out AResult: TGocciaRegExpMatchResult): Boolean;
var
  Matcher: TRegExpr;
  SearchInput: string;
  RelativeIndex: Integer;
  I: Integer;
begin
  AResult.Found := False;
  AResult.MatchIndex := -1;
  AResult.MatchEnd := -1;
  AResult.NextIndex := -1;
  SetLength(AResult.Groups, 0);

  ValidateRegExpFlags(AFlags);
  if AStartIndex > Length(AInput) then
    Exit(False);

  if APattern = EMPTY_REGEX then
  begin
    AResult.Found := True;
    AResult.MatchIndex := AStartIndex;
    AResult.MatchEnd := AStartIndex;
    AResult.NextIndex := AdvanceStringIndex(AInput, AStartIndex,
      HasRegExpFlag(AFlags, 'u'));
    SetLength(AResult.Groups, 1);
    AResult.Groups[0].Matched := True;
    AResult.Groups[0].Value := '';
    Exit(True);
  end;

  Matcher := TRegExpr.Create;
  try
    Matcher.Expression := GetExecutableRegExpPattern(APattern);
    Matcher.ModifierI := HasRegExpFlag(AFlags, 'i');
    Matcher.ModifierM := HasRegExpFlag(AFlags, 'm');
    Matcher.ModifierS := HasRegExpFlag(AFlags, 's');
    Matcher.Compile;

    SearchInput := Copy(AInput, AStartIndex + 1, MaxInt);
    Result := Matcher.Exec(SearchInput);
    if Result and ARequireStart and (Matcher.MatchPos[0] <> 1) then
      Result := False;
    if not Result then
      Exit(False);

    RelativeIndex := Matcher.MatchPos[0] - 1;
    AResult.Found := True;
    AResult.MatchIndex := AStartIndex + RelativeIndex;
    AResult.MatchEnd := AResult.MatchIndex + Matcher.MatchLen[0];
    AResult.NextIndex := AResult.MatchEnd;
    if Matcher.MatchLen[0] = 0 then
      AResult.NextIndex := AdvanceStringIndex(AInput, AResult.NextIndex,
        HasRegExpFlag(AFlags, 'u'));

    SetLength(AResult.Groups, Matcher.SubExprMatchCount + 1);
    for I := 0 to Matcher.SubExprMatchCount do
    begin
      AResult.Groups[I].Matched := Matcher.MatchPos[I] > 0;
      if AResult.Groups[I].Matched then
        AResult.Groups[I].Value := Matcher.Match[I]
      else
        AResult.Groups[I].Value := '';
    end;
  finally
    Matcher.Free;
  end;
end;

end.
