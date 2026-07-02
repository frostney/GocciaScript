unit Goccia.RegExp.Engine;

{$I Goccia.inc}

interface

uses
  Goccia.RegExp.&Program;

type
  TGocciaRegExpMatchGroup = record
    Matched: Boolean;
    StartIndex: Integer;
    EndIndex: Integer;
    Value: string;
  end;

  TGocciaRegExpMatchGroups = array of TGocciaRegExpMatchGroup;

  TGocciaRegExpMatchResult = record
    Found: Boolean;
    MatchIndex: Integer;
    MatchEnd: Integer;
    NextIndex: Integer;
    HasIndices: Boolean;
    Groups: TGocciaRegExpMatchGroups;
    NamedGroups: TGocciaRegExpNamedGroups;
  end;

function NormalizeRegExpSource(const APattern: string): string;
function EscapeRegExpPattern(const APattern: string): string;
function HasRegExpFlag(const AFlags: string; const AFlag: Char): Boolean;
procedure ValidateRegExpFlags(const AFlags: string);
procedure ValidateRegExpPattern(const APattern, AFlags: string);
function CompileRegExpProgram(const APattern, AFlags: string): TRegExpProgram;
function CanonicalizeRegExpFlags(const AFlags: string): string;
function RegExpToString(const APattern, AFlags: string): string;
function ExecuteCompiledRegExp(const AProgram: TRegExpProgram;
  const APattern, AFlags, AInput: string; const AStartIndex: Integer;
  const ARequireStart: Boolean; out AResult: TGocciaRegExpMatchResult): Boolean;
function ExecuteRegExp(const APattern, AFlags, AInput: string;
  const AStartIndex: Integer; const ARequireStart: Boolean;
  out AResult: TGocciaRegExpMatchResult): Boolean;

implementation

uses
  SysUtils,

  TextSemantics,

  Goccia.RegExp.Compiler,
  Goccia.RegExp.VM;

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

// ES2026 §22.2.6.13.1 EscapeRegExpPattern ( pattern, flags )
function EscapeRegExpPattern(const APattern: string): string;
var
  ByteLength: Integer;
  CodePoint: Cardinal;
  I: Integer;
begin
  Result := '';
  I := 1;
  while I <= Length(APattern) do
  begin
    if TryReadUTF8CodePointAllowSurrogates(APattern, I, CodePoint,
       ByteLength) then
    begin
      case CodePoint of
        Ord('/'):
          Result := Result + '\/';
        $000A:
          Result := Result + '\n';
        $000D:
          Result := Result + '\r';
        $2028:
          Result := Result + '\u2028';
        $2029:
          Result := Result + '\u2029';
      else
        Result := Result + Copy(APattern, I, ByteLength);
      end;
      Inc(I, ByteLength);
    end
    else
    begin
      if APattern[I] = '/' then
        Result := Result + '\/'
      else
        Result := Result + APattern[I];
      Inc(I);
    end;
  end;
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
  if HasRegExpFlag(AFlags, 'u') and HasRegExpFlag(AFlags, 'v') then
    raise EConvertError.Create('Invalid regular expression flags');
end;

procedure ValidateRegExpPattern(const APattern, AFlags: string);
begin
  CompileRegExpProgram(APattern, AFlags);
end;

function CompileRegExpProgram(const APattern, AFlags: string): TRegExpProgram;
var
  PatternToCompile: string;
begin
  ValidateRegExpFlags(AFlags);
  PatternToCompile := APattern;
  if PatternToCompile = EMPTY_REGEX then
    PatternToCompile := '';
  Result := CompileRegExp(PatternToCompile, AFlags);
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
  Result := '/' + EscapeRegExpPattern(APattern) + '/' +
    CanonicalizeRegExpFlags(AFlags);
end;

function ExecuteCompiledRegExp(const AProgram: TRegExpProgram;
  const APattern, AFlags, AInput: string; const AStartIndex: Integer;
  const ARequireStart: Boolean;
  out AResult: TGocciaRegExpMatchResult): Boolean;
var
  VMResult: TRegExpVMResult;
  IsUnicode: Boolean;
  I, GroupCount: Integer;
  SlotStart, SlotEnd: Integer;
begin
  AResult.Found := False;
  AResult.MatchIndex := -1;
  AResult.MatchEnd := -1;
  AResult.NextIndex := -1;
  AResult.HasIndices := HasRegExpFlag(AFlags, 'd');
  SetLength(AResult.Groups, 0);
  SetLength(AResult.NamedGroups, 0);
  ValidateRegExpFlags(AFlags);
  IsUnicode := HasRegExpFlag(AFlags, 'u') or HasRegExpFlag(AFlags, 'v');
  if AStartIndex > UTF16CodeUnitLength(AInput) then
    Exit(False);
  if APattern = EMPTY_REGEX then
  begin
    AResult.Found := True;
    AResult.MatchIndex := AStartIndex;
    AResult.MatchEnd := AStartIndex;
    AResult.NextIndex := AdvanceUTF16StringIndex(AInput, AStartIndex,
      IsUnicode);
    SetLength(AResult.Groups, 1);
    AResult.Groups[0].Matched := True;
    AResult.Groups[0].StartIndex := AStartIndex;
    AResult.Groups[0].EndIndex := AStartIndex;
    AResult.Groups[0].Value := '';
    Exit(True);
  end;
  Result := ExecuteRegExpVM(AProgram, AInput, AStartIndex, ARequireStart, VMResult);
  if not Result then
    Exit(False);
  AResult.Found := True;
  if Length(VMResult.CaptureSlots) < 2 then
    Exit(False);
  AResult.MatchIndex := VMResult.CaptureSlots[0];
  AResult.MatchEnd := VMResult.CaptureSlots[1];
  AResult.NextIndex := AResult.MatchEnd;
  if AResult.MatchEnd = AResult.MatchIndex then
    AResult.NextIndex := AdvanceUTF16StringIndex(AInput, AResult.NextIndex,
      IsUnicode);
  GroupCount := AProgram.CaptureCount + 1;
  SetLength(AResult.Groups, GroupCount);
  for I := 0 to GroupCount - 1 do
  begin
    SlotStart := -1;
    SlotEnd := -1;
    if I * 2 + 1 < Length(VMResult.CaptureSlots) then
    begin
      SlotStart := VMResult.CaptureSlots[I * 2];
      SlotEnd := VMResult.CaptureSlots[I * 2 + 1];
    end;
    if (SlotStart >= 0) and (SlotEnd >= SlotStart) and
       (SlotEnd <= UTF16CodeUnitLength(AInput)) then
    begin
      AResult.Groups[I].Matched := True;
      AResult.Groups[I].StartIndex := SlotStart;
      AResult.Groups[I].EndIndex := SlotEnd;
      AResult.Groups[I].Value := UTF16Substring(AInput, SlotStart,
        SlotEnd - SlotStart);
    end
    else
    begin
      AResult.Groups[I].Matched := False;
      AResult.Groups[I].StartIndex := -1;
      AResult.Groups[I].EndIndex := -1;
      AResult.Groups[I].Value := '';
    end;
  end;
  AResult.NamedGroups := AProgram.NamedGroups;
end;

function ExecuteRegExp(const APattern, AFlags, AInput: string;
  const AStartIndex: Integer; const ARequireStart: Boolean;
  out AResult: TGocciaRegExpMatchResult): Boolean;
var
  CompiledProgram: TRegExpProgram;
begin
  CompiledProgram := CompileRegExpProgram(APattern, AFlags);
  Result := ExecuteCompiledRegExp(CompiledProgram, APattern, AFlags, AInput,
    AStartIndex, ARequireStart, AResult);
end;

end.
