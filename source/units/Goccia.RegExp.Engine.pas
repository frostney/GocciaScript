unit Goccia.RegExp.Engine;

{$I Goccia.inc}

interface

uses
  Goccia.RegExp.&Program,
  Goccia.RegExp.VM;

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

  { Repeated RegExpBuiltinExec matching of one compiled RegExp against one
    subject, for the global loops of replace, match, split and matchAll: the
    subject is decoded once and the matcher's buffers are reused. Exec has
    the same result as ExecuteCompiledRegExp. }
  TGocciaRegExpScanner = class
  private
    FProgram: TRegExpProgram;
    FInput: string;
    FInputLength: Integer;
    FIsUnicode: Boolean;
    FHasIndices: Boolean;
    FEmptyPattern: Boolean;
    FMatcher: TRegExpMatcher;
    FMatchIndex: Integer;
    FMatchEnd: Integer;
  public
    constructor Create(const AProgram: TRegExpProgram;
      const APattern, AFlags, AInput: string);
    destructor Destroy; override;
    function Exec(const AStartIndex: Integer;
      const ARequireStart: Boolean): Boolean;
    { The capture group AGroup (0 is the whole match) of the last successful
      Exec, as code unit offsets; False when the group did not participate. }
    function TryGetGroup(const AGroup: Integer; out AStart,
      AEnd: Integer): Boolean;
    procedure GetMatchResult(out AResult: TGocciaRegExpMatchResult);
    property CaptureCount: Integer read FProgram.CaptureCount;
    function HasNamedGroups: Boolean;
    property IsUnicode: Boolean read FIsUnicode;
    property InputLength: Integer read FInputLength;
    property MatchIndex: Integer read FMatchIndex;
    property MatchEnd: Integer read FMatchEnd;
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

  Goccia.RegExp.Compiler;

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
    if TryReadCodePointAtAllowSurrogates(APattern, I, CodePoint,
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

procedure InitRegExpMatchResult(const AFlags: string;
  out AResult: TGocciaRegExpMatchResult);
begin
  AResult.Found := False;
  AResult.MatchIndex := -1;
  AResult.MatchEnd := -1;
  AResult.NextIndex := -1;
  AResult.HasIndices := HasRegExpFlag(AFlags, 'd');
  SetLength(AResult.Groups, 0);
  SetLength(AResult.NamedGroups, 0);
end;

procedure SetEmptyPatternMatch(const AInput: string;
  const AStartIndex: Integer; const AIsUnicode: Boolean;
  var AResult: TGocciaRegExpMatchResult);
begin
  AResult.Found := True;
  AResult.MatchIndex := AStartIndex;
  AResult.MatchEnd := AStartIndex;
  AResult.NextIndex := AdvanceUTF16StringIndex(AInput, AStartIndex,
    AIsUnicode);
  SetLength(AResult.Groups, 1);
  AResult.Groups[0].Matched := True;
  AResult.Groups[0].StartIndex := AStartIndex;
  AResult.Groups[0].EndIndex := AStartIndex;
  AResult.Groups[0].Value := '';
end;

// Fills AResult from a successful VM match. ASlots holds a start and an end
// code unit offset per group, -1 for a group that did not participate.
procedure SetSlotsMatch(const AProgram: TRegExpProgram;
  const AInput: string; const AInputLength: Integer;
  const ASlots: array of Integer; const AIsUnicode: Boolean;
  var AResult: TGocciaRegExpMatchResult);
var
  GroupCount, I, SlotStart, SlotEnd: Integer;
begin
  AResult.Found := True;
  AResult.MatchIndex := ASlots[0];
  AResult.MatchEnd := ASlots[1];
  AResult.NextIndex := AResult.MatchEnd;
  if AResult.MatchEnd = AResult.MatchIndex then
    AResult.NextIndex := AdvanceUTF16StringIndex(AInput, AResult.NextIndex,
      AIsUnicode);
  GroupCount := AProgram.CaptureCount + 1;
  SetLength(AResult.Groups, GroupCount);
  for I := 0 to GroupCount - 1 do
  begin
    SlotStart := -1;
    SlotEnd := -1;
    if I * 2 + 1 < Length(ASlots) then
    begin
      SlotStart := ASlots[I * 2];
      SlotEnd := ASlots[I * 2 + 1];
    end;
    if (SlotStart >= 0) and (SlotEnd >= SlotStart) and
       (SlotEnd <= AInputLength) then
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

function ExecuteCompiledRegExp(const AProgram: TRegExpProgram;
  const APattern, AFlags, AInput: string; const AStartIndex: Integer;
  const ARequireStart: Boolean;
  out AResult: TGocciaRegExpMatchResult): Boolean;
var
  VMResult: TRegExpVMResult;
  IsUnicode: Boolean;
  InputLength: Integer;
begin
  InitRegExpMatchResult(AFlags, AResult);
  ValidateRegExpFlags(AFlags);
  IsUnicode := HasRegExpFlag(AFlags, 'u') or HasRegExpFlag(AFlags, 'v');
  InputLength := RegExpInputCodeUnitLength(AInput);
  if AStartIndex > InputLength then
    Exit(False);
  if APattern = EMPTY_REGEX then
  begin
    SetEmptyPatternMatch(AInput, AStartIndex, IsUnicode, AResult);
    Exit(True);
  end;
  Result := ExecuteRegExpVM(AProgram, AInput, AStartIndex, ARequireStart, VMResult);
  if not Result then
    Exit(False);
  AResult.Found := True;
  if Length(VMResult.CaptureSlots) < 2 then
    Exit(False);
  SetSlotsMatch(AProgram, AInput, InputLength, VMResult.CaptureSlots,
    IsUnicode, AResult);
end;

{ TGocciaRegExpScanner }

constructor TGocciaRegExpScanner.Create(const AProgram: TRegExpProgram;
  const APattern, AFlags, AInput: string);
begin
  inherited Create;
  ValidateRegExpFlags(AFlags);
  FProgram := AProgram;
  FInput := AInput;
  // The decoded subject has one unit per UTF-16 code unit; the matcher
  // decodes it only when a match attempt needs it.
  FInputLength := Length(AInput);
  FIsUnicode := HasRegExpFlag(AFlags, 'u') or HasRegExpFlag(AFlags, 'v');
  FHasIndices := HasRegExpFlag(AFlags, 'd');
  FEmptyPattern := APattern = EMPTY_REGEX;
  if not FEmptyPattern then
    FMatcher := TRegExpMatcher.Create(AProgram, AInput);
  FMatchIndex := -1;
  FMatchEnd := -1;
end;

destructor TGocciaRegExpScanner.Destroy;
begin
  FMatcher.Free;
  inherited;
end;

function TGocciaRegExpScanner.Exec(const AStartIndex: Integer;
  const ARequireStart: Boolean): Boolean;
begin
  FMatchIndex := -1;
  FMatchEnd := -1;
  if AStartIndex > FInputLength then
    Exit(False);
  if FEmptyPattern then
  begin
    FMatchIndex := AStartIndex;
    FMatchEnd := AStartIndex;
    Exit(True);
  end;
  if not FMatcher.Exec(AStartIndex, ARequireStart) then
    Exit(False);
  FMatchIndex := FMatcher.Slot(0);
  FMatchEnd := FMatcher.Slot(1);
  Result := True;
end;

function TGocciaRegExpScanner.HasNamedGroups: Boolean;
begin
  Result := Length(FProgram.NamedGroups) > 0;
end;

function TGocciaRegExpScanner.TryGetGroup(const AGroup: Integer;
  out AStart, AEnd: Integer): Boolean;
begin
  if AGroup = 0 then
  begin
    AStart := FMatchIndex;
    AEnd := FMatchEnd;
  end
  else if FEmptyPattern or (AGroup * 2 + 1 >= FMatcher.SlotCount) then
  begin
    AStart := -1;
    AEnd := -1;
  end
  else
  begin
    AStart := FMatcher.Slot(AGroup * 2);
    AEnd := FMatcher.Slot(AGroup * 2 + 1);
  end;
  Result := (AStart >= 0) and (AEnd >= AStart) and (AEnd <= FInputLength);
end;

procedure TGocciaRegExpScanner.GetMatchResult(
  out AResult: TGocciaRegExpMatchResult);
begin
  AResult.Found := False;
  AResult.MatchIndex := -1;
  AResult.MatchEnd := -1;
  AResult.NextIndex := -1;
  AResult.HasIndices := FHasIndices;
  SetLength(AResult.NamedGroups, 0);
  if FEmptyPattern then
  begin
    SetEmptyPatternMatch(FInput, FMatchIndex, FIsUnicode, AResult);
    Exit;
  end;
  SetSlotsMatch(FProgram, FInput, FInputLength, FMatcher.Slots, FIsUnicode,
    AResult);
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
