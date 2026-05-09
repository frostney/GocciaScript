unit Goccia.RegExp.Compiler;

{$I Goccia.inc}

interface

uses
  Goccia.RegExp.Engine;

type
  TRegExpOpCode = (
    RX_CHAR          = 0,
    RX_CHAR_CLASS    = 1,
    RX_CHAR_CLASS_NEG = 2,
    RX_ANY           = 3,
    RX_SPLIT         = 4,
    RX_SPLIT_LAZY    = 5,
    RX_JUMP          = 6,
    RX_SAVE          = 7,
    RX_BACKREF       = 8,
    RX_ASSERT_START  = 9,
    RX_ASSERT_END    = 10,
    RX_ASSERT_WORD   = 11,
    RX_LOOKAHEAD     = 12,
    RX_LOOKBEHIND    = 13,
    RX_MATCH         = 14,
    RX_FAIL          = 15
  );

  TRegExpCharRange = record
    Lo: Cardinal;
    Hi: Cardinal;
  end;

  TRegExpCharClass = record
    Ranges: array of TRegExpCharRange;
  end;

  TRegExpProgram = record
    Code: array of UInt32;
    CharClasses: array of TRegExpCharClass;
    CaptureCount: Integer;
    NamedGroups: TGocciaRegExpNamedGroups;
  end;

const
  BACKREF_STRICT_FLAG = $800000;
  BACKREF_ICASE_FLAG = $400000;
  BACKREF_INDEX_MASK = $3FFFFF;
  LOOK_NEGATED_FLAG = $800000;
  LOOK_TARGET_MASK = $7FFFFF;

function CompileRegExp(const APattern, AFlags: string): TRegExpProgram;
procedure ValidateRegExpPatternNew(const APattern, AFlags: string);

implementation

uses
  Math,
  SysUtils,

  TextSemantics;

type
  TModifierState = record
    IgnoreCase: Boolean;
    Multiline: Boolean;
    DotAll: Boolean;
  end;

  TRegExpCompiler = class
  private
    FPattern: string;
    FPos: Integer;
    FCode: array of UInt32;
    FCodeLen: Integer;
    FCharClasses: array of TRegExpCharClass;
    FCaptureCount: Integer;
    FNamedGroups: TGocciaRegExpNamedGroups;
    FAltStack: array of Integer;
    FAltStackDepth: Integer;
    FModifier: TModifierState;
    FUnicode: Boolean;
    function Peek: Char;
    function PeekAt(AOffset: Integer): Char;
    function AtEnd: Boolean;
    function Advance: Char;
    function Match(C: Char): Boolean;
    procedure Emit(AInstr: UInt32);
    function EmitHole: Integer;
    procedure PatchHole(AIndex: Integer; ATarget: Integer);
    function CurrentPC: Integer;
    function EncodeOp(AOp: TRegExpOpCode): UInt32;
    function EncodeOpBx(AOp: TRegExpOpCode; ABx: Integer): UInt32;
    function AddCharClass(const ARanges: array of TRegExpCharRange): Integer;
    procedure CompilePattern;
    procedure CompileDisjunction;
    procedure CompileAlternative;
    procedure CompileTerm;
    procedure CompileAtom;
    procedure CompileQuantifier(AAtomStart: Integer);
    procedure CompileCharacterClass;
    procedure CompileEscape(var ARanges: array of TRegExpCharRange; var ARangeCount: Integer);
    procedure CompileEscapeAtom;
    procedure CompileGroup;
    procedure CompileModifierGroup;
    function ParseGroupName: string;
    function ParseUnicodeEscape: Cardinal;
    function ParseHexEscape(ADigits: Integer): Cardinal;
    function ParseDecimalEscape: Integer;
    procedure EmitCharMatch(ACodePoint: Cardinal);
    procedure EmitCharClassRanges(const ARanges: array of TRegExpCharRange;
      ARangeCount: Integer; ANegated: Boolean);
    procedure AddBuiltinCharClass(AEscapeChar: Char; var ARanges: array of TRegExpCharRange; var ARangeCount: Integer);
    procedure AddRange(var ARanges: array of TRegExpCharRange; var ARangeCount: Integer; ALo, AHi: Cardinal);
    procedure EmitUnicodePropertyClass(const APropertyName: string; ANegated: Boolean);
    procedure GetUnicodePropertyRanges(const APropertyName: string; var ARanges: array of TRegExpCharRange; var ARangeCount: Integer);
    function ReadCodePoint: Cardinal;
    procedure EnsureCodeCapacity(ANeeded: Integer);
    procedure EmitBody(const ABody: array of UInt32; ALen: Integer);
    procedure EmitBodyAt(const ABody: array of UInt32; ALen: Integer;
      AOrigStart: Integer);
    procedure ValidateNamedGroups;
    procedure PreScanNamedGroups;
    procedure InsertSplitAt(APos: Integer);
    procedure EmitDuplicateNamedBackref(const AName: string;
      AICaseFlag: Integer);
  public
    constructor Create(const APattern, AFlags: string);
    function Compile: TRegExpProgram;
  end;

const
  MAX_CHAR_RANGES = 512;

constructor TRegExpCompiler.Create(const APattern, AFlags: string);
begin
  inherited Create;
  FPattern := APattern;
  FPos := 1;
  FCodeLen := 0;
  SetLength(FCode, 256);
  SetLength(FCharClasses, 0);
  FCaptureCount := 0;
  SetLength(FNamedGroups, 0);
  SetLength(FAltStack, 64);
  FAltStackDepth := 0;
  FAltStack[0] := 0;
  FModifier.IgnoreCase := HasRegExpFlag(AFlags, 'i');
  FModifier.Multiline := HasRegExpFlag(AFlags, 'm');
  FModifier.DotAll := HasRegExpFlag(AFlags, 's');
  FUnicode := HasRegExpFlag(AFlags, 'u') or HasRegExpFlag(AFlags, 'v');
end;

function TRegExpCompiler.Peek: Char;
begin
  if FPos <= Length(FPattern) then
    Result := FPattern[FPos]
  else
    Result := #0;
end;

function TRegExpCompiler.PeekAt(AOffset: Integer): Char;
var
  Idx: Integer;
begin
  Idx := FPos + AOffset;
  if (Idx >= 1) and (Idx <= Length(FPattern)) then
    Result := FPattern[Idx]
  else
    Result := #0;
end;

function TRegExpCompiler.AtEnd: Boolean;
begin
  Result := FPos > Length(FPattern);
end;

function TRegExpCompiler.Advance: Char;
begin
  Result := Peek;
  Inc(FPos);
end;

function TRegExpCompiler.Match(C: Char): Boolean;
begin
  if Peek = C then
  begin
    Inc(FPos);
    Result := True;
  end
  else
    Result := False;
end;

procedure TRegExpCompiler.Emit(AInstr: UInt32);
begin
  if FCodeLen >= Length(FCode) then
    SetLength(FCode, FCodeLen * 2 + 16);
  FCode[FCodeLen] := AInstr;
  Inc(FCodeLen);
end;

function TRegExpCompiler.EmitHole: Integer;
begin
  Result := FCodeLen;
  Emit(0);
end;

procedure TRegExpCompiler.PatchHole(AIndex: Integer; ATarget: Integer);
var
  Op: TRegExpOpCode;
begin
  Op := TRegExpOpCode(FCode[AIndex] and $FF);
  FCode[AIndex] := UInt32(Ord(Op)) or (UInt32(ATarget) shl 8);
end;

function TRegExpCompiler.CurrentPC: Integer;
begin
  Result := FCodeLen;
end;

function TRegExpCompiler.EncodeOp(AOp: TRegExpOpCode): UInt32;
begin
  Result := UInt32(Ord(AOp));
end;

function TRegExpCompiler.EncodeOpBx(AOp: TRegExpOpCode; ABx: Integer): UInt32;
begin
  Result := UInt32(Ord(AOp)) or (UInt32(ABx) shl 8);
end;

function TRegExpCompiler.AddCharClass(
  const ARanges: array of TRegExpCharRange): Integer;
var
  I: Integer;
begin
  Result := Length(FCharClasses);
  SetLength(FCharClasses, Result + 1);
  SetLength(FCharClasses[Result].Ranges, Length(ARanges));
  for I := 0 to High(ARanges) do
    FCharClasses[Result].Ranges[I] := ARanges[I];
end;


procedure TRegExpCompiler.EmitCharMatch(ACodePoint: Cardinal);
var
  Ranges: array[0..1] of TRegExpCharRange;
  ClassIdx: Integer;
  Lower, Upper: Cardinal;
begin
  if FModifier.IgnoreCase then
  begin
    if (ACodePoint >= Ord('A')) and (ACodePoint <= Ord('Z')) then
    begin
      Lower := ACodePoint + 32;
      Ranges[0].Lo := ACodePoint;
      Ranges[0].Hi := ACodePoint;
      Ranges[1].Lo := Lower;
      Ranges[1].Hi := Lower;
      ClassIdx := AddCharClass(Ranges);
      Emit(EncodeOpBx(RX_CHAR_CLASS, ClassIdx));
      Exit;
    end;
    if (ACodePoint >= Ord('a')) and (ACodePoint <= Ord('z')) then
    begin
      Upper := ACodePoint - 32;
      Ranges[0].Lo := Upper;
      Ranges[0].Hi := Upper;
      Ranges[1].Lo := ACodePoint;
      Ranges[1].Hi := ACodePoint;
      ClassIdx := AddCharClass(Ranges);
      Emit(EncodeOpBx(RX_CHAR_CLASS, ClassIdx));
      Exit;
    end;
    if FUnicode and (ACodePoint = $212A) then
    begin
      Ranges[0].Lo := Ord('K');
      Ranges[0].Hi := Ord('K');
      Ranges[1].Lo := Ord('k');
      Ranges[1].Hi := Ord('k');
      ClassIdx := AddCharClass(Ranges);
      Emit(EncodeOpBx(RX_CHAR_CLASS, ClassIdx));
      Exit;
    end;
  end;
  Emit(EncodeOpBx(RX_CHAR, Integer(ACodePoint)));
end;

procedure TRegExpCompiler.AddRange(var ARanges: array of TRegExpCharRange;
  var ARangeCount: Integer; ALo, AHi: Cardinal);
begin
  if ARangeCount >= Length(ARanges) then
    Exit;
  ARanges[ARangeCount].Lo := ALo;
  ARanges[ARangeCount].Hi := AHi;
  Inc(ARangeCount);
end;

procedure TRegExpCompiler.AddBuiltinCharClass(AEscapeChar: Char;
  var ARanges: array of TRegExpCharRange; var ARangeCount: Integer);
begin
  case AEscapeChar of
    'd':
      AddRange(ARanges, ARangeCount, Ord('0'), Ord('9'));
    'D':
      begin
        AddRange(ARanges, ARangeCount, 0, Ord('0') - 1);
        AddRange(ARanges, ARangeCount, Ord('9') + 1, $10FFFF);
      end;
    'w':
      begin
        AddRange(ARanges, ARangeCount, Ord('0'), Ord('9'));
        AddRange(ARanges, ARangeCount, Ord('A'), Ord('Z'));
        AddRange(ARanges, ARangeCount, Ord('_'), Ord('_'));
        AddRange(ARanges, ARangeCount, Ord('a'), Ord('z'));
      end;
    'W':
      begin
        AddRange(ARanges, ARangeCount, 0, Ord('0') - 1);
        AddRange(ARanges, ARangeCount, Ord('9') + 1, Ord('A') - 1);
        AddRange(ARanges, ARangeCount, Ord('Z') + 1, Ord('_') - 1);
        AddRange(ARanges, ARangeCount, Ord('_') + 1, Ord('a') - 1);
        AddRange(ARanges, ARangeCount, Ord('z') + 1, $10FFFF);
      end;
    's':
      begin
        AddRange(ARanges, ARangeCount, $09, $0D);
        AddRange(ARanges, ARangeCount, $20, $20);
        AddRange(ARanges, ARangeCount, $A0, $A0);
        AddRange(ARanges, ARangeCount, $1680, $1680);
        AddRange(ARanges, ARangeCount, $2000, $200A);
        AddRange(ARanges, ARangeCount, $2028, $2029);
        AddRange(ARanges, ARangeCount, $202F, $202F);
        AddRange(ARanges, ARangeCount, $205F, $205F);
        AddRange(ARanges, ARangeCount, $3000, $3000);
        AddRange(ARanges, ARangeCount, $FEFF, $FEFF);
      end;
    'S':
      begin
        AddRange(ARanges, ARangeCount, 0, $08);
        AddRange(ARanges, ARangeCount, $0E, $1F);
        AddRange(ARanges, ARangeCount, $21, $9F);
        AddRange(ARanges, ARangeCount, $A1, $167F);
        AddRange(ARanges, ARangeCount, $1681, $1FFF);
        AddRange(ARanges, ARangeCount, $200B, $2027);
        AddRange(ARanges, ARangeCount, $202A, $202E);
        AddRange(ARanges, ARangeCount, $2030, $205E);
        AddRange(ARanges, ARangeCount, $2060, $2FFF);
        AddRange(ARanges, ARangeCount, $3001, $FEFE);
        AddRange(ARanges, ARangeCount, $FF00, $10FFFF);
      end;
  end;
end;

procedure TRegExpCompiler.GetUnicodePropertyRanges(const APropertyName: string;
  var ARanges: array of TRegExpCharRange; var ARangeCount: Integer);
begin
  if (APropertyName = 'L') or (APropertyName = 'Letter') then
  begin
    AddRange(ARanges, ARangeCount, $41, $5A);
    AddRange(ARanges, ARangeCount, $61, $7A);
    AddRange(ARanges, ARangeCount, $C0, $D6);
    AddRange(ARanges, ARangeCount, $D8, $F6);
    AddRange(ARanges, ARangeCount, $F8, $2FF);
    AddRange(ARanges, ARangeCount, $370, $37D);
    AddRange(ARanges, ARangeCount, $37F, $1FFF);
    AddRange(ARanges, ARangeCount, $200C, $200D);
    AddRange(ARanges, ARangeCount, $2070, $218F);
    AddRange(ARanges, ARangeCount, $2C00, $2FEF);
    AddRange(ARanges, ARangeCount, $3001, $D7FF);
    AddRange(ARanges, ARangeCount, $F900, $FDCF);
    AddRange(ARanges, ARangeCount, $FDF0, $FFFD);
    AddRange(ARanges, ARangeCount, $10000, $EFFFF);
  end
  else if (APropertyName = 'Lu') or (APropertyName = 'Uppercase_Letter') then
  begin
    AddRange(ARanges, ARangeCount, $41, $5A);
    AddRange(ARanges, ARangeCount, $C0, $D6);
    AddRange(ARanges, ARangeCount, $D8, $DE);
  end
  else if (APropertyName = 'Ll') or (APropertyName = 'Lowercase_Letter') then
  begin
    AddRange(ARanges, ARangeCount, $61, $7A);
    AddRange(ARanges, ARangeCount, $DF, $F6);
    AddRange(ARanges, ARangeCount, $F8, $FF);
  end
  else if (APropertyName = 'N') or (APropertyName = 'Number') then
    AddRange(ARanges, ARangeCount, $30, $39)
  else if (APropertyName = 'Nd') or (APropertyName = 'Decimal_Number') then
    AddRange(ARanges, ARangeCount, $30, $39)
  else if (APropertyName = 'P') or (APropertyName = 'Punctuation') then
  begin
    AddRange(ARanges, ARangeCount, $21, $23);
    AddRange(ARanges, ARangeCount, $25, $2A);
    AddRange(ARanges, ARangeCount, $2C, $2F);
    AddRange(ARanges, ARangeCount, $3A, $3B);
    AddRange(ARanges, ARangeCount, $3F, $40);
    AddRange(ARanges, ARangeCount, $5B, $5D);
    AddRange(ARanges, ARangeCount, $5F, $5F);
    AddRange(ARanges, ARangeCount, $7B, $7B);
    AddRange(ARanges, ARangeCount, $7D, $7D);
  end
  else if (APropertyName = 'S') or (APropertyName = 'Symbol') then
  begin
    AddRange(ARanges, ARangeCount, $24, $24);
    AddRange(ARanges, ARangeCount, $2B, $2B);
    AddRange(ARanges, ARangeCount, $3C, $3E);
    AddRange(ARanges, ARangeCount, $5E, $5E);
    AddRange(ARanges, ARangeCount, $60, $60);
    AddRange(ARanges, ARangeCount, $7C, $7C);
    AddRange(ARanges, ARangeCount, $7E, $7E);
  end
  else if (APropertyName = 'Z') or (APropertyName = 'Separator') then
  begin
    AddRange(ARanges, ARangeCount, $20, $20);
    AddRange(ARanges, ARangeCount, $A0, $A0);
    AddRange(ARanges, ARangeCount, $1680, $1680);
    AddRange(ARanges, ARangeCount, $2000, $200A);
    AddRange(ARanges, ARangeCount, $2028, $2029);
    AddRange(ARanges, ARangeCount, $202F, $202F);
    AddRange(ARanges, ARangeCount, $205F, $205F);
    AddRange(ARanges, ARangeCount, $3000, $3000);
  end
  else if (APropertyName = 'Cc') or (APropertyName = 'Control') then
  begin
    AddRange(ARanges, ARangeCount, $00, $1F);
    AddRange(ARanges, ARangeCount, $7F, $9F);
  end
  else if APropertyName = 'ASCII' then
    AddRange(ARanges, ARangeCount, $00, $7F)
  else if APropertyName = 'ASCII_Hex_Digit' then
  begin
    AddRange(ARanges, ARangeCount, $30, $39);
    AddRange(ARanges, ARangeCount, $41, $46);
    AddRange(ARanges, ARangeCount, $61, $66);
  end
  else if APropertyName = 'White_Space' then
  begin
    AddRange(ARanges, ARangeCount, $09, $0D);
    AddRange(ARanges, ARangeCount, $20, $20);
    AddRange(ARanges, ARangeCount, $85, $85);
    AddRange(ARanges, ARangeCount, $A0, $A0);
    AddRange(ARanges, ARangeCount, $1680, $1680);
    AddRange(ARanges, ARangeCount, $2000, $200A);
    AddRange(ARanges, ARangeCount, $2028, $2029);
    AddRange(ARanges, ARangeCount, $202F, $202F);
    AddRange(ARanges, ARangeCount, $205F, $205F);
    AddRange(ARanges, ARangeCount, $3000, $3000);
  end
  else
    raise EConvertError.Create('Invalid Unicode property name: ' + APropertyName);
end;

procedure TRegExpCompiler.EmitUnicodePropertyClass(const APropertyName: string;
  ANegated: Boolean);
var
  Ranges: array[0..MAX_CHAR_RANGES - 1] of TRegExpCharRange;
  RangeCount: Integer;
begin
  RangeCount := 0;
  GetUnicodePropertyRanges(APropertyName, Ranges, RangeCount);
  EmitCharClassRanges(Ranges, RangeCount, ANegated);
end;

procedure TRegExpCompiler.EmitCharClassRanges(
  const ARanges: array of TRegExpCharRange;
  ARangeCount: Integer; ANegated: Boolean);
var
  ClassIdx, I, OrigCount: Integer;
  Op: TRegExpOpCode;
  DynRanges: array of TRegExpCharRange;
begin
  SetLength(DynRanges, ARangeCount);
  for I := 0 to ARangeCount - 1 do
    DynRanges[I] := ARanges[I];
  if FModifier.IgnoreCase then
  begin
    OrigCount := Length(DynRanges);
    for I := 0 to OrigCount - 1 do
    begin
      if (DynRanges[I].Lo >= Ord('A')) and (DynRanges[I].Hi <= Ord('Z')) then
      begin
        SetLength(DynRanges, Length(DynRanges) + 1);
        DynRanges[High(DynRanges)].Lo := DynRanges[I].Lo + 32;
        DynRanges[High(DynRanges)].Hi := DynRanges[I].Hi + 32;
      end
      else if (DynRanges[I].Lo >= Ord('a')) and (DynRanges[I].Hi <= Ord('z')) then
      begin
        SetLength(DynRanges, Length(DynRanges) + 1);
        DynRanges[High(DynRanges)].Lo := DynRanges[I].Lo - 32;
        DynRanges[High(DynRanges)].Hi := DynRanges[I].Hi - 32;
      end;
    end;
  end;
  ClassIdx := AddCharClass(DynRanges);
  if ANegated then
    Op := RX_CHAR_CLASS_NEG
  else
    Op := RX_CHAR_CLASS;
  Emit(EncodeOpBx(Op, ClassIdx));
end;

function TRegExpCompiler.ReadCodePoint: Cardinal;
var
  ByteLen: Integer;
begin
  if FUnicode and (FPos <= Length(FPattern)) then
  begin
    if TryReadUTF8CodePoint(FPattern, FPos, Result, ByteLen) and (ByteLen > 1) then
    begin
      Inc(FPos, ByteLen);
      Exit;
    end;
  end;
  Result := Ord(Advance);
end;

function TRegExpCompiler.ParseGroupName: string;
var
  C: Char;
begin
  Result := '';
  while not AtEnd do
  begin
    C := Peek;
    if C = '>' then
    begin
      Inc(FPos);
      Exit;
    end;
    Result := Result + Advance;
  end;
  raise EConvertError.Create('Unterminated group name');
end;

function TRegExpCompiler.ParseHexEscape(ADigits: Integer): Cardinal;
var
  I: Integer;
  C: Char;
begin
  Result := 0;
  for I := 1 to ADigits do
  begin
    if AtEnd then
      raise EConvertError.Create('Invalid hex escape');
    C := Advance;
    case C of
      '0'..'9': Result := Result * 16 + Cardinal(Ord(C) - Ord('0'));
      'a'..'f': Result := Result * 16 + Cardinal(Ord(C) - Ord('a') + 10);
      'A'..'F': Result := Result * 16 + Cardinal(Ord(C) - Ord('A') + 10);
    else
      raise EConvertError.Create('Invalid hex escape');
    end;
  end;
end;

function TRegExpCompiler.ParseUnicodeEscape: Cardinal;
var
  HighSurrogate: Cardinal;
begin
  if Match('{') then
  begin
    Result := 0;
    while not AtEnd and (Peek <> '}') do
    begin
      case Peek of
        '0'..'9': Result := Result * 16 + Cardinal(Ord(Advance) - Ord('0'));
        'a'..'f': Result := Result * 16 + Cardinal(Ord(Advance) - Ord('a') + 10);
        'A'..'F': Result := Result * 16 + Cardinal(Ord(Advance) - Ord('A') + 10);
      else
        raise EConvertError.Create('Invalid Unicode escape');
      end;
    end;
    if not Match('}') then
      raise EConvertError.Create('Unterminated Unicode escape');
    if Result > $10FFFF then
      raise EConvertError.Create('Unicode escape out of range');
    Exit;
  end;
  Result := ParseHexEscape(4);
  if (Result >= $D800) and (Result <= $DBFF) then
  begin
    HighSurrogate := Result;
    if (Peek = '\') and (PeekAt(1) = 'u') then
    begin
      Inc(FPos, 2);
      Result := ParseHexEscape(4);
      if (Result >= $DC00) and (Result <= $DFFF) then
      begin
        Result := $10000 + ((HighSurrogate - $D800) shl 10) +
          (Result - $DC00);
        Exit;
      end;
      Dec(FPos, 6);
    end;
    Result := HighSurrogate;
  end;
end;

function TRegExpCompiler.ParseDecimalEscape: Integer;
const
  MAX_QUANTIFIER = 1000000;
var
  C: Char;
begin
  Result := 0;
  while not AtEnd do
  begin
    C := Peek;
    if (C < '0') or (C > '9') then
      Break;
    if Result <= MAX_QUANTIFIER then
      Result := Result * 10 + (Ord(Advance) - Ord('0'))
    else
      Advance;
  end;
  if Result > MAX_QUANTIFIER then
    Result := MAX_QUANTIFIER;
end;

procedure TRegExpCompiler.EmitDuplicateNamedBackref(const AName: string;
  AICaseFlag: Integer);
var
  Indices: array of Integer;
  Count, I: Integer;
  SplitHole: Integer;
  JumpHoles: array of Integer;
  JumpCount: Integer;
begin
  Count := 0;
  SetLength(Indices, Length(FNamedGroups));
  for I := 0 to High(FNamedGroups) do
    if FNamedGroups[I].Name = AName then
    begin
      Indices[Count] := FNamedGroups[I].Index;
      Inc(Count);
    end;
  SetLength(Indices, Count);
  if Count = 1 then
  begin
    Emit(EncodeOpBx(RX_BACKREF, Indices[0] or AICaseFlag));
    Exit;
  end;
  JumpCount := 0;
  SetLength(JumpHoles, Count + 1);
  for I := 0 to Count - 1 do
  begin
    SplitHole := CurrentPC;
    Emit(EncodeOpBx(RX_SPLIT, 0));
    Emit(EncodeOpBx(RX_BACKREF, Indices[I] or BACKREF_STRICT_FLAG or AICaseFlag));
    JumpHoles[JumpCount] := CurrentPC;
    Inc(JumpCount);
    Emit(0);
    PatchHole(SplitHole, CurrentPC);
  end;
  Emit(EncodeOp(RX_FAIL));
  for I := 0 to JumpCount - 1 do
    FCode[JumpHoles[I]] := EncodeOpBx(RX_JUMP, CurrentPC);
end;

procedure TRegExpCompiler.CompileEscapeAtom;
var
  C: Char;
  Ranges: array[0..MAX_CHAR_RANGES - 1] of TRegExpCharRange;
  RangeCount: Integer;
  PropertyName: string;
  Negated: Boolean;
  GroupName: string;
  BackrefIdx, I, GroupCount, BackrefICaseFlag: Integer;
  CodePoint: Cardinal;
begin
  if FModifier.IgnoreCase then
    BackrefICaseFlag := BACKREF_ICASE_FLAG
  else
    BackrefICaseFlag := 0;
  C := Advance;
  case C of
    'd', 'D', 'w', 'W', 's', 'S':
      begin
        RangeCount := 0;
        AddBuiltinCharClass(C, Ranges, RangeCount);
        EmitCharClassRanges(Ranges, RangeCount, False);
      end;
    'b':
      Emit(EncodeOpBx(RX_ASSERT_WORD, 0));
    'B':
      Emit(EncodeOpBx(RX_ASSERT_WORD, 1));
    'p', 'P':
      begin
        if FUnicode and Match('{') then
        begin
          Negated := C = 'P';
          PropertyName := '';
          while not AtEnd and (Peek <> '}') do
            PropertyName := PropertyName + Advance;
          if not Match('}') then
            raise EConvertError.Create('Unterminated Unicode property escape');
          EmitUnicodePropertyClass(PropertyName, Negated);
        end
        else
          EmitCharMatch(Ord(C));
      end;
    'k':
      begin
        if Match('<') then
        begin
          GroupName := ParseGroupName;
          BackrefIdx := -1;
          GroupCount := 0;
          for I := 0 to High(FNamedGroups) do
            if FNamedGroups[I].Name = GroupName then
            begin
              if BackrefIdx < 0 then
                BackrefIdx := FNamedGroups[I].Index;
              Inc(GroupCount);
            end;
          if BackrefIdx < 0 then
            raise EConvertError.Create(
              'Invalid named backreference: ' + GroupName);
          if GroupCount <= 1 then
            Emit(EncodeOpBx(RX_BACKREF, BackrefIdx or BackrefICaseFlag))
          else
            EmitDuplicateNamedBackref(GroupName, BackrefICaseFlag);
        end
        else
          EmitCharMatch(Ord('k'));
      end;
    '1'..'9':
      begin
        BackrefIdx := Ord(C) - Ord('0');
        while not AtEnd and (Peek >= '0') and (Peek <= '9') do
          BackrefIdx := BackrefIdx * 10 + (Ord(Advance) - Ord('0'));
        Emit(EncodeOpBx(RX_BACKREF, BackrefIdx or BackrefICaseFlag));
      end;
    'n': EmitCharMatch($0A);
    'r': EmitCharMatch($0D);
    't': EmitCharMatch($09);
    'v': EmitCharMatch($0B);
    'f': EmitCharMatch($0C);
    '0':
      begin
        if not AtEnd and (Peek >= '0') and (Peek <= '9') then
          EmitCharMatch(Ord(C))
        else
          EmitCharMatch(0);
      end;
    'x': EmitCharMatch(ParseHexEscape(2));
    'u': EmitCharMatch(ParseUnicodeEscape);
    'c':
      begin
        if not AtEnd and (((Peek >= 'a') and (Peek <= 'z')) or
           ((Peek >= 'A') and (Peek <= 'Z'))) then
          EmitCharMatch(Ord(Advance) mod 32)
        else if FUnicode then
          raise EConvertError.Create(
            'Invalid regular expression: invalid control escape in unicode mode')
        else
          EmitCharMatch(Ord('c'));
      end;
  else
    if FUnicode and not CharInSet(C, ['/', '^', '$', '\', '.', '*', '+',
       '?', '(', ')', '[', ']', '{', '}', '|']) then
      raise EConvertError.Create(
        'Invalid regular expression: invalid escape in unicode mode')
    else
      EmitCharMatch(Ord(C));
  end;
end;

procedure TRegExpCompiler.CompileEscape(
  var ARanges: array of TRegExpCharRange; var ARangeCount: Integer);
var
  C: Char;
  PropertyName: string;
  CodePoint: Cardinal;
begin
  C := Advance;
  case C of
    'd', 'D', 'w', 'W', 's', 'S':
      AddBuiltinCharClass(C, ARanges, ARangeCount);
    'n': AddRange(ARanges, ARangeCount, $0A, $0A);
    'r': AddRange(ARanges, ARangeCount, $0D, $0D);
    't': AddRange(ARanges, ARangeCount, $09, $09);
    'v': AddRange(ARanges, ARangeCount, $0B, $0B);
    'f': AddRange(ARanges, ARangeCount, $0C, $0C);
    '0':
      begin
        if not AtEnd and (Peek >= '0') and (Peek <= '9') then
          AddRange(ARanges, ARangeCount, Ord(C), Ord(C))
        else
          AddRange(ARanges, ARangeCount, 0, 0);
      end;
    'x':
      begin
        CodePoint := ParseHexEscape(2);
        AddRange(ARanges, ARangeCount, CodePoint, CodePoint);
      end;
    'u':
      begin
        CodePoint := ParseUnicodeEscape;
        AddRange(ARanges, ARangeCount, CodePoint, CodePoint);
      end;
    'p', 'P':
      begin
        if FUnicode and Match('{') then
        begin
          PropertyName := '';
          while not AtEnd and (Peek <> '}') do
            PropertyName := PropertyName + Advance;
          if not Match('}') then
            raise EConvertError.Create('Unterminated Unicode property escape');
          if C = 'P' then
            raise EConvertError.Create(
              'Negated Unicode property escape \\P{...} is not supported inside character classes')
          else
            GetUnicodePropertyRanges(PropertyName, ARanges, ARangeCount);
        end
        else
          AddRange(ARanges, ARangeCount, Ord(C), Ord(C));
      end;
    'b':
      AddRange(ARanges, ARangeCount, $08, $08);
    'c':
      begin
        if not AtEnd and (((Peek >= 'a') and (Peek <= 'z')) or
           ((Peek >= 'A') and (Peek <= 'Z'))) then
          AddRange(ARanges, ARangeCount, Ord(Advance) mod 32,
            Ord(FPattern[FPos - 1]) mod 32)
        else if FUnicode then
          raise EConvertError.Create(
            'Invalid regular expression: invalid control escape in unicode mode')
        else
          AddRange(ARanges, ARangeCount, Ord('c'), Ord('c'));
      end;
  else
    if FUnicode and not CharInSet(C, ['/', '^', '$', '\', '.', '*', '+',
       '?', '(', ')', '[', ']', '{', '}', '|', '-']) then
      raise EConvertError.Create(
        'Invalid regular expression: invalid escape in unicode mode')
    else
      AddRange(ARanges, ARangeCount, Ord(C), Ord(C));
  end;
end;

procedure TRegExpCompiler.CompileCharacterClass;
var
  Ranges: array[0..MAX_CHAR_RANGES - 1] of TRegExpCharRange;
  RangeCount: Integer;
  Negated: Boolean;
  C: Char;
  Lo, Hi: Cardinal;
  SavePos: Integer;
begin
  Negated := Match('^');
  RangeCount := 0;
  while not AtEnd and (Peek <> ']') do
  begin
    if Peek = '\' then
    begin
      Inc(FPos);
      CompileEscape(Ranges, RangeCount);
      Continue;
    end;
    Lo := ReadCodePoint;
    if (not AtEnd) and (Peek = '-') and (PeekAt(1) <> ']') then
    begin
      Inc(FPos);
      if Peek = '\' then
      begin
        SavePos := RangeCount;
        Inc(FPos);
        CompileEscape(Ranges, RangeCount);
        if RangeCount > SavePos then
        begin
          Hi := Ranges[RangeCount - 1].Lo;
          Dec(RangeCount);
          if Lo > Hi then
            raise EConvertError.Create(
              'Invalid regular expression: range out of order in character class');
          AddRange(Ranges, RangeCount, Lo, Hi);
        end;
      end
      else
      begin
        Hi := ReadCodePoint;
        if Lo > Hi then
          raise EConvertError.Create(
            'Invalid regular expression: range out of order in character class');
        AddRange(Ranges, RangeCount, Lo, Hi);
      end;
    end
    else
      AddRange(Ranges, RangeCount, Lo, Lo);
  end;
  if not Match(']') then
    raise EConvertError.Create('Unterminated character class');
  EmitCharClassRanges(Ranges, RangeCount, Negated);
end;

procedure TRegExpCompiler.CompileModifierGroup;
var
  C: Char;
  EnableFlags, DisableFlags: string;
  InDisable: Boolean;
  SavedModifier: TModifierState;
begin
  EnableFlags := '';
  DisableFlags := '';
  InDisable := False;
  while not AtEnd and (Peek <> ':') and (Peek <> ')') do
  begin
    C := Advance;
    if C = '-' then
    begin
      if InDisable then
        raise EConvertError.Create(
          'Invalid regular expression: unexpected - in modifier group');
      InDisable := True;
      Continue;
    end;
    if not CharInSet(C, ['i', 'm', 's']) then
      raise EConvertError.CreateFmt(
        'Invalid regular expression: ''%s'' is not a valid modifier flag', [C]);
    if InDisable then
    begin
      if Pos(C, DisableFlags) > 0 then
        raise EConvertError.CreateFmt(
          'Invalid regular expression: duplicate modifier flag ''%s''', [C]);
      if Pos(C, EnableFlags) > 0 then
        raise EConvertError.CreateFmt(
          'Invalid regular expression: ''%s'' in both enable and disable', [C]);
      DisableFlags := DisableFlags + C;
    end
    else
    begin
      if Pos(C, EnableFlags) > 0 then
        raise EConvertError.CreateFmt(
          'Invalid regular expression: duplicate modifier flag ''%s''', [C]);
      EnableFlags := EnableFlags + C;
    end;
  end;
  if (EnableFlags = '') and (DisableFlags = '') then
    raise EConvertError.Create(
      'Invalid regular expression: modifier group must enable or disable at least one flag');
  if not Match(':') then
    raise EConvertError.Create(
      'Invalid regular expression: modifier group must use (?flags:...) syntax');
  SavedModifier := FModifier;
  if Pos('i', EnableFlags) > 0 then FModifier.IgnoreCase := True;
  if Pos('m', EnableFlags) > 0 then FModifier.Multiline := True;
  if Pos('s', EnableFlags) > 0 then FModifier.DotAll := True;
  if Pos('i', DisableFlags) > 0 then FModifier.IgnoreCase := False;
  if Pos('m', DisableFlags) > 0 then FModifier.Multiline := False;
  if Pos('s', DisableFlags) > 0 then FModifier.DotAll := False;
  CompileDisjunction;
  if not Match(')') then
    raise EConvertError.Create('Unterminated modifier group');
  FModifier := SavedModifier;
end;

procedure TRegExpCompiler.CompileGroup;
var
  SaveAltDepth: Integer;
  GroupName: string;
  CaptureIdx, I: Integer;
  SplitHole, JumpHole: Integer;
  LookStart: Integer;
  IsNegative: Boolean;
begin
  Inc(FAltStackDepth);
  if FAltStackDepth >= Length(FAltStack) then
    SetLength(FAltStack, FAltStackDepth * 2 + 4);
  FAltStack[FAltStackDepth] := 0;
  if Match('?') then
  begin
    if Match(':') then
    begin
      CompileDisjunction;
      if not Match(')') then
        raise EConvertError.Create('Unterminated non-capturing group');
    end
    else if Match('=') then
    begin
      SplitHole := EmitHole;
      FCode[SplitHole] := EncodeOpBx(RX_LOOKAHEAD, 0);
      LookStart := CurrentPC;
      CompileDisjunction;
      if not Match(')') then
        raise EConvertError.Create('Unterminated lookahead');
      Emit(EncodeOp(RX_MATCH));
      PatchHole(SplitHole, CurrentPC);
      FCode[SplitHole] := EncodeOpBx(RX_LOOKAHEAD, CurrentPC);
    end
    else if Match('!') then
    begin
      SplitHole := EmitHole;
      FCode[SplitHole] := EncodeOpBx(RX_LOOKAHEAD, 0);
      CompileDisjunction;
      if not Match(')') then
        raise EConvertError.Create('Unterminated negative lookahead');
      Emit(EncodeOp(RX_MATCH));
      PatchHole(SplitHole, CurrentPC);
      FCode[SplitHole] := EncodeOpBx(RX_LOOKAHEAD, CurrentPC or LOOK_NEGATED_FLAG);
    end
    else if Match('<') then
    begin
      if Match('=') then
      begin
        SplitHole := EmitHole;
        FCode[SplitHole] := EncodeOpBx(RX_LOOKBEHIND, 0);
        CompileDisjunction;
        if not Match(')') then
          raise EConvertError.Create('Unterminated lookbehind');
        Emit(EncodeOp(RX_MATCH));
        PatchHole(SplitHole, CurrentPC);
        FCode[SplitHole] := EncodeOpBx(RX_LOOKBEHIND, CurrentPC);
      end
      else if Match('!') then
      begin
        SplitHole := EmitHole;
        FCode[SplitHole] := EncodeOpBx(RX_LOOKBEHIND, 0);
        CompileDisjunction;
        if not Match(')') then
          raise EConvertError.Create('Unterminated negative lookbehind');
        Emit(EncodeOp(RX_MATCH));
        PatchHole(SplitHole, CurrentPC);
        FCode[SplitHole] := EncodeOpBx(RX_LOOKBEHIND, CurrentPC or LOOK_NEGATED_FLAG);
      end
      else
      begin
        GroupName := ParseGroupName;
        Inc(FCaptureCount);
        CaptureIdx := FCaptureCount;
        Emit(EncodeOpBx(RX_SAVE, CaptureIdx * 2));
        CompileDisjunction;
        if not Match(')') then
          raise EConvertError.Create('Unterminated named capture group');
        Emit(EncodeOpBx(RX_SAVE, CaptureIdx * 2 + 1));
      end;
    end
    else if CharInSet(Peek, ['i', 'm', 's', '-']) then
    begin
      CompileModifierGroup;
    end
    else
      raise EConvertError.Create('Invalid group syntax');
  end
  else
  begin
    Inc(FCaptureCount);
    CaptureIdx := FCaptureCount;
    Emit(EncodeOpBx(RX_SAVE, CaptureIdx * 2));
    CompileDisjunction;
    if not Match(')') then
      raise EConvertError.Create('Unterminated capturing group');
    Emit(EncodeOpBx(RX_SAVE, CaptureIdx * 2 + 1));
  end;
  if FAltStackDepth > 0 then
    Dec(FAltStackDepth);
end;

procedure TRegExpCompiler.CompileAtom;
var
  C: Char;
  CodePoint: Cardinal;
begin
  C := Peek;
  case C of
    '(':
      begin
        Inc(FPos);
        CompileGroup;
      end;
    '[':
      begin
        Inc(FPos);
        CompileCharacterClass;
      end;
    '.':
      begin
        Inc(FPos);
        if FModifier.DotAll then
          Emit(EncodeOpBx(RX_ANY, 1))
        else
          Emit(EncodeOpBx(RX_ANY, 0));
      end;
    '^':
      begin
        Inc(FPos);
        if FModifier.Multiline then
          Emit(EncodeOpBx(RX_ASSERT_START, 1))
        else
          Emit(EncodeOpBx(RX_ASSERT_START, 0));
      end;
    '$':
      begin
        Inc(FPos);
        if FModifier.Multiline then
          Emit(EncodeOpBx(RX_ASSERT_END, 1))
        else
          Emit(EncodeOpBx(RX_ASSERT_END, 0));
      end;
    '\':
      begin
        Inc(FPos);
        if AtEnd then
          raise EConvertError.Create(
            'Invalid regular expression: \ at end of pattern');
        CompileEscapeAtom;
      end;
  else
    begin
      if FUnicode then
      begin
        CodePoint := ReadCodePoint;
        EmitCharMatch(CodePoint);
      end
      else
      begin
        Inc(FPos);
        EmitCharMatch(Ord(C));
      end;
    end;
  end;
end;

procedure TRegExpCompiler.EnsureCodeCapacity(ANeeded: Integer);
begin
  if FCodeLen + ANeeded >= Length(FCode) then
    SetLength(FCode, (FCodeLen + ANeeded) * 2 + 16);
end;

procedure TRegExpCompiler.EmitBody(const ABody: array of UInt32; ALen: Integer);
begin
  EmitBodyAt(ABody, ALen, 0);
end;

procedure TRegExpCompiler.EmitBodyAt(const ABody: array of UInt32;
  ALen: Integer; AOrigStart: Integer);
var
  DstStart, Delta, J: Integer;
  Op: TRegExpOpCode;
  Bx: Integer;
  NegFlag: Integer;
begin
  EnsureCodeCapacity(ALen);
  DstStart := FCodeLen;
  Move(ABody[0], FCode[DstStart], ALen * SizeOf(UInt32));
  Delta := DstStart - AOrigStart;
  if Delta <> 0 then
  begin
    for J := DstStart to DstStart + ALen - 1 do
    begin
      Op := TRegExpOpCode(FCode[J] and $FF);
      case Op of
        RX_SPLIT, RX_SPLIT_LAZY, RX_JUMP:
          begin
            Bx := Integer(FCode[J] shr 8);
            Inc(Bx, Delta);
            FCode[J] := EncodeOpBx(Op, Bx);
          end;
        RX_LOOKAHEAD, RX_LOOKBEHIND:
          begin
            Bx := Integer(FCode[J] shr 8);
            NegFlag := Bx and LOOK_NEGATED_FLAG;
            Bx := (Bx and LOOK_TARGET_MASK) + Delta;
            FCode[J] := EncodeOpBx(Op, Bx or NegFlag);
          end;
      end;
    end;
  end;
  Inc(FCodeLen, ALen);
end;

procedure TRegExpCompiler.CompileQuantifier(AAtomStart: Integer);
var
  SplitPC: Integer;
  MinCount, MaxCount, I: Integer;
  Lazy: Boolean;
  C: Char;
  BodyLen: Integer;
  BodyCode: array of UInt32;
  SavePos: Integer;
begin
  if AtEnd then
    Exit;
  C := Peek;
  MinCount := -1;
  MaxCount := -1;
  SavePos := FPos;
  case C of
    '*': begin MinCount := 0; MaxCount := -1; Inc(FPos); end;
    '+': begin MinCount := 1; MaxCount := -1; Inc(FPos); end;
    '?': begin MinCount := 0; MaxCount := 1; Inc(FPos); end;
    '{':
      begin
        Inc(FPos);
        if AtEnd or not CharInSet(Peek, ['0'..'9']) then
        begin
          Dec(FPos);
          Exit;
        end;
        MinCount := ParseDecimalEscape;
        if Match(',') then
        begin
          if Peek = '}' then
            MaxCount := -1
          else
            MaxCount := ParseDecimalEscape;
        end
        else
          MaxCount := MinCount;
        if not Match('}') then
        begin
          FPos := SavePos;
          Exit;
        end;
        if (MaxCount >= 0) and (MinCount > MaxCount) then
          raise EConvertError.Create(
            'Invalid regular expression: numbers out of order in quantifier');
      end;
  else
    Exit;
  end;
  Lazy := Match('?');
  BodyLen := CurrentPC - AAtomStart;
  if BodyLen = 0 then
    Exit;
  SetLength(BodyCode, BodyLen);
  Move(FCode[AAtomStart], BodyCode[0], BodyLen * SizeOf(UInt32));
  FCodeLen := AAtomStart;
  for I := 1 to MinCount do
    EmitBodyAt(BodyCode, BodyLen, AAtomStart);
  if MaxCount = -1 then
  begin
    SplitPC := CurrentPC;
    if Lazy then
      Emit(EncodeOpBx(RX_SPLIT_LAZY, 0))
    else
      Emit(EncodeOpBx(RX_SPLIT, 0));
    EmitBodyAt(BodyCode, BodyLen, AAtomStart);
    Emit(EncodeOpBx(RX_JUMP, SplitPC));
    PatchHole(SplitPC, CurrentPC);
  end
  else
  begin
    for I := MinCount + 1 to MaxCount do
    begin
      SplitPC := CurrentPC;
      if Lazy then
        Emit(EncodeOpBx(RX_SPLIT_LAZY, 0))
      else
        Emit(EncodeOpBx(RX_SPLIT, 0));
      EmitBodyAt(BodyCode, BodyLen, AAtomStart);
      PatchHole(SplitPC, CurrentPC);
    end;
  end;
end;

function IsQuantifierChar(C: Char): Boolean; inline;
begin
  Result := (C = '*') or (C = '+') or (C = '?') or (C = '{');
end;

procedure TRegExpCompiler.CompileTerm;
var
  AtomStart: Integer;
  C: Char;
  IsAssertion: Boolean;
begin
  C := Peek;
  if IsQuantifierChar(C) then
    raise EConvertError.Create('Invalid regular expression: nothing to repeat');
  IsAssertion := (C = '^') or (C = '$') or
    ((C = '\') and ((PeekAt(1) = 'b') or (PeekAt(1) = 'B')));
  if (C = '(') and (PeekAt(1) = '?') and
     ((PeekAt(2) = '=') or (PeekAt(2) = '!') or
      ((PeekAt(2) = '<') and ((PeekAt(3) = '=') or (PeekAt(3) = '!')))) then
    IsAssertion := True;
  AtomStart := CurrentPC;
  CompileAtom;
  if (not AtEnd) and IsQuantifierChar(Peek) and IsAssertion and FUnicode then
    raise EConvertError.Create(
      'Invalid regular expression: quantifier on assertion in unicode mode');
  CompileQuantifier(AtomStart);
end;

procedure TRegExpCompiler.CompileAlternative;
begin
  while not AtEnd and (Peek <> '|') and (Peek <> ')') do
    CompileTerm;
end;

procedure TRegExpCompiler.InsertSplitAt(APos: Integer);
var
  I: Integer;
  Op: TRegExpOpCode;
  Bx: Integer;
  Negated: Boolean;
begin
  EnsureCodeCapacity(1);
  Move(FCode[APos], FCode[APos + 1], (FCodeLen - APos) * SizeOf(UInt32));
  FCode[APos] := EncodeOpBx(RX_SPLIT, 0);
  Inc(FCodeLen);
  for I := APos + 1 to FCodeLen - 1 do
  begin
    Op := TRegExpOpCode(FCode[I] and $FF);
    case Op of
      RX_SPLIT, RX_SPLIT_LAZY, RX_JUMP:
        begin
          Bx := Integer(FCode[I] shr 8);
          if Bx >= APos then
          begin
            Inc(Bx);
            FCode[I] := EncodeOpBx(Op, Bx);
          end;
        end;
      RX_LOOKAHEAD, RX_LOOKBEHIND:
        begin
          Bx := Integer(FCode[I] shr 8);
          Negated := (Bx and LOOK_NEGATED_FLAG) <> 0;
          Bx := Bx and LOOK_TARGET_MASK;
          if Bx >= APos then
          begin
            Inc(Bx);
            if Negated then
              Bx := Bx or LOOK_NEGATED_FLAG;
            FCode[I] := EncodeOpBx(Op, Bx);
          end;
        end;
    end;
  end;
end;

procedure TRegExpCompiler.CompileDisjunction;
var
  StartPC: Integer;
  JumpHoles: array of Integer;
  JumpCount, I: Integer;
begin
  JumpCount := 0;
  SetLength(JumpHoles, 8);
  StartPC := CurrentPC;
  CompileAlternative;
  while (not AtEnd) and (Peek = '|') do
  begin
    Inc(FPos);
    if FAltStackDepth < Length(FAltStack) then
      Inc(FAltStack[FAltStackDepth]);
    InsertSplitAt(StartPC);
    // Update existing jump holes since they shifted by 1
    for I := 0 to JumpCount - 1 do
      if JumpHoles[I] >= StartPC then
        Inc(JumpHoles[I]);
    if JumpCount >= Length(JumpHoles) then
      SetLength(JumpHoles, JumpCount * 2 + 8);
    JumpHoles[JumpCount] := CurrentPC;
    Inc(JumpCount);
    Emit(0);
    PatchHole(StartPC, CurrentPC);
    StartPC := CurrentPC;
    CompileAlternative;
  end;
  for I := 0 to JumpCount - 1 do
    FCode[JumpHoles[I]] := EncodeOpBx(RX_JUMP, CurrentPC);
end;

procedure TRegExpCompiler.CompilePattern;
begin
  Emit(EncodeOpBx(RX_SAVE, 0));
  CompileDisjunction;
  Emit(EncodeOpBx(RX_SAVE, 1));
  Emit(EncodeOp(RX_MATCH));
end;

procedure TRegExpCompiler.PreScanNamedGroups;
var
  I, GroupIndex, CloseAngle, J: Integer;
  InCharClass: Boolean;
  GroupName: string;
  AltStack: array of Integer;
  AltStackDepth: Integer;
begin
  SetLength(AltStack, 64);
  AltStackDepth := 0;
  AltStack[0] := 0;
  I := 1;
  GroupIndex := 0;
  InCharClass := False;
  while I <= Length(FPattern) do
  begin
    if FPattern[I] = '\' then
    begin
      if I + 1 <= Length(FPattern) then
        Inc(I, 2)
      else
        Inc(I);
      Continue;
    end;
    if FPattern[I] = '[' then
    begin
      InCharClass := True;
      Inc(I);
      Continue;
    end;
    if (FPattern[I] = ']') and InCharClass then
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
    if FPattern[I] = '|' then
    begin
      Inc(AltStack[AltStackDepth]);
      Inc(I);
      Continue;
    end;
    if FPattern[I] = ')' then
    begin
      if AltStackDepth > 0 then
        Dec(AltStackDepth);
      Inc(I);
      Continue;
    end;
    if FPattern[I] = '(' then
    begin
      Inc(AltStackDepth);
      if AltStackDepth >= Length(AltStack) then
        SetLength(AltStack, AltStackDepth * 2 + 4);
      AltStack[AltStackDepth] := 0;
      if (I + 1 <= Length(FPattern)) and (FPattern[I + 1] = '?') then
      begin
        if (I + 2 <= Length(FPattern)) and (FPattern[I + 2] = '<') then
        begin
          if (I + 3 <= Length(FPattern)) and
             ((FPattern[I + 3] = '=') or (FPattern[I + 3] = '!')) then
          begin
            Inc(I, 3);
            Continue;
          end;
          CloseAngle := I + 3;
          while (CloseAngle <= Length(FPattern)) and
                (FPattern[CloseAngle] <> '>') do
            Inc(CloseAngle);
          if CloseAngle <= Length(FPattern) then
          begin
            Inc(GroupIndex);
            GroupName := Copy(FPattern, I + 3, CloseAngle - I - 3);
            SetLength(FNamedGroups, Length(FNamedGroups) + 1);
            FNamedGroups[High(FNamedGroups)].Name := GroupName;
            FNamedGroups[High(FNamedGroups)].Index := GroupIndex;
            SetLength(FNamedGroups[High(FNamedGroups)].DisjunctionPath,
              AltStackDepth + 1);
            for J := 0 to AltStackDepth do
              FNamedGroups[High(FNamedGroups)].DisjunctionPath[J] := AltStack[J];
            I := CloseAngle + 1;
            Continue;
          end;
        end;
        Inc(I, 2);
        Continue;
      end;
      Inc(GroupIndex);
    end;
    Inc(I);
  end;
end;

procedure TRegExpCompiler.ValidateNamedGroups;
var
  K, L, MinLen, I: Integer;
  ShareBranch: Boolean;
begin
  for K := 0 to High(FNamedGroups) - 1 do
    for L := K + 1 to High(FNamedGroups) do
      if FNamedGroups[K].Name = FNamedGroups[L].Name then
      begin
        MinLen := Length(FNamedGroups[K].DisjunctionPath);
        if Length(FNamedGroups[L].DisjunctionPath) < MinLen then
          MinLen := Length(FNamedGroups[L].DisjunctionPath);
        ShareBranch := True;
        for I := 0 to MinLen - 1 do
          if FNamedGroups[K].DisjunctionPath[I] <>
             FNamedGroups[L].DisjunctionPath[I] then
          begin
            ShareBranch := False;
            Break;
          end;
        if ShareBranch then
          raise EConvertError.CreateFmt(
            'Duplicate named capture group: %s', [FNamedGroups[K].Name]);
      end;
end;

function TRegExpCompiler.Compile: TRegExpProgram;
begin
  PreScanNamedGroups;
  ValidateNamedGroups;
  CompilePattern;
  SetLength(FCode, FCodeLen);
  Result.Code := FCode;
  Result.CharClasses := FCharClasses;
  Result.CaptureCount := FCaptureCount;
  Result.NamedGroups := FNamedGroups;
end;

function CompileRegExp(const APattern, AFlags: string): TRegExpProgram;
var
  Compiler: TRegExpCompiler;
begin
  Compiler := TRegExpCompiler.Create(APattern, AFlags);
  try
    Result := Compiler.Compile;
  finally
    Compiler.Free;
  end;
end;

procedure ValidateRegExpPatternNew(const APattern, AFlags: string);
begin
  ValidateRegExpFlags(AFlags);
  if APattern = '(?:)' then
    Exit;
  CompileRegExp(APattern, AFlags);
end;

end.
