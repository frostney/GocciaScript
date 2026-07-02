unit Goccia.RegExp.Compiler;

{$I Goccia.inc}

interface

uses
  Goccia.RegExp.&Program;

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
    RX_FAIL          = 15,
    RX_CLEAR_CAPTURES = 16,
    RX_REPEAT_ENTER  = 17,
    RX_REPEAT_CHECK  = 18,
    RX_CAPTURE_UNDEFINED_JUMP = 19,
    RX_STRING_SET    = 20
  );

const
  BACKREF_STRICT_FLAG = $800000;
  BACKREF_ICASE_FLAG = $400000;
  BACKREF_UNICODE_FLAG = $200000;
  BACKREF_INDEX_MASK = $1FFFFF;
  LOOK_NEGATED_FLAG = $800000;
  LOOK_TARGET_MASK = $7FFFFF;
  WORD_ASSERT_NEGATED_FLAG = $1;
  WORD_ASSERT_ICASE_FLAG = $2;
  CLEAR_CAPTURE_COUNT_BITS = 12;
  CLEAR_CAPTURE_COUNT_MASK = (1 shl CLEAR_CAPTURE_COUNT_BITS) - 1;

function CompileRegExp(const APattern, AFlags: string): TRegExpProgram;

implementation

uses
  Math,
  SysUtils,

  TextSemantics,
  UnicodeICU,

  Goccia.Identifier,
  Goccia.RegExp.Engine,
  Goccia.RegExp.UnicodeData;

type
  TModifierState = record
    IgnoreCase: Boolean;
    Multiline: Boolean;
    DotAll: Boolean;
  end;

  TRegExpTermCode = record
    Code: array of UInt32;
    Length: Integer;
    OriginalStart: Integer;
  end;

  TRegExpClassContents = record
    Ranges: array of TRegExpCharRange;
    RangeCount: Integer;
    Strings: array of TRegExpStringSequence;
    StringCount: Integer;
  end;

  TRegExpCompiler = class
  private
    FPattern: string;
    FPos: Integer;
    FCode: array of UInt32;
    FCodeLen: Integer;
    FCharClasses: array of TRegExpCharClass;
    FStringSets: array of TRegExpStringSet;
    FCaptureCount: Integer;
    FNamedGroups: TGocciaRegExpNamedGroups;
    FAltStack: array of Integer;
    FAltStackDepth: Integer;
    FModifier: TModifierState;
    FUnicode: Boolean;
    FUnicodeSets: Boolean;
    FPreScanCaptureCount: Integer;
    FPendingCodeUnit: Integer;
    FBackward: Boolean;
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
    function AddStringSet(const AContents: TRegExpClassContents): Integer;
    procedure EmitRawCharClassRanges(const ARanges: array of TRegExpCharRange;
      ANegated: Boolean);
    procedure CompilePattern;
    procedure CompileDisjunction;
    procedure CompileAlternative;
    procedure CompileTerm;
    procedure CompileAtom;
    procedure CompileQuantifier(AAtomStart: Integer);
    procedure CompileCharacterClass;
    procedure CompileUnicodeSetsClass;
    procedure ApplyNestedSetOps(var AContents: TRegExpClassContents);
    procedure ParseClassUnion(var AContents: TRegExpClassContents);
    procedure ParseClassEscape(var AContents: TRegExpClassContents);
    procedure ParseStringDisjunction(var AContents: TRegExpClassContents);
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
    procedure EmitClassContents(const AContents: TRegExpClassContents;
      ANegated: Boolean);
    procedure AddBuiltinCharClass(AEscapeChar: Char; var ARanges: array of TRegExpCharRange; var ARangeCount: Integer);
    procedure AddWordCharacterRanges(var ARanges: array of TRegExpCharRange; var ARangeCount: Integer);
    procedure AddCaseFoldedWordComplementRanges(var ARanges: array of TRegExpCharRange; var ARangeCount: Integer);
    procedure AddRange(var ARanges: array of TRegExpCharRange; var ARangeCount: Integer; ALo, AHi: Cardinal);
    procedure EmitUnicodePropertyClass(const APropertyName: string; ANegated: Boolean);
    procedure GetUnicodePropertyRanges(const APropertyName: string; var ARanges: array of TRegExpCharRange; var ARangeCount: Integer);
    function IsStringProperty(const APropertyName: string): Boolean;
    procedure GetStringPropertySequences(const APropertyName: string;
      var AContents: TRegExpClassContents);
    function ReadCodePoint: Cardinal;
    procedure EnsureCodeCapacity(ANeeded: Integer);
    procedure EmitBody(const ABody: array of UInt32; ALen: Integer);
    procedure EmitBodyAt(const ABody: array of UInt32; ALen: Integer;
      AOrigStart: Integer);
    procedure ValidateNamedGroups;
    procedure PreScanNamedGroups;
    procedure InsertSplitAt(APos: Integer);
    procedure EmitDuplicateNamedBackref(const AName: string;
      ABackrefFlags: Integer);
  public
    constructor Create(const APattern, AFlags: string);
    function Compile: TRegExpProgram;
  end;

const
  MAX_CHAR_RANGES = 2048;

procedure NormalizeRanges(var ARanges: array of TRegExpCharRange;
  var ARangeCount: Integer); forward;

function IsRegExpHexString(const AValue: string): Boolean;
var
  I: Integer;
begin
  if AValue = '' then
    Exit(False);
  for I := 1 to Length(AValue) do
    if not CharInSet(AValue[I], ['0'..'9', 'a'..'f', 'A'..'F']) then
      Exit(False);
  Result := True;
end;

function DecodeRegExpGroupNameUnicodeEscape(const AText: string;
  var AIndex: Integer): Cardinal;
var
  CodePoint: Cardinal;
  HexStart: Integer;
  HexStr: string;
  LowSurrogate: Cardinal;
begin
  if (AIndex + 1 > Length(AText)) or (AText[AIndex] <> '\') or
     (AText[AIndex + 1] <> 'u') then
    raise EConvertError.Create('Invalid Unicode escape in group name');

  Inc(AIndex, 2);
  if (AIndex <= Length(AText)) and (AText[AIndex] = '{') then
  begin
    Inc(AIndex);
    HexStart := AIndex;
    while (AIndex <= Length(AText)) and (AText[AIndex] <> '}') do
      Inc(AIndex);
    if AIndex > Length(AText) then
      raise EConvertError.Create('Unterminated Unicode escape in group name');
    HexStr := Copy(AText, HexStart, AIndex - HexStart);
    if not IsRegExpHexString(HexStr) then
      raise EConvertError.Create('Invalid Unicode escape in group name');
    Inc(AIndex);
  end
  else
  begin
    if AIndex + 3 > Length(AText) then
      raise EConvertError.Create('Invalid Unicode escape in group name');
    HexStr := Copy(AText, AIndex, 4);
    if not IsRegExpHexString(HexStr) then
      raise EConvertError.Create('Invalid Unicode escape in group name');
    Inc(AIndex, 4);
  end;

  CodePoint := StrToInt('$' + HexStr);
  if (CodePoint >= $D800) and (CodePoint <= $DBFF) and
     (AIndex + 5 <= Length(AText)) and (AText[AIndex] = '\') and
     (AText[AIndex + 1] = 'u') then
  begin
    HexStr := Copy(AText, AIndex + 2, 4);
    if IsRegExpHexString(HexStr) then
    begin
      LowSurrogate := StrToInt('$' + HexStr);
      if (LowSurrogate >= $DC00) and (LowSurrogate <= $DFFF) then
      begin
        CodePoint := $10000 + ((CodePoint - $D800) shl 10) +
          (LowSurrogate - $DC00);
        Inc(AIndex, 6);
      end;
    end;
  end;
  if CodePoint > $10FFFF then
    raise EConvertError.Create('Unicode escape out of range in group name');

  Result := CodePoint;
end;

function ReadRegExpGroupNameLiteralCodePoint(const AText: string;
  var AIndex: Integer): Cardinal;
var
  ByteLength: Integer;
  CodePoint: Cardinal;
  LowSurrogate: Cardinal;
  LowSurrogateByteLength: Integer;
begin
  if not TryReadUTF8CodePointAllowSurrogates(AText, AIndex, CodePoint,
     ByteLength) then
  begin
    Result := Ord(AText[AIndex]);
    Inc(AIndex);
    Exit;
  end;

  Inc(AIndex, ByteLength);
  if (CodePoint >= $D800) and (CodePoint <= $DBFF) and
     TryReadUTF8CodePointAllowSurrogates(AText, AIndex, LowSurrogate,
       LowSurrogateByteLength) and
     (LowSurrogate >= $DC00) and (LowSurrogate <= $DFFF) then
  begin
    Inc(AIndex, LowSurrogateByteLength);
    CodePoint := $10000 + ((CodePoint - $D800) shl 10) +
      (LowSurrogate - $DC00);
  end;

  Result := CodePoint;
end;

procedure ValidateRegExpGroupNameCodePoint(ACodePoint: Cardinal;
  AAtStart: Boolean);
begin
  if AAtStart then
  begin
    if not IsIdentifierStartCodePoint(ACodePoint) then
      raise EConvertError.Create('Invalid group name');
  end
  else if not IsIdentifierPartCodePoint(ACodePoint) then
    raise EConvertError.Create('Invalid group name');
end;

function DecodeRegExpGroupName(const ARawName: string): string;
var
  CodePoint: Cardinal;
  I: Integer;
  AtStart: Boolean;
begin
  if ARawName = '' then
    raise EConvertError.Create('Invalid group name');

  Result := '';
  I := 1;
  AtStart := True;
  while I <= Length(ARawName) do
  begin
    if (ARawName[I] = '\') and (I + 1 <= Length(ARawName)) and
       (ARawName[I + 1] = 'u') then
      CodePoint := DecodeRegExpGroupNameUnicodeEscape(ARawName, I)
    else
      CodePoint := ReadRegExpGroupNameLiteralCodePoint(ARawName, I);

    ValidateRegExpGroupNameCodePoint(CodePoint, AtStart);
    Result := Result + CodePointToUTF8(CodePoint);
    AtStart := False;
  end;
end;

constructor TRegExpCompiler.Create(const APattern, AFlags: string);
begin
  inherited Create;
  FPattern := APattern;
  FPos := 1;
  FCodeLen := 0;
  SetLength(FCode, 256);
  SetLength(FCharClasses, 0);
  SetLength(FStringSets, 0);
  FCaptureCount := 0;
  SetLength(FNamedGroups, 0);
  SetLength(FAltStack, 64);
  FAltStackDepth := 0;
  FAltStack[0] := 0;
  FModifier.IgnoreCase := HasRegExpFlag(AFlags, 'i');
  FModifier.Multiline := HasRegExpFlag(AFlags, 'm');
  FModifier.DotAll := HasRegExpFlag(AFlags, 's');
  FUnicodeSets := HasRegExpFlag(AFlags, 'v');
  FUnicode := HasRegExpFlag(AFlags, 'u') or FUnicodeSets;
  FPreScanCaptureCount := 0;
  FPendingCodeUnit := -1;
  FBackward := False;
end;

function TRegExpCompiler.Peek: Char;
begin
  if FPendingCodeUnit >= 0 then
    Exit(#1);
  if FPos <= Length(FPattern) then
    Result := FPattern[FPos]
  else
    Result := #0;
end;

function TRegExpCompiler.PeekAt(AOffset: Integer): Char;
var
  Idx: Integer;
begin
  if FPendingCodeUnit >= 0 then
  begin
    if AOffset = 0 then
      Exit(#1);
    Dec(AOffset);
  end;
  Idx := FPos + AOffset;
  if (Idx >= 1) and (Idx <= Length(FPattern)) then
    Result := FPattern[Idx]
  else
    Result := #0;
end;

function TRegExpCompiler.AtEnd: Boolean;
begin
  Result := (FPendingCodeUnit < 0) and (FPos > Length(FPattern));
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
  Page: Integer;
  PageLo: Cardinal;
  RangeCount: Integer;
  RangeIndex: Integer;
  Normalized: TRegExpCharRangeArray;
begin
  Result := Length(FCharClasses);
  SetLength(FCharClasses, Result + 1);
  RangeCount := Length(ARanges);
  SetLength(Normalized, RangeCount);
  for I := 0 to High(ARanges) do
    Normalized[I] := ARanges[I];
  NormalizeRanges(Normalized, RangeCount);
  SetLength(FCharClasses[Result].Ranges, RangeCount);
  for I := 0 to RangeCount - 1 do
    FCharClasses[Result].Ranges[I] := Normalized[I];
  if RangeCount > 8 then
  begin
    SetLength(FCharClasses[Result].PageFirstRange,
      REGEXP_CHAR_CLASS_PAGE_COUNT);
    RangeIndex := 0;
    for Page := 0 to REGEXP_CHAR_CLASS_PAGE_COUNT - 1 do
    begin
      PageLo := Cardinal(Page) shl REGEXP_CHAR_CLASS_PAGE_BITS;
      while (RangeIndex < RangeCount) and
            (FCharClasses[Result].Ranges[RangeIndex].Hi < PageLo) do
        Inc(RangeIndex);
      FCharClasses[Result].PageFirstRange[Page] := RangeIndex;
    end;
  end
  else
    SetLength(FCharClasses[Result].PageFirstRange, 0);
end;

function TRegExpCompiler.AddStringSet(
  const AContents: TRegExpClassContents): Integer;
var
  I, StringIndex: Integer;
  Ranges: TRegExpCharRangeArray;
begin
  Result := Length(FStringSets);
  SetLength(FStringSets, Result + 1);

  SetLength(Ranges, AContents.RangeCount);
  for I := 0 to AContents.RangeCount - 1 do
    Ranges[I] := AContents.Ranges[I];
  FStringSets[Result].CharClassIndex := AddCharClass(Ranges);

  SetLength(FStringSets[Result].Strings, AContents.StringCount);
  StringIndex := 0;
  for I := AContents.StringCount - 1 downto 0 do
    if Length(AContents.Strings[I].CodePoints) > 1 then
    begin
      FStringSets[Result].Strings[StringIndex] := AContents.Strings[I];
      Inc(StringIndex);
    end;
  SetLength(FStringSets[Result].Strings, StringIndex);
end;

procedure TRegExpCompiler.EmitCharMatch(ACodePoint: Cardinal);
var
  Ranges: array[0..1] of TRegExpCharRange;
begin
  if FModifier.IgnoreCase then
  begin
    Ranges[0].Lo := ACodePoint;
    Ranges[0].Hi := ACodePoint;
    EmitCharClassRanges(Ranges, 1, False);
    Exit;
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

procedure TRegExpCompiler.AddWordCharacterRanges(
  var ARanges: array of TRegExpCharRange; var ARangeCount: Integer);
begin
  AddRange(ARanges, ARangeCount, Ord('0'), Ord('9'));
  AddRange(ARanges, ARangeCount, Ord('A'), Ord('Z'));
  AddRange(ARanges, ARangeCount, Ord('_'), Ord('_'));
  AddRange(ARanges, ARangeCount, Ord('a'), Ord('z'));
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
      AddWordCharacterRanges(ARanges, ARangeCount);
    'W':
      begin
        if FModifier.IgnoreCase and FUnicode then
          AddCaseFoldedWordComplementRanges(ARanges, ARangeCount)
        else
        begin
          AddRange(ARanges, ARangeCount, 0, Ord('0') - 1);
          AddRange(ARanges, ARangeCount, Ord('9') + 1, Ord('A') - 1);
          AddRange(ARanges, ARangeCount, Ord('Z') + 1, Ord('_') - 1);
          AddRange(ARanges, ARangeCount, Ord('_') + 1, Ord('a') - 1);
          AddRange(ARanges, ARangeCount, Ord('z') + 1, $10FFFF);
        end;
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
var
  EqPos, I: Integer;
  LookupPropPart, LookupValuePart, PropPart, ValuePart: string;
  ICURanges: TUnicodePropertyRangeArray;

  procedure CopyICURanges;
  var
    J: Integer;
  begin
    if ARangeCount + Length(ICURanges) > Length(ARanges) then
      raise EConvertError.Create('Unicode property range count exceeds buffer capacity');
    for J := 0 to High(ICURanges) do
      AddRange(ARanges, ARangeCount, ICURanges[J].Lo, ICURanges[J].Hi);
  end;

  procedure SubtractUnicodeRanges(var ABase: TUnicodePropertyRangeArray;
    const ARemove: TUnicodePropertyRangeArray);
  var
    BaseRange, RemoveRange: TUnicodePropertyRange;
    Cursor: Cardinal;
    OutRanges: TUnicodePropertyRangeArray;
    OutCount, RemoveIndex, ScanIndex: Integer;
  begin
    SetLength(OutRanges, Length(ABase) + Length(ARemove));
    OutCount := 0;
    RemoveIndex := 0;

    for BaseRange in ABase do
    begin
      Cursor := BaseRange.Lo;
      while (RemoveIndex <= High(ARemove)) and
            (ARemove[RemoveIndex].Hi < Cursor) do
        Inc(RemoveIndex);

      ScanIndex := RemoveIndex;
      while (ScanIndex <= High(ARemove)) and
            (ARemove[ScanIndex].Lo <= BaseRange.Hi) do
      begin
        RemoveRange := ARemove[ScanIndex];
        if RemoveRange.Lo > Cursor then
        begin
          OutRanges[OutCount].Lo := Cursor;
          OutRanges[OutCount].Hi := RemoveRange.Lo - 1;
          Inc(OutCount);
        end;
        if RemoveRange.Hi = High(Cardinal) then
        begin
          Cursor := High(Cardinal);
          Break;
        end;
        Cursor := Max(Cursor, RemoveRange.Hi + 1);
        if Cursor > BaseRange.Hi then
          Break;
        Inc(ScanIndex);
      end;

      if Cursor <= BaseRange.Hi then
      begin
        OutRanges[OutCount].Lo := Cursor;
        OutRanges[OutCount].Hi := BaseRange.Hi;
        Inc(OutCount);
      end;
    end;

    SetLength(OutRanges, OutCount);
    ABase := OutRanges;
  end;

  procedure ExcludeUnknownFromCommonScriptExtensions;
  var
    UnknownRanges: TUnicodePropertyRangeArray;
  begin
    if (LookupPropPart = 'scx') and
       ((LookupValuePart = 'Zyyy') or (ValuePart = 'Common')) and
       (TryGetUnicodePropertyRanges('scx/Zzzz', UnknownRanges) or
        TryGetUnicodePropertyRanges('scx/Unknown', UnknownRanges)) then
      SubtractUnicodeRanges(ICURanges, UnknownRanges);
  end;

begin
  EqPos := Pos('=', APropertyName);
  if EqPos > 0 then
  begin
    PropPart := Copy(APropertyName, 1, EqPos - 1);
    ValuePart := Copy(APropertyName, EqPos + 1, Length(APropertyName) - EqPos);
  end
  else
  begin
    PropPart := APropertyName;
    ValuePart := '';
  end;

  if (PropPart = 'Script_Extensions') or (PropPart = 'scx') then
    LookupPropPart := 'scx'
  else if (PropPart = 'Script') or (PropPart = 'sc') then
    LookupPropPart := 'sc'
  else if (PropPart = 'General_Category') or (PropPart = 'gc') then
    LookupPropPart := 'gc'
  else
    LookupPropPart := PropPart;

  if ((LookupPropPart = 'sc') or (LookupPropPart = 'scx')) and
     (ValuePart = 'Common') then
    LookupValuePart := 'Zyyy'
  else if ((LookupPropPart = 'sc') or (LookupPropPart = 'scx')) and
          (ValuePart = 'Unknown') then
    LookupValuePart := 'Zzzz'
  else
    LookupValuePart := ValuePart;

  if EqPos > 0 then
  begin
    if TryGetUnicodePropertyRanges(LookupPropPart + '/' + LookupValuePart,
       ICURanges) then
    begin
      ExcludeUnknownFromCommonScriptExtensions;
      CopyICURanges;
      Exit;
    end;

    if (LookupValuePart <> ValuePart) and
       TryGetUnicodePropertyRanges(LookupPropPart + '/' + ValuePart,
       ICURanges) then
    begin
      ExcludeUnknownFromCommonScriptExtensions;
      CopyICURanges;
      Exit;
    end;
  end
  else
  begin
    if TryGetUnicodePropertyRanges(APropertyName, ICURanges) then
    begin
      CopyICURanges;
      Exit;
    end;

    if TryGetUnicodePropertyRanges('gc/' + APropertyName, ICURanges) then
    begin
      CopyICURanges;
      Exit;
    end;
  end;

  if TryICUGetUnicodePropertyRanges(PropPart, ValuePart, ICURanges) then
  begin
    ExcludeUnknownFromCommonScriptExtensions;
    CopyICURanges;
    Exit;
  end;

  raise EConvertError.Create('Invalid Unicode property name: ' + APropertyName);
end;

function CharRangesToUnicodeRanges(const ARanges: array of TRegExpCharRange;
  ARangeCount: Integer): TUnicodePropertyRangeArray;
var
  I: Integer;
begin
  SetLength(Result, ARangeCount);
  for I := 0 to ARangeCount - 1 do
  begin
    Result[I].Lo := ARanges[I].Lo;
    Result[I].Hi := ARanges[I].Hi;
  end;
end;

function UnicodeRangesToCharRanges(const ARanges: TUnicodePropertyRangeArray):
  TRegExpCharRangeArray;
var
  I: Integer;
begin
  SetLength(Result, Length(ARanges));
  for I := 0 to High(ARanges) do
  begin
    Result[I].Lo := ARanges[I].Lo;
    Result[I].Hi := ARanges[I].Hi;
  end;
end;

procedure TRegExpCompiler.EmitUnicodePropertyClass(const APropertyName: string;
  ANegated: Boolean);
var
  Ranges: array[0..MAX_CHAR_RANGES - 1] of TRegExpCharRange;
  FoldRanges: TUnicodePropertyRangeArray;
  RangeCount: Integer;
begin
  RangeCount := 0;
  GetUnicodePropertyRanges(APropertyName, Ranges, RangeCount);
  if ANegated and FModifier.IgnoreCase and FUnicode then
  begin
    FoldRanges := CharRangesToUnicodeRanges(Ranges, RangeCount);
    ReduceUnicodeSimpleCaseFoldClosed(FoldRanges);
    EmitRawCharClassRanges(UnicodeRangesToCharRanges(FoldRanges), True);
    Exit;
  end;
  EmitCharClassRanges(Ranges, RangeCount, ANegated);
end;

procedure NormalizeRanges(var ARanges: array of TRegExpCharRange;
  var ARangeCount: Integer);
var
  I, J, WriteIdx: Integer;
  Temp: TRegExpCharRange;
begin
  if ARangeCount <= 1 then
    Exit;
  for I := 1 to ARangeCount - 1 do
  begin
    Temp := ARanges[I];
    J := I - 1;
    while (J >= 0) and (ARanges[J].Lo > Temp.Lo) do
    begin
      ARanges[J + 1] := ARanges[J];
      Dec(J);
    end;
    ARanges[J + 1] := Temp;
  end;
  WriteIdx := 0;
  for I := 1 to ARangeCount - 1 do
  begin
    if ARanges[I].Lo <= ARanges[WriteIdx].Hi + 1 then
    begin
      if ARanges[I].Hi > ARanges[WriteIdx].Hi then
        ARanges[WriteIdx].Hi := ARanges[I].Hi;
    end
    else
    begin
      Inc(WriteIdx);
      ARanges[WriteIdx] := ARanges[I];
    end;
  end;
  ARangeCount := WriteIdx + 1;
end;

procedure ClearStartCheck(var ACheck: TRegExpStartCheck);
var
  I: Integer;
begin
  ACheck.Enabled := False;
  ACheck.HasNonLatin1 := False;
  for I := Low(ACheck.Latin1Bits) to High(ACheck.Latin1Bits) do
    ACheck.Latin1Bits[I] := 0;
end;

procedure IncludeStartCheckLatin1(var ACheck: TRegExpStartCheck;
  ACodePoint: Cardinal); inline;
begin
  ACheck.Latin1Bits[ACodePoint shr 5] :=
    ACheck.Latin1Bits[ACodePoint shr 5] or (UInt32(1) shl (ACodePoint and 31));
end;

procedure IncludeStartCheckRange(var ACheck: TRegExpStartCheck;
  const ARange: TRegExpCharRange);
var
  CodePoint: Cardinal;
  Hi: Cardinal;
begin
  if ARange.Lo > $FF then
  begin
    ACheck.HasNonLatin1 := True;
    Exit;
  end;

  Hi := ARange.Hi;
  if Hi > $FF then
  begin
    Hi := $FF;
    ACheck.HasNonLatin1 := True;
  end;

  for CodePoint := ARange.Lo to Hi do
    IncludeStartCheckLatin1(ACheck, CodePoint);
end;

function StartCheckIsUseful(const ACheck: TRegExpStartCheck): Boolean;
var
  I: Integer;
begin
  if not ACheck.Enabled then
    Exit(False);
  if not ACheck.HasNonLatin1 then
    Exit(True);
  for I := Low(ACheck.Latin1Bits) to High(ACheck.Latin1Bits) do
    if ACheck.Latin1Bits[I] <> High(UInt32) then
      Exit(True);
  Result := False;
end;

procedure BuildStartCheck(const ACode: array of UInt32; ACodeLen: Integer;
  const ACharClasses: array of TRegExpCharClass; out ACheck: TRegExpStartCheck);
const
  MAX_ZERO_WIDTH_PREFIX = 64;
var
  Bx: Integer;
  I: Integer;
  Instr: UInt32;
  Op: TRegExpOpCode;
  PC: Integer;
  Range: TRegExpCharRange;
  Steps: Integer;
begin
  ClearStartCheck(ACheck);
  PC := 0;
  Steps := 0;
  while (PC >= 0) and (PC < ACodeLen) and (Steps < MAX_ZERO_WIDTH_PREFIX) do
  begin
    Inc(Steps);
    Instr := ACode[PC];
    Op := TRegExpOpCode(Instr and $FF);
    Bx := Integer(Instr shr 8);
    case Op of
      RX_SAVE,
      RX_CLEAR_CAPTURES,
      RX_REPEAT_ENTER,
      RX_REPEAT_CHECK,
      RX_ASSERT_START,
      RX_ASSERT_END,
      RX_ASSERT_WORD:
        Inc(PC);

      RX_CHAR:
        begin
          ACheck.Enabled := True;
          Range.Lo := Cardinal(Bx);
          Range.Hi := Cardinal(Bx);
          IncludeStartCheckRange(ACheck, Range);
          if not StartCheckIsUseful(ACheck) then
            ClearStartCheck(ACheck);
          Exit;
        end;

      RX_CHAR_CLASS:
        begin
          if (Bx < 0) or (Bx > High(ACharClasses)) then
            Exit;
          ACheck.Enabled := True;
          for I := 0 to High(ACharClasses[Bx].Ranges) do
            IncludeStartCheckRange(ACheck, ACharClasses[Bx].Ranges[I]);
          if not StartCheckIsUseful(ACheck) then
            ClearStartCheck(ACheck);
          Exit;
        end;
    else
      Exit;
    end;
  end;
end;

procedure IntersectRanges(
  const ALeft: array of TRegExpCharRange; ALeftCount: Integer;
  const ARight: array of TRegExpCharRange; ARightCount: Integer;
  var AOut: array of TRegExpCharRange; var AOutCount: Integer);
var
  I, J: Integer;
  Lo, Hi: Cardinal;
begin
  AOutCount := 0;
  I := 0;
  J := 0;
  while (I < ALeftCount) and (J < ARightCount) do
  begin
    if ALeft[I].Lo > ARight[J].Lo then
      Lo := ALeft[I].Lo
    else
      Lo := ARight[J].Lo;
    if ALeft[I].Hi < ARight[J].Hi then
      Hi := ALeft[I].Hi
    else
      Hi := ARight[J].Hi;
    if Lo <= Hi then
    begin
      if AOutCount < Length(AOut) then
      begin
        AOut[AOutCount].Lo := Lo;
        AOut[AOutCount].Hi := Hi;
        Inc(AOutCount);
      end;
    end;
    if ALeft[I].Hi < ARight[J].Hi then
      Inc(I)
    else
      Inc(J);
  end;
end;

procedure SubtractRanges(
  const ALeft: array of TRegExpCharRange; ALeftCount: Integer;
  const ARight: array of TRegExpCharRange; ARightCount: Integer;
  var AOut: array of TRegExpCharRange; var AOutCount: Integer);
var
  I, J: Integer;
  CurLo, CurHi: Cardinal;
begin
  AOutCount := 0;
  J := 0;
  for I := 0 to ALeftCount - 1 do
  begin
    CurLo := ALeft[I].Lo;
    CurHi := ALeft[I].Hi;
    while (J < ARightCount) and (ARight[J].Hi < CurLo) do
      Inc(J);
    while (J < ARightCount) and (ARight[J].Lo <= CurHi) do
    begin
      if ARight[J].Lo > CurLo then
      begin
        if AOutCount < Length(AOut) then
        begin
          AOut[AOutCount].Lo := CurLo;
          AOut[AOutCount].Hi := ARight[J].Lo - 1;
          Inc(AOutCount);
        end;
      end;
      if ARight[J].Hi >= CurHi then
      begin
        CurLo := CurHi + 1;
        Break;
      end;
      CurLo := ARight[J].Hi + 1;
      Inc(J);
    end;
    if CurLo <= CurHi then
    begin
      if AOutCount < Length(AOut) then
      begin
        AOut[AOutCount].Lo := CurLo;
        AOut[AOutCount].Hi := CurHi;
        Inc(AOutCount);
      end;
    end;
  end;
end;

procedure TRegExpCompiler.AddCaseFoldedWordComplementRanges(
  var ARanges: array of TRegExpCharRange; var ARangeCount: Integer);
var
  AllRange: array[0..0] of TRegExpCharRange;
  ComplementRanges: array[0..MAX_CHAR_RANGES - 1] of TRegExpCharRange;
  FoldRanges: TUnicodePropertyRangeArray;
  FoldedRanges: array[0..MAX_CHAR_RANGES - 1] of TRegExpCharRange;
  WordRanges: array[0..MAX_CHAR_RANGES - 1] of TRegExpCharRange;
  ComplementCount, FoldedCount, I, WordCount: Integer;
begin
  WordCount := 0;
  AddWordCharacterRanges(WordRanges, WordCount);
  SetLength(FoldRanges, WordCount);
  for I := 0 to WordCount - 1 do
  begin
    FoldRanges[I].Lo := WordRanges[I].Lo;
    FoldRanges[I].Hi := WordRanges[I].Hi;
  end;
  ExpandUnicodeSimpleCaseFolding(FoldRanges);
  if Length(FoldRanges) > Length(FoldedRanges) then
    raise EConvertError.Create('Folded word character range count exceeds buffer capacity');
  FoldedCount := Length(FoldRanges);
  for I := 0 to FoldedCount - 1 do
  begin
    FoldedRanges[I].Lo := FoldRanges[I].Lo;
    FoldedRanges[I].Hi := FoldRanges[I].Hi;
  end;
  NormalizeRanges(FoldedRanges, FoldedCount);

  AllRange[0].Lo := 0;
  AllRange[0].Hi := $10FFFF;
  SubtractRanges(AllRange, 1, FoldedRanges, FoldedCount,
    ComplementRanges, ComplementCount);
  for I := 0 to ComplementCount - 1 do
    AddRange(ARanges, ARangeCount, ComplementRanges[I].Lo,
      ComplementRanges[I].Hi);
end;

procedure InitClassContents(var AContents: TRegExpClassContents);
begin
  SetLength(AContents.Ranges, MAX_CHAR_RANGES);
  AContents.RangeCount := 0;
  SetLength(AContents.Strings, 0);
  AContents.StringCount := 0;
end;

procedure AddClassRange(var AContents: TRegExpClassContents;
  ALo, AHi: Cardinal);
begin
  if AContents.RangeCount < Length(AContents.Ranges) then
  begin
    AContents.Ranges[AContents.RangeCount].Lo := ALo;
    AContents.Ranges[AContents.RangeCount].Hi := AHi;
    Inc(AContents.RangeCount);
  end;
end;

procedure AddClassString(var AContents: TRegExpClassContents;
  const ACodePoints: array of Cardinal);
var
  Seq: TRegExpStringSequence;
  I: Integer;
begin
  SetLength(Seq.CodePoints, Length(ACodePoints));
  for I := 0 to High(ACodePoints) do
    Seq.CodePoints[I] := ACodePoints[I];
  if AContents.StringCount >= Length(AContents.Strings) then
    SetLength(AContents.Strings, AContents.StringCount + 16);
  AContents.Strings[AContents.StringCount] := Seq;
  Inc(AContents.StringCount);
end;

procedure MergeClassContents(var ADest: TRegExpClassContents;
  const ASrc: TRegExpClassContents);
var
  I: Integer;
begin
  for I := 0 to ASrc.RangeCount - 1 do
    AddClassRange(ADest, ASrc.Ranges[I].Lo, ASrc.Ranges[I].Hi);
  for I := 0 to ASrc.StringCount - 1 do
  begin
    if ADest.StringCount >= Length(ADest.Strings) then
      SetLength(ADest.Strings, ADest.StringCount + 16);
    ADest.Strings[ADest.StringCount] := ASrc.Strings[I];
    Inc(ADest.StringCount);
  end;
end;

procedure FilterClassStrings(var ADest: TRegExpClassContents;
  const ARight: TRegExpClassContents; AKeepMatched: Boolean);
var
  I, J: Integer;
  Found: Boolean;
  ResultStrings: array of TRegExpStringSequence;
  ResultStringCount: Integer;
begin
  ResultStringCount := 0;
  SetLength(ResultStrings, ADest.StringCount);
  for I := 0 to ADest.StringCount - 1 do
  begin
    Found := False;
    for J := 0 to ARight.StringCount - 1 do
    begin
      if Length(ADest.Strings[I].CodePoints) = Length(ARight.Strings[J].CodePoints) then
      begin
        Found := True;
        if Length(ADest.Strings[I].CodePoints) > 0 then
          Found := CompareMem(@ADest.Strings[I].CodePoints[0],
            @ARight.Strings[J].CodePoints[0],
            Length(ADest.Strings[I].CodePoints) * SizeOf(Cardinal));
      end;
      if Found then
        Break;
    end;
    if Found = AKeepMatched then
    begin
      ResultStrings[ResultStringCount] := ADest.Strings[I];
      Inc(ResultStringCount);
    end;
  end;
  ADest.StringCount := ResultStringCount;
  SetLength(ADest.Strings, ResultStringCount);
  for I := 0 to ResultStringCount - 1 do
    ADest.Strings[I] := ResultStrings[I];
end;

procedure ApplyClassSetOp(var ADest: TRegExpClassContents;
  const ARight: TRegExpClassContents; AIsIntersect: Boolean);
var
  ResultRanges: array[0..MAX_CHAR_RANGES - 1] of TRegExpCharRange;
  ResultCount: Integer;
  NormLeft, NormRight: array[0..MAX_CHAR_RANGES - 1] of TRegExpCharRange;
  NormLeftCount, NormRightCount, I: Integer;
begin
  NormLeftCount := ADest.RangeCount;
  for I := 0 to NormLeftCount - 1 do
    NormLeft[I] := ADest.Ranges[I];
  NormalizeRanges(NormLeft, NormLeftCount);
  NormRightCount := ARight.RangeCount;
  for I := 0 to NormRightCount - 1 do
    NormRight[I] := ARight.Ranges[I];
  NormalizeRanges(NormRight, NormRightCount);
  if AIsIntersect then
    IntersectRanges(NormLeft, NormLeftCount, NormRight, NormRightCount,
      ResultRanges, ResultCount)
  else
    SubtractRanges(NormLeft, NormLeftCount, NormRight, NormRightCount,
      ResultRanges, ResultCount);
  ADest.RangeCount := ResultCount;
  for I := 0 to ResultCount - 1 do
    ADest.Ranges[I] := ResultRanges[I];
  FilterClassStrings(ADest, ARight, AIsIntersect);
end;

procedure IntersectClassContents(var ADest: TRegExpClassContents;
  const ARight: TRegExpClassContents);
begin
  ApplyClassSetOp(ADest, ARight, True);
end;

procedure SubtractClassContents(var ADest: TRegExpClassContents;
  const ARight: TRegExpClassContents);
begin
  ApplyClassSetOp(ADest, ARight, False);
end;

procedure ComplementContents(var AContents: TRegExpClassContents);
var
  AllRange: array[0..0] of TRegExpCharRange;
  ResultRanges: array[0..MAX_CHAR_RANGES - 1] of TRegExpCharRange;
  ResultCount, I: Integer;
begin
  AllRange[0].Lo := 0;
  AllRange[0].Hi := $10FFFF;
  SubtractRanges(AllRange, 1, AContents.Ranges, AContents.RangeCount,
    ResultRanges, ResultCount);
  AContents.RangeCount := ResultCount;
  for I := 0 to ResultCount - 1 do
    AContents.Ranges[I] := ResultRanges[I];
end;

function TRegExpCompiler.IsStringProperty(const APropertyName: string): Boolean;
begin
  Result := (APropertyName = 'Basic_Emoji') or
    (APropertyName = 'Emoji_Keycap_Sequence') or
    (APropertyName = 'RGI_Emoji_Flag_Sequence') or
    (APropertyName = 'RGI_Emoji_Modifier_Sequence') or
    (APropertyName = 'RGI_Emoji_Tag_Sequence') or
    (APropertyName = 'RGI_Emoji_ZWJ_Sequence') or
    (APropertyName = 'RGI_Emoji');
end;

procedure TRegExpCompiler.GetStringPropertySequences(
  const APropertyName: string; var AContents: TRegExpClassContents);
var
  I: Integer;
  Sequences: TUnicodeStringSequenceArray;
begin
  if TryGetUnicodeStringPropertySequences('strings/' + APropertyName,
     Sequences) then
  begin
    for I := 0 to High(Sequences) do
    begin
      if Length(Sequences[I].CodePoints) = 1 then
        AddClassRange(AContents, Sequences[I].CodePoints[0],
          Sequences[I].CodePoints[0])
      else if Length(Sequences[I].CodePoints) > 1 then
        AddClassString(AContents, Sequences[I].CodePoints);
    end;
    Exit;
  end;

  if APropertyName = 'RGI_Emoji' then
  begin
    GetStringPropertySequences('Basic_Emoji', AContents);
    GetStringPropertySequences('Emoji_Keycap_Sequence', AContents);
    GetStringPropertySequences('RGI_Emoji_Flag_Sequence', AContents);
    GetStringPropertySequences('RGI_Emoji_Modifier_Sequence', AContents);
    GetStringPropertySequences('RGI_Emoji_Tag_Sequence', AContents);
    GetStringPropertySequences('RGI_Emoji_ZWJ_Sequence', AContents);
    Exit;
  end;

  if APropertyName = 'Basic_Emoji' then
  begin
    AddClassRange(AContents, $231A, $231B);
    AddClassRange(AContents, $23E9, $23F3);
    AddClassRange(AContents, $23F8, $23FA);
    AddClassRange(AContents, $25AA, $25AB);
    AddClassRange(AContents, $25B6, $25B6);
    AddClassRange(AContents, $25C0, $25C0);
    AddClassRange(AContents, $25FB, $25FE);
    AddClassRange(AContents, $2600, $2604);
    AddClassRange(AContents, $260E, $260E);
    AddClassRange(AContents, $2611, $2611);
    AddClassRange(AContents, $2614, $2615);
    AddClassRange(AContents, $2618, $2618);
    AddClassRange(AContents, $261D, $261D);
    AddClassRange(AContents, $2620, $2620);
    AddClassRange(AContents, $2622, $2623);
    AddClassRange(AContents, $2626, $2626);
    AddClassRange(AContents, $262A, $262A);
    AddClassRange(AContents, $262E, $262F);
    AddClassRange(AContents, $2638, $263A);
    AddClassRange(AContents, $2640, $2640);
    AddClassRange(AContents, $2642, $2642);
    AddClassRange(AContents, $2648, $2653);
    AddClassRange(AContents, $265F, $2660);
    AddClassRange(AContents, $2663, $2663);
    AddClassRange(AContents, $2665, $2666);
    AddClassRange(AContents, $2668, $2668);
    AddClassRange(AContents, $267B, $267B);
    AddClassRange(AContents, $267E, $267F);
    AddClassRange(AContents, $2692, $2697);
    AddClassRange(AContents, $2699, $2699);
    AddClassRange(AContents, $269B, $269C);
    AddClassRange(AContents, $26A0, $26A1);
    AddClassRange(AContents, $26A7, $26A7);
    AddClassRange(AContents, $26AA, $26AB);
    AddClassRange(AContents, $26B0, $26B1);
    AddClassRange(AContents, $26BD, $26BE);
    AddClassRange(AContents, $26C4, $26C5);
    AddClassRange(AContents, $26C8, $26C8);
    AddClassRange(AContents, $26CE, $26CF);
    AddClassRange(AContents, $26D1, $26D1);
    AddClassRange(AContents, $26D3, $26D4);
    AddClassRange(AContents, $26E9, $26EA);
    AddClassRange(AContents, $26F0, $26F5);
    AddClassRange(AContents, $26F7, $26FA);
    AddClassRange(AContents, $26FD, $26FD);
    AddClassRange(AContents, $2702, $2702);
    AddClassRange(AContents, $2705, $2705);
    AddClassRange(AContents, $2708, $270D);
    AddClassRange(AContents, $270F, $270F);
    AddClassRange(AContents, $2712, $2712);
    AddClassRange(AContents, $2714, $2714);
    AddClassRange(AContents, $2716, $2716);
    AddClassRange(AContents, $271D, $271D);
    AddClassRange(AContents, $2721, $2721);
    AddClassRange(AContents, $2728, $2728);
    AddClassRange(AContents, $2733, $2734);
    AddClassRange(AContents, $2744, $2744);
    AddClassRange(AContents, $2747, $2747);
    AddClassRange(AContents, $274C, $274C);
    AddClassRange(AContents, $274E, $274E);
    AddClassRange(AContents, $2753, $2755);
    AddClassRange(AContents, $2757, $2757);
    AddClassRange(AContents, $2763, $2764);
    AddClassRange(AContents, $2795, $2797);
    AddClassRange(AContents, $27A1, $27A1);
    AddClassRange(AContents, $27B0, $27B0);
    AddClassRange(AContents, $27BF, $27BF);
    AddClassRange(AContents, $2934, $2935);
    AddClassRange(AContents, $2B05, $2B07);
    AddClassRange(AContents, $2B1B, $2B1C);
    AddClassRange(AContents, $2B50, $2B50);
    AddClassRange(AContents, $2B55, $2B55);
    AddClassRange(AContents, $3030, $3030);
    AddClassRange(AContents, $303D, $303D);
    AddClassRange(AContents, $3297, $3297);
    AddClassRange(AContents, $3299, $3299);
    AddClassRange(AContents, $1F004, $1F004);
    AddClassRange(AContents, $1F0CF, $1F0CF);
    AddClassRange(AContents, $1F170, $1F171);
    AddClassRange(AContents, $1F17E, $1F17F);
    AddClassRange(AContents, $1F18E, $1F18E);
    AddClassRange(AContents, $1F191, $1F19A);
    AddClassRange(AContents, $1F1E6, $1F1FF);
    AddClassRange(AContents, $1F201, $1F202);
    AddClassRange(AContents, $1F21A, $1F21A);
    AddClassRange(AContents, $1F22F, $1F22F);
    AddClassRange(AContents, $1F232, $1F23A);
    AddClassRange(AContents, $1F250, $1F251);
    AddClassRange(AContents, $1F300, $1F321);
    AddClassRange(AContents, $1F324, $1F393);
    AddClassRange(AContents, $1F396, $1F397);
    AddClassRange(AContents, $1F399, $1F39B);
    AddClassRange(AContents, $1F39E, $1F3F0);
    AddClassRange(AContents, $1F3F3, $1F3F5);
    AddClassRange(AContents, $1F3F7, $1F4FD);
    AddClassRange(AContents, $1F4FF, $1F53D);
    AddClassRange(AContents, $1F549, $1F54E);
    AddClassRange(AContents, $1F550, $1F567);
    AddClassRange(AContents, $1F56F, $1F570);
    AddClassRange(AContents, $1F573, $1F57A);
    AddClassRange(AContents, $1F587, $1F587);
    AddClassRange(AContents, $1F58A, $1F58D);
    AddClassRange(AContents, $1F590, $1F590);
    AddClassRange(AContents, $1F595, $1F596);
    AddClassRange(AContents, $1F5A4, $1F5A5);
    AddClassRange(AContents, $1F5A8, $1F5A8);
    AddClassRange(AContents, $1F5B1, $1F5B2);
    AddClassRange(AContents, $1F5BC, $1F5BC);
    AddClassRange(AContents, $1F5C2, $1F5C4);
    AddClassRange(AContents, $1F5D1, $1F5D3);
    AddClassRange(AContents, $1F5DC, $1F5DE);
    AddClassRange(AContents, $1F5E1, $1F5E1);
    AddClassRange(AContents, $1F5E3, $1F5E3);
    AddClassRange(AContents, $1F5E8, $1F5E8);
    AddClassRange(AContents, $1F5EF, $1F5EF);
    AddClassRange(AContents, $1F5F3, $1F5F3);
    AddClassRange(AContents, $1F5FA, $1F64F);
    AddClassRange(AContents, $1F680, $1F6C5);
    AddClassRange(AContents, $1F6CB, $1F6D2);
    AddClassRange(AContents, $1F6D5, $1F6D7);
    AddClassRange(AContents, $1F6DC, $1F6E5);
    AddClassRange(AContents, $1F6E9, $1F6E9);
    AddClassRange(AContents, $1F6EB, $1F6EC);
    AddClassRange(AContents, $1F6F0, $1F6F0);
    AddClassRange(AContents, $1F6F3, $1F6FC);
    AddClassRange(AContents, $1F7E0, $1F7EB);
    AddClassRange(AContents, $1F7F0, $1F7F0);
    AddClassRange(AContents, $1F90C, $1F93A);
    AddClassRange(AContents, $1F93C, $1F945);
    AddClassRange(AContents, $1F947, $1F9FF);
    AddClassRange(AContents, $1FA00, $1FA53);
    AddClassRange(AContents, $1FA60, $1FA6D);
    AddClassRange(AContents, $1FA70, $1FA7C);
    AddClassRange(AContents, $1FA80, $1FA89);
    AddClassRange(AContents, $1FA8F, $1FAC6);
    AddClassRange(AContents, $1FACE, $1FADC);
    AddClassRange(AContents, $1FADF, $1FAE9);
    AddClassRange(AContents, $1FAF0, $1FAF8);
    AddClassRange(AContents, $A9, $A9);
    AddClassRange(AContents, $AE, $AE);
    AddClassRange(AContents, $203C, $203C);
    AddClassRange(AContents, $2049, $2049);
    AddClassRange(AContents, $2122, $2122);
    AddClassRange(AContents, $2139, $2139);
    AddClassRange(AContents, $2194, $2199);
    AddClassRange(AContents, $21A9, $21AA);
    AddClassRange(AContents, $24C2, $24C2);
    AddClassRange(AContents, $FE0F, $FE0F);
    AddClassRange(AContents, $1F202, $1F202);
    AddClassRange(AContents, $1F237, $1F237);
    AddClassString(AContents, [$23, $FE0F, $20E3]);
    AddClassString(AContents, [$2A, $FE0F, $20E3]);
    AddClassString(AContents, [$30, $FE0F, $20E3]);
    AddClassString(AContents, [$31, $FE0F, $20E3]);
    AddClassString(AContents, [$32, $FE0F, $20E3]);
    AddClassString(AContents, [$33, $FE0F, $20E3]);
    AddClassString(AContents, [$34, $FE0F, $20E3]);
    AddClassString(AContents, [$35, $FE0F, $20E3]);
    AddClassString(AContents, [$36, $FE0F, $20E3]);
    AddClassString(AContents, [$37, $FE0F, $20E3]);
    AddClassString(AContents, [$38, $FE0F, $20E3]);
    AddClassString(AContents, [$39, $FE0F, $20E3]);
  end
  else if APropertyName = 'Emoji_Keycap_Sequence' then
  begin
    AddClassString(AContents, [$23, $FE0F, $20E3]);
    AddClassString(AContents, [$2A, $FE0F, $20E3]);
    AddClassString(AContents, [$30, $FE0F, $20E3]);
    AddClassString(AContents, [$31, $FE0F, $20E3]);
    AddClassString(AContents, [$32, $FE0F, $20E3]);
    AddClassString(AContents, [$33, $FE0F, $20E3]);
    AddClassString(AContents, [$34, $FE0F, $20E3]);
    AddClassString(AContents, [$35, $FE0F, $20E3]);
    AddClassString(AContents, [$36, $FE0F, $20E3]);
    AddClassString(AContents, [$37, $FE0F, $20E3]);
    AddClassString(AContents, [$38, $FE0F, $20E3]);
    AddClassString(AContents, [$39, $FE0F, $20E3]);
  end
  else if APropertyName = 'RGI_Emoji_Flag_Sequence' then
  begin
    AddClassString(AContents, [$1F1FA, $1F1F8]);
    AddClassString(AContents, [$1F1EC, $1F1E7]);
    AddClassString(AContents, [$1F1EB, $1F1F7]);
    AddClassString(AContents, [$1F1E9, $1F1EA]);
    AddClassString(AContents, [$1F1EF, $1F1F5]);
    AddClassString(AContents, [$1F1E8, $1F1F3]);
    AddClassString(AContents, [$1F1EE, $1F1F3]);
    AddClassString(AContents, [$1F1E7, $1F1F7]);
    AddClassString(AContents, [$1F1F7, $1F1FA]);
    AddClassString(AContents, [$1F1E8, $1F1E6]);
    AddClassString(AContents, [$1F1E6, $1F1FA]);
    AddClassString(AContents, [$1F1EE, $1F1F9]);
    AddClassString(AContents, [$1F1EA, $1F1F8]);
    AddClassString(AContents, [$1F1F2, $1F1FD]);
    AddClassString(AContents, [$1F1F0, $1F1F7]);
  end
  else if APropertyName = 'RGI_Emoji_Modifier_Sequence' then
  begin
    AddClassString(AContents, [$261D, $1F3FB]);
    AddClassString(AContents, [$261D, $1F3FC]);
    AddClassString(AContents, [$261D, $1F3FD]);
    AddClassString(AContents, [$261D, $1F3FE]);
    AddClassString(AContents, [$261D, $1F3FF]);
    AddClassString(AContents, [$270C, $1F3FB]);
    AddClassString(AContents, [$270C, $1F3FC]);
    AddClassString(AContents, [$270C, $1F3FD]);
    AddClassString(AContents, [$270C, $1F3FE]);
    AddClassString(AContents, [$270C, $1F3FF]);
    AddClassString(AContents, [$1F44D, $1F3FB]);
    AddClassString(AContents, [$1F44D, $1F3FC]);
    AddClassString(AContents, [$1F44D, $1F3FD]);
    AddClassString(AContents, [$1F44D, $1F3FE]);
    AddClassString(AContents, [$1F44D, $1F3FF]);
  end
  else if APropertyName = 'RGI_Emoji_Tag_Sequence' then
  begin
    AddClassString(AContents, [$1F3F4, $E0067, $E0062, $E0065, $E006E, $E0067, $E007F]);
    AddClassString(AContents, [$1F3F4, $E0067, $E0062, $E0073, $E0063, $E0074, $E007F]);
    AddClassString(AContents, [$1F3F4, $E0067, $E0062, $E0077, $E006C, $E0073, $E007F]);
  end
  else if APropertyName = 'RGI_Emoji_ZWJ_Sequence' then
  begin
    AddClassString(AContents, [$1F468, $200D, $1F469, $200D, $1F466]);
    AddClassString(AContents, [$1F468, $200D, $1F469, $200D, $1F467]);
    AddClassString(AContents, [$1F468, $200D, $2764, $FE0F, $200D, $1F468]);
    AddClassString(AContents, [$1F469, $200D, $2764, $FE0F, $200D, $1F469]);
    AddClassString(AContents, [$1F469, $200D, $2764, $FE0F, $200D, $1F48B, $200D, $1F468]);
    AddClassString(AContents, [$1F468, $200D, $1F4BB]);
    AddClassString(AContents, [$1F469, $200D, $1F4BB]);
    AddClassString(AContents, [$1F468, $200D, $2695, $FE0F]);
    AddClassString(AContents, [$1F469, $200D, $2695, $FE0F]);
    AddClassString(AContents, [$1F468, $200D, $1F3EB]);
    AddClassString(AContents, [$1F469, $200D, $1F3EB]);
    AddClassString(AContents, [$1F468, $200D, $1F527]);
    AddClassString(AContents, [$1F469, $200D, $1F527]);
    AddClassString(AContents, [$1F468, $200D, $1F373]);
    AddClassString(AContents, [$1F469, $200D, $1F373]);
  end
  else
    raise EConvertError.Create('Invalid Unicode property of strings: ' + APropertyName);
end;

procedure TRegExpCompiler.ParseClassEscape(var AContents: TRegExpClassContents);
var
  C: Char;
  PropertyName: string;
  CodePoint: Cardinal;
  NegContents: TRegExpClassContents;
begin
  C := Advance;
  case C of
    'd', 'D', 'w', 'W', 's', 'S':
      begin
        AddBuiltinCharClass(C, AContents.Ranges, AContents.RangeCount);
      end;
    'n': AddClassRange(AContents, $0A, $0A);
    'r': AddClassRange(AContents, $0D, $0D);
    't': AddClassRange(AContents, $09, $09);
    'v': AddClassRange(AContents, $0B, $0B);
    'f': AddClassRange(AContents, $0C, $0C);
    '0':
      begin
        if not AtEnd and (Peek >= '0') and (Peek <= '9') then
          AddClassRange(AContents, Ord(C), Ord(C))
        else
          AddClassRange(AContents, 0, 0);
      end;
    'x':
      begin
        CodePoint := ParseHexEscape(2);
        AddClassRange(AContents, CodePoint, CodePoint);
      end;
    'u':
      begin
        CodePoint := ParseUnicodeEscape;
        AddClassRange(AContents, CodePoint, CodePoint);
      end;
    'p', 'P':
      begin
        if Match('{') then
        begin
          PropertyName := '';
          while not AtEnd and (Peek <> '}') do
            PropertyName := PropertyName + Advance;
          if not Match('}') then
            raise EConvertError.Create('Unterminated Unicode property escape');
          if IsStringProperty(PropertyName) then
          begin
            if C = 'P' then
              raise EConvertError.Create(
                'Negated \\P{} cannot be used with properties of strings');
            GetStringPropertySequences(PropertyName, AContents);
          end
          else if C = 'P' then
          begin
            InitClassContents(NegContents);
            GetUnicodePropertyRanges(PropertyName, NegContents.Ranges,
              NegContents.RangeCount);
            NormalizeRanges(NegContents.Ranges, NegContents.RangeCount);
            ComplementContents(NegContents);
            MergeClassContents(AContents, NegContents);
          end
          else
            GetUnicodePropertyRanges(PropertyName, AContents.Ranges,
              AContents.RangeCount);
        end
        else
          raise EConvertError.Create(
            'Invalid regular expression: invalid escape in unicode mode');
      end;
    'q':
      begin
        if FUnicodeSets and Match('{') then
          ParseStringDisjunction(AContents)
        else
          raise EConvertError.Create(
            'Invalid regular expression: invalid escape in unicode mode');
      end;
    'b':
      AddClassRange(AContents, $08, $08);
    'c':
      begin
        if not AtEnd and (((Peek >= 'a') and (Peek <= 'z')) or
           ((Peek >= 'A') and (Peek <= 'Z'))) then
          AddClassRange(AContents, Ord(Advance) mod 32, Ord(FPattern[FPos - 1]) mod 32)
        else
          raise EConvertError.Create(
            'Invalid regular expression: invalid control escape in unicode mode');
      end;
  else
    if not CharInSet(C, ['/', '^', '$', '\', '.', '*', '+',
       '?', '(', ')', '[', ']', '{', '}', '|', '-', '&', '!', '#',
       '%', ',', ':', ';', '<', '=', '>', '@', '`', '~']) then
      raise EConvertError.Create(
        'Invalid regular expression: invalid escape in unicode mode')
    else
      AddClassRange(AContents, Ord(C), Ord(C));
  end;
end;

procedure TRegExpCompiler.ParseStringDisjunction(
  var AContents: TRegExpClassContents);
var
  CodePoints: array of Cardinal;
  CodePointCount: Integer;
  CodePoint: Cardinal;
begin
  CodePointCount := 0;
  SetLength(CodePoints, 16);
  while not AtEnd and (Peek <> '}') do
  begin
    if Peek = '|' then
    begin
      if CodePointCount = 1 then
        AddClassRange(AContents, CodePoints[0], CodePoints[0])
      else if CodePointCount > 0 then
      begin
        SetLength(CodePoints, CodePointCount);
        AddClassString(AContents, CodePoints);
        SetLength(CodePoints, 16);
      end;
      CodePointCount := 0;
      Inc(FPos);
      Continue;
    end;
    if Peek = '\' then
    begin
      Inc(FPos);
      case Peek of
        'u':
          begin
            Inc(FPos);
            CodePoint := ParseUnicodeEscape;
          end;
        'n': begin Inc(FPos); CodePoint := $0A; end;
        'r': begin Inc(FPos); CodePoint := $0D; end;
        't': begin Inc(FPos); CodePoint := $09; end;
      else
        CodePoint := Ord(Advance);
      end;
    end
    else
      CodePoint := ReadCodePoint;
    if CodePointCount >= Length(CodePoints) then
      SetLength(CodePoints, CodePointCount * 2);
    CodePoints[CodePointCount] := CodePoint;
    Inc(CodePointCount);
  end;
  if not Match('}') then
    raise EConvertError.Create('Unterminated string disjunction \\q{}');
  if CodePointCount = 1 then
    AddClassRange(AContents, CodePoints[0], CodePoints[0])
  else if CodePointCount > 0 then
  begin
    SetLength(CodePoints, CodePointCount);
    AddClassString(AContents, CodePoints);
  end;
end;

procedure TRegExpCompiler.ApplyNestedSetOps(
  var AContents: TRegExpClassContents);
var
  RightContents: TRegExpClassContents;
  HaveIntersect, HaveSubtract: Boolean;
begin
  HaveIntersect := False;
  HaveSubtract := False;
  while not AtEnd and (Peek <> ']') do
  begin
    if (Peek = '&') and (PeekAt(1) = '&') then
    begin
      if HaveSubtract then
        raise EConvertError.Create(
          'Invalid regular expression: cannot mix && and -- in the same class');
      HaveIntersect := True;
      Inc(FPos, 2);
      InitClassContents(RightContents);
      ParseClassUnion(RightContents);
      IntersectClassContents(AContents, RightContents);
    end
    else if (Peek = '-') and (PeekAt(1) = '-') then
    begin
      if HaveIntersect then
        raise EConvertError.Create(
          'Invalid regular expression: cannot mix && and -- in the same class');
      HaveSubtract := True;
      Inc(FPos, 2);
      InitClassContents(RightContents);
      ParseClassUnion(RightContents);
      SubtractClassContents(AContents, RightContents);
    end
    else
      Break;
  end;
end;

procedure TRegExpCompiler.ParseClassUnion(var AContents: TRegExpClassContents);
var
  Lo, Hi: Cardinal;
  SavePos, SaveStringPos: Integer;
  IsClassEscape: Boolean;
  EscContents: TRegExpClassContents;
begin
  while not AtEnd and (Peek <> ']') do
  begin
    if (Peek = '&') and (PeekAt(1) = '&') then
      Break;
    if (Peek = '-') and (PeekAt(1) = '-') then
      Break;
    if Peek = '[' then
    begin
      Inc(FPos);
      InitClassContents(EscContents);
      if Match('^') then
      begin
        ParseClassUnion(EscContents);
        ApplyNestedSetOps(EscContents);
        if not Match(']') then
          raise EConvertError.Create('Unterminated character class');
        if EscContents.StringCount > 0 then
          raise EConvertError.Create(
            'Negated character class cannot contain strings');
        NormalizeRanges(EscContents.Ranges, EscContents.RangeCount);
        ComplementContents(EscContents);
        MergeClassContents(AContents, EscContents);
      end
      else
      begin
        ParseClassUnion(EscContents);
        ApplyNestedSetOps(EscContents);
        if not Match(']') then
          raise EConvertError.Create('Unterminated character class');
        MergeClassContents(AContents, EscContents);
      end;
      Continue;
    end;
    if Peek = '\' then
    begin
      Inc(FPos);
      IsClassEscape := FUnicodeSets and CharInSet(Peek,
        ['d', 'D', 'w', 'W', 's', 'S', 'p', 'P', 'q']);
      SavePos := AContents.RangeCount;
      SaveStringPos := AContents.StringCount;
      ParseClassEscape(AContents);
      if (AContents.StringCount > SaveStringPos) and
         (AContents.Strings[AContents.StringCount - 1].CodePoints <> nil) then
        Continue;
      if (not AtEnd) and (Peek = '-') and (PeekAt(1) <> ']') and
         (PeekAt(1) <> '-') then
      begin
        if IsClassEscape then
          raise EConvertError.Create(
            'Invalid regular expression: character class escape in range');
        if AContents.RangeCount > SavePos then
        begin
          Lo := AContents.Ranges[AContents.RangeCount - 1].Lo;
          Dec(AContents.RangeCount);
          Inc(FPos);
          if Peek = '\' then
          begin
            Inc(FPos);
            if FUnicodeSets and CharInSet(Peek,
               ['d', 'D', 'w', 'W', 's', 'S', 'p', 'P', 'q']) then
              raise EConvertError.Create(
                'Invalid regular expression: character class escape in range');
            InitClassContents(EscContents);
            ParseClassEscape(EscContents);
            if EscContents.RangeCount > 0 then
            begin
              Hi := EscContents.Ranges[0].Lo;
              if Lo > Hi then
                raise EConvertError.Create(
                  'Invalid regular expression: range out of order in character class');
              AddClassRange(AContents, Lo, Hi);
            end;
          end
          else
          begin
            Hi := ReadCodePoint;
            if Lo > Hi then
              raise EConvertError.Create(
                'Invalid regular expression: range out of order in character class');
            AddClassRange(AContents, Lo, Hi);
          end;
        end;
      end;
      Continue;
    end;
    if FUnicodeSets and CharInSet(Peek, ['(', ')', '/', '|']) then
      raise EConvertError.Create(
        'Invalid regular expression: unescaped ' + Peek + ' in unicode sets mode character class');
    Lo := ReadCodePoint;
    if (not AtEnd) and (Peek = '-') and (PeekAt(1) <> ']') and
       (PeekAt(1) <> '-') then
    begin
      Inc(FPos);
      if Peek = '\' then
      begin
        Inc(FPos);
        InitClassContents(EscContents);
        ParseClassEscape(EscContents);
        if EscContents.RangeCount > 0 then
        begin
          Hi := EscContents.Ranges[0].Lo;
          if Lo > Hi then
            raise EConvertError.Create(
              'Invalid regular expression: range out of order in character class');
          AddClassRange(AContents, Lo, Hi);
        end;
      end
      else
      begin
        Hi := ReadCodePoint;
        if Lo > Hi then
          raise EConvertError.Create(
            'Invalid regular expression: range out of order in character class');
        AddClassRange(AContents, Lo, Hi);
      end;
    end
    else
      AddClassRange(AContents, Lo, Lo);
  end;
end;

procedure TRegExpCompiler.EmitClassContents(
  const AContents: TRegExpClassContents; ANegated: Boolean);
var
  I, J: Integer;
  SplitHoles: array of Integer;
  SplitCount: Integer;
  JumpHoles: array of Integer;
  JumpCount: Integer;
begin
  if (AContents.StringCount > 128) and (not ANegated) and
     (not FBackward) then
  begin
    Emit(EncodeOpBx(RX_STRING_SET, AddStringSet(AContents)));
    Exit;
  end;

  if (AContents.StringCount = 0) or ANegated then
  begin
    EmitCharClassRanges(AContents.Ranges, AContents.RangeCount, ANegated);
    Exit;
  end;
  SplitCount := 0;
  JumpCount := 0;
  SetLength(SplitHoles, AContents.StringCount + 1);
  SetLength(JumpHoles, AContents.StringCount + 1);
  for I := AContents.StringCount - 1 downto 0 do
  begin
    if Length(AContents.Strings[I].CodePoints) > 1 then
    begin
      SplitHoles[SplitCount] := CurrentPC;
      Inc(SplitCount);
      Emit(EncodeOpBx(RX_SPLIT, 0));
      if FBackward then
      begin
        for J := High(AContents.Strings[I].CodePoints) downto 0 do
          EmitCharMatch(AContents.Strings[I].CodePoints[J]);
      end
      else
      begin
        for J := 0 to High(AContents.Strings[I].CodePoints) do
          EmitCharMatch(AContents.Strings[I].CodePoints[J]);
      end;
      JumpHoles[JumpCount] := CurrentPC;
      Inc(JumpCount);
      Emit(EncodeOpBx(RX_JUMP, 0));
      PatchHole(SplitHoles[SplitCount - 1], CurrentPC);
    end;
  end;
  if AContents.RangeCount > 0 then
    EmitCharClassRanges(AContents.Ranges, AContents.RangeCount, False)
  else
    Emit(EncodeOp(RX_FAIL));
  for I := 0 to JumpCount - 1 do
    FCode[JumpHoles[I]] := EncodeOpBx(RX_JUMP, CurrentPC);
end;

procedure TRegExpCompiler.CompileUnicodeSetsClass;
var
  Contents: TRegExpClassContents;
  Negated: Boolean;
begin
  Negated := Match('^');
  InitClassContents(Contents);
  ParseClassUnion(Contents);
  ApplyNestedSetOps(Contents);
  if not Match(']') then
    raise EConvertError.Create('Unterminated character class');
  if Negated and (Contents.StringCount > 0) then
    raise EConvertError.Create(
      'Negated character class cannot contain strings');
  EmitClassContents(Contents, Negated);
end;

procedure TRegExpCompiler.EmitCharClassRanges(
  const ARanges: array of TRegExpCharRange;
  ARangeCount: Integer; ANegated: Boolean);
var
  I: Integer;
  DynRanges: TRegExpCharRangeArray;
  FoldRanges: TUnicodePropertyRangeArray;
begin
  SetLength(DynRanges, ARangeCount);
  for I := 0 to ARangeCount - 1 do
    DynRanges[I] := ARanges[I];
  if FModifier.IgnoreCase then
  begin
    FoldRanges := CharRangesToUnicodeRanges(DynRanges, Length(DynRanges));

    if FUnicode then
      ExpandUnicodeSimpleCaseFolding(FoldRanges)
    else
      ExpandRegExpNonUnicodeCaseFolding(FoldRanges);

    DynRanges := UnicodeRangesToCharRanges(FoldRanges);
  end;
  EmitRawCharClassRanges(DynRanges, ANegated);
end;

procedure TRegExpCompiler.EmitRawCharClassRanges(
  const ARanges: array of TRegExpCharRange; ANegated: Boolean);
var
  ClassIdx: Integer;
  Op: TRegExpOpCode;
begin
  ClassIdx := AddCharClass(ARanges);
  if ANegated then
    Op := RX_CHAR_CLASS_NEG
  else
    Op := RX_CHAR_CLASS;
  Emit(EncodeOpBx(Op, ClassIdx));
end;

function TRegExpCompiler.ReadCodePoint: Cardinal;
var
  ByteLen: Integer;
  CodePoint: Cardinal;
  Supplementary: Cardinal;
begin
  if FPendingCodeUnit >= 0 then
  begin
    Result := Cardinal(FPendingCodeUnit);
    FPendingCodeUnit := -1;
    Exit;
  end;

  if FPos <= Length(FPattern) then
  begin
    if TryReadUTF8CodePointAllowSurrogates(FPattern, FPos, CodePoint,
       ByteLen) and (ByteLen > 1) then
    begin
      Inc(FPos, ByteLen);
      if (not FUnicode) and (CodePoint > $FFFF) then
      begin
        Supplementary := CodePoint - $10000;
        FPendingCodeUnit := Integer($DC00 + (Supplementary and $3FF));
        Result := $D800 + (Supplementary shr 10);
        Exit;
      end;
      Result := CodePoint;
      Exit;
    end;
  end;
  Result := Ord(Advance);
end;

function TRegExpCompiler.ParseGroupName: string;
var
  C: Char;
  RawName: string;
begin
  RawName := '';
  while not AtEnd do
  begin
    C := Peek;
    if C = '>' then
    begin
      Inc(FPos);
      Result := DecodeRegExpGroupName(RawName);
      Exit;
    end;
    RawName := RawName + Advance;
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
  Digit: Cardinal;
  HighSurrogate: Cardinal;
  HasDigit: Boolean;
begin
  if Match('{') then
  begin
    Result := 0;
    HasDigit := False;
    while not AtEnd and (Peek <> '}') do
    begin
      case Peek of
        '0'..'9':
          Digit := Cardinal(Ord(Advance) - Ord('0'));
        'a'..'f':
          Digit := Cardinal(Ord(Advance) - Ord('a') + 10);
        'A'..'F':
          Digit := Cardinal(Ord(Advance) - Ord('A') + 10);
      else
        raise EConvertError.Create('Invalid Unicode escape');
      end;
      HasDigit := True;
      if Result > (($10FFFF - Digit) div 16) then
        raise EConvertError.Create('Unicode escape out of range');
      Result := Result * 16 + Digit;
    end;
    if not HasDigit then
      raise EConvertError.Create('Invalid Unicode escape');
    if not Match('}') then
      raise EConvertError.Create('Unterminated Unicode escape');
    if Result > $10FFFF then
      raise EConvertError.Create('Unicode escape out of range');
    Exit;
  end;
  Result := ParseHexEscape(4);
  if FUnicode and (Result >= $D800) and (Result <= $DBFF) then
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
  ABackrefFlags: Integer);
var
  Indices: array of Integer;
  Count, I: Integer;
  UndefinedJumpHole: Integer;
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
    Emit(EncodeOpBx(RX_BACKREF, Indices[0] or ABackrefFlags));
    Exit;
  end;
  JumpCount := 0;
  SetLength(JumpHoles, Count + 1);
  for I := 0 to Count - 1 do
  begin
    Emit(EncodeOpBx(RX_CAPTURE_UNDEFINED_JUMP, Indices[I]));
    UndefinedJumpHole := CurrentPC;
    Emit(0);
    Emit(EncodeOpBx(RX_BACKREF, Indices[I] or ABackrefFlags));
    JumpHoles[JumpCount] := CurrentPC;
    Inc(JumpCount);
    Emit(0);
    FCode[UndefinedJumpHole] := EncodeOpBx(RX_JUMP, CurrentPC);
  end;
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
  Contents: TRegExpClassContents;
  BackrefIdx, I, GroupCount, BackrefFlags, WordAssertFlags: Integer;
  CodePoint: Cardinal;
begin
  BackrefFlags := 0;
  if FModifier.IgnoreCase then
    BackrefFlags := BackrefFlags or BACKREF_ICASE_FLAG;
  if FUnicode then
    BackrefFlags := BackrefFlags or BACKREF_UNICODE_FLAG;
  C := Advance;
  case C of
    'd', 'D', 'w', 'W', 's', 'S':
      begin
        RangeCount := 0;
        AddBuiltinCharClass(C, Ranges, RangeCount);
        EmitCharClassRanges(Ranges, RangeCount, False);
      end;
    'b':
      begin
        WordAssertFlags := 0;
        if FModifier.IgnoreCase then
          WordAssertFlags := WordAssertFlags or WORD_ASSERT_ICASE_FLAG;
        Emit(EncodeOpBx(RX_ASSERT_WORD, WordAssertFlags));
      end;
    'B':
      begin
        WordAssertFlags := WORD_ASSERT_NEGATED_FLAG;
        if FModifier.IgnoreCase then
          WordAssertFlags := WordAssertFlags or WORD_ASSERT_ICASE_FLAG;
        Emit(EncodeOpBx(RX_ASSERT_WORD, WordAssertFlags));
      end;
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
          if FUnicodeSets and IsStringProperty(PropertyName) then
          begin
            if Negated then
              raise EConvertError.Create(
                'Negated \\P{} cannot be used with properties of strings');
            InitClassContents(Contents);
            GetStringPropertySequences(PropertyName, Contents);
            EmitClassContents(Contents, False);
          end
          else
            EmitUnicodePropertyClass(PropertyName, Negated);
        end
        else if FUnicode then
          raise EConvertError.Create(
            'Invalid regular expression: invalid escape in unicode mode')
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
          begin
            if FUnicode or (Length(FNamedGroups) > 0) then
              raise EConvertError.Create(
                'Invalid named backreference: ' + GroupName);
            EmitCharMatch(Ord('k'));
            EmitCharMatch(Ord('<'));
            I := 1;
            while I <= Length(GroupName) do
            begin
              CodePoint := ReadRegExpGroupNameLiteralCodePoint(GroupName, I);
              EmitCharMatch(CodePoint);
            end;
            EmitCharMatch(Ord('>'));
            Exit;
          end;
          if GroupCount <= 1 then
            Emit(EncodeOpBx(RX_BACKREF, BackrefIdx or BackrefFlags))
          else
            EmitDuplicateNamedBackref(GroupName, BackrefFlags);
        end
        else if FUnicode then
          raise EConvertError.Create(
            'Invalid regular expression: invalid escape in unicode mode')
        else
          EmitCharMatch(Ord('k'));
      end;
    '1'..'9':
      begin
        BackrefIdx := Ord(C) - Ord('0');
        while not AtEnd and (Peek >= '0') and (Peek <= '9') do
          BackrefIdx := BackrefIdx * 10 + (Ord(Advance) - Ord('0'));
        if FUnicode and (BackrefIdx > FPreScanCaptureCount) then
          raise EConvertError.Create(
            'Invalid regular expression: invalid decimal escape in unicode mode');
        Emit(EncodeOpBx(RX_BACKREF, BackrefIdx or BackrefFlags));
      end;
    'n': EmitCharMatch($0A);
    'r': EmitCharMatch($0D);
    't': EmitCharMatch($09);
    'v': EmitCharMatch($0B);
    'f': EmitCharMatch($0C);
    '0':
      begin
        if FUnicode and not AtEnd and (Peek >= '0') and (Peek <= '9') then
          raise EConvertError.Create(
            'Invalid regular expression: invalid decimal escape in unicode mode');
        if not AtEnd and (Peek >= '0') and (Peek <= '9') then
          EmitCharMatch(Ord(C))
        else
          EmitCharMatch(0);
      end;
    'x':
      begin
        if FUnicode then
          EmitCharMatch(ParseHexEscape(2))
        else
        begin
          I := FPos;
          try
            EmitCharMatch(ParseHexEscape(2));
          except
            on E: EConvertError do
            begin
              FPos := I;
              EmitCharMatch(Ord('x'));
            end;
          end;
        end;
      end;
    'u':
      begin
        if (not FUnicode) and (Peek = '{') then
          EmitCharMatch(Ord('u'))
        else
          EmitCharMatch(ParseUnicodeEscape);
      end;
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
  AllRange: array[0..0] of TRegExpCharRange;
  PropertyRanges: array[0..MAX_CHAR_RANGES - 1] of TRegExpCharRange;
  ComplementRanges: array[0..MAX_CHAR_RANGES - 1] of TRegExpCharRange;
  PropertyRangeCount, ComplementRangeCount, I: Integer;
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
        if FUnicode and not AtEnd and (Peek >= '0') and (Peek <= '9') then
          raise EConvertError.Create(
            'Invalid regular expression: invalid decimal escape in unicode mode');
        if not AtEnd and (Peek >= '0') and (Peek <= '9') then
          AddRange(ARanges, ARangeCount, Ord(C), Ord(C))
        else
          AddRange(ARanges, ARangeCount, 0, 0);
      end;
    'x':
      begin
        if FUnicode then
          CodePoint := ParseHexEscape(2)
        else
        begin
          I := FPos;
          try
            CodePoint := ParseHexEscape(2);
          except
            on E: EConvertError do
            begin
              FPos := I;
              CodePoint := Ord('x');
            end;
          end;
        end;
        AddRange(ARanges, ARangeCount, CodePoint, CodePoint);
      end;
    'u':
      begin
        if (not FUnicode) and (Peek = '{') then
          AddRange(ARanges, ARangeCount, Ord('u'), Ord('u'))
        else
        begin
          CodePoint := ParseUnicodeEscape;
          AddRange(ARanges, ARangeCount, CodePoint, CodePoint);
        end;
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
          begin
            PropertyRangeCount := 0;
            GetUnicodePropertyRanges(PropertyName, PropertyRanges,
              PropertyRangeCount);
            AllRange[0].Lo := 0;
            AllRange[0].Hi := $10FFFF;
            SubtractRanges(AllRange, 1, PropertyRanges, PropertyRangeCount,
              ComplementRanges, ComplementRangeCount);
            for I := 0 to ComplementRangeCount - 1 do
              AddRange(ARanges, ARangeCount, ComplementRanges[I].Lo,
                ComplementRanges[I].Hi);
          end
          else
            GetUnicodePropertyRanges(PropertyName, ARanges, ARangeCount);
        end
        else if FUnicode then
          raise EConvertError.Create(
            'Invalid regular expression: invalid escape in unicode mode')
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
  IsClassEscape: Boolean;
begin
  Negated := Match('^');
  RangeCount := 0;
  while not AtEnd and (Peek <> ']') do
  begin
    if Peek = '\' then
    begin
      Inc(FPos);
      IsClassEscape := FUnicode and CharInSet(Peek,
        ['d', 'D', 'w', 'W', 's', 'S', 'p', 'P']);
      SavePos := RangeCount;
      CompileEscape(Ranges, RangeCount);
      if (not AtEnd) and (Peek = '-') and (PeekAt(1) <> ']') then
      begin
        if IsClassEscape then
          raise EConvertError.Create(
            'Invalid regular expression: character class escape in range');
        if RangeCount > SavePos then
        begin
          Lo := Ranges[RangeCount - 1].Lo;
          Dec(RangeCount);
          Inc(FPos);
          if Peek = '\' then
          begin
            Inc(FPos);
            if FUnicode and CharInSet(Peek,
               ['d', 'D', 'w', 'W', 's', 'S', 'p', 'P']) then
              raise EConvertError.Create(
                'Invalid regular expression: character class escape in range');
            SavePos := RangeCount;
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
        end;
      end;
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
        if FUnicode and CharInSet(Peek,
           ['d', 'D', 'w', 'W', 's', 'S', 'p', 'P']) then
          raise EConvertError.Create(
            'Invalid regular expression: character class escape in range');
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
  SavedBackward: Boolean;
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
      SavedBackward := FBackward;
      FBackward := False;
      LookStart := CurrentPC;
      CompileDisjunction;
      FBackward := SavedBackward;
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
      SavedBackward := FBackward;
      FBackward := False;
      CompileDisjunction;
      FBackward := SavedBackward;
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
        SavedBackward := FBackward;
        FBackward := True;
        CompileDisjunction;
        FBackward := SavedBackward;
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
        SavedBackward := FBackward;
        FBackward := True;
        CompileDisjunction;
        FBackward := SavedBackward;
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
        // Backward capture groups still store source-order [start, end] slots.
        if FBackward then
          Emit(EncodeOpBx(RX_SAVE, CaptureIdx * 2 + 1))
        else
          Emit(EncodeOpBx(RX_SAVE, CaptureIdx * 2));
        CompileDisjunction;
        if not Match(')') then
          raise EConvertError.Create('Unterminated named capture group');
        if FBackward then
          Emit(EncodeOpBx(RX_SAVE, CaptureIdx * 2))
        else
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
    // Backward capture groups still store source-order [start, end] slots.
    if FBackward then
      Emit(EncodeOpBx(RX_SAVE, CaptureIdx * 2 + 1))
    else
      Emit(EncodeOpBx(RX_SAVE, CaptureIdx * 2));
    CompileDisjunction;
    if not Match(')') then
      raise EConvertError.Create('Unterminated capturing group');
    if FBackward then
      Emit(EncodeOpBx(RX_SAVE, CaptureIdx * 2))
    else
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
        if FUnicodeSets then
          CompileUnicodeSetsClass
        else
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
      if FUnicode and CharInSet(C, [']', '}']) then
        raise EConvertError.Create('Invalid regular expression: unexpected token');
      CodePoint := ReadCodePoint;
      EmitCharMatch(CodePoint);
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
  if ALen <= 0 then
    Exit;

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
  ClearOperand: Integer;
  HasCaptureClear: Boolean;
  NeedsRepeatGuard: Boolean;

  function BodyRequiresProgress: Boolean;
  var
    Op: TRegExpOpCode;
  begin
    if BodyLen <> 1 then
      Exit(False);
    Op := TRegExpOpCode(BodyCode[0] and $FF);
    Result := Op in [RX_CHAR, RX_CHAR_CLASS, RX_CHAR_CLASS_NEG, RX_ANY];
  end;

  function TryBuildCaptureClearOperand(out AOperand: Integer): Boolean;
  var
    GroupIndex: Integer;
    Instr: UInt32;
    J: Integer;
    MaxGroup: Integer;
    MinGroup: Integer;
    Op: TRegExpOpCode;
    SlotIndex: Integer;
  begin
    MinGroup := High(Integer);
    MaxGroup := -1;
    for J := 0 to BodyLen - 1 do
    begin
      Instr := BodyCode[J];
      Op := TRegExpOpCode(Instr and $FF);
      if Op <> RX_SAVE then
        Continue;
      SlotIndex := Integer(Instr shr 8);
      GroupIndex := SlotIndex div 2;
      if GroupIndex <= 0 then
        Continue;
      if GroupIndex < MinGroup then
        MinGroup := GroupIndex;
      if GroupIndex > MaxGroup then
        MaxGroup := GroupIndex;
    end;

    Result := MaxGroup >= MinGroup;
    if not Result then
      Exit;
    if (MinGroup > CLEAR_CAPTURE_COUNT_MASK) or
       ((MaxGroup - MinGroup + 1) > CLEAR_CAPTURE_COUNT_MASK) then
      raise EConvertError.Create('Too many captures in quantified atom');
    AOperand := (MinGroup shl CLEAR_CAPTURE_COUNT_BITS) or
      (MaxGroup - MinGroup + 1);
  end;

  procedure EmitCaptureClearIfNeeded;
  begin
    if HasCaptureClear then
      Emit(EncodeOpBx(RX_CLEAR_CAPTURES, ClearOperand));
  end;

  procedure EmitOptionalBody;
  begin
    if NeedsRepeatGuard then
      Emit(EncodeOp(RX_REPEAT_ENTER));
    EmitCaptureClearIfNeeded;
    EmitBodyAt(BodyCode, BodyLen, AAtomStart);
    if NeedsRepeatGuard then
      Emit(EncodeOp(RX_REPEAT_CHECK));
  end;
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
  HasCaptureClear := TryBuildCaptureClearOperand(ClearOperand);
  NeedsRepeatGuard := not BodyRequiresProgress;
  FCodeLen := AAtomStart;
  for I := 1 to MinCount do
  begin
    EmitCaptureClearIfNeeded;
    EmitBodyAt(BodyCode, BodyLen, AAtomStart);
  end;
  if MaxCount = -1 then
  begin
    SplitPC := CurrentPC;
    if Lazy then
      Emit(EncodeOpBx(RX_SPLIT_LAZY, 0))
    else
      Emit(EncodeOpBx(RX_SPLIT, 0));
    EmitOptionalBody;
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
      EmitOptionalBody;
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

// ES2026 §22.2.2.3.4 MatchSequence(m1, m2, direction)
procedure TRegExpCompiler.CompileAlternative;
var
  TermStart: Integer;
  TermCount: Integer;
  TermIndex: Integer;
  Terms: array of TRegExpTermCode;
begin
  if not FBackward then
  begin
    while not AtEnd and (Peek <> '|') and (Peek <> ')') do
      CompileTerm;
    Exit;
  end;

  TermCount := 0;
  SetLength(Terms, 8);
  while not AtEnd and (Peek <> '|') and (Peek <> ')') do
  begin
    TermStart := CurrentPC;
    CompileTerm;
    if TermCount >= Length(Terms) then
      SetLength(Terms, TermCount * 2 + 8);
    Terms[TermCount].Length := CurrentPC - TermStart;
    Terms[TermCount].OriginalStart := TermStart;
    SetLength(Terms[TermCount].Code, Terms[TermCount].Length);
    if Terms[TermCount].Length > 0 then
      Move(FCode[TermStart], Terms[TermCount].Code[0],
        Terms[TermCount].Length * SizeOf(UInt32));
    FCodeLen := TermStart;
    Inc(TermCount);
  end;

  for TermIndex := TermCount - 1 downto 0 do
    EmitBodyAt(Terms[TermIndex].Code, Terms[TermIndex].Length,
      Terms[TermIndex].OriginalStart);
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
  if not AtEnd then
    raise EConvertError.Create('Invalid regular expression: unexpected token');
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
            GroupName := DecodeRegExpGroupName(
              Copy(FPattern, I + 3, CloseAngle - I - 3));
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
  FPreScanCaptureCount := GroupIndex;
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
  Result.FullUnicode := FUnicode;
  BuildStartCheck(FCode, FCodeLen, FCharClasses, Result.StartCheck);
  Result.NamedGroups := FNamedGroups;
  Result.StringSets := FStringSets;
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

end.
