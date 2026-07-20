unit UnicodeNormalization;

{$I Shared.inc}

interface

type
  TUnicodeNormalizationForm = (
    unfNFC,
    unfNFD,
    unfNFKC,
    unfNFKD
  );

function NormalizeUnicode(const AText: string;
  const AForm: TUnicodeNormalizationForm): string;

implementation

uses
  Generated.UnicodeNormalizationData,
  StringBuffer;

type
  TUnicodeCodePoints = array of Cardinal;

const
  HANGUL_S_BASE = $AC00;
  HANGUL_L_BASE = $1100;
  HANGUL_V_BASE = $1161;
  HANGUL_T_BASE = $11A7;
  HANGUL_L_COUNT = 19;
  HANGUL_V_COUNT = 21;
  HANGUL_T_COUNT = 28;
  HANGUL_N_COUNT = HANGUL_V_COUNT * HANGUL_T_COUNT;
  HANGUL_S_COUNT = HANGUL_L_COUNT * HANGUL_N_COUNT;

procedure AppendCodePoint(var ACodePoints: TUnicodeCodePoints;
  var ACount: Integer; const ACodePoint: Cardinal);
var
  NewCapacity: Integer;
begin
  if ACount = Length(ACodePoints) then
  begin
    NewCapacity := Length(ACodePoints) * 2;
    if NewCapacity < 16 then
      NewCapacity := 16;
    SetLength(ACodePoints, NewCapacity);
  end;
  ACodePoints[ACount] := ACodePoint;
  Inc(ACount);
end;

function TryReadCodePoint(const AText: string; const AIndex: Integer;
  out ACodePoint: Cardinal; out ALength: Integer): Boolean;
var
  FirstCodeUnit, SecondCodeUnit: Cardinal;
begin
  if (AIndex < 1) or (AIndex > Length(AText)) then
    Exit(False);
  FirstCodeUnit := Ord(AText[AIndex]);
  ACodePoint := FirstCodeUnit;
  ALength := 1;
  if (FirstCodeUnit < $D800) or (FirstCodeUnit > $DBFF) or
     (AIndex = Length(AText)) then
    Exit(True);
  SecondCodeUnit := Ord(AText[AIndex + 1]);
  if (SecondCodeUnit < $DC00) or (SecondCodeUnit > $DFFF) then
    Exit(True);
  ACodePoint := $10000 + ((FirstCodeUnit - $D800) shl 10) +
    (SecondCodeUnit - $DC00);
  ALength := 2;
  Result := True;
end;

procedure DecomposeCodePoint(const ACodePoint: Cardinal;
  const ACompatibility: Boolean; var ACodePoints: TUnicodeCodePoints;
  var ACount: Integer);
var
  HangulIndex, MappingIndex, MappingLength, Offset: Integer;
begin
  HangulIndex := Integer(ACodePoint) - HANGUL_S_BASE;
  if (HangulIndex >= 0) and (HangulIndex < HANGUL_S_COUNT) then
  begin
    AppendCodePoint(ACodePoints, ACount,
      HANGUL_L_BASE + Cardinal(HangulIndex div HANGUL_N_COUNT));
    AppendCodePoint(ACodePoints, ACount,
      HANGUL_V_BASE + Cardinal((HangulIndex mod HANGUL_N_COUNT) div
        HANGUL_T_COUNT));
    Offset := HangulIndex mod HANGUL_T_COUNT;
    if Offset <> 0 then
      AppendCodePoint(ACodePoints, ACount,
        HANGUL_T_BASE + Cardinal(Offset));
    Exit;
  end;

  if TryGetUnicodeDecomposition(ACodePoint, ACompatibility, Offset,
    MappingLength) then
  begin
    for MappingIndex := 0 to MappingLength - 1 do
      DecomposeCodePoint(
        UnicodeDecompositionCodePoint(Offset + MappingIndex),
        ACompatibility, ACodePoints, ACount);
    Exit;
  end;
  AppendCodePoint(ACodePoints, ACount, ACodePoint);
end;

procedure CanonicallyOrder(var ACodePoints: TUnicodeCodePoints;
  const ACount: Integer);
var
  CombiningClass, PreviousClass: Byte;
  CodePoint: Cardinal;
  Index, Position: Integer;
begin
  for Index := 1 to ACount - 1 do
  begin
    CombiningClass := UnicodeCanonicalCombiningClass(ACodePoints[Index]);
    if CombiningClass = 0 then
      Continue;
    CodePoint := ACodePoints[Index];
    Position := Index;
    while Position > 0 do
    begin
      PreviousClass := UnicodeCanonicalCombiningClass(
        ACodePoints[Position - 1]);
      if (PreviousClass = 0) or (PreviousClass <= CombiningClass) then
        Break;
      ACodePoints[Position] := ACodePoints[Position - 1];
      Dec(Position);
    end;
    ACodePoints[Position] := CodePoint;
  end;
end;

function TryComposeHangul(const AFirst, ASecond: Cardinal;
  out AComposite: Cardinal): Boolean;
var
  FirstIndex, LIndex, TIndex, VIndex: Integer;
begin
  LIndex := Integer(AFirst) - HANGUL_L_BASE;
  VIndex := Integer(ASecond) - HANGUL_V_BASE;
  if (LIndex >= 0) and (LIndex < HANGUL_L_COUNT) and
     (VIndex >= 0) and (VIndex < HANGUL_V_COUNT) then
  begin
    AComposite := HANGUL_S_BASE + Cardinal(
      (LIndex * HANGUL_V_COUNT + VIndex) * HANGUL_T_COUNT);
    Exit(True);
  end;

  FirstIndex := Integer(AFirst) - HANGUL_S_BASE;
  TIndex := Integer(ASecond) - HANGUL_T_BASE;
  if (FirstIndex >= 0) and (FirstIndex < HANGUL_S_COUNT) and
     ((FirstIndex mod HANGUL_T_COUNT) = 0) and
     (TIndex > 0) and (TIndex < HANGUL_T_COUNT) then
  begin
    AComposite := AFirst + Cardinal(TIndex);
    Exit(True);
  end;
  AComposite := 0;
  Result := False;
end;

function TryComposePair(const AFirst, ASecond: Cardinal;
  out AComposite: Cardinal): Boolean;
begin
  Result := TryComposeHangul(AFirst, ASecond, AComposite) or
    TryGetUnicodeComposition(AFirst, ASecond, AComposite);
end;

procedure CanonicallyCompose(var ACodePoints: TUnicodeCodePoints;
  var ACount: Integer);
var
  CodePoint, Composite, Starter: Cardinal;
  CombiningClass, LastCombiningClass: Byte;
  HasStarter: Boolean;
  Index, OutputCount, StarterPosition: Integer;
begin
  HasStarter := False;
  LastCombiningClass := 0;
  OutputCount := 0;
  Starter := 0;
  StarterPosition := 0;
  for Index := 0 to ACount - 1 do
  begin
    CodePoint := ACodePoints[Index];
    CombiningClass := UnicodeCanonicalCombiningClass(CodePoint);
    if HasStarter and
       ((LastCombiningClass = 0) or
        (LastCombiningClass < CombiningClass)) and
       TryComposePair(Starter, CodePoint, Composite) then
    begin
      ACodePoints[StarterPosition] := Composite;
      Starter := Composite;
    end
    else
    begin
      ACodePoints[OutputCount] := CodePoint;
      if CombiningClass = 0 then
      begin
        StarterPosition := OutputCount;
        Starter := CodePoint;
        HasStarter := True;
      end;
      LastCombiningClass := CombiningClass;
      Inc(OutputCount);
    end;
  end;
  ACount := OutputCount;
end;

procedure AppendUTF16(var ABuffer: TStringBuffer;
  const ACodePoint: Cardinal);
var
  Supplementary: Cardinal;
begin
  if ACodePoint <= $FFFF then
    ABuffer.AppendChar(Char(ACodePoint))
  else
  begin
    Supplementary := ACodePoint - $10000;
    ABuffer.AppendChar(Char($D800 + (Supplementary shr 10)));
    ABuffer.AppendChar(Char($DC00 + (Supplementary and $3FF)));
  end;
end;

function NormalizeUnicode(const AText: string;
  const AForm: TUnicodeNormalizationForm): string;
var
  Buffer: TStringBuffer;
  CodePoint: Cardinal;
  CodePoints: TUnicodeCodePoints;
  Compatibility, Compose: Boolean;
  CodePointLength, Count, Index: Integer;
begin
  Compatibility := AForm in [unfNFKC, unfNFKD];
  Compose := AForm in [unfNFC, unfNFKC];
  Count := 0;
  Index := 1;
  while Index <= Length(AText) do
  begin
    TryReadCodePoint(AText, Index, CodePoint, CodePointLength);
    DecomposeCodePoint(CodePoint, Compatibility, CodePoints, Count);
    Inc(Index, CodePointLength);
  end;
  CanonicallyOrder(CodePoints, Count);
  if Compose then
    CanonicallyCompose(CodePoints, Count);

  Buffer := TStringBuffer.Create(Length(AText));
  for Index := 0 to Count - 1 do
    AppendUTF16(Buffer, CodePoints[Index]);
  Result := Buffer.ToString;
end;

end.
