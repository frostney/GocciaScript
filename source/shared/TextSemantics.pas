unit TextSemantics;

{$I Shared.inc}

interface

uses
  Classes,
  SysUtils;

type
  TReplacementCapture = record
    Text: string;
    Matched: Boolean;
  end;

  TReplacementNamedCapture = record
    Name: string;
    Text: string;
    Matched: Boolean;
  end;

const
  UNICODE_REPLACEMENT_CODE_POINT = $FFFD;
  UTF8_REPLACEMENT_CHARACTER = #$EF#$BF#$BD;

function RetagUTF8Text(const ABytes: RawByteString): string;
function UTF8SequenceLengthFromLeadByte(const AChar: Char): Integer;
function UTF8SequenceLengthAt(const AText: string;
  const AIndex: Integer): Integer;
function TryReadUTF8CodePoint(const AText: string; const AIndex: Integer;
  out ACodePoint: Cardinal; out AByteLength: Integer): Boolean;
function TryReadUTF8CodePointAllowSurrogates(const AText: string;
  const AIndex: Integer; out ACodePoint: Cardinal;
  out AByteLength: Integer): Boolean;
function CodePointToUTF8(const ACodePoint: Cardinal): string;
function IsUnicodeSurrogateCodePoint(const ACodePoint: Cardinal): Boolean; inline;
function IsECMAScriptWhitespaceCodePoint(const ACodePoint: Cardinal): Boolean;
function TrimECMAScriptWhitespace(const AText: string): string;
function TrimECMAScriptWhitespaceStart(const AText: string): string;
function TrimECMAScriptWhitespaceEnd(const AText: string): string;
function IsWellFormedUTF8(const AText: string): Boolean;
function ToWellFormedUTF8(const AText: string): string;
function UTF8CodePointLength(const AText: string): Integer;
function UTF8CodePointAt(const AText: string; const AIndex: Integer): string;
function UTF16CodeUnitLength(const AText: string): Integer;
function UTF16CodeUnitAt(const AText: string; const AIndex: Integer): string;
function TryUTF16CodePointValueAt(const AText: string; const AIndex: Integer;
  out ACodePoint: Cardinal): Boolean;
function UTF16Substring(const AText: string; const AStart,
  ACount: Integer): string;
function UTF16IndexOf(const AText, ASearch: string;
  const AStart: Integer = 0): Integer;
function UTF16LastIndexOf(const AText, ASearch: string;
  const AStart: Integer): Integer;
function UnicodeLowerCaseUTF8(const AText: string): string;
function UnicodeUpperCaseUTF8(const AText: string): string;
function AdvanceUTF8StringIndex(const AText: string; const AIndex: Integer;
  const AUnicode: Boolean): Integer;
function ExpandReplacementPattern(const AReplacement, AMatched,
  AInput: string; const AMatchStart: Integer;
  const ACaptures: array of TReplacementCapture;
  const ANamedCaptures: array of TReplacementNamedCapture): string;
function CreateUTF8StringList(const AText: string): TStringList;
function CreateECMAScriptSourceLines(const AText: string): TStringList;
function CreateUTF8FileTextLines(const AText: UTF8String): TStringList;
function NormalizeNewlinesToLF(const AText: string): string;
function NormalizeUTF8NewlinesToLF(const AText: UTF8String): UTF8String;
function StringListToLFText(const ALines: TStrings): string;

implementation

uses
  Math,

  fpwidestring,
  StringBuffer;

function RetagUTF8Text(const ABytes: RawByteString): string;
var
  Bytes: RawByteString;
begin
  Bytes := ABytes;
  SetCodePage(Bytes, CP_UTF8, False);
  Result := string(Bytes);
end;

function UTF8SequenceLengthFromLeadByte(const AChar: Char): Integer;
var
  ByteValue: Byte;
begin
  ByteValue := Ord(AChar);
  if ByteValue < $80 then
    Exit(1);
  if (ByteValue and $E0) = $C0 then
    Exit(2);
  if (ByteValue and $F0) = $E0 then
    Exit(3);
  if (ByteValue and $F8) = $F0 then
    Exit(4);
  Result := 1;
end;

function IsContinuationByte(const AText: string; const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 1) and (AIndex <= Length(AText)) and
    ((Ord(AText[AIndex]) and $C0) = $80);
end;

function TryReadUTF8CodePoint(const AText: string; const AIndex: Integer;
  out ACodePoint: Cardinal; out AByteLength: Integer): Boolean;
var
  ByteValue: array[0..3] of Byte;
begin
  Result := False;
  ACodePoint := 0;
  AByteLength := 1;

  if (AIndex < 1) or (AIndex > Length(AText)) then
    Exit;

  ByteValue[0] := Ord(AText[AIndex]);
  if ByteValue[0] < $80 then
  begin
    ACodePoint := ByteValue[0];
    Exit(True);
  end;

  if (ByteValue[0] < $C2) or (ByteValue[0] > $F4) then
    Exit;

  if (ByteValue[0] and $E0) = $C0 then
  begin
    AByteLength := 2;
    if (AIndex + 1 > Length(AText)) or
       not IsContinuationByte(AText, AIndex + 1) then
      Exit;
    ByteValue[1] := Ord(AText[AIndex + 1]);
    ACodePoint := Cardinal(ByteValue[0] and $1F) shl 6 or
      Cardinal(ByteValue[1] and $3F);
    Exit(True);
  end;

  if (ByteValue[0] and $F0) = $E0 then
  begin
    AByteLength := 3;
    if (AIndex + 2 > Length(AText)) or
       not IsContinuationByte(AText, AIndex + 1) or
       not IsContinuationByte(AText, AIndex + 2) then
      Exit;
    ByteValue[1] := Ord(AText[AIndex + 1]);
    ByteValue[2] := Ord(AText[AIndex + 2]);
    if ((ByteValue[0] = $E0) and (ByteValue[1] < $A0)) or
       ((ByteValue[0] = $ED) and (ByteValue[1] >= $A0)) then
      Exit;
    ACodePoint := Cardinal(ByteValue[0] and $0F) shl 12 or
      Cardinal(ByteValue[1] and $3F) shl 6 or
      Cardinal(ByteValue[2] and $3F);
    Exit(True);
  end;

  AByteLength := 4;
  if (AIndex + 3 > Length(AText)) or
     not IsContinuationByte(AText, AIndex + 1) or
     not IsContinuationByte(AText, AIndex + 2) or
     not IsContinuationByte(AText, AIndex + 3) then
    Exit;
  ByteValue[1] := Ord(AText[AIndex + 1]);
  ByteValue[2] := Ord(AText[AIndex + 2]);
  ByteValue[3] := Ord(AText[AIndex + 3]);
  if ((ByteValue[0] = $F0) and (ByteValue[1] < $90)) or
     ((ByteValue[0] = $F4) and (ByteValue[1] > $8F)) then
    Exit;
  ACodePoint := Cardinal(ByteValue[0] and $07) shl 18 or
    Cardinal(ByteValue[1] and $3F) shl 12 or
    Cardinal(ByteValue[2] and $3F) shl 6 or
    Cardinal(ByteValue[3] and $3F);
  Result := True;
end;

function TryReadUTF8SurrogateCodePoint(const AText: string;
  const AIndex: Integer; out ACodePoint: Cardinal;
  out AByteLength: Integer): Boolean;
var
  ByteValue: array[0..2] of Byte;
begin
  Result := False;
  ACodePoint := 0;
  AByteLength := 1;

  if (AIndex < 1) or (AIndex + 2 > Length(AText)) then
    Exit;

  ByteValue[0] := Ord(AText[AIndex]);
  ByteValue[1] := Ord(AText[AIndex + 1]);
  ByteValue[2] := Ord(AText[AIndex + 2]);

  if (ByteValue[0] <> $ED) or (ByteValue[1] < $A0) or
     (ByteValue[1] > $BF) or ((ByteValue[2] and $C0) <> $80) then
    Exit;

  ACodePoint := Cardinal(ByteValue[0] and $0F) shl 12 or
    Cardinal(ByteValue[1] and $3F) shl 6 or
    Cardinal(ByteValue[2] and $3F);
  AByteLength := 3;
  Result := True;
end;

function TryReadUTF8CodePointAllowSurrogates(const AText: string;
  const AIndex: Integer; out ACodePoint: Cardinal;
  out AByteLength: Integer): Boolean;
begin
  Result := TryReadUTF8SurrogateCodePoint(AText, AIndex, ACodePoint,
    AByteLength);
  if not Result then
    Result := TryReadUTF8CodePoint(AText, AIndex, ACodePoint, AByteLength);
end;

function UTF8SequenceLengthAt(const AText: string;
  const AIndex: Integer): Integer;
var
  ByteLength: Integer;
  CodePoint: Cardinal;
begin
  if (AIndex < 1) or (AIndex > Length(AText)) then
    Exit(0);
  if TryReadUTF8CodePoint(AText, AIndex, CodePoint, ByteLength) then
    Exit(ByteLength);
  Result := 1;
end;

function CodePointToUTF8(const ACodePoint: Cardinal): string;
var
  CodePoint: Cardinal;
begin
  if ACodePoint > $10FFFF then
    CodePoint := UNICODE_REPLACEMENT_CODE_POINT
  else
    CodePoint := ACodePoint;

  if CodePoint <= $7F then
    Result := Chr(CodePoint)
  else if CodePoint <= $7FF then
    Result := Chr($C0 or (CodePoint shr 6)) +
      Chr($80 or (CodePoint and $3F))
  else if CodePoint <= $FFFF then
    Result := Chr($E0 or (CodePoint shr 12)) +
      Chr($80 or ((CodePoint shr 6) and $3F)) +
      Chr($80 or (CodePoint and $3F))
  else
    Result := Chr($F0 or (CodePoint shr 18)) +
      Chr($80 or ((CodePoint shr 12) and $3F)) +
      Chr($80 or ((CodePoint shr 6) and $3F)) +
      Chr($80 or (CodePoint and $3F));
end;

function IsUnicodeSurrogateCodePoint(const ACodePoint: Cardinal): Boolean;
begin
  Result := (ACodePoint >= $D800) and (ACodePoint <= $DFFF);
end;

function IsECMAScriptWhitespaceCodePoint(const ACodePoint: Cardinal): Boolean;
begin
  case ACodePoint of
    $0009,
    $000A,
    $000B,
    $000C,
    $000D,
    $0020,
    $00A0,
    $1680,
    $2028,
    $2029,
    $202F,
    $205F,
    $3000,
    $FEFF:
      Exit(True);
    $2000..$200A:
      Exit(True);
  end;
  Result := False;
end;

function IsECMAScriptWhitespaceAt(const AText: string; const AIndex: Integer;
  out AByteLength: Integer): Boolean;
var
  CodePoint: Cardinal;
begin
  Result := TryReadUTF8CodePoint(AText, AIndex, CodePoint, AByteLength) and
    IsECMAScriptWhitespaceCodePoint(CodePoint);
  if not Result then
    AByteLength := 1;
end;

function TrimECMAScriptWhitespaceStart(const AText: string): string;
var
  ByteLength: Integer;
  StartIndex: Integer;
begin
  StartIndex := 1;
  while (StartIndex <= Length(AText)) and
        IsECMAScriptWhitespaceAt(AText, StartIndex, ByteLength) do
    Inc(StartIndex, ByteLength);
  Result := Copy(AText, StartIndex, MaxInt);
end;

function TrimECMAScriptWhitespaceEnd(const AText: string): string;
var
  ByteLength: Integer;
  EndIndex: Integer;
  Index: Integer;
begin
  EndIndex := 0;
  Index := 1;
  while Index <= Length(AText) do
  begin
    if not IsECMAScriptWhitespaceAt(AText, Index, ByteLength) then
      EndIndex := Index + ByteLength - 1;
    Inc(Index, ByteLength);
  end;
  Result := Copy(AText, 1, EndIndex);
end;

function TrimECMAScriptWhitespace(const AText: string): string;
begin
  Result := TrimECMAScriptWhitespaceEnd(
    TrimECMAScriptWhitespaceStart(AText));
end;

function IsWellFormedUTF8(const AText: string): Boolean;
var
  ByteLength: Integer;
  CodePoint: Cardinal;
  Index: Integer;
begin
  Index := 1;
  while Index <= Length(AText) do
  begin
    if not TryReadUTF8CodePoint(AText, Index, CodePoint, ByteLength) then
      Exit(False);
    Inc(Index, ByteLength);
  end;
  Result := True;
end;

function ToWellFormedUTF8(const AText: string): string;
var
  Buffer: TStringBuffer;
  ByteLength: Integer;
  CodePoint: Cardinal;
  Index: Integer;
  InvalidLength: Integer;
begin
  Buffer := TStringBuffer.Create(Length(AText));
  Index := 1;
  while Index <= Length(AText) do
  begin
    if TryReadUTF8CodePoint(AText, Index, CodePoint, ByteLength) then
    begin
      Buffer.Append(Copy(AText, Index, ByteLength));
      Inc(Index, ByteLength);
    end
    else
    begin
      Buffer.Append(UTF8_REPLACEMENT_CHARACTER);
      InvalidLength := 1;
      if ByteLength < 1 then
        ByteLength := 1;
      while (InvalidLength < ByteLength) and
        (Index + InvalidLength <= Length(AText)) and
        IsContinuationByte(AText, Index + InvalidLength) do
        Inc(InvalidLength);
      if Index + InvalidLength - 1 > Length(AText) then
        InvalidLength := Length(AText) - Index + 1;
      Inc(Index, InvalidLength);
    end;
  end;
  Result := Buffer.ToString;
end;

function UTF8CodePointLength(const AText: string): Integer;
var
  ByteLength: Integer;
  CodePoint: Cardinal;
  Index: Integer;
begin
  Result := 0;
  Index := 1;
  while Index <= Length(AText) do
  begin
    if TryReadUTF8CodePointAllowSurrogates(AText, Index, CodePoint,
      ByteLength) then
      Inc(Index, ByteLength)
    else
      Inc(Index);
    Inc(Result);
  end;
end;

function UTF8CodePointAt(const AText: string; const AIndex: Integer): string;
var
  ByteLength: Integer;
  CodePoint: Cardinal;
  CodePointIndex: Integer;
  Index: Integer;
begin
  if AIndex < 0 then
    Exit('');

  CodePointIndex := 0;
  Index := 1;
  while Index <= Length(AText) do
  begin
    if TryReadUTF8CodePointAllowSurrogates(AText, Index, CodePoint,
      ByteLength) then
    begin
      if CodePointIndex = AIndex then
        Exit(Copy(AText, Index, ByteLength));
      Inc(Index, ByteLength);
    end
    else
    begin
      if CodePointIndex = AIndex then
        Exit(AText[Index]);
      Inc(Index);
    end;
    Inc(CodePointIndex);
  end;

  Result := '';
end;

function UTF16CodeUnitToUTF8(const ACodeUnit: Cardinal): string;
begin
  if ACodeUnit <= $7F then
    Result := Chr(ACodeUnit)
  else if ACodeUnit <= $7FF then
    Result := Chr($C0 or (ACodeUnit shr 6)) +
      Chr($80 or (ACodeUnit and $3F))
  else
    Result := Chr($E0 or (ACodeUnit shr 12)) +
      Chr($80 or ((ACodeUnit shr 6) and $3F)) +
      Chr($80 or (ACodeUnit and $3F));
end;

function UTF16CodeUnitLength(const AText: string): Integer;
var
  ByteLength: Integer;
  CodePoint: Cardinal;
  Index: Integer;
begin
  Result := 0;
  Index := 1;
  while Index <= Length(AText) do
  begin
    if TryReadUTF8CodePointAllowSurrogates(AText, Index, CodePoint,
      ByteLength) then
    begin
      if CodePoint > $FFFF then
        Inc(Result, 2)
      else
        Inc(Result);
      Inc(Index, ByteLength);
    end
    else
    begin
      Inc(Result);
      Inc(Index);
    end;
  end;
end;

function UTF16CodeUnitAt(const AText: string; const AIndex: Integer): string;
var
  ByteLength: Integer;
  CodePoint: Cardinal;
  CodeUnitIndex: Integer;
  HighSurrogate: Cardinal;
  Index: Integer;
  LowSurrogate: Cardinal;
  Supplementary: Cardinal;
begin
  if AIndex < 0 then
    Exit('');

  CodeUnitIndex := 0;
  Index := 1;
  while Index <= Length(AText) do
  begin
    if TryReadUTF8CodePointAllowSurrogates(AText, Index, CodePoint,
      ByteLength) then
    begin
      if CodePoint <= $FFFF then
      begin
        if CodeUnitIndex = AIndex then
          Exit(Copy(AText, Index, ByteLength));
        Inc(CodeUnitIndex);
      end
      else
      begin
        Supplementary := CodePoint - $10000;
        HighSurrogate := $D800 + (Supplementary shr 10);
        LowSurrogate := $DC00 + (Supplementary and $3FF);
        if CodeUnitIndex = AIndex then
          Exit(UTF16CodeUnitToUTF8(HighSurrogate));
        if CodeUnitIndex + 1 = AIndex then
          Exit(UTF16CodeUnitToUTF8(LowSurrogate));
        Inc(CodeUnitIndex, 2);
      end;
      Inc(Index, ByteLength);
    end
    else
    begin
      if CodeUnitIndex = AIndex then
        Exit(AText[Index]);
      Inc(CodeUnitIndex);
      Inc(Index);
    end;
  end;

  Result := '';
end;

function TryUTF16CodePointValueAt(const AText: string; const AIndex: Integer;
  out ACodePoint: Cardinal): Boolean;
var
  ByteLength: Integer;
  CodePoint: Cardinal;
  CodeUnitIndex: Integer;
  Index: Integer;
  LowSurrogate: Cardinal;
  Supplementary: Cardinal;
begin
  ACodePoint := 0;
  if AIndex < 0 then
    Exit(False);

  CodeUnitIndex := 0;
  Index := 1;
  while Index <= Length(AText) do
  begin
    if TryReadUTF8CodePointAllowSurrogates(AText, Index, CodePoint,
      ByteLength) then
    begin
      if CodePoint <= $FFFF then
      begin
        if CodeUnitIndex = AIndex then
        begin
          ACodePoint := CodePoint;
          Exit(True);
        end;
        Inc(CodeUnitIndex);
      end
      else
      begin
        if CodeUnitIndex = AIndex then
        begin
          ACodePoint := CodePoint;
          Exit(True);
        end;
        Supplementary := CodePoint - $10000;
        LowSurrogate := $DC00 + (Supplementary and $3FF);
        if CodeUnitIndex + 1 = AIndex then
        begin
          ACodePoint := LowSurrogate;
          Exit(True);
        end;
        Inc(CodeUnitIndex, 2);
      end;
      Inc(Index, ByteLength);
    end
    else
    begin
      if CodeUnitIndex = AIndex then
      begin
        ACodePoint := Ord(AText[Index]);
        Exit(True);
      end;
      Inc(CodeUnitIndex);
      Inc(Index);
    end;
  end;

  Result := False;
end;

function UTF16Substring(const AText: string; const AStart,
  ACount: Integer): string;
var
  Buffer: TStringBuffer;
  ByteLength: Integer;
  CodePoint: Cardinal;
  CodeUnitIndex: Integer;
  HighSurrogate: Cardinal;
  Index: Integer;
  LowSelected: Boolean;
  LowSurrogate: Cardinal;
  HighSelected: Boolean;
  TargetEnd: Integer;
  Supplementary: Cardinal;
begin
  if (AStart < 0) or (ACount <= 0) then
    Exit('');

  Buffer := TStringBuffer.Create(Length(AText));
  CodeUnitIndex := 0;
  Index := 1;
  TargetEnd := AStart + ACount;
  while (Index <= Length(AText)) and (CodeUnitIndex < TargetEnd) do
  begin
    if TryReadUTF8CodePointAllowSurrogates(AText, Index, CodePoint,
      ByteLength) then
    begin
      if CodePoint <= $FFFF then
      begin
        if (CodeUnitIndex >= AStart) and (CodeUnitIndex < TargetEnd) then
          Buffer.Append(Copy(AText, Index, ByteLength));
        Inc(CodeUnitIndex);
      end
      else
      begin
        HighSelected := (CodeUnitIndex >= AStart) and
          (CodeUnitIndex < TargetEnd);
        LowSelected := (CodeUnitIndex + 1 >= AStart) and
          (CodeUnitIndex + 1 < TargetEnd);
        if HighSelected and LowSelected then
          Buffer.Append(Copy(AText, Index, ByteLength))
        else
        begin
          Supplementary := CodePoint - $10000;
          HighSurrogate := $D800 + (Supplementary shr 10);
          LowSurrogate := $DC00 + (Supplementary and $3FF);
          if HighSelected then
            Buffer.Append(UTF16CodeUnitToUTF8(HighSurrogate));
          if LowSelected then
            Buffer.Append(UTF16CodeUnitToUTF8(LowSurrogate));
        end;
        Inc(CodeUnitIndex, 2);
      end;
      Inc(Index, ByteLength);
    end
    else
    begin
      if (CodeUnitIndex >= AStart) and (CodeUnitIndex < TargetEnd) then
        Buffer.Append(AText[Index]);
      Inc(CodeUnitIndex);
      Inc(Index);
    end;
  end;
  Result := Buffer.ToString;
end;

type
  TUTF16CodeUnitArray = array of Cardinal;

function BuildUTF16CodeUnitArray(const AText: string): TUTF16CodeUnitArray;
var
  ByteLength: Integer;
  CodePoint: Cardinal;
  Count: Integer;
  Index: Integer;
  Supplementary: Cardinal;
begin
  SetLength(Result, Length(AText));
  Count := 0;
  Index := 1;
  while Index <= Length(AText) do
  begin
    if TryReadUTF8CodePointAllowSurrogates(AText, Index, CodePoint,
      ByteLength) then
    begin
      if CodePoint <= $FFFF then
      begin
        Result[Count] := CodePoint;
        Inc(Count);
      end
      else
      begin
        Supplementary := CodePoint - $10000;
        Result[Count] := $D800 + (Supplementary shr 10);
        Result[Count + 1] := $DC00 + (Supplementary and $3FF);
        Inc(Count, 2);
      end;
      Inc(Index, ByteLength);
    end
    else
    begin
      Result[Count] := Ord(AText[Index]);
      Inc(Count);
      Inc(Index);
    end;
  end;
  SetLength(Result, Count);
end;

function UTF16CodeUnitsEqualAt(const ATextUnits,
  ASearchUnits: TUTF16CodeUnitArray; const AStart: Integer): Boolean;
var
  I: Integer;
begin
  if Length(ASearchUnits) = 0 then
    Exit(True);
  if AStart + Length(ASearchUnits) > Length(ATextUnits) then
    Exit(False);

  for I := 0 to Length(ASearchUnits) - 1 do
    if ATextUnits[AStart + I] <> ASearchUnits[I] then
      Exit(False);

  Result := True;
end;

function UTF16IndexOf(const AText, ASearch: string;
  const AStart: Integer): Integer;
var
  I: Integer;
  SearchUnits: TUTF16CodeUnitArray;
  SearchLength: Integer;
  StartIndex: Integer;
  TextUnits: TUTF16CodeUnitArray;
  TextLength: Integer;
begin
  TextUnits := BuildUTF16CodeUnitArray(AText);
  SearchUnits := BuildUTF16CodeUnitArray(ASearch);
  TextLength := Length(TextUnits);
  SearchLength := Length(SearchUnits);
  StartIndex := Max(0, Min(AStart, TextLength));

  if SearchLength = 0 then
    Exit(StartIndex);
  if SearchLength > TextLength then
    Exit(-1);

  for I := StartIndex to TextLength - SearchLength do
    if UTF16CodeUnitsEqualAt(TextUnits, SearchUnits, I) then
      Exit(I);

  Result := -1;
end;

function UTF16LastIndexOf(const AText, ASearch: string;
  const AStart: Integer): Integer;
var
  I: Integer;
  SearchUnits: TUTF16CodeUnitArray;
  SearchLength: Integer;
  StartIndex: Integer;
  TextUnits: TUTF16CodeUnitArray;
  TextLength: Integer;
begin
  TextUnits := BuildUTF16CodeUnitArray(AText);
  SearchUnits := BuildUTF16CodeUnitArray(ASearch);
  TextLength := Length(TextUnits);
  SearchLength := Length(SearchUnits);
  StartIndex := Max(0, Min(AStart, TextLength));

  if SearchLength = 0 then
    Exit(StartIndex);
  if SearchLength > TextLength then
    Exit(-1);

  for I := Min(StartIndex, TextLength - SearchLength) downto 0 do
    if UTF16CodeUnitsEqualAt(TextUnits, SearchUnits, I) then
      Exit(I);

  Result := -1;
end;

function UTF8TextToUnicodeString(const AText: string): UnicodeString;
var
  Bytes: RawByteString;
begin
  Bytes := RawByteString(AText);
  SetCodePage(Bytes, CP_UTF8, False);
  Result := UTF8Decode(UTF8String(Bytes));
end;

function UnicodeStringToUTF8Text(const AText: UnicodeString): string;
var
  Bytes: RawByteString;
begin
  Bytes := RawByteString(UTF8Encode(AText));
  Result := RetagUTF8Text(Bytes);
end;

function UnicodeLowerCaseUTF8(const AText: string): string;
begin
  Result := UnicodeStringToUTF8Text(
    UnicodeLowerCase(UTF8TextToUnicodeString(AText)));
end;

function UnicodeUpperCaseUTF8(const AText: string): string;
begin
  Result := UnicodeStringToUTF8Text(
    UnicodeUpperCase(UTF8TextToUnicodeString(AText)));
end;

function AdvanceUTF8StringIndex(const AText: string; const AIndex: Integer;
  const AUnicode: Boolean): Integer;
var
  ByteLength: Integer;
begin
  if AIndex >= Length(AText) then
    Exit(AIndex + 1);
  if not AUnicode then
    Exit(AIndex + 1);
  ByteLength := UTF8SequenceLengthAt(AText, AIndex + 1);
  if ByteLength <= 0 then
    ByteLength := 1;
  Result := Min(AIndex + ByteLength, Length(AText));
end;

function ReplacementCaptureText(const ACaptures: array of TReplacementCapture;
  const AIndex: Integer): string;
begin
  if (AIndex < 1) or (AIndex > Length(ACaptures)) or
     not ACaptures[AIndex - 1].Matched then
    Exit('');
  Result := ACaptures[AIndex - 1].Text;
end;

function FindNamedReplacementCapture(
  const ANamedCaptures: array of TReplacementNamedCapture;
  const AName: string; out AText: string): Boolean;
var
  I: Integer;
begin
  for I := Low(ANamedCaptures) to High(ANamedCaptures) do
    if ANamedCaptures[I].Name = AName then
    begin
      if ANamedCaptures[I].Matched then
        AText := ANamedCaptures[I].Text
      else
        AText := '';
      Exit(True);
    end;
  AText := '';
  Result := False;
end;

function ExpandReplacementPattern(const AReplacement, AMatched,
  AInput: string; const AMatchStart: Integer;
  const ACaptures: array of TReplacementCapture;
  const ANamedCaptures: array of TReplacementNamedCapture): string;
var
  Buffer: TStringBuffer;
  CaptureCount: Integer;
  ClosingIndex: Integer;
  GroupIndex: Integer;
  I: Integer;
  Name: string;
  NamedText: string;
  NextChar: Char;
  TwoDigitIndex: Integer;
begin
  Buffer := TStringBuffer.Create(Length(AReplacement));
  CaptureCount := Length(ACaptures);
  I := 1;
  while I <= Length(AReplacement) do
  begin
    if AReplacement[I] <> '$' then
    begin
      Buffer.AppendChar(AReplacement[I]);
      Inc(I);
      Continue;
    end;

    if I = Length(AReplacement) then
    begin
      Buffer.AppendChar('$');
      Break;
    end;

    NextChar := AReplacement[I + 1];
    case NextChar of
      '$':
        begin
          Buffer.AppendChar('$');
          Inc(I, 2);
        end;
      '&':
        begin
          Buffer.Append(AMatched);
          Inc(I, 2);
        end;
      '`':
        begin
          Buffer.Append(Copy(AInput, 1, AMatchStart));
          Inc(I, 2);
        end;
      '''':
        begin
          Buffer.Append(Copy(AInput, AMatchStart + Length(AMatched) + 1,
            MaxInt));
          Inc(I, 2);
        end;
      '<':
        begin
          if Length(ANamedCaptures) = 0 then
          begin
            Buffer.Append('$<');
            Inc(I, 2);
            Continue;
          end;

          ClosingIndex := I + 2;
          while (ClosingIndex <= Length(AReplacement)) and
                (AReplacement[ClosingIndex] <> '>') do
            Inc(ClosingIndex);
          if ClosingIndex > Length(AReplacement) then
          begin
            Buffer.Append('$<');
            Inc(I, 2);
            Continue;
          end;

          Name := Copy(AReplacement, I + 2, ClosingIndex - I - 2);
          FindNamedReplacementCapture(ANamedCaptures, Name, NamedText);
          Buffer.Append(NamedText);
          I := ClosingIndex + 1;
        end;
      '0'..'9':
        begin
          if NextChar = '0' then
          begin
            Buffer.Append('$0');
            Inc(I, 2);
            Continue;
          end;

          GroupIndex := Ord(NextChar) - Ord('0');
          if GroupIndex > CaptureCount then
          begin
            Buffer.AppendChar('$');
            Buffer.AppendChar(NextChar);
            Inc(I, 2);
            Continue;
          end;

          if (I + 2 <= Length(AReplacement)) and
             CharInSet(AReplacement[I + 2], ['0'..'9']) then
          begin
            TwoDigitIndex := GroupIndex * 10 +
              Ord(AReplacement[I + 2]) - Ord('0');
            if (TwoDigitIndex > 0) and (TwoDigitIndex <= CaptureCount) then
            begin
              GroupIndex := TwoDigitIndex;
              Inc(I, 3);
            end
            else
              Inc(I, 2);
          end
          else
            Inc(I, 2);

          Buffer.Append(ReplacementCaptureText(ACaptures, GroupIndex));
        end;
    else
      begin
        Buffer.AppendChar('$');
        Buffer.AppendChar(NextChar);
        Inc(I, 2);
      end;
    end;
  end;
  Result := Buffer.ToString;
end;

function CreateUTF8StringList(const AText: string): TStringList;
var
  LineStart: Integer;
  TextIndex: Integer;
begin
  Result := TStringList.Create;
  LineStart := 1;
  TextIndex := 1;

  while TextIndex <= Length(AText) do
  begin
    if AText[TextIndex] = #13 then
    begin
      Result.Add(Copy(AText, LineStart, TextIndex - LineStart));
      if (TextIndex < Length(AText)) and (AText[TextIndex + 1] = #10) then
        Inc(TextIndex);
      LineStart := TextIndex + 1;
    end
    else if AText[TextIndex] = #10 then
    begin
      Result.Add(Copy(AText, LineStart, TextIndex - LineStart));
      LineStart := TextIndex + 1;
    end;
    Inc(TextIndex);
  end;

  if LineStart <= Length(AText) then
    Result.Add(Copy(AText, LineStart, Length(AText) - LineStart + 1))
  else if (Length(AText) > 0) and ((AText[Length(AText)] = #10) or
    (AText[Length(AText)] = #13)) then
    Result.Add('');
end;

function IsUTF8LineOrParagraphSeparatorAt(const AText: string;
  const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 1) and (AIndex + 2 <= Length(AText)) and
    (AText[AIndex] = #$E2) and
    (AText[AIndex + 1] = #$80) and
    ((AText[AIndex + 2] = #$A8) or (AText[AIndex + 2] = #$A9));
end;

function CreateECMAScriptSourceLines(const AText: string): TStringList;
const
  UTF8_LINE_TERMINATOR_BYTE_LENGTH = 3;
var
  LineStart: Integer;
  TextIndex: Integer;
begin
  Result := TStringList.Create;
  LineStart := 1;
  TextIndex := 1;

  while TextIndex <= Length(AText) do
  begin
    if AText[TextIndex] = #13 then
    begin
      Result.Add(Copy(AText, LineStart, TextIndex - LineStart));
      if (TextIndex < Length(AText)) and (AText[TextIndex + 1] = #10) then
        Inc(TextIndex);
      LineStart := TextIndex + 1;
    end
    else if AText[TextIndex] = #10 then
    begin
      Result.Add(Copy(AText, LineStart, TextIndex - LineStart));
      LineStart := TextIndex + 1;
    end
    else if IsUTF8LineOrParagraphSeparatorAt(AText, TextIndex) then
    begin
      Result.Add(Copy(AText, LineStart, TextIndex - LineStart));
      Inc(TextIndex, UTF8_LINE_TERMINATOR_BYTE_LENGTH - 1);
      LineStart := TextIndex + 1;
    end;
    Inc(TextIndex);
  end;

  if LineStart <= Length(AText) then
    Result.Add(Copy(AText, LineStart, Length(AText) - LineStart + 1))
  else if (Length(AText) > 0) and
    ((AText[Length(AText)] = #10) or (AText[Length(AText)] = #13) or
    IsUTF8LineOrParagraphSeparatorAt(AText,
    Length(AText) - UTF8_LINE_TERMINATOR_BYTE_LENGTH + 1)) then
    Result.Add('');
end;

function CreateUTF8FileTextLines(const AText: UTF8String): TStringList;
var
  LineText: UTF8String;
  LineStart: Integer;
  TextIndex: Integer;
begin
  Result := TStringList.Create;
  LineStart := 1;
  TextIndex := 1;

  while TextIndex <= Length(AText) do
  begin
    if AText[TextIndex] = #13 then
    begin
      LineText := Copy(AText, LineStart, TextIndex - LineStart);
      Result.Add(RetagUTF8Text(RawByteString(LineText)));
      if (TextIndex < Length(AText)) and (AText[TextIndex + 1] = #10) then
        Inc(TextIndex);
      LineStart := TextIndex + 1;
    end
    else if AText[TextIndex] = #10 then
    begin
      LineText := Copy(AText, LineStart, TextIndex - LineStart);
      Result.Add(RetagUTF8Text(RawByteString(LineText)));
      LineStart := TextIndex + 1;
    end;
    Inc(TextIndex);
  end;

  if LineStart <= Length(AText) then
  begin
    LineText := Copy(AText, LineStart, Length(AText) - LineStart + 1);
    Result.Add(RetagUTF8Text(RawByteString(LineText)));
  end
  else if (Length(AText) > 0) and ((AText[Length(AText)] = #10) or
    (AText[Length(AText)] = #13)) then
    Result.Add('');
end;

function NormalizeNewlinesToLF(const AText: string): string;
begin
  Result := StringReplace(AText, #13#10, #10, [rfReplaceAll]);
  Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
end;

function NormalizeRawNewlinesToLF(const AText: RawByteString): RawByteString;
begin
  Result := StringReplace(AText, #13#10, #10, [rfReplaceAll]);
  Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
end;

function NormalizeUTF8NewlinesToLF(const AText: UTF8String): UTF8String;
var
  NormalizedBytes: RawByteString;
begin
  NormalizedBytes := NormalizeRawNewlinesToLF(RawByteString(AText));
  SetCodePage(NormalizedBytes, CP_UTF8, False);
  Result := UTF8String(NormalizedBytes);
end;

function StringListToLFText(const ALines: TStrings): string;
var
  Buffer: TStringBuffer;
  I: Integer;
begin
  if not Assigned(ALines) then
    Exit('');

  Buffer := TStringBuffer.Create;
  for I := 0 to ALines.Count - 1 do
  begin
    if I > 0 then
      Buffer.AppendChar(#10);
    Buffer.Append(ALines[I]);
  end;

  Result := Buffer.ToString;
end;

initialization
  {$IFDEF MSWINDOWS}
  DefaultSystemCodePage := CP_UTF8;
  {$ENDIF}

end.
