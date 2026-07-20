unit TextSemantics;

{$I Shared.inc}

interface

uses
  Classes,
  StrUtils,
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
  MAX_UNICODE_CODE_POINT = $10FFFF;
  UNICODE_REPLACEMENT_CODE_POINT = $FFFD;
  UNICODE_REPLACEMENT_CHARACTER = #$FFFD;

function CodePointSequenceLengthAt(const AText: string;
  const AIndex: Integer): Integer;
function TryReadCodePointAt(const AText: string; const AIndex: Integer;
  out ACodePoint: Cardinal; out AByteLength: Integer): Boolean;
function TryReadCodePointAtAllowSurrogates(const AText: string;
  const AIndex: Integer; out ACodePoint: Cardinal;
  out AByteLength: Integer): Boolean;
function IsUnicodeSurrogateCodePoint(const ACodePoint: Cardinal): Boolean;
{$IFDEF FPC}inline;{$ENDIF}
function IsECMAScriptWhitespaceCodePoint(const ACodePoint: Cardinal): Boolean;
function TrimECMAScriptWhitespace(const AText: string): string;
function TrimECMAScriptWhitespaceStart(const AText: string): string;
function TrimECMAScriptWhitespaceEnd(const AText: string): string;
function IsWellFormedUTF16(const AText: string): Boolean;
function ToWellFormedUTF16(const AText: string): string;
function UnicodeCodePointLength(const AText: string): Integer;
function UnicodeCodePointAt(const AText: string; const AIndex: Integer): string;
function UTF16CodeUnitToString(const ACodeUnit: Cardinal): string;
function UTF16CodeUnitPairToString(const AFirst, ASecond: Cardinal): string;
function UTF16CodeUnitLength(const AText: string): Integer;
function UTF16CodeUnitAt(const AText: string; const AIndex: Integer): string;
function UTF16StringsEqual(const ALeft, ARight: string): Boolean;
function UTF16StringHash(const AText: string): Cardinal;
function TryUTF16CodePointValueAt(const AText: string; const AIndex: Integer;
  out ACodePoint: Cardinal): Boolean;
function UTF16Substring(const AText: string; const AStart,
  ACount: Integer): string;
function RepeatUTF16String(const AText: string;
  const ACount: Integer): string;
function UTF16IndexOf(const AText, ASearch: string;
  const AStart: Integer = 0): Integer;
function UTF16LastIndexOf(const AText, ASearch: string;
  const AStart: Integer): Integer;
function ECMAScriptDefaultLowerCase(const AText: string): string;
function ECMAScriptDefaultUpperCase(const AText: string): string;
function AdvanceUTF16StringIndex(const AText: string; const AIndex: Integer;
  const AUnicode: Boolean): Integer;
function ExpandReplacementPattern(const AReplacement, AMatched,
  AInput: string; const AMatchStart: Integer;
  const ACaptures: array of TReplacementCapture;
  const ANamedCaptures: array of TReplacementNamedCapture): string;
function CreateTextLines(const AText: string): TStringList;
function CreateECMAScriptSourceLines(const AText: string): TStringList;
function CreateFileTextLines(const AText: string): TStringList;
function NormalizeNewlinesToLF(const AText: string): string;
function StringListToSourceText(const ALines: TStrings): string;
function StringListToLFText(const ALines: TStrings): string;

{ Release the per-thread is-ASCII memo. FPC does not auto-finalize managed
  threadvars at thread exit. This unit's own finalization clears the main
  thread's slots on process shutdown; because the unit stays free of engine
  dependencies, that path works in every binary that links it (including the
  shared JSON/numeric/config tools that never link the engine). Worker threads
  are covered separately: the engine's Goccia.RegExp.VM registers this proc with
  Goccia.ThreadCleanupRegistry, whose drain releases each worker's slots on exit. }
procedure ClearAsciiMemo;

implementation

uses
  Math,
{$IF DEFINED(FPC) AND DEFINED(UNIX)}
  cwstring,
{$ENDIF}
  Generated.UnicodeCaseData,
  StringBuffer;

type
  TSourceTextStringList = class(TStringList)
  private
    FSourceText: string;
    FSourceTextValid: Boolean;
  protected
    procedure Changed; override;
  public
    procedure SetSourceText(const ASourceText: string);
    function HasSourceText: Boolean;
    property SourceText: string read FSourceText;
  end;

procedure TSourceTextStringList.Changed;
begin
  inherited Changed;
  FSourceText := '';
  FSourceTextValid := False;
end;

procedure TSourceTextStringList.SetSourceText(const ASourceText: string);
begin
  FSourceText := ASourceText;
  FSourceTextValid := True;
end;

function TSourceTextStringList.HasSourceText: Boolean;
begin
  Result := FSourceTextValid;
end;

function IsHighSurrogateCodeUnit(const ACodeUnit: Cardinal): Boolean;
{$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (ACodeUnit >= $D800) and (ACodeUnit <= $DBFF);
end;

function IsLowSurrogateCodeUnit(const ACodeUnit: Cardinal): Boolean;
{$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (ACodeUnit >= $DC00) and (ACodeUnit <= $DFFF);
end;

function TryReadCodePointAt(const AText: string; const AIndex: Integer;
  out ACodePoint: Cardinal; out AByteLength: Integer): Boolean;
var
  FirstCodeUnit, SecondCodeUnit: Cardinal;
begin
  Result := False;
  ACodePoint := 0;
  AByteLength := 1;
  if (AIndex < 1) or (AIndex > Length(AText)) then
    Exit;
  FirstCodeUnit := Ord(AText[AIndex]);
  if IsLowSurrogateCodeUnit(FirstCodeUnit) then
    Exit;
  if IsHighSurrogateCodeUnit(FirstCodeUnit) then
  begin
    if AIndex >= Length(AText) then
      Exit;
    SecondCodeUnit := Ord(AText[AIndex + 1]);
    if not IsLowSurrogateCodeUnit(SecondCodeUnit) then
      Exit;
    ACodePoint := $10000 + ((FirstCodeUnit - $D800) shl 10) +
      (SecondCodeUnit - $DC00);
    AByteLength := 2;
  end
  else
    ACodePoint := FirstCodeUnit;
  Result := True;
end;

function TryReadCodePointAtAllowSurrogates(const AText: string;
  const AIndex: Integer; out ACodePoint: Cardinal;
  out AByteLength: Integer): Boolean;
begin
  if TryReadCodePointAt(AText, AIndex, ACodePoint, AByteLength) then
    Exit(True);
  if (AIndex >= 1) and (AIndex <= Length(AText)) then
  begin
    ACodePoint := Ord(AText[AIndex]);
    AByteLength := 1;
    Exit(True);
  end;
  ACodePoint := 0;
  AByteLength := 0;
  Result := False;
end;

function CodePointSequenceLengthAt(const AText: string;
  const AIndex: Integer): Integer;
var
  ByteLength: Integer;
  CodePoint: Cardinal;
begin
  if (AIndex < 1) or (AIndex > Length(AText)) then
    Exit(0);
  if TryReadCodePointAt(AText, AIndex, CodePoint, ByteLength) then
    Exit(ByteLength);
  Result := 1;
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
  Result := TryReadCodePointAt(AText, AIndex, CodePoint, AByteLength) and
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


function IsWellFormedUTF16(const AText: string): Boolean;
var
  Index: Integer;
begin
  Index := 1;
  while Index <= Length(AText) do
  begin
    if IsLowSurrogateCodeUnit(Ord(AText[Index])) then
      Exit(False);
    if IsHighSurrogateCodeUnit(Ord(AText[Index])) then
    begin
      if (Index = Length(AText)) or
         not IsLowSurrogateCodeUnit(Ord(AText[Index + 1])) then
        Exit(False);
      Inc(Index, 2);
    end
    else
      Inc(Index);
  end;
  Result := True;
end;

function ToWellFormedUTF16(const AText: string): string;
var
  Buffer: TStringBuffer;
  Index: Integer;
begin
  Buffer := TStringBuffer.Create(Length(AText));
  Index := 1;
  while Index <= Length(AText) do
  begin
    if IsHighSurrogateCodeUnit(Ord(AText[Index])) then
    begin
      if (Index < Length(AText)) and
         IsLowSurrogateCodeUnit(Ord(AText[Index + 1])) then
      begin
        Buffer.AppendChar(AText[Index]);
        Buffer.AppendChar(AText[Index + 1]);
        Inc(Index, 2);
      end
      else
      begin
        Buffer.AppendChar(Char($FFFD));
        Inc(Index);
      end;
    end
    else if IsLowSurrogateCodeUnit(Ord(AText[Index])) then
    begin
      Buffer.AppendChar(Char($FFFD));
      Inc(Index);
    end
    else
    begin
      Buffer.AppendChar(AText[Index]);
      Inc(Index);
    end;
  end;
  Result := Buffer.ToString;
end;

function UnicodeCodePointLength(const AText: string): Integer;
var
  Index: Integer;
begin
  Result := 0;
  Index := 1;
  while Index <= Length(AText) do
  begin
    if IsHighSurrogateCodeUnit(Ord(AText[Index])) and
       (Index < Length(AText)) and
       IsLowSurrogateCodeUnit(Ord(AText[Index + 1])) then
      Inc(Index, 2)
    else
      Inc(Index);
    Inc(Result);
  end;
end;

function UnicodeCodePointAt(const AText: string; const AIndex: Integer): string;
var
  CodePointIndex, Index: Integer;
begin
  if AIndex < 0 then
    Exit('');
  CodePointIndex := 0;
  Index := 1;
  while Index <= Length(AText) do
  begin
    if CodePointIndex = AIndex then
    begin
      if IsHighSurrogateCodeUnit(Ord(AText[Index])) and
         (Index < Length(AText)) and
         IsLowSurrogateCodeUnit(Ord(AText[Index + 1])) then
        Exit(Copy(AText, Index, 2));
      Exit(AText[Index]);
    end;
    if IsHighSurrogateCodeUnit(Ord(AText[Index])) and
       (Index < Length(AText)) and
       IsLowSurrogateCodeUnit(Ord(AText[Index + 1])) then
      Inc(Index, 2)
    else
      Inc(Index);
    Inc(CodePointIndex);
  end;
  Result := '';
end;

function UTF16CodeUnitToString(const ACodeUnit: Cardinal): string;
begin
  Result := Char(ACodeUnit and $FFFF);
end;

function UTF16CodeUnitPairToString(const AFirst, ASecond: Cardinal): string;
begin
  Result := Char(AFirst and $FFFF) + Char(ASecond and $FFFF);
end;

threadvar
  GAsciiMemoStr0: string;
  GAsciiMemoStr1: string;

function UTF16StringsEqual(const ALeft, ARight: string): Boolean;
begin
  Result := ALeft = ARight;
end;

function UTF16StringHash(const AText: string): Cardinal;
var
  I: Integer;
begin
  Result := 5381;
  for I := 1 to Length(AText) do
    Result := Cardinal((UInt64(Result) * 33 + Ord(AText[I])) and $FFFFFFFF);
end;

function UTF16CodeUnitLength(const AText: string): Integer;
begin
  Result := Length(AText);
end;

function UTF16CodeUnitAt(const AText: string; const AIndex: Integer): string;
begin
  if (AIndex < 0) or (AIndex >= Length(AText)) then
    Exit('');
  Result := AText[AIndex + 1];
end;

function TryUTF16CodePointValueAt(const AText: string;
  const AIndex: Integer; out ACodePoint: Cardinal): Boolean;
var
  FirstCodeUnit, SecondCodeUnit: Cardinal;
begin
  ACodePoint := 0;
  if (AIndex < 0) or (AIndex >= Length(AText)) then
    Exit(False);
  FirstCodeUnit := Ord(AText[AIndex + 1]);
  if IsHighSurrogateCodeUnit(FirstCodeUnit) and
     (AIndex + 1 < Length(AText)) then
  begin
    SecondCodeUnit := Ord(AText[AIndex + 2]);
    if IsLowSurrogateCodeUnit(SecondCodeUnit) then
    begin
      ACodePoint := $10000 + ((FirstCodeUnit - $D800) shl 10) +
        (SecondCodeUnit - $DC00);
      Exit(True);
    end;
  end;
  ACodePoint := FirstCodeUnit;
  Result := True;
end;

function UTF16Substring(const AText: string; const AStart,
  ACount: Integer): string;
var
  Count, StartIndex: Integer;
begin
  StartIndex := Max(0, Min(AStart, Length(AText)));
  Count := Max(0, Min(ACount, Length(AText) - StartIndex));
  Result := Copy(AText, StartIndex + 1, Count);
end;

function RepeatUTF16String(const AText: string;
  const ACount: Integer): string;
var
  Buffer: TStringBuffer;
  I: Integer;
begin
  if (AText = '') or (ACount <= 0) then
    Exit('');
  if ACount = 1 then
    Exit(AText);

  Buffer := TStringBuffer.Create(Length(AText) * ACount);
  for I := 1 to ACount do
    Buffer.Append(AText);
  Result := Buffer.ToString;
end;

function UTF16IndexOf(const AText, ASearch: string;
  const AStart: Integer): Integer;
var
  MatchPosition, StartIndex: Integer;
begin
  StartIndex := Max(0, Min(AStart, Length(AText)));
  if ASearch = '' then
    Exit(StartIndex);
  MatchPosition := PosEx(ASearch, AText, StartIndex + 1);
  if MatchPosition = 0 then
    Exit(-1);
  Result := MatchPosition - 1;
end;

function UTF16LastIndexOf(const AText, ASearch: string;
  const AStart: Integer): Integer;
var
  I, StartIndex: Integer;
begin
  StartIndex := Max(0, Min(AStart, Length(AText)));
  if ASearch = '' then
    Exit(StartIndex);
  for I := Min(StartIndex, Length(AText) - Length(ASearch)) downto 0 do
    if Copy(AText, I + 1, Length(ASearch)) = ASearch then
      Exit(I);
  Result := -1;
end;

function HasCasedCodePointBefore(const AText: string;
  const AIndex: Integer): Boolean;
var
  CodePoint: Cardinal;
  CodeUnitLength, I: Integer;
begin
  I := AIndex - 1;
  while I >= 1 do
  begin
    if IsLowSurrogateCodeUnit(Ord(AText[I])) and (I > 1) and
       IsHighSurrogateCodeUnit(Ord(AText[I - 1])) then
      Dec(I);
    TryReadCodePointAtAllowSurrogates(AText, I, CodePoint,
      CodeUnitLength);
    if not IsUnicodeCaseIgnorable(CodePoint) then
      Exit(IsUnicodeCased(CodePoint));
    Dec(I);
  end;
  Result := False;
end;

function HasCasedCodePointAfter(const AText: string;
  const AIndex: Integer): Boolean;
var
  CodePoint: Cardinal;
  CodeUnitLength, I: Integer;
begin
  I := AIndex;
  while I <= Length(AText) do
  begin
    TryReadCodePointAtAllowSurrogates(AText, I, CodePoint,
      CodeUnitLength);
    if not IsUnicodeCaseIgnorable(CodePoint) then
      Exit(IsUnicodeCased(CodePoint));
    Inc(I, CodeUnitLength);
  end;
  Result := False;
end;

function IsFinalSigma(const AText: string; const AIndex,
  ACodeUnitLength: Integer): Boolean;
begin
  Result := HasCasedCodePointBefore(AText, AIndex) and
    not HasCasedCodePointAfter(AText, AIndex + ACodeUnitLength);
end;

function MapECMAScriptDefaultCase(const AText: string;
  const AUppercase: Boolean): string;
var
  Buffer: TStringBuffer;
  CodePoint: Cardinal;
  CodeUnitLength, I: Integer;
  Mapping: string;
begin
  Buffer := TStringBuffer.Create(Length(AText));
  I := 1;
  while I <= Length(AText) do
  begin
    TryReadCodePointAtAllowSurrogates(AText, I, CodePoint,
      CodeUnitLength);
    if not AUppercase and (CodePoint = $03A3) and
       IsFinalSigma(AText, I, CodeUnitLength) then
      Mapping := #$03C2
    else if AUppercase then
    begin
      if not TryGetUnicodeDefaultUppercase(CodePoint, Mapping) then
        Mapping := Copy(AText, I, CodeUnitLength);
    end
    else if not TryGetUnicodeDefaultLowercase(CodePoint, Mapping) then
      Mapping := Copy(AText, I, CodeUnitLength);
    Buffer.Append(Mapping);
    Inc(I, CodeUnitLength);
  end;
  Result := Buffer.ToString;
end;

function ECMAScriptDefaultLowerCase(const AText: string): string;
begin
  Result := MapECMAScriptDefaultCase(AText, False);
end;

function ECMAScriptDefaultUpperCase(const AText: string): string;
begin
  Result := MapECMAScriptDefaultCase(AText, True);
end;

function AdvanceUTF16StringIndex(const AText: string;
  const AIndex: Integer; const AUnicode: Boolean): Integer;
begin
  Result := AIndex + 1;
  if AUnicode and (AIndex >= 0) and (AIndex + 1 < Length(AText)) and
     IsHighSurrogateCodeUnit(Ord(AText[AIndex + 1])) and
     IsLowSurrogateCodeUnit(Ord(AText[AIndex + 2])) then
    Inc(Result);
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
          Buffer.Append(UTF16Substring(AInput, 0, AMatchStart));
          Inc(I, 2);
        end;
      '''':
        begin
          Buffer.Append(UTF16Substring(AInput,
            AMatchStart + UTF16CodeUnitLength(AMatched),
            UTF16CodeUnitLength(AInput)));
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

function CreateTextLines(const AText: string): TStringList;
var
  LineStart: Integer;
  TextIndex: Integer;
begin
  Result := TSourceTextStringList.Create;
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
  TSourceTextStringList(Result).SetSourceText(AText);
end;

function IsLineOrParagraphSeparatorAt(const AText: string;
  const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 1) and (AIndex <= Length(AText)) and
    ((AText[AIndex] = Char($2028)) or (AText[AIndex] = Char($2029)));
end;

function CreateECMAScriptSourceLines(const AText: string): TStringList;
var
  LineStart: Integer;
  TextIndex: Integer;
begin
  Result := TSourceTextStringList.Create;
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
    else if IsLineOrParagraphSeparatorAt(AText, TextIndex) then
    begin
      Result.Add(Copy(AText, LineStart, TextIndex - LineStart));
      LineStart := TextIndex + 1;
    end;
    Inc(TextIndex);
  end;

  if LineStart <= Length(AText) then
    Result.Add(Copy(AText, LineStart, Length(AText) - LineStart + 1))
  else if (Length(AText) > 0) and
    ((AText[Length(AText)] = #10) or (AText[Length(AText)] = #13) or
    IsLineOrParagraphSeparatorAt(AText, Length(AText))) then
    Result.Add('');
  TSourceTextStringList(Result).SetSourceText(AText);
end;

function CreateFileTextLines(const AText: string): TStringList;
begin
  Result := CreateTextLines(AText);
end;

function NormalizeNewlinesToLF(const AText: string): string;
begin
  Result := StringReplace(AText, #13#10, #10, [rfReplaceAll]);
  Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
end;

function StringListToSourceText(const ALines: TStrings): string;
begin
  if not Assigned(ALines) then
    Exit('');
  if (ALines is TSourceTextStringList) and
     TSourceTextStringList(ALines).HasSourceText then
    Exit(TSourceTextStringList(ALines).SourceText);
  Result := StringListToLFText(ALines);
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

// Release the is-ASCII memo strings; FPC does not finalize managed threadvars at
// thread exit. Run from this unit's own finalization on the main thread, and —
// for worker threads — via Goccia.ThreadCleanupRegistry, where Goccia.RegExp.VM
// registers it (see the declaration comment above).
procedure ClearAsciiMemo;
begin
  GAsciiMemoStr0 := '';
  GAsciiMemoStr1 := '';
end;

initialization

finalization
  // Main-thread cleanup, kept here because this generic unit has no engine
  // dependency and so cannot self-register with Goccia.ThreadCleanupRegistry;
  // it runs in every binary that links TextSemantics, including ones that never
  // link the engine (Goccia.RegExp.VM registers the worker-thread path). FPC
  // does not auto-finalize managed threadvars at thread exit.
  ClearAsciiMemo;

end.
