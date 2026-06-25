unit NumericText;

{$I Shared.inc}

interface

// Converts a String to a Number following the ECMA-262 StringToNumber
// abstract operation and the StringNumericLiteral grammar. Returns a raw
// Double (NaN for any input that is not a StringNumericLiteral) and performs
// no value-object allocation, so runtime primitive coercion and compile-time
// constant folding can share a single implementation instead of each keeping
// its own partial parser.
function StringToNumber(const AText: string): Double;

implementation

uses
  Math,
  SysUtils,

  TextSemantics;

const
  INFINITY_TEXT = 'Infinity';

function HexDigitValue(const AChar: Char): Integer;
begin
  case AChar of
    '0'..'9': Result := Ord(AChar) - Ord('0');
    'a'..'f': Result := Ord(AChar) - Ord('a') + 10;
    'A'..'F': Result := Ord(AChar) - Ord('A') + 10;
  else
    Result := -1;
  end;
end;

// ES2026 §7.1.4.1 NonDecimalIntegerLiteral (BinaryIntegerLiteral,
// OctalIntegerLiteral, HexIntegerLiteral). These never carry a leading sign.
// Returns True when AText has a 0b / 0o / 0x prefix and therefore belongs to
// the non-decimal space; ANumber is then the parsed value, or NaN when the
// digits after the prefix are missing or invalid for the radix.
function TryNonDecimalIntegerToNumber(const AText: string;
  out ANumber: Double): Boolean;
var
  Radix: Integer;
  Index: Integer;
  DigitValue: Integer;
begin
  Result := False;
  if (Length(AText) < 2) or (AText[1] <> '0') then
    Exit;

  case AText[2] of
    'b', 'B': Radix := 2;
    'o', 'O': Radix := 8;
    'x', 'X': Radix := 16;
  else
    Exit;
  end;

  Result := True;

  if Length(AText) < 3 then
  begin
    ANumber := NaN;
    Exit;
  end;

  ANumber := 0.0;
  for Index := 3 to Length(AText) do
  begin
    DigitValue := HexDigitValue(AText[Index]);
    if (DigitValue < 0) or (DigitValue >= Radix) then
    begin
      ANumber := NaN;
      Exit;
    end;
    // Accumulate in Double so very large magnitudes round to the nearest
    // Double (matching StringNumericValue) rather than wrapping a fixed-width
    // integer the way a base-aware StrToInt would.
    ANumber := ANumber * Radix + DigitValue;
  end;
end;

// ES2026 §7.1.4.1 StrDecimalLiteral grammar check: an optional sign followed by
// a StrUnsignedDecimalLiteral (integer and/or fraction digits with an optional
// exponent part). Reports the sign so the caller can pick the correct signed
// Infinity when a grammar-valid literal overflows the Double range.
function IsStrDecimalLiteral(const AText: string;
  out ANegative: Boolean): Boolean;
var
  Index: Integer;
  HasDigit: Boolean;
  HasIntegerOrFractionDigit: Boolean;
  HasExponentDigit: Boolean;
begin
  ANegative := False;
  HasIntegerOrFractionDigit := False;
  Index := 1;

  if Index <= Length(AText) then
  begin
    if AText[Index] = '-' then
    begin
      ANegative := True;
      Inc(Index);
    end
    else if AText[Index] = '+' then
      Inc(Index);
  end;

  HasDigit := False;
  while (Index <= Length(AText)) and (AText[Index] >= '0') and
        (AText[Index] <= '9') do
  begin
    HasDigit := True;
    Inc(Index);
  end;
  HasIntegerOrFractionDigit := HasIntegerOrFractionDigit or HasDigit;

  if (Index <= Length(AText)) and (AText[Index] = '.') then
  begin
    Inc(Index);
    HasDigit := False;
    while (Index <= Length(AText)) and (AText[Index] >= '0') and
          (AText[Index] <= '9') do
    begin
      HasDigit := True;
      Inc(Index);
    end;
    HasIntegerOrFractionDigit := HasIntegerOrFractionDigit or HasDigit;
  end;

  if not HasIntegerOrFractionDigit then
    Exit(False);

  if (Index <= Length(AText)) and
     ((AText[Index] = 'e') or (AText[Index] = 'E')) then
  begin
    Inc(Index);
    if (Index <= Length(AText)) and
       ((AText[Index] = '+') or (AText[Index] = '-')) then
      Inc(Index);

    HasExponentDigit := False;
    while (Index <= Length(AText)) and (AText[Index] >= '0') and
          (AText[Index] <= '9') do
    begin
      HasExponentDigit := True;
      Inc(Index);
    end;

    if not HasExponentDigit then
      Exit(False);
  end;

  Result := Index > Length(AText);
end;

function StrDecimalLiteralToNumber(const AText: string): Double;
var
  Negative: Boolean;
  FormatSettings: TFormatSettings;
  Parsed: Double;
begin
  if not IsStrDecimalLiteral(AText, Negative) then
    Exit(NaN);

  // source/shared cannot import Goccia.Values.Primitives, so build the
  // invariant '.' decimal settings inline (see code-style.md § Source layout).
  FormatSettings := DefaultFormatSettings;
  FormatSettings.DecimalSeparator := '.';
  if TryStrToFloat(AText, Parsed, FormatSettings) then
    Exit(Parsed);

  // Grammar-valid but not representable as a finite Double: the only way a
  // StrDecimalLiteral fails to parse is overflow, whose StringNumericValue
  // rounds to +/-Infinity. (A zero magnitude is always representable, so it
  // never reaches this branch.)
  if Negative then
    Result := NegInfinity
  else
    Result := Infinity;
end;

// ES2026 §7.1.4.1.1 StringToNumber ( str )
function StringToNumber(const AText: string): Double;
var
  Trimmed: string;
begin
  // StringNumericLiteral allows leading and trailing ECMAScript whitespace.
  Trimmed := TrimECMAScriptWhitespace(AText);

  // StringNumericLiteral ::: [empty] evaluates to +0.
  if Trimmed = '' then
    Exit(0.0);

  // StrDecimalLiteral ::: (+ | -)? Infinity
  if Trimmed = INFINITY_TEXT then
    Exit(Infinity);
  if Trimmed = '+' + INFINITY_TEXT then
    Exit(Infinity);
  if Trimmed = '-' + INFINITY_TEXT then
    Exit(NegInfinity);

  // NonDecimalIntegerLiteral ::: 0b... | 0o... | 0x...  (no sign permitted)
  if TryNonDecimalIntegerToNumber(Trimmed, Result) then
    Exit;

  // StrDecimalLiteral, or NaN when the text is not a StringNumericLiteral.
  Result := StrDecimalLiteralToNumber(Trimmed);
end;

end.
