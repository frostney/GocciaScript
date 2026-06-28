unit Goccia.Values.NumberObjectValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaNumberObjectValue = class(TGocciaInstanceValue)
  private
    FPrimitive: TGocciaNumberLiteralValue;

    function ExtractPrimitive(const AValue: TGocciaValue): TGocciaNumberLiteralValue;
  public
    constructor Create(const APrimitive: TGocciaNumberLiteralValue; const AClass: TGocciaClassValue = nil);
    function ToStringTag: string; override;
    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;
    procedure InitializePrototype;
    procedure MarkReferences; override;

    class function GetSharedPrototype: TGocciaObjectValue;

    property Primitive: TGocciaNumberLiteralValue read FPrimitive;

    // Number prototype methods
  public
    function NumberToFixed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function NumberToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function NumberValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function NumberToPrecision(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function NumberToExponential(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  Math,
  SysUtils,

  BigInteger,

  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Realm,
  Goccia.ThreadCleanupRegistry,
  Goccia.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction;

// Number.prototype lives in a per-realm slot.  Method host and member
// definitions stay process-wide (immutable across realms).
var
  GNumberPrototypeSlot: TGocciaRealmSlotId;

threadvar
  FPrototypeMethodHost: TGocciaNumberObjectValue;
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

procedure ClearThreadvarMembers;
begin
  SetLength(FPrototypeMembers, 0);
end;

const
  DOUBLE_EXPONENT_BIAS = 1023;
  DOUBLE_EXPONENT_BITS_MASK = $7FF;
  DOUBLE_FRACTION_BITS = 52;
  DOUBLE_FRACTION_BITS_MASK: QWord = $000FFFFFFFFFFFFF;
  DOUBLE_HIDDEN_BIT: QWord = $0010000000000000;
  DECIMAL_EXPONENT_FIXED_THRESHOLD = -6;
  DECIMAL_RADIX = 10;

function BigIntPower10(const AExponent: Integer): TBigInteger;
var
  Base: TBigInteger;
  Exponent: Integer;
begin
  Result := TBigInteger.One;
  if AExponent <= 0 then
    Exit;

  Base := TBigInteger.FromInt64(DECIMAL_RADIX);
  Exponent := AExponent;
  while Exponent > 0 do
  begin
    if (Exponent and 1) = 1 then
      Result := Result.Multiply(Base);
    Exponent := Exponent shr 1;
    if Exponent > 0 then
      Base := Base.Multiply(Base);
  end;
end;

function CompareBinaryRationalToPowerOfTen(const ANumerator,
  ADenominator: TBigInteger; const ADecimalExponent: Integer): Integer;
var
  LeftValue, RightValue: TBigInteger;
begin
  if ADecimalExponent >= 0 then
  begin
    LeftValue := ANumerator;
    RightValue := ADenominator.Multiply(BigIntPower10(ADecimalExponent));
  end
  else
  begin
    LeftValue := ANumerator.Multiply(BigIntPower10(-ADecimalExponent));
    RightValue := ADenominator;
  end;
  Result := LeftValue.Compare(RightValue);
end;

function PadLeftWithZeros(const AValue: string; const AWidth: Integer): string;
begin
  Result := AValue;
  if Length(Result) < AWidth then
    Result := StringOfChar('0', AWidth - Length(Result)) + Result;
end;

procedure PositiveDoubleToBinaryRational(const AValue: Double;
  out ANumerator, ADenominator: TBigInteger);
var
  BinaryExponent: Integer;
  Bits, Mantissa: QWord;
  ExponentBits: QWord;
begin
  Move(AValue, Bits, SizeOf(Bits));
  ExponentBits := (Bits shr DOUBLE_FRACTION_BITS) and DOUBLE_EXPONENT_BITS_MASK;
  Mantissa := Bits and DOUBLE_FRACTION_BITS_MASK;
  if ExponentBits = 0 then
    BinaryExponent := 1 - DOUBLE_EXPONENT_BIAS - DOUBLE_FRACTION_BITS
  else
  begin
    Mantissa := Mantissa or DOUBLE_HIDDEN_BIT;
    BinaryExponent := Integer(ExponentBits) - DOUBLE_EXPONENT_BIAS -
      DOUBLE_FRACTION_BITS;
  end;

  ANumerator := TBigInteger.FromInt64(Int64(Mantissa));
  ADenominator := TBigInteger.One;
  if BinaryExponent >= 0 then
    ANumerator := ANumerator.ShiftLeft(BinaryExponent)
  else
    ADenominator := ADenominator.ShiftLeft(-BinaryExponent);
end;

function DecimalExponentForBinaryRational(const ANumerator,
  ADenominator: TBigInteger; const AApproximation: Double): Integer;
begin
  Result := Trunc(Math.Floor(Math.Log10(AApproximation)));
  while CompareBinaryRationalToPowerOfTen(ANumerator, ADenominator, Result) < 0 do
    Dec(Result);
  while CompareBinaryRationalToPowerOfTen(ANumerator, ADenominator, Result + 1) >= 0 do
    Inc(Result);
end;

function RoundedPrecisionDigits(const ANumerator, ADenominator: TBigInteger;
  const APrecision: Integer; var ADecimalExponent: Integer): string;
var
  DecimalScale: Integer;
  Quotient, Remainder, ScaledDenominator, ScaledNumerator: TBigInteger;
begin
  DecimalScale := APrecision - 1 - ADecimalExponent;
  if DecimalScale >= 0 then
  begin
    ScaledNumerator := ANumerator.Multiply(BigIntPower10(DecimalScale));
    ScaledDenominator := ADenominator;
  end
  else
  begin
    ScaledNumerator := ANumerator;
    ScaledDenominator := ADenominator.Multiply(BigIntPower10(-DecimalScale));
  end;

  Quotient := ScaledNumerator.Divide(ScaledDenominator);
  Remainder := ScaledNumerator.Modulo(ScaledDenominator);
  // ES2026 §21.1.3.5 step 10.a chooses the larger candidate on a tie.
  if Remainder.Multiply(TBigInteger.FromInt64(2)).Compare(ScaledDenominator) >= 0 then
    Quotient := Quotient.Add(TBigInteger.One);

  if Quotient.Compare(BigIntPower10(APrecision)) >= 0 then
  begin
    Quotient := Quotient.Divide(TBigInteger.FromInt64(DECIMAL_RADIX));
    Inc(ADecimalExponent);
  end;

  Result := PadLeftWithZeros(Quotient.ToString, APrecision);
end;

function FormatPrecisionString(const ASign, ADigits: string;
  const ADecimalExponent, APrecision: Integer): string;
var
  Mantissa: string;
begin
  if (ADecimalExponent < DECIMAL_EXPONENT_FIXED_THRESHOLD) or
     (ADecimalExponent >= APrecision) then
  begin
    Mantissa := ADigits;
    if APrecision <> 1 then
      Mantissa := ADigits[1] + '.' + Copy(ADigits, 2, APrecision - 1);
    if ADecimalExponent > 0 then
      Exit(ASign + Mantissa + 'e+' + IntToStr(ADecimalExponent));
    Exit(ASign + Mantissa + 'e-' + IntToStr(-ADecimalExponent));
  end;

  if ADecimalExponent = APrecision - 1 then
    Exit(ASign + ADigits);

  if ADecimalExponent >= 0 then
    Exit(ASign + Copy(ADigits, 1, ADecimalExponent + 1) + '.' +
      Copy(ADigits, ADecimalExponent + 2, APrecision - ADecimalExponent - 1));

  Result := ASign + '0.' + StringOfChar('0', -(ADecimalExponent + 1)) + ADigits;
end;

function FormatDoubleToPrecision(const AValue: Double;
  const APrecision: Integer): string;
var
  DecimalExponent: Integer;
  Denominator, Numerator: TBigInteger;
  Digits, Sign: string;
  Value: Double;
begin
  Value := AValue;
  if Value < 0 then
  begin
    Sign := '-';
    Value := -Value;
  end
  else
    Sign := '';

  if Value = 0 then
  begin
    Digits := StringOfChar('0', APrecision);
    Exit(FormatPrecisionString(Sign, Digits, 0, APrecision));
  end;

  PositiveDoubleToBinaryRational(Value, Numerator, Denominator);
  DecimalExponent := DecimalExponentForBinaryRational(Numerator,
    Denominator, Value);
  Digits := RoundedPrecisionDigits(Numerator, Denominator, APrecision,
    DecimalExponent);
  Result := FormatPrecisionString(Sign, Digits, DecimalExponent, APrecision);
end;

function GetSharedNumberPrototype: TGocciaObjectValue; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaObjectValue(CurrentRealm.GetSlot(GNumberPrototypeSlot))
  else
    Result := nil;
end;

// ES2026 §21.1.3 thisNumberValue(value)
function TGocciaNumberObjectValue.ExtractPrimitive(const AValue: TGocciaValue): TGocciaNumberLiteralValue;
begin
  if AValue is TGocciaNumberLiteralValue then
    Result := TGocciaNumberLiteralValue(AValue)
  else if AValue is TGocciaNumberObjectValue then
    Result := TGocciaNumberObjectValue(AValue).Primitive
  else
    ThrowTypeError(SErrorNotANumber, SSuggestNotANumber);
end;

constructor TGocciaNumberObjectValue.Create(const APrimitive: TGocciaNumberLiteralValue; const AClass: TGocciaClassValue = nil);
var
  SharedPrototype: TGocciaObjectValue;
begin
  inherited Create(AClass);
  FPrimitive := APrimitive;
  InitializePrototype;
  SharedPrototype := GetSharedNumberPrototype;
  if not Assigned(AClass) and Assigned(SharedPrototype) then
    FPrototype := SharedPrototype;
end;

function TGocciaNumberObjectValue.ToStringTag: string;
begin
  Result := 'Number';
end;

function TGocciaNumberObjectValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaNumberObjectValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
var
  SharedPrototype: TGocciaObjectValue;
begin
  Result := inherited GetPropertyWithContext(AName, AThisContext);
  if not (Result is TGocciaUndefinedLiteralValue) then
    Exit;

  SharedPrototype := GetSharedNumberPrototype;
  if Assigned(SharedPrototype) then
    Result := SharedPrototype.GetPropertyWithContext(AName, AThisContext);
end;

procedure TGocciaNumberObjectValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  SharedPrototype: TGocciaObjectValue;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetSharedNumberPrototype) then Exit;

  SharedPrototype := TGocciaObjectValue.Create;
  CurrentRealm.SetSlot(GNumberPrototypeSlot, SharedPrototype);
  if Length(FPrototypeMembers) = 0 then
  begin
    FPrototypeMethodHost := Self;
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddNamedMethod('toFixed', NumberToFixed, 1);
      Members.AddNamedMethod(PROP_TO_STRING, NumberToString, 1);
      Members.AddNamedMethod(PROP_VALUE_OF, NumberValueOf, 0);
      Members.AddNamedMethod('toPrecision', NumberToPrecision, 1);
      Members.AddNamedMethod('toExponential', NumberToExponential, 1);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
    // Method host is a process-wide singleton; pin it once so cached
    // FPrototypeMembers callbacks remain valid across realms.
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FPrototypeMethodHost);
  end;
  RegisterMemberDefinitions(SharedPrototype, FPrototypeMembers);
end;

class function TGocciaNumberObjectValue.GetSharedPrototype: TGocciaObjectValue;
begin
  if not Assigned(GetSharedNumberPrototype) then
    TGocciaNumberObjectValue.Create(TGocciaNumberLiteralValue.ZeroValue);
  Result := GetSharedNumberPrototype;
end;

procedure TGocciaNumberObjectValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FPrimitive) then
    FPrimitive.MarkReferences;
end;

// ES2026 §21.1.3.3 Number.prototype.toFixed(fractionDigits)
function TGocciaNumberObjectValue.NumberToFixed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Prim: TGocciaNumberLiteralValue;
  Digits: Integer;
begin
  // Step 1: Let x be ? thisNumberValue(this value)
  Prim := ExtractPrimitive(AThisValue);

  // Step 2: Let f be ? ToIntegerOrInfinity(fractionDigits)
  if AArgs.Length > 0 then
    Digits := ToIntegerFromArgs(AArgs, 0)
  else
    Digits := 0;

  // Step 4: If f < 0 or f > 100, throw a RangeError exception
  // (precedes the non-finite returns below, so (NaN).toFixed(101) throws)
  if (Digits < 0) or (Digits > 100) then
    ThrowRangeError(SErrorToFixedArgRange, SSuggestNumberRange);

  // Step 5: If x is not finite, return Number::toString(x, 10)
  if Prim.IsNaN or Prim.IsInfinity or Prim.IsNegativeInfinity then
  begin
    Result := Prim.ToStringLiteral;
    Exit;
  end;

  // Step 6: Format x with exactly f digits after the decimal point
  Result := TGocciaStringLiteralValue.Create(FormatFloat('0.' + StringOfChar('0', Digits), Prim.Value, InvariantFormatSettings));
end;

// Convert a finite non-negative integer-valued Double to its base-ARadix
// digit sequence, using 0-9 and a-z (lowercase) for digit values 10-35.
// Uses Double arithmetic so any finite Number (including those above
// Int64.MaxValue or 2^53) gets a sensible representation; doubles past 2^53
// are necessarily aligned to powers of 2 and the trailing digits reflect that
// alignment per ES2026 §6.1.6.1.20's "implementation-defined" clause.
function IntegerToRadixString(const AValue: Double; const ARadix: Integer): string;
const
  DIGITS: array[0..35] of Char =
    ('0','1','2','3','4','5','6','7','8','9',
     'a','b','c','d','e','f','g','h','i','j',
     'k','l','m','n','o','p','q','r','s','t',
     'u','v','w','x','y','z');
var
  V, Q: Double;
  D: Integer;
begin
  if AValue = 0 then
    Exit('0');
  V := AValue;
  Result := '';
  while V > 0 do
  begin
    // Use Int() rather than Trunc() throughout — Trunc returns Int64 and
    // overflows on doubles past 2^63 (e.g. (1e100).toString(16)). Int()
    // returns the truncated value as Double so arbitrarily large finite
    // numbers stringify; Round() narrows just the small-modulo digit.
    Q := Int(V / ARadix);
    D := Round(V - Q * ARadix);
    if D < 0 then D := 0;
    if D > ARadix - 1 then D := ARadix - 1;
    Result := DIGITS[D] + Result;
    V := Q;
  end;
end;

// Convert a positive finite fractional value (0 < AValue < 1) to its base-ARadix
// representation. The digit budget is sized to capture the binary64 mantissa
// in any radix (53 / Log2(R), rounded up, with headroom) so common values like
// (0.1).toString(2) emit enough binary digits to round-trip within double
// precision — full subnormal range (which can need ~1074 binary digits) is
// out of scope and tracked separately.
function FractionToRadixString(const AValue: Double; const ARadix: Integer): string;
const
  DIGITS: array[0..35] of Char =
    ('0','1','2','3','4','5','6','7','8','9',
     'a','b','c','d','e','f','g','h','i','j',
     'k','l','m','n','o','p','q','r','s','t',
     'u','v','w','x','y','z');
var
  V: Double;
  Digit, I, LastNonZero, MaxDigits: Integer;
begin
  Result := '';
  V := AValue;
  // Ceil(53 / Log2(ARadix)) — enough for binary64 mantissa precision in any
  // base 2..36.  Add a small constant headroom for the rounding-into-the-next
  // place behaviour at the boundary.  53/Ln(2) ≈ 76, divided by Ln(R)/Ln(2).
  MaxDigits := Trunc(53.0 / (Ln(ARadix) / Ln(2.0))) + 4;
  if MaxDigits < 16 then MaxDigits := 16;
  for I := 1 to MaxDigits do
  begin
    V := V * ARadix;
    Digit := Trunc(V);
    if Digit < 0 then Digit := 0;
    if Digit > ARadix - 1 then Digit := ARadix - 1;
    Result := Result + DIGITS[Digit];
    V := V - Digit;
    if V = 0 then Break;
  end;
  // Trim trailing zeros — a base-N fraction terminates exactly at the
  // last non-zero digit.
  LastNonZero := Length(Result);
  while (LastNonZero > 0) and (Result[LastNonZero] = '0') do
    Dec(LastNonZero);
  SetLength(Result, LastNonZero);
end;

// ES2026 §21.1.3.6 Number.prototype.toString([radix])
function TGocciaNumberObjectValue.NumberToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Prim: TGocciaNumberLiteralValue;
  Radix: Integer;
  V, IntPart, FracPart: Double;
  IntStr, FracStr, Sign: string;
begin
  // Step 1: Let x be ? thisNumberValue(this value)
  Prim := ExtractPrimitive(AThisValue);

  // Special values always return their default string regardless of radix
  if Prim.IsNaN or Prim.IsInfinity or Prim.IsNegativeInfinity or Prim.IsNegativeZero then
  begin
    Result := Prim.ToStringLiteral;
    Exit;
  end;

  // Step 2: If radix is undefined, let radixMV be 10
  if (AArgs.Length > 0) and not (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
  begin
    Radix := ToIntegerFromArgs(AArgs, 0);
    // Step 3: If radixMV < 2 or radixMV > 36, throw a RangeError exception
    if (Radix < 2) or (Radix > 36) then
      ThrowRangeError(SErrorBigIntInvalidRadix, SSuggestBigIntInvalidRadix);
    if Radix = 10 then
    begin
      Result := Prim.ToStringLiteral;
      Exit;
    end;
    // Step 4: Return the String representation of x in the given radix.
    // ES2026 §6.1.6.1.20 Number::toString — implementation-defined for
    // non-integer values, but spec asserts the result must be a "generalization"
    // of the decimal algorithm: sign + integer-digits + optional '.' + fraction.
    V := Prim.Value;
    if V < 0 then
    begin
      Sign := '-';
      V := -V;
    end
    else
      Sign := '';
    // Int() returns the truncated value as Double so V > Int64.MaxValue
    // doesn't trip a range check before IntegerToRadixString sees it.
    IntPart := Int(V);
    FracPart := V - IntPart;
    IntStr := IntegerToRadixString(IntPart, Radix);
    if FracPart > 0 then
    begin
      FracStr := FractionToRadixString(FracPart, Radix);
      if FracStr <> '' then
        Result := TGocciaStringLiteralValue.Create(Sign + IntStr + '.' + FracStr)
      else
        Result := TGocciaStringLiteralValue.Create(Sign + IntStr);
    end
    else
      Result := TGocciaStringLiteralValue.Create(Sign + IntStr);
  end
  else
    // Step 2: If radix is undefined or 10, return ! ToString(x)
    Result := Prim.ToStringLiteral;
end;

// ES2026 §21.1.3.7 Number.prototype.valueOf()
function TGocciaNumberObjectValue.NumberValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Return ? thisNumberValue(this value)
  Result := ExtractPrimitive(AThisValue);
end;

// ES2026 §21.1.3.5 Number.prototype.toPrecision(precision)
function TGocciaNumberObjectValue.NumberToPrecision(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Prim: TGocciaNumberLiteralValue;
  Precision: Integer;
begin
  // Step 1: Let x be ? thisNumberValue(this value)
  Prim := ExtractPrimitive(AThisValue);

  // Step 2: If precision is undefined, return ! ToString(x)
  if (AArgs.Length = 0) or (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
  begin
    Result := Prim.ToStringLiteral;
    Exit;
  end;

  // Step 3: Let p be ? ToIntegerOrInfinity(precision)
  Precision := ToIntegerFromArgs(AArgs, 0);

  // Step 4: If x is not finite, return Number::toString(x, 10)
  if Prim.IsNaN or Prim.IsInfinity or Prim.IsNegativeInfinity then
  begin
    Result := Prim.ToStringLiteral;
    Exit;
  end;

  // Step 5: If p < 1 or p > 100, throw a RangeError exception
  if (Precision < 1) or (Precision > 100) then
    ThrowRangeError(SErrorToPrecisionArgRange, SSuggestNumberRange);

  // Steps 6-15: Format x with p significant digits.
  Result := TGocciaStringLiteralValue.Create(FormatDoubleToPrecision(Prim.Value,
    Precision));
end;

// ES2026 §21.1.3.2 Number.prototype.toExponential(fractionDigits)
function TGocciaNumberObjectValue.NumberToExponential(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Prim: TGocciaNumberLiteralValue;
  FractionDigits, Exp: Integer;
  HasExplicitDigits: Boolean;
  NumVal, Mantissa: Double;
  Sign, MantissaStr, ExpSign: string;
begin
  // Step 1: Let x be ? thisNumberValue(this value)
  Prim := ExtractPrimitive(AThisValue);

  // Steps 3-4: If x is not finite, return Number::toString(x, 10)
  if Prim.IsNaN or Prim.IsInfinity or Prim.IsNegativeInfinity then
  begin
    Result := Prim.ToStringLiteral;
    Exit;
  end;

  // Step 2: Let f be ? ToIntegerOrInfinity(fractionDigits)
  HasExplicitDigits := (AArgs.Length > 0) and not (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue);
  if HasExplicitDigits then
  begin
    FractionDigits := ToIntegerFromArgs(AArgs, 0);
    // Step 5: If f < 0 or f > 100, throw a RangeError exception
    if (FractionDigits < 0) or (FractionDigits > 100) then
      ThrowRangeError(SErrorToExponentialArgRange, SSuggestNumberRange);
  end
  else
    FractionDigits := -1;

  // Step 6: If x < 0, let s be "-" and let x be -x; otherwise let s be ""
  NumVal := Prim.Value;
  Sign := '';
  if NumVal < 0 then
  begin
    Sign := '-';
    NumVal := -NumVal;
  end;

  // Step 7: If x = 0, the mantissa string is "0" (with optional trailing zeros)
  if NumVal = 0 then
  begin
    if FractionDigits < 0 then
      FractionDigits := 0;
    if FractionDigits = 0 then
      MantissaStr := '0'
    else
      MantissaStr := '0.' + StringOfChar('0', FractionDigits);
    Result := TGocciaStringLiteralValue.Create(Sign + MantissaStr + 'e+0');
    Exit;
  end;

  // Step 8: Compute exponent e such that 10^e ≤ x < 10^(e+1)
  Exp := Trunc(Math.Floor(Math.Log10(NumVal)));
  // Step 9: Compute mantissa m = x / 10^e
  Mantissa := NumVal / Math.Power(10, Exp);

  // Step 10: Format mantissa with f fraction digits
  if FractionDigits < 0 then
  begin
    MantissaStr := FloatToStrF(Mantissa, ffGeneral, 15, 0, InvariantFormatSettings);
    if Pos('.', MantissaStr) = 0 then
      MantissaStr := MantissaStr;
  end
  else
  begin
    MantissaStr := FormatFloat('0.' + StringOfChar('0', FractionDigits), Mantissa, InvariantFormatSettings);
    if Pos('.', MantissaStr) > 2 then
    begin
      Mantissa := Mantissa / 10;
      Inc(Exp);
      MantissaStr := FormatFloat('0.' + StringOfChar('0', FractionDigits), Mantissa, InvariantFormatSettings);
    end;
  end;

  // Step 11: Build the exponent sign and return s + m + "e" + expSign + e
  if Exp >= 0 then
    ExpSign := '+'
  else
    ExpSign := '';

  Result := TGocciaStringLiteralValue.Create(Sign + MantissaStr + 'e' + ExpSign + IntToStr(Exp));
end;

initialization
  GNumberPrototypeSlot := RegisterRealmSlot('Number.prototype');
  RegisterThreadvarCleanup(@ClearThreadvarMembers);

end.
