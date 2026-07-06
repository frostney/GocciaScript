unit Goccia.Builtins.Math;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaMath = class(TGocciaBuiltin)
  published
    function MathAbs(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathFloor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathCeil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathRound(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathMax(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathMin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathPow(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathSqrt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathRandom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathClamp(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathSign(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathTrunc(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathExp(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathLog(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathLog10(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathSin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathCos(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathTan(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathAcos(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathAsin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathAtan(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathAtan2(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathCbrt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathCosh(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathSinh(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathTanh(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathAcosh(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathAsinh(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathAtanh(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathExpm1(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathF16round(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathFround(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathHypot(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathImul(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathLog1p(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathLog2(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathClz32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathSumPrecise(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  Math,
  SysUtils,

  BigInteger,

  Goccia.Arguments.Converter,
  Goccia.Arguments.Validator,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Float16,
  Goccia.GarbageCollector,
  Goccia.ThreadCleanupRegistry,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.IteratorSupport,
  Goccia.Values.IteratorValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

const
  DOUBLE_SIGN_MASK = QWord($8000000000000000);
  DOUBLE_EXP_MASK  = QWord($7FF0000000000000);
  DOUBLE_FRAC_MASK = QWord($000FFFFFFFFFFFFF);
  DOUBLE_HIDDEN_BIT = QWord($0010000000000000);
  DOUBLE_EXP_BIAS = 1023;
  DOUBLE_FRAC_BITS = 52;
  DOUBLE_MIN_NORMAL_EXPONENT = -1022;
  DOUBLE_MIN_SUBNORMAL_EXPONENT = -1074;
  DOUBLE_MAX_EXPONENT = 1023;
  DOUBLE_INTEGRAL_LIMIT = 4503599627370496.0; // 2^52

threadvar
  FStaticMembers: TArray<TGocciaMemberDefinition>;

{$IFDEF DARWIN}
function LibMSinh(const AValue: Double): Double; cdecl; external name 'sinh';
function LibMTanh(const AValue: Double): Double; cdecl; external name 'tanh';
function LibMAsinh(const AValue: Double): Double; cdecl; external name 'asinh';
function LibMAtanh(const AValue: Double): Double; cdecl; external name 'atanh';
{$ENDIF}
{$IFDEF LINUX}
function LibMSinh(const AValue: Double): Double; cdecl; external 'm' name 'sinh';
function LibMTanh(const AValue: Double): Double; cdecl; external 'm' name 'tanh';
function LibMAsinh(const AValue: Double): Double; cdecl; external 'm' name 'asinh';
function LibMAtanh(const AValue: Double): Double; cdecl; external 'm' name 'atanh';
{$ENDIF}

function IsFiniteIntegral(const AValue: Double): Boolean; inline;
begin
  Result := (not IsNan(AValue)) and (not IsInfinite(AValue)) and
    ((Abs(AValue) >= DOUBLE_INTEGRAL_LIMIT) or (Frac(AValue) = 0));
end;

function CubeRootApprox(const AValue: Double): Double;
var
  AbsValue, Guess: Double;
  I: Integer;
begin
  if AValue = 0 then
    Exit(AValue);
  AbsValue := Abs(AValue);
  Guess := Power(AbsValue, 1.0 / 3.0);
  for I := 0 to 2 do
    Guess := (2.0 * Guess + AbsValue / (Guess * Guess)) / 3.0;
  if AValue < 0 then
    Result := -Guess
  else
    Result := Guess;
end;

function Expm1Approx(const AValue: Double): Double;
var
  Term: Double;
  N: Integer;
begin
  if Abs(AValue) >= 0.5 then
    Exit(Exp(AValue) - 1.0);
  Result := AValue;
  Term := AValue;
  for N := 2 to 48 do
  begin
    Term := Term * AValue / N;
    Result := Result + Term;
    if Abs(Term) <= Abs(Result) * 1.0E-17 then
      Break;
  end;
end;

function Log1pApprox(const AValue: Double): Double;
var
  X2, X3, X4, X5, X6: Double;
begin
  if Abs(AValue) >= 1.0E-3 then
    Exit(Ln(1.0 + AValue));
  X2 := AValue * AValue;
  X3 := X2 * AValue;
  X4 := X3 * AValue;
  X5 := X4 * AValue;
  X6 := X5 * AValue;
  Result := AValue - X2 / 2.0 + X3 / 3.0 - X4 / 4.0 +
    X5 / 5.0 - X6 / 6.0;
end;

function AcoshApprox(const AValue: Double): Double;
var
  Delta: Double;
  Root: Double;
begin
  Delta := AValue - 1.0;
  if Delta = 0 then
    Result := 0
  else if Delta < 1.0E-3 then
  begin
    Root := Sqrt(2.0 * Delta);
    Result := Root * (1.0 - Delta / 12.0 + 3.0 * Delta * Delta / 160.0 -
      5.0 * Delta * Delta * Delta / 896.0);
  end
  else if AValue > 1.0E154 then
    Result := Ln(AValue) + Ln(2.0)
  else
    Result := Ln(AValue + Sqrt(AValue - 1.0) * Sqrt(AValue + 1.0));
end;

function AsinhApprox(const AValue: Double): Double;
var
  AbsValue, Inner, Term, X2: Double;
  N: Integer;
begin
  {$IFDEF DARWIN}
  Exit(LibMAsinh(AValue));
  {$ENDIF}
  {$IFDEF LINUX}
  Exit(LibMAsinh(AValue));
  {$ENDIF}

  if AValue = 0 then
    Exit(AValue);
  AbsValue := Abs(AValue);
  if AbsValue < 0.5 then
  begin
    Result := AValue;
    Term := AValue;
    X2 := AValue * AValue;
    for N := 1 to 80 do
    begin
      Term := -Term * X2 * Sqr(2 * N - 1) / ((2 * N) * (2 * N + 1));
      Result := Result + Term;
      if Abs(Term) <= Abs(Result) * 1.0E-17 then
        Break;
    end;
    Exit;
  end;
  if AbsValue > 1.0E154 then
    Inner := Ln(AbsValue) + Ln(2.0)
  else
    Inner := ArcSinh(AbsValue);
  if AValue < 0 then
    Result := -Inner
  else
    Result := Inner;
end;

function AtanhApprox(const AValue: Double): Double;
var
  X2, Term: Double;
  Denominator: Integer;
begin
  {$IFDEF DARWIN}
  Exit(LibMAtanh(AValue));
  {$ENDIF}
  {$IFDEF LINUX}
  Exit(LibMAtanh(AValue));
  {$ENDIF}

  if AValue = 0 then
    Exit(AValue);
  if Abs(AValue) < 0.5 then
  begin
    Result := AValue;
    Term := AValue;
    X2 := AValue * AValue;
    Denominator := 3;
    while Denominator <= 161 do
    begin
      Term := Term * X2;
      Result := Result + Term / Denominator;
      if Abs(Term / Denominator) <= Abs(Result) * 1.0E-17 then
        Break;
      Inc(Denominator, 2);
    end;
    Exit;
  end;
  Result := ArcTanh(AValue);
end;

function TanhApprox(const AValue: Double): Double;
var
  E: Double;
begin
  {$IFDEF DARWIN}
  Exit(LibMTanh(AValue));
  {$ENDIF}
  {$IFDEF LINUX}
  Exit(LibMTanh(AValue));
  {$ENDIF}

  if AValue = 0 then
    Exit(AValue);
  if AValue > 20 then
    Exit(1.0);
  if AValue < -20 then
    Exit(-1.0);
  if Abs(AValue) < 0.25 then
  begin
    E := Expm1Approx(2.0 * AValue);
    Exit(E / (E + 2.0));
  end;
  Result := Tanh(AValue);
end;

function SinhApprox(const AValue: Double): Double;
var
  Term, X2: Double;
  N: Integer;
begin
  {$IFDEF DARWIN}
  Exit(LibMSinh(AValue));
  {$ENDIF}
  {$IFDEF LINUX}
  Exit(LibMSinh(AValue));
  {$ENDIF}

  if AValue = 0 then
    Exit(AValue);
  if Abs(AValue) >= 0.125 then
    Exit(Sinh(AValue));
  Result := AValue;
  Term := AValue;
  X2 := AValue * AValue;
  for N := 1 to 24 do
  begin
    Term := Term * X2 / ((2 * N) * (2 * N + 1));
    Result := Result + Term;
    if Abs(Term) <= Abs(Result) * 1.0E-17 then
      Break;
  end;
end;

function DecomposeFiniteDouble(const AValue: Double; out AMantissa: TBigInteger;
  out AExponent: Integer): Boolean;
var
  Bits, Fraction: QWord;
  RawExponent: Integer;
begin
  Bits := PQWord(@AValue)^;
  Fraction := Bits and DOUBLE_FRAC_MASK;
  RawExponent := Integer((Bits and DOUBLE_EXP_MASK) shr DOUBLE_FRAC_BITS);
  if RawExponent = 0 then
  begin
    if Fraction = 0 then
      Exit(False);
    AMantissa := TBigInteger.FromInt64(Int64(Fraction));
    AExponent := DOUBLE_MIN_SUBNORMAL_EXPONENT;
  end
  else
  begin
    AMantissa := TBigInteger.FromInt64(Int64(DOUBLE_HIDDEN_BIT or Fraction));
    AExponent := RawExponent - DOUBLE_EXP_BIAS - DOUBLE_FRAC_BITS;
  end;
  if (Bits and DOUBLE_SIGN_MASK) <> 0 then
    AMantissa := AMantissa.Negate;
  Result := True;
end;

function RoundShiftRightToEven(const AValue: TBigInteger;
  const AShift: Integer): TBigInteger;
var
  Divisor, Quotient, Remainder, TwiceRemainder: TBigInteger;
  Cmp: Integer;
begin
  if AShift <= 0 then
    Exit(AValue.ShiftLeft(-AShift));
  Quotient := AValue.ShiftRight(AShift);
  Remainder := AValue.Subtract(Quotient.ShiftLeft(AShift));
  if Remainder.IsZero then
    Exit(Quotient);
  Divisor := TBigInteger.One.ShiftLeft(AShift);
  TwiceRemainder := Remainder.ShiftLeft(1);
  Cmp := TwiceRemainder.Compare(Divisor);
  if (Cmp > 0) or ((Cmp = 0) and Quotient.GetBit(0)) then
    Quotient := Quotient.Add(TBigInteger.One);
  Result := Quotient;
end;

function ExactScaledIntegerToDouble(const AValue: TBigInteger;
  const AExponent: Integer): Double;
var
  Negative: Boolean;
  Magnitude, Significand: TBigInteger;
  Exponent, Shift: Integer;
begin
  if AValue.IsZero then
    Exit(0.0);

  Negative := AValue.IsNegative;
  Magnitude := AValue.AbsValue;
  Exponent := Magnitude.BitLength - 1 + AExponent;

  if Exponent > DOUBLE_MAX_EXPONENT then
  begin
    if Negative then
      Exit(NegInfinity);
    Exit(Infinity);
  end;

  if Exponent >= DOUBLE_MIN_NORMAL_EXPONENT then
  begin
    Shift := Exponent - DOUBLE_FRAC_BITS - AExponent;
    Significand := RoundShiftRightToEven(Magnitude, Shift);
    if Significand.BitLength > DOUBLE_FRAC_BITS + 1 then
    begin
      Significand := Significand.ShiftRight(1);
      Inc(Exponent);
      if Exponent > DOUBLE_MAX_EXPONENT then
      begin
        if Negative then
          Exit(NegInfinity);
        Exit(Infinity);
      end;
    end;
    Result := Ldexp(Significand.ToDouble, Exponent - DOUBLE_FRAC_BITS);
  end
  else
  begin
    Shift := DOUBLE_MIN_SUBNORMAL_EXPONENT - AExponent;
    Significand := RoundShiftRightToEven(Magnitude, Shift);
    if Significand.IsZero then
      Result := 0.0
    else
      Result := Ldexp(Significand.ToDouble, DOUBLE_MIN_SUBNORMAL_EXPONENT);
  end;

  if Negative then
    Result := -Result;
end;

procedure ClearThreadvarMembers;
begin
  SetLength(FStaticMembers, 0);
end;

{ TGocciaMath }

constructor TGocciaMath.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  Members: TGocciaMemberCollection;
begin
  inherited Create(AName, AScope, AThrowError);

  // Constants: non-writable, non-enumerable, non-configurable
  FBuiltinObject.RegisterConstant('PI', TGocciaNumberLiteralValue.Create(Pi));
  FBuiltinObject.RegisterConstant('E', TGocciaNumberLiteralValue.Create(Exp(1)));
  FBuiltinObject.RegisterConstant('LN2', TGocciaNumberLiteralValue.Create(Ln(2)));
  FBuiltinObject.RegisterConstant('LN10', TGocciaNumberLiteralValue.Create(Ln(10)));
  FBuiltinObject.RegisterConstant('SQRT2', TGocciaNumberLiteralValue.Create(Sqrt(2)));
  FBuiltinObject.RegisterConstant('LOG2E', TGocciaNumberLiteralValue.Create(1 / Ln(2)));
  FBuiltinObject.RegisterConstant('LOG10E', TGocciaNumberLiteralValue.Create(1 / Ln(10)));
  FBuiltinObject.RegisterConstant('SQRT1_2', TGocciaNumberLiteralValue.Create(Sqrt(0.5)));

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddMethod(MathAbs, 1, gmkStaticMethod);
    Members.AddMethod(MathFloor, 1, gmkStaticMethod);
    Members.AddMethod(MathCeil, 1, gmkStaticMethod);
    Members.AddMethod(MathRound, 1, gmkStaticMethod);
    Members.AddMethod(MathMax, 2, gmkStaticMethod);
    Members.AddMethod(MathMin, 2, gmkStaticMethod);
    Members.AddMethod(MathPow, 2, gmkStaticMethod);
    Members.AddMethod(MathSqrt, 1, gmkStaticMethod);
    Members.AddMethod(MathRandom, 0, gmkStaticMethod);
    Members.AddMethod(MathClamp, 3, gmkStaticMethod);
    Members.AddMethod(MathSign, 1, gmkStaticMethod);
    Members.AddMethod(MathTrunc, 1, gmkStaticMethod);
    Members.AddMethod(MathExp, 1, gmkStaticMethod);
    Members.AddMethod(MathLog, 1, gmkStaticMethod);
    Members.AddMethod(MathLog10, 1, gmkStaticMethod);
    Members.AddMethod(MathSin, 1, gmkStaticMethod);
    Members.AddMethod(MathCos, 1, gmkStaticMethod);
    Members.AddMethod(MathTan, 1, gmkStaticMethod);
    Members.AddMethod(MathAcos, 1, gmkStaticMethod);
    Members.AddMethod(MathAsin, 1, gmkStaticMethod);
    Members.AddMethod(MathAtan, 1, gmkStaticMethod);
    Members.AddMethod(MathAtan2, 2, gmkStaticMethod);
    Members.AddMethod(MathCbrt, 1, gmkStaticMethod);
    Members.AddMethod(MathCosh, 1, gmkStaticMethod);
    Members.AddMethod(MathSinh, 1, gmkStaticMethod);
    Members.AddMethod(MathTanh, 1, gmkStaticMethod);
    Members.AddMethod(MathAcosh, 1, gmkStaticMethod);
    Members.AddMethod(MathAsinh, 1, gmkStaticMethod);
    Members.AddMethod(MathAtanh, 1, gmkStaticMethod);
    Members.AddMethod(MathExpm1, 1, gmkStaticMethod);
    Members.AddMethod(MathF16round, 1, gmkStaticMethod);
    Members.AddMethod(MathFround, 1, gmkStaticMethod);
    Members.AddMethod(MathHypot, 2, gmkStaticMethod);
    Members.AddMethod(MathImul, 2, gmkStaticMethod);
    Members.AddMethod(MathLog1p, 1, gmkStaticMethod);
    Members.AddMethod(MathLog2, 1, gmkStaticMethod);
    Members.AddMethod(MathClz32, 1, gmkStaticMethod);
    Members.AddMethod(MathSumPrecise, 1, gmkStaticMethod);
    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create('Math'),
      [pfConfigurable]);
    FStaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(FBuiltinObject, FStaticMembers);

  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet, True);
end;

// §21.3.2.1 Math.abs ( x )
function TGocciaMath.MathAbs(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: If n is NaN, return NaN.
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  // Step 3: If n is -∞𝔽 or +∞𝔽, return +∞𝔽.
  else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  // Step 4: Return abs(n).
  else
    Result := TGocciaNumberLiteralValue.Create(Abs(NumberArg.Value));
end;

// §21.3.2.16 Math.floor ( x )
function TGocciaMath.MathFloor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
  IntegralPart: Double;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := TGocciaArgumentConverter.GetNumber(AArgs, 0);
  // Step 2: If n is NaN, +∞𝔽, -∞𝔽, return n. Signed-zero is preserved
  // (Math.floor(-0𝔽) is -0𝔽, not +0𝔽).
  if NumberArg.IsNaN or NumberArg.IsInfinite or NumberArg.IsNegativeZero then
  begin
    Result := NumberArg;
    Exit;
  end;
  if NumberArg.Value = 0 then
  begin
    Result := NumberArg;
    Exit;
  end;
  // Step 3: Return the greatest (closest to +∞) integral Number ≤ n.
  IntegralPart := Int(NumberArg.Value);
  if (NumberArg.Value < 0) and (IntegralPart <> NumberArg.Value) then
    IntegralPart := IntegralPart - 1;
  Result := TGocciaNumberLiteralValue.Create(IntegralPart);
end;

// §21.3.2.10 Math.ceil ( x )
function TGocciaMath.MathCeil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
  V: Double;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := TGocciaArgumentConverter.GetNumber(AArgs, 0);
  // Step 2: If n is NaN, +∞𝔽, -∞𝔽, or an integral Number, return n.
  if NumberArg.IsNaN or NumberArg.IsInfinite or NumberArg.IsNegativeZero or
     (NumberArg.Value = 0) or IsFiniteIntegral(NumberArg.Value) then
  begin
    Result := NumberArg;
    Exit;
  end;
  // Step 3: If n < +0𝔽 and n > -1𝔽, return -0𝔽 (preserves signed-zero).
  V := NumberArg.Value;
  if (V < 0) and (V > -1) then
  begin
    Result := TGocciaNumberLiteralValue.NegativeZeroValue;
    Exit;
  end;
  // Step 4: Return the smallest (closest to -∞) integral Number ≥ n.
  Result := TGocciaNumberLiteralValue.Create(Ceil(V));
end;

// §21.3.2.28 Math.round ( x )
function TGocciaMath.MathRound(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: If n is NaN, +∞𝔽, -∞𝔽, or an integral Number, return n.
  if NumberArg.IsNaN or NumberArg.IsInfinite or NumberArg.IsNegativeZero or
     (NumberArg.Value = 0) or IsFiniteIntegral(NumberArg.Value) then
  begin
    Result := NumberArg;
    Exit;
  end;
  // Step 3: If n < +0𝔽 and n ≥ -0.5𝔽, return -0𝔽.
  if (NumberArg.Value < 0) and (NumberArg.Value >= -0.5) then
    Result := TGocciaNumberLiteralValue.NegativeZeroValue
  else if (NumberArg.Value > 0) and (NumberArg.Value < 0.5) then
    Result := TGocciaNumberLiteralValue.ZeroValue
  // Step 4: Return floor(n + 0.5).
  else
    Result := TGocciaNumberLiteralValue.Create(Floor(NumberArg.Value + 0.5));
end;

// §21.3.2.24 Math.max ( ...args )
function TGocciaMath.MathMax(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  MaxVal, NumVal: TGocciaNumberLiteralValue;
  Coerced: array of TGocciaNumberLiteralValue;
  HasNaN: Boolean;
begin
  // Step 1: Let coerced be a new empty List.
  // Step 2: For each element arg of args, append ? ToNumber(arg) to coerced.
  // Step 3: If coerced has no elements, return -∞𝔽.
  if AArgs.Length = 0 then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  else
  begin
    SetLength(Coerced, AArgs.Length);
    HasNaN := False;
    for I := 0 to AArgs.Length - 1 do
    begin
      Coerced[I] := AArgs.GetElement(I).ToNumberLiteral;
      if Coerced[I].IsNaN then
        HasNaN := True;
    end;

    // Step 4: If any element is NaN, return NaN after all coercions.
    if HasNaN then
      Exit(TGocciaNumberLiteralValue.NaNValue);

    // Step 5: Return the largest value among the elements of coerced.
    MaxVal := Coerced[0];
    for I := 1 to High(Coerced) do
    begin
      NumVal := Coerced[I];
      if NumVal.IsGreaterThan(MaxVal).Value or
         ((NumVal.Value = 0) and (not NumVal.IsNegativeZero) and
          MaxVal.IsNegativeZero) then
        MaxVal := NumVal;
    end;
    Result := MaxVal;
  end;
end;

// §21.3.2.25 Math.min ( ...args )
function TGocciaMath.MathMin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  MinVal, NumVal: TGocciaNumberLiteralValue;
  Coerced: array of TGocciaNumberLiteralValue;
  HasNaN: Boolean;
begin
  // Step 1: Let coerced be a new empty List.
  // Step 2: For each element arg of args, append ? ToNumber(arg) to coerced.
  // Step 3: If coerced has no elements, return +∞𝔽.
  if AArgs.Length = 0 then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else
  begin
    SetLength(Coerced, AArgs.Length);
    HasNaN := False;
    for I := 0 to AArgs.Length - 1 do
    begin
      Coerced[I] := AArgs.GetElement(I).ToNumberLiteral;
      if Coerced[I].IsNaN then
        HasNaN := True;
    end;

    // Step 4: If any element is NaN, return NaN after all coercions.
    if HasNaN then
      Exit(TGocciaNumberLiteralValue.NaNValue);

    // Step 5: Return the smallest value among the elements of coerced.
    MinVal := Coerced[0];
    for I := 1 to High(Coerced) do
    begin
      NumVal := Coerced[I];
      if NumVal.IsLessThan(MinVal).Value or
         (NumVal.IsNegativeZero and (MinVal.Value = 0) and
          (not MinVal.IsNegativeZero)) then
        MinVal := NumVal;
    end;
    Result := MinVal;
  end;
end;

// §21.3.2.26 Math.pow ( base, exponent ) — calls Number::exponentiate (§6.1.6.1.3)
function TGocciaMath.MathPow(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Base, Exponent: TGocciaNumberLiteralValue;
  B, E, AbsB: Double;
  IntPart: Double;
  ExpIsOddInteger: Boolean;
begin
  Base := TGocciaArgumentConverter.GetNumber(AArgs, 0);
  Exponent := TGocciaArgumentConverter.GetNumber(AArgs, 1);
  B := Base.Value;
  E := Exponent.Value;

  // ES2026 §6.1.6.1.3 Number::exponentiate(base, exponent)
  // 1. If exponent is NaN, return NaN.
  if Exponent.IsNaN then
    Exit(TGocciaNumberLiteralValue.NaNValue);
  // 2. If exponent is either +0 or -0, return 1.
  if E = 0 then
    Exit(TGocciaNumberLiteralValue.Create(1));
  // 3. If base is NaN, return NaN.
  if Base.IsNaN then
    Exit(TGocciaNumberLiteralValue.NaNValue);

  // Pre-compute "exponent is an odd integer" for sign-of-zero / sign-of-Infinity cases.
  // Doubles above 2^53 round each integer position to even precision, so
  // "odd integer" can only be detected up to that magnitude — guard against
  // huge values to avoid Int64 overflow in Round() on values like 1.8e308.
  ExpIsOddInteger := (not Exponent.IsInfinite)
    and (Frac(E) = 0)
    and (Abs(E) < 9007199254740992.0) // 2^53
    and (Round(E) <> Round(E / 2) * 2);

  // 4. If base is +∞, return +∞ if exponent > 0 else +0.
  if Base.IsInfinity then
  begin
    if E > 0 then
      Exit(TGocciaNumberLiteralValue.InfinityValue);
    Exit(TGocciaNumberLiteralValue.Create(0));
  end;
  // 5. If base is -∞:
  //    a. If exponent > 0: -∞ if odd-integer else +∞.
  //    b. If exponent < 0: -0 if odd-integer else +0.
  if Base.IsNegativeInfinity then
  begin
    if E > 0 then
    begin
      if ExpIsOddInteger then
        Exit(TGocciaNumberLiteralValue.NegativeInfinityValue);
      Exit(TGocciaNumberLiteralValue.InfinityValue);
    end;
    if ExpIsOddInteger then
      Exit(TGocciaNumberLiteralValue.NegativeZeroValue);
    Exit(TGocciaNumberLiteralValue.Create(0));
  end;
  // 6. If base is +0, return +0 if exponent > 0 else +∞.
  // 7. If base is -0:
  //    a. If exponent > 0: -0 if odd-integer else +0.
  //    b. If exponent < 0: -∞ if odd-integer else +∞.
  if B = 0 then
  begin
    if Base.IsNegativeZero then
    begin
      if E > 0 then
      begin
        if ExpIsOddInteger then
          Exit(TGocciaNumberLiteralValue.NegativeZeroValue);
        Exit(TGocciaNumberLiteralValue.Create(0));
      end;
      if ExpIsOddInteger then
        Exit(TGocciaNumberLiteralValue.NegativeInfinityValue);
      Exit(TGocciaNumberLiteralValue.InfinityValue);
    end;
    if E > 0 then
      Exit(TGocciaNumberLiteralValue.Create(0));
    Exit(TGocciaNumberLiteralValue.InfinityValue);
  end;
  // 8. (Assert base is finite, base ≠ 0.) — ensured by the cases above.
  // 9. If exponent is +∞:
  //    a. abs(base) > 1 → +∞.
  //    b. abs(base) = 1 → NaN.
  //    c. abs(base) < 1 → +0.
  if Exponent.IsInfinity then
  begin
    AbsB := Abs(B);
    if AbsB > 1 then
      Exit(TGocciaNumberLiteralValue.InfinityValue);
    if AbsB = 1 then
      Exit(TGocciaNumberLiteralValue.NaNValue);
    Exit(TGocciaNumberLiteralValue.Create(0));
  end;
  // 10. If exponent is -∞:
  //     a. abs(base) > 1 → +0.
  //     b. abs(base) = 1 → NaN.
  //     c. abs(base) < 1 → +∞.
  if Exponent.IsNegativeInfinity then
  begin
    AbsB := Abs(B);
    if AbsB > 1 then
      Exit(TGocciaNumberLiteralValue.Create(0));
    if AbsB = 1 then
      Exit(TGocciaNumberLiteralValue.NaNValue);
    Exit(TGocciaNumberLiteralValue.InfinityValue);
  end;
  // 11. (base, exponent both finite & base ≠ 0.)
  // 12. If base < 0 and exponent is not an integer, return NaN.
  if (B < 0) and (Frac(E) <> 0) then
    Exit(TGocciaNumberLiteralValue.NaNValue);
  // 13. Implementation-defined approximation. FreePascal's Power handles the
  //     non-special cases via exp/ln; the special cases above are covered.
  IntPart := Power(B, E);
  Result := TGocciaNumberLiteralValue.Create(IntPart);
end;

// §21.3.2.32 Math.sqrt ( x )
function TGocciaMath.MathSqrt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: If n is NaN, return NaN.
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  // If n is +∞𝔽, return +∞𝔽.
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  // If n < -0𝔽 (including -∞𝔽), return NaN.
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.Value < 0 then
    Result := TGocciaNumberLiteralValue.NaNValue
  // Step 3: Return the square root of n.
  else
    Result := TGocciaNumberLiteralValue.Create(Sqrt(NumberArg.Value));
end;

// §21.3.2.27 Math.random ( )
function TGocciaMath.MathRandom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Return a Number value with positive sign, ≥ +0𝔽 and < 1𝔽,
  // chosen randomly with approximately uniform distribution.
  Result := TGocciaNumberLiteralValue.Create(Random);
end;

// Math.clamp ( x, lower, upper ) — ES2026 Stage 3 proposal
function TGocciaMath.MathClamp(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Value, MinVal, MaxVal: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 3, 'Math.clamp', ThrowError);

  // Step 1: Let x be ? ToNumber(x).
  Value := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: Let lower be ? ToNumber(lower).
  MinVal := AArgs.GetElement(1).ToNumberLiteral;
  // Step 3: Let upper be ? ToNumber(upper).
  MaxVal := AArgs.GetElement(2).ToNumberLiteral;

  // Step 4: If any argument is NaN, return NaN.
  if Value.IsNaN or MinVal.IsNaN or MaxVal.IsNaN then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  // Step 5: If lower > upper, throw a RangeError.
  if MinVal.IsGreaterThan(MaxVal).Value then
  begin
    ThrowRangeError(SErrorMathClampInvalidRange, SSuggestNumberRange);
    Exit;
  end;

  // Step 6: Return max(lower, min(upper, x)).
  if Value.IsLessThan(MinVal).Value then
    Result := MinVal
  else if Value.IsGreaterThan(MaxVal).Value then
    Result := MaxVal
  else
    Result := Value;
end;

// §21.3.2.29 Math.sign ( x )
function TGocciaMath.MathSign(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberLiteral: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberLiteral := AArgs.GetElement(0).ToNumberLiteral;

  // Step 2: If n is NaN, return NaN.
  if NumberLiteral.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  // Step 3: If n is +∞𝔽, return 1𝔽.
  else if NumberLiteral.IsInfinity then
    Result := TGocciaNumberLiteralValue.OneValue
  // Step 4: If n is -∞𝔽, return -1𝔽.
  else if NumberLiteral.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.Create(-1)
  // Step 5: If n < +0𝔽, return -1𝔽. If n > +0𝔽, return 1𝔽. Otherwise return n (±0).
  else if NumberLiteral.Value = 0 then
    Result := NumberLiteral
  else if NumberLiteral.Value < 0 then
    Result := TGocciaNumberLiteralValue.Create(-1)
  else
    Result := TGocciaNumberLiteralValue.OneValue;
end;

// §21.3.2.35 Math.trunc ( x )
function TGocciaMath.MathTrunc(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: If n is NaN, +∞𝔽, -∞𝔽, +0𝔽, or -0𝔽, return n.
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  else if NumberArg.IsNegativeZero or (NumberArg.Value = 0) then
    Result := NumberArg
  else if Abs(NumberArg.Value) >= DOUBLE_INTEGRAL_LIMIT then
    Result := NumberArg
  else if (NumberArg.Value < 0) and (NumberArg.Value > -1) then
    Result := TGocciaNumberLiteralValue.NegativeZeroValue
  // Step 3: Return the integral Number nearest n in the direction of +0𝔽.
  else
    Result := TGocciaNumberLiteralValue.Create(Trunc(NumberArg.Value));
end;

// §21.3.2.14 Math.exp ( x )
function TGocciaMath.MathExp(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberLiteral: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberLiteral := AArgs.GetElement(0).ToNumberLiteral;

  // Step 2: Return the implementation-approximated Number value for e^n.
  if NumberLiteral.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberLiteral.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.ZeroValue
  else if NumberLiteral.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else
    Result := TGocciaNumberLiteralValue.Create(Exp(NumberLiteral.Value));
end;

// §21.3.2.20 Math.log ( x )
function TGocciaMath.MathLog(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: Return the implementation-approximated Number value for ln(n).
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.Value = 0 then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  else if NumberArg.Value < 0 then
    Result := TGocciaNumberLiteralValue.NaNValue
  else
    Result := TGocciaNumberLiteralValue.Create(Ln(NumberArg.Value));
end;

// §21.3.2.21 Math.log10 ( x )
function TGocciaMath.MathLog10(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: Return the implementation-approximated Number value for log10(n).
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.Value = 0 then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  else if NumberArg.Value < 0 then
    Result := TGocciaNumberLiteralValue.NaNValue
  else
    Result := TGocciaNumberLiteralValue.Create(Log10(NumberArg.Value));
end;

// §21.3.2.30 Math.sin ( x )
function TGocciaMath.MathSin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: Return the implementation-approximated Number value for sin(n).
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue
  else
    Result := TGocciaNumberLiteralValue.Create(Sin(NumberArg.Value));
end;

// §21.3.2.12 Math.cos ( x )
function TGocciaMath.MathCos(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: Return the implementation-approximated Number value for cos(n).
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue
  else
    Result := TGocciaNumberLiteralValue.Create(Cos(NumberArg.Value));
end;

// §21.3.2.33 Math.tan ( x )
function TGocciaMath.MathTan(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: Return the implementation-approximated Number value for tan(n).
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue
  else
    Result := TGocciaNumberLiteralValue.Create(Tan(NumberArg.Value));
end;

// §21.3.2.2 Math.acos ( x )
function TGocciaMath.MathAcos(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: Return the implementation-approximated Number value for acos(n).
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue
  // If n > 1𝔽 or n < -1𝔽, return NaN.
  else if (NumberArg.Value < -1.0) or (NumberArg.Value > 1.0) then
    Result := TGocciaNumberLiteralValue.NaNValue
  else
    Result := TGocciaNumberLiteralValue.Create(ArcCos(NumberArg.Value));
end;

// §21.3.2.4 Math.asin ( x )
function TGocciaMath.MathAsin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: Return the implementation-approximated Number value for asin(n).
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue
  // If n > 1𝔽 or n < -1𝔽, return NaN.
  else if (NumberArg.Value < -1.0) or (NumberArg.Value > 1.0) then
    Result := TGocciaNumberLiteralValue.NaNValue
  else
    Result := TGocciaNumberLiteralValue.Create(ArcSin(NumberArg.Value));
end;

// §21.3.2.6 Math.atan ( x )
function TGocciaMath.MathAtan(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: Return the implementation-approximated Number value for atan(n).
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  // If n is +∞𝔽, return an approximation of π/2.
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.Create(Pi / 2)
  // If n is -∞𝔽, return an approximation of -π/2.
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.Create(-Pi / 2)
  else
    Result := TGocciaNumberLiteralValue.Create(ArcTan(NumberArg.Value));
end;

// §21.3.2.8 Math.atan2 ( y, x )
function TGocciaMath.MathAtan2(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Y, X: TGocciaNumberLiteralValue;
begin
  // Step 1: Let ny be ? ToNumber(y).
  Y := TGocciaArgumentConverter.GetNumber(AArgs, 0);
  // Step 2: Let nx be ? ToNumber(x).
  X := TGocciaArgumentConverter.GetNumber(AArgs, 1);

  // Step 3: Return the implementation-approximated Number value for atan2(ny, nx).
  if Y.IsNaN or X.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if Y.Value = 0 then
  begin
    if Y.IsNegativeZero then
    begin
      if X.IsNegativeZero or (X.Value < 0) or X.IsNegativeInfinity then
        Result := TGocciaNumberLiteralValue.Create(-Pi)
      else
        Result := TGocciaNumberLiteralValue.NegativeZeroValue;
    end
    else if X.IsNegativeZero or (X.Value < 0) or X.IsNegativeInfinity then
      Result := TGocciaNumberLiteralValue.Create(Pi)
    else
      Result := TGocciaNumberLiteralValue.ZeroValue;
  end
  else if Y.IsInfinity and X.IsInfinity then
    Result := TGocciaNumberLiteralValue.Create(Pi / 4)
  else if Y.IsInfinity and X.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.Create(3 * Pi / 4)
  else if Y.IsInfinity then
    Result := TGocciaNumberLiteralValue.Create(Pi / 2)
  else if Y.IsNegativeInfinity and X.IsInfinity then
    Result := TGocciaNumberLiteralValue.Create(-Pi / 4)
  else if Y.IsNegativeInfinity and X.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.Create(-3 * Pi / 4)
  else if Y.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.Create(-Pi / 2)
  else if X.IsInfinity then
  begin
    if Y.Value < 0 then
      Result := TGocciaNumberLiteralValue.NegativeZeroValue
    else
      Result := TGocciaNumberLiteralValue.ZeroValue;
  end
  else if X.IsNegativeInfinity then
  begin
    if Y.Value < 0 then
      Result := TGocciaNumberLiteralValue.Create(-Pi)
    else
      Result := TGocciaNumberLiteralValue.Create(Pi);
  end
  else
    Result := TGocciaNumberLiteralValue.Create(ArcTan2(Y.Value, X.Value));
end;

// §21.3.2.9 Math.cbrt ( x )
function TGocciaMath.MathCbrt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: Return the implementation-approximated Number value for cbrt(n).
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  else if NumberArg.Value = 0.0 then
    Result := NumberArg
  else
    Result := TGocciaNumberLiteralValue.Create(CubeRootApprox(NumberArg.Value));
end;

// §21.3.2.13 Math.cosh ( x )
function TGocciaMath.MathCosh(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: Return the implementation-approximated Number value for cosh(n).
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else
    Result := TGocciaNumberLiteralValue.Create(Cosh(NumberArg.Value));
end;

// §21.3.2.31 Math.sinh ( x )
function TGocciaMath.MathSinh(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: Return the implementation-approximated Number value for sinh(n).
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  else if NumberArg.Value = 0 then
    Result := NumberArg
  else
    Result := TGocciaNumberLiteralValue.Create(SinhApprox(NumberArg.Value));
end;

// §21.3.2.34 Math.tanh ( x )
function TGocciaMath.MathTanh(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: Return the implementation-approximated Number value for tanh(n).
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.OneValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.Create(-1)
  else if NumberArg.Value = 0 then
    Result := NumberArg
  else
    Result := TGocciaNumberLiteralValue.Create(TanhApprox(NumberArg.Value));
end;

// §21.3.2.3 Math.acosh ( x )
function TGocciaMath.MathAcosh(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: Return the implementation-approximated Number value for acosh(n).
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue
  // If n < 1𝔽, return NaN.
  else if NumberArg.Value < 1.0 then
    Result := TGocciaNumberLiteralValue.NaNValue
  else
    Result := TGocciaNumberLiteralValue.Create(AcoshApprox(NumberArg.Value));
end;

// §21.3.2.5 Math.asinh ( x )
function TGocciaMath.MathAsinh(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: Return the implementation-approximated Number value for asinh(n).
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  else
    Result := TGocciaNumberLiteralValue.Create(AsinhApprox(NumberArg.Value));
end;

// §21.3.2.7 Math.atanh ( x )
function TGocciaMath.MathAtanh(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: Return the implementation-approximated Number value for atanh(n).
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue
  // If n > 1𝔽 or n < -1𝔽, return NaN.
  else if NumberArg.Value > 1.0 then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.Value < -1.0 then
    Result := TGocciaNumberLiteralValue.NaNValue
  // If n is 1𝔽, return +∞𝔽.
  else if NumberArg.Value = 1.0 then
    Result := TGocciaNumberLiteralValue.InfinityValue
  // If n is -1𝔽, return -∞𝔽.
  else if NumberArg.Value = -1.0 then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  else if NumberArg.Value = 0 then
    Result := NumberArg
  else
    Result := TGocciaNumberLiteralValue.Create(AtanhApprox(NumberArg.Value));
end;

// §21.3.2.15 Math.expm1 ( x )
function TGocciaMath.MathExpm1(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: Return the implementation-approximated Number value for e^n - 1.
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.Create(-1)
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if NumberArg.Value = 0 then
    Result := NumberArg
  else
    Result := TGocciaNumberLiteralValue.Create(Expm1Approx(NumberArg.Value));
end;

// ES2026 §21.3.2.17 Math.f16round ( x )
function TGocciaMath.MathF16round(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: If n is NaN, return NaN. If n is ±∞ or ±0, return n.
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  else if NumberArg.Value = 0 then
    Result := NumberArg
  // Step 3: Return the result of rounding n to the nearest float16 value.
  else
    Result := TGocciaNumberLiteralValue.Create(Float16ToDouble(DoubleToFloat16(NumberArg.Value)));
end;

// §21.3.2.17 Math.fround ( x )
function TGocciaMath.MathFround(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
  SingleVal: Single;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: If n is NaN, return NaN. If n is ±∞, return n.
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  else if NumberArg.Value = 0 then
    Result := NumberArg
  // Step 3: Return the result of rounding n to the nearest float32 value.
  else
  begin
    SingleVal := NumberArg.Value;
    Result := TGocciaNumberLiteralValue.Create(SingleVal);
  end;
end;

// §21.3.2.18 Math.hypot ( ...args )
function TGocciaMath.MathHypot(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaNumberLiteralValue;
  Accumulator: Double;
  HasNaN, HasInfinity: Boolean;
  I: Integer;
begin
  // Step 1: Let coerced be a new empty List.
  // Step 2: For each element arg of args, append ? ToNumber(arg) to coerced.
  Accumulator := 0;
  HasNaN := False;
  HasInfinity := False;

  for I := 0 to AArgs.Length - 1 do
  begin
    Arg := TGocciaArgumentConverter.GetNumber(AArgs, I);
    if Arg.IsNaN then
      HasNaN := True
    else if Arg.IsInfinity or Arg.IsNegativeInfinity then
      HasInfinity := True
    else
      Accumulator := Hypot(Accumulator, Arg.Value);
  end;

  // Step 3: If any element is ±∞, return +∞𝔽.
  if HasInfinity then
    Exit(TGocciaNumberLiteralValue.InfinityValue);
  // Step 4: If any element is NaN, return NaN.
  if HasNaN then
    Exit(TGocciaNumberLiteralValue.NaNValue);
  // Step 5: Return sqrt(sum of squares).
  Result := TGocciaNumberLiteralValue.Create(Accumulator);
end;

// §21.3.2.19 Math.imul ( x, y )
function TGocciaMath.MathImul(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  X, Y: Cardinal;
  Product: UInt64;
  SignedProduct: Int64;
begin
  // Step 1: Let a be ? ToUint32(x).
  X := ToUint32Value(AArgs.GetElement(0));
  // Step 2: Let b be ? ToUint32(y).
  Y := ToUint32Value(AArgs.GetElement(1));
  // Step 3: Let product be (a x b) modulo 2^32.
  Product := (UInt64(X) * UInt64(Y)) and UInt64($FFFFFFFF);
  // Step 4: If product >= 2^31, return product - 2^32; otherwise return product.
  if Product >= UInt64($80000000) then
    SignedProduct := Int64(Product) - Int64($100000000)
  else
    SignedProduct := Int64(Product);
  Result := TGocciaNumberLiteralValue.Create(SignedProduct);
end;

// §21.3.2.22 Math.log1p ( x )
function TGocciaMath.MathLog1p(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: Return the implementation-approximated Number value for ln(1 + n).
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue
  // If n is -1𝔽, return -∞𝔽.
  else if NumberArg.Value = -1.0 then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  // If n < -1𝔽, return NaN.
  else if NumberArg.Value < -1.0 then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.Value = 0 then
    Result := NumberArg
  else
    Result := TGocciaNumberLiteralValue.Create(Log1pApprox(NumberArg.Value));
end;

// §21.3.2.23 Math.log2 ( x )
function TGocciaMath.MathLog2(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
  LogValue: Double;
  RoundedLog: Integer;
begin
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: Return the implementation-approximated Number value for log2(n).
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.Value = 0 then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  else if NumberArg.Value < 0 then
    Result := TGocciaNumberLiteralValue.NaNValue
  else
  begin
    LogValue := Log2(NumberArg.Value);
    RoundedLog := Round(LogValue);
    if (RoundedLog > -1075) and (RoundedLog < 1024) and
       (Power(2.0, RoundedLog) = NumberArg.Value) then
      Result := TGocciaNumberLiteralValue.Create(RoundedLog)
    else
      Result := TGocciaNumberLiteralValue.Create(LogValue);
  end;
end;

// §21.3.2.11 Math.clz32 ( x )
function TGocciaMath.MathClz32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Value: LongWord;
  Count: Integer;
begin
  // Step 1: Let n be ? ToUint32(x). NaN/±∞ coerce to 0, yielding 32.
  Value := ToUint32Value(AArgs.GetElement(0));
  if Value = 0 then
    Count := 32
  else
  begin
    // Step 2: Let p be the number of leading zero bits in the unsigned 32-bit
    // binary representation of n.
    Count := 0;
    while (Value and $80000000) = 0 do
    begin
      Inc(Count);
      Value := Value shl 1;
    end;
  end;
  // Step 3: Return 𝔽(p).
  Result := TGocciaNumberLiteralValue.Create(Count);
end;

// TC39 proposal-math-sum §1 Math.sumPrecise(items)
function TGocciaMath.MathSumPrecise(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

  procedure AccumulateValue(const AElement: TGocciaValue;
    var AExactSum: TBigInteger; var ACommonExponent: Integer;
    var AHasExactSum, AHasPosInf, AHasNegInf, AHasNaN,
    AHasNonMinusZero: Boolean);
  var
    NumVal: TGocciaNumberLiteralValue;
    Mantissa: TBigInteger;
    Exponent: Integer;
  begin
    if not (AElement is TGocciaNumberLiteralValue) then
      Goccia.Values.ErrorHelper.ThrowTypeError(SErrorMathSumPreciseNotNumber, SSuggestNumberRange);
    NumVal := TGocciaNumberLiteralValue(AElement);
    if NumVal.IsNaN then
      AHasNaN := True
    else if NumVal.IsInfinity then
      AHasPosInf := True
    else if NumVal.IsNegativeInfinity then
      AHasNegInf := True
    else
    begin
      if (NumVal.Value <> 0) or not NumVal.IsNegativeZero then
        AHasNonMinusZero := True;
      if DecomposeFiniteDouble(NumVal.Value, Mantissa, Exponent) then
      begin
        if not AHasExactSum then
        begin
          AExactSum := Mantissa;
          ACommonExponent := Exponent;
          AHasExactSum := True;
        end
        else
        begin
          if Exponent < ACommonExponent then
          begin
            AExactSum := AExactSum.ShiftLeft(ACommonExponent - Exponent);
            ACommonExponent := Exponent;
          end;
          AExactSum := AExactSum.Add(Mantissa.ShiftLeft(Exponent - ACommonExponent));
        end;
      end;
    end;
  end;

var
  Iterable, Element: TGocciaValue;
  Iterator: TGocciaIteratorValue;
  Done: Boolean;
  ExactSum: TBigInteger;
  CommonExponent: Integer;
  Sum: Double;
  HasExactSum, HasPosInf, HasNegInf, HasNaN, HasNonMinusZero: Boolean;
begin
  Iterable := AArgs.GetElement(0);

  ExactSum := TBigInteger.Zero;
  CommonExponent := 0;
  HasExactSum := False;
  HasPosInf := False;
  HasNegInf := False;
  HasNaN := False;
  HasNonMinusZero := False;

  Iterator := GetIteratorFromValue(Iterable);
  if not Assigned(Iterator) then
    Goccia.Values.ErrorHelper.ThrowTypeError(SErrorMathSumPreciseNotIterable,
      SSuggestNotIterable);

  TGarbageCollector.Instance.AddTempRoot(Iterator);
  try
    Element := Iterator.DirectNext(Done);
    while not Done do
    begin
      try
        AccumulateValue(Element, ExactSum, CommonExponent, HasExactSum,
          HasPosInf, HasNegInf, HasNaN, HasNonMinusZero);
      except
        CloseIteratorPreservingError(Iterator);
        raise;
      end;
      Element := Iterator.DirectNext(Done);
    end;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(Iterator);
  end;

  // Handle special cases per spec
  if HasNaN then
    Exit(TGocciaNumberLiteralValue.NaNValue);
  if HasPosInf and HasNegInf then
    Exit(TGocciaNumberLiteralValue.NaNValue);
  if HasPosInf then
    Exit(TGocciaNumberLiteralValue.InfinityValue);
  if HasNegInf then
    Exit(TGocciaNumberLiteralValue.NegativeInfinityValue);
  if not HasNonMinusZero and not HasExactSum then
    Exit(TGocciaNumberLiteralValue.NegativeZeroValue);

  Sum := ExactScaledIntegerToDouble(ExactSum, CommonExponent);
  if (Sum = 0) and not HasNonMinusZero then
    Exit(TGocciaNumberLiteralValue.NegativeZeroValue);
  Result := TGocciaNumberLiteralValue.Create(Sum);
end;

initialization
  RegisterThreadvarCleanup(@ClearThreadvarMembers);

end.
