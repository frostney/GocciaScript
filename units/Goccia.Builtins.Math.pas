unit Goccia.Builtins.Math;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaMath = class(TGocciaBuiltin)
  protected
    // Native methods
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
    function MathFround(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathHypot(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathImul(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathLog1p(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathLog2(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MathClz32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  Math,

  Goccia.Arguments.Converter,
  Goccia.Arguments.Validator,
  Goccia.Values.ClassHelper,
  Goccia.Values.NativeFunction;

{ TGocciaMath }

constructor TGocciaMath.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
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

  // Methods: writable, non-enumerable, configurable
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathAbs, 'abs', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathFloor, 'floor', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathCeil, 'ceil', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathRound, 'round', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathMax, 'max', -1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathMin, 'min', -1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathPow, 'pow', 2));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathSqrt, 'sqrt', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathRandom, 'random', 0));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathClamp, 'clamp', 3));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathSign, 'sign', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathTrunc, 'trunc', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathExp, 'exp', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathLog, 'log', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathLog10, 'log10', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathSin, 'sin', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathCos, 'cos', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathTan, 'tan', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathAcos, 'acos', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathAsin, 'asin', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathAtan, 'atan', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathAtan2, 'atan2', 2));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathCbrt, 'cbrt', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathCosh, 'cosh', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathSinh, 'sinh', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathTanh, 'tanh', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathAcosh, 'acosh', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathAsinh, 'asinh', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathAtanh, 'atanh', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathExpm1, 'expm1', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathFround, 'fround', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathHypot, 'hypot', 2));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathImul, 'imul', 2));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathLog1p, 'log1p', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathLog2, 'log2', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MathClz32, 'clz32', 1));

  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet);
end;

// §21.3.2.1 Math.abs ( x )
function TGocciaMath.MathAbs(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.abs', ThrowError);

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
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.floor', ThrowError);
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := TGocciaArgumentConverter.GetNumber(AArgs, 0);
  // Step 2: If n is NaN, +∞𝔽, -∞𝔽, or an integral Number, return n.
  if NumberArg.IsNaN or NumberArg.IsInfinite then
  begin
    Result := NumberArg;
    Exit;
  end;
  // Step 3: Return the greatest (closest to +∞) integral Number ≤ n.
  Result := TGocciaNumberLiteralValue.Create(Floor(NumberArg.Value));
end;

// §21.3.2.10 Math.ceil ( x )
function TGocciaMath.MathCeil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.ceil', ThrowError);
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := TGocciaArgumentConverter.GetNumber(AArgs, 0);
  // Step 2: If n is NaN, +∞𝔽, -∞𝔽, or an integral Number, return n.
  if NumberArg.IsNaN or NumberArg.IsInfinite then
  begin
    Result := NumberArg;
    Exit;
  end;
  // Step 3: Return the smallest (closest to -∞) integral Number ≥ n.
  Result := TGocciaNumberLiteralValue.Create(Ceil(NumberArg.Value));
end;

// §21.3.2.28 Math.round ( x )
function TGocciaMath.MathRound(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.round', ThrowError);
  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: If n is NaN, +∞𝔽, -∞𝔽, or an integral Number, return n.
  if NumberArg.IsNaN or NumberArg.IsInfinite then
  begin
    Result := NumberArg;
    Exit;
  end;
  // Step 3: If n < +0𝔽 and n ≥ -0.5𝔽, return -0𝔽.
  if (NumberArg.Value < 0) and (NumberArg.Value >= -0.5) then
    Result := TGocciaNumberLiteralValue.NegativeZeroValue
  // Step 4: Return floor(n + 0.5).
  else
    Result := TGocciaNumberLiteralValue.Create(Floor(NumberArg.Value + 0.5));
end;

// §21.3.2.24 Math.max ( ...args )
function TGocciaMath.MathMax(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  MaxVal, NumVal: TGocciaNumberLiteralValue;
begin
  // Step 1: Let coerced be a new empty List.
  // Step 2: For each element arg of args, append ? ToNumber(arg) to coerced.
  // Step 3: If coerced has no elements, return -∞𝔽.
  if AArgs.Length = 0 then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  else
  begin
    MaxVal := AArgs.GetElement(0).ToNumberLiteral;

    // Step 4: If any element is NaN, return NaN.
    if MaxVal.IsNaN then
    begin
      Result := TGocciaNumberLiteralValue.NaNValue;
      Exit;
    end;

    // Step 5: Return the largest value among the elements of coerced.
    for I := 1 to AArgs.Length - 1 do
    begin
      NumVal := AArgs.GetElement(I).ToNumberLiteral;

      if NumVal.IsNaN then
      begin
        Result := TGocciaNumberLiteralValue.NaNValue;
        Exit;
      end;

      if NumVal.IsGreaterThan(MaxVal).Value then
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
begin
  // Step 1: Let coerced be a new empty List.
  // Step 2: For each element arg of args, append ? ToNumber(arg) to coerced.
  // Step 3: If coerced has no elements, return +∞𝔽.
  if AArgs.Length = 0 then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else
  begin
    MinVal := AArgs.GetElement(0).ToNumberLiteral;

    // Step 4: If any element is NaN, return NaN.
    if MinVal.IsNaN then
    begin
      Result := TGocciaNumberLiteralValue.NaNValue;
      Exit;
    end;

    // Step 5: Return the smallest value among the elements of coerced.
    for I := 1 to AArgs.Length - 1 do
    begin
      NumVal := AArgs.GetElement(I).ToNumberLiteral;

      if NumVal.IsNaN then
      begin
        Result := TGocciaNumberLiteralValue.NaNValue;
        Exit;
      end;

      if NumVal.IsLessThan(MinVal).Value then
        MinVal := NumVal;
    end;
    Result := MinVal;
  end;
end;

// §21.3.2.26 Math.pow ( base, exponent )
function TGocciaMath.MathPow(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Base, Exponent: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 2, 'Math.pow', ThrowError);
  // Step 1: Let base be ? ToNumber(base).
  Base := TGocciaArgumentConverter.GetNumber(AArgs, 0);
  // Step 2: Let exponent be ? ToNumber(exponent).
  Exponent := TGocciaArgumentConverter.GetNumber(AArgs, 1);
  // Step 3: Return Number::exponentiate(base, exponent).
  Result := TGocciaNumberLiteralValue.Create(Power(Base.Value, Exponent.Value));
end;

// §21.3.2.32 Math.sqrt ( x )
function TGocciaMath.MathSqrt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.sqrt', ThrowError);

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
  TGocciaArgumentValidator.RequireNone(AArgs, 'Math.random', ThrowError);
  // Step 1: Return a Number value with positive sign, ≥ +0𝔽 and < 1𝔽,
  // chosen randomly with approximately uniform distribution.
  Result := TGocciaNumberLiteralValue.Create(Random);
end;

// Math.clamp ( x, lower, upper ) — ES2026 Stage 3 proposal
function TGocciaMath.MathClamp(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Value, MinVal, MaxVal: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 3, 'Math.clamp', ThrowError);

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
    ThrowError('RangeError: Invalid range in Math.clamp', 0, 0);
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
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.sign', ThrowError);

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
  else
    Result := TGocciaNumberLiteralValue.Create(Sign(NumberLiteral.Value));
end;

// §21.3.2.35 Math.trunc ( x )
function TGocciaMath.MathTrunc(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.trunc', ThrowError);

  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: If n is NaN, +∞𝔽, -∞𝔽, +0𝔽, or -0𝔽, return n.
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  // Step 3: Return the integral Number nearest n in the direction of +0𝔽.
  else
    Result := TGocciaNumberLiteralValue.Create(Trunc(NumberArg.Value));
end;

// §21.3.2.14 Math.exp ( x )
function TGocciaMath.MathExp(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberLiteral: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.exp', ThrowError);

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
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.log', ThrowError);

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
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.log10', ThrowError);

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
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.sin', ThrowError);

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
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.cos', ThrowError);

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
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.tan', ThrowError);

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
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.acos', ThrowError);

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
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.asin', ThrowError);

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
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.atan', ThrowError);

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
  TGocciaArgumentValidator.RequireExactly(AArgs, 2, 'Math.atan2', ThrowError);

  // Step 1: Let ny be ? ToNumber(y).
  Y := TGocciaArgumentConverter.GetNumber(AArgs, 0);
  // Step 2: Let nx be ? ToNumber(x).
  X := TGocciaArgumentConverter.GetNumber(AArgs, 1);

  // Step 3: Return the implementation-approximated Number value for atan2(ny, nx).
  if Y.IsNaN or X.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if Y.IsInfinity and X.IsInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if Y.IsNegativeInfinity and X.IsInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if Y.IsInfinity and X.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if Y.IsNegativeInfinity and X.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue
  else
    Result := TGocciaNumberLiteralValue.Create(ArcTan2(Y.Value, X.Value));
end;

// §21.3.2.9 Math.cbrt ( x )
function TGocciaMath.MathCbrt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
  SignVal: Double;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.cbrt', ThrowError);

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
    Result := TGocciaNumberLiteralValue.ZeroValue
  else
  begin
    SignVal := Sign(NumberArg.Value);
    Result := TGocciaNumberLiteralValue.Create(SignVal * Power(Abs(NumberArg.Value), 1.0 / 3.0));
  end;
end;

// §21.3.2.13 Math.cosh ( x )
function TGocciaMath.MathCosh(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.cosh', ThrowError);

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
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.sinh', ThrowError);

  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: Return the implementation-approximated Number value for sinh(n).
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  else
    Result := TGocciaNumberLiteralValue.Create(Sinh(NumberArg.Value));
end;

// §21.3.2.34 Math.tanh ( x )
function TGocciaMath.MathTanh(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.tanh', ThrowError);

  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: Return the implementation-approximated Number value for tanh(n).
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.OneValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.Create(-1)
  else
    Result := TGocciaNumberLiteralValue.Create(Tanh(NumberArg.Value));
end;

// §21.3.2.3 Math.acosh ( x )
function TGocciaMath.MathAcosh(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.acosh', ThrowError);

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
    Result := TGocciaNumberLiteralValue.Create(ArcCosh(NumberArg.Value));
end;

// §21.3.2.5 Math.asinh ( x )
function TGocciaMath.MathAsinh(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.asinh', ThrowError);

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
    Result := TGocciaNumberLiteralValue.Create(ArcSinh(NumberArg.Value));
end;

// §21.3.2.7 Math.atanh ( x )
function TGocciaMath.MathAtanh(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.atanh', ThrowError);

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
  else
    Result := TGocciaNumberLiteralValue.Create(ArcTanh(NumberArg.Value));
end;

// §21.3.2.15 Math.expm1 ( x )
function TGocciaMath.MathExpm1(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.expm1', ThrowError);

  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: Return the implementation-approximated Number value for e^n - 1.
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.Create(-1)
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else
    Result := TGocciaNumberLiteralValue.Create(Exp(NumberArg.Value) - 1.0);
end;

// §21.3.2.17 Math.fround ( x )
function TGocciaMath.MathFround(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
  SingleVal: Single;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.fround', ThrowError);

  // Step 1: Let n be ? ToNumber(x).
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 2: If n is NaN, return NaN. If n is ±∞, return n.
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
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
  X, Y: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 2, 'Math.hypot', ThrowError);

  // Step 1: Let coerced be a new empty List.
  // Step 2: For each element arg of args, append ? ToNumber(arg) to coerced.
  X := TGocciaArgumentConverter.GetNumber(AArgs, 0);
  Y := TGocciaArgumentConverter.GetNumber(AArgs, 1);

  // Step 3: If any element is NaN, return NaN.
  if X.IsNaN or Y.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  // Step 4: If any element is ±∞, return +∞𝔽.
  else if (X.IsInfinity or X.IsNegativeInfinity) or (Y.IsInfinity or Y.IsNegativeInfinity) then
    Result := TGocciaNumberLiteralValue.InfinityValue
  // Step 5: Return sqrt(sum of squares).
  else
    Result := TGocciaNumberLiteralValue.Create(Hypot(X.Value, Y.Value));
end;

// §21.3.2.19 Math.imul ( x, y )
function TGocciaMath.MathImul(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  X, Y: TGocciaNumberLiteralValue;
  XInt, YInt: LongInt;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 2, 'Math.imul', ThrowError);

  X := TGocciaArgumentConverter.GetNumber(AArgs, 0);
  Y := TGocciaArgumentConverter.GetNumber(AArgs, 1);

  if X.IsNaN or Y.IsNaN then
    Result := TGocciaNumberLiteralValue.ZeroValue
  else
  begin
    // Step 1: Let a be ? ToUint32(x).
    XInt := LongInt(Trunc(X.Value));
    // Step 2: Let b be ? ToUint32(y).
    YInt := LongInt(Trunc(Y.Value));
    // Step 3: Let product be (a × b) modulo 2^32.
    // Step 4: If product ≥ 2^31, return product - 2^32; otherwise return product.
    Result := TGocciaNumberLiteralValue.Create(XInt * YInt);
  end;
end;

// §21.3.2.22 Math.log1p ( x )
function TGocciaMath.MathLog1p(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.log1p', ThrowError);

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
  else
    Result := TGocciaNumberLiteralValue.Create(Ln(1.0 + NumberArg.Value));
end;

// §21.3.2.23 Math.log2 ( x )
function TGocciaMath.MathLog2(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.log2', ThrowError);

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
    Result := TGocciaNumberLiteralValue.Create(Log2(NumberArg.Value));
end;

// §21.3.2.11 Math.clz32 ( x )
function TGocciaMath.MathClz32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
  Value: LongWord;
  Count: Integer;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.clz32', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  // Step 1: Let n be ? ToUint32(x). NaN/±∞ coerce to 0, yielding 32.
  if NumberArg.IsNaN or NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.Create(32)
  else
  begin
    Value := LongWord(Trunc(NumberArg.Value));
    // Step 2: Let p be the number of leading zero bits in the unsigned 32-bit
    // binary representation of n.
    if Value = 0 then
      Count := 32
    else
    begin
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
end;

end.
