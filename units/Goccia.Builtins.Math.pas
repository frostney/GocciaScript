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

function TGocciaMath.MathAbs(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.abs', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else
    Result := TGocciaNumberLiteralValue.Create(Abs(NumberArg.Value));
end;

function TGocciaMath.MathFloor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.floor', ThrowError);
  NumberArg := TGocciaArgumentConverter.GetNumber(AArgs, 0);
  if NumberArg.IsNaN or NumberArg.IsInfinite then
  begin
    Result := NumberArg;
    Exit;
  end;
  Result := TGocciaNumberLiteralValue.Create(Floor(NumberArg.Value));
end;

function TGocciaMath.MathCeil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.ceil', ThrowError);
  NumberArg := TGocciaArgumentConverter.GetNumber(AArgs, 0);
  if NumberArg.IsNaN or NumberArg.IsInfinite then
  begin
    Result := NumberArg;
    Exit;
  end;
  Result := TGocciaNumberLiteralValue.Create(Ceil(NumberArg.Value));
end;

function TGocciaMath.MathRound(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.round', ThrowError);
  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN or NumberArg.IsInfinite then
  begin
    Result := NumberArg;
    Exit;
  end;
  if (NumberArg.Value < 0) and (NumberArg.Value >= -0.5) then
    Result := TGocciaNumberLiteralValue.NegativeZeroValue
  else
    Result := TGocciaNumberLiteralValue.Create(Floor(NumberArg.Value + 0.5));
end;

function TGocciaMath.MathMax(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  MaxVal, NumVal: TGocciaNumberLiteralValue;
begin
  if AArgs.Length = 0 then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  else
  begin
    MaxVal := AArgs.GetElement(0).ToNumberLiteral;

    // If any argument is NaN, return NaN
    if MaxVal.IsNaN then
    begin
      Result := TGocciaNumberLiteralValue.NaNValue;
      Exit;
    end;

    for I := 1 to AArgs.Length - 1 do
    begin
      NumVal := AArgs.GetElement(I).ToNumberLiteral;

      // If any argument is NaN, return NaN
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

function TGocciaMath.MathMin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  MinVal, NumVal: TGocciaNumberLiteralValue;
begin
  if AArgs.Length = 0 then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else
  begin
    MinVal := AArgs.GetElement(0).ToNumberLiteral;

    // If any argument is NaN, return NaN
    if MinVal.IsNaN then
    begin
      Result := TGocciaNumberLiteralValue.NaNValue;
      Exit;
    end;

    for I := 1 to AArgs.Length - 1 do
    begin
      NumVal := AArgs.GetElement(I).ToNumberLiteral;

      // If any argument is NaN, return NaN
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

function TGocciaMath.MathPow(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Base, Exponent: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 2, 'Math.pow', ThrowError);
  Base := TGocciaArgumentConverter.GetNumber(AArgs, 0);
  Exponent := TGocciaArgumentConverter.GetNumber(AArgs, 1);
  Result := TGocciaNumberLiteralValue.Create(Power(Base.Value, Exponent.Value));
end;

function TGocciaMath.MathSqrt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.sqrt', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue // sqrt(-Infinity) = NaN
  else if NumberArg.Value < 0 then
    Result := TGocciaNumberLiteralValue.NaNValue // sqrt of negative number = NaN
  else
    Result := TGocciaNumberLiteralValue.Create(Sqrt(NumberArg.Value));
end;

function TGocciaMath.MathRandom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireNone(AArgs, 'Math.random', ThrowError);
  Result := TGocciaNumberLiteralValue.Create(Random);
end;

function TGocciaMath.MathClamp(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Value, MinVal, MaxVal: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 3, 'Math.clamp', ThrowError);

  Value := AArgs.GetElement(0).ToNumberLiteral;
  MinVal := AArgs.GetElement(1).ToNumberLiteral;
  MaxVal := AArgs.GetElement(2).ToNumberLiteral;

  // If any argument is NaN, return NaN
  if Value.IsNaN or MinVal.IsNaN or MaxVal.IsNaN then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  // If min > max, throw RangeError
  if MinVal.IsGreaterThan(MaxVal).Value then
  begin
    ThrowError('RangeError: Invalid range in Math.clamp', 0, 0);
    Exit;
  end;

  // Clamp the value - return the appropriate number literal object
  if Value.IsLessThan(MinVal).Value then
    Result := MinVal
  else if Value.IsGreaterThan(MaxVal).Value then
    Result := MaxVal
  else
    Result := Value;
end;

function TGocciaMath.MathSign(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberLiteral: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.sign', ThrowError);

  NumberLiteral := AArgs.GetElement(0).ToNumberLiteral;

  if NumberLiteral.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberLiteral.IsInfinity then
    Result := TGocciaNumberLiteralValue.OneValue  // +Infinity has sign 1
  else if NumberLiteral.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.Create(-1)  // -Infinity has sign -1
  else
    Result := TGocciaNumberLiteralValue.Create(Sign(NumberLiteral.Value));
end;

function TGocciaMath.MathTrunc(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.trunc', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  else
    Result := TGocciaNumberLiteralValue.Create(Trunc(NumberArg.Value));
end;

function TGocciaMath.MathExp(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberLiteral: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.exp', ThrowError);

  NumberLiteral := AArgs.GetElement(0).ToNumberLiteral;

  if NumberLiteral.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberLiteral.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.ZeroValue  // exp(-Infinity) = 0
  else if NumberLiteral.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue  // exp(+Infinity) = +Infinity
  else
    Result := TGocciaNumberLiteralValue.Create(Exp(NumberLiteral.Value));
end;

function TGocciaMath.MathLog(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.log', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue // log(-Infinity) = NaN
  else if NumberArg.Value = 0 then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue // log(0) = -Infinity
  else if NumberArg.Value < 0 then
    Result := TGocciaNumberLiteralValue.NaNValue // log of negative number = NaN
  else
    Result := TGocciaNumberLiteralValue.Create(Ln(NumberArg.Value)); // Natural logarithm (base e)
end;

function TGocciaMath.MathLog10(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.log10', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue // log10(-Infinity) = NaN
  else if NumberArg.Value = 0 then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue // log10(0) = -Infinity
  else if NumberArg.Value < 0 then
    Result := TGocciaNumberLiteralValue.NaNValue // log10 of negative number = NaN
  else
    Result := TGocciaNumberLiteralValue.Create(Log10(NumberArg.Value));
end;

function TGocciaMath.MathSin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.sin', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue  // sin(±Infinity) = NaN
  else
    Result := TGocciaNumberLiteralValue.Create(Sin(NumberArg.Value));
end;

function TGocciaMath.MathCos(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.cos', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue  // cos(±Infinity) = NaN
  else
    Result := TGocciaNumberLiteralValue.Create(Cos(NumberArg.Value));
end;

function TGocciaMath.MathTan(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.tan', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue  // tan(±Infinity) = NaN
  else
    Result := TGocciaNumberLiteralValue.Create(Tan(NumberArg.Value));
end;

function TGocciaMath.MathAcos(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.acos', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue  // acos(±Infinity) = NaN
  else if (NumberArg.Value < -1.0) or (NumberArg.Value > 1.0) then
    Result := TGocciaNumberLiteralValue.NaNValue
  else
    Result := TGocciaNumberLiteralValue.Create(ArcCos(NumberArg.Value));
end;

function TGocciaMath.MathAsin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.asin', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if (NumberArg.Value < -1.0) or (NumberArg.Value > 1.0) then
    Result := TGocciaNumberLiteralValue.NaNValue
  else
    Result := TGocciaNumberLiteralValue.Create(ArcSin(NumberArg.Value));
end;

function TGocciaMath.MathAtan(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.atan', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.Create(Pi / 2)  // atan(+Infinity) = π/2
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.Create(-Pi / 2)  // atan(-Infinity) = -π/2
  else
    Result := TGocciaNumberLiteralValue.Create(ArcTan(NumberArg.Value));
end;

function TGocciaMath.MathAtan2(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Y, X: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 2, 'Math.atan2', ThrowError);

  Y := TGocciaArgumentConverter.GetNumber(AArgs, 0);
  X := TGocciaArgumentConverter.GetNumber(AArgs, 1);

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

function TGocciaMath.MathCbrt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
  SignVal: Double;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.cbrt', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
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

function TGocciaMath.MathCosh(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.cosh', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue  // cosh(±Infinity) = +Infinity
  else
    Result := TGocciaNumberLiteralValue.Create(Cosh(NumberArg.Value));
end;

function TGocciaMath.MathSinh(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.sinh', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue  // sinh(+Infinity) = +Infinity
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue  // sinh(-Infinity) = -Infinity
  else
    Result := TGocciaNumberLiteralValue.Create(Sinh(NumberArg.Value));
end;

function TGocciaMath.MathTanh(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.tanh', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.OneValue  // tanh(+Infinity) = 1
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.Create(-1)  // tanh(-Infinity) = -1
  else
    Result := TGocciaNumberLiteralValue.Create(Tanh(NumberArg.Value));
end;

function TGocciaMath.MathAcosh(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.acosh', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue  // acosh(+Infinity) = +Infinity
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue  // acosh(-Infinity) = NaN
  else if NumberArg.Value < 1.0 then
    Result := TGocciaNumberLiteralValue.NaNValue  // acosh(x < 1) = NaN
  else
    Result := TGocciaNumberLiteralValue.Create(ArcCosh(NumberArg.Value));
end;

function TGocciaMath.MathAsinh(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.asinh', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue  // asinh(+Infinity) = +Infinity
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue  // asinh(-Infinity) = -Infinity
  else
    Result := TGocciaNumberLiteralValue.Create(ArcSinh(NumberArg.Value));
end;

function TGocciaMath.MathAtanh(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.atanh', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue  // atanh(±Infinity) = NaN
  else if NumberArg.Value > 1.0 then
    Result := TGocciaNumberLiteralValue.NaNValue  // atanh(x > 1) = NaN
  else if NumberArg.Value < -1.0 then
    Result := TGocciaNumberLiteralValue.NaNValue  // atanh(x < -1) = NaN
  else if NumberArg.Value = 1.0 then
    Result := TGocciaNumberLiteralValue.InfinityValue  // atanh(1) = +Infinity
  else if NumberArg.Value = -1.0 then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue  // atanh(-1) = -Infinity
  else
    Result := TGocciaNumberLiteralValue.Create(ArcTanh(NumberArg.Value));
end;

function TGocciaMath.MathExpm1(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.expm1', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.Create(-1)  // expm1(-Infinity) = -1
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue  // expm1(+Infinity) = +Infinity
  else
    Result := TGocciaNumberLiteralValue.Create(Exp(NumberArg.Value) - 1.0);
end;

function TGocciaMath.MathFround(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
  SingleVal: Single;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.fround', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  else
  begin
    SingleVal := NumberArg.Value;
    Result := TGocciaNumberLiteralValue.Create(SingleVal);
  end;
end;

function TGocciaMath.MathHypot(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  X, Y: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 2, 'Math.hypot', ThrowError);

  X := TGocciaArgumentConverter.GetNumber(AArgs, 0);
  Y := TGocciaArgumentConverter.GetNumber(AArgs, 1);

  if X.IsNaN or Y.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if (X.IsInfinity or X.IsNegativeInfinity) or (Y.IsInfinity or Y.IsNegativeInfinity) then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else
    Result := TGocciaNumberLiteralValue.Create(Hypot(X.Value, Y.Value));
end;

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
    XInt := LongInt(Trunc(X.Value));
    YInt := LongInt(Trunc(Y.Value));
    Result := TGocciaNumberLiteralValue.Create(XInt * YInt);
  end;
end;

function TGocciaMath.MathLog1p(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.log1p', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue  // log1p(-Infinity) = NaN
  else if NumberArg.Value = -1.0 then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue  // log1p(-1) = -Infinity
  else if NumberArg.Value < -1.0 then
    Result := TGocciaNumberLiteralValue.NaNValue  // log1p(x < -1) = NaN
  else
    Result := TGocciaNumberLiteralValue.Create(Ln(1.0 + NumberArg.Value));
end;

function TGocciaMath.MathLog2(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.log2', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue  // log2(-Infinity) = NaN
  else if NumberArg.Value = 0 then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue  // log2(0) = -Infinity
  else if NumberArg.Value < 0 then
    Result := TGocciaNumberLiteralValue.NaNValue  // log2 of negative number = NaN
  else
    Result := TGocciaNumberLiteralValue.Create(Log2(NumberArg.Value));
end;

function TGocciaMath.MathClz32(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
  Value: LongWord;
  Count: Integer;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.clz32', ThrowError);

  NumberArg := AArgs.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN or NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.Create(32)
  else
  begin
    Value := LongWord(Trunc(NumberArg.Value));
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
    Result := TGocciaNumberLiteralValue.Create(Count);
  end;
end;

end.
