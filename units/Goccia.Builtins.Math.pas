unit Goccia.Builtins.Math;

{$I Goccia.inc}

interface

uses
  Goccia.Scope, Goccia.Error, Goccia.Error.ThrowErrorCallback, Goccia.Values.NativeFunction, Goccia.Values.Primitives, Goccia.Values.ObjectValue, Generics.Collections, Math, Goccia.Builtins.Base, Goccia.Arguments.Collection, Goccia.Arguments.Validator, Goccia.Arguments.Converter;

type
  TGocciaMath = class(TGocciaBuiltin)
  protected
    // Native methods
    function MathAbs(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MathFloor(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MathCeil(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MathRound(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MathMax(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MathMin(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MathPow(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MathSqrt(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MathRandom(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MathClamp(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MathSign(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MathTrunc(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MathExp(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MathLog(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MathLog10(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MathSin(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MathCos(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MathTan(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses Goccia.Values.ClassHelper;

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

  AScope.DefineBuiltin(AName, FBuiltinObject);
end;

function TGocciaMath.MathAbs(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'Math.abs', ThrowError);

  NumberArg := Args.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else
    Result := TGocciaNumberLiteralValue.Create(Abs(NumberArg.Value));
end;

function TGocciaMath.MathFloor(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'Math.floor', ThrowError);
  NumberArg := TGocciaArgumentConverter.GetNumber(Args, 0);
  Result := TGocciaNumberLiteralValue.Create(Floor(NumberArg.Value));
end;

function TGocciaMath.MathCeil(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'Math.ceil', ThrowError);
  NumberArg := TGocciaArgumentConverter.GetNumber(Args, 0);
  Result := TGocciaNumberLiteralValue.Create(Ceil(NumberArg.Value));
end;

function TGocciaMath.MathRound(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'Math.round', ThrowError);
  Result := TGocciaNumberLiteralValue.Create(Round(Args.GetElement(0).ToNumberLiteral.Value));
end;

function TGocciaMath.MathMax(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  Max, Current: Double;
begin
  if Args.Length = 0 then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  else
  begin
    Max := Args.GetElement(0).ToNumberLiteral.Value;

    // If any argument is NaN, return NaN
    if IsNaN(Max) then
    begin
      Result := TGocciaNumberLiteralValue.NaNValue;
      Exit;
    end;

    for I := 1 to Args.Length - 1 do
    begin
      Current := Args.GetElement(I).ToNumberLiteral.Value;

      // If any argument is NaN, return NaN
      if IsNaN(Current) then
      begin
        Result := TGocciaNumberLiteralValue.NaNValue;
        Exit;
      end;

      if Current > Max then
        Max := Current;
    end;
    Result := TGocciaNumberLiteralValue.Create(Max);
  end;
end;

function TGocciaMath.MathMin(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  Min, Current: Double;
begin
  if Args.Length = 0 then
    Result := TGocciaNumberLiteralValue.Create(Infinity)
  else
  begin
    Min := Args.GetElement(0).ToNumberLiteral.Value;

    // If any argument is NaN, return NaN
    if IsNaN(Min) then
    begin
      Result := TGocciaNumberLiteralValue.NaNValue;
      Exit;
    end;

    for I := 1 to Args.Length - 1 do
    begin
      Current := Args.GetElement(I).ToNumberLiteral.Value;

      // If any argument is NaN, return NaN
      if IsNaN(Current) then
      begin
        Result := TGocciaNumberLiteralValue.NaNValue;
        Exit;
      end;

      if Current < Min then
        Min := Current;
    end;
    Result := TGocciaNumberLiteralValue.Create(Min);
  end;
end;

function TGocciaMath.MathPow(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Base, Exponent: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 2, 'Math.pow', ThrowError);
  Base := TGocciaArgumentConverter.GetNumber(Args, 0);
  Exponent := TGocciaArgumentConverter.GetNumber(Args, 1);
  Result := TGocciaNumberLiteralValue.Create(Power(Base.Value, Exponent.Value));
end;

function TGocciaMath.MathSqrt(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'Math.sqrt', ThrowError);

  NumberArg := Args.GetElement(0).ToNumberLiteral;
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

function TGocciaMath.MathRandom(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireNone(Args, 'Math.random', ThrowError);
  Result := TGocciaNumberLiteralValue.Create(Random);
end;

function TGocciaMath.MathClamp(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Value, MinVal, MaxVal: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 3, 'Math.clamp', ThrowError);

  Value := Args.GetElement(0).ToNumberLiteral;
  MinVal := Args.GetElement(1).ToNumberLiteral;
  MaxVal := Args.GetElement(2).ToNumberLiteral;

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

    // TODO: Handle negative zero properly - for now skip this special case

  // Clamp the value
  if Value.IsLessThan(MinVal).Value then
    Result := TGocciaNumberLiteralValue.Create(MinVal.Value)
  else if Value.IsGreaterThan(MaxVal).Value then
    Result := TGocciaNumberLiteralValue.Create(MaxVal.Value)
  else
    Result := TGocciaNumberLiteralValue.Create(Value.Value);
end;

function TGocciaMath.MathSign(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberLiteral: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'Math.sign', ThrowError);

  NumberLiteral := Args.GetElement(0).ToNumberLiteral;

  if NumberLiteral.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else
    Result := TGocciaNumberLiteralValue.Create(Sign(NumberLiteral.Value));
end;

function TGocciaMath.MathTrunc(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'Math.trunc', ThrowError);

  NumberArg := Args.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else if NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  else
    Result := TGocciaNumberLiteralValue.Create(Trunc(NumberArg.Value));
end;

function TGocciaMath.MathExp(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberLiteral: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'Math.exp', ThrowError);

  NumberLiteral := Args.GetElement(0).ToNumberLiteral;

  if NumberLiteral.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else
    Result := TGocciaNumberLiteralValue.Create(Exp(NumberLiteral.Value));
end;

function TGocciaMath.MathLog(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'Math.log', ThrowError);

  NumberArg := Args.GetElement(0).ToNumberLiteral;
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

function TGocciaMath.MathLog10(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'Math.log10', ThrowError);

  NumberArg := Args.GetElement(0).ToNumberLiteral;
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

function TGocciaMath.MathSin(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'Math.sin', ThrowError);

  NumberArg := Args.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue  // sin(±Infinity) = NaN
  else
    Result := TGocciaNumberLiteralValue.Create(Sin(NumberArg.Value));
end;

function TGocciaMath.MathCos(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'Math.cos', ThrowError);

  NumberArg := Args.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue  // cos(±Infinity) = NaN
  else
    Result := TGocciaNumberLiteralValue.Create(Cos(NumberArg.Value));
end;

function TGocciaMath.MathTan(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'Math.tan', ThrowError);

  NumberArg := Args.GetElement(0).ToNumberLiteral;
  if NumberArg.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
    Result := TGocciaNumberLiteralValue.NaNValue  // tan(±Infinity) = NaN
  else
    Result := TGocciaNumberLiteralValue.Create(Tan(NumberArg.Value));
end;

end.
