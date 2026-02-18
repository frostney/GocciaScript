unit Goccia.Builtins.Math;

{$I Goccia.inc}

interface

uses
  Generics.Collections,
  Math,

  Goccia.Arguments.Collection,
  Goccia.Arguments.Converter,
  Goccia.Arguments.Validator,
  Goccia.Builtins.Base,
  Goccia.Error,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.NativeFunction,
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

  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  Goccia.Values.ClassHelper;

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
  Result := TGocciaNumberLiteralValue.Create(Floor(NumberArg.Value));
end;

function TGocciaMath.MathCeil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.ceil', ThrowError);
  NumberArg := TGocciaArgumentConverter.GetNumber(AArgs, 0);
  Result := TGocciaNumberLiteralValue.Create(Ceil(NumberArg.Value));
end;

function TGocciaMath.MathRound(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'Math.round', ThrowError);
  Result := TGocciaNumberLiteralValue.Create(Round(AArgs.GetElement(0).ToNumberLiteral.Value));
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

end.
