unit Goccia.Builtins.Math;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Core, Goccia.Scope, Goccia.Error, Goccia.Values.NativeFunction, Goccia.Values.Primitives, Goccia.Values.ObjectValue, Generics.Collections, Math, Goccia.Builtins.Base;

type
  TGocciaMath = class(TGocciaBuiltin)
  protected
    // Native methods
    function MathAbs(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathFloor(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathCeil(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathRound(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathMax(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathMin(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathPow(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathSqrt(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathRandom(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathClamp(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathSign(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathTrunc(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathExp(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathLog(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathLog10(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathSin(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathCos(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathTan(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;

  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
  end;

implementation

uses Goccia.Values.ClassHelper;

constructor TGocciaMath.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
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

function TGocciaMath.MathAbs(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.abs expects exactly 1 argument', 0, 0);

  NumberArg := TGocciaNumberLiteralValue.Create(Args[0].ToNumberLiteral.Value);
  try
    if NumberArg.IsNaN then
      Result := TGocciaNumberLiteralValue.NaNValue
    else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
      Result := TGocciaNumberLiteralValue.InfinityValue
    else
      Result := TGocciaNumberLiteralValue.Create(Abs(NumberArg.Value));
  finally
    NumberArg.Free;
  end;
end;

function TGocciaMath.MathFloor(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.floor expects exactly 1 argument', 0, 0);
  Result := TGocciaNumberLiteralValue.Create(Floor(Args[0].ToNumberLiteral.Value));
end;

function TGocciaMath.MathCeil(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.ceil expects exactly 1 argument', 0, 0);
  Result := TGocciaNumberLiteralValue.Create(Ceil(Args[0].ToNumberLiteral.Value));
end;

function TGocciaMath.MathRound(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.round expects exactly 1 argument', 0, 0);
  Result := TGocciaNumberLiteralValue.Create(Round(Args[0].ToNumberLiteral.Value));
end;

function TGocciaMath.MathMax(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  Max, Current: Double;
begin
  if Args.Count = 0 then
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue
  else
  begin
    Max := Args[0].ToNumberLiteral.Value;

    // If any argument is NaN, return NaN
    if IsNaN(Max) then
    begin
      Result := TGocciaNumberLiteralValue.NaNValue;
      Exit;
    end;

    for I := 1 to Args.Count - 1 do
    begin
      Current := Args[I].ToNumberLiteral.Value;

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

function TGocciaMath.MathMin(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  Min, Current: Double;
begin
  if Args.Count = 0 then
    Result := TGocciaNumberLiteralValue.Create(Infinity)
  else
  begin
    Min := Args[0].ToNumberLiteral.Value;

    // If any argument is NaN, return NaN
    if IsNaN(Min) then
    begin
      Result := TGocciaNumberLiteralValue.NaNValue;
      Exit;
    end;

    for I := 1 to Args.Count - 1 do
    begin
      Current := Args[I].ToNumberLiteral.Value;

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

function TGocciaMath.MathPow(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  if Args.Count <> 2 then
    ThrowError('Math.pow expects exactly 2 arguments', 0, 0);
  Result := TGocciaNumberLiteralValue.Create(
    Power(Args[0].ToNumberLiteral.Value, Args[1].ToNumberLiteral.Value));
end;

function TGocciaMath.MathSqrt(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.sqrt expects exactly 1 argument', 0, 0);

  NumberArg := TGocciaNumberLiteralValue.Create(Args[0].ToNumberLiteral.Value);
  try
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
  finally
    NumberArg.Free;
  end;
end;

function TGocciaMath.MathRandom(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(Random);
end;

function TGocciaMath.MathClamp(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Value, MinVal, MaxVal: TGocciaNumberLiteralValue;
begin
  if Args.Count <> 3 then
    ThrowError('Math.clamp expects exactly 3 arguments', 0, 0);

  Value := Args[0].ToNumberLiteral;
  MinVal := Args[1].ToNumberLiteral;
  MaxVal := Args[2].ToNumberLiteral;

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

function TGocciaMath.MathSign(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberLiteral: TGocciaNumberLiteralValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.sign expects exactly 1 argument', 0, 0);

  NumberLiteral := Args[0].ToNumberLiteral;

  if NumberLiteral.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else
    Result := TGocciaNumberLiteralValue.Create(Sign(NumberLiteral.Value));
end;

function TGocciaMath.MathTrunc(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.trunc expects exactly 1 argument', 0, 0);

  NumberArg := TGocciaNumberLiteralValue.Create(Args[0].ToNumberLiteral.Value);
  try
    if NumberArg.IsNaN then
      Result := TGocciaNumberLiteralValue.NaNValue
    else if NumberArg.IsInfinity then
      Result := TGocciaNumberLiteralValue.InfinityValue
    else if NumberArg.IsNegativeInfinity then
      Result := TGocciaNumberLiteralValue.NegativeInfinityValue
    else
      Result := TGocciaNumberLiteralValue.Create(Trunc(NumberArg.Value));
  finally
    NumberArg.Free;
  end;
end;

function TGocciaMath.MathExp(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberLiteral: TGocciaNumberLiteralValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.exp expects exactly 1 argument', 0, 0);

  NumberLiteral := Args[0].ToNumberLiteral;

  if NumberLiteral.IsNaN then
    Result := TGocciaNumberLiteralValue.NaNValue
  else
    Result := TGocciaNumberLiteralValue.Create(Exp(NumberLiteral.Value));
end;

function TGocciaMath.MathLog(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.log expects exactly 1 argument', 0, 0);

  NumberArg := Args[0].ToNumberLiteral;
  try
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
  finally
    NumberArg.Free;
  end;
end;

function TGocciaMath.MathLog10(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.log10 expects exactly 1 argument', 0, 0);

  NumberArg := Args[0].ToNumberLiteral;
  try
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
  finally
    NumberArg.Free;
  end;
end;

function TGocciaMath.MathSin(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.sin expects exactly 1 argument', 0, 0);

  NumberArg := Args[0].ToNumberLiteral;
  try
    if NumberArg.IsNaN then
      Result := TGocciaNumberLiteralValue.NaNValue
    else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
      Result := TGocciaNumberLiteralValue.NaNValue  // sin(±Infinity) = NaN
    else
      Result := TGocciaNumberLiteralValue.Create(Sin(NumberArg.Value));
  finally
    NumberArg.Free;
  end;
end;

function TGocciaMath.MathCos(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.cos expects exactly 1 argument', 0, 0);

  NumberArg := Args[0].ToNumberLiteral;
  try
    if NumberArg.IsNaN then
      Result := TGocciaNumberLiteralValue.NaNValue
    else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
      Result := TGocciaNumberLiteralValue.NaNValue  // cos(±Infinity) = NaN
    else
      Result := TGocciaNumberLiteralValue.Create(Cos(NumberArg.Value));
  finally
    NumberArg.Free;
  end;
end;

function TGocciaMath.MathTan(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.tan expects exactly 1 argument', 0, 0);

  NumberArg := Args[0].ToNumberLiteral;
  try
    if NumberArg.IsNaN then
      Result := TGocciaNumberLiteralValue.NaNValue
    else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
      Result := TGocciaNumberLiteralValue.NaNValue  // tan(±Infinity) = NaN
    else
      Result := TGocciaNumberLiteralValue.Create(Tan(NumberArg.Value));
  finally
    NumberArg.Free;
  end;
end;

end.
