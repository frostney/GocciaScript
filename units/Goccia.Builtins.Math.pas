unit Goccia.Builtins.Math;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Scope, Goccia.Error, Goccia.Values.NativeFunction, Goccia.Values.UndefinedValue, Goccia.Values.ObjectValue, Generics.Collections, Math, Goccia.Builtins.Base;

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

uses
  Goccia.Values.NumberValue;

constructor TGocciaMath.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
begin
  inherited Create(AName, AScope, AThrowError);

  // Constants: non-writable, non-enumerable, non-configurable
  FBuiltinObject.RegisterConstant('PI', TGocciaNumberValue.Create(Pi));
  FBuiltinObject.RegisterConstant('E', TGocciaNumberValue.Create(Exp(1)));
  FBuiltinObject.RegisterConstant('LN2', TGocciaNumberValue.Create(Ln(2)));
  FBuiltinObject.RegisterConstant('LN10', TGocciaNumberValue.Create(Ln(10)));
  FBuiltinObject.RegisterConstant('SQRT2', TGocciaNumberValue.Create(Sqrt(2)));

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
  NumberArg: TGocciaNumberValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.abs expects exactly 1 argument', 0, 0);

  NumberArg := TGocciaNumberValue.Create(Args[0].ToNumber);
  try
    if NumberArg.IsNaN then
      Result := TGocciaNumberValue.CreateNaN
    else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
      Result := TGocciaNumberValue.CreateInfinity
    else
      Result := TGocciaNumberValue.Create(Abs(NumberArg.Value));
  finally
    NumberArg.Free;
  end;
end;

function TGocciaMath.MathFloor(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.floor expects exactly 1 argument', 0, 0);
  Result := TGocciaNumberValue.Create(Floor(Args[0].ToNumber));
end;

function TGocciaMath.MathCeil(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.ceil expects exactly 1 argument', 0, 0);
  Result := TGocciaNumberValue.Create(Ceil(Args[0].ToNumber));
end;

function TGocciaMath.MathRound(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.round expects exactly 1 argument', 0, 0);
  Result := TGocciaNumberValue.Create(Round(Args[0].ToNumber));
end;

function TGocciaMath.MathMax(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  Max, Current: Double;
begin
  if Args.Count = 0 then
    Result := TGocciaNumberValue.Create(-Infinity)
  else
  begin
    Max := Args[0].ToNumber;

    // If any argument is NaN, return NaN
    if IsNaN(Max) then
    begin
      Result := TGocciaNumberValue.CreateNaN;
      Exit;
    end;

    for I := 1 to Args.Count - 1 do
    begin
      Current := Args[I].ToNumber;

      // If any argument is NaN, return NaN
      if IsNaN(Current) then
      begin
        Result := TGocciaNumberValue.CreateNaN;
        Exit;
      end;

      if Current > Max then
        Max := Current;
    end;
    Result := TGocciaNumberValue.Create(Max);
  end;
end;

function TGocciaMath.MathMin(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  Min, Current: Double;
begin
  if Args.Count = 0 then
    Result := TGocciaNumberValue.Create(Infinity)
  else
  begin
    Min := Args[0].ToNumber;

    // If any argument is NaN, return NaN
    if IsNaN(Min) then
    begin
      Result := TGocciaNumberValue.CreateNaN;
      Exit;
    end;

    for I := 1 to Args.Count - 1 do
    begin
      Current := Args[I].ToNumber;

      // If any argument is NaN, return NaN
      if IsNaN(Current) then
      begin
        Result := TGocciaNumberValue.CreateNaN;
        Exit;
      end;

      if Current < Min then
        Min := Current;
    end;
    Result := TGocciaNumberValue.Create(Min);
  end;
end;

function TGocciaMath.MathPow(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  if Args.Count <> 2 then
    ThrowError('Math.pow expects exactly 2 arguments', 0, 0);
  Result := TGocciaNumberValue.Create(
    Power(Args[0].ToNumber, Args[1].ToNumber));
end;

function TGocciaMath.MathSqrt(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.sqrt expects exactly 1 argument', 0, 0);

  NumberArg := TGocciaNumberValue.Create(Args[0].ToNumber);
  try
    if NumberArg.IsNaN then
      Result := TGocciaNumberValue.CreateNaN
    else if NumberArg.IsInfinity then
      Result := TGocciaNumberValue.CreateInfinity
    else if NumberArg.IsNegativeInfinity then
      Result := TGocciaNumberValue.CreateNaN // sqrt(-Infinity) = NaN
    else if NumberArg.Value < 0 then
      Result := TGocciaNumberValue.CreateNaN // sqrt of negative number = NaN
    else
      Result := TGocciaNumberValue.Create(Sqrt(NumberArg.Value));
  finally
    NumberArg.Free;
  end;
end;

function TGocciaMath.MathRandom(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberValue.Create(Random);
end;

function TGocciaMath.MathClamp(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Value, MinVal, MaxVal: Double;
begin
  if Args.Count <> 3 then
    ThrowError('Math.clamp expects exactly 3 arguments', 0, 0);

  Value := Args[0].ToNumber;
  MinVal := Args[1].ToNumber;
  MaxVal := Args[2].ToNumber;

  // If any argument is NaN, return NaN
  if IsNaN(Value) or IsNaN(MinVal) or IsNaN(MaxVal) then
  begin
    Result := TGocciaNumberValue.CreateNaN;
    Exit;
  end;

  // If min > max, throw RangeError
  if MinVal > MaxVal then
  begin
    ThrowError('RangeError: Invalid range in Math.clamp', 0, 0);
    Exit;
  end;

    // TODO: Handle negative zero properly - for now skip this special case

  // Clamp the value
  if Value < MinVal then
    Result := TGocciaNumberValue.Create(MinVal)
  else if Value > MaxVal then
    Result := TGocciaNumberValue.Create(MaxVal)
  else
    Result := TGocciaNumberValue.Create(Value);
end;

function TGocciaMath.MathSign(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Value: Double;
begin
  if Args.Count <> 1 then
    ThrowError('Math.sign expects exactly 1 argument', 0, 0);

  Value := Args[0].ToNumber;

  if IsNaN(Value) then
    Result := TGocciaNumberValue.CreateNaN
  else
    Result := TGocciaNumberValue.Create(Sign(Value));
end;

function TGocciaMath.MathTrunc(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.trunc expects exactly 1 argument', 0, 0);

  NumberArg := TGocciaNumberValue.Create(Args[0].ToNumber);
  try
    if NumberArg.IsNaN then
      Result := TGocciaNumberValue.CreateNaN
    else if NumberArg.IsInfinity then
      Result := TGocciaNumberValue.CreateInfinity
    else if NumberArg.IsNegativeInfinity then
      Result := TGocciaNumberValue.CreateNegativeInfinity
    else
      Result := TGocciaNumberValue.Create(Trunc(NumberArg.Value));
  finally
    NumberArg.Free;
  end;
end;

function TGocciaMath.MathExp(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Value: Double;
begin
  if Args.Count <> 1 then
    ThrowError('Math.exp expects exactly 1 argument', 0, 0);

  Value := Args[0].ToNumber;

  if IsNaN(Value) then
    Result := TGocciaNumberValue.CreateNaN
  else
    Result := TGocciaNumberValue.Create(Exp(Value));
end;

function TGocciaMath.MathLog(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.log expects exactly 1 argument', 0, 0);

  NumberArg := TGocciaNumberValue.Create(Args[0].ToNumber);
  try
    if NumberArg.IsNaN then
      Result := TGocciaNumberValue.CreateNaN
    else if NumberArg.IsInfinity then
      Result := TGocciaNumberValue.CreateInfinity
    else if NumberArg.IsNegativeInfinity then
      Result := TGocciaNumberValue.CreateNaN // log(-Infinity) = NaN
    else if NumberArg.Value = 0 then
      Result := TGocciaNumberValue.CreateNegativeInfinity // log(0) = -Infinity
    else if NumberArg.Value < 0 then
      Result := TGocciaNumberValue.CreateNaN // log of negative number = NaN
    else
      Result := TGocciaNumberValue.Create(Ln(NumberArg.Value)); // Natural logarithm (base e)
  finally
    NumberArg.Free;
  end;
end;

function TGocciaMath.MathLog10(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.log10 expects exactly 1 argument', 0, 0);

  NumberArg := TGocciaNumberValue.Create(Args[0].ToNumber);
  try
    if NumberArg.IsNaN then
      Result := TGocciaNumberValue.CreateNaN
    else if NumberArg.IsInfinity then
      Result := TGocciaNumberValue.CreateInfinity
    else if NumberArg.IsNegativeInfinity then
      Result := TGocciaNumberValue.CreateNaN // log10(-Infinity) = NaN
    else if NumberArg.Value = 0 then
      Result := TGocciaNumberValue.CreateNegativeInfinity // log10(0) = -Infinity
    else if NumberArg.Value < 0 then
      Result := TGocciaNumberValue.CreateNaN // log10 of negative number = NaN
    else
      Result := TGocciaNumberValue.Create(Log10(NumberArg.Value));
  finally
    NumberArg.Free;
  end;
end;

function TGocciaMath.MathSin(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.sin expects exactly 1 argument', 0, 0);

  NumberArg := TGocciaNumberValue.Create(Args[0].ToNumber);
  try
    if NumberArg.IsNaN then
      Result := TGocciaNumberValue.CreateNaN
    else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
      Result := TGocciaNumberValue.CreateNaN  // sin(±Infinity) = NaN
    else
      Result := TGocciaNumberValue.Create(Sin(NumberArg.Value));
  finally
    NumberArg.Free;
  end;
end;

function TGocciaMath.MathCos(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.cos expects exactly 1 argument', 0, 0);

  NumberArg := TGocciaNumberValue.Create(Args[0].ToNumber);
  try
    if NumberArg.IsNaN then
      Result := TGocciaNumberValue.CreateNaN
    else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
      Result := TGocciaNumberValue.CreateNaN  // cos(±Infinity) = NaN
    else
      Result := TGocciaNumberValue.Create(Cos(NumberArg.Value));
  finally
    NumberArg.Free;
  end;
end;

function TGocciaMath.MathTan(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.tan expects exactly 1 argument', 0, 0);

  NumberArg := TGocciaNumberValue.Create(Args[0].ToNumber);
  try
    if NumberArg.IsNaN then
      Result := TGocciaNumberValue.CreateNaN
    else if NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
      Result := TGocciaNumberValue.CreateNaN  // tan(±Infinity) = NaN
    else
      Result := TGocciaNumberValue.Create(Tan(NumberArg.Value));
  finally
    NumberArg.Free;
  end;
end;

end.
