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

  // Methods: writable, non-enumerable, configurable
  FBuiltinObject.RegisterMethod(TGocciaNativeFunctionValue.Create(MathAbs, 'abs', 1));
  FBuiltinObject.RegisterMethod(TGocciaNativeFunctionValue.Create(MathFloor, 'floor', 1));
  FBuiltinObject.RegisterMethod(TGocciaNativeFunctionValue.Create(MathCeil, 'ceil', 1));
  FBuiltinObject.RegisterMethod(TGocciaNativeFunctionValue.Create(MathRound, 'round', 1));
  FBuiltinObject.RegisterMethod(TGocciaNativeFunctionValue.Create(MathMax, 'max', -1));
  FBuiltinObject.RegisterMethod(TGocciaNativeFunctionValue.Create(MathMin, 'min', -1));
  FBuiltinObject.RegisterMethod(TGocciaNativeFunctionValue.Create(MathPow, 'pow', 2));
  FBuiltinObject.RegisterMethod(TGocciaNativeFunctionValue.Create(MathSqrt, 'sqrt', 1));
  FBuiltinObject.RegisterMethod(TGocciaNativeFunctionValue.Create(MathRandom, 'random', 0));
  FBuiltinObject.RegisterMethod(TGocciaNativeFunctionValue.Create(MathClamp, 'clamp', 3));
  FBuiltinObject.RegisterMethod(TGocciaNativeFunctionValue.Create(MathSign, 'sign', 1));
  FBuiltinObject.RegisterMethod(TGocciaNativeFunctionValue.Create(MathTrunc, 'trunc', 1));
  FBuiltinObject.RegisterMethod(TGocciaNativeFunctionValue.Create(MathExp, 'exp', 1));
  FBuiltinObject.RegisterMethod(TGocciaNativeFunctionValue.Create(MathLog, 'log', 1));
  FBuiltinObject.RegisterMethod(TGocciaNativeFunctionValue.Create(MathLog10, 'log10', 1));

  AScope.DefineBuiltin(AName, FBuiltinObject);
end;

function TGocciaMath.MathAbs(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Value: Double;
begin
  if Args.Count <> 1 then
    ThrowError('Math.abs expects exactly 1 argument', 0, 0);

  Value := Args[0].ToNumber;

  if IsNaN(Value) then
    Result := TGocciaNumberValue.Create(NaN)
  else if IsInfinite(Value) then
    Result := TGocciaNumberValue.Create(Infinity)
  else
    Result := TGocciaNumberValue.Create(Abs(Value));
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
      Result := TGocciaNumberValue.Create(NaN);
      Exit;
    end;

    for I := 1 to Args.Count - 1 do
    begin
      Current := Args[I].ToNumber;

      // If any argument is NaN, return NaN
      if IsNaN(Current) then
      begin
        Result := TGocciaNumberValue.Create(NaN);
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
      Result := TGocciaNumberValue.Create(NaN);
      Exit;
    end;

    for I := 1 to Args.Count - 1 do
    begin
      Current := Args[I].ToNumber;

      // If any argument is NaN, return NaN
      if IsNaN(Current) then
      begin
        Result := TGocciaNumberValue.Create(NaN);
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
  Value: Double;
begin
  if Args.Count <> 1 then
    ThrowError('Math.sqrt expects exactly 1 argument', 0, 0);

  Value := Args[0].ToNumber;

  if IsNaN(Value) then
    Result := TGocciaNumberValue.Create(NaN)
  else if IsInfinite(Value) then
  begin
    if Value > 0 then
      Result := TGocciaNumberValue.Create(Infinity)
    else
      Result := TGocciaNumberValue.Create(NaN); // sqrt(-Infinity) = NaN
  end
  else if Value < 0 then
    Result := TGocciaNumberValue.Create(NaN) // sqrt of negative number = NaN
  else
    Result := TGocciaNumberValue.Create(Sqrt(Value));
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
    Result := TGocciaNumberValue.Create(NaN);
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
    Result := TGocciaNumberValue.Create(NaN)
  else
    Result := TGocciaNumberValue.Create(Sign(Value));
end;

function TGocciaMath.MathTrunc(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Value: Double;
begin
  if Args.Count <> 1 then
    ThrowError('Math.trunc expects exactly 1 argument', 0, 0);

  Value := Args[0].ToNumber;

  if IsNaN(Value) then
    Result := TGocciaNumberValue.Create(NaN)
  else if IsInfinite(Value) then
  begin
    if Value > 0 then
      Result := TGocciaNumberValue.Create(Infinity)
    else if Value < 0 then
      Result := TGocciaNumberValue.Create(-Infinity)
    else
      Result := TGocciaNumberValue.Create(NaN);
  end
  else
    Result := TGocciaNumberValue.Create(Trunc(Value));
end;

function TGocciaMath.MathExp(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Value: Double;
begin
  if Args.Count <> 1 then
    ThrowError('Math.exp expects exactly 1 argument', 0, 0);

  Value := Args[0].ToNumber;

  if IsNaN(Value) then
    Result := TGocciaNumberValue.Create(NaN)
  else
    Result := TGocciaNumberValue.Create(Exp(Value));
end;

function TGocciaMath.MathLog(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Value: Double;
begin
  if Args.Count <> 1 then
    ThrowError('Math.log expects exactly 1 argument', 0, 0);

  Value := Args[0].ToNumber;

  if IsNaN(Value) then
    Result := TGocciaNumberValue.Create(NaN)
  else if IsInfinite(Value) then
  begin
    if Value > 0 then
      Result := TGocciaNumberValue.Create(Infinity)
    else
      Result := TGocciaNumberValue.Create(NaN); // log(-Infinity) = NaN
  end
  else if Value = 0 then
    Result := TGocciaNumberValue.Create(-Infinity) // log(0) = -Infinity
  else if Value < 0 then
    Result := TGocciaNumberValue.Create(NaN) // log of negative number = NaN
  else
    Result := TGocciaNumberValue.Create(Ln(Value)); // Natural logarithm (base e)
end;

function TGocciaMath.MathLog10(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Value: Double;
begin
  if Args.Count <> 1 then
    ThrowError('Math.log10 expects exactly 1 argument', 0, 0);

  Value := Args[0].ToNumber;

  if IsNaN(Value) then
    Result := TGocciaNumberValue.Create(NaN)
  else if IsInfinite(Value) then
  begin
    if Value > 0 then
      Result := TGocciaNumberValue.Create(Infinity)
    else
      Result := TGocciaNumberValue.Create(NaN); // log10(-Infinity) = NaN
  end
  else if Value = 0 then
    Result := TGocciaNumberValue.Create(-Infinity) // log10(0) = -Infinity
  else if Value < 0 then
    Result := TGocciaNumberValue.Create(NaN) // log10 of negative number = NaN
  else
    Result := TGocciaNumberValue.Create(Log10(Value));
end;


end.
