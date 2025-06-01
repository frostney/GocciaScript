unit Goccia.Builtins.Math;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Scope, Goccia.Error, Goccia.Values.NativeFunction, Goccia.Values.Undefined, Goccia.Values.ObjectValue, Generics.Collections, Math;

type
  TGocciaMath = class
  private
    FName: string;
    FMath: TGocciaObjectValue;
    FThrowError: TGocciaThrowError;
  public
    constructor Create(const AName: string; AScope: TGocciaScope; AThrowError: TGocciaThrowError);
    destructor Destroy; override;

    // Native methods
    function MathAbs(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathFloor(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathCeil(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathRound(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathMax(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathMin(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathPow(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function MathSqrt(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;

    property Name: string read FName;
    property ThrowError: TGocciaThrowError read FThrowError;
  end;

implementation

uses
  Goccia.Values.Number;

constructor TGocciaMath.Create(const AName: string; AScope: TGocciaScope; AThrowError: TGocciaThrowError);
begin
  FName := AName;
  FMath := TGocciaObjectValue.Create;
  FThrowError := AThrowError;

  FMath.SetProperty('PI', TGocciaNumberValue.Create(Pi));
  FMath.SetProperty('E', TGocciaNumberValue.Create(Exp(1)));
  FMath.SetProperty('abs', TGocciaNativeFunctionValue.Create(MathAbs, 'abs', 1));
  FMath.SetProperty('floor', TGocciaNativeFunctionValue.Create(MathFloor, 'floor', 1));
  FMath.SetProperty('ceil', TGocciaNativeFunctionValue.Create(MathCeil, 'ceil', 1));
  FMath.SetProperty('round', TGocciaNativeFunctionValue.Create(MathRound, 'round', 1));
  FMath.SetProperty('max', TGocciaNativeFunctionValue.Create(MathMax, 'max', -1));
  FMath.SetProperty('min', TGocciaNativeFunctionValue.Create(MathMin, 'min', -1));
  FMath.SetProperty('pow', TGocciaNativeFunctionValue.Create(MathPow, 'pow', 2));
  FMath.SetProperty('sqrt', TGocciaNativeFunctionValue.Create(MathSqrt, 'sqrt', 1));

  AScope.SetValue(AName, FMath);
end;

destructor TGocciaMath.Destroy;
begin
  FMath.Free;
  inherited;
end;

function TGocciaMath.MathAbs(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  if Args.Count <> 1 then
    ThrowError('Math.abs expects exactly 1 argument', 0, 0);
  Result := TGocciaNumberValue.Create(Abs(Args[0].ToNumber));
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
    for I := 1 to Args.Count - 1 do
    begin
      Current := Args[I].ToNumber;
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
    for I := 1 to Args.Count - 1 do
    begin
      Current := Args[I].ToNumber;
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
begin
  if Args.Count <> 1 then
    ThrowError('Math.sqrt expects exactly 1 argument', 0, 0);
  Result := TGocciaNumberValue.Create(Sqrt(Args[0].ToNumber));
end;

end.