unit Goccia.Values.ArrayValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Values.ObjectValue, Goccia.Values.FunctionValue, Goccia.Values.NativeFunction, Goccia.Error, Generics.Collections, Math, SysUtils;

type
  TGocciaArrayValue = class(TGocciaObjectValue)
  private
    function GetLength(const AObject: TGocciaObjectValue): TGocciaValue;

    function ArrayMap(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayFilter(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayReduce(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayForEach(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;

    procedure ThrowError(const Message: string; const Args: array of const); overload;
    procedure ThrowError(const Message: string); overload;
  protected
    FElements: TObjectList<TGocciaValue>;
  public
    constructor Create;
    destructor Destroy; override;
    function ToString: string; override;
    function ToBoolean: Boolean; override;
    function ToNumber: Double; override;
    function TypeName: string; override;
    property Elements: TObjectList<TGocciaValue> read FElements;
  end;

implementation

uses
  Goccia.Logger, Goccia.Values.NumberValue, Goccia.Values.BooleanValue, Goccia.Values.UndefinedValue, Goccia.Evaluator;

constructor TGocciaArrayValue.Create;
begin
  inherited Create;

  FElements := TObjectList<TGocciaValue>.Create(False);
  FPrototype := TGocciaObjectValue.Create;

  FPrototype.SetComputedProperty('length', GetLength);
  FPrototype.SetProperty('map', TGocciaNativeFunctionValue.Create(ArrayMap, 'map', 1));
  FPrototype.SetProperty('filter', TGocciaNativeFunctionValue.Create(ArrayFilter, 'filter', 1));
  FPrototype.SetProperty('reduce', TGocciaNativeFunctionValue.Create(ArrayReduce, 'reduce', 1));
  FPrototype.SetProperty('forEach', TGocciaNativeFunctionValue.Create(ArrayForEach, 'forEach', 1));
end;

destructor TGocciaArrayValue.Destroy;
begin
  FElements.Free;
  inherited;
end;

procedure TGocciaArrayValue.ThrowError(const Message: string; const Args: array of const);
begin
  raise TGocciaError.Create(Format(Message, Args), 0, 0, '', nil);
end;

procedure TGocciaArrayValue.ThrowError(const Message: string);
begin
  ThrowError(Message, []);
end;

function TGocciaArrayValue.GetLength(const AObject: TGocciaObjectValue): TGocciaValue;
begin
  Result := TGocciaNumberValue.Create(FElements.Count);
end;

function TGocciaArrayValue.ArrayMap(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  ResultArray: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TObjectList<TGocciaValue>;
  I: Integer;
begin
  TGocciaLogger.Debug('ArrayMap: Entered');
  TGocciaLogger.Debug('  ThisValue type: %s', [ThisValue.ClassName]);
  TGocciaLogger.Debug('  Args.Count: %d', [Args.Count]);

  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.map called on non-array');

  if Args.Count < 1 then
    ThrowError('Array.map expects callback function');

  Callback := Args[0];

  if not ((Callback is TGocciaFunctionValue) or (Callback is TGocciaNativeFunctionValue)) then
    ThrowError('Callback must be a function');

  ResultArray := TGocciaArrayValue.Create;

  for I := 0 to FElements.Count - 1 do
  begin
    CallArgs := TObjectList<TGocciaValue>.Create(False);
    CallArgs.Add(FElements[I]);
    CallArgs.Add(TGocciaNumberValue.Create(I));

    if Callback is TGocciaNativeFunctionValue then
      ResultArray.Elements.Add(TGocciaNativeFunctionValue(Callback).NativeFunction(CallArgs, ThisValue))
    else if Callback is TGocciaFunctionValue then
      ResultArray.Elements.Add(TGocciaFunctionValue(Callback).Call(CallArgs, ThisValue));
  end;

  TGocciaLogger.Debug('ArrayMap: Returning result type: %s', [ResultArray.ClassName]);
  TGocciaLogger.Debug('ArrayMap: Result ToString: %s', [ResultArray.ToString]);
  Result := ResultArray;
end;

function TGocciaArrayValue.ArrayFilter(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Callback: TGocciaValue;
  ResultArray: TGocciaArrayValue;
  CallArgs: TObjectList<TGocciaValue>;
  PredicateResult: TGocciaValue;
  I: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.filter called on non-array');

  if Args.Count < 1 then
    ThrowError('Array.filter expects callback function');

  Callback := Args[0];

  if not ((Callback is TGocciaFunctionValue) or (Callback is TGocciaNativeFunctionValue)) then
    ThrowError('Callback must be a function');

  ResultArray := TGocciaArrayValue.Create;

  for I := 0 to Elements.Count - 1 do
  begin
    CallArgs := TObjectList<TGocciaValue>.Create(False);
    try
      // Add the current element as first argument
      CallArgs.Add(Elements[I]);
      // Add the index as second argument
      CallArgs.Add(TGocciaNumberValue.Create(I));

      if Callback is TGocciaNativeFunctionValue then
        PredicateResult := TGocciaNativeFunctionValue(Callback).NativeFunction(CallArgs, ThisValue)
      else if Callback is TGocciaFunctionValue then
      begin
        PredicateResult := TGocciaFunctionValue(Callback).Call(CallArgs, ThisValue);
      end;

      if PredicateResult is TGocciaBooleanValue then
      begin
        if TGocciaBooleanValue(PredicateResult).Value then
          ResultArray.Elements.Add(Elements[I]);
      end
      else
        ThrowError('Filter callback must return boolean');
    finally
      CallArgs.Free;
    end;
  end;

  Result := ResultArray;
end;

function TGocciaArrayValue.ArrayReduce(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  Accumulator: TGocciaValue;
  CallArgs: TObjectList<TGocciaValue>;
  I, StartIndex: Integer;
begin
  if Args.Count < 1 then
    ThrowError('Array.reduce expects array as this value');

  if not (Args[0] is TGocciaArrayValue) then
    ThrowError('Array.reduce called on non-array');

  if Args.Count < 2 then
    ThrowError('Array.reduce expects callback function');

  Arr := TGocciaArrayValue(Args[0]);
  Callback := Args[1];

  if not ((Callback is TGocciaFunctionValue) or (Callback is TGocciaNativeFunctionValue)) then
    ThrowError('Callback must be a function');

  if Args.Count >= 3 then
  begin
    Accumulator := Args[2];
    StartIndex := 0;
  end
  else
  begin
    if Arr.Elements.Count = 0 then
      ThrowError('Reduce of empty array with no initial value');
    Accumulator := Arr.Elements[0];
    StartIndex := 1;
  end;

  CallArgs := TObjectList<TGocciaValue>.Create(False);
  for I := StartIndex to Elements.Count - 1 do
  begin
    CallArgs.Add(Accumulator);
    CallArgs.Add(Elements[I]);
    CallArgs.Add(TGocciaNumberValue.Create(I));
  end;

  if Callback is TGocciaNativeFunctionValue then
    Result := TGocciaNativeFunctionValue(Callback).NativeFunction(CallArgs, ThisValue)
  else if Callback is TGocciaFunctionValue then
  begin
    Result := TGocciaFunctionValue(Callback).Call(CallArgs, ThisValue);
    Exit;
  end;
end;

function TGocciaArrayValue.ArrayForEach(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TObjectList<TGocciaValue>;
  I: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.forEach called on non-array');

  if Args.Count < 1 then
    ThrowError('Array.forEach expects callback function');

  Arr := TGocciaArrayValue(ThisValue);
  Callback := Args[0];

  if not ((Callback is TGocciaFunctionValue) or (Callback is TGocciaNativeFunctionValue)) then
    ThrowError('Callback must be a function');

  CallArgs := TObjectList<TGocciaValue>.Create(False);
  for I := 0 to Elements.Count - 1 do
  begin
    CallArgs.Add(Elements[I]);
    CallArgs.Add(TGocciaNumberValue.Create(I));

    if Callback is TGocciaNativeFunctionValue then
      TGocciaNativeFunctionValue(Callback).NativeFunction(CallArgs, ThisValue)
    else if Callback is TGocciaFunctionValue then
      TGocciaFunctionValue(Callback).Call(CallArgs, ThisValue);
  end;

  Result := TGocciaUndefinedValue.Create;
end;

function TGocciaArrayValue.ToString: string;
var
  I: Integer;
begin
  Result := '[';
  for I := 0 to FElements.Count - 1 do
  begin
    if I > 0 then
      Result := Result + ', ';
    Result := Result + FElements[I].ToString;
  end;
  Result := Result + ']';
end;

function TGocciaArrayValue.ToBoolean: Boolean;
begin
  Result := True;
end;

function TGocciaArrayValue.ToNumber: Double;
begin
  if FElements.Count = 0 then
    Result := NaN
  else
    Result := FElements[0].ToNumber;
end;

function TGocciaArrayValue.TypeName: string;
begin
  Result := 'object';
end;

end.