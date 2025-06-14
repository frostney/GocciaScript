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
    function ArraySome(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayEvery(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayJoin(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayIncludes(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;

    function ArrayToReversed(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayToSorted(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;

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
    function GetProperty(const AName: string): TGocciaValue;
    property Elements: TObjectList<TGocciaValue> read FElements;
  end;

  function DefaultCompare(constref A, B: TGocciaValue): Integer;

implementation

uses
  Goccia.Logger, Goccia.Values.NumberValue, Goccia.Values.BooleanValue, Goccia.Values.UndefinedValue, Goccia.Evaluator, Goccia.Values.StringValue, Generics.Defaults;

function DefaultCompare(constref A, B: TGocciaValue): Integer;
var
  NumA, NumB: Double;
  StrA, StrB: string;
begin
  if A is TGocciaNumberValue then
  begin
    NumA := A.ToNumber;
    NumB := B.ToNumber;
    if NumA < NumB then
      Result := -1
    else if NumA > NumB then
      Result := 1
    else
      Result := 0;
  end
  else if A is TGocciaStringValue then
  begin
    StrA := A.ToString;
    StrB := B.ToString;
    Result := CompareStr(StrA, StrB);
  end
  else if A is TGocciaBooleanValue then
  begin
    if A.ToBoolean = B.ToBoolean then
      Result := 0
    else if A.ToBoolean and not B.ToBoolean then
      Result := 1
    else
      Result := -1;
  end
  else
    Result := 0;
end;

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
  FPrototype.SetProperty('some', TGocciaNativeFunctionValue.Create(ArraySome, 'some', 1));
  FPrototype.SetProperty('every', TGocciaNativeFunctionValue.Create(ArrayEvery, 'every', 1));
  FPrototype.SetProperty('join', TGocciaNativeFunctionValue.Create(ArrayJoin, 'join', 1));
  FPrototype.SetProperty('includes', TGocciaNativeFunctionValue.Create(ArrayIncludes, 'includes', 1));
  FPrototype.SetProperty('toReversed', TGocciaNativeFunctionValue.Create(ArrayToReversed, 'toReversed', 0));
  FPrototype.SetProperty('toSorted', TGocciaNativeFunctionValue.Create(ArrayToSorted, 'toSorted', 0));
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
  ElementCount: Integer;
  LocalElements: TObjectList<TGocciaValue>;
  LocalThisValue: TGocciaValue;
begin
  Logger.Debug('ArrayMap: Starting with ThisValue type: %s, pointer: %p', [ThisValue.ClassName, Pointer(ThisValue)]);

  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.map called on non-array');

  if Args.Count < 1 then
    ThrowError('Array.map expects callback function');

  Callback := Args[0];

  if not ((Callback is TGocciaFunctionValue) or (Callback is TGocciaNativeFunctionValue)) then
    ThrowError('Callback must be a function');

  ResultArray := TGocciaArrayValue.Create;

  ElementCount := FElements.Count;

  LocalThisValue := ThisValue;

  LocalElements := TObjectList<TGocciaValue>.Create(False);
  try
    for I := 0 to ElementCount - 1 do
      LocalElements.Add(FElements[I]);

    for I := 0 to ElementCount - 1 do
    begin
      Logger.Debug('ArrayMap: Iteration %d, about to call with ThisValue type: %s, pointer: %p', [I, LocalThisValue.ClassName, Pointer(LocalThisValue)]);

      CallArgs := TObjectList<TGocciaValue>.Create(False);
      try
        CallArgs.Add(LocalElements[I]);
        CallArgs.Add(TGocciaNumberValue.Create(I));

        if Callback is TGocciaNativeFunctionValue then
          ResultArray.Elements.Add(TGocciaNativeFunctionValue(Callback).Call(CallArgs, LocalThisValue))
        else if Callback is TGocciaFunctionValue then
          ResultArray.Elements.Add(TGocciaFunctionValue(Callback).Call(CallArgs, LocalThisValue))
        else
          ThrowError('Callback must be a function');

        Logger.Debug('ArrayMap: After iteration %d, LocalThisValue type: %s, pointer: %p', [I, LocalThisValue.ClassName, Pointer(LocalThisValue)]);
      finally
        CallArgs.Free;
      end;
    end;

    Result := ResultArray;
  finally
    LocalElements.Free;
  end;
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

function TGocciaArrayValue.ArrayJoin(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Separator: string;
  I: Integer;
  ResultString: string;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.join called on non-array');

  Arr := TGocciaArrayValue(ThisValue);

  if Args.Count < 1 then
    Separator := ','
  else
    Separator := Args[0].ToString;

  ResultString := '';
  for I := 0 to Arr.Elements.Count - 1 do
  begin
    if I > 0 then
      ResultString := ResultString + Separator;
    ResultString := ResultString + Arr.Elements[I].ToString;
  end;

  Result := TGocciaStringValue.Create(ResultString);
end;

function TGocciaArrayValue.ArrayIncludes(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  SearchValue: TGocciaValue;
  I: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.includes called on non-array');

  Arr := TGocciaArrayValue(ThisValue);

  if Args.Count < 1 then
    ThrowError('Array.includes expects search value');

  SearchValue := Args[0];

  for I := 0 to Arr.Elements.Count - 1 do
  begin
    if Arr.Elements[I].Equals(SearchValue) then
    begin
      Result := TGocciaBooleanValue.Create(True);
      Exit;
    end;
  end;

  Result := TGocciaBooleanValue.Create(False);
end;

function TGocciaArrayValue.ArraySome(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;

var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TObjectList<TGocciaValue>;
  I: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.some called on non-array');

  if Args.Count < 1 then
    ThrowError('Array.some expects callback function');

  Arr := TGocciaArrayValue(ThisValue);
  Callback := Args[0];

  if not ((Callback is TGocciaFunctionValue) or (Callback is TGocciaNativeFunctionValue)) then
    ThrowError('Callback must be a function');

  CallArgs := TObjectList<TGocciaValue>.Create(False);
  for I := 0 to Arr.Elements.Count - 1 do
  begin
    CallArgs.Add(Arr.Elements[I]);
    CallArgs.Add(TGocciaNumberValue.Create(I));

    if Callback is TGocciaNativeFunctionValue then
      if TGocciaNativeFunctionValue(Callback).Call(CallArgs, ThisValue).ToBoolean then
      begin
        Result := TGocciaBooleanValue.Create(True);
        Exit;
      end;

    if Callback is TGocciaFunctionValue then
      if TGocciaFunctionValue(Callback).Call(CallArgs, ThisValue).ToBoolean then
      begin
        Result := TGocciaBooleanValue.Create(True);
        Exit;
      end;
  end;

  Result := TGocciaBooleanValue.Create(False);
end;

function TGocciaArrayValue.ArrayEvery(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TObjectList<TGocciaValue>;
  I: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.every called on non-array');

  if Args.Count < 1 then
    ThrowError('Array.every expects callback function');

  Arr := TGocciaArrayValue(ThisValue);
  Callback := Args[0];

  if not ((Callback is TGocciaFunctionValue) or (Callback is TGocciaNativeFunctionValue)) then
    ThrowError('Callback must be a function');

  CallArgs := TObjectList<TGocciaValue>.Create(False);
  for I := 0 to Arr.Elements.Count - 1 do
  begin
    CallArgs.Add(Arr.Elements[I]);
    CallArgs.Add(TGocciaNumberValue.Create(I));
  end;

  if Callback is TGocciaNativeFunctionValue then
    Result := TGocciaNativeFunctionValue(Callback).Call(CallArgs, ThisValue)
  else if Callback is TGocciaFunctionValue then
    Result := TGocciaFunctionValue(Callback).Call(CallArgs, ThisValue);

  Result := TGocciaBooleanValue.Create(Result.ToBoolean);
end;

function TGocciaArrayValue.ArrayToReversed(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  I: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.toReversed called on non-array');

  Arr := TGocciaArrayValue(ThisValue);

  ResultArray := TGocciaArrayValue.Create;

  for I := Arr.Elements.Count - 1 downto 0 do
    ResultArray.Elements.Add(Arr.Elements[I]);

  Result := ResultArray;
end;

function TGocciaArrayValue.ArrayToSorted(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  I: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.toSorted called on non-array');

  Arr := TGocciaArrayValue(ThisValue);

  ResultArray := TGocciaArrayValue.Create;

  for I := 0 to Arr.Elements.Count - 1 do
    ResultArray.Elements.Add(Arr.Elements[I]);

  ResultArray.Elements.Sort(TComparer<TGocciaValue>.Construct(DefaultCompare));

  Result := ResultArray;
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

function TGocciaArrayValue.GetProperty(const AName: string): TGocciaValue;
var
  Index: Integer;
begin
  // Check if property name is a numeric index
  if TryStrToInt(AName, Index) then
  begin
    if (Index >= 0) and (Index < FElements.Count) then
      Result := FElements[Index]
    else
      Result := TGocciaUndefinedValue.Create;
  end
  else
  begin
    // Fall back to regular object property lookup
    Result := inherited GetProperty(AName);
  end;
end;

end.
