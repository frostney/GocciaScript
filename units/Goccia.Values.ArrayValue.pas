unit Goccia.Values.ArrayValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Values.ObjectValue, Goccia.Values.FunctionValue, Goccia.Values.NativeFunction, Goccia.Error, Generics.Collections, Math, SysUtils;

type
  TGocciaArrayValue = class(TGocciaObjectValue)
  private
    function GetLength(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue; inline;

    // Helper methods for reducing duplication
    function ValidateArrayMethodCall(const MethodName: string; Args: TObjectList<TGocciaValue>;
      ThisValue: TGocciaValue; RequiresCallback: Boolean = True): TGocciaValue;
    function ExecuteArrayCallback(Callback: TGocciaValue; Element: TGocciaValue;
      Index: Integer; ThisArray: TGocciaValue): TGocciaValue;
    function IsArrayHole(Element: TGocciaValue): Boolean; inline;

    function ArrayMap(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayFilter(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayReduce(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayForEach(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ArraySome(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayEvery(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayFlat(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayFlatMap(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;

    function ArrayJoin(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayIncludes(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;

    function ArrayPush(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayPop(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;

    function ArrayToReversed(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayToSorted(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayToSpliced(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;

    procedure ThrowError(const Message: string; const Args: array of const); overload; inline;
    procedure ThrowError(const Message: string); overload; inline;
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
    procedure SetProperty(const AName: string; AValue: TGocciaValue);
    function Includes(const AValue: TGocciaValue; FromIndex: Integer = 0): Boolean;
    property Elements: TObjectList<TGocciaValue> read FElements;
  end;

  function DefaultCompare(constref A, B: TGocciaValue): Integer;

implementation

uses
  Goccia.Logger, Goccia.Values.NumberValue, Goccia.Values.BooleanValue, Goccia.Values.UndefinedValue, Goccia.Values.NullValue, Goccia.Evaluator, Goccia.Values.StringValue, Goccia.Values.ObjectPropertyDescriptor, Generics.Defaults, Goccia.Utils;

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

  FPrototype.DefineProperty('length', TGocciaPropertyDescriptorAccessor.Create(TGocciaNativeFunctionValue.Create(GetLength, 'length', 0), nil, [pfEnumerable, pfConfigurable]));
  // Array prototype methods: writable, non-enumerable, configurable
  FPrototype.RegisterMethod(TGocciaNativeFunctionValue.Create(ArrayMap, 'map', 1));
  FPrototype.RegisterMethod(TGocciaNativeFunctionValue.Create(ArrayFilter, 'filter', 1));
  FPrototype.RegisterMethod(TGocciaNativeFunctionValue.Create(ArrayReduce, 'reduce', 1));
  FPrototype.RegisterMethod(TGocciaNativeFunctionValue.Create(ArrayForEach, 'forEach', 1));
  FPrototype.RegisterMethod(TGocciaNativeFunctionValue.Create(ArraySome, 'some', 1));
  FPrototype.RegisterMethod(TGocciaNativeFunctionValue.Create(ArrayEvery, 'every', 1));
  FPrototype.RegisterMethod(TGocciaNativeFunctionValue.Create(ArrayFlat, 'flat', 1));
  FPrototype.RegisterMethod(TGocciaNativeFunctionValue.Create(ArrayFlatMap, 'flatMap', 1));
  FPrototype.RegisterMethod(TGocciaNativeFunctionValue.Create(ArrayJoin, 'join', 1));
  FPrototype.RegisterMethod(TGocciaNativeFunctionValue.Create(ArrayIncludes, 'includes', 1));
  FPrototype.RegisterMethod(TGocciaNativeFunctionValue.Create(ArrayPush, 'push', 1));
  FPrototype.RegisterMethod(TGocciaNativeFunctionValue.Create(ArrayPop, 'pop', 0));
  FPrototype.RegisterMethod(TGocciaNativeFunctionValue.Create(ArrayToReversed, 'toReversed', 0));
  FPrototype.RegisterMethod(TGocciaNativeFunctionValue.Create(ArrayToSorted, 'toSorted', 0));
  FPrototype.RegisterMethod(TGocciaNativeFunctionValue.Create(ArrayToSpliced, 'toSpliced', 1));
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

function TGocciaArrayValue.GetLength(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberValue.Create(FElements.Count);
end;

function TGocciaArrayValue.ValidateArrayMethodCall(const MethodName: string; Args: TObjectList<TGocciaValue>;
  ThisValue: TGocciaValue; RequiresCallback: Boolean = True): TGocciaValue;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.%s called on non-array', [MethodName]);

  if Args.Count < 1 then
    ThrowError('Array.%s expects callback function', [MethodName]);

  Result := Args[0];

  if RequiresCallback then
  begin
    if not ((Result is TGocciaFunctionValue) or (Result is TGocciaNativeFunctionValue)) then
      ThrowError('Callback must be a function');
  end
  else
  begin
    if Result is TGocciaUndefinedValue then
      ThrowError('Callback must not be undefined');
  end;
end;

function TGocciaArrayValue.ExecuteArrayCallback(Callback: TGocciaValue; Element: TGocciaValue;
  Index: Integer; ThisArray: TGocciaValue): TGocciaValue;
var
  CallArgs: TObjectList<TGocciaValue>;
begin
  CallArgs := TObjectList<TGocciaValue>.Create(False);
  try
    CallArgs.Add(Element);
    CallArgs.Add(TGocciaNumberValue.Create(Index));
    CallArgs.Add(ThisArray);

    if Callback is TGocciaNativeFunctionValue then
      Result := TGocciaNativeFunctionValue(Callback).Call(CallArgs, ThisArray)
    else if Callback is TGocciaFunctionValue then
      Result := TGocciaFunctionValue(Callback).Call(CallArgs, ThisArray)
    else
      ThrowError('Callback must be a function');
  finally
    CallArgs.Free;
  end;
end;

function TGocciaArrayValue.IsArrayHole(Element: TGocciaValue): Boolean;
begin
  Result := Element = nil;
end;

function TGocciaArrayValue.ArrayMap(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  ResultArray: TGocciaArrayValue;
  Callback: TGocciaValue;
  I: Integer;
begin
  Logger.Debug('ArrayMap: Starting with ThisValue type: %s, pointer: %p', [ThisValue.ClassName, Pointer(ThisValue)]);

  Callback := ValidateArrayMethodCall('map', Args, ThisValue, True);
  ResultArray := TGocciaArrayValue.Create;

  for I := 0 to FElements.Count - 1 do
  begin
    // Skip holes in sparse arrays (represented as nil)
    if IsArrayHole(FElements[I]) then
    begin
      ResultArray.Elements.Add(nil); // Preserve holes in map result
      Continue;
    end;

    Logger.Debug('ArrayMap: Iteration %d, about to call with ThisValue type: %s, pointer: %p', [I, ThisValue.ClassName, Pointer(ThisValue)]);
    ResultArray.Elements.Add(ExecuteArrayCallback(Callback, FElements[I], I, ThisValue));
    Logger.Debug('ArrayMap: After iteration %d, ThisValue type: %s, pointer: %p', [I, ThisValue.ClassName, Pointer(ThisValue)]);
  end;

  Result := ResultArray;
end;

function TGocciaArrayValue.ArrayFilter(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Callback: TGocciaValue;
  ResultArray: TGocciaArrayValue;
  PredicateResult: TGocciaValue;
  I: Integer;
begin
  Callback := ValidateArrayMethodCall('filter', Args, ThisValue, True);
  ResultArray := TGocciaArrayValue.Create;

  for I := 0 to Elements.Count - 1 do
  begin
    // Skip holes in sparse arrays (represented as nil)
    if IsArrayHole(Elements[I]) then
      Continue;

    PredicateResult := ExecuteArrayCallback(Callback, Elements[I], I, ThisValue);

    if PredicateResult is TGocciaBooleanValue then
    begin
      if TGocciaBooleanValue(PredicateResult).Value then
        ResultArray.Elements.Add(Elements[I]);
    end
    else
      ThrowError('Filter callback must return boolean');
  end;

  Result := ResultArray;
end;

function TGocciaArrayValue.ArrayReduce(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Callback: TGocciaValue;
  Accumulator: TGocciaValue;
  CallArgs: TObjectList<TGocciaValue>;
  I, StartIndex: Integer;
begin
  Callback := ValidateArrayMethodCall('reduce', Args, ThisValue, True);

  if Args.Count >= 2 then
  begin
    Accumulator := Args[1];
    StartIndex := 0;
  end
  else
  begin
    if Elements.Count = 0 then
      ThrowError('Reduce of empty array with no initial value');
    Accumulator := Elements[0];
    StartIndex := 1;
  end;

  for I := StartIndex to Elements.Count - 1 do
  begin
    // Skip holes in sparse arrays (represented as nil)
    if IsArrayHole(Elements[I]) then
      Continue;

    CallArgs := TObjectList<TGocciaValue>.Create(False);
    try
      CallArgs.Add(Accumulator);
      CallArgs.Add(Elements[I]);
      CallArgs.Add(TGocciaNumberValue.Create(I));
      CallArgs.Add(ThisValue);

      if Callback is TGocciaNativeFunctionValue then
        Accumulator := TGocciaNativeFunctionValue(Callback).Call(CallArgs, ThisValue)
      else if Callback is TGocciaFunctionValue then
        Accumulator := TGocciaFunctionValue(Callback).Call(CallArgs, ThisValue);
    finally
      CallArgs.Free;
    end;
  end;

  Result := Accumulator;
end;

function TGocciaArrayValue.ArrayForEach(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Callback: TGocciaValue;
  I: Integer;
begin
  Callback := ValidateArrayMethodCall('forEach', Args, ThisValue, True);

  for I := 0 to Elements.Count - 1 do
  begin
    // Skip holes in sparse arrays (represented as nil)
    if IsArrayHole(Elements[I]) then
      Continue;

    ExecuteArrayCallback(Callback, Elements[I], I, ThisValue);
  end;

  Result := TGocciaUndefinedValue.Create;
end;

function TGocciaArrayValue.ArrayJoin(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Separator: string;
  I: Integer;
  ResultString: string;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.join called on non-array');

  if Args.Count < 1 then
    Separator := ','
  else
    Separator := Args[0].ToString;

  ResultString := '';
  for I := 0 to Elements.Count - 1 do
  begin
    if I > 0 then
      ResultString := ResultString + Separator;
    ResultString := ResultString + Elements[I].ToString;
  end;

  Result := TGocciaStringValue.Create(ResultString);
end;

function TGocciaArrayValue.ArrayIncludes(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  SearchValue: TGocciaValue;
  I, FromIndex: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.includes called on non-array');

  if Args.Count < 1 then
    ThrowError('Array.includes expects search value');

  SearchValue := Args[0];

  if SearchValue is TGocciaUndefinedValue then
  begin
    Result := TGocciaBooleanValue.Create(False);
    Exit;
  end;

  if SearchValue is TGocciaNullValue then
  begin
    Result := TGocciaBooleanValue.Create(False);
    Exit;
  end;

  FromIndex := 0;

  if Args.Count > 1 then
  begin
    if not (Args[1] is TGocciaNumberValue) then
      ThrowError('Array.includes expects second argument to be a number');

    FromIndex := Trunc(Args[1].ToNumber);
  end;

  Result := TGocciaBooleanValue.Create(Includes(SearchValue, FromIndex));
end;

function TGocciaArrayValue.ArraySome(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Callback: TGocciaValue;
  I: Integer;
  SomeResult: TGocciaValue;
begin
  Callback := ValidateArrayMethodCall('some', Args, ThisValue, True);

  for I := 0 to Elements.Count - 1 do
  begin
    // Skip holes in sparse arrays (represented as nil)
    if IsArrayHole(Elements[I]) then
      Continue;

    SomeResult := ExecuteArrayCallback(Callback, Elements[I], I, ThisValue);
    if SomeResult.ToBoolean then
    begin
      Result := TGocciaBooleanValue.Create(True);
      Exit;
    end;
  end;

  Result := TGocciaBooleanValue.Create(False);
end;

function TGocciaArrayValue.ArrayEvery(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Callback: TGocciaValue;
  I: Integer;
  EveryResult: TGocciaValue;
begin
  Callback := ValidateArrayMethodCall('every', Args, ThisValue, True);

  for I := 0 to Elements.Count - 1 do
  begin
    // Skip holes in sparse arrays (represented as nil)
    if IsArrayHole(Elements[I]) then
      Continue;

    EveryResult := ExecuteArrayCallback(Callback, Elements[I], I, ThisValue);
    if not EveryResult.ToBoolean then
    begin
      Result := TGocciaBooleanValue.Create(False);
      Exit;
    end;
  end;

  Result := TGocciaBooleanValue.Create(True);
end;

function TGocciaArrayValue.ArrayFlat(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  ResultArray: TGocciaArrayValue;
  I, J: Integer;
  Depth: Integer;
  CallArgs: TObjectList<TGocciaValue>;
  FlattenedSubArray: TGocciaArrayValue;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.flat called on non-array');

  ResultArray := TGocciaArrayValue.Create;

  // Default depth is 1
  Depth := 1;

  if Args.Count > 0 then
  begin
    if not (Args[0] is TGocciaNumberValue) then
      ThrowError('Array.flat expects depth argument to be a number');

    Depth := Trunc(Args[0].ToNumber);

    if Depth < 0 then
      Depth := 0;
  end;

  for I := 0 to Elements.Count - 1 do
  begin
    // Skip holes in sparse arrays (represented as nil)
    if IsArrayHole(Elements[I]) then
      Continue;

    if (Elements[I] is TGocciaArrayValue) and (Depth > 0) then
    begin
      // Recursively flatten the nested array
      CallArgs := TObjectList<TGocciaValue>.Create(False);
      try
        CallArgs.Add(TGocciaNumberValue.Create(Depth - 1));
        FlattenedSubArray := TGocciaArrayValue(TGocciaArrayValue(Elements[I]).ArrayFlat(CallArgs, Elements[I]));

        // Add all elements from the flattened subarray
        for J := 0 to FlattenedSubArray.Elements.Count - 1 do
        begin
          ResultArray.Elements.Add(FlattenedSubArray.Elements[J]);
        end;
      finally
        CallArgs.Free;
      end;
    end
    else
    begin
      // Add the element as-is (either not an array or depth is 0)
      ResultArray.Elements.Add(Elements[I]);
    end;
  end;

  Result := ResultArray;
end;

function TGocciaArrayValue.ArrayFlatMap(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  ResultArray: TGocciaArrayValue;
  Callback: TGocciaValue;
  I, J: Integer;
  MappedValue: TGocciaValue;
begin
  Callback := ValidateArrayMethodCall('flatMap', Args, ThisValue, True);
  ResultArray := TGocciaArrayValue.Create;

  for I := 0 to Elements.Count - 1 do
  begin
    // Skip holes in sparse arrays (represented as nil)
    if IsArrayHole(Elements[I]) then
      Continue;

    MappedValue := ExecuteArrayCallback(Callback, Elements[I], I, ThisValue);

    // flatMap only flattens one level - if the mapped value is an array,
    // add each of its elements to the result array
    if MappedValue is TGocciaArrayValue then
    begin
      // Add each element from the mapped array (flatten one level)
      // Skip holes (nil elements) during flattening
      for J := 0 to TGocciaArrayValue(MappedValue).Elements.Count - 1 do
      begin
        if TGocciaArrayValue(MappedValue).Elements[J] <> nil then
          ResultArray.Elements.Add(TGocciaArrayValue(MappedValue).Elements[J]);
      end;
    end
    else
    begin
      // If not an array, add the value as-is
      ResultArray.Elements.Add(MappedValue);
    end;
  end;

  Result := ResultArray;
end;

function TGocciaArrayValue.ArrayPush(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
begin
  if Args.Count < 1 then
    ThrowError('Array.push expects at least one argument');

  for I := 0 to Args.Count - 1 do
    Elements.Add(Args[I]);

  Result := TGocciaNumberValue.Create(Elements.Count);
end;

function TGocciaArrayValue.ArrayPop(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
begin
  if Elements.Count = 0 then
    ThrowError('Array.pop called on empty array');

  Result := Elements[Elements.Count - 1];
  Elements.Delete(Elements.Count - 1);
end;

function TGocciaArrayValue.ArrayToReversed(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  ResultArray: TGocciaArrayValue;
  I: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.toReversed called on non-array');

  ResultArray := TGocciaArrayValue.Create;

  for I := Elements.Count - 1 downto 0 do
    ResultArray.Elements.Add(Elements[I]);

  Result := ResultArray;
end;

function TGocciaArrayValue.ArrayToSorted(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  ResultArray: TGocciaArrayValue;
  CustomSortFunction: TGocciaValue;
  I, J: Integer;
  ShouldSwap: Boolean;
  CallArgs: TObjectList<TGocciaValue>;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.toSorted called on non-array');

  ResultArray := TGocciaArrayValue.Create;

  for I := 0 to Elements.Count - 1 do
    ResultArray.Elements.Add(Elements[I]);

  if Args.Count > 0 then
  begin
    CustomSortFunction := Args[0];

    if not ((CustomSortFunction is TGocciaFunctionValue) or (CustomSortFunction is TGocciaNativeFunctionValue)) then
      ThrowError('Custom sort function must be a function');

    // Use bubble sort with custom comparison function
    for I := 0 to ResultArray.Elements.Count - 2 do
    begin
      for J := 0 to ResultArray.Elements.Count - 2 - I do
      begin
        CallArgs := TObjectList<TGocciaValue>.Create(False);
        CallArgs.Add(ResultArray.Elements[J]);
        CallArgs.Add(ResultArray.Elements[J + 1]);

        if CustomSortFunction is TGocciaNativeFunctionValue then
          ShouldSwap := TGocciaNativeFunctionValue(CustomSortFunction).Call(CallArgs, ThisValue).ToNumber > 0
        else
          ShouldSwap := TGocciaFunctionValue(CustomSortFunction).Call(CallArgs, ThisValue).ToNumber > 0;

        if ShouldSwap then
          ResultArray.Elements.Exchange(J, J + 1);
      end;
    end;
  end else
  begin
    ResultArray.Elements.Sort(TComparer<TGocciaValue>.Construct(DefaultCompare));
  end;

  Result := ResultArray;
end;

function TGocciaArrayValue.ArrayToSpliced(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  ResultArray: TGocciaArrayValue;
  StartIndex, DeleteCount: Integer;
  I: Integer;
  ActualStartIndex: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.toSpliced called on non-array');

  ResultArray := TGocciaArrayValue.Create;

  // Handle start index
  if Args.Count < 1 then
    StartIndex := 0
  else
    StartIndex := Trunc(Args[0].ToNumber);

  // Handle negative start index
  if StartIndex < 0 then
    ActualStartIndex := Elements.Count + StartIndex
  else
    ActualStartIndex := StartIndex;

  // Clamp start index to valid range
  if ActualStartIndex < 0 then
    ActualStartIndex := 0
  else if ActualStartIndex > Elements.Count then
    ActualStartIndex := Elements.Count;

  // Handle delete count
  if Args.Count < 2 then
    DeleteCount := Elements.Count - ActualStartIndex
  else
    DeleteCount := Trunc(Args[1].ToNumber);

  // Clamp delete count to valid range
  if DeleteCount < 0 then
    DeleteCount := 0
  else if ActualStartIndex + DeleteCount > Elements.Count then
    DeleteCount := Elements.Count - ActualStartIndex;

  // Copy elements before the splice point
  for I := 0 to ActualStartIndex - 1 do
  begin
    if Elements[I] = nil then
      ResultArray.Elements.Add(TGocciaUndefinedValue.Create)
    else
      ResultArray.Elements.Add(Elements[I]);
  end;

  // Add new elements (if any)
  for I := 2 to Args.Count - 1 do
    ResultArray.Elements.Add(Args[I]);

  // Copy elements after the deleted section
  for I := ActualStartIndex + DeleteCount to Elements.Count - 1 do
  begin
    if Elements[I] = nil then
      ResultArray.Elements.Add(TGocciaUndefinedValue.Create)
    else
      ResultArray.Elements.Add(Elements[I]);
  end;

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

    // For holes (nil), show as empty
    if FElements[I] = nil then
      Result := Result + ''
    else
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

function TGocciaArrayValue.Includes(const AValue: TGocciaValue; FromIndex: Integer = 0): Boolean;
var
  I: Integer;
begin
  if FromIndex < 0 then
    FromIndex := FElements.Count + FromIndex;

  if FromIndex < 0 then
    FromIndex := 0;

  for I := FromIndex to FElements.Count - 1 do
  begin
    if FElements[I] = nil then
      Continue;

    if IsEqual(FElements[I], AValue) then
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

function TGocciaArrayValue.GetProperty(const AName: string): TGocciaValue;
var
  Index: Integer;
begin
  // Check if property name is a numeric index
  if TryStrToInt(AName, Index) then
  begin
    if (Index >= 0) and (Index < FElements.Count) then
    begin
      // If the element is nil (hole), return undefined
      if FElements[Index] = nil then
        Result := TGocciaUndefinedValue.Create
      else
        Result := FElements[Index];
    end
    else
      Result := TGocciaUndefinedValue.Create;
  end
  else
  begin
    // Fall back to regular object property lookup
    Result := inherited GetProperty(AName);
  end;
end;

procedure TGocciaArrayValue.SetProperty(const AName: string; AValue: TGocciaValue);
var
  Index: Integer;
begin
  // Check if property name is a numeric index
  if TryStrToInt(AName, Index) then
  begin
    if Index >= 0 then
    begin
      // Expand array if necessary
      while FElements.Count <= Index do
        FElements.Add(nil); // Add holes for missing indices

      // Set the element
      FElements[Index] := AValue;
    end;
    // Negative indices are ignored for array assignment
  end
  else
  begin
    // Fall back to regular object property assignment
    inherited AssignProperty(AName, AValue);
  end;
end;

end.
