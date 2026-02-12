unit Goccia.Values.ArrayValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Goccia.Values.ObjectValue, Goccia.Values.NativeFunction,
  Goccia.Error, Goccia.Arguments.Collection, Generics.Collections, Math, SysUtils, Goccia.Values.Interfaces;

type
  TGocciaArrayValue = class(TGocciaObjectValue, IIndexMethods)
  private
    function GetLengthProperty(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue; inline;

    // Helper methods for reducing duplication
    function ValidateArrayMethodCall(const MethodName: string; Args: TGocciaArgumentsCollection;
      ThisValue: TGocciaValue; RequiresCallback: Boolean = True): TGocciaValue;
    function ExecuteArrayCallback(Callback: TGocciaValue; Element: TGocciaValue;
      Index: Integer; ThisArray: TGocciaValue): TGocciaValue;
    function IsArrayHole(Element: TGocciaValue): Boolean; inline;

    function ArrayMap(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayFilter(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayReduce(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayForEach(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArraySome(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayEvery(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayFlat(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayFlatMap(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    function ArrayJoin(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayIncludes(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    function ArrayPush(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayPop(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArraySlice(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    function ArrayFind(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayFindIndex(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayIndexOf(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayLastIndexOf(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayConcat(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayReverse(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    function ArrayToReversed(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayToSorted(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayToSpliced(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    procedure ThrowError(const Message: string; const Args: array of const); overload; inline;
    procedure ThrowError(const Message: string); overload; inline;
  protected
    FElements: TObjectList<TGocciaValue>;
  public
    constructor Create;
    destructor Destroy; override;

    function ToStringTag: string; override;

    function ToStringLiteral: TGocciaStringLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;

    // IIndexMethods interface implementation
    function GetLength: Integer;
    function GetElement(const AIndex: Integer): TGocciaValue;
    function SetElement(const AIndex: Integer; AValue: TGocciaValue): Boolean;
    function TypeName: string; override;
    function GetProperty(const AName: string): TGocciaValue; override;
    procedure SetProperty(const AName: string; AValue: TGocciaValue);
    function Includes(const AValue: TGocciaValue; FromIndex: Integer = 0): Boolean;
    property Elements: TObjectList<TGocciaValue> read FElements;
  end;

  function DefaultCompare(constref A, B: TGocciaValue): Integer;

implementation

uses
  Goccia.Logger, Goccia.Evaluator, Goccia.Values.ObjectPropertyDescriptor, Generics.Defaults, Goccia.Utils, Goccia.Evaluator.Comparison, Goccia.Values.ClassHelper, Goccia.Values.FunctionValue;

function DefaultCompare(constref A, B: TGocciaValue): Integer;
var
  NumA, NumB: Double;
  StrA, StrB: string;
begin
  if A is TGocciaNumberLiteralValue then
  begin
    NumA := A.ToNumberLiteral.Value;
    NumB := B.ToNumberLiteral.Value;
    if NumA < NumB then
      Result := -1
    else if NumA > NumB then
      Result := 1
    else
      Result := 0;
  end
  else if A is TGocciaStringLiteralValue then
  begin
    StrA := A.ToStringLiteral.Value;
    StrB := B.ToStringLiteral.Value;
    Result := CompareStr(StrA, StrB);
  end
  else if A is TGocciaBooleanLiteralValue then
  begin
    if A.ToBooleanLiteral.Value = B.ToBooleanLiteral.Value then
      Result := 0
    else if A.ToBooleanLiteral.Value and not B.ToBooleanLiteral.Value then
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

  FPrototype.DefineProperty('length', TGocciaPropertyDescriptorAccessor.Create(TGocciaNativeFunctionValue.Create(GetLengthProperty, 'length', 0), nil, [pfEnumerable, pfConfigurable]));
  // Array prototype methods: writable, non-enumerable, configurable
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayMap, 'map', 1));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayFilter, 'filter', 1));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayReduce, 'reduce', 1));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayForEach, 'forEach', 1));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArraySome, 'some', 1));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayEvery, 'every', 1));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayFlat, 'flat', 1));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayFlatMap, 'flatMap', 1));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayJoin, 'join', 1));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayIncludes, 'includes', 1));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayPush, 'push', 1));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayPop, 'pop', 0));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArraySlice, 'slice', 2));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayFind, 'find', 1));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayFindIndex, 'findIndex', 1));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayIndexOf, 'indexOf', 1));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayLastIndexOf, 'lastIndexOf', 1));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayConcat, 'concat', 1));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayReverse, 'reverse', 0));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayToReversed, 'toReversed', 0));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayToSorted, 'toSorted', 0));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayToSpliced, 'toSpliced', 1));
end;

destructor TGocciaArrayValue.Destroy;
begin
  FElements.Free;
  inherited;
end;

function TGocciaArrayValue.ToStringTag: string;
begin
  Result := 'Array';
end;

procedure TGocciaArrayValue.ThrowError(const Message: string; const Args: array of const);
begin
  raise TGocciaError.Create(Format(Message, Args), 0, 0, '', nil);
end;

procedure TGocciaArrayValue.ThrowError(const Message: string);
begin
  ThrowError(Message, []);
end;

function TGocciaArrayValue.GetLengthProperty(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(FElements.Count);
end;

// IIndexMethods interface implementations
function TGocciaArrayValue.GetLength: Integer;
begin
  Result := FElements.Count;
end;

function TGocciaArrayValue.GetElement(const AIndex: Integer): TGocciaValue;
begin
  if (AIndex >= 0) and (AIndex < FElements.Count) then
    Result := FElements[AIndex]
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaArrayValue.SetElement(const AIndex: Integer; AValue: TGocciaValue): Boolean;
begin
  if AIndex < 0 then
  begin
    Result := False;
    Exit;
  end;

  // Extend array if necessary
  while FElements.Count <= AIndex do
    FElements.Add(nil); // Add holes for sparse arrays

  FElements[AIndex] := AValue;
  Result := True;
end;

function TGocciaArrayValue.ValidateArrayMethodCall(const MethodName: string; Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue; RequiresCallback: Boolean = True): TGocciaValue;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.%s called on non-array', [MethodName]);

  if Args.Length < 1 then
    ThrowError('Array.%s expects callback function', [MethodName]);

  Result := Args.GetElement(0);

  if RequiresCallback then
  begin
    if not ((Result is TGocciaFunctionValue) or (Result is TGocciaNativeFunctionValue)) then
      ThrowError('Callback must be a function');
  end
  else
  begin
    if Result is TGocciaUndefinedLiteralValue then
      ThrowError('Callback must not be undefined');
  end;
end;

function TGocciaArrayValue.ExecuteArrayCallback(Callback: TGocciaValue; Element: TGocciaValue;
  Index: Integer; ThisArray: TGocciaValue): TGocciaValue;
var
  CallArgs: TGocciaArgumentsCollection;
begin
  CallArgs := TGocciaArgumentsCollection.Create([Element, TGocciaNumberLiteralValue.Create(Index), ThisArray]);
  try
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

function TGocciaArrayValue.ArrayMap(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
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

function TGocciaArrayValue.ArrayFilter(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
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

    if PredicateResult is TGocciaBooleanLiteralValue then
    begin
      if TGocciaBooleanLiteralValue(PredicateResult).Value then
        ResultArray.Elements.Add(Elements[I]);
    end
    else
      ThrowError('Filter callback must return boolean');
  end;

  Result := ResultArray;
end;

function TGocciaArrayValue.ArrayReduce(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Callback: TGocciaValue;
  Accumulator: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I, StartIndex: Integer;
begin
  Callback := ValidateArrayMethodCall('reduce', Args, ThisValue, True);

  if Args.Length >= 2 then
  begin
    Accumulator := Args.GetElement(1);
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

    CallArgs := TGocciaArgumentsCollection.Create([Accumulator, Elements[I], TGocciaNumberLiteralValue.Create(I), ThisValue]);
    try
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

function TGocciaArrayValue.ArrayForEach(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
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

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaArrayValue.ArrayJoin(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Separator: string;
  I: Integer;
  ResultString: string;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.join called on non-array');

  if Args.Length < 1 then
    Separator := ','
  else
    Separator := Args.GetElement(0).ToStringLiteral.Value;

  ResultString := '';
  for I := 0 to Elements.Count - 1 do
  begin
    if I > 0 then
      ResultString := ResultString + Separator;
    ResultString := ResultString + Elements[I].ToStringLiteral.Value;
  end;

  Result := TGocciaStringLiteralValue.Create(ResultString);
end;

function TGocciaArrayValue.ArrayIncludes(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  SearchValue: TGocciaValue;
  FromIndex: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.includes called on non-array');

  if Args.Length < 1 then
  begin
    // No argument means searching for undefined
    Result := TGocciaBooleanLiteralValue.Create(Includes(TGocciaUndefinedLiteralValue.UndefinedValue, 0));
    Exit;
  end;

  SearchValue := Args.GetElement(0);

  FromIndex := 0;

  if Args.Length > 1 then
    FromIndex := Trunc(Args.GetElement(1).ToNumberLiteral.Value);

  Result := TGocciaBooleanLiteralValue.Create(Includes(SearchValue, FromIndex));
end;

function TGocciaArrayValue.ArraySome(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
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
    if SomeResult.ToBooleanLiteral.Value then
    begin
      Result := TGocciaBooleanLiteralValue.Create(True);
      Exit;
    end;
  end;

  Result := TGocciaBooleanLiteralValue.Create(False);
end;

function TGocciaArrayValue.ArrayEvery(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
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
    if not EveryResult.ToBooleanLiteral.Value then
    begin
      Result := TGocciaBooleanLiteralValue.Create(False);
      Exit;
    end;
  end;

  Result := TGocciaBooleanLiteralValue.Create(True);
end;

function TGocciaArrayValue.ArrayFlat(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  ResultArray: TGocciaArrayValue;
  I, J: Integer;
  Depth: Integer;
  CallArgs: TGocciaArgumentsCollection;
  FlattenedSubArray: TGocciaArrayValue;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.flat called on non-array');

  ResultArray := TGocciaArrayValue.Create;

  // Default depth is 1
  Depth := 1;

  if Args.Length > 0 then
  begin
    if not (Args.GetElement(0) is TGocciaNumberLiteralValue) then
      ThrowError('Array.flat expects depth argument to be a number');

    Depth := Trunc(Args.GetElement(0).ToNumberLiteral.Value);

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
      CallArgs := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(Depth - 1)]);
      try
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

function TGocciaArrayValue.ArrayFlatMap(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
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

function TGocciaArrayValue.ArrayPush(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
begin
  if Args.Length < 1 then
    ThrowError('Array.push expects at least one argument');

  for I := 0 to Args.Length - 1 do
    Elements.Add(Args.GetElement(I));

  Result := TGocciaNumberLiteralValue.Create(Elements.Count);
end;

function TGocciaArrayValue.ArrayPop(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
begin
  if Elements.Count = 0 then
    ThrowError('Array.pop called on empty array');

  Result := Elements[Elements.Count - 1];
  Elements.Delete(Elements.Count - 1);
end;

function TGocciaArrayValue.ArraySlice(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  ResultArray: TGocciaArrayValue;
  StartIndex, EndIndex: Integer;
  I: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.slice called on non-array');

  ResultArray := TGocciaArrayValue.Create;

  // Handle start index (default to 0)
  if Args.Length >= 1 then
    StartIndex := Trunc(Args.GetElement(0).ToNumberLiteral.Value)
  else
    StartIndex := 0;

  // Handle negative start index
  if StartIndex < 0 then
    StartIndex := Elements.Count + StartIndex;

  // Clamp start index to valid range
  if StartIndex < 0 then
    StartIndex := 0
  else if StartIndex > Elements.Count then
    StartIndex := Elements.Count;

  // Handle end index (default to array length)
  if Args.Length >= 2 then
    EndIndex := Trunc(Args.GetElement(1).ToNumberLiteral.Value)
  else
    EndIndex := Elements.Count;

  // Handle negative end index
  if EndIndex < 0 then
    EndIndex := Elements.Count + EndIndex;

  // Clamp end index to valid range
  if EndIndex < 0 then
    EndIndex := 0
  else if EndIndex > Elements.Count then
    EndIndex := Elements.Count;

  // Copy elements from start to end (exclusive)
  for I := StartIndex to EndIndex - 1 do
  begin
    if (I >= 0) and (I < Elements.Count) then
    begin
      // Preserve holes (nil) when slicing
      if Elements[I] = nil then
        ResultArray.Elements.Add(nil)
      else
        ResultArray.Elements.Add(Elements[I]);
    end;
  end;

  Result := ResultArray;
end;

function TGocciaArrayValue.ArrayFind(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Callback: TGocciaValue;
  I: Integer;
  Element, CallResult: TGocciaValue;
begin
  Callback := ValidateArrayMethodCall('find', Args, ThisValue, True);

  for I := 0 to Elements.Count - 1 do
  begin
    Element := Elements[I];
    if IsArrayHole(Element) then
      Element := TGocciaUndefinedLiteralValue.UndefinedValue;

    CallResult := ExecuteArrayCallback(Callback, Element, I, ThisValue);
    if CallResult.ToBooleanLiteral.Value then
    begin
      Result := Element;
      Exit;
    end;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaArrayValue.ArrayFindIndex(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Callback: TGocciaValue;
  I: Integer;
  Element, CallResult: TGocciaValue;
begin
  Callback := ValidateArrayMethodCall('findIndex', Args, ThisValue, True);

  for I := 0 to Elements.Count - 1 do
  begin
    Element := Elements[I];
    if IsArrayHole(Element) then
      Element := TGocciaUndefinedLiteralValue.UndefinedValue;

    CallResult := ExecuteArrayCallback(Callback, Element, I, ThisValue);
    if CallResult.ToBooleanLiteral.Value then
    begin
      Result := TGocciaNumberLiteralValue.Create(I);
      Exit;
    end;
  end;

  Result := TGocciaNumberLiteralValue.Create(-1);
end;

function TGocciaArrayValue.ArrayIndexOf(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  SearchValue: TGocciaValue;
  I, FromIndex: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.indexOf called on non-array');

  if Args.Length < 1 then
  begin
    Result := TGocciaNumberLiteralValue.Create(-1);
    Exit;
  end;

  SearchValue := Args.GetElement(0);

  FromIndex := 0;
  if Args.Length > 1 then
  begin
    FromIndex := Trunc(Args.GetElement(1).ToNumberLiteral.Value);
    if FromIndex < 0 then
      FromIndex := Elements.Count + FromIndex;
    if FromIndex < 0 then
      FromIndex := 0;
  end;

  for I := FromIndex to Elements.Count - 1 do
  begin
    if IsArrayHole(Elements[I]) then
      Continue;
    if IsStrictEqual(Elements[I], SearchValue) then
    begin
      Result := TGocciaNumberLiteralValue.Create(I);
      Exit;
    end;
  end;

  Result := TGocciaNumberLiteralValue.Create(-1);
end;

function TGocciaArrayValue.ArrayLastIndexOf(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  SearchValue: TGocciaValue;
  I, FromIndex: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.lastIndexOf called on non-array');

  if Args.Length < 1 then
  begin
    Result := TGocciaNumberLiteralValue.Create(-1);
    Exit;
  end;

  SearchValue := Args.GetElement(0);

  FromIndex := Elements.Count - 1;
  if Args.Length > 1 then
  begin
    FromIndex := Trunc(Args.GetElement(1).ToNumberLiteral.Value);
    if FromIndex < 0 then
      FromIndex := Elements.Count + FromIndex;
  end;

  if FromIndex >= Elements.Count then
    FromIndex := Elements.Count - 1;

  for I := FromIndex downto 0 do
  begin
    if IsArrayHole(Elements[I]) then
      Continue;
    if IsStrictEqual(Elements[I], SearchValue) then
    begin
      Result := TGocciaNumberLiteralValue.Create(I);
      Exit;
    end;
  end;

  Result := TGocciaNumberLiteralValue.Create(-1);
end;

function TGocciaArrayValue.ArrayConcat(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  ResultArray: TGocciaArrayValue;
  I, J: Integer;
  Arg: TGocciaValue;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.concat called on non-array');

  ResultArray := TGocciaArrayValue.Create;

  // Copy elements from this array
  for I := 0 to Elements.Count - 1 do
    ResultArray.Elements.Add(Elements[I]);

  // Append each argument
  for I := 0 to Args.Length - 1 do
  begin
    Arg := Args.GetElement(I);
    if Arg is TGocciaArrayValue then
    begin
      for J := 0 to TGocciaArrayValue(Arg).Elements.Count - 1 do
        ResultArray.Elements.Add(TGocciaArrayValue(Arg).Elements[J]);
    end
    else
      ResultArray.Elements.Add(Arg);
  end;

  Result := ResultArray;
end;

function TGocciaArrayValue.ArrayReverse(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  I, J: Integer;
  Temp: TGocciaValue;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.reverse called on non-array');

  // Reverse in place
  I := 0;
  J := Elements.Count - 1;
  while I < J do
  begin
    Temp := Elements[I];
    Elements[I] := Elements[J];
    Elements[J] := Temp;
    Inc(I);
    Dec(J);
  end;

  Result := ThisValue;
end;

function TGocciaArrayValue.ArrayToReversed(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
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

function TGocciaArrayValue.ArrayToSorted(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  ResultArray: TGocciaArrayValue;
  CustomSortFunction: TGocciaValue;
  I, J: Integer;
  ShouldSwap: Boolean;
  CallArgs: TGocciaArgumentsCollection;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.toSorted called on non-array');

  ResultArray := TGocciaArrayValue.Create;

  for I := 0 to Elements.Count - 1 do
    ResultArray.Elements.Add(Elements[I]);

  if Args.Length > 0 then
  begin
    CustomSortFunction := Args.GetElement(0);

    if not ((CustomSortFunction is TGocciaFunctionValue) or (CustomSortFunction is TGocciaNativeFunctionValue)) then
      ThrowError('Custom sort function must be a function');

    // Use bubble sort with custom comparison function
    for I := 0 to ResultArray.Elements.Count - 2 do
    begin
      for J := 0 to ResultArray.Elements.Count - 2 - I do
      begin
        CallArgs := TGocciaArgumentsCollection.Create([ResultArray.Elements[J], ResultArray.Elements[J + 1]]);
        try
          if CustomSortFunction is TGocciaNativeFunctionValue then
            ShouldSwap := TGocciaNativeFunctionValue(CustomSortFunction).Call(CallArgs, ThisValue).ToNumberLiteral.Value > 0
          else
            ShouldSwap := TGocciaFunctionValue(CustomSortFunction).Call(CallArgs, ThisValue).ToNumberLiteral.Value > 0;

          if ShouldSwap then
            ResultArray.Elements.Exchange(J, J + 1);
        finally
          CallArgs.Free;
        end;
      end;
    end;
  end else
  begin
    ResultArray.Elements.Sort(TComparer<TGocciaValue>.Construct(DefaultCompare));
  end;

  Result := ResultArray;
end;

function TGocciaArrayValue.ArrayToSpliced(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
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
  if Args.Length < 1 then
    StartIndex := 0
  else
    StartIndex := Trunc(Args.GetElement(0).ToNumberLiteral.Value);

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
  if Args.Length < 2 then
    DeleteCount := Elements.Count - ActualStartIndex
  else
    DeleteCount := Trunc(Args.GetElement(1).ToNumberLiteral.Value);

  // Clamp delete count to valid range
  if DeleteCount < 0 then
    DeleteCount := 0
  else if ActualStartIndex + DeleteCount > Elements.Count then
    DeleteCount := Elements.Count - ActualStartIndex;

  // Copy elements before the splice point
  for I := 0 to ActualStartIndex - 1 do
  begin
    if Elements[I] = nil then
      ResultArray.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue)
    else
      ResultArray.Elements.Add(Elements[I]);
  end;

  // Add new elements (if any)
  for I := 2 to Args.Length - 1 do
    ResultArray.Elements.Add(Args.GetElement(I));

  // Copy elements after the deleted section
  for I := ActualStartIndex + DeleteCount to Elements.Count - 1 do
  begin
    if Elements[I] = nil then
      ResultArray.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue)
    else
      ResultArray.Elements.Add(Elements[I]);
  end;

  Result := ResultArray;
end;

function TGocciaArrayValue.ToStringLiteral: TGocciaStringLiteralValue;
var
  I: Integer;
  S: string;
begin
  // ECMAScript compliant: Array.toString() behaves like Array.join() with comma separator
  // Empty array converts to empty string
  if FElements.Count = 0 then
  begin
    Result := TGocciaStringLiteralValue.Create('');
    Exit;
  end;

  S := '';
  for I := 0 to FElements.Count - 1 do
  begin
    if I > 0 then
      S := S + ',';

    // For holes (nil), show as empty (consistent with join behavior)
    if FElements[I] = nil then
      S := S + ''
    else
      S := S + FElements[I].ToStringLiteral.Value;
  end;

  Result := TGocciaStringLiteralValue.Create(S);
end;

function TGocciaArrayValue.ToNumberLiteral: TGocciaNumberLiteralValue;
begin
  // ECMAScript: [] -> "" -> 0, [n] -> n, [a, b] -> NaN
  if FElements.Count = 0 then
    Result := TGocciaNumberLiteralValue.Create(0)
  else if FElements.Count = 1 then
    Result := FElements[0].ToNumberLiteral
  else
    Result := TGocciaNumberLiteralValue.NaNValue;
end;

function TGocciaArrayValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.Create(True);
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
    // Array holes (nil) match undefined
    if FElements[I] = nil then
    begin
      if AValue is TGocciaUndefinedLiteralValue then
      begin
        Result := True;
        Exit;
      end;
      Continue;
    end;

    // ECMAScript specifies SameValueZero for includes
    if IsSameValueZero(FElements[I], AValue) then
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
        Result := TGocciaUndefinedLiteralValue.UndefinedValue
      else
        Result := FElements[Index];
    end
    else
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
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
