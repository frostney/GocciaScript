unit Goccia.Values.ArrayValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Goccia.Values.ObjectValue, Goccia.Values.NativeFunction,
  Goccia.Error, Goccia.Arguments.Collection, Generics.Collections, Math, SysUtils;

type
  TGocciaArrayValue = class(TGocciaObjectValue)
  private
    function GetLengthProperty(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue; inline;

    // Helper methods for reducing duplication
    function ValidateArrayMethodCall(const MethodName: string; Args: TGocciaArgumentsCollection;
      ThisValue: TGocciaValue; RequiresCallback: Boolean = True): TGocciaValue;
    function IsArrayHole(Element: TGocciaValue): Boolean; inline;

    function ArrayMap(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayFilter(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayReduce(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayForEach(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArraySome(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayEvery(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayFlat(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    procedure FlattenInto(Target: TGocciaArrayValue; Depth: Integer);
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

    function ArraySort(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArraySplice(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayShift(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayUnshift(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayFill(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayAt(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

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

    // Index-based element access
    function GetLength: Integer;
    function GetElement(const AIndex: Integer): TGocciaValue;
    function SetElement(const AIndex: Integer; AValue: TGocciaValue): Boolean;
    function TypeName: string; override;
    function GetProperty(const AName: string): TGocciaValue; override;
    procedure SetProperty(const AName: string; AValue: TGocciaValue); override;
    function Includes(const AValue: TGocciaValue; FromIndex: Integer = 0): Boolean;

    procedure GCMarkReferences; override;

    property Elements: TObjectList<TGocciaValue> read FElements;
  end;

  function DefaultCompare(constref A, B: TGocciaValue): Integer;

implementation

uses
  Goccia.Evaluator, Goccia.Values.ObjectPropertyDescriptor, Generics.Defaults, Goccia.Utils, Goccia.Evaluator.Comparison, Goccia.Values.ClassHelper, Goccia.Values.FunctionValue;

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

function CallCompareFunc(CompareFunc: TGocciaValue; CallArgs: TGocciaArgumentsCollection;
  A, B: TGocciaValue; ThisValue: TGocciaValue): Double;
begin
  CallArgs.SetElement(0, A);
  CallArgs.SetElement(1, B);
  if CompareFunc is TGocciaNativeFunctionValue then
    Result := TGocciaNativeFunctionValue(CompareFunc).Call(CallArgs, ThisValue).ToNumberLiteral.Value
  else
    Result := TGocciaFunctionValue(CompareFunc).Call(CallArgs, ThisValue).ToNumberLiteral.Value;
end;

procedure QuickSortElements(Elements: TObjectList<TGocciaValue>; CompareFunc: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection; ThisValue: TGocciaValue; Lo, Hi: Integer);
var
  I, J: Integer;
  Pivot: TGocciaValue;
begin
  if Lo >= Hi then Exit;

  Pivot := Elements[(Lo + Hi) div 2];
  I := Lo;
  J := Hi;

  while I <= J do
  begin
    while CallCompareFunc(CompareFunc, CallArgs, Elements[I], Pivot, ThisValue) < 0 do
      Inc(I);
    while CallCompareFunc(CompareFunc, CallArgs, Elements[J], Pivot, ThisValue) > 0 do
      Dec(J);

    if I <= J then
    begin
      Elements.Exchange(I, J);
      Inc(I);
      Dec(J);
    end;
  end;

  if Lo < J then
    QuickSortElements(Elements, CompareFunc, CallArgs, ThisValue, Lo, J);
  if I < Hi then
    QuickSortElements(Elements, CompareFunc, CallArgs, ThisValue, I, Hi);
end;

function InvokeCallback(Callback: TGocciaValue; CallArgs: TGocciaArgumentsCollection;
  ThisArray: TGocciaValue): TGocciaValue; inline;
begin
  if Callback is TGocciaNativeFunctionValue then
    Result := TGocciaNativeFunctionValue(Callback).Call(CallArgs, ThisArray)
  else
    Result := TGocciaFunctionValue(Callback).Call(CallArgs, ThisArray);
end;

function CreateArrayCallbackArgs(Element: TGocciaValue; Index: Integer; ThisArray: TGocciaValue): TGocciaArgumentsCollection;
begin
  Result := TGocciaArgumentsCollection.Create([Element, TGocciaNumberLiteralValue.SmallInt(Index), ThisArray]);
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
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArraySort, 'sort', 1));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArraySplice, 'splice', 2));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayShift, 'shift', 0));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayUnshift, 'unshift', -1));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayFill, 'fill', 1));
  FPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayAt, 'at', 1));
end;

destructor TGocciaArrayValue.Destroy;
begin
  FElements.Free;
  inherited;
end;

procedure TGocciaArrayValue.GCMarkReferences;
var
  I: Integer;
begin
  if GCMarked then Exit;
  inherited; // Marks self + object properties/prototype

  // Mark all elements
  for I := 0 to FElements.Count - 1 do
  begin
    if Assigned(FElements[I]) then
      FElements[I].GCMarkReferences;
  end;
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

// Index-based element access implementations
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

function TGocciaArrayValue.IsArrayHole(Element: TGocciaValue): Boolean;
begin
  Result := Element = nil;
end;

function TGocciaArrayValue.ArrayMap(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  ResultArray: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  Callback := ValidateArrayMethodCall('map', Args, ThisValue, True);
  ResultArray := TGocciaArrayValue.Create;

  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, ThisValue]);
  try
    for I := 0 to FElements.Count - 1 do
    begin
      if IsArrayHole(FElements[I]) then
      begin
        ResultArray.Elements.Add(nil);
        Continue;
      end;

      CallArgs.SetElement(0, FElements[I]);
      CallArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(I));
      ResultArray.Elements.Add(InvokeCallback(Callback, CallArgs, ThisValue));
    end;
  finally
    CallArgs.Free;
  end;

  Result := ResultArray;
end;

function TGocciaArrayValue.ArrayFilter(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Callback: TGocciaValue;
  ResultArray: TGocciaArrayValue;
  CallArgs: TGocciaArgumentsCollection;
  PredicateResult: TGocciaValue;
  I: Integer;
begin
  Callback := ValidateArrayMethodCall('filter', Args, ThisValue, True);
  ResultArray := TGocciaArrayValue.Create;

  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, ThisValue]);
  try
    for I := 0 to Elements.Count - 1 do
    begin
      if IsArrayHole(Elements[I]) then
        Continue;

      CallArgs.SetElement(0, Elements[I]);
      CallArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(I));
      PredicateResult := InvokeCallback(Callback, CallArgs, ThisValue);

      if PredicateResult is TGocciaBooleanLiteralValue then
      begin
        if TGocciaBooleanLiteralValue(PredicateResult).Value then
          ResultArray.Elements.Add(Elements[I]);
      end
      else
        ThrowError('Filter callback must return boolean');
    end;
  finally
    CallArgs.Free;
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

  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, nil, ThisValue]);
  try
    for I := StartIndex to Elements.Count - 1 do
    begin
      if IsArrayHole(Elements[I]) then
        Continue;

      CallArgs.SetElement(0, Accumulator);
      CallArgs.SetElement(1, Elements[I]);
      CallArgs.SetElement(2, TGocciaNumberLiteralValue.SmallInt(I));
      Accumulator := InvokeCallback(Callback, CallArgs, ThisValue);
    end;
  finally
    CallArgs.Free;
  end;

  Result := Accumulator;
end;

function TGocciaArrayValue.ArrayForEach(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  Callback := ValidateArrayMethodCall('forEach', Args, ThisValue, True);

  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, ThisValue]);
  try
    for I := 0 to Elements.Count - 1 do
    begin
      if IsArrayHole(Elements[I]) then
        Continue;

      CallArgs.SetElement(0, Elements[I]);
      CallArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(I));
      InvokeCallback(Callback, CallArgs, ThisValue);
    end;
  finally
    CallArgs.Free;
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
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
  SomeResult: TGocciaValue;
begin
  Callback := ValidateArrayMethodCall('some', Args, ThisValue, True);

  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, ThisValue]);
  try
    for I := 0 to Elements.Count - 1 do
    begin
      if IsArrayHole(Elements[I]) then
        Continue;

      CallArgs.SetElement(0, Elements[I]);
      CallArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(I));
      SomeResult := InvokeCallback(Callback, CallArgs, ThisValue);
      if SomeResult.ToBooleanLiteral.Value then
      begin
        Result := TGocciaBooleanLiteralValue.Create(True);
        Exit;
      end;
    end;
  finally
    CallArgs.Free;
  end;

  Result := TGocciaBooleanLiteralValue.Create(False);
end;

function TGocciaArrayValue.ArrayEvery(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
  EveryResult: TGocciaValue;
begin
  Callback := ValidateArrayMethodCall('every', Args, ThisValue, True);

  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, ThisValue]);
  try
    for I := 0 to Elements.Count - 1 do
    begin
      if IsArrayHole(Elements[I]) then
        Continue;

      CallArgs.SetElement(0, Elements[I]);
      CallArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(I));
      EveryResult := InvokeCallback(Callback, CallArgs, ThisValue);
      if not EveryResult.ToBooleanLiteral.Value then
      begin
        Result := TGocciaBooleanLiteralValue.Create(False);
        Exit;
      end;
    end;
  finally
    CallArgs.Free;
  end;

  Result := TGocciaBooleanLiteralValue.Create(True);
end;

procedure TGocciaArrayValue.FlattenInto(Target: TGocciaArrayValue; Depth: Integer);
var
  I: Integer;
begin
  for I := 0 to Elements.Count - 1 do
  begin
    if IsArrayHole(Elements[I]) then
      Continue;

    if (Elements[I] is TGocciaArrayValue) and (Depth > 0) then
      TGocciaArrayValue(Elements[I]).FlattenInto(Target, Depth - 1)
    else
      Target.Elements.Add(Elements[I]);
  end;
end;

function TGocciaArrayValue.ArrayFlat(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  ResultArray: TGocciaArrayValue;
  Depth: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.flat called on non-array');

  Depth := 1;

  if Args.Length > 0 then
  begin
    if not (Args.GetElement(0) is TGocciaNumberLiteralValue) then
      ThrowError('Array.flat expects depth argument to be a number');

    Depth := Trunc(Args.GetElement(0).ToNumberLiteral.Value);

    if Depth < 0 then
      Depth := 0;
  end;

  ResultArray := TGocciaArrayValue.Create;
  FlattenInto(ResultArray, Depth);
  Result := ResultArray;
end;

function TGocciaArrayValue.ArrayFlatMap(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  ResultArray: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I, J: Integer;
  MappedValue: TGocciaValue;
begin
  Callback := ValidateArrayMethodCall('flatMap', Args, ThisValue, True);
  ResultArray := TGocciaArrayValue.Create;

  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, ThisValue]);
  try
    for I := 0 to Elements.Count - 1 do
    begin
      if IsArrayHole(Elements[I]) then
        Continue;

      CallArgs.SetElement(0, Elements[I]);
      CallArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(I));
      MappedValue := InvokeCallback(Callback, CallArgs, ThisValue);

      if MappedValue is TGocciaArrayValue then
      begin
        for J := 0 to TGocciaArrayValue(MappedValue).Elements.Count - 1 do
        begin
          if TGocciaArrayValue(MappedValue).Elements[J] <> nil then
            ResultArray.Elements.Add(TGocciaArrayValue(MappedValue).Elements[J]);
        end;
      end
      else
        ResultArray.Elements.Add(MappedValue);
    end;
  finally
    CallArgs.Free;
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
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
  Element, CallResult: TGocciaValue;
begin
  Callback := ValidateArrayMethodCall('find', Args, ThisValue, True);

  CallArgs := nil;
  try
    for I := 0 to Elements.Count - 1 do
    begin
      Element := Elements[I];
      if IsArrayHole(Element) then
        Element := TGocciaUndefinedLiteralValue.UndefinedValue;

      CallArgs.Free;
      CallArgs := CreateArrayCallbackArgs(Element, I, ThisValue);
      CallResult := InvokeCallback(Callback, CallArgs, ThisValue);
      if CallResult.ToBooleanLiteral.Value then
      begin
        Result := Element;
        Exit;
      end;
    end;
  finally
    CallArgs.Free;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaArrayValue.ArrayFindIndex(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
  Element, CallResult: TGocciaValue;
begin
  Callback := ValidateArrayMethodCall('findIndex', Args, ThisValue, True);

  CallArgs := nil;
  try
    for I := 0 to Elements.Count - 1 do
    begin
      Element := Elements[I];
      if IsArrayHole(Element) then
        Element := TGocciaUndefinedLiteralValue.UndefinedValue;

      CallArgs.Free;
      CallArgs := CreateArrayCallbackArgs(Element, I, ThisValue);
      CallResult := InvokeCallback(Callback, CallArgs, ThisValue);
      if CallResult.ToBooleanLiteral.Value then
      begin
        Result := TGocciaNumberLiteralValue.SmallInt(I);
        Exit;
      end;
    end;
  finally
    CallArgs.Free;
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
  I: Integer;
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

    // Quicksort with custom comparison function
    // Reuse a single args collection across all comparisons
    CallArgs := TGocciaArgumentsCollection.Create([nil, nil]);
    try
      QuickSortElements(ResultArray.Elements, CustomSortFunction, CallArgs, ThisValue, 0, ResultArray.Elements.Count - 1);
    finally
      CallArgs.Free;
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

function TGocciaArrayValue.ArraySort(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  CustomSortFunction: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.sort called on non-array');

  Arr := TGocciaArrayValue(ThisValue);

  if Args.Length > 0 then
  begin
    CustomSortFunction := Args.GetElement(0);

    if not ((CustomSortFunction is TGocciaFunctionValue) or (CustomSortFunction is TGocciaNativeFunctionValue)) then
      ThrowError('Custom sort function must be a function');

    // Quicksort with custom comparison function (mutates in-place)
    // Reuse a single args collection across all comparisons
    CallArgs := TGocciaArgumentsCollection.Create([nil, nil]);
    try
      QuickSortElements(Arr.Elements, CustomSortFunction, CallArgs, ThisValue, 0, Arr.Elements.Count - 1);
    finally
      CallArgs.Free;
    end;
  end else
  begin
    Arr.Elements.Sort(TComparer<TGocciaValue>.Construct(DefaultCompare));
  end;

  // sort() returns the same array (mutated in-place)
  Result := Arr;
end;

function TGocciaArrayValue.ArraySplice(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Removed: TGocciaArrayValue;
  StartIndex, DeleteCount, ActualStart: Integer;
  I: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.splice called on non-array');

  Arr := TGocciaArrayValue(ThisValue);
  Removed := TGocciaArrayValue.Create;

  // Handle start index
  if Args.Length < 1 then
  begin
    Result := Removed;
    Exit;
  end;

  StartIndex := Trunc(Args.GetElement(0).ToNumberLiteral.Value);

  // Handle negative start
  if StartIndex < 0 then
    ActualStart := Arr.Elements.Count + StartIndex
  else
    ActualStart := StartIndex;

  // Clamp
  if ActualStart < 0 then
    ActualStart := 0
  else if ActualStart > Arr.Elements.Count then
    ActualStart := Arr.Elements.Count;

  // Handle delete count
  if Args.Length < 2 then
    DeleteCount := Arr.Elements.Count - ActualStart
  else
    DeleteCount := Trunc(Args.GetElement(1).ToNumberLiteral.Value);

  if DeleteCount < 0 then
    DeleteCount := 0
  else if ActualStart + DeleteCount > Arr.Elements.Count then
    DeleteCount := Arr.Elements.Count - ActualStart;

  // Remove elements and collect them in Removed array
  for I := 0 to DeleteCount - 1 do
  begin
    Removed.Elements.Add(Arr.Elements[ActualStart]);
    Arr.Elements.Delete(ActualStart);
  end;

  // Insert new elements at start position
  for I := 2 to Args.Length - 1 do
  begin
    Arr.Elements.Insert(ActualStart + (I - 2), Args.GetElement(I));
  end;

  Result := Removed;
end;

function TGocciaArrayValue.ArrayShift(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.shift called on non-array');

  Arr := TGocciaArrayValue(ThisValue);

  if Arr.Elements.Count = 0 then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  Result := Arr.Elements[0];
  Arr.Elements.Delete(0);
end;

function TGocciaArrayValue.ArrayUnshift(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  I: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.unshift called on non-array');

  Arr := TGocciaArrayValue(ThisValue);

  // Insert elements at the beginning in the correct order
  for I := Args.Length - 1 downto 0 do
    Arr.Elements.Insert(0, Args.GetElement(I));

  // Return new length
  Result := TGocciaNumberLiteralValue.Create(Arr.Elements.Count);
end;

function TGocciaArrayValue.ArrayFill(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  FillValue: TGocciaValue;
  StartIdx, EndIdx, I: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.fill called on non-array');

  Arr := TGocciaArrayValue(ThisValue);

  if Args.Length < 1 then
    FillValue := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    FillValue := Args.GetElement(0);

  if Args.Length > 1 then
    StartIdx := Trunc(Args.GetElement(1).ToNumberLiteral.Value)
  else
    StartIdx := 0;

  if Args.Length > 2 then
    EndIdx := Trunc(Args.GetElement(2).ToNumberLiteral.Value)
  else
    EndIdx := Arr.Elements.Count;

  // Handle negatives
  if StartIdx < 0 then StartIdx := Arr.Elements.Count + StartIdx;
  if EndIdx < 0 then EndIdx := Arr.Elements.Count + EndIdx;

  // Clamp
  if StartIdx < 0 then StartIdx := 0;
  if EndIdx > Arr.Elements.Count then EndIdx := Arr.Elements.Count;

  for I := StartIdx to EndIdx - 1 do
    Arr.Elements[I] := FillValue;

  Result := Arr;
end;

function TGocciaArrayValue.ArrayAt(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Index: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.at called on non-array');

  Arr := TGocciaArrayValue(ThisValue);

  if Args.Length < 1 then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  Index := Trunc(Args.GetElement(0).ToNumberLiteral.Value);

  // Handle negative index
  if Index < 0 then
    Index := Arr.Elements.Count + Index;

  if (Index < 0) or (Index >= Arr.Elements.Count) then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    Result := Arr.Elements[Index];
end;

end.
