unit Goccia.Values.ArrayValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Goccia.Values.ObjectValue, Goccia.Values.NativeFunction,
  Goccia.Error, Goccia.Arguments.Collection, Generics.Collections, Math, SysUtils;

type
  TGocciaArrayValue = class(TGocciaObjectValue)
  private
    class var FSharedArrayPrototype: TGocciaObjectValue;
    class var FPrototypeMethodHost: TGocciaArrayValue;

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
    procedure InitializePrototype;

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
  Goccia.Evaluator, Goccia.Values.ObjectPropertyDescriptor, Generics.Defaults, Goccia.Utils, Goccia.Evaluator.Comparison, Goccia.Values.ClassHelper, Goccia.Values.FunctionValue, Goccia.GarbageCollector;

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
  // Pass undefined as ThisValue so arrow function callbacks inherit
  // 'this' from their lexical scope rather than receiving the array.
  // The array is already available as the third argument in CallArgs.
  if Callback is TGocciaNativeFunctionValue then
    Result := TGocciaNativeFunctionValue(Callback).Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue)
  else
    Result := TGocciaFunctionValue(Callback).Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
end;

function CreateArrayCallbackArgs(Element: TGocciaValue; Index: Integer; ThisArray: TGocciaValue): TGocciaArgumentsCollection;
begin
  Result := TGocciaArgumentsCollection.Create([Element, TGocciaNumberLiteralValue.SmallInt(Index), ThisArray]);
end;

constructor TGocciaArrayValue.Create;
begin
  inherited Create;
  FElements := TObjectList<TGocciaValue>.Create(False);
  InitializePrototype;
  if Assigned(FSharedArrayPrototype) then
    FPrototype := FSharedArrayPrototype;
end;

procedure TGocciaArrayValue.InitializePrototype;
begin
  if Assigned(FSharedArrayPrototype) then Exit;

  FSharedArrayPrototype := TGocciaObjectValue.Create;
  FPrototypeMethodHost := Self;

  FSharedArrayPrototype.DefineProperty('length', TGocciaPropertyDescriptorAccessor.Create(TGocciaNativeFunctionValue.Create(GetLengthProperty, 'length', 0), nil, [pfEnumerable, pfConfigurable]));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayMap, 'map', 1));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayFilter, 'filter', 1));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayReduce, 'reduce', 1));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayForEach, 'forEach', 1));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArraySome, 'some', 1));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayEvery, 'every', 1));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayFlat, 'flat', 1));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayFlatMap, 'flatMap', 1));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayJoin, 'join', 1));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayIncludes, 'includes', 1));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayPush, 'push', 1));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayPop, 'pop', 0));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArraySlice, 'slice', 2));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayFind, 'find', 1));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayFindIndex, 'findIndex', 1));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayIndexOf, 'indexOf', 1));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayLastIndexOf, 'lastIndexOf', 1));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayConcat, 'concat', 1));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayReverse, 'reverse', 0));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayToReversed, 'toReversed', 0));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayToSorted, 'toSorted', 0));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayToSpliced, 'toSpliced', 1));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArraySort, 'sort', 1));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArraySplice, 'splice', 2));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayShift, 'shift', 0));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayUnshift, 'unshift', -1));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayFill, 'fill', 1));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayAt, 'at', 1));

  if Assigned(TGocciaGC.Instance) then
  begin
    TGocciaGC.Instance.PinValue(FSharedArrayPrototype);
    TGocciaGC.Instance.PinValue(FPrototypeMethodHost);
  end;
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
  Result := TGocciaNumberLiteralValue.Create(TGocciaArrayValue(ThisValue).Elements.Count);
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
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  Callback := ValidateArrayMethodCall('map', Args, ThisValue, True);
  Arr := TGocciaArrayValue(ThisValue);
  ResultArray := TGocciaArrayValue.Create;

  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, ThisValue]);
  try
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      if IsArrayHole(Arr.Elements[I]) then
      begin
        ResultArray.Elements.Add(nil);
        Continue;
      end;

      CallArgs.SetElement(0, Arr.Elements[I]);
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
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  ResultArray: TGocciaArrayValue;
  CallArgs: TGocciaArgumentsCollection;
  PredicateResult: TGocciaValue;
  I: Integer;
begin
  Callback := ValidateArrayMethodCall('filter', Args, ThisValue, True);
  Arr := TGocciaArrayValue(ThisValue);
  ResultArray := TGocciaArrayValue.Create;

  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, ThisValue]);
  try
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      if IsArrayHole(Arr.Elements[I]) then
        Continue;

      CallArgs.SetElement(0, Arr.Elements[I]);
      CallArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(I));
      PredicateResult := InvokeCallback(Callback, CallArgs, ThisValue);

      if PredicateResult is TGocciaBooleanLiteralValue then
      begin
        if TGocciaBooleanLiteralValue(PredicateResult).Value then
          ResultArray.Elements.Add(Arr.Elements[I]);
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
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  Accumulator: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I, StartIndex: Integer;
begin
  Callback := ValidateArrayMethodCall('reduce', Args, ThisValue, True);
  Arr := TGocciaArrayValue(ThisValue);

  if Args.Length >= 2 then
  begin
    Accumulator := Args.GetElement(1);
    StartIndex := 0;
  end
  else
  begin
    if Arr.Elements.Count = 0 then
      ThrowError('Reduce of empty array with no initial value');
    Accumulator := Arr.Elements[0];
    StartIndex := 1;
  end;

  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, nil, ThisValue]);
  try
    for I := StartIndex to Arr.Elements.Count - 1 do
    begin
      if IsArrayHole(Arr.Elements[I]) then
        Continue;

      CallArgs.SetElement(0, Accumulator);
      CallArgs.SetElement(1, Arr.Elements[I]);
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
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  Callback := ValidateArrayMethodCall('forEach', Args, ThisValue, True);
  Arr := TGocciaArrayValue(ThisValue);

  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, ThisValue]);
  try
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      if IsArrayHole(Arr.Elements[I]) then
        Continue;

      CallArgs.SetElement(0, Arr.Elements[I]);
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
  Arr: TGocciaArrayValue;
  Separator: string;
  I: Integer;
  ResultString: string;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.join called on non-array');

  Arr := TGocciaArrayValue(ThisValue);

  if Args.Length < 1 then
    Separator := ','
  else
    Separator := Args.GetElement(0).ToStringLiteral.Value;

  ResultString := '';
  for I := 0 to Arr.Elements.Count - 1 do
  begin
    if I > 0 then
      ResultString := ResultString + Separator;
    ResultString := ResultString + Arr.Elements[I].ToStringLiteral.Value;
  end;

  Result := TGocciaStringLiteralValue.Create(ResultString);
end;

function TGocciaArrayValue.ArrayIncludes(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  SearchValue: TGocciaValue;
  FromIndex: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.includes called on non-array');

  Arr := TGocciaArrayValue(ThisValue);

  if Args.Length < 1 then
  begin
    if Arr.Includes(TGocciaUndefinedLiteralValue.UndefinedValue, 0) then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  SearchValue := Args.GetElement(0);

  FromIndex := 0;

  if Args.Length > 1 then
    FromIndex := Trunc(Args.GetElement(1).ToNumberLiteral.Value);

  if Arr.Includes(SearchValue, FromIndex) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaArrayValue.ArraySome(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
  SomeResult: TGocciaValue;
begin
  Callback := ValidateArrayMethodCall('some', Args, ThisValue, True);
  Arr := TGocciaArrayValue(ThisValue);

  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, ThisValue]);
  try
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      if IsArrayHole(Arr.Elements[I]) then
        Continue;

      CallArgs.SetElement(0, Arr.Elements[I]);
      CallArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(I));
      SomeResult := InvokeCallback(Callback, CallArgs, ThisValue);
      if SomeResult.ToBooleanLiteral.Value then
      begin
        Result := TGocciaBooleanLiteralValue.TrueValue;
        Exit;
      end;
    end;
  finally
    CallArgs.Free;
  end;

  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaArrayValue.ArrayEvery(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
  EveryResult: TGocciaValue;
begin
  Callback := ValidateArrayMethodCall('every', Args, ThisValue, True);
  Arr := TGocciaArrayValue(ThisValue);

  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, ThisValue]);
  try
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      if IsArrayHole(Arr.Elements[I]) then
        Continue;

      CallArgs.SetElement(0, Arr.Elements[I]);
      CallArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(I));
      EveryResult := InvokeCallback(Callback, CallArgs, ThisValue);
      if not EveryResult.ToBooleanLiteral.Value then
      begin
        Result := TGocciaBooleanLiteralValue.FalseValue;
        Exit;
      end;
    end;
  finally
    CallArgs.Free;
  end;

  Result := TGocciaBooleanLiteralValue.TrueValue;
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
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  Depth: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.flat called on non-array');

  Arr := TGocciaArrayValue(ThisValue);
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
  Arr.FlattenInto(ResultArray, Depth);
  Result := ResultArray;
end;

function TGocciaArrayValue.ArrayFlatMap(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I, J: Integer;
  MappedValue: TGocciaValue;
begin
  Callback := ValidateArrayMethodCall('flatMap', Args, ThisValue, True);
  Arr := TGocciaArrayValue(ThisValue);
  ResultArray := TGocciaArrayValue.Create;

  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, ThisValue]);
  try
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      if IsArrayHole(Arr.Elements[I]) then
        Continue;

      CallArgs.SetElement(0, Arr.Elements[I]);
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
  Arr: TGocciaArrayValue;
  I: Integer;
begin
  if Args.Length < 1 then
    ThrowError('Array.push expects at least one argument');

  Arr := TGocciaArrayValue(ThisValue);

  for I := 0 to Args.Length - 1 do
    Arr.Elements.Add(Args.GetElement(I));

  Result := TGocciaNumberLiteralValue.Create(Arr.Elements.Count);
end;

function TGocciaArrayValue.ArrayPop(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
begin
  Arr := TGocciaArrayValue(ThisValue);

  if Arr.Elements.Count = 0 then
    ThrowError('Array.pop called on empty array');

  Result := Arr.Elements[Arr.Elements.Count - 1];
  Arr.Elements.Delete(Arr.Elements.Count - 1);
end;

function TGocciaArrayValue.ArraySlice(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  StartIndex, EndIndex: Integer;
  I: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.slice called on non-array');

  Arr := TGocciaArrayValue(ThisValue);
  ResultArray := TGocciaArrayValue.Create;

  if Args.Length >= 1 then
    StartIndex := Trunc(Args.GetElement(0).ToNumberLiteral.Value)
  else
    StartIndex := 0;

  if StartIndex < 0 then
    StartIndex := Arr.Elements.Count + StartIndex;

  if StartIndex < 0 then
    StartIndex := 0
  else if StartIndex > Arr.Elements.Count then
    StartIndex := Arr.Elements.Count;

  if Args.Length >= 2 then
    EndIndex := Trunc(Args.GetElement(1).ToNumberLiteral.Value)
  else
    EndIndex := Arr.Elements.Count;

  if EndIndex < 0 then
    EndIndex := Arr.Elements.Count + EndIndex;

  if EndIndex < 0 then
    EndIndex := 0
  else if EndIndex > Arr.Elements.Count then
    EndIndex := Arr.Elements.Count;

  for I := StartIndex to EndIndex - 1 do
  begin
    if (I >= 0) and (I < Arr.Elements.Count) then
    begin
      if Arr.Elements[I] = nil then
        ResultArray.Elements.Add(nil)
      else
        ResultArray.Elements.Add(Arr.Elements[I]);
    end;
  end;

  Result := ResultArray;
end;

function TGocciaArrayValue.ArrayFind(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
  Element, CallResult: TGocciaValue;
begin
  Callback := ValidateArrayMethodCall('find', Args, ThisValue, True);
  Arr := TGocciaArrayValue(ThisValue);

  CallArgs := nil;
  try
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      Element := Arr.Elements[I];
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
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
  Element, CallResult: TGocciaValue;
begin
  Callback := ValidateArrayMethodCall('findIndex', Args, ThisValue, True);
  Arr := TGocciaArrayValue(ThisValue);

  CallArgs := nil;
  try
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      Element := Arr.Elements[I];
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
  Arr: TGocciaArrayValue;
  SearchValue: TGocciaValue;
  I, FromIndex: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.indexOf called on non-array');

  Arr := TGocciaArrayValue(ThisValue);

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
      FromIndex := Arr.Elements.Count + FromIndex;
    if FromIndex < 0 then
      FromIndex := 0;
  end;

  for I := FromIndex to Arr.Elements.Count - 1 do
  begin
    if IsArrayHole(Arr.Elements[I]) then
      Continue;
    if IsStrictEqual(Arr.Elements[I], SearchValue) then
    begin
      Result := TGocciaNumberLiteralValue.Create(I);
      Exit;
    end;
  end;

  Result := TGocciaNumberLiteralValue.Create(-1);
end;

function TGocciaArrayValue.ArrayLastIndexOf(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  SearchValue: TGocciaValue;
  I, FromIndex: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.lastIndexOf called on non-array');

  Arr := TGocciaArrayValue(ThisValue);

  if Args.Length < 1 then
  begin
    Result := TGocciaNumberLiteralValue.Create(-1);
    Exit;
  end;

  SearchValue := Args.GetElement(0);

  FromIndex := Arr.Elements.Count - 1;
  if Args.Length > 1 then
  begin
    FromIndex := Trunc(Args.GetElement(1).ToNumberLiteral.Value);
    if FromIndex < 0 then
      FromIndex := Arr.Elements.Count + FromIndex;
  end;

  if FromIndex >= Arr.Elements.Count then
    FromIndex := Arr.Elements.Count - 1;

  for I := FromIndex downto 0 do
  begin
    if IsArrayHole(Arr.Elements[I]) then
      Continue;
    if IsStrictEqual(Arr.Elements[I], SearchValue) then
    begin
      Result := TGocciaNumberLiteralValue.Create(I);
      Exit;
    end;
  end;

  Result := TGocciaNumberLiteralValue.Create(-1);
end;

function TGocciaArrayValue.ArrayConcat(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  I, J: Integer;
  Arg: TGocciaValue;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.concat called on non-array');

  Arr := TGocciaArrayValue(ThisValue);
  ResultArray := TGocciaArrayValue.Create;

  for I := 0 to Arr.Elements.Count - 1 do
    ResultArray.Elements.Add(Arr.Elements[I]);

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
  Arr: TGocciaArrayValue;
  I, J: Integer;
  Temp: TGocciaValue;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.reverse called on non-array');

  Arr := TGocciaArrayValue(ThisValue);
  I := 0;
  J := Arr.Elements.Count - 1;
  while I < J do
  begin
    Temp := Arr.Elements[I];
    Arr.Elements[I] := Arr.Elements[J];
    Arr.Elements[J] := Temp;
    Inc(I);
    Dec(J);
  end;

  Result := ThisValue;
end;

function TGocciaArrayValue.ArrayToReversed(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
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

function TGocciaArrayValue.ArrayToSorted(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  CustomSortFunction: TGocciaValue;
  I: Integer;
  CallArgs: TGocciaArgumentsCollection;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.toSorted called on non-array');

  Arr := TGocciaArrayValue(ThisValue);
  ResultArray := TGocciaArrayValue.Create;

  for I := 0 to Arr.Elements.Count - 1 do
    ResultArray.Elements.Add(Arr.Elements[I]);

  if Args.Length > 0 then
  begin
    CustomSortFunction := Args.GetElement(0);

    if not ((CustomSortFunction is TGocciaFunctionValue) or (CustomSortFunction is TGocciaNativeFunctionValue)) then
      ThrowError('Custom sort function must be a function');

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
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  StartIndex, DeleteCount: Integer;
  I: Integer;
  ActualStartIndex: Integer;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.toSpliced called on non-array');

  Arr := TGocciaArrayValue(ThisValue);
  ResultArray := TGocciaArrayValue.Create;

  if Args.Length < 1 then
    StartIndex := 0
  else
    StartIndex := Trunc(Args.GetElement(0).ToNumberLiteral.Value);

  if StartIndex < 0 then
    ActualStartIndex := Arr.Elements.Count + StartIndex
  else
    ActualStartIndex := StartIndex;

  if ActualStartIndex < 0 then
    ActualStartIndex := 0
  else if ActualStartIndex > Arr.Elements.Count then
    ActualStartIndex := Arr.Elements.Count;

  if Args.Length < 2 then
    DeleteCount := Arr.Elements.Count - ActualStartIndex
  else
    DeleteCount := Trunc(Args.GetElement(1).ToNumberLiteral.Value);

  if DeleteCount < 0 then
    DeleteCount := 0
  else if ActualStartIndex + DeleteCount > Arr.Elements.Count then
    DeleteCount := Arr.Elements.Count - ActualStartIndex;

  for I := 0 to ActualStartIndex - 1 do
  begin
    if Arr.Elements[I] = nil then
      ResultArray.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue)
    else
      ResultArray.Elements.Add(Arr.Elements[I]);
  end;

  for I := 2 to Args.Length - 1 do
    ResultArray.Elements.Add(Args.GetElement(I));

  for I := ActualStartIndex + DeleteCount to Arr.Elements.Count - 1 do
  begin
    if Arr.Elements[I] = nil then
      ResultArray.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue)
    else
      ResultArray.Elements.Add(Arr.Elements[I]);
  end;

  Result := ResultArray;
end;

function TGocciaArrayValue.ToStringLiteral: TGocciaStringLiteralValue;
var
  I: Integer;
  SB: TStringBuilder;
begin
  if FElements.Count = 0 then
  begin
    Result := TGocciaStringLiteralValue.Create('');
    Exit;
  end;

  SB := TStringBuilder.Create;
  try
    for I := 0 to FElements.Count - 1 do
    begin
      if I > 0 then
        SB.Append(',');
      if FElements[I] <> nil then
        SB.Append(FElements[I].ToStringLiteral.Value);
    end;
    Result := TGocciaStringLiteralValue.Create(SB.ToString);
  finally
    SB.Free;
  end;
end;

function TGocciaArrayValue.ToNumberLiteral: TGocciaNumberLiteralValue;
begin
  // ECMAScript: [] -> "" -> 0, [n] -> n, [a, b] -> NaN
  if FElements.Count = 0 then
    Result := TGocciaNumberLiteralValue.ZeroValue
  else if FElements.Count = 1 then
    Result := FElements[0].ToNumberLiteral
  else
    Result := TGocciaNumberLiteralValue.NaNValue;
end;

function TGocciaArrayValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.TrueValue;
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
