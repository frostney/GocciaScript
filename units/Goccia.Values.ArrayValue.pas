unit Goccia.Values.ArrayValue;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaArrayValue = class(TGocciaInstanceValue)
  private
    class var FSharedArrayPrototype: TGocciaObjectValue;
    class var FPrototypeMethodHost: TGocciaArrayValue;

    function GetLengthProperty(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; inline;

    // Helper methods for reducing duplication
    function ValidateArrayMethodCall(const AMethodName: string; const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue; const ARequiresCallback: Boolean = True): TGocciaValue;
    function IsArrayHole(const AElement: TGocciaValue): Boolean; inline;

    function ArrayMap(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayFilter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayReduce(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArraySome(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayEvery(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayFlat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure FlattenInto(const ATarget: TGocciaArrayValue; const ADepth: Integer);
    function ArrayFlatMap(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function ArrayJoin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayIncludes(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function ArrayPush(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayPop(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArraySlice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function ArrayFind(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayFindIndex(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayLastIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayConcat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayReverse(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function ArrayToReversed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayToSorted(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayToSpliced(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function ArraySort(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArraySplice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayShift(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayUnshift(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayFill(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayAt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function ArrayValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArraySymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    procedure ThrowError(const AMessage: string; const AArgs: array of const); overload; inline;
    procedure ThrowError(const AMessage: string); overload; inline;
  protected
    FElements: TObjectList<TGocciaValue>;
  public
    constructor Create(const AClass: TGocciaClassValue = nil);
    destructor Destroy; override;
    procedure InitializePrototype;

    function ToStringTag: string; override;

    function ToStringLiteral: TGocciaStringLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;

    // Index-based element access
    function GetLength: Integer;
    function GetElement(const AIndex: Integer): TGocciaValue;
    function SetElement(const AIndex: Integer; const AValue: TGocciaValue): Boolean;
    function TypeName: string; override;
    function GetProperty(const AName: string): TGocciaValue; override;
    procedure SetProperty(const AName: string; const AValue: TGocciaValue); override;
    function Includes(const AValue: TGocciaValue; AFromIndex: Integer = 0): Boolean;

    procedure InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); override;
    procedure MarkReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);

    property Elements: TObjectList<TGocciaValue> read FElements;
  end;

  function DefaultCompare(constref A, B: TGocciaValue): Integer;

implementation

uses
  Generics.Defaults,
  Math,
  SysUtils,

  Goccia.Error,
  Goccia.Evaluator.Comparison,
  Goccia.GarbageCollector,
  Goccia.Utils,
  Goccia.Values.ClassHelper,
  Goccia.Values.FunctionValue,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

function NumericRank(const ANum: TGocciaNumberLiteralValue): Double;
begin
  if ANum.IsNaN then
    Result := Infinity
  else if ANum.IsNegativeInfinity then
    Result := -1.7e308
  else if ANum.IsInfinity then
    Result := 1.7e308
  else if ANum.IsNegativeZero then
    Result := -1e-324
  else
    Result := ANum.Value;
end;

function DefaultCompare(constref A, B: TGocciaValue): Integer;
var
  RankA, RankB: Double;
  StrA, StrB: string;
begin
  if A is TGocciaNumberLiteralValue then
  begin
    RankA := NumericRank(TGocciaNumberLiteralValue(A));
    RankB := NumericRank(TGocciaNumberLiteralValue(B.ToNumberLiteral));
    if RankA < RankB then
      Result := -1
    else if RankA > RankB then
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

function CallCompareFunc(const ACompareFunc: TGocciaValue; const ACallArgs: TGocciaArgumentsCollection;
  const A, B: TGocciaValue; const AThisValue: TGocciaValue): Double;
var
  CompResult: TGocciaNumberLiteralValue;
begin
  ACallArgs.SetElement(0, A);
  ACallArgs.SetElement(1, B);
  if ACompareFunc is TGocciaNativeFunctionValue then
    CompResult := TGocciaNativeFunctionValue(ACompareFunc).Call(ACallArgs, AThisValue).ToNumberLiteral
  else
    CompResult := TGocciaFunctionValue(ACompareFunc).Call(ACallArgs, AThisValue).ToNumberLiteral;

  if CompResult.IsNaN then
    Result := 0
  else if CompResult.IsInfinity then
    Result := 1
  else if CompResult.IsNegativeInfinity then
    Result := -1
  else
    Result := CompResult.Value;
end;

procedure QuickSortElements(const AElements: TObjectList<TGocciaValue>; const ACompareFunc: TGocciaValue;
  const ACallArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue; const ALo, AHi: Integer);
var
  I, J: Integer;
  Pivot: TGocciaValue;
begin
  if ALo >= AHi then Exit;

  Pivot := AElements[(ALo + AHi) div 2];
  I := ALo;
  J := AHi;

  while I <= J do
  begin
    while CallCompareFunc(ACompareFunc, ACallArgs, AElements[I], Pivot, AThisValue) < 0 do
      Inc(I);
    while CallCompareFunc(ACompareFunc, ACallArgs, AElements[J], Pivot, AThisValue) > 0 do
      Dec(J);

    if I <= J then
    begin
      AElements.Exchange(I, J);
      Inc(I);
      Dec(J);
    end;
  end;

  if ALo < J then
    QuickSortElements(AElements, ACompareFunc, ACallArgs, AThisValue, ALo, J);
  if I < AHi then
    QuickSortElements(AElements, ACompareFunc, ACallArgs, AThisValue, I, AHi);
end;

function InvokeCallback(const ACallback: TGocciaValue; const ACallArgs: TGocciaArgumentsCollection;
  const AThisArray: TGocciaValue): TGocciaValue; inline;
begin
  // Pass undefined as ThisValue so arrow function callbacks inherit
  // 'this' from their lexical scope rather than receiving the array.
  // The array is already available as the third argument in ACallArgs.
  if ACallback is TGocciaNativeFunctionValue then
    Result := TGocciaNativeFunctionValue(ACallback).Call(ACallArgs, TGocciaUndefinedLiteralValue.UndefinedValue)
  else
    Result := TGocciaFunctionValue(ACallback).Call(ACallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
end;

function CreateArrayCallbackArgs(const AElement: TGocciaValue; const AIndex: Integer; const AThisArray: TGocciaValue): TGocciaArgumentsCollection;
begin
  Result := TGocciaArgumentsCollection.Create([AElement, TGocciaNumberLiteralValue.SmallInt(AIndex), AThisArray]);
end;

constructor TGocciaArrayValue.Create(const AClass: TGocciaClassValue = nil);
begin
  inherited Create(AClass);
  FElements := TObjectList<TGocciaValue>.Create(False);
  InitializePrototype;
  if not Assigned(AClass) and Assigned(FSharedArrayPrototype) then
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
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayValues, 'values', 0));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayKeys, 'keys', 0));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayEntries, 'entries', 0));

  FSharedArrayPrototype.DefineSymbolProperty(
    TGocciaSymbolValue.WellKnownIterator,
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.Create(ArraySymbolIterator, '[Symbol.iterator]', 0),
      [pfConfigurable, pfWritable]
    )
  );

  if Assigned(TGocciaGarbageCollector.Instance) then
  begin
    TGocciaGarbageCollector.Instance.PinValue(FSharedArrayPrototype);
    TGocciaGarbageCollector.Instance.PinValue(FPrototypeMethodHost);
  end;
end;

class procedure TGocciaArrayValue.ExposePrototype(const AConstructor: TGocciaValue);
begin
  if not Assigned(FSharedArrayPrototype) then
    TGocciaArrayValue.Create;
  if AConstructor is TGocciaClassValue then
    TGocciaClassValue(AConstructor).ReplacePrototype(FSharedArrayPrototype)
  else if AConstructor is TGocciaObjectValue then
    TGocciaObjectValue(AConstructor).AssignProperty('prototype', FSharedArrayPrototype);
  FSharedArrayPrototype.AssignProperty('constructor', AConstructor);
end;

destructor TGocciaArrayValue.Destroy;
begin
  FElements.Free;
  inherited;
end;

procedure TGocciaArrayValue.InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection);
var
  LenArg: TGocciaValue;
  Len: Double;
  I: Integer;
begin
  if AArguments.Length = 0 then
    Exit;

  if AArguments.Length = 1 then
  begin
    LenArg := AArguments.GetElement(0);
    if LenArg is TGocciaNumberLiteralValue then
    begin
      Len := TGocciaNumberLiteralValue(LenArg).Value;
      if (Len <> Trunc(Len)) or (Len < 0) or (Len > 4294967295) then
        raise Exception.Create('Invalid array length');
      FElements.Count := Trunc(Len);
    end
    else
      FElements.Add(LenArg);
  end
  else
  begin
    FElements.Capacity := AArguments.Length;
    for I := 0 to AArguments.Length - 1 do
      FElements.Add(AArguments.GetElement(I));
  end;
end;

procedure TGocciaArrayValue.MarkReferences;
var
  I: Integer;
begin
  if GCMarked then Exit;
  inherited;

  for I := 0 to FElements.Count - 1 do
  begin
    if Assigned(FElements[I]) then
      FElements[I].MarkReferences;
  end;
end;

function TGocciaArrayValue.ToStringTag: string;
begin
  Result := 'Array';
end;

procedure TGocciaArrayValue.ThrowError(const AMessage: string; const AArgs: array of const);
begin
  raise TGocciaError.Create(Format(AMessage, AArgs), 0, 0, '', nil);
end;

procedure TGocciaArrayValue.ThrowError(const AMessage: string);
begin
  ThrowError(AMessage, []);
end;

function TGocciaArrayValue.GetLengthProperty(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(TGocciaArrayValue(AThisValue).Elements.Count);
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

function TGocciaArrayValue.SetElement(const AIndex: Integer; const AValue: TGocciaValue): Boolean;
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

function TGocciaArrayValue.ValidateArrayMethodCall(const AMethodName: string; const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue; const ARequiresCallback: Boolean = True): TGocciaValue;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.%s called on non-array', [AMethodName]);

  if AArgs.Length < 1 then
    ThrowError('Array.%s expects callback function', [AMethodName]);

  Result := AArgs.GetElement(0);

  if ARequiresCallback then
  begin
    if not Result.IsCallable then
      ThrowError('Callback must be a function');
  end
  else
  begin
    if Result is TGocciaUndefinedLiteralValue then
      ThrowError('Callback must not be undefined');
  end;
end;

function TGocciaArrayValue.IsArrayHole(const AElement: TGocciaValue): Boolean;
begin
  Result := AElement = nil;
end;

function TGocciaArrayValue.ArrayMap(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  Callback := ValidateArrayMethodCall('map', AArgs, AThisValue, True);
  Arr := TGocciaArrayValue(AThisValue);
  ResultArray := TGocciaArrayValue.Create;

  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, AThisValue]);
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
      ResultArray.Elements.Add(InvokeCallback(Callback, CallArgs, AThisValue));
    end;
  finally
    CallArgs.Free;
  end;

  Result := ResultArray;
end;

function TGocciaArrayValue.ArrayFilter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  ResultArray: TGocciaArrayValue;
  CallArgs: TGocciaArgumentsCollection;
  PredicateResult: TGocciaValue;
  I: Integer;
begin
  Callback := ValidateArrayMethodCall('filter', AArgs, AThisValue, True);
  Arr := TGocciaArrayValue(AThisValue);
  ResultArray := TGocciaArrayValue.Create;

  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, AThisValue]);
  try
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      if IsArrayHole(Arr.Elements[I]) then
        Continue;

      CallArgs.SetElement(0, Arr.Elements[I]);
      CallArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(I));
      PredicateResult := InvokeCallback(Callback, CallArgs, AThisValue);

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

function TGocciaArrayValue.ArrayReduce(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  Accumulator: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I, StartIndex: Integer;
begin
  Callback := ValidateArrayMethodCall('reduce', AArgs, AThisValue, True);
  Arr := TGocciaArrayValue(AThisValue);

  if AArgs.Length >= 2 then
  begin
    Accumulator := AArgs.GetElement(1);
    StartIndex := 0;
  end
  else
  begin
    if Arr.Elements.Count = 0 then
      ThrowError('Reduce of empty array with no initial value');
    Accumulator := Arr.Elements[0];
    StartIndex := 1;
  end;

  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, nil, AThisValue]);
  try
    for I := StartIndex to Arr.Elements.Count - 1 do
    begin
      if IsArrayHole(Arr.Elements[I]) then
        Continue;

      CallArgs.SetElement(0, Accumulator);
      CallArgs.SetElement(1, Arr.Elements[I]);
      CallArgs.SetElement(2, TGocciaNumberLiteralValue.SmallInt(I));
      Accumulator := InvokeCallback(Callback, CallArgs, AThisValue);
    end;
  finally
    CallArgs.Free;
  end;

  Result := Accumulator;
end;

function TGocciaArrayValue.ArrayForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  Callback := ValidateArrayMethodCall('forEach', AArgs, AThisValue, True);
  Arr := TGocciaArrayValue(AThisValue);

  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, AThisValue]);
  try
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      if IsArrayHole(Arr.Elements[I]) then
        Continue;

      CallArgs.SetElement(0, Arr.Elements[I]);
      CallArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(I));
      InvokeCallback(Callback, CallArgs, AThisValue);
    end;
  finally
    CallArgs.Free;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaArrayValue.ArrayJoin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Separator: string;
  I: Integer;
  ResultString: string;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.join called on non-array');

  Arr := TGocciaArrayValue(AThisValue);

  if AArgs.Length < 1 then
    Separator := ','
  else
    Separator := AArgs.GetElement(0).ToStringLiteral.Value;

  ResultString := '';
  for I := 0 to Arr.Elements.Count - 1 do
  begin
    if I > 0 then
      ResultString := ResultString + Separator;
    ResultString := ResultString + Arr.Elements[I].ToStringLiteral.Value;
  end;

  Result := TGocciaStringLiteralValue.Create(ResultString);
end;

function TGocciaArrayValue.ArrayIncludes(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  SearchValue: TGocciaValue;
  FromIndex: Integer;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.includes called on non-array');

  Arr := TGocciaArrayValue(AThisValue);

  if AArgs.Length < 1 then
  begin
    if Arr.Includes(TGocciaUndefinedLiteralValue.UndefinedValue, 0) then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  SearchValue := AArgs.GetElement(0);

  FromIndex := 0;

  if AArgs.Length > 1 then
    FromIndex := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value);

  if Arr.Includes(SearchValue, FromIndex) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaArrayValue.ArraySome(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
  SomeResult: TGocciaValue;
begin
  Callback := ValidateArrayMethodCall('some', AArgs, AThisValue, True);
  Arr := TGocciaArrayValue(AThisValue);

  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, AThisValue]);
  try
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      if IsArrayHole(Arr.Elements[I]) then
        Continue;

      CallArgs.SetElement(0, Arr.Elements[I]);
      CallArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(I));
      SomeResult := InvokeCallback(Callback, CallArgs, AThisValue);
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

function TGocciaArrayValue.ArrayEvery(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
  EveryResult: TGocciaValue;
begin
  Callback := ValidateArrayMethodCall('every', AArgs, AThisValue, True);
  Arr := TGocciaArrayValue(AThisValue);

  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, AThisValue]);
  try
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      if IsArrayHole(Arr.Elements[I]) then
        Continue;

      CallArgs.SetElement(0, Arr.Elements[I]);
      CallArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(I));
      EveryResult := InvokeCallback(Callback, CallArgs, AThisValue);
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

procedure TGocciaArrayValue.FlattenInto(const ATarget: TGocciaArrayValue; const ADepth: Integer);
var
  I: Integer;
begin
  for I := 0 to Elements.Count - 1 do
  begin
    if IsArrayHole(Elements[I]) then
      Continue;

    if (Elements[I] is TGocciaArrayValue) and (ADepth > 0) then
      TGocciaArrayValue(Elements[I]).FlattenInto(ATarget, ADepth - 1)
    else
      ATarget.Elements.Add(Elements[I]);
  end;
end;

function TGocciaArrayValue.ArrayFlat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  Depth: Integer;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.flat called on non-array');

  Arr := TGocciaArrayValue(AThisValue);
  Depth := 1;

  if AArgs.Length > 0 then
  begin
    if not (AArgs.GetElement(0) is TGocciaNumberLiteralValue) then
      ThrowError('Array.flat expects depth argument to be a number');

    Depth := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);

    if Depth < 0 then
      Depth := 0;
  end;

  ResultArray := TGocciaArrayValue.Create;
  Arr.FlattenInto(ResultArray, Depth);
  Result := ResultArray;
end;

function TGocciaArrayValue.ArrayFlatMap(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I, J: Integer;
  MappedValue: TGocciaValue;
begin
  Callback := ValidateArrayMethodCall('flatMap', AArgs, AThisValue, True);
  Arr := TGocciaArrayValue(AThisValue);
  ResultArray := TGocciaArrayValue.Create;

  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, AThisValue]);
  try
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      if IsArrayHole(Arr.Elements[I]) then
        Continue;

      CallArgs.SetElement(0, Arr.Elements[I]);
      CallArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(I));
      MappedValue := InvokeCallback(Callback, CallArgs, AThisValue);

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

function TGocciaArrayValue.ArrayPush(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  I: Integer;
begin
  if AArgs.Length < 1 then
    ThrowError('Array.push expects at least one argument');

  Arr := TGocciaArrayValue(AThisValue);

  for I := 0 to AArgs.Length - 1 do
    Arr.Elements.Add(AArgs.GetElement(I));

  Result := TGocciaNumberLiteralValue.Create(Arr.Elements.Count);
end;

function TGocciaArrayValue.ArrayPop(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
begin
  Arr := TGocciaArrayValue(AThisValue);

  if Arr.Elements.Count = 0 then
    ThrowError('Array.pop called on empty array');

  Result := Arr.Elements[Arr.Elements.Count - 1];
  Arr.Elements.Delete(Arr.Elements.Count - 1);
end;

function TGocciaArrayValue.ArraySlice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  StartIndex, EndIndex: Integer;
  I: Integer;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.slice called on non-array');

  Arr := TGocciaArrayValue(AThisValue);
  ResultArray := TGocciaArrayValue.Create;

  if AArgs.Length >= 1 then
    StartIndex := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value)
  else
    StartIndex := 0;

  if StartIndex < 0 then
    StartIndex := Arr.Elements.Count + StartIndex;

  if StartIndex < 0 then
    StartIndex := 0
  else if StartIndex > Arr.Elements.Count then
    StartIndex := Arr.Elements.Count;

  if AArgs.Length >= 2 then
    EndIndex := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value)
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

function TGocciaArrayValue.ArrayFind(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
  Element, CallResult: TGocciaValue;
begin
  Callback := ValidateArrayMethodCall('find', AArgs, AThisValue, True);
  Arr := TGocciaArrayValue(AThisValue);

  CallArgs := nil;
  try
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      Element := Arr.Elements[I];
      if IsArrayHole(Element) then
        Element := TGocciaUndefinedLiteralValue.UndefinedValue;

      CallArgs.Free;
      CallArgs := CreateArrayCallbackArgs(Element, I, AThisValue);
      CallResult := InvokeCallback(Callback, CallArgs, AThisValue);
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

function TGocciaArrayValue.ArrayFindIndex(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
  Element, CallResult: TGocciaValue;
begin
  Callback := ValidateArrayMethodCall('findIndex', AArgs, AThisValue, True);
  Arr := TGocciaArrayValue(AThisValue);

  CallArgs := nil;
  try
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      Element := Arr.Elements[I];
      if IsArrayHole(Element) then
        Element := TGocciaUndefinedLiteralValue.UndefinedValue;

      CallArgs.Free;
      CallArgs := CreateArrayCallbackArgs(Element, I, AThisValue);
      CallResult := InvokeCallback(Callback, CallArgs, AThisValue);
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

function TGocciaArrayValue.ArrayIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  SearchValue: TGocciaValue;
  I, FromIndex: Integer;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.indexOf called on non-array');

  Arr := TGocciaArrayValue(AThisValue);

  if AArgs.Length < 1 then
  begin
    Result := TGocciaNumberLiteralValue.Create(-1);
    Exit;
  end;

  SearchValue := AArgs.GetElement(0);

  FromIndex := 0;
  if AArgs.Length > 1 then
  begin
    FromIndex := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value);
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

function TGocciaArrayValue.ArrayLastIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  SearchValue: TGocciaValue;
  I, FromIndex: Integer;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.lastIndexOf called on non-array');

  Arr := TGocciaArrayValue(AThisValue);

  if AArgs.Length < 1 then
  begin
    Result := TGocciaNumberLiteralValue.Create(-1);
    Exit;
  end;

  SearchValue := AArgs.GetElement(0);

  FromIndex := Arr.Elements.Count - 1;
  if AArgs.Length > 1 then
  begin
    FromIndex := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value);
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

function TGocciaArrayValue.ArrayConcat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  I, J: Integer;
  Arg: TGocciaValue;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.concat called on non-array');

  Arr := TGocciaArrayValue(AThisValue);
  ResultArray := TGocciaArrayValue.Create;

  for I := 0 to Arr.Elements.Count - 1 do
    ResultArray.Elements.Add(Arr.Elements[I]);

  for I := 0 to AArgs.Length - 1 do
  begin
    Arg := AArgs.GetElement(I);
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

function TGocciaArrayValue.ArrayReverse(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  I, J: Integer;
  Temp: TGocciaValue;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.reverse called on non-array');

  Arr := TGocciaArrayValue(AThisValue);
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

  Result := AThisValue;
end;

function TGocciaArrayValue.ArrayToReversed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  I: Integer;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.toReversed called on non-array');

  Arr := TGocciaArrayValue(AThisValue);
  ResultArray := TGocciaArrayValue.Create;

  for I := Arr.Elements.Count - 1 downto 0 do
    ResultArray.Elements.Add(Arr.Elements[I]);

  Result := ResultArray;
end;

function TGocciaArrayValue.ArrayToSorted(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  CustomSortFunction: TGocciaValue;
  I: Integer;
  CallArgs: TGocciaArgumentsCollection;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.toSorted called on non-array');

  Arr := TGocciaArrayValue(AThisValue);
  ResultArray := TGocciaArrayValue.Create;

  for I := 0 to Arr.Elements.Count - 1 do
    ResultArray.Elements.Add(Arr.Elements[I]);

  if AArgs.Length > 0 then
  begin
    CustomSortFunction := AArgs.GetElement(0);

    if not CustomSortFunction.IsCallable then
      ThrowError('Custom sort function must be a function');

    CallArgs := TGocciaArgumentsCollection.Create([nil, nil]);
    try
      QuickSortElements(ResultArray.Elements, CustomSortFunction, CallArgs, AThisValue, 0, ResultArray.Elements.Count - 1);
    finally
      CallArgs.Free;
    end;
  end else
  begin
    ResultArray.Elements.Sort(TComparer<TGocciaValue>.Construct(DefaultCompare));
  end;

  Result := ResultArray;
end;

function TGocciaArrayValue.ArrayToSpliced(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  StartIndex, DeleteCount: Integer;
  I: Integer;
  ActualStartIndex: Integer;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.toSpliced called on non-array');

  Arr := TGocciaArrayValue(AThisValue);
  ResultArray := TGocciaArrayValue.Create;

  if AArgs.Length < 1 then
    StartIndex := 0
  else
    StartIndex := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);

  if StartIndex < 0 then
    ActualStartIndex := Arr.Elements.Count + StartIndex
  else
    ActualStartIndex := StartIndex;

  if ActualStartIndex < 0 then
    ActualStartIndex := 0
  else if ActualStartIndex > Arr.Elements.Count then
    ActualStartIndex := Arr.Elements.Count;

  if AArgs.Length < 2 then
    DeleteCount := Arr.Elements.Count - ActualStartIndex
  else
    DeleteCount := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value);

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

  for I := 2 to AArgs.Length - 1 do
    ResultArray.Elements.Add(AArgs.GetElement(I));

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

function TGocciaArrayValue.Includes(const AValue: TGocciaValue; AFromIndex: Integer = 0): Boolean;
var
  I: Integer;
begin
  if AFromIndex < 0 then
    AFromIndex := FElements.Count + AFromIndex;

  if AFromIndex < 0 then
    AFromIndex := 0;

  for I := AFromIndex to FElements.Count - 1 do
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

procedure TGocciaArrayValue.SetProperty(const AName: string; const AValue: TGocciaValue);
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

function TGocciaArrayValue.ArraySort(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  CustomSortFunction: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.sort called on non-array');

  Arr := TGocciaArrayValue(AThisValue);

  if AArgs.Length > 0 then
  begin
    CustomSortFunction := AArgs.GetElement(0);

    if not CustomSortFunction.IsCallable then
      ThrowError('Custom sort function must be a function');

    // Quicksort with custom comparison function (mutates in-place)
    // Reuse a single args collection across all comparisons
    CallArgs := TGocciaArgumentsCollection.Create([nil, nil]);
    try
      QuickSortElements(Arr.Elements, CustomSortFunction, CallArgs, AThisValue, 0, Arr.Elements.Count - 1);
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

function TGocciaArrayValue.ArraySplice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Removed: TGocciaArrayValue;
  StartIndex, DeleteCount, ActualStart: Integer;
  I: Integer;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.splice called on non-array');

  Arr := TGocciaArrayValue(AThisValue);
  Removed := TGocciaArrayValue.Create;

  // Handle start index
  if AArgs.Length < 1 then
  begin
    Result := Removed;
    Exit;
  end;

  StartIndex := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);

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
  if AArgs.Length < 2 then
    DeleteCount := Arr.Elements.Count - ActualStart
  else
    DeleteCount := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value);

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
  for I := 2 to AArgs.Length - 1 do
  begin
    Arr.Elements.Insert(ActualStart + (I - 2), AArgs.GetElement(I));
  end;

  Result := Removed;
end;

function TGocciaArrayValue.ArrayShift(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.shift called on non-array');

  Arr := TGocciaArrayValue(AThisValue);

  if Arr.Elements.Count = 0 then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  Result := Arr.Elements[0];
  Arr.Elements.Delete(0);
end;

function TGocciaArrayValue.ArrayUnshift(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  I: Integer;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.unshift called on non-array');

  Arr := TGocciaArrayValue(AThisValue);

  // Insert elements at the beginning in the correct order
  for I := AArgs.Length - 1 downto 0 do
    Arr.Elements.Insert(0, AArgs.GetElement(I));

  // Return new length
  Result := TGocciaNumberLiteralValue.Create(Arr.Elements.Count);
end;

function TGocciaArrayValue.ArrayFill(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  FillValue: TGocciaValue;
  StartIdx, EndIdx, I: Integer;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.fill called on non-array');

  Arr := TGocciaArrayValue(AThisValue);

  if AArgs.Length < 1 then
    FillValue := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    FillValue := AArgs.GetElement(0);

  if AArgs.Length > 1 then
    StartIdx := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value)
  else
    StartIdx := 0;

  if AArgs.Length > 2 then
    EndIdx := Trunc(AArgs.GetElement(2).ToNumberLiteral.Value)
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

function TGocciaArrayValue.ArrayAt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Index: Integer;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.at called on non-array');

  Arr := TGocciaArrayValue(AThisValue);

  if AArgs.Length < 1 then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  Index := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);

  // Handle negative index
  if Index < 0 then
    Index := Arr.Elements.Count + Index;

  if (Index < 0) or (Index >= Arr.Elements.Count) then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    Result := Arr.Elements[Index];
end;

function TGocciaArrayValue.ArrayValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.values called on non-array');
  Result := TGocciaArrayIteratorValue.Create(AThisValue, akValues);
end;

function TGocciaArrayValue.ArrayKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.keys called on non-array');
  Result := TGocciaArrayIteratorValue.Create(AThisValue, akKeys);
end;

function TGocciaArrayValue.ArrayEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.entries called on non-array');
  Result := TGocciaArrayIteratorValue.Create(AThisValue, akEntries);
end;

function TGocciaArrayValue.ArraySymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('[Symbol.iterator] called on non-array');
  Result := TGocciaArrayIteratorValue.Create(AThisValue, akValues);
end;

end.
