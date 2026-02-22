unit Goccia.Values.ArrayValue;

{$I Goccia.inc}

interface

uses
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

    function ArrayFindLast(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayFindLastIndex(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayCopyWithin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function ArrayValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArraySymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    procedure ThrowError(const AMessage: string; const AArgs: array of const); overload; inline;
    procedure ThrowError(const AMessage: string); overload; inline;
  protected
    FElements: TGocciaValueList;
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

    property Elements: TGocciaValueList read FElements;
  end;

  function DefaultCompare(constref A, B: TGocciaValue): Integer;

implementation

uses
  Generics.Defaults,
  Math,
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.Evaluator.Comparison,
  Goccia.GarbageCollector,
  Goccia.Utils,
  Goccia.Utils.Arrays,
  Goccia.Values.ClassHelper,
  Goccia.Values.ErrorHelper,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

function DefaultCompare(constref A, B: TGocciaValue): Integer;
var
  StrA, StrB: string;
begin
  StrA := A.ToStringLiteral.Value;
  StrB := B.ToStringLiteral.Value;
  if StrA < StrB then
    Result := -1
  else if StrA > StrB then
    Result := 1
  else
    Result := 0;
end;

// ES2026 §7.3.35 ArraySpeciesCreate(originalArray, length)
function ArraySpeciesCreate(const AOriginal: TGocciaArrayValue; const ALength: Integer): TGocciaArrayValue;
var
  ConstructorMethod, Species: TGocciaValue;
  SpeciesClass: TGocciaClassValue;
  LengthArgs: TGocciaArgumentsCollection;
  Instance: TGocciaValue;
begin
  // Step 3: Let C be Get(originalArray, "constructor")
  ConstructorMethod := AOriginal.GetProperty(PROP_CONSTRUCTOR);

  // Steps 4-5: If C is an object, get @@species; if null/undefined, fall through
  if ConstructorMethod is TGocciaClassValue then
  begin
    SpeciesClass := TGocciaClassValue(ConstructorMethod);
    Species := SpeciesClass.GetSymbolProperty(TGocciaSymbolValue.WellKnownSpecies);

    if (Species is TGocciaUndefinedLiteralValue) or (Species is TGocciaNullLiteralValue) then
    begin
      // Step 6: If C is undefined, return ArrayCreate(length)
      Result := TGocciaArrayValue.Create;
      Exit;
    end;

    // Step 7: If IsConstructor(C) is false, throw TypeError
    if not (Species is TGocciaClassValue) then
      ThrowTypeError('Species is not a constructor');

    // Step 8: Return Construct(C, « length »)
    SpeciesClass := TGocciaClassValue(Species);
    LengthArgs := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.SmallInt(ALength)]);
    try
      Instance := SpeciesClass.Instantiate(LengthArgs);
    finally
      LengthArgs.Free;
    end;

    if Instance is TGocciaArrayValue then
    begin
      Result := TGocciaArrayValue(Instance);
      Exit;
    end;
  end;

  // Step 6: If C is undefined, return ArrayCreate(length)
  Result := TGocciaArrayValue.Create;
end;


function CallCompareFunc(const ACompareFunc: TGocciaValue; const ACallArgs: TGocciaArgumentsCollection;
  const A, B: TGocciaValue; const AThisValue: TGocciaValue): Double;
var
  CompResult: TGocciaNumberLiteralValue;
begin
  ACallArgs.SetElement(0, A);
  ACallArgs.SetElement(1, B);
  CompResult := InvokeCallable(ACompareFunc, ACallArgs, AThisValue).ToNumberLiteral;

  if CompResult.IsNaN then
    Result := 0
  else if CompResult.IsInfinity then
    Result := 1
  else if CompResult.IsNegativeInfinity then
    Result := -1
  else
    Result := CompResult.Value;
end;

procedure QuickSortElements(const AElements: TGocciaValueList; const ACompareFunc: TGocciaValue;
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

function CreateArrayCallbackArgs(const AElement: TGocciaValue; const AIndex: Integer; const AThisArray: TGocciaValue): TGocciaArgumentsCollection;
begin
  Result := TGocciaArgumentsCollection.Create([AElement, TGocciaNumberLiteralValue.SmallInt(AIndex), AThisArray]);
end;

constructor TGocciaArrayValue.Create(const AClass: TGocciaClassValue = nil);
begin
  inherited Create(AClass);
  FElements := TGocciaValueList.Create(False);
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
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayFindLast, 'findLast', 1));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayFindLastIndex, 'findLastIndex', 1));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayWith, 'with', 2));
  FSharedArrayPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayCopyWithin, 'copyWithin', 2));
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
    TGocciaObjectValue(AConstructor).AssignProperty(PROP_PROTOTYPE, FSharedArrayPrototype);
  FSharedArrayPrototype.AssignProperty(PROP_CONSTRUCTOR, AConstructor);
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
        ThrowRangeError('Invalid array length');
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
  // ES2026 §23.1.3.18 step 4: ArraySpeciesCreate(O, len)
  ResultArray := ArraySpeciesCreate(Arr, Arr.Elements.Count);

  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, AThisValue]);
  try
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      // Step 6c: If kPresent, CreateDataPropertyOrThrow(A, Pk, mappedValue)
      if IsArrayHole(Arr.Elements[I]) then
        Continue;

      CallArgs.SetElement(0, Arr.Elements[I]);
      CallArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(I));
      ArrayCreateDataProperty(ResultArray, I, InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue));
    end;
  finally
    CallArgs.Free;
  end;

  Result := ResultArray;
end;

// ES2026 §23.1.3.8 Array.prototype.filter(callbackfn [, thisArg])
function TGocciaArrayValue.ArrayFilter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  ResultArray: TGocciaArrayValue;
  CallArgs: TGocciaArgumentsCollection;
  PredicateResult: TGocciaValue;
  I: Integer;
begin
  // Steps 1-2: Let O be ToObject(this), let len be LengthOfArrayLike(O)
  // Step 3: If IsCallable(callbackfn) is false, throw TypeError
  Callback := ValidateArrayMethodCall('filter', AArgs, AThisValue, True);
  Arr := TGocciaArrayValue(AThisValue);
  // Step 4: Let A be ArraySpeciesCreate(O, 0)
  ResultArray := ArraySpeciesCreate(Arr, 0);

  // Step 5: Let k be 0, let to be 0
  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, AThisValue]);
  try
    // Step 6: Repeat, while k < len
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      // Step 6b: Let kPresent be HasProperty(O, Pk)
      if IsArrayHole(Arr.Elements[I]) then
        Continue;

      // Step 6c-i: Let kValue be Get(O, Pk)
      CallArgs.SetElement(0, Arr.Elements[I]);
      CallArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(I));
      // Step 6c-ii: Let selected be ToBoolean(Call(callbackfn, thisArg, « kValue, k, O »))
      PredicateResult := InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);

      // Step 6c-iii: If selected is true, CreateDataPropertyOrThrow(A, ToString(to), kValue)
      if PredicateResult.ToBooleanLiteral.Value then
        ResultArray.Elements.Add(Arr.Elements[I]);
    end;
  finally
    CallArgs.Free;
  end;

  // Step 7: Return A
  Result := ResultArray;
end;

// ES2026 §23.1.3.22 Array.prototype.reduce(callbackfn [, initialValue])
function TGocciaArrayValue.ArrayReduce(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  Accumulator: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I, StartIndex: Integer;
begin
  // Steps 1-2: Let O be ToObject(this), let len be LengthOfArrayLike(O)
  // Step 3: If IsCallable(callbackfn) is false, throw TypeError
  Callback := ValidateArrayMethodCall('reduce', AArgs, AThisValue, True);
  Arr := TGocciaArrayValue(AThisValue);

  // Steps 4-6: If initialValue is present, set accumulator; else find first element
  if AArgs.Length >= 2 then
  begin
    Accumulator := AArgs.GetElement(1);
    StartIndex := 0;
  end
  else
  begin
    // Step 6: If kPresent is false, throw TypeError
    if Arr.Elements.Count = 0 then
      ThrowTypeError('Reduce of empty array with no initial value');
    Accumulator := Arr.Elements[0];
    StartIndex := 1;
  end;

  // Step 7: Repeat, while k < len
  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, nil, AThisValue]);
  try
    for I := StartIndex to Arr.Elements.Count - 1 do
    begin
      // Step 7b: Let kPresent be HasProperty(O, Pk)
      if IsArrayHole(Arr.Elements[I]) then
        Continue;

      // Step 7c: Let accumulator be Call(callbackfn, undefined, « accumulator, kValue, k, O »)
      CallArgs.SetElement(0, Accumulator);
      CallArgs.SetElement(1, Arr.Elements[I]);
      CallArgs.SetElement(2, TGocciaNumberLiteralValue.SmallInt(I));
      Accumulator := InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    end;
  finally
    CallArgs.Free;
  end;

  // Step 8: Return accumulator
  Result := Accumulator;
end;

// ES2026 §23.1.3.12 Array.prototype.forEach(callbackfn [, thisArg])
function TGocciaArrayValue.ArrayForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  // Steps 1-2: Let O be ToObject(this), let len be LengthOfArrayLike(O)
  // Step 3: If IsCallable(callbackfn) is false, throw TypeError
  Callback := ValidateArrayMethodCall('forEach', AArgs, AThisValue, True);
  Arr := TGocciaArrayValue(AThisValue);

  // Step 4: Let k be 0
  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, AThisValue]);
  try
    // Step 5: Repeat, while k < len
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      // Step 5b: Let kPresent be HasProperty(O, Pk)
      if IsArrayHole(Arr.Elements[I]) then
        Continue;

      // Step 5c: Let kValue be Get(O, Pk)
      CallArgs.SetElement(0, Arr.Elements[I]);
      CallArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(I));
      // Step 5d: Call(callbackfn, thisArg, « kValue, k, O »)
      InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    end;
  finally
    CallArgs.Free;
  end;

  // Step 6: Return undefined
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §23.1.3.16 Array.prototype.join(separator)
function TGocciaArrayValue.ArrayJoin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Separator: string;
  I: Integer;
  ResultString: string;
begin
  // Step 1: Let O be ToObject(this value)
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.join called on non-array');

  // Step 2: Let len be LengthOfArrayLike(O)
  Arr := TGocciaArrayValue(AThisValue);

  // Step 3: If separator is undefined, let sep be ","
  // Step 4: Else let sep be ToString(separator)
  if (AArgs.Length < 1) or (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
    Separator := ','
  else
    Separator := AArgs.GetElement(0).ToStringLiteral.Value;

  // Steps 5-8: Build result string, treating undefined/null elements as empty string
  ResultString := '';
  for I := 0 to Arr.Elements.Count - 1 do
  begin
    if I > 0 then
      ResultString := ResultString + Separator;
    // Step 7b: If element is undefined or null, let next be ""
    if (Arr.Elements[I] = nil) or
       (Arr.Elements[I] is TGocciaUndefinedLiteralValue) or
       (Arr.Elements[I] is TGocciaNullLiteralValue) then
      Continue
    else
      ResultString := ResultString + Arr.Elements[I].ToStringLiteral.Value;
  end;

  // Step 9: Return R
  Result := TGocciaStringLiteralValue.Create(ResultString);
end;

// ES2026 §23.1.3.14 Array.prototype.includes(searchElement [, fromIndex])
function TGocciaArrayValue.ArrayIncludes(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  SearchValue: TGocciaValue;
  FromIndex: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.includes called on non-array');

  // Step 2: Let len be LengthOfArrayLike(O)
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

  // Steps 4-5: Let n be ToIntegerOrInfinity(fromIndex), compute k
  FromIndex := ToIntegerFromArgs(AArgs, 1);

  // Steps 6-8: Search using SameValueZero comparison
  if Arr.Includes(SearchValue, FromIndex) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §23.1.3.28 Array.prototype.some(callbackfn [, thisArg])
function TGocciaArrayValue.ArraySome(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
  SomeResult: TGocciaValue;
begin
  // Steps 1-3: Let O be ToObject(this), let len be LengthOfArrayLike(O), validate callbackfn
  Callback := ValidateArrayMethodCall('some', AArgs, AThisValue, True);
  Arr := TGocciaArrayValue(AThisValue);

  // Step 4: Let k be 0
  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, AThisValue]);
  try
    // Step 5: Repeat, while k < len
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      if IsArrayHole(Arr.Elements[I]) then
        Continue;

      // Step 5c-i: Let kValue be Get(O, Pk)
      CallArgs.SetElement(0, Arr.Elements[I]);
      CallArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(I));
      // Step 5c-ii: Let testResult be ToBoolean(Call(callbackfn, thisArg, « kValue, k, O »))
      SomeResult := InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
      // Step 5c-iii: If testResult is true, return true
      if SomeResult.ToBooleanLiteral.Value then
      begin
        Result := TGocciaBooleanLiteralValue.TrueValue;
        Exit;
      end;
    end;
  finally
    CallArgs.Free;
  end;

  // Step 6: Return false
  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §23.1.3.6 Array.prototype.every(callbackfn [, thisArg])
function TGocciaArrayValue.ArrayEvery(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
  EveryResult: TGocciaValue;
begin
  // Steps 1-3: Let O be ToObject(this), let len be LengthOfArrayLike(O), validate callbackfn
  Callback := ValidateArrayMethodCall('every', AArgs, AThisValue, True);
  Arr := TGocciaArrayValue(AThisValue);

  // Step 4: Let k be 0
  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, AThisValue]);
  try
    // Step 5: Repeat, while k < len
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      if IsArrayHole(Arr.Elements[I]) then
        Continue;

      // Step 5c-i: Let kValue be Get(O, Pk)
      CallArgs.SetElement(0, Arr.Elements[I]);
      CallArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(I));
      // Step 5c-ii: Let testResult be ToBoolean(Call(callbackfn, thisArg, « kValue, k, O »))
      EveryResult := InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
      // Step 5c-iii: If testResult is false, return false
      if not EveryResult.ToBooleanLiteral.Value then
      begin
        Result := TGocciaBooleanLiteralValue.FalseValue;
        Exit;
      end;
    end;
  finally
    CallArgs.Free;
  end;

  // Step 6: Return true
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

// ES2026 §23.1.3.11.1 FlattenIntoArray(target, source, ..., depth)
procedure TGocciaArrayValue.FlattenInto(const ATarget: TGocciaArrayValue; const ADepth: Integer);
var
  I: Integer;
begin
  // Step 3: Repeat, while sourceIndex < sourceLen
  for I := 0 to Elements.Count - 1 do
  begin
    if IsArrayHole(Elements[I]) then
      Continue;

    // Step 3c-v: If depth > 0 and IsConcatSpreadable(element), recurse
    if (Elements[I] is TGocciaArrayValue) and (ADepth > 0) then
      TGocciaArrayValue(Elements[I]).FlattenInto(ATarget, ADepth - 1)
    else
      // Step 3c-v-2: Else, CreateDataPropertyOrThrow(target, targetIndex, element)
      ATarget.Elements.Add(Elements[I]);
  end;
end;

// ES2026 §23.1.3.11 Array.prototype.flat([depth])
function TGocciaArrayValue.ArrayFlat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  Depth: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.flat called on non-array');

  // Step 2: Let sourceLen be LengthOfArrayLike(O)
  Arr := TGocciaArrayValue(AThisValue);
  // Step 3: Let depthNum be 1 (default)
  Depth := 1;

  // Step 4: If depth is not undefined, let depthNum be ToIntegerOrInfinity(depth)
  if AArgs.Length > 0 then
  begin
    if not (AArgs.GetElement(0) is TGocciaNumberLiteralValue) then
      ThrowError('Array.flat expects depth argument to be a number');

    if AArgs.GetElement(0).ToNumberLiteral.IsInfinity then
      Depth := MaxInt
    else
      Depth := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);

    if Depth < 0 then
      Depth := 0;
  end;

  // Step 5: Let A be ArraySpeciesCreate(O, 0)
  ResultArray := ArraySpeciesCreate(Arr, 0);
  // Step 6: FlattenIntoArray(A, O, sourceLen, 0, depthNum)
  Arr.FlattenInto(ResultArray, Depth);
  // Step 7: Return A
  Result := ResultArray;
end;

// ES2026 §23.1.3.10 Array.prototype.flatMap(mapperFunction [, thisArg])
function TGocciaArrayValue.ArrayFlatMap(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I, J: Integer;
  MappedValue: TGocciaValue;
begin
  // Steps 1-3: Let O be ToObject(this), let len, validate callbackfn
  Callback := ValidateArrayMethodCall('flatMap', AArgs, AThisValue, True);
  Arr := TGocciaArrayValue(AThisValue);
  // Step 4: Let A be ArraySpeciesCreate(O, 0)
  ResultArray := ArraySpeciesCreate(Arr, 0);

  // Step 5: FlattenIntoArray(A, O, sourceLen, 0, 1, mapperFunction, thisArg)
  CallArgs := TGocciaArgumentsCollection.Create([nil, nil, AThisValue]);
  try
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      if IsArrayHole(Arr.Elements[I]) then
        Continue;

      // Step: Let mappedValue be Call(mapperFunction, thisArg, « kValue, k, O »)
      CallArgs.SetElement(0, Arr.Elements[I]);
      CallArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(I));
      MappedValue := InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);

      // Step: If IsConcatSpreadable(mappedValue), flatten one level
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

  // Step 6: Return A
  Result := ResultArray;
end;

// ES2026 §23.1.3.21 Array.prototype.push(...items)
function TGocciaArrayValue.ArrayPush(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  I: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  // Step 2: Let len be LengthOfArrayLike(O)
  Arr := TGocciaArrayValue(AThisValue);

  // Steps 4-5: For each element E of items, Set(O, ToString(len), E), len = len + 1
  for I := 0 to AArgs.Length - 1 do
    Arr.Elements.Add(AArgs.GetElement(I));

  // Step 7: Return len
  Result := TGocciaNumberLiteralValue.Create(Arr.Elements.Count);
end;

// ES2026 §23.1.3.20 Array.prototype.pop()
function TGocciaArrayValue.ArrayPop(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
begin
  // Step 1: Let O be ToObject(this value)
  // Step 2: Let len be LengthOfArrayLike(O)
  Arr := TGocciaArrayValue(AThisValue);

  // Step 3: If len = 0, set length to 0 and return undefined
  if Arr.Elements.Count = 0 then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  // Step 4: Let newLen be len - 1
  // Step 5: Let element be Get(O, ToString(newLen))
  Result := Arr.Elements[Arr.Elements.Count - 1];
  // Step 6: DeletePropertyOrThrow(O, ToString(newLen))
  // Step 7: Set(O, "length", newLen)
  Arr.Elements.Delete(Arr.Elements.Count - 1);
end;

// ES2026 §23.1.3.26 Array.prototype.slice(start, end)
function TGocciaArrayValue.ArraySlice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  StartIndex, EndIndex: Integer;
  I, N: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.slice called on non-array');

  // Step 2: Let len be LengthOfArrayLike(O)
  Arr := TGocciaArrayValue(AThisValue);

  // Step 3: Let relativeStart be ToIntegerOrInfinity(start)
  StartIndex := ToIntegerFromArgs(AArgs);

  // Step 4: If relativeStart < 0, let k be max(len + relativeStart, 0); else min(relativeStart, len)
  StartIndex := NormalizeRelativeIndex(StartIndex, Arr.Elements.Count);

  // Step 5: If end is undefined, let relativeEnd be len; else ToIntegerOrInfinity(end)
  EndIndex := ToIntegerFromArgs(AArgs, 1, Arr.Elements.Count);

  // Step 6: If relativeEnd < 0, let final be max(len + relativeEnd, 0); else min(relativeEnd, len)
  EndIndex := NormalizeRelativeIndex(EndIndex, Arr.Elements.Count);

  // Step 7: Let count be max(final - k, 0)
  // Step 8: Let A be ArraySpeciesCreate(O, count)
  ResultArray := ArraySpeciesCreate(Arr, Max(EndIndex - StartIndex, 0));

  // Step 9: Let n be 0; repeat while k < final
  N := 0;
  for I := StartIndex to EndIndex - 1 do
  begin
    if (I >= 0) and (I < Arr.Elements.Count) then
    begin
      // Step 9c: CreateDataPropertyOrThrow(A, ToString(n), kValue)
      ArrayCreateDataProperty(ResultArray, N, Arr.Elements[I]);
      Inc(N);
    end;
  end;

  // Step 11: Return A
  Result := ResultArray;
end;

// ES2026 §23.1.3.9 Array.prototype.find(predicate [, thisArg])
function TGocciaArrayValue.ArrayFind(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
  Element, CallResult: TGocciaValue;
begin
  // Steps 1-3: Let O be ToObject(this), let len, validate predicate
  Callback := ValidateArrayMethodCall('find', AArgs, AThisValue, True);
  Arr := TGocciaArrayValue(AThisValue);

  // Step 4: Let k be 0
  CallArgs := nil;
  try
    // Step 5: Repeat, while k < len
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      // Step 5a: Let kValue be Get(O, Pk)
      Element := Arr.Elements[I];
      if IsArrayHole(Element) then
        Element := TGocciaUndefinedLiteralValue.UndefinedValue;

      CallArgs.Free;
      CallArgs := CreateArrayCallbackArgs(Element, I, AThisValue);
      // Step 5b: Let testResult be ToBoolean(Call(predicate, thisArg, « kValue, k, O »))
      CallResult := InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
      // Step 5c: If testResult is true, return kValue
      if CallResult.ToBooleanLiteral.Value then
      begin
        Result := Element;
        Exit;
      end;
    end;
  finally
    CallArgs.Free;
  end;

  // Step 6: Return undefined
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §23.1.3.10 Array.prototype.findIndex(predicate [, thisArg])
function TGocciaArrayValue.ArrayFindIndex(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
  Element, CallResult: TGocciaValue;
begin
  // Steps 1-3: Let O be ToObject(this), let len, validate predicate
  Callback := ValidateArrayMethodCall('findIndex', AArgs, AThisValue, True);
  Arr := TGocciaArrayValue(AThisValue);

  // Step 4: Let k be 0
  CallArgs := nil;
  try
    // Step 5: Repeat, while k < len
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      // Step 5a: Let kValue be Get(O, Pk)
      Element := Arr.Elements[I];
      if IsArrayHole(Element) then
        Element := TGocciaUndefinedLiteralValue.UndefinedValue;

      CallArgs.Free;
      CallArgs := CreateArrayCallbackArgs(Element, I, AThisValue);
      // Step 5b: Let testResult be ToBoolean(Call(predicate, thisArg, « kValue, k, O »))
      CallResult := InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
      // Step 5c: If testResult is true, return k
      if CallResult.ToBooleanLiteral.Value then
      begin
        Result := TGocciaNumberLiteralValue.SmallInt(I);
        Exit;
      end;
    end;
  finally
    CallArgs.Free;
  end;

  // Step 6: Return -1𝔽
  Result := TGocciaNumberLiteralValue.Create(-1);
end;

// ES2026 §23.1.3.11 Array.prototype.findLast(predicate [, thisArg])
function TGocciaArrayValue.ArrayFindLast(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
  Element, CallResult: TGocciaValue;
begin
  // Steps 1-3: Let O be ToObject(this), let len, validate predicate
  Callback := ValidateArrayMethodCall('findLast', AArgs, AThisValue, True);
  Arr := TGocciaArrayValue(AThisValue);

  // Step 4: Let k be len - 1
  CallArgs := nil;
  try
    // Step 5: Repeat, while k >= 0
    for I := Arr.Elements.Count - 1 downto 0 do
    begin
      // Step 5a: Let kValue be Get(O, Pk)
      Element := Arr.Elements[I];
      if IsArrayHole(Element) then
        Element := TGocciaUndefinedLiteralValue.UndefinedValue;

      CallArgs.Free;
      CallArgs := CreateArrayCallbackArgs(Element, I, AThisValue);
      // Step 5b: Let testResult be ToBoolean(Call(predicate, thisArg, « kValue, k, O »))
      CallResult := InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
      // Step 5c: If testResult is true, return kValue
      if CallResult.ToBooleanLiteral.Value then
      begin
        Result := Element;
        Exit;
      end;
    end;
  finally
    CallArgs.Free;
  end;

  // Step 6: Return undefined
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §23.1.3.12 Array.prototype.findLastIndex(predicate [, thisArg])
function TGocciaArrayValue.ArrayFindLastIndex(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
  Element, CallResult: TGocciaValue;
begin
  // Steps 1-3: Let O be ToObject(this), let len, validate predicate
  Callback := ValidateArrayMethodCall('findLastIndex', AArgs, AThisValue, True);
  Arr := TGocciaArrayValue(AThisValue);

  // Step 4: Let k be len - 1
  CallArgs := nil;
  try
    // Step 5: Repeat, while k >= 0
    for I := Arr.Elements.Count - 1 downto 0 do
    begin
      // Step 5a: Let kValue be Get(O, Pk)
      Element := Arr.Elements[I];
      if IsArrayHole(Element) then
        Element := TGocciaUndefinedLiteralValue.UndefinedValue;

      CallArgs.Free;
      CallArgs := CreateArrayCallbackArgs(Element, I, AThisValue);
      // Step 5b: Let testResult be ToBoolean(Call(predicate, thisArg, « kValue, k, O »))
      CallResult := InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
      // Step 5c: If testResult is true, return k
      if CallResult.ToBooleanLiteral.Value then
      begin
        Result := TGocciaNumberLiteralValue.SmallInt(I);
        Exit;
      end;
    end;
  finally
    CallArgs.Free;
  end;

  // Step 6: Return -1𝔽
  Result := TGocciaNumberLiteralValue.Create(-1);
end;

// ES2026 §23.1.3.38 Array.prototype.with(index, value)
function TGocciaArrayValue.ArrayWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr, ResultArray: TGocciaArrayValue;
  Index, ActualIndex, I: Integer;
  NewValue: TGocciaValue;
begin
  // Step 1: Let O be ToObject(this value)
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.with called on non-array');

  // Step 2: Let len be LengthOfArrayLike(O)
  Arr := TGocciaArrayValue(AThisValue);

  if AArgs.Length < 2 then
    ThrowError('Array.with requires index and value arguments');

  // Step 3: Let relativeIndex be ToIntegerOrInfinity(index)
  Index := ToIntegerFromArgs(AArgs);
  // Step 5: Let value be args[1]
  NewValue := AArgs.GetElement(1);

  // Step 4: If relativeIndex >= 0, let actualIndex be relativeIndex; else len + relativeIndex
  if Index < 0 then
    ActualIndex := Arr.Elements.Count + Index
  else
    ActualIndex := Index;

  // Step 6: If actualIndex >= len or actualIndex < 0, throw RangeError
  if (ActualIndex < 0) or (ActualIndex >= Arr.Elements.Count) then
    ThrowRangeError('Invalid index for Array.with');

  // Step 7: Let A be ArrayCreate(len)
  ResultArray := TGocciaArrayValue.Create;
  // Step 8: Let k be 0; repeat while k < len
  for I := 0 to Arr.Elements.Count - 1 do
  begin
    // Step 8b: If k = actualIndex, let fromValue be value; else Get(O, Pk)
    if I = ActualIndex then
      ResultArray.Elements.Add(NewValue)
    else
      ResultArray.Elements.Add(Arr.Elements[I]);
  end;

  // Step 9: Return A
  Result := ResultArray;
end;

// ES2026 §23.1.3.4 Array.prototype.copyWithin(target, start [, end])
function TGocciaArrayValue.ArrayCopyWithin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Target, Start, EndIdx, Len, Count, I: Integer;
  Temp: array of TGocciaValue;
begin
  // Step 1: Let O be ToObject(this value)
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.copyWithin called on non-array');

  // Step 2: Let len be LengthOfArrayLike(O)
  Arr := TGocciaArrayValue(AThisValue);
  Len := Arr.Elements.Count;

  if AArgs.Length < 1 then
    ThrowError('Array.copyWithin requires a target argument');

  // Step 3: Let relativeTarget be ToIntegerOrInfinity(target)
  Target := ToIntegerFromArgs(AArgs);
  // Step 4: If relativeTarget < 0, let to be max(len + relativeTarget, 0); else min(relativeTarget, len)
  Target := NormalizeRelativeIndex(Target, Len);

  // Step 5: Let relativeStart be ToIntegerOrInfinity(start)
  Start := ToIntegerFromArgs(AArgs, 1);
  // Step 6: If relativeStart < 0, let from be max(len + relativeStart, 0); else min(relativeStart, len)
  Start := NormalizeRelativeIndex(Start, Len);

  // Step 7: If end is undefined, let relativeEnd be len; else ToIntegerOrInfinity(end)
  EndIdx := ToIntegerFromArgs(AArgs, 2, Len);
  // Step 8: If relativeEnd < 0, let final be max(len + relativeEnd, 0); else min(relativeEnd, len)
  EndIdx := NormalizeRelativeIndex(EndIdx, Len);

  // Step 9: Let count be min(final - from, len - to)
  Count := Min(EndIdx - Start, Len - Target);
  if Count <= 0 then
  begin
    Result := Arr;
    Exit;
  end;

  // Steps 10-12: Copy elements (using temp buffer for overlap safety)
  SetLength(Temp, Count);
  for I := 0 to Count - 1 do
    Temp[I] := Arr.Elements[Start + I];
  for I := 0 to Count - 1 do
    Arr.Elements[Target + I] := Temp[I];

  // Step 13: Return O
  Result := Arr;
end;

// ES2026 §23.1.3.15 Array.prototype.indexOf(searchElement [, fromIndex])
function TGocciaArrayValue.ArrayIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  SearchValue: TGocciaValue;
  I, FromIndex: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.indexOf called on non-array');

  // Step 2: Let len be LengthOfArrayLike(O)
  Arr := TGocciaArrayValue(AThisValue);

  // Step 3: If len = 0, return -1𝔽
  if AArgs.Length < 1 then
  begin
    Result := TGocciaNumberLiteralValue.Create(-1);
    Exit;
  end;

  SearchValue := AArgs.GetElement(0);

  // Step 4: Let n be ToIntegerOrInfinity(fromIndex)
  // Step 6: If n >= 0, let k be n; else let k be max(len + n, 0)
  FromIndex := ToIntegerFromArgs(AArgs, 1);
  if FromIndex < 0 then
    FromIndex := Arr.Elements.Count + FromIndex;
  if FromIndex < 0 then
    FromIndex := 0;

  // Step 7: Repeat, while k < len
  for I := FromIndex to Arr.Elements.Count - 1 do
  begin
    // Step 7a: Let kPresent be HasProperty(O, Pk)
    if IsArrayHole(Arr.Elements[I]) then
      Continue;
    // Step 7b: If kPresent, let elementK be Get(O, Pk)
    // Step 7c: If IsStrictlyEqual(searchElement, elementK) is true, return k
    if IsStrictEqual(Arr.Elements[I], SearchValue) then
    begin
      Result := TGocciaNumberLiteralValue.Create(I);
      Exit;
    end;
  end;

  // Step 8: Return -1𝔽
  Result := TGocciaNumberLiteralValue.Create(-1);
end;

// ES2026 §23.1.3.17 Array.prototype.lastIndexOf(searchElement [, fromIndex])
function TGocciaArrayValue.ArrayLastIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  SearchValue: TGocciaValue;
  I, FromIndex: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.lastIndexOf called on non-array');

  // Step 2: Let len be LengthOfArrayLike(O)
  Arr := TGocciaArrayValue(AThisValue);

  // Step 3: If len = 0, return -1𝔽
  if AArgs.Length < 1 then
  begin
    Result := TGocciaNumberLiteralValue.Create(-1);
    Exit;
  end;

  SearchValue := AArgs.GetElement(0);

  // Step 4: If fromIndex is present, let n be ToIntegerOrInfinity(fromIndex); else let n be len - 1
  FromIndex := ToIntegerFromArgs(AArgs, 1, Arr.Elements.Count - 1);
  // Step 6: If n < 0, let k be len + n
  if FromIndex < 0 then
    FromIndex := Arr.Elements.Count + FromIndex;

  if FromIndex >= Arr.Elements.Count then
    FromIndex := Arr.Elements.Count - 1;

  // Step 7: Repeat, while k >= 0
  for I := FromIndex downto 0 do
  begin
    // Step 7a: Let kPresent be HasProperty(O, Pk)
    if IsArrayHole(Arr.Elements[I]) then
      Continue;
    // Step 7c: If IsStrictlyEqual(searchElement, elementK) is true, return k
    if IsStrictEqual(Arr.Elements[I], SearchValue) then
    begin
      Result := TGocciaNumberLiteralValue.Create(I);
      Exit;
    end;
  end;

  // Step 8: Return -1𝔽
  Result := TGocciaNumberLiteralValue.Create(-1);
end;

// ES2026 §23.1.3.1 Array.prototype.concat(...arguments)
function TGocciaArrayValue.ArrayConcat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  I, J: Integer;
  Arg: TGocciaValue;
begin
  // Step 1: Let O be ToObject(this value)
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.concat called on non-array');

  // Step 2: Let A be ArraySpeciesCreate(O, 0)
  Arr := TGocciaArrayValue(AThisValue);
  ResultArray := ArraySpeciesCreate(Arr, 0);

  // Step 3: Let n be 0
  // Step 4: Prepend O to items (copy this array's elements first)
  for I := 0 to Arr.Elements.Count - 1 do
    ResultArray.Elements.Add(Arr.Elements[I]);

  // Step 5: For each element E of items
  for I := 0 to AArgs.Length - 1 do
  begin
    Arg := AArgs.GetElement(I);
    // Step 5b: If IsConcatSpreadable(E) is true, spread elements
    if Arg is TGocciaArrayValue then
    begin
      for J := 0 to TGocciaArrayValue(Arg).Elements.Count - 1 do
        ResultArray.Elements.Add(TGocciaArrayValue(Arg).Elements[J]);
    end
    // Step 5c: Else, CreateDataPropertyOrThrow(A, ToString(n), E)
    else
      ResultArray.Elements.Add(Arg);
  end;

  // Step 6: Return A
  Result := ResultArray;
end;

// ES2026 §23.1.3.23 Array.prototype.reverse()
function TGocciaArrayValue.ArrayReverse(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  I, J: Integer;
  Temp: TGocciaValue;
begin
  // Step 1: Let O be ToObject(this value)
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.reverse called on non-array');

  // Step 2: Let len be LengthOfArrayLike(O)
  Arr := TGocciaArrayValue(AThisValue);
  // Step 3: Let middle be floor(len / 2), let lower be 0
  I := 0;
  J := Arr.Elements.Count - 1;
  // Step 4: Repeat, while lower ≠ middle
  while I < J do
  begin
    // Steps 4d-i: Swap elements at lower and upper
    Temp := Arr.Elements[I];
    Arr.Elements[I] := Arr.Elements[J];
    Arr.Elements[J] := Temp;
    Inc(I);
    Dec(J);
  end;

  // Step 5: Return O
  Result := AThisValue;
end;

// ES2026 §23.1.3.33 Array.prototype.toReversed()
function TGocciaArrayValue.ArrayToReversed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  I: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.toReversed called on non-array');

  // Step 2: Let len be LengthOfArrayLike(O)
  Arr := TGocciaArrayValue(AThisValue);
  // Step 3: Let A be ArrayCreate(len)
  ResultArray := TGocciaArrayValue.Create;

  // Step 4: Let k be 0; repeat while k < len
  // Step 4b: Let fromValue be Get(O, ToString(len - k - 1))
  for I := Arr.Elements.Count - 1 downto 0 do
    ResultArray.Elements.Add(Arr.Elements[I]);

  // Step 5: Return A
  Result := ResultArray;
end;

// ES2026 §23.1.3.34 Array.prototype.toSorted(comparefn)
function TGocciaArrayValue.ArrayToSorted(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  CustomSortFunction: TGocciaValue;
  I: Integer;
  CallArgs: TGocciaArgumentsCollection;
begin
  // Step 1: Let O be ToObject(this value)
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.toSorted called on non-array');

  // Step 2: If comparefn is not undefined and IsCallable(comparefn) is false, throw TypeError
  // Step 3: Let len be LengthOfArrayLike(O)
  Arr := TGocciaArrayValue(AThisValue);
  // Step 4: Let A be ArrayCreate(len)
  ResultArray := TGocciaArrayValue.Create;

  // Step 5: Copy elements from O into A
  for I := 0 to Arr.Elements.Count - 1 do
    ResultArray.Elements.Add(Arr.Elements[I]);

  // Step 6: Sort A using SortCompare with comparefn
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
    // Default: sort by ToString comparison
    ResultArray.Elements.Sort(TComparer<TGocciaValue>.Construct(DefaultCompare));
  end;

  // Step 7: Return A
  Result := ResultArray;
end;

// ES2026 §23.1.3.35 Array.prototype.toSpliced(start, skipCount, ...items)
function TGocciaArrayValue.ArrayToSpliced(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  ResultArray: TGocciaArrayValue;
  StartIndex, DeleteCount: Integer;
  I: Integer;
  ActualStartIndex: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.toSpliced called on non-array');

  // Step 2: Let len be LengthOfArrayLike(O)
  Arr := TGocciaArrayValue(AThisValue);

  // Step 3: Let relativeStart be ToIntegerOrInfinity(start)
  StartIndex := ToIntegerFromArgs(AArgs);

  // Step 4: If relativeStart < 0, let actualStart be max(len + relativeStart, 0); else min(relativeStart, len)
  ActualStartIndex := NormalizeRelativeIndex(StartIndex, Arr.Elements.Count);

  // Step 5: If skipCount is not present, let actualSkipCount be len - actualStart
  // Step 6: Else let actualSkipCount be max(ToIntegerOrInfinity(skipCount), 0)
  DeleteCount := ToIntegerFromArgs(AArgs, 1, Arr.Elements.Count - ActualStartIndex);
  if DeleteCount < 0 then
    DeleteCount := 0
  else if ActualStartIndex + DeleteCount > Arr.Elements.Count then
    DeleteCount := Arr.Elements.Count - ActualStartIndex;

  // Step 9: Let A be ArrayCreate(newLen)
  ResultArray := TGocciaArrayValue.Create;

  // Step 11: Copy elements before actualStart
  for I := 0 to ActualStartIndex - 1 do
  begin
    if Arr.Elements[I] = nil then
      ResultArray.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue)
    else
      ResultArray.Elements.Add(Arr.Elements[I]);
  end;

  // Step 12: Insert new items
  for I := 2 to AArgs.Length - 1 do
    ResultArray.Elements.Add(AArgs.GetElement(I));

  // Step 13: Copy elements after actualStart + actualSkipCount
  for I := ActualStartIndex + DeleteCount to Arr.Elements.Count - 1 do
  begin
    if Arr.Elements[I] = nil then
      ResultArray.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue)
    else
      ResultArray.Elements.Add(Arr.Elements[I]);
  end;

  // Step 14: Return A
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

// ES2026 §23.1.3.29 Array.prototype.sort(comparefn)
function TGocciaArrayValue.ArraySort(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  CustomSortFunction: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  // Step 1: If comparefn is not undefined and IsCallable(comparefn) is false, throw TypeError
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.sort called on non-array');

  // Step 2: Let obj be ToObject(this value)
  Arr := TGocciaArrayValue(AThisValue);

  // Step 4: Sort elements using SortCompare with comparefn
  if AArgs.Length > 0 then
  begin
    CustomSortFunction := AArgs.GetElement(0);

    if not CustomSortFunction.IsCallable then
      ThrowError('Custom sort function must be a function');

    CallArgs := TGocciaArgumentsCollection.Create([nil, nil]);
    try
      QuickSortElements(Arr.Elements, CustomSortFunction, CallArgs, AThisValue, 0, Arr.Elements.Count - 1);
    finally
      CallArgs.Free;
    end;
  end else
  begin
    // Default: sort by ToString comparison (§23.1.3.29.2 SortCompare)
    Arr.Elements.Sort(TComparer<TGocciaValue>.Construct(DefaultCompare));
  end;

  // Step 5: Return obj (mutated in-place)
  Result := Arr;
end;

// ES2026 §23.1.3.32 Array.prototype.splice(start, deleteCount, ...items)
function TGocciaArrayValue.ArraySplice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Removed: TGocciaArrayValue;
  StartIndex, DeleteCount, ActualStart: Integer;
  I: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.splice called on non-array');

  // Step 2: Let len be LengthOfArrayLike(O)
  Arr := TGocciaArrayValue(AThisValue);

  // Step 5: If start is not present
  if AArgs.Length < 1 then
  begin
    // Step 10: Let A be ArraySpeciesCreate(O, 0)
    Removed := ArraySpeciesCreate(Arr, 0);
    Result := Removed;
    Exit;
  end;

  // Step 3: Let relativeStart be ToIntegerOrInfinity(start)
  StartIndex := ToIntegerFromArgs(AArgs);

  // Step 4: If relativeStart < 0, let actualStart be max(len + relativeStart, 0); else min(relativeStart, len)
  ActualStart := NormalizeRelativeIndex(StartIndex, Arr.Elements.Count);

  // Steps 5-9: Determine actualDeleteCount
  DeleteCount := ToIntegerFromArgs(AArgs, 1, Arr.Elements.Count - ActualStart);
  if DeleteCount < 0 then
    DeleteCount := 0
  else if ActualStart + DeleteCount > Arr.Elements.Count then
    DeleteCount := Arr.Elements.Count - ActualStart;

  // Step 10: Let A be ArraySpeciesCreate(O, actualDeleteCount)
  Removed := ArraySpeciesCreate(Arr, DeleteCount);

  // Step 11: Let k be 0; repeat while k < actualDeleteCount
  // Step 11c: CreateDataPropertyOrThrow(A, ToString(k), fromValue)
  for I := 0 to DeleteCount - 1 do
  begin
    ArrayCreateDataProperty(Removed, I, Arr.Elements[ActualStart]);
    Arr.Elements.Delete(ActualStart);
  end;

  // Steps 14-15: Insert items at actualStart
  for I := 2 to AArgs.Length - 1 do
  begin
    Arr.Elements.Insert(ActualStart + (I - 2), AArgs.GetElement(I));
  end;

  // Step 17: Return A
  Result := Removed;
end;

// ES2026 §23.1.3.25 Array.prototype.shift()
function TGocciaArrayValue.ArrayShift(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
begin
  // Step 1: Let O be ToObject(this value)
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.shift called on non-array');

  // Step 2: Let len be LengthOfArrayLike(O)
  Arr := TGocciaArrayValue(AThisValue);

  // Step 3: If len = 0, set length to 0 and return undefined
  if Arr.Elements.Count = 0 then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  // Step 4: Let first be Get(O, "0")
  Result := Arr.Elements[0];
  // Steps 5-7: Shift elements down, delete last, set length
  Arr.Elements.Delete(0);
end;

// ES2026 §23.1.3.37 Array.prototype.unshift(...items)
function TGocciaArrayValue.ArrayUnshift(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  I: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.unshift called on non-array');

  // Step 2: Let len be LengthOfArrayLike(O)
  Arr := TGocciaArrayValue(AThisValue);

  // Step 3: Let argCount be the number of elements in items
  // Steps 4-5: Shift existing elements up by argCount, then set items at front
  for I := AArgs.Length - 1 downto 0 do
    Arr.Elements.Insert(0, AArgs.GetElement(I));

  // Step 7: Return len + argCount
  Result := TGocciaNumberLiteralValue.Create(Arr.Elements.Count);
end;

// ES2026 §23.1.3.7 Array.prototype.fill(value [, start [, end]])
function TGocciaArrayValue.ArrayFill(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  FillValue: TGocciaValue;
  StartIdx, EndIdx, I: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.fill called on non-array');

  // Step 2: Let len be LengthOfArrayLike(O)
  Arr := TGocciaArrayValue(AThisValue);

  // Step 3: Let value (default undefined)
  if AArgs.Length < 1 then
    FillValue := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    FillValue := AArgs.GetElement(0);

  // Step 4: Let relativeStart be ToIntegerOrInfinity(start)
  StartIdx := ToIntegerFromArgs(AArgs, 1);

  // Step 6: If end is undefined, let relativeEnd be len; else ToIntegerOrInfinity(end)
  EndIdx := ToIntegerFromArgs(AArgs, 2, Arr.Elements.Count);

  // Step 5: If relativeStart < 0, let k be max(len + relativeStart, 0); else min(relativeStart, len)
  StartIdx := NormalizeRelativeIndex(StartIdx, Arr.Elements.Count);
  // Step 7: If relativeEnd < 0, let final be max(len + relativeEnd, 0); else min(relativeEnd, len)
  EndIdx := NormalizeRelativeIndex(EndIdx, Arr.Elements.Count);

  // Step 8: Repeat, while k < final — Set(O, ToString(k), value)
  for I := StartIdx to EndIdx - 1 do
    Arr.Elements[I] := FillValue;

  // Step 9: Return O
  Result := Arr;
end;

// ES2026 §23.1.3.1 Array.prototype.at(index)
function TGocciaArrayValue.ArrayAt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Index: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.at called on non-array');

  // Step 2: Let len be LengthOfArrayLike(O)
  Arr := TGocciaArrayValue(AThisValue);

  if AArgs.Length < 1 then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  // Step 3: Let relativeIndex be ToIntegerOrInfinity(index)
  Index := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);

  // Step 4: If relativeIndex >= 0, let k be relativeIndex; else len + relativeIndex
  if Index < 0 then
    Index := Arr.Elements.Count + Index;

  // Step 5: If k < 0 or k >= len, return undefined
  if (Index < 0) or (Index >= Arr.Elements.Count) then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    // Step 6: Return Get(O, ToString(k))
    Result := Arr.Elements[Index];
end;

// ES2026 §23.1.3.36 Array.prototype.values()
function TGocciaArrayValue.ArrayValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.values called on non-array');
  // Step 1: Let O be ToObject(this value)
  // Step 2: Return CreateArrayIterator(O, value)
  Result := TGocciaArrayIteratorValue.Create(AThisValue, akValues);
end;

// ES2026 §23.1.3.17 Array.prototype.keys()
function TGocciaArrayValue.ArrayKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.keys called on non-array');
  // Step 1: Let O be ToObject(this value)
  // Step 2: Return CreateArrayIterator(O, key)
  Result := TGocciaArrayIteratorValue.Create(AThisValue, akKeys);
end;

// ES2026 §23.1.3.5 Array.prototype.entries()
function TGocciaArrayValue.ArrayEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('Array.entries called on non-array');
  // Step 1: Let O be ToObject(this value)
  // Step 2: Return CreateArrayIterator(O, key+value)
  Result := TGocciaArrayIteratorValue.Create(AThisValue, akEntries);
end;

// ES2026 §23.1.3.39 Array.prototype[@@iterator]()
function TGocciaArrayValue.ArraySymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaArrayValue) then
    ThrowError('[Symbol.iterator] called on non-array');
  // Step 1: Return the result of calling Array.prototype.values
  Result := TGocciaArrayIteratorValue.Create(AThisValue, akValues);
end;

end.
