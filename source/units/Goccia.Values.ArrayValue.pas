unit Goccia.Values.ArrayValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaArrayValue = class(TGocciaInstanceValue)
  private
    // Helper methods for reducing duplication
    function ValidateArrayMethodCall(const AMethodName: string; const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue; const ARequiresCallback: Boolean = True): TGocciaValue;
    function IsArrayHole(const AElement: TGocciaValue): Boolean; inline;

    procedure FlattenInto(const ATarget: TGocciaArrayValue; const ADepth: Integer);
    procedure ThrowError(const AMessage: string; const AArgs: array of const); overload; inline;
    procedure ThrowError(const AMessage: string); overload; inline;
    procedure ThrowError(const AMessage: string; const AArgs: array of const; const ASuggestion: string); overload; inline;
  protected
    FElements: TGocciaValueList;
  public
    constructor Create(const AClass: TGocciaClassValue = nil;
      const AElementCapacity: Integer = 0);
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
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;
    procedure SetProperty(const AName: string; const AValue: TGocciaValue); override;
    function HasOwnProperty(const AName: string): Boolean; override;
    function GetOwnPropertyDescriptor(const AName: string): TGocciaPropertyDescriptor; override;
    procedure DefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor); override;
    function TryDefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor): Boolean; override;
    function Includes(const AValue: TGocciaValue; AFromIndex: Integer = 0): Boolean;

    procedure InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); override;
    procedure MarkReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);

    property Elements: TGocciaValueList read FElements;
  published
    function ArrayMap(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayFilter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayReduce(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArraySome(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayEvery(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayFlat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayFlatMap(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayJoin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
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
    function GetLengthProperty(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

  function DefaultCompare(constref A, B: TGocciaValue): Integer;

implementation

uses
  Generics.Defaults,
  Math,
  SysUtils,

  StringBuffer,

  Goccia.Arguments.Callbacks,
  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Evaluator.Comparison,
  Goccia.GarbageCollector,
  Goccia.Utils,
  Goccia.Utils.Arrays,
  Goccia.Values.ClassHelper,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.HoleValue,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToObject,
  Goccia.Values.ToPrimitive;

threadvar
  FSharedArrayPrototype: TGocciaObjectValue;
  FPrototypeMethodHost: TGocciaArrayValue;
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

type
  // Uniform facade over native arrays and generic array-like objects.
  // Methods use this to operate on any ToObject'd receiver, keeping a fast
  // path for TGocciaArrayValue and a generic path for plain objects.
  TArrayLikeView = record
    Obj: TGocciaObjectValue;
    Len: Integer;
    Arr: TGocciaArrayValue; // non-nil only when Obj is a native array
    procedure Init(const AThisValue: TGocciaValue);
    function Get(const AIndex: Integer): TGocciaValue;
    procedure Put(const AIndex: Integer; const AValue: TGocciaValue);
    function HasIndex(const AIndex: Integer): Boolean;
    procedure DeleteIndex(const AIndex: Integer);
    procedure SetLen(const ANewLen: Integer);
  end;

procedure TArrayLikeView.Init(const AThisValue: TGocciaValue);
begin
  Obj := ToObject(AThisValue);
  if Obj is TGocciaArrayValue then
  begin
    Arr := TGocciaArrayValue(Obj);
    Len := Arr.Elements.Count;
  end
  else
  begin
    Arr := nil;
    Len := LengthOfArrayLike(Obj);
  end;
end;

function TArrayLikeView.Get(const AIndex: Integer): TGocciaValue;
begin
  if Assigned(Arr) then
  begin
    if (AIndex >= 0) and (AIndex < Arr.Elements.Count) and
       (Arr.Elements[AIndex] <> TGocciaHoleValue.HoleValue) then
    begin
      Result := Arr.Elements[AIndex];
      Exit;
    end;
    // Hole or out-of-range: fall back to Obj.GetProperty for prototype lookup
    Result := Obj.GetProperty(IntToStr(AIndex));
    if not Assigned(Result) then
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    Result := Obj.GetProperty(IntToStr(AIndex));
    if not Assigned(Result) then
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

procedure TArrayLikeView.Put(const AIndex: Integer; const AValue: TGocciaValue);
begin
  if Assigned(Arr) then
    Arr.SetElement(AIndex, AValue)
  else
    Obj.AssignProperty(IntToStr(AIndex), AValue);
end;

function TArrayLikeView.HasIndex(const AIndex: Integer): Boolean;
begin
  if Assigned(Arr) then
  begin
    // Fast check: own element present and not a hole
    if (AIndex >= 0) and (AIndex < Arr.Elements.Count) and
       (Arr.Elements[AIndex] <> TGocciaHoleValue.HoleValue) then
    begin
      Result := True;
      Exit;
    end;
    // Hole or out-of-range: fall back to HasProperty for prototype lookup
    Result := Obj.HasProperty(IntToStr(AIndex));
  end
  else
    // ES spec uses [[HasProperty]] which traverses the prototype chain
    Result := Obj.HasProperty(IntToStr(AIndex));
end;

procedure TArrayLikeView.DeleteIndex(const AIndex: Integer);
begin
  if Assigned(Arr) then
  begin
    if (AIndex >= 0) and (AIndex < Arr.Elements.Count) then
      Arr.Elements[AIndex] := TGocciaHoleValue.HoleValue;
  end
  else
    Obj.DeleteProperty(IntToStr(AIndex));
end;

procedure TArrayLikeView.SetLen(const ANewLen: Integer);
begin
  if Assigned(Arr) then
  begin
    while Arr.Elements.Count > ANewLen do
      Arr.Elements.Delete(Arr.Elements.Count - 1);
    while Arr.Elements.Count < ANewLen do
      Arr.Elements.Add(TGocciaHoleValue.HoleValue);
  end
  else
    Obj.AssignProperty(PROP_LENGTH, TGocciaNumberLiteralValue.Create(ANewLen));
end;

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
      ThrowTypeError(SErrorSpeciesNotConstructor,
        SSuggestSpeciesConstructor);

    // Step 8: Return Construct(C, « length »)
    SpeciesClass := TGocciaClassValue(Species);
    LengthArgs := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(ALength)]);
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


function CallCompareFunc(const ACompareFunc: TGocciaFunctionBase; const ACallArgs: TGocciaArgumentsCollection;
  const A, B: TGocciaValue; const AThisValue: TGocciaValue): Double;
var
  CompResult: TGocciaNumberLiteralValue;
begin
  ACallArgs.SetElement(0, A);
  ACallArgs.SetElement(1, B);
  CompResult := ACompareFunc.Call(ACallArgs, AThisValue).ToNumberLiteral;

  if CompResult.IsNaN then
    Result := 0
  else if CompResult.IsInfinity then
    Result := 1
  else if CompResult.IsNegativeInfinity then
    Result := -1
  else
    Result := CompResult.Value;
end;

procedure QuickSortElements(const AElements: TGocciaValueList; const ACompareFunc: TGocciaFunctionBase;
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

constructor TGocciaArrayValue.Create(const AClass: TGocciaClassValue = nil;
  const AElementCapacity: Integer = 0);
begin
  inherited Create(AClass);
  FElements := TGocciaValueList.Create(False);
  if AElementCapacity > 0 then
    FElements.Capacity := AElementCapacity;
  InitializePrototype;
  if not Assigned(AClass) and Assigned(FSharedArrayPrototype) then
    FPrototype := FSharedArrayPrototype;
end;

procedure TGocciaArrayValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
begin
  if Assigned(FSharedArrayPrototype) then Exit;

  FSharedArrayPrototype := TGocciaObjectValue.Create;
  FPrototypeMethodHost := Self;
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddDataProperty(PROP_LENGTH, TGocciaNumberLiteralValue.ZeroValue, [pfWritable]);
      Members.AddMethod(ArrayMap, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayFilter, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayReduce, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayForEach, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArraySome, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayEvery, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayFlat, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayFlatMap, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayJoin, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayToString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayIncludes, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayPush, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayPop, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArraySlice, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayFind, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayFindIndex, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayIndexOf, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayLastIndexOf, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayConcat, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayReverse, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayToReversed, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayToSorted, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayToSpliced, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArraySort, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArraySplice, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayShift, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayUnshift, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayFill, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayAt, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayFindLast, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayFindLastIndex, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayWith, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayCopyWithin, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayValues, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayKeys, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayEntries, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolMethod(
        TGocciaSymbolValue.WellKnownIterator,
        '[Symbol.iterator]',
        ArraySymbolIterator,
        0,
        [pfConfigurable, pfWritable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(FSharedArrayPrototype, FPrototypeMembers);

  if Assigned(TGarbageCollector.Instance) then
  begin
    TGarbageCollector.Instance.PinObject(FSharedArrayPrototype);
    TGarbageCollector.Instance.PinObject(FPrototypeMethodHost);
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
  FSharedArrayPrototype.DefineProperty(PROP_CONSTRUCTOR,
    TGocciaPropertyDescriptorData.Create(AConstructor, [pfConfigurable, pfWritable]));
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
        ThrowRangeError(SErrorInvalidArrayLength,
          SSuggestArrayLengthRange);
      for I := 0 to Trunc(Len) - 1 do
        FElements.Add(TGocciaHoleValue.HoleValue);
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

procedure TGocciaArrayValue.ThrowError(const AMessage: string; const AArgs: array of const; const ASuggestion: string);
begin
  raise TGocciaError.Create(Format(AMessage, AArgs), 0, 0, '', nil, ASuggestion);
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
    FElements.Add(TGocciaHoleValue.HoleValue);

  FElements[AIndex] := AValue;
  Result := True;
end;

function TGocciaArrayValue.ValidateArrayMethodCall(const AMethodName: string; const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue; const ARequiresCallback: Boolean = True): TGocciaValue;
begin
  // ToObject check moved to each caller via TArrayLikeView.Init — no array
  // type guard here so that generic receivers are accepted per ES spec.

  if AArgs.Length < 1 then
    ThrowError(SErrorArrayMethodExpectsCallback, [AMethodName], SSuggestIteratorCallable);

  Result := AArgs.GetElement(0);

  if ARequiresCallback then
  begin
    if not Result.IsCallable then
      ThrowError(SErrorCallbackMustBeFunction, [], SSuggestIteratorCallable);
  end
  else
  begin
    if Result is TGocciaUndefinedLiteralValue then
      ThrowError(SErrorCallbackMustNotBeUndefined, [], SSuggestIteratorCallable);
  end;
end;

function TGocciaArrayValue.IsArrayHole(const AElement: TGocciaValue): Boolean;
begin
  Result := AElement = TGocciaHoleValue.HoleValue;
end;

function TGocciaArrayValue.ArrayMap(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  ResultArray: TGocciaArrayValue;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  CallArgs: TGocciaArrayCallbackArgs;
  I: Integer;
begin
  View.Init(AThisValue);
  Callback := ValidateArrayMethodCall('map', AArgs, AThisValue, True);
  if Assigned(View.Arr) then
    ResultArray := ArraySpeciesCreate(View.Arr, View.Len)
  else
    ResultArray := TGocciaArrayValue.Create(nil, View.Len);

  TypedCallback := nil;
  if Callback is TGocciaFunctionBase then
    TypedCallback := TGocciaFunctionBase(Callback);

  CallArgs := TGocciaArrayCallbackArgs.Create(View.Obj);
  try
    for I := 0 to View.Len - 1 do
    begin
      if not View.HasIndex(I) then
        Continue;

      CallArgs.Element := View.Get(I);
      CallArgs.Index := TGocciaNumberLiteralValue.Create(I);
      if Assigned(TypedCallback) then
        ArrayCreateDataProperty(ResultArray, I, TypedCallback.Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue))
      else
        ArrayCreateDataProperty(ResultArray, I, InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue));
    end;
  finally
    CallArgs.Free;
  end;

  // Ensure result has correct length (preserve trailing holes)
  while ResultArray.Elements.Count < View.Len do
    ResultArray.Elements.Add(TGocciaHoleValue.HoleValue);

  Result := ResultArray;
end;

// ES2026 §23.1.3.8 Array.prototype.filter(callbackfn [, thisArg])
function TGocciaArrayValue.ArrayFilter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  ResultArray: TGocciaArrayValue;
  CallArgs: TGocciaArrayCallbackArgs;
  PredicateResult, Element: TGocciaValue;
  I: Integer;
begin
  View.Init(AThisValue);
  Callback := ValidateArrayMethodCall('filter', AArgs, AThisValue, True);
  if Assigned(View.Arr) then
    ResultArray := ArraySpeciesCreate(View.Arr, 0)
  else
    ResultArray := TGocciaArrayValue.Create;

  TypedCallback := nil;
  if Callback is TGocciaFunctionBase then
    TypedCallback := TGocciaFunctionBase(Callback);

  CallArgs := TGocciaArrayCallbackArgs.Create(View.Obj);
  try
    for I := 0 to View.Len - 1 do
    begin
      if not View.HasIndex(I) then
        Continue;

      Element := View.Get(I);
      CallArgs.Element := Element;
      CallArgs.Index := TGocciaNumberLiteralValue.Create(I);
      if Assigned(TypedCallback) then
        PredicateResult := TypedCallback.Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue)
      else
        PredicateResult := InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);

      if PredicateResult.ToBooleanLiteral.Value then
        ResultArray.Elements.Add(Element);
    end;
  finally
    CallArgs.Free;
  end;

  Result := ResultArray;
end;

// ES2026 §23.1.3.22 Array.prototype.reduce(callbackfn [, initialValue])
function TGocciaArrayValue.ArrayReduce(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  Accumulator: TGocciaValue;
  CallArgs: TGocciaReduceCallbackArgs;
  I, StartIndex: Integer;
begin
  View.Init(AThisValue);
  Callback := ValidateArrayMethodCall('reduce', AArgs, AThisValue, True);

  if AArgs.Length >= 2 then
  begin
    Accumulator := AArgs.GetElement(1);
    StartIndex := 0;
  end
  else
  begin
    // ES2026 §23.1.3.22 steps 6-8: scan for first present element
    StartIndex := -1;
    for I := 0 to View.Len - 1 do
    begin
      if View.HasIndex(I) then
      begin
        Accumulator := View.Get(I);
        StartIndex := I + 1;
        Break;
      end;
    end;
    if StartIndex < 0 then
      ThrowTypeError(SErrorReduceEmptyArray,
        SSuggestReduceInitialValue);
  end;

  TypedCallback := nil;
  if Callback is TGocciaFunctionBase then
    TypedCallback := TGocciaFunctionBase(Callback);

  CallArgs := TGocciaReduceCallbackArgs.Create(View.Obj);
  try
    for I := StartIndex to View.Len - 1 do
    begin
      if not View.HasIndex(I) then
        Continue;

      CallArgs.Accumulator := Accumulator;
      CallArgs.Element := View.Get(I);
      CallArgs.Index := TGocciaNumberLiteralValue.Create(I);
      if Assigned(TypedCallback) then
        Accumulator := TypedCallback.Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue)
      else
        Accumulator := InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    end;
  finally
    CallArgs.Free;
  end;

  Result := Accumulator;
end;

// ES2026 §23.1.3.12 Array.prototype.forEach(callbackfn [, thisArg])
function TGocciaArrayValue.ArrayForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  CallArgs: TGocciaArrayCallbackArgs;
  I: Integer;
begin
  View.Init(AThisValue);
  Callback := ValidateArrayMethodCall('forEach', AArgs, AThisValue, True);

  TypedCallback := nil;
  if Callback is TGocciaFunctionBase then
    TypedCallback := TGocciaFunctionBase(Callback);

  CallArgs := TGocciaArrayCallbackArgs.Create(View.Obj);
  try
    for I := 0 to View.Len - 1 do
    begin
      if not View.HasIndex(I) then
        Continue;

      CallArgs.Element := View.Get(I);
      CallArgs.Index := TGocciaNumberLiteralValue.Create(I);
      if Assigned(TypedCallback) then
        TypedCallback.Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue)
      else
        InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    end;
  finally
    CallArgs.Free;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §23.1.3.16 Array.prototype.join(separator)
function TGocciaArrayValue.ArrayJoin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Separator: string;
  I: Integer;
  ResultString: string;
  Element: TGocciaValue;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  // Step 3: If separator is undefined, let sep be ","
  // Step 4: Else let sep be ToString(separator)
  if (AArgs.Length < 1) or (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
    Separator := ','
  else
    Separator := AArgs.GetElement(0).ToStringLiteral.Value;

  // Steps 5-8: Build result string, treating undefined/null elements as empty string
  ResultString := '';
  for I := 0 to View.Len - 1 do
  begin
    if I > 0 then
      ResultString := ResultString + Separator;
    // Step 7b: If element is undefined or null, let next be ""
    Element := View.Get(I);
    if (Element is TGocciaUndefinedLiteralValue) or
       (Element is TGocciaNullLiteralValue) then
      Continue
    else
      // ES2026 §23.1.3.16 step 7c: Let next be ? ToString(element)
      ResultString := ResultString + ToECMAString(Element).Value;
  end;

  // Step 9: Return R
  Result := TGocciaStringLiteralValue.Create(ResultString);
end;

// ES2026 §23.1.3.35 Array.prototype.toString()
function TGocciaArrayValue.ArrayToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  EmptyArgs: TGocciaArgumentsCollection;
begin
  EmptyArgs := TGocciaArgumentsCollection.Create([]);
  try
    Result := ArrayJoin(EmptyArgs, AThisValue);
  finally
    EmptyArgs.Free;
  end;
end;

// ES2026 §23.1.3.14 Array.prototype.includes(searchElement [, fromIndex])
function TGocciaArrayValue.ArrayIncludes(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  SearchValue, Element: TGocciaValue;
  FromIndex, I: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  if AArgs.Length < 1 then
    SearchValue := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    SearchValue := AArgs.GetElement(0);

  // Steps 4-5: Let n be ToIntegerOrInfinity(fromIndex), compute k
  FromIndex := ToIntegerFromArgs(AArgs, 1);
  if FromIndex < 0 then
    FromIndex := View.Len + FromIndex;
  if FromIndex < 0 then
    FromIndex := 0;

  // Steps 6-8: Search using SameValueZero comparison
  for I := FromIndex to View.Len - 1 do
  begin
    Element := View.Get(I);
    if IsSameValueZero(Element, SearchValue) then
    begin
      Result := TGocciaBooleanLiteralValue.TrueValue;
      Exit;
    end;
  end;

  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §23.1.3.28 Array.prototype.some(callbackfn [, thisArg])
function TGocciaArrayValue.ArraySome(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  CallArgs: TGocciaArrayCallbackArgs;
  I: Integer;
  SomeResult: TGocciaValue;
begin
  View.Init(AThisValue);
  Callback := ValidateArrayMethodCall('some', AArgs, AThisValue, True);

  TypedCallback := nil;
  if Callback is TGocciaFunctionBase then
    TypedCallback := TGocciaFunctionBase(Callback);

  CallArgs := TGocciaArrayCallbackArgs.Create(View.Obj);
  try
    for I := 0 to View.Len - 1 do
    begin
      if not View.HasIndex(I) then
        Continue;

      CallArgs.Element := View.Get(I);
      CallArgs.Index := TGocciaNumberLiteralValue.Create(I);
      if Assigned(TypedCallback) then
        SomeResult := TypedCallback.Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue)
      else
        SomeResult := InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
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

// ES2026 §23.1.3.6 Array.prototype.every(callbackfn [, thisArg])
function TGocciaArrayValue.ArrayEvery(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  CallArgs: TGocciaArrayCallbackArgs;
  I: Integer;
  EveryResult: TGocciaValue;
begin
  View.Init(AThisValue);
  Callback := ValidateArrayMethodCall('every', AArgs, AThisValue, True);

  TypedCallback := nil;
  if Callback is TGocciaFunctionBase then
    TypedCallback := TGocciaFunctionBase(Callback);

  CallArgs := TGocciaArrayCallbackArgs.Create(View.Obj);
  try
    for I := 0 to View.Len - 1 do
    begin
      if not View.HasIndex(I) then
        Continue;

      CallArgs.Element := View.Get(I);
      CallArgs.Index := TGocciaNumberLiteralValue.Create(I);
      if Assigned(TypedCallback) then
        EveryResult := TypedCallback.Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue)
      else
        EveryResult := InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
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

// ES2026 §23.1.3.11.1 FlattenIntoArray(target, source, ..., depth)
procedure TGocciaArrayValue.FlattenInto(const ATarget: TGocciaArrayValue; const ADepth: Integer);
var
  View: TArrayLikeView;
  I: Integer;
  Element: TGocciaValue;
begin
  // Use View-based iteration for prototype-aware access
  View.Init(Self);
  for I := 0 to View.Len - 1 do
  begin
    if not View.HasIndex(I) then
      Continue;

    Element := View.Get(I);
    // Step 3c-v: If depth > 0 and IsConcatSpreadable(element), recurse
    if (Element is TGocciaArrayValue) and (ADepth > 0) then
      TGocciaArrayValue(Element).FlattenInto(ATarget, ADepth - 1)
    else
      // Step 3c-v-2: Else, CreateDataPropertyOrThrow(target, targetIndex, element)
      ATarget.Elements.Add(Element);
  end;
end;

// ES2026 §23.1.3.11 Array.prototype.flat([depth])
function TGocciaArrayValue.ArrayFlat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  ResultArray: TGocciaArrayValue;
  DepthNum: TGocciaNumberLiteralValue;
  Depth, I: Integer;
  Element: TGocciaValue;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);
  // Step 3: Let depthNum be 1 (default)
  Depth := 1;

  // Step 4: If depth is not undefined, let depthNum be ToIntegerOrInfinity(depth)
  if AArgs.Length > 0 then
  begin
    if not (AArgs.GetElement(0) is TGocciaNumberLiteralValue) then
      ThrowError(SErrorArrayFlatExpectsNumber, [], SSuggestArrayThisType);

    DepthNum := AArgs.GetElement(0).ToNumberLiteral;
    if DepthNum.IsNaN then
      Depth := 0
    else if DepthNum.IsInfinity then
      Depth := MaxInt
    else if DepthNum.IsNegativeInfinity then
      Depth := 0
    else if DepthNum.Value > MaxInt then
      Depth := MaxInt
    else if DepthNum.Value < 0 then
      Depth := 0
    else
      Depth := Trunc(DepthNum.Value);
  end;

  // Step 5: Let A be ArraySpeciesCreate(O, 0)
  if Assigned(View.Arr) then
    ResultArray := ArraySpeciesCreate(View.Arr, 0)
  else
    ResultArray := TGocciaArrayValue.Create;

  // Step 6: FlattenIntoArray via View for prototype-aware semantics
  for I := 0 to View.Len - 1 do
  begin
    if not View.HasIndex(I) then
      Continue;
    Element := View.Get(I);
    if (Element is TGocciaArrayValue) and (Depth > 0) then
      TGocciaArrayValue(Element).FlattenInto(ResultArray, Depth - 1)
    else
      ResultArray.Elements.Add(Element);
  end;
  // Step 7: Return A
  Result := ResultArray;
end;

// ES2026 §23.1.3.10 Array.prototype.flatMap(mapperFunction [, thisArg])
function TGocciaArrayValue.ArrayFlatMap(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View, MappedView: TArrayLikeView;
  ResultArray: TGocciaArrayValue;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  CallArgs: TGocciaArrayCallbackArgs;
  I, J: Integer;
  MappedValue: TGocciaValue;
begin
  View.Init(AThisValue);
  Callback := ValidateArrayMethodCall('flatMap', AArgs, AThisValue, True);
  if Assigned(View.Arr) then
    ResultArray := ArraySpeciesCreate(View.Arr, 0)
  else
    ResultArray := TGocciaArrayValue.Create;

  TypedCallback := nil;
  if Callback is TGocciaFunctionBase then
    TypedCallback := TGocciaFunctionBase(Callback);

  CallArgs := TGocciaArrayCallbackArgs.Create(View.Obj);
  try
    for I := 0 to View.Len - 1 do
    begin
      if not View.HasIndex(I) then
        Continue;

      CallArgs.Element := View.Get(I);
      CallArgs.Index := TGocciaNumberLiteralValue.Create(I);
      if Assigned(TypedCallback) then
        MappedValue := TypedCallback.Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue)
      else
        MappedValue := InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);

      if MappedValue is TGocciaArrayValue then
      begin
        // Use View-based iteration for prototype-aware access
        MappedView.Init(MappedValue);
        for J := 0 to MappedView.Len - 1 do
        begin
          if MappedView.HasIndex(J) then
            ResultArray.Elements.Add(MappedView.Get(J));
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

// ES2026 §23.1.3.21 Array.prototype.push(...items)
function TGocciaArrayValue.ArrayPush(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  NewLen, I: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  // Fast path: native array
  if Assigned(View.Arr) then
  begin
    for I := 0 to AArgs.Length - 1 do
      View.Arr.Elements.Add(AArgs.GetElement(I));
    Result := TGocciaNumberLiteralValue.Create(View.Arr.Elements.Count);
    Exit;
  end;

  // Generic path: Set(O, ToString(len), E), len = len + 1
  NewLen := View.Len;
  for I := 0 to AArgs.Length - 1 do
  begin
    View.Put(NewLen, AArgs.GetElement(I));
    Inc(NewLen);
  end;
  View.SetLen(NewLen);
  Result := TGocciaNumberLiteralValue.Create(NewLen);
end;

// ES2026 §23.1.3.20 Array.prototype.pop()
function TGocciaArrayValue.ArrayPop(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  // Step 3: If len = 0, set length to 0 and return undefined
  if View.Len = 0 then
  begin
    View.SetLen(0);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  // Step 5: Let element be Get(O, ToString(newLen))
  Result := View.Get(View.Len - 1);

  // Fast path: native array
  if Assigned(View.Arr) then
    View.Arr.Elements.Delete(View.Arr.Elements.Count - 1)
  else
  begin
    // Step 6: DeletePropertyOrThrow(O, ToString(newLen))
    View.DeleteIndex(View.Len - 1);
    // Step 7: Set(O, "length", newLen)
    View.SetLen(View.Len - 1);
  end;
end;

// ES2026 §23.1.3.26 Array.prototype.slice(start, end)
function TGocciaArrayValue.ArraySlice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  ResultArray: TGocciaArrayValue;
  StartIndex, EndIndex: Integer;
  I, N: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  // Step 3: Let relativeStart be ToIntegerOrInfinity(start)
  StartIndex := ToIntegerFromArgs(AArgs);

  // Step 4: If relativeStart < 0, let k be max(len + relativeStart, 0); else min(relativeStart, len)
  StartIndex := NormalizeRelativeIndex(StartIndex, View.Len);

  // Step 5: If end is undefined, let relativeEnd be len; else ToIntegerOrInfinity(end)
  EndIndex := ToIntegerFromArgs(AArgs, 1, View.Len);

  // Step 6: If relativeEnd < 0, let final be max(len + relativeEnd, 0); else min(relativeEnd, len)
  EndIndex := NormalizeRelativeIndex(EndIndex, View.Len);

  // Step 7: Let count be max(final - k, 0)
  // Step 8: Let A be ArraySpeciesCreate(O, count)
  if Assigned(View.Arr) then
    ResultArray := ArraySpeciesCreate(View.Arr, Max(EndIndex - StartIndex, 0))
  else
    ResultArray := TGocciaArrayValue.Create(nil, Max(EndIndex - StartIndex, 0));

  // Step 9: Let n be 0; repeat while k < final
  N := 0;
  for I := StartIndex to EndIndex - 1 do
  begin
    // Step 9b-c: only create property when source index is present (preserve holes)
    if View.HasIndex(I) then
      ArrayCreateDataProperty(ResultArray, N, View.Get(I));
    // Step 9e: n increments unconditionally per ES spec
    Inc(N);
  end;

  // Ensure result has correct length (preserve trailing holes)
  while ResultArray.Elements.Count < N do
    ResultArray.Elements.Add(TGocciaHoleValue.HoleValue);

  // Step 11: Return A
  Result := ResultArray;
end;

// ES2026 §23.1.3.9 Array.prototype.find(predicate [, thisArg])
function TGocciaArrayValue.ArrayFind(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  CallArgs: TGocciaArrayCallbackArgs;
  I: Integer;
  Element, CallResult: TGocciaValue;
begin
  View.Init(AThisValue);
  Callback := ValidateArrayMethodCall('find', AArgs, AThisValue, True);

  TypedCallback := nil;
  if Callback is TGocciaFunctionBase then
    TypedCallback := TGocciaFunctionBase(Callback);

  CallArgs := TGocciaArrayCallbackArgs.Create(View.Obj);
  try
    for I := 0 to View.Len - 1 do
    begin
      Element := View.Get(I);

      CallArgs.Element := Element;
      CallArgs.Index := TGocciaNumberLiteralValue.Create(I);
      if Assigned(TypedCallback) then
        CallResult := TypedCallback.Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue)
      else
        CallResult := InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
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

// ES2026 §23.1.3.10 Array.prototype.findIndex(predicate [, thisArg])
function TGocciaArrayValue.ArrayFindIndex(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  CallArgs: TGocciaArrayCallbackArgs;
  I: Integer;
  Element, CallResult: TGocciaValue;
begin
  View.Init(AThisValue);
  Callback := ValidateArrayMethodCall('findIndex', AArgs, AThisValue, True);

  TypedCallback := nil;
  if Callback is TGocciaFunctionBase then
    TypedCallback := TGocciaFunctionBase(Callback);

  CallArgs := TGocciaArrayCallbackArgs.Create(View.Obj);
  try
    for I := 0 to View.Len - 1 do
    begin
      Element := View.Get(I);

      CallArgs.Element := Element;
      CallArgs.Index := TGocciaNumberLiteralValue.Create(I);
      if Assigned(TypedCallback) then
        CallResult := TypedCallback.Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue)
      else
        CallResult := InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
      if CallResult.ToBooleanLiteral.Value then
      begin
        Result := TGocciaNumberLiteralValue.Create(I);
        Exit;
      end;
    end;
  finally
    CallArgs.Free;
  end;

  Result := TGocciaNumberLiteralValue.Create(-1);
end;

// ES2026 §23.1.3.11 Array.prototype.findLast(predicate [, thisArg])
function TGocciaArrayValue.ArrayFindLast(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  CallArgs: TGocciaArrayCallbackArgs;
  I: Integer;
  Element, CallResult: TGocciaValue;
begin
  View.Init(AThisValue);
  Callback := ValidateArrayMethodCall('findLast', AArgs, AThisValue, True);

  TypedCallback := nil;
  if Callback is TGocciaFunctionBase then
    TypedCallback := TGocciaFunctionBase(Callback);

  CallArgs := TGocciaArrayCallbackArgs.Create(View.Obj);
  try
    for I := View.Len - 1 downto 0 do
    begin
      Element := View.Get(I);

      CallArgs.Element := Element;
      CallArgs.Index := TGocciaNumberLiteralValue.Create(I);
      if Assigned(TypedCallback) then
        CallResult := TypedCallback.Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue)
      else
        CallResult := InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
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

// ES2026 §23.1.3.12 Array.prototype.findLastIndex(predicate [, thisArg])
function TGocciaArrayValue.ArrayFindLastIndex(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  CallArgs: TGocciaArrayCallbackArgs;
  I: Integer;
  Element, CallResult: TGocciaValue;
begin
  View.Init(AThisValue);
  Callback := ValidateArrayMethodCall('findLastIndex', AArgs, AThisValue, True);

  TypedCallback := nil;
  if Callback is TGocciaFunctionBase then
    TypedCallback := TGocciaFunctionBase(Callback);

  CallArgs := TGocciaArrayCallbackArgs.Create(View.Obj);
  try
    for I := View.Len - 1 downto 0 do
    begin
      Element := View.Get(I);

      CallArgs.Element := Element;
      CallArgs.Index := TGocciaNumberLiteralValue.Create(I);
      if Assigned(TypedCallback) then
        CallResult := TypedCallback.Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue)
      else
        CallResult := InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
      if CallResult.ToBooleanLiteral.Value then
      begin
        Result := TGocciaNumberLiteralValue.Create(I);
        Exit;
      end;
    end;
  finally
    CallArgs.Free;
  end;

  Result := TGocciaNumberLiteralValue.Create(-1);
end;

// ES2026 §23.1.3.38 Array.prototype.with(index, value)
function TGocciaArrayValue.ArrayWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  ResultArray: TGocciaArrayValue;
  Index, ActualIndex, I: Integer;
  NewValue: TGocciaValue;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  if AArgs.Length < 2 then
    ThrowError(SErrorArrayWithRequiresArgs, [], SSuggestArrayThisType);

  // Step 3: Let relativeIndex be ToIntegerOrInfinity(index)
  Index := ToIntegerFromArgs(AArgs);
  // Step 5: Let value be args[1]
  NewValue := AArgs.GetElement(1);

  // Step 4: If relativeIndex >= 0, let actualIndex be relativeIndex; else len + relativeIndex
  if Index < 0 then
    ActualIndex := View.Len + Index
  else
    ActualIndex := Index;

  // Step 6: If actualIndex >= len or actualIndex < 0, throw RangeError
  if (ActualIndex < 0) or (ActualIndex >= View.Len) then
    ThrowRangeError(SErrorInvalidArrayWithIndex,
      Format('index must be between %d and %d', [-View.Len, View.Len - 1]));

  // Step 7: Let A be ArrayCreate(len)
  ResultArray := TGocciaArrayValue.Create;
  // Step 8: Let k be 0; repeat while k < len
  for I := 0 to View.Len - 1 do
  begin
    // Step 8b: If k = actualIndex, let fromValue be value; else Get(O, Pk)
    if I = ActualIndex then
      ResultArray.Elements.Add(NewValue)
    else
      ResultArray.Elements.Add(View.Get(I));
  end;

  // Step 9: Return A
  Result := ResultArray;
end;

// ES2026 §23.1.3.4 Array.prototype.copyWithin(target, start [, end])
function TGocciaArrayValue.ArrayCopyWithin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Target, Start, EndIdx, Count, I: Integer;
  Temp: array of TGocciaValue;
  Present: array of Boolean;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  if AArgs.Length < 1 then
    ThrowError(SErrorArrayCopyWithinRequiresTarget, [], SSuggestArrayThisType);

  // Step 3: Let relativeTarget be ToIntegerOrInfinity(target)
  Target := ToIntegerFromArgs(AArgs);
  // Step 4: If relativeTarget < 0, let to be max(len + relativeTarget, 0); else min(relativeTarget, len)
  Target := NormalizeRelativeIndex(Target, View.Len);

  // Step 5: Let relativeStart be ToIntegerOrInfinity(start)
  Start := ToIntegerFromArgs(AArgs, 1);
  // Step 6: If relativeStart < 0, let from be max(len + relativeStart, 0); else min(relativeStart, len)
  Start := NormalizeRelativeIndex(Start, View.Len);

  // Step 7: If end is undefined, let relativeEnd be len; else ToIntegerOrInfinity(end)
  EndIdx := ToIntegerFromArgs(AArgs, 2, View.Len);
  // Step 8: If relativeEnd < 0, let final be max(len + relativeEnd, 0); else min(relativeEnd, len)
  EndIdx := NormalizeRelativeIndex(EndIdx, View.Len);

  // Step 9: Let count be min(final - from, len - to)
  Count := Min(EndIdx - Start, View.Len - Target);
  if Count <= 0 then
  begin
    Result := View.Obj;
    Exit;
  end;

  // Steps 10-12: Copy elements (using temp buffer for overlap safety)
  // Preserve sparsity per ES spec: delete destination when source is absent.
  SetLength(Temp, Count);
  SetLength(Present, Count);
  for I := 0 to Count - 1 do
  begin
    Present[I] := View.HasIndex(Start + I);
    if Present[I] then
      Temp[I] := View.Get(Start + I);
  end;
  for I := 0 to Count - 1 do
  begin
    if Present[I] then
      View.Put(Target + I, Temp[I])
    else
      View.DeleteIndex(Target + I);
  end;

  // Step 13: Return O
  Result := View.Obj;
end;

// ES2026 §23.1.3.15 Array.prototype.indexOf(searchElement [, fromIndex])
function TGocciaArrayValue.ArrayIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  SearchValue: TGocciaValue;
  I, FromIndex: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

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
    FromIndex := View.Len + FromIndex;
  if FromIndex < 0 then
    FromIndex := 0;

  // Step 7: Repeat, while k < len
  for I := FromIndex to View.Len - 1 do
  begin
    // Step 7a: Let kPresent be HasProperty(O, Pk)
    if not View.HasIndex(I) then
      Continue;
    // Step 7b: If kPresent, let elementK be Get(O, Pk)
    // Step 7c: If IsStrictlyEqual(searchElement, elementK) is true, return k
    if IsStrictEqual(View.Get(I), SearchValue) then
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
  View: TArrayLikeView;
  SearchValue: TGocciaValue;
  I, FromIndex: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  // Step 3: If len = 0, return -1𝔽
  if AArgs.Length < 1 then
  begin
    Result := TGocciaNumberLiteralValue.Create(-1);
    Exit;
  end;

  SearchValue := AArgs.GetElement(0);

  // Step 4: If fromIndex is present, let n be ToIntegerOrInfinity(fromIndex); else let n be len - 1
  FromIndex := ToIntegerFromArgs(AArgs, 1, View.Len - 1);
  // Step 6: If n < 0, let k be len + n
  if FromIndex < 0 then
    FromIndex := View.Len + FromIndex;

  if FromIndex >= View.Len then
    FromIndex := View.Len - 1;

  // Step 7: Repeat, while k >= 0
  for I := FromIndex downto 0 do
  begin
    // Step 7a: Let kPresent be HasProperty(O, Pk)
    if not View.HasIndex(I) then
      Continue;
    // Step 7c: If IsStrictlyEqual(searchElement, elementK) is true, return k
    if IsStrictEqual(View.Get(I), SearchValue) then
    begin
      Result := TGocciaNumberLiteralValue.Create(I);
      Exit;
    end;
  end;

  // Step 8: Return -1𝔽
  Result := TGocciaNumberLiteralValue.Create(-1);
end;

// ES2026 §7.2.15 IsConcatSpreadable(O)
function IsConcatSpreadable(const AValue: TGocciaValue): Boolean;
var
  Spreadable: TGocciaValue;
begin
  if not (AValue is TGocciaObjectValue) then
  begin
    Result := False;
    Exit;
  end;
  // Step 1: If Type(O) is Object, let spreadable be Get(O, @@isConcatSpreadable)
  Spreadable := TGocciaObjectValue(AValue).GetSymbolProperty(TGocciaSymbolValue.WellKnownIsConcatSpreadable);
  if Assigned(Spreadable) and not (Spreadable is TGocciaUndefinedLiteralValue) then
  begin
    Result := Spreadable.ToBooleanLiteral.Value;
    Exit;
  end;
  // Step 2: Return IsArray(O)
  Result := AValue is TGocciaArrayValue;
end;

// Spread a concat-spreadable value into the result array using array-like
// iteration so that non-array spreadable objects (e.g., objects with
// Symbol.isConcatSpreadable) are handled correctly.
procedure SpreadIntoConcat(const ASource: TGocciaValue; const AResult: TGocciaArrayValue; var N: Integer);
var
  SrcView: TArrayLikeView;
  J: Integer;
begin
  SrcView.Init(ASource);
  for J := 0 to SrcView.Len - 1 do
  begin
    if SrcView.HasIndex(J) then
      ArrayCreateDataProperty(AResult, N, SrcView.Get(J));
    Inc(N);
  end;
  // Ensure result reflects trailing holes from this spread
  while AResult.Elements.Count < N do
    AResult.Elements.Add(TGocciaHoleValue.HoleValue);
end;

// ES2026 §23.1.3.1 Array.prototype.concat(...arguments)
function TGocciaArrayValue.ArrayConcat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  ResultArray: TGocciaArrayValue;
  I, N: Integer;
  Arg: TGocciaValue;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  // Step 2: Let A be ArraySpeciesCreate(O, 0)
  if Assigned(View.Arr) then
    ResultArray := ArraySpeciesCreate(View.Arr, 0)
  else
    ResultArray := TGocciaArrayValue.Create;

  // Step 3: Let n be 0
  N := 0;

  // Step 4: Prepend O to items — check IsConcatSpreadable
  if IsConcatSpreadable(View.Obj) then
    SpreadIntoConcat(View.Obj, ResultArray, N)
  else
  begin
    ArrayCreateDataProperty(ResultArray, N, View.Obj);
    Inc(N);
  end;

  // Step 5: For each element E of items
  for I := 0 to AArgs.Length - 1 do
  begin
    Arg := AArgs.GetElement(I);
    // Step 5b: If IsConcatSpreadable(E) is true, spread via array-like iteration
    if IsConcatSpreadable(Arg) then
      SpreadIntoConcat(Arg, ResultArray, N)
    // Step 5c: Else, CreateDataPropertyOrThrow(A, ToString(n), E)
    else
    begin
      ArrayCreateDataProperty(ResultArray, N, Arg);
      Inc(N);
    end;
  end;

  // Step 6: Return A
  Result := ResultArray;
end;

// ES2026 §23.1.3.23 Array.prototype.reverse()
function TGocciaArrayValue.ArrayReverse(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  I, J: Integer;
  LowerExists, UpperExists: Boolean;
  Lower, Upper: TGocciaValue;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);
  // Step 3: Let middle be floor(len / 2), let lower be 0
  I := 0;
  J := View.Len - 1;
  // Step 4: Repeat, while lower ≠ middle
  while I < J do
  begin
    // ES2026 §23.1.3.23 steps 4a-i: preserve sparsity via HasProperty/Delete
    LowerExists := View.HasIndex(I);
    UpperExists := View.HasIndex(J);
    if LowerExists then
      Lower := View.Get(I);
    if UpperExists then
      Upper := View.Get(J);

    if LowerExists and UpperExists then
    begin
      View.Put(I, Upper);
      View.Put(J, Lower);
    end
    else if UpperExists then
    begin
      View.Put(I, Upper);
      View.DeleteIndex(J);
    end
    else if LowerExists then
    begin
      View.Put(J, Lower);
      View.DeleteIndex(I);
    end;
    // else: both absent — nothing to do
    Inc(I);
    Dec(J);
  end;

  // Step 5: Return O
  Result := View.Obj;
end;

// ES2026 §23.1.3.33 Array.prototype.toReversed()
function TGocciaArrayValue.ArrayToReversed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  ResultArray: TGocciaArrayValue;
  I: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);
  // Step 3: Let A be ArrayCreate(len)
  ResultArray := TGocciaArrayValue.Create;

  // Step 4: Let k be 0; repeat while k < len
  for I := View.Len - 1 downto 0 do
    ResultArray.Elements.Add(View.Get(I));

  // Step 5: Return A
  Result := ResultArray;
end;

// ES2026 §23.1.3.34 Array.prototype.toSorted(comparefn)
function TGocciaArrayValue.ArrayToSorted(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  ResultArray: TGocciaArrayValue;
  CustomSortFunction: TGocciaValue;
  I: Integer;
  CallArgs: TGocciaArgumentsCollection;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);
  // Step 4: Let A be ArrayCreate(len)
  ResultArray := TGocciaArrayValue.Create;

  // Step 5: Copy elements from O into A
  for I := 0 to View.Len - 1 do
    ResultArray.Elements.Add(View.Get(I));

  if AArgs.Length > 0 then
  begin
    CustomSortFunction := AArgs.GetElement(0);

    if not CustomSortFunction.IsCallable then
      ThrowError(SErrorCustomSortMustBeFunction, [], SSuggestCallbackRequired);

    CallArgs := TGocciaArgumentsCollection.Create([nil, nil]);
    try
      QuickSortElements(ResultArray.Elements, TGocciaFunctionBase(CustomSortFunction), CallArgs, AThisValue, 0, ResultArray.Elements.Count - 1);
    finally
      CallArgs.Free;
    end;
  end else
    ResultArray.Elements.Sort(TComparer<TGocciaValue>.Construct(DefaultCompare));

  // Step 7: Return A
  Result := ResultArray;
end;

// ES2026 §23.1.3.35 Array.prototype.toSpliced(start, skipCount, ...items)
function TGocciaArrayValue.ArrayToSpliced(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  ResultArray: TGocciaArrayValue;
  StartIndex, DeleteCount: Integer;
  I: Integer;
  ActualStartIndex: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  // Step 3: Let relativeStart be ToIntegerOrInfinity(start)
  StartIndex := ToIntegerFromArgs(AArgs);

  // Step 4: If relativeStart < 0, let actualStart be max(len + relativeStart, 0); else min(relativeStart, len)
  ActualStartIndex := NormalizeRelativeIndex(StartIndex, View.Len);

  // Step 5: If skipCount is not present, let actualSkipCount be len - actualStart
  // Step 6: Else let actualSkipCount be max(ToIntegerOrInfinity(skipCount), 0)
  DeleteCount := ToIntegerFromArgs(AArgs, 1, View.Len - ActualStartIndex);
  if DeleteCount < 0 then
    DeleteCount := 0
  else if ActualStartIndex + DeleteCount > View.Len then
    DeleteCount := View.Len - ActualStartIndex;

  // Step 9: Let A be ArrayCreate(newLen)
  ResultArray := TGocciaArrayValue.Create;

  // Step 11: Copy elements before actualStart
  for I := 0 to ActualStartIndex - 1 do
    ResultArray.Elements.Add(View.Get(I));

  // Step 12: Insert new items
  for I := 2 to AArgs.Length - 1 do
    ResultArray.Elements.Add(AArgs.GetElement(I));

  // Step 13: Copy elements after actualStart + actualSkipCount
  for I := ActualStartIndex + DeleteCount to View.Len - 1 do
    ResultArray.Elements.Add(View.Get(I));

  // Step 14: Return A
  Result := ResultArray;
end;

function TGocciaArrayValue.ToStringLiteral: TGocciaStringLiteralValue;
var
  I: Integer;
  SB: TStringBuffer;
begin
  if FElements.Count = 0 then
  begin
    Result := TGocciaStringLiteralValue.Create('');
    Exit;
  end;

  SB := TStringBuffer.Create;
  for I := 0 to FElements.Count - 1 do
  begin
    if I > 0 then
      SB.AppendChar(',');
    if not IsArrayHole(FElements[I]) then
      SB.Append(FElements[I].ToStringLiteral.Value);
  end;
  Result := TGocciaStringLiteralValue.Create(SB.ToString);
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
    if IsArrayHole(FElements[I]) then
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
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaArrayValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
var
  Index: Integer;
begin
  if AName = PROP_LENGTH then
  begin
    Result := TGocciaNumberLiteralValue.Create(FElements.Count);
    Exit;
  end;

  // Check if property name is a numeric index
  if TryStrToInt(AName, Index) then
  begin
    if (Index >= 0) and (Index < FElements.Count) and
       not IsArrayHole(FElements[Index]) then
    begin
      Result := FElements[Index];
      Exit;
    end;
    // Hole or out-of-range: fall through to prototype per ES [[Get]]
    Result := inherited GetPropertyWithContext(AName, AThisContext);
    Exit;
  end
  else
  begin
    // Fall back to regular object property lookup
    Result := inherited GetPropertyWithContext(AName, AThisContext);
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
        FElements.Add(TGocciaHoleValue.HoleValue);

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

function TGocciaArrayValue.HasOwnProperty(const AName: string): Boolean;
var
  Index: Integer;
begin
  if TryStrToInt(AName, Index) and (Index >= 0) and (Index < FElements.Count) and
     not IsArrayHole(FElements[Index]) then
    Result := True
  else if AName = PROP_LENGTH then
    Result := True
  else
    Result := inherited HasOwnProperty(AName);
end;

function TGocciaArrayValue.GetOwnPropertyDescriptor(const AName: string): TGocciaPropertyDescriptor;
var
  Index: Integer;
begin
  if TryStrToInt(AName, Index) and (Index >= 0) and (Index < FElements.Count) and
     not IsArrayHole(FElements[Index]) then
    Result := TGocciaPropertyDescriptorData.Create(FElements[Index], [pfEnumerable, pfConfigurable, pfWritable])
  else if AName = PROP_LENGTH then
    Result := TGocciaPropertyDescriptorData.Create(TGocciaNumberLiteralValue.Create(FElements.Count), [pfWritable])
  else
    Result := inherited GetOwnPropertyDescriptor(AName);
end;

// ES2026 §10.4.2.1 ArrayDefineOwnProperty(A, P, Desc)
procedure TGocciaArrayValue.DefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor);
begin
  if not TryDefineProperty(AName, ADescriptor) then
    ThrowError(SErrorCannotRedefineNonConfigurable, [AName], SSuggestCannotDeleteNonConfigurable);
end;

// ES2026 §10.4.2.1 ArrayDefineOwnProperty — boolean variant
function TGocciaArrayValue.TryDefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor): Boolean;
var
  Index, NewLen, I: Integer;
  RawLen: Double;
begin
  if AName = PROP_LENGTH then
  begin
    // §10.4.2.4 ArraySetLength(A, Desc)
    if ADescriptor is TGocciaPropertyDescriptorData then
    begin
      RawLen := TGocciaPropertyDescriptorData(ADescriptor).Value.ToNumberLiteral.Value;
      NewLen := Trunc(RawLen);
      if (RawLen <> NewLen) or (NewLen < 0) or (RawLen > 4294967295) then
      begin
        ADescriptor.Free;
        Exit(False);
      end;
      // Truncate elements
      if NewLen < FElements.Count then
        for I := FElements.Count - 1 downto NewLen do
          FElements.Delete(I);
      // Extend elements
      while FElements.Count < NewLen do
        FElements.Add(TGocciaHoleValue.HoleValue);
    end;
    ADescriptor.Free;
    Result := True;
    Exit;
  end;

  // Numeric index — update FElements directly
  if TryStrToInt(AName, Index) and (Index >= 0) then
  begin
    if ADescriptor is TGocciaPropertyDescriptorData then
    begin
      // Extend array if needed
      while FElements.Count <= Index do
        FElements.Add(TGocciaHoleValue.HoleValue);
      FElements[Index] := TGocciaPropertyDescriptorData(ADescriptor).Value;
      ADescriptor.Free;
      Result := True;
    end
    else
      // Accessor descriptor on array index — store as regular property
      Result := inherited TryDefineProperty(AName, ADescriptor);
    Exit;
  end;

  // Non-index, non-length — delegate to inherited
  Result := inherited TryDefineProperty(AName, ADescriptor);
end;

// ES2026 §23.1.3.29 Array.prototype.sort(comparefn)
function TGocciaArrayValue.ArraySort(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  TempArr: TGocciaArrayValue;
  CustomSortFunction: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  View.Init(AThisValue);

  // Collect only present elements via View for prototype-aware access
  TempArr := TGocciaArrayValue.Create;
  for I := 0 to View.Len - 1 do
  begin
    if View.HasIndex(I) then
      TempArr.Elements.Add(View.Get(I));
  end;

  if AArgs.Length > 0 then
  begin
    CustomSortFunction := AArgs.GetElement(0);
    if not CustomSortFunction.IsCallable then
      ThrowError(SErrorCustomSortMustBeFunction, [], SSuggestCallbackRequired);
    CallArgs := TGocciaArgumentsCollection.Create([nil, nil]);
    try
      if TempArr.Elements.Count > 1 then
        QuickSortElements(TempArr.Elements, TGocciaFunctionBase(CustomSortFunction), CallArgs, AThisValue, 0, TempArr.Elements.Count - 1);
    finally
      CallArgs.Free;
    end;
  end else if TempArr.Elements.Count > 1 then
    TempArr.Elements.Sort(TComparer<TGocciaValue>.Construct(DefaultCompare));

  // Write sorted elements back to front indices
  for I := 0 to TempArr.Elements.Count - 1 do
    View.Put(I, TempArr.Elements[I]);
  // Delete trailing indices (holes moved to end)
  for I := TempArr.Elements.Count to View.Len - 1 do
    View.DeleteIndex(I);

  Result := View.Obj;
end;

// ES2026 §23.1.3.32 Array.prototype.splice(start, deleteCount, ...items)
function TGocciaArrayValue.ArraySplice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Removed: TGocciaArrayValue;
  StartIndex, DeleteCount, ActualStart, ItemCount, NewLen: Integer;
  I, From, Target: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  // Step 5: If start is not present
  if AArgs.Length < 1 then
  begin
    if Assigned(View.Arr) then
      Removed := ArraySpeciesCreate(View.Arr, 0)
    else
      Removed := TGocciaArrayValue.Create;
    Result := Removed;
    Exit;
  end;

  // Step 3: Let relativeStart be ToIntegerOrInfinity(start)
  StartIndex := ToIntegerFromArgs(AArgs);

  // Step 4: If relativeStart < 0, let actualStart be max(len + relativeStart, 0); else min(relativeStart, len)
  ActualStart := NormalizeRelativeIndex(StartIndex, View.Len);

  // Steps 5-9: Determine actualDeleteCount
  DeleteCount := ToIntegerFromArgs(AArgs, 1, View.Len - ActualStart);
  if DeleteCount < 0 then
    DeleteCount := 0
  else if ActualStart + DeleteCount > View.Len then
    DeleteCount := View.Len - ActualStart;

  ItemCount := AArgs.Length - 2;
  if ItemCount < 0 then
    ItemCount := 0;

  // Step 10: Let A be ArraySpeciesCreate(O, actualDeleteCount)
  if Assigned(View.Arr) then
    Removed := ArraySpeciesCreate(View.Arr, DeleteCount)
  else
    Removed := TGocciaArrayValue.Create;

  // Step 11: Collect removed elements (preserve sparsity per ES spec §23.1.3.32 step 11)
  for I := 0 to DeleteCount - 1 do
  begin
    if View.HasIndex(ActualStart + I) then
      ArrayCreateDataProperty(Removed, I, View.Get(ActualStart + I));
    // else: absent → leave hole in Removed (sparse)
  end;

  // Shift elements via View methods for prototype-aware semantics
  NewLen := View.Len - DeleteCount + ItemCount;
  if ItemCount < DeleteCount then
  begin
    // Shift elements left
    for I := ActualStart to View.Len - DeleteCount - 1 do
    begin
      From := I + DeleteCount;
      Target := I + ItemCount;
      if View.HasIndex(From) then
        View.Put(Target, View.Get(From))
      else
        View.DeleteIndex(Target);
    end;
    // Delete trailing properties
    for I := NewLen to View.Len - 1 do
      View.DeleteIndex(I);
  end
  else if ItemCount > DeleteCount then
  begin
    // Shift elements right
    for I := View.Len - DeleteCount - 1 downto ActualStart do
    begin
      From := I + DeleteCount;
      Target := I + ItemCount;
      if View.HasIndex(From) then
        View.Put(Target, View.Get(From))
      else
        View.DeleteIndex(Target);
    end;
  end;
  // Insert new items
  for I := 0 to ItemCount - 1 do
    View.Put(ActualStart + I, AArgs.GetElement(I + 2));
  View.SetLen(NewLen);

  // Ensure removed array has correct length (preserve trailing holes)
  while Removed.Elements.Count < DeleteCount do
    Removed.Elements.Add(TGocciaHoleValue.HoleValue);

  // Step 17: Return A
  Result := Removed;
end;

// ES2026 §23.1.3.25 Array.prototype.shift()
function TGocciaArrayValue.ArrayShift(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  I: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  // Step 3: If len = 0, set length to 0 and return undefined
  if View.Len = 0 then
  begin
    View.SetLen(0);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  // Step 4: Let first be Get(O, "0")
  Result := View.Get(0);

  // Steps 5-7: shift elements down via View for prototype-aware semantics
  for I := 1 to View.Len - 1 do
  begin
    if View.HasIndex(I) then
      View.Put(I - 1, View.Get(I))
    else
      View.DeleteIndex(I - 1);
  end;
  View.DeleteIndex(View.Len - 1);
  View.SetLen(View.Len - 1);
end;

// ES2026 §23.1.3.37 Array.prototype.unshift(...items)
function TGocciaArrayValue.ArrayUnshift(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  ArgCount, NewLen, I: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);
  ArgCount := AArgs.Length;

  // ES2026 §23.1.3.37 step 4: skip shifting when no arguments provided
  if ArgCount = 0 then
  begin
    Result := TGocciaNumberLiteralValue.Create(View.Len);
    Exit;
  end;

  // Shift existing elements up by argCount via View for prototype-aware semantics
  NewLen := View.Len + ArgCount;
  for I := View.Len - 1 downto 0 do
  begin
    if View.HasIndex(I) then
      View.Put(I + ArgCount, View.Get(I))
    else
      View.DeleteIndex(I + ArgCount);
  end;
  // Set items at front
  for I := 0 to ArgCount - 1 do
    View.Put(I, AArgs.GetElement(I));
  View.SetLen(NewLen);

  // Step 7: Return len + argCount
  Result := TGocciaNumberLiteralValue.Create(NewLen);
end;

// ES2026 §23.1.3.7 Array.prototype.fill(value [, start [, end]])
function TGocciaArrayValue.ArrayFill(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  FillValue: TGocciaValue;
  StartIdx, EndIdx, I: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  // Step 3: Let value (default undefined)
  if AArgs.Length < 1 then
    FillValue := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    FillValue := AArgs.GetElement(0);

  // Step 4: Let relativeStart be ToIntegerOrInfinity(start)
  StartIdx := ToIntegerFromArgs(AArgs, 1);

  // Step 6: If end is undefined, let relativeEnd be len; else ToIntegerOrInfinity(end)
  EndIdx := ToIntegerFromArgs(AArgs, 2, View.Len);

  // Step 5: If relativeStart < 0, let k be max(len + relativeStart, 0); else min(relativeStart, len)
  StartIdx := NormalizeRelativeIndex(StartIdx, View.Len);
  // Step 7: If relativeEnd < 0, let final be max(len + relativeEnd, 0); else min(relativeEnd, len)
  EndIdx := NormalizeRelativeIndex(EndIdx, View.Len);

  // Step 8: Repeat, while k < final — Set(O, ToString(k), value)
  for I := StartIdx to EndIdx - 1 do
    View.Put(I, FillValue);

  // Step 9: Return O
  Result := View.Obj;
end;

// ES2026 §23.1.3.1 Array.prototype.at(index)
function TGocciaArrayValue.ArrayAt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Index: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  if AArgs.Length < 1 then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  // Step 3: Let relativeIndex be ToIntegerOrInfinity(index)
  Index := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);

  // Step 4: If relativeIndex >= 0, let k be relativeIndex; else len + relativeIndex
  if Index < 0 then
    Index := View.Len + Index;

  // Step 5: If k < 0 or k >= len, return undefined
  if (Index < 0) or (Index >= View.Len) then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    // Step 6: Return Get(O, ToString(k))
    Result := View.Get(Index);
end;

// ES2026 §23.1.3.36 Array.prototype.values()
function TGocciaArrayValue.ArrayValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Let O be ToObject(this value)
  // Step 2: Return CreateArrayIterator(O, value)
  Result := TGocciaArrayIteratorValue.Create(ToObject(AThisValue), akValues);
end;

// ES2026 §23.1.3.17 Array.prototype.keys()
function TGocciaArrayValue.ArrayKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Let O be ToObject(this value)
  // Step 2: Return CreateArrayIterator(O, key)
  Result := TGocciaArrayIteratorValue.Create(ToObject(AThisValue), akKeys);
end;

// ES2026 §23.1.3.5 Array.prototype.entries()
function TGocciaArrayValue.ArrayEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Let O be ToObject(this value)
  // Step 2: Return CreateArrayIterator(O, key+value)
  Result := TGocciaArrayIteratorValue.Create(ToObject(AThisValue), akEntries);
end;

// ES2026 §23.1.3.39 Array.prototype[@@iterator]()
function TGocciaArrayValue.ArraySymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Return the result of calling Array.prototype.values
  Result := TGocciaArrayIteratorValue.Create(ToObject(AThisValue), akValues);
end;

end.
