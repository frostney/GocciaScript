unit Goccia.Values.SetValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaSetValue = class(TGocciaInstanceValue)
  private
    FItems: TGocciaValueList;
  public
    function SetHas(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetDelete(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetClear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetSymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetUnion(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetIntersection(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetDifference(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetSymmetricDifference(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetIsSubsetOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetIsSupersetOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetIsDisjointFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  private
    procedure InitializePrototype;
  public
    constructor Create(const AClass: TGocciaClassValue = nil);
    destructor Destroy; override;

    function ContainsValue(const AValue: TGocciaValue): Boolean;
    procedure AddItem(const AValue: TGocciaValue);

    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;
    function ToArray: TGocciaArrayValue;
    function ToStringTag: string; override;

    procedure InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); override;
    procedure MarkReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);

    property Items: TGocciaValueList read FItems;
  end;

implementation

uses
  SysUtils,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Evaluator.Comparison,
  Goccia.GarbageCollector,
  Goccia.Realm,
  Goccia.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.IteratorValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

type
  TGocciaSetRecord = record
    SetObject: TGocciaObjectValue;
    Size: Double;
    HasMethod: TGocciaValue;
    KeysMethod: TGocciaValue;
  end;

var
  GSetSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetSetShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GSetSharedSlot))
  else
    Result := nil;
end;

function SetDataIndex(const AItems: TGocciaValueList; const AValue: TGocciaValue): Integer;
var
  I: Integer;
begin
  for I := 0 to AItems.Count - 1 do
    if IsSameValueZero(AItems[I], AValue) then
      Exit(I);
  Result := -1;
end;

procedure RemoveSetItem(const ASet: TGocciaSetValue; const AValue: TGocciaValue);
var
  Index: Integer;
begin
  Index := SetDataIndex(ASet.FItems, AValue);
  if Index >= 0 then
    ASet.FItems.Delete(Index);
end;

function GetSetRecord(const AValue: TGocciaValue; const AMethodName: string): TGocciaSetRecord;
var
  RawSize: TGocciaValue;
  NumberSize: TGocciaNumberLiteralValue;
begin
  // ES2026 §24.2.1.2 GetSetRecord(obj)
  if not (AValue is TGocciaObjectValue) then
    ThrowTypeError(Format(SErrorSetOperationRequiresSetLike, [AMethodName]), SSuggestSetOperationArgType);

  Result.SetObject := TGocciaObjectValue(AValue);
  RawSize := Result.SetObject.GetProperty(PROP_SIZE);
  NumberSize := RawSize.ToNumberLiteral;
  if NumberSize.IsNaN then
    ThrowTypeError(Format(SErrorSetLikeSizeMustBeNumber, [AMethodName]), SSuggestSetOperationArgType);
  if NumberSize.Value < 0 then
    ThrowRangeError(Format(SErrorSetLikeSizeNonNegative, [AMethodName]), SSuggestSetOperationArgType);
  if NumberSize.IsInfinity then
    Result.Size := NumberSize.Value
  else
    Result.Size := Trunc(NumberSize.Value);

  Result.HasMethod := Result.SetObject.GetProperty(PROP_HAS);
  if not Result.HasMethod.IsCallable then
    ThrowTypeError(Format(SErrorSetLikeHasNotCallable, [AMethodName]), SSuggestSetOperationArgType);

  Result.KeysMethod := Result.SetObject.GetProperty(PROP_KEYS);
  if not Result.KeysMethod.IsCallable then
    ThrowTypeError(Format(SErrorSetLikeKeysNotCallable, [AMethodName]), SSuggestSetOperationArgType);
end;

function SetRecordHas(const ARecord: TGocciaSetRecord; const AValue: TGocciaValue): Boolean;
var
  CallArgs: TGocciaArgumentsCollection;
begin
  CallArgs := TGocciaArgumentsCollection.Create([AValue]);
  try
    Result := InvokeCallable(ARecord.HasMethod, CallArgs, ARecord.SetObject).ToBooleanLiteral.Value;
  finally
    CallArgs.Free;
  end;
end;

function GetSetRecordKeysIterator(const ARecord: TGocciaSetRecord; const AMethodName: string): TGocciaIteratorValue;
var
  CallArgs: TGocciaArgumentsCollection;
  IteratorObject, NextMethod: TGocciaValue;
begin
  // ES2026 §24.2.4 Set operation methods: GetIteratorFromMethod(otherRec.[[SetObject]], otherRec.[[Keys]])
  CallArgs := TGocciaArgumentsCollection.Create;
  try
    IteratorObject := InvokeCallable(ARecord.KeysMethod, CallArgs, ARecord.SetObject);
  finally
    CallArgs.Free;
  end;

  if IteratorObject is TGocciaIteratorValue then
    Exit(TGocciaIteratorValue(IteratorObject));

  if IteratorObject is TGocciaObjectValue then
  begin
    NextMethod := TGocciaObjectValue(IteratorObject).GetProperty(PROP_NEXT);
    if Assigned(NextMethod) and not (NextMethod is TGocciaUndefinedLiteralValue) and NextMethod.IsCallable then
      Exit(TGocciaGenericIteratorValue.Create(IteratorObject));
  end;

  ThrowTypeError(Format(SErrorSetLikeKeysIterator, [AMethodName]), SSuggestIteratorProtocol);
  Result := nil;
end;

constructor TGocciaSetValue.Create(const AClass: TGocciaClassValue = nil);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(AClass);
  FItems := TGocciaValueList.Create(False);
  InitializePrototype;
  Shared := GetSetShared;
  if not Assigned(AClass) and Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

procedure TGocciaSetValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetSetShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GSetSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddNamedMethod('has', SetHas, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('add', SetAdd, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('delete', SetDelete, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('clear', SetClear, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('forEach', SetForEach, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('values', SetValues, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('keys', SetKeys, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('entries', SetEntries, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('union', SetUnion, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('intersection', SetIntersection, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('difference', SetDifference, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('symmetricDifference', SetSymmetricDifference, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('isSubsetOf', SetIsSubsetOf, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('isSupersetOf', SetIsSupersetOf, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('isDisjointFrom', SetIsDisjointFrom, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolMethod(
        TGocciaSymbolValue.WellKnownIterator,
        '[Symbol.iterator]',
        SetSymbolIterator,
        0,
        [pfConfigurable, pfWritable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaSetValue.ExposePrototype(const AConstructor: TGocciaValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetSetShared;
  if not Assigned(Shared) then
  begin
    TGocciaSetValue.Create;
    Shared := GetSetShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

destructor TGocciaSetValue.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TGocciaSetValue.InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection);
var
  InitArg: TGocciaValue;
  ArrValue: TGocciaArrayValue;
  I: Integer;
begin
  if AArguments.Length = 0 then
    Exit;
  InitArg := AArguments.GetElement(0);
  if InitArg is TGocciaArrayValue then
  begin
    ArrValue := TGocciaArrayValue(InitArg);
    for I := 0 to ArrValue.Elements.Count - 1 do
      if Assigned(ArrValue.Elements[I]) then
        AddItem(ArrValue.Elements[I]);
  end
  else if InitArg is TGocciaSetValue then
  begin
    for I := 0 to TGocciaSetValue(InitArg).Items.Count - 1 do
      AddItem(TGocciaSetValue(InitArg).Items[I]);
  end;
end;

procedure TGocciaSetValue.MarkReferences;
var
  I: Integer;
begin
  if GCMarked then Exit;
  inherited;

  for I := 0 to FItems.Count - 1 do
  begin
    if Assigned(FItems[I]) then
      FItems[I].MarkReferences;
  end;
end;

function TGocciaSetValue.ContainsValue(const AValue: TGocciaValue): Boolean;
begin
  Result := SetDataIndex(FItems, AValue) >= 0;
end;

procedure TGocciaSetValue.AddItem(const AValue: TGocciaValue);
begin
  if not ContainsValue(AValue) then
    FItems.Add(AValue);
end;

function TGocciaSetValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaSetValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
begin
  if AName = PROP_SIZE then
    Result := TGocciaNumberLiteralValue.Create(FItems.Count)
  else
    Result := inherited GetPropertyWithContext(AName, AThisContext);
end;

function TGocciaSetValue.ToArray: TGocciaArrayValue;
var
  I: Integer;
begin
  Result := TGocciaArrayValue.Create;
  for I := 0 to FItems.Count - 1 do
    Result.Elements.Add(FItems[I]);
end;

function TGocciaSetValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_SET;
end;

{ Instance methods }

// ES2026 §24.2.3.7 Set.prototype.has(value)
function TGocciaSetValue.SetHas(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  S: TGocciaSetValue;
begin
  // Step 1: Let S be the this value
  // Steps 2-3: If S does not have a [[SetData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaSetValue) then
    ThrowTypeError(SErrorSetHasNonSet, SSuggestSetThisType);
  S := TGocciaSetValue(AThisValue);
  // Step 4: For each element e of S.[[SetData]], do
  //   If e is not empty and SameValueZero(e, value) is true, return true
  if (AArgs.Length > 0) and S.ContainsValue(AArgs.GetElement(0)) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    // Step 5: Return false
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §24.2.3.1 Set.prototype.add(value)
function TGocciaSetValue.SetAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  S: TGocciaSetValue;
begin
  // Step 1: Let S be the this value
  // Steps 2-3: If S does not have a [[SetData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaSetValue) then
    ThrowTypeError(SErrorSetAddNonSet, SSuggestSetThisType);
  S := TGocciaSetValue(AThisValue);
  // Step 4: For each element e of S.[[SetData]], do
  //   If e is not empty and SameValueZero(e, value) is true, return S
  // Step 5: If value is -0, set value to +0
  // Step 6: Append value to S.[[SetData]]
  if AArgs.Length > 0 then
    S.AddItem(AArgs.GetElement(0));
  // Step 7: Return S
  Result := AThisValue;
end;

// ES2026 §24.2.3.4 Set.prototype.delete(value)
function TGocciaSetValue.SetDelete(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  S: TGocciaSetValue;
  I: Integer;
begin
  // Step 1: Let S be the this value
  // Steps 2-3: If S does not have a [[SetData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaSetValue) then
    ThrowTypeError(SErrorSetDeleteNonSet, SSuggestSetThisType);
  S := TGocciaSetValue(AThisValue);
  // Step 5 (early): Default return false
  Result := TGocciaBooleanLiteralValue.FalseValue;
  if AArgs.Length > 0 then
  begin
    // Step 4: For each element e of S.[[SetData]], do
    for I := 0 to S.FItems.Count - 1 do
    begin
      // Step 4a: If e is not empty and SameValueZero(e, value) is true
      if IsSameValueZero(S.FItems[I], AArgs.GetElement(0)) then
      begin
        // Step 4a.i: Replace the element of S.[[SetData]] with empty
        S.FItems.Delete(I);
        // Step 4a.ii: Return true
        Result := TGocciaBooleanLiteralValue.TrueValue;
        Exit;
      end;
    end;
  end;
  // Step 5: Return false
end;

// ES2026 §24.2.3.2 Set.prototype.clear()
function TGocciaSetValue.SetClear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Let S be the this value
  // Steps 2-3: If S does not have a [[SetData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaSetValue) then
    ThrowTypeError(SErrorSetClearNonSet, SSuggestSetThisType);
  // Step 4: For each element e of S.[[SetData]], replace e with empty
  TGocciaSetValue(AThisValue).FItems.Clear;
  // Step 5: Return undefined
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §24.2.3.6 Set.prototype.forEach(callbackfn [, thisArg])
function TGocciaSetValue.SetForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  S: TGocciaSetValue;
  Callback, ThisArg: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  // Steps 1-3: If S does not have a [[SetData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaSetValue) then
    ThrowTypeError(SErrorSetForEachNonSet, SSuggestSetThisType);
  S := TGocciaSetValue(AThisValue);

  // Step 4: If IsCallable(callbackfn) is false, throw a TypeError
  if AArgs.Length > 0 then
    Callback := AArgs.GetElement(0)
  else
    Callback := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not Callback.IsCallable then
    ThrowTypeError(SErrorSetForEachNotCallable, SSuggestSetCallbackRequired);

  // Step 5: Let thisArg be the second argument
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;

  TypedCallback := nil;
  if Callback is TGocciaFunctionBase then
    TypedCallback := TGocciaFunctionBase(Callback);

  // Step 6: For each element e of S.[[SetData]], do
  for I := 0 to S.FItems.Count - 1 do
  begin
    // Step 6b: Call(callbackfn, thisArg, « e, e, S »)
    CallArgs := TGocciaArgumentsCollection.Create([S.FItems[I], S.FItems[I], S]);
    try
      if Assigned(TypedCallback) then
        TypedCallback.Call(CallArgs, ThisArg)
      else
        InvokeCallable(Callback, CallArgs, ThisArg);
    finally
      CallArgs.Free;
    end;
  end;

  // Step 7: Return undefined
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §24.2.3.10 Set.prototype.values()
function TGocciaSetValue.SetValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Let S be the this value
  // Steps 2-3: If S does not have a [[SetData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaSetValue) then
    ThrowTypeError(SErrorSetValuesNonSet, SSuggestSetThisType);
  // Step 4: Return CreateSetIterator(S, value)
  Result := TGocciaSetIteratorValue.Create(AThisValue, skValues);
end;

// ES2026 §24.2.3.8 Set.prototype.keys()
function TGocciaSetValue.SetKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: This method is the same function as Set.prototype.values (per spec)
  // Steps 2-3: If S does not have a [[SetData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaSetValue) then
    ThrowTypeError(SErrorSetKeysNonSet, SSuggestSetThisType);
  // Step 4: Return CreateSetIterator(S, value)
  Result := TGocciaSetIteratorValue.Create(AThisValue, skValues);
end;

// ES2026 §24.2.3.5 Set.prototype.entries()
function TGocciaSetValue.SetEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Let S be the this value
  // Steps 2-3: If S does not have a [[SetData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaSetValue) then
    ThrowTypeError(SErrorSetEntriesNonSet, SSuggestSetThisType);
  // Step 4: Return CreateSetIterator(S, key+value)
  Result := TGocciaSetIteratorValue.Create(AThisValue, skEntries);
end;

// ES2026 §24.2.3.11 Set.prototype[@@iterator]()
function TGocciaSetValue.SetSymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: This method is the same function as Set.prototype.values (per spec)
  // Steps 2-3: If S does not have a [[SetData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaSetValue) then
    ThrowTypeError(SErrorSetIteratorNonSet, SSuggestSetThisType);
  Result := TGocciaSetIteratorValue.Create(AThisValue, skValues);
end;

// ES2026 §24.2.3.12 Set.prototype.union(other)
function TGocciaSetValue.SetUnion(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSet, ResultSet: TGocciaSetValue;
  OtherRecord: TGocciaSetRecord;
  Iterator: TGocciaIteratorValue;
  NextValue: TGocciaValue;
  Done, WasResultRooted, WasIteratorRooted: Boolean;
  I: Integer;
  GC: TGarbageCollector;
begin
  // Step 1: Let O be the this value
  // Steps 2-3: If O does not have a [[SetData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaSetValue) then
    ThrowTypeError(SErrorSetUnionNonSet, SSuggestSetThisType);
  ThisSet := TGocciaSetValue(AThisValue);
  // Step 4: Let otherRec be GetSetRecord(other)
  OtherRecord := GetSetRecord(AArgs.GetElement(0), 'union');
  // Step 5: Let resultSetData be a copy of O.[[SetData]]
  ResultSet := TGocciaSetValue.Create;
  GC := TGarbageCollector.Instance;
  WasResultRooted := Assigned(GC) and GC.IsTempRoot(ResultSet);
  if Assigned(GC) and not WasResultRooted then
    GC.AddTempRoot(ResultSet);
  try
    for I := 0 to ThisSet.FItems.Count - 1 do
      ResultSet.AddItem(ThisSet.FItems[I]);
    // Step 6: Let keysIter be GetIteratorFromSetLike(otherRec)
    Iterator := GetSetRecordKeysIterator(OtherRecord, 'union');
    WasIteratorRooted := Assigned(GC) and GC.IsTempRoot(Iterator);
    if Assigned(GC) and not WasIteratorRooted then
      GC.AddTempRoot(Iterator);
    try
      // Step 7: For each element nextValue from keysIter, do
      NextValue := Iterator.DirectNext(Done);
      while not Done do
      begin
        // If nextValue is not already in resultSetData, append nextValue
        ResultSet.AddItem(NextValue);
        NextValue := Iterator.DirectNext(Done);
      end;
    finally
      if Assigned(GC) and not WasIteratorRooted then
        GC.RemoveTempRoot(Iterator);
    end;

    // Step 8: Let result be OrdinaryObjectCreate(SetPrototype, « [[SetData]] »)
    // Step 9: Set result.[[SetData]] to resultSetData
    // Step 10: Return result
    Result := ResultSet;
  finally
    if Assigned(GC) and not WasResultRooted then
      GC.RemoveTempRoot(ResultSet);
  end;
end;

// ES2026 §24.2.3.7 Set.prototype.intersection(other)
function TGocciaSetValue.SetIntersection(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSet, ResultSet: TGocciaSetValue;
  OtherRecord: TGocciaSetRecord;
  Iterator: TGocciaIteratorValue;
  NextValue: TGocciaValue;
  Done, WasResultRooted, WasIteratorRooted: Boolean;
  I: Integer;
  GC: TGarbageCollector;
begin
  // Step 1: Let O be the this value
  // Steps 2-3: If O does not have a [[SetData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaSetValue) then
    ThrowTypeError(SErrorSetIntersectionNonSet, SSuggestSetThisType);
  ThisSet := TGocciaSetValue(AThisValue);
  // Step 4: Let otherRec be GetSetRecord(other)
  OtherRecord := GetSetRecord(AArgs.GetElement(0), 'intersection');
  // Step 5: Let resultSetData be a new empty List
  ResultSet := TGocciaSetValue.Create;
  GC := TGarbageCollector.Instance;
  WasResultRooted := Assigned(GC) and GC.IsTempRoot(ResultSet);
  if Assigned(GC) and not WasResultRooted then
    GC.AddTempRoot(ResultSet);
  try
    if ThisSet.FItems.Count <= OtherRecord.Size then
    begin
      // Step 6: For each element e of O.[[SetData]], do
      for I := 0 to ThisSet.FItems.Count - 1 do
        // Step 6a: If e is not empty and SetDataHas(otherRec, e) is true, append e
        if SetRecordHas(OtherRecord, ThisSet.FItems[I]) then
          ResultSet.AddItem(ThisSet.FItems[I]);
    end
    else
    begin
      Iterator := GetSetRecordKeysIterator(OtherRecord, 'intersection');
      WasIteratorRooted := Assigned(GC) and GC.IsTempRoot(Iterator);
      if Assigned(GC) and not WasIteratorRooted then
        GC.AddTempRoot(Iterator);
      try
        NextValue := Iterator.DirectNext(Done);
        while not Done do
        begin
          if ThisSet.ContainsValue(NextValue) then
            ResultSet.AddItem(NextValue);
          NextValue := Iterator.DirectNext(Done);
        end;
      finally
        if Assigned(GC) and not WasIteratorRooted then
          GC.RemoveTempRoot(Iterator);
      end;
    end;

    // Step 7: Let result be OrdinaryObjectCreate(SetPrototype, « [[SetData]] »)
    // Step 8: Set result.[[SetData]] to resultSetData
    // Step 9: Return result
    Result := ResultSet;
  finally
    if Assigned(GC) and not WasResultRooted then
      GC.RemoveTempRoot(ResultSet);
  end;
end;

// ES2026 §24.2.3.3 Set.prototype.difference(other)
function TGocciaSetValue.SetDifference(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSet, ResultSet: TGocciaSetValue;
  OtherRecord: TGocciaSetRecord;
  Iterator: TGocciaIteratorValue;
  NextValue: TGocciaValue;
  Done, WasResultRooted, WasIteratorRooted: Boolean;
  I: Integer;
  GC: TGarbageCollector;
begin
  // Step 1: Let O be the this value
  // Steps 2-3: If O does not have a [[SetData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaSetValue) then
    ThrowTypeError(SErrorSetDifferenceNonSet, SSuggestSetThisType);
  ThisSet := TGocciaSetValue(AThisValue);
  // Step 4: Let otherRec be GetSetRecord(other)
  OtherRecord := GetSetRecord(AArgs.GetElement(0), 'difference');
  // Step 5: Let resultSetData be a copy of O.[[SetData]]
  ResultSet := TGocciaSetValue.Create;
  GC := TGarbageCollector.Instance;
  WasResultRooted := Assigned(GC) and GC.IsTempRoot(ResultSet);
  if Assigned(GC) and not WasResultRooted then
    GC.AddTempRoot(ResultSet);
  try
    if ThisSet.FItems.Count <= OtherRecord.Size then
    begin
      // Step 6: For each element e of resultSetData, do
      for I := 0 to ThisSet.FItems.Count - 1 do
        // Step 6a: If e is not empty and SetDataHas(otherRec, e) is true, remove e
        if not SetRecordHas(OtherRecord, ThisSet.FItems[I]) then
          ResultSet.AddItem(ThisSet.FItems[I]);
    end
    else
    begin
      for I := 0 to ThisSet.FItems.Count - 1 do
        ResultSet.AddItem(ThisSet.FItems[I]);

      Iterator := GetSetRecordKeysIterator(OtherRecord, 'difference');
      WasIteratorRooted := Assigned(GC) and GC.IsTempRoot(Iterator);
      if Assigned(GC) and not WasIteratorRooted then
        GC.AddTempRoot(Iterator);
      try
        NextValue := Iterator.DirectNext(Done);
        while not Done do
        begin
          RemoveSetItem(ResultSet, NextValue);
          NextValue := Iterator.DirectNext(Done);
        end;
      finally
        if Assigned(GC) and not WasIteratorRooted then
          GC.RemoveTempRoot(Iterator);
      end;
    end;

    // Step 7: Let result be OrdinaryObjectCreate(SetPrototype, « [[SetData]] »)
    // Step 8: Set result.[[SetData]] to resultSetData
    // Step 9: Return result
    Result := ResultSet;
  finally
    if Assigned(GC) and not WasResultRooted then
      GC.RemoveTempRoot(ResultSet);
  end;
end;

// ES2026 §24.2.3.9 Set.prototype.symmetricDifference(other)
function TGocciaSetValue.SetSymmetricDifference(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSet, ResultSet: TGocciaSetValue;
  OtherRecord: TGocciaSetRecord;
  Iterator: TGocciaIteratorValue;
  NextValue: TGocciaValue;
  Done, WasResultRooted, WasIteratorRooted: Boolean;
  I: Integer;
  GC: TGarbageCollector;
begin
  // Step 1: Let O be the this value
  // Steps 2-3: If O does not have a [[SetData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaSetValue) then
    ThrowTypeError(SErrorSetSymmetricDifferenceNonSet, SSuggestSetThisType);
  ThisSet := TGocciaSetValue(AThisValue);
  // Step 4: Let otherRec be GetSetRecord(other)
  OtherRecord := GetSetRecord(AArgs.GetElement(0), 'symmetricDifference');
  // Step 5: Let resultSetData be a copy of O.[[SetData]]
  ResultSet := TGocciaSetValue.Create;
  GC := TGarbageCollector.Instance;
  WasResultRooted := Assigned(GC) and GC.IsTempRoot(ResultSet);
  if Assigned(GC) and not WasResultRooted then
    GC.AddTempRoot(ResultSet);
  try
    for I := 0 to ThisSet.FItems.Count - 1 do
      ResultSet.AddItem(ThisSet.FItems[I]);

    // Step 7: Let keysIter be GetIteratorFromSetLike(otherRec)
    Iterator := GetSetRecordKeysIterator(OtherRecord, 'symmetricDifference');
    WasIteratorRooted := Assigned(GC) and GC.IsTempRoot(Iterator);
    if Assigned(GC) and not WasIteratorRooted then
      GC.AddTempRoot(Iterator);
    try
      // Step 8: For each element nextValue from keysIter, do
      NextValue := Iterator.DirectNext(Done);
      while not Done do
      begin
        // If nextValue was in O.[[SetData]], remove it; otherwise append it
        if ThisSet.ContainsValue(NextValue) then
          RemoveSetItem(ResultSet, NextValue)
        else
          ResultSet.AddItem(NextValue);
        NextValue := Iterator.DirectNext(Done);
      end;
    finally
      if Assigned(GC) and not WasIteratorRooted then
        GC.RemoveTempRoot(Iterator);
    end;

    // Step 9: Let result be OrdinaryObjectCreate(SetPrototype, « [[SetData]] »)
    // Step 10: Set result.[[SetData]] to resultSetData
    // Step 11: Return result
    Result := ResultSet;
  finally
    if Assigned(GC) and not WasResultRooted then
      GC.RemoveTempRoot(ResultSet);
  end;
end;

// ES2026 §24.2.3.8 Set.prototype.isSubsetOf(other)
function TGocciaSetValue.SetIsSubsetOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSet: TGocciaSetValue;
  OtherRecord: TGocciaSetRecord;
  I: Integer;
begin
  // Step 1: Let O be the this value
  // Steps 2-3: If O does not have a [[SetData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaSetValue) then
    ThrowTypeError(SErrorSetIsSubsetOfNonSet, SSuggestSetThisType);
  ThisSet := TGocciaSetValue(AThisValue);
  // Step 4: Let otherRec be GetSetRecord(other)
  OtherRecord := GetSetRecord(AArgs.GetElement(0), 'isSubsetOf');

  // Step 5: If SetDataSize(O) > otherRec.[[Size]], return false (optimization)
  if ThisSet.FItems.Count > OtherRecord.Size then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  // Step 6: For each element e of O.[[SetData]], do
  for I := 0 to ThisSet.FItems.Count - 1 do
    // Step 6a: If SetDataHas(otherRec, e) is false, return false
    if not SetRecordHas(OtherRecord, ThisSet.FItems[I]) then
    begin
      Result := TGocciaBooleanLiteralValue.FalseValue;
      Exit;
    end;

  // Step 7: Return true
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

// ES2026 §24.2.3.9 Set.prototype.isSupersetOf(other)
function TGocciaSetValue.SetIsSupersetOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSet: TGocciaSetValue;
  OtherRecord: TGocciaSetRecord;
  Iterator: TGocciaIteratorValue;
  NextValue: TGocciaValue;
  Done, WasIteratorRooted: Boolean;
  GC: TGarbageCollector;
begin
  // Step 1: Let O be the this value
  // Steps 2-3: If O does not have a [[SetData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaSetValue) then
    ThrowTypeError(SErrorSetIsSupersetOfNonSet, SSuggestSetThisType);
  ThisSet := TGocciaSetValue(AThisValue);
  // Step 4: Let otherRec be GetSetRecord(other)
  OtherRecord := GetSetRecord(AArgs.GetElement(0), 'isSupersetOf');

  // Step 5: If SetDataSize(O) < otherRec.[[Size]], return false (optimization)
  if ThisSet.FItems.Count < OtherRecord.Size then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  // Step 6: Let keysIter be GetIteratorFromSetLike(otherRec)
  Iterator := GetSetRecordKeysIterator(OtherRecord, 'isSupersetOf');
  GC := TGarbageCollector.Instance;
  WasIteratorRooted := Assigned(GC) and GC.IsTempRoot(Iterator);
  if Assigned(GC) and not WasIteratorRooted then
    GC.AddTempRoot(Iterator);
  try
    // Step 7: For each element nextValue from keysIter, do
    NextValue := Iterator.DirectNext(Done);
    while not Done do
    begin
      // Step 7a: If SetDataHas(O, nextValue) is false, return false
      if not ThisSet.ContainsValue(NextValue) then
      begin
        Iterator.Close;
        Result := TGocciaBooleanLiteralValue.FalseValue;
        Exit;
      end;
      NextValue := Iterator.DirectNext(Done);
    end;
  finally
    if Assigned(GC) and not WasIteratorRooted then
      GC.RemoveTempRoot(Iterator);
  end;

  // Step 8: Return true
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

// ES2026 §24.2.3.6 Set.prototype.isDisjointFrom(other)
function TGocciaSetValue.SetIsDisjointFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSet: TGocciaSetValue;
  OtherRecord: TGocciaSetRecord;
  Iterator: TGocciaIteratorValue;
  NextValue: TGocciaValue;
  Done, WasIteratorRooted: Boolean;
  I: Integer;
  GC: TGarbageCollector;
begin
  // Step 1: Let O be the this value
  // Steps 2-3: If O does not have a [[SetData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaSetValue) then
    ThrowTypeError(SErrorSetIsDisjointFromNonSet, SSuggestSetThisType);
  ThisSet := TGocciaSetValue(AThisValue);
  // Step 4: Let otherRec be GetSetRecord(other)
  OtherRecord := GetSetRecord(AArgs.GetElement(0), 'isDisjointFrom');

  if ThisSet.FItems.Count <= OtherRecord.Size then
  begin
    // Step 5: For each element e of O.[[SetData]], do
    for I := 0 to ThisSet.FItems.Count - 1 do
      // Step 5a: If e is not empty and SetDataHas(otherRec, e) is true, return false
      if SetRecordHas(OtherRecord, ThisSet.FItems[I]) then
      begin
        Result := TGocciaBooleanLiteralValue.FalseValue;
        Exit;
      end;
  end
  else
  begin
    Iterator := GetSetRecordKeysIterator(OtherRecord, 'isDisjointFrom');
    GC := TGarbageCollector.Instance;
    WasIteratorRooted := Assigned(GC) and GC.IsTempRoot(Iterator);
    if Assigned(GC) and not WasIteratorRooted then
      GC.AddTempRoot(Iterator);
    try
      NextValue := Iterator.DirectNext(Done);
      while not Done do
      begin
        if ThisSet.ContainsValue(NextValue) then
        begin
          Iterator.Close;
          Result := TGocciaBooleanLiteralValue.FalseValue;
          Exit;
        end;
        NextValue := Iterator.DirectNext(Done);
      end;
    finally
      if Assigned(GC) and not WasIteratorRooted then
        GC.RemoveTempRoot(Iterator);
    end;
  end;

  // Step 6: Return true
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

initialization
  GSetSharedSlot := RegisterRealmOwnedSlot('Set.shared');

end.
