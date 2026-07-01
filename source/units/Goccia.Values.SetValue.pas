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
  Goccia.Values.OrderedValueMap,
  Goccia.Values.Primitives;

type
  TGocciaSetValue = class(TGocciaInstanceValue)
  private
    // Backed by the shared SameValueZero ordered store, keyed by the element
    // with the element itself as the value (key = value).
    FStore: TGocciaOrderedValueMap;
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

    // Live, insertion-ordered cursor over active members. Seed ACursor with 0.
    function NextItem(var ACursor: Integer; out AValue: TGocciaValue): Boolean;
    // Like NextItem, but stops at physical slot ALimit (captured via
    // EntrySlotCount before a set operation that runs user callbacks), so
    // members appended during a callback are not visited.
    function NextItemBounded(var ACursor: Integer; ALimit: Integer;
      out AValue: TGocciaValue): Boolean;
    function EntrySlotCount: Integer;
    procedure RetainIterator;
    procedure ReleaseIterator;
    function Count: Integer;

    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;
    function ToArray: TGocciaArrayValue;
    function ToStringTag: string; override;
    function UsesECMAScriptBuiltinTagFallback: Boolean; override;

    procedure InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); override;
    procedure MarkReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);
  end;

implementation

uses
  SysUtils,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
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

function GetSetShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GSetSharedSlot))
  else
    Result := nil;
end;

procedure RemoveSetItem(const ASet: TGocciaSetValue; const AValue: TGocciaValue);
begin
  ASet.FStore.Remove(AValue);
end;

// Copy the set's current members into a plain array. `difference` iterates this
// snapshot because the spec (ES2026 §24.2.4.5) builds its result from a copy of
// O.[[SetData]] taken before any `has` callback runs, so later mutations of the
// receiver — including delete-then-readd — do not affect the result. The
// live-iterating operations (intersection/isSubsetOf/isDisjointFrom) instead use
// NextItemBounded over the original physical slots.
function SnapshotSetItems(const ASet: TGocciaSetValue): TArray<TGocciaValue>;
var
  Cursor, Count: Integer;
  Item: TGocciaValue;
begin
  SetLength(Result, ASet.Count);
  Count := 0;
  Cursor := 0;
  while ASet.NextItem(Cursor, Item) do
  begin
    Result[Count] := Item;
    Inc(Count);
  end;
  SetLength(Result, Count);
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
  IteratorRoot, NextMethodRoot: TGocciaTempRoot;
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
    InitializeTempRoot(IteratorRoot);
    InitializeTempRoot(NextMethodRoot);
    AddTempRootIfNeeded(IteratorRoot, IteratorObject);
    try
      NextMethod := TGocciaObjectValue(IteratorObject).GetProperty(PROP_NEXT);
      AddTempRootIfNeeded(NextMethodRoot, NextMethod);
      try
        if Assigned(NextMethod) and
           not (NextMethod is TGocciaUndefinedLiteralValue) and
           NextMethod.IsCallable then
          // Pass the validated NextMethod to the iterator wrapper so the
          // capture-once contract from §7.4.2 GetIteratorDirect holds.
          Exit(CreateRootedGenericIterator(IteratorObject, NextMethod));
      finally
        RemoveTempRootIfNeeded(NextMethodRoot);
      end;
    finally
      RemoveTempRootIfNeeded(IteratorRoot);
    end;
  end;

  ThrowTypeError(Format(SErrorSetLikeKeysIterator, [AMethodName]), SSuggestIteratorProtocol);
  Result := nil;
end;

constructor TGocciaSetValue.Create(const AClass: TGocciaClassValue = nil);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(AClass);
  FStore := TGocciaOrderedValueMap.Create;
  InitializePrototype;
  Shared := GetSetShared;
  if not Assigned(AClass) and Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

procedure TGocciaSetValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetSetShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GSetSharedSlot, Shared);
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
    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create(CONSTRUCTOR_SET),
      [pfConfigurable]);
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Shared.Prototype, PrototypeMembers);
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
  FStore.Free;
  inherited;
end;

procedure TGocciaSetValue.InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection);
var
  InitArg: TGocciaValue;
  ArrValue: TGocciaArrayValue;
  OtherSet: TGocciaSetValue;
  Cursor: Integer;
  Item: TGocciaValue;
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
    OtherSet := TGocciaSetValue(InitArg);
    Cursor := 0;
    while OtherSet.NextItem(Cursor, Item) do
      AddItem(Item);
  end;
end;

procedure TGocciaSetValue.MarkReferences;
var
  Cursor: Integer;
  Item: TGocciaValue;
begin
  if GCMarked then Exit;
  inherited;

  Cursor := 0;
  while NextItem(Cursor, Item) do
    if Assigned(Item) then
      Item.MarkReferences;
end;

function TGocciaSetValue.ContainsValue(const AValue: TGocciaValue): Boolean;
begin
  Result := FStore.ContainsKey(AValue);
end;

procedure TGocciaSetValue.AddItem(const AValue: TGocciaValue);
begin
  // The store canonicalizes and stores the element as both key and value, so
  // iteration yields the canonical form (-0 observed as +0). An existing member
  // is left in place, matching Set.add (no reordering on re-add).
  FStore.AddSetMember(AValue);
end;

function TGocciaSetValue.NextItem(var ACursor: Integer; out AValue: TGocciaValue): Boolean;
var
  Key: TGocciaValue;
begin
  Result := FStore.NextEntry(ACursor, Key, AValue);
end;

function TGocciaSetValue.NextItemBounded(var ACursor: Integer; ALimit: Integer;
  out AValue: TGocciaValue): Boolean;
var
  Key: TGocciaValue;
begin
  Result := FStore.NextEntryBounded(ACursor, ALimit, Key, AValue);
end;

function TGocciaSetValue.EntrySlotCount: Integer;
begin
  Result := FStore.EntrySlotCount;
end;

procedure TGocciaSetValue.RetainIterator;
begin
  FStore.RetainIterator;
end;

procedure TGocciaSetValue.ReleaseIterator;
begin
  FStore.ReleaseIterator;
end;

function TGocciaSetValue.Count: Integer;
begin
  Result := FStore.Count;
end;

function TGocciaSetValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaSetValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
begin
  if AName = PROP_SIZE then
    Result := TGocciaNumberLiteralValue.Create(FStore.Count)
  else
    Result := inherited GetPropertyWithContext(AName, AThisContext);
end;

function TGocciaSetValue.ToArray: TGocciaArrayValue;
var
  Cursor: Integer;
  Item: TGocciaValue;
begin
  Result := TGocciaArrayValue.Create;
  Cursor := 0;
  while NextItem(Cursor, Item) do
    Result.Elements.Add(Item);
end;

function TGocciaSetValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_SET;
end;

function TGocciaSetValue.UsesECMAScriptBuiltinTagFallback: Boolean;
begin
  Result := True;
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
  // An omitted argument is the value `undefined` (GetElement yields undefined).
  if S.ContainsValue(AArgs.GetElement(0)) then
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
  // An omitted argument is the value `undefined` (GetElement yields undefined).
  S.AddItem(AArgs.GetElement(0));
  // Step 7: Return S
  Result := AThisValue;
end;

// ES2026 §24.2.3.4 Set.prototype.delete(value)
function TGocciaSetValue.SetDelete(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  S: TGocciaSetValue;
begin
  // Step 1: Let S be the this value
  // Steps 2-3: If S does not have a [[SetData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaSetValue) then
    ThrowTypeError(SErrorSetDeleteNonSet, SSuggestSetThisType);
  S := TGocciaSetValue(AThisValue);
  // Step 4: For each element e of S.[[SetData]], do
  //   If SameValueZero(e, value) is true, replace e with empty (tombstone)
  //   and return true
  // An omitted argument is the value `undefined` (GetElement yields undefined).
  if S.FStore.Remove(AArgs.GetElement(0)) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    // Step 5: Return false
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §24.2.3.2 Set.prototype.clear()
function TGocciaSetValue.SetClear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Let S be the this value
  // Steps 2-3: If S does not have a [[SetData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaSetValue) then
    ThrowTypeError(SErrorSetClearNonSet, SSuggestSetThisType);
  // Step 4: For each element e of S.[[SetData]], replace e with empty
  TGocciaSetValue(AThisValue).FStore.Clear;
  // Step 5: Return undefined
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §24.2.3.6 Set.prototype.forEach(callbackfn [, thisArg])
function TGocciaSetValue.SetForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  S: TGocciaSetValue;
  Callback, ThisArg, Item: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  CallArgs: TGocciaArgumentsCollection;
  Cursor: Integer;
  SetRoot, CallbackRoot, ThisRoot: TGocciaTempRoot;
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

  InitializeTempRoot(SetRoot);
  InitializeTempRoot(CallbackRoot);
  InitializeTempRoot(ThisRoot);
  AddTempRootIfNeeded(SetRoot, S);
  AddTempRootIfNeeded(CallbackRoot, Callback);
  AddTempRootIfNeeded(ThisRoot, ThisArg);
  // Keep entry indices stable across callback mutation (appended values are
  // visited, deleted values are skipped — §24.2.3.6 re-reads numEntries).
  S.RetainIterator;
  try
    // Step 6: For each element e of S.[[SetData]], do
    Cursor := 0;
    while S.NextItem(Cursor, Item) do
    begin
      // Step 6b: Call(callbackfn, thisArg, « e, e, S »)
      CallArgs := TGocciaArgumentsCollection.Create([Item, Item, S]);
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
  finally
    S.ReleaseIterator;
    RemoveTempRootIfNeeded(ThisRoot);
    RemoveTempRootIfNeeded(CallbackRoot);
    RemoveTempRootIfNeeded(SetRoot);
  end;
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
  NextValue, Item: TGocciaValue;
  Done, WasResultRooted, WasIteratorRooted: Boolean;
  Cursor: Integer;
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
    Cursor := 0;
    while ThisSet.NextItem(Cursor, Item) do
      ResultSet.AddItem(Item);
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
  NextValue, Item: TGocciaValue;
  Done, WasResultRooted, WasIteratorRooted: Boolean;
  Cursor, Limit: Integer;
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
    if ThisSet.Count <= OtherRecord.Size then
    begin
      // Step 6: For each element e of O.[[SetData]] present when the operation
      // began. SetRecordHas runs user code that may mutate O; retain the store
      // so physical slot indices stay stable, and bound iteration to the
      // original slot count — appended members are not visited, deleted members
      // are skipped, and a delete-then-readd lands past the bound (§24.2.4.6).
      Limit := ThisSet.EntrySlotCount;
      ThisSet.RetainIterator;
      try
        Cursor := 0;
        while ThisSet.NextItemBounded(Cursor, Limit, Item) do
          // Step 6a: If SetDataHas(otherRec, e) is true, append e
          if SetRecordHas(OtherRecord, Item) then
            ResultSet.AddItem(Item);
      finally
        ThisSet.ReleaseIterator;
      end;
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
  NextValue, Item: TGocciaValue;
  Done, WasResultRooted, WasIteratorRooted: Boolean;
  Cursor, I: Integer;
  Snapshot: TArray<TGocciaValue>;
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
    if ThisSet.Count <= OtherRecord.Size then
    begin
      // Step 5/6: resultSetData is a copy of O.[[SetData]]; remove members that
      // are in other. Copy into the temp-rooted ResultSet first so the snapshot
      // members stay reachable even if a SetRecordHas callback deletes one from
      // O and triggers a GC, then remove those present in other (§24.2.4.5).
      // Operating on the copy also means mutations to O during the callback do
      // not affect the result.
      Snapshot := SnapshotSetItems(ThisSet);
      for I := 0 to High(Snapshot) do
        ResultSet.AddItem(Snapshot[I]);
      for I := 0 to High(Snapshot) do
        // Step 6a: If e is in other, remove it from the result.
        if SetRecordHas(OtherRecord, Snapshot[I]) then
          RemoveSetItem(ResultSet, Snapshot[I]);
    end
    else
    begin
      Cursor := 0;
      while ThisSet.NextItem(Cursor, Item) do
        ResultSet.AddItem(Item);

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
  NextValue, Item: TGocciaValue;
  Done, WasResultRooted, WasIteratorRooted: Boolean;
  Cursor: Integer;
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
    Cursor := 0;
    while ThisSet.NextItem(Cursor, Item) do
      ResultSet.AddItem(Item);

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
  Item: TGocciaValue;
  Cursor, Limit: Integer;
  IsSubset: Boolean;
begin
  // Step 1: Let O be the this value
  // Steps 2-3: If O does not have a [[SetData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaSetValue) then
    ThrowTypeError(SErrorSetIsSubsetOfNonSet, SSuggestSetThisType);
  ThisSet := TGocciaSetValue(AThisValue);
  // Step 4: Let otherRec be GetSetRecord(other)
  OtherRecord := GetSetRecord(AArgs.GetElement(0), 'isSubsetOf');

  // Step 5: If SetDataSize(O) > otherRec.[[Size]], return false (optimization)
  if ThisSet.Count > OtherRecord.Size then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  IsSubset := True;
  // Step 6: For each element e of O.[[SetData]] present when the operation
  // began. SetRecordHas runs user code that may mutate O; retain the store and
  // bound iteration to the original slot count so appended members are not
  // checked, deleted members are skipped, and a delete-then-readd lands past
  // the bound (§24.2.3.8).
  Limit := ThisSet.EntrySlotCount;
  ThisSet.RetainIterator;
  try
    Cursor := 0;
    while ThisSet.NextItemBounded(Cursor, Limit, Item) do
      // Step 6a: If SetDataHas(otherRec, e) is false, return false
      if not SetRecordHas(OtherRecord, Item) then
      begin
        IsSubset := False;
        Break;
      end;
  finally
    ThisSet.ReleaseIterator;
  end;

  // Step 7: Return true
  if IsSubset then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
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
  if ThisSet.Count < OtherRecord.Size then
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
  NextValue, Item: TGocciaValue;
  Done, WasIteratorRooted, IsDisjoint: Boolean;
  Cursor, Limit: Integer;
  GC: TGarbageCollector;
begin
  // Step 1: Let O be the this value
  // Steps 2-3: If O does not have a [[SetData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaSetValue) then
    ThrowTypeError(SErrorSetIsDisjointFromNonSet, SSuggestSetThisType);
  ThisSet := TGocciaSetValue(AThisValue);
  // Step 4: Let otherRec be GetSetRecord(other)
  OtherRecord := GetSetRecord(AArgs.GetElement(0), 'isDisjointFrom');

  if ThisSet.Count <= OtherRecord.Size then
  begin
    IsDisjoint := True;
    // Step 5: For each element e of O.[[SetData]] present when the operation
    // began. SetRecordHas runs user code that may mutate O; retain the store and
    // bound iteration to the original slot count so appended elements are not
    // checked, deleted elements are skipped, and a delete-then-readd lands past
    // the bound (§24.2.3.6).
    Limit := ThisSet.EntrySlotCount;
    ThisSet.RetainIterator;
    try
      Cursor := 0;
      while ThisSet.NextItemBounded(Cursor, Limit, Item) do
        // Step 5a: If SetDataHas(otherRec, e) is true, return false
        if SetRecordHas(OtherRecord, Item) then
        begin
          IsDisjoint := False;
          Break;
        end;
    finally
      ThisSet.ReleaseIterator;
    end;
    if not IsDisjoint then
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
