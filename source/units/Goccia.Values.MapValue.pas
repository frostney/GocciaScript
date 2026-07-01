unit Goccia.Values.MapValue;

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
  TGocciaMapValue = class(TGocciaInstanceValue)
  private
    FStore: TGocciaOrderedValueMap;
  public
    function MapGet(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MapSet(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MapHas(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MapDelete(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MapClear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MapForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MapKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MapValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MapEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MapSymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MapGetOrInsert(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MapGetOrInsertComputed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure InitializePrototype;
  public
    constructor Create(const AClass: TGocciaClassValue = nil);
    destructor Destroy; override;

    // Insert or in-place update (keeps insertion position), with -0 -> +0
    // key canonicalization.
    procedure SetEntry(const AKey, AValue: TGocciaValue);
    // SameValueZero lookup. Returns False (and AValue undefined) when absent.
    function TryGetValue(const AKey: TGocciaValue; out AValue: TGocciaValue): Boolean;

    // Live, insertion-ordered cursor used by the Map iterators / forEach.
    // Seed ACursor with 0; new entries appended mid-iteration are visited and
    // deleted entries are skipped. Bracket external iteration with
    // RetainIterator / ReleaseIterator so compaction cannot renumber entries.
    function NextEntry(var ACursor: Integer; out AKey, AValue: TGocciaValue): Boolean;
    procedure RetainIterator;
    procedure ReleaseIterator;
    function Count: Integer;

    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;
    function ToArray: TGocciaArrayValue;
    function ToStringTag: string; override;

    procedure InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); override;
    procedure MarkReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);
  end;

implementation

uses
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
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

// Map.prototype lives in a per-realm owned slot; its member definitions are
// rebuilt per realm (bound to that realm's host), not cached cross-realm.
var
  GMapSharedSlot: TGocciaRealmOwnedSlotId;

function GetMapShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GMapSharedSlot))
  else
    Result := nil;
end;

constructor TGocciaMapValue.Create(const AClass: TGocciaClassValue = nil);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(AClass);
  FStore := TGocciaOrderedValueMap.Create;
  InitializePrototype;
  Shared := GetMapShared;
  if not Assigned(AClass) and Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

procedure TGocciaMapValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetMapShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GMapSharedSlot, Shared);
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod('get', MapGet, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('set', MapSet, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('has', MapHas, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('delete', MapDelete, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('clear', MapClear, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('forEach', MapForEach, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('keys', MapKeys, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('values', MapValues, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('entries', MapEntries, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('getOrInsert', MapGetOrInsert, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('getOrInsertComputed', MapGetOrInsertComputed, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddSymbolMethod(
      TGocciaSymbolValue.WellKnownIterator,
      '[Symbol.iterator]',
      MapSymbolIterator,
      0,
      [pfConfigurable, pfWritable]);
    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create(CONSTRUCTOR_MAP),
      [pfConfigurable]);
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Shared.Prototype, PrototypeMembers);
end;

class procedure TGocciaMapValue.ExposePrototype(const AConstructor: TGocciaValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetMapShared;
  if not Assigned(Shared) then
  begin
    TGocciaMapValue.Create;
    Shared := GetMapShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;


destructor TGocciaMapValue.Destroy;
begin
  FStore.Free;
  inherited;
end;

procedure TGocciaMapValue.InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection);
var
  InitArg: TGocciaValue;
  ArrValue: TGocciaArrayValue;
  EntryArr: TGocciaArrayValue;
  EntryObj: TGocciaObjectValue;
  I: Integer;
begin
  if AArguments.Length = 0 then
    Exit;
  InitArg := AArguments.GetElement(0);
  if InitArg is TGocciaArrayValue then
  begin
    ArrValue := TGocciaArrayValue(InitArg);
    for I := 0 to ArrValue.Elements.Count - 1 do
    begin
      if Assigned(ArrValue.Elements[I]) and (ArrValue.Elements[I] is TGocciaArrayValue) then
      begin
        EntryArr := TGocciaArrayValue(ArrValue.Elements[I]);
        if EntryArr.Elements.Count >= 2 then
          SetEntry(EntryArr.Elements[0], EntryArr.Elements[1]);
      end
      else if Assigned(ArrValue.Elements[I]) and
              (ArrValue.Elements[I] is TGocciaObjectValue) then
      begin
        EntryObj := TGocciaObjectValue(ArrValue.Elements[I]);
        SetEntry(EntryObj.GetProperty('0'), EntryObj.GetProperty('1'));
      end;
    end;
  end;
end;

procedure TGocciaMapValue.MarkReferences;
var
  Cursor: Integer;
  Key, Value: TGocciaValue;
begin
  if GCMarked then Exit;
  inherited;

  Cursor := 0;
  while FStore.NextEntry(Cursor, Key, Value) do
  begin
    if Assigned(Key) then
      Key.MarkReferences;
    if Assigned(Value) then
      Value.MarkReferences;
  end;
end;

procedure TGocciaMapValue.SetEntry(const AKey, AValue: TGocciaValue);
begin
  FStore.SetEntry(AKey, AValue);
end;

function TGocciaMapValue.TryGetValue(const AKey: TGocciaValue;
  out AValue: TGocciaValue): Boolean;
begin
  Result := FStore.TryGetValue(AKey, AValue);
end;

function TGocciaMapValue.NextEntry(var ACursor: Integer;
  out AKey, AValue: TGocciaValue): Boolean;
begin
  Result := FStore.NextEntry(ACursor, AKey, AValue);
end;

procedure TGocciaMapValue.RetainIterator;
begin
  FStore.RetainIterator;
end;

procedure TGocciaMapValue.ReleaseIterator;
begin
  FStore.ReleaseIterator;
end;

function TGocciaMapValue.Count: Integer;
begin
  Result := FStore.Count;
end;

function TGocciaMapValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaMapValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
begin
  if AName = PROP_SIZE then
    Result := TGocciaNumberLiteralValue.Create(FStore.Count)
  else
    Result := inherited GetPropertyWithContext(AName, AThisContext);
end;

function TGocciaMapValue.ToArray: TGocciaArrayValue;
var
  Cursor: Integer;
  Key, Value: TGocciaValue;
  EntryArr: TGocciaArrayValue;
begin
  Result := TGocciaArrayValue.Create;
  Cursor := 0;
  while FStore.NextEntry(Cursor, Key, Value) do
  begin
    EntryArr := TGocciaArrayValue.Create;
    EntryArr.Elements.Add(Key);
    EntryArr.Elements.Add(Value);
    Result.Elements.Add(EntryArr);
  end;
end;

function TGocciaMapValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_MAP;
end;

{ Instance methods }

// ES2026 §24.1.3.6 Map.prototype.get(key)
function TGocciaMapValue.MapGet(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
  Value: TGocciaValue;
begin
  // Step 1: Let M be the this value
  // Steps 2-3: If M does not have a [[MapData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaMapValue) then
    ThrowTypeError(SErrorMapGetNonMap, SSuggestMapThisType);
  M := TGocciaMapValue(AThisValue);
  // Step 4: For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
  // Step 4a: If p.[[Key]] is not empty and SameValueZero(p.[[Key]], key) is true, return p.[[Value]]
  // An omitted argument is the value `undefined` (GetElement yields undefined).
  if M.TryGetValue(AArgs.GetElement(0), Value) then
    Result := Value
  else
    // Step 5: Return undefined
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §24.1.3.9 Map.prototype.set(key, value)
function TGocciaMapValue.MapSet(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
  MapKey, MapValue: TGocciaValue;
begin
  // Step 1: Let M be the this value
  // Steps 2-3: If M does not have a [[MapData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaMapValue) then
    ThrowTypeError(SErrorMapSetNonMap, SSuggestMapThisType);
  M := TGocciaMapValue(AThisValue);
  // Omitted arguments are the value `undefined` (GetElement yields undefined).
  MapKey := AArgs.GetElement(0);
  MapValue := AArgs.GetElement(1);
  // Step 4: For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
  //   If p.[[Key]] is not empty and SameValueZero(p.[[Key]], key) is true,
  //   set p.[[Value]] to value and return M
  // Step 5: Set key to CanonicalizeKeyedCollectionKey(key) (-0 -> +0)
  // Step 6: Let p be the Record { [[Key]]: key, [[Value]]: value }
  // Step 7: Append p to M.[[MapData]]
  M.SetEntry(MapKey, MapValue);
  // Step 8: Return M
  Result := AThisValue;
end;

// ES2026 §24.1.3.7 Map.prototype.has(key)
function TGocciaMapValue.MapHas(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
  Value: TGocciaValue;
begin
  // Step 1: Let M be the this value
  // Steps 2-3: If M does not have a [[MapData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaMapValue) then
    ThrowTypeError(SErrorMapHasNonMap, SSuggestMapThisType);
  M := TGocciaMapValue(AThisValue);
  // Step 4: For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
  //   If p.[[Key]] is not empty and SameValueZero(p.[[Key]], key) is true, return true
  // An omitted argument is the value `undefined` (GetElement yields undefined).
  if M.TryGetValue(AArgs.GetElement(0), Value) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    // Step 5: Return false
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §24.1.3.3 Map.prototype.delete(key)
function TGocciaMapValue.MapDelete(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
begin
  // Step 1: Let M be the this value
  // Steps 2-3: If M does not have a [[MapData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaMapValue) then
    ThrowTypeError(SErrorMapDeleteNonMap, SSuggestMapThisType);
  M := TGocciaMapValue(AThisValue);
  // Step 4: For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
  //   If SameValueZero(p.[[Key]], key) is true, set p.[[Key]]/[[Value]] to
  //   empty (tombstone) and return true
  // An omitted argument is the value `undefined` (GetElement yields undefined).
  if M.FStore.Remove(AArgs.GetElement(0)) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    // Step 5: Return false
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §24.1.3.1 Map.prototype.clear()
function TGocciaMapValue.MapClear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Let M be the this value
  // Steps 2-3: If M does not have a [[MapData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaMapValue) then
    ThrowTypeError(SErrorMapClearNonMap, SSuggestMapThisType);
  // Step 4: For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
  //   Set p.[[Key]] to empty, set p.[[Value]] to empty
  TGocciaMapValue(AThisValue).FStore.Clear;
  // Step 5: Return undefined
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §24.1.3.5 Map.prototype.forEach(callbackfn [, thisArg])
function TGocciaMapValue.MapForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
  Callback, ThisArg, Key, Value: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  CallArgs: TGocciaArgumentsCollection;
  Cursor: Integer;
  MapRoot, CallbackRoot, ThisRoot: TGocciaTempRoot;
begin
  // Steps 1-3: If M does not have a [[MapData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaMapValue) then
    ThrowTypeError(SErrorMapForEachNonMap, SSuggestMapThisType);
  M := TGocciaMapValue(AThisValue);

  // Step 4: If IsCallable(callbackfn) is false, throw a TypeError
  if AArgs.Length > 0 then
    Callback := AArgs.GetElement(0)
  else
    Callback := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not Callback.IsCallable then
    ThrowTypeError(SErrorMapForEachNotCallable, SSuggestMapCallbackRequired);

  // Step 5: Let thisArg be the second argument
  if AArgs.Length > 1 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;

  TypedCallback := nil;
  if Callback is TGocciaFunctionBase then
    TypedCallback := TGocciaFunctionBase(Callback);

  InitializeTempRoot(MapRoot);
  InitializeTempRoot(CallbackRoot);
  InitializeTempRoot(ThisRoot);
  AddTempRootIfNeeded(MapRoot, M);
  AddTempRootIfNeeded(CallbackRoot, Callback);
  AddTempRootIfNeeded(ThisRoot, ThisArg);
  // Hold compaction off so the cursor's entry indices stay valid even if the
  // callback adds or deletes entries (ES2026 §24.1.3.5: numEntries is re-read
  // each step; appended keys are visited, deleted keys are skipped).
  M.RetainIterator;
  try
    // Step 6: For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
    Cursor := 0;
    while M.NextEntry(Cursor, Key, Value) do
    begin
      // Step 6b: Call(callbackfn, thisArg, « p.[[Value]], p.[[Key]], M »)
      CallArgs := TGocciaArgumentsCollection.Create([Value, Key, M]);
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
    M.ReleaseIterator;
    RemoveTempRootIfNeeded(ThisRoot);
    RemoveTempRootIfNeeded(CallbackRoot);
    RemoveTempRootIfNeeded(MapRoot);
  end;
end;

// ES2026 §24.1.3.8 Map.prototype.keys()
function TGocciaMapValue.MapKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Let M be the this value
  // Steps 2-3: If M does not have a [[MapData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaMapValue) then
    ThrowTypeError(SErrorMapKeysNonMap, SSuggestMapThisType);
  // Step 4: Return CreateMapIterator(M, key)
  Result := TGocciaMapIteratorValue.Create(AThisValue, mkKeys);
end;

// ES2026 §24.1.3.11 Map.prototype.values()
function TGocciaMapValue.MapValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Let M be the this value
  // Steps 2-3: If M does not have a [[MapData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaMapValue) then
    ThrowTypeError(SErrorMapValuesNonMap, SSuggestMapThisType);
  // Step 4: Return CreateMapIterator(M, value)
  Result := TGocciaMapIteratorValue.Create(AThisValue, mkValues);
end;

// ES2026 §24.1.3.4 Map.prototype.entries()
function TGocciaMapValue.MapEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Let M be the this value
  // Steps 2-3: If M does not have a [[MapData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaMapValue) then
    ThrowTypeError(SErrorMapEntriesNonMap, SSuggestMapThisType);
  // Step 4: Return CreateMapIterator(M, key+value)
  Result := TGocciaMapIteratorValue.Create(AThisValue, mkEntries);
end;

// ES2026 §24.1.3.12 Map.prototype[@@iterator]()
function TGocciaMapValue.MapSymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Let M be the this value
  // Steps 2-3: If M does not have a [[MapData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaMapValue) then
    ThrowTypeError(SErrorMapIteratorNonMap, SSuggestMapThisType);
  // Step 4: Return the result of calling Map.prototype.entries()
  Result := TGocciaMapIteratorValue.Create(AThisValue, mkEntries);
end;

// TC39 proposal-upsert §1.1 Map.prototype.getOrInsert(key, value)
function TGocciaMapValue.MapGetOrInsert(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
  MapKey, DefaultValue, Existing: TGocciaValue;
begin
  // Steps 1-3: If M does not have a [[MapData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaMapValue) then
    ThrowTypeError(SErrorMapGetOrInsertNonMap, SSuggestMapThisType);
  M := TGocciaMapValue(AThisValue);
  if AArgs.Length > 0 then
    MapKey := AArgs.GetElement(0)
  else
    MapKey := TGocciaUndefinedLiteralValue.UndefinedValue;

  // Step 4: For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
  //   If SameValueZero(p.[[Key]], key) is true, return p.[[Value]]
  if M.TryGetValue(MapKey, Existing) then
    Exit(Existing);

  if AArgs.Length >= 2 then
    DefaultValue := AArgs.GetElement(1)
  else
    DefaultValue := TGocciaUndefinedLiteralValue.UndefinedValue;
  // Step 5: Set key to CanonicalizeKeyedCollectionKey(key)
  // Step 6: Append Record { [[Key]]: key, [[Value]]: value } to M.[[MapData]]
  M.SetEntry(MapKey, DefaultValue);
  // Step 7: Return value
  Result := DefaultValue;
end;

// TC39 proposal-upsert §1.2 Map.prototype.getOrInsertComputed(key, callbackfn)
function TGocciaMapValue.MapGetOrInsertComputed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
  MapKey, CallbackArg, ComputedValue, Existing: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  MapRoot, KeyRoot, CallbackRoot: TGocciaTempRoot;
begin
  // Steps 1-3: If M does not have a [[MapData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaMapValue) then
    ThrowTypeError(SErrorMapGetOrInsertComputedNonMap, SSuggestMapThisType);
  M := TGocciaMapValue(AThisValue);
  if AArgs.Length > 0 then
    MapKey := AArgs.GetElement(0)
  else
    MapKey := TGocciaUndefinedLiteralValue.UndefinedValue;

  // Step 3: If IsCallable(callbackfn) is false, throw a TypeError
  if AArgs.Length > 1 then
    CallbackArg := AArgs.GetElement(1)
  else
    CallbackArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not CallbackArg.IsCallable then
    ThrowTypeError(SErrorMapGetOrInsertComputedNotCallable, SSuggestMapCallbackRequired);

  // Step 4: For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
  //   If SameValueZero(p.[[Key]], key) is true, return p.[[Value]]
  if M.TryGetValue(MapKey, Existing) then
    Exit(Existing);

  InitializeTempRoot(MapRoot);
  InitializeTempRoot(KeyRoot);
  InitializeTempRoot(CallbackRoot);
  AddTempRootIfNeeded(MapRoot, M);
  AddTempRootIfNeeded(KeyRoot, MapKey);
  AddTempRootIfNeeded(CallbackRoot, CallbackArg);
  try
    CallArgs := TGocciaArgumentsCollection.Create([MapKey]);
    try
      // Step 5: Let value be ? Call(callbackfn, undefined, « key »)
      ComputedValue := InvokeCallable(CallbackArg, CallArgs,
        TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      CallArgs.Free;
    end;

    // Step 6: Set key to CanonicalizeKeyedCollectionKey(key)
    // Step 7: Append Record { [[Key]]: key, [[Value]]: value } to M.[[MapData]]
    M.SetEntry(MapKey, ComputedValue);
    // Step 8: Return value
    Result := ComputedValue;
  finally
    RemoveTempRootIfNeeded(CallbackRoot);
    RemoveTempRootIfNeeded(KeyRoot);
    RemoveTempRootIfNeeded(MapRoot);
  end;
end;

initialization
  GMapSharedSlot := RegisterRealmOwnedSlot('Map.shared');

end.
