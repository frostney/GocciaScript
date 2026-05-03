unit Goccia.Values.MapValue;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.GarbageCollector,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaMapEntry = record
    Key: TGocciaValue;
    Value: TGocciaValue;
  end;

  TGocciaMapValue = class(TGocciaInstanceValue)
  private
    FEntries: TList<TGocciaMapEntry>;
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
    function FindEntry(const AKey: TGocciaValue): Integer;
    constructor Create(const AClass: TGocciaClassValue = nil);
    destructor Destroy; override;

    procedure SetEntry(const AKey, AValue: TGocciaValue);

    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;
    function ToArray: TGocciaArrayValue;
    function ToStringTag: string; override;

    procedure InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); override;
    procedure MarkReferences; override;
    function MarkEscapedReferencesIn(const AVisited: TGCObjectSet): Boolean; override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);

    property Entries: TList<TGocciaMapEntry> read FEntries;
  end;

implementation

uses
  Goccia.Arithmetic,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Realm,
  Goccia.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

// Map.prototype lives in a per-realm owned slot.  Member definitions stay
// process-wide (immutable across realms).
var
  GMapSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

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
  FEntries := TList<TGocciaMapEntry>.Create;
  InitializePrototype;
  Shared := GetMapShared;
  if not Assigned(AClass) and Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

procedure TGocciaMapValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetMapShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GMapSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
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
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
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
  FEntries.Free;
  inherited;
end;

procedure TGocciaMapValue.InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection);
var
  InitArg: TGocciaValue;
  ArrValue: TGocciaArrayValue;
  EntryArr: TGocciaArrayValue;
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
      end;
    end;
  end;
end;

procedure TGocciaMapValue.MarkReferences;
var
  I: Integer;
begin
  if GCMarked then Exit;
  inherited;

  for I := 0 to FEntries.Count - 1 do
  begin
    if Assigned(FEntries[I].Key) then
      FEntries[I].Key.MarkReferences;
    if Assigned(FEntries[I].Value) then
      FEntries[I].Value.MarkReferences;
  end;
end;

function TGocciaMapValue.MarkEscapedReferencesIn(
  const AVisited: TGCObjectSet): Boolean;
var
  I: Integer;
begin
  Result := inherited MarkEscapedReferencesIn(AVisited);
  if not Result then
    Exit;

  for I := 0 to FEntries.Count - 1 do
  begin
    if Assigned(FEntries[I].Key) then
      FEntries[I].Key.MarkEscapedReferencesIn(AVisited);
    if Assigned(FEntries[I].Value) then
      FEntries[I].Value.MarkEscapedReferencesIn(AVisited);
  end;
end;

function TGocciaMapValue.FindEntry(const AKey: TGocciaValue): Integer;
var
  I: Integer;
begin
  for I := 0 to FEntries.Count - 1 do
  begin
    if IsSameValueZero(FEntries[I].Key, AKey) then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

procedure TGocciaMapValue.SetEntry(const AKey, AValue: TGocciaValue);
var
  Index: Integer;
  Entry: TGocciaMapEntry;
begin
  Index := FindEntry(AKey);
  Entry.Key := AKey;
  Entry.Value := AValue;
  if Assigned(AKey) and AKey.CanContainEscapedReferences then
    AKey.MarkEscapedReferences;
  if Assigned(AValue) and AValue.CanContainEscapedReferences then
    AValue.MarkEscapedReferences;

  if Index >= 0 then
    FEntries[Index] := Entry
  else
    FEntries.Add(Entry);
end;

function TGocciaMapValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaMapValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
begin
  if AName = PROP_SIZE then
    Result := TGocciaNumberLiteralValue.Create(FEntries.Count)
  else
    Result := inherited GetPropertyWithContext(AName, AThisContext);
end;

function TGocciaMapValue.ToArray: TGocciaArrayValue;
var
  I: Integer;
  EntryArr: TGocciaArrayValue;
begin
  Result := TGocciaArrayValue.Create;
  for I := 0 to FEntries.Count - 1 do
  begin
    EntryArr := TGocciaArrayValue.Create;
    EntryArr.Elements.Add(FEntries[I].Key);
    EntryArr.Elements.Add(FEntries[I].Value);
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
  Index: Integer;
begin
  // Step 1: Let M be the this value
  // Steps 2-3: If M does not have a [[MapData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaMapValue) then
    ThrowTypeError(SErrorMapGetNonMap, SSuggestMapThisType);
  M := TGocciaMapValue(AThisValue);
  if AArgs.Length > 0 then
  begin
    // Step 4: For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
    // Step 4a: If p.[[Key]] is not empty and SameValueZero(p.[[Key]], key) is true, return p.[[Value]]
    Index := M.FindEntry(AArgs.GetElement(0));
    if Index >= 0 then
    begin
      Result := M.Entries[Index].Value;
      Exit;
    end;
  end;
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
  if AArgs.Length >= 2 then
  begin
    MapKey := AArgs.GetElement(0);
    MapValue := AArgs.GetElement(1);
    // Step 4: For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
    //   If p.[[Key]] is not empty and SameValueZero(p.[[Key]], key) is true,
    //   set p.[[Value]] to value and return M
    // Step 5: If key is -0, set key to +0
    // Step 6: Let p be the Record { [[Key]]: key, [[Value]]: value }
    // Step 7: Append p to M.[[MapData]]
    M.SetEntry(MapKey, MapValue);
  end;
  // Step 8: Return M
  Result := AThisValue;
end;

// ES2026 §24.1.3.7 Map.prototype.has(key)
function TGocciaMapValue.MapHas(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
begin
  // Step 1: Let M be the this value
  // Steps 2-3: If M does not have a [[MapData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaMapValue) then
    ThrowTypeError(SErrorMapHasNonMap, SSuggestMapThisType);
  M := TGocciaMapValue(AThisValue);
  // Step 4: For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
  //   If p.[[Key]] is not empty and SameValueZero(p.[[Key]], key) is true, return true
  if (AArgs.Length > 0) and (M.FindEntry(AArgs.GetElement(0)) >= 0) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    // Step 5: Return false
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §24.1.3.3 Map.prototype.delete(key)
function TGocciaMapValue.MapDelete(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
  Index: Integer;
begin
  // Step 1: Let M be the this value
  // Steps 2-3: If M does not have a [[MapData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaMapValue) then
    ThrowTypeError(SErrorMapDeleteNonMap, SSuggestMapThisType);
  M := TGocciaMapValue(AThisValue);
  // Step 5 (early): Default return false
  Result := TGocciaBooleanLiteralValue.FalseValue;
  if AArgs.Length > 0 then
  begin
    // Step 4: For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
    Index := M.FindEntry(AArgs.GetElement(0));
    if Index >= 0 then
    begin
      // Step 4a: If p.[[Key]] is not empty and SameValueZero(p.[[Key]], key) is true
      // Step 4a.i: Set p.[[Key]] to empty / Step 4a.ii: Set p.[[Value]] to empty
      M.FEntries.Delete(Index);
      // Step 4a.iii: Return true
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end;
  end;
  // Step 5: Return false
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
  TGocciaMapValue(AThisValue).FEntries.Clear;
  // Step 5: Return undefined
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §24.1.3.5 Map.prototype.forEach(callbackfn [, thisArg])
function TGocciaMapValue.MapForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
  Callback, ThisArg: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
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

  // Step 6: For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
  for I := 0 to M.Entries.Count - 1 do
  begin
    // Step 6b: Call(callbackfn, thisArg, « p.[[Value]], p.[[Key]], M »)
    CallArgs := TGocciaArgumentsCollection.Create([M.Entries[I].Value, M.Entries[I].Key, M]);
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
  MapKey, DefaultValue: TGocciaValue;
  Index: Integer;
begin
  // Steps 1-3: If M does not have a [[MapData]] internal slot, throw a TypeError
  if not (AThisValue is TGocciaMapValue) then
    ThrowTypeError(SErrorMapGetOrInsertNonMap, SSuggestMapThisType);
  M := TGocciaMapValue(AThisValue);
  if AArgs.Length < 2 then
  begin
    if AArgs.Length > 0 then
      MapKey := AArgs.GetElement(0)
    else
      MapKey := TGocciaUndefinedLiteralValue.UndefinedValue;
    Index := M.FindEntry(MapKey);
    if Index >= 0 then
      Exit(M.Entries[Index].Value);
    DefaultValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    M.SetEntry(MapKey, DefaultValue);
    Exit(DefaultValue);
  end;

  MapKey := AArgs.GetElement(0);
  DefaultValue := AArgs.GetElement(1);
  // Step 4: For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
  //   If SameValueZero(p.[[Key]], key) is true, return p.[[Value]]
  Index := M.FindEntry(MapKey);
  if Index >= 0 then
    Exit(M.Entries[Index].Value);
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
  MapKey, CallbackArg, ComputedValue: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  CallArgs: TGocciaArgumentsCollection;
  Index: Integer;
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
  Index := M.FindEntry(MapKey);
  if Index >= 0 then
    Exit(M.Entries[Index].Value);

  // Step 5: Let value be ? Call(callbackfn, undefined, « key »)
  TypedCallback := TGocciaFunctionBase(CallbackArg);
  CallArgs := TGocciaArgumentsCollection.Create([MapKey]);
  try
    ComputedValue := TypedCallback.Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    CallArgs.Free;
  end;

  // Step 6: Set key to CanonicalizeKeyedCollectionKey(key)
  // Step 7: Append Record { [[Key]]: key, [[Value]]: value } to M.[[MapData]]
  M.SetEntry(MapKey, ComputedValue);
  // Step 8: Return value
  Result := ComputedValue;
end;

initialization
  GMapSharedSlot := RegisterRealmOwnedSlot('Map.shared');

end.
