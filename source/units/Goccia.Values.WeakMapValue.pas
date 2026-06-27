unit Goccia.Values.WeakMapValue;

{$I Goccia.inc}

interface

uses
  HashMap,

  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaWeakMapStorage = THashMap<TGocciaValue, TGocciaValue>;

  TGocciaWeakMapValue = class(TGocciaInstanceValue)
  private
    FEntries: TGocciaWeakMapStorage;
    procedure InitializePrototype;
  public
    function WeakMapDelete(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function WeakMapGet(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function WeakMapGetOrInsert(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function WeakMapGetOrInsertComputed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function WeakMapHas(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function WeakMapSet(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    constructor Create(const AClass: TGocciaClassValue = nil);
    destructor Destroy; override;

    function TryGetEntry(const AKey: TGocciaValue; out AValue: TGocciaValue): Boolean;
    procedure SetEntry(const AKey, AValue: TGocciaValue);
    function DeleteEntry(const AKey: TGocciaValue): Boolean;
    function HasEntry(const AKey: TGocciaValue): Boolean;

    function ToStringTag: string; override;

    procedure InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); override;
    procedure MarkReferences; override;
    function TraceWeakReferences: Boolean; override;
    procedure SweepWeakReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);

    property Entries: TGocciaWeakMapStorage read FEntries;
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
  Goccia.ThreadCleanupRegistry,
  Goccia.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.IteratorSupport,
  Goccia.Values.IteratorValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.WeakReferenceSupport;

var
  GWeakMapSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

procedure ClearThreadvarMembers;
begin
  SetLength(FPrototypeMembers, 0);
end;

function GetWeakMapShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GWeakMapSharedSlot))
  else
    Result := nil;
end;

constructor TGocciaWeakMapValue.Create(const AClass: TGocciaClassValue);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(AClass);
  FEntries := TGocciaWeakMapStorage.Create;
  InitializePrototype;
  Shared := GetWeakMapShared;
  if not Assigned(AClass) and Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

destructor TGocciaWeakMapValue.Destroy;
begin
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.UnregisterWeakContainer(Self);
  FEntries.Free;
  inherited;
end;

procedure TGocciaWeakMapValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetWeakMapShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GWeakMapSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      // Built-in prototype methods are not constructors per ES spec.
      Members.AddNamedMethod('delete', WeakMapDelete, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype, gmfNotConstructable]);
      Members.AddNamedMethod('get', WeakMapGet, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype, gmfNotConstructable]);
      Members.AddNamedMethod('getOrInsert', WeakMapGetOrInsert, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype, gmfNotConstructable]);
      Members.AddNamedMethod('getOrInsertComputed', WeakMapGetOrInsertComputed, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype, gmfNotConstructable]);
      Members.AddNamedMethod('has', WeakMapHas, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype, gmfNotConstructable]);
      Members.AddNamedMethod('set', WeakMapSet, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype, gmfNotConstructable]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create(CONSTRUCTOR_WEAK_MAP),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaWeakMapValue.ExposePrototype(
  const AConstructor: TGocciaValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetWeakMapShared;
  if not Assigned(Shared) then
  begin
    TGocciaWeakMapValue.Create;
    Shared := GetWeakMapShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaWeakMapValue.TryGetEntry(const AKey: TGocciaValue;
  out AValue: TGocciaValue): Boolean;
begin
  Result := FEntries.TryGetValue(AKey, AValue);
end;

procedure TGocciaWeakMapValue.SetEntry(const AKey, AValue: TGocciaValue);
begin
  // Register as a weak container on the first entry so the GC runs its weak
  // passes only for code that uses weak collections; a never-populated WeakMap
  // (e.g. the pinned prototype host) stays unregistered. Registration lasts
  // until destruction, so a later-emptied map stays registered.
  if (FEntries.Count = 0) and Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.RegisterWeakContainer(Self);
  FEntries.AddOrSetValue(AKey, AValue);
end;

function TGocciaWeakMapValue.DeleteEntry(const AKey: TGocciaValue): Boolean;
begin
  Result := FEntries.Remove(AKey);
end;

function TGocciaWeakMapValue.HasEntry(const AKey: TGocciaValue): Boolean;
begin
  Result := FEntries.ContainsKey(AKey);
end;

function TGocciaWeakMapValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_WEAK_MAP;
end;

procedure TGocciaWeakMapValue.InitializeNativeFromArguments(
  const AArguments: TGocciaArgumentsCollection);
var
  InitArg, Adder, NextValue, Key, Value: TGocciaValue;
  Iterator: TGocciaIteratorValue;
  CallArgs: TGocciaArgumentsCollection;
  Done: Boolean;
  GC: TGarbageCollector;
  WasSelfRooted, WasInitRooted, WasAdderRooted, WasIteratorRooted: Boolean;
  WasNextRooted, WasKeyRooted, WasValueRooted: Boolean;

  function AddRootIfNeeded(const AValue: TGocciaValue): Boolean;
  begin
    Result := Assigned(GC) and Assigned(AValue) and not GC.IsTempRoot(AValue);
    if Result then
      GC.AddTempRoot(AValue);
  end;

  procedure RemoveRootIfNeeded(const AValue: TGocciaValue; const AWasAdded: Boolean);
  begin
    if AWasAdded then
      GC.RemoveTempRoot(AValue);
  end;

begin
  if AArguments.Length = 0 then
    Exit;
  InitArg := AArguments.GetElement(0);
  if (InitArg is TGocciaUndefinedLiteralValue) or
     (InitArg is TGocciaNullLiteralValue) then
    Exit;

  GC := TGarbageCollector.Instance;
  WasSelfRooted := AddRootIfNeeded(Self);
  WasInitRooted := AddRootIfNeeded(InitArg);
  try
    Adder := GetProperty(PROP_SET);
    if not Assigned(Adder) or not Adder.IsCallable then
      ThrowTypeError(Format(SErrorValueNotFunction, [PROP_SET]), SSuggestWeakMapThisType);

    WasAdderRooted := AddRootIfNeeded(Adder);
    try
      Iterator := GetIteratorFromValue(InitArg);
      if not Assigned(Iterator) then
        ThrowTypeError(Format(SErrorWeakCollectionConstructorNotIterable, [CONSTRUCTOR_WEAK_MAP]), SSuggestNotIterable);

      WasIteratorRooted := AddRootIfNeeded(Iterator);
      try
        try
          NextValue := Iterator.DirectNext(Done);
          while not Done do
          begin
            WasNextRooted := AddRootIfNeeded(NextValue);
            try
              if not (NextValue is TGocciaObjectValue) then
                ThrowTypeError(SErrorWeakMapConstructorEntryNotObject, SSuggestIteratorProtocol);

              Key := TGocciaObjectValue(NextValue).GetProperty('0');
              WasKeyRooted := AddRootIfNeeded(Key);
              try
                Value := TGocciaObjectValue(NextValue).GetProperty('1');
                WasValueRooted := AddRootIfNeeded(Value);
                try
                  CallArgs := TGocciaArgumentsCollection.Create([Key, Value]);
                  try
                    InvokeCallable(Adder, CallArgs, Self);
                  finally
                    CallArgs.Free;
                  end;
                finally
                  RemoveRootIfNeeded(Value, WasValueRooted);
                end;
              finally
                RemoveRootIfNeeded(Key, WasKeyRooted);
              end;
            finally
              RemoveRootIfNeeded(NextValue, WasNextRooted);
            end;
            NextValue := Iterator.DirectNext(Done);
          end;
        except
          AcquireExceptionObject;
          CloseIteratorPreservingError(Iterator);
          raise;
        end;
      finally
        RemoveRootIfNeeded(Iterator, WasIteratorRooted);
      end;
    finally
      RemoveRootIfNeeded(Adder, WasAdderRooted);
    end;
  finally
    RemoveRootIfNeeded(InitArg, WasInitRooted);
    RemoveRootIfNeeded(Self, WasSelfRooted);
  end;
end;

procedure TGocciaWeakMapValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
end;

// ES2026 §9.9.3 WeakRef Execution: a WeakMap entry's value is live only while
// its key is live. Marking a value can make it the key of another entry in the
// same map (an ephemeron chain), so the live set must be propagated to a
// fixpoint. A single full-slot scan per GC pass would make a chain of length N
// cost O(N^2) (one link resolved per pass). Resolving the chain with a worklist
// keyed by object identity makes the whole intra-map trace O(entries): the
// initial scan seeds directly-reachable values, then each newly marked value is
// looked up as a key in O(1) to extend the chain. Cross-map links are still
// driven by the collector's outer trace loop.
function TGocciaWeakMapValue.TraceWeakReferences: Boolean;
var
  Pair: TGocciaWeakMapStorage.TKeyValuePair;
  WorkList: array of TGocciaValue;
  WorkCount: Integer;
  Current, NextValue: TGocciaValue;

  procedure MarkValue(const AValue: TGocciaValue);
  begin
    if not Assigned(AValue) or AValue.GCMarked then
      Exit;
    AValue.MarkReferences;
    Result := True;
    if WorkCount = Length(WorkList) then
      SetLength(WorkList, (WorkCount * 2) + 1);
    WorkList[WorkCount] := AValue;
    Inc(WorkCount);
  end;

begin
  Result := False;
  WorkCount := 0;

  // Seed the worklist with the values of all entries whose key is currently live.
  for Pair in FEntries do
    if Assigned(Pair.Key) and Pair.Key.GCMarked then
      MarkValue(Pair.Value);

  // Propagate: a freshly marked value may itself be a live key in this map,
  // keeping the value of that entry live in turn.
  while WorkCount > 0 do
  begin
    Dec(WorkCount);
    Current := WorkList[WorkCount];
    if FEntries.TryGetValue(Current, NextValue) then
      MarkValue(NextValue);
  end;
end;

procedure TGocciaWeakMapValue.SweepWeakReferences;
var
  Keys: TGocciaWeakMapStorage.TKeyArray;
  I: Integer;
begin
  Keys := FEntries.Keys;
  for I := 0 to Length(Keys) - 1 do
    if Assigned(Keys[I]) and not Keys[I].GCMarked then
      FEntries.Remove(Keys[I]);
end;

function TGocciaWeakMapValue.WeakMapDelete(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaWeakMapValue;
  Key: TGocciaValue;
begin
  if not (AThisValue is TGocciaWeakMapValue) then
    ThrowTypeError(SErrorWeakMapDeleteNonWeakMap, SSuggestWeakMapThisType);
  M := TGocciaWeakMapValue(AThisValue);
  Key := AArgs.GetElement(0);
  if not CanBeHeldWeakly(Key) then
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  if M.DeleteEntry(Key) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaWeakMapValue.WeakMapGet(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaWeakMapValue;
  Key, Value: TGocciaValue;
begin
  if not (AThisValue is TGocciaWeakMapValue) then
    ThrowTypeError(SErrorWeakMapGetNonWeakMap, SSuggestWeakMapThisType);
  M := TGocciaWeakMapValue(AThisValue);
  Key := AArgs.GetElement(0);
  if CanBeHeldWeakly(Key) and M.TryGetEntry(Key, Value) then
    Result := Value
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaWeakMapValue.WeakMapGetOrInsert(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaWeakMapValue;
  Key, DefaultValue, Existing: TGocciaValue;
begin
  if not (AThisValue is TGocciaWeakMapValue) then
    ThrowTypeError(SErrorWeakMapGetOrInsertNonWeakMap, SSuggestWeakMapThisType);
  M := TGocciaWeakMapValue(AThisValue);
  Key := AArgs.GetElement(0);
  RequireCanBeHeldWeakly(Key, 'WeakMap.prototype.getOrInsert');
  if M.TryGetEntry(Key, Existing) then
    Exit(Existing);
  DefaultValue := AArgs.GetElement(1);
  M.SetEntry(Key, DefaultValue);
  Result := DefaultValue;
end;

function TGocciaWeakMapValue.WeakMapGetOrInsertComputed(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaWeakMapValue;
  Key, CallbackArg, Existing, ComputedValue: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  GC: TGarbageCollector;
  WasMapRooted, WasKeyRooted: Boolean;
begin
  if not (AThisValue is TGocciaWeakMapValue) then
    ThrowTypeError(SErrorWeakMapGetOrInsertComputedNonWeakMap, SSuggestWeakMapThisType);
  M := TGocciaWeakMapValue(AThisValue);
  Key := AArgs.GetElement(0);
  RequireCanBeHeldWeakly(Key, 'WeakMap.prototype.getOrInsertComputed');
  CallbackArg := AArgs.GetElement(1);
  if not CallbackArg.IsCallable then
    ThrowTypeError(SErrorWeakMapGetOrInsertComputedNotCallable, SSuggestMapCallbackRequired);
  if M.TryGetEntry(Key, Existing) then
    Exit(Existing);

  GC := TGarbageCollector.Instance;
  WasMapRooted := Assigned(GC) and not GC.IsTempRoot(M);
  if WasMapRooted then
    GC.AddTempRoot(M);
  WasKeyRooted := Assigned(GC) and not GC.IsTempRoot(Key);
  if WasKeyRooted then
    GC.AddTempRoot(Key);
  try
    CallArgs := TGocciaArgumentsCollection.Create([Key]);
    try
      ComputedValue := InvokeCallable(CallbackArg, CallArgs,
        TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      CallArgs.Free;
    end;
    M.SetEntry(Key, ComputedValue);
    Result := ComputedValue;
  finally
    if WasKeyRooted then
      GC.RemoveTempRoot(Key);
    if WasMapRooted then
      GC.RemoveTempRoot(M);
  end;
end;

function TGocciaWeakMapValue.WeakMapHas(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaWeakMapValue;
  Key: TGocciaValue;
begin
  if not (AThisValue is TGocciaWeakMapValue) then
    ThrowTypeError(SErrorWeakMapHasNonWeakMap, SSuggestWeakMapThisType);
  M := TGocciaWeakMapValue(AThisValue);
  Key := AArgs.GetElement(0);
  if CanBeHeldWeakly(Key) and M.HasEntry(Key) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaWeakMapValue.WeakMapSet(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaWeakMapValue;
  Key, Value: TGocciaValue;
begin
  if not (AThisValue is TGocciaWeakMapValue) then
    ThrowTypeError(SErrorWeakMapSetNonWeakMap, SSuggestWeakMapThisType);
  M := TGocciaWeakMapValue(AThisValue);
  Key := AArgs.GetElement(0);
  RequireCanBeHeldWeakly(Key, 'WeakMap.prototype.set');
  Value := AArgs.GetElement(1);
  M.SetEntry(Key, Value);
  Result := M;
end;

initialization
  RegisterThreadvarCleanup(@ClearThreadvarMembers);
  GWeakMapSharedSlot := RegisterRealmOwnedSlot('WeakMap.shared');

end.
