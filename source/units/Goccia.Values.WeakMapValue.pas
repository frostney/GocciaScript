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
      Members.AddNamedMethod('delete', WeakMapDelete, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('get', WeakMapGet, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('getOrInsert', WeakMapGetOrInsert, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('getOrInsertComputed', WeakMapGetOrInsertComputed, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('has', WeakMapHas, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('set', WeakMapSet, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
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
  WasSelfRooted, WasIteratorRooted: Boolean;
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
  try
    Adder := GetProperty(PROP_SET);
    if not Assigned(Adder) or not Adder.IsCallable then
      ThrowTypeError(Format(SErrorValueNotFunction, [PROP_SET]), SSuggestWeakMapThisType);

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
        CloseIteratorPreservingError(Iterator);
        raise;
      end;
    finally
      RemoveRootIfNeeded(Iterator, WasIteratorRooted);
    end;
  finally
    RemoveRootIfNeeded(Self, WasSelfRooted);
  end;
end;

procedure TGocciaWeakMapValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
end;

function TGocciaWeakMapValue.TraceWeakReferences: Boolean;
var
  Pair: TGocciaWeakMapStorage.TKeyValuePair;
  WasMarked: Boolean;
begin
  Result := False;
  for Pair in FEntries do
  begin
    if Assigned(Pair.Key) and Pair.Key.GCMarked and Assigned(Pair.Value) then
    begin
      WasMarked := Pair.Value.GCMarked;
      Pair.Value.MarkReferences;
      if not WasMarked and Pair.Value.GCMarked then
        Result := True;
    end;
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
  GWeakMapSharedSlot := RegisterRealmOwnedSlot('WeakMap.shared');

end.
