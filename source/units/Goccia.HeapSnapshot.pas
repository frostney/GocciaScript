unit Goccia.HeapSnapshot;

{ Heap snapshot prototype: engine-level entry points.

  BuildBuiltinTable numbers the built-ins of a fresh engine by a
  deterministic walk: the global object, the global scope's bindings, the
  realm slots, the realm's shared prototypes and the well-known symbols,
  then breadth first through prototypes and own properties. Each entry is
  named by the first path that reached it (segments separated by
  HEAP_SNAPSHOT_PATH_SEPARATOR), and a snapshot refers to built-ins by those
  names.

  The walk does not materialize lazy members: a lazy global (Intl,
  Temporal, the bootstrap shims) is built only when something reads it, and
  building all of them costs far more than the walk. ExtendBuiltinTable
  walks again before a snapshot, so what the run materialized since is
  numbered too, and the realm slots filled since (an array iterator's
  prototype, for one) are added by name. A restore resolves a name its
  table does not hold by walking the path, which materializes exactly the
  members on it (TGocciaBuiltinPathResolver); a realm slot a path starts
  from is created through its registered materializer. }

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Engine,
  Goccia.GarbageCollector,
  Goccia.HeapSnapshot.Codec;

type
  // Resolves built-in paths a restore-side table does not hold yet by
  // walking them from their root, materializing lazy members on the way.
  TGocciaBuiltinPathResolver = class
  private
    FTable: TGocciaHeapSnapshotBuiltinTable;
    FEngine: TGocciaEngine;
  public
    constructor Create(const ATable: TGocciaHeapSnapshotBuiltinTable;
      const AEngine: TGocciaEngine);
    function Resolve(const APath: string): TGCManagedObject;
  end;

procedure BuildBuiltinTable(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AEngine: TGocciaEngine);
// Walks the table again for members the run materialized since it was
// built, and adds the realm slots filled since and what they reach;
// ALateSlots receives the names of those slots.
procedure ExtendBuiltinTable(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AEngine: TGocciaEngine; const ALateSlots: TStrings);

implementation

uses
  SysUtils,

  Goccia.Builtins.Base,
  Goccia.Realm,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
  Goccia.SharedPrototype,
  Goccia.Values.ClassValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue,
  Goccia.VM;

procedure AddValue(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AValue: TGocciaValue; const APath: string);
begin
  if (AValue is TGocciaObjectValue) or (AValue is TGocciaSymbolValue) then
    ATable.Add(AValue, APath);
end;

function Seg(const APath, ASegment: string): string; inline;
begin
  Result := APath + HEAP_SNAPSHOT_PATH_SEPARATOR + ASegment;
end;

procedure Visit(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AObject: TGCManagedObject; const APath: string);
var
  Obj: TGocciaObjectValue;
  Pair: TGocciaPropertyMap.TKeyValuePair;
  Descriptor: TGocciaPropertyDescriptor;
  Symbols: TArray<TGocciaSymbolValue>;
  SymbolPath: string;
  I: Integer;
begin
  if not (AObject is TGocciaObjectValue) then
    Exit;
  Obj := TGocciaObjectValue(AObject);
  if Obj is TGocciaClassValue then
  begin
    AddValue(ATable, TGocciaClassValue(Obj).GetConstructorPrototype,
      Seg(APath, '[[Prototype]]'));
    AddValue(ATable, TGocciaClassValue(Obj).Prototype,
      Seg(APath, 'prototype'));
  end
  else
    AddValue(ATable, Obj.Prototype, Seg(APath, '[[Prototype]]'));
  // Lazy members stay lazy: only what exists is numbered.
  for Pair in Obj.Properties do
  begin
    Descriptor := Pair.Value;
    if Descriptor is TGocciaLazyPropertyDescriptorData then
      Continue;
    if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      AddValue(ATable, TGocciaPropertyDescriptorAccessor(Descriptor).Getter,
        Seg(APath, '<get ' + Pair.Key + '>'));
      AddValue(ATable, TGocciaPropertyDescriptorAccessor(Descriptor).Setter,
        Seg(APath, '<set ' + Pair.Key + '>'));
    end
    else if Descriptor is TGocciaPropertyDescriptorData then
      AddValue(ATable, TGocciaPropertyDescriptorData(Descriptor).Value,
        Seg(APath, Pair.Key));
  end;
  Symbols := Obj.GetOwnSymbols;
  for I := 0 to High(Symbols) do
  begin
    SymbolPath := '[' + Symbols[I].Description + ']';
    AddValue(ATable, Symbols[I], Seg(APath, SymbolPath + '<key>'));
    Descriptor := Obj.GetOwnSymbolPropertyDescriptor(Symbols[I]);
    if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      AddValue(ATable, TGocciaPropertyDescriptorAccessor(Descriptor).Getter,
        Seg(APath, SymbolPath + '<get>'));
      AddValue(ATable, TGocciaPropertyDescriptorAccessor(Descriptor).Setter,
        Seg(APath, SymbolPath + '<set>'));
    end
    else if Descriptor is TGocciaPropertyDescriptorData then
      AddValue(ATable, TGocciaPropertyDescriptorData(Descriptor).Value,
        Seg(APath, SymbolPath));
  end;
  if Obj is TGocciaNativeFunctionValue then
    AddValue(ATable, TGocciaNativeFunctionValue(Obj).CapturedRoot,
      Seg(APath, '[[CapturedRoot]]'));
end;

procedure WalkFrom(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AStart: Integer);
var
  I: Integer;
begin
  I := AStart;
  while I < ATable.Count do
  begin
    Visit(ATable, ATable.ObjectAt(I), ATable.PathAt(I));
    Inc(I);
  end;
end;

function FindSlotByName(const AName: string): Integer;
var
  I: Integer;
begin
  for I := 0 to RealmSlotCount - 1 do
    if RealmSlotName(I) = AName then
      Exit(I);
  Result := -1;
end;

// Each builtin's own object is a collector root the engine keeps; most are
// never exposed, but they are not the script's state.
procedure AddLiveBuiltinObjects(const ATable: TGocciaHeapSnapshotBuiltinTable);
var
  Builtin: TGocciaBuiltin;
begin
  Builtin := FirstLiveBuiltin;
  while Assigned(Builtin) do
  begin
    AddValue(ATable, Builtin.BuiltinObject, Seg('%builtin', Builtin.Name));
    Builtin := Builtin.NextLive;
  end;
end;

function FindLiveBuiltinObject(const AName: string): TGocciaObjectValue;
var
  Builtin: TGocciaBuiltin;
begin
  Builtin := FirstLiveBuiltin;
  while Assigned(Builtin) do
  begin
    if Builtin.Name = AName then
      Exit(Builtin.BuiltinObject);
    Builtin := Builtin.NextLive;
  end;
  Result := nil;
end;

procedure AddOwnedSlots(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AEngine: TGocciaEngine);
var
  Owned: TObject;
  I: Integer;
begin
  for I := 0 to RealmOwnedSlotCount - 1 do
  begin
    Owned := AEngine.Realm.GetOwnedSlot(I);
    if Owned is TGocciaSharedPrototype then
    begin
      AddValue(ATable, TGocciaSharedPrototype(Owned).Prototype,
        Seg(Seg('%owned', RealmOwnedSlotName(I)), 'prototype'));
      AddValue(ATable, TGocciaSharedPrototype(Owned).MethodHost,
        Seg(Seg('%owned', RealmOwnedSlotName(I)), 'host'));
    end;
  end;
end;

function FindOwnedSlotByName(const AName: string): Integer;
var
  I: Integer;
begin
  for I := 0 to RealmOwnedSlotCount - 1 do
    if RealmOwnedSlotName(I) = AName then
      Exit(I);
  Result := -1;
end;

procedure BuildBuiltinTable(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AEngine: TGocciaEngine);
var
  Realm: TGocciaRealm;
  Names: TGocciaStringArray;
  Binding: TLexicalBinding;
  I: Integer;
begin
  ATable.Clear;
  EnsureVMFunctionObjectIntrinsics;
  Realm := AEngine.Realm;
  ATable.Add(Realm.GlobalObject, 'globalThis');
  ATable.Add(AEngine.GlobalScope, '%globalScope');
  Names := AEngine.GlobalScope.GetOwnBindingNames;
  for I := 0 to High(Names) do
    if AEngine.GlobalScope.TryGetOwnLexicalBindingRaw(Names[I], Binding) and
       Binding.Initialized then
      AddValue(ATable, Binding.Value, Seg('%scope', Names[I]));
  for I := 0 to RealmSlotCount - 1 do
    if Assigned(Realm.GetSlot(I)) then
      ATable.Add(Realm.GetSlot(I), Seg('%slot', RealmSlotName(I)))
    else
      ATable.EmptySlots.Add(I);
  AddOwnedSlots(ATable, AEngine);
  AddLiveBuiltinObjects(ATable);
  AddValue(ATable, TGocciaSymbolValue.WellKnownIterator, '@@iterator');
  AddValue(ATable, TGocciaSymbolValue.WellKnownMatch, '@@match');
  AddValue(ATable, TGocciaSymbolValue.WellKnownMatchAll, '@@matchAll');
  AddValue(ATable, TGocciaSymbolValue.WellKnownReplace, '@@replace');
  AddValue(ATable, TGocciaSymbolValue.WellKnownSearch, '@@search');
  AddValue(ATable, TGocciaSymbolValue.WellKnownSplit, '@@split');
  AddValue(ATable, TGocciaSymbolValue.WellKnownSpecies, '@@species');
  AddValue(ATable, TGocciaSymbolValue.WellKnownHasInstance, '@@hasInstance');
  AddValue(ATable, TGocciaSymbolValue.WellKnownToPrimitive, '@@toPrimitive');
  AddValue(ATable, TGocciaSymbolValue.WellKnownToStringTag, '@@toStringTag');
  AddValue(ATable, TGocciaSymbolValue.WellKnownIsConcatSpreadable,
    '@@isConcatSpreadable');
  AddValue(ATable, TGocciaSymbolValue.WellKnownAsyncIterator,
    '@@asyncIterator');
  AddValue(ATable, TGocciaSymbolValue.WellKnownMetadata, '@@metadata');
  AddValue(ATable, TGocciaSymbolValue.WellKnownDispose, '@@dispose');
  AddValue(ATable, TGocciaSymbolValue.WellKnownAsyncDispose,
    '@@asyncDispose');
  AddValue(ATable, TGocciaSymbolValue.WellKnownUnscopables, '@@unscopables');
  AddValue(ATable, TGocciaSymbolValue.WellKnownCustomMatcher,
    '@@customMatcher');
  WalkFrom(ATable, 0);
  ATable.RecordFingerprints;
end;

procedure ExtendBuiltinTable(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AEngine: TGocciaEngine; const ALateSlots: TStrings);
var
  I, Slot, Start: Integer;
begin
  // Every entry again first, so what a materialized global reaches is named
  // by its path from globalThis: members materialized since the table was
  // built.
  WalkFrom(ATable, 0);
  Start := ATable.Count;
  for I := 0 to ATable.EmptySlots.Count - 1 do
  begin
    Slot := ATable.EmptySlots[I];
    if Assigned(AEngine.Realm.GetSlot(Slot)) and
       ATable.Add(AEngine.Realm.GetSlot(Slot),
         Seg('%slot', RealmSlotName(Slot))) then
      ALateSlots.Add(RealmSlotName(Slot));
  end;
  AddOwnedSlots(ATable, AEngine);
  AddLiveBuiltinObjects(ATable);
  WalkFrom(ATable, Start);
end;

{ TGocciaBuiltinPathResolver }

constructor TGocciaBuiltinPathResolver.Create(
  const ATable: TGocciaHeapSnapshotBuiltinTable; const AEngine: TGocciaEngine);
begin
  inherited Create;
  FTable := ATable;
  FEngine := AEngine;
end;

function ResolveRoot(const AEngine: TGocciaEngine; const ARoot,
  AName, AWhich: string; out AConsumed: Integer): TGCManagedObject;
var
  Slot: Integer;
  Binding: TLexicalBinding;
  Materializer: TGocciaSnapshotSlotMaterializer;
  Owned: TObject;
begin
  Result := nil;
  AConsumed := 1;
  if ARoot = '%owned' then
  begin
    AConsumed := 3;
    Slot := FindOwnedSlotByName(AName);
    if Slot < 0 then
      Exit(nil);
    Owned := AEngine.Realm.GetOwnedSlot(Slot);
    if not (Owned is TGocciaSharedPrototype) then
      Exit(nil);
    if AWhich = 'prototype' then
      Exit(TGocciaSharedPrototype(Owned).Prototype);
    if AWhich = 'host' then
      Exit(TGocciaSharedPrototype(Owned).MethodHost);
    Exit(nil);
  end;
  if ARoot = 'globalThis' then
    Exit(AEngine.Realm.GlobalObject);
  if ARoot = '%builtin' then
  begin
    AConsumed := 2;
    Exit(FindLiveBuiltinObject(AName));
  end;
  if ARoot = '%globalScope' then
    Exit(AEngine.GlobalScope);
  if ARoot = '%scope' then
  begin
    AConsumed := 2;
    if AEngine.GlobalScope.TryGetOwnLexicalBindingRaw(AName, Binding) and
       Binding.Initialized then
      Exit(Binding.Value);
    Exit(nil);
  end;
  if ARoot = '%slot' then
  begin
    AConsumed := 2;
    Slot := FindSlotByName(AName);
    if Slot < 0 then
      Exit(nil);
    if not Assigned(AEngine.Realm.GetSlot(Slot)) then
    begin
      Materializer := FindSnapshotSlotMaterializer(AName);
      if Assigned(Materializer) then
        Materializer();
    end;
    Exit(AEngine.Realm.GetSlot(Slot));
  end;
  // The well-known symbols are always in the table.
end;

function ResolveSegment(const AObject: TGCManagedObject;
  const ASegment: string): TGCManagedObject;
var
  Obj: TGocciaObjectValue;
  Descriptor: TGocciaPropertyDescriptor;
  Symbols: TArray<TGocciaSymbolValue>;
  Close, I: Integer;
  Description, Part, Key: string;
begin
  Result := nil;
  if not (AObject is TGocciaObjectValue) then
    Exit;
  Obj := TGocciaObjectValue(AObject);
  if ASegment = '[[Prototype]]' then
  begin
    if Obj is TGocciaClassValue then
      Exit(TGocciaClassValue(Obj).GetConstructorPrototype);
    Exit(Obj.Prototype);
  end;
  if (ASegment = 'prototype') and (Obj is TGocciaClassValue) then
    Exit(TGocciaClassValue(Obj).Prototype);
  if ASegment = '[[CapturedRoot]]' then
  begin
    if Obj is TGocciaNativeFunctionValue then
      Exit(TGocciaNativeFunctionValue(Obj).CapturedRoot);
    Exit(nil);
  end;
  if (ASegment <> '') and (ASegment[1] = '[') then
  begin
    Close := LastDelimiter(']', ASegment);
    Description := Copy(ASegment, 2, Close - 2);
    Part := Copy(ASegment, Close + 1, MaxInt);
    Symbols := Obj.GetOwnSymbols;
    for I := 0 to High(Symbols) do
      if Symbols[I].Description = Description then
      begin
        if Part = '<key>' then
          Exit(Symbols[I]);
        Descriptor := Obj.GetOwnSymbolPropertyDescriptor(Symbols[I]);
        if (Part = '<get>') and
           (Descriptor is TGocciaPropertyDescriptorAccessor) then
          Exit(TGocciaPropertyDescriptorAccessor(Descriptor).Getter);
        if (Part = '<set>') and
           (Descriptor is TGocciaPropertyDescriptorAccessor) then
          Exit(TGocciaPropertyDescriptorAccessor(Descriptor).Setter);
        if (Part = '') and (Descriptor is TGocciaPropertyDescriptorData) then
          Exit(TGocciaPropertyDescriptorData(Descriptor).Value);
        Exit(nil);
      end;
    Exit(nil);
  end;
  if (Copy(ASegment, 1, 5) = '<get ') or (Copy(ASegment, 1, 5) = '<set ') then
  begin
    Key := Copy(ASegment, 6, Length(ASegment) - 6);
    // Reading the descriptor materializes a lazy member.
    Descriptor := Obj.GetOwnPropertyDescriptor(Key);
    if not (Descriptor is TGocciaPropertyDescriptorAccessor) then
      Exit(nil);
    if ASegment[2] = 'g' then
      Exit(TGocciaPropertyDescriptorAccessor(Descriptor).Getter);
    Exit(TGocciaPropertyDescriptorAccessor(Descriptor).Setter);
  end;
  Descriptor := Obj.GetOwnPropertyDescriptor(ASegment);
  if Descriptor is TGocciaPropertyDescriptorData then
    Exit(TGocciaPropertyDescriptorData(Descriptor).Value);
end;

function SplitPath(const APath: string): TStringArray;
var
  Start, I, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  Start := 1;
  for I := 1 to Length(APath) + 1 do
    if (I > Length(APath)) or (APath[I] = HEAP_SNAPSHOT_PATH_SEPARATOR) then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := Copy(APath, Start, I - Start);
      Inc(Count);
      Start := I + 1;
    end;
end;

function TGocciaBuiltinPathResolver.Resolve(
  const APath: string): TGCManagedObject;
var
  Parts: TStringArray;
  Start, I, Consumed: Integer;
  Prefix, Name, Which: string;
begin
  Result := FTable.FindPath(APath);
  if Assigned(Result) then
    Exit;
  Parts := SplitPath(APath);
  if Length(Parts) = 0 then
    Exit(nil);
  // Start from the longest prefix the table holds, else from the root.
  Start := -1;
  Prefix := Parts[0];
  for I := 1 to High(Parts) do
  begin
    if Assigned(FTable.FindPath(Prefix)) then
      Start := I - 1;
    Prefix := Prefix + HEAP_SNAPSHOT_PATH_SEPARATOR + Parts[I];
  end;
  if Start >= 0 then
  begin
    Prefix := Parts[0];
    for I := 1 to Start do
      Prefix := Prefix + HEAP_SNAPSHOT_PATH_SEPARATOR + Parts[I];
    Result := FTable.FindPath(Prefix);
    I := Start + 1;
  end
  else
  begin
    Name := '';
    Which := '';
    if Length(Parts) > 1 then
      Name := Parts[1];
    if Length(Parts) > 2 then
      Which := Parts[2];
    Result := ResolveRoot(FEngine, Parts[0], Name, Which, Consumed);
    I := Consumed;
  end;
  while Assigned(Result) and (I <= High(Parts)) do
  begin
    Result := ResolveSegment(Result, Parts[I]);
    Inc(I);
  end;
  if Assigned(Result) then
    FTable.Add(Result, APath);
end;

end.
