unit Goccia.HeapSnapshot;

{ Heap snapshot prototype: engine-level entry points.

  BuildBuiltinTable numbers the built-ins of a fresh engine by a
  deterministic walk: the global object, the global scope's bindings, the
  realm slots, the realm's shared prototypes and the well-known symbols,
  then breadth first through prototypes and own properties. Each entry is
  named by the first path that reached it, and a snapshot refers to
  built-ins by those names. Lazy members of the objects walked are
  materialized first, so the table is the same in every process that builds
  it at the same point.

  Some intrinsics are created only when first used (an array iterator's
  prototype, for one). ExtendBuiltinTableWithLateSlots adds the ones a run
  created before the snapshot, by realm slot, and a restore creates the same
  ones through their registered materializers before it reads the objects
  (TGocciaLateSlotRestorer). }

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Engine,
  Goccia.HeapSnapshot.Codec;

type
  TGocciaLateSlotRestorer = class
  private
    FTable: TGocciaHeapSnapshotBuiltinTable;
    FEngine: TGocciaEngine;
  public
    constructor Create(const ATable: TGocciaHeapSnapshotBuiltinTable;
      const AEngine: TGocciaEngine);
    procedure Resolve(const ASlotNames: TStrings);
  end;

procedure BuildBuiltinTable(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AEngine: TGocciaEngine);
// Adds the realm slots filled since the table was built, and what they
// reach, to ATable; ALateSlots receives their names.
procedure ExtendBuiltinTableWithLateSlots(
  const ATable: TGocciaHeapSnapshotBuiltinTable; const AEngine: TGocciaEngine;
  const ALateSlots: TStrings);

implementation

uses
  SysUtils,

  Goccia.GarbageCollector,
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
      APath + '.[[Prototype]]');
    AddValue(ATable, TGocciaClassValue(Obj).Prototype, APath + '.prototype');
  end
  else
    AddValue(ATable, Obj.Prototype, APath + '.[[Prototype]]');
  // Materializes every lazy string-keyed member.
  try
    Obj.GetOwnPropertyKeys;
  except
    on E: Exception do
      ;
  end;
  for Pair in Obj.Properties do
  begin
    Descriptor := Pair.Value;
    if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      AddValue(ATable, TGocciaPropertyDescriptorAccessor(Descriptor).Getter,
        APath + '.<get ' + Pair.Key + '>');
      AddValue(ATable, TGocciaPropertyDescriptorAccessor(Descriptor).Setter,
        APath + '.<set ' + Pair.Key + '>');
    end
    else if Descriptor is TGocciaPropertyDescriptorData then
      AddValue(ATable, TGocciaPropertyDescriptorData(Descriptor).Value,
        APath + '.' + Pair.Key);
  end;
  Symbols := Obj.GetOwnSymbols;
  for I := 0 to High(Symbols) do
  begin
    SymbolPath := APath + '[' + Symbols[I].Description + ']';
    AddValue(ATable, Symbols[I], SymbolPath + '.<key>');
    Descriptor := Obj.GetOwnSymbolPropertyDescriptor(Symbols[I]);
    if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      AddValue(ATable, TGocciaPropertyDescriptorAccessor(Descriptor).Getter,
        SymbolPath + '.<get>');
      AddValue(ATable, TGocciaPropertyDescriptorAccessor(Descriptor).Setter,
        SymbolPath + '.<set>');
    end
    else if Descriptor is TGocciaPropertyDescriptorData then
      AddValue(ATable, TGocciaPropertyDescriptorData(Descriptor).Value,
        SymbolPath);
  end;
  if Obj is TGocciaNativeFunctionValue then
    AddValue(ATable, TGocciaNativeFunctionValue(Obj).CapturedRoot,
      APath + '.[[CapturedRoot]]');
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

procedure BuildBuiltinTable(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AEngine: TGocciaEngine);
var
  Realm: TGocciaRealm;
  Names: TGocciaStringArray;
  Binding: TLexicalBinding;
  Owned: TObject;
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
      AddValue(ATable, Binding.Value, '%scope.' + Names[I]);
  for I := 0 to RealmSlotCount - 1 do
    if Assigned(Realm.GetSlot(I)) then
      ATable.Add(Realm.GetSlot(I), '%slot.' + RealmSlotName(I))
    else
      ATable.EmptySlots.Add(I);
  for I := 0 to RealmOwnedSlotCount - 1 do
  begin
    Owned := Realm.GetOwnedSlot(I);
    if Owned is TGocciaSharedPrototype then
    begin
      AddValue(ATable, TGocciaSharedPrototype(Owned).Prototype,
        '%owned.' + RealmOwnedSlotName(I) + '.prototype');
      AddValue(ATable, TGocciaSharedPrototype(Owned).MethodHost,
        '%owned.' + RealmOwnedSlotName(I) + '.host');
    end;
  end;
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

procedure ExtendBuiltinTableWithLateSlots(
  const ATable: TGocciaHeapSnapshotBuiltinTable; const AEngine: TGocciaEngine;
  const ALateSlots: TStrings);
var
  Start, I, Slot: Integer;
begin
  Start := ATable.Count;
  for I := 0 to ATable.EmptySlots.Count - 1 do
  begin
    Slot := ATable.EmptySlots[I];
    if Assigned(AEngine.Realm.GetSlot(Slot)) then
    begin
      ATable.Add(AEngine.Realm.GetSlot(Slot), '%slot.' + RealmSlotName(Slot));
      ALateSlots.Add(RealmSlotName(Slot));
    end;
  end;
  WalkFrom(ATable, Start);
end;

{ TGocciaLateSlotRestorer }

constructor TGocciaLateSlotRestorer.Create(
  const ATable: TGocciaHeapSnapshotBuiltinTable; const AEngine: TGocciaEngine);
begin
  inherited Create;
  FTable := ATable;
  FEngine := AEngine;
end;

procedure TGocciaLateSlotRestorer.Resolve(const ASlotNames: TStrings);
var
  Start, I, Slot: Integer;
  Materializer: TGocciaSnapshotSlotMaterializer;
begin
  Start := FTable.Count;
  for I := 0 to ASlotNames.Count - 1 do
  begin
    Slot := FindSlotByName(ASlotNames[I]);
    if Slot < 0 then
      raise EGocciaHeapSnapshotRefused.Create('restore: unknown realm slot ' +
        ASlotNames[I]);
    if not Assigned(FEngine.Realm.GetSlot(Slot)) then
    begin
      Materializer := FindSnapshotSlotMaterializer(ASlotNames[I]);
      if not Assigned(Materializer) then
        raise EGocciaHeapSnapshotRefused.Create(
          'restore: no materializer for the lazy intrinsic ' + ASlotNames[I]);
      Materializer();
    end;
    if not Assigned(FEngine.Realm.GetSlot(Slot)) then
      raise EGocciaHeapSnapshotRefused.Create('restore: the lazy intrinsic ' +
        ASlotNames[I] + ' was not created');
    FTable.Add(FEngine.Realm.GetSlot(Slot), '%slot.' + ASlotNames[I]);
  end;
  WalkFrom(FTable, Start);
end;

end.
