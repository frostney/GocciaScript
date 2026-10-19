unit Goccia.HeapSnapshot;

{ Heap snapshots (ADR 0148): naming an engine's built-ins, and the session an
  embedder takes snapshots and restores through.

  Built-ins are named, not written. BuildBuiltinTable numbers the built-ins
  of an engine breadth first from its roots: the global object, the global
  scope's bindings, the realm's slots and shared prototypes, the live
  builtin objects and the well-known symbols. Each is named by the first
  path that reached it. The walk does not materialize lazy members; a
  restore resolves a name its table does not hold by walking the path
  (TGocciaBuiltinPathResolver), which materializes exactly the members the
  path crosses. ExtendBuiltinTable walks again before a snapshot, so what
  the run materialized since is named too.

  A path is its root and then segments, separated by
  HEAP_SNAPSHOT_PATH_SEPARATOR:
    .key        the value of the own data property key
    g.key       the getter of the own accessor key; s.key its setter
    @sym        the value of the symbol-keyed property; g@sym, s@sym, and
                k@sym the key itself. sym is a well-known symbol's name
                (@@iterator) or [description]#n, the n-th own symbol with
                that description
    %proto      [[Prototype]] (a class's constructor prototype)
    %classproto a class's prototype object
    %captured   a native function's captured root

  TGocciaHeapSnapshotSession is created right after the engine, before the
  run: it builds the table, records the built-ins' fingerprints (a built-in
  the run changes refuses the snapshot until built-in differences are
  written) and the objects alive before the run (the accounting check
  ignores them). }

{$I Goccia.inc}

interface

uses
  Classes,
  SysUtils,

  Goccia.Engine,
  Goccia.GarbageCollector,
  Goccia.HeapSnapshot.Codec;

type
  // Resolves names a restore-side table does not hold yet by walking them
  // from their root, materializing lazy members on the way.
  TGocciaBuiltinPathResolver = class
  private
    FTable: TGocciaHeapSnapshotBuiltinTable;
    FEngine: TGocciaEngine;
  public
    constructor Create(const ATable: TGocciaHeapSnapshotBuiltinTable;
      const AEngine: TGocciaEngine);
    function Resolve(const APath: string): TGCManagedObject;
  end;

  { An engine's snapshot state: its built-in table and what was alive before
    the run. Create it right after the engine, before anything runs on it,
    and free it before the engine. }
  TGocciaHeapSnapshotSession = class
  private
    FEngine: TGocciaEngine;
    FTable: TGocciaHeapSnapshotBuiltinTable;
    FBaseline: TGocciaHeapSnapshotBaseline;
    FCheckAccounting: Boolean;
    FLastObjectCount: Integer;
  public
    constructor Create(const AEngine: TGocciaEngine);
    destructor Destroy; override;
    { Writes what ARoots reach to AStream, or refuses with
      EGocciaHeapSnapshotRefused and leaves AStream as it was. }
    procedure Snapshot(const ARoots: array of TGCManagedObject;
      const AStream: TStream);
    { Objects the embedder keeps alive itself, which the accounting check
      is not to count as missed. }
    procedure AddToBaseline(const AObject: TGCManagedObject);
    property Table: TGocciaHeapSnapshotBuiltinTable read FTable;
    // Whether Snapshot runs the accounting check (on by default).
    property CheckAccounting: Boolean read FCheckAccounting
      write FCheckAccounting;
    // How many objects the last snapshot wrote.
    property LastObjectCount: Integer read FLastObjectCount;
  end;

  { A restored snapshot's roots, in the order they were written. They stay
    rooted until the restore is freed; the caller takes over what it keeps. }
  TGocciaHeapSnapshotRestore = class
  private
    FReader: TGocciaHeapSnapshotReader;
    FTable: TGocciaHeapSnapshotBuiltinTable;
    FResolver: TGocciaBuiltinPathResolver;
    FRoots: TArray<TGCManagedObject>;
  public
    constructor Create(const AEngine: TGocciaEngine; const AStream: TStream;
      const ALimits: TGocciaHeapSnapshotLimits);
    destructor Destroy; override;
    property Roots: TArray<TGCManagedObject> read FRoots;
  end;

procedure BuildBuiltinTable(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AEngine: TGocciaEngine);
// Walks the table again for members the run materialized since it was
// built, and adds the realm slots filled since and what they reach.
procedure ExtendBuiltinTable(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AEngine: TGocciaEngine);

implementation

uses
  Goccia.Builtins.Base,
  Goccia.MicrotaskQueue,
  Goccia.NativeLimits,
  Goccia.Realm,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
  Goccia.SharedPrototype,
  Goccia.Values.ClassValue,
  Goccia.Values.Error,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue;

const
  SEP = HEAP_SNAPSHOT_PATH_SEPARATOR;

// A thrown value's message, for a refusal: the error's own message property
// when it has one. Never runs script.
function ThrowMessage(const AThrow: TGocciaThrowValue): string;
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  Result := '';
  if not (AThrow.Value is TGocciaObjectValue) then
    Exit;
  if not TGocciaObjectValue(AThrow.Value).Properties.TryGetValue('message',
    Descriptor) then
    Exit;
  if (Descriptor is TGocciaPropertyDescriptorData) and
     (TGocciaPropertyDescriptorData(Descriptor).Value is
      TGocciaStringLiteralValue) then
    Result := TGocciaStringLiteralValue(
      TGocciaPropertyDescriptorData(Descriptor).Value).Value;
end;

function Seg(const APath, ASegment: string): string; inline;
begin
  Result := APath + SEP + ASegment;
end;

function WellKnownSymbolName(const ASymbol: TGocciaSymbolValue): string;
begin
  if ASymbol = TGocciaSymbolValue.WellKnownIterator then Exit('@@iterator');
  if ASymbol = TGocciaSymbolValue.WellKnownMatch then Exit('@@match');
  if ASymbol = TGocciaSymbolValue.WellKnownMatchAll then Exit('@@matchAll');
  if ASymbol = TGocciaSymbolValue.WellKnownReplace then Exit('@@replace');
  if ASymbol = TGocciaSymbolValue.WellKnownSearch then Exit('@@search');
  if ASymbol = TGocciaSymbolValue.WellKnownSplit then Exit('@@split');
  if ASymbol = TGocciaSymbolValue.WellKnownSpecies then Exit('@@species');
  if ASymbol = TGocciaSymbolValue.WellKnownHasInstance then Exit('@@hasInstance');
  if ASymbol = TGocciaSymbolValue.WellKnownToPrimitive then Exit('@@toPrimitive');
  if ASymbol = TGocciaSymbolValue.WellKnownToStringTag then Exit('@@toStringTag');
  if ASymbol = TGocciaSymbolValue.WellKnownIsConcatSpreadable then
    Exit('@@isConcatSpreadable');
  if ASymbol = TGocciaSymbolValue.WellKnownAsyncIterator then
    Exit('@@asyncIterator');
  if ASymbol = TGocciaSymbolValue.WellKnownMetadata then Exit('@@metadata');
  if ASymbol = TGocciaSymbolValue.WellKnownDispose then Exit('@@dispose');
  if ASymbol = TGocciaSymbolValue.WellKnownAsyncDispose then
    Exit('@@asyncDispose');
  if ASymbol = TGocciaSymbolValue.WellKnownUnscopables then Exit('@@unscopables');
  if ASymbol = TGocciaSymbolValue.WellKnownCustomMatcher then
    Exit('@@customMatcher');
  Result := '';
end;

// The segment name of AObject's own symbol key at AIndex in ASymbols.
function SymbolSegment(const ASymbols: TArray<TGocciaSymbolValue>;
  const AIndex: Integer): string;
var
  I, Occurrence: Integer;
begin
  Result := WellKnownSymbolName(ASymbols[AIndex]);
  if Result <> '' then
    Exit;
  Occurrence := 0;
  for I := 0 to AIndex - 1 do
    if (WellKnownSymbolName(ASymbols[I]) = '') and
       (ASymbols[I].Description = ASymbols[AIndex].Description) then
      Inc(Occurrence);
  Result := '[' + ASymbols[AIndex].Description + ']#' + IntToStr(Occurrence);
end;

procedure AddValue(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AValue: TGocciaValue; const APath: string);
begin
  if (AValue is TGocciaObjectValue) or (AValue is TGocciaSymbolValue) then
    ATable.Add(AValue, APath);
end;

// Numbers what one entry reaches; lazy members stay lazy.
procedure Visit(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AObject: TGCManagedObject; const APath: string);
var
  Obj: TGocciaObjectValue;
  Pair: TGocciaPropertyMap.TKeyValuePair;
  Descriptor: TGocciaPropertyDescriptor;
  Symbols: TArray<TGocciaSymbolValue>;
  Name: string;
  I: Integer;
begin
  if not (AObject is TGocciaObjectValue) then
    Exit;
  Obj := TGocciaObjectValue(AObject);
  if Obj is TGocciaClassValue then
  begin
    AddValue(ATable, TGocciaClassValue(Obj).GetConstructorPrototype,
      Seg(APath, '%proto'));
    AddValue(ATable, TGocciaClassValue(Obj).Prototype,
      Seg(APath, '%classproto'));
  end
  else
    AddValue(ATable, Obj.Prototype, Seg(APath, '%proto'));
  for Pair in Obj.Properties do
  begin
    Descriptor := Pair.Value;
    if Descriptor is TGocciaLazyPropertyDescriptorData then
      Continue;
    if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      AddValue(ATable, TGocciaPropertyDescriptorAccessor(Descriptor).Getter,
        Seg(APath, 'g.' + Pair.Key));
      AddValue(ATable, TGocciaPropertyDescriptorAccessor(Descriptor).Setter,
        Seg(APath, 's.' + Pair.Key));
    end
    else if Descriptor is TGocciaPropertyDescriptorData then
      AddValue(ATable, TGocciaPropertyDescriptorData(Descriptor).Value,
        Seg(APath, '.' + Pair.Key));
  end;
  Symbols := Obj.GetOwnSymbols;
  for I := 0 to High(Symbols) do
  begin
    Name := SymbolSegment(Symbols, I);
    AddValue(ATable, Symbols[I], Seg(APath, 'k@' + Name));
    Descriptor := Obj.GetOwnSymbolPropertyDescriptor(Symbols[I]);
    if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      AddValue(ATable, TGocciaPropertyDescriptorAccessor(Descriptor).Getter,
        Seg(APath, 'g@' + Name));
      AddValue(ATable, TGocciaPropertyDescriptorAccessor(Descriptor).Setter,
        Seg(APath, 's@' + Name));
    end
    else if Descriptor is TGocciaPropertyDescriptorData then
      AddValue(ATable, TGocciaPropertyDescriptorData(Descriptor).Value,
        Seg(APath, '@' + Name));
  end;
  if Obj is TGocciaNativeFunctionValue then
    AddValue(ATable, TGocciaNativeFunctionValue(Obj).CapturedRoot,
      Seg(APath, '%captured'));
end;

// Breadth first over the table from AStart: the table is the queue.
procedure WalkFrom(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AStart: Integer);
var
  I: Integer;
begin
  I := AStart;
  while I < ATable.Count do
  begin
    // A run can add to a built-in what it likes before the snapshot refuses
    // it: the walk is as long as the run made it.
    CheckNativeWork;
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

function FindOwnedSlotByName(const AName: string): Integer;
var
  I: Integer;
begin
  for I := 0 to RealmOwnedSlotCount - 1 do
    if RealmOwnedSlotName(I) = AName then
      Exit(I);
  Result := -1;
end;

// Each builtin's own object is a collector root the engine keeps.
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

procedure AddSlots(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AEngine: TGocciaEngine);
var
  Owned: TObject;
  I: Integer;
begin
  for I := 0 to RealmSlotCount - 1 do
    ATable.Add(AEngine.Realm.GetSlot(I), Seg('%slot', RealmSlotName(I)));
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

procedure AddWellKnownSymbols(const ATable: TGocciaHeapSnapshotBuiltinTable);

  procedure AddSymbol(const ASymbol: TGocciaSymbolValue);
  begin
    ATable.Add(ASymbol, WellKnownSymbolName(ASymbol));
  end;

begin
  AddSymbol(TGocciaSymbolValue.WellKnownIterator);
  AddSymbol(TGocciaSymbolValue.WellKnownMatch);
  AddSymbol(TGocciaSymbolValue.WellKnownMatchAll);
  AddSymbol(TGocciaSymbolValue.WellKnownReplace);
  AddSymbol(TGocciaSymbolValue.WellKnownSearch);
  AddSymbol(TGocciaSymbolValue.WellKnownSplit);
  AddSymbol(TGocciaSymbolValue.WellKnownSpecies);
  AddSymbol(TGocciaSymbolValue.WellKnownHasInstance);
  AddSymbol(TGocciaSymbolValue.WellKnownToPrimitive);
  AddSymbol(TGocciaSymbolValue.WellKnownToStringTag);
  AddSymbol(TGocciaSymbolValue.WellKnownIsConcatSpreadable);
  AddSymbol(TGocciaSymbolValue.WellKnownAsyncIterator);
  AddSymbol(TGocciaSymbolValue.WellKnownMetadata);
  AddSymbol(TGocciaSymbolValue.WellKnownDispose);
  AddSymbol(TGocciaSymbolValue.WellKnownAsyncDispose);
  AddSymbol(TGocciaSymbolValue.WellKnownUnscopables);
  AddSymbol(TGocciaSymbolValue.WellKnownCustomMatcher);
end;

procedure BuildBuiltinTable(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AEngine: TGocciaEngine);
var
  Names: TGocciaStringArray;
  Binding: TLexicalBinding;
  I: Integer;
begin
  ATable.Clear;
  AddWellKnownSymbols(ATable);
  ATable.Add(AEngine.Realm.GlobalObject, 'globalThis');
  ATable.Add(AEngine.GlobalScope, '%globalScope');
  Names := AEngine.GlobalScope.GetOwnBindingNames;
  for I := 0 to High(Names) do
    if AEngine.GlobalScope.TryGetOwnLexicalBindingRaw(Names[I], Binding) and
       Binding.Initialized then
      AddValue(ATable, Binding.Value, Seg('%scope', Names[I]));
  AddSlots(ATable, AEngine);
  AddLiveBuiltinObjects(ATable);
  WalkFrom(ATable, 0);
  ATable.RecordFingerprints;
end;

procedure ExtendBuiltinTable(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AEngine: TGocciaEngine);
var
  Start: Integer;
begin
  // Every entry again first: members materialized since are named by their
  // path from the entry that holds them.
  WalkFrom(ATable, 0);
  Start := ATable.Count;
  AddSlots(ATable, AEngine);
  AddLiveBuiltinObjects(ATable);
  WalkFrom(ATable, Start);
end;

{ Path resolution }

function SplitPath(const APath: string): TStringArray;
var
  Start, I, Count: Integer;
begin
  Result := nil;
  Count := 0;
  Start := 1;
  for I := 1 to Length(APath) + 1 do
    if (I > Length(APath)) or (APath[I] = SEP) then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := Copy(APath, Start, I - Start);
      Inc(Count);
      Start := I + 1;
    end;
end;

// The object a path's root names, and how many of its parts the root took.
function ResolveRoot(const AEngine: TGocciaEngine; const AParts: TStringArray;
  out AConsumed: Integer): TGCManagedObject;
var
  Slot: Integer;
  Binding: TLexicalBinding;
  Owned: TObject;
begin
  Result := nil;
  AConsumed := 1;
  if AParts[0] = 'globalThis' then
    Exit(AEngine.Realm.GlobalObject);
  if AParts[0] = '%globalScope' then
    Exit(AEngine.GlobalScope);
  if Length(AParts) < 2 then
    Exit;
  AConsumed := 2;
  if AParts[0] = '%builtin' then
    Exit(FindLiveBuiltinObject(AParts[1]));
  if AParts[0] = '%scope' then
  begin
    if AEngine.GlobalScope.TryGetOwnLexicalBindingRaw(AParts[1], Binding) and
       Binding.Initialized then
      Exit(Binding.Value);
    Exit;
  end;
  if AParts[0] = '%slot' then
  begin
    Slot := FindSlotByName(AParts[1]);
    if Slot >= 0 then
      Exit(AEngine.Realm.GetSlot(Slot));
    Exit;
  end;
  if (AParts[0] = '%owned') and (Length(AParts) >= 3) then
  begin
    AConsumed := 3;
    Slot := FindOwnedSlotByName(AParts[1]);
    if Slot < 0 then
      Exit;
    Owned := AEngine.Realm.GetOwnedSlot(Slot);
    if not (Owned is TGocciaSharedPrototype) then
      Exit;
    if AParts[2] = 'prototype' then
      Exit(TGocciaSharedPrototype(Owned).Prototype);
    if AParts[2] = 'host' then
      Exit(TGocciaSharedPrototype(Owned).MethodHost);
  end;
end;

// Follows one segment from AObject; reading a lazy member materializes it.
function ResolveSegment(const AObject: TGCManagedObject;
  const ASegment: string): TGCManagedObject;
var
  Obj: TGocciaObjectValue;
  Descriptor: TGocciaPropertyDescriptor;
  Symbols: TArray<TGocciaSymbolValue>;
  I: Integer;
  Kind, Name: string;
  Owned: Boolean;
begin
  Result := nil;
  if not (AObject is TGocciaObjectValue) then
    Exit;
  Obj := TGocciaObjectValue(AObject);
  if ASegment = '%proto' then
  begin
    if Obj is TGocciaClassValue then
      Exit(TGocciaClassValue(Obj).GetConstructorPrototype);
    Exit(Obj.Prototype);
  end;
  if ASegment = '%classproto' then
  begin
    if Obj is TGocciaClassValue then
      Exit(TGocciaClassValue(Obj).Prototype);
    Exit;
  end;
  if ASegment = '%captured' then
  begin
    if Obj is TGocciaNativeFunctionValue then
      Exit(TGocciaNativeFunctionValue(Obj).CapturedRoot);
    Exit;
  end;
  I := Pos('@', ASegment);
  if (I > 0) and (I <= 2) then
  begin
    Kind := Copy(ASegment, 1, I - 1);
    Name := Copy(ASegment, I + 1, MaxInt);
    Symbols := Obj.GetOwnSymbols;
    for I := 0 to High(Symbols) do
      if SymbolSegment(Symbols, I) = Name then
      begin
        if Kind = 'k' then
          Exit(Symbols[I]);
        Descriptor := Obj.GetOwnSymbolPropertyDescriptor(Symbols[I]);
        if not Assigned(Descriptor) then
          Exit;
        Owned := Descriptor.CallerOwned;
        try
          if (Kind = '') and (Descriptor is TGocciaPropertyDescriptorData) then
            Result := TGocciaPropertyDescriptorData(Descriptor).Value
          else if (Kind = 'g') and
             (Descriptor is TGocciaPropertyDescriptorAccessor) then
            Result := TGocciaPropertyDescriptorAccessor(Descriptor).Getter
          else if (Kind = 's') and
             (Descriptor is TGocciaPropertyDescriptorAccessor) then
            Result := TGocciaPropertyDescriptorAccessor(Descriptor).Setter;
        finally
          if Owned then
            Descriptor.Free;
        end;
        Exit;
      end;
    Exit;
  end;
  I := Pos('.', ASegment);
  if (I = 0) or (I > 2) then
    Exit;
  Kind := Copy(ASegment, 1, I - 1);
  Name := Copy(ASegment, I + 1, MaxInt);
  Descriptor := Obj.GetOwnPropertyDescriptor(Name);
  if not Assigned(Descriptor) then
    Exit;
  Owned := Descriptor.CallerOwned;
  try
    if (Kind = '') and (Descriptor is TGocciaPropertyDescriptorData) then
      Result := TGocciaPropertyDescriptorData(Descriptor).Value
    else if (Kind = 'g') and
       (Descriptor is TGocciaPropertyDescriptorAccessor) then
      Result := TGocciaPropertyDescriptorAccessor(Descriptor).Getter
    else if (Kind = 's') and
       (Descriptor is TGocciaPropertyDescriptorAccessor) then
      Result := TGocciaPropertyDescriptorAccessor(Descriptor).Setter;
  finally
    if Owned then
      Descriptor.Free;
  end;
end;

{ TGocciaBuiltinPathResolver }

constructor TGocciaBuiltinPathResolver.Create(
  const ATable: TGocciaHeapSnapshotBuiltinTable; const AEngine: TGocciaEngine);
begin
  inherited Create;
  FTable := ATable;
  FEngine := AEngine;
end;

function TGocciaBuiltinPathResolver.Resolve(
  const APath: string): TGCManagedObject;
var
  Parts: TStringArray;
  I, Consumed: Integer;
  Prefix: string;
  Known: TGCManagedObject;
begin
  Result := FTable.FindPath(APath);
  if Assigned(Result) then
    Exit;
  Parts := SplitPath(APath);
  if (Length(Parts) = 0) or (Parts[0] = '') then
    Exit(nil);
  // From the longest prefix the table holds, else from the root.
  Result := nil;
  Consumed := 0;
  Prefix := Parts[0];
  for I := 1 to High(Parts) do
  begin
    Known := FTable.FindPath(Prefix);
    if Assigned(Known) then
    begin
      Result := Known;
      Consumed := I;
    end;
    Prefix := Seg(Prefix, Parts[I]);
  end;
  if not Assigned(Result) then
    Result := ResolveRoot(FEngine, Parts, Consumed);
  I := Consumed;
  while Assigned(Result) and (I <= High(Parts)) do
  begin
    Result := ResolveSegment(Result, Parts[I]);
    Inc(I);
  end;
  if Assigned(Result) then
    FTable.Add(Result, APath);
end;

{ TGocciaHeapSnapshotSession }

constructor TGocciaHeapSnapshotSession.Create(const AEngine: TGocciaEngine);
var
  GC: TGarbageCollector;
  I: Integer;
  Obj: TGCManagedObject;
begin
  inherited Create;
  FEngine := AEngine;
  FCheckAccounting := True;
  FTable := TGocciaHeapSnapshotBuiltinTable.Create;
  FBaseline := TGocciaHeapSnapshotBaseline.Create;
  BuildBuiltinTable(FTable, AEngine);
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
    for I := 0 to GC.ManagedObjectCount - 1 do
    begin
      Obj := GC.ManagedObjects[I];
      if Assigned(Obj) then
        FBaseline.AddOrSetValue(Obj, True);
    end;
end;

destructor TGocciaHeapSnapshotSession.Destroy;
begin
  FBaseline.Free;
  FTable.Free;
  inherited;
end;

procedure TGocciaHeapSnapshotSession.AddToBaseline(
  const AObject: TGCManagedObject);
begin
  if Assigned(AObject) then
    FBaseline.AddOrSetValue(AObject, True);
end;

procedure TGocciaHeapSnapshotSession.Snapshot(
  const ARoots: array of TGCManagedObject; const AStream: TStream);
var
  Writer: TGocciaHeapSnapshotWriter;
  Buffer: TMemoryStream;
  Reason: string;
  Queue: TGocciaMicrotaskQueue;
begin
  Reason := FEngine.HeapSnapshotRefusal;
  if Reason <> '' then
    raise EGocciaHeapSnapshotRefused.Create(
      'state outside the heap no codec covers: ' + Reason);
  Writer := nil;
  Buffer := nil;
  try
    try
      // The one collection a snapshot runs, before anything is written: a
      // collection can queue work (a FinalizationRegistry whose target died),
      // and a run with a job queued is not at a checkpoint (ADR 0148). The
      // writer's own charges never collect.
      // What the last frames left in the VM's scratch is not the run's
      // state: released, so the collection and the accounting check do not
      // count it.
      if not FEngine.VM.ReleaseIdleScratch then
        raise EGocciaHeapSnapshotRefused.Create('a bytecode frame is running');
      if FCheckAccounting then
      begin
        TGarbageCollector.Instance.Collect;
        Queue := TGocciaMicrotaskQueue.Instance;
        if Assigned(Queue) and Queue.HasPending then
          raise EGocciaHeapSnapshotRefused.Create('the collection before ' +
            'the snapshot queued a job: the run is not idle any more');
      end;
      ExtendBuiltinTable(FTable, FEngine);
      Writer := TGocciaHeapSnapshotWriter.Create(FTable);
      Buffer := TGocciaChargedMemoryStream.Create;
      Writer.Serialize(ARoots, Buffer);
      if FCheckAccounting then
        Writer.CheckAccounting(FBaseline);
    except
      // The run's memory ceiling, reached by the writer's own buffers: the
      // snapshot is refused and the run goes on. A timeout or a host stop
      // still ends the run.
      on E: TGocciaThrowValue do
        raise EGocciaHeapSnapshotRefused.Create(
          'the snapshot does not fit the run''s limits: ' + ThrowMessage(E));
    end;
    FLastObjectCount := Writer.ObjectCount;
    AStream.CopyFrom(Buffer, 0);
  finally
    Buffer.Free;
    Writer.Free;
  end;
end;

{ TGocciaHeapSnapshotRestore }

constructor TGocciaHeapSnapshotRestore.Create(const AEngine: TGocciaEngine;
  const AStream: TStream; const ALimits: TGocciaHeapSnapshotLimits);
begin
  inherited Create;
  FTable := TGocciaHeapSnapshotBuiltinTable.Create;
  BuildBuiltinTable(FTable, AEngine);
  FResolver := TGocciaBuiltinPathResolver.Create(FTable, AEngine);
  FReader := TGocciaHeapSnapshotReader.Create(FTable, ALimits);
  FReader.BuiltinResolve := FResolver.Resolve;
  try
    FRoots := FReader.Restore(AStream);
  except
    // The run's memory ceiling, or a script error from building a lazy
    // built-in a name crossed: the snapshot cannot be restored here.
    on E: TGocciaThrowValue do
      raise EGocciaHeapSnapshotInvalid.Create('restore: ' + ThrowMessage(E));
  end;
end;

destructor TGocciaHeapSnapshotRestore.Destroy;
begin
  FReader.Free;
  FResolver.Free;
  FTable.Free;
  inherited;
end;

end.
