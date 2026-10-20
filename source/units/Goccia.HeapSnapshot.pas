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
  the run changes is written as its difference: its whole own state, which
  the restore gives the same built-in) and the objects alive before the run
  (the accounting check ignores them). A lazy member built during the run
  is named and recorded as it is built. }

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,
  SysUtils,

  Goccia.Engine,
  Goccia.GarbageCollector,
  Goccia.HeapSnapshot.Codec,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue;

type
  // Resolves names a restore-side table does not hold yet by walking them
  // from their root, materializing lazy members on the way.
  TGocciaBuiltinPathResolver = class
  private
    FTable: TGocciaHeapSnapshotBuiltinTable;
    FEngine: TGocciaEngine;
    FSymbolSegments: TObject;
  public
    constructor Create(const ATable: TGocciaHeapSnapshotBuiltinTable;
      const AEngine: TGocciaEngine);
    destructor Destroy; override;
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
    // Registered when the session was made and kept alive by it: what the
    // collector lists before it was alive before the run.
    FFirstOfRun: TGCManagedObject;
    FCheckAccounting: Boolean;
    FLastObjectCount: Integer;
    FAccountingWriter: TGocciaHeapSnapshotWriter;
    FMarkOverflows: Integer;
    FPendingRefusal: string;
    procedure MarkBuiltinInternals;
    procedure CheckAccountingMarked;
    function IsBuiltinInternal(const AObject: TGCManagedObject): Boolean;
    procedure MemberBuilt(const AObject: TGocciaObjectValue;
      const AName: string; const ADescriptor: TGocciaPropertyDescriptor);
  public
    constructor Create(const AEngine: TGocciaEngine);
    destructor Destroy; override;
    { Writes what ARoots reach to AStream from its position, and ends the
      stream there, or refuses with EGocciaHeapSnapshotRefused and leaves
      AStream as it was. The run must be idle (SnapshotIdleRefusal). }
    procedure Snapshot(const ARoots: array of TGCManagedObject;
      const AStream: TStream);
    { Objects the embedder keeps alive itself (for as long as the session),
      which the accounting check is not to count as missed. }
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
    FEngine: TGocciaEngine;
    FReader: TGocciaHeapSnapshotReader;
    FTable: TGocciaHeapSnapshotBuiltinTable;
    FResolver: TGocciaBuiltinPathResolver;
    FTemplates: TGocciaSnapshotTemplateTable;
    FRoots: TGCManagedObjectArray;
    function VerifyBuiltins(const APaths: TStringArray;
      const AFingerprints: array of string): string;
  public
    constructor Create(const AEngine: TGocciaEngine; const AStream: TStream;
      const ALimits: TGocciaHeapSnapshotLimits);
    destructor Destroy; override;
    property Roots: TGCManagedObjectArray read FRoots;
  end;

// ARecordFingerprints: record each built-in's state now, for DirtyPaths; a
// session does, a restore does not need to.
procedure BuildBuiltinTable(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AEngine: TGocciaEngine; const ARecordFingerprints: Boolean = True);
// Walks the table again for members the run materialized since it was
// built, and adds the realm slots filled since and what they reach. Of a
// built-in the run changed (AChanged, table indexes), only what the engine
// gave it is walked: what the run put there is the run's, and written.
procedure ExtendBuiltinTable(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AEngine: TGocciaEngine; const AChanged: TList<Integer> = nil);

implementation

uses
  Goccia.Builtins.Base,
  Goccia.ExecutionContext,
  Goccia.MemoryLimit,
  Goccia.MicrotaskQueue,
  Goccia.NativeLimits,
  Goccia.Realm,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
  Goccia.SharedPrototype,
  Goccia.Shims,
  Goccia.Values.ClassValue,
  Goccia.Values.Error,
  Goccia.Values.NativeFunction,
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

// The segment names of an object's own symbol keys, in their order: a
// well-known symbol's name, else [description]#n for the n-th own symbol with
// that description. One pass, counting descriptions as it goes.
// Far above any built-in's name. A longer path is not a name: what only a
// path that long reaches is not named (anything a run hangs on a built-in
// that deep has changed that built-in anyway), and SplitPath refuses one.
const
  MAX_PATH_CHARS = 4096;

type
  // Names an object's own symbol keys one at a time, in their order, as
  // SymbolSegments does; a name that could not be part of a path (its
  // description too long) is '' and still counted.
  TSymbolSegmentNamer = class
  private
    FSeen: TDictionary<string, Integer>;
  public
    constructor Create;
    destructor Destroy; override;
    function Next(const ASymbol: TGocciaSymbolValue): string;
  end;

constructor TSymbolSegmentNamer.Create;
begin
  inherited Create;
  FSeen := TDictionary<string, Integer>.Create;
end;

destructor TSymbolSegmentNamer.Destroy;
begin
  FSeen.Free;
  inherited;
end;

function TSymbolSegmentNamer.Next(const ASymbol: TGocciaSymbolValue): string;
var
  Occurrence: Integer;
begin
  Result := WellKnownSymbolName(ASymbol);
  if Result <> '' then
    Exit;
  // A description too long for a path names nothing: it is neither hashed
  // nor kept, so the map holds only descriptions within the path cap (the
  // strings are shared, not copied). Occurrences count per description, so
  // skipping these leaves every other symbol's number unchanged.
  if Length(ASymbol.Description) > MAX_PATH_CHARS then
    Exit;
  if not FSeen.TryGetValue(ASymbol.Description, Occurrence) then
    Occurrence := 0;
  FSeen.AddOrSetValue(ASymbol.Description, Occurrence + 1);
  Result := '[' + ASymbol.Description + ']#' + IntToStr(Occurrence);
end;

function SymbolSegments(
  const ASymbols: TArray<TGocciaSymbolValue>): TStringArray;
var
  Namer: TSymbolSegmentNamer;
  I: Integer;
begin
  SetLength(Result, Length(ASymbols));
  Namer := TSymbolSegmentNamer.Create;
  try
    for I := 0 to High(ASymbols) do
      Result[I] := Namer.Next(ASymbols[I]);
  finally
    Namer.Free;
  end;
end;

procedure AddValue(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AValue: TGocciaValue; const APath: string);
begin
  if ((AValue is TGocciaObjectValue) or (AValue is TGocciaSymbolValue)) and
     (Length(APath) <= MAX_PATH_CHARS) then
    ATable.Add(AValue, APath);
end;

// A member at APath's segment AKind + AKey: the path is built only when the
// value could be named and the path would not be too long, so a key the run
// made long is never copied.
procedure AddMember(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AValue: TGocciaValue; const APath, AKind, AKey: string);
begin
  if ((AValue is TGocciaObjectValue) or (AValue is TGocciaSymbolValue)) and
     (Length(APath) + Length(AKind) + Length(AKey) + 1 <= MAX_PATH_CHARS) then
    ATable.Add(AValue, APath + SEP + AKind + AKey);
end;

// Numbers what one entry reaches; lazy members stay lazy.
procedure Visit(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AObject: TGCManagedObject; const APath: string);
var
  Obj: TGocciaObjectValue;
  Pair: TGocciaPropertyMap.TKeyValuePair;
  Descriptor: TGocciaPropertyDescriptor;
  Symbol: TGocciaSymbolValue;
  Namer: TSymbolSegmentNamer;
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
    CheckNativeWork;
    Descriptor := Pair.Value;
    if Descriptor is TGocciaLazyPropertyDescriptorData then
      Continue;
    if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      AddMember(ATable, TGocciaPropertyDescriptorAccessor(Descriptor).Getter,
        APath, 'g.', Pair.Key);
      AddMember(ATable, TGocciaPropertyDescriptorAccessor(Descriptor).Setter,
        APath, 's.', Pair.Key);
    end
    else if Descriptor is TGocciaPropertyDescriptorData then
      AddMember(ATable, TGocciaPropertyDescriptorData(Descriptor).Value,
        APath, '.', Pair.Key);
  end;
  // One symbol at a time, without a copy of the list.
  Namer := TSymbolSegmentNamer.Create;
  try
    for I := 0 to Obj.StoredSymbolCount - 1 do
    begin
      CheckNativeWork;
      Symbol := Obj.StoredSymbolAt(I);
      Name := Namer.Next(Symbol);
      // A description too long for a path: nothing it reaches is named.
      if Name = '' then
        Continue;
      AddMember(ATable, Symbol, APath, 'k@', Name);
      Descriptor := Obj.GetOwnSymbolPropertyDescriptor(Symbol);
      if Descriptor is TGocciaPropertyDescriptorAccessor then
      begin
        AddMember(ATable, TGocciaPropertyDescriptorAccessor(Descriptor).Getter,
          APath, 'g@', Name);
        AddMember(ATable, TGocciaPropertyDescriptorAccessor(Descriptor).Setter,
          APath, 's@', Name);
      end
      else if Descriptor is TGocciaPropertyDescriptorData then
        AddMember(ATable, TGocciaPropertyDescriptorData(Descriptor).Value,
          APath, '@', Name);
    end;
  finally
    Namer.Free;
  end;
  if Obj is TGocciaNativeFunctionValue then
    AddValue(ATable, TGocciaNativeFunctionValue(Obj).CapturedRoot,
      Seg(APath, '%captured'));
end;

// What a built-in the run changed holds that the engine gave it: its
// internals, and the members the engine built that the run left alone. The
// rest (its prototype, what the run put there) is written with its
// difference.
procedure VisitChanged(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AObject: TGCManagedObject; const APath: string);
var
  Obj: TGocciaObjectValue;
  Pair: TGocciaPropertyMap.TKeyValuePair;
  Descriptor: TGocciaPropertyDescriptor;
begin
  if not (AObject is TGocciaObjectValue) then
    Exit;
  Obj := TGocciaObjectValue(AObject);
  if Obj is TGocciaClassValue then
    AddValue(ATable, TGocciaClassValue(Obj).Prototype,
      Seg(APath, '%classproto'));
  for Pair in Obj.Properties do
  begin
    CheckNativeWork;
    Descriptor := Pair.Value;
    if (Descriptor is TGocciaLazyPropertyDescriptorData) or
       not ATable.IsPristineMember(Obj, Pair.Key, Descriptor) then
      Continue;
    if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      AddMember(ATable, TGocciaPropertyDescriptorAccessor(Descriptor).Getter,
        APath, 'g.', Pair.Key);
      AddMember(ATable, TGocciaPropertyDescriptorAccessor(Descriptor).Setter,
        APath, 's.', Pair.Key);
    end
    else if Descriptor is TGocciaPropertyDescriptorData then
      AddMember(ATable, TGocciaPropertyDescriptorData(Descriptor).Value,
        APath, '.', Pair.Key);
  end;
  if Obj is TGocciaNativeFunctionValue then
    AddValue(ATable, TGocciaNativeFunctionValue(Obj).CapturedRoot,
      Seg(APath, '%captured'));
end;

// Breadth first over the table from AStart: the table is the queue. An
// entry in AChanged is walked only as far as VisitChanged goes.
procedure WalkFrom(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AStart: Integer; const AChanged: THashSet<Integer> = nil);
var
  I: Integer;
begin
  I := AStart;
  while I < ATable.Count do
  begin
    // A run can add to a built-in what it likes: the walk is as long as the
    // run made it.
    CheckNativeWork;
    if Assigned(AChanged) and AChanged.Contains(I) then
      VisitChanged(ATable, ATable.ObjectAt(I), ATable.PathAt(I))
    else
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

// This engine's builtin with AName, nil when it has none or more than one
// (the live list is the thread's; a name two of them share names neither).
function FindLiveBuiltinObject(const AEngine: TGocciaEngine;
  const AName: string): TGocciaObjectValue;
var
  Builtin: TGocciaBuiltin;
begin
  Result := nil;
  Builtin := FirstLiveBuiltin;
  while Assigned(Builtin) do
  begin
    if (Builtin.Scope = AEngine.GlobalScope) and (Builtin.Name = AName) then
    begin
      if Assigned(Result) and (Result <> Builtin.BuiltinObject) then
        Exit(nil);
      Result := Builtin.BuiltinObject;
    end;
    Builtin := Builtin.NextLive;
  end;
end;

// Each builtin's own object is a collector root the engine keeps; named only
// when its name is this engine's for it alone.
procedure AddLiveBuiltinObjects(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AEngine: TGocciaEngine);
var
  Builtin: TGocciaBuiltin;
begin
  Builtin := FirstLiveBuiltin;
  while Assigned(Builtin) do
  begin
    if (Builtin.Scope = AEngine.GlobalScope) and
       (FindLiveBuiltinObject(AEngine, Builtin.Name) = Builtin.BuiltinObject) then
      AddValue(ATable, Builtin.BuiltinObject, Seg('%builtin', Builtin.Name));
    Builtin := Builtin.NextLive;
  end;
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
  const AEngine: TGocciaEngine; const ARecordFingerprints: Boolean);
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
  AddLiveBuiltinObjects(ATable, AEngine);
  WalkFrom(ATable, 0);
  if ARecordFingerprints then
    ATable.RecordFingerprints;
end;

procedure ExtendBuiltinTable(const ATable: TGocciaHeapSnapshotBuiltinTable;
  const AEngine: TGocciaEngine; const AChanged: TList<Integer>);
var
  Start, I: Integer;
  Changed: THashSet<Integer>;
begin
  Changed := nil;
  try
    if Assigned(AChanged) and (AChanged.Count > 0) then
    begin
      Changed := THashSet<Integer>.Create;
      for I := 0 to AChanged.Count - 1 do
        Changed.Add(AChanged[I]);
    end;
    // Every entry again first: members materialized since are named by
    // their path from the entry that holds them.
    WalkFrom(ATable, 0, Changed);
    Start := ATable.Count;
    AddSlots(ATable, AEngine);
    AddLiveBuiltinObjects(ATable, AEngine);
    WalkFrom(ATable, Start, Changed);
  finally
    Changed.Free;
  end;
end;

{ Path resolution }

// The segments of a name: nil when it has more than MAX_PATH_SEGMENTS. The
// built-ins of an engine sit a few segments from its roots; a hostile name
// is bounded here, before anything is allocated for it.
function SplitPath(const APath: string): TStringArray;
const
  MAX_PATH_SEGMENTS = 256;
var
  Start, I, Count, Segments: Integer;
begin
  Result := nil;
  if Length(APath) > MAX_PATH_CHARS then
    Exit;
  Segments := 1;
  for I := 1 to Length(APath) do
    if APath[I] = SEP then
    begin
      Inc(Segments);
      if Segments > MAX_PATH_SEGMENTS then
        Exit;
    end;
  SetLength(Result, Segments);
  Count := 0;
  Start := 1;
  for I := 1 to Length(APath) + 1 do
    if (I > Length(APath)) or (APath[I] = SEP) then
    begin
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
    Exit(FindLiveBuiltinObject(AEngine, AParts[1]));
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
    if Slot < 0 then
      Exit;
    // Filled on first use: filled here as using it would have.
    if not Assigned(AEngine.Realm.GetSlot(Slot)) then
      MaterializeSnapshotSlot(AParts[1]);
    Exit(AEngine.Realm.GetSlot(Slot));
  end;
  if (AParts[0] = '%owned') and (Length(AParts) >= 3) then
  begin
    AConsumed := 3;
    Slot := FindOwnedSlotByName(AParts[1]);
    if Slot < 0 then
      Exit;
    // Filled on first use: filled here as using it would have.
    if not Assigned(AEngine.Realm.GetOwnedSlot(Slot)) then
      MaterializeSnapshotSlot(AParts[1]);
    Owned := AEngine.Realm.GetOwnedSlot(Slot);
    if not (Owned is TGocciaSharedPrototype) then
      Exit;
    if AParts[2] = 'prototype' then
      Exit(TGocciaSharedPrototype(Owned).Prototype);
    if AParts[2] = 'host' then
      Exit(TGocciaSharedPrototype(Owned).MethodHost);
  end;
end;

type
  TSymbolSegmentCache = TDictionary<TGCManagedObject, TStringArray>;

// Follows one segment from AObject; reading a lazy member materializes it.
// ACache keeps each object's symbol segment names, built once.
function ResolveSegment(const AObject: TGCManagedObject;
  const ASegment: string; const ACache: TSymbolSegmentCache): TGCManagedObject;
var
  Obj: TGocciaObjectValue;
  Descriptor: TGocciaPropertyDescriptor;
  Symbols: TArray<TGocciaSymbolValue>;
  Names: TStringArray;
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
    if not ACache.TryGetValue(Obj, Names) or
       (Length(Names) <> Length(Symbols)) then
    begin
      Names := SymbolSegments(Symbols);
      ACache.AddOrSetValue(Obj, Names);
    end;
    for I := 0 to High(Symbols) do
      if Names[I] = Name then
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
  FSymbolSegments := TSymbolSegmentCache.Create;
end;

destructor TGocciaBuiltinPathResolver.Destroy;
begin
  FSymbolSegments.Free;
  inherited;
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
    CheckNativeWork;
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
    CheckNativeWork;
    Result := ResolveSegment(Result, Parts[I],
      TSymbolSegmentCache(FSymbolSegments));
    Inc(I);
  end;
  if Assigned(Result) then
    FTable.Add(Result, APath);
end;

// The modules an engine compiled, whose templates a snapshot names.
function EngineTemplates(
  const AEngine: TGocciaEngine): TGocciaSnapshotTemplateTable;
var
  I: Integer;
begin
  Result := TGocciaSnapshotTemplateTable.Create;
  try
    for I := 0 to AEngine.RetainedModuleCount - 1 do
    begin
      CheckNativeWork;
      Result.AddModule(AEngine.RetainedModule(I));
    end;
  except
    Result.Free;
    raise;
  end;
end;

{ TGocciaHeapSnapshotSession }

constructor TGocciaHeapSnapshotSession.Create(const AEngine: TGocciaEngine);
begin
  inherited Create;
  FEngine := AEngine;
  FCheckAccounting := True;
  FTable := TGocciaHeapSnapshotBuiltinTable.Create;
  FBaseline := TGocciaHeapSnapshotBaseline.Create;
  BuildBuiltinTable(FTable, AEngine);
  // From here on, a lazy member a built-in builds is told apart from a
  // change the run makes to it.
  FTable.Install(AEngine.Realm);
  FTable.OnMemberBuilt := MemberBuilt;
  FFirstOfRun := TGocciaObjectValue.Create;
  TGarbageCollector.Instance.AddTempRoot(FFirstOfRun);
end;

destructor TGocciaHeapSnapshotSession.Destroy;
begin
  if Assigned(FFirstOfRun) and Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.RemoveTempRoot(FFirstOfRun);
  FBaseline.Free;
  FTable.Free;
  inherited;
end;

{ The internals of built-ins written in JavaScript (a shim's module scope,
  its closures' cells, its private functions and tables), built on first use
  and held only by the built-ins, are named through the built-ins that hold
  them (ADR 0148): nothing writes them, and the accounting check is not to
  count them as missed. Everything the named built-ins reach is marked, by
  the collector's own marking outside a collection, and of that only what a
  shim's load created is passed over: a run's object that a shim took in
  while it loaded (what a global held then) is still counted. The global
  scope is marked first, so what the run's own top-level bindings hold,
  which a built-in's scope chain reaches, is counted too. No limit is
  polled: marking is bounded by the heap, as the collection before it is. }
procedure TGocciaHeapSnapshotSession.MarkBuiltinInternals;
var
  I: Integer;
begin
  FMarkOverflows := TGarbageCollector.MarkStackOverflows;
  FEngine.GlobalScope.GCMarked := True;
  for I := 0 to FTable.Count - 1 do
    FTable.ObjectAt(I).MarkReferences;
end;

procedure TGocciaHeapSnapshotSession.CheckAccountingMarked;
begin
  // A mark stack that could not grow is recovered by tracing every marked
  // object again, the global scope too: what the run's bindings hold would
  // then be skipped as a built-in's. The collect lock is held, so only this
  // marking can have overflowed.
  if TGarbageCollector.MarkStackOverflows <> FMarkOverflows then
    raise EGocciaHeapSnapshotRefused.Create('the mark stack overflowed ' +
      'while the accounting check marked what built-ins reach');
  FAccountingWriter.CheckAccounting(FFirstOfRun, FBaseline,
    IsBuiltinInternal);
end;

function TGocciaHeapSnapshotSession.IsBuiltinInternal(
  const AObject: TGCManagedObject): Boolean;
var
  IsMarker: Boolean;
begin
  Result := CreatedByShimLoad(FEngine.Realm, AObject, IsMarker) and
    (IsMarker or AObject.GCMarked);
end;

{ A lazy member of a named built-in was just built: what it holds is named
  now, by its path from that built-in, and recorded as building it produced
  it, so a change the run makes to it later is a difference like any other
  (ADR 0148). A built-in filled on first use some other way (natively, as
  the iterator prototypes are) is named before the snapshot, unrecorded:
  changed, it refuses the restore. }
procedure TGocciaHeapSnapshotSession.MemberBuilt(
  const AObject: TGocciaObjectValue; const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor);
var
  Index, Start, I: Integer;
  Path: string;
begin
  if FPendingRefusal <> '' then
    Exit;
  Index := FTable.IndexOf(AObject);
  if Index < 0 then
    Exit;
  Path := FTable.PathAt(Index);
  Start := FTable.Count;
  try
    if ADescriptor is TGocciaPropertyDescriptorAccessor then
    begin
      AddMember(FTable, TGocciaPropertyDescriptorAccessor(ADescriptor).Getter,
        Path, 'g.', AName);
      AddMember(FTable, TGocciaPropertyDescriptorAccessor(ADescriptor).Setter,
        Path, 's.', AName);
    end
    else if ADescriptor is TGocciaPropertyDescriptorData then
      AddMember(FTable, TGocciaPropertyDescriptorData(ADescriptor).Value, Path,
        '.', AName);
    WalkFrom(FTable, Start);
    for I := Start to FTable.Count - 1 do
      FTable.RecordFingerprint(I);
  except
    // Observation must not turn an ordinary property read into a snapshot
    // refusal. Its partially recorded table cannot describe a later pause.
    on E: EGocciaHeapSnapshotRefused do
      if FPendingRefusal = '' then
        FPendingRefusal := E.Message;
  end;
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
  Roots: TGocciaActiveRootFrame;
  I: Integer;
  Templates: TGocciaSnapshotTemplateTable;
  Changed: TList<Integer>;
begin
  // The run must be idle: no frame, job, waiter or other extension's work
  // the snapshot would drop (ADR 0148).
  Reason := FEngine.SnapshotIdleRefusal;
  if Reason <> '' then
    raise EGocciaHeapSnapshotRefused.Create(Reason);
  if FPendingRefusal <> '' then
    raise EGocciaHeapSnapshotRefused.Create(FPendingRefusal);
  Writer := nil;
  Buffer := nil;
  Templates := nil;
  Changed := nil;
  // The roots may be held only by the caller's array: rooted before the
  // collection below, and until the snapshot is written.
  Roots.Initialize;
  for I := 0 to High(ARoots) do
    Roots.Add(ARoots[I]);
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
        TGarbageCollector.Instance.Collect;
      Queue := TGocciaMicrotaskQueue.Instance;
      if Assigned(Queue) and Queue.HasPending then
        raise EGocciaHeapSnapshotRefused.Create('the collection before ' +
          'the snapshot queued a job: the run is not idle any more');
      // The built-ins the run changed are written as their differences;
      // the walk does not name what the run put on them.
      Changed := FTable.ChangedIndexes;
      ExtendBuiltinTable(FTable, FEngine, Changed);
      Writer := TGocciaHeapSnapshotWriter.Create(FTable);
      Templates := EngineTemplates(FEngine);
      Writer.Templates := Templates;
      Writer.Differences := Changed;
      Buffer := TGocciaChargedMemoryStream.Create;
      Writer.Serialize(ARoots, Buffer);
      if FCheckAccounting then
      begin
        FAccountingWriter := Writer;
        try
          TGarbageCollector.Instance.InspectMarked(MarkBuiltinInternals,
            CheckAccountingMarked);
        finally
          FAccountingWriter := nil;
        end;
      end;
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
    // The snapshot ends the stream: what a reused stream held past it would
    // read as bytes after the roots.
    if AStream.Size > AStream.Position then
      AStream.Size := AStream.Position;
  finally
    Buffer.Free;
    Writer.Free;
    Templates.Free;
    Changed.Free;
    Roots.Clear;
  end;
end;

{ TGocciaHeapSnapshotRestore }

constructor TGocciaHeapSnapshotRestore.Create(const AEngine: TGocciaEngine;
  const AStream: TStream; const ALimits: TGocciaHeapSnapshotLimits);
var
  Context: TGocciaExecutionContextScope;
begin
  inherited Create;
  FEngine := AEngine;
  // In this engine's realm, whichever is current on the thread: built-ins
  // built on first use are built into it.
  Context := AEngine.ActivateRealmExecutionContext;
  try
    try
      FTable := TGocciaHeapSnapshotBuiltinTable.Create;
      BuildBuiltinTable(FTable, AEngine, False);
      // The lazy members this engine builds while it resolves names are its
      // pristine ones, as the writing engine's were when it built them.
      FTable.Install(AEngine.Realm);
      FResolver := TGocciaBuiltinPathResolver.Create(FTable, AEngine);
      FReader := TGocciaHeapSnapshotReader.Create(FTable, ALimits);
      FReader.BuiltinResolve := FResolver.Resolve;
      FReader.BuiltinVerify := VerifyBuiltins;
      FReader.Context := AEngine.VM;
      // The functions the snapshot restores run the code of this engine's
      // own compiled modules, matched by their bytecode.
      FTemplates := EngineTemplates(AEngine);
      FReader.Templates := FTemplates;
      FRoots := FReader.Restore(AStream);
    except
      // The run's memory ceiling, or a script error from building a lazy
      // built-in a name crossed: the snapshot cannot be restored here.
      on E: TGocciaThrowValue do
        raise EGocciaHeapSnapshotInvalid.Create('restore: ' + ThrowMessage(E));
      // A store's growth the memory ceiling refused (a large array's
      // elements, a changed built-in's new properties): ended a run would be,
      // a restore is refused.
      on E: TGocciaMemoryLimitError do
        raise EGocciaHeapSnapshotInvalid.Create('restore: ' + E.Message);
      // A charge of the tables' that did not fit, the built-in table's
      // included.
      on E: EGocciaHeapSnapshotRefused do
        if E is EGocciaHeapSnapshotInvalid then
          raise
        else
          raise EGocciaHeapSnapshotInvalid.Create('restore: ' + E.Message);
    end;
  finally
    Context.Free;
  end;
end;

// The built-ins the writing run built, built here by name, each compared
// with the fingerprint the run's took: pristine here, so a difference is a
// change the run made (or an engine that builds it otherwise).
function TGocciaHeapSnapshotRestore.VerifyBuiltins(const APaths: TStringArray;
  const AFingerprints: array of string): string;
var
  I, Index: Integer;
begin
  for I := 0 to High(APaths) do
    if not Assigned(FResolver.Resolve(APaths[I])) then
      Exit(APaths[I]);
  // What they reach is named here as it was there.
  ExtendBuiltinTable(FTable, FEngine);
  for I := 0 to High(APaths) do
  begin
    CheckNativeWork;
    Index := FTable.IndexOf(FTable.FindPath(APaths[I]));
    if (Index < 0) or (FTable.ComputeFingerprint(Index) <> AFingerprints[I]) then
      Exit(APaths[I]);
  end;
  Result := '';
end;

destructor TGocciaHeapSnapshotRestore.Destroy;
begin
  FReader.Free;
  FTemplates.Free;
  FResolver.Free;
  FTable.Free;
  inherited;
end;

end.
