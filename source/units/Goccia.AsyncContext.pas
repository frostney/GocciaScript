{ Continuation-scoped async context.

  One snapshot holds every AsyncLocalStorage binding that is in effect for a
  continuation: a small association list from the storage instance to the store
  it carries. Snapshots are immutable — deriving a binding copies the entries
  once — so a continuation that captured a snapshot keeps seeing exactly the
  bindings that were in effect when it was created, no matter what the code
  that created it does afterwards.

  Two seams keep the current snapshot travelling with continuations, and both
  live in the machinery both executors share:

    - a promise reaction records the snapshot in effect when it is registered
      (Goccia.Values.PromiseValue), and
    - the microtask queue installs a job's snapshot while the job runs and
      restores the previous one afterwards (Goccia.MicrotaskQueue).

  `await` needs no seam of its own. GocciaScript drains awaits synchronously
  inside the awaiting call, so the awaiting frame never leaves the Pascal
  stack; the drained jobs restore the snapshot they found, which leaves the
  awaiting frame's own snapshot in place across the await.

  The current snapshot is a garbage-collection root: it is reachable from
  nothing else while a `run` callback executes, and the stores it holds are
  ordinary JavaScript values. }

unit Goccia.AsyncContext;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  { An immutable set of AsyncLocalStorage bindings. Never exposed to
    JavaScript; it descends from TGocciaObjectValue only so the collector
    traces the stores it holds through the ordinary MarkReferences path. }
  TGocciaAsyncContextSnapshot = class(TGocciaObjectValue)
  private
    FKeys: TArray<TGocciaValue>;
    FStores: TArray<TGocciaValue>;
    FCount: Integer;
    function IndexOfKey(const AKey: TGocciaValue): Integer;
  public
    constructor Create;
    function TryGetStore(const AKey: TGocciaValue;
      out AStore: TGocciaValue): Boolean;
    procedure MarkReferences; override;
  end;

{ The snapshot in effect on this thread, or nil when no binding is in effect.
  nil is the empty snapshot rather than a missing one, so a program that never
  touches AsyncLocalStorage allocates nothing. }
function CurrentAsyncContext: TGocciaAsyncContextSnapshot;

{ Replaces the current snapshot without saving the old one. For the operations
  that deliberately mutate the ambient context and never restore it —
  `enterWith` and `disable`. Every scoped installation must use
  EnterAsyncContext/LeaveAsyncContext instead. }
procedure SetCurrentAsyncContext(
  const ASnapshot: TGocciaAsyncContextSnapshot);

{ Installs ASnapshot and saves the outgoing one on a stack the collector also
  marks, returning the token LeaveAsyncContext needs.

  The stack is what makes the save safe. A displaced snapshot is reachable from
  nothing else — the current-context slot is its only root — so holding it in a
  bare Pascal local across a guest call let a collection inside that call free
  it and left the restore writing a dangling pointer. Guest code reaches a
  collection safe point trivially (`Goccia.gc()`, or any allocation once the
  threshold is met), so this was a crash in practice, not in theory.

  The token is the stack depth at entry rather than a pop count, so a frame
  restores to the depth it was entered at and cannot unwind past an enclosing
  frame's entry even if the stack is left unbalanced beneath it. }
function EnterAsyncContext(
  const ASnapshot: TGocciaAsyncContextSnapshot): Integer;
procedure LeaveAsyncContext(const AToken: Integer);

{ Drops every snapshot this thread is holding. Thread teardown only: an engine
  must use EnterEngineAsyncContext/LeaveEngineAsyncContext instead, which do
  the same job for the engine's own span without reaching past it. }
procedure ResetAsyncContextState;

{ The bracket an engine holds for its whole lifetime.

  Enter hides whatever the thread was already holding and starts the engine on
  an empty context, so a worker thread reusing a slot cannot let one engine
  inherit the snapshot the previous one left behind — `enterWith` installs a
  context with no scope to leave, so a program that calls it outside a `run`
  deliberately ends with one in effect, and marking it would walk objects
  belonging to a realm that no longer exists.

  Leave restores exactly what Enter hid. Engines nest on one thread — a
  ShadowRealm owns a child engine, and freeing it can happen inside the outer
  engine's `run` or a microtask callback — so the teardown has to put the outer
  engine's context back rather than clear the thread. The displaced snapshot
  waits on the same collector-marked stack EnterAsyncContext uses, which is
  what keeps it alive while the inner engine runs. }
function EnterEngineAsyncContext: Integer;
procedure LeaveEngineAsyncContext(const AToken: Integer);

{ Both derivations tolerate a nil source snapshot and return nil when the
  result would be empty, so the nil-is-empty representation is closed. }
function DeriveAsyncContext(const ASnapshot: TGocciaAsyncContextSnapshot;
  const AKey, AStore: TGocciaValue): TGocciaAsyncContextSnapshot;
function DeriveAsyncContextWithout(
  const ASnapshot: TGocciaAsyncContextSnapshot;
  const AKey: TGocciaValue): TGocciaAsyncContextSnapshot;

implementation

uses
  SysUtils,

  Goccia.GarbageCollector,
  Goccia.ThreadCleanupRegistry;

type
  { Publishes the current snapshot to the collector. One instance per thread,
    created the first time a binding takes effect on that thread. }
  TGocciaAsyncContextRoots = class(TGCRootSource)
  private
    FCollector: TGarbageCollector;
  public
    procedure MarkRootReferences; override;
    property Collector: TGarbageCollector read FCollector write FCollector;
  end;

const
  SAVED_SNAPSHOT_INITIAL_CAPACITY = 8;

threadvar
  GCurrentSnapshot: TGocciaAsyncContextSnapshot;
  GSnapshotRoots: TGocciaAsyncContextRoots;
  { Snapshots displaced by an active EnterAsyncContext, innermost last. Held
    here rather than only in the entering frame's local so that they stay
    reachable for the collector while guest code runs. }
  GSavedSnapshots: TArray<TGocciaAsyncContextSnapshot>;
  GSavedSnapshotCount: Integer;

{ TGocciaAsyncContextRoots }

procedure TGocciaAsyncContextRoots.MarkRootReferences;
var
  I: Integer;
begin
  if Assigned(GCurrentSnapshot) then
    GCurrentSnapshot.MarkReferences;
  for I := 0 to GSavedSnapshotCount - 1 do
    if Assigned(GSavedSnapshots[I]) then
      GSavedSnapshots[I].MarkReferences;
end;

{ TGocciaAsyncContextSnapshot }

constructor TGocciaAsyncContextSnapshot.Create;
begin
  inherited Create(nil);
  FCount := 0;
end;

function TGocciaAsyncContextSnapshot.IndexOfKey(
  const AKey: TGocciaValue): Integer;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    if FKeys[I] = AKey then
      Exit(I);
  Result := -1;
end;

function TGocciaAsyncContextSnapshot.TryGetStore(const AKey: TGocciaValue;
  out AStore: TGocciaValue): Boolean;
var
  Index: Integer;
begin
  AStore := nil;
  Index := IndexOfKey(AKey);
  Result := Index >= 0;
  if Result then
    AStore := FStores[Index];
end;

procedure TGocciaAsyncContextSnapshot.MarkReferences;
var
  I: Integer;
begin
  if GCMarked then Exit;
  inherited;

  for I := 0 to FCount - 1 do
  begin
    if Assigned(FKeys[I]) then
      FKeys[I].MarkReferences;
    if Assigned(FStores[I]) then
      FStores[I].MarkReferences;
  end;
end;

{ Current snapshot }

function CurrentAsyncContext: TGocciaAsyncContextSnapshot;
begin
  Result := GCurrentSnapshot;
end;

{ The root source registers with whichever collector is current when it is
  built, so a thread whose collector was replaced needs a fresh one. }
procedure EnsureSnapshotRoots;
var
  Collector: TGarbageCollector;
begin
  Collector := TGarbageCollector.Instance;
  if Assigned(GSnapshotRoots) and (GSnapshotRoots.Collector = Collector) then
    Exit;

  FreeAndNil(GSnapshotRoots);
  if not Assigned(Collector) then
    Exit;

  GSnapshotRoots := TGocciaAsyncContextRoots.Create;
  GSnapshotRoots.Collector := Collector;
end;

procedure SetCurrentAsyncContext(
  const ASnapshot: TGocciaAsyncContextSnapshot);
begin
  GCurrentSnapshot := ASnapshot;
  if Assigned(ASnapshot) then
    EnsureSnapshotRoots;
end;

function EnterAsyncContext(
  const ASnapshot: TGocciaAsyncContextSnapshot): Integer;
begin
  if GSavedSnapshotCount >= Length(GSavedSnapshots) then
    SetLength(GSavedSnapshots,
      GSavedSnapshotCount * 2 + SAVED_SNAPSHOT_INITIAL_CAPACITY);

  Result := GSavedSnapshotCount;
  GSavedSnapshots[Result] := GCurrentSnapshot;
  Inc(GSavedSnapshotCount);
  GCurrentSnapshot := ASnapshot;

  { The root source has to exist before the first collection that could see
    either slot, and either slot may be the only reference to a snapshot. }
  if Assigned(ASnapshot) or Assigned(GSavedSnapshots[Result]) then
    EnsureSnapshotRoots;
end;

procedure LeaveAsyncContext(const AToken: Integer);
var
  I: Integer;
begin
  if (AToken < 0) or (AToken >= GSavedSnapshotCount) then
    Exit;

  GCurrentSnapshot := GSavedSnapshots[AToken];
  { Drop the released entries so a snapshot that is no longer reachable is not
    kept alive by a stale slot above the new top. }
  for I := AToken to GSavedSnapshotCount - 1 do
    GSavedSnapshots[I] := nil;
  GSavedSnapshotCount := AToken;
end;

function DeriveAsyncContext(const ASnapshot: TGocciaAsyncContextSnapshot;
  const AKey, AStore: TGocciaValue): TGocciaAsyncContextSnapshot;
var
  Derived: TGocciaAsyncContextSnapshot;
  Existing, I: Integer;
begin
  Derived := TGocciaAsyncContextSnapshot.Create;

  if not Assigned(ASnapshot) then
  begin
    SetLength(Derived.FKeys, 1);
    SetLength(Derived.FStores, 1);
    Derived.FKeys[0] := AKey;
    Derived.FStores[0] := AStore;
    Derived.FCount := 1;
    Exit(Derived);
  end;

  Existing := ASnapshot.IndexOfKey(AKey);
  if Existing >= 0 then
    Derived.FCount := ASnapshot.FCount
  else
    Derived.FCount := ASnapshot.FCount + 1;
  SetLength(Derived.FKeys, Derived.FCount);
  SetLength(Derived.FStores, Derived.FCount);

  for I := 0 to ASnapshot.FCount - 1 do
  begin
    Derived.FKeys[I] := ASnapshot.FKeys[I];
    Derived.FStores[I] := ASnapshot.FStores[I];
  end;

  if Existing >= 0 then
    Derived.FStores[Existing] := AStore
  else
  begin
    Derived.FKeys[Derived.FCount - 1] := AKey;
    Derived.FStores[Derived.FCount - 1] := AStore;
  end;

  Result := Derived;
end;

function DeriveAsyncContextWithout(
  const ASnapshot: TGocciaAsyncContextSnapshot;
  const AKey: TGocciaValue): TGocciaAsyncContextSnapshot;
var
  Derived: TGocciaAsyncContextSnapshot;
  I, Target: Integer;
begin
  if not Assigned(ASnapshot) then
    Exit(nil);
  if ASnapshot.IndexOfKey(AKey) < 0 then
    Exit(ASnapshot);
  if ASnapshot.FCount = 1 then
    Exit(nil);

  Derived := TGocciaAsyncContextSnapshot.Create;
  Derived.FCount := ASnapshot.FCount - 1;
  SetLength(Derived.FKeys, Derived.FCount);
  SetLength(Derived.FStores, Derived.FCount);

  Target := 0;
  for I := 0 to ASnapshot.FCount - 1 do
  begin
    if ASnapshot.FKeys[I] = AKey then
      Continue;
    Derived.FKeys[Target] := ASnapshot.FKeys[I];
    Derived.FStores[Target] := ASnapshot.FStores[I];
    Inc(Target);
  end;

  Result := Derived;
end;

procedure ResetAsyncContextState;
var
  I: Integer;
begin
  GCurrentSnapshot := nil;
  for I := 0 to High(GSavedSnapshots) do
    GSavedSnapshots[I] := nil;
  GSavedSnapshotCount := 0;
  { The root source is dropped too: it registered with the collector this
    engine used, and the next engine gets a fresh registration on demand. }
  FreeAndNil(GSnapshotRoots);
end;

function EnterEngineAsyncContext: Integer;
begin
  Result := EnterAsyncContext(nil);
end;

procedure LeaveEngineAsyncContext(const AToken: Integer);
begin
  LeaveAsyncContext(AToken);
  { The root source registered with the collector this engine used, so drop it
    once nothing is left for it to mark. While an outer engine still holds a
    context it has to stay: nothing else publishes that snapshot, and
    EnsureSnapshotRoots is only reached from the next Set/Enter, which may
    never come. }
  if (not Assigned(GCurrentSnapshot)) and (GSavedSnapshotCount = 0) then
    FreeAndNil(GSnapshotRoots);
end;

procedure CleanupAsyncContextThreadState;
begin
  ResetAsyncContextState;
  SetLength(GSavedSnapshots, 0);
end;

initialization
  RegisterThreadvarCleanup(CleanupAsyncContextThreadState);

end.
