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
procedure SetCurrentAsyncContext(
  const ASnapshot: TGocciaAsyncContextSnapshot);

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

threadvar
  GCurrentSnapshot: TGocciaAsyncContextSnapshot;
  GSnapshotRoots: TGocciaAsyncContextRoots;

{ TGocciaAsyncContextRoots }

procedure TGocciaAsyncContextRoots.MarkRootReferences;
begin
  if Assigned(GCurrentSnapshot) then
    GCurrentSnapshot.MarkReferences;
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

procedure CleanupAsyncContextThreadState;
begin
  GCurrentSnapshot := nil;
  FreeAndNil(GSnapshotRoots);
end;

initialization
  RegisterThreadvarCleanup(CleanupAsyncContextThreadState);

end.
