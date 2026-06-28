{ Registry of per-thread cleanup callbacks drained at worker-thread exit and
  at main-thread shutdown.

  FPC does not auto-finalize managed threadvars (AnsiString, dynamic arrays,
  interfaces) when a thread exits. Engine units that keep per-thread managed
  state in a threadvar register a parameterless cleanup proc here once, in
  their unit initialization. Goccia.Threading.ShutdownThreadRuntime drains the
  registry on each worker thread before it exits, and this unit's finalization
  drains it on the main thread at process shutdown, so every callback releases
  the calling thread's own threadvar copy on both paths.

  Threading contract: RegisterThreadvarCleanup is only ever called from unit
  initialization sections, which FPC runs sequentially on the main thread
  before the program body spawns any worker thread. The callback list is
  therefore written once during startup and only read afterwards, so the
  concurrent worker-thread reads in RunThreadvarCleanups need no lock. }

unit Goccia.ThreadCleanupRegistry;

{$I Goccia.inc}

interface

type
  { Parameterless cleanup callback. Must release only the calling thread's own
    managed threadvars (e.g. SetLength(FMembers, 0)); it runs on whichever
    thread drains the registry, so it must not touch another thread's state. }
  TGocciaThreadvarCleanupProc = procedure;

{ Register a threadvar-cleanup callback. Call once per unit, from the unit's
  initialization section (before any worker thread is spawned). A nil callback
  is ignored. }
procedure RegisterThreadvarCleanup(const AProc: TGocciaThreadvarCleanupProc);

{ Run every registered cleanup on the calling thread. Drained by
  ShutdownThreadRuntime (worker threads) and this unit's finalization (main
  thread). Safe to call multiple times and on any thread. }
procedure RunThreadvarCleanups;

implementation

const
  CLEANUPS_INITIAL_CAPACITY = 8;

var
  { Write-once at unit initialization (single-threaded), read-only afterwards.
    See the threading contract in the unit header. Capacity grows by doubling so
    registration stays amortised O(1); GCleanupCount is the live length. }
  GCleanups: array of TGocciaThreadvarCleanupProc;
  GCleanupCount: Integer;

procedure RegisterThreadvarCleanup(const AProc: TGocciaThreadvarCleanupProc);
begin
  if not Assigned(AProc) then
    Exit;
  if GCleanupCount >= Length(GCleanups) then
  begin
    if Length(GCleanups) = 0 then
      SetLength(GCleanups, CLEANUPS_INITIAL_CAPACITY)
    else
      SetLength(GCleanups, Length(GCleanups) * 2);
  end;
  GCleanups[GCleanupCount] := AProc;
  Inc(GCleanupCount);
end;

procedure RunThreadvarCleanups;
var
  I: Integer;
begin
  for I := 0 to GCleanupCount - 1 do
    GCleanups[I]();
end;

finalization
  RunThreadvarCleanups;

end.
