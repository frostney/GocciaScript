unit Goccia.MemoryLimit;

{$I Goccia.inc}

interface

uses
  SysUtils;

type
  { Raised when an allocation would push the engine past its memory budget.

    Mirrors TGocciaInstructionLimitError: a host-catchable Pascal exception,
    distinct from anything script code can produce. That distinction is the
    point. The budget previously surfaced only as a script-visible RangeError,
    which a host cannot tell apart from a RangeError the script threw itself —
    so "the sandbox hit its ceiling" and "the guest called new Array(-1)"
    arrived through the same channel and could not be reported differently.

    This does not disturb the pre-existing paths. Allocation sites that
    already charged the budget (string payloads, ArrayBuffer resize) still
    raise the script-visible RangeError they always did, so in-language error
    handling is unchanged there.

    This error is raised only by the gate below, and is deliberately NOT
    catchable from script. That matches the rest of the limit family —
    TGocciaInstructionLimitError and TGocciaTimeoutError are likewise opaque
    to the guest — because a resource ceiling the guest can catch is a
    resource ceiling the guest can ignore in a loop. The gate guards paths
    that had no check at all before, so nothing that used to be catchable
    stops being catchable.

    Opacity is a property of the handlers, not of the class. Every boundary
    that would otherwise convert a Pascal exception into something the guest
    can observe — the evaluator's try/catch statement paths, the VM's
    async-iterator and dynamic-import arms, the microtask, await, generator
    and promise-reaction boundaries, Array.fromAsync, the sandbox's
    fs.promises and shell boundaries, Response.json, the benchmark run loop,
    and the test library's toThrow — names this class in its re-raise
    allowlist ahead of its generic `on E: Exception` arm, either as an
    explicit `on E: TGocciaMemoryLimitError do raise;` or through the shared
    IsUncatchableFault predicate in Goccia.UncatchableFault.pas. A new
    boundary that omits it makes the ceiling catchable again, which is what
    Goccia.MemoryLimit.Test.pas guards against in both execution modes.
    Prefer the shared predicate at new boundaries: an allowlist spelled out by
    hand is one that can be spelled out incompletely.

    The same boundary list carries a second, wider guard: where the allowlist
    above names a class, the generic arm opens with
    `if IsEngineIntegrityFault(E) then raise;`. This ceiling is opaque because
    a ceiling the guest can catch is one it can ignore; an integrity fault is
    opaque for a stronger reason still, and is the backstop for the failure
    this family cannot describe — an evaluator temporary left unrooted across a
    collecting safe point surfaces as a use-after-free, which without the guard
    `catch (e)` absorbs. The family, its two deliberate carve-outs and the
    reasoning live in Goccia.EngineFault.pas and
    docs/adr/0109-engine-integrity-faults-are-uncatchable.md. }
  TGocciaMemoryLimitError = class(Exception)
  private
    FRequestedBytes: Int64;
    FBudgetBytes: Int64;
  public
    constructor Create(const ARequestedBytes, ABudgetBytes: Int64);
    { Size of the allocation that was refused. Zero when the refusing site
      could not attribute a size. }
    property RequestedBytes: Int64 read FRequestedBytes;
    { The ceiling in force when the refusal happened. }
    property BudgetBytes: Int64 read FBudgetBytes;
  end;

{ Pre-allocation gate for a natively-sized allocation.

  Answers "would ABytes fit in the remaining budget", WITHOUT charging it.
  Use where the allocation's lifetime is not owned by a value object that
  can release the charge again — extending an element list, growing property
  storage — so the budget still bounds the peak without leaking a permanent
  reservation the engine can never give back.

  Where an owner does exist (ArrayBuffer, string payloads), keep using
  TGarbageCollector.TryReserveExternalBytes and release in the destructor:
  that accounts the allocation for as long as it is live, which a gate
  cannot do.

  This reads the counter and nothing else — it never collects, so it is safe
  to ask from anywhere, including from a caller that has not made its own
  temporaries safe to collect across. RequireNativeBytes below does collect
  before it refuses, and states the obligation that comes with that. }
function CanAllocateNativeBytes(const ABytes: Int64): Boolean;

{ Raises TGocciaMemoryLimitError unless ABytes fits the remaining budget.

  Unlike CanAllocateNativeBytes, which only reads the counter, this refuses
  only once a collection has actually been attempted: a request that does not
  fit forces one full collection and re-tests the fit before raising. Whether a
  storage doubling is refused therefore depends on what the program is holding,
  not on how much collectable garbage happened to be resident when the growth
  arrived — measured, the same 4 MiB-ceiling property-growth loop that refused
  at a 2,359,200-byte doubling now reaches the arithmetic crossing at 4,718,496.

  Three shapes still refuse without walking the heap, because for them no
  collection could change the answer: a request larger than the whole budget, a
  repeat of a request the previous forced collection already refused at the
  current live level, and a re-entrant call from inside the collector. See
  TGarbageCollector.ShouldForceLimitCollection and ADR 0110.

  The forced collection is a real safe point, and this gate is reached from
  inside a property or element store. Callers must therefore have made the
  growth point safe to collect at BEFORE consulting the budget — which they do:
  TGocciaShapedPropertyMap.RequireStorageBytes and
  TGocciaElementList.RequireStorageBytes each open a TGocciaActiveRootFrame
  over the store's owner and its pending value and call the budget from inside
  it. That is why nothing is passed here to protect. }
procedure RequireNativeBytes(const ABytes: Int64);

implementation

uses
  Goccia.GarbageCollector;

constructor TGocciaMemoryLimitError.Create(const ARequestedBytes,
  ABudgetBytes: Int64);
begin
  inherited CreateFmt(
    'Allocation of %d bytes would exceed the memory budget of %d bytes',
    [ARequestedBytes, ABudgetBytes]);
  FRequestedBytes := ARequestedBytes;
  FBudgetBytes := ABudgetBytes;
end;

function CanAllocateNativeBytes(const ABytes: Int64): Boolean;
var
  GC: TGarbageCollector;
  LiveBytes: Int64;
begin
  if ABytes <= 0 then
    Exit(True);
  GC := TGarbageCollector.Instance;
  { No GC means no budget to enforce — embedding hosts that never initialise
    one are unbounded by construction, and that is their choice to make. }
  if not Assigned(GC) or (GC.MaxBytes <= 0) then
    Exit(True);
  { One snapshot for both tests: a cross-thread release between two reads
    would split the decision across totals, and on 32-bit each read is a
    locked load. }
  LiveBytes := GC.BytesAllocated;
  { Overflow guard first: a JS-controlled length can multiply into a value
    that wraps, and a wrapped total would compare as comfortably in budget. }
  if LiveBytes > High(Int64) - ABytes then
    Exit(False);
  Result := LiveBytes + ABytes <= GC.MaxBytes;
end;

procedure RequireNativeBytes(const ABytes: Int64);
var
  GC: TGarbageCollector;
  Budget: Int64;
begin
  if CanAllocateNativeBytes(ABytes) then
    Exit;
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
  begin
    { Refuse only once a collection has actually been attempted. Without this
      the answer depends on how much collectable garbage happens to be resident
      when the growth arrives, not on what the program is holding: the same
      workload that refused a doubling at 32,766 entries under a 4 MiB ceiling
      reached 65,534 with a periodic explicit collection.
      TryCollectForLimitedBytes forces the collection, re-tests the fit once,
      and declines to walk the heap for the shapes no collection could change —
      a request larger than the whole budget, and a repeat of a request the
      previous forced collection already refused at the current live level.

      Nothing is passed to protect: the callers that reach this gate have
      already opened their rooting window around the consult (see the contract
      above), so a value this call could be handed is one they have rooted. }
    if GC.TryCollectForLimitedBytes(ABytes) then
      Exit;
    Budget := GC.MaxBytes;
  end
  else
    Budget := 0;
  raise TGocciaMemoryLimitError.Create(ABytes, Budget);
end;

end.
