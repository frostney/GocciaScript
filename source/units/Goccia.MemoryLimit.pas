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
    can observe — the VM's async-iterator and dynamic-import arms, the
    microtask, await, generator
    and promise-reaction boundaries, Array.fromAsync, the sandbox's
    fs.promises and shell boundaries, Response.json, the benchmark run loop,
    and the test library's toThrow — names this class in its re-raise
    allowlist ahead of its generic `on E: Exception` arm, either as an
    explicit `on E: TGocciaMemoryLimitError do raise;` or through the shared
    IsUncatchableFault predicate in Goccia.UncatchableFault.pas. A new
    boundary that omits it makes the ceiling catchable again, which is what
    Goccia.MemoryLimit.Test.pas guards against.
    Prefer the shared predicate at new boundaries: an allowlist spelled out by
    hand is one that can be spelled out incompletely.

    The same boundary list carries a second, wider guard: where the allowlist
    above names a class, the generic arm opens with
    `if IsEngineFault(E) then raise;`. This ceiling is opaque because
    a ceiling the guest can catch is one it can ignore; an integrity fault is
    opaque for a stronger reason still, and is the backstop for the failure
    this family cannot describe — a temporary left unrooted across a
    collecting safe point surfaces as a use-after-free, which without the guard
    `catch (e)` absorbs. The family, its deliberate exclusions and the
    reasoning live in Goccia.EngineFault.pas,
    docs/adr/0109-engine-integrity-faults-are-uncatchable.md and
    docs/adr/0198-every-application-stops-on-an-engine-fault.md. }
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

implementation

constructor TGocciaMemoryLimitError.Create(const ARequestedBytes,
  ABudgetBytes: Int64);
begin
  inherited CreateFmt(
    'Allocation of %d bytes would exceed the memory budget of %d bytes',
    [ARequestedBytes, ABudgetBytes]);
  FRequestedBytes := ARequestedBytes;
  FBudgetBytes := ABudgetBytes;
end;

end.
