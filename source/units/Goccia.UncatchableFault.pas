unit Goccia.UncatchableFault;

{$I Goccia.inc}

interface

uses
  SysUtils;

{ Answers whether AException must keep unwinding to the host instead of being
  converted into something script code can observe.

  Three separate contracts converge on one answer, which is why they are stated
  once here rather than re-spelled at each boundary:

    The resource ceilings — TGocciaTimeoutError, TGocciaInstructionLimitError,
    TGocciaMemoryLimitError — are opaque because a ceiling the guest can catch
    is a ceiling it can ignore in a loop. The full argument, and the inventory
    of boundaries that honour it, live in Goccia.MemoryLimit.pas.

    EGocciaCapabilityAuditDeliveryError is opaque because an embedder whose
    audit sink failed must not be told the guarded call merely failed; losing
    the record is the more serious event of the two, and only the host can
    decide what to do about it.

    The engine-integrity family is opaque for the strongest reason of all: it
    means a pointer was already freed or the heap is unsound, so there is no
    defined continuation to hand back. Goccia.EngineFault.pas is the
    authoritative list and docs/adr/0109-engine-integrity-faults-are-uncatchable.md
    records the decision.

  Why this unit exists on top of Goccia.EngineFault: that unit is deliberately
  SysUtils-only, so it cannot name the engine's own exception classes. This one
  can, and it keeps the union from being spelled a second and third way as new
  boundaries are hardened — a boundary that lists three of the four families is
  a hole that reads like a guard.

  Boundaries that predate this unit keep their explicit `on E: ... do raise;`
  arms: those arms carry per-class commentary about the specific path they
  guard, and the classes they name are exactly this set.

  Use it as the first statement of a generic conversion arm, so the bare
  `raise` re-raises inside the handler that caught it:

      on E: Exception do
      begin
        if IsUncatchableFault(E) then
          raise;
        ...convert E into a guest value...
      end;

  From a helper function called by such an arm — where a bare `raise` will not
  compile because the handler is not lexically enclosing — re-raise with
  `raise Exception(AcquireExceptionObject)` instead, so the in-flight exception
  is not freed out from under the second propagation. }
function IsUncatchableFault(const AException: Exception): Boolean;

implementation

uses
  Goccia.CapabilityAudit,
  Goccia.EngineFault,
  Goccia.InstructionLimit,
  Goccia.MemoryLimit,
  Goccia.Timeout;

function IsUncatchableFault(const AException: Exception): Boolean;
begin
  Result := (AException is TGocciaTimeoutError) or
    (AException is TGocciaInstructionLimitError) or
    (AException is TGocciaMemoryLimitError) or
    (AException is EGocciaCapabilityAuditDeliveryError) or
    IsEngineIntegrityFault(AException);
end;

end.
