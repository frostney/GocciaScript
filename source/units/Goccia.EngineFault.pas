unit Goccia.EngineFault;

{$I Goccia.inc}

interface

uses
  SysUtils;

{ Answers whether AException is an engine-integrity fault: a signal that the
  process state is no longer trustworthy, not something the running script did.
  This unit is the authoritative list; the decision is recorded in
  docs/adr/0109-engine-integrity-faults-are-uncatchable.md.

  These are the faults that mean a pointer was already freed, a mapping was
  never valid, or the heap itself is broken. Unlike a TypeError or a refused
  allocation, they carry no defined continuation — the only correct response is
  to stop, and the guest must not be able to prevent that.

  Why the predicate exists at all: every guest-visible boundary in the engine
  ends in a generic `on E: Exception` arm that turns a Pascal exception into a
  script value (an Error object, a promise rejection, a recorded test failure).
  That arm is written for the engine's own domain exceptions. When an integrity
  fault reaches it, the arm converts a use-after-free into an ordinary catch block, and
  the guest carries on running on top of corrupted memory. This is the same
  argument the limit family already makes for TGocciaMemoryLimitError (see
  Goccia.MemoryLimit.pas): a fault the guest can catch is a fault it can ignore.

  Dev builds compile with `$OBJECTCHECKS ON` (source/shared/Shared.inc), so a
  virtual call through a collected TGocciaValue surfaces as EObjectCheck; a
  production build with the check off reaches the same code as EAccessViolation
  or a silent corruption. Both spellings of the same bug belong here.

  The family, and what each covers:

    EObjectCheck       Virtual dispatch through a nil or freed instance
                       (`$OBJECTCHECKS ON`). Descends directly from Exception.
    EAccessViolation   An invalid dereference. Covers its descendant EBusError
                       (misaligned or unmapped access).
    EInvalidPointer    Freeing or resizing a pointer the heap does not own —
                       a double free, or a write that ran past a block and
                       destroyed the allocator's own bookkeeping. The heap
                       manager cannot continue truthfully from there.
    EDivByZero         Integer division by zero. No JavaScript operator produces
                       one: `/` and `%` are IEEE-754 double operations, and
                       BigInt division checks its divisor before dividing and
                       throws a guest RangeError (Goccia.Arithmetic.pas). An
                       EDivByZero reaching a boundary is therefore an engine bug
                       in native index or size arithmetic.
    EPrivilege         A privileged instruction was executed.
    EExternalException A hardware or OS exception FPC could not map to a more
                       specific class.

  Deliberately NOT in the family — each is raised by the engine on a path with a
  defined continuation, so classifying it as an integrity fault would turn
  working behavior into a fatal exit:

    EStackOverflow     Descends from EExternal alongside EAccessViolation, but
                       the engine converts it to the guest RangeError
                       "Maximum call stack size exceeded" on purpose
                       (PascalExceptionToErrorObject in Goccia.Evaluator.pas).
                       Recursion depth is a guest-observable limit, not
                       corruption. No class listed above is an ancestor of it,
                       so it keeps converting.
    EOutOfMemory       A refusal, not a corruption: the allocator declined a
                       request and left the heap exactly as it was. FPC 3.2.2
                       raises it from a failed SetLength, and the engine's own
                       handlers unwind cleanly through it — ArrayBufferValue's
                       SetDataLength releases the bytes it had already reserved
                       before re-raising, so the byte accounting stays true. A
                       guest that asks for more than the process can give must
                       keep seeing an ordinary error: on 32-bit targets the
                       700 MB budget cap sits well below the address space, so
                       catching the failure of a 600 MB `new ArrayBuffer` and
                       carrying on with a smaller one is a realistic program,
                       not a broken one. Its sibling
                       EInvalidPointer above is in the family precisely because
                       it is the other half of EHeapMemoryError: a refusal has a
                       continuation, a corrupted heap does not. One engine path
                       also raises EOutOfMemory as a pure domain signal — FFI
                       callback-slot exhaustion in Goccia.FFI.CallbackSlots.pas,
                       converted to a guest RangeError by its own caller in
                       Goccia.Values.FFICallback.pas.
    ERangeError        Also an EIntError, but the engine raises it deliberately
                       as argument validation — Goccia.Bytecode.Chunk.pas,
                       OrderedMap.pas, OrderedStringMap.pas and
                       Goccia.SourcePipeline.pas all use it for index bounds.
                       It does not unambiguously mean corruption.
    EIntOverflow       Fires from `$overflowchecks on`, which Shared.inc enables
                       in non-production builds only. Making it fatal would give
                       dev and production builds different unwind behavior for
                       the same arithmetic.
    EConvertError      A normal-operation signal: invalid RegExp flags
                       (Goccia.RegExp.Engine.pas), malformed TOML integers
                       (Goccia.TOML.pas), invalid base64, invalid UTF-8. It must
                       stay convertible into a guest-visible error.
    EInvalidOpException /
    EInvalidOperation  Raised deliberately by FFI, module and engine code for
                       domain invariants (Goccia.FFI.Call.pas,
                       Goccia.Values.FFICallback.pas, Goccia.Modules.Loader.pas,
                       Goccia.Modules.Virtual.pas, Goccia.Engine.pas).
    EInvalidCast       Nothing in the engine raises one today, but it is an
                       ordinary type-assertion failure rather than evidence the
                       heap is unsound, so it stays convertible.

  Callers use this at a conversion boundary, as the first statement of the
  generic arm, so the bare `raise` re-raises inside the handler that caught it:

      on E: Exception do
      begin
        if IsEngineIntegrityFault(E) then
          raise;
        ...convert E into a guest value...
      end; }
function IsEngineIntegrityFault(const AException: Exception): Boolean;

implementation

function IsEngineIntegrityFault(const AException: Exception): Boolean;
begin
  Result := (AException is EObjectCheck) or
    (AException is EAccessViolation) or
    (AException is EInvalidPointer) or
    (AException is EDivByZero) or
    (AException is EPrivilege) or
    (AException is EExternalException);
end;

end.
