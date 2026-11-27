unit Goccia.EngineFault;

{$I Goccia.inc}

interface

uses
  SysUtils;

{ Answers whether AException is an engine-integrity fault: a signal that the
  process state is no longer trustworthy, not something the running script did.
  This unit is the authoritative list; the decision is recorded in
  docs/adr/0109-engine-integrity-faults-are-uncatchable.md, and what every
  application does with a fault that reaches it in
  docs/adr/0196-every-application-stops-on-an-engine-fault.md.

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
    EAssertionFailed   An internal assertion (`Assert`, compiled in by `$C+`)
                       found an engine or host invariant broken. Nothing a
                       script does is meant to trip one, so one that fires is
                       an engine bug, not a guest error.
    EAbstractError     A call reached an abstract method: a dispatch the engine
                       never meant to make.

  Deliberately NOT in the family — each is raised by the engine on a path with a
  defined continuation, so classifying it as an integrity fault would turn
  working behavior into a fatal exit:

    EStackOverflow     Descends from EExternal alongside EAccessViolation, but
                       the engine's script boundaries convert it into a
                       catchable guest error like any other non-fault Pascal
                       exception. Recursion depth is a guest-observable
                       limit, not corruption. No class listed above is an ancestor of it,
                       so it keeps converting. One that reaches a host, past
                       every script boundary, is an engine fault all the same:
                       see IsEngineFault below.
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

const
  { The exit status of a process that stopped on an engine fault: sysexits'
    EX_SOFTWARE, "an internal software error has been detected". Distinct from
    1 (the work was done and failed: a script threw, a test failed) and 2 (the
    invocation was unusable), so a harness can tell "this failed" from "stop
    believing this process" (docs/contributing/cli-conventions.md). }
  EXIT_CODE_ENGINE_FAULT = 70;

  { Opens every line of the diagnostic AbortOnEngineFault writes. No other
    line any application prints starts with it, so `grep 'Engine fault:'` over
    a log finds the fault and nothing else. }
  ENGINE_FAULT_PREFIX = 'Engine fault: ';

{ Answers whether AException, having reached a host past every script
  boundary, is an engine fault: the integrity family above, or an
  EStackOverflow. A script's own recursion never surfaces as one: the engine
  bounds it and reports a RangeError the script can catch. An EStackOverflow
  that a host sees is native code that outran its stack without the engine's
  bound noticing, which is the engine failing, not the script.

  Everything else a host catches is an ordinary failure with its own report:
  a script error, a file that does not exist, an invalid option. This is the
  test hosts apply in every generic handler that would otherwise report the
  exception as one of those (docs/adr/0196-every-application-stops-on-an-engine-fault.md). }
function IsEngineFault(const AException: Exception): Boolean;

{ Writes the engine-fault diagnostic for AException to stderr and ends the
  process with EXIT_CODE_ENGINE_FAULT. Never returns. ASite names where the
  fault surfaced: the file whose run it ended, or the application's name when
  it came from outside any file.

  It runs from whichever thread caught the fault, worker threads included,
  and does not unwind. Unwinding would run the handlers and finalizers between
  here and the program's end on state the fault calls into question, and
  would let the caller go on to print a summary or a JSON report, whose
  verdict the process is in no position to give.

  The first fault in the process writes the diagnostic: two lines, each
  opening with ENGINE_FAULT_PREFIX, the first naming the exception's class, the
  site and the message. A later fault on another thread writes nothing and
  waits, up to a bound, for that diagnostic to be flushed before it ends the
  process, so the abort never cuts its own message short. The calling
  thread's buffered stdout is flushed first, so what the script printed before
  the fault comes out ahead of the diagnostic; what other threads still hold
  dies with the process. }
procedure AbortOnEngineFault(const ASite: string; const AException: Exception);

{ Ends the process at once with AStatus, running no unit finalization and
  flushing nothing: _exit, or TerminateProcess on Windows, which unlike
  ExitProcess also skips DLL process-detach handlers that could deadlock on a
  lock a dead thread held. Halt would run unit finalization on the calling
  thread and tear down process-wide RTL state (the thread manager among it)
  under the threads still running. }
procedure TerminateProcessNow(AStatus: LongInt);

{ Raises the exception the RTL maps run-time error AErrno to (SysUtils'
  ErrorProc: 204 is EInvalidPointer, 202 EStackOverflow, 216
  EAccessViolation), through the path the RTL's own internal errors take
  (FPC_HANDLEERROR). An engine check that finds its invariants broken calls
  this rather than RunError: RunError is a program's own exit request, which
  runs Halt, with unit finalization, on the calling thread and raises nothing
  a host could report. }
procedure RaiseRuntimeError(AErrno: LongInt);

implementation

procedure FpcHandleError(AErrno: LongInt); external name 'FPC_HANDLEERROR';

procedure RaiseRuntimeError(AErrno: LongInt);
begin
  FpcHandleError(AErrno);
end;

function IsEngineIntegrityFault(const AException: Exception): Boolean;
begin
  Result := (AException is EObjectCheck) or
    (AException is EAccessViolation) or
    (AException is EInvalidPointer) or
    (AException is EDivByZero) or
    (AException is EPrivilege) or
    (AException is EExternalException) or
    (AException is EAssertionFailed) or
    (AException is EAbstractError);
end;

function IsEngineFault(const AException: Exception): Boolean;
begin
  Result := IsEngineIntegrityFault(AException) or
    (AException is EStackOverflow);
end;

{$IF DEFINED(MSWINDOWS)}
function GetCurrentProcess: THandle; stdcall;
  external 'kernel32' name 'GetCurrentProcess';
function TerminateProcess(AProcess: THandle; AExitCode: LongWord): LongBool;
  stdcall; external 'kernel32' name 'TerminateProcess';

procedure TerminateProcessNow(AStatus: LongInt);
begin
  TerminateProcess(GetCurrentProcess, LongWord(AStatus));
end;
{$ELSEIF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
procedure TerminateProcessNow(AStatus: LongInt); cdecl; external name '_exit';
{$ELSE}
procedure TerminateProcessNow(AStatus: LongInt);
begin
  Halt(AStatus);
end;
{$IFEND}

const
  { How long, in milliseconds, a thread ending the process waits for the
    thread that won the report to finish writing it: generous next to one
    flushed write, and bounded so a reporter that dies mid-write cannot leave
    the process hanging. }
  DIAGNOSTIC_WAIT_MS = 2000;

var
  { Claimed, with an interlocked exchange, by the first thread to report a
    fault; never cleared. }
  GFaultReported: LongInt = 0;
  { Set by the reporting thread once its diagnostic is written and flushed. }
  GDiagnosticWritten: LongInt = 0;

procedure AbortOnEngineFault(const ASite: string; const AException: Exception);
var
  Deadline: QWord;
begin
  if InterlockedExchange(GFaultReported, 1) = 0 then
  begin
    { best effort throughout: a closed or full stream must not keep the
      process from ending with the fault's status (fault-guard: the process
      is already ending on a fault, and nothing below may stop that) }
    try
      Flush(Output);
    except
      { fault-guard: see above }
    end;
    try
      WriteLn(ErrOutput, ENGINE_FAULT_PREFIX + AException.ClassName + ' in ' +
        ASite + ': ' + AException.Message + LineEnding + ENGINE_FAULT_PREFIX +
        'the run was stopped because the engine can no longer vouch for its ' +
        'own state; no result of this run can be trusted.');
      Flush(ErrOutput);
    except
      { fault-guard: see above }
    end;
    InterlockedExchange(GDiagnosticWritten, 1);
  end;
  Deadline := GetTickCount64 + DIAGNOSTIC_WAIT_MS;
  while (GDiagnosticWritten = 0) and (GetTickCount64 < Deadline) do
    Sleep(1);
  TerminateProcessNow(EXIT_CODE_ENGINE_FAULT);
end;

end.
