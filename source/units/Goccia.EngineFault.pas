unit Goccia.EngineFault;

{$I Goccia.inc}

interface

uses
  SysUtils;

{ Answers whether AException is an engine fault: a signal that the process
  state is no longer trustworthy, not something the running script did. This
  unit is the authoritative list; the decisions are recorded in
  docs/adr/0109-engine-integrity-faults-are-uncatchable.md and
  docs/adr/0196-every-application-stops-on-an-engine-fault.md.

  These are the faults that mean a pointer was already freed, a mapping was
  never valid, the heap itself is broken, or an invariant the engine checks
  does not hold. Unlike a TypeError or a refused allocation, they carry no
  defined continuation: the only correct response is to stop, and the guest
  must not be able to prevent that.

  Why the predicate exists at all: a guest-visible boundary in the engine can
  end in a generic `on E: Exception` arm that turns a Pascal exception into a
  script value (an Error object, a promise rejection, a recorded test failure),
  and a host can end in one that reports it as an ordinary failure. Such an arm
  is written for the domain exceptions. When a fault reaches it, the arm turns
  a use-after-free into an ordinary catch block or a failed file, and the run
  carries on on top of corrupted memory.

  The family, and what each covers:

    EObjectCheck       Virtual dispatch through a nil or freed instance
                       (`$OBJECTCHECKS ON` in the development build of the
                       host units). Descends directly from Exception.
    EAccessViolation   An invalid dereference. Covers its descendant EBusError
                       (misaligned or unmapped access).
    EInvalidPointer    Freeing or resizing a pointer the heap does not own (a
                       double free, or a write that ran past a block and
                       destroyed the allocator's own bookkeeping), and the
                       engine's own check that a native's root bookkeeping is
                       off (Goccia.Core). The heap cannot continue truthfully
                       from there.
    EDivByZero         Integer division by zero. No JavaScript operator produces
                       one: `/` and `%` are IEEE-754 double operations, and
                       BigInt division checks its divisor and throws a guest
                       RangeError first (Goccia.VM). An EDivByZero is therefore
                       an engine bug in native index or size arithmetic.
    EPrivilege         A privileged instruction was executed.
    EExternalException A hardware or OS exception FPC could not map to a more
                       specific class.
    EAssertionFailed   An internal assertion (`Assert`, compiled in by `$C+`)
                       found an invariant broken. Nothing a script does is
                       meant to trip one.
    EAbstractError     A call reached an abstract method: a dispatch the engine
                       never meant to make.
    EStackOverflow     Native code ran out of stack. The engine bounds every
                       recursion a script can drive (call frames, the parser's
                       nesting, JSON, RegExp) and reports the guest RangeError
                       "Maximum call stack size exceeded" itself, so no path
                       relies on this exception becoming a script error: one
                       that is raised means a native recursion outran the
                       engine's bound.

  Deliberately NOT in the family: each is raised on a path with a defined
  continuation, so classifying it as a fault would turn working behavior into
  a fatal exit.

    EOutOfMemory       A refusal, not a corruption: the allocator declined a
                       request and left the heap as it was. A guest that asks
                       for more than the process can give keeps seeing an
                       ordinary error. One engine path also raises it as a pure
                       domain signal: FFI callback-slot exhaustion
                       (Goccia.FFI.CallbackSlots), which Goccia.FFI converts to
                       a guest RangeError. Its sibling EInvalidPointer is in the
                       family because it is the other half of
                       EHeapMemoryError: a refusal has a continuation, a
                       corrupted heap does not.
    ERangeError        Also an EIntError, but raised deliberately as argument
                       validation (OrderedMap, OrderedStringMap and
                       Goccia.SourcePipeline use it for index bounds).
    EIntOverflow       Fires from `$overflowchecks on`, which Shared.inc enables
                       in development builds only. Making it fatal would give
                       development and production builds different unwind
                       behavior for the same arithmetic.
    EConvertError      A normal-operation signal: invalid RegExp flags
                       (Goccia.RegExp.Engine), malformed TOML integers
                       (Goccia.TOML), invalid base64, invalid UTF-8.
    EInvalidOpException /
    EInvalidOperation  Raised deliberately for domain invariants
                       (Goccia.FFI.Call, Goccia.Modules.Virtual, Goccia.Engine).
    EInvalidCast       An ordinary type-assertion failure rather than evidence
                       the heap is unsound.

  At a script boundary the generic arm opens by re-raising, so the bare
  `raise` re-raises inside the handler that caught it:

      on E: Exception do
      begin
        if IsEngineFault(E) then
          raise;
        ...convert E into a guest value...
      end;

  A host, or a thread whose exception would otherwise end it silently, stops
  the process instead: `if IsEngineFault(E) then AbortOnEngineFault(Site, E);`.
  scripts/check-fault-guards.ts holds every catch-all handler to one of the
  two. }
function IsEngineFault(const AException: Exception): Boolean;

const
  { The exit status of a process that stopped on an engine fault: sysexits'
    EX_SOFTWARE, "an internal software error has been detected". Distinct from
    1 (the work was done and failed: a script threw, a test failed) and 2 (the
    invocation was unusable), so a harness can tell "this failed" from "stop
    believing this process" (docs/contributing/cli-conventions.md). }
  EXIT_CODE_ENGINE_FAULT = 70;

  { Opens every line of the diagnostic this unit writes. No other line any
    application prints starts with it, so `grep 'Engine fault:'` over a log
    finds the fault and nothing else. }
  ENGINE_FAULT_PREFIX = 'Engine fault: ';

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
  waits, up to a bound, for that diagnostic before it ends the process, so the
  abort never cuts its own message short. The calling thread's buffered stdout
  is flushed first, so what the script printed before the fault comes out
  ahead of the diagnostic; what other threads still hold dies with the process.

  Output can fail, and the exit status must not: a write to a closed pipe
  returns an error rather than raising SIGPIPE, and a write that blocks (a full
  pipe nobody reads) cannot hold the process past the bound, because a
  watchdog thread ends it then. }
procedure AbortOnEngineFault(const ASite: string; const AException: Exception);

{ The same stop for one of the engine's own integrity checks, which has
  already written what it found (a heap cell, a shape, a root count) to
  stderr: the two diagnostic lines name the check, and the process ends with
  EXIT_CODE_ENGINE_FAULT without unwinding. For a check that finds the heap
  broken in the middle of a collection, where raising would unwind through
  the collector. Never returns. }
procedure StopOnEngineCheck(const ACheck: string);

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
  (FPC_HANDLEERROR). An engine check that finds its invariants broken outside
  a collection calls this rather than RunError: RunError is a program's own
  exit request, which runs Halt, with unit finalization, on the calling thread
  and raises nothing a host could report. }
procedure RaiseRuntimeError(AErrno: LongInt);

implementation

{$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
uses
  BaseUnix;
{$IFEND}

procedure FpcHandleError(AErrno: LongInt); external name 'FPC_HANDLEERROR';

procedure RaiseRuntimeError(AErrno: LongInt);
begin
  FpcHandleError(AErrno);
end;

function IsEngineFault(const AException: Exception): Boolean;
begin
  Result := (AException is EObjectCheck) or
    (AException is EAccessViolation) or
    (AException is EInvalidPointer) or
    (AException is EDivByZero) or
    (AException is EPrivilege) or
    (AException is EExternalException) or
    (AException is EAssertionFailed) or
    (AException is EAbstractError) or
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
  { How long, in milliseconds, the process may spend on the diagnostic: the
    bound a thread that lost the report waits for the winner, and the time
    after which the watchdog ends a winner whose write blocks. Generous next to
    one flushed write. }
  DIAGNOSTIC_WAIT_MS = 2000;

var
  { Claimed, with an interlocked exchange, by the first thread to report a
    fault; never cleared. }
  GFaultReported: LongInt = 0;
  { Set by the reporting thread once its diagnostic is written and flushed. }
  GDiagnosticWritten: LongInt = 0;

{ The watchdog's whole body: a write that blocks past the bound does not keep
  the process from ending with the fault's status. }
function WatchdogThread(AParameter: Pointer): PtrInt;
begin
  Sleep(DIAGNOSTIC_WAIT_MS);
  TerminateProcessNow(EXIT_CODE_ENGINE_FAULT);
  Result := 0;
end;

procedure ReportAndTerminate(const AFirstLine: string);
var
  Deadline: QWord;
begin
  if InterlockedExchange(GFaultReported, 1) = 0 then
  begin
    { fault-guard throughout: the process is already ending on a fault, and a
      failed write must not stop that }
    {$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
    { a closed pipe fails the write with EPIPE instead of killing the process }
    fpSignal(SIGPIPE, SignalHandler(SIG_IGN));
    {$IFEND}
    try
      BeginThread(@WatchdogThread);
    except
      { fault-guard: without a watchdog the write is merely unbounded }
    end;
    try
      Flush(Output);
    except
      { fault-guard: see above }
    end;
    try
      WriteLn(ErrOutput, ENGINE_FAULT_PREFIX + AFirstLine + LineEnding + ENGINE_FAULT_PREFIX +
        'the run was stopped because the engine can no longer vouch for its own state; no result of this run can ' +
        'be trusted.');
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

procedure AbortOnEngineFault(const ASite: string; const AException: Exception);
begin
  ReportAndTerminate(AException.ClassName + ' in ' + ASite + ': ' + AException.Message);
end;

procedure StopOnEngineCheck(const ACheck: string);
begin
  ReportAndTerminate('the engine''s ' + ACheck + ' check failed (see above)');
end;

end.
