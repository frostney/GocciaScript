unit Goccia.StackLimit;

{$I Goccia.inc}

interface

const
  DEFAULT_MAX_STACK_DEPTH = 2200;

  // Native re-entry (generator resume, host eval, and native callbacks such as
  // Array iteration methods or sort comparators) runs the VM on a fresh native
  // stack frame instead of the trampolined frame stack. Each level therefore
  // costs a real native stack frame (kilobytes), and the 8 MB thread stack is
  // exhausted after roughly 1000-1500 levels -> SIGSEGV. This fixed cap fences
  // that off well before the native stack runs out, independent of the
  // configurable --max-stack limit (which bounds the much cheaper trampolined
  // JS frames). Generator-mediated infinite recursion now throws a RangeError
  // instead of crashing the engine.
  MAX_NATIVE_REENTRY_DEPTH = 512;

procedure SetMaxStackDepth(const AMaxDepth: Integer);
procedure CheckStackDepth(const ACurrentDepth: Integer);
procedure CheckNativeReentryDepth(const ADepth: Integer);
// The throw both checks share. It is exported only because FPC does not inline
// a procedure into another unit when it calls one that is local to the
// implementation section, and the two checks are small enough to be inlined
// into their callers.
procedure ThrowMaxCallStackExceeded;

implementation

uses
  Goccia.Error.Messages,
  Goccia.Values.ErrorHelper;

var
  GMaxStackDepth: Integer;

procedure SetMaxStackDepth(const AMaxDepth: Integer);
begin
  GMaxStackDepth := AMaxDepth;
end;

// Loading the resource string needs a managed temporary, and a procedure that
// has one installs an implicit exception frame on every call. The throw lives
// here so that the two checks, which run on each call, stay without one
// (docs/core-patterns.md, "Managed Locals on Hot Paths").
//
// Production builds switch on FPC's automatic inlining (Shared.inc), which
// folds a procedure this small back into its callers and brings the frame
// with it, so it is switched off for this one procedure. {$PUSH} and {$POP}
// do not save optimizer switches in FPC 3.2.2; the switch is turned back on
// explicitly, under the condition Shared.inc turns it on.
{$IFDEF FPC}{$OPTIMIZATION NOAUTOINLINE}{$ENDIF}
procedure ThrowMaxCallStackExceeded;
begin
  ThrowRangeError(SErrorMaxCallStackExceeded);
end;
{$IFDEF PRODUCTION}
  {$IFDEF FPC}
    {$OPTIMIZATION AUTOINLINE}
  {$ENDIF}
{$ENDIF}

procedure CheckStackDepth(const ACurrentDepth: Integer);
begin
  if (GMaxStackDepth > 0) and (ACurrentDepth > GMaxStackDepth) then
    ThrowMaxCallStackExceeded;
end;

procedure CheckNativeReentryDepth(const ADepth: Integer);
begin
  if ADepth > MAX_NATIVE_REENTRY_DEPTH then
    ThrowMaxCallStackExceeded;
end;

end.
