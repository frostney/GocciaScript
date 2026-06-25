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
  // configurable --stack-size limit (which bounds the much cheaper trampolined
  // JS frames). Generator-mediated infinite recursion now throws a RangeError
  // instead of crashing the engine.
  MAX_NATIVE_REENTRY_DEPTH = 512;

procedure SetMaxStackDepth(const AMaxDepth: Integer);
procedure CheckStackDepth(const ACurrentDepth: Integer);
procedure CheckNativeReentryDepth(const ADepth: Integer);

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

procedure CheckStackDepth(const ACurrentDepth: Integer);
begin
  if (GMaxStackDepth > 0) and (ACurrentDepth > GMaxStackDepth) then
    ThrowRangeError(SErrorMaxCallStackExceeded);
end;

procedure CheckNativeReentryDepth(const ADepth: Integer);
begin
  if ADepth > MAX_NATIVE_REENTRY_DEPTH then
    ThrowRangeError(SErrorMaxCallStackExceeded);
end;

end.
