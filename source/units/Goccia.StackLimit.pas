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

  // A prototype walk loops over ordinary objects, so a chain of them costs no
  // native stack however long it is. A Proxy in the chain is entered by a real
  // native call instead, and so is a Proxy's target. A prototype cycle closed
  // through a Proxy, which ES2026 §10.1.2.1 OrdinarySetPrototypeOf cannot
  // refuse, repeats those calls without end, and Proxies nested deeply enough
  // do the same to the native stack. This cap on the calls into a Proxy that
  // are live at once turns both into a RangeError.
  MAX_PROPERTY_DELEGATION_DEPTH = 1000;

  // The same walk enters an array, function, class or class instance by a
  // native call too, because their classes override the lookup. Those calls
  // are counted separately against this much larger cap: a long chain of such
  // objects reads as it always did, and a prototype cycle through a Proxy that
  // holds many of them still ends in a RangeError before the native stack
  // does.
  MAX_OBJECT_DELEGATION_DEPTH = 3000;

  // OrdinaryHasInstance and Object.prototype.isPrototypeOf follow
  // [[GetPrototypeOf]] in a loop. Through a Proxy that loop can run forever,
  // either around a prototype cycle or because a getPrototypeOf trap keeps
  // producing new proxies; past this many Proxy steps it ends in a RangeError.
  // Steps over ordinary objects are not counted: their chain is finite.
  MAX_PROXY_PROTOTYPE_STEPS = 100000;

procedure SetMaxStackDepth(const AMaxDepth: Integer);
procedure CheckStackDepth(const ACurrentDepth: Integer);
procedure CheckNativeReentryDepth(const ADepth: Integer);
// The throw the checks share. It is exported only because FPC does not inline
// a procedure into another unit when it calls one that is local to the
// implementation section, and the two checks are small enough to be inlined
// into their callers.
procedure ThrowMaxCallStackExceeded;

// Bracket one native call into a Proxy's [[Get]], [[Set]] or
// [[HasProperty]]. EnterPropertyDelegation throws before it counts, so a
// caller pairs it with LeavePropertyDelegation in a try/finally that starts
// after it.
procedure EnterPropertyDelegation;
procedure LeavePropertyDelegation;
// The same for a call into any other object whose class overrides the lookup,
// counted against MAX_OBJECT_DELEGATION_DEPTH.
procedure EnterObjectDelegation;
procedure LeaveObjectDelegation;
procedure CheckProxyPrototypeSteps(const AProxySteps: Integer);

implementation

uses
  Goccia.Error.Messages,
  Goccia.Values.ErrorHelper;

var
  GMaxStackDepth: Integer;

threadvar
  GPropertyDelegationDepth: Integer;
  GObjectDelegationDepth: Integer;

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

procedure EnterPropertyDelegation;
begin
  if GPropertyDelegationDepth >= MAX_PROPERTY_DELEGATION_DEPTH then
    ThrowMaxCallStackExceeded;
  Inc(GPropertyDelegationDepth);
end;

procedure LeavePropertyDelegation;
begin
  Dec(GPropertyDelegationDepth);
end;

procedure EnterObjectDelegation;
begin
  if GObjectDelegationDepth >= MAX_OBJECT_DELEGATION_DEPTH then
    ThrowMaxCallStackExceeded;
  Inc(GObjectDelegationDepth);
end;

procedure LeaveObjectDelegation;
begin
  Dec(GObjectDelegationDepth);
end;

procedure CheckProxyPrototypeSteps(const AProxySteps: Integer);
begin
  if AProxySteps > MAX_PROXY_PROTOTYPE_STEPS then
    ThrowMaxCallStackExceeded;
end;

end.
