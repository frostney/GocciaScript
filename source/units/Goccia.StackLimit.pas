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

  // OrdinaryHasInstance and Object.prototype.isPrototypeOf follow
  // [[GetPrototypeOf]] in a loop. Through a Proxy that loop can run forever,
  // either around a prototype cycle or because a getPrototypeOf trap keeps
  // producing new proxies; past this many Proxy steps it ends in a RangeError.
  // Steps over ordinary objects are not counted: their chain is finite.
  MAX_PROXY_PROTOTYPE_STEPS = 100000;

procedure SetMaxStackDepth(const AMaxDepth: Integer);
procedure CheckStackDepth(const ACurrentDepth: Integer);
procedure CheckNativeReentryDepth(const ADepth: Integer);

// Bracket one native call into a Proxy's [[Get]], [[Set]] or
// [[HasProperty]]. EnterPropertyDelegation throws before it counts, so a
// caller pairs it with LeavePropertyDelegation in a try/finally that starts
// after it.
procedure EnterPropertyDelegation;
procedure LeavePropertyDelegation;
procedure CheckProxyPrototypeSteps(const AProxySteps: Integer);

implementation

uses
  Goccia.Error.Messages,
  Goccia.Values.ErrorHelper;

var
  GMaxStackDepth: Integer;

threadvar
  GPropertyDelegationDepth: Integer;

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

procedure EnterPropertyDelegation;
begin
  if GPropertyDelegationDepth >= MAX_PROPERTY_DELEGATION_DEPTH then
    ThrowRangeError(SErrorMaxCallStackExceeded);
  Inc(GPropertyDelegationDepth);
end;

procedure LeavePropertyDelegation;
begin
  Dec(GPropertyDelegationDepth);
end;

procedure CheckProxyPrototypeSteps(const AProxySteps: Integer);
begin
  if AProxySteps > MAX_PROXY_PROTOTYPE_STEPS then
    ThrowRangeError(SErrorMaxCallStackExceeded);
end;

end.
