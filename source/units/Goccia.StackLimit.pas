unit Goccia.StackLimit;

{$I Goccia.inc}

interface

const
  DEFAULT_MAX_STACK_DEPTH = 2200;

  // A native re-entry runs the bytecode loop on a fresh native stack frame
  // instead of the trampolined frame stack: a constructor, an async function,
  // a generator's next(), a getter, setter or Proxy trap, and a callback that
  // a built-in calls. Each one costs kilobytes of native stack, so how many
  // fit depends on the thread's stack size and on the build. The VM keeps
  // NATIVE_STACK_RESERVE bytes of the stack free and throws the RangeError of
  // --max-stack instead of entering once less than that remains, so no
  // --max-stack value, including 0, lets these re-entries overflow the native
  // stack. A chain of native calls that never enters the bytecode loop
  // (Proxies forwarding to their targets, a bound function between them, a
  // class extending a Proxy) is counted by the delegation caps below, which
  // check the same reserve once they nest. Native re-entries are function
  // calls, and count against --max-stack as well.
  NATIVE_STACK_RESERVE = 256 * 1024;

  // Where the bounds of the running thread's stack cannot be found, a native
  // re-entry is refused once this many are live instead.
  MAX_NATIVE_REENTRY_DEPTH = 512;

  // Native re-entries nested no deeper than this are not checked against the
  // stack: even in a development build they use less than 64 KiB of it.
  NATIVE_REENTRY_UNCHECKED_DEPTH = 8;

  // A caller's cached NativeStackLimit before it has been looked up.
  NATIVE_STACK_LIMIT_UNSET = High(NativeUInt);

  // A prototype walk loops over ordinary objects, so a chain of them costs no
  // native stack however long it is. A Proxy in the chain is entered by a real
  // native call instead, and so is a Proxy's target. A prototype cycle closed
  // through a Proxy, which ES2026 §10.1.2.1 OrdinarySetPrototypeOf cannot
  // refuse, repeats those calls without end, and Proxies nested deeply enough
  // do the same to the native stack. This cap on the calls into a Proxy that
  // are live at once turns both into a RangeError. An assignment or a `new`
  // through a nest of N Proxies goes down the nest twice (the innermost
  // [[Set]] defines the property on the outermost Proxy, and [[Construct]]
  // reads `prototype` from it), so N up to half the cap still completes.
  // A level costs more native stack when its trap is a native function
  // reached through a bound function or a Proxy (`Reflect.apply.bind(null)`,
  // `new Proxy(Reflect.apply, {})`): in a production build on Linux x86-64,
  // under the deepest recursion the default --max-stack allows, such a nest
  // ran out of native stack after about 2,130 levels (measured under the
  // since-removed tree-walk interpreter). The cap stays below that. A level
  // that costs more again, such as a trap wrapped in several bound functions,
  // is stopped before the cap by the native stack check
  // EnterPropertyDelegation makes once delegations nest.
  MAX_PROPERTY_DELEGATION_DEPTH = 2000;

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
// ADepth is the number of native re-entries that would be live with this one,
// and ALimit the caller's copy of NativeStackLimit for the running thread,
// NATIVE_STACK_LIMIT_UNSET until the first check that needs it looks it up.
// The first NATIVE_REENTRY_UNCHECKED_DEPTH are let through unchecked, so a
// program whose native re-entries do not nest never asks the system for the
// bounds of its stack, which on Linux means reading /proc/self/maps.
procedure CheckNativeStackHeadroom(const ADepth: Integer;
  var ALimit: NativeUInt);
// The lowest native stack address a native re-entry may start at on the
// calling thread: NATIVE_STACK_RESERVE above the bottom of its stack, or a
// quarter of the stack above it on a stack smaller than four reserves. 0 when
// the bounds of the thread's stack cannot be found. Found once per thread.
function NativeStackLimit: NativeUInt;
// The address of a local in a frame called from the caller's, which is where
// the native stack currently ends.
function NativeStackPosition: NativeUInt;
// The throw the checks share. It is exported only because FPC does not inline
// a procedure into another unit when it calls one that is local to the
// implementation section, and the two checks are small enough to be inlined
// into their callers.
procedure ThrowMaxCallStackExceeded;

// Bracket one native call into a Proxy's internal method or trap.
// EnterPropertyDelegation throws before it counts, so a caller pairs it with
// LeavePropertyDelegation in a try/finally that starts after it.
procedure EnterPropertyDelegation;
procedure LeavePropertyDelegation;
// The same for a call into any other object whose class overrides the lookup,
// counted against MAX_OBJECT_DELEGATION_DEPTH.
procedure EnterObjectDelegation;
procedure LeaveObjectDelegation;
procedure CheckProxyPrototypeSteps(const AProxySteps: Integer);

implementation

uses
  NativeStackBounds,

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

{$IFDEF FPC}{$OPTIMIZATION NOAUTOINLINE}{$ENDIF}
function NativeStackPosition: NativeUInt;
var
  Marker: Byte;
begin
  Result := NativeUInt(@Marker);
end;
{$IFDEF PRODUCTION}
  {$IFDEF FPC}
    {$OPTIMIZATION AUTOINLINE}
  {$ENDIF}
{$ENDIF}

threadvar
  GNativeStackLimit: NativeUInt;
  GNativeStackLimitFound: Boolean;

function FindNativeStackLimit: NativeUInt;
var
  StackLow, StackSize, Reserve, Position: NativeUInt;
begin
  Result := 0;
  if not TryGetThreadStackBounds(StackLow, StackSize) then
    Exit;
  // Bounds that do not hold the running frame describe some other stack.
  Position := NativeStackPosition;
  if (Position <= StackLow) or (Position - StackLow > StackSize) then
    Exit;
  Reserve := NATIVE_STACK_RESERVE;
  if StackSize < 4 * Reserve then
    Reserve := StackSize div 4;
  Result := StackLow + Reserve;
end;

function NativeStackLimit: NativeUInt;
begin
  if not GNativeStackLimitFound then
  begin
    GNativeStackLimit := FindNativeStackLimit;
    GNativeStackLimitFound := True;
  end;
  Result := GNativeStackLimit;
end;

// Out of line: it runs only once native re-entries nest.
{$IFDEF FPC}{$OPTIMIZATION NOAUTOINLINE}{$ENDIF}
procedure CheckNativeStackHeadroomSlow(const ADepth: Integer;
  var ALimit: NativeUInt);
begin
  if ALimit = NATIVE_STACK_LIMIT_UNSET then
    ALimit := NativeStackLimit;
  if ALimit <> 0 then
  begin
    // Stacks grow down on every target the engine builds for, so the
    // position falls toward the limit as native re-entries nest.
    if NativeStackPosition < ALimit then
      ThrowMaxCallStackExceeded;
  end
  else if ADepth > MAX_NATIVE_REENTRY_DEPTH then
    ThrowMaxCallStackExceeded;
end;
{$IFDEF PRODUCTION}
  {$IFDEF FPC}
    {$OPTIMIZATION AUTOINLINE}
  {$ENDIF}
{$ENDIF}

procedure CheckNativeStackHeadroom(const ADepth: Integer;
  var ALimit: NativeUInt);
begin
  if ADepth > NATIVE_REENTRY_UNCHECKED_DEPTH then
    CheckNativeStackHeadroomSlow(ADepth, ALimit);
end;

// Out of line: it runs only once delegations nest. A level costs a different
// amount of native stack depending on what it forwards through (a bound
// function, a native trap, a class's implicit constructor), so the count alone
// does not keep a nest inside the stack when native re-entries already hold
// most of it. Past NATIVE_REENTRY_UNCHECKED_DEPTH nested delegations each one
// also checks the stack, like a native re-entry into the VM does. Where the
// bounds of the stack cannot be found, the count is the only bound.
{$IFDEF FPC}{$OPTIMIZATION NOAUTOINLINE}{$ENDIF}
procedure CheckDelegationNativeStack;
var
  Limit: NativeUInt;
begin
  Limit := NativeStackLimit;
  if (Limit <> 0) and (NativeStackPosition < Limit) then
    ThrowMaxCallStackExceeded;
end;
{$IFDEF PRODUCTION}
  {$IFDEF FPC}
    {$OPTIMIZATION AUTOINLINE}
  {$ENDIF}
{$ENDIF}

procedure EnterPropertyDelegation;
begin
  if GPropertyDelegationDepth >= MAX_PROPERTY_DELEGATION_DEPTH then
    ThrowMaxCallStackExceeded;
  if GPropertyDelegationDepth >= NATIVE_REENTRY_UNCHECKED_DEPTH then
    CheckDelegationNativeStack;
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
  if GObjectDelegationDepth >= NATIVE_REENTRY_UNCHECKED_DEPTH then
    CheckDelegationNativeStack;
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
