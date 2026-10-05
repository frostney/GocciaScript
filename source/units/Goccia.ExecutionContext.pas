unit Goccia.ExecutionContext;

{$I Goccia.inc}

interface

uses
  Goccia.Diagnostics.SourceRegistry,
  Goccia.Realm,
  Goccia.Scope,
  Goccia.Values.Primitives;

type
  // ECMA-262 execution context slice used by runtime execution paths.
  // Spec fields that are not modelled yet stay nil/empty rather than being
  // inferred from process-global state.
  TGocciaExecutionContext = record
    Realm: TGocciaRealm;
    Scope: TGocciaScope;
    FunctionValue: TGocciaValue;
    ScriptOrModule: TObject;
  private
    FSourcePathRef: Pointer;
    function GetSourcePath: string; {$IFDEF FPC}inline;{$ENDIF}
  public
    property SourcePath: string read GetSourcePath;
  end;

  { The two records below are the context stack's own storage. They are
    declared here only so that the call-path push and pop can be inlined into
    the bytecode VM; nothing outside this unit should read or write them. }
  TGocciaExecutionContextStackEntry = record
    Context: TGocciaExecutionContext;
    PreviousRealm: TGocciaRealm;
  end;
  PGocciaExecutionContextStackEntry = ^TGocciaExecutionContextStackEntry;

  // The stack and its depth live in one thread variable. Every reference to a
  // thread variable is a thread-local lookup, and Push and Pop run on each VM
  // call, so they resolve this record once and work through the pointer.
  TGocciaExecutionContextThreadState = record
    Entries: array of TGocciaExecutionContextStackEntry;
    Count: Integer;
    // Length(Entries), kept beside Count so that the call path's capacity
    // check is two loads.
    Capacity: Integer;
    // Goccia.Realm's current-realm variable for this thread, resolved by
    // ThreadState so the call-path push and pop switch realms through it.
    RealmSlot: PGocciaRealm;
  end;
  PGocciaExecutionContextThreadState = ^TGocciaExecutionContextThreadState;

  TGocciaExecutionContextStack = class
  private
    class procedure RaiseRealmRequired; static;
    class procedure RaiseUnderflow; static;
  public
    class procedure Push(const AContext: TGocciaExecutionContext); static;
    class function Pop: TGocciaExecutionContext; static;
    class function Running: TGocciaExecutionContext; static;
    class function HasRunning: Boolean; static;
    class function CurrentRealm: TGocciaRealm; static;

    { Call-path entry points for the bytecode VM, which pushes and pops one
      context per function call.

      ThreadState returns a handle to the calling thread's context stack. It
      stays valid for as long as the thread lives and must never be handed to
      another thread. PushFunctionContext and PopFunctionContext do exactly
      what Push and Pop do for a context built by CreateExecutionContext with
      no ScriptOrModule, but through that handle: neither performs a
      thread-local lookup, copies a context record, or looks a source path up.
      ASourcePathRef comes from InternSourcePath on the same thread.

      PushFunctionContext does not grow the stack: when FunctionContextsFull,
      the caller makes room first through SetFunctionContextCapacity. The VM grows it that way so that the growth
      is charged to --max-memory with its own stacks (ADR 0130,
      Amendment 1). }
    class function ThreadState: Pointer; static;
    class procedure PushFunctionContext(const AThreadState: Pointer;
      const ARealm: TGocciaRealm; const AScope: TGocciaScope;
      const AFunctionValue: TGocciaValue; const ASourcePathRef: Pointer);
      static; {$IFDEF FPC}inline;{$ENDIF}
    class function FunctionContextsFull(const AThreadState: Pointer): Boolean;
      static; {$IFDEF FPC}inline;{$ENDIF}
    class function FunctionContextCount(const AThreadState: Pointer): Integer;
      static; {$IFDEF FPC}inline;{$ENDIF}
    class function FunctionContextCapacity(
      const AThreadState: Pointer): Integer; static; {$IFDEF FPC}inline;{$ENDIF}
    // Resizes the stack to ACapacity entries, at least its count.
    class procedure SetFunctionContextCapacity(const AThreadState: Pointer;
      const ACapacity: Integer); static;
    class procedure PopFunctionContext(const AThreadState: Pointer);
      static; {$IFDEF FPC}inline;{$ENDIF}
  end;

  TGocciaExecutionContextScope = class
  private
    FPopped: Boolean;
    FHasDiagnostic: Boolean;
    FPrevDiagnosticScope: TGocciaDiagnosticSourceScope;
  public
    { When ADiagnosticScope is assigned, this scope also makes it the active
      diagnostic capture target for its lifetime and restores the previous one
      on Pop — so a cross-engine transition (ShadowRealm evaluate/importValue/
      wrapped function) captures code frames from the engine actually running,
      not from whichever engine happened to be active before the switch. }
    constructor Create(const AContext: TGocciaExecutionContext;
      const ADiagnosticScope: TGocciaDiagnosticSourceScope = nil);
    destructor Destroy; override;
    procedure Pop;
  end;

function CreateExecutionContext(const ARealm: TGocciaRealm;
  const AScope: TGocciaScope; const ASourcePath: string;
  const AScriptOrModule: TObject = nil;
  const AFunctionValue: TGocciaValue = nil): TGocciaExecutionContext;

// The stable, thread-owned reference an execution context stores in place of
// its source path (nil for the empty path). Equal paths give the same
// reference, which stays valid for the life of the process, so a caller that
// pushes many contexts for one path can look it up once.
function InternSourcePath(const ASourcePath: string): Pointer;

function RunningExecutionContext: TGocciaExecutionContext; {$IFDEF FPC}inline;{$ENDIF}
function HasRunningExecutionContext: Boolean; {$IFDEF FPC}inline;{$ENDIF}

implementation

uses
  SysUtils;

type
  PGocciaInternedSourcePath = ^TGocciaInternedSourcePath;
  TGocciaInternedSourcePath = record
    Next: PGocciaInternedSourcePath;
    Value: UnicodeString;
  end;

threadvar
  // Non-owning context stack.  Scope and FunctionValue are GC-managed objects
  // held as raw pointers, and no root source marks this array; that is
  // deliberate, and safe because every push site keeps both members reachable
  // through a root the collector already walks for at least as long as the
  // entry lives:
  //
  //   * TGocciaVM.SetupNewFrame — Scope is FGlobalScope and FunctionValue is
  //     AClosure.FunctionValue.  The entry is pushed with the VM frame and
  //     popped in TeardownCurrentFrame (FCurrentExecutionContextPushed rides
  //     on the frame record), so for its whole life the closure is either
  //     FCurrentClosure, an FFrameStack entry, or an FTempSavedStateRoots
  //     entry — and TGocciaVMStackRoot.MarkClosureReferences marks the
  //     closure's FunctionValue from all three.  Native re-entry displaces a
  //     frame into FTempSavedStateRoots rather than dropping it, so a
  //     displaced frame's entry stays covered too.
  //   * TGocciaVM.ExecuteModule / .ExecuteFunction and every
  //     TGocciaExecutionContextScope in the engine and the tree-walking
  //     interpreter — FunctionValue is nil, and Scope is a scope the collector
  //     roots outright rather than one it reaches through a frame.  The VM
  //     entries, TGocciaEngine and TGocciaInterpreter.Execute carry the engine
  //     global scope (an explicit AddRootObject); the two module paths
  //     (EvaluateModuleProgram and
  //     TGocciaInterpreterAsyncModuleEvaluation.Resume) carry the *module*
  //     scope, which TGocciaModule.SetEnvironment likewise registers with
  //     AddRootObject for as long as the module holds it.  The async
  //     evaluation additionally marks FContext.Scope itself, so the entry
  //     stays covered without depending on the module's registration or on
  //     the continuation's own bookkeeping.
  //   * TGocciaVM direct eval — Scope is the eval activation scope, temp-
  //     rooted around the whole eval, and FunctionValue is the caller
  //     closure's function value, covered as above.
  //
  // Verified empirically: an instrumented sweep probe that reports whenever a
  // swept object is still named by an entry stayed silent across both engine
  // modes of the full suite (2.9M sweeps per mode, stacks up to 66 deep), and
  // across an adversarial file that forces collections from getters, native
  // callbacks, Proxy traps, coercion hooks, error unwinding, direct eval,
  // generators and async resumptions with the callee dropped by its caller.
  //
  // A push site that cannot point at such a root makes this array the last
  // reference to a collectible object; it would then need a real
  // TGCRootSource (see TGocciaAsyncContextRoots for the shape).
  GExecutionContextState: TGocciaExecutionContextThreadState;
  GInternedSourcePaths: PGocciaInternedSourcePath;

function InternSourcePath(const ASourcePath: string): Pointer;
var
  Node: PGocciaInternedSourcePath;
begin
  { Pointer intern so TGocciaExecutionContext stays unmanaged: Push/Pop must
    not FPC_COPY a UnicodeString on every VM call. }
  if ASourcePath = '' then
    Exit(nil);
  Node := GInternedSourcePaths;
  while Node <> nil do
  begin
    if Node.Value = ASourcePath then
      Exit(@Node.Value);
    Node := Node.Next;
  end;
  New(Node);
  Node.Value := ASourcePath;
  Node.Next := GInternedSourcePaths;
  GInternedSourcePaths := Node;
  Result := @Node.Value;
end;

function TGocciaExecutionContext.GetSourcePath: string;
begin
  if FSourcePathRef = nil then
    Result := ''
  else
    Result := PUnicodeString(FSourcePathRef)^;
end;

function CreateExecutionContext(const ARealm: TGocciaRealm;
  const AScope: TGocciaScope; const ASourcePath: string;
  const AScriptOrModule: TObject;
  const AFunctionValue: TGocciaValue): TGocciaExecutionContext;
begin
  Result.Realm := ARealm;
  Result.Scope := AScope;
  Result.FunctionValue := AFunctionValue;
  Result.ScriptOrModule := AScriptOrModule;
  Result.FSourcePathRef := InternSourcePath(ASourcePath);
end;

function RunningExecutionContext: TGocciaExecutionContext;
begin
  Result := TGocciaExecutionContextStack.Running;
end;

function HasRunningExecutionContext: Boolean;
begin
  Result := TGocciaExecutionContextStack.HasRunning;
end;

{ TGocciaExecutionContextStack }

class procedure TGocciaExecutionContextStack.Push(
  const AContext: TGocciaExecutionContext);
var
  State: PGocciaExecutionContextThreadState;
  Entry: PGocciaExecutionContextStackEntry;
begin
  if not Assigned(AContext.Realm) then
    raise Exception.Create('Execution context requires a realm.');

  State := @GExecutionContextState;
  if State^.Count >= State^.Capacity then
  begin
    SetLength(State^.Entries, State^.Count * 2 + 8);
    State^.Capacity := Length(State^.Entries);
  end;

  Entry := @State^.Entries[State^.Count];
  Entry^.Context := AContext;
  Entry^.PreviousRealm := ExchangeCurrentRealm(AContext.Realm);
  Inc(State^.Count);
end;

class function TGocciaExecutionContextStack.Pop: TGocciaExecutionContext;
var
  State: PGocciaExecutionContextThreadState;
  Entry: PGocciaExecutionContextStackEntry;
  PreviousRealm: TGocciaRealm;
begin
  State := @GExecutionContextState;
  if State^.Count <= 0 then
    raise Exception.Create('Execution context stack underflow.');

  Dec(State^.Count);
  Entry := @State^.Entries[State^.Count];
  Result := Entry^.Context;
  PreviousRealm := Entry^.PreviousRealm;
  Entry^ := Default(TGocciaExecutionContextStackEntry);
  SetCurrentRealm(PreviousRealm);
end;

{ The failure branches of the inlined call-path push and pop. They are kept
  out of line so that inlining those two into the VM's frame setup and
  teardown brings no exception construction with it. Automatic inlining is
  switched off for them and back on after them by name: FPC 3.2.2 does not
  save optimizer switches on $PUSH, so a $POP would leave it off for the rest
  of the unit. }
{$IFDEF FPC}{$OPTIMIZATION NOAUTOINLINE}{$ENDIF}
class procedure TGocciaExecutionContextStack.RaiseRealmRequired;
begin
  raise Exception.Create('Execution context requires a realm.');
end;

class procedure TGocciaExecutionContextStack.RaiseUnderflow;
begin
  raise Exception.Create('Execution context stack underflow.');
end;
{$IFDEF PRODUCTION}{$IFDEF FPC}{$OPTIMIZATION AUTOINLINE}{$ENDIF}{$ENDIF}

class function TGocciaExecutionContextStack.ThreadState: Pointer;
var
  State: PGocciaExecutionContextThreadState;
begin
  State := @GExecutionContextState;
  if State^.RealmSlot = nil then
    State^.RealmSlot := CurrentRealmSlot;
  Result := State;
end;

class procedure TGocciaExecutionContextStack.PushFunctionContext(
  const AThreadState: Pointer; const ARealm: TGocciaRealm;
  const AScope: TGocciaScope; const AFunctionValue: TGocciaValue;
  const ASourcePathRef: Pointer);
var
  State: PGocciaExecutionContextThreadState;
  Entry: PGocciaExecutionContextStackEntry;
begin
  if not Assigned(ARealm) then
    RaiseRealmRequired;

  State := PGocciaExecutionContextThreadState(AThreadState);
  Assert(State^.Count < State^.Capacity,
    'PushFunctionContext without room on the context stack');

  Entry := @State^.Entries[State^.Count];
  Entry^.Context.Realm := ARealm;
  Entry^.Context.Scope := AScope;
  Entry^.Context.FunctionValue := AFunctionValue;
  Entry^.Context.ScriptOrModule := nil;
  Entry^.Context.FSourcePathRef := ASourcePathRef;
  Entry^.PreviousRealm := State^.RealmSlot^;
  State^.RealmSlot^ := ARealm;
  Inc(State^.Count);
end;

class procedure TGocciaExecutionContextStack.PopFunctionContext(
  const AThreadState: Pointer);
var
  State: PGocciaExecutionContextThreadState;
  Entry: PGocciaExecutionContextStackEntry;
  PreviousRealm: TGocciaRealm;
begin
  State := PGocciaExecutionContextThreadState(AThreadState);
  if State^.Count <= 0 then
    RaiseUnderflow;

  Dec(State^.Count);
  Entry := @State^.Entries[State^.Count];
  PreviousRealm := Entry^.PreviousRealm;
  // Leave the vacated entry cleared, as Pop does, field by field: assigning
  // Default() to it is a FillChar call.
  Entry^.Context.Realm := nil;
  Entry^.Context.Scope := nil;
  Entry^.Context.FunctionValue := nil;
  Entry^.Context.ScriptOrModule := nil;
  Entry^.Context.FSourcePathRef := nil;
  Entry^.PreviousRealm := nil;
  State^.RealmSlot^ := PreviousRealm;
end;

class function TGocciaExecutionContextStack.FunctionContextsFull(
  const AThreadState: Pointer): Boolean;
var
  State: PGocciaExecutionContextThreadState;
begin
  State := PGocciaExecutionContextThreadState(AThreadState);
  Result := State^.Count >= State^.Capacity;
end;

class function TGocciaExecutionContextStack.FunctionContextCount(
  const AThreadState: Pointer): Integer;
begin
  Result := PGocciaExecutionContextThreadState(AThreadState)^.Count;
end;

class function TGocciaExecutionContextStack.FunctionContextCapacity(
  const AThreadState: Pointer): Integer;
begin
  Result := PGocciaExecutionContextThreadState(AThreadState)^.Capacity;
end;

class procedure TGocciaExecutionContextStack.SetFunctionContextCapacity(
  const AThreadState: Pointer; const ACapacity: Integer);
var
  State: PGocciaExecutionContextThreadState;
begin
  State := PGocciaExecutionContextThreadState(AThreadState);
  Assert(ACapacity >= State^.Count,
    'Context stack capacity below its count');
  SetLength(State^.Entries, ACapacity);
  State^.Capacity := ACapacity;
end;

class function TGocciaExecutionContextStack.Running: TGocciaExecutionContext;
var
  State: PGocciaExecutionContextThreadState;
begin
  State := @GExecutionContextState;
  if State^.Count > 0 then
    Result := State^.Entries[State^.Count - 1].Context
  else
    Result := Default(TGocciaExecutionContext);

  // The tree-walking evaluator reports its running function through the
  // Goccia.Realm facade (see the rooting note on GCurrentFunctionContextStack
  // there); the bytecode VM writes its function value straight into the entry
  // it pushes, so an empty facade stack leaves the entry's own value in place.
  if Goccia.Realm.HasCurrentFunctionExecutionContext then
  begin
    Result.Scope := TGocciaScope(
      Goccia.Realm.CurrentFunctionExecutionContextScope);
    Result.FunctionValue := TGocciaValue(
      Goccia.Realm.CurrentFunctionExecutionContextValue);
  end;
end;

class function TGocciaExecutionContextStack.HasRunning: Boolean;
begin
  Result := GExecutionContextState.Count > 0;
end;

class function TGocciaExecutionContextStack.CurrentRealm: TGocciaRealm;
var
  State: PGocciaExecutionContextThreadState;
begin
  State := @GExecutionContextState;
  if State^.Count > 0 then
    Result := State^.Entries[State^.Count - 1].Context.Realm
  else
    Result := Goccia.Realm.CurrentRealm;
end;

{ TGocciaExecutionContextScope }

constructor TGocciaExecutionContextScope.Create(
  const AContext: TGocciaExecutionContext;
  const ADiagnosticScope: TGocciaDiagnosticSourceScope);
begin
  inherited Create;
  FPopped := False;
  TGocciaExecutionContextStack.Push(AContext);
  FHasDiagnostic := Assigned(ADiagnosticScope);
  if FHasDiagnostic then
    FPrevDiagnosticScope :=
      TGocciaDiagnosticSourceRegistry.Activate(ADiagnosticScope);
end;

destructor TGocciaExecutionContextScope.Destroy;
begin
  Pop;
  inherited;
end;

procedure TGocciaExecutionContextScope.Pop;
begin
  if FPopped then
    Exit;
  // Restore the diagnostic scope before unwinding the context, mirroring Create.
  if FHasDiagnostic then
  begin
    TGocciaDiagnosticSourceRegistry.Deactivate(FPrevDiagnosticScope);
    FHasDiagnostic := False;
  end;
  TGocciaExecutionContextStack.Pop;
  FPopped := True;
end;

initialization

finalization
  SetLength(GExecutionContextState.Entries, 0);
  GExecutionContextState.Capacity := 0;
  GExecutionContextState.Count := 0;

end.
