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

  TGocciaExecutionContextStack = class
  public
    class procedure Push(const AContext: TGocciaExecutionContext); static;
    class function Pop: TGocciaExecutionContext; static;
    class function Running: TGocciaExecutionContext; static;
    class function HasRunning: Boolean; static;
    class function CurrentRealm: TGocciaRealm; static;
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

  TGocciaExecutionContextStackEntry = record
    Context: TGocciaExecutionContext;
    PreviousRealm: TGocciaRealm;
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
  GExecutionContextStack: array of TGocciaExecutionContextStackEntry;
  GExecutionContextStackCount: Integer;
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
begin
  if not Assigned(AContext.Realm) then
    raise Exception.Create('Execution context requires a realm.');

  if GExecutionContextStackCount >= Length(GExecutionContextStack) then
    SetLength(GExecutionContextStack, GExecutionContextStackCount * 2 + 8);

  GExecutionContextStack[GExecutionContextStackCount].Context := AContext;
  GExecutionContextStack[GExecutionContextStackCount].PreviousRealm :=
    Goccia.Realm.CurrentRealm;
  Inc(GExecutionContextStackCount);

  SetCurrentRealm(AContext.Realm);
end;

class function TGocciaExecutionContextStack.Pop: TGocciaExecutionContext;
var
  PreviousRealm: TGocciaRealm;
begin
  if GExecutionContextStackCount <= 0 then
    raise Exception.Create('Execution context stack underflow.');

  Dec(GExecutionContextStackCount);
  Result := GExecutionContextStack[GExecutionContextStackCount].Context;
  PreviousRealm := GExecutionContextStack[GExecutionContextStackCount].PreviousRealm;
  GExecutionContextStack[GExecutionContextStackCount] :=
    Default(TGocciaExecutionContextStackEntry);
  SetCurrentRealm(PreviousRealm);
end;

class function TGocciaExecutionContextStack.Running: TGocciaExecutionContext;
begin
  if GExecutionContextStackCount > 0 then
    Result := GExecutionContextStack[GExecutionContextStackCount - 1].Context
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
  Result := GExecutionContextStackCount > 0;
end;

class function TGocciaExecutionContextStack.CurrentRealm: TGocciaRealm;
begin
  if GExecutionContextStackCount > 0 then
    Result := GExecutionContextStack[GExecutionContextStackCount - 1].Context.Realm
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
  SetLength(GExecutionContextStack, 0);
  GExecutionContextStackCount := 0;

end.
