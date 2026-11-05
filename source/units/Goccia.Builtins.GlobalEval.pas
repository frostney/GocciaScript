unit Goccia.Builtins.GlobalEval;

{$I Goccia.inc}

{ eval ( x ) — ES2026 §19.2.1.

  Installed only when the host opts in through EnableEval, because eval
  performs dynamic source evaluation, which GocciaScript keeps out of the
  default runtime (see VISION.md). Like ShadowRealm, eval is a global that is
  absent until a host installs it, so the option is an installer in the
  EnableShadowRealm style rather than an Enabled toggle on an always-present
  intrinsic like the Function constructor.

  EnableEval records the function as the realm's %eval%, which is what makes
  a call written `eval(...)` a direct eval (ES2026 §13.3.6.1 step 6.a); the VM
  evaluates a direct eval at its call site and never calls this function. A
  call through any other reference is an indirect eval (§19.2.1.1), which
  runs here through TGocciaEngine.EvaluateGlobalEval. Every call of %eval%,
  direct or indirect, reports an `eval.call` capability audit event. }

interface

uses
  Goccia.Engine;

{ Installs globalThis.eval on AEngine as its realm's %eval% (idempotent).
  Off by default: an engine that never calls this has no eval. }
procedure EnableEval(const AEngine: TGocciaEngine);

implementation

uses
  Goccia.Arguments.Collection,
  Goccia.CapabilityAudit,
  Goccia.ExecutionContext,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

const
  EVAL_NAME = 'eval';
  EVAL_SOURCE_PATH = '<eval>';

type
  { Per-engine host of the realm's %eval%. Registered as an engine extension
    so it lives exactly as long as the engine whose realm it evaluates in. }
  TGocciaEvalHost = class(TGocciaEngineExtension)
  private
    FEngine: TGocciaEngine;
    FFunction: TGocciaNativeFunctionValue;
    function Invoke(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AEngine: TGocciaEngine);
    procedure Install;
  end;

constructor TGocciaEvalHost.Create(const AEngine: TGocciaEngine);
begin
  inherited Create;
  FEngine := AEngine;
end;

procedure TGocciaEvalHost.Install;
var
  RealmScope: TGocciaExecutionContextScope;
begin
  if Assigned(FFunction) then
    Exit;
  // Build the function from this engine's realm, so its [[Prototype]] is this
  // realm's %Function.prototype%.
  RealmScope := FEngine.ActivateRealmExecutionContext;
  try
    // §19.2.1: length 1, name "eval"; a built-in function that is not a
    // constructor has no "prototype" property (§10.3).
    FFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(Invoke,
      EVAL_NAME, 1);
    // The realm slot keeps the function alive, so the direct-eval identity
    // check still holds after a script replaces or deletes the global.
    InstallRealmEvalFunction(FEngine.Realm, FFunction);
    // §19: { [[Writable]]: true, [[Enumerable]]: false,
    // [[Configurable]]: true }.
    (FEngine.Realm.GlobalObject as TGocciaObjectValue).DefineProperty(
      EVAL_NAME, TGocciaPropertyDescriptorData.Create(FFunction,
        [pfWritable, pfConfigurable]));
  finally
    RealmScope.Free;
  end;
end;

// ES2026 §19.2.1.1 eval ( x ), called indirectly: PerformEval(x, false,
// false) in this host's realm. A direct eval never reaches this function; the
// VM evaluates it at the call site.
function TGocciaEvalHost.Invoke(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  SourceValue: TGocciaValue;
  Stage: TGocciaGlobalEvalStage;
begin
  FEngine.EmitCapabilityAudit(gckEval, gcdAllow, EVAL_NAME,
    'indirect eval is enabled');
  // Step 2: if x is not a String, return x unchanged.
  if AArgs.Length = 0 then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  SourceValue := AArgs.GetElement(0);
  if not (SourceValue is TGocciaStringLiteralValue) then
    Exit(SourceValue);
  Result := FEngine.EvaluateGlobalEval(
    TGocciaStringLiteralValue(SourceValue).Value, EVAL_SOURCE_PATH, Stage);
end;

procedure EnableEval(const AEngine: TGocciaEngine);
var
  Host: TGocciaEvalHost;
begin
  if Assigned(AEngine.FindExtension(TGocciaEvalHost)) then
    Exit;
  Host := TGocciaEvalHost.Create(AEngine);
  AEngine.AddExtension(Host);
  Host.Install;
end;

end.
