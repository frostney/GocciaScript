unit Goccia.Builtins.GlobalShadowRealm;

{$I Goccia.inc}

{ ShadowRealm — TC39 proposal-shadowrealm (stage 2.7).

  Installed only when the host opts in via --unsafe-shadowrealm, because
  ShadowRealm.prototype.evaluate performs dynamic source evaluation, which
  GocciaScript keeps out of normal runtimes by default (see VISION.md). This
  is a core language built-in (a global constructor), not a runtime extension
  or a $262 host hook, so the engine owns it and both loaders share this code.

  Each ShadowRealm instance owns a fresh child engine (its own realm with its
  own intrinsics and global object). Child engines are owned for the lifetime
  of the engine that created them and freed at engine teardown, mirroring the
  $262.createRealm ownership model — GC sweep runs object destructors without
  an active realm context, so a child engine cannot be freed from an instance
  destructor. Child realms recursively get their own ShadowRealm so nested
  realms work. }

interface

uses
  Goccia.Engine;

{ Installs globalThis.ShadowRealm on AEngine (idempotent). }
procedure EnableShadowRealm(const AEngine: TGocciaEngine);

implementation

uses
  Classes,
  Generics.Collections,
  Math,

  Goccia.Arguments.Collection,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.Evaluator,
  Goccia.Evaluator.Context,
  Goccia.ExecutionContext,
  Goccia.Executor,
  Goccia.Executor.Bytecode,
  Goccia.Executor.Interpreter,
  Goccia.GarbageCollector,
  Goccia.Modules,
  Goccia.Modules.Loader,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.SourcePipeline,
  Goccia.Utils,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.HoleValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.PromiseValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToPrimitive,
  Goccia.VM.Exception;

type
  { Owns one child engine (a fresh realm) created by `new ShadowRealm()`. }
  TGocciaShadowRealmChildRealm = class
  private
    FEngine: TGocciaEngine;
    FSource: TStringList;
    FExecutor: TGocciaExecutor;
  public
    constructor Create(const AParentEngine: TGocciaEngine);
    destructor Destroy; override;
    property Engine: TGocciaEngine read FEngine;
  end;

  { The ShadowRealm instance object; holds [[ShadowRealm]] as a child realm. }
  TGocciaShadowRealmValue = class(TGocciaObjectValue)
  private
    FChildRealm: TGocciaShadowRealmChildRealm;
  public
    property ChildRealm: TGocciaShadowRealmChildRealm read FChildRealm
      write FChildRealm;
  end;

  { Per-engine host: builds the constructor + prototype, owns child realms and
    wrapped-function hosts, installs globalThis.ShadowRealm. Registered as an
    engine extension so it (and the child engines it owns) is freed at engine
    teardown, with each child realm context activated before teardown. }
  TGocciaShadowRealmHost = class(TGocciaEngineExtension)
  private
    FEngine: TGocciaEngine;
    FPrototype: TGocciaObjectValue;
    FConstructor: TGocciaNativeFunctionValue;
    FChildRealms: TObjectList<TGocciaShadowRealmChildRealm>;
    FWrappedHosts: TObjectList<TObject>;
    function ConstructShadowRealm(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function Evaluate(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ImportValue(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AEngine: TGocciaEngine);
    destructor Destroy; override;
    procedure Install;
    { GetWrappedValue (TC39 ShadowRealm §3.1.5): wrap a value crossing INTO
      this host's realm from ASourceEngine's realm. }
    function WrapIncoming(const AValue: TGocciaValue;
      const ASourceEngine: TGocciaEngine): TGocciaValue;
  end;

  { One wrapped function exotic object's behavior (TC39 ShadowRealm §2.1 / §2.3). The
    wrapped function is a native function in FOwnerEngine's realm; calling it
    marshals arguments into FTargetEngine's realm, calls FTarget there, and
    marshals the result back. }
  TGocciaWrappedFunctionHost = class
  private
    FTarget: TGocciaValue;
    FTargetEngine: TGocciaEngine;
    FOwnerEngine: TGocciaEngine;
  public
    constructor Create(const ATarget: TGocciaValue;
      const ATargetEngine, AOwnerEngine: TGocciaEngine);
    function Invoke(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

{ CopyNameAndLength (TC39 ShadowRealm §3.1.2): clamp a target's "length" to a
  non-negative integer length for the wrapped function. }
function ClampWrappedLength(const AValue: Double): Integer;
begin
  if IsNan(AValue) or (AValue <= 0) then
    Result := 0
  else if AValue >= MaxInt then
    Result := MaxInt
  else
    Result := Trunc(AValue);
end;

function GetShadowRealmHost(
  const AEngine: TGocciaEngine): TGocciaShadowRealmHost;
begin
  Result := TGocciaShadowRealmHost(
    AEngine.FindExtension(TGocciaShadowRealmHost));
end;

procedure EnableShadowRealm(const AEngine: TGocciaEngine);
var
  Host: TGocciaShadowRealmHost;
begin
  if Assigned(GetShadowRealmHost(AEngine)) then
    Exit;
  Host := TGocciaShadowRealmHost.Create(AEngine);
  AEngine.AddExtension(Host);
  Host.Install;
end;

{ TGocciaShadowRealmChildRealm }

constructor TGocciaShadowRealmChildRealm.Create(
  const AParentEngine: TGocciaEngine);
begin
  inherited Create;
  FSource := TStringList.Create;
  // Match the parent's execution mode so bytecode-mode hosts get bytecode
  // child realms (and interpreter hosts get interpreter child realms).
  // evaluate() runs source through the eval evaluator regardless of mode, but
  // the engine still needs an executor of the host's mode for bootstrapping.
  if AParentEngine.Executor is TGocciaBytecodeExecutor then
    FExecutor := TGocciaBytecodeExecutor.Create
  else
    FExecutor := TGocciaInterpreterExecutor.Create;
  FEngine := TGocciaEngine.Create('<shadow-realm>', FSource, FExecutor);
  // Inherit the host's language surface so the child realm understands the
  // same syntax the host enabled (e.g. --compat-function), while staying a
  // fresh realm with its own intrinsics and global object.
  FEngine.Compatibility := AParentEngine.Compatibility;
  FEngine.Preprocessors := AParentEngine.Preprocessors;
  FEngine.RefreshGlobalThis;
  EnableShadowRealm(FEngine);
  // Let ShadowRealm.prototype.importValue resolve and read modules the same way
  // the realm that created this one does. The imported module still evaluates
  // in this child realm against its own intrinsics; only host file access is
  // shared. Engine.Compatibility already propagates to the loader; preprocessors
  // do not, so mirror them here too.
  FEngine.ModuleLoader.SetContentProvider(AParentEngine.ContentProvider, False);
  FEngine.ModuleLoader.Preprocessors := AParentEngine.Preprocessors;
  FEngine.SuspendRealmExecutionContext;
end;

destructor TGocciaShadowRealmChildRealm.Destroy;
var
  RealmScope: TGocciaExecutionContextScope;
begin
  if Assigned(FEngine) then
  begin
    // Freeing an engine unpins the realm's intrinsics; its realm context must
    // be active so teardown sees a valid current realm (see TBareTest262Realm).
    RealmScope := FEngine.ActivateRealmExecutionContext;
    try
      FEngine.Free;
    finally
      RealmScope.Free;
    end;
  end;
  FExecutor.Free;
  FSource.Free;
  inherited;
end;

{ TGocciaShadowRealmHost }

constructor TGocciaShadowRealmHost.Create(const AEngine: TGocciaEngine);
begin
  inherited Create;
  FEngine := AEngine;
  FChildRealms := TObjectList<TGocciaShadowRealmChildRealm>.Create(True);
  FWrappedHosts := TObjectList<TObject>.Create(True);
end;

destructor TGocciaShadowRealmHost.Destroy;
begin
  FChildRealms.Free;
  FWrappedHosts.Free;
  inherited;
end;

procedure TGocciaShadowRealmHost.Install;
var
  RealmScope: TGocciaExecutionContextScope;
  Members: TGocciaMemberCollection;
  ObjectProto: TGocciaObjectValue;
begin
  if Assigned(FConstructor) then
    Exit;

  // Activate this engine's realm so the prototype/constructor are built from
  // this realm's intrinsics (Object.prototype, Function.prototype).
  RealmScope := FEngine.ActivateRealmExecutionContext;
  try
    if not Assigned(TGocciaObjectValue.SharedObjectPrototype) then
      TGocciaObjectValue.InitializeSharedPrototype;
    ObjectProto := TGocciaObjectValue.SharedObjectPrototype;

    // ShadowRealm.prototype (TC39 ShadowRealm §3.3); [[Prototype]] is Object.prototype.
    FPrototype := TGocciaObjectValue.Create(ObjectProto);
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FPrototype);

    Members := TGocciaMemberCollection.Create;
    try
      // ShadowRealm.prototype.evaluate (TC39 ShadowRealm §3.4.1): length 1.
      Members.AddNamedMethod('evaluate', Evaluate, 1, gmkPrototypeMethod,
        [gmfNoFunctionPrototype]);
      // ShadowRealm.prototype.importValue (TC39 ShadowRealm §3.4.2): length 2.
      Members.AddNamedMethod('importValue', ImportValue, 2, gmkPrototypeMethod,
        [gmfNoFunctionPrototype]);
      // ShadowRealm.prototype[@@toStringTag] (TC39 ShadowRealm §3.4.3):
      // { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: true }.
      Members.AddSymbolDataProperty(TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create(CONSTRUCTOR_SHADOW_REALM),
        [pfConfigurable]);
      RegisterMemberDefinitions(FPrototype, Members.ToDefinitions);
    finally
      Members.Free;
    end;

    // ShadowRealm constructor (TC39 ShadowRealm §3.2): length 0, name "ShadowRealm".
    FConstructor := TGocciaNativeFunctionValue.Create(ConstructShadowRealm,
      CONSTRUCTOR_SHADOW_REALM, 0);
    // ShadowRealm.prototype: { [[W]]: false, [[E]]: false, [[C]]: false }.
    FConstructor.DefineProperty(PROP_PROTOTYPE,
      TGocciaPropertyDescriptorData.Create(FPrototype, []));
    // ShadowRealm.prototype.constructor: { [[W]]: true, [[E]]: false, [[C]]: true }.
    FPrototype.DefineProperty(PROP_CONSTRUCTOR,
      TGocciaPropertyDescriptorData.Create(FConstructor,
        [pfWritable, pfConfigurable]));

    // globalThis.ShadowRealm: standard built-in property attributes
    // { [[W]]: true, [[E]]: false, [[C]]: true } via the exported binding.
    FEngine.Interpreter.GlobalScope.DefineLexicalBinding(
      CONSTRUCTOR_SHADOW_REALM, FConstructor, dtConst, True);
    FEngine.RefreshGlobalThis;
  finally
    RealmScope.Free;
  end;
end;

// TC39 ShadowRealm §3.2.1 ShadowRealm ( )
function TGocciaShadowRealmHost.ConstructShadowRealm(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Child: TGocciaShadowRealmChildRealm;
  Instance: TGocciaShadowRealmValue;
begin
  // Step 1: a native constructor receives a hole `this` when invoked with
  // `new`; a non-hole `this` means a [[Call]] without new — throw a TypeError.
  if not (AThisValue is TGocciaHoleValue) then
    ThrowTypeError('Constructor ShadowRealm requires ''new''');

  // Steps 2-9: a fresh realm with its own global object and intrinsics.
  Child := TGocciaShadowRealmChildRealm.Create(FEngine);
  FChildRealms.Add(Child);

  Instance := TGocciaShadowRealmValue.Create(FPrototype);
  Instance.ChildRealm := Child;
  Result := Instance;
end;

// TC39 ShadowRealm §3.4.1 ShadowRealm.prototype.evaluate ( sourceText )
// (delegates to PerformShadowRealmEval §3.1.3)
function TGocciaShadowRealmHost.Evaluate(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Instance: TGocciaShadowRealmValue;
  ChildEngine: TGocciaEngine;
  SourceValue: TGocciaValue;
  SourceText: string;
  SourceList: TStringList;
  Options: TGocciaSourcePipelineOptions;
  OptionsScope: TGocciaSourcePipelineOptionsScope;
  ParseResult: TGocciaSourcePipelineResult;
  OwnerScope, ChildScope: TGocciaExecutionContextScope;
  EvalScope, VarScope: TGocciaScope;
  EvalContext: TGocciaEvaluationContext;
  StrictEval: Boolean;
  RawResult: TGocciaValue;
  Threw: Boolean;
  I: Integer;
begin
  // PerformShadowRealmEval runs with the caller realm current. That caller
  // realm is this evaluate method's realm (FEngine) — not the ambient realm of
  // whoever invoked evaluate — so the SyntaxError/TypeError it throws and the
  // wrapped value it returns all derive from FEngine's intrinsics.
  OwnerScope := FEngine.ActivateRealmExecutionContext;
  try
    // 3.4.1 step 2: ValidateShadowRealmObject(O).
    if not (AThisValue is TGocciaShadowRealmValue) then
      ThrowTypeError(
        'ShadowRealm.prototype.evaluate called on incompatible receiver');
    Instance := TGocciaShadowRealmValue(AThisValue);

    // 3.4.1 step 3: if sourceText is not a String, throw a TypeError.
    if AArgs.Length = 0 then
      SourceValue := TGocciaUndefinedLiteralValue.UndefinedValue
    else
      SourceValue := AArgs.GetElement(0);
    if not (SourceValue is TGocciaStringLiteralValue) then
      ThrowTypeError('ShadowRealm.prototype.evaluate expects a string');
    SourceText := TGocciaStringLiteralValue(SourceValue).Value;

    ChildEngine := Instance.ChildRealm.Engine;
    RawResult := TGocciaUndefinedLiteralValue.UndefinedValue;
    Threw := False;

    SourceList := TStringList.Create;
    try
      SourceList.Text := SourceText;
      Options := TGocciaSourcePipeline.DefaultOptions;
      Options.SourceType := stScript;
      Options.Preprocessors := ChildEngine.Preprocessors;
      Options.Compatibility := ChildEngine.Compatibility;

      OptionsScope := TGocciaSourcePipeline.ActivateOptions(Options);
      try
        // 3.1.3 step 2: a parse failure throws a SyntaxError in the caller
        // realm. FEngine is the active realm here, so re-raising through
        // ThrowSyntaxError tags the error with FEngine's intrinsics rather
        // than the ambient realm the raw parser error would surface in.
        try
          ParseResult := TGocciaSourcePipeline.Parse(SourceList,
            '<shadow-realm-eval>', Options);
        except
          on E: TGocciaError do
            ThrowSyntaxError(E.Message);
        end;
        try
          // §3.1.3 step 2: top-level using / await using declarations are not
          // allowed in the eval body — reject as a caller-realm SyntaxError.
          for I := 0 to ParseResult.ProgramNode.Body.Count - 1 do
            if ParseResult.ProgramNode.Body[I] is TGocciaUsingDeclaration then
              ThrowSyntaxError('Using declarations are not allowed at the ' +
                'top level of ShadowRealm.prototype.evaluate');
          // §3.1.3 steps 9-11: declaration instantiation + evaluation run in
          // the eval realm; an abrupt completion becomes a caller-realm
          // TypeError. Strictness comes only from the source's own directive,
          // never the outer realm (ScriptIsStrict of script).
          ChildScope := ChildEngine.ActivateRealmExecutionContext;
          try
            try
              StrictEval := HasUseStrictDirective(ParseResult.ProgramNode);
              if StrictEval then
                EvalScope := ChildEngine.Interpreter.GlobalScope.CreateChild(
                  skFunction, 'ShadowRealmEval')
              else
                EvalScope := ChildEngine.Interpreter.GlobalScope.CreateChild(
                  skBlock, 'ShadowRealmEval');
              EvalScope.ThisValue := ChildEngine.Interpreter.GlobalScope.ThisValue;
              if Assigned(TGarbageCollector.Instance) then
                TGarbageCollector.Instance.AddTempRoot(EvalScope);
              try
                EvalContext := ChildEngine.Interpreter.CreateEvaluationContext;
                EvalContext.Scope := EvalScope;
                EvalContext.CurrentFilePath := '<shadow-realm-eval>';
                EvalContext.NonStrictMode := not StrictEval;
                if StrictEval then
                  VarScope := EvalScope
                else
                  VarScope := ChildEngine.Interpreter.GlobalScope;
                // EvalDeclarationInstantiation: var/function bind in the realm's
                // global var environment (so they persist across evaluate calls
                // and surface on globalThis); let/const bind in this fresh
                // per-call lexical scope, so re-evaluating top-level lexical
                // declarations does not clash.
                RawResult := EvaluateEvalProgram(ParseResult.ProgramNode,
                  EvalContext, VarScope, EvalScope, StrictEval, False, nil,
                  False, False, False);
              finally
                if Assigned(TGarbageCollector.Instance) then
                  TGarbageCollector.Instance.RemoveTempRoot(EvalScope);
              end;
            except
              on E: TGocciaThrowValue do Threw := True;
              on E: EGocciaBytecodeThrow do Threw := True;
              on E: TGocciaError do Threw := True;
            end;
          finally
            ChildScope.Free;
          end;
        finally
          ParseResult.Free;
        end;
      finally
        OptionsScope.Free;
      end;
    finally
      SourceList.Free;
    end;

    // 3.1.3 step 15: wrap evaluation errors into a caller-realm TypeError.
    if Threw then
      ThrowTypeError('ShadowRealm evaluate threw');

    // 3.1.3 step 16: GetWrappedValue(callerRealm, result).
    Result := WrapIncoming(RawResult, ChildEngine);
  finally
    OwnerScope.Free;
  end;
end;

// TC39 ShadowRealm §3.4.2 ShadowRealm.prototype.importValue ( specifier, exportName )
// (delegates to RealmImportValue, sec-realmimportvalue)
function TGocciaShadowRealmHost.ImportValue(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Instance: TGocciaShadowRealmValue;
  SpecifierArg, ExportNameArg, SpecifierPrimitive: TGocciaValue;
  Specifier, ExportName: string;
  ChildEngine: TGocciaEngine;
  ChildScope, OwnerScope: TGocciaExecutionContextScope;
  Module: TGocciaModule;
  ExportValue, WrappedValue, Settled: TGocciaValue;
  Promise: TGocciaPromiseValue;
  Threw, ExportFound, WrapThrew, HaveWrapped, SettleReject: Boolean;
  GC: TGarbageCollector;
begin
  // As in evaluate(), importValue runs with the caller realm current. That
  // caller realm is this method's realm (FEngine) — not the ambient realm of
  // whoever invoked importValue — so the synchronous TypeErrors below, the
  // returned promise, and any rejection reason all derive from FEngine's
  // intrinsics.
  GC := TGarbageCollector.Instance;
  OwnerScope := FEngine.ActivateRealmExecutionContext;
  try
    // §3.4.2 step 2: ValidateShadowRealmObject(O).
    if not (AThisValue is TGocciaShadowRealmValue) then
      ThrowTypeError(
        'ShadowRealm.prototype.importValue called on incompatible receiver');
    Instance := TGocciaShadowRealmValue(AThisValue);

    // §3.4.2 step 3: specifierString = ? ToString(specifier). A throwing
    // coercion (valueOf/toString/@@toPrimitive, or a Symbol) propagates
    // synchronously in the caller realm — it is not turned into a rejection.
    if AArgs.Length = 0 then
      SpecifierArg := TGocciaUndefinedLiteralValue.UndefinedValue
    else
      SpecifierArg := AArgs.GetElement(0);
    SpecifierPrimitive := ToPrimitive(SpecifierArg, tphString);
    if SpecifierPrimitive is TGocciaSymbolValue then
      ThrowTypeError('Cannot convert a Symbol value to a string');
    Specifier := SpecifierPrimitive.ToStringLiteral.Value;

    // §3.4.2 step 4: if exportName is not a String, throw a TypeError. exportName
    // is type-checked, never coerced — ToString is not called on it.
    if AArgs.Length < 2 then
      ExportNameArg := TGocciaUndefinedLiteralValue.UndefinedValue
    else
      ExportNameArg := AArgs.GetElement(1);
    if not (ExportNameArg is TGocciaStringLiteralValue) then
      ThrowTypeError(
        'ShadowRealm.prototype.importValue expects a string export name');
    ExportName := TGocciaStringLiteralValue(ExportNameArg).Value;

    ChildEngine := Instance.ChildRealm.Engine;
    Module := nil;
    ExportValue := nil;
    WrappedValue := nil;
    Threw := False;
    ExportFound := False;
    WrapThrew := False;
    HaveWrapped := False;

    // RealmImportValue: import the module into the child realm and read the
    // requested export, all while the child realm is current so the module
    // evaluates against the child realm's intrinsics. GocciaScript module
    // loading is synchronous, so the dynamic import is performed inline and the
    // caller-realm promise is settled before returning; the .then reactions the
    // caller attaches still run as microtasks.
    ChildScope := ChildEngine.ActivateRealmExecutionContext;
    try
      try
        Module := ChildEngine.ModuleLoader.LoadModule(Specifier,
          FEngine.SourcePath);
        if Assigned(Module) then
          ExportFound := Module.TryGetExportValue(ExportName, ExportValue);
      except
        // A failed import — resolution, parse, link, or an evaluation throw —
        // rejects with a caller-realm TypeError (RealmImportValue's rejection
        // handler is callerRealm.[[Intrinsics]].[[%ThrowTypeError%]]).
        on E: TGocciaThrowValue do Threw := True;
        on E: EGocciaBytecodeThrow do Threw := True;
        on E: TGocciaError do Threw := True;
      end;

      // ExportGetter final step: value = GetWrappedValue(callerRealm, binding).
      // A non-callable object export makes GetWrappedValue throw, which the
      // promise turns into a rejection.
      if (not Threw) and ExportFound then
      begin
        try
          WrappedValue := WrapIncoming(ExportValue, ChildEngine);
          HaveWrapped := True;
        except
          on E: TGocciaThrowValue do WrapThrew := True;
          on E: EGocciaBytecodeThrow do WrapThrew := True;
          on E: TGocciaError do WrapThrew := True;
        end;
      end;
    finally
      ChildScope.Free;
    end;

    // Settle in the caller realm (OwnerScope is active again): import failure, a
    // missing export, or a non-wrappable export all reject with a caller-realm
    // TypeError; success resolves with the wrapped value.
    if HaveWrapped then
    begin
      SettleReject := False;
      Settled := WrappedValue;
    end
    else
    begin
      SettleReject := True;
      if Threw then
        Settled := Goccia.Values.ErrorHelper.CreateErrorObject(TYPE_ERROR_NAME,
          'ShadowRealm could not import "' + Specifier + '"')
      else if WrapThrew then
        Settled := Goccia.Values.ErrorHelper.CreateErrorObject(TYPE_ERROR_NAME,
          'ShadowRealm cannot pass the imported value across the realm boundary')
      else
        Settled := Goccia.Values.ErrorHelper.CreateErrorObject(TYPE_ERROR_NAME,
          'ShadowRealm module "' + Specifier + '" has no export named "' +
          ExportName + '"');
    end;

    // Root the settle value across the promise allocation so a GC triggered by
    // creating the promise cannot collect it.
    if Assigned(GC) then
      GC.AddTempRoot(Settled);
    try
      Promise := TGocciaPromiseValue.Create;
      if SettleReject then
        Promise.Reject(Settled)
      else
        Promise.Resolve(Settled);
    finally
      if Assigned(GC) then
        GC.RemoveTempRoot(Settled);
    end;

    Result := Promise;
  finally
    OwnerScope.Free;
  end;
end;

// TC39 ShadowRealm §3.1.5 GetWrappedValue ( callerRealm, value )
function TGocciaShadowRealmHost.WrapIncoming(const AValue: TGocciaValue;
  const ASourceEngine: TGocciaEngine): TGocciaValue;
var
  TargetLength: Integer;
  TargetName: string;
  LengthValue, NameValue: TGocciaValue;
  WrappedHost: TGocciaWrappedFunctionHost;
  Wrapped: TGocciaNativeFunctionValue;
  DestScope: TGocciaExecutionContextScope;
  ValueRoot: TGocciaTempRoot;
begin
  // Step 2: primitives cross the boundary unchanged.
  if AValue.IsPrimitive then
    Exit(AValue);

  // Step 1.a: a non-callable object cannot cross — throw a TypeError.
  if not AValue.IsCallable then
    ThrowTypeError(
      'ShadowRealm cannot pass a non-callable object across the realm boundary');

  // Reading "length"/"name" can run a proxy get-trap (a GC safe point), and
  // the wrapper is allocated before CapturedRoot keeps the target alive, so
  // root the target across both — mirroring InvokeCallable's input rooting.
  InitializeTempRoot(ValueRoot);
  try
    AddTempRootIfNeeded(ValueRoot, AValue);

    // Step 1.b: WrappedFunctionCreate (TC39 ShadowRealm §3.1.1).
    // CopyNameAndLength (3.1.2) reads the target's "length"/"name"; if a read
    // throws (e.g. a revoked proxy target), WrappedFunctionCreate throws a
    // TypeError (step 8).
    TargetLength := 0;
    TargetName := '';
    try
      if (AValue is TGocciaObjectValue) and
         TGocciaObjectValue(AValue).HasOwnProperty(PROP_LENGTH) then
      begin
        LengthValue := AValue.GetProperty(PROP_LENGTH);
        if LengthValue is TGocciaNumberLiteralValue then
          TargetLength := ClampWrappedLength(
            TGocciaNumberLiteralValue(LengthValue).Value);
      end;
      NameValue := AValue.GetProperty(PROP_NAME);
      if NameValue is TGocciaStringLiteralValue then
        TargetName := TGocciaStringLiteralValue(NameValue).Value;
    except
      on E: TGocciaThrowValue do
        ThrowTypeError(
          'ShadowRealm cannot copy wrapped function name and length');
      on E: EGocciaBytecodeThrow do
        ThrowTypeError(
          'ShadowRealm cannot copy wrapped function name and length');
      on E: TGocciaError do
        ThrowTypeError(
          'ShadowRealm cannot copy wrapped function name and length');
    end;

    WrappedHost := TGocciaWrappedFunctionHost.Create(AValue, ASourceEngine,
      FEngine);
    FWrappedHosts.Add(WrappedHost);
    // Create the wrapper in FEngine's realm so its [[Prototype]] is FEngine's
    // %Function.prototype% (the realm the value crosses INTO), independent of
    // whichever realm is currently running.
    DestScope := FEngine.ActivateRealmExecutionContext;
    try
      Wrapped := TGocciaNativeFunctionValue.CreateWithoutPrototype(
        WrappedHost.Invoke, TargetName, TargetLength);
    finally
      DestScope.Free;
    end;
    // Keep the wrapped target reachable for the GC through the wrapper.
    Wrapped.CapturedRoot := AValue;
    Result := Wrapped;
  finally
    RemoveTempRootIfNeeded(ValueRoot);
  end;
end;

{ TGocciaWrappedFunctionHost }

constructor TGocciaWrappedFunctionHost.Create(const ATarget: TGocciaValue;
  const ATargetEngine, AOwnerEngine: TGocciaEngine);
begin
  inherited Create;
  FTarget := ATarget;
  FTargetEngine := ATargetEngine;
  FOwnerEngine := AOwnerEngine;
end;

// TC39 ShadowRealm §2.1 Wrapped Function Exotic Object [[Call]] ( thisArgument, argumentsList )
// (via OrdinaryWrappedFunctionCall §2.3)
function TGocciaWrappedFunctionHost.Invoke(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  TargetHost, OwnerHost: TGocciaShadowRealmHost;
  OwnerScope, TargetScope: TGocciaExecutionContextScope;
  WrappedArgs: TGocciaArgumentsCollection;
  WrappedThis, WrappedArg, CallResult: TGocciaValue;
  ThisRoot: TGocciaTempRoot;
  ArgRoots: array of TGocciaTempRoot;
  I: Integer;
  Threw: Boolean;
begin
  TargetHost := GetShadowRealmHost(FTargetEngine);
  OwnerHost := GetShadowRealmHost(FOwnerEngine);
  CallResult := TGocciaUndefinedLiteralValue.UndefinedValue;
  Threw := False;

  // The running realm during the wrapped call is the wrapper's own realm
  // (FOwnerEngine). Argument and return wrapping therefore report non-wrappable
  // values as TypeErrors in this realm, even though each wrapped argument
  // derives its prototype from the target realm it is passed into.
  OwnerScope := FOwnerEngine.ActivateRealmExecutionContext;
  try
    // Each WrapIncoming below allocates a wrapper and may run a proxy get-trap
    // (a GC safe point), so keep the already-wrapped receiver and arguments
    // rooted until InvokeCallable takes over their rooting.
    InitializeTempRoot(ThisRoot);
    SetLength(ArgRoots, AArgs.Length);
    for I := 0 to High(ArgRoots) do
      InitializeTempRoot(ArgRoots[I]);
    WrappedArgs := TGocciaArgumentsCollection.Create;
    try
      // 2.3 steps 5-7: wrap the receiver and each argument INTO the target
      // realm. A failure here (e.g. a non-callable object argument) propagates
      // as-is, exactly like the spec's `?` on GetWrappedValue.
      WrappedThis := TargetHost.WrapIncoming(AThisValue, FOwnerEngine);
      AddTempRootIfNeeded(ThisRoot, WrappedThis);
      for I := 0 to AArgs.Length - 1 do
      begin
        WrappedArg := TargetHost.WrapIncoming(AArgs.GetElement(I), FOwnerEngine);
        AddTempRootIfNeeded(ArgRoots[I], WrappedArg);
        WrappedArgs.Add(WrappedArg);
      end;
      // 2.3 step 8: call the target function in its own realm.
      TargetScope := FTargetEngine.ActivateRealmExecutionContext;
      try
        try
          CallResult := InvokeCallable(FTarget, WrappedArgs, WrappedThis);
        except
          on E: TGocciaThrowValue do Threw := True;
          on E: EGocciaBytecodeThrow do Threw := True;
          on E: TGocciaError do Threw := True;
        end;
      finally
        TargetScope.Free;
      end;
    finally
      WrappedArgs.Free;
      for I := High(ArgRoots) downto 0 do
        RemoveTempRootIfNeeded(ArgRoots[I]);
      RemoveTempRootIfNeeded(ThisRoot);
    end;

    // 2.3 step 10: a thrown target becomes a TypeError in the caller realm.
    if Threw then
      ThrowTypeError('ShadowRealm wrapped function threw');

    // 2.3 step 9: wrap the result back INTO the owner (caller) realm.
    Result := OwnerHost.WrapIncoming(CallResult, FTargetEngine);
  finally
    OwnerScope.Free;
  end;
end;

end.
