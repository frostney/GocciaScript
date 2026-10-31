unit Goccia.Engine;

{$I Goccia.inc}

interface

uses
  Classes,
  Contnrs,
  Generics.Collections,
  SysUtils,

  OrderedStringMap,
  UnicodeStringList,

  Goccia.Arguments.Collection,
  Goccia.AST.BindingPatterns,
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Builtins.Atomics,
  Goccia.Builtins.Base,
  Goccia.Builtins.DisposableStack,
  Goccia.Builtins.GlobalArray,
  Goccia.Builtins.GlobalArrayBuffer,
  Goccia.Builtins.GlobalBigInt,
  Goccia.Builtins.GlobalMap,
  Goccia.Builtins.GlobalNumber,
  Goccia.Builtins.GlobalObject,
  Goccia.Builtins.GlobalPromise,
  Goccia.Builtins.GlobalProxy,
  Goccia.Builtins.GlobalReflect,
  Goccia.Builtins.GlobalRegExp,
  Goccia.Builtins.Globals,
  Goccia.Builtins.GlobalString,
  Goccia.Builtins.GlobalSymbol,
  Goccia.Builtins.Intl,
  Goccia.Builtins.JSON,
  Goccia.Builtins.Math,
  Goccia.Builtins.Temporal,
  Goccia.Bytecode.Chunk,
  Goccia.Bytecode.Module,
  Goccia.Capabilities,
  Goccia.CapabilityAudit,
  Goccia.Constants,
  Goccia.Diagnostics.SourceRegistry,
  Goccia.EvaluationContext,
  Goccia.ExecutionContext,
  Goccia.FetchTransport,
  Goccia.HostEnvironment,
  Goccia.JSON,
  Goccia.Keywords.Reserved,
  Goccia.Modules,
  Goccia.Modules.ContentProvider,
  Goccia.Modules.Loader,
  Goccia.Modules.Resolver,
  Goccia.Modules.Virtual,
  Goccia.ObjectModel,
  Goccia.ObjectModel.Engine,
  Goccia.Packages.Address,
  Goccia.Profiler.Compile,
  Goccia.Realm,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
  Goccia.Shims,
  Goccia.SourceMap,
  Goccia.SourcePipeline,
  Goccia.Values.BigIntValue,
  Goccia.Values.ClassValue,
  Goccia.Values.FunctionBase,
  Goccia.Values.HoleValue,
  Goccia.Values.IteratorValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.TypedArrayValue,
  Goccia.VM;

type
  TGocciaPreprocessor = Goccia.SourcePipeline.TGocciaPreprocessor;
  TGocciaPreprocessors = Goccia.SourcePipeline.TGocciaPreprocessors;

  TGocciaCompatibility = Goccia.SourcePipeline.TGocciaCompatibility;
  TGocciaCompatibilityFlags = Goccia.SourcePipeline.TGocciaCompatibilityFlags;

  TGocciaSourceType = Goccia.SourcePipeline.TGocciaSourceType;

const
  ppJSX = Goccia.SourcePipeline.ppJSX;

  cfASI = Goccia.SourcePipeline.cfASI;
  cfVar = Goccia.SourcePipeline.cfVar;
  cfFunction = Goccia.SourcePipeline.cfFunction;
  cfTraditionalFor = Goccia.SourcePipeline.cfTraditionalFor;
  cfWhileLoops = Goccia.SourcePipeline.cfWhileLoops;
  cfLooseEquality = Goccia.SourcePipeline.cfLooseEquality;
  cfNonStrictMode = Goccia.SourcePipeline.cfNonStrictMode;
  cfArgumentsObject = Goccia.SourcePipeline.cfArgumentsObject;
  cfLabel = Goccia.SourcePipeline.cfLabel;
  cfForIn = Goccia.SourcePipeline.cfForIn;
  cfExperimentalJSModuleSource =
    Goccia.SourcePipeline.cfExperimentalJSModuleSource;

  stScript = Goccia.SourcePipeline.stScript;
  stModule = Goccia.SourcePipeline.stModule;
  stDetect = Goccia.SourcePipeline.stDetect;

type

  TGocciaScriptResult = record
    Result: TGocciaValue;
    LexTimeNanoseconds: Int64;
    ParseTimeNanoseconds: Int64;
    CompileTimeNanoseconds: Int64;
    ExecuteTimeNanoseconds: Int64;
    TotalTimeNanoseconds: Int64;
    FileName: string;
  end;

  TGocciaEngineExtension = class
  public
    { Runs this extension's pending host work — a fetch in flight, a due
      timer — until it has none left, settling the promises it owns. }
    procedure WaitForIdle; virtual;
    { True while this extension still has host work WaitForIdle would wait
      for. Guest code that runs after WaitForIdle returns, in a later
      extension's pass or in a microtask, can start more. }
    function HasPendingWork: Boolean; virtual;
    { Runs this extension's pending work while the engine drives one promise
      to settlement outside its idle wait — an async test's returned promise,
      `.resolves`, an async hook — and returns True if it had any. The
      default runs WaitForIdle when HasPendingWork. An extension whose work
      that wait already drives in its own order returns False. }
    function RunPendingWorkForPromiseWait: Boolean; virtual;
    { Why a heap snapshot of the run would lose state this extension holds
      outside the heap, or '' when it would not (ADR 0148). Separate from
      HasPendingWork, which says whether the idle wait has work: a timer the
      idle wait does not run is still state a snapshot would lose. }
    function HeapSnapshotRefusal: string; virtual;
    { Materialize only the named native builtin, without guest execution or
      host I/O. Unknown names must leave the extension unchanged. }
    procedure MaterializeHeapSnapshotBuiltin(const AName: string); virtual;
    procedure DiscardPending; virtual;
    function InjectGlobalsFromJSON5(
      const AJSON5String: string): Boolean; virtual;
    function InjectGlobalsFromTOML(
      const ATOMLString: string): Boolean; virtual;
    function InjectGlobalsFromYAML(const AYamlString: string): Boolean; virtual;
    function InjectModulesFromJSON5(const AJSON5String: string;
      const ABaseAddress: string): Boolean; virtual;
    function InjectModulesFromTOML(const ATOMLString: string;
      const ABaseAddress: string): Boolean; virtual;
    function InjectModulesFromYAML(const AYAMLString: string;
      const ABaseAddress: string): Boolean; virtual;
  end;

  TGocciaEngineExtensionClass = class of TGocciaEngineExtension;
  TGocciaEngineExtensionList = TObjectList<TGocciaEngineExtension>;

  { What a run does about a promise that is still rejected with no handler
    once the run has nothing left to do (ES2026 §27.2.1.9
    HostPromiseRejectionTracker). }
  TGocciaUnhandledRejectionMode = (
    { Raise the rejection reason from the run, like an uncaught throw. }
    urThrow,
    { Leave it tracked on the microtask queue for the host to take with
      TakeUnhandledRejection; whatever is left goes when the queue is
      cleared. }
    urIgnore);

  { Called for a promise a run left rejected with no handler; see
    TGocciaEngine.OnUnhandledRejection. AReason is APromise's rejection
    reason. }
  TGocciaUnhandledRejectionEvent = procedure(const APromise: TGocciaValue;
    const AReason: TGocciaValue) of object;

  { How far a global eval got before it raised (see
    TGocciaEngine.EvaluateGlobalEval): gesCompile covers parsing, the static
    early errors and compilation, gesRun the declaration instantiation and the
    body. TC39 ShadowRealm §3.1.3 PerformShadowRealmEval throws a SyntaxError
    for the first and a TypeError for the second. }
  TGocciaGlobalEvalStage = (gesCompile, gesRun);

  TGocciaEngine = class;

  { Ends a run that paused at a pause checkpoint (ADR 0148): raised out of
    Execute once the OnPauseCheckpoint handler asked to pause. No script can
    catch it: no frame is running at a checkpoint. }
  EGocciaRunPaused = class(Exception);

  { Called at a pause checkpoint (ADR 0148). The handler may take a heap
    snapshot; setting APause ends the run with EGocciaRunPaused, and leaving
    it False goes on waiting for the host calls as usual. }
  TGocciaPauseCheckpointEvent = procedure(const AEngine: TGocciaEngine;
    var APause: Boolean) of object;

  TGocciaEngine = class
  public
    const DefaultPreprocessors: TGocciaPreprocessors = [ppJSX];
    const DefaultCompatibility: TGocciaCompatibilityFlags = [];
    const DefaultWarningUnsupportedFeatures = False;
    { ADR 0140: an entry whose file name does not name its goal is a Module
      when it has a top-level import or export declaration. }
    const DefaultSourceType: TGocciaSourceType = stDetect;
  private
    { The realm's global environment. The engine owns it. }
    FGlobalScope: TGocciaGlobalScope;
    FSourcePath: string;
    FModuleLoader: TGocciaModuleLoader;
    FOwnsModuleLoader: Boolean;
    FInjectedGlobals: TStringList;
    FPreprocessors: TGocciaPreprocessors;
    FCompatibility: TGocciaCompatibilityFlags;
    FLabelStatementsEnabled: Boolean;
    FForInLoopsEnabled: Boolean;
    FExperimentalJSModuleSourceEnabled: Boolean;
    FWarningUnsupportedFeatures: Boolean;
    FSourceType: TGocciaSourceType;
    FStrictTypes: Boolean;
    FShims: TStringList;
    { The bytecode VM every program, module body, shim and eval of this
      engine runs on. The engine owns it. }
    FVM: TGocciaVM;
    FGlobalBackedTopLevel: Boolean;
    { The modules the program being run linked (LinkModuleRequests), and the
      file that requested them; nil outside a run. }
    FLinkedModules: TOrderedStringMap<TGocciaModule>;
    FLinkedReferrer: string;
    { A module-source entry's linked requests as its OP_IMPORTs evaluate
      them, and how many are still to evaluate (AwaitEntryRequests); nil
      outside such an entry's imports. }
    FEntryRequests: TGocciaModuleList;
    FEntryRequestsLeft: Integer;
    FCompileTimeNanoseconds: Int64;
    FExecuteTimeNanoseconds: Int64;
    FShimLoader: TGocciaShimLoader;
    { ADR 0148: the extension that owns the run's host calls, and the
      handler called when the run waits only on those. }
    FHostCallOwner: TGocciaEngineExtension;
    FOnPauseCheckpoint: TGocciaPauseCheckpointEvent;
    FSourceLines: TStringList;
    FExtensions: TGocciaEngineExtensionList;
    // Qualified: Generics.Collections' TObjectList<T> shadows the
    // classic Contnrs form for unqualified references under lakon's
    // stricter later-unit-wins rule; the qualification is a no-op
    // for FPC.
    FRetainedModules: Contnrs.TObjectList;
    FLazyThunks: Contnrs.TObjectList;
    FHostEnvironment: TGocciaHostEnvironment;
    FCapabilityAuditSink: TGocciaCapabilityAuditSink;
    { Fixed at construction and never replaced: the engine's authority over
      the host (ADR 0122). }
    FCapabilities: TGocciaCapabilities;
    FProjectRoot: string;
    FFetchMaxResponseBytes: Integer;
    FFetchTransport: IGocciaFetchTransport;
    FIsCapabilityChild: Boolean;
    FEffectiveCapabilitiesAudited: Boolean;
    FCapabilityProvenance: string;

    // Core language built-in objects
    FBuiltinMath: TGocciaMath;
    FBuiltinGlobalObject: TGocciaGlobalObject;
    FBuiltinGlobalArray: TGocciaGlobalArray;
    FBuiltinGlobalNumber: TGocciaGlobalNumber;
    FBuiltinGlobalBigInt: TGocciaGlobalBigInt;
    FBuiltinRegExp: TGocciaGlobalRegExp;
    FBuiltinGlobalString: TGocciaGlobalString;
    FBuiltinGlobals: TGocciaGlobals;
    FBuiltinJSON: TGocciaJSONBuiltin;
    FBuiltinSymbol: TGocciaGlobalSymbol;
    FBuiltinMap: TGocciaGlobalMap;
    FBuiltinPromise: TGocciaGlobalPromise;
    FBuiltinTemporal: TGocciaTemporalBuiltin;
    FBuiltinIntl: TGocciaIntlBuiltin;
    FBuiltinAtomics: TGocciaAtomics;
    FBuiltinArrayBuffer: TGocciaGlobalArrayBuffer;
    FBuiltinProxy: TGocciaGlobalProxy;
    FBuiltinReflect: TGocciaGlobalReflect;
    FBuiltinDisposableStack: TGocciaBuiltinDisposableStack;
    FGocciaGlobal: TGocciaObjectValue;
    FRealm: TGocciaRealm;
    FRealmExecutionContext: TGocciaExecutionContextScope;
    FObjectConstructor: TGocciaClassValue;
    FFunctionConstructor: TGocciaFunctionConstructorClassValue;
    FTypedArrayIntrinsic: TGocciaClassValue;
    FSuppressWarnings: Boolean;
    FUnhandledRejections: TGocciaUnhandledRejectionMode;
    FOnUnhandledRejection: TGocciaUnhandledRejectionEvent;
    FReportingUnhandledRejections: Boolean;
    { The async-context bracket this engine holds for its whole lifetime; see
      EnterEngineAsyncContext. }
    FAsyncContextToken: Integer;
    FLastTiming: TGocciaScriptResult;
    FLastSourceMap: TGocciaSourceMap;
    procedure NotifyUnhandledRejections;
    procedure SetStrictTypes(const AValue: Boolean);
    function GetContentProvider: TGocciaModuleContentProvider;
    function GetModuleResolver: TGocciaModuleResolver;
    procedure SetPreprocessors(const AValue: TGocciaPreprocessors);
    procedure SetCompatibility(AValue: TGocciaCompatibilityFlags);
    procedure SetLabelStatementsEnabled(const AValue: Boolean);
    procedure SetForInLoopsEnabled(const AValue: Boolean);
    procedure SetExperimentalJSModuleSourceEnabled(const AValue: Boolean);
    procedure SetWarningUnsupportedFeatures(const AValue: Boolean);

    procedure PinSingletons;
    procedure RegisterBuiltIns;
    procedure RegisterBuiltinConstructors;
    procedure DefineBuiltinBindingPlaceholder(const AName: string;
      const ADeclarationType: TGocciaDeclarationType);
    procedure DefineLazyGlobalThisProperty(const AName: string;
      const AFactory: TGocciaLazyPropertyFactory);
    procedure RegisterLazyBuiltinGlobalProperties;
    procedure ExecuteShims;
    { The context a module program evaluates in at the top of this realm:
      the global environment, the engine's module callbacks and language
      profile. }
    function CreateEvaluationContext: TGocciaEvaluationContext;
    procedure RegisterTypedArrayConstructor(const AName: string; const AKind: TGocciaTypedArrayKind; const AObjectConstructor: TGocciaClassValue);
    procedure RegisterGlobalThis;
    procedure RegisterGocciaScriptGlobal;
    procedure Initialize(const AFileName: string; const ASourceLines: TStringList;
      const AModuleLoader: TGocciaModuleLoader;
      const AOwnsModuleLoader: Boolean;
      const ACapabilities: TGocciaCapabilities);
    procedure SetProjectRoot(const AValue: string);
    procedure ApplyCapabilityPolicy;
    procedure GrantProviderPackage(const ASpecifier: string;
      const AAddress: TGocciaProviderAddress);
    procedure AuditProviderPackage(const AAllowed: Boolean;
      const ASubject, AReason: string);
    function GrantNodeModules(const ASpecifier, AImportingDirectory: string;
      out ACeiling: string): Boolean;
    function GetResolver: TGocciaModuleResolver;
    function SpeciesGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GocciaGC(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GocciaGCMaxBytesGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GocciaGCSuggestedMaxBytesGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GocciaGCBytesAllocatedGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MaterializeTemporalGlobal: TGocciaValue;
    function MaterializeIntlGlobal: TGocciaValue;
    function MaterializeAtomicsGlobal: TGocciaValue;
    function MaterializeProxyGlobal: TGocciaValue;
    function MaterializeReflectGlobal: TGocciaValue;
    function MaterializeDisposableStackGlobal: TGocciaValue;
    function MaterializeAsyncDisposableStackGlobal: TGocciaValue;
    procedure DiscardRuntimePending;
    { Compile AProgram with this engine's language profile. ANonStrictMode
      is false for module source, which is always strict. }
    function CompileProgram(const AProgram: TGocciaProgram;
      const ANonStrictMode: Boolean): TGocciaBytecodeModule;
    { Run AModule on the VM against the VM's current global scope, after
      linking its static requests; AThisValue is its top-level `this`, or nil
      for the scope's. }
    function RunBytecodeModule(const AModule: TGocciaBytecodeModule;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure LinkModuleRequests(const AModule: TGocciaBytecodeModule);
    procedure AwaitEntryRequests;
    procedure InitializeEntryLocalExports(const AModule: TGocciaBytecodeModule;
      const AEntry: TGocciaModule);
    procedure InitializeEntryLinkedExports(const AModule: TGocciaBytecodeModule;
      const AEntry: TGocciaModule);
    function LoadLinkedModule(const AModulePath,
      AImportingFilePath: string): TGocciaModule;
    { The module loader's callbacks: ES2026 §16.2.1.7.3.1
      InitializeEnvironment for a module's function declarations, and the
      evaluation of its body in AContext, returning the completion value
      (last expression value, or undefined). }
    procedure InstantiateModuleFunctions(const AProgram: TGocciaProgram;
      var AContext: TGocciaEvaluationContext);
    function CompileModuleProgram(const AProgram: TGocciaProgram;
      const AFilePath: string; const ANonStrictMode,
      AArgumentsObjectEnabled: Boolean): TGocciaBytecodeModule;
    function EvaluateModuleBody(const AProgram: TGocciaProgram;
      const AContext: TGocciaEvaluationContext;
      out AProgramConsumed: Boolean): TGocciaValue;
    procedure InstantiateCompiledModuleFunctions(
      const AModule: TGocciaBytecodeModule; const AScope: TGocciaScope;
      const ARealm: TGocciaRealm);
    { The shim loader's evaluator (TGocciaShimEvaluator). }
    function EvaluateShim(const AProgram: TGocciaProgram;
      const AScope: TGocciaScope; const AFileName: string): Boolean;
    { The VM's compiler for direct eval at a strict call site. }
    function CompileDirectEvalCode(const AProgram: TGocciaProgram;
      const ASourcePath: string; const ASite: TGocciaDirectEvalEnvironment;
      const AStrictEval: Boolean;
      const ADeclaredVarNames: TUnicodeStringList): TGocciaFunctionTemplate;
    function ExecuteDynamicFunction(
      const AProgram: TGocciaProgram): TGocciaValue;
    procedure PrintSourcePipelineWarnings(
      const APipelineResult: TGocciaSourcePipelineResult);
    { The parse options of this engine's language profile. }
    procedure ValidateImportedBytecode(const AModule: TGocciaBytecodeModule);
    function DecodeModuleBytecode(const ABytes: TBytes): TGocciaBytecodeModule;
    procedure ReleaseModuleBytecode(const AModule: TGocciaBytecodeModule);
    function RunRegisteredBytecodeEntry(const AAddress: string): TGocciaValue;
    function SourcePipelineOptions(
      const ASourceType: TGocciaSourceType): TGocciaSourcePipelineOptions;
    function ExtensionsHavePendingWork: Boolean;
    function RunExtensionWorkForPromiseWait: Boolean;
    function CompileDynamicFunction(const AParamsSources: array of string;
      const ABodySource: string;
      const AKind: TGocciaDynamicFunctionKind): TGocciaFunctionBase;
    function CompileDynamicFunctionCode(const AParamStr, ABodySource: string;
      const AKind: TGocciaDynamicFunctionKind;
      const ACompileProfile: TGocciaCompileProfiler;
      const AProfileSource: string): TGocciaFunctionBase;
    procedure InjectModulesFromValue(const AValue: TGocciaValue;
      const ABaseAddress, AProvenance: string);
  public
    { The engine compiles everything it runs to bytecode and runs it on a VM
      of its own. }
    constructor Create(const AFileName: string;
      const ASourceLines: TStringList); overload;
    constructor Create(const AFileName: string;
      const ASourceLines: TStringList;
      const AResolver: TGocciaModuleResolver); overload;
    constructor Create(const AFileName: string;
      const ASourceLines: TStringList;
      const AModuleLoader: TGocciaModuleLoader); overload;
    { The capability set is fixed here for the engine's lifetime. The overloads
      without one create an engine with TGocciaCapabilities.None: it reaches
      nothing outside the process beyond the module-graph exemption. }
    constructor Create(const AFileName: string;
      const ASourceLines: TStringList;
      const ACapabilities: TGocciaCapabilities); overload;
    constructor Create(const AFileName: string;
      const ASourceLines: TStringList;
      const AResolver: TGocciaModuleResolver;
      const ACapabilities: TGocciaCapabilities); overload;
    constructor Create(const AFileName: string;
      const ASourceLines: TStringList;
      const AModuleLoader: TGocciaModuleLoader;
      const ACapabilities: TGocciaCapabilities); overload;
    destructor Destroy; override;

    function Execute: TGocciaScriptResult;
    function ExecuteProgram(const AProgram: TGocciaProgram): TGocciaValue;
    procedure WaitForRuntimeIdle;
    { ADR 0148: true while the run is at a pause checkpoint. No bytecode
      frame runs (the caller is the idle wait), the microtask queue is empty,
      no Atomics.waitAsync waiter of this engine is pending, no extension but
      HostCallOwner has pending work, none holds state outside the heap
      (HeapSnapshotRefusal), and HostCallOwner has pending work: the run
      waits only on host calls the embedder answers. }
    function IsAtPauseCheckpoint: Boolean;
    { ADR 0148: why the run is not idle enough for a heap snapshot, or ''
      when it is: a bytecode frame running, a job queued, an Atomics.waitAsync
      waiter of this engine pending, an extension other than HostCallOwner
      with pending work, or state outside the heap (HeapSnapshotRefusal).
      IsAtPauseCheckpoint is this plus pending host calls on the owner. }
    function SnapshotIdleRefusal: string;
    { ADR 0148: why a heap snapshot of the run would lose state held outside
      the heap, from every extension, or '' when none would. }
    function HeapSnapshotRefusal: string;
    procedure MaterializeHeapSnapshotBuiltin(const AName: string);
    procedure RaiseUnhandledRejection;
    procedure ClearPendingWorkOfFailedRun(
      const AOuterRunningEngine: TGocciaEngine);
    function CompileModule(
      const AProgram: TGocciaProgram): TGocciaBytecodeModule;
    function CompileImportableModule(const AProgram: TGocciaProgram): TGocciaBytecodeModule;
    function ValidateBytecodeModule(const AModule: TGocciaBytecodeModule): TGocciaSourceType;
    procedure RegisterBytecodeModule(const AAddress: string;
      const ABytes: TBytes; const ABaseAddress: string = '');
    function RunBytecodeBuffer(const ABytes: TBytes;
      const AAddress: string): TGocciaValue;
    procedure RetainModule(const AModule: TGocciaBytecodeModule);
    { How many compiled modules the engine keeps for its lifetime: the
      programs it compiled, and the code of the Function constructor and of
      eval (ADR 0134, decision 1). }
    function RetainedModuleCount: Integer;
    // The compiled modules this engine keeps, in the order it compiled them:
    // the functions a heap snapshot names run their code (ADR 0148).
    function RetainedModule(const AIndex: Integer): TGocciaBytecodeModule;
    { Compiles the source module at ASourcePath as linking it would, without
      linking or running it, and keeps it: a heap snapshot restore needs the
      code of the modules it restores (ADR 0148). The engine's own source
      when ASourcePath is its source path, else the loader reads the module
      under its read policy, and ASource (AHasSource) is what loading it
      would record. AAsCompiledEntry: the engine's own source, compiled as
      CompileModule compiles it. }
    function CompileModuleForSnapshot(const ASourcePath: string;
      const AIsHostOwned, AAsCompiledEntry: Boolean; out AHasSource: Boolean;
      out ASource: TGocciaModuleSnapshotSource): TGocciaBytecodeModule;
    function RunModule(
      const AModule: TGocciaBytecodeModule): TGocciaValue;
    function RunModuleInScope(const AModule: TGocciaBytecodeModule;
      const AScope: TGocciaScope): TGocciaValue;
    function RunModuleForSourceType(const AModule: TGocciaBytecodeModule;
      const AFileName: string): TGocciaValue;
    { Calls AFunction as a run of this engine, as ExecuteProgram runs a
      program: it waits for the work the call leaves pending and raises a
      rejection the call leaves unhandled. For script a host calls after the
      engine's own run, such as the test runner's runTests. }
    function CallAsRun(const AFunction: TGocciaFunctionBase;
      const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure ThrowError(const AMessage: string; const ALine,
      AColumn: Integer);

    procedure AddAlias(const APattern, AReplacement: string);
    procedure EmitCapabilityAudit(const AKind: TGocciaCapabilityKind;
      const ADecision: TGocciaCapabilityDecision;
      const ASubject, AReason: string);
    { As EmitCapabilityAudit, attributed to ASource instead of the current
      call site; an empty file path falls back to the engine's entry. }
    procedure EmitCapabilityAuditAt(const AKind: TGocciaCapabilityKind;
      const ADecision: TGocciaCapabilityDecision;
      const ASubject, AReason: string;
      const ASource: TGocciaCapabilityAuditSource);
    procedure ConfigureCapabilityAuditAsChildOf(
      const AParent: TGocciaEngine);
    { Emits the one capabilities.effective event for this engine: the set's
      JSON. Hosts call it right after installing a sink; the engine also calls
      it before the first other event reaches the sink. Idempotent, and a no-op
      for child contexts, which inherit their parent's set. }
    procedure AuditEffectiveCapabilities;
    procedure InjectGlobal(const AKey: string; const AValue: TGocciaValue);
    procedure RegisterGlobal(const AName: string; const AValue: TGocciaValue);
    procedure RegisterLazyGlobal(const AName: string;
      const AFactory: TGocciaLazyPropertyFactory;
      const ADeclarationType: TGocciaDeclarationType);
    procedure DefineLazyObjectProperty(const ATarget: TGocciaObjectValue;
      const AName: string; const AFactory: TGocciaLazyPlainFactory;
      const AFlags: TPropertyFlags);
    procedure InjectGlobalsFromJSON(const AJsonString: string);
    procedure InjectGlobalsFromJSON5(const AJSON5String: string);
    procedure InjectGlobalsFromTOML(const ATOMLString: string);
    procedure InjectGlobalsFromYAML(const AYamlString: string);
    procedure InjectGlobalsFromModule(const APath: string);
    procedure InjectModule(const AName, AContent: string;
      const AType: string = ''; const ABaseAddress: string = '');
    procedure InjectModulesFromJSON(const AJSONString: string;
      const ABaseAddress: string = '');
    procedure InjectModulesFromJSON5(const AJSON5String: string;
      const ABaseAddress: string = '');
    procedure InjectModulesFromTOML(const ATOMLString: string;
      const ABaseAddress: string = '');
    procedure InjectModulesFromYAML(const AYAMLString: string;
      const ABaseAddress: string = '');
    procedure InjectModulesFromModule(const APath: string);
    procedure ClearTransientCaches;
    procedure RegisterGlobalModule(const AName: string; const AModule: TGocciaModule);
    procedure RegisterGlobalModuleProvider(const AName: string;
      const AProvider: TGocciaGlobalModuleProvider);
    procedure UnregisterGlobalModuleProvider(const AName: string);
    procedure RegisterHostModule(const AName: string;
      const AModule: TGocciaModule);
    procedure RegisterHostModuleProvider(const AName: string;
      const AProvider: TGocciaGlobalModuleProvider);
    procedure UnregisterHostModuleProvider(const AName: string);
    procedure RefreshGlobalThis;
    procedure SuspendRealmExecutionContext;
    function ActivateRealmExecutionContext: TGocciaExecutionContextScope;
    { ES2026 §19.2.1.1 PerformEval for eval code whose variable environment
      is this realm's global environment: an indirect call of the realm's
      `eval`, and ShadowRealm.prototype.evaluate (TC39 ShadowRealm §3.1.3).
      ASourceText is the eval's source; the caller has handled a non-String.
      Parses it as a Script, checks the static early errors, compiles it and
      runs it on the VM in this realm (ADR 0134 Phase 2). Returns the
      completion value. When it raises, AStage tells whether compiling or
      running failed. }
    function EvaluateGlobalEval(const ASourceText, ASourcePath: string;
      out AStage: TGocciaGlobalEvalStage): TGocciaValue;
    function AddExtension(
      const AExtension: TGocciaEngineExtension): TGocciaEngineExtension;
    function FindExtension(
      const AClass: TGocciaEngineExtensionClass): TGocciaEngineExtension;

    { One-shot helpers that run the source in an engine of their own. }
    class function RunScript(const ASource: string; const AFileName: string = 'inline.goccia'): TGocciaScriptResult; overload;
    class function RunScriptFromStringList(const ASource: TStringList; const AFileName: string): TGocciaScriptResult; overload;

    { The realm's global environment. }
    property GlobalScope: TGocciaGlobalScope read FGlobalScope;
    { Whether the top-level declarations of a program this engine compiles
      bind in the realm's global environment, as a Script's do (ES2026
      §16.1.7 GlobalDeclarationInstantiation): `var` and function
      declarations on the global object, lexical declarations in its
      declarative record, so later programs on the engine see them. False by
      default, which keeps them in the program's own frame. The CLI hosts set
      it to SourceType <> stModule; CompileModule and Execute clear it for a
      run whose goal settles on Module (ADR 0140). }
    property GlobalBackedTopLevel: Boolean read FGlobalBackedTopLevel
      write FGlobalBackedTopLevel;
    { How this engine loads its bootstrap shims; GetDateIntrinsic takes it. }
    property ShimLoader: TGocciaShimLoader read FShimLoader;
    property Resolver: TGocciaModuleResolver read GetResolver;
    property ContentProvider: TGocciaModuleContentProvider read GetContentProvider;
    property ModuleResolver: TGocciaModuleResolver read GetModuleResolver;
    property ModuleLoader: TGocciaModuleLoader read FModuleLoader;
    property HostEnvironment: TGocciaHostEnvironment read FHostEnvironment;
    property CapabilityAuditSink: TGocciaCapabilityAuditSink
      read FCapabilityAuditSink write FCapabilityAuditSink;
    property Capabilities: TGocciaCapabilities read FCapabilities;
    { Where the host says the set came from, such as `cli --allow-net=a.test;
      config /repo/goccia.json trusted sha256:...`. The reason of the
      capabilities.effective event when set. }
    property CapabilityProvenance: string read FCapabilityProvenance
      write FCapabilityProvenance;
    { The directory whose files static imports with literal specifiers may
      read without a read grant: the nearest goccia.json/.json5/.toml above the
      entry, else the entry's directory. Canonical. }
    property ProjectRoot: string read FProjectRoot write SetProjectRoot;
    { Response-body ceiling for fetch in bytes; zero selects the default.
      A setting, not a capability (ADR 0122). }
    property FetchMaxResponseBytes: Integer read FFetchMaxResponseBytes
      write FFetchMaxResponseBytes;
    { The embedder's transport for this engine's fetch() requests, or nil
      (the default) for the built-in network client. Read when each request
      starts, so it can be set or replaced between runs; not inherited by
      any other engine (ADR 0146). }
    property FetchTransport: IGocciaFetchTransport read FFetchTransport
      write FFetchTransport;
    property SourcePath: string read FSourcePath;
    { Read-only view of the entry source. Runtime extensions attach after
      Initialize has stored the source but before Execute parses it, which is
      the only window in which an extension can rewrite the module graph for
      the file it is about to run — the vitest compatibility shim uses it to
      hoist `vi.mock` calls into virtual modules. Deliberately read-only: the
      engine owns the list and callers must not mutate what it will parse. }
    property SourceLines: TStringList read FSourceLines;
    property FunctionConstructor: TGocciaFunctionConstructorClassValue read FFunctionConstructor;
    property ObjectConstructor: TGocciaClassValue read FObjectConstructor;
    property Preprocessors: TGocciaPreprocessors read FPreprocessors write SetPreprocessors;
    property Compatibility: TGocciaCompatibilityFlags read FCompatibility write SetCompatibility;
    property LabelStatementsEnabled: Boolean
      read FLabelStatementsEnabled write SetLabelStatementsEnabled;
    property ForInLoopsEnabled: Boolean
      read FForInLoopsEnabled write SetForInLoopsEnabled;
    property ExperimentalJSModuleSourceEnabled: Boolean
      read FExperimentalJSModuleSourceEnabled
      write SetExperimentalJSModuleSourceEnabled;
    property WarningUnsupportedFeatures: Boolean
      read FWarningUnsupportedFeatures write SetWarningUnsupportedFeatures;
    { The entry's goal. stDetect (the default unless the file name is .mjs or
      .mts) leaves it to the source (ADR 0140): Execute settles it for each
      run. A host that parses the entry itself assigns the goal its parse
      settled (TGocciaSourcePipelineResult.SourceType) before CompileModule.
      Bytecode execution settles stDetect from the artifact's profile
      without changing this setting. }
    property SourceType: TGocciaSourceType read FSourceType write FSourceType;
    property StrictTypes: Boolean read FStrictTypes write SetStrictTypes;
    property Shims: TStringList read FShims;
    property BuiltinMath: TGocciaMath read FBuiltinMath;
    property BuiltinGlobalObject: TGocciaGlobalObject read FBuiltinGlobalObject;
    property BuiltinGlobalArray: TGocciaGlobalArray read FBuiltinGlobalArray;
    property BuiltinGlobalNumber: TGocciaGlobalNumber read FBuiltinGlobalNumber;
    property BuiltinGlobals: TGocciaGlobals read FBuiltinGlobals;
    property BuiltinJSON: TGocciaJSONBuiltin read FBuiltinJSON;
    property BuiltinSymbol: TGocciaGlobalSymbol read FBuiltinSymbol;
    property BuiltinMap: TGocciaGlobalMap read FBuiltinMap;
    property BuiltinPromise: TGocciaGlobalPromise read FBuiltinPromise;
    property BuiltinTemporal: TGocciaTemporalBuiltin read FBuiltinTemporal;
    property BuiltinIntl: TGocciaIntlBuiltin read FBuiltinIntl;
    property BuiltinAtomics: TGocciaAtomics read FBuiltinAtomics;
    property BuiltinArrayBuffer: TGocciaGlobalArrayBuffer read FBuiltinArrayBuffer;
    property BuiltinProxy: TGocciaGlobalProxy read FBuiltinProxy;
    property BuiltinReflect: TGocciaGlobalReflect read FBuiltinReflect;
    property GocciaGlobal: TGocciaObjectValue read FGocciaGlobal;
    property Realm: TGocciaRealm read FRealm;
    { The bytecode VM this engine runs on; a heap snapshot releases its idle
      scratch (ADR 0148). }
    property VM: TGocciaVM read FVM;
    property SuppressWarnings: Boolean read FSuppressWarnings write FSuppressWarnings;
    property UnhandledRejections: TGocciaUnhandledRejectionMode
      read FUnhandledRejections write FUnhandledRejections;
    { ADR 0148: the extension that owns the run's host calls; one of this
      engine's extensions, or nil for none. }
    property HostCallOwner: TGocciaEngineExtension read FHostCallOwner
      write FHostCallOwner;
    { ADR 0148: called where the idle wait would call HostCallOwner's
      WaitForIdle, before it blocks, whenever the run is at a pause
      checkpoint. }
    property OnPauseCheckpoint: TGocciaPauseCheckpointEvent
      read FOnPauseCheckpoint write FOnPauseCheckpoint;
    { Observes what UnhandledRejections decides about. Once a run has nothing
      left to do, and before the mode is applied, the hook is called for each
      promise the current microtask scope holds as rejected with no handler,
      oldest first. A promise the hook gives a handler is handled: the run
      does not fail for it. A run that ends by exception reports nothing.
      Under urIgnore the reported promises are forgotten afterwards, so
      TakeUnhandledRejection finds nothing after the run.

      Only a nested Execute has a scope of its own. An ExecuteProgram or
      RunModule that runs inside another engine's run shares that run's
      scope, and reports and forgets its rejections too.

      The hook may run script; a rejection that script leaves is not
      reported, and a run the hook starts on this engine reports nothing.
      The queue keeps the promise alive while it is unhandled: a hook that
      gives it a handler and goes on using it roots it itself. An exception
      the hook raises ends the run. }
    property OnUnhandledRejection: TGocciaUnhandledRejectionEvent
      read FOnUnhandledRejection write FOnUnhandledRejection;
    property LastTiming: TGocciaScriptResult read FLastTiming;
    // Source map from the most recent source pipeline run, if any.
    // Ownership transfers to the caller when read (set to nil after access).
    function TakeLastSourceMap: TGocciaSourceMap;
  end;


implementation

uses
  Math,

  TextSemantics,
  TimingUtils,

  Goccia.AsyncContext,
  Goccia.Bytecode.Binary,
  Goccia.Bytecode.Profile,
  Goccia.Bytecode.Profile.Options,
  Goccia.CallStack,
  Goccia.Compiler,
  Goccia.Compiler.ConstantValue,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Coverage,
  Goccia.Error,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Eval.EarlyErrors,
  Goccia.Execution.CallSite,
  Goccia.FetchManager,
  Goccia.FileExtensions,
  Goccia.FloatingPoint,
  Goccia.GarbageCollector,
  Goccia.ImportMeta,
  Goccia.InstructionLimit,
  Goccia.Keywords.Contextual,
  Goccia.MicrotaskQueue,
  Goccia.Platform,
  Goccia.Profiler,
  Goccia.Scope.Declarations,
  Goccia.Scope.Redeclaration,
  Goccia.Spec,
  Goccia.Timeout,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.ArrayValue,
  Goccia.Values.BooleanObjectValue,
  Goccia.Values.DataViewValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FinalizationRegistryValue,
  Goccia.Values.MapValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.NumberObjectValue,
  Goccia.Values.PromiseValue,
  Goccia.Values.SetValue,
  Goccia.Values.SharedArrayBufferValue,
  Goccia.Values.StringObjectValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.Uint8ArrayEncoding,
  Goccia.Values.WeakMapValue,
  Goccia.Values.WeakRefValue,
  Goccia.Values.WeakSetValue,
  Goccia.Version,
  Goccia.VM.Exception;

threadvar
  { The engine whose run — Execute, ExecuteProgram, RunModule — is innermost
    on this thread, or nil between runs. A run that starts while a different
    engine holds it is nested inside that engine's execution. Dereferenced
    only by RunRunningEngineWorkForPromiseWait, inside that engine's run. }
  GRunningEngine: TGocciaEngine;

{ TGocciaEngineExtension }

procedure TGocciaEngineExtension.WaitForIdle;
begin
end;

function TGocciaEngineExtension.HasPendingWork: Boolean;
begin
  Result := False;
end;

function TGocciaEngineExtension.RunPendingWorkForPromiseWait: Boolean;
begin
  Result := HasPendingWork;
  if Result then
    WaitForIdle;
end;

function TGocciaEngineExtension.HeapSnapshotRefusal: string;
begin
  Result := '';
end;

procedure TGocciaEngineExtension.MaterializeHeapSnapshotBuiltin(
  const AName: string);
begin
end;

procedure TGocciaEngineExtension.DiscardPending;
begin
end;

function TGocciaEngineExtension.InjectGlobalsFromJSON5(
  const AJSON5String: string): Boolean;
begin
  Result := False;
end;

function TGocciaEngineExtension.InjectGlobalsFromTOML(
  const ATOMLString: string): Boolean;
begin
  Result := False;
end;

function TGocciaEngineExtension.InjectGlobalsFromYAML(
  const AYamlString: string): Boolean;
begin
  Result := False;
end;

function TGocciaEngineExtension.InjectModulesFromJSON5(
  const AJSON5String: string; const ABaseAddress: string): Boolean;
begin
  Result := False;
end;

function TGocciaEngineExtension.InjectModulesFromTOML(
  const ATOMLString: string; const ABaseAddress: string): Boolean;
begin
  Result := False;
end;

function TGocciaEngineExtension.InjectModulesFromYAML(
  const AYAMLString: string; const ABaseAddress: string): Boolean;
begin
  Result := False;
end;

{ TGocciaEngine }

function TGocciaEngine.GetContentProvider: TGocciaModuleContentProvider;
begin
  Result := FModuleLoader.ContentProvider;
end;

function TGocciaEngine.GetModuleResolver: TGocciaModuleResolver;
begin
  Result := FModuleLoader.Resolver;
end;

procedure TGocciaEngine.RegisterGlobal(const AName: string;
  const AValue: TGocciaValue);
begin
  if FGlobalScope.ContainsOwnLexicalBinding(AName) then
  begin
    if FInjectedGlobals.IndexOf(AName) >= 0 then
      FGlobalScope.ForceUpdateBinding(AName, AValue)
    else
      ThrowError(
        'Cannot override built-in global "' + AName + '" via globals injection.',
        0, 0);
  end
  else
  begin
    FGlobalScope.DefineLexicalBinding(AName, AValue, dtConst);
    FInjectedGlobals.Add(AName);
  end;
end;

procedure TGocciaEngine.RegisterLazyGlobal(const AName: string;
  const AFactory: TGocciaLazyPropertyFactory;
  const ADeclarationType: TGocciaDeclarationType);
begin
  // Public seam for runtime extensions: expose a global name whose backing
  // object is built only on first reflective touch.  Mirrors the lazy
  // built-in path (DefineBuiltinBindingPlaceholder + DefineLazyGlobalThisProperty)
  // but is callable after engine boot, when extensions attach.
  DefineBuiltinBindingPlaceholder(AName, ADeclarationType);
  DefineLazyGlobalThisProperty(AName, AFactory);
  // Mark the binding as already mirrored so the RefreshGlobalThis that
  // TGocciaRuntimeCore.Install runs after each attach skips it, preserving the
  // lazy descriptor instead of resolving (and materializing) the value while
  // re-mirroring globals.
  FGlobalScope.MarkGlobalObjectBackedBinding(AName);
end;

procedure TGocciaEngine.DefineLazyObjectProperty(
  const ATarget: TGocciaObjectValue; const AName: string;
  const AFactory: TGocciaLazyPlainFactory; const AFlags: TPropertyFlags);
var
  Thunk: TGocciaLazyValueThunk;
begin
  if not Assigned(ATarget) then
    Exit;
  // Back a lazy own property on ATarget with a standalone factory, deferring its
  // construction until the property is first read or reflected on.  The engine
  // owns the bridging thunk for its lifetime.
  Thunk := TGocciaLazyValueThunk.Create(AFactory);
  FLazyThunks.Add(Thunk);
  ATarget.DefineProperty(AName,
    TGocciaLazyPropertyDescriptorData.Create(Thunk.Materialize, AFlags));
end;

procedure TGocciaEngine.InjectGlobalsFromJSON(const AJsonString: string);
var
  Parser: TGocciaJSONParser;
  ParsedValue: TGocciaValue;
  Obj: TGocciaObjectValue;
  Key: string;
begin
  Parser := TGocciaJSONParser.Create;
  try
    ParsedValue := Parser.Parse(AJsonString);
  finally
    Parser.Free;
  end;

  if not (ParsedValue is TGocciaObjectValue) then
    ThrowError('Globals JSON must be a top-level object.', 0, 0);

  TGarbageCollector.Instance.AddTempRoot(ParsedValue);
  try
    Obj := TGocciaObjectValue(ParsedValue);
    for Key in Obj.GetOwnPropertyKeys do
      RegisterGlobal(Key, Obj.GetProperty(Key));
  finally
    TGarbageCollector.Instance.RemoveTempRoot(ParsedValue);
  end;
end;

procedure TGocciaEngine.InjectGlobalsFromJSON5(
  const AJSON5String: string);
var
  I: Integer;
begin
  for I := 0 to FExtensions.Count - 1 do
    if FExtensions[I].InjectGlobalsFromJSON5(AJSON5String) then
      Exit;
  ThrowError('JSON5 globals require a runtime extension.', 0, 0);
end;

procedure TGocciaEngine.InjectGlobalsFromTOML(
  const ATOMLString: string);
var
  I: Integer;
begin
  for I := 0 to FExtensions.Count - 1 do
    if FExtensions[I].InjectGlobalsFromTOML(ATOMLString) then
      Exit;
  ThrowError('TOML globals require a runtime extension.', 0, 0);
end;

procedure TGocciaEngine.InjectGlobalsFromYAML(const AYamlString: string);
var
  I: Integer;
begin
  for I := 0 to FExtensions.Count - 1 do
    if FExtensions[I].InjectGlobalsFromYAML(AYamlString) then
      Exit;
  ThrowError('YAML globals require a runtime extension.', 0, 0);
end;

procedure TGocciaEngine.InjectGlobalsFromModule(const APath: string);
var
  ExportName: string;
  ExportNames: TArray<string>;
  ExportValue: TGocciaValue;
  Module: TGocciaModule;
  FloatingPointState: TGocciaFloatingPointState;
begin
  // The module is compiled and run here, possibly outside any run.
  EnterGocciaFloatingPointScope(FloatingPointState);
  try
    // Host enrollment is durable on the root module/address. Every static,
    // dynamic or deferred import from it inherits host ownership regardless
    // of when the import resolves.
    Module := FModuleLoader.LoadHostModule(APath, FSourcePath);
    ExportNames := Module.GetExportNames;
    for ExportName in ExportNames do
      if Module.TryGetExportValue(ExportName, ExportValue) then
        RegisterGlobal(ExportName, ExportValue);
  finally
    LeaveGocciaFloatingPointScope(FloatingPointState);
  end;
end;

procedure TGocciaEngine.InjectModule(const AName, AContent: string;
  const AType: string; const ABaseAddress: string);
var
  BaseAddress: string;
begin
  BaseAddress := ABaseAddress;
  if BaseAddress = '' then
    BaseAddress := FSourcePath;
  FModuleLoader.InjectModule(AName, AContent, AType, BaseAddress,
    'embedding API');
end;

procedure TGocciaEngine.InjectModulesFromValue(const AValue: TGocciaValue;
  const ABaseAddress, AProvenance: string);
var
  Address, BaseAddress, Content, ContentType: string;
  ContentValue, DescriptorValue, TypeValue: TGocciaValue;
  Descriptor: TGocciaObjectValue;
  Stringifier: TGocciaJSONStringifier;
begin
  BaseAddress := ABaseAddress;
  if BaseAddress = '' then
    BaseAddress := FSourcePath;
  if (AValue is TGocciaArrayValue) or
     not (AValue is TGocciaObjectValue) then
    raise EArgumentException.Create(
      'Virtual modules manifest must be a top-level object.');

  for Address in TGocciaObjectValue(AValue).GetOwnPropertyKeys do
  begin
    DescriptorValue := TGocciaObjectValue(AValue).GetProperty(Address);
    if (DescriptorValue is TGocciaArrayValue) or
       not (DescriptorValue is TGocciaObjectValue) then
      raise EArgumentException.CreateFmt(
        'Virtual module "%s" must be an object descriptor.', [Address]);
    Descriptor := TGocciaObjectValue(DescriptorValue);
    if not Descriptor.HasOwnProperty('content') then
      raise EArgumentException.CreateFmt(
        'Virtual module "%s" is missing its content field.', [Address]);

    ContentType := '';
    if Descriptor.HasOwnProperty('type') then
    begin
      TypeValue := Descriptor.GetProperty('type');
      if not (TypeValue is TGocciaStringLiteralValue) then
        raise EArgumentException.CreateFmt(
          'Virtual module "%s" type must be a string.', [Address]);
      ContentType := TGocciaStringLiteralValue(TypeValue).Value;
    end;

    ContentValue := Descriptor.GetProperty('content');
    if SameText(ContentType, 'json') then
    begin
      Stringifier := TGocciaJSONStringifier.Create;
      try
        Content := Stringifier.Stringify(ContentValue);
      finally
        Stringifier.Free;
      end;
    end
    else
    begin
      if not (ContentValue is TGocciaStringLiteralValue) then
        raise EArgumentException.CreateFmt(
          'Virtual module "%s" content must be a string for type "%s".',
          [Address, ContentType]);
      Content := TGocciaStringLiteralValue(ContentValue).Value;
    end;
    FModuleLoader.InjectModule(Address, Content, ContentType, BaseAddress,
      AProvenance);
  end;
end;

procedure TGocciaEngine.InjectModulesFromJSON(const AJSONString: string;
  const ABaseAddress: string);
var
  Parser: TGocciaJSONParser;
  ParsedValue: TGocciaValue;
begin
  Parser := TGocciaJSONParser.Create;
  try
    ParsedValue := Parser.Parse(AJSONString);
  finally
    Parser.Free;
  end;
  if (TGarbageCollector.Instance <> nil) and Assigned(ParsedValue) then
    TGarbageCollector.Instance.AddTempRoot(ParsedValue);
  try
    InjectModulesFromValue(ParsedValue, ABaseAddress, 'JSON manifest');
  finally
    if (TGarbageCollector.Instance <> nil) and Assigned(ParsedValue) then
      TGarbageCollector.Instance.RemoveTempRoot(ParsedValue);
  end;
end;

procedure TGocciaEngine.InjectModulesFromJSON5(
  const AJSON5String: string; const ABaseAddress: string);
var
  I: Integer;
begin
  for I := 0 to FExtensions.Count - 1 do
    if FExtensions[I].InjectModulesFromJSON5(AJSON5String,
       ABaseAddress) then
      Exit;
  ThrowError('JSON5 virtual modules require a runtime extension.', 0, 0);
end;

procedure TGocciaEngine.InjectModulesFromTOML(const ATOMLString: string;
  const ABaseAddress: string);
var
  I: Integer;
begin
  for I := 0 to FExtensions.Count - 1 do
    if FExtensions[I].InjectModulesFromTOML(ATOMLString,
       ABaseAddress) then
      Exit;
  ThrowError('TOML virtual modules require a runtime extension.', 0, 0);
end;

procedure TGocciaEngine.InjectModulesFromYAML(const AYAMLString: string;
  const ABaseAddress: string);
var
  I: Integer;
begin
  for I := 0 to FExtensions.Count - 1 do
    if FExtensions[I].InjectModulesFromYAML(AYAMLString,
       ABaseAddress) then
      Exit;
  ThrowError('YAML virtual modules require a runtime extension.', 0, 0);
end;

procedure TGocciaEngine.InjectModulesFromModule(const APath: string);
var
  DefaultValue: TGocciaValue;
  Module: TGocciaModule;
  FloatingPointState: TGocciaFloatingPointState;
begin
  // The manifest module is compiled and run here, possibly outside any run.
  EnterGocciaFloatingPointScope(FloatingPointState);
  try
    // The manifest module is host-provided config. Durable ownership also
    // covers imports its exported code may initiate after manifest evaluation
    // returns.
    Module := FModuleLoader.LoadHostModule(APath, FSourcePath);
    if not Module.TryGetExportValue(KEYWORD_DEFAULT, DefaultValue) then
      raise EArgumentException.Create(
        'Virtual modules manifest module must have a default export.');
    InjectModulesFromValue(DefaultValue, Module.Path, Module.Path);
  finally
    LeaveGocciaFloatingPointScope(FloatingPointState);
  end;
end;

procedure TGocciaEngine.ClearTransientCaches;
begin
  // The VM runs directly on TGocciaValue and keeps no transient caches that
  // need clearing between measurements.
end;

function TGocciaEngine.AddExtension(
  const AExtension: TGocciaEngineExtension): TGocciaEngineExtension;
begin
  if not Assigned(AExtension) then
    raise Exception.Create('Engine extension cannot be nil.');

  FExtensions.Add(AExtension);
  Result := AExtension;
end;

function TGocciaEngine.FindExtension(
  const AClass: TGocciaEngineExtensionClass): TGocciaEngineExtension;
var
  I: Integer;
begin
  for I := 0 to FExtensions.Count - 1 do
    if FExtensions[I].InheritsFrom(AClass) then
      Exit(FExtensions[I]);
  Result := nil;
end;

const
  PROJECT_CONFIG_FILE_NAMES: array[0..2] of string = ('goccia.json',
    'goccia.json5', 'goccia.toml');

{ The module-graph exemption's project: the directory of the nearest goccia
  config above the entry, or the entry's own directory (ADR 0122). }
function DiscoverCapabilityProjectRoot(const AEntryFileName: string): string;
var
  EntryDirectory, Directory, ParentDirectory: string;
  I: Integer;
begin
  EntryDirectory := ExcludeTrailingPathDelimiter(
    ExtractFilePath(ExpandFileName(AEntryFileName)));
  if EntryDirectory = '' then
    EntryDirectory := GetCurrentDir;
  Directory := EntryDirectory;
  while Directory <> '' do
  begin
    for I := Low(PROJECT_CONFIG_FILE_NAMES) to High(PROJECT_CONFIG_FILE_NAMES) do
      if FileExists(IncludeTrailingPathDelimiter(Directory) +
         PROJECT_CONFIG_FILE_NAMES[I]) then
        Exit(CanonicalCapabilityPath(Directory));
    ParentDirectory := ExtractFileDir(Directory);
    if (ParentDirectory = '') or (ParentDirectory = Directory) then
      Break;
    Directory := ParentDirectory;
  end;
  Result := CanonicalCapabilityPath(EntryDirectory);
end;

constructor TGocciaEngine.Create(const AFileName: string;
  const ASourceLines: TStringList);
begin
  Create(AFileName, ASourceLines, TGocciaCapabilities.None);
end;

constructor TGocciaEngine.Create(const AFileName: string;
  const ASourceLines: TStringList;
  const AResolver: TGocciaModuleResolver);
begin
  Create(AFileName, ASourceLines, AResolver, TGocciaCapabilities.None);
end;

constructor TGocciaEngine.Create(const AFileName: string;
  const ASourceLines: TStringList; const AModuleLoader: TGocciaModuleLoader);
begin
  Create(AFileName, ASourceLines, AModuleLoader, TGocciaCapabilities.None);
end;

constructor TGocciaEngine.Create(const AFileName: string;
  const ASourceLines: TStringList;
  const ACapabilities: TGocciaCapabilities);
begin
  Initialize(AFileName, ASourceLines, TGocciaModuleLoader.Create(AFileName),
    True, ACapabilities);
end;

constructor TGocciaEngine.Create(const AFileName: string;
  const ASourceLines: TStringList;
  const AResolver: TGocciaModuleResolver;
  const ACapabilities: TGocciaCapabilities);
begin
  Initialize(AFileName, ASourceLines,
    TGocciaModuleLoader.Create(AFileName, AResolver), True, ACapabilities);
end;

constructor TGocciaEngine.Create(const AFileName: string;
  const ASourceLines: TStringList; const AModuleLoader: TGocciaModuleLoader;
  const ACapabilities: TGocciaCapabilities);
begin
  Initialize(AFileName, ASourceLines, AModuleLoader, False, ACapabilities);
end;

procedure TGocciaEngine.ExecuteShims;
var
  I, J: Integer;
  Shim: TGocciaShimDefinition;
  Materializer: TGocciaShimMaterializer;
  Member: TGocciaShimPrototypeMember;
  MemberMaterializer: TGocciaShimMemberMaterializer;
  Members: TGocciaShimPrototypeMembers;
  Target: TGocciaObjectValue;
begin
  // No shim runs at boot. Each is compiled and run the first time a name it
  // provides is touched: the heaviest
  // shim (Date, ~671 source lines) is then never parsed for scripts that
  // don't use it, and empty-script boot pays for none of them.
  for I := 0 to DefaultShimCount - 1 do
  begin
    Shim := DefaultShim(I);
    Members := ShimPrototypeMembers(I);
    if Length(Members) = 0 then
    begin
      Materializer := TGocciaShimMaterializer.Create(FShimLoader, I);
      FLazyThunks.Add(Materializer);
      RegisterLazyGlobal(Shim.Name, Materializer.Materialize, dtConst);
      Continue;
    end;
    // A prototype-member shim installs each member as a lazy property of
    // its prototype, in the order the shim used to define them at boot.
    for J := 0 to High(Members) do
    begin
      Member := Members[J];
      case Member.Target of
        stNumberPrototype:
          Target := TGocciaNumberObjectValue.GetSharedPrototype;
        stArrayPrototype:
          Target := GetSharedArrayPrototypeForRealm(FRealm);
      else
        Target := TGocciaObjectValue.SharedObjectPrototype;
      end;
      if not Assigned(Target) or
         (Member.OnlyIfAbsent and Target.HasOwnProperty(Member.Name)) then
        Continue;
      MemberMaterializer := TGocciaShimMemberMaterializer.Create(FShimLoader, I,
        Member.ExportKey);
      FLazyThunks.Add(MemberMaterializer);
      if Member.Accessor then
        Target.DefineProperty(Member.Name,
          TGocciaLazyPropertyDescriptorData.CreateAccessor(
            MemberMaterializer.MaterializeAccessor, [pfConfigurable]))
      else
        Target.DefineProperty(Member.Name,
          TGocciaLazyPropertyDescriptorData.Create(
            MemberMaterializer.Materialize, [pfWritable, pfConfigurable]));
    end;
  end;
end;

function TGocciaEngine.CreateEvaluationContext: TGocciaEvaluationContext;
begin
  Result := Default(TGocciaEvaluationContext);
  Result.Realm := FRealm;
  Result.Scope := FGlobalScope;
  Result.OnError := ThrowError;
  Result.LoadModule := FModuleLoader.LoadModule;
  Result.LoadModuleSource := FModuleLoader.LoadModuleSourceValue;
  Result.LoadDeferredModule := FModuleLoader.LoadDeferredModuleNamespaceValue;
  Result.ResolveModuleURL := FModuleLoader.ResolveModuleURL;
  Result.CurrentFilePath := FSourcePath;
  Result.NonStrictMode := cfNonStrictMode in FCompatibility;
  Result.CompatibilityNonStrictMode := cfNonStrictMode in FCompatibility;
end;

procedure TGocciaEngine.Initialize(const AFileName: string;
  const ASourceLines: TStringList; const AModuleLoader: TGocciaModuleLoader;
  const AOwnsModuleLoader: Boolean;
  const ACapabilities: TGocciaCapabilities);
begin
  { Created before TGarbageCollector.Initialize below: the VM charges its
    stack growth to the collector the thread has when it is created
    (TGocciaVM.StackChargeCollector). }
  FVM := TGocciaVM.Create;
  { A set with no layers is None; normalize so ToJSON and the audit event show
    the root layer. }
  if ACapabilities.LayerCount = 0 then
    FCapabilities := TGocciaCapabilities.None
  else
    FCapabilities := ACapabilities;
  FProjectRoot := DiscoverCapabilityProjectRoot(AFileName);
  FUnhandledRejections := urThrow;
  { Not a valid token until EnterEngineAsyncContext returns one, so a
    constructor that fails before then cannot make Destroy unwind past an
    enclosing engine's entry. }
  FAsyncContextToken := -1;
  FSourcePath := AFileName;
  FSourceLines := ASourceLines;
  FModuleLoader := AModuleLoader;
  FOwnsModuleLoader := AOwnsModuleLoader;
  if not Assigned(FModuleLoader) then
  begin
    FModuleLoader := TGocciaModuleLoader.Create(AFileName);
    FOwnsModuleLoader := True;
  end;

  TGarbageCollector.Initialize;
  TGocciaCallStack.Initialize;
  TGocciaMicrotaskQueue.Initialize;

  { Start on an empty async context and remember what this thread was holding,
    so neither a reused worker-thread slot nor an enclosing engine leaks its
    snapshot into this one. Destroy restores it. }
  FAsyncContextToken := EnterEngineAsyncContext;

  // Per-realm intrinsic state (Array.prototype, ...) lives on FRealm.  The
  // execution-context stack makes it current after the global environment is
  // available, before any built-in construction performs lazy intrinsic lookup.
  FRealm := TGocciaRealm.Create(AFileName);
  FHostEnvironment := TGocciaHostEnvironment.Create;

  FPreprocessors := DefaultPreprocessors;
  FCompatibility := DefaultCompatibility;
  if IsModuleSourceFileName(AFileName) then
    FSourceType := stModule
  else
    FSourceType := DefaultSourceType;
  FStrictTypes := False;
  FShims := TStringList.Create;
  FExtensions := TGocciaEngineExtensionList.Create(True);

  FGlobalScope := TGocciaGlobalScope.Create;
  // Seed the error channel at the root: every scope created under the
  // global scope inherits OnError at construction, so code whose scope
  // chain was built before an error callback reached it (class bodies,
  // static methods) can still throw structured errors.
  FGlobalScope.OnError := ThrowError;
  FModuleLoader.BindRuntime(FGlobalScope, ThrowError);
  FGlobalScope.LoadModule := FModuleLoader.LoadModule;
  FGlobalScope.LoadModuleSource := FModuleLoader.LoadModuleSourceValue;
  FGlobalScope.LoadDeferredModule :=
    FModuleLoader.LoadDeferredModuleNamespaceValue;
  FGlobalScope.ResolveModuleURL := FModuleLoader.ResolveModuleURL;
  FModuleLoader.JSXEnabled := ppJSX in FPreprocessors;
  FRealm.GlobalEnv := FGlobalScope;
  FRealm.LoadedModules := FModuleLoader;
  FLabelStatementsEnabled := False;
  FForInLoopsEnabled := False;
  FExperimentalJSModuleSourceEnabled := False;
  FWarningUnsupportedFeatures := DefaultWarningUnsupportedFeatures;
  if Assigned(FModuleLoader) then
  begin
    FModuleLoader.LabelStatementsEnabled := FLabelStatementsEnabled;
    FModuleLoader.ForInLoopsEnabled := FForInLoopsEnabled;
    FModuleLoader.ExperimentalJSModuleSourceEnabled :=
      FExperimentalJSModuleSourceEnabled;
    FModuleLoader.WarningUnsupportedFeatures := FWarningUnsupportedFeatures;
  end;
  FRealmExecutionContext := TGocciaExecutionContextScope.Create(
    CreateExecutionContext(FRealm, FGlobalScope, AFileName));

  TGarbageCollector.Instance.AddRootObject(FGlobalScope);

  FInjectedGlobals := TStringList.Create;
  // Owns the lazy materializers (shims, runtime namespaces) whose factory
  // closures back lazy global properties, and the plain objects whose methods
  // back native functions (the TypedArray statics, Uint8Array encoding);
  // freed in the destructor.
  FLazyThunks := Contnrs.TObjectList.Create(True);
  RegisterDefaultShimNames(FShims);
  PinSingletons;
  RegisterBuiltIns;

  FRetainedModules := Contnrs.TObjectList.Create(True);

  FVM.EnsureStackRootRegistered;
  FVM.Realm := FRealm;
  FVM.GlobalScope := FGlobalScope;
  FVM.GlobalThisValue := FGlobalScope.ThisValue;
  FVM.LoadModule := FModuleLoader.LoadModule;
  FVM.LoadModuleSource := FModuleLoader.LoadModuleSourceValue;
  FVM.LoadDeferredModule := FModuleLoader.LoadDeferredModuleNamespaceValue;
  FVM.ResolveModuleURL := FModuleLoader.ResolveModuleURL;
  FVM.CompileDirectEval := CompileDirectEvalCode;
  FModuleLoader.EvaluateModuleBody := EvaluateModuleBody;
  FModuleLoader.InstantiateModuleFunctions := InstantiateModuleFunctions;
  FModuleLoader.ReadModuleBytecode := DecodeModuleBytecode;
  FModuleLoader.ReleaseModuleBytecode := ReleaseModuleBytecode;
  FModuleLoader.ValidateModuleBytecode := ValidateImportedBytecode;
  // A shim runs on the VM, so the VM is set up first.
  FShimLoader.Realm := FRealm;
  FShimLoader.GlobalScope := FGlobalScope;
  FShimLoader.Evaluate := EvaluateShim;
  RememberShimLoader(FShimLoader);
  ExecuteShims;

  if Assigned(FFunctionConstructor) then
  begin
    FFunctionConstructor.CompileDynamicFunction := CompileDynamicFunction;
    FFunctionConstructor.CapabilityAuditEmitter := EmitCapabilityAudit;
  end;
  ApplyCapabilityPolicy;
end;

procedure TGocciaEngine.ApplyCapabilityPolicy;
begin
  if not Assigned(FModuleLoader) then
    Exit;
  FModuleLoader.ConfigureCapabilities(FCapabilities, FProjectRoot,
    EmitCapabilityAudit);
  if Assigned(FModuleLoader.Resolver) then
  begin
    FModuleLoader.Resolver.NodeModulesGrant := GrantNodeModules;
    FModuleLoader.Resolver.ProviderGrant := GrantProviderPackage;
    FModuleLoader.Resolver.ProviderAudit := AuditProviderPackage;
  end;
end;

{ A provider import needs an import scope covering its repository:
  `github`, `github:<owner>`, or `github:<owner>/<repo>` (ADR 0122). The
  PermissionDenied names the package, which is what the import map asked
  for; the suggestion names the grants that would cover it, or the deny
  that refused it. }
procedure TGocciaEngine.GrantProviderPackage(const ASpecifier: string;
  const AAddress: TGocciaProviderAddress);
var
  DenyScope, Owner, Suggestion: string;
begin
  if FCapabilities.AllowsProviderPackage(IMPORT_PROVIDER_GITHUB,
     AAddress.Owner, AAddress.Repository) then
  begin
    EmitCapabilityAudit(gckImportProvider, gcdAllow, AAddress.PackageKey,
      'the import capability covers ' + AAddress.ScopeText);
    Exit;
  end;
  if FCapabilities.ProviderPackageDenyScope(IMPORT_PROVIDER_GITHUB,
     AAddress.Owner, AAddress.Repository, DenyScope) then
  begin
    if DenyScope = '' then
      Suggestion := 'refused by an unscoped import deny'
    else
      Suggestion := 'refused by the import deny ' + DenyScope;
    EmitCapabilityAudit(gckImportProvider, gcdDeny, AAddress.PackageKey,
      Suggestion);
  end
  else
  begin
    Owner := LowerCase(GITHUB_PROVIDER_PREFIX + AAddress.Owner);
    Suggestion := Format(SSuggestProviderImportNotGranted,
      [AAddress.ScopeText, AAddress.ScopeText, Owner]);
    EmitCapabilityAudit(gckImportProvider, gcdDeny, AAddress.PackageKey,
      'the import capability does not cover ' + AAddress.ScopeText);
  end;
  ThrowPermissionDenied(CapabilityName(gcImport), AAddress.PackageKey,
    Suggestion);
end;

procedure TGocciaEngine.AuditProviderPackage(const AAllowed: Boolean;
  const ASubject, AReason: string);
begin
  if AAllowed then
    EmitCapabilityAudit(gckImportProvider, gcdAllow, ASubject, AReason)
  else
    EmitCapabilityAudit(gckImportProvider, gcdDeny, ASubject, AReason);
end;

procedure TGocciaEngine.SetProjectRoot(const AValue: string);
begin
  if AValue = '' then
    FProjectRoot := ''
  else
    FProjectRoot := CanonicalCapabilityPath(AValue);
  ApplyCapabilityPolicy;
end;

function TGocciaEngine.GrantNodeModules(const ASpecifier,
  AImportingDirectory: string; out ACeiling: string): Boolean;
var
  Reason: string;
begin
  Result := FCapabilities.NodeModulesCeiling(AImportingDirectory, ACeiling);
  if Result then
  begin
    if ACeiling = '' then
      Reason := 'node_modules walk is unbounded'
    else
      Reason := 'node_modules walk is bounded by ' + ACeiling;
    EmitCapabilityAudit(gckImportNodeModules, gcdAllow, ASpecifier, Reason);
    Exit;
  end;
  EmitCapabilityAudit(gckImportNodeModules, gcdDeny, ASpecifier,
    'the import capability does not grant node_modules here');
  { An outright deny is a PermissionDenied; a capability that was simply never
    granted keeps the sealed-by-default resolution message. }
  if FCapabilities.DeniesNodeModules(AImportingDirectory) then
    ThrowPermissionDenied(CapabilityName(gcImport), ASpecifier,
      'node_modules resolution is denied for ' + AImportingDirectory);
end;

procedure TGocciaEngine.AuditEffectiveCapabilities;
begin
  if (not Assigned(FCapabilityAuditSink)) or FIsCapabilityChild or
     FEffectiveCapabilitiesAudited then
    Exit;
  FEffectiveCapabilitiesAudited := True;
  if FCapabilityProvenance <> '' then
    EmitCapabilityAudit(gckCapabilitiesEffective, gcdAllow,
      FCapabilities.ToJSON, FCapabilityProvenance)
  else
    EmitCapabilityAudit(gckCapabilitiesEffective, gcdAllow,
      FCapabilities.ToJSON, 'effective capability set at engine start');
end;

destructor TGocciaEngine.Destroy;
var
  I: Integer;
begin
  { Async-context snapshots hold this engine's objects, and `enterWith` can
    leave one installed with no scope to unwind it. Drop everything this engine
    left behind before anything else is torn down, and restore whatever was
    current when it was constructed — engines nest on one thread, so clearing
    the thread outright would strip an outer engine's context mid-run. }
  LeaveEngineAsyncContext(FAsyncContextToken);

  { The thread's queue keeps a promise left rejected alive until a host takes
    it. Code a host calls after the engine's own run — a benchmark body, a
    test — has no run whose end would report or drop it, so drop it here,
    before the realm it lives in goes away. An engine freed inside another
    engine's run (a sandbox child, a ShadowRealm) leaves that run's
    rejections alone. }
  if (not Assigned(GRunningEngine)) and
     Assigned(TGocciaMicrotaskQueue.Instance) then
    TGocciaMicrotaskQueue.Instance.DiscardUnhandledRejections;

  { ApplyCapabilityPolicy wired this engine into the resolver, and a host
    can pass in a loader that outlives the engine. A method pointer left
    behind would make the next provider import call into a freed engine. }
  if Assigned(FModuleLoader) and Assigned(FModuleLoader.Resolver) then
  begin
    if TMethod(FModuleLoader.Resolver.ProviderGrant).Data = Pointer(Self) then
      FModuleLoader.Resolver.ProviderGrant := nil;
    if TMethod(FModuleLoader.Resolver.ProviderAudit).Data = Pointer(Self) then
      FModuleLoader.Resolver.ProviderAudit := nil;
  end;

  { Bytecode callbacks borrow this engine just like resolver callbacks.
    Preserve any replacements the host installed before teardown. }
  if Assigned(FModuleLoader) then
  begin
    if TMethod(FModuleLoader.ReadModuleBytecode).Data = Pointer(Self) then
      FModuleLoader.ReadModuleBytecode := nil;
    if TMethod(FModuleLoader.ReleaseModuleBytecode).Data = Pointer(Self) then
      FModuleLoader.ReleaseModuleBytecode := nil;
    if TMethod(FModuleLoader.ValidateModuleBytecode).Data = Pointer(Self) then
      FModuleLoader.ValidateModuleBytecode := nil;
  end;

  if (TGarbageCollector.Instance <> nil) and Assigned(FGlobalScope) then
    TGarbageCollector.Instance.RemoveRootObject(FGlobalScope);

    if Assigned(FModuleLoader) and Assigned(FRetainedModules) then
      for I := 0 to FRetainedModules.Count - 1 do
        FModuleLoader.ForgetBytecodeModule(
          TGocciaBytecodeModule(FRetainedModules[I]));
    FRetainedModules.Free;
    FLazyThunks.Free;
    FExtensions.Free;
    FBuiltinMath.Free;
    FBuiltinGlobalObject.Free;
    FBuiltinGlobalArray.Free;
    FBuiltinGlobalNumber.Free;
    FBuiltinGlobalBigInt.Free;
    FBuiltinRegExp.Free;
    FBuiltinGlobalString.Free;
    FBuiltinGlobals.Free;
    FBuiltinJSON.Free;
    FBuiltinSymbol.Free;
    FBuiltinMap.Free;
    FBuiltinPromise.Free;
    FBuiltinTemporal.Free;
    FBuiltinIntl.Free;
    FBuiltinAtomics.Free;
    FBuiltinArrayBuffer.Free;
    FBuiltinProxy.Free;
    FBuiltinReflect.Free;
    FBuiltinDisposableStack.Free;
    FHostEnvironment.Free;
    ClearImportMetaCache;
    FLastSourceMap.Free;
    FInjectedGlobals.Free;
    FShims.Free;
    if TGarbageCollector.Instance = nil then
      FGlobalScope.Free;
    if FOwnsModuleLoader then
      FModuleLoader.Free;

    // Drop the realm only after every built-in has been freed - those owners
    // may still touch prototype objects during teardown.  Freeing the realm unpins the
    // intrinsic prototype graph so the GC can collect it before the next
    // engine on this worker thread starts up.
    if Assigned(FRealm) then
    begin
      FRealmExecutionContext.Free;
      FRealmExecutionContext := nil;
      FRealm.Free;
      FRealm := nil;
  end;
  { Last: nothing above runs code on the VM. }
  FVM.Free;
  inherited;
end;

procedure TGocciaEngine.PinSingletons;
begin
  PinPrimitiveSingletons;
  TGarbageCollector.Instance.PinObject(TGocciaHoleValue.HoleValue);
  // Eagerly initialize BigInt singletons on the main thread
  TGocciaBigIntValue.BigIntZero;
  TGocciaBigIntValue.BigIntOne;
end;

procedure TGocciaEngine.RegisterBuiltIns;
var
  Scope: TGocciaScope;
begin
  Scope := FGlobalScope;

  // Core language built-ins: always registered.
  FBuiltinMath := TGocciaMath.Create('Math', Scope, ThrowError,
    FHostEnvironment);
  FBuiltinGlobalObject := TGocciaGlobalObject.Create(CONSTRUCTOR_OBJECT, Scope, ThrowError);
  FBuiltinGlobalArray := TGocciaGlobalArray.Create(CONSTRUCTOR_ARRAY, Scope, ThrowError);
  FBuiltinGlobalNumber := TGocciaGlobalNumber.Create(CONSTRUCTOR_NUMBER, Scope, ThrowError);
  FBuiltinGlobalBigInt := TGocciaGlobalBigInt.Create(CONSTRUCTOR_BIGINT, Scope, ThrowError);
  FBuiltinJSON := TGocciaJSONBuiltin.Create('JSON', Scope, ThrowError);
  FBuiltinSymbol := TGocciaGlobalSymbol.Create(CONSTRUCTOR_SYMBOL, Scope, ThrowError);
  FBuiltinMap := TGocciaGlobalMap.Create(CONSTRUCTOR_MAP, Scope, ThrowError);
  FBuiltinPromise := TGocciaGlobalPromise.Create(CONSTRUCTOR_PROMISE, Scope, ThrowError);
  DefineBuiltinBindingPlaceholder('Temporal', dtLet);
  DefineBuiltinBindingPlaceholder('Intl', dtLet);
  DefineBuiltinBindingPlaceholder(CONSTRUCTOR_ATOMICS, dtLet);
  FBuiltinArrayBuffer := TGocciaGlobalArrayBuffer.Create(CONSTRUCTOR_ARRAY_BUFFER, Scope, ThrowError);
  DefineBuiltinBindingPlaceholder(CONSTRUCTOR_PROXY, dtConst);
  DefineBuiltinBindingPlaceholder('Reflect', dtConst);
  FBuiltinGlobalString := TGocciaGlobalString.Create(CONSTRUCTOR_STRING, Scope, ThrowError);
  FBuiltinGlobals := TGocciaGlobals.Create('Globals', Scope, ThrowError);
  DefineBuiltinBindingPlaceholder(CONSTRUCTOR_DISPOSABLE_STACK, dtConst);
  DefineBuiltinBindingPlaceholder(CONSTRUCTOR_ASYNC_DISPOSABLE_STACK, dtConst);
  Scope.DefineLexicalBinding(CONSTRUCTOR_ITERATOR, TGocciaIteratorValue.CreateGlobalObject, dtConst, True);
  RegisterBuiltinConstructors;
end;

procedure TGocciaEngine.DefineBuiltinBindingPlaceholder(const AName: string;
  const ADeclarationType: TGocciaDeclarationType);
begin
  FGlobalScope.DefineLexicalBinding(AName,
    TGocciaUndefinedLiteralValue.UndefinedValue, ADeclarationType, True);
end;

procedure TGocciaEngine.DefineLazyGlobalThisProperty(const AName: string;
  const AFactory: TGocciaLazyPropertyFactory);
begin
  if not (FRealm.GlobalObject is TGocciaObjectValue) then
    Exit;

  TGocciaObjectValue(FRealm.GlobalObject).DefineProperty(AName,
    TGocciaLazyPropertyDescriptorData.Create(AFactory,
      [pfWritable, pfConfigurable]));
end;

procedure TGocciaEngine.RegisterLazyBuiltinGlobalProperties;
begin
  DefineLazyGlobalThisProperty('Temporal', MaterializeTemporalGlobal);
  DefineLazyGlobalThisProperty('Intl', MaterializeIntlGlobal);
  DefineLazyGlobalThisProperty(CONSTRUCTOR_ATOMICS, MaterializeAtomicsGlobal);
  DefineLazyGlobalThisProperty(CONSTRUCTOR_PROXY, MaterializeProxyGlobal);
  DefineLazyGlobalThisProperty('Reflect', MaterializeReflectGlobal);
  DefineLazyGlobalThisProperty(CONSTRUCTOR_DISPOSABLE_STACK,
    MaterializeDisposableStackGlobal);
  DefineLazyGlobalThisProperty(CONSTRUCTOR_ASYNC_DISPOSABLE_STACK,
    MaterializeAsyncDisposableStackGlobal);
end;

function TGocciaEngine.MaterializeTemporalGlobal: TGocciaValue;
begin
  if not Assigned(FBuiltinTemporal) then
    FBuiltinTemporal := TGocciaTemporalBuiltin.Create('Temporal',
      FGlobalScope, ThrowError, FHostEnvironment, False);
  Result := FBuiltinTemporal.TemporalNamespace;
end;

function TGocciaEngine.MaterializeIntlGlobal: TGocciaValue;
begin
  if not Assigned(FBuiltinIntl) then
  begin
    FBuiltinIntl := TGocciaIntlBuiltin.Create('Intl', FGlobalScope,
      ThrowError, FHostEnvironment, False);
    // Capture the intrinsic %Intl.NumberFormat% / %Intl.DateTimeFormat% into
    // hidden global bindings the locale shims use. ECMA-402 toLocaleString
    // constructs the intrinsic, not the global, so Number/Date toLocaleString
    // must keep working after user code replaces the Intl.NumberFormat /
    // Intl.DateTimeFormat namespace properties. These are captured here (on Intl
    // construction, before any user taint can run) and defined after boot, so
    // they are never mirrored onto globalThis.
    // Define-if-absent: user code could have declared these reserved names
    // before first touching Intl; redeclaring a const binding would throw and
    // abort materialization.
    if not FGlobalScope.ContainsOwnLexicalBinding(
      '__GocciaIntlNumberFormat') then
      FGlobalScope.DefineLexicalBinding('__GocciaIntlNumberFormat',
        FBuiltinIntl.IntlNamespace.GetProperty('NumberFormat'), dtConst, True);
    if not FGlobalScope.ContainsOwnLexicalBinding(
      '__GocciaIntlDateTimeFormat') then
      FGlobalScope.DefineLexicalBinding('__GocciaIntlDateTimeFormat',
        FBuiltinIntl.IntlNamespace.GetProperty('DateTimeFormat'), dtConst, True);
  end;
  Result := FBuiltinIntl.IntlNamespace;
end;

function TGocciaEngine.MaterializeAtomicsGlobal: TGocciaValue;
begin
  if not Assigned(FBuiltinAtomics) then
    FBuiltinAtomics := TGocciaAtomics.Create(CONSTRUCTOR_ATOMICS,
      FGlobalScope, ThrowError, False);
  Result := FBuiltinAtomics.BuiltinObject;
end;

function TGocciaEngine.MaterializeProxyGlobal: TGocciaValue;
begin
  if not Assigned(FBuiltinProxy) then
    FBuiltinProxy := TGocciaGlobalProxy.Create(FGlobalScope, False);
  Result := FBuiltinProxy.ConstructorValue;
end;

function TGocciaEngine.MaterializeReflectGlobal: TGocciaValue;
begin
  if not Assigned(FBuiltinReflect) then
    FBuiltinReflect := TGocciaGlobalReflect.Create('Reflect',
      FGlobalScope, ThrowError, False);
  Result := FBuiltinReflect.BuiltinObject;
end;

function TGocciaEngine.MaterializeDisposableStackGlobal: TGocciaValue;
begin
  if not Assigned(FBuiltinDisposableStack) then
    FBuiltinDisposableStack := TGocciaBuiltinDisposableStack.Create(
      CONSTRUCTOR_DISPOSABLE_STACK, FGlobalScope, ThrowError,
      False);
  Result := FBuiltinDisposableStack.DisposableStackConstructorValue;
end;

function TGocciaEngine.MaterializeAsyncDisposableStackGlobal: TGocciaValue;
begin
  if not Assigned(FBuiltinDisposableStack) then
    FBuiltinDisposableStack := TGocciaBuiltinDisposableStack.Create(
      CONSTRUCTOR_DISPOSABLE_STACK, FGlobalScope, ThrowError,
      False);
  Result := FBuiltinDisposableStack.AsyncDisposableStackConstructorValue;
end;

function ObjectPrototypeProvider: TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.SharedObjectPrototype;
end;

function StringPrototypeProvider: TGocciaObjectValue;
begin
  Result := TGocciaStringObjectValue.GetSharedPrototype;
end;

function NumberPrototypeProvider: TGocciaObjectValue;
begin
  Result := TGocciaNumberObjectValue.GetSharedPrototype;
end;

function BooleanPrototypeProvider: TGocciaObjectValue;
begin
  Result := TGocciaBooleanObjectValue.GetSharedPrototype;
end;

function BuiltinObjectOrNil(const ABuiltin: TGocciaBuiltin): TGocciaObjectValue;
begin
  if Assigned(ABuiltin) then
    Result := ABuiltin.BuiltinObject
  else
    Result := nil;
end;

procedure ExposeMapPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaMapValue.ExposePrototype(AConstructor);
end;

procedure ExposeArrayPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaArrayValue.ExposePrototype(AConstructor);
end;

procedure ExposeSetPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaSetValue.ExposePrototype(AConstructor);
end;

procedure ExposeWeakMapPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaWeakMapValue.ExposePrototype(AConstructor);
end;

procedure ExposeWeakSetPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaWeakSetValue.ExposePrototype(AConstructor);
end;

procedure ExposeWeakRefPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaWeakRefValue.ExposePrototype(AConstructor);
end;

procedure ExposeFinalizationRegistryPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaFinalizationRegistryValue.ExposePrototype(AConstructor);
end;

procedure TGocciaEngine.RegisterBuiltinConstructors;
var
  Key: string;
  GenericConstructor: TGocciaClassValue;
  ObjectConstructor, FunctionConstructor: TGocciaClassValue;
  ArrayConstructor: TGocciaArrayClassValue;
  MapConstructor: TGocciaMapClassValue;
  SetConstructor: TGocciaSetClassValue;
  WeakMapConstructor: TGocciaWeakMapClassValue;
  WeakSetConstructor: TGocciaWeakSetClassValue;
  WeakRefConstructor: TGocciaWeakRefClassValue;
  FinalizationRegistryConstructor: TGocciaFinalizationRegistryClassValue;
  ArrayBufferConstructor: TGocciaArrayBufferClassValue;
  SharedArrayBufferConstructor: TGocciaSharedArrayBufferClassValue;
  DataViewConstructor: TGocciaDataViewClassValue;
  StringConstructor: TGocciaStringClassValue;
  NumberConstructor: TGocciaNumberClassValue;
  BooleanConstructor: TGocciaBooleanClassValue;
  TypeDef: TGocciaTypeDefinition;
  TypedArrayStatic: TGocciaTypedArrayStaticFrom;
  Roots: TGocciaActiveRootFrame;
begin
  // The constructors made here directly are reachable from nothing until the
  // global scope binds them (%TypedArray% until a typed array constructor
  // inherits from it), and every allocation in between can collect.
  Roots.Initialize;
  try
    TGocciaObjectValue.InitializeSharedPrototype;
    TypeDef.ConstructorName := CONSTRUCTOR_OBJECT;
    TypeDef.Kind := gtdkNativeInstanceType;
    TypeDef.ClassValueClass := TGocciaObjectClassValue;
    TypeDef.ExposePrototype := nil;
    TypeDef.PrototypeProvider := @ObjectPrototypeProvider;
    TypeDef.StaticSource := BuiltinObjectOrNil(FBuiltinGlobalObject);
    TypeDef.PrototypeParent := nil;
    TypeDef.AddSpeciesGetter := False;
    RegisterTypeDefinition(FGlobalScope, TypeDef, SpeciesGetter, ObjectConstructor);
    FObjectConstructor := ObjectConstructor;
    if (GetErrorProto <> nil) and (GetErrorProto.Prototype = nil) then
      GetErrorProto.Prototype := ObjectConstructor.Prototype;

    TypeDef.ConstructorName := CONSTRUCTOR_ARRAY;
    TypeDef.Kind := gtdkNativeInstanceType;
    TypeDef.ClassValueClass := TGocciaArrayClassValue;
    TypeDef.ExposePrototype := @ExposeArrayPrototype;
    TypeDef.PrototypeProvider := nil;
    TypeDef.StaticSource := BuiltinObjectOrNil(FBuiltinGlobalArray);
    TypeDef.PrototypeParent := ObjectConstructor.Prototype;
    TypeDef.AddSpeciesGetter := True;
    RegisterTypeDefinition(FGlobalScope, TypeDef, SpeciesGetter, GenericConstructor);
    ArrayConstructor := TGocciaArrayClassValue(GenericConstructor);

    TypeDef.ConstructorName := CONSTRUCTOR_MAP;
    TypeDef.Kind := gtdkCollectionLikeNativeType;
    TypeDef.ClassValueClass := TGocciaMapClassValue;
    TypeDef.ExposePrototype := @ExposeMapPrototype;
    TypeDef.PrototypeProvider := nil;
    TypeDef.StaticSource := BuiltinObjectOrNil(FBuiltinMap);
    TypeDef.PrototypeParent := ObjectConstructor.Prototype;
    TypeDef.AddSpeciesGetter := True;
    RegisterTypeDefinition(FGlobalScope, TypeDef, SpeciesGetter, GenericConstructor);
    MapConstructor := TGocciaMapClassValue(GenericConstructor);

    TypeDef.ConstructorName := CONSTRUCTOR_SET;
    TypeDef.Kind := gtdkCollectionLikeNativeType;
    TypeDef.ClassValueClass := TGocciaSetClassValue;
    TypeDef.ExposePrototype := @ExposeSetPrototype;
    TypeDef.PrototypeProvider := nil;
    TypeDef.StaticSource := nil;
    TypeDef.PrototypeParent := ObjectConstructor.Prototype;
    TypeDef.AddSpeciesGetter := True;
    RegisterTypeDefinition(FGlobalScope, TypeDef, SpeciesGetter, GenericConstructor);
    SetConstructor := TGocciaSetClassValue(GenericConstructor);

    TypeDef.ConstructorName := CONSTRUCTOR_WEAK_MAP;
    TypeDef.Kind := gtdkCollectionLikeNativeType;
    TypeDef.ClassValueClass := TGocciaWeakMapClassValue;
    TypeDef.ExposePrototype := @ExposeWeakMapPrototype;
    TypeDef.PrototypeProvider := nil;
    TypeDef.StaticSource := nil;
    TypeDef.PrototypeParent := ObjectConstructor.Prototype;
    TypeDef.AddSpeciesGetter := False;
    RegisterTypeDefinition(FGlobalScope, TypeDef, SpeciesGetter, GenericConstructor);
    WeakMapConstructor := TGocciaWeakMapClassValue(GenericConstructor);

    TypeDef.ConstructorName := CONSTRUCTOR_WEAK_SET;
    TypeDef.Kind := gtdkCollectionLikeNativeType;
    TypeDef.ClassValueClass := TGocciaWeakSetClassValue;
    TypeDef.ExposePrototype := @ExposeWeakSetPrototype;
    TypeDef.PrototypeProvider := nil;
    TypeDef.StaticSource := nil;
    TypeDef.PrototypeParent := ObjectConstructor.Prototype;
    TypeDef.AddSpeciesGetter := False;
    RegisterTypeDefinition(FGlobalScope, TypeDef, SpeciesGetter, GenericConstructor);
    WeakSetConstructor := TGocciaWeakSetClassValue(GenericConstructor);

    TypeDef.ConstructorName := CONSTRUCTOR_WEAK_REF;
    TypeDef.Kind := gtdkNativeInstanceType;
    TypeDef.ClassValueClass := TGocciaWeakRefClassValue;
    TypeDef.ExposePrototype := @ExposeWeakRefPrototype;
    TypeDef.PrototypeProvider := nil;
    TypeDef.StaticSource := nil;
    TypeDef.PrototypeParent := ObjectConstructor.Prototype;
    TypeDef.AddSpeciesGetter := False;
    RegisterTypeDefinition(FGlobalScope, TypeDef, SpeciesGetter, GenericConstructor);
    WeakRefConstructor := TGocciaWeakRefClassValue(GenericConstructor);

    TypeDef.ConstructorName := CONSTRUCTOR_FINALIZATION_REGISTRY;
    TypeDef.Kind := gtdkNativeInstanceType;
    TypeDef.ClassValueClass := TGocciaFinalizationRegistryClassValue;
    TypeDef.ExposePrototype := @ExposeFinalizationRegistryPrototype;
    TypeDef.PrototypeProvider := nil;
    TypeDef.StaticSource := nil;
    TypeDef.PrototypeParent := ObjectConstructor.Prototype;
    TypeDef.AddSpeciesGetter := False;
    RegisterTypeDefinition(FGlobalScope, TypeDef, SpeciesGetter, GenericConstructor);
    FinalizationRegistryConstructor :=
      TGocciaFinalizationRegistryClassValue(GenericConstructor);

    ArrayBufferConstructor := TGocciaArrayBufferClassValue.Create(CONSTRUCTOR_ARRAY_BUFFER, nil);
    Roots.Add(ArrayBufferConstructor);
    TGocciaArrayBufferValue.ExposePrototype(ArrayBufferConstructor);
    ArrayBufferConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
    ArrayBufferConstructor.DefineSymbolProperty(
      TGocciaSymbolValue.WellKnownSpecies,
      TGocciaPropertyDescriptorAccessor.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(
          SpeciesGetter, 'get [Symbol.species]', 0),
        nil, [pfConfigurable]));
    if Assigned(FBuiltinArrayBuffer) then
      for Key in FBuiltinArrayBuffer.BuiltinObject.GetAllPropertyNames do
        ArrayBufferConstructor.DefineProperty(Key,
          TGocciaPropertyDescriptorData.Create(
            FBuiltinArrayBuffer.BuiltinObject.GetProperty(Key),
            [pfConfigurable, pfWritable]));
    FGlobalScope.DefineLexicalBinding(CONSTRUCTOR_ARRAY_BUFFER, ArrayBufferConstructor, dtConst, True);

    SharedArrayBufferConstructor := TGocciaSharedArrayBufferClassValue.Create(CONSTRUCTOR_SHARED_ARRAY_BUFFER, nil);
    Roots.Add(SharedArrayBufferConstructor);
    TGocciaSharedArrayBufferValue.ExposePrototype(SharedArrayBufferConstructor);
    SharedArrayBufferConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
    SharedArrayBufferConstructor.DefineSymbolProperty(
      TGocciaSymbolValue.WellKnownSpecies,
      TGocciaPropertyDescriptorAccessor.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(
          SpeciesGetter, 'get [Symbol.species]', 0),
        nil, [pfConfigurable]));
    FGlobalScope.DefineLexicalBinding(CONSTRUCTOR_SHARED_ARRAY_BUFFER, SharedArrayBufferConstructor, dtConst, True);

    DataViewConstructor := TGocciaDataViewClassValue.Create(CONSTRUCTOR_DATA_VIEW, nil);
    Roots.Add(DataViewConstructor);
    TGocciaDataViewValue.ExposePrototype(DataViewConstructor);
    DataViewConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
    FGlobalScope.DefineLexicalBinding(CONSTRUCTOR_DATA_VIEW, DataViewConstructor, dtConst, True);

    // Create %TypedArray% intrinsic (not globally exposed per spec §23.2.1)
    FTypedArrayIntrinsic := TGocciaTypedArrayIntrinsicClassValue.Create('TypedArray', nil);
    Roots.Add(FTypedArrayIntrinsic);
    // §17: built-in name/length are {writable: false, enumerable: false, configurable: true}
    FTypedArrayIntrinsic.DefineProperty(PROP_NAME,
      TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create('TypedArray'), [pfConfigurable]));
    FTypedArrayIntrinsic.DefineProperty(PROP_LENGTH,
      TGocciaPropertyDescriptorData.Create(TGocciaNumberLiteralValue.Create(0), [pfConfigurable]));
    TypedArrayStatic := TGocciaTypedArrayStaticFrom.Create;
    FLazyThunks.Add(TypedArrayStatic);
    FTypedArrayIntrinsic.DefineProperty(PROP_FROM,
      TGocciaPropertyDescriptorData.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(
          TypedArrayStatic.TypedArrayFrom, PROP_FROM, 1),
        [pfConfigurable, pfWritable]));
    FTypedArrayIntrinsic.DefineProperty(PROP_OF,
      TGocciaPropertyDescriptorData.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(
          TypedArrayStatic.TypedArrayOf, PROP_OF, 0),
        [pfConfigurable, pfWritable]));
    FTypedArrayIntrinsic.DefineSymbolProperty(
      TGocciaSymbolValue.WellKnownSpecies,
      TGocciaPropertyDescriptorAccessor.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(
          SpeciesGetter, 'get [Symbol.species]', 0),
        nil, [pfConfigurable]));

    RegisterTypedArrayConstructor(CONSTRUCTOR_INT8_ARRAY, takInt8, ObjectConstructor);
    RegisterTypedArrayConstructor(CONSTRUCTOR_UINT8_ARRAY, takUint8, ObjectConstructor);
    RegisterTypedArrayConstructor(CONSTRUCTOR_UINT8_CLAMPED_ARRAY, takUint8Clamped, ObjectConstructor);
    RegisterTypedArrayConstructor(CONSTRUCTOR_INT16_ARRAY, takInt16, ObjectConstructor);
    RegisterTypedArrayConstructor(CONSTRUCTOR_UINT16_ARRAY, takUint16, ObjectConstructor);
    RegisterTypedArrayConstructor(CONSTRUCTOR_INT32_ARRAY, takInt32, ObjectConstructor);
    RegisterTypedArrayConstructor(CONSTRUCTOR_UINT32_ARRAY, takUint32, ObjectConstructor);
    RegisterTypedArrayConstructor(CONSTRUCTOR_FLOAT16_ARRAY, takFloat16, ObjectConstructor);
    RegisterTypedArrayConstructor(CONSTRUCTOR_FLOAT32_ARRAY, takFloat32, ObjectConstructor);
    RegisterTypedArrayConstructor(CONSTRUCTOR_FLOAT64_ARRAY, takFloat64, ObjectConstructor);
    RegisterTypedArrayConstructor(CONSTRUCTOR_BIGINT64_ARRAY, takBigInt64, ObjectConstructor);
    RegisterTypedArrayConstructor(CONSTRUCTOR_BIGUINT64_ARRAY, takBigUint64, ObjectConstructor);

    // Wire %TypedArray%.prototype to the shared prototype (which has all methods)
    if TGocciaTypedArrayValue.GetSharedPrototypeObject <> nil then
    begin
      FTypedArrayIntrinsic.ReplacePrototype(TGocciaTypedArrayValue.GetSharedPrototypeObject);
      // §23.2.3: %TypedArray%.prototype.constructor = %TypedArray%
      TGocciaTypedArrayValue.GetSharedPrototypeObject.DefineProperty(PROP_CONSTRUCTOR,
        TGocciaPropertyDescriptorData.Create(FTypedArrayIntrinsic, [pfConfigurable, pfWritable]));
      // §23.2.3.33: %TypedArray%.prototype.toString is the same function object
      // as Array.prototype.toString.
      TGocciaTypedArrayValue.GetSharedPrototypeObject.DefineProperty(PROP_TO_STRING,
        TGocciaPropertyDescriptorData.Create(
          ArrayConstructor.Prototype.GetProperty(PROP_TO_STRING),
          [pfConfigurable, pfWritable]));
    end;

    TypeDef.ConstructorName := CONSTRUCTOR_STRING;
    TypeDef.Kind := gtdkPrimitiveWrapper;
    TypeDef.ClassValueClass := TGocciaStringClassValue;
    TypeDef.ExposePrototype := nil;
    TypeDef.PrototypeProvider := @StringPrototypeProvider;
    TypeDef.StaticSource := BuiltinObjectOrNil(FBuiltinGlobalString);
    TypeDef.PrototypeParent := ObjectConstructor.Prototype;
    TypeDef.AddSpeciesGetter := False;
    RegisterTypeDefinition(FGlobalScope, TypeDef, SpeciesGetter, GenericConstructor);
    StringConstructor := TGocciaStringClassValue(GenericConstructor);

    FBuiltinRegExp := TGocciaGlobalRegExp.Create(CONSTRUCTOR_REGEXP,
      FGlobalScope, ThrowError, ObjectConstructor.Prototype);

    TypeDef.ConstructorName := CONSTRUCTOR_NUMBER;
    TypeDef.Kind := gtdkPrimitiveWrapper;
    TypeDef.ClassValueClass := TGocciaNumberClassValue;
    TypeDef.ExposePrototype := nil;
    TypeDef.PrototypeProvider := @NumberPrototypeProvider;
    TypeDef.StaticSource := BuiltinObjectOrNil(FBuiltinGlobalNumber);
    TypeDef.PrototypeParent := ObjectConstructor.Prototype;
    TypeDef.AddSpeciesGetter := False;
    RegisterTypeDefinition(FGlobalScope, TypeDef, SpeciesGetter, GenericConstructor);
    NumberConstructor := TGocciaNumberClassValue(GenericConstructor);

    TypeDef.ConstructorName := CONSTRUCTOR_BOOLEAN;
    TypeDef.Kind := gtdkPrimitiveWrapper;
    TypeDef.ClassValueClass := TGocciaBooleanClassValue;
    TypeDef.ExposePrototype := nil;
    TypeDef.PrototypeProvider := @BooleanPrototypeProvider;
    TypeDef.StaticSource := nil;
    TypeDef.PrototypeParent := ObjectConstructor.Prototype;
    TypeDef.AddSpeciesGetter := False;
    RegisterTypeDefinition(FGlobalScope, TypeDef, SpeciesGetter, GenericConstructor);
    BooleanConstructor := TGocciaBooleanClassValue(GenericConstructor);

    FFunctionConstructor := TGocciaFunctionConstructorClassValue.Create('Function', nil);
    Roots.Add(FFunctionConstructor);
    FunctionConstructor := FFunctionConstructor;
    // Wire Function.prototype to be the shared function prototype (which has
    // call/apply/bind/toString) so that Object.getPrototypeOf(fn) === Function.prototype.
    // Chain: Function.prototype (= FSharedPrototype) → ObjectConstructor.Prototype
    TGocciaFunctionBase.GetSharedPrototype.Prototype := ObjectConstructor.Prototype;
    FunctionConstructor.ReplacePrototype(TGocciaFunctionBase.GetSharedPrototype);
    FunctionConstructor.Prototype.DefineProperty(PROP_CONSTRUCTOR,
      TGocciaPropertyDescriptorData.Create(FunctionConstructor, [pfConfigurable, pfWritable]));
    // Set Function.prototype as the default [[Prototype]] for all class constructors
    // (§15.7.3 step 7.a: classes without extends have [[Prototype]] = Function.prototype)
    TGocciaClassValue.SetDefaultPrototype(FunctionConstructor.Prototype);
    // Retroactively set [[Prototype]] on constructors created before FunctionConstructor
    TGocciaClassValue.PatchDefaultPrototype(ObjectConstructor);
    TGocciaClassValue.PatchDefaultPrototype(GenericConstructor);
    TGocciaClassValue.PatchDefaultPrototype(ArrayConstructor);
    TGocciaClassValue.PatchDefaultPrototype(MapConstructor);
    TGocciaClassValue.PatchDefaultPrototype(SetConstructor);
    TGocciaClassValue.PatchDefaultPrototype(WeakMapConstructor);
    TGocciaClassValue.PatchDefaultPrototype(WeakSetConstructor);
    TGocciaClassValue.PatchDefaultPrototype(WeakRefConstructor);
    TGocciaClassValue.PatchDefaultPrototype(FinalizationRegistryConstructor);
    TGocciaClassValue.PatchDefaultPrototype(ArrayBufferConstructor);
    TGocciaClassValue.PatchDefaultPrototype(SharedArrayBufferConstructor);
    TGocciaClassValue.PatchDefaultPrototype(DataViewConstructor);
    TGocciaClassValue.PatchDefaultPrototype(FTypedArrayIntrinsic);
    TGocciaClassValue.PatchDefaultPrototype(StringConstructor);
    TGocciaClassValue.PatchDefaultPrototype(NumberConstructor);
    TGocciaClassValue.PatchDefaultPrototype(BooleanConstructor);
    TGocciaClassValue.PatchDefaultPrototype(FunctionConstructor);
    FGlobalScope.DefineLexicalBinding('Function', FunctionConstructor, dtConst, True);

    // ES2026 §20.4.3: Symbol.prototype's [[Prototype]] is %Object.prototype%
    if TGocciaSymbolValue.SharedPrototype <> nil then
      TGocciaObjectValue(TGocciaSymbolValue.SharedPrototype).Prototype := ObjectConstructor.Prototype;

    RegisterGocciaScriptGlobal;
    RegisterGlobalThis;
    RegisterLazyBuiltinGlobalProperties;
  finally
    Roots.Clear;
  end;
end;

procedure TGocciaEngine.RegisterTypedArrayConstructor(const AName: string; const AKind: TGocciaTypedArrayKind; const AObjectConstructor: TGocciaClassValue);
var
  TAConstructor: TGocciaTypedArrayClassValue;
  BPE: TGocciaNumberLiteralValue;
  Encoding: TGocciaUint8ArrayEncoding;
  Roots: TGocciaActiveRootFrame;
begin
  // The constructor and the shared BYTES_PER_ELEMENT number are reachable
  // from nothing until they are stored or bound, and the steps between them
  // allocate.
  Roots.Initialize;
  try
    TAConstructor := TGocciaTypedArrayClassValue.Create(AName, FTypedArrayIntrinsic, AKind);
    Roots.Add(TAConstructor);
    TGocciaTypedArrayValue.ExposePrototype(TAConstructor);
    TGocciaTypedArrayValue.SetKindPrototype(AKind, TAConstructor.Prototype);
    TGocciaTypedArrayValue.SetSharedPrototypeParent(AObjectConstructor.Prototype);
    BPE := TGocciaNumberLiteralValue.Create(TGocciaTypedArrayValue.BytesPerElement(AKind));
    Roots.Add(BPE);
    TAConstructor.DefineProperty(PROP_BYTES_PER_ELEMENT,
      TGocciaPropertyDescriptorData.Create(BPE, []));
    TAConstructor.Prototype.DefineProperty(PROP_BYTES_PER_ELEMENT,
      TGocciaPropertyDescriptorData.Create(BPE, []));

    // Uint8Array-only: Base64/Hex encoding methods (TC39 Uint8Array Base64)
    if AKind = takUint8 then
    begin
      Encoding := TGocciaUint8ArrayEncoding.Create;
      FLazyThunks.Add(Encoding);

      // Static methods on Uint8Array constructor
      TAConstructor.DefineProperty(PROP_FROM_BASE64,
        TGocciaPropertyDescriptorData.Create(
          TGocciaNativeFunctionValue.CreateWithoutPrototype(
            Encoding.FromBase64, PROP_FROM_BASE64, 1),
          [pfConfigurable, pfWritable]));
      TAConstructor.DefineProperty(PROP_FROM_HEX,
        TGocciaPropertyDescriptorData.Create(
          TGocciaNativeFunctionValue.CreateWithoutPrototype(
            Encoding.FromHex, PROP_FROM_HEX, 1),
          [pfConfigurable, pfWritable]));

      // Prototype methods on Uint8Array.prototype
      TAConstructor.Prototype.DefineProperty(PROP_TO_BASE64,
        TGocciaPropertyDescriptorData.Create(
          TGocciaNativeFunctionValue.CreateWithoutPrototype(
            Encoding.ToBase64, PROP_TO_BASE64, 0),
          [pfConfigurable, pfWritable]));
      TAConstructor.Prototype.DefineProperty(PROP_TO_HEX,
        TGocciaPropertyDescriptorData.Create(
          TGocciaNativeFunctionValue.CreateWithoutPrototype(
            Encoding.ToHex, PROP_TO_HEX, 0),
          [pfConfigurable, pfWritable]));
      TAConstructor.Prototype.DefineProperty(PROP_SET_FROM_BASE64,
        TGocciaPropertyDescriptorData.Create(
          TGocciaNativeFunctionValue.CreateWithoutPrototype(
            Encoding.SetFromBase64, PROP_SET_FROM_BASE64, 1),
          [pfConfigurable, pfWritable]));
      TAConstructor.Prototype.DefineProperty(PROP_SET_FROM_HEX,
        TGocciaPropertyDescriptorData.Create(
          TGocciaNativeFunctionValue.CreateWithoutPrototype(
            Encoding.SetFromHex, PROP_SET_FROM_HEX, 1),
          [pfConfigurable, pfWritable]));

      // Uint8Array instances use constructor's prototype (encoding methods + shared chain)
      TGocciaTypedArrayValue.SetUint8Prototype(TAConstructor.Prototype);
    end;

    FGlobalScope.DefineLexicalBinding(AName, TAConstructor, dtConst, True);
  finally
    Roots.Clear;
  end;
end;

procedure TGocciaEngine.RegisterGlobalThis;
var
  GlobalThisObj: TGocciaObjectValue;
  Scope: TGocciaScope;
  Name: string;
  Flags: TPropertyFlags;
begin
  Scope := FGlobalScope;
  if Scope.ContainsOwnLexicalBinding(PROP_GLOBAL_THIS) and
     (Scope.GetValue(PROP_GLOBAL_THIS) is TGocciaObjectValue) then
    GlobalThisObj := TGocciaObjectValue(Scope.GetValue(PROP_GLOBAL_THIS))
  else
    GlobalThisObj := TGocciaObjectValue.Create;

  if (not Assigned(GlobalThisObj.Prototype)) and
     (TGocciaObjectValue.SharedObjectPrototype <> nil) then
    GlobalThisObj.Prototype := TGocciaObjectValue.SharedObjectPrototype;

  for Name in Scope.GetOwnBindingNames do
  begin
    // Internal intrinsic-capture bindings (e.g. __GocciaIntlNumberFormat) must
    // never be mirrored onto globalThis; they are engine-private and exist only
    // for the locale shims.
    if (Length(Name) >= 8) and (Copy(Name, 1, 8) = '__Goccia') then
      Continue;
    // A built-in binding becomes GlobalObjectBacked only after it has already
    // been mirrored onto the global object below, so skipping backed *built-in*
    // bindings makes RefreshGlobalThis (re-run on every runtime-extension
    // install) cost O(new bindings) instead of O(all globals).  It is also
    // load-bearing for lazy globals: re-reading their value here through
    // Scope.GetValue would resolve the global-object property and force
    // materialization, defeating the lazy descriptor installed by
    // RegisterLazyGlobal / lazy built-ins.  Host-injected globals (BuiltIn =
    // False) are NOT skipped: RegisterGlobal can replace them via
    // ForceUpdateBinding, and the mirrored globalThis property must be refreshed
    // to the new value.
    if Scope.IsGlobalObjectBackedBinding(Name) and Scope.IsBuiltInBinding(Name) then
      Continue;
    // ES2026 §19.1: Value properties (NaN, Infinity, undefined) are
    // { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    // All other global object properties are
    // { [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true }.
    if (Name = 'NaN') or (Name = 'Infinity') or (Name = 'undefined') then
      Flags := []
    else
      Flags := [pfWritable, pfConfigurable];
    GlobalThisObj.DefineProperty(Name,
      TGocciaPropertyDescriptorData.Create(Scope.GetValue(Name), Flags));
    Scope.MarkGlobalObjectBackedBinding(Name);
  end;

  GlobalThisObj.DefineProperty(PROP_GLOBAL_THIS,
    TGocciaPropertyDescriptorData.Create(GlobalThisObj, [pfWritable, pfConfigurable]));
  if Scope.ContainsOwnLexicalBinding(PROP_GLOBAL_THIS) then
    Scope.ForceUpdateBinding(PROP_GLOBAL_THIS, GlobalThisObj)
  else
    Scope.DefineLexicalBinding(PROP_GLOBAL_THIS, GlobalThisObj, dtConst, True);
  Scope.MarkGlobalObjectBackedBinding(PROP_GLOBAL_THIS);

  // ES2026 §9.1.2.5 NewGlobalEnvironment: a global Environment Record's
  // [[GlobalThisValue]] is the global object. Top-level `this` resolves
  // here via §9.4.3 ResolveThisBinding -> GlobalEnvironmentRecord.GetThisBinding.
  Scope.ThisValue := GlobalThisObj;
  FRealm.GlobalObject := GlobalThisObj;
end;

procedure TGocciaEngine.RefreshGlobalThis;
begin
  RegisterGlobalThis;
end;

procedure TGocciaEngine.SuspendRealmExecutionContext;
begin
  if Assigned(FRealmExecutionContext) then
    FRealmExecutionContext.Pop;
end;

function TGocciaEngine.ActivateRealmExecutionContext:
  TGocciaExecutionContextScope;
begin
  // Switching into this engine's realm (ShadowRealm evaluate/importValue/wrapped
  // function) also makes this engine's diagnostic scope the active capture
  // target, so a code frame captured while this engine runs comes from its own
  // source — never the caller engine's — and is restored when the scope pops.
  Result := TGocciaExecutionContextScope.Create(
    CreateExecutionContext(FRealm, FGlobalScope, FSourcePath),
    FModuleLoader.DiagnosticScope);
end;

procedure TGocciaEngine.RegisterGocciaScriptGlobal;
var
  GocciaObj: TGocciaObjectValue;
  BuildObj: TGocciaObjectValue;
  RuntimeGlobalsArray: TGocciaArrayValue;
  ShimsArray: TGocciaArrayValue;
  GCFunc: TGocciaNativeFunctionValue;
  I: Integer;
  RuntimeGlobalsRoot: TGocciaTempRoot;
  BuildRoot: TGocciaTempRoot;
  ShimsRoot: TGocciaTempRoot;
  GocciaRoot: TGocciaTempRoot;
begin
  { Registration runs before the Goccia object reaches the global scope, and
    each version/os/shim string is charged against the memory ceiling — a GC
    safe point under a tight --max-memory — so the objects under construction
    need temp roots. }
  InitializeTempRoot(RuntimeGlobalsRoot);
  InitializeTempRoot(BuildRoot);
  InitializeTempRoot(ShimsRoot);
  InitializeTempRoot(GocciaRoot);
  try
  RuntimeGlobalsArray := TGocciaArrayValue.Create;
  AddTempRootIfNeeded(RuntimeGlobalsRoot, RuntimeGlobalsArray);

  BuildObj := TGocciaObjectValue.Create;
  AddTempRootIfNeeded(BuildRoot, BuildObj);
  BuildObj.DefineProperty('os', TGocciaPropertyDescriptorData.Create(
    TGocciaStringLiteralValue.Create(GetBuildOS), [pfEnumerable]));
  BuildObj.DefineProperty('arch', TGocciaPropertyDescriptorData.Create(
    TGocciaStringLiteralValue.Create(GetBuildArch), [pfEnumerable]));
  BuildObj.DefineProperty('date', TGocciaPropertyDescriptorData.Create(
    TGocciaStringLiteralValue.Create(GetBuildDate), [pfEnumerable]));
  BuildObj.DefineProperty('gcStress', TGocciaPropertyDescriptorData.Create(
    TGocciaBooleanLiteralValue.FromBoolean(IsGCStressBuild), [pfEnumerable]));

  ShimsArray := TGocciaArrayValue.Create;
  AddTempRootIfNeeded(ShimsRoot, ShimsArray);
  for I := 0 to FShims.Count - 1 do
    ShimsArray.Elements.Add(TGocciaStringLiteralValue.Create(FShims[I]));

  GocciaObj := TGocciaObjectValue.Create;
  AddTempRootIfNeeded(GocciaRoot, GocciaObj);
  GocciaObj.AssignProperty('version', TGocciaStringLiteralValue.Create(GetVersion));
  GocciaObj.AssignProperty('commit', TGocciaStringLiteralValue.Create(GetCommit));
  GocciaObj.AssignProperty(PROP_RUNTIME_GLOBALS, RuntimeGlobalsArray);
  GocciaObj.AssignProperty('build', BuildObj);
  GocciaObj.DefineProperty('spec', TGocciaPropertyDescriptorData.Create(
    CreateSpecObject, [pfEnumerable]));
  GocciaObj.DefineProperty('proposal', TGocciaPropertyDescriptorData.Create(
    CreateProposalObject, [pfEnumerable]));
  GocciaObj.DefineProperty('shims', TGocciaPropertyDescriptorData.Create(
    ShimsArray, [pfEnumerable]));
  GCFunc := TGocciaNativeFunctionValue.CreateWithoutPrototype(GocciaGC, 'gc', 0);
  GCFunc.DefineProperty('maxBytes',
    TGocciaPropertyDescriptorAccessor.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(GocciaGCMaxBytesGetter, 'get maxBytes', 0),
      nil,
      []));
  GCFunc.DefineProperty('suggestedMaxBytes',
    TGocciaPropertyDescriptorAccessor.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(GocciaGCSuggestedMaxBytesGetter, 'get suggestedMaxBytes', 0),
      nil,
      []));
  GCFunc.DefineProperty('bytesAllocated',
    TGocciaPropertyDescriptorAccessor.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(GocciaGCBytesAllocatedGetter, 'get bytesAllocated', 0),
      nil,
      []));
  GocciaObj.AssignProperty('gc', GCFunc);

  FGocciaGlobal := GocciaObj;
  FGlobalScope.DefineLexicalBinding(PROP_GOCCIA, FGocciaGlobal, dtConst, True);
  finally
    RemoveTempRootIfNeeded(GocciaRoot);
    RemoveTempRootIfNeeded(ShimsRoot);
    RemoveTempRootIfNeeded(BuildRoot);
    RemoveTempRootIfNeeded(RuntimeGlobalsRoot);
  end;
end;

function TGocciaEngine.GetResolver: TGocciaModuleResolver;
begin
  Result := FModuleLoader.Resolver;
end;

procedure TGocciaEngine.AddAlias(const APattern, AReplacement: string);
begin
  Resolver.AddAlias(APattern, AReplacement);
end;

procedure TGocciaEngine.EmitCapabilityAudit(
  const AKind: TGocciaCapabilityKind;
  const ADecision: TGocciaCapabilityDecision;
  const ASubject, AReason: string);
var
  CallSite: TGocciaCallSite;
  Source: TGocciaCapabilityAuditSource;
begin
  if not Assigned(FCapabilityAuditSink) then
    Exit;
  Source := Default(TGocciaCapabilityAuditSource);
  if CurrentGocciaCallSite(CallSite) then
  begin
    Source.FilePath := CallSite.FilePath;
    Source.Line := CallSite.Line;
    Source.Column := CallSite.Column;
  end;
  EmitCapabilityAuditAt(AKind, ADecision, ASubject, AReason, Source);
end;

procedure TGocciaEngine.EmitCapabilityAuditAt(
  const AKind: TGocciaCapabilityKind;
  const ADecision: TGocciaCapabilityDecision;
  const ASubject, AReason: string;
  const ASource: TGocciaCapabilityAuditSource);
var
  AuditEvent: TGocciaCapabilityAuditEvent;
begin
  if not Assigned(FCapabilityAuditSink) then
    Exit;
  if AKind <> gckCapabilitiesEffective then
    AuditEffectiveCapabilities;

  AuditEvent.Kind := AKind;
  AuditEvent.Decision := ADecision;
  AuditEvent.Subject := ASubject;
  AuditEvent.Reason := AReason;
  AuditEvent.Source := ASource;
  if AuditEvent.Source.FilePath = '' then
  begin
    AuditEvent.Source.FilePath := FSourcePath;
    AuditEvent.Source.Line := 0;
    AuditEvent.Source.Column := 0;
  end;

  try
    FCapabilityAuditSink(AuditEvent);
  except
    on E: EGocciaCapabilityAuditDeliveryError do
      raise;
    on E: Exception do
      raise EGocciaCapabilityAuditDeliveryError.Create(
        'capability audit delivery failed: ' + E.Message);
  end;
end;

procedure TGocciaEngine.ConfigureCapabilityAuditAsChildOf(
  const AParent: TGocciaEngine);
begin
  FIsCapabilityChild := True;
  if Assigned(AParent) then
    FCapabilityAuditSink := AParent.FCapabilityAuditSink
  else
    FCapabilityAuditSink := nil;
end;

function TGocciaEngine.ExtensionsHavePendingWork: Boolean;
var
  I: Integer;
begin
  for I := 0 to FExtensions.Count - 1 do
    if FExtensions[I].HasPendingWork then
      Exit(True);
  Result := False;
end;

{ The promise wait runs outside the idle wait, so it offers no pause
  checkpoint: with a checkpoint handler installed, the host-call owner's work
  is left for the idle wait, which offers one before it. }
function TGocciaEngine.RunExtensionWorkForPromiseWait: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FExtensions.Count - 1 do
  begin
    if (FExtensions[I] = FHostCallOwner) and Assigned(FOnPauseCheckpoint) then
      Continue;
    if FExtensions[I].RunPendingWorkForPromiseWait then
      Result := True;
  end;
end;

{ GPromiseWaitHostWork: the innermost running engine's extension work. }
function RunRunningEngineWorkForPromiseWait: Boolean;
begin
  Result := Assigned(GRunningEngine) and
    GRunningEngine.RunExtensionWorkForPromiseWait;
end;

{ Runs the engine to idle: no host work in flight, no job queued. A job can
  start host work after that extension's pass: in
  `await Promise.resolve(); await fetch(url)` at the top of a module, the
  fetch runs in the job that resumes the module. One round left that request
  in flight and the run ended without it, so rounds repeat until one leaves
  no extension with work. }
function TGocciaEngine.SnapshotIdleRefusal: string;
var
  I: Integer;
  Queue: TGocciaMicrotaskQueue;
  Reason: string;
begin
  if Assigned(FVM) and not FVM.IsIdle then
    Exit('a bytecode frame is running');
  Queue := TGocciaMicrotaskQueue.Instance;
  if Assigned(Queue) and Queue.HasPending then
    Exit('a job is queued');
  // This engine's waiters, settled or not; another engine's on the thread
  // are its own.
  if HasAtomicsWaitAsyncWaiters(FBuiltinAtomics) then
    Exit('an Atomics.waitAsync waiter is pending');
  for I := 0 to FExtensions.Count - 1 do
    if (FExtensions[I] <> FHostCallOwner) and FExtensions[I].HasPendingWork then
      Exit('an extension other than the host-call owner has pending work');
  // State outside the heap no codec covers (ADR 0148).
  Reason := HeapSnapshotRefusal;
  if Reason <> '' then
    Exit('state outside the heap no codec covers: ' + Reason);
  Result := '';
end;

function TGocciaEngine.IsAtPauseCheckpoint: Boolean;
begin
  Result := Assigned(FHostCallOwner) and (SnapshotIdleRefusal = '') and
    FHostCallOwner.HasPendingWork;
end;

function TGocciaEngine.HeapSnapshotRefusal: string;
var
  I: Integer;
begin
  for I := 0 to FExtensions.Count - 1 do
  begin
    Result := FExtensions[I].HeapSnapshotRefusal;
    if Result <> '' then
      Exit;
  end;
  Result := '';
end;

procedure TGocciaEngine.MaterializeHeapSnapshotBuiltin(const AName: string);
var
  I: Integer;
begin
  // A sorted manifest can name the builtin before the global whose lazy
  // property normally creates it. These factories contain only native setup.
  if AName = 'Temporal' then
    MaterializeTemporalGlobal
  else if AName = 'Intl' then
    MaterializeIntlGlobal
  else if AName = CONSTRUCTOR_ATOMICS then
    MaterializeAtomicsGlobal
  else if AName = 'Reflect' then
    MaterializeReflectGlobal
  else if AName = CONSTRUCTOR_DISPOSABLE_STACK then
    MaterializeDisposableStackGlobal;
  for I := 0 to FExtensions.Count - 1 do
    FExtensions[I].MaterializeHeapSnapshotBuiltin(AName);
end;

procedure TGocciaEngine.WaitForRuntimeIdle;
var
  I: Integer;
  Queue: TGocciaMicrotaskQueue;
  GC: TGarbageCollector;
  Repeating, Pause: Boolean;
  FloatingPointState: TGocciaFloatingPointState;
begin
  Repeating := False;
  { The jobs this runs are script, and an embedder can call it outside a run:
    establish the floating-point mode as the run entries do. }
  EnterGocciaFloatingPointScope(FloatingPointState);
  try
    repeat
      { A further round waits again: the deadline and a host's stop end it. }
      if Repeating then
      begin
        CheckExecutionTimeoutNow;
        CheckInstructionLimit;
      end;
      Repeating := True;
      for I := 0 to FExtensions.Count - 1 do
      begin
        if (FExtensions[I] = FHostCallOwner) and
           Assigned(FOnPauseCheckpoint) and IsAtPauseCheckpoint then
        begin
          { No job runs at a checkpoint, so the objects WeakRef and
            FinalizationRegistry kept alive for the current job are let go
            here (ES2026 §9.10 ClearKeptObjects), as the pass below would.
            Kept, they are state a snapshot would not hold. }
          GC := TGarbageCollector.Instance;
          if Assigned(GC) then
            GC.ClearKeptObjects;
          Pause := False;
          FOnPauseCheckpoint(Self, Pause);
          if Pause then
            raise EGocciaRunPaused.Create('the run paused at a checkpoint');
        end;
        FExtensions[I].WaitForIdle;
      end;
      GC := TGarbageCollector.Instance;
      if Assigned(GC) then
        GC.ClearKeptObjects;
      Queue := TGocciaMicrotaskQueue.Instance;
      repeat
        if PumpAtomicsWaitAsyncCompletions = 0 then
        begin
          if Assigned(Queue) and Queue.HasPending then
            Queue.DrainQueue
          else if ExtensionsHavePendingWork then
            { A job started host work after its extension's pass: the next
              round waits on it before this loop sleeps for an Atomics
              timeout. The fetch wait settles a waiter that falls due
              meanwhile. }
            Break
          else if HasSelfSettlingAtomicsWaitAsyncWaiters(FBuiltinAtomics) then
          begin
            { An Atomics.waitAsync timeout still to expire. The deadline and
              a host's stop end the wait, as in every other engine wait. }
            CheckExecutionTimeoutNow;
            CheckInstructionLimit;
            Sleep(1);
          end
          else
            Break;
        end
        else if Assigned(Queue) and Queue.HasPending then
          Queue.DrainQueue;
      until False;
    until not ExtensionsHavePendingWork;
  finally
    GC := TGarbageCollector.Instance;
    if Assigned(GC) then
      GC.ClearKeptObjects;
    LeaveGocciaFloatingPointScope(FloatingPointState);
  end;
end;

{ Called once a run has nothing left to do. A promise that was rejected and
  still has no handler will never get one from this run, so the rejection is
  the run's failure, raised like an uncaught throw. }
procedure TGocciaEngine.RaiseUnhandledRejection;
var
  Queue: TGocciaMicrotaskQueue;
  Promise: TGocciaValue;
begin
  Queue := TGocciaMicrotaskQueue.Instance;
  { A run the hook started on this engine is still inside the report: the
    promise being reported is tracked, and would be reported or raised again. }
  if (not Assigned(Queue)) or FReportingUnhandledRejections then
    Exit;
  if Assigned(FOnUnhandledRejection) then
  begin
    NotifyUnhandledRejections;
    { Reported. Left tracked, they would be reported again at this engine's
      next idle point. }
    if FUnhandledRejections = urIgnore then
      Queue.DiscardUnhandledRejections;
  end;
  if FUnhandledRejections <> urThrow then
    Exit;
  if not Queue.TakeUnhandledRejection(Promise) then
    Exit;
  raise TGocciaThrowValue.Create(TGocciaPromiseValue(Promise).PromiseResult);
end;

procedure TGocciaEngine.NotifyUnhandledRejections;
var
  I: Integer;
  Promises: TArray<TGocciaValue>;
  Queue: TGocciaMicrotaskQueue;
begin
  Queue := TGocciaMicrotaskQueue.Instance;
  Promises := Queue.UnhandledRejectionsInOrder;
  FReportingUnhandledRejections := True;
  try
    for I := 0 to High(Promises) do
    begin
      { An earlier call can have given this promise a handler, and then the
        queue no longer keeps it alive either. A tracked promise is alive, and
        nothing here touches it once the hook has it: temp roots are a set,
        so a root added here would be taken from whoever else holds one. }
      if not Queue.IsRejectionTracked(Promises[I]) then
        Continue;
      FOnUnhandledRejection(Promises[I],
        TGocciaPromiseValue(Promises[I]).PromiseResult);
    end;
  finally
    FReportingUnhandledRejections := False;
  end;
end;

{ A run that fails has reported itself by failing. The jobs it left queued,
  the promises it left rejected and its pending fetches go with it: left on
  the thread, the next run would execute those jobs against this engine's
  state, which a host has usually freed by then, and be blamed for those
  rejections. Execute gets this from clearing its queue or leaving its scope;
  ExecuteProgram and RunModule call this when they exit by exception.
  Only the outermost run does: a failure inside another run may be caught by
  the script that is still running, and its own pending work is still its. }
procedure TGocciaEngine.ClearPendingWorkOfFailedRun(
  const AOuterRunningEngine: TGocciaEngine);
begin
  if Assigned(AOuterRunningEngine) then
    Exit;
  if Assigned(TGocciaMicrotaskQueue.Instance) then
    TGocciaMicrotaskQueue.Instance.ClearQueue;
  DiscardRuntimePending;
end;

{ Compile AProgram for this engine: its strict-types setting, `arguments`
  compatibility and coverage shape, with its top-level declarations bound in
  the global environment when GlobalBackedTopLevel is set, which first checks
  them against the global environment's existing bindings. }
function TGocciaEngine.CompileProgram(const AProgram: TGocciaProgram;
  const ANonStrictMode: Boolean): TGocciaBytecodeModule;
var
  Compiler: TGocciaCompiler;
  Options: TGocciaCompilerOptimizationOptions;
begin
  if FGlobalBackedTopLevel and Assigned(FGlobalScope) then
    CheckTopLevelRedeclarations(AProgram, FGlobalScope, FSourcePath);

  Compiler := TGocciaCompiler.Create(FSourcePath);
  try
    Compiler.GlobalBackedTopLevel := FGlobalBackedTopLevel;
    Compiler.StrictTypes := FStrictTypes;
    Compiler.NonStrictMode := ANonStrictMode;
    Compiler.ArgumentsObjectEnabled := cfArgumentsObject in FCompatibility;
    Options := Compiler.OptimizationOptions;
    Options.PreserveCoverageShape :=
      (TGocciaCoverageTracker.Instance <> nil) and
      TGocciaCoverageTracker.Instance.Enabled;
    Compiler.OptimizationOptions := Options;
    Result := Compiler.Compile(AProgram);
    if FSourceType = stModule then
      Result.CompileProfile := BuildBytecodeCompileProfile(SourcePipelineOptions(stModule), FStrictTypes)
    else
      Result.CompileProfile := BuildBytecodeCompileProfile(SourcePipelineOptions(stScript), FStrictTypes);
  finally
    Compiler.Free;
  end;
end;

{ ES2026 §16.2.1.7.3.1 InitializeEnvironment, the function declarations of a
  module: compile the whole module now, while it is linked, and instantiate
  each top-level function declaration from its compiled template in the
  module environment, so the functions exist, as compiled closures, before
  any module body runs (a module in an import cycle can call them first).
  The body is compiled with them and kept in AContext.CompiledModule for
  EvaluateModuleBody, which reads each function from the environment instead
  of creating it again. }
procedure TGocciaEngine.InstantiateModuleFunctions(
  const AProgram: TGocciaProgram; var AContext: TGocciaEvaluationContext);
var
  BytecodeModule: TGocciaBytecodeModule;
begin
  if AContext.CompiledModule is TGocciaBytecodeModule then
  begin
    ValidateImportedBytecode(TGocciaBytecodeModule(AContext.CompiledModule));
    InstantiateCompiledModuleFunctions(TGocciaBytecodeModule(AContext.CompiledModule),
      AContext.Scope, AContext.Realm);
    Exit;
  end;
  BytecodeModule := CompileModuleProgram(AProgram, AContext.CurrentFilePath,
    AContext.NonStrictMode, AContext.Scope.EffectiveArgumentsObjectEnabled);
  AContext.CompiledModule := BytecodeModule;
  InstantiateCompiledModuleFunctions(BytecodeModule, AContext.Scope,
    AContext.Realm);
end;

{ A module-form program compiled for linking, and kept for the engine's
  life. }
function TGocciaEngine.CompileModuleProgram(const AProgram: TGocciaProgram;
  const AFilePath: string; const ANonStrictMode,
  AArgumentsObjectEnabled: Boolean): TGocciaBytecodeModule;
var
  Compiler: TGocciaCompiler;
  Options: TGocciaCompilerOptimizationOptions;
begin
  Compiler := TGocciaCompiler.Create(AFilePath);
  try
    // The module's top-level bindings live in its Module Environment
    // Record, so the module body and its functions read them there by name:
    // a function instantiated now observes the body's later let/const
    // initialization and writes.
    Compiler.GlobalBackedTopLevel := True;
    Compiler.AsyncTopLevel := AProgram.HasTopLevelAwait;
    Compiler.PreinitializedTopLevelFunctions := True;
    Compiler.StrictTypes := FStrictTypes;
    Compiler.NonStrictMode := ANonStrictMode;
    Compiler.ArgumentsObjectEnabled := AArgumentsObjectEnabled;
    Options := Compiler.OptimizationOptions;
    Options.PreserveCoverageShape :=
      (TGocciaCoverageTracker.Instance <> nil) and
      TGocciaCoverageTracker.Instance.Enabled;
    Compiler.OptimizationOptions := Options;
    Result := Compiler.Compile(AProgram);
    Result.CompileProfile := BuildBytecodeCompileProfile(
      SourcePipelineOptions(stModule), FStrictTypes);
  finally
    Compiler.Free;
  end;
  FRetainedModules.Add(Result);
end;

{ ADR 0148: a restore compiles each source module a snapshot names as linking
  it compiled it, a module's code being strict and its arguments object the
  loader's setting, or the engine's for its own source. The snapshot names
  the bytecode by its hash, so a module compiled differently is refused. }
function TGocciaEngine.CompileModuleForSnapshot(const ASourcePath: string;
  const AIsHostOwned, AAsCompiledEntry: Boolean; out AHasSource: Boolean;
  out ASource: TGocciaModuleSnapshotSource): TGocciaBytecodeModule;
var
  ParseResult: TGocciaSourcePipelineResult;
  ProgramNode: TGocciaProgram;
  SavedSourceType: TGocciaSourceType;
begin
  AHasSource := False;
  ASource := Default(TGocciaModuleSnapshotSource);
  if AAsCompiledEntry then
  begin
    if ASourcePath <> FSourcePath then
      raise Exception.Create('an entry module this engine was not made with');
    ParseResult := TGocciaSourcePipeline.Parse(FSourceLines, FSourcePath,
      SourcePipelineOptions(stModule));
    // The record names a Module, regardless of the host's next parse goal.
    SavedSourceType := FSourceType;
    FSourceType := stModule;
    try
      Result := CompileModule(ParseResult.ProgramNode);
    finally
      FSourceType := SavedSourceType;
      ParseResult.Free;
    end;
    Exit;
  end;
  if ASourcePath = FSourcePath then
  begin
    ParseResult := TGocciaSourcePipeline.Parse(FSourceLines, FSourcePath,
      SourcePipelineOptions(stModule));
    try
      Result := CompileModuleProgram(ParseResult.ProgramNode, FSourcePath,
        False, cfArgumentsObject in FCompatibility);
    finally
      ParseResult.Free;
    end;
    Exit;
  end;
  ProgramNode := FModuleLoader.ParseModuleForSnapshot(ASourcePath,
    AIsHostOwned, ASource);
  try
    Result := CompileModuleProgram(ProgramNode, ASourcePath, False,
      FModuleLoader.ArgumentsObjectEnabled);
  finally
    ProgramNode.Free;
  end;
  AHasSource := True;
end;

{ Create each top-level function declaration of AModule, compiled for
  linking, from its template in the module environment AScope. }
procedure TGocciaEngine.InstantiateCompiledModuleFunctions(
  const AModule: TGocciaBytecodeModule; const AScope: TGocciaScope;
  const ARealm: TGocciaRealm);
var
  GC: TGarbageCollector;
  I: Integer;
  SavedRealm: TGocciaRealm;
  Template: TGocciaFunctionTemplate;
  Value: TGocciaValue;
begin
  GC := TGarbageCollector.Instance;
  SavedRealm := FVM.Realm;
  if Assigned(ARealm) then
    FVM.Realm := ARealm;
  try
    for I := 0 to AModule.TopLevel.FunctionCount - 1 do
    begin
      Template := AModule.TopLevel.GetFunction(I);
      if Template.ModuleEnvironmentBinding = '' then
        Continue;
      Value := FVM.InstantiateModuleFunction(Template, AScope);
      if Assigned(GC) then
        GC.AddTempRoot(Value);
      try
        if AScope.ContainsOwnLexicalBinding(
           Template.ModuleEnvironmentBinding) then
          AScope.ForceUpdateBinding(Template.ModuleEnvironmentBinding,
            Value)
        else
          AScope.DefineLexicalBinding(
            Template.ModuleEnvironmentBinding, Value, dtLet);
      finally
        if Assigned(GC) then
          GC.RemoveTempRoot(Value);
      end;
    end;
  finally
    FVM.Realm := SavedRealm;
  end;
end;

{ A bootstrap shim runs as compiled bytecode, so every function it creates is
  a VM closure (#873, #911). The shim is a module: it is compiled the way the
  loader compiles one while linking it, with its top-level bindings read by
  name from its module environment AScope, where Goccia.Shims reads the
  export back. The language profile is the shim's own, not the engine's: a
  shim is strict module code that may use `arguments` (AScope enables it),
  and it is never compiled with strict types or for coverage. Its functions
  hide their source text, as built-in functions do, and the coverage tracker
  ignores AFileName. AProgram is shared by every engine on the thread, so
  compiling and running it must not change it. The shim can be materialized
  while the VM is running user code (a lazy global read), so the VM state the
  module run changes is restored afterwards, as EvaluateModuleBody does. }
// Every template of a shim's code is ShimCode (TGocciaVM.BeginShimLoad).
procedure MarkShimCode(const ATemplate: TGocciaFunctionTemplate);
var
  Pending: TList<TGocciaFunctionTemplate>;
  Template: TGocciaFunctionTemplate;
  I: Integer;
begin
  Pending := TList<TGocciaFunctionTemplate>.Create;
  try
    Pending.Add(ATemplate);
    while Pending.Count > 0 do
    begin
      Template := Pending[Pending.Count - 1];
      Pending.Delete(Pending.Count - 1);
      Template.ShimCode := True;
      for I := 0 to Template.FunctionCount - 1 do
        Pending.Add(Template.GetFunction(I));
    end;
  finally
    Pending.Free;
  end;
end;

function TGocciaEngine.EvaluateShim(const AProgram: TGocciaProgram;
  const AScope: TGocciaScope; const AFileName: string): Boolean;
var
  ShimLoadToken: Int64;
  BytecodeModule: TGocciaBytecodeModule;
  Compiler: TGocciaCompiler;
  PipelineOptions: TGocciaSourcePipelineOptions;
  GC: TGarbageCollector;
  SavedGlobalBackedTopLevel: Boolean;
  SavedGlobalScope: TGocciaScope;
  SavedRealm: TGocciaRealm;
  SavedRuntimeModule: TGocciaModule;
begin
  if TGocciaCoverageTracker.Instance <> nil then
    TGocciaCoverageTracker.Instance.ExcludeFile(AFileName);
  if TGocciaCompileProfiler.Instance <> nil then
    TGocciaCompileProfiler.Instance.ExcludeFile(AFileName);
  Compiler := TGocciaCompiler.Create(AFileName);
  try
    Compiler.GlobalBackedTopLevel := True;
    Compiler.PreinitializedTopLevelFunctions := True;
    Compiler.ArgumentsObjectEnabled := AScope.EffectiveArgumentsObjectEnabled;
    Compiler.HideSourceText := True;
    BytecodeModule := Compiler.Compile(AProgram);
    PipelineOptions := ShimSourcePipelineOptions;
    if Compiler.ArgumentsObjectEnabled then
      Include(PipelineOptions.Compatibility, cfArgumentsObject);
    BytecodeModule.CompileProfile := BuildBytecodeCompileProfile(
      PipelineOptions, Compiler.StrictTypes);
  finally
    Compiler.Free;
  end;
  FRetainedModules.Add(BytecodeModule);
  MarkShimCode(BytecodeModule.TopLevel);

  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
    GC.AddTempRoot(AScope);
  SavedGlobalScope := FVM.GlobalScope;
  SavedRealm := FVM.Realm;
  SavedGlobalBackedTopLevel := FVM.GlobalBackedTopLevel;
  SavedRuntimeModule := FVM.CurrentRuntimeModule;
  ShimLoadToken := FVM.BeginShimLoad;
  try
    PredeclareModuleLexicalDeclarations(AProgram, AScope);
    InstantiateCompiledModuleFunctions(BytecodeModule, AScope, FRealm);
    FVM.GlobalScope := AScope;
    FVM.GlobalBackedTopLevel := False;
    FVM.CurrentRuntimeModule := nil;
    if Assigned(FRealm) then
      FVM.Realm := FRealm;
    FVM.ExecuteImportedModule(BytecodeModule);
  finally
    Result := FVM.EndShimLoad(ShimLoadToken);
    FVM.GlobalScope := SavedGlobalScope;
    FVM.Realm := SavedRealm;
    FVM.GlobalBackedTopLevel := SavedGlobalBackedTopLevel;
    FVM.CurrentRuntimeModule := SavedRuntimeModule;
    if Assigned(GC) then
      GC.RemoveTempRoot(AScope);
  end;
end;

function TGocciaEngine.EvaluateModuleBody(const AProgram: TGocciaProgram;
  const AContext: TGocciaEvaluationContext;
  out AProgramConsumed: Boolean): TGocciaValue;
var
  BytecodeModule: TGocciaBytecodeModule;
  InstantiationContext: TGocciaEvaluationContext;
  SavedGlobalScope: TGocciaScope;
  SavedRealm: TGocciaRealm;
  SavedGlobalBackedTopLevel: Boolean;
  SavedLoadDeferredModule: TLoadDeferredModuleCallback;
  SavedLoadModule: TLoadModuleCallback;
  SavedLoadModuleSource: TLoadModuleSourceCallback;
  SavedRuntimeModule: TGocciaModule;
  SavedResolveModuleURL: TResolveModuleURLCallback;
begin
  AProgramConsumed := False;
  if AContext.CompiledModule is TGocciaBytecodeModule then
    BytecodeModule := TGocciaBytecodeModule(AContext.CompiledModule)
  else
  begin
    { A module environment nothing initialized yet gets its functions now,
      before its body runs. }
    InstantiationContext := AContext;
    InstantiateModuleFunctions(AProgram, InstantiationContext);
    BytecodeModule := TGocciaBytecodeModule(
      InstantiationContext.CompiledModule);
  end;
  if BytecodeModule.WasLoadedFromBytecode then
    ValidateImportedBytecode(BytecodeModule);
  SavedGlobalScope := FVM.GlobalScope;
  SavedRealm := FVM.Realm;
  SavedGlobalBackedTopLevel := FVM.GlobalBackedTopLevel;
  SavedLoadModule := FVM.LoadModule;
  SavedLoadModuleSource := FVM.LoadModuleSource;
  SavedLoadDeferredModule := FVM.LoadDeferredModule;
  SavedResolveModuleURL := FVM.ResolveModuleURL;
  SavedRuntimeModule := FVM.CurrentRuntimeModule;
  FVM.GlobalScope := AContext.Scope;
  FVM.GlobalBackedTopLevel := False;
  FVM.LoadModule := AContext.LoadModule;
  FVM.LoadModuleSource := AContext.LoadModuleSource;
  FVM.LoadDeferredModule := AContext.LoadDeferredModule;
  FVM.ResolveModuleURL := AContext.ResolveModuleURL;
  FVM.CurrentRuntimeModule := AContext.CurrentModule;
  try
    if Assigned(AContext.Realm) then
      FVM.Realm := AContext.Realm;
    Result := FVM.ExecuteImportedModule(BytecodeModule);
    if AProgram.HasTopLevelAwait and Assigned(AContext.CurrentModule) and
       (Result is TGocciaPromiseValue) then
      AContext.CurrentModule.EvaluationPromise := Result;
  finally
    FVM.GlobalScope := SavedGlobalScope;
    FVM.Realm := SavedRealm;
    FVM.GlobalBackedTopLevel := SavedGlobalBackedTopLevel;
    FVM.LoadModule := SavedLoadModule;
    FVM.LoadModuleSource := SavedLoadModuleSource;
    FVM.LoadDeferredModule := SavedLoadDeferredModule;
    FVM.ResolveModuleURL := SavedResolveModuleURL;
    FVM.CurrentRuntimeModule := SavedRuntimeModule;
  end;
end;

{ The code of a Function constructor call, compiled and run to the function
  object it creates. }
function TGocciaEngine.ExecuteDynamicFunction(
  const AProgram: TGocciaProgram): TGocciaValue;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileProgram(AProgram, cfNonStrictMode in FCompatibility);
  if Module.TopLevel.FunctionCount > 0 then
    Module.TopLevel.GetFunction(0).Name := 'anonymous';
  FRetainedModules.Add(Module);
  Result := RunBytecodeModule(Module, nil);
end;

{ The VM's compiler for direct eval at a strict call site (ADR 0134 Phase
  3): AProgram compiled as a function nested at the call that eval site
  record ASite describes, with this engine's language profile. The compiled
  module is kept for the engine's life like the Function constructor's code
  (ADR 0134, decision 1). }
function TGocciaEngine.CompileDirectEvalCode(
  const AProgram: TGocciaProgram; const ASourcePath: string;
  const ASite: TGocciaDirectEvalEnvironment; const AStrictEval: Boolean;
  const ADeclaredVarNames: TUnicodeStringList): TGocciaFunctionTemplate;
var
  Compiler: TGocciaCompiler;
  Module: TGocciaBytecodeModule;
  Options: TGocciaCompilerOptimizationOptions;
begin
  Compiler := TGocciaCompiler.Create(ASourcePath);
  try
    Compiler.StrictTypes := FStrictTypes;
    Compiler.NonStrictMode := cfNonStrictMode in FCompatibility;
    Compiler.ArgumentsObjectEnabled := cfArgumentsObject in FCompatibility;
    Options := Compiler.OptimizationOptions;
    Options.PreserveCoverageShape :=
      (TGocciaCoverageTracker.Instance <> nil) and
      TGocciaCoverageTracker.Instance.Enabled;
    Compiler.OptimizationOptions := Options;
    Module := Compiler.CompileDirectEval(AProgram, ASite, AStrictEval,
      ADeclaredVarNames);
    Module.CompileProfile := BuildBytecodeCompileProfile(
      SourcePipelineOptions(stScript), FStrictTypes);
  finally
    Compiler.Free;
  end;
  FRetainedModules.Add(Module);
  Result := Module.TopLevel;
end;

{ ES2026 §16.2.1.6.1.2 Link(): a program run here was linked by no module
  loader, so link the graph behind each of its static requests, in source
  order, before it runs. A name it imports or re-exports that does not
  resolve is a SyntaxError raised before any module of the graph evaluates.
  The linked modules are kept for the run, so the program's own OP_IMPORTs
  evaluate them without resolving their requests a second time. }
procedure TGocciaEngine.LinkModuleRequests(
  const AModule: TGocciaBytecodeModule);
var
  I, J: Integer;
  ModuleRequest: TGocciaModuleImport;
  Names: array of string;
begin
  if (AModule.ImportCount = 0) or not Assigned(FModuleLoader) then
    Exit;
  FLinkedModules := TOrderedStringMap<TGocciaModule>.Create;
  FLinkedReferrer := AModule.SourcePath;
  for I := 0 to AModule.ImportCount - 1 do
  begin
    ModuleRequest := AModule.GetImport(I);
    SetLength(Names, Length(ModuleRequest.Bindings));
    for J := 0 to High(ModuleRequest.Bindings) do
      Names[J] := ModuleRequest.Bindings[J].ExportName;
    FLinkedModules.AddOrSetValue(ModuleRequest.ModulePath,
      FModuleLoader.LinkModuleRequest(ModuleRequest.ModulePath,
        AModule.SourcePath, Names, ModuleRequest.Line, ModuleRequest.Column));
  end;
  FVM.LoadModule := LoadLinkedModule;
end;

{ ES2026 §16.2.1.7.3.1 InitializeEnvironment for a module-source entry, the
  Module Record the engine registered as the VM's current runtime module.
  The entry's local exports are bound before its requests are linked, as
  Execute binds them: a module in a cycle with the entry that links
  meanwhile resolves each of them, and its `export * from` the entry
  forwards them (§16.2.1.7.2.1 GetExportedNames), since the loader settles a
  star export's names when the module is linked. A local export is a binding
  in its temporal dead zone, kept in a module environment of its own (the
  program's top-level bindings live in VM registers); the program's
  OP_EXPORTs initialize and update it. }
procedure TGocciaEngine.InitializeEntryLocalExports(
  const AModule: TGocciaBytecodeModule; const AEntry: TGocciaModule);
var
  ExportEnvironment: TGocciaScope;
  Export_: TGocciaModuleExport;
  I: Integer;
begin
  ExportEnvironment := nil;
  for I := 0 to AModule.ExportCount - 1 do
  begin
    Export_ := AModule.GetExport(I);
    if Export_.Kind <> mekLocal then
      Continue;
    if not Assigned(ExportEnvironment) then
      ExportEnvironment := TGocciaScope.Create(nil, skModule,
        'ModuleExports:' + AEntry.Path);
    ExportEnvironment.PredeclareLexicalBinding(Export_.Name, dtLet);
    AEntry.AddExportBinding(Export_.Name, Export_.Name, ExportEnvironment);
  end;
  if Assigned(ExportEnvironment) then
    AEntry.SetEnvironment(ExportEnvironment);
end;

{ The entry's other exports bind to the linked module of their request, so
  they are bound once its requests are linked and before any of them
  evaluates. }
procedure TGocciaEngine.InitializeEntryLinkedExports(
  const AModule: TGocciaBytecodeModule; const AEntry: TGocciaModule);
var
  Changed: Boolean;
  Export_: TGocciaModuleExport;
  I: Integer;
  SourceModule: TGocciaModule;
  StarName: string;
begin
  for I := 0 to AModule.ExportCount - 1 do
  begin
    Export_ := AModule.GetExport(I);
    case Export_.Kind of
      mekSource:
        AEntry.AddExportValue(Export_.Name,
          FModuleLoader.LoadModuleSourceValue(Export_.ModuleRequest,
            AModule.SourcePath));
      mekDeferredNamespace:
        AEntry.AddExportValue(Export_.Name,
          FModuleLoader.LoadDeferredModuleNamespaceValue(
            Export_.ModuleRequest, AModule.SourcePath));
      mekIndirect, mekNamespace:
        if Assigned(FLinkedModules) and
           FLinkedModules.TryGetValue(Export_.ModuleRequest, SourceModule) then
        begin
          if Export_.Kind = mekNamespace then
            AEntry.AddExportValue(Export_.Name,
              SourceModule.GetNamespaceObject)
          else
            AEntry.AddExportForwarding(Export_.Name, SourceModule,
              Export_.ImportName);
        end;
    end;
  end;

  { Star exports last: a name the entry exports explicitly is not replaced
    by one a star export would forward (§16.2.1.7.2.2 ResolveExport). }
  for I := 0 to AModule.ExportCount - 1 do
  begin
    Export_ := AModule.GetExport(I);
    if (Export_.Kind <> mekStar) or not Assigned(FLinkedModules) or
       not FLinkedModules.TryGetValue(Export_.ModuleRequest, SourceModule) then
      Continue;
    repeat
      Changed := False;
      for StarName in SourceModule.GetExportNames do
        if StarName <> KEYWORD_DEFAULT then
          Changed := AEntry.AddStarExportForwarding(StarName, SourceModule,
            StarName) or Changed;
    until not Changed;
  end;
end;

{ The program's OP_IMPORTs, which run in source order after its function
  declarations are instantiated and before any other statement of its body,
  evaluate its linked requests. For a module-source entry this is ES2026
  §16.2.1.6.1.3.1 InnerModuleEvaluation steps 11-12: once its last request
  has evaluated, the entry waits for each request whose evaluation is
  asynchronous (one of its [[PendingAsyncDependencies]]) before its body
  runs, and fails with the error of one that failed. The names the last
  import declaration binds are assigned after this wait. }
function TGocciaEngine.LoadLinkedModule(const AModulePath,
  AImportingFilePath: string): TGocciaModule;
begin
  if Assigned(FLinkedModules) and (AImportingFilePath = FLinkedReferrer) and
     FLinkedModules.TryGetValue(AModulePath, Result) then
  begin
    Result := FModuleLoader.EvaluateModule(Result);
    if Assigned(FEntryRequests) then
    begin
      FEntryRequests.Add(Result);
      Dec(FEntryRequestsLeft);
      if FEntryRequestsLeft = 0 then
        AwaitEntryRequests;
    end;
  end
  else
    Result := FModuleLoader.LoadModule(AModulePath, AImportingFilePath);
end;

procedure TGocciaEngine.AwaitEntryRequests;
var
  I: Integer;
  PendingPromises: TList<TGocciaPromiseValue>;
  Promise: TGocciaPromiseValue;
  Requests: TGocciaModuleList;
begin
  Requests := FEntryRequests;
  FEntryRequests := nil;
  PendingPromises := TList<TGocciaPromiseValue>.Create;
  try
    for I := 0 to Requests.Count - 1 do
    begin
      Promise := TGocciaModuleLoader.EvaluationWaitPromise(Requests[I],
        FVM.CurrentRuntimeModule);
      if Assigned(Promise) and (Promise.State <> gpsFulfilled) and
         (PendingPromises.IndexOf(Promise) < 0) then
        PendingPromises.Add(Promise);
    end;
    if PendingPromises.Count > 0 then
      FVM.AwaitModuleDependencies(
        TGocciaModuleLoader.AllEvaluationsSettled(PendingPromises));
  finally
    PendingPromises.Free;
    Requests.Free;
  end;
end;

function TGocciaEngine.RunBytecodeModule(const AModule: TGocciaBytecodeModule;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  GC: TGarbageCollector;
  HasEntryExports: Boolean;
  SavedLinkedModules: TOrderedStringMap<TGocciaModule>;
  SavedLinkedReferrer: string;
  SavedLoadModule: TLoadModuleCallback;
  SavedEntryRequests: TGocciaModuleList;
  SavedEntryRequestsLeft: Integer;
  WasEnabled: Boolean;
begin
  if not Assigned(AModule) then
    raise EArgumentNilException.Create('Bytecode module is nil');
  { Every public raw execution path meets here. Loaded artifacts must keep
    their recorded profile even when callers bypass the file/buffer helpers. }
  if AModule.WasLoadedFromBytecode then
    ValidateBytecodeModule(AModule);
  if Assigned(AModule.LinkPlan) then
    raise EInvalidOperation.Create(
      'Linked bytecode requires RunModuleForSourceType or RunBytecodeBuffer');
  GC := TGarbageCollector.Instance;
  WasEnabled := GC.Enabled;
  GC.Enabled := False;
  FVM.CoverageEnabled := (TGocciaCoverageTracker.Instance <> nil)
    and TGocciaCoverageTracker.Instance.Enabled;
  FVM.ProfilingOpcodes := (TGocciaProfiler.Instance <> nil)
    and TGocciaProfiler.Instance.Enabled
    and (pmOpcodes in TGocciaProfiler.Instance.Mode);
  FVM.ProfilingFunctions := (TGocciaProfiler.Instance <> nil)
    and TGocciaProfiler.Instance.Enabled
    and (pmFunctions in TGocciaProfiler.Instance.Mode);
  if AModule.WasLoadedFromBytecode then
    FVM.GlobalBackedTopLevel := AModule.GlobalBackedTopLevel
  else
    FVM.GlobalBackedTopLevel := FGlobalBackedTopLevel;
  { A run can nest (the Function constructor runs its body here), so the
    outer run's linked modules are set aside, not overwritten. }
  SavedLinkedModules := FLinkedModules;
  SavedLinkedReferrer := FLinkedReferrer;
  SavedLoadModule := FVM.LoadModule;
  SavedEntryRequests := FEntryRequests;
  SavedEntryRequestsLeft := FEntryRequestsLeft;
  FLinkedModules := nil;
  FEntryRequests := nil;
  try
    HasEntryExports := Assigned(FVM.CurrentRuntimeModule) and
      (AModule.ExportCount > 0);
    if HasEntryExports then
      InitializeEntryLocalExports(AModule, FVM.CurrentRuntimeModule);
    LinkModuleRequests(AModule);
    if HasEntryExports then
      InitializeEntryLinkedExports(AModule, FVM.CurrentRuntimeModule);
    if Assigned(FVM.CurrentRuntimeModule) and Assigned(FLinkedModules) then
    begin
      FEntryRequests := TGocciaModuleList.Create;
      FEntryRequestsLeft := AModule.ImportCount;
    end;
    Result := FVM.ExecuteModule(AModule, AThisValue);
  finally
    FEntryRequests.Free;
    FEntryRequests := SavedEntryRequests;
    FEntryRequestsLeft := SavedEntryRequestsLeft;
    FVM.LoadModule := SavedLoadModule;
    FLinkedModules.Free;
    FLinkedModules := SavedLinkedModules;
    FLinkedReferrer := SavedLinkedReferrer;
    GC.Enabled := WasEnabled;
  end;
end;

function TGocciaEngine.CompileModule(
  const AProgram: TGocciaProgram): TGocciaBytecodeModule;
var
  SavedGlobalBackedTopLevel: Boolean;
  FloatingPointState: TGocciaFloatingPointState;
begin
  EnterGocciaFloatingPointScope(FloatingPointState);
  try
    { A host configures GlobalBackedTopLevel before the parse settles a
      detected goal (ADR 0140), so a Module's top level is kept off the global
      object here. }
    SavedGlobalBackedTopLevel := FGlobalBackedTopLevel;
    if FSourceType = stModule then
      FGlobalBackedTopLevel := False;
    try
      // Module source is always strict.
      Result := CompileProgram(AProgram,
        (FSourceType <> stModule) and (cfNonStrictMode in FCompatibility));
    finally
      FGlobalBackedTopLevel := SavedGlobalBackedTopLevel;
    end;
    FRetainedModules.Add(Result);
  finally
    LeaveGocciaFloatingPointScope(FloatingPointState);
  end;
end;

function TGocciaEngine.EvaluateGlobalEval(const ASourceText,
  ASourcePath: string; out AStage: TGocciaGlobalEvalStage): TGocciaValue;
var
  Options: TGocciaSourcePipelineOptions;
  OptionsScope: TGocciaSourcePipelineOptionsScope;
  ParseResult: TGocciaSourcePipelineResult;
  RealmScope: TGocciaExecutionContextScope;
  SourceLines: TStringList;
  StrictEval: Boolean;
  VarNames, FunctionNames, DeclaredVarNames: TUnicodeStringList;
  Compiler: TGocciaCompiler;
  Module: TGocciaBytecodeModule;
  CompilerOptions: TGocciaCompilerOptimizationOptions;
  SavedGlobalScope: TGocciaScope;
  SavedGlobalBackedTopLevel: Boolean;
  FloatingPointState: TGocciaFloatingPointState;
  CompileProfile: TGocciaCompileProfiler;
  CompileProfileUnitDepth: Integer;
begin
  AStage := gesCompile;
  // The eval code's parse and compile are profiled as a unit of their own,
  // not as functions of the caller's file; its run is not part of it.
  CompileProfile := TGocciaCompileProfiler.Instance;
  CompileProfileUnitDepth := -1;
  if Assigned(CompileProfile) then
  begin
    CompileProfileUnitDepth := CompileProfile.UnitDepth;
    CompileProfile.BeginUnit('<eval>', ASourceText);
  end;
  VarNames := TUnicodeStringList.Create;
  FunctionNames := TUnicodeStringList.Create;
  DeclaredVarNames := TUnicodeStringList.Create;
  SourceLines := CreateECMAScriptSourceLines(ASourceText);
  // An embedder can call this entry outside any run, so it establishes the
  // floating-point mode (NaN and infinity arithmetic without traps, ES2026
  // §6.1.6.1) for the compile and the run, as Execute does, and restores the
  // caller's.
  EnterGocciaFloatingPointScope(FloatingPointState);
  try
    Options := SourcePipelineOptions(stScript);
    OptionsScope := TGocciaSourcePipeline.ActivateOptions(Options);
    try
      RealmScope := ActivateRealmExecutionContext;
      try
        // §19.2.1.1 step 11: parse the source as a Script and check the early
        // errors the eval call owns. An indirect eval is not in a function, a
        // method or a derived constructor, so new.target, super.x and super()
        // are SyntaxErrors.
        ParseResult := TGocciaSourcePipeline.Parse(SourceLines, ASourcePath,
          Options);
        try
          ValidateEvalScriptBody(ParseResult.ProgramNode.Body);
          // Step 13: the eval is strict only by its own directive.
          StrictEval := HasUseStrictDirective(ParseResult.ProgramNode);
          ValidateEvalEarlyErrors(ParseResult.ProgramNode, StrictEval, False,
            False, False);
          ValidateEvalControlFlow(ParseResult.ProgramNode);
          CollectGlobalEvalDeclarationNames(ParseResult.ProgramNode,
            VarBindingNameCollectionMode(not StrictEval,
              cfNonStrictMode in FCompatibility),
            VarNames, FunctionNames, DeclaredVarNames);
          CheckEvalLexicalDeclarationConflicts(ParseResult.ProgramNode,
            VarNames);

          // Steps 17-33, compiled: the eval code compiles to a module of its
          // own, kept for the engine's life like the Function constructor's
          // code (ADR 0134, decision 1).
          Compiler := TGocciaCompiler.Create(ASourcePath);
          try
            Compiler.StrictTypes := FStrictTypes;
            Compiler.NonStrictMode := cfNonStrictMode in FCompatibility;
            Compiler.ArgumentsObjectEnabled :=
              cfArgumentsObject in FCompatibility;
            CompilerOptions := Compiler.OptimizationOptions;
            CompilerOptions.PreserveCoverageShape :=
              (TGocciaCoverageTracker.Instance <> nil) and
              TGocciaCoverageTracker.Instance.Enabled;
            Compiler.OptimizationOptions := CompilerOptions;
            Module := Compiler.CompileGlobalEval(ParseResult.ProgramNode,
              StrictEval, DeclaredVarNames);
            Module.CompileProfile := BuildBytecodeCompileProfile(
              Options, FStrictTypes);
          finally
            Compiler.Free;
          end;
          FRetainedModules.Add(Module);
          if Assigned(CompileProfile) then
            CompileProfile.EndUnitTo(CompileProfileUnitDepth);

          // §19.2.1.3 EvalDeclarationInstantiation checks the global object
          // first; then the template binds the eval's functions and vars and
          // runs the body on this realm's VM, against the realm's global
          // environment even when the eval was called from a module body.
          AStage := gesRun;
          if not StrictEval then
            CheckGlobalEvalDeclarations(FGlobalScope, VarNames,
              FunctionNames, DeclaredVarNames);

          // The eval can run inside a module body, which has the VM's global
          // scope and script-top-level flag set for itself
          // (EvaluateModuleBody); RunBytecodeModule sets both for the eval,
          // so the module body gets its own back.
          SavedGlobalScope := FVM.GlobalScope;
          SavedGlobalBackedTopLevel := FVM.GlobalBackedTopLevel;
          FVM.GlobalScope := FGlobalScope;
          try
            Result := RunBytecodeModule(Module, FGlobalScope.ThisValue);
          finally
            FVM.GlobalBackedTopLevel := SavedGlobalBackedTopLevel;
            FVM.GlobalScope := SavedGlobalScope;
          end;
        finally
          ParseResult.Free;
        end;
      finally
        RealmScope.Free;
      end;
    finally
      OptionsScope.Free;
    end;
  finally
    if Assigned(CompileProfile) then
      CompileProfile.EndUnitTo(CompileProfileUnitDepth);
    LeaveGocciaFloatingPointScope(FloatingPointState);
    SourceLines.Free;
    DeclaredVarNames.Free;
    FunctionNames.Free;
    VarNames.Free;
  end;
end;

function TGocciaEngine.SourcePipelineOptions(
  const ASourceType: TGocciaSourceType): TGocciaSourcePipelineOptions;
begin
  Result := TGocciaSourcePipeline.DefaultOptions;
  Result.Preprocessors := FPreprocessors;
  Result.Compatibility := FCompatibility;
  Result.LabelStatementsEnabled := FLabelStatementsEnabled;
  Result.ForInLoopsEnabled := FForInLoopsEnabled;
  Result.ExperimentalJSModuleSourceEnabled :=
    FExperimentalJSModuleSourceEnabled;
  Result.WarningUnsupportedFeatures := FWarningUnsupportedFeatures;
  Result.SourceType := ASourceType;
end;

procedure TGocciaEngine.RetainModule(const AModule: TGocciaBytecodeModule);
begin
  FRetainedModules.Add(AModule);
end;

function TGocciaEngine.RetainedModuleCount: Integer;
begin
  Result := FRetainedModules.Count;
end;

function TGocciaEngine.RetainedModule(
  const AIndex: Integer): TGocciaBytecodeModule;
begin
  Result := TGocciaBytecodeModule(FRetainedModules[AIndex]);
end;

procedure RegisterEntryModuleExports(const AModule: TGocciaModule;
  const AProgram: TGocciaProgram; const AModuleScope: TGocciaScope;
  const AModuleLoader: TGocciaModuleLoader;
  const ARegisterIndirectExports: Boolean);
var
  ExportClassDecl: TGocciaExportClassDeclaration;
  ExportDecl: TGocciaExportDeclaration;
  ExportDefaultDecl: TGocciaExportDefaultDeclaration;
  ExportDestructuringDecl: TGocciaExportDestructuringDeclaration;
  ExportEnumDecl: TGocciaExportEnumDeclaration;
  ExportFuncDecl: TGocciaExportFunctionDeclaration;
  ExportPair: TStringStringMap.TKeyValuePair;
  ExportVarDecl: TGocciaExportVariableDeclaration;
  ExportName: string;
  I: Integer;
  Name: string;
  Names: TUnicodeStringList;
  ReExportDecl: TGocciaReExportDeclaration;
  SourceModule: TGocciaModule;
  Stmt: TGocciaStatement;
  Value: TGocciaValue;
  VarInfo: TGocciaVariableInfo;
  VarName: string;

  procedure AddStarExportForwardings(const ASourceModule: TGocciaModule);
  var
    Changed: Boolean;
    ExportName: string;
  begin
    repeat
      Changed := False;
      for ExportName in ASourceModule.GetExportNames do
        if ExportName <> KEYWORD_DEFAULT then
          Changed := AModule.AddStarExportForwarding(ExportName,
            ASourceModule, ExportName) or Changed;
    until not Changed;
  end;

  procedure ValidateIndirectReExports;
  var
    ExportPair: TStringStringMap.TKeyValuePair;
    ReExportDecl: TGocciaReExportDeclaration;
    SourceModule: TGocciaModule;
    Stmt: TGocciaStatement;
    ValidationIndex: Integer;
  begin
    if not Assigned(AModuleLoader) then
      Exit;

    for ValidationIndex := 0 to AProgram.Body.Count - 1 do
    begin
      Stmt := AProgram.Body[ValidationIndex];
      if not (Stmt is TGocciaReExportDeclaration) then
        Continue;

      ReExportDecl := TGocciaReExportDeclaration(Stmt);
      if ReExportDecl.IsStarExport then
        Continue;

      SourceModule := AModuleLoader.LoadModule(EncodeImportSpecifierAttribute(
        ReExportDecl.ModulePath, ReExportDecl.AttributeType), AModule.Path);
      for ExportPair in ReExportDecl.ExportsTable do
        if (not AModule.CanResolveExport(ExportPair.Key)) and
           (not AModuleLoader.IsEvaluatingModulePath(SourceModule.Path)) then
          raise TGocciaSyntaxError.Create(
            Format('Module "%s" has no export named "%s"',
              [ReExportDecl.ModulePath, ExportPair.Value]),
            ReExportDecl.Line, ReExportDecl.Column, AModule.Path, nil);
    end;
  end;

  function IsImportedLocalName(const ALocalName: string): Boolean;
  var
    ImportDecl: TGocciaImportDeclaration;
    ImportPair: TStringStringMap.TKeyValuePair;
    J: Integer;
  begin
    Result := False;
    for J := 0 to AProgram.Body.Count - 1 do
    begin
      if not (AProgram.Body[J] is TGocciaImportDeclaration) then
        Continue;

      ImportDecl := TGocciaImportDeclaration(AProgram.Body[J]);
      if ImportDecl.NamespaceName = ALocalName then
        Exit(True);

      for ImportPair in ImportDecl.Imports do
        if ImportPair.Key = ALocalName then
          Exit(True);
    end;
  end;

  function TryAddImportedLocalExport(const ALocalName,
    AExportName: string): Boolean;
  var
    ImportDecl: TGocciaImportDeclaration;
    ImportPair: TStringStringMap.TKeyValuePair;
    J: Integer;
  begin
    Result := False;
    if not Assigned(AModuleLoader) then
      Exit;

    for J := 0 to AProgram.Body.Count - 1 do
    begin
      if not (AProgram.Body[J] is TGocciaImportDeclaration) then
        Continue;

      ImportDecl := TGocciaImportDeclaration(AProgram.Body[J]);
      if ImportDecl.NamespaceName = ALocalName then
      begin
        case ImportDecl.Phase of
          icpDefer:
            Value := AModuleLoader.LoadDeferredModuleNamespaceValue(
              EncodeImportSpecifierAttribute(ImportDecl.ModulePath,
              ImportDecl.AttributeType), AModule.Path);
          icpSource:
            Value := AModuleLoader.LoadModuleSourceValue(
              EncodeImportSpecifierAttribute(ImportDecl.ModulePath,
              ImportDecl.AttributeType), AModule.Path);
        else
          Value := AModuleLoader.LoadModule(EncodeImportSpecifierAttribute(
            ImportDecl.ModulePath, ImportDecl.AttributeType),
            AModule.Path).GetNamespaceObject;
        end;
        AModule.AddExportValue(AExportName, Value);
        Exit(True);
      end;

      for ImportPair in ImportDecl.Imports do
      begin
        if ImportPair.Key <> ALocalName then
          Continue;

        if ImportDecl.Phase = icpSource then
        begin
          Value := AModuleLoader.LoadModuleSourceValue(
            EncodeImportSpecifierAttribute(ImportDecl.ModulePath,
            ImportDecl.AttributeType), AModule.Path);
          AModule.AddExportValue(AExportName, Value);
        end
        else
        begin
          SourceModule := AModuleLoader.LoadModule(
            EncodeImportSpecifierAttribute(ImportDecl.ModulePath,
            ImportDecl.AttributeType), AModule.Path);
          AModule.AddExportForwarding(AExportName, SourceModule,
            ImportPair.Value);
        end;
        Exit(True);
      end;
    end;
  end;
begin
  if (not Assigned(AModule)) or (not Assigned(AProgram)) or
     (not Assigned(AModuleScope)) then
    Exit;

  if not ARegisterIndirectExports then
  begin
    AModule.ClearExports;

    for I := 0 to AProgram.Body.Count - 1 do
    begin
      Stmt := AProgram.Body[I];

    if Stmt is TGocciaExportDeclaration then
    begin
      ExportDecl := TGocciaExportDeclaration(Stmt);
      for ExportPair in ExportDecl.ExportsTable do
        if (not IsImportedLocalName(ExportPair.Value)) and
           AModuleScope.Contains(ExportPair.Value) then
          AModule.AddExportBinding(ExportPair.Key, ExportPair.Value,
            AModuleScope);
    end
    else if Stmt is TGocciaExportDefaultDeclaration then
    begin
      ExportDefaultDecl := TGocciaExportDefaultDeclaration(Stmt);
      AModule.AddExportBinding(KEYWORD_DEFAULT,
        ExportDefaultDecl.LocalName, AModuleScope);
    end
    else if Stmt is TGocciaExportVariableDeclaration then
    begin
      ExportVarDecl := TGocciaExportVariableDeclaration(Stmt);
      Names := TUnicodeStringList.Create;
      try
        for VarInfo in ExportVarDecl.Declaration.Variables do
          CollectVariableInfoBindingNames(VarInfo, Names, True);
        for VarName in Names do
          AModule.AddExportBinding(VarName, VarName, AModuleScope);
      finally
        Names.Free;
      end;
    end
    else if Stmt is TGocciaExportDestructuringDeclaration then
    begin
      ExportDestructuringDecl := TGocciaExportDestructuringDeclaration(Stmt);
      Names := TUnicodeStringList.Create;
      try
        CollectPatternBindingNames(ExportDestructuringDecl.Declaration.Pattern,
          Names, True);
        for Name in Names do
          AModule.AddExportBinding(Name, Name, AModuleScope);
      finally
        Names.Free;
      end;
    end
    else if Stmt is TGocciaExportFunctionDeclaration then
    begin
      ExportFuncDecl := TGocciaExportFunctionDeclaration(Stmt);
      AModule.AddExportBinding(ExportFuncDecl.Declaration.Name,
        ExportFuncDecl.Declaration.Name, AModuleScope);
    end
    else if Stmt is TGocciaExportClassDeclaration then
    begin
      ExportClassDecl := TGocciaExportClassDeclaration(Stmt);
      AModule.AddExportBinding(ExportClassDecl.Declaration.ClassDefinition.Name,
        ExportClassDecl.Declaration.ClassDefinition.Name, AModuleScope);
    end
    else if Stmt is TGocciaExportEnumDeclaration then
    begin
      ExportEnumDecl := TGocciaExportEnumDeclaration(Stmt);
      AModule.AddExportBinding(ExportEnumDecl.Declaration.Name,
        ExportEnumDecl.Declaration.Name, AModuleScope);
    end
    end;

    AModule.InvalidateNamespaceObject;
    Exit;
  end;

  for I := 0 to AProgram.Body.Count - 1 do
  begin
    Stmt := AProgram.Body[I];

    if Stmt is TGocciaExportDeclaration then
    begin
      ExportDecl := TGocciaExportDeclaration(Stmt);
      for ExportPair in ExportDecl.ExportsTable do
        if TryAddImportedLocalExport(ExportPair.Value, ExportPair.Key) then
          Continue;
    end
    else if (Stmt is TGocciaReExportDeclaration) and Assigned(AModuleLoader) then
    begin
      ReExportDecl := TGocciaReExportDeclaration(Stmt);
      SourceModule := AModuleLoader.LoadModule(EncodeImportSpecifierAttribute(
        ReExportDecl.ModulePath, ReExportDecl.AttributeType), AModule.Path);
      if ReExportDecl.IsStarExport then
      begin
        if ReExportDecl.NamespaceName <> '' then
          AModule.AddExportValue(ReExportDecl.NamespaceName,
            SourceModule.GetNamespaceObject)
        else
          AddStarExportForwardings(SourceModule);
      end
      else
      begin
        for ExportPair in ReExportDecl.ExportsTable do
        begin
          if SourceModule.IsAmbiguousExport(ExportPair.Value) then
            raise TGocciaSyntaxError.Create(
              Format('Module "%s" has ambiguous export named "%s"',
                [ReExportDecl.ModulePath, ExportPair.Value]),
              ReExportDecl.Line, ReExportDecl.Column, AModule.Path, nil);
          if (not SourceModule.HasExport(ExportPair.Value)) and
             (not AModuleLoader.IsEvaluatingModulePath(SourceModule.Path)) then
            raise TGocciaSyntaxError.Create(
              Format('Module "%s" has no export named "%s"',
                [ReExportDecl.ModulePath, ExportPair.Value]),
              ReExportDecl.Line, ReExportDecl.Column, AModule.Path, nil);
          AModule.AddExportForwarding(ExportPair.Key, SourceModule,
            ExportPair.Value);
        end;
      end;
    end;
  end;

  ValidateIndirectReExports;
  if Assigned(AModuleLoader) then
    AModuleLoader.ValidateStaticNamedImports(AProgram, AModule);
end;

{ Each request of the entry, in source order, loaded as LoadModule loads it.
  An evaluation-phase import binds its names in the entry's environment
  AModuleScope as soon as its module is loaded (ES2026 §16.2.1.7.3.1
  InitializeEnvironment step 7, as the loader binds an imported module's), so
  the entry's function declarations, which read their imports from the
  environment, see them from then on. }
procedure EvaluateEntryRequestedModulesInSourceOrder(
  const AProgram: TGocciaProgram; const AModuleLoader: TGocciaModuleLoader;
  const AImportingFilePath: string; const AModuleScope: TGocciaScope;
  const ARequestedModules: TGocciaModuleList);
var
  I: Integer;
  ImportDecl: TGocciaImportDeclaration;
  PreviousCallSite: TGocciaCallSite;
  RequestedModule: TGocciaModule;
  Stmt: TGocciaStatement;
begin
  if (not Assigned(AProgram)) or (not Assigned(AModuleLoader)) then
    Exit;

  for I := 0 to AProgram.Body.Count - 1 do
  begin
    Stmt := AProgram.Body[I];
    if not ((Stmt is TGocciaImportDeclaration) or
       (Stmt is TGocciaReExportDeclaration)) then
      Continue;
    { The declaration is the call site of what loading it decides or refuses,
      as OP_IMPORT makes it in the VM. }
    EnterGocciaCallSite(AImportingFilePath, Stmt.Line, Stmt.Column,
      PreviousCallSite);
    try
      if Stmt is TGocciaImportDeclaration then
      begin
        ImportDecl := TGocciaImportDeclaration(Stmt);
        case ImportDecl.Phase of
          icpEvaluation:
          begin
            RequestedModule := AModuleLoader.LoadModule(
              EncodeImportSpecifierAttribute(ImportDecl.ModulePath,
              ImportDecl.AttributeType), AImportingFilePath);
            if Assigned(RequestedModule) and Assigned(ARequestedModules) then
              ARequestedModules.Add(RequestedModule);
            if Assigned(RequestedModule) and Assigned(AModuleScope) then
              BindLinkedImportDeclaration(AModuleScope, ImportDecl,
                RequestedModule);
          end;
          icpDefer:
            AModuleLoader.LoadDeferredModuleNamespaceValueForEvaluation(
              EncodeImportSpecifierAttribute(ImportDecl.ModulePath,
              ImportDecl.AttributeType), AImportingFilePath,
              ARequestedModules);
        end;
      end
      else
      begin
        RequestedModule := AModuleLoader.LoadModule(
          EncodeImportSpecifierAttribute(
            TGocciaReExportDeclaration(Stmt).ModulePath,
            TGocciaReExportDeclaration(Stmt).AttributeType),
          AImportingFilePath);
        if Assigned(RequestedModule) and Assigned(ARequestedModules) then
          ARequestedModules.Add(RequestedModule);
      end;
    finally
      LeaveGocciaCallSite(PreviousCallSite);
    end;
  end;
end;

procedure DrainEntryRequestedModuleEvaluationPromises(
  const ARequestedModules: TGocciaModuleList; const AEntryModule: TGocciaModule);
var
  EvalPromise: TGocciaPromiseValue;
  HasPending: Boolean;
  I: Integer;
  PendingPromise: TGocciaPromiseValue;
  Queue: TGocciaMicrotaskQueue;
  SawWaitPromise: Boolean;

  function WaitPromiseFor(const ARequestedModule: TGocciaModule): TGocciaPromiseValue;
  var
    WaitModule: TGocciaModule;
  begin
    Result := nil;
    if not Assigned(ARequestedModule) then
      Exit;

    if ARequestedModule.EvaluationPromise is TGocciaPromiseValue then
      Exit(TGocciaPromiseValue(ARequestedModule.EvaluationPromise));

    WaitModule := ARequestedModule.AsyncCycleRoot;
    if Assigned(WaitModule) and (WaitModule <> AEntryModule) and
       (WaitModule.EvaluationPromise is TGocciaPromiseValue) then
      Result := TGocciaPromiseValue(WaitModule.EvaluationPromise);
  end;
begin
  if not Assigned(ARequestedModules) then
    Exit;

  Queue := TGocciaMicrotaskQueue.Instance;
  SawWaitPromise := False;
  repeat
    HasPending := False;
    for I := 0 to ARequestedModules.Count - 1 do
    begin
      EvalPromise := WaitPromiseFor(ARequestedModules[I]);
      if Assigned(EvalPromise) then
      begin
        SawWaitPromise := True;
        case EvalPromise.State of
          gpsRejected:
            begin
              if Assigned(Queue) then
                while Queue.HasPending do
                  Queue.DrainOneJob;
              raise TGocciaThrowValue.Create(EvalPromise.PromiseResult);
            end;
          gpsPending:
            HasPending := True;
        end;
      end;
    end;
    if HasPending and Assigned(Queue) and Queue.HasPending then
      Queue.DrainOneJob
    else
      Break;
  until False;

  PendingPromise := nil;
  for I := 0 to ARequestedModules.Count - 1 do
  begin
    EvalPromise := WaitPromiseFor(ARequestedModules[I]);
    if Assigned(EvalPromise) then
    begin
      case EvalPromise.State of
        gpsRejected:
          begin
            if Assigned(Queue) then
              while Queue.HasPending do
                Queue.DrainOneJob;
            raise TGocciaThrowValue.Create(EvalPromise.PromiseResult);
          end;
        gpsPending:
          if not Assigned(PendingPromise) then
            PendingPromise := EvalPromise;
      end;
    end;
  end;

  if SawWaitPromise and (not Assigned(PendingPromise)) and Assigned(Queue) then
    while Queue.HasPending do
      Queue.DrainOneJob;

  if Assigned(PendingPromise) and Assigned(AEntryModule) and
     not Assigned(AEntryModule.EvaluationPromise) then
    AEntryModule.EvaluationPromise := PendingPromise;
end;

procedure RegisterEntrySyntheticDefaultExports(const AModule: TGocciaModule;
  const AProgram: TGocciaProgram; const AModuleScope: TGocciaScope);
var
  ExportDefaultDecl: TGocciaExportDefaultDeclaration;
  I: Integer;
  Stmt: TGocciaStatement;
begin
  if (not Assigned(AModule)) or (not Assigned(AProgram)) or
     (not Assigned(AModuleScope)) then
    Exit;

  for I := 0 to AProgram.Body.Count - 1 do
  begin
    Stmt := AProgram.Body[I];
    if not (Stmt is TGocciaExportDefaultDeclaration) then
      Continue;

    ExportDefaultDecl := TGocciaExportDefaultDeclaration(Stmt);
    if (ExportDefaultDecl.LocalName = GOCCIA_DEFAULT_EXPORT_BINDING) and
       AModuleScope.Contains(GOCCIA_DEFAULT_EXPORT_BINDING) then
      AModule.AddExportBinding(KEYWORD_DEFAULT,
        GOCCIA_DEFAULT_EXPORT_BINDING, AModuleScope);
  end;
end;

function TGocciaEngine.RunModule(
  const AModule: TGocciaBytecodeModule): TGocciaValue;
var
  OptionsScope: TGocciaSourcePipelineOptionsScope;
  RunSourceType: TGocciaSourceType;
  GC: TGarbageCollector;
  FloatingPointState: TGocciaFloatingPointState;
  PreviousRunningEngine: TGocciaEngine;
  { A flag rather than the RTL's exception object: on SEH targets that object
    is not populated while a finally runs during unwinding. }
  Completed: Boolean;
begin
  EnterGocciaFloatingPointScope(FloatingPointState);
  PreviousRunningEngine := GRunningEngine;
  GRunningEngine := Self;
  Completed := False;
  OptionsScope := nil;
  try
    RunSourceType := FSourceType;
    if Assigned(AModule) and AModule.WasLoadedFromBytecode then
      RunSourceType := ValidateBytecodeModule(AModule);
    OptionsScope := TGocciaSourcePipeline.ActivateOptions(
      SourcePipelineOptions(RunSourceType));
    Result := RunBytecodeModule(AModule, nil);
    GC := TGarbageCollector.Instance;
    if Assigned(Result) and Assigned(GC) then
      GC.AddTempRoot(Result);
    try
      WaitForRuntimeIdle;
      RaiseUnhandledRejection;
    finally
      if Assigned(Result) and Assigned(GC) then
        GC.RemoveTempRoot(Result);
    end;
    Completed := True;
  finally
    if not Completed then
      ClearPendingWorkOfFailedRun(PreviousRunningEngine);
    GRunningEngine := PreviousRunningEngine;
    OptionsScope.Free;
    LeaveGocciaFloatingPointScope(FloatingPointState);
  end;
end;

function TGocciaEngine.RunModuleInScope(
  const AModule: TGocciaBytecodeModule;
  const AScope: TGocciaScope): TGocciaValue;
var
  OptionsScope: TGocciaSourcePipelineOptionsScope;
  RunSourceType: TGocciaSourceType;
  GC: TGarbageCollector;
  SavedGlobalScope: TGocciaScope;
  FloatingPointState: TGocciaFloatingPointState;
  PreviousRunningEngine: TGocciaEngine;
  { A flag rather than the RTL's exception object: on SEH targets that object
    is not populated while a finally runs during unwinding. }
  Completed: Boolean;
begin
  EnterGocciaFloatingPointScope(FloatingPointState);
  PreviousRunningEngine := GRunningEngine;
  GRunningEngine := Self;
  Completed := False;
  OptionsScope := nil;
  try
    RunSourceType := FSourceType;
    if Assigned(AModule) and AModule.WasLoadedFromBytecode then
      RunSourceType := ValidateBytecodeModule(AModule);
    OptionsScope := TGocciaSourcePipeline.ActivateOptions(
      SourcePipelineOptions(RunSourceType));
    SavedGlobalScope := FVM.GlobalScope;
    FVM.GlobalScope := AScope;
    try
      Result := RunBytecodeModule(AModule, nil);
    finally
      FVM.GlobalScope := SavedGlobalScope;
    end;
    GC := TGarbageCollector.Instance;
    if Assigned(Result) and Assigned(GC) then
      GC.AddTempRoot(Result);
    try
      WaitForRuntimeIdle;
      RaiseUnhandledRejection;
    finally
      if Assigned(Result) and Assigned(GC) then
        GC.RemoveTempRoot(Result);
    end;
    Completed := True;
  finally
    if not Completed then
      ClearPendingWorkOfFailedRun(PreviousRunningEngine);
    GRunningEngine := PreviousRunningEngine;
    OptionsScope.Free;
    LeaveGocciaFloatingPointScope(FloatingPointState);
  end;
end;

{ The provider owns its entry namespace independently of source-file
  existence. In particular, a source-free sandbox entry stays `/`-rooted on
  Windows instead of acquiring a host drive prefix. }
function EntryModulePath(const ALoader: TGocciaModuleLoader;
  const ASourcePath: string): string;
var
  Provider: TGocciaModuleContentProvider;
begin
  if IsVirtualModuleURI(ASourcePath) then
    Exit(ALoader.VirtualModules.CanonicalAddress(ASourcePath, ''));
  Provider := ALoader.ContentProvider;
  if Assigned(Provider) then
    Result := Provider.NormalizeEntryAddress(ASourcePath)
  else
    Result := ExpandFileName(ASourcePath);
end;

function TGocciaEngine.RunModuleForSourceType(
  const AModule: TGocciaBytecodeModule;
  const AFileName: string): TGocciaValue;
var
  EntryLastModified: TDateTime;
  EntryModule: TGocciaModule;
  ModuleScope: TGocciaScope;
  PrevScope: TGocciaDiagnosticSourceScope;
  SavedGlobalBackedTopLevel: Boolean;
  SavedRuntimeModule: TGocciaModule;
  RunSourceType: TGocciaSourceType;
begin
  RunSourceType := ValidateBytecodeModule(AModule);
  if Assigned(AModule.LinkPlan) then
  begin
    FModuleLoader.RegisterPrecompiledModule(EntryModulePath(FModuleLoader, AFileName),
      AModule, nil);
    Exit(RunRegisteredBytecodeEntry(EntryModulePath(FModuleLoader, AFileName)));
  end;
  if AModule.WasLoadedFromBytecode then
    AModule.RelocateSourcePath(EntryModulePath(FModuleLoader, AFileName));
  // Entry for an already compiled module (Execute parses and compiles the
  // engine's source itself). Bind code-frame capture to this engine's own
  // scope for the run, as Execute does.
  PrevScope := TGocciaDiagnosticSourceRegistry.Activate(
    FModuleLoader.DiagnosticScope);
  try
    if RunSourceType = stModule then
    begin
      ModuleScope := FGlobalScope.CreateChild(skModule,
        'Module:' + AFileName);
      ModuleScope.ThisValue := TGocciaUndefinedLiteralValue.UndefinedValue;
      ModuleScope.NonStrictMode := False;
      ModuleScope.ArgumentsObjectEnabled :=
        cfArgumentsObject in FCompatibility;
      { ES2026 §16.2.1.10 HostLoadImportedModule: an import that resolves to
        the entry's own file gets the entry's Module Record, which
        InnerModuleEvaluation (§16.2.1.6.1.3.1) does not evaluate again while
        it is evaluating or once it is evaluated. Register the entry under its
        resolved path, stamped with its file's modification time, and mark it
        evaluating before its imports are linked, as Execute does. As the
        VM's current runtime module it gets the program's exports
        (InitializeEntryLocalExports and InitializeEntryLinkedExports, then
        OP_EXPORT). }
      EntryModule := TGocciaModule.Create(
        EntryModulePath(FModuleLoader, AFileName));
      if FModuleLoader.ContentProvider.TryGetLastModified(EntryModule.Path,
         EntryLastModified) then
        EntryModule.LastModified := EntryLastModified;
      EntryModule.SnapshotKind := mskEntryCompiled;
      EntryModule.CompiledModule := AModule;
      FModuleLoader.RegisterModule(EntryModule.Path, EntryModule);
      SavedRuntimeModule := FVM.CurrentRuntimeModule;
      FVM.CurrentRuntimeModule := EntryModule;
      { As CompileModule: the Module's top level is not global-backed. }
      SavedGlobalBackedTopLevel := FGlobalBackedTopLevel;
      FGlobalBackedTopLevel := False;
      FModuleLoader.BeginEvaluatingModulePath(EntryModule.Path);
      try
        try
          Result := RunModuleInScope(AModule, ModuleScope);
        except
          on E: Exception do
          begin
            { A later import of the entry gets its error, never the record
              of this incomplete run. }
            FModuleLoader.FailEntryModule(EntryModule, E);
            raise;
          end;
        end;
      finally
        FModuleLoader.EndEvaluatingModulePath(EntryModule.Path);
        FVM.CurrentRuntimeModule := SavedRuntimeModule;
        FGlobalBackedTopLevel := SavedGlobalBackedTopLevel;
      end;
    end
    else
    begin
      SavedGlobalBackedTopLevel := FGlobalBackedTopLevel;
      FGlobalBackedTopLevel := AModule.GlobalBackedTopLevel;
      try
        Result := RunModule(AModule);
      finally
        FGlobalBackedTopLevel := SavedGlobalBackedTopLevel;
      end;
    end;
  finally
    TGocciaDiagnosticSourceRegistry.Deactivate(PrevScope);
  end;
end;

function TGocciaEngine.CallAsRun(const AFunction: TGocciaFunctionBase;
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  GC: TGarbageCollector;
  FloatingPointState: TGocciaFloatingPointState;
  PrevScope: TGocciaDiagnosticSourceScope;
  PreviousRunningEngine: TGocciaEngine;
  Completed: Boolean;
begin
  PrevScope := TGocciaDiagnosticSourceRegistry.Activate(
    FModuleLoader.DiagnosticScope);
  EnterGocciaFloatingPointScope(FloatingPointState);
  PreviousRunningEngine := GRunningEngine;
  GRunningEngine := Self;
  Completed := False;
  try
    Result := AFunction.Call(AArguments, AThisValue);
    GC := TGarbageCollector.Instance;
    if Assigned(Result) and Assigned(GC) then
      GC.AddTempRoot(Result);
    try
      WaitForRuntimeIdle;
      RaiseUnhandledRejection;
    finally
      if Assigned(Result) and Assigned(GC) then
        GC.RemoveTempRoot(Result);
    end;
    Completed := True;
  finally
    if not Completed then
      ClearPendingWorkOfFailedRun(PreviousRunningEngine);
    GRunningEngine := PreviousRunningEngine;
    LeaveGocciaFloatingPointScope(FloatingPointState);
    TGocciaDiagnosticSourceRegistry.Deactivate(PrevScope);
  end;
end;

procedure TGocciaEngine.DiscardRuntimePending;
var
  I: Integer;
begin
  for I := 0 to FExtensions.Count - 1 do
    FExtensions[I].DiscardPending;
end;

procedure TGocciaEngine.InjectGlobal(const AKey: string; const AValue: TGocciaValue);
begin
  RegisterGlobal(AKey, AValue);
end;

procedure TGocciaEngine.RegisterGlobalModule(const AName: string; const AModule: TGocciaModule);
begin
  if FModuleLoader.VirtualModules.Contains(AName) then
    raise EInvalidOperation.CreateFmt(
      'Host module "%s" conflicts with a configured virtual module.',
      [AName]);
  AModule.IsHostOwned := True;
  FModuleLoader.GlobalModules.AddOrSetValue(AName, AModule);
end;

procedure TGocciaEngine.RegisterHostModule(const AName: string;
  const AModule: TGocciaModule);
begin
  RegisterGlobalModule(AName, AModule);
end;

procedure TGocciaEngine.RegisterGlobalModuleProvider(const AName: string;
  const AProvider: TGocciaGlobalModuleProvider);
begin
  FModuleLoader.RegisterGlobalModuleProvider(AName, AProvider);
end;

procedure TGocciaEngine.RegisterHostModuleProvider(const AName: string;
  const AProvider: TGocciaGlobalModuleProvider);
begin
  RegisterGlobalModuleProvider(AName, AProvider);
end;

procedure TGocciaEngine.UnregisterGlobalModuleProvider(const AName: string);
begin
  FModuleLoader.UnregisterGlobalModuleProvider(AName);
end;

procedure TGocciaEngine.UnregisterHostModuleProvider(const AName: string);
begin
  UnregisterGlobalModuleProvider(AName);
end;

{ The entry's evaluation promise once the run is idle. Rejected, the entry
  threw. Still pending, its top-level await waits on a promise that nothing
  left in the run can settle: that fails the run as it does where the entry's
  awaits run synchronously (Goccia.Values.Await.AwaitValue), never reporting
  a run that stopped partway as a success. A rejection no handler took is
  usually why — a reaction that threw instead of settling what the entry
  awaits — so it is reported first, and under the throw policy it is the
  run's error. }
procedure RaiseUnlessEntryPromiseFulfilled(const AEngine: TGocciaEngine;
  const APromise: TGocciaPromiseValue);
begin
  case APromise.State of
    gpsRejected:
      raise TGocciaThrowValue.Create(APromise.PromiseResult);
    gpsPending:
      begin
        AEngine.RaiseUnhandledRejection;
        ThrowTypeError(SErrorAwaitPromiseUnsettled,
          SSuggestAwaitMicrotaskDrain);
      end;
  end;
end;

function TGocciaEngine.ValidateBytecodeModule(
  const AModule: TGocciaBytecodeModule): TGocciaSourceType;
var
  Goal: TGocciaSourceType;
  Expected: TGocciaCompileProfile;
begin
  if not Assigned(AModule) then
    raise EArgumentNilException.Create('Bytecode module is nil');
  Goal := FSourceType;
  if Goal = stDetect then
    if AModule.CompileProfile.SourceType = bstModule then
      Goal := stModule
    else
      Goal := stScript;
  Expected := BuildBytecodeCompileProfile(SourcePipelineOptions(Goal), FStrictTypes);
  if not AModule.HasCompileProfile or not AModule.CompileProfile.Matches(Expected) then
    raise EArgumentException.Create('Bytecode compile profile does not match the engine');
  Result := Goal;
end;

procedure TGocciaEngine.ValidateImportedBytecode(const AModule: TGocciaBytecodeModule);
var
  Expected: TGocciaCompileProfile;
begin
  Expected := BuildBytecodeCompileProfile(SourcePipelineOptions(stModule), FStrictTypes);
  if not AModule.HasCompileProfile or not AModule.CompileProfile.Matches(Expected) then
    raise EArgumentException.Create('Bytecode compile profile does not match the engine');
  if (AModule.ExecutionKind <> bekLinkedModule) or not Assigned(AModule.LinkPlan) then
    raise EArgumentException.Create('Imported bytecode requires a linked module artifact');
end;

function TGocciaEngine.DecodeModuleBytecode(const ABytes: TBytes): TGocciaBytecodeModule;
begin
  Result := LoadModuleFromBuffer(ABytes);
  try
    ValidateImportedBytecode(Result);
    FRetainedModules.Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TGocciaEngine.CompileImportableModule(
  const AProgram: TGocciaProgram): TGocciaBytecodeModule;
var
  Compiler: TGocciaCompiler;
  FloatingPointState: TGocciaFloatingPointState;
begin
  EnterGocciaFloatingPointScope(FloatingPointState);
  try
    Compiler := TGocciaCompiler.Create(FSourcePath);
    try
      Compiler.GlobalBackedTopLevel := True;
      Compiler.PreinitializedTopLevelFunctions := True;
      Compiler.AsyncTopLevel := AProgram.HasTopLevelAwait;
      Compiler.NonStrictMode := False;
      Compiler.StrictTypes := FStrictTypes;
      Compiler.ArgumentsObjectEnabled := cfArgumentsObject in FCompatibility;
      Result := Compiler.Compile(AProgram);
      Result.CompileProfile := BuildBytecodeCompileProfile(SourcePipelineOptions(stModule), FStrictTypes);
      FRetainedModules.Add(Result);
    finally
      Compiler.Free;
    end;
  finally
    LeaveGocciaFloatingPointScope(FloatingPointState);
  end;
end;

procedure TGocciaEngine.ReleaseModuleBytecode(const AModule: TGocciaBytecodeModule);
begin
  FModuleLoader.ForgetBytecodeModule(AModule);
  FRetainedModules.Remove(AModule);
end;

procedure TGocciaEngine.RegisterBytecodeModule(const AAddress: string;
  const ABytes: TBytes; const ABaseAddress: string);
var
  Module: TGocciaBytecodeModule;
begin
  Module := DecodeModuleBytecode(ABytes);
  try
    FModuleLoader.RegisterPrecompiledModule(AAddress, Module, ABytes, ABaseAddress);
  except
    FRetainedModules.Remove(Module);
    raise;
  end;
end;

function TGocciaEngine.RunBytecodeBuffer(const ABytes: TBytes;
  const AAddress: string): TGocciaValue;
var
  Module: TGocciaBytecodeModule;
  Address: string;
begin
  Address := FModuleLoader.VirtualModules.CanonicalAddress(AAddress, '');
  Module := LoadModuleFromBuffer(ABytes);
  try
    ValidateBytecodeModule(Module);
  except
    Module.Free;
    raise;
  end;
  FRetainedModules.Add(Module);
  if Assigned(Module.LinkPlan) then
  begin
    try
      FModuleLoader.RegisterPrecompiledModule(Address, Module, ABytes);
    except
      FRetainedModules.Remove(Module);
      raise;
    end;
    Result := RunRegisteredBytecodeEntry(Address);
  end
  else
    Result := RunModuleForSourceType(Module, Address);
end;

function TGocciaEngine.RunRegisteredBytecodeEntry(const AAddress: string): TGocciaValue;
var
  OptionsScope: TGocciaSourcePipelineOptionsScope;
  Module: TGocciaModule;
  FloatingPointState: TGocciaFloatingPointState;
  PreviousRunningEngine: TGocciaEngine;
  PreviousDiagnosticScope: TGocciaDiagnosticSourceScope;
  Completed, NestedRun, OwnsMicrotaskScope: Boolean;
  Queue: TGocciaMicrotaskQueue;
  MicrotaskScopeToken: Integer;
begin
  PreviousDiagnosticScope := TGocciaDiagnosticSourceRegistry.Activate(FModuleLoader.DiagnosticScope);
  EnterGocciaFloatingPointScope(FloatingPointState);
  PreviousRunningEngine := GRunningEngine;
  GRunningEngine := Self;
  Completed := False;
  OptionsScope := nil;
  Queue := TGocciaMicrotaskQueue.Instance;
  NestedRun := Assigned(PreviousRunningEngine) and (PreviousRunningEngine <> Self);
  OwnsMicrotaskScope := False;
  MicrotaskScopeToken := 0;
  try
    OptionsScope := TGocciaSourcePipeline.ActivateOptions(
      SourcePipelineOptions(stModule));
    if NestedRun and Assigned(Queue) then
    begin
      MicrotaskScopeToken := Queue.EnterScope;
      OwnsMicrotaskScope := True;
    end;
    Module := FModuleLoader.LinkModuleRequest(AAddress, FSourcePath, [], 0, 0);
    Module.CaptureEvaluationResult := True;
    FModuleLoader.EvaluateModule(Module);
    WaitForRuntimeIdle;
    if Module.EvaluationPromise is TGocciaPromiseValue then
      RaiseUnlessEntryPromiseFulfilled(Self, TGocciaPromiseValue(Module.EvaluationPromise));
    RaiseUnhandledRejection;
    Result := Module.EvaluationResult;
    if not Assigned(Result) then
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Completed := True;
  finally
    if not Completed then
      ClearPendingWorkOfFailedRun(PreviousRunningEngine);
    if OwnsMicrotaskScope then
      Queue.LeaveScope(MicrotaskScopeToken)
    else if (not NestedRun) and Assigned(Queue) then
      Queue.ClearQueue;
    DiscardRuntimePending;
    GRunningEngine := PreviousRunningEngine;
    OptionsScope.Free;
    LeaveGocciaFloatingPointScope(FloatingPointState);
    TGocciaDiagnosticSourceRegistry.Deactivate(PreviousDiagnosticScope);
  end;
end;

function TGocciaEngine.Execute: TGocciaScriptResult;
var
  RunSourceType: TGocciaSourceType;
  PipelineOptions: TGocciaSourcePipelineOptions;
  ActiveOptionsScope: TGocciaSourcePipelineOptionsScope;
  PipelineResult: TGocciaSourcePipelineResult;
  StartTime, ExecStart, ExecEnd: Int64;
  ModuleScope: TGocciaScope;
  ModuleContext: TGocciaEvaluationContext;
  ModuleResult: TGocciaValue;
  ModuleProgramConsumed: Boolean;
  EntryHasTopLevelAwait: Boolean;
  EntryModule: TGocciaModule;
  EntryModuleEvaluationStarted: Boolean;
  EntryRequestedModules: TGocciaModuleList;
  EntryLastModified: TDateTime;
  SavedVMGlobalScope: TGocciaScope;
  GC: TGarbageCollector;
  FloatingPointState: TGocciaFloatingPointState;
  PrevDiagScope: TGocciaDiagnosticSourceScope;
  MicrotaskQueue: TGocciaMicrotaskQueue;
  MicrotaskScopeToken: Integer;
  NestedRun, OwnsMicrotaskScope: Boolean;
  PreviousRunningEngine: TGocciaEngine;
begin
  // Bind runtime-error code-frame capture to THIS engine's source scope for the
  // duration of its execution, restoring the caller's on exit. The previous
  // scope is a per-invocation LOCAL (never an instance field): a host callback
  // that re-enters this same engine's Execute must not overwrite an outer
  // invocation's saved scope, or the outer restore would dangle after the
  // engine is freed (use-after-free). Capture targets the active scope, so a
  // nested (sandbox/ShadowRealm) child, a parent that resumes while a child
  // stays alive, and a later sequential run each capture only their own guest
  // source. See Goccia.Diagnostics.SourceRegistry.
  PrevDiagScope := TGocciaDiagnosticSourceRegistry.Activate(
    FModuleLoader.DiagnosticScope);
  EnterGocciaFloatingPointScope(FloatingPointState);
  try
  FillChar(FLastTiming, SizeOf(FLastTiming), 0);
  FLastTiming.FileName := FSourcePath;
  StartTime := GetNanoseconds;
  PipelineResult := nil;

  try
    PipelineOptions := SourcePipelineOptions(FSourceType);
    PipelineResult := TGocciaSourcePipeline.Parse(FSourceLines, FSourcePath,
      PipelineOptions);
    { The goal this run has: FSourceType, or the one the parse settled for
      stDetect (ADR 0140). FSourceType keeps the setting, so a long-lived
      engine settles each Execute's source on its own. }
    RunSourceType := PipelineResult.SourceType;
    PipelineOptions.SourceType := RunSourceType;
    FLastTiming.LexTimeNanoseconds := PipelineResult.LexTimeNanoseconds;
    FLastTiming.ParseTimeNanoseconds := PipelineResult.ParseTimeNanoseconds;
    PrintSourcePipelineWarnings(PipelineResult);

    if (TGocciaCoverageTracker.Instance <> nil) and
       TGocciaCoverageTracker.Instance.Enabled then
    begin
      TGocciaCoverageTracker.Instance.RegisterSourceFile(
        FSourcePath, CountExecutableLines(PipelineResult.GeneratedSourceLines));
      if Assigned(PipelineResult.SourceMap) then
        TGocciaCoverageTracker.Instance.RegisterSourceMap(
          FSourcePath, PipelineResult.SourceMap.Clone);
    end;

    ActiveOptionsScope := TGocciaSourcePipeline.ActivateOptions(PipelineOptions);
    { A nested run gets a microtask scope of its own. Execute can be reached
      from inside another engine's execution on this thread (a sandbox
      `runScript` child), and that engine's pending jobs are neither this
      run's to drain nor, on failure, this run's to discard. The outermost run
      keeps the thread's own scope, so jobs the host queued ahead of it — a
      globals module loaded before the entry script — still drain with it. A
      host callback that re-enters this same engine shares its scope too.
      Per-invocation locals for the same reason as PrevDiagScope above. }
    MicrotaskQueue := TGocciaMicrotaskQueue.Instance;
    PreviousRunningEngine := GRunningEngine;
    NestedRun := Assigned(PreviousRunningEngine) and
      (PreviousRunningEngine <> Self);
    OwnsMicrotaskScope := False;
    MicrotaskScopeToken := 0;
    GRunningEngine := Self;
    try
      if NestedRun and Assigned(MicrotaskQueue) then
      begin
        MicrotaskScopeToken := MicrotaskQueue.EnterScope;
        OwnsMicrotaskScope := True;
      end;
      if RunSourceType = stModule then
      begin
        // ES2026 §16.2.1.6.4: a Module Environment Record's
        // [[ThisValue]] is undefined.  Run the entry program in a
        // fresh module scope so import.meta resolves and top-level
        // `this` is undefined, matching the semantics imported
        // modules already receive via TGocciaModuleLoader.
        ModuleScope := FGlobalScope.CreateChild(skModule,
          'Module:' + FSourcePath);
        ModuleScope.ThisValue :=
          TGocciaUndefinedLiteralValue.UndefinedValue;
        ModuleScope.NonStrictMode := False;
        ModuleScope.ArgumentsObjectEnabled :=
          cfArgumentsObject in FCompatibility;
        EntryModule := nil;
          EntryModuleEvaluationStarted := False;
          EntryRequestedModules := nil;
          ModuleContext := CreateEvaluationContext;
        ModuleContext.Realm := FRealm;
        ModuleContext.Scope := ModuleScope;
        ModuleContext.CurrentFilePath := FSourcePath;
        ModuleContext.CurrentModule := nil;
        ModuleContext.NonStrictMode := False;
        ModuleContext.CompatibilityNonStrictMode := False;
        if Assigned(FModuleLoader) then
        begin
          EntryRequestedModules := TGocciaModuleList.Create;
          EntryModule := TGocciaModule.Create(
            EntryModulePath(FModuleLoader, FSourcePath));
          { Stamp the entry with its file's modification time, as the
            loader stamps every module it loads. Unstamped, a later
            import of the entry's own path took it for a changed file
            and evaluated its body a second time. }
          if FModuleLoader.ContentProvider.TryGetLastModified(
             EntryModule.Path, EntryLastModified) then
            EntryModule.LastModified := EntryLastModified;
          EntryModule.SetEnvironment(ModuleScope);
          FModuleLoader.RegisterModule(EntryModule.Path, EntryModule);
          ModuleContext.CurrentModule := EntryModule;
          FModuleLoader.BeginEvaluatingModulePath(EntryModule.Path);
          EntryModuleEvaluationStarted := True;
          try
            PredeclareModuleLexicalDeclarations(PipelineResult.ProgramNode,
              ModuleScope);
            InstantiateModuleFunctions(PipelineResult.ProgramNode,
              ModuleContext);
            EntryModule.SnapshotKind := mskSource;
            EntryModule.CompiledModule := ModuleContext.CompiledModule;
            if FModuleLoader.VarEnabled then
              HoistVarDeclarations(PipelineResult.ProgramNode.Body,
                ModuleScope, ModuleContext);
            ModuleContext.ModuleEnvironmentInitialized := True;
            RegisterEntryModuleExports(EntryModule, PipelineResult.ProgramNode,
              ModuleScope, FModuleLoader, False);
            EvaluateEntryRequestedModulesInSourceOrder(
              PipelineResult.ProgramNode, FModuleLoader, FSourcePath,
              ModuleScope, EntryRequestedModules);
            RegisterEntryModuleExports(EntryModule, PipelineResult.ProgramNode,
              ModuleScope, FModuleLoader, True);
            SavedVMGlobalScope := FVM.GlobalScope;
            FVM.GlobalScope := ModuleScope;
            try
              DrainEntryRequestedModuleEvaluationPromises(
                EntryRequestedModules, EntryModule);
            finally
              FVM.GlobalScope := SavedVMGlobalScope;
            end;
          except
            on E: Exception do
            begin
              FModuleLoader.EndEvaluatingModulePath(EntryModule.Path);
              EntryModuleEvaluationStarted := False;
              FModuleLoader.FailEntryModule(EntryModule, E);
              raise;
            end;
          end;
        end
        else
        begin
          PredeclareModuleLexicalDeclarations(PipelineResult.ProgramNode,
            ModuleScope);
          InstantiateModuleFunctions(PipelineResult.ProgramNode,
            ModuleContext);
          ModuleContext.ModuleEnvironmentInitialized := True;
        end;
        ExecStart := GetNanoseconds;
        ModuleResult := nil;
        { Read before evaluating: evaluation may take the program node. }
        EntryHasTopLevelAwait := PipelineResult.ProgramNode.HasTopLevelAwait;
        GC := TGarbageCollector.Instance;
        if Assigned(GC) then
          GC.AddTempRoot(ModuleScope);
        try
          try
            ModuleResult := EvaluateModuleBody(
              PipelineResult.ProgramNode, ModuleContext,
              ModuleProgramConsumed);
            if Assigned(EntryModule) then
              RegisterEntrySyntheticDefaultExports(EntryModule,
                PipelineResult.ProgramNode, ModuleScope);
            if ModuleProgramConsumed then
              PipelineResult.TakeProgramNode;
            if Assigned(ModuleResult) and Assigned(GC) then
              GC.AddTempRoot(ModuleResult);
            try
              SavedVMGlobalScope := FVM.GlobalScope;
              FVM.GlobalScope := ModuleScope;
              try
                WaitForRuntimeIdle;
                { Only a module with top-level await returns its evaluation
                  promise here. Any other module returns the completion value
                  of its last statement, which is guest data: a rejected
                  promise there is the guest's, handled or not, and failing
                  the run with it reported a handled rejection, or a later one
                  ahead of the oldest unhandled one. Those reach
                  RaiseUnhandledRejection below, as they do in bytecode
                  (RunModuleForSourceType). }
                if EntryHasTopLevelAwait and
                   (ModuleResult is TGocciaPromiseValue) then
                  RaiseUnlessEntryPromiseFulfilled(Self,
                    TGocciaPromiseValue(ModuleResult));
                if Assigned(EntryModule) and
                   (EntryModule.EvaluationPromise is TGocciaPromiseValue) then
                  RaiseUnlessEntryPromiseFulfilled(Self,
                    TGocciaPromiseValue(EntryModule.EvaluationPromise));
                RaiseUnhandledRejection;
                FLastTiming.Result := ModuleResult;
              finally
                FVM.GlobalScope := SavedVMGlobalScope;
              end;
            finally
              if Assigned(ModuleResult) and Assigned(GC) then
                GC.RemoveTempRoot(ModuleResult);
            end;
          except
            on E: Exception do
            begin
              { A later import of the entry gets its error, never the
                record of this incomplete run. }
              if Assigned(EntryModule) and Assigned(FModuleLoader) then
                FModuleLoader.FailEntryModule(EntryModule, E);
              raise;
            end;
          end;
        finally
          if EntryModuleEvaluationStarted and Assigned(FModuleLoader) then
            FModuleLoader.EndEvaluatingModulePath(EntryModule.Path);
          EntryRequestedModules.Free;
          if Assigned(GC) then
            GC.RemoveTempRoot(ModuleScope);
        end;
        ExecEnd := GetNanoseconds;
        FLastTiming.CompileTimeNanoseconds := 0;
        FLastTiming.ExecuteTimeNanoseconds := ExecEnd - ExecStart;
      end
      else
      begin
        CheckTopLevelRedeclarations(PipelineResult.ProgramNode,
          FGlobalScope, FSourcePath);
        FLastTiming.Result := ExecuteProgram(PipelineResult.ProgramNode);
        ExecEnd := GetNanoseconds;
        FLastTiming.CompileTimeNanoseconds := FCompileTimeNanoseconds;
        FLastTiming.ExecuteTimeNanoseconds := FExecuteTimeNanoseconds;
      end;
      FLastTiming.TotalTimeNanoseconds := ExecEnd - StartTime;
    finally
      GRunningEngine := PreviousRunningEngine;
      ActiveOptionsScope.Free;
      { Either way this run's leftover jobs are discarded. Leaving the scope
        also puts the enclosing engine's jobs back. }
      if OwnsMicrotaskScope then
        MicrotaskQueue.LeaveScope(MicrotaskScopeToken)
      else if (not NestedRun) and (TGocciaMicrotaskQueue.Instance <> nil) then
        TGocciaMicrotaskQueue.Instance.ClearQueue;
      DiscardRuntimePending;
    end;
  finally
    if FLastTiming.TotalTimeNanoseconds = 0 then
      FLastTiming.TotalTimeNanoseconds := GetNanoseconds - StartTime;
    FLastSourceMap.Free;
    if Assigned(PipelineResult) then
      FLastSourceMap := PipelineResult.TakeSourceMap
    else
      FLastSourceMap := nil;
    PipelineResult.Free;
  end;

  Result := FLastTiming;
  finally
    LeaveGocciaFloatingPointScope(FloatingPointState);
    TGocciaDiagnosticSourceRegistry.Deactivate(PrevDiagScope);
  end;
end;

function TGocciaEngine.TakeLastSourceMap: TGocciaSourceMap;
begin
  Result := FLastSourceMap;
  FLastSourceMap := nil;
end;

function TGocciaEngine.ExecuteProgram(const AProgram: TGocciaProgram): TGocciaValue;
var
  GC: TGarbageCollector;
  FloatingPointState: TGocciaFloatingPointState;
  PrevScope: TGocciaDiagnosticSourceScope;
  PreviousRunningEngine: TGocciaEngine;
  Completed: Boolean;
  Module: TGocciaBytecodeModule;
  Start: Int64;
begin
  PrevScope := TGocciaDiagnosticSourceRegistry.Activate(
    FModuleLoader.DiagnosticScope);
  EnterGocciaFloatingPointScope(FloatingPointState);
  PreviousRunningEngine := GRunningEngine;
  GRunningEngine := Self;
  Completed := False;
  try
    Start := GetNanoseconds;
    Module := CompileProgram(AProgram, cfNonStrictMode in FCompatibility);
    FCompileTimeNanoseconds := GetNanoseconds - Start;
    FRetainedModules.Add(Module);
    Start := GetNanoseconds;
    Result := RunBytecodeModule(Module, nil);
    FExecuteTimeNanoseconds := GetNanoseconds - Start;
    GC := TGarbageCollector.Instance;
    if Assigned(Result) and Assigned(GC) then
      GC.AddTempRoot(Result);
    try
      WaitForRuntimeIdle;
      RaiseUnhandledRejection;
    finally
      if Assigned(Result) and Assigned(GC) then
        GC.RemoveTempRoot(Result);
    end;
    Completed := True;
  finally
    if not Completed then
      ClearPendingWorkOfFailedRun(PreviousRunningEngine);
    GRunningEngine := PreviousRunningEngine;
    LeaveGocciaFloatingPointScope(FloatingPointState);
    TGocciaDiagnosticSourceRegistry.Deactivate(PrevScope);
  end;
end;

class function TGocciaEngine.RunScript(const ASource: string;
  const AFileName: string): TGocciaScriptResult;
var
  SourceList: TStringList;
begin
  SourceList := CreateTextLines(ASource);
  try
    Result := RunScriptFromStringList(SourceList, AFileName);
  finally
    SourceList.Free;
  end;
end;

class function TGocciaEngine.RunScriptFromStringList(
  const ASource: TStringList; const AFileName: string): TGocciaScriptResult;
var
  Engine: TGocciaEngine;
begin
  Engine := TGocciaEngine.Create(AFileName, ASource);
  try
    { Script source keeps its top-level bindings on the global object; a
      Module (an .mjs name, or a detected one, ADR 0140) does not, which
      Execute arranges for itself. }
    Engine.GlobalBackedTopLevel := Engine.SourceType <> stModule;
    Result := Engine.Execute;
  finally
    Engine.Free;
  end;
end;

procedure TGocciaEngine.PrintSourcePipelineWarnings(
  const APipelineResult: TGocciaSourcePipelineResult);
var
  Warning: TGocciaSourcePipelineWarning;
  I: Integer;
begin
  if FSuppressWarnings then
    Exit;
  if not Assigned(APipelineResult) then
    Exit;
  for I := 0 to APipelineResult.WarningCount - 1 do
  begin
    Warning := APipelineResult.Warnings[I];
    WriteLn(Format('Warning: %s', [Warning.Message]));
    if Warning.Suggestion <> '' then
      WriteLn(Format('  Suggestion: %s', [Warning.Suggestion]));
    WriteLn(Format('  --> %s:%d:%d', [FSourcePath, Warning.Line,
      Warning.Column]));
  end;
  { Standard output is block-buffered when it is not a terminal, and the hosts
    that print these warnings also write diagnostics to the unbuffered
    ErrOutput. With both redirected to one file, whatever is still sitting in
    the stdout buffer surfaces after the next stderr write — which is how a
    warning's `--> <path>` line came out cut mid-path with other output spliced
    into it. Flushing at the end of the block keeps a warning whole. }
  if APipelineResult.WarningCount > 0 then
    Flush(Output);
end;

function TGocciaEngine.SpeciesGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := AThisValue;
end;

function TGocciaEngine.GocciaGC(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
    GC.Collect;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaEngine.GocciaGCMaxBytesGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  GC: TGarbageCollector;
  WasFiring: Boolean;
begin
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
  begin
    WasFiring := GC.MemoryLimitFiring;
    GC.MemoryLimitFiring := True;
    try
      Result := TGocciaNumberLiteralValue.Create(GC.MaxBytes);
    finally
      GC.MemoryLimitFiring := WasFiring;
    end;
  end
  else
    Result := TGocciaNumberLiteralValue.ZeroValue;
end;

function TGocciaEngine.GocciaGCSuggestedMaxBytesGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  GC: TGarbageCollector;
  WasFiring: Boolean;
begin
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
  begin
    WasFiring := GC.MemoryLimitFiring;
    GC.MemoryLimitFiring := True;
    try
      Result := TGocciaNumberLiteralValue.Create(GC.SuggestedMaxBytes);
    finally
      GC.MemoryLimitFiring := WasFiring;
    end;
  end
  else
    Result := TGocciaNumberLiteralValue.ZeroValue;
end;

function TGocciaEngine.GocciaGCBytesAllocatedGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  GC: TGarbageCollector;
  WasFiring: Boolean;
begin
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
  begin
    WasFiring := GC.MemoryLimitFiring;
    GC.MemoryLimitFiring := True;
    try
      Result := TGocciaNumberLiteralValue.Create(GC.BytesAllocated);
    finally
      GC.MemoryLimitFiring := WasFiring;
    end;
  end
  else
    Result := TGocciaNumberLiteralValue.ZeroValue;
end;

procedure TGocciaEngine.SetStrictTypes(const AValue: Boolean);
begin
  FStrictTypes := AValue;
  if Assigned(FModuleLoader) then
    FModuleLoader.StrictTypesEnabled := AValue;
  if Assigned(FGlobalScope) then
    FGlobalScope.StrictTypes := AValue;
end;

procedure TGocciaEngine.SetPreprocessors(const AValue: TGocciaPreprocessors);
begin
  FPreprocessors := AValue;
  FModuleLoader.JSXEnabled := ppJSX in AValue;
end;

procedure TGocciaEngine.SetCompatibility(AValue: TGocciaCompatibilityFlags);
begin
  FCompatibility := AValue;
  FLabelStatementsEnabled := cfLabel in AValue;
  FForInLoopsEnabled := cfForIn in AValue;
  FExperimentalJSModuleSourceEnabled :=
    cfExperimentalJSModuleSource in AValue;
  FGlobalScope.NonStrictMode := cfNonStrictMode in AValue;
  FGlobalScope.ArgumentsObjectEnabled := cfArgumentsObject in AValue;
  if Assigned(FModuleLoader) then
  begin
    FModuleLoader.Compatibility := AValue;
    FModuleLoader.LabelStatementsEnabled := FLabelStatementsEnabled;
    FModuleLoader.ForInLoopsEnabled := FForInLoopsEnabled;
    FModuleLoader.ExperimentalJSModuleSourceEnabled :=
      FExperimentalJSModuleSourceEnabled;
  end;
end;

procedure TGocciaEngine.SetLabelStatementsEnabled(const AValue: Boolean);
begin
  FLabelStatementsEnabled := AValue;
  if Assigned(FModuleLoader) then
    FModuleLoader.LabelStatementsEnabled := AValue;
end;

procedure TGocciaEngine.SetForInLoopsEnabled(const AValue: Boolean);
begin
  FForInLoopsEnabled := AValue;
  if Assigned(FModuleLoader) then
    FModuleLoader.ForInLoopsEnabled := AValue;
end;

procedure TGocciaEngine.SetExperimentalJSModuleSourceEnabled(
  const AValue: Boolean);
begin
  FExperimentalJSModuleSourceEnabled := AValue;
  if Assigned(FModuleLoader) then
    FModuleLoader.ExperimentalJSModuleSourceEnabled := AValue;
end;

procedure TGocciaEngine.SetWarningUnsupportedFeatures(const AValue: Boolean);
begin
  FWarningUnsupportedFeatures := AValue;
  if Assigned(FModuleLoader) then
    FModuleLoader.WarningUnsupportedFeatures := AValue;
end;

procedure TGocciaEngine.ThrowError(const AMessage: string; const ALine, AColumn: Integer);
begin
  raise TGocciaRuntimeError.Create(AMessage, ALine, AColumn, FSourcePath, FSourceLines);
end;

function DynamicParametersContainAwait(
  const AParameters: TGocciaParameterArray): Boolean;
begin
  Result := ParameterListContainsExpressionClass(AParameters,
    TGocciaAwaitExpression) or ParameterListBindsName(AParameters,
    KEYWORD_AWAIT);
end;

function DynamicParametersContainYield(
  const AParameters: TGocciaParameterArray): Boolean;
begin
  Result := ParameterListContainsExpressionClass(AParameters,
    TGocciaYieldExpression) or ParameterListBindsName(AParameters,
    KEYWORD_YIELD);
end;

procedure ValidateDynamicFunctionSuperExpression(
  const AExpr: TGocciaExpression); forward;
procedure ValidateDynamicFunctionSuperStatement(
  const AStmt: TGocciaStatement); forward;
procedure ValidateDynamicFunctionSuperPattern(
  const APattern: TGocciaDestructuringPattern); forward;
procedure ValidateDynamicFunctionSuperMatchPattern(
  const APattern: TGocciaMatchPattern); forward;

procedure ValidateDynamicFunctionSuperFunction(
  const AFunction: TGocciaFunctionExpression);
var
  I: Integer;
begin
  if not Assigned(AFunction) then
    Exit;
  for I := 0 to High(AFunction.Parameters) do
  begin
    if AFunction.Parameters[I].IsPattern then
      ValidateDynamicFunctionSuperPattern(AFunction.Parameters[I].Pattern);
    ValidateDynamicFunctionSuperExpression(AFunction.Parameters[I].DefaultValue);
  end;
  if AFunction.Body is TGocciaStatement then
    ValidateDynamicFunctionSuperStatement(TGocciaStatement(AFunction.Body))
  else if AFunction.Body is TGocciaExpression then
    ValidateDynamicFunctionSuperExpression(TGocciaExpression(AFunction.Body));
end;

procedure ValidateDynamicFunctionSuperPattern(
  const APattern: TGocciaDestructuringPattern);
var
  ArrPat: TGocciaArrayDestructuringPattern;
  AssignmentPattern: TGocciaAssignmentDestructuringPattern;
  ObjPat: TGocciaObjectDestructuringPattern;
  Prop: TGocciaDestructuringProperty;
  RestPattern: TGocciaRestDestructuringPattern;
  I: Integer;
begin
  if not Assigned(APattern) then
    Exit;

  if APattern is TGocciaArrayDestructuringPattern then
  begin
    ArrPat := TGocciaArrayDestructuringPattern(APattern);
    for I := 0 to ArrPat.Elements.Count - 1 do
      ValidateDynamicFunctionSuperPattern(ArrPat.Elements[I]);
  end
  else if APattern is TGocciaObjectDestructuringPattern then
  begin
    ObjPat := TGocciaObjectDestructuringPattern(APattern);
    for I := 0 to ObjPat.Properties.Count - 1 do
    begin
      Prop := ObjPat.Properties[I];
      if Prop.Computed then
        ValidateDynamicFunctionSuperExpression(Prop.KeyExpression);
      ValidateDynamicFunctionSuperPattern(Prop.Pattern);
    end;
  end
  else if APattern is TGocciaAssignmentDestructuringPattern then
  begin
    AssignmentPattern := TGocciaAssignmentDestructuringPattern(APattern);
    ValidateDynamicFunctionSuperPattern(AssignmentPattern.Left);
    ValidateDynamicFunctionSuperExpression(AssignmentPattern.Right);
  end
  else if APattern is TGocciaRestDestructuringPattern then
  begin
    RestPattern := TGocciaRestDestructuringPattern(APattern);
    ValidateDynamicFunctionSuperPattern(RestPattern.Argument);
  end
  else if APattern is TGocciaMemberExpressionDestructuringPattern then
    ValidateDynamicFunctionSuperExpression(
      TGocciaMemberExpressionDestructuringPattern(APattern).Expression)
  else if APattern is TGocciaPrivateMemberExpressionDestructuringPattern then
    ValidateDynamicFunctionSuperExpression(
      TGocciaPrivateMemberExpressionDestructuringPattern(APattern).Expression);
end;

procedure ValidateDynamicFunctionSuperMatchPattern(
  const APattern: TGocciaMatchPattern);
var
  ArrayPattern: TGocciaArrayMatchPattern;
  ObjectPattern: TGocciaObjectMatchPattern;
  PatternList: TGocciaMatchPatternList;
  ExtractorPattern: TGocciaExtractorMatchPattern;
  I: Integer;
begin
  if not Assigned(APattern) then
    Exit;

  if APattern is TGocciaValueMatchPattern then
    ValidateDynamicFunctionSuperExpression(
      TGocciaValueMatchPattern(APattern).Expression)
  else if APattern is TGocciaArrayMatchPattern then
  begin
    ArrayPattern := TGocciaArrayMatchPattern(APattern);
    for I := 0 to ArrayPattern.Elements.Count - 1 do
      ValidateDynamicFunctionSuperMatchPattern(ArrayPattern.Elements[I]);
    ValidateDynamicFunctionSuperMatchPattern(ArrayPattern.RestPattern);
  end
  else if APattern is TGocciaObjectMatchPattern then
  begin
    ObjectPattern := TGocciaObjectMatchPattern(APattern);
    for I := 0 to ObjectPattern.Properties.Count - 1 do
    begin
      if ObjectPattern.Properties[I].Computed then
        ValidateDynamicFunctionSuperExpression(
          ObjectPattern.Properties[I].KeyExpression);
      ValidateDynamicFunctionSuperMatchPattern(
        ObjectPattern.Properties[I].Pattern);
    end;
    ValidateDynamicFunctionSuperMatchPattern(ObjectPattern.RestPattern);
  end
  else if APattern is TGocciaRelationalMatchPattern then
    ValidateDynamicFunctionSuperExpression(
      TGocciaRelationalMatchPattern(APattern).Expression)
  else if APattern is TGocciaGuardMatchPattern then
    ValidateDynamicFunctionSuperExpression(
      TGocciaGuardMatchPattern(APattern).Condition)
  else if APattern is TGocciaAsMatchPattern then
    ValidateDynamicFunctionSuperMatchPattern(
      TGocciaAsMatchPattern(APattern).Pattern)
  else if APattern is TGocciaAndMatchPattern then
  begin
    PatternList := TGocciaAndMatchPattern(APattern).Patterns;
    for I := 0 to PatternList.Count - 1 do
      ValidateDynamicFunctionSuperMatchPattern(PatternList[I]);
  end
  else if APattern is TGocciaOrMatchPattern then
  begin
    PatternList := TGocciaOrMatchPattern(APattern).Patterns;
    for I := 0 to PatternList.Count - 1 do
      ValidateDynamicFunctionSuperMatchPattern(PatternList[I]);
  end
  else if APattern is TGocciaNotMatchPattern then
    ValidateDynamicFunctionSuperMatchPattern(
      TGocciaNotMatchPattern(APattern).Pattern)
  else if APattern is TGocciaExtractorMatchPattern then
  begin
    ExtractorPattern := TGocciaExtractorMatchPattern(APattern);
    ValidateDynamicFunctionSuperExpression(ExtractorPattern.MatcherExpression);
    for I := 0 to ExtractorPattern.Arguments.Count - 1 do
      ValidateDynamicFunctionSuperMatchPattern(ExtractorPattern.Arguments[I]);
    ValidateDynamicFunctionSuperMatchPattern(ExtractorPattern.RestPattern);
  end;
end;

procedure ValidateDynamicFunctionSuperExpression(
  const AExpr: TGocciaExpression);
var
  ArrayExpr: TGocciaArrayExpression;
  ArrowExpr: TGocciaArrowFunctionExpression;
  BinaryExpr: TGocciaBinaryExpression;
  CallExpr: TGocciaCallExpression;
  ConditionalExpr: TGocciaConditionalExpression;
  ImportExpr: TGocciaImportCallExpression;
  MemberExpr: TGocciaMemberExpression;
  NewExpr: TGocciaNewExpression;
  ObjectExpr: TGocciaObjectExpression;
  Pair: TPair<TGocciaExpression, TGocciaExpression>;
  I: Integer;
begin
  if not Assigned(AExpr) then
    Exit;

  if AExpr is TGocciaSuperExpression then
    ThrowSyntaxError('super is not allowed in this function context');

  if AExpr is TGocciaFunctionExpression then
    ValidateDynamicFunctionSuperFunction(TGocciaFunctionExpression(AExpr))
  else if AExpr is TGocciaArrowFunctionExpression then
  begin
    ArrowExpr := TGocciaArrowFunctionExpression(AExpr);
    for I := 0 to High(ArrowExpr.Parameters) do
      ValidateDynamicFunctionSuperExpression(ArrowExpr.Parameters[I].DefaultValue);
    if ArrowExpr.Body is TGocciaStatement then
      ValidateDynamicFunctionSuperStatement(TGocciaStatement(ArrowExpr.Body))
    else if ArrowExpr.Body is TGocciaExpression then
      ValidateDynamicFunctionSuperExpression(TGocciaExpression(ArrowExpr.Body));
  end
  else if AExpr is TGocciaCallExpression then
  begin
    CallExpr := TGocciaCallExpression(AExpr);
    if CallExpr.Callee is TGocciaSuperExpression then
      ThrowSyntaxError('super() is not allowed in this function context');
    ValidateDynamicFunctionSuperExpression(CallExpr.Callee);
    for I := 0 to CallExpr.Arguments.Count - 1 do
      ValidateDynamicFunctionSuperExpression(CallExpr.Arguments[I]);
  end
  else if AExpr is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(AExpr);
    if MemberExpr.ObjectExpr is TGocciaSuperExpression then
      ThrowSyntaxError('super property access is not allowed in this function context');
    ValidateDynamicFunctionSuperExpression(MemberExpr.ObjectExpr);
    if MemberExpr.Computed then
      ValidateDynamicFunctionSuperExpression(MemberExpr.PropertyExpression);
  end
  else if AExpr is TGocciaBinaryExpression then
  begin
    BinaryExpr := TGocciaBinaryExpression(AExpr);
    ValidateDynamicFunctionSuperExpression(BinaryExpr.Left);
    ValidateDynamicFunctionSuperExpression(BinaryExpr.Right);
  end
  else if AExpr is TGocciaSequenceExpression then
    for I := 0 to TGocciaSequenceExpression(AExpr).Expressions.Count - 1 do
      ValidateDynamicFunctionSuperExpression(
        TGocciaSequenceExpression(AExpr).Expressions[I])
  else if AExpr is TGocciaUnaryExpression then
    ValidateDynamicFunctionSuperExpression(TGocciaUnaryExpression(AExpr).Operand)
  else if AExpr is TGocciaAssignmentExpression then
    ValidateDynamicFunctionSuperExpression(TGocciaAssignmentExpression(AExpr).Value)
  else if AExpr is TGocciaPropertyAssignmentExpression then
  begin
    ValidateDynamicFunctionSuperExpression(
      TGocciaPropertyAssignmentExpression(AExpr).ObjectExpr);
    ValidateDynamicFunctionSuperExpression(
      TGocciaPropertyAssignmentExpression(AExpr).Value);
  end
  else if AExpr is TGocciaComputedPropertyAssignmentExpression then
  begin
    ValidateDynamicFunctionSuperExpression(
      TGocciaComputedPropertyAssignmentExpression(AExpr).ObjectExpr);
    ValidateDynamicFunctionSuperExpression(
      TGocciaComputedPropertyAssignmentExpression(AExpr).PropertyExpression);
    ValidateDynamicFunctionSuperExpression(
      TGocciaComputedPropertyAssignmentExpression(AExpr).Value);
  end
  else if AExpr is TGocciaCompoundAssignmentExpression then
    ValidateDynamicFunctionSuperExpression(
      TGocciaCompoundAssignmentExpression(AExpr).Value)
  else if AExpr is TGocciaPropertyCompoundAssignmentExpression then
  begin
    ValidateDynamicFunctionSuperExpression(
      TGocciaPropertyCompoundAssignmentExpression(AExpr).ObjectExpr);
    ValidateDynamicFunctionSuperExpression(
      TGocciaPropertyCompoundAssignmentExpression(AExpr).Value);
  end
  else if AExpr is TGocciaComputedPropertyCompoundAssignmentExpression then
  begin
    ValidateDynamicFunctionSuperExpression(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).ObjectExpr);
    ValidateDynamicFunctionSuperExpression(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).PropertyExpression);
    ValidateDynamicFunctionSuperExpression(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).Value);
  end
  else if AExpr is TGocciaIncrementExpression then
    ValidateDynamicFunctionSuperExpression(
      TGocciaIncrementExpression(AExpr).Operand)
  else if AExpr is TGocciaArrayExpression then
  begin
    ArrayExpr := TGocciaArrayExpression(AExpr);
    for I := 0 to ArrayExpr.Elements.Count - 1 do
      ValidateDynamicFunctionSuperExpression(ArrayExpr.Elements[I]);
  end
  else if AExpr is TGocciaObjectExpression then
  begin
    ObjectExpr := TGocciaObjectExpression(AExpr);
    for I := 0 to High(ObjectExpr.PropertySourceOrder) do
      case ObjectExpr.PropertySourceOrder[I].PropertyType of
        pstStatic:
          ValidateDynamicFunctionSuperExpression(
            ObjectExpr.PropertySourceOrder[I].Expression);
        pstComputed:
          begin
            Pair := ObjectExpr.ComputedPropertiesInOrder[
              ObjectExpr.PropertySourceOrder[I].ComputedIndex];
            ValidateDynamicFunctionSuperExpression(Pair.Key);
            ValidateDynamicFunctionSuperExpression(Pair.Value);
          end;
        pstComputedGetter,
        pstComputedSetter:
          begin
            Pair := ObjectExpr.ComputedPropertiesInOrder[
              ObjectExpr.PropertySourceOrder[I].ComputedIndex];
            ValidateDynamicFunctionSuperExpression(Pair.Key);
          end;
      end;
  end
  else if AExpr is TGocciaYieldExpression then
    ValidateDynamicFunctionSuperExpression(TGocciaYieldExpression(AExpr).Operand)
  else if AExpr is TGocciaAwaitExpression then
    ValidateDynamicFunctionSuperExpression(TGocciaAwaitExpression(AExpr).Operand)
  else if AExpr is TGocciaConditionalExpression then
  begin
    ConditionalExpr := TGocciaConditionalExpression(AExpr);
    ValidateDynamicFunctionSuperExpression(ConditionalExpr.Condition);
    ValidateDynamicFunctionSuperExpression(ConditionalExpr.Consequent);
    ValidateDynamicFunctionSuperExpression(ConditionalExpr.Alternate);
  end
  else if AExpr is TGocciaNewExpression then
  begin
    NewExpr := TGocciaNewExpression(AExpr);
    ValidateDynamicFunctionSuperExpression(NewExpr.Callee);
    for I := 0 to NewExpr.Arguments.Count - 1 do
      ValidateDynamicFunctionSuperExpression(NewExpr.Arguments[I]);
  end
  else if AExpr is TGocciaImportCallExpression then
  begin
    ImportExpr := TGocciaImportCallExpression(AExpr);
    ValidateDynamicFunctionSuperExpression(ImportExpr.Specifier);
    ValidateDynamicFunctionSuperExpression(ImportExpr.Options);
  end
  else if AExpr is TGocciaSpreadExpression then
    ValidateDynamicFunctionSuperExpression(TGocciaSpreadExpression(AExpr).Argument)
  else if AExpr is TGocciaTemplateWithInterpolationExpression then
  begin
    for I := 0 to TGocciaTemplateWithInterpolationExpression(AExpr).Parts.Count - 1 do
      ValidateDynamicFunctionSuperExpression(
        TGocciaTemplateWithInterpolationExpression(AExpr).Parts[I]);
  end
  else if AExpr is TGocciaTaggedTemplateExpression then
  begin
    ValidateDynamicFunctionSuperExpression(
      TGocciaTaggedTemplateExpression(AExpr).Tag);
    for I := 0 to TGocciaTaggedTemplateExpression(AExpr).Expressions.Count - 1 do
      ValidateDynamicFunctionSuperExpression(
        TGocciaTaggedTemplateExpression(AExpr).Expressions[I]);
  end
  else if AExpr is TGocciaDestructuringAssignmentExpression then
  begin
    ValidateDynamicFunctionSuperPattern(
      TGocciaDestructuringAssignmentExpression(AExpr).Left);
    ValidateDynamicFunctionSuperExpression(
      TGocciaDestructuringAssignmentExpression(AExpr).Right);
  end
  else if AExpr is TGocciaPrivateMemberExpression then
    ValidateDynamicFunctionSuperExpression(
      TGocciaPrivateMemberExpression(AExpr).ObjectExpr)
  else if AExpr is TGocciaPrivatePropertyAssignmentExpression then
  begin
    ValidateDynamicFunctionSuperExpression(
      TGocciaPrivatePropertyAssignmentExpression(AExpr).ObjectExpr);
    ValidateDynamicFunctionSuperExpression(
      TGocciaPrivatePropertyAssignmentExpression(AExpr).Value);
  end
  else if AExpr is TGocciaPrivatePropertyCompoundAssignmentExpression then
  begin
    ValidateDynamicFunctionSuperExpression(
      TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).ObjectExpr);
    ValidateDynamicFunctionSuperExpression(
      TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).Value);
  end
  else if AExpr is TGocciaIsExpression then
  begin
    ValidateDynamicFunctionSuperExpression(TGocciaIsExpression(AExpr).Subject);
    ValidateDynamicFunctionSuperMatchPattern(TGocciaIsExpression(AExpr).Pattern);
  end
  else if AExpr is TGocciaMatchExpression then
  begin
    ValidateDynamicFunctionSuperExpression(
      TGocciaMatchExpression(AExpr).Subject);
    for I := 0 to TGocciaMatchExpression(AExpr).Clauses.Count - 1 do
    begin
      ValidateDynamicFunctionSuperMatchPattern(
        TGocciaMatchExpression(AExpr).Clauses[I].Pattern);
      ValidateDynamicFunctionSuperExpression(
        TGocciaMatchExpression(AExpr).Clauses[I].Expression);
    end;
    ValidateDynamicFunctionSuperExpression(
      TGocciaMatchExpression(AExpr).DefaultExpression);
  end;
end;

procedure ValidateDynamicFunctionSuperStatement(
  const AStmt: TGocciaStatement);
var
  BlockStmt: TGocciaBlockStatement;
  CaseClause: TGocciaCaseClause;
  DoWhileStmt: TGocciaDoWhileStatement;
  ExpressionStmt: TGocciaExpressionStatement;
  ForInStmt: TGocciaForInStatement;
  ForOfStmt: TGocciaForOfStatement;
  ForStmt: TGocciaForStatement;
  IfStmt: TGocciaIfStatement;
  ReturnStmt: TGocciaReturnStatement;
  SwitchStmt: TGocciaSwitchStatement;
  ThrowStmt: TGocciaThrowStatement;
  TryStmt: TGocciaTryStatement;
  UsingDecl: TGocciaUsingDeclaration;
  VarDecl: TGocciaVariableDeclaration;
  WhileStmt: TGocciaWhileStatement;
  WithStmt: TGocciaWithStatement;
  I, J: Integer;
begin
  if not Assigned(AStmt) then
    Exit;
  if AStmt is TGocciaExpressionStatement then
  begin
    ExpressionStmt := TGocciaExpressionStatement(AStmt);
    ValidateDynamicFunctionSuperExpression(ExpressionStmt.Expression);
  end
  else if AStmt is TGocciaBlockStatement then
  begin
    BlockStmt := TGocciaBlockStatement(AStmt);
    for I := 0 to BlockStmt.Nodes.Count - 1 do
      if BlockStmt.Nodes[I] is TGocciaStatement then
        ValidateDynamicFunctionSuperStatement(TGocciaStatement(BlockStmt.Nodes[I]))
      else if BlockStmt.Nodes[I] is TGocciaExpression then
        ValidateDynamicFunctionSuperExpression(TGocciaExpression(BlockStmt.Nodes[I]));
  end
  else if AStmt is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(AStmt);
    for I := 0 to High(VarDecl.Variables) do
    begin
      if VarDecl.Variables[I].IsPattern or Assigned(VarDecl.Variables[I].Pattern) then
        ValidateDynamicFunctionSuperPattern(VarDecl.Variables[I].Pattern);
      ValidateDynamicFunctionSuperExpression(VarDecl.Variables[I].Initializer);
    end;
  end
  else if AStmt is TGocciaDestructuringDeclaration then
  begin
    ValidateDynamicFunctionSuperPattern(
      TGocciaDestructuringDeclaration(AStmt).Pattern);
    ValidateDynamicFunctionSuperExpression(
      TGocciaDestructuringDeclaration(AStmt).Initializer);
  end
  else if AStmt is TGocciaFunctionDeclaration then
    ValidateDynamicFunctionSuperFunction(
      TGocciaFunctionDeclaration(AStmt).FunctionExpression)
  else if AStmt is TGocciaIfStatement then
  begin
    IfStmt := TGocciaIfStatement(AStmt);
    ValidateDynamicFunctionSuperExpression(IfStmt.Condition);
    ValidateDynamicFunctionSuperStatement(IfStmt.Consequent);
    ValidateDynamicFunctionSuperStatement(IfStmt.Alternate);
  end
  else if AStmt is TGocciaReturnStatement then
  begin
    ReturnStmt := TGocciaReturnStatement(AStmt);
    ValidateDynamicFunctionSuperExpression(ReturnStmt.Value);
  end
  else if AStmt is TGocciaThrowStatement then
  begin
    ThrowStmt := TGocciaThrowStatement(AStmt);
    ValidateDynamicFunctionSuperExpression(ThrowStmt.Value);
  end
  else if AStmt is TGocciaForStatement then
  begin
    ForStmt := TGocciaForStatement(AStmt);
    ValidateDynamicFunctionSuperStatement(ForStmt.Init);
    ValidateDynamicFunctionSuperExpression(ForStmt.Condition);
    ValidateDynamicFunctionSuperExpression(ForStmt.Update);
    ValidateDynamicFunctionSuperStatement(ForStmt.Body);
  end
  else if AStmt is TGocciaWhileStatement then
  begin
    WhileStmt := TGocciaWhileStatement(AStmt);
    ValidateDynamicFunctionSuperExpression(WhileStmt.Condition);
    ValidateDynamicFunctionSuperStatement(WhileStmt.Body);
  end
  else if AStmt is TGocciaDoWhileStatement then
  begin
    DoWhileStmt := TGocciaDoWhileStatement(AStmt);
    ValidateDynamicFunctionSuperStatement(DoWhileStmt.Body);
    ValidateDynamicFunctionSuperExpression(DoWhileStmt.Condition);
  end
  else if AStmt is TGocciaWithStatement then
  begin
    WithStmt := TGocciaWithStatement(AStmt);
    ValidateDynamicFunctionSuperExpression(WithStmt.ObjectExpression);
    ValidateDynamicFunctionSuperStatement(WithStmt.Body);
  end
  else if AStmt is TGocciaForOfStatement then
  begin
    ForOfStmt := TGocciaForOfStatement(AStmt);
    ValidateDynamicFunctionSuperPattern(ForOfStmt.BindingPattern);
    ValidateDynamicFunctionSuperPattern(ForOfStmt.AssignmentTarget);
    ValidateDynamicFunctionSuperMatchPattern(ForOfStmt.MatchPattern);
    ValidateDynamicFunctionSuperExpression(ForOfStmt.Iterable);
    ValidateDynamicFunctionSuperStatement(ForOfStmt.Body);
  end
  else if AStmt is TGocciaForInStatement then
  begin
    ForInStmt := TGocciaForInStatement(AStmt);
    ValidateDynamicFunctionSuperPattern(ForInStmt.BindingPattern);
    ValidateDynamicFunctionSuperPattern(ForInStmt.AssignmentTarget);
    ValidateDynamicFunctionSuperExpression(ForInStmt.ObjectExpression);
    ValidateDynamicFunctionSuperStatement(ForInStmt.Body);
  end
  else if AStmt is TGocciaTryStatement then
  begin
    TryStmt := TGocciaTryStatement(AStmt);
    ValidateDynamicFunctionSuperStatement(TryStmt.Block);
    ValidateDynamicFunctionSuperMatchPattern(TryStmt.CatchPattern);
    ValidateDynamicFunctionSuperPattern(TryStmt.CatchBindingPattern);
    ValidateDynamicFunctionSuperStatement(TryStmt.CatchBlock);
    ValidateDynamicFunctionSuperStatement(TryStmt.FinallyBlock);
  end
  else if AStmt is TGocciaSwitchStatement then
  begin
    SwitchStmt := TGocciaSwitchStatement(AStmt);
    ValidateDynamicFunctionSuperExpression(SwitchStmt.Discriminant);
    for I := 0 to SwitchStmt.Cases.Count - 1 do
    begin
      CaseClause := SwitchStmt.Cases[I];
      ValidateDynamicFunctionSuperExpression(CaseClause.Test);
      for J := 0 to CaseClause.Consequent.Count - 1 do
        ValidateDynamicFunctionSuperStatement(CaseClause.Consequent[J]);
    end;
  end
  else if AStmt is TGocciaUsingDeclaration then
  begin
    UsingDecl := TGocciaUsingDeclaration(AStmt);
    for I := 0 to High(UsingDecl.Variables) do
      ValidateDynamicFunctionSuperExpression(
        UsingDecl.Variables[I].Initializer);
  end;
end;

// The Function constructor's code is profiled as a compile unit of its own,
// not as functions of the caller's file. Its run creates only the function.
function TGocciaEngine.CompileDynamicFunction(
  const AParamsSources: array of string;
  const ABodySource: string;
  const AKind: TGocciaDynamicFunctionKind): TGocciaFunctionBase;
var
  CompileProfile: TGocciaCompileProfiler;
  UnitDepth, I: Integer;
  ParamStr, ProfileSource: string;
begin
  ParamStr := '';
  for I := 0 to High(AParamsSources) do
  begin
    if I > 0 then
      ParamStr := ParamStr + ',';
    ParamStr := ParamStr + AParamsSources[I];
  end;
  CompileProfile := TGocciaCompileProfiler.Instance;
  if not Assigned(CompileProfile) then
    Exit(CompileDynamicFunctionCode(ParamStr, ABodySource, AKind, nil, ''));
  UnitDepth := CompileProfile.UnitDepth;
  // The kind, the parameters as the wrapper joins them, and the body: the
  // same function source is the same unit, whatever its arguments' split.
  // The parameters are length-prefixed: no character they or the body may
  // hold can move text from one field to the other.
  ProfileSource := IntToStr(Ord(AKind)) + ':' + IntToStr(Length(ParamStr)) +
    ':' + ParamStr + ABodySource;
  CompileProfile.BeginUnit('<Function>', ProfileSource);
  try
    Result := CompileDynamicFunctionCode(ParamStr, ABodySource, AKind,
      CompileProfile, ProfileSource);
  finally
    CompileProfile.EndUnitTo(UnitDepth);
  end;
end;

{ ACompileProfile, when the compile profile is on, gets the parameter and
  body validation parses as units of their own beside the wrapper's
  (AProfileSource identifies them): their positions are not the wrapper's. }
function TGocciaEngine.CompileDynamicFunctionCode(const AParamStr,
  ABodySource: string; const AKind: TGocciaDynamicFunctionKind;
  const ACompileProfile: TGocciaCompileProfiler;
  const AProfileSource: string): TGocciaFunctionBase;
var
  UnitDepth: Integer;
  BodyParseResult: TGocciaFunctionBodyParseResult;
  FunctionExpression: TGocciaFunctionExpression;
  PipelineOptions: TGocciaSourcePipelineOptions;
  WrapperOptions: TGocciaSourcePipelineOptions;
  ProgramNode: TGocciaProgram;
  ResultValue: TGocciaValue;
begin
  // ES2026 §20.2.1.1.1 CreateDynamicFunction: parse the requested dynamic
  // function kind as function anonymous(P) { body }.
  //
  // Params and body are parsed as separate, self-contained programs first.
  // This prevents injection: a crafted body that tries to close the method
  // wrapper will fail validation because it will have unmatched braces when
  // parsed as a standalone function body.
  // Validate params: must parse as a valid parameter list.
  // If params contain tokens that escape the signature, this will throw.
  PipelineOptions := TGocciaSourcePipeline.DefaultOptions;
  PipelineOptions.Preprocessors := [];
  PipelineOptions.Compatibility := FCompatibility + [cfFunction];
  PipelineOptions.LabelStatementsEnabled := FLabelStatementsEnabled;
  PipelineOptions.ForInLoopsEnabled := FForInLoopsEnabled;
  PipelineOptions.ExperimentalJSModuleSourceEnabled :=
    FExperimentalJSModuleSourceEnabled;
  PipelineOptions.WarningUnsupportedFeatures := FWarningUnsupportedFeatures;
  PipelineOptions.SourceType := stScript;
  // A raise leaves these units open; CompileDynamicFunction closes them.
  UnitDepth := -1;
  if Assigned(ACompileProfile) then
  begin
    UnitDepth := ACompileProfile.UnitDepth;
    ACompileProfile.BeginUnit('<Function:params>', AProfileSource);
  end;
  if not TGocciaSourcePipeline.ParseFunctionParameters(AParamStr,
    '<Function:params>', PipelineOptions, AKind) then
    ThrowSyntaxError('Invalid parameter list for Function constructor');

  // Validate body: must parse as a valid function body.
  // An arrow wrapper ensures the body is enclosed in matching braces.
  if Assigned(ACompileProfile) then
  begin
    ACompileProfile.EndUnitTo(UnitDepth);
    ACompileProfile.BeginUnit('<Function:body>', AProfileSource);
  end;
  BodyParseResult := TGocciaSourcePipeline.ParseFunctionBodyWrapper(
    ABodySource, '<Function:body>', PipelineOptions, AKind);
  if not BodyParseResult.IsValid then
    ThrowSyntaxError('Invalid body for Function constructor');
  if Assigned(ACompileProfile) then
    ACompileProfile.EndUnitTo(UnitDepth);

  // Both pieces validated — safe to assemble the wrapper
  WrapperOptions := PipelineOptions;
  if BodyParseResult.HasUseStrictDirective then
    WrapperOptions.InheritedStrictMode := True;
  ProgramNode := TGocciaSourcePipeline.ParseDynamicFunctionWrapper(AParamStr,
    ABodySource, '<Function>', WrapperOptions, AKind);
  try
    FunctionExpression := nil;
    if (ProgramNode.Body.Count = 1) and
       (ProgramNode.Body[0] is TGocciaExpressionStatement) and
       (TGocciaExpressionStatement(ProgramNode.Body[0]).Expression is
       TGocciaFunctionExpression) then
      FunctionExpression := TGocciaFunctionExpression(
        TGocciaExpressionStatement(ProgramNode.Body[0]).Expression);

    if Assigned(FunctionExpression) and
       ((AKind = dfkAsync) or (AKind = dfkAsyncGenerator)) and
       DynamicParametersContainAwait(FunctionExpression.Parameters) then
      ThrowSyntaxError('await is not allowed in dynamic async function parameters');
    if Assigned(FunctionExpression) and
       ((AKind = dfkGenerator) or (AKind = dfkAsyncGenerator)) and
       DynamicParametersContainYield(FunctionExpression.Parameters) then
      ThrowSyntaxError('yield is not allowed in dynamic generator function parameters');

    if BodyParseResult.HasUseStrictDirective and
       Assigned(FunctionExpression) then
      ValidateStrictFunctionEarlyErrors(FunctionExpression);
    if Assigned(FunctionExpression) then
      ValidateDynamicFunctionSuperFunction(FunctionExpression);

    ResultValue := ExecuteDynamicFunction(ProgramNode);
    Result := TGocciaFunctionBase(ResultValue);
    // ES2026 §15.2.2.4: Function-constructor bodies are non-strict
    // unless the parsed body contains a Use Strict Directive (§11.2.2).
    if not BodyParseResult.HasUseStrictDirective then
    begin
      Result.StrictThis := False;
      Result.StrictCode := False;
    end;
  finally
    ProgramNode.Free;
  end;
end;

initialization
  GPromiseWaitHostWork := @RunRunningEngineWorkForPromiseWait;

end.
