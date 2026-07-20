unit Goccia.Engine;

{$I Goccia.inc}

interface

uses
  Classes,
  Contnrs,
  Generics.Collections,

  OrderedStringMap,

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
  Goccia.CapabilityAudit,
  Goccia.Constants,
  Goccia.Evaluator,
  Goccia.Evaluator.Context,
  Goccia.ExecutionContext,
  Goccia.Executor,
  Goccia.HostEnvironment,
  Goccia.Interpreter,
  Goccia.JSON,
  Goccia.Keywords.Reserved,
  Goccia.Modules,
  Goccia.Modules.ContentProvider,
  Goccia.Modules.Loader,
  Goccia.Modules.Resolver,
  Goccia.ObjectModel,
  Goccia.ObjectModel.Engine,
  Goccia.Realm,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
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
  Goccia.Values.TypedArrayValue;

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
    procedure WaitForIdle; virtual;
    procedure DiscardPending; virtual;
    procedure SetAllowedFetchHosts(const AHosts: TStrings); virtual;
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

  TGocciaEngine = class
  public
    const DefaultPreprocessors: TGocciaPreprocessors = [ppJSX];
    const DefaultCompatibility: TGocciaCompatibilityFlags = [];
    const DefaultWarningUnsupportedFeatures = False;
    const DefaultSourceType: TGocciaSourceType = stScript;
  private
    FInterpreter: TGocciaInterpreter;
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
    FExecutor: TGocciaExecutor;
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
    FLastTiming: TGocciaScriptResult;
    FLastSourceMap: TGocciaSourceMap;
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
    procedure RegisterTypedArrayConstructor(const AName: string; const AKind: TGocciaTypedArrayKind; const AObjectConstructor: TGocciaClassValue);
    procedure RegisterGlobalThis;
    procedure RegisterGocciaScriptGlobal;
    procedure Initialize(const AFileName: string; const ASourceLines: TStringList;
      const AModuleLoader: TGocciaModuleLoader;
      const AOwnsModuleLoader: Boolean);
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
    procedure DoRetainModule(const AModule: TObject);
    procedure DiscardRuntimePending;
    procedure PrintSourcePipelineWarnings(
      const APipelineResult: TGocciaSourcePipelineResult);
    function CompileDynamicFunction(const AParamsSources: array of string;
      const ABodySource: string;
      const AKind: TGocciaDynamicFunctionKind): TGocciaFunctionBase;
    procedure InjectModulesFromValue(const AValue: TGocciaValue;
      const ABaseAddress, AProvenance: string);
  public
    { Callers always provide an explicit executor — no implicit interpreter
      fallback. Pair an executor with the engine that owns it; the engine
      does not free executors. }
    constructor Create(const AFileName: string;
      const ASourceLines: TStringList;
      const AExecutor: TGocciaExecutor); overload;
    constructor Create(const AFileName: string;
      const ASourceLines: TStringList;
      const AResolver: TGocciaModuleResolver;
      const AExecutor: TGocciaExecutor); overload;
    constructor Create(const AFileName: string;
      const ASourceLines: TStringList;
      const AModuleLoader: TGocciaModuleLoader;
      const AExecutor: TGocciaExecutor); overload;
    destructor Destroy; override;

    function Execute: TGocciaScriptResult;
    function ExecuteProgram(const AProgram: TGocciaProgram): TGocciaValue;
    procedure WaitForRuntimeIdle;
    function CompileModule(
      const AProgram: TGocciaProgram): TGocciaCompiledModule;
    procedure RetainModule(const AModule: TGocciaCompiledModule);
    function RunModule(
      const AModule: TGocciaCompiledModule): TGocciaValue;
    function RunModuleInScope(const AModule: TGocciaCompiledModule;
      const AScope: TGocciaScope): TGocciaValue;
    function RunModuleForSourceType(const AModule: TGocciaCompiledModule;
      const AFileName: string): TGocciaValue;
    procedure ThrowError(const AMessage: string; const ALine,
      AColumn: Integer);

    procedure AddAlias(const APattern, AReplacement: string);
    procedure SetAllowedFetchHosts(const AHosts: TStrings);
    procedure EmitCapabilityAudit(const AKind: TGocciaCapabilityKind;
      const ADecision: TGocciaCapabilityDecision;
      const ASubject, AReason: string);
    procedure ConfigureCapabilityAuditAsChildOf(
      const AParent: TGocciaEngine);
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
    function AddExtension(
      const AExtension: TGocciaEngineExtension): TGocciaEngineExtension;
    function FindExtension(
      const AClass: TGocciaEngineExtensionClass): TGocciaEngineExtension;

    class function RunScript(const ASource: string; const AFileName: string = 'inline.goccia'): TGocciaScriptResult; overload;
    class function RunScriptFromStringList(const ASource: TStringList; const AFileName: string): TGocciaScriptResult; overload;

    property Executor: TGocciaExecutor read FExecutor;
    property Interpreter: TGocciaInterpreter read FInterpreter;
    property Resolver: TGocciaModuleResolver read GetResolver;
    property ContentProvider: TGocciaModuleContentProvider read GetContentProvider;
    property ModuleResolver: TGocciaModuleResolver read GetModuleResolver;
    property ModuleLoader: TGocciaModuleLoader read FModuleLoader;
    property HostEnvironment: TGocciaHostEnvironment read FHostEnvironment;
    property CapabilityAuditSink: TGocciaCapabilityAuditSink
      read FCapabilityAuditSink write FCapabilityAuditSink;
    property SourcePath: string read FSourcePath;
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
    property SuppressWarnings: Boolean read FSuppressWarnings write FSuppressWarnings;
    property LastTiming: TGocciaScriptResult read FLastTiming;
    // Source map from the most recent source pipeline run, if any.
    // Ownership transfers to the caller when read (set to nil after access).
    function TakeLastSourceMap: TGocciaSourceMap;
  end;


implementation

uses
  Math,
  SysUtils,

  TextSemantics,
  TimingUtils,

  Goccia.CallStack,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Coverage,
  Goccia.Error,
  Goccia.Execution.CallSite,
  Goccia.Executor.Bytecode,
  Goccia.Executor.Interpreter,
  Goccia.FileExtensions,
  Goccia.FloatingPoint,
  Goccia.GarbageCollector,
  Goccia.ImportMeta,
  Goccia.Keywords.Contextual,
  Goccia.MicrotaskQueue,
  Goccia.Platform,
  Goccia.Scope.Redeclaration,
  Goccia.Shims,
  Goccia.Spec,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.ArrayValue,
  Goccia.Values.BooleanObjectValue,
  Goccia.Values.DataViewValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FinalizationRegistryValue,
  Goccia.Values.FunctionValue,
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

{ TGocciaEngineExtension }

procedure TGocciaEngineExtension.WaitForIdle;
begin
end;

procedure TGocciaEngineExtension.DiscardPending;
begin
end;

procedure TGocciaEngineExtension.SetAllowedFetchHosts(const AHosts: TStrings);
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
  if FInterpreter.GlobalScope.ContainsOwnLexicalBinding(AName) then
  begin
    if FInjectedGlobals.IndexOf(AName) >= 0 then
      FInterpreter.GlobalScope.ForceUpdateBinding(AName, AValue)
    else
      ThrowError(
        'Cannot override built-in global "' + AName + '" via globals injection.',
        0, 0);
  end
  else
  begin
    FInterpreter.GlobalScope.DefineLexicalBinding(AName, AValue, dtConst);
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
  FInterpreter.GlobalScope.MarkGlobalObjectBackedBinding(AName);
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
begin
  Module := FModuleLoader.LoadModule(APath, FSourcePath);
  ExportNames := Module.GetExportNames;
  for ExportName in ExportNames do
    if Module.TryGetExportValue(ExportName, ExportValue) then
      RegisterGlobal(ExportName, ExportValue);
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
begin
  Module := FModuleLoader.LoadModule(APath, FSourcePath);
  if not Module.TryGetExportValue(KEYWORD_DEFAULT, DefaultValue) then
    raise EArgumentException.Create(
      'Virtual modules manifest module must have a default export.');
  InjectModulesFromValue(DefaultValue, Module.Path, Module.Path);
end;

procedure TGocciaEngine.ClearTransientCaches;
begin
  if Assigned(FExecutor) then
    FExecutor.ClearTransientCaches;
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

constructor TGocciaEngine.Create(const AFileName: string;
  const ASourceLines: TStringList; const AExecutor: TGocciaExecutor);
begin
  if not Assigned(AExecutor) then
    raise Exception.Create('TGocciaEngine.Create: AExecutor is required.');
  FExecutor := AExecutor;
  Initialize(AFileName, ASourceLines, TGocciaModuleLoader.Create(AFileName),
    True);
end;

constructor TGocciaEngine.Create(const AFileName: string;
  const ASourceLines: TStringList;
  const AResolver: TGocciaModuleResolver;
  const AExecutor: TGocciaExecutor);
begin
  if not Assigned(AExecutor) then
    raise Exception.Create('TGocciaEngine.Create: AExecutor is required.');
  FExecutor := AExecutor;
  Initialize(AFileName, ASourceLines,
    TGocciaModuleLoader.Create(AFileName, AResolver), True);
end;

constructor TGocciaEngine.Create(const AFileName: string;
  const ASourceLines: TStringList; const AModuleLoader: TGocciaModuleLoader;
  const AExecutor: TGocciaExecutor);
begin
  if not Assigned(AExecutor) then
    raise Exception.Create('TGocciaEngine.Create: AExecutor is required.');
  FExecutor := AExecutor;
  Initialize(AFileName, ASourceLines, AModuleLoader, False);
end;

procedure TGocciaEngine.ExecuteShims;
var
  I: Integer;
  Shim: TGocciaShimDefinition;
  Materializer: TGocciaShimMaterializer;
begin
  for I := 0 to DefaultShimCount - 1 do
  begin
    Shim := DefaultShim(I);
    if IsSideEffectShim(Shim.Name) then
      // Mutates Object.prototype with no exported global to bind lazily, so it
      // runs eagerly at boot.
      LoadShimValue(FInterpreter, Shim)
    else
    begin
      // Defer the name-bound shim's lex/parse/tree-walk until the global is
      // first touched.  The heaviest shim (Date, ~671 source lines) is then
      // never parsed for scripts that don't use it.
      Materializer := TGocciaShimMaterializer.Create(FInterpreter, Shim);
      FLazyThunks.Add(Materializer);
      RegisterLazyGlobal(Shim.Name, Materializer.Materialize, dtConst);
    end;
  end;
end;

procedure TGocciaEngine.Initialize(const AFileName: string;
  const ASourceLines: TStringList; const AModuleLoader: TGocciaModuleLoader;
  const AOwnsModuleLoader: Boolean);
begin
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

  FInterpreter := TGocciaInterpreter.Create(AFileName, ASourceLines,
    FModuleLoader);
  FInterpreter.Realm := FRealm;
  FInterpreter.JSXEnabled := ppJSX in FPreprocessors;
  FRealm.GlobalEnv := FInterpreter.GlobalScope;
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
    CreateExecutionContext(FRealm, FInterpreter.GlobalScope, AFileName));

  TGarbageCollector.Instance.AddRootObject(FInterpreter.GlobalScope);

  FInjectedGlobals := TStringList.Create;
  // Owns the lazy materializers (shims, runtime namespaces) whose factory
  // closures back lazy global properties; freed in the destructor.
  FLazyThunks := Contnrs.TObjectList.Create(True);
  RegisterDefaultShimNames(FShims);
  PinSingletons;
  RegisterBuiltIns;
  ExecuteShims;

  // The executor was provided by the caller via the constructor.  When the
  // caller chose interpreter mode, bind the bootstrapped interpreter into
  // it now so its tree-walk methods can dispatch.  The bytecode executor
  // does not need this hook.
  FRetainedModules := Contnrs.TObjectList.Create(True);

  if FExecutor is TGocciaInterpreterExecutor then
    TGocciaInterpreterExecutor(FExecutor).Interpreter := FInterpreter;
  FExecutor.RetainModuleCallback := DoRetainModule;
  FExecutor.Initialize(FInterpreter.GlobalScope, FModuleLoader, AFileName);

  if Assigned(FFunctionConstructor) then
  begin
    FFunctionConstructor.CompileDynamicFunction := CompileDynamicFunction;
    FFunctionConstructor.CapabilityAuditEmitter := EmitCapabilityAudit;
  end;
end;

destructor TGocciaEngine.Destroy;
begin
  if (TGarbageCollector.Instance <> nil) and Assigned(FInterpreter) then
    TGarbageCollector.Instance.RemoveRootObject(FInterpreter.GlobalScope);

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
    FInterpreter.Free;
    if FOwnsModuleLoader then
      FModuleLoader.Free;

    // Drop the realm only after every built-in (and the interpreter, which
    // holds the global scope) has been freed - those owners may still touch
    // prototype objects during teardown.  Freeing the realm unpins the
    // intrinsic prototype graph so the GC can collect it before the next
    // engine on this worker thread starts up.
    if Assigned(FRealm) then
    begin
      FRealmExecutionContext.Free;
      FRealmExecutionContext := nil;
      FRealm.Free;
      FRealm := nil;
  end;
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
  Scope := FInterpreter.GlobalScope;

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
  FInterpreter.GlobalScope.DefineLexicalBinding(AName,
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
      FInterpreter.GlobalScope, ThrowError, FHostEnvironment, False);
  Result := FBuiltinTemporal.TemporalNamespace;
end;

function TGocciaEngine.MaterializeIntlGlobal: TGocciaValue;
begin
  if not Assigned(FBuiltinIntl) then
  begin
    FBuiltinIntl := TGocciaIntlBuiltin.Create('Intl', FInterpreter.GlobalScope,
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
    if not FInterpreter.GlobalScope.ContainsOwnLexicalBinding(
      '__GocciaIntlNumberFormat') then
      FInterpreter.GlobalScope.DefineLexicalBinding('__GocciaIntlNumberFormat',
        FBuiltinIntl.IntlNamespace.GetProperty('NumberFormat'), dtConst, True);
    if not FInterpreter.GlobalScope.ContainsOwnLexicalBinding(
      '__GocciaIntlDateTimeFormat') then
      FInterpreter.GlobalScope.DefineLexicalBinding('__GocciaIntlDateTimeFormat',
        FBuiltinIntl.IntlNamespace.GetProperty('DateTimeFormat'), dtConst, True);
  end;
  Result := FBuiltinIntl.IntlNamespace;
end;

function TGocciaEngine.MaterializeAtomicsGlobal: TGocciaValue;
begin
  if not Assigned(FBuiltinAtomics) then
    FBuiltinAtomics := TGocciaAtomics.Create(CONSTRUCTOR_ATOMICS,
      FInterpreter.GlobalScope, ThrowError, False);
  Result := FBuiltinAtomics.BuiltinObject;
end;

function TGocciaEngine.MaterializeProxyGlobal: TGocciaValue;
begin
  if not Assigned(FBuiltinProxy) then
    FBuiltinProxy := TGocciaGlobalProxy.Create(FInterpreter.GlobalScope, False);
  Result := FBuiltinProxy.ConstructorValue;
end;

function TGocciaEngine.MaterializeReflectGlobal: TGocciaValue;
begin
  if not Assigned(FBuiltinReflect) then
    FBuiltinReflect := TGocciaGlobalReflect.Create('Reflect',
      FInterpreter.GlobalScope, ThrowError, False);
  Result := FBuiltinReflect.BuiltinObject;
end;

function TGocciaEngine.MaterializeDisposableStackGlobal: TGocciaValue;
begin
  if not Assigned(FBuiltinDisposableStack) then
    FBuiltinDisposableStack := TGocciaBuiltinDisposableStack.Create(
      CONSTRUCTOR_DISPOSABLE_STACK, FInterpreter.GlobalScope, ThrowError,
      False);
  Result := FBuiltinDisposableStack.DisposableStackConstructorValue;
end;

function TGocciaEngine.MaterializeAsyncDisposableStackGlobal: TGocciaValue;
begin
  if not Assigned(FBuiltinDisposableStack) then
    FBuiltinDisposableStack := TGocciaBuiltinDisposableStack.Create(
      CONSTRUCTOR_DISPOSABLE_STACK, FInterpreter.GlobalScope, ThrowError,
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
begin
  TGocciaObjectValue.InitializeSharedPrototype;
  TypeDef.ConstructorName := CONSTRUCTOR_OBJECT;
  TypeDef.Kind := gtdkNativeInstanceType;
  TypeDef.ClassValueClass := TGocciaClassValue;
  TypeDef.ExposePrototype := nil;
  TypeDef.PrototypeProvider := @ObjectPrototypeProvider;
  TypeDef.StaticSource := BuiltinObjectOrNil(FBuiltinGlobalObject);
  TypeDef.PrototypeParent := nil;
  TypeDef.AddSpeciesGetter := False;
  RegisterTypeDefinition(FInterpreter.GlobalScope, TypeDef, SpeciesGetter, ObjectConstructor);
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
  RegisterTypeDefinition(FInterpreter.GlobalScope, TypeDef, SpeciesGetter, GenericConstructor);
  ArrayConstructor := TGocciaArrayClassValue(GenericConstructor);

  TypeDef.ConstructorName := CONSTRUCTOR_MAP;
  TypeDef.Kind := gtdkCollectionLikeNativeType;
  TypeDef.ClassValueClass := TGocciaMapClassValue;
  TypeDef.ExposePrototype := @ExposeMapPrototype;
  TypeDef.PrototypeProvider := nil;
  TypeDef.StaticSource := BuiltinObjectOrNil(FBuiltinMap);
  TypeDef.PrototypeParent := ObjectConstructor.Prototype;
  TypeDef.AddSpeciesGetter := True;
  RegisterTypeDefinition(FInterpreter.GlobalScope, TypeDef, SpeciesGetter, GenericConstructor);
  MapConstructor := TGocciaMapClassValue(GenericConstructor);

  TypeDef.ConstructorName := CONSTRUCTOR_SET;
  TypeDef.Kind := gtdkCollectionLikeNativeType;
  TypeDef.ClassValueClass := TGocciaSetClassValue;
  TypeDef.ExposePrototype := @ExposeSetPrototype;
  TypeDef.PrototypeProvider := nil;
  TypeDef.StaticSource := nil;
  TypeDef.PrototypeParent := ObjectConstructor.Prototype;
  TypeDef.AddSpeciesGetter := True;
  RegisterTypeDefinition(FInterpreter.GlobalScope, TypeDef, SpeciesGetter, GenericConstructor);
  SetConstructor := TGocciaSetClassValue(GenericConstructor);

  TypeDef.ConstructorName := CONSTRUCTOR_WEAK_MAP;
  TypeDef.Kind := gtdkCollectionLikeNativeType;
  TypeDef.ClassValueClass := TGocciaWeakMapClassValue;
  TypeDef.ExposePrototype := @ExposeWeakMapPrototype;
  TypeDef.PrototypeProvider := nil;
  TypeDef.StaticSource := nil;
  TypeDef.PrototypeParent := ObjectConstructor.Prototype;
  TypeDef.AddSpeciesGetter := False;
  RegisterTypeDefinition(FInterpreter.GlobalScope, TypeDef, SpeciesGetter, GenericConstructor);
  WeakMapConstructor := TGocciaWeakMapClassValue(GenericConstructor);

  TypeDef.ConstructorName := CONSTRUCTOR_WEAK_SET;
  TypeDef.Kind := gtdkCollectionLikeNativeType;
  TypeDef.ClassValueClass := TGocciaWeakSetClassValue;
  TypeDef.ExposePrototype := @ExposeWeakSetPrototype;
  TypeDef.PrototypeProvider := nil;
  TypeDef.StaticSource := nil;
  TypeDef.PrototypeParent := ObjectConstructor.Prototype;
  TypeDef.AddSpeciesGetter := False;
  RegisterTypeDefinition(FInterpreter.GlobalScope, TypeDef, SpeciesGetter, GenericConstructor);
  WeakSetConstructor := TGocciaWeakSetClassValue(GenericConstructor);

  TypeDef.ConstructorName := CONSTRUCTOR_WEAK_REF;
  TypeDef.Kind := gtdkNativeInstanceType;
  TypeDef.ClassValueClass := TGocciaWeakRefClassValue;
  TypeDef.ExposePrototype := @ExposeWeakRefPrototype;
  TypeDef.PrototypeProvider := nil;
  TypeDef.StaticSource := nil;
  TypeDef.PrototypeParent := ObjectConstructor.Prototype;
  TypeDef.AddSpeciesGetter := False;
  RegisterTypeDefinition(FInterpreter.GlobalScope, TypeDef, SpeciesGetter, GenericConstructor);
  WeakRefConstructor := TGocciaWeakRefClassValue(GenericConstructor);

  TypeDef.ConstructorName := CONSTRUCTOR_FINALIZATION_REGISTRY;
  TypeDef.Kind := gtdkNativeInstanceType;
  TypeDef.ClassValueClass := TGocciaFinalizationRegistryClassValue;
  TypeDef.ExposePrototype := @ExposeFinalizationRegistryPrototype;
  TypeDef.PrototypeProvider := nil;
  TypeDef.StaticSource := nil;
  TypeDef.PrototypeParent := ObjectConstructor.Prototype;
  TypeDef.AddSpeciesGetter := False;
  RegisterTypeDefinition(FInterpreter.GlobalScope, TypeDef, SpeciesGetter, GenericConstructor);
  FinalizationRegistryConstructor :=
    TGocciaFinalizationRegistryClassValue(GenericConstructor);

  ArrayBufferConstructor := TGocciaArrayBufferClassValue.Create(CONSTRUCTOR_ARRAY_BUFFER, nil);
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
  FInterpreter.GlobalScope.DefineLexicalBinding(CONSTRUCTOR_ARRAY_BUFFER, ArrayBufferConstructor, dtConst, True);

  SharedArrayBufferConstructor := TGocciaSharedArrayBufferClassValue.Create(CONSTRUCTOR_SHARED_ARRAY_BUFFER, nil);
  TGocciaSharedArrayBufferValue.ExposePrototype(SharedArrayBufferConstructor);
  SharedArrayBufferConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
  SharedArrayBufferConstructor.DefineSymbolProperty(
    TGocciaSymbolValue.WellKnownSpecies,
    TGocciaPropertyDescriptorAccessor.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(
        SpeciesGetter, 'get [Symbol.species]', 0),
      nil, [pfConfigurable]));
  FInterpreter.GlobalScope.DefineLexicalBinding(CONSTRUCTOR_SHARED_ARRAY_BUFFER, SharedArrayBufferConstructor, dtConst, True);

  DataViewConstructor := TGocciaDataViewClassValue.Create(CONSTRUCTOR_DATA_VIEW, nil);
  TGocciaDataViewValue.ExposePrototype(DataViewConstructor);
  DataViewConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
  FInterpreter.GlobalScope.DefineLexicalBinding(CONSTRUCTOR_DATA_VIEW, DataViewConstructor, dtConst, True);

  // Create %TypedArray% intrinsic (not globally exposed per spec §23.2.1)
  FTypedArrayIntrinsic := TGocciaTypedArrayIntrinsicClassValue.Create('TypedArray', nil);
  // §17: built-in name/length are {writable: false, enumerable: false, configurable: true}
  FTypedArrayIntrinsic.DefineProperty(PROP_NAME,
    TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create('TypedArray'), [pfConfigurable]));
  FTypedArrayIntrinsic.DefineProperty(PROP_LENGTH,
    TGocciaPropertyDescriptorData.Create(TGocciaNumberLiteralValue.Create(0), [pfConfigurable]));
  TypedArrayStatic := TGocciaTypedArrayStaticFrom.Create;
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
  RegisterTypeDefinition(FInterpreter.GlobalScope, TypeDef, SpeciesGetter, GenericConstructor);
  StringConstructor := TGocciaStringClassValue(GenericConstructor);

  FBuiltinRegExp := TGocciaGlobalRegExp.Create(CONSTRUCTOR_REGEXP,
    FInterpreter.GlobalScope, ThrowError, ObjectConstructor.Prototype);

  TypeDef.ConstructorName := CONSTRUCTOR_NUMBER;
  TypeDef.Kind := gtdkPrimitiveWrapper;
  TypeDef.ClassValueClass := TGocciaNumberClassValue;
  TypeDef.ExposePrototype := nil;
  TypeDef.PrototypeProvider := @NumberPrototypeProvider;
  TypeDef.StaticSource := BuiltinObjectOrNil(FBuiltinGlobalNumber);
  TypeDef.PrototypeParent := ObjectConstructor.Prototype;
  TypeDef.AddSpeciesGetter := False;
  RegisterTypeDefinition(FInterpreter.GlobalScope, TypeDef, SpeciesGetter, GenericConstructor);
  NumberConstructor := TGocciaNumberClassValue(GenericConstructor);

  TypeDef.ConstructorName := CONSTRUCTOR_BOOLEAN;
  TypeDef.Kind := gtdkPrimitiveWrapper;
  TypeDef.ClassValueClass := TGocciaBooleanClassValue;
  TypeDef.ExposePrototype := nil;
  TypeDef.PrototypeProvider := @BooleanPrototypeProvider;
  TypeDef.StaticSource := nil;
  TypeDef.PrototypeParent := ObjectConstructor.Prototype;
  TypeDef.AddSpeciesGetter := False;
  RegisterTypeDefinition(FInterpreter.GlobalScope, TypeDef, SpeciesGetter, GenericConstructor);
  BooleanConstructor := TGocciaBooleanClassValue(GenericConstructor);

  FFunctionConstructor := TGocciaFunctionConstructorClassValue.Create('Function', nil);
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
  FInterpreter.GlobalScope.DefineLexicalBinding('Function', FunctionConstructor, dtConst, True);

  // ES2026 §20.4.3: Symbol.prototype's [[Prototype]] is %Object.prototype%
  if TGocciaSymbolValue.SharedPrototype <> nil then
    TGocciaObjectValue(TGocciaSymbolValue.SharedPrototype).Prototype := ObjectConstructor.Prototype;

  RegisterGocciaScriptGlobal;
  RegisterGlobalThis;
  RegisterLazyBuiltinGlobalProperties;
end;

procedure TGocciaEngine.RegisterTypedArrayConstructor(const AName: string; const AKind: TGocciaTypedArrayKind; const AObjectConstructor: TGocciaClassValue);
var
  TAConstructor: TGocciaTypedArrayClassValue;
  BPE: TGocciaNumberLiteralValue;
  Encoding: TGocciaUint8ArrayEncoding;
begin
  TAConstructor := TGocciaTypedArrayClassValue.Create(AName, FTypedArrayIntrinsic, AKind);
  TGocciaTypedArrayValue.ExposePrototype(TAConstructor);
  TGocciaTypedArrayValue.SetSharedPrototypeParent(AObjectConstructor.Prototype);
  BPE := TGocciaNumberLiteralValue.Create(TGocciaTypedArrayValue.BytesPerElement(AKind));
  TAConstructor.DefineProperty(PROP_BYTES_PER_ELEMENT,
    TGocciaPropertyDescriptorData.Create(BPE, []));
  TAConstructor.Prototype.DefineProperty(PROP_BYTES_PER_ELEMENT,
    TGocciaPropertyDescriptorData.Create(BPE, []));

  // Uint8Array-only: Base64/Hex encoding methods (TC39 Uint8Array Base64)
  if AKind = takUint8 then
  begin
    Encoding := TGocciaUint8ArrayEncoding.Create;

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

  FInterpreter.GlobalScope.DefineLexicalBinding(AName, TAConstructor, dtConst, True);
end;

procedure TGocciaEngine.RegisterGlobalThis;
var
  GlobalThisObj: TGocciaObjectValue;
  Scope: TGocciaScope;
  Name: string;
  Flags: TPropertyFlags;
begin
  Scope := FInterpreter.GlobalScope;
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
  Result := TGocciaExecutionContextScope.Create(
    CreateExecutionContext(FRealm, FInterpreter.GlobalScope, FSourcePath));
end;

procedure TGocciaEngine.RegisterGocciaScriptGlobal;
var
  GocciaObj: TGocciaObjectValue;
  BuildObj: TGocciaObjectValue;
  RuntimeGlobalsArray: TGocciaArrayValue;
  ShimsArray: TGocciaArrayValue;
  GCFunc: TGocciaNativeFunctionValue;
  I: Integer;
begin
  RuntimeGlobalsArray := TGocciaArrayValue.Create;

  BuildObj := TGocciaObjectValue.Create;
  BuildObj.DefineProperty('os', TGocciaPropertyDescriptorData.Create(
    TGocciaStringLiteralValue.Create(GetBuildOS), [pfEnumerable]));
  BuildObj.DefineProperty('arch', TGocciaPropertyDescriptorData.Create(
    TGocciaStringLiteralValue.Create(GetBuildArch), [pfEnumerable]));
  BuildObj.DefineProperty('date', TGocciaPropertyDescriptorData.Create(
    TGocciaStringLiteralValue.Create(GetBuildDate), [pfEnumerable]));

  ShimsArray := TGocciaArrayValue.Create;
  for I := 0 to FShims.Count - 1 do
    ShimsArray.Elements.Add(TGocciaStringLiteralValue.Create(FShims[I]));

  GocciaObj := TGocciaObjectValue.Create;
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
  FInterpreter.GlobalScope.DefineLexicalBinding(PROP_GOCCIA, FGocciaGlobal, dtConst, True);
end;

function TGocciaEngine.GetResolver: TGocciaModuleResolver;
begin
  Result := FModuleLoader.Resolver;
end;

procedure TGocciaEngine.AddAlias(const APattern, AReplacement: string);
begin
  Resolver.AddAlias(APattern, AReplacement);
end;

procedure TGocciaEngine.SetAllowedFetchHosts(const AHosts: TStrings);
var
  I: Integer;
begin
  for I := 0 to FExtensions.Count - 1 do
    FExtensions[I].SetAllowedFetchHosts(AHosts);
end;

procedure TGocciaEngine.EmitCapabilityAudit(
  const AKind: TGocciaCapabilityKind;
  const ADecision: TGocciaCapabilityDecision;
  const ASubject, AReason: string);
var
  AuditEvent: TGocciaCapabilityAuditEvent;
  CallSite: TGocciaCallSite;
begin
  if not Assigned(FCapabilityAuditSink) then
    Exit;

  AuditEvent.Kind := AKind;
  AuditEvent.Decision := ADecision;
  AuditEvent.Subject := ASubject;
  AuditEvent.Reason := AReason;
  if CurrentGocciaCallSite(CallSite) then
  begin
    AuditEvent.Source.FilePath := CallSite.FilePath;
    AuditEvent.Source.Line := CallSite.Line;
    AuditEvent.Source.Column := CallSite.Column;
  end
  else
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
  if Assigned(AParent) then
    FCapabilityAuditSink := AParent.FCapabilityAuditSink
  else
    FCapabilityAuditSink := nil;
end;

procedure TGocciaEngine.WaitForRuntimeIdle;
var
  I: Integer;
  Queue: TGocciaMicrotaskQueue;
  GC: TGarbageCollector;
begin
  try
    for I := 0 to FExtensions.Count - 1 do
      FExtensions[I].WaitForIdle;
    GC := TGarbageCollector.Instance;
    if Assigned(GC) then
      GC.ClearKeptObjects;
    Queue := TGocciaMicrotaskQueue.Instance;
    repeat
      if PumpAtomicsWaitAsyncCompletions = 0 then
      begin
        if Assigned(Queue) and Queue.HasPending then
          Queue.DrainQueue
        else
          Break;
      end
      else if Assigned(Queue) and Queue.HasPending then
        Queue.DrainQueue;
    until False;
  finally
    GC := TGarbageCollector.Instance;
    if Assigned(GC) then
      GC.ClearKeptObjects;
  end;
end;

procedure TGocciaEngine.DoRetainModule(const AModule: TObject);
begin
  FRetainedModules.Add(AModule);
end;

function TGocciaEngine.CompileModule(
  const AProgram: TGocciaProgram): TGocciaCompiledModule;
var
  SavedArgumentsObjectEnabled: Boolean;
  SavedNonStrictMode: Boolean;
  FloatingPointState: TGocciaFloatingPointState;
begin
  EnterGocciaFloatingPointScope(FloatingPointState);
  try
    if (FSourceType = stModule) and (FExecutor is TGocciaBytecodeExecutor) then
    begin
      SavedArgumentsObjectEnabled :=
        TGocciaBytecodeExecutor(FExecutor).ArgumentsObjectEnabled;
      SavedNonStrictMode := TGocciaBytecodeExecutor(FExecutor).NonStrictMode;
      TGocciaBytecodeExecutor(FExecutor).ArgumentsObjectEnabled :=
        cfArgumentsObject in FCompatibility;
      TGocciaBytecodeExecutor(FExecutor).NonStrictMode := False;
      try
        Result := FExecutor.CompileModule(AProgram);
      finally
        TGocciaBytecodeExecutor(FExecutor).ArgumentsObjectEnabled :=
          SavedArgumentsObjectEnabled;
        TGocciaBytecodeExecutor(FExecutor).NonStrictMode := SavedNonStrictMode;
      end;
    end
    else
      Result := FExecutor.CompileModule(AProgram);
    FRetainedModules.Add(Result);
  finally
    LeaveGocciaFloatingPointScope(FloatingPointState);
  end;
end;

procedure TGocciaEngine.RetainModule(const AModule: TGocciaCompiledModule);
begin
  FRetainedModules.Add(AModule);
end;

procedure RegisterEntryModuleExports(const AModule: TGocciaModule;
  const AProgram: TGocciaProgram; const AModuleScope: TGocciaScope;
  const AModuleLoader: TGocciaModuleLoader;
  const AContext: TGocciaEvaluationContext;
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
  Names: TStringList;
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
      if ExportDefaultDecl.IsDirectDeclaration and
         (ExportDefaultDecl.Expression is TGocciaFunctionExpression) then
      begin
        Value := ExportDefaultDecl.Expression.Evaluate(AContext);
        if (Value is TGocciaFunctionValue) and
           (TGocciaFunctionValue(Value).Name = '') then
          TGocciaFunctionValue(Value).Name := KEYWORD_DEFAULT;
        AModuleScope.ForceUpdateBinding(ExportDefaultDecl.LocalName, Value);
      end;
    end
    else if Stmt is TGocciaExportVariableDeclaration then
    begin
      ExportVarDecl := TGocciaExportVariableDeclaration(Stmt);
      Names := TStringList.Create;
      try
        Names.CaseSensitive := True;
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
      Names := TStringList.Create;
      try
        Names.CaseSensitive := True;
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

procedure EvaluateEntryRequestedModulesInSourceOrder(
  const AProgram: TGocciaProgram; const AModuleLoader: TGocciaModuleLoader;
  const AImportingFilePath: string;
  const ARequestedModules: TGocciaModuleList);
var
  I: Integer;
  ImportDecl: TGocciaImportDeclaration;
  RequestedModule: TGocciaModule;
  Stmt: TGocciaStatement;
begin
  if (not Assigned(AProgram)) or (not Assigned(AModuleLoader)) then
    Exit;

  for I := 0 to AProgram.Body.Count - 1 do
  begin
    Stmt := AProgram.Body[I];
    if Stmt is TGocciaImportDeclaration then
    begin
      ImportDecl := TGocciaImportDeclaration(Stmt);
      case ImportDecl.Phase of
        icpEvaluation:
        begin
          RequestedModule := AModuleLoader.LoadModule(EncodeImportSpecifierAttribute(
            ImportDecl.ModulePath, ImportDecl.AttributeType),
            AImportingFilePath);
          if Assigned(RequestedModule) and Assigned(ARequestedModules) then
            ARequestedModules.Add(RequestedModule);
        end;
        icpDefer:
          AModuleLoader.LoadDeferredModuleNamespaceValueForEvaluation(
            EncodeImportSpecifierAttribute(ImportDecl.ModulePath,
            ImportDecl.AttributeType), AImportingFilePath, ARequestedModules);
      end;
    end
    else if Stmt is TGocciaReExportDeclaration then
    begin
      RequestedModule := AModuleLoader.LoadModule(
        EncodeImportSpecifierAttribute(
          TGocciaReExportDeclaration(Stmt).ModulePath,
          TGocciaReExportDeclaration(Stmt).AttributeType),
        AImportingFilePath);
      if Assigned(RequestedModule) and Assigned(ARequestedModules) then
        ARequestedModules.Add(RequestedModule);
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
  const AModule: TGocciaCompiledModule): TGocciaValue;
var
  GC: TGarbageCollector;
  FloatingPointState: TGocciaFloatingPointState;
begin
  EnterGocciaFloatingPointScope(FloatingPointState);
  try
    Result := FExecutor.RunCompiledModule(AModule);
    GC := TGarbageCollector.Instance;
    if Assigned(Result) and Assigned(GC) then
      GC.AddTempRoot(Result);
    try
      WaitForRuntimeIdle;
    finally
      if Assigned(Result) and Assigned(GC) then
        GC.RemoveTempRoot(Result);
    end;
  finally
    LeaveGocciaFloatingPointScope(FloatingPointState);
  end;
end;

function TGocciaEngine.RunModuleInScope(
  const AModule: TGocciaCompiledModule;
  const AScope: TGocciaScope): TGocciaValue;
var
  GC: TGarbageCollector;
  FloatingPointState: TGocciaFloatingPointState;
begin
  EnterGocciaFloatingPointScope(FloatingPointState);
  try
    Result := FExecutor.RunCompiledModuleInScope(AModule, AScope);
    GC := TGarbageCollector.Instance;
    if Assigned(Result) and Assigned(GC) then
      GC.AddTempRoot(Result);
    try
      WaitForRuntimeIdle;
    finally
      if Assigned(Result) and Assigned(GC) then
        GC.RemoveTempRoot(Result);
    end;
  finally
    LeaveGocciaFloatingPointScope(FloatingPointState);
  end;
end;

function TGocciaEngine.RunModuleForSourceType(
  const AModule: TGocciaCompiledModule;
  const AFileName: string): TGocciaValue;
var
  ModuleScope: TGocciaScope;
begin
  if FSourceType = stModule then
  begin
    ModuleScope := FInterpreter.GlobalScope.CreateChild(skModule,
      'Module:' + AFileName);
    ModuleScope.ThisValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    ModuleScope.NonStrictMode := False;
    ModuleScope.ArgumentsObjectEnabled :=
      cfArgumentsObject in FCompatibility;
    Result := RunModuleInScope(AModule, ModuleScope);
  end
  else
    Result := RunModule(AModule);
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
  FInterpreter.GlobalModules.AddOrSetValue(AName, AModule);
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

function TGocciaEngine.Execute: TGocciaScriptResult;
var
  PipelineOptions: TGocciaSourcePipelineOptions;
  ActiveOptionsScope: TGocciaSourcePipelineOptionsScope;
  PipelineResult: TGocciaSourcePipelineResult;
  StartTime, ExecStart, ExecEnd: Int64;
  ModuleScope: TGocciaScope;
  ModuleContext: TGocciaEvaluationContext;
  ModuleResult: TGocciaValue;
  ModuleProgramConsumed: Boolean;
  EntryModule: TGocciaModule;
  EntryModuleEvaluationStarted: Boolean;
  EntryRequestedModules: TGocciaModuleList;
  EntryPromise: TGocciaPromiseValue;
  SavedVMGlobalScope: TGocciaScope;
  GC: TGarbageCollector;
  FloatingPointState: TGocciaFloatingPointState;
begin
  EnterGocciaFloatingPointScope(FloatingPointState);
  try
  FillChar(FLastTiming, SizeOf(FLastTiming), 0);
  FLastTiming.FileName := FSourcePath;
  StartTime := GetNanoseconds;
  PipelineResult := nil;

  try
    PipelineOptions := TGocciaSourcePipeline.DefaultOptions;
    PipelineOptions.Preprocessors := FPreprocessors;
    PipelineOptions.Compatibility := FCompatibility;
    PipelineOptions.LabelStatementsEnabled := FLabelStatementsEnabled;
    PipelineOptions.ForInLoopsEnabled := FForInLoopsEnabled;
    PipelineOptions.ExperimentalJSModuleSourceEnabled :=
      FExperimentalJSModuleSourceEnabled;
    PipelineOptions.WarningUnsupportedFeatures := FWarningUnsupportedFeatures;
    PipelineOptions.SourceType := FSourceType;
    PipelineResult := TGocciaSourcePipeline.Parse(FSourceLines, FSourcePath,
      PipelineOptions);
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
    try
      if FSourceType = stModule then
      begin
        // ES2026 §16.2.1.6.4: a Module Environment Record's
        // [[ThisValue]] is undefined.  Run the entry program in a
        // fresh module scope so import.meta resolves and top-level
        // `this` is undefined, matching the semantics imported
        // modules already receive via TGocciaModuleLoader.
        ModuleScope := FInterpreter.GlobalScope.CreateChild(skModule,
          'Module:' + FSourcePath);
        ModuleScope.ThisValue :=
          TGocciaUndefinedLiteralValue.UndefinedValue;
        ModuleScope.NonStrictMode := False;
        ModuleScope.ArgumentsObjectEnabled :=
          cfArgumentsObject in FCompatibility;
        EntryModule := nil;
          EntryModuleEvaluationStarted := False;
          EntryRequestedModules := nil;
          ModuleContext := FInterpreter.CreateEvaluationContext;
        ModuleContext.Realm := FRealm;
        ModuleContext.Scope := ModuleScope;
        ModuleContext.CurrentFilePath := FSourcePath;
        ModuleContext.CurrentModule := nil;
        ModuleContext.NonStrictMode := False;
        ModuleContext.CompatibilityNonStrictMode := False;
        if Assigned(FModuleLoader) then
        begin
          EntryRequestedModules := TGocciaModuleList.Create;
          EntryModule := TGocciaModule.Create(ExpandFileName(FSourcePath));
          EntryModule.SetEnvironment(ModuleScope);
          FModuleLoader.RegisterModule(EntryModule.Path, EntryModule);
          ModuleContext.CurrentModule := EntryModule;
          FModuleLoader.BeginEvaluatingModulePath(EntryModule.Path);
          EntryModuleEvaluationStarted := True;
          try
            PredeclareModuleLexicalDeclarations(PipelineResult.ProgramNode,
              ModuleScope);
            HoistFunctionDeclarations(PipelineResult.ProgramNode.Body,
              ModuleContext, True);
            if FModuleLoader.VarEnabled then
              HoistVarDeclarations(PipelineResult.ProgramNode.Body,
                ModuleScope, ModuleContext);
            ModuleContext.ModuleEnvironmentInitialized := True;
            RegisterEntryModuleExports(EntryModule, PipelineResult.ProgramNode,
              ModuleScope, FModuleLoader, ModuleContext, False);
            EvaluateEntryRequestedModulesInSourceOrder(
              PipelineResult.ProgramNode, FModuleLoader, FSourcePath,
              EntryRequestedModules);
            RegisterEntryModuleExports(EntryModule, PipelineResult.ProgramNode,
              ModuleScope, FModuleLoader, ModuleContext, True);
            SavedVMGlobalScope := nil;
            if FExecutor is TGocciaBytecodeExecutor then
            begin
              SavedVMGlobalScope :=
                TGocciaBytecodeExecutor(FExecutor).VM.GlobalScope;
              TGocciaBytecodeExecutor(FExecutor).VM.GlobalScope := ModuleScope;
            end;
            try
              DrainEntryRequestedModuleEvaluationPromises(
                EntryRequestedModules, EntryModule);
            finally
              if FExecutor is TGocciaBytecodeExecutor then
                TGocciaBytecodeExecutor(FExecutor).VM.GlobalScope :=
                  SavedVMGlobalScope;
            end;
          except
            FModuleLoader.EndEvaluatingModulePath(EntryModule.Path);
            EntryModuleEvaluationStarted := False;
            raise;
          end;
        end
        else
        begin
          PredeclareModuleLexicalDeclarations(PipelineResult.ProgramNode,
            ModuleScope);
          HoistFunctionDeclarations(PipelineResult.ProgramNode.Body,
            ModuleContext, True);
          ModuleContext.ModuleEnvironmentInitialized := True;
        end;
        ExecStart := GetNanoseconds;
        ModuleResult := nil;
        GC := TGarbageCollector.Instance;
        if Assigned(GC) then
          GC.AddTempRoot(ModuleScope);
        try
          ModuleResult := FExecutor.EvaluateModuleBody(
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
            SavedVMGlobalScope := nil;
            if FExecutor is TGocciaBytecodeExecutor then
            begin
              SavedVMGlobalScope :=
                TGocciaBytecodeExecutor(FExecutor).VM.GlobalScope;
              TGocciaBytecodeExecutor(FExecutor).VM.GlobalScope := ModuleScope;
            end;
            try
              WaitForRuntimeIdle;
              if ModuleResult is TGocciaPromiseValue then
              begin
                EntryPromise := TGocciaPromiseValue(ModuleResult);
                if EntryPromise.State = gpsRejected then
                  raise TGocciaThrowValue.Create(EntryPromise.PromiseResult);
              end;
              if Assigned(EntryModule) and
                 (EntryModule.EvaluationPromise is TGocciaPromiseValue) then
              begin
                EntryPromise := TGocciaPromiseValue(EntryModule.EvaluationPromise);
                if EntryPromise.State = gpsRejected then
                  raise TGocciaThrowValue.Create(EntryPromise.PromiseResult);
              end;
              FLastTiming.Result := ModuleResult;
            finally
              if FExecutor is TGocciaBytecodeExecutor then
                TGocciaBytecodeExecutor(FExecutor).VM.GlobalScope :=
                  SavedVMGlobalScope;
            end;
          finally
            if Assigned(ModuleResult) and Assigned(GC) then
              GC.RemoveTempRoot(ModuleResult);
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
          FInterpreter.GlobalScope, FSourcePath);
        FLastTiming.Result := ExecuteProgram(PipelineResult.ProgramNode);
        ExecEnd := GetNanoseconds;
        FLastTiming.CompileTimeNanoseconds :=
          FExecutor.CompileTimeNanoseconds;
        FLastTiming.ExecuteTimeNanoseconds :=
          FExecutor.ExecuteTimeNanoseconds;
      end;
      FLastTiming.TotalTimeNanoseconds := ExecEnd - StartTime;
    finally
      ActiveOptionsScope.Free;
      if (TGocciaMicrotaskQueue.Instance <> nil) then
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
begin
  EnterGocciaFloatingPointScope(FloatingPointState);
  try
    Result := FExecutor.ExecuteProgram(AProgram);
    GC := TGarbageCollector.Instance;
    if Assigned(Result) and Assigned(GC) then
      GC.AddTempRoot(Result);
    try
      WaitForRuntimeIdle;
    finally
      if Assigned(Result) and Assigned(GC) then
        GC.RemoveTempRoot(Result);
    end;
  finally
    LeaveGocciaFloatingPointScope(FloatingPointState);
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
  Executor: TGocciaInterpreterExecutor;
begin
  Executor := TGocciaInterpreterExecutor.Create;
  try
    Engine := TGocciaEngine.Create(AFileName, ASource, Executor);
    try
      Result := Engine.Execute;
    finally
      Engine.Free;
    end;
  finally
    Executor.Free;
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
  if Assigned(FInterpreter) then
  begin
    FInterpreter.StrictTypesEnabled := AValue;
    if Assigned(FInterpreter.GlobalScope) then
      FInterpreter.GlobalScope.StrictTypes := AValue;
  end;
  if FExecutor is TGocciaBytecodeExecutor then
    TGocciaBytecodeExecutor(FExecutor).StrictTypes := AValue;
end;

procedure TGocciaEngine.SetPreprocessors(const AValue: TGocciaPreprocessors);
begin
  FPreprocessors := AValue;
  FInterpreter.JSXEnabled := ppJSX in AValue;
end;

procedure TGocciaEngine.SetCompatibility(AValue: TGocciaCompatibilityFlags);
begin
  FCompatibility := AValue;
  FLabelStatementsEnabled := cfLabel in AValue;
  FForInLoopsEnabled := cfForIn in AValue;
  FExperimentalJSModuleSourceEnabled :=
    cfExperimentalJSModuleSource in AValue;
  FInterpreter.ASIEnabled := cfASI in AValue;
  FInterpreter.VarEnabled := cfVar in AValue;
  FInterpreter.FunctionEnabled := cfFunction in AValue;
  FInterpreter.TraditionalForLoopsEnabled := cfTraditionalFor in AValue;
  FInterpreter.WhileLoopsEnabled := cfWhileLoops in AValue;
  FInterpreter.LooseEqualityEnabled := cfLooseEquality in AValue;
  FInterpreter.NonStrictModeEnabled := cfNonStrictMode in AValue;
  FInterpreter.ArgumentsObjectEnabled :=
    cfArgumentsObject in AValue;
  if Assigned(FModuleLoader) then
  begin
    FModuleLoader.Compatibility := AValue;
    FModuleLoader.LabelStatementsEnabled := FLabelStatementsEnabled;
    FModuleLoader.ForInLoopsEnabled := FForInLoopsEnabled;
    FModuleLoader.ExperimentalJSModuleSourceEnabled :=
      FExperimentalJSModuleSourceEnabled;
  end;
  if FExecutor is TGocciaBytecodeExecutor then
  begin
    TGocciaBytecodeExecutor(FExecutor).NonStrictMode :=
      cfNonStrictMode in AValue;
    TGocciaBytecodeExecutor(FExecutor).ArgumentsObjectEnabled :=
      cfArgumentsObject in AValue;
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

function TGocciaEngine.CompileDynamicFunction(
  const AParamsSources: array of string;
  const ABodySource: string;
  const AKind: TGocciaDynamicFunctionKind): TGocciaFunctionBase;
var
  ParamStr: string;
  I: Integer;
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
  ParamStr := '';
  for I := 0 to High(AParamsSources) do
  begin
    if I > 0 then
      ParamStr := ParamStr + ',';
    ParamStr := ParamStr + AParamsSources[I];
  end;

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
  if not TGocciaSourcePipeline.ParseFunctionParameters(ParamStr,
    '<Function:params>', PipelineOptions, AKind) then
    ThrowSyntaxError('Invalid parameter list for Function constructor');

  // Validate body: must parse as a valid function body.
  // An arrow wrapper ensures the body is enclosed in matching braces.
  BodyParseResult := TGocciaSourcePipeline.ParseFunctionBodyWrapper(
    ABodySource, '<Function:body>', PipelineOptions, AKind);
  if not BodyParseResult.IsValid then
    ThrowSyntaxError('Invalid body for Function constructor');

  // Both pieces validated — safe to assemble the wrapper
  WrapperOptions := PipelineOptions;
  if BodyParseResult.HasUseStrictDirective then
    WrapperOptions.InheritedStrictMode := True;
  ProgramNode := TGocciaSourcePipeline.ParseDynamicFunctionWrapper(ParamStr,
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

    ResultValue := FExecutor.ExecuteDynamicFunction(ProgramNode);
    Result := TGocciaFunctionBase(ResultValue);
    // ES2026 §20.2.1.1.1: the function's name is 'anonymous'
    if Result is TGocciaFunctionValue then
      TGocciaFunctionValue(Result).Name := 'anonymous';
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

end.
