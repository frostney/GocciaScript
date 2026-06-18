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
  Goccia.Constants,
  Goccia.Evaluator,
  Goccia.Evaluator.Context,
  Goccia.ExecutionContext,
  Goccia.Executor,
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
  Goccia.SourceMap,
  Goccia.SourcePipeline,
  Goccia.Values.BigIntValue,
  Goccia.Values.ClassValue,
  Goccia.Values.FunctionBase,
  Goccia.Values.HoleValue,
  Goccia.Values.IteratorValue,
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
      const AJSON5String: UTF8String): Boolean; virtual;
    function InjectGlobalsFromTOML(
      const ATOMLString: UTF8String): Boolean; virtual;
    function InjectGlobalsFromYAML(const AYamlString: string): Boolean; virtual;
  end;

  TGocciaEngineExtensionClass = class of TGocciaEngineExtension;
  TGocciaEngineExtensionList = TObjectList<TGocciaEngineExtension>;

  TGocciaEngine = class
  public
    const DefaultPreprocessors: TGocciaPreprocessors = [ppJSX];
    const DefaultCompatibility: TGocciaCompatibilityFlags = [];
    const DefaultSourceType: TGocciaSourceType = stScript;
  private
    FInterpreter: TGocciaInterpreter;
    FSourcePath: string;
    FModuleLoader: TGocciaModuleLoader;
    FOwnsModuleLoader: Boolean;
    FInjectedGlobals: TStringList;
    FPreprocessors: TGocciaPreprocessors;
    FCompatibility: TGocciaCompatibilityFlags;
    FSourceType: TGocciaSourceType;
    FStrictTypes: Boolean;
    FShims: TStringList;
    FExecutor: TGocciaExecutor;
    FSourceLines: TStringList;
    FExtensions: TGocciaEngineExtensionList;
    FRetainedModules: TObjectList;

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
    FPreviousExceptionMask: TFPUExceptionMask;
    FSuppressWarnings: Boolean;
    FLastTiming: TGocciaScriptResult;
    FLastSourceMap: TGocciaSourceMap;
    procedure SetStrictTypes(const AValue: Boolean);
    function GetContentProvider: TGocciaModuleContentProvider;
    function GetModuleResolver: TGocciaModuleResolver;
    procedure SetPreprocessors(const AValue: TGocciaPreprocessors);
    procedure SetCompatibility(const AValue: TGocciaCompatibilityFlags);

    procedure PinSingletons;
    procedure RegisterBuiltIns;
    procedure RegisterBuiltinConstructors;
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
    procedure DoRetainModule(const AModule: TObject);
    procedure DiscardRuntimePending;
    procedure PrintSourcePipelineWarnings(
      const APipelineResult: TGocciaSourcePipelineResult);
    function CompileDynamicFunction(const AParamsSources: array of string;
      const ABodySource: string;
      const AKind: TGocciaDynamicFunctionKind): TGocciaFunctionBase;
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
    procedure InjectGlobal(const AKey: string; const AValue: TGocciaValue);
    procedure RegisterGlobal(const AName: string; const AValue: TGocciaValue);
    procedure InjectGlobalsFromJSON(const AJsonString: string);
    procedure InjectGlobalsFromJSON5(const AJSON5String: UTF8String);
    procedure InjectGlobalsFromTOML(const ATOMLString: UTF8String);
    procedure InjectGlobalsFromYAML(const AYamlString: string);
    procedure InjectGlobalsFromModule(const APath: string);
    procedure ClearTransientCaches;
    procedure RegisterGlobalModule(const AName: string; const AModule: TGocciaModule);
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
    property FunctionConstructor: TGocciaFunctionConstructorClassValue read FFunctionConstructor;
    property ObjectConstructor: TGocciaClassValue read FObjectConstructor;
    property Preprocessors: TGocciaPreprocessors read FPreprocessors write SetPreprocessors;
    property Compatibility: TGocciaCompatibilityFlags read FCompatibility write SetCompatibility;
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
  Goccia.Executor.Bytecode,
  Goccia.Executor.Interpreter,
  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.ImportMeta,
  Goccia.MicrotaskQueue,
  Goccia.Platform,
  Goccia.Scope.Redeclaration,
  Goccia.Shims,
  Goccia.Spec,
  Goccia.Threading,
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
  Goccia.Values.ObjectPropertyDescriptor,
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
  const AJSON5String: UTF8String): Boolean;
begin
  Result := False;
end;

function TGocciaEngineExtension.InjectGlobalsFromTOML(
  const ATOMLString: UTF8String): Boolean;
begin
  Result := False;
end;

function TGocciaEngineExtension.InjectGlobalsFromYAML(
  const AYamlString: string): Boolean;
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
  const AJSON5String: UTF8String);
var
  I: Integer;
begin
  for I := 0 to FExtensions.Count - 1 do
    if FExtensions[I].InjectGlobalsFromJSON5(AJSON5String) then
      Exit;
  ThrowError('JSON5 globals require a runtime extension.', 0, 0);
end;

procedure TGocciaEngine.InjectGlobalsFromTOML(
  const ATOMLString: UTF8String);
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
begin
  for I := 0 to DefaultShimCount - 1 do
  begin
    Shim := DefaultShim(I);
    if (Shim.Name = 'hasOwnProperty') or (Shim.Name = '__proto__') or
       (Shim.Name = 'defineGetter') or (Shim.Name = 'defineSetter') or
       (Shim.Name = 'lookupGetter') or (Shim.Name = 'lookupSetter') then
      LoadShimValue(FInterpreter, Shim)
    else
      FInterpreter.GlobalScope.DefineLexicalBinding(Shim.Name,
        LoadShimValue(FInterpreter, Shim), dtConst, True);
  end;
end;

procedure TGocciaEngine.Initialize(const AFileName: string;
  const ASourceLines: TStringList; const AModuleLoader: TGocciaModuleLoader;
  const AOwnsModuleLoader: Boolean);
begin
  FPreviousExceptionMask := GetExceptionMask;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
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
  FRealmExecutionContext := TGocciaExecutionContextScope.Create(
    CreateExecutionContext(FRealm, FInterpreter.GlobalScope, AFileName));

  TGarbageCollector.Instance.AddRootObject(FInterpreter.GlobalScope);

  FInjectedGlobals := TStringList.Create;
  RegisterDefaultShimNames(FShims);
  PinSingletons;
  RegisterBuiltIns;
  ExecuteShims;

  // The executor was provided by the caller via the constructor.  When the
  // caller chose interpreter mode, bind the bootstrapped interpreter into
  // it now so its tree-walk methods can dispatch.  The bytecode executor
  // does not need this hook.
  FRetainedModules := TObjectList.Create(True);

  if FExecutor is TGocciaInterpreterExecutor then
    TGocciaInterpreterExecutor(FExecutor).Interpreter := FInterpreter;
  FExecutor.RetainModuleCallback := DoRetainModule;
  FExecutor.Initialize(FInterpreter.GlobalScope, FModuleLoader, AFileName);

  if Assigned(FFunctionConstructor) then
    FFunctionConstructor.CompileDynamicFunction := CompileDynamicFunction;
end;

destructor TGocciaEngine.Destroy;
begin
  try
    if Assigned(TGarbageCollector.Instance) and Assigned(FInterpreter) then
      TGarbageCollector.Instance.RemoveRootObject(FInterpreter.GlobalScope);

    FRetainedModules.Free;
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
  finally
    SetExceptionMask(FPreviousExceptionMask);
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
  FBuiltinMath := TGocciaMath.Create('Math', Scope, ThrowError);
  FBuiltinGlobalObject := TGocciaGlobalObject.Create(CONSTRUCTOR_OBJECT, Scope, ThrowError);
  FBuiltinGlobalArray := TGocciaGlobalArray.Create(CONSTRUCTOR_ARRAY, Scope, ThrowError);
  FBuiltinGlobalNumber := TGocciaGlobalNumber.Create(CONSTRUCTOR_NUMBER, Scope, ThrowError);
  FBuiltinGlobalBigInt := TGocciaGlobalBigInt.Create(CONSTRUCTOR_BIGINT, Scope, ThrowError);
  FBuiltinJSON := TGocciaJSONBuiltin.Create('JSON', Scope, ThrowError);
  FBuiltinSymbol := TGocciaGlobalSymbol.Create(CONSTRUCTOR_SYMBOL, Scope, ThrowError);
  FBuiltinMap := TGocciaGlobalMap.Create(CONSTRUCTOR_MAP, Scope, ThrowError);
  FBuiltinPromise := TGocciaGlobalPromise.Create(CONSTRUCTOR_PROMISE, Scope, ThrowError);
  FBuiltinTemporal := TGocciaTemporalBuiltin.Create('Temporal', Scope, ThrowError);
  FBuiltinIntl := TGocciaIntlBuiltin.Create('Intl', Scope, ThrowError);
  FBuiltinAtomics := TGocciaAtomics.Create(CONSTRUCTOR_ATOMICS, Scope, ThrowError);
  FBuiltinArrayBuffer := TGocciaGlobalArrayBuffer.Create(CONSTRUCTOR_ARRAY_BUFFER, Scope, ThrowError);
  FBuiltinProxy := TGocciaGlobalProxy.Create(Scope);
  FBuiltinReflect := TGocciaGlobalReflect.Create('Reflect', Scope, ThrowError);
  FBuiltinGlobalString := TGocciaGlobalString.Create(CONSTRUCTOR_STRING, Scope, ThrowError);
  FBuiltinGlobals := TGocciaGlobals.Create('Globals', Scope, ThrowError);
  FBuiltinDisposableStack := TGocciaBuiltinDisposableStack.Create('DisposableStack', Scope, ThrowError);
  Scope.DefineLexicalBinding(CONSTRUCTOR_ITERATOR, TGocciaIteratorValue.CreateGlobalObject, dtConst, True);
  RegisterBuiltinConstructors;
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
  if Assigned(GetErrorProto) and not Assigned(GetErrorProto.Prototype) then
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
      ArrayBufferConstructor.SetProperty(Key, FBuiltinArrayBuffer.BuiltinObject.GetProperty(Key));
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
  FTypedArrayIntrinsic := TGocciaClassValue.Create('TypedArray', nil);
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
  if Assigned(TGocciaTypedArrayValue.GetSharedPrototypeObject) then
  begin
    FTypedArrayIntrinsic.ReplacePrototype(TGocciaTypedArrayValue.GetSharedPrototypeObject);
    // §23.2.3: %TypedArray%.prototype.constructor = %TypedArray%
    TGocciaTypedArrayValue.GetSharedPrototypeObject.DefineProperty(PROP_CONSTRUCTOR,
      TGocciaPropertyDescriptorData.Create(FTypedArrayIntrinsic, [pfConfigurable, pfWritable]));
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
  if Assigned(TGocciaSymbolValue.SharedPrototype) then
    TGocciaObjectValue(TGocciaSymbolValue.SharedPrototype).Prototype := ObjectConstructor.Prototype;

  RegisterGocciaScriptGlobal;
  RegisterGlobalThis;
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
  if Scope.ContainsOwnLexicalBinding('globalThis') and
     (Scope.GetValue('globalThis') is TGocciaObjectValue) then
    GlobalThisObj := TGocciaObjectValue(Scope.GetValue('globalThis'))
  else
    GlobalThisObj := TGocciaObjectValue.Create;

  if (not Assigned(GlobalThisObj.Prototype)) and
     Assigned(TGocciaObjectValue.SharedObjectPrototype) then
    GlobalThisObj.Prototype := TGocciaObjectValue.SharedObjectPrototype;

  for Name in Scope.GetOwnBindingNames do
  begin
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
  end;

  GlobalThisObj.DefineProperty('globalThis',
    TGocciaPropertyDescriptorData.Create(GlobalThisObj, [pfWritable, pfConfigurable]));
  if Scope.ContainsOwnLexicalBinding('globalThis') then
    Scope.ForceUpdateBinding('globalThis', GlobalThisObj)
  else
    Scope.DefineLexicalBinding('globalThis', GlobalThisObj, dtConst, True);

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
  FInterpreter.GlobalScope.DefineLexicalBinding('Goccia', FGocciaGlobal, dtConst, True);
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
    if Assigned(Queue) and Queue.HasPending then
      Queue.DrainQueue;
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
begin
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
  ImportDecl: TGocciaImportDeclaration;
  ImportPair: TStringStringMap.TKeyValuePair;
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
      for VarInfo in ExportVarDecl.Declaration.Variables do
        AModule.AddExportBinding(VarInfo.Name, VarInfo.Name, AModuleScope);
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
begin
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
end;

function TGocciaEngine.RunModuleInScope(
  const AModule: TGocciaCompiledModule;
  const AScope: TGocciaScope): TGocciaValue;
var
  GC: TGarbageCollector;
begin
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
  FInterpreter.GlobalModules.AddOrSetValue(AName, AModule);
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
begin
  FillChar(FLastTiming, SizeOf(FLastTiming), 0);
  FLastTiming.FileName := FSourcePath;
  StartTime := GetNanoseconds;
  PipelineResult := nil;

  try
    PipelineOptions := TGocciaSourcePipeline.DefaultOptions;
    PipelineOptions.Preprocessors := FPreprocessors;
    PipelineOptions.Compatibility := FCompatibility;
    PipelineOptions.SourceType := FSourceType;
    PipelineResult := TGocciaSourcePipeline.Parse(FSourceLines, FSourcePath,
      PipelineOptions);
    FLastTiming.LexTimeNanoseconds := PipelineResult.LexTimeNanoseconds;
    FLastTiming.ParseTimeNanoseconds := PipelineResult.ParseTimeNanoseconds;
    PrintSourcePipelineWarnings(PipelineResult);

    if Assigned(TGocciaCoverageTracker.Instance) and
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
      if Assigned(TGocciaMicrotaskQueue.Instance) then
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
end;

function TGocciaEngine.TakeLastSourceMap: TGocciaSourceMap;
begin
  Result := FLastSourceMap;
  FLastSourceMap := nil;
end;

function TGocciaEngine.ExecuteProgram(const AProgram: TGocciaProgram): TGocciaValue;
var
  GC: TGarbageCollector;
begin
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
end;

class function TGocciaEngine.RunScript(const ASource: string;
  const AFileName: string): TGocciaScriptResult;
var
  SourceList: TStringList;
begin
  SourceList := CreateUTF8StringList(ASource);
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

procedure TGocciaEngine.SetCompatibility(const AValue: TGocciaCompatibilityFlags);
begin
  FCompatibility := AValue;
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
    FModuleLoader.Compatibility := AValue;
  if FExecutor is TGocciaBytecodeExecutor then
  begin
    TGocciaBytecodeExecutor(FExecutor).NonStrictMode :=
      cfNonStrictMode in AValue;
    TGocciaBytecodeExecutor(FExecutor).ArgumentsObjectEnabled :=
      cfArgumentsObject in AValue;
  end;
end;

procedure TGocciaEngine.ThrowError(const AMessage: string; const ALine, AColumn: Integer);
begin
  raise TGocciaRuntimeError.Create(AMessage, ALine, AColumn, FSourcePath, FSourceLines);
end;

function IsStrictDynamicRestrictedName(const AName: string): Boolean;
begin
  Result := (AName = 'eval') or (AName = IDENTIFIER_ARGUMENTS);
end;

procedure RejectStrictDynamicBindingName(const AName: string;
  const ALine, AColumn: Integer);
begin
  if IsStrictDynamicRestrictedName(AName) then
    raise TGocciaSyntaxError.Create(
      Format('Invalid binding name ''%s'' in strict mode', [AName]),
      ALine, AColumn, '', nil);
end;

procedure ValidateStrictDynamicExpression(const AExpr: TGocciaExpression); forward;
procedure ValidateStrictDynamicStatement(const AStmt: TGocciaStatement); forward;

function DynamicExpressionContainsAwait(const AExpr: TGocciaExpression): Boolean; forward;

function DynamicPatternContainsAwait(
  const APattern: TGocciaDestructuringPattern): Boolean;
var
  ArrPat: TGocciaArrayDestructuringPattern;
  AssignmentPattern: TGocciaAssignmentDestructuringPattern;
  ObjPat: TGocciaObjectDestructuringPattern;
  Prop: TGocciaDestructuringProperty;
  RestPattern: TGocciaRestDestructuringPattern;
  I: Integer;
begin
  if not Assigned(APattern) then
    Exit(False);

  if APattern is TGocciaArrayDestructuringPattern then
  begin
    ArrPat := TGocciaArrayDestructuringPattern(APattern);
    for I := 0 to ArrPat.Elements.Count - 1 do
      if DynamicPatternContainsAwait(ArrPat.Elements[I]) then
        Exit(True);
  end
  else if APattern is TGocciaObjectDestructuringPattern then
  begin
    ObjPat := TGocciaObjectDestructuringPattern(APattern);
    for I := 0 to ObjPat.Properties.Count - 1 do
    begin
      Prop := ObjPat.Properties[I];
      if (Prop.Computed and DynamicExpressionContainsAwait(Prop.KeyExpression)) or
         DynamicPatternContainsAwait(Prop.Pattern) then
        Exit(True);
    end;
  end
  else if APattern is TGocciaAssignmentDestructuringPattern then
  begin
    AssignmentPattern := TGocciaAssignmentDestructuringPattern(APattern);
    Exit(DynamicPatternContainsAwait(AssignmentPattern.Left) or
      DynamicExpressionContainsAwait(AssignmentPattern.Right));
  end
  else if APattern is TGocciaRestDestructuringPattern then
  begin
    RestPattern := TGocciaRestDestructuringPattern(APattern);
    Exit(DynamicPatternContainsAwait(RestPattern.Argument));
  end
  else if APattern is TGocciaMemberExpressionDestructuringPattern then
    Exit(DynamicExpressionContainsAwait(
      TGocciaMemberExpressionDestructuringPattern(APattern).Expression))
  else if APattern is TGocciaPrivateMemberExpressionDestructuringPattern then
    Exit(DynamicExpressionContainsAwait(
      TGocciaPrivateMemberExpressionDestructuringPattern(APattern).Expression));

  Result := False;
end;

function DynamicExpressionContainsAwait(const AExpr: TGocciaExpression): Boolean;
var
  ArrayExpr: TGocciaArrayExpression;
  CallExpr: TGocciaCallExpression;
  ImportExpr: TGocciaImportCallExpression;
  MemberExpr: TGocciaMemberExpression;
  NewExpr: TGocciaNewExpression;
  ObjectExpr: TGocciaObjectExpression;
  Pair: TPair<TGocciaExpression, TGocciaExpression>;
  I: Integer;
begin
  if not Assigned(AExpr) then
    Exit(False);

  if AExpr is TGocciaAwaitExpression then
    Exit(True)
  else if AExpr is TGocciaCallExpression then
  begin
    CallExpr := TGocciaCallExpression(AExpr);
    if DynamicExpressionContainsAwait(CallExpr.Callee) then
      Exit(True);
    for I := 0 to CallExpr.Arguments.Count - 1 do
      if DynamicExpressionContainsAwait(CallExpr.Arguments[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(AExpr);
    if DynamicExpressionContainsAwait(MemberExpr.ObjectExpr) then
      Exit(True);
    if MemberExpr.Computed and
       DynamicExpressionContainsAwait(MemberExpr.PropertyExpression) then
      Exit(True);
  end
  else if AExpr is TGocciaBinaryExpression then
    Exit(DynamicExpressionContainsAwait(TGocciaBinaryExpression(AExpr).Left) or
      DynamicExpressionContainsAwait(TGocciaBinaryExpression(AExpr).Right))
  else if AExpr is TGocciaSequenceExpression then
  begin
    for I := 0 to TGocciaSequenceExpression(AExpr).Expressions.Count - 1 do
      if DynamicExpressionContainsAwait(
        TGocciaSequenceExpression(AExpr).Expressions[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaUnaryExpression then
    Exit(DynamicExpressionContainsAwait(TGocciaUnaryExpression(AExpr).Operand))
  else if AExpr is TGocciaAssignmentExpression then
    Exit(DynamicExpressionContainsAwait(TGocciaAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaPropertyAssignmentExpression then
    Exit(DynamicExpressionContainsAwait(
      TGocciaPropertyAssignmentExpression(AExpr).ObjectExpr) or
      DynamicExpressionContainsAwait(
        TGocciaPropertyAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaComputedPropertyAssignmentExpression then
    Exit(DynamicExpressionContainsAwait(
      TGocciaComputedPropertyAssignmentExpression(AExpr).ObjectExpr) or
      DynamicExpressionContainsAwait(
        TGocciaComputedPropertyAssignmentExpression(AExpr).PropertyExpression) or
      DynamicExpressionContainsAwait(
        TGocciaComputedPropertyAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaCompoundAssignmentExpression then
    Exit(DynamicExpressionContainsAwait(
      TGocciaCompoundAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaPropertyCompoundAssignmentExpression then
    Exit(DynamicExpressionContainsAwait(
      TGocciaPropertyCompoundAssignmentExpression(AExpr).ObjectExpr) or
      DynamicExpressionContainsAwait(
        TGocciaPropertyCompoundAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaComputedPropertyCompoundAssignmentExpression then
    Exit(DynamicExpressionContainsAwait(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).ObjectExpr) or
      DynamicExpressionContainsAwait(
        TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).PropertyExpression) or
      DynamicExpressionContainsAwait(
        TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaIncrementExpression then
    Exit(DynamicExpressionContainsAwait(
      TGocciaIncrementExpression(AExpr).Operand))
  else if AExpr is TGocciaArrayExpression then
  begin
    ArrayExpr := TGocciaArrayExpression(AExpr);
    for I := 0 to ArrayExpr.Elements.Count - 1 do
      if DynamicExpressionContainsAwait(ArrayExpr.Elements[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaObjectExpression then
  begin
    ObjectExpr := TGocciaObjectExpression(AExpr);
    for I := 0 to High(ObjectExpr.PropertySourceOrder) do
      case ObjectExpr.PropertySourceOrder[I].PropertyType of
        pstStatic:
          if DynamicExpressionContainsAwait(
            ObjectExpr.PropertySourceOrder[I].Expression) then
            Exit(True);
        pstComputed:
          begin
            Pair := ObjectExpr.ComputedPropertiesInOrder[
              ObjectExpr.PropertySourceOrder[I].ComputedIndex];
            if DynamicExpressionContainsAwait(Pair.Key) or
               DynamicExpressionContainsAwait(Pair.Value) then
              Exit(True);
          end;
        pstComputedGetter,
        pstComputedSetter:
          begin
            Pair := ObjectExpr.ComputedPropertiesInOrder[
              ObjectExpr.PropertySourceOrder[I].ComputedIndex];
            if DynamicExpressionContainsAwait(Pair.Key) then
              Exit(True);
          end;
      end;
  end
  else if AExpr is TGocciaYieldExpression then
    Exit(DynamicExpressionContainsAwait(TGocciaYieldExpression(AExpr).Operand))
  else if AExpr is TGocciaConditionalExpression then
    Exit(DynamicExpressionContainsAwait(
      TGocciaConditionalExpression(AExpr).Condition) or
      DynamicExpressionContainsAwait(
        TGocciaConditionalExpression(AExpr).Consequent) or
      DynamicExpressionContainsAwait(
        TGocciaConditionalExpression(AExpr).Alternate))
  else if AExpr is TGocciaNewExpression then
  begin
    NewExpr := TGocciaNewExpression(AExpr);
    if DynamicExpressionContainsAwait(NewExpr.Callee) then
      Exit(True);
    for I := 0 to NewExpr.Arguments.Count - 1 do
      if DynamicExpressionContainsAwait(NewExpr.Arguments[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaImportCallExpression then
  begin
    ImportExpr := TGocciaImportCallExpression(AExpr);
    Exit(DynamicExpressionContainsAwait(ImportExpr.Specifier) or
      DynamicExpressionContainsAwait(ImportExpr.Options));
  end
  else if AExpr is TGocciaSpreadExpression then
    Exit(DynamicExpressionContainsAwait(TGocciaSpreadExpression(AExpr).Argument))
  else if AExpr is TGocciaTemplateWithInterpolationExpression then
  begin
    for I := 0 to TGocciaTemplateWithInterpolationExpression(AExpr).Parts.Count - 1 do
      if DynamicExpressionContainsAwait(
        TGocciaTemplateWithInterpolationExpression(AExpr).Parts[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaTaggedTemplateExpression then
  begin
    if DynamicExpressionContainsAwait(TGocciaTaggedTemplateExpression(AExpr).Tag) then
      Exit(True);
    for I := 0 to TGocciaTaggedTemplateExpression(AExpr).Expressions.Count - 1 do
      if DynamicExpressionContainsAwait(
        TGocciaTaggedTemplateExpression(AExpr).Expressions[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaDestructuringAssignmentExpression then
    Exit(DynamicPatternContainsAwait(
      TGocciaDestructuringAssignmentExpression(AExpr).Left) or
      DynamicExpressionContainsAwait(
        TGocciaDestructuringAssignmentExpression(AExpr).Right))
  else if AExpr is TGocciaPrivateMemberExpression then
    Exit(DynamicExpressionContainsAwait(
      TGocciaPrivateMemberExpression(AExpr).ObjectExpr))
  else if AExpr is TGocciaPrivatePropertyAssignmentExpression then
    Exit(DynamicExpressionContainsAwait(
      TGocciaPrivatePropertyAssignmentExpression(AExpr).ObjectExpr) or
      DynamicExpressionContainsAwait(
        TGocciaPrivatePropertyAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaPrivatePropertyCompoundAssignmentExpression then
    Exit(DynamicExpressionContainsAwait(
      TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).ObjectExpr) or
      DynamicExpressionContainsAwait(
        TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).Value));

  Result := False;
end;

function DynamicParametersContainAwait(
  const AParameters: TGocciaParameterArray): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AParameters) do
  begin
    if AParameters[I].IsPattern and
       DynamicPatternContainsAwait(AParameters[I].Pattern) then
      Exit(True);
    if DynamicExpressionContainsAwait(AParameters[I].DefaultValue) then
      Exit(True);
  end;
  Result := False;
end;

procedure ValidateStrictDynamicPattern(
  const APattern: TGocciaDestructuringPattern);
var
  Names: TStringList;
  I: Integer;
begin
  if not Assigned(APattern) then
    Exit;
  Names := TStringList.Create;
  try
    Names.CaseSensitive := True;
    CollectPatternBindingNames(APattern, Names);
    for I := 0 to Names.Count - 1 do
      RejectStrictDynamicBindingName(Names[I], APattern.Line,
        APattern.Column);
  finally
    Names.Free;
  end;
end;

procedure ValidateStrictDynamicFunction(
  const AFunction: TGocciaFunctionExpression);
var
  BindingNames, PatternNames: TStringList;
  I, J: Integer;
  procedure AddParameterName(const AName: string);
  begin
    RejectStrictDynamicBindingName(AName, AFunction.Line, AFunction.Column);
    if AName = '' then
      Exit;
    if BindingNames.IndexOf(AName) >= 0 then
      raise TGocciaSyntaxError.Create(
        'Duplicate parameter name not allowed in strict mode',
        AFunction.Line, AFunction.Column, '', nil);
    BindingNames.Add(AName);
  end;
begin
  RejectStrictDynamicBindingName(AFunction.Name, AFunction.Line,
    AFunction.Column);
  BindingNames := TStringList.Create;
  PatternNames := TStringList.Create;
  try
    BindingNames.CaseSensitive := True;
    PatternNames.CaseSensitive := True;
    for I := 0 to High(AFunction.Parameters) do
    begin
      if AFunction.Parameters[I].IsPattern then
      begin
        ValidateStrictDynamicPattern(AFunction.Parameters[I].Pattern);
        PatternNames.Clear;
        CollectPatternBindingNames(AFunction.Parameters[I].Pattern,
          PatternNames);
        for J := 0 to PatternNames.Count - 1 do
          AddParameterName(PatternNames[J]);
      end
      else
        AddParameterName(AFunction.Parameters[I].Name);
      ValidateStrictDynamicExpression(AFunction.Parameters[I].DefaultValue);
    end;
  finally
    PatternNames.Free;
    BindingNames.Free;
  end;
  if AFunction.Body is TGocciaStatement then
    ValidateStrictDynamicStatement(TGocciaStatement(AFunction.Body))
  else if AFunction.Body is TGocciaExpression then
    ValidateStrictDynamicExpression(TGocciaExpression(AFunction.Body));
end;

procedure ValidateStrictDynamicStatement(const AStmt: TGocciaStatement);
var
  BlockStmt: TGocciaBlockStatement;
  FuncDecl: TGocciaFunctionDeclaration;
  IfStmt: TGocciaIfStatement;
  ReturnStmt: TGocciaReturnStatement;
  VarDecl: TGocciaVariableDeclaration;
  I: Integer;
begin
  if not Assigned(AStmt) then
    Exit;
  if AStmt is TGocciaExpressionStatement then
    ValidateStrictDynamicExpression(TGocciaExpressionStatement(AStmt).Expression)
  else if AStmt is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(AStmt);
    for I := 0 to High(VarDecl.Variables) do
    begin
      RejectStrictDynamicBindingName(VarDecl.Variables[I].Name,
        AStmt.Line, AStmt.Column);
      ValidateStrictDynamicExpression(VarDecl.Variables[I].Initializer);
    end;
  end
  else if AStmt is TGocciaDestructuringDeclaration then
  begin
    ValidateStrictDynamicPattern(
      TGocciaDestructuringDeclaration(AStmt).Pattern);
    ValidateStrictDynamicExpression(
      TGocciaDestructuringDeclaration(AStmt).Initializer);
  end
  else if AStmt is TGocciaFunctionDeclaration then
  begin
    FuncDecl := TGocciaFunctionDeclaration(AStmt);
    RejectStrictDynamicBindingName(FuncDecl.Name, AStmt.Line, AStmt.Column);
    ValidateStrictDynamicFunction(FuncDecl.FunctionExpression);
  end
  else if AStmt is TGocciaBlockStatement then
  begin
    BlockStmt := TGocciaBlockStatement(AStmt);
    for I := 0 to BlockStmt.Nodes.Count - 1 do
      if BlockStmt.Nodes[I] is TGocciaStatement then
        ValidateStrictDynamicStatement(TGocciaStatement(BlockStmt.Nodes[I]))
      else if BlockStmt.Nodes[I] is TGocciaExpression then
        ValidateStrictDynamicExpression(TGocciaExpression(BlockStmt.Nodes[I]));
  end
  else if AStmt is TGocciaIfStatement then
  begin
    IfStmt := TGocciaIfStatement(AStmt);
    ValidateStrictDynamicExpression(IfStmt.Condition);
    ValidateStrictDynamicStatement(IfStmt.Consequent);
    ValidateStrictDynamicStatement(IfStmt.Alternate);
  end
  else if AStmt is TGocciaReturnStatement then
  begin
    ReturnStmt := TGocciaReturnStatement(AStmt);
    ValidateStrictDynamicExpression(ReturnStmt.Value);
  end;
end;

procedure ValidateStrictDynamicExpression(const AExpr: TGocciaExpression);
var
  CallExpr: TGocciaCallExpression;
  I: Integer;
begin
  if not Assigned(AExpr) then
    Exit;
  if AExpr is TGocciaAssignmentExpression then
  begin
    RejectStrictDynamicBindingName(TGocciaAssignmentExpression(AExpr).Name,
      AExpr.Line, AExpr.Column);
    ValidateStrictDynamicExpression(TGocciaAssignmentExpression(AExpr).Value);
  end
  else if AExpr is TGocciaCompoundAssignmentExpression then
  begin
    RejectStrictDynamicBindingName(
      TGocciaCompoundAssignmentExpression(AExpr).Name, AExpr.Line,
      AExpr.Column);
    ValidateStrictDynamicExpression(
      TGocciaCompoundAssignmentExpression(AExpr).Value);
  end
  else if AExpr is TGocciaIncrementExpression then
  begin
    if TGocciaIncrementExpression(AExpr).Operand is TGocciaIdentifierExpression then
      RejectStrictDynamicBindingName(
        TGocciaIdentifierExpression(
          TGocciaIncrementExpression(AExpr).Operand).Name,
        AExpr.Line, AExpr.Column);
    ValidateStrictDynamicExpression(TGocciaIncrementExpression(AExpr).Operand);
  end
  else if AExpr is TGocciaFunctionExpression then
    ValidateStrictDynamicFunction(TGocciaFunctionExpression(AExpr))
  else if AExpr is TGocciaCallExpression then
  begin
    CallExpr := TGocciaCallExpression(AExpr);
    ValidateStrictDynamicExpression(CallExpr.Callee);
    for I := 0 to CallExpr.Arguments.Count - 1 do
      ValidateStrictDynamicExpression(CallExpr.Arguments[I]);
  end
  else if AExpr is TGocciaBinaryExpression then
  begin
    ValidateStrictDynamicExpression(TGocciaBinaryExpression(AExpr).Left);
    ValidateStrictDynamicExpression(TGocciaBinaryExpression(AExpr).Right);
  end
  else if AExpr is TGocciaSequenceExpression then
    for I := 0 to TGocciaSequenceExpression(AExpr).Expressions.Count - 1 do
      ValidateStrictDynamicExpression(
        TGocciaSequenceExpression(AExpr).Expressions[I])
  else if AExpr is TGocciaUnaryExpression then
    ValidateStrictDynamicExpression(TGocciaUnaryExpression(AExpr).Operand);
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
      ParamStr := ParamStr + ', ';
    ParamStr := ParamStr + AParamsSources[I];
  end;

  // Validate params: must parse as a valid parameter list.
  // If params contain tokens that escape the signature, this will throw.
  PipelineOptions := TGocciaSourcePipeline.DefaultOptions;
  PipelineOptions.Preprocessors := [];
  PipelineOptions.Compatibility := FCompatibility + [cfFunction];
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
  ProgramNode := TGocciaSourcePipeline.ParseDynamicFunctionWrapper(ParamStr,
    ABodySource, '<Function>', PipelineOptions, AKind);
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

    if BodyParseResult.HasUseStrictDirective and
       Assigned(FunctionExpression) then
      ValidateStrictDynamicFunction(FunctionExpression);

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
