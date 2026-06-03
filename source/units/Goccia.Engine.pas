unit Goccia.Engine;

{$I Goccia.inc}

interface

uses
  Classes,
  Contnrs,
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.AST.Node,
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
  Goccia.Builtins.GlobalSet,
  Goccia.Builtins.GlobalString,
  Goccia.Builtins.GlobalSymbol,
  Goccia.Builtins.GlobalWeakMap,
  Goccia.Builtins.GlobalWeakSet,
  Goccia.Builtins.Intl,
  Goccia.Builtins.JSON,
  Goccia.Builtins.Math,
  Goccia.Builtins.Temporal,
  Goccia.Evaluator.Context,
  Goccia.Executor,
  Goccia.Interpreter,
  Goccia.JSON,
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
  cfLabel = Goccia.SourcePipeline.cfLabel;

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
    FBuiltinSet: TGocciaGlobalSet;
    FBuiltinMap: TGocciaGlobalMap;
    FBuiltinWeakSet: TGocciaGlobalWeakSet;
    FBuiltinWeakMap: TGocciaGlobalWeakMap;
    FBuiltinPromise: TGocciaGlobalPromise;
    FBuiltinTemporal: TGocciaTemporalBuiltin;
    FBuiltinIntl: TGocciaIntlBuiltin;
    FBuiltinArrayBuffer: TGocciaGlobalArrayBuffer;
    FBuiltinProxy: TGocciaGlobalProxy;
    FBuiltinReflect: TGocciaGlobalReflect;
    FBuiltinDisposableStack: TGocciaBuiltinDisposableStack;
    FGocciaGlobal: TGocciaObjectValue;
    FRealm: TGocciaRealm;
    FPrevRealm: TGocciaRealm;
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
      const ABodySource: string): TGocciaFunctionBase;
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
    property BuiltinSet: TGocciaGlobalSet read FBuiltinSet;
    property BuiltinMap: TGocciaGlobalMap read FBuiltinMap;
    property BuiltinWeakSet: TGocciaGlobalWeakSet read FBuiltinWeakSet;
    property BuiltinWeakMap: TGocciaGlobalWeakMap read FBuiltinWeakMap;
    property BuiltinPromise: TGocciaGlobalPromise read FBuiltinPromise;
    property BuiltinTemporal: TGocciaTemporalBuiltin read FBuiltinTemporal;
    property BuiltinIntl: TGocciaIntlBuiltin read FBuiltinIntl;
    property BuiltinArrayBuffer: TGocciaGlobalArrayBuffer read FBuiltinArrayBuffer;
    property BuiltinProxy: TGocciaGlobalProxy read FBuiltinProxy;
    property BuiltinReflect: TGocciaGlobalReflect read FBuiltinReflect;
    property GocciaGlobal: TGocciaObjectValue read FGocciaGlobal;
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
  Goccia.Values.ErrorHelper,
  Goccia.Values.FinalizationRegistryValue,
  Goccia.Values.FunctionValue,
  Goccia.Values.MapValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.NumberObjectValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SetValue,
  Goccia.Values.SharedArrayBufferValue,
  Goccia.Values.StringObjectValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.Uint8ArrayEncoding,
  Goccia.Values.WeakMapValue,
  Goccia.Values.WeakRefValue,
  Goccia.Values.WeakSetValue,
  Goccia.Version;

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
  Module: TGocciaModule;
  ExportPair: TGocciaValueMap.TKeyValuePair;
begin
  Module := FModuleLoader.LoadModule(APath, FSourcePath);
  for ExportPair in Module.ExportsTable do
    RegisterGlobal(ExportPair.Key, ExportPair.Value);
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
    if Shim.Name = 'hasOwnProperty' then
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

  // Per-realm intrinsic state (Array.prototype, ...) lives on FRealm.  Must
  // be assigned before any value is constructed so lazy prototype init in
  // value units finds a realm to write into.  The previous engine's realm
  // (if any) was freed in Destroy, so its prototype state is gone.
  // Save any pre-existing CurrentRealm so a nested TGocciaEngine on the same
  // thread can restore the outer realm on teardown rather than clobbering it.
  FPrevRealm := CurrentRealm;
  FRealm := TGocciaRealm.Create;
  SetCurrentRealm(FRealm);

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
  FInterpreter.JSXEnabled := ppJSX in FPreprocessors;

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
    FBuiltinSet.Free;
    FBuiltinMap.Free;
    FBuiltinWeakSet.Free;
    FBuiltinWeakMap.Free;
    FBuiltinPromise.Free;
    FBuiltinTemporal.Free;
    FBuiltinIntl.Free;
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
      // Restore the realm that was current when this engine was constructed
      // (typically nil; for nested engines, the outer engine's realm).  Only
      // touch CurrentRealm if it still points at our realm — preserve any
      // intentional reassignment by intervening code.
      if CurrentRealm = FRealm then
        SetCurrentRealm(FPrevRealm);
      FRealm.Free;
      FRealm := nil;
      FPrevRealm := nil;
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
  FBuiltinSet := TGocciaGlobalSet.Create(CONSTRUCTOR_SET, Scope, ThrowError);
  FBuiltinMap := TGocciaGlobalMap.Create(CONSTRUCTOR_MAP, Scope, ThrowError);
  FBuiltinWeakSet := TGocciaGlobalWeakSet.Create(CONSTRUCTOR_WEAK_SET, Scope, ThrowError);
  FBuiltinWeakMap := TGocciaGlobalWeakMap.Create(CONSTRUCTOR_WEAK_MAP, Scope, ThrowError);
  FBuiltinPromise := TGocciaGlobalPromise.Create(CONSTRUCTOR_PROMISE, Scope, ThrowError);
  FBuiltinTemporal := TGocciaTemporalBuiltin.Create('Temporal', Scope, ThrowError);
  FBuiltinIntl := TGocciaIntlBuiltin.Create('Intl', Scope, ThrowError);
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
  if Assigned(FBuiltinArrayBuffer) then
    for Key in FBuiltinArrayBuffer.BuiltinObject.GetAllPropertyNames do
      ArrayBufferConstructor.SetProperty(Key, FBuiltinArrayBuffer.BuiltinObject.GetProperty(Key));
  FInterpreter.GlobalScope.DefineLexicalBinding(CONSTRUCTOR_ARRAY_BUFFER, ArrayBufferConstructor, dtConst, True);

  SharedArrayBufferConstructor := TGocciaSharedArrayBufferClassValue.Create(CONSTRUCTOR_SHARED_ARRAY_BUFFER, nil);
  TGocciaSharedArrayBufferValue.ExposePrototype(SharedArrayBufferConstructor);
  SharedArrayBufferConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
  FInterpreter.GlobalScope.DefineLexicalBinding(CONSTRUCTOR_SHARED_ARRAY_BUFFER, SharedArrayBufferConstructor, dtConst, True);

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
end;

procedure TGocciaEngine.RefreshGlobalThis;
begin
  RegisterGlobalThis;
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
  SavedNonStrictMode: Boolean;
begin
  if (FSourceType = stModule) and (FExecutor is TGocciaBytecodeExecutor) then
  begin
    SavedNonStrictMode := TGocciaBytecodeExecutor(FExecutor).NonStrictMode;
    TGocciaBytecodeExecutor(FExecutor).NonStrictMode := False;
    try
      Result := FExecutor.CompileModule(AProgram);
    finally
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
  GC: TGarbageCollector;
begin
  FillChar(FLastTiming, SizeOf(FLastTiming), 0);
  FLastTiming.FileName := FSourcePath;
  StartTime := GetNanoseconds;
  PipelineResult := nil;

  try
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
        ModuleContext := FInterpreter.CreateEvaluationContext;
        ModuleContext.Scope := ModuleScope;
        ModuleContext.CurrentFilePath := FSourcePath;
        ModuleContext.NonStrictMode := False;
        ExecStart := GetNanoseconds;
        ModuleResult := nil;
        GC := TGarbageCollector.Instance;
        if Assigned(GC) then
          GC.AddTempRoot(ModuleScope);
        try
          ModuleResult := FExecutor.EvaluateModuleBody(
            PipelineResult.ProgramNode, ModuleContext);
          if Assigned(ModuleResult) and Assigned(GC) then
            GC.AddTempRoot(ModuleResult);
          try
            WaitForRuntimeIdle;
            FLastTiming.Result := ModuleResult;
          finally
            if Assigned(ModuleResult) and Assigned(GC) then
              GC.RemoveTempRoot(ModuleResult);
          end;
        finally
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
  if FExecutor is TGocciaBytecodeExecutor then
    TGocciaBytecodeExecutor(FExecutor).NonStrictMode :=
      cfNonStrictMode in AValue;
end;

procedure TGocciaEngine.ThrowError(const AMessage: string; const ALine, AColumn: Integer);
begin
  raise TGocciaRuntimeError.Create(AMessage, ALine, AColumn, FSourcePath, FSourceLines);
end;

function TGocciaEngine.CompileDynamicFunction(
  const AParamsSources: array of string;
  const ABodySource: string): TGocciaFunctionBase;
var
  ParamStr: string;
  I: Integer;
  BodyParseResult: TGocciaFunctionBodyParseResult;
  PipelineOptions: TGocciaSourcePipelineOptions;
  ProgramNode: TGocciaProgram;
  ResultValue: TGocciaValue;
begin
  // ES2026 §20.2.1.1: parse as  function anonymous(P) { body }
  // We use method shorthand so the function gets its own this binding.
  // The member access extracts the function from the wrapper object.
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
  PipelineOptions.Preprocessors := [];
  PipelineOptions.Compatibility := FCompatibility;
  PipelineOptions.SourceType := stScript;
  if not TGocciaSourcePipeline.ParseFunctionParameters(ParamStr,
    '<Function:params>', PipelineOptions) then
    ThrowSyntaxError('Invalid parameter list for Function constructor');

  // Validate body: must parse as a valid function body.
  // An arrow wrapper ensures the body is enclosed in matching braces.
  BodyParseResult := TGocciaSourcePipeline.ParseFunctionBodyWrapper(
    ABodySource, '<Function:body>', PipelineOptions);
  if not BodyParseResult.IsValid then
    ThrowSyntaxError('Invalid body for Function constructor');

  // Both pieces validated — safe to assemble the wrapper
  ProgramNode := TGocciaSourcePipeline.ParseDynamicFunctionWrapper(ParamStr,
    ABodySource, '<Function>', PipelineOptions);
  try
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
