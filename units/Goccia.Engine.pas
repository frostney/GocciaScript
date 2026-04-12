unit Goccia.Engine;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Arguments.Collection,
  Goccia.AST.Node,
  Goccia.Builtins.Base,
  Goccia.Builtins.Benchmark,
  Goccia.Builtins.Console,
  Goccia.Builtins.DisposableStack,
  Goccia.Builtins.GlobalArray,
  Goccia.Builtins.GlobalArrayBuffer,
  Goccia.Builtins.GlobalFFI,
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
  Goccia.Builtins.GlobalTextDecoder,
  Goccia.Builtins.GlobalTextEncoder,
  Goccia.Builtins.GlobalURL,
  Goccia.Builtins.JSON,
  Goccia.Builtins.JSON5,
  Goccia.Builtins.JSONL,
  Goccia.Builtins.Math,
  Goccia.Builtins.Performance,
  Goccia.Builtins.Semver,
  Goccia.Builtins.Temporal,
  Goccia.Builtins.TestAssertions,
  Goccia.Builtins.TOML,
  Goccia.Builtins.YAML,
  Goccia.Interpreter,
  Goccia.JSON,
  Goccia.JSON5,
  Goccia.JSONL,
  Goccia.JSX.SourceMap,
  Goccia.Modules,
  Goccia.Modules.ContentProvider,
  Goccia.Modules.Loader,
  Goccia.Modules.Resolver,
  Goccia.ObjectModel,
  Goccia.ObjectModel.Engine,
  Goccia.Parser,
  Goccia.TextFiles,
  Goccia.TOML,
  Goccia.Values.ClassValue,
  Goccia.Values.FunctionBase,
  Goccia.Values.HoleValue,
  Goccia.Values.IteratorValue,
  Goccia.Values.Primitives,
  Goccia.Values.TypedArrayValue,
  Goccia.YAML;

type
  TGocciaGlobalBuiltin = (
    ggTestAssertions,
    ggBenchmark,
    ggFFI
  );

  TGocciaGlobalBuiltins = set of TGocciaGlobalBuiltin;

  TGocciaPreprocessor = (ppJSX);
  TGocciaPreprocessors = set of TGocciaPreprocessor;

  TGocciaCompatibility = (cfASI);
  TGocciaCompatibilityFlags = set of TGocciaCompatibility;

  TGocciaScriptResult = record
    Result: TGocciaValue;
    LexTimeNanoseconds: Int64;
    ParseTimeNanoseconds: Int64;
    CompileTimeNanoseconds: Int64;
    ExecuteTimeNanoseconds: Int64;
    TotalTimeNanoseconds: Int64;
    FileName: string;
  end;

type
  TGocciaEngine = class
  public
    const DefaultPreprocessors: TGocciaPreprocessors = [ppJSX];
    const DefaultCompatibility: TGocciaCompatibilityFlags = [];
  private
    FInterpreter: TGocciaInterpreter;
    FFileName: string;
    FSourceLines: TStringList;
    FInjectedGlobals: TStringList;
    FGlobals: TGocciaGlobalBuiltins;
    FModuleLoader: TGocciaModuleLoader;
    FOwnsModuleLoader: Boolean;

    // Built-in objects
    FBuiltinConsole: TGocciaConsole;
    FBuiltinMath: TGocciaMath;
    FBuiltinGlobalObject: TGocciaGlobalObject;
    FBuiltinGlobalArray: TGocciaGlobalArray;
    FBuiltinGlobalNumber: TGocciaGlobalNumber;
    FBuiltinRegExp: TGocciaGlobalRegExp;
    FBuiltinGlobalString: TGocciaGlobalString;
    FBuiltinGlobals: TGocciaGlobals;
    FBuiltinJSON: TGocciaJSONBuiltin;
    FBuiltinJSON5: TGocciaJSON5Builtin;
    FBuiltinTOML: TGocciaTOMLBuiltin;
    FBuiltinJSONL: TGocciaJSONLBuiltin;
    FBuiltinYAML: TGocciaYAMLBuiltin;
    FBuiltinSymbol: TGocciaGlobalSymbol;
    FBuiltinSet: TGocciaGlobalSet;
    FBuiltinMap: TGocciaGlobalMap;
    FBuiltinPerformance: TGocciaPerformance;
    FBuiltinPromise: TGocciaGlobalPromise;
    FBuiltinTestAssertions: TGocciaTestAssertions;
    FBuiltinBenchmark: TGocciaBenchmark;
    FBuiltinTemporal: TGocciaTemporalBuiltin;
    FBuiltinArrayBuffer: TGocciaGlobalArrayBuffer;
    FBuiltinProxy: TGocciaGlobalProxy;
    FBuiltinFFI: TGocciaGlobalFFI;
    FBuiltinReflect: TGocciaGlobalReflect;
    FBuiltinTextEncoder: TGocciaGlobalTextEncoder;
    FBuiltinTextDecoder: TGocciaGlobalTextDecoder;
    FBuiltinURL: TGocciaGlobalURL;
    FBuiltinURLSearchParams: TGocciaGlobalURLSearchParams;
    FBuiltinDisposableStack: TGocciaBuiltinDisposableStack;
    FPreviousExceptionMask: TFPUExceptionMask;
    FPreprocessors: TGocciaPreprocessors;
    FCompatibility: TGocciaCompatibilityFlags;
    FStrictTypes: Boolean;
    FShims: TStringList;
    FSuppressWarnings: Boolean;
    FLastTiming: TGocciaScriptResult;
    function GetASIEnabled: Boolean;
    procedure SetASIEnabled(const AValue: Boolean);
    procedure SetPreprocessors(const AValue: TGocciaPreprocessors);
    procedure SetCompatibility(const AValue: TGocciaCompatibilityFlags);

    procedure PinSingletons;
    procedure RegisterBuiltIns;
    procedure RegisterBuiltinConstructors;
    procedure RegisterTypedArrayConstructor(const AName: string; const AKind: TGocciaTypedArrayKind; const AObjectConstructor: TGocciaClassValue);
    procedure RegisterGlobalThis;
    procedure RegisterGocciaScriptGlobal;
    procedure Initialize(const AFileName: string; const ASourceLines: TStringList;
      const AGlobals: TGocciaGlobalBuiltins; const AModuleLoader: TGocciaModuleLoader;
      const AOwnsModuleLoader: Boolean);
    function GetContentProvider: TGocciaModuleContentProvider;
    function GetResolver: TGocciaModuleResolver;
    function SpeciesGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure PrintParserWarnings(const AParser: TGocciaParser; const ASourceMap: TGocciaSourceMap = nil);
    procedure ThrowError(const AMessage: string; const ALine, AColumn: Integer);
  public
    constructor Create(const AFileName: string; const ASourceLines: TStringList; const AGlobals: TGocciaGlobalBuiltins); overload;
    constructor Create(const AFileName: string; const ASourceLines: TStringList; const AGlobals: TGocciaGlobalBuiltins; const AResolver: TGocciaModuleResolver); overload;
    constructor Create(const AFileName: string; const ASourceLines: TStringList; const AGlobals: TGocciaGlobalBuiltins; const AModuleLoader: TGocciaModuleLoader); overload;
    destructor Destroy; override;

    function Execute: TGocciaScriptResult;
    function ExecuteProgram(const AProgram: TGocciaProgram): TGocciaValue;

    procedure AddAlias(const APattern, AReplacement: string);
    procedure InjectGlobal(const AKey: string; const AValue: TGocciaValue);
    procedure InjectGlobalsFromJSON(const AJsonString: string);
    procedure InjectGlobalsFromJSON5(const AJSON5String: UTF8String);
    procedure InjectGlobalsFromTOML(const ATOMLString: UTF8String);
    procedure InjectGlobalsFromYAML(const AYamlString: string);
    procedure InjectGlobalsFromModule(const APath: string);
    procedure RegisterGlobalModule(const AName: string; const AModule: TGocciaModule);

    class function RunScript(const ASource: string; const AFileName: string; const AGlobals: TGocciaGlobalBuiltins): TGocciaScriptResult; overload;
    class function RunScript(const ASource: string; const AFileName: string = 'inline.goccia'): TGocciaScriptResult; overload;
    class function RunScriptFromFile(const AFileName: string; const AGlobals: TGocciaGlobalBuiltins): TGocciaScriptResult; overload;
    class function RunScriptFromFile(const AFileName: string): TGocciaScriptResult; overload;
    class function RunScriptFromStringList(const ASource: TStringList; const AFileName: string; const AGlobals: TGocciaGlobalBuiltins): TGocciaScriptResult; overload;
    class function RunScriptFromStringList(const ASource: TStringList; const AFileName: string): TGocciaScriptResult; overload;

    property Interpreter: TGocciaInterpreter read FInterpreter;
    property Resolver: TGocciaModuleResolver read GetResolver;
    property ContentProvider: TGocciaModuleContentProvider read GetContentProvider;
    property BuiltinConsole: TGocciaConsole read FBuiltinConsole;
    property BuiltinMath: TGocciaMath read FBuiltinMath;
    property BuiltinGlobalObject: TGocciaGlobalObject read FBuiltinGlobalObject;
    property BuiltinGlobalArray: TGocciaGlobalArray read FBuiltinGlobalArray;
    property BuiltinGlobalNumber: TGocciaGlobalNumber read FBuiltinGlobalNumber;
    property BuiltinGlobals: TGocciaGlobals read FBuiltinGlobals;
    property BuiltinJSON: TGocciaJSONBuiltin read FBuiltinJSON;
    property BuiltinJSON5: TGocciaJSON5Builtin read FBuiltinJSON5;
    property BuiltinTOML: TGocciaTOMLBuiltin read FBuiltinTOML;
    property BuiltinJSONL: TGocciaJSONLBuiltin read FBuiltinJSONL;
    property BuiltinYAML: TGocciaYAMLBuiltin read FBuiltinYAML;
    property BuiltinSymbol: TGocciaGlobalSymbol read FBuiltinSymbol;
    property BuiltinSet: TGocciaGlobalSet read FBuiltinSet;
    property BuiltinMap: TGocciaGlobalMap read FBuiltinMap;
    property BuiltinPerformance: TGocciaPerformance read FBuiltinPerformance;
    property BuiltinPromise: TGocciaGlobalPromise read FBuiltinPromise;
    property BuiltinTestAssertions: TGocciaTestAssertions read FBuiltinTestAssertions;
    property BuiltinBenchmark: TGocciaBenchmark read FBuiltinBenchmark;
    property BuiltinTemporal: TGocciaTemporalBuiltin read FBuiltinTemporal;
    property BuiltinArrayBuffer: TGocciaGlobalArrayBuffer read FBuiltinArrayBuffer;
    property BuiltinProxy: TGocciaGlobalProxy read FBuiltinProxy;
    property ASIEnabled: Boolean read GetASIEnabled write SetASIEnabled;
    property Preprocessors: TGocciaPreprocessors read FPreprocessors write SetPreprocessors;
    property Compatibility: TGocciaCompatibilityFlags read FCompatibility write SetCompatibility;
    property StrictTypes: Boolean read FStrictTypes write FStrictTypes;
    property Shims: TStringList read FShims;
    property BuiltinFFI: TGocciaGlobalFFI read FBuiltinFFI;
    property BuiltinReflect: TGocciaGlobalReflect read FBuiltinReflect;
    property BuiltinURL: TGocciaGlobalURL read FBuiltinURL;
    property BuiltinURLSearchParams: TGocciaGlobalURLSearchParams read FBuiltinURLSearchParams;
    property SuppressWarnings: Boolean read FSuppressWarnings write FSuppressWarnings;
    property LastTiming: TGocciaScriptResult read FLastTiming;
  end;


implementation

uses
  Generics.Collections,
  Math,
  SysUtils,
  TypInfo,

  TimingUtils,

  Goccia.CallStack,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Coverage,
  Goccia.Error,
  Goccia.GarbageCollector,
  Goccia.ImportMeta,
  Goccia.JSX.Transformer,
  Goccia.Lexer,
  Goccia.MicrotaskQueue,
  Goccia.Platform,
  Goccia.Scope,
  Goccia.Scope.Redeclaration,
  Goccia.Spec,
  Goccia.Token,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.ArrayValue,
  Goccia.Values.BooleanObjectValue,
  Goccia.Values.MapValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.NumberObjectValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.SetValue,
  Goccia.Values.SharedArrayBufferValue,
  Goccia.Values.StringObjectValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.TextDecoderValue,
  Goccia.Values.TextEncoderValue,
  Goccia.Values.Uint8ArrayEncoding,
  Goccia.Values.URLSearchParamsValue,
  Goccia.Values.URLValue,
  Goccia.Version;

constructor TGocciaEngine.Create(const AFileName: string; const ASourceLines: TStringList; const AGlobals: TGocciaGlobalBuiltins);
begin
  Initialize(AFileName, ASourceLines, AGlobals,
    TGocciaModuleLoader.Create(AFileName), True);
end;

constructor TGocciaEngine.Create(const AFileName: string; const ASourceLines: TStringList; const AGlobals: TGocciaGlobalBuiltins; const AResolver: TGocciaModuleResolver);
begin
  Initialize(AFileName, ASourceLines, AGlobals,
    TGocciaModuleLoader.Create(AFileName, AResolver), True);
end;

constructor TGocciaEngine.Create(const AFileName: string;
  const ASourceLines: TStringList; const AGlobals: TGocciaGlobalBuiltins;
  const AModuleLoader: TGocciaModuleLoader);
begin
  Initialize(AFileName, ASourceLines, AGlobals, AModuleLoader, False);
end;

procedure TGocciaEngine.Initialize(const AFileName: string;
  const ASourceLines: TStringList; const AGlobals: TGocciaGlobalBuiltins;
  const AModuleLoader: TGocciaModuleLoader; const AOwnsModuleLoader: Boolean);
begin
  FPreviousExceptionMask := GetExceptionMask;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  FFileName := AFileName;
  FSourceLines := ASourceLines;
  FGlobals := AGlobals;
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

  FPreprocessors := DefaultPreprocessors;
  FCompatibility := DefaultCompatibility;
  FStrictTypes := False;
  FShims := TStringList.Create;

  FInterpreter := TGocciaInterpreter.Create(AFileName, ASourceLines,
    FModuleLoader);
  FInterpreter.JSXEnabled := ppJSX in FPreprocessors;

  TGarbageCollector.Instance.AddRootObject(FInterpreter.GlobalScope);

  FInjectedGlobals := TStringList.Create;
  PinSingletons;
  RegisterBuiltIns;
end;

destructor TGocciaEngine.Destroy;
begin
  try
    if Assigned(TGarbageCollector.Instance) and Assigned(FInterpreter) then
      TGarbageCollector.Instance.RemoveRootObject(FInterpreter.GlobalScope);

    FBuiltinConsole.Free;
    FBuiltinMath.Free;
    FBuiltinGlobalObject.Free;
    FBuiltinGlobalArray.Free;
    FBuiltinGlobalNumber.Free;
    FBuiltinRegExp.Free;
  FBuiltinGlobalString.Free;
  FBuiltinGlobals.Free;
  FBuiltinJSON.Free;
  FBuiltinJSON5.Free;
  FBuiltinTOML.Free;
    FBuiltinJSONL.Free;
    FBuiltinYAML.Free;
    FBuiltinSymbol.Free;
    FBuiltinSet.Free;
    FBuiltinMap.Free;
    FBuiltinPerformance.Free;
    FBuiltinPromise.Free;
    FBuiltinTestAssertions.Free;
    FBuiltinBenchmark.Free;
    FBuiltinTemporal.Free;
    FBuiltinArrayBuffer.Free;
    FBuiltinProxy.Free;
    FBuiltinFFI.Free;
    FBuiltinReflect.Free;
    FBuiltinTextEncoder.Free;
    FBuiltinTextDecoder.Free;
    FBuiltinURL.Free;
    FBuiltinURLSearchParams.Free;
    ClearImportMetaCache;
    FInjectedGlobals.Free;
    FShims.Free;
    FInterpreter.Free;
    if FOwnsModuleLoader then
      FModuleLoader.Free;
  finally
    SetExceptionMask(FPreviousExceptionMask);
  end;
  inherited;
end;

procedure TGocciaEngine.PinSingletons;
begin
  PinPrimitiveSingletons;
  TGarbageCollector.Instance.PinObject(TGocciaHoleValue.HoleValue);
end;

procedure TGocciaEngine.RegisterBuiltIns;
var
  Scope: TGocciaScope;
begin
  Scope := FInterpreter.GlobalScope;

  // Standard built-ins: always registered.
  FBuiltinConsole := TGocciaConsole.Create('console', Scope, ThrowError);
  FBuiltinMath := TGocciaMath.Create('Math', Scope, ThrowError);
  FBuiltinGlobalObject := TGocciaGlobalObject.Create(CONSTRUCTOR_OBJECT, Scope, ThrowError);
  FBuiltinGlobalArray := TGocciaGlobalArray.Create(CONSTRUCTOR_ARRAY, Scope, ThrowError);
  FBuiltinGlobalNumber := TGocciaGlobalNumber.Create(CONSTRUCTOR_NUMBER, Scope, ThrowError);
  FBuiltinJSON := TGocciaJSONBuiltin.Create('JSON', Scope, ThrowError);
  FBuiltinJSON5 := TGocciaJSON5Builtin.Create('JSON5', Scope, ThrowError);
  FBuiltinJSONL := TGocciaJSONLBuiltin.Create('JSONL', Scope, ThrowError);
  FBuiltinTOML := TGocciaTOMLBuiltin.Create('TOML', Scope, ThrowError);
  FBuiltinYAML := TGocciaYAMLBuiltin.Create('YAML', Scope, ThrowError);
  FBuiltinSymbol := TGocciaGlobalSymbol.Create(CONSTRUCTOR_SYMBOL, Scope, ThrowError);
  FBuiltinSet := TGocciaGlobalSet.Create(CONSTRUCTOR_SET, Scope, ThrowError);
  FBuiltinMap := TGocciaGlobalMap.Create(CONSTRUCTOR_MAP, Scope, ThrowError);
  FBuiltinPerformance := TGocciaPerformance.Create('performance', Scope, ThrowError);
  FBuiltinPromise := TGocciaGlobalPromise.Create(CONSTRUCTOR_PROMISE, Scope, ThrowError);
  FBuiltinTemporal := TGocciaTemporalBuiltin.Create('Temporal', Scope, ThrowError);
  FBuiltinArrayBuffer := TGocciaGlobalArrayBuffer.Create(CONSTRUCTOR_ARRAY_BUFFER, Scope, ThrowError);
  FBuiltinProxy := TGocciaGlobalProxy.Create(Scope);
  FBuiltinReflect := TGocciaGlobalReflect.Create('Reflect', Scope, ThrowError);
  FBuiltinTextEncoder := TGocciaGlobalTextEncoder.Create(
    CONSTRUCTOR_TEXT_ENCODER, Scope, ThrowError);
  FBuiltinTextDecoder := TGocciaGlobalTextDecoder.Create(
    CONSTRUCTOR_TEXT_DECODER, Scope, ThrowError);
  FBuiltinURL := TGocciaGlobalURL.Create(CONSTRUCTOR_URL, Scope, ThrowError);
  FBuiltinURLSearchParams := TGocciaGlobalURLSearchParams.Create(
    CONSTRUCTOR_URL_SEARCH_PARAMS, Scope, ThrowError);

  // Special-purpose built-ins: flag-gated.
  if ggTestAssertions in FGlobals then
    FBuiltinTestAssertions := TGocciaTestAssertions.Create('TestAssertions', Scope, ThrowError);
  if ggBenchmark in FGlobals then
    FBuiltinBenchmark := TGocciaBenchmark.Create('Benchmark', Scope, ThrowError);
  if ggFFI in FGlobals then
    FBuiltinFFI := TGocciaGlobalFFI.Create(CONSTRUCTOR_FFI, Scope, ThrowError);

  // Always-registered built-ins
  FBuiltinGlobalString := TGocciaGlobalString.Create(CONSTRUCTOR_STRING, Scope, ThrowError);
  FBuiltinGlobals := TGocciaGlobals.Create('Globals', Scope, ThrowError);
  FBuiltinDisposableStack := TGocciaBuiltinDisposableStack.Create('DisposableStack', Scope, ThrowError);
  Scope.DefineLexicalBinding(CONSTRUCTOR_ITERATOR, TGocciaIteratorValue.CreateGlobalObject, dtConst);
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

procedure ExposeTextEncoderPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaTextEncoderValue.ExposePrototype(AConstructor);
end;

procedure ExposeTextDecoderPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaTextDecoderValue.ExposePrototype(AConstructor);
end;

procedure ExposeURLPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaURLValue.ExposePrototype(AConstructor);
end;

procedure ExposeURLSearchParamsPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaURLSearchParamsValue.ExposePrototype(AConstructor);
end;

procedure TGocciaEngine.RegisterBuiltinConstructors;
var
  Key: string;
  GenericConstructor: TGocciaClassValue;
  ObjectConstructor, FunctionConstructor: TGocciaClassValue;
  ArrayConstructor: TGocciaArrayClassValue;
  MapConstructor: TGocciaMapClassValue;
  SetConstructor: TGocciaSetClassValue;
  ArrayBufferConstructor: TGocciaArrayBufferClassValue;
  SharedArrayBufferConstructor: TGocciaSharedArrayBufferClassValue;
  StringConstructor: TGocciaStringClassValue;
  NumberConstructor: TGocciaNumberClassValue;
  BooleanConstructor: TGocciaBooleanClassValue;
  PerformanceConstructor: TGocciaNativeFunctionValue;
  URLConstructor: TGocciaURLClassValue;
  URLSearchParamsConstructor: TGocciaURLSearchParamsClassValue;
  TypeDef: TGocciaTypeDefinition;
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

  TypeDef.ConstructorName := CONSTRUCTOR_TEXT_ENCODER;
  TypeDef.Kind := gtdkNativeInstanceType;
  TypeDef.ClassValueClass := TGocciaTextEncoderClassValue;
  TypeDef.ExposePrototype := @ExposeTextEncoderPrototype;
  TypeDef.PrototypeProvider := nil;
  TypeDef.StaticSource := nil;
  TypeDef.PrototypeParent := ObjectConstructor.Prototype;
  TypeDef.AddSpeciesGetter := False;
  RegisterTypeDefinition(FInterpreter.GlobalScope, TypeDef, SpeciesGetter, GenericConstructor);

  TypeDef.ConstructorName := CONSTRUCTOR_TEXT_DECODER;
  TypeDef.Kind := gtdkNativeInstanceType;
  TypeDef.ClassValueClass := TGocciaTextDecoderClassValue;
  TypeDef.ExposePrototype := @ExposeTextDecoderPrototype;
  TypeDef.PrototypeProvider := nil;
  TypeDef.StaticSource := nil;
  TypeDef.PrototypeParent := ObjectConstructor.Prototype;
  TypeDef.AddSpeciesGetter := False;
  RegisterTypeDefinition(FInterpreter.GlobalScope, TypeDef, SpeciesGetter, GenericConstructor);

  TypeDef.ConstructorName := CONSTRUCTOR_URL;
  TypeDef.Kind := gtdkCollectionLikeNativeType;
  TypeDef.ClassValueClass := TGocciaURLClassValue;
  TypeDef.ExposePrototype := @ExposeURLPrototype;
  TypeDef.PrototypeProvider := nil;
  TypeDef.StaticSource := BuiltinObjectOrNil(FBuiltinURL);
  TypeDef.PrototypeParent := ObjectConstructor.Prototype;
  TypeDef.AddSpeciesGetter := False;
  RegisterTypeDefinition(FInterpreter.GlobalScope, TypeDef, SpeciesGetter, GenericConstructor);
  URLConstructor := TGocciaURLClassValue(GenericConstructor);

  TypeDef.ConstructorName := CONSTRUCTOR_URL_SEARCH_PARAMS;
  TypeDef.Kind := gtdkCollectionLikeNativeType;
  TypeDef.ClassValueClass := TGocciaURLSearchParamsClassValue;
  TypeDef.ExposePrototype := @ExposeURLSearchParamsPrototype;
  TypeDef.PrototypeProvider := nil;
  TypeDef.StaticSource := BuiltinObjectOrNil(FBuiltinURLSearchParams);
  TypeDef.PrototypeParent := ObjectConstructor.Prototype;
  TypeDef.AddSpeciesGetter := False;
  RegisterTypeDefinition(FInterpreter.GlobalScope, TypeDef, SpeciesGetter, GenericConstructor);
  URLSearchParamsConstructor := TGocciaURLSearchParamsClassValue(GenericConstructor);

  ArrayBufferConstructor := TGocciaArrayBufferClassValue.Create(CONSTRUCTOR_ARRAY_BUFFER, nil);
  TGocciaArrayBufferValue.ExposePrototype(ArrayBufferConstructor);
  ArrayBufferConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
  if Assigned(FBuiltinArrayBuffer) then
    for Key in FBuiltinArrayBuffer.BuiltinObject.GetAllPropertyNames do
      ArrayBufferConstructor.SetProperty(Key, FBuiltinArrayBuffer.BuiltinObject.GetProperty(Key));
  FInterpreter.GlobalScope.DefineLexicalBinding(CONSTRUCTOR_ARRAY_BUFFER, ArrayBufferConstructor, dtConst);

  SharedArrayBufferConstructor := TGocciaSharedArrayBufferClassValue.Create(CONSTRUCTOR_SHARED_ARRAY_BUFFER, nil);
  TGocciaSharedArrayBufferValue.ExposePrototype(SharedArrayBufferConstructor);
  SharedArrayBufferConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
  FInterpreter.GlobalScope.DefineLexicalBinding(CONSTRUCTOR_SHARED_ARRAY_BUFFER, SharedArrayBufferConstructor, dtConst);

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

  FunctionConstructor := TGocciaClassValue.Create('Function', nil);
  TGocciaFunctionBase.SetSharedPrototypeParent(FunctionConstructor.Prototype);
  FInterpreter.GlobalScope.DefineLexicalBinding('Function', FunctionConstructor, dtConst);

  PerformanceConstructor := TGocciaPerformance.CreateInterfaceObject;
  FInterpreter.GlobalScope.DefineLexicalBinding(CONSTRUCTOR_PERFORMANCE, PerformanceConstructor, dtConst);

  RegisterGocciaScriptGlobal;
  RegisterGlobalThis;
end;

procedure TGocciaEngine.RegisterTypedArrayConstructor(const AName: string; const AKind: TGocciaTypedArrayKind; const AObjectConstructor: TGocciaClassValue);
var
  TAConstructor: TGocciaTypedArrayClassValue;
  BPE: TGocciaNumberLiteralValue;
  FromFn, OfFn: TGocciaTypedArrayStaticFrom;
  Encoding: TGocciaUint8ArrayEncoding;
begin
  TAConstructor := TGocciaTypedArrayClassValue.Create(AName, nil, AKind);
  TGocciaTypedArrayValue.ExposePrototype(TAConstructor);
  TGocciaTypedArrayValue.SetSharedPrototypeParent(AObjectConstructor.Prototype);
  BPE := TGocciaNumberLiteralValue.Create(TGocciaTypedArrayValue.BytesPerElement(AKind));
  TAConstructor.SetProperty(PROP_BYTES_PER_ELEMENT, BPE);
  TAConstructor.Prototype.AssignProperty(PROP_BYTES_PER_ELEMENT, BPE);

  FromFn := TGocciaTypedArrayStaticFrom.Create(AKind);
  TAConstructor.SetProperty(PROP_FROM,
    TGocciaNativeFunctionValue.CreateWithoutPrototype(FromFn.TypedArrayFrom, 'from', 1));
  OfFn := TGocciaTypedArrayStaticFrom.Create(AKind);
  TAConstructor.SetProperty(PROP_OF,
    TGocciaNativeFunctionValue.CreateWithoutPrototype(OfFn.TypedArrayOf, 'of', 0));

  // Uint8Array-only: Base64/Hex encoding methods (TC39 Uint8Array Base64)
  if AKind = takUint8 then
  begin
    Encoding := TGocciaUint8ArrayEncoding.Create;

    // Static methods on Uint8Array constructor
    TAConstructor.SetProperty(PROP_FROM_BASE64,
      TGocciaNativeFunctionValue.CreateWithoutPrototype(Encoding.FromBase64, 'fromBase64', 1));
    TAConstructor.SetProperty(PROP_FROM_HEX,
      TGocciaNativeFunctionValue.CreateWithoutPrototype(Encoding.FromHex, 'fromHex', 1));

    // Prototype methods on Uint8Array.prototype
    TAConstructor.Prototype.AssignProperty(PROP_TO_BASE64,
      TGocciaNativeFunctionValue.CreateWithoutPrototype(Encoding.ToBase64, 'toBase64', 0));
    TAConstructor.Prototype.AssignProperty(PROP_TO_HEX,
      TGocciaNativeFunctionValue.CreateWithoutPrototype(Encoding.ToHex, 'toHex', 0));
    TAConstructor.Prototype.AssignProperty(PROP_SET_FROM_BASE64,
      TGocciaNativeFunctionValue.CreateWithoutPrototype(Encoding.SetFromBase64, 'setFromBase64', 1));
    TAConstructor.Prototype.AssignProperty(PROP_SET_FROM_HEX,
      TGocciaNativeFunctionValue.CreateWithoutPrototype(Encoding.SetFromHex, 'setFromHex', 1));

    // Uint8Array instances use constructor's prototype (encoding methods + shared chain)
    TGocciaTypedArrayValue.SetUint8Prototype(TAConstructor.Prototype);
  end;

  FInterpreter.GlobalScope.DefineLexicalBinding(AName, TAConstructor, dtConst);
end;

procedure TGocciaEngine.RegisterGlobalThis;
var
  GlobalThisObj: TGocciaObjectValue;
  Scope: TGocciaScope;
  Name: string;
begin
  Scope := FInterpreter.GlobalScope;
  GlobalThisObj := TGocciaObjectValue.Create;

  for Name in Scope.GetOwnBindingNames do
    GlobalThisObj.AssignProperty(Name, Scope.GetValue(Name));

  GlobalThisObj.AssignProperty('globalThis', GlobalThisObj);
  Scope.DefineLexicalBinding('globalThis', GlobalThisObj, dtConst);
end;

procedure TGocciaEngine.RegisterGocciaScriptGlobal;
const
  PREFIX_LENGTH = 2; // Strip 'gg' prefix from enum names
var
  GocciaObj: TGocciaObjectValue;
  BuildObj: TGocciaObjectValue;
  BuiltInsArray: TGocciaArrayValue;
  ShimsArray: TGocciaArrayValue;
  Flag: TGocciaGlobalBuiltin;
  Name: string;
  I: Integer;
begin
  BuiltInsArray := TGocciaArrayValue.Create;
  for Flag in FGlobals do
  begin
    Name := GetEnumName(TypeInfo(TGocciaGlobalBuiltin), Ord(Flag));
    BuiltInsArray.Elements.Add(TGocciaStringLiteralValue.Create(
      Copy(Name, PREFIX_LENGTH + 1, Length(Name) - PREFIX_LENGTH)));
  end;

  BuildObj := TGocciaObjectValue.Create;
  BuildObj.DefineProperty('os', TGocciaPropertyDescriptorData.Create(
    TGocciaStringLiteralValue.Create(GetBuildOS), [pfEnumerable]));
  BuildObj.DefineProperty('arch', TGocciaPropertyDescriptorData.Create(
    TGocciaStringLiteralValue.Create(GetBuildArch), [pfEnumerable]));

  ShimsArray := TGocciaArrayValue.Create;
  for I := 0 to FShims.Count - 1 do
    ShimsArray.Elements.Add(TGocciaStringLiteralValue.Create(FShims[I]));

  GocciaObj := TGocciaObjectValue.Create;
  GocciaObj.AssignProperty('version', TGocciaStringLiteralValue.Create(GetVersion));
  GocciaObj.AssignProperty('commit', TGocciaStringLiteralValue.Create(GetCommit));
  GocciaObj.AssignProperty('builtIns', BuiltInsArray);
  if FStrictTypes then
    GocciaObj.AssignProperty(PROP_STRICT_TYPES, TGocciaBooleanLiteralValue.TrueValue)
  else
    GocciaObj.AssignProperty(PROP_STRICT_TYPES, TGocciaBooleanLiteralValue.FalseValue);
  GocciaObj.AssignProperty(SEMVER_NAMESPACE_PROPERTY, CreateSemverNamespace);
  GocciaObj.AssignProperty('build', BuildObj);
  GocciaObj.DefineProperty('spec', TGocciaPropertyDescriptorData.Create(
    CreateSpecObject, [pfEnumerable]));
  GocciaObj.DefineProperty('proposal', TGocciaPropertyDescriptorData.Create(
    CreateProposalObject, [pfEnumerable]));
  GocciaObj.DefineProperty('shims', TGocciaPropertyDescriptorData.Create(
    ShimsArray, [pfEnumerable]));

  FInterpreter.GlobalScope.DefineLexicalBinding('Goccia', GocciaObj, dtConst);
end;

function TGocciaEngine.GetContentProvider: TGocciaModuleContentProvider;
begin
  Result := FModuleLoader.ContentProvider;
end;

function TGocciaEngine.GetResolver: TGocciaModuleResolver;
begin
  Result := FModuleLoader.Resolver;
end;

procedure TGocciaEngine.AddAlias(const APattern, AReplacement: string);
begin
  Resolver.AddAlias(APattern, AReplacement);
end;

procedure TGocciaEngine.InjectGlobal(const AKey: string; const AValue: TGocciaValue);
begin
  if FInterpreter.GlobalScope.ContainsOwnLexicalBinding(AKey) then
  begin
    if FInjectedGlobals.IndexOf(AKey) >= 0 then
      FInterpreter.GlobalScope.ForceUpdateBinding(AKey, AValue)
    else
      raise TGocciaRuntimeError.Create(
        'Cannot override built-in global "' + AKey + '" via globals injection.',
        0, 0, FFileName, FSourceLines);
  end
  else
  begin
    FInterpreter.GlobalScope.DefineLexicalBinding(AKey, AValue, dtConst);
    FInjectedGlobals.Add(AKey);
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
    raise TGocciaRuntimeError.Create('Globals JSON must be a top-level object.',
      0, 0, FFileName, nil);

  TGarbageCollector.Instance.AddTempRoot(ParsedValue);
  try
    Obj := TGocciaObjectValue(ParsedValue);
    for Key in Obj.GetOwnPropertyKeys do
      InjectGlobal(Key, Obj.GetProperty(Key));
  finally
    TGarbageCollector.Instance.RemoveTempRoot(ParsedValue);
  end;
end;

procedure TGocciaEngine.InjectGlobalsFromJSON5(const AJSON5String: UTF8String);
var
  Key: string;
  Obj: TGocciaObjectValue;
  ParsedValue: TGocciaValue;
  Parser: TGocciaJSON5Parser;
begin
  Parser := TGocciaJSON5Parser.Create;
  try
    ParsedValue := Parser.Parse(AJSON5String);
  finally
    Parser.Free;
  end;

  if not (ParsedValue is TGocciaObjectValue) then
    raise TGocciaRuntimeError.Create('Globals JSON5 must be a top-level object.',
      0, 0, FFileName, nil);

  TGarbageCollector.Instance.AddTempRoot(ParsedValue);
  try
    Obj := TGocciaObjectValue(ParsedValue);
    for Key in Obj.GetOwnPropertyKeys do
      InjectGlobal(Key, Obj.GetProperty(Key));
  finally
    TGarbageCollector.Instance.RemoveTempRoot(ParsedValue);
  end;
end;

procedure TGocciaEngine.InjectGlobalsFromTOML(const ATOMLString: UTF8String);
var
  Key: string;
  Obj: TGocciaObjectValue;
  ParsedValue: TGocciaValue;
  Parser: TGocciaTOMLParser;
begin
  Parser := TGocciaTOMLParser.Create;
  try
    ParsedValue := Parser.Parse(ATOMLString);
  finally
    Parser.Free;
  end;

  if not (ParsedValue is TGocciaObjectValue) then
    raise TGocciaRuntimeError.Create('Globals TOML must be a top-level object.',
      0, 0, FFileName, nil);

  TGarbageCollector.Instance.AddTempRoot(ParsedValue);
  try
    Obj := TGocciaObjectValue(ParsedValue);
    for Key in Obj.GetOwnPropertyKeys do
      InjectGlobal(Key, Obj.GetProperty(Key));
  finally
    TGarbageCollector.Instance.RemoveTempRoot(ParsedValue);
  end;
end;

procedure TGocciaEngine.InjectGlobalsFromYAML(const AYamlString: string);
var
  Documents: TGocciaArrayValue;
  ParsedDocument: TGocciaValue;
  Parser: TGocciaYAMLParser;
  Obj: TGocciaObjectValue;
  Key: string;
begin
  Parser := TGocciaYAMLParser.Create;
  try
    Documents := Parser.ParseDocuments(AYamlString);
  finally
    Parser.Free;
  end;

  try
    if Documents.Elements.Count <> 1 then
      raise TGocciaRuntimeError.Create(
        'Globals YAML must contain exactly one top-level document.',
        0, 0, FFileName, nil);

    ParsedDocument := Documents.Elements[0];
    if not (ParsedDocument is TGocciaObjectValue) then
      raise TGocciaRuntimeError.Create(
        'Globals YAML must be a top-level object.', 0, 0, FFileName, nil);

    TGarbageCollector.Instance.AddTempRoot(ParsedDocument);
    Obj := TGocciaObjectValue(ParsedDocument);
    try
      for Key in Obj.GetOwnPropertyKeys do
        InjectGlobal(Key, Obj.GetProperty(Key));
    finally
      TGarbageCollector.Instance.RemoveTempRoot(ParsedDocument);
    end;
  finally
    Documents.Free;
  end;
end;

procedure TGocciaEngine.InjectGlobalsFromModule(const APath: string);
var
  Module: TGocciaModule;
  ExportPair: TGocciaValueMap.TKeyValuePair;
begin
  Module := FInterpreter.LoadModule(APath, FFileName);
  for ExportPair in Module.ExportsTable do
    InjectGlobal(ExportPair.Key, ExportPair.Value);
end;

procedure TGocciaEngine.RegisterGlobalModule(const AName: string; const AModule: TGocciaModule);
begin
  FInterpreter.GlobalModules.AddOrSetValue(AName, AModule);
end;

function TGocciaEngine.Execute: TGocciaScriptResult;
var
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  Tokens: TObjectList<TGocciaToken>;
  StartTime, LexEnd, ParseEnd, ExecEnd: Int64;
  SourceText: string;
  JSXResult: TGocciaJSXTransformResult;
  SourceMap: TGocciaSourceMap;
  OrigLine, OrigCol: Integer;
begin
  FillChar(FLastTiming, SizeOf(FLastTiming), 0);
  FLastTiming.FileName := FFileName;
  StartTime := GetNanoseconds;

  if Assigned(FSourceLines) then
    SourceText := StringListToLFText(FSourceLines)
  else
    SourceText := '';
  SourceMap := nil;

  if ppJSX in FPreprocessors then
  begin
    JSXResult := TGocciaJSXTransformer.Transform(SourceText);
    SourceText := JSXResult.Source;
    SourceMap := JSXResult.SourceMap;
    if Assigned(SourceMap) then
      WarnIfJSXExtensionMismatch(FFileName);
  end;

  try
    try
      Lexer := TGocciaLexer.Create(SourceText, FFileName);
      try
        Tokens := Lexer.ScanTokens;
        LexEnd := GetNanoseconds;
        FLastTiming.LexTimeNanoseconds := LexEnd - StartTime;

        Parser := TGocciaParser.Create(Tokens, FFileName, Lexer.SourceLines);
        Parser.AutomaticSemicolonInsertion := cfASI in FCompatibility;
        try
          ProgramNode := Parser.Parse;
          PrintParserWarnings(Parser, SourceMap);
          ParseEnd := GetNanoseconds;
          FLastTiming.ParseTimeNanoseconds := ParseEnd - LexEnd;

          if Assigned(TGocciaCoverageTracker.Instance) and
             TGocciaCoverageTracker.Instance.Enabled then
            TGocciaCoverageTracker.Instance.RegisterSourceFile(
              FFileName, CountExecutableLines(Lexer.SourceLines));

          try
            CheckTopLevelRedeclarations(ProgramNode,
              FInterpreter.GlobalScope, FFileName);
            FLastTiming.CompileTimeNanoseconds := 0;
            FLastTiming.Result := FInterpreter.Execute(ProgramNode);
            if Assigned(TGocciaMicrotaskQueue.Instance) then
              TGocciaMicrotaskQueue.Instance.DrainQueue;
            ExecEnd := GetNanoseconds;
            FLastTiming.ExecuteTimeNanoseconds := ExecEnd - ParseEnd;
            FLastTiming.TotalTimeNanoseconds := ExecEnd - StartTime;
          finally
            if Assigned(TGocciaMicrotaskQueue.Instance) then
              TGocciaMicrotaskQueue.Instance.ClearQueue;
            ProgramNode.Free;
          end;
        finally
          Parser.Free;
        end;
      finally
        Lexer.Free;
      end;
    except
      on E: TGocciaError do
      begin
        if Assigned(SourceMap) and
           SourceMap.Translate(E.Line, E.Column, OrigLine, OrigCol) then
          E.TranslatePosition(OrigLine, OrigCol, FSourceLines);
        raise;
      end;
    end;
  finally
    if FLastTiming.TotalTimeNanoseconds = 0 then
      FLastTiming.TotalTimeNanoseconds := GetNanoseconds - StartTime;
    SourceMap.Free;
  end;

  Result := FLastTiming;
end;

function TGocciaEngine.ExecuteProgram(const AProgram: TGocciaProgram): TGocciaValue;
begin
  try
    Result := FInterpreter.Execute(AProgram);
    if Assigned(TGocciaMicrotaskQueue.Instance) then
      TGocciaMicrotaskQueue.Instance.DrainQueue;
  finally
    if Assigned(TGocciaMicrotaskQueue.Instance) then
      TGocciaMicrotaskQueue.Instance.ClearQueue;
  end;
end;

class function TGocciaEngine.RunScript(const ASource: string; const AFileName: string; const AGlobals: TGocciaGlobalBuiltins): TGocciaScriptResult;
var
  SourceList: TStringList;
begin
  SourceList := CreateUTF8StringList(ASource);
  try
    Result := RunScriptFromStringList(SourceList, AFileName, AGlobals);
  finally
    SourceList.Free;
  end;
end;

class function TGocciaEngine.RunScript(const ASource: string; const AFileName: string): TGocciaScriptResult;
begin
  Result := RunScript(ASource, AFileName, []);
end;

class function TGocciaEngine.RunScriptFromFile(const AFileName: string; const AGlobals: TGocciaGlobalBuiltins): TGocciaScriptResult;
var
  Source: TStringList;
begin
  Source := CreateUTF8FileTextLines(ReadUTF8FileText(AFileName));
  try
    Result := RunScriptFromStringList(Source, AFileName, AGlobals);
  finally
    Source.Free;
  end;
end;

class function TGocciaEngine.RunScriptFromFile(const AFileName: string): TGocciaScriptResult;
begin
  Result := RunScriptFromFile(AFileName, []);
end;

class function TGocciaEngine.RunScriptFromStringList(const ASource: TStringList; const AFileName: string; const AGlobals: TGocciaGlobalBuiltins): TGocciaScriptResult;
var
  Engine: TGocciaEngine;
begin
  Engine := TGocciaEngine.Create(AFileName, ASource, AGlobals);
  try
    Result := Engine.Execute;
  finally
    Engine.Free;
  end;
end;

class function TGocciaEngine.RunScriptFromStringList(const ASource: TStringList; const AFileName: string): TGocciaScriptResult;
begin
  Result := RunScriptFromStringList(ASource, AFileName, []);
end;

procedure TGocciaEngine.PrintParserWarnings(const AParser: TGocciaParser; const ASourceMap: TGocciaSourceMap);
var
  Warning: TGocciaParserWarning;
  I, OrigLine, OrigCol: Integer;
begin
  if FSuppressWarnings then
    Exit;
  for I := 0 to AParser.WarningCount - 1 do
  begin
    Warning := AParser.GetWarning(I);
    WriteLn(Format('Warning: %s', [Warning.Message]));
    if Warning.Suggestion <> '' then
      WriteLn(Format('  Suggestion: %s', [Warning.Suggestion]));
    if Assigned(ASourceMap) and ASourceMap.Translate(Warning.Line, Warning.Column, OrigLine, OrigCol) then
      WriteLn(Format('  --> %s:%d:%d', [FFileName, OrigLine, OrigCol]))
    else
      WriteLn(Format('  --> %s:%d:%d', [FFileName, Warning.Line, Warning.Column]));
  end;
end;

function TGocciaEngine.SpeciesGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := AThisValue;
end;

function TGocciaEngine.GetASIEnabled: Boolean;
begin
  Result := cfASI in FCompatibility;
end;

procedure TGocciaEngine.SetASIEnabled(const AValue: Boolean);
begin
  if AValue then
    Include(FCompatibility, cfASI)
  else
    Exclude(FCompatibility, cfASI);
  FInterpreter.ASIEnabled := AValue;
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
end;

procedure TGocciaEngine.ThrowError(const AMessage: string; const ALine, AColumn: Integer);
begin
  raise TGocciaRuntimeError.Create(AMessage, ALine, AColumn, FFileName, FSourceLines);
end;

end.
