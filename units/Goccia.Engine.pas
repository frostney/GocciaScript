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
  Goccia.Builtins.GlobalArray,
  Goccia.Builtins.GlobalArrayBuffer,
  Goccia.Builtins.GlobalMap,
  Goccia.Builtins.GlobalNumber,
  Goccia.Builtins.GlobalObject,
  Goccia.Builtins.GlobalPromise,
  Goccia.Builtins.GlobalRegExp,
  Goccia.Builtins.Globals,
  Goccia.Builtins.GlobalSet,
  Goccia.Builtins.GlobalString,
  Goccia.Builtins.GlobalSymbol,
  Goccia.Builtins.JSON,
  Goccia.Builtins.JSON5,
  Goccia.Builtins.JSONL,
  Goccia.Builtins.Math,
  Goccia.Builtins.Performance,
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
    ggConsole,
    ggMath,
    ggGlobalObject,
    ggGlobalArray,
    ggGlobalNumber,
    ggPromise,
    ggJSON,
    ggJSON5,
    ggJSONL,
    ggTOML,
    ggYAML,
    ggSymbol,
    ggSet,
    ggMap,
    ggPerformance,
    ggTestAssertions,
    ggBenchmark,
    ggTemporal,
    ggJSX,
    ggArrayBuffer
  );

  TGocciaGlobalBuiltins = set of TGocciaGlobalBuiltin;

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
    const DefaultGlobals: TGocciaGlobalBuiltins = [ggConsole, ggMath, ggGlobalObject, ggGlobalArray, ggGlobalNumber, ggPromise, ggJSON, ggJSON5, ggJSONL, ggTOML, ggYAML, ggSymbol, ggSet, ggMap, ggPerformance, ggTemporal, ggJSX, ggArrayBuffer];
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
    FPreviousExceptionMask: TFPUExceptionMask;
    FSuppressWarnings: Boolean;

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
    property SuppressWarnings: Boolean read FSuppressWarnings write FSuppressWarnings;
  end;


implementation

uses
  Generics.Collections,
  Math,
  SysUtils,
  TypInfo,

  GarbageCollector.Generic,
  TimingUtils,

  Goccia.CallStack,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.JSX.Transformer,
  Goccia.Lexer,
  Goccia.MicrotaskQueue,
  Goccia.Scope,
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

  FInterpreter := TGocciaInterpreter.Create(AFileName, ASourceLines,
    FModuleLoader);
  FInterpreter.JSXEnabled := ggJSX in FGlobals;

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
    FInjectedGlobals.Free;
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

  // Flag-gated built-ins: each creates a wrapper that registers its
  // native methods/properties into the global scope.
  if ggConsole in FGlobals then
    FBuiltinConsole := TGocciaConsole.Create('console', Scope, ThrowError);
  if ggMath in FGlobals then
    FBuiltinMath := TGocciaMath.Create('Math', Scope, ThrowError);
  if ggGlobalObject in FGlobals then
    FBuiltinGlobalObject := TGocciaGlobalObject.Create(CONSTRUCTOR_OBJECT, Scope, ThrowError);
  if ggGlobalArray in FGlobals then
    FBuiltinGlobalArray := TGocciaGlobalArray.Create(CONSTRUCTOR_ARRAY, Scope, ThrowError);
  if ggGlobalNumber in FGlobals then
    FBuiltinGlobalNumber := TGocciaGlobalNumber.Create(CONSTRUCTOR_NUMBER, Scope, ThrowError);
  if ggJSON in FGlobals then
    FBuiltinJSON := TGocciaJSONBuiltin.Create('JSON', Scope, ThrowError);
  if ggJSON5 in FGlobals then
    FBuiltinJSON5 := TGocciaJSON5Builtin.Create('JSON5', Scope, ThrowError);
  if ggJSONL in FGlobals then
    FBuiltinJSONL := TGocciaJSONLBuiltin.Create('JSONL', Scope, ThrowError);
  if ggTOML in FGlobals then
    FBuiltinTOML := TGocciaTOMLBuiltin.Create('TOML', Scope, ThrowError);
  if ggYAML in FGlobals then
    FBuiltinYAML := TGocciaYAMLBuiltin.Create('YAML', Scope, ThrowError);
  if ggSymbol in FGlobals then
    FBuiltinSymbol := TGocciaGlobalSymbol.Create(CONSTRUCTOR_SYMBOL, Scope, ThrowError);
  if ggSet in FGlobals then
    FBuiltinSet := TGocciaGlobalSet.Create(CONSTRUCTOR_SET, Scope, ThrowError);
  if ggMap in FGlobals then
    FBuiltinMap := TGocciaGlobalMap.Create(CONSTRUCTOR_MAP, Scope, ThrowError);
  if ggPerformance in FGlobals then
    FBuiltinPerformance := TGocciaPerformance.Create('performance', Scope, ThrowError);
  if ggPromise in FGlobals then
    FBuiltinPromise := TGocciaGlobalPromise.Create(CONSTRUCTOR_PROMISE, Scope, ThrowError);
  if ggTestAssertions in FGlobals then
    FBuiltinTestAssertions := TGocciaTestAssertions.Create('TestAssertions', Scope, ThrowError);
  if ggBenchmark in FGlobals then
    FBuiltinBenchmark := TGocciaBenchmark.Create('Benchmark', Scope, ThrowError);
  if ggTemporal in FGlobals then
    FBuiltinTemporal := TGocciaTemporalBuiltin.Create('Temporal', Scope, ThrowError);
  if ggArrayBuffer in FGlobals then
    FBuiltinArrayBuffer := TGocciaGlobalArrayBuffer.Create(CONSTRUCTOR_ARRAY_BUFFER, Scope, ThrowError);

  // Always-registered built-ins
  FBuiltinGlobalString := TGocciaGlobalString.Create(CONSTRUCTOR_STRING, Scope, ThrowError);
  FBuiltinGlobals := TGocciaGlobals.Create('Globals', Scope, ThrowError);
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

  if ggMap in FGlobals then
  begin
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
  end;

  if ggSet in FGlobals then
  begin
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
  end;

  if ggArrayBuffer in FGlobals then
  begin
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
    RegisterTypedArrayConstructor(CONSTRUCTOR_FLOAT32_ARRAY, takFloat32, ObjectConstructor);
    RegisterTypedArrayConstructor(CONSTRUCTOR_FLOAT64_ARRAY, takFloat64, ObjectConstructor);
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

  FunctionConstructor := TGocciaClassValue.Create('Function', nil);
  TGocciaFunctionBase.SetSharedPrototypeParent(FunctionConstructor.Prototype);
  FInterpreter.GlobalScope.DefineLexicalBinding('Function', FunctionConstructor, dtConst);

  if ggPerformance in FGlobals then
  begin
    PerformanceConstructor := TGocciaPerformance.CreateInterfaceObject;
    FInterpreter.GlobalScope.DefineLexicalBinding(CONSTRUCTOR_PERFORMANCE, PerformanceConstructor, dtConst);
  end;

  RegisterGocciaScriptGlobal;
  RegisterGlobalThis;
end;

procedure TGocciaEngine.RegisterTypedArrayConstructor(const AName: string; const AKind: TGocciaTypedArrayKind; const AObjectConstructor: TGocciaClassValue);
var
  TAConstructor: TGocciaTypedArrayClassValue;
  BPE: TGocciaNumberLiteralValue;
  FromFn, OfFn: TGocciaTypedArrayStaticFrom;
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
  BuiltInsArray: TGocciaArrayValue;
  Flag: TGocciaGlobalBuiltin;
  Name: string;
begin
  BuiltInsArray := TGocciaArrayValue.Create;
  for Flag in FGlobals do
  begin
    Name := GetEnumName(TypeInfo(TGocciaGlobalBuiltin), Ord(Flag));
    BuiltInsArray.Elements.Add(TGocciaStringLiteralValue.Create(Copy(Name, PREFIX_LENGTH + 1, Length(Name) - PREFIX_LENGTH)));
  end;

  GocciaObj := TGocciaObjectValue.Create;
  GocciaObj.AssignProperty('version', TGocciaStringLiteralValue.Create(GetVersion));
  GocciaObj.AssignProperty('commit', TGocciaStringLiteralValue.Create(GetCommit));
  GocciaObj.AssignProperty('builtIns', BuiltInsArray);
  GocciaObj.AssignProperty(PROP_STRICT_TYPES, TGocciaBooleanLiteralValue.FalseValue);

  FInterpreter.GlobalScope.DefineLexicalBinding('GocciaScript', GocciaObj, dtConst);
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
  Result.FileName := FFileName;
  StartTime := GetNanoseconds;

  if Assigned(FSourceLines) then
    SourceText := StringListToLFText(FSourceLines)
  else
    SourceText := '';
  SourceMap := nil;

  if ggJSX in FGlobals then
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
        Result.LexTimeNanoseconds := LexEnd - StartTime;

        Parser := TGocciaParser.Create(Tokens, FFileName, Lexer.SourceLines);
        try
          ProgramNode := Parser.Parse;
          PrintParserWarnings(Parser, SourceMap);
          ParseEnd := GetNanoseconds;
          Result.ParseTimeNanoseconds := ParseEnd - LexEnd;

          try
            Result.CompileTimeNanoseconds := 0;
            Result.Result := FInterpreter.Execute(ProgramNode);
            if Assigned(TGocciaMicrotaskQueue.Instance) then
              TGocciaMicrotaskQueue.Instance.DrainQueue;
            ExecEnd := GetNanoseconds;
            Result.ExecuteTimeNanoseconds := ExecEnd - ParseEnd;
            Result.TotalTimeNanoseconds := ExecEnd - StartTime;
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
    SourceMap.Free;
  end;
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
  Result := RunScript(ASource, AFileName, TGocciaEngine.DefaultGlobals);
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
  Result := RunScriptFromFile(AFileName, TGocciaEngine.DefaultGlobals);
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
  Result := RunScriptFromStringList(ASource, AFileName, TGocciaEngine.DefaultGlobals);
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

procedure TGocciaEngine.ThrowError(const AMessage: string; const ALine, AColumn: Integer);
begin
  raise TGocciaRuntimeError.Create(AMessage, ALine, AColumn, FFileName, FSourceLines);
end;

end.
