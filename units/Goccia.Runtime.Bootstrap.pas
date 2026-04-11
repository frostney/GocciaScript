unit Goccia.Runtime.Bootstrap;

{$I Goccia.inc}

interface

uses
  TypInfo,

  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Builtins.Benchmark,
  Goccia.Builtins.Console,
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
  Goccia.Engine,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Interpreter,
  Goccia.ObjectModel.Engine,
  Goccia.Values.ClassValue,
  Goccia.Values.FunctionBase,
  Goccia.Values.Primitives,
  Goccia.Values.TypedArrayValue;

type
  TGocciaRuntimeBootstrap = class
  private
    FInterpreter: TGocciaInterpreter;
    FGlobals: TGocciaGlobalBuiltins;
    FThrowError: TGocciaThrowErrorCallback;

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
    FBuiltinFFI: TGocciaGlobalFFI;
    FBuiltinProxy: TGocciaGlobalProxy;
    FBuiltinReflect: TGocciaGlobalReflect;
    FBuiltinURL: TGocciaGlobalURL;
    FBuiltinURLSearchParams: TGocciaGlobalURLSearchParams;

    procedure PinSingletons;
    procedure RegisterBuiltIns;
    procedure RegisterBuiltinConstructors;
    procedure RegisterTypedArrayConstructor(const AName: string; const AKind: TGocciaTypedArrayKind; const AObjectConstructor: TGocciaClassValue);
    procedure RegisterGlobalThis;
    procedure RegisterGocciaScriptGlobal;
    function SpeciesGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AInterpreter: TGocciaInterpreter; const AGlobals: TGocciaGlobalBuiltins;
      const AThrowError: TGocciaThrowErrorCallback);
    destructor Destroy; override;

    property BuiltinConsole: TGocciaConsole read FBuiltinConsole;
    property BuiltinBenchmark: TGocciaBenchmark read FBuiltinBenchmark;
  end;

implementation

uses
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.Scope,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.ArrayValue,
  Goccia.Values.BooleanObjectValue,
  Goccia.Values.HoleValue,
  Goccia.Values.IteratorValue,
  Goccia.Values.MapValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.NumberObjectValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.SetValue,
  Goccia.Values.SharedArrayBufferValue,
  Goccia.Values.StringObjectValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.Uint8ArrayEncoding,
  Goccia.Values.URLSearchParamsValue,
  Goccia.Values.URLValue,
  Goccia.Version;

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

procedure ExposeURLPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaURLValue.ExposePrototype(AConstructor);
end;

procedure ExposeURLSearchParamsPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaURLSearchParamsValue.ExposePrototype(AConstructor);
end;

constructor TGocciaRuntimeBootstrap.Create(const AInterpreter: TGocciaInterpreter;
  const AGlobals: TGocciaGlobalBuiltins; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create;
  FInterpreter := AInterpreter;
  FGlobals := AGlobals;
  FThrowError := AThrowError;
  PinSingletons;
  RegisterBuiltIns;
end;

destructor TGocciaRuntimeBootstrap.Destroy;
begin
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
  FBuiltinFFI.Free;
  FBuiltinProxy.Free;
  FBuiltinReflect.Free;
  FBuiltinURL.Free;
  FBuiltinURLSearchParams.Free;
  inherited;
end;

procedure TGocciaRuntimeBootstrap.PinSingletons;
begin
  PinPrimitiveSingletons;
  TGarbageCollector.Instance.PinObject(TGocciaHoleValue.HoleValue);
end;

procedure TGocciaRuntimeBootstrap.RegisterBuiltIns;
var
  Scope: TGocciaScope;
begin
  Scope := FInterpreter.GlobalScope;

  if ggConsole in FGlobals then
    FBuiltinConsole := TGocciaConsole.Create('console', Scope, FThrowError);
  if ggMath in FGlobals then
    FBuiltinMath := TGocciaMath.Create('Math', Scope, FThrowError);
  if ggGlobalObject in FGlobals then
    FBuiltinGlobalObject := TGocciaGlobalObject.Create(CONSTRUCTOR_OBJECT, Scope, FThrowError);
  if ggGlobalArray in FGlobals then
    FBuiltinGlobalArray := TGocciaGlobalArray.Create(CONSTRUCTOR_ARRAY, Scope, FThrowError);
  if ggGlobalNumber in FGlobals then
    FBuiltinGlobalNumber := TGocciaGlobalNumber.Create(CONSTRUCTOR_NUMBER, Scope, FThrowError);
  if ggJSON in FGlobals then
    FBuiltinJSON := TGocciaJSONBuiltin.Create('JSON', Scope, FThrowError);
  if ggJSON5 in FGlobals then
    FBuiltinJSON5 := TGocciaJSON5Builtin.Create('JSON5', Scope, FThrowError);
  if ggJSONL in FGlobals then
    FBuiltinJSONL := TGocciaJSONLBuiltin.Create('JSONL', Scope, FThrowError);
  if ggTOML in FGlobals then
    FBuiltinTOML := TGocciaTOMLBuiltin.Create('TOML', Scope, FThrowError);
  if ggYAML in FGlobals then
    FBuiltinYAML := TGocciaYAMLBuiltin.Create('YAML', Scope, FThrowError);
  if ggSymbol in FGlobals then
    FBuiltinSymbol := TGocciaGlobalSymbol.Create(CONSTRUCTOR_SYMBOL, Scope, FThrowError);
  if ggSet in FGlobals then
    FBuiltinSet := TGocciaGlobalSet.Create(CONSTRUCTOR_SET, Scope, FThrowError);
  if ggMap in FGlobals then
    FBuiltinMap := TGocciaGlobalMap.Create(CONSTRUCTOR_MAP, Scope, FThrowError);
  if ggPerformance in FGlobals then
    FBuiltinPerformance := TGocciaPerformance.Create('performance', Scope, FThrowError);
  if ggPromise in FGlobals then
    FBuiltinPromise := TGocciaGlobalPromise.Create(CONSTRUCTOR_PROMISE, Scope, FThrowError);
  if ggTestAssertions in FGlobals then
    FBuiltinTestAssertions := TGocciaTestAssertions.Create('TestAssertions', Scope, FThrowError);
  if ggBenchmark in FGlobals then
    FBuiltinBenchmark := TGocciaBenchmark.Create('Benchmark', Scope, FThrowError);
  if ggTemporal in FGlobals then
    FBuiltinTemporal := TGocciaTemporalBuiltin.Create('Temporal', Scope, FThrowError);
  if ggArrayBuffer in FGlobals then
    FBuiltinArrayBuffer := TGocciaGlobalArrayBuffer.Create(CONSTRUCTOR_ARRAY_BUFFER, Scope, FThrowError);
  if ggFFI in FGlobals then
    FBuiltinFFI := TGocciaGlobalFFI.Create(CONSTRUCTOR_FFI, Scope, FThrowError);
  if ggProxy in FGlobals then
    FBuiltinProxy := TGocciaGlobalProxy.Create(Scope);
  if ggReflect in FGlobals then
    FBuiltinReflect := TGocciaGlobalReflect.Create('Reflect', Scope, FThrowError);
  if ggURL in FGlobals then
  begin
    FBuiltinURL := TGocciaGlobalURL.Create(CONSTRUCTOR_URL, Scope, FThrowError);
    FBuiltinURLSearchParams := TGocciaGlobalURLSearchParams.Create(
      CONSTRUCTOR_URL_SEARCH_PARAMS, Scope, FThrowError);
  end;

  FBuiltinGlobalString := TGocciaGlobalString.Create(CONSTRUCTOR_STRING, Scope, FThrowError);
  FBuiltinGlobals := TGocciaGlobals.Create('Globals', Scope, FThrowError);
  Scope.DefineLexicalBinding(CONSTRUCTOR_ITERATOR, TGocciaIteratorValue.CreateGlobalObject, dtConst);
  RegisterBuiltinConstructors;
end;

procedure TGocciaRuntimeBootstrap.RegisterBuiltinConstructors;
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

  if ggURL in FGlobals then
  begin
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
    RegisterTypedArrayConstructor(CONSTRUCTOR_FLOAT16_ARRAY, takFloat16, ObjectConstructor);
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
    FInterpreter.GlobalScope, FThrowError, ObjectConstructor.Prototype);

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

procedure TGocciaRuntimeBootstrap.RegisterTypedArrayConstructor(const AName: string;
  const AKind: TGocciaTypedArrayKind; const AObjectConstructor: TGocciaClassValue);
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

procedure TGocciaRuntimeBootstrap.RegisterGlobalThis;
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

procedure TGocciaRuntimeBootstrap.RegisterGocciaScriptGlobal;
const
  PREFIX_LENGTH = 2;
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
    BuiltInsArray.Elements.Add(TGocciaStringLiteralValue.Create(
      Copy(Name, PREFIX_LENGTH + 1, Length(Name) - PREFIX_LENGTH)));
  end;

  GocciaObj := TGocciaObjectValue.Create;
  GocciaObj.AssignProperty('version', TGocciaStringLiteralValue.Create(GetVersion));
  GocciaObj.AssignProperty('commit', TGocciaStringLiteralValue.Create(GetCommit));
  GocciaObj.AssignProperty('builtIns', BuiltInsArray);
  GocciaObj.AssignProperty(PROP_STRICT_TYPES, TGocciaBooleanLiteralValue.FalseValue);
  GocciaObj.AssignProperty(SEMVER_NAMESPACE_PROPERTY, CreateSemverNamespace);

  FInterpreter.GlobalScope.DefineLexicalBinding('Goccia', GocciaObj, dtConst);
end;

function TGocciaRuntimeBootstrap.SpeciesGetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := AThisValue;
end;

end.
