unit Goccia.Environment.Bootstrap;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Benchmark,
  Goccia.Builtins.Console,
  Goccia.Builtins.GlobalArray,
  Goccia.Builtins.GlobalArrayBuffer,
  Goccia.Builtins.GlobalMap,
  Goccia.Builtins.GlobalNumber,
  Goccia.Builtins.GlobalObject,
  Goccia.Builtins.GlobalPromise,
  Goccia.Builtins.Globals,
  Goccia.Builtins.GlobalSet,
  Goccia.Builtins.GlobalString,
  Goccia.Builtins.GlobalSymbol,
  Goccia.Builtins.JSON,
  Goccia.Builtins.Math,
  Goccia.Builtins.Performance,
  Goccia.Builtins.Temporal,
  Goccia.Builtins.TestAssertions,
  Goccia.Environment.Types,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Runtime.Operations,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
  Goccia.Values.ClassValue,
  Goccia.Values.Primitives,
  Goccia.Values.TypedArrayValue;

type
  TGocciaEnvironmentBootstrap = class
  private
    FGlobals: TGocciaGlobalBuiltins;
    FScope: TGocciaScope;
    FThrowError: TGocciaThrowErrorCallback;
    FBuiltinConsole: TGocciaConsole;
    FBuiltinMath: TGocciaMath;
    FBuiltinGlobalObject: TGocciaGlobalObject;
    FBuiltinGlobalArray: TGocciaGlobalArray;
    FBuiltinGlobalNumber: TGocciaGlobalNumber;
    FBuiltinGlobalString: TGocciaGlobalString;
    FBuiltinGlobals: TGocciaGlobals;
    FBuiltinJSON: TGocciaJSONBuiltin;
    FBuiltinSymbol: TGocciaGlobalSymbol;
    FBuiltinSet: TGocciaGlobalSet;
    FBuiltinMap: TGocciaGlobalMap;
    FBuiltinPerformance: TGocciaPerformance;
    FBuiltinPromise: TGocciaGlobalPromise;
    FBuiltinTestAssertions: TGocciaTestAssertions;
    FBuiltinBenchmark: TGocciaBenchmark;
    FBuiltinTemporal: TGocciaTemporalBuiltin;
    FBuiltinArrayBuffer: TGocciaGlobalArrayBuffer;

    procedure RegisterBuiltIns;
    procedure RegisterBuiltinConstructors(const AStrictTypes: Boolean);
    procedure RegisterTypedArrayConstructor(const AName: string;
      const AKind: TGocciaTypedArrayKind;
      const AObjectConstructor: TGocciaClassValue);
    procedure RegisterGlobalThis;
    procedure RegisterGocciaScriptGlobal(const AStrictTypes: Boolean);
    function SpeciesGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AGlobals: TGocciaGlobalBuiltins;
      const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback);
    destructor Destroy; override;

    procedure MaterializeInterpreterEnvironment(
      const AStrictTypes: Boolean = False);
    procedure MaterializeSouffleEnvironment(
      const ARuntime: TGocciaRuntimeOperations);

    property BuiltinConsole: TGocciaConsole read FBuiltinConsole;
    property BuiltinBenchmark: TGocciaBenchmark read FBuiltinBenchmark;
  end;

implementation

uses
  SysUtils,
  TypInfo,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.ArrayValue,
  Goccia.Values.BooleanObjectValue,
  Goccia.Values.IteratorValue,
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

constructor TGocciaEnvironmentBootstrap.Create(
  const AGlobals: TGocciaGlobalBuiltins; const AScope: TGocciaScope;
  const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create;
  FGlobals := AGlobals;
  FScope := AScope;
  FThrowError := AThrowError;
end;

destructor TGocciaEnvironmentBootstrap.Destroy;
begin
  FBuiltinConsole.Free;
  FBuiltinMath.Free;
  FBuiltinGlobalObject.Free;
  FBuiltinGlobalArray.Free;
  FBuiltinGlobalNumber.Free;
  FBuiltinGlobalString.Free;
  FBuiltinGlobals.Free;
  FBuiltinJSON.Free;
  FBuiltinSymbol.Free;
  FBuiltinSet.Free;
  FBuiltinMap.Free;
  FBuiltinPerformance.Free;
  FBuiltinPromise.Free;
  FBuiltinTestAssertions.Free;
  FBuiltinBenchmark.Free;
  FBuiltinTemporal.Free;
  FBuiltinArrayBuffer.Free;
  inherited;
end;

procedure TGocciaEnvironmentBootstrap.MaterializeInterpreterEnvironment(
  const AStrictTypes: Boolean);
begin
  RegisterBuiltIns;
  RegisterBuiltinConstructors(AStrictTypes);
end;

procedure TGocciaEnvironmentBootstrap.MaterializeSouffleEnvironment(
  const ARuntime: TGocciaRuntimeOperations);
var
  Names: TGocciaStringArray;
  I: Integer;
  Name: string;
begin
  ARuntime.RegisterDelegates;
  Names := FScope.GetOwnBindingNames;
  for I := 0 to Length(Names) - 1 do
  begin
    Name := Names[I];
    if FScope.GetLexicalBinding(Name).DeclarationType = dtConst then
      ARuntime.RegisterConstGlobal(Name, ARuntime.ToSouffleValue(FScope.GetValue(Name)))
    else
      ARuntime.RegisterGlobal(Name, ARuntime.ToSouffleValue(FScope.GetValue(Name)));
  end;
  ARuntime.RegisterTestNatives;
  ARuntime.RegisterNativeBuiltins;
end;

procedure TGocciaEnvironmentBootstrap.RegisterBuiltIns;
begin
  if ggConsole in FGlobals then
    FBuiltinConsole := TGocciaConsole.Create('console', FScope, FThrowError);
  if ggMath in FGlobals then
    FBuiltinMath := TGocciaMath.Create('Math', FScope, FThrowError);
  if ggGlobalObject in FGlobals then
    FBuiltinGlobalObject := TGocciaGlobalObject.Create(CONSTRUCTOR_OBJECT, FScope, FThrowError);
  if ggGlobalArray in FGlobals then
    FBuiltinGlobalArray := TGocciaGlobalArray.Create(CONSTRUCTOR_ARRAY, FScope, FThrowError);
  if ggGlobalNumber in FGlobals then
    FBuiltinGlobalNumber := TGocciaGlobalNumber.Create(CONSTRUCTOR_NUMBER, FScope, FThrowError);
  if ggJSON in FGlobals then
    FBuiltinJSON := TGocciaJSONBuiltin.Create('JSON', FScope, FThrowError);
  if ggSymbol in FGlobals then
    FBuiltinSymbol := TGocciaGlobalSymbol.Create(CONSTRUCTOR_SYMBOL, FScope, FThrowError);
  if ggSet in FGlobals then
    FBuiltinSet := TGocciaGlobalSet.Create(CONSTRUCTOR_SET, FScope, FThrowError);
  if ggMap in FGlobals then
    FBuiltinMap := TGocciaGlobalMap.Create(CONSTRUCTOR_MAP, FScope, FThrowError);
  if ggPerformance in FGlobals then
    FBuiltinPerformance := TGocciaPerformance.Create('performance', FScope, FThrowError);
  if ggPromise in FGlobals then
    FBuiltinPromise := TGocciaGlobalPromise.Create(CONSTRUCTOR_PROMISE, FScope, FThrowError);
  if ggTestAssertions in FGlobals then
    FBuiltinTestAssertions := TGocciaTestAssertions.Create('TestAssertions', FScope, FThrowError);
  if ggBenchmark in FGlobals then
    FBuiltinBenchmark := TGocciaBenchmark.Create('Benchmark', FScope, FThrowError);
  if ggTemporal in FGlobals then
    FBuiltinTemporal := TGocciaTemporalBuiltin.Create('Temporal', FScope, FThrowError);
  if ggArrayBuffer in FGlobals then
    FBuiltinArrayBuffer := TGocciaGlobalArrayBuffer.Create(CONSTRUCTOR_ARRAY_BUFFER, FScope, FThrowError);

  FBuiltinGlobalString := TGocciaGlobalString.Create(CONSTRUCTOR_STRING, FScope, FThrowError);
  FBuiltinGlobals := TGocciaGlobals.Create('Globals', FScope, FThrowError);
  FScope.DefineLexicalBinding(CONSTRUCTOR_ITERATOR, TGocciaIteratorValue.CreateGlobalObject, dtConst);
end;

function TGocciaEnvironmentBootstrap.SpeciesGetter(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := AThisValue;
end;

procedure TGocciaEnvironmentBootstrap.RegisterBuiltinConstructors(
  const AStrictTypes: Boolean);
var
  ObjectConstructor, FunctionConstructor: TGocciaClassValue;
  ArrayConstructor: TGocciaArrayClassValue;
  MapConstructor: TGocciaMapClassValue;
  SetConstructor: TGocciaSetClassValue;
  ArrayBufferConstructor: TGocciaArrayBufferClassValue;
  SharedArrayBufferConstructor: TGocciaSharedArrayBufferClassValue;
  StringConstructor: TGocciaStringClassValue;
  NumberConstructor: TGocciaNumberClassValue;
  BooleanConstructor: TGocciaBooleanClassValue;
  Key: string;
begin
  TGocciaObjectValue.InitializeSharedPrototype;
  ObjectConstructor := TGocciaClassValue.Create(CONSTRUCTOR_OBJECT, nil);
  ObjectConstructor.ReplacePrototype(TGocciaObjectValue.SharedObjectPrototype);
  TGocciaObjectValue.SharedObjectPrototype.AssignProperty(PROP_CONSTRUCTOR, ObjectConstructor);
  if Assigned(FBuiltinGlobalObject) then
    for Key in FBuiltinGlobalObject.BuiltinObject.GetAllPropertyNames do
      ObjectConstructor.SetProperty(Key, FBuiltinGlobalObject.BuiltinObject.GetProperty(Key));
  FScope.DefineLexicalBinding(CONSTRUCTOR_OBJECT, ObjectConstructor, dtConst);

  ArrayConstructor := TGocciaArrayClassValue.Create(CONSTRUCTOR_ARRAY, nil);
  TGocciaArrayValue.ExposePrototype(ArrayConstructor);
  ArrayConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
  if Assigned(FBuiltinGlobalArray) then
    for Key in FBuiltinGlobalArray.BuiltinObject.GetAllPropertyNames do
      ArrayConstructor.SetProperty(Key, FBuiltinGlobalArray.BuiltinObject.GetProperty(Key));
  ArrayConstructor.DefineSymbolProperty(
    TGocciaSymbolValue.WellKnownSpecies,
    TGocciaPropertyDescriptorAccessor.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(SpeciesGetter, 'get [Symbol.species]', 0),
      nil, [pfConfigurable]));
  FScope.DefineLexicalBinding(CONSTRUCTOR_ARRAY, ArrayConstructor, dtConst);

  if ggMap in FGlobals then
  begin
    MapConstructor := TGocciaMapClassValue.Create(CONSTRUCTOR_MAP, nil);
    TGocciaMapValue.ExposePrototype(MapConstructor);
    MapConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
    if Assigned(FBuiltinMap) then
      for Key in FBuiltinMap.BuiltinObject.GetAllPropertyNames do
        MapConstructor.SetProperty(Key, FBuiltinMap.BuiltinObject.GetProperty(Key));
    MapConstructor.DefineSymbolProperty(
      TGocciaSymbolValue.WellKnownSpecies,
      TGocciaPropertyDescriptorAccessor.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(SpeciesGetter, 'get [Symbol.species]', 0),
        nil, [pfConfigurable]));
    FScope.DefineLexicalBinding(CONSTRUCTOR_MAP, MapConstructor, dtConst);
  end;

  if ggSet in FGlobals then
  begin
    SetConstructor := TGocciaSetClassValue.Create(CONSTRUCTOR_SET, nil);
    TGocciaSetValue.ExposePrototype(SetConstructor);
    SetConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
    SetConstructor.DefineSymbolProperty(
      TGocciaSymbolValue.WellKnownSpecies,
      TGocciaPropertyDescriptorAccessor.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(SpeciesGetter, 'get [Symbol.species]', 0),
        nil, [pfConfigurable]));
    FScope.DefineLexicalBinding(CONSTRUCTOR_SET, SetConstructor, dtConst);
  end;

  if ggArrayBuffer in FGlobals then
  begin
    ArrayBufferConstructor := TGocciaArrayBufferClassValue.Create(CONSTRUCTOR_ARRAY_BUFFER, nil);
    TGocciaArrayBufferValue.ExposePrototype(ArrayBufferConstructor);
    ArrayBufferConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
    if Assigned(FBuiltinArrayBuffer) then
      for Key in FBuiltinArrayBuffer.BuiltinObject.GetAllPropertyNames do
        ArrayBufferConstructor.SetProperty(Key, FBuiltinArrayBuffer.BuiltinObject.GetProperty(Key));
    FScope.DefineLexicalBinding(CONSTRUCTOR_ARRAY_BUFFER, ArrayBufferConstructor, dtConst);

    SharedArrayBufferConstructor := TGocciaSharedArrayBufferClassValue.Create(CONSTRUCTOR_SHARED_ARRAY_BUFFER, nil);
    TGocciaSharedArrayBufferValue.ExposePrototype(SharedArrayBufferConstructor);
    SharedArrayBufferConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
    FScope.DefineLexicalBinding(CONSTRUCTOR_SHARED_ARRAY_BUFFER, SharedArrayBufferConstructor, dtConst);

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

  StringConstructor := TGocciaStringClassValue.Create(CONSTRUCTOR_STRING, nil);
  StringConstructor.ReplacePrototype(TGocciaStringObjectValue.GetSharedPrototype);
  StringConstructor.Prototype.AssignProperty(PROP_CONSTRUCTOR, StringConstructor);
  StringConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
  if Assigned(FBuiltinGlobalString) then
    for Key in FBuiltinGlobalString.BuiltinObject.GetAllPropertyNames do
      StringConstructor.SetProperty(Key, FBuiltinGlobalString.BuiltinObject.GetProperty(Key));
  FScope.DefineLexicalBinding(CONSTRUCTOR_STRING, StringConstructor, dtConst);

  NumberConstructor := TGocciaNumberClassValue.Create(CONSTRUCTOR_NUMBER, nil);
  NumberConstructor.ReplacePrototype(TGocciaNumberObjectValue.GetSharedPrototype);
  NumberConstructor.Prototype.AssignProperty(PROP_CONSTRUCTOR, NumberConstructor);
  NumberConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
  if Assigned(FBuiltinGlobalNumber) then
    for Key in FBuiltinGlobalNumber.BuiltinObject.GetAllPropertyNames do
      NumberConstructor.SetProperty(Key, FBuiltinGlobalNumber.BuiltinObject.GetProperty(Key));
  FScope.DefineLexicalBinding(CONSTRUCTOR_NUMBER, NumberConstructor, dtConst);

  BooleanConstructor := TGocciaBooleanClassValue.Create(CONSTRUCTOR_BOOLEAN, nil);
  BooleanConstructor.ReplacePrototype(TGocciaBooleanObjectValue.GetSharedPrototype);
  BooleanConstructor.Prototype.AssignProperty(PROP_CONSTRUCTOR, BooleanConstructor);
  BooleanConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
  FScope.DefineLexicalBinding(CONSTRUCTOR_BOOLEAN, BooleanConstructor, dtConst);

  FunctionConstructor := TGocciaClassValue.Create('Function', nil);
  FScope.DefineLexicalBinding('Function', FunctionConstructor, dtConst);

  RegisterGocciaScriptGlobal(AStrictTypes);
  RegisterGlobalThis;
end;

procedure TGocciaEnvironmentBootstrap.RegisterTypedArrayConstructor(
  const AName: string; const AKind: TGocciaTypedArrayKind;
  const AObjectConstructor: TGocciaClassValue);
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

  FScope.DefineLexicalBinding(AName, TAConstructor, dtConst);
end;

procedure TGocciaEnvironmentBootstrap.RegisterGlobalThis;
var
  GlobalThisObj: TGocciaObjectValue;
  Name: string;
begin
  GlobalThisObj := TGocciaObjectValue.Create;

  for Name in FScope.GetOwnBindingNames do
    GlobalThisObj.AssignProperty(Name, FScope.GetValue(Name));

  GlobalThisObj.AssignProperty('globalThis', GlobalThisObj);
  FScope.DefineLexicalBinding('globalThis', GlobalThisObj, dtConst);
end;

procedure TGocciaEnvironmentBootstrap.RegisterGocciaScriptGlobal(
  const AStrictTypes: Boolean);
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
    BuiltInsArray.Elements.Add(
      TGocciaStringLiteralValue.Create(
        Copy(Name, PREFIX_LENGTH + 1, Length(Name) - PREFIX_LENGTH)));
  end;

  GocciaObj := TGocciaObjectValue.Create;
  GocciaObj.AssignProperty('version', TGocciaStringLiteralValue.Create(GetVersion));
  GocciaObj.AssignProperty('commit', TGocciaStringLiteralValue.Create(GetCommit));
  GocciaObj.AssignProperty('builtIns', BuiltInsArray);
  if AStrictTypes then
    GocciaObj.AssignProperty(PROP_STRICT_TYPES, TGocciaBooleanLiteralValue.TrueValue)
  else
    GocciaObj.AssignProperty(PROP_STRICT_TYPES, TGocciaBooleanLiteralValue.FalseValue);

  FScope.DefineLexicalBinding('GocciaScript', GocciaObj, dtConst);
end;

end.
