unit Goccia.Values.ClassValue;

{$I Goccia.inc}

interface

uses
  Classes,

  HashMap,
  OrderedStringMap,

  Goccia.Arguments.Collection,
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.Values.FunctionBase,
  Goccia.Values.FunctionValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue;

type
  TStaticSymbolDescriptorMap = THashMap<TGocciaSymbolValue, TGocciaPropertyDescriptor>;
  // Forward declaration
  TGocciaInstanceValue = class;

  TGocciaClassFieldOrderEntry = record
    Name: string;
    IsPrivate: Boolean;
  end;

  TGocciaClassValue = class(TGocciaObjectValue)
  private
    FName: string;
    FSuperClass: TGocciaClassValue;
    FMethods: TOrderedStringMap<TGocciaMethodValue>;
    FGetters: TOrderedStringMap<TGocciaFunctionBase>;
    FSetters: TOrderedStringMap<TGocciaFunctionBase>;
    FClassPrototype: TGocciaObjectValue;
    FConstructorMethod: TGocciaMethodValue;
    FStaticMethods: TGocciaValueMap;
    FInstancePropertyDefs: TGocciaExpressionMap;
    FPrivateInstancePropertyDefs: TGocciaExpressionMap;
    FFieldOrder: array of TGocciaClassFieldOrderEntry;
    FPrivateStaticProperties: TGocciaValueMap;
    FPrivateMethods: TOrderedStringMap<TGocciaMethodValue>;
    FPrivateGetters: TOrderedStringMap<TGocciaFunctionBase>;
    FPrivateSetters: TOrderedStringMap<TGocciaFunctionBase>;
    FStaticGetters: TOrderedStringMap<TGocciaFunctionBase>;
    FStaticSetters: TOrderedStringMap<TGocciaFunctionBase>;
    FStaticSymbolDescriptors: TStaticSymbolDescriptorMap;
    FMethodInitializers: array of TGocciaValue;
    FFieldInitializers: array of TGocciaValue;
    FDecoratorFieldInitializers: array of record
      Name: string;
      Initializer: TGocciaValue;
      IsPrivate: Boolean;
      IsStatic: Boolean;
    end;
    function GetPropertyGetter(const AName: string): TGocciaFunctionBase; inline;
    function GetPropertySetter(const AName: string): TGocciaFunctionBase; inline;
    function GetStaticPropertyGetter(const AName: string): TGocciaFunctionBase; inline;
    function GetStaticPropertySetter(const AName: string): TGocciaFunctionBase; inline;
    function GetPrivatePropertyGetter(const AName: string): TGocciaFunctionBase;
    function GetPrivatePropertySetter(const AName: string): TGocciaFunctionBase;
  public
    class procedure SetDefaultPrototype(const AProto: TGocciaObjectValue); static;
    class procedure PatchDefaultPrototype(const AClassValue: TGocciaClassValue); static;
    constructor Create(const AName: string; const ASuperClass: TGocciaClassValue);
    destructor Destroy; override;
    function IsCallable: Boolean; override;
    function TypeName: string; override;
    function TypeOf: string; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;
    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    procedure AddMethod(const AName: string; const AMethod: TGocciaMethodValue);
    function GetMethod(const AName: string): TGocciaMethodValue;
    procedure AddGetter(const AName: string; const AGetter: TGocciaFunctionBase);
    procedure AddSetter(const AName: string; const ASetter: TGocciaFunctionBase);
    procedure AddStaticGetter(const AName: string; const AGetter: TGocciaFunctionBase);
    procedure AddStaticSetter(const AName: string; const ASetter: TGocciaFunctionBase);
    procedure AddPrivateGetter(const AName: string; const AGetter: TGocciaFunctionBase);
    procedure AddPrivateSetter(const AName: string; const ASetter: TGocciaFunctionBase);
    function HasPrivateGetter(const AName: string): Boolean;
    function HasPrivateSetter(const AName: string): Boolean;
    function HasOwnPrivateGetter(const AName: string): Boolean;
    function HasOwnPrivateSetter(const AName: string): Boolean;
    function HasOwnPrivateStaticProperty(const AName: string): Boolean;
    function GetOwnPrivatePropertyGetter(
      const AName: string): TGocciaFunctionBase;
    function GetOwnPrivatePropertySetter(
      const AName: string): TGocciaFunctionBase;
    procedure AddInstanceProperty(const AName: string; const AExpression: TGocciaExpression);
    procedure AddPrivateInstanceProperty(const AName: string; const AExpression: TGocciaExpression);
    procedure AddPrivateStaticProperty(const AName: string; const AValue: TGocciaValue);
    procedure AddPrivateMethod(const AName: string; const AMethod: TGocciaMethodValue);
    function GetPrivateMethod(const AName: string): TGocciaMethodValue;
    procedure AppendOwnPrivateNames(const ANames: TStrings);
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; virtual;
    function Instantiate(const AArguments: TGocciaArgumentsCollection;
      const ANewTarget: TGocciaClassValue = nil): TGocciaValue; virtual;
    function EstimatedInstancePropertyCapacity: Integer;
    // ECMAScript: number of expected constructor parameters before the first
    // default/rest. Built-in classes default to 0; user classes derive from
    // their constructor method's formal parameters.
    function GetClassLength: Integer; virtual;
    function GetProperty(const AName: string): TGocciaValue; override;
    procedure SetProperty(const AName: string; const AValue: TGocciaValue); override;
    function GetOwnPropertyDescriptor(const AName: string): TGocciaPropertyDescriptor; override;
    function HasOwnProperty(const AName: string): Boolean; override;
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; virtual;

    procedure ReplacePrototype(const APrototype: TGocciaObjectValue);
    procedure SetInferredName(const AName: string);
    procedure DefineSymbolProperty(const ASymbol: TGocciaSymbolValue; const ADescriptor: TGocciaPropertyDescriptor);
    procedure AssignSymbolProperty(const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue);
    function GetOwnStaticSymbolDescriptor(const ASymbol: TGocciaSymbolValue): TGocciaPropertyDescriptor;
    function GetOwnSymbolPropertyDescriptor(const ASymbol: TGocciaSymbolValue): TGocciaPropertyDescriptor; override;
    function GetOwnSymbols: TArray<TGocciaSymbolValue>; override;
    function GetSymbolProperty(const ASymbol: TGocciaSymbolValue): TGocciaValue;
    function GetSymbolPropertyWithReceiver(const ASymbol: TGocciaSymbolValue; const AReceiver: TGocciaValue): TGocciaValue;

    property Name: string read FName;
    property SuperClass: TGocciaClassValue read FSuperClass write FSuperClass;
    property Prototype: TGocciaObjectValue read FClassPrototype;
    property ConstructorMethod: TGocciaMethodValue read FConstructorMethod;
    property InstancePropertyDefs: TGocciaExpressionMap read FInstancePropertyDefs;
    property PrivateInstancePropertyDefs: TGocciaExpressionMap read FPrivateInstancePropertyDefs;
    procedure SetFieldOrder(const AOrder: array of TGocciaClassFieldOrderEntry);
    function FieldOrderCount: Integer;
    function FieldOrderEntry(const AIndex: Integer): TGocciaClassFieldOrderEntry;
    property PrivateStaticProperties: TGocciaValueMap read FPrivateStaticProperties;
    property PrivateMethods: TOrderedStringMap<TGocciaMethodValue> read FPrivateMethods;
    property PropertyGetter[const AName: string]: TGocciaFunctionBase read GetPropertyGetter;
    property PropertySetter[const AName: string]: TGocciaFunctionBase read GetPropertySetter;
    property StaticPropertyGetter[const AName: string]: TGocciaFunctionBase read GetStaticPropertyGetter;
    property StaticPropertySetter[const AName: string]: TGocciaFunctionBase read GetStaticPropertySetter;
    property PrivatePropertyGetter[const AName: string]: TGocciaFunctionBase read GetPrivatePropertyGetter;
    property PrivatePropertySetter[const AName: string]: TGocciaFunctionBase read GetPrivatePropertySetter;
    procedure MarkReferences; override;

    procedure AddFieldInitializer(const AName: string; const AInitializer: TGocciaValue; const AIsPrivate, AIsStatic: Boolean);
    procedure SetMethodInitializers(const AInitializers: array of TGocciaValue);
    procedure SetFieldInitializers(const AInitializers: array of TGocciaValue);
    procedure AppendMethodInitializers(const AInitializers: array of TGocciaValue);
    procedure AppendFieldInitializers(const AInitializers: array of TGocciaValue);
    procedure AddAutoAccessor(const AName, ABackingName: string; const AIsStatic: Boolean);
    procedure RunMethodInitializers(const AInstance: TGocciaValue);
    procedure RunFieldInitializers(const AInstance: TGocciaValue);
    procedure RunDecoratorFieldInitializers(const AInstance: TGocciaValue);
  end;

  TGocciaArrayClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    // ECMAScript 23.1.1.1: Array constructor length is 1.
    function GetClassLength: Integer; override;
  end;

  TGocciaMapClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
  end;

  TGocciaSetClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
  end;

  TGocciaWeakMapClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
  end;

  TGocciaWeakSetClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
  end;

  TGocciaStringClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    // ECMAScript 22.1.1.1: String constructor length is 1.
    function GetClassLength: Integer; override;
  end;

  TGocciaNumberClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    // ECMAScript 21.1.1.1: Number constructor length is 1.
    function GetClassLength: Integer; override;
  end;

  TGocciaBooleanClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    // ECMAScript 20.3.1.1: Boolean constructor length is 1.
    function GetClassLength: Integer; override;
  end;

  TGocciaArrayBufferClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    // ECMAScript 25.1.1.1: ArrayBuffer constructor length is 1.
    function GetClassLength: Integer; override;
  end;

  TGocciaSharedArrayBufferClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    // ECMAScript 25.2.1.1: SharedArrayBuffer constructor length is 1.
    function GetClassLength: Integer; override;
  end;

  TGocciaTextEncoderClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
  end;

  TGocciaTextDecoderClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
  end;

  TGocciaURLClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    // WHATWG URL: URL(url, base?) — required url means length 1.
    function GetClassLength: Integer; override;
  end;

  TGocciaURLSearchParamsClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
  end;

  TGocciaHeadersClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
  end;

  TGocciaResponseClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    // Fetch spec: Response constructor reports length 1 in WPT/V8.
    function GetClassLength: Integer; override;
  end;

  TGocciaCompileDynamicFunction = function(const AParamsSources: array of string;
    const ABodySource: string): TGocciaFunctionBase of object;

  TGocciaFunctionConstructorClassValue = class(TGocciaClassValue)
  private
    FEnabled: Boolean;
    FCompileDynamicFunction: TGocciaCompileDynamicFunction;
    function BuildFunction(const AArguments: TGocciaArgumentsCollection): TGocciaFunctionBase;
  public
    constructor Create(const AName: string; const ASuperClass: TGocciaClassValue);
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    // ECMAScript 20.2.2: Function constructor length is 1.
    function GetClassLength: Integer; override;

    property Enabled: Boolean read FEnabled write FEnabled;
    property CompileDynamicFunction: TGocciaCompileDynamicFunction
      read FCompileDynamicFunction write FCompileDynamicFunction;
  end;

  TGocciaInstanceValue = class(TGocciaObjectValue)
  private
    FClass: TGocciaClassValue;
    FPrivateProperties: TGocciaValueMap;
  public
    constructor Create(const AClass: TGocciaClassValue = nil;
      const APropertyCapacity: Integer = 0);
    destructor Destroy; override;
    function TypeName: string; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;
    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;
    procedure AssignProperty(const AName: string; const AValue: TGocciaValue; const ACanCreate: Boolean = True); override;
    procedure SetProperty(const AName: string; const AValue: TGocciaValue); override;
    function GetPrivateProperty(const AName: string; const AAccessClass: TGocciaClassValue): TGocciaValue;
    procedure SetPrivateProperty(const AName: string; const AValue: TGocciaValue; const AAccessClass: TGocciaClassValue);
    function TryGetRawPrivateProperty(const AKey: string; out AValue: TGocciaValue): Boolean;
    procedure SetRawPrivateProperty(const AKey: string; const AValue: TGocciaValue);
    function IsInstanceOf(const AClass: TGocciaClassValue): Boolean; inline;
    procedure InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); virtual;
    procedure MarkReferences; override;

    property ClassValue: TGocciaClassValue read FClass write FClass;
  end;

implementation

uses
  Generics.Collections,
  SysUtils,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Constants.TypeNames,
  Goccia.Error,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.ArrayValue,
  Goccia.Values.AutoAccessor,
  Goccia.Values.ClassHelper,
  Goccia.Values.ErrorHelper,
  Goccia.Values.HeadersValue,
  Goccia.Values.MapValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ResponseValue,
  Goccia.Values.SetValue,
  Goccia.Values.SharedArrayBufferValue,
  Goccia.Values.TextDecoderValue,
  Goccia.Values.TextEncoderValue,
  Goccia.Values.URLSearchParamsValue,
  Goccia.Values.URLValue,
  Goccia.Values.WeakMapValue,
  Goccia.Values.WeakSetValue;

// SetDefaultPrototype / PatchDefaultPrototype previously cached the
// "default constructor [[Prototype]]" (Function.prototype) in a threadvar.
// That cache held a stale reference across realm teardown: when the previous
// engine was freed its realm unpinned Function.prototype, but the threadvar
// still pointed at the dead object - so the next engine's PatchDefaultPrototype
// calls would patch new constructors with the prior realm's prototype.
// Now we read TGocciaFunctionBase.GetSharedPrototype directly each call,
// which always returns the current realm's Function.prototype.
class procedure TGocciaClassValue.SetDefaultPrototype(const AProto: TGocciaObjectValue);
begin
  // Intentionally a no-op.  The current-realm lookup in PatchDefaultPrototype
  // is the source of truth.  Kept for backwards compatibility with existing
  // engine bootstrap call sites.
end;

class procedure TGocciaClassValue.PatchDefaultPrototype(const AClassValue: TGocciaClassValue);
var
  FunctionPrototype: TGocciaObjectValue;
begin
  if not Assigned(AClassValue) then Exit;
  if Assigned(AClassValue.FPrototype) then Exit;
  FunctionPrototype := TGocciaFunctionBase.GetSharedPrototype;
  if Assigned(FunctionPrototype) then
    AClassValue.FPrototype := FunctionPrototype;
end;

constructor TGocciaClassValue.Create(const AName: string; const ASuperClass: TGocciaClassValue);
begin
  inherited Create;
  FName := AName;
  FSuperClass := ASuperClass;
  // Set [[Prototype]]: superclass for derived, Function.prototype for base classes
  if Assigned(FSuperClass) then
    FPrototype := FSuperClass
  else if Assigned(TGocciaFunctionBase.GetSharedPrototype) then
    FPrototype := TGocciaFunctionBase.GetSharedPrototype;
  FMethods := TOrderedStringMap<TGocciaMethodValue>.Create;
  FGetters := TOrderedStringMap<TGocciaFunctionBase>.Create;
  FSetters := TOrderedStringMap<TGocciaFunctionBase>.Create;
  FStaticMethods := TGocciaValueMap.Create;
  FInstancePropertyDefs := TGocciaExpressionMap.Create;
  FPrivateInstancePropertyDefs := TGocciaExpressionMap.Create;
  FPrivateStaticProperties := TGocciaValueMap.Create;
  FPrivateMethods := TOrderedStringMap<TGocciaMethodValue>.Create;
  FPrivateGetters := TOrderedStringMap<TGocciaFunctionBase>.Create;
  FPrivateSetters := TOrderedStringMap<TGocciaFunctionBase>.Create;
  FStaticGetters := TOrderedStringMap<TGocciaFunctionBase>.Create;
  FStaticSetters := TOrderedStringMap<TGocciaFunctionBase>.Create;
  FStaticSymbolDescriptors := TStaticSymbolDescriptorMap.Create;
  FClassPrototype := TGocciaObjectValue.Create;
  FConstructorMethod := nil;
  if Assigned(FSuperClass) then
    FClassPrototype.Prototype := FSuperClass.Prototype
  else if Assigned(TGocciaObjectValue.SharedObjectPrototype) then
    FClassPrototype.Prototype := TGocciaObjectValue.SharedObjectPrototype;

  // .name is synthesized in GetOwnPropertyDescriptor (like .prototype)
  // to allow static field overrides via SetProperty.
end;

destructor TGocciaClassValue.Destroy;
begin
  FMethods.Free;
  FGetters.Free;
  FSetters.Free;
  FStaticMethods.Free;
  FInstancePropertyDefs.Free;
  FPrivateInstancePropertyDefs.Free;
  FPrivateStaticProperties.Free;
  FPrivateMethods.Free;
  FPrivateGetters.Free;
  FPrivateSetters.Free;
  FStaticGetters.Free;
  FStaticSetters.Free;
  FStaticSymbolDescriptors.Free;
  // Don't free FClassPrototype - it's GC-managed
  inherited;
end;

procedure TGocciaClassValue.MarkReferences;
var
  MethodPair: TOrderedStringMap<TGocciaMethodValue>.TKeyValuePair;
  FuncPair: TOrderedStringMap<TGocciaFunctionBase>.TKeyValuePair;
  ValPair: TGocciaValueMap.TKeyValuePair;
  SymPair: TStaticSymbolDescriptorMap.TKeyValuePair;
  Accessor: TGocciaPropertyDescriptorAccessor;
  I: Integer;
begin
  if GCMarked then Exit;
  inherited;

  if Assigned(FSuperClass) then
    FSuperClass.MarkReferences;

  if Assigned(FClassPrototype) then
    FClassPrototype.MarkReferences;

  if Assigned(FConstructorMethod) then
    FConstructorMethod.MarkReferences;

  for MethodPair in FMethods do
    MethodPair.Value.MarkReferences;

  for FuncPair in FGetters do
    FuncPair.Value.MarkReferences;
  for FuncPair in FSetters do
    FuncPair.Value.MarkReferences;

  for ValPair in FStaticMethods do
    if Assigned(ValPair.Value) then
      ValPair.Value.MarkReferences;

  for ValPair in FPrivateStaticProperties do
    if Assigned(ValPair.Value) then
      ValPair.Value.MarkReferences;

  for MethodPair in FPrivateMethods do
    MethodPair.Value.MarkReferences;

  for FuncPair in FPrivateGetters do
    FuncPair.Value.MarkReferences;
  for FuncPair in FPrivateSetters do
    FuncPair.Value.MarkReferences;

  for FuncPair in FStaticGetters do
    FuncPair.Value.MarkReferences;
  for FuncPair in FStaticSetters do
    FuncPair.Value.MarkReferences;

  for SymPair in FStaticSymbolDescriptors do
  begin
    SymPair.Key.MarkReferences;
    if SymPair.Value is TGocciaPropertyDescriptorData then
    begin
      if Assigned(TGocciaPropertyDescriptorData(SymPair.Value).Value) then
        TGocciaPropertyDescriptorData(SymPair.Value).Value.MarkReferences;
    end
    else if SymPair.Value is TGocciaPropertyDescriptorAccessor then
    begin
      Accessor := TGocciaPropertyDescriptorAccessor(SymPair.Value);
      if Assigned(Accessor.Getter) then
        Accessor.Getter.MarkReferences;
      if Assigned(Accessor.Setter) then
        Accessor.Setter.MarkReferences;
    end;
  end;

  // Mark decorator initializer arrays
  for I := 0 to High(FMethodInitializers) do
    if Assigned(FMethodInitializers[I]) then
      FMethodInitializers[I].MarkReferences;
  for I := 0 to High(FFieldInitializers) do
    if Assigned(FFieldInitializers[I]) then
      FFieldInitializers[I].MarkReferences;
  for I := 0 to High(FDecoratorFieldInitializers) do
    if Assigned(FDecoratorFieldInitializers[I].Initializer) then
      FDecoratorFieldInitializers[I].Initializer.MarkReferences;
end;

function TGocciaClassValue.IsCallable: Boolean;
begin
  Result := True;
end;

function TGocciaClassValue.TypeName: string;
begin
  Result := FUNCTION_TYPE_NAME;
end;

function TGocciaClassValue.TypeOf: string;
begin
  Result := FUNCTION_TYPE_NAME;
end;

function TGocciaClassValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  // For error classes, just return the name to make toThrow work properly
  if (FName = RANGE_ERROR_NAME) or (FName = ERROR_NAME) or (FName = TYPE_ERROR_NAME) or (FName = REFERENCE_ERROR_NAME) then
    Result := TGocciaStringLiteralValue.Create(FName)
  else
    Result := TGocciaStringLiteralValue.Create(Format('[Class: %s]', [FName]));
end;

function TGocciaClassValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

function TGocciaClassValue.ToNumberLiteral: TGocciaNumberLiteralValue;
begin
  Result := TGocciaNumberLiteralValue.NaNValue;
end;

procedure TGocciaClassValue.AddMethod(const AName: string; const AMethod: TGocciaMethodValue);
begin
  // If this is the constructor, store it separately
  if AName = PROP_CONSTRUCTOR then
  begin
    FConstructorMethod := AMethod;
    Exit;
  end;

  FMethods.AddOrSetValue(AName, AMethod);
  // ES §14.3.7: class method definitions have enumerable: false
  FClassPrototype.DefineProperty(AName,
    TGocciaPropertyDescriptorData.Create(AMethod, [pfConfigurable, pfWritable]));
end;

function TGocciaClassValue.GetMethod(const AName: string): TGocciaMethodValue;
begin
  if not FMethods.TryGetValue(AName, Result) then
  begin
    if Assigned(FSuperClass) then
      Result := FSuperClass.GetMethod(AName)
    else
      Result := nil;
  end;
end;

procedure TGocciaClassValue.AddGetter(const AName: string; const AGetter: TGocciaFunctionBase);
var
  ExistingDescriptor: TGocciaPropertyDescriptor;
  ExistingSetter: TGocciaValue;
begin
  FGetters.AddOrSetValue(AName, AGetter);

  // Check if there's already a setter for this property
  ExistingDescriptor := FClassPrototype.GetOwnPropertyDescriptor(AName);
  if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
     Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter) then
  begin
    // Merge with existing setter
    ExistingSetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter;
    FClassPrototype.DefineProperty(AName, TGocciaPropertyDescriptorAccessor.Create(AGetter, ExistingSetter, [pfConfigurable, pfWritable]));
  end
  else
  begin
    // No existing setter, create getter-only descriptor
    FClassPrototype.DefineProperty(AName, TGocciaPropertyDescriptorAccessor.Create(AGetter, nil, [pfConfigurable, pfWritable]));
  end;
end;

procedure TGocciaClassValue.AddSetter(const AName: string; const ASetter: TGocciaFunctionBase);
var
  ExistingDescriptor: TGocciaPropertyDescriptor;
  ExistingGetter: TGocciaValue;
begin
  FSetters.AddOrSetValue(AName, ASetter);

  // Check if there's already a getter for this property
  ExistingDescriptor := FClassPrototype.GetOwnPropertyDescriptor(AName);
  if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
     Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter) then
  begin
    // Merge with existing getter
    ExistingGetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter;
    FClassPrototype.DefineProperty(AName, TGocciaPropertyDescriptorAccessor.Create(ExistingGetter, ASetter, [pfConfigurable, pfWritable]));
  end
  else
  begin
    // No existing getter, create setter-only descriptor
    FClassPrototype.DefineProperty(AName, TGocciaPropertyDescriptorAccessor.Create(nil, ASetter, [pfConfigurable, pfWritable]));
  end;
end;

procedure TGocciaClassValue.AddStaticGetter(const AName: string; const AGetter: TGocciaFunctionBase);
begin
  FStaticGetters.AddOrSetValue(AName, AGetter);
end;

procedure TGocciaClassValue.AddStaticSetter(const AName: string; const ASetter: TGocciaFunctionBase);
begin
  FStaticSetters.AddOrSetValue(AName, ASetter);
end;

procedure TGocciaClassValue.AddPrivateGetter(const AName: string; const AGetter: TGocciaFunctionBase);
begin
  FPrivateGetters.AddOrSetValue(AName, AGetter);
end;

procedure TGocciaClassValue.AddPrivateSetter(const AName: string; const ASetter: TGocciaFunctionBase);
begin
  FPrivateSetters.AddOrSetValue(AName, ASetter);
end;

function TGocciaClassValue.HasPrivateGetter(const AName: string): Boolean;
begin
  Result := FPrivateGetters.ContainsKey(AName);
  if not Result and Assigned(FSuperClass) then
    Result := FSuperClass.HasPrivateGetter(AName);
end;

function TGocciaClassValue.HasPrivateSetter(const AName: string): Boolean;
begin
  Result := FPrivateSetters.ContainsKey(AName);
  if not Result and Assigned(FSuperClass) then
    Result := FSuperClass.HasPrivateSetter(AName);
end;

function TGocciaClassValue.HasOwnPrivateGetter(const AName: string): Boolean;
begin
  Result := FPrivateGetters.ContainsKey(AName);
end;

function TGocciaClassValue.HasOwnPrivateSetter(const AName: string): Boolean;
begin
  Result := FPrivateSetters.ContainsKey(AName);
end;

function TGocciaClassValue.HasOwnPrivateStaticProperty(
  const AName: string): Boolean;
begin
  Result := FPrivateStaticProperties.ContainsKey(AName);
end;

function TGocciaClassValue.GetOwnPrivatePropertyGetter(
  const AName: string): TGocciaFunctionBase;
begin
  if not FPrivateGetters.TryGetValue(AName, Result) then
    Result := nil;
end;

function TGocciaClassValue.GetOwnPrivatePropertySetter(
  const AName: string): TGocciaFunctionBase;
begin
  if not FPrivateSetters.TryGetValue(AName, Result) then
    Result := nil;
end;

function TGocciaClassValue.GetPrivatePropertyGetter(const AName: string): TGocciaFunctionBase;
begin
  if FPrivateGetters.TryGetValue(AName, Result) then
    Exit;
  if Assigned(FSuperClass) then
    Result := FSuperClass.GetPrivatePropertyGetter(AName)
  else
    Result := nil;
end;

function TGocciaClassValue.GetPrivatePropertySetter(const AName: string): TGocciaFunctionBase;
begin
  if FPrivateSetters.TryGetValue(AName, Result) then
    Exit;
  if Assigned(FSuperClass) then
    Result := FSuperClass.GetPrivatePropertySetter(AName)
  else
    Result := nil;
end;

function TGocciaClassValue.GetPropertyGetter(const AName: string): TGocciaFunctionBase;
begin
  if not FGetters.TryGetValue(AName, Result) then
    Result := nil;
end;

function TGocciaClassValue.GetPropertySetter(const AName: string): TGocciaFunctionBase;
begin
  if not FSetters.TryGetValue(AName, Result) then
    Result := nil;
end;

function TGocciaClassValue.GetStaticPropertyGetter(const AName: string): TGocciaFunctionBase;
begin
  if not FStaticGetters.TryGetValue(AName, Result) then
    Result := nil;
end;

function TGocciaClassValue.GetStaticPropertySetter(const AName: string): TGocciaFunctionBase;
begin
  if not FStaticSetters.TryGetValue(AName, Result) then
    Result := nil;
end;

procedure TGocciaClassValue.AddFieldInitializer(const AName: string; const AInitializer: TGocciaValue; const AIsPrivate, AIsStatic: Boolean);
begin
  SetLength(FDecoratorFieldInitializers, Length(FDecoratorFieldInitializers) + 1);
  FDecoratorFieldInitializers[High(FDecoratorFieldInitializers)].Name := AName;
  FDecoratorFieldInitializers[High(FDecoratorFieldInitializers)].Initializer := AInitializer;
  FDecoratorFieldInitializers[High(FDecoratorFieldInitializers)].IsPrivate := AIsPrivate;
  FDecoratorFieldInitializers[High(FDecoratorFieldInitializers)].IsStatic := AIsStatic;
end;

procedure TGocciaClassValue.SetMethodInitializers(const AInitializers: array of TGocciaValue);
var
  Idx: Integer;
begin
  SetLength(FMethodInitializers, Length(AInitializers));
  for Idx := 0 to High(AInitializers) do
    FMethodInitializers[Idx] := AInitializers[Idx];
end;

procedure TGocciaClassValue.SetFieldInitializers(const AInitializers: array of TGocciaValue);
var
  Idx: Integer;
begin
  SetLength(FFieldInitializers, Length(AInitializers));
  for Idx := 0 to High(AInitializers) do
    FFieldInitializers[Idx] := AInitializers[Idx];
end;

procedure TGocciaClassValue.AppendMethodInitializers(const AInitializers: array of TGocciaValue);
var
  OldLen, Idx: Integer;
begin
  OldLen := Length(FMethodInitializers);
  SetLength(FMethodInitializers, OldLen + Length(AInitializers));
  for Idx := 0 to High(AInitializers) do
    FMethodInitializers[OldLen + Idx] := AInitializers[Idx];
end;

procedure TGocciaClassValue.AppendFieldInitializers(const AInitializers: array of TGocciaValue);
var
  OldLen, Idx: Integer;
begin
  OldLen := Length(FFieldInitializers);
  SetLength(FFieldInitializers, OldLen + Length(AInitializers));
  for Idx := 0 to High(AInitializers) do
    FFieldInitializers[OldLen + Idx] := AInitializers[Idx];
end;

// TC39 proposal-decorators: auto-accessor creates backing getter/setter
procedure TGocciaClassValue.AddAutoAccessor(const AName, ABackingName: string; const AIsStatic: Boolean);
var
  GetterHelper: TGocciaAutoAccessorGetter;
  SetterHelper: TGocciaAutoAccessorSetter;
  GetterFn, SetterFn: TGocciaNativeFunctionValue;
  Target: TGocciaObjectValue;
begin
  GetterHelper := TGocciaAutoAccessorGetter.Create(ABackingName);
  SetterHelper := TGocciaAutoAccessorSetter.Create(ABackingName);

  GetterFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(GetterHelper.Get, 'get ' + AName, 0);
  SetterFn := TGocciaNativeFunctionValue.CreateWithoutPrototype(SetterHelper.SetValue, 'set ' + AName, 1);

  // Static auto-accessors go on the constructor; instance ones on the prototype
  if AIsStatic then
    Target := Self
  else
    Target := FClassPrototype;
  Target.DefineProperty(AName, TGocciaPropertyDescriptorAccessor.Create(
    GetterFn, SetterFn, [pfConfigurable, pfWritable]));
end;

procedure TGocciaClassValue.RunMethodInitializers(const AInstance: TGocciaValue);
var
  Idx: Integer;
  Args: TGocciaArgumentsCollection;
begin
  for Idx := 0 to High(FMethodInitializers) do
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      TGocciaFunctionBase(FMethodInitializers[Idx]).Call(Args, AInstance);
    finally
      Args.Free;
    end;
  end;
end;

procedure TGocciaClassValue.RunFieldInitializers(const AInstance: TGocciaValue);
var
  Idx: Integer;
  Args: TGocciaArgumentsCollection;
begin
  for Idx := 0 to High(FFieldInitializers) do
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      TGocciaFunctionBase(FFieldInitializers[Idx]).Call(Args, AInstance);
    finally
      Args.Free;
    end;
  end;
end;

procedure TGocciaClassValue.RunDecoratorFieldInitializers(const AInstance: TGocciaValue);
var
  Idx: Integer;
  Args: TGocciaArgumentsCollection;
  InitResult, OriginalValue: TGocciaValue;
begin
  for Idx := 0 to High(FDecoratorFieldInitializers) do
  begin
    if FDecoratorFieldInitializers[Idx].IsStatic then
      Continue;

    if Assigned(AInstance) and (AInstance is TGocciaObjectValue) then
    begin
      OriginalValue := TGocciaObjectValue(AInstance).GetProperty(FDecoratorFieldInitializers[Idx].Name);
      if not Assigned(OriginalValue) then
        OriginalValue := TGocciaUndefinedLiteralValue.UndefinedValue;

      Args := TGocciaArgumentsCollection.Create([OriginalValue]);
      try
        InitResult := TGocciaFunctionBase(FDecoratorFieldInitializers[Idx].Initializer).Call(Args, AInstance);
        if Assigned(InitResult) and not (InitResult is TGocciaUndefinedLiteralValue) then
          TGocciaObjectValue(AInstance).AssignProperty(FDecoratorFieldInitializers[Idx].Name, InitResult);
      finally
        Args.Free;
      end;
    end;
  end;
end;

procedure TGocciaClassValue.AddInstanceProperty(const AName: string; const AExpression: TGocciaExpression);
begin
  FInstancePropertyDefs.Add(AName, AExpression);
end;

procedure TGocciaClassValue.AddPrivateInstanceProperty(const AName: string; const AExpression: TGocciaExpression);
begin
  FPrivateInstancePropertyDefs.Add(AName, AExpression);
end;

procedure TGocciaClassValue.AddPrivateStaticProperty(const AName: string; const AValue: TGocciaValue);
begin
  FPrivateStaticProperties.AddOrSetValue(AName, AValue);
end;

procedure TGocciaClassValue.AddPrivateMethod(const AName: string; const AMethod: TGocciaMethodValue);
begin
  FPrivateMethods.AddOrSetValue(AName, AMethod);
end;

function TGocciaClassValue.GetPrivateMethod(const AName: string): TGocciaMethodValue;
begin
  if not FPrivateMethods.TryGetValue(AName, Result) then
    Result := nil;
end;

procedure TGocciaClassValue.AppendOwnPrivateNames(const ANames: TStrings);
var
  PropertyPair: TGocciaExpressionMap.TKeyValuePair;
  StaticPair: TGocciaValueMap.TKeyValuePair;
  MethodPair: TOrderedStringMap<TGocciaMethodValue>.TKeyValuePair;
  FuncPair: TOrderedStringMap<TGocciaFunctionBase>.TKeyValuePair;
begin
  if not Assigned(ANames) then
    Exit;

  for PropertyPair in FPrivateInstancePropertyDefs do
    if ANames.IndexOf(PropertyPair.Key) < 0 then
      ANames.Add(PropertyPair.Key);

  for StaticPair in FPrivateStaticProperties do
    if ANames.IndexOf(StaticPair.Key) < 0 then
      ANames.Add(StaticPair.Key);

  for MethodPair in FPrivateMethods do
    if ANames.IndexOf(MethodPair.Key) < 0 then
      ANames.Add(MethodPair.Key);

  for FuncPair in FPrivateGetters do
    if ANames.IndexOf(FuncPair.Key) < 0 then
      ANames.Add(FuncPair.Key);

  for FuncPair in FPrivateSetters do
    if ANames.IndexOf(FuncPair.Key) < 0 then
      ANames.Add(FuncPair.Key);
end;

function TGocciaClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := nil;
end;

procedure TGocciaClassValue.ReplacePrototype(const APrototype: TGocciaObjectValue);
begin
  FClassPrototype := APrototype;
end;

procedure TGocciaClassValue.SetInferredName(const AName: string);
begin
  // Only infer name for anonymous classes — named classes keep their own name.
  // FName update is sufficient; GetOwnPropertyDescriptor synthesizes the descriptor.
  if (FName = '') or (FName = '<anonymous>') then
    FName := AName;
end;

procedure TGocciaClassValue.SetFieldOrder(const AOrder: array of TGocciaClassFieldOrderEntry);
var
  I: Integer;
begin
  SetLength(FFieldOrder, Length(AOrder));
  for I := 0 to High(AOrder) do
    FFieldOrder[I] := AOrder[I];
end;

function TGocciaClassValue.FieldOrderCount: Integer;
begin
  Result := Length(FFieldOrder);
end;

function TGocciaClassValue.FieldOrderEntry(const AIndex: Integer): TGocciaClassFieldOrderEntry;
begin
  Result := FFieldOrder[AIndex];
end;

function TGocciaClassValue.EstimatedInstancePropertyCapacity: Integer;
var
  WalkClass: TGocciaClassValue;
  I: Integer;
begin
  Result := 0;
  WalkClass := Self;
  while Assigned(WalkClass) do
  begin
    Inc(Result, Length(WalkClass.FFieldOrder));
    for I := 0 to High(WalkClass.FDecoratorFieldInitializers) do
      if not WalkClass.FDecoratorFieldInitializers[I].IsStatic then
        Inc(Result);
    WalkClass := WalkClass.SuperClass;
  end;
end;

// ES2026 §10.2.2 [[Construct]](argumentsList, newTarget)
function TGocciaClassValue.Instantiate(const AArguments: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaClassValue): TGocciaValue;
var
  Instance: TGocciaObjectValue;
  WalkClass: TGocciaClassValue;
  NativeInstance: TGocciaObjectValue;
  ConstructorToCall: TGocciaMethodValue;
  InstancePrototype: TGocciaObjectValue;
begin
  // ES2026 §10.2.2 step 5: Let proto be ? GetPrototypeFromConstructor(newTarget)
  if Assigned(ANewTarget) then
    InstancePrototype := ANewTarget.Prototype
  else
    InstancePrototype := FClassPrototype;

  NativeInstance := nil;
  WalkClass := Self;
  while Assigned(WalkClass) do
  begin
    NativeInstance := WalkClass.CreateNativeInstance(AArguments);
    if Assigned(NativeInstance) then
      Break;
    WalkClass := WalkClass.SuperClass;
  end;

  // ES2026 §10.2.2 step 6: Set proto on the instance before constructor runs
  if Assigned(NativeInstance) then
  begin
    Instance := NativeInstance;
    Instance.Prototype := InstancePrototype;
    if NativeInstance is TGocciaInstanceValue then
      TGocciaInstanceValue(NativeInstance).ClassValue := Self;
  end
  else
  begin
    Instance := TGocciaInstanceValue.Create(Self,
      EstimatedInstancePropertyCapacity);
    Instance.Prototype := InstancePrototype;
  end;

  ConstructorToCall := FConstructorMethod;
  if not Assigned(ConstructorToCall) and Assigned(FSuperClass) then
    ConstructorToCall := FSuperClass.ConstructorMethod;

  if Assigned(ConstructorToCall) then
  begin
    TGarbageCollector.Instance.AddTempRoot(Instance);
    try
      ConstructorToCall.Call(AArguments, Instance);
    finally
      TGarbageCollector.Instance.RemoveTempRoot(Instance);
    end;
  end
  else if Assigned(NativeInstance) and (NativeInstance is TGocciaInstanceValue) then
    TGocciaInstanceValue(NativeInstance).InitializeNativeFromArguments(AArguments);

  Result := Instance;
end;

function TGocciaClassValue.GetClassLength: Integer;
var
  I: Integer;
  Params: TGocciaParameterArray;
begin
  // ECMAScript: a class's length is the number of formal parameters of its
  // constructor before the first default/rest. Native classes without a JS
  // constructor method (Map, Set, WeakMap, WeakSet, etc.) default to 0,
  // which matches the spec for built-in collection constructors.
  Result := 0;
  if not Assigned(FConstructorMethod) then
    Exit;
  Params := FConstructorMethod.Parameters;
  for I := 0 to Length(Params) - 1 do
  begin
    if Params[I].IsRest then Break;
    if Assigned(Params[I].DefaultValue) then Break;
    Inc(Result);
  end;
end;

function TGocciaClassValue.GetProperty(const AName: string): TGocciaValue;
var
  Getter: TGocciaFunctionBase;
  Args: TGocciaArgumentsCollection;
  Current: TGocciaClassValue;
begin
  if AName = PROP_PROTOTYPE then
  begin
    Result := FClassPrototype;
    Exit;
  end;

  if FStaticMethods.TryGetValue(AName, Result) then
    Exit;

  if FStaticGetters.TryGetValue(AName, Getter) then
  begin
    Args := TGocciaArgumentsCollection.CreateWithCapacity(0);
    try
      Result := Getter.Call(Args, Self);
    finally
      Args.Free;
    end;
    Exit;
  end;

  if FStaticSetters.ContainsKey(AName) then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  if AName = PROP_NAME then
  begin
    if FName = '<anonymous>' then
      Result := TGocciaStringLiteralValue.Create('')
    else
      Result := TGocciaStringLiteralValue.Create(FName);
    Exit;
  end;

  if AName = PROP_LENGTH then
  begin
    Result := TGocciaNumberLiteralValue.Create(GetClassLength);
    Exit;
  end;

  Current := FSuperClass;
  while Assigned(Current) do
  begin
    if Current.FStaticMethods.TryGetValue(AName, Result) then
      Exit;

    if Current.FStaticGetters.TryGetValue(AName, Getter) then
    begin
      Args := TGocciaArgumentsCollection.CreateWithCapacity(0);
      try
        Result := Getter.Call(Args, Self);
      finally
        Args.Free;
      end;
      Exit;
    end;

    if Current.FStaticSetters.ContainsKey(AName) then
    begin
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;

    Current := Current.FSuperClass;
  end;

  // Fall through to own properties (inherited from TGocciaObjectValue) and
  // then up the [[Prototype]] chain (Function.prototype → Object.prototype)
  Result := inherited GetProperty(AName);
end;

procedure TGocciaClassValue.SetProperty(const AName: string; const AValue: TGocciaValue);
var
  Setter: TGocciaFunctionBase;
  Args: TGocciaArgumentsCollection;
  Current: TGocciaClassValue;
begin
  Current := Self;
  repeat
    if Current.FStaticSetters.TryGetValue(AName, Setter) then
    begin
      Args := TGocciaArgumentsCollection.CreateWithCapacity(1);
      try
        Args.Add(AValue);
        Setter.Call(Args, Self);
      finally
        Args.Free;
      end;
      Exit;
    end;
    Current := Current.FSuperClass;
  until not Assigned(Current);

  // .name override via static field or assignment: use DefineProperty to
  // override the synthesized non-writable descriptor (which is configurable)
  if AName = PROP_NAME then
  begin
    if AValue is TGocciaStringLiteralValue then
      FName := TGocciaStringLiteralValue(AValue).Value;
    inherited DefineProperty(AName,
      TGocciaPropertyDescriptorData.Create(AValue, [pfConfigurable, pfWritable, pfEnumerable]));
    Exit;
  end;

  // Runtime property assignment (C.x = val) creates enumerable data properties
  inherited SetProperty(AName, AValue);
end;

function TGocciaClassValue.GetOwnPropertyDescriptor(const AName: string): TGocciaPropertyDescriptor;
begin
  if AName = PROP_PROTOTYPE then
    Result := TGocciaPropertyDescriptorData.Create(FClassPrototype, [])
  else if AName = PROP_NAME then
  begin
    // Check if .name was explicitly set (e.g. static name = 'Custom')
    Result := inherited GetOwnPropertyDescriptor(AName);
    if not Assigned(Result) then
    begin
      // Synthesize from FName: { writable: false, enumerable: false, configurable: true }
      if (FName = '') or (FName = '<anonymous>') then
        Result := TGocciaPropertyDescriptorData.Create(
          TGocciaStringLiteralValue.Create(''), [pfConfigurable])
      else
        Result := TGocciaPropertyDescriptorData.Create(
          TGocciaStringLiteralValue.Create(FName), [pfConfigurable]);
    end;
  end
  else if AName = PROP_LENGTH then
  begin
    // Honour explicit own-property redefinitions (length is configurable, so
    // userland may override via Object.defineProperty); fall back to a
    // synthesized descriptor only when no own descriptor exists.
    Result := inherited GetOwnPropertyDescriptor(AName);
    if not Assigned(Result) then
      Result := TGocciaPropertyDescriptorData.Create(
        TGocciaNumberLiteralValue.Create(GetClassLength), [pfConfigurable]);
  end
  else
    Result := inherited GetOwnPropertyDescriptor(AName);
end;

function TGocciaClassValue.HasOwnProperty(const AName: string): Boolean;
begin
  if (AName = PROP_PROTOTYPE) or (AName = PROP_NAME) or (AName = PROP_LENGTH) then
    Result := True
  else
    Result := inherited HasOwnProperty(AName);
end;

procedure TGocciaClassValue.DefineSymbolProperty(const ASymbol: TGocciaSymbolValue; const ADescriptor: TGocciaPropertyDescriptor);
begin
  FStaticSymbolDescriptors.AddOrSetValue(ASymbol, ADescriptor);
end;

function TGocciaClassValue.GetOwnStaticSymbolDescriptor(const ASymbol: TGocciaSymbolValue): TGocciaPropertyDescriptor;
begin
  if not FStaticSymbolDescriptors.TryGetValue(ASymbol, Result) then
    Result := nil;
end;

function TGocciaClassValue.GetOwnSymbolPropertyDescriptor(const ASymbol: TGocciaSymbolValue): TGocciaPropertyDescriptor;
begin
  Result := GetOwnStaticSymbolDescriptor(ASymbol);
  if not Assigned(Result) then
    Result := inherited GetOwnSymbolPropertyDescriptor(ASymbol);
end;

function TGocciaClassValue.GetOwnSymbols: TArray<TGocciaSymbolValue>;
var
  Symbols, InheritedSymbols: TArray<TGocciaSymbolValue>;
  Seen: THashMap<TGocciaSymbolValue, Boolean>;
  Symbol: TGocciaSymbolValue;
  Count: Integer;
begin
  Symbols := FStaticSymbolDescriptors.Keys;
  InheritedSymbols := inherited GetOwnSymbols;
  Seen := THashMap<TGocciaSymbolValue, Boolean>.Create;
  try
    SetLength(Result, Length(Symbols) + Length(InheritedSymbols));
    Count := 0;

    for Symbol in Symbols do
    begin
      if Seen.ContainsKey(Symbol) then
        Continue;
      Seen.Add(Symbol, True);
      Result[Count] := Symbol;
      Inc(Count);
    end;

    for Symbol in InheritedSymbols do
    begin
      if Seen.ContainsKey(Symbol) then
        Continue;
      Seen.Add(Symbol, True);
      Result[Count] := Symbol;
      Inc(Count);
    end;

    SetLength(Result, Count);
  finally
    Seen.Free;
  end;
end;

procedure TGocciaClassValue.AssignSymbolProperty(const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue);
var
  Descriptor: TGocciaPropertyDescriptor;
  Accessor: TGocciaPropertyDescriptorAccessor;
  Args: TGocciaArgumentsCollection;
  Current: TGocciaClassValue;
begin
  Current := Self;
  while Assigned(Current) do
  begin
    if Current.FStaticSymbolDescriptors.TryGetValue(ASymbol, Descriptor) then
    begin
      if Descriptor is TGocciaPropertyDescriptorAccessor then
      begin
        Accessor := TGocciaPropertyDescriptorAccessor(Descriptor);
      if Assigned(Accessor.Setter) then
      begin
        Args := TGocciaArgumentsCollection.Create;
        try
          Args.Add(AValue);
          if Accessor.Setter is TGocciaFunctionBase then
            TGocciaFunctionBase(Accessor.Setter).Call(Args, Self);
        finally
          Args.Free;
        end;
        Exit;
        end
        else
          ThrowTypeError(SErrorClassSetterOnlyAccessor, SSuggestPropertyHasOnlyGetter);
      end;
      Break;
    end;
    Current := Current.FSuperClass;
  end;

  DefineSymbolProperty(ASymbol, TGocciaPropertyDescriptorData.Create(AValue, [pfEnumerable, pfConfigurable, pfWritable]));
end;

function TGocciaClassValue.GetSymbolProperty(const ASymbol: TGocciaSymbolValue): TGocciaValue;
begin
  Result := GetSymbolPropertyWithReceiver(ASymbol, Self);
end;

function TGocciaClassValue.GetSymbolPropertyWithReceiver(const ASymbol: TGocciaSymbolValue; const AReceiver: TGocciaValue): TGocciaValue;
var
  Descriptor: TGocciaPropertyDescriptor;
  Accessor: TGocciaPropertyDescriptorAccessor;
  Args: TGocciaArgumentsCollection;
begin
  if FStaticSymbolDescriptors.TryGetValue(ASymbol, Descriptor) then
  begin
    if Descriptor is TGocciaPropertyDescriptorData then
    begin
      Result := TGocciaPropertyDescriptorData(Descriptor).Value;
      Exit;
    end
    else if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      Accessor := TGocciaPropertyDescriptorAccessor(Descriptor);
      if Assigned(Accessor.Getter) then
      begin
        if Accessor.Getter is TGocciaFunctionBase then
        begin
          Args := TGocciaArgumentsCollection.CreateWithCapacity(0);
          try
            Result := TGocciaFunctionBase(Accessor.Getter).Call(Args, AReceiver);
          finally
            Args.Free;
          end;
        end
        else
          Result := TGocciaUndefinedLiteralValue.UndefinedValue;
        Exit;
      end;
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;
  end;

  if Assigned(FSuperClass) then
    Result := FSuperClass.GetSymbolPropertyWithReceiver(ASymbol, AReceiver)
  else
    Result := inherited GetSymbolPropertyWithReceiver(ASymbol, AReceiver);
end;

// ES2026 §10.2.2 [[Call]] — class constructors are not callable without new
function TGocciaClassValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // String/Number/Boolean act as type conversion functions when called without new
  // ES2026 §22.1.1.1 String(value) — Symbol returns SymbolDescriptiveString
  if FName = CONSTRUCTOR_STRING then
  begin
    if AArguments.Length = 0 then
      Result := TGocciaStringLiteralValue.Create('')
    else if AArguments.GetElement(0) is TGocciaSymbolValue then
      Result := TGocciaSymbolValue(AArguments.GetElement(0)).ToDisplayString
    else
      Result := AArguments.GetElement(0).ToStringLiteral;
  end
  else if FName = CONSTRUCTOR_NUMBER then
  begin
    if AArguments.Length = 0 then
      Result := TGocciaNumberLiteralValue.ZeroValue
    else
      Result := AArguments.GetElement(0).ToNumberLiteral;
  end
  else if FName = CONSTRUCTOR_BOOLEAN then
  begin
    if AArguments.Length = 0 then
      Result := TGocciaBooleanLiteralValue.FalseValue
    else
      Result := AArguments.GetElement(0).ToBooleanLiteral;
  end
  else if FName = CONSTRUCTOR_ARRAY then
    Result := Instantiate(AArguments)
  // ES2026 §20.1.1.1 Object(value): ToObject for primitives, new empty object otherwise
  else if FName = CONSTRUCTOR_OBJECT then
  begin
    if (AArguments.Length > 0) and
       not (AArguments.GetElement(0) is TGocciaUndefinedLiteralValue) and
       not (AArguments.GetElement(0) is TGocciaNullLiteralValue) then
    begin
      if AArguments.GetElement(0).IsPrimitive then
        Result := AArguments.GetElement(0).Box
      else
        Result := AArguments.GetElement(0);
    end
    else
      Result := Instantiate(AArguments);
  end
  else
    ThrowTypeError(Format(SErrorClassConstructorRequiresNew, [FName]), SSuggestRequiresNew);
end;

{ TGocciaArrayClassValue }

function TGocciaArrayClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaArrayValue.Create;
end;

function TGocciaArrayClassValue.GetClassLength: Integer;
begin
  Result := 1;
end;

{ TGocciaMapClassValue }

function TGocciaMapClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaMapValue.Create;
end;

{ TGocciaSetClassValue }

function TGocciaSetClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaSetValue.Create;
end;

{ TGocciaWeakMapClassValue }

function TGocciaWeakMapClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaWeakMapValue.Create;
end;

{ TGocciaWeakSetClassValue }

function TGocciaWeakSetClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaWeakSetValue.Create;
end;

{ TGocciaArrayBufferClassValue }

function TGocciaArrayBufferClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaArrayBufferValue.Create(nil);
end;

function TGocciaArrayBufferClassValue.GetClassLength: Integer;
begin
  Result := 1;
end;

{ TGocciaSharedArrayBufferClassValue }

function TGocciaSharedArrayBufferClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaSharedArrayBufferValue.Create(nil);
end;

function TGocciaSharedArrayBufferClassValue.GetClassLength: Integer;
begin
  Result := 1;
end;

{ TGocciaTextEncoderClassValue }

function TGocciaTextEncoderClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaTextEncoderValue.Create;
end;

{ TGocciaTextDecoderClassValue }

function TGocciaTextDecoderClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaTextDecoderValue.Create;
end;

{ TGocciaURLClassValue }

function TGocciaURLClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaURLValue.Create(nil);
end;

function TGocciaURLClassValue.GetClassLength: Integer;
begin
  Result := 1;
end;

{ TGocciaURLSearchParamsClassValue }

function TGocciaURLSearchParamsClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaURLSearchParamsValue.Create(nil);
end;

{ TGocciaHeadersClassValue }

function TGocciaHeadersClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaHeadersValue.Create;
end;

{ TGocciaResponseClassValue }

function TGocciaResponseClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaResponseValue.Create;
end;

function TGocciaResponseClassValue.GetClassLength: Integer;
begin
  Result := 1;
end;

{ TGocciaStringClassValue }

function TGocciaStringClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
var
  Prim: TGocciaStringLiteralValue;
begin
  if AArguments.Length = 0 then
    Prim := TGocciaStringLiteralValue.Create('')
  else if AArguments.GetElement(0) is TGocciaSymbolValue then
    Prim := TGocciaSymbolValue(AArguments.GetElement(0)).ToDisplayString
  else
    Prim := AArguments.GetElement(0).ToStringLiteral;
  Result := Prim.Box;
end;

function TGocciaStringClassValue.GetClassLength: Integer;
begin
  Result := 1;
end;

{ TGocciaNumberClassValue }

function TGocciaNumberClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
var
  Prim: TGocciaNumberLiteralValue;
begin
  if AArguments.Length = 0 then
    Prim := TGocciaNumberLiteralValue.ZeroValue
  else
    Prim := AArguments.GetElement(0).ToNumberLiteral;
  Result := Prim.Box;
end;

function TGocciaNumberClassValue.GetClassLength: Integer;
begin
  Result := 1;
end;

{ TGocciaBooleanClassValue }

function TGocciaBooleanClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
var
  Prim: TGocciaBooleanLiteralValue;
begin
  if AArguments.Length = 0 then
    Prim := TGocciaBooleanLiteralValue.FalseValue
  else
    Prim := AArguments.GetElement(0).ToBooleanLiteral;
  Result := Prim.Box;
end;

function TGocciaBooleanClassValue.GetClassLength: Integer;
begin
  Result := 1;
end;

{ TGocciaFunctionConstructorClassValue }

constructor TGocciaFunctionConstructorClassValue.Create(const AName: string;
  const ASuperClass: TGocciaClassValue);
begin
  inherited Create(AName, ASuperClass);
  FEnabled := False;
  FCompileDynamicFunction := nil;
end;

function TGocciaFunctionConstructorClassValue.BuildFunction(
  const AArguments: TGocciaArgumentsCollection): TGocciaFunctionBase;
var
  ParamSources: array of string;
  BodyStr: string;
  I: Integer;
begin
  if not FEnabled then
    ThrowTypeError('Dynamic code generation is disabled. ' +
      'Pass --unsafe-function-constructor to enable the Function constructor');

  if not Assigned(FCompileDynamicFunction) then
    ThrowTypeError('Function constructor is not available in this environment');

  // ES2026 §20.2.1.1: collect parameters and body from arguments
  if AArguments.Length = 0 then
  begin
    SetLength(ParamSources, 0);
    BodyStr := '';
  end
  else if AArguments.Length = 1 then
  begin
    SetLength(ParamSources, 0);
    BodyStr := AArguments.GetElement(0).ToStringLiteral.Value;
  end
  else
  begin
    SetLength(ParamSources, AArguments.Length - 1);
    for I := 0 to AArguments.Length - 2 do
      ParamSources[I] := AArguments.GetElement(I).ToStringLiteral.Value;
    BodyStr := AArguments.GetElement(AArguments.Length - 1).ToStringLiteral.Value;
  end;

  Result := FCompileDynamicFunction(ParamSources, BodyStr);
end;

function TGocciaFunctionConstructorClassValue.Call(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  // ES2026 §20.2.1.1: Function(...) is equivalent to new Function(...)
  Result := BuildFunction(AArguments);
end;

function TGocciaFunctionConstructorClassValue.CreateNativeInstance(
  const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := BuildFunction(AArguments);
end;

function TGocciaFunctionConstructorClassValue.GetClassLength: Integer;
begin
  Result := 1;
end;

{ TGocciaInstanceValue }

constructor TGocciaInstanceValue.Create(const AClass: TGocciaClassValue = nil;
  const APropertyCapacity: Integer = 0);
begin
  inherited Create(nil, APropertyCapacity);
  FClass := AClass;
end;

function TGocciaInstanceValue.TypeName: string;
begin
  if Assigned(FClass) then
    Result := FClass.Name
  else
    Result := CONSTRUCTOR_OBJECT;
end;

function TGocciaInstanceValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  if Assigned(FClass) then
    Result := TGocciaStringLiteralValue.Create(Format('[Instance of %s]', [FClass.Name]))
  else
    Result := TGocciaStringLiteralValue.Create('[object Object]');
end;

function TGocciaInstanceValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaInstanceValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
var
  Descriptor: TGocciaPropertyDescriptor;
  Args: TGocciaArgumentsCollection;
  Method: TGocciaFunctionValue;
begin
  if FProperties.TryGetValue(AName, Descriptor) then
  begin
    if Descriptor is TGocciaPropertyDescriptorData then
      Result := TGocciaPropertyDescriptorData(Descriptor).Value
    else if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      if Assigned(TGocciaPropertyDescriptorAccessor(Descriptor).Getter) and
         TGocciaPropertyDescriptorAccessor(Descriptor).Getter.IsCallable then
      begin
        Args := TGocciaArgumentsCollection.CreateWithCapacity(0);
        try
          Result := TGocciaFunctionBase(TGocciaPropertyDescriptorAccessor(Descriptor).Getter)
            .Call(Args, AThisContext);
        finally
          Args.Free;
        end;
      end
      else
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    end
    else
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  // Check for getters/setters on the prototype with the receiver as context
  if Assigned(FPrototype) then
  begin
    Result := FPrototype.GetPropertyWithContext(AName, AThisContext);
    if not (Result is TGocciaUndefinedLiteralValue) then
      Exit;
  end;

  if Assigned(FClass) then
  begin
    Method := FClass.GetMethod(AName);
    if Assigned(Method) then
    begin
      Result := Method;
      Exit;
    end;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaInstanceValue.AssignProperty(const AName: string; const AValue: TGocciaValue; const ACanCreate: Boolean = True);
var
  Descriptor: TGocciaPropertyDescriptor;
  Args: TGocciaArgumentsCollection;
  Proto: TGocciaObjectValue;
begin
  if FProperties.TryGetValue(AName, Descriptor) and
     (Descriptor is TGocciaPropertyDescriptorData) then
  begin
    if not TGocciaPropertyDescriptorData(Descriptor).Writable then
      ThrowTypeError(Format(SErrorCannotAssignReadOnly, [AName]), SSuggestCannotDeleteNonConfigurable);
    TGocciaPropertyDescriptorData(Descriptor).Value := AValue;
    Exit;
  end;

  Proto := FPrototype;
  while Assigned(Proto) do
  begin
    Descriptor := Proto.GetOwnPropertyDescriptor(AName);
    if Assigned(Descriptor) then
    begin
      if (Descriptor is TGocciaPropertyDescriptorAccessor) and
         Assigned(TGocciaPropertyDescriptorAccessor(Descriptor).Setter) then
      begin
        Args := TGocciaArgumentsCollection.CreateWithCapacity(1);
        try
          Args.Add(AValue);
          TGocciaFunctionBase(TGocciaPropertyDescriptorAccessor(Descriptor).Setter)
            .Call(Args, Self);
        finally
          Args.Free;
        end;
        Exit;
      end
      else if Descriptor is TGocciaPropertyDescriptorAccessor then
      begin
        ThrowTypeError(Format(SErrorSetPropertyOnlyGetter, [AName, ToStringTag]), SSuggestPropertyHasOnlyGetter);
      end
      else if (Descriptor is TGocciaPropertyDescriptorData) and
              (not TGocciaPropertyDescriptorData(Descriptor).Writable) then
        ThrowTypeError(Format(SErrorCannotAssignReadOnly, [AName]), SSuggestCannotDeleteNonConfigurable);
    end;
    Proto := Proto.Prototype;
  end;

  if not ACanCreate then
    ThrowTypeError(Format(SErrorCannotAssignNonExistent, [AName]), SSuggestCannotDeleteNonConfigurable);

  if not FExtensible then
    ThrowTypeError(Format(SErrorCannotAddPropertyNotExtensible, [AName]), SSuggestObjectNotExtensible);

  DefineProperty(AName, TGocciaPropertyDescriptorData.Create(AValue,
    [pfEnumerable, pfConfigurable, pfWritable]));
end;

procedure TGocciaInstanceValue.SetProperty(const AName: string; const AValue: TGocciaValue);
begin
  // Delegate to AssignProperty which has the setter logic
  AssignProperty(AName, AValue);
end;

function TGocciaInstanceValue.TryGetRawPrivateProperty(const AKey: string;
  out AValue: TGocciaValue): Boolean;
begin
  Result := Assigned(FPrivateProperties) and
    FPrivateProperties.TryGetValue(AKey, AValue);
end;

procedure TGocciaInstanceValue.SetRawPrivateProperty(const AKey: string;
  const AValue: TGocciaValue);
begin
  if not Assigned(FPrivateProperties) then
    FPrivateProperties := TGocciaValueMap.Create;
  FPrivateProperties.AddOrSetValue(AKey, AValue);
end;

procedure TGocciaInstanceValue.InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection);
begin
  // No-op by default; native subclasses override to handle constructor arguments
end;

function TGocciaInstanceValue.IsInstanceOf(const AClass: TGocciaClassValue): Boolean; inline;
var
  CurrentClass: TGocciaClassValue;
begin
  Result := False;
  CurrentClass := FClass;

  while Assigned(CurrentClass) do
  begin
    if CurrentClass = AClass then
    begin
      Result := True;
      Exit;
    end;
    CurrentClass := CurrentClass.SuperClass;
  end;
end;

function TGocciaInstanceValue.GetPrivateProperty(const AName: string; const AAccessClass: TGocciaClassValue): TGocciaValue;
var
  CurrentClass: TGocciaClassValue;
  CanAccess: Boolean;
  CompositeKey: string;
begin
  // Check if the accessing class can access this private field
  // Rule: A class can access private fields if:
  // 1. It's accessing its own private field (AAccessClass = the class that owns the field)
  // 2. It's a subclass of the class that owns the field
  CanAccess := False;

  // Check if AAccessClass is the same as FClass (derived class accessing derived class field)
  if AAccessClass = FClass then
  begin
    CanAccess := True;
  end
  else
  begin
    // Check if AAccessClass is in the inheritance chain of FClass (superclass accessing its own field)
    CurrentClass := FClass;
    while Assigned(CurrentClass) do
    begin
      if CurrentClass = AAccessClass then
      begin
        CanAccess := True;
        Break;
      end;
      CurrentClass := CurrentClass.SuperClass;
    end;
  end;

  if not CanAccess then
    raise TGocciaError.Create(Format('Private field "%s" is not accessible', [AName]), 0, 0, '', nil);

  // Use composite key (ClassName:FieldName) to support per-class private field scoping
  CompositeKey := AAccessClass.Name + ':' + AName;
  if Assigned(FPrivateProperties) and FPrivateProperties.TryGetValue(CompositeKey, Result) then
    Exit
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaInstanceValue.SetPrivateProperty(const AName: string; const AValue: TGocciaValue; const AAccessClass: TGocciaClassValue);
var
  CurrentClass: TGocciaClassValue;
  CanAccess: Boolean;
  CompositeKey: string;
begin
  // Check if the accessing class can access this private field
  // Rule: A class can access private fields if:
  // 1. It's accessing its own private field (AAccessClass = the class that owns the field)
  // 2. It's a subclass of the class that owns the field
  CanAccess := False;

  // Check if AAccessClass is the same as FClass (derived class accessing derived class field)
  if AAccessClass = FClass then
  begin
    CanAccess := True;
  end
  else
  begin
    // Check if AAccessClass is in the inheritance chain of FClass (superclass accessing its own field)
    CurrentClass := FClass;
    while Assigned(CurrentClass) do
    begin
      if CurrentClass = AAccessClass then
      begin
        CanAccess := True;
        Break;
      end;
      CurrentClass := CurrentClass.SuperClass;
    end;
  end;

  if not CanAccess then
    raise TGocciaError.Create(Format('Private field "%s" is not accessible', [AName]), 0, 0, '', nil);

  // Use composite key (ClassName:FieldName) to support per-class private field scoping
  CompositeKey := AAccessClass.Name + ':' + AName;
  if not Assigned(FPrivateProperties) then
    FPrivateProperties := TGocciaValueMap.Create;
  FPrivateProperties.AddOrSetValue(CompositeKey, AValue);
end;

destructor TGocciaInstanceValue.Destroy;
begin
  FPrivateProperties.Free;
  inherited;
end;

procedure TGocciaInstanceValue.MarkReferences;
var
  ValPair: TGocciaValueMap.TKeyValuePair;
begin
  if GCMarked then Exit;
  inherited;

  if Assigned(FClass) then
    FClass.MarkReferences;

  if Assigned(FPrivateProperties) then
    for ValPair in FPrivateProperties do
      if Assigned(ValPair.Value) then
        ValPair.Value.MarkReferences;
end;

end.
