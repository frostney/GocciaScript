unit Goccia.Values.ClassValue;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.AST.Node,
  Goccia.Interfaces,
  Goccia.Values.FunctionValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue;

type
  // Forward declaration
  TGocciaInstanceValue = class;

  TGocciaClassValue = class(TGocciaValue, IGocciaCallable)
  private
    FName: string;
    FSuperClass: TGocciaClassValue;
    FMethods: TDictionary<string, TGocciaMethodValue>;
    FGetters: TDictionary<string, TGocciaFunctionValue>;
    FSetters: TDictionary<string, TGocciaFunctionValue>;
    FPrototype: TGocciaObjectValue;
    FConstructorMethod: TGocciaMethodValue;
    FStaticMethods: TDictionary<string, TGocciaValue>; // For static methods like Array.isArray
    FInstancePropertyDefs: TDictionary<string, TGocciaExpression>; // Instance property definitions
    FInstancePropertyOrder: TStringList; // Preserves declaration order for instance properties
    FPrivateInstancePropertyDefs: TDictionary<string, TGocciaExpression>; // Private instance property definitions
    FPrivateInstancePropertyOrder: TStringList; // Preserves declaration order for private instance properties
    FPrivateStaticProperties: TDictionary<string, TGocciaValue>; // Private static property values
    FPrivateMethods: TDictionary<string, TGocciaMethodValue>; // Private methods
    FPrivateGetters: TDictionary<string, TGocciaFunctionValue>;
    FPrivateSetters: TDictionary<string, TGocciaFunctionValue>;
    FStaticGetters: TDictionary<string, TGocciaFunctionValue>;
    FStaticSetters: TDictionary<string, TGocciaFunctionValue>;
    FStaticSymbolDescriptors: TDictionary<TGocciaSymbolValue, TGocciaPropertyDescriptor>;
  public
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
    procedure AddGetter(const AName: string; const AGetter: TGocciaFunctionValue);
    procedure AddSetter(const AName: string; const ASetter: TGocciaFunctionValue);
    procedure AddStaticGetter(const AName: string; const AGetter: TGocciaFunctionValue);
    procedure AddStaticSetter(const AName: string; const ASetter: TGocciaFunctionValue);
    procedure AddPrivateGetter(const AName: string; const AGetter: TGocciaFunctionValue);
    procedure AddPrivateSetter(const AName: string; const ASetter: TGocciaFunctionValue);
    function HasPrivateGetter(const AName: string): Boolean;
    function HasPrivateSetter(const AName: string): Boolean;
    function GetPrivateGetter(const AName: string): TGocciaFunctionValue;
    function GetPrivateSetter(const AName: string): TGocciaFunctionValue;
    procedure AddInstanceProperty(const AName: string; const AExpression: TGocciaExpression);
    procedure AddPrivateInstanceProperty(const AName: string; const AExpression: TGocciaExpression);
    procedure AddPrivateStaticProperty(const AName: string; const AValue: TGocciaValue);
    function GetPrivateStaticProperty(const AName: string): TGocciaValue;
    procedure AddPrivateMethod(const AName: string; const AMethod: TGocciaMethodValue);
    function GetPrivateMethod(const AName: string): TGocciaMethodValue;
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; virtual;
    function Instantiate(const AArguments: TGocciaArgumentsCollection): TGocciaValue; virtual;
    function GetProperty(const AName: string): TGocciaValue; override;
    procedure SetProperty(const AName: string; const AValue: TGocciaValue); override;
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; virtual;

    procedure ReplacePrototype(const APrototype: TGocciaObjectValue);
    procedure DefineSymbolProperty(const ASymbol: TGocciaSymbolValue; const ADescriptor: TGocciaPropertyDescriptor);
    function GetSymbolProperty(const ASymbol: TGocciaSymbolValue): TGocciaValue;
    function GetSymbolPropertyWithReceiver(const ASymbol: TGocciaSymbolValue; const AReceiver: TGocciaValue): TGocciaValue;

    property Name: string read FName;
    property SuperClass: TGocciaClassValue read FSuperClass;
    property Prototype: TGocciaObjectValue read FPrototype;
    property ConstructorMethod: TGocciaMethodValue read FConstructorMethod;
    property InstancePropertyDefs: TDictionary<string, TGocciaExpression> read FInstancePropertyDefs;
    property InstancePropertyOrder: TStringList read FInstancePropertyOrder;
    property PrivateInstancePropertyDefs: TDictionary<string, TGocciaExpression> read FPrivateInstancePropertyDefs;
    property PrivateInstancePropertyOrder: TStringList read FPrivateInstancePropertyOrder;
    property PrivateStaticProperties: TDictionary<string, TGocciaValue> read FPrivateStaticProperties;
    property PrivateMethods: TDictionary<string, TGocciaMethodValue> read FPrivateMethods;
    procedure MarkReferences; override;

    property PrivateGetters: TDictionary<string, TGocciaFunctionValue> read FPrivateGetters;
    property PrivateSetters: TDictionary<string, TGocciaFunctionValue> read FPrivateSetters;
  end;

  TGocciaArrayClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
  end;

  TGocciaMapClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
  end;

  TGocciaSetClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
  end;

  TGocciaStringClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
  end;

  TGocciaNumberClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
  end;

  TGocciaBooleanClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
  end;

  TGocciaInstanceValue = class(TGocciaObjectValue)
  private
    FClass: TGocciaClassValue;
    FPrivateProperties: TDictionary<string, TGocciaValue>;
  public
    constructor Create(const AClass: TGocciaClassValue = nil);
    destructor Destroy; override;
    function TypeName: string; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;
    function GetProperty(const AName: string): TGocciaValue; override;
    procedure AssignProperty(const AName: string; const AValue: TGocciaValue; const ACanCreate: Boolean = True); override;
    procedure SetProperty(const AName: string; const AValue: TGocciaValue); override;
    function GetPrivateProperty(const AName: string; const AAccessClass: TGocciaClassValue): TGocciaValue;
    procedure SetPrivateProperty(const AName: string; const AValue: TGocciaValue; const AAccessClass: TGocciaClassValue);
    function HasPrivateProperty(const AName: string): Boolean;
    function IsInstanceOf(const AClass: TGocciaClassValue): Boolean; inline;
    procedure InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); virtual;
    procedure MarkReferences; override;

    property ClassValue: TGocciaClassValue read FClass write FClass;
    property PrivateProperties: TDictionary<string, TGocciaValue> read FPrivateProperties;
  end;

implementation

uses
  SysUtils,

  Goccia.Error,
  Goccia.GarbageCollector,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.MapValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.SetValue;

constructor TGocciaClassValue.Create(const AName: string; const ASuperClass: TGocciaClassValue);
begin
  FName := AName;
  FSuperClass := ASuperClass;
  FMethods := TDictionary<string, TGocciaMethodValue>.Create;
  FGetters := TDictionary<string, TGocciaFunctionValue>.Create;
  FSetters := TDictionary<string, TGocciaFunctionValue>.Create;
  FStaticMethods := TDictionary<string, TGocciaValue>.Create;
  FInstancePropertyDefs := TDictionary<string, TGocciaExpression>.Create;
  FInstancePropertyOrder := TStringList.Create;
  FPrivateInstancePropertyDefs := TDictionary<string, TGocciaExpression>.Create;
  FPrivateInstancePropertyOrder := TStringList.Create;
  FPrivateStaticProperties := TDictionary<string, TGocciaValue>.Create;
  FPrivateMethods := TDictionary<string, TGocciaMethodValue>.Create;
  FPrivateGetters := TDictionary<string, TGocciaFunctionValue>.Create;
  FPrivateSetters := TDictionary<string, TGocciaFunctionValue>.Create;
  FStaticGetters := TDictionary<string, TGocciaFunctionValue>.Create;
  FStaticSetters := TDictionary<string, TGocciaFunctionValue>.Create;
  FStaticSymbolDescriptors := TDictionary<TGocciaSymbolValue, TGocciaPropertyDescriptor>.Create;
  FPrototype := TGocciaObjectValue.Create;
  FConstructorMethod := nil;
  if Assigned(FSuperClass) then
    FPrototype.Prototype := FSuperClass.Prototype;
end;

destructor TGocciaClassValue.Destroy;
begin
  FMethods.Free;
  FGetters.Free;
  FSetters.Free;
  FStaticMethods.Free;
  FInstancePropertyDefs.Free;
  FInstancePropertyOrder.Free;
  FPrivateInstancePropertyDefs.Free;
  FPrivateInstancePropertyOrder.Free;
  FPrivateStaticProperties.Free;
  FPrivateMethods.Free;
  FPrivateGetters.Free;
  FPrivateSetters.Free;
  FStaticGetters.Free;
  FStaticSetters.Free;
  FStaticSymbolDescriptors.Free;
  // Don't free FPrototype - it's GC-managed
  inherited;
end;

procedure TGocciaClassValue.MarkReferences;
var
  MethodPair: TPair<string, TGocciaMethodValue>;
  FuncPair: TPair<string, TGocciaFunctionValue>;
  ValuePair: TPair<string, TGocciaValue>;
  SymPair: TPair<TGocciaSymbolValue, TGocciaPropertyDescriptor>;
  Accessor: TGocciaPropertyDescriptorAccessor;
begin
  if GCMarked then Exit;
  inherited; // Sets mark

  // Mark superclass
  if Assigned(FSuperClass) then
    FSuperClass.MarkReferences;

  // Mark prototype
  if Assigned(FPrototype) then
    FPrototype.MarkReferences;

  // Mark constructor
  if Assigned(FConstructorMethod) then
    FConstructorMethod.MarkReferences;

  // Mark methods
  for MethodPair in FMethods do
    MethodPair.Value.MarkReferences;

  // Mark getters and setters
  for FuncPair in FGetters do
    FuncPair.Value.MarkReferences;
  for FuncPair in FSetters do
    FuncPair.Value.MarkReferences;

  // Mark static methods
  for ValuePair in FStaticMethods do
    if Assigned(ValuePair.Value) then
      ValuePair.Value.MarkReferences;

  // Mark private static properties
  for ValuePair in FPrivateStaticProperties do
    if Assigned(ValuePair.Value) then
      ValuePair.Value.MarkReferences;

  // Mark private methods
  for MethodPair in FPrivateMethods do
    MethodPair.Value.MarkReferences;

  // Mark private getters and setters
  for FuncPair in FPrivateGetters do
    FuncPair.Value.MarkReferences;
  for FuncPair in FPrivateSetters do
    FuncPair.Value.MarkReferences;

  // Mark static getters and setters
  for FuncPair in FStaticGetters do
    FuncPair.Value.MarkReferences;
  for FuncPair in FStaticSetters do
    FuncPair.Value.MarkReferences;

  // Mark static symbol descriptors
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
end;

function TGocciaClassValue.IsCallable: Boolean;
begin
  Result := True;
end;

function TGocciaClassValue.TypeName: string;
begin
  Result := 'function';
end;

function TGocciaClassValue.TypeOf: string;
begin
  Result := 'function';
end;

function TGocciaClassValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  // For error classes, just return the name to make toThrow work properly
  if (FName = 'RangeError') or (FName = 'Error') or (FName = 'TypeError') or (FName = 'ReferenceError') then
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
  if AName = 'constructor' then
  begin
    FConstructorMethod := AMethod;
    Exit;
  end;

  FMethods.AddOrSetValue(AName, AMethod);
  FPrototype.AssignProperty(AName, AMethod);
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

procedure TGocciaClassValue.AddGetter(const AName: string; const AGetter: TGocciaFunctionValue);
var
  ExistingDescriptor: TGocciaPropertyDescriptor;
  ExistingSetter: TGocciaValue;
begin
  FGetters.AddOrSetValue(AName, AGetter);

  // Check if there's already a setter for this property
  ExistingDescriptor := FPrototype.GetOwnPropertyDescriptor(AName);
  if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
     Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter) then
  begin
    // Merge with existing setter
    ExistingSetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter;
    FPrototype.DefineProperty(AName, TGocciaPropertyDescriptorAccessor.Create(AGetter, ExistingSetter, [pfEnumerable, pfConfigurable, pfWritable]));
  end
  else
  begin
    // No existing setter, create getter-only descriptor
    FPrototype.DefineProperty(AName, TGocciaPropertyDescriptorAccessor.Create(AGetter, nil, [pfEnumerable, pfConfigurable, pfWritable]));
  end;
end;

procedure TGocciaClassValue.AddSetter(const AName: string; const ASetter: TGocciaFunctionValue);
var
  ExistingDescriptor: TGocciaPropertyDescriptor;
  ExistingGetter: TGocciaValue;
begin
  FSetters.AddOrSetValue(AName, ASetter);

  // Check if there's already a getter for this property
  ExistingDescriptor := FPrototype.GetOwnPropertyDescriptor(AName);
  if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
     Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter) then
  begin
    // Merge with existing getter
    ExistingGetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter;
    FPrototype.DefineProperty(AName, TGocciaPropertyDescriptorAccessor.Create(ExistingGetter, ASetter, [pfEnumerable, pfConfigurable, pfWritable]));
  end
  else
  begin
    // No existing getter, create setter-only descriptor
    FPrototype.DefineProperty(AName, TGocciaPropertyDescriptorAccessor.Create(nil, ASetter, [pfEnumerable, pfConfigurable, pfWritable]));
  end;
end;

procedure TGocciaClassValue.AddStaticGetter(const AName: string; const AGetter: TGocciaFunctionValue);
begin
  FStaticGetters.AddOrSetValue(AName, AGetter);
end;

procedure TGocciaClassValue.AddStaticSetter(const AName: string; const ASetter: TGocciaFunctionValue);
begin
  FStaticSetters.AddOrSetValue(AName, ASetter);
end;

procedure TGocciaClassValue.AddPrivateGetter(const AName: string; const AGetter: TGocciaFunctionValue);
begin
  FPrivateGetters.AddOrSetValue(AName, AGetter);
end;

procedure TGocciaClassValue.AddPrivateSetter(const AName: string; const ASetter: TGocciaFunctionValue);
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

function TGocciaClassValue.GetPrivateGetter(const AName: string): TGocciaFunctionValue;
begin
  if FPrivateGetters.TryGetValue(AName, Result) then
    Exit;
  if Assigned(FSuperClass) then
    Result := FSuperClass.GetPrivateGetter(AName)
  else
    Result := nil;
end;

function TGocciaClassValue.GetPrivateSetter(const AName: string): TGocciaFunctionValue;
begin
  if FPrivateSetters.TryGetValue(AName, Result) then
    Exit;
  if Assigned(FSuperClass) then
    Result := FSuperClass.GetPrivateSetter(AName)
  else
    Result := nil;
end;

procedure TGocciaClassValue.AddInstanceProperty(const AName: string; const AExpression: TGocciaExpression);
begin
  FInstancePropertyDefs.AddOrSetValue(AName, AExpression);
  if FInstancePropertyOrder.IndexOf(AName) = -1 then
    FInstancePropertyOrder.Add(AName);
end;

procedure TGocciaClassValue.AddPrivateInstanceProperty(const AName: string; const AExpression: TGocciaExpression);
begin
  FPrivateInstancePropertyDefs.AddOrSetValue(AName, AExpression);
  if FPrivateInstancePropertyOrder.IndexOf(AName) = -1 then
    FPrivateInstancePropertyOrder.Add(AName);
end;

procedure TGocciaClassValue.AddPrivateStaticProperty(const AName: string; const AValue: TGocciaValue);
begin
  FPrivateStaticProperties.AddOrSetValue(AName, AValue);
end;

function TGocciaClassValue.GetPrivateStaticProperty(const AName: string): TGocciaValue;
begin
  if not FPrivateStaticProperties.TryGetValue(AName, Result) then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
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

function TGocciaClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := nil;
end;

procedure TGocciaClassValue.ReplacePrototype(const APrototype: TGocciaObjectValue);
begin
  FPrototype := APrototype;
end;

function TGocciaClassValue.Instantiate(const AArguments: TGocciaArgumentsCollection): TGocciaValue;
var
  Instance: TGocciaObjectValue;
  WalkClass: TGocciaClassValue;
  NativeInstance: TGocciaObjectValue;
  ConstructorToCall: TGocciaMethodValue;
begin
  NativeInstance := nil;
  WalkClass := Self;
  while Assigned(WalkClass) do
  begin
    NativeInstance := WalkClass.CreateNativeInstance(AArguments);
    if Assigned(NativeInstance) then
      Break;
    WalkClass := WalkClass.SuperClass;
  end;

  if Assigned(NativeInstance) then
  begin
    Instance := NativeInstance;
    Instance.Prototype := FPrototype;
    if NativeInstance is TGocciaInstanceValue then
      TGocciaInstanceValue(NativeInstance).ClassValue := Self;
  end
  else
  begin
    Instance := TGocciaInstanceValue.Create(Self);
    Instance.Prototype := FPrototype;
  end;

  ConstructorToCall := FConstructorMethod;
  if not Assigned(ConstructorToCall) and Assigned(FSuperClass) then
    ConstructorToCall := FSuperClass.ConstructorMethod;

  if Assigned(ConstructorToCall) then
  begin
    TGocciaGarbageCollector.Instance.AddTempRoot(Instance);
    try
      ConstructorToCall.Call(AArguments, Instance);
    finally
      TGocciaGarbageCollector.Instance.RemoveTempRoot(Instance);
    end;
  end
  else if Assigned(NativeInstance) and (NativeInstance is TGocciaInstanceValue) then
    TGocciaInstanceValue(NativeInstance).InitializeNativeFromArguments(AArguments);

  Result := Instance;
end;

function TGocciaClassValue.GetProperty(const AName: string): TGocciaValue;
var
  Getter: TGocciaFunctionValue;
  Args: TGocciaArgumentsCollection;
begin
  if AName = 'prototype' then
  begin
    Result := FPrototype;
    Exit;
  end;

  if FStaticMethods.TryGetValue(AName, Result) then
    Exit;

  if FStaticGetters.TryGetValue(AName, Getter) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Result := Getter.Call(Args, Self);
    finally
      Args.Free;
    end;
    Exit;
  end;

  if Assigned(FSuperClass) then
    Result := FSuperClass.GetProperty(AName)
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaClassValue.SetProperty(const AName: string; const AValue: TGocciaValue);
var
  Setter: TGocciaFunctionValue;
  Args: TGocciaArgumentsCollection;
begin
  if FStaticSetters.TryGetValue(AName, Setter) then
  begin
    Args := TGocciaArgumentsCollection.Create([AValue]);
    try
      Setter.Call(Args, Self);
    finally
      Args.Free;
    end;
    Exit;
  end;

  FStaticMethods.AddOrSetValue(AName, AValue);
end;

procedure TGocciaClassValue.DefineSymbolProperty(const ASymbol: TGocciaSymbolValue; const ADescriptor: TGocciaPropertyDescriptor);
begin
  FStaticSymbolDescriptors.AddOrSetValue(ASymbol, ADescriptor);
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
        Args := TGocciaArgumentsCollection.Create;
        try
          if Accessor.Getter is TGocciaNativeFunctionValue then
            Result := TGocciaNativeFunctionValue(Accessor.Getter).Call(Args, AReceiver)
          else if Accessor.Getter is TGocciaFunctionValue then
            Result := TGocciaFunctionValue(Accessor.Getter).Call(Args, AReceiver)
          else
            Result := TGocciaUndefinedLiteralValue.UndefinedValue;
        finally
          Args.Free;
        end;
        Exit;
      end;
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;
  end;

  if Assigned(FSuperClass) then
    Result := FSuperClass.GetSymbolPropertyWithReceiver(ASymbol, AReceiver)
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaClassValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if FName = 'String' then
  begin
    if AArguments.Length = 0 then
      Result := TGocciaStringLiteralValue.Create('')
    else
      Result := AArguments.GetElement(0).ToStringLiteral;
  end
  else if FName = 'Number' then
  begin
    if AArguments.Length = 0 then
      Result := TGocciaNumberLiteralValue.ZeroValue
    else
      Result := AArguments.GetElement(0).ToNumberLiteral;
  end
  else if FName = 'Boolean' then
  begin
    if AArguments.Length = 0 then
      Result := TGocciaBooleanLiteralValue.FalseValue
    else
      Result := AArguments.GetElement(0).ToBooleanLiteral;
  end
  else if (FName = 'Array') or (FName = 'Map') or (FName = 'Set') then
    Result := Instantiate(AArguments)
  else
    Result := Instantiate(AArguments);
end;

{ TGocciaArrayClassValue }

function TGocciaArrayClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaArrayValue.Create;
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

{ TGocciaStringClassValue }

function TGocciaStringClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
var
  Prim: TGocciaStringLiteralValue;
begin
  if AArguments.Length = 0 then
    Prim := TGocciaStringLiteralValue.Create('')
  else
    Prim := AArguments.GetElement(0).ToStringLiteral;
  Result := Prim.Box;
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

{ TGocciaInstanceValue }

constructor TGocciaInstanceValue.Create(const AClass: TGocciaClassValue = nil);
begin
  inherited Create;
  FClass := AClass;
end;

function TGocciaInstanceValue.TypeName: string;
begin
  if Assigned(FClass) then
    Result := FClass.Name
  else
    Result := 'Object';
end;

function TGocciaInstanceValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  if Assigned(FClass) then
    Result := TGocciaStringLiteralValue.Create(Format('[Instance of %s]', [FClass.Name]))
  else
    Result := TGocciaStringLiteralValue.Create('[object Object]');
end;

function TGocciaInstanceValue.GetProperty(const AName: string): TGocciaValue;
var
  Method: TGocciaFunctionValue;
  Descriptor: TGocciaPropertyDescriptor;
  GetterFunction: TGocciaFunctionValue;
  Args: TGocciaArgumentsCollection;
begin
  // First check instance properties directly using property descriptors
  if FPropertyDescriptors.ContainsKey(AName) then
  begin
    Result := inherited GetProperty(AName);
    Exit;
  end;

  // Check for getters/setters on the prototype with this instance as context
  if Assigned(FPrototype) then
  begin
    Result := FPrototype.GetPropertyWithContext(AName, Self);
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
  SetterFunction: TGocciaFunctionValue;
  NativeSetterFunction: TGocciaNativeFunctionValue;
  Args: TGocciaArgumentsCollection;
begin
  // First check for setters on the prototype
  if Assigned(FPrototype) then
  begin
    Descriptor := FPrototype.GetOwnPropertyDescriptor(AName);
    if (Descriptor is TGocciaPropertyDescriptorAccessor) and
       Assigned(TGocciaPropertyDescriptorAccessor(Descriptor).Setter) then
    begin
      // Call the setter with this instance as context
      if TGocciaPropertyDescriptorAccessor(Descriptor).Setter is TGocciaFunctionValue then
      begin
        SetterFunction := TGocciaFunctionValue(TGocciaPropertyDescriptorAccessor(Descriptor).Setter);
        Args := TGocciaArgumentsCollection.Create;
        try
          Args.Add(AValue);
          SetterFunction.Call(Args, Self); // Use this instance as context
        finally
          Args.Free;
        end;
        Exit;
      end
      else if TGocciaPropertyDescriptorAccessor(Descriptor).Setter is TGocciaNativeFunctionValue then
      begin
        NativeSetterFunction := TGocciaNativeFunctionValue(TGocciaPropertyDescriptorAccessor(Descriptor).Setter);
        Args := TGocciaArgumentsCollection.Create;
        try
          Args.Add(AValue);
          NativeSetterFunction.Call(Args, Self); // Use this instance as context
        finally
          Args.Free;
        end;
        Exit;
      end;
    end;
  end;

  // No setter found, create instance property (inherited behavior)
  inherited AssignProperty(AName, AValue, ACanCreate);
end;

procedure TGocciaInstanceValue.SetProperty(const AName: string; const AValue: TGocciaValue);
begin
  // Delegate to AssignProperty which has the setter logic
  AssignProperty(AName, AValue);
end;

function TGocciaInstanceValue.HasPrivateProperty(const AName: string): Boolean;
var
  Key: string;
  Suffix: string;
begin
  if not Assigned(FPrivateProperties) then
  begin
    Result := False;
    Exit;
  end;
  Suffix := ':' + AName;
  for Key in FPrivateProperties.Keys do
  begin
    if (Key = AName) or (Copy(Key, Length(Key) - Length(Suffix) + 1, Length(Suffix)) = Suffix) then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
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
    FPrivateProperties := TDictionary<string, TGocciaValue>.Create;
  FPrivateProperties.AddOrSetValue(CompositeKey, AValue);
end;

destructor TGocciaInstanceValue.Destroy;
begin
  FPrivateProperties.Free;
  inherited;
end;

procedure TGocciaInstanceValue.MarkReferences;
var
  ValuePair: TPair<string, TGocciaValue>;
begin
  if GCMarked then Exit;
  inherited; // Marks self + object properties/prototype

  // Mark class reference
  if Assigned(FClass) then
    FClass.MarkReferences;

  if Assigned(FPrivateProperties) then
    for ValuePair in FPrivateProperties do
      if Assigned(ValuePair.Value) then
        ValuePair.Value.MarkReferences;
end;

end.
