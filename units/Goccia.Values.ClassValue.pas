unit Goccia.Values.ClassValue;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,
  Math,
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.AST.Node,
  Goccia.Error,
  Goccia.Interfaces,
  Goccia.Values.FunctionValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

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
    FPrivateGetters: TDictionary<string, TGocciaFunctionValue>; // Private getters
    FPrivateSetters: TDictionary<string, TGocciaFunctionValue>; // Private setters
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
    function Instantiate(const AArguments: TGocciaArgumentsCollection): TGocciaValue;
    function GetProperty(const AName: string): TGocciaValue; override;
    procedure SetProperty(const AName: string; const AValue: TGocciaValue); override;
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; virtual;

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
    procedure GCMarkReferences; override;

    property PrivateGetters: TDictionary<string, TGocciaFunctionValue> read FPrivateGetters;
    property PrivateSetters: TDictionary<string, TGocciaFunctionValue> read FPrivateSetters;
  end;

  // Subclasses for primitive wrapper constructors
  TGocciaStringClassValue = class(TGocciaClassValue)
  end;

  TGocciaNumberClassValue = class(TGocciaClassValue)
  end;

  TGocciaBooleanClassValue = class(TGocciaClassValue)
  end;

  TGocciaInstanceValue = class(TGocciaObjectValue)
  private
    FClass: TGocciaClassValue;
    FPrototype: TGocciaObjectValue;
    FPrivateProperties: TDictionary<string, TGocciaValue>; // Private field values
    procedure SetPrototype(const APrototype: TGocciaObjectValue);
  public
    constructor Create(const AClass: TGocciaClassValue);
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
    procedure GCMarkReferences; override;

    property ClassValue: TGocciaClassValue read FClass;
    property Prototype: TGocciaObjectValue read FPrototype write SetPrototype;
    property PrivateProperties: TDictionary<string, TGocciaValue> read FPrivateProperties;
  end;

implementation

uses
  Goccia.GarbageCollector,
  Goccia.Values.ClassHelper,
  Goccia.Values.NativeFunction;

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
  // Don't free FPrototype - it's GC-managed
  inherited;
end;

procedure TGocciaClassValue.GCMarkReferences;
var
  MethodPair: TPair<string, TGocciaMethodValue>;
  FuncPair: TPair<string, TGocciaFunctionValue>;
  ValuePair: TPair<string, TGocciaValue>;
begin
  if GCMarked then Exit;
  inherited; // Sets mark

  // Mark superclass
  if Assigned(FSuperClass) then
    FSuperClass.GCMarkReferences;

  // Mark prototype
  if Assigned(FPrototype) then
    FPrototype.GCMarkReferences;

  // Mark constructor
  if Assigned(FConstructorMethod) then
    FConstructorMethod.GCMarkReferences;

  // Mark methods
  for MethodPair in FMethods do
    MethodPair.Value.GCMarkReferences;

  // Mark getters and setters
  for FuncPair in FGetters do
    FuncPair.Value.GCMarkReferences;
  for FuncPair in FSetters do
    FuncPair.Value.GCMarkReferences;

  // Mark static methods
  for ValuePair in FStaticMethods do
    if Assigned(ValuePair.Value) then
      ValuePair.Value.GCMarkReferences;

  // Mark private static properties
  for ValuePair in FPrivateStaticProperties do
    if Assigned(ValuePair.Value) then
      ValuePair.Value.GCMarkReferences;

  // Mark private methods
  for MethodPair in FPrivateMethods do
    MethodPair.Value.GCMarkReferences;

  // Mark private getters and setters
  for FuncPair in FPrivateGetters do
    FuncPair.Value.GCMarkReferences;
  for FuncPair in FPrivateSetters do
    FuncPair.Value.GCMarkReferences;
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

function TGocciaClassValue.Instantiate(const AArguments: TGocciaArgumentsCollection): TGocciaValue;
var
  Instance: TGocciaInstanceValue;
  ConstructorToCall: TGocciaMethodValue;
begin
  // Step 1: Create the basic instance
  Instance := TGocciaInstanceValue.Create(Self);

  // Step 2: Set up the prototype chain
  Instance.Prototype := FPrototype;

  // Step 3: Find and call constructor (NOTE: No property initialization in basic path)
  ConstructorToCall := FConstructorMethod;
  if not Assigned(ConstructorToCall) and Assigned(FSuperClass) then
    ConstructorToCall := FSuperClass.ConstructorMethod;

  if Assigned(ConstructorToCall) then
  begin
    TGocciaGC.Instance.AddTempRoot(Instance);
    try
      ConstructorToCall.Call(AArguments, Instance);
    finally
      TGocciaGC.Instance.RemoveTempRoot(Instance);
    end;
  end;

  Result := Instance;
end;

function TGocciaClassValue.GetProperty(const AName: string): TGocciaValue;
begin
  // Class.prototype access - required for prototype chain inspection.
  // When static property support is added, this should be handled through
  // the general static property mechanism instead of this special case.
  if AName = 'prototype' then
  begin
    Result := FPrototype;
    Exit;
  end;

  if FStaticMethods.TryGetValue(AName, Result) then
    Exit;

  // Check inherited static methods
  if Assigned(FSuperClass) then
    Result := FSuperClass.GetProperty(AName)
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaClassValue.SetProperty(const AName: string; const AValue: TGocciaValue);
begin
  FStaticMethods.AddOrSetValue(AName, AValue);
end;

function TGocciaClassValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Handle primitive wrappers when called as functions (type conversion)
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
  else
    // Default: create an instance
    Result := Instantiate(AArguments);
end;

constructor TGocciaInstanceValue.Create(const AClass: TGocciaClassValue);
begin
  inherited Create;
  FClass := AClass;
  FPrototype := nil;
  FPrivateProperties := TDictionary<string, TGocciaValue>.Create;
end;

function TGocciaInstanceValue.TypeName: string;
begin
  Result := FClass.Name;
end;

function TGocciaInstanceValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  Result := TGocciaStringLiteralValue.Create(Format('[Instance of %s]', [FClass.Name]));
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

  // Check for class methods
  Method := FClass.GetMethod(AName);
  if Assigned(Method) then
  begin
    Result := Method;
    Exit;
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

procedure TGocciaInstanceValue.SetPrototype(const APrototype: TGocciaObjectValue);
begin
  FPrototype := APrototype;

  // Also set the inherited prototype field so that inherited GetProperty works correctly
  inherited Prototype := APrototype;
end;

function TGocciaInstanceValue.HasPrivateProperty(const AName: string): Boolean;
var
  Key: string;
  Suffix: string;
begin
  // Private properties are stored with composite keys (ClassName:FieldName)
  // Check if any class's version of this field exists
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
  if FPrivateProperties.TryGetValue(CompositeKey, Result) then
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
  FPrivateProperties.AddOrSetValue(CompositeKey, AValue);
end;

destructor TGocciaInstanceValue.Destroy;
begin
  FPrivateProperties.Free;
  inherited;
end;

procedure TGocciaInstanceValue.GCMarkReferences;
var
  ValuePair: TPair<string, TGocciaValue>;
begin
  if GCMarked then Exit;
  inherited; // Marks self + object properties/prototype

  // Mark class reference
  if Assigned(FClass) then
    FClass.GCMarkReferences;

  // Mark private properties
  for ValuePair in FPrivateProperties do
    if Assigned(ValuePair.Value) then
      ValuePair.Value.GCMarkReferences;
end;

end.
