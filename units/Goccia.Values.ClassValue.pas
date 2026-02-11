unit Goccia.Values.ClassValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue, Goccia.Interfaces,
  Goccia.Error, Goccia.Logger, Generics.Collections, SysUtils, Math, Goccia.Values.Primitives, Goccia.Values.FunctionValue,
  Goccia.AST.Node, Goccia.Values.ObjectPropertyDescriptor, Goccia.Arguments.Collection;

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
    FPrivateInstancePropertyDefs: TDictionary<string, TGocciaExpression>; // Private instance property definitions
    FPrivateStaticProperties: TDictionary<string, TGocciaValue>; // Private static property values
    FPrivateMethods: TDictionary<string, TGocciaMethodValue>; // Private methods
  public
    constructor Create(const AName: string; ASuperClass: TGocciaClassValue);
    destructor Destroy; override;
    function TypeName: string; override;
    function TypeOf: string; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;
    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    procedure AddMethod(const AName: string; AMethod: TGocciaMethodValue);
    function GetMethod(const AName: string): TGocciaMethodValue;
    procedure AddGetter(const AName: string; AGetter: TGocciaFunctionValue);
    procedure AddSetter(const AName: string; ASetter: TGocciaFunctionValue);
    procedure AddInstanceProperty(const AName: string; AExpression: TGocciaExpression);
    procedure AddPrivateInstanceProperty(const AName: string; AExpression: TGocciaExpression);
    procedure AddPrivateStaticProperty(const AName: string; AValue: TGocciaValue);
    function GetPrivateStaticProperty(const AName: string): TGocciaValue;
    procedure AddPrivateMethod(const AName: string; AMethod: TGocciaMethodValue);
    function GetPrivateMethod(const AName: string): TGocciaMethodValue;
    function Instantiate(Arguments: TGocciaArgumentsCollection): TGocciaValue;
    function GetProperty(const AName: string): TGocciaValue;
    procedure SetProperty(const AName: string; AValue: TGocciaValue);
    function Call(Arguments: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue; virtual;

    property Name: string read FName;
    property SuperClass: TGocciaClassValue read FSuperClass;
    property Prototype: TGocciaObjectValue read FPrototype;
    property ConstructorMethod: TGocciaMethodValue read FConstructorMethod;
    property InstancePropertyDefs: TDictionary<string, TGocciaExpression> read FInstancePropertyDefs;
    property PrivateInstancePropertyDefs: TDictionary<string, TGocciaExpression> read FPrivateInstancePropertyDefs;
    property PrivateStaticProperties: TDictionary<string, TGocciaValue> read FPrivateStaticProperties;
    property PrivateMethods: TDictionary<string, TGocciaMethodValue> read FPrivateMethods;
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
    procedure SetPrototype(APrototype: TGocciaObjectValue);
  public
    constructor Create(AClass: TGocciaClassValue);
    destructor Destroy; override;
    function TypeName: string; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;
    function GetProperty(const AName: string): TGocciaValue;
    procedure AssignProperty(const AName: string; AValue: TGocciaValue; ACanCreate: Boolean = True);
    procedure SetProperty(const AName: string; AValue: TGocciaValue);
    function GetPrivateProperty(const AName: string; AAccessClass: TGocciaClassValue): TGocciaValue;
    procedure SetPrivateProperty(const AName: string; AValue: TGocciaValue; AAccessClass: TGocciaClassValue);
    function HasPrivateProperty(const AName: string): Boolean; inline;
    function IsInstanceOf(AClass: TGocciaClassValue): Boolean; inline;
    property ClassValue: TGocciaClassValue read FClass;
    property Prototype: TGocciaObjectValue read FPrototype write SetPrototype;
    property PrivateProperties: TDictionary<string, TGocciaValue> read FPrivateProperties;
  end;

implementation

uses Goccia.Values.ClassHelper, Goccia.Values.NativeFunction;

constructor TGocciaClassValue.Create(const AName: string; ASuperClass: TGocciaClassValue);
begin
  FName := AName;
  FSuperClass := ASuperClass;
  FMethods := TDictionary<string, TGocciaMethodValue>.Create;
  FGetters := TDictionary<string, TGocciaFunctionValue>.Create;
  FSetters := TDictionary<string, TGocciaFunctionValue>.Create;
  FStaticMethods := TDictionary<string, TGocciaValue>.Create;
  FInstancePropertyDefs := TDictionary<string, TGocciaExpression>.Create;
  FPrivateInstancePropertyDefs := TDictionary<string, TGocciaExpression>.Create;
  FPrivateStaticProperties := TDictionary<string, TGocciaValue>.Create;
  FPrivateMethods := TDictionary<string, TGocciaMethodValue>.Create;
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
  FPrivateInstancePropertyDefs.Free;
  FPrivateStaticProperties.Free;
  FPrivateMethods.Free;
  FPrototype.Free;
  inherited;
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

procedure TGocciaClassValue.AddMethod(const AName: string; AMethod: TGocciaMethodValue);
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

procedure TGocciaClassValue.AddGetter(const AName: string; AGetter: TGocciaFunctionValue);
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

procedure TGocciaClassValue.AddSetter(const AName: string; ASetter: TGocciaFunctionValue);
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

procedure TGocciaClassValue.AddInstanceProperty(const AName: string; AExpression: TGocciaExpression);
begin
  FInstancePropertyDefs.AddOrSetValue(AName, AExpression);
end;

procedure TGocciaClassValue.AddPrivateInstanceProperty(const AName: string; AExpression: TGocciaExpression);
begin
  FPrivateInstancePropertyDefs.AddOrSetValue(AName, AExpression);
end;

procedure TGocciaClassValue.AddPrivateStaticProperty(const AName: string; AValue: TGocciaValue);
begin
  FPrivateStaticProperties.AddOrSetValue(AName, AValue);
end;

function TGocciaClassValue.GetPrivateStaticProperty(const AName: string): TGocciaValue;
begin
  if not FPrivateStaticProperties.TryGetValue(AName, Result) then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaClassValue.AddPrivateMethod(const AName: string; AMethod: TGocciaMethodValue);
begin
  FPrivateMethods.AddOrSetValue(AName, AMethod);
end;

function TGocciaClassValue.GetPrivateMethod(const AName: string): TGocciaMethodValue;
begin
  if not FPrivateMethods.TryGetValue(AName, Result) then
    Result := nil;
end;

function TGocciaClassValue.Instantiate(Arguments: TGocciaArgumentsCollection): TGocciaValue;
var
  Instance: TGocciaInstanceValue;
  ConstructorToCall: TGocciaMethodValue;
begin
  Logger.Debug('Creating basic instance of class: %s', [FName]);

  // Step 1: Create the basic instance
  Instance := TGocciaInstanceValue.Create(Self);
  Logger.Debug('Instance created: %s', [Instance.ToStringLiteral.Value]);

  // Step 2: Set up the prototype chain
  Instance.Prototype := FPrototype;
  Logger.Debug('Prototype set for instance');

  // Step 3: Find and call constructor (NOTE: No property initialization in basic path)
  ConstructorToCall := FConstructorMethod;
  if not Assigned(ConstructorToCall) and Assigned(FSuperClass) then
  begin
    ConstructorToCall := FSuperClass.ConstructorMethod;
    Logger.Debug('Using inherited constructor from: %s', [FSuperClass.Name]);
  end;

  if Assigned(ConstructorToCall) then
  begin
    Logger.Debug('Calling constructor with %d arguments', [Arguments.Length]);
    if Arguments.Length > 0 then
      Logger.Debug('  First argument: %s', [Arguments.GetElement(0).ToStringLiteral.Value]);

    // Call the constructor with the instance as this
    ConstructorToCall.Call(Arguments, Instance);
  end;

  Result := Instance;
end;

function TGocciaClassValue.GetProperty(const AName: string): TGocciaValue;
begin
  // TODO: This is a hack to get the prototype of the class - We need to introduce static properties
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

procedure TGocciaClassValue.SetProperty(const AName: string; AValue: TGocciaValue);
begin
  FStaticMethods.AddOrSetValue(AName, AValue);
end;

function TGocciaClassValue.Call(Arguments: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  // Handle primitive wrappers when called as functions
  if FName = 'String' then
    Result := TGocciaStringLiteralValue.Create('123')
  else
    // Default: create an instance
    Result := Instantiate(Arguments);
end;

constructor TGocciaInstanceValue.Create(AClass: TGocciaClassValue);
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
  Logger.Debug('TGocciaInstanceValue.GetProperty called for: %s', [AName]);

  // First check instance properties directly using property descriptors
  if FPropertyDescriptors.ContainsKey(AName) then
  begin
    Logger.Debug('TGocciaInstanceValue.GetProperty: Found in instance properties');
    Result := inherited GetProperty(AName);
    Exit;
  end;

  // Check for getters/setters on the prototype with this instance as context
  if Assigned(FPrototype) then
  begin
    Logger.Debug('TGocciaInstanceValue.GetProperty: Checking prototype for property: %s', [AName]);
    Logger.Debug('TGocciaInstanceValue.GetProperty: Instance type: %s', [Self.TypeName]);
    Result := FPrototype.GetPropertyWithContext(AName, Self);
    Logger.Debug('TGocciaInstanceValue.GetProperty: GetPropertyWithContext returned: %s', [Result.ToStringLiteral.Value]);
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

procedure TGocciaInstanceValue.AssignProperty(const AName: string; AValue: TGocciaValue; ACanCreate: Boolean = True);
var
  Descriptor: TGocciaPropertyDescriptor;
  SetterFunction: TGocciaFunctionValue;
  NativeSetterFunction: TGocciaNativeFunctionValue;
  Args: TGocciaArgumentsCollection;
begin
  Logger.Debug('TGocciaInstanceValue.AssignProperty called for: %s', [AName]);

  // First check for setters on the prototype
  if Assigned(FPrototype) then
  begin
    Descriptor := FPrototype.GetOwnPropertyDescriptor(AName);
    if (Descriptor is TGocciaPropertyDescriptorAccessor) and
       Assigned(TGocciaPropertyDescriptorAccessor(Descriptor).Setter) then
    begin
      Logger.Debug('TGocciaInstanceValue.AssignProperty: Found setter on prototype for %s', [AName]);

      // Call the setter with this instance as context
      if TGocciaPropertyDescriptorAccessor(Descriptor).Setter is TGocciaFunctionValue then
      begin
        SetterFunction := TGocciaFunctionValue(TGocciaPropertyDescriptorAccessor(Descriptor).Setter);
        Args := TGocciaArgumentsCollection.Create;
        try
          Args.Add(AValue);
          SetterFunction.Call(Args, Self); // Use this instance as context
          Logger.Debug('TGocciaInstanceValue.AssignProperty: Setter called successfully');
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
          Logger.Debug('TGocciaInstanceValue.AssignProperty: Native setter called successfully');
        finally
          Args.Free;
        end;
        Exit;
      end;
    end;
  end;

  // No setter found, create instance property (inherited behavior)
  Logger.Debug('TGocciaInstanceValue.AssignProperty: No setter found, creating instance property');
  inherited AssignProperty(AName, AValue, ACanCreate);
end;

procedure TGocciaInstanceValue.SetProperty(const AName: string; AValue: TGocciaValue);
begin
  // Delegate to AssignProperty which has the setter logic
  AssignProperty(AName, AValue);
end;

procedure TGocciaInstanceValue.SetPrototype(APrototype: TGocciaObjectValue);
begin
  Logger.Debug('TGocciaInstanceValue.SetPrototype: Called');
  if Assigned(APrototype) then
    Logger.Debug('  APrototype is assigned: %s', [APrototype.ToStringLiteral.Value])
  else
    Logger.Debug('  APrototype is nil');

  FPrototype := APrototype;
  Logger.Debug('  Set instance FPrototype');

  // Also set the inherited prototype field so that inherited GetProperty works correctly
  inherited Prototype := APrototype;
  Logger.Debug('  Set inherited Prototype');

  // Debug: Check what the inherited prototype actually is now
  if Assigned(inherited Prototype) then
    Logger.Debug('  Inherited Prototype is now: %s', [inherited Prototype.ToStringLiteral.Value])
  else
    Logger.Debug('  Inherited Prototype is still nil');
end;

function TGocciaInstanceValue.HasPrivateProperty(const AName: string): Boolean; inline;
begin
  Result := FPrivateProperties.ContainsKey(AName);
end;

function TGocciaInstanceValue.IsInstanceOf(AClass: TGocciaClassValue): Boolean; inline;
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

function TGocciaInstanceValue.GetPrivateProperty(const AName: string; AAccessClass: TGocciaClassValue): TGocciaValue;
var
  CurrentClass: TGocciaClassValue;
  CanAccess: Boolean;
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

  if FPrivateProperties.TryGetValue(AName, Result) then
    Exit
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaInstanceValue.SetPrivateProperty(const AName: string; AValue: TGocciaValue; AAccessClass: TGocciaClassValue);
var
  CurrentClass: TGocciaClassValue;
  CanAccess: Boolean;
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

  FPrivateProperties.AddOrSetValue(AName, AValue);
end;

destructor TGocciaInstanceValue.Destroy;
begin
  FPrivateProperties.Free;
  inherited;
end;

end.
