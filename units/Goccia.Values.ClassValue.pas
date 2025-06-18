unit Goccia.Values.ClassValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Values.FunctionValue, Goccia.Values.ObjectValue, Goccia.Interfaces,
  Goccia.Error, Goccia.Logger, Generics.Collections, SysUtils, Math, Goccia.Values.UndefinedValue,
  Goccia.AST.Node;

type
  // Forward declaration
  TGocciaInstanceValue = class;

  TGocciaClassValue = class(TGocciaValue)
  private
    FName: string;
    FSuperClass: TGocciaClassValue;
    FMethods: TDictionary<string, TGocciaMethodValue>;
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
    function ToString: string; override;
    function ToBoolean: Boolean; override;
    function ToNumber: Double; override;
    function TypeName: string; override;
    procedure AddMethod(const AName: string; AMethod: TGocciaMethodValue);
    function GetMethod(const AName: string): TGocciaMethodValue;
    procedure AddInstanceProperty(const AName: string; AExpression: TGocciaExpression);
    procedure AddPrivateInstanceProperty(const AName: string; AExpression: TGocciaExpression);
    procedure AddPrivateStaticProperty(const AName: string; AValue: TGocciaValue);
    function GetPrivateStaticProperty(const AName: string): TGocciaValue;
    procedure AddPrivateMethod(const AName: string; AMethod: TGocciaMethodValue);
    function GetPrivateMethod(const AName: string): TGocciaMethodValue;
    function Instantiate(Arguments: TObjectList<TGocciaValue>): TGocciaValue;
    function GetProperty(const AName: string): TGocciaValue;
    procedure SetProperty(const AName: string; AValue: TGocciaValue);
    property Name: string read FName;
    property SuperClass: TGocciaClassValue read FSuperClass;
    property Prototype: TGocciaObjectValue read FPrototype;
    property ConstructorMethod: TGocciaMethodValue read FConstructorMethod;
    property InstancePropertyDefs: TDictionary<string, TGocciaExpression> read FInstancePropertyDefs;
    property PrivateInstancePropertyDefs: TDictionary<string, TGocciaExpression> read FPrivateInstancePropertyDefs;
    property PrivateStaticProperties: TDictionary<string, TGocciaValue> read FPrivateStaticProperties;
    property PrivateMethods: TDictionary<string, TGocciaMethodValue> read FPrivateMethods;
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
    function GetProperty(const AName: string): TGocciaValue;
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

constructor TGocciaClassValue.Create(const AName: string; ASuperClass: TGocciaClassValue);
begin
  FName := AName;
  FSuperClass := ASuperClass;
  FMethods := TDictionary<string, TGocciaMethodValue>.Create;
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
  FStaticMethods.Free;
  FInstancePropertyDefs.Free;
  FPrivateInstancePropertyDefs.Free;
  FPrivateStaticProperties.Free;
  FPrivateMethods.Free;
  FPrototype.Free;
  inherited;
end;

function TGocciaClassValue.ToString: string;
begin
  Result := Format('[Class: %s]', [FName]);
end;

function TGocciaClassValue.ToBoolean: Boolean;
begin
  Result := True;
end;

function TGocciaClassValue.ToNumber: Double;
begin
  Result := NaN;
end;

function TGocciaClassValue.TypeName: string;
begin
  Result := 'function';
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
  FPrototype.SetProperty(AName, AMethod);
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
    Result := TGocciaUndefinedValue.Create;
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

function TGocciaClassValue.Instantiate(Arguments: TObjectList<TGocciaValue>): TGocciaValue;
var
  Instance: TGocciaInstanceValue;
  ConstructorToCall: TGocciaMethodValue;
begin
  // Create the instance
  Instance := TGocciaInstanceValue.Create(Self);
  Logger.Debug('Instance created: %s', [Instance.ToString]);
  // Set up the prototype chain
  Instance.Prototype := FPrototype;
  Logger.Debug('Prototype set for instance');

  // NOTE: Instance properties will be initialized by the evaluator
  // after instantiation but before returning to ensure proper order

  // Find constructor - either this class's or inherited from parent
  ConstructorToCall := FConstructorMethod;
  if not Assigned(ConstructorToCall) and Assigned(FSuperClass) then
  begin
    ConstructorToCall := FSuperClass.ConstructorMethod;
    Logger.Debug('Using inherited constructor from: %s', [FSuperClass.Name]);
  end;

  // Call constructor if one exists
  if Assigned(ConstructorToCall) then
  begin
    Logger.Debug('Calling constructor');
    Logger.Debug('  Arguments.Count: %d', [Arguments.Count]);
    if Arguments.Count > 0 then
      Logger.Debug('  First argument: %s', [Arguments[0].ToString]);

    // Call the constructor with the instance as this
    ConstructorToCall.Call(Arguments, Instance);
  end;

  Result := Instance;
end;

function TGocciaClassValue.GetProperty(const AName: string): TGocciaValue;
begin
  if FStaticMethods.TryGetValue(AName, Result) then
    Exit;

  // Check inherited static methods
  if Assigned(FSuperClass) then
    Result := FSuperClass.GetProperty(AName)
  else
    Result := TGocciaUndefinedValue.Create;
end;

procedure TGocciaClassValue.SetProperty(const AName: string; AValue: TGocciaValue);
begin
  FStaticMethods.AddOrSetValue(AName, AValue);
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

function TGocciaInstanceValue.GetProperty(const AName: string): TGocciaValue;
var
  Method: TGocciaFunctionValue;
begin
  // First check if the property exists in the instance
  Result := inherited GetProperty(AName);

  // If not found in instance (inherited GetProperty returns TGocciaUndefinedValue), check class methods
  if Result is TGocciaUndefinedValue then
  begin
    // Check if it's a method from the class
    Method := FClass.GetMethod(AName);
    if Assigned(Method) then
      Result := Method;
  end;
end;

procedure TGocciaInstanceValue.SetProperty(const AName: string; AValue: TGocciaValue);
begin
  // Always set properties on the instance, not the prototype
  inherited SetProperty(AName, AValue);
end;

procedure TGocciaInstanceValue.SetPrototype(APrototype: TGocciaObjectValue);
begin
  Logger.Debug('TGocciaInstanceValue.SetPrototype: Called');
  if Assigned(APrototype) then
    Logger.Debug('  APrototype is assigned: %s', [APrototype.ToString])
  else
    Logger.Debug('  APrototype is nil');

  FPrototype := APrototype;
  Logger.Debug('  Set instance FPrototype');

  // Also set the inherited prototype field so that inherited GetProperty works correctly
  inherited Prototype := APrototype;
  Logger.Debug('  Set inherited Prototype');

  // Debug: Check what the inherited prototype actually is now
  if Assigned(inherited Prototype) then
    Logger.Debug('  Inherited Prototype is now: %s', [inherited Prototype.ToString])
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
begin
  // Private fields can only be accessed by the class that declares them
  // (not by subclasses unless they redeclare the same private field)
  if AAccessClass <> FClass then
    raise TGocciaError.Create(Format('Private field "%s" is not accessible', [AName]), 0, 0, '', nil);

  if FPrivateProperties.TryGetValue(AName, Result) then
    Exit
  else
    Result := TGocciaUndefinedValue.Create;
end;

procedure TGocciaInstanceValue.SetPrivateProperty(const AName: string; AValue: TGocciaValue; AAccessClass: TGocciaClassValue);
begin
  // Private fields can only be accessed by the class that declares them
  if AAccessClass <> FClass then
    raise TGocciaError.Create(Format('Private field "%s" is not accessible', [AName]), 0, 0, '', nil);

  FPrivateProperties.AddOrSetValue(AName, AValue);
end;

destructor TGocciaInstanceValue.Destroy;
begin
  FPrivateProperties.Free;
  inherited;
end;

end.
