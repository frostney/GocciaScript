unit Goccia.Values.ClassValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Values.FunctionValue, Goccia.Values.ObjectValue, Goccia.Interfaces,
  Goccia.Error, Goccia.Logger, Generics.Collections, SysUtils, Math, Goccia.Values.UndefinedValue;

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
  public
    constructor Create(const AName: string; ASuperClass: TGocciaClassValue);
    destructor Destroy; override;
    function ToString: string; override;
    function ToBoolean: Boolean; override;
    function ToNumber: Double; override;
    function TypeName: string; override;
    procedure AddMethod(const AName: string; AMethod: TGocciaMethodValue);
    function GetMethod(const AName: string): TGocciaMethodValue;
    function Instantiate(Arguments: TObjectList<TGocciaValue>): TGocciaValue;
    property Name: string read FName;
    property SuperClass: TGocciaClassValue read FSuperClass;
    property Prototype: TGocciaObjectValue read FPrototype;
    property ConstructorMethod: TGocciaMethodValue read FConstructorMethod;
  end;

  TGocciaInstanceValue = class(TGocciaObjectValue)
  private
    FClass: TGocciaClassValue;
    FPrototype: TGocciaObjectValue;
    procedure SetPrototype(APrototype: TGocciaObjectValue);
  public
    constructor Create(AClass: TGocciaClassValue);
    function TypeName: string; override;
    function GetProperty(const AName: string): TGocciaValue;
    procedure SetProperty(const AName: string; AValue: TGocciaValue);
    property ClassValue: TGocciaClassValue read FClass;
    property Prototype: TGocciaObjectValue read FPrototype write SetPrototype;
  end;

implementation

constructor TGocciaClassValue.Create(const AName: string; ASuperClass: TGocciaClassValue);
begin
  FName := AName;
  FSuperClass := ASuperClass;
  FMethods := TDictionary<string, TGocciaMethodValue>.Create;
  FPrototype := TGocciaObjectValue.Create;
  FConstructorMethod := nil;
  if Assigned(FSuperClass) then
    FPrototype.Prototype := FSuperClass.Prototype;
end;

destructor TGocciaClassValue.Destroy;
begin
  FMethods.Free;
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

constructor TGocciaInstanceValue.Create(AClass: TGocciaClassValue);
begin
  inherited Create;
  FClass := AClass;
  FPrototype := nil;
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

end.
