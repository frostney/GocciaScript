unit Goccia.Values.ClassValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Values.FunctionValue, Goccia.Values.ObjectValue, Goccia.Interfaces,
  Goccia.Error, Goccia.Logger, Generics.Collections, SysUtils, Math;

type
  // Forward declaration
  TGocciaInstanceValue = class;

  TGocciaClassValue = class(TGocciaValue)
  private
    FName: string;
    FSuperClass: TGocciaClassValue;
    FMethods: TDictionary<string, TGocciaMethodValue>;
    FPrototype: TGocciaObjectValue;
  public
    constructor Create(const AName: string; ASuperClass: TGocciaClassValue);
    destructor Destroy; override;
    function ToString: string; override;
    function ToNumber: Double; override;
    function TypeName: string; override;
    procedure AddMethod(const AName: string; AMethod: TGocciaMethodValue);
    function GetMethod(const AName: string): TGocciaMethodValue;
    function Instantiate(Arguments: TObjectList<TGocciaValue>; Interpreter: IGocciaInterpreter): TGocciaInstanceValue;
    property Name: string read FName;
    property SuperClass: TGocciaClassValue read FSuperClass;
    property Prototype: TGocciaObjectValue read FPrototype;
  end;

  TGocciaInstanceValue = class(TGocciaObjectValue)
  private
    FClass: TGocciaClassValue;
  public
    constructor Create(AClass: TGocciaClassValue);
    function TypeName: string; override;
    function GetProperty(const AName: string): TGocciaValue;
    property ClassValue: TGocciaClassValue read FClass;
  end;

implementation

constructor TGocciaClassValue.Create(const AName: string; ASuperClass: TGocciaClassValue);
begin
  FName := AName;
  FSuperClass := ASuperClass;
  FMethods := TDictionary<string, TGocciaMethodValue>.Create;
  FPrototype := TGocciaObjectValue.Create;
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

function TGocciaClassValue.Instantiate(Arguments: TObjectList<TGocciaValue>; Interpreter: IGocciaInterpreter): TGocciaInstanceValue;
var
  Instance: TGocciaInstanceValue;
  Method: TGocciaMethodValue;
  ConstructorResult: TGocciaValue;
begin
  // Create the instance with this class as its class value
  Instance := TGocciaInstanceValue.Create(Self);
  TGocciaLogger.Debug('Instance created: %s', [Instance.ToString]);

  // Set up the prototype chain
  // The instance's prototype should be the class's prototype
  Instance.Prototype := FPrototype;
  TGocciaLogger.Debug('Prototype set for instance');

  // Call constructor if it exists
  Method := GetMethod('constructor');
  TGocciaLogger.Debug('Constructor method found: %s', [Assigned(Method)]);

  if Assigned(Method) then
  begin
    TGocciaLogger.Debug('Calling constructor');
    TGocciaLogger.Debug('Arguments: %d', [Arguments.Count]);
    if Arguments.Count > 0 then
      TGocciaLogger.Debug('Arguments: %s', [Arguments[0].ToString]);
    TGocciaLogger.Debug('Instance: %s', [Instance.ToString]);

    // Call the constructor with the instance as this value
    ConstructorResult := Method.Call(Arguments, Instance, Interpreter);
    TGocciaLogger.Debug('Constructor call completed, result: %s', [ConstructorResult.ToString]);
  end;

  Result := Instance;
  TGocciaLogger.Debug('Returning instance: %s', [Result.ToString]);
end;

constructor TGocciaInstanceValue.Create(AClass: TGocciaClassValue);
begin
  inherited Create;
  FClass := AClass;
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
  if Result = nil then
  begin
    // If not found in instance, check if it's a method
    Method := FClass.GetMethod(AName);
    if Assigned(Method) then
      Result := Method;
  end;
end;

end.
