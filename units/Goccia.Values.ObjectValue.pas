unit Goccia.Values.ObjectValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Generics.Collections, Goccia.Values.UndefinedValue, Math, Goccia.Logger, SysUtils;

type
  // TGocciaProperty = class
  // private
  //   FName: string;
  //   FValue: TGocciaValue;
  //   FWritable: Boolean;
  //   FEnumerable: Boolean;
  //   FConfigurable: Boolean;
  // public
  //   constructor Create(const AName: string; AValue: TGocciaValue; AWritable: Boolean = True; AEnumerable: Boolean = True; AConfigurable: Boolean = True);

  //   property Name: string read FName;
  //   property Value: TGocciaValue read FValue;
  //   property Writable: Boolean read FWritable;
  //   property Enumerable: Boolean read FEnumerable;
  //   property Configurable: Boolean read FConfigurable;
  // end;

  TGocciaObjectValue = class;

  TComputedPropertyFunction = function(const AObject: TGocciaObjectValue): TGocciaValue of object;

  // Property descriptor for getter/setter support
  TGocciaPropertyDescriptor = class
  private
    FHasValue: Boolean;
    FValue: TGocciaValue;
    FHasGetter: Boolean;
    FGetter: TGocciaValue; // Function value
    FHasSetter: Boolean;
    FSetter: TGocciaValue; // Function value
    FEnumerable: Boolean;
    FConfigurable: Boolean;
  public
    constructor Create;
    property HasValue: Boolean read FHasValue write FHasValue;
    property Value: TGocciaValue read FValue write FValue;
    property HasGetter: Boolean read FHasGetter write FHasGetter;
    property Getter: TGocciaValue read FGetter write FGetter;
    property HasSetter: Boolean read FHasSetter write FHasSetter;
    property Setter: TGocciaValue read FSetter write FSetter;
    property Enumerable: Boolean read FEnumerable write FEnumerable;
    property Configurable: Boolean read FConfigurable write FConfigurable;
  end;

  TGocciaObjectValue = class(TGocciaValue)
  protected
    FProperties: TDictionary<string, TGocciaValue>;
    FComputedProperties: TDictionary<string, TComputedPropertyFunction>;
    FPropertyDescriptors: TDictionary<string, TGocciaPropertyDescriptor>;
    FPrototype: TGocciaObjectValue;
  public
    constructor Create;
    destructor Destroy; override;
    function ToDebugString: string;
    function ToString: string; override;
    function ToBoolean: Boolean; override;
    function ToNumber: Double; override;
    function TypeName: string; override;
    procedure SetProperty(const AName: string; AValue: TGocciaValue);
    procedure SetComputedProperty(const AName: string; AValue: TComputedPropertyFunction);
    procedure SetPropertyDescriptor(const AName: string; ADescriptor: TGocciaPropertyDescriptor);
    procedure SetGetter(const AName: string; AGetterFunction: TGocciaValue);
    procedure SetSetter(const AName: string; ASetterFunction: TGocciaValue);
    function GetProperty(const AName: string): TGocciaValue;
    function GetPropertyWithContext(const AName: string; AThisContext: TGocciaValue): TGocciaValue;
    function HasProperty(const AName: string): Boolean;
    function HasOwnProperty(const AName: string): Boolean;
    procedure DeleteProperty(const AName: string);

    property Properties: TDictionary<string, TGocciaValue> read FProperties;
    property Prototype: TGocciaObjectValue read FPrototype write FPrototype;
  end;


implementation

uses Goccia.Values.FunctionValue;

{ TGocciaPropertyDescriptor }

constructor TGocciaPropertyDescriptor.Create;
begin
  FHasValue := False;
  FValue := nil;
  FHasGetter := False;
  FGetter := nil;
  FHasSetter := False;
  FSetter := nil;
  FEnumerable := True;
  FConfigurable := True;
end;

{ TGocciaObjectValue }

// TODO: Should we allow the prototype to be set in the constructor?
constructor TGocciaObjectValue.Create;
begin
  FProperties := TDictionary<string, TGocciaValue>.Create;
  FComputedProperties := TDictionary<string, TComputedPropertyFunction>.Create;
  FPropertyDescriptors := TDictionary<string, TGocciaPropertyDescriptor>.Create;

  // TODO: Should this be TGocciaNullValue?
  // TODO: Should we set a default prototype?
  FPrototype := nil;
end;

destructor TGocciaObjectValue.Destroy;
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  // Don't free the values - they might be referenced elsewhere
  // The scope or other owners should handle their cleanup
  FProperties.Free;
  FComputedProperties.Free;

  // Free property descriptors
  for Descriptor in FPropertyDescriptors.Values do
    Descriptor.Free;
  FPropertyDescriptors.Free;

  inherited;
end;

function TGocciaObjectValue.ToDebugString: string;
var
  Pair: TPair<string, TGocciaValue>;
  First: Boolean;
begin
  Result := '{';
  First := True;
  for Pair in FProperties do
  begin
    if not First then
      Result := Result + ', ';

    if Pair.Value is TGocciaObjectValue then
      Result := Result + Pair.Key + ': ' + TGocciaObjectValue(Pair.Value).ToDebugString
    else
      Result := Result + Pair.Key + ': ' + Pair.Value.ToString;

    First := False;
  end;

  if Assigned(FPrototype) then
  begin
    if not First then
      Result := Result + ', ';

    Result := Result + '[[Prototype]]: ' + FPrototype.ToDebugString;
    First := False;
  end;

  Result := Result + '}';
end;

function TGocciaObjectValue.ToString: string;
begin
  Result := '[object Object]';
end;

function TGocciaObjectValue.ToBoolean: Boolean;
begin
  Result := True;
end;

function TGocciaObjectValue.ToNumber: Double;
begin
  Result := NaN;
end;

function TGocciaObjectValue.TypeName: string;
begin
  Result := 'object';
end;

procedure TGocciaObjectValue.SetProperty(const AName: string; AValue: TGocciaValue);
var
  Descriptor: TGocciaPropertyDescriptor;
  SetterFunction: TGocciaFunctionValue;
  Args: TObjectList<TGocciaValue>;
begin
  // First check for property descriptors (getters/setters)
  if FPropertyDescriptors.ContainsKey(AName) then
  begin
    Descriptor := FPropertyDescriptors[AName];
    if Descriptor.HasSetter then
    begin
      // Call the setter function
      if Descriptor.Setter is TGocciaFunctionValue then
      begin
        SetterFunction := TGocciaFunctionValue(Descriptor.Setter);
        Args := TObjectList<TGocciaValue>.Create(False);
        try
          Args.Add(AValue);
          SetterFunction.Call(Args, Self);
        finally
          Args.Free;
        end;
        Exit;
      end;
    end
    else if Descriptor.HasValue then
    begin
      // Property has a value descriptor, update it
      Descriptor.Value := AValue;
      Exit;
    end;
  end;

  // Default behavior: set as regular property
  FProperties.AddOrSetValue(AName, AValue);
end;

procedure TGocciaObjectValue.SetComputedProperty(const AName: string; AValue: TComputedPropertyFunction);
begin
  FComputedProperties.AddOrSetValue(AName, AValue);
end;

procedure TGocciaObjectValue.SetPropertyDescriptor(const AName: string; ADescriptor: TGocciaPropertyDescriptor);
begin
  FPropertyDescriptors.AddOrSetValue(AName, ADescriptor);
end;

procedure TGocciaObjectValue.SetGetter(const AName: string; AGetterFunction: TGocciaValue);
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  if FPropertyDescriptors.ContainsKey(AName) then
    Descriptor := FPropertyDescriptors[AName]
  else
  begin
    Descriptor := TGocciaPropertyDescriptor.Create;
    FPropertyDescriptors.Add(AName, Descriptor);
  end;

  Descriptor.HasGetter := True;
  Descriptor.Getter := AGetterFunction;
end;

procedure TGocciaObjectValue.SetSetter(const AName: string; ASetterFunction: TGocciaValue);
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  if FPropertyDescriptors.ContainsKey(AName) then
    Descriptor := FPropertyDescriptors[AName]
  else
  begin
    Descriptor := TGocciaPropertyDescriptor.Create;
    FPropertyDescriptors.Add(AName, Descriptor);
  end;

  Descriptor.HasSetter := True;
  Descriptor.Setter := ASetterFunction;
end;

function TGocciaObjectValue.GetProperty(const AName: string): TGocciaValue;
var
  Descriptor: TGocciaPropertyDescriptor;
  GetterFunction: TGocciaFunctionValue;
  Args: TObjectList<TGocciaValue>;
begin
  Logger.Debug('TGocciaObjectValue.GetProperty: Start');
  Logger.Debug('  Name: %s', [AName]);

  // First check for property descriptors (getters/setters)
  if FPropertyDescriptors.ContainsKey(AName) then
  begin
    Descriptor := FPropertyDescriptors[AName];
    if Descriptor.HasGetter then
    begin
      // Call the getter function
      if Descriptor.Getter is TGocciaFunctionValue then
      begin
        GetterFunction := TGocciaFunctionValue(Descriptor.Getter);
        Args := TObjectList<TGocciaValue>.Create(False);
        try
          Result := GetterFunction.Call(Args, Self);
        finally
          Args.Free;
        end;
        Exit;
      end;
    end
    else if Descriptor.HasValue then
    begin
      Result := Descriptor.Value;
      Exit;
    end;
  end;

  // Then check computed properties (legacy)
  if FComputedProperties.ContainsKey(AName) then
  begin
    Logger.Debug('TGocciaObjectValue.GetProperty: FComputedProperties.ContainsKey(AName)');
    Result := FComputedProperties[AName](Self);
    Exit;
  end;

  // Then check regular properties
  if FProperties.ContainsKey(AName) then
  begin
    Result := FProperties[AName];
    Exit;
  end;

  // Finally check prototype chain
  if Assigned(FPrototype) then
  begin
    Logger.Debug('TGocciaObjectValue.GetProperty: FPrototype is assigned');
    Result := FPrototype.GetProperty(AName);
    Exit;
  end;

  Logger.Debug('TGocciaObjectValue.GetProperty: Property not found');
  Result := TGocciaUndefinedValue.Create;
end;

function TGocciaObjectValue.GetPropertyWithContext(const AName: string; AThisContext: TGocciaValue): TGocciaValue;
var
  Descriptor: TGocciaPropertyDescriptor;
  GetterFunction: TGocciaFunctionValue;
  Args: TObjectList<TGocciaValue>;
begin
      Logger.Debug('TGocciaObjectValue.GetPropertyWithContext: Property=%s, ThisContext=%s', [AName, AThisContext.TypeName]);

  // First check for property descriptors (getters/setters) with custom this context
  if FPropertyDescriptors.ContainsKey(AName) then
  begin
    Logger.Debug('TGocciaObjectValue.GetPropertyWithContext: Found property descriptor for %s', [AName]);
    Descriptor := FPropertyDescriptors[AName];
    if Descriptor.HasGetter then
    begin
      Logger.Debug('TGocciaObjectValue.GetPropertyWithContext: Property has getter');
      // Call the getter function with the provided this context
      if Descriptor.Getter is TGocciaFunctionValue then
      begin
        Logger.Debug('TGocciaObjectValue.GetPropertyWithContext: Calling getter with context %s', [AThisContext.TypeName]);
        GetterFunction := TGocciaFunctionValue(Descriptor.Getter);
        Args := TObjectList<TGocciaValue>.Create(False);
        try
          Result := GetterFunction.Call(Args, AThisContext); // Use provided context
          Logger.Debug('TGocciaObjectValue.GetPropertyWithContext: Getter returned %s', [Result.ToString]);
        finally
          Args.Free;
        end;
        Exit;
      end;
    end
    else if Descriptor.HasValue then
    begin
      Result := Descriptor.Value;
      Exit;
    end;
  end;

  // Fall back to regular GetProperty for other cases
  Logger.Debug('TGocciaObjectValue.GetPropertyWithContext: No descriptor found, falling back to GetProperty');
  Result := GetProperty(AName);
end;

function TGocciaObjectValue.HasProperty(const AName: string): Boolean;
begin
  Result := HasOwnProperty(AName);

  if not Result and Assigned(FPrototype) then
    Result := FPrototype.HasProperty(AName);
end;

function TGocciaObjectValue.HasOwnProperty(const AName: string): Boolean;
begin
  Result := FProperties.ContainsKey(AName);
end;

procedure TGocciaObjectValue.DeleteProperty(const AName: string);
begin
  if FProperties.ContainsKey(AName) then
    FProperties.Remove(AName);

  if FComputedProperties.ContainsKey(AName) then
    FComputedProperties.Remove(AName);
end;

end.
