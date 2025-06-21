unit Goccia.Values.ObjectValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Generics.Collections, Goccia.Values.UndefinedValue, Goccia.Values.ObjectPropertyDescriptor, Math, Goccia.Logger, SysUtils;

type
  TGocciaObjectValue = class(TGocciaValue)
  protected
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

    procedure DefineProperty(const AName: string; ADescriptor: TGocciaPropertyDescriptor);
    procedure DefineProperties(const AProperties: TDictionary<string, TGocciaPropertyDescriptor>);

    procedure SetProperty(const AName: string; AValue: TGocciaValue);

    // Convenience methods for built-in objects
    procedure RegisterMethod(AMethod: TGocciaValue);
    procedure RegisterConstant(const AName: string; const AValue: TGocciaValue);

    function GetProperty(const AName: string): TGocciaValue;
    function GetPropertyWithContext(const AName: string; AThisContext: TGocciaValue): TGocciaValue;
    function HasProperty(const AName: string): Boolean;
    function HasOwnProperty(const AName: string): Boolean;
    procedure DeleteProperty(const AName: string);

    // Property enumeration methods
    function GetEnumerablePropertyNames: TArray<string>;
    function GetEnumerablePropertyValues: TArray<TGocciaValue>;
    function GetEnumerablePropertyEntries: TArray<TPair<string, TGocciaValue>>;
    function GetAllPropertyNames: TArray<string>;

    property Prototype: TGocciaObjectValue read FPrototype write FPrototype;
  end;


implementation

uses Goccia.Values.FunctionValue, Goccia.Values.NativeFunction;

{ TGocciaObjectValue }

// TODO: Should we allow the prototype to be set in the constructor?
constructor TGocciaObjectValue.Create;
begin
  FPropertyDescriptors := TDictionary<string, TGocciaPropertyDescriptor>.Create;

  // TODO: Should this be TGocciaNullValue?
  // TODO: Should we set a default prototype?
  FPrototype := nil;
end;

destructor TGocciaObjectValue.Destroy;
begin
  // Don't free the values - they might be referenced elsewhere
  // The scope or other owners should handle their cleanup

  // Property descriptors are now records - no manual cleanup needed
  FPropertyDescriptors.Free;

  inherited;
end;

function TGocciaObjectValue.ToDebugString: string;
var
  Pair: TPair<string, TGocciaPropertyDescriptor>;
  First: Boolean;
  Value: TGocciaValue;
begin
  Result := '{';
  First := True;

  // Iterate through property descriptors
  for Pair in FPropertyDescriptors do
  begin
    if not First then
      Result := Result + ', ';

    // Get the value from the descriptor
    if Pair.Value is TGocciaPropertyDescriptorData then
      Value := TGocciaPropertyDescriptorData(Pair.Value).Value
    else if Pair.Value is TGocciaPropertyDescriptorAccessor then
      Value := nil // Accessor descriptors don't have direct values
    else
      Value := nil;

    if Assigned(Value) then
    begin
      if Value is TGocciaObjectValue then
        Result := Result + Pair.Key + ': ' + TGocciaObjectValue(Value).ToDebugString
      else
        Result := Result + Pair.Key + ': ' + Value.ToString;
    end
    else
      Result := Result + Pair.Key + ': [accessor]';

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
  NativeSetterFunction: TGocciaNativeFunctionValue;
  Args: TObjectList<TGocciaValue>;
begin
  // First check for existing property descriptors
  if FPropertyDescriptors.ContainsKey(AName) then
  begin
    Descriptor := FPropertyDescriptors[AName];
    if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      // Call the setter function if it exists
      if Assigned(TGocciaPropertyDescriptorAccessor(Descriptor).Setter) then
      begin
        if TGocciaPropertyDescriptorAccessor(Descriptor).Setter is TGocciaFunctionValue then
        begin
          SetterFunction := TGocciaFunctionValue(TGocciaPropertyDescriptorAccessor(Descriptor).Setter);
          Args := TObjectList<TGocciaValue>.Create(False);
          try
            Args.Add(AValue);
            SetterFunction.Call(Args, Self);
          finally
            Args.Free;
          end;
          Exit;
        end
        else if TGocciaPropertyDescriptorAccessor(Descriptor).Setter is TGocciaNativeFunctionValue then
        begin
          NativeSetterFunction := TGocciaNativeFunctionValue(TGocciaPropertyDescriptorAccessor(Descriptor).Setter);
          Args := TObjectList<TGocciaValue>.Create(False);
          try
            Args.Add(AValue);
            NativeSetterFunction.Call(Args, Self);
          finally
            Args.Free;
          end;
          Exit;
        end;
      end;
      // Accessor descriptor without setter - property is not writable
      Exit;
    end
    else if Descriptor is TGocciaPropertyDescriptorData then
    begin
      // Check if property is writable
      if TGocciaPropertyDescriptorData(Descriptor).Writable then
      begin
        // Property has a data descriptor, update it
        Descriptor := TGocciaPropertyDescriptorData.Create(AValue, [pfEnumerable, pfConfigurable, pfWritable]);
        FPropertyDescriptors[AName] := Descriptor;
        Exit;
      end;
      // Property is not writable - silently ignore in non-strict mode
      Exit;
    end;
  end;

  // Create new property with proper descriptor (JavaScript default attributes)
  Descriptor := TGocciaPropertyDescriptorData.Create(AValue, [pfEnumerable, pfConfigurable, pfWritable]);
  FPropertyDescriptors.AddOrSetValue(AName, Descriptor);
end;

procedure TGocciaObjectValue.DefineProperty(const AName: string; ADescriptor: TGocciaPropertyDescriptor);
begin
  if FPropertyDescriptors.ContainsKey(AName) then
  begin
    if FPropertyDescriptors[AName].Configurable then
    begin
      FPropertyDescriptors[AName] := ADescriptor;
      Exit;
    end;

    if FPropertyDescriptors[AName].Writable then
    begin
      FPropertyDescriptors[AName] := ADescriptor;
      Exit;
    end;
  end;

  FPropertyDescriptors.AddOrSetValue(AName, ADescriptor);
end;

procedure TGocciaObjectValue.DefineProperties(const AProperties: TDictionary<string, TGocciaPropertyDescriptor>);
var
  Pair: TPair<string, TGocciaPropertyDescriptor>;
begin
  for Pair in AProperties do
    DefineProperty(Pair.Key, Pair.Value);
end;

procedure TGocciaObjectValue.RegisterMethod(AMethod: TGocciaValue);
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  if not (AMethod is TGocciaFunctionValue) and not (AMethod is TGocciaNativeFunctionValue) then
    raise Exception.Create('Method must be a function or native function');

  // Built-in methods: { writable: true, enumerable: false, configurable: true }
  Descriptor := TGocciaPropertyDescriptorData.Create(AMethod, [pfConfigurable, pfWritable]);

  if AMethod is TGocciaFunctionValue then
  begin
    FPropertyDescriptors.AddOrSetValue(TGocciaFunctionValue(AMethod).Name, Descriptor);
  end
  else if AMethod is TGocciaNativeFunctionValue then
  begin
    FPropertyDescriptors.AddOrSetValue(TGocciaNativeFunctionValue(AMethod).Name, Descriptor);
  end;
end;

procedure TGocciaObjectValue.RegisterConstant(const AName: string; const AValue: TGocciaValue);
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  // Built-in constants: { writable: false, enumerable: false, configurable: false }
  Descriptor := TGocciaPropertyDescriptorData.Create(AValue, []);
  FPropertyDescriptors.AddOrSetValue(AName, Descriptor);
end;

function TGocciaObjectValue.GetProperty(const AName: string): TGocciaValue;
var
  Descriptor: TGocciaPropertyDescriptor;
  GetterFunction: TGocciaFunctionValue;
  NativeGetterFunction: TGocciaNativeFunctionValue;
  Args: TObjectList<TGocciaValue>;
begin
  Logger.Debug('TGocciaObjectValue.GetProperty: Start');
  Logger.Debug('  Name: %s', [AName]);

  // First priority: property descriptors (modern system)
  if FPropertyDescriptors.ContainsKey(AName) then
  begin
    Descriptor := FPropertyDescriptors[AName];
    if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      // Call the getter function if it exists
      if Assigned(TGocciaPropertyDescriptorAccessor(Descriptor).Getter) then
      begin
        if TGocciaPropertyDescriptorAccessor(Descriptor).Getter is TGocciaFunctionValue then
        begin
          GetterFunction := TGocciaFunctionValue(TGocciaPropertyDescriptorAccessor(Descriptor).Getter);
          Args := TObjectList<TGocciaValue>.Create(False);
          try
            Result := GetterFunction.Call(Args, Self);
          finally
            Args.Free;
          end;
          Exit;
        end
        else if TGocciaPropertyDescriptorAccessor(Descriptor).Getter is TGocciaNativeFunctionValue then
        begin
          NativeGetterFunction := TGocciaNativeFunctionValue(TGocciaPropertyDescriptorAccessor(Descriptor).Getter);
          Args := TObjectList<TGocciaValue>.Create(False);
          try
            Result := NativeGetterFunction.Call(Args, Self);
          finally
            Args.Free;
          end;
          Exit;
        end;
      end;
      // No getter - return undefined
      Result := TGocciaUndefinedValue.Create;
      Exit;
    end
    else if Descriptor is TGocciaPropertyDescriptorData then
    begin
      Result := TGocciaPropertyDescriptorData(Descriptor).Value;
      Exit;
    end;
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
  NativeGetterFunction: TGocciaNativeFunctionValue;
  Args: TObjectList<TGocciaValue>;
begin
      Logger.Debug('TGocciaObjectValue.GetPropertyWithContext: Property=%s, ThisContext=%s', [AName, AThisContext.TypeName]);

  // First check for property descriptors (getters/setters) with custom this context
  if FPropertyDescriptors.ContainsKey(AName) then
  begin
    Logger.Debug('TGocciaObjectValue.GetPropertyWithContext: Found property descriptor for %s', [AName]);
    Descriptor := FPropertyDescriptors[AName];
    if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      Logger.Debug('TGocciaObjectValue.GetPropertyWithContext: Property has accessor descriptor');
      // Call the getter function with the provided this context if it exists
      if Assigned(TGocciaPropertyDescriptorAccessor(Descriptor).Getter) then
      begin
        if TGocciaPropertyDescriptorAccessor(Descriptor).Getter is TGocciaFunctionValue then
        begin
          Logger.Debug('TGocciaObjectValue.GetPropertyWithContext: Calling getter with context %s', [AThisContext.TypeName]);
          GetterFunction := TGocciaFunctionValue(TGocciaPropertyDescriptorAccessor(Descriptor).Getter);
          Args := TObjectList<TGocciaValue>.Create(False);
          try
            Result := GetterFunction.Call(Args, AThisContext); // Use provided context
            Logger.Debug('TGocciaObjectValue.GetPropertyWithContext: Getter returned %s', [Result.ToString]);
          finally
            Args.Free;
          end;
          Exit;
        end
        else if TGocciaPropertyDescriptorAccessor(Descriptor).Getter is TGocciaNativeFunctionValue then
        begin
          Logger.Debug('TGocciaObjectValue.GetPropertyWithContext: Calling native getter with context %s', [AThisContext.TypeName]);
          NativeGetterFunction := TGocciaNativeFunctionValue(TGocciaPropertyDescriptorAccessor(Descriptor).Getter);
          Args := TObjectList<TGocciaValue>.Create(False);
          try
            Result := NativeGetterFunction.Call(Args, AThisContext); // Use provided context
            Logger.Debug('TGocciaObjectValue.GetPropertyWithContext: Native getter returned %s', [Result.ToString]);
          finally
            Args.Free;
          end;
          Exit;
        end;
      end;
      // No getter - return undefined
      Result := TGocciaUndefinedValue.Create;
      Exit;
    end
    else if Descriptor is TGocciaPropertyDescriptorData then
    begin
      Result := TGocciaPropertyDescriptorData(Descriptor).Value;
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
  Result := FPropertyDescriptors.ContainsKey(AName);
end;

procedure TGocciaObjectValue.DeleteProperty(const AName: string);
begin
  if FPropertyDescriptors.ContainsKey(AName) then
    FPropertyDescriptors.Remove(AName);
end;

function TGocciaObjectValue.GetEnumerablePropertyNames: TArray<string>;
var
  Names: TArray<string>;
  Count: Integer;
  Pair: TPair<string, TGocciaPropertyDescriptor>;
begin
  SetLength(Names, FPropertyDescriptors.Count);
  Count := 0;

  for Pair in FPropertyDescriptors do
  begin
    if Pair.Value.Enumerable then
    begin
      Names[Count] := Pair.Key;
      Inc(Count);
    end;
  end;

  SetLength(Names, Count);
  Result := Names;
end;

function TGocciaObjectValue.GetEnumerablePropertyValues: TArray<TGocciaValue>;
var
  Values: TArray<TGocciaValue>;
  Count: Integer;
  Pair: TPair<string, TGocciaPropertyDescriptor>;
begin
  SetLength(Values, FPropertyDescriptors.Count);
  Count := 0;

  for Pair in FPropertyDescriptors do
  begin
    if Pair.Value.Enumerable and (Pair.Value is TGocciaPropertyDescriptorData) then
    begin
      Values[Count] := TGocciaPropertyDescriptorData(Pair.Value).Value;
      Inc(Count);
    end;
  end;

  SetLength(Values, Count);
  Result := Values;
end;

function TGocciaObjectValue.GetEnumerablePropertyEntries: TArray<TPair<string, TGocciaValue>>;
var
  Entries: TArray<TPair<string, TGocciaValue>>;
  Count: Integer;
  Pair: TPair<string, TGocciaPropertyDescriptor>;
  Entry: TPair<string, TGocciaValue>;
begin
  SetLength(Entries, FPropertyDescriptors.Count);
  Count := 0;

  for Pair in FPropertyDescriptors do
  begin
    if Pair.Value.Enumerable and (Pair.Value is TGocciaPropertyDescriptorData) then
    begin
      Entry.Key := Pair.Key;
      Entry.Value := TGocciaPropertyDescriptorData(Pair.Value).Value;
      Entries[Count] := Entry;
      Inc(Count);
    end;
  end;

  SetLength(Entries, Count);
  Result := Entries;
end;

function TGocciaObjectValue.GetAllPropertyNames: TArray<string>;
var
  Names: TArray<string>;
  Count: Integer;
  Pair: TPair<string, TGocciaPropertyDescriptor>;
begin
  SetLength(Names, FPropertyDescriptors.Count);
  Count := 0;

  for Pair in FPropertyDescriptors do
  begin
    Names[Count] := Pair.Key;
    Inc(Count);
  end;

  SetLength(Names, Count);
  Result := Names;
end;

end.
