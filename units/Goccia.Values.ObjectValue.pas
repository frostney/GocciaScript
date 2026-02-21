unit Goccia.Values.ObjectValue;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue;

type

  TGocciaObjectValue = class(TGocciaValue)
  protected
    FPropertyDescriptors: TDictionary<string, TGocciaPropertyDescriptor>;
    FPropertyInsertionOrder: TStringList;
    FSymbolDescriptors: TDictionary<TGocciaSymbolValue, TGocciaPropertyDescriptor>;
    FSymbolInsertionOrder: TList<TGocciaSymbolValue>;
    FPrototype: TGocciaObjectValue;
    FFrozen: Boolean;
    FSealed: Boolean;
    FExtensible: Boolean;
  public
    constructor Create(const APrototype: TGocciaObjectValue = nil);
    destructor Destroy; override;
    function ToDebugString: string;
    function TypeName: string; override;
    function TypeOf: string; override;
    function ValueOf: TGocciaValue;
    function ToStringTag: string; virtual;

    function ToStringLiteral: TGocciaStringLiteralValue; override;
    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;

    procedure DefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor);
    procedure DefineProperties(const AProperties: TDictionary<string, TGocciaPropertyDescriptor>);

    procedure AssignProperty(const AName: string; const AValue: TGocciaValue; const ACanCreate: Boolean = True); virtual;

    // Convenience methods for built-in objects
    procedure RegisterNativeMethod(const AMethod: TGocciaValue);
    procedure RegisterConstant(const AName: string; const AValue: TGocciaValue);

    // Property accessors
    function GetProperty(const AName: string): TGocciaValue; override;
    procedure SetProperty(const AName: string; const AValue: TGocciaValue); override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
    function GetOwnPropertyDescriptor(const AName: string): TGocciaPropertyDescriptor;
    function HasProperty(const AName: string): Boolean;
    function HasOwnProperty(const AName: string): Boolean;
    function DeleteProperty(const AName: string): Boolean;

    // Property enumeration methods
    function GetEnumerablePropertyNames: TArray<string>;
    function GetEnumerablePropertyValues: TArray<TGocciaValue>;
    function GetEnumerablePropertyEntries: TArray<TPair<string, TGocciaValue>>;
    function GetAllPropertyNames: TArray<string>;
    function GetOwnPropertyNames: TStringList;
    function GetOwnPropertyKeys: TStringList;

    // Symbol property methods
    procedure DefineSymbolProperty(const ASymbol: TGocciaSymbolValue; const ADescriptor: TGocciaPropertyDescriptor);
    procedure AssignSymbolProperty(const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue);
    function GetSymbolProperty(const ASymbol: TGocciaSymbolValue): TGocciaValue;
    function GetOwnSymbolPropertyDescriptor(const ASymbol: TGocciaSymbolValue): TGocciaPropertyDescriptor;
    function HasSymbolProperty(const ASymbol: TGocciaSymbolValue): Boolean;
    function GetEnumerableSymbolProperties: TArray<TPair<TGocciaSymbolValue, TGocciaValue>>;
    function GetOwnSymbols: TArray<TGocciaSymbolValue>;

    // Freeze/seal/extensibility support
    procedure Freeze;
    function IsFrozen: Boolean;
    procedure Seal;
    function IsSealed: Boolean;
    procedure PreventExtensions;
    function IsExtensible: Boolean;

    procedure MarkReferences; override;

    property Prototype: TGocciaObjectValue read FPrototype write FPrototype;
    property Frozen: Boolean read FFrozen;
    property Sealed: Boolean read FSealed;
    property Extensible: Boolean read FExtensible;
  end;


implementation

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Values.ClassHelper,
  Goccia.Values.ConstructorNames,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionValue,
  Goccia.Values.NativeFunction;

procedure MarkPropertyDescriptor(const ADescriptor: TGocciaPropertyDescriptor);
begin
  if ADescriptor is TGocciaPropertyDescriptorData then
  begin
    if Assigned(TGocciaPropertyDescriptorData(ADescriptor).Value) then
      TGocciaPropertyDescriptorData(ADescriptor).Value.MarkReferences;
  end
  else if ADescriptor is TGocciaPropertyDescriptorAccessor then
  begin
    if Assigned(TGocciaPropertyDescriptorAccessor(ADescriptor).Getter) then
      TGocciaPropertyDescriptorAccessor(ADescriptor).Getter.MarkReferences;
    if Assigned(TGocciaPropertyDescriptorAccessor(ADescriptor).Setter) then
      TGocciaPropertyDescriptorAccessor(ADescriptor).Setter.MarkReferences;
  end;
end;

{ TGocciaObjectValue }

constructor TGocciaObjectValue.Create(const APrototype: TGocciaObjectValue = nil);
begin
  FPropertyDescriptors := TDictionary<string, TGocciaPropertyDescriptor>.Create;
  FPropertyInsertionOrder := TStringList.Create;
  FPropertyInsertionOrder.Duplicates := dupIgnore; // Prevent duplicates

  FSymbolDescriptors := TDictionary<TGocciaSymbolValue, TGocciaPropertyDescriptor>.Create;
  FSymbolInsertionOrder := TList<TGocciaSymbolValue>.Create;

  FPrototype := APrototype;
  FFrozen := False;
  FSealed := False;
  FExtensible := True;
end;

destructor TGocciaObjectValue.Destroy;
begin
  FPropertyDescriptors.Free;
  FPropertyInsertionOrder.Free;
  FSymbolDescriptors.Free;
  FSymbolInsertionOrder.Free;
  inherited;
end;

procedure TGocciaObjectValue.MarkReferences;
var
  Pair: TPair<string, TGocciaPropertyDescriptor>;
  SymPair: TPair<TGocciaSymbolValue, TGocciaPropertyDescriptor>;
begin
  if GCMarked then Exit;
  inherited;

  if Assigned(FPrototype) then
    FPrototype.MarkReferences;

  for Pair in FPropertyDescriptors do
    MarkPropertyDescriptor(Pair.Value);

  for SymPair in FSymbolDescriptors do
  begin
    SymPair.Key.MarkReferences;
    MarkPropertyDescriptor(SymPair.Value);
  end;
end;

function TGocciaObjectValue.ToDebugString: string;
var
  SB: TStringBuilder;
  Pair: TPair<string, TGocciaPropertyDescriptor>;
  First: Boolean;
  Value: TGocciaValue;
begin
  SB := TStringBuilder.Create;
  try
    SB.Append('{');
    First := True;

    // Iterate through property descriptors
    for Pair in FPropertyDescriptors do
    begin
      if not First then
        SB.Append(', ');

      // Get the value from the descriptor
      if Pair.Value is TGocciaPropertyDescriptorData then
        Value := TGocciaPropertyDescriptorData(Pair.Value).Value
      else if Pair.Value is TGocciaPropertyDescriptorAccessor then
        Value := nil
      else
        Value := nil;

      if Assigned(Value) then
      begin
        if Value is TGocciaObjectValue then
          SB.Append(Pair.Key).Append(': ').Append(TGocciaObjectValue(Value).ToDebugString)
        else
          SB.Append(Pair.Key).Append(': ').Append(Value.ToStringLiteral.Value);
      end
      else
        SB.Append(Pair.Key).Append(': [accessor]');

      First := False;
    end;

    if Assigned(FPrototype) then
    begin
      if not First then
        SB.Append(', ');

      SB.Append('[[Prototype]]: ').Append(FPrototype.ToDebugString);
    end;

    SB.Append('}');
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TGocciaObjectValue.TypeName: string;
begin
  Result := 'object';
end;

function TGocciaObjectValue.TypeOf: string;
begin
  Result := 'object';
end;

function TGocciaObjectValue.ValueOf: TGocciaValue;
begin
  Result := Self;
end;

function TGocciaObjectValue.ToStringTag: string;
begin
  Result := CTOR_OBJECT;
end;

function TGocciaObjectValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  Result := TGocciaStringLiteralValue.Create(Format('[%s %s]', [TypeName, ToStringTag]));
end;

function TGocciaObjectValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

function TGocciaObjectValue.ToNumberLiteral: TGocciaNumberLiteralValue;
begin
  Result := TGocciaNumberLiteralValue.NaNValue;
end;

procedure TGocciaObjectValue.AssignProperty(const AName: string; const AValue: TGocciaValue; const ACanCreate: Boolean = True);
var
  Descriptor: TGocciaPropertyDescriptor;
  SetterFunction: TGocciaFunctionValue;
  NativeSetterFunction: TGocciaNativeFunctionValue;
  Args: TGocciaArgumentsCollection;
begin
  // Frozen objects cannot be modified
  if FFrozen then
    ThrowTypeError('Cannot assign to read only property ''' + AName + ''' of frozen object');

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
          Args := TGocciaArgumentsCollection.Create;
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
          Args := TGocciaArgumentsCollection.Create;
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
      ThrowTypeError('Cannot set property ''' + AName + ''' which has only a getter');
    end
    else if Descriptor is TGocciaPropertyDescriptorData then
    begin
      // Check if property is writable
      if TGocciaPropertyDescriptorData(Descriptor).Writable then
      begin
        // Property has a data descriptor, update its value while preserving flags
        Descriptor := TGocciaPropertyDescriptorData.Create(AValue, Descriptor.Flags);
        FPropertyDescriptors[AName] := Descriptor;
        Exit;
      end;
      // Property is not writable - throw TypeError (strict mode behavior)
      ThrowTypeError('Cannot assign to read only property ''' + AName + '''');
    end;
  end;

  // Property doesn't exist - check if we can create it
  if not ACanCreate then
    ThrowTypeError('Cannot assign to non-existent property ''' + AName + '''');

  if not FExtensible then
    ThrowTypeError('Cannot add property ''' + AName + ''', object is not extensible');

  DefineProperty(AName, TGocciaPropertyDescriptorData.Create(AValue, [pfEnumerable, pfConfigurable, pfWritable]));
end;

procedure TGocciaObjectValue.DefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor);
var
  ExistingDescriptor: TGocciaPropertyDescriptor;
begin
  if FPropertyDescriptors.ContainsKey(AName) then
  begin
    ExistingDescriptor := FPropertyDescriptors[AName];

    // Check if the existing property is configurable
    if not ExistingDescriptor.Configurable then
      ThrowTypeError('Cannot redefine non-configurable property ''' + AName + '''');

    // Property is configurable - allow redefinition
    FPropertyDescriptors[AName] := ADescriptor;
    Exit; // Don't change insertion order for existing properties
  end
  else
  begin
    // New property - track insertion order
    FPropertyInsertionOrder.Add(AName);
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

function TGocciaObjectValue.GetOwnPropertyNames: TStringList;
begin
  Result := FPropertyInsertionOrder;
end;

function TGocciaObjectValue.GetOwnPropertyKeys: TStringList;
begin
  Result := FPropertyInsertionOrder;
end;



procedure TGocciaObjectValue.RegisterNativeMethod(const AMethod: TGocciaValue);
var
  Descriptor: TGocciaPropertyDescriptor;
  MethodName: string;
begin
  if not (AMethod is TGocciaNativeFunctionValue) then
    raise Exception.Create('Method must be a native function');

  // Built-in methods: { writable: true, enumerable: false, configurable: true }
  Descriptor := TGocciaPropertyDescriptorData.Create(AMethod, [pfConfigurable, pfWritable]);

  if AMethod is TGocciaNativeFunctionValue then
  begin
    MethodName := TGocciaNativeFunctionValue(AMethod).Name;
    FPropertyDescriptors.AddOrSetValue(MethodName, Descriptor);
    // Track insertion order for GetAllPropertyNames
    if FPropertyInsertionOrder.IndexOf(MethodName) = -1 then
      FPropertyInsertionOrder.Add(MethodName);
  end;
end;

procedure TGocciaObjectValue.RegisterConstant(const AName: string; const AValue: TGocciaValue);
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  // Built-in constants: { writable: false, enumerable: false, configurable: false }
  Descriptor := TGocciaPropertyDescriptorData.Create(AValue, []);
  FPropertyDescriptors.AddOrSetValue(AName, Descriptor);
  // Track insertion order for GetAllPropertyNames
  if FPropertyInsertionOrder.IndexOf(AName) = -1 then
    FPropertyInsertionOrder.Add(AName);
end;

function TGocciaObjectValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

procedure TGocciaObjectValue.SetProperty(const AName: string; const AValue: TGocciaValue);
begin
  AssignProperty(AName, AValue);
end;

function TGocciaObjectValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
var
  Descriptor: TGocciaPropertyDescriptor;
  GetterFunction: TGocciaFunctionValue;
  NativeGetterFunction: TGocciaNativeFunctionValue;
  Args: TGocciaArgumentsCollection;
begin
  // Check property descriptors (getters/setters) with the provided this context
  if FPropertyDescriptors.ContainsKey(AName) then
  begin
    Descriptor := FPropertyDescriptors[AName];
    if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      // Call the getter function with the provided this context if it exists
      if Assigned(TGocciaPropertyDescriptorAccessor(Descriptor).Getter) then
      begin
        if TGocciaPropertyDescriptorAccessor(Descriptor).Getter is TGocciaFunctionValue then
        begin
          GetterFunction := TGocciaFunctionValue(TGocciaPropertyDescriptorAccessor(Descriptor).Getter);
          Args := TGocciaArgumentsCollection.Create;
          try
            Result := GetterFunction.Call(Args, AThisContext);
          finally
            Args.Free;
          end;
          Exit;
        end
        else if TGocciaPropertyDescriptorAccessor(Descriptor).Getter is TGocciaNativeFunctionValue then
        begin
          NativeGetterFunction := TGocciaNativeFunctionValue(TGocciaPropertyDescriptorAccessor(Descriptor).Getter);
          Args := TGocciaArgumentsCollection.Create;
          try
            Result := NativeGetterFunction.Call(Args, AThisContext);
          finally
            Args.Free;
          end;
          Exit;
        end;
      end;
      // No getter - return undefined
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end
    else if Descriptor is TGocciaPropertyDescriptorData then
    begin
      Result := TGocciaPropertyDescriptorData(Descriptor).Value;
      Exit;
    end;
  end;

  // Check prototype chain
  if Assigned(FPrototype) then
  begin
    Result := FPrototype.GetPropertyWithContext(AName, AThisContext);
    Exit;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaObjectValue.GetOwnPropertyDescriptor(const AName: string): TGocciaPropertyDescriptor;
begin
  if FPropertyDescriptors.ContainsKey(AName) then
    Result := FPropertyDescriptors[AName]
  else
    Result := nil;
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

function TGocciaObjectValue.DeleteProperty(const AName: string): Boolean;
var
  Index: Integer;
  Descriptor: TGocciaPropertyDescriptor;
begin
  if not FPropertyDescriptors.ContainsKey(AName) then
  begin
    Result := True;
    Exit;
  end;

  Descriptor := FPropertyDescriptors[AName];

  if not Descriptor.Configurable then
  begin
    Result := False;
    Exit;
  end;

  FPropertyDescriptors.Remove(AName);
  Index := FPropertyInsertionOrder.IndexOf(AName);
  if Index >= 0 then
    FPropertyInsertionOrder.Delete(Index);

  Result := True;
end;

function TGocciaObjectValue.GetEnumerablePropertyNames: TArray<string>;
var
  Names: TArray<string>;
  Count: Integer;
  I: Integer;
  PropertyName: string;
  Descriptor: TGocciaPropertyDescriptor;
begin
  SetLength(Names, FPropertyInsertionOrder.Count);
  Count := 0;

  // Iterate in insertion order
  for I := 0 to FPropertyInsertionOrder.Count - 1 do
  begin
    PropertyName := FPropertyInsertionOrder[I];
    if FPropertyDescriptors.TryGetValue(PropertyName, Descriptor) and Descriptor.Enumerable then
    begin
      Names[Count] := PropertyName;
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
  I: Integer;
  PropertyName: string;
  Descriptor: TGocciaPropertyDescriptor;
begin
  SetLength(Values, FPropertyInsertionOrder.Count);
  Count := 0;

  // Iterate in insertion order
  for I := 0 to FPropertyInsertionOrder.Count - 1 do
  begin
    PropertyName := FPropertyInsertionOrder[I];
    if FPropertyDescriptors.TryGetValue(PropertyName, Descriptor) and
       Descriptor.Enumerable and (Descriptor is TGocciaPropertyDescriptorData) then
    begin
      Values[Count] := TGocciaPropertyDescriptorData(Descriptor).Value;
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
  I: Integer;
  PropertyName: string;
  Descriptor: TGocciaPropertyDescriptor;
  Entry: TPair<string, TGocciaValue>;
begin
  SetLength(Entries, FPropertyInsertionOrder.Count);
  Count := 0;

  // Iterate in insertion order
  for I := 0 to FPropertyInsertionOrder.Count - 1 do
  begin
    PropertyName := FPropertyInsertionOrder[I];
    if FPropertyDescriptors.TryGetValue(PropertyName, Descriptor) and
       Descriptor.Enumerable and (Descriptor is TGocciaPropertyDescriptorData) then
    begin
      Entry.Key := PropertyName;
      Entry.Value := TGocciaPropertyDescriptorData(Descriptor).Value;
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
  I: Integer;
  PropertyName: string;
begin
  SetLength(Names, FPropertyInsertionOrder.Count);
  Count := 0;

  // Iterate in insertion order
  for I := 0 to FPropertyInsertionOrder.Count - 1 do
  begin
    PropertyName := FPropertyInsertionOrder[I];
    if FPropertyDescriptors.ContainsKey(PropertyName) then
    begin
      Names[Count] := PropertyName;
      Inc(Count);
    end;
  end;

  SetLength(Names, Count);
  Result := Names;
end;

{ Symbol property methods }

procedure TGocciaObjectValue.DefineSymbolProperty(const ASymbol: TGocciaSymbolValue; const ADescriptor: TGocciaPropertyDescriptor);
var
  ExistingDescriptor: TGocciaPropertyDescriptor;
begin
  if FSymbolDescriptors.TryGetValue(ASymbol, ExistingDescriptor) then
  begin
    if not ExistingDescriptor.Configurable then
      ThrowTypeError('Cannot redefine non-configurable property ''' + ASymbol.ToStringLiteral.Value + '''');
  end
  else
    FSymbolInsertionOrder.Add(ASymbol);

  FSymbolDescriptors.AddOrSetValue(ASymbol, ADescriptor);
end;

procedure TGocciaObjectValue.AssignSymbolProperty(const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue);
begin
  DefineSymbolProperty(ASymbol, TGocciaPropertyDescriptorData.Create(AValue, [pfEnumerable, pfConfigurable, pfWritable]));
end;

function TGocciaObjectValue.GetSymbolProperty(const ASymbol: TGocciaSymbolValue): TGocciaValue;
var
  Descriptor: TGocciaPropertyDescriptor;
  Accessor: TGocciaPropertyDescriptorAccessor;
  Args: TGocciaArgumentsCollection;
begin
  if FSymbolDescriptors.TryGetValue(ASymbol, Descriptor) then
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
            Result := TGocciaNativeFunctionValue(Accessor.Getter).Call(Args, Self)
          else if Accessor.Getter is TGocciaFunctionValue then
            Result := TGocciaFunctionValue(Accessor.Getter).Call(Args, Self)
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

  // Check prototype chain
  if Assigned(FPrototype) then
  begin
    Result := FPrototype.GetSymbolProperty(ASymbol);
    Exit;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaObjectValue.GetOwnSymbolPropertyDescriptor(const ASymbol: TGocciaSymbolValue): TGocciaPropertyDescriptor;
begin
  if FSymbolDescriptors.ContainsKey(ASymbol) then
    Result := FSymbolDescriptors[ASymbol]
  else
    Result := nil;
end;

function TGocciaObjectValue.HasSymbolProperty(const ASymbol: TGocciaSymbolValue): Boolean;
begin
  Result := FSymbolDescriptors.ContainsKey(ASymbol);
end;

function TGocciaObjectValue.GetEnumerableSymbolProperties: TArray<TPair<TGocciaSymbolValue, TGocciaValue>>;
var
  Entries: TArray<TPair<TGocciaSymbolValue, TGocciaValue>>;
  Count, I: Integer;
  Symbol: TGocciaSymbolValue;
  Descriptor: TGocciaPropertyDescriptor;
begin
  SetLength(Entries, FSymbolInsertionOrder.Count);
  Count := 0;

  for I := 0 to FSymbolInsertionOrder.Count - 1 do
  begin
    Symbol := FSymbolInsertionOrder[I];
    if FSymbolDescriptors.TryGetValue(Symbol, Descriptor) then
    begin
      if Descriptor.Enumerable then
      begin
        if Descriptor is TGocciaPropertyDescriptorData then
        begin
          Entries[Count] := TPair<TGocciaSymbolValue, TGocciaValue>.Create(Symbol, TGocciaPropertyDescriptorData(Descriptor).Value);
          Inc(Count);
        end;
      end;
    end;
  end;

  SetLength(Entries, Count);
  Result := Entries;
end;

function TGocciaObjectValue.GetOwnSymbols: TArray<TGocciaSymbolValue>;
var
  Symbols: TArray<TGocciaSymbolValue>;
  I: Integer;
begin
  SetLength(Symbols, FSymbolInsertionOrder.Count);
  for I := 0 to FSymbolInsertionOrder.Count - 1 do
    Symbols[I] := FSymbolInsertionOrder[I];
  Result := Symbols;
end;

procedure TGocciaObjectValue.Freeze;
var
  Key: string;
  Descriptor: TGocciaPropertyDescriptor;
  NewDescriptor: TGocciaPropertyDescriptor;
begin
  // Make all existing properties non-writable and non-configurable
  for Key in FPropertyInsertionOrder do
  begin
    if FPropertyDescriptors.ContainsKey(Key) then
    begin
      Descriptor := FPropertyDescriptors[Key];
      if Descriptor is TGocciaPropertyDescriptorData then
      begin
        // Recreate with only enumerable flag (no writable, no configurable)
        if Descriptor.Enumerable then
          NewDescriptor := TGocciaPropertyDescriptorData.Create(TGocciaPropertyDescriptorData(Descriptor).Value, [pfEnumerable])
        else
          NewDescriptor := TGocciaPropertyDescriptorData.Create(TGocciaPropertyDescriptorData(Descriptor).Value, []);
        FPropertyDescriptors[Key] := NewDescriptor;
      end
      else if Descriptor is TGocciaPropertyDescriptorAccessor then
      begin
        // Accessor descriptors become non-configurable
        if Descriptor.Enumerable then
          NewDescriptor := TGocciaPropertyDescriptorAccessor.Create(
            TGocciaPropertyDescriptorAccessor(Descriptor).Getter,
            TGocciaPropertyDescriptorAccessor(Descriptor).Setter,
            [pfEnumerable])
        else
          NewDescriptor := TGocciaPropertyDescriptorAccessor.Create(
            TGocciaPropertyDescriptorAccessor(Descriptor).Getter,
            TGocciaPropertyDescriptorAccessor(Descriptor).Setter,
            []);
        FPropertyDescriptors[Key] := NewDescriptor;
      end;
    end;
  end;
  FFrozen := True;
  FSealed := True;
  FExtensible := False;
end;

function TGocciaObjectValue.IsFrozen: Boolean;
begin
  Result := FFrozen;
end;

procedure TGocciaObjectValue.Seal;
var
  Key: string;
  Descriptor: TGocciaPropertyDescriptor;
  NewDescriptor: TGocciaPropertyDescriptor;
  Flags: TPropertyFlags;
begin
  for Key in FPropertyInsertionOrder do
  begin
    if FPropertyDescriptors.ContainsKey(Key) then
    begin
      Descriptor := FPropertyDescriptors[Key];
      Flags := [];
      if Descriptor.Enumerable then
        Include(Flags, pfEnumerable);
      if Descriptor is TGocciaPropertyDescriptorData then
      begin
        if Descriptor.Writable then
          Include(Flags, pfWritable);
        NewDescriptor := TGocciaPropertyDescriptorData.Create(TGocciaPropertyDescriptorData(Descriptor).Value, Flags);
        FPropertyDescriptors[Key] := NewDescriptor;
      end
      else if Descriptor is TGocciaPropertyDescriptorAccessor then
      begin
        NewDescriptor := TGocciaPropertyDescriptorAccessor.Create(
          TGocciaPropertyDescriptorAccessor(Descriptor).Getter,
          TGocciaPropertyDescriptorAccessor(Descriptor).Setter,
          Flags);
        FPropertyDescriptors[Key] := NewDescriptor;
      end;
    end;
  end;
  FSealed := True;
  FExtensible := False;
end;

function TGocciaObjectValue.IsSealed: Boolean;
begin
  Result := FSealed or FFrozen;
end;

procedure TGocciaObjectValue.PreventExtensions;
begin
  FExtensible := False;
end;

function TGocciaObjectValue.IsExtensible: Boolean;
begin
  Result := FExtensible and not FSealed and not FFrozen;
end;

end.
