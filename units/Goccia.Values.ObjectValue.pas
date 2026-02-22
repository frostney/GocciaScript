unit Goccia.Values.ObjectValue;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue;

type

  TGocciaObjectValue = class(TGocciaValue)
  protected
    FProperties: TGocciaPropertyMap;
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

    procedure AssignProperty(const AName: string; const AValue: TGocciaValue; const ACanCreate: Boolean = True); virtual;

    procedure RegisterNativeMethod(const AMethod: TGocciaValue);
    procedure RegisterConstant(const AName: string; const AValue: TGocciaValue);

    function GetProperty(const AName: string): TGocciaValue; override;
    procedure SetProperty(const AName: string; const AValue: TGocciaValue); override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
    function GetOwnPropertyDescriptor(const AName: string): TGocciaPropertyDescriptor;
    function HasProperty(const AName: string): Boolean;
    function HasOwnProperty(const AName: string): Boolean;
    function DeleteProperty(const AName: string): Boolean;

    function GetEnumerablePropertyNames: TArray<string>;
    function GetEnumerablePropertyValues: TArray<TGocciaValue>;
    function GetEnumerablePropertyEntries: TArray<TPair<string, TGocciaValue>>;
    function GetAllPropertyNames: TArray<string>;
    function GetOwnPropertyNames: TArray<string>;
    function GetOwnPropertyKeys: TArray<string>;

    procedure DefineSymbolProperty(const ASymbol: TGocciaSymbolValue; const ADescriptor: TGocciaPropertyDescriptor);
    procedure AssignSymbolProperty(const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue);
    function GetSymbolProperty(const ASymbol: TGocciaSymbolValue): TGocciaValue;
    function GetOwnSymbolPropertyDescriptor(const ASymbol: TGocciaSymbolValue): TGocciaPropertyDescriptor;
    function HasSymbolProperty(const ASymbol: TGocciaSymbolValue): Boolean;
    function GetEnumerableSymbolProperties: TArray<TPair<TGocciaSymbolValue, TGocciaValue>>;
    function GetOwnSymbols: TArray<TGocciaSymbolValue>;

    procedure Freeze;
    function IsFrozen: Boolean;
    procedure Seal;
    function IsSealed: Boolean;
    procedure PreventExtensions;
    function IsExtensible: Boolean;

    procedure MarkReferences; override;

    property Properties: TGocciaPropertyMap read FProperties;
    property Prototype: TGocciaObjectValue read FPrototype write FPrototype;
    property Frozen: Boolean read FFrozen;
    property Sealed: Boolean read FSealed;
    property Extensible: Boolean read FExtensible;
  end;


implementation

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Constants.ConstructorNames,
  Goccia.Values.ClassHelper,
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
  FProperties := TGocciaPropertyMap.Create;
  FSymbolDescriptors := TDictionary<TGocciaSymbolValue, TGocciaPropertyDescriptor>.Create;
  FSymbolInsertionOrder := TList<TGocciaSymbolValue>.Create;
  FPrototype := APrototype;
  FFrozen := False;
  FSealed := False;
  FExtensible := True;
end;

destructor TGocciaObjectValue.Destroy;
var
  PropEntries: TGocciaPropertyMap.TKeyValueArray;
  I: Integer;
  SymPair: TPair<TGocciaSymbolValue, TGocciaPropertyDescriptor>;
begin
  PropEntries := FProperties.ToArray;
  for I := 0 to Length(PropEntries) - 1 do
    PropEntries[I].Value.Free;
  FProperties.Free;

  for SymPair in FSymbolDescriptors do
    SymPair.Value.Free;
  FSymbolDescriptors.Free;

  FSymbolInsertionOrder.Free;
  inherited;
end;

procedure TGocciaObjectValue.MarkReferences;
var
  Entries: TGocciaPropertyMap.TKeyValueArray;
  I: Integer;
  SymPair: TPair<TGocciaSymbolValue, TGocciaPropertyDescriptor>;
begin
  if GCMarked then Exit;
  inherited;

  if Assigned(FPrototype) then
    FPrototype.MarkReferences;

  Entries := FProperties.ToArray;
  for I := 0 to Length(Entries) - 1 do
    MarkPropertyDescriptor(Entries[I].Value);

  for SymPair in FSymbolDescriptors do
  begin
    SymPair.Key.MarkReferences;
    MarkPropertyDescriptor(SymPair.Value);
  end;
end;

function TGocciaObjectValue.ToDebugString: string;
var
  SB: TStringBuilder;
  Entries: TGocciaPropertyMap.TKeyValueArray;
  I: Integer;
  First: Boolean;
  Value: TGocciaValue;
begin
  SB := TStringBuilder.Create;
  try
    SB.Append('{');
    First := True;

    Entries := FProperties.ToArray;
    for I := 0 to Length(Entries) - 1 do
    begin
      if not First then
        SB.Append(', ');

      if Entries[I].Value is TGocciaPropertyDescriptorData then
        Value := TGocciaPropertyDescriptorData(Entries[I].Value).Value
      else
        Value := nil;

      if Assigned(Value) then
      begin
        if Value is TGocciaObjectValue then
          SB.Append(Entries[I].Key).Append(': ').Append(TGocciaObjectValue(Value).ToDebugString)
        else
          SB.Append(Entries[I].Key).Append(': ').Append(Value.ToStringLiteral.Value);
      end
      else
        SB.Append(Entries[I].Key).Append(': [accessor]');

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
  Result := CONSTRUCTOR_OBJECT;
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
  if FFrozen then
    ThrowTypeError('Cannot assign to read only property ''' + AName + ''' of frozen object');

  if FProperties.TryGetValue(AName, Descriptor) then
  begin
    if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
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
      ThrowTypeError('Cannot set property ''' + AName + ''' which has only a getter');
    end
    else if Descriptor is TGocciaPropertyDescriptorData then
    begin
      if TGocciaPropertyDescriptorData(Descriptor).Writable then
      begin
        FProperties.Add(AName, TGocciaPropertyDescriptorData.Create(AValue, Descriptor.Flags));
        Descriptor.Free;
        Exit;
      end;
      ThrowTypeError('Cannot assign to read only property ''' + AName + '''');
    end;
  end;

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
  if FProperties.TryGetValue(AName, ExistingDescriptor) then
  begin
    if not ExistingDescriptor.Configurable then
      ThrowTypeError('Cannot redefine non-configurable property ''' + AName + '''');
    ExistingDescriptor.Free;
  end;

  FProperties.Add(AName, ADescriptor);
end;

function TGocciaObjectValue.GetOwnPropertyNames: TArray<string>;
begin
  Result := FProperties.Keys;
end;

function TGocciaObjectValue.GetOwnPropertyKeys: TArray<string>;
begin
  Result := FProperties.Keys;
end;



procedure TGocciaObjectValue.RegisterNativeMethod(const AMethod: TGocciaValue);
var
  OldDescriptor: TGocciaPropertyDescriptor;
begin
  if not (AMethod is TGocciaNativeFunctionValue) then
    raise Exception.Create('Method must be a native function');

  if FProperties.TryGetValue(TGocciaNativeFunctionValue(AMethod).Name, OldDescriptor) then
    OldDescriptor.Free;
  FProperties.Add(TGocciaNativeFunctionValue(AMethod).Name,
    TGocciaPropertyDescriptorData.Create(AMethod, [pfConfigurable, pfWritable]));
end;

procedure TGocciaObjectValue.RegisterConstant(const AName: string; const AValue: TGocciaValue);
var
  OldDescriptor: TGocciaPropertyDescriptor;
begin
  if FProperties.TryGetValue(AName, OldDescriptor) then
    OldDescriptor.Free;
  FProperties.Add(AName, TGocciaPropertyDescriptorData.Create(AValue, []));
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
  if FProperties.TryGetValue(AName, Descriptor) then
  begin
    if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
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
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end
    else if Descriptor is TGocciaPropertyDescriptorData then
    begin
      Result := TGocciaPropertyDescriptorData(Descriptor).Value;
      Exit;
    end;
  end;

  if Assigned(FPrototype) then
  begin
    Result := FPrototype.GetPropertyWithContext(AName, AThisContext);
    Exit;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaObjectValue.GetOwnPropertyDescriptor(const AName: string): TGocciaPropertyDescriptor;
begin
  if not FProperties.TryGetValue(AName, Result) then
    Result := nil;
end;

function TGocciaObjectValue.HasProperty(const AName: string): Boolean;
begin
  Result := FProperties.ContainsKey(AName);

  if not Result and Assigned(FPrototype) then
    Result := FPrototype.HasProperty(AName);
end;

function TGocciaObjectValue.HasOwnProperty(const AName: string): Boolean;
begin
  Result := FProperties.ContainsKey(AName);
end;

function TGocciaObjectValue.DeleteProperty(const AName: string): Boolean;
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  if not FProperties.TryGetValue(AName, Descriptor) then
  begin
    Result := True;
    Exit;
  end;

  if not Descriptor.Configurable then
  begin
    Result := False;
    Exit;
  end;

  FProperties.Remove(AName);
  Descriptor.Free;
  Result := True;
end;

function TGocciaObjectValue.GetEnumerablePropertyNames: TArray<string>;
var
  AllEntries: TGocciaPropertyMap.TKeyValueArray;
  Names: TArray<string>;
  Count, I: Integer;
begin
  AllEntries := FProperties.ToArray;
  SetLength(Names, Length(AllEntries));
  Count := 0;

  for I := 0 to Length(AllEntries) - 1 do
    if AllEntries[I].Value.Enumerable then
    begin
      Names[Count] := AllEntries[I].Key;
      Inc(Count);
    end;

  SetLength(Names, Count);
  Result := Names;
end;

function TGocciaObjectValue.GetEnumerablePropertyValues: TArray<TGocciaValue>;
var
  AllEntries: TGocciaPropertyMap.TKeyValueArray;
  Values: TArray<TGocciaValue>;
  Count, I: Integer;
begin
  AllEntries := FProperties.ToArray;
  SetLength(Values, Length(AllEntries));
  Count := 0;

  for I := 0 to Length(AllEntries) - 1 do
    if AllEntries[I].Value.Enumerable and (AllEntries[I].Value is TGocciaPropertyDescriptorData) then
    begin
      Values[Count] := TGocciaPropertyDescriptorData(AllEntries[I].Value).Value;
      Inc(Count);
    end;

  SetLength(Values, Count);
  Result := Values;
end;

function TGocciaObjectValue.GetEnumerablePropertyEntries: TArray<TPair<string, TGocciaValue>>;
var
  AllEntries: TGocciaPropertyMap.TKeyValueArray;
  Entries: TArray<TPair<string, TGocciaValue>>;
  Count, I: Integer;
  Entry: TPair<string, TGocciaValue>;
begin
  AllEntries := FProperties.ToArray;
  SetLength(Entries, Length(AllEntries));
  Count := 0;

  for I := 0 to Length(AllEntries) - 1 do
    if AllEntries[I].Value.Enumerable and (AllEntries[I].Value is TGocciaPropertyDescriptorData) then
    begin
      Entry.Key := AllEntries[I].Key;
      Entry.Value := TGocciaPropertyDescriptorData(AllEntries[I].Value).Value;
      Entries[Count] := Entry;
      Inc(Count);
    end;

  SetLength(Entries, Count);
  Result := Entries;
end;

function TGocciaObjectValue.GetAllPropertyNames: TArray<string>;
begin
  Result := FProperties.Keys;
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
    ExistingDescriptor.Free;
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
  AllEntries: TGocciaPropertyMap.TKeyValueArray;
  I: Integer;
  Descriptor: TGocciaPropertyDescriptor;
  NewDescriptor: TGocciaPropertyDescriptor;
begin
  AllEntries := FProperties.ToArray;
  for I := 0 to Length(AllEntries) - 1 do
  begin
    Descriptor := AllEntries[I].Value;
    if Descriptor is TGocciaPropertyDescriptorData then
    begin
      if Descriptor.Enumerable then
        NewDescriptor := TGocciaPropertyDescriptorData.Create(TGocciaPropertyDescriptorData(Descriptor).Value, [pfEnumerable])
      else
        NewDescriptor := TGocciaPropertyDescriptorData.Create(TGocciaPropertyDescriptorData(Descriptor).Value, []);
      FProperties.Add(AllEntries[I].Key, NewDescriptor);
      Descriptor.Free;
    end
    else if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
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
      FProperties.Add(AllEntries[I].Key, NewDescriptor);
      Descriptor.Free;
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
  AllEntries: TGocciaPropertyMap.TKeyValueArray;
  I: Integer;
  Descriptor: TGocciaPropertyDescriptor;
  NewDescriptor: TGocciaPropertyDescriptor;
  Flags: TPropertyFlags;
begin
  AllEntries := FProperties.ToArray;
  for I := 0 to Length(AllEntries) - 1 do
  begin
    Descriptor := AllEntries[I].Value;
    Flags := [];
    if Descriptor.Enumerable then
      Include(Flags, pfEnumerable);
    if Descriptor is TGocciaPropertyDescriptorData then
    begin
      if Descriptor.Writable then
        Include(Flags, pfWritable);
      NewDescriptor := TGocciaPropertyDescriptorData.Create(TGocciaPropertyDescriptorData(Descriptor).Value, Flags);
      FProperties.Add(AllEntries[I].Key, NewDescriptor);
      Descriptor.Free;
    end
    else if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      NewDescriptor := TGocciaPropertyDescriptorAccessor.Create(
        TGocciaPropertyDescriptorAccessor(Descriptor).Getter,
        TGocciaPropertyDescriptorAccessor(Descriptor).Setter,
        Flags);
      FProperties.Add(AllEntries[I].Key, NewDescriptor);
      Descriptor.Free;
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
