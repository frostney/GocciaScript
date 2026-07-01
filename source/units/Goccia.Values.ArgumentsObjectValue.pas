unit Goccia.Values.ArgumentsObjectValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Scope,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.VM.Upvalue;

// ES2026 §10.4.4.6 CreateUnmappedArgumentsObject(argumentsList).
function CreateUnmappedArgumentsObject(
  const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
// ES2026 §10.4.4.7 CreateMappedArgumentsObject(func, formals, argumentsList, env).
function CreateMappedArgumentsObject(
  const AArguments: TGocciaArgumentsCollection;
  const AParameterNames: array of string;
  const AEnvironment: TGocciaScope;
  const ACallee: TGocciaValue): TGocciaObjectValue;
function CreateMappedBytecodeArgumentsObject(
  const AArguments: TGocciaArgumentsCollection;
  const AParameterCells: array of TGocciaBytecodeCell;
  const ACallee: TGocciaValue): TGocciaObjectValue;

implementation

uses
  Generics.Collections,
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Values.ArrayValue,
  Goccia.Values.FunctionBase,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.VM.Registers;

type
  TGocciaArgumentMapping = record
    ParameterName: string;
    ParameterCell: TGocciaBytecodeCell;
  end;

  TGocciaArgumentsObjectValue = class(TGocciaObjectValue)
  public
    function ToStringTag: string; override;
  end;

  TGocciaMappedArgumentsObjectValue = class(TGocciaArgumentsObjectValue)
  private
    FEnvironment: TGocciaScope;
    FMappings: array of TGocciaArgumentMapping;

    function TryMappedIndex(const AName: string; out AIndex: Integer): Boolean;
    function GetMappedValue(const AIndex: Integer): TGocciaValue;
    procedure SetMappedValue(const AIndex: Integer; const AValue: TGocciaValue);
    procedure DeleteMappedValue(const AIndex: Integer);
    procedure ApplyDefineMapping(const AName: string;
      const ADescriptor: TGocciaPropertyDescriptor);
  public
    constructor CreateForScope(const AArguments: TGocciaArgumentsCollection;
      const AParameterNames: array of string;
      const AEnvironment: TGocciaScope;
      const ACallee: TGocciaValue);
    constructor CreateForCells(const AArguments: TGocciaArgumentsCollection;
      const AParameterCells: array of TGocciaBytecodeCell;
      const ACallee: TGocciaValue);

    function GetPropertyWithContext(const AName: string;
      const AThisContext: TGocciaValue): TGocciaValue; override;
    function GetOwnPropertyDescriptor(
      const AName: string): TGocciaPropertyDescriptor; override;
    procedure AssignProperty(const AName: string; const AValue: TGocciaValue;
      const ACanCreate: Boolean = True); override;
    function AssignPropertyWithReceiver(const AName: string;
      const AValue: TGocciaValue;
      const AReceiver: TGocciaValue): Boolean; override;
    procedure DefineProperty(const AName: string;
      const ADescriptor: TGocciaPropertyDescriptor); override;
    function TryDefineProperty(const AName: string;
      const ADescriptor: TGocciaPropertyDescriptor): Boolean; override;
    function DeleteProperty(const AName: string): Boolean; override;
    function GetEnumerablePropertyValues: TArray<TGocciaValue>; override;
    function GetEnumerablePropertyEntries: TArray<TPair<string, TGocciaValue>>; override;
    procedure MarkReferences; override;
  end;

function TGocciaArgumentsObjectValue.ToStringTag: string;
begin
  Result := 'Arguments';
end;

function CreateThrowTypeErrorFunction: TGocciaFunctionBase;
begin
  Result := GetThrowTypeErrorIntrinsic;
end;

function ArrayPrototypeIteratorFunction: TGocciaValue;
var
  ArrayValue: TGocciaArrayValue;
begin
  ArrayValue := TGocciaArrayValue.Create;
  Result := ArrayValue.GetSymbolProperty(TGocciaSymbolValue.WellKnownIterator);
end;

procedure DefineArgumentsIndexedProperties(
  const ATarget: TGocciaObjectValue;
  const AArguments: TGocciaArgumentsCollection);
var
  I: Integer;
begin
  for I := 0 to AArguments.Length - 1 do
    ATarget.DefineProperty(IntToStr(I),
      TGocciaPropertyDescriptorData.Create(
        AArguments.GetElement(I),
        [pfEnumerable, pfWritable, pfConfigurable]));
end;

procedure DefineArgumentsLengthAndIterator(
  const ATarget: TGocciaObjectValue;
  const AArguments: TGocciaArgumentsCollection);
var
  IteratorValue: TGocciaValue;
begin
  ATarget.DefineProperty(PROP_LENGTH,
    TGocciaPropertyDescriptorData.Create(
      TGocciaNumberLiteralValue.Create(AArguments.Length),
      [pfWritable, pfConfigurable]));

  IteratorValue := ArrayPrototypeIteratorFunction;
  if Assigned(IteratorValue) and not (IteratorValue is TGocciaUndefinedLiteralValue) then
    ATarget.DefineSymbolProperty(TGocciaSymbolValue.WellKnownIterator,
      TGocciaPropertyDescriptorData.Create(IteratorValue,
        [pfWritable, pfConfigurable]));
end;

function CompleteCalleeValue(const ACallee: TGocciaValue): TGocciaValue;
begin
  if Assigned(ACallee) then
    Result := ACallee
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

{ TGocciaMappedArgumentsObjectValue }

constructor TGocciaMappedArgumentsObjectValue.CreateForScope(
  const AArguments: TGocciaArgumentsCollection;
  const AParameterNames: array of string;
  const AEnvironment: TGocciaScope;
  const ACallee: TGocciaValue);
var
  I, J: Integer;
  AlreadyMapped: Boolean;
  ParameterName: string;
begin
  inherited Create(TGocciaObjectValue.SharedObjectPrototype,
    AArguments.Length + 3);
  FEnvironment := AEnvironment;
  DefineArgumentsIndexedProperties(Self, AArguments);
  DefineArgumentsLengthAndIterator(Self, AArguments);
  DefineProperty(PROP_CALLEE,
    TGocciaPropertyDescriptorData.Create(
      CompleteCalleeValue(ACallee), [pfWritable, pfConfigurable]));

  SetLength(FMappings, AArguments.Length);
  for I := High(AParameterNames) downto 0 do
  begin
    if I >= AArguments.Length then
      Continue;
    ParameterName := AParameterNames[I];
    AlreadyMapped := False;
    for J := I + 1 to High(AParameterNames) do
      if AParameterNames[J] = ParameterName then
      begin
        AlreadyMapped := True;
        Break;
      end;
    if AlreadyMapped then
      Continue;
    FMappings[I].ParameterName := ParameterName;
  end;
end;

constructor TGocciaMappedArgumentsObjectValue.CreateForCells(
  const AArguments: TGocciaArgumentsCollection;
  const AParameterCells: array of TGocciaBytecodeCell;
  const ACallee: TGocciaValue);
var
  I: Integer;
begin
  inherited Create(TGocciaObjectValue.SharedObjectPrototype,
    AArguments.Length + 3);
  DefineArgumentsIndexedProperties(Self, AArguments);
  DefineArgumentsLengthAndIterator(Self, AArguments);
  DefineProperty(PROP_CALLEE,
    TGocciaPropertyDescriptorData.Create(
      CompleteCalleeValue(ACallee), [pfWritable, pfConfigurable]));

  SetLength(FMappings, AArguments.Length);
  for I := 0 to High(AParameterCells) do
  begin
    if I >= Length(FMappings) then
      Break;
    FMappings[I].ParameterCell := AParameterCells[I];
  end;
end;

function TGocciaMappedArgumentsObjectValue.TryMappedIndex(
  const AName: string; out AIndex: Integer): Boolean;
begin
  Result := TryStrToInt(AName, AIndex) and (AIndex >= 0) and
    (AIndex < Length(FMappings)) and (IntToStr(AIndex) = AName) and
    ((FMappings[AIndex].ParameterName <> '') or
     Assigned(FMappings[AIndex].ParameterCell));
end;

function TGocciaMappedArgumentsObjectValue.GetMappedValue(
  const AIndex: Integer): TGocciaValue;
begin
  if (AIndex < 0) or (AIndex >= Length(FMappings)) then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  if Assigned(FMappings[AIndex].ParameterCell) then
    Exit(RegisterToValue(FMappings[AIndex].ParameterCell.Value));
  if Assigned(FEnvironment) and (FMappings[AIndex].ParameterName <> '') then
    Exit(FEnvironment.GetValue(FMappings[AIndex].ParameterName));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaMappedArgumentsObjectValue.SetMappedValue(
  const AIndex: Integer; const AValue: TGocciaValue);
begin
  if (AIndex < 0) or (AIndex >= Length(FMappings)) then
    Exit;
  if Assigned(FMappings[AIndex].ParameterCell) then
  begin
    FMappings[AIndex].ParameterCell.Value := ValueToRegister(AValue);
    Exit;
  end;
  if Assigned(FEnvironment) and (FMappings[AIndex].ParameterName <> '') then
    FEnvironment.ForceUpdateBinding(FMappings[AIndex].ParameterName, AValue);
end;

procedure TGocciaMappedArgumentsObjectValue.DeleteMappedValue(
  const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Length(FMappings)) then
    Exit;
  FMappings[AIndex].ParameterName := '';
  FMappings[AIndex].ParameterCell := nil;
end;

procedure TGocciaMappedArgumentsObjectValue.ApplyDefineMapping(
  const AName: string; const ADescriptor: TGocciaPropertyDescriptor);
var
  Index: Integer;
begin
  if not TryMappedIndex(AName, Index) then
    Exit;

  if IsAccessorDescriptor(ADescriptor) then
  begin
    DeleteMappedValue(Index);
    Exit;
  end;

  if ADescriptor.HasValue and (ADescriptor is TGocciaPropertyDescriptorData) then
    SetMappedValue(Index, TGocciaPropertyDescriptorData(ADescriptor).Value);
  if ADescriptor.HasWritableField and not ADescriptor.Writable then
    DeleteMappedValue(Index);
end;

function TGocciaMappedArgumentsObjectValue.GetPropertyWithContext(
  const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
var
  Descriptor: TGocciaPropertyDescriptor;
  Index: Integer;
begin
  if TryMappedIndex(AName, Index) then
  begin
    Descriptor := inherited GetOwnPropertyDescriptor(AName);
    if Assigned(Descriptor) and IsDataDescriptor(Descriptor) then
      Exit(GetMappedValue(Index));
  end;
  Result := inherited GetPropertyWithContext(AName, AThisContext);
end;

function TGocciaMappedArgumentsObjectValue.GetOwnPropertyDescriptor(
  const AName: string): TGocciaPropertyDescriptor;
var
  Index: Integer;
begin
  Result := inherited GetOwnPropertyDescriptor(AName);
  if Assigned(Result) and IsDataDescriptor(Result) and
     TryMappedIndex(AName, Index) and
     (Result is TGocciaPropertyDescriptorData) then
    TGocciaPropertyDescriptorData(Result).Value := GetMappedValue(Index);
end;

procedure TGocciaMappedArgumentsObjectValue.AssignProperty(
  const AName: string; const AValue: TGocciaValue;
  const ACanCreate: Boolean = True);
var
  Index: Integer;
begin
  inherited AssignProperty(AName, AValue, ACanCreate);
  if TryMappedIndex(AName, Index) then
    SetMappedValue(Index, AValue);
end;

function TGocciaMappedArgumentsObjectValue.AssignPropertyWithReceiver(
  const AName: string; const AValue: TGocciaValue;
  const AReceiver: TGocciaValue): Boolean;
var
  Index: Integer;
begin
  Result := inherited AssignPropertyWithReceiver(AName, AValue, AReceiver);
  if Result and (AReceiver = Self) and TryMappedIndex(AName, Index) then
    SetMappedValue(Index, AValue);
end;

procedure TGocciaMappedArgumentsObjectValue.DefineProperty(
  const AName: string; const ADescriptor: TGocciaPropertyDescriptor);
var
  MappingDescriptor: TGocciaPropertyDescriptor;
begin
  MappingDescriptor := ClonePropertyDescriptor(ADescriptor);
  try
    inherited DefineProperty(AName, ADescriptor);
    ApplyDefineMapping(AName, MappingDescriptor);
  finally
    MappingDescriptor.Free;
  end;
end;

function TGocciaMappedArgumentsObjectValue.TryDefineProperty(
  const AName: string; const ADescriptor: TGocciaPropertyDescriptor): Boolean;
var
  MappingDescriptor: TGocciaPropertyDescriptor;
begin
  MappingDescriptor := ClonePropertyDescriptor(ADescriptor);
  try
    Result := inherited TryDefineProperty(AName, ADescriptor);
    if Result then
      ApplyDefineMapping(AName, MappingDescriptor);
  finally
    MappingDescriptor.Free;
  end;
end;

function TGocciaMappedArgumentsObjectValue.DeleteProperty(
  const AName: string): Boolean;
var
  Index: Integer;
begin
  Result := inherited DeleteProperty(AName);
  if Result and TryMappedIndex(AName, Index) then
    DeleteMappedValue(Index);
end;

function TGocciaMappedArgumentsObjectValue.GetEnumerablePropertyValues:
  TArray<TGocciaValue>;
var
  Count: Integer;
  Pair: TGocciaPropertyMap.TKeyValuePair;
  Values: TArray<TGocciaValue>;
begin
  SetLength(Values, FProperties.Count);
  Count := 0;

  for Pair in FProperties do
    if Pair.Value.Enumerable and (Pair.Value is TGocciaPropertyDescriptorData) then
    begin
      Values[Count] := GetProperty(Pair.Key);
      Inc(Count);
    end;

  SetLength(Values, Count);
  Result := Values;
end;

function TGocciaMappedArgumentsObjectValue.GetEnumerablePropertyEntries:
  TArray<TPair<string, TGocciaValue>>;
var
  Count: Integer;
  Entries: TArray<TPair<string, TGocciaValue>>;
  Entry: TPair<string, TGocciaValue>;
  Pair: TGocciaPropertyMap.TKeyValuePair;
begin
  SetLength(Entries, FProperties.Count);
  Count := 0;

  for Pair in FProperties do
    if Pair.Value.Enumerable and (Pair.Value is TGocciaPropertyDescriptorData) then
    begin
      Entry.Key := Pair.Key;
      Entry.Value := GetProperty(Pair.Key);
      Entries[Count] := Entry;
      Inc(Count);
    end;

  SetLength(Entries, Count);
  Result := Entries;
end;

procedure TGocciaMappedArgumentsObjectValue.MarkReferences;
var
  I: Integer;
begin
  if GCMarked then
    Exit;
  inherited MarkReferences;
  if Assigned(FEnvironment) then
    FEnvironment.MarkReferences;
  for I := 0 to High(FMappings) do
    if Assigned(FMappings[I].ParameterCell) then
      MarkRegisterReferences(FMappings[I].ParameterCell.Value);
end;

function CreateUnmappedArgumentsObject(
  const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
var
  Thrower: TGocciaFunctionBase;
begin
  Result := TGocciaArgumentsObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype,
    AArguments.Length + 2);

  DefineArgumentsLengthAndIterator(Result, AArguments);
  DefineArgumentsIndexedProperties(Result, AArguments);

  Thrower := CreateThrowTypeErrorFunction;
  Result.DefineProperty(PROP_CALLEE,
    TGocciaPropertyDescriptorAccessor.Create(
      Thrower,
      Thrower,
      []));
end;

function CreateMappedArgumentsObject(
  const AArguments: TGocciaArgumentsCollection;
  const AParameterNames: array of string;
  const AEnvironment: TGocciaScope;
  const ACallee: TGocciaValue): TGocciaObjectValue;
begin
  Result := TGocciaMappedArgumentsObjectValue.CreateForScope(
    AArguments, AParameterNames, AEnvironment, ACallee);
end;

function CreateMappedBytecodeArgumentsObject(
  const AArguments: TGocciaArgumentsCollection;
  const AParameterCells: array of TGocciaBytecodeCell;
  const ACallee: TGocciaValue): TGocciaObjectValue;
begin
  Result := TGocciaMappedArgumentsObjectValue.CreateForCells(
    AArguments, AParameterCells, ACallee);
end;

end.
