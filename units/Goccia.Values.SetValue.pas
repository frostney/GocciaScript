unit Goccia.Values.SetValue;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.SharedPrototype,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaSetValue = class(TGocciaInstanceValue)
  private
    class var FShared: TGocciaSharedPrototype;
  private
    FItems: TList<TGocciaValue>;

    function SetHas(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetDelete(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetClear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetSymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetUnion(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetIntersection(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetDifference(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetSymmetricDifference(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetIsSubsetOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetIsSupersetOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetIsDisjointFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    procedure InitializePrototype;
  public
    constructor Create(const AClass: TGocciaClassValue = nil);
    destructor Destroy; override;

    function ContainsValue(const AValue: TGocciaValue): Boolean;
    procedure AddItem(const AValue: TGocciaValue);

    function GetProperty(const AName: string): TGocciaValue; override;
    function ToArray: TGocciaArrayValue;
    function ToStringTag: string; override;

    procedure InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); override;
    procedure MarkReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);

    property Items: TList<TGocciaValue> read FItems;
  end;

implementation

uses
  Goccia.Evaluator.Comparison,
  Goccia.GarbageCollector,
  Goccia.Values.FunctionBase,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

constructor TGocciaSetValue.Create(const AClass: TGocciaClassValue = nil);
begin
  inherited Create(AClass);
  FItems := TList<TGocciaValue>.Create;
  InitializePrototype;
  if not Assigned(AClass) and Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

procedure TGocciaSetValue.InitializePrototype;
begin
  if Assigned(FShared) then Exit;

  FShared := TGocciaSharedPrototype.Create(Self);

  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(SetHas, 'has', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(SetAdd, 'add', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(SetDelete, 'delete', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(SetClear, 'clear', 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(SetForEach, 'forEach', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(SetValues, 'values', 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(SetKeys, 'keys', 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(SetEntries, 'entries', 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(SetUnion, 'union', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(SetIntersection, 'intersection', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(SetDifference, 'difference', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(SetSymmetricDifference, 'symmetricDifference', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(SetIsSubsetOf, 'isSubsetOf', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(SetIsSupersetOf, 'isSupersetOf', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(SetIsDisjointFrom, 'isDisjointFrom', 1));

  FShared.Prototype.DefineSymbolProperty(
    TGocciaSymbolValue.WellKnownIterator,
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(SetSymbolIterator, '[Symbol.iterator]', 0),
      [pfConfigurable, pfWritable]
    )
  );
end;

class procedure TGocciaSetValue.ExposePrototype(const AConstructor: TGocciaValue);
begin
  if not Assigned(FShared) then
    TGocciaSetValue.Create;
  FShared.ExposeOnConstructor(AConstructor);
end;

destructor TGocciaSetValue.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TGocciaSetValue.InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection);
var
  InitArg: TGocciaValue;
  ArrValue: TGocciaArrayValue;
  I: Integer;
begin
  if AArguments.Length = 0 then
    Exit;
  InitArg := AArguments.GetElement(0);
  if InitArg is TGocciaArrayValue then
  begin
    ArrValue := TGocciaArrayValue(InitArg);
    for I := 0 to ArrValue.Elements.Count - 1 do
      if Assigned(ArrValue.Elements[I]) then
        AddItem(ArrValue.Elements[I]);
  end
  else if InitArg is TGocciaSetValue then
  begin
    for I := 0 to TGocciaSetValue(InitArg).Items.Count - 1 do
      AddItem(TGocciaSetValue(InitArg).Items[I]);
  end;
end;

procedure TGocciaSetValue.MarkReferences;
var
  I: Integer;
begin
  if GCMarked then Exit;
  inherited;

  for I := 0 to FItems.Count - 1 do
  begin
    if Assigned(FItems[I]) then
      FItems[I].MarkReferences;
  end;
end;

function TGocciaSetValue.ContainsValue(const AValue: TGocciaValue): Boolean;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    if IsSameValueZero(FItems[I], AValue) then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

procedure TGocciaSetValue.AddItem(const AValue: TGocciaValue);
begin
  if not ContainsValue(AValue) then
    FItems.Add(AValue);
end;

function TGocciaSetValue.GetProperty(const AName: string): TGocciaValue;
begin
  if AName = 'size' then
    Result := TGocciaNumberLiteralValue.Create(FItems.Count)
  else
    Result := inherited GetProperty(AName);
end;

function TGocciaSetValue.ToArray: TGocciaArrayValue;
var
  I: Integer;
begin
  Result := TGocciaArrayValue.Create;
  for I := 0 to FItems.Count - 1 do
    Result.Elements.Add(FItems[I]);
end;

function TGocciaSetValue.ToStringTag: string;
begin
  Result := 'Set';
end;

{ Instance methods }

function TGocciaSetValue.SetHas(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  S: TGocciaSetValue;
begin
  S := TGocciaSetValue(AThisValue);
  if (AArgs.Length > 0) and S.ContainsValue(AArgs.GetElement(0)) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaSetValue.SetAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  S: TGocciaSetValue;
begin
  S := TGocciaSetValue(AThisValue);
  if AArgs.Length > 0 then
    S.AddItem(AArgs.GetElement(0));
  Result := AThisValue;
end;

function TGocciaSetValue.SetDelete(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  S: TGocciaSetValue;
  I: Integer;
begin
  S := TGocciaSetValue(AThisValue);
  Result := TGocciaBooleanLiteralValue.FalseValue;
  if AArgs.Length > 0 then
  begin
    for I := 0 to S.FItems.Count - 1 do
    begin
      if IsSameValueZero(S.FItems[I], AArgs.GetElement(0)) then
      begin
        S.FItems.Delete(I);
        Result := TGocciaBooleanLiteralValue.TrueValue;
        Exit;
      end;
    end;
  end;
end;

function TGocciaSetValue.SetClear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaSetValue(AThisValue).FItems.Clear;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaSetValue.SetForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  S: TGocciaSetValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if AArgs.Length = 0 then Exit;

  S := TGocciaSetValue(AThisValue);
  Callback := AArgs.GetElement(0);
  if not Callback.IsCallable then Exit;

  for I := 0 to S.FItems.Count - 1 do
  begin
    CallArgs := TGocciaArgumentsCollection.Create([S.FItems[I], S.FItems[I], AThisValue]);
    try
      TGocciaFunctionBase(Callback).Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      CallArgs.Free;
    end;
  end;
end;

function TGocciaSetValue.SetValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaSetIteratorValue.Create(AThisValue, skValues);
end;

function TGocciaSetValue.SetKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaSetIteratorValue.Create(AThisValue, skValues);
end;

function TGocciaSetValue.SetEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaSetIteratorValue.Create(AThisValue, skEntries);
end;

function TGocciaSetValue.SetSymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaSetIteratorValue.Create(AThisValue, skValues);
end;

function TGocciaSetValue.SetUnion(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSet, OtherSet, ResultSet: TGocciaSetValue;
  I: Integer;
begin
  ThisSet := TGocciaSetValue(AThisValue);
  OtherSet := TGocciaSetValue(AArgs.GetElement(0));
  ResultSet := TGocciaSetValue.Create;

  for I := 0 to ThisSet.FItems.Count - 1 do
    ResultSet.AddItem(ThisSet.FItems[I]);
  for I := 0 to OtherSet.FItems.Count - 1 do
    ResultSet.AddItem(OtherSet.FItems[I]);

  Result := ResultSet;
end;

function TGocciaSetValue.SetIntersection(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSet, OtherSet, ResultSet: TGocciaSetValue;
  I: Integer;
begin
  ThisSet := TGocciaSetValue(AThisValue);
  OtherSet := TGocciaSetValue(AArgs.GetElement(0));
  ResultSet := TGocciaSetValue.Create;

  for I := 0 to ThisSet.FItems.Count - 1 do
    if OtherSet.ContainsValue(ThisSet.FItems[I]) then
      ResultSet.AddItem(ThisSet.FItems[I]);

  Result := ResultSet;
end;

function TGocciaSetValue.SetDifference(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSet, OtherSet, ResultSet: TGocciaSetValue;
  I: Integer;
begin
  ThisSet := TGocciaSetValue(AThisValue);
  OtherSet := TGocciaSetValue(AArgs.GetElement(0));
  ResultSet := TGocciaSetValue.Create;

  for I := 0 to ThisSet.FItems.Count - 1 do
    if not OtherSet.ContainsValue(ThisSet.FItems[I]) then
      ResultSet.AddItem(ThisSet.FItems[I]);

  Result := ResultSet;
end;

function TGocciaSetValue.SetSymmetricDifference(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSet, OtherSet, ResultSet: TGocciaSetValue;
  I: Integer;
begin
  ThisSet := TGocciaSetValue(AThisValue);
  OtherSet := TGocciaSetValue(AArgs.GetElement(0));
  ResultSet := TGocciaSetValue.Create;

  for I := 0 to ThisSet.FItems.Count - 1 do
    if not OtherSet.ContainsValue(ThisSet.FItems[I]) then
      ResultSet.AddItem(ThisSet.FItems[I]);

  for I := 0 to OtherSet.FItems.Count - 1 do
    if not ThisSet.ContainsValue(OtherSet.FItems[I]) then
      ResultSet.AddItem(OtherSet.FItems[I]);

  Result := ResultSet;
end;

function TGocciaSetValue.SetIsSubsetOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSet, OtherSet: TGocciaSetValue;
  I: Integer;
begin
  ThisSet := TGocciaSetValue(AThisValue);
  OtherSet := TGocciaSetValue(AArgs.GetElement(0));

  for I := 0 to ThisSet.FItems.Count - 1 do
    if not OtherSet.ContainsValue(ThisSet.FItems[I]) then
    begin
      Result := TGocciaBooleanLiteralValue.FalseValue;
      Exit;
    end;

  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

function TGocciaSetValue.SetIsSupersetOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSet, OtherSet: TGocciaSetValue;
  I: Integer;
begin
  ThisSet := TGocciaSetValue(AThisValue);
  OtherSet := TGocciaSetValue(AArgs.GetElement(0));

  for I := 0 to OtherSet.FItems.Count - 1 do
    if not ThisSet.ContainsValue(OtherSet.FItems[I]) then
    begin
      Result := TGocciaBooleanLiteralValue.FalseValue;
      Exit;
    end;

  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

function TGocciaSetValue.SetIsDisjointFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSet, OtherSet: TGocciaSetValue;
  I: Integer;
begin
  ThisSet := TGocciaSetValue(AThisValue);
  OtherSet := TGocciaSetValue(AArgs.GetElement(0));

  for I := 0 to ThisSet.FItems.Count - 1 do
    if OtherSet.ContainsValue(ThisSet.FItems[I]) then
    begin
      Result := TGocciaBooleanLiteralValue.FalseValue;
      Exit;
    end;

  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

end.
