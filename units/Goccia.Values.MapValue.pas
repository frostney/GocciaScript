unit Goccia.Values.MapValue;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.SharedPrototype,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaMapEntry = record
    Key: TGocciaValue;
    Value: TGocciaValue;
  end;

  TGocciaMapValue = class(TGocciaObjectValue)
  private
    class var FShared: TGocciaSharedPrototype;
  private
    FEntries: TList<TGocciaMapEntry>;

    function MapGet(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MapSet(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MapHas(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MapDelete(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MapClear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MapForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MapKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MapValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MapEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function MapSymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function FindEntry(const AKey: TGocciaValue): Integer;
    procedure InitializePrototype;
  public
    constructor Create; overload;
    destructor Destroy; override;

    procedure SetEntry(const AKey, AValue: TGocciaValue);

    function GetProperty(const AName: string): TGocciaValue; override;
    function ToArray: TGocciaArrayValue;
    function ToStringTag: string; override;

    procedure MarkReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);

    property Entries: TList<TGocciaMapEntry> read FEntries;
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

constructor TGocciaMapValue.Create;
begin
  inherited Create(nil);
  FEntries := TList<TGocciaMapEntry>.Create;
  InitializePrototype;
  if Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

procedure TGocciaMapValue.InitializePrototype;
begin
  if Assigned(FShared) then Exit;

  FShared := TGocciaSharedPrototype.Create(Self);

  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapGet, 'get', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapSet, 'set', 2));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapHas, 'has', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapDelete, 'delete', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapClear, 'clear', 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapForEach, 'forEach', 1));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapKeys, 'keys', 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapValues, 'values', 0));
  FShared.Prototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapEntries, 'entries', 0));

  FShared.Prototype.DefineSymbolProperty(
    TGocciaSymbolValue.WellKnownIterator,
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(MapSymbolIterator, '[Symbol.iterator]', 0),
      [pfConfigurable, pfWritable]
    )
  );
end;

class procedure TGocciaMapValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
begin
  if not Assigned(FShared) then
    TGocciaMapValue.Create;
  FShared.ExposeOnConstructor(AConstructor);
end;

destructor TGocciaMapValue.Destroy;
begin
  FEntries.Free;
  inherited;
end;

procedure TGocciaMapValue.MarkReferences;
var
  I: Integer;
begin
  if GCMarked then Exit;
  inherited; // Marks self + object properties/prototype

  // Mark all map entries (keys and values)
  for I := 0 to FEntries.Count - 1 do
  begin
    if Assigned(FEntries[I].Key) then
      FEntries[I].Key.MarkReferences;
    if Assigned(FEntries[I].Value) then
      FEntries[I].Value.MarkReferences;
  end;
end;

function TGocciaMapValue.FindEntry(const AKey: TGocciaValue): Integer;
var
  I: Integer;
begin
  for I := 0 to FEntries.Count - 1 do
  begin
    if IsSameValueZero(FEntries[I].Key, AKey) then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

procedure TGocciaMapValue.SetEntry(const AKey, AValue: TGocciaValue);
var
  Index: Integer;
  Entry: TGocciaMapEntry;
begin
  Index := FindEntry(AKey);
  Entry.Key := AKey;
  Entry.Value := AValue;

  if Index >= 0 then
    FEntries[Index] := Entry
  else
    FEntries.Add(Entry);
end;

function TGocciaMapValue.GetProperty(const AName: string): TGocciaValue;
begin
  if AName = 'size' then
    Result := TGocciaNumberLiteralValue.Create(FEntries.Count)
  else
    Result := inherited GetProperty(AName);
end;

function TGocciaMapValue.ToArray: TGocciaArrayValue;
var
  I: Integer;
  EntryArr: TGocciaArrayValue;
begin
  Result := TGocciaArrayValue.Create;
  for I := 0 to FEntries.Count - 1 do
  begin
    EntryArr := TGocciaArrayValue.Create;
    EntryArr.Elements.Add(FEntries[I].Key);
    EntryArr.Elements.Add(FEntries[I].Value);
    Result.Elements.Add(EntryArr);
  end;
end;

function TGocciaMapValue.ToStringTag: string;
begin
  Result := 'Map';
end;

{ Instance methods }

function TGocciaMapValue.MapGet(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
  Index: Integer;
begin
  M := TGocciaMapValue(AThisValue);
  if AArgs.Length > 0 then
  begin
    Index := M.FindEntry(AArgs.GetElement(0));
    if Index >= 0 then
    begin
      Result := M.Entries[Index].Value;
      Exit;
    end;
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaMapValue.MapSet(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
  MapKey, MapValue: TGocciaValue;
begin
  M := TGocciaMapValue(AThisValue);
  if AArgs.Length >= 2 then
  begin
    MapKey := AArgs.GetElement(0);
    MapValue := AArgs.GetElement(1);
    M.SetEntry(MapKey, MapValue);
  end;
  Result := AThisValue;
end;

function TGocciaMapValue.MapHas(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
begin
  M := TGocciaMapValue(AThisValue);
  if (AArgs.Length > 0) and (M.FindEntry(AArgs.GetElement(0)) >= 0) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaMapValue.MapDelete(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
  Index: Integer;
begin
  M := TGocciaMapValue(AThisValue);
  Result := TGocciaBooleanLiteralValue.FalseValue;
  if AArgs.Length > 0 then
  begin
    Index := M.FindEntry(AArgs.GetElement(0));
    if Index >= 0 then
    begin
      M.FEntries.Delete(Index);
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end;
  end;
end;

function TGocciaMapValue.MapClear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaMapValue(AThisValue).FEntries.Clear;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaMapValue.MapForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if AArgs.Length = 0 then Exit;

  M := TGocciaMapValue(AThisValue);
  Callback := AArgs.GetElement(0);
  if not Callback.IsCallable then Exit;

  for I := 0 to M.Entries.Count - 1 do
  begin
    CallArgs := TGocciaArgumentsCollection.Create([M.Entries[I].Value, M.Entries[I].Key, AThisValue]);
    try
      TGocciaFunctionBase(Callback).Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      CallArgs.Free;
    end;
  end;
end;

function TGocciaMapValue.MapKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaMapIteratorValue.Create(AThisValue, mkKeys);
end;

function TGocciaMapValue.MapValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaMapIteratorValue.Create(AThisValue, mkValues);
end;

function TGocciaMapValue.MapEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaMapIteratorValue.Create(AThisValue, mkEntries);
end;

function TGocciaMapValue.MapSymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaMapIteratorValue.Create(AThisValue, mkEntries);
end;

end.
