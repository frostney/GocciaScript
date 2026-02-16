unit Goccia.Values.MapValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Goccia.Values.ObjectValue, Goccia.Values.ArrayValue,
  Goccia.Values.NativeFunction, Goccia.Arguments.Collection,
  Generics.Collections, SysUtils;

type
  TGocciaMapEntry = record
    Key: TGocciaValue;
    Value: TGocciaValue;
  end;

  TGocciaMapValue = class(TGocciaObjectValue)
  private
    class var FSharedMapPrototype: TGocciaObjectValue;
    class var FPrototypeMethodHost: TGocciaMapValue;
  private
    FEntries: TList<TGocciaMapEntry>;

    function MapGet(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MapSet(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MapHas(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MapDelete(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MapClear(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MapForEach(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MapKeys(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MapValues(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function MapEntries(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    function FindEntry(AKey: TGocciaValue): Integer;
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure InitializePrototype;

    procedure SetEntry(AKey, AValue: TGocciaValue);

    function GetProperty(const AName: string): TGocciaValue; override;
    function ToArray: TGocciaArrayValue;
    function ToStringTag: string; override;

    procedure GCMarkReferences; override;

    property Entries: TList<TGocciaMapEntry> read FEntries;
  end;

implementation

uses
  Goccia.Evaluator.Comparison, Goccia.Values.FunctionValue,
  Goccia.Values.FunctionBase, Goccia.GarbageCollector;

constructor TGocciaMapValue.Create;
begin
  inherited Create(nil);
  FEntries := TList<TGocciaMapEntry>.Create;
  InitializePrototype;
  if Assigned(FSharedMapPrototype) then
    FPrototype := FSharedMapPrototype;
end;

procedure TGocciaMapValue.InitializePrototype;
begin
  if Assigned(FSharedMapPrototype) then Exit;

  FSharedMapPrototype := TGocciaObjectValue.Create;
  FPrototypeMethodHost := Self;

  FSharedMapPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapGet, 'get', 1));
  FSharedMapPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapSet, 'set', 2));
  FSharedMapPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapHas, 'has', 1));
  FSharedMapPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapDelete, 'delete', 1));
  FSharedMapPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapClear, 'clear', 0));
  FSharedMapPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapForEach, 'forEach', 1));
  FSharedMapPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapKeys, 'keys', 0));
  FSharedMapPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapValues, 'values', 0));
  FSharedMapPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapEntries, 'entries', 0));

  if Assigned(TGocciaGC.Instance) then
  begin
    TGocciaGC.Instance.PinValue(FSharedMapPrototype);
    TGocciaGC.Instance.PinValue(FPrototypeMethodHost);
  end;
end;

destructor TGocciaMapValue.Destroy;
begin
  FEntries.Free;
  inherited;
end;

procedure TGocciaMapValue.GCMarkReferences;
var
  I: Integer;
begin
  if GCMarked then Exit;
  inherited; // Marks self + object properties/prototype

  // Mark all map entries (keys and values)
  for I := 0 to FEntries.Count - 1 do
  begin
    if Assigned(FEntries[I].Key) then
      FEntries[I].Key.GCMarkReferences;
    if Assigned(FEntries[I].Value) then
      FEntries[I].Value.GCMarkReferences;
  end;
end;

function TGocciaMapValue.FindEntry(AKey: TGocciaValue): Integer;
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

procedure TGocciaMapValue.SetEntry(AKey, AValue: TGocciaValue);
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

function TGocciaMapValue.MapGet(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
  Index: Integer;
begin
  M := TGocciaMapValue(ThisValue);
  if Args.Length > 0 then
  begin
    Index := M.FindEntry(Args.GetElement(0));
    if Index >= 0 then
    begin
      Result := M.Entries[Index].Value;
      Exit;
    end;
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaMapValue.MapSet(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
  MapKey, MapValue: TGocciaValue;
begin
  M := TGocciaMapValue(ThisValue);
  if Args.Length >= 2 then
  begin
    MapKey := Args.GetElement(0);
    MapValue := Args.GetElement(1);
    M.SetEntry(MapKey, MapValue);
  end;
  Result := ThisValue;
end;

function TGocciaMapValue.MapHas(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
begin
  M := TGocciaMapValue(ThisValue);
  if (Args.Length > 0) and (M.FindEntry(Args.GetElement(0)) >= 0) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaMapValue.MapDelete(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
  Index: Integer;
begin
  M := TGocciaMapValue(ThisValue);
  Result := TGocciaBooleanLiteralValue.FalseValue;
  if Args.Length > 0 then
  begin
    Index := M.FindEntry(Args.GetElement(0));
    if Index >= 0 then
    begin
      M.FEntries.Delete(Index);
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end;
  end;
end;

function TGocciaMapValue.MapClear(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaMapValue(ThisValue).FEntries.Clear;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaMapValue.MapForEach(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if Args.Length = 0 then Exit;

  M := TGocciaMapValue(ThisValue);
  Callback := Args.GetElement(0);
  if not (Callback is TGocciaFunctionBase) then Exit;

  for I := 0 to M.Entries.Count - 1 do
  begin
    CallArgs := TGocciaArgumentsCollection.Create([M.Entries[I].Value, M.Entries[I].Key, ThisValue]);
    try
      TGocciaFunctionBase(Callback).Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      CallArgs.Free;
    end;
  end;
end;

function TGocciaMapValue.MapKeys(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
  Arr: TGocciaArrayValue;
  I: Integer;
begin
  M := TGocciaMapValue(ThisValue);
  Arr := TGocciaArrayValue.Create;
  for I := 0 to M.Entries.Count - 1 do
    Arr.Elements.Add(M.Entries[I].Key);
  Result := Arr;
end;

function TGocciaMapValue.MapValues(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
  Arr: TGocciaArrayValue;
  I: Integer;
begin
  M := TGocciaMapValue(ThisValue);
  Arr := TGocciaArrayValue.Create;
  for I := 0 to M.Entries.Count - 1 do
    Arr.Elements.Add(M.Entries[I].Value);
  Result := Arr;
end;

function TGocciaMapValue.MapEntries(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaMapValue(ThisValue).ToArray;
end;

end.
