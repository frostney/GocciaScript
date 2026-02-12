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

    procedure SetEntry(AKey, AValue: TGocciaValue);

    function GetProperty(const AName: string): TGocciaValue; override;
    function ToArray: TGocciaArrayValue;
    function ToStringTag: string; override;

    property Entries: TList<TGocciaMapEntry> read FEntries;
  end;

implementation

uses
  Goccia.Evaluator.Comparison, Goccia.Values.FunctionValue,
  Goccia.Values.FunctionBase;

constructor TGocciaMapValue.Create;
begin
  inherited Create(nil);
  FEntries := TList<TGocciaMapEntry>.Create;

  // Register instance methods
  RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapGet, 'get', 1));
  RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapSet, 'set', 2));
  RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapHas, 'has', 1));
  RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapDelete, 'delete', 1));
  RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapClear, 'clear', 0));
  RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapForEach, 'forEach', 1));
  RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapKeys, 'keys', 0));
  RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapValues, 'values', 0));
  RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(MapEntries, 'entries', 0));
end;

destructor TGocciaMapValue.Destroy;
begin
  FEntries.Free;
  inherited;
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
  Index: Integer;
begin
  if Args.Length > 0 then
  begin
    Index := FindEntry(Args.GetElement(0));
    if Index >= 0 then
    begin
      Result := FEntries[Index].Value;
      Exit;
    end;
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaMapValue.MapSet(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  MapKey, MapValue: TGocciaValue;
begin
  if Args.Length >= 2 then
  begin
    MapKey := Args.GetElement(0);
    MapValue := Args.GetElement(1);
    SetEntry(MapKey, MapValue);
  end;
  Result := Self; // Returns the Map itself for chaining
end;

function TGocciaMapValue.MapHas(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  if Args.Length > 0 then
    Result := TGocciaBooleanLiteralValue.Create(FindEntry(Args.GetElement(0)) >= 0)
  else
    Result := TGocciaBooleanLiteralValue.Create(False);
end;

function TGocciaMapValue.MapDelete(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Index: Integer;
begin
  Result := TGocciaBooleanLiteralValue.Create(False);
  if Args.Length > 0 then
  begin
    Index := FindEntry(Args.GetElement(0));
    if Index >= 0 then
    begin
      FEntries.Delete(Index);
      Result := TGocciaBooleanLiteralValue.Create(True);
    end;
  end;
end;

function TGocciaMapValue.MapClear(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  FEntries.Clear;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaMapValue.MapForEach(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if Args.Length = 0 then Exit;

  Callback := Args.GetElement(0);
  if not (Callback is TGocciaFunctionBase) then Exit;

  for I := 0 to FEntries.Count - 1 do
  begin
    CallArgs := TGocciaArgumentsCollection.Create([FEntries[I].Value, FEntries[I].Key, Self]);
    try
      TGocciaFunctionBase(Callback).Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      CallArgs.Free;
    end;
  end;
end;

function TGocciaMapValue.MapKeys(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  I: Integer;
begin
  Arr := TGocciaArrayValue.Create;
  for I := 0 to FEntries.Count - 1 do
    Arr.Elements.Add(FEntries[I].Key);
  Result := Arr;
end;

function TGocciaMapValue.MapValues(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  I: Integer;
begin
  Arr := TGocciaArrayValue.Create;
  for I := 0 to FEntries.Count - 1 do
    Arr.Elements.Add(FEntries[I].Value);
  Result := Arr;
end;

function TGocciaMapValue.MapEntries(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := ToArray;
end;

end.
