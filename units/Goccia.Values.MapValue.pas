unit Goccia.Values.MapValue;

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
  TGocciaMapEntry = record
    Key: TGocciaValue;
    Value: TGocciaValue;
  end;

  TGocciaMapValue = class(TGocciaInstanceValue)
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
    constructor Create(const AClass: TGocciaClassValue = nil);
    destructor Destroy; override;

    procedure SetEntry(const AKey, AValue: TGocciaValue);

    function GetProperty(const AName: string): TGocciaValue; override;
    function ToArray: TGocciaArrayValue;
    function ToStringTag: string; override;

    procedure InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); override;
    procedure MarkReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);

    property Entries: TList<TGocciaMapEntry> read FEntries;
  end;

implementation

uses
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Evaluator.Comparison,
  Goccia.GarbageCollector,
  Goccia.Utils,
  Goccia.Values.FunctionBase,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

constructor TGocciaMapValue.Create(const AClass: TGocciaClassValue = nil);
begin
  inherited Create(AClass);
  FEntries := TList<TGocciaMapEntry>.Create;
  InitializePrototype;
  if not Assigned(AClass) and Assigned(FShared) then
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

class procedure TGocciaMapValue.ExposePrototype(const AConstructor: TGocciaValue);
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

procedure TGocciaMapValue.InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection);
var
  InitArg: TGocciaValue;
  ArrValue: TGocciaArrayValue;
  EntryArr: TGocciaArrayValue;
  I: Integer;
begin
  if AArguments.Length = 0 then
    Exit;
  InitArg := AArguments.GetElement(0);
  if InitArg is TGocciaArrayValue then
  begin
    ArrValue := TGocciaArrayValue(InitArg);
    for I := 0 to ArrValue.Elements.Count - 1 do
    begin
      if Assigned(ArrValue.Elements[I]) and (ArrValue.Elements[I] is TGocciaArrayValue) then
      begin
        EntryArr := TGocciaArrayValue(ArrValue.Elements[I]);
        if EntryArr.Elements.Count >= 2 then
          SetEntry(EntryArr.Elements[0], EntryArr.Elements[1]);
      end;
    end;
  end;
end;

procedure TGocciaMapValue.MarkReferences;
var
  I: Integer;
begin
  if GCMarked then Exit;
  inherited;

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
  if AName = PROP_SIZE then
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
  Result := CONSTRUCTOR_MAP;
end;

{ Instance methods }

// ES2026 §24.1.3.6 Map.prototype.get(key)
function TGocciaMapValue.MapGet(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
  Index: Integer;
begin
  // Step 1: Let M be the this value
  M := TGocciaMapValue(AThisValue);
  // Steps 2-3 (implicit): Require M has [[MapData]] internal slot
  if AArgs.Length > 0 then
  begin
    // Step 4: For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
    // Step 4a: If p.[[Key]] is not empty and SameValueZero(p.[[Key]], key) is true, return p.[[Value]]
    Index := M.FindEntry(AArgs.GetElement(0));
    if Index >= 0 then
    begin
      Result := M.Entries[Index].Value;
      Exit;
    end;
  end;
  // Step 5: Return undefined
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §24.1.3.9 Map.prototype.set(key, value)
function TGocciaMapValue.MapSet(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
  MapKey, MapValue: TGocciaValue;
begin
  // Step 1: Let M be the this value
  M := TGocciaMapValue(AThisValue);
  // Steps 2-3 (implicit): Require M has [[MapData]] internal slot
  if AArgs.Length >= 2 then
  begin
    MapKey := AArgs.GetElement(0);
    MapValue := AArgs.GetElement(1);
    // Step 4: For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
    //   If p.[[Key]] is not empty and SameValueZero(p.[[Key]], key) is true,
    //   set p.[[Value]] to value and return M
    // Step 5: If key is -0, set key to +0
    // Step 6: Let p be the Record { [[Key]]: key, [[Value]]: value }
    // Step 7: Append p to M.[[MapData]]
    M.SetEntry(MapKey, MapValue);
  end;
  // Step 8: Return M
  Result := AThisValue;
end;

// ES2026 §24.1.3.7 Map.prototype.has(key)
function TGocciaMapValue.MapHas(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
begin
  // Step 1: Let M be the this value
  M := TGocciaMapValue(AThisValue);
  // Steps 2-3 (implicit): Require M has [[MapData]] internal slot
  // Step 4: For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
  //   If p.[[Key]] is not empty and SameValueZero(p.[[Key]], key) is true, return true
  if (AArgs.Length > 0) and (M.FindEntry(AArgs.GetElement(0)) >= 0) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    // Step 5: Return false
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §24.1.3.3 Map.prototype.delete(key)
function TGocciaMapValue.MapDelete(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
  Index: Integer;
begin
  // Step 1: Let M be the this value
  M := TGocciaMapValue(AThisValue);
  // Steps 2-3 (implicit): Require M has [[MapData]] internal slot
  // Step 5 (early): Default return false
  Result := TGocciaBooleanLiteralValue.FalseValue;
  if AArgs.Length > 0 then
  begin
    // Step 4: For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
    Index := M.FindEntry(AArgs.GetElement(0));
    if Index >= 0 then
    begin
      // Step 4a: If p.[[Key]] is not empty and SameValueZero(p.[[Key]], key) is true
      // Step 4a.i: Set p.[[Key]] to empty / Step 4a.ii: Set p.[[Value]] to empty
      M.FEntries.Delete(Index);
      // Step 4a.iii: Return true
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end;
  end;
  // Step 5: Return false
end;

// ES2026 §24.1.3.1 Map.prototype.clear()
function TGocciaMapValue.MapClear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Let M be the this value
  // Steps 2-3 (implicit): Require M has [[MapData]] internal slot
  // Step 4: For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
  //   Set p.[[Key]] to empty, set p.[[Value]] to empty
  TGocciaMapValue(AThisValue).FEntries.Clear;
  // Step 5: Return undefined
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §24.1.3.5 Map.prototype.forEach(callbackfn [, thisArg])
function TGocciaMapValue.MapForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  M: TGocciaMapValue;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if AArgs.Length = 0 then Exit;

  M := TGocciaMapValue(AThisValue);
  Callback := AArgs.GetElement(0);
  if not Callback.IsCallable then Exit;

  TypedCallback := nil;
  if Callback is TGocciaFunctionBase then
    TypedCallback := TGocciaFunctionBase(Callback);

  for I := 0 to M.Entries.Count - 1 do
  begin
    CallArgs := TGocciaArgumentsCollection.Create([M.Entries[I].Value, M.Entries[I].Key, AThisValue]);
    try
      if Assigned(TypedCallback) then
        TypedCallback.Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue)
      else
        InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      CallArgs.Free;
    end;
  end;
end;

// ES2026 §24.1.3.8 Map.prototype.keys()
function TGocciaMapValue.MapKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Let M be the this value
  // Step 2: Return CreateMapIterator(M, key)
  Result := TGocciaMapIteratorValue.Create(AThisValue, mkKeys);
end;

// ES2026 §24.1.3.11 Map.prototype.values()
function TGocciaMapValue.MapValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Let M be the this value
  // Step 2: Return CreateMapIterator(M, value)
  Result := TGocciaMapIteratorValue.Create(AThisValue, mkValues);
end;

// ES2026 §24.1.3.4 Map.prototype.entries()
function TGocciaMapValue.MapEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Let M be the this value
  // Step 2: Return CreateMapIterator(M, key+value)
  Result := TGocciaMapIteratorValue.Create(AThisValue, mkEntries);
end;

// ES2026 §24.1.3.12 Map.prototype[@@iterator]()
function TGocciaMapValue.MapSymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Return the result of calling Map.prototype.entries()
  Result := TGocciaMapIteratorValue.Create(AThisValue, mkEntries);
end;

end.
