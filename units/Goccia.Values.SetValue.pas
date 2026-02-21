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
  Goccia.Utils,
  Goccia.Values.ConstructorNames,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.PropertyNames,
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
  if AName = PROP_SIZE then
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
  Result := CTOR_SET;
end;

{ Instance methods }

// ES2026 §24.2.3.7 Set.prototype.has(value)
function TGocciaSetValue.SetHas(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  S: TGocciaSetValue;
begin
  // Step 1: Let S be the this value
  S := TGocciaSetValue(AThisValue);
  // Steps 2-3 (implicit): Require S has [[SetData]] internal slot
  // Step 4: For each element e of S.[[SetData]], do
  //   If e is not empty and SameValueZero(e, value) is true, return true
  if (AArgs.Length > 0) and S.ContainsValue(AArgs.GetElement(0)) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    // Step 5: Return false
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §24.2.3.1 Set.prototype.add(value)
function TGocciaSetValue.SetAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  S: TGocciaSetValue;
begin
  // Step 1: Let S be the this value
  S := TGocciaSetValue(AThisValue);
  // Steps 2-3 (implicit): Require S has [[SetData]] internal slot
  // Step 4: For each element e of S.[[SetData]], do
  //   If e is not empty and SameValueZero(e, value) is true, return S
  // Step 5: If value is -0, set value to +0
  // Step 6: Append value to S.[[SetData]]
  if AArgs.Length > 0 then
    S.AddItem(AArgs.GetElement(0));
  // Step 7: Return S
  Result := AThisValue;
end;

// ES2026 §24.2.3.4 Set.prototype.delete(value)
function TGocciaSetValue.SetDelete(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  S: TGocciaSetValue;
  I: Integer;
begin
  // Step 1: Let S be the this value
  S := TGocciaSetValue(AThisValue);
  // Steps 2-3 (implicit): Require S has [[SetData]] internal slot
  // Step 5 (early): Default return false
  Result := TGocciaBooleanLiteralValue.FalseValue;
  if AArgs.Length > 0 then
  begin
    // Step 4: For each element e of S.[[SetData]], do
    for I := 0 to S.FItems.Count - 1 do
    begin
      // Step 4a: If e is not empty and SameValueZero(e, value) is true
      if IsSameValueZero(S.FItems[I], AArgs.GetElement(0)) then
      begin
        // Step 4a.i: Replace the element of S.[[SetData]] with empty
        S.FItems.Delete(I);
        // Step 4a.ii: Return true
        Result := TGocciaBooleanLiteralValue.TrueValue;
        Exit;
      end;
    end;
  end;
  // Step 5: Return false
end;

// ES2026 §24.2.3.2 Set.prototype.clear()
function TGocciaSetValue.SetClear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Let S be the this value
  // Steps 2-3 (implicit): Require S has [[SetData]] internal slot
  // Step 4: For each element e of S.[[SetData]], replace e with empty
  TGocciaSetValue(AThisValue).FItems.Clear;
  // Step 5: Return undefined
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §24.2.3.6 Set.prototype.forEach(callbackfn [, thisArg])
function TGocciaSetValue.SetForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  S: TGocciaSetValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if AArgs.Length = 0 then Exit;

  // Step 1: Let S be the this value
  S := TGocciaSetValue(AThisValue);
  // Steps 2-3 (implicit): Require S has [[SetData]] internal slot
  // Step 4: If IsCallable(callbackfn) is false, throw a TypeError exception
  Callback := AArgs.GetElement(0);
  if not Callback.IsCallable then Exit;

  // Step 5: Let entries be S.[[SetData]]
  // Step 6: Let numEntries be the number of elements of entries
  // Step 7: Let index be 0
  // Step 8: Repeat, while index < numEntries
  for I := 0 to S.FItems.Count - 1 do
  begin
    // Step 8a: Let e be entries[index]
    // Step 8b: If e is not empty, then
    //   Call(callbackfn, thisArg, « e, e, S »)
    CallArgs := TGocciaArgumentsCollection.Create([S.FItems[I], S.FItems[I], AThisValue]);
    try
      InvokeCallable(Callback, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      CallArgs.Free;
    end;
  end;
  // Step 9: Return undefined
end;

// ES2026 §24.2.3.10 Set.prototype.values()
function TGocciaSetValue.SetValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Let S be the this value
  // Step 2: Return CreateSetIterator(S, value)
  Result := TGocciaSetIteratorValue.Create(AThisValue, skValues);
end;

// ES2026 §24.2.3.8 Set.prototype.keys()
function TGocciaSetValue.SetKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: This method is the same function as Set.prototype.values (per spec)
  // Step 2: Return CreateSetIterator(S, value)
  Result := TGocciaSetIteratorValue.Create(AThisValue, skValues);
end;

// ES2026 §24.2.3.5 Set.prototype.entries()
function TGocciaSetValue.SetEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Let S be the this value
  // Step 2: Return CreateSetIterator(S, key+value)
  Result := TGocciaSetIteratorValue.Create(AThisValue, skEntries);
end;

// ES2026 §24.2.3.11 Set.prototype[@@iterator]()
function TGocciaSetValue.SetSymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: This method is the same function as Set.prototype.values (per spec)
  Result := TGocciaSetIteratorValue.Create(AThisValue, skValues);
end;

// ES2026 §24.2.3.12 Set.prototype.union(other)
function TGocciaSetValue.SetUnion(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSet, OtherSet, ResultSet: TGocciaSetValue;
  I: Integer;
begin
  // Step 1: Let O be the this value
  ThisSet := TGocciaSetValue(AThisValue);
  // Steps 2-3 (implicit): Require O has [[SetData]] internal slot
  // Step 4: Let otherRec be GetSetRecord(other)
  OtherSet := TGocciaSetValue(AArgs.GetElement(0));
  // Step 5: Let resultSetData be a copy of O.[[SetData]]
  ResultSet := TGocciaSetValue.Create;

  for I := 0 to ThisSet.FItems.Count - 1 do
    ResultSet.AddItem(ThisSet.FItems[I]);
  // Step 6: Let keysIter be GetIteratorFromSetLike(otherRec)
  // Step 7: For each element nextValue from keysIter, do
  //   If nextValue is not already in resultSetData, append nextValue
  for I := 0 to OtherSet.FItems.Count - 1 do
    ResultSet.AddItem(OtherSet.FItems[I]);

  // Step 8: Let result be OrdinaryObjectCreate(SetPrototype, « [[SetData]] »)
  // Step 9: Set result.[[SetData]] to resultSetData
  // Step 10: Return result
  Result := ResultSet;
end;

// ES2026 §24.2.3.7 Set.prototype.intersection(other)
function TGocciaSetValue.SetIntersection(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSet, OtherSet, ResultSet: TGocciaSetValue;
  I: Integer;
begin
  // Step 1: Let O be the this value
  ThisSet := TGocciaSetValue(AThisValue);
  // Steps 2-3 (implicit): Require O has [[SetData]] internal slot
  // Step 4: Let otherRec be GetSetRecord(other)
  OtherSet := TGocciaSetValue(AArgs.GetElement(0));
  // Step 5: Let resultSetData be a new empty List
  ResultSet := TGocciaSetValue.Create;

  // Step 6: For each element e of O.[[SetData]], do
  for I := 0 to ThisSet.FItems.Count - 1 do
    // Step 6a: If e is not empty and SetDataHas(otherRec, e) is true, append e
    if OtherSet.ContainsValue(ThisSet.FItems[I]) then
      ResultSet.AddItem(ThisSet.FItems[I]);

  // Step 7: Let result be OrdinaryObjectCreate(SetPrototype, « [[SetData]] »)
  // Step 8: Set result.[[SetData]] to resultSetData
  // Step 9: Return result
  Result := ResultSet;
end;

// ES2026 §24.2.3.3 Set.prototype.difference(other)
function TGocciaSetValue.SetDifference(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSet, OtherSet, ResultSet: TGocciaSetValue;
  I: Integer;
begin
  // Step 1: Let O be the this value
  ThisSet := TGocciaSetValue(AThisValue);
  // Steps 2-3 (implicit): Require O has [[SetData]] internal slot
  // Step 4: Let otherRec be GetSetRecord(other)
  OtherSet := TGocciaSetValue(AArgs.GetElement(0));
  // Step 5: Let resultSetData be a copy of O.[[SetData]]
  ResultSet := TGocciaSetValue.Create;

  // Step 6: For each element e of resultSetData, do
  for I := 0 to ThisSet.FItems.Count - 1 do
    // Step 6a: If e is not empty and SetDataHas(otherRec, e) is true, remove e
    if not OtherSet.ContainsValue(ThisSet.FItems[I]) then
      ResultSet.AddItem(ThisSet.FItems[I]);

  // Step 7: Let result be OrdinaryObjectCreate(SetPrototype, « [[SetData]] »)
  // Step 8: Set result.[[SetData]] to resultSetData
  // Step 9: Return result
  Result := ResultSet;
end;

// ES2026 §24.2.3.9 Set.prototype.symmetricDifference(other)
function TGocciaSetValue.SetSymmetricDifference(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSet, OtherSet, ResultSet: TGocciaSetValue;
  I: Integer;
begin
  // Step 1: Let O be the this value
  ThisSet := TGocciaSetValue(AThisValue);
  // Steps 2-3 (implicit): Require O has [[SetData]] internal slot
  // Step 4: Let otherRec be GetSetRecord(other)
  OtherSet := TGocciaSetValue(AArgs.GetElement(0));
  // Step 5: Let resultSetData be a copy of O.[[SetData]]
  ResultSet := TGocciaSetValue.Create;

  // Step 6: Remove elements from resultSetData that are in other
  for I := 0 to ThisSet.FItems.Count - 1 do
    if not OtherSet.ContainsValue(ThisSet.FItems[I]) then
      ResultSet.AddItem(ThisSet.FItems[I]);

  // Step 7: Let keysIter be GetIteratorFromSetLike(otherRec)
  // Step 8: For each element nextValue from keysIter, do
  //   If nextValue was in O.[[SetData]], it was already removed; otherwise append it
  for I := 0 to OtherSet.FItems.Count - 1 do
    if not ThisSet.ContainsValue(OtherSet.FItems[I]) then
      ResultSet.AddItem(OtherSet.FItems[I]);

  // Step 9: Let result be OrdinaryObjectCreate(SetPrototype, « [[SetData]] »)
  // Step 10: Set result.[[SetData]] to resultSetData
  // Step 11: Return result
  Result := ResultSet;
end;

// ES2026 §24.2.3.8 Set.prototype.isSubsetOf(other)
function TGocciaSetValue.SetIsSubsetOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSet, OtherSet: TGocciaSetValue;
  I: Integer;
begin
  // Step 1: Let O be the this value
  ThisSet := TGocciaSetValue(AThisValue);
  // Steps 2-3 (implicit): Require O has [[SetData]] internal slot
  // Step 4: Let otherRec be GetSetRecord(other)
  OtherSet := TGocciaSetValue(AArgs.GetElement(0));

  // Step 5: If SetDataSize(O) > otherRec.[[Size]], return false (optimization)
  // Step 6: For each element e of O.[[SetData]], do
  for I := 0 to ThisSet.FItems.Count - 1 do
    // Step 6a: If SetDataHas(otherRec, e) is false, return false
    if not OtherSet.ContainsValue(ThisSet.FItems[I]) then
    begin
      Result := TGocciaBooleanLiteralValue.FalseValue;
      Exit;
    end;

  // Step 7: Return true
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

// ES2026 §24.2.3.9 Set.prototype.isSupersetOf(other)
function TGocciaSetValue.SetIsSupersetOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSet, OtherSet: TGocciaSetValue;
  I: Integer;
begin
  // Step 1: Let O be the this value
  ThisSet := TGocciaSetValue(AThisValue);
  // Steps 2-3 (implicit): Require O has [[SetData]] internal slot
  // Step 4: Let otherRec be GetSetRecord(other)
  OtherSet := TGocciaSetValue(AArgs.GetElement(0));

  // Step 5: If SetDataSize(O) < otherRec.[[Size]], return false (optimization)
  // Step 6: Let keysIter be GetIteratorFromSetLike(otherRec)
  // Step 7: For each element nextValue from keysIter, do
  for I := 0 to OtherSet.FItems.Count - 1 do
    // Step 7a: If SetDataHas(O, nextValue) is false, return false
    if not ThisSet.ContainsValue(OtherSet.FItems[I]) then
    begin
      Result := TGocciaBooleanLiteralValue.FalseValue;
      Exit;
    end;

  // Step 8: Return true
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

// ES2026 §24.2.3.6 Set.prototype.isDisjointFrom(other)
function TGocciaSetValue.SetIsDisjointFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSet, OtherSet: TGocciaSetValue;
  I: Integer;
begin
  // Step 1: Let O be the this value
  ThisSet := TGocciaSetValue(AThisValue);
  // Steps 2-3 (implicit): Require O has [[SetData]] internal slot
  // Step 4: Let otherRec be GetSetRecord(other)
  OtherSet := TGocciaSetValue(AArgs.GetElement(0));

  // Step 5: For each element e of O.[[SetData]], do
  for I := 0 to ThisSet.FItems.Count - 1 do
    // Step 5a: If e is not empty and SetDataHas(otherRec, e) is true, return false
    if OtherSet.ContainsValue(ThisSet.FItems[I]) then
    begin
      Result := TGocciaBooleanLiteralValue.FalseValue;
      Exit;
    end;

  // Step 6: Return true
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

end.
