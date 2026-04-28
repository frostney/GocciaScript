unit Goccia.Values.WeakSetValue;

{$I Goccia.inc}

interface

uses
  HashMap,

  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaWeakSetStorage = THashMap<TGocciaValue, Boolean>;

  TGocciaWeakSetValue = class(TGocciaInstanceValue)
  private
    FItems: TGocciaWeakSetStorage;
    procedure InitializePrototype;
  public
    function WeakSetAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function WeakSetDelete(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function WeakSetHas(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    constructor Create(const AClass: TGocciaClassValue = nil);
    destructor Destroy; override;

    function ContainsValue(const AValue: TGocciaValue): Boolean;
    procedure AddItem(const AValue: TGocciaValue);
    function DeleteItem(const AValue: TGocciaValue): Boolean;

    function ToStringTag: string; override;

    procedure InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); override;
    procedure MarkReferences; override;
    procedure SweepWeakReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);

    property Items: TGocciaWeakSetStorage read FItems;
  end;

implementation

uses
  SysUtils,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Realm,
  Goccia.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.IteratorSupport,
  Goccia.Values.IteratorValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.WeakReferenceSupport;

var
  GWeakSetSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetWeakSetShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GWeakSetSharedSlot))
  else
    Result := nil;
end;

constructor TGocciaWeakSetValue.Create(const AClass: TGocciaClassValue);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(AClass);
  FItems := TGocciaWeakSetStorage.Create;
  InitializePrototype;
  Shared := GetWeakSetShared;
  if not Assigned(AClass) and Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

destructor TGocciaWeakSetValue.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TGocciaWeakSetValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetWeakSetShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GWeakSetSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddNamedMethod('add', WeakSetAdd, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('delete', WeakSetDelete, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('has', WeakSetHas, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create(CONSTRUCTOR_WEAK_SET),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaWeakSetValue.ExposePrototype(
  const AConstructor: TGocciaValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetWeakSetShared;
  if not Assigned(Shared) then
  begin
    TGocciaWeakSetValue.Create;
    Shared := GetWeakSetShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaWeakSetValue.ContainsValue(const AValue: TGocciaValue): Boolean;
begin
  Result := FItems.ContainsKey(AValue);
end;

procedure TGocciaWeakSetValue.AddItem(const AValue: TGocciaValue);
begin
  FItems.AddOrSetValue(AValue, True);
end;

function TGocciaWeakSetValue.DeleteItem(const AValue: TGocciaValue): Boolean;
begin
  Result := FItems.Remove(AValue);
end;

function TGocciaWeakSetValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_WEAK_SET;
end;

procedure TGocciaWeakSetValue.InitializeNativeFromArguments(
  const AArguments: TGocciaArgumentsCollection);
var
  InitArg, Adder, NextValue: TGocciaValue;
  Iterator: TGocciaIteratorValue;
  CallArgs: TGocciaArgumentsCollection;
  Done: Boolean;
  GC: TGarbageCollector;
  WasSelfRooted, WasAdderRooted, WasIteratorRooted, WasNextRooted: Boolean;

  function AddRootIfNeeded(const AValue: TGocciaValue): Boolean;
  begin
    Result := Assigned(GC) and Assigned(AValue) and not GC.IsTempRoot(AValue);
    if Result then
      GC.AddTempRoot(AValue);
  end;

  procedure RemoveRootIfNeeded(const AValue: TGocciaValue; const AWasAdded: Boolean);
  begin
    if AWasAdded then
      GC.RemoveTempRoot(AValue);
  end;

begin
  if AArguments.Length = 0 then
    Exit;
  InitArg := AArguments.GetElement(0);
  if (InitArg is TGocciaUndefinedLiteralValue) or
     (InitArg is TGocciaNullLiteralValue) then
    Exit;

  GC := TGarbageCollector.Instance;
  WasSelfRooted := AddRootIfNeeded(Self);
  try
    Adder := GetProperty('add');
    if not Assigned(Adder) or not Adder.IsCallable then
      ThrowTypeError(Format(SErrorValueNotFunction, ['add']), SSuggestWeakSetThisType);

    WasAdderRooted := AddRootIfNeeded(Adder);
    try
      Iterator := GetIteratorFromValue(InitArg);
      if not Assigned(Iterator) then
        ThrowTypeError(Format(SErrorWeakCollectionConstructorNotIterable, [CONSTRUCTOR_WEAK_SET]), SSuggestNotIterable);

      WasIteratorRooted := AddRootIfNeeded(Iterator);
      try
        try
          NextValue := Iterator.DirectNext(Done);
          while not Done do
          begin
            WasNextRooted := AddRootIfNeeded(NextValue);
            try
              CallArgs := TGocciaArgumentsCollection.Create([NextValue]);
              try
                InvokeCallable(Adder, CallArgs, Self);
              finally
                CallArgs.Free;
              end;
            finally
              RemoveRootIfNeeded(NextValue, WasNextRooted);
            end;
            NextValue := Iterator.DirectNext(Done);
          end;
        except
          CloseIteratorPreservingError(Iterator);
          raise;
        end;
      finally
        RemoveRootIfNeeded(Iterator, WasIteratorRooted);
      end;
    finally
      RemoveRootIfNeeded(Adder, WasAdderRooted);
    end;
  finally
    RemoveRootIfNeeded(Self, WasSelfRooted);
  end;
end;

procedure TGocciaWeakSetValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
end;

procedure TGocciaWeakSetValue.SweepWeakReferences;
var
  Keys: TGocciaWeakSetStorage.TKeyArray;
  I: Integer;
begin
  Keys := FItems.Keys;
  for I := 0 to Length(Keys) - 1 do
    if Assigned(Keys[I]) and not Keys[I].GCMarked then
      FItems.Remove(Keys[I]);
end;

function TGocciaWeakSetValue.WeakSetAdd(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  S: TGocciaWeakSetValue;
  Value: TGocciaValue;
begin
  if not (AThisValue is TGocciaWeakSetValue) then
    ThrowTypeError(SErrorWeakSetAddNonWeakSet, SSuggestWeakSetThisType);
  S := TGocciaWeakSetValue(AThisValue);
  Value := AArgs.GetElement(0);
  RequireCanBeHeldWeakly(Value, 'WeakSet.prototype.add');
  S.AddItem(Value);
  Result := S;
end;

function TGocciaWeakSetValue.WeakSetDelete(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  S: TGocciaWeakSetValue;
  Value: TGocciaValue;
begin
  if not (AThisValue is TGocciaWeakSetValue) then
    ThrowTypeError(SErrorWeakSetDeleteNonWeakSet, SSuggestWeakSetThisType);
  S := TGocciaWeakSetValue(AThisValue);
  Value := AArgs.GetElement(0);
  if not CanBeHeldWeakly(Value) then
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  if S.DeleteItem(Value) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaWeakSetValue.WeakSetHas(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  S: TGocciaWeakSetValue;
  Value: TGocciaValue;
begin
  if not (AThisValue is TGocciaWeakSetValue) then
    ThrowTypeError(SErrorWeakSetHasNonWeakSet, SSuggestWeakSetThisType);
  S := TGocciaWeakSetValue(AThisValue);
  Value := AArgs.GetElement(0);
  if CanBeHeldWeakly(Value) and S.ContainsValue(Value) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

initialization
  GWeakSetSharedSlot := RegisterRealmOwnedSlot('WeakSet.shared');

end.
