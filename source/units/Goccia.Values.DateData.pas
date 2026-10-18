unit Goccia.Values.DateData;

{$I Goccia.inc}

{ Native access to the [[DateValue]] internal slot.

  The legacy Date constructor is a JavaScript shim (Goccia.Shims), and it keeps
  each instance's time value in a module-private WeakMap keyed by the instance.
  That WeakMap is the slot: a Date that loses its prototype is still a Date, and
  an object that merely inherits from Date.prototype is not one. Native code
  that must recognize a Date the way ECMA-262 does (Object.prototype.toString,
  structuredClone) asks here instead of reading the prototype chain.

  The shim registers its WeakMap for the current realm when it is first
  evaluated. Until then no Date can exist in that realm, so every query answers
  False without a lookup.

  A Date of another realm has the slot too: `new Date(otherRealmDate)`
  copies its time value (ES2026 §21.4.2.1 Date step 4.b), and each
  Date.prototype method accepts it (RequireInternalSlot(dateObject,
  [[DateValue]]), as in §21.4.4.10 getTime). Each realm that registered a
  store is kept in a per-thread list until the realm is destroyed, so a query
  falls back to the other realms' stores, and the shim reaches them through
  the two functions CreateForeignDateValueFunctions makes. }

interface

uses
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

{ Records AStore, the shim's slot WeakMap, for the current realm. }
procedure RegisterDateValueStore(const AStore: TGocciaValue);

{ Records AConstructor, the realm's %Date%, for CreateDateObject. }
procedure RegisterDateConstructor(const AConstructor: TGocciaValue);

{ True when AObject has a [[DateValue]] slot in the current realm. }
function HasDateValue(const AObject: TGocciaObjectValue): Boolean;

{ Reads AObject's [[DateValue]] into ATimeValue (NaN for an invalid Date).
  False, leaving ATimeValue NaN, when AObject is not a Date. }
function TryGetDateValue(const AObject: TGocciaObjectValue;
  out ATimeValue: Double): Boolean;

{ A new Date of the current realm holding ATimeValue: an instance of %Date%
  with %Date.prototype% and ATimeValue in its [[DateValue]] slot. It is built
  natively, so no script-visible function (the shim's constructor body,
  Math.trunc, WeakMap.prototype.set) runs. Only valid once the realm's Date
  shim has been evaluated, which any existing Date guarantees; nil before. }
function CreateDateObject(const ATimeValue: Double): TGocciaObjectValue;

{ The Date shim's view of the other realms' [[DateValue]] slots:
  AGetter(obj) is obj's time value in the store of the realm that holds it,
  or undefined; ASetter(obj, value) stores value there and answers true, or
  answers false when no realm's store holds obj. }
procedure CreateForeignDateValueFunctions(out AGetter, ASetter: TGocciaValue);

implementation

uses
  Math,

  Goccia.Arguments.Collection,
  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.Realm,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.WeakMapValue;

type
  { Owned by a realm that registered a Date store: its destruction, with the
    realm's, takes the realm off the thread's list. }
  TGocciaDateRealmRegistration = class
  private
    FRealm: TGocciaRealm;
  public
    constructor Create(const ARealm: TGocciaRealm);
    destructor Destroy; override;
  end;

  TGocciaForeignDateValueHost = class
  public
    function GetValue(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function SetValue(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

var
  GDateValueStoreSlot: TGocciaRealmSlotId;
  GDateConstructorSlot: TGocciaRealmSlotId;
  GDateRegistrationSlot: TGocciaRealmOwnedSlotId;
  GForeignDateValueHost: TGocciaForeignDateValueHost;

threadvar
  { The live realms of this thread that registered a Date store. }
  GDateRealms: array of TGocciaRealm;

constructor TGocciaDateRealmRegistration.Create(const ARealm: TGocciaRealm);
begin
  inherited Create;
  FRealm := ARealm;
  SetLength(GDateRealms, Length(GDateRealms) + 1);
  GDateRealms[High(GDateRealms)] := ARealm;
end;

destructor TGocciaDateRealmRegistration.Destroy;
var
  I, J: Integer;
begin
  for I := 0 to High(GDateRealms) do
    if GDateRealms[I] = FRealm then
    begin
      for J := I to High(GDateRealms) - 1 do
        GDateRealms[J] := GDateRealms[J + 1];
      SetLength(GDateRealms, Length(GDateRealms) - 1);
      Break;
    end;
  inherited;
end;

function RealmDateValueStore(
  const ARealm: TGocciaRealm): TGocciaWeakMapValue; {$IFDEF FPC}inline;{$ENDIF}
var
  Store: TObject;
begin
  Store := ARealm.GetSlot(GDateValueStoreSlot);
  if Store is TGocciaWeakMapValue then
    Result := TGocciaWeakMapValue(Store)
  else
    Result := nil;
end;

{ The store, of any realm on this thread, that holds AObject's
  [[DateValue]], the current realm's first; nil when AObject is not a Date. }
function FindDateValueStore(const AObject: TGocciaValue): TGocciaWeakMapValue;
var
  I: Integer;
  Realm: TGocciaRealm;
begin
  Realm := CurrentRealm;
  if Assigned(Realm) then
  begin
    Result := RealmDateValueStore(Realm);
    if Assigned(Result) and Result.HasEntry(AObject) then
      Exit;
  end;
  for I := 0 to High(GDateRealms) do
    if GDateRealms[I] <> Realm then
    begin
      Result := RealmDateValueStore(GDateRealms[I]);
      if Assigned(Result) and Result.HasEntry(AObject) then
        Exit;
    end;
  Result := nil;
end;

function TGocciaForeignDateValueHost.GetValue(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Store: TGocciaWeakMapValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if (AArgs.Length = 0) or not (AArgs.GetElement(0) is TGocciaObjectValue) then
    Exit;
  Store := FindDateValueStore(AArgs.GetElement(0));
  if Assigned(Store) then
    Store.TryGetEntry(AArgs.GetElement(0), Result);
end;

function TGocciaForeignDateValueHost.SetValue(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Store: TGocciaWeakMapValue;
begin
  Result := TGocciaBooleanLiteralValue.FalseValue;
  if (AArgs.Length < 2) or not (AArgs.GetElement(0) is TGocciaObjectValue) then
    Exit;
  Store := FindDateValueStore(AArgs.GetElement(0));
  if not Assigned(Store) then
    Exit;
  Store.SetEntry(AArgs.GetElement(0), AArgs.GetElement(1));
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

procedure CreateForeignDateValueFunctions(out AGetter, ASetter: TGocciaValue);
begin
  AGetter := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    GForeignDateValueHost.GetValue, '__GocciaForeignDateValue', 1);
  ASetter := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    GForeignDateValueHost.SetValue, '__GocciaSetForeignDateValue', 2);
end;

function CurrentDateValueStore: TGocciaWeakMapValue; {$IFDEF FPC}inline;{$ENDIF}
var
  Store: TObject;
begin
  Result := nil;
  if CurrentRealm = nil then
    Exit;
  Store := CurrentRealm.GetSlot(GDateValueStoreSlot);
  if Store is TGocciaWeakMapValue then
    Result := TGocciaWeakMapValue(Store);
end;

procedure RegisterDateValueStore(const AStore: TGocciaValue);
begin
  if (CurrentRealm <> nil) and (AStore is TGocciaWeakMapValue) then
  begin
    CurrentRealm.SetSlot(GDateValueStoreSlot, AStore);
    if not Assigned(CurrentRealm.GetOwnedSlot(GDateRegistrationSlot)) then
      CurrentRealm.SetOwnedSlot(GDateRegistrationSlot,
        TGocciaDateRealmRegistration.Create(CurrentRealm));
  end;
end;

procedure RegisterDateConstructor(const AConstructor: TGocciaValue);
begin
  if (CurrentRealm <> nil) and Assigned(AConstructor) then
    CurrentRealm.SetSlot(GDateConstructorSlot, AConstructor);
end;

function HasDateValue(const AObject: TGocciaObjectValue): Boolean;
begin
  Result := (Length(GDateRealms) > 0) and
    Assigned(FindDateValueStore(AObject));
end;

function TryGetDateValue(const AObject: TGocciaObjectValue;
  out ATimeValue: Double): Boolean;
var
  Store: TGocciaWeakMapValue;
  Slot: TGocciaValue;
begin
  ATimeValue := NaN;
  if Length(GDateRealms) = 0 then
    Exit(False);
  Store := FindDateValueStore(AObject);
  Result := Assigned(Store) and Store.TryGetEntry(AObject, Slot) and
    (Slot is TGocciaNumberLiteralValue);
  if Result then
    ATimeValue := TGocciaNumberLiteralValue(Slot).Value;
end;

function CreateDateObject(const ATimeValue: Double): TGocciaObjectValue;
var
  DateConstructor: TObject;
  Descriptor: TGocciaPropertyDescriptor;
  Prototype: TGocciaObjectValue;
  Store: TGocciaWeakMapValue;
  Root: TGocciaTempRoot;
begin
  Result := nil;
  Store := CurrentDateValueStore;
  if not Assigned(Store) then
    Exit;
  DateConstructor := CurrentRealm.GetSlot(GDateConstructorSlot);
  if not (DateConstructor is TGocciaObjectValue) then
    Exit;
  // %Date%.prototype is a non-writable, non-configurable data property, so
  // reading its descriptor runs nothing and always finds %Date.prototype%.
  Descriptor := TGocciaObjectValue(DateConstructor).GetOwnPropertyDescriptor(
    PROP_PROTOTYPE);
  if (Descriptor is TGocciaPropertyDescriptorData) and
     (TGocciaPropertyDescriptorData(Descriptor).Value is TGocciaObjectValue) then
    Prototype := TGocciaObjectValue(
      TGocciaPropertyDescriptorData(Descriptor).Value)
  else
    Prototype := nil;
  ReleaseDescriptor(Descriptor);
  if not Assigned(Prototype) then
    Exit;
  // What `new Date(t)` builds before its body runs: an ordinary object with
  // %Date.prototype%. The body's only lasting effect is the slot store.
  Result := TGocciaObjectValue.Create(Prototype);
  InitializeTempRoot(Root);
  AddTempRootIfNeeded(Root, Result);
  try
    Store.SetEntry(Result, TGocciaNumberLiteralValue.Create(ATimeValue));
  finally
    RemoveTempRootIfNeeded(Root);
  end;
end;

initialization
  GDateValueStoreSlot := RegisterRealmSlot('%DateValueStore%');
  GDateConstructorSlot := RegisterRealmSlot('%DateConstructor%');
  GDateRegistrationSlot := RegisterRealmOwnedSlot('%DateRealmRegistration%');
  GForeignDateValueHost := TGocciaForeignDateValueHost.Create;

finalization
  GForeignDateValueHost.Free;

end.
