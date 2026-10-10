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
  False without a lookup. }

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

implementation

uses
  Math,

  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.Realm,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.WeakMapValue;

var
  GDateValueStoreSlot: TGocciaRealmSlotId;
  GDateConstructorSlot: TGocciaRealmSlotId;

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
    CurrentRealm.SetSlot(GDateValueStoreSlot, AStore);
end;

procedure RegisterDateConstructor(const AConstructor: TGocciaValue);
begin
  if (CurrentRealm <> nil) and Assigned(AConstructor) then
    CurrentRealm.SetSlot(GDateConstructorSlot, AConstructor);
end;

function HasDateValue(const AObject: TGocciaObjectValue): Boolean;
var
  Store: TGocciaWeakMapValue;
begin
  Store := CurrentDateValueStore;
  Result := Assigned(Store) and Store.HasEntry(AObject);
end;

function TryGetDateValue(const AObject: TGocciaObjectValue;
  out ATimeValue: Double): Boolean;
var
  Store: TGocciaWeakMapValue;
  Slot: TGocciaValue;
begin
  ATimeValue := NaN;
  Store := CurrentDateValueStore;
  Result := Assigned(Store) and Store.TryGetEntry(AObject, Slot) and
    (Slot is TGocciaNumberLiteralValue);
  if Result then
    ATimeValue := TGocciaNumberLiteralValue(Slot).Value;
end;

function CreateDateObject(const ATimeValue: Double): TGocciaObjectValue;
var
  DateConstructor: TObject;
  Descriptor: TGocciaPropertyDescriptor;
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
  if not (Descriptor is TGocciaPropertyDescriptorData) or
     not (TGocciaPropertyDescriptorData(Descriptor).Value is
       TGocciaObjectValue) then
    Exit;
  // What `new Date(t)` builds before its body runs: an ordinary object with
  // %Date.prototype%. The body's only lasting effect is the slot store.
  Result := TGocciaObjectValue.Create(TGocciaObjectValue(
    TGocciaPropertyDescriptorData(Descriptor).Value));
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

end.
