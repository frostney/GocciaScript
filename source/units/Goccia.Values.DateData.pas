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

{ A new Date of the current realm holding ATimeValue, built by %Date% (so its
  prototype is %Date.prototype%). Only valid once the realm's Date shim has
  been evaluated, which any existing Date guarantees; nil before that. }
function CreateDateObject(const ATimeValue: Double): TGocciaObjectValue;

implementation

uses
  Math,

  Goccia.Arguments.Collection,
  Goccia.Realm,
  Goccia.Values.FunctionBase,
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
  Arguments: TGocciaArgumentsCollection;
  Constructed: TGocciaValue;
  DateConstructor: TObject;
begin
  Result := nil;
  if CurrentRealm = nil then
    Exit;
  DateConstructor := CurrentRealm.GetSlot(GDateConstructorSlot);
  if not (DateConstructor is TGocciaValue) then
    Exit;
  // new Date(t) stores TimeClip(t), which is t itself for any time value a
  // Date already holds, NaN included.
  Arguments := TGocciaArgumentsCollection.Create(
    [TGocciaNumberLiteralValue.Create(ATimeValue)]);
  try
    Constructed := ConstructValue(TGocciaValue(DateConstructor), Arguments,
      TGocciaValue(DateConstructor));
  finally
    Arguments.Free;
  end;
  if Constructed is TGocciaObjectValue then
    Result := TGocciaObjectValue(Constructed);
end;

initialization
  GDateValueStoreSlot := RegisterRealmSlot('%DateValueStore%');
  GDateConstructorSlot := RegisterRealmSlot('%DateConstructor%');

end.
