unit Goccia.SharedPrototype;

{$I Goccia.inc}

interface

uses
  Goccia.Realm,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  // Realm-owned prototype + method host: stored in a TGocciaRealm owned slot, so
  // the realm Frees this on Destroy and our destructor unpins both members below.
  // Built-in value units create one as TGocciaSharedPrototype.Create(Self), where
  // Self is the *current realm's* host instance whose methods back the prototype's
  // members.
  //
  // Member definitions are rebuilt per realm — the units no longer cache them in a
  // cross-realm threadvar (#892 / ADR 0084) — so the host bound into a prototype's
  // method callbacks is always the live, realm-owned host that is released with the
  // realm on Destroy. (Caching a member array across realms, bound to an earlier
  // realm's now-freed host, is what made this unsafe before the per-realm rebuild;
  // Goccia.SharedPrototypeRealmReuse.Test guards against a regression. As defence
  // in depth the callbacks also take their receiver from the JS `this`, not Self.)
  TGocciaSharedPrototype = class
  private
    FPrototype: TGocciaObjectValue;
    FMethodHost: TGocciaValue;
  public
    constructor Create(const AMethodHost: TGocciaValue);
    destructor Destroy; override;
    procedure ExposeOnConstructor(const AConstructor: TGocciaValue);
    property Prototype: TGocciaObjectValue read FPrototype;
  end;

function GetSharedPrototypeFromOwnedSlot(const ARealm: TGocciaRealm;
  const ASlotId: TGocciaRealmOwnedSlotId): TGocciaObjectValue;

implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectPropertyDescriptor;

function GetSharedPrototypeFromOwnedSlot(const ARealm: TGocciaRealm;
  const ASlotId: TGocciaRealmOwnedSlotId): TGocciaObjectValue;
var
  Shared: TGocciaSharedPrototype;
begin
  Result := nil;
  if not Assigned(ARealm) then
    Exit;
  Shared := TGocciaSharedPrototype(ARealm.GetOwnedSlot(ASlotId));
  if Assigned(Shared) then
    Result := Shared.Prototype;
end;

constructor TGocciaSharedPrototype.Create(const AMethodHost: TGocciaValue);
begin
  inherited Create;
  FPrototype := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  FMethodHost := AMethodHost;

  if Assigned(TGarbageCollector.Instance) then
  begin
    TGarbageCollector.Instance.PinObject(FPrototype);
    if Assigned(FMethodHost) then
      TGarbageCollector.Instance.PinObject(FMethodHost);
  end;
end;

destructor TGocciaSharedPrototype.Destroy;
begin
  // Unpin our GC-managed members so the GC can collect them.  This is the
  // counterpart to the pinning done in Create.  The realm calls .Free on us
  // when it's torn down (we live in a TGocciaRealm owned slot), so this is
  // where the unpinning has to happen.
  if Assigned(TGarbageCollector.Instance) then
  begin
    if Assigned(FPrototype) then
      TGarbageCollector.Instance.UnpinObject(FPrototype);
    if Assigned(FMethodHost) then
      TGarbageCollector.Instance.UnpinObject(FMethodHost);
  end;
  inherited;
end;

procedure TGocciaSharedPrototype.ExposeOnConstructor(const AConstructor: TGocciaValue);
begin
  if AConstructor is TGocciaClassValue then
    TGocciaClassValue(AConstructor).ReplacePrototype(FPrototype)
  else if AConstructor is TGocciaObjectValue then
    TGocciaObjectValue(AConstructor).DefineProperty(PROP_PROTOTYPE,
      TGocciaPropertyDescriptorData.Create(FPrototype, []));
  FPrototype.DefineProperty(PROP_CONSTRUCTOR,
    TGocciaPropertyDescriptorData.Create(AConstructor, [pfConfigurable, pfWritable]));
end;

end.
