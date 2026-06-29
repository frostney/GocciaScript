unit Goccia.SharedPrototype;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  // Realm-owned prototype + method host: stored in a TGocciaRealm owned slot, so
  // the realm Frees this on Destroy and our destructor unpins both members below.
  // Built-in value units create one as TGocciaSharedPrototype.Create(Self), where
  // Self is a per-realm host instance whose methods back the prototype's members.
  //
  // INVARIANT (#892 / ADR 0084): a bound prototype callback must derive its
  // receiver from the JS `this` (AThisValue) and must never dereference the bound
  // host (Self). Many units cache their member definitions cross-realm (guarded by
  // `if Length(FPrototypeMembers) = 0`), so a later realm reuses callbacks still
  // bound to an earlier realm's host — which has since been unpinned and may be
  // freed. Because those callbacks only ever touch AThisValue (and none are
  // virtual), the freed host is passed as Self but never read, so the reuse is
  // safe. A callback that read Self or a bare instance field/method would turn it
  // into a use-after-free. Goccia.SharedPrototypeRealmReuse.Test guards this.
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

implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectPropertyDescriptor;

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
