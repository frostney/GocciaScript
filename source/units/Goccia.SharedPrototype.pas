unit Goccia.SharedPrototype;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
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
  FPrototype := TGocciaObjectValue.Create;
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
    TGocciaObjectValue(AConstructor).AssignProperty(PROP_PROTOTYPE, FPrototype);
  FPrototype.DefineProperty(PROP_CONSTRUCTOR,
    TGocciaPropertyDescriptorData.Create(AConstructor, [pfConfigurable, pfWritable]));
end;

end.
