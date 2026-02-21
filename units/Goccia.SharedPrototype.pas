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
    procedure ExposeOnConstructor(const AConstructor: TGocciaValue);
    property Prototype: TGocciaObjectValue read FPrototype;
  end;

implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.Values.ClassValue;

constructor TGocciaSharedPrototype.Create(const AMethodHost: TGocciaValue);
begin
  inherited Create;
  FPrototype := TGocciaObjectValue.Create;
  FMethodHost := AMethodHost;

  if Assigned(TGocciaGarbageCollector.Instance) then
  begin
    TGocciaGarbageCollector.Instance.PinValue(FPrototype);
    if Assigned(FMethodHost) then
      TGocciaGarbageCollector.Instance.PinValue(FMethodHost);
  end;
end;

procedure TGocciaSharedPrototype.ExposeOnConstructor(const AConstructor: TGocciaValue);
begin
  if AConstructor is TGocciaClassValue then
    TGocciaClassValue(AConstructor).ReplacePrototype(FPrototype)
  else if AConstructor is TGocciaObjectValue then
    TGocciaObjectValue(AConstructor).AssignProperty(PROP_PROTOTYPE, FPrototype);
  FPrototype.AssignProperty(PROP_CONSTRUCTOR, AConstructor);
end;

end.
