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
  GarbageCollector.Generic,

  Goccia.Constants.PropertyNames,
  Goccia.Values.ClassValue;

constructor TGocciaSharedPrototype.Create(const AMethodHost: TGocciaValue);
begin
  inherited Create;
  FPrototype := TGocciaObjectValue.Create;
  FMethodHost := AMethodHost;

  if Assigned(TGenericGarbageCollector.Instance) then
  begin
    TGenericGarbageCollector.Instance.PinObject(FPrototype);
    if Assigned(FMethodHost) then
      TGenericGarbageCollector.Instance.PinObject(FMethodHost);
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
