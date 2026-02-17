unit Goccia.SharedPrototype;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Goccia.Values.ObjectValue;

type
  TGocciaSharedPrototype = class
  private
    FPrototype: TGocciaObjectValue;
    FMethodHost: TGocciaValue;
  public
    constructor Create(AMethodHost: TGocciaValue);
    procedure ExposeOnConstructor(AConstructor: TGocciaObjectValue);
    property Prototype: TGocciaObjectValue read FPrototype;
  end;

implementation

uses
  Goccia.GarbageCollector;

constructor TGocciaSharedPrototype.Create(AMethodHost: TGocciaValue);
begin
  inherited Create;
  FPrototype := TGocciaObjectValue.Create;
  FMethodHost := AMethodHost;

  if Assigned(TGocciaGC.Instance) then
  begin
    TGocciaGC.Instance.PinValue(FPrototype);
    TGocciaGC.Instance.PinValue(FMethodHost);
  end;
end;

procedure TGocciaSharedPrototype.ExposeOnConstructor(AConstructor: TGocciaObjectValue);
begin
  AConstructor.AssignProperty('prototype', FPrototype);
  FPrototype.AssignProperty('constructor', AConstructor);
end;

end.
