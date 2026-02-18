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
    procedure ExposeOnConstructor(const AConstructor: TGocciaObjectValue);
    property Prototype: TGocciaObjectValue read FPrototype;
  end;

implementation

uses
  Goccia.GarbageCollector;

constructor TGocciaSharedPrototype.Create(const AMethodHost: TGocciaValue);
begin
  inherited Create;
  FPrototype := TGocciaObjectValue.Create;
  FMethodHost := AMethodHost;

  if Assigned(TGocciaGC.Instance) then
  begin
    TGocciaGC.Instance.PinValue(FPrototype);
    if Assigned(FMethodHost) then
      TGocciaGC.Instance.PinValue(FMethodHost);
  end;
end;

procedure TGocciaSharedPrototype.ExposeOnConstructor(const AConstructor: TGocciaObjectValue);
begin
  AConstructor.AssignProperty('prototype', FPrototype);
  FPrototype.AssignProperty('constructor', AConstructor);
end;

end.
