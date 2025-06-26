unit Goccia.Values.BooleanObjectValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Core, Goccia.Values.Primitives, Goccia.Values.ObjectValue;

type
  TGocciaBooleanObjectValue = class(TGocciaObjectValue)
  private
    FPrimitive: TGocciaBooleanLiteralValue;
  public
    constructor Create(APrimitive: TGocciaBooleanLiteralValue);
    property Primitive: TGocciaBooleanLiteralValue read FPrimitive;
  end;

implementation

constructor TGocciaBooleanObjectValue.Create(APrimitive: TGocciaBooleanLiteralValue);
begin
  inherited Create;
  FPrimitive := APrimitive;
end;

end.