unit Goccia.Values.BooleanObjectValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaBooleanObjectValue = class(TGocciaObjectValue)
  private
    FPrimitive: TGocciaBooleanLiteralValue;
  public
    constructor Create(const APrimitive: TGocciaBooleanLiteralValue);
    procedure GCMarkReferences; override;
    property Primitive: TGocciaBooleanLiteralValue read FPrimitive;
  end;

implementation

constructor TGocciaBooleanObjectValue.Create(const APrimitive: TGocciaBooleanLiteralValue);
begin
  inherited Create;
  FPrimitive := APrimitive;
end;

procedure TGocciaBooleanObjectValue.GCMarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FPrimitive) then
    FPrimitive.GCMarkReferences;
end;

end.