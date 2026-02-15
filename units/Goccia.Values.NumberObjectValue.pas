unit Goccia.Values.NumberObjectValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Goccia.Values.ObjectValue;

type
  TGocciaNumberObjectValue = class(TGocciaObjectValue)
  private
    FPrimitive: TGocciaNumberLiteralValue;
  public
    constructor Create(APrimitive: TGocciaNumberLiteralValue);
    procedure GCMarkReferences; override;
    property Primitive: TGocciaNumberLiteralValue read FPrimitive;
  end;

implementation

constructor TGocciaNumberObjectValue.Create(APrimitive: TGocciaNumberLiteralValue);
begin
  inherited Create;
  FPrimitive := APrimitive;
end;

procedure TGocciaNumberObjectValue.GCMarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FPrimitive) then
    FPrimitive.GCMarkReferences;
end;

end.