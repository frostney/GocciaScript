unit Goccia.Values.HoleValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

type
  TGocciaHoleValue = class(TGocciaValue)
  private
    class var FHoleValue: TGocciaHoleValue;
  public
    class function HoleValue: TGocciaHoleValue;

    function IsPrimitive: Boolean; override;
    function TypeName: string; override;
    function TypeOf: string; override;
    function RuntimeCopy: TGocciaValue; override;

    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;
  end;

implementation

uses
  Goccia.Constants.TypeNames;

class function TGocciaHoleValue.HoleValue: TGocciaHoleValue;
begin
  if not Assigned(FHoleValue) then
    FHoleValue := TGocciaHoleValue.Create;
  Result := FHoleValue;
end;

function TGocciaHoleValue.IsPrimitive: Boolean;
begin
  Result := False;
end;

function TGocciaHoleValue.TypeName: string;
begin
  Result := 'hole';
end;

function TGocciaHoleValue.TypeOf: string;
begin
  Result := UNDEFINED_TYPE_NAME;
end;

function TGocciaHoleValue.RuntimeCopy: TGocciaValue;
begin
  Result := HoleValue;
end;

function TGocciaHoleValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaHoleValue.ToNumberLiteral: TGocciaNumberLiteralValue;
begin
  Result := TGocciaNumberLiteralValue.NaNValue;
end;

function TGocciaHoleValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  Result := TGocciaStringLiteralValue.Create('');
end;

end.
