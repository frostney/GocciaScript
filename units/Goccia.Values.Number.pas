unit Goccia.Values.Number;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, SysUtils, Math;

type
  TGocciaNumberValue = class(TGocciaValue)
  private
    FValue: Double;
  public
    constructor Create(AValue: Double);
    function ToString: string; override;
    function ToBoolean: Boolean; override;
    function ToNumber: Double; override;
    function TypeName: string; override;
    property Value: Double read FValue;
  end;

implementation

constructor TGocciaNumberValue.Create(AValue: Double);
begin
  FValue := AValue;
end;

function TGocciaNumberValue.ToString: string;
begin
  Result := FloatToStr(FValue);
end;

function TGocciaNumberValue.ToBoolean: Boolean;
begin
  Result := (FValue <> 0) and not IsNaN(FValue);
end;

function TGocciaNumberValue.ToNumber: Double;
begin
  Result := FValue;
end;

function TGocciaNumberValue.TypeName: string;
begin
  Result := 'number';
end;

end.