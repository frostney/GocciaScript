unit Goccia.Values.BooleanValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base;

type
  TGocciaBooleanValue = class(TGocciaValue)
  private
    FValue: Boolean;
  public
    constructor Create(AValue: Boolean);
    function ToString: string; override;
    function ToBoolean: Boolean; override;
    function ToNumber: Double; override;
    function TypeName: string; override;
    property Value: Boolean read FValue;
  end;

implementation

constructor TGocciaBooleanValue.Create(AValue: Boolean);
begin
  FValue := AValue;
end;

function TGocciaBooleanValue.ToString: string;
begin
  if FValue then
    Result := 'true'
  else
    Result := 'false';
end;

function TGocciaBooleanValue.ToBoolean: Boolean;
begin
  Result := FValue;
end;

function TGocciaBooleanValue.ToNumber: Double;
begin
  if FValue then
    Result := 1
  else
    Result := 0;
end;

function TGocciaBooleanValue.TypeName: string;
begin
  Result := 'boolean';
end;

end.
