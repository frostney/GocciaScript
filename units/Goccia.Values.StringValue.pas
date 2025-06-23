unit Goccia.Values.StringValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, SysUtils, Math;

type
  TGocciaStringValue = class(TGocciaValue)
  private
    FValue: string;
  public
    constructor Create(const AValue: string);
    function ToString: string; override;
    function ToBoolean: Boolean; override;
    function ToNumber: Double; override;
    function TypeName: string; override;
    property Value: string read FValue;
  end;

implementation

constructor TGocciaStringValue.Create(const AValue: string);
begin
  FValue := AValue;
end;

function TGocciaStringValue.ToString: string;
begin
  Result := FValue;
end;

function TGocciaStringValue.ToBoolean: Boolean;
begin
  Result := FValue <> '';
end;

function TGocciaStringValue.ToNumber: Double;
begin
  if not TryStrToFloat(FValue, Result) then
    Result := 0.0/0.0;  // Safe calculated NaN
end;

function TGocciaStringValue.TypeName: string;
begin
  Result := 'string';
end;

end.
