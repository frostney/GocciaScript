unit Goccia.Values.NumberValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, SysUtils, Math;

type
  TGocciaNumberValue = class(TGocciaValue)
  private
    FValue: Double;
    FIsNaN: Boolean;
  public
    constructor Create(AValue: Double);
    class function CreateNaN: TGocciaNumberValue;
    function ToString: string; override;
    function ToBoolean: Boolean; override;
    function ToNumber: Double; override;
    function TypeName: string; override;
    function IsNaN: Boolean;
    property Value: Double read FValue;
  end;

implementation

constructor TGocciaNumberValue.Create(AValue: Double);
begin
  // Check if the input is NaN without storing it
  if Math.IsNaN(AValue) then
  begin
    FIsNaN := True;
    FValue := 0.0; // Store a safe value instead of NaN
  end
  else
  begin
    FIsNaN := False;
    FValue := AValue;
  end;
end;

class function TGocciaNumberValue.CreateNaN: TGocciaNumberValue;
begin
  Result := TGocciaNumberValue.Create(0.0);
  Result.FIsNaN := True;
end;

function TGocciaNumberValue.ToString: string;
begin
  if FIsNaN then
    Result := 'NaN'
  else if (FValue = 0.0) then
    Result := '0'
  else if IsInfinite(FValue) then
  begin
    if FValue > 0 then
      Result := 'Infinity'
    else
      Result := '-Infinity';
  end
  else
    Result := FloatToStr(FValue);
end;

function TGocciaNumberValue.ToBoolean: Boolean;
begin
  if FIsNaN then
    Result := False
  else
    Result := (FValue <> 0);
end;

function TGocciaNumberValue.ToNumber: Double;
begin
  if FIsNaN then
    Result := 0.0/0.0  // Return a calculated NaN, not stored one
  else
    Result := FValue;
end;

function TGocciaNumberValue.IsNaN: Boolean;
begin
  Result := FIsNaN;
end;

function TGocciaNumberValue.TypeName: string;
begin
  Result := 'number';
end;

end.
