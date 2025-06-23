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
    FIsNegativeZero: Boolean;
    FIsInfinity: Boolean;
    FIsNegativeInfinity: Boolean;
  public
    constructor Create(AValue: Double);
    class function CreateNaN: TGocciaNumberValue;
    class function CreateNegativeZero: TGocciaNumberValue;
    class function CreateInfinity: TGocciaNumberValue;
    class function CreateNegativeInfinity: TGocciaNumberValue;
    function ToString: string; override;
    function ToBoolean: Boolean; override;
    function ToNumber: Double; override;
    function TypeName: string; override;

    property Value: Double read FValue;
    property IsNegativeZero: Boolean read FIsNegativeZero;
    property IsNaN: Boolean read FIsNaN;
    property IsInfinity: Boolean read FIsInfinity;
    property IsNegativeInfinity: Boolean read FIsNegativeInfinity;
  end;

implementation

constructor TGocciaNumberValue.Create(AValue: Double);
begin
  // Check if the input is NaN without storing it
  if Math.IsNaN(AValue) then
  begin
    FIsNaN := True;
    FIsNegativeZero := False;
    FIsInfinity := False;
    FIsNegativeInfinity := False;
    FValue := 0.0; // Store a safe value instead of NaN
  end
  else if IsInfinite(AValue) then
  begin
    FIsNaN := False;
    FIsNegativeZero := False;
    FIsInfinity := AValue > 0;
    FIsNegativeInfinity := AValue < 0;
    FValue := 0.0; // Store a safe value instead of infinity
  end
  else
  begin
    FIsNaN := False;
    FIsNegativeZero := False;
    FIsInfinity := False;
    FIsNegativeInfinity := False;
    FValue := AValue;
  end;
end;

class function TGocciaNumberValue.CreateNaN: TGocciaNumberValue;
begin
  Result := TGocciaNumberValue.Create(0.0);
  Result.FIsNaN := True;
end;

class function TGocciaNumberValue.CreateNegativeZero: TGocciaNumberValue;
begin
  Result := TGocciaNumberValue.Create(0.0);
  Result.FIsNegativeZero := True;
end;

class function TGocciaNumberValue.CreateInfinity: TGocciaNumberValue;
begin
  Result := TGocciaNumberValue.Create(0.0);
  Result.FIsInfinity := True;
end;

class function TGocciaNumberValue.CreateNegativeInfinity: TGocciaNumberValue;
begin
  Result := TGocciaNumberValue.Create(0.0);
  Result.FIsNegativeInfinity := True;
end;

function TGocciaNumberValue.ToString: string;
begin
  if FIsNaN then
    Result := 'NaN'
  else if FIsInfinity then
    Result := 'Infinity'
  else if FIsNegativeInfinity then
    Result := '-Infinity'
  else if (FValue = 0.0) then
  begin
    if FIsNegativeZero then
      Result := '-0'
    else
      Result := '0';
  end
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
  else if FIsInfinity then
    Result := 1.0/0.0  // Return positive infinity
  else if FIsNegativeInfinity then
    Result := -1.0/0.0  // Return negative infinity
  else
    Result := FValue;
end;

function TGocciaNumberValue.TypeName: string;
begin
  Result := 'number';
end;

end.
