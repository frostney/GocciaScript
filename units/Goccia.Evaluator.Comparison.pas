unit Goccia.Evaluator.Comparison;

{$I Goccia.inc}

interface

uses
  Math, Goccia.Values.Base, Goccia.Values.UndefinedValue, Goccia.Values.NullValue,
  Goccia.Values.BooleanValue, Goccia.Values.NumberValue, Goccia.Values.StringValue;

function IsStrictEqual(Left, Right: TGocciaValue): Boolean;
function IsNotStrictEqual(Left, Right: TGocciaValue): Boolean; inline;

function GreaterThan(Left, Right: TGocciaValue): Boolean;
function GreaterThanOrEqual(Left, Right: TGocciaValue): Boolean; inline;
function LessThan(Left, Right: TGocciaValue): Boolean;
function LessThanOrEqual(Left, Right: TGocciaValue): Boolean; inline;

implementation

function IsStrictEqual(Left, Right: TGocciaValue): Boolean;
begin
  if (Left is TGocciaUndefinedValue) and (Right is TGocciaUndefinedValue) then
    Result := True
  else if (Left is TGocciaNullValue) and (Right is TGocciaNullValue) then
    Result := True
  else if (Left is TGocciaBooleanValue) and (Right is TGocciaBooleanValue) then
    Result := TGocciaBooleanValue(Left).ToBoolean = TGocciaBooleanValue(Right).ToBoolean
  else if (Left is TGocciaNumberValue) and (Right is TGocciaNumberValue) then
  begin
    if IsNaN(TGocciaNumberValue(Left).ToNumber) or IsNaN(TGocciaNumberValue(Right).ToNumber) then
    begin
      Result := False;
      Exit;
    end;

    if IsInfinite(TGocciaNumberValue(Left).ToNumber) and IsInfinite(TGocciaNumberValue(Right).ToNumber) then
    begin
      Result := Sign(TGocciaNumberValue(Left).ToNumber) = Sign(TGocciaNumberValue(Right).ToNumber);
      Exit;
    end;

    if IsInfinite(TGocciaNumberValue(Left).ToNumber) or IsInfinite(TGocciaNumberValue(Right).ToNumber) then
    begin
      Result := False;
      Exit;
    end;

    Result := TGocciaNumberValue(Left).ToNumber = TGocciaNumberValue(Right).ToNumber;
    Exit;
  end else if (Left is TGocciaStringValue) and (Right is TGocciaStringValue) then
    Result := TGocciaStringValue(Left).ToString = TGocciaStringValue(Right).ToString
  else
    Result := Left = Right; // Reference equality for objects
end;

function IsNotStrictEqual(Left, Right: TGocciaValue): Boolean;
begin
  Result := not IsStrictEqual(Left, Right);
end;

function GreaterThan(Left, Right: TGocciaValue): Boolean;
begin
  if (Left is TGocciaUndefinedValue) or (Right is TGocciaUndefinedValue) then
  begin
    Result := False;
    Exit;
  end;

  if (Left is TGocciaNullValue) or (Right is TGocciaNullValue) then
  begin
    Result := False;
    Exit;
  end;

  if (Left is TGocciaBooleanValue) and (Right is TGocciaBooleanValue) then
  begin
    Result := TGocciaBooleanValue(Left).ToBoolean > TGocciaBooleanValue(Right).ToBoolean;
    Exit;
  end;

  if (Left is TGocciaNumberValue) and (Right is TGocciaNumberValue) then
  begin
    Result := TGocciaNumberValue(Left).ToNumber > TGocciaNumberValue(Right).ToNumber;
    Exit;
  end;

  if (Left is TGocciaStringValue) and (Right is TGocciaStringValue) then
  begin
    Result := TGocciaStringValue(Left).ToString > TGocciaStringValue(Right).ToString;
    Exit;
  end;

  Result := False; // Default for non-comparable types
end;

function GreaterThanOrEqual(Left, Right: TGocciaValue): Boolean;
begin
  Result := GreaterThan(Left, Right) or IsStrictEqual(Left, Right);
end;

function LessThan(Left, Right: TGocciaValue): Boolean;
begin
  if (Left is TGocciaUndefinedValue) or (Right is TGocciaUndefinedValue) then
  begin
    Result := False;
    Exit;
  end;

  if (Left is TGocciaNullValue) or (Right is TGocciaNullValue) then
  begin
    Result := False;
    Exit;
  end;

  if (Left is TGocciaBooleanValue) and (Right is TGocciaBooleanValue) then
  begin
    Result := TGocciaBooleanValue(Left).ToBoolean < TGocciaBooleanValue(Right).ToBoolean;
    Exit;
  end;

  if (Left is TGocciaNumberValue) and (Right is TGocciaNumberValue) then
  begin
    Result := TGocciaNumberValue(Left).ToNumber < TGocciaNumberValue(Right).ToNumber;
    Exit;
  end;

  if (Left is TGocciaStringValue) and (Right is TGocciaStringValue) then
  begin
    Result := TGocciaStringValue(Left).ToString < TGocciaStringValue(Right).ToString;
    Exit;
  end;

  Result := False; // Default for non-comparable types
end;

function LessThanOrEqual(Left, Right: TGocciaValue): Boolean;
begin
  Result := LessThan(Left, Right) or IsStrictEqual(Left, Right);
end;

end.
