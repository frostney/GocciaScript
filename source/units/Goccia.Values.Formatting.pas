unit Goccia.Values.Formatting;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

function FormatForDisplay(const AValue: TGocciaValue): string;

implementation

uses
  StringBuffer,

  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue;

function DoFormat(const AValue: TGocciaValue; const ANested: Boolean): string; forward;

function FormatArray(const AArr: TGocciaArrayValue): string;
var
  SB: TStringBuffer;
  I: Integer;
begin
  SB := TStringBuffer.Create;
  SB.AppendChar('[');
  for I := 0 to AArr.Elements.Count - 1 do
  begin
    if I > 0 then
      SB.Append(', ');
    SB.Append(DoFormat(AArr.Elements[I], True));
  end;
  SB.AppendChar(']');
  Result := SB.ToString;
end;

function FormatObject(const AObj: TGocciaObjectValue): string;
var
  SB: TStringBuffer;
  Key: string;
  First: Boolean;
begin
  SB := TStringBuffer.Create;
  SB.AppendChar('{');
  First := True;
  for Key in AObj.GetEnumerablePropertyNames do
  begin
    if not First then
      SB.Append(', ');
    First := False;
    SB.Append(Key);
    SB.Append(': ');
    SB.Append(DoFormat(AObj.GetProperty(Key), True));
  end;
  SB.AppendChar('}');
  Result := SB.ToString;
end;

function DoFormat(const AValue: TGocciaValue; const ANested: Boolean): string;
begin
  if not Assigned(AValue) then
  begin
    Result := 'undefined';
    Exit;
  end;

  if AValue is TGocciaSymbolValue then
    Result := TGocciaSymbolValue(AValue).ToDisplayString.Value
  else if AValue is TGocciaArrayValue then
    Result := FormatArray(TGocciaArrayValue(AValue))
  else if AValue is TGocciaObjectValue then
    Result := FormatObject(TGocciaObjectValue(AValue))
  else if ANested and (AValue is TGocciaStringLiteralValue) then
    Result := '''' + AValue.ToStringLiteral.Value + ''''
  else
    Result := AValue.ToStringLiteral.Value;
end;

function FormatForDisplay(const AValue: TGocciaValue): string;
begin
  Result := DoFormat(AValue, False);
end;

end.
