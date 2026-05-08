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

const
  MAX_FORMAT_DEPTH = 5;

function DoFormat(const AValue: TGocciaValue; const ANested: Boolean; const ADepth: Integer): string; forward;

function FormatArray(const AArr: TGocciaArrayValue; const ADepth: Integer): string;
var
  SB: TStringBuffer;
  I: Integer;
begin
  if ADepth >= MAX_FORMAT_DEPTH then
  begin
    Result := '[...]';
    Exit;
  end;
  SB := TStringBuffer.Create;
  SB.AppendChar('[');
  for I := 0 to AArr.Elements.Count - 1 do
  begin
    if I > 0 then
      SB.Append(', ');
    SB.Append(DoFormat(AArr.Elements[I], True, ADepth + 1));
  end;
  SB.AppendChar(']');
  Result := SB.ToString;
end;

function FormatObject(const AObj: TGocciaObjectValue; const ADepth: Integer): string;
var
  SB: TStringBuffer;
  Key: string;
  First: Boolean;
begin
  if ADepth >= MAX_FORMAT_DEPTH then
  begin
    Result := '{...}';
    Exit;
  end;
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
    SB.Append(DoFormat(AObj.GetProperty(Key), True, ADepth + 1));
  end;
  SB.AppendChar('}');
  Result := SB.ToString;
end;

function DoFormat(const AValue: TGocciaValue; const ANested: Boolean; const ADepth: Integer): string;
begin
  if not Assigned(AValue) then
  begin
    Result := 'undefined';
    Exit;
  end;

  if AValue is TGocciaSymbolValue then
    Result := TGocciaSymbolValue(AValue).ToDisplayString.Value
  else if AValue is TGocciaArrayValue then
    Result := FormatArray(TGocciaArrayValue(AValue), ADepth)
  else if AValue is TGocciaObjectValue then
    Result := FormatObject(TGocciaObjectValue(AValue), ADepth)
  else if ANested and (AValue is TGocciaStringLiteralValue) then
    Result := '''' + AValue.ToStringLiteral.Value + ''''
  else
    Result := AValue.ToStringLiteral.Value;
end;

function FormatForDisplay(const AValue: TGocciaValue): string;
begin
  Result := DoFormat(AValue, False, 0);
end;

end.
