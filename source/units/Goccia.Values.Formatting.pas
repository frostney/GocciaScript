unit Goccia.Values.Formatting;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

function FormatForDisplay(const AValue: TGocciaValue): string;

implementation

uses
  Generics.Collections,

  StringBuffer,

  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue;

const
  MAX_FORMAT_DEPTH = 5;

function FormatRecursive(const AValue: TGocciaValue; const ANested: Boolean; const ADepth: Integer): string; forward;

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
    SB.Append(FormatRecursive(AArr.Elements[I], True, ADepth + 1));
  end;
  SB.AppendChar(']');
  Result := SB.ToString;
end;

function FormatObject(const AObj: TGocciaObjectValue; const ADepth: Integer): string;
var
  SB: TStringBuffer;
  Entry: TPair<string, TGocciaValue>;
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
  for Entry in AObj.GetEnumerablePropertyEntries do
  begin
    if not First then
      SB.Append(', ');
    First := False;
    SB.Append(Entry.Key);
    SB.Append(': ');
    SB.Append(FormatRecursive(Entry.Value, True, ADepth + 1));
  end;
  SB.AppendChar('}');
  Result := SB.ToString;
end;

function FormatRecursive(const AValue: TGocciaValue; const ANested: Boolean; const ADepth: Integer): string;
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
  Result := FormatRecursive(AValue, False, 0);
end;

end.
