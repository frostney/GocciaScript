unit Goccia.Values.Formatting;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

const
  DEFAULT_INSPECT_DEPTH = 5;
  MAX_INSPECT_DEPTH = 64;

procedure SetInspectDepth(const ADepth: Integer);
function FormatForDisplay(const AValue: TGocciaValue): string;

implementation

uses
  Generics.Collections,
  Math,
  SysUtils,

  StringBuffer,

  Goccia.Values.ArrayValue,
  Goccia.Values.MapValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.SetValue,
  Goccia.Values.SymbolValue;

var
  GInspectDepth: Integer = DEFAULT_INSPECT_DEPTH;

procedure SetInspectDepth(const ADepth: Integer);
begin
  GInspectDepth := Min(MAX_INSPECT_DEPTH, Max(1, ADepth));
end;

function FormatRecursive(const AValue: TGocciaValue; const ANested: Boolean; const ADepth: Integer): string; forward;

function FormatArray(const AArr: TGocciaArrayValue; const ADepth: Integer): string;
var
  SB: TStringBuffer;
  I: Integer;
begin
  if ADepth >= GInspectDepth then
  begin
    Result := '[Array]';
    Exit;
  end;
  if AArr.Elements.Count = 0 then
  begin
    Result := '[]';
    Exit;
  end;
  SB := TStringBuffer.Create;
  SB.Append('[ ');
  for I := 0 to AArr.Elements.Count - 1 do
  begin
    if I > 0 then
      SB.Append(', ');
    SB.Append(FormatRecursive(AArr.Elements[I], True, ADepth + 1));
  end;
  SB.Append(' ]');
  Result := SB.ToString;
end;

function FormatSet(const ASet: TGocciaSetValue; const ADepth: Integer): string;
var
  SB: TStringBuffer;
  Cursor: Integer;
  Item: TGocciaValue;
  First: Boolean;
begin
  if ADepth >= GInspectDepth then
  begin
    Result := '[Set]';
    Exit;
  end;
  if ASet.Count = 0 then
  begin
    Result := 'Set(0) {}';
    Exit;
  end;
  SB := TStringBuffer.Create;
  SB.Append('Set(');
  SB.Append(IntToStr(ASet.Count));
  SB.Append(') { ');
  First := True;
  Cursor := 0;
  ASet.RetainIterator;
  try
    while ASet.NextItem(Cursor, Item) do
    begin
      if not First then
        SB.Append(', ');
      First := False;
      SB.Append(FormatRecursive(Item, True, ADepth + 1));
    end;
  finally
    ASet.ReleaseIterator;
  end;
  SB.Append(' }');
  Result := SB.ToString;
end;

function FormatMap(const AMap: TGocciaMapValue; const ADepth: Integer): string;
var
  SB: TStringBuffer;
  Cursor: Integer;
  Key, Value: TGocciaValue;
  First: Boolean;
begin
  if ADepth >= GInspectDepth then
  begin
    Result := '[Map]';
    Exit;
  end;
  if AMap.Count = 0 then
  begin
    Result := 'Map(0) {}';
    Exit;
  end;
  SB := TStringBuffer.Create;
  SB.Append('Map(');
  SB.Append(IntToStr(AMap.Count));
  SB.Append(') { ');
  First := True;
  Cursor := 0;
  AMap.RetainIterator;
  try
    while AMap.NextEntry(Cursor, Key, Value) do
    begin
      if not First then
        SB.Append(', ');
      First := False;
      SB.Append(FormatRecursive(Key, True, ADepth + 1));
      SB.Append(' => ');
      SB.Append(FormatRecursive(Value, True, ADepth + 1));
    end;
  finally
    AMap.ReleaseIterator;
  end;
  SB.Append(' }');
  Result := SB.ToString;
end;

function FormatObject(const AObj: TGocciaObjectValue; const ADepth: Integer): string;
var
  SB: TStringBuffer;
  Entry: TPair<string, TGocciaValue>;
  First: Boolean;
begin
  if ADepth >= GInspectDepth then
  begin
    Result := '[Object]';
    Exit;
  end;
  SB := TStringBuffer.Create;
  SB.AppendChar('{');
  First := True;
  for Entry in AObj.GetEnumerablePropertyEntries do
  begin
    if First then
      SB.AppendChar(' ')
    else
      SB.Append(', ');
    First := False;
    SB.Append(Entry.Key);
    SB.Append(': ');
    SB.Append(FormatRecursive(Entry.Value, True, ADepth + 1));
  end;
  if not First then
    SB.Append(' }')
  else
    SB.AppendChar('}');
  Result := SB.ToString;
end;

function QuoteString(const AStr: string): string;
begin
  if Pos('''', AStr) = 0 then
    Result := '''' + AStr + ''''
  else if Pos('"', AStr) = 0 then
    Result := '"' + AStr + '"'
  else
    Result := '`' + AStr + '`';
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
  else if AValue is TGocciaSetValue then
    Result := FormatSet(TGocciaSetValue(AValue), ADepth)
  else if AValue is TGocciaMapValue then
    Result := FormatMap(TGocciaMapValue(AValue), ADepth)
  else if AValue is TGocciaObjectValue then
    Result := FormatObject(TGocciaObjectValue(AValue), ADepth)
  else if ANested and (AValue is TGocciaStringLiteralValue) then
    Result := QuoteString(AValue.ToStringLiteral.Value)
  else
    Result := AValue.ToStringLiteral.Value;
end;

function FormatForDisplay(const AValue: TGocciaValue): string;
begin
  Result := FormatRecursive(AValue, False, 0);
end;

end.
