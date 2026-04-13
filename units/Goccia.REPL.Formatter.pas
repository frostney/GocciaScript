unit Goccia.REPL.Formatter;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

function FormatREPLValue(const AValue: TGocciaValue;
  const AUseColor: Boolean): string;

implementation

uses
  Generics.Collections,
  SysUtils,

  StringBuffer,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.RegExp.Runtime,
  Goccia.Terminal.Colors,
  Goccia.Values.ArrayValue,
  Goccia.Values.FunctionBase,
  Goccia.Values.MapValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.PromiseValue,
  Goccia.Values.SetValue,
  Goccia.Values.SymbolValue;

const
  MAX_INSPECT_DEPTH = 2;
  INDENT_SIZE = 2;

type
  TVisitedSet = TList<TGocciaValue>;

function EscapeString(const AValue: string): string;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(AValue) do
  begin
    C := AValue[I];
    case C of
      '''': Result := Result + '\''';
      '\': Result := Result + '\\';
      #8: Result := Result + '\b';
      #9: Result := Result + '\t';
      #10: Result := Result + '\n';
      #13: Result := Result + '\r';
    else
      if C < ' ' then
        Result := Result + '\x' + IntToHex(Ord(C), 2)
      else
        Result := Result + C;
    end;
  end;
end;

function Indent(const ADepth: Integer): string;
begin
  Result := StringOfChar(' ', ADepth * INDENT_SIZE);
end;

function InspectValue(const AValue: TGocciaValue;
  const AUseColor: Boolean; const ADepth: Integer;
  const AVisited: TVisitedSet): string; forward;

function InspectObjectProperties(const AObj: TGocciaObjectValue;
  const AUseColor: Boolean; const ADepth: Integer;
  const AVisited: TVisitedSet): string;
var
  Key: string;
  SB: TStringBuffer;
  First: Boolean;
  ChildIndent: string;
begin
  SB := TStringBuffer.Create;
  First := True;
  ChildIndent := Indent(ADepth + 1);
  for Key in AObj.GetEnumerablePropertyNames do
  begin
    if not First then
      SB.AppendChar(',');
    First := False;
    SB.Append(LineEnding);
    SB.Append(ChildIndent);
    SB.Append(Key);
    SB.Append(': ');
    SB.Append(InspectValue(AObj.GetProperty(Key), AUseColor, ADepth + 1,
      AVisited));
  end;
  Result := SB.ToString;
end;

function InspectValue(const AValue: TGocciaValue;
  const AUseColor: Boolean; const ADepth: Integer;
  const AVisited: TVisitedSet): string;
var
  Arr: TGocciaArrayValue;
  Obj: TGocciaObjectValue;
  MapVal: TGocciaMapValue;
  SetVal: TGocciaSetValue;
  PromiseVal: TGocciaPromiseValue;
  I: Integer;
  SB: TStringBuffer;
  Tag, Props, ErrorName, ErrorMessage, ChildIndent, ParentIndent: string;
begin
  // nil -> undefined
  if AValue = nil then
    Exit(Colorize('undefined', ANSI_GRAY, AUseColor));

  // Primitives — no depth tracking needed
  if AValue is TGocciaUndefinedLiteralValue then
    Exit(Colorize('undefined', ANSI_GRAY, AUseColor));
  if AValue is TGocciaNullLiteralValue then
    Exit(Colorize('null', ANSI_BOLD, AUseColor));
  if AValue is TGocciaStringLiteralValue then
    Exit(Colorize('''' + EscapeString(TGocciaStringLiteralValue(AValue).Value) +
      '''', ANSI_GREEN, AUseColor));
  if AValue is TGocciaNumberLiteralValue then
    Exit(Colorize(AValue.ToStringLiteral.Value, ANSI_YELLOW, AUseColor));
  if AValue is TGocciaBooleanLiteralValue then
    Exit(Colorize(AValue.ToStringLiteral.Value, ANSI_YELLOW, AUseColor));
  if AValue is TGocciaSymbolValue then
    Exit(Colorize(AValue.ToStringLiteral.Value, ANSI_GREEN, AUseColor));

  // Functions
  if AValue is TGocciaFunctionBase then
  begin
    Tag := TGocciaFunctionBase(AValue).GetProperty(PROP_NAME).ToStringLiteral.Value;
    if Tag <> '' then
      Exit(Colorize('[Function: ' + Tag + ']', ANSI_CYAN, AUseColor))
    else
      Exit(Colorize('[Function (anonymous)]', ANSI_CYAN, AUseColor));
  end;

  // Circular reference detection
  if AVisited.IndexOf(AValue) >= 0 then
    Exit(Colorize('[Circular]', ANSI_CYAN, AUseColor));

  // At depth limit, show abbreviated type tag
  if ADepth >= MAX_INSPECT_DEPTH then
  begin
    if AValue is TGocciaArrayValue then
      Exit(Colorize('[Array]', ANSI_CYAN, AUseColor));
    if AValue is TGocciaMapValue then
      Exit(Colorize('[Map]', ANSI_CYAN, AUseColor));
    if AValue is TGocciaSetValue then
      Exit(Colorize('[Set]', ANSI_CYAN, AUseColor));
    if AValue is TGocciaObjectValue then
      Exit(Colorize('[Object]', ANSI_CYAN, AUseColor));
    Exit(AValue.ToStringLiteral.Value);
  end;

  ChildIndent := Indent(ADepth + 1);
  ParentIndent := Indent(ADepth);

  AVisited.Add(AValue);
  try
    // Arrays
    if AValue is TGocciaArrayValue then
    begin
      Arr := TGocciaArrayValue(AValue);
      if Arr.Elements.Count = 0 then
        Exit('[]');
      SB := TStringBuffer.Create;
      SB.AppendChar('[');
      for I := 0 to Arr.Elements.Count - 1 do
      begin
        if I > 0 then
          SB.AppendChar(',');
        SB.Append(LineEnding);
        SB.Append(ChildIndent);
        SB.Append(InspectValue(Arr.Elements[I], AUseColor, ADepth + 1,
          AVisited));
      end;
      SB.Append(LineEnding);
      SB.Append(ParentIndent);
      SB.AppendChar(']');
      Result := SB.ToString;
    end

    // Maps
    else if AValue is TGocciaMapValue then
    begin
      MapVal := TGocciaMapValue(AValue);
      SB := TStringBuffer.Create;
      SB.Append('Map(' + IntToStr(MapVal.Entries.Count) + ')');
      if MapVal.Entries.Count = 0 then
      begin
        SB.Append(' {}');
      end
      else
      begin
        SB.Append(' {');
        for I := 0 to MapVal.Entries.Count - 1 do
        begin
          if I > 0 then
            SB.AppendChar(',');
          SB.Append(LineEnding);
          SB.Append(ChildIndent);
          SB.Append(InspectValue(MapVal.Entries[I].Key, AUseColor,
            ADepth + 1, AVisited));
          SB.Append(' => ');
          SB.Append(InspectValue(MapVal.Entries[I].Value, AUseColor,
            ADepth + 1, AVisited));
        end;
        SB.Append(LineEnding);
        SB.Append(ParentIndent);
        SB.AppendChar('}');
      end;
      Result := SB.ToString;
    end

    // Sets
    else if AValue is TGocciaSetValue then
    begin
      SetVal := TGocciaSetValue(AValue);
      SB := TStringBuffer.Create;
      SB.Append('Set(' + IntToStr(SetVal.Items.Count) + ')');
      if SetVal.Items.Count = 0 then
      begin
        SB.Append(' {}');
      end
      else
      begin
        SB.Append(' {');
        for I := 0 to SetVal.Items.Count - 1 do
        begin
          if I > 0 then
            SB.AppendChar(',');
          SB.Append(LineEnding);
          SB.Append(ChildIndent);
          SB.Append(InspectValue(SetVal.Items[I], AUseColor, ADepth + 1,
            AVisited));
        end;
        SB.Append(LineEnding);
        SB.Append(ParentIndent);
        SB.AppendChar('}');
      end;
      Result := SB.ToString;
    end

    // Promises
    else if AValue is TGocciaPromiseValue then
    begin
      PromiseVal := TGocciaPromiseValue(AValue);
      case PromiseVal.State of
        gpsPending:
          Result := 'Promise { ' + Colorize('<pending>', ANSI_CYAN, AUseColor)
            + ' }';
        gpsFulfilled:
          Result := 'Promise { ' + InspectValue(PromiseVal.PromiseResult,
            AUseColor, ADepth + 1, AVisited) + ' }';
        gpsRejected:
          Result := 'Promise { ' + Colorize('<rejected>', ANSI_RED, AUseColor)
            + ' ' + InspectValue(PromiseVal.PromiseResult, AUseColor,
            ADepth + 1, AVisited) + ' }';
      end;
    end

    // General objects (including errors and RegExp)
    else if AValue is TGocciaObjectValue then
    begin
      Obj := TGocciaObjectValue(AValue);

      // Error objects
      if Obj.HasErrorData then
      begin
        ErrorName := Obj.GetProperty(PROP_NAME).ToStringLiteral.Value;
        ErrorMessage := Obj.GetProperty(PROP_MESSAGE).ToStringLiteral.Value;
        Result := Colorize(ErrorName + ': ' + ErrorMessage, ANSI_RED,
          AUseColor);
      end

      // RegExp objects
      else if IsRegExpValue(AValue) then
      begin
        Result := Colorize(RegExpObjectToString(AValue), ANSI_RED, AUseColor);
      end

      // Plain objects and class instances
      else
      begin
        SB := TStringBuffer.Create;
        Tag := Obj.ToStringTag;
        if (Tag <> '') and (Tag <> CONSTRUCTOR_OBJECT) then
          SB.Append(Tag + ' ');
        Props := InspectObjectProperties(Obj, AUseColor, ADepth, AVisited);
        if Props = '' then
          SB.Append('{}')
        else
        begin
          SB.AppendChar('{');
          SB.Append(Props);
          SB.Append(LineEnding);
          SB.Append(ParentIndent);
          SB.AppendChar('}');
        end;
        Result := SB.ToString;
      end;
    end

    else
      Result := AValue.ToStringLiteral.Value;
  finally
    AVisited.Remove(AValue);
  end;
end;

function FormatREPLValue(const AValue: TGocciaValue;
  const AUseColor: Boolean): string;
var
  Visited: TVisitedSet;
begin
  Visited := TVisitedSet.Create;
  try
    Result := InspectValue(AValue, AUseColor, 0, Visited);
  finally
    Visited.Free;
  end;
end;

end.
