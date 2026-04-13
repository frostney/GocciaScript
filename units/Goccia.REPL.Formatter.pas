unit Goccia.REPL.Formatter;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

function FormatREPLValue(const AValue: TGocciaValue;
  const AUseColor: Boolean): string;

implementation

uses
  SysUtils,

  StringBuffer,

  Goccia.Constants.PropertyNames,
  Goccia.JSON,
  Goccia.RegExp.Runtime,
  Goccia.Terminal.Colors,
  Goccia.Values.FunctionBase,
  Goccia.Values.MapValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.PromiseValue,
  Goccia.Values.SetValue,
  Goccia.Values.SymbolValue;

const
  REPL_INDENT = '  ';

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

function StringifyIndented(const AValue: TGocciaValue): string;
var
  Stringifier: TGocciaJSONStringifier;
begin
  Stringifier := TGocciaJSONStringifier.Create;
  try
    Result := Stringifier.Stringify(AValue, REPL_INDENT);
  finally
    Stringifier.Free;
  end;
end;

function StringifyCompact(const AValue: TGocciaValue): string;
var
  Stringifier: TGocciaJSONStringifier;
begin
  Stringifier := TGocciaJSONStringifier.Create;
  try
    Result := Stringifier.Stringify(AValue);
  finally
    Stringifier.Free;
  end;
end;

function FormatMapValue(const AMap: TGocciaMapValue): string;
var
  SB: TStringBuffer;
  I: Integer;
begin
  SB := TStringBuffer.Create;
  SB.Append('Map(' + IntToStr(AMap.Entries.Count) + ')');
  if AMap.Entries.Count = 0 then
    SB.Append(' {}')
  else
  begin
    SB.Append(' { ');
    for I := 0 to AMap.Entries.Count - 1 do
    begin
      if I > 0 then
        SB.Append(', ');
      SB.Append(StringifyCompact(AMap.Entries[I].Key));
      SB.Append(' => ');
      SB.Append(StringifyCompact(AMap.Entries[I].Value));
    end;
    SB.Append(' }');
  end;
  Result := SB.ToString;
end;

function FormatSetValue(const ASet: TGocciaSetValue): string;
var
  SB: TStringBuffer;
  I: Integer;
begin
  SB := TStringBuffer.Create;
  SB.Append('Set(' + IntToStr(ASet.Items.Count) + ')');
  if ASet.Items.Count = 0 then
    SB.Append(' {}')
  else
  begin
    SB.Append(' { ');
    for I := 0 to ASet.Items.Count - 1 do
    begin
      if I > 0 then
        SB.Append(', ');
      SB.Append(StringifyCompact(ASet.Items[I]));
    end;
    SB.Append(' }');
  end;
  Result := SB.ToString;
end;

function FormatREPLValue(const AValue: TGocciaValue;
  const AUseColor: Boolean): string;
var
  Tag: string;
  Obj: TGocciaObjectValue;
  PromiseVal: TGocciaPromiseValue;
begin
  if AValue = nil then
    Exit(Colorize('undefined', ANSI_GRAY, AUseColor));

  // Primitives — JSON.stringify loses these, so handle directly
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

  // Functions — JSON.stringify returns null for these
  if AValue is TGocciaFunctionBase then
  begin
    Tag := TGocciaFunctionBase(AValue).GetProperty(PROP_NAME).ToStringLiteral.Value;
    if Tag <> '' then
      Exit(Colorize('[Function: ' + Tag + ']', ANSI_CYAN, AUseColor))
    else
      Exit(Colorize('[Function (anonymous)]', ANSI_CYAN, AUseColor));
  end;

  // Promises — JSON.stringify would show empty object
  if AValue is TGocciaPromiseValue then
  begin
    PromiseVal := TGocciaPromiseValue(AValue);
    case PromiseVal.State of
      gpsPending:
        Exit('Promise { ' + Colorize('<pending>', ANSI_CYAN, AUseColor) + ' }');
      gpsFulfilled:
        Exit('Promise { ' + FormatREPLValue(PromiseVal.PromiseResult,
          AUseColor) + ' }');
      gpsRejected:
        Exit('Promise { ' + Colorize('<rejected>', ANSI_RED, AUseColor) + ' ' +
          FormatREPLValue(PromiseVal.PromiseResult, AUseColor) + ' }');
    end;
  end;

  // Error objects — show name: message instead of properties
  if (AValue is TGocciaObjectValue) and
    TGocciaObjectValue(AValue).HasErrorData then
  begin
    Obj := TGocciaObjectValue(AValue);
    Exit(Colorize(
      Obj.GetProperty(PROP_NAME).ToStringLiteral.Value + ': ' +
      Obj.GetProperty(PROP_MESSAGE).ToStringLiteral.Value,
      ANSI_RED, AUseColor));
  end;

  // RegExp — JSON.stringify would show empty object
  if IsRegExpValue(AValue) then
    Exit(Colorize(RegExpObjectToString(AValue), ANSI_RED, AUseColor));

  // Maps — JSON.stringify can't see internal entries
  if AValue is TGocciaMapValue then
    Exit(FormatMapValue(TGocciaMapValue(AValue)));

  // Sets — JSON.stringify can't see internal items
  if AValue is TGocciaSetValue then
    Exit(FormatSetValue(TGocciaSetValue(AValue)));

  // Objects and arrays — delegate to JSON.stringify with 2-space indent
  Result := StringifyIndented(AValue);
end;

end.
