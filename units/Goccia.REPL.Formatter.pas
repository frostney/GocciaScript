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

  Goccia.Terminal.Colors,
  Goccia.Values.ArrayValue,
  Goccia.Values.FunctionBase,
  Goccia.Values.SymbolValue;

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

function FormatREPLValue(const AValue: TGocciaValue;
  const AUseColor: Boolean): string;
begin
  if AValue = nil then
    Exit(Colorize('undefined', ANSI_GRAY, AUseColor));

  if AValue is TGocciaUndefinedLiteralValue then
    Result := Colorize('undefined', ANSI_GRAY, AUseColor)
  else if AValue is TGocciaNullLiteralValue then
    Result := Colorize('null', ANSI_BOLD, AUseColor)
  else if AValue is TGocciaStringLiteralValue then
    Result := Colorize('''' + EscapeString(TGocciaStringLiteralValue(AValue).Value) + '''', ANSI_GREEN, AUseColor)
  else if AValue is TGocciaNumberLiteralValue then
    Result := Colorize(AValue.ToStringLiteral.Value, ANSI_YELLOW, AUseColor)
  else if AValue is TGocciaBooleanLiteralValue then
    Result := Colorize(AValue.ToStringLiteral.Value, ANSI_YELLOW, AUseColor)
  else if AValue is TGocciaSymbolValue then
    Result := Colorize(AValue.ToStringLiteral.Value, ANSI_GREEN, AUseColor)
  else if AValue is TGocciaFunctionBase then
    Result := Colorize('[Function]', ANSI_CYAN, AUseColor)
  else
    Result := AValue.ToStringLiteral.Value;
end;

end.
