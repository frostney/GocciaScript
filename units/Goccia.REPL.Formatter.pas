unit Goccia.REPL.Formatter;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

function FormatREPLValue(const AValue: TGocciaValue): string;

implementation

uses
  SysUtils,

  Goccia.Values.ArrayValue,
  Goccia.Values.FunctionBase,
  Goccia.Values.SymbolValue;

const
  ANSI_GREEN = #27'[32m';
  ANSI_YELLOW = #27'[33m';
  ANSI_CYAN = #27'[36m';
  ANSI_GRAY = #27'[90m';
  ANSI_BOLD = #27'[1m';
  ANSI_RESET = #27'[0m';

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

function FormatREPLValue(const AValue: TGocciaValue): string;
begin
  if AValue = nil then
    Exit(ANSI_GRAY + 'undefined' + ANSI_RESET);

  if AValue is TGocciaUndefinedLiteralValue then
    Result := ANSI_GRAY + 'undefined' + ANSI_RESET
  else if AValue is TGocciaNullLiteralValue then
    Result := ANSI_BOLD + 'null' + ANSI_RESET
  else if AValue is TGocciaStringLiteralValue then
    Result := ANSI_GREEN + '''' + EscapeString(TGocciaStringLiteralValue(AValue).Value) + '''' + ANSI_RESET
  else if AValue is TGocciaNumberLiteralValue then
    Result := ANSI_YELLOW + AValue.ToStringLiteral.Value + ANSI_RESET
  else if AValue is TGocciaBooleanLiteralValue then
    Result := ANSI_YELLOW + AValue.ToStringLiteral.Value + ANSI_RESET
  else if AValue is TGocciaSymbolValue then
    Result := ANSI_GREEN + AValue.ToStringLiteral.Value + ANSI_RESET
  else if AValue is TGocciaFunctionBase then
    Result := ANSI_CYAN + '[Function]' + ANSI_RESET
  else
    Result := AValue.ToStringLiteral.Value;
end;

end.
