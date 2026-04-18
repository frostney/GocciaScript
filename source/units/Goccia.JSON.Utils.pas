unit Goccia.JSON.Utils;

{$I Goccia.inc}

interface

/// Escape a string for embedding inside a JSON string literal.
///
/// All C0 control characters (U+0000-U+001F) are escaped:
///   - Known short escapes: \b (#8), \t (#9), \n (#10), \f (#12), \r (#13)
///   - Remaining control characters: \u00XX hex escapes
///
/// Backslash, double-quote and solidus are also escaped.
function EscapeJSONString(const AValue: string): string;

/// Wrap a value in escaped double quotes: "…"
function QuoteJSONString(const AValue: string): string;

implementation

uses
  SysUtils,

  StringBuffer;

function EscapeJSONString(const AValue: string): string;
var
  Buffer: TStringBuffer;
  I: Integer;
  Ch: Char;
begin
  Buffer := TStringBuffer.Create(Length(AValue));
  for I := 1 to Length(AValue) do
  begin
    Ch := AValue[I];
    case Ch of
      '"': Buffer.Append('\"');
      '\': Buffer.Append('\\');
      '/': Buffer.Append('\/');
      #8: Buffer.Append('\b');
      #9: Buffer.Append('\t');
      #10: Buffer.Append('\n');
      #12: Buffer.Append('\f');
      #13: Buffer.Append('\r');
    else
      if Ord(Ch) < 32 then
      begin
        Buffer.Append('\u');
        Buffer.Append(IntToHex(Ord(Ch), 4));
      end
      else
        Buffer.AppendChar(Ch);
    end;
  end;
  Result := Buffer.ToString;
end;

function QuoteJSONString(const AValue: string): string;
begin
  Result := '"' + EscapeJSONString(AValue) + '"';
end;

end.
