unit CSVFields;

{$I Shared.inc}

interface

{ AValue as one CSV field (RFC 4180 §2): in double quotes, each quote doubled,
  when it holds ADelimiter, a quote or a line break; as it is otherwise. }
function EscapeCSVField(const AValue: string; const ADelimiter: Char): string;

implementation

uses
  SysUtils;

function EscapeCSVField(const AValue: string; const ADelimiter: Char): string;
var
  NeedsQuoting: Boolean;
  I: Integer;
begin
  NeedsQuoting := False;
  for I := 1 to Length(AValue) do
  begin
    if (AValue[I] = ADelimiter) or (AValue[I] = '"') or
       (AValue[I] = #10) or (AValue[I] = #13) then
    begin
      NeedsQuoting := True;
      Break;
    end;
  end;

  if NeedsQuoting then
    Result := '"' + StringReplace(AValue, '"', '""', [rfReplaceAll]) + '"'
  else
    Result := AValue;
end;

end.
