unit Goccia.Error.Detail;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Values.Primitives;

{ Extracts the first stack frame's line and column from a JS error object's
  stack trace string. Returns True if a location was found. }
function ExtractThrowLocation(const AThrown: TGocciaValue;
  out AErrorName, AErrorMessage: string;
  out ALine, AColumn: Integer): Boolean;

{ Formats a detailed error message for a TGocciaThrowValue, including source
  context with caret pointer when source lines and location are available.
  Falls back to the stack trace or bare message when context is unavailable. }
function FormatThrowDetail(const AThrown: TGocciaValue;
  const AFileName: string; const ASourceLines: TStringList;
  const AUseColor: Boolean): string;

implementation

uses
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.Values.ObjectValue;

function ExtractThrowLocation(const AThrown: TGocciaValue;
  out AErrorName, AErrorMessage: string;
  out ALine, AColumn: Integer): Boolean;
var
  StackValue, MsgValue, NameValue: TGocciaValue;
  StackStr: string;
  AtPos, ParenPos, ColonPos1, ColonPos2, I: Integer;
begin
  Result := False;
  ALine := 0;
  AColumn := 0;
  AErrorName := 'Error';
  AErrorMessage := '';

  if not (AThrown is TGocciaObjectValue) then Exit;

  NameValue := TGocciaObjectValue(AThrown).GetProperty(PROP_NAME);
  if Assigned(NameValue) and (NameValue is TGocciaStringLiteralValue) then
    AErrorName := TGocciaStringLiteralValue(NameValue).Value;

  MsgValue := TGocciaObjectValue(AThrown).GetProperty(PROP_MESSAGE);
  if Assigned(MsgValue) and (MsgValue is TGocciaStringLiteralValue) then
    AErrorMessage := TGocciaStringLiteralValue(MsgValue).Value;

  StackValue := TGocciaObjectValue(AThrown).GetProperty(PROP_STACK);
  if not Assigned(StackValue) or not (StackValue is TGocciaStringLiteralValue) then Exit;

  StackStr := TGocciaStringLiteralValue(StackValue).Value;

  // Find first "at ... (file:line:col)" frame
  AtPos := Pos('    at ', StackStr);
  if AtPos = 0 then Exit;

  ParenPos := Pos('(', Copy(StackStr, AtPos, MaxInt));
  if ParenPos = 0 then Exit;
  ParenPos := AtPos + ParenPos - 1;

  // Find file:line:col within parentheses — scan backwards from closing paren
  I := Pos(')', Copy(StackStr, ParenPos, MaxInt));
  if I = 0 then Exit;
  I := ParenPos + I - 2; // position of last char before ')'

  // Parse ":col" from the end
  ColonPos2 := I;
  while (ColonPos2 > ParenPos) and (StackStr[ColonPos2] <> ':') do
    Dec(ColonPos2);
  if ColonPos2 <= ParenPos then Exit;

  TryStrToInt(Copy(StackStr, ColonPos2 + 1, I - ColonPos2), AColumn);

  // Parse ":line" before that
  ColonPos1 := ColonPos2 - 1;
  while (ColonPos1 > ParenPos) and (StackStr[ColonPos1] <> ':') do
    Dec(ColonPos1);
  if ColonPos1 <= ParenPos then Exit;

  Result := TryStrToInt(Copy(StackStr, ColonPos1 + 1, ColonPos2 - ColonPos1 - 1), ALine);
end;

function FormatThrowDetail(const AThrown: TGocciaValue;
  const AFileName: string; const ASourceLines: TStringList;
  const AUseColor: Boolean): string;
var
  ErrorName, ErrorMessage: string;
  Line, Col: Integer;
  StackValue: TGocciaValue;
begin
  if ExtractThrowLocation(AThrown, ErrorName, ErrorMessage, Line, Col) and
     Assigned(ASourceLines) and (Line > 0) and (Line <= ASourceLines.Count) then
    Result := FormatErrorWithSourceContext(
      ErrorName, ErrorMessage, AFileName, Line, Col, ASourceLines, AUseColor)
  else
  begin
    // Fallback: just show the stack trace or message
    if AThrown is TGocciaObjectValue then
    begin
      StackValue := TGocciaObjectValue(AThrown).GetProperty(PROP_STACK);
      if Assigned(StackValue) and (StackValue is TGocciaStringLiteralValue) and
         (TGocciaStringLiteralValue(StackValue).Value <> '') then
        Result := TGocciaStringLiteralValue(StackValue).Value
      else
        Result := AThrown.ToStringLiteral.Value;
    end
    else
      Result := AThrown.ToStringLiteral.Value;
  end;
end;

end.
