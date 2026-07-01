unit Goccia.Error.Detail;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Values.Primitives;

{ Extracts the first stack frame's line, column, and filename from a JS error
  object's stack trace string. Returns True if a location was found. }
function ExtractThrowLocation(const AThrown: TGocciaValue;
  out AErrorName, AErrorMessage, AFrameFileName: string;
  out ALine, AColumn: Integer): Boolean;

{ Formats a detailed error message for a TGocciaThrowValue, including source
  context with caret pointer when source lines and location are available.
  Falls back to the stack trace or a debug-format placeholder when context is
  unavailable. The fallback never invokes user toString()/valueOf() — by the
  time this is called the bytecode VM has unwound and re-entering it would
  raise a Pascal range-check error. }
function FormatThrowDetail(const AThrown: TGocciaValue;
  const AFileName: string; const ASourceLines: TStringList;
  const AUseColor: Boolean; const ASuggestion: string = ''): string;

implementation

uses
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue;

function TryGetStringDataProperty(const AObject: TGocciaObjectValue;
  const AName: string; out AValue: string): Boolean;
var
  Current: TGocciaObjectValue;
  Descriptor: TGocciaPropertyDescriptor;
  Value: TGocciaValue;
begin
  Result := False;
  AValue := '';
  Current := AObject;

  while Assigned(Current) do
  begin
    Descriptor := Current.GetOwnPropertyDescriptor(AName);
    if Assigned(Descriptor) then
    begin
      if Descriptor is TGocciaPropertyDescriptorData then
      begin
        Value := TGocciaPropertyDescriptorData(Descriptor).Value;
        if Value is TGocciaStringLiteralValue then
        begin
          AValue := TGocciaStringLiteralValue(Value).Value;
          Exit(True);
        end;
      end;
      Exit(False);
    end;

    Current := Current.Prototype;
  end;
end;

function ExtractThrowLocation(const AThrown: TGocciaValue;
  out AErrorName, AErrorMessage, AFrameFileName: string;
  out ALine, AColumn: Integer): Boolean;
var
  TextValue: string;
  StackStr: string;
  AtPos, ParenPos, ColonPos1, ColonPos2, I: Integer;
begin
  Result := False;
  ALine := 0;
  AColumn := 0;
  AErrorName := 'Error';
  AErrorMessage := '';
  AFrameFileName := '';

  if not (AThrown is TGocciaObjectValue) then Exit;

  if TryGetStringDataProperty(TGocciaObjectValue(AThrown), PROP_NAME, TextValue) then
    AErrorName := TextValue;
  if TryGetStringDataProperty(TGocciaObjectValue(AThrown), PROP_MESSAGE, TextValue) then
    AErrorMessage := TextValue;

  if not TryGetStringDataProperty(TGocciaObjectValue(AThrown), PROP_STACK, StackStr) then Exit;

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

  if not TryStrToInt(Copy(StackStr, ColonPos2 + 1, I - ColonPos2), AColumn) then Exit;

  // Parse ":line" before that
  ColonPos1 := ColonPos2 - 1;
  while (ColonPos1 > ParenPos) and (StackStr[ColonPos1] <> ':') do
    Dec(ColonPos1);
  if ColonPos1 <= ParenPos then Exit;

  if not TryStrToInt(Copy(StackStr, ColonPos1 + 1, ColonPos2 - ColonPos1 - 1), ALine) then Exit;

  // Extract filename between '(' and the first ':'
  AFrameFileName := Copy(StackStr, ParenPos + 1, ColonPos1 - ParenPos - 1);
  Result := True;
end;

function FormatThrowDetail(const AThrown: TGocciaValue;
  const AFileName: string; const ASourceLines: TStringList;
  const AUseColor: Boolean; const ASuggestion: string = ''): string;
var
  ErrorName, ErrorMessage, FrameFileName: string;
  Line, Col: Integer;
  EffectiveFileName: string;
  StackText, MessageText, NameText: string;
begin
  if ExtractThrowLocation(AThrown, ErrorName, ErrorMessage, FrameFileName, Line, Col) and
     Assigned(ASourceLines) and (Line > 0) and (Line <= ASourceLines.Count) then
  begin
    if FrameFileName <> '' then
      EffectiveFileName := FrameFileName
    else
      EffectiveFileName := AFileName;

    Result := FormatErrorWithSourceContext(
      ErrorName, ErrorMessage, EffectiveFileName, Line, Col, ASourceLines,
      AUseColor, ASuggestion);
  end
  else
  begin
    // Fallback when no parseable stack frame is available. Use the stack
    // string if present; otherwise render a static debug representation that
    // does not touch user toString()/valueOf() — the bytecode VM has already
    // unwound by the time this runs and re-entry is unsafe.
    if AThrown is TGocciaObjectValue then
    begin
      if TryGetStringDataProperty(TGocciaObjectValue(AThrown), PROP_STACK, StackText) and
         (StackText <> '') then
        Result := StackText
      else if TryGetStringDataProperty(TGocciaObjectValue(AThrown), PROP_MESSAGE, MessageText) and
              (MessageText <> '') then
      begin
        if TryGetStringDataProperty(TGocciaObjectValue(AThrown), PROP_NAME, NameText) and
           (NameText <> '') then
          Result := Format('%s: %s', [NameText, MessageText])
        else
          Result := MessageText;
      end
      else
        Result := Format('[object %s]', [TGocciaObjectValue(AThrown).ToStringTag]);
    end
    else if AThrown is TGocciaSymbolValue then
      Result := TGocciaSymbolValue(AThrown).ToDisplayString.Value
    else
      // Primitive non-Symbol values: ToStringLiteral cannot invoke user code
      // and cannot throw, so it is safe to call here.
      Result := AThrown.ToStringLiteral.Value;
  end;
end;

end.
