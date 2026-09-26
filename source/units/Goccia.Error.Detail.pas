unit Goccia.Error.Detail;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Values.Primitives;

{ Formats a detailed error message for a TGocciaThrowValue, including source
  context with caret pointer when the engine recorded source provenance for the
  error. Falls back to the error's own message/stack text otherwise. The
  fallback never invokes user toString()/valueOf() — by the time this is called
  the bytecode VM has unwound and re-entering it would raise a Pascal
  range-check error.

  Security: the code frame is rendered ONLY from provenance the engine captured
  onto the error object at creation (Goccia.Values.ErrorHelper), never from the
  thrown value's guest-writable `stack` string. A forged error object gets no
  code frame. See docs/module-resolution.md "Runtime code frames".

  AGuestBound marks output that is handed back to guest code (a sandbox
  runScript child's stderr, a benchmark result's error). It then carries no
  host-side suggestion: neither the one recorded on the error nor an
  ASuggestion equal to it. }
function FormatThrowDetail(const AThrown: TGocciaValue;
  const AFileName: string; const ASourceLines: TStringList;
  const AUseColor: Boolean; const AExpectedPrincipal: Int64;
  const ASuggestion: string = ''; const AGuestBound: Boolean = False): string;

{ Returns the thrown value's `name` when it is a string data property on the
  object or its prototype chain, and 'Error' otherwise. Like FormatThrowDetail,
  it never invokes accessors, so it is safe after the bytecode VM has unwound. }
function ThrownErrorName(const AThrown: TGocciaValue): string;

implementation

uses
  SysUtils,

  TextSemantics,

  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.Terminal.Colors,
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

    if (Current = AObject) and (AName = PROP_STACK) and
       AObject.HasErrorData and (AObject.ErrorStack <> '') then
    begin
      AValue := AObject.ErrorStack;
      Exit(True);
    end;

    Current := Current.Prototype;
  end;
end;

function ThrownErrorName(const AThrown: TGocciaValue): string;
begin
  if not ((AThrown is TGocciaObjectValue) and
     TryGetStringDataProperty(TGocciaObjectValue(AThrown), PROP_NAME,
       Result)) then
    Result := 'Error';
end;

function FormatThrowDetail(const AThrown: TGocciaValue;
  const AFileName: string; const ASourceLines: TStringList;
  const AUseColor: Boolean; const AExpectedPrincipal: Int64;
  const ASuggestion: string; const AGuestBound: Boolean): string;
var
  ErrorObject: TGocciaErrorObjectValue;
  ErrorName, ErrorMessage: string;
  StackText, MessageText, NameText: string;
  ExcerptLines: TStringList;
  SourceAuthorized: Boolean;
  HostSuggestion, Suggestion: string;
begin
  { A denial's host-side suggestion travels on the error itself, so it is
    shown even when the error reached the host through a rejected promise
    rather than the exception that first carried it. }
  Suggestion := ASuggestion;
  if AThrown is TGocciaErrorObjectValue then
  begin
    HostSuggestion := TGocciaErrorObjectValue(AThrown).ErrorHostSuggestion;
    if AGuestBound then
    begin
      if (HostSuggestion <> '') and (Suggestion = HostSuggestion) then
        Suggestion := '';
    end
    else if Suggestion = '' then
      Suggestion := HostSuggestion;
  end;

  // A code frame is rendered ONLY from the engine's own recorded provenance,
  // captured onto the error object when the engine created it (see
  // Goccia.Values.ErrorHelper.AttachErrorSourceProvenance). The thrown value's
  // `stack` string is guest-writable and is never used to choose a file or a
  // line, so a forged `{ stack: "...at f (/etc/passwd:1:1)" }` object — which
  // carries no engine provenance — gets no code frame and cannot disclose any
  // file. The captured excerpt travels on the error, so this renders correctly
  // even after the throwing engine has been freed.
  if (AThrown is TGocciaErrorObjectValue) and
     TGocciaErrorObjectValue(AThrown).HasErrorData and
     TGocciaErrorObjectValue(AThrown).HasErrorSourceLocation then
  begin
    ErrorObject := TGocciaErrorObjectValue(AThrown);
    ErrorName := 'Error';
    if TryGetStringDataProperty(ErrorObject, PROP_NAME, NameText) then
      ErrorName := NameText;
    ErrorMessage := '';
    if TryGetStringDataProperty(ErrorObject, PROP_MESSAGE, MessageText) then
      ErrorMessage := MessageText;

    // RENDER-TIME PRINCIPAL ENFORCEMENT. Authorization comes only from the host
    // that owns this render operation, explicitly threaded through every
    // formatter call. Ambient execution state is not authority: after Execute
    // unwinds there is normally no active scope, and treating that absence as
    // authorization disclosed retained cross-engine errors. Zero means the
    // caller supplied no principal and therefore authorizes source from none.
    SourceAuthorized := (AExpectedPrincipal <> 0) and
      (AExpectedPrincipal = ErrorObject.ErrorSourcePrincipal);

    if SourceAuthorized and (ErrorObject.ErrorSourceExcerpt <> '') then
    begin
      // Render from the ±context window captured at throw time. No file is
      // read and no path is looked up here.
      ExcerptLines := CreateECMAScriptSourceLines(ErrorObject.ErrorSourceExcerpt);
      try
        Exit(FormatErrorWithSourceContext(ErrorName, ErrorMessage,
          ErrorObject.ErrorSourcePath, ErrorObject.ErrorSourceLine,
          ErrorObject.ErrorSourceColumn, ExcerptLines, AUseColor, Suggestion,
          ErrorObject.ErrorSourceExcerptFirstLine));
      finally
        ExcerptLines.Free;
      end;
    end
    else if SourceAuthorized and (ErrorObject.ErrorSourcePath = AFileName) and
            Assigned(ASourceLines) then
      // Entry file: no excerpt was captured (the entry is not a loaded module),
      // but the host still holds its full lines. The path is the engine-recorded
      // one, matched against the current run's entry — never a guest path. The
      // same principal gate applies: a parent formatting a child's entry error
      // (child source passed as ASourceLines) is not authorized, so the entry
      // lines are withheld and only the location is shown.
      Exit(FormatErrorWithSourceContext(ErrorName, ErrorMessage,
        ErrorObject.ErrorSourcePath, ErrorObject.ErrorSourceLine,
        ErrorObject.ErrorSourceColumn, ASourceLines, AUseColor, Suggestion))
    else if ErrorObject.ErrorSourceLine > 0 then
      // Provenance recorded, but the source is unavailable (not the entry, not
      // a captured module). Show the header/location without a code frame rather
      // than quoting any other file.
      Exit(FormatErrorWithSourceContext(ErrorName, ErrorMessage,
        ErrorObject.ErrorSourcePath, ErrorObject.ErrorSourceLine,
        ErrorObject.ErrorSourceColumn, nil, AUseColor, Suggestion));
  end;

  // Fallback: forged error object, thrown non-error object, or a primitive —
  // no engine provenance, so no code frame. Echo the value's own message/stack
  // text (the guest's own data; no file is read). Never invokes user
  // toString()/valueOf(): the bytecode VM has already unwound.
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
  if Suggestion <> '' then
    Result := Result + sLineBreak + '  ' +
      Colorize('Suggestion: ' + Suggestion, ANSI_YELLOW, AUseColor);
end;

end.
