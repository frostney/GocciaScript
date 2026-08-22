unit Goccia.Error.CallDiagnostics;

{$I Goccia.inc}

{ Shared "value is not callable / not a constructor" diagnostics.

  Both executors have to name the callee the way the author wrote it — the
  tree-walk evaluator reads it off the call expression's source span, the
  bytecode VM off the per-call-site descriptor its compiler records. Building
  the message and the suggestion here is what keeps the two modes textually
  identical: neither executor formats these strings itself. }

interface

type
  { A call site's callee, described well enough to build the same diagnostic
    from either executor.

    CalleeText is the callee expression's source text, normalized (see
    NormalizeCalleeText); '' when it is unavailable or unreasonably long, in
    which case the diagnostics fall back to the runtime type name.

    ObjectText and PropertyName are filled only for a non-computed member
    callee whose object is a plain identifier (`obj.method()`), which is the
    one shape that supports the "does not have method" suggestion. }
  TGocciaCalleeDescriptor = record
    CalleeText: string;
    ObjectText: string;
    PropertyName: string;
  end;

{ Longest callee text carried into a diagnostic. A callee spanning more than
  this is a multi-line chain whose text would swamp the message, so it is
  dropped and the type-name form is used instead. The limit is shared so both
  executors drop the same texts. }
const
  GOCCIA_MAX_CALLEE_TEXT_LENGTH = 96;

function EmptyCalleeDescriptor: TGocciaCalleeDescriptor;

{ Collapses interior whitespace runs (including newlines) to single spaces and
  trims the ends, then drops the text entirely when it is longer than
  GOCCIA_MAX_CALLEE_TEXT_LENGTH. }
function NormalizeCalleeText(const AText: string): string;

{ True when AText is a single plain identifier — the shape whose suggestion
  reads "'name' is of type '...' and cannot be called as a function". }
function IsPlainIdentifierText(const AText: string): Boolean;

{ "obj.method is not a function" / "<type> is not a function". }
function NotCallableMessage(const ADescriptor: TGocciaCalleeDescriptor;
  const ACalleeTypeName: string): string;

{ The matching suggestion line. AReceiverTypeName is the type of the member
  callee's receiver and is ignored for the non-member shapes. }
function NotCallableSuggestion(const ADescriptor: TGocciaCalleeDescriptor;
  const AReceiverTypeName, ACalleeTypeName: string): string;

{ "obj.Klass is not a constructor" / "<type> is not a constructor". }
function NotConstructorMessage(const ADescriptor: TGocciaCalleeDescriptor;
  const ACalleeTypeName: string): string;

{ The matching suggestion line for a failed `new`. }
function NotConstructorSuggestion(const ADescriptor: TGocciaCalleeDescriptor;
  const ACalleeTypeName: string): string;

implementation

uses
  SysUtils,

  Goccia.Error.Messages,
  Goccia.Error.Suggestions;

resourcestring
  SSuggestMemberNotMethod =
    '''%s'' is of type ''%s'' which does not have method ''%s''';
  SSuggestIdentifierNotCallable =
    '''%s'' is of type ''%s'' and cannot be called as a function';
  SSuggestIdentifierNotConstructor =
    '''%s'' is of type ''%s'' and cannot be used with ''new''';
  SSuggestValueNotConstructor =
    'values of type ''%s'' cannot be used with ''new''';

function EmptyCalleeDescriptor: TGocciaCalleeDescriptor;
begin
  Result.CalleeText := '';
  Result.ObjectText := '';
  Result.PropertyName := '';
end;

function NormalizeCalleeText(const AText: string): string;
var
  I, Written: Integer;
  PendingSpace: Boolean;
  Ch: Char;
begin
  Result := '';
  // Collapsing only ever shrinks the text, so anything this far over the limit
  // cannot come back under it — and walking it would be wasted compile time,
  // since every call site in a program runs through here.
  if Length(AText) > GOCCIA_MAX_CALLEE_TEXT_LENGTH * 8 then
    Exit;
  SetLength(Result, Length(AText));
  Written := 0;
  PendingSpace := False;
  for I := 1 to Length(AText) do
  begin
    Ch := AText[I];
    if (Ch = ' ') or (Ch = #9) or (Ch = #10) or (Ch = #13) then
    begin
      PendingSpace := Written > 0;
      Continue;
    end;
    if PendingSpace then
    begin
      Inc(Written);
      Result[Written] := ' ';
      PendingSpace := False;
    end;
    Inc(Written);
    Result[Written] := Ch;
  end;
  if Written > GOCCIA_MAX_CALLEE_TEXT_LENGTH then
    Result := ''
  else
    SetLength(Result, Written);
end;

function IsPlainIdentifierText(const AText: string): Boolean;
var
  I: Integer;
  Ch: Char;
begin
  Result := False;
  if AText = '' then
    Exit;
  for I := 1 to Length(AText) do
  begin
    Ch := AText[I];
    if (Ch >= 'a') and (Ch <= 'z') then
      Continue;
    if (Ch >= 'A') and (Ch <= 'Z') then
      Continue;
    if (Ch = '_') or (Ch = '$') then
      Continue;
    if (Ch >= '0') and (Ch <= '9') then
    begin
      if I = 1 then
        Exit;
      Continue;
    end;
    // Non-ASCII identifier parts (the lexer already decoded any \u escapes)
    // are accepted: they cannot be operators or punctuation.
    if Ch >= #128 then
      Continue;
    Exit;
  end;
  Result := True;
end;

function NotCallableMessage(const ADescriptor: TGocciaCalleeDescriptor;
  const ACalleeTypeName: string): string;
begin
  if ADescriptor.CalleeText <> '' then
    Result := Format(SErrorValueNotFunction, [ADescriptor.CalleeText])
  else
    Result := Format(SErrorValueNotFunction, [ACalleeTypeName]);
end;

function NotCallableSuggestion(const ADescriptor: TGocciaCalleeDescriptor;
  const AReceiverTypeName, ACalleeTypeName: string): string;
begin
  if (ADescriptor.PropertyName <> '') and (ADescriptor.ObjectText <> '') then
    Result := Format(SSuggestMemberNotMethod,
      [ADescriptor.ObjectText, AReceiverTypeName, ADescriptor.PropertyName])
  else if ADescriptor.PropertyName <> '' then
    Result := Format(SSuggestMemberNotMethod,
      [AReceiverTypeName, AReceiverTypeName, ADescriptor.PropertyName])
  else if IsPlainIdentifierText(ADescriptor.CalleeText) then
    Result := Format(SSuggestIdentifierNotCallable,
      [ADescriptor.CalleeText, ACalleeTypeName])
  else
    Result := SSuggestNotFunctionType;
end;

function NotConstructorMessage(const ADescriptor: TGocciaCalleeDescriptor;
  const ACalleeTypeName: string): string;
begin
  if ADescriptor.CalleeText <> '' then
    Result := Format(SErrorValueNotConstructor, [ADescriptor.CalleeText])
  else
    Result := Format(SErrorValueNotConstructor, [ACalleeTypeName]);
end;

function NotConstructorSuggestion(const ADescriptor: TGocciaCalleeDescriptor;
  const ACalleeTypeName: string): string;
begin
  if IsPlainIdentifierText(ADescriptor.CalleeText) then
    Result := Format(SSuggestIdentifierNotConstructor,
      [ADescriptor.CalleeText, ACalleeTypeName])
  else
    Result := Format(SSuggestValueNotConstructor, [ACalleeTypeName]);
end;

end.
