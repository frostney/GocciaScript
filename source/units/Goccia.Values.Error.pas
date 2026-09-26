unit Goccia.Values.Error;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Values.Primitives;

type
  // TGocciaThrowValue is the exception type for JS throw statements and runtime
  // errors (ThrowTypeError, ThrowRangeError, etc.). It propagates naturally through
  // the call stack and is caught by EvaluateTry (JS try/catch), async wrappers, or
  // the top-level engine. Return and break use TGocciaControlFlow records instead.
  TGocciaThrowValue = class(Exception)
  private
    FValue: TGocciaValue;
    FSuggestion: string;
    FSuggestionIsHostOnly: Boolean;
  public
    constructor Create(const AValue: TGocciaValue); overload;
    constructor Create(const AValue: TGocciaValue;
      const ASuggestion: string;
      const ASuggestionIsHostOnly: Boolean = False); overload;
    property Value: TGocciaValue read FValue;
    property Suggestion: string read FSuggestion;
    { True when Suggestion is host-side advice (a PermissionDenied's grant,
      option, or canonical path). Output handed back to guest code must
      never carry such a suggestion. }
    property SuggestionIsHostOnly: Boolean read FSuggestionIsHostOnly;
  end;

implementation

{ TGocciaThrowValue }

constructor TGocciaThrowValue.Create(const AValue: TGocciaValue);
begin
  inherited Create('');
  FValue := AValue;
  FSuggestion := '';
  FSuggestionIsHostOnly := False;
end;

constructor TGocciaThrowValue.Create(const AValue: TGocciaValue;
  const ASuggestion: string; const ASuggestionIsHostOnly: Boolean);
begin
  inherited Create('');
  FValue := AValue;
  FSuggestion := ASuggestion;
  FSuggestionIsHostOnly := ASuggestionIsHostOnly;
end;

end.
