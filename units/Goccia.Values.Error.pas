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
  public
    constructor Create(const AValue: TGocciaValue); overload;
    constructor Create(const AValue: TGocciaValue;
      const ASuggestion: string); overload;
    property Value: TGocciaValue read FValue;
    property Suggestion: string read FSuggestion;
  end;

implementation

{ TGocciaThrowValue }

constructor TGocciaThrowValue.Create(const AValue: TGocciaValue);
begin
  inherited Create('');
  FValue := AValue;
  FSuggestion := '';
end;

constructor TGocciaThrowValue.Create(const AValue: TGocciaValue;
  const ASuggestion: string);
begin
  inherited Create('');
  FValue := AValue;
  FSuggestion := ASuggestion;
end;

end.
