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
  public
    constructor Create(const AValue: TGocciaValue);
    property Value: TGocciaValue read FValue;
  end;

implementation

{ TGocciaThrowValue }

constructor TGocciaThrowValue.Create(const AValue: TGocciaValue);
begin
  inherited Create('');
  FValue := AValue;
end;

end.
