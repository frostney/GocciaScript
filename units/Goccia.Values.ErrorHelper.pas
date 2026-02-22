unit Goccia.Values.ErrorHelper;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue;

{ Creates a JavaScript error object with the given name and message.
  ASkipTop controls how many frames to skip from the top of the call stack. }
function CreateErrorObject(const AName, AMessage: string; const ASkipTop: Integer = 0): TGocciaObjectValue;

{ Raises a TGocciaThrowValue with a TypeError }
procedure ThrowTypeError(const AMessage: string);

{ Raises a TGocciaThrowValue with a RangeError }
procedure ThrowRangeError(const AMessage: string);

{ Raises a TGocciaThrowValue with a ReferenceError }
procedure ThrowReferenceError(const AMessage: string);

{ Raises a TGocciaThrowValue with a SyntaxError }
procedure ThrowSyntaxError(const AMessage: string);

{ Raises a TGocciaThrowValue with a DataCloneError (DOMException with code 25) }
procedure ThrowDataCloneError(const AMessage: string);

{ Raises a TGocciaThrowValue with a generic Error }
procedure ThrowError(const AMessage: string);

implementation

uses
  Goccia.Builtins.Globals,
  Goccia.CallStack,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Values.Error,
  Goccia.Values.Primitives;

function CreateErrorObject(const AName, AMessage: string; const ASkipTop: Integer = 0): TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create;
  Result.AssignProperty(PROP_NAME, TGocciaStringLiteralValue.Create(AName));
  Result.AssignProperty(PROP_MESSAGE, TGocciaStringLiteralValue.Create(AMessage));

  if Assigned(TGocciaCallStack.Instance) then
    Result.AssignProperty(PROP_STACK,
      TGocciaStringLiteralValue.Create(
        TGocciaCallStack.Instance.CaptureStackTrace(AName, AMessage, ASkipTop)));
end;

procedure ThrowTypeError(const AMessage: string);
begin
  raise TGocciaThrowValue.Create(CreateErrorObject(TYPE_ERROR_NAME, AMessage));
end;

procedure ThrowRangeError(const AMessage: string);
begin
  raise TGocciaThrowValue.Create(CreateErrorObject(RANGE_ERROR_NAME, AMessage));
end;

procedure ThrowReferenceError(const AMessage: string);
begin
  raise TGocciaThrowValue.Create(CreateErrorObject(REFERENCE_ERROR_NAME, AMessage));
end;

procedure ThrowSyntaxError(const AMessage: string);
begin
  raise TGocciaThrowValue.Create(CreateErrorObject(SYNTAX_ERROR_NAME, AMessage));
end;

procedure ThrowDataCloneError(const AMessage: string);
var
  ErrorObj: TGocciaObjectValue;
begin
  ErrorObj := CreateErrorObject(DATA_CLONE_ERROR_NAME, AMessage);
  ErrorObj.AssignProperty(PROP_CODE, TGocciaNumberLiteralValue.Create(25));
  if Assigned(GDOMExceptionProto) then
    ErrorObj.Prototype := GDOMExceptionProto;
  raise TGocciaThrowValue.Create(ErrorObj);
end;

procedure ThrowError(const AMessage: string);
begin
  raise TGocciaThrowValue.Create(CreateErrorObject(ERROR_NAME, AMessage));
end;

end.
