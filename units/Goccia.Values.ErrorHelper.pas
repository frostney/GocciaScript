unit Goccia.Values.ErrorHelper;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue;

{ Creates a JavaScript error object with the given name and message.
  ASkipTop controls how many frames to skip from the top of the call stack. }
function CreateErrorObject(const AName, AMessage: string; const ASkipTop: Integer = 0): TGocciaObjectValue;

{ Raises a TGocciaThrowValue with a TypeError }
procedure ThrowTypeError(const AMessage: string); overload;
procedure ThrowTypeError(const AMessage, ASuggestion: string); overload;

{ Raises a TGocciaThrowValue with a RangeError }
procedure ThrowRangeError(const AMessage: string); overload;
procedure ThrowRangeError(const AMessage, ASuggestion: string); overload;

{ Raises a TGocciaThrowValue with a ReferenceError }
procedure ThrowReferenceError(const AMessage: string); overload;
procedure ThrowReferenceError(const AMessage, ASuggestion: string); overload;

{ Raises a TGocciaThrowValue with a SyntaxError }
procedure ThrowSyntaxError(const AMessage: string); overload;
procedure ThrowSyntaxError(const AMessage, ASuggestion: string); overload;

{ Raises a TGocciaThrowValue with a DataCloneError (DOMException with code 25) }
procedure ThrowDataCloneError(const AMessage: string); overload;
procedure ThrowDataCloneError(const AMessage, ASuggestion: string); overload;

{ Raises a TGocciaThrowValue with an InvalidCharacterError (DOMException with code 5) }
procedure ThrowInvalidCharacterError(const AMessage: string);

{ Raises a TGocciaThrowValue with a URIError }
procedure ThrowURIError(const AMessage: string); overload;
procedure ThrowURIError(const AMessage, ASuggestion: string); overload;

{ Raises a TGocciaThrowValue with a generic Error }
procedure ThrowError(const AMessage: string); overload;
procedure ThrowError(const AMessage, ASuggestion: string); overload;

implementation

uses
  Goccia.Builtins.Globals,
  Goccia.CallStack,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Values.Error,
  Goccia.Values.Primitives;

function GetErrorPrototype(const AName: string): TGocciaObjectValue;
begin
  if AName = TYPE_ERROR_NAME then
    Result := GTypeErrorProto
  else if AName = RANGE_ERROR_NAME then
    Result := GRangeErrorProto
  else if AName = REFERENCE_ERROR_NAME then
    Result := GReferenceErrorProto
  else if AName = SYNTAX_ERROR_NAME then
    Result := GSyntaxErrorProto
  else if AName = URI_ERROR_NAME then
    Result := GURIErrorProto
  else if AName = AGGREGATE_ERROR_NAME then
    Result := GAggregateErrorProto
  else if AName = SUPPRESSED_ERROR_NAME then
    Result := GSuppressedErrorProto
  else if AName = ERROR_NAME then
    Result := GErrorProto
  else
    Result := GErrorProto;
end;

// ES2026 §10.4.4.4 [[ErrorData]]
function CreateErrorObject(const AName, AMessage: string; const ASkipTop: Integer = 0): TGocciaObjectValue;
var
  Proto: TGocciaObjectValue;
begin
  Proto := GetErrorPrototype(AName);
  if Assigned(Proto) then
    Result := TGocciaObjectValue.Create(Proto)
  else
    Result := TGocciaObjectValue.Create;
  Result.HasErrorData := True;
  Result.AssignProperty(PROP_NAME, TGocciaStringLiteralValue.Create(AName));
  Result.AssignProperty(PROP_MESSAGE, TGocciaStringLiteralValue.Create(AMessage));

  if Assigned(TGocciaCallStack.Instance) then
    Result.AssignProperty(PROP_STACK,
      TGocciaStringLiteralValue.Create(
        TGocciaCallStack.Instance.CaptureStackTrace(AName, AMessage, ASkipTop)));
end;

{ Shared raise helper — creates the error object and raises with optional suggestion }
procedure RaiseNativeError(const AErrorName, AMessage, ASuggestion: string);
begin
  raise TGocciaThrowValue.Create(
    CreateErrorObject(AErrorName, AMessage), ASuggestion);
end;

procedure ThrowTypeError(const AMessage: string);
begin
  raise TGocciaThrowValue.Create(CreateErrorObject(TYPE_ERROR_NAME, AMessage));
end;

procedure ThrowTypeError(const AMessage, ASuggestion: string);
begin
  RaiseNativeError(TYPE_ERROR_NAME, AMessage, ASuggestion);
end;

procedure ThrowRangeError(const AMessage: string);
begin
  raise TGocciaThrowValue.Create(CreateErrorObject(RANGE_ERROR_NAME, AMessage));
end;

procedure ThrowRangeError(const AMessage, ASuggestion: string);
begin
  RaiseNativeError(RANGE_ERROR_NAME, AMessage, ASuggestion);
end;

procedure ThrowReferenceError(const AMessage: string);
begin
  raise TGocciaThrowValue.Create(CreateErrorObject(REFERENCE_ERROR_NAME, AMessage));
end;

procedure ThrowReferenceError(const AMessage, ASuggestion: string);
begin
  RaiseNativeError(REFERENCE_ERROR_NAME, AMessage, ASuggestion);
end;

procedure ThrowSyntaxError(const AMessage: string);
begin
  raise TGocciaThrowValue.Create(CreateErrorObject(SYNTAX_ERROR_NAME, AMessage));
end;

procedure ThrowSyntaxError(const AMessage, ASuggestion: string);
begin
  RaiseNativeError(SYNTAX_ERROR_NAME, AMessage, ASuggestion);
end;

procedure ThrowDataCloneError(const AMessage: string);
var
  ErrorObj: TGocciaObjectValue;
begin
  ErrorObj := CreateErrorObject(DATA_CLONE_ERROR_NAME, AMessage);
  ErrorObj.HasErrorData := False;
  ErrorObj.AssignProperty(PROP_CODE, TGocciaNumberLiteralValue.Create(25));
  if Assigned(GDOMExceptionProto) then
    ErrorObj.Prototype := GDOMExceptionProto;
  raise TGocciaThrowValue.Create(ErrorObj);
end;

procedure ThrowDataCloneError(const AMessage, ASuggestion: string);
var
  ErrorObj: TGocciaObjectValue;
begin
  ErrorObj := CreateErrorObject(DATA_CLONE_ERROR_NAME, AMessage);
  ErrorObj.HasErrorData := False;
  ErrorObj.AssignProperty(PROP_CODE, TGocciaNumberLiteralValue.Create(25));
  if Assigned(GDOMExceptionProto) then
    ErrorObj.Prototype := GDOMExceptionProto;
  raise TGocciaThrowValue.Create(ErrorObj, ASuggestion);
end;

procedure ThrowInvalidCharacterError(const AMessage: string);
var
  ErrorObj: TGocciaObjectValue;
begin
  ErrorObj := CreateErrorObject(INVALID_CHARACTER_ERROR_NAME, AMessage);
  ErrorObj.HasErrorData := False;
  ErrorObj.AssignProperty(PROP_CODE, TGocciaNumberLiteralValue.Create(5));
  if Assigned(GDOMExceptionProto) then
    ErrorObj.Prototype := GDOMExceptionProto;
  raise TGocciaThrowValue.Create(ErrorObj);
end;

procedure ThrowURIError(const AMessage: string);
begin
  raise TGocciaThrowValue.Create(CreateErrorObject(URI_ERROR_NAME, AMessage));
end;

procedure ThrowURIError(const AMessage, ASuggestion: string);
begin
  RaiseNativeError(URI_ERROR_NAME, AMessage, ASuggestion);
end;

procedure ThrowError(const AMessage: string);
begin
  raise TGocciaThrowValue.Create(CreateErrorObject(ERROR_NAME, AMessage));
end;

procedure ThrowError(const AMessage, ASuggestion: string);
begin
  RaiseNativeError(ERROR_NAME, AMessage, ASuggestion);
end;

end.
