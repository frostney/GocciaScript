unit Goccia.Values.ErrorHelper;

{$I Goccia.inc}

interface

uses
  Goccia.Realm,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

{ Creates a JavaScript error object with the given name and message.
  ASkipTop controls how many frames to skip from the top of the call stack. }
function CreateErrorObject(const AName, AMessage: string; const ASkipTop: Integer = 0): TGocciaObjectValue;

{ True when AValue carries [[ErrorData]] — the slot every error constructor
  installs, including through a `class MyError extends Error` subclass. Merely
  inheriting from Error.prototype does not make an object an error.

  DOMException clears the slot on purpose, so it compares as an ordinary
  object through its enumerable name/message/code. That is a deliberate,
  project-level divergence from Vitest, which treats every DOMException as
  equal to every other one regardless of name or message. }
function IsErrorObject(const AValue: TGocciaValue): Boolean;

{ Creates a DOMException object with the standard legacy code for AName. }
function CreateDOMExceptionObject(const AName, AMessage: string;
  const ASkipTop: Integer = 0): TGocciaObjectValue;

{ Raises a TGocciaThrowValue with a TypeError }
procedure ThrowTypeError(const AMessage: string); overload;
procedure ThrowTypeError(const AMessage, ASuggestion: string); overload;
procedure ThrowTypeErrorInRealm(const AMessage, ASuggestion: string;
  const ARealm: TGocciaRealm); overload;

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
  Goccia.Values.ObjectPropertyDescriptor;

function DOMExceptionLegacyCode(const AName: string): Integer;
begin
  if AName = DATA_CLONE_ERROR_NAME then
    Result := 25
  else if AName = INVALID_CHARACTER_ERROR_NAME then
    Result := 5
  else if AName = ABORT_ERROR_NAME then
    Result := 20
  else if AName = TIMEOUT_ERROR_NAME then
    Result := 23
  else if AName = INVALID_STATE_ERROR_NAME then
    Result := 11
  else
    Result := 0;
end;

function GetErrorPrototype(const AName: string): TGocciaObjectValue;
begin
  if AName = TYPE_ERROR_NAME then
    Result := GetTypeErrorProto
  else if AName = EVAL_ERROR_NAME then
    Result := GetEvalErrorProto
  else if AName = RANGE_ERROR_NAME then
    Result := GetRangeErrorProto
  else if AName = REFERENCE_ERROR_NAME then
    Result := GetReferenceErrorProto
  else if AName = SYNTAX_ERROR_NAME then
    Result := GetSyntaxErrorProto
  else if AName = URI_ERROR_NAME then
    Result := GetURIErrorProto
  else if AName = AGGREGATE_ERROR_NAME then
    Result := GetAggregateErrorProto
  else if AName = SUPPRESSED_ERROR_NAME then
    Result := GetSuppressedErrorProto
  else if AName = ERROR_NAME then
    Result := GetErrorProto
  else
    Result := GetErrorProto;
end;

function IsErrorObject(const AValue: TGocciaValue): Boolean;
begin
  Result := (AValue is TGocciaObjectValue) and
    TGocciaObjectValue(AValue).HasErrorData;
end;

function CreateDOMExceptionObject(const AName, AMessage: string;
  const ASkipTop: Integer): TGocciaObjectValue;
begin
  Result := CreateErrorObject(AName, AMessage, ASkipTop);
  Result.HasErrorData := False;
  if GetDOMExceptionProto <> nil then
    Result.Prototype := GetDOMExceptionProto;
  if Result.ErrorStack <> '' then
    Result.DefineProperty(PROP_STACK,
      TGocciaPropertyDescriptorData.Create(
        TGocciaStringLiteralValue.Create(Result.ErrorStack),
        [pfConfigurable, pfWritable]));
  Result.AssignProperty(PROP_CODE,
    TGocciaNumberLiteralValue.Create(DOMExceptionLegacyCode(AName)));
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

  if (TGocciaCallStack.Instance <> nil) then
    Result.ErrorStack :=
      TGocciaCallStack.Instance.CaptureStackTrace(AName, AMessage, ASkipTop);
end;

function CreateErrorObjectInRealm(const AName, AMessage: string;
  const ASkipTop: Integer; const ARealm: TGocciaRealm): TGocciaObjectValue;
var
  PreviousRealm: TGocciaRealm;
begin
  if (not Assigned(ARealm)) or (ARealm = CurrentRealm) then
    Exit(CreateErrorObject(AName, AMessage, ASkipTop));

  PreviousRealm := CurrentRealm;
  SetCurrentRealm(ARealm);
  try
    Result := CreateErrorObject(AName, AMessage, ASkipTop);
  finally
    SetCurrentRealm(PreviousRealm);
  end;
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

procedure ThrowTypeErrorInRealm(const AMessage, ASuggestion: string;
  const ARealm: TGocciaRealm);
begin
  raise TGocciaThrowValue.Create(
    CreateErrorObjectInRealm(TYPE_ERROR_NAME, AMessage, 0, ARealm),
    ASuggestion);
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
  ErrorObj := CreateDOMExceptionObject(DATA_CLONE_ERROR_NAME, AMessage);
  raise TGocciaThrowValue.Create(ErrorObj);
end;

procedure ThrowDataCloneError(const AMessage, ASuggestion: string);
var
  ErrorObj: TGocciaObjectValue;
begin
  ErrorObj := CreateDOMExceptionObject(DATA_CLONE_ERROR_NAME, AMessage);
  raise TGocciaThrowValue.Create(ErrorObj, ASuggestion);
end;

procedure ThrowInvalidCharacterError(const AMessage: string);
var
  ErrorObj: TGocciaObjectValue;
begin
  ErrorObj := CreateDOMExceptionObject(INVALID_CHARACTER_ERROR_NAME, AMessage);
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
