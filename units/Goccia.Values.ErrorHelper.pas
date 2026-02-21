unit Goccia.Values.ErrorHelper;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue;

{ Creates a JavaScript error object with the given name and message }
function CreateErrorObject(const AName, AMessage: string): TGocciaObjectValue;

{ Raises a TGocciaThrowValue with a TypeError }
procedure ThrowTypeError(const AMessage: string);

{ Raises a TGocciaThrowValue with a RangeError }
procedure ThrowRangeError(const AMessage: string);

{ Raises a TGocciaThrowValue with a ReferenceError }
procedure ThrowReferenceError(const AMessage: string);

{ Raises a TGocciaThrowValue with a SyntaxError }
procedure ThrowSyntaxError(const AMessage: string);

{ Raises a TGocciaThrowValue with a generic Error }
procedure ThrowError(const AMessage: string);

implementation

uses
  Goccia.Values.Error,
  Goccia.Values.ErrorNames,
  Goccia.Values.Primitives,
  Goccia.Values.PropertyNames;

function CreateErrorObject(const AName, AMessage: string): TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create;
  Result.AssignProperty(PROP_NAME, TGocciaStringLiteralValue.Create(AName));
  Result.AssignProperty(PROP_MESSAGE, TGocciaStringLiteralValue.Create(AMessage));
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

procedure ThrowError(const AMessage: string);
begin
  raise TGocciaThrowValue.Create(CreateErrorObject(ERROR_NAME, AMessage));
end;

end.
