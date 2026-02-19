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

{ Raises a TGocciaThrowValue with a generic Error }
procedure ThrowError(const AMessage: string);

implementation

uses
  Goccia.Values.Error,
  Goccia.Values.Primitives;

function CreateErrorObject(const AName, AMessage: string): TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create;
  Result.AssignProperty('name', TGocciaStringLiteralValue.Create(AName));
  Result.AssignProperty('message', TGocciaStringLiteralValue.Create(AMessage));
end;

procedure ThrowTypeError(const AMessage: string);
begin
  raise TGocciaThrowValue.Create(CreateErrorObject('TypeError', AMessage));
end;

procedure ThrowRangeError(const AMessage: string);
begin
  raise TGocciaThrowValue.Create(CreateErrorObject('RangeError', AMessage));
end;

procedure ThrowReferenceError(const AMessage: string);
begin
  raise TGocciaThrowValue.Create(CreateErrorObject('ReferenceError', AMessage));
end;

procedure ThrowError(const AMessage: string);
begin
  raise TGocciaThrowValue.Create(CreateErrorObject('Error', AMessage));
end;

end.
