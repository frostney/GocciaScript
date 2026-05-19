unit Goccia.Intl.Helpers;

{$I Goccia.inc}

interface

uses
  IntlTypes,

  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

// Converts an ICU format-part array into a JS Array of
// part objects with 'type' and 'value' properties.
function FormatPartsToArray(const AParts: TIntlFormatPartArray): TGocciaArrayValue;

// Reads a string-valued property from AOptions.  Returns True and sets AValue
// when the property exists and is not undefined; returns False otherwise.
function TryReadStringOption(const AOptions: TGocciaObjectValue;
  const AName: string; var AValue: string): Boolean;

// Like TryReadStringOption but additionally rejects NUL characters via
// ThrowRangeError (SErrorIntlInvalidOption).
procedure ReadValidatedStringOption(const AOptions: TGocciaObjectValue;
  const AName: string; var AValue: string);

implementation

uses
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Values.ErrorHelper;

function FormatPartsToArray(const AParts: TIntlFormatPartArray): TGocciaArrayValue;
var
  I: Integer;
  PartObj: TGocciaObjectValue;
begin
  Result := TGocciaArrayValue.Create;
  for I := 0 to Length(AParts) - 1 do
  begin
    PartObj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
    PartObj.AssignProperty(PROP_TYPE, TGocciaStringLiteralValue.Create(AParts[I].PartType));
    PartObj.AssignProperty(PROP_VALUE, TGocciaStringLiteralValue.Create(AParts[I].Value));
    if AParts[I].Source <> '' then
      PartObj.AssignProperty(PROP_SOURCE, TGocciaStringLiteralValue.Create(AParts[I].Source));
    Result.Elements.Add(PartObj);
  end;
end;

function TryReadStringOption(const AOptions: TGocciaObjectValue;
  const AName: string; var AValue: string): Boolean;
var
  V: TGocciaValue;
begin
  V := AOptions.GetProperty(AName);
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
  begin
    AValue := V.ToStringLiteral.Value;
    Result := True;
  end
  else
    Result := False;
end;

procedure ReadValidatedStringOption(const AOptions: TGocciaObjectValue;
  const AName: string; var AValue: string);
var
  S: string;
begin
  if TryReadStringOption(AOptions, AName, S) then
  begin
    if ContainsNulCharacter(S) then
      ThrowRangeError(Format(SErrorIntlInvalidOption, [S, AName]));
    AValue := S;
  end;
end;

end.
