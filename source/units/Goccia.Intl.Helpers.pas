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

function LocaleWithoutUnicodeExtension(const ALocale: string): string;
function TryGetUnicodeLocaleExtensionKeyword(const ALocale, AKey: string;
  out AValue: string): Boolean;
function IsSupportedNumberingSystem(const AValue: string): Boolean;

implementation

uses
  StrUtils,
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
    if AParts[I].UnitIdentifier <> '' then
      PartObj.AssignProperty(PROP_UNIT, TGocciaStringLiteralValue.Create(AParts[I].UnitIdentifier));
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

function LocaleWithoutUnicodeExtension(const ALocale: string): string;
var
  ExtensionStart: Integer;
begin
  ExtensionStart := Pos('-u-', ALocale);
  if ExtensionStart = 0 then
    Result := ALocale
  else
    Result := Copy(ALocale, 1, ExtensionStart - 1);
end;

function TryGetUnicodeLocaleExtensionKeyword(const ALocale, AKey: string;
  out AValue: string): Boolean;
var
  ExtensionStart, Index, NextDash: Integer;
  Tail, Subtag: string;
begin
  Result := False;
  AValue := '';
  ExtensionStart := Pos('-u-', ALocale);
  if ExtensionStart = 0 then
    Exit;

  Tail := Copy(ALocale, ExtensionStart + 3, MaxInt);
  Index := 1;
  while Index <= Length(Tail) do
  begin
    NextDash := PosEx('-', Tail, Index);
    if NextDash = 0 then
      NextDash := Length(Tail) + 1;
    Subtag := Copy(Tail, Index, NextDash - Index);
    Index := NextDash + 1;

    if SameText(Subtag, AKey) then
    begin
      NextDash := PosEx('-', Tail, Index);
      if NextDash = 0 then
        NextDash := Length(Tail) + 1;
      AValue := Copy(Tail, Index, NextDash - Index);
      Result := AValue <> '';
      Exit;
    end;
  end;
end;

function IsSupportedNumberingSystem(const AValue: string): Boolean;
const
  SupportedNumberingSystems: array[0..62] of string = (
    'adlm', 'ahom', 'arab', 'arabext', 'bali', 'beng', 'bhks', 'brah',
    'cakm', 'cham', 'deva', 'fullwide', 'gong', 'gonm', 'gujr', 'guru',
    'hanidec', 'hmng', 'java', 'kali', 'khmr', 'knda', 'lana',
    'lanatham', 'laoo', 'latn', 'lepc', 'limb', 'mathbold', 'mathdbl',
    'mathmono', 'mathsanb', 'mathsans', 'mlym', 'modi', 'mong', 'mroo',
    'mtei', 'mymr', 'mymrshan', 'mymrtlng', 'newa', 'nkoo', 'olck',
    'orya', 'osma', 'rohg', 'saur', 'shrd', 'sind', 'sinh', 'sora',
    'sund', 'takr', 'talu', 'tamldec', 'telu', 'thai', 'tibt', 'tirh',
    'vaii', 'wara', 'wcho');
var
  I: Integer;
begin
  Result := False;
  for I := Low(SupportedNumberingSystems) to High(SupportedNumberingSystems) do
  begin
    if AValue = SupportedNumberingSystems[I] then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

end.
