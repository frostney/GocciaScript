unit Goccia.Intl.Helpers;

{$I Goccia.inc}

interface

uses
  IntlTypes,

  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue;

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
function RemoveUnicodeLocaleExtensionKeyword(const ALocale, AKey: string): string;
function AddUnicodeLocaleExtensionKeyword(const ALocale, AKey, AValue: string): string;
function IsSupportedNumberingSystem(const AValue: string): Boolean;
function IntlFallbackSymbol: TGocciaSymbolValue;

implementation

uses
  StrUtils,
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Realm,
  Goccia.Values.ErrorHelper;

const
  INTL_LEGACY_CONSTRUCTED_SYMBOL_DESCRIPTION = 'IntlLegacyConstructedSymbol';

var
  GIntlFallbackSymbolSlot: TGocciaRealmSlotId;

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

function FindSingletonExtensionStart(const ALocale, ASingleton: string; out AStart: Integer): Boolean;
var
  Index, NextDash, SubtagStart: Integer;
  Subtag, SingletonLower: string;
begin
  Result := False;
  AStart := 0;
  SingletonLower := LowerCase(ASingleton);
  Index := 1;
  while Index <= Length(ALocale) do
  begin
    SubtagStart := Index;
    NextDash := Pos('-', Copy(ALocale, Index, MaxInt));
    if NextDash = 0 then
      NextDash := Length(ALocale) + 1
    else
      NextDash := Index + NextDash - 1;
    Subtag := LowerCase(Copy(ALocale, Index, NextDash - Index));

    if Length(Subtag) = 1 then
    begin
      if Subtag = SingletonLower then
      begin
        AStart := SubtagStart - 1;
        Result := True;
        Exit;
      end;
      if Subtag = 'x' then
        Exit;
    end;

    Index := NextDash + 1;
  end;
end;

function FindUnicodeExtensionRangeFrom(const ALocale: string; ASearchStart: Integer;
  out AStart, AEnd: Integer): Boolean;
var
  Index, NextDash, SubtagStart: Integer;
  Subtag: string;
begin
  Result := False;
  AStart := 0;
  AEnd := 0;
  Index := ASearchStart;
  if Index < 1 then
    Index := 1;
  while Index <= Length(ALocale) do
  begin
    SubtagStart := Index;
    NextDash := Pos('-', Copy(ALocale, Index, MaxInt));
    if NextDash = 0 then
      NextDash := Length(ALocale) + 1
    else
      NextDash := Index + NextDash - 1;
    Subtag := LowerCase(Copy(ALocale, Index, NextDash - Index));

    if Length(Subtag) = 1 then
    begin
      if Subtag = 'x' then
        Exit;
      if Subtag = 'u' then
      begin
        AStart := SubtagStart - 1;
        AEnd := Length(ALocale) + 1;
        Index := NextDash + 1;
        while Index <= Length(ALocale) do
        begin
          SubtagStart := Index;
          NextDash := Pos('-', Copy(ALocale, Index, MaxInt));
          if NextDash = 0 then
            NextDash := Length(ALocale) + 1
          else
            NextDash := Index + NextDash - 1;
          Subtag := LowerCase(Copy(ALocale, Index, NextDash - Index));
          if Length(Subtag) = 1 then
          begin
            AEnd := SubtagStart - 1;
            Break;
          end;
          Index := NextDash + 1;
        end;
        Result := True;
        Exit;
      end;
    end;

    Index := NextDash + 1;
  end;
end;

function FindUnicodeExtensionRange(const ALocale: string; out AStart, AEnd: Integer): Boolean;
begin
  Result := FindUnicodeExtensionRangeFrom(ALocale, 1, AStart, AEnd);
end;

function LocaleWithoutUnicodeExtension(const ALocale: string): string;
var
  ExtensionStart, ExtensionEnd: Integer;
begin
  if FindUnicodeExtensionRange(ALocale, ExtensionStart, ExtensionEnd) then
    Result := Copy(ALocale, 1, ExtensionStart - 1) + Copy(ALocale, ExtensionEnd, MaxInt)
  else
    Result := ALocale;
end;

function TryGetUnicodeLocaleExtensionKeyword(const ALocale, AKey: string;
  out AValue: string): Boolean;
var
  SearchStart, ExtensionStart, ExtensionEnd, Index, NextDash: Integer;
  Tail, Subtag, KeyLower: string;
begin
  Result := False;
  AValue := '';
  KeyLower := LowerCase(AKey);
  SearchStart := 1;
  while FindUnicodeExtensionRangeFrom(ALocale, SearchStart, ExtensionStart, ExtensionEnd) do
  begin
    Tail := Copy(ALocale, ExtensionStart + 3, ExtensionEnd - ExtensionStart - 3);
    Index := 1;
    while Index <= Length(Tail) do
    begin
      NextDash := Pos('-', Copy(Tail, Index, MaxInt));
      if NextDash = 0 then
        NextDash := Length(Tail) + 1
      else
        NextDash := Index + NextDash - 1;
      Subtag := LowerCase(Copy(Tail, Index, NextDash - Index));
      Index := NextDash + 1;

      if Length(Subtag) <> 2 then
        Continue;

      if Subtag = KeyLower then
      begin
        Result := True;
        while Index <= Length(Tail) do
        begin
          NextDash := Pos('-', Copy(Tail, Index, MaxInt));
          if NextDash = 0 then
            NextDash := Length(Tail) + 1
          else
            NextDash := Index + NextDash - 1;
          Subtag := LowerCase(Copy(Tail, Index, NextDash - Index));
          if Length(Subtag) = 2 then
            Exit;
          if AValue = '' then
            AValue := Subtag
          else
            AValue := AValue + '-' + Subtag;
          Index := NextDash + 1;
        end;
        Exit;
      end;
    end;

    SearchStart := ExtensionEnd + 1;
  end;
end;

function RemoveUnicodeLocaleExtensionKeyword(const ALocale, AKey: string): string;
var
  ExtensionStart, ExtensionEnd, Index, NextDash: Integer;
  Base, Tail, Suffix, Subtag, KeyLower, NewTail: string;
  Removing: Boolean;
begin
  KeyLower := LowerCase(AKey);
  if not FindUnicodeExtensionRange(ALocale, ExtensionStart, ExtensionEnd) then
  begin
    Result := ALocale;
    Exit;
  end;

  Base := Copy(ALocale, 1, ExtensionStart - 1);
  Tail := Copy(ALocale, ExtensionStart + 3, ExtensionEnd - ExtensionStart - 3);
  Suffix := Copy(ALocale, ExtensionEnd, MaxInt);
  NewTail := '';
  Removing := False;
  Index := 1;
  while Index <= Length(Tail) do
  begin
    NextDash := Pos('-', Copy(Tail, Index, MaxInt));
    if NextDash = 0 then
      NextDash := Length(Tail) + 1
    else
      NextDash := Index + NextDash - 1;
    Subtag := Copy(Tail, Index, NextDash - Index);
    Index := NextDash + 1;

    if Length(Subtag) = 2 then
      Removing := LowerCase(Subtag) = KeyLower;
    if Removing then
      Continue;
    if NewTail <> '' then
      NewTail := NewTail + '-';
    NewTail := NewTail + Subtag;
  end;

  if NewTail = '' then
    Result := Base + Suffix
  else
    Result := Base + '-u-' + NewTail + Suffix;
end;

function AddUnicodeLocaleExtensionKeyword(const ALocale, AKey, AValue: string): string;
var
  ExtensionStart, ExtensionEnd, PrivateUseStart: Integer;
  Addition: string;
begin
  Addition := AKey;
  if AValue <> '' then
    Addition := Addition + '-' + AValue;

  if FindUnicodeExtensionRange(ALocale, ExtensionStart, ExtensionEnd) then
    Result := Copy(ALocale, 1, ExtensionEnd - 1) + '-' + Addition +
      Copy(ALocale, ExtensionEnd, MaxInt)
  else if FindSingletonExtensionStart(ALocale, 'x', PrivateUseStart) then
    Result := Copy(ALocale, 1, PrivateUseStart - 1) + '-u-' + Addition +
      Copy(ALocale, PrivateUseStart, MaxInt)
  else
    Result := ALocale + '-u-' + Addition;
end;

function IsSupportedNumberingSystem(const AValue: string): Boolean;
const
  SupportedNumberingSystems: array[0..77] of string = (
    'adlm', 'ahom', 'arab', 'arabext', 'bali', 'beng', 'bhks', 'brah',
    'cakm', 'cham', 'deva', 'diak', 'fullwide', 'gara', 'gong', 'gonm',
    'gujr', 'gukh', 'guru', 'hanidec', 'hmng', 'hmnp', 'java', 'kali',
    'kawi', 'khmr', 'knda', 'krai', 'lana', 'lanatham', 'laoo', 'latn',
    'lepc', 'limb', 'mathbold', 'mathdbl', 'mathmono', 'mathsanb',
    'mathsans', 'mlym', 'modi', 'mong', 'mroo', 'mtei', 'mymr',
    'mymrepka', 'mymrpao', 'mymrshan', 'mymrtlng', 'nagm', 'newa', 'nkoo',
    'olck', 'onao', 'orya', 'osma', 'outlined', 'rohg', 'saur', 'segment',
    'shrd', 'sind', 'sinh', 'sora', 'sund', 'sunu', 'takr', 'talu',
    'tamldec', 'telu', 'thai', 'tibt', 'tirh', 'tnsa', 'tols', 'vaii',
    'wara', 'wcho');
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

function IntlFallbackSymbol: TGocciaSymbolValue;
begin
  if (CurrentRealm <> nil) then
  begin
    Result := TGocciaSymbolValue(CurrentRealm.GetSlot(GIntlFallbackSymbolSlot));
    if Assigned(Result) then
      Exit;

    Result := TGocciaSymbolValue.Create(INTL_LEGACY_CONSTRUCTED_SYMBOL_DESCRIPTION);
    CurrentRealm.SetSlot(GIntlFallbackSymbolSlot, Result);
    Exit;
  end;

  Result := TGocciaSymbolValue.Create(INTL_LEGACY_CONSTRUCTED_SYMBOL_DESCRIPTION);
end;

initialization
  GIntlFallbackSymbolSlot := RegisterRealmSlot('Intl.[[FallbackSymbol]]');

end.
