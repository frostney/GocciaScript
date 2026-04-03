unit Goccia.RegExp.Runtime;

{$I Goccia.inc}

interface

uses
  Goccia.RegExp.Engine,
  Goccia.Values.Primitives;

function GetRegExpPrototype: TGocciaValue;
procedure SetRegExpPrototype(const APrototype: TGocciaValue);

function IsRegExpValue(const AValue: TGocciaValue): Boolean;
function CreateRegExpObject(const APattern, AFlags: string): TGocciaValue;
function CloneRegExpObject(const AValue: TGocciaValue): TGocciaValue;
function MatchRegExpObjectOnce(const AValue: TGocciaValue; const AInput: string;
  out AMatchArray: TGocciaValue): Boolean;
function MatchRegExpObject(const AValue: TGocciaValue; const AInput: string;
  const AStartIndex: Integer; const ARequireStart, AUpdateLastIndex: Boolean;
  out AMatchArray: TGocciaValue; out AMatchIndex, AMatchEnd,
  ANextIndex: Integer): Boolean;
function RegExpObjectToString(const AValue: TGocciaValue): string;

implementation

uses
  Math,
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue;

var
  GRegExpPrototype: TGocciaObjectValue;

function GetRegExpPrototype: TGocciaValue;
begin
  Result := GRegExpPrototype;
end;

procedure SetRegExpPrototype(const APrototype: TGocciaValue);
begin
  GRegExpPrototype := TGocciaObjectValue(APrototype);
end;

function GetStringProperty(const AObject: TGocciaObjectValue;
  const AName: string): string;
begin
  Result := AObject.GetProperty(AName).ToStringLiteral.Value;
end;

function GetBooleanProperty(const AObject: TGocciaObjectValue;
  const AName: string): Boolean;
begin
  Result := AObject.GetProperty(AName).ToBooleanLiteral.Value;
end;

function GetIntegerProperty(const AObject: TGocciaObjectValue;
  const AName: string): Integer;
begin
  Result := Max(0, Trunc(AObject.GetProperty(AName).ToNumberLiteral.Value));
end;

function BuildMatchArray(const AInput: string;
  const AMatchResult: TGocciaRegExpMatchResult): TGocciaObjectValue;
var
  MatchArray: TGocciaArrayValue;
  I: Integer;
begin
  MatchArray := TGocciaArrayValue.Create;
  for I := 0 to High(AMatchResult.Groups) do
  begin
    if AMatchResult.Groups[I].Matched then
      MatchArray.Elements.Add(
        TGocciaStringLiteralValue.Create(AMatchResult.Groups[I].Value))
    else
      MatchArray.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;
  MatchArray.AssignProperty(PROP_INDEX,
    TGocciaNumberLiteralValue.Create(AMatchResult.MatchIndex));
  MatchArray.AssignProperty(PROP_INPUT, TGocciaStringLiteralValue.Create(AInput));
  Result := MatchArray;
end;

function IsRegExpValue(const AValue: TGocciaValue): Boolean;
var
  Tag: TGocciaValue;
begin
  if not (AValue is TGocciaObjectValue) then
    Exit(False);
  Tag := TGocciaObjectValue(AValue).GetSymbolProperty(
    TGocciaSymbolValue.WellKnownToStringTag);
  Result := (Tag is TGocciaStringLiteralValue) and
    (TGocciaStringLiteralValue(Tag).Value = 'RegExp');
end;

function CreateRegExpObject(const APattern, AFlags: string): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Source: string;
  CanonicalFlags: string;
begin
  try
    ValidateRegExpFlags(AFlags);
  except
    on E: Exception do
      ThrowSyntaxError(E.Message);
  end;

  Source := NormalizeRegExpSource(APattern);
  CanonicalFlags := CanonicalizeRegExpFlags(AFlags);
  Obj := TGocciaObjectValue.Create(GRegExpPrototype);
  Obj.DefineProperty(PROP_SOURCE,
    TGocciaPropertyDescriptorData.Create(
      TGocciaStringLiteralValue.Create(Source), []));
  Obj.DefineProperty(PROP_FLAGS,
    TGocciaPropertyDescriptorData.Create(
      TGocciaStringLiteralValue.Create(CanonicalFlags), []));
  Obj.DefineProperty(PROP_LAST_INDEX,
    TGocciaPropertyDescriptorData.Create(
      TGocciaNumberLiteralValue.Create(0), [pfWritable]));
  Obj.DefineProperty(PROP_GLOBAL,
    TGocciaPropertyDescriptorData.Create(
      TGocciaBooleanLiteralValue.Create(HasRegExpFlag(CanonicalFlags, 'g')), []));
  Obj.DefineProperty(PROP_IGNORE_CASE,
    TGocciaPropertyDescriptorData.Create(
      TGocciaBooleanLiteralValue.Create(HasRegExpFlag(CanonicalFlags, 'i')), []));
  Obj.DefineProperty(PROP_MULTILINE,
    TGocciaPropertyDescriptorData.Create(
      TGocciaBooleanLiteralValue.Create(HasRegExpFlag(CanonicalFlags, 'm')), []));
  Obj.DefineProperty(PROP_DOT_ALL,
    TGocciaPropertyDescriptorData.Create(
      TGocciaBooleanLiteralValue.Create(HasRegExpFlag(CanonicalFlags, 's')), []));
  Obj.DefineProperty(PROP_UNICODE,
    TGocciaPropertyDescriptorData.Create(
      TGocciaBooleanLiteralValue.Create(HasRegExpFlag(CanonicalFlags, 'u')), []));
  Obj.DefineProperty(PROP_STICKY,
    TGocciaPropertyDescriptorData.Create(
      TGocciaBooleanLiteralValue.Create(HasRegExpFlag(CanonicalFlags, 'y')), []));
  Result := Obj;
end;

function CloneRegExpObject(const AValue: TGocciaValue): TGocciaValue;
begin
  Result := CreateRegExpObject(
    GetStringProperty(TGocciaObjectValue(AValue), PROP_SOURCE),
    GetStringProperty(TGocciaObjectValue(AValue), PROP_FLAGS));
  Result.SetProperty(PROP_LAST_INDEX, TGocciaNumberLiteralValue.Create(
    GetIntegerProperty(TGocciaObjectValue(AValue), PROP_LAST_INDEX)));
end;

function MatchRegExpObjectOnce(const AValue: TGocciaValue; const AInput: string;
  out AMatchArray: TGocciaValue): Boolean;
var
  Obj: TGocciaObjectValue;
  StartIndex, MatchIndex, MatchEnd, NextIndex: Integer;
begin
  Obj := TGocciaObjectValue(AValue);
  if GetBooleanProperty(Obj, PROP_GLOBAL) or GetBooleanProperty(Obj, PROP_STICKY) then
    StartIndex := GetIntegerProperty(Obj, PROP_LAST_INDEX)
  else
    StartIndex := 0;
  Result := MatchRegExpObject(AValue, AInput, StartIndex,
    GetBooleanProperty(Obj, PROP_STICKY), True, AMatchArray, MatchIndex,
    MatchEnd, NextIndex);
end;

function MatchRegExpObject(const AValue: TGocciaValue; const AInput: string;
  const AStartIndex: Integer; const ARequireStart, AUpdateLastIndex: Boolean;
  out AMatchArray: TGocciaValue; out AMatchIndex, AMatchEnd,
  ANextIndex: Integer): Boolean;
var
  Obj: TGocciaObjectValue;
  MatchResult: TGocciaRegExpMatchResult;
  ShouldUpdate: Boolean;
begin
  Obj := TGocciaObjectValue(AValue);
  Result := ExecuteRegExp(
    GetStringProperty(Obj, PROP_SOURCE),
    GetStringProperty(Obj, PROP_FLAGS),
    AInput,
    AStartIndex,
    ARequireStart,
    MatchResult);

  ShouldUpdate := AUpdateLastIndex and
    (GetBooleanProperty(Obj, PROP_GLOBAL) or GetBooleanProperty(Obj, PROP_STICKY));

  if not Result then
  begin
    if ShouldUpdate then
      Obj.SetProperty(PROP_LAST_INDEX, TGocciaNumberLiteralValue.Create(0));
    AMatchArray := nil;
    AMatchIndex := -1;
    AMatchEnd := -1;
    ANextIndex := -1;
    Exit(False);
  end;

  AMatchIndex := MatchResult.MatchIndex;
  AMatchEnd := MatchResult.MatchEnd;
  ANextIndex := MatchResult.NextIndex;
  AMatchArray := BuildMatchArray(AInput, MatchResult);
  if ShouldUpdate then
    Obj.SetProperty(PROP_LAST_INDEX, TGocciaNumberLiteralValue.Create(ANextIndex));
end;

function RegExpObjectToString(const AValue: TGocciaValue): string;
begin
  Result := Goccia.RegExp.Engine.RegExpToString(
    GetStringProperty(TGocciaObjectValue(AValue), PROP_SOURCE),
    GetStringProperty(TGocciaObjectValue(AValue), PROP_FLAGS));
end;

end.
