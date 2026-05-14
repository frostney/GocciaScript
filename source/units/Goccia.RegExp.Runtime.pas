unit Goccia.RegExp.Runtime;

{$I Goccia.inc}

interface

uses
  Goccia.RegExp.Engine,
  Goccia.Values.Primitives;

function GetRegExpPrototype: TGocciaValue;
procedure SetRegExpPrototype(const APrototype: TGocciaValue);

function IsRegExpInstance(const AValue: TGocciaValue): Boolean;
function IsRegExp(const AValue: TGocciaValue): Boolean;
function CreateRegExpObject(const APattern, AFlags: string): TGocciaValue;
function CloneRegExpObject(const AValue: TGocciaValue): TGocciaValue;
function MatchRegExpObjectOnce(const AValue: TGocciaValue; const AInput: string;
  out AMatchArray: TGocciaValue): Boolean;
function MatchRegExpObject(const AValue: TGocciaValue; const AInput: string;
  const AStartIndex: Integer; const ARequireStart, AUpdateLastIndex: Boolean;
  out AMatchArray: TGocciaValue; out AMatchIndex, AMatchEnd,
  ANextIndex: Integer): Boolean;
function HasUnicodeRegExpFlag(const AFlags: string): Boolean;
function GetClampedLastIndex(const AValue: TGocciaValue;
  const AInputLength: Integer): Integer;
function AdvanceProtocolLastIndexAfterEmptyMatch(
  const AValue: TGocciaValue; const AInput: string;
  const AUnicode: Boolean): Integer;
function RegExpObjectToString(const AValue: TGocciaValue): string;

implementation

uses
  Math,
  SysUtils,

  TextSemantics,

  Goccia.Arguments.Collection,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.RegExp.VM,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue;

threadvar
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

function HasUnicodeRegExpFlag(const AFlags: string): Boolean;
begin
  Result := HasRegExpFlag(AFlags, 'u') or HasRegExpFlag(AFlags, 'v');
end;

function GetClampedLastIndex(const AValue: TGocciaValue;
  const AInputLength: Integer): Integer;
var
  LastIndex: TGocciaNumberLiteralValue;
begin
  LastIndex := TGocciaObjectValue(AValue).GetProperty(PROP_LAST_INDEX)
    .ToNumberLiteral;
  if LastIndex.IsNaN or LastIndex.IsNegativeInfinity or (LastIndex.Value <= 0) then
    Exit(0);
  if LastIndex.IsInfinity or (LastIndex.Value >= AInputLength) then
    Exit(AInputLength);
  Result := Trunc(LastIndex.Value);
end;

function AdvanceProtocolLastIndexAfterEmptyMatch(
  const AValue: TGocciaValue; const AInput: string;
  const AUnicode: Boolean): Integer;
begin
  Result := AdvanceUTF8StringIndex(AInput,
    GetClampedLastIndex(AValue, Length(AInput)), AUnicode);
  TGocciaObjectValue(AValue).SetProperty(PROP_LAST_INDEX,
    TGocciaNumberLiteralValue.Create(Result));
end;

// ES2026 §22.2.7.3 BuildMatchArray
function BuildMatchArray(const AInput: string;
  const AMatchResult: TGocciaRegExpMatchResult): TGocciaObjectValue;
var
  MatchArray: TGocciaArrayValue;
  GroupsObject: TGocciaObjectValue;
  GroupIndex: Integer;
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
  MatchArray.AssignProperty(PROP_INPUT,
    TGocciaStringLiteralValue.Create(AInput));
  if Length(AMatchResult.NamedGroups) > 0 then
  begin
    GroupsObject := TGocciaObjectValue.Create(nil);
    // ES2025: Two-pass approach for duplicate named capture groups.
    // Pass 1: Initialize all unique names to undefined.
    for I := 0 to High(AMatchResult.NamedGroups) do
      GroupsObject.AssignProperty(AMatchResult.NamedGroups[I].Name,
        TGocciaUndefinedLiteralValue.UndefinedValue);
    // Pass 2: Overwrite with matched values. For duplicate names, only the
    // participating group's value is set — non-participating duplicates are
    // skipped, preserving the correct value regardless of iteration order.
    for I := 0 to High(AMatchResult.NamedGroups) do
    begin
      GroupIndex := AMatchResult.NamedGroups[I].Index;
      if (GroupIndex <= High(AMatchResult.Groups)) and
         AMatchResult.Groups[GroupIndex].Matched then
        GroupsObject.AssignProperty(AMatchResult.NamedGroups[I].Name,
          TGocciaStringLiteralValue.Create(
            AMatchResult.Groups[GroupIndex].Value));
    end;
    MatchArray.AssignProperty(PROP_GROUPS, GroupsObject);
  end
  else
    MatchArray.AssignProperty(PROP_GROUPS,
      TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := MatchArray;
end;

function IsRegExpInstance(const AValue: TGocciaValue): Boolean;
begin
  if not (AValue is TGocciaObjectValue) then
    Exit(False);
  Result := TGocciaObjectValue(AValue).HasRegExpData;
end;

function IsRegExp(const AValue: TGocciaValue): Boolean;
var
  Matcher: TGocciaValue;
begin
  if not (AValue is TGocciaObjectValue) then
    Exit(False);
  Matcher := TGocciaObjectValue(AValue).GetSymbolProperty(
    TGocciaSymbolValue.WellKnownMatch);
  if not (Matcher is TGocciaUndefinedLiteralValue) then
    Exit(Matcher.ToBooleanLiteral.Value);
  Result := IsRegExpInstance(AValue);
end;

function CreateRegExpObject(const APattern, AFlags: string): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Source: string;
  CanonicalFlags: string;
begin
  try
    ValidateRegExpPattern(APattern, AFlags);
  except
    on E: Exception do
      ThrowSyntaxError(E.Message);
  end;

  Source := NormalizeRegExpSource(APattern);
  CanonicalFlags := CanonicalizeRegExpFlags(AFlags);
  Obj := TGocciaObjectValue.Create(GRegExpPrototype);
  Obj.HasRegExpData := True;
  Obj.DefineProperty(PROP_SOURCE,
    TGocciaPropertyDescriptorData.Create(
      TGocciaStringLiteralValue.Create(Source), []));
  Obj.DefineProperty(PROP_FLAGS,
    TGocciaPropertyDescriptorData.Create(
      TGocciaStringLiteralValue.Create(CanonicalFlags), []));
  Obj.DefineProperty(PROP_LAST_INDEX,
    TGocciaPropertyDescriptorData.Create(
      TGocciaNumberLiteralValue.Create(0), [pfWritable]));
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
const
  MATCH_TEXT_PROPERTY = '0';
var
  Obj: TGocciaObjectValue;
  ExecMethod: TGocciaValue;
  ExecArgs: TGocciaArgumentsCollection;
  ExecResult: TGocciaValue;
  MatchText: string;
  MatchResult: TGocciaRegExpMatchResult;
  ShouldUpdate: Boolean;
begin
  Obj := TGocciaObjectValue(AValue);
  ExecMethod := Obj.GetProperty(PROP_EXEC);
  if (not IsRegExpInstance(AValue)) and Assigned(ExecMethod) and
     (not (ExecMethod is TGocciaUndefinedLiteralValue)) and ExecMethod.IsCallable then
  begin
    ExecArgs := TGocciaArgumentsCollection.Create([
      TGocciaStringLiteralValue.Create(AInput)
    ]);
    try
      ExecResult := InvokeCallable(ExecMethod, ExecArgs, Obj);
    finally
      ExecArgs.Free;
    end;

    if ExecResult is TGocciaNullLiteralValue then
    begin
      AMatchArray := nil;
      AMatchIndex := -1;
      AMatchEnd := -1;
      ANextIndex := -1;
      Exit(False);
    end;

    if not (ExecResult is TGocciaObjectValue) then
      ThrowTypeError(SErrorRegExpExecReturnType);

    AMatchArray := ExecResult;
    AMatchIndex := Trunc(TGocciaObjectValue(ExecResult).GetProperty(PROP_INDEX)
      .ToNumberLiteral.Value);
    MatchText := TGocciaObjectValue(ExecResult).GetProperty(MATCH_TEXT_PROPERTY)
      .ToStringLiteral.Value;
    AMatchEnd := AMatchIndex + Length(MatchText);
    ANextIndex := AMatchEnd;
    Exit(True);
  end;

  if not IsRegExpInstance(AValue) then
    ThrowTypeError(SErrorRegExpExecNonRegExp);

  try
    Result := ExecuteRegExp(
      GetStringProperty(Obj, PROP_SOURCE),
      GetStringProperty(Obj, PROP_FLAGS),
      AInput,
      AStartIndex,
      ARequireStart,
      MatchResult);
  except
    on E: ERegExpRuntimeError do
      ThrowError(E.Message);
  end;

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
