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
function GetRegExpLastIndexLength(const AValue: TGocciaValue): Double;
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
  Goccia.Constants.NumericLimits,
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

function ToLengthIndex(const AValue: TGocciaValue): Double;
var
  Index: TGocciaNumberLiteralValue;
begin
  if not Assigned(AValue) then
    Exit(0);

  Index := AValue.ToNumberLiteral;
  if Index.IsNaN or Index.IsNegativeInfinity or (Index.Value <= 0) then
    Exit(0);
  if Index.IsInfinity or (Index.Value > MAX_SAFE_INTEGER_F) then
    Exit(MAX_SAFE_INTEGER_F);
  Result := Trunc(Index.Value);
end;

function GetClampedIndexValue(const AValue: TGocciaValue;
  const AInputLength: Integer): Integer;
var
  Index: TGocciaNumberLiteralValue;
begin
  Index := AValue.ToNumberLiteral;
  if Index.IsNaN or Index.IsNegativeInfinity or (Index.Value <= 0) then
    Exit(0);
  if Index.IsInfinity or (Index.Value >= AInputLength) then
    Exit(AInputLength);
  Result := Trunc(Index.Value);
end;

function HasUnicodeRegExpFlag(const AFlags: string): Boolean;
begin
  Result := HasRegExpFlag(AFlags, 'u') or HasRegExpFlag(AFlags, 'v');
end;

function GetRegExpLastIndexLength(const AValue: TGocciaValue): Double;
begin
  Result := ToLengthIndex(TGocciaObjectValue(AValue).GetProperty(PROP_LAST_INDEX));
end;

function GetClampedLastIndex(const AValue: TGocciaValue;
  const AInputLength: Integer): Integer;
var
  Index: Double;
begin
  Index := GetRegExpLastIndexLength(AValue);
  if Index >= AInputLength then
    Result := AInputLength
  else
    Result := Trunc(Index);
end;

function AdvanceProtocolLastIndexAfterEmptyMatch(
  const AValue: TGocciaValue; const AInput: string;
  const AUnicode: Boolean): Integer;
var
  InputLength: Integer;
  ThisIndex, NextIndex: Double;
begin
  InputLength := UTF16CodeUnitLength(AInput);
  ThisIndex := GetRegExpLastIndexLength(AValue);
  if ThisIndex > InputLength then
    NextIndex := ThisIndex + 1
  else
    NextIndex := AdvanceUTF16StringIndex(AInput, Trunc(ThisIndex),
      AUnicode);
  TGocciaObjectValue(AValue).SetProperty(PROP_LAST_INDEX,
    TGocciaNumberLiteralValue.Create(NextIndex));

  if NextIndex > InputLength then
  begin
    if InputLength < MaxInt then
      Result := InputLength + 1
    else
      Result := MaxInt;
  end
  else
    Result := Trunc(NextIndex);
end;

// ES2026 §22.2.7.3 BuildMatchArray
function BuildMatchArray(const AInput: string;
  const AMatchResult: TGocciaRegExpMatchResult): TGocciaObjectValue;
var
  IndicesArray: TGocciaArrayValue;
  IndicesGroupsObject: TGocciaObjectValue;
  MatchArray: TGocciaArrayValue;
  GroupsObject: TGocciaObjectValue;
  GroupIndex: Integer;
  I: Integer;

  procedure CreateDataProperty(const AObject: TGocciaObjectValue;
    const AName: string; const AValue: TGocciaValue);
  begin
    AObject.DefineProperty(AName, TGocciaPropertyDescriptorData.Create(
      AValue, [pfEnumerable, pfConfigurable, pfWritable]));
  end;

  function CreateIndexPair(const AStartIndex, AEndIndex: Integer):
    TGocciaArrayValue;
  begin
    Result := TGocciaArrayValue.Create;
    Result.Elements.Add(TGocciaNumberLiteralValue.Create(AStartIndex));
    Result.Elements.Add(TGocciaNumberLiteralValue.Create(AEndIndex));
  end;
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
  CreateDataProperty(MatchArray, PROP_INDEX,
    TGocciaNumberLiteralValue.Create(AMatchResult.MatchIndex));
  CreateDataProperty(MatchArray, PROP_INPUT, TGocciaStringLiteralValue.Create(AInput));
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
    CreateDataProperty(MatchArray, PROP_GROUPS, GroupsObject);
  end
  else
    CreateDataProperty(MatchArray, PROP_GROUPS,
      TGocciaUndefinedLiteralValue.UndefinedValue);

  if AMatchResult.HasIndices then
  begin
    IndicesArray := TGocciaArrayValue.Create;
    for I := 0 to High(AMatchResult.Groups) do
    begin
      if AMatchResult.Groups[I].Matched then
        IndicesArray.Elements.Add(CreateIndexPair(
          AMatchResult.Groups[I].StartIndex,
          AMatchResult.Groups[I].EndIndex))
      else
        IndicesArray.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue);
    end;

    if Length(AMatchResult.NamedGroups) > 0 then
    begin
      IndicesGroupsObject := TGocciaObjectValue.Create(nil);
      for I := 0 to High(AMatchResult.NamedGroups) do
        CreateDataProperty(IndicesGroupsObject, AMatchResult.NamedGroups[I].Name,
          TGocciaUndefinedLiteralValue.UndefinedValue);
      for I := 0 to High(AMatchResult.NamedGroups) do
      begin
        GroupIndex := AMatchResult.NamedGroups[I].Index;
        if (GroupIndex <= High(AMatchResult.Groups)) and
           AMatchResult.Groups[GroupIndex].Matched then
          CreateDataProperty(IndicesGroupsObject,
            AMatchResult.NamedGroups[I].Name,
            CreateIndexPair(AMatchResult.Groups[GroupIndex].StartIndex,
              AMatchResult.Groups[GroupIndex].EndIndex));
      end;
      CreateDataProperty(IndicesArray, PROP_GROUPS, IndicesGroupsObject);
    end
    else
      CreateDataProperty(IndicesArray, PROP_GROUPS,
        TGocciaUndefinedLiteralValue.UndefinedValue);

    CreateDataProperty(MatchArray, PROP_INDICES, IndicesArray);
  end;

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
    GetRegExpLastIndexLength(AValue)));
end;

function MatchRegExpObjectOnce(const AValue: TGocciaValue; const AInput: string;
  out AMatchArray: TGocciaValue): Boolean;
var
  Obj: TGocciaObjectValue;
  StartIndex, MatchIndex, MatchEnd, NextIndex: Integer;
  InputLength: Integer;
  LastIndex: Double;
begin
  Obj := TGocciaObjectValue(AValue);
  if not IsRegExpInstance(AValue) then
  begin
    Result := MatchRegExpObject(AValue, AInput, 0, False, False, AMatchArray,
      MatchIndex, MatchEnd, NextIndex);
    Exit;
  end;

  if GetBooleanProperty(Obj, PROP_GLOBAL) or GetBooleanProperty(Obj, PROP_STICKY) then
  begin
    InputLength := UTF16CodeUnitLength(AInput);
    LastIndex := GetRegExpLastIndexLength(AValue);
    if LastIndex > InputLength then
    begin
      if InputLength < MaxInt then
        StartIndex := InputLength + 1
      else
        StartIndex := MaxInt;
    end
    else
      StartIndex := Trunc(LastIndex);
  end
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
     (not (ExecMethod is TGocciaUndefinedLiteralValue)) then
  begin
    if not ExecMethod.IsCallable then
      ThrowTypeError(SErrorRegExpExecNotCallable);

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
    AMatchIndex := GetClampedIndexValue(
      TGocciaObjectValue(ExecResult).GetProperty(PROP_INDEX),
      UTF16CodeUnitLength(AInput));
    MatchText := TGocciaObjectValue(ExecResult).GetProperty(MATCH_TEXT_PROPERTY)
      .ToStringLiteral.Value;
    AMatchEnd := AMatchIndex + UTF16CodeUnitLength(MatchText);
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
    Obj.SetProperty(PROP_LAST_INDEX, TGocciaNumberLiteralValue.Create(AMatchEnd));
end;

function RegExpObjectToString(const AValue: TGocciaValue): string;
begin
  Result := Goccia.RegExp.Engine.RegExpToString(
    GetStringProperty(TGocciaObjectValue(AValue), PROP_SOURCE),
    GetStringProperty(TGocciaObjectValue(AValue), PROP_FLAGS));
end;

end.
