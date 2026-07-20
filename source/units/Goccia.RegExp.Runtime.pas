unit Goccia.RegExp.Runtime;

{$I Goccia.inc}

interface

uses
  Goccia.RegExp.Engine,
  Goccia.Values.Primitives;

type
  TGocciaRegExpExecutionResultKind = (rerkNative, rerkCustom);

  TGocciaRegExpExecutionResult = record
  private
    FKind: TGocciaRegExpExecutionResultKind;
    FNativeMatch: TGocciaRegExpMatchResult;
    FCustomMatch: TGocciaValue;
    function CustomProperty(const AName: string): TGocciaValue;
  public
    class function FromNative(const AMatch: TGocciaRegExpMatchResult):
      TGocciaRegExpExecutionResult; static;
    class function FromCustom(
      const AMatch: TGocciaValue): TGocciaRegExpExecutionResult; static;
    function MatchedText: string;
    function MatchIndex(const AInputLength: Integer): Integer;
    function CaptureCount: Integer;
    function CaptureText(const AIndex: Integer;
      out AMatched: Boolean): string;
    function NamedCapturesValue: TGocciaValue;
    function RetainedObject: TGocciaValue;
  end;

function GetRegExpPrototype: TGocciaValue;
function GetRegExpBuiltinExec: TGocciaValue;
procedure SetRegExpPrototype(const APrototype: TGocciaValue);
procedure SetRegExpBuiltinExec(const AExec: TGocciaValue);

function IsRegExpInstance(const AValue: TGocciaValue): Boolean;
function IsRegExp(const AValue: TGocciaValue): Boolean;
function CreateRegExpObject(const APattern, AFlags: string): TGocciaValue;
function CreateRegExpLiteralObject(const APattern, AFlags: string;
  const AProgramCache: TObject; out AUpdatedProgramCache: TObject): TGocciaValue;
function CloneRegExpObject(const AValue: TGocciaValue): TGocciaValue;
procedure ReinitializeRegExpObject(const AValue: TGocciaValue;
  const APattern, AFlags: string);
function GetRegExpInternalSource(const AValue: TGocciaValue): string;
function GetRegExpInternalFlags(const AValue: TGocciaValue): string;
function MatchRegExpObjectOnce(const AValue: TGocciaValue; const AInput: string;
  out AMatchArray: TGocciaValue): Boolean;
function MatchRegExpObjectOnceResult(const AValue: TGocciaValue;
  const AInput: string; out AMatch: TGocciaRegExpExecutionResult): Boolean;
function MatchRegExpBuiltinObjectOnce(const AValue: TGocciaValue;
  const AInput: string; out AMatchArray: TGocciaValue): Boolean;
function MatchRegExpObject(const AValue: TGocciaValue; const AInput: string;
  const AStartIndex: Integer; const ARequireStart, AUpdateLastIndex: Boolean;
  out AMatchArray: TGocciaValue; out AMatchIndex, AMatchEnd,
  ANextIndex: Integer; const AUseCustomExec: Boolean = True): Boolean;
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
  Goccia.GarbageCollector,
  Goccia.Realm,
  Goccia.RegExp.&Program,
  Goccia.RegExp.VM,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToObject;

type
  TGocciaRegExpProgramData = class
  private
    FCompiledProgram: TRegExpProgram;
    FOriginalSource: string;
    FOriginalFlags: string;
  public
    constructor Create(const AProgram: TRegExpProgram;
      const AOriginalSource, AOriginalFlags: string);
    property CompiledProgram: TRegExpProgram read FCompiledProgram;
    property OriginalSource: string read FOriginalSource;
    property OriginalFlags: string read FOriginalFlags;
  end;

  TGocciaCachedRegExpProgram = class
  private
    FFlags: string;
    FProgram: TRegExpProgram;
  public
    constructor Create(const AProgram: TRegExpProgram; const AFlags: string);
    property Flags: string read FFlags;
    property Program_: TRegExpProgram read FProgram;
  end;

threadvar
  // Non-owning cache of the current realm's RegExp prototype (a GC-managed,
  // realm-owned object set on each engine run), so it is not a thread-exit leak
  // (object-reference threadvar audit, #892).
  GRegExpPrototype: TGocciaObjectValue;
  GRegExpBuiltinExec: TGocciaValue;

var
  GRegExpRuntimePrototypeSlot: TGocciaRealmSlotId;
  GRegExpRuntimeBuiltinExecSlot: TGocciaRealmSlotId;

constructor TGocciaRegExpProgramData.Create(const AProgram: TRegExpProgram;
  const AOriginalSource, AOriginalFlags: string);
begin
  inherited Create;
  FCompiledProgram := AProgram;
  FOriginalSource := AOriginalSource;
  FOriginalFlags := AOriginalFlags;
end;

constructor TGocciaCachedRegExpProgram.Create(const AProgram: TRegExpProgram;
  const AFlags: string);
begin
  inherited Create;
  FProgram := AProgram;
  FFlags := AFlags;
end;

function GetRegExpPrototype: TGocciaValue;
begin
  if (CurrentRealm <> nil) and
     CurrentRealm.HasSlot(GRegExpRuntimePrototypeSlot) then
    Exit(TGocciaValue(CurrentRealm.GetSlot(GRegExpRuntimePrototypeSlot)));
  Result := GRegExpPrototype;
end;

function GetRegExpBuiltinExec: TGocciaValue;
begin
  if (CurrentRealm <> nil) and
     CurrentRealm.HasSlot(GRegExpRuntimeBuiltinExecSlot) then
    Exit(TGocciaValue(CurrentRealm.GetSlot(GRegExpRuntimeBuiltinExecSlot)));
  Result := GRegExpBuiltinExec;
end;

procedure SetRegExpPrototype(const APrototype: TGocciaValue);
begin
  GRegExpPrototype := TGocciaObjectValue(APrototype);
  if (CurrentRealm <> nil) then
    CurrentRealm.SetSlot(GRegExpRuntimePrototypeSlot,
      TGocciaObjectValue(APrototype));
end;

procedure SetRegExpBuiltinExec(const AExec: TGocciaValue);
begin
  GRegExpBuiltinExec := AExec;
  if (CurrentRealm <> nil) then
    CurrentRealm.SetSlot(GRegExpRuntimeBuiltinExecSlot,
      TGCManagedObject(AExec));
end;

function CreateRegExpObjectFromProgram(const APattern, AFlags: string;
  const ACompiledProgram: TRegExpProgram): TGocciaValue; forward;
function MatchRegExpNativeObject(const AValue: TGocciaValue;
  const AInput: string; const AStartIndex: Integer;
  const ARequireStart, AUpdateLastIndex: Boolean;
  out AMatchResult: TGocciaRegExpMatchResult; out AMatchIndex, AMatchEnd,
  ANextIndex: Integer): Boolean; forward;
function MatchRegExpObjectResult(const AValue: TGocciaValue;
  const AInput: string; const AStartIndex: Integer;
  const ARequireStart, AUpdateLastIndex: Boolean;
  out AMatch: TGocciaRegExpExecutionResult; out AMatchIndex, AMatchEnd,
  ANextIndex: Integer; const AUseCustomExec: Boolean = True): Boolean; forward;

function GetRegExpProgramData(const AValue: TGocciaValue): TGocciaRegExpProgramData;
begin
  if (not IsRegExpInstance(AValue)) or
     not (TGocciaObjectValue(AValue).RegExpData is TGocciaRegExpProgramData) then
    ThrowTypeError(SErrorRegExpExecNonRegExp);
  Result := TGocciaRegExpProgramData(TGocciaObjectValue(AValue).RegExpData);
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

function IsDefaultRegExpExecMethod(const AMethod: TGocciaValue): Boolean;
var
  BuiltinExec: TGocciaValue;
begin
  BuiltinExec := GetRegExpBuiltinExec;
  Result := Assigned(BuiltinExec) and (AMethod = BuiltinExec);
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

function TryRunRegExpCustomExec(const AValue: TGocciaValue;
  const AInput: string; out AMatchArray: TGocciaValue; out AMatchIndex,
  AMatchEnd, ANextIndex: Integer; out AHandled: Boolean): Boolean;
var
  ExecArgs: TGocciaArgumentsCollection;
  ExecMethod: TGocciaValue;
  ExecResult: TGocciaValue;
  Obj: TGocciaObjectValue;
begin
  Obj := TGocciaObjectValue(AValue);
  AHandled := False;

  ExecMethod := Obj.GetProperty(PROP_EXEC);
  if Assigned(ExecMethod) and
     (not (ExecMethod is TGocciaUndefinedLiteralValue)) and
     (not IsDefaultRegExpExecMethod(ExecMethod)) then
  begin
    if not ExecMethod.IsCallable then
    begin
      if not IsRegExpInstance(AValue) then
        ThrowTypeError(SErrorRegExpExecNotCallable);
      Exit(False);
    end;

    AHandled := True;
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
    AMatchIndex := 0;
    AMatchEnd := 0;
    ANextIndex := 0;
    Exit(True);
  end;

  Result := False;
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
  InputLength := RegExpInputCodeUnitLength(AInput);
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
function BuildNamedGroupsValue(
  const AMatchResult: TGocciaRegExpMatchResult): TGocciaValue;
var
  GroupsObject: TGocciaObjectValue;
  GroupIndex: Integer;
  I: Integer;
begin
  if Length(AMatchResult.NamedGroups) = 0 then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);

  GroupsObject := TGocciaObjectValue.Create(nil);
  // ES2026: Two-pass approach for duplicate named capture groups.
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
  Result := GroupsObject;
end;

function BuildMatchArray(const AInput: string;
  const AMatchResult: TGocciaRegExpMatchResult): TGocciaObjectValue;
var
  IndicesArray: TGocciaArrayValue;
  IndicesGroupsObject: TGocciaObjectValue;
  MatchArray: TGocciaArrayValue;
  GroupsValue: TGocciaValue;
  GroupIndex: Integer;
  I: Integer;

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
  MatchArray.CreateDataPropertyOrThrow(PROP_INDEX,
    TGocciaNumberLiteralValue.Create(AMatchResult.MatchIndex));
  MatchArray.CreateDataPropertyOrThrow(PROP_INPUT,
    TGocciaStringLiteralValue.Create(AInput));
  GroupsValue := BuildNamedGroupsValue(AMatchResult);
  MatchArray.CreateDataPropertyOrThrow(PROP_GROUPS, GroupsValue);

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
        IndicesGroupsObject.CreateDataPropertyOrThrow(AMatchResult.NamedGroups[I].Name,
          TGocciaUndefinedLiteralValue.UndefinedValue);
      for I := 0 to High(AMatchResult.NamedGroups) do
      begin
        GroupIndex := AMatchResult.NamedGroups[I].Index;
        if (GroupIndex <= High(AMatchResult.Groups)) and
           AMatchResult.Groups[GroupIndex].Matched then
          IndicesGroupsObject.CreateDataPropertyOrThrow(
            AMatchResult.NamedGroups[I].Name,
            CreateIndexPair(AMatchResult.Groups[GroupIndex].StartIndex,
              AMatchResult.Groups[GroupIndex].EndIndex));
      end;
      IndicesArray.CreateDataPropertyOrThrow(PROP_GROUPS, IndicesGroupsObject);
    end
    else
      IndicesArray.CreateDataPropertyOrThrow(PROP_GROUPS,
        TGocciaUndefinedLiteralValue.UndefinedValue);

    MatchArray.CreateDataPropertyOrThrow(PROP_INDICES, IndicesArray);
  end;

  Result := MatchArray;
end;

class function TGocciaRegExpExecutionResult.FromNative(
  const AMatch: TGocciaRegExpMatchResult): TGocciaRegExpExecutionResult;
begin
  Result.FKind := rerkNative;
  Result.FNativeMatch := AMatch;
  Result.FCustomMatch := nil;
end;

class function TGocciaRegExpExecutionResult.FromCustom(
  const AMatch: TGocciaValue): TGocciaRegExpExecutionResult;
begin
  Result.FKind := rerkCustom;
  Result.FCustomMatch := AMatch;
end;

function TGocciaRegExpExecutionResult.CustomProperty(
  const AName: string): TGocciaValue;
begin
  Result := TGocciaObjectValue(FCustomMatch).GetProperty(AName);
  if not Assigned(Result) then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaRegExpExecutionResult.MatchedText: string;
begin
  if FKind = rerkCustom then
    Exit(CustomProperty('0').ToStringLiteral.Value);
  Result := FNativeMatch.Groups[0].Value;
end;

function TGocciaRegExpExecutionResult.MatchIndex(
  const AInputLength: Integer): Integer;
begin
  if FKind = rerkCustom then
    Exit(GetClampedIndexValue(CustomProperty(PROP_INDEX), AInputLength));
  if FNativeMatch.MatchIndex <= 0 then
    Exit(0);
  if FNativeMatch.MatchIndex >= AInputLength then
    Exit(AInputLength);
  Result := FNativeMatch.MatchIndex;
end;

function TGocciaRegExpExecutionResult.CaptureCount: Integer;
begin
  if FKind = rerkCustom then
  begin
    Result := LengthOfArrayLike(TGocciaObjectValue(FCustomMatch));
    if Result > 1 then
      Dec(Result)
    else
      Result := 0;
    Exit;
  end;

  Result := Length(FNativeMatch.Groups) - 1;
  if Result < 0 then
    Result := 0;
end;

function TGocciaRegExpExecutionResult.CaptureText(const AIndex: Integer;
  out AMatched: Boolean): string;
var
  Value: TGocciaValue;
begin
  if FKind = rerkCustom then
  begin
    Value := CustomProperty(IntToStr(AIndex));
    AMatched := not (Value is TGocciaUndefinedLiteralValue);
    if AMatched then
      Exit(Value.ToStringLiteral.Value);
    Exit('');
  end;

  if (AIndex <= 0) or (AIndex > High(FNativeMatch.Groups)) or
     not FNativeMatch.Groups[AIndex].Matched then
  begin
    AMatched := False;
    Exit('');
  end;
  AMatched := True;
  Result := FNativeMatch.Groups[AIndex].Value;
end;

function TGocciaRegExpExecutionResult.NamedCapturesValue: TGocciaValue;
begin
  if FKind = rerkCustom then
    Exit(CustomProperty(PROP_GROUPS));
  Result := BuildNamedGroupsValue(FNativeMatch);
end;

function TGocciaRegExpExecutionResult.RetainedObject: TGocciaValue;
begin
  if FKind = rerkCustom then
    Exit(FCustomMatch);
  Result := nil;
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
  CanonicalFlags: string;
  CompiledProgram: TRegExpProgram;
begin
  try
    CanonicalFlags := CanonicalizeRegExpFlags(AFlags);
    CompiledProgram := CompileRegExpProgram(APattern, CanonicalFlags);
  except
    on E: Exception do
      ThrowSyntaxError(E.Message);
  end;

  Result := CreateRegExpObjectFromProgram(APattern, CanonicalFlags, CompiledProgram);
end;

procedure ReinitializeRegExpObject(const AValue: TGocciaValue;
  const APattern, AFlags: string);
var
  CanonicalFlags: string;
  CompiledProgram: TRegExpProgram;
  Source: string;
begin
  if not IsRegExpInstance(AValue) then
    ThrowTypeError(SErrorRegExpExecNonRegExp);
  try
    CanonicalFlags := CanonicalizeRegExpFlags(AFlags);
    CompiledProgram := CompileRegExpProgram(APattern, CanonicalFlags);
  except
    on E: Exception do
      ThrowSyntaxError(E.Message);
  end;
  Source := NormalizeRegExpSource(APattern);
  TGocciaObjectValue(AValue).RegExpData := TGocciaRegExpProgramData.Create(
    CompiledProgram, Source, CanonicalFlags);
end;

function CreateRegExpObjectFromProgram(const APattern, AFlags: string;
  const ACompiledProgram: TRegExpProgram): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Source: string;
begin
  Source := NormalizeRegExpSource(APattern);
  Obj := TGocciaObjectValue.Create(TGocciaObjectValue(GetRegExpPrototype));
  Obj.HasRegExpData := True;
  Obj.RegExpData := TGocciaRegExpProgramData.Create(ACompiledProgram, Source,
    AFlags);
  Obj.DefineProperty(PROP_LAST_INDEX,
    TGocciaPropertyDescriptorData.Create(
      TGocciaNumberLiteralValue.Create(0), [pfWritable]));
  Result := Obj;
end;

function CreateRegExpLiteralObject(const APattern, AFlags: string;
  const AProgramCache: TObject; out AUpdatedProgramCache: TObject): TGocciaValue;
var
  Cached: TGocciaCachedRegExpProgram;
  CanonicalFlags: string;
  CompiledProgram: TRegExpProgram;
begin
  Cached := TGocciaCachedRegExpProgram(AProgramCache);
  if not Assigned(Cached) then
  begin
    try
      CanonicalFlags := CanonicalizeRegExpFlags(AFlags);
      CompiledProgram := CompileRegExpProgram(APattern, CanonicalFlags);
    except
      on E: Exception do
        ThrowSyntaxError(E.Message);
    end;
    Cached := TGocciaCachedRegExpProgram.Create(CompiledProgram, CanonicalFlags);
  end;

  AUpdatedProgramCache := Cached;
  Result := CreateRegExpObjectFromProgram(APattern, Cached.FFlags, Cached.FProgram);
end;

function CloneRegExpObject(const AValue: TGocciaValue): TGocciaValue;
begin
  Result := CreateRegExpObject(
    GetRegExpInternalSource(AValue),
    GetRegExpInternalFlags(AValue));
  Result.SetProperty(PROP_LAST_INDEX, TGocciaNumberLiteralValue.Create(
    GetRegExpLastIndexLength(AValue)));
end;

function GetRegExpInternalSource(const AValue: TGocciaValue): string;
begin
  Result := GetRegExpProgramData(AValue).OriginalSource;
end;

function GetRegExpInternalFlags(const AValue: TGocciaValue): string;
begin
  Result := GetRegExpProgramData(AValue).OriginalFlags;
end;

function MatchRegExpObjectOnce(const AValue: TGocciaValue; const AInput: string;
  out AMatchArray: TGocciaValue): Boolean;
var
  Flags: string;
  StartIndex, MatchIndex, MatchEnd, NextIndex: Integer;
  InputLength: Integer;
  LastIndex: Double;
  HandledCustomExec: Boolean;
begin
  if not IsRegExpInstance(AValue) then
  begin
    Result := MatchRegExpObject(AValue, AInput, 0, False, False, AMatchArray,
      MatchIndex, MatchEnd, NextIndex);
    Exit;
  end;

  Result := TryRunRegExpCustomExec(AValue, AInput, AMatchArray, MatchIndex,
    MatchEnd, NextIndex, HandledCustomExec);
  if HandledCustomExec then
    Exit;

  Flags := GetRegExpInternalFlags(AValue);
  LastIndex := GetRegExpLastIndexLength(AValue);
  if HasRegExpFlag(Flags, 'g') or HasRegExpFlag(Flags, 'y') then
  begin
    InputLength := RegExpInputCodeUnitLength(AInput);
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
    HasRegExpFlag(Flags, 'y'), True, AMatchArray, MatchIndex,
    MatchEnd, NextIndex, False);
end;

function MatchRegExpObjectOnceResult(const AValue: TGocciaValue;
  const AInput: string; out AMatch: TGocciaRegExpExecutionResult): Boolean;
var
  CustomMatch: TGocciaValue;
  Flags: string;
  StartIndex, MatchIndex, MatchEnd, NextIndex: Integer;
  InputLength: Integer;
  LastIndex: Double;
  HandledCustomExec: Boolean;
begin
  if not IsRegExpInstance(AValue) then
  begin
    Result := MatchRegExpObjectResult(AValue, AInput, 0, False, False, AMatch,
      MatchIndex, MatchEnd, NextIndex);
    Exit;
  end;

  Result := TryRunRegExpCustomExec(AValue, AInput, CustomMatch, MatchIndex,
    MatchEnd, NextIndex, HandledCustomExec);
  if HandledCustomExec then
  begin
    if Result then
      AMatch := TGocciaRegExpExecutionResult.FromCustom(CustomMatch);
    Exit;
  end;

  Flags := GetRegExpInternalFlags(AValue);
  LastIndex := GetRegExpLastIndexLength(AValue);
  if HasRegExpFlag(Flags, 'g') or HasRegExpFlag(Flags, 'y') then
  begin
    InputLength := RegExpInputCodeUnitLength(AInput);
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
  Result := MatchRegExpObjectResult(AValue, AInput, StartIndex,
    HasRegExpFlag(Flags, 'y'), True, AMatch, MatchIndex,
    MatchEnd, NextIndex, False);
end;

function MatchRegExpBuiltinObjectOnce(const AValue: TGocciaValue;
  const AInput: string; out AMatchArray: TGocciaValue): Boolean;
var
  Flags: string;
  StartIndex, MatchIndex, MatchEnd, NextIndex: Integer;
  InputLength: Integer;
  LastIndex: Double;
begin
  if not IsRegExpInstance(AValue) then
    ThrowTypeError(SErrorRegExpExecNonRegExp);

  Flags := GetRegExpInternalFlags(AValue);
  LastIndex := GetRegExpLastIndexLength(AValue);
  if HasRegExpFlag(Flags, 'g') or HasRegExpFlag(Flags, 'y') then
  begin
    InputLength := RegExpInputCodeUnitLength(AInput);
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
    HasRegExpFlag(Flags, 'y'), True, AMatchArray, MatchIndex,
    MatchEnd, NextIndex, False);
end;

function MatchRegExpObject(const AValue: TGocciaValue; const AInput: string;
  const AStartIndex: Integer; const ARequireStart, AUpdateLastIndex: Boolean;
  out AMatchArray: TGocciaValue; out AMatchIndex, AMatchEnd,
  ANextIndex: Integer; const AUseCustomExec: Boolean): Boolean;
var
  HandledCustomExec: Boolean;
  MatchResult: TGocciaRegExpMatchResult;
begin
  if AUseCustomExec then
  begin
    Result := TryRunRegExpCustomExec(AValue, AInput, AMatchArray,
      AMatchIndex, AMatchEnd, ANextIndex, HandledCustomExec);
    if HandledCustomExec then
      Exit;
  end;

  Result := MatchRegExpNativeObject(AValue, AInput, AStartIndex,
    ARequireStart, AUpdateLastIndex, MatchResult, AMatchIndex, AMatchEnd,
    ANextIndex);
  if Result then
    AMatchArray := BuildMatchArray(AInput, MatchResult)
  else
    AMatchArray := nil;
end;

function MatchRegExpObjectResult(const AValue: TGocciaValue;
  const AInput: string; const AStartIndex: Integer;
  const ARequireStart, AUpdateLastIndex: Boolean;
  out AMatch: TGocciaRegExpExecutionResult; out AMatchIndex, AMatchEnd,
  ANextIndex: Integer; const AUseCustomExec: Boolean): Boolean;
var
  CustomMatch: TGocciaValue;
  HandledCustomExec: Boolean;
  MatchResult: TGocciaRegExpMatchResult;
begin
  if AUseCustomExec then
  begin
    Result := TryRunRegExpCustomExec(AValue, AInput, CustomMatch,
      AMatchIndex, AMatchEnd, ANextIndex, HandledCustomExec);
    if HandledCustomExec then
    begin
      if Result then
        AMatch := TGocciaRegExpExecutionResult.FromCustom(CustomMatch);
      Exit;
    end;
  end;

  Result := MatchRegExpNativeObject(AValue, AInput, AStartIndex,
    ARequireStart, AUpdateLastIndex, MatchResult, AMatchIndex, AMatchEnd,
    ANextIndex);
  if Result then
    AMatch := TGocciaRegExpExecutionResult.FromNative(MatchResult);
end;

function MatchRegExpNativeObject(const AValue: TGocciaValue;
  const AInput: string; const AStartIndex: Integer;
  const ARequireStart, AUpdateLastIndex: Boolean;
  out AMatchResult: TGocciaRegExpMatchResult; out AMatchIndex, AMatchEnd,
  ANextIndex: Integer): Boolean;
var
  Obj: TGocciaObjectValue;
  ProgramData: TGocciaRegExpProgramData;
  ShouldUpdate: Boolean;
begin
  if not IsRegExpInstance(AValue) then
    ThrowTypeError(SErrorRegExpExecNonRegExp);

  Obj := TGocciaObjectValue(AValue);
  try
    if Obj.RegExpData is TGocciaRegExpProgramData then
    begin
      ProgramData := TGocciaRegExpProgramData(Obj.RegExpData);
      Result := ExecuteCompiledRegExp(
        ProgramData.FCompiledProgram,
        ProgramData.FOriginalSource,
        ProgramData.FOriginalFlags,
        AInput,
        AStartIndex,
        ARequireStart,
        AMatchResult);
    end
    else
      Result := ExecuteRegExp(
        GetRegExpInternalSource(Obj),
        GetRegExpInternalFlags(Obj),
        AInput,
        AStartIndex,
        ARequireStart,
        AMatchResult);
  except
    on E: ERegExpRuntimeError do
      ThrowError(E.Message);
  end;

  ShouldUpdate := AUpdateLastIndex and
    (HasRegExpFlag(GetRegExpInternalFlags(AValue), 'g') or
     HasRegExpFlag(GetRegExpInternalFlags(AValue), 'y'));

  if not Result then
  begin
    if ShouldUpdate then
      Obj.SetProperty(PROP_LAST_INDEX, TGocciaNumberLiteralValue.Create(0));
    AMatchIndex := -1;
    AMatchEnd := -1;
    ANextIndex := -1;
    Exit(False);
  end;

  AMatchIndex := AMatchResult.MatchIndex;
  AMatchEnd := AMatchResult.MatchEnd;
  ANextIndex := AMatchResult.NextIndex;
  if ShouldUpdate then
    Obj.SetProperty(PROP_LAST_INDEX, TGocciaNumberLiteralValue.Create(AMatchEnd));
end;

function RegExpObjectToString(const AValue: TGocciaValue): string;
begin
  Result := Goccia.RegExp.Engine.RegExpToString(
    GetRegExpInternalSource(AValue),
    GetRegExpInternalFlags(AValue));
end;

initialization
  GRegExpRuntimePrototypeSlot := RegisterRealmSlot(
    'RegExp.runtime.prototype');
  GRegExpRuntimeBuiltinExecSlot := RegisterRealmSlot(
    'RegExp.runtime.builtinExec');

end.
