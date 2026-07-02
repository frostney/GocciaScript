unit Goccia.RegExp.Runtime;

{$I Goccia.inc}

interface

uses
  Goccia.RegExp.Engine,
  Goccia.Values.Primitives;

function GetRegExpPrototype: TGocciaValue;
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
  Goccia.RegExp.&Program,
  Goccia.RegExp.VM,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue;

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
  Result := GRegExpPrototype;
end;

procedure SetRegExpPrototype(const APrototype: TGocciaValue);
begin
  GRegExpPrototype := TGocciaObjectValue(APrototype);
end;

procedure SetRegExpBuiltinExec(const AExec: TGocciaValue);
begin
  GRegExpBuiltinExec := AExec;
end;

function CreateRegExpObjectFromProgram(const APattern, AFlags: string;
  const ACompiledProgram: TRegExpProgram): TGocciaValue; forward;

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
begin
  Result := Assigned(GRegExpBuiltinExec) and (AMethod = GRegExpBuiltinExec);
end;

function HasCustomRegExpExecMethod(const AObject: TGocciaObjectValue): Boolean;
var
  ExecMethod: TGocciaValue;
begin
  ExecMethod := AObject.GetProperty(PROP_EXEC);
  Result := Assigned(ExecMethod) and
    (not (ExecMethod is TGocciaUndefinedLiteralValue)) and
    ExecMethod.IsCallable and
    (not IsDefaultRegExpExecMethod(ExecMethod));
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
    MatchArray.CreateDataPropertyOrThrow(PROP_GROUPS, GroupsObject);
  end
  else
    MatchArray.CreateDataPropertyOrThrow(PROP_GROUPS,
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
  Obj := TGocciaObjectValue.Create(GRegExpPrototype);
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
  Obj: TGocciaObjectValue;
  Flags: string;
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

  if HasCustomRegExpExecMethod(Obj) then
  begin
    Result := MatchRegExpObject(AValue, AInput, 0, False, False, AMatchArray,
      MatchIndex, MatchEnd, NextIndex);
    Exit;
  end;

  Flags := GetRegExpInternalFlags(AValue);
  LastIndex := GetRegExpLastIndexLength(AValue);
  if HasRegExpFlag(Flags, 'g') or HasRegExpFlag(Flags, 'y') then
  begin
    InputLength := UTF16CodeUnitLength(AInput);
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
    MatchEnd, NextIndex);
end;

function MatchRegExpObject(const AValue: TGocciaValue; const AInput: string;
  const AStartIndex: Integer; const ARequireStart, AUpdateLastIndex: Boolean;
  out AMatchArray: TGocciaValue; out AMatchIndex, AMatchEnd,
  ANextIndex: Integer): Boolean;
var
  Obj: TGocciaObjectValue;
  ExecMethod: TGocciaValue;
  ExecArgs: TGocciaArgumentsCollection;
  ExecResult: TGocciaValue;
  MatchResult: TGocciaRegExpMatchResult;
  ProgramData: TGocciaRegExpProgramData;
  ShouldUpdate: Boolean;
begin
  Obj := TGocciaObjectValue(AValue);
  ExecMethod := Obj.GetProperty(PROP_EXEC);
  if Assigned(ExecMethod) and
     (not (ExecMethod is TGocciaUndefinedLiteralValue)) and
     (not IsDefaultRegExpExecMethod(ExecMethod)) then
  begin
    if not ExecMethod.IsCallable then
    begin
      if not IsRegExpInstance(AValue) then
        ThrowTypeError(SErrorRegExpExecNotCallable);
    end
    else
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
      AMatchIndex := 0;
      AMatchEnd := 0;
      ANextIndex := 0;
      Exit(True);
    end;
  end;

  if not IsRegExpInstance(AValue) then
    ThrowTypeError(SErrorRegExpExecNonRegExp);

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
        MatchResult);
    end
    else
      Result := ExecuteRegExp(
        GetRegExpInternalSource(Obj),
        GetRegExpInternalFlags(Obj),
        AInput,
        AStartIndex,
        ARequireStart,
        MatchResult);
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
    GetRegExpInternalSource(AValue),
    GetRegExpInternalFlags(AValue));
end;

end.
