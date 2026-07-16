unit Goccia.Builtins.GlobalRegExp;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.NativeFunction,
  Goccia.Values.NativeFunctionCallback,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaGlobalRegExp = class(TGocciaBuiltin)
  private
    FRegExpConstructor: TGocciaNativeFunctionValue;
    FRegExpPrototype: TGocciaObjectValue;

    function RegExpConstructorFn(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RegExpConstruct(const AArgs: TGocciaArgumentsCollection;
      const ANewTarget: TGocciaValue): TGocciaValue;
  published
    function RegExpSpeciesGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RegExpEscape(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RegExpCompile(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RegExpExec(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RegExpTest(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RegExpToStringMethod(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RegExpSymbolMatch(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RegExpSymbolMatchAll(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RegExpSymbolReplace(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RegExpSymbolSearch(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RegExpSymbolSplit(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RegExpSourceGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RegExpFlagsGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RegExpGlobalGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RegExpIgnoreCaseGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RegExpMultilineGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RegExpDotAllGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RegExpUnicodeGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RegExpStickyGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RegExpUnicodeSetsGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RegExpHasIndicesGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback;
      const AObjectPrototype: TGocciaObjectValue);
  end;

implementation

uses
  StrUtils,
  SysUtils,

  TextSemantics,

  Goccia.Arithmetic,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Realm,
  Goccia.RegExp.Engine,
  Goccia.RegExp.Runtime,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.HoleValue,
  Goccia.Values.Iterator.RegExp,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToObject;

type
  TRegexReplacementCapture = record
    Value: TGocciaValue;
    Text: string;
    Matched: Boolean;
  end;

  TRegexReplacementCaptures = array of TRegexReplacementCapture;

  TReplacementFragmentKind = (rfkText, rfkInputSlice);

  TReplacementFragment = record
    Kind: TReplacementFragmentKind;
    Text: string;
    StartIndex: Integer;
    Count: Integer;
  end;

  TReplacementFragments = array of TReplacementFragment;

  TRegExpExecutionResults = array of TGocciaRegExpExecutionResult;

const
  INITIAL_REPLACEMENT_FRAGMENT_CAPACITY = 8;
  INITIAL_REGEXP_EXECUTION_RESULT_CAPACITY = 8;

var
  GRegExpPrototypeSlot: TGocciaRealmSlotId;

function RequireRegExpObjectReceiver(const AValue: TGocciaValue;
  const AMethodName: string): TGocciaObjectValue;
begin
  if not (AValue is TGocciaObjectValue) then
    ThrowTypeError(AMethodName + ' called on non-object');
  Result := TGocciaObjectValue(AValue);
end;

function GetRegExpFlagsProperty(const AValue: TGocciaObjectValue): string;
begin
  Result := AValue.GetProperty(PROP_FLAGS).ToStringLiteral.Value;
end;

function AddStickyRegExpFlag(const AFlags: string): string;
begin
  if HasRegExpFlag(AFlags, 'y') then
    Result := AFlags
  else
    Result := AFlags + 'y';
end;

function GetRegexMatchLength(const AMatchArray: TGocciaObjectValue): Integer;
begin
  Result := LengthOfArrayLike(AMatchArray);
end;

function GetRegexMatchElement(const AMatchArray: TGocciaObjectValue;
  const AIndex: Integer): TGocciaValue;
begin
  Result := AMatchArray.GetProperty(IntToStr(AIndex));
end;

function RegExpSpeciesConstructor(const ARegExp: TGocciaObjectValue;
  const ADefaultConstructor: TGocciaValue): TGocciaValue;
var
  ConstructorValue, SpeciesValue: TGocciaValue;
begin
  // ES2026 §7.3.22 SpeciesConstructor(O, defaultConstructor)
  ConstructorValue := ARegExp.GetProperty(PROP_CONSTRUCTOR);
  if ConstructorValue is TGocciaUndefinedLiteralValue then
    Exit(ADefaultConstructor);
  if not (ConstructorValue is TGocciaObjectValue) then
    ThrowTypeError(SErrorSpeciesNotConstructor, SSuggestSpeciesConstructor);

  SpeciesValue := TGocciaObjectValue(ConstructorValue).GetSymbolProperty(
    TGocciaSymbolValue.WellKnownSpecies);
  if (SpeciesValue is TGocciaUndefinedLiteralValue) or
     (SpeciesValue is TGocciaNullLiteralValue) then
    Exit(ADefaultConstructor);
  if not SpeciesValue.IsConstructable then
    ThrowTypeError(SErrorSpeciesNotConstructor, SSuggestSpeciesConstructor);
  Result := SpeciesValue;
end;

function ConstructRegExpSpeciesMatcher(const ASpeciesConstructor: TGocciaValue;
  const ARegExp: TGocciaObjectValue; const AFlags: string): TGocciaObjectValue;
var
  ConstructArgs: TGocciaArgumentsCollection;
  ConstructedMatcher: TGocciaValue;
begin
  ConstructArgs := TGocciaArgumentsCollection.Create([
    ARegExp,
    TGocciaStringLiteralValue.Create(AFlags)
  ]);
  try
    ConstructedMatcher := ConstructValue(ASpeciesConstructor, ConstructArgs,
      ASpeciesConstructor);
  finally
    ConstructArgs.Free;
  end;
  if not (ConstructedMatcher is TGocciaObjectValue) then
    ThrowTypeError(SErrorSpeciesNotConstructor, SSuggestSpeciesConstructor);
  Result := TGocciaObjectValue(ConstructedMatcher);
end;

function GetRegexReplacementGroup(
  const ACaptures: TRegexReplacementCaptures;
  const AGroupIndex: Integer): string;
begin
  if (AGroupIndex <= 0) or (AGroupIndex > Length(ACaptures)) then
    Exit('');
  if not ACaptures[AGroupIndex - 1].Matched then
    Exit('');
  Result := ACaptures[AGroupIndex - 1].Text;
end;

function ExpandRegexCaptureReference(
  const ACaptures: TRegexReplacementCaptures;
  const AReferenceText: string): string;
var
  CaptureCount: Integer;
  OneDigitIndex, TwoDigitIndex: Integer;
begin
  CaptureCount := Length(ACaptures);
  if Length(AReferenceText) = 0 then
    Exit('$');

  OneDigitIndex := Ord(AReferenceText[1]) - Ord('0');
  if (Length(AReferenceText) >= 2) and CharInSet(AReferenceText[2], ['0'..'9']) then
  begin
    TwoDigitIndex := (OneDigitIndex * 10) + (Ord(AReferenceText[2]) - Ord('0'));
    if (TwoDigitIndex > 0) and (TwoDigitIndex <= CaptureCount) then
      Exit(GetRegexReplacementGroup(ACaptures, TwoDigitIndex));
    if OneDigitIndex = 0 then
      Exit('$' + AReferenceText);
  end;

  if (OneDigitIndex > 0) and (OneDigitIndex <= CaptureCount) then
  begin
    Result := GetRegexReplacementGroup(ACaptures, OneDigitIndex);
    if Length(AReferenceText) >= 2 then
      Result := Result + Copy(AReferenceText, 2, MaxInt);
    Exit;
  end;

  Result := '$' + AReferenceText;
end;

procedure AddReplacementByteLength(var ATotalLength: Int64;
  const AFragment: string);
var
  FragmentLength: Integer;
begin
  FragmentLength := Length(AFragment);
  if FragmentLength = 0 then
    Exit;
  if ATotalLength > MaxInt - FragmentLength then
    ThrowRangeError(SErrorInvalidStringLength);
  Inc(ATotalLength, FragmentLength);
end;

procedure AppendReplacementFragment(var ATarget: string;
  const AFragment: string);
var
  TargetLength: Int64;
begin
  TargetLength := Length(ATarget);
  AddReplacementByteLength(TargetLength, AFragment);
  ATarget := ATarget + AFragment;
end;

function UTF16CodeUnitByteLength(const ACodeUnit: Cardinal): Integer;
begin
  if ACodeUnit <= $7F then
    Result := 1
  else if ACodeUnit <= $7FF then
    Result := 2
  else
    Result := 3;
end;

function UTF16SubstringByteLength(const AText: string; const AStart,
  ACount: Integer): Integer;
var
  ByteLength: Integer;
  CodePoint: Cardinal;
  CodeUnitIndex: Integer;
  HighSurrogate: Cardinal;
  Index: Integer;
  LowSelected: Boolean;
  LowSurrogate: Cardinal;
  HighSelected: Boolean;
  TargetEnd: Integer;
  Supplementary: Cardinal;
begin
  Result := 0;
  if (AStart < 0) or (ACount <= 0) then
    Exit;

  CodeUnitIndex := 0;
  Index := 1;
  TargetEnd := AStart + ACount;
  while (Index <= Length(AText)) and (CodeUnitIndex < TargetEnd) do
  begin
    if TryReadUTF8CodePointAllowSurrogates(AText, Index, CodePoint,
      ByteLength) then
    begin
      if CodePoint <= $FFFF then
      begin
        if (CodeUnitIndex >= AStart) and (CodeUnitIndex < TargetEnd) then
          Inc(Result, ByteLength);
        Inc(CodeUnitIndex);
      end
      else
      begin
        HighSelected := (CodeUnitIndex >= AStart) and
          (CodeUnitIndex < TargetEnd);
        LowSelected := (CodeUnitIndex + 1 >= AStart) and
          (CodeUnitIndex + 1 < TargetEnd);
        if HighSelected and LowSelected then
          Inc(Result, ByteLength)
        else
        begin
          Supplementary := CodePoint - $10000;
          HighSurrogate := $D800 + (Supplementary shr 10);
          LowSurrogate := $DC00 + (Supplementary and $3FF);
          if HighSelected then
            Inc(Result, UTF16CodeUnitByteLength(HighSurrogate));
          if LowSelected then
            Inc(Result, UTF16CodeUnitByteLength(LowSurrogate));
        end;
        Inc(CodeUnitIndex, 2);
      end;
      Inc(Index, ByteLength);
    end
    else
    begin
      if (CodeUnitIndex >= AStart) and (CodeUnitIndex < TargetEnd) then
        Inc(Result);
      Inc(CodeUnitIndex);
      Inc(Index);
    end;
  end;
end;

procedure EnsureReplacementFragmentCapacity(
  var AFragments: TReplacementFragments; const ARequiredCount: Integer);
var
  NewCapacity: Integer;
begin
  if Length(AFragments) >= ARequiredCount then
    Exit;

  NewCapacity := Length(AFragments);
  if NewCapacity = 0 then
    NewCapacity := INITIAL_REPLACEMENT_FRAGMENT_CAPACITY;
  while NewCapacity < ARequiredCount do
    if NewCapacity > MaxInt div 2 then
      NewCapacity := ARequiredCount
    else
      NewCapacity := NewCapacity * 2;
  SetLength(AFragments, NewCapacity);
end;

procedure EnsureRegExpExecutionResultCapacity(
  var AResults: TRegExpExecutionResults; const ARequiredCount: Integer);
var
  NewCapacity: Integer;
begin
  if Length(AResults) >= ARequiredCount then
    Exit;

  NewCapacity := Length(AResults);
  if NewCapacity = 0 then
    NewCapacity := INITIAL_REGEXP_EXECUTION_RESULT_CAPACITY;
  while NewCapacity < ARequiredCount do
    if NewCapacity > MaxInt div 2 then
      NewCapacity := ARequiredCount
    else
      NewCapacity := NewCapacity * 2;
  SetLength(AResults, NewCapacity);
end;

procedure AddReplacementTextFragment(var AFragments: TReplacementFragments;
  var AFragmentCount: Integer; var ATotalLength: Int64;
  const AText: string);
begin
  if AText = '' then
    Exit;

  AddReplacementByteLength(ATotalLength, AText);
  EnsureReplacementFragmentCapacity(AFragments, AFragmentCount + 1);
  AFragments[AFragmentCount].Kind := rfkText;
  AFragments[AFragmentCount].Text := AText;
  AFragments[AFragmentCount].StartIndex := 0;
  AFragments[AFragmentCount].Count := 0;
  Inc(AFragmentCount);
end;

procedure AddReplacementInputSliceFragment(
  var AFragments: TReplacementFragments; var AFragmentCount: Integer;
  var ATotalLength: Int64; const AStart, ACount: Integer;
  const AFragmentLength: Integer);
begin
  if AFragmentLength = 0 then
    Exit;

  if ATotalLength > MaxInt - AFragmentLength then
    ThrowRangeError(SErrorInvalidStringLength);
  Inc(ATotalLength, AFragmentLength);
  EnsureReplacementFragmentCapacity(AFragments, AFragmentCount + 1);
  AFragments[AFragmentCount].Kind := rfkInputSlice;
  AFragments[AFragmentCount].Text := '';
  AFragments[AFragmentCount].StartIndex := AStart;
  AFragments[AFragmentCount].Count := ACount;
  Inc(AFragmentCount);
end;

function BuildReplacementFragmentsString(const AInput: string;
  const AFragments: TReplacementFragments;
  const AFragmentCount: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to AFragmentCount - 1 do
    case AFragments[I].Kind of
      rfkText:
        AppendReplacementFragment(Result, AFragments[I].Text);
      rfkInputSlice:
        AppendReplacementFragment(Result, UTF16Substring(AInput,
          AFragments[I].StartIndex, AFragments[I].Count));
    end;
end;

function ExpandRegexReplacementString(const AReplaceValue: string;
  const AMatched: string; const AInput: string; const AMatchIndex: Integer;
  const ACaptures: TRegexReplacementCaptures;
  const ANamedCaptures: TGocciaObjectValue): string;
var
  InputLength: Integer;
  I: Integer;
  NextChar: Char;
  GroupText: string;
  CloseAngle: Integer;
  GroupName: string;
  MatchLength: Integer;
  GroupValue: TGocciaValue;
  Fragments: TReplacementFragments;
  FragmentCount: Integer;
  PrefixByteLength: Integer;
  SuffixByteLength: Integer;
  TotalLength: Int64;
begin
  FragmentCount := 0;
  PrefixByteLength := -1;
  SuffixByteLength := -1;
  TotalLength := 0;
  InputLength := UTF16CodeUnitLength(AInput);
  MatchLength := UTF16CodeUnitLength(AMatched);
  I := 1;
  while I <= Length(AReplaceValue) do
  begin
    if AReplaceValue[I] <> '$' then
    begin
      AddReplacementTextFragment(Fragments, FragmentCount, TotalLength,
        AReplaceValue[I]);
      Inc(I);
      Continue;
    end;

    if I = Length(AReplaceValue) then
    begin
      AddReplacementTextFragment(Fragments, FragmentCount, TotalLength, '$');
      Break;
    end;

    NextChar := AReplaceValue[I + 1];
    case NextChar of
      '$':
        begin
          AddReplacementTextFragment(Fragments, FragmentCount, TotalLength, '$');
          Inc(I, 2);
        end;
      '&':
        begin
          AddReplacementTextFragment(Fragments, FragmentCount, TotalLength,
            AMatched);
          Inc(I, 2);
        end;
      '`':
        begin
          if PrefixByteLength < 0 then
            PrefixByteLength := UTF16SubstringByteLength(AInput, 0,
              AMatchIndex);
          AddReplacementInputSliceFragment(Fragments, FragmentCount,
            TotalLength, 0, AMatchIndex, PrefixByteLength);
          Inc(I, 2);
        end;
      '''':
        begin
          if SuffixByteLength < 0 then
            SuffixByteLength := UTF16SubstringByteLength(AInput,
              AMatchIndex + MatchLength,
              InputLength - AMatchIndex - MatchLength);
          AddReplacementInputSliceFragment(Fragments, FragmentCount,
            TotalLength, AMatchIndex + MatchLength,
            InputLength - AMatchIndex - MatchLength, SuffixByteLength);
          Inc(I, 2);
        end;
      '<':
        begin
          CloseAngle := PosEx('>', AReplaceValue, I + 2);
          if CloseAngle = 0 then
          begin
            AddReplacementTextFragment(Fragments, FragmentCount, TotalLength,
              '$<');
            Inc(I, 2);
          end
          else
          begin
            GroupName := Copy(AReplaceValue, I + 2, CloseAngle - I - 2);
            if not Assigned(ANamedCaptures) then
            begin
              AddReplacementTextFragment(Fragments, FragmentCount, TotalLength,
                '$<');
              Inc(I, 2);
            end
            else
            begin
              GroupValue := ANamedCaptures.GetProperty(GroupName);
              if (GroupValue = nil) or
                 (GroupValue is TGocciaUndefinedLiteralValue) then
                AddReplacementTextFragment(Fragments, FragmentCount,
                  TotalLength, '')
              else
                AddReplacementTextFragment(Fragments, FragmentCount,
                  TotalLength,
                  GroupValue.ToStringLiteral.Value);
              I := CloseAngle + 1;
            end;
          end;
        end;
      '0'..'9':
        begin
          GroupText := NextChar;
          if (I + 2 <= Length(AReplaceValue)) and
             CharInSet(AReplaceValue[I + 2], ['0'..'9']) then
          begin
            GroupText := GroupText + AReplaceValue[I + 2];
            Inc(I, 3);
          end
          else
            Inc(I, 2);

          AddReplacementTextFragment(Fragments, FragmentCount, TotalLength,
            ExpandRegexCaptureReference(ACaptures, GroupText));
        end;
    else
      begin
        AddReplacementTextFragment(Fragments, FragmentCount, TotalLength,
          '$' + NextChar);
        Inc(I, 2);
      end;
    end;
  end;

  Result := BuildReplacementFragmentsString(AInput, Fragments, FragmentCount);
end;

constructor TGocciaGlobalRegExp.Create(const AName: string;
  const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback;
  const AObjectPrototype: TGocciaObjectValue);
var
  Members: TGocciaMemberCollection;
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
  StaticMembers: TArray<TGocciaMemberDefinition>;
begin
  inherited Create(AName, AScope, AThrowError);

  FRegExpPrototype := TGocciaObjectValue.Create(AObjectPrototype);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.PinObject(FRegExpPrototype);

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod('compile', RegExpCompile, 2, gmkPrototypeMethod,
      [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('exec', RegExpExec, 1, gmkPrototypeMethod,
      [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('test', RegExpTest, 1, gmkPrototypeMethod,
      [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('toString', RegExpToStringMethod, 0,
      gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddSymbolMethod(TGocciaSymbolValue.WellKnownMatch,
      '[Symbol.match]', RegExpSymbolMatch, 1,
      [pfConfigurable, pfWritable], [gmfNoFunctionPrototype]);
    Members.AddSymbolMethod(TGocciaSymbolValue.WellKnownMatchAll,
      '[Symbol.matchAll]', RegExpSymbolMatchAll, 1,
      [pfConfigurable, pfWritable], [gmfNoFunctionPrototype]);
    Members.AddSymbolMethod(TGocciaSymbolValue.WellKnownReplace,
      '[Symbol.replace]', RegExpSymbolReplace, 2,
      [pfConfigurable, pfWritable], [gmfNoFunctionPrototype]);
    Members.AddSymbolMethod(TGocciaSymbolValue.WellKnownSearch,
      '[Symbol.search]', RegExpSymbolSearch, 1,
      [pfConfigurable, pfWritable], [gmfNoFunctionPrototype]);
    Members.AddSymbolMethod(TGocciaSymbolValue.WellKnownSplit,
      '[Symbol.split]', RegExpSymbolSplit, 2,
      [pfConfigurable, pfWritable], [gmfNoFunctionPrototype]);
    Members.AddAccessor(PROP_SOURCE, RegExpSourceGetter, nil,
      [pfConfigurable]);
    Members.AddAccessor(PROP_FLAGS, RegExpFlagsGetter, nil,
      [pfConfigurable]);
    Members.AddAccessor(PROP_GLOBAL, RegExpGlobalGetter, nil,
      [pfConfigurable]);
    Members.AddAccessor(PROP_IGNORE_CASE, RegExpIgnoreCaseGetter, nil,
      [pfConfigurable]);
    Members.AddAccessor(PROP_MULTILINE, RegExpMultilineGetter, nil,
      [pfConfigurable]);
    Members.AddAccessor(PROP_DOT_ALL, RegExpDotAllGetter, nil,
      [pfConfigurable]);
    Members.AddAccessor(PROP_UNICODE, RegExpUnicodeGetter, nil,
      [pfConfigurable]);
    Members.AddAccessor(PROP_STICKY, RegExpStickyGetter, nil,
      [pfConfigurable]);
    Members.AddAccessor(PROP_UNICODE_SETS, RegExpUnicodeSetsGetter, nil,
      [pfConfigurable]);
    Members.AddAccessor(PROP_HAS_INDICES, RegExpHasIndicesGetter, nil,
      [pfConfigurable]);
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(FRegExpPrototype, PrototypeMembers);

  if Assigned(CurrentRealm) then
    CurrentRealm.SetSlot(GRegExpPrototypeSlot, FRegExpPrototype);
  SetRegExpPrototype(FRegExpPrototype);
  SetRegExpBuiltinExec(FRegExpPrototype.GetProperty(PROP_EXEC));

  FRegExpConstructor := TGocciaNativeFunctionValue.Create(RegExpConstructorFn,
    CONSTRUCTOR_REGEXP, 2);
  FRegExpConstructor.ConstructCallback := RegExpConstruct;
  FRegExpConstructor.DefineProperty(PROP_PROTOTYPE,
    TGocciaPropertyDescriptorData.Create(FRegExpPrototype, []));
  FRegExpConstructor.DefineSymbolProperty(TGocciaSymbolValue.WellKnownSpecies,
    TGocciaPropertyDescriptorAccessor.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(
        RegExpSpeciesGetter, 'get [Symbol.species]', 0),
      nil, [pfConfigurable]));
  FRegExpPrototype.DefineProperty(PROP_CONSTRUCTOR,
    TGocciaPropertyDescriptorData.Create(FRegExpConstructor, [pfConfigurable, pfWritable]));

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddMethod(RegExpEscape, 1, gmkStaticMethod);
    StaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(FRegExpConstructor, StaticMembers);

  AScope.DefineLexicalBinding(AName, FRegExpConstructor, dtConst, True);
end;

function RequireRegExpThis(const AThisValue: TGocciaValue;
  const AMethodName: string): TGocciaObjectValue;
begin
  if not IsRegExpInstance(AThisValue) then
    ThrowTypeError(AMethodName + ' requires a RegExp object');
  Result := TGocciaObjectValue(AThisValue);
end;

function TGocciaGlobalRegExp.RegExpSourceGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AThisValue = FRegExpPrototype then
    Exit(TGocciaStringLiteralValue.Create('(?:)'));
  Result := TGocciaStringLiteralValue.Create(EscapeRegExpPattern(
    GetRegExpInternalSource(
      RequireRegExpThis(AThisValue, 'get RegExp.prototype.source'))));
end;

function TGocciaGlobalRegExp.RegExpFlagsGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Flags: string;
begin
  if not (AThisValue is TGocciaObjectValue) then
    ThrowTypeError('get RegExp.prototype.flags requires an object');
  Obj := TGocciaObjectValue(AThisValue);
  Flags := '';
  if Obj.GetProperty(PROP_HAS_INDICES).ToBooleanLiteral.Value then
    Flags := Flags + 'd';
  if Obj.GetProperty(PROP_GLOBAL).ToBooleanLiteral.Value then
    Flags := Flags + 'g';
  if Obj.GetProperty(PROP_IGNORE_CASE).ToBooleanLiteral.Value then
    Flags := Flags + 'i';
  if Obj.GetProperty(PROP_MULTILINE).ToBooleanLiteral.Value then
    Flags := Flags + 'm';
  if Obj.GetProperty(PROP_DOT_ALL).ToBooleanLiteral.Value then
    Flags := Flags + 's';
  if Obj.GetProperty(PROP_UNICODE).ToBooleanLiteral.Value then
    Flags := Flags + 'u';
  if Obj.GetProperty(PROP_UNICODE_SETS).ToBooleanLiteral.Value then
    Flags := Flags + 'v';
  if Obj.GetProperty(PROP_STICKY).ToBooleanLiteral.Value then
    Flags := Flags + 'y';
  Result := TGocciaStringLiteralValue.Create(Flags);
end;

function TGocciaGlobalRegExp.RegExpGlobalGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AThisValue = FRegExpPrototype then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := TGocciaBooleanLiteralValue.Create(
    HasRegExpFlag(GetRegExpInternalFlags(
      RequireRegExpThis(AThisValue, 'get RegExp.prototype.global')), 'g'));
end;

function TGocciaGlobalRegExp.RegExpIgnoreCaseGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AThisValue = FRegExpPrototype then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := TGocciaBooleanLiteralValue.Create(
    HasRegExpFlag(GetRegExpInternalFlags(
      RequireRegExpThis(AThisValue, 'get RegExp.prototype.ignoreCase')), 'i'));
end;

function TGocciaGlobalRegExp.RegExpMultilineGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AThisValue = FRegExpPrototype then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := TGocciaBooleanLiteralValue.Create(
    HasRegExpFlag(GetRegExpInternalFlags(
      RequireRegExpThis(AThisValue, 'get RegExp.prototype.multiline')), 'm'));
end;

function TGocciaGlobalRegExp.RegExpDotAllGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AThisValue = FRegExpPrototype then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := TGocciaBooleanLiteralValue.Create(
    HasRegExpFlag(GetRegExpInternalFlags(
      RequireRegExpThis(AThisValue, 'get RegExp.prototype.dotAll')), 's'));
end;

function TGocciaGlobalRegExp.RegExpUnicodeGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AThisValue = FRegExpPrototype then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := TGocciaBooleanLiteralValue.Create(
    HasRegExpFlag(GetRegExpInternalFlags(
      RequireRegExpThis(AThisValue, 'get RegExp.prototype.unicode')), 'u'));
end;

function TGocciaGlobalRegExp.RegExpStickyGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AThisValue = FRegExpPrototype then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := TGocciaBooleanLiteralValue.Create(
    HasRegExpFlag(GetRegExpInternalFlags(
      RequireRegExpThis(AThisValue, 'get RegExp.prototype.sticky')), 'y'));
end;

function TGocciaGlobalRegExp.RegExpUnicodeSetsGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AThisValue = FRegExpPrototype then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := TGocciaBooleanLiteralValue.Create(
    HasRegExpFlag(GetRegExpInternalFlags(
      RequireRegExpThis(AThisValue, 'get RegExp.prototype.unicodeSets')), 'v'));
end;

function TGocciaGlobalRegExp.RegExpHasIndicesGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AThisValue = FRegExpPrototype then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := TGocciaBooleanLiteralValue.Create(
    HasRegExpFlag(GetRegExpInternalFlags(
      RequireRegExpThis(AThisValue, 'get RegExp.prototype.hasIndices')), 'd'));
end;

function TGocciaGlobalRegExp.RegExpSpeciesGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := AThisValue;
end;

function IsECMAScriptWhiteSpaceOrLineTerminator(
  const ACodePoint: Cardinal): Boolean; inline;
begin
  case ACodePoint of
    // ES2026 §12.2 White Space
    $0009, $000B, $000C, $0020, $00A0, $FEFF,
    // ES2026 §12.2 White Space — Unicode Zs category
    $1680, $2000..$200A, $202F, $205F, $3000,
    // ES2026 §12.3 Line Terminators
    $000A, $000D, $2028, $2029:
      Result := True;
  else
    Result := False;
  end;
end;

function DecodeUTF8CodePoint(const AStr: string; const AIndex: Integer;
  out ACodePoint: Cardinal): Integer;
var
  B1: Byte;
begin
  B1 := Ord(AStr[AIndex]);

  // Single byte (ASCII)
  if B1 < $80 then
  begin
    ACodePoint := B1;
    Result := 1;
  end
  // 2-byte sequence
  else if ((B1 and $E0) = $C0) and (AIndex + 1 <= Length(AStr)) then
  begin
    ACodePoint := ((B1 and $1F) shl 6) or
                  (Ord(AStr[AIndex + 1]) and $3F);
    Result := 2;
  end
  // 3-byte sequence
  else if ((B1 and $F0) = $E0) and (AIndex + 2 <= Length(AStr)) then
  begin
    ACodePoint := ((B1 and $0F) shl 12) or
                  ((Ord(AStr[AIndex + 1]) and $3F) shl 6) or
                  (Ord(AStr[AIndex + 2]) and $3F);
    Result := 3;
  end
  // 4-byte sequence
  else if ((B1 and $F8) = $F0) and (AIndex + 3 <= Length(AStr)) then
  begin
    ACodePoint := ((B1 and $07) shl 18) or
                  ((Ord(AStr[AIndex + 1]) and $3F) shl 12) or
                  ((Ord(AStr[AIndex + 2]) and $3F) shl 6) or
                  (Ord(AStr[AIndex + 3]) and $3F);
    Result := 4;
  end
  else
  begin
    // Invalid or incomplete sequence — treat as single byte
    ACodePoint := B1;
    Result := 1;
  end;
end;

// TC39 RegExp Escaping §1.1.1 EncodeForRegExpEscape(c)
function EncodeForRegExpEscape(const ACodePoint: Cardinal): string;
var
  High, Low: Cardinal;
begin
  if ACodePoint <= $FF then
    Result := '\x' + LowerCase(IntToHex(ACodePoint, 2))
  else if ACodePoint <= $FFFF then
    Result := '\u' + LowerCase(IntToHex(ACodePoint, 4))
  else
  begin
    // TC39 RegExp Escaping §1.1.1 step 3-7: surrogate pair encoding
    High := $D800 + ((ACodePoint - $10000) shr 10);
    Low := $DC00 + ((ACodePoint - $10000) and $3FF);
    Result := '\u' + LowerCase(IntToHex(High, 4)) +
              '\u' + LowerCase(IntToHex(Low, 4));
  end;
end;

function EncodeControlEscapeForRegExpEscape(const ACodePoint: Cardinal;
  out AEscaped: string): Boolean; inline;
begin
  Result := True;
  case ACodePoint of
    $0009:
      AEscaped := '\t';
    $000A:
      AEscaped := '\n';
    $000B:
      AEscaped := '\v';
    $000C:
      AEscaped := '\f';
    $000D:
      AEscaped := '\r';
  else
    Result := False;
  end;
end;

// TC39 RegExp Escaping §1.1 RegExp.escape(string)
function TGocciaGlobalRegExp.RegExpEscape(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
const
  SYNTAX_CHARACTERS = ['^', '$', '\', '.', '*', '+', '?',
    '(', ')', '[', ']', '{', '}', '|', '/'];
  CLASS_SET_RESERVED_PUNCTUATORS = ['"', '&', '''', '-', '!', '#', '%', ',',
    ':', ';', '<', '=', '>', '@', '`', '~'];
var
  Arg: TGocciaValue;
  Input, Escaped, EscapedCodePoint: string;
  I, ByteLen: Integer;
  CodePoint: Cardinal;
  IsFirst: Boolean;
begin
  // TC39 RegExp Escaping §1.1 step 1
  if AArgs.Length = 0 then
    ThrowTypeError(SErrorRegExpEscapeRequiresString, SSuggestRegExpEscapeString);

  Arg := AArgs.GetElement(0);
  if not (Arg is TGocciaStringLiteralValue) then
    ThrowTypeError(SErrorRegExpEscapeArgMustBeString, SSuggestRegExpEscapeString);

  Input := Arg.ToStringLiteral.Value;
  Escaped := '';
  IsFirst := True;
  I := 1;

  while I <= Length(Input) do
  begin
    if not TryReadUTF8CodePointAllowSurrogates(Input, I, CodePoint, ByteLen) then
      ByteLen := DecodeUTF8CodePoint(Input, I, CodePoint);

    // TC39 RegExp Escaping §1.1 step 4a: first code point is digit or letter
    if IsFirst and (CodePoint < $80) and
       CharInSet(Chr(CodePoint), ['0'..'9', 'a'..'z', 'A'..'Z']) then
    begin
      Escaped := Escaped + EncodeForRegExpEscape(CodePoint);
      IsFirst := False;
      Inc(I, ByteLen);
      Continue;
    end;

    IsFirst := False;

    // ES2026 §22.2.5.1.1 step 2: ControlEscape code points
    if EncodeControlEscapeForRegExpEscape(CodePoint, EscapedCodePoint) then
    begin
      Escaped := Escaped + EscapedCodePoint;
      Inc(I, ByteLen);
      Continue;
    end;

    if (CodePoint >= $D800) and (CodePoint <= $DFFF) then
    begin
      Escaped := Escaped + EncodeForRegExpEscape(CodePoint);
      Inc(I, ByteLen);
      Continue;
    end;

    // TC39 RegExp Escaping §1.1 step 4b: SyntaxCharacter or solidus
    if (CodePoint < $80) and CharInSet(Chr(CodePoint), SYNTAX_CHARACTERS) then
    begin
      Escaped := Escaped + '\' + Chr(CodePoint);
      Inc(I, ByteLen);
      Continue;
    end;

    // TC39 RegExp Escaping §1.1 step 4c: ClassSetReservedPunctuator
    if (CodePoint < $80) and
       CharInSet(Chr(CodePoint), CLASS_SET_RESERVED_PUNCTUATORS) then
    begin
      Escaped := Escaped + EncodeForRegExpEscape(CodePoint);
      Inc(I, ByteLen);
      Continue;
    end;

    // TC39 RegExp Escaping §1.1 step 4d: WhiteSpace or LineTerminator
    if IsECMAScriptWhiteSpaceOrLineTerminator(CodePoint) then
    begin
      Escaped := Escaped + EncodeForRegExpEscape(CodePoint);
      Inc(I, ByteLen);
      Continue;
    end;

    // TC39 RegExp Escaping §1.1 step 4e: pass through
    Escaped := Escaped + Copy(Input, I, ByteLen);
    Inc(I, ByteLen);
  end;

  Result := TGocciaStringLiteralValue.Create(Escaped);
end;

// ES2026 §22.2.3.1 RegExp ( pattern, flags )
function TGocciaGlobalRegExp.RegExpConstructorFn(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  PatternArg, PropVal: TGocciaValue;
  Pattern, Flags: string;
  IsConstructCall, PatternIsRegExp: Boolean;
begin
  Pattern := '';
  Flags := '';
  IsConstructCall := AThisValue = TGocciaHoleValue.HoleValue;

  if AArgs.Length > 0 then
  begin
    PatternArg := AArgs.GetElement(0);
    PatternIsRegExp := IsRegExp(PatternArg);

    // §22.2.3.1 step 2b: non-construct, regexp-like, no flags, same constructor
    if PatternIsRegExp and not IsConstructCall and
       ((AArgs.Length <= 1) or
        (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue)) and
       (TGocciaObjectValue(PatternArg).GetProperty(PROP_CONSTRUCTOR) =
        FRegExpConstructor) then
      Exit(PatternArg);

    // §22.2.3.1 steps 3–4: read source/flags when regexp-like
    if PatternIsRegExp then
    begin
      if IsRegExpInstance(PatternArg) then
        Pattern := GetRegExpInternalSource(PatternArg)
      else
      begin
        PropVal := TGocciaObjectValue(PatternArg).GetProperty(PROP_SOURCE);
        if not (PropVal is TGocciaUndefinedLiteralValue) then
          Pattern := PropVal.ToStringLiteral.Value;
      end;
      if (AArgs.Length > 1) and
         not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
        Flags := AArgs.GetElement(1).ToStringLiteral.Value
      else
      begin
        if IsRegExpInstance(PatternArg) then
          Flags := GetRegExpInternalFlags(PatternArg)
        else
        begin
          PropVal := TGocciaObjectValue(PatternArg).GetProperty(PROP_FLAGS);
          if not (PropVal is TGocciaUndefinedLiteralValue) then
            Flags := PropVal.ToStringLiteral.Value;
        end;
      end;
    end
    else
    begin
      if not (PatternArg is TGocciaUndefinedLiteralValue) then
        Pattern := PatternArg.ToStringLiteral.Value;
      if (AArgs.Length > 1) and
         not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
        Flags := AArgs.GetElement(1).ToStringLiteral.Value;
    end;
  end;

  Result := CreateRegExpObject(Pattern, Flags);
end;

function TGocciaGlobalRegExp.RegExpConstruct(
  const AArgs: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
var
  Proto: TGocciaObjectValue;
  PatternArg, PropVal: TGocciaValue;
  Pattern, Flags: string;
  PatternIsRegExp, FlagsProvided: Boolean;
begin
  Pattern := '';
  Flags := '';
  PatternIsRegExp := False;
  FlagsProvided := (AArgs.Length > 1) and
    not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue);

  // §22.2.4.1 steps 1, 3–4: IsRegExp check, then read source/flags if regexp-like
  if AArgs.Length > 0 then
  begin
    PatternArg := AArgs.GetElement(0);
    PatternIsRegExp := IsRegExp(PatternArg);
    if PatternIsRegExp then
    begin
      if IsRegExpInstance(PatternArg) then
        Pattern := GetRegExpInternalSource(PatternArg)
      else
      begin
        PropVal := TGocciaObjectValue(PatternArg).GetProperty(PROP_SOURCE);
        if not (PropVal is TGocciaUndefinedLiteralValue) then
          Pattern := PropVal.ToStringLiteral.Value;
      end;
      if not FlagsProvided then
      begin
        if IsRegExpInstance(PatternArg) then
          Flags := GetRegExpInternalFlags(PatternArg)
        else
        begin
          PropVal := TGocciaObjectValue(PatternArg).GetProperty(PROP_FLAGS);
          if not (PropVal is TGocciaUndefinedLiteralValue) then
            Flags := PropVal.ToStringLiteral.Value;
        end;
      end;
    end;
  end;

  // §22.2.4.1 step 6: RegExpAlloc(newTarget) → OrdinaryCreateFromConstructor
  Proto := GetProtoFromConstructorWithIntrinsic(ANewTarget, FRegExpPrototype,
    GRegExpPrototypeSlot);

  // §22.2.4.1 step 7: RegExpInitialize — remaining ToString coercions
  if not PatternIsRegExp and (AArgs.Length > 0) and
     not (PatternArg is TGocciaUndefinedLiteralValue) then
    Pattern := PatternArg.ToStringLiteral.Value;
  if FlagsProvided then
    Flags := AArgs.GetElement(1).ToStringLiteral.Value;

  Result := CreateRegExpObject(Pattern, Flags);
  TGocciaObjectValue(Result).Prototype := Proto;
end;

function TGocciaGlobalRegExp.RegExpCompile(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Flags: string;
  Pattern: string;
  PatternArg, PropVal: TGocciaValue;
  PatternIsRegExp: Boolean;
begin
  if not IsRegExpInstance(AThisValue) then
    ThrowTypeError(SErrorRegExpExecNonRegExp, SSuggestRegExpThisType);

  Pattern := '';
  Flags := '';
  if AArgs.Length > 0 then
  begin
    PatternArg := AArgs.GetElement(0);
    PatternIsRegExp := IsRegExp(PatternArg);
    if PatternIsRegExp then
    begin
      if (AArgs.Length > 1) and
         not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
        ThrowTypeError('Cannot supply flags when compiling from a RegExp');
      if IsRegExpInstance(PatternArg) then
      begin
        Pattern := GetRegExpInternalSource(PatternArg);
        Flags := GetRegExpInternalFlags(PatternArg);
      end
      else
      begin
        PropVal := TGocciaObjectValue(PatternArg).GetProperty(PROP_SOURCE);
        if not (PropVal is TGocciaUndefinedLiteralValue) then
          Pattern := PropVal.ToStringLiteral.Value;
        PropVal := TGocciaObjectValue(PatternArg).GetProperty(PROP_FLAGS);
        if not (PropVal is TGocciaUndefinedLiteralValue) then
          Flags := PropVal.ToStringLiteral.Value;
      end;
    end
    else
    begin
      Pattern := PatternArg.ToStringLiteral.Value;
      if (AArgs.Length > 1) and
         not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
        Flags := AArgs.GetElement(1).ToStringLiteral.Value;
    end;
  end;

  ReinitializeRegExpObject(AThisValue, Pattern, Flags);
  TGocciaObjectValue(AThisValue).SetProperty(PROP_LAST_INDEX,
    TGocciaNumberLiteralValue.Create(0));
  Result := AThisValue;
end;

// ES2026 §22.2.6.2 RegExp.prototype.exec(string)
function TGocciaGlobalRegExp.RegExpExec(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Input: string;
  MatchValue: TGocciaValue;
begin
  if not IsRegExpInstance(AThisValue) then
    ThrowTypeError(SErrorRegExpExecNonRegExp, SSuggestRegExpThisType);

  if AArgs.Length > 0 then
    Input := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Input := TGocciaUndefinedLiteralValue.UndefinedValue.ToStringLiteral.Value;

  if MatchRegExpBuiltinObjectOnce(AThisValue, Input, MatchValue) then
    Result := MatchValue
  else
    Result := TGocciaNullLiteralValue.NullValue;
end;

// ES2026 §22.2.6.16 RegExp.prototype.test(S)
function TGocciaGlobalRegExp.RegExpTest(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  CallArgs: TGocciaArgumentsCollection;
  ExecMethod: TGocciaValue;
  Input: string;
  MatchValue: TGocciaValue;
begin
  if not (AThisValue is TGocciaObjectValue) then
    ThrowTypeError(SErrorRegExpTestNonRegExp, SSuggestRegExpThisType);

  if AArgs.Length > 0 then
    Input := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Input := TGocciaUndefinedLiteralValue.UndefinedValue.ToStringLiteral.Value;

  if IsRegExpInstance(AThisValue) then
    Result := TGocciaBooleanLiteralValue.Create(
      MatchRegExpObjectOnce(AThisValue, Input, MatchValue))
  else
  begin
    ExecMethod := TGocciaObjectValue(AThisValue).GetProperty(PROP_EXEC);
    if not ExecMethod.IsCallable then
      ThrowTypeError('RegExp exec property is not callable');
    CallArgs := TGocciaArgumentsCollection.Create;
    try
      CallArgs.Add(TGocciaStringLiteralValue.Create(Input));
      MatchValue := InvokeCallable(ExecMethod, CallArgs, AThisValue);
    finally
      CallArgs.Free;
    end;
    if not (MatchValue is TGocciaObjectValue) and
       not (MatchValue is TGocciaNullLiteralValue) then
      ThrowTypeError(
        'RegExp exec method returned something other than an Object or null');
    Result := TGocciaBooleanLiteralValue.Create(
      not (MatchValue is TGocciaNullLiteralValue));
  end;
end;

// ES2026 §22.2.6.17 RegExp.prototype.toString()
function TGocciaGlobalRegExp.RegExpToStringMethod(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Flags, Source: string;
  Obj: TGocciaObjectValue;
begin
  if not (AThisValue is TGocciaObjectValue) then
    ThrowTypeError(SErrorRegExpToStringNonRegExp, SSuggestRegExpThisType);

  Obj := TGocciaObjectValue(AThisValue);
  Source := Obj.GetProperty(PROP_SOURCE).ToStringLiteral.Value;
  Flags := Obj.GetProperty(PROP_FLAGS).ToStringLiteral.Value;
  Result := TGocciaStringLiteralValue.Create('/' + Source + '/' + Flags);
end;

// ES2026 §22.2.6.8 RegExp.prototype [ @@match ] ( string )
function TGocciaGlobalRegExp.RegExpSymbolMatch(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Flags, Input, MatchString: string;
  RegexValue, RetainedObject: TGocciaObjectValue;
  Match: TGocciaRegExpExecutionResult;
  MatchValue: TGocciaValue;
  ResultArray: TGocciaArrayValue;
  MatchCount: Integer;
  IsUnicode: Boolean;
begin
  RegexValue := RequireRegExpObjectReceiver(AThisValue,
    'RegExp.prototype[Symbol.match]');

  if AArgs.Length > 0 then
    Input := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Input := TGocciaUndefinedLiteralValue.UndefinedValue.ToStringLiteral.Value;

  Flags := GetRegExpFlagsProperty(RegexValue);
  if not HasRegExpFlag(Flags, 'g') then
  begin
    if MatchRegExpObjectOnce(RegexValue, Input, MatchValue) then
      Result := MatchValue
    else
      Result := TGocciaNullLiteralValue.NullValue;
    Exit;
  end;

  IsUnicode := HasUnicodeRegExpFlag(Flags);
  RegexValue.SetProperty(PROP_LAST_INDEX, TGocciaNumberLiteralValue.Create(0));
  ResultArray := TGocciaArrayValue.Create;
  TGarbageCollector.Instance.AddTempRoot(ResultArray);
  try
    MatchCount := 0;
    while True do
    begin
      if not MatchRegExpObjectOnceResult(RegexValue, Input, Match) then
      begin
        if MatchCount = 0 then
          Result := TGocciaNullLiteralValue.NullValue
        else
          Result := ResultArray;
        Exit;
      end;

      RetainedObject := TGocciaObjectValue(Match.RetainedObject);
      if Assigned(RetainedObject) then
        TGarbageCollector.Instance.AddTempRoot(RetainedObject);
      try
        MatchString := Match.MatchedText;
      finally
        if Assigned(RetainedObject) then
          TGarbageCollector.Instance.RemoveTempRoot(RetainedObject);
      end;
      ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(MatchString));
      if MatchString = '' then
        AdvanceProtocolLastIndexAfterEmptyMatch(RegexValue, Input, IsUnicode);
      Inc(MatchCount);
    end;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(ResultArray);
  end;
end;

// ES2026 §22.2.6.9 RegExp.prototype [ @@matchAll ] ( string )
function TGocciaGlobalRegExp.RegExpSymbolMatchAll(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Flags, Input: string;
  RegexValue, Matcher: TGocciaObjectValue;
  SpeciesConstructor: TGocciaValue;
  LastIndex: Double;
  IsGlobal, IsUnicode: Boolean;
begin
  RegexValue := RequireRegExpObjectReceiver(AThisValue,
    'RegExp.prototype[Symbol.matchAll]');

  if AArgs.Length > 0 then
    Input := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Input := TGocciaUndefinedLiteralValue.UndefinedValue.ToStringLiteral.Value;

  SpeciesConstructor := RegExpSpeciesConstructor(RegexValue, FRegExpConstructor);
  Flags := GetRegExpFlagsProperty(RegexValue);
  Matcher := ConstructRegExpSpeciesMatcher(SpeciesConstructor, RegexValue, Flags);
  TGarbageCollector.Instance.AddTempRoot(Matcher);
  try
    LastIndex := GetRegExpLastIndexLength(RegexValue);
    Matcher.SetProperty(PROP_LAST_INDEX, TGocciaNumberLiteralValue.Create(LastIndex));
    IsGlobal := HasRegExpFlag(Flags, 'g');
    IsUnicode := HasUnicodeRegExpFlag(Flags);
    Result := TGocciaRegExpMatchAllIteratorValue.Create(Matcher, Input,
      IsGlobal, IsUnicode);
  finally
    TGarbageCollector.Instance.RemoveTempRoot(Matcher);
  end;
end;

// ES2026 §22.2.6.11 RegExp.prototype [ @@replace ] ( string, replaceValue )
function TGocciaGlobalRegExp.RegExpSymbolReplace(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  AccumulatedResult, Flags, Input, Matched, ReplacementString,
  ReplacementText: string;
  CallArgs: TGocciaArgumentsCollection;
  Captures: TRegexReplacementCaptures;
  CaptureNumber, CaptureCount, I, MatchLength, NextSourcePosition,
  Position, ResultCount, InputLength: Integer;
  NamedCapturesValue, ReplaceValue, ReplacementValue: TGocciaValue;
  CaptureMatched, FunctionalReplace, Global, IsUnicode: Boolean;
  Match: TGocciaRegExpExecutionResult;
  NamedCapturesObject, RegexValue, RetainedObject: TGocciaObjectValue;
  NamedCapturesRoot: TGocciaTempRoot;
  Results: TRegExpExecutionResults;
begin
  RegexValue := RequireRegExpObjectReceiver(AThisValue,
    'RegExp.prototype[Symbol.replace]');

  if AArgs.Length > 0 then
    Input := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Input := TGocciaUndefinedLiteralValue.UndefinedValue.ToStringLiteral.Value;

  if AArgs.Length > 1 then
    ReplaceValue := AArgs.GetElement(1)
  else
    ReplaceValue := TGocciaUndefinedLiteralValue.UndefinedValue;

  FunctionalReplace := ReplaceValue.IsCallable;
  if not FunctionalReplace then
    ReplacementText := ReplaceValue.ToStringLiteral.Value;

  Flags := GetRegExpFlagsProperty(RegexValue);
  Global := HasRegExpFlag(Flags, 'g');
  IsUnicode := HasUnicodeRegExpFlag(Flags);
  if Global then
    RegexValue.SetProperty(PROP_LAST_INDEX, TGocciaNumberLiteralValue.Create(0));

  ResultCount := 0;
  try
    while True do
    begin
      if not MatchRegExpObjectOnceResult(RegexValue, Input, Match) then
        Break;

      EnsureRegExpExecutionResultCapacity(Results, ResultCount + 1);
      Results[ResultCount] := Match;
      Inc(ResultCount);
      RetainedObject := TGocciaObjectValue(Match.RetainedObject);
      if Assigned(RetainedObject) then
        TGarbageCollector.Instance.AddTempRoot(RetainedObject);

      if not Global then
        Break;

      Matched := Match.MatchedText;
      if Matched = '' then
        AdvanceProtocolLastIndexAfterEmptyMatch(RegexValue, Input, IsUnicode);
    end;

    AccumulatedResult := '';
    InputLength := UTF16CodeUnitLength(Input);
    NextSourcePosition := 0;
    for I := 0 to ResultCount - 1 do
    begin
      CaptureCount := Results[I].CaptureCount;

      Matched := Results[I].MatchedText;
      MatchLength := UTF16CodeUnitLength(Matched);
      Position := Results[I].MatchIndex(InputLength);

      SetLength(Captures, CaptureCount);
      for CaptureNumber := 1 to CaptureCount do
      begin
        Captures[CaptureNumber - 1].Text := Results[I].CaptureText(
          CaptureNumber, CaptureMatched);
        if not CaptureMatched then
        begin
          Captures[CaptureNumber - 1].Value :=
            TGocciaUndefinedLiteralValue.UndefinedValue;
          Captures[CaptureNumber - 1].Matched := False;
        end
        else
        begin
          Captures[CaptureNumber - 1].Value :=
            TGocciaStringLiteralValue.Create(Captures[CaptureNumber - 1].Text);
          Captures[CaptureNumber - 1].Matched := True;
        end;
      end;

      NamedCapturesValue := Results[I].NamedCapturesValue;
      InitializeTempRoot(NamedCapturesRoot);
      if NamedCapturesValue is TGocciaObjectValue then
        AddTempRootIfNeeded(NamedCapturesRoot,
          TGocciaObjectValue(NamedCapturesValue));
      try
        if FunctionalReplace then
        begin
          CallArgs := TGocciaArgumentsCollection.CreateWithCapacity(
            CaptureCount + 4);
          try
            CallArgs.Add(TGocciaStringLiteralValue.Create(Matched));
            for CaptureNumber := 0 to CaptureCount - 1 do
              CallArgs.Add(Captures[CaptureNumber].Value);
            CallArgs.Add(TGocciaNumberLiteralValue.Create(Position));
            CallArgs.Add(TGocciaStringLiteralValue.Create(Input));
            if not (NamedCapturesValue is TGocciaUndefinedLiteralValue) then
              CallArgs.Add(NamedCapturesValue);
            ReplacementValue := InvokeCallable(ReplaceValue, CallArgs,
              TGocciaUndefinedLiteralValue.UndefinedValue);
            ReplacementString := ReplacementValue.ToStringLiteral.Value;
          finally
            CallArgs.Free;
          end;
        end
        else
        begin
          if NamedCapturesValue is TGocciaUndefinedLiteralValue then
            NamedCapturesObject := nil
          else
            NamedCapturesObject := ToObject(NamedCapturesValue);
          ReplacementString := ExpandRegexReplacementString(ReplacementText,
            Matched, Input, Position, Captures, NamedCapturesObject);
        end;
      finally
        RemoveTempRootIfNeeded(NamedCapturesRoot);
      end;

      if Position >= NextSourcePosition then
      begin
        AppendReplacementFragment(AccumulatedResult,
          UTF16Substring(Input, NextSourcePosition,
            Position - NextSourcePosition));
        AppendReplacementFragment(AccumulatedResult, ReplacementString);
        NextSourcePosition := Position + MatchLength;
      end;
    end;

    if NextSourcePosition >= InputLength then
      Result := TGocciaStringLiteralValue.Create(AccumulatedResult)
    else
    begin
      AppendReplacementFragment(AccumulatedResult,
        UTF16Substring(Input, NextSourcePosition,
          InputLength - NextSourcePosition));
      Result := TGocciaStringLiteralValue.Create(AccumulatedResult);
    end;
  finally
    for I := 0 to ResultCount - 1 do
    begin
      RetainedObject := TGocciaObjectValue(Results[I].RetainedObject);
      if Assigned(RetainedObject) then
        TGarbageCollector.Instance.RemoveTempRoot(RetainedObject);
    end;
  end;
end;

// ES2026 §22.2.6.12 RegExp.prototype [ @@search ] ( string )
function TGocciaGlobalRegExp.RegExpSymbolSearch(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Input: string;
  RegexValue: TGocciaObjectValue;
  MatchValue: TGocciaValue;
  CurrentLastIndex, PreviousLastIndex: TGocciaValue;
  HasMatch: Boolean;
begin
  RegexValue := RequireRegExpObjectReceiver(AThisValue,
    'RegExp.prototype[Symbol.search]');

  if AArgs.Length > 0 then
    Input := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Input := TGocciaUndefinedLiteralValue.UndefinedValue.ToStringLiteral.Value;

  PreviousLastIndex := RegexValue.GetProperty(PROP_LAST_INDEX);
  if not IsSameValue(PreviousLastIndex, TGocciaNumberLiteralValue.ZeroValue) then
    RegexValue.SetProperty(PROP_LAST_INDEX, TGocciaNumberLiteralValue.Create(0));
  HasMatch := MatchRegExpObjectOnce(RegexValue, Input, MatchValue);
  CurrentLastIndex := RegexValue.GetProperty(PROP_LAST_INDEX);
  if not IsSameValue(CurrentLastIndex, PreviousLastIndex) then
    RegexValue.SetProperty(PROP_LAST_INDEX, PreviousLastIndex);
  if not HasMatch then
    Result := TGocciaNumberLiteralValue.Create(-1)
  else
    Result := TGocciaObjectValue(MatchValue).GetProperty(PROP_INDEX);
end;

// ES2026 §22.2.6.14 RegExp.prototype [ @@split ] ( string, limit )
function TGocciaGlobalRegExp.RegExpSymbolSplit(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Flags, Input, NewFlags: string;
  RegexValue, SplitterValue: TGocciaObjectValue;
  ResultArray: TGocciaArrayValue;
  MatchArray: TGocciaObjectValue;
  MatchValue: TGocciaValue;
  LastMatchEnd, MatchEnd, NumberOfCaptures, SearchIndex, Size: Integer;
  Limit: Cardinal;
  CaptureIndex: Integer;
  SpeciesConstructor: TGocciaValue;
  UnicodeMatching: Boolean;
begin
  RegexValue := RequireRegExpObjectReceiver(AThisValue,
    'RegExp.prototype[Symbol.split]');

  if AArgs.Length > 0 then
    Input := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Input := TGocciaUndefinedLiteralValue.UndefinedValue.ToStringLiteral.Value;

  SpeciesConstructor := RegExpSpeciesConstructor(RegexValue, FRegExpConstructor);
  Flags := GetRegExpFlagsProperty(RegexValue);
  UnicodeMatching := HasUnicodeRegExpFlag(Flags);
  NewFlags := AddStickyRegExpFlag(Flags);
  SplitterValue := ConstructRegExpSpeciesMatcher(SpeciesConstructor, RegexValue,
    NewFlags);

  ResultArray := TGocciaArrayValue.Create;
  TGarbageCollector.Instance.AddTempRoot(ResultArray);
  TGarbageCollector.Instance.AddTempRoot(SplitterValue);
  try
    if (AArgs.Length > 1) and
       not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
      Limit := ToUint32Value(AArgs.GetElement(1))
    else
      Limit := High(Cardinal);

    if Limit = 0 then
    begin
      Result := ResultArray;
      Exit;
    end;

    if UTF16CodeUnitLength(Input) = 0 then
    begin
      if MatchRegExpObjectOnce(SplitterValue, Input, MatchValue) then
      begin
        Result := ResultArray;
        Exit;
      end;
      ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(Input));
      Result := ResultArray;
      Exit;
    end;

    Size := UTF16CodeUnitLength(Input);
    LastMatchEnd := 0;
    SearchIndex := LastMatchEnd;
    while SearchIndex < Size do
    begin
      SplitterValue.SetProperty(PROP_LAST_INDEX,
        TGocciaNumberLiteralValue.Create(SearchIndex));
      if not MatchRegExpObjectOnce(SplitterValue, Input, MatchValue) then
      begin
        SearchIndex := AdvanceUTF16StringIndex(Input, SearchIndex,
          UnicodeMatching);
        Continue;
      end;

      MatchArray := TGocciaObjectValue(MatchValue);
      MatchEnd := GetClampedLastIndex(SplitterValue, Size);
      if MatchEnd = LastMatchEnd then
      begin
        SearchIndex := AdvanceUTF16StringIndex(Input, SearchIndex,
          UnicodeMatching);
        Continue;
      end;

      ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(
        UTF16Substring(Input, LastMatchEnd, SearchIndex - LastMatchEnd)));
      if Cardinal(ResultArray.Elements.Count) >= Limit then
      begin
        Result := ResultArray;
        Exit;
      end;

      LastMatchEnd := MatchEnd;
      NumberOfCaptures := GetRegexMatchLength(MatchArray);
      if NumberOfCaptures > 1 then
        Dec(NumberOfCaptures)
      else
        NumberOfCaptures := 0;
      for CaptureIndex := 1 to NumberOfCaptures do
      begin
        ResultArray.Elements.Add(GetRegexMatchElement(MatchArray, CaptureIndex));
        if Cardinal(ResultArray.Elements.Count) >= Limit then
        begin
          Result := ResultArray;
          Exit;
        end;
      end;
      SearchIndex := LastMatchEnd;
    end;

    ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(
      UTF16Substring(Input, LastMatchEnd, Size - LastMatchEnd)));
    Result := ResultArray;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(SplitterValue);
    TGarbageCollector.Instance.RemoveTempRoot(ResultArray);
  end;
end;

initialization
  GRegExpPrototypeSlot := RegisterRealmSlot('RegExp.prototype');

end.
