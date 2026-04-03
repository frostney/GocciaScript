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
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaGlobalRegExp = class(TGocciaBuiltin)
  private
    FRegExpConstructor: TGocciaNativeFunctionValue;
    FRegExpPrototype: TGocciaObjectValue;
    class var FPrototypeMembers: array of TGocciaMemberDefinition;

    function RegExpConstructorFn(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  published
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
  public
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback;
      const AObjectPrototype: TGocciaObjectValue);
  end;

implementation

uses
  SysUtils,

  GarbageCollector.Generic,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.RegExp.Runtime,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.HoleValue,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

function GetRegExpBooleanProperty(const AValue: TGocciaObjectValue;
  const AName: string): Boolean;
begin
  Result := AValue.GetProperty(AName).ToBooleanLiteral.Value;
end;

function MatchRegExpObjectValue(const AValue: TGocciaObjectValue;
  const AInput: string; const AStartIndex: Integer;
  const ARequireStart, AUpdateLastIndex: Boolean;
  out AMatchArray: TGocciaObjectValue; out AMatchIndex, AMatchEnd,
  ANextIndex: Integer): Boolean;
var
  MatchValue: TGocciaValue;
begin
  Result := MatchRegExpObject(AValue, AInput, AStartIndex, ARequireStart,
    AUpdateLastIndex, MatchValue, AMatchIndex, AMatchEnd, ANextIndex);
  if Result then
    AMatchArray := TGocciaObjectValue(MatchValue)
  else
    AMatchArray := nil;
end;

function GetRegexReplacementGroup(const AMatchArray: TGocciaArrayValue;
  const AGroupIndex: Integer): string;
var
  GroupValue: TGocciaValue;
begin
  if (AGroupIndex < 0) or (AGroupIndex >= AMatchArray.Elements.Count) then
    Exit('');

  GroupValue := AMatchArray.Elements[AGroupIndex];
  if GroupValue is TGocciaUndefinedLiteralValue then
    Exit('');

  Result := GroupValue.ToStringLiteral.Value;
end;

function ExpandRegexReplacementString(const AReplaceValue: string;
  const AMatchArray: TGocciaArrayValue; const AMatchIndex: Integer;
  const AInput: string): string;
var
  I: Integer;
  NextChar: Char;
  GroupIndex: Integer;
  GroupText: string;
  MatchText: string;
begin
  Result := '';
  MatchText := GetRegexReplacementGroup(AMatchArray, 0);
  I := 1;
  while I <= Length(AReplaceValue) do
  begin
    if AReplaceValue[I] <> '$' then
    begin
      Result := Result + AReplaceValue[I];
      Inc(I);
      Continue;
    end;

    if I = Length(AReplaceValue) then
    begin
      Result := Result + '$';
      Break;
    end;

    NextChar := AReplaceValue[I + 1];
    case NextChar of
      '$':
        begin
          Result := Result + '$';
          Inc(I, 2);
        end;
      '&':
        begin
          Result := Result + MatchText;
          Inc(I, 2);
        end;
      '`':
        begin
          Result := Result + Copy(AInput, 1, AMatchIndex);
          Inc(I, 2);
        end;
      '''':
        begin
          Result := Result + Copy(AInput, AMatchIndex + Length(MatchText) + 1,
            MaxInt);
          Inc(I, 2);
        end;
      '1'..'9':
        begin
          GroupIndex := Ord(NextChar) - Ord('0');
          if (I + 2 <= Length(AReplaceValue)) and
             CharInSet(AReplaceValue[I + 2], ['0'..'9']) and
             ((GroupIndex * 10) +
              (Ord(AReplaceValue[I + 2]) - Ord('0')) <=
              AMatchArray.Elements.Count - 1) then
          begin
            GroupIndex := (GroupIndex * 10) +
              (Ord(AReplaceValue[I + 2]) - Ord('0'));
            Inc(I, 3);
          end
          else
            Inc(I, 2);

          GroupText := GetRegexReplacementGroup(AMatchArray, GroupIndex);
          Result := Result + GroupText;
        end;
    else
      begin
        Result := Result + '$' + NextChar;
        Inc(I, 2);
      end;
    end;
  end;
end;

function BuildRegexReplacement(const AReplaceArg: TGocciaValue;
  const AMatchArray: TGocciaArrayValue; const AMatchIndex: Integer;
  const AInput: string): string;
var
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  if not AReplaceArg.IsCallable then
    Exit(ExpandRegexReplacementString(AReplaceArg.ToStringLiteral.Value,
      AMatchArray, AMatchIndex, AInput));

  CallArgs := TGocciaArgumentsCollection.CreateWithCapacity(
    AMatchArray.Elements.Count + 2);
  try
    for I := 0 to AMatchArray.Elements.Count - 1 do
      CallArgs.Add(AMatchArray.Elements[I]);
    CallArgs.Add(TGocciaNumberLiteralValue.Create(AMatchIndex));
    CallArgs.Add(TGocciaStringLiteralValue.Create(AInput));
    Result := InvokeCallable(AReplaceArg, CallArgs,
      TGocciaUndefinedLiteralValue.UndefinedValue).ToStringLiteral.Value;
  finally
    CallArgs.Free;
  end;
end;

constructor TGocciaGlobalRegExp.Create(const AName: string;
  const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback;
  const AObjectPrototype: TGocciaObjectValue);
var
  Members: TGocciaMemberCollection;
begin
  inherited Create(AName, AScope, AThrowError);

  FRegExpPrototype := TGocciaObjectValue.Create(AObjectPrototype);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.PinObject(FRegExpPrototype);

  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
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
      Members.AddSymbolDataProperty(TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create(CONSTRUCTOR_REGEXP), [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(FRegExpPrototype, FPrototypeMembers);

  SetRegExpPrototype(FRegExpPrototype);

  FRegExpConstructor := TGocciaNativeFunctionValue.Create(RegExpConstructorFn,
    CONSTRUCTOR_REGEXP, 2);
  FRegExpConstructor.AssignProperty(PROP_PROTOTYPE, FRegExpPrototype);
  FRegExpPrototype.AssignProperty(PROP_CONSTRUCTOR, FRegExpConstructor);

  AScope.DefineLexicalBinding(AName, FRegExpConstructor, dtConst);
end;

// ES2026 §22.2.3.1 RegExp ( pattern, flags )
function TGocciaGlobalRegExp.RegExpConstructorFn(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  PatternArg: TGocciaValue;
  Pattern, Flags: string;
  IsConstructCall: Boolean;
begin
  Pattern := '';
  Flags := '';
  IsConstructCall := AThisValue = TGocciaHoleValue.HoleValue;

  if AArgs.Length > 0 then
  begin
    PatternArg := AArgs.GetElement(0);
    if IsRegExpValue(PatternArg) then
    begin
      if not IsConstructCall and
         ((AArgs.Length <= 1) or
          (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue)) then
        Exit(PatternArg);

      Pattern := TGocciaObjectValue(PatternArg).GetProperty(PROP_SOURCE)
        .ToStringLiteral.Value;
      if (AArgs.Length > 1) and
         not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
        Flags := AArgs.GetElement(1).ToStringLiteral.Value
      else
        Flags := TGocciaObjectValue(PatternArg).GetProperty(PROP_FLAGS)
          .ToStringLiteral.Value;
    end
    else
    begin
      Pattern := PatternArg.ToStringLiteral.Value;
      if (AArgs.Length > 1) and
         not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
        Flags := AArgs.GetElement(1).ToStringLiteral.Value;
    end;
  end;

  Result := CreateRegExpObject(Pattern, Flags);
end;

// ES2026 §22.2.6.2 RegExp.prototype.exec(string)
function TGocciaGlobalRegExp.RegExpExec(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Input: string;
  MatchValue: TGocciaValue;
begin
  if not IsRegExpValue(AThisValue) then
    ThrowTypeError('RegExp.prototype.exec called on non-RegExp object');

  if AArgs.Length > 0 then
    Input := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Input := TGocciaUndefinedLiteralValue.UndefinedValue.ToStringLiteral.Value;

  if MatchRegExpObjectOnce(AThisValue, Input, MatchValue) then
    Result := MatchValue
  else
    Result := TGocciaNullLiteralValue.NullValue;
end;

// ES2026 §22.2.6.16 RegExp.prototype.test(S)
function TGocciaGlobalRegExp.RegExpTest(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Input: string;
  MatchValue: TGocciaValue;
begin
  if not IsRegExpValue(AThisValue) then
    ThrowTypeError('RegExp.prototype.test called on non-RegExp object');

  if AArgs.Length > 0 then
    Input := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Input := TGocciaUndefinedLiteralValue.UndefinedValue.ToStringLiteral.Value;

  Result := TGocciaBooleanLiteralValue.Create(
    MatchRegExpObjectOnce(AThisValue, Input, MatchValue));
end;

// ES2026 §22.2.6.17 RegExp.prototype.toString()
function TGocciaGlobalRegExp.RegExpToStringMethod(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not IsRegExpValue(AThisValue) then
    ThrowTypeError('RegExp.prototype.toString called on non-RegExp object');

  Result := TGocciaStringLiteralValue.Create(RegExpObjectToString(AThisValue));
end;

// ES2026 §22.2.6.8 RegExp.prototype [ @@match ] ( string )
function TGocciaGlobalRegExp.RegExpSymbolMatch(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Input: string;
  RegexValue: TGocciaObjectValue;
  MatchValue: TGocciaValue;
  MatchArray: TGocciaObjectValue;
  ResultArray: TGocciaArrayValue;
  MatchIndex, MatchEnd, NextIndex: Integer;
begin
  if not IsRegExpValue(AThisValue) then
    ThrowTypeError('RegExp.prototype[Symbol.match] called on non-RegExp object');

  if AArgs.Length > 0 then
    Input := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Input := TGocciaUndefinedLiteralValue.UndefinedValue.ToStringLiteral.Value;

  RegexValue := TGocciaObjectValue(AThisValue);
  if GetRegExpBooleanProperty(RegexValue, PROP_GLOBAL) then
  begin
    RegexValue.SetProperty(PROP_LAST_INDEX, TGocciaNumberLiteralValue.Create(0));
    ResultArray := TGocciaArrayValue.Create;
    TGarbageCollector.Instance.AddTempRoot(ResultArray);
    try
      while MatchRegExpObjectOnce(RegexValue, Input, MatchValue) do
        ResultArray.Elements.Add(TGocciaArrayValue(MatchValue).Elements[0]);
      RegexValue.SetProperty(PROP_LAST_INDEX, TGocciaNumberLiteralValue.Create(0));
      if ResultArray.Elements.Count = 0 then
        Result := TGocciaNullLiteralValue.NullValue
      else
        Result := ResultArray;
    finally
      TGarbageCollector.Instance.RemoveTempRoot(ResultArray);
    end;
  end
  else if MatchRegExpObjectValue(RegexValue, Input, 0, False, False,
    MatchArray, MatchIndex, MatchEnd, NextIndex) then
    Result := MatchArray
  else
    Result := TGocciaNullLiteralValue.NullValue;
end;

// ES2026 §22.2.6.9 RegExp.prototype [ @@matchAll ] ( string )
function TGocciaGlobalRegExp.RegExpSymbolMatchAll(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Input: string;
  RegexValue: TGocciaObjectValue;
  MatchesArray: TGocciaArrayValue;
  MatchValue: TGocciaValue;
  MatchArray: TGocciaObjectValue;
  MatchIndex, MatchEnd, NextIndex: Integer;
begin
  if not IsRegExpValue(AThisValue) then
    ThrowTypeError('RegExp.prototype[Symbol.matchAll] called on non-RegExp object');

  if AArgs.Length > 0 then
    Input := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Input := TGocciaUndefinedLiteralValue.UndefinedValue.ToStringLiteral.Value;

  RegexValue := TGocciaObjectValue(CloneRegExpObject(AThisValue));
  TGarbageCollector.Instance.AddTempRoot(RegexValue);
  try
    MatchesArray := TGocciaArrayValue.Create;
    TGarbageCollector.Instance.AddTempRoot(MatchesArray);
    try
      if GetRegExpBooleanProperty(RegexValue, PROP_GLOBAL) or
         GetRegExpBooleanProperty(RegexValue, PROP_STICKY) then
      begin
        while MatchRegExpObjectOnce(RegexValue, Input, MatchValue) do
          MatchesArray.Elements.Add(MatchValue);
      end
      else if MatchRegExpObjectValue(RegexValue, Input, 0, False, False,
        MatchArray, MatchIndex, MatchEnd, NextIndex) then
        MatchesArray.Elements.Add(MatchArray);

      Result := TGocciaArrayIteratorValue.Create(MatchesArray, akValues);
    finally
      TGarbageCollector.Instance.RemoveTempRoot(MatchesArray);
    end;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(RegexValue);
  end;
end;

// ES2026 §22.2.6.11 RegExp.prototype [ @@replace ] ( string, replaceValue )
function TGocciaGlobalRegExp.RegExpSymbolReplace(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Input, ResultStr: string;
  ReplaceValue: TGocciaValue;
  RegexValue: TGocciaObjectValue;
  MatchValue: TGocciaValue;
  MatchArray: TGocciaObjectValue;
  MatchIndex, MatchEnd, NextIndex, SearchIndex, OutputIndex: Integer;
begin
  if not IsRegExpValue(AThisValue) then
    ThrowTypeError('RegExp.prototype[Symbol.replace] called on non-RegExp object');

  if AArgs.Length > 0 then
    Input := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Input := TGocciaUndefinedLiteralValue.UndefinedValue.ToStringLiteral.Value;

  if AArgs.Length > 1 then
    ReplaceValue := AArgs.GetElement(1)
  else
    ReplaceValue := TGocciaUndefinedLiteralValue.UndefinedValue;

  RegexValue := TGocciaObjectValue(AThisValue);
  if GetRegExpBooleanProperty(RegexValue, PROP_GLOBAL) then
  begin
    ResultStr := '';
    RegexValue.SetProperty(PROP_LAST_INDEX, TGocciaNumberLiteralValue.Create(0));
    OutputIndex := 0;
    while MatchRegExpObjectOnce(RegexValue, Input, MatchValue) do
    begin
      MatchArray := TGocciaObjectValue(MatchValue);
      MatchIndex := Trunc(MatchArray.GetProperty(PROP_INDEX).ToNumberLiteral.Value);
      MatchEnd := MatchIndex + Length(TGocciaArrayValue(MatchArray).Elements[0]
        .ToStringLiteral.Value);
      SearchIndex := Trunc(RegexValue.GetProperty(PROP_LAST_INDEX)
        .ToNumberLiteral.Value);
      ResultStr := ResultStr + Copy(Input, OutputIndex + 1,
        MatchIndex - OutputIndex);
      ResultStr := ResultStr + BuildRegexReplacement(ReplaceValue,
        TGocciaArrayValue(MatchArray), MatchIndex, Input);
      OutputIndex := MatchEnd;
      if SearchIndex > Length(Input) then
        Break;
    end;
    RegexValue.SetProperty(PROP_LAST_INDEX, TGocciaNumberLiteralValue.Create(0));
    if OutputIndex <= Length(Input) then
      ResultStr := ResultStr + Copy(Input, OutputIndex + 1, MaxInt);
    Result := TGocciaStringLiteralValue.Create(ResultStr);
  end
  else if MatchRegExpObjectValue(RegexValue, Input, 0, False, False,
    MatchArray, MatchIndex, MatchEnd, NextIndex) then
  begin
    ResultStr := BuildRegexReplacement(ReplaceValue,
      TGocciaArrayValue(MatchArray), MatchIndex, Input);
    Result := TGocciaStringLiteralValue.Create(
      Copy(Input, 1, MatchIndex) + ResultStr +
      Copy(Input, MatchEnd + 1, MaxInt));
  end
  else
    Result := TGocciaStringLiteralValue.Create(Input);
end;

// ES2026 §22.2.6.14 RegExp.prototype [ @@search ] ( string )
function TGocciaGlobalRegExp.RegExpSymbolSearch(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Input: string;
  RegexValue: TGocciaObjectValue;
  MatchArray: TGocciaObjectValue;
  MatchIndex, MatchEnd, NextIndex: Integer;
begin
  if not IsRegExpValue(AThisValue) then
    ThrowTypeError('RegExp.prototype[Symbol.search] called on non-RegExp object');

  if AArgs.Length > 0 then
    Input := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Input := TGocciaUndefinedLiteralValue.UndefinedValue.ToStringLiteral.Value;

  RegexValue := TGocciaObjectValue(AThisValue);
  if MatchRegExpObjectValue(RegexValue, Input, 0, False, False, MatchArray,
    MatchIndex, MatchEnd, NextIndex) then
    Result := TGocciaNumberLiteralValue.Create(MatchIndex)
  else
    Result := TGocciaNumberLiteralValue.Create(-1);
end;

// ES2026 §22.2.6.13 RegExp.prototype [ @@split ] ( string, limit )
function TGocciaGlobalRegExp.RegExpSymbolSplit(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Input: string;
  RegexValue: TGocciaObjectValue;
  ResultArray: TGocciaArrayValue;
  MatchArray: TGocciaObjectValue;
  MatchIndex, MatchEnd, NextIndex: Integer;
  PreviousIndex, SearchIndex: Integer;
  Limit: Integer;
  HasLimit: Boolean;
  LastMatchWasZeroWidth: Boolean;
  I: Integer;
begin
  if not IsRegExpValue(AThisValue) then
    ThrowTypeError('RegExp.prototype[Symbol.split] called on non-RegExp object');

  if AArgs.Length > 0 then
    Input := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Input := TGocciaUndefinedLiteralValue.UndefinedValue.ToStringLiteral.Value;

  HasLimit := AArgs.Length > 1;
  if HasLimit then
  begin
    Limit := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value);
    if Limit = 0 then
    begin
      Result := TGocciaArrayValue.Create;
      Exit;
    end;
    if Limit < 0 then
      HasLimit := False;
  end;

  RegexValue := TGocciaObjectValue(AThisValue);
  ResultArray := TGocciaArrayValue.Create;
  TGarbageCollector.Instance.AddTempRoot(ResultArray);
  try
    PreviousIndex := 0;
    SearchIndex := 0;
    LastMatchWasZeroWidth := False;
    while MatchRegExpObjectValue(RegexValue, Input, SearchIndex, False, False,
      MatchArray, MatchIndex, MatchEnd, NextIndex) do
    begin
      LastMatchWasZeroWidth := MatchIndex = MatchEnd;
      if (MatchIndex > PreviousIndex) or not LastMatchWasZeroWidth then
        ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(
          Copy(Input, PreviousIndex + 1, MatchIndex - PreviousIndex)));
      if HasLimit and (ResultArray.Elements.Count >= Limit) then
        Break;

      for I := 1 to TGocciaArrayValue(MatchArray).Elements.Count - 1 do
      begin
        ResultArray.Elements.Add(TGocciaArrayValue(MatchArray).Elements[I]);
        if HasLimit and (ResultArray.Elements.Count >= Limit) then
          Break;
      end;
      if HasLimit and (ResultArray.Elements.Count >= Limit) then
        Break;

      PreviousIndex := MatchEnd;
      SearchIndex := NextIndex;
      if SearchIndex > Length(Input) then
        Break;
    end;

    if (not HasLimit or (ResultArray.Elements.Count < Limit)) and
       ((Length(Input) > PreviousIndex) or not LastMatchWasZeroWidth) then
      ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(
        Copy(Input, PreviousIndex + 1, Length(Input) - PreviousIndex)));

    Result := ResultArray;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(ResultArray);
  end;
end;

end.
