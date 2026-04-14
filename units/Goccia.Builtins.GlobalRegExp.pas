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

    function RegExpConstructorFn(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  published
    function RegExpEscape(const AArgs: TGocciaArgumentsCollection;
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
  public
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback;
      const AObjectPrototype: TGocciaObjectValue);
  end;

implementation

uses
  StrUtils,
  SysUtils,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.RegExp.Runtime,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.HoleValue,
  Goccia.Values.Iterator.RegExp,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;
  FStaticMembers: TArray<TGocciaMemberDefinition>;

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

function ExpandRegexCaptureReference(const AMatchArray: TGocciaArrayValue;
  const AReferenceText: string): string;
var
  CaptureCount: Integer;
  OneDigitIndex, TwoDigitIndex: Integer;
begin
  CaptureCount := AMatchArray.Elements.Count - 1;
  if Length(AReferenceText) = 0 then
    Exit('$');

  OneDigitIndex := Ord(AReferenceText[1]) - Ord('0');
  if (Length(AReferenceText) >= 2) and CharInSet(AReferenceText[2], ['0'..'9']) then
  begin
    TwoDigitIndex := (OneDigitIndex * 10) + (Ord(AReferenceText[2]) - Ord('0'));
    if TwoDigitIndex <= CaptureCount then
      Exit(GetRegexReplacementGroup(AMatchArray, TwoDigitIndex));
  end;

  if OneDigitIndex <= CaptureCount then
  begin
    Result := GetRegexReplacementGroup(AMatchArray, OneDigitIndex);
    if Length(AReferenceText) >= 2 then
      Result := Result + Copy(AReferenceText, 2, MaxInt);
    Exit;
  end;

  Result := '$' + AReferenceText;
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
  CloseAngle: Integer;
  GroupName: string;
  GroupsValue, GroupValue: TGocciaValue;
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
      '<':
        begin
          CloseAngle := PosEx('>', AReplaceValue, I + 2);
          if CloseAngle = 0 then
          begin
            Result := Result + '$<';
            Inc(I, 2);
          end
          else
          begin
            GroupName := Copy(AReplaceValue, I + 2, CloseAngle - I - 2);
            GroupsValue := AMatchArray.GetProperty(PROP_GROUPS);
            if (GroupsValue = nil) or
               (GroupsValue is TGocciaUndefinedLiteralValue) then
              Result := Result + ''
            else
            begin
              GroupValue := GroupsValue.GetProperty(GroupName);
              if (GroupValue = nil) or
                 (GroupValue is TGocciaUndefinedLiteralValue) then
                Result := Result + ''
              else
                Result := Result + GroupValue.ToStringLiteral.Value;
            end;
            I := CloseAngle + 1;
          end;
        end;
      '1'..'9':
        begin
          GroupIndex := Ord(NextChar) - Ord('0');
          GroupText := NextChar;
          if (I + 2 <= Length(AReplaceValue)) and
             CharInSet(AReplaceValue[I + 2], ['0'..'9']) then
          begin
            GroupText := GroupText + AReplaceValue[I + 2];
            Inc(I, 3);
          end
          else
            Inc(I, 2);

          Result := Result + ExpandRegexCaptureReference(AMatchArray, GroupText);
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
  GroupsValue: TGocciaValue;
  I: Integer;
begin
  if not AReplaceArg.IsCallable then
    Exit(ExpandRegexReplacementString(AReplaceArg.ToStringLiteral.Value,
      AMatchArray, AMatchIndex, AInput));

  CallArgs := TGocciaArgumentsCollection.CreateWithCapacity(
    AMatchArray.Elements.Count + 3);
  try
    for I := 0 to AMatchArray.Elements.Count - 1 do
      CallArgs.Add(AMatchArray.Elements[I]);
    CallArgs.Add(TGocciaNumberLiteralValue.Create(AMatchIndex));
    CallArgs.Add(TGocciaStringLiteralValue.Create(AInput));
    GroupsValue := AMatchArray.GetProperty(PROP_GROUPS);
    if Assigned(GroupsValue) and
       not (GroupsValue is TGocciaUndefinedLiteralValue) then
      CallArgs.Add(GroupsValue);
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

  if Length(FStaticMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddMethod(RegExpEscape, 1, gmkStaticMethod);
      FStaticMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(FRegExpConstructor, FStaticMembers);

  AScope.DefineLexicalBinding(AName, FRegExpConstructor, dtConst);
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

// TC39 RegExp Escaping §1.1 RegExp.escape(string)
function TGocciaGlobalRegExp.RegExpEscape(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
const
  SYNTAX_CHARACTERS = ['^', '$', '\', '.', '*', '+', '?',
    '(', ')', '[', ']', '{', '}', '|', '/'];
  CLASS_SET_RESERVED_PUNCTUATORS = ['&', '-', '!', '#', '%', ',',
    ':', ';', '<', '=', '>', '@', '`', '~'];
var
  Arg: TGocciaValue;
  Input, Escaped: string;
  I, ByteLen: Integer;
  CodePoint: Cardinal;
  IsFirst: Boolean;
begin
  // TC39 RegExp Escaping §1.1 step 1
  if AArgs.Length = 0 then
    ThrowTypeError('RegExp.escape requires a string argument');

  Arg := AArgs.GetElement(0);
  if not (Arg is TGocciaStringLiteralValue) then
    ThrowTypeError('First argument to RegExp.escape must be a string');

  Input := Arg.ToStringLiteral.Value;
  Escaped := '';
  IsFirst := True;
  I := 1;

  while I <= Length(Input) do
  begin
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
  else if MatchRegExpObjectOnce(RegexValue, Input, MatchValue) then
    Result := MatchValue
  else
    Result := TGocciaNullLiteralValue.NullValue;
end;

// ES2026 §22.2.6.9 RegExp.prototype [ @@matchAll ] ( string )
function TGocciaGlobalRegExp.RegExpSymbolMatchAll(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Input: string;
  RegexClone: TGocciaObjectValue;
  IsGlobal: Boolean;
begin
  if not IsRegExpValue(AThisValue) then
    ThrowTypeError('RegExp.prototype[Symbol.matchAll] called on non-RegExp object');

  if AArgs.Length > 0 then
    Input := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Input := TGocciaUndefinedLiteralValue.UndefinedValue.ToStringLiteral.Value;

  // ES2026 §22.2.6.9 step 4: Let matcher be a clone of R
  RegexClone := TGocciaObjectValue(CloneRegExpObject(AThisValue));
  // ES2026 §22.2.6.9 step 5: global is true iff flags contains "g"
  // Sticky (y) affects match positioning but does not enable repeated iteration
  IsGlobal := GetRegExpBooleanProperty(RegexClone, PROP_GLOBAL);
  Result := TGocciaRegExpMatchAllIteratorValue.Create(RegexClone, Input, IsGlobal);
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
  else if MatchRegExpObjectOnce(RegexValue, Input, MatchValue) then
  begin
    MatchArray := TGocciaObjectValue(MatchValue);
    MatchIndex := Trunc(MatchArray.GetProperty(PROP_INDEX).ToNumberLiteral.Value);
    MatchEnd := MatchIndex + Length(TGocciaArrayValue(MatchArray).Elements[0]
      .ToStringLiteral.Value);
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
  Limit: Cardinal;
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

  HasLimit := (AArgs.Length > 1) and
    not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue);
  if HasLimit then
  begin
    Limit := ToUint32Value(AArgs.GetElement(1));
    // ES2026 §22.2.6.13 step 10: If lim = 0, return ArrayCreate(0).
    if Limit = 0 then
    begin
      Result := TGocciaArrayValue.Create;
      Exit;
    end;
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
      if HasLimit and (Cardinal(ResultArray.Elements.Count) >= Limit) then
        Break;

      for I := 1 to TGocciaArrayValue(MatchArray).Elements.Count - 1 do
      begin
        ResultArray.Elements.Add(TGocciaArrayValue(MatchArray).Elements[I]);
        if HasLimit and (Cardinal(ResultArray.Elements.Count) >= Limit) then
          Break;
      end;
      if HasLimit and (Cardinal(ResultArray.Elements.Count) >= Limit) then
        Break;

      PreviousIndex := MatchEnd;
      SearchIndex := NextIndex;
      if SearchIndex > Length(Input) then
        Break;
    end;

    if (not HasLimit or (Cardinal(ResultArray.Elements.Count) < Limit)) and
       ((Length(Input) > PreviousIndex) or not LastMatchWasZeroWidth) then
      ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(
        Copy(Input, PreviousIndex + 1, Length(Input) - PreviousIndex)));

    Result := ResultArray;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(ResultArray);
  end;
end;

end.
