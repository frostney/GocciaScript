unit Goccia.Values.StringObjectValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaStringObjectValue = class(TGocciaInstanceValue)
  private
    FPrimitive: TGocciaStringLiteralValue;

    class var FSharedStringPrototype: TGocciaObjectValue;
    class var FPrototypeMethodHost: TGocciaStringObjectValue;
    class var FPrototypeMembers: array of TGocciaMemberDefinition;

    function ExtractStringValue(const AValue: TGocciaValue): string;
  public
    constructor Create(const APrimitive: TGocciaStringLiteralValue; const AClass: TGocciaClassValue = nil);
    destructor Destroy; override;
    function TypeName: string; override;
    function GetProperty(const AName: string): TGocciaValue; override;

    procedure InitializePrototype;
    procedure MarkReferences; override;

    class function GetSharedPrototype: TGocciaObjectValue;

    property Primitive: TGocciaStringLiteralValue read FPrimitive;

  published
    // String prototype methods
    function StringLength(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringCharAt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringCharCodeAt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringToUpperCase(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringToLowerCase(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringSlice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringSubstring(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringLastIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringIncludes(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringStartsWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringEndsWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringTrim(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringTrimStart(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringTrimEnd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringReplaceMethod(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringReplaceAllMethod(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringSplit(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringMatch(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringMatchAll(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringSearch(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringRepeat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringPadStart(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringPadEnd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringConcat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringAt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringSymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringCodePointAt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringLocaleCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringNormalize(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringIsWellFormed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringToWellFormed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;


implementation

uses
  Math,
  StrUtils,
  SysUtils,

  GarbageCollector.Generic,

  Goccia.Constants.PropertyNames,
  Goccia.RegExp.Runtime,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

{ TGocciaStringObjectValue }

function CoerceRegExpValue(const AValue: TGocciaValue;
  const ANewFlags: string = ''): TGocciaObjectValue;
var
  Pattern: string;
begin
  if IsRegExpValue(AValue) then
    Result := TGocciaObjectValue(CloneRegExpObject(AValue))
  else
  begin
    if AValue is TGocciaUndefinedLiteralValue then
      Pattern := ''
    else
      Pattern := AValue.ToStringLiteral.Value;
    Result := TGocciaObjectValue(CreateRegExpObject(
      Pattern, ANewFlags));
  end;
end;

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

function GetMethodBySymbol(const AValue: TGocciaValue;
  const ASymbol: TGocciaSymbolValue): TGocciaValue;
begin
  if AValue is TGocciaObjectValue then
    Result := TGocciaObjectValue(AValue).GetSymbolProperty(ASymbol)
  else if AValue is TGocciaClassValue then
    Result := TGocciaClassValue(AValue).GetSymbolProperty(ASymbol)
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
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

function TGocciaStringObjectValue.ExtractStringValue(const AValue: TGocciaValue): string;
begin
  if AValue is TGocciaStringLiteralValue then
    Result := TGocciaStringLiteralValue(AValue).Value
  else if AValue is TGocciaStringObjectValue then
    Result := TGocciaStringObjectValue(AValue).Primitive.Value
  else if AValue is TGocciaSymbolValue then
    ThrowTypeError('Cannot convert a Symbol value to a string')
  else
    Result := AValue.ToStringLiteral.Value;
end;

constructor TGocciaStringObjectValue.Create(const APrimitive: TGocciaStringLiteralValue; const AClass: TGocciaClassValue = nil);
begin
  inherited Create(AClass);
  FPrimitive := APrimitive;
  InitializePrototype;
  if not Assigned(AClass) and Assigned(FSharedStringPrototype) then
    FPrototype := FSharedStringPrototype;
end;

destructor TGocciaStringObjectValue.Destroy;
begin
  // Don't free FPrimitiveValue - it's GC-managed
  inherited Destroy;
end;

procedure TGocciaStringObjectValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FPrimitive) then
    FPrimitive.MarkReferences;
end;

function TGocciaStringObjectValue.TypeName: string;
begin
  Result := 'object';  // Boxed primitives are objects
end;

function TGocciaStringObjectValue.GetProperty(const AName: string): TGocciaValue;
var
  Index: Integer;
  StringValue: string;
begin
  StringValue := FPrimitive.ToStringLiteral.Value;

  if TryStrToInt(AName, Index) then
  begin
    if (Index >= 0) and (Index < Length(StringValue)) then
      Result := TGocciaStringLiteralValue.Create(StringValue[Index + 1])
    else
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  Result := inherited GetProperty(AName);
  if not (Result is TGocciaUndefinedLiteralValue) then
    Exit;

  if Assigned(FSharedStringPrototype) then
    Result := FSharedStringPrototype.GetPropertyWithContext(AName, Self);
end;

procedure TGocciaStringObjectValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
begin
  if Assigned(FSharedStringPrototype) then Exit;

  FSharedStringPrototype := TGocciaObjectValue.Create;
  FPrototypeMethodHost := Self;
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddAccessor(PROP_LENGTH, StringLength, nil, []);
      Members.AddMethod(StringCharAt, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringCharCodeAt, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringToUpperCase, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringToLowerCase, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringSlice, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringSubstring, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringIndexOf, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringLastIndexOf, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringIncludes, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringStartsWith, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringEndsWith, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringTrim, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringTrimStart, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringTrimEnd, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('replace', StringReplaceMethod, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('replaceAll', StringReplaceAllMethod, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringSplit, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringMatch, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringMatchAll, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringSearch, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringRepeat, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringPadStart, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringPadEnd, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringConcat, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringAt, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringValueOf, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringToString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringCodePointAt, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringLocaleCompare, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringNormalize, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringIsWellFormed, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(StringToWellFormed, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolMethod(
        TGocciaSymbolValue.WellKnownIterator,
        '[Symbol.iterator]',
        StringSymbolIterator,
        0,
        [pfConfigurable, pfWritable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(FSharedStringPrototype, FPrototypeMembers);

  if Assigned(TGarbageCollector.Instance) then
  begin
    TGarbageCollector.Instance.PinObject(FSharedStringPrototype);
    TGarbageCollector.Instance.PinObject(FPrototypeMethodHost);
  end;
end;

class function TGocciaStringObjectValue.GetSharedPrototype: TGocciaObjectValue;
begin
  if not Assigned(FSharedStringPrototype) then
    TGocciaStringObjectValue.Create(TGocciaStringLiteralValue.Create(''));
  Result := FSharedStringPrototype;
end;

// ES2026 §22.1.3 String.prototype.length (accessor)
function TGocciaStringObjectValue.StringLength(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);
  // Step 3: Return the number of code units in S
  Result := TGocciaNumberLiteralValue.Create(Length(StringValue));
end;

// ES2026 §22.1.3.1 String.prototype.charAt(pos)
function TGocciaStringObjectValue.StringCharAt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  Index: Integer;
  TempNumberValue: TGocciaNumberLiteralValue;
  Arg: TGocciaValue;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let position be ToIntegerOrInfinity(pos)
  if AArgs.Length > 0 then
  begin
    Arg := AArgs.GetElement(0);
    if (Arg is TGocciaUndefinedLiteralValue) or (Arg is TGocciaNullLiteralValue) then
      Index := 0
    else
    begin
      TempNumberValue := Arg.ToNumberLiteral;
      if TempNumberValue.IsNaN then
        Index := 0
      else if TempNumberValue.IsInfinity or TempNumberValue.IsNegativeInfinity then
        Index := MaxInt
      else
        Index := Trunc(TempNumberValue.Value);
    end;
  end
  else
    Index := 0;

  // Step 4: If position < 0 or position >= len(S), return ""
  // Step 5: Return the String value of length 1 containing the code unit at index position
  if (Index >= 0) and (Index < Length(StringValue)) then
    Result := TGocciaStringLiteralValue.Create(StringValue[Index + 1])
  else
    Result := TGocciaStringLiteralValue.Create('');
end;

// ES2026 §22.1.3.2 String.prototype.charCodeAt(pos)
function TGocciaStringObjectValue.StringCharCodeAt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  Index: Integer;
  TempNumberValue: TGocciaNumberLiteralValue;
  Arg: TGocciaValue;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let position be ToIntegerOrInfinity(pos)
  if AArgs.Length > 0 then
  begin
    Arg := AArgs.GetElement(0);
    if (Arg is TGocciaUndefinedLiteralValue) or (Arg is TGocciaNullLiteralValue) then
      Index := 0
    else
    begin
      TempNumberValue := Arg.ToNumberLiteral;
      if TempNumberValue.IsNaN then
        Index := 0
      else if TempNumberValue.IsInfinity or TempNumberValue.IsNegativeInfinity then
        Index := MaxInt
      else
        Index := Trunc(TempNumberValue.Value);
    end;
  end
  else
    Index := 0;

  // Step 4: If position < 0 or position >= len(S), return NaN
  // Step 5: Return the numeric value of the code unit at index position
  if (Index >= 0) and (Index < Length(StringValue)) then
    Result := TGocciaNumberLiteralValue.Create(Ord(StringValue[Index + 1]))
  else
    Result := TGocciaNumberLiteralValue.NaNValue;
end;

// ES2026 §22.1.3.28 String.prototype.toUpperCase()
function TGocciaStringObjectValue.StringToUpperCase(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let cpList be StringToCodePoints(S)
  // Step 4: Let cuList be the result of toUppercase(cpList)
  // Step 5: Return CodePointsToString(cuList)
  Result := TGocciaStringLiteralValue.Create(UpperCase(StringValue));
end;

// ES2026 §22.1.3.26 String.prototype.toLowerCase()
function TGocciaStringObjectValue.StringToLowerCase(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let cpList be StringToCodePoints(S)
  // Step 4: Let cuList be the result of toLowercase(cpList)
  // Step 5: Return CodePointsToString(cuList)
  Result := TGocciaStringLiteralValue.Create(LowerCase(StringValue));
end;

// ES2026 §22.1.3.22 String.prototype.slice(start, end)
function TGocciaStringObjectValue.StringSlice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  StartIndex, EndIndex: Integer;
  Len: Integer;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let len be the length of S
  Len := Length(StringValue);

  // Step 4: Let intStart be ToIntegerOrInfinity(start)
  StartIndex := ToIntegerFromArgs(AArgs);

  // Step 5: If intStart < 0, let from be max(len + intStart, 0); else let from be min(intStart, len)
  StartIndex := NormalizeRelativeIndex(StartIndex, Len);

  // Step 6: If end is undefined, let intEnd be len; else let intEnd be ToIntegerOrInfinity(end)
  EndIndex := ToIntegerFromArgs(AArgs, 1, Len);

  // Step 7: If intEnd < 0, let to be max(len + intEnd, 0); else let to be min(intEnd, len)
  EndIndex := NormalizeRelativeIndex(EndIndex, Len);

  // Step 8: If from >= to, return ""
  if StartIndex > EndIndex then
    StartIndex := EndIndex;

  // Step 9: Return the substring of S from index from to index to
  if (StartIndex >= 0) and (StartIndex < Len) and (EndIndex > StartIndex) then
    Result := TGocciaStringLiteralValue.Create(Copy(StringValue, StartIndex + 1, EndIndex - StartIndex))
  else
    Result := TGocciaStringLiteralValue.Create('');
end;

// ES2026 §22.1.3.25 String.prototype.substring(start, end)
function TGocciaStringObjectValue.StringSubstring(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  StartIndex, EndIndex: Integer;
  Len: Integer;
  Temp: Integer;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let len be the length of S
  Len := Length(StringValue);

  // Step 4: Let intStart be ToIntegerOrInfinity(start)
  StartIndex := ToIntegerFromArgs(AArgs);

  // Step 5: If end is undefined, let intEnd be len; else let intEnd be ToIntegerOrInfinity(end)
  EndIndex := ToIntegerFromArgs(AArgs, 1, Len);

  // Step 6: Let finalStart be min(max(intStart, 0), len)
  // Step 7: Let finalEnd be min(max(intEnd, 0), len)
  StartIndex := Max(0, Min(StartIndex, Len));
  EndIndex := Max(0, Min(EndIndex, Len));

  // Step 8: Let from be min(finalStart, finalEnd), let to be max(finalStart, finalEnd)
  if StartIndex > EndIndex then
  begin
    Temp := StartIndex;
    StartIndex := EndIndex;
    EndIndex := Temp;
  end;

  // Step 9: Return the substring of S from index from to index to
  if EndIndex > StartIndex then
    Result := TGocciaStringLiteralValue.Create(Copy(StringValue, StartIndex + 1, EndIndex - StartIndex))
  else
    Result := TGocciaStringLiteralValue.Create('');
end;

// ES2026 §22.1.3.9 String.prototype.indexOf(searchString [, position])
function TGocciaStringObjectValue.StringIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  StartPosition: Integer;
  FoundIndex: Integer;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let searchStr be ToString(searchString)
  if AArgs.Length > 0 then
    SearchValue := AArgs.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  // Step 4: Let pos be ToIntegerOrInfinity(position)
  StartPosition := ToIntegerFromArgs(AArgs, 1);

  // Step 5: Let start be min(max(pos, 0), len)
  StartPosition := Max(0, StartPosition);

  if SearchValue = '' then
  begin
    Result := TGocciaNumberLiteralValue.Create(Min(StartPosition, Length(StringValue)));
    Exit;
  end;

  // Step 6-7: Search for first occurrence of searchStr in S at or after start; return index or -1
  if StartPosition < Length(StringValue) then
  begin
    FoundIndex := Pos(SearchValue, Copy(StringValue, StartPosition + 1, Length(StringValue)));
    if FoundIndex > 0 then
      Result := TGocciaNumberLiteralValue.Create(FoundIndex + StartPosition - 1)
    else
      Result := TGocciaNumberLiteralValue.Create(-1);
  end
  else
    Result := TGocciaNumberLiteralValue.Create(-1);
end;

// ES2026 §22.1.3.10 String.prototype.lastIndexOf(searchString [, position])
function TGocciaStringObjectValue.StringLastIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  StartPosition: Integer;
  FoundIndex: Integer;
  I: Integer;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let searchStr be ToString(searchString)
  if AArgs.Length > 0 then
    SearchValue := AArgs.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  // Step 4: Let numPos be ToNumber(position); if NaN let pos be +∞, else ToIntegerOrInfinity
  if AArgs.Length > 1 then
    StartPosition := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value)
  else
    StartPosition := Length(StringValue);

  // Step 5: Let start be min(max(pos, 0), len)
  StartPosition := Min(StartPosition, Length(StringValue));

  if SearchValue = '' then
  begin
    Result := TGocciaNumberLiteralValue.Create(StartPosition);
    Exit;
  end;

  // Step 6-7: Search backwards for last occurrence of searchStr at or before start; return index or -1
  FoundIndex := -1;
  if StartPosition >= 0 then
  begin
    for I := Min(StartPosition, Length(StringValue) - Length(SearchValue)) downto 0 do
    begin
      if (I + Length(SearchValue) <= Length(StringValue)) and
         (Copy(StringValue, I + 1, Length(SearchValue)) = SearchValue) then
      begin
        FoundIndex := I;
        Break;
      end;
    end;
  end;

  Result := TGocciaNumberLiteralValue.Create(FoundIndex);
end;

// ES2026 §22.1.3.7 String.prototype.includes(searchString [, position])
function TGocciaStringObjectValue.StringIncludes(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  StartPosition: Integer;
  FoundIndex: Integer;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let searchStr be ToString(searchString)
  if AArgs.Length > 0 then
    SearchValue := AArgs.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  // Step 4: Let pos be ToIntegerOrInfinity(position)
  if AArgs.Length > 1 then
    StartPosition := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value)
  else
    StartPosition := 0;

  // Step 5: Let start be min(max(pos, 0), len)
  StartPosition := Max(0, StartPosition);

  if StartPosition >= Length(StringValue) then
  begin
    if SearchValue = '' then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  if SearchValue = '' then
  begin
    Result := TGocciaBooleanLiteralValue.TrueValue;
    Exit;
  end;

  // Step 6: Search for searchStr in S starting at start
  // Step 7: If found, return true; otherwise return false
  FoundIndex := Pos(SearchValue, Copy(StringValue, StartPosition + 1, Length(StringValue)));
  if FoundIndex > 0 then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §22.1.3.24 String.prototype.startsWith(searchString [, position])
function TGocciaStringObjectValue.StringStartsWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  StartPosition: Integer;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let searchStr be ToString(searchString)
  if AArgs.Length > 0 then
    SearchValue := AArgs.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  // Step 4: Let searchLength be the length of searchStr
  // Step 5: Let start be min(max(ToIntegerOrInfinity(position), 0), len)
  if AArgs.Length > 1 then
    StartPosition := Max(0, Trunc(AArgs.GetElement(1).ToNumberLiteral.Value))
  else
    StartPosition := 0;

  // Step 6: If searchLength + start > len(S), return false
  // Step 7: If the code units of S starting at start match searchStr, return true; else false
  if StartPosition + Length(SearchValue) > Length(StringValue) then
    Result := TGocciaBooleanLiteralValue.FalseValue
  else if Copy(StringValue, StartPosition + 1, Length(SearchValue)) = SearchValue then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §22.1.3.6 String.prototype.endsWith(searchString [, endPosition])
function TGocciaStringObjectValue.StringEndsWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  EndPosition: Integer;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let searchStr be ToString(searchString)
  if AArgs.Length > 0 then
    SearchValue := AArgs.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  // Step 4: Let searchLength be the length of searchStr
  // Step 5: If endPosition is undefined, let pos be len; else let pos be ToIntegerOrInfinity(endPosition)
  // Step 6: Let end be min(max(pos, 0), len)
  if AArgs.Length > 1 then
    EndPosition := Min(Max(0, Trunc(AArgs.GetElement(1).ToNumberLiteral.Value)), Length(StringValue))
  else
    EndPosition := Length(StringValue);

  // Step 7: Let start be end - searchLength
  // Step 8: If start < 0, return false
  // Step 9: If code units of S from start to end match searchStr, return true; else false
  if EndPosition < Length(SearchValue) then
    Result := TGocciaBooleanLiteralValue.FalseValue
  else if Copy(StringValue, EndPosition - Length(SearchValue) + 1, Length(SearchValue)) = SearchValue then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §22.1.3.29 String.prototype.trim()
function TGocciaStringObjectValue.StringTrim(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Return TrimString(S, start+end)
  Result := TGocciaStringLiteralValue.Create(Trim(StringValue));
end;

// ES2026 §22.1.3.30 String.prototype.trimStart()
function TGocciaStringObjectValue.StringTrimStart(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Return TrimString(S, start)
  Result := TGocciaStringLiteralValue.Create(TrimLeft(StringValue));
end;

// ES2026 §22.1.3.31 String.prototype.trimEnd()
function TGocciaStringObjectValue.StringTrimEnd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Return TrimString(S, end)
  Result := TGocciaStringLiteralValue.Create(TrimRight(StringValue));
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

// ES2026 §22.1.3.19 String.prototype.replace(searchValue, replaceValue)
function TGocciaStringObjectValue.StringReplaceMethod(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue, ReplaceValue, ResultStr: string;
  SearchArg: TGocciaValue;
  ReplaceArg: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  FoundPos: Integer;
  CallResult: TGocciaValue;
  ReplaceMethod: TGocciaValue;
  RegexValue: TGocciaObjectValue;
  MatchArray: TGocciaObjectValue;
  MatchIndex, MatchEnd, NextIndex, Offset: Integer;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let string be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let searchString be ToString(searchValue)
  if AArgs.Length > 0 then
    SearchArg := AArgs.GetElement(0)
  else
    SearchArg := TGocciaUndefinedLiteralValue.UndefinedValue;

  // Step 4: Let replaceValue (or functionalReplace if callable)
  if AArgs.Length > 1 then
    ReplaceArg := AArgs.GetElement(1)
  else
    ReplaceArg := TGocciaUndefinedLiteralValue.UndefinedValue;

  ReplaceMethod := GetMethodBySymbol(SearchArg,
    TGocciaSymbolValue.WellKnownReplace);
  if not (ReplaceMethod is TGocciaUndefinedLiteralValue) then
  begin
    if not ReplaceMethod.IsCallable then
      ThrowTypeError('@@replace is not callable');
    CallArgs := TGocciaArgumentsCollection.Create([
      TGocciaStringLiteralValue.Create(StringValue),
      ReplaceArg
    ]);
    try
      Result := InvokeCallable(ReplaceMethod, CallArgs, SearchArg);
    finally
      CallArgs.Free;
    end;
    Exit;
  end;

  if IsRegExpValue(SearchArg) then
  begin
    RegexValue := CoerceRegExpValue(SearchArg);
    TGarbageCollector.Instance.AddTempRoot(RegexValue);
    try
      if GetRegExpBooleanProperty(RegexValue, PROP_GLOBAL) then
      begin
        ResultStr := '';
        Offset := 0;
        while MatchRegExpObjectValue(RegexValue, StringValue, Offset, False,
          False, MatchArray, MatchIndex, MatchEnd, NextIndex) do
        begin
          ResultStr := ResultStr + Copy(StringValue, Offset + 1,
            MatchIndex - Offset);
          ResultStr := ResultStr + BuildRegexReplacement(ReplaceArg,
            TGocciaArrayValue(MatchArray), MatchIndex, StringValue);
          Offset := NextIndex;
          if Offset > Length(StringValue) then
            Break;
        end;
        if Offset <= Length(StringValue) then
          ResultStr := ResultStr + Copy(StringValue, Offset + 1, MaxInt);
        Result := TGocciaStringLiteralValue.Create(ResultStr);
      end
      else if MatchRegExpObjectValue(RegexValue, StringValue, 0, False, False,
        MatchArray, MatchIndex, MatchEnd, NextIndex) then
      begin
        ReplaceValue := BuildRegexReplacement(ReplaceArg,
          TGocciaArrayValue(MatchArray),
          MatchIndex, StringValue);
        Result := TGocciaStringLiteralValue.Create(
          Copy(StringValue, 1, MatchIndex) + ReplaceValue +
          Copy(StringValue, MatchEnd + 1, MaxInt));
      end
      else
        Result := TGocciaStringLiteralValue.Create(StringValue);
    finally
      TGarbageCollector.Instance.RemoveTempRoot(RegexValue);
    end;
    Exit;
  end;

  SearchValue := SearchArg.ToStringLiteral.Value;

  // Step 5: Let pos be StringIndexOf(string, searchString, 0)
  // Step 6: If not found, return string
  // Step 7: Replace first occurrence
  if ReplaceArg.IsCallable then
  begin
    FoundPos := Pos(SearchValue, StringValue);
    if FoundPos > 0 then
    begin
      CallArgs := TGocciaArgumentsCollection.Create([
        TGocciaStringLiteralValue.Create(SearchValue),
        TGocciaNumberLiteralValue.Create(FoundPos - 1),
        TGocciaStringLiteralValue.Create(StringValue)
      ]);
      try
        CallResult := InvokeCallable(ReplaceArg, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
        ReplaceValue := CallResult.ToStringLiteral.Value;
      finally
        CallArgs.Free;
      end;
      Result := TGocciaStringLiteralValue.Create(
        Copy(StringValue, 1, FoundPos - 1) + ReplaceValue +
        Copy(StringValue, FoundPos + Length(SearchValue), Length(StringValue)));
    end
    else
      Result := TGocciaStringLiteralValue.Create(StringValue);
  end
  else
  begin
    ReplaceValue := ReplaceArg.ToStringLiteral.Value;
    if SearchValue = '' then
      Result := TGocciaStringLiteralValue.Create(ReplaceValue + StringValue)
    else
      Result := TGocciaStringLiteralValue.Create(StringReplace(StringValue, SearchValue, ReplaceValue, []));
  end;
end;

// ES2026 §22.1.3.20 String.prototype.replaceAll(searchValue, replaceValue)
function TGocciaStringObjectValue.StringReplaceAllMethod(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue, ReplaceValue, ResultStr: string;
  SearchArg: TGocciaValue;
  ReplaceArg: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  CallResult: TGocciaValue;
  SearchPos, Offset: Integer;
  ReplaceMethod: TGocciaValue;
  RegexValue: TGocciaObjectValue;
  MatchArray: TGocciaObjectValue;
  MatchIndex, MatchEnd, NextIndex: Integer;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let string be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let searchString be ToString(searchValue)
  if AArgs.Length > 0 then
    SearchArg := AArgs.GetElement(0)
  else
    SearchArg := TGocciaUndefinedLiteralValue.UndefinedValue;

  // Step 4: Let replaceValue (or functionalReplace if callable)
  if AArgs.Length > 1 then
    ReplaceArg := AArgs.GetElement(1)
  else
    ReplaceArg := TGocciaUndefinedLiteralValue.UndefinedValue;

  if IsRegExpValue(SearchArg) and
     not GetRegExpBooleanProperty(TGocciaObjectValue(SearchArg), PROP_GLOBAL) then
    ThrowTypeError('String.prototype.replaceAll requires a global RegExp');

  ReplaceMethod := GetMethodBySymbol(SearchArg,
    TGocciaSymbolValue.WellKnownReplace);
  if not (ReplaceMethod is TGocciaUndefinedLiteralValue) then
  begin
    if not ReplaceMethod.IsCallable then
      ThrowTypeError('@@replace is not callable');
    CallArgs := TGocciaArgumentsCollection.Create([
      TGocciaStringLiteralValue.Create(StringValue),
      ReplaceArg
    ]);
    try
      Result := InvokeCallable(ReplaceMethod, CallArgs, SearchArg);
    finally
      CallArgs.Free;
    end;
    Exit;
  end;

  if IsRegExpValue(SearchArg) then
  begin
    RegexValue := CoerceRegExpValue(SearchArg);
    TGarbageCollector.Instance.AddTempRoot(RegexValue);
    try
      ResultStr := '';
      Offset := 0;
      while MatchRegExpObjectValue(RegexValue, StringValue, Offset, False,
        False,
        MatchArray, MatchIndex, MatchEnd, NextIndex) do
      begin
        ResultStr := ResultStr + Copy(StringValue, Offset + 1,
          MatchIndex - Offset);
        ResultStr := ResultStr + BuildRegexReplacement(ReplaceArg,
          TGocciaArrayValue(MatchArray), MatchIndex, StringValue);
        Offset := NextIndex;
        if Offset > Length(StringValue) then
          Break;
      end;
      if Offset <= Length(StringValue) then
        ResultStr := ResultStr + Copy(StringValue, Offset + 1, MaxInt);
      Result := TGocciaStringLiteralValue.Create(ResultStr);
    finally
      TGarbageCollector.Instance.RemoveTempRoot(RegexValue);
    end;
    Exit;
  end;

  SearchValue := SearchArg.ToStringLiteral.Value;

  // Step 5: Let searchLength be the length of searchString
  // Step 6: Find all occurrences of searchString in string
  // Step 7: For each occurrence, compute replacement and build result
  if ReplaceArg.IsCallable then
  begin
    ResultStr := '';
    Offset := 1;
    if SearchValue = '' then
    begin
      Result := TGocciaStringLiteralValue.Create(StringValue);
      Exit;
    end;

    SearchPos := Pos(SearchValue, Copy(StringValue, Offset, Length(StringValue)));
    if SearchPos = 0 then
    begin
      Result := TGocciaStringLiteralValue.Create(StringValue);
      Exit;
    end;

    CallArgs := TGocciaArgumentsCollection.Create([nil, nil, TGocciaStringLiteralValue.Create(StringValue)]);
    try
      SearchPos := Pos(SearchValue, Copy(StringValue, Offset, Length(StringValue)));
      while SearchPos > 0 do
      begin
        ResultStr := ResultStr + Copy(StringValue, Offset, SearchPos - 1);
        CallArgs.SetElement(0, TGocciaStringLiteralValue.Create(SearchValue));
        CallArgs.SetElement(1, TGocciaNumberLiteralValue.Create(Offset + SearchPos - 2));
        CallResult := InvokeCallable(ReplaceArg, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
        ResultStr := ResultStr + CallResult.ToStringLiteral.Value;
        Offset := Offset + SearchPos - 1 + Length(SearchValue);
        if Offset > Length(StringValue) then
          Break;
        SearchPos := Pos(SearchValue, Copy(StringValue, Offset, Length(StringValue)));
      end;
      if Offset <= Length(StringValue) then
        ResultStr := ResultStr + Copy(StringValue, Offset, Length(StringValue));
    finally
      CallArgs.Free;
    end;
    Result := TGocciaStringLiteralValue.Create(ResultStr);
  end
  else
  begin
    // Step 7 (non-callable): Let replaceStr be ToString(replaceValue), replace all occurrences
    ReplaceValue := ReplaceArg.ToStringLiteral.Value;
    Result := TGocciaStringLiteralValue.Create(StringReplace(StringValue, SearchValue, ReplaceValue, [rfReplaceAll]));
  end;
end;

// ES2026 §22.1.3.23 String.prototype.split(separator, limit)
function TGocciaStringObjectValue.StringSplit(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, Separator: string;
  SeparatorArg: TGocciaValue;
  ResultArray: TGocciaArrayValue;
  I: Integer;
  RemainingString: string;
  SeparatorPos: Integer;
  Segment: string;
  Limit: Cardinal;
  HasLimit: Boolean;
  SplitMethod: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  RegexValue: TGocciaObjectValue;
  MatchArray: TGocciaObjectValue;
  MatchIndex, MatchEnd, NextIndex: Integer;
  PreviousIndex, SearchIndex: Integer;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let A be ArrayCreate(0)
  // Step 4: Let sep be ToString(separator)
  if AArgs.Length > 0 then
    SeparatorArg := AArgs.GetElement(0)
  else
    SeparatorArg := TGocciaUndefinedLiteralValue.UndefinedValue;

  HasLimit := (AArgs.Length > 1) and
    not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue);
  if HasLimit then
  begin
    Limit := ToUint32Value(AArgs.GetElement(1));
    if Limit = 0 then
    begin
      Result := TGocciaArrayValue.Create;
      Exit;
    end;
  end;

  ResultArray := TGocciaArrayValue.Create;
  TGarbageCollector.Instance.AddTempRoot(ResultArray);
  try
    SplitMethod := GetMethodBySymbol(SeparatorArg,
      TGocciaSymbolValue.WellKnownSplit);
    if not (SplitMethod is TGocciaUndefinedLiteralValue) then
    begin
      if not SplitMethod.IsCallable then
        ThrowTypeError('@@split is not callable');
      if HasLimit then
        CallArgs := TGocciaArgumentsCollection.Create([
          TGocciaStringLiteralValue.Create(StringValue),
          AArgs.GetElement(1)
        ])
      else
        CallArgs := TGocciaArgumentsCollection.Create([
          TGocciaStringLiteralValue.Create(StringValue)
        ]);
      try
        Result := InvokeCallable(SplitMethod, CallArgs, SeparatorArg);
      finally
        CallArgs.Free;
      end;
      Exit;
    end;

    if IsRegExpValue(SeparatorArg) then
    begin
      RegexValue := CoerceRegExpValue(SeparatorArg);
      TGarbageCollector.Instance.AddTempRoot(RegexValue);
      try
        PreviousIndex := 0;
        SearchIndex := 0;
        while MatchRegExpObjectValue(RegexValue, StringValue, SearchIndex,
          False,
          False, MatchArray, MatchIndex, MatchEnd, NextIndex) do
        begin
          ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(
            Copy(StringValue, PreviousIndex + 1, MatchIndex - PreviousIndex)));
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
          if SearchIndex > Length(StringValue) then
            Break;
        end;

        if not HasLimit or (Cardinal(ResultArray.Elements.Count) < Limit) then
          ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(
            Copy(StringValue, PreviousIndex + 1, Length(StringValue) - PreviousIndex)));

        Result := ResultArray;
      finally
        TGarbageCollector.Instance.RemoveTempRoot(RegexValue);
      end;
      Exit;
    end;

    Separator := SeparatorArg.ToStringLiteral.Value;

    // Step 5: If separator is undefined, return [S]
    if StringValue = '' then
    begin
      if Separator <> '' then
        ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(''));
      Result := ResultArray;
      Exit;
    end;

    // Step 6: If sep is "", split each character
    if Separator = '' then
    begin
      for I := 1 to Length(StringValue) do
      begin
        ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(StringValue[I]));
        if HasLimit and (Cardinal(ResultArray.Elements.Count) >= Limit) then
          Break;
      end;
      Result := ResultArray;
      Exit;
    end;

    // Step 7: Split by occurrences of sep, respecting limit
    if Pos(Separator, StringValue) = 0 then
      ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(StringValue))
    else
    begin
      RemainingString := StringValue;
      while True do
      begin
        SeparatorPos := Pos(Separator, RemainingString);
        if SeparatorPos = 0 then
        begin
          ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(RemainingString));
          Break;
        end
        else
        begin
          Segment := Copy(RemainingString, 1, SeparatorPos - 1);
          ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(Segment));

          if HasLimit and (Cardinal(ResultArray.Elements.Count) >= Limit) then
            Break;

          RemainingString := Copy(RemainingString, SeparatorPos + Length(Separator), Length(RemainingString));
        end;
      end;
    end;

    Result := ResultArray;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(ResultArray);
  end;
end;

// ES2026 §22.1.3.15 String.prototype.match(regexp)
function TGocciaStringObjectValue.StringMatch(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  MatchMethod: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  RegexValue: TGocciaObjectValue;
  MatchArray: TGocciaObjectValue;
  ResultArray: TGocciaArrayValue;
  MatchIndex, MatchEnd, NextIndex, SearchIndex: Integer;
begin
  StringValue := ExtractStringValue(AThisValue);
  if AArgs.Length > 0 then
  begin
    MatchMethod := GetMethodBySymbol(AArgs.GetElement(0),
      TGocciaSymbolValue.WellKnownMatch);
    if not (MatchMethod is TGocciaUndefinedLiteralValue) then
    begin
      if not MatchMethod.IsCallable then
        ThrowTypeError('@@match is not callable');
      CallArgs := TGocciaArgumentsCollection.Create([
        TGocciaStringLiteralValue.Create(StringValue)
      ]);
      try
        Result := InvokeCallable(MatchMethod, CallArgs, AArgs.GetElement(0));
      finally
        CallArgs.Free;
      end;
      Exit;
    end;
  end;

  if AArgs.Length > 0 then
    RegexValue := CoerceRegExpValue(AArgs.GetElement(0))
  else
    RegexValue := CoerceRegExpValue(TGocciaUndefinedLiteralValue.UndefinedValue);

  TGarbageCollector.Instance.AddTempRoot(RegexValue);
  try
    if GetRegExpBooleanProperty(RegexValue, PROP_GLOBAL) then
    begin
      ResultArray := TGocciaArrayValue.Create;
      TGarbageCollector.Instance.AddTempRoot(ResultArray);
      try
        SearchIndex := 0;
        while MatchRegExpObjectValue(RegexValue, StringValue, SearchIndex,
          False,
          False, MatchArray, MatchIndex, MatchEnd, NextIndex) do
        begin
          ResultArray.Elements.Add(TGocciaArrayValue(MatchArray).Elements[0]);
          SearchIndex := NextIndex;
          if SearchIndex > Length(StringValue) then
            Break;
        end;
        if ResultArray.Elements.Count = 0 then
          Result := TGocciaNullLiteralValue.NullValue
        else
          Result := ResultArray;
      finally
        TGarbageCollector.Instance.RemoveTempRoot(ResultArray);
      end;
    end
    else if MatchRegExpObjectValue(RegexValue, StringValue, 0, False, False,
      MatchArray, MatchIndex, MatchEnd, NextIndex) then
      Result := MatchArray
    else
      Result := TGocciaNullLiteralValue.NullValue;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(RegexValue);
  end;
end;

// ES2026 §22.1.3.16 String.prototype.matchAll(regexp)
function TGocciaStringObjectValue.StringMatchAll(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  MatchAllMethod: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  RegexValue: TGocciaObjectValue;
  MatchesArray: TGocciaArrayValue;
  MatchArray: TGocciaObjectValue;
  MatchIndex, MatchEnd, NextIndex, SearchIndex: Integer;
begin
  StringValue := ExtractStringValue(AThisValue);
  if (AArgs.Length > 0) and IsRegExpValue(AArgs.GetElement(0)) and
     not GetRegExpBooleanProperty(TGocciaObjectValue(AArgs.GetElement(0)),
       PROP_GLOBAL) then
    ThrowTypeError('String.prototype.matchAll requires a global RegExp');

  if AArgs.Length > 0 then
  begin
    MatchAllMethod := GetMethodBySymbol(AArgs.GetElement(0),
      TGocciaSymbolValue.WellKnownMatchAll);
    if not (MatchAllMethod is TGocciaUndefinedLiteralValue) then
    begin
      if not MatchAllMethod.IsCallable then
        ThrowTypeError('@@matchAll is not callable');
      CallArgs := TGocciaArgumentsCollection.Create([
        TGocciaStringLiteralValue.Create(StringValue)
      ]);
      try
        Result := InvokeCallable(MatchAllMethod, CallArgs, AArgs.GetElement(0));
      finally
        CallArgs.Free;
      end;
      Exit;
    end;
  end;

  if AArgs.Length > 0 then
    RegexValue := CoerceRegExpValue(AArgs.GetElement(0), 'g')
  else
    RegexValue := CoerceRegExpValue(TGocciaUndefinedLiteralValue.UndefinedValue,
      'g');

  TGarbageCollector.Instance.AddTempRoot(RegexValue);
  try
    if not GetRegExpBooleanProperty(RegexValue, PROP_GLOBAL) then
      ThrowTypeError('String.prototype.matchAll requires a global RegExp');

    MatchesArray := TGocciaArrayValue.Create;
    TGarbageCollector.Instance.AddTempRoot(MatchesArray);
    try
      SearchIndex := 0;
      while MatchRegExpObjectValue(RegexValue, StringValue, SearchIndex,
        False,
        False, MatchArray, MatchIndex, MatchEnd, NextIndex) do
      begin
        MatchesArray.Elements.Add(MatchArray);
        SearchIndex := NextIndex;
        if SearchIndex > Length(StringValue) then
          Break;
      end;
      Result := TGocciaArrayIteratorValue.Create(MatchesArray, akValues);
    finally
      TGarbageCollector.Instance.RemoveTempRoot(MatchesArray);
    end;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(RegexValue);
  end;
end;

// ES2026 §22.1.3.27 String.prototype.search(regexp)
function TGocciaStringObjectValue.StringSearch(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  SearchMethod: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  RegexValue: TGocciaObjectValue;
  MatchArray: TGocciaObjectValue;
  MatchIndex, MatchEnd, NextIndex: Integer;
begin
  StringValue := ExtractStringValue(AThisValue);
  if AArgs.Length > 0 then
  begin
    SearchMethod := GetMethodBySymbol(AArgs.GetElement(0),
      TGocciaSymbolValue.WellKnownSearch);
    if not (SearchMethod is TGocciaUndefinedLiteralValue) then
    begin
      if not SearchMethod.IsCallable then
        ThrowTypeError('@@search is not callable');
      CallArgs := TGocciaArgumentsCollection.Create([
        TGocciaStringLiteralValue.Create(StringValue)
      ]);
      try
        Result := InvokeCallable(SearchMethod, CallArgs, AArgs.GetElement(0));
      finally
        CallArgs.Free;
      end;
      Exit;
    end;
  end;

  if AArgs.Length > 0 then
    RegexValue := CoerceRegExpValue(AArgs.GetElement(0))
  else
    RegexValue := CoerceRegExpValue(TGocciaUndefinedLiteralValue.UndefinedValue);

  TGarbageCollector.Instance.AddTempRoot(RegexValue);
  try
    if MatchRegExpObjectValue(RegexValue, StringValue, 0, False, False,
      MatchArray,
      MatchIndex, MatchEnd, NextIndex) then
      Result := TGocciaNumberLiteralValue.Create(MatchIndex)
    else
      Result := TGocciaNumberLiteralValue.Create(-1);
  finally
    TGarbageCollector.Instance.RemoveTempRoot(RegexValue);
  end;
end;

// ES2026 §22.1.3.18 String.prototype.repeat(count)
function TGocciaStringObjectValue.StringRepeat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  CountValue: TGocciaNumberLiteralValue;
  Count: Integer;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let n be ToIntegerOrInfinity(count)
  if AArgs.Length > 0 then
    CountValue := AArgs.GetElement(0).ToNumberLiteral
  else
    CountValue := TGocciaNumberLiteralValue.Create(1);

  if CountValue.IsNaN then
  begin
    // NaN converts to 0 via ToIntegerOrInfinity; repeat 0 times = ""
    Result := TGocciaStringLiteralValue.Create('');
    Exit;
  end;

  // Step 4: If n < 0 or n = +∞, throw a RangeError exception
  if CountValue.IsInfinity or CountValue.IsNegativeInfinity or (CountValue.Value < 0) then
    ThrowRangeError('Invalid count value: ' + CountValue.ToStringLiteral.Value);

  // Step 5: If n = 0, return ""
  // Step 6: Return S repeated n times
  Count := Trunc(CountValue.Value);
  Result := TGocciaStringLiteralValue.Create(DupeString(StringValue, Count));
end;

// ES2026 §22.1.3.15 String.prototype.padStart(maxLength [, fillString])
function TGocciaStringObjectValue.StringPadStart(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, PadString, Padding: string;
  TargetLength, PadNeeded: Integer;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let intMaxLength be ToLength(maxLength)
  TargetLength := ToIntegerFromArgs(AArgs);

  // Step 4: If intMaxLength <= len(S), return S
  if Length(StringValue) >= TargetLength then
  begin
    Result := TGocciaStringLiteralValue.Create(StringValue);
    Exit;
  end;

  // Step 5: If fillString is undefined, let filler be " "; else let filler be ToString(fillString)
  if (AArgs.Length > 1) and not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
    PadString := AArgs.GetElement(1).ToStringLiteral.Value
  else
    PadString := ' ';

  if PadString = '' then
  begin
    Result := TGocciaStringLiteralValue.Create(StringValue);
    Exit;
  end;

  // Step 6: Let fillLen be intMaxLength - len(S)
  // Step 7: Let truncatedStringFiller be filler repeated and truncated to fillLen
  // Step 8: Return truncatedStringFiller + S
  PadNeeded := TargetLength - Length(StringValue);
  Padding := '';
  while Length(Padding) < PadNeeded do
    Padding := Padding + PadString;
  Padding := Copy(Padding, 1, PadNeeded);

  Result := TGocciaStringLiteralValue.Create(Padding + StringValue);
end;

// ES2026 §22.1.3.14 String.prototype.padEnd(maxLength [, fillString])
function TGocciaStringObjectValue.StringPadEnd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, PadString, Padding: string;
  TargetLength, PadNeeded: Integer;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let intMaxLength be ToLength(maxLength)
  TargetLength := ToIntegerFromArgs(AArgs);

  // Step 4: If intMaxLength <= len(S), return S
  if Length(StringValue) >= TargetLength then
  begin
    Result := TGocciaStringLiteralValue.Create(StringValue);
    Exit;
  end;

  // Step 5: If fillString is undefined, let filler be " "; else let filler be ToString(fillString)
  if (AArgs.Length > 1) and not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
    PadString := AArgs.GetElement(1).ToStringLiteral.Value
  else
    PadString := ' ';

  if PadString = '' then
  begin
    Result := TGocciaStringLiteralValue.Create(StringValue);
    Exit;
  end;

  // Step 6: Let fillLen be intMaxLength - len(S)
  // Step 7: Let truncatedStringFiller be filler repeated and truncated to fillLen
  // Step 8: Return S + truncatedStringFiller
  PadNeeded := TargetLength - Length(StringValue);
  Padding := '';
  while Length(Padding) < PadNeeded do
    Padding := Padding + PadString;
  Padding := Copy(Padding, 1, PadNeeded);

  Result := TGocciaStringLiteralValue.Create(StringValue + Padding);
end;

// ES2026 §22.1.3.4 String.prototype.concat(...args)
function TGocciaStringObjectValue.StringConcat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  I: Integer;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: For each element next of args, let R be the string-concatenation of R and ToString(next)
  for I := 0 to AArgs.Length - 1 do
  begin
    if AArgs.GetElement(I) is TGocciaSymbolValue then
      ThrowTypeError('Cannot convert a Symbol value to a string');
    StringValue := StringValue + AArgs.GetElement(I).ToStringLiteral.Value;
  end;

  // Step 4: Return R
  Result := TGocciaStringLiteralValue.Create(StringValue);
end;

// ES2026 §22.1.3.1 String.prototype.at(index)
function TGocciaStringObjectValue.StringAt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  Index: Integer;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let len be the length of S
  // Step 4: Let relativeIndex be ToIntegerOrInfinity(index)
  Index := ToIntegerFromArgs(AArgs);

  // Step 5: If relativeIndex >= 0, let k be relativeIndex; else let k be len + relativeIndex
  if Index < 0 then
    Index := Length(StringValue) + Index;

  // Step 6: If k < 0 or k >= len, return undefined
  if (Index < 0) or (Index >= Length(StringValue)) then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  // Step 7: Return the substring of S from k to k + 1
  Result := TGocciaStringLiteralValue.Create(StringValue[Index + 1]);
end;

// ES2026 §22.1.3.32 String.prototype.valueOf()
function TGocciaStringObjectValue.StringValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Return the [[StringData]] internal slot of this value
  Result := TGocciaStringLiteralValue.Create(ExtractStringValue(AThisValue));
end;

// ES2026 §22.1.3.27 String.prototype.toString()
function TGocciaStringObjectValue.StringToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Return the [[StringData]] internal slot of this value (same as valueOf)
  Result := TGocciaStringLiteralValue.Create(ExtractStringValue(AThisValue));
end;

// ES2026 §22.1.3.34 String.prototype[@@iterator]()
function TGocciaStringObjectValue.StringSymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  // Step 3: Return CreateStringIterator(S)
  Result := TGocciaStringIteratorValue.Create(TGocciaStringLiteralValue.Create(ExtractStringValue(AThisValue)));
end;

// ES2026 §22.1.3.3 String.prototype.codePointAt(pos)
function TGocciaStringObjectValue.StringCodePointAt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  Index, ByteLen: Integer;
  B: Byte;
  CodePoint: Cardinal;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let position be ToIntegerOrInfinity(pos)
  Index := ToIntegerFromArgs(AArgs);

  // Step 4: If position < 0 or position >= len(S), return undefined
  if (Index < 0) or (Index >= Length(StringValue)) then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  // Step 5: Let cp be CodePointAt(S, position) and return cp.[[CodePoint]]
  B := Ord(StringValue[Index + 1]);
  if B < $80 then
    CodePoint := B
  else if (B and $E0) = $C0 then
  begin
    ByteLen := 2;
    if Index + ByteLen > Length(StringValue) then
    begin
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;
    CodePoint := (Cardinal(B and $1F) shl 6) or Cardinal(Ord(StringValue[Index + 2]) and $3F);
  end
  else if (B and $F0) = $E0 then
  begin
    ByteLen := 3;
    if Index + ByteLen > Length(StringValue) then
    begin
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;
    CodePoint := (Cardinal(B and $0F) shl 12) or (Cardinal(Ord(StringValue[Index + 2]) and $3F) shl 6) or Cardinal(Ord(StringValue[Index + 3]) and $3F);
  end
  else if (B and $F8) = $F0 then
  begin
    ByteLen := 4;
    if Index + ByteLen > Length(StringValue) then
    begin
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;
    CodePoint := (Cardinal(B and $07) shl 18) or (Cardinal(Ord(StringValue[Index + 2]) and $3F) shl 12) or (Cardinal(Ord(StringValue[Index + 3]) and $3F) shl 6) or Cardinal(Ord(StringValue[Index + 4]) and $3F);
  end
  else
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  Result := TGocciaNumberLiteralValue.Create(CodePoint * 1.0);
end;

// ES2026 §22.1.3.11 String.prototype.localeCompare(that)
function TGocciaStringObjectValue.StringLocaleCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, ThatString: string;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let That be ToString(that)
  if AArgs.Length = 0 then
    ThatString := 'undefined'
  else
    ThatString := AArgs.GetElement(0).ToStringLiteral.Value;

  // Step 4: Compare S and That, return negative, zero, or positive
  if StringValue < ThatString then
    Result := TGocciaNumberLiteralValue.Create(-1)
  else if StringValue > ThatString then
    Result := TGocciaNumberLiteralValue.Create(1)
  else
    Result := TGocciaNumberLiteralValue.Create(0);
end;

// ES2026 §22.1.3.13 String.prototype.normalize([form])
function TGocciaStringObjectValue.StringNormalize(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, Form: string;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: If form is undefined, let f be "NFC"; else let f be ToString(form)
  if (AArgs.Length > 0) and not (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
    Form := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Form := 'NFC';

  // Step 4: If f is not one of "NFC", "NFD", "NFKC", "NFKD", throw a RangeError
  if (Form <> 'NFC') and (Form <> 'NFD') and (Form <> 'NFKC') and (Form <> 'NFKD') then
    ThrowRangeError('The normalization form should be one of NFC, NFD, NFKC, NFKD');

  // Step 5: Return the Unicode Normalization Form f of S
  Result := TGocciaStringLiteralValue.Create(StringValue);
end;

// ES2026 §22.1.3.8 String.prototype.isWellFormed()
function TGocciaStringObjectValue.StringIsWellFormed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  I: Integer;
  B: Byte;
begin
  // ES2026 §22.1.3.8 step 1: Let O be RequireObjectCoercible(this value)
  if (AThisValue is TGocciaUndefinedLiteralValue) or (AThisValue is TGocciaNullLiteralValue) then
    ThrowTypeError('String.prototype.isWellFormed requires that ''this'' not be null or undefined');
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);
  // Step 3: Return IsStringWellFormedUnicode(S)
  I := 1;
  while I <= Length(StringValue) do
  begin
    B := Ord(StringValue[I]);
    if B < $80 then
      Inc(I)
    else if (B and $E0) = $C0 then
    begin
      if (I + 1 > Length(StringValue)) or ((Ord(StringValue[I + 1]) and $C0) <> $80) then
      begin
        Result := TGocciaBooleanLiteralValue.FalseValue;
        Exit;
      end;
      Inc(I, 2);
    end
    else if (B and $F0) = $E0 then
    begin
      if (I + 2 > Length(StringValue)) or ((Ord(StringValue[I + 1]) and $C0) <> $80) or ((Ord(StringValue[I + 2]) and $C0) <> $80) then
      begin
        Result := TGocciaBooleanLiteralValue.FalseValue;
        Exit;
      end;
      // ES2026 §6.1.4: Surrogate code points (U+D800..U+DFFF) are not well-formed
      if (B = $ED) and (Ord(StringValue[I + 1]) >= $A0) then
      begin
        Result := TGocciaBooleanLiteralValue.FalseValue;
        Exit;
      end;
      Inc(I, 3);
    end
    else if (B and $F8) = $F0 then
    begin
      if (I + 3 > Length(StringValue)) or ((Ord(StringValue[I + 1]) and $C0) <> $80) or ((Ord(StringValue[I + 2]) and $C0) <> $80) or ((Ord(StringValue[I + 3]) and $C0) <> $80) then
      begin
        Result := TGocciaBooleanLiteralValue.FalseValue;
        Exit;
      end;
      Inc(I, 4);
    end
    else
    begin
      Result := TGocciaBooleanLiteralValue.FalseValue;
      Exit;
    end;
  end;
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

// ES2026 §22.1.3.33 String.prototype.toWellFormed()
function TGocciaStringObjectValue.StringToWellFormed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, ResultStr: string;
  I: Integer;
  B: Byte;
begin
  // ES2026 §22.1.3.33 step 1: Let O be RequireObjectCoercible(this value)
  if (AThisValue is TGocciaUndefinedLiteralValue) or (AThisValue is TGocciaNullLiteralValue) then
    ThrowTypeError('String.prototype.toWellFormed requires that ''this'' not be null or undefined');
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);
  // Step 3: Let result be ""
  ResultStr := '';
  // Step 4: For each code unit in S, if it is a lone surrogate replace with U+FFFD, else append
  I := 1;
  while I <= Length(StringValue) do
  begin
    B := Ord(StringValue[I]);
    if B < $80 then
    begin
      ResultStr := ResultStr + StringValue[I];
      Inc(I);
    end
    else if (B and $E0) = $C0 then
    begin
      if (I + 1 > Length(StringValue)) or ((Ord(StringValue[I + 1]) and $C0) <> $80) then
      begin
        ResultStr := ResultStr + #$EF#$BF#$BD;
        Inc(I);
      end
      else
      begin
        ResultStr := ResultStr + Copy(StringValue, I, 2);
        Inc(I, 2);
      end;
    end
    else if (B and $F0) = $E0 then
    begin
      if (I + 2 > Length(StringValue)) or ((Ord(StringValue[I + 1]) and $C0) <> $80) or ((Ord(StringValue[I + 2]) and $C0) <> $80) then
      begin
        ResultStr := ResultStr + #$EF#$BF#$BD;
        Inc(I);
      end
      else if (B = $ED) and (Ord(StringValue[I + 1]) >= $A0) then
      begin
        // ES2026 §6.1.4: Replace surrogate code points (U+D800..U+DFFF) with U+FFFD
        ResultStr := ResultStr + #$EF#$BF#$BD;
        Inc(I, 3);
      end
      else
      begin
        ResultStr := ResultStr + Copy(StringValue, I, 3);
        Inc(I, 3);
      end;
    end
    else if (B and $F8) = $F0 then
    begin
      if (I + 3 > Length(StringValue)) or ((Ord(StringValue[I + 1]) and $C0) <> $80) or ((Ord(StringValue[I + 2]) and $C0) <> $80) or ((Ord(StringValue[I + 3]) and $C0) <> $80) then
      begin
        ResultStr := ResultStr + #$EF#$BF#$BD;
        Inc(I);
      end
      else
      begin
        ResultStr := ResultStr + Copy(StringValue, I, 4);
        Inc(I, 4);
      end;
    end
    else
    begin
      ResultStr := ResultStr + #$EF#$BF#$BD;
      Inc(I);
    end;
  end;
  // Step 5: Return result
  Result := TGocciaStringLiteralValue.Create(ResultStr);
end;

end.
