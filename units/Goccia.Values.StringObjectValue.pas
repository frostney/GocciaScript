unit Goccia.Values.StringObjectValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaStringObjectValue = class(TGocciaInstanceValue)
  private
    FPrimitive: TGocciaStringLiteralValue;

    class var FSharedStringPrototype: TGocciaObjectValue;
    class var FPrototypeMethodHost: TGocciaStringObjectValue;

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

  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

{ TGocciaStringObjectValue }

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
begin
  if Assigned(FSharedStringPrototype) then Exit;

  FSharedStringPrototype := TGocciaObjectValue.Create;
  FPrototypeMethodHost := Self;

  FSharedStringPrototype.DefineProperty(PROP_LENGTH, TGocciaPropertyDescriptorAccessor.Create(TGocciaNativeFunctionValue.Create(StringLength, PROP_LENGTH, 0), nil, []));

  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringCharAt, 'charAt', 1));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringCharCodeAt, 'charCodeAt', 1));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringToUpperCase, 'toUpperCase', 0));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringToLowerCase, 'toLowerCase', 0));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringSlice, 'slice', 2));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringSubstring, 'substring', 2));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringIndexOf, 'indexOf', 1));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringLastIndexOf, 'lastIndexOf', 1));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringIncludes, 'includes', 1));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringStartsWith, 'startsWith', 1));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringEndsWith, 'endsWith', 1));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringTrim, 'trim', 0));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringTrimStart, 'trimStart', 0));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringTrimEnd, 'trimEnd', 0));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringReplaceMethod, 'replace', 2));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringReplaceAllMethod, 'replaceAll', 2));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringSplit, 'split', 1));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringRepeat, 'repeat', 1));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringPadStart, 'padStart', 1));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringPadEnd, 'padEnd', 1));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringConcat, 'concat', 1));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringAt, 'at', 1));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringValueOf, PROP_VALUE_OF, 0));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringToString, PROP_TO_STRING, 0));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringCodePointAt, 'codePointAt', 1));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringLocaleCompare, 'localeCompare', 1));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringNormalize, 'normalize', 0));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringIsWellFormed, 'isWellFormed', 0));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringToWellFormed, 'toWellFormed', 0));

  FSharedStringPrototype.DefineSymbolProperty(
    TGocciaSymbolValue.WellKnownIterator,
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.Create(StringSymbolIterator, '[Symbol.iterator]', 0),
      [pfConfigurable, pfWritable]
    )
  );

  if Assigned(TGocciaGarbageCollector.Instance) then
  begin
    TGocciaGarbageCollector.Instance.PinValue(FSharedStringPrototype);
    TGocciaGarbageCollector.Instance.PinValue(FPrototypeMethodHost);
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
    Result := TGocciaNumberLiteralValue.SmallInt(Ord(StringValue[Index + 1]))
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

// ES2026 §22.1.3.19 String.prototype.replace(searchValue, replaceValue)
function TGocciaStringObjectValue.StringReplaceMethod(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue, ReplaceValue: string;
  ReplaceArg: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  FoundPos: Integer;
  CallResult: TGocciaValue;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let string be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let searchString be ToString(searchValue)
  if AArgs.Length > 0 then
    SearchValue := AArgs.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  // Step 4: Let replaceValue (or functionalReplace if callable)
  if AArgs.Length > 1 then
    ReplaceArg := AArgs.GetElement(1)
  else
    ReplaceArg := TGocciaUndefinedLiteralValue.UndefinedValue;

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
  ReplaceArg: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  CallResult: TGocciaValue;
  SearchPos, Offset: Integer;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let string be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let searchString be ToString(searchValue)
  if AArgs.Length > 0 then
    SearchValue := AArgs.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  // Step 4: Let replaceValue (or functionalReplace if callable)
  if AArgs.Length > 1 then
    ReplaceArg := AArgs.GetElement(1)
  else
    ReplaceArg := TGocciaUndefinedLiteralValue.UndefinedValue;

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
  ResultArray: TGocciaArrayValue;
  I: Integer;
  RemainingString: string;
  SeparatorPos: Integer;
  Segment: string;
  Limit: Integer;
  HasLimit: Boolean;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let A be ArrayCreate(0)
  // Step 4: Let sep be ToString(separator)
  if AArgs.Length > 0 then
    Separator := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Separator := 'undefined';

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

  ResultArray := TGocciaArrayValue.Create;
  TGocciaGarbageCollector.Instance.AddTempRoot(ResultArray);
  try
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
        if HasLimit and (ResultArray.Elements.Count >= Limit) then
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

          if HasLimit and (ResultArray.Elements.Count >= Limit) then
            Break;

          RemainingString := Copy(RemainingString, SeparatorPos + Length(Separator), Length(RemainingString));
        end;
      end;
    end;

    Result := ResultArray;
  finally
    TGocciaGarbageCollector.Instance.RemoveTempRoot(ResultArray);
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
    Result := TGocciaNumberLiteralValue.SmallInt(-1)
  else if StringValue > ThatString then
    Result := TGocciaNumberLiteralValue.SmallInt(1)
  else
    Result := TGocciaNumberLiteralValue.SmallInt(0);
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
