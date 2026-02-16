unit Goccia.Values.StringObjectValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives,
  Goccia.Values.ObjectValue,
  Goccia.Values.FunctionValue,
  Goccia.Values.NativeFunction,
  Goccia.Arguments.Collection,
  SysUtils,
  StrUtils,
  Math,
  Generics.Collections;

type
  TGocciaStringObjectValue = class(TGocciaObjectValue)
  private
    FPrimitive: TGocciaStringLiteralValue;

    class var FSharedStringPrototype: TGocciaObjectValue;
    class var FPrototypeMethodHost: TGocciaStringObjectValue;

    function ExtractStringValue(Value: TGocciaValue): string;
  public
    constructor Create(APrimitive: TGocciaStringLiteralValue);
    destructor Destroy; override;
    function TypeName: string; override;
    function GetProperty(const AName: string): TGocciaValue; override;

    procedure InitializePrototype;
    procedure GCMarkReferences; override;
    property Primitive: TGocciaStringLiteralValue read FPrimitive;

    // String prototype methods
    function StringLength(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function StringCharAt(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function StringCharCodeAt(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function StringToUpperCase(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function StringToLowerCase(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function StringSlice(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function StringSubstring(Args: TGocciaArgumentsCollection ; ThisValue: TGocciaValue): TGocciaValue;
    function StringIndexOf(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function StringLastIndexOf(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function StringIncludes(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function StringStartsWith(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function StringEndsWith(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function StringTrim(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function StringTrimStart(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function StringTrimEnd(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function StringReplaceMethod(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function StringReplaceAllMethod(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function StringSplit(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function StringRepeat(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function StringPadStart(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function StringPadEnd(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function StringConcat(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function StringAt(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
  end;


implementation

uses
  Goccia.Values.ClassHelper,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.GarbageCollector;

{ TGocciaStringObjectValue }

function TGocciaStringObjectValue.ExtractStringValue(Value: TGocciaValue): string;
begin
  if Value is TGocciaStringLiteralValue then
    Result := TGocciaStringLiteralValue(Value).Value
  else if Value is TGocciaStringObjectValue then
    Result := TGocciaStringObjectValue(Value).Primitive.Value
  else
    Result := Value.ToStringLiteral.Value;
end;

constructor TGocciaStringObjectValue.Create(APrimitive: TGocciaStringLiteralValue);
begin
  inherited Create;
  FPrimitive := APrimitive;

  InitializePrototype;

  if Assigned(FSharedStringPrototype) then
    Self.Prototype := FSharedStringPrototype;
end;

destructor TGocciaStringObjectValue.Destroy;
begin
  // Don't free FPrimitiveValue - it's GC-managed
  inherited Destroy;
end;

procedure TGocciaStringObjectValue.GCMarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FPrimitive) then
    FPrimitive.GCMarkReferences;
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

  // Handle numeric index access: str[0], str[1], etc.
  if TryStrToInt(AName, Index) then
  begin
    if (Index >= 0) and (Index < Length(StringValue)) then
      Result := TGocciaStringLiteralValue.Create(StringValue[Index + 1])
    else
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  // Look up in shared prototype with this object as context
  if Assigned(FSharedStringPrototype) then
    Result := FSharedStringPrototype.GetPropertyWithContext(AName, Self)
  else if Assigned(FPrototype) then
    Result := FPrototype.GetPropertyWithContext(AName, Self)
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaStringObjectValue.InitializePrototype;
begin
  if Assigned(FSharedStringPrototype) then Exit;

  FSharedStringPrototype := TGocciaObjectValue.Create;
  FPrototypeMethodHost := Self;

  FSharedStringPrototype.DefineProperty('length', TGocciaPropertyDescriptorAccessor.Create(TGocciaNativeFunctionValue.Create(StringLength, 'length', 0), nil, []));

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

  if Assigned(TGocciaGC.Instance) then
  begin
    TGocciaGC.Instance.PinValue(FSharedStringPrototype);
    TGocciaGC.Instance.PinValue(FPrototypeMethodHost);
  end;
end;

{ TGocciaStringObjectValue }

function TGocciaStringObjectValue.StringLength(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Get the string value - handle both primitives and boxed objects
  StringValue := ExtractStringValue(ThisValue);
  Result := TGocciaNumberLiteralValue.Create(Length(StringValue));
end;

function TGocciaStringObjectValue.StringCharAt(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  Index: Integer;
  NumberValue: Double;
  TempNumberValue: TGocciaNumberLiteralValue;
  Arg: TGocciaValue;
begin
  // Get the string value
  StringValue := ExtractStringValue(ThisValue);

  // Get the index argument with safe conversion
  if Args.Length > 0 then
  begin
    try
      Arg := Args.GetElement(0);
      
      // Handle special values according to ECMAScript spec:
      // - undefined/null convert to 0
      // - NaN converts to 0
      // - Infinity/-Infinity are treated as out-of-bounds (use very large number)
      if (Arg is TGocciaUndefinedLiteralValue) or (Arg is TGocciaNullLiteralValue) then
        Index := 0
      else if (Arg is TGocciaNumberLiteralValue) then
      begin
        if TGocciaNumberLiteralValue(Arg).IsNaN then
          Index := 0
        else if TGocciaNumberLiteralValue(Arg).IsInfinity or TGocciaNumberLiteralValue(Arg).IsNegativeInfinity then
          Index := MaxInt // Force out-of-bounds
        else
          Index := Trunc(TGocciaNumberLiteralValue(Arg).Value);
      end
      else
      begin
        NumberValue := Arg.ToNumberLiteral.Value;
        Index := Trunc(NumberValue);
      end;
    except
      Index := 0; // Default to 0 if conversion fails
    end;
  end
  else
    Index := 0;

  // Return character at index or empty string
  if (Index >= 0) and (Index < Length(StringValue)) then
    Result := TGocciaStringLiteralValue.Create(StringValue[Index + 1]) // Pascal is 1-indexed
  else
    Result := TGocciaStringLiteralValue.Create('');
end;

function TGocciaStringObjectValue.StringCharCodeAt(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  Index: Integer;
  NumberValue: Double;
  TempNumberValue: TGocciaNumberLiteralValue;
  Arg: TGocciaValue;
begin
  // Get the string value
  StringValue := ExtractStringValue(ThisValue);

  // Get the index argument with safe conversion
  if Args.Length > 0 then
  begin
    try
      Arg := Args.GetElement(0);
      
      // Handle special values according to ECMAScript spec:
      // - undefined/null convert to 0
      // - NaN converts to 0
      // - Infinity/-Infinity are treated as out-of-bounds (use very large number)
      if (Arg is TGocciaUndefinedLiteralValue) or (Arg is TGocciaNullLiteralValue) then
        Index := 0
      else if (Arg is TGocciaNumberLiteralValue) then
      begin
        if TGocciaNumberLiteralValue(Arg).IsNaN then
          Index := 0
        else if TGocciaNumberLiteralValue(Arg).IsInfinity or TGocciaNumberLiteralValue(Arg).IsNegativeInfinity then
          Index := MaxInt // Force out-of-bounds
        else
          Index := Trunc(TGocciaNumberLiteralValue(Arg).Value);
      end
      else
      begin
        NumberValue := Arg.ToNumberLiteral.Value;
        Index := Trunc(NumberValue);
      end;
    except
      Index := 0; // Default to 0 if conversion fails
    end;
  end
  else
    Index := 0;

  // Return character code at index or NaN
  if (Index >= 0) and (Index < Length(StringValue)) then
    Result := TGocciaNumberLiteralValue.Create(Ord(StringValue[Index + 1])) // Pascal is 1-indexed
  else
    Result := TGocciaNumberLiteralValue.NaNValue;
end;

function TGocciaStringObjectValue.StringToUpperCase(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Get the string value
  StringValue := ExtractStringValue(ThisValue);

  Result := TGocciaStringLiteralValue.Create(UpperCase(StringValue));
end;

function TGocciaStringObjectValue.StringToLowerCase(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Get the string value
  StringValue := ExtractStringValue(ThisValue);

  Result := TGocciaStringLiteralValue.Create(LowerCase(StringValue));
end;

function TGocciaStringObjectValue.StringSlice(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  StartIndex, EndIndex: Integer;
  Len: Integer;
begin
  // Get the string value
  StringValue := ExtractStringValue(ThisValue);

  Len := Length(StringValue);

  // Get start index
  if Args.Length > 0 then
    StartIndex := Trunc(Args.GetElement(0).ToNumberLiteral.Value)
  else
    StartIndex := 0;

  // Handle negative start index
  if StartIndex < 0 then
    StartIndex := Max(0, Len + StartIndex);

  // Get end index
  if Args.Length > 1 then
    EndIndex := Trunc(Args.GetElement(1).ToNumberLiteral.Value)
  else
    EndIndex := Len;

  // Handle negative end index
  if EndIndex < 0 then
    EndIndex := Max(0, Len + EndIndex);

  // Ensure start <= end
  if StartIndex > EndIndex then
    StartIndex := EndIndex;

  // Extract substring (Pascal is 1-indexed)
  if (StartIndex >= 0) and (StartIndex < Len) and (EndIndex > StartIndex) then
    Result := TGocciaStringLiteralValue.Create(Copy(StringValue, StartIndex + 1, EndIndex - StartIndex))
  else
    Result := TGocciaStringLiteralValue.Create('');
end;

function TGocciaStringObjectValue.StringSubstring(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  StartIndex, EndIndex: Integer;
  Len: Integer;
  Temp: Integer;
begin
  // Get the string value
  StringValue := ExtractStringValue(ThisValue);

  Len := Length(StringValue);

  // Get start index
  if Args.Length > 0 then
    StartIndex := Trunc(Args.GetElement(0).ToNumberLiteral.Value)
  else
    StartIndex := 0;

  // Get end index
  if Args.Length > 1 then
    EndIndex := Trunc(Args.GetElement(1).ToNumberLiteral.Value)
  else
    EndIndex := Len;

  // Clamp to valid range
  StartIndex := Max(0, Min(StartIndex, Len));
  EndIndex := Max(0, Min(EndIndex, Len));

  // Swap if start > end (substring behavior)
  if StartIndex > EndIndex then
  begin
    Temp := StartIndex;
    StartIndex := EndIndex;
    EndIndex := Temp;
  end;

  // Extract substring (Pascal is 1-indexed)
  if EndIndex > StartIndex then
    Result := TGocciaStringLiteralValue.Create(Copy(StringValue, StartIndex + 1, EndIndex - StartIndex))
  else
    Result := TGocciaStringLiteralValue.Create('');
end;

function TGocciaStringObjectValue.StringIndexOf(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  StartPosition: Integer;
  FoundIndex: Integer;
begin
  // Get the string value
  StringValue := ExtractStringValue(ThisValue);

  // Get search string
  if Args.Length > 0 then
    SearchValue := Args.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  // Get start position
  if Args.Length > 1 then
    StartPosition := Trunc(Args.GetElement(1).ToNumberLiteral.Value)
  else
    StartPosition := 0;

  // Clamp start position
  StartPosition := Max(0, StartPosition);

  // Search for the substring (Pascal is 1-indexed)
  if StartPosition < Length(StringValue) then
  begin
    FoundIndex := Pos(SearchValue, Copy(StringValue, StartPosition + 1, Length(StringValue)));
    if FoundIndex > 0 then
      Result := TGocciaNumberLiteralValue.Create(FoundIndex + StartPosition - 1) // Convert back to 0-indexed
    else
      Result := TGocciaNumberLiteralValue.Create(-1);
  end
  else
    Result := TGocciaNumberLiteralValue.Create(-1);
end;

function TGocciaStringObjectValue.StringLastIndexOf(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  StartPosition: Integer;
  FoundIndex: Integer;
  I: Integer;
begin
  // Get the string value
  StringValue := ExtractStringValue(ThisValue);

  // Get search string
  if Args.Length > 0 then
    SearchValue := Args.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  // Get start position
  if Args.Length > 1 then
    StartPosition := Trunc(Args.GetElement(1).ToNumberLiteral.Value)
  else
    StartPosition := Length(StringValue) - 1; // 0-indexed

  // Clamp start position
  StartPosition := Min(StartPosition, Length(StringValue) - 1);

  // Search backwards for the substring
  FoundIndex := -1;
  if (SearchValue <> '') and (StartPosition >= 0) then
  begin
    for I := StartPosition downto 0 do
    begin
      if (I + Length(SearchValue) <= Length(StringValue)) and
         (Copy(StringValue, I + 1, Length(SearchValue)) = SearchValue) then
      begin
        FoundIndex := I;
        Break;
      end;
    end;
  end
  else if SearchValue = '' then
  begin
    // Empty string can be found at any position
    FoundIndex := Min(StartPosition, Length(StringValue));
  end;

  Result := TGocciaNumberLiteralValue.Create(FoundIndex);
end;

function TGocciaStringObjectValue.StringIncludes(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  StartPosition: Integer;
  FoundIndex: Integer;
begin
  // Get the string value
  StringValue := ExtractStringValue(ThisValue);

  // Get search string
  if Args.Length > 0 then
    SearchValue := Args.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  // Get start position
  if Args.Length > 1 then
    StartPosition := Trunc(Args.GetElement(1).ToNumberLiteral.Value)
  else
    StartPosition := 0;

  // Clamp start position
  StartPosition := Max(0, StartPosition);

  // Handle edge cases
  if StartPosition >= Length(StringValue) then
  begin
    // If start position is beyond string length, only empty string can be found
    Result := TGocciaBooleanLiteralValue.Create(SearchValue = '');
    Exit;
  end;

  // Search for the substring (Pascal is 1-indexed)
  FoundIndex := Pos(SearchValue, Copy(StringValue, StartPosition + 1, Length(StringValue)));
  Result := TGocciaBooleanLiteralValue.Create(FoundIndex > 0);
end;

function TGocciaStringObjectValue.StringStartsWith(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  StartPosition: Integer;
  FoundIndex: Integer;
begin
  // Get the string value
  StringValue := ExtractStringValue(ThisValue);

  // Get search string
  if Args.Length > 0 then
    SearchValue := Args.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  // Check if the string starts with the search string
  if Length(StringValue) >= Length(SearchValue) then
    Result := TGocciaBooleanLiteralValue.Create(Copy(StringValue, 1, Length(SearchValue)) = SearchValue)
  else
    Result := TGocciaBooleanLiteralValue.Create(False);
end;

function TGocciaStringObjectValue.StringEndsWith(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  StartPosition: Integer;
  FoundIndex: Integer;
begin
  // Get the string value
  StringValue := ExtractStringValue(ThisValue);

  // Get search string
  if Args.Length > 0 then
    SearchValue := Args.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  // Check if the string ends with the search string
  if Length(StringValue) >= Length(SearchValue) then
    Result := TGocciaBooleanLiteralValue.Create(Copy(StringValue, Length(StringValue) - Length(SearchValue) + 1, Length(SearchValue)) = SearchValue)
  else
    Result := TGocciaBooleanLiteralValue.Create(False);
end;

function TGocciaStringObjectValue.StringTrim(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Get the string value
  StringValue := ExtractStringValue(ThisValue);

  // Trim the string
  Result := TGocciaStringLiteralValue.Create(Trim(StringValue));
end;

function TGocciaStringObjectValue.StringTrimStart(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Get the string value
  StringValue := ExtractStringValue(ThisValue);

  // Trim the string
  Result := TGocciaStringLiteralValue.Create(TrimLeft(StringValue));
end;

function TGocciaStringObjectValue.StringTrimEnd(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Get the string value
  StringValue := ExtractStringValue(ThisValue);

  // Trim the string
  Result := TGocciaStringLiteralValue.Create(TrimRight(StringValue));
end;

function TGocciaStringObjectValue.StringReplaceMethod(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue, ReplaceValue: string;
begin
  // Get the string value
  StringValue := ExtractStringValue(ThisValue);

  // Get search string
  if Args.Length > 0 then
    SearchValue := Args.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  // Get replace string
  if Args.Length > 1 then
    ReplaceValue := Args.GetElement(1).ToStringLiteral.Value
  else
    ReplaceValue := 'undefined';

  // Replace the first occurrence only (ECMAScript spec)
  Result := TGocciaStringLiteralValue.Create(StringReplace(StringValue, SearchValue, ReplaceValue, []));
end;

function TGocciaStringObjectValue.StringReplaceAllMethod(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue, ReplaceValue: string;
begin
  StringValue := ExtractStringValue(ThisValue);

  if Args.Length > 0 then
    SearchValue := Args.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  if Args.Length > 1 then
    ReplaceValue := Args.GetElement(1).ToStringLiteral.Value
  else
    ReplaceValue := 'undefined';

  // Replace all occurrences
  Result := TGocciaStringLiteralValue.Create(StringReplace(StringValue, SearchValue, ReplaceValue, [rfReplaceAll]));
end;

function TGocciaStringObjectValue.StringSplit(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
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
  // Get the string value
  StringValue := ExtractStringValue(ThisValue);

  // Get separator
  if Args.Length > 0 then
    Separator := Args.GetElement(0).ToStringLiteral.Value
  else
    Separator := 'undefined';

  // Get limit parameter (ECMAScript requirement)
  HasLimit := Args.Length > 1;
  if HasLimit then
  begin
    Limit := Trunc(Args.GetElement(1).ToNumberLiteral.Value);
    // If limit is 0, return empty array
    if Limit = 0 then
    begin
      Result := TGocciaArrayValue.Create;
      Exit;
    end;
    // Negative limits are treated as no limit
    if Limit < 0 then
      HasLimit := False;
  end;

  ResultArray := TGocciaArrayValue.Create;

  if StringValue = '' then
  begin
    if Separator = '' then
    begin
      // "".split("") should return []
      Result := ResultArray;
      Exit;
    end
    else
    begin
      // "".split("anything") should return [""]
      ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(''));
      Result := ResultArray;
      Exit;
    end;
  end;

  if Separator = '' then
  begin
    // Split into individual characters
    for I := 1 to Length(StringValue) do
    begin
      ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(StringValue[I]));
      // Check limit
      if HasLimit and (ResultArray.Elements.Count >= Limit) then
        Break;
    end;
    Result := ResultArray;
    Exit;
  end;

  // Split the string using simple string replacement approach
  if Pos(Separator, StringValue) = 0 then
  begin
    // Separator not found, return array with the original string
    ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(StringValue));
  end
  else
  begin
    // Manual split implementation using standard Pascal functions
    RemainingString := StringValue;
    while True do
    begin
      SeparatorPos := Pos(Separator, RemainingString);
      if SeparatorPos = 0 then
      begin
        // No more separators, add the remaining string
        ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(RemainingString));
        Break;
      end
      else
      begin
        // Found separator, add segment before it
        Segment := Copy(RemainingString, 1, SeparatorPos - 1);
        ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(Segment));

        // Check limit before continuing
        if HasLimit and (ResultArray.Elements.Count >= Limit) then
          Break;

        // Remove processed part including separator
        RemainingString := Copy(RemainingString, SeparatorPos + Length(Separator), Length(RemainingString));
      end;
    end;
  end;

  Result := ResultArray;
end;

function TGocciaStringObjectValue.StringRepeat(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  Count: Integer;
begin
  // Get the string value
  StringValue := ExtractStringValue(ThisValue);

  // Get count
  if Args.Length > 0 then
    Count := Trunc(Args.GetElement(0).ToNumberLiteral.Value)
  else
    Count := 1;

  // Repeat the string
  Result := TGocciaStringLiteralValue.Create(DupeString(StringValue, Count));
end;

function TGocciaStringObjectValue.StringPadStart(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, PadString, Padding: string;
  TargetLength, PadNeeded: Integer;
begin
  StringValue := ExtractStringValue(ThisValue);

  if Args.Length > 0 then
    TargetLength := Trunc(Args.GetElement(0).ToNumberLiteral.Value)
  else
    TargetLength := 0;

  if Length(StringValue) >= TargetLength then
  begin
    Result := TGocciaStringLiteralValue.Create(StringValue);
    Exit;
  end;

  if (Args.Length > 1) and not (Args.GetElement(1) is TGocciaUndefinedLiteralValue) then
    PadString := Args.GetElement(1).ToStringLiteral.Value
  else
    PadString := ' ';

  if PadString = '' then
  begin
    Result := TGocciaStringLiteralValue.Create(StringValue);
    Exit;
  end;

  PadNeeded := TargetLength - Length(StringValue);
  Padding := '';
  while Length(Padding) < PadNeeded do
    Padding := Padding + PadString;
  Padding := Copy(Padding, 1, PadNeeded);

  Result := TGocciaStringLiteralValue.Create(Padding + StringValue);
end;

function TGocciaStringObjectValue.StringPadEnd(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, PadString, Padding: string;
  TargetLength, PadNeeded: Integer;
begin
  StringValue := ExtractStringValue(ThisValue);

  if Args.Length > 0 then
    TargetLength := Trunc(Args.GetElement(0).ToNumberLiteral.Value)
  else
    TargetLength := 0;

  if Length(StringValue) >= TargetLength then
  begin
    Result := TGocciaStringLiteralValue.Create(StringValue);
    Exit;
  end;

  if (Args.Length > 1) and not (Args.GetElement(1) is TGocciaUndefinedLiteralValue) then
    PadString := Args.GetElement(1).ToStringLiteral.Value
  else
    PadString := ' ';

  if PadString = '' then
  begin
    Result := TGocciaStringLiteralValue.Create(StringValue);
    Exit;
  end;

  PadNeeded := TargetLength - Length(StringValue);
  Padding := '';
  while Length(Padding) < PadNeeded do
    Padding := Padding + PadString;
  Padding := Copy(Padding, 1, PadNeeded);

  Result := TGocciaStringLiteralValue.Create(StringValue + Padding);
end;

function TGocciaStringObjectValue.StringConcat(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  I: Integer;
begin
  StringValue := ExtractStringValue(ThisValue);

  for I := 0 to Args.Length - 1 do
    StringValue := StringValue + Args.GetElement(I).ToStringLiteral.Value;

  Result := TGocciaStringLiteralValue.Create(StringValue);
end;

function TGocciaStringObjectValue.StringAt(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  Index: Integer;
begin
  StringValue := ExtractStringValue(ThisValue);

  if Args.Length > 0 then
    Index := Trunc(Args.GetElement(0).ToNumberLiteral.Value)
  else
    Index := 0;

  // Support negative indices
  if Index < 0 then
    Index := Length(StringValue) + Index;

  if (Index < 0) or (Index >= Length(StringValue)) then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  Result := TGocciaStringLiteralValue.Create(StringValue[Index + 1]); // Pascal is 1-based
end;

end.
