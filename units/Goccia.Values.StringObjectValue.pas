unit Goccia.Values.StringObjectValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Core,
  Goccia.Values.ObjectValue,
  Goccia.Values.FunctionValue,
  Goccia.Values.NativeFunction,
  SysUtils,
  StrUtils,
  Math,
  Generics.Collections;

type
  TGocciaStringObjectValue = class(TGocciaObjectValue)
  private
    FPrimitiveValue: TGocciaStringValue;
    FStringPrototype: TGocciaObjectValue;
  public
    constructor Create(APrimitiveValue: TGocciaStringValue);
    destructor Destroy; override;
    function ToString: string; override;
    function TypeName: string; override;
    function GetProperty(const AName: string): TGocciaValue; override;

    procedure InitializePrototype;
    property StringPrototype: TGocciaObjectValue read FStringPrototype write FStringPrototype;
    property PrimitiveValue: TGocciaStringValue read FPrimitiveValue;

    // String prototype methods
    function StringLength(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function StringCharAt(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function StringCharCodeAt(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function StringToUpperCase(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function StringToLowerCase(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function StringSlice(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function StringSubstring(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function StringIndexOf(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function StringLastIndexOf(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function StringIncludes(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function StringStartsWith(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function StringEndsWith(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function StringTrim(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function StringTrimStart(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function StringTrimEnd(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function StringReplaceMethod(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function StringSplit(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function StringRepeat(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
  end;





// Boxing/unboxing utility functions
function BoxPrimitive(Value: TGocciaValue): TGocciaObjectValue;
function UnboxIfPrimitive(Value: TGocciaValue): TGocciaValue;
function IsPrimitive(Value: TGocciaValue): Boolean;

implementation

uses
  Goccia.Values.BooleanValue,
  Goccia.Values.ArrayValue,
  Goccia.Values.NullValue,
  Goccia.Values.ObjectPropertyDescriptor;

{ TGocciaStringObjectValue }

constructor TGocciaStringObjectValue.Create(APrimitiveValue: TGocciaStringValue);
begin
  inherited Create;
  FPrimitiveValue := APrimitiveValue;

// TODO: We are not using a shared prototype but re-creating this for every instance
  InitializePrototype;

  // Set prototype to shared String.prototype
  if Assigned(FStringPrototype) then
    Self.Prototype := FStringPrototype;
end;

destructor TGocciaStringObjectValue.Destroy;
begin
  // Don't free FPrimitiveValue - it might be referenced elsewhere
  inherited Destroy;
end;

function TGocciaStringObjectValue.ToString: string;
begin
  Result := FPrimitiveValue.ToString;
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
  StringValue := FPrimitiveValue.ToString;

  // Handle numeric index access: str[0], str[1], etc.
  if TryStrToInt(AName, Index) then
  begin
    if (Index >= 0) and (Index < Length(StringValue)) then
      Result := TGocciaStringValue.Create(StringValue[Index + 1]) // Pascal is 1-indexed
    else
      Result := TGocciaUndefinedValue.Create;
    Exit;
  end;

  // Look up in prototype chain
  Result := inherited GetProperty(AName);
end;

procedure TGocciaStringObjectValue.InitializePrototype;
begin
  if not Assigned(FStringPrototype) then
  begin
    FStringPrototype := TGocciaObjectValue.Create;

    FStringPrototype.DefineProperty('length', TGocciaPropertyDescriptorAccessor.Create(TGocciaNativeFunctionValue.Create(StringLength, 'length', 0), nil, []));

    // String prototype methods: writable, non-enumerable, configurable
    FStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringCharAt, 'charAt', 1));
    FStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringCharCodeAt, 'charCodeAt', 1));
    FStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringToUpperCase, 'toUpperCase', 0));
    FStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringToLowerCase, 'toLowerCase', 0));
    FStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringSlice, 'slice', 2));
    FStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringSubstring, 'substring', 2));
    FStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringIndexOf, 'indexOf', 1));
    FStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringLastIndexOf, 'lastIndexOf', 1));
    FStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringIncludes, 'includes', 1));
    FStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringStartsWith, 'startsWith', 1));
    FStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringEndsWith, 'endsWith', 1));
    FStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringTrim, 'trim', 0));
    FStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringTrimStart, 'trimStart', 0));
    FStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringTrimEnd, 'trimEnd', 0));
    FStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringReplaceMethod, 'replace', 2));
    FStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringSplit, 'split', 1));
    FStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringRepeat, 'repeat', 1));
  end;
end;



{ Utility Functions }

function BoxPrimitive(Value: TGocciaValue): TGocciaObjectValue;
begin
  if Value is TGocciaStringValue then
    Result := TGocciaStringObjectValue.Create(Value as TGocciaStringValue)
  // TODO: Add other primitive types when implemented
  // else if Value is TGocciaNumberValue then
  //   Result := TGocciaNumberObjectValue.Create(Value as TGocciaNumberValue)
  // else if Value is TGocciaBooleanValue then
  //   Result := TGocciaBooleanObjectValue.Create(Value as TGocciaBooleanValue)
  else if Value is TGocciaObjectValue then
    Result := Value as TGocciaObjectValue
  else
    Result := nil; // null/undefined can't be boxed
end;

function UnboxIfPrimitive(Value: TGocciaValue): TGocciaValue;
begin
  if Value is TGocciaStringObjectValue then
    Result := TGocciaStringObjectValue(Value).PrimitiveValue
  // TODO: Add other primitive object types when implemented
  else
    Result := Value;
end;

{ TGocciaStringObjectValue }

function TGocciaStringObjectValue.StringLength(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Get the string value - handle both primitives and boxed objects
  if ThisValue is TGocciaStringValue then
  begin
    StringValue := ThisValue.ToString;
  end
  else if ThisValue is TGocciaStringObjectValue then
  begin
    StringValue := TGocciaStringObjectValue(ThisValue).PrimitiveValue.ToString;
  end
  else
  begin
    // For any other case that might be a boxing context, try to extract the primitive
    // This handles cases where ThisValue is the boxing object itself
    StringValue := Self.PrimitiveValue.ToString;
  end;

  Result := TGocciaNumberValue.Create(Length(StringValue));
end;

function TGocciaStringObjectValue.StringCharAt(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  Index: Integer;
  NumberValue: Double;
  TempNumberValue: TGocciaNumberValue;
begin
  // Get the string value
  if ThisValue is TGocciaStringValue then
    StringValue := ThisValue.ToString
  else if ThisValue is TGocciaStringObjectValue then
    StringValue := TGocciaStringObjectValue(ThisValue).PrimitiveValue.ToString
  else
  begin
    // For any other case that might be a boxing context, try to extract the primitive
    StringValue := Self.PrimitiveValue.ToString;
  end;

  // Get the index argument with safe conversion
  if Args.Count > 0 then
  begin
    try
      // Handle special values according to ECMAScript spec:
      // - NaN converts to 0
      // - Infinity/-Infinity are treated as out-of-bounds (use very large number)
      if (Args[0] is TGocciaNumberValue) and TGocciaNumberValue(Args[0]).IsNaN then
        Index := 0
      else
      begin
        NumberValue := Args[0].ToNumber;
        // Check for infinity using TGocciaNumberValue properties
        TempNumberValue := TGocciaNumberValue.Create(NumberValue);
        try
          if TempNumberValue.IsInfinity or TempNumberValue.IsNegativeInfinity then
            Index := MaxInt // Force out-of-bounds
          else
            Index := Trunc(NumberValue);
        finally
          TempNumberValue.Free;
        end;
      end;
    except
      Index := 0; // Default to 0 if conversion fails
    end;
  end
  else
    Index := 0;

  // Return character at index or empty string
  if (Index >= 0) and (Index < Length(StringValue)) then
    Result := TGocciaStringValue.Create(StringValue[Index + 1]) // Pascal is 1-indexed
  else
    Result := TGocciaStringValue.Create('');
end;

function TGocciaStringObjectValue.StringCharCodeAt(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  Index: Integer;
  NumberValue: Double;
  TempNumberValue: TGocciaNumberValue;
begin
  // Get the string value
  if ThisValue is TGocciaStringValue then
    StringValue := ThisValue.ToString
  else if ThisValue is TGocciaStringObjectValue then
    StringValue := TGocciaStringObjectValue(ThisValue).PrimitiveValue.ToString
  else
  begin
    // For any other case that might be a boxing context, try to extract the primitive
    StringValue := Self.PrimitiveValue.ToString;
  end;

  // Get the index argument with safe conversion
  if Args.Count > 0 then
  begin
    try
      // Handle special values according to ECMAScript spec:
      // - NaN converts to 0
      // - Infinity/-Infinity are treated as out-of-bounds (use very large number)
      if (Args[0] is TGocciaNumberValue) and TGocciaNumberValue(Args[0]).IsNaN then
        Index := 0
      else
      begin
        NumberValue := Args[0].ToNumber;
        // Check for infinity using TGocciaNumberValue properties
        TempNumberValue := TGocciaNumberValue.Create(NumberValue);
        try
          if TempNumberValue.IsInfinity or TempNumberValue.IsNegativeInfinity then
            Index := MaxInt // Force out-of-bounds
          else
            Index := Trunc(NumberValue);
        finally
          TempNumberValue.Free;
        end;
      end;
    except
      Index := 0; // Default to 0 if conversion fails
    end;
  end
  else
    Index := 0;

  // Return character code at index or NaN
  if (Index >= 0) and (Index < Length(StringValue)) then
    Result := TGocciaNumberValue.Create(Ord(StringValue[Index + 1])) // Pascal is 1-indexed
  else
    Result := TGocciaNumberValue.CreateNaN;
end;

function TGocciaStringObjectValue.StringToUpperCase(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Get the string value
  if ThisValue is TGocciaStringValue then
    StringValue := ThisValue.ToString
  else if ThisValue is TGocciaStringObjectValue then
    StringValue := TGocciaStringObjectValue(ThisValue).PrimitiveValue.ToString
  else
    StringValue := Self.PrimitiveValue.ToString;

  Result := TGocciaStringValue.Create(UpperCase(StringValue));
end;

function TGocciaStringObjectValue.StringToLowerCase(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Get the string value
  if ThisValue is TGocciaStringValue then
    StringValue := ThisValue.ToString
  else if ThisValue is TGocciaStringObjectValue then
    StringValue := TGocciaStringObjectValue(ThisValue).PrimitiveValue.ToString
  else
    StringValue := ThisValue.ToString;

  Result := TGocciaStringValue.Create(LowerCase(StringValue));
end;

function TGocciaStringObjectValue.StringSlice(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  StartIndex, EndIndex: Integer;
  Len: Integer;
begin
  // Get the string value
  if ThisValue is TGocciaStringValue then
    StringValue := ThisValue.ToString
  else if ThisValue is TGocciaStringObjectValue then
    StringValue := TGocciaStringObjectValue(ThisValue).PrimitiveValue.ToString
  else
    StringValue := ThisValue.ToString;

  Len := Length(StringValue);

  // Get start index
  if Args.Count > 0 then
    StartIndex := Trunc(Args[0].ToNumber)
  else
    StartIndex := 0;

  // Handle negative start index
  if StartIndex < 0 then
    StartIndex := Max(0, Len + StartIndex);

  // Get end index
  if Args.Count > 1 then
    EndIndex := Trunc(Args[1].ToNumber)
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
    Result := TGocciaStringValue.Create(Copy(StringValue, StartIndex + 1, EndIndex - StartIndex))
  else
    Result := TGocciaStringValue.Create('');
end;

function TGocciaStringObjectValue.StringSubstring(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  StartIndex, EndIndex: Integer;
  Len: Integer;
  Temp: Integer;
begin
  // Get the string value
  if ThisValue is TGocciaStringValue then
    StringValue := ThisValue.ToString
  else if ThisValue is TGocciaStringObjectValue then
    StringValue := TGocciaStringObjectValue(ThisValue).PrimitiveValue.ToString
  else
    StringValue := ThisValue.ToString;

  Len := Length(StringValue);

  // Get start index
  if Args.Count > 0 then
    StartIndex := Trunc(Args[0].ToNumber)
  else
    StartIndex := 0;

  // Get end index
  if Args.Count > 1 then
    EndIndex := Trunc(Args[1].ToNumber)
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
    Result := TGocciaStringValue.Create(Copy(StringValue, StartIndex + 1, EndIndex - StartIndex))
  else
    Result := TGocciaStringValue.Create('');
end;

function TGocciaStringObjectValue.StringIndexOf(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  StartPosition: Integer;
  FoundIndex: Integer;
begin
  // Get the string value
  if ThisValue is TGocciaStringValue then
    StringValue := ThisValue.ToString
  else if ThisValue is TGocciaStringObjectValue then
    StringValue := TGocciaStringObjectValue(ThisValue).PrimitiveValue.ToString
  else
    StringValue := ThisValue.ToString;

  // Get search string
  if Args.Count > 0 then
    SearchValue := Args[0].ToString
  else
    SearchValue := 'undefined';

  // Get start position
  if Args.Count > 1 then
    StartPosition := Trunc(Args[1].ToNumber)
  else
    StartPosition := 0;

  // Clamp start position
  StartPosition := Max(0, StartPosition);

  // Search for the substring (Pascal is 1-indexed)
  if StartPosition < Length(StringValue) then
  begin
    FoundIndex := Pos(SearchValue, Copy(StringValue, StartPosition + 1, Length(StringValue)));
    if FoundIndex > 0 then
      Result := TGocciaNumberValue.Create(FoundIndex + StartPosition - 1) // Convert back to 0-indexed
    else
      Result := TGocciaNumberValue.Create(-1);
  end
  else
    Result := TGocciaNumberValue.Create(-1);
end;

function TGocciaStringObjectValue.StringLastIndexOf(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  StartPosition: Integer;
  FoundIndex: Integer;
  I: Integer;
begin
  // Get the string value
  if ThisValue is TGocciaStringValue then
    StringValue := ThisValue.ToString
  else if ThisValue is TGocciaStringObjectValue then
    StringValue := TGocciaStringObjectValue(ThisValue).PrimitiveValue.ToString
  else
    StringValue := ThisValue.ToString;

  // Get search string
  if Args.Count > 0 then
    SearchValue := Args[0].ToString
  else
    SearchValue := 'undefined';

  // Get start position
  if Args.Count > 1 then
    StartPosition := Trunc(Args[1].ToNumber)
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

  Result := TGocciaNumberValue.Create(FoundIndex);
end;

function TGocciaStringObjectValue.StringIncludes(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  StartPosition: Integer;
  FoundIndex: Integer;
begin
  // Get the string value
  if ThisValue is TGocciaStringValue then
    StringValue := ThisValue.ToString
  else if ThisValue is TGocciaStringObjectValue then
    StringValue := TGocciaStringObjectValue(ThisValue).PrimitiveValue.ToString
  else
    StringValue := Self.PrimitiveValue.ToString;

  // Get search string
  if Args.Count > 0 then
    SearchValue := Args[0].ToString
  else
    SearchValue := 'undefined';

  // Get start position
  if Args.Count > 1 then
    StartPosition := Trunc(Args[1].ToNumber)
  else
    StartPosition := 0;

  // Clamp start position
  StartPosition := Max(0, StartPosition);

  // Handle edge cases
  if StartPosition >= Length(StringValue) then
  begin
    // If start position is beyond string length, only empty string can be found
    Result := TGocciaBooleanValue.Create(SearchValue = '');
    Exit;
  end;

  // Search for the substring (Pascal is 1-indexed)
  FoundIndex := Pos(SearchValue, Copy(StringValue, StartPosition + 1, Length(StringValue)));
  Result := TGocciaBooleanValue.Create(FoundIndex > 0);
end;

function TGocciaStringObjectValue.StringStartsWith(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  StartPosition: Integer;
  FoundIndex: Integer;
begin
  // Get the string value
  if ThisValue is TGocciaStringValue then
    StringValue := ThisValue.ToString
  else if ThisValue is TGocciaStringObjectValue then
    StringValue := TGocciaStringObjectValue(ThisValue).PrimitiveValue.ToString
  else
    StringValue := ThisValue.ToString;

  // Get search string
  if Args.Count > 0 then
    SearchValue := Args[0].ToString
  else
    SearchValue := 'undefined';

  // Check if the string starts with the search string
  if Length(StringValue) >= Length(SearchValue) then
    Result := TGocciaBooleanValue.Create(Copy(StringValue, 1, Length(SearchValue)) = SearchValue)
  else
    Result := TGocciaBooleanValue.Create(False);
end;

function TGocciaStringObjectValue.StringEndsWith(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  StartPosition: Integer;
  FoundIndex: Integer;
begin
  // Get the string value
  if ThisValue is TGocciaStringValue then
    StringValue := ThisValue.ToString
  else if ThisValue is TGocciaStringObjectValue then
    StringValue := TGocciaStringObjectValue(ThisValue).PrimitiveValue.ToString
  else
    StringValue := ThisValue.ToString;

  // Get search string
  if Args.Count > 0 then
    SearchValue := Args[0].ToString
  else
    SearchValue := 'undefined';

  // Check if the string ends with the search string
  if Length(StringValue) >= Length(SearchValue) then
    Result := TGocciaBooleanValue.Create(Copy(StringValue, Length(StringValue) - Length(SearchValue) + 1, Length(SearchValue)) = SearchValue)
  else
    Result := TGocciaBooleanValue.Create(False);
end;

function TGocciaStringObjectValue.StringTrim(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Get the string value
  if ThisValue is TGocciaStringValue then
    StringValue := ThisValue.ToString
  else if ThisValue is TGocciaStringObjectValue then
    StringValue := TGocciaStringObjectValue(ThisValue).PrimitiveValue.ToString
  else
    StringValue := ThisValue.ToString;

  // Trim the string
  Result := TGocciaStringValue.Create(Trim(StringValue));
end;

function TGocciaStringObjectValue.StringTrimStart(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Get the string value
  if ThisValue is TGocciaStringValue then
    StringValue := ThisValue.ToString
  else if ThisValue is TGocciaStringObjectValue then
    StringValue := TGocciaStringObjectValue(ThisValue).PrimitiveValue.ToString
  else
    StringValue := ThisValue.ToString;

  // Trim the string
  Result := TGocciaStringValue.Create(TrimLeft(StringValue));
end;

function TGocciaStringObjectValue.StringTrimEnd(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Get the string value
  if ThisValue is TGocciaStringValue then
    StringValue := ThisValue.ToString
  else if ThisValue is TGocciaStringObjectValue then
    StringValue := TGocciaStringObjectValue(ThisValue).PrimitiveValue.ToString
  else
    StringValue := ThisValue.ToString;

  // Trim the string
  Result := TGocciaStringValue.Create(TrimRight(StringValue));
end;

function TGocciaStringObjectValue.StringReplaceMethod(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue, ReplaceValue: string;
begin
  // Get the string value
  if ThisValue is TGocciaStringValue then
    StringValue := ThisValue.ToString
  else if ThisValue is TGocciaStringObjectValue then
    StringValue := TGocciaStringObjectValue(ThisValue).PrimitiveValue.ToString
  else
    StringValue := ThisValue.ToString;

  // Get search string
  if Args.Count > 0 then
    SearchValue := Args[0].ToString
  else
    SearchValue := 'undefined';

  // Get replace string
  if Args.Count > 1 then
    ReplaceValue := Args[1].ToString
  else
    ReplaceValue := 'undefined';

  // Replace the string
  Result := TGocciaStringValue.Create(StringReplace(StringValue, SearchValue, ReplaceValue, [rfReplaceAll, rfIgnoreCase]));
end;

function TGocciaStringObjectValue.StringSplit(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
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
  if ThisValue is TGocciaStringValue then
    StringValue := ThisValue.ToString
  else if ThisValue is TGocciaStringObjectValue then
    StringValue := TGocciaStringObjectValue(ThisValue).PrimitiveValue.ToString
  else
    StringValue := ThisValue.ToString;

  // Get separator
  if Args.Count > 0 then
    Separator := Args[0].ToString
  else
    Separator := 'undefined';

  // Get limit parameter (ECMAScript requirement)
  HasLimit := Args.Count > 1;
  if HasLimit then
  begin
    Limit := Trunc(Args[1].ToNumber);
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
      ResultArray.Elements.Add(TGocciaStringValue.Create(''));
      Result := ResultArray;
      Exit;
    end;
  end;

  if Separator = '' then
  begin
    // Split into individual characters
    for I := 1 to Length(StringValue) do
    begin
      ResultArray.Elements.Add(TGocciaStringValue.Create(StringValue[I]));
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
    ResultArray.Elements.Add(TGocciaStringValue.Create(StringValue));
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
        ResultArray.Elements.Add(TGocciaStringValue.Create(RemainingString));
        Break;
      end
      else
      begin
        // Found separator, add segment before it
        Segment := Copy(RemainingString, 1, SeparatorPos - 1);
        ResultArray.Elements.Add(TGocciaStringValue.Create(Segment));

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

function TGocciaStringObjectValue.StringRepeat(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  Count: Integer;
begin
  // Get the string value
  if ThisValue is TGocciaStringValue then
    StringValue := ThisValue.ToString
  else if ThisValue is TGocciaStringObjectValue then
    StringValue := TGocciaStringObjectValue(ThisValue).PrimitiveValue.ToString
  else
    StringValue := ThisValue.ToString;

  // Get count
  if Args.Count > 0 then
    Count := Trunc(Args[0].ToNumber)
  else
    Count := 1;

  // Repeat the string
  Result := TGocciaStringValue.Create(DupeString(StringValue, Count));
end;

end.
