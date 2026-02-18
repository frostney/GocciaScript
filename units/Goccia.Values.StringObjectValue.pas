unit Goccia.Values.StringObjectValue;

{$I Goccia.inc}

interface

uses
  Generics.Collections,
  Math,
  StrUtils,
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Values.FunctionValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaStringObjectValue = class(TGocciaObjectValue)
  private
    FPrimitive: TGocciaStringLiteralValue;

    class var FSharedStringPrototype: TGocciaObjectValue;
    class var FPrototypeMethodHost: TGocciaStringObjectValue;

    function ExtractStringValue(const AValue: TGocciaValue): string;
  public
    constructor Create(const APrimitive: TGocciaStringLiteralValue);
    destructor Destroy; override;
    function TypeName: string; override;
    function GetProperty(const AName: string): TGocciaValue; override;

    procedure InitializePrototype;
    procedure MarkReferences; override;
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
  end;


implementation

uses
  Goccia.GarbageCollector,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.ErrorHelper,
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

constructor TGocciaStringObjectValue.Create(const APrimitive: TGocciaStringLiteralValue);
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

  if Assigned(TGocciaGarbageCollector.Instance) then
  begin
    TGocciaGarbageCollector.Instance.PinValue(FSharedStringPrototype);
    TGocciaGarbageCollector.Instance.PinValue(FPrototypeMethodHost);
  end;
end;

{ TGocciaStringObjectValue }

function TGocciaStringObjectValue.StringLength(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Get the string value - handle both primitives and boxed objects
  StringValue := ExtractStringValue(AThisValue);
  Result := TGocciaNumberLiteralValue.Create(Length(StringValue));
end;

function TGocciaStringObjectValue.StringCharAt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  Index: Integer;
  TempNumberValue: TGocciaNumberLiteralValue;
  Arg: TGocciaValue;
begin
  // Get the string value
  StringValue := ExtractStringValue(AThisValue);

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

  if (Index >= 0) and (Index < Length(StringValue)) then
    Result := TGocciaStringLiteralValue.Create(StringValue[Index + 1])
  else
    Result := TGocciaStringLiteralValue.Create('');
end;

function TGocciaStringObjectValue.StringCharCodeAt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  Index: Integer;
  TempNumberValue: TGocciaNumberLiteralValue;
  Arg: TGocciaValue;
begin
  // Get the string value
  StringValue := ExtractStringValue(AThisValue);

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

  if (Index >= 0) and (Index < Length(StringValue)) then
    Result := TGocciaNumberLiteralValue.SmallInt(Ord(StringValue[Index + 1]))
  else
    Result := TGocciaNumberLiteralValue.NaNValue;
end;

function TGocciaStringObjectValue.StringToUpperCase(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Get the string value
  StringValue := ExtractStringValue(AThisValue);

  Result := TGocciaStringLiteralValue.Create(UpperCase(StringValue));
end;

function TGocciaStringObjectValue.StringToLowerCase(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Get the string value
  StringValue := ExtractStringValue(AThisValue);

  Result := TGocciaStringLiteralValue.Create(LowerCase(StringValue));
end;

function TGocciaStringObjectValue.StringSlice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  StartIndex, EndIndex: Integer;
  Len: Integer;
begin
  // Get the string value
  StringValue := ExtractStringValue(AThisValue);

  Len := Length(StringValue);

  // Get start index
  if AArgs.Length > 0 then
    StartIndex := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value)
  else
    StartIndex := 0;

  // Handle negative start index
  if StartIndex < 0 then
    StartIndex := Max(0, Len + StartIndex);

  // Get end index
  if AArgs.Length > 1 then
    EndIndex := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value)
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

function TGocciaStringObjectValue.StringSubstring(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  StartIndex, EndIndex: Integer;
  Len: Integer;
  Temp: Integer;
begin
  // Get the string value
  StringValue := ExtractStringValue(AThisValue);

  Len := Length(StringValue);

  // Get start index
  if AArgs.Length > 0 then
    StartIndex := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value)
  else
    StartIndex := 0;

  // Get end index
  if AArgs.Length > 1 then
    EndIndex := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value)
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

function TGocciaStringObjectValue.StringIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  StartPosition: Integer;
  FoundIndex: Integer;
begin
  // Get the string value
  StringValue := ExtractStringValue(AThisValue);

  // Get search string
  if AArgs.Length > 0 then
    SearchValue := AArgs.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  // Get start position
  if AArgs.Length > 1 then
    StartPosition := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value)
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

function TGocciaStringObjectValue.StringLastIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  StartPosition: Integer;
  FoundIndex: Integer;
  I: Integer;
begin
  // Get the string value
  StringValue := ExtractStringValue(AThisValue);

  // Get search string
  if AArgs.Length > 0 then
    SearchValue := AArgs.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  // Get start position
  if AArgs.Length > 1 then
    StartPosition := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value)
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

function TGocciaStringObjectValue.StringIncludes(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  StartPosition: Integer;
  FoundIndex: Integer;
begin
  // Get the string value
  StringValue := ExtractStringValue(AThisValue);

  // Get search string
  if AArgs.Length > 0 then
    SearchValue := AArgs.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  // Get start position
  if AArgs.Length > 1 then
    StartPosition := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value)
  else
    StartPosition := 0;

  // Clamp start position
  StartPosition := Max(0, StartPosition);

  // Handle edge cases
  if StartPosition >= Length(StringValue) then
  begin
    // If start position is beyond string length, only empty string can be found
    if SearchValue = '' then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  // Search for the substring (Pascal is 1-indexed)
  FoundIndex := Pos(SearchValue, Copy(StringValue, StartPosition + 1, Length(StringValue)));
  if FoundIndex > 0 then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaStringObjectValue.StringStartsWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  StartPosition: Integer;
  FoundIndex: Integer;
begin
  // Get the string value
  StringValue := ExtractStringValue(AThisValue);

  // Get search string
  if AArgs.Length > 0 then
    SearchValue := AArgs.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  // Check if the string starts with the search string
  if (Length(StringValue) >= Length(SearchValue)) and
     (Copy(StringValue, 1, Length(SearchValue)) = SearchValue) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaStringObjectValue.StringEndsWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  StartPosition: Integer;
  FoundIndex: Integer;
begin
  // Get the string value
  StringValue := ExtractStringValue(AThisValue);

  // Get search string
  if AArgs.Length > 0 then
    SearchValue := AArgs.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  // Check if the string ends with the search string
  if (Length(StringValue) >= Length(SearchValue)) and
     (Copy(StringValue, Length(StringValue) - Length(SearchValue) + 1, Length(SearchValue)) = SearchValue) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaStringObjectValue.StringTrim(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Get the string value
  StringValue := ExtractStringValue(AThisValue);

  // Trim the string
  Result := TGocciaStringLiteralValue.Create(Trim(StringValue));
end;

function TGocciaStringObjectValue.StringTrimStart(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Get the string value
  StringValue := ExtractStringValue(AThisValue);

  // Trim the string
  Result := TGocciaStringLiteralValue.Create(TrimLeft(StringValue));
end;

function TGocciaStringObjectValue.StringTrimEnd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Get the string value
  StringValue := ExtractStringValue(AThisValue);

  // Trim the string
  Result := TGocciaStringLiteralValue.Create(TrimRight(StringValue));
end;

function TGocciaStringObjectValue.StringReplaceMethod(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue, ReplaceValue: string;
begin
  // Get the string value
  StringValue := ExtractStringValue(AThisValue);

  // Get search string
  if AArgs.Length > 0 then
    SearchValue := AArgs.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  // Get replace string
  if AArgs.Length > 1 then
    ReplaceValue := AArgs.GetElement(1).ToStringLiteral.Value
  else
    ReplaceValue := 'undefined';

  // Replace the first occurrence only (ECMAScript spec)
  Result := TGocciaStringLiteralValue.Create(StringReplace(StringValue, SearchValue, ReplaceValue, []));
end;

function TGocciaStringObjectValue.StringReplaceAllMethod(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue, ReplaceValue: string;
begin
  StringValue := ExtractStringValue(AThisValue);

  if AArgs.Length > 0 then
    SearchValue := AArgs.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  if AArgs.Length > 1 then
    ReplaceValue := AArgs.GetElement(1).ToStringLiteral.Value
  else
    ReplaceValue := 'undefined';

  // Replace all occurrences
  Result := TGocciaStringLiteralValue.Create(StringReplace(StringValue, SearchValue, ReplaceValue, [rfReplaceAll]));
end;

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
  StringValue := ExtractStringValue(AThisValue);

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
    if StringValue = '' then
    begin
      if Separator <> '' then
        ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(''));
      Result := ResultArray;
      Exit;
    end;

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

function TGocciaStringObjectValue.StringRepeat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  CountValue: TGocciaNumberLiteralValue;
  Count: Integer;
begin
  StringValue := ExtractStringValue(AThisValue);

  if AArgs.Length > 0 then
    CountValue := AArgs.GetElement(0).ToNumberLiteral
  else
    CountValue := TGocciaNumberLiteralValue.Create(1);

  if CountValue.IsNaN or CountValue.IsInfinity or CountValue.IsNegativeInfinity or (CountValue.Value < 0) then
    ThrowRangeError('Invalid count value');

  Count := Trunc(CountValue.Value);
  Result := TGocciaStringLiteralValue.Create(DupeString(StringValue, Count));
end;

function TGocciaStringObjectValue.StringPadStart(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, PadString, Padding: string;
  TargetLength, PadNeeded: Integer;
begin
  StringValue := ExtractStringValue(AThisValue);

  if AArgs.Length > 0 then
    TargetLength := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value)
  else
    TargetLength := 0;

  if Length(StringValue) >= TargetLength then
  begin
    Result := TGocciaStringLiteralValue.Create(StringValue);
    Exit;
  end;

  if (AArgs.Length > 1) and not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
    PadString := AArgs.GetElement(1).ToStringLiteral.Value
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

function TGocciaStringObjectValue.StringPadEnd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, PadString, Padding: string;
  TargetLength, PadNeeded: Integer;
begin
  StringValue := ExtractStringValue(AThisValue);

  if AArgs.Length > 0 then
    TargetLength := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value)
  else
    TargetLength := 0;

  if Length(StringValue) >= TargetLength then
  begin
    Result := TGocciaStringLiteralValue.Create(StringValue);
    Exit;
  end;

  if (AArgs.Length > 1) and not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
    PadString := AArgs.GetElement(1).ToStringLiteral.Value
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

function TGocciaStringObjectValue.StringConcat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  I: Integer;
begin
  StringValue := ExtractStringValue(AThisValue);

  for I := 0 to AArgs.Length - 1 do
  begin
    if AArgs.GetElement(I) is TGocciaSymbolValue then
      ThrowTypeError('Cannot convert a Symbol value to a string');
    StringValue := StringValue + AArgs.GetElement(I).ToStringLiteral.Value;
  end;

  Result := TGocciaStringLiteralValue.Create(StringValue);
end;

function TGocciaStringObjectValue.StringAt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  Index: Integer;
begin
  StringValue := ExtractStringValue(AThisValue);

  if AArgs.Length > 0 then
    Index := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value)
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
