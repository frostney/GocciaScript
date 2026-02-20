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

  Goccia.GarbageCollector,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
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
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringValueOf, 'valueOf', 0));
  FSharedStringPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringToString, 'toString', 0));
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

  StartPosition := Max(0, StartPosition);

  if SearchValue = '' then
  begin
    Result := TGocciaNumberLiteralValue.Create(Min(StartPosition, Length(StringValue)));
    Exit;
  end;

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

  if AArgs.Length > 1 then
    StartPosition := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value)
  else
    StartPosition := Length(StringValue);

  StartPosition := Min(StartPosition, Length(StringValue));

  if SearchValue = '' then
  begin
    Result := TGocciaNumberLiteralValue.Create(StartPosition);
    Exit;
  end;

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

  if SearchValue = '' then
  begin
    Result := TGocciaBooleanLiteralValue.TrueValue;
    Exit;
  end;

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
begin
  StringValue := ExtractStringValue(AThisValue);

  if AArgs.Length > 0 then
    SearchValue := AArgs.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  if AArgs.Length > 1 then
    StartPosition := Max(0, Trunc(AArgs.GetElement(1).ToNumberLiteral.Value))
  else
    StartPosition := 0;

  if StartPosition + Length(SearchValue) > Length(StringValue) then
    Result := TGocciaBooleanLiteralValue.FalseValue
  else if Copy(StringValue, StartPosition + 1, Length(SearchValue)) = SearchValue then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaStringObjectValue.StringEndsWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  EndPosition: Integer;
begin
  StringValue := ExtractStringValue(AThisValue);

  if AArgs.Length > 0 then
    SearchValue := AArgs.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  if AArgs.Length > 1 then
    EndPosition := Min(Max(0, Trunc(AArgs.GetElement(1).ToNumberLiteral.Value)), Length(StringValue))
  else
    EndPosition := Length(StringValue);

  if EndPosition < Length(SearchValue) then
    Result := TGocciaBooleanLiteralValue.FalseValue
  else if Copy(StringValue, EndPosition - Length(SearchValue) + 1, Length(SearchValue)) = SearchValue then
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
  ReplaceArg: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  FoundPos: Integer;
  CallResult: TGocciaValue;
begin
  StringValue := ExtractStringValue(AThisValue);

  if AArgs.Length > 0 then
    SearchValue := AArgs.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  if AArgs.Length > 1 then
    ReplaceArg := AArgs.GetElement(1)
  else
    ReplaceArg := TGocciaUndefinedLiteralValue.UndefinedValue;

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
        CallResult := TGocciaFunctionBase(ReplaceArg).Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
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

function TGocciaStringObjectValue.StringReplaceAllMethod(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue, ReplaceValue, ResultStr: string;
  ReplaceArg: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  CallResult: TGocciaValue;
  SearchPos, Offset: Integer;
begin
  StringValue := ExtractStringValue(AThisValue);

  if AArgs.Length > 0 then
    SearchValue := AArgs.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  if AArgs.Length > 1 then
    ReplaceArg := AArgs.GetElement(1)
  else
    ReplaceArg := TGocciaUndefinedLiteralValue.UndefinedValue;

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
        CallResult := TGocciaFunctionBase(ReplaceArg).Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
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
    ReplaceValue := ReplaceArg.ToStringLiteral.Value;
    Result := TGocciaStringLiteralValue.Create(StringReplace(StringValue, SearchValue, ReplaceValue, [rfReplaceAll]));
  end;
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

  if CountValue.IsNaN then
  begin
    Result := TGocciaStringLiteralValue.Create('');
    Exit;
  end;

  if CountValue.IsInfinity or CountValue.IsNegativeInfinity or (CountValue.Value < 0) then
    ThrowRangeError('Invalid count value: ' + CountValue.ToStringLiteral.Value);

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

function TGocciaStringObjectValue.StringValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(ExtractStringValue(AThisValue));
end;

function TGocciaStringObjectValue.StringToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(ExtractStringValue(AThisValue));
end;

function TGocciaStringObjectValue.StringSymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringIteratorValue.Create(TGocciaStringLiteralValue.Create(ExtractStringValue(AThisValue)));
end;

function TGocciaStringObjectValue.StringCodePointAt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  Index, ByteLen: Integer;
  B: Byte;
  CodePoint: Cardinal;
begin
  StringValue := ExtractStringValue(AThisValue);

  if AArgs.Length = 0 then
    Index := 0
  else
    Index := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);

  if (Index < 0) or (Index >= Length(StringValue)) then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

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

function TGocciaStringObjectValue.StringLocaleCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, ThatString: string;
begin
  StringValue := ExtractStringValue(AThisValue);

  if AArgs.Length = 0 then
    ThatString := 'undefined'
  else
    ThatString := AArgs.GetElement(0).ToStringLiteral.Value;

  if StringValue < ThatString then
    Result := TGocciaNumberLiteralValue.SmallInt(-1)
  else if StringValue > ThatString then
    Result := TGocciaNumberLiteralValue.SmallInt(1)
  else
    Result := TGocciaNumberLiteralValue.SmallInt(0);
end;

function TGocciaStringObjectValue.StringNormalize(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, Form: string;
begin
  StringValue := ExtractStringValue(AThisValue);

  if (AArgs.Length > 0) and not (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
    Form := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Form := 'NFC';

  if (Form <> 'NFC') and (Form <> 'NFD') and (Form <> 'NFKC') and (Form <> 'NFKD') then
    ThrowRangeError('The normalization form should be one of NFC, NFD, NFKC, NFKD');

  Result := TGocciaStringLiteralValue.Create(StringValue);
end;

function TGocciaStringObjectValue.StringIsWellFormed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  I: Integer;
  B: Byte;
begin
  StringValue := ExtractStringValue(AThisValue);
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

function TGocciaStringObjectValue.StringToWellFormed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, ResultStr: string;
  I: Integer;
  B: Byte;
begin
  StringValue := ExtractStringValue(AThisValue);
  ResultStr := '';
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
  Result := TGocciaStringLiteralValue.Create(ResultStr);
end;

end.
