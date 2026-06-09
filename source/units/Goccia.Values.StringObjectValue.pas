unit Goccia.Values.StringObjectValue;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaStringObjectValue = class(TGocciaInstanceValue)
  private
    FPrimitive: TGocciaStringLiteralValue;

    function ExtractStringValue(const AValue: TGocciaValue): string;
  public
    constructor Create(const APrimitive: TGocciaStringLiteralValue; const AClass: TGocciaClassValue = nil);
    destructor Destroy; override;
    function TypeName: string; override;
    function ToStringTag: string; override;
    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;
    function GetEnumerablePropertyNames: TArray<string>; override;
    function GetEnumerablePropertyValues: TArray<TGocciaValue>; override;
    function GetEnumerablePropertyEntries: TArray<TPair<string, TGocciaValue>>; override;
    function GetAllPropertyNames: TArray<string>; override;
    function GetOwnPropertyDescriptor(const AName: string): TGocciaPropertyDescriptor; override;
    function HasOwnProperty(const AName: string): Boolean; override;

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
    function StringToLocaleUpperCase(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringToLocaleLowerCase(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
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

  IntlICU,
  IntlLocaleResolver,
  IntlTypes,
  TextSemantics,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Realm,
  Goccia.RegExp.Runtime,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.IntlCollator,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.Iterator.RegExp,
  Goccia.Values.ProxyValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToObject;

// String.prototype lives in a per-realm slot.  Method host and member
// definitions stay process-wide (immutable across realms).
var
  GStringPrototypeSlot: TGocciaRealmSlotId;

threadvar
  FPrototypeMethodHost: TGocciaStringObjectValue;
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetSharedStringPrototype: TGocciaObjectValue; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaObjectValue(CurrentRealm.GetSlot(GStringPrototypeSlot))
  else
    Result := nil;
end;

function LocaleCompareArgumentToLocale(const AArg: TGocciaValue): string;
var
  Element: TGocciaValue;
  Tag, Canonical: string;
  FirstUnicodeExtension, SecondUnicodeExtension: Integer;
  LowerTag, Tail: string;
begin
  Result := '';
  if (AArg is TGocciaUndefinedLiteralValue) or (AArg = nil) then
    Exit;

  if AArg is TGocciaStringLiteralValue then
    Tag := TGocciaStringLiteralValue(AArg).Value
  else if AArg is TGocciaArrayValue then
  begin
    if TGocciaArrayValue(AArg).GetLength = 0 then
      Exit;
    Element := TGocciaArrayValue(AArg).GetElement(0);
    if Element is TGocciaStringLiteralValue then
      Tag := TGocciaStringLiteralValue(Element).Value
    else if Element is TGocciaObjectValue then
      Tag := Element.ToStringLiteral.Value
    else
      ThrowTypeError('locales array elements must be strings or objects');
  end
  else if AArg is TGocciaObjectValue then
    Tag := AArg.ToStringLiteral.Value
  else
    ThrowTypeError('locales argument must be a string, object, array, or undefined');

  Canonical := CanonicalizeUnicodeLocaleId(Tag);
  if Canonical = '' then
  begin
    LowerTag := LowerCase(Tag);
    FirstUnicodeExtension := Pos('-u-', LowerTag);
    if FirstUnicodeExtension <> 0 then
    begin
      Tail := Copy(LowerTag, FirstUnicodeExtension + 3, MaxInt);
      SecondUnicodeExtension := Pos('-u-', Tail);
      if SecondUnicodeExtension <> 0 then
      begin
        SecondUnicodeExtension := FirstUnicodeExtension + 3 + SecondUnicodeExtension - 1;
        Canonical := CanonicalizeUnicodeLocaleId(Copy(Tag, 1, SecondUnicodeExtension - 1));
      end;
    end;
    if Canonical = '' then
      ThrowRangeError(Format('invalid language tag: %s', [Tag]));
  end;
  Result := Tag;
end;

{ TGocciaStringObjectValue }

function CoerceRegExpValue(const AValue: TGocciaValue;
  const ANewFlags: string = ''): TGocciaObjectValue;
var
  Pattern, Flags: string;
  PropVal: TGocciaValue;
begin
  if IsRegExp(AValue) then
  begin
    PropVal := TGocciaObjectValue(AValue).GetProperty(PROP_SOURCE);
    if PropVal is TGocciaUndefinedLiteralValue then
      Pattern := ''
    else
      Pattern := PropVal.ToStringLiteral.Value;
    if ANewFlags <> '' then
      Flags := ANewFlags
    else
    begin
      PropVal := TGocciaObjectValue(AValue).GetProperty(PROP_FLAGS);
      if PropVal is TGocciaUndefinedLiteralValue then
        Flags := ''
      else
        Flags := PropVal.ToStringLiteral.Value;
    end;
    Result := TGocciaObjectValue(CreateRegExpObject(Pattern, Flags));
  end
  else
  begin
    if AValue is TGocciaUndefinedLiteralValue then
      Pattern := ''
    else
      Pattern := AValue.ToStringLiteral.Value;
    Result := TGocciaObjectValue(CreateRegExpObject(Pattern, ANewFlags));
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

function RegexReplacementCapture(const AMatchArray: TGocciaArrayValue;
  const AGroupIndex: Integer): TReplacementCapture;
var
  GroupValue: TGocciaValue;
begin
  Result.Text := '';
  Result.Matched := False;

  if (AGroupIndex < 0) or (AGroupIndex >= AMatchArray.Elements.Count) then
    Exit;

  GroupValue := AMatchArray.Elements[AGroupIndex];
  if GroupValue is TGocciaUndefinedLiteralValue then
    Exit;

  Result.Text := GroupValue.ToStringLiteral.Value;
  Result.Matched := True;
end;

function BuildReplacementCaptures(
  const AMatchArray: TGocciaArrayValue): TArray<TReplacementCapture>;
var
  I: Integer;
begin
  SetLength(Result, Max(AMatchArray.Elements.Count - 1, 0));
  for I := 0 to High(Result) do
    Result[I] := RegexReplacementCapture(AMatchArray, I + 1);
end;

function BuildReplacementNamedCaptures(
  const AMatchArray: TGocciaArrayValue): TArray<TReplacementNamedCapture>;
var
  GroupName: string;
  GroupNames: TArray<string>;
  GroupsValue: TGocciaValue;
  I: Integer;
  NamedValue: TGocciaValue;
begin
  GroupsValue := AMatchArray.GetProperty(PROP_GROUPS);
  if not (GroupsValue is TGocciaObjectValue) then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  GroupNames := TGocciaObjectValue(GroupsValue).GetEnumerablePropertyNames;
  SetLength(Result, Length(GroupNames));
  for I := 0 to High(GroupNames) do
  begin
    GroupName := GroupNames[I];
    NamedValue := TGocciaObjectValue(GroupsValue).GetProperty(GroupName);
    Result[I].Name := GroupName;
    if NamedValue is TGocciaUndefinedLiteralValue then
    begin
      Result[I].Text := '';
      Result[I].Matched := False;
    end
    else
    begin
      Result[I].Text := NamedValue.ToStringLiteral.Value;
      Result[I].Matched := True;
    end;
  end;
end;

function TGocciaStringObjectValue.ExtractStringValue(const AValue: TGocciaValue): string;
var
  Unwrapped: TGocciaValue;
begin
  if (AValue is TGocciaUndefinedLiteralValue) or
     (AValue is TGocciaNullLiteralValue) then
    ThrowTypeError(SErrorStringPrototypeRequiresNonNullish,
      SSuggestCheckNullBeforeAccess)
  else if AValue is TGocciaStringLiteralValue then
    Result := TGocciaStringLiteralValue(AValue).Value
  else if AValue is TGocciaStringObjectValue then
    Result := TGocciaStringObjectValue(AValue).Primitive.Value
  else if AValue is TGocciaSymbolValue then
    ThrowTypeError(SErrorSymbolToString, SSuggestSymbolNoImplicitConversion)
  else if AValue is TGocciaProxyValue then
  begin
    // ES2026 String.prototype methods do `Let S be ToString(O)` per
    // §22.1.3.x. For a Proxy whose [[ProxyTarget]] is (transitively) a
    // String wrapper, V8/Node read [[StringData]] from the underlying
    // wrapper. Unwrap the Proxy chain here — falling back to ToStringLiteral
    // would call user toString through the Proxy get-trap chain, which for
    // a get:null/empty handler chain forwards back to a String accessor
    // that re-enters ExtractStringValue → infinite recursion → SIGSEGV.
    Unwrapped := AValue;
    while Unwrapped is TGocciaProxyValue do
      Unwrapped := TGocciaProxyValue(Unwrapped).Target;
    if Unwrapped is TGocciaStringObjectValue then
      Result := TGocciaStringObjectValue(Unwrapped).Primitive.Value
    else if Unwrapped is TGocciaStringLiteralValue then
      Result := TGocciaStringLiteralValue(Unwrapped).Value
    else
      Result := AValue.ToStringLiteral.Value;
  end
  else
    Result := AValue.ToStringLiteral.Value;
end;

constructor TGocciaStringObjectValue.Create(const APrimitive: TGocciaStringLiteralValue; const AClass: TGocciaClassValue = nil);
var
  SharedPrototype: TGocciaObjectValue;
begin
  inherited Create(AClass);
  FPrimitive := APrimitive;
  InitializePrototype;
  SharedPrototype := GetSharedStringPrototype;
  if not Assigned(AClass) and Assigned(SharedPrototype) and
     (SharedPrototype <> TGocciaObjectValue(Self)) then
    FPrototype := SharedPrototype;
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

function TGocciaStringObjectValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_STRING;
end;

function TGocciaStringObjectValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaStringObjectValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
var
  Index: Integer;
  StringValue: string;
begin
  StringValue := FPrimitive.ToStringLiteral.Value;

  if TryStrToInt(AName, Index) and (AName = IntToStr(Index)) and
     (Index >= 0) and (Index < UTF16CodeUnitLength(StringValue)) then
    Exit(TGocciaStringLiteralValue.Create(
      UTF16CodeUnitAt(StringValue, Index)));

  if AName = PROP_LENGTH then
    Exit(TGocciaNumberLiteralValue.Create(UTF16CodeUnitLength(StringValue)));

  Result := inherited GetPropertyWithContext(AName, AThisContext);
end;

// ES2026 §10.4.3.6 StringExoticObject [[OwnPropertyKeys]]
// Character indices are enumerable, non-writable, non-configurable data properties.

// ES2026 §10.4.3.3 StringExoticObject [[OwnPropertyKeys]] (enumerable string keys)
// Character indices come first, then any expando own enumerable properties.
function TGocciaStringObjectValue.GetEnumerablePropertyNames: TArray<string>;
var
  StringValue: string;
  InheritedNames: TArray<string>;
  I: Integer;
begin
  StringValue := FPrimitive.ToStringLiteral.Value;
  InheritedNames := inherited GetEnumerablePropertyNames;
  SetLength(Result, UTF16CodeUnitLength(StringValue) + Length(InheritedNames));
  for I := 0 to UTF16CodeUnitLength(StringValue) - 1 do
    Result[I] := IntToStr(I);
  for I := 0 to High(InheritedNames) do
    Result[UTF16CodeUnitLength(StringValue) + I] := InheritedNames[I];
end;

function TGocciaStringObjectValue.GetEnumerablePropertyValues: TArray<TGocciaValue>;
var
  StringValue: string;
  InheritedValues: TArray<TGocciaValue>;
  I: Integer;
begin
  StringValue := FPrimitive.ToStringLiteral.Value;
  InheritedValues := inherited GetEnumerablePropertyValues;
  SetLength(Result, UTF16CodeUnitLength(StringValue) + Length(InheritedValues));
  for I := 0 to UTF16CodeUnitLength(StringValue) - 1 do
    Result[I] := TGocciaStringLiteralValue.Create(
      UTF16CodeUnitAt(StringValue, I));
  for I := 0 to High(InheritedValues) do
    Result[UTF16CodeUnitLength(StringValue) + I] := InheritedValues[I];
end;

function TGocciaStringObjectValue.GetEnumerablePropertyEntries: TArray<TPair<string, TGocciaValue>>;
var
  StringValue: string;
  InheritedEntries: TArray<TPair<string, TGocciaValue>>;
  I: Integer;
  Entry: TPair<string, TGocciaValue>;
begin
  StringValue := FPrimitive.ToStringLiteral.Value;
  InheritedEntries := inherited GetEnumerablePropertyEntries;
  SetLength(Result, UTF16CodeUnitLength(StringValue) + Length(InheritedEntries));
  for I := 0 to UTF16CodeUnitLength(StringValue) - 1 do
  begin
    Entry.Key := IntToStr(I);
    Entry.Value := TGocciaStringLiteralValue.Create(
      UTF16CodeUnitAt(StringValue, I));
    Result[I] := Entry;
  end;
  for I := 0 to High(InheritedEntries) do
    Result[UTF16CodeUnitLength(StringValue) + I] := InheritedEntries[I];
end;

// ES2026 §10.4.3.6 StringExoticObject [[OwnPropertyKeys]] (all own string keys)
// Indices first, then 'length', then any expando own properties.
function TGocciaStringObjectValue.GetAllPropertyNames: TArray<string>;
var
  StringValue: string;
  InheritedNames: TArray<string>;
  I: Integer;
begin
  StringValue := FPrimitive.ToStringLiteral.Value;
  InheritedNames := inherited GetAllPropertyNames;
  SetLength(Result, UTF16CodeUnitLength(StringValue) + 1 +
    Length(InheritedNames));
  for I := 0 to UTF16CodeUnitLength(StringValue) - 1 do
    Result[I] := IntToStr(I);
  Result[UTF16CodeUnitLength(StringValue)] := PROP_LENGTH;
  for I := 0 to High(InheritedNames) do
    Result[UTF16CodeUnitLength(StringValue) + 1 + I] := InheritedNames[I];
end;

// ES2026 §10.4.3.1 StringExoticObject [[GetOwnProperty]](P)
function TGocciaStringObjectValue.GetOwnPropertyDescriptor(const AName: string): TGocciaPropertyDescriptor;
var
  Index: Integer;
  StringValue: string;
begin
  // Only canonical non-negative decimal integers are string indices (e.g. "0", "1",
  // not "01" or "-0"). AName = IntToStr(Index) ensures round-trip canonicality.
  if TryStrToInt(AName, Index) and (AName = IntToStr(Index)) then
  begin
    StringValue := FPrimitive.ToStringLiteral.Value;
    if (Index >= 0) and (Index < UTF16CodeUnitLength(StringValue)) then
    begin
      // String character indices: enumerable, non-writable, non-configurable
      Result := TGocciaPropertyDescriptorData.Create(
        TGocciaStringLiteralValue.Create(UTF16CodeUnitAt(StringValue, Index)),
        [pfEnumerable]);
      Exit;
    end;
  end;

  if AName = PROP_LENGTH then
  begin
    // length: non-enumerable, non-writable, non-configurable
    Result := TGocciaPropertyDescriptorData.Create(
      TGocciaNumberLiteralValue.Create(
        UTF16CodeUnitLength(FPrimitive.ToStringLiteral.Value)),
      []);
    Exit;
  end;

  Result := inherited GetOwnPropertyDescriptor(AName);
end;

// ES2026 §10.4.3.1 StringExoticObject [[GetOwnProperty]](P) — existence check
function TGocciaStringObjectValue.HasOwnProperty(const AName: string): Boolean;
var
  Index: Integer;
  StringValue: string;
begin
  // Same canonical check as GetOwnPropertyDescriptor: reject "01", "-0", etc.
  if TryStrToInt(AName, Index) and (AName = IntToStr(Index)) then
  begin
    StringValue := FPrimitive.ToStringLiteral.Value;
    Result := (Index >= 0) and (Index < UTF16CodeUnitLength(StringValue));
    Exit;
  end;
  if AName = PROP_LENGTH then
  begin
    Result := True;
    Exit;
  end;
  Result := inherited HasOwnProperty(AName);
end;

procedure TGocciaStringObjectValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetSharedStringPrototype) then Exit;

  CurrentRealm.SetSlot(GStringPrototypeSlot, Self);
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
      Members.AddNamedMethod('toLocaleUpperCase', StringToLocaleUpperCase, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('toLocaleLowerCase', StringToLocaleLowerCase, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
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
  RegisterMemberDefinitions(Self, FPrototypeMembers);

  // SharedPrototype is pinned through the realm slot.  Pin the host directly
  // since it's a process-wide singleton.
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.PinObject(FPrototypeMethodHost);
end;

class function TGocciaStringObjectValue.GetSharedPrototype: TGocciaObjectValue;
begin
  if not Assigned(GetSharedStringPrototype) then
    TGocciaStringObjectValue.Create(TGocciaStringLiteralValue.Create(''));
  Result := GetSharedStringPrototype;
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
  Result := TGocciaNumberLiteralValue.Create(UTF16CodeUnitLength(StringValue));
end;

// ES2026 §22.1.3.1 String.prototype.charAt(pos)
function TGocciaStringObjectValue.StringCharAt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
  Index: Integer;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let position be ToIntegerOrInfinity(pos)
  Index := ToIntegerFromArgs(AArgs);

  // Step 4: If position < 0 or position >= len(S), return ""
  // Step 5: Return the String value of length 1 containing the code unit at index position
  if (Index >= 0) and (Index < UTF16CodeUnitLength(StringValue)) then
    Result := TGocciaStringLiteralValue.Create(
      UTF16CodeUnitAt(StringValue, Index))
  else
    Result := TGocciaStringLiteralValue.Create('');
end;

// ES2026 §22.1.3.2 String.prototype.charCodeAt(pos)
function TGocciaStringObjectValue.StringCharCodeAt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Character: string;
  ByteLength: Integer;
  CodePoint: Cardinal;
  StringValue: string;
  Index: Integer;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let position be ToIntegerOrInfinity(pos)
  Index := ToIntegerFromArgs(AArgs);

  // Step 4: If position < 0 or position >= len(S), return NaN
  // Step 5: Return the numeric value of the code unit at index position
  if (Index < 0) or (Index >= UTF16CodeUnitLength(StringValue)) then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  Character := UTF16CodeUnitAt(StringValue, Index);
  if TryReadUTF8CodePointAllowSurrogates(Character, 1, CodePoint,
    ByteLength) then
    Result := TGocciaNumberLiteralValue.Create(CodePoint)
  else if Character <> '' then
    Result := TGocciaNumberLiteralValue.Create(Ord(Character[1]))
  else
    Result := TGocciaNumberLiteralValue.NaNValue;
end;

// ES2026 §22.1.3.30 String.prototype.toUpperCase()
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
  Result := TGocciaStringLiteralValue.Create(UnicodeUpperCaseUTF8(StringValue));
end;

// ES2026 §22.1.3.28 String.prototype.toLowerCase()
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
  Result := TGocciaStringLiteralValue.Create(UnicodeLowerCaseUTF8(StringValue));
end;

// ES2026 §22.1.3.27 String.prototype.toLocaleUpperCase([ reserved1 [ , reserved2 ]])
function TGocciaStringObjectValue.StringToLocaleUpperCase(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, Locale, UpperResult: string;
begin
  StringValue := ExtractStringValue(AThisValue);
  Locale := '';
  if (AArgs.Length > 0) and not (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
    Locale := AArgs.GetElement(0).ToStringLiteral.Value;
  if TryICUUpperCase(Locale, StringValue, UpperResult) then
    Result := TGocciaStringLiteralValue.Create(UpperResult)
  else
    Result := StringToUpperCase(AArgs, AThisValue);
end;

// ES2026 §22.1.3.26 String.prototype.toLocaleLowerCase([ reserved1 [ , reserved2 ]])
function TGocciaStringObjectValue.StringToLocaleLowerCase(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, Locale, LowerResult: string;
begin
  StringValue := ExtractStringValue(AThisValue);
  Locale := '';
  if (AArgs.Length > 0) and not (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
    Locale := AArgs.GetElement(0).ToStringLiteral.Value;
  if TryICULowerCase(Locale, StringValue, LowerResult) then
    Result := TGocciaStringLiteralValue.Create(LowerResult)
  else
    Result := StringToLowerCase(AArgs, AThisValue);
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
  Len := UTF16CodeUnitLength(StringValue);

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
    Result := TGocciaStringLiteralValue.Create(
      UTF16Substring(StringValue, StartIndex, EndIndex - StartIndex))
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
  Len := UTF16CodeUnitLength(StringValue);

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
    Result := TGocciaStringLiteralValue.Create(
      UTF16Substring(StringValue, StartIndex, EndIndex - StartIndex))
  else
    Result := TGocciaStringLiteralValue.Create('');
end;

// ES2026 §22.1.3.9 String.prototype.indexOf(searchString [, position])
function TGocciaStringObjectValue.StringIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  StartPosition: Integer;
  FoundIndex: Integer;
  Len: Integer;
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
  Len := UTF16CodeUnitLength(StringValue);
  StartPosition := Min(Max(0, StartPosition), Len);

  if SearchValue = '' then
  begin
    Result := TGocciaNumberLiteralValue.Create(StartPosition);
    Exit;
  end;

  // Step 6-7: Search for first occurrence of searchStr in S at or after start; return index or -1
  FoundIndex := UTF16IndexOf(StringValue, SearchValue, StartPosition);
  Result := TGocciaNumberLiteralValue.Create(FoundIndex);
end;

// ES2026 §22.1.3.10 String.prototype.lastIndexOf(searchString [, position])
function TGocciaStringObjectValue.StringLastIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  StartPosition: Integer;
  FoundIndex: Integer;
  Len: Integer;
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
  if (AArgs.Length > 1) and
     not (AArgs.GetElement(1).ToNumberLiteral.IsNaN) then
    StartPosition := ToIntegerFromArgs(AArgs, 1, UTF16CodeUnitLength(StringValue))
  else
    StartPosition := UTF16CodeUnitLength(StringValue);

  // Step 5: Let start be min(max(pos, 0), len)
  Len := UTF16CodeUnitLength(StringValue);
  StartPosition := Min(Max(StartPosition, 0), Len);

  if SearchValue = '' then
  begin
    Result := TGocciaNumberLiteralValue.Create(StartPosition);
    Exit;
  end;

  // Step 6-7: Search backwards for last occurrence of searchStr at or before start; return index or -1
  FoundIndex := UTF16LastIndexOf(StringValue, SearchValue, StartPosition);
  Result := TGocciaNumberLiteralValue.Create(FoundIndex);
end;

// ES2026 §22.1.3.7 String.prototype.includes(searchString [, position])
function TGocciaStringObjectValue.StringIncludes(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  StartPosition: Integer;
  FoundIndex: Integer;
  Len: Integer;
begin
  // Step 1–2: Let S be ToString(RequireObjectCoercible(this))
  StringValue := ExtractStringValue(AThisValue);

  // §22.1.3.8 steps 3–4: throw if searchString is regexp-like
  if (AArgs.Length > 0) and IsRegExp(AArgs.GetElement(0)) then
    ThrowTypeError(Format(SErrorFirstArgMustNotBeRegExp,
      ['String.prototype.includes']), SSuggestUseMatchOrSearch);

  // Step 5: Let searchStr be ToString(searchString)
  if AArgs.Length > 0 then
    SearchValue := AArgs.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  // Step 4: Let pos be ToIntegerOrInfinity(position)
  StartPosition := ToIntegerFromArgs(AArgs, 1);

  // Step 5: Let start be min(max(pos, 0), len)
  Len := UTF16CodeUnitLength(StringValue);
  StartPosition := Min(Max(0, StartPosition), Len);

  if StartPosition >= Len then
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
  FoundIndex := UTF16IndexOf(StringValue, SearchValue, StartPosition);
  if FoundIndex >= 0 then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §22.1.3.24 String.prototype.startsWith(searchString [, position])
function TGocciaStringObjectValue.StringStartsWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  StartPosition: Integer;
  Len: Integer;
begin
  // Step 1–2: Let S be ToString(RequireObjectCoercible(this))
  StringValue := ExtractStringValue(AThisValue);

  // §22.1.3.24 steps 3–4: throw if searchString is regexp-like
  if (AArgs.Length > 0) and IsRegExp(AArgs.GetElement(0)) then
    ThrowTypeError(Format(SErrorFirstArgMustNotBeRegExp,
      ['String.prototype.startsWith']), SSuggestUseMatchOrSearch);

  // Step 5: Let searchStr be ToString(searchString)
  if AArgs.Length > 0 then
    SearchValue := AArgs.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  // Step 6: Let start be min(max(ToIntegerOrInfinity(position), 0), len)
  Len := UTF16CodeUnitLength(StringValue);
  StartPosition := Min(Max(0, ToIntegerFromArgs(AArgs, 1)), Len);

  // Step 7–8: If searchLength + start > len(S), return false; else compare
  if StartPosition + UTF16CodeUnitLength(SearchValue) > Len then
    Result := TGocciaBooleanLiteralValue.FalseValue
  else if UTF16IndexOf(StringValue, SearchValue, StartPosition) = StartPosition then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §22.1.3.6 String.prototype.endsWith(searchString [, endPosition])
function TGocciaStringObjectValue.StringEndsWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue: string;
  EndPosition: Integer;
  SearchLength: Integer;
  StartPosition: Integer;
begin
  // Step 1–2: Let S be ToString(RequireObjectCoercible(this))
  StringValue := ExtractStringValue(AThisValue);

  // §22.1.3.6 steps 3–4: throw if searchString is regexp-like
  if (AArgs.Length > 0) and IsRegExp(AArgs.GetElement(0)) then
    ThrowTypeError(Format(SErrorFirstArgMustNotBeRegExp,
      ['String.prototype.endsWith']), SSuggestUseMatchOrSearch);

  // Step 5: Let searchStr be ToString(searchString)
  if AArgs.Length > 0 then
    SearchValue := AArgs.GetElement(0).ToStringLiteral.Value
  else
    SearchValue := 'undefined';

  // Step 6: Let searchLength be the length of searchStr
  // Step 7: If endPosition is undefined, let pos be len; else let pos be ToIntegerOrInfinity(endPosition)
  // Step 8: Let end be min(max(pos, 0), len)
  EndPosition := Min(Max(0, ToIntegerFromArgs(AArgs, 1,
    UTF16CodeUnitLength(StringValue))), UTF16CodeUnitLength(StringValue));

  // Step 7: Let start be end - searchLength
  // Step 8: If start < 0, return false
  // Step 9: If code units of S from start to end match searchStr, return true; else false
  SearchLength := UTF16CodeUnitLength(SearchValue);
  StartPosition := EndPosition - SearchLength;
  if StartPosition < 0 then
    Result := TGocciaBooleanLiteralValue.FalseValue
  else if UTF16IndexOf(StringValue, SearchValue, StartPosition) = StartPosition then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §22.1.3.32 String.prototype.trim()
function TGocciaStringObjectValue.StringTrim(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Return TrimString(S, start+end)
  Result := TGocciaStringLiteralValue.Create(
    TrimECMAScriptWhitespace(StringValue));
end;

// ES2026 §22.1.3.34 String.prototype.trimStart()
function TGocciaStringObjectValue.StringTrimStart(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Return TrimString(S, start)
  Result := TGocciaStringLiteralValue.Create(
    TrimECMAScriptWhitespaceStart(StringValue));
end;

// ES2026 §22.1.3.33 String.prototype.trimEnd()
function TGocciaStringObjectValue.StringTrimEnd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Return TrimString(S, end)
  Result := TGocciaStringLiteralValue.Create(
    TrimECMAScriptWhitespaceEnd(StringValue));
end;

function BuildRegexReplacement(const AReplaceArg: TGocciaValue;
  const AMatchArray: TGocciaArrayValue; const AMatchIndex: Integer;
  const AInput: string): string;
var
  CallArgs: TGocciaArgumentsCollection;
  Captures: TArray<TReplacementCapture>;
  I: Integer;
  MatchCapture: TReplacementCapture;
  NamedCaptures: TArray<TReplacementNamedCapture>;
  GroupsValue: TGocciaValue;
begin
  if not AReplaceArg.IsCallable then
  begin
    Captures := BuildReplacementCaptures(AMatchArray);
    NamedCaptures := BuildReplacementNamedCaptures(AMatchArray);
    MatchCapture := RegexReplacementCapture(AMatchArray, 0);
    Exit(ExpandReplacementPattern(AReplaceArg.ToStringLiteral.Value,
      MatchCapture.Text, AInput, AMatchIndex,
      Captures, NamedCaptures));
  end;

  CallArgs := TGocciaArgumentsCollection.CreateWithCapacity(
    AMatchArray.Elements.Count + 3);
  try
    for I := 0 to AMatchArray.Elements.Count - 1 do
      CallArgs.Add(AMatchArray.Elements[I]);
    CallArgs.Add(TGocciaNumberLiteralValue.Create(AMatchIndex));
    CallArgs.Add(TGocciaStringLiteralValue.Create(AInput));
    GroupsValue := AMatchArray.GetProperty(PROP_GROUPS);
    if not (GroupsValue is TGocciaUndefinedLiteralValue) then
      CallArgs.Add(GroupsValue);
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
  SearchLength, StringLength: Integer;
begin
  // ES2026 §22.1.3.19 String.prototype.replace:
  //   1. Let O be ? RequireObjectCoercible(this value).
  //   2. If searchValue is neither undefined nor null:
  //      c. Let replacer be ? GetMethod(searchValue, @@replace).
  //      d. If replacer is not undefined: return ? Call(replacer, searchValue, « O, replaceValue »).
  //   3. Let string be ? ToString(O).
  // Step 1 — RequireObjectCoercible: throw if `this` is null/undefined.
  if (AThisValue is TGocciaUndefinedLiteralValue) or
     (AThisValue is TGocciaNullLiteralValue) then
    ThrowTypeError(SErrorStringPrototypeRequiresNonNullish, SSuggestCheckNullBeforeAccess);

  if AArgs.Length > 0 then
    SearchArg := AArgs.GetElement(0)
  else
    SearchArg := TGocciaUndefinedLiteralValue.UndefinedValue;

  if AArgs.Length > 1 then
    ReplaceArg := AArgs.GetElement(1)
  else
    ReplaceArg := TGocciaUndefinedLiteralValue.UndefinedValue;

  // Step 2.c-d: dispatch to @@replace before doing any ToString on O —
  // matches the StringReplaceAllMethod fix above.
  ReplaceMethod := GetMethodBySymbol(SearchArg,
    TGocciaSymbolValue.WellKnownReplace);
  if not (ReplaceMethod is TGocciaUndefinedLiteralValue) then
  begin
    if not ReplaceMethod.IsCallable then
      ThrowTypeError(SErrorSymbolReplaceNotCallable, SSuggestWellKnownSymbolCallable);
    // Spec passes O through to the replacer — not a stringified copy.
    CallArgs := TGocciaArgumentsCollection.Create([
      AThisValue,
      ReplaceArg
    ]);
    try
      Result := InvokeCallable(ReplaceMethod, CallArgs, SearchArg);
    finally
      CallArgs.Free;
    end;
    Exit;
  end;

  // Step 3: Let string be ? ToString(O).
  StringValue := ExtractStringValue(AThisValue);

  // §22.1.3.19 step 4: Let searchString be ? ToString(searchValue).
  SearchValue := SearchArg.ToStringLiteral.Value;
  SearchLength := UTF16CodeUnitLength(SearchValue);
  StringLength := UTF16CodeUnitLength(StringValue);

  // Step 5: Let pos be StringIndexOf(string, searchString, 0)
  // Step 6: If not found, return string
  // Step 7: Replace first occurrence
  if ReplaceArg.IsCallable then
  begin
    if SearchValue = '' then
      FoundPos := 0
    else
      FoundPos := UTF16IndexOf(StringValue, SearchValue, 0);
    if FoundPos >= 0 then
    begin
      CallArgs := TGocciaArgumentsCollection.Create([
        TGocciaStringLiteralValue.Create(SearchValue),
        TGocciaNumberLiteralValue.Create(FoundPos),
        TGocciaStringLiteralValue.Create(StringValue)
      ]);
      try
        CallResult := InvokeCallable(ReplaceArg, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
        ReplaceValue := CallResult.ToStringLiteral.Value;
      finally
        CallArgs.Free;
      end;
      Result := TGocciaStringLiteralValue.Create(
        UTF16Substring(StringValue, 0, FoundPos) + ReplaceValue +
        UTF16Substring(StringValue, FoundPos + SearchLength,
        StringLength - FoundPos - SearchLength));
    end
    else
      Result := TGocciaStringLiteralValue.Create(StringValue);
  end
  else
  begin
    ReplaceValue := ReplaceArg.ToStringLiteral.Value;
    if SearchValue = '' then
      FoundPos := 0
    else
      FoundPos := UTF16IndexOf(StringValue, SearchValue, 0);
    if SearchValue = '' then
      Result := TGocciaStringLiteralValue.Create(
        ExpandReplacementPattern(ReplaceValue, SearchValue, StringValue, 0,
          [], []) + StringValue)
    else if FoundPos >= 0 then
      Result := TGocciaStringLiteralValue.Create(
        UTF16Substring(StringValue, 0, FoundPos) +
        ExpandReplacementPattern(ReplaceValue, SearchValue, StringValue,
          FoundPos, [], []) +
        UTF16Substring(StringValue, FoundPos + SearchLength,
          StringLength - FoundPos - SearchLength))
    else
      Result := TGocciaStringLiteralValue.Create(StringValue);
  end;
end;

// ES2026 §22.1.3.20 String.prototype.replaceAll(searchValue, replaceValue)
function TGocciaStringObjectValue.StringReplaceAllMethod(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, SearchValue, ReplaceValue, ResultStr: string;
  SearchArg, FlagsVal: TGocciaValue;
  ReplaceArg: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  CallResult: TGocciaValue;
  MatchStart, NextOffset, SearchPos, Offset: Integer;
  SearchLength, StringLength: Integer;
  ReplaceMethod: TGocciaValue;
  RegexValue: TGocciaObjectValue;
  MatchArray: TGocciaObjectValue;
  MatchIndex, MatchEnd, NextIndex: Integer;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  if (AThisValue is TGocciaUndefinedLiteralValue) or
     (AThisValue is TGocciaNullLiteralValue) then
    ThrowTypeError(SErrorStringPrototypeRequiresNonNullish, SSuggestCheckNullBeforeAccess);

  if AArgs.Length > 0 then
    SearchArg := AArgs.GetElement(0)
  else
    SearchArg := TGocciaUndefinedLiteralValue.UndefinedValue;

  if AArgs.Length > 1 then
    ReplaceArg := AArgs.GetElement(1)
  else
    ReplaceArg := TGocciaUndefinedLiteralValue.UndefinedValue;

  // §22.1.3.20 step 2a–b: IsRegExp check; RequireObjectCoercible(flags); 'g' check
  if IsRegExp(SearchArg) then
  begin
    FlagsVal := TGocciaObjectValue(SearchArg).GetProperty(PROP_FLAGS);
    if (FlagsVal is TGocciaUndefinedLiteralValue) or
       (FlagsVal is TGocciaNullLiteralValue) then
      ThrowTypeError(SErrorCannotConvertNullOrUndefined, SSuggestObjectArgType);
    if Pos('g', FlagsVal.ToStringLiteral.Value) = 0 then
      ThrowTypeError(SErrorReplaceAllRequiresGlobalRegExp, SSuggestReplaceAllGlobalFlag);
  end;

  ReplaceMethod := GetMethodBySymbol(SearchArg,
    TGocciaSymbolValue.WellKnownReplace);
  if not (ReplaceMethod is TGocciaUndefinedLiteralValue) then
  begin
    if not ReplaceMethod.IsCallable then
      ThrowTypeError(SErrorSymbolReplaceNotCallable, SSuggestWellKnownSymbolCallable);
    // ES2026 §22.1.3.20 step 2.d.i: Call(replacer, searchValue, « O, replaceValue »).
    // The replacer receives the original `this` (O), not a stringified copy —
    // ToString(O) only happens in step 3 if there is no @@replace replacer.
    CallArgs := TGocciaArgumentsCollection.Create([
      AThisValue,
      ReplaceArg
    ]);
    try
      Result := InvokeCallable(ReplaceMethod, CallArgs, SearchArg);
    finally
      CallArgs.Free;
    end;
    Exit;
  end;

  // Step 3 (spec): Let string be ? ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // §22.1.3.20 step 7: Let searchString be ? ToString(searchValue).
  SearchValue := SearchArg.ToStringLiteral.Value;
  SearchLength := UTF16CodeUnitLength(SearchValue);
  StringLength := UTF16CodeUnitLength(StringValue);

  // Step 5: Let searchLength be the length of searchString
  // Step 6: Find all occurrences of searchString in string
  // Step 7: For each occurrence, compute replacement and build result
  if ReplaceArg.IsCallable then
  begin
    ResultStr := '';
    if SearchValue = '' then
    begin
      Offset := 0;
      CallArgs := TGocciaArgumentsCollection.Create([
        TGocciaStringLiteralValue.Create(''),
        nil,
        TGocciaStringLiteralValue.Create(StringValue)
      ]);
      try
        while Offset <= StringLength do
        begin
          CallArgs.SetElement(1, TGocciaNumberLiteralValue.Create(Offset));
          CallResult := InvokeCallable(ReplaceArg, CallArgs,
            TGocciaUndefinedLiteralValue.UndefinedValue);
          ResultStr := ResultStr + CallResult.ToStringLiteral.Value;
          if Offset = StringLength then
            Break;
          NextOffset := Offset + 1;
          ResultStr := ResultStr + UTF16Substring(StringValue, Offset,
            NextOffset - Offset);
          Offset := NextOffset;
        end;
      finally
        CallArgs.Free;
      end;
      Result := TGocciaStringLiteralValue.Create(ResultStr);
      Exit;
    end;

    Offset := 0;
    SearchPos := UTF16IndexOf(StringValue, SearchValue, Offset);
    if SearchPos < 0 then
    begin
      Result := TGocciaStringLiteralValue.Create(StringValue);
      Exit;
    end;

    CallArgs := TGocciaArgumentsCollection.Create([nil, nil, TGocciaStringLiteralValue.Create(StringValue)]);
    try
      while SearchPos >= 0 do
      begin
        ResultStr := ResultStr + UTF16Substring(StringValue, Offset,
          SearchPos - Offset);
        CallArgs.SetElement(0, TGocciaStringLiteralValue.Create(SearchValue));
        CallArgs.SetElement(1, TGocciaNumberLiteralValue.Create(SearchPos));
        CallResult := InvokeCallable(ReplaceArg, CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
        ResultStr := ResultStr + CallResult.ToStringLiteral.Value;
        Offset := SearchPos + SearchLength;
        if Offset > StringLength then
          Break;
        SearchPos := UTF16IndexOf(StringValue, SearchValue, Offset);
      end;
      if Offset <= StringLength then
        ResultStr := ResultStr + UTF16Substring(StringValue, Offset,
          StringLength - Offset);
    finally
      CallArgs.Free;
    end;
    Result := TGocciaStringLiteralValue.Create(ResultStr);
  end
  else
  begin
    // Step 7 (non-callable): Let replaceStr be ToString(replaceValue), replace all occurrences
    ReplaceValue := ReplaceArg.ToStringLiteral.Value;
    ResultStr := '';
    if SearchValue = '' then
    begin
      Offset := 0;
      while Offset <= StringLength do
      begin
        ResultStr := ResultStr + ExpandReplacementPattern(ReplaceValue,
          SearchValue, StringValue, Offset, [], []);
        if Offset = StringLength then
          Break;
        NextOffset := Offset + 1;
        ResultStr := ResultStr + UTF16Substring(StringValue, Offset,
          NextOffset - Offset);
        Offset := NextOffset;
      end;
      Result := TGocciaStringLiteralValue.Create(ResultStr);
      Exit;
    end;

    Offset := 0;
    SearchPos := UTF16IndexOf(StringValue, SearchValue, Offset);
    if SearchPos < 0 then
    begin
      Result := TGocciaStringLiteralValue.Create(StringValue);
      Exit;
    end;

    while SearchPos >= 0 do
    begin
      MatchStart := SearchPos;
      ResultStr := ResultStr + UTF16Substring(StringValue, Offset,
        MatchStart - Offset);
      ResultStr := ResultStr + ExpandReplacementPattern(ReplaceValue,
        SearchValue, StringValue, MatchStart, [], []);
      Offset := MatchStart + SearchLength;
      if Offset > StringLength then
        Break;
      SearchPos := UTF16IndexOf(StringValue, SearchValue, Offset);
    end;
    if Offset <= StringLength then
      ResultStr := ResultStr + UTF16Substring(StringValue, Offset,
        StringLength - Offset);
    Result := TGocciaStringLiteralValue.Create(ResultStr);
  end;
end;

// ES2026 §22.1.3.23 String.prototype.split(separator, limit)
function TGocciaStringObjectValue.StringSplit(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, Separator: string;
  SeparatorArg: TGocciaValue;
  ResultArray: TGocciaArrayValue;
  I: Integer;
  PreviousSeparatorEnd: Integer;
  SearchStart: Integer;
  SeparatorLength: Integer;
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
  // ES2026 §22.1.3.23 String.prototype.split:
  //   1. RequireObjectCoercible(this).
  //   2. If separator is non-null/undefined, GetMethod(separator, @@split)
  //      and dispatch through it BEFORE any ToString on O.
  //   3. ToString(O); fall back to the regular path.
  if (AThisValue is TGocciaUndefinedLiteralValue) or
     (AThisValue is TGocciaNullLiteralValue) then
    ThrowTypeError(SErrorStringPrototypeRequiresNonNullish, SSuggestCheckNullBeforeAccess);

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
        ThrowTypeError(SErrorSymbolSplitNotCallable, SSuggestWellKnownSymbolCallable);
      // Spec passes O directly to the @@split callable, not a stringified copy.
      if HasLimit then
        CallArgs := TGocciaArgumentsCollection.Create([
          AThisValue,
          AArgs.GetElement(1)
        ])
      else
        CallArgs := TGocciaArgumentsCollection.Create([
          AThisValue
        ]);
      try
        Result := InvokeCallable(SplitMethod, CallArgs, SeparatorArg);
      finally
        CallArgs.Free;
      end;
      Exit;
    end;

    // Step 3 (spec): Let S be ? ToString(O). Only reached when no @@split.
    StringValue := ExtractStringValue(AThisValue);

    // §22.1.3.23 step 5: Let R be ? ToString(separator).
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
      for I := 0 to UTF16CodeUnitLength(StringValue) - 1 do
      begin
        ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(
          UTF16CodeUnitAt(StringValue, I)));
        if HasLimit and (Cardinal(ResultArray.Elements.Count) >= Limit) then
          Break;
      end;
      Result := ResultArray;
      Exit;
    end;

    // Step 7: Split by occurrences of sep, respecting limit
    if UTF16IndexOf(StringValue, Separator) = -1 then
      ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(StringValue))
    else
    begin
      SeparatorLength := UTF16CodeUnitLength(Separator);
      PreviousSeparatorEnd := 0;
      SearchStart := 0;
      while True do
      begin
        SeparatorPos := UTF16IndexOf(StringValue, Separator, SearchStart);
        if SeparatorPos = -1 then
        begin
          ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(
            UTF16Substring(StringValue, PreviousSeparatorEnd,
            UTF16CodeUnitLength(StringValue) - PreviousSeparatorEnd)));
          Break;
        end
        else
        begin
          Segment := UTF16Substring(StringValue, PreviousSeparatorEnd,
            SeparatorPos - PreviousSeparatorEnd);
          ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(Segment));

          if HasLimit and (Cardinal(ResultArray.Elements.Count) >= Limit) then
            Break;

          PreviousSeparatorEnd := SeparatorPos + SeparatorLength;
          SearchStart := PreviousSeparatorEnd;
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
  // ES2026 §22.1.3.15 String.prototype.match:
  //   1. RequireObjectCoercible(this).
  //   2. If regexp is non-null/undefined, GetMethod(regexp, @@match) and
  //      dispatch through it BEFORE any ToString on O.
  //   3. ToString(O); fall back to RegExp coercion.
  if (AThisValue is TGocciaUndefinedLiteralValue) or
     (AThisValue is TGocciaNullLiteralValue) then
    ThrowTypeError(SErrorStringPrototypeRequiresNonNullish, SSuggestCheckNullBeforeAccess);

  if AArgs.Length > 0 then
  begin
    MatchMethod := GetMethodBySymbol(AArgs.GetElement(0),
      TGocciaSymbolValue.WellKnownMatch);
    if not (MatchMethod is TGocciaUndefinedLiteralValue) then
    begin
      if not MatchMethod.IsCallable then
        ThrowTypeError(SErrorSymbolMatchNotCallable, SSuggestWellKnownSymbolCallable);
      // Spec passes O directly to the @@match callable, not a stringified copy.
      CallArgs := TGocciaArgumentsCollection.Create([
        AThisValue
      ]);
      try
        Result := InvokeCallable(MatchMethod, CallArgs, AArgs.GetElement(0));
      finally
        CallArgs.Free;
      end;
      Exit;
    end;
  end;

  // Step 3 (spec): Let S be ? ToString(O). Only reached when no @@match.
  StringValue := ExtractStringValue(AThisValue);

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
  RegExpArg, FlagsVal, MatchAllMethod: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  RegexValue: TGocciaObjectValue;
begin
  // ES2026 §22.1.3.16 String.prototype.matchAll:
  //   1. RequireObjectCoercible(this).
  //   2. If regexp is non-null/undefined and is a RegExp, validate global flag.
  //   3. GetMethod(regexp, @@matchAll); if defined, dispatch through it
  //      BEFORE any ToString on O.
  //   4. ToString(O); fall back to RegExp coercion.
  if (AThisValue is TGocciaUndefinedLiteralValue) or
     (AThisValue is TGocciaNullLiteralValue) then
    ThrowTypeError(SErrorStringPrototypeRequiresNonNullish, SSuggestCheckNullBeforeAccess);

  // §22.1.3.13 step 2a–b: IsRegExp check; RequireObjectCoercible(flags); 'g' check
  if AArgs.Length > 0 then
  begin
    RegExpArg := AArgs.GetElement(0);
    if IsRegExp(RegExpArg) then
    begin
      FlagsVal := TGocciaObjectValue(RegExpArg).GetProperty(PROP_FLAGS);
      if (FlagsVal is TGocciaUndefinedLiteralValue) or
         (FlagsVal is TGocciaNullLiteralValue) then
        ThrowTypeError(SErrorCannotConvertNullOrUndefined, SSuggestObjectArgType);
      if Pos('g', FlagsVal.ToStringLiteral.Value) = 0 then
        ThrowTypeError(SErrorMatchAllRequiresGlobalRegExp, SSuggestReplaceAllGlobalFlag);
    end;

    MatchAllMethod := GetMethodBySymbol(RegExpArg,
      TGocciaSymbolValue.WellKnownMatchAll);
    if not (MatchAllMethod is TGocciaUndefinedLiteralValue) then
    begin
      if not MatchAllMethod.IsCallable then
        ThrowTypeError(SErrorSymbolMatchAllNotCallable, SSuggestWellKnownSymbolCallable);
      // Spec passes O directly to the @@matchAll callable, not a stringified copy.
      CallArgs := TGocciaArgumentsCollection.Create([
        AThisValue
      ]);
      try
        Result := InvokeCallable(MatchAllMethod, CallArgs, RegExpArg);
      finally
        CallArgs.Free;
      end;
      Exit;
    end;
  end;

  // Step 4 (spec): Let S be ? ToString(O). Only reached when no @@matchAll.
  StringValue := ExtractStringValue(AThisValue);

  if AArgs.Length > 0 then
    RegexValue := CoerceRegExpValue(RegExpArg, 'g')
  else
    RegexValue := CoerceRegExpValue(TGocciaUndefinedLiteralValue.UndefinedValue,
      'g');

  Result := TGocciaRegExpMatchAllIteratorValue.Create(RegexValue, StringValue,
    True, HasUnicodeRegExpFlag(RegexValue.GetProperty(PROP_FLAGS)
    .ToStringLiteral.Value));
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
  // ES2026 §22.1.3.27 String.prototype.search:
  //   1. RequireObjectCoercible(this).
  //   2. If regexp is non-null/undefined, GetMethod(regexp, @@search) and
  //      dispatch through it BEFORE any ToString on O.
  //   3. ToString(O); fall back to RegExp coercion.
  if (AThisValue is TGocciaUndefinedLiteralValue) or
     (AThisValue is TGocciaNullLiteralValue) then
    ThrowTypeError(SErrorStringPrototypeRequiresNonNullish, SSuggestCheckNullBeforeAccess);

  if AArgs.Length > 0 then
  begin
    SearchMethod := GetMethodBySymbol(AArgs.GetElement(0),
      TGocciaSymbolValue.WellKnownSearch);
    if not (SearchMethod is TGocciaUndefinedLiteralValue) then
    begin
      if not SearchMethod.IsCallable then
        ThrowTypeError(SErrorSymbolSearchNotCallable, SSuggestWellKnownSymbolCallable);
      // Spec passes O directly to the @@search callable, not a stringified copy.
      CallArgs := TGocciaArgumentsCollection.Create([
        AThisValue
      ]);
      try
        Result := InvokeCallable(SearchMethod, CallArgs, AArgs.GetElement(0));
      finally
        CallArgs.Free;
      end;
      Exit;
    end;
  end;

  // Step 3 (spec): Let S be ? ToString(O). Only reached when no @@search.
  StringValue := ExtractStringValue(AThisValue);

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
    ThrowRangeError(Format(SErrorInvalidRepeatCount, [CountValue.ToStringLiteral.Value]), SSuggestRepeatCountRange);

  // Step 5: If n = 0, return ""
  // Step 6: Return S repeated n times
  Count := Trunc(CountValue.Value);
  Result := TGocciaStringLiteralValue.Create(DupeString(StringValue, Count));
end;

// ES2026 §22.1.3.15 String.prototype.padStart(maxLength [, fillString])
function TGocciaStringObjectValue.StringPadStart(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, PadString, Padding: string;
  StringLength, TargetLength, PadNeeded: Integer;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let intMaxLength be ToLength(maxLength)
  TargetLength := ToIntegerFromArgs(AArgs);

  // Step 4: If intMaxLength <= len(S), return S
  StringLength := UTF16CodeUnitLength(StringValue);
  if StringLength >= TargetLength then
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
  PadNeeded := TargetLength - StringLength;
  Padding := '';
  while UTF16CodeUnitLength(Padding) < PadNeeded do
    Padding := Padding + PadString;
  Padding := UTF16Substring(Padding, 0, PadNeeded);

  Result := TGocciaStringLiteralValue.Create(Padding + StringValue);
end;

// ES2026 §22.1.3.14 String.prototype.padEnd(maxLength [, fillString])
function TGocciaStringObjectValue.StringPadEnd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, PadString, Padding: string;
  StringLength, TargetLength, PadNeeded: Integer;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let intMaxLength be ToLength(maxLength)
  TargetLength := ToIntegerFromArgs(AArgs);

  // Step 4: If intMaxLength <= len(S), return S
  StringLength := UTF16CodeUnitLength(StringValue);
  if StringLength >= TargetLength then
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
  PadNeeded := TargetLength - StringLength;
  Padding := '';
  while UTF16CodeUnitLength(Padding) < PadNeeded do
    Padding := Padding + PadString;
  Padding := UTF16Substring(Padding, 0, PadNeeded);

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
      ThrowTypeError(SErrorSymbolToString, SSuggestSymbolNoImplicitConversion);
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
    Index := UTF16CodeUnitLength(StringValue) + Index;

  // Step 6: If k < 0 or k >= len, return undefined
  if (Index < 0) or (Index >= UTF16CodeUnitLength(StringValue)) then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  // Step 7: Return the substring of S from k to k + 1
  Result := TGocciaStringLiteralValue.Create(UTF16CodeUnitAt(StringValue, Index));
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
  CodePoint: Cardinal;
  Index: Integer;
begin
  // Step 1: Let O be RequireObjectCoercible(this value)
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);

  // Step 3: Let position be ToIntegerOrInfinity(pos)
  Index := ToIntegerFromArgs(AArgs);

  // Step 4: If position < 0 or position >= len(S), return undefined
  if (Index < 0) or (Index >= UTF16CodeUnitLength(StringValue)) then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  // Step 5: Let cp be CodePointAt(S, position) and return cp.[[CodePoint]]
  if TryUTF16CodePointValueAt(StringValue, Index, CodePoint) then
    Result := TGocciaNumberLiteralValue.Create(CodePoint)
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §22.1.3.11 String.prototype.localeCompare(that)
function TGocciaStringObjectValue.StringLocaleCompare(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue, ThatString, Locale: string;
  Options: TGocciaObjectValue;
  Collator: TGocciaIntlCollatorValue;
begin
  StringValue := ExtractStringValue(AThisValue);

  if AArgs.Length = 0 then
    ThatString := 'undefined'
  else
    ThatString := AArgs.GetElement(0).ToStringLiteral.Value;

  Locale := '';
  if (AArgs.Length > 1) and not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
    Locale := LocaleCompareArgumentToLocale(AArgs.GetElement(1));

  Options := nil;
  if (AArgs.Length > 2) and not (AArgs.GetElement(2) is TGocciaUndefinedLiteralValue) then
    Options := ToObject(AArgs.GetElement(2));

  Collator := TGocciaIntlCollatorValue.Create(Locale, Options);
  Result := TGocciaNumberLiteralValue.Create(Collator.CompareStrings(StringValue, ThatString));
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
    ThrowRangeError(SErrorInvalidNormalizationForm, SSuggestNormalizationForm);

  // Step 5: Return the Unicode Normalization Form f of S
  Result := TGocciaStringLiteralValue.Create(StringValue);
end;

// ES2026 §22.1.3.8 String.prototype.isWellFormed()
function TGocciaStringObjectValue.StringIsWellFormed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // ES2026 §22.1.3.8 step 1: Let O be RequireObjectCoercible(this value)
  if (AThisValue is TGocciaUndefinedLiteralValue) or (AThisValue is TGocciaNullLiteralValue) then
    ThrowTypeError(SErrorIsWellFormedRequiresNonNullish, SSuggestCheckNullBeforeAccess);
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);
  // Step 3: Return IsStringWellFormedUnicode(S)
  Result := TGocciaBooleanLiteralValue.Create(IsWellFormedUTF8(StringValue));
end;

// ES2026 §22.1.3.33 String.prototype.toWellFormed()
function TGocciaStringObjectValue.StringToWellFormed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  StringValue: string;
begin
  // ES2026 §22.1.3.33 step 1: Let O be RequireObjectCoercible(this value)
  if (AThisValue is TGocciaUndefinedLiteralValue) or (AThisValue is TGocciaNullLiteralValue) then
    ThrowTypeError(SErrorToWellFormedRequiresNonNullish, SSuggestCheckNullBeforeAccess);
  // Step 2: Let S be ToString(O)
  StringValue := ExtractStringValue(AThisValue);
  // Step 3-5: Replace ill-formed Unicode sequences with U+FFFD.
  Result := TGocciaStringLiteralValue.Create(ToWellFormedUTF8(StringValue));
end;

initialization
  GStringPrototypeSlot := RegisterRealmSlot('String.prototype');

end.
