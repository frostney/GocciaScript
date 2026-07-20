unit Goccia.Builtins.JSON;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  UnicodeStringList,

  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.JSON,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.RawJSON;

type
  TGocciaJSONBuiltin = class(TGocciaBuiltin)
  private
    FParser: TGocciaJSONParser;
    FReplacerTraversalStack: TList<TGocciaObjectValue>;
    FStringifier: TGocciaJSONStringifier;

    function ApplyReviver(const AHolder: TGocciaValue; const AKey: string;
      const AReviver: TGocciaValue; const AParseRecord: TGocciaJSONParseRecord): TGocciaValue;
    function ApplyReplacer(const AHolder: TGocciaValue; const AKey: string; const AValue: TGocciaValue; const AReplacer: TGocciaValue): TGocciaValue;
    function ApplyToJSON(const AValue: TGocciaValue; const AKey: string): TGocciaValue;
    function ResolveGap(const ASpaceArg: TGocciaValue): string;
    function TransformWithReplacer(const AHolder: TGocciaValue; const AKey: string; const AValue: TGocciaValue; const AReplacer: TGocciaValue): TGocciaValue;
    function StringifyWithReplacer(const AValue: TGocciaValue; const AReplacer: TGocciaValue; const AGap: string): string;
    function StringifyWithAllowList(const AValue: TGocciaValue; const AAllowList: TGocciaObjectValue; const AGap: string): string;
  protected
  published
    function JSONIsRawJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function JSONParse(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function JSONRawJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function JSONStringify(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  SysUtils,

  TextSemantics,

  Goccia.Arithmetic,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.ThreadCleanupRegistry,
  Goccia.Utils,
  Goccia.Values.BigIntValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.HoleValue,
  Goccia.Values.NumberObjectValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ProxyValue,
  Goccia.Values.StringObjectValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToObject,
  Goccia.Values.WrapperPrimitives,
  Goccia.VM.Exception;

threadvar
  FStaticMembers: TArray<TGocciaMemberDefinition>;

procedure ClearThreadvarMembers;
begin
  SetLength(FStaticMembers, 0);
end;

constructor TGocciaJSONBuiltin.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  Members: TGocciaMemberCollection;
begin
  inherited Create(AName, AScope, AThrowError);

  FParser := TGocciaJSONParser.Create;
  FStringifier := TGocciaJSONStringifier.Create;
  FReplacerTraversalStack := TList<TGocciaObjectValue>.Create;

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddMethod(JSONIsRawJSON, 1, gmkStaticMethod);
    Members.AddMethod(JSONParse, 2, gmkStaticMethod);
    Members.AddMethod(JSONRawJSON, 1, gmkStaticMethod);
    Members.AddMethod(JSONStringify, 3, gmkStaticMethod);
    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create('JSON'),
      [pfConfigurable]);
    FStaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(FBuiltinObject, FStaticMembers);

  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet, True);
end;

destructor TGocciaJSONBuiltin.Destroy;
begin
  FParser.Free;
  FStringifier.Free;
  FReplacerTraversalStack.Free;
  inherited;
end;

function IsJSONObjectArray(const AValue: TGocciaValue): Boolean;
var
  Current: TGocciaValue;
  Proxy: TGocciaProxyValue;
begin
  Current := AValue;
  while Current is TGocciaProxyValue do
  begin
    Proxy := TGocciaProxyValue(Current);
    if Proxy.Revoked then
      ThrowTypeError(SErrorProxyRevoked, SSuggestProxyRevoked);
    Current := Proxy.Target;
  end;
  Result := Current is TGocciaArrayValue;
end;

function ChildRecordForIndex(const AParseRecord: TGocciaJSONParseRecord;
  const AIndex: Integer): TGocciaJSONParseRecord;
begin
  if Assigned(AParseRecord) then
    Result := AParseRecord.ElementAt(AIndex)
  else
    Result := nil;
end;

function ChildRecordForKey(const AParseRecord: TGocciaJSONParseRecord;
  const AKey: string): TGocciaJSONParseRecord;
begin
  if Assigned(AParseRecord) then
    Result := AParseRecord.EntryForKey(AKey)
  else
    Result := nil;
end;

// ES2026 §25.5.2.4 InternalizeJSONProperty ( holder, name, reviver, parseRecord )
function TGocciaJSONBuiltin.ApplyReviver(const AHolder: TGocciaValue; const AKey: string;
  const AReviver: TGocciaValue; const AParseRecord: TGocciaJSONParseRecord): TGocciaValue;
var
  Value, NewValue: TGocciaValue;
  Obj: TGocciaObjectValue;
  I: Integer;
  Len: Integer;
  PropKey: string;
  Keys: TArray<string>;
  Args: TGocciaArgumentsCollection;
  Context: TGocciaObjectValue;
begin
  // Step 1: Let val be ? Get(holder, name).
  Value := AHolder.GetProperty(AKey);

  // Step 2: If val is an Object, recurse into its properties.
  if Value is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(Value);
    if IsJSONObjectArray(Obj) then
    begin
      // Step 2a: If val is an Array, for each index I from 0 to len-1.
      Len := LengthOfArrayLike(Obj);
      for I := 0 to Len - 1 do
      begin
        // Step 2a.i: Let newElement be ? InternalizeJSONProperty(val, ToString(I), reviver).
        NewValue := ApplyReviver(Obj, IntToStr(I), AReviver,
          ChildRecordForIndex(AParseRecord, I));
        // Step 2a.ii: If newElement is undefined, delete the element; else create a data property.
        if NewValue is TGocciaUndefinedLiteralValue then
          Obj.DeleteProperty(IntToStr(I))
        else
          Obj.TryCreateDataProperty(IntToStr(I), NewValue);
      end;
    end
    else
    begin
      // Step 2b: Else val is an ordinary Object, for each enumerable key:
      Keys := Obj.GetEnumerablePropertyNames;
      for PropKey in Keys do
      begin
        // Step 2b.i: Let newElement be ? InternalizeJSONProperty(val, P, reviver).
        NewValue := ApplyReviver(Obj, PropKey, AReviver,
          ChildRecordForKey(AParseRecord, PropKey));
        // Step 2b.ii: If newElement is undefined, delete property; else create a data property.
        if NewValue is TGocciaUndefinedLiteralValue then
          Obj.DeleteProperty(PropKey)
        else
          Obj.TryCreateDataProperty(PropKey, NewValue);
      end;
    end;
  end;

  // ES2026 §25.5.2.4 step 3: Build the context object for source text access.
  Context := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  if Assigned(AParseRecord) and not (Value is TGocciaObjectValue) and
    (AParseRecord.SourceText <> '') and IsSameValue(AParseRecord.Value, Value) then
    Context.CreateDataPropertyOrThrow(PROP_SOURCE,
      TGocciaStringLiteralValue.Create(AParseRecord.SourceText));

  // Step 4: Return ? Call(reviver, holder, « name, val, context »).
  Args := TGocciaArgumentsCollection.CreateWithCapacity(3);
  try
    Args.Add(TGocciaStringLiteralValue.Create(AKey));
    Args.Add(Value);
    Args.Add(Context);
    try
      Result := InvokeCallable(AReviver, Args, AHolder);
    except
      on E: EGocciaBytecodeThrow do
        ReraiseBytecodeThrow(E);
    end;
  finally
    Args.Free;
  end;
end;

// ES2026 §25.5.1 JSON.isRawJSON ( O )
function TGocciaJSONBuiltin.JSONIsRawJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Value: TGocciaValue;
begin
  // No arity enforcement per spec — missing argument is treated as undefined.
  if AArgs.Length >= 1 then
    Value := AArgs.GetElement(0)
  else
    Value := TGocciaUndefinedLiteralValue.UndefinedValue;
  // Step 1: If Type(O) is Object and O has an [[IsRawJSON]] internal slot, return true.
  // Step 2: Return false.
  if Value is TGocciaRawJSONValue then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §25.5.2 JSON.parse ( text [ , reviver ] )
function TGocciaJSONBuiltin.JSONParse(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  HasReviver: Boolean;
  JSONString: string;
  ParseRecord: TGocciaJSONParseRecord;
  Reviver: TGocciaValue;
  Root: TGocciaObjectValue;
  TextValue: TGocciaValue;
begin
  // ES2026 §25.5.2 step 1: Let jsonString be ? ToString(text).
  if AArgs.Length >= 1 then
    TextValue := AArgs.GetElement(0)
  else
    TextValue := TGocciaUndefinedLiteralValue.UndefinedValue;
  JSONString := TextValue.ToStringLiteral.Value;

  // Step 2-3: Parse and optionally apply the reviver with source text access.
  HasReviver := (AArgs.Length >= 2) and AArgs.GetElement(1).IsCallable;

  if HasReviver then
  begin
    Reviver := AArgs.GetElement(1);
    ParseRecord := nil;
    try
      // Step 2: Parse with parse-record tracking for the reviver context.
      try
        FParser.ParseWithRecord(JSONString, Result, ParseRecord);
      except
        on E: Exception do
          ThrowSyntaxError(E.Message, SSuggestJSONFormat);
      end;

      // Steps 5-7: Wrap in root object and create the empty-string root data property.
      Root := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
      Root.CreateDataPropertyOrThrow('', Result);

      Result := ApplyReviver(Root, '', Reviver, ParseRecord);
    finally
      ParseRecord.Free;
    end;
  end
  else
  begin
    // No reviver — parse without source text collection.
    try
      Result := FParser.Parse(JSONString);
    except
      on E: Exception do
        ThrowSyntaxError(E.Message, SSuggestJSONFormat);
    end;
  end;
end;

// ES2026 §25.5.3 JSON.rawJSON ( text )
function TGocciaJSONBuiltin.JSONRawJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
const
  JSON_WHITESPACE_TAB = #9;
  JSON_WHITESPACE_LF = #10;
  JSON_WHITESPACE_CR = #13;
  JSON_WHITESPACE_SPACE = #32;
var
  TextValue: TGocciaValue;
  JSONString: string;
  Parsed: TGocciaValue;
  FirstChar, LastChar: Char;
begin
  // Step 1: Let jsonString be ? ToString(text).
  // No arity enforcement per spec — missing argument is treated as undefined.
  if AArgs.Length >= 1 then
    TextValue := AArgs.GetElement(0)
  else
    TextValue := TGocciaUndefinedLiteralValue.UndefinedValue;
  JSONString := TextValue.ToStringLiteral.Value;

  // Step 2: Throw a SyntaxError if jsonString is the empty string.
  if JSONString = '' then
    ThrowSyntaxError(SErrorJSONRawJSONEmptyString, SSuggestJSONFormat);

  // Step 2 (continued): Throw a SyntaxError if the first or last code unit is whitespace.
  FirstChar := JSONString[1];
  LastChar := JSONString[Length(JSONString)];
  if (FirstChar = JSON_WHITESPACE_TAB) or (FirstChar = JSON_WHITESPACE_LF) or
    (FirstChar = JSON_WHITESPACE_CR) or (FirstChar = JSON_WHITESPACE_SPACE) then
    ThrowSyntaxError(SErrorJSONRawJSONLeadingWhitespace, SSuggestJSONFormat);
  if (LastChar = JSON_WHITESPACE_TAB) or (LastChar = JSON_WHITESPACE_LF) or
    (LastChar = JSON_WHITESPACE_CR) or (LastChar = JSON_WHITESPACE_SPACE) then
    ThrowSyntaxError(SErrorJSONRawJSONTrailingWhitespace, SSuggestJSONFormat);

  // Step 3: Parse jsonString as a JSON text. Throw SyntaxError if invalid.
  try
    Parsed := FParser.Parse(JSONString);
  except
    on E: Exception do
      ThrowSyntaxError(Format(SErrorJSONRawJSONInvalid, [E.Message]), SSuggestJSONFormat);
  end;

  // Step 3 (continued): Throw SyntaxError if outermost value is object or array.
  if (Parsed is TGocciaObjectValue) then
    ThrowSyntaxError(SErrorJSONRawJSONMustBePrimitive, SSuggestJSONFormat);

  // Steps 4-7: Create frozen object with null prototype, [[IsRawJSON]], and "rawJSON" property.
  Result := TGocciaRawJSONValue.Create(JSONString);
end;

// ES2026 §25.5.4 (sec-json.stringify) steps 6-9: Resolve the gap (indentation)
// string from the space argument.
function TGocciaJSONBuiltin.ResolveGap(const ASpaceArg: TGocciaValue): string;
var
  Space: TGocciaValue;
  SpaceNumber: Double;
  SpaceCount: Integer;
begin
  Result := '';
  // Step 6: If space is an Object with a [[NumberData]] slot, set space to
  // ? ToNumber(space); with a [[StringData]] slot, to ? ToString(space).
  // A boxed Boolean unwraps to a boolean primitive, which matches neither
  // branch below — same empty gap as leaving it boxed.
  Space := CoerceWrappedPrimitive(ASpaceArg);
  // Step 7: If space is a Number, let gap be min(10, ToIntegerOrInfinity(space))
  // spaces. Clamp before Trunc so NaN, ±Infinity, and doubles beyond Integer
  // range never reach Trunc.
  if Space is TGocciaNumberLiteralValue then
  begin
    SpaceNumber := Space.ToNumberLiteral.Value;
    if Math.IsNaN(SpaceNumber) or (SpaceNumber < 1) then
      Exit;
    if SpaceNumber > 10 then
      SpaceCount := 10
    else
      SpaceCount := Trunc(SpaceNumber);
    Result := StringOfChar(' ', SpaceCount);
  end
  // Step 8: Else if space is a String, let gap be the first 10 code units.
  // Length() counts bytes, so it can only overestimate the code-unit count;
  // UTF16Substring then truncates without splitting a UTF-8 sequence.
  else if Space is TGocciaStringLiteralValue then
  begin
    Result := Space.ToStringLiteral.Value;
    if Length(Result) > 10 then
      Result := UTF16Substring(Result, 0, 10);
  end;
  // Step 9: Else let gap be the empty string (already default).
end;

// ES2026 §25.5.4.2 SerializeJSONProperty ( state, key, holder ) step 2: toJSON hook.
function TGocciaJSONBuiltin.ApplyToJSON(const AValue: TGocciaValue; const AKey: string): TGocciaValue;
var
  ToJSONMethod: TGocciaValue;
  Args: TGocciaArgumentsCollection;
begin
  Result := AValue;
  if not ((AValue is TGocciaObjectValue) or (AValue is TGocciaBigIntValue)) then
    Exit;

  ToJSONMethod := AValue.GetProperty(PROP_TO_JSON);
  if not Assigned(ToJSONMethod) or not ToJSONMethod.IsCallable then
    Exit;

  Args := TGocciaArgumentsCollection.CreateWithCapacity(1);
  try
    Args.Add(TGocciaStringLiteralValue.Create(AKey));
    Result := InvokeCallable(ToJSONMethod, Args, AValue);
  finally
    Args.Free;
  end;
end;

// §25.5.4.2 SerializeJSONProperty — step 3: Call replacer function.
// Invokes the replacer with (key, value) bound to the holder object.
function TGocciaJSONBuiltin.ApplyReplacer(const AHolder: TGocciaValue; const AKey: string; const AValue: TGocciaValue; const AReplacer: TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
begin
  // Step 2: Let val be ? Call(state.ReplacerFunction, holder, « key, val »).
  Args := TGocciaArgumentsCollection.Create;
  Args.Add(TGocciaStringLiteralValue.Create(AKey));
  Args.Add(AValue);
  Result := InvokeCallable(AReplacer, Args, AHolder);
end;

// §25.5.4.2 SerializeJSONProperty ( state, key, holder ) — recursive transformation.
// Applies the replacer function then recursively transforms nested objects/arrays.
function TGocciaJSONBuiltin.TransformWithReplacer(const AHolder: TGocciaValue; const AKey: string; const AValue: TGocciaValue; const AReplacer: TGocciaValue): TGocciaValue;
var
  Replaced, PropValue, TransformedProp: TGocciaValue;
  Obj: TGocciaObjectValue;
  NewObj: TGocciaObjectValue;
  NewArr: TGocciaArrayValue;
  Key: string;
  I: Integer;
  Len: Integer;
begin
  // Step 1: Apply a toJSON hook before the replacer sees the value.
  Replaced := ApplyToJSON(AValue, AKey);

  // Step 2: Call the replacer to get the transformed value.
  Replaced := ApplyReplacer(AHolder, AKey, Replaced, AReplacer);

  // Step 3: If replacer returned undefined, signal omission.
  if Replaced is TGocciaUndefinedLiteralValue then
  begin
    Result := Replaced;
    Exit;
  end;

  // ES2026 §25.5.4.2 step 4.a: If result has [[IsRawJSON]], pass through directly.
  if Replaced is TGocciaRawJSONValue then
  begin
    Result := Replaced;
    Exit;
  end;

  // Steps 4.b-4.d: unwrap boxed primitives.
  Replaced := CoerceWrappedPrimitive(Replaced);

  if Replaced.IsCallable or (Replaced is TGocciaSymbolValue) then
  begin
    Result := Replaced;
    Exit;
  end;

  // Step 4: If result is an Array, recursively serialize each element (SerializeJSONArray).
  if (Replaced is TGocciaObjectValue) and IsJSONObjectArray(Replaced) then
  begin
    Obj := TGocciaObjectValue(Replaced);
    if FReplacerTraversalStack.IndexOf(Obj) <> -1 then
      ThrowTypeError(SErrorJSONCircularStructure, SSuggestJSONFormat);

    FReplacerTraversalStack.Add(Obj);
    NewArr := TGocciaArrayValue.Create;
    try
      Len := LengthOfArrayLike(Obj);
      for I := 0 to Len - 1 do
      begin
        PropValue := Obj.GetProperty(IntToStr(I));
        TransformedProp := TransformWithReplacer(Obj, IntToStr(I), PropValue, AReplacer);
        NewArr.Elements.Add(TransformedProp);
      end;
    finally
      FReplacerTraversalStack.Delete(FReplacerTraversalStack.Count - 1);
    end;
    Result := NewArr;
  end
  // Step 5: If result is an Object, recursively serialize each property (SerializeJSONObject).
  else if Replaced is TGocciaObjectValue then
  begin
    if FReplacerTraversalStack.IndexOf(TGocciaObjectValue(Replaced)) <> -1 then
      ThrowTypeError(SErrorJSONCircularStructure, SSuggestJSONFormat);

    FReplacerTraversalStack.Add(TGocciaObjectValue(Replaced));
    Obj := TGocciaObjectValue(Replaced);
    NewObj := TGocciaObjectValue.Create;
    try
      for Key in Obj.GetEnumerablePropertyNames do
      begin
        PropValue := Obj.GetProperty(Key);
        TransformedProp := TransformWithReplacer(Obj, Key, PropValue, AReplacer);
        // Omit properties whose replacer result cannot appear in JSON objects.
        if not ((TransformedProp is TGocciaUndefinedLiteralValue) or
          TransformedProp.IsCallable or
          (TransformedProp is TGocciaSymbolValue)) then
          NewObj.AssignProperty(Key, TransformedProp);
      end;
    finally
      FReplacerTraversalStack.Delete(FReplacerTraversalStack.Count - 1);
    end;
    Result := NewObj;
  end
  // Step 6: Primitive values pass through directly.
  else
    Result := Replaced;
end;

// §25.5.4 steps 10-13: Stringify with a replacer function.
// Wraps the value in a root object, applies the replacer recursively, then serializes.
function TGocciaJSONBuiltin.StringifyWithReplacer(const AValue: TGocciaValue; const AReplacer: TGocciaValue; const AGap: string): string;
var
  Root: TGocciaObjectValue;
  Transformed: TGocciaValue;
begin
  // Step 10: Let wrapper be OrdinaryObjectCreate(%Object.prototype%).
  Root := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  // Step 11: Perform ! CreateDataPropertyOrThrow(wrapper, "", value).
  Root.CreateDataPropertyOrThrow('', AValue);
  // Step 12: Return ? SerializeJSONProperty(state, "", wrapper).
  FReplacerTraversalStack.Clear;
  Transformed := TransformWithReplacer(Root, '', AValue, AReplacer);

  if Transformed is TGocciaUndefinedLiteralValue then
  begin
    Result := '';
    Exit;
  end;

  Result := FStringifier.Stringify(Transformed, AGap, #0, nil, False);
end;

// §25.5.4 step 5: Stringify with an Array replacer (PropertyList filter).
// PropertyList is part of the serializer state, so it filters every object
// at every depth (SerializeJSONObject step 5), not just the root.
function TGocciaJSONBuiltin.StringifyWithAllowList(const AValue: TGocciaValue; const AAllowList: TGocciaObjectValue; const AGap: string): string;
var
  Element: TGocciaValue;
  HasItem: Boolean;
  I: Integer;
  Item: string;
  Len: Integer;
  PropertyList: TUnicodeStringList;
  Seen: TDictionary<string, Boolean>;
begin
  PropertyList := TUnicodeStringList.Create;
  Seen := TDictionary<string, Boolean>.Create;
  try
    Len := LengthOfArrayLike(AAllowList);
    for I := 0 to Len - 1 do
    begin
      // Step 5.b.ii: item stays undefined unless the element is a String or
      // Number primitive, or a String/Number wrapper object — converted via
      // ? ToString(v), which may invoke a user-defined toString. Booleans,
      // null, undefined, and plain objects are skipped.
      Element := AAllowList.GetProperty(IntToStr(I));
      Item := '';
      HasItem := (Element is TGocciaStringLiteralValue) or
        (Element is TGocciaNumberLiteralValue) or
        (Element is TGocciaStringObjectValue) or
        (Element is TGocciaNumberObjectValue);
      if HasItem then
        Item := Element.ToStringLiteral.Value;
      // Step 5.b.ii: append only if PropertyList does not contain item.
      if HasItem and not Seen.ContainsKey(Item) then
      begin
        Seen.Add(Item, True);
        PropertyList.Add(Item);
      end;
    end;

    Result := FStringifier.Stringify(AValue, AGap, #0, PropertyList);
  finally
    Seen.Free;
    PropertyList.Free;
  end;
end;

// ES2026 §25.5.4 JSON.stringify ( value [ , replacer [ , space ] ] )
function TGocciaJSONBuiltin.JSONStringify(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Value, ReplacerArg, SpaceArg: TGocciaValue;
  Gap: string;
  Stringified: string;
begin
  // Step 1: Let state be { ReplacerFunction: undefined, PropertyList: undefined, Indent: "", Gap: "" }.
  if AArgs.Length >= 1 then
    Value := AArgs.GetElement(0)
  else
    Value := TGocciaUndefinedLiteralValue.UndefinedValue;

  Gap := '';
  try
    // Steps 6-9: Resolve the gap/indent string from the space argument. Space
    // coercion can run user valueOf/toString, so it must stay inside the
    // error-normalization block.
    if AArgs.Length >= 3 then
    begin
      SpaceArg := AArgs.GetElement(2);
      Gap := ResolveGap(SpaceArg);
    end;

    if AArgs.Length >= 2 then
    begin
      ReplacerArg := AArgs.GetElement(1);
      // Step 2: If replacer is callable, set state.ReplacerFunction.
      if ReplacerArg.IsCallable then
      begin
        Stringified := StringifyWithReplacer(Value, ReplacerArg, Gap);
        if Stringified = '' then
          Result := TGocciaUndefinedLiteralValue.UndefinedValue
        else
          Result := TGocciaStringLiteralValue.Create(Stringified);
        Exit;
      end
      // Steps 4-5: If replacer is an Array, build state.PropertyList.
      else if (ReplacerArg is TGocciaObjectValue) and IsJSONObjectArray(ReplacerArg) then
      begin
        Stringified := StringifyWithAllowList(Value, TGocciaObjectValue(ReplacerArg), Gap);
        if Stringified = '' then
          Result := TGocciaUndefinedLiteralValue.UndefinedValue
        else
          Result := TGocciaStringLiteralValue.Create(Stringified);
        Exit;
      end;
    end;

    // Step 10-12: Create wrapper, set wrapper[""] = value, return SerializeJSONProperty(state, "", wrapper).
    Stringified := FStringifier.Stringify(Value, Gap);
    if Stringified = '' then
      Result := TGocciaUndefinedLiteralValue.UndefinedValue
    else
      Result := TGocciaStringLiteralValue.Create(Stringified);
  except
    on E: TGocciaThrowValue do
      raise;
    on E: Exception do
    begin
      ReraiseBytecodeThrow(E);
      ThrowTypeError(Format(SErrorJSONStringifyError, [E.Message]), SSuggestJSONFormat);
    end;
  end;
end;

initialization
  RegisterThreadvarCleanup(@ClearThreadvarMembers);

end.
