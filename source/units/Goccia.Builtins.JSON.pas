unit Goccia.Builtins.JSON;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.Arguments.Validator,
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
    FReviverSourceIndex: Integer;
    FReviverSourceTexts: TStringList;
    FStringifier: TGocciaJSONStringifier;

    function ApplyReviver(const AHolder: TGocciaValue; const AKey: string; const AReviver: TGocciaValue): TGocciaValue;
    function ApplyReplacer(const AHolder: TGocciaValue; const AKey: string; const AValue: TGocciaValue; const AReplacer: TGocciaValue): TGocciaValue;
    function ApplyToJSON(const AValue: TGocciaValue; const AKey: string): TGocciaValue;
    function ResolveGap(const ASpaceArg: TGocciaValue): string;
    function TransformWithReplacer(const AHolder: TGocciaValue; const AKey: string; const AValue: TGocciaValue; const AReplacer: TGocciaValue): TGocciaValue;
    function StringifyWithReplacer(const AValue: TGocciaValue; const AReplacer: TGocciaValue; const AGap: string): string;
    function StringifyWithAllowList(const AValue: TGocciaValue; const AAllowList: TGocciaArrayValue; const AGap: string): string;
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
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Utils,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.HoleValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

threadvar
  FStaticMembers: TArray<TGocciaMemberDefinition>;

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
    Members.AddMethod(JSONParse, 1, gmkStaticMethod);
    Members.AddMethod(JSONRawJSON, 1, gmkStaticMethod);
    Members.AddMethod(JSONStringify, 1, gmkStaticMethod);
    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create('JSON'),
      [pfConfigurable]);
    FStaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(FBuiltinObject, FStaticMembers);

  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet);
end;

destructor TGocciaJSONBuiltin.Destroy;
begin
  FParser.Free;
  FStringifier.Free;
  FReplacerTraversalStack.Free;
  inherited;
end;

// ES2026 §25.5.1.1 InternalizeJSONProperty ( holder, name, reviver )
function TGocciaJSONBuiltin.ApplyReviver(const AHolder: TGocciaValue; const AKey: string; const AReviver: TGocciaValue): TGocciaValue;
var
  Value, NewValue: TGocciaValue;
  Obj: TGocciaObjectValue;
  Arr: TGocciaArrayValue;
  I: Integer;
  PropKey: string;
  Args: TGocciaArgumentsCollection;
  Context: TGocciaObjectValue;
begin
  // Step 1: Let val be ? Get(holder, name).
  Value := AHolder.GetProperty(AKey);

  // Step 2: If val is an Object, recurse into its properties.
  if Value is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(Value);
    if Obj is TGocciaArrayValue then
    begin
      // Step 2a: If val is an Array, for each index I from 0 to len-1:
      Arr := TGocciaArrayValue(Obj);
      for I := 0 to Arr.Elements.Count - 1 do
      begin
        // Step 2a.i: Let newElement be ? InternalizeJSONProperty(val, ToString(I), reviver).
        NewValue := ApplyReviver(Arr, IntToStr(I), AReviver);
        // Step 2a.ii: If newElement is undefined, delete the element; else replace it.
        if NewValue is TGocciaUndefinedLiteralValue then
          Arr.Elements[I] := TGocciaHoleValue.HoleValue
        else
          Arr.Elements[I] := NewValue;
      end;
    end
    else
    begin
      // Step 2b: Else val is an ordinary Object, for each enumerable key:
      for PropKey in Obj.GetEnumerablePropertyNames do
      begin
        // Step 2b.i: Let newElement be ? InternalizeJSONProperty(val, P, reviver).
        NewValue := ApplyReviver(Obj, PropKey, AReviver);
        // Step 2b.ii: If newElement is undefined, delete property; else set it.
        if NewValue is TGocciaUndefinedLiteralValue then
          Obj.DeleteProperty(PropKey)
        else
          Obj.AssignProperty(PropKey, NewValue);
      end;
    end;
  end;

  // ES2026 §25.5.1.1 step 3: Build the context object for source text access.
  Context := TGocciaObjectValue.Create;
  if not (Value is TGocciaObjectValue) and Assigned(FReviverSourceTexts) and
    (FReviverSourceIndex < FReviverSourceTexts.Count) then
  begin
    Context.AssignProperty(PROP_SOURCE,
      TGocciaStringLiteralValue.Create(FReviverSourceTexts[FReviverSourceIndex]));
    Inc(FReviverSourceIndex);
  end;

  // Step 4: Return ? Call(reviver, holder, « name, val, context »).
  Args := TGocciaArgumentsCollection.CreateWithCapacity(3);
  try
    Args.Add(TGocciaStringLiteralValue.Create(AKey));
    Args.Add(Value);
    Args.Add(Context);
    Result := InvokeCallable(AReviver, Args, AHolder);
  finally
    Args.Free;
  end;
end;

// ES2026 §25.5.2.3 JSON.isRawJSON ( O )
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

// ES2026 §25.5.1 JSON.parse ( text [ , reviver ] )
function TGocciaJSONBuiltin.JSONParse(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  HasReviver: Boolean;
  PreviousSourceIndex: Integer;
  PreviousSourceTexts: TStringList;
  Reviver: TGocciaValue;
  Root: TGocciaObjectValue;
  SourceTexts: TStringList;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'JSON.parse', ThrowError);

  // Step 1: Let jsonString be ? ToString(text).
  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowTypeError(SErrorJSONParseArgMustBeString, SSuggestStringArgRequired);

  // Step 2-3: Parse and optionally apply the reviver with source text access.
  HasReviver := (AArgs.Length >= 2) and AArgs.GetElement(1).IsCallable;

  if HasReviver then
  begin
    Reviver := AArgs.GetElement(1);
    SourceTexts := TStringList.Create;
    try
      // Step 2: Parse with source text tracking for the reviver context.
      try
        FParser.ParseWithSources(
          AArgs.GetElement(0).ToStringLiteral.Value, Result, SourceTexts);
      except
        on E: Exception do
          ThrowSyntaxError(E.Message, SSuggestJSONFormat);
      end;

      // Step 3: Wrap in root object and apply InternalizeJSONProperty.
      Root := TGocciaObjectValue.Create;
      Root.AssignProperty('', Result);

      // Save/restore for reentrancy (reviver may call JSON.parse).
      PreviousSourceTexts := FReviverSourceTexts;
      PreviousSourceIndex := FReviverSourceIndex;
      FReviverSourceTexts := SourceTexts;
      FReviverSourceIndex := 0;
      try
        Result := ApplyReviver(Root, '', Reviver);
      finally
        FReviverSourceTexts := PreviousSourceTexts;
        FReviverSourceIndex := PreviousSourceIndex;
      end;
    finally
      SourceTexts.Free;
    end;
  end
  else
  begin
    // No reviver — parse without source text collection.
    try
      Result := FParser.Parse(AArgs.GetElement(0).ToStringLiteral.Value);
    except
      on E: Exception do
        ThrowSyntaxError(E.Message, SSuggestJSONFormat);
    end;
  end;
end;

// ES2026 §25.5.2.4 JSON.rawJSON ( text )
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
    Parsed := FParser.Parse(UTF8String(JSONString));
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

// §25.5.2 steps 7-8: Resolve the gap (indentation) string from the space argument.
function TGocciaJSONBuiltin.ResolveGap(const ASpaceArg: TGocciaValue): string;
var
  SpaceCount: Integer;
  I: Integer;
begin
  Result := '';
  // Step 7: If space is a Number, let gap be min(10, ToInteger(space)) spaces.
  if ASpaceArg is TGocciaNumberLiteralValue then
  begin
    SpaceCount := Trunc(ASpaceArg.ToNumberLiteral.Value);
    if SpaceCount > 10 then
      SpaceCount := 10;
    if SpaceCount < 1 then
      Exit;
    for I := 1 to SpaceCount do
      Result := Result + ' ';
  end
  // Step 8: Else if space is a String, let gap be the first 10 characters.
  else if ASpaceArg is TGocciaStringLiteralValue then
  begin
    Result := ASpaceArg.ToStringLiteral.Value;
    if Length(Result) > 10 then
      Result := Copy(Result, 1, 10);
  end;
  // Step 9: Else let gap be the empty string (already default).
end;

// ES2026 §25.5.2.2 SerializeJSONProperty ( state, key, holder ) step 1: toJSON hook.
function TGocciaJSONBuiltin.ApplyToJSON(const AValue: TGocciaValue; const AKey: string): TGocciaValue;
var
  ToJSONMethod: TGocciaValue;
  Args: TGocciaArgumentsCollection;
begin
  Result := AValue;
  if not (AValue is TGocciaObjectValue) then
    Exit;

  ToJSONMethod := TGocciaObjectValue(AValue).GetProperty(PROP_TO_JSON);
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

// §25.5.2.1 SerializeJSONProperty — Step 2: Call replacer function.
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

// §25.5.2.1 SerializeJSONProperty ( state, key, holder ) — recursive transformation.
// Applies the replacer function then recursively transforms nested objects/arrays.
function TGocciaJSONBuiltin.TransformWithReplacer(const AHolder: TGocciaValue; const AKey: string; const AValue: TGocciaValue; const AReplacer: TGocciaValue): TGocciaValue;
var
  Replaced, PropValue, TransformedProp: TGocciaValue;
  Obj: TGocciaObjectValue;
  Arr: TGocciaArrayValue;
  NewObj: TGocciaObjectValue;
  NewArr: TGocciaArrayValue;
  Key: string;
  I: Integer;
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

  // ES2026 §25.5.2.2 step 4a: If result has [[IsRawJSON]], pass through directly.
  if Replaced is TGocciaRawJSONValue then
  begin
    Result := Replaced;
    Exit;
  end;

  // Step 4: If result is an Array, recursively serialize each element (SerializeJSONArray).
  if Replaced is TGocciaArrayValue then
  begin
    if FReplacerTraversalStack.IndexOf(TGocciaArrayValue(Replaced)) <> -1 then
      ThrowTypeError(SErrorJSONCircularStructure, SSuggestJSONFormat);

    FReplacerTraversalStack.Add(TGocciaArrayValue(Replaced));
    Arr := TGocciaArrayValue(Replaced);
    NewArr := TGocciaArrayValue.Create;
    try
      for I := 0 to Arr.Elements.Count - 1 do
      begin
        PropValue := Arr.Elements[I];
        TransformedProp := TransformWithReplacer(Arr, IntToStr(I), PropValue, AReplacer);
        NewArr.Elements.Add(TransformedProp);
      end;
    finally
      FReplacerTraversalStack.Delete(FReplacerTraversalStack.Count - 1);
    end;
    Result := NewArr;
  end
  // Step 5: If result is an Object, recursively serialize each property (SerializeJSONObject).
  else if (Replaced is TGocciaObjectValue) and not (Replaced is TGocciaArrayValue) then
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
        // Omit properties whose replacer result is undefined.
        if not (TransformedProp is TGocciaUndefinedLiteralValue) then
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

// §25.5.2 steps 10-11: Stringify with a replacer function.
// Wraps the value in a root object, applies the replacer recursively, then serializes.
function TGocciaJSONBuiltin.StringifyWithReplacer(const AValue: TGocciaValue; const AReplacer: TGocciaValue; const AGap: string): string;
var
  Root: TGocciaObjectValue;
  Transformed: TGocciaValue;
begin
  // Step 10: Let wrapper be OrdinaryObjectCreate(null).
  Root := TGocciaObjectValue.Create;
  // Step 11: Perform ! CreateDataPropertyOrThrow(wrapper, "", value).
  Root.AssignProperty('', AValue);
  // Step 12: Return ? SerializeJSONProperty(state, "", wrapper).
  FReplacerTraversalStack.Clear;
  Transformed := TransformWithReplacer(Root, '', AValue, AReplacer);

  if Transformed is TGocciaUndefinedLiteralValue then
  begin
    Result := '';
    Exit;
  end;

  Result := FStringifier.Stringify(Transformed, AGap);
end;

// §25.5.2 steps 4-5: Stringify with an Array replacer (PropertyList filter).
// When the replacer is an Array, only properties whose names appear in the
// allow-list are included in the output (SerializeJSONObject step 6a).
function TGocciaJSONBuiltin.StringifyWithAllowList(const AValue: TGocciaValue; const AAllowList: TGocciaArrayValue; const AGap: string): string;
var
  Obj: TGocciaObjectValue;
  Filtered: TGocciaObjectValue;
  I: Integer;
  Key: string;
  PropValue: TGocciaValue;
begin
  // Non-object or Array values are serialized directly (PropertyList is ignored).
  if not (AValue is TGocciaObjectValue) or (AValue is TGocciaArrayValue) then
  begin
    Result := FStringifier.Stringify(AValue, AGap);
    Exit;
  end;

  // Step 4b: Build PropertyList from the Array replacer elements.
  Obj := TGocciaObjectValue(AValue);
  Filtered := TGocciaObjectValue.Create;

  // Step 5: For each element in PropertyList, include the property if it exists.
  for I := 0 to AAllowList.Elements.Count - 1 do
  begin
    Key := AAllowList.Elements[I].ToStringLiteral.Value;
    PropValue := Obj.GetProperty(Key);
    if (PropValue <> nil) and not (PropValue is TGocciaUndefinedLiteralValue) then
      Filtered.AssignProperty(Key, PropValue);
  end;

  Result := FStringifier.Stringify(Filtered, AGap);
end;

// §25.5.2 JSON.stringify ( value [ , replacer [ , space ] ] )
function TGocciaJSONBuiltin.JSONStringify(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Value, ReplacerArg, SpaceArg: TGocciaValue;
  Gap: string;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'JSON.stringify', ThrowError);

  // Step 1: Let state be { ReplacerFunction: undefined, PropertyList: undefined, Indent: "", Gap: "" }.
  Value := AArgs.GetElement(0);

  // Undefined values produce undefined (not "undefined").
  if Value is TGocciaUndefinedLiteralValue then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  // Steps 7-9: Resolve the gap/indent string from the space argument.
  Gap := '';
  if AArgs.Length >= 3 then
  begin
    SpaceArg := AArgs.GetElement(2);
    Gap := ResolveGap(SpaceArg);
  end;

  try
    if AArgs.Length >= 2 then
    begin
      ReplacerArg := AArgs.GetElement(1);
      // Step 2: If replacer is callable, set state.ReplacerFunction.
      if ReplacerArg.IsCallable then
      begin
        Result := TGocciaStringLiteralValue.Create(StringifyWithReplacer(Value, ReplacerArg, Gap));
        Exit;
      end
      // Steps 4-5: If replacer is an Array, build state.PropertyList.
      else if ReplacerArg is TGocciaArrayValue then
      begin
        Result := TGocciaStringLiteralValue.Create(StringifyWithAllowList(Value, TGocciaArrayValue(ReplacerArg), Gap));
        Exit;
      end;
    end;

    // Step 10-12: Create wrapper, set wrapper[""] = value, return SerializeJSONProperty(state, "", wrapper).
    Result := TGocciaStringLiteralValue.Create(FStringifier.Stringify(Value, Gap));
  except
    on E: TGocciaThrowValue do
      raise;
    on E: Exception do
      ThrowTypeError(Format(SErrorJSONStringifyError, [E.Message]), SSuggestJSONFormat);
  end;
end;

end.
