unit Goccia.Builtins.JSON;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Arguments.Validator,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.JSON,
  Goccia.Scope,
  Goccia.Values.ArrayValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives;

type
  TGocciaJSONBuiltin = class(TGocciaBuiltin)
  private
    FParser: TGocciaJSONParser;
    FStringifier: TGocciaJSONStringifier;

    function ApplyReviver(const AHolder: TGocciaValue; const AKey: string; const AReviver: TGocciaValue): TGocciaValue;
    function ApplyReplacer(const AHolder: TGocciaValue; const AKey: string; const AValue: TGocciaValue; const AReplacer: TGocciaValue): TGocciaValue;
    function ResolveGap(const ASpaceArg: TGocciaValue): string;
    function TransformWithReplacer(const AHolder: TGocciaValue; const AKey: string; const AValue: TGocciaValue; const AReplacer: TGocciaValue): TGocciaValue;
    function StringifyWithReplacer(const AValue: TGocciaValue; const AReplacer: TGocciaValue; const AGap: string): string;
    function StringifyWithAllowList(const AValue: TGocciaValue; const AAllowList: TGocciaArrayValue; const AGap: string): string;
  protected
    function JSONParse(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function JSONStringify(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,

  Goccia.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectValue;

constructor TGocciaJSONBuiltin.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FParser := TGocciaJSONParser.Create;
  FStringifier := TGocciaJSONStringifier.Create;

  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(JSONParse, 'parse', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(JSONStringify, 'stringify', 1));

  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet);
end;

destructor TGocciaJSONBuiltin.Destroy;
begin
  FParser.Free;
  FStringifier.Free;
  inherited;
end;

// §25.5.1.1 InternalizeJSONProperty ( holder, name, reviver )
function TGocciaJSONBuiltin.ApplyReviver(const AHolder: TGocciaValue; const AKey: string; const AReviver: TGocciaValue): TGocciaValue;
var
  Value, NewValue: TGocciaValue;
  Obj: TGocciaObjectValue;
  Arr: TGocciaArrayValue;
  I: Integer;
  PropKey: string;
  Args: TGocciaArgumentsCollection;
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
        // Step 2a.ii: If newElement is undefined, set element to undefined; else replace.
        if NewValue is TGocciaUndefinedLiteralValue then
          Arr.Elements[I] := TGocciaUndefinedLiteralValue.UndefinedValue
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

  // Step 3: Return ? Call(reviver, holder, « name, val »).
  Args := TGocciaArgumentsCollection.Create;
  Args.Add(TGocciaStringLiteralValue.Create(AKey));
  Args.Add(Value);
  Result := InvokeCallable(AReviver, Args, AHolder);
end;

// §25.5.1 JSON.parse ( text [ , reviver ] )
function TGocciaJSONBuiltin.JSONParse(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Reviver: TGocciaValue;
  Root: TGocciaObjectValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'JSON.parse', ThrowError);

  // Step 1: Let jsonString be ? ToString(text).
  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowError('JSON.parse: argument must be a string', 0, 0);

  // Step 2: Parse jsonString as a JSON text (throw SyntaxError on failure).
  // Let unfiltered be the result.
  try
    Result := FParser.Parse(AArgs.GetElement(0).ToStringLiteral.Value);
  except
    on E: Exception do
      ThrowSyntaxError(E.Message);
  end;

  // Step 3: If reviver is callable, wrap in root object and apply InternalizeJSONProperty.
  if AArgs.Length >= 2 then
  begin
    Reviver := AArgs.GetElement(1);
    if Reviver.IsCallable then
    begin
      // Step 3a: Let root be OrdinaryObjectCreate(null).
      Root := TGocciaObjectValue.Create;
      // Step 3b: Perform ! CreateDataPropertyOrThrow(root, "", unfiltered).
      Root.AssignProperty('', Result);
      // Step 3c: Return ? InternalizeJSONProperty(root, "", reviver).
      Result := ApplyReviver(Root, '', Reviver);
    end;
  end;
  // Step 4: Else return unfiltered.
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
  // Step 1: Call the replacer to get the transformed value.
  Replaced := ApplyReplacer(AHolder, AKey, AValue, AReplacer);

  // Step 2: If replacer returned undefined, signal omission.
  if Replaced is TGocciaUndefinedLiteralValue then
  begin
    Result := Replaced;
    Exit;
  end;

  // Step 3: If result is an Array, recursively serialize each element (SerializeJSONArray).
  if Replaced is TGocciaArrayValue then
  begin
    Arr := TGocciaArrayValue(Replaced);
    NewArr := TGocciaArrayValue.Create;
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      PropValue := Arr.Elements[I];
      TransformedProp := TransformWithReplacer(Arr, IntToStr(I), PropValue, AReplacer);
      NewArr.Elements.Add(TransformedProp);
    end;
    Result := NewArr;
  end
  // Step 4: If result is an Object, recursively serialize each property (SerializeJSONObject).
  else if (Replaced is TGocciaObjectValue) and not (Replaced is TGocciaArrayValue) then
  begin
    Obj := TGocciaObjectValue(Replaced);
    NewObj := TGocciaObjectValue.Create;
    for Key in Obj.GetEnumerablePropertyNames do
    begin
      PropValue := Obj.GetProperty(Key);
      TransformedProp := TransformWithReplacer(Obj, Key, PropValue, AReplacer);
      // Omit properties whose replacer result is undefined.
      if not (TransformedProp is TGocciaUndefinedLiteralValue) then
        NewObj.AssignProperty(Key, TransformedProp);
    end;
    Result := NewObj;
  end
  // Step 5: Primitive values pass through directly.
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
    on E: Exception do
      ThrowError('JSON.stringify error: ' + E.Message, 0, 0);
  end;
end;

end.
