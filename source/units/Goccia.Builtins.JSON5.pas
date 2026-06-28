unit Goccia.Builtins.JSON5;

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
  Goccia.JSON5,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaJSON5Builtin = class(TGocciaBuiltin)
  private
    FParser: TGocciaJSON5Parser;
    FReplacerTraversalStack: TList<TGocciaObjectValue>;
    FReviverSourceIndex: Integer;
    FReviverSourceTexts: TStringList;
    FStringifier: TGocciaJSONStringifier;

    function ApplyReviver(const AHolder: TGocciaValue; const AKey: string;
      const AReviver: TGocciaValue): TGocciaValue;
    function ApplyReplacer(const AHolder: TGocciaValue; const AKey: string;
      const AValue: TGocciaValue; const AReplacer: TGocciaValue): TGocciaValue;
    function ApplyToJSON(const AValue: TGocciaValue; const AKey: string): TGocciaValue;
    function ResolveQuoteChar(const AQuoteArg: TGocciaValue): Char;
    function ResolveGap(const ASpaceArg: TGocciaValue): string;
    function RootResultShouldBeUndefined(const AValue: TGocciaValue): Boolean;
    function TryExtractAllowListKey(const AValue: TGocciaValue; out AKey: string): Boolean;
    function UnboxWrappedPrimitive(const AValue: TGocciaValue): TGocciaValue;
    function StringifyWithAllowList(const AValue: TGocciaValue;
      const AAllowList: TGocciaArrayValue; const AGap: string;
      const APreferredQuoteChar: Char): string;
    function StringifyWithReplacer(const AValue: TGocciaValue;
      const AReplacer: TGocciaValue; const AGap: string;
      const APreferredQuoteChar: Char): string;
    function TransformWithReplacer(const AHolder: TGocciaValue;
      const AKey: string; const AValue: TGocciaValue;
      const AReplacer: TGocciaValue): TGocciaValue;
  published
    function JSON5Parse(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function JSON5Stringify(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback);
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  SysUtils,

  TextSemantics,

  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.ThreadCleanupRegistry,
  Goccia.Utils,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.HoleValue,
  Goccia.Values.NumberObjectValue,
  Goccia.Values.ObjectPropertyDescriptor,
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

function UTF8CopyByCharacters(const AText: string;
  const AMaxChars: Integer): string;
var
  ByteIndex: Integer;
  CharacterCount: Integer;
  SequenceLength: Integer;
begin
  if AMaxChars <= 0 then
    Exit('');

  ByteIndex := 1;
  CharacterCount := 0;
  while (ByteIndex <= Length(AText)) and (CharacterCount < AMaxChars) do
  begin
    SequenceLength := TextSemantics.UTF8SequenceLengthFromLeadByte(AText[ByteIndex]);
    Inc(ByteIndex, SequenceLength);
    Inc(CharacterCount);
  end;

  Result := Copy(AText, 1, ByteIndex - 1);
end;

constructor TGocciaJSON5Builtin.Create(const AName: string;
  const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  Members: TGocciaMemberCollection;
begin
  inherited Create(AName, AScope, AThrowError);

  FParser := TGocciaJSON5Parser.Create;
  FStringifier := TGocciaJSONStringifier.Create(jsmJSON5);
  FReplacerTraversalStack := TList<TGocciaObjectValue>.Create;

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddMethod(JSON5Parse, 1, gmkStaticMethod);
    Members.AddMethod(JSON5Stringify, 1, gmkStaticMethod);
    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create('JSON5'),
      [pfConfigurable]);
    FStaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;

  RegisterMemberDefinitions(FBuiltinObject, FStaticMembers);
  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet, True);
end;

destructor TGocciaJSON5Builtin.Destroy;
begin
  FParser.Free;
  FStringifier.Free;
  FReplacerTraversalStack.Free;
  inherited;
end;

// ES2026 §25.5.1.1 InternalizeJSONProperty ( holder, name, reviver )
function TGocciaJSON5Builtin.ApplyReviver(const AHolder: TGocciaValue;
  const AKey: string; const AReviver: TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
  Arr: TGocciaArrayValue;
  Context: TGocciaObjectValue;
  I: Integer;
  NewValue: TGocciaValue;
  Obj: TGocciaObjectValue;
  PropKey: string;
  Value: TGocciaValue;
begin
  Value := AHolder.GetProperty(AKey);

  if Value is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(Value);
    if Obj is TGocciaArrayValue then
    begin
      Arr := TGocciaArrayValue(Obj);
      for I := 0 to Arr.Elements.Count - 1 do
      begin
        NewValue := ApplyReviver(Arr, IntToStr(I), AReviver);
        if NewValue is TGocciaUndefinedLiteralValue then
          Arr.Elements[I] := TGocciaHoleValue.HoleValue
        else
          Arr.Elements[I] := NewValue;
      end;
    end
    else
    begin
      for PropKey in Obj.GetEnumerablePropertyNames do
      begin
        NewValue := ApplyReviver(Obj, PropKey, AReviver);
        if NewValue is TGocciaUndefinedLiteralValue then
          Obj.DeleteProperty(PropKey)
        else
          Obj.AssignProperty(PropKey, NewValue);
      end;
    end;
  end;

  // Build the context object for source text access.
  Context := TGocciaObjectValue.Create;
  if not (Value is TGocciaObjectValue) and Assigned(FReviverSourceTexts) and
    (FReviverSourceIndex < FReviverSourceTexts.Count) then
  begin
    Context.AssignProperty(PROP_SOURCE,
      TGocciaStringLiteralValue.Create(FReviverSourceTexts[FReviverSourceIndex]));
    Inc(FReviverSourceIndex);
  end;

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

function TGocciaJSON5Builtin.ApplyToJSON(const AValue: TGocciaValue;
  const AKey: string): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
  ToJSONMethod: TGocciaValue;
begin
  Result := AValue;
  if not (AValue is TGocciaObjectValue) then
    Exit;

  ToJSONMethod := TGocciaObjectValue(AValue).GetProperty(PROP_TO_JSON5);
  if not Assigned(ToJSONMethod) or not ToJSONMethod.IsCallable then
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

function TGocciaJSON5Builtin.ApplyReplacer(const AHolder: TGocciaValue;
  const AKey: string; const AValue: TGocciaValue;
  const AReplacer: TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
begin
  Args := TGocciaArgumentsCollection.Create;
  try
    Args.Add(TGocciaStringLiteralValue.Create(AKey));
    Args.Add(AValue);
    Result := InvokeCallable(AReplacer, Args, AHolder);
  finally
    Args.Free;
  end;
end;

function TGocciaJSON5Builtin.UnboxWrappedPrimitive(
  const AValue: TGocciaValue): TGocciaValue;
begin
  Result := Goccia.Values.WrapperPrimitives.UnboxWrappedPrimitive(AValue);
end;

function TGocciaJSON5Builtin.TransformWithReplacer(const AHolder: TGocciaValue;
  const AKey: string; const AValue: TGocciaValue;
  const AReplacer: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  I: Integer;
  Key: string;
  NewArr: TGocciaArrayValue;
  NewObj: TGocciaObjectValue;
  Obj: TGocciaObjectValue;
  PropValue: TGocciaValue;
  Replaced: TGocciaValue;
  TransformedProp: TGocciaValue;
  Len: Integer;
begin
  Replaced := ApplyToJSON(AValue, AKey);
  Replaced := ApplyReplacer(AHolder, AKey, Replaced, AReplacer);
  // ES2026 §25.5.4.2 steps 4.b-4.d: unwrap boxed primitives.
  Replaced := CoerceWrappedPrimitive(Replaced);

  if Replaced is TGocciaUndefinedLiteralValue then
  begin
    Result := Replaced;
    Exit;
  end;

  if Replaced.IsCallable or (Replaced is TGocciaSymbolValue) then
  begin
    Result := Replaced;
    Exit;
  end;

  if Replaced is TGocciaArrayValue then
  begin
    if FReplacerTraversalStack.IndexOf(TGocciaArrayValue(Replaced)) <> -1 then
      ThrowTypeError(SErrorCircularStructureToJSON5, SSuggestJSONFormat);

    FReplacerTraversalStack.Add(TGocciaArrayValue(Replaced));
    Arr := TGocciaArrayValue(Replaced);
    NewArr := TGocciaArrayValue.Create;
    try
      Len := LengthOfArrayLike(Arr);
      for I := 0 to Len - 1 do
      begin
        PropValue := Arr.GetProperty(IntToStr(I));
        TransformedProp := TransformWithReplacer(Arr, IntToStr(I), PropValue,
          AReplacer);
        NewArr.Elements.Add(TransformedProp);
      end;
    finally
      FReplacerTraversalStack.Delete(FReplacerTraversalStack.Count - 1);
    end;
    Result := NewArr;
  end
  else if (Replaced is TGocciaObjectValue) and
    not (Replaced is TGocciaArrayValue) then
  begin
    if FReplacerTraversalStack.IndexOf(TGocciaObjectValue(Replaced)) <> -1 then
      ThrowTypeError(SErrorCircularStructureToJSON5, SSuggestJSONFormat);

    FReplacerTraversalStack.Add(TGocciaObjectValue(Replaced));
    Obj := TGocciaObjectValue(Replaced);
    NewObj := TGocciaObjectValue.Create;
    try
      for Key in Obj.GetEnumerablePropertyNames do
      begin
        PropValue := Obj.GetProperty(Key);
        TransformedProp := TransformWithReplacer(Obj, Key, PropValue, AReplacer);
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
  else
    Result := Replaced;
end;

// The upstream json5 stringifier never coerces quote explicitly, but every
// use site (string concatenation, property-key lookup) applies ToString, so a
// quote with a [[StringData]] slot honors a user-defined toString. Other
// objects stay ignored: Goccia validates quote strictly to a single ' or ",
// and no non-String wrapper can satisfy that.
function TGocciaJSON5Builtin.ResolveQuoteChar(const AQuoteArg: TGocciaValue): Char;
var
  QuoteText: string;
  QuoteValue: TGocciaValue;
begin
  Result := #0;
  QuoteValue := AQuoteArg;
  if QuoteValue is TGocciaStringObjectValue then
    QuoteValue := QuoteValue.ToStringLiteral;
  if not (QuoteValue is TGocciaStringLiteralValue) then
    Exit;

  QuoteText := QuoteValue.ToStringLiteral.Value;
  if (Length(QuoteText) = 1) and (QuoteText[1] in ['''', '"']) then
    Result := QuoteText[1];
end;

// The upstream json5 stringifier coerces a space with a [[NumberData]] slot
// via Number(space) and one with a [[StringData]] slot via String(space). The
// object-level ToNumberLiteral/ToStringLiteral route through ToPrimitive, so
// user-defined valueOf/toString are honored, matching JSON.stringify.
function TGocciaJSON5Builtin.ResolveGap(const ASpaceArg: TGocciaValue): string;
var
  SpaceNumber: Double;
  SpaceCount: Integer;
  SpaceValue: TGocciaValue;
begin
  Result := '';
  SpaceValue := ASpaceArg;
  if SpaceValue is TGocciaNumberObjectValue then
    SpaceValue := SpaceValue.ToNumberLiteral
  else if SpaceValue is TGocciaStringObjectValue then
    SpaceValue := SpaceValue.ToStringLiteral;
  // Clamp before Trunc so NaN, ±Infinity, and doubles beyond Integer range
  // never reach Trunc.
  if SpaceValue is TGocciaNumberLiteralValue then
  begin
    SpaceNumber := SpaceValue.ToNumberLiteral.Value;
    if Math.IsNaN(SpaceNumber) or (SpaceNumber < 1) then
      Exit;
    if SpaceNumber > 10 then
      SpaceCount := 10
    else
      SpaceCount := Trunc(SpaceNumber);
    Result := StringOfChar(' ', SpaceCount);
  end
  else if SpaceValue is TGocciaStringLiteralValue then
  begin
    Result := SpaceValue.ToStringLiteral.Value;
    Result := UTF8CopyByCharacters(Result, 10);
  end;
end;

function TGocciaJSON5Builtin.RootResultShouldBeUndefined(
  const AValue: TGocciaValue): Boolean;
var
  RootValue: TGocciaValue;
begin
  RootValue := UnboxWrappedPrimitive(AValue);
  Result := (RootValue is TGocciaUndefinedLiteralValue) or
    RootValue.IsCallable or
    (RootValue is TGocciaSymbolValue);
end;

// The upstream json5 stringifier admits primitive strings and numbers plus
// Number/String wrappers as allow-list entries, coercing wrappers with
// String(v) — so a user-defined toString is honored, never a raw slot read.
function TGocciaJSON5Builtin.TryExtractAllowListKey(const AValue: TGocciaValue;
  out AKey: string): Boolean;
begin
  Result := (AValue is TGocciaStringLiteralValue) or
    (AValue is TGocciaNumberLiteralValue) or
    (AValue is TGocciaStringObjectValue) or
    (AValue is TGocciaNumberObjectValue);
  if Result then
    AKey := AValue.ToStringLiteral.Value
  else
    AKey := '';
end;

function TGocciaJSON5Builtin.StringifyWithReplacer(const AValue: TGocciaValue;
  const AReplacer: TGocciaValue; const AGap: string;
  const APreferredQuoteChar: Char): string;
var
  PreviousTraversalStack: TList<TGocciaObjectValue>;
  Root: TGocciaObjectValue;
  Transformed: TGocciaValue;
begin
  Root := TGocciaObjectValue.Create;
  Root.AssignProperty('', AValue);
  PreviousTraversalStack := FReplacerTraversalStack;
  FReplacerTraversalStack := TList<TGocciaObjectValue>.Create;
  try
    Transformed := TransformWithReplacer(Root, '', AValue, AReplacer);

    if RootResultShouldBeUndefined(Transformed) then
    begin
      Result := '';
      Exit;
    end;

    Result := FStringifier.Stringify(Transformed, AGap, APreferredQuoteChar);
  finally
    FReplacerTraversalStack.Free;
    FReplacerTraversalStack := PreviousTraversalStack;
  end;
end;

function TGocciaJSON5Builtin.StringifyWithAllowList(const AValue: TGocciaValue;
  const AAllowList: TGocciaArrayValue; const AGap: string;
  const APreferredQuoteChar: Char): string;
var
  I: Integer;
  Key: string;
  Keys: TStringList;
  Len: Integer;
  Seen: TDictionary<string, Boolean>;
begin
  // Upstream json5 reference: the property list is extracted and de-duplicated
  // once, before serialization, so a wrapper's toString runs once per element.
  Keys := TStringList.Create;
  Seen := TDictionary<string, Boolean>.Create;
  try
    Len := LengthOfArrayLike(AAllowList);
    for I := 0 to Len - 1 do
    begin
      if not TryExtractAllowListKey(AAllowList.GetProperty(IntToStr(I)), Key) then
        Continue;
      if Seen.ContainsKey(Key) then
        Continue;
      Seen.Add(Key, True);
      Keys.Add(Key);
    end;

    Result := FStringifier.Stringify(AValue, AGap, APreferredQuoteChar, Keys);
  finally
    Seen.Free;
    Keys.Free;
  end;
end;

function TGocciaJSON5Builtin.JSON5Parse(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  HasReviver: Boolean;
  PreviousSourceIndex: Integer;
  PreviousSourceTexts: TStringList;
  Reviver: TGocciaValue;
  Root: TGocciaObjectValue;
  SourceTexts: TStringList;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'JSON5.parse', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowTypeError(SErrorJSON5ParseArgMustBeString, SSuggestStringArgRequired);

  HasReviver := (AArgs.Length >= 2) and AArgs.GetElement(1).IsCallable;

  if HasReviver then
  begin
    Reviver := AArgs.GetElement(1);
    SourceTexts := TStringList.Create;
    try
      try
        FParser.ParseWithSources(
          AArgs.GetElement(0).ToStringLiteral.Value, Result, SourceTexts);
      except
        on E: Exception do
          ThrowSyntaxError(E.Message, SSuggestJSONFormat);
      end;

      Root := TGocciaObjectValue.Create;
      Root.AssignProperty('', Result);

      // Save/restore for reentrancy (reviver may call JSON5.parse).
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
    try
      Result := FParser.Parse(AArgs.GetElement(0).ToStringLiteral.Value);
    except
      on E: Exception do
        ThrowSyntaxError(E.Message, SSuggestJSONFormat);
    end;
  end;
end;

function TGocciaJSON5Builtin.JSON5Stringify(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Gap: string;
  Options: TGocciaObjectValue;
  QuoteChar: Char;
  ReplacerArg: TGocciaValue;
  SpaceArg: TGocciaValue;
  Stringified: string;
  UseOptionsObject: Boolean;
  Value: TGocciaValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'JSON5.stringify',
    ThrowError);

  Value := AArgs.GetElement(0);

  Gap := '';
  QuoteChar := #0;
  ReplacerArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  SpaceArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  UseOptionsObject := False;

  try
    // Quote/space coercion can run user valueOf/toString, so it must stay
    // inside the error-normalization block.
    if AArgs.Length >= 2 then
    begin
      ReplacerArg := AArgs.GetElement(1);
      UseOptionsObject := (ReplacerArg is TGocciaObjectValue) and
        not (ReplacerArg is TGocciaArrayValue) and
        not ReplacerArg.IsCallable;
      if UseOptionsObject then
      begin
        Options := TGocciaObjectValue(ReplacerArg);
        ReplacerArg := Options.GetProperty(PROP_REPLACER);
        SpaceArg := Options.GetProperty(PROP_SPACE);
        QuoteChar := ResolveQuoteChar(Options.GetProperty(PROP_QUOTE));
      end;
    end;

    if not UseOptionsObject and (AArgs.Length >= 3) then
    begin
      SpaceArg := AArgs.GetElement(2);
    end;
    Gap := ResolveGap(SpaceArg);

    if not (ReplacerArg is TGocciaUndefinedLiteralValue) then
    begin
      if ReplacerArg.IsCallable then
      begin
        Stringified := StringifyWithReplacer(Value, ReplacerArg, Gap, QuoteChar);
        if Stringified = '' then
          Result := TGocciaUndefinedLiteralValue.UndefinedValue
        else
          Result := TGocciaStringLiteralValue.Create(Stringified);
        Exit;
      end
      else if ReplacerArg is TGocciaArrayValue then
      begin
        Stringified := StringifyWithAllowList(Value, TGocciaArrayValue(ReplacerArg),
          Gap, QuoteChar);
        if Stringified = '' then
          Result := TGocciaUndefinedLiteralValue.UndefinedValue
        else
          Result := TGocciaStringLiteralValue.Create(Stringified);
        Exit;
      end;
    end;

    if RootResultShouldBeUndefined(Value) then
      Result := TGocciaUndefinedLiteralValue.UndefinedValue
    else
    begin
      Stringified := FStringifier.Stringify(Value, Gap, QuoteChar);
      if Stringified = '' then
        Result := TGocciaUndefinedLiteralValue.UndefinedValue
      else
        Result := TGocciaStringLiteralValue.Create(Stringified);
    end;
  except
    on E: TGocciaThrowValue do
      raise;
    on E: Exception do
    begin
      ReraiseBytecodeThrow(E);
      ThrowTypeError(Format(SErrorJSON5StringifyError, [E.Message]), SSuggestJSONFormat);
    end;
  end;
end;

initialization
  RegisterThreadvarCleanup(@ClearThreadvarMembers);

end.
