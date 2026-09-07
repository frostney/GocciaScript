unit Goccia.Builtins.JSON5;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  UnicodeStringList,

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
    FReviverSourceTexts: TUnicodeStringList;
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
      const AThrowError: TGocciaThrowErrorCallback;
      const ADefineGlobalBinding: Boolean = True);
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  SysUtils,

  TextSemantics,

  Goccia.Constants.PropertyNames,
  Goccia.EngineFault,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.MemoryLimit,
  Goccia.Timeout,
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

function CopyByCodePoints(const AText: string;
  const AMaxChars: Integer): string;
var
  CodeUnitIndex: Integer;
  CharacterCount: Integer;
  SequenceLength: Integer;
begin
  if AMaxChars <= 0 then
    Exit('');

  CodeUnitIndex := 1;
  CharacterCount := 0;
  while (CodeUnitIndex <= Length(AText)) and
        (CharacterCount < AMaxChars) do
  begin
    SequenceLength := TextSemantics.CodePointSequenceLengthAt(
      AText, CodeUnitIndex);
    Inc(CodeUnitIndex, SequenceLength);
    Inc(CharacterCount);
  end;

  Result := Copy(AText, 1, CodeUnitIndex - 1);
end;

constructor TGocciaJSON5Builtin.Create(const AName: string;
  const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback;
  const ADefineGlobalBinding: Boolean = True);
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
    RegisterMemberDefinitions(FBuiltinObject, Members.ToDefinitions);
  finally
    Members.Free;
  end;
  if ADefineGlobalBinding then
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
  ValueRoot, ContextRoot: TGocciaTempRoot;
  NewValueRoots: TGocciaActiveRootFrame;
begin
  Value := AHolder.GetProperty(AKey);

  InitializeTempRoot(ValueRoot);
  InitializeTempRoot(ContextRoot);
  NewValueRoots.Initialize;
  try
    // Every recursion below ends in a reviver call, and the reviver is arbitrary
    // user code — a GC safe point that can also delete the holder property this
    // value came from. Between the read above and the call at the end, this
    // frame is the only thing holding it.
    AddTempRootIfNeeded(ValueRoot, Value);

    if Value is TGocciaObjectValue then
    begin
      Obj := TGocciaObjectValue(Value);
      if Obj is TGocciaArrayValue then
      begin
        Arr := TGocciaArrayValue(Obj);
        for I := 0 to Arr.Elements.Count - 1 do
        begin
          NewValue := ApplyReviver(Arr, IntToStr(I), AReviver);
          // The reviver's return value is reachable only from this local
          // between the recursion releasing its roots and the store below,
          // and the store can collect. A frame and not a re-pointed temp
          // root: a temp root is a set, so a reviver returning one object for
          // two keys would have the first store's release unroot it for the
          // second. See the JSON builtin's twin for the full argument.
          NewValueRoots.Add(NewValue);
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
          // Same window as the array arm above.
          NewValueRoots.Add(NewValue);
          if NewValue is TGocciaUndefinedLiteralValue then
            Obj.DeleteProperty(PropKey)
          else
            Obj.AssignProperty(PropKey, NewValue);
        end;
      end;
    end;

    // Build the context object for source text access.
    Context := TGocciaObjectValue.Create;
    // Nothing references the context until it enters the argument list below,
    // and the source-text property write plus the argument allocations in
    // between are all allocation points that can collect it.
    AddTempRootIfNeeded(ContextRoot, Context);
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
  finally
    NewValueRoots.Clear;
    RemoveTempRootIfNeeded(ContextRoot);
    RemoveTempRootIfNeeded(ValueRoot);
  end;
end;

function TGocciaJSON5Builtin.ApplyToJSON(const AValue: TGocciaValue;
  const AKey: string): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
  MethodRoot: TGocciaTempRoot;
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

  InitializeTempRoot(MethodRoot);
  try
    // An accessor can hand back a function that lives nowhere else, and both
    // allocations below happen before the call that finally uses it.
    AddTempRootIfNeeded(MethodRoot, ToJSONMethod);
    Args := TGocciaArgumentsCollection.CreateWithCapacity(1);
    try
      Args.Add(TGocciaStringLiteralValue.Create(AKey));
      Result := InvokeCallable(ToJSONMethod, Args, AValue);
    finally
      Args.Free;
    end;
  finally
    RemoveTempRootIfNeeded(MethodRoot);
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
  ReplacedRoot, ResultRoot, PropValueRoot, TransformedPropRoot: TGocciaTempRoot;
begin
  // The replacer is user code and so is every accessor this walk reads through,
  // which makes each of them a GC safe point. Nothing in this frame is reachable
  // from a real root while that code runs: the value the replacer returned, the
  // partially built copy, and the property being transformed all live only in
  // these locals. FReplacerTraversalStack is a plain TList and is not a root
  // either, so the entry this frame pushes onto it is rooted here as well — that
  // is what keeps the whole ancestor chain alive across a nested replacer call.
  // The nest-safe TGocciaTempRoot form is used because the roots are re-pointed
  // as the walk proceeds, and the finally releases whatever is left rooted.
  InitializeTempRoot(ReplacedRoot);
  InitializeTempRoot(ResultRoot);
  InitializeTempRoot(PropValueRoot);
  InitializeTempRoot(TransformedPropRoot);
  try
    Replaced := ApplyToJSON(AValue, AKey);
    AddTempRootIfNeeded(ReplacedRoot, Replaced);
    Replaced := ApplyReplacer(AHolder, AKey, Replaced, AReplacer);
    AddTempRootIfNeeded(ReplacedRoot, Replaced);
    // ES2026 §25.5.4.2 steps 4.b-4.d: unwrap boxed primitives.
    Replaced := CoerceWrappedPrimitive(Replaced);
    AddTempRootIfNeeded(ReplacedRoot, Replaced);

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
      AddTempRootIfNeeded(ResultRoot, NewArr);
      try
        Len := LengthOfArrayLike(Arr);
        for I := 0 to Len - 1 do
        begin
          PropValue := Arr.GetProperty(IntToStr(I));
          AddTempRootIfNeeded(PropValueRoot, PropValue);
          TransformedProp := TransformWithReplacer(Arr, IntToStr(I), PropValue,
            AReplacer);
          AddTempRootIfNeeded(TransformedPropRoot, TransformedProp);
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
      AddTempRootIfNeeded(ResultRoot, NewObj);
      try
        for Key in Obj.GetEnumerablePropertyNames do
        begin
          PropValue := Obj.GetProperty(Key);
          AddTempRootIfNeeded(PropValueRoot, PropValue);
          TransformedProp := TransformWithReplacer(Obj, Key, PropValue, AReplacer);
          AddTempRootIfNeeded(TransformedPropRoot, TransformedProp);
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
  finally
    RemoveTempRootIfNeeded(TransformedPropRoot);
    RemoveTempRootIfNeeded(PropValueRoot);
    RemoveTempRootIfNeeded(ResultRoot);
    RemoveTempRootIfNeeded(ReplacedRoot);
  end;
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
    Result := CopyByCodePoints(Result, 10);
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
  HolderRoot, TransformedRoot: TGocciaTempRoot;
begin
  InitializeTempRoot(HolderRoot);
  InitializeTempRoot(TransformedRoot);
  try
    Root := TGocciaObjectValue.Create;
    // The wrapper exists only for this call and is the replacer's `this`, so
    // this local is its only reference for as long as user code can run.
    AddTempRootIfNeeded(HolderRoot, Root);
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

      // The transformed tree is freshly built and reachable from nothing else;
      // serializing it allocates the result string's intermediate values.
      AddTempRootIfNeeded(TransformedRoot, Transformed);
      Result := FStringifier.Stringify(Transformed, AGap, APreferredQuoteChar);
    finally
      FReplacerTraversalStack.Free;
      FReplacerTraversalStack := PreviousTraversalStack;
    end;
  finally
    RemoveTempRootIfNeeded(TransformedRoot);
    RemoveTempRootIfNeeded(HolderRoot);
  end;
end;

function TGocciaJSON5Builtin.StringifyWithAllowList(const AValue: TGocciaValue;
  const AAllowList: TGocciaArrayValue; const AGap: string;
  const APreferredQuoteChar: Char): string;
var
  I: Integer;
  Key: string;
  Keys: TUnicodeStringList;
  Len: Integer;
  Seen: TDictionary<string, Boolean>;
begin
  // Upstream json5 reference: the property list is extracted and de-duplicated
  // once, before serialization, so a wrapper's toString runs once per element.
  Keys := TUnicodeStringList.Create;
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
  PreviousSourceTexts: TUnicodeStringList;
  Reviver: TGocciaValue;
  Root: TGocciaObjectValue;
  SourceTexts: TUnicodeStringList;
  ParsedRoot, HolderRoot: TGocciaTempRoot;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'JSON5.parse', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowTypeError(SErrorJSON5ParseArgMustBeString, SSuggestStringArgRequired);

  HasReviver := (AArgs.Length >= 2) and AArgs.GetElement(1).IsCallable;

  if HasReviver then
  begin
    Reviver := AArgs.GetElement(1);
    SourceTexts := TUnicodeStringList.Create;
    InitializeTempRoot(ParsedRoot);
    InitializeTempRoot(HolderRoot);
    try
      // EGocciaJSON5ParseError descends from EGocciaJSONParseError, and the
      // reviver path goes through the inherited ParseWithSources, which raises the
      // base class — so one arm covers both. A blanket Exception arm here also
      // swallowed the engine's own failures: a refused allocation
      // (TGocciaThrowValue carrying a RangeError, whose Pascal Message is empty by
      // construction) became `SyntaxError: ` with no message, and a ceiling the
      // guest can mistake for a syntax error is a ceiling it can retry in a loop.
      try
        FParser.ParseWithSources(
          AArgs.GetElement(0).ToStringLiteral.Value, Result, SourceTexts);
      except
        on E: EGocciaJSONParseError do
          ThrowSyntaxError(E.Message, SSuggestJSONFormat);
      end;

      // The parsed tree is held only by this frame until the wrapper's property
      // write below, and creating the wrapper allocates.
      AddTempRootIfNeeded(ParsedRoot, Result);
      Root := TGocciaObjectValue.Create;
      // The wrapper is the reviver's `this` and exists solely for this call, so
      // nothing else keeps it alive while the reviver runs user code.
      AddTempRootIfNeeded(HolderRoot, Root);
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
      RemoveTempRootIfNeeded(HolderRoot);
      RemoveTempRootIfNeeded(ParsedRoot);
      SourceTexts.Free;
    end;
  end
  else
  begin
    try
      Result := FParser.Parse(AArgs.GetElement(0).ToStringLiteral.Value);
    except
      on E: EGocciaJSONParseError do
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
    // Same allowlist as JSON.stringify, and for the same reason: the generic arm
    // is for the serializer's own native failures, while the deadline poll, the
    // instruction counter and the property-storage growth gate all reach here
    // too. A ceiling the guest can catch is a ceiling it can ignore in a loop.
    on E: TGocciaTimeoutError do
      raise;
    on E: TGocciaInstructionLimitError do
      raise;
    on E: TGocciaMemoryLimitError do
      raise;
    on E: Exception do
    begin
      if IsEngineIntegrityFault(E) then
        raise;
      ReraiseBytecodeThrow(E);
      ThrowTypeError(Format(SErrorJSON5StringifyError, [E.Message]), SSuggestJSONFormat);
    end;
  end;
end;

end.
