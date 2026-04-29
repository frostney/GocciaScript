unit Goccia.Evaluator.PatternMatching;

{$I Goccia.inc}

interface

uses
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.Evaluator.Context,
  Goccia.Values.Primitives;

function EvaluateIsExpression(const AExpression: TGocciaIsExpression;
  const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateMatchExpression(const AExpression: TGocciaMatchExpression;
  const AContext: TGocciaEvaluationContext): TGocciaValue;
function TryEvaluateMatchPattern(const ASubject: TGocciaValue;
  const APattern: TGocciaMatchPattern; const AContext: TGocciaEvaluationContext;
  out AMatchContext: TGocciaEvaluationContext): Boolean;
function TryEvaluateMatchPatternInContext(const ASubject: TGocciaValue;
  const APattern: TGocciaMatchPattern; const AContext: TGocciaEvaluationContext;
  out AMatchContext: TGocciaEvaluationContext): Boolean;
function EvaluateConditionWithPatternBindings(const ACondition: TGocciaExpression;
  const AContext: TGocciaEvaluationContext; out ABodyContext: TGocciaEvaluationContext;
  out AHandled: Boolean): Boolean;
procedure ReleaseMatchContext(const AMatchContext,
  AParentContext: TGocciaEvaluationContext);

implementation

uses
  Classes,
  Generics.Collections,
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Evaluator,
  Goccia.Evaluator.Comparison,
  Goccia.GarbageCollector,
  Goccia.PatternMatching,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
  Goccia.Token,
  Goccia.Values.ArrayValue,
  Goccia.Values.BigIntObjectValue,
  Goccia.Values.BigIntValue,
  Goccia.Values.BooleanObjectValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.ClassValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.IteratorValue,
  Goccia.Values.NumberObjectValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.ProxyValue,
  Goccia.Values.StringObjectValue,
  Goccia.Values.SymbolValue;

function BooleanValue(const AValue: Boolean): TGocciaBooleanLiteralValue; inline;
begin
  if AValue then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function CreatePatternChildContext(const AContext: TGocciaEvaluationContext;
  const ALabel: string): TGocciaEvaluationContext;
begin
  Result := AContext;
  Result.Scope := AContext.Scope.CreateChild(skBlock, ALabel);
end;

procedure ReleaseMatchContext(const AMatchContext,
  AParentContext: TGocciaEvaluationContext);
var
  CurrentScope, ParentScope, StopScope: TGocciaScope;
begin
  StopScope := AParentContext.Scope;
  CurrentScope := AMatchContext.Scope;
  while Assigned(CurrentScope) and (CurrentScope <> StopScope) do
  begin
    ParentScope := CurrentScope.Parent;
    CurrentScope.Free;
    CurrentScope := ParentScope;
  end;
end;

procedure BindPatternName(const AContext: TGocciaEvaluationContext;
  const AName: string; const AValue: TGocciaValue;
  const ADeclarationType: TGocciaDeclarationType);
begin
  AContext.Scope.DefineLexicalBinding(AName, AValue, ADeclarationType);
end;

function TryMatchPatternInternal(const ASubject: TGocciaValue;
  const APattern: TGocciaMatchPattern; const AContext: TGocciaEvaluationContext;
  out AMatchContext: TGocciaEvaluationContext): Boolean; forward;

function GetIteratorFromPatternValue(const AValue: TGocciaValue): TGocciaIteratorValue;
var
  IteratorMethod, IteratorObject, NextMethod: TGocciaValue;
  IteratorSource: TGocciaObjectValue;
  CallArguments: TGocciaArgumentsCollection;
begin
  if AValue is TGocciaIteratorValue then
    Exit(TGocciaIteratorValue(AValue));

  if AValue is TGocciaObjectValue then
    IteratorSource := TGocciaObjectValue(AValue)
  else
    IteratorSource := AValue.Box;

  if Assigned(IteratorSource) then
  begin
    IteratorMethod := IteratorSource.GetSymbolProperty(
      TGocciaSymbolValue.WellKnownIterator);
    if not Assigned(IteratorMethod) or (IteratorMethod is TGocciaUndefinedLiteralValue) then
      Exit(nil);
    if not IteratorMethod.IsCallable then
      ThrowTypeError('Object [Symbol.iterator] must be callable');

    CallArguments := TGocciaArgumentsCollection.Create;
    try
      IteratorObject := TGocciaFunctionBase(IteratorMethod).Call(
        CallArguments, AValue);
    finally
      CallArguments.Free;
    end;

    if IteratorObject is TGocciaIteratorValue then
      Exit(TGocciaIteratorValue(IteratorObject));

    if IteratorObject is TGocciaObjectValue then
    begin
      NextMethod := IteratorObject.GetProperty(PROP_NEXT);
      if Assigned(NextMethod) and not (NextMethod is TGocciaUndefinedLiteralValue)
         and NextMethod.IsCallable then
        Exit(TGocciaGenericIteratorValue.Create(IteratorObject));
    end;

    ThrowTypeError('[Symbol.iterator] did not return a valid iterator');
  end;

  Result := nil;
end;

function TryMatchIteratorItems(const AIterator: TGocciaIteratorValue;
  const AElements: TGocciaMatchPatternList; const ARestPattern: TGocciaMatchPattern;
  const AHasRestWildcard: Boolean; const AContext: TGocciaEvaluationContext;
  out AMatchContext: TGocciaEvaluationContext): Boolean;
var
  I: Integer;
  IterationResult: TGocciaObjectValue;
  CurrentContext, NextContext: TGocciaEvaluationContext;
  RestArray: TGocciaArrayValue;
  RestArrayRooted: Boolean;
  Success: Boolean;
begin
  CurrentContext := AContext;
  AMatchContext := AContext;
  Success := False;

  try
    for I := 0 to AElements.Count - 1 do
    begin
      IterationResult := AIterator.AdvanceNext;
      if IterationResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value then
        Exit(False);
      if not Assigned(AElements[I]) then
        Continue;
      if not TryMatchPatternInternal(IterationResult.GetProperty(PROP_VALUE),
         AElements[I], CurrentContext, NextContext) then
        Exit(False);
      CurrentContext := NextContext;
    end;

    if Assigned(ARestPattern) then
    begin
      RestArray := TGocciaArrayValue.Create;
      RestArrayRooted := Assigned(TGarbageCollector.Instance);
      if RestArrayRooted then
        TGarbageCollector.Instance.AddTempRoot(RestArray);
      try
        IterationResult := AIterator.AdvanceNext;
        while not IterationResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value do
        begin
          RestArray.Elements.Add(IterationResult.GetProperty(PROP_VALUE));
          IterationResult := AIterator.AdvanceNext;
        end;
        if not TryMatchPatternInternal(RestArray, ARestPattern,
           CurrentContext, NextContext) then
          Exit(False);
        CurrentContext := NextContext;
      finally
        if RestArrayRooted then
          TGarbageCollector.Instance.RemoveTempRoot(RestArray);
      end;
    end
    else if not AHasRestWildcard then
    begin
      IterationResult := AIterator.AdvanceNext;
      if not IterationResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value then
        Exit(False);
    end;

    AMatchContext := CurrentContext;
    Success := True;
    Result := True;
  finally
    if not Success then
      ReleaseMatchContext(CurrentContext, AContext);
  end;
end;

function BoxPrimitiveForMatch(const ASubject: TGocciaValue): TGocciaObjectValue;
begin
  if ASubject is TGocciaStringLiteralValue then
    Exit(TGocciaStringObjectValue.Create(TGocciaStringLiteralValue(ASubject)));
  if ASubject is TGocciaNumberLiteralValue then
    Exit(TGocciaNumberObjectValue.Create(TGocciaNumberLiteralValue(ASubject)));
  if ASubject is TGocciaBooleanLiteralValue then
    Exit(TGocciaBooleanObjectValue.Create(TGocciaBooleanLiteralValue(ASubject)));
  if ASubject is TGocciaBigIntValue then
    Exit(TGocciaBigIntObjectValue.Create(TGocciaBigIntValue(ASubject)));
  Result := nil;
end;

function HasSymbolPropertyInChain(const AObject: TGocciaObjectValue;
  const ASymbol: TGocciaSymbolValue): Boolean;
var
  Current: TGocciaObjectValue;
begin
  Current := AObject;
  while Assigned(Current) do
  begin
    if Current.HasSymbolProperty(ASymbol) then
      Exit(True);
    Current := Current.Prototype;
  end;
  Result := False;
end;

function HasMatchProperty(const ASubject, AKey: TGocciaValue): Boolean;
var
  KeyName: string;
  BoxedSubject: TGocciaObjectValue;
  SymbolPrototype: TGocciaValue;
  PropertyValue: TGocciaValue;
begin
  if AKey is TGocciaSymbolValue then
  begin
    if ASubject is TGocciaObjectValue then
    begin
      if ASubject is TGocciaProxyValue then
        Exit(TGocciaProxyValue(ASubject).HasSymbolTrap(TGocciaSymbolValue(AKey)));
      Exit(HasSymbolPropertyInChain(TGocciaObjectValue(ASubject),
        TGocciaSymbolValue(AKey)));
    end;

    BoxedSubject := BoxPrimitiveForMatch(ASubject);
    if Assigned(BoxedSubject) then
      Exit(HasSymbolPropertyInChain(BoxedSubject, TGocciaSymbolValue(AKey)));

    if ASubject is TGocciaSymbolValue then
    begin
      SymbolPrototype := TGocciaSymbolValue.SharedPrototype;
      if SymbolPrototype is TGocciaObjectValue then
        Exit(HasSymbolPropertyInChain(TGocciaObjectValue(SymbolPrototype),
          TGocciaSymbolValue(AKey)));
    end;

    Exit(False);
  end;

  KeyName := AKey.ToStringLiteral.Value;
  if ASubject is TGocciaProxyValue then
    Exit(TGocciaProxyValue(ASubject).HasTrap(KeyName));

  if ASubject is TGocciaObjectValue then
    Exit(TGocciaObjectValue(ASubject).HasProperty(KeyName));

  BoxedSubject := BoxPrimitiveForMatch(ASubject);
  if Assigned(BoxedSubject) then
    Exit(BoxedSubject.HasProperty(KeyName));

  if ASubject is TGocciaSymbolValue then
  begin
    SymbolPrototype := TGocciaSymbolValue.SharedPrototype;
    if SymbolPrototype is TGocciaObjectValue then
      Exit(TGocciaObjectValue(SymbolPrototype).HasProperty(KeyName));
  end;

  PropertyValue := ASubject.GetProperty(KeyName);
  Result := Assigned(PropertyValue);
end;

function GetMatchProperty(const ASubject, AKey: TGocciaValue): TGocciaValue;
var
  BoxedSubject: TGocciaObjectValue;
  SymbolPrototype: TGocciaValue;
begin
  if AKey is TGocciaSymbolValue then
  begin
    if ASubject is TGocciaObjectValue then
      Result := TGocciaObjectValue(ASubject).GetSymbolProperty(TGocciaSymbolValue(AKey))
    else
    begin
      BoxedSubject := BoxPrimitiveForMatch(ASubject);
      if Assigned(BoxedSubject) then
        Result := BoxedSubject.GetSymbolProperty(TGocciaSymbolValue(AKey))
      else if ASubject is TGocciaSymbolValue then
      begin
        SymbolPrototype := TGocciaSymbolValue.SharedPrototype;
        if SymbolPrototype is TGocciaObjectValue then
          Result := TGocciaObjectValue(SymbolPrototype).GetSymbolProperty(
            TGocciaSymbolValue(AKey))
        else
          Result := nil;
      end
      else
        Result := nil;
    end;
  end
  else if ASubject is TGocciaObjectValue then
    Result := ASubject.GetProperty(AKey.ToStringLiteral.Value)
  else
  begin
    BoxedSubject := BoxPrimitiveForMatch(ASubject);
    if Assigned(BoxedSubject) then
      Result := BoxedSubject.GetProperty(AKey.ToStringLiteral.Value)
    else
      Result := ASubject.GetProperty(AKey.ToStringLiteral.Value);
  end;
  if not Assigned(Result) then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function CallMatcherFunction(const AMatcherFunction, AThisValue, ASubject: TGocciaValue;
  const AMatchType: string): TGocciaValue;
var
  Arguments: TGocciaArgumentsCollection;
  Hint: TGocciaObjectValue;
begin
  Arguments := TGocciaArgumentsCollection.Create;
  try
    Hint := TGocciaObjectValue.Create;
    Hint.AssignProperty(PROP_MATCH_TYPE, TGocciaStringLiteralValue.Create(AMatchType));
    Arguments.Add(ASubject);
    Arguments.Add(Hint);
    Result := TGocciaFunctionBase(AMatcherFunction).Call(Arguments, AThisValue);
  finally
    Arguments.Free;
  end;
end;

function ClassMatchesSubject(const AMatcher, ASubject: TGocciaValue): Boolean;
begin
  Result := False;
  if not (AMatcher is TGocciaClassValue) then
    Exit;

  if ASubject is TGocciaInstanceValue then
    Exit(TGocciaInstanceValue(ASubject).IsInstanceOf(TGocciaClassValue(AMatcher)));

  if ASubject is TGocciaObjectValue then
    Exit(IsObjectInstanceOfClass(TGocciaObjectValue(ASubject), TGocciaClassValue(AMatcher)));
end;

function GetRootBindingValue(const AContext: TGocciaEvaluationContext;
  const AName: string): TGocciaValue;
var
  RootScope: TGocciaScope;
begin
  Result := nil;
  RootScope := AContext.Scope;
  while Assigned(RootScope) and Assigned(RootScope.Parent) do
    RootScope := RootScope.Parent;
  if Assigned(RootScope) and RootScope.ContainsOwnLexicalBinding(AName) then
    Result := RootScope.GetValue(AName);
end;

function TryPrimitiveBuiltinConstructorMatch(const AMatcher, ASubject: TGocciaValue;
  const AContext: TGocciaEvaluationContext; out AMatches: Boolean): Boolean;
var
  BigIntConstructor, SymbolConstructor: TGocciaValue;
begin
  Result := False;
  AMatches := False;

  BigIntConstructor := GetRootBindingValue(AContext, CONSTRUCTOR_BIGINT);
  if Assigned(BigIntConstructor) and (AMatcher = BigIntConstructor) then
  begin
    AMatches := ASubject is TGocciaBigIntValue;
    Exit(True);
  end;

  SymbolConstructor := GetRootBindingValue(AContext, CONSTRUCTOR_SYMBOL);
  if Assigned(SymbolConstructor) and (AMatcher = SymbolConstructor) then
  begin
    AMatches := ASubject is TGocciaSymbolValue;
    Exit(True);
  end;
end;

function BuiltinConstructorMatchesSubject(const AMatcher, ASubject: TGocciaValue;
  const AContext: TGocciaEvaluationContext): Boolean;
var
  ObjectConstructor: TGocciaValue;
begin
  Result := False;

  if not (AMatcher is TGocciaClassValue) then
    Exit;

  ObjectConstructor := GetRootBindingValue(AContext, CONSTRUCTOR_OBJECT);
  if Assigned(ObjectConstructor) and (AMatcher = ObjectConstructor) then
    Result := ASubject is TGocciaObjectValue
  else if AMatcher is TGocciaArrayClassValue then
    Result := ASubject is TGocciaArrayValue
  else if AMatcher is TGocciaStringClassValue then
    Result := ASubject is TGocciaStringLiteralValue
  else if AMatcher is TGocciaNumberClassValue then
    Result := ASubject is TGocciaNumberLiteralValue
  else if AMatcher is TGocciaBooleanClassValue then
    Result := ASubject is TGocciaBooleanLiteralValue
  else if AMatcher is TGocciaFunctionConstructorClassValue then
    Result := ASubject.IsCallable;
end;

function TryMatchValuePattern(const ASubject: TGocciaValue;
  const APattern: TGocciaValueMatchPattern; const AContext: TGocciaEvaluationContext): Boolean;
var
  Candidate, CustomMatcher, MatcherResult: TGocciaValue;
  PrimitiveBuiltinMatch: Boolean;
begin
  Candidate := EvaluateExpression(APattern.Expression, AContext);

  CustomMatcher := GetCustomMatcher(Candidate);
  if Assigned(CustomMatcher) then
  begin
    if not CustomMatcher.IsCallable then
      ThrowTypeError('Symbol.customMatcher must be callable');
    MatcherResult := CallMatcherFunction(CustomMatcher, Candidate, ASubject, 'boolean');
    Exit(MatcherResult.ToBooleanLiteral.Value);
  end;

  if TryPrimitiveBuiltinConstructorMatch(Candidate, ASubject, AContext,
    PrimitiveBuiltinMatch) then
    Exit(PrimitiveBuiltinMatch);

  if BuiltinConstructorMatchesSubject(Candidate, ASubject, AContext) then
    Exit(True);

  if ClassMatchesSubject(Candidate, ASubject) then
    Exit(True);

  if (Candidate is TGocciaFunctionBase) and not (Candidate is TGocciaClassValue) then
  begin
    MatcherResult := CallMatcherFunction(Candidate, TGocciaUndefinedLiteralValue.UndefinedValue,
      ASubject, 'boolean');
    Exit(MatcherResult.ToBooleanLiteral.Value);
  end;

  if APattern.UseSameValueZero then
    Result := IsSameValueZero(ASubject, Candidate)
  else
    Result := MatchValueEquals(ASubject, Candidate);
end;

function TryMatchArrayItems(const AItems: TGocciaValueList;
  const AElements: TGocciaMatchPatternList; const ARestPattern: TGocciaMatchPattern;
  const AHasRestWildcard: Boolean; const AContext: TGocciaEvaluationContext;
  out AMatchContext: TGocciaEvaluationContext): Boolean;
var
  I: Integer;
  CurrentContext, NextContext: TGocciaEvaluationContext;
  RestArray: TGocciaArrayValue;
  Success: Boolean;
begin
  if not Assigned(ARestPattern) and not AHasRestWildcard and
     (AItems.Count <> AElements.Count) then
    Exit(False);
  if (Assigned(ARestPattern) or AHasRestWildcard) and
     (AItems.Count < AElements.Count) then
    Exit(False);

  CurrentContext := AContext;
  AMatchContext := AContext;
  Success := False;

  try
    for I := 0 to AElements.Count - 1 do
    begin
      if not Assigned(AElements[I]) then
        Continue;
      if not TryMatchPatternInternal(AItems[I], AElements[I], CurrentContext,
         NextContext) then
        Exit(False);
      CurrentContext := NextContext;
    end;

    if Assigned(ARestPattern) then
    begin
      RestArray := TGocciaArrayValue.Create;
      for I := AElements.Count to AItems.Count - 1 do
        RestArray.Elements.Add(AItems[I]);
      if not TryMatchPatternInternal(RestArray, ARestPattern, CurrentContext,
         NextContext) then
        Exit(False);
      CurrentContext := NextContext;
    end;

    AMatchContext := CurrentContext;
    Success := True;
    Result := True;
  finally
    if not Success then
      ReleaseMatchContext(CurrentContext, AContext);
  end;
end;

function TryMatchArrayPattern(const ASubject: TGocciaValue;
  const APattern: TGocciaArrayMatchPattern; const AContext: TGocciaEvaluationContext;
  out AMatchContext: TGocciaEvaluationContext): Boolean;
var
  Iterator: TGocciaIteratorValue;
begin
  Iterator := GetIteratorFromPatternValue(ASubject);
  if not Assigned(Iterator) then
    Exit(False);
  Result := TryMatchIteratorItems(Iterator, APattern.Elements, APattern.RestPattern,
    APattern.HasRestWildcard, AContext, AMatchContext);
end;

function TryMatchObjectPattern(const ASubject: TGocciaValue;
  const APattern: TGocciaObjectMatchPattern; const AContext: TGocciaEvaluationContext;
  out AMatchContext: TGocciaEvaluationContext): Boolean;
var
  I: Integer;
  Prop: TGocciaObjectMatchProperty;
  KeyValue, PropertyValue: TGocciaValue;
  CurrentContext, NextContext: TGocciaEvaluationContext;
  MatchedKeys: TStringList;
  MatchedSymbols: TList<TGocciaSymbolValue>;
  Remainder: TGocciaObjectValue;
  RestSubject: TGocciaObjectValue;
  Entry: TPair<string, TGocciaValue>;
  SymbolEntry: TPair<TGocciaSymbolValue, TGocciaValue>;
  Success: Boolean;
begin
  if (ASubject is TGocciaNullLiteralValue) or
     (ASubject is TGocciaUndefinedLiteralValue) then
    Exit(False);

  MatchedKeys := TStringList.Create;
  MatchedKeys.CaseSensitive := True;
  MatchedSymbols := TList<TGocciaSymbolValue>.Create;
  CurrentContext := AContext;
  AMatchContext := AContext;
  Success := False;
  try
    for I := 0 to APattern.Properties.Count - 1 do
    begin
      Prop := APattern.Properties[I];
      if Prop.Computed then
        KeyValue := EvaluateExpression(Prop.KeyExpression, CurrentContext)
      else
        KeyValue := TGocciaStringLiteralValue.Create(Prop.Key);

      if not HasMatchProperty(ASubject, KeyValue) then
        Exit(False);

      PropertyValue := GetMatchProperty(ASubject, KeyValue);
      if not TryMatchPatternInternal(PropertyValue, Prop.Pattern, CurrentContext, NextContext) then
        Exit(False);

      if KeyValue is TGocciaSymbolValue then
        MatchedSymbols.Add(TGocciaSymbolValue(KeyValue))
      else
        MatchedKeys.Add(KeyValue.ToStringLiteral.Value);
      CurrentContext := NextContext;
    end;

    if Assigned(APattern.RestPattern) then
    begin
      if ASubject is TGocciaObjectValue then
        RestSubject := TGocciaObjectValue(ASubject)
      else
        RestSubject := BoxPrimitiveForMatch(ASubject);

      if not Assigned(RestSubject) then
        Exit(False);

      Remainder := TGocciaObjectValue.Create;
      for Entry in RestSubject.GetEnumerablePropertyEntries do
      begin
        if MatchedKeys.IndexOf(Entry.Key) < 0 then
          Remainder.AssignProperty(Entry.Key, Entry.Value);
      end;
      for SymbolEntry in RestSubject.GetEnumerableSymbolProperties do
      begin
        if not MatchedSymbols.Contains(SymbolEntry.Key) then
          Remainder.AssignSymbolProperty(SymbolEntry.Key, SymbolEntry.Value);
      end;

      if not TryMatchPatternInternal(Remainder, APattern.RestPattern,
        CurrentContext, NextContext) then
        Exit(False);
      CurrentContext := NextContext;
    end;

    AMatchContext := CurrentContext;
    Success := True;
    Result := True;
  finally
    if not Success then
      ReleaseMatchContext(CurrentContext, AContext);
    MatchedSymbols.Free;
    MatchedKeys.Free;
  end;
end;

function TryMatchExtractorPattern(const ASubject: TGocciaValue;
  const APattern: TGocciaExtractorMatchPattern; const AContext: TGocciaEvaluationContext;
  out AMatchContext: TGocciaEvaluationContext): Boolean;
var
  Matcher, CustomMatcher, Extracted: TGocciaValue;
  Items: TGocciaValueList;
  Iterator: TGocciaIteratorValue;
begin
  Matcher := EvaluateExpression(APattern.MatcherExpression, AContext);
  CustomMatcher := GetCustomMatcher(Matcher);
  if not Assigned(CustomMatcher) then
  begin
    if Matcher is TGocciaClassValue then
    begin
      if not ClassMatchesSubject(Matcher, ASubject) then
        Exit(False);
      Extracted := TGocciaBooleanLiteralValue.TrueValue;
    end
    else
      ThrowTypeError('Extractor pattern requires a custom matcher');
  end
  else
  begin
    if not CustomMatcher.IsCallable then
      ThrowTypeError('Symbol.customMatcher must be callable');
    Extracted := CallMatcherFunction(CustomMatcher, Matcher, ASubject, 'extractor');
  end;

  if Extracted is TGocciaBooleanLiteralValue then
  begin
    if not TGocciaBooleanLiteralValue(Extracted).Value then
      Exit(False);
    Items := TGocciaValueList.Create(False);
  end
  else
  begin
    Iterator := GetIteratorFromPatternValue(Extracted);
    if not Assigned(Iterator) then
      ThrowTypeError('Extractor pattern result must be true, false, or iterable');
    Exit(TryMatchIteratorItems(Iterator, APattern.Arguments, APattern.RestPattern,
      APattern.HasRestWildcard, AContext, AMatchContext));
  end;

  try
    Result := TryMatchArrayItems(Items, APattern.Arguments, APattern.RestPattern,
      APattern.HasRestWildcard, AContext, AMatchContext);
  finally
    Items.Free;
  end;
end;

function TryMatchRelationalPattern(const ASubject: TGocciaValue;
  const APattern: TGocciaRelationalMatchPattern; const AContext: TGocciaEvaluationContext): Boolean;
var
  Candidate: TGocciaValue;
begin
  Candidate := EvaluateExpression(APattern.Expression, AContext);
  case APattern.Operator of
    gttLess:         Result := LessThan(ASubject, Candidate);
    gttLessEqual:    Result := LessThanOrEqual(ASubject, Candidate);
    gttGreater:      Result := GreaterThan(ASubject, Candidate);
    gttGreaterEqual: Result := GreaterThanOrEqual(ASubject, Candidate);
  else
    Result := False;
  end;
end;

function TryMatchPatternInternal(const ASubject: TGocciaValue;
  const APattern: TGocciaMatchPattern; const AContext: TGocciaEvaluationContext;
  out AMatchContext: TGocciaEvaluationContext): Boolean;
var
  CurrentContext, NextContext, BranchContext: TGocciaEvaluationContext;
  I: Integer;
  InnerMatched: Boolean;
begin
  AMatchContext := AContext;

  if APattern is TGocciaWildcardMatchPattern then
    Exit(True);

  if APattern is TGocciaBindingMatchPattern then
  begin
    BindPatternName(AContext, TGocciaBindingMatchPattern(APattern).Name, ASubject,
      TGocciaBindingMatchPattern(APattern).DeclarationType);
    Exit(True);
  end;

  if APattern is TGocciaValueMatchPattern then
    Exit(TryMatchValuePattern(ASubject, TGocciaValueMatchPattern(APattern), AContext));

  if APattern is TGocciaArrayMatchPattern then
    Exit(TryMatchArrayPattern(ASubject, TGocciaArrayMatchPattern(APattern), AContext, AMatchContext));

  if APattern is TGocciaObjectMatchPattern then
    Exit(TryMatchObjectPattern(ASubject, TGocciaObjectMatchPattern(APattern), AContext, AMatchContext));

  if APattern is TGocciaRelationalMatchPattern then
    Exit(TryMatchRelationalPattern(ASubject, TGocciaRelationalMatchPattern(APattern), AContext));

  if APattern is TGocciaGuardMatchPattern then
    Exit(EvaluateExpression(TGocciaGuardMatchPattern(APattern).Condition, AContext)
      .ToBooleanLiteral.Value);

  if APattern is TGocciaAsMatchPattern then
  begin
    if not TryMatchPatternInternal(ASubject, TGocciaAsMatchPattern(APattern).Pattern,
       AContext, NextContext) then
      Exit(False);
    BindPatternName(NextContext, TGocciaAsMatchPattern(APattern).Name, ASubject,
      TGocciaAsMatchPattern(APattern).DeclarationType);
    AMatchContext := NextContext;
    Exit(True);
  end;

  if APattern is TGocciaAndMatchPattern then
  begin
    CurrentContext := AContext;
    for I := 0 to TGocciaAndMatchPattern(APattern).Patterns.Count - 1 do
    begin
      try
        if not TryMatchPatternInternal(ASubject,
           TGocciaAndMatchPattern(APattern).Patterns[I], CurrentContext,
           NextContext) then
        begin
          ReleaseMatchContext(CurrentContext, AContext);
          Exit(False);
        end;
      except
        ReleaseMatchContext(CurrentContext, AContext);
        raise;
      end;
      CurrentContext := NextContext;
    end;
    AMatchContext := CurrentContext;
    Exit(True);
  end;

  if APattern is TGocciaOrMatchPattern then
  begin
    for I := 0 to TGocciaOrMatchPattern(APattern).Patterns.Count - 1 do
    begin
      BranchContext := CreatePatternChildContext(AContext, 'PatternOrBranch');
      try
        if TryMatchPatternInternal(ASubject,
           TGocciaOrMatchPattern(APattern).Patterns[I], BranchContext,
           NextContext) then
        begin
          AMatchContext := NextContext;
          Exit(True);
        end;
      except
        ReleaseMatchContext(BranchContext, AContext);
        raise;
      end;
      BranchContext.Scope.Free;
    end;
    Exit(False);
  end;

  if APattern is TGocciaNotMatchPattern then
  begin
    BranchContext := CreatePatternChildContext(AContext, 'PatternNotBranch');
    NextContext := BranchContext;
    try
      InnerMatched := TryMatchPatternInternal(ASubject,
        TGocciaNotMatchPattern(APattern).Pattern, BranchContext, NextContext);
    except
      ReleaseMatchContext(NextContext, AContext);
      raise;
    end;
    if InnerMatched then
      ReleaseMatchContext(NextContext, AContext)
    else if Assigned(BranchContext.Scope) then
      BranchContext.Scope.Free;
    Result := not InnerMatched;
    Exit;
  end;

  if APattern is TGocciaExtractorMatchPattern then
    Exit(TryMatchExtractorPattern(ASubject, TGocciaExtractorMatchPattern(APattern),
      AContext, AMatchContext));

  Result := False;
end;

function TryEvaluateMatchPatternInContext(const ASubject: TGocciaValue;
  const APattern: TGocciaMatchPattern; const AContext: TGocciaEvaluationContext;
  out AMatchContext: TGocciaEvaluationContext): Boolean;
begin
  Result := TryMatchPatternInternal(ASubject, APattern, AContext, AMatchContext);
end;

function TryEvaluateMatchPattern(const ASubject: TGocciaValue;
  const APattern: TGocciaMatchPattern; const AContext: TGocciaEvaluationContext;
  out AMatchContext: TGocciaEvaluationContext): Boolean;
var
  ScratchContext: TGocciaEvaluationContext;
begin
  ScratchContext := CreatePatternChildContext(AContext, 'PatternMatchScope');
  try
    Result := TryMatchPatternInternal(ASubject, APattern, ScratchContext, AMatchContext);
  except
    ScratchContext.Scope.Free;
    AMatchContext := AContext;
    raise;
  end;
  if not Result then
  begin
    ScratchContext.Scope.Free;
    AMatchContext := AContext;
  end;
end;

function EvaluateIsExpression(const AExpression: TGocciaIsExpression;
  const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Subject: TGocciaValue;
  MatchContext: TGocciaEvaluationContext;
begin
  Subject := EvaluateExpression(AExpression.Subject, AContext);
  Result := BooleanValue(TryEvaluateMatchPattern(Subject, AExpression.Pattern,
    AContext, MatchContext));
  if TGocciaBooleanLiteralValue(Result).Value then
    ReleaseMatchContext(MatchContext, AContext);
end;

function EvaluateMatchExpression(const AExpression: TGocciaMatchExpression;
  const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Subject: TGocciaValue;
  MatchContext: TGocciaEvaluationContext;
  I: Integer;
begin
  Subject := EvaluateExpression(AExpression.Subject, AContext);

  for I := 0 to AExpression.Clauses.Count - 1 do
  begin
    if TryEvaluateMatchPattern(Subject, AExpression.Clauses[I].Pattern,
       AContext, MatchContext) then
    begin
      try
        Exit(EvaluateExpression(AExpression.Clauses[I].Expression, MatchContext));
      finally
        ReleaseMatchContext(MatchContext, AContext);
      end;
    end;
  end;

  if Assigned(AExpression.DefaultExpression) then
    Exit(EvaluateExpression(AExpression.DefaultExpression, AContext));

  ThrowNoMatchingPattern;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function EvaluateConditionWithPatternBindings(const ACondition: TGocciaExpression;
  const AContext: TGocciaEvaluationContext; out ABodyContext: TGocciaEvaluationContext;
  out AHandled: Boolean): Boolean;
var
  IsExpression: TGocciaIsExpression;
  BinaryExpression: TGocciaBinaryExpression;
  Subject: TGocciaValue;
  LeftHandled, RightHandled: Boolean;
  LeftResult, RightResult: Boolean;
  LeftContext, RightContext: TGocciaEvaluationContext;
begin
  AHandled := False;
  ABodyContext := AContext;

  if ACondition is TGocciaIsExpression then
  begin
    AHandled := True;
    IsExpression := TGocciaIsExpression(ACondition);
    Subject := EvaluateExpression(IsExpression.Subject, AContext);
    Result := TryEvaluateMatchPattern(Subject, IsExpression.Pattern, AContext,
      ABodyContext);
    if not Result then
      ABodyContext := AContext;
    Exit;
  end;

  if ACondition is TGocciaBinaryExpression then
  begin
    BinaryExpression := TGocciaBinaryExpression(ACondition);
    if BinaryExpression.Operator <> gttAnd then
      Exit(False);

    AHandled := True;
    LeftResult := EvaluateConditionWithPatternBindings(BinaryExpression.Left,
      AContext, LeftContext, LeftHandled);
    if not LeftHandled then
    begin
      LeftContext := AContext;
      LeftResult := EvaluateExpression(BinaryExpression.Left, AContext)
        .ToBooleanLiteral.Value;
    end;

    if not LeftResult then
    begin
      ABodyContext := AContext;
      Result := False;
      Exit;
    end;

    try
      RightResult := EvaluateConditionWithPatternBindings(BinaryExpression.Right,
        LeftContext, RightContext, RightHandled);
      if not RightHandled then
      begin
        RightContext := LeftContext;
        RightResult := EvaluateExpression(BinaryExpression.Right, LeftContext)
          .ToBooleanLiteral.Value;
      end;
    except
      if LeftHandled then
        ReleaseMatchContext(LeftContext, AContext);
      raise;
    end;

    if RightResult then
      ABodyContext := RightContext
    else
    begin
      if LeftHandled then
        ReleaseMatchContext(LeftContext, AContext);
      ABodyContext := AContext;
    end;
    Result := RightResult;
    Exit;
  end;

  Result := EvaluateExpression(ACondition, AContext).ToBooleanLiteral.Value;
end;

end.
