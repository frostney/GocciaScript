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
  Goccia.PatternMatching,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
  Goccia.Token,
  Goccia.Values.ArrayValue,
  Goccia.Values.BigIntValue,
  Goccia.Values.ClassValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.IteratorValue,
  Goccia.Values.ObjectValue,
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

procedure BindPatternName(const AContext: TGocciaEvaluationContext;
  const AName: string; const AValue: TGocciaValue;
  const ADeclarationType: TGocciaDeclarationType);
begin
  AContext.Scope.DefineLexicalBinding(AName, AValue, ADeclarationType);
end;

function GetIteratorFromPatternValue(const AValue: TGocciaValue): TGocciaIteratorValue;
var
  IteratorMethod, IteratorObject, NextMethod: TGocciaValue;
  CallArguments: TGocciaArgumentsCollection;
begin
  if AValue is TGocciaIteratorValue then
    Exit(TGocciaIteratorValue(AValue));

  if AValue is TGocciaArrayValue then
    Exit(TGocciaArrayIteratorValue.Create(AValue, akValues));

  if AValue is TGocciaStringLiteralValue then
    Exit(TGocciaStringIteratorValue.Create(AValue));

  if AValue is TGocciaObjectValue then
  begin
    IteratorMethod := TGocciaObjectValue(AValue).GetSymbolProperty(
      TGocciaSymbolValue.WellKnownIterator);
    if Assigned(IteratorMethod) and not (IteratorMethod is TGocciaUndefinedLiteralValue)
       and IteratorMethod.IsCallable then
    begin
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
    end;
  end;

  Result := nil;
end;

function CollectIterableItems(const AValue: TGocciaValue;
  const AItems: TGocciaValueList): Boolean;
var
  Iterator: TGocciaIteratorValue;
  IterationResult: TGocciaObjectValue;
begin
  Iterator := GetIteratorFromPatternValue(AValue);
  if not Assigned(Iterator) then
    Exit(False);

  IterationResult := Iterator.AdvanceNext;
  while not IterationResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value do
  begin
    AItems.Add(IterationResult.GetProperty(PROP_VALUE));
    IterationResult := Iterator.AdvanceNext;
  end;

  Result := True;
end;

function HasMatchProperty(const ASubject, AKey: TGocciaValue): Boolean;
var
  KeyName: string;
  PropertyValue: TGocciaValue;
begin
  if (AKey is TGocciaSymbolValue) and (ASubject is TGocciaObjectValue) then
    Exit(TGocciaObjectValue(ASubject).HasSymbolProperty(TGocciaSymbolValue(AKey)));

  KeyName := AKey.ToStringLiteral.Value;
  if ASubject is TGocciaObjectValue then
    Exit(TGocciaObjectValue(ASubject).HasProperty(KeyName));

  PropertyValue := ASubject.GetProperty(KeyName);
  Result := Assigned(PropertyValue) and not (PropertyValue is TGocciaUndefinedLiteralValue);
end;

function GetMatchProperty(const ASubject, AKey: TGocciaValue): TGocciaValue;
begin
  if (AKey is TGocciaSymbolValue) and (ASubject is TGocciaObjectValue) then
    Result := TGocciaObjectValue(ASubject).GetSymbolProperty(TGocciaSymbolValue(AKey))
  else
    Result := ASubject.GetProperty(AKey.ToStringLiteral.Value);
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

function BuiltinConstructorMatchesSubject(const AMatcher, ASubject: TGocciaValue): Boolean;
var
  ClassName: string;
begin
  Result := False;
  if not (AMatcher is TGocciaClassValue) then
    Exit;

  ClassName := TGocciaClassValue(AMatcher).Name;
  if ClassName = CONSTRUCTOR_OBJECT then
    Result := ASubject is TGocciaObjectValue
  else if ClassName = CONSTRUCTOR_ARRAY then
    Result := ASubject is TGocciaArrayValue
  else if ClassName = CONSTRUCTOR_STRING then
    Result := ASubject is TGocciaStringLiteralValue
  else if ClassName = CONSTRUCTOR_NUMBER then
    Result := ASubject is TGocciaNumberLiteralValue
  else if ClassName = CONSTRUCTOR_BOOLEAN then
    Result := ASubject is TGocciaBooleanLiteralValue
  else if ClassName = CONSTRUCTOR_BIGINT then
    Result := ASubject is TGocciaBigIntValue
  else if ClassName = CONSTRUCTOR_SYMBOL then
    Result := ASubject is TGocciaSymbolValue
  else if ClassName = CONSTRUCTOR_FUNCTION then
    Result := ASubject.IsCallable;
end;

function TryMatchPatternInternal(const ASubject: TGocciaValue;
  const APattern: TGocciaMatchPattern; const AContext: TGocciaEvaluationContext;
  out AMatchContext: TGocciaEvaluationContext): Boolean; forward;

function TryMatchValuePattern(const ASubject: TGocciaValue;
  const APattern: TGocciaValueMatchPattern; const AContext: TGocciaEvaluationContext): Boolean;
var
  Candidate, CustomMatcher, MatcherResult: TGocciaValue;
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

  if BuiltinConstructorMatchesSubject(Candidate, ASubject) then
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
begin
  if not Assigned(ARestPattern) and not AHasRestWildcard and
     (AItems.Count <> AElements.Count) then
    Exit(False);
  if (Assigned(ARestPattern) or AHasRestWildcard) and
     (AItems.Count < AElements.Count) then
    Exit(False);

  CurrentContext := AContext;
  for I := 0 to AElements.Count - 1 do
  begin
    if not Assigned(AElements[I]) then
      Continue;
    if not TryMatchPatternInternal(AItems[I], AElements[I], CurrentContext, NextContext) then
      Exit(False);
    CurrentContext := NextContext;
  end;

  if Assigned(ARestPattern) then
  begin
    RestArray := TGocciaArrayValue.Create;
    for I := AElements.Count to AItems.Count - 1 do
      RestArray.Elements.Add(AItems[I]);
    if not TryMatchPatternInternal(RestArray, ARestPattern, CurrentContext, NextContext) then
      Exit(False);
    CurrentContext := NextContext;
  end;

  AMatchContext := CurrentContext;
  Result := True;
end;

function TryMatchArrayPattern(const ASubject: TGocciaValue;
  const APattern: TGocciaArrayMatchPattern; const AContext: TGocciaEvaluationContext;
  out AMatchContext: TGocciaEvaluationContext): Boolean;
var
  Items: TGocciaValueList;
begin
  Items := TGocciaValueList.Create(False);
  try
    if not CollectIterableItems(ASubject, Items) then
      Exit(False);
    Result := TryMatchArrayItems(Items, APattern.Elements, APattern.RestPattern,
      APattern.HasRestWildcard, AContext, AMatchContext);
  finally
    Items.Free;
  end;
end;

function TryMatchObjectPattern(const ASubject: TGocciaValue;
  const APattern: TGocciaObjectMatchPattern; const AContext: TGocciaEvaluationContext;
  out AMatchContext: TGocciaEvaluationContext): Boolean;
var
  I: Integer;
  Prop: TGocciaObjectMatchProperty;
  KeyValue, PropertyValue: TGocciaValue;
  CurrentContext, NextContext: TGocciaEvaluationContext;
begin
  if (ASubject is TGocciaNullLiteralValue) or
     (ASubject is TGocciaUndefinedLiteralValue) then
    Exit(False);

  CurrentContext := AContext;
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
    CurrentContext := NextContext;
  end;

  if Assigned(APattern.RestPattern) then
  begin
    if not (ASubject is TGocciaObjectValue) then
      Exit(False);
    if not TryMatchPatternInternal(TGocciaObjectValue.Create,
      APattern.RestPattern, CurrentContext, NextContext) then
      Exit(False);
    CurrentContext := NextContext;
  end;

  AMatchContext := CurrentContext;
  Result := True;
end;

function TryMatchExtractorPattern(const ASubject: TGocciaValue;
  const APattern: TGocciaExtractorMatchPattern; const AContext: TGocciaEvaluationContext;
  out AMatchContext: TGocciaEvaluationContext): Boolean;
var
  Matcher, CustomMatcher, Extracted: TGocciaValue;
  Items: TGocciaValueList;
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
    Items := TGocciaValueList.Create(False);
    if not CollectIterableItems(Extracted, Items) then
    begin
      Items.Free;
      ThrowTypeError('Extractor pattern result must be true, false, or iterable');
    end;
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
      if not TryMatchPatternInternal(ASubject,
         TGocciaAndMatchPattern(APattern).Patterns[I], CurrentContext,
         NextContext) then
        Exit(False);
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
      if TryMatchPatternInternal(ASubject,
         TGocciaOrMatchPattern(APattern).Patterns[I], BranchContext,
         NextContext) then
      begin
        AMatchContext := NextContext;
        Exit(True);
      end;
      BranchContext.Scope.Free;
    end;
    Exit(False);
  end;

  if APattern is TGocciaNotMatchPattern then
  begin
    BranchContext := CreatePatternChildContext(AContext, 'PatternNotBranch');
    try
      Result := not TryMatchPatternInternal(ASubject,
        TGocciaNotMatchPattern(APattern).Pattern, BranchContext, NextContext);
    finally
      BranchContext.Scope.Free;
    end;
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
  Result := TryMatchPatternInternal(ASubject, APattern, ScratchContext, AMatchContext);
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
    MatchContext.Scope.Free;
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
        MatchContext.Scope.Free;
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

    RightResult := EvaluateConditionWithPatternBindings(BinaryExpression.Right,
      LeftContext, RightContext, RightHandled);
    if not RightHandled then
    begin
      RightContext := LeftContext;
      RightResult := EvaluateExpression(BinaryExpression.Right, LeftContext)
        .ToBooleanLiteral.Value;
    end;

    if RightResult then
      ABodyContext := RightContext
    else
    begin
      if LeftHandled and (LeftContext.Scope <> AContext.Scope) then
        LeftContext.Scope.Free;
      ABodyContext := AContext;
    end;
    Result := RightResult;
    Exit;
  end;

  Result := EvaluateExpression(ACondition, AContext).ToBooleanLiteral.Value;
end;

end.
