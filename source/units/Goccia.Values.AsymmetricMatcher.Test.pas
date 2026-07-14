program Goccia.Values.AsymmetricMatcher.Test;

{$I Goccia.inc}

uses
  TestingPascalLibrary,

  Goccia.Evaluator.Comparison,
  Goccia.TestSetup,
  Goccia.Values.ArrayValue,
  Goccia.Values.AsymmetricMatcher,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TTestAsymmetricMatchers = class(TTestSuite)
  private
    procedure TestAnything;
    procedure TestSymmetricDispatch;
    procedure TestNestedObjectMatcher;
    procedure TestArrayContaining;
    procedure TestInverseMatcher;
  public
    procedure SetupTests; override;
  end;

procedure TTestAsymmetricMatchers.SetupTests;
begin
  Test('Anything excludes null and undefined', TestAnything);
  Test('Matcher dispatch is symmetric', TestSymmetricDispatch);
  Test('Matcher participates in nested object equality',
    TestNestedObjectMatcher);
  Test('Array containing uses recursive equality', TestArrayContaining);
  Test('Inverse matcher negates its result', TestInverseMatcher);
end;

procedure TTestAsymmetricMatchers.TestAnything;
var
  Matcher: TGocciaAnythingMatcherValue;
begin
  Matcher := TGocciaAnythingMatcherValue.Create;
  Expect<Boolean>(IsDeepEqual(TGocciaNumberLiteralValue.ZeroValue,
    Matcher)).ToBe(True);
  Expect<Boolean>(IsDeepEqual(TGocciaNullLiteralValue.NullValue,
    Matcher)).ToBe(False);
  Expect<Boolean>(IsDeepEqual(TGocciaUndefinedLiteralValue.UndefinedValue,
    Matcher)).ToBe(False);
end;

procedure TTestAsymmetricMatchers.TestSymmetricDispatch;
var
  Matcher: TGocciaAnythingMatcherValue;
begin
  Matcher := TGocciaAnythingMatcherValue.Create;
  Expect<Boolean>(IsDeepEqual(Matcher,
    TGocciaStringLiteralValue.Create('value'))).ToBe(True);
end;

procedure TTestAsymmetricMatchers.TestNestedObjectMatcher;
var
  ActualObject, ExpectedObject: TGocciaObjectValue;
begin
  ActualObject := TGocciaObjectValue.Create;
  ActualObject.DefineProperty('value', TGocciaPropertyDescriptorData.Create(
    TGocciaNumberLiteralValue.Create(42), [pfEnumerable]));
  ExpectedObject := TGocciaObjectValue.Create;
  ExpectedObject.DefineProperty('value', TGocciaPropertyDescriptorData.Create(
    TGocciaAnythingMatcherValue.Create, [pfEnumerable]));

  Expect<Boolean>(IsDeepEqual(ActualObject, ExpectedObject)).ToBe(True);
end;

procedure TTestAsymmetricMatchers.TestArrayContaining;
var
  ActualArray, ExpectedArray: TGocciaArrayValue;
  Matcher: TGocciaArrayContainingMatcherValue;
begin
  ActualArray := TGocciaArrayValue.Create;
  ActualArray.Elements.Add(TGocciaStringLiteralValue.Create('extra'));
  ActualArray.Elements.Add(TGocciaNumberLiteralValue.Create(42));
  ExpectedArray := TGocciaArrayValue.Create;
  ExpectedArray.Elements.Add(TGocciaAnythingMatcherValue.Create);
  Matcher := TGocciaArrayContainingMatcherValue.Create(ExpectedArray);

  Expect<Boolean>(IsDeepEqual(ActualArray, Matcher)).ToBe(True);
end;

procedure TTestAsymmetricMatchers.TestInverseMatcher;
var
  ActualArray, ExpectedArray: TGocciaArrayValue;
  Matcher: TGocciaArrayContainingMatcherValue;
begin
  ActualArray := TGocciaArrayValue.Create;
  ActualArray.Elements.Add(TGocciaNumberLiteralValue.Create(1));
  ExpectedArray := TGocciaArrayValue.Create;
  ExpectedArray.Elements.Add(TGocciaNumberLiteralValue.Create(2));
  Matcher := TGocciaArrayContainingMatcherValue.Create(ExpectedArray, True);

  Expect<Boolean>(IsDeepEqual(ActualArray, Matcher)).ToBe(True);
end;

begin
  TestRunnerProgram.AddSuite(TTestAsymmetricMatchers.Create(
    'Asymmetric matcher values'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
