program Goccia.Builtins.TestAssertions.Test;

{$I Goccia.inc}

uses
  Classes,
  Generics.Collections,
  SysUtils,

  TestRunner,

  Goccia.Arguments.Collection,
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.Builtins.TestAssertions,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.ArrayValue,
  Goccia.Values.FunctionValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.SetValue;

type
  TTestExpectationMatchers = class(TestRunner.TTestSuite)
  private
    FScope: TGocciaScope;
    FAssertions: TGocciaTestAssertions;

    procedure DummyThrowError(const AMessage: string; const ALine, AColumn: Integer);

    function MakeExpectation(const AValue: TGocciaValue; const AIsNegated: Boolean = False): TGocciaExpectationValue;

    procedure ExpectPass(const AValue: TGocciaValue);
    procedure ExpectFail(const AValue: TGocciaValue);

    { toBe }
    procedure TestToBeEqualNumbers;
    procedure TestToBeUnequalNumbers;
    procedure TestToBeEqualStrings;
    procedure TestToBeSameObject;
    procedure TestToBeDifferentObjects;
    procedure TestToBeNegated;

    { toEqual }
    procedure TestToEqualPrimitives;
    procedure TestToEqualArrays;
    procedure TestToEqualMismatch;

    { toBeNull }
    procedure TestToBeNullWithNull;
    procedure TestToBeNullWithNonNull;
    procedure TestToBeNullNegated;

    { toBeNaN }
    procedure TestToBeNaNWithNaN;
    procedure TestToBeNaNWithNumber;
    procedure TestToBeNaNWithUndefined;

    { toBeUndefined }
    procedure TestToBeUndefinedWithUndefined;
    procedure TestToBeUndefinedWithValue;
    procedure TestToBeUndefinedNegated;

    { toBeDefined }
    procedure TestToBeDefinedWithValue;
    procedure TestToBeDefinedWithNull;
    procedure TestToBeDefinedWithUndefined;
    procedure TestToBeDefinedNegated;
    procedure TestToBeDefinedNegatedFail;

    { toBeTruthy }
    procedure TestToBeTruthyWithTrue;
    procedure TestToBeTruthyWithNonZero;
    procedure TestToBeTruthyWithString;
    procedure TestToBeTruthyWithZero;
    procedure TestToBeTruthyWithEmptyString;

    { toBeFalsy }
    procedure TestToBeFalsyWithFalse;
    procedure TestToBeFalsyWithZero;
    procedure TestToBeFalsyWithEmptyString;
    procedure TestToBeFalsyWithNull;
    procedure TestToBeFalsyWithUndefined;
    procedure TestToBeFalsyWithNonZero;

    { toBeGreaterThan / toBeGreaterThanOrEqual }
    procedure TestToBeGreaterThanPass;
    procedure TestToBeGreaterThanFail;
    procedure TestToBeGreaterThanOrEqualPass;
    procedure TestToBeGreaterThanOrEqualEqual;

    { toBeLessThan / toBeLessThanOrEqual }
    procedure TestToBeLessThanPass;
    procedure TestToBeLessThanFail;
    procedure TestToBeLessThanOrEqualPass;
    procedure TestToBeLessThanOrEqualEqual;

    { toContain }
    procedure TestToContainSubstring;
    procedure TestToContainSubstringMissing;
    procedure TestToContainArrayElement;
    procedure TestToContainArrayMissing;
    procedure TestToContainSetElement;
    procedure TestToContainOnNonContainer;

    { toHaveLength }
    procedure TestToHaveLengthArray;
    procedure TestToHaveLengthString;
    procedure TestToHaveLengthMismatch;

    { toHaveProperty }
    procedure TestToHavePropertyOnObject;
    procedure TestToHavePropertyMissing;
    procedure TestToHavePropertyNegated;
    procedure TestToHavePropertyNegatedMissing;
    procedure TestToHavePropertyOnNumber;
    procedure TestToHavePropertyOnString;
    procedure TestToHavePropertyOnBoolean;
    procedure TestToHavePropertyOnNull;
    procedure TestToHavePropertyOnUndefined;
    procedure TestToHavePropertyNegatedOnNonObject;

    { toBeCloseTo }
    procedure TestToBeCloseToPass;
    procedure TestToBeCloseToFail;
    procedure TestToBeCloseToCustomPrecision;

    { not (negation getter) }
    procedure TestGetNotReturnsNegatedExpectation;
  public
    procedure BeforeAll; override;
    procedure AfterAll; override;
    procedure BeforeEach; override;
    procedure SetupTests; override;
  end;

  TTestSkipAndConditionalAPIs = class(TestRunner.TTestSuite)
  private
    FScope: TGocciaScope;
    FAssertions: TGocciaTestAssertions;

    procedure DummyThrowError(const AMessage: string; const ALine, AColumn: Integer);
    function CreateNoOpFunction: TGocciaFunctionValue;

    { describe.skip }
    procedure TestDescribeSkipRegistersSkippedSuite;

    { describe.skipIf }
    procedure TestDescribeSkipIfTrueReturnsFunction;
    procedure TestDescribeSkipIfFalseReturnsFunction;
    procedure TestDescribeSkipIfTrueRegistersSkippedSuite;
    procedure TestDescribeSkipIfFalseRegistersNormalSuite;

    { describe.runIf }
    procedure TestDescribeRunIfTrueReturnsFunction;
    procedure TestDescribeRunIfFalseReturnsFunction;
    procedure TestDescribeRunIfTrueRegistersNormalSuite;
    procedure TestDescribeRunIfFalseRegistersSkippedSuite;

    { test.skipIf }
    procedure TestTestSkipIfTrueReturnsFunction;
    procedure TestTestSkipIfFalseReturnsFunction;
    procedure TestTestSkipIfTrueRegistersSkippedTest;
    procedure TestTestSkipIfFalseRegistersNormalTest;

    { test.runIf }
    procedure TestTestRunIfTrueReturnsFunction;
    procedure TestTestRunIfFalseReturnsFunction;
    procedure TestTestRunIfTrueRegistersNormalTest;
    procedure TestTestRunIfFalseRegistersSkippedTest;
  public
    procedure BeforeAll; override;
    procedure AfterAll; override;
    procedure BeforeEach; override;
    procedure AfterEach; override;
    procedure SetupTests; override;
  end;

procedure TTestExpectationMatchers.DummyThrowError(const AMessage: string; const ALine, AColumn: Integer);
begin
  raise Exception.Create(AMessage);
end;

procedure TTestExpectationMatchers.BeforeAll;
begin
  FScope := TGocciaGlobalScope.Create;
  FAssertions := TGocciaTestAssertions.Create('test', FScope, DummyThrowError);
end;

procedure TTestExpectationMatchers.AfterAll;
begin
  FAssertions.Free;
  FScope.Free;
end;

procedure TTestExpectationMatchers.BeforeEach;
begin
  FAssertions.ResetCurrentTestState;
end;

function TTestExpectationMatchers.MakeExpectation(const AValue: TGocciaValue; const AIsNegated: Boolean): TGocciaExpectationValue;
begin
  Result := TGocciaExpectationValue.Create(AValue, FAssertions, AIsNegated);
end;

procedure TTestExpectationMatchers.ExpectPass(const AValue: TGocciaValue);
begin
  Expect<Boolean>(AValue = TGocciaUndefinedLiteralValue.UndefinedValue).ToBe(True);
  Expect<Boolean>(FAssertions.CurrentTestHasFailures).ToBe(False);
end;

procedure TTestExpectationMatchers.ExpectFail(const AValue: TGocciaValue);
begin
  Expect<Boolean>(AValue = TGocciaUndefinedLiteralValue.UndefinedValue).ToBe(True);
  Expect<Boolean>(FAssertions.CurrentTestHasFailures).ToBe(True);
end;

procedure TTestExpectationMatchers.SetupTests;
begin
  { toBe }
  Test('toBe passes for equal numbers', TestToBeEqualNumbers);
  Test('toBe fails for unequal numbers', TestToBeUnequalNumbers);
  Test('toBe passes for equal strings', TestToBeEqualStrings);
  Test('toBe passes for same object reference', TestToBeSameObject);
  Test('toBe fails for different object instances', TestToBeDifferentObjects);
  Test('not.toBe passes for unequal values', TestToBeNegated);

  { toEqual }
  Test('toEqual passes for equal primitives', TestToEqualPrimitives);
  Test('toEqual passes for structurally equal arrays', TestToEqualArrays);
  Test('toEqual fails for mismatched values', TestToEqualMismatch);

  { toBeNull }
  Test('toBeNull passes for null', TestToBeNullWithNull);
  Test('toBeNull fails for non-null', TestToBeNullWithNonNull);
  Test('not.toBeNull passes for non-null', TestToBeNullNegated);

  { toBeNaN }
  Test('toBeNaN passes for NaN', TestToBeNaNWithNaN);
  Test('toBeNaN fails for regular number', TestToBeNaNWithNumber);
  Test('toBeNaN passes for undefined', TestToBeNaNWithUndefined);

  { toBeUndefined }
  Test('toBeUndefined passes for undefined', TestToBeUndefinedWithUndefined);
  Test('toBeUndefined fails for non-undefined', TestToBeUndefinedWithValue);
  Test('not.toBeUndefined passes for non-undefined', TestToBeUndefinedNegated);

  { toBeDefined }
  Test('toBeDefined passes for defined value', TestToBeDefinedWithValue);
  Test('toBeDefined passes for null', TestToBeDefinedWithNull);
  Test('toBeDefined fails for undefined', TestToBeDefinedWithUndefined);
  Test('not.toBeDefined passes for undefined', TestToBeDefinedNegated);
  Test('not.toBeDefined fails for defined value', TestToBeDefinedNegatedFail);

  { toBeTruthy }
  Test('toBeTruthy passes for true', TestToBeTruthyWithTrue);
  Test('toBeTruthy passes for non-zero number', TestToBeTruthyWithNonZero);
  Test('toBeTruthy passes for non-empty string', TestToBeTruthyWithString);
  Test('toBeTruthy fails for zero', TestToBeTruthyWithZero);
  Test('toBeTruthy fails for empty string', TestToBeTruthyWithEmptyString);

  { toBeFalsy }
  Test('toBeFalsy passes for false', TestToBeFalsyWithFalse);
  Test('toBeFalsy passes for zero', TestToBeFalsyWithZero);
  Test('toBeFalsy passes for empty string', TestToBeFalsyWithEmptyString);
  Test('toBeFalsy passes for null', TestToBeFalsyWithNull);
  Test('toBeFalsy passes for undefined', TestToBeFalsyWithUndefined);
  Test('toBeFalsy fails for non-zero', TestToBeFalsyWithNonZero);

  { toBeGreaterThan / toBeGreaterThanOrEqual }
  Test('toBeGreaterThan passes when greater', TestToBeGreaterThanPass);
  Test('toBeGreaterThan fails when not greater', TestToBeGreaterThanFail);
  Test('toBeGreaterThanOrEqual passes when greater', TestToBeGreaterThanOrEqualPass);
  Test('toBeGreaterThanOrEqual passes when equal', TestToBeGreaterThanOrEqualEqual);

  { toBeLessThan / toBeLessThanOrEqual }
  Test('toBeLessThan passes when less', TestToBeLessThanPass);
  Test('toBeLessThan fails when not less', TestToBeLessThanFail);
  Test('toBeLessThanOrEqual passes when less', TestToBeLessThanOrEqualPass);
  Test('toBeLessThanOrEqual passes when equal', TestToBeLessThanOrEqualEqual);

  { toContain }
  Test('toContain passes for substring', TestToContainSubstring);
  Test('toContain fails for missing substring', TestToContainSubstringMissing);
  Test('toContain passes for array element', TestToContainArrayElement);
  Test('toContain fails for missing array element', TestToContainArrayMissing);
  Test('toContain passes for set element', TestToContainSetElement);
  Test('toContain fails for non-container', TestToContainOnNonContainer);

  { toHaveLength }
  Test('toHaveLength passes for correct array length', TestToHaveLengthArray);
  Test('toHaveLength passes for correct string length', TestToHaveLengthString);
  Test('toHaveLength fails for wrong length', TestToHaveLengthMismatch);

  { toHaveProperty }
  Test('toHaveProperty passes for existing property', TestToHavePropertyOnObject);
  Test('toHaveProperty fails for missing property', TestToHavePropertyMissing);
  Test('not.toHaveProperty fails for existing property', TestToHavePropertyNegated);
  Test('not.toHaveProperty passes for missing property', TestToHavePropertyNegatedMissing);
  Test('toHaveProperty on number does not crash', TestToHavePropertyOnNumber);
  Test('toHaveProperty on string does not crash', TestToHavePropertyOnString);
  Test('toHaveProperty on boolean does not crash', TestToHavePropertyOnBoolean);
  Test('toHaveProperty on null does not crash', TestToHavePropertyOnNull);
  Test('toHaveProperty on undefined does not crash', TestToHavePropertyOnUndefined);
  Test('not.toHaveProperty passes for non-object', TestToHavePropertyNegatedOnNonObject);

  { toBeCloseTo }
  Test('toBeCloseTo passes for close values', TestToBeCloseToPass);
  Test('toBeCloseTo fails for distant values', TestToBeCloseToFail);
  Test('toBeCloseTo respects custom precision', TestToBeCloseToCustomPrecision);

  { not }
  Test('not getter returns negated expectation', TestGetNotReturnsNegatedExpectation);
end;

{ ---- toBe ---- }

procedure TTestExpectationMatchers.TestToBeEqualNumbers;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(42)]);
  try
    ExpectPass(MakeExpectation(TGocciaNumberLiteralValue.Create(42)).ToBe(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeUnequalNumbers;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(99)]);
  try
    ExpectFail(MakeExpectation(TGocciaNumberLiteralValue.Create(42)).ToBe(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeEqualStrings;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('hello')]);
  try
    ExpectPass(MakeExpectation(TGocciaStringLiteralValue.Create('hello')).ToBe(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeSameObject;
var
  A: TGocciaArgumentsCollection;
  Obj: TGocciaObjectValue;
begin
  Obj := TGocciaObjectValue.Create;
  A := TGocciaArgumentsCollection.Create([Obj]);
  try
    ExpectPass(MakeExpectation(Obj).ToBe(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeDifferentObjects;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaObjectValue.Create]);
  try
    ExpectFail(MakeExpectation(TGocciaObjectValue.Create).ToBe(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeNegated;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(2)]);
  try
    ExpectPass(MakeExpectation(TGocciaNumberLiteralValue.Create(1), True).ToBe(A, nil));
  finally
    A.Free;
  end;
end;

{ ---- toEqual ---- }

procedure TTestExpectationMatchers.TestToEqualPrimitives;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('abc')]);
  try
    ExpectPass(MakeExpectation(TGocciaStringLiteralValue.Create('abc')).ToEqual(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToEqualArrays;
var
  A: TGocciaArgumentsCollection;
  Arr1, Arr2: TGocciaArrayValue;
begin
  Arr1 := TGocciaArrayValue.Create;
  Arr1.Elements.Add(TGocciaNumberLiteralValue.Create(1));
  Arr1.Elements.Add(TGocciaNumberLiteralValue.Create(2));

  Arr2 := TGocciaArrayValue.Create;
  Arr2.Elements.Add(TGocciaNumberLiteralValue.Create(1));
  Arr2.Elements.Add(TGocciaNumberLiteralValue.Create(2));

  A := TGocciaArgumentsCollection.Create([Arr2]);
  try
    ExpectPass(MakeExpectation(Arr1).ToEqual(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToEqualMismatch;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(2)]);
  try
    ExpectFail(MakeExpectation(TGocciaNumberLiteralValue.Create(1)).ToEqual(A, nil));
  finally
    A.Free;
  end;
end;

{ ---- toBeNull ---- }

procedure TTestExpectationMatchers.TestToBeNullWithNull;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectPass(MakeExpectation(TGocciaNullLiteralValue.Create).ToBeNull(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeNullWithNonNull;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectFail(MakeExpectation(TGocciaNumberLiteralValue.Create(0)).ToBeNull(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeNullNegated;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectPass(MakeExpectation(TGocciaNumberLiteralValue.Create(0), True).ToBeNull(A, nil));
  finally
    A.Free;
  end;
end;

{ ---- toBeNaN ---- }

procedure TTestExpectationMatchers.TestToBeNaNWithNaN;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectPass(MakeExpectation(TGocciaNumberLiteralValue.NaNValue).ToBeNaN(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeNaNWithNumber;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectFail(MakeExpectation(TGocciaNumberLiteralValue.Create(42)).ToBeNaN(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeNaNWithUndefined;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectPass(MakeExpectation(TGocciaUndefinedLiteralValue.UndefinedValue).ToBeNaN(A, nil));
  finally
    A.Free;
  end;
end;

{ ---- toBeUndefined ---- }

procedure TTestExpectationMatchers.TestToBeUndefinedWithUndefined;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectPass(MakeExpectation(TGocciaUndefinedLiteralValue.UndefinedValue).ToBeUndefined(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeUndefinedWithValue;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectFail(MakeExpectation(TGocciaStringLiteralValue.Create('x')).ToBeUndefined(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeUndefinedNegated;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectPass(MakeExpectation(TGocciaStringLiteralValue.Create('x'), True).ToBeUndefined(A, nil));
  finally
    A.Free;
  end;
end;

{ ---- toBeDefined ---- }

procedure TTestExpectationMatchers.TestToBeDefinedWithValue;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectPass(MakeExpectation(TGocciaNumberLiteralValue.Create(42)).ToBeDefined(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeDefinedWithNull;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectPass(MakeExpectation(TGocciaNullLiteralValue.Create).ToBeDefined(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeDefinedWithUndefined;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectFail(MakeExpectation(TGocciaUndefinedLiteralValue.UndefinedValue).ToBeDefined(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeDefinedNegated;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectPass(MakeExpectation(TGocciaUndefinedLiteralValue.UndefinedValue, True).ToBeDefined(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeDefinedNegatedFail;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectFail(MakeExpectation(TGocciaNumberLiteralValue.Create(42), True).ToBeDefined(A, nil));
  finally
    A.Free;
  end;
end;

{ ---- toBeTruthy ---- }

procedure TTestExpectationMatchers.TestToBeTruthyWithTrue;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectPass(MakeExpectation(TGocciaBooleanLiteralValue.Create(True)).ToBeTruthy(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeTruthyWithNonZero;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectPass(MakeExpectation(TGocciaNumberLiteralValue.Create(7)).ToBeTruthy(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeTruthyWithString;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectPass(MakeExpectation(TGocciaStringLiteralValue.Create('hello')).ToBeTruthy(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeTruthyWithZero;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectFail(MakeExpectation(TGocciaNumberLiteralValue.Create(0)).ToBeTruthy(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeTruthyWithEmptyString;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectFail(MakeExpectation(TGocciaStringLiteralValue.Create('')).ToBeTruthy(A, nil));
  finally
    A.Free;
  end;
end;

{ ---- toBeFalsy ---- }

procedure TTestExpectationMatchers.TestToBeFalsyWithFalse;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectPass(MakeExpectation(TGocciaBooleanLiteralValue.Create(False)).ToBeFalsy(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeFalsyWithZero;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectPass(MakeExpectation(TGocciaNumberLiteralValue.Create(0)).ToBeFalsy(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeFalsyWithEmptyString;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectPass(MakeExpectation(TGocciaStringLiteralValue.Create('')).ToBeFalsy(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeFalsyWithNull;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectPass(MakeExpectation(TGocciaNullLiteralValue.Create).ToBeFalsy(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeFalsyWithUndefined;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectPass(MakeExpectation(TGocciaUndefinedLiteralValue.UndefinedValue).ToBeFalsy(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeFalsyWithNonZero;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    ExpectFail(MakeExpectation(TGocciaNumberLiteralValue.Create(1)).ToBeFalsy(A, nil));
  finally
    A.Free;
  end;
end;

{ ---- toBeGreaterThan / toBeGreaterThanOrEqual ---- }

procedure TTestExpectationMatchers.TestToBeGreaterThanPass;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(5)]);
  try
    ExpectPass(MakeExpectation(TGocciaNumberLiteralValue.Create(10)).ToBeGreaterThan(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeGreaterThanFail;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(5)]);
  try
    ExpectFail(MakeExpectation(TGocciaNumberLiteralValue.Create(3)).ToBeGreaterThan(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeGreaterThanOrEqualPass;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(5)]);
  try
    ExpectPass(MakeExpectation(TGocciaNumberLiteralValue.Create(10)).ToBeGreaterThanOrEqual(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeGreaterThanOrEqualEqual;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(5)]);
  try
    ExpectPass(MakeExpectation(TGocciaNumberLiteralValue.Create(5)).ToBeGreaterThanOrEqual(A, nil));
  finally
    A.Free;
  end;
end;

{ ---- toBeLessThan / toBeLessThanOrEqual ---- }

procedure TTestExpectationMatchers.TestToBeLessThanPass;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(10)]);
  try
    ExpectPass(MakeExpectation(TGocciaNumberLiteralValue.Create(3)).ToBeLessThan(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeLessThanFail;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(3)]);
  try
    ExpectFail(MakeExpectation(TGocciaNumberLiteralValue.Create(10)).ToBeLessThan(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeLessThanOrEqualPass;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(10)]);
  try
    ExpectPass(MakeExpectation(TGocciaNumberLiteralValue.Create(3)).ToBeLessThanOrEqual(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeLessThanOrEqualEqual;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(5)]);
  try
    ExpectPass(MakeExpectation(TGocciaNumberLiteralValue.Create(5)).ToBeLessThanOrEqual(A, nil));
  finally
    A.Free;
  end;
end;

{ ---- toContain ---- }

procedure TTestExpectationMatchers.TestToContainSubstring;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('world')]);
  try
    ExpectPass(MakeExpectation(TGocciaStringLiteralValue.Create('hello world')).ToContain(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToContainSubstringMissing;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('xyz')]);
  try
    ExpectFail(MakeExpectation(TGocciaStringLiteralValue.Create('hello')).ToContain(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToContainArrayElement;
var
  A: TGocciaArgumentsCollection;
  Arr: TGocciaArrayValue;
begin
  Arr := TGocciaArrayValue.Create;
  Arr.Elements.Add(TGocciaNumberLiteralValue.Create(1));
  Arr.Elements.Add(TGocciaNumberLiteralValue.Create(2));
  Arr.Elements.Add(TGocciaNumberLiteralValue.Create(3));

  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(2)]);
  try
    ExpectPass(MakeExpectation(Arr).ToContain(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToContainArrayMissing;
var
  A: TGocciaArgumentsCollection;
  Arr: TGocciaArrayValue;
begin
  Arr := TGocciaArrayValue.Create;
  Arr.Elements.Add(TGocciaNumberLiteralValue.Create(1));
  Arr.Elements.Add(TGocciaNumberLiteralValue.Create(2));

  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(99)]);
  try
    ExpectFail(MakeExpectation(Arr).ToContain(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToContainSetElement;
var
  A: TGocciaArgumentsCollection;
  SetVal: TGocciaSetValue;
begin
  SetVal := TGocciaSetValue.Create;
  SetVal.AddItem(TGocciaStringLiteralValue.Create('a'));
  SetVal.AddItem(TGocciaStringLiteralValue.Create('b'));

  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('a')]);
  try
    ExpectPass(MakeExpectation(SetVal).ToContain(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToContainOnNonContainer;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(4)]);
  try
    ExpectFail(MakeExpectation(TGocciaNumberLiteralValue.Create(42)).ToContain(A, nil));
  finally
    A.Free;
  end;
end;

{ ---- toHaveLength ---- }

procedure TTestExpectationMatchers.TestToHaveLengthArray;
var
  A: TGocciaArgumentsCollection;
  Arr: TGocciaArrayValue;
begin
  Arr := TGocciaArrayValue.Create;
  Arr.Elements.Add(TGocciaNumberLiteralValue.Create(10));
  Arr.Elements.Add(TGocciaNumberLiteralValue.Create(20));
  Arr.Elements.Add(TGocciaNumberLiteralValue.Create(30));

  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(3)]);
  try
    ExpectPass(MakeExpectation(Arr).ToHaveLength(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToHaveLengthString;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(5)]);
  try
    ExpectPass(MakeExpectation(TGocciaStringLiteralValue.Create('hello')).ToHaveLength(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToHaveLengthMismatch;
var
  A: TGocciaArgumentsCollection;
  Arr: TGocciaArrayValue;
begin
  Arr := TGocciaArrayValue.Create;
  Arr.Elements.Add(TGocciaNumberLiteralValue.Create(1));

  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(5)]);
  try
    ExpectFail(MakeExpectation(Arr).ToHaveLength(A, nil));
  finally
    A.Free;
  end;
end;

{ ---- toHaveProperty ---- }

procedure TTestExpectationMatchers.TestToHavePropertyOnObject;
var
  A: TGocciaArgumentsCollection;
  Obj: TGocciaObjectValue;
begin
  Obj := TGocciaObjectValue.Create;
  Obj.AssignProperty('name', TGocciaStringLiteralValue.Create('Alice'));

  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('name')]);
  try
    ExpectPass(MakeExpectation(Obj).ToHaveProperty(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToHavePropertyMissing;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('missing')]);
  try
    ExpectFail(MakeExpectation(TGocciaObjectValue.Create).ToHaveProperty(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToHavePropertyNegated;
var
  A: TGocciaArgumentsCollection;
  Obj: TGocciaObjectValue;
begin
  Obj := TGocciaObjectValue.Create;
  Obj.AssignProperty('key', TGocciaNumberLiteralValue.Create(42));

  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('key')]);
  try
    ExpectFail(MakeExpectation(Obj, True).ToHaveProperty(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToHavePropertyNegatedMissing;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('absent')]);
  try
    ExpectPass(MakeExpectation(TGocciaObjectValue.Create, True).ToHaveProperty(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToHavePropertyOnNumber;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('x')]);
  try
    ExpectFail(MakeExpectation(TGocciaNumberLiteralValue.Create(42)).ToHaveProperty(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToHavePropertyOnString;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('x')]);
  try
    ExpectFail(MakeExpectation(TGocciaStringLiteralValue.Create('hello')).ToHaveProperty(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToHavePropertyOnBoolean;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('x')]);
  try
    ExpectFail(MakeExpectation(TGocciaBooleanLiteralValue.Create(True)).ToHaveProperty(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToHavePropertyOnNull;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('x')]);
  try
    ExpectFail(MakeExpectation(TGocciaNullLiteralValue.Create).ToHaveProperty(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToHavePropertyOnUndefined;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('x')]);
  try
    ExpectFail(MakeExpectation(TGocciaUndefinedLiteralValue.UndefinedValue).ToHaveProperty(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToHavePropertyNegatedOnNonObject;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('x')]);
  try
    ExpectPass(MakeExpectation(TGocciaNumberLiteralValue.Create(42), True).ToHaveProperty(A, nil));
  finally
    A.Free;
  end;
end;

{ ---- toBeCloseTo ---- }

procedure TTestExpectationMatchers.TestToBeCloseToPass;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(0.3)]);
  try
    ExpectPass(MakeExpectation(TGocciaNumberLiteralValue.Create(0.1 + 0.2)).ToBeCloseTo(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeCloseToFail;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(0.5)]);
  try
    ExpectFail(MakeExpectation(TGocciaNumberLiteralValue.Create(0.1)).ToBeCloseTo(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToBeCloseToCustomPrecision;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(1.006), TGocciaNumberLiteralValue.Create(2)]);
  try
    ExpectPass(MakeExpectation(TGocciaNumberLiteralValue.Create(1.005)).ToBeCloseTo(A, nil));
  finally
    A.Free;
  end;
end;

{ ---- not ---- }

procedure TTestExpectationMatchers.TestGetNotReturnsNegatedExpectation;
var
  A: TGocciaArgumentsCollection;
  Negated: TGocciaValue;
begin
  A := TGocciaArgumentsCollection.Create([]);
  try
    Negated := MakeExpectation(TGocciaNumberLiteralValue.Create(42)).GetNot(A, nil);
    Expect<Boolean>(Negated is TGocciaExpectationValue).ToBe(True);
  finally
    A.Free;
  end;
end;

{ ---- TTestSkipAndConditionalAPIs ---- }

procedure TTestSkipAndConditionalAPIs.DummyThrowError(const AMessage: string; const ALine, AColumn: Integer);
begin
  raise Exception.Create(AMessage);
end;

function TTestSkipAndConditionalAPIs.CreateNoOpFunction: TGocciaFunctionValue;
var
  Statements: TObjectList<TGocciaASTNode>;
  ParamArray: TGocciaParameterArray;
begin
  Statements := TObjectList<TGocciaASTNode>.Create;
  Statements.Add(TGocciaLiteralExpression.Create(TGocciaUndefinedLiteralValue.UndefinedValue, 0, 0));
  SetLength(ParamArray, 0);
  Result := TGocciaFunctionValue.Create(ParamArray, Statements, FScope, 'noop');
end;

procedure TTestSkipAndConditionalAPIs.BeforeAll;
begin
end;

procedure TTestSkipAndConditionalAPIs.AfterAll;
begin
end;

procedure TTestSkipAndConditionalAPIs.BeforeEach;
begin
  FScope := TGocciaGlobalScope.Create;
  FAssertions := TGocciaTestAssertions.Create('test', FScope, DummyThrowError);
end;

procedure TTestSkipAndConditionalAPIs.AfterEach;
begin
  FAssertions.Free;
  FScope.Free;
end;

procedure TTestSkipAndConditionalAPIs.SetupTests;
begin
  { describe.skip }
  Test('describe.skip registers a skipped suite', TestDescribeSkipRegistersSkippedSuite);

  { describe.skipIf }
  Test('describe.skipIf(true) returns a function', TestDescribeSkipIfTrueReturnsFunction);
  Test('describe.skipIf(false) returns a function', TestDescribeSkipIfFalseReturnsFunction);
  Test('describe.skipIf(true) + call registers skipped suite', TestDescribeSkipIfTrueRegistersSkippedSuite);
  Test('describe.skipIf(false) + call registers normal suite', TestDescribeSkipIfFalseRegistersNormalSuite);

  { describe.runIf }
  Test('describe.runIf(true) returns a function', TestDescribeRunIfTrueReturnsFunction);
  Test('describe.runIf(false) returns a function', TestDescribeRunIfFalseReturnsFunction);
  Test('describe.runIf(true) + call registers normal suite', TestDescribeRunIfTrueRegistersNormalSuite);
  Test('describe.runIf(false) + call registers skipped suite', TestDescribeRunIfFalseRegistersSkippedSuite);

  { test.skipIf }
  Test('test.skipIf(true) returns a function', TestTestSkipIfTrueReturnsFunction);
  Test('test.skipIf(false) returns a function', TestTestSkipIfFalseReturnsFunction);
  Test('test.skipIf(true) + call registers skipped test', TestTestSkipIfTrueRegistersSkippedTest);
  Test('test.skipIf(false) + call registers normal test', TestTestSkipIfFalseRegistersNormalTest);

  { test.runIf }
  Test('test.runIf(true) returns a function', TestTestRunIfTrueReturnsFunction);
  Test('test.runIf(false) returns a function', TestTestRunIfFalseReturnsFunction);
  Test('test.runIf(true) + call registers normal test', TestTestRunIfTrueRegistersNormalTest);
  Test('test.runIf(false) + call registers skipped test', TestTestRunIfFalseRegistersSkippedTest);
end;

{ describe.skip }

procedure TTestSkipAndConditionalAPIs.TestDescribeSkipRegistersSkippedSuite;
var
  A: TGocciaArgumentsCollection;
  ResultObj: TGocciaObjectValue;
  RunArgs: TGocciaArgumentsCollection;
  SilentParams: TGocciaObjectValue;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('skipped suite'), CreateNoOpFunction]);
  try
    FAssertions.DescribeSkip(A, nil);
  finally
    A.Free;
  end;

  SilentParams := TGocciaObjectValue.Create;
  SilentParams.AssignProperty('silent', TGocciaBooleanLiteralValue.Create(True));
  RunArgs := TGocciaArgumentsCollection.Create([SilentParams]);
  try
    ResultObj := FAssertions.RunTests(RunArgs, nil) as TGocciaObjectValue;
    Expect<Double>(ResultObj.GetProperty('skipped').ToNumberLiteral.Value).ToBe(0);
    Expect<Double>(ResultObj.GetProperty('totalRunTests').ToNumberLiteral.Value).ToBe(0);
  finally
    RunArgs.Free;
  end;
end;

{ describe.skipIf }

procedure TTestSkipAndConditionalAPIs.TestDescribeSkipIfTrueReturnsFunction;
var
  A: TGocciaArgumentsCollection;
  R: TGocciaValue;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaBooleanLiteralValue.Create(True)]);
  try
    R := FAssertions.DescribeSkipIf(A, nil);
    Expect<Boolean>(R is TGocciaNativeFunctionValue).ToBe(True);
  finally
    A.Free;
  end;
end;

procedure TTestSkipAndConditionalAPIs.TestDescribeSkipIfFalseReturnsFunction;
var
  A: TGocciaArgumentsCollection;
  R: TGocciaValue;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaBooleanLiteralValue.Create(False)]);
  try
    R := FAssertions.DescribeSkipIf(A, nil);
    Expect<Boolean>(R is TGocciaNativeFunctionValue).ToBe(True);
  finally
    A.Free;
  end;
end;

procedure TTestSkipAndConditionalAPIs.TestDescribeSkipIfTrueRegistersSkippedSuite;
var
  CondArgs, DescArgs, RunArgs: TGocciaArgumentsCollection;
  ReturnedFunc: TGocciaNativeFunctionValue;
  ResultObj: TGocciaObjectValue;
  SilentParams: TGocciaObjectValue;
begin
  CondArgs := TGocciaArgumentsCollection.Create([TGocciaBooleanLiteralValue.Create(True)]);
  try
    ReturnedFunc := FAssertions.DescribeSkipIf(CondArgs, nil) as TGocciaNativeFunctionValue;
  finally
    CondArgs.Free;
  end;

  DescArgs := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('cond-skipped'), CreateNoOpFunction]);
  try
    ReturnedFunc.Call(DescArgs, nil);
  finally
    DescArgs.Free;
  end;

  SilentParams := TGocciaObjectValue.Create;
  SilentParams.AssignProperty('silent', TGocciaBooleanLiteralValue.Create(True));
  RunArgs := TGocciaArgumentsCollection.Create([SilentParams]);
  try
    ResultObj := FAssertions.RunTests(RunArgs, nil) as TGocciaObjectValue;
    Expect<Double>(ResultObj.GetProperty('skipped').ToNumberLiteral.Value).ToBe(0);
  finally
    RunArgs.Free;
  end;
end;

procedure TTestSkipAndConditionalAPIs.TestDescribeSkipIfFalseRegistersNormalSuite;
var
  CondArgs, DescArgs, RunArgs: TGocciaArgumentsCollection;
  ReturnedFunc: TGocciaNativeFunctionValue;
  ResultObj: TGocciaObjectValue;
  SilentParams: TGocciaObjectValue;
begin
  CondArgs := TGocciaArgumentsCollection.Create([TGocciaBooleanLiteralValue.Create(False)]);
  try
    ReturnedFunc := FAssertions.DescribeSkipIf(CondArgs, nil) as TGocciaNativeFunctionValue;
  finally
    CondArgs.Free;
  end;

  DescArgs := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('cond-normal'), CreateNoOpFunction]);
  try
    ReturnedFunc.Call(DescArgs, nil);
  finally
    DescArgs.Free;
  end;

  SilentParams := TGocciaObjectValue.Create;
  SilentParams.AssignProperty('silent', TGocciaBooleanLiteralValue.Create(True));
  RunArgs := TGocciaArgumentsCollection.Create([SilentParams]);
  try
    ResultObj := FAssertions.RunTests(RunArgs, nil) as TGocciaObjectValue;
    Expect<Double>(ResultObj.GetProperty('skipped').ToNumberLiteral.Value).ToBe(0);
    Expect<Double>(ResultObj.GetProperty('failed').ToNumberLiteral.Value).ToBe(0);
  finally
    RunArgs.Free;
  end;
end;

{ describe.runIf }

procedure TTestSkipAndConditionalAPIs.TestDescribeRunIfTrueReturnsFunction;
var
  A: TGocciaArgumentsCollection;
  R: TGocciaValue;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaBooleanLiteralValue.Create(True)]);
  try
    R := FAssertions.DescribeRunIf(A, nil);
    Expect<Boolean>(R is TGocciaNativeFunctionValue).ToBe(True);
  finally
    A.Free;
  end;
end;

procedure TTestSkipAndConditionalAPIs.TestDescribeRunIfFalseReturnsFunction;
var
  A: TGocciaArgumentsCollection;
  R: TGocciaValue;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaBooleanLiteralValue.Create(False)]);
  try
    R := FAssertions.DescribeRunIf(A, nil);
    Expect<Boolean>(R is TGocciaNativeFunctionValue).ToBe(True);
  finally
    A.Free;
  end;
end;

procedure TTestSkipAndConditionalAPIs.TestDescribeRunIfTrueRegistersNormalSuite;
var
  CondArgs, DescArgs, RunArgs: TGocciaArgumentsCollection;
  ReturnedFunc: TGocciaNativeFunctionValue;
  ResultObj: TGocciaObjectValue;
  SilentParams: TGocciaObjectValue;
begin
  CondArgs := TGocciaArgumentsCollection.Create([TGocciaBooleanLiteralValue.Create(True)]);
  try
    ReturnedFunc := FAssertions.DescribeRunIf(CondArgs, nil) as TGocciaNativeFunctionValue;
  finally
    CondArgs.Free;
  end;

  DescArgs := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('runif-normal'), CreateNoOpFunction]);
  try
    ReturnedFunc.Call(DescArgs, nil);
  finally
    DescArgs.Free;
  end;

  SilentParams := TGocciaObjectValue.Create;
  SilentParams.AssignProperty('silent', TGocciaBooleanLiteralValue.Create(True));
  RunArgs := TGocciaArgumentsCollection.Create([SilentParams]);
  try
    ResultObj := FAssertions.RunTests(RunArgs, nil) as TGocciaObjectValue;
    Expect<Double>(ResultObj.GetProperty('skipped').ToNumberLiteral.Value).ToBe(0);
    Expect<Double>(ResultObj.GetProperty('failed').ToNumberLiteral.Value).ToBe(0);
  finally
    RunArgs.Free;
  end;
end;

procedure TTestSkipAndConditionalAPIs.TestDescribeRunIfFalseRegistersSkippedSuite;
var
  CondArgs, DescArgs, RunArgs: TGocciaArgumentsCollection;
  ReturnedFunc: TGocciaNativeFunctionValue;
  ResultObj: TGocciaObjectValue;
  SilentParams: TGocciaObjectValue;
begin
  CondArgs := TGocciaArgumentsCollection.Create([TGocciaBooleanLiteralValue.Create(False)]);
  try
    ReturnedFunc := FAssertions.DescribeRunIf(CondArgs, nil) as TGocciaNativeFunctionValue;
  finally
    CondArgs.Free;
  end;

  DescArgs := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('runif-skipped'), CreateNoOpFunction]);
  try
    ReturnedFunc.Call(DescArgs, nil);
  finally
    DescArgs.Free;
  end;

  SilentParams := TGocciaObjectValue.Create;
  SilentParams.AssignProperty('silent', TGocciaBooleanLiteralValue.Create(True));
  RunArgs := TGocciaArgumentsCollection.Create([SilentParams]);
  try
    ResultObj := FAssertions.RunTests(RunArgs, nil) as TGocciaObjectValue;
    Expect<Double>(ResultObj.GetProperty('skipped').ToNumberLiteral.Value).ToBe(0);
  finally
    RunArgs.Free;
  end;
end;

{ test.skipIf }

procedure TTestSkipAndConditionalAPIs.TestTestSkipIfTrueReturnsFunction;
var
  A: TGocciaArgumentsCollection;
  R: TGocciaValue;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaBooleanLiteralValue.Create(True)]);
  try
    R := FAssertions.TestSkipIf(A, nil);
    Expect<Boolean>(R is TGocciaNativeFunctionValue).ToBe(True);
  finally
    A.Free;
  end;
end;

procedure TTestSkipAndConditionalAPIs.TestTestSkipIfFalseReturnsFunction;
var
  A: TGocciaArgumentsCollection;
  R: TGocciaValue;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaBooleanLiteralValue.Create(False)]);
  try
    R := FAssertions.TestSkipIf(A, nil);
    Expect<Boolean>(R is TGocciaNativeFunctionValue).ToBe(True);
  finally
    A.Free;
  end;
end;

procedure TTestSkipAndConditionalAPIs.TestTestSkipIfTrueRegistersSkippedTest;
var
  CondArgs, TestArgs, RunArgs: TGocciaArgumentsCollection;
  ReturnedFunc: TGocciaNativeFunctionValue;
  ResultObj: TGocciaObjectValue;
  SilentParams: TGocciaObjectValue;
begin
  CondArgs := TGocciaArgumentsCollection.Create([TGocciaBooleanLiteralValue.Create(True)]);
  try
    ReturnedFunc := FAssertions.TestSkipIf(CondArgs, nil) as TGocciaNativeFunctionValue;
  finally
    CondArgs.Free;
  end;

  TestArgs := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('skipped-test'), CreateNoOpFunction]);
  try
    ReturnedFunc.Call(TestArgs, nil);
  finally
    TestArgs.Free;
  end;

  SilentParams := TGocciaObjectValue.Create;
  SilentParams.AssignProperty('silent', TGocciaBooleanLiteralValue.Create(True));
  RunArgs := TGocciaArgumentsCollection.Create([SilentParams]);
  try
    ResultObj := FAssertions.RunTests(RunArgs, nil) as TGocciaObjectValue;
    Expect<Double>(ResultObj.GetProperty('skipped').ToNumberLiteral.Value).ToBe(1);
    Expect<Double>(ResultObj.GetProperty('totalRunTests').ToNumberLiteral.Value).ToBe(1);
  finally
    RunArgs.Free;
  end;
end;

procedure TTestSkipAndConditionalAPIs.TestTestSkipIfFalseRegistersNormalTest;
var
  CondArgs, TestArgs, RunArgs: TGocciaArgumentsCollection;
  ReturnedFunc: TGocciaNativeFunctionValue;
  ResultObj: TGocciaObjectValue;
  SilentParams: TGocciaObjectValue;
begin
  CondArgs := TGocciaArgumentsCollection.Create([TGocciaBooleanLiteralValue.Create(False)]);
  try
    ReturnedFunc := FAssertions.TestSkipIf(CondArgs, nil) as TGocciaNativeFunctionValue;
  finally
    CondArgs.Free;
  end;

  TestArgs := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('normal-test'), CreateNoOpFunction]);
  try
    ReturnedFunc.Call(TestArgs, nil);
  finally
    TestArgs.Free;
  end;

  SilentParams := TGocciaObjectValue.Create;
  SilentParams.AssignProperty('silent', TGocciaBooleanLiteralValue.Create(True));
  RunArgs := TGocciaArgumentsCollection.Create([SilentParams]);
  try
    ResultObj := FAssertions.RunTests(RunArgs, nil) as TGocciaObjectValue;
    Expect<Double>(ResultObj.GetProperty('skipped').ToNumberLiteral.Value).ToBe(0);
    Expect<Double>(ResultObj.GetProperty('passed').ToNumberLiteral.Value).ToBe(1);
  finally
    RunArgs.Free;
  end;
end;

{ test.runIf }

procedure TTestSkipAndConditionalAPIs.TestTestRunIfTrueReturnsFunction;
var
  A: TGocciaArgumentsCollection;
  R: TGocciaValue;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaBooleanLiteralValue.Create(True)]);
  try
    R := FAssertions.TestRunIf(A, nil);
    Expect<Boolean>(R is TGocciaNativeFunctionValue).ToBe(True);
  finally
    A.Free;
  end;
end;

procedure TTestSkipAndConditionalAPIs.TestTestRunIfFalseReturnsFunction;
var
  A: TGocciaArgumentsCollection;
  R: TGocciaValue;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaBooleanLiteralValue.Create(False)]);
  try
    R := FAssertions.TestRunIf(A, nil);
    Expect<Boolean>(R is TGocciaNativeFunctionValue).ToBe(True);
  finally
    A.Free;
  end;
end;

procedure TTestSkipAndConditionalAPIs.TestTestRunIfTrueRegistersNormalTest;
var
  CondArgs, TestArgs, RunArgs: TGocciaArgumentsCollection;
  ReturnedFunc: TGocciaNativeFunctionValue;
  ResultObj: TGocciaObjectValue;
  SilentParams: TGocciaObjectValue;
begin
  CondArgs := TGocciaArgumentsCollection.Create([TGocciaBooleanLiteralValue.Create(True)]);
  try
    ReturnedFunc := FAssertions.TestRunIf(CondArgs, nil) as TGocciaNativeFunctionValue;
  finally
    CondArgs.Free;
  end;

  TestArgs := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('runif-normal'), CreateNoOpFunction]);
  try
    ReturnedFunc.Call(TestArgs, nil);
  finally
    TestArgs.Free;
  end;

  SilentParams := TGocciaObjectValue.Create;
  SilentParams.AssignProperty('silent', TGocciaBooleanLiteralValue.Create(True));
  RunArgs := TGocciaArgumentsCollection.Create([SilentParams]);
  try
    ResultObj := FAssertions.RunTests(RunArgs, nil) as TGocciaObjectValue;
    Expect<Double>(ResultObj.GetProperty('skipped').ToNumberLiteral.Value).ToBe(0);
    Expect<Double>(ResultObj.GetProperty('passed').ToNumberLiteral.Value).ToBe(1);
  finally
    RunArgs.Free;
  end;
end;

procedure TTestSkipAndConditionalAPIs.TestTestRunIfFalseRegistersSkippedTest;
var
  CondArgs, TestArgs, RunArgs: TGocciaArgumentsCollection;
  ReturnedFunc: TGocciaNativeFunctionValue;
  ResultObj: TGocciaObjectValue;
  SilentParams: TGocciaObjectValue;
begin
  CondArgs := TGocciaArgumentsCollection.Create([TGocciaBooleanLiteralValue.Create(False)]);
  try
    ReturnedFunc := FAssertions.TestRunIf(CondArgs, nil) as TGocciaNativeFunctionValue;
  finally
    CondArgs.Free;
  end;

  TestArgs := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('runif-skipped'), CreateNoOpFunction]);
  try
    ReturnedFunc.Call(TestArgs, nil);
  finally
    TestArgs.Free;
  end;

  SilentParams := TGocciaObjectValue.Create;
  SilentParams.AssignProperty('silent', TGocciaBooleanLiteralValue.Create(True));
  RunArgs := TGocciaArgumentsCollection.Create([SilentParams]);
  try
    ResultObj := FAssertions.RunTests(RunArgs, nil) as TGocciaObjectValue;
    Expect<Double>(ResultObj.GetProperty('skipped').ToNumberLiteral.Value).ToBe(1);
    Expect<Double>(ResultObj.GetProperty('totalRunTests').ToNumberLiteral.Value).ToBe(1);
  finally
    RunArgs.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TTestExpectationMatchers.Create('Test Assertions - Expectation Matchers'));
  TestRunnerProgram.AddSuite(TTestSkipAndConditionalAPIs.Create('Test Assertions - Skip and Conditional APIs'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
