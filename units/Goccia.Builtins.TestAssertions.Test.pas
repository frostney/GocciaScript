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
  Goccia.RegExp.Runtime,
  Goccia.Scope,
  Goccia.TestSetup,
  Goccia.Values.ArrayValue,
  Goccia.Values.FunctionBase,
  Goccia.Values.FunctionValue,
  Goccia.Values.MockFunction,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.PromiseValue,
  Goccia.Values.SetValue,
  Goccia.Values.SymbolValue;

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

    { toContainEqual }
    procedure TestToContainEqualArrayValue;
    procedure TestToContainEqualArrayMissing;
    procedure TestToContainEqualOnNonArray;

    { toStrictEqual }
    procedure TestToStrictEqualArrays;
    procedure TestToStrictEqualMismatch;
    procedure TestToStrictEqualNegated;
    procedure TestToStrictEqualCyclicObjects;

    { toMatchObject }
    procedure TestToMatchObjectSubset;
    procedure TestToMatchObjectNestedSubset;
    procedure TestToMatchObjectMismatch;
    procedure TestToMatchObjectOnNonObject;
    procedure TestToMatchObjectIgnoresPrototypeProperties;
    procedure TestToMatchObjectCyclicSubset;

    { toMatch }
    procedure TestToMatchSubstring;
    procedure TestToMatchMissingSubstring;
    procedure TestToMatchOnNonString;
    procedure TestToMatchRegExpPattern;
    procedure TestToMatchRegExpNegated;

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
    FRecordedEvents: TStringList;

    procedure DummyThrowError(const AMessage: string; const ALine, AColumn: Integer);
    function CreateNoOpFunction: TGocciaFunctionValue;
    function ResolveGlobalCallable(const AName: string): TGocciaFunctionBase;
    function ResolveCallableProperty(const AName, AProperty: string): TGocciaFunctionBase;
    function RunRegisteredTests: TGocciaObjectValue;
    function CreateRunOptions: TGocciaArgumentsCollection;
    function BeforeAllCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function AfterAllCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RecordTestBodyCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RecordFocusedCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RecordNonFocusedCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RecordEachRowCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RegisterBeforeAllSuiteCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RegisterAfterAllSuiteCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RegisterFocusedSuiteCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RegisterNonFocusedSuiteCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RegisterDescribeEachSuiteCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

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

    { lifecycle and structure }
    procedure TestBeforeAllRunsOncePerSuite;
    procedure TestAfterAllRunsAfterSuiteTests;
    procedure TestTestOnlySkipsNonFocusedTests;
    procedure TestItOnlySkipsNonFocusedTests;
    procedure TestDescribeOnlySkipsNonFocusedSuites;
    procedure TestTestTodoRegistersSkippedPlaceholder;
    procedure TestTestEachReturnsCallable;
    procedure TestTestEachRegistersExpandedTests;
    procedure TestDescribeEachReturnsCallable;
    procedure TestDescribeEachRegistersExpandedSuites;
  public
    procedure BeforeAll; override;
    procedure AfterAll; override;
    procedure BeforeEach; override;
    procedure AfterEach; override;
    procedure SetupTests; override;
  end;

  TTestMockAndSpyAPIs = class(TestRunner.TTestSuite)
  private
    FScope: TGocciaScope;
    FAssertions: TGocciaTestAssertions;

    procedure DummyThrowError(const AMessage: string; const ALine, AColumn: Integer);

    function MakeExpectation(const AValue: TGocciaValue; const AIsNegated: Boolean = False): TGocciaExpectationValue;
    procedure ExpectPass(const AValue: TGocciaValue);
    procedure ExpectFail(const AValue: TGocciaValue);

    function CreateMockViaGlobal(const AImpl: TGocciaValue = nil): TGocciaMockFunctionValue;
    function CreateSpyViaGlobal(const ATarget: TGocciaObjectValue; const AMethodName: string): TGocciaMockFunctionValue;
    function CallMockFunction(const AMock: TGocciaMockFunctionValue; const AArgs: array of TGocciaValue): TGocciaValue;

    // Reusable callbacks for mock implementations (FPC 3.2.2 has no anonymous functions)
    function CallbackReturn42(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function CallbackReturn99(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function CallbackReturnDefault(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function CallbackReturnOnce(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function CallbackReturnFirst(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function CallbackReturnSecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function CallbackReturnImpl(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function CallbackReturnOriginal(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function CallbackReturnMocked(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function CallbackReturnUndefined(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    { mock() creation and call tracking }
    procedure TestMockReturnsUndefinedByDefault;
    procedure TestMockTracksCallArguments;
    procedure TestMockTracksMultipleCalls;
    procedure TestMockWithImplementation;
    procedure TestMockDotMockCallsArray;
    procedure TestMockDotMockResultsArray;
    procedure TestMockDotMockLastCall;
    procedure TestMockDotMockLastCallWhenNeverCalled;

    { mockImplementation }
    procedure TestMockImplementationSetsImpl;
    procedure TestMockImplementationReturnsTheMock;

    { mockImplementationOnce }
    procedure TestMockImplementationOnceThenFallback;
    procedure TestMockImplementationOnceChained;

    { mockReturnValue }
    procedure TestMockReturnValuePermanent;
    procedure TestMockReturnValueOverridesImpl;

    { mockReturnValueOnce }
    procedure TestMockReturnValueOnceThenUndefined;
    procedure TestMockReturnValueOnceChained;
    procedure TestMockReturnValueOncePriorityOverPermanent;

    { mockClear / mockReset }
    procedure TestMockClearKeepsImplementation;
    procedure TestMockResetClearsImplementation;

    { mockName / getMockName }
    procedure TestMockNameDefault;
    procedure TestMockNameCustom;

    { spyOn }
    procedure TestSpyOnPassesThrough;
    procedure TestSpyOnTracksCalls;
    procedure TestSpyOnMockImplementationOverrides;
    procedure TestSpyOnMockRestoreRestoresOriginal;
    procedure TestSpyOnMockReturnValueOverrides;

    { mock matchers - toHaveBeenCalled }
    procedure TestToHaveBeenCalledPass;
    procedure TestToHaveBeenCalledFail;
    procedure TestToHaveBeenCalledNegated;

    { mock matchers - toHaveBeenCalledTimes }
    procedure TestToHaveBeenCalledTimesPass;
    procedure TestToHaveBeenCalledTimesFail;

    { mock matchers - toHaveBeenCalledWith }
    procedure TestToHaveBeenCalledWithPass;
    procedure TestToHaveBeenCalledWithFail;
    procedure TestToHaveBeenCalledWithDeepEquality;

    { mock matchers - toHaveBeenLastCalledWith }
    procedure TestToHaveBeenLastCalledWithPass;
    procedure TestToHaveBeenLastCalledWithFail;

    { mock matchers - toHaveBeenNthCalledWith }
    procedure TestToHaveBeenNthCalledWithPass;

    { mock matchers - toHaveReturned }
    procedure TestToHaveReturnedPass;
    procedure TestToHaveReturnedFail;

    { mock matchers - toHaveReturnedTimes }
    procedure TestToHaveReturnedTimesPass;

    { mock matchers - toHaveReturnedWith }
    procedure TestToHaveReturnedWithPass;
    procedure TestToHaveReturnedWithFail;

    { mock matchers - toHaveLastReturnedWith }
    procedure TestToHaveLastReturnedWithPass;

    { mock matchers - toHaveNthReturnedWith }
    procedure TestToHaveNthReturnedWithPass;

    { matchers reject non-mock values }
    procedure TestMatcherRejectsNonMock;
  public
    procedure BeforeAll; override;
    procedure AfterAll; override;
    procedure BeforeEach; override;
    procedure SetupTests; override;
  end;

  TTestOnTestFinished = class(TestRunner.TTestSuite)
  private
    FScope: TGocciaScope;
    FAssertions: TGocciaTestAssertions;
    FRecordedEvents: TStringList;

    procedure DummyThrowError(const AMessage: string; const ALine, AColumn: Integer);
    function ResolveGlobalCallable(const AName: string): TGocciaFunctionBase;
    function CreateRunOptions: TGocciaArgumentsCollection;
    function RunRegisteredTests: TGocciaObjectValue;

    // Callbacks for test body and hooks (FPC 3.2.2 has no anonymous functions)
    function RecordTestBodyCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RecordFinishedCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RecordFinishedACallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RecordFinishedBCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RecordAfterEachCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    // Suite registration callbacks
    function RegisterSuiteWithOnTestFinished(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RegisterSuiteWithMultipleCallbacks(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RegisterSuiteWithAfterEachAndCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RegisterSuiteWithScopedCallbacks(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    // Test body callbacks that register onTestFinished
    function TestBodyWithOnTestFinished(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TestBodyWithMultipleCallbacks(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TestBodyWithAfterEachCallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TestBodyNoCallbacks(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    { onTestFinished }
    procedure TestOnTestFinishedCallbackRuns;
    procedure TestOnTestFinishedMultipleInOrder;
    procedure TestOnTestFinishedRunsAfterAfterEach;
    procedure TestOnTestFinishedScopedToCurrentTest;
  public
    procedure BeforeAll; override;
    procedure AfterAll; override;
    procedure BeforeEach; override;
    procedure AfterEach; override;
    procedure SetupTests; override;
  end;

  TTestResolvesAndRejects = class(TestRunner.TTestSuite)
  private
    FScope: TGocciaScope;
    FAssertions: TGocciaTestAssertions;

    procedure DummyThrowError(const AMessage: string; const ALine, AColumn: Integer);

    function MakeExpectation(const AValue: TGocciaValue; const AIsNegated: Boolean = False): TGocciaExpectationValue;
    procedure ExpectPass(const AValue: TGocciaValue);
    procedure ExpectFail(const AValue: TGocciaValue);

    { resolves }
    procedure TestResolvesWithFulfilledPromise;
    procedure TestResolvesFailsWithRejectedPromise;
    procedure TestResolvesFailsWithNonPromise;
    procedure TestResolvesWithNegation;

    { rejects }
    procedure TestRejectsWithRejectedPromise;
    procedure TestRejectsFailsWithFulfilledPromise;
    procedure TestRejectsFailsWithNonPromise;
  public
    procedure BeforeAll; override;
    procedure AfterAll; override;
    procedure BeforeEach; override;
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
  FAssertions.SuppressOutput := True;
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

  { toContainEqual }
  Test('toContainEqual passes for deep-equal array elements', TestToContainEqualArrayValue);
  Test('toContainEqual fails when deep-equal element is missing', TestToContainEqualArrayMissing);
  Test('toContainEqual on non-array fails cleanly', TestToContainEqualOnNonArray);

  { toStrictEqual }
  Test('toStrictEqual passes for identical nested arrays', TestToStrictEqualArrays);
  Test('toStrictEqual fails for mismatched values', TestToStrictEqualMismatch);
  Test('not.toStrictEqual passes for different values', TestToStrictEqualNegated);
  Test('toStrictEqual handles cyclic objects', TestToStrictEqualCyclicObjects);

  { toMatchObject }
  Test('toMatchObject passes for object subsets', TestToMatchObjectSubset);
  Test('toMatchObject passes for nested object subsets', TestToMatchObjectNestedSubset);
  Test('toMatchObject fails for mismatched subset values', TestToMatchObjectMismatch);
  Test('toMatchObject on non-object fails cleanly', TestToMatchObjectOnNonObject);
  Test('toMatchObject ignores inherited prototype properties', TestToMatchObjectIgnoresPrototypeProperties);
  Test('toMatchObject handles cyclic object subsets', TestToMatchObjectCyclicSubset);

  { toMatch }
  Test('toMatch passes for string substrings', TestToMatchSubstring);
  Test('toMatch fails for missing substrings', TestToMatchMissingSubstring);
  Test('toMatch on non-string fails cleanly', TestToMatchOnNonString);
  Test('toMatch passes for RegExp pattern', TestToMatchRegExpPattern);
  Test('not.toMatch passes for non-matching RegExp', TestToMatchRegExpNegated);

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

{ ---- toContainEqual ---- }

procedure TTestExpectationMatchers.TestToContainEqualArrayValue;
var
  A: TGocciaArgumentsCollection;
  ActualArray: TGocciaArrayValue;
  ExpectedObject: TGocciaObjectValue;
  ActualObject: TGocciaObjectValue;
begin
  ActualArray := TGocciaArrayValue.Create;
  ActualObject := TGocciaObjectValue.Create;
  ActualObject.AssignProperty('id', TGocciaNumberLiteralValue.Create(1));
  ActualObject.AssignProperty('label', TGocciaStringLiteralValue.Create('first'));
  ActualArray.Elements.Add(ActualObject);

  ExpectedObject := TGocciaObjectValue.Create;
  ExpectedObject.AssignProperty('id', TGocciaNumberLiteralValue.Create(1));
  ExpectedObject.AssignProperty('label', TGocciaStringLiteralValue.Create('first'));

  A := TGocciaArgumentsCollection.Create([ExpectedObject]);
  try
    ExpectPass(MakeExpectation(ActualArray).ToContainEqual(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToContainEqualArrayMissing;
var
  A: TGocciaArgumentsCollection;
  ActualArray: TGocciaArrayValue;
  ExpectedObject: TGocciaObjectValue;
  ActualObject: TGocciaObjectValue;
begin
  ActualArray := TGocciaArrayValue.Create;
  ActualObject := TGocciaObjectValue.Create;
  ActualObject.AssignProperty('id', TGocciaNumberLiteralValue.Create(1));
  ActualArray.Elements.Add(ActualObject);

  ExpectedObject := TGocciaObjectValue.Create;
  ExpectedObject.AssignProperty('id', TGocciaNumberLiteralValue.Create(2));

  A := TGocciaArgumentsCollection.Create([ExpectedObject]);
  try
    ExpectFail(MakeExpectation(ActualArray).ToContainEqual(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToContainEqualOnNonArray;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaObjectValue.Create]);
  try
    ExpectFail(MakeExpectation(TGocciaStringLiteralValue.Create('value')).ToContainEqual(A, nil));
  finally
    A.Free;
  end;
end;

{ ---- toStrictEqual ---- }

procedure TTestExpectationMatchers.TestToStrictEqualArrays;
var
  A: TGocciaArgumentsCollection;
  ActualArray, ExpectedArray: TGocciaArrayValue;
  ActualObject, ExpectedObject: TGocciaObjectValue;
begin
  ActualArray := TGocciaArrayValue.Create;
  ExpectedArray := TGocciaArrayValue.Create;

  ActualObject := TGocciaObjectValue.Create;
  ActualObject.AssignProperty('count', TGocciaNumberLiteralValue.Create(3));
  ActualArray.Elements.Add(ActualObject);

  ExpectedObject := TGocciaObjectValue.Create;
  ExpectedObject.AssignProperty('count', TGocciaNumberLiteralValue.Create(3));
  ExpectedArray.Elements.Add(ExpectedObject);

  A := TGocciaArgumentsCollection.Create([ExpectedArray]);
  try
    ExpectPass(MakeExpectation(ActualArray).ToStrictEqual(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToStrictEqualMismatch;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('second')]);
  try
    ExpectFail(MakeExpectation(TGocciaStringLiteralValue.Create('first')).ToStrictEqual(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToStrictEqualNegated;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('second')]);
  try
    ExpectPass(MakeExpectation(TGocciaStringLiteralValue.Create('first'), True).ToStrictEqual(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToStrictEqualCyclicObjects;
var
  A: TGocciaArgumentsCollection;
  ActualObject, ExpectedObject: TGocciaObjectValue;
begin
  ActualObject := TGocciaObjectValue.Create;
  ActualObject.AssignProperty('label', TGocciaStringLiteralValue.Create('node'));
  ActualObject.AssignProperty('self', ActualObject);

  ExpectedObject := TGocciaObjectValue.Create;
  ExpectedObject.AssignProperty('label', TGocciaStringLiteralValue.Create('node'));
  ExpectedObject.AssignProperty('self', ExpectedObject);

  A := TGocciaArgumentsCollection.Create([ExpectedObject]);
  try
    ExpectPass(MakeExpectation(ActualObject).ToStrictEqual(A, nil));
  finally
    A.Free;
  end;
end;

{ ---- toMatchObject ---- }

procedure TTestExpectationMatchers.TestToMatchObjectSubset;
var
  A: TGocciaArgumentsCollection;
  ActualObject, ExpectedObject: TGocciaObjectValue;
begin
  ActualObject := TGocciaObjectValue.Create;
  ActualObject.AssignProperty('id', TGocciaNumberLiteralValue.Create(1));
  ActualObject.AssignProperty('name', TGocciaStringLiteralValue.Create('Ada'));
  ActualObject.AssignProperty('role', TGocciaStringLiteralValue.Create('admin'));

  ExpectedObject := TGocciaObjectValue.Create;
  ExpectedObject.AssignProperty('id', TGocciaNumberLiteralValue.Create(1));
  ExpectedObject.AssignProperty('name', TGocciaStringLiteralValue.Create('Ada'));

  A := TGocciaArgumentsCollection.Create([ExpectedObject]);
  try
    ExpectPass(MakeExpectation(ActualObject).ToMatchObject(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToMatchObjectNestedSubset;
var
  A: TGocciaArgumentsCollection;
  ActualObject, ExpectedObject: TGocciaObjectValue;
  ActualMeta, ExpectedMeta: TGocciaObjectValue;
begin
  ActualMeta := TGocciaObjectValue.Create;
  ActualMeta.AssignProperty('active', TGocciaBooleanLiteralValue.Create(True));
  ActualMeta.AssignProperty('version', TGocciaNumberLiteralValue.Create(2));

  ActualObject := TGocciaObjectValue.Create;
  ActualObject.AssignProperty('meta', ActualMeta);
  ActualObject.AssignProperty('name', TGocciaStringLiteralValue.Create('service'));

  ExpectedMeta := TGocciaObjectValue.Create;
  ExpectedMeta.AssignProperty('active', TGocciaBooleanLiteralValue.Create(True));

  ExpectedObject := TGocciaObjectValue.Create;
  ExpectedObject.AssignProperty('meta', ExpectedMeta);

  A := TGocciaArgumentsCollection.Create([ExpectedObject]);
  try
    ExpectPass(MakeExpectation(ActualObject).ToMatchObject(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToMatchObjectMismatch;
var
  A: TGocciaArgumentsCollection;
  ActualObject, ExpectedObject: TGocciaObjectValue;
begin
  ActualObject := TGocciaObjectValue.Create;
  ActualObject.AssignProperty('id', TGocciaNumberLiteralValue.Create(1));

  ExpectedObject := TGocciaObjectValue.Create;
  ExpectedObject.AssignProperty('id', TGocciaNumberLiteralValue.Create(2));

  A := TGocciaArgumentsCollection.Create([ExpectedObject]);
  try
    ExpectFail(MakeExpectation(ActualObject).ToMatchObject(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToMatchObjectOnNonObject;
var
  A: TGocciaArgumentsCollection;
  ExpectedObject: TGocciaObjectValue;
begin
  ExpectedObject := TGocciaObjectValue.Create;
  ExpectedObject.AssignProperty('id', TGocciaNumberLiteralValue.Create(1));

  A := TGocciaArgumentsCollection.Create([ExpectedObject]);
  try
    ExpectFail(MakeExpectation(TGocciaNumberLiteralValue.Create(7)).ToMatchObject(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToMatchObjectIgnoresPrototypeProperties;
var
  A: TGocciaArgumentsCollection;
  ParentObject, ActualObject, ExpectedObject: TGocciaObjectValue;
begin
  ParentObject := TGocciaObjectValue.Create;
  ParentObject.AssignProperty('inherited', TGocciaBooleanLiteralValue.Create(True));

  ActualObject := TGocciaObjectValue.Create(ParentObject);
  ActualObject.AssignProperty('own', TGocciaNumberLiteralValue.Create(1));

  ExpectedObject := TGocciaObjectValue.Create;
  ExpectedObject.AssignProperty('inherited', TGocciaBooleanLiteralValue.Create(True));

  A := TGocciaArgumentsCollection.Create([ExpectedObject]);
  try
    ExpectFail(MakeExpectation(ActualObject).ToMatchObject(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToMatchObjectCyclicSubset;
var
  A: TGocciaArgumentsCollection;
  ActualObject, ExpectedObject: TGocciaObjectValue;
begin
  ActualObject := TGocciaObjectValue.Create;
  ActualObject.AssignProperty('label', TGocciaStringLiteralValue.Create('node'));
  ActualObject.AssignProperty('extra', TGocciaBooleanLiteralValue.Create(True));
  ActualObject.AssignProperty('self', ActualObject);

  ExpectedObject := TGocciaObjectValue.Create;
  ExpectedObject.AssignProperty('label', TGocciaStringLiteralValue.Create('node'));
  ExpectedObject.AssignProperty('self', ExpectedObject);

  A := TGocciaArgumentsCollection.Create([ExpectedObject]);
  try
    ExpectPass(MakeExpectation(ActualObject).ToMatchObject(A, nil));
  finally
    A.Free;
  end;
end;

{ ---- toMatch ---- }

procedure TTestExpectationMatchers.TestToMatchSubstring;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('world')]);
  try
    ExpectPass(MakeExpectation(TGocciaStringLiteralValue.Create('hello world')).ToMatch(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToMatchMissingSubstring;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('planet')]);
  try
    ExpectFail(MakeExpectation(TGocciaStringLiteralValue.Create('hello world')).ToMatch(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToMatchOnNonString;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('1')]);
  try
    ExpectFail(MakeExpectation(TGocciaNumberLiteralValue.Create(1)).ToMatch(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToMatchRegExpPattern;
var
  A: TGocciaArgumentsCollection;
  RegExpObj: TGocciaValue;
begin
  RegExpObj := CreateRegExpObject('wo.ld', '');
  TGocciaObjectValue(RegExpObj).AssignSymbolProperty(
    TGocciaSymbolValue.WellKnownToStringTag, TGocciaStringLiteralValue.Create('RegExp'));
  A := TGocciaArgumentsCollection.Create([RegExpObj]);
  try
    ExpectPass(MakeExpectation(TGocciaStringLiteralValue.Create('hello world')).ToMatch(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestExpectationMatchers.TestToMatchRegExpNegated;
var
  A: TGocciaArgumentsCollection;
  RegExpObj: TGocciaValue;
begin
  RegExpObj := CreateRegExpObject('z+', '');
  TGocciaObjectValue(RegExpObj).AssignSymbolProperty(
    TGocciaSymbolValue.WellKnownToStringTag, TGocciaStringLiteralValue.Create('RegExp'));
  A := TGocciaArgumentsCollection.Create([RegExpObj]);
  try
    ExpectPass(MakeExpectation(TGocciaStringLiteralValue.Create('hello world'), True).ToMatch(A, nil));
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
    ExpectPass(MakeExpectation(TGocciaNullLiteralValue.NullValue).ToBeNull(A, nil));
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
    ExpectPass(MakeExpectation(TGocciaNullLiteralValue.NullValue).ToBeDefined(A, nil));
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
    ExpectPass(MakeExpectation(TGocciaNullLiteralValue.NullValue).ToBeFalsy(A, nil));
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
    ExpectFail(MakeExpectation(TGocciaNullLiteralValue.NullValue).ToHaveProperty(A, nil));
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

function TTestSkipAndConditionalAPIs.ResolveGlobalCallable(
  const AName: string): TGocciaFunctionBase;
var
  Value: TGocciaValue;
begin
  Value := FScope.ResolveIdentifier(AName);
  if not Assigned(Value) then
    raise Exception.CreateFmt(
      'Expected global callable "%s" to resolve to TGocciaFunctionBase, got nil',
      [AName]
    );

  if not (Value is TGocciaFunctionBase) then
    raise Exception.CreateFmt(
      'Expected global callable "%s" to resolve to TGocciaFunctionBase, got %s',
      [AName, Value.ClassName]
    );

  Result := TGocciaFunctionBase(Value);
end;

function TTestSkipAndConditionalAPIs.ResolveCallableProperty(const AName,
  AProperty: string): TGocciaFunctionBase;
var
  PropertyValue: TGocciaValue;
begin
  PropertyValue := ResolveGlobalCallable(AName).GetProperty(AProperty);
  if not Assigned(PropertyValue) then
    raise Exception.CreateFmt(
      'Expected callable property "%s.%s" to resolve to TGocciaFunctionBase, got nil',
      [AName, AProperty]
    );

  if not (PropertyValue is TGocciaFunctionBase) then
    raise Exception.CreateFmt(
      'Expected callable property "%s.%s" to resolve to TGocciaFunctionBase, got %s',
      [AName, AProperty, PropertyValue.ClassName]
    );

  Result := TGocciaFunctionBase(PropertyValue);
end;

function TTestSkipAndConditionalAPIs.CreateRunOptions: TGocciaArgumentsCollection;
var
  Params: TGocciaObjectValue;
begin
  Params := TGocciaObjectValue.Create;
  Params.AssignProperty('showTestResults', TGocciaBooleanLiteralValue.Create(False));
  Result := TGocciaArgumentsCollection.Create([Params]);
end;

function TTestSkipAndConditionalAPIs.RunRegisteredTests: TGocciaObjectValue;
var
  RunArgs: TGocciaArgumentsCollection;
begin
  RunArgs := CreateRunOptions;
  try
    Result := FAssertions.RunTests(RunArgs, nil) as TGocciaObjectValue;
  finally
    RunArgs.Free;
  end;
end;

function TTestSkipAndConditionalAPIs.BeforeAllCallback(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  FRecordedEvents.Add('beforeAll');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTestSkipAndConditionalAPIs.AfterAllCallback(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  FRecordedEvents.Add('afterAll');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTestSkipAndConditionalAPIs.RecordTestBodyCallback(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  FRecordedEvents.Add('test');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTestSkipAndConditionalAPIs.RecordFocusedCallback(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  FRecordedEvents.Add('focused');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTestSkipAndConditionalAPIs.RecordNonFocusedCallback(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  FRecordedEvents.Add('non-focused');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTestSkipAndConditionalAPIs.RecordEachRowCallback(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  Parts: TStringList;
begin
  Parts := TStringList.Create;
  try
    for I := 0 to AArgs.Length - 1 do
      Parts.Add(AArgs.GetElement(I).ToStringLiteral.Value);
    FRecordedEvents.Add(Parts.CommaText);
  finally
    Parts.Free;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTestSkipAndConditionalAPIs.RegisterBeforeAllSuiteCallback(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  BeforeAllFunc: TGocciaFunctionBase;
  TestFunc: TGocciaFunctionBase;
  HookArgs, TestArgs: TGocciaArgumentsCollection;
begin
  BeforeAllFunc := ResolveGlobalCallable('beforeAll');
  TestFunc := ResolveGlobalCallable('test');

  HookArgs := TGocciaArgumentsCollection.Create([
    TGocciaNativeFunctionValue.Create(BeforeAllCallback, 'beforeAllCallback', 0)
  ]);
  try
    BeforeAllFunc.Call(HookArgs, nil);
  finally
    HookArgs.Free;
  end;

  TestArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('first'),
    TGocciaNativeFunctionValue.Create(RecordTestBodyCallback, 'recordTest', 0)
  ]);
  try
    TestFunc.Call(TestArgs, nil);
  finally
    TestArgs.Free;
  end;

  TestArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('second'),
    TGocciaNativeFunctionValue.Create(RecordTestBodyCallback, 'recordTest', 0)
  ]);
  try
    TestFunc.Call(TestArgs, nil);
  finally
    TestArgs.Free;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTestSkipAndConditionalAPIs.RegisterAfterAllSuiteCallback(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  AfterAllFunc: TGocciaFunctionBase;
  TestFunc: TGocciaFunctionBase;
  HookArgs, TestArgs: TGocciaArgumentsCollection;
begin
  AfterAllFunc := ResolveGlobalCallable('afterAll');
  TestFunc := ResolveGlobalCallable('test');

  TestArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('test'),
    TGocciaNativeFunctionValue.Create(RecordTestBodyCallback, 'recordTest', 0)
  ]);
  try
    TestFunc.Call(TestArgs, nil);
  finally
    TestArgs.Free;
  end;

  HookArgs := TGocciaArgumentsCollection.Create([
    TGocciaNativeFunctionValue.Create(AfterAllCallback, 'afterAllCallback', 0)
  ]);
  try
    AfterAllFunc.Call(HookArgs, nil);
  finally
    HookArgs.Free;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTestSkipAndConditionalAPIs.RegisterFocusedSuiteCallback(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TestFunc: TGocciaFunctionBase;
  TestArgs: TGocciaArgumentsCollection;
begin
  TestFunc := ResolveGlobalCallable('test');
  TestArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('focused-test'),
    TGocciaNativeFunctionValue.Create(RecordFocusedCallback, 'focused', 0)
  ]);
  try
    TestFunc.Call(TestArgs, nil);
  finally
    TestArgs.Free;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTestSkipAndConditionalAPIs.RegisterNonFocusedSuiteCallback(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TestFunc: TGocciaFunctionBase;
  TestArgs: TGocciaArgumentsCollection;
begin
  TestFunc := ResolveGlobalCallable('test');
  TestArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('ordinary-test'),
    TGocciaNativeFunctionValue.Create(RecordNonFocusedCallback, 'ordinary', 0)
  ]);
  try
    TestFunc.Call(TestArgs, nil);
  finally
    TestArgs.Free;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTestSkipAndConditionalAPIs.RegisterDescribeEachSuiteCallback(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TestFunc: TGocciaFunctionBase;
  TestArgs: TGocciaArgumentsCollection;
begin
  TestFunc := ResolveGlobalCallable('test');
  if AArgs.Length > 0 then
    FRecordedEvents.Add('suite:' + AArgs.GetElement(0).ToStringLiteral.Value);

  TestArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('generated-test'),
    TGocciaNativeFunctionValue.Create(RecordTestBodyCallback, 'generatedTest', 0)
  ]);
  try
    TestFunc.Call(TestArgs, nil);
  finally
    TestArgs.Free;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TTestSkipAndConditionalAPIs.BeforeAll;
begin
  FRecordedEvents := TStringList.Create;
end;

procedure TTestSkipAndConditionalAPIs.AfterAll;
begin
  FRecordedEvents.Free;
end;

procedure TTestSkipAndConditionalAPIs.BeforeEach;
begin
  FRecordedEvents.Clear;
  FScope := TGocciaGlobalScope.Create;
  FAssertions := TGocciaTestAssertions.Create('test', FScope, DummyThrowError);
  FAssertions.SuppressOutput := True;
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

  { lifecycle and structure }
  Test('beforeAll runs once per suite', TestBeforeAllRunsOncePerSuite);
  Test('afterAll runs after suite tests', TestAfterAllRunsAfterSuiteTests);
  Test('test.only skips non-focused tests', TestTestOnlySkipsNonFocusedTests);
  Test('it.only skips non-focused tests', TestItOnlySkipsNonFocusedTests);
  Test('describe.only skips non-focused suites', TestDescribeOnlySkipsNonFocusedSuites);
  Test('test.todo registers a skipped placeholder', TestTestTodoRegistersSkippedPlaceholder);
  Test('test.each returns a callable registration function', TestTestEachReturnsCallable);
  Test('test.each expands tests for each row', TestTestEachRegistersExpandedTests);
  Test('describe.each returns a callable registration function', TestDescribeEachReturnsCallable);
  Test('describe.each expands suites for each row', TestDescribeEachRegistersExpandedSuites);
end;

{ describe.skip }

procedure TTestSkipAndConditionalAPIs.TestDescribeSkipRegistersSkippedSuite;
var
  A: TGocciaArgumentsCollection;
  ResultObj: TGocciaObjectValue;
begin
  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('skipped suite'), CreateNoOpFunction]);
  try
    FAssertions.DescribeSkip(A, nil);
  finally
    A.Free;
  end;

  ResultObj := RunRegisteredTests;
  Expect<Double>(ResultObj.GetProperty('skipped').ToNumberLiteral.Value).ToBe(0);
  Expect<Double>(ResultObj.GetProperty('totalRunTests').ToNumberLiteral.Value).ToBe(0);
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
  CondArgs, DescArgs: TGocciaArgumentsCollection;
  ReturnedFunc: TGocciaNativeFunctionValue;
  ResultObj: TGocciaObjectValue;
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

  ResultObj := RunRegisteredTests;
  Expect<Double>(ResultObj.GetProperty('skipped').ToNumberLiteral.Value).ToBe(0);
end;

procedure TTestSkipAndConditionalAPIs.TestDescribeSkipIfFalseRegistersNormalSuite;
var
  CondArgs, DescArgs: TGocciaArgumentsCollection;
  ReturnedFunc: TGocciaNativeFunctionValue;
  ResultObj: TGocciaObjectValue;
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

  ResultObj := RunRegisteredTests;
  Expect<Double>(ResultObj.GetProperty('skipped').ToNumberLiteral.Value).ToBe(0);
  Expect<Double>(ResultObj.GetProperty('failed').ToNumberLiteral.Value).ToBe(0);
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
  CondArgs, DescArgs: TGocciaArgumentsCollection;
  ReturnedFunc: TGocciaNativeFunctionValue;
  ResultObj: TGocciaObjectValue;
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

  ResultObj := RunRegisteredTests;
  Expect<Double>(ResultObj.GetProperty('skipped').ToNumberLiteral.Value).ToBe(0);
  Expect<Double>(ResultObj.GetProperty('failed').ToNumberLiteral.Value).ToBe(0);
end;

procedure TTestSkipAndConditionalAPIs.TestDescribeRunIfFalseRegistersSkippedSuite;
var
  CondArgs, DescArgs: TGocciaArgumentsCollection;
  ReturnedFunc: TGocciaNativeFunctionValue;
  ResultObj: TGocciaObjectValue;
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

  ResultObj := RunRegisteredTests;
  Expect<Double>(ResultObj.GetProperty('skipped').ToNumberLiteral.Value).ToBe(0);
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
  CondArgs, TestArgs: TGocciaArgumentsCollection;
  ReturnedFunc: TGocciaNativeFunctionValue;
  ResultObj: TGocciaObjectValue;
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

  ResultObj := RunRegisteredTests;
  Expect<Double>(ResultObj.GetProperty('skipped').ToNumberLiteral.Value).ToBe(1);
  Expect<Double>(ResultObj.GetProperty('totalRunTests').ToNumberLiteral.Value).ToBe(1);
end;

procedure TTestSkipAndConditionalAPIs.TestTestSkipIfFalseRegistersNormalTest;
var
  CondArgs, TestArgs: TGocciaArgumentsCollection;
  ReturnedFunc: TGocciaNativeFunctionValue;
  ResultObj: TGocciaObjectValue;
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

  ResultObj := RunRegisteredTests;
  Expect<Double>(ResultObj.GetProperty('skipped').ToNumberLiteral.Value).ToBe(0);
  Expect<Double>(ResultObj.GetProperty('passed').ToNumberLiteral.Value).ToBe(1);
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
  CondArgs, TestArgs: TGocciaArgumentsCollection;
  ReturnedFunc: TGocciaNativeFunctionValue;
  ResultObj: TGocciaObjectValue;
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

  ResultObj := RunRegisteredTests;
  Expect<Double>(ResultObj.GetProperty('skipped').ToNumberLiteral.Value).ToBe(0);
  Expect<Double>(ResultObj.GetProperty('passed').ToNumberLiteral.Value).ToBe(1);
end;

procedure TTestSkipAndConditionalAPIs.TestTestRunIfFalseRegistersSkippedTest;
var
  CondArgs, TestArgs: TGocciaArgumentsCollection;
  ReturnedFunc: TGocciaNativeFunctionValue;
  ResultObj: TGocciaObjectValue;
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

  ResultObj := RunRegisteredTests;
  Expect<Double>(ResultObj.GetProperty('skipped').ToNumberLiteral.Value).ToBe(1);
  Expect<Double>(ResultObj.GetProperty('totalRunTests').ToNumberLiteral.Value).ToBe(1);
end;

procedure TTestSkipAndConditionalAPIs.TestBeforeAllRunsOncePerSuite;
var
  DescribeFunc: TGocciaFunctionBase;
  SuiteArgs: TGocciaArgumentsCollection;
  ResultObj: TGocciaObjectValue;
begin
  DescribeFunc := ResolveGlobalCallable('describe');
  SuiteArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('suite'),
    TGocciaNativeFunctionValue.Create(RegisterBeforeAllSuiteCallback, 'registerBeforeAllSuite', 0)
  ]);
  try
    DescribeFunc.Call(SuiteArgs, nil);
  finally
    SuiteArgs.Free;
  end;

  ResultObj := RunRegisteredTests;
  Expect<Double>(ResultObj.GetProperty('passed').ToNumberLiteral.Value).ToBe(2);
  Expect<String>(FRecordedEvents.CommaText).ToBe('beforeAll,test,test');
end;

procedure TTestSkipAndConditionalAPIs.TestAfterAllRunsAfterSuiteTests;
var
  DescribeFunc: TGocciaFunctionBase;
  SuiteArgs: TGocciaArgumentsCollection;
  ResultObj: TGocciaObjectValue;
begin
  DescribeFunc := ResolveGlobalCallable('describe');
  SuiteArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('suite'),
    TGocciaNativeFunctionValue.Create(RegisterAfterAllSuiteCallback, 'registerAfterAllSuite', 0)
  ]);
  try
    DescribeFunc.Call(SuiteArgs, nil);
  finally
    SuiteArgs.Free;
  end;

  ResultObj := RunRegisteredTests;
  Expect<Double>(ResultObj.GetProperty('passed').ToNumberLiteral.Value).ToBe(1);
  Expect<String>(FRecordedEvents.CommaText).ToBe('test,afterAll');
end;

procedure TTestSkipAndConditionalAPIs.TestTestOnlySkipsNonFocusedTests;
var
  TestFunc: TGocciaFunctionBase;
  TestOnlyFunc: TGocciaFunctionBase;
  TestArgs: TGocciaArgumentsCollection;
  ResultObj: TGocciaObjectValue;
begin
  TestFunc := ResolveGlobalCallable('test');
  TestOnlyFunc := ResolveCallableProperty('test', 'only');
  TestArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('ordinary'),
    TGocciaNativeFunctionValue.Create(RecordNonFocusedCallback, 'ordinary', 0)
  ]);
  try
    TestFunc.Call(TestArgs, nil);
  finally
    TestArgs.Free;
  end;

  TestArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('focused'),
    TGocciaNativeFunctionValue.Create(RecordFocusedCallback, 'focused', 0)
  ]);
  try
    TestOnlyFunc.Call(TestArgs, nil);
  finally
    TestArgs.Free;
  end;

  ResultObj := RunRegisteredTests;
  Expect<Double>(ResultObj.GetProperty('passed').ToNumberLiteral.Value).ToBe(1);
  Expect<Double>(ResultObj.GetProperty('skipped').ToNumberLiteral.Value).ToBe(1);
  Expect<String>(FRecordedEvents.CommaText).ToBe('focused');
end;

procedure TTestSkipAndConditionalAPIs.TestItOnlySkipsNonFocusedTests;
var
  TestFunc: TGocciaFunctionBase;
  OnlyFunc: TGocciaFunctionBase;
  TestArgs: TGocciaArgumentsCollection;
  ResultObj: TGocciaObjectValue;
begin
  TestFunc := ResolveGlobalCallable('test');
  OnlyFunc := ResolveCallableProperty('it', 'only');
  TestArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('ordinary'),
    TGocciaNativeFunctionValue.Create(RecordNonFocusedCallback, 'ordinary', 0)
  ]);
  try
    TestFunc.Call(TestArgs, nil);
  finally
    TestArgs.Free;
  end;
  TestArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('focused'),
    TGocciaNativeFunctionValue.Create(RecordFocusedCallback, 'focused', 0)
  ]);
  try
    OnlyFunc.Call(TestArgs, nil);
  finally
    TestArgs.Free;
  end;

  ResultObj := RunRegisteredTests;
  Expect<Double>(ResultObj.GetProperty('passed').ToNumberLiteral.Value).ToBe(1);
  Expect<Double>(ResultObj.GetProperty('skipped').ToNumberLiteral.Value).ToBe(1);
  Expect<String>(FRecordedEvents.CommaText).ToBe('focused');
end;

procedure TTestSkipAndConditionalAPIs.TestDescribeOnlySkipsNonFocusedSuites;
var
  DescribeFunc: TGocciaFunctionBase;
  DescribeOnlyFunc: TGocciaFunctionBase;
  SuiteArgs: TGocciaArgumentsCollection;
  ResultObj: TGocciaObjectValue;
begin
  DescribeFunc := ResolveGlobalCallable('describe');
  DescribeOnlyFunc := ResolveCallableProperty('describe', 'only');
  SuiteArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('ordinary-suite'),
    TGocciaNativeFunctionValue.Create(RegisterNonFocusedSuiteCallback, 'ordinarySuite', 0)
  ]);
  try
    DescribeFunc.Call(SuiteArgs, nil);
  finally
    SuiteArgs.Free;
  end;

  SuiteArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('focused-suite'),
    TGocciaNativeFunctionValue.Create(RegisterFocusedSuiteCallback, 'focusedSuite', 0)
  ]);
  try
    DescribeOnlyFunc.Call(SuiteArgs, nil);
  finally
    SuiteArgs.Free;
  end;

  ResultObj := RunRegisteredTests;
  Expect<Double>(ResultObj.GetProperty('passed').ToNumberLiteral.Value).ToBe(1);
  Expect<Double>(ResultObj.GetProperty('skipped').ToNumberLiteral.Value).ToBe(1);
  Expect<String>(FRecordedEvents.CommaText).ToBe('focused');
end;

procedure TTestSkipAndConditionalAPIs.TestTestTodoRegistersSkippedPlaceholder;
var
  TodoFunc: TGocciaFunctionBase;
  TodoArgs: TGocciaArgumentsCollection;
  ResultObj: TGocciaObjectValue;
begin
  TodoFunc := ResolveCallableProperty('test', 'todo');
  TodoArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('todo')
  ]);
  try
    TodoFunc.Call(TodoArgs, nil);
  finally
    TodoArgs.Free;
  end;

  ResultObj := RunRegisteredTests;
  Expect<Double>(ResultObj.GetProperty('skipped').ToNumberLiteral.Value).ToBe(1);
  Expect<Double>(ResultObj.GetProperty('passed').ToNumberLiteral.Value).ToBe(0);
  Expect<Integer>(FRecordedEvents.Count).ToBe(0);
end;

procedure TTestSkipAndConditionalAPIs.TestTestEachReturnsCallable;
var
  TestEachFunc: TGocciaFunctionBase;
  EachArgs: TGocciaArgumentsCollection;
  Table: TGocciaArrayValue;
  ResultValue: TGocciaValue;
begin
  TestEachFunc := ResolveCallableProperty('test', 'each');
  Table := TGocciaArrayValue.Create;
  Table.Elements.Add(TGocciaArrayValue.Create);
  EachArgs := TGocciaArgumentsCollection.Create([Table]);
  try
    ResultValue := TestEachFunc.Call(EachArgs, nil);
    Expect<Boolean>(ResultValue.IsCallable).ToBe(True);
  finally
    EachArgs.Free;
  end;
end;

procedure TTestSkipAndConditionalAPIs.TestTestEachRegistersExpandedTests;
var
  TestEachFunc: TGocciaFunctionBase;
  EachArgs, RegisterArgs: TGocciaArgumentsCollection;
  Table, Row: TGocciaArrayValue;
  ReturnedFunc: TGocciaFunctionBase;
  ResultObj: TGocciaObjectValue;
begin
  TestEachFunc := ResolveCallableProperty('test', 'each');
  Table := TGocciaArrayValue.Create;

  Row := TGocciaArrayValue.Create;
  Row.Elements.Add(TGocciaNumberLiteralValue.Create(1));
  Row.Elements.Add(TGocciaNumberLiteralValue.Create(2));
  Table.Elements.Add(Row);

  Row := TGocciaArrayValue.Create;
  Row.Elements.Add(TGocciaStringLiteralValue.Create('alpha'));
  Table.Elements.Add(Row);

  EachArgs := TGocciaArgumentsCollection.Create([Table]);
  try
    ReturnedFunc := TestEachFunc.Call(EachArgs, nil) as TGocciaFunctionBase;
  finally
    EachArgs.Free;
  end;

  RegisterArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('row %s'),
    TGocciaNativeFunctionValue.Create(RecordEachRowCallback, 'recordEach', -1)
  ]);
  try
    ReturnedFunc.Call(RegisterArgs, nil);
  finally
    RegisterArgs.Free;
  end;

  ResultObj := RunRegisteredTests;
  Expect<Double>(ResultObj.GetProperty('passed').ToNumberLiteral.Value).ToBe(2);
  Expect<String>(FRecordedEvents[0]).ToBe('1,2');
  Expect<String>(FRecordedEvents[1]).ToBe('alpha');
end;

procedure TTestSkipAndConditionalAPIs.TestDescribeEachReturnsCallable;
var
  DescribeEachFunc: TGocciaFunctionBase;
  EachArgs: TGocciaArgumentsCollection;
  Table: TGocciaArrayValue;
  ResultValue: TGocciaValue;
begin
  DescribeEachFunc := ResolveCallableProperty('describe', 'each');
  Table := TGocciaArrayValue.Create;
  Table.Elements.Add(TGocciaArrayValue.Create);
  EachArgs := TGocciaArgumentsCollection.Create([Table]);
  try
    ResultValue := DescribeEachFunc.Call(EachArgs, nil);
    Expect<Boolean>(ResultValue.IsCallable).ToBe(True);
  finally
    EachArgs.Free;
  end;
end;

procedure TTestSkipAndConditionalAPIs.TestDescribeEachRegistersExpandedSuites;
var
  DescribeEachFunc: TGocciaFunctionBase;
  EachArgs, RegisterArgs: TGocciaArgumentsCollection;
  Table, Row: TGocciaArrayValue;
  ReturnedFunc: TGocciaFunctionBase;
  ResultObj: TGocciaObjectValue;
begin
  DescribeEachFunc := ResolveCallableProperty('describe', 'each');
  Table := TGocciaArrayValue.Create;

  Row := TGocciaArrayValue.Create;
  Row.Elements.Add(TGocciaStringLiteralValue.Create('one'));
  Table.Elements.Add(Row);

  Row := TGocciaArrayValue.Create;
  Row.Elements.Add(TGocciaStringLiteralValue.Create('two'));
  Table.Elements.Add(Row);

  EachArgs := TGocciaArgumentsCollection.Create([Table]);
  try
    ReturnedFunc := DescribeEachFunc.Call(EachArgs, nil) as TGocciaFunctionBase;
  finally
    EachArgs.Free;
  end;

  RegisterArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('suite %s'),
    TGocciaNativeFunctionValue.Create(RegisterDescribeEachSuiteCallback, 'describeEachSuite', -1)
  ]);
  try
    ReturnedFunc.Call(RegisterArgs, nil);
  finally
    RegisterArgs.Free;
  end;

  ResultObj := RunRegisteredTests;
  Expect<Double>(ResultObj.GetProperty('passed').ToNumberLiteral.Value).ToBe(2);
  Expect<Integer>(FRecordedEvents.Count).ToBe(4);
  Expect<String>(FRecordedEvents[0]).ToBe('suite:one');
  Expect<String>(FRecordedEvents[1]).ToBe('suite:two');
end;

{ ======== TTestMockAndSpyAPIs ======== }

procedure TTestMockAndSpyAPIs.DummyThrowError(const AMessage: string; const ALine, AColumn: Integer);
begin
  raise Exception.Create(AMessage);
end;

procedure TTestMockAndSpyAPIs.BeforeAll;
begin
  FScope := TGocciaGlobalScope.Create;
  FAssertions := TGocciaTestAssertions.Create('test', FScope, DummyThrowError);
  FAssertions.SuppressOutput := True;
end;

procedure TTestMockAndSpyAPIs.AfterAll;
begin
  FAssertions.Free;
  FScope.Free;
end;

procedure TTestMockAndSpyAPIs.BeforeEach;
begin
  FAssertions.ResetCurrentTestState;
end;

function TTestMockAndSpyAPIs.MakeExpectation(const AValue: TGocciaValue; const AIsNegated: Boolean): TGocciaExpectationValue;
begin
  Result := TGocciaExpectationValue.Create(AValue, FAssertions, AIsNegated);
end;

procedure TTestMockAndSpyAPIs.ExpectPass(const AValue: TGocciaValue);
begin
  Expect<Boolean>(AValue = TGocciaUndefinedLiteralValue.UndefinedValue).ToBe(True);
  Expect<Boolean>(FAssertions.CurrentTestHasFailures).ToBe(False);
end;

procedure TTestMockAndSpyAPIs.ExpectFail(const AValue: TGocciaValue);
begin
  Expect<Boolean>(AValue = TGocciaUndefinedLiteralValue.UndefinedValue).ToBe(True);
  Expect<Boolean>(FAssertions.CurrentTestHasFailures).ToBe(True);
end;

function TTestMockAndSpyAPIs.CreateMockViaGlobal(const AImpl: TGocciaValue): TGocciaMockFunctionValue;
var
  MockCallable: TGocciaFunctionBase;
  Args: TGocciaArgumentsCollection;
  Result_: TGocciaValue;
begin
  MockCallable := TGocciaFunctionBase(FScope.ResolveIdentifier('mock'));
  if Assigned(AImpl) then
    Args := TGocciaArgumentsCollection.Create([AImpl])
  else
    Args := TGocciaArgumentsCollection.Create;
  try
    Result_ := MockCallable.Call(Args, nil);
  finally
    Args.Free;
  end;
  Result := TGocciaMockFunctionValue(Result_);
end;

function TTestMockAndSpyAPIs.CreateSpyViaGlobal(const ATarget: TGocciaObjectValue; const AMethodName: string): TGocciaMockFunctionValue;
var
  SpyCallable: TGocciaFunctionBase;
  Args: TGocciaArgumentsCollection;
  Result_: TGocciaValue;
begin
  SpyCallable := TGocciaFunctionBase(FScope.ResolveIdentifier('spyOn'));
  Args := TGocciaArgumentsCollection.Create([ATarget, TGocciaStringLiteralValue.Create(AMethodName)]);
  try
    Result_ := SpyCallable.Call(Args, nil);
  finally
    Args.Free;
  end;
  Result := TGocciaMockFunctionValue(Result_);
end;

function TTestMockAndSpyAPIs.CallMockFunction(const AMock: TGocciaMockFunctionValue; const AArgs: array of TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
  I: Integer;
begin
  Args := TGocciaArgumentsCollection.CreateWithCapacity(Length(AArgs));
  try
    for I := 0 to Length(AArgs) - 1 do
      Args.Add(AArgs[I]);
    Result := AMock.Call(Args, TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    Args.Free;
  end;
end;

function TTestMockAndSpyAPIs.CallbackReturn42(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(42);
end;

function TTestMockAndSpyAPIs.CallbackReturn99(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(99);
end;

function TTestMockAndSpyAPIs.CallbackReturnDefault(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create('default');
end;

function TTestMockAndSpyAPIs.CallbackReturnOnce(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create('once');
end;

function TTestMockAndSpyAPIs.CallbackReturnFirst(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create('first');
end;

function TTestMockAndSpyAPIs.CallbackReturnSecond(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create('second');
end;

function TTestMockAndSpyAPIs.CallbackReturnImpl(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create('impl');
end;

function TTestMockAndSpyAPIs.CallbackReturnOriginal(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create('original');
end;

function TTestMockAndSpyAPIs.CallbackReturnMocked(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create('mocked');
end;

function TTestMockAndSpyAPIs.CallbackReturnUndefined(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TTestMockAndSpyAPIs.SetupTests;
begin
  { mock() creation and call tracking }
  Test('mock() returns undefined by default', TestMockReturnsUndefinedByDefault);
  Test('mock() tracks call arguments', TestMockTracksCallArguments);
  Test('mock() tracks multiple calls', TestMockTracksMultipleCalls);
  Test('mock(impl) uses the provided implementation', TestMockWithImplementation);
  Test('mock.calls contains argument arrays', TestMockDotMockCallsArray);
  Test('mock.results contains return entries', TestMockDotMockResultsArray);
  Test('mock.lastCall returns last call args', TestMockDotMockLastCall);
  Test('mock.lastCall is undefined when never called', TestMockDotMockLastCallWhenNeverCalled);

  { mockImplementation }
  Test('mockImplementation sets new implementation', TestMockImplementationSetsImpl);
  Test('mockImplementation returns the mock for chaining', TestMockImplementationReturnsTheMock);

  { mockImplementationOnce }
  Test('mockImplementationOnce uses impl once then falls back', TestMockImplementationOnceThenFallback);
  Test('mockImplementationOnce can be chained', TestMockImplementationOnceChained);

  { mockReturnValue }
  Test('mockReturnValue sets permanent return value', TestMockReturnValuePermanent);
  Test('mockReturnValue overrides existing implementation', TestMockReturnValueOverridesImpl);

  { mockReturnValueOnce }
  Test('mockReturnValueOnce returns once then undefined', TestMockReturnValueOnceThenUndefined);
  Test('mockReturnValueOnce can be chained', TestMockReturnValueOnceChained);
  Test('mockReturnValueOnce takes priority over mockReturnValue', TestMockReturnValueOncePriorityOverPermanent);

  { mockClear / mockReset }
  Test('mockClear clears tracking but keeps implementation', TestMockClearKeepsImplementation);
  Test('mockReset clears tracking and implementation', TestMockResetClearsImplementation);

  { mockName / getMockName }
  Test('getMockName returns default name', TestMockNameDefault);
  Test('mockName sets custom name', TestMockNameCustom);

  { spyOn }
  Test('spyOn passes through to original implementation', TestSpyOnPassesThrough);
  Test('spyOn tracks calls', TestSpyOnTracksCalls);
  Test('spyOn mockImplementation overrides original', TestSpyOnMockImplementationOverrides);
  Test('spyOn mockRestore restores the original method', TestSpyOnMockRestoreRestoresOriginal);
  Test('spyOn mockReturnValue overrides original', TestSpyOnMockReturnValueOverrides);

  { mock matchers - toHaveBeenCalled }
  Test('toHaveBeenCalled passes when mock was called', TestToHaveBeenCalledPass);
  Test('toHaveBeenCalled fails when mock was not called', TestToHaveBeenCalledFail);
  Test('not.toHaveBeenCalled passes when mock was not called', TestToHaveBeenCalledNegated);

  { mock matchers - toHaveBeenCalledTimes }
  Test('toHaveBeenCalledTimes passes for correct count', TestToHaveBeenCalledTimesPass);
  Test('toHaveBeenCalledTimes fails for wrong count', TestToHaveBeenCalledTimesFail);

  { mock matchers - toHaveBeenCalledWith }
  Test('toHaveBeenCalledWith passes when args match a call', TestToHaveBeenCalledWithPass);
  Test('toHaveBeenCalledWith fails when no call matches', TestToHaveBeenCalledWithFail);
  Test('toHaveBeenCalledWith uses deep equality', TestToHaveBeenCalledWithDeepEquality);

  { mock matchers - toHaveBeenLastCalledWith }
  Test('toHaveBeenLastCalledWith passes for last call args', TestToHaveBeenLastCalledWithPass);
  Test('toHaveBeenLastCalledWith fails for non-last args', TestToHaveBeenLastCalledWithFail);

  { mock matchers - toHaveBeenNthCalledWith }
  Test('toHaveBeenNthCalledWith passes for correct nth call', TestToHaveBeenNthCalledWithPass);

  { mock matchers - toHaveReturned }
  Test('toHaveReturned passes when mock returned', TestToHaveReturnedPass);
  Test('toHaveReturned fails when mock was never called', TestToHaveReturnedFail);

  { mock matchers - toHaveReturnedTimes }
  Test('toHaveReturnedTimes passes for correct count', TestToHaveReturnedTimesPass);

  { mock matchers - toHaveReturnedWith }
  Test('toHaveReturnedWith passes when value matches a return', TestToHaveReturnedWithPass);
  Test('toHaveReturnedWith fails when value does not match', TestToHaveReturnedWithFail);

  { mock matchers - toHaveLastReturnedWith }
  Test('toHaveLastReturnedWith passes for last return value', TestToHaveLastReturnedWithPass);

  { mock matchers - toHaveNthReturnedWith }
  Test('toHaveNthReturnedWith passes for correct nth return', TestToHaveNthReturnedWithPass);

  { matchers reject non-mock values }
  Test('mock matchers fail for non-mock values', TestMatcherRejectsNonMock);
end;

{ ---- mock() creation and call tracking ---- }

procedure TTestMockAndSpyAPIs.TestMockReturnsUndefinedByDefault;
var
  M: TGocciaMockFunctionValue;
  R: TGocciaValue;
begin
  M := CreateMockViaGlobal;
  R := CallMockFunction(M, []);
  Expect<Boolean>(R is TGocciaUndefinedLiteralValue).ToBe(True);
end;

procedure TTestMockAndSpyAPIs.TestMockTracksCallArguments;
var
  M: TGocciaMockFunctionValue;
begin
  M := CreateMockViaGlobal;
  CallMockFunction(M, [TGocciaNumberLiteralValue.Create(1), TGocciaNumberLiteralValue.Create(2)]);
  Expect<Integer>(M.MockCalls.Count).ToBe(1);
  Expect<Boolean>(M.MockCalls[0] is TGocciaArrayValue).ToBe(True);
  Expect<Integer>(TGocciaArrayValue(M.MockCalls[0]).Elements.Count).ToBe(2);
end;

procedure TTestMockAndSpyAPIs.TestMockTracksMultipleCalls;
var
  M: TGocciaMockFunctionValue;
begin
  M := CreateMockViaGlobal;
  CallMockFunction(M, []);
  CallMockFunction(M, [TGocciaStringLiteralValue.Create('a')]);
  CallMockFunction(M, [TGocciaNumberLiteralValue.Create(1), TGocciaNumberLiteralValue.Create(2)]);
  Expect<Integer>(M.MockCalls.Count).ToBe(3);
  Expect<Integer>(TGocciaArrayValue(M.MockCalls[0]).Elements.Count).ToBe(0);
  Expect<Integer>(TGocciaArrayValue(M.MockCalls[1]).Elements.Count).ToBe(1);
  Expect<Integer>(TGocciaArrayValue(M.MockCalls[2]).Elements.Count).ToBe(2);
end;

procedure TTestMockAndSpyAPIs.TestMockWithImplementation;
var
  M: TGocciaMockFunctionValue;
  R: TGocciaValue;
begin
  // Use a simple impl that returns a known value
  M := TGocciaMockFunctionValue.Create(
    TGocciaNativeFunctionValue.Create(CallbackReturn42, 'impl', 0));
  R := CallMockFunction(M, []);
  Expect<Double>(R.ToNumberLiteral.Value).ToBe(42);
end;

procedure TTestMockAndSpyAPIs.TestMockDotMockCallsArray;
var
  M: TGocciaMockFunctionValue;
  MockObj: TGocciaValue;
  CallsArr: TGocciaValue;
begin
  M := CreateMockViaGlobal;
  CallMockFunction(M, [TGocciaStringLiteralValue.Create('hello')]);
  MockObj := M.GetProperty('mock');
  Expect<Boolean>(MockObj is TGocciaObjectValue).ToBe(True);
  CallsArr := TGocciaObjectValue(MockObj).GetProperty('calls');
  Expect<Boolean>(CallsArr is TGocciaArrayValue).ToBe(True);
  Expect<Integer>(TGocciaArrayValue(CallsArr).Elements.Count).ToBe(1);
end;

procedure TTestMockAndSpyAPIs.TestMockDotMockResultsArray;
var
  M: TGocciaMockFunctionValue;
  MockObj: TGocciaValue;
  ResultsArr: TGocciaValue;
  FirstResult: TGocciaValue;
begin
  M := CreateMockViaGlobal;
  CallMockFunction(M, []);
  MockObj := M.GetProperty('mock');
  ResultsArr := TGocciaObjectValue(MockObj).GetProperty('results');
  Expect<Boolean>(ResultsArr is TGocciaArrayValue).ToBe(True);
  Expect<Integer>(TGocciaArrayValue(ResultsArr).Elements.Count).ToBe(1);
  FirstResult := TGocciaArrayValue(ResultsArr).Elements[0];
  Expect<string>(TGocciaObjectValue(FirstResult).GetProperty('type').ToStringLiteral.Value).ToBe('return');
end;

procedure TTestMockAndSpyAPIs.TestMockDotMockLastCall;
var
  M: TGocciaMockFunctionValue;
  MockObj: TGocciaValue;
  LastCall: TGocciaValue;
begin
  M := CreateMockViaGlobal;
  CallMockFunction(M, [TGocciaNumberLiteralValue.Create(1)]);
  CallMockFunction(M, [TGocciaNumberLiteralValue.Create(2), TGocciaNumberLiteralValue.Create(3)]);
  MockObj := M.GetProperty('mock');
  LastCall := TGocciaObjectValue(MockObj).GetProperty('lastCall');
  Expect<Boolean>(LastCall is TGocciaArrayValue).ToBe(True);
  Expect<Integer>(TGocciaArrayValue(LastCall).Elements.Count).ToBe(2);
end;

procedure TTestMockAndSpyAPIs.TestMockDotMockLastCallWhenNeverCalled;
var
  M: TGocciaMockFunctionValue;
  MockObj: TGocciaValue;
  LastCall: TGocciaValue;
begin
  M := CreateMockViaGlobal;
  MockObj := M.GetProperty('mock');
  LastCall := TGocciaObjectValue(MockObj).GetProperty('lastCall');
  Expect<Boolean>(LastCall is TGocciaUndefinedLiteralValue).ToBe(True);
end;

{ ---- mockImplementation ---- }

procedure TTestMockAndSpyAPIs.TestMockImplementationSetsImpl;
var
  M: TGocciaMockFunctionValue;
  Args: TGocciaArgumentsCollection;
  ImplFn: TGocciaNativeFunctionValue;
  R: TGocciaValue;
begin
  M := CreateMockViaGlobal;
  ImplFn := TGocciaNativeFunctionValue.Create(CallbackReturn99, 'impl', 0);
  Args := TGocciaArgumentsCollection.Create([ImplFn]);
  try
    M.DoMockImplementation(Args, nil);
  finally
    Args.Free;
  end;
  R := CallMockFunction(M, []);
  Expect<Double>(R.ToNumberLiteral.Value).ToBe(99);
end;

procedure TTestMockAndSpyAPIs.TestMockImplementationReturnsTheMock;
var
  M: TGocciaMockFunctionValue;
  Args: TGocciaArgumentsCollection;
  R: TGocciaValue;
  ImplFn: TGocciaNativeFunctionValue;
begin
  M := CreateMockViaGlobal;
  ImplFn := TGocciaNativeFunctionValue.Create(CallbackReturnUndefined, 'impl', 0);
  Args := TGocciaArgumentsCollection.Create([ImplFn]);
  try
    R := M.DoMockImplementation(Args, nil);
  finally
    Args.Free;
  end;
  Expect<Boolean>(R = M).ToBe(True);
end;

{ ---- mockImplementationOnce ---- }

procedure TTestMockAndSpyAPIs.TestMockImplementationOnceThenFallback;
var
  M: TGocciaMockFunctionValue;
  Args: TGocciaArgumentsCollection;
  OnceFn, DefaultFn: TGocciaNativeFunctionValue;
  R: TGocciaValue;
begin
  DefaultFn := TGocciaNativeFunctionValue.Create(CallbackReturnDefault, 'default', 0);
  M := TGocciaMockFunctionValue.Create(DefaultFn);

  OnceFn := TGocciaNativeFunctionValue.Create(CallbackReturnOnce, 'once', 0);
  Args := TGocciaArgumentsCollection.Create([OnceFn]);
  try
    M.DoMockImplementationOnce(Args, nil);
  finally
    Args.Free;
  end;

  R := CallMockFunction(M, []);
  Expect<string>(R.ToStringLiteral.Value).ToBe('once');
  R := CallMockFunction(M, []);
  Expect<string>(R.ToStringLiteral.Value).ToBe('default');
end;

procedure TTestMockAndSpyAPIs.TestMockImplementationOnceChained;
var
  M: TGocciaMockFunctionValue;
  Args1, Args2: TGocciaArgumentsCollection;
  Fn1, Fn2: TGocciaNativeFunctionValue;
  R: TGocciaValue;
begin
  M := CreateMockViaGlobal;

  Fn1 := TGocciaNativeFunctionValue.Create(CallbackReturnFirst, 'fn1', 0);
  Fn2 := TGocciaNativeFunctionValue.Create(CallbackReturnSecond, 'fn2', 0);

  Args1 := TGocciaArgumentsCollection.Create([Fn1]);
  Args2 := TGocciaArgumentsCollection.Create([Fn2]);
  try
    M.DoMockImplementationOnce(Args1, nil);
    M.DoMockImplementationOnce(Args2, nil);
  finally
    Args1.Free;
    Args2.Free;
  end;

  R := CallMockFunction(M, []);
  Expect<string>(R.ToStringLiteral.Value).ToBe('first');
  R := CallMockFunction(M, []);
  Expect<string>(R.ToStringLiteral.Value).ToBe('second');
  R := CallMockFunction(M, []);
  Expect<Boolean>(R is TGocciaUndefinedLiteralValue).ToBe(True);
end;

{ ---- mockReturnValue ---- }

procedure TTestMockAndSpyAPIs.TestMockReturnValuePermanent;
var
  M: TGocciaMockFunctionValue;
  Args: TGocciaArgumentsCollection;
  R: TGocciaValue;
begin
  M := CreateMockViaGlobal;
  Args := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(42)]);
  try
    M.DoMockReturnValue(Args, nil);
  finally
    Args.Free;
  end;
  R := CallMockFunction(M, []);
  Expect<Double>(R.ToNumberLiteral.Value).ToBe(42);
  R := CallMockFunction(M, []);
  Expect<Double>(R.ToNumberLiteral.Value).ToBe(42);
end;

procedure TTestMockAndSpyAPIs.TestMockReturnValueOverridesImpl;
var
  M: TGocciaMockFunctionValue;
  ImplFn: TGocciaNativeFunctionValue;
  RetArgs: TGocciaArgumentsCollection;
  R: TGocciaValue;
begin
  ImplFn := TGocciaNativeFunctionValue.Create(CallbackReturnImpl, 'impl', 0);
  M := TGocciaMockFunctionValue.Create(ImplFn);

  R := CallMockFunction(M, []);
  Expect<string>(R.ToStringLiteral.Value).ToBe('impl');

  RetArgs := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('ret')]);
  try
    M.DoMockReturnValue(RetArgs, nil);
  finally
    RetArgs.Free;
  end;

  R := CallMockFunction(M, []);
  Expect<string>(R.ToStringLiteral.Value).ToBe('ret');
end;

{ ---- mockReturnValueOnce ---- }

procedure TTestMockAndSpyAPIs.TestMockReturnValueOnceThenUndefined;
var
  M: TGocciaMockFunctionValue;
  Args: TGocciaArgumentsCollection;
  R: TGocciaValue;
begin
  M := CreateMockViaGlobal;
  Args := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(1)]);
  try
    M.DoMockReturnValueOnce(Args, nil);
  finally
    Args.Free;
  end;
  R := CallMockFunction(M, []);
  Expect<Double>(R.ToNumberLiteral.Value).ToBe(1);
  R := CallMockFunction(M, []);
  Expect<Boolean>(R is TGocciaUndefinedLiteralValue).ToBe(True);
end;

procedure TTestMockAndSpyAPIs.TestMockReturnValueOnceChained;
var
  M: TGocciaMockFunctionValue;
  Args1, Args2: TGocciaArgumentsCollection;
  R: TGocciaValue;
begin
  M := CreateMockViaGlobal;
  Args1 := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('a')]);
  Args2 := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('b')]);
  try
    M.DoMockReturnValueOnce(Args1, nil);
    M.DoMockReturnValueOnce(Args2, nil);
  finally
    Args1.Free;
    Args2.Free;
  end;
  R := CallMockFunction(M, []);
  Expect<string>(R.ToStringLiteral.Value).ToBe('a');
  R := CallMockFunction(M, []);
  Expect<string>(R.ToStringLiteral.Value).ToBe('b');
  R := CallMockFunction(M, []);
  Expect<Boolean>(R is TGocciaUndefinedLiteralValue).ToBe(True);
end;

procedure TTestMockAndSpyAPIs.TestMockReturnValueOncePriorityOverPermanent;
var
  M: TGocciaMockFunctionValue;
  RetArgs, OnceArgs: TGocciaArgumentsCollection;
  R: TGocciaValue;
begin
  M := CreateMockViaGlobal;
  RetArgs := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('perm')]);
  OnceArgs := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('once')]);
  try
    M.DoMockReturnValue(RetArgs, nil);
    M.DoMockReturnValueOnce(OnceArgs, nil);
  finally
    RetArgs.Free;
    OnceArgs.Free;
  end;
  R := CallMockFunction(M, []);
  Expect<string>(R.ToStringLiteral.Value).ToBe('once');
  // After once is consumed, the permanent return value is gone (mockReturnValue clears impl)
  // and default return value was set
  R := CallMockFunction(M, []);
  Expect<string>(R.ToStringLiteral.Value).ToBe('perm');
end;

{ ---- mockClear / mockReset ---- }

procedure TTestMockAndSpyAPIs.TestMockClearKeepsImplementation;
var
  M: TGocciaMockFunctionValue;
  EmptyArgs: TGocciaArgumentsCollection;
  R: TGocciaValue;
  ImplFn: TGocciaNativeFunctionValue;
begin
  ImplFn := TGocciaNativeFunctionValue.Create(CallbackReturn42, 'impl', 0);
  M := TGocciaMockFunctionValue.Create(ImplFn);

  CallMockFunction(M, []);
  Expect<Integer>(M.MockCalls.Count).ToBe(1);

  EmptyArgs := TGocciaArgumentsCollection.Create;
  try
    M.DoMockClear(EmptyArgs, nil);
  finally
    EmptyArgs.Free;
  end;

  Expect<Integer>(M.MockCalls.Count).ToBe(0);
  Expect<Integer>(M.MockResults.Count).ToBe(0);

  // Implementation should still work
  R := CallMockFunction(M, []);
  Expect<Double>(R.ToNumberLiteral.Value).ToBe(42);
end;

procedure TTestMockAndSpyAPIs.TestMockResetClearsImplementation;
var
  M: TGocciaMockFunctionValue;
  EmptyArgs: TGocciaArgumentsCollection;
  R: TGocciaValue;
  ImplFn: TGocciaNativeFunctionValue;
begin
  ImplFn := TGocciaNativeFunctionValue.Create(CallbackReturn42, 'impl', 0);
  M := TGocciaMockFunctionValue.Create(ImplFn);

  CallMockFunction(M, []);

  EmptyArgs := TGocciaArgumentsCollection.Create;
  try
    M.DoMockReset(EmptyArgs, nil);
  finally
    EmptyArgs.Free;
  end;

  Expect<Integer>(M.MockCalls.Count).ToBe(0);
  R := CallMockFunction(M, []);
  Expect<Boolean>(R is TGocciaUndefinedLiteralValue).ToBe(True);
end;

{ ---- mockName / getMockName ---- }

procedure TTestMockAndSpyAPIs.TestMockNameDefault;
var
  M: TGocciaMockFunctionValue;
  Args: TGocciaArgumentsCollection;
  R: TGocciaValue;
begin
  M := CreateMockViaGlobal;
  Args := TGocciaArgumentsCollection.Create;
  try
    R := M.DoGetMockName(Args, nil);
  finally
    Args.Free;
  end;
  Expect<string>(R.ToStringLiteral.Value).ToBe('mock');
end;

procedure TTestMockAndSpyAPIs.TestMockNameCustom;
var
  M: TGocciaMockFunctionValue;
  NameArgs, EmptyArgs: TGocciaArgumentsCollection;
  R: TGocciaValue;
begin
  M := CreateMockViaGlobal;
  NameArgs := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('myMock')]);
  EmptyArgs := TGocciaArgumentsCollection.Create;
  try
    M.DoMockName(NameArgs, nil);
    R := M.DoGetMockName(EmptyArgs, nil);
  finally
    NameArgs.Free;
    EmptyArgs.Free;
  end;
  Expect<string>(R.ToStringLiteral.Value).ToBe('myMock');
end;

{ ---- spyOn ---- }

procedure TTestMockAndSpyAPIs.TestSpyOnPassesThrough;
var
  Obj: TGocciaObjectValue;
  OrigFn: TGocciaNativeFunctionValue;
  Spy: TGocciaMockFunctionValue;
  R: TGocciaValue;
begin
  OrigFn := TGocciaNativeFunctionValue.Create(CallbackReturnOriginal, 'orig', 0);
  Obj := TGocciaObjectValue.Create;
  Obj.AssignProperty('method', OrigFn);
  Spy := CreateSpyViaGlobal(Obj, 'method');
  R := CallMockFunction(Spy, []);
  Expect<string>(R.ToStringLiteral.Value).ToBe('original');
end;

procedure TTestMockAndSpyAPIs.TestSpyOnTracksCalls;
var
  Obj: TGocciaObjectValue;
  OrigFn: TGocciaNativeFunctionValue;
  Spy: TGocciaMockFunctionValue;
begin
  OrigFn := TGocciaNativeFunctionValue.Create(CallbackReturnUndefined, 'orig', 0);
  Obj := TGocciaObjectValue.Create;
  Obj.AssignProperty('method', OrigFn);
  Spy := CreateSpyViaGlobal(Obj, 'method');
  CallMockFunction(Spy, [TGocciaNumberLiteralValue.Create(1)]);
  CallMockFunction(Spy, [TGocciaNumberLiteralValue.Create(2)]);
  Expect<Integer>(Spy.MockCalls.Count).ToBe(2);
end;

procedure TTestMockAndSpyAPIs.TestSpyOnMockImplementationOverrides;
var
  Obj: TGocciaObjectValue;
  OrigFn, NewFn: TGocciaNativeFunctionValue;
  Spy: TGocciaMockFunctionValue;
  Args: TGocciaArgumentsCollection;
  R: TGocciaValue;
begin
  OrigFn := TGocciaNativeFunctionValue.Create(CallbackReturnOriginal, 'orig', 0);
  Obj := TGocciaObjectValue.Create;
  Obj.AssignProperty('method', OrigFn);
  Spy := CreateSpyViaGlobal(Obj, 'method');

  NewFn := TGocciaNativeFunctionValue.Create(CallbackReturnMocked, 'new', 0);
  Args := TGocciaArgumentsCollection.Create([NewFn]);
  try
    Spy.DoMockImplementation(Args, nil);
  finally
    Args.Free;
  end;

  R := CallMockFunction(Spy, []);
  Expect<string>(R.ToStringLiteral.Value).ToBe('mocked');
end;

procedure TTestMockAndSpyAPIs.TestSpyOnMockRestoreRestoresOriginal;
var
  Obj: TGocciaObjectValue;
  OrigFn: TGocciaNativeFunctionValue;
  Spy: TGocciaMockFunctionValue;
  EmptyArgs: TGocciaArgumentsCollection;
  PropValue: TGocciaValue;
begin
  OrigFn := TGocciaNativeFunctionValue.Create(CallbackReturnOriginal, 'orig', 0);
  Obj := TGocciaObjectValue.Create;
  Obj.AssignProperty('method', OrigFn);
  Spy := CreateSpyViaGlobal(Obj, 'method');

  // Verify spy is installed
  PropValue := Obj.GetProperty('method');
  Expect<Boolean>(PropValue = Spy).ToBe(True);

  // Restore
  EmptyArgs := TGocciaArgumentsCollection.Create;
  try
    Spy.DoMockRestore(EmptyArgs, nil);
  finally
    EmptyArgs.Free;
  end;

  // Verify original is restored
  PropValue := Obj.GetProperty('method');
  Expect<Boolean>(PropValue = OrigFn).ToBe(True);
end;

procedure TTestMockAndSpyAPIs.TestSpyOnMockReturnValueOverrides;
var
  Obj: TGocciaObjectValue;
  OrigFn: TGocciaNativeFunctionValue;
  Spy: TGocciaMockFunctionValue;
  RetArgs: TGocciaArgumentsCollection;
  R: TGocciaValue;
begin
  OrigFn := TGocciaNativeFunctionValue.Create(CallbackReturnOriginal, 'orig', 0);
  Obj := TGocciaObjectValue.Create;
  Obj.AssignProperty('method', OrigFn);
  Spy := CreateSpyViaGlobal(Obj, 'method');

  RetArgs := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('mocked')]);
  try
    Spy.DoMockReturnValue(RetArgs, nil);
  finally
    RetArgs.Free;
  end;

  R := CallMockFunction(Spy, []);
  Expect<string>(R.ToStringLiteral.Value).ToBe('mocked');
end;

{ ---- mock matchers ---- }

procedure TTestMockAndSpyAPIs.TestToHaveBeenCalledPass;
var
  M: TGocciaMockFunctionValue;
  A: TGocciaArgumentsCollection;
begin
  M := CreateMockViaGlobal;
  CallMockFunction(M, []);
  A := TGocciaArgumentsCollection.Create;
  try
    ExpectPass(MakeExpectation(M).ToHaveBeenCalled(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestMockAndSpyAPIs.TestToHaveBeenCalledFail;
var
  M: TGocciaMockFunctionValue;
  A: TGocciaArgumentsCollection;
begin
  M := CreateMockViaGlobal;
  A := TGocciaArgumentsCollection.Create;
  try
    ExpectFail(MakeExpectation(M).ToHaveBeenCalled(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestMockAndSpyAPIs.TestToHaveBeenCalledNegated;
var
  M: TGocciaMockFunctionValue;
  A: TGocciaArgumentsCollection;
begin
  M := CreateMockViaGlobal;
  A := TGocciaArgumentsCollection.Create;
  try
    ExpectPass(MakeExpectation(M, True).ToHaveBeenCalled(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestMockAndSpyAPIs.TestToHaveBeenCalledTimesPass;
var
  M: TGocciaMockFunctionValue;
  A: TGocciaArgumentsCollection;
begin
  M := CreateMockViaGlobal;
  CallMockFunction(M, []);
  CallMockFunction(M, []);
  CallMockFunction(M, []);
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(3)]);
  try
    ExpectPass(MakeExpectation(M).ToHaveBeenCalledTimes(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestMockAndSpyAPIs.TestToHaveBeenCalledTimesFail;
var
  M: TGocciaMockFunctionValue;
  A: TGocciaArgumentsCollection;
begin
  M := CreateMockViaGlobal;
  CallMockFunction(M, []);
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(2)]);
  try
    ExpectFail(MakeExpectation(M).ToHaveBeenCalledTimes(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestMockAndSpyAPIs.TestToHaveBeenCalledWithPass;
var
  M: TGocciaMockFunctionValue;
  A: TGocciaArgumentsCollection;
begin
  M := CreateMockViaGlobal;
  CallMockFunction(M, [TGocciaNumberLiteralValue.Create(1), TGocciaNumberLiteralValue.Create(2)]);
  CallMockFunction(M, [TGocciaNumberLiteralValue.Create(3), TGocciaNumberLiteralValue.Create(4)]);
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(3), TGocciaNumberLiteralValue.Create(4)]);
  try
    ExpectPass(MakeExpectation(M).ToHaveBeenCalledWith(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestMockAndSpyAPIs.TestToHaveBeenCalledWithFail;
var
  M: TGocciaMockFunctionValue;
  A: TGocciaArgumentsCollection;
begin
  M := CreateMockViaGlobal;
  CallMockFunction(M, [TGocciaNumberLiteralValue.Create(1)]);
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(99)]);
  try
    ExpectFail(MakeExpectation(M).ToHaveBeenCalledWith(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestMockAndSpyAPIs.TestToHaveBeenCalledWithDeepEquality;
var
  M: TGocciaMockFunctionValue;
  Arr: TGocciaArrayValue;
  ExpectedArr: TGocciaArrayValue;
  A: TGocciaArgumentsCollection;
begin
  M := CreateMockViaGlobal;
  Arr := TGocciaArrayValue.Create;
  Arr.Elements.Add(TGocciaNumberLiteralValue.Create(1));
  Arr.Elements.Add(TGocciaNumberLiteralValue.Create(2));
  CallMockFunction(M, [Arr]);

  ExpectedArr := TGocciaArrayValue.Create;
  ExpectedArr.Elements.Add(TGocciaNumberLiteralValue.Create(1));
  ExpectedArr.Elements.Add(TGocciaNumberLiteralValue.Create(2));
  A := TGocciaArgumentsCollection.Create([ExpectedArr]);
  try
    ExpectPass(MakeExpectation(M).ToHaveBeenCalledWith(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestMockAndSpyAPIs.TestToHaveBeenLastCalledWithPass;
var
  M: TGocciaMockFunctionValue;
  A: TGocciaArgumentsCollection;
begin
  M := CreateMockViaGlobal;
  CallMockFunction(M, [TGocciaStringLiteralValue.Create('first')]);
  CallMockFunction(M, [TGocciaStringLiteralValue.Create('last')]);
  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('last')]);
  try
    ExpectPass(MakeExpectation(M).ToHaveBeenLastCalledWith(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestMockAndSpyAPIs.TestToHaveBeenLastCalledWithFail;
var
  M: TGocciaMockFunctionValue;
  A: TGocciaArgumentsCollection;
begin
  M := CreateMockViaGlobal;
  CallMockFunction(M, [TGocciaStringLiteralValue.Create('first')]);
  CallMockFunction(M, [TGocciaStringLiteralValue.Create('last')]);
  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('first')]);
  try
    ExpectFail(MakeExpectation(M).ToHaveBeenLastCalledWith(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestMockAndSpyAPIs.TestToHaveBeenNthCalledWithPass;
var
  M: TGocciaMockFunctionValue;
  A: TGocciaArgumentsCollection;
begin
  M := CreateMockViaGlobal;
  CallMockFunction(M, [TGocciaStringLiteralValue.Create('a')]);
  CallMockFunction(M, [TGocciaStringLiteralValue.Create('b')]);
  CallMockFunction(M, [TGocciaStringLiteralValue.Create('c')]);
  // 1-based index: nth=2 should be 'b'
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(2), TGocciaStringLiteralValue.Create('b')]);
  try
    ExpectPass(MakeExpectation(M).ToHaveBeenNthCalledWith(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestMockAndSpyAPIs.TestToHaveReturnedPass;
var
  M: TGocciaMockFunctionValue;
  A: TGocciaArgumentsCollection;
begin
  M := CreateMockViaGlobal;
  CallMockFunction(M, []);
  A := TGocciaArgumentsCollection.Create;
  try
    ExpectPass(MakeExpectation(M).ToHaveReturned(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestMockAndSpyAPIs.TestToHaveReturnedFail;
var
  M: TGocciaMockFunctionValue;
  A: TGocciaArgumentsCollection;
begin
  M := CreateMockViaGlobal;
  // Never called — no returns
  A := TGocciaArgumentsCollection.Create;
  try
    ExpectFail(MakeExpectation(M).ToHaveReturned(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestMockAndSpyAPIs.TestToHaveReturnedTimesPass;
var
  M: TGocciaMockFunctionValue;
  A: TGocciaArgumentsCollection;
begin
  M := CreateMockViaGlobal;
  CallMockFunction(M, []);
  CallMockFunction(M, []);
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(2)]);
  try
    ExpectPass(MakeExpectation(M).ToHaveReturnedTimes(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestMockAndSpyAPIs.TestToHaveReturnedWithPass;
var
  M: TGocciaMockFunctionValue;
  RetArgs, A: TGocciaArgumentsCollection;
begin
  M := CreateMockViaGlobal;
  RetArgs := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(42)]);
  try
    M.DoMockReturnValue(RetArgs, nil);
  finally
    RetArgs.Free;
  end;
  CallMockFunction(M, []);
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(42)]);
  try
    ExpectPass(MakeExpectation(M).ToHaveReturnedWith(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestMockAndSpyAPIs.TestToHaveReturnedWithFail;
var
  M: TGocciaMockFunctionValue;
  A: TGocciaArgumentsCollection;
begin
  M := CreateMockViaGlobal;
  CallMockFunction(M, []);
  // Default return is undefined, look for 42
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(42)]);
  try
    ExpectFail(MakeExpectation(M).ToHaveReturnedWith(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestMockAndSpyAPIs.TestToHaveLastReturnedWithPass;
var
  M: TGocciaMockFunctionValue;
  OnceArgs1, OnceArgs2, A: TGocciaArgumentsCollection;
begin
  M := CreateMockViaGlobal;
  OnceArgs1 := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(1)]);
  OnceArgs2 := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(2)]);
  try
    M.DoMockReturnValueOnce(OnceArgs1, nil);
    M.DoMockReturnValueOnce(OnceArgs2, nil);
  finally
    OnceArgs1.Free;
    OnceArgs2.Free;
  end;
  CallMockFunction(M, []);
  CallMockFunction(M, []);
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(2)]);
  try
    ExpectPass(MakeExpectation(M).ToHaveLastReturnedWith(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestMockAndSpyAPIs.TestToHaveNthReturnedWithPass;
var
  M: TGocciaMockFunctionValue;
  OnceArgs1, OnceArgs2, OnceArgs3, A: TGocciaArgumentsCollection;
begin
  M := CreateMockViaGlobal;
  OnceArgs1 := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('a')]);
  OnceArgs2 := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('b')]);
  OnceArgs3 := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('c')]);
  try
    M.DoMockReturnValueOnce(OnceArgs1, nil);
    M.DoMockReturnValueOnce(OnceArgs2, nil);
    M.DoMockReturnValueOnce(OnceArgs3, nil);
  finally
    OnceArgs1.Free;
    OnceArgs2.Free;
    OnceArgs3.Free;
  end;
  CallMockFunction(M, []);
  CallMockFunction(M, []);
  CallMockFunction(M, []);
  // 1-based: nth=2 should be 'b'
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(2), TGocciaStringLiteralValue.Create('b')]);
  try
    ExpectPass(MakeExpectation(M).ToHaveNthReturnedWith(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestMockAndSpyAPIs.TestMatcherRejectsNonMock;
var
  A: TGocciaArgumentsCollection;
begin
  A := TGocciaArgumentsCollection.Create;
  try
    ExpectFail(MakeExpectation(TGocciaNumberLiteralValue.Create(42)).ToHaveBeenCalled(A, nil));
  finally
    A.Free;
  end;
end;

{ ============================================================================ }
{ TTestOnTestFinished                                                        }
{ ============================================================================ }

procedure TTestOnTestFinished.DummyThrowError(const AMessage: string; const ALine, AColumn: Integer);
begin
  raise Exception.Create(AMessage);
end;

procedure TTestOnTestFinished.BeforeAll;
begin
  FRecordedEvents := TStringList.Create;
end;

procedure TTestOnTestFinished.AfterAll;
begin
  FRecordedEvents.Free;
end;

procedure TTestOnTestFinished.BeforeEach;
begin
  FRecordedEvents.Clear;
  FScope := TGocciaGlobalScope.Create;
  FAssertions := TGocciaTestAssertions.Create('test', FScope, DummyThrowError);
  FAssertions.SuppressOutput := True;
end;

procedure TTestOnTestFinished.AfterEach;
begin
  FAssertions.Free;
  FScope.Free;
end;

function TTestOnTestFinished.ResolveGlobalCallable(const AName: string): TGocciaFunctionBase;
var
  Value: TGocciaValue;
begin
  Value := FScope.ResolveIdentifier(AName);
  if not Assigned(Value) then
    raise Exception.CreateFmt('Expected global "%s" to be defined', [AName]);
  if not (Value is TGocciaFunctionBase) then
    raise Exception.CreateFmt('Expected global "%s" to be callable', [AName]);
  Result := TGocciaFunctionBase(Value);
end;

function TTestOnTestFinished.CreateRunOptions: TGocciaArgumentsCollection;
var
  Params: TGocciaObjectValue;
begin
  Params := TGocciaObjectValue.Create;
  Params.AssignProperty('showTestResults', TGocciaBooleanLiteralValue.Create(False));
  Result := TGocciaArgumentsCollection.Create([Params]);
end;

function TTestOnTestFinished.RunRegisteredTests: TGocciaObjectValue;
var
  RunArgs: TGocciaArgumentsCollection;
begin
  RunArgs := CreateRunOptions;
  try
    Result := FAssertions.RunTests(RunArgs, nil) as TGocciaObjectValue;
  finally
    RunArgs.Free;
  end;
end;

function TTestOnTestFinished.RecordTestBodyCallback(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  FRecordedEvents.Add('test-body');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTestOnTestFinished.RecordFinishedCallback(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  FRecordedEvents.Add('finished');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTestOnTestFinished.RecordFinishedACallback(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  FRecordedEvents.Add('finished-A');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTestOnTestFinished.RecordFinishedBCallback(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  FRecordedEvents.Add('finished-B');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTestOnTestFinished.RecordAfterEachCallback(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  FRecordedEvents.Add('afterEach');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTestOnTestFinished.TestBodyWithOnTestFinished(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  OnTestFinishedFunc: TGocciaFunctionBase;
  CallbackArgs: TGocciaArgumentsCollection;
begin
  OnTestFinishedFunc := ResolveGlobalCallable('onTestFinished');
  CallbackArgs := TGocciaArgumentsCollection.Create([
    TGocciaNativeFunctionValue.Create(RecordFinishedCallback, 'finishedCallback', 0)
  ]);
  try
    OnTestFinishedFunc.Call(CallbackArgs, nil);
  finally
    CallbackArgs.Free;
  end;
  FRecordedEvents.Add('test-body');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTestOnTestFinished.TestBodyWithMultipleCallbacks(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  OnTestFinishedFunc: TGocciaFunctionBase;
  CallbackArgs: TGocciaArgumentsCollection;
begin
  OnTestFinishedFunc := ResolveGlobalCallable('onTestFinished');

  CallbackArgs := TGocciaArgumentsCollection.Create([
    TGocciaNativeFunctionValue.Create(RecordFinishedACallback, 'finishedA', 0)
  ]);
  try
    OnTestFinishedFunc.Call(CallbackArgs, nil);
  finally
    CallbackArgs.Free;
  end;

  CallbackArgs := TGocciaArgumentsCollection.Create([
    TGocciaNativeFunctionValue.Create(RecordFinishedBCallback, 'finishedB', 0)
  ]);
  try
    OnTestFinishedFunc.Call(CallbackArgs, nil);
  finally
    CallbackArgs.Free;
  end;

  FRecordedEvents.Add('test-body');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTestOnTestFinished.TestBodyWithAfterEachCallback(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  OnTestFinishedFunc: TGocciaFunctionBase;
  CallbackArgs: TGocciaArgumentsCollection;
begin
  OnTestFinishedFunc := ResolveGlobalCallable('onTestFinished');
  CallbackArgs := TGocciaArgumentsCollection.Create([
    TGocciaNativeFunctionValue.Create(RecordFinishedCallback, 'finishedCallback', 0)
  ]);
  try
    OnTestFinishedFunc.Call(CallbackArgs, nil);
  finally
    CallbackArgs.Free;
  end;
  FRecordedEvents.Add('test-body');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTestOnTestFinished.TestBodyNoCallbacks(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  FRecordedEvents.Add('test-body-no-cb');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTestOnTestFinished.RegisterSuiteWithOnTestFinished(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TestFunc: TGocciaFunctionBase;
  TestArgs: TGocciaArgumentsCollection;
begin
  TestFunc := ResolveGlobalCallable('test');
  TestArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('test with callback'),
    TGocciaNativeFunctionValue.Create(TestBodyWithOnTestFinished, 'testBody', 0)
  ]);
  try
    TestFunc.Call(TestArgs, nil);
  finally
    TestArgs.Free;
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTestOnTestFinished.RegisterSuiteWithMultipleCallbacks(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TestFunc: TGocciaFunctionBase;
  TestArgs: TGocciaArgumentsCollection;
begin
  TestFunc := ResolveGlobalCallable('test');
  TestArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('test with multiple callbacks'),
    TGocciaNativeFunctionValue.Create(TestBodyWithMultipleCallbacks, 'testBody', 0)
  ]);
  try
    TestFunc.Call(TestArgs, nil);
  finally
    TestArgs.Free;
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTestOnTestFinished.RegisterSuiteWithAfterEachAndCallback(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TestFunc, AfterEachFunc: TGocciaFunctionBase;
  TestArgs, HookArgs: TGocciaArgumentsCollection;
begin
  AfterEachFunc := ResolveGlobalCallable('afterEach');
  HookArgs := TGocciaArgumentsCollection.Create([
    TGocciaNativeFunctionValue.Create(RecordAfterEachCallback, 'afterEachCallback', 0)
  ]);
  try
    AfterEachFunc.Call(HookArgs, nil);
  finally
    HookArgs.Free;
  end;

  TestFunc := ResolveGlobalCallable('test');
  TestArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('test with afterEach and callback'),
    TGocciaNativeFunctionValue.Create(TestBodyWithAfterEachCallback, 'testBody', 0)
  ]);
  try
    TestFunc.Call(TestArgs, nil);
  finally
    TestArgs.Free;
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTestOnTestFinished.RegisterSuiteWithScopedCallbacks(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TestFunc: TGocciaFunctionBase;
  TestArgs: TGocciaArgumentsCollection;
begin
  TestFunc := ResolveGlobalCallable('test');

  // First test registers a callback
  TestArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('test with callback'),
    TGocciaNativeFunctionValue.Create(TestBodyWithOnTestFinished, 'testBody', 0)
  ]);
  try
    TestFunc.Call(TestArgs, nil);
  finally
    TestArgs.Free;
  end;

  // Second test does NOT register any callback
  TestArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('test without callback'),
    TGocciaNativeFunctionValue.Create(TestBodyNoCallbacks, 'testBodyNoCb', 0)
  ]);
  try
    TestFunc.Call(TestArgs, nil);
  finally
    TestArgs.Free;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TTestOnTestFinished.SetupTests;
begin
  Test('onTestFinished callback runs after test', TestOnTestFinishedCallbackRuns);
  Test('multiple onTestFinished callbacks run in registration order', TestOnTestFinishedMultipleInOrder);
  Test('onTestFinished runs after afterEach hooks', TestOnTestFinishedRunsAfterAfterEach);
  Test('onTestFinished callbacks are scoped to the current test', TestOnTestFinishedScopedToCurrentTest);
end;

procedure TTestOnTestFinished.TestOnTestFinishedCallbackRuns;
var
  DescribeFunc: TGocciaFunctionBase;
  SuiteArgs: TGocciaArgumentsCollection;
begin
  DescribeFunc := ResolveGlobalCallable('describe');
  SuiteArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('suite'),
    TGocciaNativeFunctionValue.Create(RegisterSuiteWithOnTestFinished, 'registerSuite', 0)
  ]);
  try
    DescribeFunc.Call(SuiteArgs, nil);
  finally
    SuiteArgs.Free;
  end;

  RunRegisteredTests;
  // Order: test-body, finished
  Expect<Integer>(FRecordedEvents.Count).ToBe(2);
  Expect<String>(FRecordedEvents[0]).ToBe('test-body');
  Expect<String>(FRecordedEvents[1]).ToBe('finished');
end;

procedure TTestOnTestFinished.TestOnTestFinishedMultipleInOrder;
var
  DescribeFunc: TGocciaFunctionBase;
  SuiteArgs: TGocciaArgumentsCollection;
begin
  DescribeFunc := ResolveGlobalCallable('describe');
  SuiteArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('suite'),
    TGocciaNativeFunctionValue.Create(RegisterSuiteWithMultipleCallbacks, 'registerSuite', 0)
  ]);
  try
    DescribeFunc.Call(SuiteArgs, nil);
  finally
    SuiteArgs.Free;
  end;

  RunRegisteredTests;
  // Order: test-body, finished-A, finished-B
  Expect<Integer>(FRecordedEvents.Count).ToBe(3);
  Expect<String>(FRecordedEvents[0]).ToBe('test-body');
  Expect<String>(FRecordedEvents[1]).ToBe('finished-A');
  Expect<String>(FRecordedEvents[2]).ToBe('finished-B');
end;

procedure TTestOnTestFinished.TestOnTestFinishedRunsAfterAfterEach;
var
  DescribeFunc: TGocciaFunctionBase;
  SuiteArgs: TGocciaArgumentsCollection;
begin
  DescribeFunc := ResolveGlobalCallable('describe');
  SuiteArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('suite'),
    TGocciaNativeFunctionValue.Create(RegisterSuiteWithAfterEachAndCallback, 'registerSuite', 0)
  ]);
  try
    DescribeFunc.Call(SuiteArgs, nil);
  finally
    SuiteArgs.Free;
  end;

  RunRegisteredTests;
  // Order: test-body, afterEach, finished
  Expect<Integer>(FRecordedEvents.Count).ToBe(3);
  Expect<String>(FRecordedEvents[0]).ToBe('test-body');
  Expect<String>(FRecordedEvents[1]).ToBe('afterEach');
  Expect<String>(FRecordedEvents[2]).ToBe('finished');
end;

procedure TTestOnTestFinished.TestOnTestFinishedScopedToCurrentTest;
var
  DescribeFunc: TGocciaFunctionBase;
  SuiteArgs: TGocciaArgumentsCollection;
begin
  DescribeFunc := ResolveGlobalCallable('describe');
  SuiteArgs := TGocciaArgumentsCollection.Create([
    TGocciaStringLiteralValue.Create('suite'),
    TGocciaNativeFunctionValue.Create(RegisterSuiteWithScopedCallbacks, 'registerSuite', 0)
  ]);
  try
    DescribeFunc.Call(SuiteArgs, nil);
  finally
    SuiteArgs.Free;
  end;

  RunRegisteredTests;
  // First test: test-body, finished. Second test: test-body-no-cb (no "finished" callback)
  Expect<Integer>(FRecordedEvents.Count).ToBe(3);
  Expect<String>(FRecordedEvents[0]).ToBe('test-body');
  Expect<String>(FRecordedEvents[1]).ToBe('finished');
  Expect<String>(FRecordedEvents[2]).ToBe('test-body-no-cb');
end;

{ ============================================================================ }
{ TTestResolvesAndRejects                                                     }
{ ============================================================================ }

procedure TTestResolvesAndRejects.DummyThrowError(const AMessage: string; const ALine, AColumn: Integer);
begin
  raise Exception.Create(AMessage);
end;

procedure TTestResolvesAndRejects.BeforeAll;
begin
  FScope := TGocciaGlobalScope.Create;
  FAssertions := TGocciaTestAssertions.Create('test', FScope, DummyThrowError);
  FAssertions.SuppressOutput := True;
end;

procedure TTestResolvesAndRejects.AfterAll;
begin
  FAssertions.Free;
  FScope.Free;
end;

procedure TTestResolvesAndRejects.BeforeEach;
begin
  FAssertions.ResetCurrentTestState;
end;

function TTestResolvesAndRejects.MakeExpectation(const AValue: TGocciaValue; const AIsNegated: Boolean): TGocciaExpectationValue;
begin
  Result := TGocciaExpectationValue.Create(AValue, FAssertions, AIsNegated);
end;

procedure TTestResolvesAndRejects.ExpectPass(const AValue: TGocciaValue);
begin
  Expect<Boolean>(AValue = TGocciaUndefinedLiteralValue.UndefinedValue).ToBe(True);
  Expect<Boolean>(FAssertions.CurrentTestHasFailures).ToBe(False);
end;

procedure TTestResolvesAndRejects.ExpectFail(const AValue: TGocciaValue);
begin
  Expect<Boolean>(AValue = TGocciaUndefinedLiteralValue.UndefinedValue).ToBe(True);
  Expect<Boolean>(FAssertions.CurrentTestHasFailures).ToBe(True);
end;

procedure TTestResolvesAndRejects.SetupTests;
begin
  { resolves }
  Test('resolves with fulfilled Promise unwraps value', TestResolvesWithFulfilledPromise);
  Test('resolves fails with rejected Promise', TestResolvesFailsWithRejectedPromise);
  Test('resolves fails with non-Promise value', TestResolvesFailsWithNonPromise);
  Test('resolves with negation passes for different value', TestResolvesWithNegation);

  { rejects }
  Test('rejects with rejected Promise unwraps reason', TestRejectsWithRejectedPromise);
  Test('rejects fails with fulfilled Promise', TestRejectsFailsWithFulfilledPromise);
  Test('rejects fails with non-Promise value', TestRejectsFailsWithNonPromise);
end;

procedure TTestResolvesAndRejects.TestResolvesWithFulfilledPromise;
var
  Promise: TGocciaPromiseValue;
  Resolved: TGocciaExpectationValue;
  A: TGocciaArgumentsCollection;
begin
  Promise := TGocciaPromiseValue.Create;
  Promise.Resolve(TGocciaNumberLiteralValue.Create(42));

  Resolved := MakeExpectation(Promise).GetResolves(nil, nil) as TGocciaExpectationValue;
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(42)]);
  try
    ExpectPass(Resolved.ToBe(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestResolvesAndRejects.TestResolvesFailsWithRejectedPromise;
var
  Promise: TGocciaPromiseValue;
  ResolvedExpectation: TGocciaValue;
begin
  Promise := TGocciaPromiseValue.Create;
  Promise.Reject(TGocciaStringLiteralValue.Create('error'));

  ResolvedExpectation := MakeExpectation(Promise).GetResolves(nil, nil);
  // GetResolves should record a failure for rejected promises
  Expect<Boolean>(FAssertions.CurrentTestHasFailures).ToBe(True);
  Expect<Boolean>(ResolvedExpectation is TGocciaExpectationValue).ToBe(True);
end;

procedure TTestResolvesAndRejects.TestResolvesFailsWithNonPromise;
var
  ResolvedExpectation: TGocciaValue;
begin
  ResolvedExpectation := MakeExpectation(TGocciaNumberLiteralValue.Create(42)).GetResolves(nil, nil);
  // GetResolves should record a failure for non-Promise values
  Expect<Boolean>(FAssertions.CurrentTestHasFailures).ToBe(True);
  Expect<Boolean>(ResolvedExpectation is TGocciaExpectationValue).ToBe(True);
end;

procedure TTestResolvesAndRejects.TestResolvesWithNegation;
var
  Promise: TGocciaPromiseValue;
  Resolved: TGocciaExpectationValue;
  A: TGocciaArgumentsCollection;
begin
  Promise := TGocciaPromiseValue.Create;
  Promise.Resolve(TGocciaNumberLiteralValue.Create(10));

  // Create non-negated expectation, GetResolves preserves negation state
  // Then chain with GetNot to negate, then ToBe(20) should pass
  Resolved := MakeExpectation(Promise).GetResolves(nil, nil) as TGocciaExpectationValue;
  // Get .not on the resolved expectation
  Resolved := Resolved.GetNot(nil, nil) as TGocciaExpectationValue;
  A := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(20)]);
  try
    ExpectPass(Resolved.ToBe(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestResolvesAndRejects.TestRejectsWithRejectedPromise;
var
  Promise: TGocciaPromiseValue;
  Rejected: TGocciaExpectationValue;
  A: TGocciaArgumentsCollection;
begin
  Promise := TGocciaPromiseValue.Create;
  Promise.Reject(TGocciaStringLiteralValue.Create('error'));

  Rejected := MakeExpectation(Promise).GetRejects(nil, nil) as TGocciaExpectationValue;
  A := TGocciaArgumentsCollection.Create([TGocciaStringLiteralValue.Create('error')]);
  try
    ExpectPass(Rejected.ToBe(A, nil));
  finally
    A.Free;
  end;
end;

procedure TTestResolvesAndRejects.TestRejectsFailsWithFulfilledPromise;
var
  Promise: TGocciaPromiseValue;
  RejectedExpectation: TGocciaValue;
begin
  Promise := TGocciaPromiseValue.Create;
  Promise.Resolve(TGocciaNumberLiteralValue.Create(42));

  RejectedExpectation := MakeExpectation(Promise).GetRejects(nil, nil);
  // GetRejects should record a failure for fulfilled promises
  Expect<Boolean>(FAssertions.CurrentTestHasFailures).ToBe(True);
  Expect<Boolean>(RejectedExpectation is TGocciaExpectationValue).ToBe(True);
end;

procedure TTestResolvesAndRejects.TestRejectsFailsWithNonPromise;
var
  RejectedExpectation: TGocciaValue;
begin
  RejectedExpectation := MakeExpectation(TGocciaNumberLiteralValue.Create(42)).GetRejects(nil, nil);
  // GetRejects should record a failure for non-Promise values
  Expect<Boolean>(FAssertions.CurrentTestHasFailures).ToBe(True);
  Expect<Boolean>(RejectedExpectation is TGocciaExpectationValue).ToBe(True);
end;

begin
  TestRunnerProgram.AddSuite(TTestExpectationMatchers.Create('Test Assertions - Expectation Matchers'));
  TestRunnerProgram.AddSuite(TTestSkipAndConditionalAPIs.Create('Test Assertions - Skip and Conditional APIs'));
  TestRunnerProgram.AddSuite(TTestMockAndSpyAPIs.Create('Test Assertions - Mock and Spy APIs'));
  TestRunnerProgram.AddSuite(TTestOnTestFinished.Create('Test Assertions - onTestFinished'));
  TestRunnerProgram.AddSuite(TTestResolvesAndRejects.Create('Test Assertions - Resolves and Rejects'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
