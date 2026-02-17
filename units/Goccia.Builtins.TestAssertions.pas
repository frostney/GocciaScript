unit Goccia.Builtins.TestAssertions;

{$I Goccia.inc}

interface

uses
  Goccia.Builtins.Base,
  Goccia.Values.ObjectValue,
  Goccia.Values.FunctionValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives,
  Goccia.Values.ArrayValue,
  Goccia.Arguments.Collection,
  Goccia.Scope,
  Goccia.Error, Goccia.Error.ThrowErrorCallback,
  Goccia.Arguments.Validator,
  Generics.Collections,
  Math,
  Classes,
  SysUtils;

type
  TGocciaTestAssertions = class;

  // Class to hold registered test suites (instead of record)
  TTestSuite = class
  public
    Name: string;
    SuiteFunction: TGocciaFunctionValue;
    constructor Create(const AName: string; ASuiteFunction: TGocciaFunctionValue);
  end;

  // Class to hold registered tests (instead of record)
  TTestCase = class
  public
    Name: string;
    TestFunction: TGocciaFunctionValue;
    SuiteName: string;
    IsSkipped: Boolean;
    constructor Create(const AName: string; ATestFunction: TGocciaFunctionValue; const ASuiteName: string; AIsSkipped: Boolean = False);
  end;

  // Expectation object that provides matchers
  TGocciaExpectationValue = class(TGocciaObjectValue)
  private
    FActualValue: TGocciaValue;
    FIsNegated: Boolean;
    FTestAssertions: TGocciaTestAssertions; // Reference to parent
  public
    constructor Create(ActualValue: TGocciaValue; TestAssertions: TGocciaTestAssertions; IsNegated: Boolean = False);

    // Core matchers
    function ToBe(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ToEqual(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeNull(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeNaN(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeUndefined(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeTruthy(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeFalsy(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeGreaterThan(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeGreaterThanOrEqual(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeLessThan(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeLessThanOrEqual(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ToContain(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeInstanceOf(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ToHaveLength(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ToHaveProperty(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ToThrow(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeCloseTo(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    // Negation support
    function GetNot(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
  end;

  // Main test assertions builtin
  TGocciaTestAssertions = class(TGocciaBuiltin)
  private
    FTestStats: record
      TotalTests: Integer;
      PassedTests: Integer;
      FailedTests: Integer;
      SkippedTests: Integer;
      CurrentSuiteName: string;
      CurrentTestName: string;
      CurrentTestHasFailures: Boolean;
      CurrentTestIsSkipped: Boolean;
      CurrentTestAssertionCount: Integer;  // Assertions in current test
      TotalAssertionCount: Integer;        // Total assertions across all tests
    end;

    FRegisteredSuites: TObjectList<TTestSuite>;
    FRegisteredTests: TObjectList<TTestCase>;
    FBeforeEachCallbacks: TGocciaArgumentsCollection;
    FAfterEachCallbacks: TGocciaArgumentsCollection;

    procedure RunCallbacks(Callbacks: TGocciaArgumentsCollection);
    procedure AssertionPassed(const TestName: string);
    procedure AssertionFailed(const TestName, Message: string);
    procedure StartTest(const TestName: string);
    procedure EndTest;
    procedure ResetTestStats;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
    destructor Destroy; override;

    // Main expect function
    function Expect(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    // Test registration functions (don't execute immediately)
    function Describe(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function Test(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function It(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function Skip(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;


    // Setup/teardown
    function BeforeEach(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function AfterEach(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    // Test execution
    function RunTests(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  Goccia.Values.ClassValue, Goccia.Evaluator, Goccia.Evaluator.Comparison,
  Goccia.Values.ObjectPropertyDescriptor, Goccia.Values.Error, Goccia.Values.ClassHelper,
  Goccia.Values.PromiseValue, Goccia.MicrotaskQueue, TimingUtils;

{ TTestSuite }

constructor TTestSuite.Create(const AName: string; ASuiteFunction: TGocciaFunctionValue);
begin
  inherited Create;
  Name := AName;
  SuiteFunction := ASuiteFunction;
end;

{ TTestCase }

constructor TTestCase.Create(const AName: string; ATestFunction: TGocciaFunctionValue; const ASuiteName: string; AIsSkipped: Boolean = False);
begin
  inherited Create;
  Name := AName;
  TestFunction := ATestFunction;
  SuiteName := ASuiteName;
  IsSkipped := AIsSkipped;
end;

{ TGocciaExpectationValue }

constructor TGocciaExpectationValue.Create(ActualValue: TGocciaValue; TestAssertions: TGocciaTestAssertions; IsNegated: Boolean);
begin
  inherited Create;
  FActualValue := ActualValue;
  FTestAssertions := TestAssertions;
  FIsNegated := IsNegated;

  // Add matcher methods
  DefineProperty('toBe', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToBe, 'toBe', 1), [pfConfigurable, pfWritable]));
  DefineProperty('toEqual', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToEqual, 'toEqual', 1), [pfConfigurable, pfWritable]));
  DefineProperty('toBeNull', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToBeNull, 'toBeNull', 0), [pfConfigurable, pfWritable]));
  DefineProperty('toBeNaN', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToBeNaN, 'toBeNaN', 0), [pfConfigurable, pfWritable]));
  DefineProperty('toBeUndefined', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToBeUndefined, 'toBeUndefined', 0), [pfConfigurable, pfWritable]));
  DefineProperty('toBeTruthy', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToBeTruthy, 'toBeTruthy', 0), [pfConfigurable, pfWritable]));
  DefineProperty('toBeFalsy', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToBeFalsy, 'toBeFalsy', 0), [pfConfigurable, pfWritable]));
  DefineProperty('toBeGreaterThan', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToBeGreaterThan, 'toBeGreaterThan', 1), [pfConfigurable, pfWritable]));
  DefineProperty('toBeGreaterThanOrEqual', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToBeGreaterThanOrEqual, 'toBeGreaterThanOrEqual', 1), [pfConfigurable, pfWritable]));
  DefineProperty('toBeLessThan', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToBeLessThan, 'toBeLessThan', 1), [pfConfigurable, pfWritable]));
  DefineProperty('toBeLessThanOrEqual', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToBeLessThanOrEqual, 'toBeLessThanOrEqual', 1), [pfConfigurable, pfWritable]));
  DefineProperty('toContain', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToContain, 'toContain', 1), [pfConfigurable, pfWritable]));
  DefineProperty('toBeInstanceOf', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToBeInstanceOf, 'toBeInstanceOf', 1), [pfConfigurable, pfWritable]));
  DefineProperty('toHaveLength', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToHaveLength, 'toHaveLength', 1), [pfConfigurable, pfWritable]));
  DefineProperty('toHaveProperty', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToHaveProperty, 'toHaveProperty', 2), [pfConfigurable, pfWritable]));
  DefineProperty('toThrow', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToThrow, 'toThrow', 1), [pfConfigurable, pfWritable]));
  DefineProperty('toBeCloseTo', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToBeCloseTo, 'toBeCloseTo', 2), [pfConfigurable, pfWritable]));

  // Negation property - use accessor to make it a getter
  DefineProperty('not', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.Create(GetNot, 'not', 0), nil, [pfConfigurable]));
end;

function TGocciaExpectationValue.ToBe(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  IsEqual: Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'toBe', FTestAssertions.ThrowError);

  Expected := Args.GetElement(0);
  IsEqual := IsSameValue(FActualValue, Expected);

  if FIsNegated then
    IsEqual := not IsEqual;

  if IsEqual then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBe');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBe',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' not to be ' + Expected.ToStringLiteral.Value)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBe',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' to be ' + Expected.ToStringLiteral.Value);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToEqual(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  IsEqual: Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'toEqual', FTestAssertions.ThrowError);

  Expected := Args.GetElement(0);
  IsEqual := IsDeepEqual(FActualValue, Expected);

  if FIsNegated then
    IsEqual := not IsEqual;

  if IsEqual then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toEqual');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toEqual',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' not to equal ' + Expected.ToStringLiteral.Value)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toEqual',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' to equal ' + Expected.ToStringLiteral.Value);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToBeNull(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  IsNull: Boolean;
begin
  IsNull := FActualValue is TGocciaNullLiteralValue;

  if FIsNegated then
    IsNull := not IsNull;

  if IsNull then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeNull');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeNull',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' not to be null')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeNull',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' to be null');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToBeNaN(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  IsNaNValue: Boolean;
begin
  // Use our safe NumberValue.IsNaN method instead of Math.IsNaN on ToNumber
  if FActualValue is TGocciaNumberLiteralValue then
    IsNaNValue := TGocciaNumberLiteralValue(FActualValue).IsNaN
  else
    IsNaNValue := Math.IsNaN(FActualValue.ToNumberLiteral.Value);

  if FIsNegated then
    IsNaNValue := not IsNaNValue;

  if IsNaNValue then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeNaN');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeNaN',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' not to be NaN')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeNaN',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' to be NaN');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToBeUndefined(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  IsUndefined: Boolean;
begin
  IsUndefined := FActualValue is TGocciaUndefinedLiteralValue;

  if FIsNegated then
    IsUndefined := not IsUndefined;

  if IsUndefined then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeUndefined');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeUndefined',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' not to be undefined')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeUndefined',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' to be undefined');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToBeTruthy(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  IsTruthy: Boolean;
begin
  IsTruthy := FActualValue.ToBooleanLiteral.Value;

  if FIsNegated then
    IsTruthy := not IsTruthy;

  if IsTruthy then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeTruthy');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeTruthy',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' not to be truthy')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeTruthy',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' to be truthy');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToBeFalsy(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  IsFalsy: Boolean;
begin
  IsFalsy := not FActualValue.ToBooleanLiteral.Value;

  if FIsNegated then
    IsFalsy := not IsFalsy;

  if IsFalsy then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeFalsy');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeFalsy',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' not to be falsy')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeFalsy',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' to be falsy');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToBeGreaterThan(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  IsGreater: Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'toBeGreaterThan', FTestAssertions.ThrowError);

  Expected := Args.GetElement(0);
  IsGreater := FActualValue.IsGreaterThan(Expected).Value;

  if FIsNegated then
    IsGreater := not IsGreater;

  if IsGreater then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeGreaterThan');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeGreaterThan',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' not to be greater than ' + Expected.ToStringLiteral.Value)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeGreaterThan',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' to be greater than ' + Expected.ToStringLiteral.Value);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToBeGreaterThanOrEqual(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  IsGreaterOrEqual: Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'toBeGreaterThanOrEqual', FTestAssertions.ThrowError);

  Expected := Args.GetElement(0);
  IsGreaterOrEqual := FActualValue.IsGreaterThanOrEqual(Expected).Value;

  if FIsNegated then
    IsGreaterOrEqual := not IsGreaterOrEqual;

  if IsGreaterOrEqual then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeGreaterThanOrEqual');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeGreaterThanOrEqual',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' not to be greater than or equal to ' + Expected.ToStringLiteral.Value)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeGreaterThanOrEqual',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' to be greater than or equal to ' + Expected.ToStringLiteral.Value);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToBeLessThan(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  IsLess: Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'toBeLessThan', FTestAssertions.ThrowError);

  Expected := Args.GetElement(0);
  IsLess := FActualValue.IsLessThan(Expected).Value;

  if FIsNegated then
    IsLess := not IsLess;

  if IsLess then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeLessThan');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeLessThan',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' not to be less than ' + Expected.ToStringLiteral.Value)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeLessThan',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' to be less than ' + Expected.ToStringLiteral.Value);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToBeLessThanOrEqual(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  IsLessOrEqual: Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'toBeLessThanOrEqual', FTestAssertions.ThrowError);

  Expected := Args.GetElement(0);
  IsLessOrEqual := FActualValue.IsLessThanOrEqual(Expected).Value;

  if FIsNegated then
    IsLessOrEqual := not IsLessOrEqual;

  if IsLessOrEqual then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeLessThanOrEqual');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeLessThanOrEqual',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' not to be less than or equal to ' + Expected.ToStringLiteral.Value)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeLessThanOrEqual',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' to be less than or equal to ' + Expected.ToStringLiteral.Value);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToContain(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  Contains: Boolean;
  ActualStr, ExpectedStr: string;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'toContain', FTestAssertions.ThrowError);

  Expected := Args.GetElement(0);

  // For strings, check substring
  if FActualValue is TGocciaStringLiteralValue then
  begin
    ActualStr := FActualValue.ToStringLiteral.Value;
    ExpectedStr := Expected.ToStringLiteral.Value;
    Contains := Pos(ExpectedStr, ActualStr) > 0;
  end
  else
  begin
    if FActualValue is TGocciaArrayValue then
    begin
      Contains := TGocciaArrayValue(FActualValue).Includes(Expected);
    end
    else if FActualValue is TGocciaObjectValue then
    begin
      Contains := TGocciaObjectValue(FActualValue).HasOwnProperty(Expected.ToStringLiteral.Value);
    end else
    begin
      Contains := FActualValue.ToStringLiteral.Value = Expected.ToStringLiteral.Value;
    end;
  end;

  if FIsNegated then
    Contains := not Contains;

  if Contains then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toContain');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toContain',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' not to contain ' + Expected.ToStringLiteral.Value)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toContain',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' to contain ' + Expected.ToStringLiteral.Value);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToBeInstanceOf(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  ExpectedConstructor: TGocciaValue;
  IsInstance: Boolean;
  ConstructorName: string;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  TGocciaArgumentValidator.RequireExactly(Args, 1, 'toBeInstanceOf', FTestAssertions.ThrowError);

  ExpectedConstructor := Args.GetElement(0);
  IsInstance := False;

    // Check for built-in types
  if ExpectedConstructor is TGocciaNativeFunctionValue then
  begin
    ConstructorName := TGocciaNativeFunctionValue(ExpectedConstructor).Name;
    if ConstructorName = 'Function' then
    begin
      // Check if actual value is any kind of function
      IsInstance := (FActualValue is TGocciaFunctionValue) or
                   (FActualValue is TGocciaNativeFunctionValue) or
                   (FActualValue.ClassName = 'TGocciaFunctionPrototypeMethod') or
                   (FActualValue.ClassName = 'TGocciaBoundFunctionValue');
    end
    else if ConstructorName = 'Array' then
    begin
      IsInstance := FActualValue is TGocciaArrayValue;
    end
    else if ConstructorName = 'Object' then
    begin
      IsInstance := FActualValue is TGocciaObjectValue;
    end
    else if ConstructorName = 'String' then
    begin
      IsInstance := FActualValue is TGocciaStringLiteralValue;
    end
    else if ConstructorName = 'Number' then
    begin
      IsInstance := FActualValue is TGocciaNumberLiteralValue;
    end
    else if ConstructorName = 'Boolean' then
    begin
      IsInstance := FActualValue is TGocciaBooleanLiteralValue;
    end;
  end
    else if ExpectedConstructor is TGocciaClassValue then
  begin
    ConstructorName := TGocciaClassValue(ExpectedConstructor).Name;
    if ConstructorName = 'Function' then
    begin
      // Check if actual value is any kind of function
      IsInstance := (FActualValue is TGocciaFunctionValue) or
                   (FActualValue is TGocciaNativeFunctionValue) or
                   (FActualValue.ClassName = 'TGocciaFunctionPrototypeMethod') or
                   (FActualValue.ClassName = 'TGocciaBoundFunctionValue');
    end
    else
    begin
      // Check if the actual value is an instance of the class
      if FActualValue is TGocciaInstanceValue then
      begin
        IsInstance := IsObjectInstanceOfClass(TGocciaInstanceValue(FActualValue), TGocciaClassValue(ExpectedConstructor));
      end;
    end;
  end;

  if FIsNegated then
    IsInstance := not IsInstance;

  if IsInstance then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeInstanceOf');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeInstanceOf',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' not to be an instance of ' + ExpectedConstructor.ToStringLiteral.Value)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeInstanceOf',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' to be an instance of ' + ExpectedConstructor.ToStringLiteral.Value);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToHaveLength(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  HasLength: Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'toHaveLength', FTestAssertions.ThrowError);

  Expected := Args.GetElement(0);

  if FActualValue is TGocciaArrayValue then
  begin
    HasLength := TGocciaArrayValue(FActualValue).Elements.Count = Expected.ToNumberLiteral.Value;
  end
  else if FActualValue is TGocciaObjectValue then
  begin
    HasLength := Length(TGocciaObjectValue(FActualValue).GetAllPropertyNames) = Expected.ToNumberLiteral.Value;
  end
  else if FActualValue is TGocciaStringLiteralValue then
  begin
    HasLength := Length(FActualValue.ToStringLiteral.Value) = Expected.ToNumberLiteral.Value;
  end
  else
  begin
    HasLength := FActualValue.ToNumberLiteral.Value = Expected.ToNumberLiteral.Value;
  end;

  if FIsNegated then
    HasLength := not HasLength;

  if HasLength then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toHaveLength');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveLength',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' not to have length ' + Expected.ToStringLiteral.Value)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveLength',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' to have length ' + Expected.ToStringLiteral.Value);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToHaveProperty(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaObjectValue;
  HasProperty: Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 2, 'toHaveProperty', FTestAssertions.ThrowError);

  Expected := Args.GetElement(0) as TGocciaObjectValue;
  HasProperty := Expected.HasProperty(Args.GetElement(1).ToStringLiteral.Value);

  if FIsNegated then
    HasProperty := not HasProperty;

  if HasProperty then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toHaveProperty');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveProperty',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' not to have property ' + Expected.ToStringLiteral.Value)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveProperty',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' to have property ' + Expected.ToStringLiteral.Value);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToThrow(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  ExpectedErrorType: string;
  EmptyArgs: TGocciaArgumentsCollection;
  TestContext: TGocciaEvaluationContext;
  TestFunc: TGocciaFunctionValue;
  TestScope: TGocciaScope;
  I: Integer;
  ThrownObj: TGocciaObjectValue;
  ErrorConstructor: TGocciaValue;
  ConstructorName: string;
  ErrorName: string;
  ErrorClassArg: TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  ExpectedErrorType := '';
  if Args.Length > 0 then
  begin
    // If args are provided, expect a specific error type
    ErrorClassArg := Args.GetElement(0);
    // If it's a class, get its name; otherwise use string representation
    if ErrorClassArg is TGocciaClassValue then
      ExpectedErrorType := TGocciaClassValue(ErrorClassArg).Name
    else if ErrorClassArg is TGocciaNativeFunctionValue then
      ExpectedErrorType := TGocciaNativeFunctionValue(ErrorClassArg).Name
    else
      ExpectedErrorType := ErrorClassArg.ToStringLiteral.Value;
  end;

  if not (FActualValue is TGocciaFunctionValue) then
  begin
    FTestAssertions.ThrowError('toThrow expects actual value to be a function', 0, 0);
    Exit;
  end;

    TestFunc := TGocciaFunctionValue(FActualValue);
  EmptyArgs := TGocciaArgumentsCollection.Create;

  // Create a strict test scope that throws for undefined variables
          TestScope := TestFunc.Closure.CreateChild(skFunction, 'ToThrow Test');
  try
    // Set up evaluation context with an OnError handler that throws exceptions
    TestContext.Scope := TestScope;
    TestContext.OnError := FTestAssertions.ThrowError;
    TestContext.LoadModule := nil;

    try
      // Execute all function body statements with proper context
      for I := 0 to TestFunc.BodyStatements.Count - 1 do
      begin
        // Guard against invalid AST nodes from syntax errors
        if TestFunc.BodyStatements[I] <> nil then
          Evaluate(TestFunc.BodyStatements[I], TestContext)
        else
          raise TGocciaSyntaxError.Create('Invalid syntax in function body', 0, 0, '', nil);
      end;
    except
      on E: TGocciaThrowValue do
      begin
        // Handle thrown JavaScript values (e.g., throw new TypeError())
        if ExpectedErrorType = '' then
        begin
          TGocciaTestAssertions(FTestAssertions).AssertionPassed('toThrow');
          Exit;
        end;

        // Check if thrown value matches expected error type
        if E.Value is TGocciaObjectValue then
        begin
          ThrownObj := TGocciaObjectValue(E.Value);

          // Check if the error object has a name property that matches
          if ThrownObj.HasProperty('name') then
          begin
            ErrorName := ThrownObj.GetProperty('name').ToStringLiteral.Value;
            // Direct comparison: "TypeError" == "TypeError"
            if (ErrorName = ExpectedErrorType) or
               (LowerCase(ErrorName) = LowerCase(ExpectedErrorType)) then
            begin
              TGocciaTestAssertions(FTestAssertions).AssertionPassed('toThrow');
              Exit;
            end;
          end;

          // Fallback: check constructor property
          if ThrownObj.HasProperty('constructor') then
          begin
            ErrorConstructor := ThrownObj.GetProperty('constructor');
            if ErrorConstructor.ToStringLiteral.Value = ExpectedErrorType then
            begin
              TGocciaTestAssertions(FTestAssertions).AssertionPassed('toThrow');
              Exit;
            end;
            if ErrorConstructor is TGocciaNativeFunctionValue then
            begin
              ConstructorName := TGocciaNativeFunctionValue(ErrorConstructor).Name;
              if (Pos(ConstructorName, ExpectedErrorType) > 0) or
                 (Pos(LowerCase(ConstructorName), LowerCase(ExpectedErrorType)) > 0) then
              begin
                TGocciaTestAssertions(FTestAssertions).AssertionPassed('toThrow');
                Exit;
              end;
            end;
          end;
        end;

        // Fallback to string matching in the thrown value
        if (Pos(LowerCase(ExpectedErrorType), LowerCase(E.Value.ToStringLiteral.Value)) > 0) then
        begin
          TGocciaTestAssertions(FTestAssertions).AssertionPassed('toThrow');
          Exit;
        end
        else
        begin
          TGocciaTestAssertions(FTestAssertions).AssertionFailed('toThrow',
            'Expected ' + FActualValue.ToStringLiteral.Value + ' to throw ' + ExpectedErrorType + ' but threw: ' + E.Value.ToStringLiteral.Value);
          Exit;
        end;
      end;
      on E: TGocciaError do
      begin
        // Handle Goccia-specific errors (syntax, runtime, etc.)
        if ExpectedErrorType = '' then
        begin
          TGocciaTestAssertions(FTestAssertions).AssertionPassed('toThrow');
          Exit;
        end;

        if (Pos(LowerCase(ExpectedErrorType), LowerCase(E.Message)) > 0) then
        begin
          TGocciaTestAssertions(FTestAssertions).AssertionPassed('toThrow');
          Exit;
        end
        else
        begin
          TGocciaTestAssertions(FTestAssertions).AssertionFailed('toThrow',
            'Expected ' + FActualValue.ToStringLiteral.Value + ' to throw ' + ExpectedErrorType + ' but threw: ' + E.Message);
          Exit;
        end;
      end;
      on E: Exception do
      begin
        // Handle other Pascal exceptions (including EInvalidCast)
        if ExpectedErrorType = '' then
        begin
          TGocciaTestAssertions(FTestAssertions).AssertionPassed('toThrow');
          Exit;
        end;

        // For non-Goccia errors, be more lenient with type matching
        if (Pos(LowerCase(ExpectedErrorType), LowerCase(E.ClassName)) > 0) or
           (Pos(LowerCase(ExpectedErrorType), LowerCase(E.Message)) > 0) then
        begin
          TGocciaTestAssertions(FTestAssertions).AssertionPassed('toThrow');
          Exit;
        end
        else
        begin
          TGocciaTestAssertions(FTestAssertions).AssertionFailed('toThrow',
            'Expected ' + FActualValue.ToStringLiteral.Value + ' to throw ' + ExpectedErrorType + ' but threw: ' + E.ClassName + ': ' + E.Message);
          Exit;
        end;
      end;
    end;
  finally
    EmptyArgs.Free;
    TestScope.Free;  // Properly free the test scope to prevent access violations
  end;

  TGocciaTestAssertions(FTestAssertions).AssertionFailed('toThrow',
    'Expected ' + FActualValue.ToStringLiteral.Value + ' to throw an exception');
end;

function TGocciaExpectationValue.ToBeCloseTo(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  Precision: Integer;
  ActualNum, ExpectedNum, Diff, Tolerance: Double;
  IsClose: Boolean;
  ActualTempNum, ExpectedTempNum: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(Args, 1, 'toBeCloseTo', FTestAssertions.ThrowError);

  Expected := Args.GetElement(0);

  // Default precision to 2 decimal places if not specified
  if Args.Length >= 2 then
    Precision := Trunc(Args.GetElement(1).ToNumberLiteral.Value)
  else
    Precision := 2;

  ActualNum := FActualValue.ToNumberLiteral.Value;
  ExpectedNum := Expected.ToNumberLiteral.Value;

  // Check infinity using TGocciaNumberLiteralValue properties
  ActualTempNum := TGocciaNumberLiteralValue.Create(ActualNum);
  ExpectedTempNum := TGocciaNumberLiteralValue.Create(ExpectedNum);
  try
    // Handle special cases for NaN first
    if ActualTempNum.IsNaN and ExpectedTempNum.IsNaN then
      IsClose := True  // Both NaN should be considered close
    else if ActualTempNum.IsNaN or ExpectedTempNum.IsNaN then
      IsClose := False  // One is NaN, other is not
    // Both Infinity with same sign should be considered close
    else if (ActualTempNum.IsInfinity or ActualTempNum.IsNegativeInfinity) and
       (ExpectedTempNum.IsInfinity or ExpectedTempNum.IsNegativeInfinity) and
       ((ActualTempNum.IsInfinity and ExpectedTempNum.IsInfinity) or
        (ActualTempNum.IsNegativeInfinity and ExpectedTempNum.IsNegativeInfinity)) then
      IsClose := True
    // One is infinity, other is not
    else if (ActualTempNum.IsInfinity or ActualTempNum.IsNegativeInfinity) or
            (ExpectedTempNum.IsInfinity or ExpectedTempNum.IsNegativeInfinity) then
      IsClose := False
    else
    begin
      // Calculate tolerance based on precision (0.5 * 10^(-precision))
      Tolerance := 0.5 * Math.Power(10, -Precision);
      Diff := Abs(ActualNum - ExpectedNum);
      IsClose := Diff < Tolerance;
    end;
  finally
    ActualTempNum.Free;
    ExpectedTempNum.Free;
  end;

  if FIsNegated then
    IsClose := not IsClose;

  if IsClose then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeCloseTo');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeCloseTo',
        Format('Expected %s not to be close to %s (precision: %d)',
               [FActualValue.ToStringLiteral.Value, Expected.ToStringLiteral.Value, Precision]))
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeCloseTo',
        Format('Expected %s to be close to %s (precision: %d)',
               [FActualValue.ToStringLiteral.Value, Expected.ToStringLiteral.Value, Precision]));
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.GetNot(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  // Return a new expectation object with negation enabled
  Result := TGocciaExpectationValue.Create(FActualValue, FTestAssertions, True);
end;

{ TGocciaTestAssertions }

constructor TGocciaTestAssertions.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  TestFunction: TGocciaNativeFunctionValue;
begin
  inherited Create(AName, AScope, AThrowError);

  FRegisteredSuites := TObjectList<TTestSuite>.Create;
  FRegisteredTests := TObjectList<TTestCase>.Create;
  FBeforeEachCallbacks := TGocciaArgumentsCollection.Create;
  FAfterEachCallbacks := TGocciaArgumentsCollection.Create;

  ResetTestStats;

  // Functions are registered on both the scope (for direct access in test scripts)
  // and the builtin object (for completeness). This dual registration is intentional.

  // Register testing functions globally for easy access
  AScope.DefineLexicalBinding('expect', TGocciaNativeFunctionValue.Create(Expect, 'expect', 1), dtConst);
  AScope.DefineLexicalBinding('describe', TGocciaNativeFunctionValue.Create(Describe, 'describe', 2), dtConst);

  // Create test function with skip property
  TestFunction := TGocciaNativeFunctionValue.Create(Test, 'test', 2);
  TestFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(Skip, 'skip', 2));
  AScope.DefineLexicalBinding('test', TestFunction, dtConst);

  AScope.DefineLexicalBinding('it', TGocciaNativeFunctionValue.Create(It, 'it', 2), dtConst);
  AScope.DefineLexicalBinding('beforeEach', TGocciaNativeFunctionValue.Create(BeforeEach, 'beforeEach', 1), dtConst);
  AScope.DefineLexicalBinding('afterEach', TGocciaNativeFunctionValue.Create(AfterEach, 'afterEach', 1), dtConst);
  AScope.DefineLexicalBinding('runTests', TGocciaNativeFunctionValue.Create(RunTests, 'runTests', 0), dtConst);

  // Also set them in the builtin object for completeness
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(Expect, 'expect', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(Describe, 'describe', 2));
  FBuiltinObject.RegisterNativeMethod(TestFunction);  // Use the same test function with skip property
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(It, 'it', 2));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(BeforeEach, 'beforeEach', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(AfterEach, 'afterEach', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(RunTests, 'runTests', 0));
end;

destructor TGocciaTestAssertions.Destroy;
begin
  FRegisteredSuites.Free;
  FRegisteredTests.Free;
  FBeforeEachCallbacks.Free;
  FAfterEachCallbacks.Free;
  inherited;
end;

procedure TGocciaTestAssertions.ResetTestStats;
begin
  FTestStats.TotalTests := 0;
  FTestStats.PassedTests := 0;
  FTestStats.FailedTests := 0;
  FTestStats.SkippedTests := 0;
  FTestStats.CurrentSuiteName := '';
  FTestStats.CurrentTestName := '';
  FTestStats.CurrentTestHasFailures := False;
  FTestStats.CurrentTestIsSkipped := False;
  FTestStats.CurrentTestAssertionCount := 0;
  FTestStats.TotalAssertionCount := 0;
end;

procedure TGocciaTestAssertions.RunCallbacks(Callbacks: TGocciaArgumentsCollection);
var
  I: Integer;
  Callback: TGocciaValue;
  EmptyArgs: TGocciaArgumentsCollection;
begin
  EmptyArgs := TGocciaArgumentsCollection.Create;
  try
    for I := 0 to Callbacks.Length - 1 do
    begin
      Callback := Callbacks.GetElement(I);
      if Callback is TGocciaFunctionValue then
      begin
        try
          TGocciaFunctionValue(Callback).Call(EmptyArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
        except
          on E: Exception do
          begin
            // Route callback exceptions through proper assertion failure mechanism
            AssertionFailed('callback execution', 'Callback threw an exception: ' + E.Message);
            // Also log for debugging
            WriteLn('Warning: Error in callback: ', E.Message);
          end;
        end;
      end;
    end;
  finally
    EmptyArgs.Free;
  end;
end;

procedure TGocciaTestAssertions.StartTest(const TestName: string);
begin
  FTestStats.CurrentTestName := TestName;
  FTestStats.CurrentTestHasFailures := False;
  FTestStats.CurrentTestIsSkipped := False;
  FTestStats.CurrentTestAssertionCount := 0;
end;

procedure TGocciaTestAssertions.EndTest;
begin
  if FTestStats.CurrentTestIsSkipped then
    Inc(FTestStats.SkippedTests)
  else if FTestStats.CurrentTestHasFailures then
    Inc(FTestStats.FailedTests)
  else
    Inc(FTestStats.PassedTests);
end;

procedure TGocciaTestAssertions.AssertionPassed(const TestName: string);
begin
  Inc(FTestStats.CurrentTestAssertionCount);
  Inc(FTestStats.TotalAssertionCount);
end;

procedure TGocciaTestAssertions.AssertionFailed(const TestName, Message: string);
begin
  Inc(FTestStats.CurrentTestAssertionCount);
  Inc(FTestStats.TotalAssertionCount);
  FTestStats.CurrentTestHasFailures := True;

  // Track the failure details for reporting
  // This is called during test execution, so we know the current test context
  if FTestStats.CurrentSuiteName <> '' then
    WriteLn('    ❌ ', FTestStats.CurrentTestName, ' in ', FTestStats.CurrentSuiteName, ': ', Message)
  else
    WriteLn('    ❌ ', FTestStats.CurrentTestName, ': ', Message);
end;

function TGocciaTestAssertions.Expect(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'expect', ThrowError);

  Result := TGocciaExpectationValue.Create(Args.GetElement(0), Self);
end;

function TGocciaTestAssertions.Describe(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  SuiteName: string;
  SuiteFunction: TGocciaFunctionValue;
  Suite: TTestSuite;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 2, 'describe', ThrowError);

  if not (Args.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowError('describe expects first argument to be a string', 0, 0);

  if not (Args.GetElement(1) is TGocciaFunctionValue) then
    ThrowError('describe expects second argument to be a function', 0, 0);

  SuiteName := Args.GetElement(0).ToStringLiteral.Value;
  SuiteFunction := Args.GetElement(1) as TGocciaFunctionValue;

  // Just register the test suite - do NOT execute it until runTests() is called
  Suite := TTestSuite.Create(SuiteName, SuiteFunction);
  FRegisteredSuites.Add(Suite);

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTestAssertions.Test(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  TestName: string;
  TestFunction: TGocciaFunctionValue;
  TestCase: TTestCase;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 2, 'test', ThrowError);

  if not (Args.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowError('test expects first argument to be a string', 0, 0);

  if not (Args.GetElement(1) is TGocciaFunctionValue) then
    ThrowError('test expects second argument to be a function', 0, 0);

  TestName := Args.GetElement(0).ToStringLiteral.Value;
  TestFunction := Args.GetElement(1) as TGocciaFunctionValue;

  // Register the test with the current suite name
  TestCase := TTestCase.Create(TestName, TestFunction, FTestStats.CurrentSuiteName);
  FRegisteredTests.Add(TestCase);

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTestAssertions.It(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  // 'it' is an alias for 'test'
  Result := Test(Args, ThisValue);
end;

function TGocciaTestAssertions.Skip(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  TestName: string;
  TestFunction: TGocciaFunctionValue;
  TestCase: TTestCase;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 2, 'test.skip', ThrowError);

  if not (Args.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowError('test.skip expects first argument to be a string', 0, 0);

  if not (Args.GetElement(1) is TGocciaFunctionValue) then
    ThrowError('test.skip expects second argument to be a function', 0, 0);

  TestName := Args.GetElement(0).ToStringLiteral.Value;
  TestFunction := Args.GetElement(1) as TGocciaFunctionValue;

  // Register the test as skipped with the current suite name
  TestCase := TTestCase.Create(TestName, TestFunction, FTestStats.CurrentSuiteName, True);
  FRegisteredTests.Add(TestCase);

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTestAssertions.BeforeEach(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'beforeEach', ThrowError);

  if not (Args.GetElement(0) is TGocciaFunctionValue) then
    ThrowError('beforeEach expects a function argument', 0, 0);

  FBeforeEachCallbacks.Add(Args.GetElement(0));

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTestAssertions.AfterEach(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(Args, 1, 'afterEach', ThrowError);

  if not (Args.GetElement(0) is TGocciaFunctionValue) then
    ThrowError('afterEach expects a function argument', 0, 0);

  FAfterEachCallbacks.Add(Args.GetElement(0));

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTestAssertions.RunTests(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  StartTime: Int64;
  Suite: TTestSuite;
  TestCase: TTestCase;
  EmptyArgs: TGocciaArgumentsCollection;
  ResultObj: TGocciaObjectValue;
  ExitOnFirstFailure: Boolean = False;
  ShowTestResults: Boolean = True;
  Silent: Boolean = False;
  Summary: string;
  PreviousSuiteName: string;
  FailedTestDetails: TStringList;
  FailedTestDetailsArray: TGocciaArrayValue;
  ClonedFunction: TGocciaFunctionValue;
  TestParams: TGocciaObjectValue;
  TestResult: TGocciaValue;
  RejectionReason: string;
begin
  // Reset test statistics and clear any previously registered tests from describe blocks
  ResetTestStats;

  StartTime := GetMicroseconds;

  // Clear tests that were registered from previous describe executions
  // Keep standalone tests that were registered during script execution
  for I := FRegisteredTests.Count - 1 downto 0 do
  begin
    if FRegisteredTests[I].SuiteName <> '' then
      FRegisteredTests.Delete(I);
  end;

  if Args.Length > 0 then
  begin
    if Args.GetElement(0) is TGocciaObjectValue then
    begin
      TestParams := Args.GetElement(0) as TGocciaObjectValue;
      if TestParams.HasProperty('exitOnFirstFailure') then
        ExitOnFirstFailure := TestParams.GetProperty('exitOnFirstFailure').ToBooleanLiteral.Value
      else
        ExitOnFirstFailure := False;
      if TestParams.HasProperty('showTestResults') then
        ShowTestResults := TestParams.GetProperty('showTestResults').ToBooleanLiteral.Value
      else
        ShowTestResults := True;
      if TestParams.HasProperty('silent') then
        Silent := TestParams.GetProperty('silent').ToBooleanLiteral.Value
      else
        Silent := False;
    end
    else
    begin
      ExitOnFirstFailure := False;
      ShowTestResults := True;
      Silent := False;
    end;
  end;

  FailedTestDetails := TStringList.Create;
  EmptyArgs := TGocciaArgumentsCollection.Create;
  try
    // First, execute all describe blocks to register their tests
    for I := 0 to FRegisteredSuites.Count - 1 do
    begin
      Suite := FRegisteredSuites[I];

      // Set current suite name so test() calls know which suite they belong to
      PreviousSuiteName := FTestStats.CurrentSuiteName;
      FTestStats.CurrentSuiteName := Suite.Name;

      try
        // Execute the describe function - this will call test() functions inside
        Suite.SuiteFunction.Call(EmptyArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
      except
        on E: Exception do
        begin
          if not Silent then
            WriteLn('Error in describe block "', Suite.Name, '": ', E.Message);
          FailedTestDetails.Add('Describe "' + Suite.Name + '": ' + E.Message);
        end;
      end;

      // Restore previous suite name
      FTestStats.CurrentSuiteName := PreviousSuiteName;
    end;

    // Now execute all registered tests
    for I := 0 to FRegisteredTests.Count - 1 do
    begin
      TestCase := FRegisteredTests[I];
      FTestStats.CurrentSuiteName := TestCase.SuiteName;

      Inc(FTestStats.TotalTests);

      // Start tracking this test
      StartTest(TestCase.Name);

      // Check if test is skipped
      if TestCase.IsSkipped then
      begin
        // Mark test as skipped and don't execute it
        FTestStats.CurrentTestIsSkipped := True;
        if not Silent then
        begin
          if TestCase.SuiteName <> '' then
            WriteLn('    ⏸️ ', TestCase.Name, ' in ', TestCase.SuiteName, ': SKIPPED')
          else
            WriteLn('    ⏸️ ', TestCase.Name, ': SKIPPED');
        end;
      end
      else
      begin
        // Run beforeEach callbacks
        RunCallbacks(FBeforeEachCallbacks);

        try
          // Execute the test function
          ClonedFunction := TestCase.TestFunction.CloneWithNewScope(FScope.CreateChild(skFunction, 'TestFunction'));
          TestResult := ClonedFunction.Call(EmptyArgs, TGocciaUndefinedLiteralValue.UndefinedValue);

          // Drain microtask queue after each test to process Promise reactions
          if Assigned(TGocciaMicrotaskQueue.Instance) then
            TGocciaMicrotaskQueue.Instance.DrainQueue;

          // If the test returned a Promise, check its final state
          if (TestResult is TGocciaPromiseValue) then
          begin
            if TGocciaPromiseValue(TestResult).State = gpsRejected then
            begin
              RejectionReason := TGocciaPromiseValue(TestResult).PromiseResult.ToStringLiteral.Value;
              AssertionFailed('async test', 'Returned Promise rejected: ' + RejectionReason);
              if TestCase.SuiteName <> '' then
                FailedTestDetails.Add('Test "' + TestCase.Name + '" in suite "' + TestCase.SuiteName + '": Promise rejected: ' + RejectionReason)
              else
                FailedTestDetails.Add('Test "' + TestCase.Name + '": Promise rejected: ' + RejectionReason);
            end
            else if TGocciaPromiseValue(TestResult).State = gpsPending then
            begin
              AssertionFailed('async test', 'Returned Promise still pending after microtask drain');
              if TestCase.SuiteName <> '' then
                FailedTestDetails.Add('Test "' + TestCase.Name + '" in suite "' + TestCase.SuiteName + '": Promise still pending after microtask drain')
              else
                FailedTestDetails.Add('Test "' + TestCase.Name + '": Promise still pending after microtask drain');
            end;
          end;
        except
          on E: Exception do
          begin
            // Clear pending microtasks to prevent cross-test callback leakage
            if Assigned(TGocciaMicrotaskQueue.Instance) then
              TGocciaMicrotaskQueue.Instance.ClearQueue;
            // Route exception through proper assertion failure mechanism
            AssertionFailed('test execution', 'Test threw an exception: ' + E.Message);
            if TestCase.SuiteName <> '' then
              FailedTestDetails.Add('Test "' + TestCase.Name + '" in suite "' + TestCase.SuiteName + '": ' + E.Message)
            else
              FailedTestDetails.Add('Test "' + TestCase.Name + '": ' + E.Message);
            if ExitOnFirstFailure then
              Break;
          end;
        end;

        // Run afterEach callbacks
        RunCallbacks(FAfterEachCallbacks);
      end;

      // End tracking this test
      EndTest;

      // Exit on first failure if requested (but not for skipped tests)
      if FTestStats.CurrentTestHasFailures and ExitOnFirstFailure then
        Break;
    end;

  finally
    EmptyArgs.Free;
  end;



  // Create a summary message
  Summary := Format('Tests: %d total, %d passed, %d failed, %d skipped',
    [FTestStats.TotalTests, FTestStats.PassedTests, FTestStats.FailedTests, FTestStats.SkippedTests]);

  if FRegisteredSuites.Count > 0 then
  begin
    Summary := Summary + ' (Suites: ';
    for I := 0 to FRegisteredSuites.Count - 1 do
    begin
      if I > 0 then Summary := Summary + ', ';
      Summary := Summary + FRegisteredSuites[I].Name;
    end;
    Summary := Summary + ')';
  end;

  FailedTestDetailsArray := TGocciaArrayValue.Create;
  if FailedTestDetails.Count > 0 then
  begin
    for I := 0 to FailedTestDetails.Count - 1 do
      FailedTestDetailsArray.Elements.Add(TGocciaStringLiteralValue.Create(FailedTestDetails[I]));
  end;

   // Create result object
  ResultObj := TGocciaObjectValue.Create;
  ResultObj.AssignProperty('totalTests', TGocciaNumberLiteralValue.Create(FRegisteredTests.Count));
  ResultObj.AssignProperty('totalRunTests', TGocciaNumberLiteralValue.Create(FTestStats.TotalTests));
  ResultObj.AssignProperty('passed', TGocciaNumberLiteralValue.Create(FTestStats.PassedTests));
  ResultObj.AssignProperty('failed', TGocciaNumberLiteralValue.Create(FTestStats.FailedTests));
  ResultObj.AssignProperty('skipped', TGocciaNumberLiteralValue.Create(FTestStats.SkippedTests));
  ResultObj.AssignProperty('assertions', TGocciaNumberLiteralValue.Create(FTestStats.TotalAssertionCount));
  ResultObj.AssignProperty('duration', TGocciaNumberLiteralValue.Create(GetMicroseconds - StartTime));
  ResultObj.AssignProperty('failedTests', FailedTestDetailsArray);
  ResultObj.AssignProperty('summary', TGocciaStringLiteralValue.Create(Summary));

  // Print the summary to console for visibility
  if ShowTestResults then
  begin
    WriteLn('');
    WriteLn('=== Test Results ===');
    WriteLn(Summary);
    WriteLn('Total Assertions: ', FTestStats.TotalAssertionCount);

    // Show failed test details
    if FailedTestDetails.Count > 0 then
    begin
      WriteLn('');
      WriteLn('Failed Tests:');
      for I := 0 to FailedTestDetails.Count - 1 do
        WriteLn('  • ', FailedTestDetails[I]);
    end;

    if FTestStats.FailedTests = 0 then
    begin
      if FTestStats.SkippedTests > 0 then
        WriteLn(Format('✅ All tests passed! (%d skipped)', [FTestStats.SkippedTests]))
      else
        WriteLn('✅ All tests passed!');
    end
    else
      WriteLn('❌ Some tests failed!');

      WriteLn('==================');
  end;

  FailedTestDetails.Free;
  Result := ResultObj;
end;

end.
