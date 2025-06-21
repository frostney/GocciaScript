unit Goccia.Builtins.TestAssertions;

{$I Goccia.inc}

interface

uses
  Goccia.Builtins.Base,
  Goccia.Values.Base,
  Goccia.Values.ObjectValue,
  Goccia.Values.FunctionValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.BooleanValue,
  Goccia.Values.NumberValue,
  Goccia.Values.StringValue,
  Goccia.Values.UndefinedValue,
  Goccia.Values.NullValue,
  Goccia.Values.ArrayValue,
  Goccia.Scope,
  Goccia.Error,
  Generics.Collections,
  Math,
  Classes;

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
    function ToBe(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ToEqual(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeNull(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeNaN(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeUndefined(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeTruthy(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeFalsy(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeGreaterThan(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeGreaterThanOrEqual(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeLessThan(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeLessThanOrEqual(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ToContain(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeInstanceOf(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ToHaveLength(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ToHaveProperty(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ToThrow(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;

    // Negation support
    function GetNot(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
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
    FBeforeEachCallbacks: TObjectList<TGocciaValue>;
    FAfterEachCallbacks: TObjectList<TGocciaValue>;

    procedure RunCallbacks(Callbacks: TObjectList<TGocciaValue>);
    procedure AssertionPassed(const TestName: string);
    procedure AssertionFailed(const TestName, Message: string);
    procedure StartTest(const TestName: string);
    procedure EndTest;
    procedure ResetTestStats;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
    destructor Destroy; override;

    // Main expect function
    function Expect(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;

    // Test registration functions (don't execute immediately)
    function Describe(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function Test(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function It(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function Skip(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;


    // Setup/teardown
    function BeforeEach(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function AfterEach(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;

    // Test execution
    function RunTests(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  SysUtils, Goccia.Values.ClassValue, Goccia.Evaluator;

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
  SetProperty('toBe', TGocciaNativeFunctionValue.Create(ToBe, 'toBe', 1));
  SetProperty('toEqual', TGocciaNativeFunctionValue.Create(ToEqual, 'toEqual', 1));
  SetProperty('toBeNull', TGocciaNativeFunctionValue.Create(ToBeNull, 'toBeNull', 0));
  SetProperty('toBeNaN', TGocciaNativeFunctionValue.Create(ToBeNaN, 'toBeNaN', 0));
  SetProperty('toBeUndefined', TGocciaNativeFunctionValue.Create(ToBeUndefined, 'toBeUndefined', 0));
  SetProperty('toBeTruthy', TGocciaNativeFunctionValue.Create(ToBeTruthy, 'toBeTruthy', 0));
  SetProperty('toBeFalsy', TGocciaNativeFunctionValue.Create(ToBeFalsy, 'toBeFalsy', 0));
  SetProperty('toBeGreaterThan', TGocciaNativeFunctionValue.Create(ToBeGreaterThan, 'toBeGreaterThan', 1));
  SetProperty('toBeGreaterThanOrEqual', TGocciaNativeFunctionValue.Create(ToBeGreaterThanOrEqual, 'toBeGreaterThanOrEqual', 1));
  SetProperty('toBeLessThan', TGocciaNativeFunctionValue.Create(ToBeLessThan, 'toBeLessThan', 1));
  SetProperty('toBeLessThanOrEqual', TGocciaNativeFunctionValue.Create(ToBeLessThanOrEqual, 'toBeLessThanOrEqual', 1));
  SetProperty('toContain', TGocciaNativeFunctionValue.Create(ToContain, 'toContain', 1));
  SetProperty('toBeInstanceOf', TGocciaNativeFunctionValue.Create(ToBeInstanceOf, 'toBeInstanceOf', 1));
  SetProperty('toHaveLength', TGocciaNativeFunctionValue.Create(ToHaveLength, 'toHaveLength', 1));
  SetProperty('toHaveProperty', TGocciaNativeFunctionValue.Create(ToHaveProperty, 'toHaveProperty', 2));
  SetProperty('toThrow', TGocciaNativeFunctionValue.Create(ToThrow, 'toThrow', 1));

  // Negation property
  SetProperty('not', TGocciaNativeFunctionValue.Create(GetNot, 'not', 0));
end;

function TGocciaExpectationValue.ToBe(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  IsEqual: Boolean;
begin
  if Args.Count <> 1 then
  begin
    FTestAssertions.ThrowError('toBe expects exactly 1 argument', 0, 0);
    Exit;
  end;

  Expected := Args[0];
  IsEqual := (FActualValue.TypeName = Expected.TypeName) and
             (FActualValue.ToString = Expected.ToString);

  if FIsNegated then
    IsEqual := not IsEqual;

  if IsEqual then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBe');
    Result := TGocciaUndefinedValue.Create;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBe',
        'Expected ' + FActualValue.ToString + ' not to be ' + Expected.ToString)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBe',
        'Expected ' + FActualValue.ToString + ' to be ' + Expected.ToString);
    Result := TGocciaUndefinedValue.Create;
  end;
end;

function TGocciaExpectationValue.ToEqual(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  // For now, toEqual works the same as toBe
  // In a full implementation, this would do deep equality checking
  Result := ToBe(Args, ThisValue);
end;

function TGocciaExpectationValue.ToBeNull(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  IsNull: Boolean;
begin
  IsNull := FActualValue is TGocciaNullValue;

  if FIsNegated then
    IsNull := not IsNull;

  if IsNull then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeNull');
    Result := TGocciaUndefinedValue.Create;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeNull',
        'Expected ' + FActualValue.ToString + ' not to be null')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeNull',
        'Expected ' + FActualValue.ToString + ' to be null');
    Result := TGocciaUndefinedValue.Create;
  end;
end;

function TGocciaExpectationValue.ToBeNaN(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  IsNaNValue: Boolean;
begin
  IsNaNValue := Math.IsNaN(FActualValue.ToNumber);

  if FIsNegated then
    IsNaNValue := not IsNaNValue;

  if IsNaNValue then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeNaN');
    Result := TGocciaUndefinedValue.Create;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeNaN',
        'Expected ' + FActualValue.ToString + ' not to be NaN')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeNaN',
        'Expected ' + FActualValue.ToString + ' to be NaN');
    Result := TGocciaUndefinedValue.Create;
  end;
end;

function TGocciaExpectationValue.ToBeUndefined(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  IsUndefined: Boolean;
begin
  IsUndefined := FActualValue is TGocciaUndefinedValue;

  if FIsNegated then
    IsUndefined := not IsUndefined;

  if IsUndefined then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeUndefined');
    Result := TGocciaUndefinedValue.Create;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeUndefined',
        'Expected ' + FActualValue.ToString + ' not to be undefined')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeUndefined',
        'Expected ' + FActualValue.ToString + ' to be undefined');
    Result := TGocciaUndefinedValue.Create;
  end;
end;

function TGocciaExpectationValue.ToBeTruthy(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  IsTruthy: Boolean;
begin
  IsTruthy := FActualValue.ToBoolean;

  if FIsNegated then
    IsTruthy := not IsTruthy;

  if IsTruthy then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeTruthy');
    Result := TGocciaUndefinedValue.Create;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeTruthy',
        'Expected ' + FActualValue.ToString + ' not to be truthy')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeTruthy',
        'Expected ' + FActualValue.ToString + ' to be truthy');
    Result := TGocciaUndefinedValue.Create;
  end;
end;

function TGocciaExpectationValue.ToBeFalsy(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  IsFalsy: Boolean;
begin
  IsFalsy := not FActualValue.ToBoolean;

  if FIsNegated then
    IsFalsy := not IsFalsy;

  if IsFalsy then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeFalsy');
    Result := TGocciaUndefinedValue.Create;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeFalsy',
        'Expected ' + FActualValue.ToString + ' not to be falsy')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeFalsy',
        'Expected ' + FActualValue.ToString + ' to be falsy');
    Result := TGocciaUndefinedValue.Create;
  end;
end;

function TGocciaExpectationValue.ToBeGreaterThan(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  IsGreater: Boolean;
begin
  if Args.Count <> 1 then
  begin
    FTestAssertions.ThrowError('toBeGreaterThan expects exactly 1 argument', 0, 0);
    Exit;
  end;

  Expected := Args[0];
  IsGreater := FActualValue.ToNumber > Expected.ToNumber;

  if FIsNegated then
    IsGreater := not IsGreater;

  if IsGreater then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeGreaterThan');
    Result := TGocciaUndefinedValue.Create;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeGreaterThan',
        'Expected ' + FActualValue.ToString + ' not to be greater than ' + Expected.ToString)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeGreaterThan',
        'Expected ' + FActualValue.ToString + ' to be greater than ' + Expected.ToString);
    Result := TGocciaUndefinedValue.Create;
  end;
end;

function TGocciaExpectationValue.ToBeGreaterThanOrEqual(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  IsGreaterOrEqual: Boolean;
begin
  if Args.Count <> 1 then
  begin
    FTestAssertions.ThrowError('toBeGreaterThanOrEqual expects exactly 1 argument', 0, 0);
    Exit;
  end;

  Expected := Args[0];
  IsGreaterOrEqual := FActualValue.ToNumber >= Expected.ToNumber;

  if FIsNegated then
    IsGreaterOrEqual := not IsGreaterOrEqual;

  if IsGreaterOrEqual then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeGreaterThanOrEqual');
    Result := TGocciaUndefinedValue.Create;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeGreaterThanOrEqual',
        'Expected ' + FActualValue.ToString + ' not to be greater than or equal to ' + Expected.ToString)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeGreaterThanOrEqual',
        'Expected ' + FActualValue.ToString + ' to be greater than or equal to ' + Expected.ToString);
    Result := TGocciaUndefinedValue.Create;
  end;
end;

function TGocciaExpectationValue.ToBeLessThan(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  IsLess: Boolean;
begin
  if Args.Count <> 1 then
  begin
    FTestAssertions.ThrowError('toBeLessThan expects exactly 1 argument', 0, 0);
    Exit;
  end;

  Expected := Args[0];
  IsLess := FActualValue.ToNumber < Expected.ToNumber;

  if FIsNegated then
    IsLess := not IsLess;

  if IsLess then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeLessThan');
    Result := TGocciaUndefinedValue.Create;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeLessThan',
        'Expected ' + FActualValue.ToString + ' not to be less than ' + Expected.ToString)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeLessThan',
        'Expected ' + FActualValue.ToString + ' to be less than ' + Expected.ToString);
    Result := TGocciaUndefinedValue.Create;
  end;
end;

function TGocciaExpectationValue.ToBeLessThanOrEqual(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  IsLessOrEqual: Boolean;
begin
  if Args.Count <> 1 then
  begin
    FTestAssertions.ThrowError('toBeLessThanOrEqual expects exactly 1 argument', 0, 0);
    Exit;
  end;

  Expected := Args[0];
  IsLessOrEqual := FActualValue.ToNumber <= Expected.ToNumber;

  if FIsNegated then
    IsLessOrEqual := not IsLessOrEqual;

  if IsLessOrEqual then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeLessThanOrEqual');
    Result := TGocciaUndefinedValue.Create;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeLessThanOrEqual',
        'Expected ' + FActualValue.ToString + ' not to be less than or equal to ' + Expected.ToString)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeLessThanOrEqual',
        'Expected ' + FActualValue.ToString + ' to be less than or equal to ' + Expected.ToString);
    Result := TGocciaUndefinedValue.Create;
  end;
end;

function TGocciaExpectationValue.ToContain(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  Contains: Boolean;
  ActualStr, ExpectedStr: string;
begin
  if Args.Count <> 1 then
  begin
    FTestAssertions.ThrowError('toContain expects exactly 1 argument', 0, 0);
    Exit;
  end;

  Expected := Args[0];

  // For strings, check substring
  if FActualValue is TGocciaStringValue then
  begin
    ActualStr := FActualValue.ToString;
    ExpectedStr := Expected.ToString;
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
      Contains := TGocciaObjectValue(FActualValue).HasOwnProperty(Expected.ToString);
    end else
    begin
      Contains := FActualValue.ToString = Expected.ToString;
    end;
  end;

  if FIsNegated then
    Contains := not Contains;

  if Contains then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toContain');
    Result := TGocciaUndefinedValue.Create;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toContain',
        'Expected ' + FActualValue.ToString + ' not to contain ' + Expected.ToString)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toContain',
        'Expected ' + FActualValue.ToString + ' to contain ' + Expected.ToString);
    Result := TGocciaUndefinedValue.Create;
  end;
end;

function TGocciaExpectationValue.ToBeInstanceOf(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  ActualInstanceClass: TGocciaInstanceValue;
begin
  Result := TGocciaUndefinedValue.Create;

  if Args.Count <> 1 then
  begin
    FTestAssertions.ThrowError('toBeInstanceOf expects exactly 1 argument', 0, 0);
    Exit;
  end;

  // Check if the actual value is an instance of the expected class (userland only, native functions don't work)
  ActualInstanceClass := FActualValue as TGocciaInstanceValue;

  if ActualInstanceClass.ClassType = Args[0].ClassType then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeInstanceOf');
    Result := TGocciaUndefinedValue.Create;
  end
  else
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeInstanceOf',
      'Expected ' + FActualValue.ToString + ' to be an instance of ' + Args[0].ToString);
    Result := TGocciaUndefinedValue.Create;
  end;
end;

function TGocciaExpectationValue.ToHaveLength(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  HasLength: Boolean;
begin
  if Args.Count <> 1 then
  begin
    FTestAssertions.ThrowError('toHaveLength expects exactly 1 argument', 0, 0);
    Exit;
  end;

  Expected := Args[0];
  HasLength := FActualValue.ToNumber = Expected.ToNumber;

  if FIsNegated then
    HasLength := not HasLength;

  if HasLength then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toHaveLength');
    Result := TGocciaUndefinedValue.Create;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveLength',
        'Expected ' + FActualValue.ToString + ' not to have length ' + Expected.ToString)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveLength',
        'Expected ' + FActualValue.ToString + ' to have length ' + Expected.ToString);
    Result := TGocciaUndefinedValue.Create;
  end;
end;

function TGocciaExpectationValue.ToHaveProperty(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaObjectValue;
  HasProperty: Boolean;
begin
  if Args.Count <> 2 then
  begin
    FTestAssertions.ThrowError('toHaveProperty expects exactly 2 arguments', 0, 0);
    Exit;
  end;

  Expected := Args[0] as TGocciaObjectValue;
  HasProperty := Expected.HasProperty(Args[1].ToString);

  if FIsNegated then
    HasProperty := not HasProperty;

  if HasProperty then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toHaveProperty');
    Result := TGocciaUndefinedValue.Create;
  end;

  if HasProperty then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toHaveProperty');
    Result := TGocciaUndefinedValue.Create;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveProperty',
        'Expected ' + FActualValue.ToString + ' not to have property ' + Expected.ToString)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveProperty',
        'Expected ' + FActualValue.ToString + ' to have property ' + Expected.ToString);
    Result := TGocciaUndefinedValue.Create;
  end;
end;

function TGocciaExpectationValue.ToThrow(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  ExpectedErrorType: string;
  EmptyArgs: TObjectList<TGocciaValue>;
  TestContext: TGocciaEvaluationContext;
  TestFunc: TGocciaFunctionValue;
  TestScope: TGocciaScope;
  I: Integer;
begin
  Result := TGocciaUndefinedValue.Create;

  ExpectedErrorType := '';
  if Args.Count > 0 then
  begin
    // If args are provided, expect a specific error type
    ExpectedErrorType := Args[0].ToString;
  end;

  if not (FActualValue is TGocciaFunctionValue) then
  begin
    FTestAssertions.ThrowError('toThrow expects actual value to be a function', 0, 0);
    Exit;
  end;

    TestFunc := TGocciaFunctionValue(FActualValue);
  EmptyArgs := TObjectList<TGocciaValue>.Create(False);

  // Create a strict test scope that throws for undefined variables
  TestScope := TGocciaStrictScope.Create(TestFunc.Closure, 'ToThrow Test');
  try
    // Set up evaluation context with an OnError handler that throws exceptions
    TestContext.Scope := TestScope;
    TestContext.OnError := FTestAssertions.ThrowError;  // Use the test framework's ThrowError
    TestContext.LoadModule := nil;

    // Set the global context so nested function calls inherit proper error handling
    GlobalEvaluationContext.OnError := TestContext.OnError;
    GlobalEvaluationContext.LoadModule := TestContext.LoadModule;

    try
      // Execute all function body statements with proper context
      for I := 0 to TestFunc.BodyStatements.Count - 1 do
      begin
        Evaluate(TestFunc.BodyStatements[I], TestContext);
      end;
    except
      on E: Exception do
      begin
        // If no specific error type expected, any exception is fine
        if ExpectedErrorType = '' then
        begin
          TGocciaTestAssertions(FTestAssertions).AssertionPassed('toThrow');
          Exit;
        end;

        // Check if the exception message contains the expected error type
        // Look for the error type name in the message (case insensitive)
        if (Pos(LowerCase(ExpectedErrorType), LowerCase(E.Message)) > 0) then
        begin
          TGocciaTestAssertions(FTestAssertions).AssertionPassed('toThrow');
          Exit;
        end
        else
        begin
          TGocciaTestAssertions(FTestAssertions).AssertionFailed('toThrow',
            'Expected ' + FActualValue.ToString + ' to throw ' + ExpectedErrorType + ' but threw: ' + E.Message);
          Exit;
        end;
      end;
    end;
  finally
    EmptyArgs.Free;
    // Don't free TestScope here as it might be referenced
  end;

  TGocciaTestAssertions(FTestAssertions).AssertionFailed('toThrow',
    'Expected ' + FActualValue.ToString + ' to throw an exception');
end;


function TGocciaExpectationValue.GetNot(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  // Return a new expectation object with negation enabled
  Result := TGocciaExpectationValue.Create(FActualValue, FTestAssertions, True);
end;

{ TGocciaTestAssertions }

constructor TGocciaTestAssertions.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
var
  TestFunction: TGocciaNativeFunctionValue;
begin
  inherited Create(AName, AScope, AThrowError);

  FRegisteredSuites := TObjectList<TTestSuite>.Create;
  FRegisteredTests := TObjectList<TTestCase>.Create;
  FBeforeEachCallbacks := TObjectList<TGocciaValue>.Create(False);
  FAfterEachCallbacks := TObjectList<TGocciaValue>.Create(False);

  ResetTestStats;

  // TODO: Remove duplication

  // Register testing functions globally for easy access
  AScope.SetValue('expect', TGocciaNativeFunctionValue.Create(Expect, 'expect', 1));
  AScope.SetValue('describe', TGocciaNativeFunctionValue.Create(Describe, 'describe', 2));

  // Create test function with skip property
  TestFunction := TGocciaNativeFunctionValue.Create(Test, 'test', 2);
  TestFunction.SetProperty('skip', TGocciaNativeFunctionValue.Create(Skip, 'skip', 2));
  AScope.SetValue('test', TestFunction);

  AScope.SetValue('it', TGocciaNativeFunctionValue.Create(It, 'it', 2));
  AScope.SetValue('beforeEach', TGocciaNativeFunctionValue.Create(BeforeEach, 'beforeEach', 1));
  AScope.SetValue('afterEach', TGocciaNativeFunctionValue.Create(AfterEach, 'afterEach', 1));
  AScope.SetValue('runTests', TGocciaNativeFunctionValue.Create(RunTests, 'runTests', 0));

  // Also set them in the builtin object for completeness
  FBuiltinObject.SetProperty('expect', TGocciaNativeFunctionValue.Create(Expect, 'expect', 1));
  FBuiltinObject.SetProperty('describe', TGocciaNativeFunctionValue.Create(Describe, 'describe', 2));
  FBuiltinObject.SetProperty('test', TestFunction);  // Use the same test function with skip property
  FBuiltinObject.SetProperty('it', TGocciaNativeFunctionValue.Create(It, 'it', 2));
  FBuiltinObject.SetProperty('beforeEach', TGocciaNativeFunctionValue.Create(BeforeEach, 'beforeEach', 1));
  FBuiltinObject.SetProperty('afterEach', TGocciaNativeFunctionValue.Create(AfterEach, 'afterEach', 1));
  FBuiltinObject.SetProperty('runTests', TGocciaNativeFunctionValue.Create(RunTests, 'runTests', 0));
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

procedure TGocciaTestAssertions.RunCallbacks(Callbacks: TObjectList<TGocciaValue>);
var
  I: Integer;
  Callback: TGocciaValue;
  EmptyArgs: TObjectList<TGocciaValue>;
begin
  EmptyArgs := TObjectList<TGocciaValue>.Create(False);
  try
    for I := 0 to Callbacks.Count - 1 do
    begin
      Callback := Callbacks[I];
      if Callback is TGocciaFunctionValue then
      begin
        try
          TGocciaFunctionValue(Callback).Call(EmptyArgs, TGocciaUndefinedValue.Create);
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

function TGocciaTestAssertions.Expect(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  if Args.Count <> 1 then
    ThrowError('expect expects exactly 1 argument', 0, 0);

  Result := TGocciaExpectationValue.Create(Args[0], Self);
end;

function TGocciaTestAssertions.Describe(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  SuiteName: string;
  SuiteFunction: TGocciaFunctionValue;
  Suite: TTestSuite;
begin
  if Args.Count <> 2 then
    ThrowError('describe expects exactly 2 arguments', 0, 0);

  if not (Args[0] is TGocciaStringValue) then
    ThrowError('describe expects first argument to be a string', 0, 0);

  if not (Args[1] is TGocciaFunctionValue) then
    ThrowError('describe expects second argument to be a function', 0, 0);

  SuiteName := Args[0].ToString;
  SuiteFunction := Args[1] as TGocciaFunctionValue;

  // Just register the test suite - do NOT execute it until runTests() is called
  Suite := TTestSuite.Create(SuiteName, SuiteFunction);
  FRegisteredSuites.Add(Suite);

  Result := TGocciaUndefinedValue.Create;
end;

function TGocciaTestAssertions.Test(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  TestName: string;
  TestFunction: TGocciaFunctionValue;
  TestCase: TTestCase;
begin
  if Args.Count <> 2 then
    ThrowError('test expects exactly 2 arguments', 0, 0);

  if not (Args[0] is TGocciaStringValue) then
    ThrowError('test expects first argument to be a string', 0, 0);

  if not (Args[1] is TGocciaFunctionValue) then
    ThrowError('test expects second argument to be a function', 0, 0);

  TestName := Args[0].ToString;
  TestFunction := Args[1] as TGocciaFunctionValue;

  // Register the test with the current suite name
  TestCase := TTestCase.Create(TestName, TestFunction, FTestStats.CurrentSuiteName);
  FRegisteredTests.Add(TestCase);

  Result := TGocciaUndefinedValue.Create;
end;

function TGocciaTestAssertions.It(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  // 'it' is an alias for 'test'
  Result := Test(Args, ThisValue);
end;

function TGocciaTestAssertions.Skip(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  TestName: string;
  TestFunction: TGocciaFunctionValue;
  TestCase: TTestCase;
begin
  if Args.Count <> 2 then
    ThrowError('test.skip expects exactly 2 arguments', 0, 0);

  if not (Args[0] is TGocciaStringValue) then
    ThrowError('test.skip expects first argument to be a string', 0, 0);

  if not (Args[1] is TGocciaFunctionValue) then
    ThrowError('test.skip expects second argument to be a function', 0, 0);

  TestName := Args[0].ToString;
  TestFunction := Args[1] as TGocciaFunctionValue;

  // Register the test as skipped with the current suite name
  TestCase := TTestCase.Create(TestName, TestFunction, FTestStats.CurrentSuiteName, True);
  FRegisteredTests.Add(TestCase);

  Result := TGocciaUndefinedValue.Create;
end;

function TGocciaTestAssertions.BeforeEach(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  if Args.Count <> 1 then
    ThrowError('beforeEach expects exactly 1 argument', 0, 0);

  if not (Args[0] is TGocciaFunctionValue) then
    ThrowError('beforeEach expects a function argument', 0, 0);

  FBeforeEachCallbacks.Add(Args[0]);

  Result := TGocciaUndefinedValue.Create;
end;

function TGocciaTestAssertions.AfterEach(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  if Args.Count <> 1 then
    ThrowError('afterEach expects exactly 1 argument', 0, 0);

  if not (Args[0] is TGocciaFunctionValue) then
    ThrowError('afterEach expects a function argument', 0, 0);

  FAfterEachCallbacks.Add(Args[0]);

  Result := TGocciaUndefinedValue.Create;
end;

function TGocciaTestAssertions.RunTests(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  StartTime: Int64;
  Suite: TTestSuite;
  TestCase: TTestCase;
  EmptyArgs: TObjectList<TGocciaValue>;
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
begin
  // Reset test statistics and clear any previously registered tests from describe blocks
  ResetTestStats;

  StartTime := GetTickCount64;

  // Clear tests that were registered from previous describe executions
  // Keep standalone tests that were registered during script execution
  for I := FRegisteredTests.Count - 1 downto 0 do
  begin
    if FRegisteredTests[I].SuiteName <> '' then
      FRegisteredTests.Delete(I);
  end;

  if Args.Count > 0 then
  begin
    if Args[0] is TGocciaObjectValue then
    begin
      TestParams := Args[0] as TGocciaObjectValue;
      if TestParams.HasProperty('exitOnFirstFailure') then
        ExitOnFirstFailure := TestParams.GetProperty('exitOnFirstFailure').ToBoolean
      else
        ExitOnFirstFailure := False;
      if TestParams.HasProperty('showTestResults') then
        ShowTestResults := TestParams.GetProperty('showTestResults').ToBoolean
      else
        ShowTestResults := True;
      if TestParams.HasProperty('silent') then
        Silent := TestParams.GetProperty('silent').ToBoolean
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
  EmptyArgs := TObjectList<TGocciaValue>.Create(False);
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
        Suite.SuiteFunction.Call(EmptyArgs, TGocciaUndefinedValue.Create);
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
          // For tests from describe blocks, clone with global scope to fix context issues
          if TestCase.SuiteName <> '' then
          begin
            // Test is from a describe block - clone function with global scope
            ClonedFunction := TestCase.TestFunction.CloneWithNewScope(FScope);
            ClonedFunction.Call(EmptyArgs, TGocciaUndefinedValue.Create);
          end
          else
          begin
            // Standalone test - call directly
            TestCase.TestFunction.Call(EmptyArgs, TGocciaUndefinedValue.Create);
          end;
        except
          on E: Exception do
          begin
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
      FailedTestDetailsArray.Properties.Add(TPair<string, TGocciaValue>.Create(IntToStr(I), TGocciaStringValue.Create(FailedTestDetails[I])));
  end;

   // Create result object
  ResultObj := TGocciaObjectValue.Create;
  ResultObj.SetProperty('totalTests', TGocciaNumberValue.Create(FRegisteredTests.Count));
  ResultObj.SetProperty('totalRunTests', TGocciaNumberValue.Create(FTestStats.TotalTests));
  ResultObj.SetProperty('passed', TGocciaNumberValue.Create(FTestStats.PassedTests));
  ResultObj.SetProperty('failed', TGocciaNumberValue.Create(FTestStats.FailedTests));
  ResultObj.SetProperty('skipped', TGocciaNumberValue.Create(FTestStats.SkippedTests));
  ResultObj.SetProperty('assertions', TGocciaNumberValue.Create(FTestStats.TotalAssertionCount));
  ResultObj.SetProperty('duration', TGocciaNumberValue.Create(GetTickCount64 - StartTime));
  ResultObj.SetProperty('failedTests', FailedTestDetailsArray);
  ResultObj.SetProperty('summary', TGocciaStringValue.Create(Summary));

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
