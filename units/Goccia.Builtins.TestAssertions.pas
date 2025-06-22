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

    // Helper function for deep equality checking
    function IsDeepEqual(const Actual, Expected: TGocciaValue): Boolean;
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
  SysUtils, Goccia.Values.ClassValue, Goccia.Evaluator, Goccia.Evaluator.Comparison, Goccia.Values.ObjectPropertyDescriptor;

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

  // Negation property - use accessor to make it a getter
  DefineProperty('not', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.Create(GetNot, 'not', 0), nil, [pfConfigurable]));
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
  IsEqual := IsStrictEqual(FActualValue, Expected);

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
var
  Expected: TGocciaValue;
  IsEqual: Boolean;
begin
  if Args.Count <> 1 then
  begin
    FTestAssertions.ThrowError('toEqual expects exactly 1 argument', 0, 0);
    Exit;
  end;

  Expected := Args[0];
  IsEqual := IsDeepEqual(FActualValue, Expected);

  if FIsNegated then
    IsEqual := not IsEqual;

  if IsEqual then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toEqual');
    Result := TGocciaUndefinedValue.Create;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toEqual',
        'Expected ' + FActualValue.ToString + ' not to equal ' + Expected.ToString)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toEqual',
        'Expected ' + FActualValue.ToString + ' to equal ' + Expected.ToString);
    Result := TGocciaUndefinedValue.Create;
  end;
end;

function TGocciaExpectationValue.IsDeepEqual(const Actual, Expected: TGocciaValue): Boolean;
var
  ActualObj, ExpectedObj: TGocciaObjectValue;
  ActualArr, ExpectedArr: TGocciaArrayValue;
  ActualKeys, ExpectedKeys: TArray<string>;
  I: Integer;
  Key: string;
begin
  // Base case: strict equality (handles primitives and same object references)
  if IsStrictEqual(Actual, Expected) then
  begin
    Result := True;
    Exit;
  end;

  // Type mismatch
  if Actual.TypeName <> Expected.TypeName then
  begin
    Result := False;
    Exit;
  end;

  // Handle arrays
  if (Actual is TGocciaArrayValue) and (Expected is TGocciaArrayValue) then
  begin
    ActualArr := TGocciaArrayValue(Actual);
    ExpectedArr := TGocciaArrayValue(Expected);

    // Check lengths
    if ActualArr.Elements.Count <> ExpectedArr.Elements.Count then
    begin
      Result := False;
      Exit;
    end;

    // Recursively compare elements
    for I := 0 to ActualArr.Elements.Count - 1 do
    begin
      if not IsDeepEqual(ActualArr.Elements[I], ExpectedArr.Elements[I]) then
      begin
        Result := False;
        Exit;
      end;
    end;

    Result := True;
    Exit;
  end;

  // Handle objects (but not arrays which inherit from TGocciaObjectValue)
  if (Actual is TGocciaObjectValue) and (Expected is TGocciaObjectValue) and
     not (Actual is TGocciaArrayValue) and not (Expected is TGocciaArrayValue) then
  begin
    ActualObj := TGocciaObjectValue(Actual);
    ExpectedObj := TGocciaObjectValue(Expected);

    // Get enumerable property names from both objects
    ActualKeys := ActualObj.GetEnumerablePropertyNames;
    ExpectedKeys := ExpectedObj.GetEnumerablePropertyNames;

    // Check if they have the same number of properties
    if Length(ActualKeys) <> Length(ExpectedKeys) then
    begin
      Result := False;
      Exit;
    end;

    // Check if all keys exist in both objects and values are deeply equal
    for I := 0 to High(ActualKeys) do
    begin
      Key := ActualKeys[I];

      // Check if expected object has this key
      if not ExpectedObj.HasOwnProperty(Key) then
      begin
        Result := False;
        Exit;
      end;

      // Recursively compare property values
      if not IsDeepEqual(ActualObj.GetProperty(Key), ExpectedObj.GetProperty(Key)) then
      begin
        Result := False;
        Exit;
      end;
    end;

    Result := True;
    Exit;
  end;

  // For other types (functions, etc.), fall back to strict equality
  Result := False;
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
          TestScope := TGocciaScope.Create(TestFunc.Closure, skFunction, 'ToThrow Test');
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
        // Guard against invalid AST nodes from syntax errors
        if TestFunc.BodyStatements[I] <> nil then
          Evaluate(TestFunc.BodyStatements[I], TestContext)
        else
          raise TGocciaSyntaxError.Create('Invalid syntax in function body', 0, 0, '', nil);
      end;
    except
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
            'Expected ' + FActualValue.ToString + ' to throw ' + ExpectedErrorType + ' but threw: ' + E.Message);
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
            'Expected ' + FActualValue.ToString + ' to throw ' + ExpectedErrorType + ' but threw: ' + E.ClassName + ': ' + E.Message);
          Exit;
        end;
      end;
    end;
  finally
    EmptyArgs.Free;
    TestScope.Free;  // Properly free the test scope to prevent access violations
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
          // Create a fresh child scope for each test to ensure isolation
          ClonedFunction := TestCase.TestFunction.CloneWithNewScope(FScope.CreateChild(skFunction, 'TestFunction'));
          ClonedFunction.Call(EmptyArgs, TGocciaUndefinedValue.Create);
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
      FailedTestDetailsArray.Elements.Add(TGocciaStringValue.Create(FailedTestDetails[I]));
  end;

   // Create result object
  ResultObj := TGocciaObjectValue.Create;
  ResultObj.AssignProperty('totalTests', TGocciaNumberValue.Create(FRegisteredTests.Count));
  ResultObj.AssignProperty('totalRunTests', TGocciaNumberValue.Create(FTestStats.TotalTests));
  ResultObj.AssignProperty('passed', TGocciaNumberValue.Create(FTestStats.PassedTests));
  ResultObj.AssignProperty('failed', TGocciaNumberValue.Create(FTestStats.FailedTests));
  ResultObj.AssignProperty('skipped', TGocciaNumberValue.Create(FTestStats.SkippedTests));
  ResultObj.AssignProperty('assertions', TGocciaNumberValue.Create(FTestStats.TotalAssertionCount));
  ResultObj.AssignProperty('duration', TGocciaNumberValue.Create(GetTickCount64 - StartTime));
  ResultObj.AssignProperty('failedTests', FailedTestDetailsArray);
  ResultObj.AssignProperty('summary', TGocciaStringValue.Create(Summary));

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
