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
  Classes;

type
  TGocciaTestAssertions = class;

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
    function ToBeUndefined(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeTruthy(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeFalsy(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeGreaterThan(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ToBeLessThan(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ToContain(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;

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
      CurrentSuiteName: string;
    end;

    FBeforeEachCallbacks: TObjectList<TGocciaValue>;
    FAfterEachCallbacks: TObjectList<TGocciaValue>;

    procedure RunCallbacks(Callbacks: TObjectList<TGocciaValue>);
    procedure AssertionPassed(const TestName: string);
    procedure AssertionFailed(const TestName, Message: string);
  public
    constructor Create(const AName: string; AScope: TGocciaScope; AThrowError: TGocciaThrowError);
    destructor Destroy; override;

    // Main expect function
    function Expect(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;

    // Test suite functions
    function Describe(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function Test(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function It(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;

    // Setup/teardown
    function BeforeEach(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function AfterEach(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;

    // Results
    function GetTestResults(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

{ TGocciaExpectationValue }

constructor TGocciaExpectationValue.Create(ActualValue: TGocciaValue; TestAssertions: TObject; IsNegated: Boolean);
begin
  inherited Create;
  FActualValue := ActualValue;
  FTestAssertions := TestAssertions;
  FIsNegated := IsNegated;

  // Add matcher methods
  SetProperty('toBe', TGocciaNativeFunctionValue.Create(ToBe, 'toBe', 1));
  SetProperty('toEqual', TGocciaNativeFunctionValue.Create(ToEqual, 'toEqual', 1));
  SetProperty('toBeNull', TGocciaNativeFunctionValue.Create(ToBeNull, 'toBeNull', 0));
  SetProperty('toBeUndefined', TGocciaNativeFunctionValue.Create(ToBeUndefined, 'toBeUndefined', 0));
  SetProperty('toBeTruthy', TGocciaNativeFunctionValue.Create(ToBeTruthy, 'toBeTruthy', 0));
  SetProperty('toBeFalsy', TGocciaNativeFunctionValue.Create(ToBeFalsy, 'toBeFalsy', 0));
  SetProperty('toBeGreaterThan', TGocciaNativeFunctionValue.Create(ToBeGreaterThan, 'toBeGreaterThan', 1));
  SetProperty('toBeLessThan', TGocciaNativeFunctionValue.Create(ToBeLessThan, 'toBeLessThan', 1));
  SetProperty('toContain', TGocciaNativeFunctionValue.Create(ToContain, 'toContain', 1));

  // Negation property
  SetProperty('not', TGocciaNativeFunctionValue.Create(GetNot, 'not', 0));
end;

function TGocciaExpectationValue.ToBe(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  IsEqual: Boolean;
begin
  if Args.Count <> 1 then
    raise TGocciaRuntimeError.Create('toBe expects exactly 1 argument');

  Expected := Args[0];
  IsEqual := (FActualValue.TypeName = Expected.TypeName) and
             (FActualValue.ToString = Expected.ToString);

  if FIsNegated then
    IsEqual := not IsEqual;

  if IsEqual then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBe');
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBe',
        'Expected ' + FActualValue.ToString + ' not to be ' + Expected.ToString)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBe',
        'Expected ' + FActualValue.ToString + ' to be ' + Expected.ToString);
  end;

  Result := TGocciaUndefinedValue.Create;
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
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeNull',
        'Expected ' + FActualValue.ToString + ' not to be null')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeNull',
        'Expected ' + FActualValue.ToString + ' to be null');
  end;

  Result := TGocciaUndefinedValue.Create;
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
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeUndefined',
        'Expected ' + FActualValue.ToString + ' not to be undefined')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeUndefined',
        'Expected ' + FActualValue.ToString + ' to be undefined');
  end;

  Result := TGocciaUndefinedValue.Create;
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
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeTruthy',
        'Expected ' + FActualValue.ToString + ' not to be truthy')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeTruthy',
        'Expected ' + FActualValue.ToString + ' to be truthy');
  end;

  Result := TGocciaUndefinedValue.Create;
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
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeFalsy',
        'Expected ' + FActualValue.ToString + ' not to be falsy')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeFalsy',
        'Expected ' + FActualValue.ToString + ' to be falsy');
  end;

  Result := TGocciaUndefinedValue.Create;
end;

function TGocciaExpectationValue.ToBeGreaterThan(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  IsGreater: Boolean;
begin
  if Args.Count <> 1 then
    raise TGocciaRuntimeError.Create('toBeGreaterThan expects exactly 1 argument');

  Expected := Args[0];
  IsGreater := FActualValue.ToNumber > Expected.ToNumber;

  if FIsNegated then
    IsGreater := not IsGreater;

  if IsGreater then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeGreaterThan');
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeGreaterThan',
        'Expected ' + FActualValue.ToString + ' not to be greater than ' + Expected.ToString)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeGreaterThan',
        'Expected ' + FActualValue.ToString + ' to be greater than ' + Expected.ToString);
  end;

  Result := TGocciaUndefinedValue.Create;
end;

function TGocciaExpectationValue.ToBeLessThan(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  IsLess: Boolean;
begin
  if Args.Count <> 1 then
    raise TGocciaRuntimeError.Create('toBeLessThan expects exactly 1 argument');

  Expected := Args[0];
  IsLess := FActualValue.ToNumber < Expected.ToNumber;

  if FIsNegated then
    IsLess := not IsLess;

  if IsLess then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeLessThan');
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeLessThan',
        'Expected ' + FActualValue.ToString + ' not to be less than ' + Expected.ToString)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeLessThan',
        'Expected ' + FActualValue.ToString + ' to be less than ' + Expected.ToString);
  end;

  Result := TGocciaUndefinedValue.Create;
end;

function TGocciaExpectationValue.ToContain(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  Contains: Boolean;
  ActualStr, ExpectedStr: string;
begin
  if Args.Count <> 1 then
    raise TGocciaRuntimeError.Create('toContain expects exactly 1 argument');

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
    // For other types, just check equality for now
    Contains := FActualValue.ToString = Expected.ToString;
  end;

  if FIsNegated then
    Contains := not Contains;

  if Contains then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toContain');
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toContain',
        'Expected ' + FActualValue.ToString + ' not to contain ' + Expected.ToString)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toContain',
        'Expected ' + FActualValue.ToString + ' to contain ' + Expected.ToString);
  end;

  Result := TGocciaUndefinedValue.Create;
end;

function TGocciaExpectationValue.GetNot(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  // Return a new expectation object with negation enabled
  Result := TGocciaExpectationValue.Create(FActualValue, FTestAssertions, True);
end;

{ TGocciaTestAssertions }

constructor TGocciaTestAssertions.Create(const AName: string; AScope: TGocciaScope; AThrowError: TGocciaThrowError);
begin
  inherited Create(AName, AScope, AThrowError);

  FBeforeEachCallbacks := TObjectList<TGocciaValue>.Create(False);
  FAfterEachCallbacks := TObjectList<TGocciaValue>.Create(False);

  // Initialize test stats
  FTestStats.TotalTests := 0;
  FTestStats.PassedTests := 0;
  FTestStats.FailedTests := 0;
  FTestStats.CurrentSuiteName := '';

  // Core testing functions
  FBuiltinObject.SetProperty('expect', TGocciaNativeFunctionValue.Create(Expect, 'expect', 1));
  FBuiltinObject.SetProperty('describe', TGocciaNativeFunctionValue.Create(Describe, 'describe', 2));
  FBuiltinObject.SetProperty('test', TGocciaNativeFunctionValue.Create(Test, 'test', 2));
  FBuiltinObject.SetProperty('it', TGocciaNativeFunctionValue.Create(It, 'it', 2));
  FBuiltinObject.SetProperty('beforeEach', TGocciaNativeFunctionValue.Create(BeforeEach, 'beforeEach', 1));
  FBuiltinObject.SetProperty('afterEach', TGocciaNativeFunctionValue.Create(AfterEach, 'afterEach', 1));
  FBuiltinObject.SetProperty('getTestResults', TGocciaNativeFunctionValue.Create(GetTestResults, 'getTestResults', 0));
end;

destructor TGocciaTestAssertions.Destroy;
begin
  FBeforeEachCallbacks.Free;
  FAfterEachCallbacks.Free;
  inherited;
end;

procedure TGocciaTestAssertions.RunCallbacks(Callbacks: TObjectList<TGocciaValue>);
var
  I: Integer;
  Callback: TGocciaValue;
begin
  for I := 0 to Callbacks.Count - 1 do
  begin
    Callback := Callbacks[I];
    if Callback is TGocciaFunctionValue then
    begin
      // Execute the callback function
      // Note: In a full implementation, this would need proper function invocation
    end;
  end;
end;

procedure TGocciaTestAssertions.AssertionPassed(const TestName: string);
begin
  Inc(FTestStats.PassedTests);
  // Could log success here
end;

procedure TGocciaTestAssertions.AssertionFailed(const TestName, Message: string);
begin
  Inc(FTestStats.FailedTests);
  ThrowError('Test failed: ' + Message, 0, 0);
end;

function TGocciaTestAssertions.Expect(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  if Args.Count <> 1 then
    ThrowError('expect expects exactly 1 argument', 0, 0);

  Result := TGocciaExpectationValue.Create(Args[0], Self);
end;

function TGocciaTestAssertions.Describe(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  if Args.Count <> 2 then
    ThrowError('describe expects exactly 2 arguments', 0, 0);

  if not (Args[0] is TGocciaStringValue) then
    ThrowError('describe expects first argument to be a string', 0, 0);

  if not (Args[1] is TGocciaFunctionValue) then
    ThrowError('describe expects second argument to be a function', 0, 0);

  FTestStats.CurrentSuiteName := Args[0].ToString;

  // In a full implementation, this would execute the function with proper scope
  // For now, we'll return undefined
  Result := TGocciaUndefinedValue.Create;
end;

function TGocciaTestAssertions.Test(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  if Args.Count <> 2 then
    ThrowError('test expects exactly 2 arguments', 0, 0);

  if not (Args[0] is TGocciaStringValue) then
    ThrowError('test expects first argument to be a string', 0, 0);

  if not (Args[1] is TGocciaFunctionValue) then
    ThrowError('test expects second argument to be a function', 0, 0);

  Inc(FTestStats.TotalTests);

  // Run beforeEach callbacks
  RunCallbacks(FBeforeEachCallbacks);

  // In a full implementation, this would execute the test function
  // and catch any assertion failures

  // Run afterEach callbacks
  RunCallbacks(FAfterEachCallbacks);

  Result := TGocciaUndefinedValue.Create;
end;

function TGocciaTestAssertions.It(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  // 'it' is an alias for 'test'
  Result := Test(Args, ThisValue);
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

function TGocciaTestAssertions.GetTestResults(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  ResultObj: TGocciaObjectValue;
begin
  ResultObj := TGocciaObjectValue.Create;
  ResultObj.SetProperty('total', TGocciaNumberValue.Create(FTestStats.TotalTests));
  ResultObj.SetProperty('passed', TGocciaNumberValue.Create(FTestStats.PassedTests));
  ResultObj.SetProperty('failed', TGocciaNumberValue.Create(FTestStats.FailedTests));
  ResultObj.SetProperty('suiteName', TGocciaStringValue.Create(FTestStats.CurrentSuiteName));

  Result := ResultObj;
end;

end.