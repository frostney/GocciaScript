unit Goccia.Builtins.TestAssertions;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.Arguments.Validator,
  Goccia.Builtins.Base,
  Goccia.Error,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.ArrayValue,
  Goccia.Values.FunctionBase,
  Goccia.Values.MockFunction,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaTestAssertions = class;
  TGocciaRegisteredEntry = class;
  TGocciaTestSuite = class;
  TGocciaTestCase = class;
  TGocciaParameterizedRegistrationFunction = class;

  TGocciaTestHookPhase = (thBeforeAll, thBeforeEach, thAfterEach, thAfterAll);
  TGocciaParameterizedRegistrationTarget = (prtDescribe, prtTest);

  TGocciaRegisteredEntryList = TObjectList<TGocciaRegisteredEntry>;

  TGocciaRegisteredEntry = class
  public
    ParentSuite: TGocciaTestSuite;
    IsSkipped: Boolean;
    IsFocused: Boolean;
    constructor Create(const AParentSuite: TGocciaTestSuite; const AIsSkipped: Boolean = False;
      const AIsFocused: Boolean = False);
    function DisplayName: string; virtual; abstract;
    function GetFullName: string; virtual; abstract;
  end;

  TGocciaTestSuite = class(TGocciaRegisteredEntry)
  public
    Name: string;
    SuiteFunction: TGocciaFunctionBase;
    SuiteArguments: TGocciaArgumentsCollection;
    Entries: TGocciaRegisteredEntryList;
    BeforeAllCallbacks: TGocciaArgumentsCollection;
    BeforeEachCallbacks: TGocciaArgumentsCollection;
    AfterEachCallbacks: TGocciaArgumentsCollection;
    AfterAllCallbacks: TGocciaArgumentsCollection;
    constructor Create(const AParentSuite: TGocciaTestSuite; const AName: string;
      const ASuiteFunction: TGocciaFunctionBase; const AArguments: TGocciaArgumentsCollection;
      const AIsSkipped: Boolean = False; const AIsFocused: Boolean = False);
    destructor Destroy; override;
    procedure AddEntry(const AEntry: TGocciaRegisteredEntry);
    procedure AddHook(const ACallback: TGocciaFunctionBase; const APhase: TGocciaTestHookPhase);
    procedure ClearRegisteredContent;
    function DisplayName: string; override;
    function GetFullName: string; override;
  end;

  TGocciaTestCase = class(TGocciaRegisteredEntry)
  public
    Name: string;
    TestFunction: TGocciaFunctionBase;
    TestArguments: TGocciaArgumentsCollection;
    IsTodo: Boolean;
    constructor Create(const AParentSuite: TGocciaTestSuite; const AName: string;
      const ATestFunction: TGocciaFunctionBase; const AArguments: TGocciaArgumentsCollection;
      const AIsSkipped: Boolean = False; const AIsFocused: Boolean = False;
      const AIsTodo: Boolean = False);
    destructor Destroy; override;
    function DisplayName: string; override;
    function GetFullName: string; override;
  end;

  TGocciaParameterizedRegistrationFunction = class(TGocciaFunctionBase)
  private
    FTestAssertions: TGocciaTestAssertions;
    FTable: TGocciaValue;
    FTarget: TGocciaParameterizedRegistrationTarget;
  protected
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
  public
    constructor Create(const ATestAssertions: TGocciaTestAssertions;
      const ATable: TGocciaValue; const ATarget: TGocciaParameterizedRegistrationTarget);
    function Call(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; override;
    procedure MarkReferences; override;
  end;

  // Expectation object that provides matchers
  TGocciaExpectationValue = class(TGocciaObjectValue)
  private
    FActualValue: TGocciaValue;
    FIsNegated: Boolean;
    FTestAssertions: TGocciaTestAssertions; // Reference to parent
  public
    constructor Create(const AActualValue: TGocciaValue; const ATestAssertions: TGocciaTestAssertions; const AIsNegated: Boolean = False);

    // Core matchers
    function ToBe(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToEqual(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToContainEqual(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToStrictEqual(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToMatchObject(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToMatch(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToBeNull(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToBeNaN(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToBeUndefined(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToBeDefined(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToBeTruthy(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToBeFalsy(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToBeGreaterThan(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToBeGreaterThanOrEqual(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToBeLessThan(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToBeLessThanOrEqual(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToContain(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToBeInstanceOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToHaveLength(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToHaveProperty(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToThrow(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToBeCloseTo(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    // Mock matchers
    function ToHaveBeenCalled(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToHaveBeenCalledTimes(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToHaveBeenCalledWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToHaveBeenLastCalledWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToHaveBeenNthCalledWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToHaveReturned(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToHaveReturnedTimes(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToHaveReturnedWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToHaveLastReturnedWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToHaveNthReturnedWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    // Negation support
    function GetNot(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    // Promise unwrapping (Vitest/Jest-compatible)
    function GetResolves(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetRejects(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
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

    FRootSuite: TGocciaTestSuite;
    FCurrentRegistrationSuite: TGocciaTestSuite;
    FSkipNextDescribe: Boolean;
    FFocusNextDescribe: Boolean;
    FSkipNextTest: Boolean;
    FFocusNextTest: Boolean;
    FSuppressOutput: Boolean;
    FOnTestFinishedCallbacks: TGocciaArgumentsCollection;

    procedure ConfigureDescribeFunction(const AFunction: TGocciaNativeFunctionValue);
    procedure ConfigureTestFunction(const AFunction: TGocciaNativeFunctionValue);
    function GetCurrentRegistrationSuite: TGocciaTestSuite;
    procedure RegisterDescribeEntry(const AName: string; const ASuiteFunction: TGocciaFunctionBase;
      const AArguments: TGocciaArgumentsCollection; const AIsSkipped: Boolean = False;
      const AIsFocused: Boolean = False);
    procedure RegisterTestEntry(const AName: string; const ATestFunction: TGocciaFunctionBase;
      const AArguments: TGocciaArgumentsCollection; const AIsSkipped: Boolean = False;
      const AIsFocused: Boolean = False; const AIsTodo: Boolean = False);
    function ValidateDescribeRegistration(const AArgs: TGocciaArgumentsCollection;
      const AFunctionName: string; out ASuiteName: string;
      out ASuiteFunction: TGocciaFunctionBase): Boolean;
    function ValidateTestRegistration(const AArgs: TGocciaArgumentsCollection;
      const AFunctionName: string; out ATestName: string;
      out ATestFunction: TGocciaFunctionBase): Boolean;
    procedure RegisterHook(const AArgs: TGocciaArgumentsCollection; const AHookName: string;
      const APhase: TGocciaTestHookPhase);
    function GetEachRowArguments(const ARow: TGocciaValue): TGocciaArgumentsCollection;
    function FormatEachName(const ATemplate: string;
      const AArguments: TGocciaArgumentsCollection; const ARowIndex: Integer): string;
    procedure ClearNestedRegistrations(const ASuite: TGocciaTestSuite);
    procedure BuildNestedRegistrations(const ASuite: TGocciaTestSuite;
      const AFailedTestDetails: TStringList);
    procedure CollectBeforeEachCallbacks(const ASuite: TGocciaTestSuite;
      const ACallbacks: TGocciaArgumentsCollection);
    procedure CollectAfterEachCallbacks(const ASuite: TGocciaTestSuite;
      const ACallbacks: TGocciaArgumentsCollection);
    function IsSuiteSkipped(const ASuite: TGocciaTestSuite): Boolean;
    function IsSuiteFocusedInHierarchy(const ASuite: TGocciaTestSuite): Boolean;
    function IsTestSelected(const ATestCase: TGocciaTestCase;
      const AHasFocusedEntries: Boolean): Boolean;
    function SuiteHasSelectedEntries(const ASuite: TGocciaTestSuite;
      const AHasFocusedEntries: Boolean): Boolean;
    function SuiteHasRunnableEntries(const ASuite: TGocciaTestSuite;
      const AHasFocusedEntries: Boolean): Boolean;
    procedure ExecuteSuite(const ASuite: TGocciaTestSuite;
      const AHasFocusedEntries, AExitOnFirstFailure: Boolean;
      const AFailedTestDetails: TStringList; var AShouldStop: Boolean);
    function CountRegisteredTests(const ASuite: TGocciaTestSuite): Integer;
    procedure CollectSuiteNames(const ASuite: TGocciaTestSuite;
      const ANames: TStringList);
    procedure RunCallbacks(const ACallbacks: TGocciaArgumentsCollection);
    procedure AssertionPassed(const ATestName: string);
    procedure AssertionFailed(const ATestName, AMessage: string);
    procedure StartTest(const ATestName: string);
    procedure EndTest;
    procedure ResetTestStats;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
    destructor Destroy; override;

    // Main expect function
    function Expect(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    // Test registration functions (don't execute immediately)
    function Describe(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DescribeSkip(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DescribeSkipIf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DescribeRunIf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DescribeOnly(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DescribeEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function DescribeConditional(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function Test(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function It(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function Skip(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TestSkipIf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TestRunIf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TestOnly(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TestEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TestTodo(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function TestConditional(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;


    // Setup/teardown
    function BeforeAll(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function BeforeEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function AfterEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function AfterAll(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function OnTestFinished(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    // Mock/spy creation
    function MockFunction(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SpyOn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    // Test execution
    function RunTests(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    // Exposed for native Pascal unit tests
    property CurrentTestHasFailures: Boolean read FTestStats.CurrentTestHasFailures;
    property SuppressOutput: Boolean read FSuppressOutput write FSuppressOutput;
    procedure ResetCurrentTestState;
  end;

implementation

uses
  Math,
  SysUtils,

  GarbageCollector.Generic,
  TimingUtils,

  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Evaluator,
  Goccia.Evaluator.Comparison,
  Goccia.MicrotaskQueue,
  Goccia.RegExp.Runtime,
  Goccia.Values.ClassHelper,
  Goccia.Values.ClassValue,
  Goccia.Values.Error,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.PromiseValue,
  Goccia.Values.SetValue;

{ TGocciaTestSuite }

procedure AddTempRootIfNeeded(const AValue: TGocciaValue);
begin
  if Assigned(TGarbageCollector.Instance) and Assigned(AValue) then
    TGarbageCollector.Instance.AddTempRoot(AValue);
end;

procedure RemoveTempRootIfNeeded(const AValue: TGocciaValue);
begin
  if Assigned(TGarbageCollector.Instance) and Assigned(AValue) then
    TGarbageCollector.Instance.RemoveTempRoot(AValue);
end;

procedure AddCollectionRoots(const ACollection: TGocciaArgumentsCollection);
var
  I: Integer;
begin
  if not Assigned(ACollection) then
    Exit;

  for I := 0 to ACollection.Length - 1 do
    AddTempRootIfNeeded(ACollection.GetElement(I));
end;

procedure RemoveCollectionRoots(const ACollection: TGocciaArgumentsCollection);
var
  I: Integer;
begin
  if not Assigned(ACollection) then
    Exit;

  for I := 0 to ACollection.Length - 1 do
    RemoveTempRootIfNeeded(ACollection.GetElement(I));
end;

{ TGocciaRegisteredEntry }

constructor TGocciaRegisteredEntry.Create(const AParentSuite: TGocciaTestSuite;
  const AIsSkipped, AIsFocused: Boolean);
begin
  inherited Create;
  ParentSuite := AParentSuite;
  IsSkipped := AIsSkipped;
  IsFocused := AIsFocused;
end;

{ TGocciaTestSuite }

constructor TGocciaTestSuite.Create(const AParentSuite: TGocciaTestSuite;
  const AName: string; const ASuiteFunction: TGocciaFunctionBase;
  const AArguments: TGocciaArgumentsCollection; const AIsSkipped,
  AIsFocused: Boolean);
begin
  inherited Create(AParentSuite, AIsSkipped, AIsFocused);
  Name := AName;
  SuiteFunction := ASuiteFunction;
  if Assigned(AArguments) then
    SuiteArguments := AArguments
  else
    SuiteArguments := TGocciaArgumentsCollection.Create;
  Entries := TGocciaRegisteredEntryList.Create(True);
  BeforeAllCallbacks := TGocciaArgumentsCollection.Create;
  BeforeEachCallbacks := TGocciaArgumentsCollection.Create;
  AfterEachCallbacks := TGocciaArgumentsCollection.Create;
  AfterAllCallbacks := TGocciaArgumentsCollection.Create;

  AddTempRootIfNeeded(SuiteFunction);
  AddCollectionRoots(SuiteArguments);
end;

destructor TGocciaTestSuite.Destroy;
begin
  RemoveCollectionRoots(BeforeAllCallbacks);
  RemoveCollectionRoots(BeforeEachCallbacks);
  RemoveCollectionRoots(AfterEachCallbacks);
  RemoveCollectionRoots(AfterAllCallbacks);
  RemoveCollectionRoots(SuiteArguments);
  RemoveTempRootIfNeeded(SuiteFunction);

  Entries.Free;
  BeforeAllCallbacks.Free;
  BeforeEachCallbacks.Free;
  AfterEachCallbacks.Free;
  AfterAllCallbacks.Free;
  SuiteArguments.Free;
  inherited;
end;

procedure TGocciaTestSuite.AddEntry(const AEntry: TGocciaRegisteredEntry);
begin
  Entries.Add(AEntry);
end;

procedure TGocciaTestSuite.AddHook(const ACallback: TGocciaFunctionBase;
  const APhase: TGocciaTestHookPhase);
begin
  AddTempRootIfNeeded(ACallback);

  case APhase of
    thBeforeAll:
      BeforeAllCallbacks.Add(ACallback);
    thBeforeEach:
      BeforeEachCallbacks.Add(ACallback);
    thAfterEach:
      AfterEachCallbacks.Add(ACallback);
    thAfterAll:
      AfterAllCallbacks.Add(ACallback);
  end;
end;

procedure TGocciaTestSuite.ClearRegisteredContent;
begin
  Entries.Clear;

  RemoveCollectionRoots(BeforeAllCallbacks);
  BeforeAllCallbacks.Clear;

  RemoveCollectionRoots(BeforeEachCallbacks);
  BeforeEachCallbacks.Clear;

  RemoveCollectionRoots(AfterEachCallbacks);
  AfterEachCallbacks.Clear;

  RemoveCollectionRoots(AfterAllCallbacks);
  AfterAllCallbacks.Clear;
end;

function TGocciaTestSuite.DisplayName: string;
begin
  Result := Name;
end;

function TGocciaTestSuite.GetFullName: string;
begin
  if not Assigned(ParentSuite) or (ParentSuite.GetFullName = '') then
    Result := Name
  else if Name = '' then
    Result := ParentSuite.GetFullName
  else
    Result := ParentSuite.GetFullName + ' > ' + Name;
end;

{ TGocciaTestCase }

constructor TGocciaTestCase.Create(const AParentSuite: TGocciaTestSuite;
  const AName: string; const ATestFunction: TGocciaFunctionBase;
  const AArguments: TGocciaArgumentsCollection; const AIsSkipped, AIsFocused,
  AIsTodo: Boolean);
begin
  inherited Create(AParentSuite, AIsSkipped, AIsFocused);
  Name := AName;
  TestFunction := ATestFunction;
  if Assigned(AArguments) then
    TestArguments := AArguments
  else
    TestArguments := TGocciaArgumentsCollection.Create;
  IsTodo := AIsTodo;

  AddTempRootIfNeeded(TestFunction);
  AddCollectionRoots(TestArguments);
end;

destructor TGocciaTestCase.Destroy;
begin
  RemoveCollectionRoots(TestArguments);
  RemoveTempRootIfNeeded(TestFunction);
  TestArguments.Free;
  inherited;
end;

function TGocciaTestCase.DisplayName: string;
begin
  Result := Name;
end;

function TGocciaTestCase.GetFullName: string;
begin
  if not Assigned(ParentSuite) or (ParentSuite.GetFullName = '') then
    Result := Name
  else
    Result := ParentSuite.GetFullName + ' > ' + Name;
end;

{ TGocciaParameterizedRegistrationFunction }

constructor TGocciaParameterizedRegistrationFunction.Create(
  const ATestAssertions: TGocciaTestAssertions; const ATable: TGocciaValue;
  const ATarget: TGocciaParameterizedRegistrationTarget);
begin
  inherited Create;
  FTestAssertions := ATestAssertions;
  FTable := ATable;
  FTarget := ATarget;
end;

function TGocciaParameterizedRegistrationFunction.GetFunctionLength: Integer;
begin
  Result := 2;
end;

function TGocciaParameterizedRegistrationFunction.GetFunctionName: string;
begin
  case FTarget of
    prtDescribe:
      Result := 'describe';
    prtTest:
      Result := 'test';
  end;
end;

function TGocciaParameterizedRegistrationFunction.Call(
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  BaseName: string;
  Callback: TGocciaFunctionBase;
  Rows: TGocciaArrayValue;
  I: Integer;
  RowArgs: TGocciaArgumentsCollection;
begin
  TGocciaArgumentValidator.RequireExactly(AArguments, 2, GetFunctionName,
    FTestAssertions.ThrowError);

  if not (AArguments.GetElement(0) is TGocciaStringLiteralValue) then
    FTestAssertions.ThrowError(GetFunctionName +
      ' expects first argument to be a string', 0, 0);

  if not (AArguments.GetElement(1) is TGocciaFunctionBase) then
    FTestAssertions.ThrowError(GetFunctionName +
      ' expects second argument to be a function', 0, 0);

  if not (FTable is TGocciaArrayValue) then
    FTestAssertions.ThrowError(GetFunctionName +
      '.each expects a table array', 0, 0);

  BaseName := AArguments.GetElement(0).ToStringLiteral.Value;
  Callback := TGocciaFunctionBase(AArguments.GetElement(1));
  Rows := TGocciaArrayValue(FTable);

  for I := 0 to Rows.Elements.Count - 1 do
  begin
    RowArgs := FTestAssertions.GetEachRowArguments(Rows.Elements[I]);
    try
      case FTarget of
        prtDescribe:
          FTestAssertions.RegisterDescribeEntry(
            FTestAssertions.FormatEachName(BaseName, RowArgs, I),
            Callback,
            RowArgs);
        prtTest:
          FTestAssertions.RegisterTestEntry(
            FTestAssertions.FormatEachName(BaseName, RowArgs, I),
            Callback,
            RowArgs);
      end;
      RowArgs := nil;
    finally
      RowArgs.Free;
    end;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaParameterizedRegistrationFunction.MarkReferences;
begin
  if GCMarked then
    Exit;

  inherited;
  if Assigned(FTable) then
    FTable.MarkReferences;
end;

{ TGocciaExpectationValue }

constructor TGocciaExpectationValue.Create(const AActualValue: TGocciaValue; const ATestAssertions: TGocciaTestAssertions; const AIsNegated: Boolean);
begin
  inherited Create;
  FActualValue := AActualValue;
  FTestAssertions := ATestAssertions;
  FIsNegated := AIsNegated;

  // Add matcher methods
  DefineProperty('toBe', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToBe, 'toBe', 1), [pfConfigurable, pfWritable]));
  DefineProperty('toEqual', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToEqual, 'toEqual', 1), [pfConfigurable, pfWritable]));
  DefineProperty('toContainEqual', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToContainEqual, 'toContainEqual', 1), [pfConfigurable, pfWritable]));
  DefineProperty('toStrictEqual', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToStrictEqual, 'toStrictEqual', 1), [pfConfigurable, pfWritable]));
  DefineProperty('toMatchObject', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToMatchObject, 'toMatchObject', 1), [pfConfigurable, pfWritable]));
  DefineProperty('toMatch', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToMatch, 'toMatch', 1), [pfConfigurable, pfWritable]));
  DefineProperty('toBeNull', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToBeNull, 'toBeNull', 0), [pfConfigurable, pfWritable]));
  DefineProperty('toBeNaN', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToBeNaN, 'toBeNaN', 0), [pfConfigurable, pfWritable]));
  DefineProperty('toBeUndefined', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToBeUndefined, 'toBeUndefined', 0), [pfConfigurable, pfWritable]));
  DefineProperty('toBeDefined', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToBeDefined, 'toBeDefined', 0), [pfConfigurable, pfWritable]));
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
    TGocciaNativeFunctionValue.Create(ToHaveProperty, 'toHaveProperty', 1), [pfConfigurable, pfWritable]));
  DefineProperty('toThrow', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToThrow, 'toThrow', 1), [pfConfigurable, pfWritable]));
  DefineProperty('toBeCloseTo', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToBeCloseTo, 'toBeCloseTo', 2), [pfConfigurable, pfWritable]));

  // Mock matchers
  DefineProperty('toHaveBeenCalled', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToHaveBeenCalled, 'toHaveBeenCalled', 0), [pfConfigurable, pfWritable]));
  DefineProperty('toHaveBeenCalledTimes', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToHaveBeenCalledTimes, 'toHaveBeenCalledTimes', 1), [pfConfigurable, pfWritable]));
  DefineProperty('toHaveBeenCalledWith', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToHaveBeenCalledWith, 'toHaveBeenCalledWith', -1), [pfConfigurable, pfWritable]));
  DefineProperty('toHaveBeenLastCalledWith', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToHaveBeenLastCalledWith, 'toHaveBeenLastCalledWith', -1), [pfConfigurable, pfWritable]));
  DefineProperty('toHaveBeenNthCalledWith', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToHaveBeenNthCalledWith, 'toHaveBeenNthCalledWith', -1), [pfConfigurable, pfWritable]));
  DefineProperty('toHaveReturned', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToHaveReturned, 'toHaveReturned', 0), [pfConfigurable, pfWritable]));
  DefineProperty('toHaveReturnedTimes', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToHaveReturnedTimes, 'toHaveReturnedTimes', 1), [pfConfigurable, pfWritable]));
  DefineProperty('toHaveReturnedWith', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToHaveReturnedWith, 'toHaveReturnedWith', 1), [pfConfigurable, pfWritable]));
  DefineProperty('toHaveLastReturnedWith', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToHaveLastReturnedWith, 'toHaveLastReturnedWith', 1), [pfConfigurable, pfWritable]));
  DefineProperty('toHaveNthReturnedWith', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToHaveNthReturnedWith, 'toHaveNthReturnedWith', 2), [pfConfigurable, pfWritable]));

  // Negation property - use accessor to make it a getter
  DefineProperty('not', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.Create(GetNot, 'not', 0), nil, [pfConfigurable]));

  // Promise unwrapping properties — Vitest/Jest-compatible
  DefineProperty(PROP_RESOLVES, TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.Create(GetResolves, PROP_RESOLVES, 0), nil, [pfConfigurable]));
  DefineProperty(PROP_REJECTS, TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.Create(GetRejects, PROP_REJECTS, 0), nil, [pfConfigurable]));
end;

function TGocciaExpectationValue.ToBe(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  IsEqual: Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'toBe', FTestAssertions.ThrowError);

  Expected := AArgs.GetElement(0);
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

function TGocciaExpectationValue.ToEqual(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  IsEqual: Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'toEqual', FTestAssertions.ThrowError);

  Expected := AArgs.GetElement(0);
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

function TGocciaExpectationValue.ToContainEqual(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  I: Integer;
  Contains: Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'toContainEqual', FTestAssertions.ThrowError);

  if not (FActualValue is TGocciaArrayValue) then
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionPassed('toContainEqual')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toContainEqual',
        'Expected an array but received ' + FActualValue.ToStringLiteral.Value);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  Expected := AArgs.GetElement(0);
  Contains := False;
  for I := 0 to TGocciaArrayValue(FActualValue).Elements.Count - 1 do
    if IsDeepEqual(TGocciaArrayValue(FActualValue).Elements[I], Expected) then
    begin
      Contains := True;
      Break;
    end;

  if FIsNegated then
    Contains := not Contains;

  if Contains then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toContainEqual');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toContainEqual',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' not to contain equal ' + Expected.ToStringLiteral.Value)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toContainEqual',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' to contain equal ' + Expected.ToStringLiteral.Value);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToStrictEqual(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  IsEqual: Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'toStrictEqual', FTestAssertions.ThrowError);

  Expected := AArgs.GetElement(0);
  IsEqual := IsDeepEqual(FActualValue, Expected);

  if FIsNegated then
    IsEqual := not IsEqual;

  if IsEqual then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toStrictEqual');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toStrictEqual',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' not to strictly equal ' + Expected.ToStringLiteral.Value)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toStrictEqual',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' to strictly equal ' + Expected.ToStringLiteral.Value);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToMatchObject(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  Matches: Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'toMatchObject', FTestAssertions.ThrowError);

  Expected := AArgs.GetElement(0);

  if not (FActualValue is TGocciaObjectValue) then
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionPassed('toMatchObject')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toMatchObject',
        'Expected an object but received ' + FActualValue.ToStringLiteral.Value);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  if not (Expected is TGocciaObjectValue) then
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionPassed('toMatchObject')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toMatchObject',
        'Expected a match object but received ' + Expected.ToStringLiteral.Value);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  Matches := IsPartialDeepEqual(FActualValue, Expected);

  if FIsNegated then
    Matches := not Matches;

  if Matches then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toMatchObject');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toMatchObject',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' not to match object ' + Expected.ToStringLiteral.Value)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toMatchObject',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' to match object ' + Expected.ToStringLiteral.Value);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToMatch(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  ActualString: string;
  Matches: Boolean;
  ExpectedDescription: string;
  MatchValue: TGocciaValue;
  MatchIndex: Integer;
  MatchEnd: Integer;
  NextIndex: Integer;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'toMatch', FTestAssertions.ThrowError);

  if not (FActualValue is TGocciaStringLiteralValue) then
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionPassed('toMatch')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toMatch',
        'Expected a string but received ' + FActualValue.ToStringLiteral.Value);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  Expected := AArgs.GetElement(0);
  if not (Expected is TGocciaStringLiteralValue) and not IsRegExpValue(Expected) then
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionPassed('toMatch')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toMatch',
        'Expected a string pattern or RegExp but received ' +
        Expected.ToStringLiteral.Value);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  ActualString := FActualValue.ToStringLiteral.Value;
  if IsRegExpValue(Expected) then
  begin
    ExpectedDescription := RegExpObjectToString(Expected);
    Matches := MatchRegExpObject(Expected, ActualString, 0, False, False,
      MatchValue, MatchIndex, MatchEnd, NextIndex);
  end
  else
  begin
    ExpectedDescription := Expected.ToStringLiteral.Value;
    Matches := Pos(ExpectedDescription, ActualString) > 0;
  end;

  if FIsNegated then
    Matches := not Matches;

  if Matches then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toMatch');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toMatch',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' not to match ' +
        ExpectedDescription)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toMatch',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' to match ' +
        ExpectedDescription);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToBeNull(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
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

function TGocciaExpectationValue.ToBeNaN(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  IsNaNValue: Boolean;
begin
  IsNaNValue := FActualValue.ToNumberLiteral.IsNaN;

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

function TGocciaExpectationValue.ToBeUndefined(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
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

function TGocciaExpectationValue.ToBeDefined(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  IsDefined: Boolean;
begin
  IsDefined := not (FActualValue is TGocciaUndefinedLiteralValue);

  if FIsNegated then
    IsDefined := not IsDefined;

  if IsDefined then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toBeDefined');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeDefined',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' not to be defined')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeDefined',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' to be defined');
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToBeTruthy(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
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

function TGocciaExpectationValue.ToBeFalsy(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
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

function TGocciaExpectationValue.ToBeGreaterThan(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  IsGreater: Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'toBeGreaterThan', FTestAssertions.ThrowError);

  Expected := AArgs.GetElement(0);
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

function TGocciaExpectationValue.ToBeGreaterThanOrEqual(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  IsGreaterOrEqual: Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'toBeGreaterThanOrEqual', FTestAssertions.ThrowError);

  Expected := AArgs.GetElement(0);
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

function TGocciaExpectationValue.ToBeLessThan(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  IsLess: Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'toBeLessThan', FTestAssertions.ThrowError);

  Expected := AArgs.GetElement(0);
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

function TGocciaExpectationValue.ToBeLessThanOrEqual(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  IsLessOrEqual: Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'toBeLessThanOrEqual', FTestAssertions.ThrowError);

  Expected := AArgs.GetElement(0);
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

function TGocciaExpectationValue.ToContain(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  Contains: Boolean;
  ActualStr, ExpectedStr: string;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'toContain', FTestAssertions.ThrowError);

  Expected := AArgs.GetElement(0);

  // For strings, check substring
  if FActualValue is TGocciaStringLiteralValue then
  begin
    ActualStr := FActualValue.ToStringLiteral.Value;
    ExpectedStr := Expected.ToStringLiteral.Value;
    Contains := Pos(ExpectedStr, ActualStr) > 0;
  end
  else if FActualValue is TGocciaArrayValue then
  begin
    Contains := TGocciaArrayValue(FActualValue).Includes(Expected);
  end
  else if FActualValue is TGocciaSetValue then
  begin
    Contains := TGocciaSetValue(FActualValue).ContainsValue(Expected);
  end
  else
  begin
    Contains := False;
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

function TGocciaExpectationValue.ToBeInstanceOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ExpectedConstructor: TGocciaValue;
  IsInstance: Boolean;
  ConstructorName: string;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'toBeInstanceOf', FTestAssertions.ThrowError);

  ExpectedConstructor := AArgs.GetElement(0);
  IsInstance := False;

    // Check for built-in types
  if ExpectedConstructor is TGocciaNativeFunctionValue then
  begin
    ConstructorName := TGocciaNativeFunctionValue(ExpectedConstructor).Name;
    if ConstructorName = 'Function' then
    begin
      // Check if actual value is any kind of function
      IsInstance := (FActualValue is TGocciaFunctionBase) or
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
      IsInstance := (FActualValue is TGocciaFunctionBase) or
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

function TGocciaExpectationValue.ToHaveLength(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  HasLength: Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'toHaveLength', FTestAssertions.ThrowError);

  Expected := AArgs.GetElement(0);

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

function TGocciaExpectationValue.ToHaveProperty(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  HasProperty: Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'toHaveProperty', FTestAssertions.ThrowError);

  if not (FActualValue is TGocciaObjectValue) then
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionPassed('toHaveProperty')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveProperty',
        'Expected an object but received ' + FActualValue.ToStringLiteral.Value);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  HasProperty := TGocciaObjectValue(FActualValue).HasProperty(AArgs.GetElement(0).ToStringLiteral.Value);

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
        'Expected ' + FActualValue.ToStringLiteral.Value + ' not to have property ' + AArgs.GetElement(0).ToStringLiteral.Value)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveProperty',
        'Expected ' + FActualValue.ToStringLiteral.Value + ' to have property ' + AArgs.GetElement(0).ToStringLiteral.Value);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToThrow(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ExpectedErrorType: string;
  EmptyArgs: TGocciaArgumentsCollection;
  TestFunc: TGocciaFunctionBase;
  ThrownObj: TGocciaObjectValue;
  ErrorConstructor: TGocciaValue;
  ConstructorName: string;
  ErrorName: string;
  ErrorClassArg: TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  ExpectedErrorType := '';
  if AArgs.Length > 0 then
  begin
    // If args are provided, expect a specific error type
    ErrorClassArg := AArgs.GetElement(0);
    // If it's a class, get its name; otherwise use string representation
    if ErrorClassArg is TGocciaClassValue then
      ExpectedErrorType := TGocciaClassValue(ErrorClassArg).Name
    else if ErrorClassArg is TGocciaNativeFunctionValue then
      ExpectedErrorType := TGocciaNativeFunctionValue(ErrorClassArg).Name
    else
      ExpectedErrorType := ErrorClassArg.ToStringLiteral.Value;
  end;

  if not (FActualValue is TGocciaFunctionBase) then
  begin
    // Support .rejects.toThrow(TypeError) — actual value is the rejection reason
    if (FActualValue is TGocciaObjectValue) and TGocciaObjectValue(FActualValue).HasErrorData then
    begin
      if ExpectedErrorType = '' then
      begin
        TGocciaTestAssertions(FTestAssertions).AssertionPassed('toThrow');
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
        Exit;
      end;
      ThrownObj := TGocciaObjectValue(FActualValue);
      if ThrownObj.HasProperty(PROP_NAME) then
      begin
        ErrorName := ThrownObj.GetProperty(PROP_NAME).ToStringLiteral.Value;
        if (ErrorName = ExpectedErrorType) or
           (LowerCase(ErrorName) = LowerCase(ExpectedErrorType)) then
        begin
          TGocciaTestAssertions(FTestAssertions).AssertionPassed('toThrow');
          Result := TGocciaUndefinedLiteralValue.UndefinedValue;
          Exit;
        end;
      end;
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toThrow',
        'Expected error of type ' + ExpectedErrorType + ' but got: ' + FActualValue.ToStringLiteral.Value);
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;

    FTestAssertions.ThrowError('toThrow expects actual value to be a function', 0, 0);
    Exit;
  end;

    TestFunc := TGocciaFunctionBase(FActualValue);
  EmptyArgs := TGocciaArgumentsCollection.Create;

  try
    try
      TestFunc.Call(EmptyArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
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
          if ThrownObj.HasProperty(PROP_NAME) then
          begin
            ErrorName := ThrownObj.GetProperty(PROP_NAME).ToStringLiteral.Value;
            // Direct comparison: "TypeError" == "TypeError"
            if (ErrorName = ExpectedErrorType) or
               (LowerCase(ErrorName) = LowerCase(ExpectedErrorType)) then
            begin
              TGocciaTestAssertions(FTestAssertions).AssertionPassed('toThrow');
              Exit;
            end;
          end;

          // Fallback: check constructor property
          if ThrownObj.HasProperty(PROP_CONSTRUCTOR) then
          begin
            ErrorConstructor := ThrownObj.GetProperty(PROP_CONSTRUCTOR);
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
        if ExpectedErrorType = '' then
        begin
          TGocciaTestAssertions(FTestAssertions).AssertionPassed('toThrow');
          Exit;
        end;

        if ((ExpectedErrorType = TYPE_ERROR_NAME) and (E is TGocciaTypeError)) or
           ((ExpectedErrorType = REFERENCE_ERROR_NAME) and (E is TGocciaReferenceError)) or
           ((ExpectedErrorType = SYNTAX_ERROR_NAME) and (E is TGocciaSyntaxError)) or
           ((ExpectedErrorType = ERROR_NAME) and (E is TGocciaRuntimeError)) or
           (Pos(LowerCase(ExpectedErrorType), LowerCase(E.Message)) > 0) then
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
  end;

  TGocciaTestAssertions(FTestAssertions).AssertionFailed('toThrow',
    'Expected ' + FActualValue.ToStringLiteral.Value + ' to throw an exception');
end;

function TGocciaExpectationValue.ToBeCloseTo(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  Precision: Integer;
  ActualNum, ExpectedNum, Diff, Tolerance: Double;
  IsClose: Boolean;
  ActualTempNum, ExpectedTempNum: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'toBeCloseTo', FTestAssertions.ThrowError);

  Expected := AArgs.GetElement(0);

  // Default precision to 2 decimal places if not specified
  if AArgs.Length >= 2 then
    Precision := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value)
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

function TGocciaExpectationValue.ToHaveBeenCalled(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  MockFn: TGocciaMockFunctionValue;
  WasCalled: Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 0, 'toHaveBeenCalled',
    TGocciaTestAssertions(FTestAssertions).ThrowError);

  if not (FActualValue is TGocciaMockFunctionValue) then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveBeenCalled',
      'Value must be a mock or spy function');
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  MockFn := TGocciaMockFunctionValue(FActualValue);
  WasCalled := MockFn.MockCalls.Count > 0;

  if FIsNegated then
    WasCalled := not WasCalled;

  if WasCalled then
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toHaveBeenCalled')
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveBeenCalled',
        Format('Expected mock not to have been called but it was called %d time(s)',
          [MockFn.MockCalls.Count]))
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveBeenCalled',
        'Expected mock to have been called but it was not called');
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaExpectationValue.ToHaveBeenCalledTimes(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  MockFn: TGocciaMockFunctionValue;
  ExpectedTimes: Integer;
  Matches: Boolean;
  NumVal: TGocciaNumberLiteralValue;
begin
  if not (FActualValue is TGocciaMockFunctionValue) then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveBeenCalledTimes',
      'Value must be a mock or spy function');
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'toHaveBeenCalledTimes',
    TGocciaTestAssertions(FTestAssertions).ThrowError);

  MockFn := TGocciaMockFunctionValue(FActualValue);
  NumVal := AArgs.GetElement(0).ToNumberLiteral;
  if NumVal.IsNaN or NumVal.IsInfinity or NumVal.IsNegativeInfinity or
     (NumVal.Value < 0) or (NumVal.Value > High(Integer)) or (Frac(NumVal.Value) <> 0) then
    TGocciaTestAssertions(FTestAssertions).ThrowError(
      'toHaveBeenCalledTimes expects a non-negative integer', 0, 0);
  ExpectedTimes := Trunc(NumVal.Value);
  Matches := MockFn.MockCalls.Count = ExpectedTimes;

  if FIsNegated then
    Matches := not Matches;

  if Matches then
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toHaveBeenCalledTimes')
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveBeenCalledTimes',
        Format('Expected mock not to have been called %d time(s)', [ExpectedTimes]))
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveBeenCalledTimes',
        Format('Expected mock to have been called %d time(s) but was called %d time(s)',
          [ExpectedTimes, MockFn.MockCalls.Count]));
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaExpectationValue.ToHaveBeenCalledWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  MockFn: TGocciaMockFunctionValue;
  Found: Boolean;
  I, J: Integer;
  CallArgs: TGocciaArrayValue;
  CallMatches: Boolean;
begin
  if not (FActualValue is TGocciaMockFunctionValue) then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveBeenCalledWith',
      'Value must be a mock or spy function');
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  MockFn := TGocciaMockFunctionValue(FActualValue);
  Found := False;

  for I := 0 to MockFn.MockCalls.Count - 1 do
  begin
    if not (MockFn.MockCalls[I] is TGocciaArrayValue) then
      Continue;

    CallArgs := TGocciaArrayValue(MockFn.MockCalls[I]);
    if CallArgs.Elements.Count <> AArgs.Length then
      Continue;

    CallMatches := True;
    for J := 0 to AArgs.Length - 1 do
    begin
      if not IsDeepEqual(CallArgs.Elements[J], AArgs.GetElement(J)) then
      begin
        CallMatches := False;
        Break;
      end;
    end;

    if CallMatches then
    begin
      Found := True;
      Break;
    end;
  end;

  if FIsNegated then
    Found := not Found;

  if Found then
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toHaveBeenCalledWith')
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveBeenCalledWith',
        'Expected mock not to have been called with the specified arguments')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveBeenCalledWith',
        Format('Expected mock to have been called with the specified arguments (%d call(s) recorded)',
          [MockFn.MockCalls.Count]));
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaExpectationValue.ToHaveBeenLastCalledWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  MockFn: TGocciaMockFunctionValue;
  LastCallArgs: TGocciaArrayValue;
  Matches: Boolean;
  J: Integer;
begin
  if not (FActualValue is TGocciaMockFunctionValue) then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveBeenLastCalledWith',
      'Value must be a mock or spy function');
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  MockFn := TGocciaMockFunctionValue(FActualValue);
  if MockFn.MockCalls.Count = 0 then
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionPassed('toHaveBeenLastCalledWith')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveBeenLastCalledWith',
        'Expected mock to have been called but it was not called');
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  LastCallArgs := TGocciaArrayValue(MockFn.MockCalls[MockFn.MockCalls.Count - 1]);
  Matches := LastCallArgs.Elements.Count = AArgs.Length;

  if Matches then
    for J := 0 to AArgs.Length - 1 do
      if not IsDeepEqual(LastCallArgs.Elements[J], AArgs.GetElement(J)) then
      begin
        Matches := False;
        Break;
      end;

  if FIsNegated then
    Matches := not Matches;

  if Matches then
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toHaveBeenLastCalledWith')
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveBeenLastCalledWith',
        'Expected mock not to have been last called with the specified arguments')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveBeenLastCalledWith',
        'Expected mock to have been last called with the specified arguments');
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaExpectationValue.ToHaveBeenNthCalledWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  MockFn: TGocciaMockFunctionValue;
  N: Integer;
  NumVal: TGocciaNumberLiteralValue;
  NthCallArgs: TGocciaArrayValue;
  Matches: Boolean;
  J: Integer;
begin
  if not (FActualValue is TGocciaMockFunctionValue) then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveBeenNthCalledWith',
      'Value must be a mock or spy function');
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  if AArgs.Length < 1 then
  begin
    TGocciaTestAssertions(FTestAssertions).ThrowError(
      'toHaveBeenNthCalledWith requires at least 1 argument (call index)', 0, 0);
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  MockFn := TGocciaMockFunctionValue(FActualValue);
  NumVal := AArgs.GetElement(0).ToNumberLiteral;
  if NumVal.IsNaN or NumVal.IsInfinity or NumVal.IsNegativeInfinity or
     (NumVal.Value < 1) or (NumVal.Value > High(Integer)) or (Frac(NumVal.Value) <> 0) then
    TGocciaTestAssertions(FTestAssertions).ThrowError(
      'toHaveBeenNthCalledWith expects a positive integer index', 0, 0);
  N := Trunc(NumVal.Value);

  if (N < 1) or (N > MockFn.MockCalls.Count) then
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionPassed('toHaveBeenNthCalledWith')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveBeenNthCalledWith',
        Format('Call index %d is out of range (mock was called %d time(s))',
          [N, MockFn.MockCalls.Count]));
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  NthCallArgs := TGocciaArrayValue(MockFn.MockCalls[N - 1]);
  Matches := NthCallArgs.Elements.Count = (AArgs.Length - 1);

  if Matches then
    for J := 1 to AArgs.Length - 1 do
      if not IsDeepEqual(NthCallArgs.Elements[J - 1], AArgs.GetElement(J)) then
      begin
        Matches := False;
        Break;
      end;

  if FIsNegated then
    Matches := not Matches;

  if Matches then
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toHaveBeenNthCalledWith')
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveBeenNthCalledWith',
        Format('Expected call %d not to have the specified arguments', [N]))
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveBeenNthCalledWith',
        Format('Expected call %d to have the specified arguments', [N]));
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaExpectationValue.ToHaveReturned(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  MockFn: TGocciaMockFunctionValue;
  HasReturn: Boolean;
  I: Integer;
  ResultObj: TGocciaObjectValue;
  ResultType: TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 0, 'toHaveReturned',
    TGocciaTestAssertions(FTestAssertions).ThrowError);

  if not (FActualValue is TGocciaMockFunctionValue) then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveReturned',
      'Value must be a mock or spy function');
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  MockFn := TGocciaMockFunctionValue(FActualValue);
  HasReturn := False;

  for I := 0 to MockFn.MockResults.Count - 1 do
    if MockFn.MockResults[I] is TGocciaObjectValue then
    begin
      ResultObj := TGocciaObjectValue(MockFn.MockResults[I]);
      ResultType := ResultObj.GetProperty(PROP_TYPE);
      if (ResultType is TGocciaStringLiteralValue) and
         (TGocciaStringLiteralValue(ResultType).Value = 'return') then
      begin
        HasReturn := True;
        Break;
      end;
    end;

  if FIsNegated then
    HasReturn := not HasReturn;

  if HasReturn then
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toHaveReturned')
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveReturned',
        'Expected mock not to have returned')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveReturned',
        'Expected mock to have returned at least once');
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaExpectationValue.ToHaveReturnedTimes(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  MockFn: TGocciaMockFunctionValue;
  ExpectedTimes, ActualCount, I: Integer;
  NumVal: TGocciaNumberLiteralValue;
  ResultObj: TGocciaObjectValue;
  ResultType: TGocciaValue;
  Matches: Boolean;
begin
  if not (FActualValue is TGocciaMockFunctionValue) then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveReturnedTimes',
      'Value must be a mock or spy function');
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'toHaveReturnedTimes',
    TGocciaTestAssertions(FTestAssertions).ThrowError);

  MockFn := TGocciaMockFunctionValue(FActualValue);
  NumVal := AArgs.GetElement(0).ToNumberLiteral;
  if NumVal.IsNaN or NumVal.IsInfinity or NumVal.IsNegativeInfinity or
     (NumVal.Value < 0) or (NumVal.Value > High(Integer)) or (Frac(NumVal.Value) <> 0) then
    TGocciaTestAssertions(FTestAssertions).ThrowError(
      'toHaveReturnedTimes expects a non-negative integer', 0, 0);
  ExpectedTimes := Trunc(NumVal.Value);
  ActualCount := 0;

  for I := 0 to MockFn.MockResults.Count - 1 do
    if MockFn.MockResults[I] is TGocciaObjectValue then
    begin
      ResultObj := TGocciaObjectValue(MockFn.MockResults[I]);
      ResultType := ResultObj.GetProperty(PROP_TYPE);
      if (ResultType is TGocciaStringLiteralValue) and
         (TGocciaStringLiteralValue(ResultType).Value = 'return') then
        Inc(ActualCount);
    end;

  Matches := ActualCount = ExpectedTimes;
  if FIsNegated then
    Matches := not Matches;

  if Matches then
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toHaveReturnedTimes')
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveReturnedTimes',
        Format('Expected mock not to have returned %d time(s)', [ExpectedTimes]))
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveReturnedTimes',
        Format('Expected mock to have returned %d time(s) but returned %d time(s)',
          [ExpectedTimes, ActualCount]));
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaExpectationValue.ToHaveReturnedWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  MockFn: TGocciaMockFunctionValue;
  Expected: TGocciaValue;
  Found: Boolean;
  I: Integer;
  ResultObj: TGocciaObjectValue;
  ResultType, ResultValue: TGocciaValue;
begin
  if not (FActualValue is TGocciaMockFunctionValue) then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveReturnedWith',
      'Value must be a mock or spy function');
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'toHaveReturnedWith',
    TGocciaTestAssertions(FTestAssertions).ThrowError);

  MockFn := TGocciaMockFunctionValue(FActualValue);
  Expected := AArgs.GetElement(0);
  Found := False;

  for I := 0 to MockFn.MockResults.Count - 1 do
    if MockFn.MockResults[I] is TGocciaObjectValue then
    begin
      ResultObj := TGocciaObjectValue(MockFn.MockResults[I]);
      ResultType := ResultObj.GetProperty(PROP_TYPE);
      if (ResultType is TGocciaStringLiteralValue) and
         (TGocciaStringLiteralValue(ResultType).Value = 'return') then
      begin
        ResultValue := ResultObj.GetProperty(PROP_VALUE);
        if IsDeepEqual(ResultValue, Expected) then
        begin
          Found := True;
          Break;
        end;
      end;
    end;

  if FIsNegated then
    Found := not Found;

  if Found then
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toHaveReturnedWith')
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveReturnedWith',
        'Expected mock not to have returned with ' + Expected.ToStringLiteral.Value)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveReturnedWith',
        'Expected mock to have returned with ' + Expected.ToStringLiteral.Value);
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaExpectationValue.ToHaveLastReturnedWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  MockFn: TGocciaMockFunctionValue;
  Expected: TGocciaValue;
  Matches: Boolean;
  LastResult: TGocciaObjectValue;
  ResultType, ResultValue: TGocciaValue;
begin
  if not (FActualValue is TGocciaMockFunctionValue) then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveLastReturnedWith',
      'Value must be a mock or spy function');
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'toHaveLastReturnedWith',
    TGocciaTestAssertions(FTestAssertions).ThrowError);

  MockFn := TGocciaMockFunctionValue(FActualValue);
  Expected := AArgs.GetElement(0);

  if MockFn.MockResults.Count = 0 then
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionPassed('toHaveLastReturnedWith')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveLastReturnedWith',
        'Expected mock to have returned but it was never called');
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  LastResult := TGocciaObjectValue(MockFn.MockResults[MockFn.MockResults.Count - 1]);
  ResultType := LastResult.GetProperty(PROP_TYPE);
  ResultValue := LastResult.GetProperty(PROP_VALUE);

  Matches := (ResultType is TGocciaStringLiteralValue) and
    (TGocciaStringLiteralValue(ResultType).Value = 'return') and
    IsDeepEqual(ResultValue, Expected);

  if FIsNegated then
    Matches := not Matches;

  if Matches then
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toHaveLastReturnedWith')
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveLastReturnedWith',
        'Expected mock not to have last returned with ' + Expected.ToStringLiteral.Value)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveLastReturnedWith',
        'Expected mock to have last returned with ' + Expected.ToStringLiteral.Value);
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaExpectationValue.ToHaveNthReturnedWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  MockFn: TGocciaMockFunctionValue;
  N: Integer;
  NumVal: TGocciaNumberLiteralValue;
  Expected: TGocciaValue;
  Matches: Boolean;
  NthResult: TGocciaObjectValue;
  ResultType, ResultValue: TGocciaValue;
begin
  if not (FActualValue is TGocciaMockFunctionValue) then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveNthReturnedWith',
      'Value must be a mock or spy function');
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  TGocciaArgumentValidator.RequireExactly(AArgs, 2, 'toHaveNthReturnedWith',
    TGocciaTestAssertions(FTestAssertions).ThrowError);

  MockFn := TGocciaMockFunctionValue(FActualValue);
  NumVal := AArgs.GetElement(0).ToNumberLiteral;
  if NumVal.IsNaN or NumVal.IsInfinity or NumVal.IsNegativeInfinity or
     (NumVal.Value < 1) or (NumVal.Value > High(Integer)) or (Frac(NumVal.Value) <> 0) then
    TGocciaTestAssertions(FTestAssertions).ThrowError(
      'toHaveNthReturnedWith expects a positive integer index', 0, 0);
  N := Trunc(NumVal.Value);
  Expected := AArgs.GetElement(1);

  if (N < 1) or (N > MockFn.MockResults.Count) then
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionPassed('toHaveNthReturnedWith')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveNthReturnedWith',
        Format('Return index %d is out of range (mock returned %d time(s))',
          [N, MockFn.MockResults.Count]));
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  NthResult := TGocciaObjectValue(MockFn.MockResults[N - 1]);
  ResultType := NthResult.GetProperty(PROP_TYPE);
  ResultValue := NthResult.GetProperty(PROP_VALUE);

  Matches := (ResultType is TGocciaStringLiteralValue) and
    (TGocciaStringLiteralValue(ResultType).Value = 'return') and
    IsDeepEqual(ResultValue, Expected);

  if FIsNegated then
    Matches := not Matches;

  if Matches then
    TGocciaTestAssertions(FTestAssertions).AssertionPassed('toHaveNthReturnedWith')
  else
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveNthReturnedWith',
        Format('Expected return %d not to be %s', [N, Expected.ToStringLiteral.Value]))
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveNthReturnedWith',
        Format('Expected return %d to be %s', [N, Expected.ToStringLiteral.Value]));
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaExpectationValue.GetNot(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaExpectationValue.Create(FActualValue, FTestAssertions, True);
end;

function TGocciaExpectationValue.GetResolves(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
begin
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AddTempRoot(FActualValue);
  try
    if Assigned(TGocciaMicrotaskQueue.Instance) then
      TGocciaMicrotaskQueue.Instance.DrainQueue;

    if not (FActualValue is TGocciaPromiseValue) then
    begin
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('resolves',
        'Expected a Promise but received ' + FActualValue.ToStringLiteral.Value);
      Result := TGocciaExpectationValue.Create(TGocciaUndefinedLiteralValue.UndefinedValue, FTestAssertions, FIsNegated);
      Exit;
    end;

    Promise := TGocciaPromiseValue(FActualValue);

    if Promise.State = gpsFulfilled then
      Result := TGocciaExpectationValue.Create(Promise.PromiseResult, FTestAssertions, FIsNegated)
    else if Promise.State = gpsRejected then
    begin
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('resolves',
        'Expected Promise to resolve but it rejected with: ' + Promise.PromiseResult.ToStringLiteral.Value);
      Result := TGocciaExpectationValue.Create(TGocciaUndefinedLiteralValue.UndefinedValue, FTestAssertions, FIsNegated);
    end
    else
    begin
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('resolves',
        'Promise still pending after microtask drain');
      Result := TGocciaExpectationValue.Create(TGocciaUndefinedLiteralValue.UndefinedValue, FTestAssertions, FIsNegated);
    end;
  finally
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveTempRoot(FActualValue);
  end;
end;

function TGocciaExpectationValue.GetRejects(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
begin
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AddTempRoot(FActualValue);
  try
    if Assigned(TGocciaMicrotaskQueue.Instance) then
      TGocciaMicrotaskQueue.Instance.DrainQueue;

    if not (FActualValue is TGocciaPromiseValue) then
    begin
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('rejects',
        'Expected a Promise but received ' + FActualValue.ToStringLiteral.Value);
      Result := TGocciaExpectationValue.Create(TGocciaUndefinedLiteralValue.UndefinedValue, FTestAssertions, FIsNegated);
      Exit;
    end;

    Promise := TGocciaPromiseValue(FActualValue);

    if Promise.State = gpsRejected then
      Result := TGocciaExpectationValue.Create(Promise.PromiseResult, FTestAssertions, FIsNegated)
    else if Promise.State = gpsFulfilled then
    begin
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('rejects',
        'Expected Promise to reject but it resolved with: ' + Promise.PromiseResult.ToStringLiteral.Value);
      Result := TGocciaExpectationValue.Create(TGocciaUndefinedLiteralValue.UndefinedValue, FTestAssertions, FIsNegated);
    end
    else
    begin
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('rejects',
        'Promise still pending after microtask drain');
      Result := TGocciaExpectationValue.Create(TGocciaUndefinedLiteralValue.UndefinedValue, FTestAssertions, FIsNegated);
    end;
  finally
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveTempRoot(FActualValue);
  end;
end;

{ TGocciaTestAssertions }

constructor TGocciaTestAssertions.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  DescribeFunction: TGocciaNativeFunctionValue;
  TestFunction: TGocciaNativeFunctionValue;
  ItFunction: TGocciaNativeFunctionValue;
begin
  inherited Create(AName, AScope, AThrowError);

  FRootSuite := TGocciaTestSuite.Create(nil, '', nil, nil);
  FCurrentRegistrationSuite := FRootSuite;
  FOnTestFinishedCallbacks := TGocciaArgumentsCollection.Create;
  ResetTestStats;

  // Functions are registered on both the scope (for direct access in test scripts)
  // and the builtin object (for completeness). This dual registration is intentional.

  // Register testing functions globally for easy access
  AScope.DefineLexicalBinding('expect', TGocciaNativeFunctionValue.Create(Expect, 'expect', 1), dtConst);

  // Create describe function with skip/skipIf/runIf properties
  DescribeFunction := TGocciaNativeFunctionValue.Create(Describe, 'describe', 2);
  ConfigureDescribeFunction(DescribeFunction);
  AScope.DefineLexicalBinding('describe', DescribeFunction, dtConst);

  // Create test function with skip/skipIf/runIf properties
  TestFunction := TGocciaNativeFunctionValue.Create(Test, 'test', 2);
  ConfigureTestFunction(TestFunction);
  AScope.DefineLexicalBinding('test', TestFunction, dtConst);

  ItFunction := TGocciaNativeFunctionValue.Create(It, 'it', 2);
  ConfigureTestFunction(ItFunction);
  AScope.DefineLexicalBinding('it', ItFunction, dtConst);
  AScope.DefineLexicalBinding('beforeAll', TGocciaNativeFunctionValue.Create(BeforeAll, 'beforeAll', 1), dtConst);
  AScope.DefineLexicalBinding('beforeEach', TGocciaNativeFunctionValue.Create(BeforeEach, 'beforeEach', 1), dtConst);
  AScope.DefineLexicalBinding('afterEach', TGocciaNativeFunctionValue.Create(AfterEach, 'afterEach', 1), dtConst);
  AScope.DefineLexicalBinding('afterAll', TGocciaNativeFunctionValue.Create(AfterAll, 'afterAll', 1), dtConst);
  AScope.DefineLexicalBinding('onTestFinished', TGocciaNativeFunctionValue.Create(OnTestFinished, 'onTestFinished', 1), dtConst);
  AScope.DefineLexicalBinding('runTests', TGocciaNativeFunctionValue.Create(RunTests, 'runTests', 0), dtConst);
  AScope.DefineLexicalBinding('mock', TGocciaNativeFunctionValue.Create(MockFunction, 'mock', 0), dtConst);
  AScope.DefineLexicalBinding('spyOn', TGocciaNativeFunctionValue.Create(SpyOn, 'spyOn', 2), dtConst);

  // Also set them in the builtin object for completeness
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(Expect, 'expect', 1));
  FBuiltinObject.RegisterNativeMethod(DescribeFunction);
  FBuiltinObject.RegisterNativeMethod(TestFunction);
  FBuiltinObject.RegisterNativeMethod(ItFunction);
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(BeforeAll, 'beforeAll', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(BeforeEach, 'beforeEach', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(AfterEach, 'afterEach', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(AfterAll, 'afterAll', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(OnTestFinished, 'onTestFinished', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(RunTests, 'runTests', 0));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MockFunction, 'mock', 0));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(SpyOn, 'spyOn', 2));
end;

destructor TGocciaTestAssertions.Destroy;
begin
  RemoveCollectionRoots(FOnTestFinishedCallbacks);
  FOnTestFinishedCallbacks.Free;
  FRootSuite.Free;
  inherited;
end;

procedure TGocciaTestAssertions.ConfigureDescribeFunction(
  const AFunction: TGocciaNativeFunctionValue);
begin
  AFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(DescribeSkip,
    'skip', 2));
  AFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    DescribeSkipIf, 'skipIf', 1));
  AFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    DescribeRunIf, 'runIf', 1));
  AFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    DescribeOnly, 'only', 2));
  AFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    DescribeEach, 'each', 1));
end;

procedure TGocciaTestAssertions.ConfigureTestFunction(
  const AFunction: TGocciaNativeFunctionValue);
begin
  AFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(Skip, 'skip',
    2));
  AFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    TestSkipIf, 'skipIf', 1));
  AFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    TestRunIf, 'runIf', 1));
  AFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    TestOnly, 'only', 2));
  AFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    TestEach, 'each', 1));
  AFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    TestTodo, 'todo', 1));
end;

function TGocciaTestAssertions.GetCurrentRegistrationSuite: TGocciaTestSuite;
begin
  if Assigned(FCurrentRegistrationSuite) then
    Result := FCurrentRegistrationSuite
  else
    Result := FRootSuite;
end;

procedure TGocciaTestAssertions.RegisterDescribeEntry(const AName: string;
  const ASuiteFunction: TGocciaFunctionBase;
  const AArguments: TGocciaArgumentsCollection; const AIsSkipped,
  AIsFocused: Boolean);
begin
  GetCurrentRegistrationSuite.AddEntry(TGocciaTestSuite.Create(
    GetCurrentRegistrationSuite, AName, ASuiteFunction, AArguments,
    AIsSkipped, AIsFocused));
end;

procedure TGocciaTestAssertions.RegisterTestEntry(const AName: string;
  const ATestFunction: TGocciaFunctionBase;
  const AArguments: TGocciaArgumentsCollection; const AIsSkipped,
  AIsFocused, AIsTodo: Boolean);
begin
  GetCurrentRegistrationSuite.AddEntry(TGocciaTestCase.Create(
    GetCurrentRegistrationSuite, AName, ATestFunction, AArguments,
    AIsSkipped, AIsFocused, AIsTodo));
end;

function TGocciaTestAssertions.ValidateDescribeRegistration(
  const AArgs: TGocciaArgumentsCollection; const AFunctionName: string;
  out ASuiteName: string; out ASuiteFunction: TGocciaFunctionBase): Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 2, AFunctionName, ThrowError);

  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowError(AFunctionName + ' expects first argument to be a string', 0, 0);

  if not (AArgs.GetElement(1) is TGocciaFunctionBase) then
    ThrowError(AFunctionName + ' expects second argument to be a function', 0,
      0);

  ASuiteName := AArgs.GetElement(0).ToStringLiteral.Value;
  ASuiteFunction := TGocciaFunctionBase(AArgs.GetElement(1));
  Result := True;
end;

function TGocciaTestAssertions.ValidateTestRegistration(
  const AArgs: TGocciaArgumentsCollection; const AFunctionName: string;
  out ATestName: string; out ATestFunction: TGocciaFunctionBase): Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 2, AFunctionName, ThrowError);

  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowError(AFunctionName + ' expects first argument to be a string', 0, 0);

  if not (AArgs.GetElement(1) is TGocciaFunctionBase) then
    ThrowError(AFunctionName + ' expects second argument to be a function', 0,
      0);

  ATestName := AArgs.GetElement(0).ToStringLiteral.Value;
  ATestFunction := TGocciaFunctionBase(AArgs.GetElement(1));
  Result := True;
end;

procedure TGocciaTestAssertions.RegisterHook(
  const AArgs: TGocciaArgumentsCollection; const AHookName: string;
  const APhase: TGocciaTestHookPhase);
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, AHookName, ThrowError);

  if not (AArgs.GetElement(0) is TGocciaFunctionBase) then
    ThrowError(AHookName + ' expects a function argument', 0, 0);

  GetCurrentRegistrationSuite.AddHook(TGocciaFunctionBase(AArgs.GetElement(0)),
    APhase);
end;

function TGocciaTestAssertions.GetEachRowArguments(
  const ARow: TGocciaValue): TGocciaArgumentsCollection;
var
  I: Integer;
begin
  if ARow is TGocciaArrayValue then
  begin
    Result := TGocciaArgumentsCollection.CreateWithCapacity(
      TGocciaArrayValue(ARow).Elements.Count);
    for I := 0 to TGocciaArrayValue(ARow).Elements.Count - 1 do
      Result.Add(TGocciaArrayValue(ARow).Elements[I]);
    Exit;
  end;

  Result := TGocciaArgumentsCollection.CreateWithCapacity(1);
  Result.Add(ARow);
end;

function TGocciaTestAssertions.FormatEachName(const ATemplate: string;
  const AArguments: TGocciaArgumentsCollection; const ARowIndex: Integer): string;
var
  I: Integer;
  Placeholder: Char;
  ArgIndex: Integer;

  function ConsumeValue: TGocciaValue;
  begin
    if ArgIndex < AArguments.Length then
    begin
      Result := AArguments.GetElement(ArgIndex);
      Inc(ArgIndex);
    end
    else
      Result := nil;
  end;

  function FormatPlaceholder(const AValue: TGocciaValue;
    const AToken: Char): string;
  begin
    if not Assigned(AValue) then
      Exit('%' + AToken);

    case AToken of
      'd', 'i':
        Result := IntToStr(Trunc(AValue.ToNumberLiteral.Value));
      'f':
        Result := FloatToStr(AValue.ToNumberLiteral.Value);
      'j', 'o', 's':
        Result := AValue.ToStringLiteral.Value;
      '#':
        Result := IntToStr(ARowIndex);
    else
      Result := '%' + AToken;
    end;
  end;
begin
  Result := '';
  ArgIndex := 0;
  I := 1;
  while I <= Length(ATemplate) do
  begin
    if ATemplate[I] <> '%' then
    begin
      Result := Result + ATemplate[I];
      Inc(I);
      Continue;
    end;

    if I = Length(ATemplate) then
    begin
      Result := Result + '%';
      Break;
    end;

    Inc(I);
    Placeholder := ATemplate[I];
    if Placeholder = '%' then
      Result := Result + '%'
    else
      Result := Result + FormatPlaceholder(ConsumeValue, Placeholder);
    Inc(I);
  end;
end;

procedure TGocciaTestAssertions.ClearNestedRegistrations(
  const ASuite: TGocciaTestSuite);
var
  I: Integer;
begin
  for I := 0 to ASuite.Entries.Count - 1 do
    if ASuite.Entries[I] is TGocciaTestSuite then
      TGocciaTestSuite(ASuite.Entries[I]).ClearRegisteredContent;
end;

procedure TGocciaTestAssertions.BuildNestedRegistrations(
  const ASuite: TGocciaTestSuite; const AFailedTestDetails: TStringList);
var
  I: Integer;
  Entry: TGocciaRegisteredEntry;
  ChildSuite: TGocciaTestSuite;
  PreviousSuite: TGocciaTestSuite;
begin
  for I := 0 to ASuite.Entries.Count - 1 do
  begin
    Entry := ASuite.Entries[I];
    if not (Entry is TGocciaTestSuite) then
      Continue;

    ChildSuite := TGocciaTestSuite(Entry);
    ChildSuite.ClearRegisteredContent;

    PreviousSuite := FCurrentRegistrationSuite;
    FCurrentRegistrationSuite := ChildSuite;
    try
      try
        if Assigned(ChildSuite.SuiteFunction) then
          ChildSuite.SuiteFunction.Call(ChildSuite.SuiteArguments,
            TGocciaUndefinedLiteralValue.UndefinedValue);
      except
        on E: Exception do
        begin
          if not FSuppressOutput then
            WriteLn('Error in describe block "', ChildSuite.GetFullName,
              '": ', E.Message);
          AFailedTestDetails.Add('Describe "' + ChildSuite.GetFullName +
            '": ' + E.Message);
        end;
      end;
    finally
      FCurrentRegistrationSuite := PreviousSuite;
    end;

    BuildNestedRegistrations(ChildSuite, AFailedTestDetails);
  end;
end;

procedure TGocciaTestAssertions.CollectBeforeEachCallbacks(
  const ASuite: TGocciaTestSuite; const ACallbacks: TGocciaArgumentsCollection);
var
  I: Integer;
begin
  if not Assigned(ASuite) then
    Exit;

  CollectBeforeEachCallbacks(ASuite.ParentSuite, ACallbacks);
  for I := 0 to ASuite.BeforeEachCallbacks.Length - 1 do
    ACallbacks.Add(ASuite.BeforeEachCallbacks.GetElement(I));
end;

procedure TGocciaTestAssertions.CollectAfterEachCallbacks(
  const ASuite: TGocciaTestSuite; const ACallbacks: TGocciaArgumentsCollection);
var
  I: Integer;
begin
  if not Assigned(ASuite) then
    Exit;

  for I := 0 to ASuite.AfterEachCallbacks.Length - 1 do
    ACallbacks.Add(ASuite.AfterEachCallbacks.GetElement(I));
  CollectAfterEachCallbacks(ASuite.ParentSuite, ACallbacks);
end;

function TGocciaTestAssertions.IsSuiteSkipped(
  const ASuite: TGocciaTestSuite): Boolean;
var
  CurrentSuite: TGocciaTestSuite;
begin
  CurrentSuite := ASuite;
  while Assigned(CurrentSuite) do
  begin
    if CurrentSuite.IsSkipped then
      Exit(True);
    CurrentSuite := CurrentSuite.ParentSuite;
  end;
  Result := False;
end;

function TGocciaTestAssertions.IsSuiteFocusedInHierarchy(
  const ASuite: TGocciaTestSuite): Boolean;
var
  CurrentSuite: TGocciaTestSuite;
begin
  CurrentSuite := ASuite;
  while Assigned(CurrentSuite) do
  begin
    if CurrentSuite.IsFocused then
      Exit(True);
    CurrentSuite := CurrentSuite.ParentSuite;
  end;
  Result := False;
end;

function TGocciaTestAssertions.IsTestSelected(const ATestCase: TGocciaTestCase;
  const AHasFocusedEntries: Boolean): Boolean;
begin
  if not AHasFocusedEntries then
    Exit(True);

  Result := ATestCase.IsFocused or IsSuiteFocusedInHierarchy(
    ATestCase.ParentSuite);
end;

function TGocciaTestAssertions.SuiteHasSelectedEntries(
  const ASuite: TGocciaTestSuite; const AHasFocusedEntries: Boolean): Boolean;
var
  I: Integer;
  Entry: TGocciaRegisteredEntry;
begin
  if not AHasFocusedEntries then
    Exit(True);

  if ASuite.IsFocused then
    Exit(True);

  for I := 0 to ASuite.Entries.Count - 1 do
  begin
    Entry := ASuite.Entries[I];
    if (Entry is TGocciaTestCase) and TGocciaTestCase(Entry).IsFocused then
      Exit(True);
    if (Entry is TGocciaTestSuite) and
       SuiteHasSelectedEntries(TGocciaTestSuite(Entry), True) then
      Exit(True);
  end;

  Result := False;
end;

function TGocciaTestAssertions.SuiteHasRunnableEntries(
  const ASuite: TGocciaTestSuite; const AHasFocusedEntries: Boolean): Boolean;
var
  I: Integer;
  Entry: TGocciaRegisteredEntry;
  TestCase: TGocciaTestCase;
begin
  for I := 0 to ASuite.Entries.Count - 1 do
  begin
    Entry := ASuite.Entries[I];
    if Entry is TGocciaTestCase then
    begin
      TestCase := TGocciaTestCase(Entry);
      if IsTestSelected(TestCase, AHasFocusedEntries) and
         not TestCase.IsSkipped and
         not IsSuiteSkipped(TestCase.ParentSuite) and
         not TestCase.IsTodo then
        Exit(True);
    end
    else if SuiteHasRunnableEntries(TGocciaTestSuite(Entry),
      AHasFocusedEntries) then
      Exit(True);
  end;

  Result := False;
end;

procedure TGocciaTestAssertions.ExecuteSuite(const ASuite: TGocciaTestSuite;
  const AHasFocusedEntries, AExitOnFirstFailure: Boolean;
  const AFailedTestDetails: TStringList; var AShouldStop: Boolean);
var
  I: Integer;
  Entry: TGocciaRegisteredEntry;
  TestCase: TGocciaTestCase;
  BeforeCallbacks: TGocciaArgumentsCollection;
  AfterCallbacks: TGocciaArgumentsCollection;
  TestResult: TGocciaValue;
  RejectionReason: string;
  FailureRecorded: Boolean;
  EffectiveSuiteName: string;
  HookFailed: Boolean;
  RunSuiteHooks: Boolean;
begin
  if AShouldStop then
    Exit;

  EffectiveSuiteName := ASuite.GetFullName;
  RunSuiteHooks := not IsSuiteSkipped(ASuite) and
    SuiteHasRunnableEntries(ASuite, AHasFocusedEntries);

  if RunSuiteHooks and (ASuite.BeforeAllCallbacks.Length > 0) then
  begin
    FTestStats.CurrentSuiteName := EffectiveSuiteName;
    FTestStats.CurrentTestName := 'beforeAll';
    ResetCurrentTestState;
    RunCallbacks(ASuite.BeforeAllCallbacks);
    HookFailed := FTestStats.CurrentTestHasFailures;
    ResetCurrentTestState;
    if HookFailed then
    begin
      AFailedTestDetails.Add('Hook "beforeAll" in suite "' + EffectiveSuiteName +
        '" failed');
      if AExitOnFirstFailure then
      begin
        AShouldStop := True;
        Exit;
      end;
    end;
  end;

  for I := 0 to ASuite.Entries.Count - 1 do
  begin
    if AShouldStop then
      Break;

    Entry := ASuite.Entries[I];
    if Entry is TGocciaTestSuite then
    begin
      ExecuteSuite(TGocciaTestSuite(Entry), AHasFocusedEntries,
        AExitOnFirstFailure, AFailedTestDetails, AShouldStop);
      Continue;
    end;

    TestCase := TGocciaTestCase(Entry);
    if Assigned(TestCase.ParentSuite) then
      FTestStats.CurrentSuiteName := TestCase.ParentSuite.GetFullName
    else
      FTestStats.CurrentSuiteName := '';

    Inc(FTestStats.TotalTests);
    StartTest(TestCase.Name);

    if TestCase.IsTodo then
    begin
      FTestStats.CurrentTestIsSkipped := True;
      if not FSuppressOutput then
      begin
        if FTestStats.CurrentSuiteName <> '' then
          WriteLn('    📝 ', TestCase.Name, ' in ', FTestStats.CurrentSuiteName,
            ': TODO')
        else
          WriteLn('    📝 ', TestCase.Name, ': TODO');
      end;
    end
    else if TestCase.IsSkipped or IsSuiteSkipped(TestCase.ParentSuite) or
      (AHasFocusedEntries and not IsTestSelected(TestCase, True)) then
    begin
      FTestStats.CurrentTestIsSkipped := True;
      if not FSuppressOutput then
      begin
        if FTestStats.CurrentSuiteName <> '' then
          WriteLn('    ⏸️ ', TestCase.Name, ' in ', FTestStats.CurrentSuiteName,
            ': SKIPPED')
        else
          WriteLn('    ⏸️ ', TestCase.Name, ': SKIPPED');
      end;
    end
    else
    begin
      BeforeCallbacks := TGocciaArgumentsCollection.Create;
      AfterCallbacks := TGocciaArgumentsCollection.Create;
      try
        CollectBeforeEachCallbacks(TestCase.ParentSuite, BeforeCallbacks);
        CollectAfterEachCallbacks(TestCase.ParentSuite, AfterCallbacks);
        RemoveCollectionRoots(FOnTestFinishedCallbacks);
        FOnTestFinishedCallbacks.Clear;

        RunCallbacks(BeforeCallbacks);

        FailureRecorded := False;
        TestResult := nil;
        try
          try
            if Assigned(TestCase.TestFunction) then
              TestResult := TestCase.TestFunction.Call(TestCase.TestArguments,
                TGocciaUndefinedLiteralValue.UndefinedValue)
            else
              TestResult := TGocciaUndefinedLiteralValue.UndefinedValue;

            if Assigned(TestResult) then
              AddTempRootIfNeeded(TestResult);
            try
              if Assigned(TGocciaMicrotaskQueue.Instance) then
                TGocciaMicrotaskQueue.Instance.DrainQueue;

              if TestResult is TGocciaPromiseValue then
              begin
                if TGocciaPromiseValue(TestResult).State = gpsRejected then
                begin
                  RejectionReason := TGocciaPromiseValue(TestResult).
                    PromiseResult.ToStringLiteral.Value;
                  AssertionFailed('async test', 'Returned Promise rejected: ' +
                    RejectionReason);
                  if FTestStats.CurrentSuiteName <> '' then
                    AFailedTestDetails.Add('Test "' + TestCase.Name +
                      '" in suite "' + FTestStats.CurrentSuiteName +
                      '": Promise rejected: ' + RejectionReason)
                  else
                    AFailedTestDetails.Add('Test "' + TestCase.Name +
                      '": Promise rejected: ' + RejectionReason);
                  FailureRecorded := True;
                end
                else if TGocciaPromiseValue(TestResult).State = gpsPending then
                begin
                  AssertionFailed('async test',
                    'Returned Promise still pending after microtask drain');
                  if FTestStats.CurrentSuiteName <> '' then
                    AFailedTestDetails.Add('Test "' + TestCase.Name +
                      '" in suite "' + FTestStats.CurrentSuiteName +
                      '": Promise still pending after microtask drain')
                  else
                    AFailedTestDetails.Add('Test "' + TestCase.Name +
                      '": Promise still pending after microtask drain');
                  FailureRecorded := True;
                end;
              end;
            finally
              if Assigned(TestResult) then
                RemoveTempRootIfNeeded(TestResult);
            end;
          except
            on E: Exception do
            begin
              if Assigned(TGocciaMicrotaskQueue.Instance) then
                TGocciaMicrotaskQueue.Instance.ClearQueue;
              AssertionFailed('test execution', 'Test threw an exception: ' +
                E.Message);
              if FTestStats.CurrentSuiteName <> '' then
                AFailedTestDetails.Add('Test "' + TestCase.Name +
                  '" in suite "' + FTestStats.CurrentSuiteName + '": ' +
                  E.Message)
              else
                AFailedTestDetails.Add('Test "' + TestCase.Name + '": ' +
                  E.Message);
              FailureRecorded := True;
            end;
          end;
        finally
          RunCallbacks(AfterCallbacks);
          if FOnTestFinishedCallbacks.Length > 0 then
          begin
            RunCallbacks(FOnTestFinishedCallbacks);
            RemoveCollectionRoots(FOnTestFinishedCallbacks);
            FOnTestFinishedCallbacks.Clear;
          end;
        end;
      finally
        BeforeCallbacks.Free;
        AfterCallbacks.Free;
      end;

      EndTest;

      if FTestStats.CurrentTestHasFailures and not FailureRecorded then
      begin
        if FTestStats.CurrentSuiteName <> '' then
          AFailedTestDetails.Add('Test "' + TestCase.Name + '" in suite "' +
            FTestStats.CurrentSuiteName + '"')
        else
          AFailedTestDetails.Add('Test "' + TestCase.Name + '"');
      end;

      if FTestStats.CurrentTestHasFailures and AExitOnFirstFailure then
      begin
        AShouldStop := True;
        Exit;
      end;

      Continue;
    end;

    EndTest;
  end;

  if RunSuiteHooks and (ASuite.AfterAllCallbacks.Length > 0) and
    not AShouldStop then
  begin
    FTestStats.CurrentSuiteName := EffectiveSuiteName;
    FTestStats.CurrentTestName := 'afterAll';
    ResetCurrentTestState;
    RunCallbacks(ASuite.AfterAllCallbacks);
    HookFailed := FTestStats.CurrentTestHasFailures;
    ResetCurrentTestState;
    if HookFailed then
    begin
      AFailedTestDetails.Add('Hook "afterAll" in suite "' + EffectiveSuiteName +
        '" failed');
      if AExitOnFirstFailure then
        AShouldStop := True;
    end;
  end;
end;

function TGocciaTestAssertions.CountRegisteredTests(
  const ASuite: TGocciaTestSuite): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ASuite.Entries.Count - 1 do
    if ASuite.Entries[I] is TGocciaTestCase then
      Inc(Result)
    else
      Inc(Result, CountRegisteredTests(TGocciaTestSuite(ASuite.Entries[I])));
end;

procedure TGocciaTestAssertions.CollectSuiteNames(const ASuite: TGocciaTestSuite;
  const ANames: TStringList);
var
  I: Integer;
begin
  if ASuite.Name <> '' then
    ANames.Add(ASuite.GetFullName);

  for I := 0 to ASuite.Entries.Count - 1 do
    if ASuite.Entries[I] is TGocciaTestSuite then
      CollectSuiteNames(TGocciaTestSuite(ASuite.Entries[I]), ANames);
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
  FSkipNextDescribe := False;
  FFocusNextDescribe := False;
  FSkipNextTest := False;
  FFocusNextTest := False;
end;

procedure TGocciaTestAssertions.RunCallbacks(const ACallbacks: TGocciaArgumentsCollection);
var
  I: Integer;
  Callback, CallbackResult: TGocciaValue;
  EmptyArgs: TGocciaArgumentsCollection;
  Promise: TGocciaPromiseValue;
begin
  EmptyArgs := TGocciaArgumentsCollection.Create;
  try
    for I := 0 to ACallbacks.Length - 1 do
    begin
      Callback := ACallbacks.GetElement(I);
      if Callback.IsCallable then
      begin
        try
          CallbackResult := TGocciaFunctionBase(Callback).Call(EmptyArgs, TGocciaUndefinedLiteralValue.UndefinedValue);

          if Assigned(TGarbageCollector.Instance) then
            TGarbageCollector.Instance.AddTempRoot(CallbackResult);
          try
            if Assigned(TGocciaMicrotaskQueue.Instance) then
              TGocciaMicrotaskQueue.Instance.DrainQueue;

            if CallbackResult is TGocciaPromiseValue then
            begin
              Promise := TGocciaPromiseValue(CallbackResult);
              if Promise.State = gpsRejected then
                AssertionFailed('callback execution', 'Async callback rejected: ' + Promise.PromiseResult.ToStringLiteral.Value)
              else if Promise.State = gpsPending then
                AssertionFailed('callback execution', 'Async callback Promise still pending after microtask drain');
            end;
          finally
            if Assigned(TGarbageCollector.Instance) then
              TGarbageCollector.Instance.RemoveTempRoot(CallbackResult);
          end;
        except
          on E: Exception do
            AssertionFailed('callback execution', 'Callback threw an exception: ' + E.Message);
        end;
      end;
    end;
  finally
    EmptyArgs.Free;
  end;
end;

procedure TGocciaTestAssertions.StartTest(const ATestName: string);
begin
  FTestStats.CurrentTestName := ATestName;
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

procedure TGocciaTestAssertions.ResetCurrentTestState;
begin
  FTestStats.CurrentTestHasFailures := False;
  FTestStats.CurrentTestAssertionCount := 0;
end;

procedure TGocciaTestAssertions.AssertionPassed(const ATestName: string);
begin
  Inc(FTestStats.CurrentTestAssertionCount);
  Inc(FTestStats.TotalAssertionCount);
end;

procedure TGocciaTestAssertions.AssertionFailed(const ATestName, AMessage: string);
begin
  Inc(FTestStats.CurrentTestAssertionCount);
  Inc(FTestStats.TotalAssertionCount);
  FTestStats.CurrentTestHasFailures := True;

  if FSuppressOutput then
    Exit;

  if FTestStats.CurrentSuiteName <> '' then
    WriteLn('    ❌ ', FTestStats.CurrentTestName, ' in ', FTestStats.CurrentSuiteName, ': ', AMessage)
  else
    WriteLn('    ❌ ', FTestStats.CurrentTestName, ': ', AMessage);
end;

function TGocciaTestAssertions.Expect(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'expect', ThrowError);

  Result := TGocciaExpectationValue.Create(AArgs.GetElement(0), Self);
end;

function TGocciaTestAssertions.MockFunction(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Impl: TGocciaValue;
begin
  if AArgs.Length > 0 then
  begin
    if not (AArgs.GetElement(0) is TGocciaFunctionBase) then
    begin
      ThrowError('mock() expects a function argument or no arguments', 0, 0);
      Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
    end;
    Impl := AArgs.GetElement(0);
  end
  else
    Impl := nil;

  Result := TGocciaMockFunctionValue.Create(Impl);
end;

function TGocciaTestAssertions.SpyOn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Target: TGocciaObjectValue;
  MethodName: string;
  ExistingValue: TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 2, 'spyOn', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaObjectValue) then
  begin
    ThrowError('spyOn expects first argument to be an object', 0, 0);
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  if not (AArgs.GetElement(1) is TGocciaStringLiteralValue) then
  begin
    ThrowError('spyOn expects second argument to be a string (method name)', 0, 0);
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  Target := TGocciaObjectValue(AArgs.GetElement(0));
  MethodName := TGocciaStringLiteralValue(AArgs.GetElement(1)).Value;

  if not Target.HasProperty(MethodName) then
  begin
    ThrowError('spyOn: cannot spy on non-existent property "' + MethodName + '"', 0, 0);
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  ExistingValue := Target.GetProperty(MethodName);
  if not Assigned(ExistingValue) or not (ExistingValue is TGocciaFunctionBase) then
  begin
    ThrowError('spyOn: property "' + MethodName + '" is not a function', 0, 0);
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  Result := TGocciaMockFunctionValue.CreateSpy(Target, MethodName);
end;

function TGocciaTestAssertions.Describe(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  SuiteName: string;
  SuiteFunction: TGocciaFunctionBase;
begin
  ValidateDescribeRegistration(AArgs, 'describe', SuiteName, SuiteFunction);
  RegisterDescribeEntry(SuiteName, SuiteFunction, nil);

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTestAssertions.DescribeSkip(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  SuiteName: string;
  SuiteFunction: TGocciaFunctionBase;
begin
  ValidateDescribeRegistration(AArgs, 'describe.skip', SuiteName, SuiteFunction);
  RegisterDescribeEntry(SuiteName, SuiteFunction, nil, True);

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTestAssertions.DescribeSkipIf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'describe.skipIf', ThrowError);

  FSkipNextDescribe := AArgs.GetElement(0).ToBooleanLiteral.Value;
  FFocusNextDescribe := False;
  Result := TGocciaNativeFunctionValue.Create(DescribeConditional, 'describe', 2);
end;

function TGocciaTestAssertions.DescribeRunIf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'describe.runIf', ThrowError);

  FSkipNextDescribe := not AArgs.GetElement(0).ToBooleanLiteral.Value;
  FFocusNextDescribe := False;
  Result := TGocciaNativeFunctionValue.Create(DescribeConditional, 'describe', 2);
end;

function TGocciaTestAssertions.DescribeOnly(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  SuiteName: string;
  SuiteFunction: TGocciaFunctionBase;
begin
  ValidateDescribeRegistration(AArgs, 'describe.only', SuiteName, SuiteFunction);
  RegisterDescribeEntry(SuiteName, SuiteFunction, nil, False, True);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTestAssertions.DescribeEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'describe.each', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaArrayValue) then
    ThrowError('describe.each expects a table array', 0, 0);

  Result := TGocciaParameterizedRegistrationFunction.Create(Self,
    AArgs.GetElement(0), prtDescribe);
end;

function TGocciaTestAssertions.DescribeConditional(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  SuiteName: string;
  SuiteFunction: TGocciaFunctionBase;
begin
  ValidateDescribeRegistration(AArgs, 'describe', SuiteName, SuiteFunction);
  RegisterDescribeEntry(SuiteName, SuiteFunction, nil, FSkipNextDescribe,
    FFocusNextDescribe);
  FSkipNextDescribe := False;
  FFocusNextDescribe := False;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTestAssertions.Test(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TestName: string;
  TestFunction: TGocciaFunctionBase;
begin
  ValidateTestRegistration(AArgs, 'test', TestName, TestFunction);
  RegisterTestEntry(TestName, TestFunction, nil);

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTestAssertions.It(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // 'it' is an alias for 'test'
  Result := Test(AArgs, AThisValue);
end;

function TGocciaTestAssertions.Skip(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TestName: string;
  TestFunction: TGocciaFunctionBase;
begin
  ValidateTestRegistration(AArgs, 'test.skip', TestName, TestFunction);
  RegisterTestEntry(TestName, TestFunction, nil, True);

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTestAssertions.TestSkipIf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'test.skipIf', ThrowError);

  FSkipNextTest := AArgs.GetElement(0).ToBooleanLiteral.Value;
  FFocusNextTest := False;
  Result := TGocciaNativeFunctionValue.Create(TestConditional, 'test', 2);
end;

function TGocciaTestAssertions.TestRunIf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'test.runIf', ThrowError);

  FSkipNextTest := not AArgs.GetElement(0).ToBooleanLiteral.Value;
  FFocusNextTest := False;
  Result := TGocciaNativeFunctionValue.Create(TestConditional, 'test', 2);
end;

function TGocciaTestAssertions.TestOnly(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TestName: string;
  TestFunction: TGocciaFunctionBase;
begin
  ValidateTestRegistration(AArgs, 'test.only', TestName, TestFunction);
  RegisterTestEntry(TestName, TestFunction, nil, False, True);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTestAssertions.TestEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'test.each', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaArrayValue) then
    ThrowError('test.each expects a table array', 0, 0);

  Result := TGocciaParameterizedRegistrationFunction.Create(Self,
    AArgs.GetElement(0), prtTest);
end;

function TGocciaTestAssertions.TestTodo(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'test.todo', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowError('test.todo expects first argument to be a string', 0, 0);

  RegisterTestEntry(AArgs.GetElement(0).ToStringLiteral.Value, nil, nil, False,
    False, True);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTestAssertions.TestConditional(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TestName: string;
  TestFunction: TGocciaFunctionBase;
begin
  ValidateTestRegistration(AArgs, 'test', TestName, TestFunction);
  RegisterTestEntry(TestName, TestFunction, nil, FSkipNextTest, FFocusNextTest);
  FSkipNextTest := False;
  FFocusNextTest := False;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTestAssertions.BeforeAll(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  RegisterHook(AArgs, 'beforeAll', thBeforeAll);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTestAssertions.BeforeEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  RegisterHook(AArgs, 'beforeEach', thBeforeEach);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTestAssertions.AfterEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  RegisterHook(AArgs, 'afterEach', thAfterEach);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTestAssertions.AfterAll(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  RegisterHook(AArgs, 'afterAll', thAfterAll);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTestAssertions.OnTestFinished(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'onTestFinished', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaFunctionBase) then
    ThrowError('onTestFinished expects a function argument', 0, 0);

  AddTempRootIfNeeded(AArgs.GetElement(0));
  FOnTestFinishedCallbacks.Add(AArgs.GetElement(0));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTestAssertions.RunTests(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  StartTime: Int64;
  ResultObj: TGocciaObjectValue;
  ExitOnFirstFailure: Boolean = False;
  ShowTestResults: Boolean = True;
  Summary: string;
  FailedTestDetails: TStringList;
  FailedTestDetailsArray: TGocciaArrayValue;
  SuiteNames: TStringList;
  Param: TGocciaValue;
  Val: TGocciaValue;
  HasFocusedEntries: Boolean;
  ShouldStop: Boolean;
begin
  ResetTestStats;
  StartTime := GetNanoseconds;

  if AArgs.Length > 0 then
  begin
    Param := AArgs.GetElement(0);
    if not Param.IsPrimitive then
    begin
      Val := Param.GetProperty('exitOnFirstFailure');
      if Assigned(Val) and not (Val is TGocciaUndefinedLiteralValue) then
        ExitOnFirstFailure := Val.ToBooleanLiteral.Value
      else
        ExitOnFirstFailure := False;
      Val := Param.GetProperty('showTestResults');
      if Assigned(Val) and not (Val is TGocciaUndefinedLiteralValue) then
        ShowTestResults := Val.ToBooleanLiteral.Value
      else
        ShowTestResults := True;
    end
    else
    begin
      ExitOnFirstFailure := False;
      ShowTestResults := True;
    end;
  end;

  FailedTestDetails := TStringList.Create;
  SuiteNames := TStringList.Create;
  try
    ClearNestedRegistrations(FRootSuite);
    FCurrentRegistrationSuite := FRootSuite;
    BuildNestedRegistrations(FRootSuite, FailedTestDetails);

    HasFocusedEntries := SuiteHasSelectedEntries(FRootSuite, True);
    ShouldStop := False;
    ExecuteSuite(FRootSuite, HasFocusedEntries, ExitOnFirstFailure,
      FailedTestDetails, ShouldStop);

    CollectSuiteNames(FRootSuite, SuiteNames);

    Summary := Format('Tests: %d total, %d passed, %d failed, %d skipped',
      [FTestStats.TotalTests, FTestStats.PassedTests, FTestStats.FailedTests,
      FTestStats.SkippedTests]);

    if SuiteNames.Count > 0 then
    begin
      Summary := Summary + ' (Suites: ';
      for I := 0 to SuiteNames.Count - 1 do
      begin
        if I > 0 then
          Summary := Summary + ', ';
        Summary := Summary + SuiteNames[I];
      end;
      Summary := Summary + ')';
    end;

    FailedTestDetailsArray := TGocciaArrayValue.Create;
    if FailedTestDetails.Count > 0 then
    begin
      for I := 0 to FailedTestDetails.Count - 1 do
        FailedTestDetailsArray.Elements.Add(TGocciaStringLiteralValue.Create(
          FailedTestDetails[I]));
    end;

     // Create result object
    ResultObj := TGocciaObjectValue.Create;
    ResultObj.AssignProperty('totalTests', TGocciaNumberLiteralValue.Create(
      CountRegisteredTests(FRootSuite)));
    ResultObj.AssignProperty('totalRunTests', TGocciaNumberLiteralValue.Create(
      FTestStats.TotalTests));
    ResultObj.AssignProperty('passed', TGocciaNumberLiteralValue.Create(
      FTestStats.PassedTests));
    ResultObj.AssignProperty('failed', TGocciaNumberLiteralValue.Create(
      FTestStats.FailedTests));
    ResultObj.AssignProperty('skipped', TGocciaNumberLiteralValue.Create(
      FTestStats.SkippedTests));
    ResultObj.AssignProperty('assertions', TGocciaNumberLiteralValue.Create(
      FTestStats.TotalAssertionCount));
    ResultObj.AssignProperty('duration', TGocciaNumberLiteralValue.Create(
      GetNanoseconds - StartTime));
    ResultObj.AssignProperty('failedTests', FailedTestDetailsArray);
    ResultObj.AssignProperty('summary', TGocciaStringLiteralValue.Create(Summary));

    if ShowTestResults and not FSuppressOutput then
    begin
      WriteLn('');
      WriteLn('=== Test Results ===');
      WriteLn(Summary);
      WriteLn('Total Assertions: ', FTestStats.TotalAssertionCount);

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
          WriteLn(Format('✅ All tests passed! (%d skipped)',
            [FTestStats.SkippedTests]))
        else
          WriteLn('✅ All tests passed!');
      end
      else
        WriteLn('❌ Some tests failed!');

      WriteLn('==================');
    end;

    Result := ResultObj;
  finally
    FailedTestDetails.Free;
    SuiteNames.Free;
  end;
end;

end.
