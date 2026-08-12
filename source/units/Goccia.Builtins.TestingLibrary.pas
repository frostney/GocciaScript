unit Goccia.Builtins.TestingLibrary;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.Arguments.Validator,
  Goccia.Builtins.Base,
  Goccia.Builtins.Testing.SnapshotFormatting,
  Goccia.Builtins.Testing.Snapshots,
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
    // True when FActualValue is a promise rejection reason unwrapped by
    // .rejects, so toThrow treats it as the thrown value instead of calling it.
    FIsRejectionReason: Boolean;
    FTestAssertions: TGocciaTestAssertions; // Reference to parent
  public
    constructor Create(const AActualValue: TGocciaValue; const ATestAssertions: TGocciaTestAssertions; const AIsNegated: Boolean = False;
      const AIsRejectionReason: Boolean = False);
    procedure MarkReferences; override;

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
    function ToMatchSnapshot(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ToMatchInlineSnapshot(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    // Mock matchers
    function ToHaveBeenCalled(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToHaveBeenCalledOnce(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
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

  // Main test assertions built-in
  TGocciaTestAssertions = class(TGocciaBuiltin)
  private
    FTestStats: record
      TotalTests: Integer;
      PassedTests: Integer;
      FailedTests: Integer;
      SkippedTests: Integer;
      { Suite-level errors: a describe callback that threw during
        registration, or a failed beforeAll/afterAll hook. Vitest keeps
        these out of the test counts (the affected tests report as
        skipped) and fails the FILE instead, so they are tracked apart
        from FailedTests and surface as the `suiteErrors` field. }
      SuiteErrors: Integer;
      CurrentSuiteName: string;
      CurrentTestName: string;
      CurrentTestHasFailures: Boolean;
      { Message of the first failure recorded for the current test or
        hook. Hook/describe detail strings surface it so the reported
        payload keeps the error text both oracles print. }
      CurrentFailureMessage: string;
      CurrentTestIsSkipped: Boolean;
      CurrentTestAssertionCount: Integer;  // Assertions in current test
      TotalAssertionCount: Integer;        // Total assertions across all tests
    end;

    FRootSuite: TGocciaTestSuite;
    FCurrentRegistrationSuite: TGocciaTestSuite;
    { A describe callback threw while the file was being collected.
      Vitest discards the WHOLE file in that case -- including suites
      collected before the throwing one -- so registration stops and no
      test runs. Distinct from a hook or test failure, which are
      execution-time and leave already-collected results intact. }
    FCollectionAborted: Boolean;
    FSkipNextDescribe: Boolean;
    FFocusNextDescribe: Boolean;
    FSkipNextTest: Boolean;
    FFocusNextTest: Boolean;
    FSuppressOutput: Boolean;
    FOnTestFinishedCallbacks: TGocciaArgumentsCollection;
    FSnapshotState: TGocciaSnapshotState;
    FSnapshotFormatting: TGocciaSnapshotFormatting;

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
    { ASetupFailed: an ancestor suite's beforeAll hook threw. Its tests
      and every descendant's report as skipped, and neither beforeAll nor
      afterAll runs for those descendants. }
    procedure ExecuteSuite(const ASuite: TGocciaTestSuite;
      const AHasFocusedEntries, AExitOnFirstFailure: Boolean;
      const AFailedTestDetails: TStringList; var AShouldStop: Boolean;
      const ASetupFailed: Boolean = False);
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
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback;
      const ASnapshotHost: IGocciaSnapshotHost = nil;
      const ASnapshotUpdateMode: TGocciaSnapshotUpdateMode = sumNew;
      const ASnapshotFormatter: IGocciaSnapshotFormatter = nil;
      const AInjectGlobals: Boolean = True);
    destructor Destroy; override;

    // Main expect function
    function Expect(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ExpectAnything(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ExpectAny(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ExpectArrayContaining(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ExpectObjectContaining(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ExpectStringContaining(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ExpectStringMatching(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ExpectCloseTo(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ExpectSchemaMatching(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ExpectToBeOneOf(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ExpectToSatisfy(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ExpectToBeFasterThan(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ExpectToBeSlowerThan(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ExpectNotArrayContaining(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ExpectNotObjectContaining(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ExpectNotStringContaining(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ExpectNotStringMatching(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ExpectNotCloseTo(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ExpectNotSchemaMatching(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ExpectNotToBeOneOf(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ExpectNotToSatisfy(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ExpectNotToBeFasterThan(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ExpectNotToBeSlowerThan(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AddSnapshotSerializer(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

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

    // Exposed for native Pascal unit tests. A plain getter rather
    // than a dotted field-path accessor (read FTestStats.X) so the
    // declaration stays inside lakon's supported property surface.
    function GetCurrentTestHasFailures: Boolean;
    property CurrentTestHasFailures: Boolean read GetCurrentTestHasFailures;
    property SuppressOutput: Boolean read FSuppressOutput write FSuppressOutput;
    procedure ResetCurrentTestState;
  end;

var
  { Per-test and per-describe deadlines, in milliseconds. Set once by
    the test runner CLI before workers spawn (read-only after that, so
    a plain global is safe). 0 = no deadline at that scope. The whole
    file still has the engine's --timeout deadline (tsFile) on top. }
  GTestRunnerTestTimeoutMs: Integer = 0;
  GTestRunnerDescribeTimeoutMs: Integer = 0;

implementation

uses
  Math,
  SysUtils,

  TimingUtils,

  Goccia.Arithmetic,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Evaluator,
  Goccia.Evaluator.Comparison,
  Goccia.Execution.CallSite,
  Goccia.FetchManager,
  Goccia.FloatingPoint,
  Goccia.GarbageCollector,
  Goccia.MemoryLimit,
  Goccia.MicrotaskQueue,
  Goccia.RegExp.Runtime,
  Goccia.Timeout,
  Goccia.Utils,
  Goccia.Values.AsymmetricMatcher,
  Goccia.Values.ClassHelper,
  Goccia.Values.ClassValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.Formatting,
  Goccia.Values.HoleValue,
  Goccia.Values.MapValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.PromiseValue,
  Goccia.Values.SetValue,
  Goccia.Values.SymbolValue,
  Goccia.VM.Exception;

function SnapshotTestName(const AAssertions: TGocciaTestAssertions): string;
begin
  if AAssertions.FTestStats.CurrentSuiteName <> '' then
    Result := AAssertions.FTestStats.CurrentSuiteName + ' > ' +
      AAssertions.FTestStats.CurrentTestName
  else
    Result := AAssertions.FTestStats.CurrentTestName;
end;

function IsSnapshotPropertyShape(const AValue: TGocciaValue): Boolean;
begin
  Result := (AValue is TGocciaObjectValue) and
    (AValue.TypeOf = 'object');
end;

function MergeSnapshotPropertyShape(const AActual,
  AProperties: TGocciaValue): TGocciaValue;
var
  ActualArray, PropertiesArray, MergedArray: TGocciaArrayValue;
  ActualObject, PropertiesObject, MergedObject: TGocciaObjectValue;
  Entry: TPair<string, TGocciaValue>;
  SymbolEntry: TPair<TGocciaSymbolValue, TGocciaValue>;
  I: Integer;
  ActualProperty: TGocciaValue;
  MergedRoot: TGocciaTempRoot;
begin
  if AProperties is TGocciaAsymmetricMatcherValue then
    Exit(AProperties);

  if (AActual is TGocciaArrayValue) and
     (AProperties is TGocciaArrayValue) then
  begin
    ActualArray := TGocciaArrayValue(AActual);
    PropertiesArray := TGocciaArrayValue(AProperties);
    MergedArray := TGocciaArrayValue.Create(nil, ActualArray.Elements.Count);
    InitializeTempRoot(MergedRoot);
    Goccia.GarbageCollector.AddTempRootIfNeeded(MergedRoot, MergedArray);
    try
      for I := 0 to ActualArray.Elements.Count - 1 do
        MergedArray.Elements.Add(ActualArray.Elements[I]);
      for I := 0 to PropertiesArray.Elements.Count - 1 do
      begin
        if PropertiesArray.Elements[I] = TGocciaHoleValue.HoleValue then
          Continue;
        ActualProperty := ActualArray.GetElement(I);
        MergedArray.SetElement(I, MergeSnapshotPropertyShape(ActualProperty,
          PropertiesArray.GetElement(I)));
      end;
      Result := MergedArray;
    finally
      Goccia.GarbageCollector.RemoveTempRootIfNeeded(MergedRoot);
    end;
    Exit;
  end;

  if (AActual is TGocciaObjectValue) and (AActual.TypeOf = 'object') and
     (AProperties is TGocciaObjectValue) and
     (AProperties.TypeOf = 'object') then
  begin
    ActualObject := TGocciaObjectValue(AActual);
    PropertiesObject := TGocciaObjectValue(AProperties);
    MergedObject := TGocciaObjectValue.Create;
    InitializeTempRoot(MergedRoot);
    Goccia.GarbageCollector.AddTempRootIfNeeded(MergedRoot, MergedObject);
    try
      for Entry in ActualObject.GetEnumerablePropertyEntries do
        MergedObject.AssignProperty(Entry.Key, Entry.Value);
      for SymbolEntry in ActualObject.GetEnumerableSymbolProperties do
        MergedObject.AssignSymbolProperty(SymbolEntry.Key, SymbolEntry.Value);

      for Entry in PropertiesObject.GetEnumerablePropertyEntries do
      begin
        ActualProperty := ActualObject.GetProperty(Entry.Key);
        MergedObject.AssignProperty(Entry.Key,
          MergeSnapshotPropertyShape(ActualProperty, Entry.Value));
      end;
      Result := MergedObject;
    finally
      Goccia.GarbageCollector.RemoveTempRootIfNeeded(MergedRoot);
    end;
    Exit;
  end;

  Result := AProperties;
end;

function StripInlineSnapshotIndentation(const AValue: string): string;
var
  Lines: TStringList;
  I, J, IndentLength: Integer;
  Normalized, Indentation: string;
begin
  Normalized := StringReplace(AValue, #13#10, #10, [rfReplaceAll]);
  Normalized := StringReplace(Normalized, #13, #10, [rfReplaceAll]);
  Lines := TStringList.Create;
  try
    Lines.Text := Normalized;
    { TStringList.Text consumes a terminal line break instead of retaining
      the empty closing line. Top-level inline snapshots end exactly in LF,
      so restore that structural line before applying Vitest's dedent. }
    if (Normalized <> '') and (Normalized[Length(Normalized)] = #10) then
      Lines.Add('');
    if Lines.Count <= 2 then
      Exit(Normalized);
    if (Trim(Lines[0]) <> '') or
       (Trim(Lines[Lines.Count - 1]) <> '') then
      Exit(Normalized);

    Indentation := '';
    for I := 1 to Lines.Count - 2 do
      if Trim(Lines[I]) <> '' then
      begin
        J := 1;
        while (J <= Length(Lines[I])) and (Lines[I][J] in [' ', #9]) do
          Inc(J);
        Indentation := Copy(Lines[I], 1, J - 1);
        Break;
      end;
    if Indentation = '' then
      Exit(Normalized);

    IndentLength := Length(Indentation);
    for I := 1 to Lines.Count - 2 do
      if Lines[I] <> '' then
      begin
        if Copy(Lines[I], 1, IndentLength) <> Indentation then
          Exit(Normalized);
        Lines[I] := Copy(Lines[I], IndentLength + 1, MaxInt);
      end;
    Lines[Lines.Count - 1] := '';

    Result := '';
    for I := 0 to Lines.Count - 1 do
    begin
      if I > 0 then
        Result := Result + #10;
      Result := Result + Lines[I];
    end;
  finally
    Lines.Free;
  end;
end;

{ toHaveProperty accepts a dotted/bracketed path string ("items[0].type") or
  an array of segments (["a", "b", 0, "c"]). The array form is also the escape
  hatch for keys that themselves contain a dot, since a string path always
  splits on dots.

  A string path drops every empty segment, so "a.", ".a" and "a..b" all parse
  the same as "a" / "a" / "a.b". That matches vitest, whose path grammar
  (chai's parsePath, `str.match(/(\\\.|[^.]+?)+/g)`) can never produce an empty
  segment. An empty-string key is therefore unreachable through a string path;
  it is reached by the literal-key lookup in ToHaveProperty (for the whole path)
  or by an array path (for a nested one). }
function ParsePropertyPath(const APathValue: TGocciaValue): TArray<string>;
var
  PathArray: TGocciaArrayValue;
  Path: string;
  Current: string;
  Bracket: string;
  Position: Integer;
  I: Integer;

  procedure AppendSegment(const ASegment: string);
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := ASegment;
  end;

begin
  SetLength(Result, 0);

  if APathValue is TGocciaArrayValue then
  begin
    PathArray := TGocciaArrayValue(APathValue);
    for I := 0 to PathArray.Elements.Count - 1 do
      AppendSegment(PathArray.GetElement(I).ToStringLiteral.Value);
    Exit;
  end;

  Path := APathValue.ToStringLiteral.Value;
  Current := '';
  Position := 1;
  while Position <= Length(Path) do
  begin
    if Path[Position] = '.' then
    begin
      // Empty segments are dropped, so a leading, doubled or trailing
      // separator never contributes a key.
      if Current <> '' then
        AppendSegment(Current);
      Current := '';
      Inc(Position);
    end
    else if Path[Position] = '[' then
    begin
      if Current <> '' then
        AppendSegment(Current);
      Current := '';
      Inc(Position);
      Bracket := '';
      while (Position <= Length(Path)) and (Path[Position] <> ']') do
      begin
        Bracket := Bracket + Path[Position];
        Inc(Position);
      end;
      if Position <= Length(Path) then
        Inc(Position);
      AppendSegment(Bracket);
      if (Position <= Length(Path)) and (Path[Position] = '.') then
        Inc(Position);
    end
    else
    begin
      Current := Current + Path[Position];
      Inc(Position);
    end;
  end;

  if Current <> '' then
    AppendSegment(Current);
end;

function DescribePropertyPath(const ASegments: TArray<string>): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(ASegments) do
  begin
    if I > 0 then
      Result := Result + '.';
    Result := Result + ASegments[I];
  end;
end;

{ Walks the path one segment at a time. Each step uses the same prototype-aware
  lookup a plain property read would, so inherited members resolve.

  Every step can run user code — an accessor on any segment is a getter call and
  therefore a GC safe point — while the only reference to the previous step's
  result is the local `Current`, and the only reference to a freshly boxed
  primitive wrapper is the local `Container`. Both are rooted for as long as they
  are live. The nest-safe TGocciaTempRoot form is used because the roots are
  re-pointed on every iteration: re-adding releases the previous target, and the
  finally releases whatever the last iteration left rooted. }
function TryResolvePropertyPath(const ARoot: TGocciaValue;
  const ASegments: TArray<string>; out AValue: TGocciaValue): Boolean;
var
  Current: TGocciaValue;
  Container: TGocciaObjectValue;
  ContainerRoot: TGocciaTempRoot;
  CurrentRoot: TGocciaTempRoot;
  I: Integer;
begin
  Result := False;
  AValue := TGocciaUndefinedLiteralValue.UndefinedValue;
  Current := ARoot;

  InitializeTempRoot(ContainerRoot);
  InitializeTempRoot(CurrentRoot);
  try
    for I := 0 to High(ASegments) do
    begin
      if Current is TGocciaObjectValue then
        Container := TGocciaObjectValue(Current)
      else
      begin
        // A primitive still exposes its wrapper's members ("a.length" on a
        // string); null and undefined box to nil and end the walk. The wrapper
        // is created here and reachable from nowhere else, so root it before
        // the lookup that may collect it.
        Container := Current.Box;
        if not Assigned(Container) then
          Exit;
        Goccia.GarbageCollector.AddTempRootIfNeeded(ContainerRoot, Container);
      end;

      if not Container.HasProperty(ASegments[I]) then
        Exit;
      Current := Container.GetPropertyWithContext(ASegments[I], Current);
      if Current = nil then
        Current := TGocciaUndefinedLiteralValue.UndefinedValue;
      Goccia.GarbageCollector.AddTempRootIfNeeded(CurrentRoot, Current);
    end;

    AValue := Current;
    Result := True;
  finally
    Goccia.GarbageCollector.RemoveTempRootIfNeeded(CurrentRoot);
    Goccia.GarbageCollector.RemoveTempRootIfNeeded(ContainerRoot);
  end;
end;

function IsNativeFunctionInstanceOf(const AObj: TGocciaObjectValue;
  const AConstructor: TGocciaNativeFunctionValue): Boolean;
var
  ConstructorProto: TGocciaValue;
  CurrentProto: TGocciaObjectValue;
begin
  Result := False;
  ConstructorProto := AConstructor.GetProperty(PROP_PROTOTYPE);
  if not (ConstructorProto is TGocciaObjectValue) then
    Exit;
  CurrentProto := AObj.Prototype;
  while Assigned(CurrentProto) do
  begin
    if CurrentProto = TGocciaObjectValue(ConstructorProto) then
    begin
      Result := True;
      Exit;
    end;
    CurrentProto := CurrentProto.Prototype;
  end;
end;

{ The message a thrown value contributes to string and RegExp comparisons.
  Jest reads the "message" property of Error-like values and falls back to the
  string form of anything else, so `throw 42` matches toThrow('42'). }
function ThrownValueMessage(const AValue: TGocciaValue): string;
var
  MessageValue: TGocciaValue;
begin
  if AValue is TGocciaObjectValue then
  begin
    MessageValue := TGocciaObjectValue(AValue).GetProperty(PROP_MESSAGE);
    if MessageValue is TGocciaStringLiteralValue then
      Exit(TGocciaStringLiteralValue(MessageValue).Value);
  end;

  if AValue is TGocciaSymbolValue then
    Exit(TGocciaSymbolValue(AValue).ToDisplayString.Value);

  Result := AValue.ToStringLiteral.Value;
end;

{ Errors carry non-enumerable fields and would otherwise render as an empty
  object, so describe them by name and message in assertion output. }
function DescribeThrowValue(const AValue: TGocciaValue): string;
var
  NameValue: TGocciaValue;
  MessageValue: TGocciaValue;
begin
  if AValue is TGocciaObjectValue then
  begin
    NameValue := TGocciaObjectValue(AValue).GetProperty(PROP_NAME);
    MessageValue := TGocciaObjectValue(AValue).GetProperty(PROP_MESSAGE);
    if (NameValue is TGocciaStringLiteralValue) and
       (MessageValue is TGocciaStringLiteralValue) then
      Exit(TGocciaStringLiteralValue(NameValue).Value + ': ' +
        TGocciaStringLiteralValue(MessageValue).Value);
  end;

  Result := FormatForDisplay(AValue);
end;

{ The JavaScript error name whose prototype an engine-level Pascal error maps
  onto, so a rebuilt error object matches the same constructors a JS throw of
  the equivalent error would. }
function EngineErrorName(const AError: TGocciaError): string;
begin
  if AError is TGocciaTypeError then
    Result := TYPE_ERROR_NAME
  else if AError is TGocciaReferenceError then
    Result := REFERENCE_ERROR_NAME
  else if AError is TGocciaSyntaxError then
    Result := SYNTAX_ERROR_NAME
  else
    Result := ERROR_NAME;
end;

{ The subject the string and RegExp forms match against. Vitest reads the
  thrown value's message, falling back to the thrown value itself only when it
  is already a string; a thrown number, boolean or message-less object offers
  no subject and matches nothing. A thrown null or undefined is the one shape
  Vitest lets match anything. }
type
  TGocciaThrowSubject = (tsAnything, tsText, tsNothing);

function ThrownMessageSubject(const AValue: TGocciaValue;
  out AText: string): TGocciaThrowSubject;
var
  MessageValue: TGocciaValue;
begin
  AText := '';

  if (AValue = nil) or (AValue is TGocciaUndefinedLiteralValue) or
     (AValue is TGocciaNullLiteralValue) then
    Exit(tsAnything);

  if AValue is TGocciaObjectValue then
  begin
    MessageValue := TGocciaObjectValue(AValue).GetProperty(PROP_MESSAGE);
    if MessageValue is TGocciaStringLiteralValue then
    begin
      AText := TGocciaStringLiteralValue(MessageValue).Value;
      Exit(tsText);
    end;
    Exit(tsNothing);
  end;

  if AValue is TGocciaStringLiteralValue then
  begin
    AText := TGocciaStringLiteralValue(AValue).Value;
    Exit(tsText);
  end;

  Result := tsNothing;
end;

{ toThrow accepts a substring, a RegExp, an error constructor, an Error
  instance, or an asymmetric matcher. Anything else is a usage error. }
function IsSupportedThrowExpectation(const AValue: TGocciaValue): Boolean;
begin
  Result := (AValue is TGocciaStringLiteralValue) or
    (AValue is TGocciaObjectValue) or AValue.IsCallable;
end;

function ThrowExpectationDescription(const AValue: TGocciaValue): string;
begin
  if IsRegExpInstance(AValue) then
    Result := RegExpObjectToString(AValue)
  else if AValue is TGocciaClassValue then
    Result := TGocciaClassValue(AValue).Name
  else if AValue is TGocciaNativeFunctionValue then
    Result := TGocciaNativeFunctionValue(AValue).Name
  else
    Result := DescribeThrowValue(AValue);
end;

{ Applies the argument form the expectation was given:
  - string: the thrown message contains it
  - RegExp: the thrown message matches it
  - constructor: the thrown value is an instance of it, subclasses included
  - Error instance: the messages are equal
  - asymmetric matcher: delegated to the matcher }
function ThrownValueMatchesExpectation(const AThrownValue,
  AExpected: TGocciaValue): Boolean;
var
  ExpectedSubstring: string;
  Subject: string;
  MatchValue: TGocciaValue;
  MatchIndex: Integer;
  MatchEnd: Integer;
  NextIndex: Integer;
begin
  if AExpected is TGocciaAsymmetricMatcherValue then
    Exit(IsDeepEqual(AThrownValue, AExpected));

  if (AExpected is TGocciaStringLiteralValue) or
     IsRegExpInstance(AExpected) then
  begin
    case ThrownMessageSubject(AThrownValue, Subject) of
      tsAnything: Exit(True);
      tsNothing: Exit(False);
    end;

    if IsRegExpInstance(AExpected) then
      Exit(MatchRegExpObject(AExpected, Subject, 0, False, False, MatchValue,
        MatchIndex, MatchEnd, NextIndex));

    ExpectedSubstring := TGocciaStringLiteralValue(AExpected).Value;
    { An empty expected string asserts an empty message rather than matching
      everything, the way Vitest compiles it to /^$/. }
    if ExpectedSubstring = '' then
      Exit(Subject = '');
    Exit(Pos(ExpectedSubstring, Subject) > 0);
  end;

  // ES2026 §13.10.2 InstanceofOperator(value, target) — the same prototype
  // walk the instanceof operator uses, so `class Derived extends Error {}`
  // satisfies both toThrow(Derived) and toThrow(Error).
  if AExpected.IsCallable then
    Exit(InstanceofOperatorResult(AThrownValue, AExpected));

  { An expected error instance is compared as a value: name, message, own
    enumerable properties and an expected-side cause all participate, so a
    TypeError never satisfies an expected plain Error. }
  Result := IsDeepEqual(AThrownValue, AExpected);
end;

function FormatThrowValueDetail(const AValue: TGocciaValue): string;
var
  MsgValue, StackValue: TGocciaValue;
begin
  if AValue is TGocciaObjectValue then
  begin
    MsgValue := TGocciaObjectValue(AValue).GetProperty(PROP_MESSAGE);
    StackValue := TGocciaObjectValue(AValue).GetProperty(PROP_STACK);
    if Assigned(StackValue) and (StackValue is TGocciaStringLiteralValue) and
       (TGocciaStringLiteralValue(StackValue).Value <> '') then
      Result := TGocciaStringLiteralValue(StackValue).Value
    else if Assigned(MsgValue) and (MsgValue is TGocciaStringLiteralValue) then
      Result := TGocciaStringLiteralValue(MsgValue).Value
    else
      Result := FormatForDisplay(AValue);
  end
  else
    Result := FormatForDisplay(AValue);
end;

{ TGocciaTestSuite }

procedure AddTempRootIfNeeded(const AValue: TGocciaValue);
begin
  if (TGarbageCollector.Instance <> nil) and Assigned(AValue) then
    TGarbageCollector.Instance.AddTempRoot(AValue);
end;

procedure RemoveTempRootIfNeeded(const AValue: TGocciaValue);
begin
  if (TGarbageCollector.Instance <> nil) and Assigned(AValue) then
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
    Goccia.Values.ErrorHelper.ThrowTypeError(Format(SErrorFunctionExpectsStringFirst,
      [GetFunctionName]), SSuggestTestUsage);

  if not (AArguments.GetElement(1) is TGocciaFunctionBase) then
    Goccia.Values.ErrorHelper.ThrowTypeError(Format(SErrorFunctionExpectsFunctionSecond,
      [GetFunctionName]), SSuggestTestUsage);

  if not (FTable is TGocciaArrayValue) then
    Goccia.Values.ErrorHelper.ThrowTypeError(Format(SErrorFunctionExpectsTableArray,
      [GetFunctionName]), SSuggestTestUsage);

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

{ The actual value is held in a native field, not as a property, so the
  inherited walk cannot see it. `expect(<expression>)` is the common case where
  nothing else refers to it — a literal or call result, alive only because this
  expectation holds it — and every matcher then re-enters user code through
  getters on the expected side before it is done reading the actual. Without
  this the value is collected mid-assertion. FTestAssertions is a builtin, not a
  collected value, so it is deliberately not marked. }
procedure TGocciaExpectationValue.MarkReferences;
begin
  inherited;
  if Assigned(FActualValue) then
    FActualValue.MarkReferences;
end;

constructor TGocciaExpectationValue.Create(const AActualValue: TGocciaValue; const ATestAssertions: TGocciaTestAssertions; const AIsNegated: Boolean;
  const AIsRejectionReason: Boolean);
begin
  inherited Create;
  FActualValue := AActualValue;
  FTestAssertions := ATestAssertions;
  FIsNegated := AIsNegated;
  FIsRejectionReason := AIsRejectionReason;

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
    TGocciaNativeFunctionValue.Create(ToHaveProperty, 'toHaveProperty', 2), [pfConfigurable, pfWritable]));
  DefineProperty('toThrow', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToThrow, 'toThrow', 1), [pfConfigurable, pfWritable]));
  DefineProperty('toBeCloseTo', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToBeCloseTo, 'toBeCloseTo', 2), [pfConfigurable, pfWritable]));
  DefineProperty('toMatchSnapshot', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToMatchSnapshot, 'toMatchSnapshot', 2),
    [pfConfigurable, pfWritable]));
  DefineProperty('toMatchInlineSnapshot', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToMatchInlineSnapshot,
      'toMatchInlineSnapshot', 3), [pfConfigurable, pfWritable]));

  // Mock matchers
  DefineProperty('toHaveBeenCalled', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToHaveBeenCalled, 'toHaveBeenCalled', 0), [pfConfigurable, pfWritable]));
  DefineProperty('toHaveBeenCalledOnce', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(ToHaveBeenCalledOnce,
      'toHaveBeenCalledOnce', 0), [pfConfigurable, pfWritable]));
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
        'Expected ' + FormatForDisplay(FActualValue) + ' not to be ' + FormatForDisplay(Expected))
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBe',
        'Expected ' + FormatForDisplay(FActualValue) + ' to be ' + FormatForDisplay(Expected));
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
        'Expected ' + FormatForDisplay(FActualValue) + ' not to equal ' + FormatForDisplay(Expected))
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toEqual',
        'Expected ' + FormatForDisplay(FActualValue) + ' to equal ' + FormatForDisplay(Expected));
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToContainEqual(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  I: Integer;
  SetCursor: Integer;
  SetItem: TGocciaValue;
  Contains: Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'toContainEqual', FTestAssertions.ThrowError);

  if not ((FActualValue is TGocciaArrayValue) or
     (FActualValue is TGocciaSetValue)) then
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionPassed('toContainEqual')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toContainEqual',
        'Expected an array or a Set but received ' +
        FormatForDisplay(FActualValue));
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  Expected := AArgs.GetElement(0);
  Contains := False;
  if FActualValue is TGocciaSetValue then
  begin
    SetCursor := 0;
    TGocciaSetValue(FActualValue).RetainIterator;
    try
      while TGocciaSetValue(FActualValue).NextItem(SetCursor, SetItem) do
        if IsDeepEqual(SetItem, Expected) then
        begin
          Contains := True;
          Break;
        end;
    finally
      TGocciaSetValue(FActualValue).ReleaseIterator;
    end;
  end
  else
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
        'Expected ' + FormatForDisplay(FActualValue) + ' not to contain equal ' + FormatForDisplay(Expected))
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toContainEqual',
        'Expected ' + FormatForDisplay(FActualValue) + ' to contain equal ' + FormatForDisplay(Expected));
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
  IsEqual := IsStrictDeepEqual(FActualValue, Expected);

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
        'Expected ' + FormatForDisplay(FActualValue) + ' not to strictly equal ' + FormatForDisplay(Expected))
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toStrictEqual',
        'Expected ' + FormatForDisplay(FActualValue) + ' to strictly equal ' + FormatForDisplay(Expected));
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
        'Expected an object but received ' + FormatForDisplay(FActualValue));
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  if not (Expected is TGocciaObjectValue) then
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionPassed('toMatchObject')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toMatchObject',
        'Expected a match object but received ' + FormatForDisplay(Expected));
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
        'Expected ' + FormatForDisplay(FActualValue) + ' not to match object ' + FormatForDisplay(Expected))
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toMatchObject',
        'Expected ' + FormatForDisplay(FActualValue) + ' to match object ' + FormatForDisplay(Expected));
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
        'Expected a string but received ' + FormatForDisplay(FActualValue));
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  Expected := AArgs.GetElement(0);
  if not (Expected is TGocciaStringLiteralValue) and not IsRegExpInstance(Expected) then
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionPassed('toMatch')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toMatch',
        'Expected a string pattern or RegExp but received ' +
        FormatForDisplay(Expected));
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  ActualString := FormatForDisplay(FActualValue);
  if IsRegExpInstance(Expected) then
  begin
    ExpectedDescription := RegExpObjectToString(Expected);
    Matches := MatchRegExpObject(Expected, ActualString, 0, False, False,
      MatchValue, MatchIndex, MatchEnd, NextIndex);
  end
  else
  begin
    ExpectedDescription := FormatForDisplay(Expected);
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
        'Expected ' + FormatForDisplay(FActualValue) + ' not to match ' +
        ExpectedDescription)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toMatch',
        'Expected ' + FormatForDisplay(FActualValue) + ' to match ' +
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
        'Expected ' + FormatForDisplay(FActualValue) + ' not to be null')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeNull',
        'Expected ' + FormatForDisplay(FActualValue) + ' to be null');
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
        'Expected ' + FormatForDisplay(FActualValue) + ' not to be NaN')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeNaN',
        'Expected ' + FormatForDisplay(FActualValue) + ' to be NaN');
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
        'Expected ' + FormatForDisplay(FActualValue) + ' not to be undefined')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeUndefined',
        'Expected ' + FormatForDisplay(FActualValue) + ' to be undefined');
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
        'Expected ' + FormatForDisplay(FActualValue) + ' not to be defined')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeDefined',
        'Expected ' + FormatForDisplay(FActualValue) + ' to be defined');
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
        'Expected ' + FormatForDisplay(FActualValue) + ' not to be truthy')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeTruthy',
        'Expected ' + FormatForDisplay(FActualValue) + ' to be truthy');
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
        'Expected ' + FormatForDisplay(FActualValue) + ' not to be falsy')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeFalsy',
        'Expected ' + FormatForDisplay(FActualValue) + ' to be falsy');
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
        'Expected ' + FormatForDisplay(FActualValue) + ' not to be greater than ' + FormatForDisplay(Expected))
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeGreaterThan',
        'Expected ' + FormatForDisplay(FActualValue) + ' to be greater than ' + FormatForDisplay(Expected));
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
        'Expected ' + FormatForDisplay(FActualValue) + ' not to be greater than or equal to ' + FormatForDisplay(Expected))
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeGreaterThanOrEqual',
        'Expected ' + FormatForDisplay(FActualValue) + ' to be greater than or equal to ' + FormatForDisplay(Expected));
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
        'Expected ' + FormatForDisplay(FActualValue) + ' not to be less than ' + FormatForDisplay(Expected))
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeLessThan',
        'Expected ' + FormatForDisplay(FActualValue) + ' to be less than ' + FormatForDisplay(Expected));
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
        'Expected ' + FormatForDisplay(FActualValue) + ' not to be less than or equal to ' + FormatForDisplay(Expected))
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeLessThanOrEqual',
        'Expected ' + FormatForDisplay(FActualValue) + ' to be less than or equal to ' + FormatForDisplay(Expected));
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
    ActualStr := FormatForDisplay(FActualValue);
    ExpectedStr := FormatForDisplay(Expected);
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
        'Expected ' + FormatForDisplay(FActualValue) + ' not to contain ' + FormatForDisplay(Expected))
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toContain',
        'Expected ' + FormatForDisplay(FActualValue) + ' to contain ' + FormatForDisplay(Expected));
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
    end
    else if FActualValue is TGocciaObjectValue then
    begin
      // General prototype-chain walk for native function constructors
      // (e.g. Intl.Collator, URL, Map, etc.)
      IsInstance := IsNativeFunctionInstanceOf(
        TGocciaObjectValue(FActualValue),
        TGocciaNativeFunctionValue(ExpectedConstructor));
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
        'Expected ' + FormatForDisplay(FActualValue) + ' not to be an instance of ' + FormatForDisplay(ExpectedConstructor))
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeInstanceOf',
        'Expected ' + FormatForDisplay(FActualValue) + ' to be an instance of ' + FormatForDisplay(ExpectedConstructor));
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
  // A Set and a Map report their entry count as size rather than length.
  else if FActualValue is TGocciaSetValue then
  begin
    HasLength := TGocciaSetValue(FActualValue).Count = Expected.ToNumberLiteral.Value;
  end
  else if FActualValue is TGocciaMapValue then
  begin
    HasLength := TGocciaMapValue(FActualValue).Count = Expected.ToNumberLiteral.Value;
  end
  else if FActualValue is TGocciaObjectValue then
  begin
    HasLength := Length(TGocciaObjectValue(FActualValue).GetAllPropertyNames) = Expected.ToNumberLiteral.Value;
  end
  else if FActualValue is TGocciaStringLiteralValue then
  begin
    HasLength := Length(FormatForDisplay(FActualValue)) = Expected.ToNumberLiteral.Value;
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
        'Expected ' + FormatForDisplay(FActualValue) + ' not to have length ' + FormatForDisplay(Expected))
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveLength',
        'Expected ' + FormatForDisplay(FActualValue) + ' to have length ' + FormatForDisplay(Expected));
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToHaveProperty(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  HasProperty: Boolean;
  Segments: TArray<string>;
  PathDescription: string;
  PathArgument: TGocciaValue;
  ResolvedValue: TGocciaValue;
  ResolvedRoot: TGocciaTempRoot;
  ExpectedValue: TGocciaValue;
  ExpectedRoot: TGocciaTempRoot;
  ExpectsValue: Boolean;
  LiteralKey: string;
  HasLiteralKey: Boolean;
begin
  TGocciaArgumentValidator.RequireBetween(AArgs, 1, 2, 'toHaveProperty',
    FTestAssertions.ThrowError);

  if not (FActualValue is TGocciaObjectValue) then
  begin
    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionPassed('toHaveProperty')
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveProperty',
        'Expected an object but received ' + FormatForDisplay(FActualValue));
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  PathArgument := AArgs.GetElement(0);
  if not ((PathArgument is TGocciaStringLiteralValue) or
     (PathArgument is TGocciaNumberLiteralValue) or
     (PathArgument is TGocciaArrayValue)) then
  begin
    ThrowTypeError(SErrorToHavePropertyExpectsPath, SSuggestTestUsage);
    Exit;
  end;

  // A string or number path is first tried as a single literal own key, and only
  // split into segments if that key is absent. Vitest does the same
  // (`Object.prototype.hasOwnProperty.call(actual, path)` ahead of its path
  // walk), which is what makes `expect({ "a.b": 5 }).toHaveProperty("a.b")` pass
  // and what lets a number reach an index without any dedicated numeric path
  // support. The check is deliberately own-only and ignores enumerability, so an
  // inherited "a.b" is not found this way but a non-enumerable own one is. It
  // also applies to the whole path only — a dotted key nested deeper in the
  // object still needs an array path.
  HasLiteralKey := (PathArgument is TGocciaStringLiteralValue) or
    (PathArgument is TGocciaNumberLiteralValue);
  if HasLiteralKey then
    LiteralKey := PathArgument.ToStringLiteral.Value
  else
    LiteralKey := '';

  Segments := ParsePropertyPath(PathArgument);
  PathDescription := DescribePropertyPath(Segments);
  ExpectsValue := AArgs.Length = 2;

  // The path walk calls accessors, and every accessor is a GC safe point. The
  // expected value lives only in the argument collection, which is not itself a
  // root, so root it before the walk can collect it.
  InitializeTempRoot(ExpectedRoot);
  InitializeTempRoot(ResolvedRoot);
  try
    if ExpectsValue then
    begin
      ExpectedValue := AArgs.GetElement(1);
      Goccia.GarbageCollector.AddTempRootIfNeeded(ExpectedRoot, ExpectedValue);
    end
    else
      ExpectedValue := TGocciaUndefinedLiteralValue.UndefinedValue;

    if HasLiteralKey and
       TGocciaObjectValue(FActualValue).HasOwnProperty(LiteralKey) then
    begin
      // Reading the key runs any accessor on it, exactly like a path step.
      ResolvedValue := TGocciaObjectValue(FActualValue)
        .GetPropertyWithContext(LiteralKey, FActualValue);
      if ResolvedValue = nil then
        ResolvedValue := TGocciaUndefinedLiteralValue.UndefinedValue;
      PathDescription := LiteralKey;
      HasProperty := True;
    end
    else if Length(Segments) = 0 then
    begin
      // Every segment of the path was empty ("", ".", ".."), and no own key
      // spells it literally, so there is nothing to resolve. Vitest throws a
      // TypeError out of its path parser here; reporting a plain assertion
      // failure is the same verdict without the crash.
      ResolvedValue := TGocciaUndefinedLiteralValue.UndefinedValue;
      HasProperty := False;
    end
    else
      HasProperty := TryResolvePropertyPath(FActualValue, Segments, ResolvedValue);

    // The walk releases its own roots on return, and a resolved value produced
    // by an accessor (or by a computed member such as `length`) is reachable
    // from nowhere else. The comparison and the failure formatting below both
    // allocate, so root it for the rest of the matcher.
    Goccia.GarbageCollector.AddTempRootIfNeeded(ResolvedRoot, ResolvedValue);

    if HasProperty and ExpectsValue then
      HasProperty := IsDeepEqual(ResolvedValue, ExpectedValue);

    if FIsNegated then
      HasProperty := not HasProperty;

    if HasProperty then
    begin
      TGocciaTestAssertions(FTestAssertions).AssertionPassed('toHaveProperty');
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;

    if ExpectsValue then
    begin
      if FIsNegated then
        TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveProperty',
          'Expected ' + FormatForDisplay(FActualValue) + ' not to have property ' +
          PathDescription + ' with value ' +
          FormatForDisplay(ExpectedValue))
      else
        TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveProperty',
          'Expected ' + FormatForDisplay(FActualValue) + ' to have property ' +
          PathDescription + ' with value ' +
          FormatForDisplay(ExpectedValue) + ' but received ' +
          FormatForDisplay(ResolvedValue));
    end
    else if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveProperty',
        'Expected ' + FormatForDisplay(FActualValue) + ' not to have property ' +
        PathDescription)
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveProperty',
        'Expected ' + FormatForDisplay(FActualValue) + ' to have property ' +
        PathDescription);

    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  finally
    Goccia.GarbageCollector.RemoveTempRootIfNeeded(ResolvedRoot);
    Goccia.GarbageCollector.RemoveTempRootIfNeeded(ExpectedRoot);
  end;
end;

function TGocciaExpectationValue.ToThrow(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  HasExpectation: Boolean;
  ThrownValue: TGocciaValue;
  DidThrow: Boolean;
  Matches: Boolean;
  EmptyArgs: TGocciaArgumentsCollection;
  TestFunction: TGocciaFunctionBase;
  SubjectDescription: string;
  ExpectedDescription: string;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  Expected := nil;
  // toThrow(undefined) is the no-argument form, matching Jest.
  HasExpectation := (AArgs.Length > 0) and
    not (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue);
  if HasExpectation then
  begin
    Expected := AArgs.GetElement(0);
    if not IsSupportedThrowExpectation(Expected) then
    begin
      ThrowTypeError(SErrorToThrowExpectsMatchableValue, SSuggestTestUsage);
      Exit;
    end;
  end;

  ThrownValue := nil;
  DidThrow := False;

  if FIsRejectionReason then
  begin
    // .rejects already unwrapped the rejection reason for us.
    DidThrow := True;
    ThrownValue := FActualValue;
  end
  else
  begin
    if not (FActualValue is TGocciaFunctionBase) then
    begin
      ThrowTypeError(SErrorToThrowExpectsFunction, SSuggestTestUsage);
      Exit;
    end;

    TestFunction := TGocciaFunctionBase(FActualValue);
    EmptyArgs := TGocciaArgumentsCollection.Create;
    try
      try
        TestFunction.Call(EmptyArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
      except
        on E: TGocciaThrowValue do
        begin
          DidThrow := True;
          ThrownValue := E.Value;
        end;
        on E: EGocciaBytecodeThrow do
        begin
          // The bytecode VM reports a JS throw with its own exception type.
          DidThrow := True;
          ThrownValue := E.ThrownValue;
        end;
        on E: TGocciaError do
        begin
          // Engine-level errors carry no JavaScript value; rebuild one so the
          // constructor and message forms see the same shape as a JS throw.
          DidThrow := True;
          ThrownValue := CreateErrorObject(EngineErrorName(E), E.Message);
        end;
        { An expired per-test/per-describe/per-file deadline is not something
          the callable threw, so it must never satisfy the expectation. Listed
          before the generic arm because TGocciaTimeoutError descends from
          Exception: absorbing it here would report toThrow as passing and let
          execution continue past the limit instead of unwinding to
          ExecuteSuite (see the same guard at RunCallbacks). }
        on E: TGocciaTimeoutError do
          raise;
        { A refused allocation is the same shape of event as an expired
          deadline: absorbing it would let `expect(fn).toThrow()` report the
          memory ceiling as a satisfied expectation, which is precisely the
          "catch the limit and keep going" the budget exists to prevent. }
        on E: TGocciaMemoryLimitError do
          raise;
        on E: Exception do
        begin
          DidThrow := True;
          ThrownValue := CreateErrorObject(ERROR_NAME,
            E.ClassName + ': ' + E.Message);
        end;
      end;
    finally
      EmptyArgs.Free;
    end;
  end;

  if DidThrow and (TGarbageCollector.Instance <> nil) then
    TGarbageCollector.Instance.AddTempRoot(ThrownValue);
  try
    Matches := DidThrow and
      (not HasExpectation or ThrownValueMatchesExpectation(ThrownValue, Expected));

    if FIsNegated then
      Matches := not Matches;

    if Matches then
    begin
      TGocciaTestAssertions(FTestAssertions).AssertionPassed('toThrow');
      Exit;
    end;

    if FIsRejectionReason then
      SubjectDescription := 'the rejected promise'
    else
      SubjectDescription := 'the function';

    if HasExpectation then
      ExpectedDescription := ' to throw ' + ThrowExpectationDescription(Expected)
    else
      ExpectedDescription := ' to throw an exception';

    if FIsNegated then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toThrow',
        'Expected ' + SubjectDescription + ' not' + ExpectedDescription +
        ' but it threw: ' + DescribeThrowValue(ThrownValue))
    else if DidThrow then
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toThrow',
        'Expected ' + SubjectDescription + ExpectedDescription +
        ' but it threw: ' + DescribeThrowValue(ThrownValue))
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toThrow',
        'Expected ' + SubjectDescription + ExpectedDescription +
        ' but it did not throw');
  finally
    if DidThrow and (TGarbageCollector.Instance <> nil) then
      TGarbageCollector.Instance.RemoveTempRoot(ThrownValue);
  end;
end;

function TGocciaExpectationValue.ToBeCloseTo(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Expected: TGocciaValue;
  Precision: Double;
  ActualNum, ExpectedNum, Diff, Tolerance: Double;
  IsClose: Boolean;
  ActualTempNum, ExpectedTempNum: TGocciaNumberLiteralValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'toBeCloseTo', FTestAssertions.ThrowError);

  Expected := AArgs.GetElement(0);

  // Default precision to 2 decimal places if not specified
  if AArgs.Length >= 2 then
    Precision := AArgs.GetElement(1).ToNumberLiteral.Value
  else
    Precision := 2;
  if Math.IsNaN(Precision) then
    Precision := 0;

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
      if Math.IsInfinite(Precision) then
      begin
        if Precision > 0 then
          Tolerance := 0
        else
          Tolerance := Math.Infinity;
      end
      else if Precision > 308 then
        Tolerance := 0
      else if Precision < -308 then
        Tolerance := Math.Infinity
      else
        Tolerance := 0.5 * Math.Power(10, -Precision);
      Diff := Abs(ActualNum - ExpectedNum);
      if Math.IsInfinite(Tolerance) then
        IsClose := True
      else
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
        Format('Expected %s not to be close to %s (precision: %s)',
               [FormatForDisplay(FActualValue), FormatForDisplay(Expected),
                FormatDouble(Precision)]))
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toBeCloseTo',
        Format('Expected %s to be close to %s (precision: %s)',
               [FormatForDisplay(FActualValue), FormatForDisplay(Expected),
                FormatDouble(Precision)]));
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaExpectationValue.ToMatchSnapshot(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Properties, FormatValue, Arg: TGocciaValue;
  Hint, Received, FailureMessage: string;
  HasProperties, PropertyMatched, Passed: Boolean;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FIsNegated then
  begin
    FTestAssertions.ThrowError(
      'toMatchSnapshot cannot be used with "not"', 0, 0);
    Exit;
  end;
  if not Assigned(FTestAssertions.FSnapshotState) then
  begin
    FTestAssertions.ThrowError(
      'toMatchSnapshot is only available when a snapshot host is installed',
      0, 0);
    Exit;
  end;

  Properties := nil;
  HasProperties := False;
  Hint := '';
  if AArgs.Length > 0 then
  begin
    Arg := AArgs.GetElement(0);
    if IsSnapshotPropertyShape(Arg) or (Arg is TGocciaNullLiteralValue) then
    begin
      Properties := Arg;
      HasProperties := True;
    end
    else if Arg is TGocciaStringLiteralValue then
      Hint := Arg.ToStringLiteral.Value
  end;
  if AArgs.Length > 1 then
  begin
    Arg := AArgs.GetElement(1);
    if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) and
       Arg.ToBooleanLiteral.Value then
      Hint := Arg.ToStringLiteral.Value;
  end;

  if HasProperties and (not (FActualValue is TGocciaObjectValue) or
     (FActualValue.TypeOf <> 'object')) then
  begin
    FTestAssertions.ThrowError(
      'Received value must be an object when the matcher has properties',
      0, 0);
    Exit;
  end;
  PropertyMatched := not HasProperties or
    IsSnapshotPartialDeepEqual(FActualValue, Properties);
  if PropertyMatched and HasProperties then
    FormatValue := MergeSnapshotPropertyShape(FActualValue, Properties)
  else
    FormatValue := FActualValue;

  AddTempRootIfNeeded(FormatValue);
  try
    Received := AddSnapshotLineBreaks(
      FTestAssertions.FSnapshotFormatting.Format(FormatValue));
  finally
    RemoveTempRootIfNeeded(FormatValue);
  end;

  Passed := FTestAssertions.FSnapshotState.MatchExternal(
    SnapshotTestName(FTestAssertions), Hint, Received, PropertyMatched,
    FailureMessage);
  if Passed then
    FTestAssertions.AssertionPassed('toMatchSnapshot')
  else
    FTestAssertions.AssertionFailed('toMatchSnapshot', FailureMessage);
end;

function TGocciaExpectationValue.ToMatchInlineSnapshot(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Properties, FormatValue, Arg: TGocciaValue;
  InlineSnapshot, Hint, Received, FailureMessage: string;
  HasInlineSnapshot, HasProperties, PropertyMatched, Passed: Boolean;
  SnapshotArgumentIndex: Integer;
  CallSite: TGocciaCallSite;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FIsNegated then
  begin
    FTestAssertions.ThrowError(
      'toMatchInlineSnapshot cannot be used with "not"', 0, 0);
    Exit;
  end;
  if not Assigned(FTestAssertions.FSnapshotState) then
  begin
    FTestAssertions.ThrowError(
      'toMatchInlineSnapshot is only available when a snapshot host is installed',
      0, 0);
    Exit;
  end;

  Properties := nil;
  HasProperties := False;
  InlineSnapshot := '';
  Hint := '';
  HasInlineSnapshot := False;
  SnapshotArgumentIndex := 0;

  if AArgs.Length > 0 then
  begin
    Arg := AArgs.GetElement(0);
    if IsSnapshotPropertyShape(Arg) or (Arg is TGocciaNullLiteralValue) then
    begin
      Properties := Arg;
      HasProperties := True;
      SnapshotArgumentIndex := 1;
    end
    else if Arg is TGocciaStringLiteralValue then
    begin
      InlineSnapshot := StripInlineSnapshotIndentation(
        Arg.ToStringLiteral.Value);
      HasInlineSnapshot := True;
    end
  end;

  if AArgs.Length > 1 then
  begin
    Arg := AArgs.GetElement(1);
    if (AArgs.GetElement(0) is TGocciaStringLiteralValue) and
       Arg.ToBooleanLiteral.Value then
      Hint := Arg.ToStringLiteral.Value
    else if (Arg is TGocciaStringLiteralValue) and
            (Arg.ToStringLiteral.Value <> '') then
    begin
      InlineSnapshot := StripInlineSnapshotIndentation(
        Arg.ToStringLiteral.Value);
      HasInlineSnapshot := True;
      SnapshotArgumentIndex := 1;
    end
    else if Arg.ToBooleanLiteral.Value then
    begin
      FTestAssertions.ThrowError(
        'toMatchInlineSnapshot inline snapshot must be a string', 0, 0);
      Exit;
    end;
  end;

  if AArgs.Length > 2 then
  begin
    Arg := AArgs.GetElement(2);
    if Arg.ToBooleanLiteral.Value then
      Hint := Arg.ToStringLiteral.Value;
  end;

  if HasProperties and (not (FActualValue is TGocciaObjectValue) or
     (FActualValue.TypeOf <> 'object')) then
  begin
    FTestAssertions.ThrowError(
      'Received value must be an object when the matcher has properties',
      0, 0);
    Exit;
  end;
  PropertyMatched := not HasProperties or
    IsSnapshotPartialDeepEqual(FActualValue, Properties);
  if PropertyMatched and HasProperties then
    FormatValue := MergeSnapshotPropertyShape(FActualValue, Properties)
  else
    FormatValue := FActualValue;

  AddTempRootIfNeeded(FormatValue);
  try
    Received := AddSnapshotLineBreaks(
      FTestAssertions.FSnapshotFormatting.Format(FormatValue));
  finally
    RemoveTempRootIfNeeded(FormatValue);
  end;

  FillChar(CallSite, SizeOf(CallSite), 0);
  CurrentGocciaCallSite(CallSite);
  Passed := FTestAssertions.FSnapshotState.MatchInline(
    SnapshotTestName(FTestAssertions), Hint, Received, InlineSnapshot,
    HasInlineSnapshot, PropertyMatched, CallSite.FilePath,
    CallSite.Line, CallSite.Column,
    SnapshotArgumentIndex, FailureMessage);
  if Passed then
    FTestAssertions.AssertionPassed('toMatchInlineSnapshot')
  else
    FTestAssertions.AssertionFailed('toMatchInlineSnapshot', FailureMessage);
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
    Goccia.Values.ErrorHelper.ThrowTypeError(
      SErrorToHaveBeenCalledTimesExpectsInt, SSuggestTestUsage);
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

function TGocciaExpectationValue.ToHaveBeenCalledOnce(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  MockFn: TGocciaMockFunctionValue;
  Matches: Boolean;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 0, 'toHaveBeenCalledOnce',
    TGocciaTestAssertions(FTestAssertions).ThrowError);

  if not (FActualValue is TGocciaMockFunctionValue) then
  begin
    TGocciaTestAssertions(FTestAssertions).AssertionFailed(
      'toHaveBeenCalledOnce', 'Value must be a mock or spy function');
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  MockFn := TGocciaMockFunctionValue(FActualValue);
  Matches := MockFn.MockCalls.Count = 1;
  if FIsNegated then
    Matches := not Matches;

  if Matches then
    TGocciaTestAssertions(FTestAssertions).AssertionPassed(
      'toHaveBeenCalledOnce')
  else if FIsNegated then
    TGocciaTestAssertions(FTestAssertions).AssertionFailed(
      'toHaveBeenCalledOnce',
      'Expected mock not to have been called exactly once')
  else
    TGocciaTestAssertions(FTestAssertions).AssertionFailed(
      'toHaveBeenCalledOnce', Format(
        'Expected mock to have been called exactly once but was called %d time(s)',
        [MockFn.MockCalls.Count]));
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
    Goccia.Values.ErrorHelper.ThrowTypeError(
      SErrorToHaveBeenNthCalledWithRequiresArg, SSuggestTestUsage);
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  MockFn := TGocciaMockFunctionValue(FActualValue);
  NumVal := AArgs.GetElement(0).ToNumberLiteral;
  if NumVal.IsNaN or NumVal.IsInfinity or NumVal.IsNegativeInfinity or
     (NumVal.Value < 1) or (NumVal.Value > High(Integer)) or (Frac(NumVal.Value) <> 0) then
    Goccia.Values.ErrorHelper.ThrowTypeError(
      SErrorToHaveBeenNthCalledWithExpectsInt, SSuggestTestUsage);
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
    Goccia.Values.ErrorHelper.ThrowTypeError(
      SErrorToHaveReturnedTimesExpectsInt, SSuggestTestUsage);
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
        'Expected mock not to have returned with ' + FormatForDisplay(Expected))
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveReturnedWith',
        'Expected mock to have returned with ' + FormatForDisplay(Expected));
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
        'Expected mock not to have last returned with ' + FormatForDisplay(Expected))
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveLastReturnedWith',
        'Expected mock to have last returned with ' + FormatForDisplay(Expected));
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
    Goccia.Values.ErrorHelper.ThrowTypeError(
      SErrorToHaveNthReturnedWithExpectsInt, SSuggestTestUsage);
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
        Format('Expected return %d not to be %s', [N, FormatForDisplay(Expected)]))
    else
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('toHaveNthReturnedWith',
        Format('Expected return %d to be %s', [N, FormatForDisplay(Expected)]));
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaExpectationValue.GetNot(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaExpectationValue.Create(FActualValue, FTestAssertions, True,
    FIsRejectionReason);
end;

function TGocciaExpectationValue.GetResolves(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
begin
  if (TGarbageCollector.Instance <> nil) then
    TGarbageCollector.Instance.AddTempRoot(FActualValue);
  try
    if not (FActualValue is TGocciaPromiseValue) then
    begin
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('resolves',
        'Expected a Promise but received ' + FormatForDisplay(FActualValue));
      Result := TGocciaExpectationValue.Create(TGocciaUndefinedLiteralValue.UndefinedValue, FTestAssertions, FIsNegated);
      Exit;
    end;

    Promise := TGocciaPromiseValue(FActualValue);
    WaitForFetchPromise(Promise);

    if Promise.State = gpsFulfilled then
      Result := TGocciaExpectationValue.Create(Promise.PromiseResult, FTestAssertions, FIsNegated)
    else if Promise.State = gpsRejected then
    begin
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('resolves',
        'Expected Promise to resolve but it rejected with: ' + FormatForDisplay(Promise.PromiseResult));
      Result := TGocciaExpectationValue.Create(TGocciaUndefinedLiteralValue.UndefinedValue, FTestAssertions, FIsNegated);
    end
    else
    begin
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('resolves',
        'Promise still pending after microtask drain');
      Result := TGocciaExpectationValue.Create(TGocciaUndefinedLiteralValue.UndefinedValue, FTestAssertions, FIsNegated);
    end;
  finally
    if (TGarbageCollector.Instance <> nil) then
      TGarbageCollector.Instance.RemoveTempRoot(FActualValue);
  end;
end;

function TGocciaExpectationValue.GetRejects(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
begin
  if (TGarbageCollector.Instance <> nil) then
    TGarbageCollector.Instance.AddTempRoot(FActualValue);
  try
    if not (FActualValue is TGocciaPromiseValue) then
    begin
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('rejects',
        'Expected a Promise but received ' + FormatForDisplay(FActualValue));
      Result := TGocciaExpectationValue.Create(TGocciaUndefinedLiteralValue.UndefinedValue, FTestAssertions, FIsNegated);
      Exit;
    end;

    Promise := TGocciaPromiseValue(FActualValue);
    WaitForFetchPromise(Promise);

    if Promise.State = gpsRejected then
      Result := TGocciaExpectationValue.Create(Promise.PromiseResult, FTestAssertions, FIsNegated, True)
    else if Promise.State = gpsFulfilled then
    begin
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('rejects',
        'Expected Promise to reject but it resolved with: ' + FormatForDisplay(Promise.PromiseResult));
      Result := TGocciaExpectationValue.Create(TGocciaUndefinedLiteralValue.UndefinedValue, FTestAssertions, FIsNegated);
    end
    else
    begin
      TGocciaTestAssertions(FTestAssertions).AssertionFailed('rejects',
        'Promise still pending after microtask drain');
      Result := TGocciaExpectationValue.Create(TGocciaUndefinedLiteralValue.UndefinedValue, FTestAssertions, FIsNegated);
    end;
  finally
    if (TGarbageCollector.Instance <> nil) then
      TGarbageCollector.Instance.RemoveTempRoot(FActualValue);
  end;
end;

{ TGocciaTestAssertions }

constructor TGocciaTestAssertions.Create(const AName: string;
  const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback;
  const ASnapshotHost: IGocciaSnapshotHost;
  const ASnapshotUpdateMode: TGocciaSnapshotUpdateMode;
  const ASnapshotFormatter: IGocciaSnapshotFormatter;
  const AInjectGlobals: Boolean);
var
  GlobalObject: TGocciaObjectValue;
  ExpectFunction: TGocciaNativeFunctionValue;
  ExpectNotObject: TGocciaObjectValue;
  DescribeFunction: TGocciaNativeFunctionValue;
  TestFunction: TGocciaNativeFunctionValue;
  ItFunction: TGocciaNativeFunctionValue;
  BeforeAllFunction: TGocciaNativeFunctionValue;
  BeforeEachFunction: TGocciaNativeFunctionValue;
  AfterEachFunction: TGocciaNativeFunctionValue;
  AfterAllFunction: TGocciaNativeFunctionValue;
  OnTestFinishedFunction: TGocciaNativeFunctionValue;
  RunTestsFunction: TGocciaNativeFunctionValue;
  MockFunctionValue: TGocciaNativeFunctionValue;
  SpyOnFunction: TGocciaNativeFunctionValue;

  { Global injection is the half a host can decline. A host that only
    registers the `goccia:test` module namespace passes AInjectGlobals=False
    and gets every helper on FBuiltinObject with nothing added to the global
    object or the global scope. }
  procedure RegisterPublicGlobal(const AName: string; const AValue: TGocciaValue);
  begin
    if not AInjectGlobals then
      Exit;
    if Assigned(GlobalObject) then
      GlobalObject.DefineProperty(AName,
        TGocciaPropertyDescriptorData.Create(AValue, [pfWritable, pfConfigurable]))
    else
      AScope.DefineLexicalBinding(AName, AValue, dtConst, True);
  end;

begin
  inherited Create(AName, AScope, AThrowError);

  FRootSuite := TGocciaTestSuite.Create(nil, '', nil, nil);
  FCurrentRegistrationSuite := FRootSuite;
  FOnTestFinishedCallbacks := TGocciaArgumentsCollection.Create;
  FSnapshotFormatting := TGocciaSnapshotFormatting.Create(ASnapshotFormatter);
  if Assigned(ASnapshotHost) then
    FSnapshotState := TGocciaSnapshotState.Create(ASnapshotHost,
      ASnapshotUpdateMode)
  else
    FSnapshotState := nil;
  ResetTestStats;

  if AScope.ThisValue is TGocciaObjectValue then
    GlobalObject := TGocciaObjectValue(AScope.ThisValue)
  else
    GlobalObject := nil;
  FSnapshotFormatting.CaptureDateIntrinsics(GlobalObject);

  // Public testing helpers are global object properties, not lexical bindings.
  // Test262 scripts intentionally declare vars named expect/test/it, and those
  // var globals must be able to shadow the runner helpers.
  ExpectFunction := TGocciaNativeFunctionValue.Create(Expect, 'expect', 1);
  ExpectFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    ExpectAnything, 'anything', 0));
  ExpectFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    ExpectAny, 'any', 1));
  ExpectFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    ExpectArrayContaining, 'arrayContaining', 1));
  ExpectFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    ExpectObjectContaining, 'objectContaining', 1));
  ExpectFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    ExpectStringContaining, 'stringContaining', 1));
  ExpectFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    ExpectStringMatching, 'stringMatching', 1));
  ExpectFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    ExpectCloseTo, 'closeTo', 2));
  ExpectFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    ExpectSchemaMatching, 'schemaMatching', 1));
  ExpectFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    ExpectToBeOneOf, 'toBeOneOf', 1));
  ExpectFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    ExpectToSatisfy, 'toSatisfy', 2));
  ExpectFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    ExpectToBeFasterThan, 'toBeFasterThan', 2));
  ExpectFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    ExpectToBeSlowerThan, 'toBeSlowerThan', 2));
  ExpectFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    AddSnapshotSerializer, 'addSnapshotSerializer', 1));

  ExpectNotObject := TGocciaObjectValue.Create;
  ExpectNotObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    ExpectNotArrayContaining, 'arrayContaining', 1));
  ExpectNotObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    ExpectNotObjectContaining, 'objectContaining', 1));
  ExpectNotObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    ExpectNotStringContaining, 'stringContaining', 1));
  ExpectNotObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    ExpectNotStringMatching, 'stringMatching', 1));
  ExpectNotObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    ExpectNotCloseTo, 'closeTo', 2));
  ExpectNotObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    ExpectNotSchemaMatching, 'schemaMatching', 1));
  ExpectNotObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    ExpectNotToBeOneOf, 'toBeOneOf', 1));
  ExpectNotObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    ExpectNotToSatisfy, 'toSatisfy', 2));
  ExpectNotObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    ExpectNotToBeFasterThan, 'toBeFasterThan', 2));
  ExpectNotObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(
    ExpectNotToBeSlowerThan, 'toBeSlowerThan', 2));
  ExpectFunction.DefineProperty('not', TGocciaPropertyDescriptorData.Create(
    ExpectNotObject, [pfConfigurable]));
  RegisterPublicGlobal('expect', ExpectFunction);

  // Create describe function with skip/skipIf/runIf properties
  DescribeFunction := TGocciaNativeFunctionValue.Create(Describe, 'describe', 2);
  ConfigureDescribeFunction(DescribeFunction);
  RegisterPublicGlobal('describe', DescribeFunction);

  // Create test function with skip/skipIf/runIf properties
  TestFunction := TGocciaNativeFunctionValue.Create(Test, 'test', 2);
  ConfigureTestFunction(TestFunction);
  RegisterPublicGlobal('test', TestFunction);

  // Private aliases used by generated Test262 wrappers.  Some conformance
  // tests intentionally declare globals named describe/test.  These are
  // global-scope bindings, so they belong to the global-injection half.
  if AInjectGlobals then
  begin
    AScope.DefineLexicalBinding('__gocciaTest262Describe', DescribeFunction,
      dtConst, True);
    AScope.DefineLexicalBinding('__gocciaTest262Test', TestFunction, dtConst,
      True);
  end;

  ItFunction := TGocciaNativeFunctionValue.Create(It, 'it', 2);
  ConfigureTestFunction(ItFunction);
  RegisterPublicGlobal('it', ItFunction);
  BeforeAllFunction := TGocciaNativeFunctionValue.Create(BeforeAll, 'beforeAll', 1);
  BeforeEachFunction := TGocciaNativeFunctionValue.Create(BeforeEach, 'beforeEach', 1);
  AfterEachFunction := TGocciaNativeFunctionValue.Create(AfterEach, 'afterEach', 1);
  AfterAllFunction := TGocciaNativeFunctionValue.Create(AfterAll, 'afterAll', 1);
  OnTestFinishedFunction := TGocciaNativeFunctionValue.Create(OnTestFinished, 'onTestFinished', 1);
  RunTestsFunction := TGocciaNativeFunctionValue.Create(RunTests, 'runTests', 0);
  MockFunctionValue := TGocciaNativeFunctionValue.Create(MockFunction, 'mock', 0);
  SpyOnFunction := TGocciaNativeFunctionValue.Create(SpyOn, 'spyOn', 2);
  RegisterPublicGlobal('beforeAll', BeforeAllFunction);
  RegisterPublicGlobal('beforeEach', BeforeEachFunction);
  RegisterPublicGlobal('afterEach', AfterEachFunction);
  RegisterPublicGlobal('afterAll', AfterAllFunction);
  RegisterPublicGlobal('onTestFinished', OnTestFinishedFunction);
  RegisterPublicGlobal('runTests', RunTestsFunction);
  RegisterPublicGlobal('mock', MockFunctionValue);
  RegisterPublicGlobal('spyOn', SpyOnFunction);

  // Also set them in the built-in object for completeness
  FBuiltinObject.RegisterNativeMethod(ExpectFunction);
  FBuiltinObject.RegisterNativeMethod(DescribeFunction);
  FBuiltinObject.RegisterNativeMethod(TestFunction);
  FBuiltinObject.RegisterNativeMethod(ItFunction);
  FBuiltinObject.RegisterNativeMethod(BeforeAllFunction);
  FBuiltinObject.RegisterNativeMethod(BeforeEachFunction);
  FBuiltinObject.RegisterNativeMethod(AfterEachFunction);
  FBuiltinObject.RegisterNativeMethod(AfterAllFunction);
  FBuiltinObject.RegisterNativeMethod(OnTestFinishedFunction);
  FBuiltinObject.RegisterNativeMethod(RunTestsFunction);
  FBuiltinObject.RegisterNativeMethod(MockFunctionValue);
  FBuiltinObject.RegisterNativeMethod(SpyOnFunction);
end;

destructor TGocciaTestAssertions.Destroy;
var
  I: Integer;
begin
  if Assigned(FSnapshotFormatting) then
    for I := 0 to FSnapshotFormatting.Serializers.Count - 1 do
      RemoveTempRootIfNeeded(FSnapshotFormatting.Serializers.SerializerAt(I));
  FSnapshotState.Free;
  FSnapshotFormatting.Free;
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
    Goccia.Values.ErrorHelper.ThrowTypeError(Format(SErrorFunctionExpectsStringFirst, [AFunctionName]), SSuggestTestUsage);

  if not (AArgs.GetElement(1) is TGocciaFunctionBase) then
    Goccia.Values.ErrorHelper.ThrowTypeError(Format(SErrorFunctionExpectsFunctionSecond, [AFunctionName]), SSuggestTestUsage);

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
    Goccia.Values.ErrorHelper.ThrowTypeError(Format(SErrorFunctionExpectsStringFirst, [AFunctionName]), SSuggestTestUsage);

  if not (AArgs.GetElement(1) is TGocciaFunctionBase) then
    Goccia.Values.ErrorHelper.ThrowTypeError(Format(SErrorFunctionExpectsFunctionSecond, [AFunctionName]), SSuggestTestUsage);

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
    Goccia.Values.ErrorHelper.ThrowTypeError(Format(SErrorFunctionExpectsFunctionArg, [AHookName]), SSuggestTestUsage);

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
  var
    NumVal: TGocciaNumberLiteralValue;
  begin
    if not Assigned(AValue) then
      Exit('%' + AToken);

    case AToken of
      'd', 'i':
      begin
        // NaN/±∞ and doubles beyond Integer cannot be Trunc'd; render them
        // like %f so test.each("%d") titles never crash on non-finite rows.
        NumVal := AValue.ToNumberLiteral;
        if NumVal.IsNaN or NumVal.IsInfinity or NumVal.IsNegativeInfinity or
           (Abs(NumVal.Value) > MaxInt) then
          Result := FormatDouble(NumVal.Value)
        else
          Result := IntToStr(Trunc(NumVal.Value));
      end;
      'f':
        Result := FormatDouble(AValue.ToNumberLiteral.Value);
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

{ ': <message>' when there is one, otherwise ''. Keeps the detail
  string readable when a hook failed without a usable payload. }
function FormatHookFailureSuffix(const AMessage: string): string;
begin
  if AMessage = '' then
    Result := ''
  else
    Result := ': ' + AMessage;
end;

{ Error objects render as an empty object literal through
  FormatForDisplay because their data lives on non-enumerable own
  properties, which loses exactly the text both oracles print. Prefer
  'Name: message' when the value looks like an Error, and fall back to
  the generic formatter otherwise. }
function DescribeThrownValue(const AValue: TGocciaValue): string;
var
  NameValue, MessageValue: TGocciaValue;
  NameText, MessageText: string;
begin
  Result := '';
  if not Assigned(AValue) then
    Exit;

  if AValue is TGocciaObjectValue then
  begin
    NameValue := AValue.GetProperty('name');
    MessageValue := AValue.GetProperty('message');
    if Assigned(MessageValue) and not (MessageValue is TGocciaUndefinedLiteralValue) then
    begin
      MessageText := MessageValue.ToStringLiteral.Value;
      NameText := '';
      if Assigned(NameValue) and not (NameValue is TGocciaUndefinedLiteralValue) then
        NameText := NameValue.ToStringLiteral.Value;
      if (NameText <> '') and (MessageText <> '') then
        Exit(NameText + ': ' + MessageText);
      if MessageText <> '' then
        Exit(MessageText);
      if NameText <> '' then
        Exit(NameText);
    end;
  end;

  Result := FormatForDisplay(AValue);
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
    { Collection died in an earlier describe: stop walking the tree. }
    if FCollectionAborted then
      Exit;

    Entry := ASuite.Entries[I];
    if not (Entry is TGocciaTestSuite) then
      Continue;

    ChildSuite := TGocciaTestSuite(Entry);
    ChildSuite.ClearRegisteredContent;

    PreviousSuite := FCurrentRegistrationSuite;
    FCurrentRegistrationSuite := ChildSuite;
    try
      { Per-describe deadline. Bounds the time spent inside the user's
        describe callback (the registration phase) only — once the child
        tests start running each one needs an unobstructed tsTest scope,
        otherwise the older tsDescribe deadline preempts the per-test
        deadline whenever both are configured to the same value. Pushed
        unconditionally so a 0 value is a no-op and the matching Pop
        runs in the outer try-finally. }
      PushTimeoutScope(tsDescribe, GTestRunnerDescribeTimeoutMs);
      try
        try
          if Assigned(ChildSuite.SuiteFunction) then
            ChildSuite.SuiteFunction.Call(ChildSuite.SuiteArguments,
              TGocciaUndefinedLiteralValue.UndefinedValue);
        except
          { A refused allocation is uncatchable and must unwind to the host on
            every execution path, not be converted into a describe failure. }
          on E: TGocciaMemoryLimitError do
            raise;
          on E: Exception do
          begin
            if not FSuppressOutput then
              WriteLn('Error in describe block "', ChildSuite.GetFullName,
                '": ', E.Message);
            { E.Message is empty for a thrown JS value; the payload lives
              on TGocciaThrowValue.Value. }
            if (E is TGocciaThrowValue) and Assigned(TGocciaThrowValue(E).Value) then
              AFailedTestDetails.Add('Describe "' + ChildSuite.GetFullName +
                '": ' + DescribeThrownValue(TGocciaThrowValue(E).Value))
            else
              AFailedTestDetails.Add('Describe "' + ChildSuite.GetFullName +
                '": ' + E.Message);
            { A throw in a describe CALLBACK is a collection failure, and
              Vitest discards the entire file for it: zero tests run, even
              ones in suites collected before the throwing describe.
              Registration stops here and RunTests skips execution
              entirely. `suiteErrors` makes the file not-ok, which drives
              the envelope `ok` and the exit code. Execution-time failures
              (hooks, tests) keep their own accounting and do NOT abort
              collection. }
            Inc(FTestStats.SuiteErrors);
            FCollectionAborted := True;
          end;
        end;
      finally
        PopTimeoutScope;
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
  const AFailedTestDetails: TStringList; var AShouldStop: Boolean;
  const ASetupFailed: Boolean);
var
  I: Integer;
  Entry: TGocciaRegisteredEntry;
  TestCase: TGocciaTestCase;
  BeforeCallbacks: TGocciaArgumentsCollection;
  AfterCallbacks: TGocciaArgumentsCollection;
  TestResult: TGocciaValue;
  RejectionReason: string;
  ExceptionDetail, ExceptionSummary: string;
  FailureRecorded: Boolean;
  EffectiveSuiteName: string;
  HookFailed: Boolean;
  HookMessage: string;
  RunSuiteHooks: Boolean;
  SetupFailed: Boolean;
begin
  { Inherited from an ancestor whose beforeAll threw; set below when this
    suite's own beforeAll fails. }
  SetupFailed := ASetupFailed;
  if AShouldStop then
    Exit;

  EffectiveSuiteName := ASuite.GetFullName;
  { ASetupFailed (not SetupFailed) gates both hooks: a suite whose own
    beforeAll throws still runs its afterAll, but a descendant of a
    failed suite runs neither. Both match Vitest. }
  RunSuiteHooks := not ASetupFailed and not IsSuiteSkipped(ASuite) and
    SuiteHasRunnableEntries(ASuite, AHasFocusedEntries);

  { The per-describe deadline is no longer pushed here.  It now lives in
    BuildNestedRegistrations around the describe callback so that nested
    tsTest deadlines are not preempted by an older tsDescribe deadline
    when --describe-timeout and --test-timeout are configured to the
    same value. }

  if RunSuiteHooks and (ASuite.BeforeAllCallbacks.Length > 0) then
  begin
    FTestStats.CurrentSuiteName := EffectiveSuiteName;
    FTestStats.CurrentTestName := 'beforeAll';
    ResetCurrentTestState;
    RunCallbacks(ASuite.BeforeAllCallbacks);
    HookFailed := FTestStats.CurrentTestHasFailures;
    HookMessage := FTestStats.CurrentFailureMessage;
    ResetCurrentTestState;
    if HookFailed then
    begin
      AFailedTestDetails.Add('Hook "beforeAll" in suite "' + EffectiveSuiteName +
        '" failed' + FormatHookFailureSuffix(HookMessage));
      { Record the hook failure as a suite error, not a failed test:
        Vitest reports this suite's tests as SKIPPED with fail=0 and
        fails the file instead. SetupFailed below carries that decision
        into the entry loop and into descendant suites. }
      Inc(FTestStats.SuiteErrors);
      SetupFailed := True;
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
        AExitOnFirstFailure, AFailedTestDetails, AShouldStop, SetupFailed);
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
      if Assigned(FSnapshotState) then
        FSnapshotState.PreserveTestSnapshots(SnapshotTestName(Self));
      if not FSuppressOutput then
      begin
        if FTestStats.CurrentSuiteName <> '' then
          WriteLn('    📝 ', TestCase.Name, ' in ', FTestStats.CurrentSuiteName,
            ': TODO')
        else
          WriteLn('    📝 ', TestCase.Name, ': TODO');
      end;
    end
    else if SetupFailed or TestCase.IsSkipped or
      IsSuiteSkipped(TestCase.ParentSuite) or
      (AHasFocusedEntries and not IsTestSelected(TestCase, True)) then
    begin
      FTestStats.CurrentTestIsSkipped := True;
      if Assigned(FSnapshotState) then
        FSnapshotState.PreserveTestSnapshots(SnapshotTestName(Self));
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
          { Per-test deadline. Push unconditionally — a 0 value
            contributes no deadline, preserving the previous
            behaviour when --test-timeout is not set. The matching
            Pop runs in the finally directly below. }
          PushTimeoutScope(tsTest, GTestRunnerTestTimeoutMs);
          try
            try
              { A failed beforeEach leaves the fixture the body depends on
                broken, so the body must not run -- Vitest and bun both
                skip it and still fail the test. RunCallbacks routes a
                throwing/rejecting hook through AssertionFailed, so this
                flag is exactly "a beforeEach for this test failed". The
                counts are unchanged; only the side effects of executing
                a body against a broken fixture go away. }
              if FTestStats.CurrentTestHasFailures then
                TestResult := TGocciaUndefinedLiteralValue.UndefinedValue
              else if Assigned(TestCase.TestFunction) then
                TestResult := TestCase.TestFunction.Call(TestCase.TestArguments,
                  TGocciaUndefinedLiteralValue.UndefinedValue)
              else
                TestResult := TGocciaUndefinedLiteralValue.UndefinedValue;

              if Assigned(TestResult) then
                AddTempRootIfNeeded(TestResult);
              try
                if TestResult is TGocciaPromiseValue then
                begin
                  WaitForFetchPromise(TGocciaPromiseValue(TestResult));
                  if TGocciaPromiseValue(TestResult).State = gpsRejected then
                  begin
                    RejectionReason := FormatForDisplay(
                      TGocciaPromiseValue(TestResult).PromiseResult);
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
                end
                else
                  DrainMicrotasksAndFetchCompletions;
              finally
                if Assigned(TestResult) then
                  RemoveTempRootIfNeeded(TestResult);
              end;
            except
              { Test-scope timeout: record TIMEOUT and let execution
                continue with the next test. Other timeout scopes
                (describe, file) propagate up. Listed first because
                TGocciaTimeoutError descends from Exception. }
              on E: TGocciaTimeoutError do
              begin
                if E.Scope = tsTest then
                begin
                  if (TGocciaMicrotaskQueue.Instance <> nil) then
                    TGocciaMicrotaskQueue.Instance.ClearQueue;
                  DiscardFetchCompletions;
                  AssertionFailed('test execution',
                    Format('Test exceeded per-test timeout of %dms',
                      [E.DurationMs]));
                  if FTestStats.CurrentSuiteName <> '' then
                    AFailedTestDetails.Add('Test "' + TestCase.Name +
                      '" in suite "' + FTestStats.CurrentSuiteName +
                      '": TIMEOUT after ' + IntToStr(E.DurationMs) + 'ms')
                  else
                    AFailedTestDetails.Add('Test "' + TestCase.Name +
                      '": TIMEOUT after ' + IntToStr(E.DurationMs) + 'ms');
                  FailureRecorded := True;
                end
                else
                  raise;
              end;
              { A refused allocation is uncatchable and must unwind to the host,
                not be converted into a test failure and swallowed here. }
              on E: TGocciaMemoryLimitError do
                raise;
              on E: Exception do
              begin
                if (TGocciaMicrotaskQueue.Instance <> nil) then
                  TGocciaMicrotaskQueue.Instance.ClearQueue;
                DiscardFetchCompletions;
                if E is TGocciaError then
                begin
                  ExceptionDetail := TGocciaError(E).GetDetailedMessage;
                  ExceptionSummary := E.Message;
                end
                else if E is TGocciaThrowValue then
                begin
                  ExceptionDetail := FormatThrowValueDetail(
                    TGocciaThrowValue(E).Value);
                  ExceptionSummary := ExceptionDetail;
                end
                else
                begin
                  ExceptionDetail := E.Message;
                  ExceptionSummary := E.Message;
                end;
                AssertionFailed('test execution', ExceptionDetail);
                if FTestStats.CurrentSuiteName <> '' then
                  AFailedTestDetails.Add('Test "' + TestCase.Name +
                    '" in suite "' + FTestStats.CurrentSuiteName + '": ' +
                    ExceptionSummary)
                else
                  AFailedTestDetails.Add('Test "' + TestCase.Name + '": ' +
                    ExceptionSummary);
                FailureRecorded := True;
              end;
            end;
          finally
            PopTimeoutScope;
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

      { Reached when the failure came from somewhere that did not write its
        own detail line — most often a beforeEach that threw, which skips
        the body entirely. The name alone leaves the JSON payload without
        any explanation, so carry the first recorded failure message (the
        one AssertionFailed kept) into the entry. }
      if FTestStats.CurrentTestHasFailures and not FailureRecorded then
      begin
        if FTestStats.CurrentSuiteName <> '' then
          AFailedTestDetails.Add('Test "' + TestCase.Name + '" in suite "' +
            FTestStats.CurrentSuiteName + '"' +
            FormatHookFailureSuffix(FTestStats.CurrentFailureMessage))
        else
          AFailedTestDetails.Add('Test "' + TestCase.Name + '"' +
            FormatHookFailureSuffix(FTestStats.CurrentFailureMessage));
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
    HookMessage := FTestStats.CurrentFailureMessage;
    ResetCurrentTestState;
    if HookFailed then
    begin
      AFailedTestDetails.Add('Hook "afterAll" in suite "' + EffectiveSuiteName +
        '" failed' + FormatHookFailureSuffix(HookMessage));
      { Teardown failure: the suite's tests have already run and keep
        their results, so nothing is skipped here. Like the beforeAll
        hook, Vitest leaves this out of the test counts (fail=0) and
        fails the file, which `suiteErrors` expresses. }
      Inc(FTestStats.SuiteErrors);
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
  FTestStats.SuiteErrors := 0;
  FCollectionAborted := False;
  FTestStats.CurrentSuiteName := '';
  FTestStats.CurrentTestName := '';
  FTestStats.CurrentTestHasFailures := False;
  FTestStats.CurrentFailureMessage := '';
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

          if (TGarbageCollector.Instance <> nil) then
            TGarbageCollector.Instance.AddTempRoot(CallbackResult);
          try
            if CallbackResult is TGocciaPromiseValue then
            begin
              Promise := TGocciaPromiseValue(CallbackResult);
              WaitForFetchPromise(Promise);
              if Promise.State = gpsRejected then
                AssertionFailed('callback execution', 'Async callback rejected: ' + DescribeThrownValue(Promise.PromiseResult))
              else if Promise.State = gpsPending then
                AssertionFailed('callback execution', 'Async callback Promise still pending after microtask drain');
            end
            else
              DrainMicrotasksAndFetchCompletions;
          finally
            if (TGarbageCollector.Instance <> nil) then
              TGarbageCollector.Instance.RemoveTempRoot(CallbackResult);
          end;
        except
          { Timeout errors flag a describe/file/test deadline expiring;
            they must propagate so the outer ExecuteSuite handler can
            record the timeout and unwind cleanly.  Downgrading them to
            an assertion failure stranded the deadline state and let
            execution keep running past the limit. }
          on E: TGocciaTimeoutError do
            raise;
          { A thrown JS value carries its payload on TGocciaThrowValue.Value;
            E.Message is empty for it, which dropped the text both oracles
            print. }
          { A refused allocation is uncatchable and must unwind to the host, not
            be converted into a hook failure and swallowed here. }
          on E: TGocciaMemoryLimitError do
            raise;
          on E: TGocciaThrowValue do
            AssertionFailed('callback execution',
              'Callback threw an exception: ' + DescribeThrownValue(E.Value));
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
  FTestStats.CurrentFailureMessage := '';
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

function TGocciaTestAssertions.GetCurrentTestHasFailures: Boolean;
begin
  Result := FTestStats.CurrentTestHasFailures;
end;

procedure TGocciaTestAssertions.ResetCurrentTestState;
begin
  FTestStats.CurrentTestHasFailures := False;
  FTestStats.CurrentFailureMessage := '';
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
  { Keep the first message: later assertions in the same hook must not
    overwrite the one that actually explains the failure. Recorded
    before the suppress-output exit so JSON runs keep it too. }
  if FTestStats.CurrentFailureMessage = '' then
    FTestStats.CurrentFailureMessage := AMessage;

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

function TGocciaTestAssertions.ExpectAnything(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaAnythingMatcherValue.Create;
end;

function TGocciaTestAssertions.ExpectAny(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaAnyMatcherValue.Create(AArgs.GetElement(0));
end;

function TGocciaTestAssertions.ExpectArrayContaining(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaArrayContainingMatcherValue.Create(AArgs.GetElement(0));
end;

function TGocciaTestAssertions.ExpectObjectContaining(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaObjectContainingMatcherValue.Create(AArgs.GetElement(0));
end;

function TGocciaTestAssertions.ExpectStringContaining(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringContainingMatcherValue.Create(AArgs.GetElement(0));
end;

function TGocciaTestAssertions.ExpectStringMatching(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringMatchingMatcherValue.Create(AArgs.GetElement(0));
end;

function TGocciaTestAssertions.ExpectCloseTo(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaCloseToMatcherValue.Create(AArgs.GetElement(0),
    AArgs.GetElement(1));
end;

function TGocciaTestAssertions.ExpectSchemaMatching(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaSchemaMatchingMatcherValue.Create(AArgs.GetElement(0));
end;

function TGocciaTestAssertions.ExpectToBeOneOf(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaOneOfMatcherValue.Create(AArgs.GetElement(0));
end;

function TGocciaTestAssertions.ExpectToSatisfy(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaSatisfyMatcherValue.Create(AArgs.GetElement(0));
end;

function TGocciaTestAssertions.ExpectToBeFasterThan(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaBenchmarkMatcherValue.Create(AArgs.GetElement(0),
    AArgs.GetElement(1), bcFaster);
end;

function TGocciaTestAssertions.ExpectToBeSlowerThan(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaBenchmarkMatcherValue.Create(AArgs.GetElement(0),
    AArgs.GetElement(1), bcSlower);
end;

function TGocciaTestAssertions.ExpectNotArrayContaining(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaArrayContainingMatcherValue.Create(AArgs.GetElement(0), True);
end;

function TGocciaTestAssertions.ExpectNotObjectContaining(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaObjectContainingMatcherValue.Create(AArgs.GetElement(0), True);
end;

function TGocciaTestAssertions.ExpectNotStringContaining(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringContainingMatcherValue.Create(AArgs.GetElement(0), True);
end;

function TGocciaTestAssertions.ExpectNotStringMatching(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringMatchingMatcherValue.Create(AArgs.GetElement(0), True);
end;

function TGocciaTestAssertions.ExpectNotCloseTo(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaCloseToMatcherValue.Create(AArgs.GetElement(0),
    AArgs.GetElement(1), True);
end;

function TGocciaTestAssertions.ExpectNotSchemaMatching(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaSchemaMatchingMatcherValue.Create(AArgs.GetElement(0), True);
end;

function TGocciaTestAssertions.ExpectNotToBeOneOf(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaOneOfMatcherValue.Create(AArgs.GetElement(0), True);
end;

function TGocciaTestAssertions.ExpectNotToSatisfy(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaSatisfyMatcherValue.Create(AArgs.GetElement(0), True);
end;

function TGocciaTestAssertions.ExpectNotToBeFasterThan(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaBenchmarkMatcherValue.Create(AArgs.GetElement(0),
    AArgs.GetElement(1), bcFaster, True);
end;

function TGocciaTestAssertions.ExpectNotToBeSlowerThan(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaBenchmarkMatcherValue.Create(AArgs.GetElement(0),
    AArgs.GetElement(1), bcSlower, True);
end;

function TGocciaTestAssertions.AddSnapshotSerializer(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Serializer: TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1,
    'expect.addSnapshotSerializer', ThrowError);
  Serializer := AArgs.GetElement(0);
  FSnapshotFormatting.Serializers.Add(Serializer);
  AddTempRootIfNeeded(Serializer);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTestAssertions.MockFunction(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Impl: TGocciaValue;
begin
  if AArgs.Length > 0 then
  begin
    if not (AArgs.GetElement(0) is TGocciaFunctionBase) then
    begin
      Goccia.Values.ErrorHelper.ThrowTypeError(SErrorMockExpectsFunctionOrNoArgs, SSuggestTestUsage);
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
    Goccia.Values.ErrorHelper.ThrowTypeError(SErrorSpyOnExpectsObject, SSuggestTestUsage);
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  if not (AArgs.GetElement(1) is TGocciaStringLiteralValue) then
  begin
    Goccia.Values.ErrorHelper.ThrowTypeError(SErrorSpyOnExpectsString, SSuggestTestUsage);
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  Target := TGocciaObjectValue(AArgs.GetElement(0));
  MethodName := TGocciaStringLiteralValue(AArgs.GetElement(1)).Value;

  if not Target.HasProperty(MethodName) then
  begin
    Goccia.Values.ErrorHelper.ThrowTypeError(Format(SErrorSpyOnNonExistentProperty, [MethodName]), SSuggestTestUsage);
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  ExistingValue := Target.GetProperty(MethodName);
  if not Assigned(ExistingValue) or not (ExistingValue is TGocciaFunctionBase) then
  begin
    Goccia.Values.ErrorHelper.ThrowTypeError(Format(SErrorSpyOnPropertyNotFunction, [MethodName]), SSuggestTestUsage);
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
    Goccia.Values.ErrorHelper.ThrowTypeError(Format(SErrorFunctionExpectsTableArray, ['describe.each']), SSuggestTestUsage);

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
    Goccia.Values.ErrorHelper.ThrowTypeError(Format(SErrorFunctionExpectsTableArray, ['test.each']), SSuggestTestUsage);

  Result := TGocciaParameterizedRegistrationFunction.Create(Self,
    AArgs.GetElement(0), prtTest);
end;

function TGocciaTestAssertions.TestTodo(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'test.todo', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    Goccia.Values.ErrorHelper.ThrowTypeError(SErrorTestTodoExpectsString, SSuggestTestUsage);

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
    Goccia.Values.ErrorHelper.ThrowTypeError(SErrorOnTestFinishedExpectsFunction, SSuggestTestUsage);

  AddTempRootIfNeeded(AArgs.GetElement(0));
  FOnTestFinishedCallbacks.Add(AArgs.GetElement(0));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTestAssertions.RunTests(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  StartTime: Int64;
  ResultObj: TGocciaObjectValue;
  ExitOnFirstFailure: Boolean;
  ShowTestResults: Boolean;
  Summary: string;
  FailedTestDetails: TStringList;
  FailedTestDetailsArray: TGocciaArrayValue;
  SuiteNames: TStringList;
  Param: TGocciaValue;
  Val: TGocciaValue;
  HasFocusedEntries: Boolean;
  ShouldStop: Boolean;
  SnapshotErrors: TStringList;
  FloatingPointState: TGocciaFloatingPointState;
begin
  ExitOnFirstFailure := False;
  ShowTestResults := True;
  EnterGocciaFloatingPointScope(FloatingPointState);
  try
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
    { Collection aborted: the file is discarded whole, so nothing runs.
      Counts stay at zero and `suiteErrors` carries the failure. }
    if not FCollectionAborted then
      ExecuteSuite(FRootSuite, HasFocusedEntries, ExitOnFirstFailure,
        FailedTestDetails, ShouldStop);

    if Assigned(FSnapshotState) then
    begin
      SnapshotErrors := TStringList.Create;
      try
        try
          FSnapshotState.Finish(SnapshotErrors, not ShouldStop);
        except
          on E: Exception do
            SnapshotErrors.Add('Snapshot finalization failed: ' + E.Message);
        end;
        if SnapshotErrors.Count > 0 then
        begin
          Inc(FTestStats.TotalTests);
          Inc(FTestStats.FailedTests);
          for I := 0 to SnapshotErrors.Count - 1 do
            FailedTestDetails.Add(SnapshotErrors[I]);
        end;
      finally
        SnapshotErrors.Free;
      end;
    end;

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
    { Collection aborted: Vitest discards the whole file, so the tests
      registered before the throwing describe are not collected either.
      Reporting them here contradicted the zero run counts beside it — the
      envelope claimed a total the runner never intended to run. }
    if FCollectionAborted then
      ResultObj.AssignProperty('totalTests', TGocciaNumberLiteralValue.ZeroValue)
    else
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
    ResultObj.AssignProperty('suiteErrors', TGocciaNumberLiteralValue.Create(
      FTestStats.SuiteErrors));
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

      { A suite-level error (throwing describe, failed beforeAll/afterAll)
        never enters FailedTests, so checking that alone printed "All tests
        passed!" for a file the runner is about to mark not-ok. }
      if (FTestStats.FailedTests = 0) and (FTestStats.SuiteErrors = 0) then
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
  finally
    LeaveGocciaFloatingPointScope(FloatingPointState);
  end;
end;

end.
