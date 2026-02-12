unit TestRunner;

{$I Goccia.inc}

interface

uses
  SysUtils, Classes, Generics.Collections, Generics.Defaults, DateUtils, TypInfo;

type
  // Forward declarations
  TTestSuite = class;
  TTestRunner = class;

  // Test method procedure type
  TTestMethod = procedure of object;

  // Test result types
  TTestStatus = (tsPass, tsFail, tsSkip);

  TTestResult = record
    Name: string;
    SuiteName: string;
    Status: TTestStatus;
    ErrorMessage: string;
    Duration: Int64; // milliseconds
  end;

  // Test registration record
  TTestRegistration = record
    Name: string;
    Method: TTestMethod;
    Skip: Boolean;
    SkipReason: string;
  end;

  // Exception for test assertions
  ETestAssertionError = class(Exception);

  // Generic expect record for fluent assertions
  // Using a record (not a class) so Expect<T>('hello').ToBe('hello') needs no Free.
  // Defined as a standalone type (not a method on TTestSuite) to avoid
  // FPC 3.2.2 AArch64 compiler crash with cross-unit generic method specialization.
  TExpect<T> = record
  private
    FActual: T;
    class function FormatValue(const Value: T): string; static;
  public
    procedure ToBe(Expected: T);
  end;

  // Base test suite class
  TTestSuite = class
  private
    FName: string;
    FRunner: TTestRunner;
    FCurrentTestName: string;
    FTests: TList<TTestRegistration>;
    FHasAssertions: Boolean;
  protected
    // Lifecycle hooks
    procedure BeforeAll; virtual;
    procedure AfterAll; virtual;
    procedure BeforeEach; virtual;
    procedure AfterEach; virtual;

    // Test registration
    procedure Test(const Name: string; Method: TTestMethod);
    procedure Skip(const Name: string; Method: TTestMethod; const Reason: string = '');


    // Override this to register your tests
    procedure SetupTests; virtual; abstract;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
    property Name: string read FName;
    property Tests: TList<TTestRegistration> read FTests;

    // Assertion helper
    procedure Fail(const Message: string);
  end;

  TTestSuiteClass = class of TTestSuite;

  // Test runner
  TTestRunner = class
  private
    FSuites: TList<TTestSuite>;
    FResults: TList<TTestResult>;
    FStartTime: TDateTime;

    procedure RunTest(Suite: TTestSuite; const Test: TTestRegistration);
    procedure PrintResults;
    procedure PrintSummary;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddSuite(SuiteClass: TTestSuiteClass); overload;
    procedure AddSuite(Suite: TTestSuite); overload;
    procedure Run;

    property Results: TList<TTestResult> read FResults;
  end;

// Standalone generic function for fluent assertions: Expect<string>('hello').ToBe('hello')
function Expect<T>(Value: T): TExpect<T>;

// Global test runner instance
var
  TestRunnerProgram: TTestRunner;
  _ActiveTestSuite: TTestSuite;

function TestResultToExitCode: Integer;

implementation

var
  CurrentDescribeSuite: TTestSuite;

function TestResultToExitCode: Integer;
var
  TestResult: TTestResult;
begin
  Result := 0;
  for TestResult in TestRunnerProgram.Results do
  begin
    if TestResult.Status = tsFail then
      Result := 1;
  end;
end;

{ TExpect<T> }

class function TExpect<T>.FormatValue(const Value: T): string;
var
  P: Pointer;
begin
  P := @Value;
  case PTypeInfo(TypeInfo(T))^.Kind of
    tkSString, tkLString, tkAString, tkUString, tkWString:
      Result := '"' + PString(P)^ + '"';
    tkFloat:
      Result := FloatToStr(PDouble(P)^);
    tkInteger:
      Result := IntToStr(PInteger(P)^);
    tkInt64:
      Result := IntToStr(PInt64(P)^);
    tkBool:
      if PBoolean(P)^ then Result := 'True'
      else Result := 'False';
    tkEnumeration:
      Result := IntToStr(PByte(P)^);
  else
    Result := '<value>';
  end;
end;

procedure TExpect<T>.ToBe(Expected: T);
var
  Comparer: IEqualityComparer<T>;
begin
  if Assigned(_ActiveTestSuite) then
    _ActiveTestSuite.FHasAssertions := True;
  Comparer := TEqualityComparer<T>.Default;
  if not Comparer.Equals(FActual, Expected) then
    raise ETestAssertionError.CreateFmt('Expected %s to be %s',
      [FormatValue(FActual), FormatValue(Expected)]);
end;

function Expect<T>(Value: T): TExpect<T>;
begin
  Result.FActual := Value;
end;

{ TTestSuite }

constructor TTestSuite.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FTests := TList<TTestRegistration>.Create;
  FHasAssertions := False;
end;

destructor TTestSuite.Destroy;
begin
  FTests.Free;
  inherited Destroy;
end;

procedure TTestSuite.BeforeAll;
begin
  // Override in derived classes
end;

procedure TTestSuite.AfterAll;
begin
  // Override in derived classes
end;

procedure TTestSuite.BeforeEach;
begin
  // Override in derived classes
end;

procedure TTestSuite.AfterEach;
begin
  // Override in derived classes
end;

procedure TTestSuite.Test(const Name: string; Method: TTestMethod);
var
  Registration: TTestRegistration;
begin
  Registration.Name := Name;
  Registration.Method := Method;
  Registration.Skip := False;
  Registration.SkipReason := '';
  FTests.Add(Registration);
end;

procedure TTestSuite.Skip(const Name: string; Method: TTestMethod; const Reason: string);
var
  Registration: TTestRegistration;
begin
  Registration.Name := Name;
  Registration.Method := Method;
  Registration.Skip := True;
  Registration.SkipReason := Reason;
  FTests.Add(Registration);
end;

procedure TTestSuite.Fail(const Message: string);
begin
  raise ETestAssertionError.Create(Message);
end;

{ TTestRunner }

constructor TTestRunner.Create;
begin
  inherited Create;
  FSuites := TList<TTestSuite>.Create;
  FResults := TList<TTestResult>.Create;
end;

destructor TTestRunner.Destroy;
var
  Suite: TTestSuite;
begin
  for Suite in FSuites do
    Suite.Free;
  FSuites.Free;
  FResults.Free;
  inherited Destroy;
end;

procedure TTestRunner.AddSuite(SuiteClass: TTestSuiteClass);
var
  Suite: TTestSuite;
begin
  Suite := SuiteClass.Create(SuiteClass.ClassName);
  Suite.FRunner := Self;
  Suite.SetupTests;
  FSuites.Add(Suite);
end;

procedure TTestRunner.AddSuite(Suite: TTestSuite);
begin
  Suite.FRunner := Self;
  Suite.SetupTests;
  FSuites.Add(Suite);
end;

procedure TTestRunner.RunTest(Suite: TTestSuite; const Test: TTestRegistration);
var
  StartTime: TDateTime;
  Result: TTestResult;
begin
  if Test.Skip then
  begin
    Result.Name := Test.Name;
    Result.SuiteName := Suite.Name;
    Result.Status := tsSkip;
    Result.ErrorMessage := Test.SkipReason;
    Result.Duration := 0;
    FResults.Add(Result);
    Exit;
  end;

  Suite.FCurrentTestName := Test.Name;
  Suite.FHasAssertions := False;
  _ActiveTestSuite := Suite;

  StartTime := Now;
  try
    Suite.BeforeEach;
    Test.Method;
    Suite.AfterEach;

    if not Suite.FHasAssertions then
      raise ETestAssertionError.Create('Test has no assertions');

    Result.Status := tsPass;
  except
    on E: ETestAssertionError do
    begin
      Result.Status := tsFail;
      Result.ErrorMessage := E.Message;
    end;
    on E: Exception do
    begin
      Result.Status := tsFail;
      Result.ErrorMessage := E.Message;
    end;
  end;

  Result.Name := Test.Name;
  Result.SuiteName := Suite.Name;
  Result.Duration := MilliSecondsBetween(Now, StartTime);
  FResults.Add(Result);
end;

procedure TTestRunner.Run;
var
  Suite: TTestSuite;
  Test: TTestRegistration;
begin
  WriteLn;
  WriteLn('Running tests...');
  WriteLn;

  FStartTime := Now;

  for Suite in FSuites do
  begin
    WriteLn('  ', Suite.Name);

    Suite.BeforeAll;
    try
      for Test in Suite.Tests do
      begin
        Suite.FCurrentTestName := Test.Name;
        RunTest(Suite, Test);
      end;

      Suite.AfterAll;
    except
      Suite.AfterAll;
      raise;
    end;
  end;

  PrintResults;
  PrintSummary;
end;

procedure TTestRunner.PrintResults;
var
  Result: TTestResult;
  StatusString: string;
  StatusColor: string;
begin
  WriteLn;
  WriteLn('Test Results:');
  WriteLn('=============');

  for Result in FResults do
  begin
    case Result.Status of
      tsPass:
        begin
          StatusString := '✓';
          StatusColor := #27 + '[32m'; // Green
        end;
      tsFail:
        begin
          StatusString := '✗';
          StatusColor := #27 + '[31m'; // Red
        end;
      tsSkip:
        begin
          StatusString := '○';
          StatusColor := #27 + '[33m'; // Yellow
        end;
    end;

    Write(StatusColor, '  ', StatusString, ' ', #27 + '[0m');
    Write(Result.SuiteName, ' › ', Result.Name);

    if Result.Duration > 0 then
      Write(Format(' (%d ms)', [Result.Duration]));

    WriteLn;

    if Result.Status = tsFail then
      WriteLn('    Error: ', Result.ErrorMessage);

    if (Result.Status = tsSkip) and (Result.ErrorMessage <> '') then
      WriteLn('    Skipped: ', Result.ErrorMessage);
  end;
end;

procedure TTestRunner.PrintSummary;
var
  TotalTests, PassedTests, FailedTests, SkippedTests: Integer;
  Duration: Int64;
  Result: TTestResult;
begin
  TotalTests := 0;
  PassedTests := 0;
  FailedTests := 0;
  SkippedTests := 0;

  for Result in FResults do
  begin
    Inc(TotalTests);
    case Result.Status of
      tsPass: Inc(PassedTests);
      tsFail: Inc(FailedTests);
      tsSkip: Inc(SkippedTests);
    end;
  end;

  Duration := MilliSecondsBetween(Now, FStartTime);

  WriteLn;
  WriteLn('Summary:');
  WriteLn('========');
  WriteLn(Format('Total:   %d tests', [TotalTests]));

  if PassedTests > 0 then
    WriteLn(Format(#27'[32mPassed:  %d'#27'[0m', [PassedTests]));

  if FailedTests > 0 then
    WriteLn(Format(#27'[31mFailed:  %d'#27'[0m', [FailedTests]));

  if SkippedTests > 0 then
    WriteLn(Format(#27'[33mSkipped: %d'#27'[0m', [SkippedTests]));

  WriteLn(Format('Time:    %d ms', [Duration]));
  WriteLn;

  if FailedTests > 0 then
    WriteLn(#27'[31mTESTS FAILED'#27'[0m')
  else
    WriteLn(#27'[32mALL TESTS PASSED'#27'[0m');
end;

initialization
  TestRunnerProgram := TTestRunner.Create;

finalization
  TestRunnerProgram.Free;

end.
