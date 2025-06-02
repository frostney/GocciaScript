unit TestRunner;

{$I Goccia.inc}

interface

uses
  SysUtils, Classes, Generics.Collections, TypInfo, DateUtils;

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

  // Generic expectation class for fluent assertions
  TExpect<T> = class
  private
    FActual: T;
    FNegated: Boolean;
    procedure AssertTrue(Condition: Boolean; const Message: string);
  public
    constructor Create(const Value: T);
    function Not_: TExpect<T>;
    procedure ToBe(const Expected: T);
    procedure ToEqual(const Expected: T);
    procedure ToBeTrue;
    procedure ToBeFalse;
    procedure ToBeNil;
    procedure ToBeGreaterThan(const Value: T);
    procedure ToBeLessThan(const Value: T);
    procedure ToContain(const Substring: string); // For string types
    procedure ToHaveLength(const Length: Integer); // For arrays/strings
  end;

  // Base test suite class
  TTestSuite = class
  private
    FName: string;
    FRunner: TTestRunner;
    FCurrentTestName: string;
    FTests: TList<TTestRegistration>;
  protected
    // Lifecycle hooks
    procedure BeforeAll; virtual;
    procedure AfterAll; virtual;
    procedure BeforeEach; virtual;
    procedure AfterEach; virtual;

    // Test registration
    procedure Test(const Name: string; Method: TTestMethod);
    procedure Skip(const Name: string; Method: TTestMethod; const Reason: string = '');

    // Assertion helpers
    function Expect<T>(const Value: T): TExpect<T>;
    procedure Fail(const Message: string);

    // Override this to register your tests
    procedure SetupTests; virtual; abstract;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
    property Name: string read FName;
    property Tests: TList<TTestRegistration> read FTests;
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

// Global test runner instance
var
  TestRunnerProgram: TTestRunner;

// Helper function to create expectations
function Expect<T>(const Value: T): TExpect<T>;

implementation

var
  CurrentDescribeSuite: TTestSuite;

{ TExpect<T> }

constructor TExpect<T>.Create(const Value: T);
begin
  FActual := Value;
  FNegated := False;
end;

function TExpect<T>.Not_: TExpect<T>;
begin
  FNegated := not FNegated;
  Result := Self;
end;

procedure TExpect<T>.AssertTrue(Condition: Boolean; const Message: string);
begin
  if FNegated then
    Condition := not Condition;

  if not Condition then
    raise ETestAssertionError.Create(Message);
end;

procedure TExpect<T>.ToBe(const Expected: T);
var
  TypeInfo: PTypeInfo;
  TypeData: PTypeData;
  IsEqual: Boolean;
  ActualStr, ExpectedStr: string;
begin
  TypeInfo := System.TypeInfo(T);
  TypeData := GetTypeData(TypeInfo);

  case TypeInfo^.Kind of
    tkInteger, tkInt64:
      begin
        IsEqual := PInt64(@FActual)^ = PInt64(@Expected)^;
        ActualStr := IntToStr(PInt64(@FActual)^);
        ExpectedStr := IntToStr(PInt64(@Expected)^);
      end;
    tkFloat:
      begin
        IsEqual := Abs(PDouble(@FActual)^ - PDouble(@Expected)^) < 1E-10;
        ActualStr := FloatToStr(PDouble(@FActual)^);
        ExpectedStr := FloatToStr(PDouble(@Expected)^);
      end;
    tkString, tkLString, tkUString, tkAString:
      begin
        IsEqual := PString(@FActual)^ = PString(@Expected)^;
        ActualStr := '"' + PString(@FActual)^ + '"';
        ExpectedStr := '"' + PString(@Expected)^ + '"';
      end;
    tkEnumeration:
      if TypeData^.BaseType = System.TypeInfo(Boolean) then
      begin
        IsEqual := PBoolean(@FActual)^ = PBoolean(@Expected)^;
        if PBoolean(@FActual)^ then ActualStr := 'True' else ActualStr := 'False';
        if PBoolean(@Expected)^ then ExpectedStr := 'True' else ExpectedStr := 'False';
      end
      else
      begin
        IsEqual := PByte(@FActual)^ = PByte(@Expected)^;
        ActualStr := IntToStr(PByte(@FActual)^);
        ExpectedStr := IntToStr(PByte(@Expected)^);
      end;
    tkClass:
      begin
        IsEqual := PPointer(@FActual)^ = PPointer(@Expected)^;
        if PPointer(@FActual)^ = nil then
          ActualStr := 'nil'
        else
          ActualStr := 'object';
        if PPointer(@Expected)^ = nil then
          ExpectedStr := 'nil'
        else
          ExpectedStr := 'object';
      end;
    else
      begin
        IsEqual := CompareMem(@FActual, @Expected, SizeOf(T));
        ActualStr := 'value';
        ExpectedStr := 'value';
      end;
  end;

  if FNegated then
    AssertTrue(IsEqual, Format('Expected %s not to be %s', [ActualStr, ExpectedStr]))
  else
    AssertTrue(IsEqual, Format('Expected %s to be %s', [ActualStr, ExpectedStr]));
end;

procedure TExpect<T>.ToEqual(const Expected: T);
begin
  ToBe(Expected);
end;

procedure TExpect<T>.ToBeTrue;
var
  Val: Boolean;
begin
  if TypeInfo(T) <> TypeInfo(Boolean) then
    raise ETestAssertionError.Create('ToBeTrue can only be used with Boolean values');

  Val := PBoolean(@FActual)^;
  AssertTrue(Val, 'Expected value to be True');
end;

procedure TExpect<T>.ToBeFalse;
var
  Val: Boolean;
begin
  if TypeInfo(T) <> TypeInfo(Boolean) then
    raise ETestAssertionError.Create('ToBeFalse can only be used with Boolean values');

  Val := PBoolean(@FActual)^;
  AssertTrue(not Val, 'Expected value to be False');
end;

procedure TExpect<T>.ToBeNil;
var
  TypeInfo: PTypeInfo;
begin
  TypeInfo := System.TypeInfo(T);
  if TypeInfo^.Kind <> tkClass then
    raise ETestAssertionError.Create('ToBeNil can only be used with class types');

  AssertTrue(PPointer(@FActual)^ = nil, 'Expected value to be nil');
end;

procedure TExpect<T>.ToBeGreaterThan(const Value: T);
var
  TypeInfo: PTypeInfo;
  TypeData: PTypeData;
  IsGreater: Boolean;
  ActualStr, ValueStr: string;
begin
  TypeInfo := System.TypeInfo(T);
  TypeData := GetTypeData(TypeInfo);

  case TypeInfo^.Kind of
    tkInteger:
      begin
        IsGreater := PInteger(@FActual)^ > PInteger(@Value)^;
        ActualStr := IntToStr(PInteger(@FActual)^);
        ValueStr := IntToStr(PInteger(@Value)^);
      end;
    tkInt64:
      begin
        IsGreater := PInt64(@FActual)^ > PInt64(@Value)^;
        ActualStr := IntToStr(PInt64(@FActual)^);
        ValueStr := IntToStr(PInt64(@Value)^);
      end;
    tkFloat:
      begin
        IsGreater := PDouble(@FActual)^ > PDouble(@Value)^;
        ActualStr := FloatToStr(PDouble(@FActual)^);
        ValueStr := FloatToStr(PDouble(@Value)^);
      end;
    else
      raise ETestAssertionError.Create('ToBeGreaterThan can only be used with numeric types');
  end;

  AssertTrue(IsGreater, Format('Expected %s to be greater than %s', [ActualStr, ValueStr]));
end;

procedure TExpect<T>.ToBeLessThan(const Value: T);
var
  TypeInfo: PTypeInfo;
  TypeData: PTypeData;
  IsLess: Boolean;
  ActualStr, ValueStr: string;
begin
  TypeInfo := System.TypeInfo(T);
  TypeData := GetTypeData(TypeInfo);

  case TypeInfo^.Kind of
    tkInteger:
      begin
        IsLess := PInteger(@FActual)^ < PInteger(@Value)^;
        ActualStr := IntToStr(PInteger(@FActual)^);
        ValueStr := IntToStr(PInteger(@Value)^);
      end;
    tkInt64:
      begin
        IsLess := PInt64(@FActual)^ < PInt64(@Value)^;
        ActualStr := IntToStr(PInt64(@FActual)^);
        ValueStr := IntToStr(PInt64(@Value)^);
      end;
    tkFloat:
      begin
        IsLess := PDouble(@FActual)^ < PDouble(@Value)^;
        ActualStr := FloatToStr(PDouble(@FActual)^);
        ValueStr := FloatToStr(PDouble(@Value)^);
      end;
    else
      raise ETestAssertionError.Create('ToBeLessThan can only be used with numeric types');
  end;

  AssertTrue(IsLess, Format('Expected %s to be less than %s', [ActualStr, ValueStr]));
end;

procedure TExpect<T>.ToContain(const Substring: string);
var
  Str: string;
begin
  if TypeInfo(T) <> TypeInfo(string) then
    raise ETestAssertionError.Create('ToContain can only be used with string types');

  Str := PString(@FActual)^;
  AssertTrue(Pos(Substring, Str) > 0, Format('Expected "%s" to contain "%s"', [Str, Substring]));
end;

procedure TExpect<T>.ToHaveLength(const Length: Integer);
var
  Str: string;
  ActualLength: Integer;
begin
  if TypeInfo(T) = TypeInfo(string) then
  begin
    Str := PString(@FActual)^;
    ActualLength := System.Length(Str);
  end
  else
    raise ETestAssertionError.Create('ToHaveLength can only be used with string or array types');

  AssertTrue(ActualLength = Length, Format('Expected length %d but got %d', [Length, ActualLength]));
end;

{ TTestSuite }

constructor TTestSuite.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FTests := TList<TTestRegistration>.Create;
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

function TTestSuite.Expect<T>(const Value: T): TExpect<T>;
begin
  Result := TExpect<T>.Create(Value);
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
  Result: TTestResult;
  StartTime: TDateTime;
begin
  Result.Name := Test.Name;
  Result.SuiteName := Suite.Name;
  Result.Status := tsPass;
  Result.ErrorMessage := '';

  if Test.Skip then
  begin
    Result.Status := tsSkip;
    Result.ErrorMessage := Test.SkipReason;
  end
  else
  begin
    StartTime := Now;
    try
      Suite.BeforeEach;
      try
        Test.Method();
        Suite.AfterEach;
      except
        Suite.AfterEach;
        raise;
      end;
    except
      on E: Exception do
      begin
        Result.Status := tsFail;
        Result.ErrorMessage := E.Message;
      end;
    end;
    Result.Duration := MilliSecondsBetween(Now, StartTime);
  end;

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

{ Global functions }

function Expect<T>(const Value: T): TExpect<T>;
begin
  Result := TExpect<T>.Create(Value);
end;

initialization
  TestRunnerProgram := TTestRunner.Create;

finalization
  TestRunnerProgram.Free;

end.
