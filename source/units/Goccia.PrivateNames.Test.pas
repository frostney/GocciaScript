program Goccia.PrivateNames.Test;

{$I Goccia.inc}

uses
  TestingPascalLibrary,

  Goccia.PrivateNames;

type
  TPrivateNamesTests = class(TTestSuite)
  private
    procedure TestCompilerStorageKey;
    procedure TestRuntimeStorageKey;
    procedure TestNameContainingDollar;
    procedure TestOtherNamesUnchanged;
  public
    procedure SetupTests; override;
  end;

procedure TPrivateNamesTests.SetupTests;
begin
  Test('a compiler storage key names its private member',
    TestCompilerStorageKey);
  Test('a runtime storage key names its private member',
    TestRuntimeStorageKey);
  Test('a private name containing a dollar sign survives',
    TestNameContainingDollar);
  Test('names that are not storage keys are unchanged',
    TestOtherNamesUnchanged);
end;

procedure TPrivateNamesTests.TestCompilerStorageKey;
begin
  Expect<string>(PrivateStorageSourceName('#slot:0$p')).ToBe('p');
  Expect<string>(DisplayClassElementName('#slot:12$field')).ToBe('#field');
end;

procedure TPrivateNamesTests.TestRuntimeStorageKey;
begin
  Expect<string>(PrivateStorageSourceName('#slot:00007F12AB34:p')).ToBe('p');
  Expect<string>(DisplayClassElementName('#slot:00007F12AB34:p')).ToBe('#p');
end;

procedure TPrivateNamesTests.TestNameContainingDollar;
begin
  Expect<string>(DisplayClassElementName('#slot:0$a$b')).ToBe('#a$b');
  Expect<string>(DisplayClassElementName('#slot:00007F12AB34:a$b'))
    .ToBe('#a$b');
end;

procedure TPrivateNamesTests.TestOtherNamesUnchanged;
begin
  Expect<string>(DisplayClassElementName('method')).ToBe('method');
  Expect<string>(DisplayClassElementName('#slot:')).ToBe('#slot:');
  Expect<string>(PrivateStorageSourceName('#brand:0$p')).ToBe('#brand:0$p');
  Expect<string>(PrivateStorageSourceName('')).ToBe('');
end;

begin
  TestRunnerProgram.AddSuite(TPrivateNamesTests.Create('Goccia.PrivateNames'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
