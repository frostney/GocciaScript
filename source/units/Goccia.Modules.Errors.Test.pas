program Goccia.Modules.Errors.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.Constants.PropertyNames,
  Goccia.Error.Suggestions,
  Goccia.Modules.Errors,
  Goccia.TestSetup,
  Goccia.Values.Error,
  Goccia.Values.ObjectValue;

type
  TModuleErrorTests = class(TTestSuite)
  private
    procedure TestCarriesTheStableCode;
    procedure TestOmitsTheResolvedModuleAddress;
    procedure TestThrowsAsAScriptVisibleValue;
  public
    procedure SetupTests; override;
  end;

const
  { Spelled out rather than taken from Goccia.Modules.Errors: hosts branch on
    this exact text, so a test that compared against the constant would stay
    green while the value changed underneath every one of them. }
  EXPECTED_CODE = 'ERR_MODULE_LOADING_UNSUPPORTED';
  EXPECTED_MESSAGE =
    'Cannot load module: no module content provider is configured';

procedure TModuleErrorTests.SetupTests;
begin
  Test('Carries the stable module loading code',
    TestCarriesTheStableCode);
  Test('Omits the resolved module address',
    TestOmitsTheResolvedModuleAddress);
  Test('Throws as a script-visible value with a host suggestion',
    TestThrowsAsAScriptVisibleValue);
end;

procedure TModuleErrorTests.TestCarriesTheStableCode;
var
  ErrorObject: TGocciaObjectValue;
begin
  ErrorObject := CreateModuleLoadingUnsupportedError;
  try
    Expect<Boolean>(ErrorObject.HasErrorData).ToBe(True);
    { A plain Error, not a TypeError: TypeError is the convention for a
      configured loader that tried and failed. }
    Expect<string>(ErrorObject.GetProperty(PROP_NAME).ToStringLiteral.Value)
      .ToBe('Error');
    Expect<string>(ErrorObject.GetProperty(PROP_CODE).ToStringLiteral.Value)
      .ToBe(EXPECTED_CODE);
    Expect<string>(ErrorObject.GetProperty(PROP_MESSAGE).ToStringLiteral.Value)
      .ToBe(EXPECTED_MESSAGE);
  finally
    ErrorObject.Free;
  end;
end;

procedure TModuleErrorTests.TestOmitsTheResolvedModuleAddress;
var
  ErrorObject: TGocciaObjectValue;
begin
  ErrorObject := CreateModuleLoadingUnsupportedError;
  try
    { The refusal is reported to potentially untrusted source, and the only
      address available where it is raised is the resolved one — an absolute
      host filesystem path under the default resolver. An enumerable own `path`
      would carry it into JSON.stringify(error) and object spread, so there is
      none, and the message names no address either. }
    Expect<Boolean>(ErrorObject.HasOwnProperty(PROP_PATH)).ToBe(False);
    Expect<Boolean>(Pos('/',
      ErrorObject.GetProperty(PROP_MESSAGE).ToStringLiteral.Value) > 0)
      .ToBe(False);
  finally
    ErrorObject.Free;
  end;
end;

procedure TModuleErrorTests.TestThrowsAsAScriptVisibleValue;
var
  ErrorObject: TGocciaObjectValue;
  Raised: Boolean;
  Suggestion: string;
begin
  ErrorObject := nil;
  Raised := False;
  Suggestion := '';
  try
    ThrowModuleLoadingUnsupported;
  except
    { Not an RTL exception class: an embedder running untrusted source with no
      provider must be able to let source catch this instead of unwinding
      through its engine boundary. }
    on E: TGocciaThrowValue do
    begin
      Raised := True;
      Suggestion := E.Suggestion;
      if E.Value is TGocciaObjectValue then
        ErrorObject := TGocciaObjectValue(E.Value);
    end;
  end;

  Expect<Boolean>(Raised).ToBe(True);
  Expect<Boolean>(Assigned(ErrorObject)).ToBe(True);
  Expect<string>(Suggestion).ToBe(SSuggestConfigureModuleContentProvider);
  if Assigned(ErrorObject) then
  try
    Expect<string>(ErrorObject.GetProperty(PROP_CODE).ToStringLiteral.Value)
      .ToBe(EXPECTED_CODE);
  finally
    ErrorObject.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(
    TModuleErrorTests.Create('ModuleErrors'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
