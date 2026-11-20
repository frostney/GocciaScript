{ Unit tests for the pure Intl helpers in source/shared/IntlTypes.pas.

  ICUNameFieldsInPattern decides which iso8601 formats fall back to Gregorian
  names when the loaded ICU lacks them (IntlICU), so a numeric date must
  report no name field while text months and eras report theirs. }

program IntlTypes.Test;

{$I Shared.inc}

uses
  IntlTypes,
  TestingPascalLibrary;

type
  TIntlTypesTests = class(TTestSuite)
  private
    procedure TestNumericFieldsNeedNoNames;
    procedure TestMonthNameWidths;
    procedure TestEraWidths;
    procedure TestQuotedTextIsSkipped;
  public
    procedure SetupTests; override;
  end;

procedure TIntlTypesTests.SetupTests;
begin
  Test('numeric dates and times use no name fields',
    TestNumericFieldsNeedNoNames);
  Test('month names are reported by width and context', TestMonthNameWidths);
  Test('eras are reported by width', TestEraWidths);
  Test('quoted literal text is not a field', TestQuotedTextIsSkipped);
end;

procedure TIntlTypesTests.TestNumericFieldsNeedNoNames;
begin
  Expect<Cardinal>(ICUNameFieldsInPattern('yMd')).ToBe(0);
  Expect<Cardinal>(ICUNameFieldsInPattern('y-MM-dd')).ToBe(0);
  Expect<Cardinal>(ICUNameFieldsInPattern('EEEEyMdHHmmsszzzz')).ToBe(0);
  Expect<Cardinal>(ICUNameFieldsInPattern('')).ToBe(0);
end;

procedure TIntlTypesTests.TestMonthNameWidths;
begin
  Expect<Cardinal>(ICUNameFieldsInPattern('yMMMd')).ToBe(
    ICU_NAME_MONTH_ABBREVIATED);
  Expect<Cardinal>(ICUNameFieldsInPattern('yMMMMEEEEd')).ToBe(
    ICU_NAME_MONTH_WIDE);
  Expect<Cardinal>(ICUNameFieldsInPattern('MMMMM')).ToBe(
    ICU_NAME_MONTH_NARROW);
  Expect<Cardinal>(ICUNameFieldsInPattern('LLL LLLL LLLLL')).ToBe(
    ICU_NAME_STANDALONE_MONTH_ABBREVIATED or ICU_NAME_STANDALONE_MONTH_WIDE or
    ICU_NAME_STANDALONE_MONTH_NARROW);
end;

procedure TIntlTypesTests.TestEraWidths;
begin
  Expect<Cardinal>(ICUNameFieldsInPattern('GyMMMMd')).ToBe(
    ICU_NAME_ERA_ABBREVIATED or ICU_NAME_MONTH_WIDE);
  Expect<Cardinal>(ICUNameFieldsInPattern('GGGy')).ToBe(
    ICU_NAME_ERA_ABBREVIATED);
  Expect<Cardinal>(ICUNameFieldsInPattern('GGGGy')).ToBe(ICU_NAME_ERA_WIDE);
  Expect<Cardinal>(ICUNameFieldsInPattern('GGGGGyMd')).ToBe(
    ICU_NAME_ERA_NARROW);
end;

procedure TIntlTypesTests.TestQuotedTextIsSkipped;
begin
  Expect<Cardinal>(ICUNameFieldsInPattern('y ''MMMM'' d')).ToBe(0);
  Expect<Cardinal>(ICUNameFieldsInPattern('h ''o''''clock G''')).ToBe(0);
  Expect<Cardinal>(ICUNameFieldsInPattern('''G'' MMM')).ToBe(
    ICU_NAME_MONTH_ABBREVIATED);
end;

begin
  TestRunnerProgram.AddSuite(TIntlTypesTests.Create('IntlTypes'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
