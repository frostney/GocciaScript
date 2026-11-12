program Goccia.Values.Formatting.Test;

{$I Goccia.inc}

uses
  TestingPascalLibrary,

  Goccia.TestSetup,
  Goccia.Values.ArrayValue,
  Goccia.Values.Formatting,
  Goccia.Values.Primitives;

type
  TTestFormatting = class(TTestSuite)
  private
    procedure TestConsoleNegativeZero;
    procedure TestConsoleNegativeZeroNested;
    procedure TestDisplayZero;
  public
    procedure SetupTests; override;
  end;

procedure TTestFormatting.SetupTests;
begin
  Test('Console output keeps the sign of zero', TestConsoleNegativeZero);
  Test('Console output keeps the sign of a nested zero',
    TestConsoleNegativeZeroNested);
  Test('Console output leaves positive zero and other numbers alone',
    TestDisplayZero);
end;

procedure TTestFormatting.TestConsoleNegativeZero;
begin
  Expect<string>(FormatForConsole(TGocciaNumberLiteralValue.NegativeZeroValue))
    .ToBe('-0');
end;

procedure TTestFormatting.TestConsoleNegativeZeroNested;
var
  ArrayValue: TGocciaArrayValue;
begin
  ArrayValue := TGocciaArrayValue.Create;
  ArrayValue.Elements.Add(TGocciaNumberLiteralValue.NegativeZeroValue);
  ArrayValue.Elements.Add(TGocciaNumberLiteralValue.Create(1));
  Expect<string>(FormatForConsole(ArrayValue)).ToBe('[ -0, 1 ]');
end;

procedure TTestFormatting.TestDisplayZero;
begin
  Expect<string>(FormatForConsole(TGocciaNumberLiteralValue.Create(0)))
    .ToBe('0');
  Expect<string>(FormatForConsole(TGocciaNumberLiteralValue.Create(-1.5)))
    .ToBe('-1.5');
end;

begin
  TestRunnerProgram.AddSuite(TTestFormatting.Create('Value Formatting'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
