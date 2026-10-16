program TestingPascalLibrary.Test;

{$I Shared.inc}

uses
  SysUtils,

  TestingPascalLibrary;

type
  TTestingPascalLibraryTests = class(TTestSuite)
  private
    procedure TestStringMismatchShowsBothStrings;
    procedure TestNonASCIIStringMismatchShowsBothStrings;
    procedure TestAnsiStringMismatchShowsBothStrings;
    procedure TestWideStringMismatchShowsBothStrings;
    procedure TestShortStringMismatchShowsBothStrings;
  public
    procedure SetupTests; override;
  end;

const
  ACTUAL_TEXT = 'evil';
  EXPECTED_TEXT = 'Provider package file github:frostney/raylib@v1.0.0/' +
    'vendor/config.yaml changed after it was verified';
  MISMATCH_MESSAGE = 'Expected "' + ACTUAL_TEXT + '" to be "' +
    EXPECTED_TEXT + '"';

procedure TTestingPascalLibraryTests.SetupTests;
begin
  Test('A failing string assertion shows both strings in full',
    TestStringMismatchShowsBothStrings);
  Test('A failing string assertion shows non-ASCII text as text',
    TestNonASCIIStringMismatchShowsBothStrings);
  Test('A failing AnsiString assertion shows both strings in full',
    TestAnsiStringMismatchShowsBothStrings);
  Test('A failing WideString assertion shows both strings in full',
    TestWideStringMismatchShowsBothStrings);
  Test('A failing ShortString assertion shows both strings in full',
    TestShortStringMismatchShowsBothStrings);
end;

procedure TTestingPascalLibraryTests.TestStringMismatchShowsBothStrings;
var
  Message: string;
begin
  Message := '';
  try
    Expect<string>(ACTUAL_TEXT).ToBe(EXPECTED_TEXT);
  except
    on E: ETestAssertionError do
      Message := E.Message;
  end;
  Expect<string>(Message).ToBe(MISMATCH_MESSAGE);
end;

procedure TTestingPascalLibraryTests.TestNonASCIIStringMismatchShowsBothStrings;
var
  Expected, Message: string;
begin
  Message := '';
  try
    Expect<string>('caf'#$00E9).ToBe(#$00FC'ber '#$D83D#$DE00);
  except
    on E: ETestAssertionError do
      Message := E.Message;
  end;
  { The RTL's Exception keeps its message as an AnsiString in the system
    code page, so on Windows a character outside it comes back as '?'. That
    loss happens after FormatValue; expect the message as Exception holds it.
    The text goes through a variable so that the conversion runs when the
    test runs, not when the compiler folds a constant. }
  Expected := 'Expected "caf'#$00E9'" to be "'#$00FC'ber '#$D83D#$DE00'"';
  Expect<string>(Message).ToBe(string(AnsiString(Expected)));
end;

procedure TTestingPascalLibraryTests.TestAnsiStringMismatchShowsBothStrings;
var
  Message: string;
begin
  Message := '';
  try
    Expect<AnsiString>(AnsiString(ACTUAL_TEXT)).ToBe(
      AnsiString(EXPECTED_TEXT));
  except
    on E: ETestAssertionError do
      Message := E.Message;
  end;
  Expect<string>(Message).ToBe(MISMATCH_MESSAGE);
end;

procedure TTestingPascalLibraryTests.TestWideStringMismatchShowsBothStrings;
var
  Message: string;
begin
  Message := '';
  try
    Expect<WideString>(WideString(ACTUAL_TEXT)).ToBe(
      WideString(EXPECTED_TEXT));
  except
    on E: ETestAssertionError do
      Message := E.Message;
  end;
  Expect<string>(Message).ToBe(MISMATCH_MESSAGE);
end;

procedure TTestingPascalLibraryTests.TestShortStringMismatchShowsBothStrings;
var
  Message: string;
begin
  Message := '';
  try
    Expect<ShortString>(ShortString(ACTUAL_TEXT)).ToBe(
      ShortString(EXPECTED_TEXT));
  except
    on E: ETestAssertionError do
      Message := E.Message;
  end;
  Expect<string>(Message).ToBe(MISMATCH_MESSAGE);
end;

begin
  TestRunnerProgram.AddSuite(TTestingPascalLibraryTests.Create(
    'TestingPascalLibrary'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
