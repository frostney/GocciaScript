program Goccia.CLI.Stdin.Test;

{$I Goccia.inc}

uses
  StrUtils,
  SysUtils,

  TestingPascalLibrary,

  Goccia.CLI.Stdin;

type
  TCLIStdinTests = class(TTestSuite)
  private
    procedure TestNoArgsAtTerminalShowsUsage;
    procedure TestNoArgsWithRedirectedStdinReadsStdin;
    procedure TestExplicitStdinArgReadsStdinAtTerminal;
    procedure TestExplicitStdinArgReadsStdinWhenRedirected;
    procedure TestPathArgumentsWinOverStdin;
    procedure TestPathArgumentsAtTerminalUsePaths;
    procedure TestDecisionIsTotalOverTheMatrix;
    procedure TestUsageExitCodeIsTwo;
    procedure TestUsageNoteIsEmptyForNonStdinCommands;
    procedure TestUsageNoteNamesTheProgramAndEscapeHatch;
    procedure TestUsageNoteMentionsREPLOnlyWhenRequested;
    procedure TestNoInputMessageIsEmptyForNonStdinCommands;
    procedure TestNoInputMessageNamesTheThreeOuts;
    procedure TestEndOfInputKeysMatchThePlatformConsole;
  public
    procedure SetupTests; override;
  end;

procedure TCLIStdinTests.SetupTests;
begin
  Test('No arguments at an interactive terminal shows usage',
    TestNoArgsAtTerminalShowsUsage);
  Test('No arguments with piped or redirected stdin reads stdin',
    TestNoArgsWithRedirectedStdinReadsStdin);
  Test('Explicit "-" reads stdin even at an interactive terminal',
    TestExplicitStdinArgReadsStdinAtTerminal);
  Test('Explicit "-" reads stdin when redirected',
    TestExplicitStdinArgReadsStdinWhenRedirected);
  Test('Path arguments use paths rather than stdin',
    TestPathArgumentsWinOverStdin);
  Test('Path arguments at an interactive terminal use paths',
    TestPathArgumentsAtTerminalUsePaths);
  Test('Decision covers every combination of the three inputs',
    TestDecisionIsTotalOverTheMatrix);
  Test('Usage-error exit code is 2', TestUsageExitCodeIsTwo);
  Test('Usage note is empty for commands that never read stdin',
    TestUsageNoteIsEmptyForNonStdinCommands);
  Test('Usage note names the program and the "-" escape hatch',
    TestUsageNoteNamesTheProgramAndEscapeHatch);
  Test('Usage note mentions GocciaREPL only when requested',
    TestUsageNoteMentionsREPLOnlyWhenRequested);
  Test('No-input message is empty for commands that never read stdin',
    TestNoInputMessageIsEmptyForNonStdinCommands);
  Test('No-input message names the three ways out',
    TestNoInputMessageNamesTheThreeOuts);
  Test('End-of-input guidance matches the platform console',
    TestEndOfInputKeysMatchThePlatformConsole);
end;

{ Argument order: AHasInputArgs, AHasExplicitStdinArg, AStdinIsTerminal. }

procedure TCLIStdinTests.TestNoArgsAtTerminalShowsUsage;
begin
  Expect<Boolean>(DecideStdinInput(False, False, True) = sdShowUsage)
    .ToBe(True);
end;

procedure TCLIStdinTests.TestNoArgsWithRedirectedStdinReadsStdin;
begin
  Expect<Boolean>(DecideStdinInput(False, False, False) = sdReadStdin)
    .ToBe(True);
end;

procedure TCLIStdinTests.TestExplicitStdinArgReadsStdinAtTerminal;
begin
  Expect<Boolean>(DecideStdinInput(True, True, True) = sdReadStdin)
    .ToBe(True);
end;

procedure TCLIStdinTests.TestExplicitStdinArgReadsStdinWhenRedirected;
begin
  Expect<Boolean>(DecideStdinInput(True, True, False) = sdReadStdin)
    .ToBe(True);
end;

procedure TCLIStdinTests.TestPathArgumentsWinOverStdin;
begin
  Expect<Boolean>(DecideStdinInput(True, False, False) = sdUsePaths)
    .ToBe(True);
end;

procedure TCLIStdinTests.TestPathArgumentsAtTerminalUsePaths;
begin
  Expect<Boolean>(DecideStdinInput(True, False, True) = sdUsePaths)
    .ToBe(True);
end;

{ The two remaining rows are unreachable from a real CLI ("-" implies a
  positional argument), but the function must still be total: an
  explicit stdin marker always means "read stdin". }
procedure TCLIStdinTests.TestDecisionIsTotalOverTheMatrix;
begin
  Expect<Boolean>(DecideStdinInput(False, True, True) = sdReadStdin)
    .ToBe(True);
  Expect<Boolean>(DecideStdinInput(False, True, False) = sdReadStdin)
    .ToBe(True);
end;

procedure TCLIStdinTests.TestUsageExitCodeIsTwo;
begin
  Expect<Integer>(EXIT_CODE_USAGE).ToBe(2);
end;

procedure TCLIStdinTests.TestUsageNoteIsEmptyForNonStdinCommands;
begin
  Expect<string>(StdinUsageNote('GocciaREPL', suNone)).ToBe('');
end;

procedure TCLIStdinTests.TestUsageNoteNamesTheProgramAndEscapeHatch;
var
  Note: string;
begin
  Note := StdinUsageNote('GocciaTestRunner', suStdinDefault);
  Expect<Boolean>(ContainsStr(Note, 'GocciaTestRunner')).ToBe(True);
  Expect<Boolean>(ContainsStr(Note, '"-"')).ToBe(True);
  Expect<Boolean>(ContainsStr(Note, EndOfInputKeys)).ToBe(True);
  Expect<Boolean>(ContainsStr(Note, 'exits 2')).ToBe(True);
end;

procedure TCLIStdinTests.TestUsageNoteMentionsREPLOnlyWhenRequested;
begin
  Expect<Boolean>(ContainsStr(
    StdinUsageNote('GocciaTestRunner', suStdinDefault), 'GocciaREPL'))
    .ToBe(False);
  Expect<Boolean>(ContainsStr(
    StdinUsageNote('GocciaScriptLoader', suStdinDefaultWithREPL),
    'GocciaREPL')).ToBe(True);
end;

procedure TCLIStdinTests.TestNoInputMessageIsEmptyForNonStdinCommands;
begin
  Expect<string>(NoInputAtTerminalMessage('GocciaREPL', suNone)).ToBe('');
end;

procedure TCLIStdinTests.TestNoInputMessageNamesTheThreeOuts;
var
  Message: string;
begin
  Message := NoInputAtTerminalMessage('GocciaScriptLoader',
    suStdinDefaultWithREPL);
  Expect<Boolean>(ContainsStr(Message, 'Error:')).ToBe(True);
  Expect<Boolean>(ContainsStr(Message, 'GocciaScriptLoader < app.js'))
    .ToBe(True);
  Expect<Boolean>(ContainsStr(Message, '"-"')).ToBe(True);
  Expect<Boolean>(ContainsStr(Message, 'GocciaREPL')).ToBe(True);
  Expect<Boolean>(ContainsStr(Message, EndOfInputKeys)).ToBe(True);
end;

{ The guidance has to name the key sequence the local console actually
  honours: a Unix terminal ends input on Ctrl-D, a Windows console on
  Ctrl-Z followed by Enter. }
procedure TCLIStdinTests.TestEndOfInputKeysMatchThePlatformConsole;
begin
{$IFDEF MSWINDOWS}
  Expect<string>(EndOfInputKeys).ToBe('Ctrl-Z then Enter');
{$ELSE}
  Expect<string>(EndOfInputKeys).ToBe('Ctrl-D');
{$ENDIF}
end;

begin
  TestRunnerProgram.AddSuite(TCLIStdinTests.Create('CLI stdin policy'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
