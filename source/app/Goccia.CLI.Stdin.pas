unit Goccia.CLI.Stdin;

{$I Goccia.inc}

{ Stdin sourcing policy shared by every Goccia CLI that can take its
  program source from standard input.

  The rule comes from https://clig.dev/ : "If your command is expecting
  to have something piped to it and stdin is an interactive terminal,
  display help immediately and quit ... doesn't just hang, like cat."

  The decision itself is a pure function of three booleans so the whole
  matrix is unit-testable without a real terminal; the platform probe
  lives next to it but is never called from the decision. }

interface

const
  { Usage-error exit code.  This matches the convention already in the
    repository: GocciaWasmTestRunner exits 2 when invoked without a
    manifest, and GocciaTOMLComplianceRunner exits 2 for an unusable
    invocation.  Runtime and script failures keep exit code 1. }
  EXIT_CODE_USAGE = 2;

type
  { Where a CLI invocation should take its program source from. }
  TGocciaStdinDecision = (
    { Read the program from stdin — implicitly (no positional
      arguments, stdin piped or redirected) or explicitly ("-"). }
    sdReadStdin,
    { Use the positional arguments as input paths. }
    sdUsePaths,
    { No positional arguments and stdin is an interactive terminal:
      reading stdin would silently hang, so print usage and quit. }
    sdShowUsage);

  { How a command relates to stdin.  Commands that never default to
    stdin (GocciaREPL, GocciaSandboxRunner) stay on suNone and are
    unaffected by the no-argument rule. }
  TGocciaStdinUsage = (
    suNone,
    suStdinDefault,
    suStdinDefaultWithREPL);

{ The no-argument rule as a pure function.

    explicit "-"        -> sdReadStdin   (opt-in, terminal or not)
    positional paths    -> sdUsePaths
    no args, not a TTY  -> sdReadStdin   (pipes and redirects, unchanged)
    no args, a TTY      -> sdShowUsage

  AHasExplicitStdinArg wins over AHasInputArgs: "-" is itself a
  positional argument, and passing it is the documented escape hatch
  for reading the terminal on purpose. }
function DecideStdinInput(const AHasInputArgs, AHasExplicitStdinArg,
  AStdinIsTerminal: Boolean): TGocciaStdinDecision;

{ True when standard input is attached to an interactive terminal.
  Never mutates console state — unlike IsColorTerminal, which enables
  virtual-terminal processing on the Windows output handle. }
function IsInputTerminal: Boolean;

{ The "Input:" section appended to --help for stdin-defaulting
  commands.  Returns '' for suNone. }
function StdinUsageNote(const AProgramName: string;
  const AUsage: TGocciaStdinUsage): string;

{ The stderr message printed under the usage text when a command is
  run with no input at an interactive terminal.  Returns '' for
  suNone. }
function NoInputAtTerminalMessage(const AProgramName: string;
  const AUsage: TGocciaStdinUsage): string;

implementation

uses
{$IFDEF UNIX}
  termio,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils;

function DecideStdinInput(const AHasInputArgs, AHasExplicitStdinArg,
  AStdinIsTerminal: Boolean): TGocciaStdinDecision;
begin
  if AHasExplicitStdinArg then
    Exit(sdReadStdin);
  if AHasInputArgs then
    Exit(sdUsePaths);
  if AStdinIsTerminal then
    Exit(sdShowUsage);
  Result := sdReadStdin;
end;

function IsInputTerminal: Boolean;
{$IFDEF MSWINDOWS}
var
  Handle: THandle;
  Mode: DWORD;
{$ENDIF}
begin
  {$IFDEF UNIX}
  Result := IsATTY(StdInputHandle) <> 0;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  Handle := GetStdHandle(STD_INPUT_HANDLE);
  if (Handle = INVALID_HANDLE_VALUE) or (Handle = 0) then
    Exit(False);
  // GetConsoleMode succeeds only for console input handles, and unlike
  // SetConsoleMode it leaves the console untouched.  Pipes, files and
  // redirected handles fail here, which is exactly the "not a terminal"
  // answer we want.
  Result := GetConsoleMode(Handle, Mode);
  {$ELSE}
  Result := False;
  {$ENDIF}
  {$ENDIF}
end;

function StdinUsageNote(const AProgramName: string;
  const AUsage: TGocciaStdinUsage): string;
begin
  if AUsage = suNone then
    Exit('');

  Result :=
    'Input:' + sLineBreak +
    '  With no path, ' + AProgramName + ' reads the program from ' +
      'standard input,' + sLineBreak +
    '  so a script can be piped or redirected:  ' + AProgramName +
      ' < app.js' + sLineBreak +
    '  At an interactive terminal that would hang waiting for input, ' +
      'so' + sLineBreak +
    '  ' + AProgramName + ' prints this help and exits ' +
      IntToStr(EXIT_CODE_USAGE) + ' instead.  Pass "-" to' + sLineBreak +
    '  read from the terminal anyway (finish with Ctrl-D).' + sLineBreak;

  if AUsage = suStdinDefaultWithREPL then
    Result := Result +
      '  For an interactive session use GocciaREPL.' + sLineBreak;
end;

function NoInputAtTerminalMessage(const AProgramName: string;
  const AUsage: TGocciaStdinUsage): string;
begin
  if AUsage = suNone then
    Exit('');

  Result :=
    'Error: no input path given and standard input is a terminal.' +
      sLineBreak +
    '  - pass a file or directory path' + sLineBreak +
    '  - pipe or redirect a script:  ' + AProgramName + ' < app.js' +
      sLineBreak +
    '  - pass "-" to read from the terminal (finish with Ctrl-D)' +
      sLineBreak;

  if AUsage = suStdinDefaultWithREPL then
    Result := Result +
      '  - use GocciaREPL for an interactive session' + sLineBreak;
end;

end.
