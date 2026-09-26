program CLI.Parser.Test;

{$I Shared.inc}

uses
  Classes,
  SysUtils,
  TypInfo,

  CLI.ConfigFile,
  CLI.Options,
  CLI.Parser,
  TestingPascalLibrary,

  Goccia.CLI.Help;

type
  { Local two-value enum for testing EnumOption without coupling to engine types }
  TTestColor = (tcRed, tcBlue);

type
  TCLIOptionsTests = class(TTestSuite)
  private
    procedure TestFlagOptionNotPresentInitially;
    procedure TestFlagOptionApply;
    procedure TestStringOptionApply;
    procedure TestStringOptionValueOrWhenPresent;
    procedure TestStringOptionValueOrWhenAbsent;
    procedure TestStringOptionMarkPresentClearsValue;
    procedure TestOptionalStringOptionParsing;
    procedure TestRequiredValueOptionAcceptsDashPrefixedValues;
    procedure TestIntegerOptionApply;
    procedure TestIntegerOptionValueOrWhenPresent;
    procedure TestIntegerOptionValueOrWhenAbsent;
    procedure TestIntegerOptionInvalidValue;
    procedure TestRepeatableOptionMultipleValues;
    procedure TestEnumOptionFirstValue;
    procedure TestEnumOptionSecondValue;
    procedure TestEnumOptionInvalidValue;
    procedure TestEnumOptionValidValues;
    procedure TestEnumOptionFormatForHelp;
    procedure TestOptionListAddFlagAndString;
    procedure TestConcatOptionsMergesTwoArrays;
    procedure TestGenerateHelpText;
    procedure TestFromCommandLineIsFalseInitially;
    procedure TestMarkFromCommandLineSetsTrue;
    procedure TestConfigAppliedOptionNotFromCommandLine;
    procedure TestFlagRejectsValue;
    procedure TestScopeListBareAndScoped;
    procedure TestScopeListAccumulates;
    procedure TestScopeListDoesNotConsumeNextArgument;
    procedure TestScopeListEmptyListRaises;
    procedure TestScopeListEmptyItemRaises;
    procedure TestScopeListRequiredScope;
    procedure TestRemovedOptionRaises;
    procedure TestHiddenOptionOmittedFromHelp;
    procedure TestUnitOptionsParse;
  public
    procedure SetupTests; override;
  end;

procedure TCLIOptionsTests.SetupTests;
begin
  Test('FlagOption is not present initially', TestFlagOptionNotPresentInitially);
  Test('FlagOption.Apply sets Present to True', TestFlagOptionApply);
  Test('StringOption.Apply stores the value', TestStringOptionApply);
  Test('StringOption.ValueOr returns value when present', TestStringOptionValueOrWhenPresent);
  Test('StringOption.ValueOr returns default when absent', TestStringOptionValueOrWhenAbsent);
  Test('StringOption.MarkPresent clears existing value', TestStringOptionMarkPresentClearsValue);
  Test('OptionalStringOption parsing keeps positionals', TestOptionalStringOptionParsing);
  Test('Required value option accepts dash-prefixed values', TestRequiredValueOptionAcceptsDashPrefixedValues);
  Test('IntegerOption.Apply parses integer value', TestIntegerOptionApply);
  Test('IntegerOption.ValueOr returns value when present', TestIntegerOptionValueOrWhenPresent);
  Test('IntegerOption.ValueOr returns default when absent', TestIntegerOptionValueOrWhenAbsent);
  Test('IntegerOption.Apply raises TParseError for non-integer', TestIntegerOptionInvalidValue);
  Test('RepeatableOption accumulates multiple values', TestRepeatableOptionMultipleValues);
  Test('EnumOption applies first value', TestEnumOptionFirstValue);
  Test('EnumOption applies second value', TestEnumOptionSecondValue);
  Test('EnumOption raises TParseError for invalid value', TestEnumOptionInvalidValue);
  Test('EnumOption.ValidValues returns comma-separated list', TestEnumOptionValidValues);
  Test('EnumOption.FormatForHelp returns pipe-separated values', TestEnumOptionFormatForHelp);
  Test('OptionList tracks added options', TestOptionListAddFlagAndString);
  Test('ConcatOptions merges two arrays', TestConcatOptionsMergesTwoArrays);
  Test('GenerateHelpText includes program name and option names', TestGenerateHelpText);
  Test('FromCommandLine is False initially', TestFromCommandLineIsFalseInitially);
  Test('MarkFromCommandLine sets FromCommandLine to True', TestMarkFromCommandLineSetsTrue);
  Test('Config-applied option has Present but not FromCommandLine', TestConfigAppliedOptionNotFromCommandLine);
  Test('A flag given a value is a usage error', TestFlagRejectsValue);
  Test('Scope list: bare flag and scoped list', TestScopeListBareAndScoped);
  Test('Scope list: repeats accumulate', TestScopeListAccumulates);
  Test('Scope list: the next argument is never consumed',
    TestScopeListDoesNotConsumeNextArgument);
  Test('Scope list: an empty list is an error', TestScopeListEmptyListRaises);
  Test('Scope list: an empty item is an error', TestScopeListEmptyItemRaises);
  Test('Scope list: a required scope is enforced', TestScopeListRequiredScope);
  Test('A removed option raises naming its replacement',
    TestRemovedOptionRaises);
  Test('Hidden options are omitted from help', TestHiddenOptionOmittedFromHelp);
  Test('Byte size, duration, and count options parse units',
    TestUnitOptionsParse);
end;

function ParseMessage(const AArgs: array of string;
  const AOptions: TOptionArray; out AIsUsageError: Boolean): string;
var
  Positionals: TStringList;
begin
  Result := '';
  AIsUsageError := False;
  try
    Positionals := ParseArguments(AArgs, AOptions);
    Positionals.Free;
  except
    on E: TParseError do
    begin
      Result := E.Message;
      AIsUsageError := E is TCLIUsageError;
    end;
  end;
end;

procedure TCLIOptionsTests.TestFlagRejectsValue;
var
  Flag: TFlagOption;
  Options: TOptionArray;
  IsUsageError: Boolean;
begin
  Flag := TFlagOption.Create('silent', 'Silence');
  try
    SetLength(Options, 1);
    Options[0] := Flag;
    Expect<string>(ParseMessage(['--silent=false'], Options, IsUsageError))
      .ToBe('--silent does not take a value; got "false". Omit the flag to ' +
        'leave it off');
    Expect<Boolean>(IsUsageError).ToBe(True);
    Expect<string>(ParseMessage(['--silent='], Options, IsUsageError))
      .ToBe('--silent does not take a value; got "". Omit the flag to ' +
        'leave it off');
    Expect<Boolean>(Flag.Present).ToBe(False);
    Expect<string>(ParseMessage(['--silent'], Options, IsUsageError)).ToBe('');
    Expect<Boolean>(Flag.Present).ToBe(True);
  finally
    Flag.Free;
  end;
end;

procedure TCLIOptionsTests.TestScopeListBareAndScoped;
var
  Opt: TScopeListOption;
  Options: TOptionArray;
  Positionals: TStringList;
begin
  Opt := TScopeListOption.Create('allow-net', 'Allow net', '<host>');
  try
    SetLength(Options, 1);
    Options[0] := Opt;
    Positionals := ParseArguments(['--allow-net'], Options);
    Positionals.Free;
    Expect<Boolean>(Opt.Unscoped).ToBe(True);
    Expect<Integer>(Opt.Scopes.Count).ToBe(0);
  finally
    Opt.Free;
  end;
  Opt := TScopeListOption.Create('allow-net', 'Allow net', '<host>');
  try
    SetLength(Options, 1);
    Options[0] := Opt;
    Positionals := ParseArguments(['--allow-net=a.test,b.test'], Options);
    Positionals.Free;
    Expect<Boolean>(Opt.Unscoped).ToBe(False);
    Expect<string>(Opt.Scopes.CommaText).ToBe('a.test,b.test');
    Expect<string>(Opt.FormatForHelp).ToBe('--allow-net[=<host>,...]');
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestScopeListAccumulates;
var
  Opt: TScopeListOption;
  Options: TOptionArray;
  Positionals: TStringList;
begin
  Opt := TScopeListOption.Create('allow-read', 'Allow read', '<path>');
  try
    SetLength(Options, 1);
    Options[0] := Opt;
    Positionals := ParseArguments(['--allow-read=a', '--allow-read',
      '--allow-read=b,c'], Options);
    Positionals.Free;
    Expect<Boolean>(Opt.Unscoped).ToBe(True);
    Expect<string>(Opt.Scopes.CommaText).ToBe('a,b,c');
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestScopeListDoesNotConsumeNextArgument;
var
  Opt: TScopeListOption;
  Options: TOptionArray;
  Positionals: TStringList;
begin
  Opt := TScopeListOption.Create('allow-read', 'Allow read', '<path>');
  try
    SetLength(Options, 1);
    Options[0] := Opt;
    Positionals := ParseArguments(['--allow-read', 'foo.js'], Options);
    try
      Expect<Boolean>(Opt.Unscoped).ToBe(True);
      Expect<Integer>(Positionals.Count).ToBe(1);
      Expect<string>(Positionals[0]).ToBe('foo.js');
    finally
      Positionals.Free;
    end;
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestScopeListEmptyListRaises;
var
  Opt: TScopeListOption;
  Options: TOptionArray;
  IsUsageError: Boolean;
begin
  Opt := TScopeListOption.Create('allow-net', 'Allow net', '<host>', '',
    False, 'allow every public host');
  try
    SetLength(Options, 1);
    Options[0] := Opt;
    Expect<string>(ParseMessage(['--allow-net='], Options, IsUsageError))
      .ToBe('--allow-net= has an empty scope list; omit "=" to allow every ' +
        'public host');
    Expect<Boolean>(IsUsageError).ToBe(False);
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestScopeListEmptyItemRaises;
var
  Opt: TScopeListOption;
  Options: TOptionArray;
  IsUsageError: Boolean;
begin
  Opt := TScopeListOption.Create('allow-read', 'Allow read', '<path>');
  try
    SetLength(Options, 1);
    Options[0] := Opt;
    Expect<string>(ParseMessage(['--allow-read=a,,b'], Options,
      IsUsageError)).ToBe('Empty scope in --allow-read=a,,b');
    Expect<string>(ParseMessage(['--allow-read=a,'], Options,
      IsUsageError)).ToBe('Empty scope in --allow-read=a,');
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestScopeListRequiredScope;
var
  Opt: TScopeListOption;
  Options: TOptionArray;
  IsUsageError: Boolean;
begin
  Opt := TScopeListOption.Create('allow-import', 'Allow import', '<source>',
    '', True, '', 'node_modules[=<dir>] or a provider such as github');
  try
    SetLength(Options, 1);
    Options[0] := Opt;
    Expect<string>(ParseMessage(['--allow-import'], Options, IsUsageError))
      .ToBe('--allow-import needs a scope: node_modules[=<dir>] or a ' +
        'provider such as github');
    Expect<string>(ParseMessage(['--allow-import='], Options, IsUsageError))
      .ToBe('--allow-import needs a scope: node_modules[=<dir>] or a ' +
        'provider such as github');
    Expect<string>(ParseMessage(['--allow-import=node_modules'], Options,
      IsUsageError)).ToBe('');
    Expect<string>(Opt.FormatForHelp).ToBe('--allow-import=<source>,...');
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestRemovedOptionRaises;
var
  Removed: TRemovedOption;
  Options: TOptionArray;
  IsUsageError: Boolean;
begin
  Removed := TRemovedOption.Create('stack-size', 'stack-size',
    'use --max-stack instead', 'use "max-stack" instead');
  try
    SetLength(Options, 1);
    Options[0] := Removed;
    Expect<string>(ParseMessage(['--stack-size=100'], Options, IsUsageError))
      .ToBe('--stack-size was removed in GocciaScript 0.14.0; use ' +
        '--max-stack instead');
    Expect<Boolean>(IsUsageError).ToBe(True);
    Expect<string>(ParseMessage(['--stack-size'], Options, IsUsageError))
      .ToBe('--stack-size was removed in GocciaScript 0.14.0; use ' +
        '--max-stack instead');
    Expect<Boolean>(Removed.Hidden).ToBe(True);
    Expect<Boolean>(Removed.ConsumesSeparateValue).ToBe(False);
  finally
    Removed.Free;
  end;
end;

procedure TCLIOptionsTests.TestHiddenOptionOmittedFromHelp;
var
  Visible, Hidden: TFlagOption;
  Options: TOptionArray;
  HelpText: string;
begin
  Visible := TFlagOption.Create('visible-flag', 'Shown');
  Hidden := TFlagOption.Create('hidden-flag', 'Not shown');
  try
    Hidden.Hidden := True;
    SetLength(Options, 2);
    Options[0] := Visible;
    Options[1] := Hidden;
    HelpText := GenerateHelpText('Test', '[options]', Options);
    Expect<Boolean>(Pos('--visible-flag', HelpText) > 0).ToBe(True);
    Expect<Boolean>(Pos('--hidden-flag', HelpText) > 0).ToBe(False);
  finally
    Visible.Free;
    Hidden.Free;
  end;
end;

procedure TCLIOptionsTests.TestUnitOptionsParse;
var
  Bytes: TByteSizeOption;
  Duration: TDurationOption;
  Count: TCountOption;
  Options: TOptionArray;
  Positionals: TStringList;
  IsUsageError: Boolean;
begin
  Bytes := TByteSizeOption.Create('max-memory', 'Memory');
  Duration := TDurationOption.Create('timeout', 'Timeout');
  Count := TCountOption.Create('max-stack', 'Stack');
  try
    SetLength(Options, 3);
    Options[0] := Bytes;
    Options[1] := Duration;
    Options[2] := Count;
    Positionals := ParseArguments(['--max-memory=64MiB', '--timeout', '5s',
      '--max-stack=2900'], Options);
    Positionals.Free;
    Expect<Int64>(Bytes.Value).ToBe(64 * 1024 * 1024);
    Expect<Integer>(Duration.Milliseconds(0)).ToBe(5000);
    Expect<Int64>(Count.Value).ToBe(2900);
    Expect<string>(ParseMessage(['--max-stack=-1'], Options, IsUsageError))
      .ToBe('Invalid value for --max-stack: -1 (use a non-negative whole ' +
        'number)');
    Expect<string>(Bytes.FormatForHelp).ToBe('--max-memory=<bytes>');
    Expect<string>(Duration.FormatForHelp).ToBe('--timeout=<duration>');
  finally
    Bytes.Free;
    Duration.Free;
    Count.Free;
  end;
end;

{ TFlagOption tests }

procedure TCLIOptionsTests.TestFlagOptionNotPresentInitially;
var
  Opt: TFlagOption;
begin
  Opt := TFlagOption.Create('verbose', 'Enable verbose output');
  try
    Expect<Boolean>(Opt.Present).ToBe(False);
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestFlagOptionApply;
var
  Opt: TFlagOption;
begin
  Opt := TFlagOption.Create('verbose', 'Enable verbose output');
  try
    Opt.Apply('');
    Expect<Boolean>(Opt.Present).ToBe(True);
  finally
    Opt.Free;
  end;
end;

{ TStringOption tests }

procedure TCLIOptionsTests.TestStringOptionApply;
var
  Opt: TStringOption;
begin
  Opt := TStringOption.Create('output', 'Output path');
  try
    Opt.Apply('hello');
    Expect<Boolean>(Opt.Present).ToBe(True);
    Expect<string>(Opt.Value).ToBe('hello');
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestStringOptionValueOrWhenPresent;
var
  Opt: TStringOption;
begin
  Opt := TStringOption.Create('output', 'Output path');
  try
    Opt.Apply('hello');
    Expect<string>(Opt.ValueOr('fallback')).ToBe('hello');
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestStringOptionValueOrWhenAbsent;
var
  Opt: TStringOption;
begin
  Opt := TStringOption.Create('output', 'Output path');
  try
    Expect<string>(Opt.ValueOr('fallback')).ToBe('fallback');
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestStringOptionMarkPresentClearsValue;
var
  Opt: TStringOption;
begin
  Opt := TStringOption.Create('output', 'Output path');
  try
    Opt.Apply('configured.map');
    Opt.MarkPresent;
    Expect<Boolean>(Opt.Present).ToBe(True);
    Expect<string>(Opt.Value).ToBe('');
    Expect<string>(Opt.ValueOr('fallback')).ToBe('');
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestOptionalStringOptionParsing;

  procedure ExpectSourceMapParse(const AExpectedValue: string;
    const AArgs: array of string);
  var
    Opt: TOptionalStringOption;
    Options: TOptionArray;
    Positionals: TStringList;
  begin
    Opt := TOptionalStringOption.Create('source-map', 'Write source map');
    try
      SetLength(Options, 1);
      Options[0] := Opt;
      Positionals := ParseArguments(AArgs, Options);
      try
        Expect<Boolean>(Opt.Present).ToBe(True);
        Expect<string>(Opt.Value).ToBe(AExpectedValue);
        Expect<Integer>(Positionals.Count).ToBe(1);
        Expect<string>(Positionals[0]).ToBe('positional');
      finally
        Positionals.Free;
      end;
    finally
      Opt.Free;
    end;
  end;

begin
  ExpectSourceMapParse('', ['--source-map', 'positional']);
  ExpectSourceMapParse('out.map', ['--source-map=out.map', 'positional']);
end;

procedure TCLIOptionsTests.TestRequiredValueOptionAcceptsDashPrefixedValues;
var
  IntegerOpt: TIntegerOption;
  StringOpt: TStringOption;
  Options: TOptionArray;
  Positionals: TStringList;
begin
  IntegerOpt := TIntegerOption.Create('timeout', 'Timeout in ms');
  try
    SetLength(Options, 1);
    Options[0] := IntegerOpt;
    Positionals := ParseArguments(['--timeout', '-1'], Options);
    try
      Expect<Integer>(IntegerOpt.Value).ToBe(-1);
      Expect<Integer>(Positionals.Count).ToBe(0);
    finally
      Positionals.Free;
    end;
  finally
    IntegerOpt.Free;
  end;

  StringOpt := TStringOption.Create('output', 'Output path');
  try
    SetLength(Options, 1);
    Options[0] := StringOpt;
    Positionals := ParseArguments(['--output', '-tmp'], Options);
    try
      Expect<string>(StringOpt.Value).ToBe('-tmp');
      Expect<Integer>(Positionals.Count).ToBe(0);
    finally
      Positionals.Free;
    end;
  finally
    StringOpt.Free;
  end;
end;

{ TIntegerOption tests }

procedure TCLIOptionsTests.TestIntegerOptionApply;
var
  Opt: TIntegerOption;
begin
  Opt := TIntegerOption.Create('timeout', 'Timeout in ms');
  try
    Opt.Apply('42');
    Expect<Boolean>(Opt.Present).ToBe(True);
    Expect<Integer>(Opt.Value).ToBe(42);
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestIntegerOptionValueOrWhenPresent;
var
  Opt: TIntegerOption;
begin
  Opt := TIntegerOption.Create('timeout', 'Timeout in ms');
  try
    Opt.Apply('42');
    Expect<Integer>(Opt.ValueOr(100)).ToBe(42);
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestIntegerOptionValueOrWhenAbsent;
var
  Opt: TIntegerOption;
begin
  Opt := TIntegerOption.Create('timeout', 'Timeout in ms');
  try
    Expect<Integer>(Opt.ValueOr(100)).ToBe(100);
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestIntegerOptionInvalidValue;
var
  Opt: TIntegerOption;
  Raised: Boolean;
begin
  Opt := TIntegerOption.Create('timeout', 'Timeout in ms');
  try
    Raised := False;
    try
      Opt.Apply('notanumber');
    except
      on E: TParseError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    Opt.Free;
  end;
end;

{ TRepeatableOption tests }

procedure TCLIOptionsTests.TestRepeatableOptionMultipleValues;
var
  Opt: TRepeatableOption;
begin
  Opt := TRepeatableOption.Create('alias', 'Import alias');
  try
    Opt.Apply('@/=./src/');
    Opt.Apply('config=./config/default.js');
    Opt.Apply('utils=./lib/utils.js');
    Expect<Boolean>(Opt.Present).ToBe(True);
    Expect<Integer>(Opt.Values.Count).ToBe(3);
    Expect<string>(Opt.Values[0]).ToBe('@/=./src/');
    Expect<string>(Opt.Values[1]).ToBe('config=./config/default.js');
    Expect<string>(Opt.Values[2]).ToBe('utils=./lib/utils.js');
  finally
    Opt.Free;
  end;
end;

{ TEnumOption tests — uses local TTestColor enum }

procedure TCLIOptionsTests.TestEnumOptionFirstValue;
var
  Opt: TEnumOption<TTestColor>;
begin
  Opt := TEnumOption<TTestColor>.Create('color', 'Pick a color');
  try
    Opt.Apply('red');
    Expect<Boolean>(Opt.Present).ToBe(True);
    Expect<Boolean>(Opt.Matches(tcRed)).ToBe(True);
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestEnumOptionSecondValue;
var
  Opt: TEnumOption<TTestColor>;
begin
  Opt := TEnumOption<TTestColor>.Create('color', 'Pick a color');
  try
    Opt.Apply('blue');
    Expect<Boolean>(Opt.Present).ToBe(True);
    Expect<Boolean>(Opt.Matches(tcBlue)).ToBe(True);
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestEnumOptionInvalidValue;
var
  Opt: TEnumOption<TTestColor>;
  Raised: Boolean;
begin
  Opt := TEnumOption<TTestColor>.Create('color', 'Pick a color');
  try
    Raised := False;
    try
      Opt.Apply('invalid');
    except
      on E: TParseError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestEnumOptionValidValues;
var
  Opt: TEnumOption<TTestColor>;
begin
  Opt := TEnumOption<TTestColor>.Create('color', 'Pick a color');
  try
    Expect<string>(Opt.ValidValues).ToBe('red, blue');
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestEnumOptionFormatForHelp;
var
  Opt: TEnumOption<TTestColor>;
begin
  Opt := TEnumOption<TTestColor>.Create('color', 'Pick a color');
  try
    Expect<string>(Opt.FormatForHelp).ToBe('--color=red|blue');
  finally
    Opt.Free;
  end;
end;

{ TOptionList tests }

procedure TCLIOptionsTests.TestOptionListAddFlagAndString;
var
  List: TOptionList;
  Arr: TOptionArray;
begin
  List := TOptionList.Create;
  try
    List.AddFlag('verbose', 'Enable verbose output');
    List.AddString('output', 'Output file path');
    Arr := List.Options;
    Expect<Integer>(Length(Arr)).ToBe(2);
    Expect<string>(Arr[0].LongName).ToBe('verbose');
    Expect<string>(Arr[1].LongName).ToBe('output');
  finally
    List.Free;
  end;
end;

{ ConcatOptions tests — uses OptionList, no engine/coverage dependency }

procedure TCLIOptionsTests.TestConcatOptionsMergesTwoArrays;
var
  ListA, ListB: TOptionList;
  Combined: TOptionArray;
begin
  ListA := TOptionList.Create;
  ListB := TOptionList.Create;
  try
    ListA.AddFlag('alpha', 'First flag');
    ListA.AddString('beta', 'First string');
    ListB.AddFlag('gamma', 'Second flag');
    Combined := ConcatOptions([ListA.Options, ListB.Options]);
    Expect<Integer>(Length(Combined)).ToBe(3);
    Expect<string>(Combined[0].LongName).ToBe('alpha');
    Expect<string>(Combined[1].LongName).ToBe('beta');
    Expect<string>(Combined[2].LongName).ToBe('gamma');
  finally
    ListB.Free;
    ListA.Free;
  end;
end;

{ GenerateHelpText tests }

procedure TCLIOptionsTests.TestGenerateHelpText;
var
  List: TOptionList;
  HelpText: string;
begin
  List := TOptionList.Create;
  try
    List.AddFlag('verbose', 'Enable verbose output');
    List.AddString('output', 'Output file path');
    HelpText := GenerateHelpText('myapp', '[options] <file>', List.Options);
    Expect<Boolean>(Pos('myapp', HelpText) > 0).ToBe(True);
    Expect<Boolean>(Pos('--verbose', HelpText) > 0).ToBe(True);
    Expect<Boolean>(Pos('--output', HelpText) > 0).ToBe(True);
    Expect<Boolean>(Pos('Enable verbose output', HelpText) > 0).ToBe(True);
    Expect<Boolean>(Pos('Output file path', HelpText) > 0).ToBe(True);
  finally
    List.Free;
  end;
end;

{ FromCommandLine tests }

procedure TCLIOptionsTests.TestFromCommandLineIsFalseInitially;
var
  Opt: TFlagOption;
begin
  Opt := TFlagOption.Create('feature', 'Enable feature');
  try
    Expect<Boolean>(Opt.FromCommandLine).ToBe(False);
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestMarkFromCommandLineSetsTrue;
var
  Opt: TFlagOption;
begin
  Opt := TFlagOption.Create('feature', 'Enable feature');
  try
    Opt.Apply('');
    Opt.MarkFromCommandLine;
    Expect<Boolean>(Opt.Present).ToBe(True);
    Expect<Boolean>(Opt.FromCommandLine).ToBe(True);
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestConfigAppliedOptionNotFromCommandLine;
var
  Opt: TFlagOption;
  Options: TOptionArray;
  Entries: TConfigEntryArray;
begin
  Opt := TFlagOption.Create('feature', 'Enable feature');
  try
    SetLength(Options, 1);
    Options[0] := Opt;
    SetLength(Entries, 1);
    Entries[0].Key := 'feature';
    Entries[0].Value := 'true';

    ApplyConfigEntries(Entries, Options);

    Expect<Boolean>(Opt.Present).ToBe(True);
    Expect<Boolean>(Opt.FromCommandLine).ToBe(False);
  finally
    Opt.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TCLIOptionsTests.Create('CLI Options'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
