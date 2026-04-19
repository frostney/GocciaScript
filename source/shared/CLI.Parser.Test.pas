program CLI.Parser.Test;

{$I Shared.inc}

uses
  SysUtils,
  TypInfo,

  CLI.Options,
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
  Test('IntegerOption.Apply parses integer value', TestIntegerOptionApply);
  Test('IntegerOption.ValueOr returns value when present', TestIntegerOptionValueOrWhenPresent);
  Test('IntegerOption.ValueOr returns default when absent', TestIntegerOptionValueOrWhenAbsent);
  Test('IntegerOption.Apply raises TGocciaParseError for non-integer', TestIntegerOptionInvalidValue);
  Test('RepeatableOption accumulates multiple values', TestRepeatableOptionMultipleValues);
  Test('EnumOption applies first value', TestEnumOptionFirstValue);
  Test('EnumOption applies second value', TestEnumOptionSecondValue);
  Test('EnumOption raises TGocciaParseError for invalid value', TestEnumOptionInvalidValue);
  Test('EnumOption.ValidValues returns comma-separated list', TestEnumOptionValidValues);
  Test('EnumOption.FormatForHelp returns pipe-separated values', TestEnumOptionFormatForHelp);
  Test('OptionList tracks added options', TestOptionListAddFlagAndString);
  Test('ConcatOptions merges two arrays', TestConcatOptionsMergesTwoArrays);
  Test('GenerateHelpText includes program name and option names', TestGenerateHelpText);
end;

{ TGocciaFlagOption tests }

procedure TCLIOptionsTests.TestFlagOptionNotPresentInitially;
var
  Opt: TGocciaFlagOption;
begin
  Opt := TGocciaFlagOption.Create('verbose', 'Enable verbose output');
  try
    Expect<Boolean>(Opt.Present).ToBe(False);
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestFlagOptionApply;
var
  Opt: TGocciaFlagOption;
begin
  Opt := TGocciaFlagOption.Create('verbose', 'Enable verbose output');
  try
    Opt.Apply('');
    Expect<Boolean>(Opt.Present).ToBe(True);
  finally
    Opt.Free;
  end;
end;

{ TGocciaStringOption tests }

procedure TCLIOptionsTests.TestStringOptionApply;
var
  Opt: TGocciaStringOption;
begin
  Opt := TGocciaStringOption.Create('output', 'Output path');
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
  Opt: TGocciaStringOption;
begin
  Opt := TGocciaStringOption.Create('output', 'Output path');
  try
    Opt.Apply('hello');
    Expect<string>(Opt.ValueOr('fallback')).ToBe('hello');
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestStringOptionValueOrWhenAbsent;
var
  Opt: TGocciaStringOption;
begin
  Opt := TGocciaStringOption.Create('output', 'Output path');
  try
    Expect<string>(Opt.ValueOr('fallback')).ToBe('fallback');
  finally
    Opt.Free;
  end;
end;

{ TGocciaIntegerOption tests }

procedure TCLIOptionsTests.TestIntegerOptionApply;
var
  Opt: TGocciaIntegerOption;
begin
  Opt := TGocciaIntegerOption.Create('timeout', 'Timeout in ms');
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
  Opt: TGocciaIntegerOption;
begin
  Opt := TGocciaIntegerOption.Create('timeout', 'Timeout in ms');
  try
    Opt.Apply('42');
    Expect<Integer>(Opt.ValueOr(100)).ToBe(42);
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestIntegerOptionValueOrWhenAbsent;
var
  Opt: TGocciaIntegerOption;
begin
  Opt := TGocciaIntegerOption.Create('timeout', 'Timeout in ms');
  try
    Expect<Integer>(Opt.ValueOr(100)).ToBe(100);
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestIntegerOptionInvalidValue;
var
  Opt: TGocciaIntegerOption;
  Raised: Boolean;
begin
  Opt := TGocciaIntegerOption.Create('timeout', 'Timeout in ms');
  try
    Raised := False;
    try
      Opt.Apply('notanumber');
    except
      on E: TGocciaParseError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    Opt.Free;
  end;
end;

{ TGocciaRepeatableOption tests }

procedure TCLIOptionsTests.TestRepeatableOptionMultipleValues;
var
  Opt: TGocciaRepeatableOption;
begin
  Opt := TGocciaRepeatableOption.Create('alias', 'Import alias');
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

{ TGocciaEnumOption tests — uses local TTestColor enum }

procedure TCLIOptionsTests.TestEnumOptionFirstValue;
var
  Opt: TGocciaEnumOption<TTestColor>;
begin
  Opt := TGocciaEnumOption<TTestColor>.Create('color', 'Pick a color');
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
  Opt: TGocciaEnumOption<TTestColor>;
begin
  Opt := TGocciaEnumOption<TTestColor>.Create('color', 'Pick a color');
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
  Opt: TGocciaEnumOption<TTestColor>;
  Raised: Boolean;
begin
  Opt := TGocciaEnumOption<TTestColor>.Create('color', 'Pick a color');
  try
    Raised := False;
    try
      Opt.Apply('invalid');
    except
      on E: TGocciaParseError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestEnumOptionValidValues;
var
  Opt: TGocciaEnumOption<TTestColor>;
begin
  Opt := TGocciaEnumOption<TTestColor>.Create('color', 'Pick a color');
  try
    Expect<string>(Opt.ValidValues).ToBe('red, blue');
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestEnumOptionFormatForHelp;
var
  Opt: TGocciaEnumOption<TTestColor>;
begin
  Opt := TGocciaEnumOption<TTestColor>.Create('color', 'Pick a color');
  try
    Expect<string>(Opt.FormatForHelp).ToBe('--color=red|blue');
  finally
    Opt.Free;
  end;
end;

{ TGocciaOptionList tests }

procedure TCLIOptionsTests.TestOptionListAddFlagAndString;
var
  List: TGocciaOptionList;
  Arr: TGocciaOptionArray;
begin
  List := TGocciaOptionList.Create;
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
  ListA, ListB: TGocciaOptionList;
  Combined: TGocciaOptionArray;
begin
  ListA := TGocciaOptionList.Create;
  ListB := TGocciaOptionList.Create;
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
  List: TGocciaOptionList;
  HelpText: string;
begin
  List := TGocciaOptionList.Create;
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

begin
  TestRunnerProgram.AddSuite(TCLIOptionsTests.Create('CLI Options'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
