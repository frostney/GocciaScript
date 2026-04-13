program Goccia.CLI.Parser.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestRunner,

  Goccia.CLI.Help,
  Goccia.CLI.Options;

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
    procedure TestEnumOptionInterpreted;
    procedure TestEnumOptionBytecode;
    procedure TestEnumOptionInvalidValue;
    procedure TestEnumOptionValidValues;
    procedure TestEnumOptionFormatForHelp;
    procedure TestEngineOptionsSubOptions;
    procedure TestEngineOptionsCount;
    procedure TestCoverageOptionsSubOptions;
    procedure TestCoverageOptionsCount;
    procedure TestOptionListAddFlagAndString;
    procedure TestConcatOptions;
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
  Test('EnumOption applies interpreted', TestEnumOptionInterpreted);
  Test('EnumOption applies bytecode', TestEnumOptionBytecode);
  Test('EnumOption raises TGocciaParseError for invalid value', TestEnumOptionInvalidValue);
  Test('EnumOption.ValidValues returns comma-separated list', TestEnumOptionValidValues);
  Test('EnumOption.FormatForHelp returns pipe-separated values', TestEnumOptionFormatForHelp);
  Test('EngineOptions exposes all sub-options', TestEngineOptionsSubOptions);
  Test('EngineOptions.Options returns correct count', TestEngineOptionsCount);
  Test('CoverageOptions exposes all sub-options', TestCoverageOptionsSubOptions);
  Test('CoverageOptions.Options returns correct count', TestCoverageOptionsCount);
  Test('OptionList tracks added options', TestOptionListAddFlagAndString);
  Test('ConcatOptions merges two arrays', TestConcatOptions);
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

{ TGocciaEnumOption tests }

procedure TCLIOptionsTests.TestEnumOptionInterpreted;
var
  Opt: TGocciaEnumOption<TGocciaExecutionMode>;
begin
  Opt := TGocciaEnumOption<TGocciaExecutionMode>.Create('mode', 'Execution mode');
  try
    Opt.Apply('interpreted');
    Expect<Boolean>(Opt.Present).ToBe(True);
    Expect<Boolean>(Opt.Value = emInterpreted).ToBe(True);
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestEnumOptionBytecode;
var
  Opt: TGocciaEnumOption<TGocciaExecutionMode>;
begin
  Opt := TGocciaEnumOption<TGocciaExecutionMode>.Create('mode', 'Execution mode');
  try
    Opt.Apply('bytecode');
    Expect<Boolean>(Opt.Present).ToBe(True);
    Expect<Boolean>(Opt.Value = emBytecode).ToBe(True);
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestEnumOptionInvalidValue;
var
  Opt: TGocciaEnumOption<TGocciaExecutionMode>;
  Raised: Boolean;
begin
  Opt := TGocciaEnumOption<TGocciaExecutionMode>.Create('mode', 'Execution mode');
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
  Opt: TGocciaEnumOption<TGocciaExecutionMode>;
begin
  Opt := TGocciaEnumOption<TGocciaExecutionMode>.Create('mode', 'Execution mode');
  try
    Expect<string>(Opt.ValidValues).ToBe('interpreted, bytecode');
  finally
    Opt.Free;
  end;
end;

procedure TCLIOptionsTests.TestEnumOptionFormatForHelp;
var
  Opt: TGocciaEnumOption<TGocciaExecutionMode>;
begin
  Opt := TGocciaEnumOption<TGocciaExecutionMode>.Create('mode', 'Execution mode');
  try
    Expect<string>(Opt.FormatForHelp).ToBe('--mode=interpreted|bytecode');
  finally
    Opt.Free;
  end;
end;

{ TGocciaEngineOptions tests }

procedure TCLIOptionsTests.TestEngineOptionsSubOptions;
var
  Opts: TGocciaEngineOptions;
begin
  Opts := TGocciaEngineOptions.Create;
  try
    Expect<Boolean>(Opts.Mode <> nil).ToBe(True);
    Expect<Boolean>(Opts.ASI <> nil).ToBe(True);
    Expect<Boolean>(Opts.ImportMap <> nil).ToBe(True);
    Expect<Boolean>(Opts.Aliases <> nil).ToBe(True);
    Expect<Boolean>(Opts.Timeout <> nil).ToBe(True);
  finally
    Opts.Free;
  end;
end;

procedure TCLIOptionsTests.TestEngineOptionsCount;
var
  Opts: TGocciaEngineOptions;
begin
  Opts := TGocciaEngineOptions.Create;
  try
    Expect<Integer>(Length(Opts.Options)).ToBe(5);
  finally
    Opts.Free;
  end;
end;

{ TGocciaCoverageOptions tests }

procedure TCLIOptionsTests.TestCoverageOptionsSubOptions;
var
  Opts: TGocciaCoverageOptions;
begin
  Opts := TGocciaCoverageOptions.Create;
  try
    Expect<Boolean>(Opts.Enabled <> nil).ToBe(True);
    Expect<Boolean>(Opts.Format <> nil).ToBe(True);
    Expect<Boolean>(Opts.OutputPath <> nil).ToBe(True);
  finally
    Opts.Free;
  end;
end;

procedure TCLIOptionsTests.TestCoverageOptionsCount;
var
  Opts: TGocciaCoverageOptions;
begin
  Opts := TGocciaCoverageOptions.Create;
  try
    Expect<Integer>(Length(Opts.Options)).ToBe(3);
  finally
    Opts.Free;
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

{ ConcatOptions tests }

procedure TCLIOptionsTests.TestConcatOptions;
var
  Engine: TGocciaEngineOptions;
  Coverage: TGocciaCoverageOptions;
  Combined: TGocciaOptionArray;
begin
  Engine := TGocciaEngineOptions.Create;
  Coverage := TGocciaCoverageOptions.Create;
  try
    Combined := ConcatOptions([Engine.Options, Coverage.Options]);
    Expect<Integer>(Length(Combined)).ToBe(8);
    Expect<string>(Combined[0].LongName).ToBe('mode');
    Expect<string>(Combined[5].LongName).ToBe('coverage');
  finally
    Coverage.Free;
    Engine.Free;
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
