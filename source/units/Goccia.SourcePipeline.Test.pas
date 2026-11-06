program Goccia.SourcePipeline.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Error,
  Goccia.GarbageCollector,
  Goccia.SourcePipeline,
  Goccia.TestSetup;

type
  TSourcePipelineTests = class(TTestSuite)
  private
    function ParseNanoseconds(const ASource: TStringList;
      const ASourceType: TGocciaSourceType): Int64;
    procedure ExpectModuleParseLinear(const AModuleLine, AScriptLine: string);
    procedure TestModuleConstDeclarationsParseInLinearTime;
    procedure TestModuleLetDeclarationsParseInLinearTime;
    procedure TestModuleClassDeclarationsParseInLinearTime;
    procedure TestModuleExportDeclarationsParseInLinearTime;
    procedure TestModuleImportDeclarationsParseInLinearTime;
    procedure TestModuleEarlyErrorNamesTheFirstImportInSourceOrder;
    procedure TestModuleEarlyErrorNamesTheFirstVarInSourceOrder;
    procedure TestAwaitSourceGoal;
    procedure TestAwaitModuleError;
    procedure ExpectModuleErrorPreserved(const ASource: string;
      const ACompatibility: TGocciaCompatibilityFlags);
    procedure TestSloppyAwaitFallback;
    procedure ExpectParsePasses(const ASource: string;
      const ASourceType, AExpectedGoal: TGocciaSourceType;
      const ACompatibility: TGocciaCompatibilityFlags;
      const AExpectedPasses: Integer);
    procedure TestExplicitGoalsParseOnce;
    procedure TestDetectionParsePassBounds;
  public
    procedure SetupTests; override;
  end;

const
  { Enough declarations that a check costing time quadratic in them (as the
    module early errors once did, about 3.5 s here against 0.05 s for the
    script parse) is far past the bound, and few enough that the linear
    parse takes a fraction of a second. }
  DECLARATION_COUNT = 10000;
  TRIALS = 3;
  { The module parse does the script parse's work plus one pass over the
    declarations, so it stays close to the script parse; the slack absorbs
    a busy machine without letting a quadratic pass through. }
  MODULE_TO_SCRIPT_RATIO = 3;
  SLACK_NANOSECONDS = 30 * 1000 * 1000;

function DeclarationLines(const ALinePattern: string;
  const ACount: Integer): TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := 0 to ACount - 1 do
    Result.Add(Format(ALinePattern, [I]));
end;

procedure TSourcePipelineTests.SetupTests;
begin
  Test('Explicit source goals parse once', TestExplicitGoalsParseOnce);
  Test('Detection respects parse pass bounds', TestDetectionParsePassBounds);
  Test('Sloppy ambiguous await falls back after strict Module failure', TestSloppyAwaitFallback);
  Test('Ambiguous bare await selects Module', TestAwaitSourceGoal);
  Test('Failed await fallback preserves Module error', TestAwaitModuleError);
  Test('Module const declarations parse in linear time',
    TestModuleConstDeclarationsParseInLinearTime);
  Test('Module let declarations parse in linear time',
    TestModuleLetDeclarationsParseInLinearTime);
  Test('Module class declarations parse in linear time',
    TestModuleClassDeclarationsParseInLinearTime);
  Test('Module export declarations parse in linear time',
    TestModuleExportDeclarationsParseInLinearTime);
  Test('Module import declarations parse in linear time',
    TestModuleImportDeclarationsParseInLinearTime);
  Test('A module early error names the first conflicting import',
    TestModuleEarlyErrorNamesTheFirstImportInSourceOrder);
  Test('A module early error names the first conflicting var',
    TestModuleEarlyErrorNamesTheFirstVarInSourceOrder);
end;

{ The fastest of TRIALS parses, so a pause on a shared machine inflates
  neither side of a comparison. }
function TSourcePipelineTests.ParseNanoseconds(const ASource: TStringList;
  const ASourceType: TGocciaSourceType): Int64;
var
  Options: TGocciaSourcePipelineOptions;
  ParseResult: TGocciaSourcePipelineResult;
  Elapsed: Int64;
  I: Integer;
begin
  Options := TGocciaSourcePipeline.DefaultOptions;
  Options.SourceType := ASourceType;
  Result := High(Int64);
  for I := 1 to TRIALS do
  begin
    ParseResult := TGocciaSourcePipeline.Parse(ASource, '<test>', Options);
    try
      Elapsed := ParseResult.ParseTimeNanoseconds;
    finally
      ParseResult.Free;
    end;
    if Elapsed < Result then
      Result := Elapsed;
  end;
end;

{ The module's parse, early errors included (ES2026 §16.2.1.1), against the
  script parse of the same number of declarations on the same machine: a
  wall-clock bound would depend on the machine, a ratio does not. }
procedure TSourcePipelineTests.ExpectModuleParseLinear(const AModuleLine,
  AScriptLine: string);
var
  ModuleSource, ScriptSource: TStringList;
  ModuleTime, ScriptTime: Int64;
begin
  ModuleSource := DeclarationLines(AModuleLine, DECLARATION_COUNT);
  ScriptSource := DeclarationLines(AScriptLine, DECLARATION_COUNT);
  try
    ScriptTime := ParseNanoseconds(ScriptSource, stScript);
    ModuleTime := ParseNanoseconds(ModuleSource, stModule);
    if ModuleTime > MODULE_TO_SCRIPT_RATIO * ScriptTime + SLACK_NANOSECONDS then
      Fail(Format('%d declarations parsed in %d ms as a module against ' +
        '%d ms as a script', [DECLARATION_COUNT, ModuleTime div 1000000,
        ScriptTime div 1000000]));
    Expect<Boolean>(ModuleTime <=
      MODULE_TO_SCRIPT_RATIO * ScriptTime + SLACK_NANOSECONDS).ToBe(True);
  finally
    ScriptSource.Free;
    ModuleSource.Free;
  end;
end;

procedure TSourcePipelineTests.TestModuleConstDeclarationsParseInLinearTime;
begin
  ExpectModuleParseLinear('const a%0:d = %0:d;', 'const a%0:d = %0:d;');
end;

procedure TSourcePipelineTests.TestModuleLetDeclarationsParseInLinearTime;
begin
  ExpectModuleParseLinear('let a%0:d = %0:d;', 'let a%0:d = %0:d;');
end;

procedure TSourcePipelineTests.TestModuleClassDeclarationsParseInLinearTime;
begin
  ExpectModuleParseLinear('class A%0:d {}', 'class A%0:d {}');
end;

procedure TSourcePipelineTests.TestModuleExportDeclarationsParseInLinearTime;
begin
  ExpectModuleParseLinear('export const a%0:d = %0:d;',
    'const a%0:d = %0:d;');
end;

procedure TSourcePipelineTests.TestModuleImportDeclarationsParseInLinearTime;
begin
  ExpectModuleParseLinear('import { x as a%0:d } from "./m.js";',
    'const a%0:d = x;');
end;

procedure TSourcePipelineTests.ExpectParsePasses(const ASource: string;
  const ASourceType, AExpectedGoal: TGocciaSourceType;
  const ACompatibility: TGocciaCompatibilityFlags;
  const AExpectedPasses: Integer);
var
  Lines: TStringList;
  Options: TGocciaSourcePipelineOptions;
  Parsed: TGocciaSourcePipelineResult;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := ASource;
    Options := TGocciaSourcePipeline.DefaultOptions;
    Options.SourceType := ASourceType;
    Options.Compatibility := ACompatibility;
    Parsed := TGocciaSourcePipeline.Parse(Lines, '<test>', Options);
    try
      Expect<Boolean>(Parsed.SourceType = AExpectedGoal).ToBe(True);
      Expect<Integer>(Parsed.ParsePassCount).ToBe(AExpectedPasses);
      Expect<Boolean>(Parsed.LexTimeNanoseconds >= 0).ToBe(True);
      Expect<Boolean>(Parsed.ParseTimeNanoseconds >= 0).ToBe(True);
    finally
      Parsed.Free;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TSourcePipelineTests.TestExplicitGoalsParseOnce;
begin
  ExpectParsePasses('const await = {0: 1}; await[0];',
    stScript, stScript, [], 1);
  ExpectParsePasses('await[0]; export {};', stModule, stModule, [], 1);
  ExpectParsePasses('globalThis.await = {0: 1}; await[0]; delete missing;',
    stScript, stScript, [cfNonStrictMode], 1);
  ExpectParsePasses('await[0]; export {};',
    stModule, stModule, [cfNonStrictMode], 1);
end;

procedure TSourcePipelineTests.TestDetectionParsePassBounds;
begin
  ExpectParsePasses('1;', stDetect, stScript, [], 1);
  ExpectParsePasses('export {};', stDetect, stModule, [], 1);
  ExpectParsePasses('await[0];', stDetect, stModule, [], 1);
  ExpectParsePasses('await[0]; export {};', stDetect, stModule, [], 1);
  // Failure of the Module reading must return the second parse, never a third.
  ExpectParsePasses('const await = {0: 1}; await[0];',
    stDetect, stScript, [], 2);
  ExpectParsePasses('const await = () => 1; await();',
    stDetect, stScript, [], 2);
  ExpectParsePasses('1;', stDetect, stScript, [cfNonStrictMode], 1);
  ExpectParsePasses('export {};', stDetect, stModule, [cfNonStrictMode], 2);
  ExpectParsePasses('await[0];', stDetect, stModule, [cfNonStrictMode], 2);
  ExpectParsePasses('const await = {0: 1}; await[0];',
    stDetect, stScript, [cfNonStrictMode], 2);
  ExpectParsePasses('globalThis.await = {0: 1}; await[0]; delete missing;',
    stDetect, stScript, [cfNonStrictMode], 3);
end;

procedure TSourcePipelineTests.TestAwaitSourceGoal;
var
  Lines: TStringList;
  Options: TGocciaSourcePipelineOptions;
  Parsed: TGocciaSourcePipelineResult;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := 'await[0];';
    Options := TGocciaSourcePipeline.DefaultOptions;
    Options.SourceType := stDetect;
    Parsed := TGocciaSourcePipeline.Parse(Lines, '<test>', Options);
    try
      Expect<Boolean>(Parsed.SourceType = stModule).ToBe(True);
      Expect<Integer>(Parsed.ParsePassCount).ToBe(1);
    finally
      Parsed.Free;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TSourcePipelineTests.TestSloppyAwaitFallback;
var
  Lines: TStringList;
  Options: TGocciaSourcePipelineOptions;
  Parsed: TGocciaSourcePipelineResult;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := 'globalThis.await = {0: 1}; await[0]; delete missing;';
    Options := TGocciaSourcePipeline.DefaultOptions;
    Options.SourceType := stDetect;
    Options.Compatibility := [cfNonStrictMode];
    Parsed := TGocciaSourcePipeline.Parse(Lines, '<test>', Options);
    try
      Expect<Boolean>(Parsed.SourceType = stScript).ToBe(True);
      Expect<Integer>(Parsed.ParsePassCount).ToBe(3);
    finally
      Parsed.Free;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TSourcePipelineTests.ExpectModuleErrorPreserved(const ASource: string;
  const ACompatibility: TGocciaCompatibilityFlags);
var
  Lines: TStringList;
  Options: TGocciaSourcePipelineOptions;
  Goal: TGocciaSourceType;
  Message, ModuleMessage: string;
  Line, Column, ModuleLine, ModuleColumn: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := ASource;
    Options := TGocciaSourcePipeline.DefaultOptions;
    Options.Compatibility := ACompatibility;
    ModuleMessage := '';
    ModuleLine := -1;
    ModuleColumn := -1;
    // Capture the explicit Module diagnostic before trying detection.
    for Goal in [stModule, stDetect] do
    begin
      Options.SourceType := Goal;
      Message := '';
      Line := -1;
      Column := -1;
      try
        TGocciaSourcePipeline.Parse(Lines, '<test>', Options).Free;
      except
        on E: TGocciaSyntaxError do
        begin
          Message := E.Message;
          Line := E.Line;
          Column := E.Column;
        end;
      end;
      Expect<Boolean>(Message <> '').ToBe(True);
      if Goal = stModule then
      begin
        ModuleMessage := Message;
        ModuleLine := Line;
        ModuleColumn := Column;
      end
      else
      begin
        Expect<string>(Message).ToBe(ModuleMessage);
        Expect<Integer>(Line).ToBe(ModuleLine);
        Expect<Integer>(Column).ToBe(ModuleColumn);
      end;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TSourcePipelineTests.TestAwaitModuleError;
begin
  ExpectModuleErrorPreserved(
    'const await = () => 1;' + LineEnding + 'await(); export {};', []);
  ExpectModuleErrorPreserved(
    'globalThis.await = {0: 1};' + LineEnding +
    'await[0]; delete missing; export {};', [cfNonStrictMode]);
end;

function EarlyErrorOf(const ASource: string;
  const ACompatibility: TGocciaCompatibilityFlags;
  out ALine, AColumn: Integer): string;
var
  Lines: TStringList;
  Options: TGocciaSourcePipelineOptions;
begin
  Result := '';
  ALine := -1;
  AColumn := -1;
  Lines := TStringList.Create;
  try
    Lines.Text := ASource;
    Options := TGocciaSourcePipeline.DefaultOptions;
    Options.SourceType := stModule;
    Options.Compatibility := ACompatibility;
    try
      TGocciaSourcePipeline.Parse(Lines, '<test>', Options).Free;
    except
      on E: TGocciaSyntaxError do
      begin
        Result := E.Message;
        ALine := E.Line;
        AColumn := E.Column;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

{ Two imports conflict with later declarations: the parser checks the
  module's declarations in source order, so the error names the first
  declaration that repeats a name and points at it. The order does not
  depend on how the names are stored. }
procedure TSourcePipelineTests.
  TestModuleEarlyErrorNamesTheFirstImportInSourceOrder;
var
  Line, Column: Integer;
begin
  Expect<string>(EarlyErrorOf(
    'const c = 0;' + LineEnding +
    'import { b } from "./m.js";' + LineEnding +
    'import { a } from "./m.js";' + LineEnding +
    'const a = 1;' + LineEnding +
    'const b = 2;', [], Line, Column))
    .ToBe('Identifier ''a'' has already been declared');
  // At the declaration that repeats the name.
  Expect<Integer>(Line).ToBe(4);
  Expect<Integer>(Column).ToBe(1);
end;

{ Two vars conflict with lexical declarations: the error names the var
  declared first, at that var. }
procedure TSourcePipelineTests.TestModuleEarlyErrorNamesTheFirstVarInSourceOrder;
var
  Line, Column: Integer;
begin
  Expect<string>(EarlyErrorOf(
    'const a = 1;' + LineEnding +
    'const b = 2;' + LineEnding +
    'var b = 3;' + LineEnding +
    'var a = 4;', [cfVar], Line, Column))
    .ToBe('Identifier ''b'' has already been declared');
  Expect<Integer>(Line).ToBe(3);
  Expect<Integer>(Column).ToBe(1);
end;

begin
  TGarbageCollector.Initialize;
  try
    TestRunnerProgram.AddSuite(
      TSourcePipelineTests.Create('SourcePipeline'));
    RunGocciaTests;
    ExitCode := TestResultToExitCode;
  finally
    TGarbageCollector.Shutdown;
  end;
end.
