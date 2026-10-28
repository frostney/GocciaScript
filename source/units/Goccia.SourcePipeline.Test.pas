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
