program Goccia.JSX.Transformer.Test;

{$I Goccia.inc}

uses
  SysUtils,

  AllocationCounter,
  TestingPascalLibrary,

  Goccia.JSX.Transformer;

type
  TJSXTransformerTests = class(TTestSuite)
  private
    procedure TestSourceWithoutJSXIsUnchanged;
    procedure TestScanAllocatesPerTokenNotPerCharacter;
  public
    procedure SetupTests; override;
  end;

const
  REPEATED_ELEMENT = '1, x, "s", 2.5e3, ';
  REPEAT_COUNT = 2000;

function RepeatedSource: string;
var
  I: Integer;
begin
  Result := 'const x = 0;' + #10 + 'const a = [';
  for I := 1 to REPEAT_COUNT do
    Result := Result + REPEATED_ELEMENT;
  Result := Result + '1];' + #10;
end;

procedure TJSXTransformerTests.SetupTests;
begin
  Test('a source without JSX comes back unchanged and unmapped',
    TestSourceWithoutJSXIsUnchanged);
  Test('scanning a source allocates per token, not per character',
    TestScanAllocatesPerTokenNotPerCharacter);
end;

procedure TJSXTransformerTests.TestSourceWithoutJSXIsUnchanged;
var
  Source: string;
  TransformResult: TGocciaJSXTransformResult;
begin
  Source := RepeatedSource;
  TransformResult := TGocciaJSXTransformer.Transform(Source, 'input.js');
  Expect<string>(TransformResult.Source).ToBe(Source);
  Expect<Boolean>(TransformResult.SourceMap = nil).ToBe(True);
  Expect<Boolean>(TransformResult.OriginMap = nil).ToBe(True);
end;

procedure TJSXTransformerTests.TestScanAllocatesPerTokenNotPerCharacter;
var
  Source: string;
  TransformResult: TGocciaJSXTransformResult;
  Allocations: Int64;
begin
  // Char is WideChar, and `AChar in ['0'..'9']` converts each character to
  // an AnsiChar through the widestring manager, allocating a string per test:
  // the scan made several allocations per input character. Reading an
  // identifier still allocates its name: one per repeated element here.
  Source := RepeatedSource;
  StartCountingAllocations;
  TransformResult := TGocciaJSXTransformer.Transform(Source, 'input.js');
  Allocations := StopCountingAllocations;
  Expect<Integer>(Length(TransformResult.Source)).ToBe(Length(Source));
  if Allocations > 2 * REPEAT_COUNT then
    Fail(Format('%d allocations for %d characters; expected at most %d',
      [Allocations, Length(Source), 2 * REPEAT_COUNT]));
end;

begin
  TestRunnerProgram.AddSuite(TJSXTransformerTests.Create('JSX transformer'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
