program Goccia.VM.DirectEvalCodeCache.Test;

{$I Goccia.inc}

{ The per-call-site code cache of direct eval (ADR 0134 Phase 4): lookups by
  source text and context, and the entry cap past which a site caches no
  more code. }

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.Bytecode.Chunk,
  Goccia.SourcePipeline,
  Goccia.VM.DirectEvalCodeCache;

const
  TEST_SOURCE_NAME = 'cache-test.js';
  TEST_TEMPLATE_NAME = '<eval>';
  UNCACHED_SOURCE = 'uncached';

type
  TDirectEvalCodeCacheTests = class(TTestSuite)
  private
    function Context(const AAllowNewTarget: Boolean): TGocciaDirectEvalCodeContext;
    procedure TestFindsCodeBySourceAndContext;
    procedure TestStopsCachingAtTheCap;
  public
    procedure SetupTests; override;
  end;

procedure TDirectEvalCodeCacheTests.SetupTests;
begin
  Test('Finds code by source text and context', TestFindsCodeBySourceAndContext);
  Test('Caches no more code past the entry cap, and keeps serving what it has',
    TestStopsCachingAtTheCap);
end;

function TDirectEvalCodeCacheTests.Context(
  const AAllowNewTarget: Boolean): TGocciaDirectEvalCodeContext;
begin
  Result := Default(TGocciaDirectEvalCodeContext);
  Result.SourceName := TEST_SOURCE_NAME;
  Result.ParseOptions := TGocciaSourcePipeline.DefaultOptions;
  Result.AllowNewTarget := AAllowNewTarget;
end;

procedure TDirectEvalCodeCacheTests.TestFindsCodeBySourceAndContext;
var
  Cache: TGocciaDirectEvalCodeCache;
  Template: TGocciaFunctionTemplate;
  InFunction, AtTopLevel: TGocciaDirectEvalCode;
begin
  Template := TGocciaFunctionTemplate.Create(TEST_TEMPLATE_NAME);
  Cache := TGocciaDirectEvalCodeCache.Create;
  try
    InFunction := TGocciaDirectEvalCode.Create(Context(True), Template, True,
      nil, nil, nil, nil);
    AtTopLevel := TGocciaDirectEvalCode.Create(Context(False), Template,
      True, nil, nil, nil, nil);
    Expect<Boolean>(Cache.TryAdd('x + 1', InFunction)).ToBe(True);
    Expect<Boolean>(Cache.TryAdd('x + 1', AtTopLevel)).ToBe(True);
    Expect<Integer>(Cache.Count).ToBe(2);
    Expect<Boolean>(Cache.Find('x + 1', Context(True)) = InFunction).ToBe(True);
    Expect<Boolean>(Cache.Find('x + 1', Context(False)) = AtTopLevel).ToBe(
      True);
    Expect<Boolean>(Cache.Find('x + 2', Context(True)) = nil).ToBe(True);
  finally
    Cache.Free;
    Template.Free;
  end;
end;

procedure TDirectEvalCodeCacheTests.TestStopsCachingAtTheCap;
var
  Cache: TGocciaDirectEvalCodeCache;
  Template: TGocciaFunctionTemplate;
  Rejected, RejectedContext: TGocciaDirectEvalCode;
  I: Integer;
begin
  Template := TGocciaFunctionTemplate.Create(TEST_TEMPLATE_NAME);
  Cache := TGocciaDirectEvalCodeCache.Create;
  Rejected := nil;
  RejectedContext := nil;
  try
    for I := 0 to DIRECT_EVAL_CODE_CACHE_MAX_ENTRIES - 1 do
      Expect<Boolean>(Cache.TryAdd(IntToStr(I),
        TGocciaDirectEvalCode.Create(Context(True), Template, True, nil, nil,
          nil, nil))).ToBe(True);
    Expect<Integer>(Cache.Count).ToBe(DIRECT_EVAL_CODE_CACHE_MAX_ENTRIES);

    Rejected := TGocciaDirectEvalCode.Create(Context(True), Template, True,
      nil, nil, nil, nil);
    Expect<Boolean>(Cache.TryAdd(UNCACHED_SOURCE, Rejected)).ToBe(False);
    // Another context of a cached source is an entry of its own.
    RejectedContext := TGocciaDirectEvalCode.Create(Context(False), Template,
      True, nil, nil, nil, nil);
    Expect<Boolean>(Cache.TryAdd('0', RejectedContext)).ToBe(False);
    Expect<Integer>(Cache.Count).ToBe(DIRECT_EVAL_CODE_CACHE_MAX_ENTRIES);
    Expect<Boolean>(Cache.Find(UNCACHED_SOURCE, Context(True)) = nil).ToBe(
      True);
    Expect<Boolean>(Cache.Find('0', Context(False)) = nil).ToBe(True);
    for I := 0 to DIRECT_EVAL_CODE_CACHE_MAX_ENTRIES - 1 do
      Expect<Boolean>(Assigned(Cache.Find(IntToStr(I), Context(True)))).ToBe(
        True);
  finally
    RejectedContext.Free;
    Rejected.Free;
    Cache.Free;
    Template.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(
    TDirectEvalCodeCacheTests.Create('Direct Eval Code Cache'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
