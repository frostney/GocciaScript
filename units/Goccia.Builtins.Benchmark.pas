unit Goccia.Builtins.Benchmark;

{$I Goccia.inc}

interface

uses
  Goccia.Builtins.Base,
  Goccia.Values.ObjectValue,
  Goccia.Values.FunctionValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives,
  Goccia.Values.ArrayValue,
  Goccia.Arguments.Collection,
  Goccia.Scope,
  Goccia.Error, Goccia.Error.ThrowErrorCallback,
  Generics.Collections,
  Classes,
  SysUtils,
  {$IFDEF UNIX}Unix,{$ENDIF}
  Math;

type
  TGocciaBenchmark = class;

  TBenchmarkCase = class
  public
    Name: string;
    BenchFunction: TGocciaFunctionValue;
    SuiteName: string;
    constructor Create(const AName: string; ABenchFunction: TGocciaFunctionValue; const ASuiteName: string);
  end;

  TBenchmarkResult = record
    Name: string;
    SuiteName: string;
    Iterations: Int64;
    TotalMs: Int64;
    OpsPerSec: Double;
    MeanMs: Double;
    VariancePct: Double;
  end;

  TGocciaBenchmark = class(TGocciaBuiltin)
  private
    FRegisteredSuites: TStringList;
    FRegisteredBenchmarks: TObjectList<TBenchmarkCase>;
    FCurrentSuiteName: string;

    function RunSingleBenchmark(BenchCase: TBenchmarkCase): TBenchmarkResult;
    function CalibrateIterations(BenchCase: TBenchmarkCase): Int64;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
    destructor Destroy; override;

    function Suite(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function Bench(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function RunBenchmarks(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  Goccia.Values.ClassHelper, Goccia.GarbageCollector;

const
  DEFAULT_WARMUP_ITERATIONS = 3;
  DEFAULT_MIN_CALIBRATION_MS = 300;  // Run for at least 300ms during calibration
  DEFAULT_CALIBRATION_BATCH = 10;    // Initial batch size for calibration
  DEFAULT_MEASUREMENT_ROUNDS = 3;    // Number of measurement rounds (take median)

var
  WARMUP_ITERATIONS: Integer;
  MIN_CALIBRATION_MS: Int64;
  CALIBRATION_BATCH: Int64;
  MEASUREMENT_ROUNDS: Integer;

function GetEnvInt(const Name: string; Default: Integer): Integer;
var
  S: string;
begin
  S := GetEnvironmentVariable(Name);
  if (S <> '') then
  begin
    if not TryStrToInt(S, Result) then
      Result := Default;
  end
  else
    Result := Default;
end;

procedure InitBenchmarkConfig;
begin
  WARMUP_ITERATIONS := GetEnvInt('GOCCIA_BENCH_WARMUP', DEFAULT_WARMUP_ITERATIONS);
  MIN_CALIBRATION_MS := GetEnvInt('GOCCIA_BENCH_CALIBRATION_MS', DEFAULT_MIN_CALIBRATION_MS);
  CALIBRATION_BATCH := GetEnvInt('GOCCIA_BENCH_CALIBRATION_BATCH', DEFAULT_CALIBRATION_BATCH);
  MEASUREMENT_ROUNDS := GetEnvInt('GOCCIA_BENCH_ROUNDS', DEFAULT_MEASUREMENT_ROUNDS);
  if MEASUREMENT_ROUNDS > 5 then MEASUREMENT_ROUNDS := 5;
  if MEASUREMENT_ROUNDS < 1 then MEASUREMENT_ROUNDS := 1;
end;

{ TBenchmarkCase }

constructor TBenchmarkCase.Create(const AName: string; ABenchFunction: TGocciaFunctionValue; const ASuiteName: string);
begin
  inherited Create;
  Name := AName;
  BenchFunction := ABenchFunction;
  SuiteName := ASuiteName;
end;

{ TGocciaBenchmark }

constructor TGocciaBenchmark.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FRegisteredSuites := TStringList.Create;
  FRegisteredBenchmarks := TObjectList<TBenchmarkCase>.Create;
  FCurrentSuiteName := '';

  // Register benchmark functions globally
  AScope.DefineLexicalBinding('suite', TGocciaNativeFunctionValue.Create(Suite, 'suite', 2), dtConst);
  AScope.DefineLexicalBinding('bench', TGocciaNativeFunctionValue.Create(Bench, 'bench', 2), dtConst);
  AScope.DefineLexicalBinding('runBenchmarks', TGocciaNativeFunctionValue.Create(RunBenchmarks, 'runBenchmarks', 0), dtConst);
end;

destructor TGocciaBenchmark.Destroy;
begin
  FRegisteredSuites.Free;
  FRegisteredBenchmarks.Free;
  inherited;
end;

function TGocciaBenchmark.Suite(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  SuiteName: string;
  SuiteFunction: TGocciaFunctionValue;
  EmptyArgs: TGocciaArgumentsCollection;
  PreviousSuiteName: string;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  if Args.Length < 2 then Exit;
  if not (Args.GetElement(0) is TGocciaStringLiteralValue) then Exit;
  if not (Args.GetElement(1) is TGocciaFunctionValue) then Exit;

  SuiteName := TGocciaStringLiteralValue(Args.GetElement(0)).Value;
  SuiteFunction := TGocciaFunctionValue(Args.GetElement(1));

  FRegisteredSuites.Add(SuiteName);

  // Execute the suite function to register bench() calls
  PreviousSuiteName := FCurrentSuiteName;
  FCurrentSuiteName := SuiteName;
  try
    EmptyArgs := TGocciaArgumentsCollection.Create;
    try
      SuiteFunction.Call(EmptyArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      EmptyArgs.Free;
    end;
  finally
    FCurrentSuiteName := PreviousSuiteName;
  end;
end;

function TGocciaBenchmark.Bench(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  BenchName: string;
  BenchFunction: TGocciaFunctionValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  if Args.Length < 2 then Exit;
  if not (Args.GetElement(0) is TGocciaStringLiteralValue) then Exit;
  if not (Args.GetElement(1) is TGocciaFunctionValue) then Exit;

  BenchName := TGocciaStringLiteralValue(Args.GetElement(0)).Value;
  BenchFunction := TGocciaFunctionValue(Args.GetElement(1));

  FRegisteredBenchmarks.Add(TBenchmarkCase.Create(BenchName, BenchFunction, FCurrentSuiteName));
end;

function GetMicroseconds: Int64;
{$IFDEF UNIX}
var
  tv: TTimeVal;
begin
  fpGetTimeOfDay(@tv, nil);
  Result := Int64(tv.tv_sec) * 1000000 + Int64(tv.tv_usec);
end;
{$ELSE}
begin
  Result := GetTickCount64 * 1000;
end;
{$ENDIF}

function TGocciaBenchmark.CalibrateIterations(BenchCase: TBenchmarkCase): Int64;
var
  EmptyArgs: TGocciaArgumentsCollection;
  BatchSize: Int64;
  StartUs, ElapsedUs, TargetUs: Int64;
  I: Int64;
begin
  TargetUs := Int64(MIN_CALIBRATION_MS) * 1000;
  BatchSize := CALIBRATION_BATCH;

  EmptyArgs := TGocciaArgumentsCollection.Create;
  try
    while True do
    begin
      StartUs := GetMicroseconds;
      I := 0;
      while I < BatchSize do
      begin
        BenchCase.BenchFunction.Call(EmptyArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
        Inc(I);
      end;
      ElapsedUs := GetMicroseconds - StartUs;

      if ElapsedUs >= TargetUs then
      begin
        Result := (BatchSize div 10) * 10;
        if Result < 10 then Result := 10;
        Exit;
      end;

      if ElapsedUs = 0 then
        BatchSize := BatchSize * 10
      else
        BatchSize := BatchSize * (TargetUs div ElapsedUs + 1);

      if BatchSize > 10000000 then
      begin
        Result := (BatchSize div 10) * 10;
        Exit;
      end;
    end;
  finally
    EmptyArgs.Free;
  end;
end;

function TGocciaBenchmark.RunSingleBenchmark(BenchCase: TBenchmarkCase): TBenchmarkResult;
var
  EmptyArgs: TGocciaArgumentsCollection;
  Iterations: Int64;
  StartUs, RoundUs: Int64;
  I: Int64;
  Round, J, K: Integer;
  OpsRounds: array[0..4] of Double;   // Max 5 rounds
  MeanRounds: array[0..4] of Double;  // Max 5 rounds
  TempDouble, OpsMean, OpsVariance: Double;
begin
  Result.Name := BenchCase.Name;
  Result.SuiteName := BenchCase.SuiteName;

  EmptyArgs := TGocciaArgumentsCollection.Create;
  try
    // Phase 1: Warmup
    for K := 1 to WARMUP_ITERATIONS do
      BenchCase.BenchFunction.Call(EmptyArgs, TGocciaUndefinedLiteralValue.UndefinedValue);

    // Phase 2: Calibrate to find iteration count per round
    Iterations := CalibrateIterations(BenchCase);

    // Phase 3: Multiple measurement rounds, each running the full iteration count
    for Round := 0 to MEASUREMENT_ROUNDS - 1 do
    begin
      if Assigned(TGocciaGC.Instance) then
        TGocciaGC.Instance.Collect;

      StartUs := GetMicroseconds;
      I := 0;
      while I < Iterations do
      begin
        BenchCase.BenchFunction.Call(EmptyArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
        Inc(I);
      end;
      RoundUs := GetMicroseconds - StartUs;

      if RoundUs > 0 then
      begin
        OpsRounds[Round] := (Iterations / RoundUs) * 1000000;
        MeanRounds[Round] := (RoundUs / 1000) / Iterations;
      end
      else
      begin
        OpsRounds[Round] := 0;
        MeanRounds[Round] := 0;
      end;
    end;

    // Compute coefficient of variation (CV%) from unsorted ops/sec data
    OpsMean := 0;
    for K := 0 to MEASUREMENT_ROUNDS - 1 do
      OpsMean := OpsMean + OpsRounds[K];
    OpsMean := OpsMean / MEASUREMENT_ROUNDS;

    OpsVariance := 0;
    for K := 0 to MEASUREMENT_ROUNDS - 1 do
      OpsVariance := OpsVariance + Sqr(OpsRounds[K] - OpsMean);
    OpsVariance := OpsVariance / MEASUREMENT_ROUNDS;

    if OpsMean > 0 then
      Result.VariancePct := (Sqrt(OpsVariance) / OpsMean) * 100
    else
      Result.VariancePct := 0;

    // Sort rounds to find median (insertion sort for small N)
    for K := 1 to MEASUREMENT_ROUNDS - 1 do
    begin
      TempDouble := OpsRounds[K];
      J := K - 1;
      while (J >= 0) and (OpsRounds[J] > TempDouble) do
      begin
        OpsRounds[J + 1] := OpsRounds[J];
        Dec(J);
      end;
      OpsRounds[J + 1] := TempDouble;
    end;
    for K := 1 to MEASUREMENT_ROUNDS - 1 do
    begin
      TempDouble := MeanRounds[K];
      J := K - 1;
      while (J >= 0) and (MeanRounds[J] > TempDouble) do
      begin
        MeanRounds[J + 1] := MeanRounds[J];
        Dec(J);
      end;
      MeanRounds[J + 1] := TempDouble;
    end;

    // Report median values
    Result.Iterations := Iterations;
    Result.TotalMs := 0;
    Result.OpsPerSec := OpsRounds[MEASUREMENT_ROUNDS div 2];
    Result.MeanMs := MeanRounds[MEASUREMENT_ROUNDS div 2];
  finally
    EmptyArgs.Free;
  end;
end;

function TGocciaBenchmark.RunBenchmarks(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  BenchCase: TBenchmarkCase;
  BenchResult: TBenchmarkResult;
  ResultObj: TGocciaObjectValue;
  ResultsArray: TGocciaArrayValue;
  SingleResult: TGocciaObjectValue;
  StartUs, TotalDurationMs: Int64;
begin
  StartUs := GetMicroseconds;

  // Protect ALL benchmark functions from GC before running any benchmarks.
  // These are held by Pascal TBenchmarkCase objects, not in GocciaScript scopes,
  // so the GC wouldn't see them as roots otherwise.
  if Assigned(TGocciaGC.Instance) then
    for I := 0 to FRegisteredBenchmarks.Count - 1 do
      TGocciaGC.Instance.AddTempRoot(FRegisteredBenchmarks[I].BenchFunction);

  try
    ResultsArray := TGocciaArrayValue.Create;
    // Protect the results array from GC - it's a local Pascal variable
    if Assigned(TGocciaGC.Instance) then
      TGocciaGC.Instance.AddTempRoot(ResultsArray);

    for I := 0 to FRegisteredBenchmarks.Count - 1 do
    begin
      BenchCase := FRegisteredBenchmarks[I];

      try
        BenchResult := RunSingleBenchmark(BenchCase);

        SingleResult := TGocciaObjectValue.Create;
        SingleResult.AssignProperty('name', TGocciaStringLiteralValue.Create(BenchResult.Name));
        SingleResult.AssignProperty('suite', TGocciaStringLiteralValue.Create(BenchResult.SuiteName));
        SingleResult.AssignProperty('opsPerSec', TGocciaNumberLiteralValue.Create(BenchResult.OpsPerSec));
        SingleResult.AssignProperty('meanMs', TGocciaNumberLiteralValue.Create(BenchResult.MeanMs));
        SingleResult.AssignProperty('iterations', TGocciaNumberLiteralValue.Create(BenchResult.Iterations));
        SingleResult.AssignProperty('totalMs', TGocciaNumberLiteralValue.Create(BenchResult.TotalMs));
        SingleResult.AssignProperty('variancePct', TGocciaNumberLiteralValue.Create(BenchResult.VariancePct));

        ResultsArray.SetElement(ResultsArray.GetLength, SingleResult);
      except
        on E: Exception do
        begin
          SingleResult := TGocciaObjectValue.Create;
          SingleResult.AssignProperty('name', TGocciaStringLiteralValue.Create(BenchCase.Name));
          SingleResult.AssignProperty('suite', TGocciaStringLiteralValue.Create(BenchCase.SuiteName));
          SingleResult.AssignProperty('error', TGocciaStringLiteralValue.Create(E.Message));

          ResultsArray.SetElement(ResultsArray.GetLength, SingleResult);
        end;
      end;
    end;

    TotalDurationMs := (GetMicroseconds - StartUs) div 1000;

    ResultObj := TGocciaObjectValue.Create;
    ResultObj.AssignProperty('totalBenchmarks', TGocciaNumberLiteralValue.Create(FRegisteredBenchmarks.Count));
    ResultObj.AssignProperty('results', ResultsArray);
    ResultObj.AssignProperty('duration', TGocciaNumberLiteralValue.Create(TotalDurationMs));

    Result := ResultObj;
  finally
    // Remove ALL benchmark functions and results array from temp roots
    if Assigned(TGocciaGC.Instance) then
    begin
      for I := 0 to FRegisteredBenchmarks.Count - 1 do
        TGocciaGC.Instance.RemoveTempRoot(FRegisteredBenchmarks[I].BenchFunction);
      TGocciaGC.Instance.RemoveTempRoot(ResultsArray);
    end;
  end;
end;

initialization
  InitBenchmarkConfig;

end.
