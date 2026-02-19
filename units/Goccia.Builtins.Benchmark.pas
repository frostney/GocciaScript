unit Goccia.Builtins.Benchmark;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  TimingUtils,

  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.FunctionValue,
  Goccia.Values.Primitives;

type
  TGocciaBenchmark = class;

  TBenchmarkProgressEvent = procedure(const ASuiteName, ABenchName: string; const AIndex, ATotal: Integer) of object;

  TBenchmarkCase = class
  public
    Name: string;
    BenchFunction: TGocciaFunctionValue;
    SuiteName: string;
    constructor Create(const AName: string; const ABenchFunction: TGocciaFunctionValue; const ASuiteName: string);
  end;

  TBenchmarkResult = record
    Name: string;
    SuiteName: string;
    Iterations: Int64;
    TotalMs: Int64;
    OpsPerSec: Double;
    MeanMs: Double;
    VariancePercentage: Double;
  end;

  TGocciaBenchmark = class(TGocciaBuiltin)
  private
    FRegisteredSuites: TStringList;
    FRegisteredBenchmarks: TObjectList<TBenchmarkCase>;
    FCurrentSuiteName: string;
    FOnProgress: TBenchmarkProgressEvent;

    function RunSingleBenchmark(const ABenchCase: TBenchmarkCase): TBenchmarkResult;
    function CalibrateIterations(const ABenchCase: TBenchmarkCase): Int64;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
    destructor Destroy; override;

    function Suite(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function Bench(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RunBenchmarks(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    property OnProgress: TBenchmarkProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

uses
  Math,
  SysUtils,

  Goccia.GarbageCollector,
  Goccia.MicrotaskQueue,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue;

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

function GetEnvInt(const AName: string; const ADefault: Integer): Integer;
var
  S: string;
begin
  S := GetEnvironmentVariable(AName);
  if (S <> '') then
  begin
    if not TryStrToInt(S, Result) then
      Result := ADefault;
  end
  else
    Result := ADefault;
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

constructor TBenchmarkCase.Create(const AName: string; const ABenchFunction: TGocciaFunctionValue; const ASuiteName: string);
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

function TGocciaBenchmark.Suite(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  SuiteName: string;
  SuiteFunction: TGocciaFunctionValue;
  EmptyArgs: TGocciaArgumentsCollection;
  PreviousSuiteName: string;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  if AArgs.Length < 2 then Exit;
  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then Exit;
  if not (AArgs.GetElement(1) is TGocciaFunctionValue) then Exit;

  SuiteName := TGocciaStringLiteralValue(AArgs.GetElement(0)).Value;
  SuiteFunction := TGocciaFunctionValue(AArgs.GetElement(1));

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

function TGocciaBenchmark.Bench(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  BenchName: string;
  BenchFunction: TGocciaFunctionValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  if AArgs.Length < 2 then Exit;
  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then Exit;
  if not (AArgs.GetElement(1) is TGocciaFunctionValue) then Exit;

  BenchName := TGocciaStringLiteralValue(AArgs.GetElement(0)).Value;
  BenchFunction := TGocciaFunctionValue(AArgs.GetElement(1));

  FRegisteredBenchmarks.Add(TBenchmarkCase.Create(BenchName, BenchFunction, FCurrentSuiteName));
end;

function TGocciaBenchmark.CalibrateIterations(const ABenchCase: TBenchmarkCase): Int64;
var
  EmptyArgs: TGocciaArgumentsCollection;
  BatchSize: Int64;
  StartNanoseconds, ElapsedNanoseconds, TargetNanoseconds: Int64;
  I: Int64;
begin
  TargetNanoseconds := Int64(MIN_CALIBRATION_MS) * 1000000;
  BatchSize := CALIBRATION_BATCH;

  EmptyArgs := TGocciaArgumentsCollection.Create;
  try
    while True do
    begin
      StartNanoseconds := GetNanoseconds;
      I := 0;
      while I < BatchSize do
      begin
        ABenchCase.BenchFunction.Call(EmptyArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
        Inc(I);
      end;
      if Assigned(TGocciaMicrotaskQueue.Instance) then
        TGocciaMicrotaskQueue.Instance.DrainQueue;
      ElapsedNanoseconds := GetNanoseconds - StartNanoseconds;

      if ElapsedNanoseconds >= TargetNanoseconds then
      begin
        Result := (BatchSize div 10) * 10;
        if Result < 10 then Result := 10;
        Exit;
      end;

      if ElapsedNanoseconds = 0 then
        BatchSize := BatchSize * 10
      else
        BatchSize := BatchSize * (TargetNanoseconds div ElapsedNanoseconds + 1);

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

function TGocciaBenchmark.RunSingleBenchmark(const ABenchCase: TBenchmarkCase): TBenchmarkResult;
var
  EmptyArgs: TGocciaArgumentsCollection;
  Iterations: Int64;
  StartNanoseconds, RoundNanoseconds: Int64;
  I: Int64;
  Round, J, K: Integer;
  OpsRounds: array[0..4] of Double;   // Max 5 rounds
  MeanRounds: array[0..4] of Double;  // Max 5 rounds
  TempDouble, OpsMean, OpsVariance: Double;
begin
  Result.Name := ABenchCase.Name;
  Result.SuiteName := ABenchCase.SuiteName;

  EmptyArgs := TGocciaArgumentsCollection.Create;
  try
    // Phase 1: Warmup
    for K := 1 to WARMUP_ITERATIONS do
      ABenchCase.BenchFunction.Call(EmptyArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    if Assigned(TGocciaMicrotaskQueue.Instance) then
      TGocciaMicrotaskQueue.Instance.DrainQueue;

    // Phase 2: Calibrate to find iteration count per round
    Iterations := CalibrateIterations(ABenchCase);

    // Phase 3: Multiple measurement rounds, each running the full iteration count
    for Round := 0 to MEASUREMENT_ROUNDS - 1 do
    begin
      if Assigned(TGocciaGarbageCollector.Instance) then
        TGocciaGarbageCollector.Instance.Collect;

      StartNanoseconds := GetNanoseconds;
      I := 0;
      while I < Iterations do
      begin
        ABenchCase.BenchFunction.Call(EmptyArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
        Inc(I);
      end;
      if Assigned(TGocciaMicrotaskQueue.Instance) then
        TGocciaMicrotaskQueue.Instance.DrainQueue;
      RoundNanoseconds := GetNanoseconds - StartNanoseconds;

      if RoundNanoseconds > 0 then
      begin
        OpsRounds[Round] := (Iterations / RoundNanoseconds) * 1000000000;
        MeanRounds[Round] := (RoundNanoseconds / 1000000) / Iterations;
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
      Result.VariancePercentage := (Sqrt(OpsVariance) / OpsMean) * 100
    else
      Result.VariancePercentage := 0;

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

function TGocciaBenchmark.RunBenchmarks(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  BenchCase: TBenchmarkCase;
  BenchResult: TBenchmarkResult;
  ResultObj: TGocciaObjectValue;
  ResultsArray: TGocciaArrayValue;
  SingleResult: TGocciaObjectValue;
  StartNanoseconds, TotalDurationNanoseconds: Int64;
begin
  StartNanoseconds := GetNanoseconds;

  // Protect ALL benchmark functions from GC before running any benchmarks.
  // These are held by Pascal TBenchmarkCase objects, not in GocciaScript scopes,
  // so the GC wouldn't see them as roots otherwise.
  if Assigned(TGocciaGarbageCollector.Instance) then
    for I := 0 to FRegisteredBenchmarks.Count - 1 do
      TGocciaGarbageCollector.Instance.AddTempRoot(FRegisteredBenchmarks[I].BenchFunction);

  try
    ResultsArray := TGocciaArrayValue.Create;
    // Protect the results array from GC - it's a local Pascal variable
    if Assigned(TGocciaGarbageCollector.Instance) then
      TGocciaGarbageCollector.Instance.AddTempRoot(ResultsArray);

    for I := 0 to FRegisteredBenchmarks.Count - 1 do
    begin
      BenchCase := FRegisteredBenchmarks[I];

      if Assigned(FOnProgress) then
        FOnProgress(BenchCase.SuiteName, BenchCase.Name, I + 1, FRegisteredBenchmarks.Count);

      try
        BenchResult := RunSingleBenchmark(BenchCase);

        SingleResult := TGocciaObjectValue.Create;
        SingleResult.AssignProperty('name', TGocciaStringLiteralValue.Create(BenchResult.Name));
        SingleResult.AssignProperty('suite', TGocciaStringLiteralValue.Create(BenchResult.SuiteName));
        SingleResult.AssignProperty('opsPerSec', TGocciaNumberLiteralValue.Create(BenchResult.OpsPerSec));
        SingleResult.AssignProperty('meanMs', TGocciaNumberLiteralValue.Create(BenchResult.MeanMs));
        SingleResult.AssignProperty('iterations', TGocciaNumberLiteralValue.Create(BenchResult.Iterations));
        SingleResult.AssignProperty('totalMs', TGocciaNumberLiteralValue.Create(BenchResult.TotalMs));
        SingleResult.AssignProperty('variancePercentage', TGocciaNumberLiteralValue.Create(BenchResult.VariancePercentage));

        ResultsArray.SetElement(ResultsArray.GetLength, SingleResult);
      except
        on E: Exception do
        begin
          if Assigned(TGocciaMicrotaskQueue.Instance) then
            TGocciaMicrotaskQueue.Instance.ClearQueue;

          SingleResult := TGocciaObjectValue.Create;
          SingleResult.AssignProperty('name', TGocciaStringLiteralValue.Create(BenchCase.Name));
          SingleResult.AssignProperty('suite', TGocciaStringLiteralValue.Create(BenchCase.SuiteName));
          SingleResult.AssignProperty('error', TGocciaStringLiteralValue.Create(E.Message));

          ResultsArray.SetElement(ResultsArray.GetLength, SingleResult);
        end;
      end;
    end;

    TotalDurationNanoseconds := GetNanoseconds - StartNanoseconds;

    ResultObj := TGocciaObjectValue.Create;
    ResultObj.AssignProperty('totalBenchmarks', TGocciaNumberLiteralValue.Create(FRegisteredBenchmarks.Count));
    ResultObj.AssignProperty('results', ResultsArray);
    ResultObj.AssignProperty('durationNanoseconds', TGocciaNumberLiteralValue.Create(TotalDurationNanoseconds));

    Result := ResultObj;
  finally
    // Remove ALL benchmark functions and results array from temp roots
    if Assigned(TGocciaGarbageCollector.Instance) then
    begin
      for I := 0 to FRegisteredBenchmarks.Count - 1 do
        TGocciaGarbageCollector.Instance.RemoveTempRoot(FRegisteredBenchmarks[I].BenchFunction);
      TGocciaGarbageCollector.Instance.RemoveTempRoot(ResultsArray);
    end;
  end;
end;

initialization
  InitBenchmarkConfig;

end.
