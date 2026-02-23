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
    SuiteName: string;
    SetupFunction: TGocciaFunctionValue;
    RunFunction: TGocciaFunctionValue;
    TeardownFunction: TGocciaFunctionValue;
    constructor Create(const AName: string; const ARunFunction: TGocciaFunctionValue;
      const ASuiteName: string; const ASetupFunction: TGocciaFunctionValue = nil;
      const ATeardownFunction: TGocciaFunctionValue = nil);
  end;

  TBenchmarkResult = record
    Name: string;
    SuiteName: string;
    Iterations: Int64;
    TotalMs: Int64;
    OpsPerSec: Double;
    MeanMs: Double;
    VariancePercentage: Double;
    SetupMs: Double;
    TeardownMs: Double;
  end;

  TGocciaBenchmark = class(TGocciaBuiltin)
  private
    FRegisteredSuites: TStringList;
    FRegisteredBenchmarks: TObjectList<TBenchmarkCase>;
    FCurrentSuiteName: string;
    FOnProgress: TBenchmarkProgressEvent;

    function RunSingleBenchmark(const ABenchCase: TBenchmarkCase): TBenchmarkResult;
    function CalibrateIterations(const ABenchCase: TBenchmarkCase; const ARunArgs: TGocciaArgumentsCollection): Int64;
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
  DEFAULT_MIN_CALIBRATION_MS = 100;
  DEFAULT_CALIBRATION_BATCH = 5;
  DEFAULT_MEASUREMENT_ROUNDS = 5;

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

constructor TBenchmarkCase.Create(const AName: string; const ARunFunction: TGocciaFunctionValue;
  const ASuiteName: string; const ASetupFunction: TGocciaFunctionValue;
  const ATeardownFunction: TGocciaFunctionValue);
begin
  inherited Create;
  Name := AName;
  RunFunction := ARunFunction;
  SuiteName := ASuiteName;
  SetupFunction := ASetupFunction;
  TeardownFunction := ATeardownFunction;
end;

{ TGocciaBenchmark }

constructor TGocciaBenchmark.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FRegisteredSuites := TStringList.Create;
  FRegisteredBenchmarks := TObjectList<TBenchmarkCase>.Create;
  FCurrentSuiteName := '';

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
  OptionsObj: TGocciaObjectValue;
  RunProp, SetupProp, TeardownProp: TGocciaValue;
  RunFn, SetupFn, TeardownFn: TGocciaFunctionValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  if AArgs.Length < 2 then Exit;
  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then Exit;
  if not (AArgs.GetElement(1) is TGocciaObjectValue) then Exit;

  BenchName := TGocciaStringLiteralValue(AArgs.GetElement(0)).Value;
  OptionsObj := TGocciaObjectValue(AArgs.GetElement(1));

  RunProp := OptionsObj.GetProperty('run');
  if not Assigned(RunProp) or not (RunProp is TGocciaFunctionValue) then Exit;
  RunFn := TGocciaFunctionValue(RunProp);

  SetupFn := nil;
  SetupProp := OptionsObj.GetProperty('setup');
  if Assigned(SetupProp) and (SetupProp is TGocciaFunctionValue) then
    SetupFn := TGocciaFunctionValue(SetupProp);

  TeardownFn := nil;
  TeardownProp := OptionsObj.GetProperty('teardown');
  if Assigned(TeardownProp) and (TeardownProp is TGocciaFunctionValue) then
    TeardownFn := TGocciaFunctionValue(TeardownProp);

  FRegisteredBenchmarks.Add(TBenchmarkCase.Create(BenchName, RunFn, FCurrentSuiteName, SetupFn, TeardownFn));
end;

function TGocciaBenchmark.CalibrateIterations(const ABenchCase: TBenchmarkCase; const ARunArgs: TGocciaArgumentsCollection): Int64;
var
  BatchSize: Int64;
  StartNanoseconds, ElapsedNanoseconds, TargetNanoseconds: Int64;
  I: Int64;
begin
  TargetNanoseconds := Int64(MIN_CALIBRATION_MS) * 1000000;
  BatchSize := CALIBRATION_BATCH;

  while True do
  begin
    StartNanoseconds := GetNanoseconds;
    I := 0;
    while I < BatchSize do
    begin
      ABenchCase.RunFunction.Call(ARunArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
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
end;

function TGocciaBenchmark.RunSingleBenchmark(const ABenchCase: TBenchmarkCase): TBenchmarkResult;
var
  RunArgs, EmptyArgs: TGocciaArgumentsCollection;
  SetupResult: TGocciaValue;
  Iterations: Int64;
  StartNanoseconds, RoundNanoseconds: Int64;
  I: Int64;
  Round, J, K: Integer;
  OpsRounds: array[0..4] of Double;
  MeanRounds: array[0..4] of Double;
  TempDouble, OpsMean, OpsVariance: Double;
  GC: TGocciaGarbageCollector;
begin
  Result.Name := ABenchCase.Name;
  Result.SuiteName := ABenchCase.SuiteName;
  Result.SetupMs := 0;
  Result.TeardownMs := 0;

  GC := TGocciaGarbageCollector.Instance;
  SetupResult := nil;
  RunArgs := nil;
  EmptyArgs := TGocciaArgumentsCollection.Create;
  try
    // Phase 0: Setup (timed, runs once)
    if Assigned(ABenchCase.SetupFunction) then
    begin
      StartNanoseconds := GetNanoseconds;
      SetupResult := ABenchCase.SetupFunction.Call(EmptyArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
      if Assigned(TGocciaMicrotaskQueue.Instance) then
        TGocciaMicrotaskQueue.Instance.DrainQueue;
      Result.SetupMs := (GetNanoseconds - StartNanoseconds) / 1000000;

      if Assigned(SetupResult) and Assigned(GC) then
        GC.AddTempRoot(SetupResult);
    end;

    try
      if Assigned(SetupResult) and not (SetupResult is TGocciaUndefinedLiteralValue) then
      begin
        RunArgs := TGocciaArgumentsCollection.Create([SetupResult]);
      end
      else
        RunArgs := TGocciaArgumentsCollection.Create;

      // Phase 1: Warmup
      for K := 1 to WARMUP_ITERATIONS do
        ABenchCase.RunFunction.Call(RunArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
      if Assigned(TGocciaMicrotaskQueue.Instance) then
        TGocciaMicrotaskQueue.Instance.DrainQueue;

      // Phase 2: Calibrate
      Iterations := CalibrateIterations(ABenchCase, RunArgs);

      // Phase 3: Measurement rounds
      for Round := 0 to MEASUREMENT_ROUNDS - 1 do
      begin
        if Assigned(GC) then
          GC.Collect;

        StartNanoseconds := GetNanoseconds;
        I := 0;
        while I < Iterations do
        begin
          ABenchCase.RunFunction.Call(RunArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
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

      Result.Iterations := Iterations;
      Result.TotalMs := 0;
      Result.OpsPerSec := OpsRounds[MEASUREMENT_ROUNDS div 2];
      Result.MeanMs := MeanRounds[MEASUREMENT_ROUNDS div 2];

      // Phase 4: Teardown (timed, runs once)
      if Assigned(ABenchCase.TeardownFunction) then
      begin
        StartNanoseconds := GetNanoseconds;
        ABenchCase.TeardownFunction.Call(RunArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
        if Assigned(TGocciaMicrotaskQueue.Instance) then
          TGocciaMicrotaskQueue.Instance.DrainQueue;
        Result.TeardownMs := (GetNanoseconds - StartNanoseconds) / 1000000;
      end;
    finally
      if Assigned(SetupResult) and Assigned(GC) then
        GC.RemoveTempRoot(SetupResult);
      RunArgs.Free;
    end;
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

  if Assigned(TGocciaGarbageCollector.Instance) then
    for I := 0 to FRegisteredBenchmarks.Count - 1 do
    begin
      TGocciaGarbageCollector.Instance.AddTempRoot(FRegisteredBenchmarks[I].RunFunction);
      if Assigned(FRegisteredBenchmarks[I].SetupFunction) then
        TGocciaGarbageCollector.Instance.AddTempRoot(FRegisteredBenchmarks[I].SetupFunction);
      if Assigned(FRegisteredBenchmarks[I].TeardownFunction) then
        TGocciaGarbageCollector.Instance.AddTempRoot(FRegisteredBenchmarks[I].TeardownFunction);
    end;

  try
    ResultsArray := TGocciaArrayValue.Create;
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
        SingleResult.AssignProperty('setupMs', TGocciaNumberLiteralValue.Create(BenchResult.SetupMs));
        SingleResult.AssignProperty('teardownMs', TGocciaNumberLiteralValue.Create(BenchResult.TeardownMs));

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
    if Assigned(TGocciaGarbageCollector.Instance) then
    begin
      for I := 0 to FRegisteredBenchmarks.Count - 1 do
      begin
        TGocciaGarbageCollector.Instance.RemoveTempRoot(FRegisteredBenchmarks[I].RunFunction);
        if Assigned(FRegisteredBenchmarks[I].SetupFunction) then
          TGocciaGarbageCollector.Instance.RemoveTempRoot(FRegisteredBenchmarks[I].SetupFunction);
        if Assigned(FRegisteredBenchmarks[I].TeardownFunction) then
          TGocciaGarbageCollector.Instance.RemoveTempRoot(FRegisteredBenchmarks[I].TeardownFunction);
      end;
      TGocciaGarbageCollector.Instance.RemoveTempRoot(ResultsArray);
    end;
  end;
end;

initialization
  InitBenchmarkConfig;

end.
