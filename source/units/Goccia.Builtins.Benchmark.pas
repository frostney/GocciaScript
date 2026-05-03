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
  Goccia.Values.FunctionBase,
  Goccia.Values.Primitives;

type
  TGocciaBenchmark = class;

  TBenchmarkProgressEvent = procedure(const ASuiteName, ABenchName: string; const AIndex, ATotal: Integer) of object;
  TBenchmarkNotifyEvent = procedure of object;

  TBenchmarkCase = class
  public
    Name: string;
    SuiteName: string;
    SetupFunction: TGocciaFunctionBase;
    RunFunction: TGocciaFunctionBase;
    TeardownFunction: TGocciaFunctionBase;
    OwnsRunRoot: Boolean;
    OwnsSetupRoot: Boolean;
    OwnsTeardownRoot: Boolean;
    constructor Create(const AName: string; const ARunFunction: TGocciaFunctionBase;
      const ASuiteName: string; const ASetupFunction: TGocciaFunctionBase = nil;
      const ATeardownFunction: TGocciaFunctionBase = nil);
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
    MinOpsPerSec: Double;
    MaxOpsPerSec: Double;
  end;

  TGocciaBenchmark = class(TGocciaBuiltin)
  private
    FRegisteredSuites: TStringList;
    FRegisteredBenchmarks: TObjectList<TBenchmarkCase>;
    FCurrentSuiteName: string;
    FOnProgress: TBenchmarkProgressEvent;
    FOnBeforeMeasurement: TBenchmarkNotifyEvent;

    function RunSingleBenchmark(const ABenchCase: TBenchmarkCase): TBenchmarkResult;
    function RunSingleBenchmarkDeterministic(const ABenchCase: TBenchmarkCase): TBenchmarkResult;
    function CalibrateIterations(const ABenchCase: TBenchmarkCase;
      const ASetupResult: TGocciaValue;
      const ARunArgs: TGocciaArgumentsCollection): Int64;
    procedure UnrootRegisteredBenchmarks;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
    destructor Destroy; override;

    function Suite(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function Bench(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RunBenchmarks(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RunDeterministicProfile(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    property OnProgress: TBenchmarkProgressEvent read FOnProgress write FOnProgress;
    property OnBeforeMeasurement: TBenchmarkNotifyEvent read FOnBeforeMeasurement write FOnBeforeMeasurement;
  end;

implementation

uses
  Math,
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Evaluator,
  Goccia.FetchManager,
  Goccia.GarbageCollector,
  Goccia.MicrotaskQueue,
  Goccia.Values.ArrayValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.PromiseValue;

const
  DEFAULT_WARMUP_ITERATIONS = 5;
  DEFAULT_MIN_CALIBRATION_MS = 200;
  DEFAULT_CALIBRATION_BATCH = 5;
  DEFAULT_MEASUREMENT_ROUNDS = 7;
  MAX_MEASUREMENT_ROUNDS = 50;
  IQR_MULTIPLIER = 1.5;

var
  { Set once by InitBenchmarkConfig in the initialization section, then
    read-only during execution. Plain var is safe because the values are
    frozen before any worker thread is spawned. }
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
  if MEASUREMENT_ROUNDS > MAX_MEASUREMENT_ROUNDS then MEASUREMENT_ROUNDS := MAX_MEASUREMENT_ROUNDS;
  if MEASUREMENT_ROUNDS < 1 then MEASUREMENT_ROUNDS := 1;
end;

function InvokeBenchmarkFunction(const AFunction: TGocciaFunctionBase;
  const ASetupResult: TGocciaValue;
  const ARunArgs: TGocciaArgumentsCollection): TGocciaValue;
var
  ThenMethod: TGocciaValue;
begin
  ARunArgs.Clear;
  if Assigned(ASetupResult) then
    ARunArgs.Add(ASetupResult);
  Result := AFunction.CallPreparedArgs(ARunArgs,
    TGocciaUndefinedLiteralValue.UndefinedValue);
  if Result is TGocciaPromiseValue then
    Result := AwaitValue(Result)
  else if Result is TGocciaObjectValue then
  begin
    ThenMethod := Result.GetProperty(PROP_THEN);
    if Assigned(ThenMethod) and not (ThenMethod is TGocciaUndefinedLiteralValue) and
       ThenMethod.IsCallable then
      Result := AwaitValue(Result);
  end;
end;

{ TBenchmarkCase }

constructor TBenchmarkCase.Create(const AName: string; const ARunFunction: TGocciaFunctionBase;
  const ASuiteName: string; const ASetupFunction: TGocciaFunctionBase;
  const ATeardownFunction: TGocciaFunctionBase);
begin
  inherited Create;
  Name := AName;
  RunFunction := ARunFunction;
  SuiteName := ASuiteName;
  SetupFunction := ASetupFunction;
  TeardownFunction := ATeardownFunction;
  OwnsRunRoot := False;
  OwnsSetupRoot := False;
  OwnsTeardownRoot := False;
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
  UnrootRegisteredBenchmarks;
  FRegisteredSuites.Free;
  FRegisteredBenchmarks.Free;
  inherited;
end;

procedure TGocciaBenchmark.UnrootRegisteredBenchmarks;
var
  I: Integer;
  GC: TGarbageCollector;
  BenchCase: TBenchmarkCase;
begin
  GC := TGarbageCollector.Instance;
  if not Assigned(GC) then
    Exit;

  for I := 0 to FRegisteredBenchmarks.Count - 1 do
  begin
    BenchCase := FRegisteredBenchmarks[I];
    if BenchCase.OwnsRunRoot then
      GC.RemoveTempRoot(BenchCase.RunFunction);
    if BenchCase.OwnsSetupRoot then
      GC.RemoveTempRoot(BenchCase.SetupFunction);
    if BenchCase.OwnsTeardownRoot then
      GC.RemoveTempRoot(BenchCase.TeardownFunction);
    BenchCase.OwnsRunRoot := False;
    BenchCase.OwnsSetupRoot := False;
    BenchCase.OwnsTeardownRoot := False;
  end;
end;

function TGocciaBenchmark.Suite(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  SuiteName: string;
  SuiteFunction: TGocciaFunctionBase;
  EmptyArgs: TGocciaArgumentsCollection;
  PreviousSuiteName: string;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  if AArgs.Length < 2 then Exit;
  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then Exit;
  if not (AArgs.GetElement(1) is TGocciaFunctionBase) then Exit;

  SuiteName := TGocciaStringLiteralValue(AArgs.GetElement(0)).Value;
  SuiteFunction := TGocciaFunctionBase(AArgs.GetElement(1));

  FRegisteredSuites.Add(SuiteName);

  PreviousSuiteName := FCurrentSuiteName;
  FCurrentSuiteName := SuiteName;
  try
    SuiteFunction.CallNoArgs(TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    FCurrentSuiteName := PreviousSuiteName;
  end;
end;

function TGocciaBenchmark.Bench(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  BenchName: string;
  OptionsObj: TGocciaObjectValue;
  RunProp, SetupProp, TeardownProp: TGocciaValue;
  RunFn, SetupFn, TeardownFn: TGocciaFunctionBase;
  BenchCase: TBenchmarkCase;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  if AArgs.Length < 2 then Exit;
  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then Exit;
  if not (AArgs.GetElement(1) is TGocciaObjectValue) then Exit;

  BenchName := TGocciaStringLiteralValue(AArgs.GetElement(0)).Value;
  OptionsObj := TGocciaObjectValue(AArgs.GetElement(1));

  RunProp := OptionsObj.GetProperty('run');
  if not Assigned(RunProp) or not (RunProp is TGocciaFunctionBase) then Exit;
  RunFn := TGocciaFunctionBase(RunProp);

  SetupFn := nil;
  SetupProp := OptionsObj.GetProperty('setup');
  if Assigned(SetupProp) and (SetupProp is TGocciaFunctionBase) then
    SetupFn := TGocciaFunctionBase(SetupProp);

  TeardownFn := nil;
  TeardownProp := OptionsObj.GetProperty('teardown');
  if Assigned(TeardownProp) and (TeardownProp is TGocciaFunctionBase) then
    TeardownFn := TGocciaFunctionBase(TeardownProp);

  BenchCase := TBenchmarkCase.Create(BenchName, RunFn, FCurrentSuiteName, SetupFn, TeardownFn);
  if Assigned(TGarbageCollector.Instance) then
  begin
    if not TGarbageCollector.Instance.IsTempRoot(RunFn) then
    begin
      TGarbageCollector.Instance.AddTempRoot(RunFn);
      BenchCase.OwnsRunRoot := True;
    end;
    if Assigned(SetupFn) and not TGarbageCollector.Instance.IsTempRoot(SetupFn) then
    begin
      TGarbageCollector.Instance.AddTempRoot(SetupFn);
      BenchCase.OwnsSetupRoot := True;
    end;
    if Assigned(TeardownFn) and not TGarbageCollector.Instance.IsTempRoot(TeardownFn) then
    begin
      TGarbageCollector.Instance.AddTempRoot(TeardownFn);
      BenchCase.OwnsTeardownRoot := True;
    end;
  end;

  FRegisteredBenchmarks.Add(BenchCase);
end;

function TGocciaBenchmark.CalibrateIterations(const ABenchCase: TBenchmarkCase;
  const ASetupResult: TGocciaValue; const ARunArgs: TGocciaArgumentsCollection): Int64;
var
  BatchSize: Int64;
  StartNanoseconds, ElapsedNanoseconds, TargetNanoseconds: Int64;
  I: Int64;
begin
  TargetNanoseconds := Int64(MIN_CALIBRATION_MS) * 1000000;
  BatchSize := CALIBRATION_BATCH;

  while True do
  begin
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.CollectIfNeeded(ABenchCase.RunFunction);
    StartNanoseconds := GetNanoseconds;
    I := 0;
    while I < BatchSize do
    begin
      InvokeBenchmarkFunction(ABenchCase.RunFunction, ASetupResult, ARunArgs);
      Inc(I);
    end;
    WaitForFetchIdle;
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

procedure InsertionSort(var A: array of Double; const ACount: Integer);
var
  K, J: Integer;
  Temp: Double;
begin
  for K := 1 to ACount - 1 do
  begin
    Temp := A[K];
    J := K - 1;
    while (J >= 0) and (A[J] > Temp) do
    begin
      A[J + 1] := A[J];
      Dec(J);
    end;
    A[J + 1] := Temp;
  end;
end;

procedure FilterOutliersIQR(const ASorted: array of Double; const ACount: Integer;
  out AFilteredStart, AFilteredEnd: Integer);
var
  Q1Index, Q3Index: Integer;
  Q1, Q3, IQR, LowerFence, UpperFence: Double;
  K: Integer;
begin
  AFilteredStart := 0;
  AFilteredEnd := ACount - 1;

  if ACount < 4 then
    Exit;

  Q1Index := ACount div 4;
  Q3Index := (ACount * 3) div 4;
  Q1 := ASorted[Q1Index];
  Q3 := ASorted[Q3Index];
  IQR := Q3 - Q1;

  if IQR <= 0 then
    Exit;

  LowerFence := Q1 - IQR_MULTIPLIER * IQR;
  UpperFence := Q3 + IQR_MULTIPLIER * IQR;

  for K := 0 to ACount - 1 do
    if ASorted[K] >= LowerFence then
    begin
      AFilteredStart := K;
      Break;
    end;

  for K := ACount - 1 downto 0 do
    if ASorted[K] <= UpperFence then
    begin
      AFilteredEnd := K;
      Break;
    end;
end;

function TGocciaBenchmark.RunSingleBenchmark(const ABenchCase: TBenchmarkCase): TBenchmarkResult;
var
  SetupResult: TGocciaValue;
  Iterations: Int64;
  StartNanoseconds, RoundNanoseconds: Int64;
  I: Int64;
  Round, K: Integer;
  OpsRounds, MeanRounds: array of Double;
  OpsMean, OpsVariance: Double;
  FilteredStart, FilteredEnd, FilteredCount: Integer;
  GC: TGarbageCollector;
  WasGCEnabled: Boolean;
  MeasurementWatermark: Integer;
  RunArgs: TGocciaArgumentsCollection;
begin
  Result.Name := ABenchCase.Name;
  Result.SuiteName := ABenchCase.SuiteName;
  Result.SetupMs := 0;
  Result.TeardownMs := 0;
  Result.MinOpsPerSec := 0;
  Result.MaxOpsPerSec := 0;

  SetLength(OpsRounds, MEASUREMENT_ROUNDS);
  SetLength(MeanRounds, MEASUREMENT_ROUNDS);

  GC := TGarbageCollector.Instance;
  WasGCEnabled := False;
  SetupResult := nil;
  RunArgs := TGocciaArgumentsCollection.CreateWithCapacity(1);
  try
    if Assigned(ABenchCase.SetupFunction) then
    begin
      StartNanoseconds := GetNanoseconds;
      SetupResult := ABenchCase.SetupFunction.CallNoArgs(
        TGocciaUndefinedLiteralValue.UndefinedValue);
      WaitForFetchIdle;
      Result.SetupMs := (GetNanoseconds - StartNanoseconds) / 1000000;

      if Assigned(SetupResult) and Assigned(GC) then
        GC.AddTempRoot(SetupResult);
    end;

    try
      for K := 1 to WARMUP_ITERATIONS do
        InvokeBenchmarkFunction(ABenchCase.RunFunction, SetupResult, RunArgs);
      WaitForFetchIdle;

      Iterations := CalibrateIterations(ABenchCase, SetupResult, RunArgs);

      if Assigned(GC) then
      begin
        WasGCEnabled := GC.Enabled;
        if Assigned(FOnBeforeMeasurement) then
          FOnBeforeMeasurement();
        {$IFDEF GC_DEBUG}
        WriteLn(Format('[BENCH] %s > %s: pre-measurement GC.Enabled=%s, objects=%d, allocs=%d',
          [ABenchCase.SuiteName, ABenchCase.Name,
           BoolToStr(GC.Enabled, 'True', 'False'), GC.ManagedObjectCount, GC.Watermark]));
        {$ENDIF}
        GC.Collect;
        GC.Enabled := False;
        MeasurementWatermark := GC.Watermark;
        {$IFDEF GC_DEBUG}
        WriteLn(Format('[BENCH] %s > %s: post-collect watermark=%d, objects=%d, iterations=%d',
          [ABenchCase.SuiteName, ABenchCase.Name,
           MeasurementWatermark, GC.ManagedObjectCount, Iterations]));
        {$ENDIF}
      end;

      for Round := 0 to MEASUREMENT_ROUNDS - 1 do
      begin
        StartNanoseconds := GetNanoseconds;
        I := 0;
        while I < Iterations do
        begin
          InvokeBenchmarkFunction(ABenchCase.RunFunction, SetupResult, RunArgs);
          Inc(I);
        end;
        WaitForFetchIdle;
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

        if Assigned(GC) and (Round < MEASUREMENT_ROUNDS - 1)
          and (GC.Watermark - MeasurementWatermark > GC.Threshold) then
        begin
          if Assigned(FOnBeforeMeasurement) then
            FOnBeforeMeasurement();
          GC.CollectYoung(MeasurementWatermark);
          {$IFDEF GC_DEBUG}
          WriteLn(Format('[BENCH] %s > %s: round %d collected, objects=%d',
            [ABenchCase.SuiteName, ABenchCase.Name,
             Round, GC.ManagedObjectCount]));
          {$ENDIF}
        end;
      end;

      {$IFDEF GC_DEBUG}
      if Assigned(GC) then
        WriteLn(Format('[BENCH] %s > %s: measurement done, objects=%d, totalCollected=%d, totalCollections=%d',
          [ABenchCase.SuiteName, ABenchCase.Name,
           GC.ManagedObjectCount, GC.TotalCollected, GC.TotalCollections]));
      {$ENDIF}

      if Assigned(GC) then
        GC.Enabled := WasGCEnabled;

      InsertionSort(OpsRounds, MEASUREMENT_ROUNDS);
      InsertionSort(MeanRounds, MEASUREMENT_ROUNDS);

      FilterOutliersIQR(OpsRounds, MEASUREMENT_ROUNDS, FilteredStart, FilteredEnd);
      FilteredCount := FilteredEnd - FilteredStart + 1;

      OpsMean := 0;
      for K := FilteredStart to FilteredEnd do
        OpsMean := OpsMean + OpsRounds[K];
      OpsMean := OpsMean / FilteredCount;

      OpsVariance := 0;
      for K := FilteredStart to FilteredEnd do
        OpsVariance := OpsVariance + Sqr(OpsRounds[K] - OpsMean);
      OpsVariance := OpsVariance / FilteredCount;

      if OpsMean > 0 then
        Result.VariancePercentage := (Sqrt(OpsVariance) / OpsMean) * 100
      else
        Result.VariancePercentage := 0;

      Result.Iterations := Iterations;
      Result.TotalMs := 0;
      Result.OpsPerSec := OpsRounds[FilteredStart + (FilteredCount div 2)];
      Result.MeanMs := MeanRounds[FilteredStart + (FilteredCount div 2)];
      Result.MinOpsPerSec := OpsRounds[FilteredStart];
      Result.MaxOpsPerSec := OpsRounds[FilteredEnd];

      if Assigned(ABenchCase.TeardownFunction) then
      begin
        StartNanoseconds := GetNanoseconds;
        InvokeBenchmarkFunction(ABenchCase.TeardownFunction, SetupResult, RunArgs);
        WaitForFetchIdle;
        Result.TeardownMs := (GetNanoseconds - StartNanoseconds) / 1000000;
      end;
    finally
      if Assigned(GC) then
        GC.Enabled := WasGCEnabled;
      if Assigned(SetupResult) and Assigned(GC) then
        GC.RemoveTempRoot(SetupResult);
    end;
  finally
    RunArgs.Free;
  end;
end;

function TGocciaBenchmark.RunSingleBenchmarkDeterministic(
  const ABenchCase: TBenchmarkCase): TBenchmarkResult;
var
  SetupResult: TGocciaValue;
  RunArgs: TGocciaArgumentsCollection;
  GC: TGarbageCollector;
begin
  Result.Name := ABenchCase.Name;
  Result.SuiteName := ABenchCase.SuiteName;
  Result.Iterations := 1;
  Result.TotalMs := 0;
  Result.OpsPerSec := 0;
  Result.MeanMs := 0;
  Result.VariancePercentage := 0;
  Result.SetupMs := 0;
  Result.TeardownMs := 0;
  Result.MinOpsPerSec := 0;
  Result.MaxOpsPerSec := 0;

  GC := TGarbageCollector.Instance;
  SetupResult := nil;
  RunArgs := TGocciaArgumentsCollection.CreateWithCapacity(1);
  try
    if Assigned(ABenchCase.SetupFunction) then
    begin
      SetupResult := ABenchCase.SetupFunction.CallNoArgs(
        TGocciaUndefinedLiteralValue.UndefinedValue);
      WaitForFetchIdle;
      if Assigned(SetupResult) and Assigned(GC) then
        GC.AddTempRoot(SetupResult);
    end;

    try
      InvokeBenchmarkFunction(ABenchCase.RunFunction, SetupResult, RunArgs);
      WaitForFetchIdle;

      if Assigned(ABenchCase.TeardownFunction) then
      begin
        InvokeBenchmarkFunction(ABenchCase.TeardownFunction, SetupResult, RunArgs);
        WaitForFetchIdle;
      end;
    finally
      if Assigned(SetupResult) and Assigned(GC) then
        GC.RemoveTempRoot(SetupResult);
    end;
  finally
    RunArgs.Free;
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

  ResultsArray := nil;
  try
    ResultsArray := TGocciaArrayValue.Create;
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AddTempRoot(ResultsArray);

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
        SingleResult.AssignProperty('minOpsPerSec', TGocciaNumberLiteralValue.Create(BenchResult.MinOpsPerSec));
        SingleResult.AssignProperty('maxOpsPerSec', TGocciaNumberLiteralValue.Create(BenchResult.MaxOpsPerSec));

        ResultsArray.SetElement(ResultsArray.GetLength, SingleResult);
      except
        on E: Exception do
        begin
          if Assigned(TGocciaMicrotaskQueue.Instance) then
            TGocciaMicrotaskQueue.Instance.ClearQueue;
          DiscardFetchCompletions;

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
    if Assigned(TGarbageCollector.Instance) then
    begin
      if Assigned(ResultsArray) then
        TGarbageCollector.Instance.RemoveTempRoot(ResultsArray);
    end;
  end;
end;

function TGocciaBenchmark.RunDeterministicProfile(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
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

  ResultsArray := nil;
  try
    ResultsArray := TGocciaArrayValue.Create;
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AddTempRoot(ResultsArray);

    for I := 0 to FRegisteredBenchmarks.Count - 1 do
    begin
      BenchCase := FRegisteredBenchmarks[I];

      if Assigned(FOnProgress) then
        FOnProgress(BenchCase.SuiteName, BenchCase.Name, I + 1, FRegisteredBenchmarks.Count);

      try
        BenchResult := RunSingleBenchmarkDeterministic(BenchCase);

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
        SingleResult.AssignProperty('minOpsPerSec', TGocciaNumberLiteralValue.Create(BenchResult.MinOpsPerSec));
        SingleResult.AssignProperty('maxOpsPerSec', TGocciaNumberLiteralValue.Create(BenchResult.MaxOpsPerSec));

        ResultsArray.SetElement(ResultsArray.GetLength, SingleResult);
      except
        on E: Exception do
        begin
          if Assigned(TGocciaMicrotaskQueue.Instance) then
            TGocciaMicrotaskQueue.Instance.ClearQueue;
          DiscardFetchCompletions;

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
    ResultObj.AssignProperty('durationNanoseconds', TGocciaNumberLiteralValue.Create(TotalDurationNanoseconds));
    ResultObj.AssignProperty('results', ResultsArray);
    Result := ResultObj;
  finally
    if Assigned(TGarbageCollector.Instance) and Assigned(ResultsArray) then
      TGarbageCollector.Instance.RemoveTempRoot(ResultsArray);
  end;
end;

initialization
  InitBenchmarkConfig;

end.
