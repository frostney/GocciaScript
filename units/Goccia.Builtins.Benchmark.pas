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
  SysUtils;

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
  Goccia.Values.ClassHelper, Goccia.GC;

const
  WARMUP_ITERATIONS = 3;
  MIN_CALIBRATION_MS = 1000;   // Run for at least 1s during calibration
  CALIBRATION_BATCH = 10;      // Initial batch size for calibration
  MEASUREMENT_ROUNDS = 3;      // Number of measurement rounds (take median)

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

function TGocciaBenchmark.CalibrateIterations(BenchCase: TBenchmarkCase): Int64;
var
  EmptyArgs: TGocciaArgumentsCollection;
  BatchSize: Int64;
  StartTime, Elapsed: Int64;
  I: Int64;
begin
  // Start with a small batch and scale up until we run for at least MIN_CALIBRATION_MS
  BatchSize := CALIBRATION_BATCH;

  EmptyArgs := TGocciaArgumentsCollection.Create;
  try
    while True do
    begin
      StartTime := GetTickCount64;
      for I := 1 to BatchSize do
        BenchCase.BenchFunction.Call(EmptyArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
      Elapsed := GetTickCount64 - StartTime;

      if Elapsed >= MIN_CALIBRATION_MS then
      begin
        // Round to a clean number for reproducibility
        Result := (BatchSize div 10) * 10;
        if Result < 10 then Result := 10;
        Exit;
      end;

      // Scale up based on how far we are from the target
      if Elapsed = 0 then
        BatchSize := BatchSize * 10
      else
        BatchSize := BatchSize * (MIN_CALIBRATION_MS div Elapsed + 1);

      // Safety cap
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
  StartTime, RoundMs: Int64;
  I: Int64;
  Round, J: Integer;
  OpsRounds: array[0..4] of Double;   // Max 5 rounds
  MeanRounds: array[0..4] of Double;  // Max 5 rounds
  TempDouble: Double;
begin
  Result.Name := BenchCase.Name;
  Result.SuiteName := BenchCase.SuiteName;

  EmptyArgs := TGocciaArgumentsCollection.Create;
  try
    // Phase 1: Warmup
    for I := 1 to WARMUP_ITERATIONS do
      BenchCase.BenchFunction.Call(EmptyArgs, TGocciaUndefinedLiteralValue.UndefinedValue);

    // Phase 2: Calibrate to find iteration count per round (~1s each)
    Iterations := CalibrateIterations(BenchCase);

    // Phase 3: Multiple measurement rounds, each running the full iteration count
    for Round := 0 to MEASUREMENT_ROUNDS - 1 do
    begin
      // Run GC before each measurement round to start with a clean heap
      if Assigned(TGocciaGC.Instance) then
        TGocciaGC.Instance.Collect;

      StartTime := GetTickCount64;
      for I := 1 to Iterations do
        BenchCase.BenchFunction.Call(EmptyArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
      RoundMs := GetTickCount64 - StartTime;

      if RoundMs > 0 then
      begin
        OpsRounds[Round] := (Iterations / RoundMs) * 1000;
        MeanRounds[Round] := RoundMs / Iterations;
      end
      else
      begin
        OpsRounds[Round] := 0;
        MeanRounds[Round] := 0;
      end;
    end;

    // Sort rounds to find median (insertion sort for small N)
    for I := 1 to MEASUREMENT_ROUNDS - 1 do
    begin
      TempDouble := OpsRounds[I];
      J := I - 1;
      while (J >= 0) and (OpsRounds[J] > TempDouble) do
      begin
        OpsRounds[J + 1] := OpsRounds[J];
        Dec(J);
      end;
      OpsRounds[J + 1] := TempDouble;
    end;
    for I := 1 to MEASUREMENT_ROUNDS - 1 do
    begin
      TempDouble := MeanRounds[I];
      J := I - 1;
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
  StartTime, TotalDuration: Int64;
begin
  StartTime := GetTickCount64;

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

    TotalDuration := GetTickCount64 - StartTime;

    ResultObj := TGocciaObjectValue.Create;
    ResultObj.AssignProperty('totalBenchmarks', TGocciaNumberLiteralValue.Create(FRegisteredBenchmarks.Count));
    ResultObj.AssignProperty('results', ResultsArray);
    ResultObj.AssignProperty('duration', TGocciaNumberLiteralValue.Create(TotalDuration));

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

end.
