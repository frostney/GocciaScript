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
  Goccia.Values.ClassHelper;

const
  WARMUP_ITERATIONS = 3;
  MIN_CALIBRATION_MS = 1000; // Run for at least 1 second during calibration
  CALIBRATION_BATCH = 10;    // Initial batch size for calibration

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
  ClonedFunction: TGocciaFunctionValue;
  BatchSize: Int64;
  StartTime, Elapsed: Int64;
  I: Int64;
begin
  // Start with a small batch and double until we run for at least MIN_CALIBRATION_MS
  BatchSize := CALIBRATION_BATCH;

  EmptyArgs := TGocciaArgumentsCollection.Create;
  try
    while True do
    begin
      StartTime := GetTickCount64;
      for I := 1 to BatchSize do
      begin
        ClonedFunction := BenchCase.BenchFunction.CloneWithNewScope(
          FScope.CreateChild(skFunction, 'Benchmark'));
        ClonedFunction.Call(EmptyArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
      end;
      Elapsed := GetTickCount64 - StartTime;

      if Elapsed >= MIN_CALIBRATION_MS then
      begin
        // We have enough data - return this batch size as the iteration count
        Result := BatchSize;
        Exit;
      end;

      // Double the batch size for next attempt
      if Elapsed = 0 then
        BatchSize := BatchSize * 10  // Very fast function, ramp up quickly
      else
        BatchSize := BatchSize * (MIN_CALIBRATION_MS div Elapsed + 1);

      // Safety cap
      if BatchSize > 10000000 then
      begin
        Result := BatchSize;
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
  ClonedFunction: TGocciaFunctionValue;
  Iterations: Int64;
  StartTime, TotalMs: Int64;
  I: Int64;
begin
  Result.Name := BenchCase.Name;
  Result.SuiteName := BenchCase.SuiteName;

  EmptyArgs := TGocciaArgumentsCollection.Create;
  try
    // Phase 1: Warmup
    for I := 1 to WARMUP_ITERATIONS do
    begin
      ClonedFunction := BenchCase.BenchFunction.CloneWithNewScope(
        FScope.CreateChild(skFunction, 'Benchmark'));
      ClonedFunction.Call(EmptyArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    end;

    // Phase 2: Calibrate to find iteration count
    Iterations := CalibrateIterations(BenchCase);

    // Phase 3: Measure
    StartTime := GetTickCount64;
    for I := 1 to Iterations do
    begin
      ClonedFunction := BenchCase.BenchFunction.CloneWithNewScope(
        FScope.CreateChild(skFunction, 'Benchmark'));
      ClonedFunction.Call(EmptyArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    end;
    TotalMs := GetTickCount64 - StartTime;

    Result.Iterations := Iterations;
    Result.TotalMs := TotalMs;
    if TotalMs > 0 then
    begin
      Result.OpsPerSec := (Iterations / TotalMs) * 1000;
      Result.MeanMs := TotalMs / Iterations;
    end
    else
    begin
      Result.OpsPerSec := 0;
      Result.MeanMs := 0;
    end;
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

  ResultsArray := TGocciaArrayValue.Create;

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
end;

end.
