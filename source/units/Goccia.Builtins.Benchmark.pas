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
  Goccia.GarbageCollector,
  Goccia.Modules,
  Goccia.Scope,
  Goccia.Values.FunctionBase,
  Goccia.Values.IteratorValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaBenchmark = class;

  TBenchmarkProgressEvent = procedure(const ASuiteName, ABenchName: string; const AIndex, ATotal: Integer) of object;
  TBenchmarkNotifyEvent = procedure of object;

  TBenchmarkRunMode = (brmStandard, brmDeterministicProfile);

  TBenchmarkCase = class
  public
    Name: string;
    SuiteName: string;
    RunFunction: TGocciaFunctionBase;
    GeneratorFunction: TGocciaFunctionBase;
    OwnsRunRoot: Boolean;
    OwnsGeneratorRoot: Boolean;
    constructor Create(const AName: string; const ARunFunction: TGocciaFunctionBase;
      const ASuiteName: string; const AGeneratorFunction: TGocciaFunctionBase = nil);
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
    FNamespaceObject: TGocciaObjectValue;
    FOwnsNamespaceRoot: Boolean;
    FHasCompletedRun: Boolean;
    FLastRunResult: TGocciaObjectValue;
    FLastRunMode: TBenchmarkRunMode;
    FOwnsLastRunRoot: Boolean;
    FOnProgress: TBenchmarkProgressEvent;
    FOnBeforeMeasurement: TBenchmarkNotifyEvent;

    function CreateNamespaceObject: TGocciaObjectValue;
    function CreateRunFunctionFromGenerator(
      const ABenchCase: TBenchmarkCase;
      var AActiveRoots: TGocciaActiveRootFrame;
      out AGeneratorIterator: TGocciaIteratorValue): TGocciaFunctionBase;
    function ExecuteWrapper(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function IsGeneratorBenchmarkFunction(const AFunction: TGocciaFunctionBase): Boolean;
    procedure StoreLastRunResult(const AResult: TGocciaObjectValue;
      const AMode: TBenchmarkRunMode);
    procedure ClearLastRunResult;
    function RunSingleBenchmark(const ABenchCase: TBenchmarkCase): TBenchmarkResult;
    function RunSingleBenchmarkDeterministic(const ABenchCase: TBenchmarkCase): TBenchmarkResult;
    function CalibrateIterations(const ARunFunction: TGocciaFunctionBase;
      const ASetupResult: TGocciaValue;
      const ARunArgs: TGocciaArgumentsCollection): Int64;
    procedure UnrootRegisteredBenchmarks;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
    destructor Destroy; override;

    function CreateModule: TGocciaModule;
    function Group(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function Bench(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function Run(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RunBenchmarks(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RunDeterministicProfile(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function RunForHost(const ADeterministicProfile: Boolean): TGocciaValue;
    function Summary(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function Boxplot(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure ClearRegisteredBenchmarks;

    property OnProgress: TBenchmarkProgressEvent read FOnProgress write FOnProgress;
    property OnBeforeMeasurement: TBenchmarkNotifyEvent read FOnBeforeMeasurement write FOnBeforeMeasurement;
  end;

implementation

uses
  Math,
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Error.Detail,
  Goccia.FetchManager,
  Goccia.MicrotaskQueue,
  Goccia.Values.ArrayValue,
  Goccia.Values.Await,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.GeneratorValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.NativeFunctionCallback,
  Goccia.Values.PromiseValue,
  Goccia.VM.Exception;

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

function BenchmarkExceptionMessage(const AException: Exception): string;
begin
  if AException is TGocciaThrowValue then
    Exit(FormatThrowDetail(TGocciaThrowValue(AException).Value, '', nil, False,
      TGocciaThrowValue(AException).Suggestion));
  if AException is EGocciaBytecodeThrow then
    Exit(FormatThrowDetail(EGocciaBytecodeThrow(AException).ThrownValue, '',
      nil, False));
  Result := AException.Message;
end;

{ TBenchmarkCase }

constructor TBenchmarkCase.Create(const AName: string;
  const ARunFunction: TGocciaFunctionBase; const ASuiteName: string;
  const AGeneratorFunction: TGocciaFunctionBase);
begin
  inherited Create;
  Name := AName;
  RunFunction := ARunFunction;
  SuiteName := ASuiteName;
  GeneratorFunction := AGeneratorFunction;
  OwnsRunRoot := False;
  OwnsGeneratorRoot := False;
end;

{ TGocciaBenchmark }

constructor TGocciaBenchmark.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FRegisteredSuites := TStringList.Create;
  FRegisteredBenchmarks := TObjectList<TBenchmarkCase>.Create;
  FCurrentSuiteName := '';
  FNamespaceObject := CreateNamespaceObject;
  FOwnsNamespaceRoot := False;
  FHasCompletedRun := False;
  FLastRunResult := nil;
  FLastRunMode := brmStandard;
  FOwnsLastRunRoot := False;

  if Assigned(TGarbageCollector.Instance) then
  begin
    TGarbageCollector.Instance.AddRootObject(FNamespaceObject);
    FOwnsNamespaceRoot := True;
  end;
end;

function AddBenchmarkFunction(const AObject: TGocciaObjectValue;
  const AName: string; const ACallback: TGocciaNativeFunctionCallback;
  const AArity: Integer): TGocciaNativeFunctionValue;
begin
  Result := TGocciaNativeFunctionValue.CreateWithoutPrototype(ACallback,
    AName, AArity);
  AObject.SetProperty(AName, Result);
end;

function TGocciaBenchmark.CreateNamespaceObject: TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype,
    8);
  AddBenchmarkFunction(Result, 'bench', Bench, 2);
  AddBenchmarkFunction(Result, 'group', Group, 2);
  AddBenchmarkFunction(Result, 'run', Run, 0);
  AddBenchmarkFunction(Result, 'summary', Summary, 1);
  AddBenchmarkFunction(Result, 'boxplot', Boxplot, 1);
end;

function TGocciaBenchmark.CreateModule: TGocciaModule;
begin
  Result := TGocciaModule.Create('goccia:microbench');
  Result.AddExportValue('bench', FNamespaceObject.GetProperty('bench'));
  Result.AddExportValue('group', FNamespaceObject.GetProperty('group'));
  Result.AddExportValue('run', FNamespaceObject.GetProperty('run'));
  Result.AddExportValue('summary', FNamespaceObject.GetProperty('summary'));
  Result.AddExportValue('boxplot', FNamespaceObject.GetProperty('boxplot'));
  Result.AddExportValue('default', FNamespaceObject);
end;

destructor TGocciaBenchmark.Destroy;
begin
  ClearLastRunResult;
  UnrootRegisteredBenchmarks;
  if FOwnsNamespaceRoot and Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.RemoveRootObject(FNamespaceObject);
  if not Assigned(TGarbageCollector.Instance) then
    FNamespaceObject.Free;
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
    if BenchCase.OwnsGeneratorRoot then
      GC.RemoveTempRoot(BenchCase.GeneratorFunction);
    BenchCase.OwnsRunRoot := False;
    BenchCase.OwnsGeneratorRoot := False;
  end;
end;

procedure TGocciaBenchmark.ClearLastRunResult;
begin
  if FOwnsLastRunRoot and Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.RemoveRootObject(FLastRunResult);
  FOwnsLastRunRoot := False;
  FHasCompletedRun := False;
  FLastRunResult := nil;
  FLastRunMode := brmStandard;
end;

procedure TGocciaBenchmark.StoreLastRunResult(const AResult: TGocciaObjectValue;
  const AMode: TBenchmarkRunMode);
begin
  ClearLastRunResult;
  FLastRunResult := AResult;
  FHasCompletedRun := Assigned(AResult);
  FLastRunMode := AMode;
  if FHasCompletedRun and Assigned(TGarbageCollector.Instance) then
  begin
    TGarbageCollector.Instance.AddRootObject(AResult);
    FOwnsLastRunRoot := True;
  end;
end;

procedure TGocciaBenchmark.ClearRegisteredBenchmarks;
begin
  ClearLastRunResult;
  UnrootRegisteredBenchmarks;
  FRegisteredBenchmarks.Clear;
  FRegisteredSuites.Clear;
  FCurrentSuiteName := '';
end;

function TGocciaBenchmark.IsGeneratorBenchmarkFunction(
  const AFunction: TGocciaFunctionBase): Boolean;
var
  ConstructorValue: TGocciaValue;
  ConstructorName: TGocciaValue;
begin
  if (AFunction is TGocciaGeneratorFunctionValue) or
     (AFunction is TGocciaGeneratorMethodValue) then
    Exit(True);

  ConstructorValue := AFunction.GetProperty(PROP_CONSTRUCTOR);
  if ConstructorValue is TGocciaObjectValue then
  begin
    ConstructorName := TGocciaObjectValue(ConstructorValue).GetProperty(PROP_NAME);
    Result := (ConstructorName is TGocciaStringLiteralValue) and
      (TGocciaStringLiteralValue(ConstructorName).Value = 'GeneratorFunction');
    Exit;
  end;

  Result := False;
end;

function TGocciaBenchmark.CreateRunFunctionFromGenerator(
  const ABenchCase: TBenchmarkCase; var AActiveRoots: TGocciaActiveRootFrame;
  out AGeneratorIterator: TGocciaIteratorValue): TGocciaFunctionBase;
var
  Done: Boolean;
  GeneratorValue: TGocciaValue;
  YieldedValue: TGocciaValue;
begin
  AGeneratorIterator := nil;
  GeneratorValue := ABenchCase.GeneratorFunction.CallNoArgs(
    TGocciaUndefinedLiteralValue.UndefinedValue);
  if not (GeneratorValue is TGocciaIteratorValue) then
    ThrowTypeError('bench generator callback must return a sync iterator');

  AGeneratorIterator := TGocciaIteratorValue(GeneratorValue);
  AActiveRoots.Add(AGeneratorIterator);
  YieldedValue := AGeneratorIterator.DirectNext(Done);
  WaitForFetchIdle;

  if Done or not (YieldedValue is TGocciaFunctionBase) then
    ThrowTypeError('bench generator callback must yield a function');

  Result := TGocciaFunctionBase(YieldedValue);
  AActiveRoots.Add(Result);
end;

function TGocciaBenchmark.Group(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  SuiteName: string;
  SuiteFunction: TGocciaFunctionBase;
  PreviousSuiteName: string;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  if (AArgs.Length < 2) or
     not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowTypeError('group requires a name string and callback');
  if not (AArgs.GetElement(1) is TGocciaFunctionBase) then
    ThrowTypeError('group requires a callback function');

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

function TGocciaBenchmark.ExecuteWrapper(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  WrapperFunction: TGocciaFunctionBase;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if (AArgs.Length < 1) or not (AArgs.GetElement(0) is TGocciaFunctionBase) then
    ThrowTypeError('benchmark wrapper requires a callback function');

  WrapperFunction := TGocciaFunctionBase(AArgs.GetElement(0));
  WrapperFunction.CallNoArgs(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

function TGocciaBenchmark.Summary(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := ExecuteWrapper(AArgs, AThisValue);
end;

function TGocciaBenchmark.Boxplot(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := ExecuteWrapper(AArgs, AThisValue);
end;

function TGocciaBenchmark.Run(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RunBenchmarks(AArgs, AThisValue);
end;

function TGocciaBenchmark.Bench(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  BenchName: string;
  CallbackFn: TGocciaFunctionBase;
  RunFn, GeneratorFn: TGocciaFunctionBase;
  BenchCase: TBenchmarkCase;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  if (AArgs.Length < 2) or
     not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowTypeError('bench requires a name string and callback');
  if not (AArgs.GetElement(1) is TGocciaFunctionBase) then
    ThrowTypeError('bench requires a callback function');

  BenchName := TGocciaStringLiteralValue(AArgs.GetElement(0)).Value;
  CallbackFn := TGocciaFunctionBase(AArgs.GetElement(1));

  if IsGeneratorBenchmarkFunction(CallbackFn) then
  begin
    RunFn := nil;
    GeneratorFn := CallbackFn;
  end
  else
  begin
    RunFn := CallbackFn;
    GeneratorFn := nil;
  end;

  BenchCase := TBenchmarkCase.Create(BenchName, RunFn, FCurrentSuiteName, GeneratorFn);
  if Assigned(TGarbageCollector.Instance) then
  begin
    if Assigned(RunFn) and not TGarbageCollector.Instance.IsTempRoot(RunFn) then
    begin
      TGarbageCollector.Instance.AddTempRoot(RunFn);
      BenchCase.OwnsRunRoot := True;
    end;
    if Assigned(GeneratorFn) and not TGarbageCollector.Instance.IsTempRoot(GeneratorFn) then
    begin
      TGarbageCollector.Instance.AddTempRoot(GeneratorFn);
      BenchCase.OwnsGeneratorRoot := True;
    end;
  end;

  FRegisteredBenchmarks.Add(BenchCase);
end;

function TGocciaBenchmark.CalibrateIterations(const ARunFunction: TGocciaFunctionBase;
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
      TGarbageCollector.Instance.CollectIfNeeded(ARunFunction);
    StartNanoseconds := GetNanoseconds;
    I := 0;
    while I < BatchSize do
    begin
      InvokeBenchmarkFunction(ARunFunction, ASetupResult, ARunArgs);
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
  ActiveRoots: TGocciaActiveRootFrame;
  RunFunction: TGocciaFunctionBase;
  GeneratorIterator: TGocciaIteratorValue;
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
  RunFunction := ABenchCase.RunFunction;
  GeneratorIterator := nil;
  ActiveRoots.Initialize;
  ActiveRoots.Add(ABenchCase.RunFunction);
  ActiveRoots.Add(ABenchCase.GeneratorFunction);
  RunArgs := TGocciaArgumentsCollection.CreateWithCapacity(1);
  try
    try
      if Assigned(ABenchCase.GeneratorFunction) then
      begin
        StartNanoseconds := GetNanoseconds;
        RunFunction := CreateRunFunctionFromGenerator(ABenchCase, ActiveRoots,
          GeneratorIterator);
        Result.SetupMs := (GetNanoseconds - StartNanoseconds) / 1000000;
      end;

      if Assigned(GC) then
      begin
        WasGCEnabled := GC.Enabled;
        if Assigned(FOnBeforeMeasurement) then
          FOnBeforeMeasurement();
        GC.Collect;
        GC.Enabled := False;
      end;

      for K := 1 to WARMUP_ITERATIONS do
        InvokeBenchmarkFunction(RunFunction, SetupResult, RunArgs);
      WaitForFetchIdle;

      Iterations := CalibrateIterations(RunFunction, SetupResult, RunArgs);

      if Assigned(GC) then
      begin
        if Assigned(FOnBeforeMeasurement) then
          FOnBeforeMeasurement();
        {$IFDEF GC_DEBUG}
        WriteLn(Format('[BENCH] %s > %s: pre-measurement GC.Enabled=%s, objects=%d, allocs=%d',
          [ABenchCase.SuiteName, ABenchCase.Name,
           BoolToStr(GC.Enabled, 'True', 'False'), GC.ManagedObjectCount, GC.Watermark]));
        {$ENDIF}
        GC.Collect;
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
          InvokeBenchmarkFunction(RunFunction, SetupResult, RunArgs);
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
    finally
      if Assigned(GC) then
        GC.Enabled := WasGCEnabled;
      if Assigned(GeneratorIterator) then
      begin
        StartNanoseconds := GetNanoseconds;
        GeneratorIterator.Close;
        WaitForFetchIdle;
        Result.TeardownMs := (GetNanoseconds - StartNanoseconds) / 1000000;
        GeneratorIterator := nil;
      end;
    end;
  finally
    RunArgs.Free;
    ActiveRoots.Clear;
  end;
end;

function TGocciaBenchmark.RunSingleBenchmarkDeterministic(
  const ABenchCase: TBenchmarkCase): TBenchmarkResult;
var
  SetupResult: TGocciaValue;
  RunArgs: TGocciaArgumentsCollection;
  GC: TGarbageCollector;
  ActiveRoots: TGocciaActiveRootFrame;
  RunFunction: TGocciaFunctionBase;
  GeneratorIterator: TGocciaIteratorValue;
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
  RunFunction := ABenchCase.RunFunction;
  GeneratorIterator := nil;
  ActiveRoots.Initialize;
  ActiveRoots.Add(ABenchCase.RunFunction);
  ActiveRoots.Add(ABenchCase.GeneratorFunction);
  RunArgs := TGocciaArgumentsCollection.CreateWithCapacity(1);
  try
    if Assigned(ABenchCase.GeneratorFunction) then
      RunFunction := CreateRunFunctionFromGenerator(ABenchCase, ActiveRoots,
        GeneratorIterator);

    InvokeBenchmarkFunction(RunFunction, SetupResult, RunArgs);
    WaitForFetchIdle;

    if Assigned(GeneratorIterator) then
    begin
      GeneratorIterator.Close;
      WaitForFetchIdle;
      GeneratorIterator := nil;
    end;
  finally
    if Assigned(GeneratorIterator) then
      GeneratorIterator.Close;
    RunArgs.Free;
    ActiveRoots.Clear;
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
  GC: TGarbageCollector;
begin
  if FHasCompletedRun and Assigned(FLastRunResult) and
     (FLastRunMode = brmStandard) then
    Exit(FLastRunResult);

  StartNanoseconds := GetNanoseconds;
  GC := TGarbageCollector.Instance;

  ResultsArray := nil;
  try
    ResultsArray := TGocciaArrayValue.Create;
    if Assigned(GC) then
      GC.AddTempRoot(ResultsArray);

    for I := 0 to FRegisteredBenchmarks.Count - 1 do
    begin
      BenchCase := FRegisteredBenchmarks[I];

      if Assigned(FOnProgress) then
        FOnProgress(BenchCase.SuiteName, BenchCase.Name, I + 1, FRegisteredBenchmarks.Count);

      try
        BenchResult := RunSingleBenchmark(BenchCase);

        SingleResult := TGocciaObjectValue.Create;
        if Assigned(GC) then
          GC.AddTempRoot(SingleResult);
        try
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
        finally
          if Assigned(GC) then
            GC.RemoveTempRoot(SingleResult);
        end;
      except
        on E: Exception do
        begin
          if Assigned(TGocciaMicrotaskQueue.Instance) then
            TGocciaMicrotaskQueue.Instance.ClearQueue;
          DiscardFetchCompletions;

          SingleResult := TGocciaObjectValue.Create;
          if Assigned(GC) then
            GC.AddTempRoot(SingleResult);
          try
            SingleResult.AssignProperty('name', TGocciaStringLiteralValue.Create(BenchCase.Name));
            SingleResult.AssignProperty('suite', TGocciaStringLiteralValue.Create(BenchCase.SuiteName));
            SingleResult.AssignProperty('error',
              TGocciaStringLiteralValue.Create(BenchmarkExceptionMessage(E)));

            ResultsArray.SetElement(ResultsArray.GetLength, SingleResult);
          finally
            if Assigned(GC) then
              GC.RemoveTempRoot(SingleResult);
          end;
        end;
      end;

    end;

    TotalDurationNanoseconds := GetNanoseconds - StartNanoseconds;

    ResultObj := TGocciaObjectValue.Create;
    if Assigned(GC) then
      GC.AddTempRoot(ResultObj);
    try
      ResultObj.AssignProperty('totalBenchmarks', TGocciaNumberLiteralValue.Create(FRegisteredBenchmarks.Count));
      ResultObj.AssignProperty('results', ResultsArray);
      ResultObj.AssignProperty('durationNanoseconds', TGocciaNumberLiteralValue.Create(TotalDurationNanoseconds));

      StoreLastRunResult(ResultObj, brmStandard);
      Result := ResultObj;
    finally
      if Assigned(GC) then
        GC.RemoveTempRoot(ResultObj);
    end;
  finally
    if Assigned(GC) then
    begin
      if Assigned(ResultsArray) then
        GC.RemoveTempRoot(ResultsArray);
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
  GC: TGarbageCollector;
begin
  if FHasCompletedRun and Assigned(FLastRunResult) and
     (FLastRunMode = brmDeterministicProfile) then
    Exit(FLastRunResult);

  StartNanoseconds := GetNanoseconds;
  GC := TGarbageCollector.Instance;

  ResultsArray := nil;
  try
    ResultsArray := TGocciaArrayValue.Create;
    if Assigned(GC) then
      GC.AddTempRoot(ResultsArray);

    for I := 0 to FRegisteredBenchmarks.Count - 1 do
    begin
      BenchCase := FRegisteredBenchmarks[I];

      if Assigned(FOnProgress) then
        FOnProgress(BenchCase.SuiteName, BenchCase.Name, I + 1, FRegisteredBenchmarks.Count);

      try
        BenchResult := RunSingleBenchmarkDeterministic(BenchCase);

        SingleResult := TGocciaObjectValue.Create;
        if Assigned(GC) then
          GC.AddTempRoot(SingleResult);
        try
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
        finally
          if Assigned(GC) then
            GC.RemoveTempRoot(SingleResult);
        end;
      except
        on E: Exception do
        begin
          if Assigned(TGocciaMicrotaskQueue.Instance) then
            TGocciaMicrotaskQueue.Instance.ClearQueue;
          DiscardFetchCompletions;

          SingleResult := TGocciaObjectValue.Create;
          if Assigned(GC) then
            GC.AddTempRoot(SingleResult);
          try
            SingleResult.AssignProperty('name', TGocciaStringLiteralValue.Create(BenchCase.Name));
            SingleResult.AssignProperty('suite', TGocciaStringLiteralValue.Create(BenchCase.SuiteName));
            SingleResult.AssignProperty('error',
              TGocciaStringLiteralValue.Create(BenchmarkExceptionMessage(E)));
            ResultsArray.SetElement(ResultsArray.GetLength, SingleResult);
          finally
            if Assigned(GC) then
              GC.RemoveTempRoot(SingleResult);
          end;
        end;
      end;
    end;

    TotalDurationNanoseconds := GetNanoseconds - StartNanoseconds;
    ResultObj := TGocciaObjectValue.Create;
    if Assigned(GC) then
      GC.AddTempRoot(ResultObj);
    try
      ResultObj.AssignProperty('totalBenchmarks', TGocciaNumberLiteralValue.Create(FRegisteredBenchmarks.Count));
      ResultObj.AssignProperty('durationNanoseconds', TGocciaNumberLiteralValue.Create(TotalDurationNanoseconds));
      ResultObj.AssignProperty('results', ResultsArray);
      StoreLastRunResult(ResultObj, brmDeterministicProfile);
      Result := ResultObj;
    finally
      if Assigned(GC) then
        GC.RemoveTempRoot(ResultObj);
    end;
  finally
    if Assigned(GC) and Assigned(ResultsArray) then
      GC.RemoveTempRoot(ResultsArray);
  end;
end;

function TGocciaBenchmark.RunForHost(
  const ADeterministicProfile: Boolean): TGocciaValue;
var
  EmptyArgs: TGocciaArgumentsCollection;
  RequestedMode: TBenchmarkRunMode;
begin
  if ADeterministicProfile then
    RequestedMode := brmDeterministicProfile
  else
    RequestedMode := brmStandard;

  if FHasCompletedRun and Assigned(FLastRunResult) and
     (FLastRunMode = RequestedMode) then
    Exit(FLastRunResult);

  EmptyArgs := TGocciaArgumentsCollection.Create;
  try
    if ADeterministicProfile then
      Result := RunDeterministicProfile(EmptyArgs,
        TGocciaUndefinedLiteralValue.UndefinedValue)
    else
      Result := RunBenchmarks(EmptyArgs,
        TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    EmptyArgs.Free;
  end;
end;

initialization
  InitBenchmarkConfig;

end.
