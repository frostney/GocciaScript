program GocciaWasmTestRunner;

{ Minimal single-threaded test runner for constrained hosts (WASI).

  GocciaTestRunner is the canonical suite driver, but its application
  layer (worker pool, JSON reporter, coverage, stdin multifile) leans
  on threads and process facilities that do not exist in the
  single-threaded WASM lane. This runner keeps ONLY the test-harness
  path — per file: load source, discover the per-directory goccia
  config, create an engine with the TestRunner runtime profile,
  execute, call the in-engine runTests() and report its counts — and
  prints a line-oriented, timing-free protocol an external harness
  can diff:

      PASS <file> total=N passed=N failed=0 skipped=N
      FAIL <file> total=N passed=N failed=N skipped=N
      FAILNAME <file> :: <failed test name>
      FILEERROR <file> :: <first line of the load/run error>
      SUMMARY files=N failedfiles=N tests=N passed=N failed=N skipped=N

  Exit code 0 iff no test failed and no file errored.

  Invocation: the single argument is a MANIFEST file listing one
  script path per line ('#' starts a comment) — argv stays tiny no
  matter how many files a batch carries. Paths are used as given, so
  a WASI host passes guest paths under its preopens.

  The runner compiles both natively (FPC) and under LAKON; the FFI
  runtime extension is host-library-based and stays out of the LAKON
  build. }

{$I Goccia.inc}

uses
  {$IFNDEF LAKON}{$IFDEF UNIX}cthreads,{$ENDIF}{$ENDIF}
  Classes,
  SysUtils,

  CLI.ConfigFile,
  TextSemantics,

  Goccia.Arguments.Collection,
  Goccia.CLI.Options,
  Goccia.Engine,
  Goccia.Error,
  Goccia.Executor.Interpreter,
  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.Console,
  {$IFNDEF LAKON}
  Goccia.RuntimeExtensions.FFI,
  {$ENDIF}
  Goccia.Builtins.GlobalShadowRealm,
  Goccia.RuntimeProfiles.TestRunner,
  Goccia.StackLimit,
  Goccia.TextFiles,
  Goccia.Values.ArrayValue,
  Goccia.Values.Formatting,
  Goccia.Values.Error,
  Goccia.Values.FunctionBase,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TFileVerdict = record
    Total: Int64;
    Passed: Int64;
    Failed: Int64;
    Skipped: Int64;
    FailedNames: array of string;
    ErrorMessage: string;
  end;

function FirstLine(const AText: string): string;
var
  Cut: Integer;
begin
  Result := AText;
  Cut := Pos(#10, Result);
  if Cut > 0 then
    Result := Copy(Result, 1, Cut - 1);
  Result := TrimRight(Result);
end;

{ Per-directory config discovery — the GocciaTestRunner rule
  (DiscoverFileConfig): walk up from the script's directory to the
  nearest goccia.json. The suite tree carries JSON configs only;
  "extends" chains are resolved by ParseConfigFile. }
function DiscoverFileConfig(const AFileName: string): TConfigEntryArray;
var
  StartDirectory, ConfigPath: string;
begin
  SetLength(Result, 0);
  if AFileName = '' then
    Exit;
  StartDirectory := ExtractFilePath(ExpandFileName(AFileName));
  if StartDirectory = '' then
    StartDirectory := GetCurrentDir;
  ConfigPath := DiscoverConfigFile(StartDirectory,
    ['goccia'], ['.json']);
  if ConfigPath <> '' then
    Result := ParseConfigFile(ConfigPath);
end;

{ source-type: per-file config > file-extension default (the
  ResolveSourceTypeOption rule without the CLI arm — this runner
  takes no engine options from the command line). }
function ResolveSourceType(const AFileConfig: TConfigEntryArray;
  const AFileName: string): TGocciaSourceType;
var
  ValueText: string;
begin
  if FindConfigEntry(AFileConfig, 'source-type', ValueText) then
  begin
    ValueText := LowerCase(Trim(ValueText));
    if ValueText = 'module' then
      Exit(Goccia.Engine.stModule);
    if ValueText = 'script' then
      Exit(Goccia.Engine.stScript);
  end;
  if IsModuleSourceFileName(AFileName) then
    Exit(Goccia.Engine.stModule);
  Result := Goccia.Engine.stScript;
end;

{ allowed-hosts: per-file config > empty (fetch blocked) — the
  ApplyFileConfigToEngine rule without the CLI arm. The empty-value
  sentinel of a merged extends chain stops accumulation. }
procedure ApplyAllowedHosts(const AEngine: TGocciaEngine;
  const AFileConfig: TConfigEntryArray);
var
  Hosts: TStringList;
  Index: Integer;
  HasHosts: Boolean;
begin
  HasHosts := False;
  for Index := 0 to High(AFileConfig) do
    if AFileConfig[Index].Key = 'allowed-hosts' then
    begin
      HasHosts := True;
      Break;
    end;
  if not HasHosts then
    Exit;

  Hosts := TStringList.Create;
  try
    for Index := 0 to High(AFileConfig) do
      if AFileConfig[Index].Key = 'allowed-hosts' then
      begin
        if AFileConfig[Index].Value = '' then
          Break;
        Hosts.Add(AFileConfig[Index].Value);
      end;
    AEngine.SetAllowedFetchHosts(Hosts);
  finally
    Hosts.Free;
  end;
end;

procedure DisableRuntimeConsole(const AEngine: TGocciaEngine);
var
  ConsoleExtension: TGocciaConsoleRuntimeExtension;
  Runtime: TGocciaRuntimeCore;
begin
  Runtime := GetRuntime(AEngine);
  if not Assigned(Runtime) then
    Exit;
  ConsoleExtension := TGocciaConsoleRuntimeExtension(
    Runtime.FindRuntimeExtension(TGocciaConsoleRuntimeExtension));
  if Assigned(ConsoleExtension) and
     Assigned(ConsoleExtension.BuiltinConsole) then
    ConsoleExtension.BuiltinConsole.Enabled := False;
end;

{ The canonical harness call — GocciaTestRunner.RunRegisteredTests:
  invoke the runTests global the TestingLibrary registered and hand
  back its result object. }
function RunRegisteredTests(
  const AEngine: TGocciaEngine): TGocciaObjectValue;
var
  GC: TGarbageCollector;
  RunTestsValue: TGocciaValue;
  Options: TGocciaObjectValue;
  Args: TGocciaArgumentsCollection;
  ResultValue: TGocciaValue;
begin
  GC := TGarbageCollector.Instance;
  RunTestsValue :=
    AEngine.Interpreter.GlobalScope.GetBinding('runTests').Value;
  if not (RunTestsValue is TGocciaFunctionBase) then
    raise Exception.Create('runTests global is not callable');

  Options := TGocciaObjectValue.Create;
  Options.AssignProperty('exitOnFirstFailure',
    TGocciaBooleanLiteralValue.FalseValue);
  Options.AssignProperty('showTestResults',
    TGocciaBooleanLiteralValue.FalseValue);

  Args := TGocciaArgumentsCollection.Create([Options]);
  try
    if Assigned(GC) then
      GC.AddTempRoot(Options);
    try
      ResultValue := TGocciaFunctionBase(RunTestsValue).Call(
        Args, TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      if Assigned(GC) then
        GC.RemoveTempRoot(Options);
    end;
  finally
    Args.Free;
  end;

  if not (ResultValue is TGocciaObjectValue) then
    raise Exception.Create('runTests did not return a result object');
  Result := TGocciaObjectValue(ResultValue);
end;

procedure ExtractVerdict(const ATestResult: TGocciaObjectValue;
  var AVerdict: TFileVerdict);
var
  FailedTests: TGocciaValue;
  FailedArray: TGocciaArrayValue;
  Index: Integer;
begin
  AVerdict.Passed :=
    Trunc(ATestResult.GetProperty('passed').ToNumberLiteral.Value);
  AVerdict.Failed :=
    Trunc(ATestResult.GetProperty('failed').ToNumberLiteral.Value);
  AVerdict.Skipped :=
    Trunc(ATestResult.GetProperty('skipped').ToNumberLiteral.Value);
  AVerdict.Total :=
    Trunc(ATestResult.GetProperty('totalRunTests').ToNumberLiteral.Value);
  FailedTests := ATestResult.GetProperty('failedTests');
  if FailedTests is TGocciaArrayValue then
  begin
    FailedArray := TGocciaArrayValue(FailedTests);
    SetLength(AVerdict.FailedNames, FailedArray.Elements.Count);
    for Index := 0 to FailedArray.Elements.Count - 1 do
      AVerdict.FailedNames[Index] :=
        FailedArray.Elements[Index].ToStringLiteral.Value;
  end;
end;

{ Runs one script through the canonical harness path. Mirrors
  GocciaTestRunner.RunGocciaScriptInterpreted minus timeouts (the
  external harness owns wall-clock discipline) and minus the worker
  machinery. }
function RunOneFile(const AFileName: string): TFileVerdict;
var
  Source: TStringList;
  Executor: TGocciaInterpreterExecutor;
  Engine: TGocciaEngine;
  Core: TGocciaRuntimeCore;
  FileConfig: TConfigEntryArray;
  EngineOptions: TGocciaEngineOptions;
  Compatibility: TGocciaCompatibilityFlags;
  TestResult: TGocciaObjectValue;
  GC: TGarbageCollector;
begin
  Result.Total := 0;
  Result.Passed := 0;
  Result.Failed := 0;
  Result.Skipped := 0;
  SetLength(Result.FailedNames, 0);
  Result.ErrorMessage := '';

  Source := nil;
  EngineOptions := nil;
  try
    try
      Source := CreateFileTextLines(ReadUTF8FileText(AFileName));
      FileConfig := DiscoverFileConfig(AFileName);
      EngineOptions := TGocciaEngineOptions.Create;

      Executor := TGocciaInterpreterExecutor.Create;
      try
        Engine := TGocciaEngine.Create(AFileName, Source, Executor);
        try
          Engine.SourceType := ResolveSourceType(FileConfig, AFileName);
          ResolveCompatibilityFlags(EngineOptions, FileConfig, Compatibility);
          Engine.Compatibility := Compatibility;
          Engine.StrictTypes :=
            ResolveFlagOption(EngineOptions.StrictTypes, FileConfig);
          Engine.FunctionConstructor.Enabled := ResolveFlagOption(
            EngineOptions.UnsafeFunctionConstructor, FileConfig);
          if ResolveFlagOption(EngineOptions.UnsafeShadowRealm,
             FileConfig) then
            EnableShadowRealm(Engine);

          Core := AttachRuntime(Engine);
          ApplyTestRunnerRuntimeProfile(Core);
          ApplyAllowedHosts(Engine, FileConfig);
          {$IFNDEF LAKON}
          if ResolveFlagOption(EngineOptions.UnsafeFFI, FileConfig) then
            Core.Install(TGocciaFFIRuntimeExtension.Create);
          {$ENDIF}
          DisableRuntimeConsole(Engine);
          Engine.SuppressWarnings := True;

          Engine.Execute;
          TestResult := RunRegisteredTests(Engine);
          GC := TGarbageCollector.Instance;
          if Assigned(GC) then
            GC.AddTempRoot(TestResult);
          try
            ExtractVerdict(TestResult, Result);
          finally
            if Assigned(GC) then
              GC.RemoveTempRoot(TestResult);
          end;
        finally
          Engine.Free;
        end;
      finally
        Executor.Free;
      end;
    except
      on E: TGocciaError do
        Result.ErrorMessage := FirstLine(E.Message);
      on E: TGocciaThrowValue do
        Result.ErrorMessage :=
          FirstLine(E.Value.ToStringLiteral.Value);
      on E: Exception do
        Result.ErrorMessage := FirstLine(E.Message);
    end;
  finally
    EngineOptions.Free;
    Source.Free;
  end;

  { A file that could not run counts as one failed test — the
    GocciaTestRunner MarkLoadError convention. }
  if Result.ErrorMessage <> '' then
  begin
    Result.Total := Result.Total + 1;
    Result.Failed := Result.Failed + 1;
  end;
end;

var
  Manifest: TStringList;
  Verdict: TFileVerdict;
  FileName, Line: string;
  Index, NameIndex: Integer;
  FileCount, FailedFileCount: Int64;
  TotalTests, TotalPassed, TotalFailed, TotalSkipped: Int64;
  GC: TGarbageCollector;

begin
  if ParamCount < 1 then
  begin
    WriteLn(ErrOutput,
      'Usage: GocciaWasmTestRunner <manifest-file>');
    WriteLn(ErrOutput,
      '  manifest: one script path per line, # starts a comment');
    ExitCode := 2;
    Exit;
  end;

  { The TGocciaCLIApplication singleton defaults (InitializeSingletons)
    — the stack governor turns runaway recursion into the RangeError
    the suite pins instead of a host stack overflow. }
  SetMaxStackDepth(DEFAULT_MAX_STACK_DEPTH);
  SetInspectDepth(DEFAULT_INSPECT_DEPTH);

  FileCount := 0;
  FailedFileCount := 0;
  TotalTests := 0;
  TotalPassed := 0;
  TotalFailed := 0;
  TotalSkipped := 0;

  Manifest := TStringList.Create;
  try
    Manifest.LoadFromFile(ParamStr(1));
    for Index := 0 to Manifest.Count - 1 do
    begin
      Line := Trim(Manifest[Index]);
      if (Line = '') or (Line[1] = '#') then
        Continue;
      FileName := Line;
      FileCount := FileCount + 1;

      Verdict := RunOneFile(FileName);
      TotalTests := TotalTests + Verdict.Total;
      TotalPassed := TotalPassed + Verdict.Passed;
      TotalFailed := TotalFailed + Verdict.Failed;
      TotalSkipped := TotalSkipped + Verdict.Skipped;

      if (Verdict.Failed > 0) or (Verdict.ErrorMessage <> '') then
      begin
        FailedFileCount := FailedFileCount + 1;
        WriteLn('FAIL ', FileName,
          ' total=', Verdict.Total,
          ' passed=', Verdict.Passed,
          ' failed=', Verdict.Failed,
          ' skipped=', Verdict.Skipped);
      end
      else
        WriteLn('PASS ', FileName,
          ' total=', Verdict.Total,
          ' passed=', Verdict.Passed,
          ' failed=', Verdict.Failed,
          ' skipped=', Verdict.Skipped);
      for NameIndex := 0 to High(Verdict.FailedNames) do
        WriteLn('FAILNAME ', FileName, ' :: ',
          FirstLine(Verdict.FailedNames[NameIndex]));
      if Verdict.ErrorMessage <> '' then
        WriteLn('FILEERROR ', FileName, ' :: ', Verdict.ErrorMessage);

      GC := TGarbageCollector.Instance;
      if Assigned(GC) then
        GC.Collect;
    end;
  finally
    Manifest.Free;
  end;

  WriteLn('SUMMARY files=', FileCount,
    ' failedfiles=', FailedFileCount,
    ' tests=', TotalTests,
    ' passed=', TotalPassed,
    ' failed=', TotalFailed,
    ' skipped=', TotalSkipped);

  if (TotalFailed > 0) or (FailedFileCount > 0) then
    ExitCode := 1;
end.
