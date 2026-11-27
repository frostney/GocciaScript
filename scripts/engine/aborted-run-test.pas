program AbortedRunTest;

{$mode delphiunicode}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils,
  Goccia.Atomics,
  Goccia.Capabilities,
  Goccia.Core,
  Goccia.Embed,
  Goccia.Fetch,
  Goccia.Promise,
  Goccia.VM,
  Goccia.Weak;

var
  CallbackCount: Integer;

procedure Check(ACondition: Boolean; const AMessage: string);
begin
  if ACondition then Exit;
  raise Exception.Create(AMessage);
end;

function CountCallback(AArgs: PValue; AArgc: Integer; var AValue: TValue): Boolean;
begin
  Inc(CallbackCount);
  AValue := VUndefined;
  Result := True;
end;

function StopRun(AArgs: PValue; AArgc: Integer; var AValue: TValue): Boolean;
begin
  Result := HostAbort('host ended the test run');
end;

function RefuseMemory(AArgs: PValue; AArgc: Integer; var AValue: TValue): Boolean;
begin
  { Lower the limit inside the test's handler, without growing an unbounded object graph. }
  HeapCeiling := 1;
  CollectMinor;
  CollectMajor;
  AValue := VUndefined;
  Result := True;
end;

procedure QueueCallbacks;
var Timer, Ignored: TValue; Args: array[0..1] of TValue; Base: Integer;
begin
  Base := TempTop;
  PushRoot(NewNativeFunction(@CountCallback, 'countCallback', 0));
  PushRoot(NewPendingPromise);
  PushRoot(NewPendingPromise);
  PromiseThenNative(RootAt(Base + 1), RootAt(Base), VUndefined, RootAt(Base + 2));
  ResolvePendingPromise(RootAt(Base + 1), VUndefined);
  Check(GetV(GlobalObj, AtomOf('setTimeout'), Timer), 'could not read setTimeout');
  Args[0] := RootAt(Base);
  Args[1] := MkInt(0);
  Check(CallValue(Timer, VUndefined, @Args[0], Length(Args), Ignored), 'could not schedule a timer');
  PopRoots(3);
end;

procedure CheckAbort(AExpected: TGocciaAbort; AFromArtifact: Boolean = False);
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
  Source, ErrorText: UnicodeString;
  Bytes: TBytes;
  Artifact: RawByteString;
  LabelText: UnicodeString;
  Profile: TLangProfile;
begin
  LabelText := 'abort ' + IntToStr(Ord(AExpected));
  Engine := TGocciaEngine.Create;
  try
    Profile := Engine.Profile;
    Profile.WhileLoop := True;
    Engine.Profile := Profile;
    if not Engine.PrepareHost(ErrorText) then Check(False, LabelText + ' host setup: ' + ErrorText);
    Engine.ResultForm := grfString;
    DefineNative(GlobalObj, 'stopRun', @StopRun, 0);
    DefineNative(GlobalObj, 'refuseMemory', @RefuseMemory, 0);
    Source := 'while (true) {}';
    case AExpected of
      gaTimeout: Engine.TimeoutMs := 100;
      gaInstructionLimit: Engine.MaxInstructions := 1000;
      gaMemoryLimit:
        begin
          Engine.MaxInstructions := 100000;
          Source := 'while (true) { try { refuseMemory(); while (true) {} } catch {} }';
        end;
      gaHostAbort: Source := 'stopRun();';
    else
      Check(False, 'expected an abort reason');
    end;
    if AFromArtifact then
    begin
      if not Engine.CompileArtifact('entry.js', Source, Bytes, RunResult) then
        Check(False, LabelText + ' artifact compile: ' + RunResult.ErrorMessage);
      SetString(Artifact, PAnsiChar(@Bytes[0]), Length(Bytes));
    end;
    CallbackCount := 0;
    QueueCallbacks;
    Check(PendingJobCount = 1, LabelText + ' setup should queue one promise job');
    if AFromArtifact then RunResult := Engine.RunArtifact('entry.gbc', Artifact)
    else RunResult := Engine.Run('entry.js', Source);
    Check((not RunResult.Ok) and (RunResult.Aborted = AExpected),
      LabelText + ' returned ' + IntToStr(Ord(RunResult.Aborted)) + ': ' + RunResult.ErrorMessage);
    Check(PendingJobCount = 0, LabelText + ' should discard its queued job');
    Check(CallbackCount = 0, LabelText + ' should run no callbacks');

    Engine.MaxMemory := 0;
    Engine.TimeoutMs := 0;
    Engine.MaxInstructions := 0;
    RunResult := Engine.Run('next.js', '1 + 1;');
    Check(RunResult.Ok and (RunResult.ResultText = '2'), LabelText + ' should allow a new run');
    Check(CallbackCount = 0, LabelText + ' should discard its queued timer');
    QueueCallbacks;
    RunResult := Engine.Run('normal.js', '1 + 1;');
    Check(RunResult.Ok and (CallbackCount = 2), LabelText + ' should allow new work to drain');
  finally
    Engine.Free;
  end;
end;

procedure CheckAsyncWaitTimeout;
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
  StartWait, Ignored: TValue;
  StartedAt: QWord;
begin
  Engine := TGocciaEngine.Create;
  try
    RunResult := Engine.Run('setup.js',
      'globalThis.cells = new Int32Array(new SharedArrayBuffer(4));' +
      'globalThis.startWait = () => Atomics.waitAsync(cells, 0, 0, 5000);');
    Check(RunResult.Ok, 'wait fixture setup: ' + RunResult.ErrorMessage);
    Check(GetV(GlobalObj, AtomOf('startWait'), StartWait), 'could not read wait fixture');
    Check(CallValue(StartWait, VUndefined, nil, 0, Ignored), 'could not start an async wait');
    Check(AsyncWaitsPending, 'setup should have a pending wait');
    Engine.TimeoutMs := 100;
    StartedAt := GetTickCount64;
    RunResult := Engine.Run('idle.js', '0;');
    Check(RunResult.Aborted = gaTimeout, 'an asynchronous wait should respect the run deadline');
    Check(GetTickCount64 - StartedAt < 3000, 'the run should stop before the wait expires');
    Check(not AsyncWaitsPending, 'timeout should remove the pending wait');
    Engine.TimeoutMs := 0;
    Engine.ResultForm := grfString;
    RunResult := Engine.Run('notify.js', 'Atomics.notify(cells, 0);');
    Check(RunResult.Ok and (RunResult.ResultText = '0'), 'timeout should unregister the waiter');
  finally
    Engine.Free;
  end;
end;

procedure CheckFinalizationAbort;
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
  RegisterTarget, Ignored: TValue;
  ErrorText: UnicodeString;
begin
  Engine := TGocciaEngine.Create;
  try
    if not Engine.PrepareHost(ErrorText) then Check(False, 'finalization host setup: ' + ErrorText);
    DefineNative(GlobalObj, 'stopRun', @StopRun, 0);
    DefineNative(GlobalObj, 'countCallback', @CountCallback, 0);
    RunResult := Engine.Run('setup.js',
      'globalThis.registry = new FinalizationRegistry(countCallback);' +
      'globalThis.registerTarget = () => {' +
      '  let target = {}; registry.register(target, "held"); target = undefined;' +
      '};');
    Check(RunResult.Ok, 'finalization fixture setup: ' + RunResult.ErrorMessage);
    Check(GetV(GlobalObj, AtomOf('registerTarget'), RegisterTarget), 'could not read finalization fixture');
    Check(CallValue(RegisterTarget, VUndefined, nil, 0, Ignored), 'could not register weak target');
    CollectMinor;
    CollectMajor;
    Check(WeakCleanupPending, 'setup should queue finalization work');
    CallbackCount := 0;
    RunResult := Engine.Run('stop.js', 'stopRun();');
    Check(RunResult.Aborted = gaHostAbort, 'finalization fixture should abort');
    Check(not WeakCleanupPending, 'abort should discard finalization work');
    RunResult := Engine.Run('next.js', '1 + 1;');
    Check(RunResult.Ok and (CallbackCount = 0), 'aborted finalization must not run later');
  finally
    Engine.Free;
  end;
end;

procedure CheckFetchAbort(const AURL: UnicodeString);
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
  StartFetch, Request, Argument, Ignored: TValue;
  ErrorText: UnicodeString;
begin
  Engine := TGocciaEngine.Create;
  try
    Engine.Capabilities := TGocciaCapabilities.None.Allow(gcNet, '127.0.0.1');
    if not Engine.PrepareHost(ErrorText) then Check(False, 'fetch host setup: ' + ErrorText);
    DefineNative(GlobalObj, 'stopRun', @StopRun, 0);
    RunResult := Engine.Run('setup.js',
      'globalThis.controller = new AbortController();' +
      'globalThis.startFetch = (url) => fetch(url, { signal: controller.signal });');
    Check(RunResult.Ok, 'fetch fixture setup: ' + RunResult.ErrorMessage);
    Check(GetV(GlobalObj, AtomOf('startFetch'), StartFetch), 'could not read fetch fixture');
    PushRoot(StartFetch);
    Argument := StrFromUnicode(AURL);
    Check(CallValue(RootAt(TempTop - 1), VUndefined, @Argument, 1, Request), 'could not start a fetch');
    PopRoots(1);
    PushRoot(Request);
    try
      Check(FetchPending, 'setup should have a fetch completion pending');
      RunResult := Engine.Run('stop.js', 'stopRun();');
      Check(RunResult.Aborted = gaHostAbort, 'fetch fixture should abort');
      Check(not FetchPending, 'abort should drop the pending fetch completion');
      RunResult := Engine.Run('next.js', 'controller.abort();');
      Check(RunResult.Ok and (PromiseStatus(RootAt(TempTop - 1), Ignored) = 0),
        'the old fetch promise should stay pending');
    finally
      PopRoots(1);
    end;
  finally
    Engine.Free;
  end;
end;

begin
  try
    CheckAbort(gaHostAbort);
    CheckAbort(gaInstructionLimit, True);
    CheckAbort(gaTimeout);
    CheckAbort(gaMemoryLimit);
    CheckAsyncWaitTimeout;
    CheckFinalizationAbort;
    if ParamCount = 1 then CheckFetchAbort(ParamStr(1));
    WriteLn('Aborted run tests passed');
  except
    on Error: Exception do
    begin
      WriteLn(StdErr, Error.Message);
      Halt(1);
    end;
  end;
end.
