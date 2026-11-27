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

function NewWhileEngine: TGocciaEngine;
var Profile: TLangProfile; ErrorText: UnicodeString;
begin
  Result := TGocciaEngine.Create;
  Profile := Result.Profile;
  Profile.WhileLoop := True;
  Result.Profile := Profile;
  if not Result.PrepareHost(ErrorText) then Check(False, 'host setup: ' + ErrorText);
  Result.ResultForm := grfString;
end;

{ a run that fails with an uncaught exception takes the jobs and timers it queued with it, as an aborted run does;
  the next run neither runs them nor is blamed for them. AHostWork: a job and a timer the host queued before the
  run, which go with it too. }
procedure CheckFailedRun(const ALabel, AFileName, ASource, AExpected: UnicodeString; AHostWork: Boolean = True);
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
begin
  Engine := NewWhileEngine;
  try
    RunResult := Engine.Run('setup.js', 'globalThis.log = [];');
    Check(RunResult.Ok, ALabel + ' setup: ' + RunResult.ErrorMessage);
    CallbackCount := 0;
    if AHostWork then QueueCallbacks;
    RunResult := Engine.Run(AFileName, ASource);
    Check(not RunResult.Ok and (RunResult.Aborted = gaNone) and (RunResult.ErrorMessage = AExpected),
      ALabel + ' should fail with ' + AExpected + ', got ' + RunResult.ErrorMessage);
    Check(PendingJobCount = 0, ALabel + ' should discard its queued jobs');
    RunResult := Engine.Run('next.js', 'log.push("next"); log.join();');
    Check(RunResult.Ok and (RunResult.ResultText = 'next'),
      ALabel + ': the next run should run only its own code, got ' + RunResult.ResultText + RunResult.ErrorMessage);
    Check(CallbackCount = 0, ALabel + ' should discard the host''s job and timer');
    QueueCallbacks;
    RunResult := Engine.Run('normal.js', '1 + 1;');
    Check(RunResult.Ok and (CallbackCount = 2), ALabel + ' should allow new work to drain');
  finally
    Engine.Free;
  end;
end;

{ a limit met inside a promise job reports its own error, as one met in the entry's code or a timer does }
procedure CheckAbortMessage(const ALabel, AFileName, ASource: UnicodeString);
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
begin
  Engine := NewWhileEngine;
  try
    Engine.MaxInstructions := 100000;
    RunResult := Engine.Run(AFileName, ASource);
    Check(RunResult.Aborted = gaInstructionLimit, ALabel + ' should meet the instruction limit');
    Check(RunResult.ErrorMessage = 'Error: Execution exceeded instruction limit of 100000',
      ALabel + ' reported ' + RunResult.ErrorMessage);
    Check((RunResult.ErrorName = 'Error') and (RunResult.ErrorText = 'Execution exceeded instruction limit of 100000'),
      ALabel + ' reported the name ' + RunResult.ErrorName + ' and the text ' + RunResult.ErrorText);
  finally
    Engine.Free;
  end;
end;

procedure WriteModule(const APath, ASource: string);
var F: TextFile;
begin
  AssignFile(F, APath);
  Rewrite(F);
  Write(F, ASource);
  CloseFile(F);
end;

{ a module whose evaluation a failed or aborted run started records the run's error, as a module that threw does:
  a later import rejects with it, whether the module was synchronous or waiting after a top-level await, and its
  code does not run again }
procedure CheckModuleRecords(const ADir: string);
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;

  procedure CheckImport(const AModule, AFirst, AExpected: string; ALimit: Boolean);
  var LabelText: string;
  begin
    LabelText := 'module ' + AModule;
    if ALimit then Engine.MaxInstructions := 100000;
    RunResult := Engine.Run(ADir + 'first-' + AModule, UnicodeString(AFirst));
    Check(not RunResult.Ok, LabelText + ': the first run should fail');
    Engine.MaxInstructions := 0;
    RunResult := Engine.Run(ADir + 'again-' + AModule,
      UnicodeString('try { await import("./' + AModule + '"); "imported" } catch (e) { String(e) + " runs=" + runs }'));
    Check(RunResult.Ok and (RunResult.ResultText = UnicodeString(AExpected + ' runs=1')),
      LabelText + ': a later import gave ' + RunResult.ResultText + RunResult.ErrorMessage);
  end;

const
  LIMIT = 'Error: Execution exceeded instruction limit of 100000';
begin
  WriteModule(ADir + 'sync-loop.mjs', 'globalThis.runs = (globalThis.runs ?? 0) + 1; while (true) {}');
  WriteModule(ADir + 'await-loop.mjs',
    'globalThis.runs = (globalThis.runs ?? 0) + 1; await Promise.resolve(); while (true) {}');
  WriteModule(ADir + 'await-timer.mjs',
    'globalThis.runs = (globalThis.runs ?? 0) + 1; await new Promise((r) => setTimeout(r, 0));' +
    'globalThis.done = true;');
  Engine := NewWhileEngine;
  try
    CheckImport('sync-loop.mjs', 'import "./sync-loop.mjs";', LIMIT, True);
  finally
    Engine.Free;
  end;
  Engine := NewWhileEngine;
  try
    CheckImport('await-loop.mjs', 'import "./await-loop.mjs";', LIMIT, True);
  finally
    Engine.Free;
  end;
  { a run that fails while a module it imported waits for a timer: the timer goes with the run. The module starts
    in the import's job, before the run's own timer fails it. }
  Engine := NewWhileEngine;
  try
    CheckImport('await-timer.mjs', 'import("./await-timer.mjs");' +
      'setTimeout(() => queueMicrotask(() => { throw new Error("left by the failed run"); }), 0);',
      'Error: left by the failed run', False);
    RunResult := Engine.Run('done.js', 'globalThis.done === undefined;');
    Check(RunResult.Ok and (RunResult.ResultText = 'true'), 'the failed run''s module should not finish later');
  finally
    Engine.Free;
  end;
end;

procedure CheckSyncWaitTimeout;
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
begin
  Engine := TGocciaEngine.Create;
  try
    Engine.ResultForm := grfString;
    RunResult := Engine.Run('setup.js', 'globalThis.cells = new Int32Array(new SharedArrayBuffer(4));');
    Check(RunResult.Ok, 'synchronous wait fixture setup: ' + RunResult.ErrorMessage);
    Engine.TimeoutMs := 100;
    RunResult := Engine.Run('wait.js', 'Atomics.wait(cells, 0, 0);');
    Check(RunResult.Aborted = gaTimeout, 'a synchronous wait should respect the run deadline');
    Engine.TimeoutMs := 0;
    RunResult := Engine.Run('notify.js', 'Atomics.notify(cells, 0);');
    Check(RunResult.Ok and (RunResult.ResultText = '0'), 'timeout should unregister the synchronous waiter');
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
      '  let target = {};' +
      '  const held = {};' +
      '  registry.register(target, held);' +
      '  globalThis.heldRef = new WeakRef(held);' +
      '  target = undefined;' +
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
    CollectMinor;
    CollectMajor;
    Engine.ResultForm := grfString;
    RunResult := Engine.Run('next.js', 'heldRef.deref() === undefined;');
    Check(RunResult.Ok and (CallbackCount = 0), 'aborted finalization must not run later');
    Check(RunResult.ResultText = 'true', 'a discarded registration should release its held value');
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

{ an aborted run drops the requests still waiting for a worker as well as the running ones, and the engine's next
  run gets its slots: its own request is answered }
procedure CheckFetchQueueAbort(const AURL: UnicodeString);
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
  StartFetches, Argument, Status, Waiting, Element, Ignored: TValue;
  ErrorText: UnicodeString;
  I: Integer;
begin
  Engine := TGocciaEngine.Create;
  try
    Engine.Capabilities := TGocciaCapabilities.None.Allow(gcNet, '127.0.0.1');
    if not Engine.PrepareHost(ErrorText) then Check(False, 'fetch host setup: ' + ErrorText);
    DefineNative(GlobalObj, 'stopRun', @StopRun, 0);
    RunResult := Engine.Run('setup.js',
      'globalThis.startFetches = (url) => { globalThis.waiting = Array.from({ length: 40 }, () => fetch(url)); };');
    Check(RunResult.Ok, 'fetch queue fixture setup: ' + RunResult.ErrorMessage);
    Check(GetV(GlobalObj, AtomOf('startFetches'), StartFetches), 'could not read fetch queue fixture');
    PushRoot(StartFetches);
    Argument := StrFromUnicode(AURL);
    Check(CallValue(RootAt(TempTop - 1), VUndefined, @Argument, 1, Status), 'could not start the fetches');
    PopRoots(1);
    Check(FetchPending, 'setup should have fetches pending');
    RunResult := Engine.Run('stop.js', 'stopRun();');
    Check(RunResult.Aborted = gaHostAbort, 'fetch queue fixture should abort');
    Check(not FetchPending, 'abort should drop the running and the waiting fetches');
    { none was refused for want of a worker: the dropped requests' promises stay pending }
    Check(GetV(GlobalObj, AtomOf('waiting'), Waiting), 'could not read the waiting fetches');
    PushRoot(Waiting);
    try
      for I := 0 to 39 do
      begin
        Check(GetElemV(RootAt(TempTop - 1), MkInt(I), Element), 'could not read a waiting fetch');
        Check(PromiseStatus(Element, Ignored) = 0, 'every dropped fetch promise should stay pending');
      end;
    finally
      PopRoots(1);
    end;
    Argument := StrFromUnicode(AURL);
    PushRoot(Argument);
    Check(SetV(GlobalObj, AtomOf('nextURL'), RootAt(TempTop - 1)), 'could not set the next URL');
    PopRoots(1);
    RunResult := Engine.Run('next.js',
      'globalThis.status = 0; fetch(nextURL).then((r) => { globalThis.status = r.status; });');
    Check(RunResult.Ok, 'the next run should fetch: ' + RunResult.ErrorMessage);
    Check(GetV(GlobalObj, AtomOf('status'), Status) and IsNumber(Status) and (NumOf(Status) = 200),
      'the next run''s request should be answered after the aborted run''s queue was dropped');
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
    CheckSyncWaitTimeout;
    CheckAsyncWaitTimeout;
    CheckFinalizationAbort;
    CheckFailedRun('a script that throws', 'thrown.js',
      'Promise.resolve().then(() => log.push("job")); setTimeout(() => log.push("timer"), 0);' +
      'throw new Error("boom");', 'Error: boom');
    CheckFailedRun('a module that throws', 'thrown.mjs',
      'Promise.resolve().then(() => log.push("job")); setTimeout(() => log.push("timer"), 0);' +
      'throw new Error("boom");', 'Error: boom');
    CheckFailedRun('a microtask that throws', 'microtask-throws.js',
      'queueMicrotask(() => { Promise.resolve().then(() => log.push("job")); setTimeout(() => log.push("timer"), 0);' +
      'throw new Error("from a microtask"); });', 'Error: from a microtask', False);
    CheckAbortMessage('a limit met in the entry', 'entry.js', 'while (true) {}');
    CheckAbortMessage('a limit met in a timer', 'timer.js', 'setTimeout(() => { while (true) {} }, 0);');
    CheckAbortMessage('a limit met in a promise job', 'job.js', 'Promise.resolve().then(() => { while (true) {} });');
    CheckAbortMessage('a limit met after a top-level await', 'tla.mjs', 'await Promise.resolve(); while (true) {}');
    CheckModuleRecords(IncludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(ParamStr(0)))));
    if ParamCount = 1 then
    begin
      CheckFetchAbort(ParamStr(1));
      CheckFetchQueueAbort(ParamStr(1));
    end;
    WriteLn('Aborted run tests passed');
  except
    on Error: Exception do
    begin
      WriteLn(StdErr, Error.Message);
      Halt(1);
    end;
  end;
end.
