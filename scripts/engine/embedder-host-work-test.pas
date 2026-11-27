program EmbedderHostWorkTest;

{$mode delphiunicode}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils,
  Goccia.Capabilities,
  Goccia.Core,
  Goccia.Embed,
  Goccia.HostAPI,
  Goccia.Promise,
  Goccia.VM;

type
  TPromiseSlots = array[0..1] of TValue;
  PPromiseSlots = ^TPromiseSlots;

  TTestWork = class(TGocciaEmbedderWork)
  private
    FSlots: PPromiseSlots;
    FSequence: QWord;
    FRequest: array[0..1] of QWord;
  public
    Stall, Ready, FailOnWait, Late, SettleThenFail, ForcePending: Boolean;
    { waits after which the first request settles with FirstValue }
    FirstAfterTurns: Integer;
    Discards, IdleTurns, ScopedDiscards: Integer;
    { the timeout of the pump that last delivered results }
    DeliveredTimeout: Int64;
    constructor Create(ASlots: PPromiseSlots);
    function BeginRequest: TValue;
    function Mark: QWord; override;
    procedure DiscardFrom(AMark: QWord); override;
    function HasPending: Boolean; override;
    function Pump(ATimeoutMs: Cardinal): Boolean; override;
    procedure Discard; override;
  end;

threadvar
  Work: TTestWork;

var
  { the result the first request settles with (FirstAfterTurns), rooted for the process }
  FirstValue: TValue;

function BeginWork(AArgs: PValue; AArgc: Integer; var AResult: TValue): Boolean;
begin
  if Work = nil then Exit(ThrowTypeError('host work is detached'));
  AResult := Work.BeginRequest;
  Result := True;
end;

function SetFirstValue(AArgs: PValue; AArgc: Integer; var AResult: TValue): Boolean;
begin
  if AArgc >= 1 then FirstValue := AArgs[1] else FirstValue := VUndefined;
  AResult := VUndefined;
  Result := True;
end;

function ReadyWork(AArgs: PValue; AArgc: Integer; var AResult: TValue): Boolean;
begin
  if Work = nil then Exit(ThrowTypeError('host work is detached'));
  Work.Ready := True;
  AResult := VUndefined;
  Result := True;
end;

constructor TTestWork.Create(ASlots: PPromiseSlots);
begin
  inherited Create;
  FSlots := ASlots;
end;

function TTestWork.BeginRequest: TValue;
var I: Integer;
begin
  for I := Low(FSlots^) to High(FSlots^) do
    if FSlots^[I] = VUndefined then
    begin
      Inc(FSequence);
      FRequest[I] := FSequence;
      FSlots^[I] := NewPendingPromise;
      Exit(FSlots^[I]);
    end;
  raise Exception.Create('no test request slot available');
end;

function TTestWork.HasPending: Boolean;
begin
  Result := ForcePending or (FSlots^[0] <> VUndefined) or (FSlots^[1] <> VUndefined);
end;

function TTestWork.Pump(ATimeoutMs: Cardinal): Boolean;
var I: Integer;
begin
  Result := True;
  if (FirstAfterTurns > 0) and (ATimeoutMs > 0) then
  begin
    Dec(FirstAfterTurns);
    if (FirstAfterTurns = 0) and (FSlots^[0] <> VUndefined) then
    begin
      ResolvePendingPromise(FSlots^[0], FirstValue);
      FSlots^[0] := VUndefined;
    end;
  end;
  if SettleThenFail and (FSlots^[0] <> VUndefined) then
  begin
    ResolvePendingPromise(FSlots^[0], MkInt(42));
    FSlots^[0] := VUndefined;
    Exit(ThrowTypeError('pump failed after a result'));
  end;
  if not Ready then
  begin
    if ATimeoutMs = 0 then Exit;
    if IdleTurns > 0 then
    begin
      Dec(IdleTurns);
      Exit;
    end;
    if FailOnWait then
    begin
      Sleep(75);
      Exit(ThrowTypeError('pump failed'));
    end;
    { results that arrive only after the run's deadline has passed }
    if Late then Sleep(120)
    else if Stall then
    begin
      Sleep(ATimeoutMs);
      Exit;
    end;
  end;
  DeliveredTimeout := ATimeoutMs;
  for I := Low(FSlots^) to High(FSlots^) do
    if FSlots^[I] <> VUndefined then
    begin
      ResolvePendingPromise(FSlots^[I], MkInt(42));
      FSlots^[I] := VUndefined;
    end;
end;

function TTestWork.Mark: QWord;
begin
  Result := FSequence;
end;

procedure TTestWork.DiscardFrom(AMark: QWord);
var I: Integer;
begin
  for I := Low(FSlots^) to High(FSlots^) do
    if (FSlots^[I] <> VUndefined) and (FRequest[I] > AMark) then
    begin
      Inc(ScopedDiscards);
      FSlots^[I] := VUndefined;
    end;
end;

procedure TTestWork.Discard;
begin
  Inc(Discards);
  ForcePending := False;
  FSlots^[0] := VUndefined;
  FSlots^[1] := VUndefined;
end;

procedure Check(ACondition: Boolean; const AMessage: UnicodeString);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

procedure InstallHostNatives;
begin
  Check(InjectGlobalBinding('hostPromise', NewNativeFunction(@BeginWork, 'hostPromise', 0)),
    'could not install hostPromise');
  Check(InjectGlobalBinding('hostReady', NewNativeFunction(@ReadyWork, 'hostReady', 0)),
    'could not install hostReady');
  Check(InjectGlobalBinding('hostFirstValue', NewNativeFunction(@SetFirstValue, 'hostFirstValue', 1)),
    'could not install hostFirstValue');
end;

{ a run whose host environment started work and then failed, or whose entry then failed, releases that work, and
  the next run delivers new work }
procedure CheckFailedProvider(AEngine: TGocciaEngine; const APath: string; const AProvider, AEntry,
  AWhat, AExpectedError: UnicodeString);
var
  Provider: TStringList;
  RunResult: TGocciaRunResult;
  Discards: Integer;
begin
  Provider := TStringList.Create;
  try
    Provider.Text := AProvider;
    Provider.SaveToFile(APath);
  finally
    Provider.Free;
  end;
  Discards := Work.Discards;
  AEngine.HostEnvironment := UnicodeString(APath);
  { each call's entries are new modules: a path already evaluated on this engine would not run again }
  RunResult := AEngine.Run('external-provider-' + IntToStr(Discards) + '.mjs', AEntry);
  AEngine.HostEnvironment := '';
  Check(not RunResult.Ok and (Work.Discards = Discards + 1) and not Work.HasPending and
    ((AExpectedError = '') or (RunResult.ErrorName = AExpectedError)),
    AWhat + ' left host work behind: ' + RunResult.ErrorName + ': ' + RunResult.ErrorMessage);
  RunResult := AEngine.Run('external-after-provider-' + IntToStr(Discards) + '.mjs', 'await hostPromise();');
  Check(RunResult.Ok and (RunResult.ResultText = '42'),
    'the run after ' + AWhat + ' could not deliver host work: ok=' + BoolToStr(RunResult.Ok, True) + ' result=' +
    RunResult.ResultText + ' error=' + RunResult.ErrorMessage);
end;

procedure CheckRuns(ABare: Boolean);
var
  Engine: TGocciaEngine;
  ErrorText, Source: UnicodeString;
  RunResult: TGocciaRunResult;
  PromiseSlots: TPromiseSlots;
  ProviderDir: string;
  Discards: Integer;
begin
  PromiseSlots[0] := VUndefined;
  PromiseSlots[1] := VUndefined;
  Work := TTestWork.Create(@PromiseSlots);
  Engine := TGocciaEngine.Create;
  try
    Engine.Bare := ABare;
    Engine.ResultForm := grfString;
    if not ABare and (ParamCount = 1) then
      Engine.Capabilities := TGocciaCapabilities.None.Allow(gcNet, '127.0.0.1');
    Engine.SetHostWork(Work);
    Check(Engine.PrepareHost(ErrorText), ErrorText);
    AddRootRange(@PromiseSlots[0], Length(PromiseSlots));
    AddRoot(@FirstValue);
    InstallHostNatives;

    if ABare then Source := 'await hostPromise();'
    else Source := 'let timer = false; setTimeout(() => { timer = true; }, 0); (await hostPromise()) + Number(timer);';
    RunResult := Engine.Run('external-ready.mjs', Source);
    Check(RunResult.Ok, RunResult.ErrorMessage);
    if ABare then Check(RunResult.ResultText = '42', 'host result is wrong')
    else Check(RunResult.ResultText = '43', 'the queued timer did not run before the wait');
    Check(not Work.HasPending and (Work.Discards = 0), 'successful work was not released normally');

    { idle pumps spend no instructions, and neither does a parked interval the run does not fire }
    Engine.MaxInstructions := 1000;
    Work.IdleTurns := 2000;
    if ABare then Source := 'await hostPromise();'
    else Source := 'const parked = setInterval(() => {}, 1000); const value = await hostPromise(); clearInterval(parked); value;';
    RunResult := Engine.Run('external-idle-budget.mjs', Source);
    Check(RunResult.Ok and (RunResult.ResultText = '42') and (Work.IdleTurns = 0), 'idle pumps spent the cap: ' + RunResult.ErrorMessage);
    Engine.MaxInstructions := 0;

    if not ABare then
    begin
      { a long timer does not overtake host work that is still pending: the virtual clock waits for it }
      Work.IdleTurns := 3;
      RunResult := Engine.Run('external-race.mjs',
        'await Promise.race([hostPromise(), new Promise(resolve => setTimeout(() => resolve("timeout"), 30000))]);');
      Check(RunResult.Ok and (RunResult.ResultText = '42') and (Work.IdleTurns = 0),
        'a virtual timer overtook pending host work: ' + RunResult.ResultText + RunResult.ErrorMessage);
    end;

    Work.Ready := True;
    Work.IdleTurns := 1;
    Work.DeliveredTimeout := -1;
    RunResult := Engine.Run('external-nonblocking.mjs', 'await hostPromise();');
    Check(RunResult.Ok and (Work.IdleTurns = 1) and (Work.DeliveredTimeout = 0),
      'ready host work needed a blocking wait: ' + IntToStr(Work.DeliveredTimeout));
    Work.IdleTurns := 0;
    Work.Ready := False;

    Engine.TimeoutMs := 2000;
    Source := 'const a = new Int32Array(new SharedArrayBuffer(8));' +
      'const held = Atomics.waitAsync(a, 0, 0).value; await Atomics.waitAsync(a, 1, 0, 1).value;' +
      'const answer = await hostPromise(); Atomics.notify(a, 0); await held; answer;';
    RunResult := Engine.Run('external-async.mjs', Source);
    Check(RunResult.Ok and (RunResult.ResultText = '42'), 'an async wait blocked host work: ' + RunResult.ErrorMessage);
    Engine.TimeoutMs := 0;

    if not ABare and (ParamCount = 1) then
    begin
      Engine.TimeoutMs := 5000;
      Source := 'const url = "' + UnicodeString(ParamStr(1)) + '/wave/";' +
        'const first = fetch(url + "first"); const answer = await hostPromise();' +
        'const rest = Array.from({ length: 15 }, (_, index) => fetch(url + index));' +
        'const responses = await Promise.all([first, ...rest]);' +
        'responses.every(response => response.status === 200) ? answer : 0;';
      RunResult := Engine.Run('external-fetch.mjs', Source);
      Check(RunResult.Ok and (RunResult.ResultText = '42'), 'fetch blocked host work: ' + RunResult.ErrorMessage);
      Work.Stall := True;
      Source := 'const done = hostPromise(); fetch("' + UnicodeString(ParamStr(1)) +
        '/sleep/30/signal", { signal: AbortSignal.timeout(2000) }).then(hostReady); await done;';
      RunResult := Engine.Run('external-fetch-signal.mjs', Source);
      Check(RunResult.Ok and (RunResult.ResultText = '42'), 'fetch timed out before its wall deadline: ' + RunResult.ErrorMessage);
      Work.Stall := False;
      Work.Ready := False;
      Engine.TimeoutMs := 0;
    end;

    RunResult := Engine.Run('external-throw.mjs', 'hostPromise(); throw Error("stop");');
    Check(not RunResult.Ok and (Work.Discards = 1) and not Work.HasPending, 'throw left host work behind');

    Work.Stall := True;
    Engine.TimeoutMs := 100;
    RunResult := Engine.Run('external-timeout.mjs', 'await hostPromise();');
    Check((RunResult.Aborted = gaTimeout) and (Work.Discards = 2) and not Work.HasPending,
      'deadline left host work behind');

    Work.Stall := False;
    Work.FailOnWait := True;
    Engine.TimeoutMs := 30;
    RunResult := Engine.Run('external-failed-pump.mjs', 'await hostPromise();');
    Check((RunResult.Aborted = gaTimeout) and (Work.Discards = 3) and not Work.HasPending,
      'a pump error masked the deadline');

    Work.FailOnWait := False;
    Engine.TimeoutMs := 0;
    RunResult := Engine.Run('external-reuse.mjs', 'await hostPromise();');
    Check(RunResult.Ok and (RunResult.ResultText = '42') and (Work.Discards = 3),
      'the next run could not deliver new host work: ' + RunResult.ErrorMessage);

    { host environments that start host work release it when the run fails: one that throws, and one that loads
      before an entry that does not compile (a bare engine has no module content provider, so it cannot load one) }
    if not ABare then
    begin
      ProviderDir := IncludeTrailingPathDelimiter(GetTempDir(False)) + 'goccia-host-work-' + IntToStr(GetProcessID) +
        '-' + IntToStr(GetTickCount64);
      Check(ForceDirectories(ProviderDir), 'could not create ' + UnicodeString(ProviderDir));
      try
        CheckFailedProvider(Engine, ProviderDir + PathDelim + 'failing.mjs',
          'hostPromise(); throw new Error("provider failed");', '42;', 'a failed host environment', '');
        CheckFailedProvider(Engine, ProviderDir + PathDelim + 'loaded.mjs', 'hostPromise();' +
          'export const epochNanoseconds = () => 1700000000000000000n; export const monotonicNanoseconds = () => 0n;' +
          'export const timeZoneIdentifier = () => "UTC"; export const random = () => 0.25;', 'const = ;',
          'an entry that did not compile after its host environment loaded', 'SyntaxError');
      finally
        DeleteFile(ProviderDir + PathDelim + 'failing.mjs');
        DeleteFile(ProviderDir + PathDelim + 'loaded.mjs');
        RemoveDir(ProviderDir);
      end;
    end;

    if not ABare then
    begin
      { a pump that runs guest code without queuing a job (a thenable's getter) can make a timer runnable while
        only a parked interval waited: the timer still fires, under an instruction limit too }
      Engine.MaxInstructions := 1000000;
      Engine.TimeoutMs := 2000;
      Work.Stall := True;
      Work.FirstAfterTurns := 3;
      RunResult := Engine.Run('external-pump-getter.mjs',
        'const parked = setInterval(() => {}, 1000);' +
        'hostFirstValue({ get then() { setTimeout(() => hostReady(), 0); return undefined; } });' +
        'hostPromise(); const value = await hostPromise(); clearInterval(parked); value;');
      Check(RunResult.Ok and (RunResult.ResultText = '42'),
        'a timer scheduled by guest code in a pump did not fire: ' + RunResult.ErrorMessage);
      Work.Stall := False;
      Work.Ready := False;
      Engine.MaxInstructions := 0;
      Engine.TimeoutMs := 0;
      FirstValue := VUndefined;
    end;

    { a pump that settles one request and then fails: that request's reaction does not run in the next run }
    Discards := Work.Discards;
    Work.SettleThenFail := True;
    RunResult := Engine.Run('external-settle-then-fail.mjs',
      'globalThis.leaked = false; hostPromise().then(() => { globalThis.leaked = true; }); await hostPromise();');
    Work.SettleThenFail := False;
    Check(not RunResult.Ok and (Work.Discards = Discards + 1) and not Work.HasPending,
      'a failed pump left host work behind: ' + RunResult.ErrorMessage);
    { a leaked reaction was queued first, so it would run before this await resumes }
    RunResult := Engine.Run('external-after-settle-then-fail.mjs', 'await null; String(globalThis.leaked);');
    Check(RunResult.Ok and (RunResult.ResultText = 'false'),
      'a reaction from a failed pump ran in the next run: ' + RunResult.ResultText + RunResult.ErrorMessage);

    Engine.SetHostWork(nil);
    FreeAndNil(Work);
    CollectMinor;
    CollectMajor;
    RunResult := Engine.Run('external-detached.mjs', '42;');
    Check(RunResult.Ok and (RunResult.ResultText = '42'), 'collecting after detach damaged the engine');
  finally
    Engine.Free;
    FreeAndNil(Work);
  end;
end;

function NewTestEngine(var APromiseSlots: TPromiseSlots): TGocciaEngine;
var
  ErrorText: UnicodeString;
begin
  APromiseSlots[0] := VUndefined;
  APromiseSlots[1] := VUndefined;
  Work := TTestWork.Create(@APromiseSlots);
  Result := TGocciaEngine.Create;
  Result.TestMode := True;
  Result.TimeoutMs := 5000;
  Result.SetHostWork(Work);
  Check(Result.PrepareHost(ErrorText), ErrorText);
  Check(InstallTestingAPI(ErrorText), ErrorText);
  AddRootRange(@APromiseSlots[0], Length(APromiseSlots));
  InstallHostNatives;
end;

{ A timed-out test drops only the host work it started. }
procedure CheckTestTimeout;
var
  Engine: TGocciaEngine;
  Json, Err: UnicodeString;
  Aborted: TGocciaAbort;
  RunResult: TGocciaRunResult;
  PromiseSlots: TPromiseSlots;
begin
  Engine := nil;
  TestTimeoutMs := 50;
  try
    Engine := NewTestEngine(PromiseSlots);
    Work.Stall := True;
    RunResult := Engine.Run('host-work-timeout.test.js',
      'let outer; beforeAll(() => { outer = hostPromise(); });' +
      'test("times out", async () => { await hostPromise(); });' +
      'test("keeps earlier work", async () => { hostReady(); expect(await outer).toBe(42); });');
    Check(RunResult.Ok, 'test file did not load: ' + RunResult.ErrorMessage);
    Json := '';
    Err := '';
    Aborted := gaNone;
    Engine.RunTests(Json, Err, Aborted);
    Check((Aborted = gaNone) and (Pos('"passed":1', Json) > 0) and (Pos('"failed":1', Json) > 0) and
      (Work.ScopedDiscards = 1) and (Work.Discards = 0) and not Work.HasPending,
      'a timed-out test did not drop only its own host work: ' + Json + Err);
  finally
    TestTimeoutMs := 0;
    Engine.Free;
    FreeAndNil(Work);
  end;
end;

{ A host that cannot prepare releases the work its source already holds. }
procedure CheckFailedPrepare;
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
  PromiseSlots: TPromiseSlots;
begin
  PromiseSlots[0] := VUndefined;
  PromiseSlots[1] := VUndefined;
  Work := TTestWork.Create(@PromiseSlots);
  Engine := TGocciaEngine.Create;
  try
    Engine.ConfigJson := '{ not json';
    Engine.SetHostWork(Work);
    Work.ForcePending := True;
    RunResult := Engine.Run('external-unprepared.mjs', '42;');
    Check(not RunResult.Ok and (Work.Discards = 1) and not Work.HasPending,
      'a host that failed to prepare kept host work: ' + RunResult.ErrorMessage);
  finally
    Engine.Free;
    FreeAndNil(Work);
  end;
end;

{ A result that arrives only after a test's deadline does not resume the timed-out test. }
procedure CheckLateDelivery;
var
  Engine: TGocciaEngine;
  Json, Err: UnicodeString;
  Aborted: TGocciaAbort;
  RunResult: TGocciaRunResult;
  PromiseSlots: TPromiseSlots;
begin
  Engine := nil;
  TestTimeoutMs := 50;
  try
    Engine := NewTestEngine(PromiseSlots);
    Work.Late := True;
    RunResult := Engine.Run('host-work-late.test.js',
      'let late = false;' +
      'test("times out", async () => { await hostPromise(); late = true; });' +
      'test("sees no late reaction", () => { expect(late).toBe(false); });');
    Check(RunResult.Ok, 'test file did not load: ' + RunResult.ErrorMessage);
    Json := '';
    Err := '';
    Aborted := gaNone;
    Engine.RunTests(Json, Err, Aborted);
    Check((Aborted = gaNone) and (Pos('"passed":1', Json) > 0) and (Pos('"failed":1', Json) > 0) and
      (Pos('TIMEOUT', Json) > 0) and not Work.HasPending,
      'a result delivered after the deadline resumed the timed-out test: ' + Json + Err);
  finally
    TestTimeoutMs := 0;
    Engine.Free;
    FreeAndNil(Work);
  end;
end;

{ Suites that end in an error without an abort release their host work. }
procedure CheckFailedSuite;
var
  Engine: TGocciaEngine;
  Json, Err: UnicodeString;
  Aborted: TGocciaAbort;
  RunResult: TGocciaRunResult;
  PromiseSlots: TPromiseSlots;
  Ok: Boolean;
begin
  Engine := nil;
  try
    Engine := NewTestEngine(PromiseSlots);
    Work.FailOnWait := True;
    RunResult := Engine.Run('host-work-failure.test.js', 'test("fails", async () => { await hostPromise(); });');
    Check(RunResult.Ok, 'failing test file did not load: ' + RunResult.ErrorMessage);
    Json := '';
    Err := '';
    Aborted := gaNone;
    Ok := Engine.RunTests(Json, Err, Aborted);
    Check(not Ok and (Aborted = gaNone) and (Work.Discards = 1) and not Work.HasPending,
      'a failed suite left host work behind: ' + Json + Err + ' ok=' + BoolToStr(Ok, True) + ' aborted=' +
      IntToStr(Ord(Aborted)) + ' discards=' + IntToStr(Work.Discards) + ' pending=' + BoolToStr(Work.HasPending, True));
  finally
    Engine.Free;
    FreeAndNil(Work);
  end;
end;

begin
  FirstValue := VUndefined;
  CheckRuns(False);
  CheckRuns(True);
  CheckFailedPrepare;
  CheckTestTimeout;
  CheckLateDelivery;
  CheckFailedSuite;
  WriteLn('Embedder host work: passed');
end.
