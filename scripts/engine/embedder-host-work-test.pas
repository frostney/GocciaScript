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
    Stall, Ready, FailOnWait: Boolean;
    Discards, IdleTurns, ScopedDiscards: Integer;
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

function BeginWork(AArgs: PValue; AArgc: Integer; var AResult: TValue): Boolean;
begin
  if Work = nil then Exit(ThrowTypeError('host work is detached'));
  AResult := Work.BeginRequest;
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
  Result := (FSlots^[0] <> VUndefined) or (FSlots^[1] <> VUndefined);
end;

function TTestWork.Pump(ATimeoutMs: Cardinal): Boolean;
var I: Integer;
begin
  Result := True;
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
    if Stall then
    begin
      Sleep(ATimeoutMs);
      Exit;
    end;
  end;
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
end;

procedure CheckRuns(ABare: Boolean);
var
  Engine: TGocciaEngine;
  ErrorText, Source: UnicodeString;
  RunResult: TGocciaRunResult;
  PromiseSlots: TPromiseSlots;
  Provider: TStringList;
  ProviderPath: string;
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
    InstallHostNatives;

    if ABare then Source := 'await hostPromise();'
    else Source := 'let timer = false; setTimeout(() => { timer = true; }, 0); (await hostPromise()) + Number(timer);';
    RunResult := Engine.Run('external-ready.mjs', Source);
    Check(RunResult.Ok, RunResult.ErrorMessage);
    if ABare then Check(RunResult.ResultText = '42', 'host result is wrong')
    else Check(RunResult.ResultText = '43', 'the queued timer did not run before the wait');
    Check(not Work.HasPending and (Work.Discards = 0), 'successful work was not released normally');

    Engine.MaxInstructions := 100;
    Work.IdleTurns := 150;
    RunResult := Engine.Run('external-idle-budget.mjs', 'await hostPromise();');
    Check(RunResult.Ok and (RunResult.ResultText = '42') and (Work.IdleTurns = 0), 'idle pumps spent the cap: ' + RunResult.ErrorMessage);
    Engine.MaxInstructions := 0;

    Work.Ready := True;
    Work.IdleTurns := 1;
    RunResult := Engine.Run('external-nonblocking.mjs', 'await hostPromise();');
    Check(RunResult.Ok and (Work.IdleTurns = 1), 'ready host work needed a blocking wait');
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

    { a host environment that starts host work and then fails releases that work before the next run (a bare engine
      has no module content provider, so it cannot load one) }
    if not ABare then
    begin
      ProviderPath := ExtractFilePath(ExpandFileName(ParamStr(0))) + 'host-work-provider.mjs';
      Provider := TStringList.Create;
      try
        Provider.Text := 'hostPromise(); throw new Error("provider failed");';
        Provider.SaveToFile(ProviderPath);
      finally
        Provider.Free;
    end;
    Discards := Work.Discards;
    Engine.HostEnvironment := UnicodeString(ProviderPath);
    RunResult := Engine.Run('external-provider.mjs', '42;');
    DeleteFile(ProviderPath);
    Check(not RunResult.Ok and (Work.Discards = Discards + 1) and not Work.HasPending,
      'a failed host environment left host work behind: ' + RunResult.ErrorMessage);
    Engine.HostEnvironment := '';
    RunResult := Engine.Run('external-after-provider.mjs', 'await hostPromise();');
    Check(RunResult.Ok and (RunResult.ResultText = '42'),
      'the run after a failed host environment could not deliver host work: ' + RunResult.ErrorMessage);
    end;

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
  CheckRuns(False);
  CheckRuns(True);
  CheckTestTimeout;
  CheckFailedSuite;
  WriteLn('Embedder host work: passed');
end.
