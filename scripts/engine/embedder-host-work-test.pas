program EmbedderHostWorkTest;

{$mode delphiunicode}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils,
  Goccia.Capabilities,
  Goccia.Core,
  Goccia.Embed,
  Goccia.Promise,
  Goccia.VM;

type
  TTestWork = class(TGocciaEmbedderWork)
  public
    Promise: TValue;
    Stall: Boolean;
    Discards: Integer;
    function HasPending: Boolean; override;
    function Pump(ATimeoutMs: Cardinal): Boolean; override;
    procedure Discard; override;
  end;

var
  Work: TTestWork;

function BeginWork(AArgs: PValue; AArgc: Integer; var AResult: TValue): Boolean;
begin
  Work.Promise := NewPendingPromise;
  AResult := Work.Promise;
  Result := True;
end;

function TTestWork.HasPending: Boolean;
begin
  Result := Promise <> VUndefined;
end;

function TTestWork.Pump(ATimeoutMs: Cardinal): Boolean;
begin
  Result := True;
  if ATimeoutMs = 0 then Exit;
  if Stall then
  begin
    Sleep(ATimeoutMs);
    Exit;
  end;
  ResolvePendingPromise(Promise, MkInt(42));
  Promise := VUndefined;
end;

procedure TTestWork.Discard;
begin
  Inc(Discards);
  Promise := VUndefined;
end;

procedure Check(ACondition: Boolean; const AMessage: UnicodeString);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

procedure CheckRuns(ABare: Boolean);
var
  Engine: TGocciaEngine;
  ErrorText, Source: UnicodeString;
  RunResult: TGocciaRunResult;
begin
  Work := TTestWork.Create;
  Work.Promise := VUndefined;
  Engine := TGocciaEngine.Create;
  try
    Engine.Bare := ABare;
    Engine.ResultForm := grfString;
    if not ABare and (ParamCount = 1) then
      Engine.Capabilities := TGocciaCapabilities.None.Allow(gcNet, '127.0.0.1');
    Engine.SetHostWork(Work);
    Check(Engine.PrepareHost(ErrorText), ErrorText);
    AddRoot(@Work.Promise);
    Check(InjectGlobalBinding('hostPromise', NewNativeFunction(@BeginWork, 'hostPromise', 0)),
      'could not install hostPromise');

    if ABare then Source := 'await hostPromise();'
    else Source := 'let timer = false; setTimeout(() => { timer = true; }, 0); (await hostPromise()) + Number(timer);';
    RunResult := Engine.Run('external-ready.mjs', Source);
    Check(RunResult.Ok, RunResult.ErrorMessage);
    if ABare then Check(RunResult.ResultText = '42', 'host result is wrong')
    else Check(RunResult.ResultText = '43', 'the queued timer did not run before the wait');
    Check(not Work.HasPending and (Work.Discards = 0), 'successful work was not released normally');

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
    Engine.TimeoutMs := 0;
    RunResult := Engine.Run('external-reuse.mjs', 'await hostPromise();');
    Check(RunResult.Ok and (RunResult.ResultText = '42') and (Work.Discards = 2),
      'the next run could not deliver new host work: ' + RunResult.ErrorMessage);
  finally
    Engine.Free;
    FreeAndNil(Work);
  end;
end;

begin
  CheckRuns(False);
  CheckRuns(True);
  WriteLn('Embedder host work: passed');
end.
