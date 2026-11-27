program RunAbortTest;

{$mode delphiunicode}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,
  Goccia.Capabilities,
  Goccia.Core,
  Goccia.Embed;

type
  TAbortRequester = class(TThread)
  private
    FGate: PRTLEvent;
    FHandle: IGocciaAbortHandle;
    FRequestedAt: QWord;
    FReadyFile: string;
  protected
    procedure Execute; override;
  public
    constructor Create(const AHandle: IGocciaAbortHandle; const AReadyFile: string = '');
    destructor Destroy; override;
    procedure Console(const AMethod, ALine: UnicodeString);
    property RequestedAt: QWord read FRequestedAt;
  end;

constructor TAbortRequester.Create(const AHandle: IGocciaAbortHandle; const AReadyFile: string);
begin
  inherited Create(True);
  FHandle := AHandle;
  FReadyFile := AReadyFile;
  FGate := RTLEventCreate;
  Start;
end;

destructor TAbortRequester.Destroy;
begin
  RTLEventSetEvent(FGate);
  WaitFor;
  RTLEventDestroy(FGate);
  inherited Destroy;
end;

procedure TAbortRequester.Execute;
var Deadline: QWord;
begin
  RTLEventWaitFor(FGate);
  if FReadyFile = '' then Sleep(25)
  else
  begin
    Deadline := GetTickCount64 + 3000;
    while not FileExists(FReadyFile) and (GetTickCount64 < Deadline) do Sleep(5);
  end;
  FRequestedAt := GetTickCount64;
  FHandle.Request;
end;

procedure TAbortRequester.Console(const AMethod, ALine: UnicodeString);
begin
  if ALine = 'ready' then RTLEventSetEvent(FGate)
  else raise Exception.Create('Unexpected script output: ' + ALine);
end;

procedure Check(ACondition: Boolean; const AMessage: UnicodeString);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

procedure CheckActiveAbort(AEngine: TGocciaEngine; const ASource: UnicodeString; const AReadyFile: string = '');
var
  Requester: TAbortRequester;
  RunResult: TGocciaRunResult;
  FinishedAt: QWord;
begin
  Requester := TAbortRequester.Create(AEngine.CaptureAbortHandle, AReadyFile);
  try
    AEngine.OnConsole := Requester.Console;
    RunResult := AEngine.Run('abort.js', ASource);
    FinishedAt := GetTickCount64;
    Check(RunResult.Aborted = gaHostAbort, 'run was not cancelled: ' + RunResult.ErrorMessage);
    Requester.WaitFor;
    Check(FinishedAt - Requester.RequestedAt < 600, 'the engine did not wake promptly');
    Check((AReadyFile = '') or FileExists(AReadyFile), 'the request never reached the HTTP server');
  finally
    AEngine.OnConsole := nil;
    Requester.Free;
  end;
  RunResult := AEngine.Run('reuse.js', '21 * 2;');
  Check(RunResult.Ok and (RunResult.ResultText = '42'), 'cancelled engine could not be reused');
end;

procedure CheckLateAbort(AEngine: TGocciaEngine);
var
  OldHandle: IGocciaAbortHandle;
  Requester: TAbortRequester;
  RunResult: TGocciaRunResult;
begin
  OldHandle := AEngine.CaptureAbortHandle;
  RunResult := AEngine.Run('prior.js', '1;');
  Check(RunResult.Ok, 'first run failed');
  Requester := TAbortRequester.Create(OldHandle);
  try
    AEngine.OnConsole := Requester.Console;
    RunResult := AEngine.Run('next.mjs',
      'console.log("ready"); await Atomics.waitAsync(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 150).value; 42;');
    Check(RunResult.Ok and (RunResult.ResultText = '42'), 'late cancellation reached a later run');
  finally
    AEngine.OnConsole := nil;
    Requester.Free;
  end;
end;

var
  Engine: TGocciaEngine;
  Profile: TLangProfile;
  Handle: IGocciaAbortHandle;
  RunResult: TGocciaRunResult;
  BaseUrl: UnicodeString;
begin
  Engine := TGocciaEngine.Create;
  try
    Profile := Engine.Profile;
    Profile.WhileLoop := True;
    Engine.Profile := Profile;
    Engine.ResultForm := grfString;
    if ParamCount = 2 then Engine.Capabilities := TGocciaCapabilities.None.Allow(gcNet, '127.0.0.1');
    Handle := Engine.CaptureAbortHandle;
    Handle.Request;
    RunResult := Engine.Run('pre-cancel.js', 'throw new Error("cancelled code ran");');
    Check((RunResult.Aborted = gaHostAbort) and not RunResult.Compiled, 'pre-start cancellation compiled the source');
    CheckActiveAbort(Engine,
      'console.log("ready"); try { while (true) {} } catch { console.log("caught"); } finally { console.log("finally"); }');
    CheckActiveAbort(Engine,
      'console.log("ready"); Atomics.waitAsync(new Int32Array(new SharedArrayBuffer(4)), 0, 0).value.then(() => console.log("settled"));');
    CheckActiveAbort(Engine,
      'console.log("ready"); Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0);');
    if ParamCount = 2 then
    begin
      BaseUrl := ParamStr(1);
      CheckActiveAbort(Engine, 'const request = fetch("' + BaseUrl + '/sleep/1500/abort"); console.log("ready");',
        ParamStr(2));
      RunResult := Engine.Run('fetch-reuse.mjs', '(await (await fetch("' + BaseUrl + '/text")).text());');
      Check(RunResult.Ok and (RunResult.ResultText = 'hello world'), 'cancelled engine could not fetch again');
    end;
    CheckLateAbort(Engine);
  finally
    Engine.Free;
  end;
  Handle.Request;
  WriteLn('Host run abort tests passed');
end.
