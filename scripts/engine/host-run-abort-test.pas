program RunAbortTest;

{$mode delphiunicode}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,
  Goccia.Capabilities,
  Goccia.Core,
  Goccia.Embed,
  Goccia.VM;

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

var
  { the handle guest code cancels its own run through, and the requests still to make once }
  CurrentAbort: IGocciaAbortHandle;
  AbortAtTop, AbortInTimeZone: Boolean;
  TimeZoneCalls: Integer;

function RequestAbort(AArgs: PValue; AArgc: Integer; var AResult: TValue): Boolean;
begin
  if CurrentAbort <> nil then CurrentAbort.Request;
  AResult := VUndefined;
  Result := True;
end;

function RequestAbortAtTop(AArgs: PValue; AArgc: Integer; var AResult: TValue): Boolean;
begin
  if AbortAtTop and (CurrentAbort <> nil) then CurrentAbort.Request;
  AbortAtTop := False;
  AResult := VUndefined;
  Result := True;
end;

function RequestAbortInTimeZone(AArgs: PValue; AArgc: Integer; var AResult: TValue): Boolean;
begin
  Inc(TimeZoneCalls);
  if AbortInTimeZone and (CurrentAbort <> nil) then CurrentAbort.Request;
  AbortInTimeZone := False;
  AResult := VUndefined;
  Result := True;
end;

procedure InstallAbortNatives(AEngine: TGocciaEngine);
var ErrorText: UnicodeString;
begin
  Check(AEngine.PrepareHost(ErrorText), ErrorText);
  Check(InjectGlobalBinding('requestAbort', NewNativeFunction(@RequestAbort, 'requestAbort', 0)) and
    InjectGlobalBinding('requestAbortAtTop', NewNativeFunction(@RequestAbortAtTop, 'requestAbortAtTop', 0)) and
    InjectGlobalBinding('requestAbortInTimeZone',
      NewNativeFunction(@RequestAbortInTimeZone, 'requestAbortInTimeZone', 0)), 'could not install abort natives');
end;

{ A request while the completion value converts: the run is cancelled and has no value. }
procedure CheckAbortDuringResult(AEngine: TGocciaEngine);
var RunResult: TGocciaRunResult;
begin
  CurrentAbort := AEngine.CaptureAbortHandle;
  RunResult := AEngine.Run('result-abort.js', '({ toString() { requestAbort(); return "late"; } });');
  CurrentAbort := nil;
  Check((RunResult.Aborted = gaHostAbort) and not RunResult.HasResult and (RunResult.ResultText = ''),
    'a cancelled run kept its completion value: ' + RunResult.ResultText);
end;

{ A cancelled host environment setup loads again: at the provider's top level, then inside its time zone call. }
procedure CheckProviderAbort;
var
  Engine: TGocciaEngine;
  Provider: TStringList;
  Directory, ProviderPath: string;
  RunResult: TGocciaRunResult;
begin
  Directory := IncludeTrailingPathDelimiter(GetTempDir(False)) + 'goccia-run-abort-' + IntToStr(GetProcessID) + '-' +
    IntToStr(GetTickCount64);
  Check(ForceDirectories(Directory), 'could not create ' + UnicodeString(Directory));
  ProviderPath := Directory + PathDelim + 'provider.mjs';
  Provider := TStringList.Create;
  Engine := TGocciaEngine.Create;
  try
    Provider.Text := 'const step = () => 0; requestAbortAtTop(); step();' +
      'export const epochNanoseconds = () => 1700000000000000000n; export const monotonicNanoseconds = () => 0n;' +
      'export const timeZoneIdentifier = () => { requestAbortInTimeZone(); step(); return "Asia/Tokyo"; };' +
      'export const random = () => 0.25;';
    Provider.SaveToFile(ProviderPath);
    Engine.ResultForm := grfString;
    InstallAbortNatives(Engine);
    Engine.HostEnvironment := UnicodeString(ProviderPath);
    TimeZoneCalls := 0;
    AbortAtTop := True;
    CurrentAbort := Engine.CaptureAbortHandle;
    RunResult := Engine.Run('provider-top.js', '1;');
    Check((RunResult.Aborted = gaHostAbort) and (TimeZoneCalls = 0),
      'cancelling the provider at its top level was not reported: ' + RunResult.ErrorMessage);
    AbortInTimeZone := True;
    CurrentAbort := Engine.CaptureAbortHandle;
    RunResult := Engine.Run('provider-zone.js', '1;');
    Check((RunResult.Aborted = gaHostAbort) and (TimeZoneCalls = 1),
      'a provider cancelled at its top level did not load again: ' + RunResult.ErrorMessage);
    CurrentAbort := nil;
    RunResult := Engine.Run('provider-loaded.js', 'Temporal.Now.timeZoneId();');
    Check(RunResult.Ok and (RunResult.ResultText = 'Asia/Tokyo') and (TimeZoneCalls = 2),
      'a provider cancelled during its setup did not finish it: ' + RunResult.ResultText + RunResult.ErrorMessage);
  finally
    CurrentAbort := nil;
    Engine.Free;
    Provider.Free;
    DeleteFile(ProviderPath);
    RemoveDir(Directory);
  end;
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

type
  { requests the old handle when the next run says it is ready, and tells that run once the request was made }
  TStaleRequester = class(TThread)
  private
    FGate, FDone: PRTLEvent;
    FHandle: IGocciaAbortHandle;
  protected
    procedure Execute; override;
  public
    Acknowledged: Boolean;
    constructor Create(const AHandle: IGocciaAbortHandle);
    destructor Destroy; override;
    procedure Console(const AMethod, ALine: UnicodeString);
  end;

constructor TStaleRequester.Create(const AHandle: IGocciaAbortHandle);
begin
  inherited Create(True);
  FHandle := AHandle;
  FGate := RTLEventCreate;
  FDone := RTLEventCreate;
  Start;
end;

destructor TStaleRequester.Destroy;
begin
  RTLEventSetEvent(FGate);
  WaitFor;
  RTLEventDestroy(FGate);
  RTLEventDestroy(FDone);
  inherited Destroy;
end;

procedure TStaleRequester.Execute;
begin
  RTLEventWaitFor(FGate);
  FHandle.Request;
  RTLEventSetEvent(FDone);
end;

procedure TStaleRequester.Console(const AMethod, ALine: UnicodeString);
begin
  if ALine <> 'ready' then raise Exception.Create('Unexpected script output: ' + ALine);
  RTLEventSetEvent(FGate);
  { the run goes on only once the old handle's request was made, so the request overlaps it }
  RTLEventWaitFor(FDone, 3000);
  Acknowledged := True;
end;

procedure CheckLateAbort(AEngine: TGocciaEngine);
var
  OldHandle: IGocciaAbortHandle;
  Requester: TStaleRequester;
  RunResult: TGocciaRunResult;
begin
  OldHandle := AEngine.CaptureAbortHandle;
  RunResult := AEngine.Run('prior.js', '1;');
  Check(RunResult.Ok, 'first run failed');
  Requester := TStaleRequester.Create(OldHandle);
  try
    AEngine.OnConsole := Requester.Console;
    { the next run has a handle of its own, which nothing requests }
    AEngine.CaptureAbortHandle;
    RunResult := AEngine.Run('next.mjs', 'console.log("ready"); let n = 0; for (const i of [1, 2, 3]) n += i; n * 7;');
    Check(Requester.Acknowledged and RunResult.Ok and (RunResult.ResultText = '42'),
      'late cancellation reached a later run: ' + RunResult.ErrorMessage);
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
    InstallAbortNatives(Engine);
    CheckAbortDuringResult(Engine);
    RunResult := Engine.Run('reuse-after-checks.js', '21 * 2;');
    Check(RunResult.Ok and (RunResult.ResultText = '42'), 'cancelled engine could not be reused');
  finally
    Engine.Free;
  end;
  Handle.Request;
  CheckProviderAbort;
  WriteLn('Host run abort tests passed');
end.
