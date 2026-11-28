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

var
  TouchPath: string;

{ marks TouchPath modified two seconds ahead, so the next import of it loads it again (ADR 0139) }
function TouchModule(AArgs: PValue; AArgc: Integer; var AResult: TValue): Boolean;
begin
  FileSetDate(TouchPath, DateTimeToFileDate(Now + 2 / SecsPerDay));
  AResult := VUndefined;
  Result := True;
end;

{ waits on an event nobody sets, through WaitForRunEvent, for longer than CheckActiveAbort allows: only the abort
  handle's wake ends the wait in time, not a polling interval of the code that waits }
function WaitForRunEventNative(AArgs: PValue; AArgc: Integer; var AResult: TValue): Boolean;
var Event: PRTLEvent;
begin
  Event := RTLEventCreate;
  try
    WaitForRunEvent(Event, 5000);
  finally
    RTLEventDestroy(Event);
  end;
  AResult := VUndefined;
  Result := True;
end;

function RequestAbort(AArgs: PValue; AArgc: Integer; var AResult: TValue): Boolean;
begin
  if CurrentAbort <> nil then CurrentAbort.Request;
  AResult := VUndefined;
  Result := True;
end;

var
  SecondNativeEntered: Boolean;

function MarkEntered(AArgs: PValue; AArgc: Integer; var AResult: TValue): Boolean;
begin
  SecondNativeEntered := True;
  AResult := VUndefined;
  Result := True;
end;

function RequestAbortAndThrow(AArgs: PValue; AArgc: Integer; var AResult: TValue): Boolean;
begin
  if CurrentAbort <> nil then CurrentAbort.Request;
  Result := ThrowTypeError('native failure after the request');
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
    InjectGlobalBinding('requestAbortAndThrow', NewNativeFunction(@RequestAbortAndThrow, 'requestAbortAndThrow', 0)) and
    InjectGlobalBinding('markEntered', NewNativeFunction(@MarkEntered, 'markEntered', 0)) and
    InjectGlobalBinding('waitForRunEvent', NewNativeFunction(@WaitForRunEventNative, 'waitForRunEvent', 0)) and
    InjectGlobalBinding('touchModule', NewNativeFunction(@TouchModule, 'touchModule', 0)) and
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

{ A run cancelled before it started leaves what earlier runs left: a FinalizationRegistry cell still cleans up. }
procedure CheckPreStartKeepsEarlierWork(AEngine: TGocciaEngine);
var
  Handle: IGocciaAbortHandle;
  RunResult: TGocciaRunResult;
begin
  RunResult := AEngine.Run('weak-register.js',
    'globalThis.cleaned = false; globalThis.target = {};' +
    'globalThis.registry = new FinalizationRegistry(() => { globalThis.cleaned = true; });' +
    'registry.register(globalThis.target, 1); 1;');
  Check(RunResult.Ok, 'weak registration failed: ' + RunResult.ErrorMessage);
  Handle := AEngine.CaptureAbortHandle;
  Handle.Request;
  RunResult := AEngine.Run('weak-cancelled.js', '1;');
  Check(RunResult.Aborted = gaHostAbort, 'pre-start cancellation was not reported');
  RunResult := AEngine.Run('weak-release.js', 'globalThis.target = undefined; 1;');
  Check(RunResult.Ok, 'weak release failed: ' + RunResult.ErrorMessage);
  RunResult := AEngine.Run('weak-check.js', 'String(globalThis.cleaned);');
  Check(RunResult.Ok and (RunResult.ResultText = 'true'),
    'a pre-start cancellation dropped an earlier run''s cleanup: ' + RunResult.ResultText + RunResult.ErrorMessage);
end;

{ A native that requests the cancellation and then throws does not hand its error to a guest catch. }
procedure CheckNativeRequestThenThrow(AEngine: TGocciaEngine);
var RunResult: TGocciaRunResult;
begin
  CurrentAbort := AEngine.CaptureAbortHandle;
  RunResult := AEngine.Run('request-then-throw.js',
    'globalThis.caught = false; try { requestAbortAndThrow(); } catch { globalThis.caught = true; } 1;');
  CurrentAbort := nil;
  Check(RunResult.Aborted = gaHostAbort, 'a native request then error was not a cancellation: ' + RunResult.ErrorMessage);
  RunResult := AEngine.Run('after-request-then-throw.js', 'String(globalThis.caught);');
  Check(RunResult.Ok and (RunResult.ResultText = 'false'), 'a guest catch ran after the host cancelled the run');
end;

{ A native that requests the cancellation and returns normally: the run ends before the next native starts, whether
  the native was called directly, through Function.prototype.call or as a bound function. }
procedure CheckNativeRequestThenReturn(AEngine: TGocciaEngine);
const
  Calls: array[0..2] of UnicodeString = ('requestAbort()', 'requestAbort.call(undefined)',
    'requestAbort.bind(undefined)()');
var RunResult: TGocciaRunResult; I: Integer;
begin
  for I := Low(Calls) to High(Calls) do
  begin
    SecondNativeEntered := False;
    CurrentAbort := AEngine.CaptureAbortHandle;
    RunResult := AEngine.Run('request-then-return.js', Calls[I] + '; markEntered(); 1;');
    CurrentAbort := nil;
    Check((RunResult.Aborted = gaHostAbort) and not SecondNativeEntered,
      'a native ran after the host cancelled the run in ' + Calls[I] + ': ' + RunResult.ErrorMessage);
  end;
end;

procedure WriteModule(const ADirectory, AName: string; const ASource: UnicodeString);
var Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := ASource;
    Lines.SaveToFile(ADirectory + PathDelim + AName);
  finally
    Lines.Free;
  end;
end;

{ A request that wakes an Atomics.waitAsync wait whose entry spent exactly its instruction budget: the run is
  cancelled, not ended at the limit, since no charge failed before the request. The budgets rise from 1: while the
  run ends at the limit before any request, the entry did not fit; the first one that waits is the entry's cost. }
procedure CheckCancelAtSpentBudget;
const
  Source = 'Atomics.waitAsync(new Int32Array(new SharedArrayBuffer(4)), 0, 0); console.log("ready");';
var
  Engine: TGocciaEngine;
  Requester: TAbortRequester;
  RunResult: TGocciaRunResult;
  Budget: Integer;
  Requested: Boolean;
begin
  for Budget := 1 to 1000 do
  begin
    Engine := TGocciaEngine.Create;
    try
      Engine.MaxInstructions := Budget;
      Requester := TAbortRequester.Create(Engine.CaptureAbortHandle);
      try
        Engine.OnConsole := Requester.Console;
        RunResult := Engine.Run('spent-budget.js', Source);
        Requested := Requester.RequestedAt <> 0;
      finally
        Engine.OnConsole := nil;
        Requester.Free;
      end;
    finally
      Engine.Free;
    end;
    if (RunResult.Aborted = gaInstructionLimit) and not Requested then Continue;
    Check(RunResult.Aborted = gaHostAbort, 'a request at a spent budget ended the run as ' +
      IntToStr(Ord(RunResult.Aborted)) + ' (budget ' + IntToStr(Budget) + '): ' + RunResult.ErrorMessage);
    Exit;
  end;
  Check(False, 'no budget let the entry finish');
end;

{ A module the cancellation interrupted evaluates again when the same path runs next, as do its imports. }
procedure CheckCancelledModuleReloads;
var
  AEngine: TGocciaEngine;
  Directory, Dependency, SourceDependency: string;
  Lines: TStringList;
  RunResult: TGocciaRunResult;
begin
  { an engine of its own, with the default capabilities, so it reads its module files }
  AEngine := TGocciaEngine.Create;
  AEngine.ResultForm := grfString;
  InstallAbortNatives(AEngine);
  Directory := IncludeTrailingPathDelimiter(GetTempDir(False)) + 'goccia-run-abort-module-' + IntToStr(GetProcessID) +
    '-' + IntToStr(GetTickCount64);
  Check(ForceDirectories(Directory), 'could not create ' + UnicodeString(Directory));
  Dependency := Directory + PathDelim + 'dependency.mjs';
  Lines := TStringList.Create;
  try
    Lines.Text := 'if (globalThis.cancelDependency) requestAbort(); const step = () => 0; step(); export const ready = 7;';
    Lines.SaveToFile(Dependency);
    CurrentAbort := AEngine.CaptureAbortHandle;
    RunResult := AEngine.Run(Directory + PathDelim + 'entry.mjs',
      'globalThis.cancelDependency = true; const { ready } = await import("./dependency.mjs"); ready;');
    CurrentAbort := nil;
    Check(RunResult.Aborted = gaHostAbort, 'the dependency''s cancellation was not reported: ' + RunResult.ErrorMessage);
    RunResult := AEngine.Run(Directory + PathDelim + 'entry.mjs',
      'globalThis.cancelDependency = false; const { ready } = await import("./dependency.mjs"); ready;');
    Check(RunResult.Ok and (RunResult.ResultText = '7'),
      'a module the cancellation interrupted did not evaluate again: ' + RunResult.ResultText + RunResult.ErrorMessage);
    { a source phase import the cancelled run finished keeps its module source: the next import of the unchanged file
      gives the same object }
    AEngine.JSModuleSources := True;
    SourceDependency := Directory + PathDelim + 'source-dependency.mjs';
    Lines.Text := 'export const value = 1;';
    Lines.SaveToFile(SourceDependency);
    CurrentAbort := AEngine.CaptureAbortHandle;
    RunResult := AEngine.Run(Directory + PathDelim + 'source-entry.mjs',
      'globalThis.saved = await import.source("./source-dependency.mjs"); requestAbort(); markEntered(); 1;');
    CurrentAbort := nil;
    Check(RunResult.Aborted = gaHostAbort, 'the source phase run was not cancelled: ' + RunResult.ErrorMessage);
    RunResult := AEngine.Run(Directory + PathDelim + 'source-check.mjs',
      '(await import.source("./source-dependency.mjs")) === globalThis.saved;');
    Check(RunResult.Ok and (RunResult.ResultText = 'true'),
      'a cancelled run''s source phase import lost its identity: ' + RunResult.ResultText + RunResult.ErrorMessage);
    { a record only loaded for its source (cycle-c) links a member (cycle-b) of a cycle whose root (cycle-a) the
      cancellation left suspended in top-level await: the member goes with its root, and so does the record that
      links it, so a later import of it evaluates afresh }
    WriteModule(Directory, 'cycle-a.mjs', 'import "./cycle-b.mjs"; ' +
      'await new Promise((resolve) => setTimeout(() => { requestAbort(); resolve(); }, 0)); export const a = 1;');
    WriteModule(Directory, 'cycle-b.mjs', 'import "./cycle-a.mjs"; export const b = 2;');
    WriteModule(Directory, 'cycle-c.mjs', 'import { b } from "./cycle-b.mjs"; export const c = b + 1;');
    CurrentAbort := AEngine.CaptureAbortHandle;
    RunResult := AEngine.Run(Directory + PathDelim + 'cycle-entry.mjs',
      'await import.source("./cycle-c.mjs"); await import("./cycle-a.mjs"); 1;');
    CurrentAbort := nil;
    Check(RunResult.Aborted = gaHostAbort, 'the cycle run was not cancelled: ' + RunResult.ErrorMessage);
    RunResult := AEngine.Run(Directory + PathDelim + 'cycle-check.mjs',
      'const { c } = await import("./cycle-c.mjs"); c;');
    Check(RunResult.Ok and (RunResult.ResultText = '3'),
      'a record linking a cancelled cycle''s member did not evaluate again: ' + RunResult.ResultText +
      RunResult.ErrorMessage);
    { a module the cancelled run linked but never started (an import.defer namespace nobody read) stays: reading the
      saved namespace and importing the unchanged file evaluate it once }
    WriteModule(Directory, 'deferred.mjs', 'globalThis.deferredRuns = (globalThis.deferredRuns ?? 0) + 1; ' +
      'export const value = 5;');
    CurrentAbort := AEngine.CaptureAbortHandle;
    RunResult := AEngine.Run(Directory + PathDelim + 'deferred-entry.mjs',
      'globalThis.deferred = await import.defer("./deferred.mjs"); requestAbort(); markEntered(); 1;');
    CurrentAbort := nil;
    Check(RunResult.Aborted = gaHostAbort, 'the deferred run was not cancelled: ' + RunResult.ErrorMessage);
    RunResult := AEngine.Run(Directory + PathDelim + 'deferred-check.mjs',
      'const read = globalThis.deferred.value; const { value } = await import("./deferred.mjs"); ' +
      '`${read} ${value} runs=${globalThis.deferredRuns}`;');
    Check(RunResult.Ok and (RunResult.ResultText = '5 5 runs=1'),
      'a linked module the cancelled run never started ran twice: ' + RunResult.ResultText + RunResult.ErrorMessage);
    { a loaded-only record whose dependency an ordinary reload replaced (its file changed) earlier in the cancelled
      run keeps its source object: that dependency did not go with the cancellation }
    WriteModule(Directory, 'reloaded-dep.mjs', 'export const dep = 1;');
    WriteModule(Directory, 'reloaded-source.mjs', 'import { dep } from "./reloaded-dep.mjs"; export const value = dep;');
    TouchPath := Directory + PathDelim + 'reloaded-dep.mjs';
    CurrentAbort := AEngine.CaptureAbortHandle;
    RunResult := AEngine.Run(Directory + PathDelim + 'reloaded-entry.mjs',
      'globalThis.reloadedSource = await import.source("./reloaded-source.mjs"); touchModule(); ' +
      'await import("./reloaded-dep.mjs"); requestAbort(); markEntered(); 1;');
    CurrentAbort := nil;
    Check(RunResult.Aborted = gaHostAbort, 'the reload run was not cancelled: ' + RunResult.ErrorMessage);
    RunResult := AEngine.Run(Directory + PathDelim + 'reloaded-check.mjs',
      '(await import.source("./reloaded-source.mjs")) === globalThis.reloadedSource;');
    Check(RunResult.Ok and (RunResult.ResultText = 'true'),
      'a cancellation dropped a source whose dependency an ordinary reload replaced: ' + RunResult.ResultText +
      RunResult.ErrorMessage);
  finally
    CurrentAbort := nil;
    AEngine.Free;
    Lines.Free;
    DeleteFile(Dependency);
    if SourceDependency <> '' then DeleteFile(SourceDependency);
    DeleteFile(Directory + PathDelim + 'cycle-a.mjs');
    DeleteFile(Directory + PathDelim + 'cycle-b.mjs');
    DeleteFile(Directory + PathDelim + 'cycle-c.mjs');
    DeleteFile(Directory + PathDelim + 'deferred.mjs');
    DeleteFile(Directory + PathDelim + 'reloaded-dep.mjs');
    DeleteFile(Directory + PathDelim + 'reloaded-source.mjs');
    RemoveDir(Directory);
  end;
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
    { set by the worker once Request returned; read on the engine thread inside the run }
    RequestMade: LongInt;
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
  InterLockedExchange(RequestMade, 1);
  RTLEventSetEvent(FDone);
end;

procedure TStaleRequester.Console(const AMethod, ALine: UnicodeString);
begin
  if ALine <> 'ready' then raise Exception.Create('Unexpected script output: ' + ALine);
  RTLEventSetEvent(FGate);
  { the run goes on only once the old handle's request was made, so the request overlaps it }
  RTLEventWaitFor(FDone, 3000);
  { only a request the worker made while this run waited proves the overlap }
  Acknowledged := InterLockedCompareExchange(RequestMade, 0, 0) = 1;
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
    CheckActiveAbort(Engine, 'console.log("ready"); waitForRunEvent(); markEntered();');
    CheckAbortDuringResult(Engine);
    CheckPreStartKeepsEarlierWork(Engine);
    CheckNativeRequestThenThrow(Engine);
    CheckNativeRequestThenReturn(Engine);
    RunResult := Engine.Run('reuse-after-checks.js', '21 * 2;');
    Check(RunResult.Ok and (RunResult.ResultText = '42'), 'cancelled engine could not be reused');
  finally
    Engine.Free;
  end;
  Handle.Request;
  CheckProviderAbort;
  CheckCancelledModuleReloads;
  CheckCancelAtSpentBudget;
  WriteLn('Host run abort tests passed');
end.
