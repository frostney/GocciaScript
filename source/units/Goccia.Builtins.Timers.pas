{ The JavaScript surface of the virtual timer queue.

  Two halves, both backed by the one queue in Goccia.Timers:

    - the `setTimeout` / `clearTimeout` / `setInterval` / `clearInterval`
      globals, and
    - the `goccia:timers` module, the low-level control surface the Vitest
      compatibility shim builds `vi.useFakeTimers` and the rest of the timer
      family on top of.

  `goccia:timers` deliberately speaks in numbers rather than in Vitest's
  shapes: `getMockedSystemTime` reports epoch milliseconds or `null`, and
  `setSystemTime` takes epoch milliseconds. Wrapping those in `Date` is the
  shim's job, which keeps the engine surface free of a dependency on the Date
  shim and keeps `goccia:timers` usable from a suite that never imports
  `vitest`.

  See docs/adr/0113-deterministic-virtual-timer-queue.md. }

unit Goccia.Builtins.Timers;

{$I Goccia.inc}

interface

uses
  Goccia.Scope,
  Goccia.Values.ObjectValue;

{ Returns the `goccia:timers` namespace. AHostToken receives an opaque handle
  for the per-namespace host state; pass it to ReleaseTimersHost when the
  registration that owns it goes away. }
function CreateTimersNamespace(out AHostToken: TObject): TGocciaObjectValue;

{ Binds setTimeout, clearTimeout, setInterval and clearInterval into AScope.
  AHostToken is shared with CreateTimersNamespace when both halves are
  installed by one extension. }
procedure RegisterTimerGlobals(const AScope: TGocciaScope;
  var AHostToken: TObject);

procedure ReleaseTimersHost(const AHostToken: TObject);
procedure ClearTimersHosts;

implementation

uses
  Generics.Collections,
  Math,
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.ThreadCleanupRegistry,
  Goccia.Timers,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.NativeFunctionCallback,
  Goccia.Values.Primitives;

const
  NOT_FAKED_MESSAGE =
    'A function to advance timers was called but the timers APIs are not ' +
    'mocked. Call `vi.useFakeTimers()` in the test file first.';
  CALLBACK_REQUIRED_MESSAGE =
    'The "callback" argument must be of type function.';
  CALLBACK_REQUIRED_SUGGESTION =
    'Pass a function as the first argument, as in setTimeout(() => {}, 0).';

type
  TGocciaTimersHostList = TObjectList<TObject>;

  { Method targets for the native functions. One instance per extension, held
    in the module-level list so a detached extension can release it. }
  TGocciaTimersHost = class
  private
    function Queue: TGocciaTimerQueue;
    function RequireFaking: TGocciaTimerQueue;
    function Schedule(const AKind: TGocciaTimerKind;
      const AArgs: TGocciaArgumentsCollection): TGocciaValue;
    function MillisecondArgument(
      const AArgs: TGocciaArgumentsCollection): Double;
  public
    function SetTimeoutCallback(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function SetIntervalCallback(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ClearTimerCallback(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    function UseFakeTimers(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function UseRealTimers(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function IsFakeTimers(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function SetSystemTime(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function GetMockedSystemTime(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function GetRealSystemTime(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AdvanceTimersByTime(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AdvanceTimersByTimeAsync(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AdvanceTimersToNextTimer(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AdvanceTimersToNextTimerAsync(
      const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RunAllTimers(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RunAllTimersAsync(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RunOnlyPendingTimers(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RunOnlyPendingTimersAsync(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ClearAllTimers(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function GetTimerCount(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

threadvar
  GTimersHosts: TGocciaTimersHostList;

function EnsureHost(var AHostToken: TObject): TGocciaTimersHost;
begin
  if AHostToken is TGocciaTimersHost then
    Exit(TGocciaTimersHost(AHostToken));

  Result := TGocciaTimersHost.Create;
  if not Assigned(GTimersHosts) then
    GTimersHosts := TGocciaTimersHostList.Create(True);
  GTimersHosts.Add(Result);
  AHostToken := Result;
end;

{ TGocciaTimersHost }

function TGocciaTimersHost.Queue: TGocciaTimerQueue;
begin
  TGocciaTimerQueue.Initialize;
  Result := TGocciaTimerQueue.Instance;
end;

function TGocciaTimersHost.RequireFaking: TGocciaTimerQueue;
begin
  Result := Queue;
  if not Result.Faking then
    ThrowError(NOT_FAKED_MESSAGE);
end;

function TGocciaTimersHost.MillisecondArgument(
  const AArgs: TGocciaArgumentsCollection): Double;
begin
  if AArgs.Length = 0 then
    Exit(0);
  Result := AArgs.GetElement(0).ToNumberLiteral.Value;
  if IsNan(Result) then
    Result := 0;
end;

function TGocciaTimersHost.Schedule(const AKind: TGocciaTimerKind;
  const AArgs: TGocciaArgumentsCollection): TGocciaValue;
var
  Callback: TGocciaValue;
  Delay: Double;
  ExtraArgs: TArray<TGocciaValue>;
  I: Integer;
begin
  if (AArgs.Length = 0) or not AArgs.GetElement(0).IsCallable then
    ThrowTypeError(CALLBACK_REQUIRED_MESSAGE, CALLBACK_REQUIRED_SUGGESTION);

  Callback := AArgs.GetElement(0);
  if AArgs.Length > 1 then
    Delay := AArgs.GetElement(1).ToNumberLiteral.Value
  else
    Delay := 0;

  SetLength(ExtraArgs, Max(0, AArgs.Length - 2));
  for I := 2 to AArgs.Length - 1 do
    ExtraArgs[I - 2] := AArgs.GetElement(I);

  Result := TGocciaNumberLiteralValue.Create(
    Queue.AddTimer(AKind, Callback, ExtraArgs, Delay));
end;

function TGocciaTimersHost.SetTimeoutCallback(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := Schedule(gtkTimeout, AArgs);
end;

function TGocciaTimersHost.SetIntervalCallback(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := Schedule(gtkInterval, AArgs);
end;

{ One implementation behind both clearTimeout and clearInterval. The fake clock
  lets either name clear either kind, and there is nothing a stricter rule
  would protect: the id space is shared. }
function TGocciaTimersHost.ClearTimerCallback(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if AArgs.Length = 0 then
    Exit;
  Queue.ClearTimer(AArgs.GetElement(0).ToNumberLiteral.Value);
end;

function TGocciaTimersHost.UseFakeTimers(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Now: Double;
  TimerQueue: TGocciaTimerQueue;
begin
  TimerQueue := Queue;
  { Vitest starts the fake clock at the date already in effect: the real time,
    or the frozen one a prior setSystemTime installed. }
  if (AArgs.Length > 0) and
     not (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
    Now := AArgs.GetElement(0).ToNumberLiteral.Value
  else if TimerQueue.Faking then
    Now := TimerQueue.NowMilliseconds
  else if TimerQueue.MockedDateOnly then
    Now := TimerQueue.MockedDate
  else
    Now := TimerQueue.RealEpochMilliseconds;
  if IsNan(Now) then
    Now := TimerQueue.RealEpochMilliseconds;

  TimerQueue.BeginFakeTimers(Now);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTimersHost.UseRealTimers(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Queue.EndFakeTimers;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTimersHost.IsFakeTimers(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaBooleanLiteralValue.FromBoolean(Queue.Faking);
end;

function TGocciaTimersHost.SetSystemTime(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  TimerQueue: TGocciaTimerQueue;
begin
  TimerQueue := Queue;
  { `setSystemTime()` with nothing to set means "freeze at now". }
  if (AArgs.Length = 0) or
     (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
    TimerQueue.SetSystemTime(TimerQueue.RealEpochMilliseconds)
  else
    TimerQueue.SetSystemTime(AArgs.GetElement(0).ToNumberLiteral.Value);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTimersHost.GetMockedSystemTime(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  TimerQueue: TGocciaTimerQueue;
begin
  TimerQueue := Queue;
  if TimerQueue.Faking then
    Result := TGocciaNumberLiteralValue.Create(TimerQueue.NowMilliseconds)
  else if TimerQueue.MockedDateOnly then
    Result := TGocciaNumberLiteralValue.Create(TimerQueue.MockedDate)
  else
    Result := TGocciaNullLiteralValue.NullValue;
end;

function TGocciaTimersHost.GetRealSystemTime(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(Queue.RealEpochMilliseconds);
end;

function TGocciaTimersHost.AdvanceTimersByTime(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  RequireFaking.Tick(MillisecondArgument(AArgs), False);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

{ The `Async` members differ from their synchronous twins in exactly one way:
  the microtask queue is drained before the first timer and again after each
  one, so a promise callback a timer scheduled runs before the next timer does.
  Because `await` in GocciaScript is a synchronous drain, doing that work here
  and letting the shim's `async` wrapper return the promise produces the same
  observable ordering as Vitest's real event-loop boundary. }
function TGocciaTimersHost.AdvanceTimersByTimeAsync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  RequireFaking.Tick(MillisecondArgument(AArgs), True);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTimersHost.AdvanceTimersToNextTimer(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  RequireFaking.AdvanceToNextTimer(False);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTimersHost.AdvanceTimersToNextTimerAsync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  RequireFaking.AdvanceToNextTimer(True);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTimersHost.RunAllTimers(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  RequireFaking.RunAllTimers(False);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTimersHost.RunAllTimersAsync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  RequireFaking.RunAllTimers(True);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTimersHost.RunOnlyPendingTimers(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  RequireFaking.RunPendingTimers(False);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTimersHost.RunOnlyPendingTimersAsync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  RequireFaking.RunPendingTimers(True);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTimersHost.ClearAllTimers(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Queue.ClearAllTimers;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaTimersHost.GetTimerCount(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(RequireFaking.CountTimers);
end;

procedure RegisterTimerGlobals(const AScope: TGocciaScope;
  var AHostToken: TObject);
var
  Host: TGocciaTimersHost;
begin
  Host := EnsureHost(AHostToken);
  AScope.DefineLexicalBinding('setTimeout',
    TGocciaNativeFunctionValue.Create(Host.SetTimeoutCallback,
      'setTimeout', 2), dtConst, True);
  AScope.DefineLexicalBinding('setInterval',
    TGocciaNativeFunctionValue.Create(Host.SetIntervalCallback,
      'setInterval', 2), dtConst, True);
  AScope.DefineLexicalBinding('clearTimeout',
    TGocciaNativeFunctionValue.Create(Host.ClearTimerCallback,
      'clearTimeout', 1), dtConst, True);
  AScope.DefineLexicalBinding('clearInterval',
    TGocciaNativeFunctionValue.Create(Host.ClearTimerCallback,
      'clearInterval', 1), dtConst, True);
end;

function CreateTimersNamespace(out AHostToken: TObject): TGocciaObjectValue;
var
  Host: TGocciaTimersHost;
  Namespace: TGocciaObjectValue;

  procedure Add(const AName: string;
    const ACallback: TGocciaNativeFunctionCallback; const AArity: Integer);
  begin
    Namespace.AssignProperty(AName,
      TGocciaNativeFunctionValue.Create(ACallback, AName, AArity));
  end;

begin
  AHostToken := nil;
  Host := EnsureHost(AHostToken);

  Namespace := TGocciaObjectValue.Create(
    TGocciaObjectValue.SharedObjectPrototype);
  Add('useFakeTimers', Host.UseFakeTimers, 1);
  Add('useRealTimers', Host.UseRealTimers, 0);
  Add('isFakeTimers', Host.IsFakeTimers, 0);
  Add('setSystemTime', Host.SetSystemTime, 1);
  Add('getMockedSystemTime', Host.GetMockedSystemTime, 0);
  Add('getRealSystemTime', Host.GetRealSystemTime, 0);
  Add('advanceTimersByTime', Host.AdvanceTimersByTime, 1);
  Add('advanceTimersByTimeAsync', Host.AdvanceTimersByTimeAsync, 1);
  Add('advanceTimersToNextTimer', Host.AdvanceTimersToNextTimer, 0);
  Add('advanceTimersToNextTimerAsync',
    Host.AdvanceTimersToNextTimerAsync, 0);
  Add('runAllTimers', Host.RunAllTimers, 0);
  Add('runAllTimersAsync', Host.RunAllTimersAsync, 0);
  Add('runOnlyPendingTimers', Host.RunOnlyPendingTimers, 0);
  Add('runOnlyPendingTimersAsync', Host.RunOnlyPendingTimersAsync, 0);
  Add('clearAllTimers', Host.ClearAllTimers, 0);
  Add('getTimerCount', Host.GetTimerCount, 0);
  Add('setTimeout', Host.SetTimeoutCallback, 2);
  Add('setInterval', Host.SetIntervalCallback, 2);
  Add('clearTimeout', Host.ClearTimerCallback, 1);
  Add('clearInterval', Host.ClearTimerCallback, 1);

  Result := Namespace;
end;

{ Drops one extension's host. The list owns its entries, so Remove frees it;
  an unknown or already-released token is ignored so a double detach is safe. }
procedure ReleaseTimersHost(const AHostToken: TObject);
begin
  if not (Assigned(AHostToken) and Assigned(GTimersHosts)) then
    Exit;
  GTimersHosts.Remove(AHostToken);
end;

procedure ClearTimersHosts;
begin
  FreeAndNil(GTimersHosts);
end;

initialization
  RegisterThreadvarCleanup(ClearTimersHosts);

end.
