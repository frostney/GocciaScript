unit Goccia.RuntimeExtensions.Timers;

{$I Goccia.inc}

{ Timer runtime extension.

  Installs the four timer globals and the `goccia:timers` control module, and
  points the virtual timer queue at this engine's host environment so a mocked
  clock reaches `Date`, `Temporal.Now` and `performance`.

  Installed by the test-runner profile only. The timers are deterministic and
  carry no ambient authority, but they are a scheduling surface a sandboxed
  script does not otherwise have, and the acceptance target for them is the
  test runner. See docs/adr/0113-deterministic-virtual-timer-queue.md. }

interface

uses
  Goccia.Runtime,
  Goccia.RuntimeExtensions.NamespaceModule,
  Goccia.Values.Primitives;

const
  TIMERS_MODULE_NAME = 'goccia:timers';

type
  TGocciaTimersRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FTimersModule: TGocciaRuntimeNamespaceModuleRegistration;
    FHostToken: TObject;
    function MaterializeTimers: TGocciaValue;
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
    procedure Detach; override;
    procedure WaitForIdle; override;
    procedure DiscardPending; override;
  end;

implementation

uses
  Goccia.Builtins.Timers,
  Goccia.Timers;

procedure TGocciaTimersRuntimeExtension.Attach(
  const ARuntime: TGocciaRuntimeCore);
var
  Queue: TGocciaTimerQueue;
begin
  inherited Attach(ARuntime);

  TGocciaTimerQueue.Initialize;
  Queue := TGocciaTimerQueue.Instance;
  { The queue is per thread and outlives one engine, so an engine that attaches
    starts from a clean queue rather than from whatever the previous file on
    this thread left pending. Each test file gets its own engine, which is what
    keeps fake-timer state from leaking across files without a reset hook of
    its own. }
  Queue.ResetForEngine;
  { Both pointers name objects this engine owns, and both are cleared in Detach
    while it is still alive. Attaching a second engine over a queue that still
    named the first would leave PublishClock writing into a freed host
    environment; assigning here — rather than only when a clock is first mocked
    — is what keeps the two in step. }
  Queue.HostEnvironment := Runtime.Engine.HostEnvironment;
  Queue.OwnerRealm := Runtime.Engine.Realm;

  RegisterTimerGlobals(Runtime.Engine.Interpreter.GlobalScope, FHostToken);
  Runtime.RegisterRuntimeGlobalName('setTimeout');
  Runtime.RegisterRuntimeGlobalName('setInterval');
  Runtime.RegisterRuntimeGlobalName('clearTimeout');
  Runtime.RegisterRuntimeGlobalName('clearInterval');

  FTimersModule := TGocciaRuntimeNamespaceModuleRegistration.Create(Runtime,
    TIMERS_MODULE_NAME, MaterializeTimers);
end;

function TGocciaTimersRuntimeExtension.MaterializeTimers: TGocciaValue;
begin
  Result := CreateTimersNamespace(FHostToken);
end;

{ Real-mode timers fire where a host event loop would have taken over.

  This is one of those points, not the only one and not the last: the engine
  reaches runtime idle when the entry module has finished evaluating, which
  under the test runner is *before* any test body has run. Timers a test
  schedules are drained by the runner's own per-test lifecycle instead
  (Goccia.Builtins.TestingLibrary), which is also what attributes a throwing
  callback to the test that scheduled it.

  Timeouts only, and nothing raises: DrainRealTimers skips intervals, because an
  uncleared one is infinite and would spend the whole budget without finishing
  any sooner, and it parks a thrown value for the host rather than propagating
  it — a module-scope timer must not fail the file from inside a teardown path.
  Under fake timers nothing runs here at all: a timer the suite never advanced
  to is a timer the suite did not want. }
procedure TGocciaTimersRuntimeExtension.WaitForIdle;
begin
  inherited;
  DrainRealTimersForHost;
end;

procedure TGocciaTimersRuntimeExtension.DiscardPending;
begin
  inherited;
  DiscardRealTimers;
end;

procedure TGocciaTimersRuntimeExtension.Detach;
var
  Queue: TGocciaTimerQueue;
begin
  FTimersModule.Free;
  FTimersModule := nil;

  Queue := TGocciaTimerQueue.Instance;
  if Assigned(Queue) then
  begin
    { The clock override has to come off before the host environment goes away,
      and the queue must stop pointing at a freed one. }
    Queue.ResetForEngine;
    Queue.HostEnvironment := nil;
    Queue.OwnerRealm := nil;
  end;

  ReleaseTimersHost(FHostToken);
  FHostToken := nil;
  inherited;
end;

end.
