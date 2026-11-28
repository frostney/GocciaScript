program SandboxHostAbortsTest;

{ How TGocciaSandboxHost reports the host aborts a run can end with: an embedder's own abort handle (a result, no
  exception), an embedder native's sandbox ceiling (sfkResourceLimit, past the script's handlers) and an audit sink
  that cannot take an event, the run's own or a nested run's (EGocciaCapabilityAuditDeliveryError, as before); and
  that the first of two causes stays the run's (a ceiling after a failed audit; an audit that fails while a
  cancelled run's work is discarded); and that a run a request stops before it starts on an engine whose last run's sink failed reports no audit failure.
  Prints "Sandbox host aborts: passed". }

{$mode delphiunicode}{$H+}
{$POINTERMATH ON}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SyncObjs,
  SysUtils,

  Goccia.CapabilityAudit,
  Goccia.Capabilities,
  Goccia.CLI.SandboxHost,
  Goccia.Core,
  Goccia.Embed,
  Goccia.Sandbox,
  Goccia.Sandbox.Context,
  Goccia.VM;

type
  TFixture = class
  public
    { 0 the abort handle, 1 the ceiling native, 2 the failing audit sink, 3 the child's sink, 4 a failed audit then a
      ceiling in one native, 5 the abort handle with host work whose discard fails an audit }
    Mode: Integer;
    Handle: IGocciaAbortHandle;
    Ready: TSimpleEvent;    // the guest is running (its console line "running")
    Started: Boolean;
    WaitFailed: Boolean;    // the aborter never saw the guest's marker
    Engine: TGocciaEngine;  // the run's engine, for the native of mode 4 and the host work of mode 5
    Work: TGocciaEmbedderWork;
    procedure Configure(const ARun: TGocciaSandboxRun);
    procedure OnLine(const AMethod, ALine: string);
    procedure FailingSink(const AEvent: TGocciaCapabilityAuditEvent);
  end;

  { host work that is pending until its discard, which reports an audit the run's failing sink cannot take }
  TAuditOnDiscard = class(TGocciaEmbedderWork)
  public
    Pending: Boolean;
    Discarded: Boolean;
    function HasPending: Boolean; override;
    function Pump(TimeoutMs: Cardinal): Boolean; override;
    procedure Discard; override;
    function Mark: QWord; override;
    procedure DiscardFrom(AMark: QWord); override;
  end;

  TAborter = class(TThread)
  public
    Fixture: TFixture;
  protected
    procedure Execute; override;
  end;

var
  Failures: Integer = 0;

procedure Fail(const AWhat: string);
begin
  WriteLn(ErrOutput, 'FAIL: ', AWhat);
  Inc(Failures);
end;

var
  GFixture: TFixture;

{ ceiling(): the embedder's native budget is spent }
function CeilingNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := SandboxCeiling('native budget exhausted');
end;

{ auditThenCeiling(): an audit the sink cannot take ends the run first; the budget then runs out too }
function AuditThenCeilingNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  if not GFixture.Engine.Audit(gckNetFetch, False, 'example.com', 'the fixture') then
  begin
    SandboxCeiling('native budget exhausted');
    Exit(False);
  end;
  Result := SandboxCeiling('native budget exhausted');
end;

function TAuditOnDiscard.HasPending: Boolean;
begin
  Result := Pending;
end;

function TAuditOnDiscard.Pump(TimeoutMs: Cardinal): Boolean;
begin
  Result := True;
end;

procedure TAuditOnDiscard.Discard;
begin
  Pending := False;
  Discarded := True;
  GFixture.Engine.Audit(gckNetFetch, False, 'example.com', 'the fixture');
end;

function TAuditOnDiscard.Mark: QWord;
begin
  Result := 0;
end;

procedure TAuditOnDiscard.DiscardFrom(AMark: QWord);
begin
end;

procedure TFixture.Configure(const ARun: TGocciaSandboxRun);
var
  Err: UnicodeString;
begin
  case Mode of
    0, 5:
      begin
        Handle := ARun.Engine.CaptureAbortHandle;
        ARun.LogLine := OnLine;
        if Mode = 5 then
        begin
          Engine := ARun.Engine;
          ARun.Engine.AuditSink := FailingSink;
          TAuditOnDiscard(Work).Pending := True;
          ARun.Engine.SetHostWork(Work);
        end;
      end;
    1, 4:
      begin
        if Mode = 4 then
        begin
          ARun.Engine.AuditSink := FailingSink;
          Engine := ARun.Engine;
        end;
        if not ARun.Engine.PrepareHost(Err) then
          Fail('PrepareHost: ' + string(Err));
        if Mode = 1 then
          PushRoot(NewNativeFunction(@CeilingNative, 'ceiling', 0))
        else
          PushRoot(NewNativeFunction(@AuditThenCeilingNative, 'ceiling', 0));
        try
          if not InjectGlobalBinding('ceiling', RootAt(TempTop - 1)) then
            Fail('inject ceiling');
        finally
          PopRoots(1);
        end;
      end;
    2:
      ARun.Engine.AuditSink := FailingSink;
    3:
      // only the runScript child's sink fails
      if ARun.Parent <> nil then
        ARun.Engine.AuditSink := FailingSink;
  end;
end;

procedure TFixture.OnLine(const AMethod, ALine: string);
begin
  if Pos('running', ALine) > 0 then
  begin
    Started := True;
    Ready.SetEvent;
  end;
end;

procedure TFixture.FailingSink(const AEvent: TGocciaCapabilityAuditEvent);
begin
  raise Exception.Create('the audit log is full');
end;

procedure TAborter.Execute;
begin
  // the guest is inside its loop before the request; without its marker the case fails rather than stop it on a timer
  if (Fixture.Ready.WaitFor(10000) = wrSignaled) and (Fixture.Handle <> nil) then
    Fixture.Handle.Request
  else
    Fixture.WaitFailed := True;
end;

function RunCase(const AFixture: TFixture; const ASource: string; out ARaised: string;
  const AChild: string = ''): TGocciaSandboxRunResult;
var
  Host: TGocciaSandboxHost;
  Aborter: TAborter;
begin
  ARaised := '';
  Result := Default(TGocciaSandboxRunResult);
  Host := TGocciaSandboxHost.Create(DEFAULT_SANDBOX_BYTE_QUOTA, DEFAULT_SANDBOX_NODE_QUOTA);
  Aborter := nil;
  try
    Host.Context.Fs.WriteAllText('/main.js', ASource);
    if AChild <> '' then
      Host.Context.Fs.WriteAllText('/child.js', AChild);
    Host.TimeoutMilliseconds := 10000;
    Host.OnConfigureEngine := AFixture.Configure;
    if AFixture.Mode in [0, 5] then
    begin
      Aborter := TAborter.Create(True);
      Aborter.Fixture := AFixture;
      Aborter.Start;
    end;
    try
      Result := Host.Run('/main.js');
    except
      on E: Exception do
        ARaised := E.ClassName + ': ' + E.Message;
    end;
  finally
    if Assigned(Aborter) then
    begin
      Aborter.WaitFor;
      Aborter.Free;
    end;
    Host.Free;
  end;
end;

{ An engine whose run's audit sink failed, then a run a request stops before it starts: no audit failure is reported
  for the second (TGocciaEngine.AuditDeliveryFailed). }
procedure CheckReusedEngine;
var
  Fixture: TFixture;
  Engine: TGocciaEngine;
  Caps: TGocciaCapabilities;
  R: TGocciaRunResult;
begin
  Fixture := TFixture.Create;
  Engine := TGocciaEngine.Create;
  try
    Caps := TGocciaCapabilities.None;
    Engine.Capabilities := Caps;
    Engine.AuditSink := Fixture.FailingSink;
    Engine.SourceKind := gskModule;
    R := Engine.Run('/audit.js', 'try { await fetch("https://example.com/"); } catch (e) { console.log(e.name); }');
    if not Engine.AuditDeliveryFailed then
      Fail(Format('the first run''s failing sink left AuditDeliveryFailed false (aborted=%d "%s")',
        [Ord(R.Aborted), R.ErrorText]));
    Engine.CaptureAbortHandle.Request;
    R := Engine.Run('/cancelled.js', 'console.log("never");');
    if R.Aborted <> gaHostAbort then
      Fail(Format('a run requested to stop before it starts ended with abort %d', [Ord(R.Aborted)]));
    if Engine.AuditDeliveryFailed then
      Fail('a run stopped before it started reported the previous run''s audit failure');
  finally
    Engine.Free;
    Fixture.Free;
  end;
end;

var
  Fixture: TFixture;
  R: TGocciaSandboxRunResult;
  Raised: string;
begin
  Fixture := TFixture.Create;
  GFixture := Fixture;
  Fixture.Ready := TSimpleEvent.Create;
  try
    // an embedder's abort handle stops a busy loop: a result the embedder asked for, not an audit failure
    Fixture.Mode := 0;
    R := RunCase(Fixture, 'const forever = { [Symbol.iterator]() { return { next: () => ({ value: 1, done: false }) }; } };' +
      LineEnding + 'console.log("running");' + LineEnding + 'for (const x of forever) {}', Raised);
    if not Fixture.Started or Fixture.WaitFailed then
      Fail('the abort case''s guest never signalled its marker: ' + Raised + ' ' + R.ErrorMessage)
    else if Raised <> '' then
      Fail('an embedder abort raised ' + Raised)
    else if R.Ok or (R.FailureKind <> sfkHostError) or (Pos('cancelled by the host', R.ErrorMessage) = 0) then
      Fail(Format('an embedder abort reported ok=%s kind=%s "%s"', [BoolToStr(R.Ok, True),
        SandboxFailureKindName(R.FailureKind), R.ErrorMessage]));

    // an embedder native's ceiling ends the run past the script's handlers, as resource-limit
    Fixture.Mode := 1;
    R := RunCase(Fixture, 'try { ceiling(); } catch (e) { console.log("caught"); }' + LineEnding +
      'console.log("after");', Raised);
    if Raised <> '' then
      Fail('a ceiling raised ' + Raised)
    else if R.Ok or (R.FailureKind <> sfkResourceLimit) or (R.ErrorMessage <> 'native budget exhausted') or
      (Pos('caught', R.Output) > 0) or (Pos('after', R.Output) > 0) then
      Fail(Format('a ceiling reported ok=%s kind=%s "%s" output "%s"', [BoolToStr(R.Ok, True),
        SandboxFailureKindName(R.FailureKind), R.ErrorMessage, R.Output]));

    // an audit sink that cannot take an event is still the host's failure, raised
    Fixture.Mode := 2;
    R := RunCase(Fixture, 'try { await fetch("https://example.com/"); } catch (e) { console.log(e.name); }', Raised);
    if Pos('EGocciaCapabilityAuditDeliveryError', Raised) <> 1 then
      Fail(Format('a failing audit sink raised "%s" (kind=%s "%s")', [Raised, SandboxFailureKindName(R.FailureKind),
        R.ErrorMessage]));

    // a nested run's sink that cannot take an event is the host's failure for the outer run too
    Fixture.Mode := 3;
    R := RunCase(Fixture, 'import { runScript } from "goccia";' + LineEnding + 'runScript("/child.js");' + LineEnding +
      'console.log("after");', Raised, 'try { await fetch("https://example.com/"); } catch (e) { console.log(e.name); }');
    if Pos('EGocciaCapabilityAuditDeliveryError', Raised) <> 1 then
      Fail(Format('a child''s failing audit sink raised "%s" (kind=%s "%s")', [Raised,
        SandboxFailureKindName(R.FailureKind), R.ErrorMessage]));

    // an audit that fails first stays the cause when a ceiling follows in the same native
    Fixture.Mode := 4;
    R := RunCase(Fixture, 'try { ceiling(); } catch (e) { console.log("caught"); }', Raised);
    if Pos('EGocciaCapabilityAuditDeliveryError', Raised) <> 1 then
      Fail(Format('a ceiling after a failed audit raised "%s" (kind=%s "%s")', [Raised,
        SandboxFailureKindName(R.FailureKind), R.ErrorMessage]));

    // an audit that fails while a cancelled run's work is discarded leaves the cancellation its cause
    Fixture.Mode := 5;
    Fixture.Started := False;
    Fixture.WaitFailed := False;
    Fixture.Ready.ResetEvent;
    Fixture.Work := TAuditOnDiscard.Create;
    try
      R := RunCase(Fixture, 'const forever = { [Symbol.iterator]() { return { next: () => ({ value: 1, done: false }) }; } };' +
        LineEnding + 'console.log("running");' + LineEnding + 'for (const x of forever) {}', Raised);
      if not Fixture.Started or Fixture.WaitFailed then
        Fail('the discard case''s guest never signalled its marker: ' + Raised + ' ' + R.ErrorMessage)
      else if not TAuditOnDiscard(Fixture.Work).Discarded then
        Fail('the cancelled run never discarded its host work')
      else if Raised <> '' then
        Fail('an audit failed while a cancelled run''s work was discarded raised ' + Raised)
      else if R.Ok or (R.FailureKind <> sfkHostError) or (Pos('cancelled by the host', R.ErrorMessage) = 0) then
        Fail(Format('an audit failed during a cancellation''s discard reported ok=%s kind=%s "%s"', [BoolToStr(R.Ok, True),
          SandboxFailureKindName(R.FailureKind), R.ErrorMessage]));
    finally
      FreeAndNil(Fixture.Work);
    end;

    CheckReusedEngine;
  finally
    Fixture.Ready.Free;
    Fixture.Free;
  end;
  if Failures > 0 then
    Halt(1);
  WriteLn('Sandbox host aborts: passed');
end.
