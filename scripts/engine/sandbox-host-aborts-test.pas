program SandboxHostAbortsTest;

{ How TGocciaSandboxHost reports the host aborts a run can end with: an embedder's own abort handle (a result, no
  exception), an embedder native's sandbox ceiling (sfkResourceLimit, past the script's handlers) and an audit sink
  that cannot take an event, the run's own or a nested run's (EGocciaCapabilityAuditDeliveryError, as before); and
  that a run a request stops before it starts on an engine whose last run's sink failed reports no audit failure.
  Prints "Sandbox host aborts: passed". }

{$mode delphiunicode}{$H+}
{$POINTERMATH ON}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
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
    Mode: Integer;          // 0 the abort handle, 1 the ceiling native, 2 the failing audit sink, 3 the child's sink
    Handle: IGocciaAbortHandle;
    Ready: PRTLEvent;       // the guest is running (its console line "running")
    Started: Boolean;
    procedure Configure(const ARun: TGocciaSandboxRun);
    procedure OnLine(const AMethod, ALine: string);
    procedure FailingSink(const AEvent: TGocciaCapabilityAuditEvent);
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

{ ceiling(): the embedder's native budget is spent }
function CeilingNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := SandboxCeiling('native budget exhausted');
end;

procedure TFixture.Configure(const ARun: TGocciaSandboxRun);
var
  Err: UnicodeString;
begin
  case Mode of
    0:
      begin
        Handle := ARun.Engine.CaptureAbortHandle;
        ARun.LogLine := OnLine;
      end;
    1:
      begin
        if not ARun.Engine.PrepareHost(Err) then
          Fail('PrepareHost: ' + string(Err));
        PushRoot(NewNativeFunction(@CeilingNative, 'ceiling', 0));
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
    RTLEventSetEvent(Ready);
  end;
end;

procedure TFixture.FailingSink(const AEvent: TGocciaCapabilityAuditEvent);
begin
  raise Exception.Create('the audit log is full');
end;

procedure TAborter.Execute;
begin
  // the guest is inside its loop before the request (bounded: a run that never starts fails the case below)
  RTLEventWaitFor(Fixture.Ready, 10000);
  if Fixture.Handle <> nil then
    Fixture.Handle.Request;
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
    if AFixture.Mode = 0 then
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
  Fixture.Ready := RTLEventCreate;
  try
    // an embedder's abort handle stops a busy loop: a result the embedder asked for, not an audit failure
    Fixture.Mode := 0;
    R := RunCase(Fixture, 'const forever = { [Symbol.iterator]() { return { next: () => ({ value: 1, done: false }) }; } };' +
      LineEnding + 'console.log("running");' + LineEnding + 'for (const x of forever) {}', Raised);
    if not Fixture.Started then
      Fail('the abort case''s guest never printed its marker: ' + Raised + ' ' + R.ErrorMessage)
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

    CheckReusedEngine;
  finally
    RTLEventDestroy(Fixture.Ready);
    Fixture.Free;
  end;
  if Failures > 0 then
    Halt(1);
  WriteLn('Sandbox host aborts: passed');
end.
