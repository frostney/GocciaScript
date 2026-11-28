program SandboxHostAbortsTest;

{ How TGocciaSandboxHost reports the host aborts a run can end with: an embedder's own abort handle (a result, no
  exception), an embedder native's sandbox ceiling (sfkResourceLimit, past the script's handlers) and an audit sink
  that cannot take an event (EGocciaCapabilityAuditDeliveryError, as before). Prints "Sandbox host aborts: passed". }

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
    Mode: Integer;          // 0 the abort handle, 1 the ceiling native, 2 the failing audit sink
    Handle: IGocciaAbortHandle;
    Ready: PRTLEvent;
    procedure Configure(const ARun: TGocciaSandboxRun);
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
        RTLEventSetEvent(Ready);
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
  end;
end;

procedure TFixture.FailingSink(const AEvent: TGocciaCapabilityAuditEvent);
begin
  raise Exception.Create('the audit log is full');
end;

procedure TAborter.Execute;
begin
  RTLEventWaitFor(Fixture.Ready);
  Sleep(150);
  Fixture.Handle.Request;
end;

function RunCase(const AFixture: TFixture; const ASource: string; out ARaised: string): TGocciaSandboxRunResult;
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
      LineEnding + 'for (const x of forever) {}', Raised);
    if Raised <> '' then
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
  finally
    RTLEventDestroy(Fixture.Ready);
    Fixture.Free;
  end;
  if Failures > 0 then
    Halt(1);
  WriteLn('Sandbox host aborts: passed');
end.
