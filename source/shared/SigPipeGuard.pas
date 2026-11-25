unit SigPipeGuard;

{$I Shared.inc}

{ Runs a write with SIGPIPE held off on the calling thread, so a write to a
  pipe or socket whose reader has gone fails with EPIPE instead of ending the
  process. BeginSigPipeGuard blocks SIGPIPE on the calling thread;
  EndSigPipeGuard takes a SIGPIPE the guarded call raised off the pending set
  and restores the mask. The process's signal disposition, an embedder's
  handlers, every other thread and the processes it starts are left alone. A
  caller that blocks SIGPIPE itself keeps the block, and a SIGPIPE it already
  held pending; one sent to this thread by another party while an unblocked
  caller's call runs is indistinguishable from ours and is taken too. Outside
  a Unix both are no-ops. }

interface

{$IFDEF UNIX}
uses
  BaseUnix;
{$ENDIF}

type
  TSigPipeGuard = record
    Active: Boolean;
    WasBlocked: Boolean;
    PendingBefore: Boolean;
    {$IFDEF UNIX}
    Previous: TSigSet;
    {$ENDIF}
  end;

procedure BeginSigPipeGuard(out AGuard: TSigPipeGuard);
{ AMayHaveRaised False says the guarded call succeeded, so it raised no
  SIGPIPE and none is looked for. }
procedure EndSigPipeGuard(const AGuard: TSigPipeGuard;
  const AMayHaveRaised: Boolean = True);

implementation

{$IFDEF UNIX}
uses
  ctypes;

type
  { glibc's sigset_t is 128 bytes, FPC's TSigSet 16, Darwin's 4: sigpending is
    called through libc with room for the largest, since FPC's own
    fpSigPending passes the Linux kernel a wrong set size and fails with
    EINVAL. Signal n is bit n - 1 in each layout. }
  TLibcSigSet = array[0..127] of Byte;

function LibcSigPending(var ASet: TLibcSigSet): cint; cdecl;
  external 'c' name 'sigpending';

{$IFNDEF LINUX}
function LibcSigWait(const ASet: TSigSet; out ASignal: cint): cint; cdecl;
  external 'c' name 'sigwait';
{$ENDIF}

function SigPipePending: Boolean;
var
  Pending: TLibcSigSet;
begin
  FillChar(Pending, SizeOf(Pending), 0);
  Result := (LibcSigPending(Pending) = 0) and
    ((Pending[(SIGPIPE - 1) div 8] and (1 shl ((SIGPIPE - 1) mod 8))) <> 0);
end;

procedure BeginSigPipeGuard(out AGuard: TSigPipeGuard);
var
  Block: TSigSet;
begin
  FillChar(AGuard, SizeOf(AGuard), 0);
  fpSigEmptySet(Block);
  fpSigAddSet(Block, SIGPIPE);
  AGuard.Active := fpSigProcMask(SIG_BLOCK, @Block, @AGuard.Previous) = 0;
  if AGuard.Active then
  begin
    AGuard.WasBlocked := fpSigIsMember(AGuard.Previous, SIGPIPE) = 1;
    { A caller that blocks SIGPIPE itself may already hold one pending; that
      one is its own and stays. }
    if AGuard.WasBlocked then
      AGuard.PendingBefore := SigPipePending;
  end;
end;

procedure EndSigPipeGuard(const AGuard: TSigPipeGuard;
  const AMayHaveRaised: Boolean);
var
  Wanted: TSigSet;
  {$IFDEF LINUX}
  NoWait: TimeSpec;
  {$ELSE}
  Taken: cint;
  {$ENDIF}
begin
  if not AGuard.Active then
    Exit;
  { Only a SIGPIPE the caller did not already hold pending is taken. Taken
    even when the caller blocks SIGPIPE, or unblocking it would end the
    process for a write that failed with an error. }
  if AMayHaveRaised and not AGuard.PendingBefore then
  begin
    fpSigEmptySet(Wanted);
    fpSigAddSet(Wanted, SIGPIPE);
    {$IFDEF LINUX}
    { A zero timeout takes the thread's pending SIGPIPE, if any, and fails
      with EAGAIN when there is none. }
    FillChar(NoWait, SizeOf(NoWait), 0);
    fpSigTimedWait(Wanted, nil, @NoWait);
    {$ELSE}
    { Darwin and the BSDs have no sigtimedwait; sigwait returns at once for
      a SIGPIPE that is pending, and is called only then. }
    if SigPipePending then
      LibcSigWait(Wanted, Taken);
    {$ENDIF}
  end;
  if not AGuard.WasBlocked then
    fpSigProcMask(SIG_SETMASK, @AGuard.Previous, nil);
end;
{$ELSE}
procedure BeginSigPipeGuard(out AGuard: TSigPipeGuard);
begin
  FillChar(AGuard, SizeOf(AGuard), 0);
end;

procedure EndSigPipeGuard(const AGuard: TSigPipeGuard;
  const AMayHaveRaised: Boolean);
begin
end;
{$ENDIF}

end.
