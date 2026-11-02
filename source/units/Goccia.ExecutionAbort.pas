unit Goccia.ExecutionAbort;

{$I Goccia.inc}

{ A thread-safe way for a host to stop a run that is executing on another
  thread, without killing the process (ADR 0174).

    // On the engine's thread, before the run starts:
    Handle := CaptureAbortHandle;
    try
      // ... StartExecutionTimeout, StartInstructionLimit, Execute ...
    finally
      // On the same thread, after the run ends or fails:
      ClearAbort;
    end;

    // From any thread, at any time while Handle is held:
    RequestAbort(Handle);          // or Handle.Request

  A request makes the next bytecode instruction, the next poll of a native
  loop (CheckExecutionTimeout, the chokepoint rule in Goccia.Timeout) and the
  next poll of an engine wait raise TGocciaAbortError on the engine's thread.
  The error is uncatchable by script, like the instruction limit it descends
  from, so it unwinds to the host the way a timeout does, and the engine is as
  safe to free or to run again afterwards as it is after one. A native call
  that is blocked outside the engine (in foreign code, or an extension's own
  blocking call) ends only when it returns.

  What a host has to know:

    The handle belongs to one run. CaptureAbortHandle arms the calling
    thread, ClearAbort disarms it and detaches the handle, and a request made
    through a detached handle, or after the thread ended, does nothing: a late
    request cannot reach the next run, which captures a handle of its own.
    Between the two, a request is sticky: every poll raises until ClearAbort,
    so a nested run's result cannot hide it from its parent.

    Capture before the run starts. An armed thread runs the VM's instrumented
    dispatch path, the one an instruction limit selects, because the
    production path has no per-instruction poll; a run already inside the
    production loop would not see the request until its next call or
    allocation. That path is the cost of being abortable, and an unarmed run
    pays nothing.

    The calling thread owns the arm: capture, ClearAbort and the run are one
    thread's calls, and only Request is for the others. A thread that ends
    without ClearAbort is disarmed by the thread cleanup registry. Capturing
    again on an armed thread detaches the previous handle first. }

interface

type
  { A reference-counted token for one armed run. Hold it wherever the thread
    that will request the abort can reach it; it stays valid after the run. }
  IGocciaAbortHandle = interface
    ['{5B1C0C6E-8E3F-4B41-9D07-6B2B6F4F7A21}']
    { Any thread. Idempotent. Does nothing once ClearAbort detached the
      handle, other than recording the request. }
    procedure Request;
    function IsRequested: Boolean;
  end;

{ Engine thread, before the run. Arms the calling thread's instruction and
  timeout polls and returns the handle that stops it. Replaces, and detaches,
  a handle this thread still held. }
function CaptureAbortHandle: IGocciaAbortHandle;

{ Any thread. The same as AHandle.Request, in the spelling of the other
  host-facing procedures. A nil handle is ignored. }
procedure RequestAbort(const AHandle: IGocciaAbortHandle);

{ Engine thread, after the run. Detaches the thread's handle and disarms its
  polls; the budgets and the deadline a host set stay as they are. Safe when
  nothing is armed. }
procedure ClearAbort;

{ Whether the calling thread holds a handle. }
function AbortIsArmed: Boolean;

implementation

uses
  SysUtils,

  CriticalSections,

  Goccia.InstructionLimit,
  Goccia.ThreadCleanupRegistry,
  Goccia.Timeout;

type
  TGocciaAbortHandle = class(TInterfacedObject, IGocciaAbortHandle)
  private
    FLock: TGocciaCriticalSection;
    { The word the engine thread's polls read. Written by any thread through
      an atomic exchange, read by the engine thread without a lock. }
    FRequested: LongInt;
    { Where a request writes 0 to end the instruction poll's single
      comparison: the capturing thread's NextLimitCount. Guarded by FLock, and
      nil once detached, so that a request never writes into a thread record
      that has been disarmed or has ended. }
    FNextLimit: PInt64;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Attach(const ANextLimit: PInt64);
    procedure Detach;
    function RequestedWord: PLongInt;
    procedure Request;
    function IsRequested: Boolean;
  end;

threadvar
  { The engine thread's own reference. A managed threadvar, released by the
    cleanup below. }
  GAbortHandle: TGocciaAbortHandle;
  GAbortHandleRef: IGocciaAbortHandle;

constructor TGocciaAbortHandle.Create;
begin
  inherited Create;
  CriticalSectionInit(FLock);
end;

destructor TGocciaAbortHandle.Destroy;
begin
  CriticalSectionDone(FLock);
  inherited Destroy;
end;

procedure TGocciaAbortHandle.Attach(const ANextLimit: PInt64);
begin
  CriticalSectionEnter(FLock);
  try
    FNextLimit := ANextLimit;
  finally
    CriticalSectionLeave(FLock);
  end;
end;

procedure TGocciaAbortHandle.Detach;
begin
  CriticalSectionEnter(FLock);
  try
    FNextLimit := nil;
  finally
    CriticalSectionLeave(FLock);
  end;
end;

function TGocciaAbortHandle.RequestedWord: PLongInt;
begin
  Result := @FRequested;
end;

{ The requesting thread's half of the handshake with the engine thread's
  recompute of NextLimitCount (Goccia.InstructionLimit): publish the request,
  fence, then lower the limit. A recompute that stored a fresh limit after
  this write reads the request word after its own fence and lowers the limit
  again, so one of the two always leaves it at 0. }
procedure TGocciaAbortHandle.Request;
begin
  AtomicExchangeInt32(FRequested, 1);
  FullMemoryBarrier;
  CriticalSectionEnter(FLock);
  try
    if FNextLimit <> nil then
      FNextLimit^ := 0;
  finally
    CriticalSectionLeave(FLock);
  end;
end;

function TGocciaAbortHandle.IsRequested: Boolean;
begin
  Result := AtomicLoadInt32(FRequested) <> 0;
end;

function AbortIsArmed: Boolean;
begin
  Result := GAbortHandle <> nil;
end;

procedure ClearAbort;
var
  Handle: TGocciaAbortHandle;
  Ref: IGocciaAbortHandle;
begin
  Handle := GAbortHandle;
  if Handle = nil then
    Exit;
  Ref := GAbortHandleRef;
  GAbortHandle := nil;
  GAbortHandleRef := nil;
  { Detach first: after it no request writes into this thread's record, so
    disarming may recompute the limit without a late 0 landing on it. The
    reference held here keeps the request word alive until the polls no
    longer read it. }
  Handle.Detach;
  DisarmInstructionAbort;
  DisarmTimeoutAbort;
  Ref := nil;
end;

function CaptureAbortHandle: IGocciaAbortHandle;
var
  Handle: TGocciaAbortHandle;
begin
  ClearAbort;
  Handle := TGocciaAbortHandle.Create;
  GAbortHandleRef := Handle;
  GAbortHandle := Handle;
  Handle.Attach(ArmInstructionAbort(Handle.RequestedWord));
  ArmTimeoutAbort(Handle.RequestedWord);
  Result := Handle;
end;

procedure RequestAbort(const AHandle: IGocciaAbortHandle);
begin
  if AHandle <> nil then
    AHandle.Request;
end;

initialization
  RegisterThreadvarCleanup(@ClearAbort);

end.
