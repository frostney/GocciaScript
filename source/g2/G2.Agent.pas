unit G2.Agent;
{ test262's agent API ($262.agent, test262 INTERPRETING.md): the main agent starts agents, each a script in
  an engine of its own on a thread of its own, broadcasts a SharedArrayBuffer and a number to them and
  collects their reports.

  Main agent:  start(source), broadcast(sab, id), getReport(), sleep(ms), monotonicNow()
  Agents:      receiveBroadcast(callback), report(value), leaving(), sleep(ms), monotonicNow()

  An agent runs its script, then waits for the broadcast if it called receiveBroadcast, calls the callback
  with a SharedArrayBuffer of its own engine over the broadcast's block, drains its jobs and ends. broadcast
  returns once every running agent has taken the message. Reports go to one process-wide queue. }
{$mode delphiunicode}{$H+}

interface

uses
  SysUtils, Classes, G2.Core, G2.VM;

{ defines $262agent (main agent) on this engine's global object }
procedure InitAgentHost;

implementation

uses
  G2.Engine, G2.Shared, G2.Atomics, G2.Promise;

type
  TArgs = PValue;

  TAgentThread = class(TThread)
  public
    Source: UnicodeString;
    { the pending broadcast, guarded by AgentLock }
    MsgBlock: PSharedBlock;
    MsgId: Double;
    HasMsg, TookMsg, Done: Boolean;
    MsgEvent: PRTLEvent;
    constructor Create(const ASource: UnicodeString);
    destructor Destroy; override;
  protected
    procedure Execute; override;
  end;

var
  AgentLock: TRTLCriticalSection;
  Agents: TList;              { of TAgentThread, guarded by AgentLock }
  Reports: TStringList;       { guarded by AgentLock }
  StartTicks: QWord;

threadvar
  CurrentAgent: TAgentThread;   { nil in the main agent }
  BroadcastCallback: TValue;
  AgentObj: TValue;

function Arg(Args: TArgs; Argc, I: Integer): TValue; inline;
begin
  if I <= Argc then Result := Args[I] else Result := VUndefined;
end;

function Agent_Sleep(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var D: Double;
begin
  if not ToNumberV(Arg(Args, Argc, 1), D) then Exit(False);
  if D > 0 then Sleep(Trunc(D));
  Res := VUndefined;
  Result := True;
end;

function Agent_MonotonicNow(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkNumber(GetTickCount64 - StartTicks);
  Result := True;
end;

{ ---------------- main agent ---------------- }

function Agent_Start(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: TValue; A: TAgentThread;
begin
  if not ToStringV(Arg(Args, Argc, 1), S) then Exit(False);
  A := TAgentThread.Create(StrToUnicode(StrOf(S)));
  EnterCriticalSection(AgentLock);
  Agents.Add(A);
  LeaveCriticalSection(AgentLock);
  A.Start;
  Res := VUndefined;
  Result := True;
end;

function Agent_Broadcast(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var B: PSharedBlock; Id: Double; I: Integer; A: TAgentThread; Waiting: Boolean;
begin
  if not IsSharedBuffer(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('broadcast requires a SharedArrayBuffer'));
  B := SharedBlockOf(Args[1]);
  if not ToNumberV(Arg(Args, Argc, 2), Id) then Exit(False);
  EnterCriticalSection(AgentLock);
  for I := 0 to Agents.Count - 1 do
  begin
    A := TAgentThread(Agents[I]);
    if A.Done then Continue;
    RetainBlock(B);
    A.MsgBlock := B;
    A.MsgId := Id;
    A.HasMsg := True;
    A.TookMsg := False;
    RTLEventSetEvent(A.MsgEvent);
  end;
  LeaveCriticalSection(AgentLock);
  { until every running agent has the message }
  repeat
    Waiting := False;
    EnterCriticalSection(AgentLock);
    for I := 0 to Agents.Count - 1 do
      with TAgentThread(Agents[I]) do
        if HasMsg and not TookMsg and not Done then Waiting := True;
    LeaveCriticalSection(AgentLock);
    if Waiting then Sleep(1);
  until not Waiting;
  Res := VUndefined;
  Result := True;
end;

function Agent_GetReport(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: UnicodeString; Have: Boolean;
begin
  EnterCriticalSection(AgentLock);
  Have := Reports.Count > 0;
  if Have then begin S := UnicodeString(Reports[0]); Reports.Delete(0); end;
  LeaveCriticalSection(AgentLock);
  if Have then Res := StrFromUnicode(S) else Res := VNull;
  Result := True;
end;

{ ---------------- agents ---------------- }

function Agent_ReceiveBroadcast(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if not IsCallable(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('receiveBroadcast requires a function'));
  BroadcastCallback := Args[1];
  Res := VUndefined;
  Result := True;
end;

function Agent_Report(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: TValue;
begin
  if not ToStringV(Arg(Args, Argc, 1), S) then Exit(False);
  EnterCriticalSection(AgentLock);
  Reports.Add(UTF8Encode(StrToUnicode(StrOf(S))));
  LeaveCriticalSection(AgentLock);
  Res := VUndefined;
  Result := True;
end;

function Agent_Leaving(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := VUndefined;
  Result := True;
end;

constructor TAgentThread.Create(const ASource: UnicodeString);
begin
  Source := ASource;
  MsgEvent := RTLEventCreate;
  FreeOnTerminate := False;
  inherited Create(True);
end;

destructor TAgentThread.Destroy;
begin
  RTLEventDestroy(MsgEvent);
  inherited;
end;

procedure TAgentThread.Execute;
var Err: UnicodeString; G, Sab, CbArgs: array[0..1] of TValue; R: TValue; B: PSharedBlock; Id: Double;
begin
  CurrentAgent := Self;
  try
    EngineInit;
    try
      AddRoot(@BroadcastCallback); AddRoot(@AgentObj);
      BroadcastCallback := VUndefined;
      AgentObj := MkPtr(NewPlainObject(ObjectProto, 8));
      DefineNative(AgentObj, 'receiveBroadcast', Agent_ReceiveBroadcast, 1);
      DefineNative(AgentObj, 'report', Agent_Report, 1);
      DefineNative(AgentObj, 'leaving', Agent_Leaving, 0);
      DefineNative(AgentObj, 'sleep', Agent_Sleep, 1);
      DefineNative(AgentObj, 'monotonicNow', Agent_MonotonicNow, 0);
      G[0] := MkPtr(NewPlainObject(ObjectProto, 4));
      PushRoot(G[0]);
      DefineOwn(RootAt(TempTop - 1), AtomOf('agent'), AgentObj, pfDefault);
      DefineOwn(GlobalObj, AtomOf('$262'), RootAt(TempTop - 1), pfHidden);
      PopRoots(1);
      if EngineRunScript(Source, '<agent>', Err) and IsCallable(BroadcastCallback) then
      begin
        { wait for the broadcast }
        repeat
          EnterCriticalSection(AgentLock);
          if HasMsg then
          begin
            B := MsgBlock; Id := MsgId; HasMsg := False; TookMsg := True;
            LeaveCriticalSection(AgentLock);
            Break;
          end;
          LeaveCriticalSection(AgentLock);
          RTLEventWaitFor(MsgEvent, 100);
        until False;
        Sab[0] := WrapSharedBlock(B);
        ReleaseBlock(B);   { the agent's own object holds its reference now }
        PushRoot(Sab[0]);
        CbArgs[0] := RootAt(TempTop - 1);
        CbArgs[1] := MkNumber(Id);
        { an async callback may wait (Atomics.waitAsync): the host loop settles its waiters }
        if CallValue(BroadcastCallback, VUndefined, @CbArgs[0], 2, R) then
          if RunMicrotasks then RunHostLoop;
        PopRoots(1);
      end;
    finally
      EngineDone;
    end;
  except
    on E: Exception do ;
  end;
  EnterCriticalSection(AgentLock);
  Done := True;
  LeaveCriticalSection(AgentLock);
end;

procedure InitAgentHost;
begin
  AddRoot(@AgentObj);
  AgentObj := MkPtr(NewPlainObject(ObjectProto, 8));
  DefineNative(AgentObj, 'start', Agent_Start, 1);
  DefineNative(AgentObj, 'broadcast', Agent_Broadcast, 2);
  DefineNative(AgentObj, 'getReport', Agent_GetReport, 0);
  DefineNative(AgentObj, 'sleep', Agent_Sleep, 1);
  DefineNative(AgentObj, 'monotonicNow', Agent_MonotonicNow, 0);
  DefineOwn(GlobalObj, AtomOf('$262agent'), AgentObj, pfHidden);
end;

initialization
  InitCriticalSection(AgentLock);
  Agents := TList.Create;
  Reports := TStringList.Create;
  StartTicks := GetTickCount64;

finalization
  { agents still running when the main script ends are left to the process exit }
  DoneCriticalSection(AgentLock);

end.
