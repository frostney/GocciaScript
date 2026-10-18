unit CriticalSections;

{$I Shared.inc}

interface

uses
  Classes
  {$IFNDEF FPC},
  System.SyncObjs
  {$ENDIF};

type
  {$IFDEF FPC}
  TGocciaCriticalSection = TRTLCriticalSection;
  {$ELSE}
  TGocciaCriticalSection = record
  private
    FInstance: TCriticalSection;
  end;
  {$ENDIF}

procedure CriticalSectionInit(var ASection: TGocciaCriticalSection); {$IFDEF FPC}inline;{$ENDIF}
procedure CriticalSectionDone(var ASection: TGocciaCriticalSection); {$IFDEF FPC}inline;{$ENDIF}
procedure CriticalSectionEnter(var ASection: TGocciaCriticalSection); {$IFDEF FPC}inline;{$ENDIF}
procedure CriticalSectionLeave(var ASection: TGocciaCriticalSection); {$IFDEF FPC}inline;{$ENDIF}
function GetGocciaThreadId: TThreadID; {$IFDEF FPC}inline;{$ENDIF}
function AtomicIncrementInt32(var AValue: Integer): Integer; {$IFDEF FPC}inline;{$ENDIF}
function AtomicDecrementInt32(var AValue: Integer): Integer; {$IFDEF FPC}inline;{$ENDIF}
function AtomicExchangeInt32(var AValue: Integer; const ANewValue: Integer): Integer; {$IFDEF FPC}inline;{$ENDIF}
{ Sequentially consistent 32-bit load and compare-and-exchange: every
  access to a value through these two is in one total order that agrees with
  program order. AtomicCompareExchangeInt32 stores ANewValue when AValue
  equals AComparand and returns the value AValue held before.

  x86 keeps loads in order and gives a locked cmpxchg a full barrier, so it
  needs no fence. FPC's InterlockedCompareExchange on a weakly ordered target
  (AArch64: a bare ldxr/stxr loop) orders nothing, so there the exchange is
  fenced on both sides and the load is followed by a fence: the fence-based
  mapping of a seq-cst load and read-modify-write. }
function AtomicLoadInt32(var AValue: Integer): Integer; {$IFDEF FPC}inline;{$ENDIF}
function AtomicCompareExchangeInt32(var AValue: Integer;
  const ANewValue, AComparand: Integer): Integer; {$IFDEF FPC}inline;{$ENDIF}
procedure ReadMemoryBarrier; {$IFDEF FPC}inline;{$ENDIF}
procedure WriteMemoryBarrier; {$IFDEF FPC}inline;{$ENDIF}

implementation

procedure CriticalSectionInit(var ASection: TGocciaCriticalSection);
begin
  {$IFDEF FPC}
  System.InitCriticalSection(ASection);
  {$ELSE}
  ASection.FInstance := TCriticalSection.Create;
  {$ENDIF}
end;

procedure CriticalSectionDone(var ASection: TGocciaCriticalSection);
begin
  {$IFDEF FPC}
  System.DoneCriticalSection(ASection);
  {$ELSE}
  ASection.FInstance.Free;
  ASection.FInstance := nil;
  {$ENDIF}
end;

procedure CriticalSectionEnter(var ASection: TGocciaCriticalSection);
begin
  {$IFDEF FPC}
  System.EnterCriticalSection(ASection);
  {$ELSE}
  ASection.FInstance.Enter;
  {$ENDIF}
end;

procedure CriticalSectionLeave(var ASection: TGocciaCriticalSection);
begin
  {$IFDEF FPC}
  System.LeaveCriticalSection(ASection);
  {$ELSE}
  ASection.FInstance.Leave;
  {$ENDIF}
end;

function GetGocciaThreadId: TThreadID;
begin
  {$IFDEF FPC}
  Result := System.GetCurrentThreadId;
  {$ELSE}
  Result := TThread.CurrentThread.ThreadID;
  {$ENDIF}
end;

function AtomicIncrementInt32(var AValue: Integer): Integer;
begin
  {$IFDEF FPC}
  Result := System.InterlockedIncrement(AValue);
  {$ELSE}
  Result := TInterlocked.Increment(AValue);
  {$ENDIF}
end;

function AtomicDecrementInt32(var AValue: Integer): Integer;
begin
  {$IFDEF FPC}
  Result := System.InterlockedDecrement(AValue);
  {$ELSE}
  Result := TInterlocked.Decrement(AValue);
  {$ENDIF}
end;

function AtomicExchangeInt32(var AValue: Integer; const ANewValue: Integer): Integer;
begin
  {$IFDEF FPC}
  Result := System.InterlockedExchange(AValue, ANewValue);
  {$ELSE}
  Result := TInterlocked.Exchange(AValue, ANewValue);
  {$ENDIF}
end;

function AtomicLoadInt32(var AValue: Integer): Integer;
begin
  Result := AValue;
  {$IFDEF FPC}
  {$IF not (defined(CPUX86_64) or defined(CPUI386))}
  System.ReadWriteBarrier;
  {$IFEND}
  {$ELSE}
  System.MemoryBarrier;
  {$ENDIF}
end;

function AtomicCompareExchangeInt32(var AValue: Integer;
  const ANewValue, AComparand: Integer): Integer;
begin
  {$IFDEF FPC}
  {$IF not (defined(CPUX86_64) or defined(CPUI386))}
  System.ReadWriteBarrier;
  {$IFEND}
  Result := System.InterlockedCompareExchange(AValue, ANewValue, AComparand);
  {$IF not (defined(CPUX86_64) or defined(CPUI386))}
  System.ReadWriteBarrier;
  {$IFEND}
  {$ELSE}
  Result := TInterlocked.CompareExchange(AValue, ANewValue, AComparand);
  {$ENDIF}
end;

procedure ReadMemoryBarrier;
begin
  {$IFDEF FPC}
  System.ReadBarrier;
  {$ELSE}
  System.MemoryBarrier;
  {$ENDIF}
end;

procedure WriteMemoryBarrier;
begin
  {$IFDEF FPC}
  System.WriteBarrier;
  {$ELSE}
  System.MemoryBarrier;
  {$ENDIF}
end;

end.
