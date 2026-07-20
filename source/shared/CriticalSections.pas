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

procedure CriticalSectionInit(var ASection: TGocciaCriticalSection);
{$IFDEF FPC}inline;{$ENDIF}
procedure CriticalSectionDone(var ASection: TGocciaCriticalSection);
{$IFDEF FPC}inline;{$ENDIF}
procedure CriticalSectionEnter(var ASection: TGocciaCriticalSection);
{$IFDEF FPC}inline;{$ENDIF}
procedure CriticalSectionLeave(var ASection: TGocciaCriticalSection);
{$IFDEF FPC}inline;{$ENDIF}
function GetGocciaThreadId: TThreadID;
{$IFDEF FPC}inline;{$ENDIF}
function AtomicIncrementInt32(var AValue: Integer): Integer;
{$IFDEF FPC}inline;{$ENDIF}
function AtomicDecrementInt32(var AValue: Integer): Integer;
{$IFDEF FPC}inline;{$ENDIF}
procedure ReadMemoryBarrier;
{$IFDEF FPC}inline;{$ENDIF}
procedure WriteMemoryBarrier;
{$IFDEF FPC}inline;{$ENDIF}

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
