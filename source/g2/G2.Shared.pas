unit G2.Shared;
{ Memory shared between engines on different threads: the data blocks of SharedArrayBuffers, atomic
  operations on them, and the waiter lists of Atomics.wait / Atomics.notify.

  Each engine is single-threaded and owns its heap; a SharedArrayBuffer object lives in one engine's heap and
  points at a TSharedBlock outside every heap. A block is reference-counted: each SharedArrayBuffer object
  that refers to it (in any engine) holds one reference, released when that object is collected
  (RegisterSharedBuffer). A block's data never moves: a growable buffer reserves its maximum length up front.

  ES2026 25.2 (SharedArrayBuffer), 25.4 (Atomics), 29 (memory model). The waiter lists follow 25.4.3
  (WaiterList): one critical section guards all of them, which is the spec's critical section per list,
  coarsened. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}

interface

uses
  SysUtils, G2.Core;

type
  PSharedBlock = ^TSharedBlock;
  TSharedBlock = record
    RefCount: Integer;      { atomic }
    ByteLength: Int64;      { atomic: a growable buffer's length only grows }
    MaxByteLength: Int64;   { -1: fixed length }
    Data: PByte;            { MaxByteLength (or ByteLength) bytes, zeroed, 16-byte aligned }
  end;

function NewSharedBlock(Len, MaxLen: Int64): PSharedBlock;   { nil when the memory cannot be had }
procedure RetainBlock(B: PSharedBlock);
procedure ReleaseBlock(B: PSharedBlock);
function BlockLength(B: PSharedBlock): Int64; inline;
{ grows B to NewLen (<= MaxByteLength); False if another thread grew it past NewLen first }
function GrowBlock(B: PSharedBlock; NewLen: Int64): Boolean;

{ This engine's SharedArrayBuffer objects: a block reference is released when its object is collected. }
procedure RegisterSharedBuffer(O: PObj; B: PSharedBlock);

type
  TAtomicOp = (aoAdd, aoAnd, aoExchange, aoOr, aoSub, aoXor);

{ Read-modify-write on the Width-byte (1, 2, 4, 8) integer at P, sequentially consistent; returns the old
  value's bits, zero-extended. 1- and 2-byte cells use a compare-and-swap on their aligned 4-byte word. }
function AtomicRMW(P: PByte; Width: Integer; Op: TAtomicOp; Operand: QWord): QWord;
function AtomicCompareExchange(P: PByte; Width: Integer; Expected, Replacement: QWord): QWord;
function AtomicLoad(P: PByte; Width: Integer): QWord;
procedure AtomicStore(P: PByte; Width: Integer; V: QWord);

type
  TWaitResult = (wrOk, wrNotEqual, wrTimedOut);
  { evaluated inside the waiter-list critical section: True if the cell still holds the expected value }
  TWaitCheck = function(Data: Pointer): Boolean;

{ Atomics.wait: suspends the calling thread on (Block, ByteIndex) if Check holds, until notified or
  TimeoutMs passes. TimeoutMs is +Infinity or >= 0 (25.4.13: NaN is +Infinity, a negative time 0). }
function SharedWait(Block: PSharedBlock; ByteIndex: Int64; Check: TWaitCheck; CheckData: Pointer;
  TimeoutMs: Double): TWaitResult;
{ Atomics.waitAsync: adds an async waiter for this thread with Token if Check holds (wrOk), else wrNotEqual.
  A notify posts Token to this thread's mailbox (SharedAsyncPoll). }
function SharedWaitAsyncAdd(Block: PSharedBlock; ByteIndex: Int64; Check: TWaitCheck; CheckData: Pointer;
  Token: Integer): TWaitResult;
{ removes this thread's async waiter Token if it is still waiting (its timeout fired first): True if removed }
function SharedAsyncCancel(Token: Integer): Boolean;
{ waits up to TimeoutMs for this thread's async waiters to be notified; returns their tokens }
function SharedAsyncPoll(TimeoutMs: Cardinal): TBoundArray;
{ Atomics.notify: wakes up to Count (negative: all) waiters on (Block, ByteIndex), oldest first, sync and
  async alike; returns how many it woke }
function SharedNotify(Block: PSharedBlock; ByteIndex: Int64; Count: Int64): Int64;

implementation

uses
  Math;

{ ---------------- blocks ---------------- }

function NewSharedBlock(Len, MaxLen: Int64): PSharedBlock;
var Cap: Int64; Raw: PByte;
begin
  Result := nil;
  if MaxLen >= 0 then Cap := MaxLen else Cap := Len;
  if (Len < 0) or (Cap < Len) or (Cap > High(PtrInt) - 64) then Exit;
  try
    Raw := AllocMem(Cap + 1);   { +1: a zero-length block still has an address }
  except
    on EOutOfMemory do Exit;   { the caller throws a RangeError (25.2.2.1 CreateByteDataBlock) }
  end;
  if Raw = nil then Exit;
  New(Result);
  Result^.RefCount := 1;
  Result^.ByteLength := Len;
  Result^.MaxByteLength := MaxLen;
  Result^.Data := Raw;
end;

procedure RetainBlock(B: PSharedBlock);
begin
  InterLockedIncrement(B^.RefCount);
end;

procedure ReleaseBlock(B: PSharedBlock);
begin
  if InterLockedDecrement(B^.RefCount) = 0 then
  begin
    FreeMem(B^.Data);
    Dispose(B);
  end;
end;

function BlockLength(B: PSharedBlock): Int64; inline;
begin
  Result := InterlockedCompareExchange64(B^.ByteLength, 0, 0);
end;

function GrowBlock(B: PSharedBlock; NewLen: Int64): Boolean;
var Cur: Int64;
begin
  repeat
    Cur := BlockLength(B);
    if NewLen < Cur then Exit(False);
    if NewLen = Cur then Exit(True);
  until InterlockedCompareExchange64(B^.ByteLength, NewLen, Cur) = Cur;
  Result := True;
end;

{ ---------------- this engine's SharedArrayBuffer objects ---------------- }

threadvar
  SharedObjs: array of PObj;
  SharedBlocks: array of PSharedBlock;
  SharedCount: Integer;

procedure SharedFinalizeHook;
var I, J: Integer; P: Pointer;
begin
  J := 0;
  for I := 0 to SharedCount - 1 do
  begin
    P := SharedObjs[I];
    if GCSurvives(P) then
    begin
      SharedObjs[J] := PObj(P);
      SharedBlocks[J] := SharedBlocks[I];
      Inc(J);
    end
    else
      ReleaseBlock(SharedBlocks[I]);
  end;
  SharedCount := J;
end;

procedure RegisterSharedBuffer(O: PObj; B: PSharedBlock);
begin
  if SharedCount = Length(SharedObjs) then
  begin
    SetLength(SharedObjs, SharedCount * 2 + 16);
    SetLength(SharedBlocks, SharedCount * 2 + 16);
  end;
  SharedObjs[SharedCount] := O;
  SharedBlocks[SharedCount] := B;
  Inc(SharedCount);
  GCFinalizeHook := @SharedFinalizeHook;
end;

{ ---------------- atomics ---------------- }

function Width4Mask(Shift, Width: Integer): Cardinal; inline;
begin
  if Width = 1 then Result := Cardinal($FF) shl Shift else Result := Cardinal($FFFF) shl Shift;
end;

function Apply(Op: TAtomicOp; Old, Operand: QWord): QWord; inline;
begin
  case Op of
    aoAdd: Result := Old + Operand;
    aoSub: Result := Old - Operand;
    aoAnd: Result := Old and Operand;
    aoOr: Result := Old or Operand;
    aoXor: Result := Old xor Operand;
  else Result := Operand;
  end;
end;

{ 1- and 2-byte cells: a CAS on the aligned 4-byte word that holds them }
function SubWordUpdate(P: PByte; Width: Integer; Op: TAtomicOp; Operand: QWord; IsCAS: Boolean;
  Expected: QWord): QWord;
var W: PLongInt; Shift: Integer; Mask: Cardinal; Old, New_: LongInt; Cell, NewCell: Cardinal;
begin
  W := PLongInt(PtrUInt(P) and not PtrUInt(3));
  Shift := (PtrUInt(P) and 3) * 8;
  Mask := Width4Mask(Shift, Width);
  repeat
    Old := W^;
    Cell := (Cardinal(Old) and Mask) shr Shift;
    if IsCAS then
    begin
      if Cell <> (Cardinal(Expected) and (Mask shr Shift)) then Exit(Cell);
      NewCell := Cardinal(Operand);
    end
    else
      NewCell := Cardinal(Apply(Op, Cell, Operand));
    New_ := LongInt((Cardinal(Old) and not Mask) or ((NewCell shl Shift) and Mask));
  until InterlockedCompareExchange(W^, New_, Old) = Old;
  Result := Cell;
end;

function AtomicRMW(P: PByte; Width: Integer; Op: TAtomicOp; Operand: QWord): QWord;
var Old32, New32: LongInt; Old64, New64: Int64;
begin
  case Width of
    1, 2: Result := SubWordUpdate(P, Width, Op, Operand, False, 0);
    4:
      repeat
        Old32 := PLongInt(P)^;
        New32 := LongInt(Cardinal(Apply(Op, Cardinal(Old32), Operand)));
        Result := Cardinal(Old32);
      until InterlockedCompareExchange(PLongInt(P)^, New32, Old32) = Old32;
  else
    repeat
      Old64 := PInt64(P)^;
      New64 := Int64(Apply(Op, QWord(Old64), Operand));
      Result := QWord(Old64);
    until InterlockedCompareExchange64(PInt64(P)^, New64, Old64) = Old64;
  end;
end;

function AtomicCompareExchange(P: PByte; Width: Integer; Expected, Replacement: QWord): QWord;
begin
  case Width of
    1, 2: Result := SubWordUpdate(P, Width, aoExchange, Replacement, True, Expected);
    4: Result := Cardinal(InterlockedCompareExchange(PLongInt(P)^, LongInt(Cardinal(Replacement)),
         LongInt(Cardinal(Expected))));
  else Result := QWord(InterlockedCompareExchange64(PInt64(P)^, Int64(Replacement), Int64(Expected)));
  end;
end;

function AtomicLoad(P: PByte; Width: Integer): QWord;
begin
  { an aligned load is atomic on x86-64; the barrier orders it after earlier stores (seq_cst) }
  ReadWriteBarrier;
  case Width of
    1: Result := P^;
    2: Result := PWord(P)^;
    4: Result := PCardinal(P)^;
  else Result := InterlockedCompareExchange64(PInt64(P)^, 0, 0);
  end;
  ReadWriteBarrier;
end;

procedure AtomicStore(P: PByte; Width: Integer; V: QWord);
begin
  case Width of
    1, 2: SubWordUpdate(P, Width, aoExchange, V, False, 0);
    4: InterLockedExchange(PLongInt(P)^, LongInt(Cardinal(V)));
  else InterLockedExchange64(PInt64(P)^, Int64(V));
  end;
end;

{ ---------------- waiters ---------------- }

type
  PWaiter = ^TWaiter;
  PMailbox = ^TMailbox;
  TMailbox = record
    Event: PRTLEvent;
    Done: TBoundArray;   { tokens of notified async waiters, guarded by WaitLock }
    DoneCount: Integer;
  end;

  TWaiter = record
    Block: PSharedBlock;
    ByteIndex: Int64;
    Event: PRTLEvent;     { a sync waiter's }
    Notified: Boolean;
    Next: PWaiter;
    Mailbox: PMailbox;    { an async waiter's owner; nil for a sync waiter }
    Token: Integer;
  end;

var
  WaitLock: TRTLCriticalSection;
  WaitHead, WaitTail: PWaiter;   { all waiters, oldest first }

threadvar
  MyMailbox: PMailbox;

function Mailbox: PMailbox;
begin
  if MyMailbox = nil then
  begin
    New(MyMailbox);
    MyMailbox^.Event := RTLEventCreate;
    MyMailbox^.Done := nil;
    MyMailbox^.DoneCount := 0;
  end;
  Result := MyMailbox;
end;

procedure Unlink(W: PWaiter);
var P, Prev: PWaiter;
begin
  Prev := nil; P := WaitHead;
  while (P <> nil) and (P <> W) do begin Prev := P; P := P^.Next; end;
  if P = nil then Exit;
  if Prev = nil then WaitHead := W^.Next else Prev^.Next := W^.Next;
  if WaitTail = W then WaitTail := Prev;
end;

function SharedWait(Block: PSharedBlock; ByteIndex: Int64; Check: TWaitCheck; CheckData: Pointer;
  TimeoutMs: Double): TWaitResult;
var W: TWaiter; Ms: Cardinal; Deadline, Now_: QWord;
begin
  EnterCriticalSection(WaitLock);
  if not Check(CheckData) then
  begin
    LeaveCriticalSection(WaitLock);
    Exit(wrNotEqual);
  end;
  W.Block := Block; W.ByteIndex := ByteIndex; W.Notified := False; W.Next := nil; W.Mailbox := nil; W.Token := 0;
  W.Event := RTLEventCreate;
  if WaitTail = nil then WaitHead := @W else WaitTail^.Next := @W;
  WaitTail := @W;
  LeaveCriticalSection(WaitLock);
  if IsInfinite(TimeoutMs) then
    repeat
      RTLEventWaitFor(W.Event);
      EnterCriticalSection(WaitLock);
      Now_ := Ord(W.Notified);
      LeaveCriticalSection(WaitLock);
    until Now_ <> 0
  else
  begin
    Deadline := GetTickCount64 + QWord(Ceil(TimeoutMs));
    repeat
      EnterCriticalSection(WaitLock);
      Now_ := Ord(W.Notified);
      LeaveCriticalSection(WaitLock);
      if Now_ <> 0 then Break;
      Now_ := GetTickCount64;
      if Now_ >= Deadline then Break;
      if Deadline - Now_ > 1000000 then Ms := 1000000 else Ms := Deadline - Now_;
      RTLEventWaitFor(W.Event, Ms);
    until False;
  end;
  EnterCriticalSection(WaitLock);
  if W.Notified then Result := wrOk
  else
  begin
    Unlink(@W);
    Result := wrTimedOut;
  end;
  LeaveCriticalSection(WaitLock);
  RTLEventDestroy(W.Event);
end;

function SharedWaitAsyncAdd(Block: PSharedBlock; ByteIndex: Int64; Check: TWaitCheck; CheckData: Pointer;
  Token: Integer): TWaitResult;
var W: PWaiter; M: PMailbox;
begin
  M := Mailbox;
  EnterCriticalSection(WaitLock);
  if not Check(CheckData) then begin LeaveCriticalSection(WaitLock); Exit(wrNotEqual); end;
  New(W);
  W^.Block := Block; W^.ByteIndex := ByteIndex; W^.Event := nil; W^.Notified := False; W^.Next := nil;
  W^.Mailbox := M; W^.Token := Token;
  if WaitTail = nil then WaitHead := W else WaitTail^.Next := W;
  WaitTail := W;
  LeaveCriticalSection(WaitLock);
  Result := wrOk;
end;

function SharedAsyncCancel(Token: Integer): Boolean;
var P: PWaiter; M: PMailbox;
begin
  M := Mailbox;
  Result := False;
  EnterCriticalSection(WaitLock);
  P := WaitHead;
  while P <> nil do
  begin
    if (P^.Mailbox = M) and (P^.Token = Token) then
    begin
      Unlink(P);
      Dispose(P);
      Result := True;
      Break;
    end;
    P := P^.Next;
  end;
  LeaveCriticalSection(WaitLock);
end;

function SharedAsyncPoll(TimeoutMs: Cardinal): TBoundArray;
var M: PMailbox; I: Integer;
begin
  M := Mailbox;
  EnterCriticalSection(WaitLock);
  I := M^.DoneCount;
  LeaveCriticalSection(WaitLock);
  if (I = 0) and (TimeoutMs > 0) then RTLEventWaitFor(M^.Event, TimeoutMs);
  EnterCriticalSection(WaitLock);
  SetLength(Result, M^.DoneCount);
  for I := 0 to M^.DoneCount - 1 do Result[I] := M^.Done[I];
  M^.DoneCount := 0;
  LeaveCriticalSection(WaitLock);
end;

function SharedNotify(Block: PSharedBlock; ByteIndex: Int64; Count: Int64): Int64;
var P, Nx: PWaiter; M: PMailbox;
begin
  Result := 0;
  EnterCriticalSection(WaitLock);
  P := WaitHead;
  while (P <> nil) and ((Count < 0) or (Result < Count)) do
  begin
    Nx := P^.Next;
    if (P^.Block = Block) and (P^.ByteIndex = ByteIndex) then
    begin
      Unlink(P);
      if P^.Mailbox <> nil then
      begin
        { an async waiter: hand its token to the owning thread }
        M := P^.Mailbox;
        if M^.DoneCount = Length(M^.Done) then SetLength(M^.Done, M^.DoneCount * 2 + 8);
        M^.Done[M^.DoneCount] := P^.Token;
        Inc(M^.DoneCount);
        RTLEventSetEvent(M^.Event);
        Dispose(P);
      end
      else
      begin
        P^.Notified := True;
        RTLEventSetEvent(P^.Event);
      end;
      Inc(Result);
    end;
    P := Nx;
  end;
  LeaveCriticalSection(WaitLock);
end;

initialization
  InitCriticalSection(WaitLock);

finalization
  DoneCriticalSection(WaitLock);

end.
