unit G2.Atomics;
{ SharedArrayBuffer (ES2026 25.2) and Atomics (25.4) on G2.Shared's blocks.

  A SharedArrayBuffer is an okArrayBuffer object with OFlags bit ofShared and its TSharedBlock in Native;
  BufferData and BufferLen (G2.Typed) read through it, so typed arrays and DataViews over one need nothing
  else. A block reaches another engine through G2.Agent, which wraps it in a new object there.

  BigInt64Array and BigUint64Array do not exist in G2 yet, so the BigInt forms of Atomics are absent. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}

interface

uses
  SysUtils, Math, G2.Core, G2.VM, G2.Typed, G2.Shared;

const
  ofShared = $20;

procedure InitAtomics;
{ the host's event loop for Atomics.waitAsync: after a script and its jobs, resolves this engine's
  async waits as other threads notify them or their timeouts pass, running the jobs that follow, until none
  is pending. False if a job threw. }
function RunHostLoop: Boolean;
{ a SharedArrayBuffer object of this engine for B; takes a reference to B }
function WrapSharedBlock(B: PSharedBlock): TValue;
function IsSharedBuffer(V: TValue): Boolean;
function SharedBlockOf(V: TValue): PSharedBlock;

threadvar
  { False in an agent that may not block (25.4.3.14 AgentCanSuspend); the main agent and G2's agents can }
  AgentCannotSuspend: Boolean;

implementation

uses
  G2.Builtins2, G2.Promise;

threadvar
  SharedBufferProto: TValue;
  AtomicsObj: TValue;
  Scratch: TValue;
  { pending waitAsync promises: AsyncPromises (a rooted array) holds the promise of token T at index T }
  AsyncPromises: TValue;
  AsyncDeadlines: array of QWord;   { High(QWord): no timeout }
  AsyncLive: array of Boolean;
  AsyncCount, AsyncPending: Integer;

type
  TArgs = PValue;

function Arg(Args: TArgs; Argc, I: Integer): TValue; inline;
begin
  if I <= Argc then Result := Args[I] else Result := VUndefined;
end;

function IsSharedBuffer(V: TValue): Boolean;
begin
  Result := IsObject(V) and (ObjOf(V)^.OKind = okArrayBuffer) and ((ObjOf(V)^.OFlags and ofShared) <> 0);
end;

function SharedBlockOf(V: TValue): PSharedBlock;
begin
  Result := PSharedBlock(ObjOf(V)^.Native);
end;

function WrapSharedBlock(B: PSharedBlock): TValue;
var O: PObj;
begin
  RetainBlock(B);
  O := NewObject(RootShapeFor(SharedBufferProto), 0, okArrayBuffer);
  O^.OFlags := O^.OFlags or ofShared;
  O^.Native := B;
  O^.Aux1 := VUndefined;
  O^.Aux2 := MkInt(0);
  RegisterSharedBuffer(O, B);
  Result := MkPtr(O);
end;

{ 7.1.22 ToIndex }
function ToIndexV(V: TValue; var R: Int64): Boolean;
var D: Double;
begin
  if V = VUndefined then begin R := 0; Exit(True); end;
  if not ToNumberV(V, D) then Exit(False);
  if IsNaN(D) then D := 0 else D := Int(D);
  if (D < 0) or (D > 9007199254740991.0) then Exit(ThrowError('RangeError', 'Invalid index'));
  R := Trunc(D);
  Result := True;
end;

{ 7.1.5 ToIntegerOrInfinity, as a Double }
function ToIntegerOrInf(V: TValue; var D: Double): Boolean;
begin
  if not ToNumberV(V, D) then Exit(False);
  if IsNaN(D) then D := 0 else if not IsInfinite(D) then D := Int(D);
  if D = 0 then D := 0;   { -0 -> +0 }
  Result := True;
end;

{ ---------------- SharedArrayBuffer ---------------- }

function SAB_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, MaxLen: Int64; Opts, M: TValue; B: PSharedBlock;
begin
  if CurNewTarget = VUndefined then Exit(ThrowTypeError('Constructor SharedArrayBuffer requires ''new'''));
  if not ToIndexV(Arg(Args, Argc, 1), Len) then Exit(False);
  MaxLen := -1;
  Opts := Arg(Args, Argc, 2);
  if IsObject(Opts) then
  begin
    if not GetV(Opts, AtomOf('maxByteLength'), M) then Exit(False);
    if M <> VUndefined then
    begin
      if not ToIndexV(M, MaxLen) then Exit(False);
      if Len > MaxLen then Exit(ThrowError('RangeError', 'byteLength exceeds maxByteLength'));
    end;
  end;
  B := NewSharedBlock(Len, MaxLen);
  if B = nil then Exit(ThrowError('RangeError', 'Array buffer allocation failed'));
  Res := WrapSharedBlock(B);
  ReleaseBlock(B);   { the object holds the reference }
  Result := True;
end;

function ThisShared(Args: TArgs; const Name: UnicodeString; var B: PSharedBlock): Boolean;
begin
  if not IsSharedBuffer(Args[0]) then Exit(ThrowTypeError('SharedArrayBuffer.prototype.' + Name + ' requires a SharedArrayBuffer'));
  B := SharedBlockOf(Args[0]);
  Result := True;
end;

function SAB_ByteLength(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var B: PSharedBlock;
begin
  if not ThisShared(Args, 'byteLength', B) then Exit(False);
  Res := MkNumber(BlockLength(B));
  Result := True;
end;

function SAB_Growable(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var B: PSharedBlock;
begin
  if not ThisShared(Args, 'growable', B) then Exit(False);
  if B^.MaxByteLength >= 0 then Res := VTrue else Res := VFalse;
  Result := True;
end;

function SAB_MaxByteLength(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var B: PSharedBlock;
begin
  if not ThisShared(Args, 'maxByteLength', B) then Exit(False);
  if B^.MaxByteLength >= 0 then Res := MkNumber(B^.MaxByteLength) else Res := MkNumber(BlockLength(B));
  Result := True;
end;

{ 25.2.5.3 SharedArrayBuffer.prototype.grow }
function SAB_Grow(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var B: PSharedBlock; N: Int64;
begin
  if not ThisShared(Args, 'grow', B) then Exit(False);
  if B^.MaxByteLength < 0 then Exit(ThrowTypeError('SharedArrayBuffer is not growable'));
  if not ToIndexV(Arg(Args, Argc, 1), N) then Exit(False);
  if N > B^.MaxByteLength then Exit(ThrowError('RangeError', 'Invalid length for grow'));
  if not GrowBlock(B, N) then Exit(ThrowError('RangeError', 'SharedArrayBuffer cannot shrink'));
  Res := VUndefined;
  Result := True;
end;

{ 25.2.5.7 SharedArrayBuffer.prototype.slice (without species: the result is a plain SharedArrayBuffer) }
function SAB_Slice(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var B, NB: PSharedBlock; Len, S, E: Int64; D: Double;
begin
  if not ThisShared(Args, 'slice', B) then Exit(False);
  Len := BlockLength(B);
  if not ToIntegerOrInf(Arg(Args, Argc, 1), D) then Exit(False);
  if D < 0 then S := Trunc(Max(Len + D, 0)) else S := Trunc(Min(D, Len));
  if Arg(Args, Argc, 2) = VUndefined then E := Len
  else
  begin
    if not ToIntegerOrInf(Args[2], D) then Exit(False);
    if D < 0 then E := Trunc(Max(Len + D, 0)) else E := Trunc(Min(D, Len));
  end;
  if E < S then E := S;
  NB := NewSharedBlock(E - S, -1);
  if NB = nil then Exit(ThrowError('RangeError', 'Array buffer allocation failed'));
  if E > S then Move((B^.Data + S)^, NB^.Data^, E - S);
  Res := WrapSharedBlock(NB);
  ReleaseBlock(NB);
  Result := True;
end;

{ ---------------- Atomics ---------------- }

{ 25.4.3.1 ValidateIntegerTypedArray: an attached, in-bounds integer typed array; Waitable: Int32Array or
  BigInt64Array }
function ValidateIntTA(V: TValue; Waitable: Boolean; var O: PObj): Boolean;
begin
  if not IsObject(V) or (ObjOf(V)^.OKind <> okTypedArray) then Exit(ThrowTypeError('Atomics operation on a non-TypedArray'));
  O := ObjOf(V);
  if TAOutOfBounds(O) then Exit(ThrowTypeError('Atomics operation on a detached or out-of-bounds TypedArray'));
  if Waitable then
  begin
    if not (TAKind(O) in [taInt32, taBigInt64]) then Exit(ThrowTypeError('Atomics.wait and notify require an Int32Array or a BigInt64Array'));
  end
  else if not (TAKind(O) in [taInt8, taUint8, taInt16, taUint16, taInt32, taUint32, taBigInt64, taBigUint64]) then
    Exit(ThrowTypeError('Atomics operations require an integer TypedArray'));
  Result := True;
end;

{ 25.4.3.2 ValidateAtomicAccess: the element's byte address }
function ValidateAccess(O: PObj; Idx: TValue; var P: PByte; var ByteIdx: Int64): Boolean;
var I: Int64;
begin
  if not ToIndexV(Idx, I) then Exit(False);
  if I >= TALength(O) then Exit(ThrowError('RangeError', 'Atomics access index out of range'));
  ByteIdx := Int64(O^.Pad) + I * TAElemSize[TAKind(O)];
  P := TAData(O) + I * TAElemSize[TAKind(O)];
  Result := True;
end;

{ 25.4.3.4 RevalidateAtomicAccess: after the value's conversion, which may have detached or shrunk the buffer }
function Revalidate(O: PObj; ByteIdx: Int64): Boolean;
begin
  if TAOutOfBounds(O) then Exit(ThrowTypeError('Atomics operation on a detached or out-of-bounds TypedArray'));
  if ByteIdx - O^.Pad >= TALength(O) * TAElemSize[TAKind(O)] then
    Exit(ThrowError('RangeError', 'Atomics access index out of range'));
  Result := True;
end;

function Bits32(D: Double): QWord; forward;

{ the operand of an operation on O: ToBigInt for a BigInt array (its two's complement bits), else
  ToIntegerOrInfinity; V the converted value (what store returns) }
function OperandBits(O: PObj; X: TValue; var Bits: QWord; var V: TValue): Boolean;
var D: Double;
begin
  if TAKind(O) in [taBigInt64, taBigUint64] then
  begin
    if not ToBigIntV(X, V) then Exit(False);
    Bits := BigToU64(V);
    Exit(True);
  end;
  if not ToIntegerOrInf(X, D) then Exit(False);
  if D = 0 then D := 0;   { -0 stores and returns +0 }
  Bits := Bits32(D);
  V := MkNumber(D);
  Result := True;
end;

{ the element's bits for D (an integer or an infinity), modulo 2^32: NumericToRawBytes through ToInt8..ToUint32 }
function Bits32(D: Double): QWord;
begin
  if IsNaN(D) or IsInfinite(D) then Exit(0);
  D := D - 4294967296.0 * DFloor(D / 4294967296.0);
  Result := QWord(Trunc(D)) and $FFFFFFFF;
end;

{ RawBytesToNumeric }
function FromBits(Kind: Integer; B: QWord): TValue;
begin
  case Kind of
    taBigInt64: Result := BigFromInt64(Int64(B));
    taBigUint64: Result := BigFromUInt64(B);
    taInt8: Result := MkInt(ShortInt(Byte(B)));
    taUint8: Result := MkInt(Byte(B));
    taInt16: Result := MkInt(SmallInt(Word(B)));
    taUint16: Result := MkInt(Word(B));
    taInt32: Result := MkInt(LongInt(Cardinal(B)));
  else Result := MkNumber(Cardinal(B));
  end;
end;

function DoRMW(Args: TArgs; Argc: Integer; Op: TAtomicOp; var Res: TValue): Boolean;
var O: PObj; P: PByte; ByteIdx: Int64; Old, Bits: QWord; V: TValue;
begin
  if not ValidateIntTA(Arg(Args, Argc, 1), False, O) then Exit(False);
  if not ValidateAccess(O, Arg(Args, Argc, 2), P, ByteIdx) then Exit(False);
  if not OperandBits(O, Arg(Args, Argc, 3), Bits, V) then Exit(False);
  O := ObjOf(Args[1]);   { a conversion may have run user code and collected garbage }
  if not Revalidate(O, ByteIdx) then Exit(False);
  P := TAData(O) + (ByteIdx - O^.Pad);
  Old := AtomicRMW(P, TAElemSize[TAKind(O)], Op, Bits);
  Res := FromBits(TAKind(O), Old);
  Result := True;
end;

function Atomics_Add(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DoRMW(Args, Argc, aoAdd, Res); end;
function Atomics_And(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DoRMW(Args, Argc, aoAnd, Res); end;
function Atomics_Exchange(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DoRMW(Args, Argc, aoExchange, Res); end;
function Atomics_Or(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DoRMW(Args, Argc, aoOr, Res); end;
function Atomics_Sub(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DoRMW(Args, Argc, aoSub, Res); end;
function Atomics_Xor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DoRMW(Args, Argc, aoXor, Res); end;

function Atomics_CompareExchange(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var O: PObj; P: PByte; ByteIdx: Int64; EB, RB, Old, Mask: QWord; W: Integer; V: TValue;
begin
  if not ValidateIntTA(Arg(Args, Argc, 1), False, O) then Exit(False);
  if not ValidateAccess(O, Arg(Args, Argc, 2), P, ByteIdx) then Exit(False);
  if not OperandBits(O, Arg(Args, Argc, 3), EB, V) then Exit(False);
  O := ObjOf(Args[1]);
  if not OperandBits(O, Arg(Args, Argc, 4), RB, V) then Exit(False);
  O := ObjOf(Args[1]);
  if not Revalidate(O, ByteIdx) then Exit(False);
  P := TAData(O) + (ByteIdx - O^.Pad);
  W := TAElemSize[TAKind(O)];
  if W = 8 then Mask := QWord($FFFFFFFFFFFFFFFF)
  else if W = 4 then Mask := $FFFFFFFF else Mask := (QWord(1) shl (8 * W)) - 1;
  Old := AtomicCompareExchange(P, W, EB and Mask, RB and Mask);
  Res := FromBits(TAKind(O), Old);
  Result := True;
end;

function Atomics_Load(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var O: PObj; P: PByte; ByteIdx: Int64;
begin
  if not ValidateIntTA(Arg(Args, Argc, 1), False, O) then Exit(False);
  if not ValidateAccess(O, Arg(Args, Argc, 2), P, ByteIdx) then Exit(False);
  Res := FromBits(TAKind(O), AtomicLoad(P, TAElemSize[TAKind(O)]));
  Result := True;
end;

{ 25.4.11 Atomics.store returns the integer it stored, not the element }
function Atomics_Store(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var O: PObj; P: PByte; ByteIdx: Int64; Bits: QWord; V: TValue;
begin
  if not ValidateIntTA(Arg(Args, Argc, 1), False, O) then Exit(False);
  if not ValidateAccess(O, Arg(Args, Argc, 2), P, ByteIdx) then Exit(False);
  if not OperandBits(O, Arg(Args, Argc, 3), Bits, V) then Exit(False);
  O := ObjOf(Args[1]);
  if not Revalidate(O, ByteIdx) then Exit(False);
  P := TAData(O) + (ByteIdx - O^.Pad);
  AtomicStore(P, TAElemSize[TAKind(O)], Bits);
  Res := V;
  Result := True;
end;

function Atomics_IsLockFree(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var D: Double;
begin
  if not ToIntegerOrInf(Arg(Args, Argc, 1), D) then Exit(False);
  if (D = 1) or (D = 2) or (D = 4) or (D = 8) then Res := VTrue else Res := VFalse;
  Result := True;
end;

type
  TWaitCell = record
    P: PByte;
    Wide: Boolean;       { a BigInt64Array element }
    Expected: Int64;
  end;
  PWaitCell = ^TWaitCell;

function CellValue(const C: TWaitCell): Int64; inline;
begin
  if C.Wide then Result := PInt64(C.P)^ else Result := PLongInt(C.P)^;
end;

function CellStillHolds(Data: Pointer): Boolean;
begin
  Result := CellValue(PWaitCell(Data)^) = PWaitCell(Data)^.Expected;
end;

{ the value Atomics.wait / waitAsync compare against: ToBigInt64 or ToInt32 }
function WaitExpected(O: PObj; X: TValue; var Cell: TWaitCell): Boolean;
var D: Double; V: TValue;
begin
  Cell.Wide := TAKind(O) = taBigInt64;
  if Cell.Wide then
  begin
    if not ToBigIntV(X, V) then Exit(False);
    Cell.Expected := Int64(BigToU64(V));
  end
  else
  begin
    if not ToNumberV(X, D) then Exit(False);
    Cell.Expected := LongInt(Cardinal(Bits32(D)));
  end;
  Result := True;
end;

{ 25.4.13 Atomics.wait }
function Atomics_Wait(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var O: PObj; P: PByte; ByteIdx: Int64; D, T: Double; Cell: TWaitCell; R: TWaitResult; Buf: TValue;
begin
  if not ValidateIntTA(Arg(Args, Argc, 1), True, O) then Exit(False);
  Buf := O^.Aux1;
  if not IsSharedBuffer(Buf) then Exit(ThrowTypeError('Atomics.wait requires a shared Int32Array'));
  if not ValidateAccess(O, Arg(Args, Argc, 2), P, ByteIdx) then Exit(False);
  if not WaitExpected(O, Arg(Args, Argc, 3), Cell) then Exit(False);
  if not ToNumberV(Arg(Args, Argc, 4), T) then Exit(False);
  if IsNaN(T) then T := Infinity else if T < 0 then T := 0;
  if AgentCannotSuspend then Exit(ThrowTypeError('Atomics.wait cannot be called in this agent'));
  O := ObjOf(Args[1]);
  Cell.P := TAData(O) + (ByteIdx - O^.Pad);
  R := SharedWait(SharedBlockOf(O^.Aux1), ByteIdx, @CellStillHolds, @Cell, T);
  case R of
    wrOk: Res := StrFromUnicode('ok');
    wrNotEqual: Res := StrFromUnicode('not-equal');
  else Res := StrFromUnicode('timed-out');
  end;
  Result := True;
end;

{ 25.4.15 Atomics.notify }
function Atomics_Notify(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var O: PObj; P: PByte; ByteIdx: Int64; D: Double; C: Int64;
begin
  if not ValidateIntTA(Arg(Args, Argc, 1), True, O) then Exit(False);
  if not ValidateAccess(O, Arg(Args, Argc, 2), P, ByteIdx) then Exit(False);
  if Arg(Args, Argc, 3) = VUndefined then C := -1
  else
  begin
    if not ToIntegerOrInf(Args[3], D) then Exit(False);
    if D <= 0 then C := 0 else if IsInfinite(D) or (D > 9e18) then C := -1 else C := Trunc(D);
  end;
  O := ObjOf(Args[1]);
  if not IsSharedBuffer(O^.Aux1) then begin Res := MkInt(0); Exit(True); end;
  Res := MkNumber(SharedNotify(SharedBlockOf(O^.Aux1), ByteIdx, C));
  Result := True;
end;

function ResultObject(IsAsync: Boolean; V: TValue): TValue;
var O: TValue;
begin
  PushRoot(V);
  O := MkPtr(NewPlainObject(ObjectProto, 2));
  PushRoot(O);
  DefineOwn(O, AtomOf('async'), MkBool(IsAsync), pfDefault);
  DefineOwn(RootAt(TempTop - 1), AtomOf('value'), RootAt(TempTop - 2), pfDefault);
  Result := RootAt(TempTop - 1);
  PopRoots(2);
end;

{ 25.4.14 Atomics.waitAsync }
function Atomics_WaitAsync(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var O: PObj; P: PByte; ByteIdx: Int64; D, T: Double; Cell: TWaitCell; Token: Integer; Pr: TValue;
begin
  if not ValidateIntTA(Arg(Args, Argc, 1), True, O) then Exit(False);
  if not IsSharedBuffer(O^.Aux1) then Exit(ThrowTypeError('Atomics.waitAsync requires a shared Int32Array'));
  if not ValidateAccess(O, Arg(Args, Argc, 2), P, ByteIdx) then Exit(False);
  if not WaitExpected(O, Arg(Args, Argc, 3), Cell) then Exit(False);
  if not ToNumberV(Arg(Args, Argc, 4), T) then Exit(False);
  if IsNaN(T) then T := Infinity else if T < 0 then T := 0;
  O := ObjOf(Args[1]);
  Cell.P := TAData(O) + (ByteIdx - O^.Pad);
  if CellValue(Cell) <> Cell.Expected then begin Res := ResultObject(False, StrFromUnicode('not-equal')); Exit(True); end;
  if T = 0 then begin Res := ResultObject(False, StrFromUnicode('timed-out')); Exit(True); end;
  Pr := NewPendingPromise;
  PushRoot(Pr);
  Token := AsyncCount;
  if SharedWaitAsyncAdd(SharedBlockOf(ObjOf(Args[1])^.Aux1), ByteIdx, @CellStillHolds, @Cell, Token) = wrNotEqual then
  begin
    PopRoots(1);
    Res := ResultObject(False, StrFromUnicode('not-equal'));
    Exit(True);
  end;
  ArrayPush(ObjOf(AsyncPromises), RootAt(TempTop - 1));
  if AsyncCount = Length(AsyncDeadlines) then
  begin
    SetLength(AsyncDeadlines, AsyncCount * 2 + 8);
    SetLength(AsyncLive, AsyncCount * 2 + 8);
  end;
  if IsInfinite(T) or (T > 1e15) then AsyncDeadlines[Token] := High(QWord)
  else AsyncDeadlines[Token] := GetTickCount64 + QWord(Ceil(T));
  AsyncLive[Token] := True;
  Inc(AsyncCount);
  Inc(AsyncPending);
  Res := ResultObject(True, RootAt(TempTop - 1));
  PopRoots(1);
  Result := True;
end;

procedure SettleAsync(Token: Integer; const Outcome: UnicodeString);
var Pr: TValue;
begin
  if not AsyncLive[Token] then Exit;
  AsyncLive[Token] := False;
  Dec(AsyncPending);
  Pr := ObjOf(AsyncPromises)^.Elements^.Data[Token];
  ObjOf(AsyncPromises)^.Elements^.Data[Token] := VUndefined;
  ResolvePendingPromise(Pr, StrFromUnicode(Outcome));
end;

function RunHostLoop: Boolean;
var Done: TBoundArray; I: Integer; Now_, Next: QWord; Wait: Cardinal;
begin
  Result := True;
  while AsyncPending > 0 do
  begin
    Now_ := GetTickCount64;
    Next := High(QWord);
    for I := 0 to AsyncCount - 1 do
      if AsyncLive[I] and (AsyncDeadlines[I] < Next) then Next := AsyncDeadlines[I];
    if Next <= Now_ then Wait := 0
    else if Next - Now_ > 1000 then Wait := 1000
    else Wait := Next - Now_;
    Done := SharedAsyncPoll(Wait);
    for I := 0 to High(Done) do SettleAsync(Done[I], 'ok');
    Now_ := GetTickCount64;
    for I := 0 to AsyncCount - 1 do
      if AsyncLive[I] and (AsyncDeadlines[I] <= Now_) and SharedAsyncCancel(I) then SettleAsync(I, 'timed-out');
    if not RunMicrotasks then Exit(False);
  end;
end;

{ Atomics.pause: a hint; validates its argument }
function Atomics_Pause(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var V: TValue;
begin
  V := Arg(Args, Argc, 1);
  if (V <> VUndefined) and (not IsNumber(V) or IsNaN(NumOf(V)) or IsInfinite(NumOf(V)) or (Frac(NumOf(V)) <> 0)) then
    Exit(ThrowTypeError('Atomics.pause argument must be an integral Number'));
  ReadWriteBarrier;
  Res := VUndefined;
  Result := True;
end;

procedure Getter(Obj: TValue; const Name: UnicodeString; Fn: TNativeFn);
var G: TValue;
begin
  G := NewNativeFunction(Fn, 'get ' + Name, 0);
  PushRoot(G);
  DefineOwn(Obj, AtomOf(Name), MkPtr(NewAccessor(RootAt(TempTop - 1), VUndefined)), pfAccessor or pfConfigurable);
  PopRoots(1);
end;

procedure InitAtomics;
var Sym, C: TValue; TagKey: PStr;
begin
  AddRoot(@SharedBufferProto); AddRoot(@AtomicsObj); AddRoot(@Scratch); AddRoot(@AsyncPromises);
  AsyncPromises := MkPtr(NewArray(0, 8));
  AsyncCount := 0; AsyncPending := 0; AsyncDeadlines := nil; AsyncLive := nil;
  SharedBufferProto := MkPtr(NewPlainObject(ObjectProto, 8));
  C := NewNativeFunction(SAB_Ctor, 'SharedArrayBuffer', 1);
  Scratch := C;
  DefineOwn(Scratch, AtomPrototype, SharedBufferProto, 0);
  DefineOwn(SharedBufferProto, AtomConstructor, Scratch, pfHidden);
  DefineOwn(GlobalObj, AtomOf('SharedArrayBuffer'), Scratch, pfHidden);
  Getter(SharedBufferProto, 'byteLength', SAB_ByteLength);
  Getter(SharedBufferProto, 'growable', SAB_Growable);
  Getter(SharedBufferProto, 'maxByteLength', SAB_MaxByteLength);
  DefineNative(SharedBufferProto, 'grow', SAB_Grow, 1);
  DefineNative(SharedBufferProto, 'slice', SAB_Slice, 2);

  AtomicsObj := MkPtr(NewPlainObject(ObjectProto, 16));
  DefineNative(AtomicsObj, 'add', Atomics_Add, 3);
  DefineNative(AtomicsObj, 'and', Atomics_And, 3);
  DefineNative(AtomicsObj, 'compareExchange', Atomics_CompareExchange, 4);
  DefineNative(AtomicsObj, 'exchange', Atomics_Exchange, 3);
  DefineNative(AtomicsObj, 'isLockFree', Atomics_IsLockFree, 1);
  DefineNative(AtomicsObj, 'load', Atomics_Load, 2);
  DefineNative(AtomicsObj, 'notify', Atomics_Notify, 3);
  DefineNative(AtomicsObj, 'or', Atomics_Or, 3);
  DefineNative(AtomicsObj, 'pause', Atomics_Pause, 0);
  DefineNative(AtomicsObj, 'store', Atomics_Store, 3);
  DefineNative(AtomicsObj, 'sub', Atomics_Sub, 3);
  DefineNative(AtomicsObj, 'wait', Atomics_Wait, 4);
  DefineNative(AtomicsObj, 'waitAsync', Atomics_WaitAsync, 4);
  DefineNative(AtomicsObj, 'xor', Atomics_Xor, 3);
  DefineOwn(GlobalObj, AtomOf('Atomics'), AtomicsObj, pfHidden);
  { @@toStringTag: Symbol.toStringTag is a symbol key registered by the builtins }
  if GetV(GlobalObj, AtomOf('Symbol'), Sym) and GetV(Sym, AtomOf('toStringTag'), Sym) and IsPtr(Sym) then
  begin
    TagKey := PStr(PPointer(PByte(PtrOf(Sym)) + 16)^);
    DefineOwn(AtomicsObj, TagKey, StrFromUnicode('Atomics'), pfConfigurable);
    DefineOwn(SharedBufferProto, TagKey, StrFromUnicode('SharedArrayBuffer'), pfConfigurable);
  end;
end;

end.
