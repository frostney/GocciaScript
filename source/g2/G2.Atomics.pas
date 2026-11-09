unit G2.Atomics;
{ SharedArrayBuffer (ES2026 25.2) and Atomics (25.4) on G2.Shared's blocks.

  A SharedArrayBuffer is an okArrayBuffer object with OFlags bit ofShared and its TSharedBlock in Native;
  BufferData and BufferLen (G2.Typed) read through it, so typed arrays and DataViews over one need nothing
  else. A block reaches another engine through G2.Agent, which wraps it in a new object there.

  BigInt64Array and BigUint64Array do not exist in G2 yet, so the BigInt forms of Atomics are absent. }
{$mode delphiunicode}{$H+}
{$POINTERMATH ON}

interface

uses
  Math,
  SysUtils,

  G2.Core,
  G2.Shared,
  G2.Typed,
  G2.VM;

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
  G2.Builtins2,
  G2.Promise;

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

function Arg(AArgs: TArgs; AArgc, I: Integer): TValue; inline;
begin
  if I <= AArgc then Result := AArgs[I] else Result := VUndefined;
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

function SAB_Ctor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var Len, MaxLen: Int64; Opts, M: TValue; B: PSharedBlock;
begin
  if CurNewTarget = VUndefined then Exit(ThrowTypeError('Constructor SharedArrayBuffer requires ''new'''));
  if not ToIndexV(Arg(AArgs, AArgc, 1), Len) then Exit(False);
  MaxLen := -1;
  Opts := Arg(AArgs, AArgc, 2);
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
  ARes := WrapSharedBlock(B);
  ReleaseBlock(B);   { the object holds the reference }
  Result := True;
end;

function ThisShared(AArgs: TArgs; const AName: UnicodeString; var B: PSharedBlock): Boolean;
begin
  if not IsSharedBuffer(AArgs[0]) then Exit(ThrowTypeError('SharedArrayBuffer.prototype.' + AName + ' requires a SharedArrayBuffer'));
  B := SharedBlockOf(AArgs[0]);
  Result := True;
end;

function SAB_ByteLength(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var B: PSharedBlock;
begin
  if not ThisShared(AArgs, 'byteLength', B) then Exit(False);
  ARes := MkNumber(BlockLength(B));
  Result := True;
end;

function SAB_Growable(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var B: PSharedBlock;
begin
  if not ThisShared(AArgs, 'growable', B) then Exit(False);
  if B^.MaxByteLength >= 0 then ARes := VTrue else ARes := VFalse;
  Result := True;
end;

function SAB_MaxByteLength(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var B: PSharedBlock;
begin
  if not ThisShared(AArgs, 'maxByteLength', B) then Exit(False);
  if B^.MaxByteLength >= 0 then ARes := MkNumber(B^.MaxByteLength) else ARes := MkNumber(BlockLength(B));
  Result := True;
end;

{ 25.2.5.3 SharedArrayBuffer.prototype.grow }
function SAB_Grow(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var B: PSharedBlock; N: Int64;
begin
  if not ThisShared(AArgs, 'grow', B) then Exit(False);
  if B^.MaxByteLength < 0 then Exit(ThrowTypeError('SharedArrayBuffer is not growable'));
  if not ToIndexV(Arg(AArgs, AArgc, 1), N) then Exit(False);
  if N > B^.MaxByteLength then Exit(ThrowError('RangeError', 'Invalid length for grow'));
  if not GrowBlock(B, N) then Exit(ThrowError('RangeError', 'SharedArrayBuffer cannot shrink'));
  ARes := VUndefined;
  Result := True;
end;

{ 25.2.5.7 SharedArrayBuffer.prototype.slice (without species: the result is a plain SharedArrayBuffer) }
function SAB_Slice(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var B, NB: PSharedBlock; Len, S, E: Int64; D: Double;
begin
  if not ThisShared(AArgs, 'slice', B) then Exit(False);
  Len := BlockLength(B);
  if not ToIntegerOrInf(Arg(AArgs, AArgc, 1), D) then Exit(False);
  if D < 0 then S := Trunc(Max(Len + D, 0)) else S := Trunc(Min(D, Len));
  if Arg(AArgs, AArgc, 2) = VUndefined then E := Len
  else
  begin
    if not ToIntegerOrInf(AArgs[2], D) then Exit(False);
    if D < 0 then E := Trunc(Max(Len + D, 0)) else E := Trunc(Min(D, Len));
  end;
  if E < S then E := S;
  NB := NewSharedBlock(E - S, -1);
  if NB = nil then Exit(ThrowError('RangeError', 'Array buffer allocation failed'));
  if E > S then Move((B^.Data + S)^, NB^.Data^, E - S);
  ARes := WrapSharedBlock(NB);
  ReleaseBlock(NB);
  Result := True;
end;

{ ---------------- Atomics ---------------- }

{ 25.4.3.1 ValidateIntegerTypedArray; Waitable: Int32Array only (no BigInt64Array yet) }
function ValidateIntTA(V: TValue; AWaitable: Boolean; var O: PObj): Boolean;
begin
  if not IsObject(V) or (ObjOf(V)^.OKind <> okTypedArray) then Exit(ThrowTypeError('Atomics operation on a non-TypedArray'));
  O := ObjOf(V);
  if AWaitable then
  begin
    if TAKind(O) <> taInt32 then Exit(ThrowTypeError('Atomics.wait and notify require an Int32Array'));
  end
  else if not (TAKind(O) in [taInt8, taUint8, taInt16, taUint16, taInt32, taUint32]) then
    Exit(ThrowTypeError('Atomics operations require an integer TypedArray'));
  Result := True;
end;

{ 25.4.3.2 ValidateAtomicAccess: the element's byte address }
function ValidateAccess(O: PObj; AIdx: TValue; var P: PByte; var AByteIdx: Int64): Boolean;
var I: Int64;
begin
  if not ToIndexV(AIdx, I) then Exit(False);
  if I >= TALength(O) then Exit(ThrowError('RangeError', 'Atomics access index out of range'));
  AByteIdx := Int64(O^.Pad) + I * TAElemSize[TAKind(O)];
  P := TAData(O) + I * TAElemSize[TAKind(O)];
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
function FromBits(AKind: Integer; B: QWord): TValue;
begin
  case AKind of
    taInt8: Result := MkInt(ShortInt(Byte(B)));
    taUint8: Result := MkInt(Byte(B));
    taInt16: Result := MkInt(SmallInt(Word(B)));
    taUint16: Result := MkInt(Word(B));
    taInt32: Result := MkInt(LongInt(Cardinal(B)));
  else Result := MkNumber(Cardinal(B));
  end;
end;

function DoRMW(AArgs: TArgs; AArgc: Integer; AOp: TAtomicOp; var ARes: TValue): Boolean;
var O: PObj; P: PByte; ByteIdx: Int64; D: Double; Old: QWord;
begin
  if not ValidateIntTA(Arg(AArgs, AArgc, 1), False, O) then Exit(False);
  if not ValidateAccess(O, Arg(AArgs, AArgc, 2), P, ByteIdx) then Exit(False);
  if not ToIntegerOrInf(Arg(AArgs, AArgc, 3), D) then Exit(False);
  O := ObjOf(AArgs[1]);   { a conversion may have run user code and collected garbage }
  P := TAData(O) + (ByteIdx - O^.Pad);
  Old := AtomicRMW(P, TAElemSize[TAKind(O)], AOp, Bits32(D));
  ARes := FromBits(TAKind(O), Old);
  Result := True;
end;

function Atomics_Add(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DoRMW(AArgs, AArgc, aoAdd, ARes); end;
function Atomics_And(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DoRMW(AArgs, AArgc, aoAnd, ARes); end;
function Atomics_Exchange(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DoRMW(AArgs, AArgc, aoExchange, ARes); end;
function Atomics_Or(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DoRMW(AArgs, AArgc, aoOr, ARes); end;
function Atomics_Sub(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DoRMW(AArgs, AArgc, aoSub, ARes); end;
function Atomics_Xor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DoRMW(AArgs, AArgc, aoXor, ARes); end;

function Atomics_CompareExchange(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var O: PObj; P: PByte; ByteIdx: Int64; E, R: Double; Old, Mask: QWord; W: Integer;
begin
  if not ValidateIntTA(Arg(AArgs, AArgc, 1), False, O) then Exit(False);
  if not ValidateAccess(O, Arg(AArgs, AArgc, 2), P, ByteIdx) then Exit(False);
  if not ToIntegerOrInf(Arg(AArgs, AArgc, 3), E) then Exit(False);
  if not ToIntegerOrInf(Arg(AArgs, AArgc, 4), R) then Exit(False);
  O := ObjOf(AArgs[1]);
  P := TAData(O) + (ByteIdx - O^.Pad);
  W := TAElemSize[TAKind(O)];
  if W = 4 then Mask := $FFFFFFFF else Mask := (QWord(1) shl (8 * W)) - 1;
  Old := AtomicCompareExchange(P, W, Bits32(E) and Mask, Bits32(R) and Mask);
  ARes := FromBits(TAKind(O), Old);
  Result := True;
end;

function Atomics_Load(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var O: PObj; P: PByte; ByteIdx: Int64;
begin
  if not ValidateIntTA(Arg(AArgs, AArgc, 1), False, O) then Exit(False);
  if not ValidateAccess(O, Arg(AArgs, AArgc, 2), P, ByteIdx) then Exit(False);
  ARes := FromBits(TAKind(O), AtomicLoad(P, TAElemSize[TAKind(O)]));
  Result := True;
end;

{ 25.4.11 Atomics.store returns the integer it stored, not the element }
function Atomics_Store(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var O: PObj; P: PByte; ByteIdx: Int64; D: Double;
begin
  if not ValidateIntTA(Arg(AArgs, AArgc, 1), False, O) then Exit(False);
  if not ValidateAccess(O, Arg(AArgs, AArgc, 2), P, ByteIdx) then Exit(False);
  if not ToIntegerOrInf(Arg(AArgs, AArgc, 3), D) then Exit(False);
  O := ObjOf(AArgs[1]);
  P := TAData(O) + (ByteIdx - O^.Pad);
  AtomicStore(P, TAElemSize[TAKind(O)], Bits32(D));
  ARes := MkNumber(D);
  Result := True;
end;

function Atomics_IsLockFree(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var D: Double;
begin
  if not ToIntegerOrInf(Arg(AArgs, AArgc, 1), D) then Exit(False);
  if (D = 1) or (D = 2) or (D = 4) or (D = 8) then ARes := VTrue else ARes := VFalse;
  Result := True;
end;

type
  TWaitCell = record
    P: PLongInt;
    Expected: LongInt;
  end;
  PWaitCell = ^TWaitCell;

function CellStillHolds(AData: Pointer): Boolean;
begin
  Result := PLongInt(PWaitCell(AData)^.P)^ = PWaitCell(AData)^.Expected;
end;

{ 25.4.13 Atomics.wait }
function Atomics_Wait(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var O: PObj; P: PByte; ByteIdx: Int64; D, T: Double; Cell: TWaitCell; R: TWaitResult; Buf: TValue;
begin
  if not ValidateIntTA(Arg(AArgs, AArgc, 1), True, O) then Exit(False);
  Buf := O^.Aux1;
  if not IsSharedBuffer(Buf) then Exit(ThrowTypeError('Atomics.wait requires a shared Int32Array'));
  if not ValidateAccess(O, Arg(AArgs, AArgc, 2), P, ByteIdx) then Exit(False);
  if not ToNumberV(Arg(AArgs, AArgc, 3), D) then Exit(False);
  if not ToNumberV(Arg(AArgs, AArgc, 4), T) then Exit(False);
  if IsNaN(T) then T := Infinity else if T < 0 then T := 0;
  if AgentCannotSuspend then Exit(ThrowTypeError('Atomics.wait cannot be called in this agent'));
  O := ObjOf(AArgs[1]);
  Cell.P := PLongInt(TAData(O) + (ByteIdx - O^.Pad));
  Cell.Expected := LongInt(Cardinal(Bits32(D)));
  R := SharedWait(SharedBlockOf(O^.Aux1), ByteIdx, @CellStillHolds, @Cell, T);
  case R of
    wrOk: ARes := StrFromUnicode('ok');
    wrNotEqual: ARes := StrFromUnicode('not-equal');
  else ARes := StrFromUnicode('timed-out');
  end;
  Result := True;
end;

{ 25.4.15 Atomics.notify }
function Atomics_Notify(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var O: PObj; P: PByte; ByteIdx: Int64; D: Double; C: Int64;
begin
  if not ValidateIntTA(Arg(AArgs, AArgc, 1), True, O) then Exit(False);
  if not ValidateAccess(O, Arg(AArgs, AArgc, 2), P, ByteIdx) then Exit(False);
  if Arg(AArgs, AArgc, 3) = VUndefined then C := -1
  else
  begin
    if not ToIntegerOrInf(AArgs[3], D) then Exit(False);
    if D <= 0 then C := 0 else if IsInfinite(D) or (D > 9e18) then C := -1 else C := Trunc(D);
  end;
  O := ObjOf(AArgs[1]);
  if not IsSharedBuffer(O^.Aux1) then begin ARes := MkInt(0); Exit(True); end;
  ARes := MkNumber(SharedNotify(SharedBlockOf(O^.Aux1), ByteIdx, C));
  Result := True;
end;

function ResultObject(AIsAsync: Boolean; V: TValue): TValue;
var O: TValue;
begin
  PushRoot(V);
  O := MkPtr(NewPlainObject(ObjectProto, 2));
  PushRoot(O);
  DefineOwn(O, AtomOf('async'), MkBool(AIsAsync), pfDefault);
  DefineOwn(RootAt(TempTop - 1), AtomOf('value'), RootAt(TempTop - 2), pfDefault);
  Result := RootAt(TempTop - 1);
  PopRoots(2);
end;

{ 25.4.14 Atomics.waitAsync }
function Atomics_WaitAsync(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var O: PObj; P: PByte; ByteIdx: Int64; D, T: Double; Cell: TWaitCell; Token: Integer; Pr: TValue;
begin
  if not ValidateIntTA(Arg(AArgs, AArgc, 1), True, O) then Exit(False);
  if not IsSharedBuffer(O^.Aux1) then Exit(ThrowTypeError('Atomics.waitAsync requires a shared Int32Array'));
  if not ValidateAccess(O, Arg(AArgs, AArgc, 2), P, ByteIdx) then Exit(False);
  if not ToNumberV(Arg(AArgs, AArgc, 3), D) then Exit(False);
  if not ToNumberV(Arg(AArgs, AArgc, 4), T) then Exit(False);
  if IsNaN(T) then T := Infinity else if T < 0 then T := 0;
  O := ObjOf(AArgs[1]);
  Cell.P := PLongInt(TAData(O) + (ByteIdx - O^.Pad));
  Cell.Expected := LongInt(Cardinal(Bits32(D)));
  if Cell.P^ <> Cell.Expected then begin ARes := ResultObject(False, StrFromUnicode('not-equal')); Exit(True); end;
  if T = 0 then begin ARes := ResultObject(False, StrFromUnicode('timed-out')); Exit(True); end;
  Pr := NewPendingPromise;
  PushRoot(Pr);
  Token := AsyncCount;
  if SharedWaitAsyncAdd(SharedBlockOf(ObjOf(AArgs[1])^.Aux1), ByteIdx, @CellStillHolds, @Cell, Token) = wrNotEqual then
  begin
    PopRoots(1);
    ARes := ResultObject(False, StrFromUnicode('not-equal'));
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
  ARes := ResultObject(True, RootAt(TempTop - 1));
  PopRoots(1);
  Result := True;
end;

procedure SettleAsync(AToken: Integer; const AOutcome: UnicodeString);
var Pr: TValue;
begin
  if not AsyncLive[AToken] then Exit;
  AsyncLive[AToken] := False;
  Dec(AsyncPending);
  Pr := ObjOf(AsyncPromises)^.Elements^.Data[AToken];
  ObjOf(AsyncPromises)^.Elements^.Data[AToken] := VUndefined;
  ResolvePendingPromise(Pr, StrFromUnicode(AOutcome));
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
function Atomics_Pause(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var V: TValue;
begin
  V := Arg(AArgs, AArgc, 1);
  if (V <> VUndefined) and (not IsNumber(V) or IsNaN(NumOf(V)) or IsInfinite(NumOf(V)) or (Frac(NumOf(V)) <> 0)) then
    Exit(ThrowTypeError('Atomics.pause argument must be an integral Number'));
  ReadWriteBarrier;
  ARes := VUndefined;
  Result := True;
end;

procedure Getter(AObj: TValue; const AName: UnicodeString; AFn: TNativeFn);
var G: TValue;
begin
  G := NewNativeFunction(AFn, 'get ' + AName, 0);
  PushRoot(G);
  DefineOwn(AObj, AtomOf(AName), MkPtr(NewAccessor(RootAt(TempTop - 1), VUndefined)), pfAccessor or pfConfigurable);
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
