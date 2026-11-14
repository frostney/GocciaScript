{ ---------------- the baseline JIT (x86-64): a template's fast paths as straight-line machine code ----------------

  The frame stays the interpreter's: the registers live in the value stack and no machine register holds a value
  across instructions, so the code can stop before any instruction and the interpreter carry on there. Every guard
  that fails jumps to its instruction's exit, which returns that instruction's PC; the instruction then runs in
  FastLoop or Execute. Calls, returns, allocation and anything that can throw stay the interpreter's. FastLoop
  enters the code at a frame's first instruction, on a return into a compiled frame and at a backward jump; the
  code runs until an instruction it does not take, in the same frame.

  Machine registers: rbx the frame's registers (R), r12 the frame record, ebp the flags; rax, rcx, rdx, rsi,
  rdi, r8-r11 and xmm0-1 are scratch. The value stack and the nursery's bounds are this thread's threadvars,
  read fs-relative as FPC's own code reads them. Entry: rdi R, rsi the instruction's code, rdx Fr, ecx the
  flags (the System V order FPC uses). }

{$IFDEF CPUX86_64}
type
  { the code at Target (an instruction's entry, from JitTable) for frame Fr; Flags 1: XJitCall's frame }
  TJitFn = function(R: PValue; Target: Pointer; Fr: PFrame; Flags: Integer): Integer;
  TJitFix = record
    At: Integer;        { the rel32 field }
    Target: Integer;    { >= 0 a bytecode PC's code; < 0 the exit of PC -1 - Target }
  end;

const
  jAX = 0; jCX = 1; jDX = 2; jBX = 3; jSP = 4; jBP = 5; jSI = 6; jDI = 7;
  jR8 = 8; jR9 = 9; jR10 = 10; jR11 = 11; jR12 = 12; jR13 = 13; jR14 = 14; jR15 = 15;
  ccO = 0; ccB = 2; ccAE = 3; ccE = 4; ccNE = 5; ccBE = 6; ccA = 7; ccS = 8; ccP = $A; ccL = $C; ccGE = $D;
  ccLE = $E; ccG = $F;
  JIT_CHUNK = 4 * 1024 * 1024;

var
  JitEnabled: Integer = -1;        { -1 not read yet (G2_JIT=0 turns the JIT off), 0 off, 1 on }
  JitBuf: array of Byte;           { the code being emitted }
  JitCap: Integer = 0;             { JitGrow's limit: past it the next JB, JD or JQ grows JitBuf }
  JitLen: Integer;
  JitFixes: array of TJitFix;
  JitFixCount: Integer;
  JitNative: array of Integer;     { each PC's code offset }
  JitExitOf: array of Integer;     { each PC's exit stub offset (-1 none yet) }
  JitTakenAt: array of Boolean;    { each PC: compiled (an entry), or only its exit }
  JitChunk: PByte = nil;           { executable memory, bump-allocated }
  JitChunkUsed: PtrUInt = 0;
  JitMainThread: TThreadID;        { the thread the VM unit started on: the only one that compiles }
  JitTaken: Boolean;               { JitInstr compiled the instruction (False: its code is only its exit) }

const
  JIT_HOT = 20;                    { a template's entries before it is compiled (G2_JIT_HOT overrides) }
  JIT_MAX_DEPTH = 256;             { code activations nested on the native stack where its bounds are unknown }
  JIT_STACK_MARGIN = 256 * 1024;   { native stack kept free below compiled code, above NativeStackLimit }

var
  CC_Skip: Integer;
  JitSlowJump: Integer = -1;           { >= 0: the instruction is a compare-and-jump to that PC (JH_JumpCmp) }
  JitSlow: Pointer = nil;              { the helper the instruction's failed guards call (nil: they exit) }
  JitSlowList: array of Integer;
  JitSlowCount: Integer;
  { the cold blocks JitCompile emits past the code: instruction PC (I), its helper, its jump target (or -1), the
    forward jumps to the block JitDeferJumps[First .. First + Count - 1]. Kind 0: the helper (the failed guards'
    slow block); 1: ICGetHit for R[A] (rax the object, rsi the IC), else the helper; 2 / 3: a typed array's
    element load into R[A] / store of R[V] (EmitTAElem: rax the object, rcx the index), else the helper }
  JitDefer: array of record PC: Integer; I: QWord; H: Pointer; Jump, First, Count, Kind, A, V: Integer; end;
  JitDeferCount: Integer;
  JitDeferJumps: array of Integer;
  JitDeferJumpCount: Integer;

{ JitBuf's room past JitLen: at least 8 bytes, so one check covers a dword or a qword }
procedure JitGrow; noinline;
begin
  SetLength(JitBuf, JitLen * 2 + 4096);
  JitCap := Length(JitBuf) - 8;
end;

procedure JB(X: Byte); inline;
begin
  if JitLen >= JitCap then JitGrow;
  JitBuf[JitLen] := X;
  Inc(JitLen);
end;

procedure JD(X: Cardinal); inline;
begin
  if JitLen >= JitCap then JitGrow;
  PCardinal(@JitBuf[JitLen])^ := X;
  Inc(JitLen, 4);
end;

procedure JQ(X: QWord);
begin
  if JitLen >= JitCap then JitGrow;
  PQWord(@JitBuf[JitLen])^ := X;
  Inc(JitLen, 8);
end;

{ a REX prefix when W or a register past r7 needs one }
procedure JRex(W: Boolean; Reg, Index, Base: Integer);
var B: Byte;
begin
  B := $40;
  if W then B := B or 8;
  if Reg >= 8 then B := B or 4;
  if Index >= 8 then B := B or 2;
  if Base >= 8 then B := B or 1;
  if B <> $40 then JB(B);
end;

{ ModRM (and SIB) for [Base + Disp32] }
{ the ModRM byte (Reg, RM), the SIB byte when Sib >= 0, then the displacement in its shortest form: none for 0
  (unless the base is rbp or r13, whose mod 00 means no base), a byte for -128 .. 127, else 32 bits }
procedure JModDisp(Reg, RM, Base, Disp, Sib: Integer);
var Md: Integer;
begin
  if (Disp = 0) and ((Base and 7) <> 5) then Md := $00
  else if (Disp >= -128) and (Disp <= 127) then Md := $40
  else Md := $80;
  JB(Md or ((Reg and 7) shl 3) or RM);
  if Sib >= 0 then JB(Sib);
  if Md = $40 then JB(Byte(Disp and $FF))
  else if Md = $80 then JD(Cardinal(Disp));
end;

procedure JMem(Reg, Base, Disp: Integer);
begin
  if (Base and 7) = 4 then JModDisp(Reg, 4, Base, Disp, $24)
  else JModDisp(Reg, Base and 7, Base, Disp, -1);
end;

{ [Base + Index * 8 + Disp] (Index not rsp) }
procedure JMemIdx(Reg, Base, Index, Disp: Integer);
begin
  JModDisp(Reg, 4, Base, Disp, $C0 or ((Index and 7) shl 3) or (Base and 7));
end;

procedure JModRR(Reg, RM: Integer);
begin
  JB($C0 or ((Reg and 7) shl 3) or (RM and 7));
end;

procedure MovRM(Dst, Base, Disp: Integer);          { mov Dst, [Base + Disp] (64-bit) }
begin JRex(True, Dst, 0, Base); JB($8B); JMem(Dst, Base, Disp); end;

procedure MovMR(Base, Disp, Src: Integer);          { mov [Base + Disp], Src }
begin JRex(True, Src, 0, Base); JB($89); JMem(Src, Base, Disp); end;

procedure MovRMIdx(Dst, Base, Index, Disp: Integer);
begin JRex(True, Dst, Index, Base); JB($8B); JMemIdx(Dst, Base, Index, Disp); end;

procedure MovMRIdx(Base, Index, Disp, Src: Integer);
begin JRex(True, Src, Index, Base); JB($89); JMemIdx(Src, Base, Index, Disp); end;

procedure Mov32RM(Dst, Base, Disp: Integer);        { mov Dst32, [Base + Disp] (zero-extends) }
begin JRex(False, Dst, 0, Base); JB($8B); JMem(Dst, Base, Disp); end;

procedure MovzxBRM(Dst, Base, Disp: Integer);       { movzx Dst32, byte [Base + Disp] }
begin JRex(False, Dst, 0, Base); JB($0F); JB($B6); JMem(Dst, Base, Disp); end;

procedure MovzxWRM(Dst, Base, Disp: Integer);       { movzx Dst32, word [Base + Disp] }
begin JRex(False, Dst, 0, Base); JB($0F); JB($B7); JMem(Dst, Base, Disp); end;

procedure MovRI(Dst: Integer; Imm: QWord);          { mov Dst, Imm (32-bit form when it zero-extends) }
begin
  if Imm <= $FFFFFFFF then
  begin
    JRex(False, 0, 0, Dst); JB($B8 + (Dst and 7)); JD(Cardinal(Imm));
  end
  else
  begin
    JRex(True, 0, 0, Dst); JB($B8 + (Dst and 7)); JQ(Imm);
  end;
end;

procedure MovRR(Dst, Src: Integer);                 { mov Dst, Src (64-bit) }
begin JRex(True, Src, 0, Dst); JB($89); JModRR(Src, Dst); end;

{ Op Dst, Src: $01 add, $09 or, $21 and, $29 sub, $31 xor, $39 cmp, $85 test }
procedure AluRR(Op: Byte; Dst, Src: Integer; W: Boolean);
begin JRex(W, Src, 0, Dst); JB(Op); JModRR(Src, Dst); end;

{ /N Dst, Imm (8 bits sign-extended when it fits, else 32): 0 add, 1 or, 4 and, 5 sub, 6 xor, 7 cmp }
procedure AluRI(N, Dst, Imm: Integer; W: Boolean);
begin
  JRex(W, 0, 0, Dst);
  if (Imm >= -128) and (Imm <= 127) then begin JB($83); JModRR(N, Dst); JB(Byte(Imm and $FF)); end
  else begin JB($81); JModRR(N, Dst); JD(Cardinal(Imm)); end;
end;

procedure Cmp32RM(Reg, Base, Disp: Integer);        { cmp Reg32, [Base + Disp] }
begin JRex(False, Reg, 0, Base); JB($3B); JMem(Reg, Base, Disp); end;

procedure CmpRM(Reg, Base, Disp: Integer);          { cmp Reg, [Base + Disp] (64-bit) }
begin JRex(True, Reg, 0, Base); JB($3B); JMem(Reg, Base, Disp); end;

{ /N Dst, Count: 4 shl, 5 shr, 7 sar }
procedure ShiftRI(N, Dst, Count: Integer; W: Boolean);
begin JRex(W, 0, 0, Dst); JB($C1); JModRR(N, Dst); JB(Count); end;

procedure Imul32(Dst, Src: Integer);                { imul Dst32, Src32 }
begin JRex(False, Dst, 0, Src); JB($0F); JB($AF); JModRR(Dst, Src); end;

procedure AddFix(Target: Integer);
begin
  if JitFixCount >= Length(JitFixes) then SetLength(JitFixes, JitFixCount * 2 + 256);
  JitFixes[JitFixCount].At := JitLen;
  JitFixes[JitFixCount].Target := Target;
  Inc(JitFixCount);
  JD(0);
end;

procedure JccPC(CC, PC: Integer);                   { jcc to PC's code }
begin JB($0F); JB($80 or CC); AddFix(PC); end;

procedure JmpPC(PC: Integer);
begin JB($E9); AddFix(PC); end;

const
  JIT_EPILOGUE = MaxInt;           { a fix's Target: the epilogue (eax already the result) }
  JIT_THROW = MaxInt - 1;          { a fix's Target: the shared throw exit (eax -1, then the epilogue) }

procedure JmpEpilogue;
begin JB($E9); AddFix(JIT_EPILOGUE); end;

procedure JccThrow(CC: Integer);                    { jcc to the code's throw exit: it returns -1 }
begin JB($0F); JB($80 or CC); AddFix(JIT_THROW); end;

function JccFwd(CC: Integer): Integer; forward;
function JmpFwd: Integer; forward;
procedure PatchHere(At: Integer); forward;

procedure AddSlow(At: Integer);                     { a forward jump to the instruction's slow block }
begin
  if JitSlowCount >= Length(JitSlowList) then SetLength(JitSlowList, JitSlowCount * 2 + 16);
  JitSlowList[JitSlowCount] := At;
  Inc(JitSlowCount);
end;

procedure JccExit(CC, PC: Integer);                 { jcc to PC's exit: the interpreter runs PC }
begin
  if JitSlow <> nil then
  begin
    { the instruction's slow path (its helper) instead }
    if JitSlowCount >= Length(JitSlowList) then SetLength(JitSlowList, JitSlowCount * 2 + 16);
    JitSlowList[JitSlowCount] := JccFwd(CC);
    Inc(JitSlowCount);
    Exit;
  end;
  JB($0F); JB($80 or CC); AddFix(-1 - PC);
end;

procedure JmpExit(PC: Integer);
begin JB($E9); AddFix(-1 - PC); end;

function JccFwd(CC: Integer): Integer;              { a forward jcc within the instruction; PatchHere ends it }
begin JB($0F); JB($80 or CC); Result := JitLen; JD(0); end;

function JmpFwd: Integer;
begin JB($E9); Result := JitLen; JD(0); end;

procedure PatchHere(At: Integer);
var D: Cardinal;
begin
  D := Cardinal(JitLen - (At + 4));
  JitBuf[At] := D and $FF; JitBuf[At + 1] := (D shr 8) and $FF;
  JitBuf[At + 2] := (D shr 16) and $FF; JitBuf[At + 3] := D shr 24;
end;

procedure CallAbs(Addr: Pointer);                   { call Addr (through rax) }
begin MovRI(jAX, QWord(PtrUInt(Addr))); JB($FF); JB($D0); end;

procedure MovqXR(X, R: Integer);                    { movq xmmX, R }
begin JB($66); JRex(True, X, 0, R); JB($0F); JB($6E); JModRR(X, R); end;

procedure MovqRX(R, X: Integer);                    { movq R, xmmX }
begin JB($66); JRex(True, X, 0, R); JB($0F); JB($7E); JModRR(X, R); end;

procedure SseRR(Prefix, Op: Byte; Dst, Src: Integer);   { addsd F2 58, subsd F2 5C, mulsd F2 59, ucomisd 66 2E }
begin JB(Prefix); JB($0F); JB(Op); JModRR(Dst, Src); end;

procedure Cvt32ToX(X, R: Integer);                  { cvtsi2sd xmmX, R32 }
begin JB($F2); JRex(False, X, 0, R); JB($0F); JB($2A); JModRR(X, R); end;

var
  JitFsVStack, JitFsNurseryStart, JitFsNurseryLimit, JitFsArrayIterNext: PtrInt;   { threadvar offsets from fs }
  JitFsFrames, JitFsFrameTop, JitFsStackTop, JitFsJitDepth, JitFsJitDepthMax, JitFsStackFloor: PtrInt;

function JitFsBase: PtrUInt; assembler; nostackframe;
asm
  movq %fs:0, %rax
end;

{ mov Dst, fs:[Disp] (a threadvar of the running thread) }
procedure MovRFs(Dst, Disp: Integer);
begin
  JB($64); JRex(True, Dst, 0, 0); JB($8B); JB($04 or ((Dst and 7) shl 3)); JB($25); JD(Cardinal(Disp));
end;

{ mov Dst32, fs:[Disp] }
procedure Mov32RFs(Dst, Disp: Integer);
begin
  JB($64); JRex(False, Dst, 0, 0); JB($8B); JB($04 or ((Dst and 7) shl 3)); JB($25); JD(Cardinal(Disp));
end;

{ mov fs:[Disp], Src32 }
procedure Mov32FsR(Disp, Src: Integer);
begin
  JB($64); JRex(False, Src, 0, 0); JB($89); JB($04 or ((Src and 7) shl 3)); JB($25); JD(Cardinal(Disp));
end;

procedure CmpBMI(Base, Disp: Integer; Imm: Byte);      { cmp byte [Base + Disp], Imm }
begin JRex(False, 0, 0, Base); JB($80); JMem(7, Base, Disp); JB(Imm); end;

procedure TestBMI(Base, Disp: Integer; Imm: Byte);     { test byte [Base + Disp], Imm }
begin JRex(False, 0, 0, Base); JB($F6); JMem(0, Base, Disp); JB(Imm); end;

procedure MovsxdRM(Dst, Base, Disp: Integer);          { movsxd Dst, dword [Base + Disp] }
begin JRex(True, Dst, 0, Base); JB($63); JMem(Dst, Base, Disp); end;

procedure Mov32MR(Base, Disp, Src: Integer);           { mov dword [Base + Disp], Src32 }
begin JRex(False, Src, 0, Base); JB($89); JMem(Src, Base, Disp); end;

procedure Mov32MI(Base, Disp: Integer; Imm: Cardinal);  { mov dword [Base + Disp], Imm }
begin JRex(False, 0, 0, Base); JB($C7); JMem(0, Base, Disp); JD(Imm); end;

procedure Mov8MI(Base, Disp: Integer; Imm: Byte);       { mov byte [Base + Disp], Imm }
begin JRex(False, 0, 0, Base); JB($C6); JMem(0, Base, Disp); JB(Imm); end;

procedure LeaRM(Dst, Base, Disp: Integer);              { lea Dst, [Base + Disp] }
begin JRex(True, Dst, 0, Base); JB($8D); JMem(Dst, Base, Disp); end;

procedure ImulRRI(Dst, Src, Imm: Integer);              { imul Dst, Src, Imm32 (64-bit) }
begin JRex(True, Dst, 0, Src); JB($69); JModRR(Dst, Src); JD(Cardinal(Imm)); end;

{ Rd := the top 16 bits of Rs }
procedure TagOf(Rd, Rs: Integer);
begin
  MovRR(Rd, Rs);
  ShiftRI(5, Rd, 48, True);
end;

{ the value in V (rax or rcx) to xmmX: an int converted, a double as it is, anything else PC's exit }
procedure ToXmm(X, V, PC: Integer);
var NotInt, Done: Integer;
begin
  TagOf(jDX, V);
  AluRI(7, jDX, TAG_INT_HI, False);
  NotInt := JccFwd(ccNE);
  Cvt32ToX(X, V);
  Done := JmpFwd;
  PatchHere(NotInt);
  MovRI(jR8, DOUBLE_LIMIT);
  AluRR($39, V, jR8, True);
  JccExit(ccAE, PC);
  MovqXR(X, V);
  PatchHere(Done);
end;

{ cvttsd2si R64, xmmX }
procedure CvtTX64(R, X: Integer);
begin JB($F2); JRex(True, R, 0, X); JB($0F); JB($2C); JModRR(R, X); end;

{ cvttsd2si R32, xmmX }
procedure CvtTX32(R, X: Integer);
begin JB($F2); JRex(False, R, 0, X); JB($0F); JB($2C); JModRR(R, X); end;

{ the value in V (rax or rcx) as an int32 in V's low half (QuickInt32: an int, or a double of magnitude below 2^53
  truncated); anything else PC's slow path. Uses rdx, r8, xmm0-2 }
procedure ToInt32Of(V, PC: Integer);
var IsIntL, Done: Integer;
begin
  TagOf(jDX, V);
  AluRI(7, jDX, TAG_INT_HI, False);
  IsIntL := JccFwd(ccE);
  MovRI(jR8, DOUBLE_LIMIT);
  AluRR($39, V, jR8, True);
  JccExit(ccAE, PC);
  MovqXR(0, V);
  MovRI(jR8, $4340000000000000);   { 2^53 }
  MovqXR(1, jR8);
  SseRR($66, $2E, 0, 1);
  JccExit(ccAE, PC);
  JccExit(ccP, PC);
  MovRI(jR8, $C340000000000000);   { -2^53 }
  MovqXR(2, jR8);
  SseRR($66, $2E, 0, 2);
  JccExit(ccBE, PC);
  CvtTX64(V, 0);
  Done := JmpFwd;
  PatchHere(IsIntL);
  PatchHere(Done);
end;


{ xmm0 to a Number in rax (MkArith: the canonical NaN), stored in R[A] }
procedure StoreArith(A: Integer);
var L: Integer;
begin
  MovqRX(jAX, 0);
  MovRI(jCX, QWord($FFF8000000000000));
  AluRR($39, jAX, jCX, True);
  L := JccFwd(ccNE);
  MovRI(jAX, CANON_NAN);
  PatchHere(L);
  MovMR(jBX, A * 8, jAX);
end;

{ the int32 in eax as an int value in R[A] }
procedure StoreInt(A: Integer);
begin
  MovRI(jCX, TAG_INT);
  AluRR($09, jAX, jCX, True);
  MovMR(jBX, A * 8, jAX);
end;

{ xmm0 to a Number in R[A] as MkNumber makes it: an int when it is one (not -0), else the double }
procedure StoreNumber(A: Integer);
var NotInt, Done, L_NZ, L_NEG, L_END: Integer;
begin
  CvtTX32(jCX, 0);
  Cvt32ToX(1, jCX);
  SseRR($66, $2E, 0, 1);
  NotInt := JccFwd(ccNE);
  Done := JccFwd(ccP);
  { 0: -0 stays a double }
  AluRR($85, jCX, jCX, False);
  L_NZ := JccFwd(ccNE);
  MovqRX(jAX, 0);
  AluRR($85, jAX, jAX, True);
  L_NEG := JccFwd(ccS);
  PatchHere(L_NZ);
  JRex(False, jAX, 0, jCX); JB($8B); JModRR(jAX, jCX);   { mov eax, ecx }
  StoreInt(A);
  L_END := JmpFwd;
  PatchHere(NotInt); PatchHere(Done); PatchHere(L_NEG);
  StoreArith(A);
  PatchHere(L_END);
end;

{ the int test of the value in V: not an int goes to the returned forward jump }
function IfNotInt(V: Integer): Integer;
begin
  TagOf(jDX, V);
  AluRI(7, jDX, TAG_INT_HI, False);
  Result := JccFwd(ccNE);
end;

{ Op [Base + Index * 2^Scale + Disp] with Reg (Prefix 0: none; a mandatory prefix goes before REX) }
procedure SibOp(Prefix: Byte; W: Boolean; const Ops: array of Byte; Reg, Base, Index, Scale, Disp: Integer);
var K: Integer;
begin
  if Prefix <> 0 then JB(Prefix);
  JRex(W, Reg, Index, Base);
  for K := 0 to High(Ops) do JB(Ops[K]);
  JModDisp(Reg, 4, Base, Disp, (Scale shl 6) or ((Index and 7) shl 3) or (Base and 7));
end;

{ strict equality of two pointer values of different bits, rax and rcx (FastLoop's case): edx 1 equal, 0 not.
  Two strings differ by length, by computed hashes, or as different atoms, and are the same atom's text equal;
  a string against any other cell differs, and so does any pair but two BigInts. Two BigInts and two strings
  without the atoms to tell are PC's slow path }
procedure EmitPtrPairEq(PC: Integer);
var LA, LN1, LN2, LN3, LN4, LH1, LH2, LH3, LE: Integer;
begin
  MovRR(jR8, jAX); AluRR($31, jR8, jR13, True);
  MovRR(jR9, jCX); AluRR($31, jR9, jR13, True);
  MovzxBRM(jR10, jR8, 4);
  MovzxBRM(jR11, jR9, 4);
  AluRR($31, jDX, jDX, False);
  AluRI(7, jR10, ckString, False);
  LA := JccFwd(ccNE);
  AluRI(7, jR11, ckString, False);
  LN1 := JccFwd(ccNE);
  Mov32RM(jR10, jR8, PtrInt(@PStr(nil)^.Len));
  Cmp32RM(jR10, jR9, PtrInt(@PStr(nil)^.Len));
  LN2 := JccFwd(ccNE);
  { both hashes computed and different: not equal }
  Mov32RM(jR10, jR8, PtrInt(@PStr(nil)^.Hash));
  AluRR($85, jR10, jR10, False);
  LH1 := JccFwd(ccE);
  Mov32RM(jR11, jR9, PtrInt(@PStr(nil)^.Hash));
  AluRR($85, jR11, jR11, False);
  LH2 := JccFwd(ccE);
  AluRR($39, jR10, jR11, False);
  LN3 := JccFwd(ccNE);
  PatchHere(LH1);
  PatchHere(LH2);
  { both atoms known: equal exactly when they are the same atom }
  MovRM(jR10, jR8, PtrInt(@PStr(nil)^.Atom));
  AluRR($85, jR10, jR10, True);
  JccExit(ccE, PC);
  MovRM(jR11, jR9, PtrInt(@PStr(nil)^.Atom));
  AluRR($85, jR11, jR11, True);
  JccExit(ccE, PC);
  AluRR($39, jR10, jR11, True);
  LH3 := JccFwd(ccNE);
  MovRI(jDX, 1);
  LE := JmpFwd;
  PatchHere(LA);
  { not a string: two BigInts are the slow path's, anything else differs }
  AluRI(7, jR10, 9, False);
  LN4 := JccFwd(ccNE);
  AluRI(7, jR11, 9, False);
  JccExit(ccE, PC);
  PatchHere(LN1);
  PatchHere(LN2);
  PatchHere(LN3);
  PatchHere(LN4);
  PatchHere(LH3);
  PatchHere(LE);
end;

{ a typed array's element (FastLoop's case: an ordinary buffer, the view's length, a kind done without a call),
  rax the object, rcx the index (an int value); a load into R[A], a store of R[V]. Anything else is PC's slow
  path. The forward jumps in JitTAEnds (JitTAEndCount of them) end the instruction: the caller patches them }
var
  JitTAEnds: array[0..15] of Integer;
  JitTAEndCount: Integer;

procedure EmitTAElem(Store: Boolean; A, V, PC: Integer);
var K, LNext, DataOff: Integer;
  procedure Kind(Ta: Integer);
  begin
    AluRI(7, jR10, Ta, False);
    LNext := JccFwd(ccNE);
  end;
  procedure Done;
  begin
    JitTAEnds[JitTAEndCount] := JmpFwd;
    Inc(JitTAEndCount);
    PatchHere(LNext);
  end;
begin
  JitTAEndCount := 0;
  DataOff := PtrInt(@PBytes(nil)^.Data);
  MovzxBRM(jDX, jAX, PtrInt(@PObj(nil)^.OKind));
  AluRI(7, jDX, okTypedArray, False);
  JccExit(ccNE, PC);
  { the index: 0 <= index < the view's length (a length past the int32 range is the slow path's) }
  Mov32RM(jDX, jAX, PtrInt(@PObj(nil)^.Aux2));
  AluRR($85, jDX, jDX, False);
  JccExit(ccS, PC);
  AluRR($39, jCX, jDX, False);
  JccExit(ccAE, PC);
  JRex(False, jCX, 0, jCX); JB($8B); JModRR(jCX, jCX);   { mov ecx, ecx }
  { the buffer: ordinary, its bytes at the view's offset in r11 }
  MovRM(jR11, jAX, PtrInt(@PObj(nil)^.Aux1));
  MovRI(jR9, PTR_MASK);
  AluRR($21, jR11, jR9, True);
  if Store then TestBMI(jR11, PtrInt(@PObj(nil)^.OFlags), bfShared or bfResizable or bfDetached or bfImmutable)
  else TestBMI(jR11, PtrInt(@PObj(nil)^.OFlags), bfShared or bfResizable or bfDetached);
  JccExit(ccNE, PC);
  MovRM(jR11, jR11, PtrInt(@PObj(nil)^.Aux1));
  AluRR($21, jR11, jR9, True);
  Mov32RM(jR9, jAX, PtrInt(@PObj(nil)^.Pad));
  AluRR($01, jR11, jR9, True);
  MovzxBRM(jR10, jAX, PtrInt(@PObj(nil)^.OFlags));
  ShiftRI(5, jR10, 4, False);
  if not Store then
  begin
    Kind(taUint8); SibOp(0, False, [$0F, $B6], jAX, jR11, jCX, 0, DataOff); StoreInt(A); Done;
    Kind(taUint8C); SibOp(0, False, [$0F, $B6], jAX, jR11, jCX, 0, DataOff); StoreInt(A); Done;
    Kind(taInt8); SibOp(0, False, [$0F, $BE], jAX, jR11, jCX, 0, DataOff); StoreInt(A); Done;
    Kind(taInt16); SibOp(0, False, [$0F, $BF], jAX, jR11, jCX, 1, DataOff); StoreInt(A); Done;
    Kind(taUint16); SibOp(0, False, [$0F, $B7], jAX, jR11, jCX, 1, DataOff); StoreInt(A); Done;
    Kind(taInt32); SibOp(0, False, [$8B], jAX, jR11, jCX, 2, DataOff); StoreInt(A); Done;
    Kind(taFloat64); SibOp($F2, False, [$0F, $10], 0, jR11, jCX, 3, DataOff); StoreNumber(A); Done;
    Kind(taFloat32);
    SibOp($F3, False, [$0F, $10], 0, jR11, jCX, 2, DataOff);
    JB($F3); JB($0F); JB($5A); JB($C0);              { cvtss2sd xmm0, xmm0 }
    StoreNumber(A);
    Done;
  end
  else
  begin
    MovRM(jSI, jBX, V * 8);
    { an int into an integer kind }
    TagOf(jDX, jSI);
    AluRI(7, jDX, TAG_INT_HI, False);
    LNext := JccFwd(ccNE);
    K := LNext;
    Kind(taUint8); SibOp(0, False, [$88], jSI, jR11, jCX, 0, DataOff); Done;
    Kind(taInt8); SibOp(0, False, [$88], jSI, jR11, jCX, 0, DataOff); Done;
    Kind(taInt16); SibOp($66, False, [$89], jSI, jR11, jCX, 1, DataOff); Done;
    Kind(taUint16); SibOp($66, False, [$89], jSI, jR11, jCX, 1, DataOff); Done;
    Kind(taInt32); SibOp(0, False, [$89], jSI, jR11, jCX, 2, DataOff); Done;
    Kind(taUint32); SibOp(0, False, [$89], jSI, jR11, jCX, 2, DataOff); Done;
    PatchHere(K);
    { a Number (an int or a double) into a float kind }
    ToXmm(0, jSI, PC);
    Kind(taFloat64); SibOp($F2, False, [$0F, $11], 0, jR11, jCX, 3, DataOff); Done;
    Kind(taFloat32);
    JB($F2); JB($0F); JB($5A); JB($C0);              { cvtsd2ss xmm0, xmm0 }
    SibOp($F3, False, [$0F, $11], 0, jR11, jCX, 2, DataOff);
    Done;
  end;
  { another kind: the slow path }
  if JitSlow <> nil then AddSlow(JmpFwd) else JmpExit(PC);
end;

procedure PatchTAEnds;
var K: Integer;
begin
  for K := 0 to JitTAEndCount - 1 do PatchHere(JitTAEnds[K]);
  JitTAEndCount := 0;
end;

{ a pointer in V to its cell in V (the tag test first: not a pointer is PC's exit) }
procedure UntagPtr(V, PC: Integer);
begin
  { xor with r13 (the pointer tag) clears a pointer's tag; anything else keeps high bits }
  AluRR($31, V, jR13, True);
  MovRR(jDX, V);
  ShiftRI(5, jDX, 48, True);
  JccExit(ccNE, PC);
end;

{ the write barrier after a store of V (in Val) into the container cell in Cell (rdi-safe registers): an old cell
  not yet remembered that now points into the nursery is remembered }
procedure Barrier(Cell, Val: Integer);
var L1, L2, L3, L4: Integer;
begin
  MovzxBRM(jDX, Cell, 5);   { Hdr.Flags }
  AluRI(4, jDX, cfOld or cfRemembered, False);
  AluRI(7, jDX, cfOld, False);
  L1 := JccFwd(ccNE);
  TagOf(jDX, Val);
  AluRI(7, jDX, TAG_PTR_HI, False);
  L2 := JccFwd(ccNE);
  MovRR(jDX, Val);
  AluRR($31, jDX, jR13, True);   { the cell address }
  MovRFs(jR8, JitFsNurseryStart);
  AluRR($39, jDX, jR8, True);
  L3 := JccFwd(ccB);
  MovRFs(jR8, JitFsNurseryLimit);
  AluRR($39, jDX, jR8, True);
  L4 := JccFwd(ccAE);
  MovRR(jDI, Cell);
  CallAbs(@Remember);
  PatchHere(L1); PatchHere(L2); PatchHere(L3); PatchHere(L4);
end;

const
  OPT_HOT = 1000;                  { a template's function entries and loop iterations in baseline code before its
                                     region is optimized (G2.Opt.inc) }
  OPT_MAX_DEOPTS = 200;

type
  { a template's optimizing tier state (G2.Opt.inc) }
  POptInfo = ^TOptInfo;
  TOptInfo = record
    Count: Integer;                { counts down at the baseline code's PC 0 and back edges }
    Deopts, Recompiles: Integer;
    Fails: Integer;                { regions not optimized: each one makes the counter's next run longer }
    BaseTable: PPtrUInt;           { the baseline code's entry per instruction }
    Table: PPtrUInt;               { BaseTable with the optimized regions' entries, nil when none }
    Tried: array of Byte;          { per region entry: 1 optimized, 2 not }
    Feedback: array of Byte;       { per instruction: the guards that failed there (1 int overflow, 2 an entry kind) }
  end;

var
  OptEnabled: Integer = 1;         { G2_OPT=0 turns the optimizing tier off }

var
  JitHot: Integer = JIT_HOT;
  JitCallStub: Pointer = nil;   { JitBuildCallStub's code: opCall's call, shared by every call site }

{$IFDEF G2_JITSTATS}
var
  JitExits, JitNestedExits, JitNoEntry: array[0..255] of Int64;   { G2_JITSTATS: where the code stops, by opcode }
  JitEntries, JitFallbacks, JitCompiles, JitCodeBytes: Int64;
  JitOpBytes, JitOpCount: array[0..255] of Int64;
{$ENDIF}

threadvar
  JitDepth: Integer;               { native calls of compiled code in progress (XJitCall) }
  { the thread's bounds on them (JitThreadInit): JitDepth below JitDepthMax (0 until set: no code runs), rsp above
    JitStackFloor (0 where the stack's bounds are unknown, and JitDepthMax is JIT_MAX_DEPTH) }
  JitDepthMax: Integer;
  JitStackFloor: PtrUInt;

procedure JitThreadInit; noinline;
var L: PtrUInt;
begin
  L := NativeStackLimit;
  if L <> 0 then
  begin
    JitStackFloor := L + JIT_STACK_MARGIN;
    JitDepthMax := MaxInt;
  end
  else
  begin
    JitStackFloor := 0;
    JitDepthMax := JIT_MAX_DEPTH;
  end;
end;

{ no further native activation of compiled code: the thread's depth bound or its native stack floor reached }
function JitTooDeep: Boolean; inline;
var Marker: Byte;
begin
  Result := (JitDepth >= JitDepthMax) or (PtrUInt(@Marker) < JitStackFloor);
end;

procedure JitCompile(T: PFuncTemplate); forward;
procedure XJitRet(Fr: PFrame; V: TValue); forward;
function OptPromising(T: PFuncTemplate): Boolean; forward;

{ a derived class constructor's compiled return of V in frame Fr (10.2.2 [[Construct]] steps 10-12 for XJitNew's
  Kind 1 frame, as Execute's opRet): an object result, else undefined gives this, which super() must have bound.
  The code runs in one realm only, so the errors are this realm's. False: a throw pending (the frame still
  pushed, as Execute's) }
function XJitRetCtor(Fr: PFrame; V: TValue): Boolean;
begin
  if (Fr^.Kind = 1) and not IsObject(V) then
  begin
    if V <> VUndefined then
    begin
      ThrowTypeError('Derived constructors may only return object or undefined');
      Exit(False);
    end;
    V := VStack[Fr^.Base];
    if V = VHole then
    begin
      ThrowError('ReferenceError', 'Must call super constructor in derived class before accessing ''this'' or returning from derived constructor');
      Exit(False);
    end;
  end;
  XJitRet(Fr, V);
  Result := True;
end;

{ a compiled frame Fr returns V (the code of a frame XJitCall pushed): as FastLoop's opRet for a call }
procedure XJitRet(Fr: PFrame; V: TValue);
var CF: PFrame;
begin
  { a construction (XJitNew's frame, a base constructor): an object result, else this (10.2.2 steps 10-12) }
  if (Fr^.Kind = 1) and not IsObject(V) then V := VStack[Fr^.Base];
  CloseUpvals(Fr^.Base);
  if not Fr^.Tpl^.NoHandlers then
    while (HandlerTop > 0) and (Handlers[HandlerTop - 1].Frame >= FrameTop - 1) do Dec(HandlerTop);
  Dec(FrameTop);
  VStack[Fr^.RetIdx] := V;
  { (a host's CallValue has no frame below: it restores StackTop itself) }
  if FrameTop > 0 then
  begin
    CF := @Frames[FrameTop - 1];
    StackTop := CF^.Base + CF^.Tpl^.MaxRegs;
  end;
end;

{ compiled code's call (opCall A, Argc arguments, ThisFlag its this-is-undefined bit) of a compiled bytecode
  function: the frame as FastLoop's opCall pushes it, then the callee's code called here, returning into the
  caller's code. 0 done (the result in R[A]), 1 not taken (nothing changed: the interpreter makes the call),
  -1 a throw pending (the frames above the caller gone). A callee whose code stops early is finished by a nested
  Execute; one that throws out of a nested call is handed to it with the throw. Fr^.PC is past the call. }
function XJitCall(Fr: PFrame; A, Argc, ThisFlag: Integer): Integer;
var VA, VB, V, This: TValue; O, O2: PObj; T: PFuncTemplate; Base, J, Res: Integer; NF: PFrame; R: PValue;
begin
  if MultiRealmAny or JitTooDeep then Exit(1);
  R := @VStack[Fr^.Base];
  VA := R[A];
  if not IsObject(VA) then Exit(1);
  O := ObjOf(VA);
  if (O^.OKind = okNative) and (O^.Native = FuncCallNative) then
  begin
    { f.call(t, ...args) (as FastLoop's opCall): the registers shift down a place, f the callee; every
      condition that would leave the call to the interpreter is checked before the shift }
    if ThisFlag <> 0 then R[A + 1] := VUndefined;
    VB := R[A + 1];
    if not IsObject(VB) then Exit(1);
    O2 := ObjOf(VB);
    if O2^.OKind = okNative then
    begin
      if O2^.Native = FuncCallNative then Exit(1);
    end
    else
    begin
      if (O2^.OKind <> okFunction) or ((O2^.OFlags and 1) <> 0) then Exit(1);
      T := PFuncTemplate(O2^.Native);
      if T^.IsAsync or T^.IsGenerator or (T^.JitState <> 1) or (PPtrUInt(T^.JitTable)[0] = 0) then Exit(1);
      if (Fr^.Base + A + 1 + T^.MaxRegs >= STACK_MAX) or (FrameTop >= FRAME_MAX - 2) then Exit(1);
      if T^.FastCall and not T^.IsStrict and not T^.IsArrow then
      begin
        if Argc = 0 then This := VUndefined else This := R[A + 2];
        if not IsObject(This) and (This <> VUndefined) and (This <> VNull) then Exit(1);
      end;
    end;
    R[A] := VB;
    if Argc = 0 then R[A + 1] := VUndefined
    else
    begin
      for J := 0 to Argc - 1 do R[A + 1 + J] := R[A + 2 + J];
      Dec(Argc);
    end;
    ThisFlag := 0;
    VA := VB;
    O := O2;
  end;
  if (O^.OKind = okNative) and (O^.Native <> FuncCallNative) then
  begin
    { a native function, as FastLoop's opCall calls it }
    if not XFastNative(O, R, A, (QWord(Argc) shl 24) or (QWord(ThisFlag) shl 40), Fr) then Exit(-1);
    Exit(0);
  end;
  if (O^.OKind <> okFunction) or ((O^.OFlags and 1) <> 0) then Exit(1);
  T := PFuncTemplate(O^.Native);
  if T^.IsAsync or T^.IsGenerator then Exit(1);
  if T^.JitState <> 1 then
  begin
    if T^.JitState = 0 then
    begin
      Inc(T^.JitCount);
      if (T^.JitCount >= JitHot) and (GetCurrentThreadId = JitMainThread) then JitCompile(T);
    end;
    if T^.JitState <> 1 then Exit(1);
  end;
  if PPtrUInt(T^.JitTable)[0] = 0 then Exit(1);
  Base := Fr^.Base + A + 1;
  if (Base + T^.MaxRegs >= STACK_MAX) or (FrameTop >= FRAME_MAX - 2) then Exit(1);
  if ThisFlag <> 0 then R[A + 1] := VUndefined;
  if T^.FastCall then
  begin
    if not T^.IsStrict and not T^.IsArrow then
    begin
      { a primitive this is boxed by the interpreter's call }
      V := R[A + 1];
      if IsObject(V) then
      else if (V = VUndefined) or (V = VNull) then R[A + 1] := GlobalObj
      else Exit(1);
    end;
    if Argc < T^.NumParams then J := Argc + 1 else J := T^.NumParams + 1;
    if J < T^.MaxRegs then FillUndef(@VStack[Base + J], T^.MaxRegs - J);
    NF := @Frames[FrameTop];
    NF^.Closure := VA;
    NF^.NewTarget := VUndefined;
    NF^.Tpl := T;
    NF^.Base := Base;
    NF^.PC := 0;
    NF^.RetIdx := Base - 1;
    NF^.Argc := Argc;
    NF^.Kind := 0;
    Inc(FrameTop);
    StackTop := Base + T^.MaxRegs;
  end
  else
  begin
    { a rest parameter or an arguments object: PushBytecodeFrame's work (it can collect and throw) }
    if StackTop < Base + 1 + Argc then StackTop := Base + 1 + Argc;
    if not PushBytecodeFrame(O, R[A + 1], @R[A + 2], Argc, Base - 1, 0, VUndefined) then Exit(-1);
    NF := @Frames[FrameTop - 1];
    if PPtrUInt(T^.JitTable)[NF^.PC] = 0 then
    begin
      { its first instruction is not the code's: the interpreter runs the frame }
      if not Execute(FrameTop - 1, V) then
      begin
        StackTop := Fr^.Base + Fr^.Tpl^.MaxRegs;
        Exit(-1);
      end;
      VStack[Base - 1] := V;
      StackTop := Fr^.Base + Fr^.Tpl^.MaxRegs;
      Exit(0);
    end;
  end;
  Inc(JitDepth);
  Res := TJitFn(T^.Jit)(@VStack[NF^.Base], Pointer(PPtrUInt(T^.JitTable)[NF^.PC]), NF, 1);
  Dec(JitDepth);
  {$IFDEF G2_JITSTATS}if Res >= 0 then Inc(JitNestedExits[T^.Code[Res] and $FF]);{$ENDIF}
  if Res = -2 then
  begin
    { returned (XJitRet or the code's inline return, which leaves StackTop to the caller) }
    StackTop := Fr^.Base + Fr^.Tpl^.MaxRegs;
    Exit(0);
  end;
  {$IFDEF G2_JITSTATS}Inc(JitFallbacks);{$ENDIF}
  { the rest of the callee (from its PC, or its pending throw) in a nested Execute }
  if Res = -1 then InjectThrow := True else NF^.PC := Res;
  if not Execute(FrameTop - 1, V) then
  begin
    StackTop := Fr^.Base + Fr^.Tpl^.MaxRegs;
    Exit(-1);
  end;
  VStack[Base - 1] := V;
  StackTop := Fr^.Base + Fr^.Tpl^.MaxRegs;
  Result := 0;
end;

{ CallValue's frame (the newest, Kind 0, just pushed): its compiled code run natively, the result read from its
  RetIdx. False: not run (no code at its PC, the JIT off): the caller runs Execute. True: Ok and Res are the call's
  (a stop part way or a throw finished by a nested Execute) }
function JitRunPushed(var Res: TValue; var Ok: Boolean): Boolean;
var NF: PFrame; T: PFuncTemplate; R: Integer; RetIdx: Integer;
begin
  if (JitEnabled <> 1) or MultiRealmAny then Exit(False);
  if JitDepthMax = 0 then JitThreadInit;
  if JitTooDeep then Exit(False);
  NF := @Frames[FrameTop - 1];
  T := NF^.Tpl;
  if T^.IsGenerator then Exit(False);
  if T^.JitState <> 1 then
  begin
    if T^.JitState = 0 then
    begin
      Inc(T^.JitCount);
      if (T^.JitCount >= JitHot) and (GetCurrentThreadId = JitMainThread) then JitCompile(T);
    end;
    if T^.JitState <> 1 then Exit(False);
  end;
  if (NF^.Kind <> 0) or (PPtrUInt(T^.JitTable)[NF^.PC] = 0) then Exit(False);
  RetIdx := NF^.RetIdx;
  Inc(JitDepth);
  R := TJitFn(T^.Jit)(@VStack[NF^.Base], Pointer(PPtrUInt(T^.JitTable)[NF^.PC]), NF, 1);
  Dec(JitDepth);
  if R = -2 then
  begin
    Res := VStack[RetIdx];
    Ok := True;
    Exit(True);
  end;
  if R = -1 then InjectThrow := True else NF^.PC := R;
  Ok := Execute(FrameTop - 1, Res);
  Result := True;
end;

{ the inline call's callee (the newest frame) stopped part way (Res its PC) or threw (-1): finished by a nested
  Execute, its result in R[A]. 0, or -1 a throw pending }
function XJitCallPost(Fr: PFrame; A, Res: Integer): Integer;
var V: TValue;
begin
  {$IFDEF G2_JITSTATS}Inc(JitFallbacks);{$ENDIF}
  if Res = -1 then InjectThrow := True else Frames[FrameTop - 1].PC := Res;
  if not Execute(FrameTop - 1, V) then
  begin
    StackTop := Fr^.Base + Fr^.Tpl^.MaxRegs;
    Exit(-1);
  end;
  VStack[Fr^.Base + A] := V;
  StackTop := Fr^.Base + Fr^.Tpl^.MaxRegs;
  Result := 0;
end;

{ compiled code's new (opNew A, Argc arguments at A + 2): a built-in constructor through Construct; a compiled base
  constructor whose prototype caches hold (as XFastNew) with this allocated, its field initializer run first (as
  Construct does) and its frame (Kind 1) run natively. 0 done (the object in R[A]), 1 the interpreter's (nothing
  changed), -1 a throw pending. Fr^.PC is past the instruction. }
function XJitNew(Fr: PFrame; A, Argc: Integer): Integer;
var VA, V: TValue; O: PObj; T: PFuncTemplate; Base, J, Res: Integer; NF: PFrame; R: PValue;
begin
  if MultiRealmAny or JitTooDeep then Exit(1);
  R := @VStack[Fr^.Base];
  VA := R[A];
  if not IsObject(VA) then Exit(1);
  O := ObjOf(VA);
  Base := Fr^.Base + A + 1;
  if StackTop < Base + 1 + Argc then StackTop := Base + 1 + Argc;
  if (O^.OKind = okNative) and ((O^.OFlags and 32) <> 0) then
  begin
    V := VUndefined;
    if not Construct(VA, @R[A + 2], Argc, VA, V) then Exit(-1);
    VStack[Base - 1] := V;
    StackTop := Fr^.Base + Fr^.Tpl^.MaxRegs;
    Exit(0);
  end;
  if O^.OKind <> okFunction then Exit(1);
  T := PFuncTemplate(O^.Native);
  if T^.IsDerived and T^.IsClassCtor and (T^.CodeLen = 3) and (T^.RestReg >= 0) and
     (TOp(T^.Code[1] and $FF) = opSuperCallV) and (TOp(T^.Code[2] and $FF) = opRetUndef) then
  begin
    { a derived class's implicit constructor, super(...args) (10.2.1.4 step 4: the parent constructed with these
      arguments, not iterated, and this new.target), then the fields }
    V := O^.Shape^.Proto;
    if not IsConstructorV(V) then Exit(1);
    if not Construct(V, @R[A + 2], Argc, VA, V) or not RunFieldInit(VA, V) then
    begin
      StackTop := Fr^.Base + Fr^.Tpl^.MaxRegs;
      Exit(-1);
    end;
    VStack[Base - 1] := V;
    StackTop := Fr^.Base + Fr^.Tpl^.MaxRegs;
    Exit(0);
  end;
  if T^.IsArrow or (T^.IsMethod and not T^.IsClassCtor) or T^.IsGenerator or T^.IsAsync or not T^.FastCall or
     (T^.IsDerived and not T^.IsClassCtor) then Exit(1);
  if T^.JitState <> 1 then
  begin
    if T^.JitState = 0 then
    begin
      Inc(T^.JitCount);
      if (T^.JitCount >= JitHot) and (GetCurrentThreadId = JitMainThread) then JitCompile(T);
    end;
    if T^.JitState <> 1 then Exit(1);
  end;
  if PPtrUInt(T^.JitTable)[0] = 0 then Exit(1);
  if T^.IsDerived then
  begin
    { a derived class constructor: this unbound until its super() (which runs the fields) }
    if (Base + T^.MaxRegs >= STACK_MAX) or (FrameTop >= FRAME_MAX - 2) then Exit(1);
    R[A + 1] := VHole;
  end
  else
  begin
    if (Pointer(O^.Shape) <> T^.CtorFnShape) or (T^.CtorRootShape = nil) or
       (PShape(T^.CtorRootShape)^.Proto <> GetSlot(O, T^.CtorProtoSlot)) then Exit(1);
    if (Base + T^.MaxRegs >= STACK_MAX) or (FrameTop >= FRAME_MAX - 2) then Exit(1);
    R[A + 1] := MkPtr(NewObject(PShape(T^.CtorRootShape), Max(4, T^.ExpectedProps), okPlain));
    if Assigned(T^.FieldInit) then
    begin
      { 10.2.2 step 6.b: the instance fields before the body }
      R := @VStack[Fr^.Base];
      if not RunFieldInit(R[A], R[A + 1]) then
      begin
        StackTop := Fr^.Base + Fr^.Tpl^.MaxRegs;
        Exit(-1);
      end;
    end;
  end;
  R := @VStack[Fr^.Base];
  VA := R[A];
  if Argc < T^.NumParams then J := Argc + 1 else J := T^.NumParams + 1;
  if J < T^.MaxRegs then FillUndef(@VStack[Base + J], T^.MaxRegs - J);
  NF := @Frames[FrameTop];
  NF^.Closure := VA;
  NF^.NewTarget := VA;
  NF^.Tpl := T;
  NF^.Base := Base;
  NF^.PC := 0;
  NF^.RetIdx := Base - 1;
  NF^.Argc := Argc;
  NF^.Kind := 1;
  Inc(FrameTop);
  StackTop := Base + T^.MaxRegs;
  Inc(JitDepth);
  Res := TJitFn(T^.Jit)(@VStack[Base], Pointer(PPtrUInt(T^.JitTable)[0]), NF, 1);
  Dec(JitDepth);
  if Res = -2 then
  begin
    StackTop := Fr^.Base + Fr^.Tpl^.MaxRegs;
    Exit(0);
  end;
  {$IFDEF G2_JITSTATS}Inc(JitFallbacks);{$ENDIF}
  if Res = -1 then InjectThrow := True else NF^.PC := Res;
  if not Execute(FrameTop - 1, V) then
  begin
    StackTop := Fr^.Base + Fr^.Tpl^.MaxRegs;
    Exit(-1);
  end;
  VStack[Base - 1] := V;
  StackTop := Fr^.Base + Fr^.Tpl^.MaxRegs;
  Result := 0;
end;

{ ---- the code's slow paths (Spike A): an instruction's whole semantics, as Execute's, for a guard that failed or
  an instruction without inline code. Fr is the running frame, its PC already past the instruction; I the
  instruction. False: a throw pending ---- }

function JH_GetProp(Fr: PFrame; I: QWord): Boolean;
var R: PValue; A, J: Integer; VB, V: TValue; IC: PPropIC; O: PObj;
begin
  R := @VStack[Fr^.Base];
  A := (I shr 8) and $FFFF;
  VB := R[(I shr 24) and $FFFF];
  IC := @Fr^.Tpl^.ICs[I shr 40];
  if (VB shr 48) = TAG_PTR_HI then
  begin
    O := PObj(PtrUInt(VB and PTR_MASK));
    if O^.Hdr.Kind = ckObject then
    begin
      if ICGet(IC, O, R[A]) then Exit(True);
      if (TOp(I and $FF) = opGetLength) and (O^.OKind = okArray) and ((O^.OFlags and ofBigLen) = 0) then
      begin
        R[A] := MkInt(O^.Elements^.Len);
        Exit(True);
      end;
      if (TOp(I and $FF) = opGetLength) and (O^.OKind = okTypedArray) and (O^.Shape^.Count = 0) then
      begin
        R[A] := MkInt(TALength(O));
        Exit(True);
      end;
      J := ICAccGet(IC, O, VB, V);
      if J = 0 then Exit(False);
      if J = 1 then begin R[A] := V; Exit(True); end;
      if not ICGetMiss(IC, O, VB, V) then Exit(False);
      VStack[Fr^.Base + A] := V;
      Exit(True);
    end
    else if O^.Hdr.Kind = ckString then
    begin
      if TOp(I and $FF) = opGetLength then
      begin
        R[A] := MkInt(PStr(O)^.Len);
        Exit(True);
      end;
      O := ObjOf(StringProto);
      if O^.Shape = IC^.Shape then
      begin
        R[A] := GetSlot(O, IC^.Slot);
        Exit(True);
      end;
      ICFillString(IC);
    end;
  end;
  if not GetV(VB, IC^.Key, V) then Exit(False);
  VStack[Fr^.Base + A] := V;
  Result := True;
end;

function JH_SetProp(Fr: PFrame; I: QWord): Boolean;
var R: PValue; A: Integer;
begin
  R := @VStack[Fr^.Base];
  A := (I shr 8) and $FFFF;
  Result := XSetPropSlow(@Fr^.Tpl^.ICs[I shr 40], @R[A], R[(I shr 24) and $FFFF], Fr^.Tpl^.IsStrict);
end;

{ FastLoop's typed-array element read and write (an ordinary buffer, the view's length, a kind done without a
  call): False leaves it to the general path }
function JitTAGet(O: PObj; CI: Integer; var V: TValue): Boolean; inline;
var P: PByte;
begin
  Result := False;
  if (CI < 0) or ((ObjOf(O^.Aux1)^.OFlags and (bfShared or bfResizable or bfDetached)) <> 0) or
     (CI >= Int32(UInt32(O^.Aux2))) then Exit;
  P := @PBytes(PtrOf(ObjOf(O^.Aux1)^.Aux1))^.Data[0] + O^.Pad;
  case O^.OFlags shr 4 of
    taInt8: V := MkInt(PShortInt(P)[CI]);
    taUint8, taUint8C: V := MkInt(P[CI]);
    taInt16: V := MkInt(PSmallInt(P)[CI]);
    taUint16: V := MkInt(PWord(P)[CI]);
    taInt32: V := MkInt(PLongInt(P)[CI]);
    taUint32: V := MkNumber(PLongWord(P)[CI]);
    taFloat32: V := MkNumber(PSingle(P)[CI]);
    taFloat64: V := MkNumber(PDouble(P)[CI]);
  else Exit;
  end;
  Result := True;
end;

function JitTASet(O: PObj; CI: Integer; V: TValue): Boolean; inline;
var P: PByte;
begin
  Result := False;
  if (CI < 0) or ((ObjOf(O^.Aux1)^.OFlags and (bfShared or bfResizable or bfDetached or bfImmutable)) <> 0) or
     (CI >= Int32(UInt32(O^.Aux2))) then Exit;
  P := @PBytes(PtrOf(ObjOf(O^.Aux1)^.Aux1))^.Data[0] + O^.Pad;
  if IsInt(V) then
    case O^.OFlags shr 4 of
      taInt8, taUint8: P[CI] := Byte(IntOf(V));
      taInt16, taUint16: PWord(P)[CI] := Word(IntOf(V));
      taInt32, taUint32: PLongWord(P)[CI] := LongWord(IntOf(V));
      taFloat32: PSingle(P)[CI] := IntOf(V);
      taFloat64: PDouble(P)[CI] := IntOf(V);
    else Exit;
    end
  else if IsDouble(V) then
    case O^.OFlags shr 4 of
      taFloat32: PSingle(P)[CI] := DoubleOf(V);
      taFloat64: PDouble(P)[CI] := DoubleOf(V);
    else Exit;
    end
  else Exit;
  Result := True;
end;

function JH_GetElem(Fr: PFrame; I: QWord): Boolean;
var R: PValue; A, CI: Integer; VA, VB, V: TValue; O: PObj; E: PValueArray;
begin
  R := @VStack[Fr^.Base];
  A := (I shr 8) and $FFFF;
  VA := R[(I shr 24) and $FFFF];
  VB := R[I shr 40];
  if IsInt(VB) and IsObject(VA) then
  begin
    O := ObjOf(VA);
    E := O^.Elements;
    CI := IntOf(VB);
    if (E <> nil) and (CI >= 0) and (CI < Integer(E^.Len)) and (E^.Data[CI] <> VHole) then
    begin
      R[A] := E^.Data[CI];
      Exit(True);
    end;
    if (O^.OKind = okTypedArray) and JitTAGet(O, CI, V) then
    begin
      R[A] := V;
      Exit(True);
    end;
  end;
  if not GetElemV(VA, VB, V) then Exit(False);
  VStack[Fr^.Base + A] := V;
  Result := True;
end;

function JH_SetElem(Fr: PFrame; I: QWord): Boolean;
var R: PValue; A, CI: Integer; VA, VB, V: TValue; O: PObj; E: PValueArray;
begin
  R := @VStack[Fr^.Base];
  A := (I shr 8) and $FFFF;
  VA := R[A];
  VB := R[(I shr 24) and $FFFF];
  V := R[I shr 40];
  if IsInt(VB) and IsObject(VA) then
  begin
    O := ObjOf(VA);
    E := O^.Elements;
    CI := IntOf(VB);
    if (E <> nil) and (CI >= 0) and (CI < Integer(E^.Len)) and ((O^.OFlags and 64) = 0) and (E^.Data[CI] <> VHole) then
    begin
      E^.Data[CI] := V;
      WriteBarrier(@E^.Hdr, V);
      Exit(True);
    end;
    if (E <> nil) and (CI = Integer(E^.Len)) and (CI < Integer(E^.Cap)) and ((O^.OFlags and 64) = 0) and
       (O^.OKind = okArray) and AppendProtosPlain(O) then
    begin
      E^.Data[CI] := V;
      WriteBarrier(@E^.Hdr, V);
      E^.Len := CI + 1;
      Exit(True);
    end;
    if (O^.OKind = okTypedArray) and JitTASet(O, CI, V) then Exit(True);
  end;
  Result := SetElemVS(VA, VB, V, Fr^.Tpl^.IsStrict);
end;

function JH_GetGlobal(Fr: PFrame; I: QWord): Boolean;
var R: PValue; A, CI: Integer; IC: PPropIC; O: PObj; V: TValue; Bool: Boolean;
begin
  R := @VStack[Fr^.Base];
  A := (I shr 8) and $FFFF;
  IC := @Fr^.Tpl^.ICs[I shr 40];
  O := ObjOf(GlobalObj);
  if O^.Shape = IC^.Shape then R[A] := GetSlot(O, IC^.Slot)
  else
  begin
    CI := GLexFind(IC^.Key);
    if CI >= 0 then
    begin
      IC^.Shape := nil;
      IC^.Depth := -2 - CI;
      if not GLexGet(CI, V) then Exit(False);
      R := @VStack[Fr^.Base];
      R[A] := V;
      if TOp(I and $FF) = opTypeofGlobal then R[A] := TypeofValue(R[A]);
      Exit(True);
    end;
    CI := ShapeLookup(O^.Shape, IC^.Key);
    if CI < 0 then
    begin
      if not InheritedHasV(O, IC^.Key, Bool) then Exit(False);
      if Bool then
      begin
        if not GetV(GlobalObj, IC^.Key, V) then Exit(False);
        VStack[Fr^.Base + A] := V;
        if TOp(I and $FF) = opTypeofGlobal then VStack[Fr^.Base + A] := TypeofValue(V);
        Exit(True);
      end;
      if TOp(I and $FF) = opTypeofGlobal then begin R[A] := MkPtr(AtomOf('undefined')); Exit(True); end;
      XThrowNotDefined(IC^.Key);
      Exit(False);
    end;
    if (O^.Shape^.PFlags[CI] and pfAccessor) = 0 then
    begin
      IC^.Shape := O^.Shape; IC^.Slot := CI;
      R[A] := GetSlot(O, CI);
    end
    else
    begin
      if not GetV(GlobalObj, IC^.Key, V) then Exit(False);
      VStack[Fr^.Base + A] := V;
    end;
  end;
  if TOp(I and $FF) = opTypeofGlobal then VStack[Fr^.Base + A] := TypeofValue(VStack[Fr^.Base + A]);
  Result := True;
end;

function JH_Compare(Fr: PFrame; I: QWord): Boolean;
var R: PValue; A, Cmp: Integer; VA, VB: TValue; Bool: Boolean;
begin
  R := @VStack[Fr^.Base];
  A := (I shr 8) and $FFFF;
  VA := R[(I shr 24) and $FFFF];
  VB := R[I shr 40];
  case TOp(I and $FF) of
    opSEq: begin R[A] := MkBool(SEqFast(VA, VB)); Exit(True); end;
    opSNe: begin R[A] := MkBool(not SEqFast(VA, VB)); Exit(True); end;
    opEq, opNe:
      begin
        if not LooseEquals(VA, VB, Bool) then Exit(False);
        if TOp(I and $FF) = opNe then Bool := not Bool;
        VStack[Fr^.Base + A] := MkBool(Bool);
        Exit(True);
      end;
  end;
  if IsInt(VA) and IsInt(VB) then
  begin
    case TOp(I and $FF) of
      opLt: Bool := IntOf(VA) < IntOf(VB);
      opLe: Bool := IntOf(VA) <= IntOf(VB);
      opGt: Bool := IntOf(VA) > IntOf(VB);
    else Bool := IntOf(VA) >= IntOf(VB);
    end;
  end
  else if IsNumber(VA) and IsNumber(VB) then
  begin
    case TOp(I and $FF) of
      opLt: Bool := NumOf(VA) < NumOf(VB);
      opLe: Bool := NumOf(VA) <= NumOf(VB);
      opGt: Bool := NumOf(VA) > NumOf(VB);
    else Bool := NumOf(VA) >= NumOf(VB);
    end;
  end
  else
  begin
    if not CompareSlow(VA, VB, Cmp) then Exit(False);
    case TOp(I and $FF) of
      opLt: Bool := Cmp = -1;
      opLe: Bool := (Cmp = -1) or (Cmp = 0);
      opGt: Bool := Cmp = 1;
    else Bool := (Cmp = 1) or (Cmp = 0);
    end;
  end;
  VStack[Fr^.Base + A] := MkBool(Bool);
  Result := True;
end;

function JH_Alloc(Fr: PFrame; I: QWord): Boolean;
var R: PValue; A: Integer;
begin
  R := @VStack[Fr^.Base];
  A := (I shr 8) and $FFFF;
  case TOp(I and $FF) of
    opClosure: XFastClosure(Fr, R, A, I shr 40);
    opNewObj: XFastNewObj(R, A, I shr 40);
    opDefProp: XFastDefProp(R[A], PStr(PtrOf(Fr^.Tpl^.Consts[I shr 40])), R[(I shr 24) and $FFFF]);
    opNot: R[A] := MkBool(not ToBool(R[(I shr 24) and $FFFF]));
    opTypeof: R[A] := TypeofValue(R[(I shr 24) and $FFFF]);
  end;
  Result := True;
end;

function JH_ToBool(V: TValue): Boolean;
begin
  Result := ToBool(V);
end;

{ the call of helper H (Fr, I) for instruction PC; a False result returns -1 (the throw pending) }
procedure EmitHelper(H: Pointer; PC: Integer; I: QWord);
begin
  JRex(False, 0, 0, jR12); JB($C7); JMem(0, jR12, PtrInt(@PFrame(nil)^.PC)); JD(Cardinal(PC + 1));
  MovRR(jDI, jR12);
  MovRI(jSI, I);
  CallAbs(H);
  JB($84); JB($C0);                                { test al, al }
  JccThrow(ccE);
end;

function JH_Arith(Fr: PFrame; I: QWord): Boolean;
var R: PValue; A, CI, NP: Integer; VA, VB, V: TValue; Sum: Int64; D: Double; Op: TOp;
begin
  R := @VStack[Fr^.Base];
  A := (I shr 8) and $FFFF;
  Op := TOp(I and $FF);
  VA := R[(I shr 24) and $FFFF];
  { C is a register only for the two-operand instructions (the immediates' and unary ones' C is not) }
  case Op of
    opAdd, opSub, opMul, opDiv, opMod, opShr, opUShr, opBitAnd, opBitOr, opBitXor, opShl, opPow: VB := R[I shr 40];
  else VB := VUndefined;
  end;
  case Op of
    opAdd:
      if IsInt(VA) and IsInt(VB) then
      begin
        Sum := Int64(IntOf(VA)) + Int64(IntOf(VB));
        if (Sum >= -2147483648) and (Sum <= 2147483647) then R[A] := MkInt(Int32(Sum)) else R[A] := MkDouble(Sum);
      end
      else if IsNumber(VA) and IsNumber(VB) then R[A] := MkDouble(NumOf(VA) + NumOf(VB))
      else if IsString(VA) and IsString(VB) then R[A] := ConcatStrings(StrOf(VA), StrOf(VB))
      else
      begin
        if not AddNumbersSlow(VA, VB, V) then Exit(False);
        VStack[Fr^.Base + A] := V;
      end;
    opAddI, opSubI, opIncI:
      begin
        CI := SarInt64(Int64(I), 40);
        if IsInt(VA) then
        begin
          Sum := Int64(IntOf(VA)) + CI;
          if (Sum >= -2147483648) and (Sum <= 2147483647) then R[A] := MkInt(Int32(Sum)) else R[A] := MkDouble(Sum);
        end
        else if VA < DOUBLE_LIMIT then R[A] := MkDouble(PDouble(@VA)^ + CI)
        else
        begin
          if not XAddImmSlow(Op, VA, CI, V) then Exit(False);
          VStack[Fr^.Base + A] := V;
        end;
      end;
    opSub, opMul:
      if IsInt(VA) and IsInt(VB) then
      begin
        if Op = opSub then Sum := Int64(IntOf(VA)) - Int64(IntOf(VB)) else Sum := Int64(IntOf(VA)) * Int64(IntOf(VB));
        if (Sum >= -2147483648) and (Sum <= 2147483647) and not ((Sum = 0) and (Op = opMul) and
           ((IntOf(VA) < 0) or (IntOf(VB) < 0))) then R[A] := MkInt(Int32(Sum))
        else if Op = opSub then R[A] := MkDouble(Sum)
        else R[A] := MkDouble(Double(IntOf(VA)) * Double(IntOf(VB)));
      end
      else if IsNumber(VA) and IsNumber(VB) then
      begin
        if Op = opSub then R[A] := MkDouble(NumOf(VA) - NumOf(VB)) else R[A] := MkDouble(NumOf(VA) * NumOf(VB));
      end
      else
      begin
        if not ArithSlow(Op, VA, VB, V) then Exit(False);
        VStack[Fr^.Base + A] := V;
      end;
    opDiv:
      if IsNumber(VA) and IsNumber(VB) then R[A] := MkNumber(NumOf(VA) / NumOf(VB))
      else
      begin
        if not ArithSlow(opDiv, VA, VB, V) then Exit(False);
        VStack[Fr^.Base + A] := V;
      end;
    opMod:
      if IsInt(VA) and IsInt(VB) and (IntOf(VB) > 0) and (IntOf(VA) >= 0) then R[A] := MkInt(IntOf(VA) mod IntOf(VB))
      else if IsNumber(VA) and IsNumber(VB) then R[A] := MkNumber(NumberRemainder(NumOf(VA), NumOf(VB)))
      else
      begin
        if not ArithSlow(opMod, VA, VB, V) then Exit(False);
        VStack[Fr^.Base + A] := V;
      end;
    opBitAndI, opBitOrI, opBitXorI, opShlI, opShrI, opUShrI:
      begin
        if not XBitImm(Op, VA, Int32(SarInt64(Int64(I), 40)), V) then Exit(False);
        VStack[Fr^.Base + A] := V;
      end;
    opPow:
      if IsNumber(VA) and IsNumber(VB) then R[A] := MkNumber(NumberExponentiate(NumOf(VA), NumOf(VB)))
      else
      begin
        if not ArithSlow(opPow, VA, VB, V) then Exit(False);
        VStack[Fr^.Base + A] := V;
      end;
    opShr, opUShr:
      if QuickInt32(VA, CI) and QuickInt32(VB, NP) then
      begin
        if Op = opShr then R[A] := MkInt(SarLongint(CI, NP and 31)) else R[A] := MkNumber(UInt32(CI) shr (NP and 31));
      end
      else
      begin
        if not ArithSlow(Op, VA, VB, V) then Exit(False);
        VStack[Fr^.Base + A] := V;
      end;
    opBitAnd, opBitOr, opBitXor, opShl:
      if QuickInt32(VA, CI) and QuickInt32(VB, NP) then
        case Op of
          opBitAnd: R[A] := MkInt(CI and NP);
          opBitOr: R[A] := MkInt(CI or NP);
          opBitXor: R[A] := MkInt(CI xor NP);
        else R[A] := MkInt(Int32(UInt32(CI) shl (NP and 31)));
        end
      else
      begin
        if not ArithSlow(Op, VA, VB, V) then Exit(False);
        VStack[Fr^.Base + A] := V;
      end;
    opNeg:
      if IsInt(VA) and (IntOf(VA) <> 0) and (IntOf(VA) <> Low(Int32)) then R[A] := MkInt(-IntOf(VA))
      else if IsBigInt(VA) then R[A] := XBigNegate(VA)
      else
      begin
        if not ToNumericV(VA, V) then Exit(False);
        if IsBigInt(V) then V := XBigNegate(V) else V := MkDouble(-NumOf(V));
        VStack[Fr^.Base + A] := V;
      end;
    opBitNot:
      if IsInt(VA) then R[A] := MkInt(not IntOf(VA))
      else
      begin
        if not ToNumericV(VA, V) then Exit(False);
        if IsBigInt(V) then V := XBigAddInt(XBigNegate(V), -1)
        else if not ArithSlow(opBitXor, V, MkInt(-1), V) then Exit(False);
        VStack[Fr^.Base + A] := V;
      end;
    opToNum, opToNumeric:
      if IsNumber(VA) then R[A] := VA
      else if IsBigInt(VA) and (Op = opToNumeric) then R[A] := VA
      else
      begin
        if not ToNumberV(VA, D) then Exit(False);
        VStack[Fr^.Base + A] := MkNumber(D);
      end;
  end;
  Result := True;
end;

{ opCheckThis's throw: a derived constructor's this before super() }
function JH_CheckThis(Fr: PFrame; I: QWord): Boolean;
begin
  Result := ThrowError('ReferenceError', 'Must call super constructor in derived class before accessing ''this'' or returning from derived constructor');
end;

{ opSuperCall / opSuperCallV (Execute's case, without its frame push): 13.3.7.1 SuperCall, the parent constructed
  with this frame's new.target, this bound, then the fields }
function JH_SuperCall(Fr: PFrame; I: QWord): Boolean;
var R: PValue; A, Argc, J: Integer; O: PObj; V, VA: TValue;
begin
  R := @VStack[Fr^.Base];
  A := (I shr 8) and $FFFF;
  if TOp(I and $FF) = opSuperCallV then
  begin
    O := ObjOf(R[A + 2]);
    Argc := ArrayLen(O);
    if Fr^.Base + A + 2 + Argc + 4 >= STACK_MAX then Exit(ThrowError('RangeError', 'Maximum call stack size exceeded'));
    for J := 0 to Argc - 1 do
    begin
      V := O^.Elements^.Data[J];
      if V = VHole then V := VUndefined;
      R[A + 2 + J] := V;
    end;
  end
  else Argc := (I shr 24) and $FFFF;
  if (I shr 40) = 1 then VA := R[A + 1] else VA := ObjOf(Fr^.Closure)^.Shape^.Proto;
  if not IsConstructorV(VA) then Exit(ThrowTypeError('Super constructor is not a constructor'));
  StackTop := Max(StackTop, Fr^.Base + A + 2 + Argc);
  if not Construct(VA, @R[A + 2], Argc, Fr^.NewTarget, V) then Exit(False);
  StackTop := Fr^.Base + Fr^.Tpl^.MaxRegs;
  R := @VStack[Fr^.Base];
  if R[0] <> VHole then Exit(ThrowError('ReferenceError', 'Super constructor may only be called once'));
  R[0] := V;
  R[A] := V;
  Result := RunFieldInit(Fr^.Closure, V);
end;

{ opDefField / opDefFieldC (Execute's case): a class field's CreateDataPropertyOrThrow; opDefField caches the add
  transition }
function JH_DefField(Fr: PFrame; I: QWord): Boolean;
var R: PValue; A: Integer; IC: PPropIC; Key: PStr; O: PObj; V, VB: TValue;
begin
  R := @VStack[Fr^.Base];
  A := (I shr 8) and $FFFF;
  if TOp(I and $FF) = opDefField then
  begin
    IC := @Fr^.Tpl^.ICs[I shr 40];
    Key := IC^.Key;
    O := ObjOf(R[A]);
    if (O^.Shape = IC^.Shape) and (IC^.NewShape <> nil) and (IC^.Slot < O^.InlineCap) and ((O^.OFlags and 8) = 0) then
    begin
      V := R[(I shr 24) and $FFFF];
      O^.Shape := IC^.NewShape;
      O^.Slots[IC^.Slot] := V;
      WriteBarrier(@O^.Hdr, V);
      Exit(True);
    end;
    VB := MkPtr(O^.Shape);
    if not CreateDataPropOrThrow(R[A], Key, R[(I shr 24) and $FFFF]) then Exit(False);
    O := ObjOf(VStack[Fr^.Base + A]);
    if (O^.OKind = okPlain) and not O^.Shape^.IsDict and (O^.Shape^.Parent = PShape(PtrOf(VB))) and
       (O^.Shape^.Count > 0) and (O^.Shape^.Keys[O^.Shape^.Count - 1] = Key) then
    begin
      IC^.Shape := PShape(PtrOf(VB));
      IC^.NewShape := O^.Shape;
      IC^.Slot := O^.Shape^.Count - 1;
    end;
    Exit(True);
  end;
  if not ToPropKey(R[I shr 40], Key) then Exit(False);
  R := @VStack[Fr^.Base];
  Result := CreateDataPropOrThrow(R[A], Key, R[(I shr 24) and $FFFF]);
end;

{ instanceof's walks, cached by the receiver's shape and the prototype looked for (main thread only): valid while no
  prototype changed (ProtoEpoch; not when AddICUnsafe says it would not be bumped) and no collection moved or freed
  an object (a prototype's address could be another object's after one) }
type
  TInstOfEntry = record Shape: Pointer; Target: TValue; Epoch, Minor, Major: Integer; Res: Integer; end;
var
  InstOfCache: array[0..1023] of TInstOfEntry;

{ (the walk and the store on their own: AddICUnsafe is a threadvar, which inlined loads FPC miscompiles) }
function CachedProtoChainMiss(O: PObj; Target: TValue; E: Pointer): Integer; noinline;
var P: ^TInstOfEntry;
begin
  Result := FastProtoChainHas(O, Target);
  if (Result <> 2) and not AddICUnsafe and (GetCurrentThreadId = JitMainThread) then
  begin
    P := E;
    P^.Shape := O^.Shape; P^.Target := Target; P^.Epoch := ProtoEpoch;
    P^.Minor := GCMinorCount; P^.Major := GCMajorCount; P^.Res := Result;
  end;
end;

function CachedProtoChainHas(O: PObj; Target: TValue): Integer; inline;
var E: ^TInstOfEntry;
begin
  E := @InstOfCache[((PtrUInt(O^.Shape) shr 4) xor (PtrUInt(Target) shr 3)) and 1023];
  if (E^.Shape = Pointer(O^.Shape)) and (E^.Target = Target) and (E^.Epoch = ProtoEpoch) and
     (E^.Minor = GCMinorCount) and (E^.Major = GCMajorCount) then Exit(E^.Res);
  Result := CachedProtoChainMiss(O, Target, E);
end;

{ instanceof (JH_Misc's case on its own, so the hot path keeps its locals in registers): FastLoop's guarded
  prototype-chain walk for a bytecode constructor, else InstanceOf }
function JH_InstanceOf(Fr: PFrame; I: QWord): Boolean;
var R: PValue; V, VB: TValue; O: PObj; Tpl: PFuncTemplate; J: Integer; Bool: Boolean;
begin
  R := @VStack[Fr^.Base];
  V := R[I shr 40];
  VB := R[(I shr 24) and $FFFF];
  if IsObject(V) and not MultiRealmAny then
  begin
    O := ObjOf(V);
    Tpl := PFuncTemplate(O^.Native);
    if (O^.OKind = okFunction) and (Pointer(O^.Shape) = Tpl^.NoHIShape) and (Pointer(O^.Shape) = Tpl^.CtorFnShape) and
       (ObjOf(FunctionProto)^.Shape = HIFPShape) then
    begin
      V := GetSlot(O, Tpl^.CtorProtoSlot);
      if IsObject(V) then
      begin
        if not IsObject(VB) then begin R[(I shr 8) and $FFFF] := VFalse; Exit(True); end;
        J := CachedProtoChainHas(ObjOf(VB), V);
        if J <> 2 then begin R[(I shr 8) and $FFFF] := MkBool(J = 1); Exit(True); end;
      end;
    end;
  end;
  if not InstanceOf(VB, R[I shr 40], Bool) then Exit(False);
  VStack[Fr^.Base + Integer((I shr 8) and $FFFF)] := MkBool(Bool);
  Result := True;
end;

function JH_Misc(Fr: PFrame; I: QWord): Boolean;
var R: PValue; A, B, C, J: Integer; V, VB: TValue; O: PObj; Bool: Boolean; Idx: Int64; Key: PStr; U: PUpval;
begin
  R := @VStack[Fr^.Base];
  A := (I shr 8) and $FFFF;
  B := (I shr 24) and $FFFF;
  C := I shr 40;
  case TOp(I and $FF) of
    opInstanceof:
      begin
        { FastLoop's case: a bytecode function whose shape InstanceOf verified and whose prototype slot is cached,
          walked without proxies }
        V := R[C];
        if IsObject(V) and not MultiRealmAny then
        begin
          O := ObjOf(V);
          if (O^.OKind = okFunction) and (Pointer(O^.Shape) = PFuncTemplate(O^.Native)^.NoHIShape) and
             (Pointer(O^.Shape) = PFuncTemplate(O^.Native)^.CtorFnShape) and
             (ObjOf(FunctionProto)^.Shape = HIFPShape) then
          begin
            V := GetSlot(O, PFuncTemplate(O^.Native)^.CtorProtoSlot);
            if IsObject(V) then
            begin
              if not IsObject(R[B]) then begin R[A] := VFalse; Exit(True); end;
              J := FastProtoChainHas(ObjOf(R[B]), V);
              if J <> 2 then begin R[A] := MkBool(J = 1); Exit(True); end;
            end;
          end;
        end;
        if not InstanceOf(R[B], R[C], Bool) then Exit(False);
        VStack[Fr^.Base + A] := MkBool(Bool);
      end;
    opIn:
      begin
        if not IsObject(R[C]) then
          Exit(ThrowTypeError('Cannot use ''in'' operator to search for a key in a non-object'));
        O := ObjOf(R[C]);
        Bool := False;
        if IndexFromValue(R[B], Idx) and (O^.Elements <> nil) and (Idx < O^.Elements^.Len) and
           (O^.Elements^.Data[Idx] <> VHole) then Bool := True
        else
        begin
          if not ToPropKey(R[B], Key) then Exit(False);
          if not HasPropertyV(VStack[Fr^.Base + C], Key, Bool) then Exit(False);
        end;
        VStack[Fr^.Base + A] := MkBool(Bool);
      end;
    opNewArr:
      begin
        O := NewArray(B, Max(B, 4));
        R := @VStack[Fr^.Base];
        if (O^.Elements^.Hdr.Flags and cfOld) <> 0 then
          for J := 0 to B - 1 do ValueArrayStore(O^.Elements, J, R[C + J])
        else
          for J := 0 to B - 1 do O^.Elements^.Data[J] := R[C + J];
        R[A] := MkPtr(O);
      end;
    opGetIter:
      begin
        if not GetIteratorRec(R[B], V, VB) then Exit(False);
        VStack[Fr^.Base + A] := V;
        VStack[Fr^.Base + A + 1] := VB;
      end;
    opForIn:
      begin
        if not ForInKeys(R[B], V, VB) then Exit(False);
        VStack[Fr^.Base + A] := V;
        VStack[Fr^.Base + C] := VB;
        VStack[Fr^.Base + A + 1] := ArrayIterNextFnOf;
      end;
    opArrPush: ArrayPush(ObjOf(R[A]), R[B]);
    opArrHole: ArrayPush(ObjOf(R[A]), VHole);
    opClose: CloseUpvals(Fr^.Base + A);
    opToStr:
      if not IsString(R[B]) then
      begin
        if not ToStringV(R[B], V) then Exit(False);
        VStack[Fr^.Base + A] := V;
      end
      else R[A] := R[B];
    opRegExp:
      begin
        if not Assigned(RegExpCreateHook) then Exit(ThrowError('SyntaxError', 'RegExp is not available'));
        if not RegExpCreateHook(PStr(PtrOf(Fr^.Tpl^.Consts[B])), PStr(PtrOf(Fr^.Tpl^.Consts[C])), V) then Exit(False);
        VStack[Fr^.Base + A] := V;
      end;
    opGetPriv:
      begin
        if not PrivateGet(R[B], PStr(PtrOf(R[C])), V) then Exit(False);
        VStack[Fr^.Base + A] := V;
      end;
    opSetPriv: if not PrivateSet(R[A], PStr(PtrOf(R[C])), R[B]) then Exit(False);
    opPrivIn:
      begin
        if not IsObject(R[B]) then
          Exit(ThrowTypeError('Cannot use ''in'' operator to search for a private field in a non-object'));
        if ShapeLookup(ObjOf(R[B])^.Shape, PStr(PtrOf(R[C]))) >= 0 then R[A] := VTrue else R[A] := VFalse;
      end;
    opSetUpv:
      begin
        U := PUpval(PtrOf(PValueArray(PtrOf(ObjOf(Fr^.Closure)^.Aux2))^.Data[B]));
        if U^.StackIndex >= 0 then VStack[U^.StackIndex] := R[A]
        else
        begin
          U^.Value := R[A];
          WriteBarrier(@U^.Hdr, R[A]);
        end;
      end;
  end;
  Result := True;
end;

{ a compare-and-jump's slow path: 1 jump, 0 not, -1 a throw pending }
function JH_JumpCmp(Fr: PFrame; I: QWord): Integer;
var R: PValue; VA, VB: TValue; Cmp: Integer; Bool: Boolean;
begin
  R := @VStack[Fr^.Base];
  VA := R[(I shr 8) and $FFFF];
  VB := R[(I shr 24) and $FFFF];
  case TOp(I and $FF) of
    opJSEq: Exit(Ord(SEqFast(VA, VB)));
    opJNSEq: Exit(Ord(not SEqFast(VA, VB)));
  end;
  if IsNumber(VA) and IsNumber(VB) then
    case TOp(I and $FF) of
      opJNLt: Bool := NumOf(VA) < NumOf(VB);
      opJNLe: Bool := NumOf(VA) <= NumOf(VB);
      opJNGt: Bool := NumOf(VA) > NumOf(VB);
    else Bool := NumOf(VA) >= NumOf(VB);
    end
  else
  begin
    if not CompareSlow(VA, VB, Cmp) then Exit(-1);
    case TOp(I and $FF) of
      opJNLt: Bool := Cmp = -1;
      opJNLe: Bool := (Cmp = -1) or (Cmp = 0);
      opJNGt: Bool := Cmp = 1;
    else Bool := (Cmp = 1) or (Cmp = 0);
    end;
  end;
  Result := Ord(not Bool);
end;

{ opIterNext over a suspended generator with the intrinsic next (FastLoop's Kind 4 case), its body compiled:
  resumed inline (XGenInline) and its code run here; a yield is suspended here (as FastLoop's opYield), a return
  completes it (XGenInlineReturn). Any other stop finishes the step as GeneratorResume does: the frame becomes
  Kind 3 and a nested Execute runs it to its next yield, its return or its throw. 1 done (the jump), 0 the value
  in the consumer's R[A], -1 a throw pending, 2 not taken (nothing changed: IteratorStepRec's) }
{ N values from Src to Dst, and N undefineds from Dst (a generator's registers: FPC's loops reload and spill per
  value, about seven instructions each) }
procedure CopyValues(Dst, Src: PValue; N: NativeInt); assembler; nostackframe;
asm
  testq %rdx, %rdx
  jle .Ldone
  testq $1, %rdx
  je .Lpairs
  movq (%rsi), %rax
  movq %rax, (%rdi)
  addq $8, %rsi
  addq $8, %rdi
  decq %rdx
  je .Ldone
.Lpairs:
  movq (%rsi), %rax
  movq 8(%rsi), %rcx
  movq %rax, (%rdi)
  movq %rcx, 8(%rdi)
  addq $16, %rsi
  addq $16, %rdi
  subq $2, %rdx
  jne .Lpairs
.Ldone:
end;

procedure FillValues(Dst: PValue; N: NativeInt; V: TValue); assembler; nostackframe;
asm
  testq %rsi, %rsi
  jle .Ldone
.Lloop:
  movq %rdx, (%rdi)
  addq $8, %rdi
  decq %rsi
  jne .Lloop
.Ldone:
end;

{ SaveSuspension's case without handlers or open upvalues on frame GF (the newest) and a state array big enough
  (nothing allocates): the live registers into it, the yielded value into the consumer's register, the frame
  popped. False: SaveSuspension's (nothing changed) }
function JitGenSuspendFast(GF: PFrame; T: PFuncTemplate; Ins: QWord; Res: Integer): Boolean;
var O: PObj; E: PValueArray; Live, ResumeCode, N, J: Integer; R: PValue; V: TValue;
begin
  Result := False;
  if (HandlerTop > 0) and (Handlers[HandlerTop - 1].Frame = FrameTop - 1) then Exit;
  if (OpenCount > 0) and (PUpval(PtrOf(OpenUpvals[OpenCount - 1]))^.StackIndex >= GF^.Base) then Exit;
  O := ObjOf(GF^.NewTarget);
  E := PValueArray(PtrOf(O^.Aux2));
  N := T^.MaxRegs + 4;
  if Integer(E^.Cap) < N then Exit;
  Live := Integer(Ins shr 40);
  ResumeCode := Integer((Ins shr 8) and $FFFF);
  if (Live <= 0) or (Live > T^.MaxRegs) then Live := T^.MaxRegs;
  if ResumeCode >= Live then Live := ResumeCode + 1;
  if Live > T^.MaxRegs then Live := T^.MaxRegs;
  R := @VStack[GF^.Base];
  V := R[(Ins shr 24) and $FFFF];
  E^.Len := N;
  CopyValues(@E^.Data[0], R, Live);
  E^.Data[T^.MaxRegs] := MkInt(ResumeCode);
  E^.Data[T^.MaxRegs + 1] := MkInt(Live);
  E^.Data[T^.MaxRegs + 2] := MkInt(0);
  E^.Data[T^.MaxRegs + 3] := MkInt(0);
  if (E^.Hdr.Flags and (cfOld or cfRemembered)) = cfOld then Remember(@E^.Hdr);
  O^.Pad := Res + 1;
  O^.OFlags := 1;
  Dec(FrameTop);
  VStack[GF^.RetIdx] := V;
  Result := True;
end;

function JitGenStep(Fr: PFrame; A, B: Integer; Gen: TValue): Integer;
var G: PObj; T: PFuncTemplate; GF: PFrame; Res, SavedTop, J, Base, NLive: Integer; Ins: QWord; V, Gv: TValue;
  Ok, Done: Boolean; State: PValueArray; R: PValue;
begin
  G := ObjOf(Gen);
  T := PFuncTemplate(ObjOf(G^.Aux1)^.Native);
  if T^.JitState <> 1 then
  begin
    if T^.JitState = 0 then
    begin
      Inc(T^.JitCount);
      if (T^.JitCount >= JitHot) and (GetCurrentThreadId = JitMainThread) then JitCompile(T);
    end;
    if T^.JitState <> 1 then Exit(2);
  end;
  if (PPtrUInt(T^.JitTable)[G^.Pad] = 0) or JitTooDeep or
     (StackTop + T^.MaxRegs + 4 >= STACK_MAX) or (FrameTop >= FRAME_MAX - 2) then Exit(2);
  SavedTop := StackTop;
  State := PValueArray(PtrOf(G^.Aux2));
  if (IntOf(State^.Data[T^.MaxRegs + 2]) = 0) and (IntOf(State^.Data[T^.MaxRegs + 3]) = 0) and
     (IntOf(State^.Data[T^.MaxRegs]) >= -1) then
  begin
    { XGenInline's resume without handlers or upvalues to restore, its copies as loops }
    Base := StackTop;
    NLive := IntOf(State^.Data[T^.MaxRegs + 1]);
    R := @VStack[Base];
    CopyValues(R, @State^.Data[0], NLive);
    FillValues(@R[NLive], T^.MaxRegs - NLive, VUndefined);
    GF := @Frames[FrameTop];
    GF^.Closure := G^.Aux1;
    GF^.NewTarget := Gen;
    GF^.Tpl := T;
    GF^.Base := Base;
    GF^.PC := G^.Pad;
    GF^.RetIdx := Fr^.Base + A;
    GF^.Argc := 0;
    GF^.Kind := 4;
    Inc(FrameTop);
    J := IntOf(State^.Data[T^.MaxRegs]);
    if J >= 0 then R[J] := VUndefined;
    StackTop := Base + T^.MaxRegs;
    G^.OFlags := 2;
    GenYielded := False;
  end
  else
  begin
    if not XGenInline(Gen, Fr^.Base + A) then Exit(-1);
    GF := @Frames[FrameTop - 1];
  end;
  Inc(JitDepth);
  Res := TJitFn(T^.Jit)(@VStack[GF^.Base], Pointer(PPtrUInt(T^.JitTable)[GF^.PC]), GF, 0);
  Dec(JitDepth);
  if Res >= 0 then
  begin
    Ins := T^.Code[Res];
    if (TOp(Ins and $FF) = opYield) and JitGenSuspendFast(GF, T, Ins, Res) then
    begin
      StackTop := SavedTop;
      Exit(0);
    end;
    if TOp(Ins and $FF) = opYield then
    begin
      { suspended, its value in the consumer's register }
      PushRoot(VStack[GF^.Base + Integer((Ins shr 24) and $FFFF)]);
      J := GF^.RetIdx;
      SaveSuspension(GF, T, @VStack[GF^.Base], Integer((Ins shr 8) and $FFFF), Res + 1, Integer(Ins shr 40));
      GenYielded := False;
      VStack[J] := RootAt(TempTop - 1);
      PopRoots(1);
      StackTop := SavedTop;
      Exit(0);
    end;
    if (TOp(Ins and $FF) = opRet) or (TOp(Ins and $FF) = opRetUndef) then
    begin
      { completed: done, the consumer's loop ends (XGenInlineReturn leaves its iterator done) }
      GF^.PC := Res + 1;
      XGenInlineReturn(GF);
      StackTop := SavedTop;
      Exit(1);
    end;
    GF^.PC := Res;
  end
  else InjectThrow := True;
  { the rest of the step as GeneratorResume's: a Kind 3 frame in a nested Execute }
  GF^.Kind := 3;
  GF^.RetIdx := GF^.Base;
  GenYielded := False;
  GenYieldRaw := False;
  PushRoot(Gen);
  Ok := Execute(FrameTop - 1, V);
  Gv := RootAt(TempTop - 1);
  PopRoots(1);
  StackTop := SavedTop;
  G := ObjOf(Gv);
  if not Ok then
  begin
    G^.OFlags := 3;
    VStack[Fr^.Base + B] := VUndefined;   { an abrupt next() leaves the iterator done (7.4.8): it is not closed }
    if IsRetComp(Pending) then
    begin
      Pending := VUndefined;
      GenYieldRaw := False;
      Exit(1);
    end;
    Exit(-1);
  end;
  if not GenYielded then
  begin
    G^.OFlags := 3;
    VStack[Fr^.Base + B] := VUndefined;
    Exit(1);
  end;
  GenYielded := False;
  if GenYieldRaw then
  begin
    { yield*'s inner result as it is (IteratorStepRec's): its done, then its value }
    GenYieldRaw := False;
    if not IsObject(V) then
    begin
      VStack[Fr^.Base + B] := VUndefined;
      Exit(Ord(ThrowTypeError('iterator result is not an object')) - 1);
    end;
    PushRoot(V);
    if not GetV(V, AtomDone, Gv) then begin PopRoots(1); VStack[Fr^.Base + B] := VUndefined; Exit(-1); end;
    Done := ToBool(Gv);
    if Done then begin PopRoots(1); VStack[Fr^.Base + B] := VUndefined; Exit(1); end;
    if not GetV(RootAt(TempTop - 1), AtomValue, Gv) then begin PopRoots(1); VStack[Fr^.Base + B] := VUndefined; Exit(-1); end;
    PopRoots(1);
    V := Gv;
  end;
  VStack[Fr^.Base + A] := V;
  Result := 0;
end;

{ opIterNext's slow path (Execute's, a suspended generator resumed through GeneratorResume): 1 done (the jump),
  0 the value in R[A], -1 a throw pending }
function JH_IterNext(Fr: PFrame; I: QWord): Integer;
var A, B, CI: Integer; VA, VB, V: TValue; O: PObj; Done: Boolean;
begin
  A := (I shr 8) and $FFFF;
  B := (I shr 24) and $FFFF;
  VB := VStack[Fr^.Base + B];
  { a done iterator record: no next() call, the value stays as it is }
  if not IsObject(VB) then Exit(1);
  O := ObjOf(VB);
  if (O^.OKind = okIterator) and (O^.OFlags = 0) and (VStack[Fr^.Base + B + 1] = ArrayIterNextFnOf) then
  begin
    VA := O^.Aux1;
    if VA = VUndefined then
    begin
      VStack[Fr^.Base + B] := VUndefined;
      Exit(1);
    end;
    CI := IntOf(O^.Aux2);
    if (ObjOf(VA)^.Elements <> nil) and ((ObjOf(VA)^.OFlags and 64) = 0) and
       (ObjOf(VA)^.OKind in [okArray, okArguments]) and (CI >= Integer(ObjOf(VA)^.Elements^.Len)) then
    begin
      O^.Aux1 := VUndefined;
      VStack[Fr^.Base + B] := VUndefined;
      Exit(1);
    end;
  end;
  if (O^.OKind = okGenerator) and (O^.OFlags <= 1) and (VStack[Fr^.Base + B + 1] = GenNextFn) and
     not MultiRealmAny then
  begin
    Result := JitGenStep(Fr, A, B, VB);
    if Result <> 2 then Exit;
  end;
  if not IteratorStepRec(VB, VStack[Fr^.Base + B + 1], V, Done) then
  begin
    VStack[Fr^.Base + B] := VUndefined;   { an abrupt next() leaves the iterator done (7.4.8): it is not closed }
    Exit(-1);
  end;
  if Done then
  begin
    VStack[Fr^.Base + B] := VUndefined;
    Exit(1);
  end;
  VStack[Fr^.Base + A] := V;
  Result := 0;
end;

{ the call of the jump helper JitSlow (JH_JumpCmp, JH_IterNext) for instruction PC: jumps to Target, falls
  through, or returns -1 }
procedure EmitJumpHelper(PC: Integer; I: QWord; Target: Integer);
var L: Integer;
begin
  JRex(False, 0, 0, jR12); JB($C7); JMem(0, jR12, PtrInt(@PFrame(nil)^.PC)); JD(Cardinal(PC + 1));
  MovRR(jDI, jR12);
  MovRI(jSI, I);
  CallAbs(JitSlow);
  AluRR($85, jAX, jAX, False);
  JccThrow(ccS);
  JccPC(ccNE, Target);
end;

function JitICGetHit(O: PObj; IC: PPropIC; Dst: PValue): Boolean;
begin
  Result := ICGetHit(O, IC, Dst);
end;

var
  JitInfo: POptInfo;   { the template being compiled's optimizing tier state (nil: none) }

{ the optimizing tier's counter (a function entry, a loop's back edge to PC): when it runs out, the exit to PC, where
  XJitRun optimizes the region }
procedure JitCountDown(PC: Integer);
begin
  MovRI(jAX, QWord(PtrUInt(@JitInfo^.Count)));
  JB($FF); JB($08);                                { dec dword [rax] }
  JB($0F); JB($84); AddFix(-1 - PC);               { jz: the exit at PC }
end;

{ a cold block of Kind (JitDefer) for instruction PC (I), entered by the forward jump At; its helper is JitSlow }
procedure DeferCold(Kind, PC: Integer; I: QWord; A, V: Integer; const At: array of Integer);
var K: Integer;
begin
  if JitDeferCount >= Length(JitDefer) then SetLength(JitDefer, JitDeferCount * 2 + 64);
  JitDefer[JitDeferCount].PC := PC;
  JitDefer[JitDeferCount].I := I;
  JitDefer[JitDeferCount].H := JitSlow;
  JitDefer[JitDeferCount].Jump := -1;
  JitDefer[JitDeferCount].First := JitDeferJumpCount;
  JitDefer[JitDeferCount].Count := Length(At);
  JitDefer[JitDeferCount].Kind := Kind;
  JitDefer[JitDeferCount].A := A;
  JitDefer[JitDeferCount].V := V;
  Inc(JitDeferCount);
  if JitDeferJumpCount + Length(At) > Length(JitDeferJumps) then
    SetLength(JitDeferJumps, (JitDeferJumpCount + Length(At)) * 2 + 256);
  for K := 0 to High(At) do JitDeferJumps[JitDeferJumpCount + K] := At[K];
  Inc(JitDeferJumpCount, Length(At));
end;

{ the forward jump At (its rel32 already emitted) to PC's code }
procedure JmpFixTo(At, PC: Integer);
begin
  if JitFixCount >= Length(JitFixes) then SetLength(JitFixes, JitFixCount * 2 + 256);
  JitFixes[JitFixCount].At := At;
  JitFixes[JitFixCount].Target := PC;
  Inc(JitFixCount);
end;

{ the code of instruction PC (I), or its exit when the JIT does not take it }
procedure JitInstr(T: PFuncTemplate; PC: Integer; I: QWord); forward;

procedure JitInstr0(T: PFuncTemplate; PC: Integer; I: QWord);
var Saved: Pointer; A, B, C: Integer; CU: Cardinal; Op: TOp; L1, L2, L3, L4, L5, L6, LS, LA: Integer; IC: PPropIC; CC: Integer;
begin
  Op := TOp(I and $FF);
  A := (I shr 8) and $FFFF;
  B := (I shr 24) and $FFFF;
  C := Integer(SarInt64(Int64(I), 40));
  CU := I shr 40;
  case Op of
    opNop: ;
    opMov:
      begin
        MovRM(jAX, jBX, B * 8);
        MovMR(jBX, A * 8, jAX);
      end;
    opLoadI: begin MovRI(jAX, TAG_INT or QWord(UInt32(C))); MovMR(jBX, A * 8, jAX); end;
    opLoadUndef: begin MovRI(jAX, VUndefined); MovMR(jBX, A * 8, jAX); end;
    opLoadNull: begin MovRI(jAX, VNull); MovMR(jBX, A * 8, jAX); end;
    opLoadTrue: begin MovRI(jAX, VTrue); MovMR(jBX, A * 8, jAX); end;
    opLoadFalse: begin MovRI(jAX, VFalse); MovMR(jBX, A * 8, jAX); end;
    opLoadHole: begin MovRI(jAX, VHole); MovMR(jBX, A * 8, jAX); end;
    opLoadK:
      begin
        MovRI(jCX, QWord(PtrUInt(@T^.Consts[CU])));
        MovRM(jAX, jCX, 0);
        MovMR(jBX, A * 8, jAX);
      end;
    opCheckHole:
      begin
        MovRM(jAX, jBX, A * 8);
        MovRI(jCX, VHole);
        AluRR($39, jAX, jCX, True);
        JccExit(ccE, PC);
      end;
    opGetUpv:
      begin
        { Fr^.Closure's upvalue array (Aux2), its cell B, then the register it is open on or its value }
        MovRM(jAX, jR12, PtrInt(@PFrame(nil)^.Closure));
        MovRI(jDX, PTR_MASK);
        AluRR($21, jAX, jDX, True);
        MovRM(jAX, jAX, PtrInt(@PObj(nil)^.Aux2));
        AluRR($21, jAX, jDX, True);
        MovRM(jAX, jAX, PtrInt(@PValueArray(nil)^.Data) + B * 8);
        AluRR($21, jAX, jDX, True);
        MovRM(jCX, jAX, PtrInt(@PUpval(nil)^.StackIndex));
        AluRR($85, jCX, jCX, True);
        L1 := JccFwd(ccS);
        MovRFs(jDX, JitFsVStack);
        MovRMIdx(jAX, jDX, jCX, 0);
        L2 := JmpFwd;
        PatchHere(L1);
        MovRM(jAX, jAX, PtrInt(@PUpval(nil)^.Value));
        PatchHere(L2);
        MovMR(jBX, A * 8, jAX);
      end;
    opJmp:
      begin
        if (C < 0) and (JitInfo <> nil) then JitCountDown(PC + 1 + C);
        JmpPC(PC + 1 + C);
      end;
    opJT, opJF:
      begin
        { true, false, undefined, null and ints decided here; anything else is the interpreter's ToBool }
        MovRM(jAX, jBX, A * 8);
        MovRI(jCX, VTrue);
        AluRR($39, jAX, jCX, True);
        if Op = opJT then JccPC(ccE, PC + 1 + C) else L1 := JccFwd(ccE);
        MovRI(jCX, VFalse);
        AluRR($39, jAX, jCX, True);
        if Op = opJF then JccPC(ccE, PC + 1 + C) else L1 := JccFwd(ccE);
        MovRI(jCX, VUndefined);
        AluRR($39, jAX, jCX, True);
        if Op = opJF then JccPC(ccE, PC + 1 + C) else L2 := JccFwd(ccE);
        MovRI(jCX, VNull);
        AluRR($39, jAX, jCX, True);
        if Op = opJF then JccPC(ccE, PC + 1 + C) else L3 := JccFwd(ccE);
        TagOf(jDX, jAX);
        AluRI(7, jDX, TAG_INT_HI, False);
        L4 := JccFwd(ccE);
        { an object (not [[IsHTMLDDA]]) is true, a string by its length }
        AluRI(7, jDX, TAG_PTR_HI, False);
        L5 := JccFwd(ccNE);
        MovRR(jCX, jAX);
        AluRR($31, jCX, jR13, True);
        CmpBMI(jCX, 4, ckObject);
        L6 := JccFwd(ccNE);
        TestBMI(jCX, 5, cfEmulatesUndefined);
        LS := JccFwd(ccNE);
        if Op = opJT then JmpPC(PC + 1 + C) else LA := JmpFwd;
        PatchHere(L6);
        CmpBMI(jCX, 4, ckString);
        L6 := JccFwd(ccNE);
        Mov32RM(jDX, jCX, PtrInt(@PStr(nil)^.Len));
        AluRR($85, jDX, jDX, False);
        if Op = opJT then JccPC(ccNE, PC + 1 + C) else JccPC(ccE, PC + 1 + C);
        CC := JmpFwd;
        { anything else: ToBool (it calls nothing) }
        PatchHere(L5); PatchHere(L6); PatchHere(LS);
        MovRR(jDI, jAX);
        CallAbs(@JH_ToBool);
        JB($84); JB($C0);                        { test al, al }
        if Op = opJT then JccPC(ccNE, PC + 1 + C) else JccPC(ccE, PC + 1 + C);
        L5 := JmpFwd;
        PatchHere(L4);
        AluRR($85, jAX, jAX, False);
        if Op = opJT then JccPC(ccNE, PC + 1 + C) else JccPC(ccE, PC + 1 + C);
        PatchHere(CC);
        PatchHere(L5);
        if Op = opJF then PatchHere(LA);
        { JT: the falsy values fall through here; JF: true does }
        PatchHere(L1);
        if Op = opJT then begin PatchHere(L2); PatchHere(L3); end;
      end;
    opAddI, opSubI, opIncI:
      begin
        MovRM(jAX, jBX, B * 8);
        L1 := IfNotInt(jAX);
        AluRI(0, jAX, C, False);
        JccExit(ccO, PC);
        StoreInt(A);
        L2 := JmpFwd;
        PatchHere(L1);
        MovRI(jR8, DOUBLE_LIMIT);
        AluRR($39, jAX, jR8, True);
        JccExit(ccAE, PC);
        MovqXR(0, jAX);
        MovRI(jCX, QWord(UInt32(C)));
        Cvt32ToX(1, jCX);
        SseRR($F2, $58, 0, 1);
        StoreArith(A);
        PatchHere(L2);
      end;
    opAdd, opSub, opMul:
      begin
        MovRM(jAX, jBX, B * 8);
        MovRM(jCX, jBX, CU * 8);
        L1 := IfNotInt(jAX);
        TagOf(jDX, jCX);
        AluRI(7, jDX, TAG_INT_HI, False);
        L2 := JccFwd(ccNE);
        case Op of
          opAdd: AluRR($01, jAX, jCX, False);
          opSub: AluRR($29, jAX, jCX, False);
        else
          begin
            MovRR(jSI, jAX);
            Imul32(jAX, jCX);
          end;
        end;
        JccExit(ccO, PC);
        if Op = opMul then
        begin
          { a zero product of a negative operand is -0: the interpreter's }
          AluRR($85, jAX, jAX, False);
          L3 := JccFwd(ccNE);
          AluRR($09, jSI, jCX, False);
          JccExit(ccS, PC);
          PatchHere(L3);
        end;
        StoreInt(A);
        L4 := JmpFwd;
        PatchHere(L1);
        PatchHere(L2);
        ToXmm(0, jAX, PC);
        ToXmm(1, jCX, PC);
        case Op of
          opAdd: SseRR($F2, $58, 0, 1);
          opSub: SseRR($F2, $5C, 0, 1);
        else SseRR($F2, $59, 0, 1);
        end;
        StoreArith(A);
        PatchHere(L4);
      end;
    opBitAndI, opBitOrI, opBitXorI, opShlI, opShrI:
      begin
        MovRM(jAX, jBX, B * 8);
        ToInt32Of(jAX, PC);
        case Op of
          opBitAndI: AluRI(4, jAX, C, False);
          opBitOrI: AluRI(1, jAX, C, False);
          opBitXorI: AluRI(6, jAX, C, False);
          opShlI: ShiftRI(4, jAX, C and 31, False);
        else ShiftRI(7, jAX, C and 31, False);
        end;
        StoreInt(A);
      end;
    opJNLt, opJNLe, opJNGt, opJNGe:
      begin
        MovRM(jAX, jBX, A * 8);
        MovRM(jCX, jBX, B * 8);
        L1 := IfNotInt(jAX);
        TagOf(jDX, jCX);
        AluRI(7, jDX, TAG_INT_HI, False);
        L2 := JccFwd(ccNE);
        AluRR($39, jAX, jCX, False);
        case Op of
          opJNLt: CC := ccGE;
          opJNLe: CC := ccG;
          opJNGt: CC := ccLE;
        else CC := ccL;
        end;
        JccPC(CC, PC + 1 + C);
        L3 := JmpFwd;
        PatchHere(L1);
        PatchHere(L2);
        ToXmm(0, jAX, PC);
        ToXmm(1, jCX, PC);
        { a < b is b above a (unordered sets CF: false), a <= b is b above or equal; the jump is the negation }
        case Op of
          opJNLt: begin SseRR($66, $2E, 1, 0); JccPC(ccBE, PC + 1 + C); end;
          opJNLe: begin SseRR($66, $2E, 1, 0); JccPC(ccB, PC + 1 + C); end;
          opJNGt: begin SseRR($66, $2E, 0, 1); JccPC(ccBE, PC + 1 + C); end;
        else begin SseRR($66, $2E, 0, 1); JccPC(ccB, PC + 1 + C); end;
        end;
        PatchHere(L3);
      end;
    opJNSEq, opJSEq:
      begin
        { equal bits: equal, but for a NaN; different bits: two pointers (strings, BigInts) or a double are the
          interpreter's, anything else differs }
        MovRM(jAX, jBX, A * 8);
        MovRM(jCX, jBX, B * 8);
        AluRR($39, jAX, jCX, True);
        L1 := JccFwd(ccNE);
        MovRI(jR8, DOUBLE_LIMIT);
        AluRR($39, jAX, jR8, True);
        L2 := JccFwd(ccAE);
        MovqXR(0, jAX);
        SseRR($66, $2E, 0, 0);
        L3 := JccFwd(ccP);
        PatchHere(L2);
        { equal }
        if Op = opJSEq then JmpPC(PC + 1 + C) else L4 := JmpFwd;
        { different bits }
        PatchHere(L1);
        TagOf(jDX, jAX);
        AluRI(7, jDX, TAG_PTR_HI, False);
        L2 := JccFwd(ccNE);
        TagOf(jDX, jCX);
        AluRI(7, jDX, TAG_PTR_HI, False);
        LS := JccFwd(ccNE);
        EmitPtrPairEq(PC);
        AluRR($85, jDX, jDX, False);
        { JSEq: equal jumps, not equal goes on; JNSEq: not equal jumps, equal goes on }
        if Op = opJSEq then JccPC(ccNE, PC + 1 + C) else JccPC(ccE, PC + 1 + C);
        L6 := JmpFwd;
        PatchHere(LS);
        PatchHere(L2);
        MovRI(jR8, DOUBLE_LIMIT);
        AluRR($39, jAX, jR8, True);
        JccExit(ccB, PC);
        AluRR($39, jCX, jR8, True);
        JccExit(ccB, PC);
        { not equal }
        PatchHere(L3);
        if Op = opJNSEq then
        begin
          JmpPC(PC + 1 + C);
          PatchHere(L4);
        end;
        PatchHere(L6);
      end;
    opGetElem:
      begin
        MovRM(jCX, jBX, CU * 8);
        TagOf(jDX, jCX);
        AluRI(7, jDX, TAG_INT_HI, False);
        JccExit(ccNE, PC);
        MovRM(jAX, jBX, B * 8);
        UntagPtr(jAX, PC);
        MovzxBRM(jDX, jAX, 4);
        AluRI(7, jDX, ckObject, False);
        JccExit(ccNE, PC);
        MovRM(jDX, jAX, PtrInt(@PObj(nil)^.Elements));
        AluRR($85, jDX, jDX, True);
        L1 := JccFwd(ccE);
        Cmp32RM(jCX, jDX, PtrInt(@PValueArray(nil)^.Len));
        JccExit(ccAE, PC);
        JRex(False, jCX, 0, jCX); JB($8B); JModRR(jCX, jCX);   { mov ecx, ecx: zero-extended index }
        MovRMIdx(jAX, jDX, jCX, PtrInt(@PValueArray(nil)^.Data));
        MovRI(jCX, VHole);
        AluRR($39, jAX, jCX, True);
        JccExit(ccE, PC);
        MovMR(jBX, A * 8, jAX);
        { no elements: a typed array's, cold }
        DeferCold(2, PC, I, A, 0, [L1]);
      end;
    opSetElem:
      begin
        MovRM(jCX, jBX, B * 8);
        TagOf(jDX, jCX);
        AluRI(7, jDX, TAG_INT_HI, False);
        JccExit(ccNE, PC);
        MovRM(jAX, jBX, A * 8);
        UntagPtr(jAX, PC);
        MovzxBRM(jDX, jAX, 4);
        AluRI(7, jDX, ckObject, False);
        JccExit(ccNE, PC);
        { no elements: a typed array's (its OFlags hold the element kind, not the frozen bit) }
        MovRM(jSI, jAX, PtrInt(@PObj(nil)^.Elements));
        AluRR($85, jSI, jSI, True);
        LA := JccFwd(ccE);
        MovzxBRM(jDX, jAX, PtrInt(@PObj(nil)^.OFlags));
        AluRI(4, jDX, 64, False);
        JccExit(ccNE, PC);
        Cmp32RM(jCX, jSI, PtrInt(@PValueArray(nil)^.Len));
        JccExit(ccAE, PC);
        JRex(False, jCX, 0, jCX); JB($8B); JModRR(jCX, jCX);
        MovRMIdx(jAX, jSI, jCX, PtrInt(@PValueArray(nil)^.Data));
        MovRI(jDX, VHole);
        AluRR($39, jAX, jDX, True);
        JccExit(ccE, PC);
        MovRM(jAX, jBX, CU * 8);
        MovMRIdx(jSI, jCX, PtrInt(@PValueArray(nil)^.Data), jAX);
        Barrier(jSI, jAX);
        DeferCold(3, PC, I, 0, CU, [LA]);
      end;
    opGetProp, opGetLength:
      begin
        IC := @T^.ICs[CU];
        MovRM(jAX, jBX, B * 8);
        UntagPtr(jAX, PC);
        MovzxBRM(jDX, jAX, 4);
        LS := -1; LA := -1;
        if Op = opGetLength then
        begin
          { a string's length, an array's without a length past its elements }
          AluRI(7, jDX, ckString, False);
          L1 := JccFwd(ccNE);
          Mov32RM(jAX, jAX, PtrInt(@PStr(nil)^.Len));
          StoreInt(A);
          LS := JmpFwd;
          PatchHere(L1);
          AluRI(7, jDX, ckObject, False);
          JccExit(ccNE, PC);
          MovzxBRM(jDX, jAX, PtrInt(@PObj(nil)^.OKind));
          AluRI(7, jDX, okArray, False);
          L3 := JccFwd(ccNE);
          MovzxBRM(jDX, jAX, PtrInt(@PObj(nil)^.OFlags));
          AluRI(4, jDX, ofBigLen, False);
          L4 := JccFwd(ccNE);
          MovRM(jAX, jAX, PtrInt(@PObj(nil)^.Elements));
          Mov32RM(jAX, jAX, PtrInt(@PValueArray(nil)^.Len));
          StoreInt(A);
          LA := JmpFwd;
          PatchHere(L3); PatchHere(L4);
        end
        else
        begin
          CmpBMI(jAX, 4, ckObject);
          JccExit(ccNE, PC);
        end;
        { IC^.Poly0: the receiver's shape, its kind (Kind, Depth, VerMask and Acc as one 32-bit word: an own
          data property), an inline slot }
        LeaRM(jSI, jR14, PtrInt(PtrUInt(IC) - PtrUInt(@T^.ICs[0])));
        MovRM(jDX, jSI, PtrInt(@PPropIC(nil)^.Poly0.Shape));
        CmpRM(jDX, jAX, PtrInt(@PObj(nil)^.Shape));
        L3 := JccFwd(ccNE);
        MovzxBRM(jDX, jAX, PtrInt(@PObj(nil)^.OKind));
        Cmp32RM(jDX, jSI, PtrInt(@PPropIC(nil)^.Poly0.Kind));
        L4 := JccFwd(ccNE);
        Mov32RM(jDX, jSI, PtrInt(@PPropIC(nil)^.Poly0.Slot));
        MovzxWRM(jCX, jAX, PtrInt(@PObj(nil)^.InlineCap));
        AluRR($39, jDX, jCX, False);
        CC := JccFwd(ccAE);
        MovRMIdx(jAX, jAX, jDX, PtrInt(@PObj(nil)^.Slots));
        MovMR(jBX, A * 8, jAX);
        { the rest of the cache (ICGetHit): cold }
        DeferCold(1, PC, I, A, 0, [L3, L4, CC]);
        if LS >= 0 then PatchHere(LS);
        if LA >= 0 then PatchHere(LA);
      end;
    opSetProp:
      begin
        { a cached store into an inline slot of the cached shape (an add transition included) }
        IC := @T^.ICs[CU];
        MovRM(jAX, jBX, A * 8);
        UntagPtr(jAX, PC);
        MovzxBRM(jDX, jAX, 4);
        AluRI(7, jDX, ckObject, False);
        JccExit(ccNE, PC);
        LeaRM(jSI, jR14, PtrInt(PtrUInt(IC) - PtrUInt(@T^.ICs[0])));
        MovRM(jDX, jSI, PtrInt(@PPropIC(nil)^.Shape));
        CmpRM(jDX, jAX, PtrInt(@PObj(nil)^.Shape));
        JccExit(ccNE, PC);
        Mov32RM(jDX, jSI, PtrInt(@PPropIC(nil)^.Slot));
        MovzxWRM(jCX, jAX, PtrInt(@PObj(nil)^.InlineCap));
        AluRR($39, jDX, jCX, False);
        JccExit(ccAE, PC);
        MovRM(jCX, jSI, PtrInt(@PPropIC(nil)^.NewShape));
        AluRR($85, jCX, jCX, True);
        L1 := JccFwd(ccE);
        MovMR(jAX, PtrInt(@PObj(nil)^.Shape), jCX);
        PatchHere(L1);
        MovRM(jCX, jBX, B * 8);
        MovMRIdx(jAX, jDX, PtrInt(@PObj(nil)^.Slots), jCX);
        Barrier(jAX, jCX);
      end;
    opDefField:
      begin
        { a class field's cached add transition: an extensible object with the cached shape and an inline slot }
        IC := @T^.ICs[CU];
        MovRM(jAX, jBX, A * 8);
        UntagPtr(jAX, PC);
        CmpBMI(jAX, 4, ckObject);
        JccExit(ccNE, PC);
        LeaRM(jSI, jR14, PtrInt(PtrUInt(IC) - PtrUInt(@T^.ICs[0])));
        MovRM(jDX, jSI, PtrInt(@PPropIC(nil)^.Shape));
        CmpRM(jDX, jAX, PtrInt(@PObj(nil)^.Shape));
        JccExit(ccNE, PC);
        MovRM(jCX, jSI, PtrInt(@PPropIC(nil)^.NewShape));
        AluRR($85, jCX, jCX, True);
        JccExit(ccE, PC);
        TestBMI(jAX, PtrInt(@PObj(nil)^.OFlags), 8);
        JccExit(ccNE, PC);
        Mov32RM(jDX, jSI, PtrInt(@PPropIC(nil)^.Slot));
        MovzxWRM(jR8, jAX, PtrInt(@PObj(nil)^.InlineCap));
        AluRR($39, jDX, jR8, False);
        JccExit(ccAE, PC);
        MovMR(jAX, PtrInt(@PObj(nil)^.Shape), jCX);
        MovRM(jCX, jBX, B * 8);
        MovMRIdx(jAX, jDX, PtrInt(@PObj(nil)^.Slots), jCX);
        Barrier(jAX, jCX);
      end;
    opDefFieldC: EmitHelper(@JH_DefField, PC, I);
    opSuperCall, opSuperCallV: EmitHelper(@JH_SuperCall, PC, I);
    opLoadCallee:
      begin
        MovRM(jAX, jR12, PtrInt(@PFrame(nil)^.Closure));
        MovMR(jBX, A * 8, jAX);
      end;
    opCheckThis:
      begin
        { R[A] the hole: the ReferenceError (cold) }
        MovRM(jAX, jBX, A * 8);
        MovRI(jCX, VHole);
        AluRR($39, jAX, jCX, True);
        L1 := JccFwd(ccNE);
        EmitHelper(@JH_CheckThis, PC, I);
        PatchHere(L1);
      end;
    opCall:
      begin
        { JitCallStub with ecx A, edx Argc and the this flag (bit 16): 0 goes on, 1 is the interpreter's (PC
          again), -1 a throw }
        Mov32MI(jR12, PtrInt(@PFrame(nil)^.PC), Cardinal(PC + 1));
        MovRI(jCX, Cardinal(A));
        MovRI(jDX, Cardinal(B) or ((CU and 1) shl 16));
        CallAbs(JitCallStub);
        AluRR($85, jAX, jAX, False);
        L1 := JccFwd(ccE);
        AluRI(7, jAX, 1, False);
        JccExit(ccE, PC);
        JmpEpilogue;
        PatchHere(L1);
      end;
    opGetMethod:
      begin
        { this := the receiver (R[A + 1]), then opGetProp's code on it }
        MovRM(jAX, jBX, B * 8);
        MovMR(jBX, (A + 1) * 8, jAX);
        JitInstr(T, PC, (I and $FFFFFF0000FFFF00) or (QWord(A + 1) shl 24) or QWord(Ord(opGetProp)));
      end;
    opSEq, opSNe, opEq, opNe:
      begin
        { equal bits: equal, but for a NaN; different bits: two ints or a pointer against a non-pointer differ
          (strictly), the nullish pair is loosely equal; two pointers, a double, or a loose pair needing
          conversion are JH_Compare's }
        MovRM(jAX, jBX, B * 8);
        MovRM(jCX, jBX, CU * 8);
        AluRR($39, jAX, jCX, True);
        L1 := JccFwd(ccNE);
        MovRI(jR8, DOUBLE_LIMIT);
        AluRR($39, jAX, jR8, True);
        L2 := JccFwd(ccAE);
        MovqXR(0, jAX);
        SseRR($66, $2E, 0, 0);
        L3 := JccFwd(ccP);
        PatchHere(L2);
        { equal }
        if (Op = opSEq) or (Op = opEq) then MovRI(jAX, VTrue) else MovRI(jAX, VFalse);
        L4 := JmpFwd;
        PatchHere(L1);
        L5 := -1;
        L6 := -1;
        if (Op = opSEq) or (Op = opSNe) then
        begin
          TagOf(jDX, jAX);
          AluRI(7, jDX, TAG_PTR_HI, False);
          L2 := JccFwd(ccNE);
          TagOf(jDX, jCX);
          AluRI(7, jDX, TAG_PTR_HI, False);
          LS := JccFwd(ccNE);
          EmitPtrPairEq(PC);
          AluRR($85, jDX, jDX, False);
          L5 := JccFwd(ccE);
          if Op = opSEq then MovRI(jAX, VTrue) else MovRI(jAX, VFalse);
          L6 := JmpFwd;
          PatchHere(LS);
          PatchHere(L2);
          MovRI(jR8, DOUBLE_LIMIT);
          AluRR($39, jAX, jR8, True);
          JccExit(ccB, PC);
          AluRR($39, jCX, jR8, True);
          JccExit(ccB, PC);
        end
        else
        begin
          { a nullish: equal to the other nullish; a pointer (an [[IsHTMLDDA]] object) is the helper's; else not }
          MovRI(jR8, VNull);
          MovRR(jDX, jAX);
          AluRI(1, jDX, 1, True);
          AluRR($39, jDX, jR8, True);
          L2 := JccFwd(ccNE);
          MovRR(jDX, jCX);
          AluRI(1, jDX, 1, True);
          AluRR($39, jDX, jR8, True);
          LS := JccFwd(ccE);
          TagOf(jDX, jCX);
          AluRI(7, jDX, TAG_PTR_HI, False);
          JccExit(ccE, PC);
          LA := JmpFwd;
          PatchHere(L2);
          MovRR(jDX, jCX);
          AluRI(1, jDX, 1, True);
          AluRR($39, jDX, jR8, True);
          L2 := JccFwd(ccNE);
          TagOf(jDX, jAX);
          AluRI(7, jDX, TAG_PTR_HI, False);
          JccExit(ccE, PC);
          L1 := JmpFwd;
          PatchHere(L2);
          { two ints differ; anything else is the helper's }
          TagOf(jDX, jAX);
          AluRI(7, jDX, TAG_INT_HI, False);
          JccExit(ccNE, PC);
          TagOf(jDX, jCX);
          AluRI(7, jDX, TAG_INT_HI, False);
          JccExit(ccNE, PC);
          PatchHere(L1);
          PatchHere(LA);
          L1 := JmpFwd;
          PatchHere(LS);
          if Op = opEq then MovRI(jAX, VTrue) else MovRI(jAX, VFalse);
          L2 := JmpFwd;
          PatchHere(L1);
          PatchHere(L3);
          if Op = opEq then MovRI(jAX, VFalse) else MovRI(jAX, VTrue);
          PatchHere(L2);
          PatchHere(L4);
          MovMR(jBX, A * 8, jAX);
          Exit;
        end;
        { not equal }
        PatchHere(L3);
        if L5 >= 0 then PatchHere(L5);
        if Op = opSEq then MovRI(jAX, VFalse) else MovRI(jAX, VTrue);
        PatchHere(L4);
        if L6 >= 0 then PatchHere(L6);
        MovMR(jBX, A * 8, jAX);
      end;
    opLt, opLe, opGt, opGe:
      begin
        MovRM(jAX, jBX, B * 8);
        MovRM(jCX, jBX, CU * 8);
        L1 := IfNotInt(jAX);
        TagOf(jDX, jCX);
        AluRI(7, jDX, TAG_INT_HI, False);
        L2 := JccFwd(ccNE);
        AluRR($39, jAX, jCX, False);
        case Op of
          opLt: CC := ccL;
          opLe: CC := ccLE;
          opGt: CC := ccG;
        else CC := ccGE;
        end;
        L3 := JccFwd(CC);
        L4 := JmpFwd;
        PatchHere(L1);
        PatchHere(L2);
        ToXmm(0, jAX, PC);
        ToXmm(1, jCX, PC);
        { a < b is b above a (unordered clears neither CF nor ZF: false), a <= b is b above or equal }
        case Op of
          opLt: begin SseRR($66, $2E, 1, 0); CC := ccA; end;
          opLe: begin SseRR($66, $2E, 1, 0); CC := ccAE; end;
          opGt: begin SseRR($66, $2E, 0, 1); CC := ccA; end;
        else begin SseRR($66, $2E, 0, 1); CC := ccAE; end;
        end;
        L1 := JccFwd(CC);
        PatchHere(L4);
        MovRI(jAX, VFalse);
        L2 := JmpFwd;
        PatchHere(L1);
        PatchHere(L3);
        MovRI(jAX, VTrue);
        PatchHere(L2);
        MovMR(jBX, A * 8, jAX);
      end;
    opToNum, opToNumeric:
      begin
        { a number as it is; anything else JH_Arith's }
        MovRM(jAX, jBX, B * 8);
        TagOf(jDX, jAX);
        AluRI(7, jDX, TAG_INT_HI, False);
        L1 := JccFwd(ccE);
        MovRI(jCX, DOUBLE_LIMIT);
        AluRR($39, jAX, jCX, True);
        JccExit(ccAE, PC);
        PatchHere(L1);
        MovMR(jBX, A * 8, jAX);
      end;
    opNeg, opBitNot:
      begin
        { an int (Neg: not 0, which gives -0, nor the int32 minimum); anything else JH_Arith's }
        MovRM(jAX, jBX, B * 8);
        TagOf(jDX, jAX);
        AluRI(7, jDX, TAG_INT_HI, False);
        JccExit(ccNE, PC);
        if Op = opNeg then
        begin
          AluRR($85, jAX, jAX, False);
          JccExit(ccE, PC);
          JB($F7); JB($D8);                     { neg eax }
          JccExit(ccO, PC);
        end
        else begin JB($F7); JB($D0); end;       { not eax }
        JRex(False, jAX, 0, jAX); JB($8B); JModRR(jAX, jAX);   { mov eax, eax: the upper half clear }
        StoreInt(A);
      end;
    opShr, opUShr, opUShrI:
      begin
        { two ints (>>> one whose result is an int32); anything else JH_Arith's }
        MovRM(jAX, jBX, B * 8);
        TagOf(jDX, jAX);
        AluRI(7, jDX, TAG_INT_HI, False);
        JccExit(ccNE, PC);
        if Op <> opUShrI then
        begin
          MovRM(jCX, jBX, CU * 8);
          TagOf(jDX, jCX);
          AluRI(7, jDX, TAG_INT_HI, False);
          JccExit(ccNE, PC);
          JB($D3);
          if Op = opShr then JB($F8) else JB($E8);    { sar / shr eax, cl }
        end
        else ShiftRI(5, jAX, C and 31, False);
        if Op <> opShr then
        begin
          AluRR($85, jAX, jAX, False);
          JccExit(ccS, PC);
        end;
        JRex(False, jAX, 0, jAX); JB($8B); JModRR(jAX, jAX);
        StoreInt(A);
      end;
    opPow:
      EmitHelper(@JH_Arith, PC, I);
    opInstanceof: EmitHelper(@JH_InstanceOf, PC, I);
    opGetIter, opForIn, opIn, opNewArr, opArrPush, opArrHole, opClose, opToStr, opRegExp, opGetPriv, opSetPriv, opPrivIn,
    opSetUpv:
      EmitHelper(@JH_Misc, PC, I);
    opBitAnd, opBitOr, opBitXor, opShl:
      begin
        MovRM(jAX, jBX, B * 8);
        ToInt32Of(jAX, PC);
        MovRR(jR9, jAX);
        MovRM(jCX, jBX, CU * 8);
        ToInt32Of(jCX, PC);
        MovRR(jAX, jR9);
        case Op of
          opBitAnd: AluRR($21, jAX, jCX, False);
          opBitOr: AluRR($09, jAX, jCX, False);
          opBitXor: AluRR($31, jAX, jCX, False);
        else begin JB($D3); JB($E0); end;                  { shl eax, cl }
        end;
        JRex(False, jAX, 0, jAX); JB($8B); JModRR(jAX, jAX); { mov eax, eax }
        StoreInt(A);
      end;
    opDiv:
      begin
        MovRM(jAX, jBX, B * 8);
        MovRM(jCX, jBX, CU * 8);
        ToXmm(0, jAX, PC);
        ToXmm(1, jCX, PC);
        SseRR($F2, $5E, 0, 1);                              { divsd }
        StoreNumber(A);
      end;
    opMod:
      begin
        { a non-negative int by a positive one; the rest JH_Arith's }
        MovRM(jAX, jBX, B * 8);
        MovRM(jCX, jBX, CU * 8);
        TagOf(jDX, jAX);
        AluRI(7, jDX, TAG_INT_HI, False);
        JccExit(ccNE, PC);
        TagOf(jDX, jCX);
        AluRI(7, jDX, TAG_INT_HI, False);
        JccExit(ccNE, PC);
        AluRR($85, jAX, jAX, False);
        JccExit(ccS, PC);
        AluRR($85, jCX, jCX, False);
        JccExit(ccLE, PC);
        AluRR($31, jDX, jDX, False);                        { xor edx, edx }
        JB($F7); JB($F1);                                   { div ecx }
        JRex(False, jAX, 0, jDX); JB($8B); JModRR(jAX, jDX);  { mov eax, edx }
        StoreInt(A);
      end;
    opJUndef, opJNotUndef, opJNullish, opJNotNullish:
      begin
        MovRM(jAX, jBX, A * 8);
        MovRI(jCX, VUndefined);
        AluRR($39, jAX, jCX, True);
        case Op of
          opJUndef: JccPC(ccE, PC + 1 + C);
          opJNotUndef: JccPC(ccNE, PC + 1 + C);
          opJNullish:
            begin
              JccPC(ccE, PC + 1 + C);
              MovRI(jCX, VNull);
              AluRR($39, jAX, jCX, True);
              JccPC(ccE, PC + 1 + C);
            end;
        else
          begin
            L1 := JccFwd(ccE);
            MovRI(jCX, VNull);
            AluRR($39, jAX, jCX, True);
            JccPC(ccNE, PC + 1 + C);
            PatchHere(L1);
          end;
        end;
      end;
    opGetGlobal, opTypeofGlobal: EmitHelper(@JH_GetGlobal, PC, I);
    opClosure, opNewObj, opDefProp, opNot, opTypeof: EmitHelper(@JH_Alloc, PC, I);
    opIterNext:
      begin
        { an array iterator's step (as FastLoop's): the element in R[A], the index on; a generator, the end of the
          array or a hole are the interpreter's }
        MovRM(jAX, jBX, B * 8);
        UntagPtr(jAX, PC);
        MovzxBRM(jDX, jAX, 4);
        AluRI(7, jDX, ckObject, False);
        JccExit(ccNE, PC);
        MovzxBRM(jDX, jAX, PtrInt(@PObj(nil)^.OKind));
        AluRI(7, jDX, okIterator, False);
        JccExit(ccNE, PC);
        MovzxBRM(jDX, jAX, PtrInt(@PObj(nil)^.OFlags));
        AluRR($85, jDX, jDX, False);
        JccExit(ccNE, PC);
        MovRM(jCX, jBX, (B + 1) * 8);
        MovRFs(jDX, JitFsArrayIterNext);
        AluRR($39, jCX, jDX, True);
        JccExit(ccNE, PC);
        MovRM(jCX, jAX, PtrInt(@PObj(nil)^.Aux1));
        UntagPtr(jCX, PC);
        MovzxBRM(jDX, jCX, 4);
        AluRI(7, jDX, ckObject, False);
        JccExit(ccNE, PC);
        MovzxBRM(jDX, jCX, PtrInt(@PObj(nil)^.OFlags));
        AluRI(4, jDX, 64, False);
        JccExit(ccNE, PC);
        MovzxBRM(jDX, jCX, PtrInt(@PObj(nil)^.OKind));
        AluRI(7, jDX, okArray, False);
        L1 := JccFwd(ccE);
        AluRI(7, jDX, okArguments, False);
        JccExit(ccNE, PC);
        PatchHere(L1);
        MovRM(jSI, jCX, PtrInt(@PObj(nil)^.Elements));
        AluRR($85, jSI, jSI, True);
        JccExit(ccE, PC);
        Mov32RM(jDX, jAX, PtrInt(@PObj(nil)^.Aux2));        { the index (an int) }
        Cmp32RM(jDX, jSI, PtrInt(@PValueArray(nil)^.Len));
        JccExit(ccAE, PC);
        MovRMIdx(jDI, jSI, jDX, PtrInt(@PValueArray(nil)^.Data));
        MovRI(jCX, VHole);
        AluRR($39, jDI, jCX, True);
        JccExit(ccE, PC);
        MovMR(jBX, A * 8, jDI);
        AluRI(0, jDX, 1, False);
        MovRI(jCX, TAG_INT);
        AluRR($09, jDX, jCX, True);
        MovMR(jAX, PtrInt(@PObj(nil)^.Aux2), jDX);
      end;
    opNew:
      begin
        { Fr^.PC past it, then XJitNew: 0 done, 1 the interpreter's, -1 a throw }
        JRex(False, 0, 0, jR12); JB($C7); JMem(0, jR12, PtrInt(@PFrame(nil)^.PC)); JD(Cardinal(PC + 1));
        MovRR(jDI, jR12);
        MovRI(jSI, Cardinal(A));
        MovRI(jDX, Cardinal(B));
        CallAbs(@XJitNew);
        AluRR($85, jAX, jAX, False);
        L1 := JccFwd(ccE);
        AluRI(7, jAX, 1, False);
        JccExit(ccE, PC);
        JccThrow(ccNE);
        PatchHere(L1);
      end;
    opRet, opRetUndef:
      begin
        { a generator's completion is the interpreter's }
        if T^.IsGenerator then
        begin
          JmpExit(PC);
          JitTaken := False;
          Exit;
        end;
        { only in a frame XJitCall pushed (ebp 1): XJitRet, then -2 to it; otherwise the interpreter's }
        JB($85); JB($ED);                       { test ebp, ebp }
        JccExit(ccE, PC);
        if Op = opRet then MovRM(jSI, jBX, A * 8) else MovRI(jSI, VUndefined);
        if T^.IsDerived and T^.IsClassCtor then
        begin
          MovRR(jDI, jR12);
          CallAbs(@XJitRetCtor);
          JB($84); JB($C0);                     { test al, al }
          JccThrow(ccE);
        end
        else if T^.NoCaptures and T^.NoHandlers then
        begin
          { nothing to close, no handler of its own: the return inline (the caller restores StackTop) }
          CmpBMI(jR12, PtrInt(@PFrame(nil)^.Kind), 1);
          L1 := JccFwd(ccNE);
          TagOf(jDX, jSI);
          AluRI(7, jDX, TAG_PTR_HI, False);
          L2 := JccFwd(ccNE);
          MovRR(jCX, jSI);
          AluRR($31, jCX, jR13, True);
          CmpBMI(jCX, 4, ckObject);
          L3 := JccFwd(ccE);
          PatchHere(L2);
          MovRM(jSI, jBX, 0);              { a construction's non-object result: this }
          PatchHere(L3);
          PatchHere(L1);
          Mov32RFs(jCX, JitFsFrameTop);
          AluRI(5, jCX, 1, False);
          Mov32FsR(JitFsFrameTop, jCX);
          MovsxdRM(jDX, jR12, PtrInt(@PFrame(nil)^.RetIdx));
          MovRFs(jAX, JitFsVStack);
          MovMRIdx(jAX, jDX, 0, jSI);
        end
        else
        begin
          MovRR(jDI, jR12);
          CallAbs(@XJitRet);
        end;
        MovRI(jAX, $FFFFFFFE);
        JmpEpilogue;
      end;
  else
    begin
      JmpExit(PC);
      JitTaken := False;
    end;
  end;
end;

procedure JitInstr(T: PFuncTemplate; PC: Integer; I: QWord);
var K: Integer; Saved: Pointer;
begin
  Saved := JitSlow;
  case TOp(I and $FF) of
    opGetProp, opGetLength: JitSlow := @JH_GetProp;
    opSetProp: JitSlow := @JH_SetProp;
    opDefField: JitSlow := @JH_DefField;
    opGetElem: JitSlow := @JH_GetElem;
    opSetElem: JitSlow := @JH_SetElem;
    opAdd, opSub, opMul, opAddI, opSubI, opIncI, opBitAndI, opBitOrI, opBitXorI, opShlI, opShrI, opBitAnd, opBitOr,
    opBitXor, opShl, opDiv, opMod, opToNum, opToNumeric, opNeg, opBitNot, opShr, opUShr, opUShrI:
      JitSlow := @JH_Arith;
    opSEq, opSNe, opEq, opNe, opLt, opLe, opGt, opGe: JitSlow := @JH_Compare;
    opIterNext:
      begin
        JitSlow := @JH_IterNext;
        JitSlowJump := PC + 1 + Integer(SarInt64(Int64(I), 40));
      end;
    opJNLt, opJNLe, opJNGt, opJNGe, opJNSEq, opJSEq:
      begin
        JitSlow := @JH_JumpCmp;
        JitSlowJump := PC + 1 + Integer(SarInt64(Int64(I), 40));
      end;
  else JitSlow := nil;
  end;
  JitSlowCount := 0;
  JitInstr0(T, PC, I);
  if JitSlowCount > 0 then
  begin
    { the failed guards' block (the helper, then on to the next instruction), emitted past the code so the
      fast paths stay together }
    if JitDeferCount >= Length(JitDefer) then SetLength(JitDefer, JitDeferCount * 2 + 64);
    JitDefer[JitDeferCount].PC := PC;
    JitDefer[JitDeferCount].I := I;
    JitDefer[JitDeferCount].H := JitSlow;
    JitDefer[JitDeferCount].Jump := JitSlowJump;
    JitDefer[JitDeferCount].First := JitDeferJumpCount;
    JitDefer[JitDeferCount].Count := JitSlowCount;
    JitDefer[JitDeferCount].Kind := 0;
    Inc(JitDeferCount);
    if JitDeferJumpCount + JitSlowCount > Length(JitDeferJumps) then
      SetLength(JitDeferJumps, (JitDeferJumpCount + JitSlowCount) * 2 + 256);
    for K := 0 to JitSlowCount - 1 do JitDeferJumps[JitDeferJumpCount + K] := JitSlowList[K];
    Inc(JitDeferJumpCount, JitSlowCount);
  end;
  JitSlowCount := 0;
  JitSlowJump := -1;
  JitSlow := Saved;
end;

{ executable memory for N bytes (nil when the system gives none) }
function JitAlloc(N: PtrUInt): PByte;
var P: Pointer;
begin
  N := (N + 15) and not PtrUInt(15);
  if (JitChunk = nil) or (JitChunkUsed + N > JIT_CHUNK) then
  begin
    if N > JIT_CHUNK then Exit(nil);
    {$IFDEF UNIX}
    P := Fpmmap(nil, JIT_CHUNK, PROT_READ or PROT_WRITE or PROT_EXEC, MAP_PRIVATE or MAP_ANONYMOUS, -1, 0);
    if (P = MAP_FAILED) or (P = nil) then Exit(nil);
    {$ELSE}
    Exit(nil);
    {$ENDIF}
    JitChunk := P;
    JitChunkUsed := 0;
  end;
  Result := JitChunk + JitChunkUsed;
  Inc(JitChunkUsed, N);
end;

{ compiles T (JitState 1, Jit and JitTable set) or marks it not compiled (2) }
{ opCall's code, shared: rbx R, r12 the frame (its PC already past the call), ecx A, edx Argc | this flag
  shl 16. A compiled bytecode callee that takes FastCall's frame is called inline: the checks, the registers past
  the arguments undefined, the frame record, then its code. Anything else is XJitCall's. eax: 0 go on, 1 the
  interpreter runs the call, -1 a throw. Keeps rbx, r12, rbp (and r13 - r15, its own) }
procedure JitBuildCallStub;
var Slow: array[0..31] of Integer; NSlow, K, L1, L2, L3, LRet0, LRet1, LRet2, LRet3, LRet4: Integer; P: PByte;
  procedure AddS(L: Integer); begin Slow[NSlow] := L; Inc(NSlow); end;
begin
  JitLen := 0;
  NSlow := 0;
  JB($41); JB($55); JB($41); JB($56); JB($41); JB($57);          { push r13; push r14; push r15: rsp aligned }
  JB($4C); JB($8D); JB($2C); JB($CB);                              { lea r13, [rbx + rcx * 8]: &R[A] }
  MovRR(jR15, jCX);                                                { r15: A }
  MovRR(jR14, jDX);                                                { r14: Argc | flag shl 16 }
  MovRM(jAX, jR13, 0);
  TagOf(jDX, jAX);
  AluRI(7, jDX, TAG_PTR_HI, False);
  AddS(JccFwd(ccNE));
  MovRI(jDX, PTR_MASK);
  AluRR($21, jAX, jDX, True);
  CmpBMI(jAX, 4, ckObject); AddS(JccFwd(ccNE));
  CmpBMI(jAX, PtrInt(@PObj(nil)^.OKind), okFunction); AddS(JccFwd(ccNE));
  TestBMI(jAX, PtrInt(@PObj(nil)^.OFlags), 1); AddS(JccFwd(ccNE));
  MovRM(jSI, jAX, PtrInt(@PObj(nil)^.Native));
  CmpBMI(jSI, PtrInt(@PFuncTemplate(nil)^.FastCall), 0); AddS(JccFwd(ccE));
  CmpBMI(jSI, PtrInt(@PFuncTemplate(nil)^.IsGenerator), 0); AddS(JccFwd(ccNE));
  CmpBMI(jSI, PtrInt(@PFuncTemplate(nil)^.JitState), 1); AddS(JccFwd(ccNE));
  { this: a strict or arrow callee takes it as it is (undefined for the flag); a sloppy one an object }
  CmpBMI(jSI, PtrInt(@PFuncTemplate(nil)^.IsStrict), 0); L1 := JccFwd(ccNE);
  CmpBMI(jSI, PtrInt(@PFuncTemplate(nil)^.IsArrow), 0); L2 := JccFwd(ccNE);
  JB($41); JB($F7); JB($C6); JD($10000);                           { test r14d, $10000 }
  AddS(JccFwd(ccNE));
  MovRM(jCX, jR13, 8);
  TagOf(jDX, jCX);
  AluRI(7, jDX, TAG_PTR_HI, False);
  AddS(JccFwd(ccNE));
  MovRI(jDX, PTR_MASK);
  AluRR($21, jCX, jDX, True);
  CmpBMI(jCX, 4, ckObject); AddS(JccFwd(ccNE));
  PatchHere(L1); PatchHere(L2);
  JB($41); JB($F7); JB($C6); JD($10000);                           { test r14d, $10000 }
  L1 := JccFwd(ccE);
  MovRI(jCX, VUndefined); MovMR(jR13, 8, jCX);
  PatchHere(L1);
  MovRM(jDI, jSI, PtrInt(@PFuncTemplate(nil)^.JitTable));
  MovRM(jDI, jDI, 0);
  AluRR($85, jDI, jDI, True); AddS(JccFwd(ccE));
  MovsxdRM(jCX, jR12, PtrInt(@PFrame(nil)^.Base));
  AluRR($01, jCX, jR15, True);
  AluRI(0, jCX, 1, True);                                          { rcx: the callee's Base }
  MovsxdRM(jDX, jSI, PtrInt(@PFuncTemplate(nil)^.MaxRegs));
  JRex(True, jR8, jDX, jCX); JB($8D); JB($04 or ((jR8 and 7) shl 3)); JB(((jDX and 7) shl 3) or (jCX and 7));  { lea r8, [rcx + rdx] }
  AluRI(7, jR8, STACK_MAX, True); AddS(JccFwd(ccAE));
  Mov32RFs(jR9, JitFsFrameTop);
  AluRI(7, jR9, FRAME_MAX - 2, False); AddS(JccFwd(ccAE));
  Mov32RFs(jR10, JitFsJitDepth);
  Mov32RFs(jR11, JitFsJitDepthMax);
  AluRR($39, jR10, jR11, False); AddS(JccFwd(ccAE));
  MovRFs(jR11, JitFsStackFloor);
  AluRR($39, jSP, jR11, True); AddS(JccFwd(ccB));                 { cmp rsp, the floor }
  { the registers from min(Argc, NumParams) + 1 to MaxRegs - 1: undefined }
  MovRR(jR11, jR14);
  AluRI(4, jR11, $FFFF, False);
  JRex(False, jR11, 0, jSI); JB($3B); JMem(jR11, jSI, PtrInt(@PFuncTemplate(nil)^.NumParams));  { cmp r11d, NumParams }
  L1 := JccFwd(ccLE);
  Mov32RM(jR11, jSI, PtrInt(@PFuncTemplate(nil)^.NumParams));
  PatchHere(L1);
  AluRI(0, jR11, 1, False);
  MovRI(jR10, VUndefined);
  L2 := JitLen;
  AluRR($39, jR11, jDX, True);
  L3 := JccFwd(ccGE);
  MovMRIdx(jR13, jR11, 8, jR10);
  AluRI(0, jR11, 1, True);
  JB($E9); JD(Cardinal(L2 - (JitLen + 4)));
  PatchHere(L3);
  { the frame record Frames[FrameTop] }
  MovRFs(jAX, JitFsFrames);
  ImulRRI(jR10, jR9, SizeOf(TFrame));
  AluRR($01, jAX, jR10, True);
  MovRM(jR10, jR13, 0);
  MovMR(jAX, PtrInt(@PFrame(nil)^.Closure), jR10);
  MovRI(jR10, VUndefined);
  MovMR(jAX, PtrInt(@PFrame(nil)^.NewTarget), jR10);
  MovMR(jAX, PtrInt(@PFrame(nil)^.Tpl), jSI);
  Mov32MR(jAX, PtrInt(@PFrame(nil)^.Base), jCX);
  Mov32MI(jAX, PtrInt(@PFrame(nil)^.PC), 0);
  LeaRM(jR10, jCX, -1);
  Mov32MR(jAX, PtrInt(@PFrame(nil)^.RetIdx), jR10);
  MovRR(jR11, jR14);
  AluRI(4, jR11, $FFFF, False);
  Mov32MR(jAX, PtrInt(@PFrame(nil)^.Argc), jR11);
  Mov8MI(jAX, PtrInt(@PFrame(nil)^.Kind), 0);
  AluRI(0, jR9, 1, False);
  Mov32FsR(JitFsFrameTop, jR9);
  Mov32FsR(JitFsStackTop, jR8);
  Mov32RFs(jR10, JitFsJitDepth);
  AluRI(0, jR10, 1, False);
  Mov32FsR(JitFsJitDepth, jR10);
  { the callee's code: R', its entry, the frame, nested (1) }
  MovRM(jR11, jSI, PtrInt(@PFuncTemplate(nil)^.Jit));
  MovRR(jSI, jDI);
  LeaRM(jDI, jR13, 8);
  MovRR(jDX, jAX);
  MovRI(jCX, 1);
  JRex(False, 0, 0, jR11); JB($FF); JB($D0 or (jR11 and 7));   { call r11 }
  Mov32RFs(jR10, JitFsJitDepth);
  AluRI(5, jR10, 1, False);
  Mov32FsR(JitFsJitDepth, jR10);
  AluRI(7, jAX, -2, False);
  L1 := JccFwd(ccNE);
  { returned: this frame's StackTop (its Base + its template's MaxRegs) }
  MovRM(jR10, jR12, PtrInt(@PFrame(nil)^.Tpl));
  Mov32RM(jCX, jR10, PtrInt(@PFuncTemplate(nil)^.MaxRegs));
  JRex(False, jCX, 0, jR12); JB($03); JMem(jCX, jR12, PtrInt(@PFrame(nil)^.Base));   { add ecx, Base }
  Mov32FsR(JitFsStackTop, jCX);
  AluRR($31, jAX, jAX, False);
  LRet0 := JmpFwd;
  PatchHere(L1);
  { stopped part way or threw: XJitCallPost (0 goes on, else a throw) }
  MovRR(jDI, jR12);
  MovRR(jSI, jR15);
  JRex(False, jDX, 0, jAX); JB($8B); JModRR(jDX, jAX);         { mov edx, eax }
  CallAbs(@XJitCallPost);
  AluRR($85, jAX, jAX, False);
  LRet1 := JccFwd(ccE);
  MovRI(jAX, $FFFFFFFF);
  LRet2 := JmpFwd;
  { the general call: XJitCall (0 goes on, 1 the interpreter's, else a throw) }
  for K := 0 to NSlow - 1 do PatchHere(Slow[K]);
  MovRR(jDI, jR12);
  MovRR(jSI, jR15);
  MovRR(jDX, jR14);
  AluRI(4, jDX, $FFFF, False);
  MovRR(jCX, jR14);
  JB($C1); JB($E9); JB(16);                                        { shr ecx, 16 }
  CallAbs(@XJitCall);
  AluRR($85, jAX, jAX, False);
  LRet3 := JccFwd(ccE);
  AluRI(7, jAX, 1, False);
  LRet4 := JccFwd(ccE);
  MovRI(jAX, $FFFFFFFF);
  PatchHere(LRet0); PatchHere(LRet1); PatchHere(LRet2); PatchHere(LRet3); PatchHere(LRet4);
  JB($41); JB($5F); JB($41); JB($5E); JB($41); JB($5D); JB($C3);  { pop r15; pop r14; pop r13; ret }
  P := JitAlloc(JitLen);
  if P = nil then Exit;
  Move(JitBuf[0], P^, JitLen);
  JitCallStub := P;
end;

{ the slow blocks JitInstr deferred (JitDefer), each back to the instruction after its own }
procedure JitEmitDeferred(T: PFuncTemplate);
var N, K, PC: Integer; Saved: Pointer;
begin
  Saved := JitSlow;
  for N := 0 to JitDeferCount - 1 do
  begin
    for K := JitDefer[N].First to JitDefer[N].First + JitDefer[N].Count - 1 do PatchHere(JitDeferJumps[K]);
    JitSlow := JitDefer[N].H;
    PC := JitDefer[N].PC;
    case JitDefer[N].Kind of
      1:
        begin
          { ICGetHit: its answer in R[A] goes on, else the helper }
          MovRR(jDI, jAX);
          LeaRM(jDX, jBX, JitDefer[N].A * 8);
          CallAbs(@JitICGetHit);
          JB($84); JB($C0);   { test al, al: the Boolean }
          JccPC(ccNE, PC + 1);
        end;
      2, 3:
        begin
          { the typed array's element; its other kinds and failed guards the helper }
          JitSlowCount := 0;
          EmitTAElem(JitDefer[N].Kind = 3, JitDefer[N].A, JitDefer[N].V, PC);
          for K := 0 to JitTAEndCount - 1 do JmpFixTo(JitTAEnds[K], PC + 1);
          JitTAEndCount := 0;
          for K := 0 to JitSlowCount - 1 do PatchHere(JitSlowList[K]);
          JitSlowCount := 0;
        end;
    end;
    if JitDefer[N].Jump >= 0 then EmitJumpHelper(PC, JitDefer[N].I, JitDefer[N].Jump)
    else EmitHelper(JitDefer[N].H, PC, JitDefer[N].I);
    if PC + 1 < T^.CodeLen then JmpPC(PC + 1) else JmpExit(PC);
  end;
  JitSlow := Saved;
  JitDeferCount := 0;
  JitDeferJumpCount := 0;
end;

procedure JitCompile(T: PFuncTemplate);
var PC, I, Exit0, Throw0, Target: Integer; Code: PByte; Table: PPtrUInt; D: Cardinal;
  Info: POptInfo;
begin
  { the intrinsics' set-up (the JS preludes at start-up, a new realm's) runs once: not compiled, and the template
    stays untried for later code }
  if IntrinsicsInit then Exit;
  T^.JitState := 2;
  { a generator's body runs compiled between its suspensions (its yields and returns are the interpreter's) }
  if (T^.Lazy <> nil) or (T^.CodeLen = 0) or T^.IsAsync then Exit;
  if JitCallStub = nil then JitBuildCallStub;
  if JitCallStub = nil then Exit;
  JitLen := 0;
  JitFixCount := 0;
  JitDeferCount := 0;
  JitDeferJumpCount := 0;
  SetLength(JitNative, T^.CodeLen);
  SetLength(JitExitOf, T^.CodeLen);
  for PC := 0 to T^.CodeLen - 1 do JitExitOf[PC] := -1;
  Table := GetMem(T^.CodeLen * SizeOf(PtrUInt));
  Info := nil;
  if (OptEnabled = 1) and T^.NoHandlers and T^.NoCaptures and not T^.IsGenerator and not T^.IsAsync and
     (T^.ArgsReg < 0) and (T^.RestReg < 0) and not T^.TopLevel and OptPromising(T) then
  begin
    New(Info);
    FillChar(Info^, SizeOf(TOptInfo), 0);
    Info^.Count := OPT_HOT;
    SetLength(Info^.Tried, T^.CodeLen + 1);
    SetLength(Info^.Feedback, T^.CodeLen + 1);
  end;
  JitInfo := Info;
  { prologue: the three callee-saved registers the code keeps, then the instruction's code (rsi) }
  { push rbx; push r12; push rbp; push r13; push r14: rsp 16-byte aligned for calls }
  JB($53); JB($41); JB($54); JB($55); JB($41); JB($55); JB($41); JB($56);
  MovRR(jBX, jDI); MovRR(jR12, jDX);
  MovRI(jR13, QWord(TAG_PTR_HI) shl 48);         { r13: the pointer tag }
  if Length(T^.ICs) > 0 then MovRI(jR14, QWord(PtrUInt(@T^.ICs[0])));   { r14: the template's ICs }
  JB($89); JB($CD);                              { mov ebp, ecx }
  JB($FF); JB($E6);                              { jmp rsi }
  SetLength(JitTakenAt, T^.CodeLen);
  for PC := 0 to T^.CodeLen - 1 do
  begin
    JitNative[PC] := JitLen;
    JitTaken := True;
    if (PC = 0) and (Info <> nil) then JitCountDown(0);
    JitInstr(T, PC, T^.Code[PC]);
    {$IFDEF G2_JITSTATS}Inc(JitOpBytes[T^.Code[PC] and $FF], JitLen - JitNative[PC]); Inc(JitOpCount[T^.Code[PC] and $FF]);{$ENDIF}
    JitTakenAt[PC] := JitTaken;
  end;
  { past the end (never reached: the last instruction is a return) }
  JmpExit(T^.CodeLen - 1);
  JitEmitDeferred(T);
  { the exits: eax the PC the interpreter runs; the epilogue }
  Exit0 := JitLen;
  JB($41); JB($5E); JB($41); JB($5D); JB($5D); JB($41); JB($5C); JB($5B); JB($C3);  { pop r14; pop r13; pop rbp; pop r12; pop rbx; ret }
  { the throw exit: -1 }
  Throw0 := JitLen;
  MovRI(jAX, $FFFFFFFF);
  JB($E9); JD(Cardinal(Exit0 - (JitLen + 4)));
  for I := 0 to JitFixCount - 1 do
    if JitFixes[I].Target < 0 then
    begin
      PC := -1 - JitFixes[I].Target;
      if JitExitOf[PC] < 0 then
      begin
        JitExitOf[PC] := JitLen;
        MovRI(jAX, QWord(PC));
        JB($E9); JD(Cardinal(Exit0 - (JitLen + 4)));
      end;
    end;
  for I := 0 to JitFixCount - 1 do
  begin
    if JitFixes[I].Target = JIT_EPILOGUE then Target := Exit0
    else if JitFixes[I].Target = JIT_THROW then Target := Throw0
    else if JitFixes[I].Target < 0 then Target := JitExitOf[-1 - JitFixes[I].Target]
    else Target := JitNative[JitFixes[I].Target];
    D := Cardinal(Target - (JitFixes[I].At + 4));
    JitBuf[JitFixes[I].At] := D and $FF; JitBuf[JitFixes[I].At + 1] := (D shr 8) and $FF;
    JitBuf[JitFixes[I].At + 2] := (D shr 16) and $FF; JitBuf[JitFixes[I].At + 3] := D shr 24;
  end;
  Code := JitAlloc(JitLen);
  if Code = nil then begin FreeMem(Table); Exit; end;
  {$IFDEF G2_JITSTATS}Inc(JitCompiles); Inc(JitCodeBytes, JitLen);{$ENDIF}
  Move(JitBuf[0], Code^, JitLen);
  { an instruction the code only exits at has no entry: FastLoop does not go in to come straight back out }
  T^.JitComplete := True;
  for PC := 0 to T^.CodeLen - 1 do
    if JitTakenAt[PC] then Table[PC] := PtrUInt(Code) + PtrUInt(JitNative[PC])
    else
    begin
      Table[PC] := 0;
      T^.JitComplete := False;
    end;
  T^.JitTable := Table;
  T^.Jit := Code;
  T^.JitState := 1;
  if Info <> nil then
  begin
    Info^.BaseTable := Table;
    T^.Opt := Info;
  end;
end;

{$I G2.Opt.inc}

{ FastLoop's entry into frame Fr's code at PC: the PC the interpreter goes on with (compiles the template first
  when it has not been tried) }
function XJitRun(Fr: PFrame; PC: Integer): Integer; noinline;
var T: PFuncTemplate;
begin
  T := Fr^.Tpl;
  { compiled on the main thread only (the emitter's buffers are shared); the code itself runs on any }
  if T^.JitState = 0 then
  begin
    Inc(T^.JitCount);
    if T^.JitCount < JitHot then Exit(PC);
    if GetCurrentThreadId = JitMainThread then JitCompile(T) else Exit(PC);
  end;
  if (T^.Opt <> nil) and ((POptInfo(T^.Opt)^.Count <= 0) or (POptInfo(T^.Opt)^.Deopts > OPT_MAX_DEOPTS)) then
    OptMaybe(Fr, PC);
  if (T^.Jit = nil) or (PPtrUInt(T^.JitTable)[PC] = 0) then
  begin
    {$IFDEF G2_JITSTATS}if T^.Jit <> nil then Inc(JitNoEntry[T^.Code[PC] and $FF]);{$ENDIF}
    Exit(PC);
  end;
  { the native stack: code entered inside code's call (XJitCall's nested Execute) counts; past the bound the
    interpreter runs the frames, which keeps a deep recursion's native depth bounded }
  if JitDepthMax = 0 then JitThreadInit;
  if JitTooDeep then Exit(PC);
  Inc(JitDepth);
  Result := TJitFn(T^.Jit)(@VStack[Fr^.Base], Pointer(PPtrUInt(T^.JitTable)[PC]), Fr, 0);
  Dec(JitDepth);
  {$IFDEF G2_JITSTATS}Inc(JitEntries); if Result >= 0 then Inc(JitExits[T^.Code[Result] and $FF]);{$ENDIF}
end;

procedure JitReadHot; noinline;
begin
  JitHot := StrToIntDef(GetEnvironmentVariable('G2_JIT_HOT'), JIT_HOT);
  if JitHot < 1 then JitHot := 1;
end;

function JitOn: Boolean;
begin
  JitMainThread := GetCurrentThreadId;
  JitFsVStack := PtrInt(PtrUInt(@VStack) - JitFsBase);
  JitFsNurseryStart := PtrInt(PtrUInt(@NurseryStart) - JitFsBase);
  JitFsNurseryLimit := PtrInt(PtrUInt(@NurseryLimit) - JitFsBase);
  JitFsArrayIterNext := PtrInt(PtrUInt(@ArrayIterNextFn) - JitFsBase);
  JitFsFrames := PtrInt(PtrUInt(@Frames) - JitFsBase);
  JitFsFrameTop := PtrInt(PtrUInt(@FrameTop) - JitFsBase);
  JitFsStackTop := PtrInt(PtrUInt(@StackTop) - JitFsBase);
  JitFsJitDepth := PtrInt(PtrUInt(@JitDepth) - JitFsBase);
  JitFsJitDepthMax := PtrInt(PtrUInt(@JitDepthMax) - JitFsBase);
  JitFsStackFloor := PtrInt(PtrUInt(@JitStackFloor) - JitFsBase);
  { the code's fs-relative loads take a signed 32-bit displacement }
  if (Abs(JitFsVStack) > $7FFFFFF0) or (Abs(JitFsNurseryStart) > $7FFFFFF0) or (Abs(JitFsNurseryLimit) > $7FFFFFF0) or
     (Abs(JitFsArrayIterNext) > $7FFFFFF0) or (Abs(JitFsFrames) > $7FFFFFF0) or (Abs(JitFsFrameTop) > $7FFFFFF0) or
     (Abs(JitFsStackTop) > $7FFFFFF0) or (Abs(JitFsJitDepth) > $7FFFFFF0) or
     (Abs(JitFsJitDepthMax) > $7FFFFFF0) or (Abs(JitFsStackFloor) > $7FFFFFF0) then
    JitEnabled := 0;
  if JitEnabled < 0 then
    if GetEnvironmentVariable('G2_JIT') = '0' then JitEnabled := 0 else JitEnabled := 1;   { on unless G2_JIT=0 }
  if JitEnabled = 1 then JitReadHot;
  if GetEnvironmentVariable('G2_OPT') = '0' then OptEnabled := 0;
  OptLog := GetEnvironmentVariable('G2_OPTLOG') = '1';
  OptMax := StrToIntDef(GetEnvironmentVariable('G2_OPTMAX'), -1);
  OptReadNoEmbed;
  Result := JitEnabled = 1;
end;
{$ENDIF}
