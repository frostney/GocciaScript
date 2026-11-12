{ ---------------- the baseline JIT (x86-64): a template's fast paths as straight-line machine code ----------------

  The frame stays the interpreter's: the registers live in the value stack and no machine register holds a value
  across instructions, so the code can stop before any instruction and the interpreter carry on there. Every guard
  that fails jumps to its instruction's exit, which returns that instruction's PC; the instruction then runs in
  FastLoop or Execute. Calls, returns, allocation and anything that can throw stay the interpreter's. FastLoop
  enters the code at a frame's first instruction, on a return into a compiled frame and at a backward jump; the
  code runs until an instruction it does not take, in the same frame.

  Machine registers: rbx the frame's registers (R), r12 the frame record, r13 the value stack (an open upvalue's
  cell), r14 / r15 the nursery's bounds (the write barrier); rax, rcx, rdx, rsi, rdi, r8-r11 and xmm0-1 are
  scratch. Entry: rdi R, esi PC, rdx Fr, rcx VS, r8 NS, r9 NL (the System V order FPC uses). }

{$IFDEF CPUX86_64}
type
  TJitFn = function(R: PValue; PC: Integer; Fr: PFrame; VS: PValue; NS, NL: PByte): Integer;
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
  JitEnabled: Integer = -1;        { -1 not read yet (G2_JIT), 0 off, 1 on }
  JitBuf: array of Byte;           { the code being emitted }
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
  JIT_HOT = 20;                    { a template's entries before it is compiled }

procedure JB(X: Byte); inline;
begin
  if JitLen >= Length(JitBuf) then SetLength(JitBuf, JitLen * 2 + 4096);
  JitBuf[JitLen] := X;
  Inc(JitLen);
end;

procedure JD(X: Cardinal);
begin
  JB(X and $FF); JB((X shr 8) and $FF); JB((X shr 16) and $FF); JB(X shr 24);
end;

procedure JQ(X: QWord);
begin
  JD(Cardinal(X and $FFFFFFFF)); JD(Cardinal(X shr 32));
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
procedure JMem(Reg, Base, Disp: Integer);
begin
  JB($80 or ((Reg and 7) shl 3) or (Base and 7));
  if (Base and 7) = 4 then JB($24);
  JD(Cardinal(Disp));
end;

{ [Base + Index * 8 + Disp32] (Index not rsp) }
procedure JMemIdx(Reg, Base, Index, Disp: Integer);
begin
  JB($80 or ((Reg and 7) shl 3) or 4);
  JB($C0 or ((Index and 7) shl 3) or (Base and 7));
  JD(Cardinal(Disp));
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

{ /N Dst, Imm32: 0 add, 1 or, 4 and, 5 sub, 6 xor, 7 cmp }
procedure AluRI(N, Dst, Imm: Integer; W: Boolean);
begin JRex(W, 0, 0, Dst); JB($81); JModRR(N, Dst); JD(Cardinal(Imm)); end;

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

procedure JmpEpilogue;
begin JB($E9); AddFix(JIT_EPILOGUE); end;

procedure JccExit(CC, PC: Integer);                 { jcc to PC's exit: the interpreter runs PC }
begin JB($0F); JB($80 or CC); AddFix(-1 - PC); end;

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

{ the int test of the value in V: not an int goes to the returned forward jump }
function IfNotInt(V: Integer): Integer;
begin
  TagOf(jDX, V);
  AluRI(7, jDX, TAG_INT_HI, False);
  Result := JccFwd(ccNE);
end;

{ a pointer in V to its cell in V (the tag test first: not a pointer is PC's exit) }
procedure UntagPtr(V, PC: Integer);
begin
  TagOf(jDX, V);
  AluRI(7, jDX, TAG_PTR_HI, False);
  JccExit(ccNE, PC);
  MovRI(jDX, PTR_MASK);
  AluRR($21, V, jDX, True);
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
  MovRI(jDX, PTR_MASK);
  AluRR($21, jDX, Val, True);   { and rdx, Val: the cell address }
  AluRR($39, jDX, jR14, True);
  L3 := JccFwd(ccB);
  AluRR($39, jDX, jR15, True);
  L4 := JccFwd(ccAE);
  MovRR(jDI, Cell);
  CallAbs(@Remember);
  PatchHere(L1); PatchHere(L2); PatchHere(L3); PatchHere(L4);
end;

threadvar
  JitDepth: Integer;               { native calls of compiled code in progress (XJitCall) }

procedure JitCompile(T: PFuncTemplate); forward;

{ a compiled frame Fr returns V (the code of a frame XJitCall pushed): as FastLoop's opRet for a call }
procedure XJitRet(Fr: PFrame; V: TValue);
var CF: PFrame;
begin
  CloseUpvals(Fr^.Base);
  if not Fr^.Tpl^.NoHandlers then
    while (HandlerTop > 0) and (Handlers[HandlerTop - 1].Frame >= FrameTop - 1) do Dec(HandlerTop);
  Dec(FrameTop);
  VStack[Fr^.RetIdx] := V;
  CF := @Frames[FrameTop - 1];
  StackTop := CF^.Base + CF^.Tpl^.MaxRegs;
end;

{ compiled code's call (opCall A, Argc arguments, ThisFlag its this-is-undefined bit) of a compiled bytecode
  function: the frame as FastLoop's opCall pushes it, then the callee's code called here, returning into the
  caller's code. 0 done (the result in R[A]), 1 not taken (nothing changed: the interpreter makes the call),
  -1 a throw pending (the frames above the caller gone). A callee whose code stops early is finished by a nested
  Execute; one that throws out of a nested call is handed to it with the throw. Fr^.PC is past the call. }
function XJitCall(Fr: PFrame; A, Argc, ThisFlag: Integer): Integer;
var VA, V: TValue; O: PObj; T: PFuncTemplate; Base, J, Res: Integer; NF: PFrame; R: PValue;
begin
  if MultiRealmAny or (JitDepth >= 400) then Exit(1);
  R := @VStack[Fr^.Base];
  VA := R[A];
  if not IsObject(VA) then Exit(1);
  O := ObjOf(VA);
  if (O^.OKind <> okFunction) or ((O^.OFlags and 1) <> 0) then Exit(1);
  T := PFuncTemplate(O^.Native);
  if not T^.FastCall then Exit(1);
  if T^.JitState <> 1 then
  begin
    if T^.JitState = 0 then
    begin
      Inc(T^.JitCount);
      if (T^.JitCount >= JIT_HOT) and (GetCurrentThreadId = JitMainThread) then JitCompile(T);
    end;
    if T^.JitState <> 1 then Exit(1);
  end;
  { a callee whose code would stop part way costs a nested Execute: only whole ones }
  if not T^.JitComplete then Exit(1);
  Base := Fr^.Base + A + 1;
  if (Base + T^.MaxRegs >= STACK_MAX) or (FrameTop >= FRAME_MAX - 2) then Exit(1);
  if ThisFlag <> 0 then R[A + 1] := VUndefined;
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
  Inc(JitDepth);
  Res := TJitFn(T^.Jit)(@VStack[Base], $40000000, NF, VStack, NurseryStart, NurseryLimit);
  Dec(JitDepth);
  if Res = -2 then Exit(0);   { returned through XJitRet }
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

function JitICGetHit(O: PObj; IC: PPropIC; Dst: PValue): Boolean;
begin
  Result := ICGetHit(O, IC, Dst);
end;

{ the code of instruction PC (I), or its exit when the JIT does not take it }
procedure JitInstr(T: PFuncTemplate; PC: Integer; I: QWord);
var A, B, C: Integer; CU: Cardinal; Op: TOp; L1, L2, L3, L4, LS, LA: Integer; IC: PPropIC; CC: Integer;
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
        MovRMIdx(jAX, jR13, jCX, 0);
        L2 := JmpFwd;
        PatchHere(L1);
        MovRM(jAX, jAX, PtrInt(@PUpval(nil)^.Value));
        PatchHere(L2);
        MovMR(jBX, A * 8, jAX);
      end;
    opJmp: JmpPC(PC + 1 + C);
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
        JccExit(ccNE, PC);
        AluRR($85, jAX, jAX, False);
        if Op = opJT then JccPC(ccNE, PC + 1 + C) else JccPC(ccE, PC + 1 + C);
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
        TagOf(jDX, jAX);
        AluRI(7, jDX, TAG_INT_HI, False);
        JccExit(ccNE, PC);
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
        JccExit(ccE, PC);
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
        JccExit(ccE, PC);
        Cmp32RM(jCX, jDX, PtrInt(@PValueArray(nil)^.Len));
        JccExit(ccAE, PC);
        JRex(False, jCX, 0, jCX); JB($8B); JModRR(jCX, jCX);   { mov ecx, ecx: zero-extended index }
        MovRMIdx(jAX, jDX, jCX, PtrInt(@PValueArray(nil)^.Data));
        MovRI(jCX, VHole);
        AluRR($39, jAX, jCX, True);
        JccExit(ccE, PC);
        MovMR(jBX, A * 8, jAX);
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
        MovzxBRM(jDX, jAX, PtrInt(@PObj(nil)^.OFlags));
        AluRI(4, jDX, 64, False);
        JccExit(ccNE, PC);
        MovRM(jSI, jAX, PtrInt(@PObj(nil)^.Elements));
        AluRR($85, jSI, jSI, True);
        JccExit(ccE, PC);
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
          AluRI(7, jDX, ckObject, False);
          JccExit(ccNE, PC);
        end;
        { IC^.Poly0: the receiver's shape, its kind (Kind, Depth, VerMask and Acc as one 32-bit word: an own
          data property), an inline slot }
        MovRI(jSI, QWord(PtrUInt(IC)));
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
        L2 := JmpFwd;
        { the rest of the cache (ICGetHit): its answer in R[A], or the interpreter's }
        PatchHere(L3); PatchHere(L4); PatchHere(CC);
        MovRR(jDI, jAX);
        { rsi is IC already }
        JRex(True, jDX, 0, jBX); JB($8D); JMem(jDX, jBX, A * 8);   { lea rdx, R[A] }
        CallAbs(@JitICGetHit);
        JB($84); JB($C0);   { test al, al: the Boolean }
        JccExit(ccE, PC);
        PatchHere(L2);
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
        MovRI(jSI, QWord(PtrUInt(IC)));
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
    opCall:
      begin
        { Fr^.PC past the call, then XJitCall: 0 done, 1 the interpreter's, -1 a throw (the code returns -1) }
        JRex(False, 0, 0, jR12); JB($C7); JMem(0, jR12, PtrInt(@PFrame(nil)^.PC)); JD(Cardinal(PC + 1));
        MovRR(jDI, jR12);
        MovRI(jSI, Cardinal(A));
        MovRI(jDX, Cardinal(B));
        MovRI(jCX, CU and 1);
        CallAbs(@XJitCall);
        AluRR($85, jAX, jAX, False);
        L1 := JccFwd(ccE);
        AluRI(7, jAX, 1, False);
        JccExit(ccE, PC);
        MovRI(jAX, $FFFFFFFF);
        JmpEpilogue;
        PatchHere(L1);
      end;
    opRet, opRetUndef:
      begin
        { only in a frame XJitCall pushed (ebp 1): XJitRet, then -2 to it; otherwise the interpreter's }
        JB($85); JB($ED);                       { test ebp, ebp }
        JccExit(ccE, PC);
        if Op = opRet then MovRM(jSI, jBX, A * 8) else MovRI(jSI, VUndefined);
        MovRR(jDI, jR12);
        CallAbs(@XJitRet);
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
procedure JitCompile(T: PFuncTemplate);
var PC, I, Exit0, Target: Integer; Code: PByte; Table: PPtrUInt; D: Cardinal;
begin
  T^.JitState := 2;
  if (T^.Lazy <> nil) or (T^.CodeLen = 0) or T^.IsGenerator or T^.IsAsync then Exit;
  JitLen := 0;
  JitFixCount := 0;
  SetLength(JitNative, T^.CodeLen);
  SetLength(JitExitOf, T^.CodeLen);
  for PC := 0 to T^.CodeLen - 1 do JitExitOf[PC] := -1;
  Table := GetMem(T^.CodeLen * SizeOf(PtrUInt));
  { prologue: the callee-saved registers, the state, then the entry of instruction PC }
  JB($55); JB($53); JB($41); JB($54); JB($41); JB($55); JB($41); JB($56); JB($41); JB($57);
  JB($48); JB($83); JB($EC); JB($08);            { sub rsp, 8: calls see a 16-byte aligned stack }
  MovRR(jBX, jDI); MovRR(jR12, jDX); MovRR(jR13, jCX); MovRR(jR14, jR8); MovRR(jR15, jR9);
  { PC's bit 30: entered by XJitCall (ebp 1: returns are the code's) }
  JB($89); JB($F5);                              { mov ebp, esi }
  JB($C1); JB($ED); JB(30);                      { shr ebp, 30 }
  JB($81); JB($E6); JD($3FFFFFFF);               { and esi, $3FFFFFFF }
  MovRI(jAX, QWord(PtrUInt(Table)));
  JB($FF); JB($24); JB($F0);                     { jmp [rax + rsi * 8] }
  SetLength(JitTakenAt, T^.CodeLen);
  for PC := 0 to T^.CodeLen - 1 do
  begin
    JitNative[PC] := JitLen;
    JitTaken := True;
    JitInstr(T, PC, T^.Code[PC]);
    JitTakenAt[PC] := JitTaken;
  end;
  { past the end (never reached: the last instruction is a return) }
  JmpExit(T^.CodeLen - 1);
  { the exits: eax the PC the interpreter runs; the epilogue }
  Exit0 := JitLen;
  JB($48); JB($83); JB($C4); JB($08);            { add rsp, 8 }
  JB($41); JB($5F); JB($41); JB($5E); JB($41); JB($5D); JB($41); JB($5C); JB($5B); JB($5D); JB($C3);
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
    else if JitFixes[I].Target < 0 then Target := JitExitOf[-1 - JitFixes[I].Target]
    else Target := JitNative[JitFixes[I].Target];
    D := Cardinal(Target - (JitFixes[I].At + 4));
    JitBuf[JitFixes[I].At] := D and $FF; JitBuf[JitFixes[I].At + 1] := (D shr 8) and $FF;
    JitBuf[JitFixes[I].At + 2] := (D shr 16) and $FF; JitBuf[JitFixes[I].At + 3] := D shr 24;
  end;
  Code := JitAlloc(JitLen);
  if Code = nil then begin FreeMem(Table); Exit; end;
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
end;

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
    if T^.JitCount < JIT_HOT then Exit(PC);
    if GetCurrentThreadId = JitMainThread then JitCompile(T) else Exit(PC);
  end;
  if (T^.Jit = nil) or (PPtrUInt(T^.JitTable)[PC] = 0) then Exit(PC);
  Result := TJitFn(T^.Jit)(@VStack[Fr^.Base], PC, Fr, VStack, NurseryStart, NurseryLimit);
end;

function JitOn: Boolean;
begin
  JitMainThread := GetCurrentThreadId;
  if JitEnabled < 0 then
    if GetEnvironmentVariable('G2_JIT') = '1' then JitEnabled := 1 else JitEnabled := 0;
  Result := JitEnabled = 1;
end;
{$ENDIF}
