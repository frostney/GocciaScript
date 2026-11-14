{ ==== The optimizing tier (O1): a function's or a loop's region as typed SSA ====

  A region is everything reachable from an entry instruction (a function's PC 0 or a loop header). Its numbers and
  booleans become SSA values in machine registers (ints in GPRs, doubles in XMM), typed by a fixpoint over the
  region with the frame's values at compile time as the entry's speculation; tagged values stay in their VStack
  registers. An instruction the tier does not take, or whose operand kinds it does not, ends its block with a side
  exit: the live numbers boxed into VStack, then the baseline code at that instruction (the interpreter's when it
  has none). A failed guard (a type, an int overflow) deopts the same way and records why, so that the next
  compilation of the region speculates less; too many deopts drop the region's code.

  The code shares the baseline's conventions (rbx R, r12 the frame, r13 the pointer tag, r14 the ICs, ebp the
  flags) and is entered through the baseline's prologue (the template's JitTable entry for the instruction). It
  saves r15 and keeps its spill slots below it; rax, rcx, rdx, xmm0 and xmm1 are its scratch registers. }

const
  vkNone = 0; vkInt = 1; vkDbl = 2; vkBool = 3; vkTag = 4;

  { nodes }
  onConst = 1; onEntry = 2; onPhi = 3;
  onAddI = 10; onSubI = 11; onMulI = 12; onModI = 13; onNegI = 14; onAndI = 15; onOrI = 16; onXorI = 17;
  onShlI = 18; onShrI = 19; onUShrI = 20; onNotI = 21;
  onAddD = 30; onSubD = 31; onMulD = 32; onDivD = 33; onNegD = 34;
  onI2D = 40; onD2I = 41;
  onCmpI = 50; onCmpD = 51; onNotB = 52; onTruthyI = 53; onTruthyD = 54;
  onStoreTag = 60; onLoadKTag = 61; onCopyTag = 62;
  onUnbox = 70;    { a tagged register (Imm) as a number of the node's kind, checked }
  onEmbed = 71;    { the baseline's code of instruction Imm on VStack: X / Y its flushed reads in OFS }
  onFlush = 72;    { node X boxed into VStack register Imm (an edge into a block that holds it tagged) }

  { feedback bits (TOptInfo.Feedback per instruction) }
  ofbOverflow = 1; ofbEntry = 2; ofbGiveUp = 4; ofbUnboxDbl = 8; ofbUnboxNo = 16; ofbTagCmp = 32;

  { conditions (onCmpI / onCmpD's Imm, a branch's TermCC) }
  ocLt = 0; ocLe = 1; ocGt = 2; ocGe = 3; ocEq = 4; ocNe = 5;

  { terminators }
  otJmp = 1; otBr = 2; otExit = 3;
  { a branch's test }
  otbCmpI = 1; otbCmpD = 2; otbTruthyI = 3; otbTruthyD = 4; otbTruthyB = 5; otbTagEq = 6; otbTruthyTag = 7;
  otbTagCmp = 8;   { strict equality with a tagged operand: TermX / TermY a node (>= 0) or -1 - a VStack register }

  OPT_MAX_NODES = 30000;
  OPT_MAX_BLOCKS = 4000;

type
  TONode = record
    Op, Kind: Byte;
    X, Y: Integer;
    Imm: Int64;
    PC: Integer;             { a guard's (or a side exit's) instruction to resume at }
    FS, FSN: Integer;        { a guard's frame state: OFS[FS .. FS + FSN - 1] }
    Blk: Integer;
    Loc: Integer;            { -1 none; 0 .. 15 a GPR or XMM register (by Kind); >= 100 spill slot Loc - 100 }
    First, Last: Integer;
    PhiArgs: Integer;        { a phi's operands: OPhiArg[PhiArgs + i] for its block's pred i }
    Reg: Integer;            { a phi's bytecode register }
  end;

  TOBlock = record
    StartPC, EndPC: Integer; { its instructions StartPC .. EndPC; StartPC -1 the synthetic entry }
    Succ: array[0..1] of Integer;
    NSucc: Integer;
    Preds: array of Integer;
    Nodes: array of Integer;
    Phis: array of Integer;
    Term, TermTest, TermCC: Integer;
    TermX, TermY: Integer;
    TermImm: Int64;
    TermPC, TermFS, TermFSN: Integer;   { otExit (and otbTagCmp's deopt): the instruction and the frame state }
    TermGuard: Integer;                 { otbTagCmp / otbTruthyTag: the node its deopt stub belongs to }
    InKinds: array of Byte;
    EndMap: array of Integer;
    RPO, PosStart, PosEnd: Integer;
    Label_: Integer;
    Visited: Boolean;
  end;

  TOFSPair = record Reg, Node: Integer; end;

var
  OptLog: Boolean = False;           { G2_OPTLOG=1: each region's compilation on stderr }
  OptMax: Integer = -1;              { G2_OPTMAX=N: at most N regions (a debugging aid) }
  OptCompiles: Integer = 0;
  OptNoEmbed: array[0..255] of Boolean;   { G2_OPTNOEMBED=op,op..: opcodes not embedded (a debugging aid) }
  ON: array of TONode;
  ONCount: Integer;
  OB: array of TOBlock;
  OBCount: Integer;
  OBlockOf: array of Integer;        { the block starting at each PC, -1 none }
  OExit: array of Boolean;           { instructions the region exits at }
  OEmbed: array of Boolean;          { instructions the region runs the baseline's code of }
  OEmbedNode: array of Integer;      { per instruction: its onEmbed node (-1 none) }
  OCurPC: Integer;                   { the instruction being built (an unbox's resume point) }
  OFS: array of TOFSPair;
  OFSCount: Integer;
  OPhiArg: array of Integer;
  OPhiArgCount: Integer;
  ORepl: array of Integer;           { trivial phis' replacements }
  OOrder: array of Integer;          { blocks in reverse postorder }
  OOrderCount: Integer;
  OT: PFuncTemplate;
  OInfo: POptInfo;
  ORegs: Integer;                    { the template's MaxRegs }
  OLive: array of QWord;             { live registers before each instruction: OLiveW words per PC }
  OLiveW: Integer;
  OMap: array of Integer;            { the building block's register map }
  OFail: Boolean;
  OSpills: Integer;
  OFrame: Integer;                   { bytes below the saved r15 }
  OEntry: Integer;
  OGuards: array of Integer;         { nodes and blocks needing a cold stub: >= 0 node, < 0 -1 - block (exit) }
  OGuardJumps: array of Integer;     { per guard: the first of its jumps in OGJ }
  OGJ: array of record G, At, Fb: Integer; end;
  OGJCount: Integer;
  OBranchFix: array of record At, Blk: Integer; end;
  OBranchFixCount: Integer;

{ ---- small helpers ---- }

function ONew(Op, Kind: Integer; X, Y: Integer; Imm: Int64): Integer;
begin
  if ONCount >= OPT_MAX_NODES then begin OFail := True; Exit(0); end;
  if ONCount >= Length(ON) then SetLength(ON, ONCount * 2 + 256);
  Result := ONCount;
  Inc(ONCount);
  FillChar(ON[Result], SizeOf(TONode), 0);
  ON[Result].Op := Op; ON[Result].Kind := Kind;
  ON[Result].X := X; ON[Result].Y := Y; ON[Result].Imm := Imm;
  ON[Result].Loc := -1; ON[Result].PhiArgs := -1; ON[Result].FS := -1;
end;

procedure OAppend(B, N: Integer);
var L: Integer;
begin
  L := Length(OB[B].Nodes);
  SetLength(OB[B].Nodes, L + 1);
  OB[B].Nodes[L] := N;
  ON[N].Blk := B;
end;

{ whether node op Op's X is a node (the constants, entries, phis and tagged stores have none) }
function OHasNodeX(Op: Integer): Boolean; inline;
begin
  Result := not (Op in [onConst, onEntry, onPhi, onStoreTag, onLoadKTag, onCopyTag, onUnbox, onEmbed]);
end;

function OKindJoin(A, B: Integer): Integer;
begin
  if A = vkNone then Exit(B);
  if B = vkNone then Exit(A);
  if A = B then Exit(A);
  if ((A = vkInt) and (B = vkDbl)) or ((A = vkDbl) and (B = vkInt)) then Exit(vkDbl);
  Result := vkTag;
end;

function OIsNum(K: Integer): Boolean; inline;
begin Result := (K = vkInt) or (K = vkDbl); end;

function OLiveAt(PC, R: Integer): Boolean; inline;
begin Result := (OLive[PC * OLiveW + (R shr 6)] and (QWord(1) shl (R and 63))) <> 0; end;

function OIns(PC: Integer): QWord; inline; begin Result := OT^.Code[PC]; end;
function OOpOf(I: QWord): TOp; inline; begin Result := TOp(I and $FF); end;
function OA(I: QWord): Integer; inline; begin Result := (I shr 8) and $FFFF; end;
function OB_(I: QWord): Integer; inline; begin Result := (I shr 24) and $FFFF; end;
function OC(I: QWord): Integer; inline; begin Result := Integer(SarInt64(Int64(I), 40)); end;
function OCU(I: QWord): Integer; inline; begin Result := Integer(I shr 40); end;

{ the instructions the tier can take at all (whether it takes one also depends on its operands' kinds) }
function OStaticOk(Op: TOp): Boolean;
begin
  case Op of
    opNop, opMov, opLoadK, opLoadI, opLoadUndef, opLoadNull, opLoadTrue, opLoadFalse, opLoadHole,
    opAdd, opSub, opMul, opDiv, opMod, opBitAnd, opBitOr, opBitXor, opShl, opShr, opUShr,
    opAddI, opSubI, opIncI, opBitAndI, opBitOrI, opBitXorI, opShlI, opShrI, opUShrI,
    opNeg, opBitNot, opNot, opToNum, opToNumeric,
    opEq, opNe, opSEq, opSNe, opLt, opLe, opGt, opGe,
    opJmp, opJT, opJF, opJNullish, opJNotNullish, opJUndef, opJNotUndef,
    opJNLt, opJNLe, opJNGt, opJNGe, opJNSEq, opJSEq: Result := True;
  else Result := False;
  end;
end;

{ an instruction the region can run as the baseline's code: the registers it reads (Reads[0 .. NR - 1]) and the
  ones it writes (WFrom .. WTo; WFrom -1 none) }
function OEmbedSets(I: QWord; var Reads: array of Integer; out NR, WFrom, WTo: Integer): Boolean;
var A, B, C, K: Integer;
  procedure R(X: Integer); begin if NR <= High(Reads) then begin Reads[NR] := X; Inc(NR); end else Result := False; end;
begin
  Result := True;
  NR := 0; WFrom := -1; WTo := -1;
  A := OA(I); B := OB_(I); C := OCU(I);
  case OOpOf(I) of
    opGetProp, opGetLength, opTypeof, opNot, opToStr, opToPropKey, opToNum, opToNumeric, opNeg, opBitNot,
    opAddI, opSubI, opIncI, opBitAndI, opBitOrI, opBitXorI, opShlI, opShrI, opUShrI:
      begin R(B); WFrom := A; WTo := A; end;
    opGetMethod: begin R(B); WFrom := A; WTo := A + 1; end;
    opSetProp, opDefProp, opDefField, opArrPush: begin R(A); R(B); end;
    opArrHole, opSetGlobal, opSetUpv: R(A);
    opGetElem, opInstanceof, opIn, opAdd, opSub, opMul, opDiv, opMod, opBitAnd, opBitOr, opBitXor, opShl, opShr,
    opUShr, opEq, opNe, opSEq, opSNe, opLt, opLe, opGt, opGe:
      begin R(B); R(C); WFrom := A; WTo := A; end;
    opSetElem: begin R(A); R(B); R(C); end;
    opGetGlobal, opTypeofGlobal, opGetUpv, opNewObj, opLoadHole: begin WFrom := A; WTo := A; end;
    opNewArr: begin for K := C to C + B - 1 do R(K); WFrom := A; WTo := A; end;
    opCall, opNew:
      begin
        { the callee, this (or new.target) and the arguments; the callee's frame then overlaps A and up }
        for K := A to A + 1 + B do R(K);
        WFrom := A; WTo := ORegs - 1;
      end;
  else Result := False;
  end;
end;

function OEmbeddable(PC: Integer): Boolean;
var Rd: array[0..63] of Integer; NR, WF, WT: Integer;
begin
  Result := (PPtrUInt(OInfo^.BaseTable)[PC] <> 0) and OEmbedSets(OIns(PC), Rd, NR, WF, WT) and
    not OptNoEmbed[OIns(PC) and $FF];
end;

function OIsCondJump(Op: TOp): Boolean;
begin
  Result := Op in [opJT, opJF, opJNullish, opJNotNullish, opJUndef, opJNotUndef, opJNLt, opJNLe, opJNGt, opJNGe,
    opJNSEq, opJSEq];
end;

{ the registers an instruction the tier takes reads (-1 none) }
procedure OReads(I: QWord; var R1, R2: Integer);
var Op: TOp;
begin
  R1 := -1; R2 := -1;
  Op := OOpOf(I);
  case Op of
    opMov, opNeg, opBitNot, opNot, opToNum, opToNumeric,
    opAddI, opSubI, opIncI, opBitAndI, opBitOrI, opBitXorI, opShlI, opShrI, opUShrI: R1 := OB_(I);
    opAdd, opSub, opMul, opDiv, opMod, opBitAnd, opBitOr, opBitXor, opShl, opShr, opUShr,
    opEq, opNe, opSEq, opSNe, opLt, opLe, opGt, opGe: begin R1 := OB_(I); R2 := OCU(I); end;
    opJT, opJF, opJNullish, opJNotNullish, opJUndef, opJNotUndef: R1 := OA(I);
    opJNLt, opJNLe, opJNGt, opJNGe, opJNSEq, opJSEq: begin R1 := OA(I); R2 := OB_(I); end;
  end;
end;

function OWrites(I: QWord): Integer;
begin
  case OOpOf(I) of
    opMov, opLoadK, opLoadI, opLoadUndef, opLoadNull, opLoadTrue, opLoadFalse, opLoadHole,
    opAdd, opSub, opMul, opDiv, opMod, opBitAnd, opBitOr, opBitXor, opShl, opShr, opUShr,
    opAddI, opSubI, opIncI, opBitAndI, opBitOrI, opBitXorI, opShlI, opShrI, opUShrI,
    opNeg, opBitNot, opNot, opToNum, opToNumeric,
    opEq, opNe, opSEq, opSNe, opLt, opLe, opGt, opGe: Result := OA(I);
  else Result := -1;
  end;
end;

{ ---- the region's blocks ---- }

{ the blocks reachable from OEntry: a block ends at a jump, at an exit or before another block's first
  instruction. False when the region is too large }
function OBuildBlocks: Boolean;
var Leader: array of Boolean; Seen: array of Boolean; Work: array of Integer; NW, PC, Op_, T, K, B, J: Integer;
  I: QWord; Op: TOp;
begin
  Result := False;
  SetLength(Leader, OT^.CodeLen + 1);
  SetLength(Seen, OT^.CodeLen + 1);
  SetLength(Work, OT^.CodeLen + 1);
  { the instructions reachable, and the leaders }
  NW := 0;
  Work[NW] := OEntry; Inc(NW);
  Leader[OEntry] := True;
  while NW > 0 do
  begin
    Dec(NW);
    PC := Work[NW];
    while (PC < OT^.CodeLen) and not Seen[PC] do
    begin
      Seen[PC] := True;
      I := OIns(PC);
      Op := OOpOf(I);
      if OExit[PC] then Break;
      if Op = opJmp then
      begin
        T := PC + 1 + OC(I);
        Leader[T] := True;
        if not Seen[T] then begin Work[NW] := T; Inc(NW); end;
        if PC + 1 < OT^.CodeLen then Leader[PC + 1] := True;
        Break;
      end;
      if OIsCondJump(Op) then
      begin
        T := PC + 1 + OC(I);
        Leader[T] := True;
        if not Seen[T] then begin Work[NW] := T; Inc(NW); end;
        Leader[PC + 1] := True;
      end;
      Inc(PC);
    end;
  end;
  { the blocks: block 0 the synthetic entry }
  SetLength(OBlockOf, OT^.CodeLen + 1);
  for PC := 0 to OT^.CodeLen do OBlockOf[PC] := -1;
  OBCount := 1;
  SetLength(OB, 64);
  OB[0].StartPC := -1; OB[0].EndPC := -1;
  for PC := 0 to OT^.CodeLen - 1 do
    if Seen[PC] and (Leader[PC] or (PC = OEntry)) then
    begin
      if OBCount >= OPT_MAX_BLOCKS then Exit;
      if OBCount >= Length(OB) then SetLength(OB, OBCount * 2);
      OB[OBCount].StartPC := PC;
      OBlockOf[PC] := OBCount;
      Inc(OBCount);
    end;
  { each block's end and successors }
  for B := 1 to OBCount - 1 do
  begin
    PC := OB[B].StartPC;
    repeat
      I := OIns(PC);
      Op := OOpOf(I);
      OB[B].EndPC := PC;
      if OExit[PC] then
      begin
        OB[B].Term := otExit;
        OB[B].TermPC := PC;
        Break;
      end;
      if Op = opJmp then
      begin
        OB[B].Term := otJmp;
        OB[B].Succ[0] := OBlockOf[PC + 1 + OC(I)];
        OB[B].NSucc := 1;
        Break;
      end;
      if OIsCondJump(Op) then
      begin
        OB[B].Term := otBr;
        OB[B].Succ[0] := OBlockOf[PC + 1 + OC(I)];
        OB[B].Succ[1] := OBlockOf[PC + 1];
        OB[B].NSucc := 2;
        Break;
      end;
      if (PC + 1 >= OT^.CodeLen) then begin OFail := True; Exit; end;
      if OBlockOf[PC + 1] >= 0 then
      begin
        OB[B].Term := otJmp;
        OB[B].Succ[0] := OBlockOf[PC + 1];
        OB[B].NSucc := 1;
        Break;
      end;
      Inc(PC);
    until False;
  end;
  OB[0].Term := otJmp;
  OB[0].Succ[0] := OBlockOf[OEntry];
  OB[0].NSucc := 1;
  { predecessors }
  for B := 0 to OBCount - 1 do
    for K := 0 to OB[B].NSucc - 1 do
    begin
      J := OB[B].Succ[K];
      if J < 0 then begin OFail := True; Exit; end;
      Op_ := Length(OB[J].Preds);
      SetLength(OB[J].Preds, Op_ + 1);
      OB[J].Preds[Op_] := B;
    end;
  Result := not OFail;
end;

{ ---- liveness of the bytecode registers (an exit reads every register) ---- }

procedure OComputeLiveness;
var Changed: Boolean; B, PC, K, W, R1, R2, Wr, S, NR, WF, WT: Integer; Cur: array of QWord; I: QWord;
  ERd: array[0..63] of Integer;
  procedure SetBit(R: Integer); begin if R >= 0 then Cur[R shr 6] := Cur[R shr 6] or (QWord(1) shl (R and 63)); end;
  procedure ClrBit(R: Integer); begin if R >= 0 then Cur[R shr 6] := Cur[R shr 6] and not (QWord(1) shl (R and 63)); end;
begin
  OLiveW := (ORegs + 63) shr 6;
  if OLiveW = 0 then OLiveW := 1;
  SetLength(OLive, (OT^.CodeLen + 1) * OLiveW);
  FillChar(OLive[0], Length(OLive) * 8, 0);
  SetLength(Cur, OLiveW);
  repeat
    Changed := False;
    for B := OBCount - 1 downto 1 do
    begin
      { live out: the successors' live in }
      for W := 0 to OLiveW - 1 do Cur[W] := 0;
      if OB[B].Term = otExit then
      begin
        { the baseline goes on from the exit: a return reads its value only, anything else every register }
        I := OIns(OB[B].TermPC);
        if OOpOf(I) = opRetUndef then
        else if OOpOf(I) = opRet then SetBit(OA(I))
        else for W := 0 to OLiveW - 1 do Cur[W] := not QWord(0);
      end
      else
        for K := 0 to OB[B].NSucc - 1 do
        begin
          S := OB[B].Succ[K];
          for W := 0 to OLiveW - 1 do Cur[W] := Cur[W] or OLive[OB[S].StartPC * OLiveW + W];
        end;
      for PC := OB[B].EndPC downto OB[B].StartPC do
      begin
        I := OIns(PC);
        if not ((PC = OB[B].EndPC) and (OB[B].Term = otExit)) then
          if OEmbed[PC] then
          begin
            OEmbedSets(I, ERd, NR, WF, WT);
            if WF >= 0 then for K := WF to WT do ClrBit(K);
            for K := 0 to NR - 1 do SetBit(ERd[K]);
          end
          else
          begin
            Wr := OWrites(I);
            ClrBit(Wr);
            OReads(I, R1, R2);
            SetBit(R1); SetBit(R2);
          end;
        for W := 0 to OLiveW - 1 do
          if OLive[PC * OLiveW + W] <> Cur[W] then
          begin
            OLive[PC * OLiveW + W] := Cur[W];
            Changed := True;
          end;
      end;
    end;
  until not Changed;
end;

{ ---- kinds: a fixpoint over the region; an instruction whose operands it cannot take becomes an exit ---- }

function OConstKind(V: TValue): Integer;
begin
  if IsInt(V) then Result := vkInt
  else if V < DOUBLE_LIMIT then Result := vkDbl
  else if (V = VTrue) or (V = VFalse) then Result := vkBool
  else Result := vkTag;
end;

{ the kind a tagged register read as a number at PC is speculated to have (vkNone: no speculation) }
function OSpec(PC: Integer): Integer;
begin
  if (OInfo^.Feedback[PC] and ofbUnboxNo) <> 0 then Result := vkNone
  else if (OInfo^.Feedback[PC] and ofbUnboxDbl) <> 0 then Result := vkDbl
  else Result := vkInt;
end;

{ the instructions that read their operands as numbers (a tagged operand speculated, then unboxed) }
function ONumConsumer(Op: TOp): Boolean;
begin
  Result := Op in [opAdd, opSub, opMul, opDiv, opMod, opBitAnd, opBitOr, opBitXor, opShl, opShr, opUShr, opAddI, opSubI,
    opIncI, opBitAndI, opBitOrI, opBitXorI, opShlI, opShrI, opUShrI, opNeg, opBitNot, opToNum, opToNumeric, opLt,
    opLe, opGt, opGe, opJNLt, opJNLe, opJNGt, opJNGe];
end;

{ the kind instruction I gives its register from its operands' kinds K (vkNone: the tier does not take it) }
function OResultKind(PC: Integer; I: QWord; const K: array of Byte): Integer;
var Op: TOp; KB, KC: Integer; Ovf: Boolean;
begin
  Op := OOpOf(I);
  Ovf := (OInfo^.Feedback[PC] and 1) <> 0;
  KB := vkNone; KC := vkNone;
  case Op of
    opMov, opNeg, opBitNot, opNot, opToNum, opToNumeric, opAddI, opSubI, opIncI, opBitAndI, opBitOrI, opBitXorI,
    opShlI, opShrI, opUShrI: KB := K[OB_(I)];
    opAdd, opSub, opMul, opDiv, opMod, opBitAnd, opBitOr, opBitXor, opShl, opShr, opUShr, opEq, opNe, opSEq, opSNe,
    opLt, opLe, opGt, opGe: begin KB := K[OB_(I)]; KC := K[OCU(I)]; end;
  end;
  if ONumConsumer(Op) then
  begin
    if KB = vkTag then KB := OSpec(PC);
    if KC = vkTag then KC := OSpec(PC);
    if (KB = vkNone) and not (Op in [opMov]) and (Op <> opNop) and (Op in [opAdd, opSub, opMul, opDiv, opMod,
      opBitAnd, opBitOr, opBitXor, opShl, opShr, opUShr, opAddI, opSubI, opIncI, opBitAndI, opBitOrI, opBitXorI,
      opShlI, opShrI, opUShrI, opNeg, opBitNot, opToNum, opToNumeric, opLt, opLe, opGt, opGe]) then Exit(vkNone);
  end;
  case Op of
    opNop: Result := vkNone;
    opMov: if KB = vkNone then Result := vkTag else Result := KB;
    opLoadI: Result := vkInt;
    opLoadK: Result := OConstKind(OT^.Consts[OCU(I)]);
    opLoadUndef, opLoadNull, opLoadHole: Result := vkTag;
    opLoadTrue, opLoadFalse: Result := vkBool;
    opAdd, opSub, opMul:
      if (KB = vkInt) and (KC = vkInt) and not Ovf then Result := vkInt
      else if OIsNum(KB) and OIsNum(KC) then Result := vkDbl
      else Result := vkNone;
    opDiv: if OIsNum(KB) and OIsNum(KC) then Result := vkDbl else Result := vkNone;
    opMod: if (KB = vkInt) and (KC = vkInt) and not Ovf then Result := vkInt else Result := vkNone;
    opAddI, opSubI, opIncI:
      if (KB = vkInt) and not Ovf then Result := vkInt
      else if OIsNum(KB) then Result := vkDbl
      else Result := vkNone;
    opBitAnd, opBitOr, opBitXor, opShl, opShr:
      if OIsNum(KB) and OIsNum(KC) then Result := vkInt else Result := vkNone;
    opUShr: if OIsNum(KB) and OIsNum(KC) and not Ovf then Result := vkInt else Result := vkNone;
    opBitAndI, opBitOrI, opBitXorI, opShlI, opShrI, opBitNot:
      if OIsNum(KB) then Result := vkInt else Result := vkNone;
    opUShrI: if OIsNum(KB) and not Ovf then Result := vkInt else Result := vkNone;
    opNeg:
      if (KB = vkInt) and not Ovf then Result := vkInt
      else if OIsNum(KB) then Result := vkDbl
      else Result := vkNone;
    opNot: if OIsNum(KB) or (KB = vkBool) then Result := vkBool else Result := vkNone;
    opToNum, opToNumeric: if OIsNum(KB) then Result := KB else Result := vkNone;
    opLt, opLe, opGt, opGe: if OIsNum(KB) and OIsNum(KC) then Result := vkBool else Result := vkNone;
    opSEq, opSNe, opEq, opNe:
      if (OIsNum(KB) and OIsNum(KC)) or ((KB = vkBool) and (KC = vkBool)) then Result := vkBool
      else Result := vkNone;
  else Result := vkNone;
  end;
end;

{ whether the tier takes the jump instruction I with operand kinds K }
function OJumpOk(PC: Integer; I: QWord; const K: array of Byte): Boolean;
var KA, KB: Integer;
begin
  KA := K[OA(I)];
  case OOpOf(I) of
    opJmp: Result := True;
    opJT, opJF: Result := (KA <> vkNone) and ((KA <> vkTag) or ((OInfo^.Feedback[PC] and ofbTagCmp) = 0));
    opJNullish, opJNotNullish, opJUndef, opJNotUndef: Result := KA <> vkNone;
    opJNLt, opJNLe, opJNGt, opJNGe:
      begin
        KB := K[OB_(I)];
        if KA = vkTag then KA := OSpec(PC);
        if KB = vkTag then KB := OSpec(PC);
        Result := OIsNum(KA) and OIsNum(KB);
      end;
    opJNSEq, opJSEq:
      begin
        KB := K[OB_(I)];
        if (KA = vkTag) or (KB = vkTag) then Result := (OInfo^.Feedback[PC] and ofbTagCmp) = 0
        else Result := (OIsNum(KA) and OIsNum(KB)) or ((KA = vkBool) and (KB = vkBool));
      end;
  else Result := False;
  end;
end;

{ the kinds at each block's start; False when it found an instruction to make an exit (the blocks then change) }
function OInferKinds(const EntryKinds: array of Byte): Boolean;
var Work: array of Integer; InWork: array of Boolean; NW, B, PC, S, K, R, RK, NR, WF, WT: Integer; Cur: array of Byte;
  I: QWord; Op: TOp; Changed: Boolean; ERd: array[0..63] of Integer;
begin
  Result := True;
  for B := 0 to OBCount - 1 do
  begin
    SetLength(OB[B].InKinds, ORegs);
    FillChar(OB[B].InKinds[0], ORegs, vkNone);
  end;
  SetLength(Cur, ORegs);
  SetLength(Work, OBCount * 4 + 16);
  SetLength(InWork, OBCount);
  for R := 0 to ORegs - 1 do OB[0].InKinds[R] := EntryKinds[R];
  NW := 0;
  Work[NW] := 0; Inc(NW); InWork[0] := True;
  while NW > 0 do
  begin
    Dec(NW);
    B := Work[NW];
    InWork[B] := False;
    Move(OB[B].InKinds[0], Cur[0], ORegs);
    if B > 0 then
      for PC := OB[B].StartPC to OB[B].EndPC do
      begin
        I := OIns(PC);
        Op := OOpOf(I);
        if OExit[PC] then Break;
        if (Op = opJmp) or OIsCondJump(Op) then
        begin
          if not OJumpOk(PC, I, Cur) then begin OExit[PC] := True; Result := False; end
          else if Op in [opJNLt, opJNLe, opJNGt, opJNGe] then
          begin
            { the tagged operands unboxed }
            if Cur[OA(I)] = vkTag then Cur[OA(I)] := OSpec(PC);
            if Cur[OB_(I)] = vkTag then Cur[OB_(I)] := OSpec(PC);
          end;
          Break;
        end;
        if Op = opNop then Continue;
        if OEmbed[PC] then
        begin
          OEmbedSets(I, ERd, NR, WF, WT);
          if WF >= 0 then for R := WF to WT do Cur[R] := vkTag;
          Continue;
        end;
        RK := OResultKind(PC, I, Cur);
        if RK = vkNone then
        begin
          if OEmbeddable(PC) then OEmbed[PC] := True else OExit[PC] := True;
          Result := False;
          Break;
        end;
        if ONumConsumer(Op) then
        begin
          { the tagged operands unboxed (as OGetAs does) }
          if not (Op in [opMov]) then
          begin
            if Cur[OB_(I)] = vkTag then Cur[OB_(I)] := OSpec(PC);
            if not (Op in [opNeg, opBitNot, opToNum, opToNumeric, opAddI, opSubI, opIncI, opBitAndI, opBitOrI,
              opBitXorI, opShlI, opShrI, opUShrI]) then
              if Cur[OCU(I)] = vkTag then Cur[OCU(I)] := OSpec(PC);
          end;
        end;
        Cur[OA(I)] := RK;
      end;
    if not Result then Exit;
    if OB[B].Term = otExit then Continue;
    for K := 0 to OB[B].NSucc - 1 do
    begin
      S := OB[B].Succ[K];
      Changed := False;
      for R := 0 to ORegs - 1 do
      begin
        RK := OKindJoin(OB[S].InKinds[R], Cur[R]);
        if RK <> OB[S].InKinds[R] then begin OB[S].InKinds[R] := RK; Changed := True; end;
      end;
      if Changed and not InWork[S] then
      begin
        if NW >= Length(Work) then SetLength(Work, NW * 2);
        Work[NW] := S; Inc(NW); InWork[S] := True;
      end;
    end;
  end;
end;

{ ---- SSA ---- }

{ the frame state at PC: each live register with a value in OMap }
procedure OSnapshot(PC: Integer; out FS, FSN: Integer);
var R: Integer;
begin
  FS := OFSCount;
  FSN := 0;
  for R := 0 to ORegs - 1 do
    if (OMap[R] >= 0) and OLiveAt(PC, R) then
    begin
      if OFSCount >= Length(OFS) then SetLength(OFS, OFSCount * 2 + 256);
      OFS[OFSCount].Reg := R;
      OFS[OFSCount].Node := OMap[R];
      Inc(OFSCount);
      Inc(FSN);
    end;
end;

function OGuard(N, PC: Integer): Integer;
begin
  ON[N].PC := PC;
  OSnapshot(PC, ON[N].FS, ON[N].FSN);
  Result := N;
end;

function OEmitNode(B, Op, Kind, X, Y: Integer; Imm: Int64): Integer;
begin
  Result := ONew(Op, Kind, X, Y, Imm);
  if not OFail then OAppend(B, Result);
end;

function OConstI(B: Integer; X: Int64): Integer;
begin Result := OEmitNode(B, onConst, vkInt, -1, -1, Int32(X)); end;

function OConstD(B: Integer; D: Double): Integer;
begin Result := OEmitNode(B, onConst, vkDbl, -1, -1, PInt64(@D)^); end;

{ register R's value as kind K (an int converted to a double; a bool to an int) }
function OGetAs(B, R, K: Integer): Integer;
var V, SK: Integer;
begin
  V := OMap[R];
  if V < 0 then
  begin
    { a tagged register read as a number: unboxed (checked against the speculation), then held as that }
    SK := OSpec(OCurPC);
    if SK = vkNone then begin OFail := True; Exit(0); end;
    V := OEmitNode(B, onUnbox, SK, -1, -1, R);
    OGuard(V, OCurPC);
    OMap[R] := V;
  end;
  if ON[V].Kind = K then Exit(V);
  if (ON[V].Kind = vkInt) and (K = vkDbl) then
  begin
    if ON[V].Op = onConst then Exit(OConstD(B, Int32(ON[V].Imm)));
    Exit(OEmitNode(B, onI2D, vkDbl, V, -1, 0));
  end;
  if (ON[V].Kind = vkDbl) and (K = vkInt) then
  begin
    { ToInt32 (a bitwise operand) }
    Result := OEmitNode(B, onD2I, vkInt, V, -1, 0);
    Exit;
  end;
  if (ON[V].Kind = vkBool) and (K = vkInt) then Exit(V);
  OFail := True;
  Result := 0;
end;

{ an embedded instruction: its reads that hold numbers flushed to VStack, the baseline's code, its writes tagged.
  Its exits' frame state: the live registers it does not read (their values are in r15 or spill slots there) }
procedure OEmbedInstr(B, PC: Integer; I: QWord);
var Rd: array[0..63] of Integer; NR, WF, WT, K, R, N: Integer; Flushed: array of Boolean;
begin
  if not OEmbedSets(I, Rd, NR, WF, WT) then begin OFail := True; Exit; end;
  N := OEmitNode(B, onEmbed, vkNone, OFSCount, 0, PC);
  if OFail then Exit;
  SetLength(Flushed, ORegs);
  for K := 0 to NR - 1 do
  begin
    R := Rd[K];
    if (R < 0) or (R >= ORegs) or Flushed[R] then Continue;
    Flushed[R] := True;
    if OMap[R] >= 0 then
    begin
      if OFSCount >= Length(OFS) then SetLength(OFS, OFSCount * 2 + 256);
      OFS[OFSCount].Reg := R; OFS[OFSCount].Node := OMap[R];
      Inc(OFSCount);
      Inc(ON[N].Y);
    end;
  end;
  ON[N].PC := PC;
  ON[N].FS := OFSCount; ON[N].FSN := 0;
  for R := 0 to ORegs - 1 do
    if (OMap[R] >= 0) and not Flushed[R] and OLiveAt(PC, R) then
    begin
      if OFSCount >= Length(OFS) then SetLength(OFS, OFSCount * 2 + 256);
      OFS[OFSCount].Reg := R; OFS[OFSCount].Node := OMap[R];
      Inc(OFSCount);
      Inc(ON[N].FSN);
    end;
  OEmbedNode[PC] := N;
  if WF >= 0 then for R := WF to WT do if R < ORegs then OMap[R] := -1;
end;

{ instruction PC (I) of block B into nodes }
procedure OInstr(B, PC: Integer; I: QWord);
var Op: TOp; A, X, Y, N: Integer; V: TValue; Ovf: Boolean; D: Double;
  function Arith(OpI, OpD: Integer): Integer;
  begin
    if OMap[OB_(I)] < 0 then OGetAs(B, OB_(I), OSpec(PC));
    if OMap[OCU(I)] < 0 then OGetAs(B, OCU(I), OSpec(PC));
    if (OMap[OB_(I)] < 0) or (OMap[OCU(I)] < 0) then begin OFail := True; Exit(0); end;
    if (ON[OMap[OB_(I)]].Kind = vkInt) and (ON[OMap[OCU(I)]].Kind = vkInt) and not Ovf and (OpI > 0) then
    begin
      Result := OEmitNode(B, OpI, vkInt, OMap[OB_(I)], OMap[OCU(I)], 0);
      OGuard(Result, PC);
    end
    else if OpD < 0 then begin OFail := True; Result := 0; end
    else Result := OEmitNode(B, OpD, vkDbl, OGetAs(B, OB_(I), vkDbl), OGetAs(B, OCU(I), vkDbl), 0);
  end;
  function ImmArith: Integer;
  var C: Integer;
  begin
    C := OC(I);
    if OMap[OB_(I)] < 0 then OGetAs(B, OB_(I), OSpec(PC));
    if OMap[OB_(I)] < 0 then begin OFail := True; Exit(0); end;
    if (ON[OMap[OB_(I)]].Kind = vkInt) and not Ovf then
    begin
      Result := OEmitNode(B, onAddI, vkInt, OMap[OB_(I)], OConstI(B, C), 0);
      OGuard(Result, PC);
    end
    else Result := OEmitNode(B, onAddD, vkDbl, OGetAs(B, OB_(I), vkDbl), OConstD(B, C), 0);
  end;
  function IntOp(O: Integer; Imm: Boolean): Integer;
  begin
    X := OGetAs(B, OB_(I), vkInt);
    if Imm then Y := OConstI(B, OC(I)) else Y := OGetAs(B, OCU(I), vkInt);
    Result := OEmitNode(B, O, vkInt, X, Y, 0);
    if O = onUShrI then OGuard(Result, PC);
  end;
  function Cmp(CC: Integer; R1, R2: Integer): Integer;
  var K1, K2: Integer;
  begin
    if OMap[R1] < 0 then OGetAs(B, R1, OSpec(PC));
    if OMap[R2] < 0 then OGetAs(B, R2, OSpec(PC));
    if (OMap[R1] < 0) or (OMap[R2] < 0) then begin OFail := True; Exit(0); end;
    K1 := ON[OMap[R1]].Kind; K2 := ON[OMap[R2]].Kind;
    if ((K1 = vkInt) or (K1 = vkBool)) and (K1 = K2) then
      Result := OEmitNode(B, onCmpI, vkBool, OMap[R1], OMap[R2], CC)
    else Result := OEmitNode(B, onCmpD, vkBool, OGetAs(B, R1, vkDbl), OGetAs(B, R2, vkDbl), CC);
  end;
begin
  Op := OOpOf(I);
  A := OA(I);
  Ovf := (OInfo^.Feedback[PC] and 1) <> 0;
  N := -1;
  if OEmbed[PC] then
  begin
    OEmbedInstr(B, PC, I);
    Exit;
  end;
  case Op of
    opNop: Exit;
    opMov:
      if OMap[OB_(I)] >= 0 then N := OMap[OB_(I)]
      else
      begin
        OEmitNode(B, onCopyTag, vkNone, OB_(I), A, 0);
        OMap[A] := -1;
        Exit;
      end;
    opLoadI: N := OConstI(B, OC(I));
    opLoadK:
      begin
        V := OT^.Consts[OCU(I)];
        case OConstKind(V) of
          vkInt: N := OConstI(B, IntOf(V));
          vkDbl: begin D := PDouble(@V)^; N := OConstD(B, D); end;
          vkBool: N := OEmitNode(B, onConst, vkBool, -1, -1, Ord(V = VTrue));
        else
          begin
            OEmitNode(B, onLoadKTag, vkNone, A, -1, OCU(I));
            OMap[A] := -1;
            Exit;
          end;
        end;
      end;
    opLoadUndef, opLoadNull, opLoadHole:
      begin
        if Op = opLoadUndef then V := VUndefined else if Op = opLoadNull then V := VNull else V := VHole;
        OEmitNode(B, onStoreTag, vkNone, A, -1, Int64(V));
        OMap[A] := -1;
        Exit;
      end;
    opLoadTrue: N := OEmitNode(B, onConst, vkBool, -1, -1, 1);
    opLoadFalse: N := OEmitNode(B, onConst, vkBool, -1, -1, 0);
    opAdd: N := Arith(onAddI, onAddD);
    opSub: N := Arith(onSubI, onSubD);
    opMul: N := Arith(onMulI, onMulD);
    opDiv: N := Arith(-1, onDivD);
    opMod: N := Arith(onModI, -1);
    opAddI, opSubI, opIncI: N := ImmArith;
    opBitAnd: N := IntOp(onAndI, False);
    opBitOr: N := IntOp(onOrI, False);
    opBitXor: N := IntOp(onXorI, False);
    opShl: N := IntOp(onShlI, False);
    opShr: N := IntOp(onShrI, False);
    opUShr: N := IntOp(onUShrI, False);
    opBitAndI: N := IntOp(onAndI, True);
    opBitOrI: N := IntOp(onOrI, True);
    opBitXorI: N := IntOp(onXorI, True);
    opShlI: N := IntOp(onShlI, True);
    opShrI: N := IntOp(onShrI, True);
    opUShrI: N := IntOp(onUShrI, True);
    opBitNot: N := OEmitNode(B, onNotI, vkInt, OGetAs(B, OB_(I), vkInt), -1, 0);
    opNeg:
      begin
        if OMap[OB_(I)] < 0 then OGetAs(B, OB_(I), OSpec(PC));
        X := OMap[OB_(I)];
        if X < 0 then begin OFail := True; Exit; end;
        if (ON[X].Kind = vkInt) and not Ovf then
        begin
          N := OEmitNode(B, onNegI, vkInt, X, -1, 0);
          OGuard(N, PC);
        end
        else N := OEmitNode(B, onNegD, vkDbl, OGetAs(B, OB_(I), vkDbl), -1, 0);
      end;
    opNot:
      begin
        X := OMap[OB_(I)];
        if X < 0 then begin OFail := True; Exit; end;
        case ON[X].Kind of
          vkBool: N := OEmitNode(B, onNotB, vkBool, X, -1, 0);
          vkInt: N := OEmitNode(B, onNotB, vkBool, OEmitNode(B, onTruthyI, vkBool, X, -1, 0), -1, 0);
        else N := OEmitNode(B, onNotB, vkBool, OEmitNode(B, onTruthyD, vkBool, X, -1, 0), -1, 0);
        end;
      end;
    opToNum, opToNumeric:
      begin
        if OMap[OB_(I)] < 0 then OGetAs(B, OB_(I), OSpec(PC));
        N := OMap[OB_(I)];
      end;
    opLt: N := Cmp(ocLt, OB_(I), OCU(I));
    opLe: N := Cmp(ocLe, OB_(I), OCU(I));
    opGt: N := Cmp(ocGt, OB_(I), OCU(I));
    opGe: N := Cmp(ocGe, OB_(I), OCU(I));
    opSEq, opEq: N := Cmp(ocEq, OB_(I), OCU(I));
    opSNe, opNe: N := Cmp(ocNe, OB_(I), OCU(I));
  else
    OFail := True;
    Exit;
  end;
  if (N < 0) or OFail then begin OFail := True; Exit; end;
  OMap[A] := N;
end;

{ a branch's deopt (otbTruthyTag, otbTagCmp): a guard node of its own, its frame state the block's end }
procedure OTermGuard(B, PC: Integer);
var G: Integer;
begin
  G := ONew(onUnbox, vkNone, -1, -1, -1);
  if OFail then Exit;
  ON[G].Blk := B;
  OGuard(G, PC);
  OB[B].TermGuard := G;
  OB[B].TermFS := ON[G].FS;
  OB[B].TermFSN := ON[G].FSN;
end;

{ block B's terminator from its last instruction }
procedure OTerminator(B: Integer);
var PC, R1, R2, K1, K2: Integer; I: QWord; Op: TOp;
  procedure CmpTest(CC: Integer; Negate: Boolean);
  begin
    if OMap[R1] < 0 then OGetAs(B, R1, OSpec(PC));
    if OMap[R2] < 0 then OGetAs(B, R2, OSpec(PC));
    if (OMap[R1] < 0) or (OMap[R2] < 0) then begin OFail := True; Exit; end;
    K1 := ON[OMap[R1]].Kind; K2 := ON[OMap[R2]].Kind;
    if ((K1 = vkInt) or (K1 = vkBool)) and (K1 = K2) then
    begin
      OB[B].TermTest := otbCmpI; OB[B].TermX := OMap[R1]; OB[B].TermY := OMap[R2];
    end
    else
    begin
      OB[B].TermTest := otbCmpD; OB[B].TermX := OGetAs(B, R1, vkDbl); OB[B].TermY := OGetAs(B, R2, vkDbl);
    end;
    OB[B].TermCC := CC;
    OB[B].TermImm := Ord(Negate);
  end;
begin
  if OB[B].Term = otExit then
  begin
    OSnapshot(OB[B].TermPC, OB[B].TermFS, OB[B].TermFSN);
    Exit;
  end;
  if OB[B].Term <> otBr then Exit;
  PC := OB[B].EndPC;
  I := OIns(PC);
  Op := OOpOf(I);
  R1 := OA(I);
  R2 := OB_(I);
  { Succ[0] the jump target: taken when the test holds (TermImm 1: when it does not) }
  case Op of
    opJT, opJF:
      begin
        if OMap[R1] < 0 then
        begin
          { a tagged register's truthiness (its uncommon cases the deopt) }
          OB[B].TermTest := otbTruthyTag;
          OB[B].TermX := R1;
          OB[B].TermImm := Ord(Op = opJF);
          OTermGuard(B, PC);
          Exit;
        end;
        case ON[OMap[R1]].Kind of
          vkInt: OB[B].TermTest := otbTruthyI;
          vkDbl: OB[B].TermTest := otbTruthyD;
        else OB[B].TermTest := otbTruthyB;
        end;
        OB[B].TermX := OMap[R1];
        OB[B].TermImm := Ord(Op = opJF);
      end;
    opJNullish, opJNotNullish, opJUndef, opJNotUndef:
      if OMap[R1] >= 0 then
      begin
        { a number or a boolean: never undefined or null }
        OB[B].Term := otJmp;
        if Op in [opJNotNullish, opJNotUndef] then OB[B].Succ[0] := OB[B].Succ[0] else OB[B].Succ[0] := OB[B].Succ[1];
        OB[B].NSucc := 1;
      end
      else
      begin
        OB[B].TermTest := otbTagEq;
        OB[B].TermX := R1;
        OB[B].TermCC := Ord(Op);
        OB[B].TermImm := 0;
      end;
    opJNSEq, opJSEq:
      if (OMap[R1] < 0) or (OMap[R2] < 0) then
      begin
        { a tagged operand: compared as values (pointers of different bits, and doubles, the deopt) }
        OB[B].TermTest := otbTagCmp;
        if OMap[R1] >= 0 then OB[B].TermX := OMap[R1] else OB[B].TermX := -1 - R1;
        if OMap[R2] >= 0 then OB[B].TermY := OMap[R2] else OB[B].TermY := -1 - R2;
        OB[B].TermCC := ocEq;
        OB[B].TermImm := Ord(Op = opJNSEq);
        OTermGuard(B, PC);
        Exit;
      end
      else CmpTest(ocEq, Op = opJNSEq);
    opJNLt: CmpTest(ocLt, True);
    opJNLe: CmpTest(ocLe, True);
    opJNGt: CmpTest(ocGt, True);
    opJNGe: CmpTest(ocGe, True);
  end;
end;

{ the representative of node N after trivial phis were replaced }
function ORep(N: Integer): Integer;
begin
  Result := N;
  if N < 0 then Exit;
  while ORepl[Result] <> Result do Result := ORepl[Result];
end;

{ a phi whose operands are one value (or itself) is that value: replaced everywhere, until none is left }
procedure ORemoveTrivialPhis;
var Changed: Boolean; B, K, J, N, P, Same, A: Integer; Keep: array of Integer; NK: Integer;
begin
  SetLength(ORepl, ONCount);
  for N := 0 to ONCount - 1 do ORepl[N] := N;
  repeat
    Changed := False;
    for B := 0 to OBCount - 1 do
      for K := 0 to High(OB[B].Phis) do
      begin
        N := OB[B].Phis[K];
        if ORepl[N] <> N then Continue;
        Same := -1;
        for P := 0 to High(OB[B].Preds) do
        begin
          A := ORep(OPhiArg[ON[N].PhiArgs + P]);
          if (A = N) or (A = Same) then Continue;
          if Same >= 0 then begin Same := -2; Break; end;
          Same := A;
        end;
        if Same >= 0 then begin ORepl[N] := Same; Changed := True; end;
      end;
  until not Changed;
  { the uses rewritten, the removed phis dropped }
  for N := 0 to ONCount - 1 do
  begin
    { (the tagged stores' X and Y are registers, not nodes) }
    if not OHasNodeX(ON[N].Op) then Continue;
    ON[N].X := ORep(ON[N].X);
    if ON[N].Op in [onAddI, onSubI, onMulI, onModI, onAndI, onOrI, onXorI, onShlI, onShrI, onUShrI, onAddD, onSubD,
      onMulD, onDivD, onCmpI, onCmpD] then ON[N].Y := ORep(ON[N].Y);
  end;
  for K := 0 to OFSCount - 1 do OFS[K].Node := ORep(OFS[K].Node);
  for K := 0 to OPhiArgCount - 1 do OPhiArg[K] := ORep(OPhiArg[K]);
  for B := 0 to OBCount - 1 do
  begin
    if OB[B].Term = otBr then
      case OB[B].TermTest of
        otbCmpI, otbCmpD: begin OB[B].TermX := ORep(OB[B].TermX); OB[B].TermY := ORep(OB[B].TermY); end;
        otbTruthyI, otbTruthyD, otbTruthyB: OB[B].TermX := ORep(OB[B].TermX);
        otbTagCmp:
          begin
            if OB[B].TermX >= 0 then OB[B].TermX := ORep(OB[B].TermX);
            if OB[B].TermY >= 0 then OB[B].TermY := ORep(OB[B].TermY);
          end;
      end;
    SetLength(Keep, Length(OB[B].Phis));
    NK := 0;
    for J := 0 to High(OB[B].Phis) do
      if ORepl[OB[B].Phis[J]] = OB[B].Phis[J] then begin Keep[NK] := OB[B].Phis[J]; Inc(NK); end;
    SetLength(OB[B].Phis, NK);
    for J := 0 to NK - 1 do OB[B].Phis[J] := Keep[J];
  end;
end;

function OBuildSSA: Boolean;
var Stack: array of Integer; Idx: array of Integer; NS, B, S, K, R, P, PC, N, V, Ph, L: Integer;
  Flushed: array of Integer;
  Phis: array of record B, R, N: Integer; end; NPhis: Integer; Visited: array of Boolean;
begin
  Result := False;
  { reverse postorder }
  SetLength(Visited, OBCount);
  SetLength(Stack, OBCount + 1);
  SetLength(Idx, OBCount + 1);
  SetLength(OOrder, OBCount);
  OOrderCount := 0;
  NS := 0;
  Stack[0] := 0; Idx[0] := 0; NS := 1; Visited[0] := True;
  while NS > 0 do
  begin
    B := Stack[NS - 1];
    if Idx[NS - 1] < OB[B].NSucc then
    begin
      S := OB[B].Succ[Idx[NS - 1]];
      Inc(Idx[NS - 1]);
      if not Visited[S] then
      begin
        Visited[S] := True;
        Stack[NS] := S; Idx[NS] := 0; Inc(NS);
      end;
    end
    else
    begin
      OOrder[OOrderCount] := B; Inc(OOrderCount);
      Dec(NS);
    end;
  end;
  for K := 0 to (OOrderCount div 2) - 1 do
  begin
    B := OOrder[K]; OOrder[K] := OOrder[OOrderCount - 1 - K]; OOrder[OOrderCount - 1 - K] := B;
  end;
  for K := 0 to OOrderCount - 1 do OB[OOrder[K]].RPO := K;
  { the blocks in that order }
  SetLength(OMap, ORegs);
  NPhis := 0;
  for K := 0 to OOrderCount - 1 do
  begin
    B := OOrder[K];
    if B = 0 then
    begin
      { the entry: each live register of a number or boolean kind loaded and checked }
      for R := 0 to ORegs - 1 do
      begin
        OMap[R] := -1;
        if (OB[0].InKinds[R] in [vkInt, vkDbl, vkBool]) and OLiveAt(OEntry, R) then
        begin
          N := OEmitNode(0, onEntry, OB[0].InKinds[R], -1, -1, R);
          ON[N].PC := OEntry;   { (no frame state: VStack is unchanged) }
          ON[N].FS := OFSCount; ON[N].FSN := 0;
          OMap[R] := N;
        end;
      end;
    end
    else if (Length(OB[B].Preds) = 1) and (OB[OB[B].Preds[0]].RPO < OB[B].RPO) then
      Move(OB[OB[B].Preds[0]].EndMap[0], OMap[0], ORegs * SizeOf(Integer))
    else
    begin
      { a join: a phi for each live register of a number or boolean kind }
      PC := OB[B].StartPC;
      for R := 0 to ORegs - 1 do
      begin
        OMap[R] := -1;
        if (OB[B].InKinds[R] in [vkInt, vkDbl, vkBool]) and OLiveAt(PC, R) then
        begin
          N := ONew(onPhi, OB[B].InKinds[R], -1, -1, 0);
          if OFail then Exit;
          ON[N].Blk := B;
          ON[N].Reg := R;
          L := Length(OB[B].Phis);
          SetLength(OB[B].Phis, L + 1);
          OB[B].Phis[L] := N;
          OMap[R] := N;
          if NPhis >= Length(Phis) then SetLength(Phis, NPhis * 2 + 64);
          Phis[NPhis].B := B; Phis[NPhis].R := R; Phis[NPhis].N := N;
          Inc(NPhis);
        end;
      end;
    end;
    if B > 0 then
      for PC := OB[B].StartPC to OB[B].EndPC do
      begin
        if (PC = OB[B].EndPC) and ((OB[B].Term = otExit) or (OB[B].Term = otBr) or
           (OOpOf(OIns(PC)) = opJmp)) then Break;
        OCurPC := PC;
        OInstr(B, PC, OIns(PC));
        if OFail then Exit;
      end;
    OCurPC := OB[B].EndPC;
    OTerminator(B);
    if OFail then Exit;
    SetLength(OB[B].EndMap, ORegs);
    Move(OMap[0], OB[B].EndMap[0], ORegs * SizeOf(Integer));
  end;
  { a successor that holds a register tagged while this block holds a number in it: the number boxed into VStack
    at the end of this block (before its terminator) }
  for B := 0 to OBCount - 1 do
  begin
    if (OB[B].EndMap = nil) or (OB[B].Term = otExit) then Continue;
    for K := 0 to OB[B].NSucc - 1 do
    begin
      S := OB[B].Succ[K];
      for R := 0 to ORegs - 1 do
        if (OB[B].EndMap[R] >= 0) and (OB[S].InKinds[R] = vkTag) and OLiveAt(OB[S].StartPC, R) then
        begin
          if Flushed = nil then SetLength(Flushed, ORegs);
          if Flushed[R] = B + 1 then Continue;
          Flushed[R] := B + 1;
          OEmitNode(B, onFlush, vkNone, OB[B].EndMap[R], -1, R);
        end;
    end;
  end;
  { the phis' operands, converted at the end of their predecessors }
  for Ph := 0 to NPhis - 1 do
  begin
    B := Phis[Ph].B; R := Phis[Ph].R; N := Phis[Ph].N;
    if OPhiArgCount + Length(OB[B].Preds) > Length(OPhiArg) then SetLength(OPhiArg, (OPhiArgCount + Length(OB[B].Preds)) * 2 + 256);
    ON[N].PhiArgs := OPhiArgCount;
    Inc(OPhiArgCount, Length(OB[B].Preds));
    for P := 0 to High(OB[B].Preds) do
    begin
      S := OB[B].Preds[P];
      V := OB[S].EndMap[R];
      if V < 0 then Exit;
      if ON[V].Kind <> ON[N].Kind then
      begin
        if (ON[V].Kind = vkInt) and (ON[N].Kind = vkDbl) then
          V := OEmitNode(S, onI2D, vkDbl, V, -1, 0)
        else Exit;
      end;
      OPhiArg[ON[N].PhiArgs + P] := V;
    end;
  end;
  ORemoveTrivialPhis;
  Result := not OFail;
end;

{ ---- SSA liveness, intervals and linear-scan allocation ---- }

{ block B's terminator's node operands (-1 none) }
procedure OTermNodes(B: Integer; out X, Y: Integer);
begin
  X := -1; Y := -1;
  if OB[B].Term <> otBr then Exit;
  case OB[B].TermTest of
    otbCmpI, otbCmpD: begin X := OB[B].TermX; Y := OB[B].TermY; end;
    otbTruthyI, otbTruthyD, otbTruthyB: X := OB[B].TermX;
    otbTagCmp:
      begin
        if OB[B].TermX >= 0 then X := OB[B].TermX;
        if OB[B].TermY >= 0 then Y := OB[B].TermY;
      end;
  end;
end;

{ whether block B's terminator has a frame state (an exit, or a branch that can deopt) }
function OTermHasFS(B: Integer): Boolean;
begin
  Result := (OB[B].Term = otExit) or ((OB[B].Term = otBr) and (OB[B].TermTest in [otbTagCmp, otbTruthyTag]));
end;

procedure OAllocate;
var Pos, K, B, J, N, S, P, W, NW, Ph, F, NClob: Integer; Changed, Cross: Boolean; Clob: array of Integer;
  LiveIn, LiveOut: array of array of QWord; Cur: array of QWord;
  Sorted: array of Integer; Active: array of Integer; NA: Integer;
  FreeG: array[0..15] of Boolean; FreeX: array[0..15] of Boolean;
  procedure Use(V, At: Integer);
  begin
    if V < 0 then Exit;
    if At > ON[V].Last then ON[V].Last := At;
  end;
  procedure SetB(var A: array of QWord; V: Integer); begin A[V shr 6] := A[V shr 6] or (QWord(1) shl (V and 63)); end;
  procedure ClrB(var A: array of QWord; V: Integer); begin A[V shr 6] := A[V shr 6] and not (QWord(1) shl (V and 63)); end;
  function TstB(const A: array of QWord; V: Integer): Boolean; begin Result := (A[V shr 6] and (QWord(1) shl (V and 63))) <> 0; end;
  function HasValue(V: Integer): Boolean;
  begin Result := (ON[V].Kind in [vkInt, vkDbl, vkBool]); end;
  function IsX(V: Integer): Boolean; begin Result := ON[V].Kind = vkDbl; end;
  procedure Mark(V: Integer); begin if V >= 0 then SetB(Cur, V); end;
  procedure UsesBackward(B: Integer);
  var T, Q, M: Integer;
  begin
    { the terminator's operands and frame state }
    if OTermHasFS(B) then
      for Q := OB[B].TermFS to OB[B].TermFS + OB[B].TermFSN - 1 do Mark(OFS[Q].Node);
    OTermNodes(B, Q, M);
    Mark(Q); Mark(M);
    for T := High(OB[B].Nodes) downto 0 do
    begin
      M := OB[B].Nodes[T];
      ClrB(Cur, M);
      if not OHasNodeX(ON[M].Op) then Continue;
      Mark(ON[M].X);
      if ON[M].Op in [onAddI, onSubI, onMulI, onModI, onAndI, onOrI, onXorI, onShlI, onShrI, onUShrI, onAddD, onSubD,
        onMulD, onDivD, onCmpI, onCmpD] then Mark(ON[M].Y);
      if ON[M].FS >= 0 then
        for Q := ON[M].FS to ON[M].FS + ON[M].FSN - 1 do Mark(OFS[Q].Node);
      if ON[M].Op = onEmbed then
        for Q := ON[M].X to ON[M].X + ON[M].Y - 1 do Mark(OFS[Q].Node);
    end;
    for T := 0 to High(OB[B].Phis) do ClrB(Cur, OB[B].Phis[T]);
  end;
begin
  W := (ONCount + 63) shr 6;
  SetLength(LiveIn, OBCount);
  SetLength(LiveOut, OBCount);
  for B := 0 to OBCount - 1 do begin SetLength(LiveIn[B], W); SetLength(LiveOut[B], W); end;
  SetLength(Cur, W);
  repeat
    Changed := False;
    for K := OOrderCount - 1 downto 0 do
    begin
      B := OOrder[K];
      FillChar(Cur[0], W * 8, 0);
      for J := 0 to OB[B].NSucc - 1 do
      begin
        S := OB[B].Succ[J];
        for NW := 0 to W - 1 do Cur[NW] := Cur[NW] or LiveIn[S][NW];
        for Ph := 0 to High(OB[S].Phis) do
        begin
          N := OB[S].Phis[Ph];
          for P := 0 to High(OB[S].Preds) do
            if OB[S].Preds[P] = B then Mark(OPhiArg[ON[N].PhiArgs + P]);
        end;
      end;
      for NW := 0 to W - 1 do
        if LiveOut[B][NW] <> Cur[NW] then begin LiveOut[B][NW] := Cur[NW]; Changed := True; end;
      UsesBackward(B);
      for NW := 0 to W - 1 do
        if LiveIn[B][NW] <> Cur[NW] then begin LiveIn[B][NW] := Cur[NW]; Changed := True; end;
    end;
  until not Changed;
  { positions and intervals }
  Pos := 0;
  NClob := 0;
  for N := 0 to ONCount - 1 do begin ON[N].First := MaxInt; ON[N].Last := -1; end;
  for K := 0 to OOrderCount - 1 do
  begin
    B := OOrder[K];
    OB[B].PosStart := Pos;
    for Ph := 0 to High(OB[B].Phis) do ON[OB[B].Phis[Ph]].First := Pos;
    Inc(Pos, 2);
    for J := 0 to High(OB[B].Nodes) do
    begin
      N := OB[B].Nodes[J];
      ON[N].First := Pos;
      if OHasNodeX(ON[N].Op) then Use(ON[N].X, Pos);
      if ON[N].Op in [onAddI, onSubI, onMulI, onModI, onAndI, onOrI, onXorI, onShlI, onShrI, onUShrI, onAddD, onSubD,
        onMulD, onDivD, onCmpI, onCmpD] then Use(ON[N].Y, Pos);
      if ON[N].FS >= 0 then
        for F := ON[N].FS to ON[N].FS + ON[N].FSN - 1 do Use(OFS[F].Node, Pos);
      if ON[N].Op = onEmbed then
      begin
        { its flushed reads, then the clobber }
        for F := ON[N].X to ON[N].X + ON[N].Y - 1 do Use(OFS[F].Node, Pos);
        if NClob >= Length(Clob) then SetLength(Clob, NClob * 2 + 16);
        Clob[NClob] := Pos; Inc(NClob);
      end;
      Inc(Pos, 2);
    end;
    OB[B].PosEnd := Pos;
    if OTermHasFS(B) then
      for F := OB[B].TermFS to OB[B].TermFS + OB[B].TermFSN - 1 do Use(OFS[F].Node, Pos);
    OTermNodes(B, F, P);
    Use(F, Pos); Use(P, Pos);
    Inc(Pos, 2);
    { live across the block: its whole range }
    for N := 0 to ONCount - 1 do
    begin
      if TstB(LiveOut[B], N) then Use(N, Pos);
      if TstB(LiveIn[B], N) and (OB[B].PosStart < ON[N].First) then ON[N].First := OB[B].PosStart;
    end;
  end;
  { linear scan: GPRs rsi, rdi, r8 - r11, r15; XMM 2 - 15 }
  for J := 0 to 15 do begin FreeG[J] := False; FreeX[J] := J >= 2; end;
  FreeG[jSI] := True; FreeG[jDI] := True; FreeG[jR8] := True; FreeG[jR9] := True; FreeG[jR10] := True;
  FreeG[jR11] := True; FreeG[jR15] := True;
  SetLength(Sorted, ONCount);
  NW := 0;
  for N := 0 to ONCount - 1 do
    if ON[N].Op = onConst then ON[N].Loc := -2
    else if HasValue(N) and (ON[N].Last >= 0) then
    begin
      if ON[N].First = MaxInt then ON[N].First := ON[N].Last;
      Sorted[NW] := N; Inc(NW);
    end
    else if HasValue(N) and (ON[N].First <> MaxInt) then
    begin
      { never used: a register for its own def }
      ON[N].Last := ON[N].First;
      Sorted[NW] := N; Inc(NW);
    end;
  { insertion sort by First (regions are small) }
  for J := 1 to NW - 1 do
  begin
    N := Sorted[J]; K := J - 1;
    while (K >= 0) and (ON[Sorted[K]].First > ON[N].First) do begin Sorted[K + 1] := Sorted[K]; Dec(K); end;
    Sorted[K + 1] := N;
  end;
  SetLength(Active, NW);
  NA := 0;
  OSpills := 0;
  for J := 0 to NW - 1 do
  begin
    N := Sorted[J];
    { expire }
    K := 0;
    while K < NA do
      if ON[Active[K]].Last < ON[N].First then
      begin
        if IsX(Active[K]) then FreeX[ON[Active[K]].Loc] := True else FreeG[ON[Active[K]].Loc] := True;
        Active[K] := Active[NA - 1]; Dec(NA);
      end
      else Inc(K);
    { live across an embedded instruction (whose code clobbers the caller-saved registers): r15 or a slot }
    Cross := False;
    for K := 0 to NClob - 1 do
      if (Clob[K] > ON[N].First) and (Clob[K] < ON[N].Last) then begin Cross := True; Break; end;
    if Cross then
    begin
      if not IsX(N) and FreeG[jR15] then
      begin
        ON[N].Loc := jR15;
        FreeG[jR15] := False;
        Active[NA] := N; Inc(NA);
      end
      else begin ON[N].Loc := 100 + OSpills; Inc(OSpills); end;
      Continue;
    end;
    { a free register of its class }
    P := -1;
    for K := 0 to 15 do
      if (IsX(N) and FreeX[K]) or (not IsX(N) and FreeG[K]) then begin P := K; Break; end;
    if P >= 0 then
    begin
      ON[N].Loc := P;
      if IsX(N) then FreeX[P] := False else FreeG[P] := False;
      Active[NA] := N; Inc(NA);
    end
    else
    begin
      { spill whichever of the same class lives longest }
      S := -1;
      for K := 0 to NA - 1 do
        if (IsX(Active[K]) = IsX(N)) and ((S < 0) or (ON[Active[K]].Last > ON[Active[S]].Last)) then S := K;
      if (S >= 0) and (ON[Active[S]].Last > ON[N].Last) then
      begin
        ON[N].Loc := ON[Active[S]].Loc;
        ON[Active[S]].Loc := 100 + OSpills; Inc(OSpills);
        Active[S] := N;
      end
      else
      begin
        ON[N].Loc := 100 + OSpills; Inc(OSpills);
      end;
    end;
  end;
  { the frame below the saved r15: the spill slots, rsp kept 16-byte aligned }
  OFrame := OSpills * 8;
  if (OFrame mod 16) = 0 then Inc(OFrame, 8);
end;

{ ---- code ---- }

procedure OSse(Prefix, Op: Byte; Dst, Src: Integer);   { an SSE op on registers (xmm8 - 15 through REX) }
begin JB(Prefix); JRex(False, Dst, 0, Src); JB($0F); JB(Op); JModRR(Dst, Src); end;

procedure OSseM(Prefix, Op: Byte; X, Base, Disp: Integer);   { an SSE op with memory [Base + Disp] }
begin JB(Prefix); JRex(False, X, 0, Base); JB($0F); JB(Op); JMem(X, Base, Disp); end;

function OSlot(V: Integer): Integer; inline; begin Result := (ON[V].Loc - 100) * 8; end;
function OInReg(V: Integer): Boolean; inline; begin Result := (ON[V].Loc >= 0) and (ON[V].Loc < 100); end;
function OIsConst(V: Integer): Boolean; inline; begin Result := ON[V].Op = onConst; end;

{ a double constant node's bits into xmm X }
procedure OConstX(X, V: Integer);
begin
  if ON[V].Imm = 0 then OSse($66, $57, X, X)
  else begin MovRI(jAX, QWord(ON[V].Imm)); MovqXR(X, jAX); end;
end;

{ V (int or bool) into GPR R (32 bits) }
procedure OLoadG(R, V: Integer);
begin
  if OIsConst(V) then begin MovRI(R, QWord(Cardinal(Int32(ON[V].Imm)))); Exit; end;
  if OInReg(V) then begin if ON[V].Loc <> R then begin JRex(False, ON[V].Loc, 0, R); JB($89); JModRR(ON[V].Loc, R); end; end
  else Mov32RM(R, jSP, OSlot(V));
end;

{ GPR R (32 bits) into V's place }
procedure OStoreG(V, R: Integer);
begin
  if OInReg(V) then begin if ON[V].Loc <> R then begin JRex(False, R, 0, ON[V].Loc); JB($89); JModRR(R, ON[V].Loc); end; end
  else Mov32MR(jSP, OSlot(V), R);
end;

procedure OLoadX(X, V: Integer);
begin
  if OIsConst(V) then begin OConstX(X, V); Exit; end;
  if OInReg(V) then begin if ON[V].Loc <> X then OSse($F2, $10, X, ON[V].Loc); end
  else OSseM($F2, $10, X, jSP, OSlot(V));
end;

procedure OStoreX(V, X: Integer);
begin
  if OInReg(V) then begin if ON[V].Loc <> X then OSse($F2, $10, ON[V].Loc, X); end
  else OSseM($F2, $11, X, jSP, OSlot(V));
end;

{ a jump to guard G's cold stub (Fb its feedback bits; 0 the guard's own) }
procedure OJccGuardFb(CC, G, Fb: Integer);
begin
  if OGJCount >= Length(OGJ) then SetLength(OGJ, OGJCount * 2 + 64);
  if CC < 0 then JB($E9) else begin JB($0F); JB($80 or CC); end;
  OGJ[OGJCount].G := G; OGJ[OGJCount].At := JitLen; OGJ[OGJCount].Fb := Fb;
  Inc(OGJCount);
  JD(0);
end;

procedure OJccGuard(CC, G: Integer);
begin OJccGuardFb(CC, G, 0); end;

procedure OJmpGuard(G: Integer);
begin OJccGuardFb(-1, G, 0); end;

procedure OJmpBlock(CC, B: Integer);   { CC -1: jmp }
begin
  if OBranchFixCount >= Length(OBranchFix) then SetLength(OBranchFix, OBranchFixCount * 2 + 64);
  if CC < 0 then JB($E9) else begin JB($0F); JB($80 or CC); end;
  OBranchFix[OBranchFixCount].At := JitLen; OBranchFix[OBranchFixCount].Blk := B;
  Inc(OBranchFixCount);
  JD(0);
end;

{ setcc al; movzx eax, al }
procedure OSetCC(CC: Integer);
begin JB($0F); JB($90 or CC); JB($C0); JB($0F); JB($B6); JB($C0); end;

{ ucomisd for condition CC on X, Y (xmm0, xmm1): the flags' condition for "holds" (unordered: does not) }
function OUcomi(CC: Integer): Integer;
begin
  case CC of
    ocLt: begin OSse($66, $2E, 1, 0); Result := ccA; end;
    ocLe: begin OSse($66, $2E, 1, 0); Result := ccAE; end;
    ocGt: begin OSse($66, $2E, 0, 1); Result := ccA; end;
    ocGe: begin OSse($66, $2E, 0, 1); Result := ccAE; end;
  else begin OSse($66, $2E, 0, 1); Result := ccE; end;   { eq / ne: ZF, with PF for unordered }
  end;
end;

function OIntCC(CC: Integer): Integer;
begin
  case CC of
    ocLt: Result := ccL; ocLe: Result := ccLE; ocGt: Result := ccG; ocGe: Result := ccGE; ocEq: Result := ccE;
  else Result := ccNE;
  end;
end;

{ a binary int op (Op the r32, r/m32 opcode: 03 add, 2B sub, 23 and, 0B or, 33 xor, AF imul) computed in N's
  register when it has one (else eax), its second operand a register or its spill slot; that register }
function OBinI(N: Integer; Op: Byte; Commutes: Boolean): Integer;
var X, Y, R, T: Integer;
begin
  X := ON[N].X; Y := ON[N].Y;
  if OInReg(N) then R := ON[N].Loc else R := jAX;
  if OInReg(Y) and (ON[Y].Loc = R) and (X <> Y) then
  begin
    if Commutes then begin T := X; X := Y; Y := T; end else R := jAX;
  end;
  if OIsConst(Y) and (R <> jCX) then
  begin
    OLoadG(R, X);
    OLoadG(jCX, Y);
    JRex(False, R, 0, jCX);
    if Op = $AF then JB($0F);
    JB(Op);
    JModRR(R, jCX);
    Exit(R);
  end;
  OLoadG(R, X);
  if OInReg(Y) then
  begin
    JRex(False, R, 0, ON[Y].Loc);
    if Op = $AF then JB($0F);
    JB(Op);
    JModRR(R, ON[Y].Loc);
  end
  else
  begin
    JRex(False, R, 0, jSP);
    if Op = $AF then JB($0F);
    JB(Op);
    JMem(R, jSP, OSlot(Y));
  end;
  Result := R;
end;

{ a binary double op (F2 0F Op: 58 add, 5C sub, 59 mul, 5E div) computed in N's register when it has one (else
  xmm0); that register }
function OBinX(N: Integer; Op: Byte; Commutes: Boolean): Integer;
var X, Y, R, T: Integer;
begin
  X := ON[N].X; Y := ON[N].Y;
  if OInReg(N) then R := ON[N].Loc else R := 0;
  if OInReg(Y) and (ON[Y].Loc = R) and (X <> Y) then
  begin
    if Commutes then begin T := X; X := Y; Y := T; end else R := 0;
  end;
  if OIsConst(Y) then
  begin
    OLoadX(R, X);
    OConstX(1, Y);
    OSse($F2, Op, R, 1);
    Exit(R);
  end;
  OLoadX(R, X);
  if OInReg(Y) then OSse($F2, Op, R, ON[Y].Loc) else OSseM($F2, Op, R, jSP, OSlot(Y));
  Result := R;
end;

procedure OBox(V: Integer); forward;

{ node N's code }
procedure OCode(N: Integer);
var L, CC: Integer; D: Int64;
begin
  case ON[N].Op of
    onConst: ;   { materialized where it is used }
    onEntry:
      begin
        MovRM(jAX, jBX, Integer(ON[N].Imm) * 8);
        case ON[N].Kind of
          vkInt:
            begin
              TagOf(jDX, jAX);
              AluRI(7, jDX, TAG_INT_HI, False);
              OJccGuard(ccNE, N);
              OStoreG(N, jAX);
            end;
          vkDbl:
            begin
              TagOf(jDX, jAX);
              AluRI(7, jDX, TAG_INT_HI, False);
              L := JccFwd(ccNE);
              Cvt32ToX(0, jAX);
              CC := JmpFwd;
              PatchHere(L);
              MovRI(jCX, DOUBLE_LIMIT);
              AluRR($39, jAX, jCX, True);
              OJccGuard(ccAE, N);
              MovqXR(0, jAX);
              PatchHere(CC);
              OStoreX(N, 0);
            end;
        else
          begin
            { true 1, false 0, anything else the deopt }
            MovRI(jCX, VTrue);
            AluRR($39, jAX, jCX, True);
            L := JccFwd(ccE);
            MovRI(jCX, VFalse);
            AluRR($39, jAX, jCX, True);
            OJccGuard(ccNE, N);
            AluRR($31, jAX, jAX, False);
            CC := JmpFwd;
            PatchHere(L);
            MovRI(jAX, 1);
            PatchHere(CC);
            OStoreG(N, jAX);
          end;
        end;
      end;
    onAddI, onSubI, onAndI, onOrI, onXorI, onMulI:
      begin
        case ON[N].Op of
          onAddI: CC := $03; onSubI: CC := $2B; onAndI: CC := $23; onOrI: CC := $0B; onXorI: CC := $33;
        else CC := $AF;
        end;
        L := OBinI(N, CC, ON[N].Op in [onAddI, onAndI, onOrI, onXorI, onMulI]);
        if ON[N].Op in [onAddI, onSubI, onMulI] then OJccGuard(ccO, N);
        if ON[N].Op = onMulI then
        begin
          { a zero product of a negative operand is -0 }
          AluRR($85, L, L, False);
          CC := JccFwd(ccNE);
          OLoadG(jDX, ON[N].X);
          OLoadG(jCX, ON[N].Y);
          AluRR($09, jDX, jCX, False);
          OJccGuard(ccS, N);
          PatchHere(CC);
        end;
        if L = jAX then OStoreG(N, jAX);
      end;
    onModI:
      begin
        { a non-negative dividend and a positive divisor (otherwise -0, NaN or the slow cases: the deopt) }
        OLoadG(jAX, ON[N].X);
        OLoadG(jCX, ON[N].Y);
        AluRR($85, jAX, jAX, False);
        OJccGuard(ccS, N);
        AluRR($85, jCX, jCX, False);
        OJccGuard(ccLE, N);
        AluRR($31, jDX, jDX, False);
        JB($F7); JB($F1);                       { div ecx }
        OStoreG(N, jDX);
      end;
    onNegI:
      begin
        OLoadG(jAX, ON[N].X);
        AluRR($85, jAX, jAX, False);
        OJccGuard(ccE, N);                      { -0 }
        JB($F7); JB($D8);                       { neg eax }
        OJccGuard(ccO, N);
        OStoreG(N, jAX);
      end;
    onNotI:
      begin
        OLoadG(jAX, ON[N].X);
        JB($F7); JB($D0);                       { not eax }
        OStoreG(N, jAX);
      end;
    onShlI, onShrI, onUShrI:
      begin
        OLoadG(jAX, ON[N].X);
        OLoadG(jCX, ON[N].Y);
        JB($D3);
        case ON[N].Op of
          onShlI: JB($E0);
          onShrI: JB($F8);
        else JB($E8);
        end;
        if ON[N].Op = onUShrI then
        begin
          AluRR($85, jAX, jAX, False);
          OJccGuard(ccS, N);                    { 2^31 and up: not an int32 }
        end;
        OStoreG(N, jAX);
      end;
    onAddD, onSubD, onMulD, onDivD:
      begin
        case ON[N].Op of
          onAddD: CC := $58; onSubD: CC := $5C; onMulD: CC := $59;
        else CC := $5E;
        end;
        L := OBinX(N, CC, ON[N].Op in [onAddD, onMulD]);
        if L = 0 then OStoreX(N, 0);
      end;
    onNegD:
      begin
        OLoadX(0, ON[N].X);
        MovRI(jAX, QWord($8000000000000000));
        MovqXR(1, jAX);
        OSse($66, $57, 0, 1);                   { xorpd }
        OStoreX(N, 0);
      end;
    onI2D:
      begin
        OLoadG(jAX, ON[N].X);
        Cvt32ToX(0, jAX);
        OStoreX(N, 0);
      end;
    onD2I:
      begin
        { ToInt32: the truncation's low 32 bits; past 2^63 (cvttsd2si's indefinite value) the deopt }
        OLoadX(0, ON[N].X);
        CvtTX64(jAX, 0);
        MovRI(jCX, QWord($8000000000000000));
        AluRR($39, jAX, jCX, True);
        OJccGuard(ccE, N);
        OStoreG(N, jAX);
      end;
    onCmpI:
      begin
        OLoadG(jCX, ON[N].X);
        OLoadG(jDX, ON[N].Y);
        AluRR($39, jCX, jDX, False);
        OSetCC(OIntCC(ON[N].Imm));
        OStoreG(N, jAX);
      end;
    onCmpD:
      begin
        OLoadX(0, ON[N].X);
        OLoadX(1, ON[N].Y);
        CC := OUcomi(ON[N].Imm);
        if ON[N].Imm = ocEq then
        begin
          JB($0F); JB($94); JB($C0);           { sete al }
          JB($0F); JB($9B); JB($C1);           { setnp cl }
          JB($20); JB($C8);                    { and al, cl }
          JB($0F); JB($B6); JB($C0);           { movzx eax, al }
        end
        else if ON[N].Imm = ocNe then
        begin
          JB($0F); JB($95); JB($C0);           { setne al }
          JB($0F); JB($9A); JB($C1);           { setp cl }
          JB($08); JB($C8);                    { or al, cl }
          JB($0F); JB($B6); JB($C0);
        end
        else OSetCC(CC);
        OStoreG(N, jAX);
      end;
    onNotB:
      begin
        OLoadG(jAX, ON[N].X);
        AluRI(6, jAX, 1, False);                { xor eax, 1 }
        OStoreG(N, jAX);
      end;
    onTruthyI:
      begin
        OLoadG(jCX, ON[N].X);
        AluRR($85, jCX, jCX, False);
        OSetCC(ccNE);
        OStoreG(N, jAX);
      end;
    onTruthyD:
      begin
        OLoadX(0, ON[N].X);
        OSse($66, $57, 1, 1);                   { xorpd xmm1, xmm1 }
        OSse($66, $2E, 0, 1);                   { ucomisd: equal or unordered is falsy }
        OSetCC(ccNE);
        OStoreG(N, jAX);
      end;
    onStoreTag:
      begin
        MovRI(jAX, QWord(ON[N].Imm));
        MovMR(jBX, ON[N].X * 8, jAX);
      end;
    onFlush:
      begin
        OBox(ON[N].X);
        MovMR(jBX, Integer(ON[N].Imm) * 8, jAX);
      end;
    onUnbox:
      begin
        MovRM(jAX, jBX, Integer(ON[N].Imm) * 8);
        TagOf(jDX, jAX);
        AluRI(7, jDX, TAG_INT_HI, False);
        if ON[N].Kind = vkInt then
        begin
          L := JccFwd(ccE);
          { not an int: a double asks for doubles next time, anything else for no speculation }
          MovRI(jCX, DOUBLE_LIMIT);
          AluRR($39, jAX, jCX, True);
          OJccGuardFb(ccB, N, ofbUnboxDbl);
          OJccGuardFb(-1, N, ofbUnboxNo);
          PatchHere(L);
          OStoreG(N, jAX);
        end
        else
        begin
          L := JccFwd(ccNE);
          Cvt32ToX(0, jAX);
          CC := JmpFwd;
          PatchHere(L);
          MovRI(jCX, DOUBLE_LIMIT);
          AluRR($39, jAX, jCX, True);
          OJccGuardFb(ccAE, N, ofbUnboxNo);
          MovqXR(0, jAX);
          PatchHere(CC);
          OStoreX(N, 0);
        end;
      end;
    onEmbed:
      begin
        for L := ON[N].X to ON[N].X + ON[N].Y - 1 do
        begin
          OBox(OFS[L].Node);
          MovMR(jBX, OFS[L].Reg * 8, jAX);
        end;
        JitInstr(OT, ON[N].PC, OIns(ON[N].PC));
        { the continuation: where its slow blocks come back to (JmpPC to the next instruction) }
        JitNative[ON[N].PC + 1] := JitLen;
      end;
    onLoadKTag:
      begin
        MovRI(jAX, QWord(PtrUInt(@OT^.Consts[ON[N].Imm])));
        MovRM(jAX, jAX, 0);
        MovMR(jBX, ON[N].X * 8, jAX);
      end;
    onCopyTag:
      begin
        MovRM(jAX, jBX, ON[N].X * 8);
        MovMR(jBX, ON[N].Y * 8, jAX);
      end;
  end;
end;

{ ---- the moves into a successor's phis: a parallel move (a cycle broken by parking the blocked destination's value
  in rax or xmm0) ---- }

{ a location's key: a GPR its number, an XMM register 50 + its number, a spill slot 100 + its index }
function OKey(Loc, Kind: Integer): Integer; inline;
begin
  if Loc >= 100 then Result := Loc else if Kind = vkDbl then Result := 50 + Loc else Result := Loc;
end;

{ a move between keys (of one class) }
procedure OMoveKey(FromK, ToK: Integer; Dbl: Boolean);
begin
  if FromK = ToK then Exit;
  if Dbl then
  begin
    if FromK >= 100 then
    begin
      if ToK >= 100 then begin OSseM($F2, $10, 1, jSP, (FromK - 100) * 8); OSseM($F2, $11, 1, jSP, (ToK - 100) * 8); end
      else OSseM($F2, $10, ToK - 50, jSP, (FromK - 100) * 8);
    end
    else if ToK >= 100 then OSseM($F2, $11, FromK - 50, jSP, (ToK - 100) * 8)
    else OSse($F2, $10, ToK - 50, FromK - 50);
  end
  else
  begin
    if FromK >= 100 then
    begin
      if ToK >= 100 then begin MovRM(jCX, jSP, (FromK - 100) * 8); MovMR(jSP, (ToK - 100) * 8, jCX); end
      else MovRM(ToK, jSP, (FromK - 100) * 8);
    end
    else if ToK >= 100 then MovMR(jSP, (ToK - 100) * 8, FromK)
    else MovRR(ToK, FromK);
  end;
end;

procedure OEdgeMoves(P, S: Integer);
var Src, Dst: array of Integer; Dbl, Done: array of Boolean; NM, K, J, Q, PIdx, Ph, Tmp: Integer;
  Progress, Blocked: Boolean;
begin
  if Length(OB[S].Phis) = 0 then Exit;
  PIdx := -1;
  for K := 0 to High(OB[S].Preds) do if OB[S].Preds[K] = P then begin PIdx := K; Break; end;
  if PIdx < 0 then Exit;
  NM := Length(OB[S].Phis);
  SetLength(Src, NM); SetLength(Dst, NM); SetLength(Dbl, NM); SetLength(Done, NM);
  NM := 0;
  for Ph := 0 to High(OB[S].Phis) do
  begin
    Q := OB[S].Phis[Ph];
    J := OPhiArg[ON[Q].PhiArgs + PIdx];
    if OIsConst(J) then Continue;
    Src[NM] := OKey(ON[J].Loc, ON[J].Kind);
    Dst[NM] := OKey(ON[Q].Loc, ON[Q].Kind);
    Dbl[NM] := ON[Q].Kind = vkDbl;
    Done[NM] := Src[NM] = Dst[NM];
    Inc(NM);
  end;
  repeat
    Progress := False;
    Blocked := False;
    for K := 0 to NM - 1 do
      if not Done[K] then
      begin
        Q := 0;
        for J := 0 to NM - 1 do
          if not Done[J] and (J <> K) and (Src[J] = Dst[K]) then begin Q := 1; Break; end;
        if Q = 0 then
        begin
          OMoveKey(Src[K], Dst[K], Dbl[K]);
          Done[K] := True;
          Progress := True;
        end
        else Blocked := True;
      end;
    if not Progress and Blocked then
    begin
      { a cycle: the first pending move's destination parked in the scratch register, its readers redirected }
      for K := 0 to NM - 1 do
        if not Done[K] then
        begin
          if Dbl[K] then Tmp := 50 else Tmp := jAX;
          OMoveKey(Dst[K], Tmp, Dbl[K]);
          for J := 0 to NM - 1 do
            if not Done[J] and (Src[J] = Dst[K]) then Src[J] := Tmp;
          Break;
        end;
      Progress := True;
    end;
  until not Progress;
  { the constants: their destinations are free now }
  for Ph := 0 to High(OB[S].Phis) do
  begin
    Q := OB[S].Phis[Ph];
    J := OPhiArg[ON[Q].PhiArgs + PIdx];
    if not OIsConst(J) then Continue;
    if ON[Q].Kind = vkDbl then
    begin
      if OInReg(Q) then OConstX(ON[Q].Loc, J) else begin OConstX(0, J); OStoreX(Q, 0); end;
    end
    else if OInReg(Q) then OLoadG(ON[Q].Loc, J)
    else begin OLoadG(jAX, J); MovMR(jSP, OSlot(Q), jAX); end;
  end;
end;

{ ---- boxing (the cold stubs): node V as a TValue in rax (MkNumber's int for an integral double, not -0) ---- }

procedure OBox(V: Integer);
var L1, L2, L3, L4: Integer;
begin
  case ON[V].Kind of
    vkInt:
      begin
        OLoadG(jAX, V);
        MovRI(jCX, TAG_INT);
        AluRR($09, jAX, jCX, True);
      end;
    vkBool:
      begin
        OLoadG(jCX, V);
        MovRI(jAX, VFalse);
        AluRR($85, jCX, jCX, False);
        L1 := JccFwd(ccE);
        MovRI(jAX, VTrue);
        PatchHere(L1);
      end;
  else
    begin
      OLoadX(0, V);
      CvtTX32(jCX, 0);
      Cvt32ToX(1, jCX);
      OSse($66, $2E, 0, 1);
      L1 := JccFwd(ccNE);
      L2 := JccFwd(ccP);
      AluRR($85, jCX, jCX, False);
      L3 := JccFwd(ccNE);
      MovqRX(jAX, 0);
      AluRR($85, jAX, jAX, True);
      L4 := JccFwd(ccS);                       { -0 stays a double }
      PatchHere(L3);
      JRex(False, jAX, 0, jCX); JB($8B); JModRR(jAX, jCX);
      MovRI(jCX, TAG_INT);
      AluRR($09, jAX, jCX, True);
      L3 := JmpFwd;
      PatchHere(L1); PatchHere(L2); PatchHere(L4);
      MovqRX(jAX, 0);
      MovRI(jCX, QWord($FFF8000000000000));
      AluRR($39, jAX, jCX, True);
      L1 := JccFwd(ccNE);
      MovRI(jAX, CANON_NAN);
      PatchHere(L1);
      PatchHere(L3);
    end;
  end;
end;

{ the frame state written back, the stack restored, then the baseline code at PC (the interpreter when it has none);
  Feedback the bits recorded for PC (0 none: a planned exit) }
procedure OLeave(FS, FSN, PC, Feedback, EpiAt: Integer);
var K: Integer; Base: PtrUInt;
begin
  for K := FS to FS + FSN - 1 do
  begin
    OBox(OFS[K].Node);
    MovMR(jBX, OFS[K].Reg * 8, jAX);
  end;
  if Feedback <> 0 then
  begin
    MovRI(jAX, QWord(PtrUInt(@OInfo^.Feedback[PC])));
    JB($80); JB($08); JB(Byte(Feedback));                 { or byte [rax], Feedback }
    MovRI(jAX, QWord(PtrUInt(@OInfo^.Deopts)));
    JB($FF); JB($00);                                     { inc dword [rax] }
  end;
  AluRI(0, jSP, OFrame, True);
  JB($41); JB($5F);                                       { pop r15 }
  Base := PPtrUInt(OInfo^.BaseTable)[PC];
  if Base <> 0 then
  begin
    MovRI(jAX, QWord(Base));
    JB($FF); JB($E0);                                     { jmp rax }
  end
  else
  begin
    MovRI(jAX, QWord(PC));
    JB($E9); JD(Cardinal(EpiAt - (JitLen + 4)));
  end;
end;

{ block B's tagged register's truthiness (ToBool) into eax 1 / 0: the booleans, undefined, null, ints, objects (not
  [[IsHTMLDDA]]), strings and doubles here; anything else the deopt }
procedure OTagTruthy(B: Integer);
var G: Integer; LT: array[0..5] of Integer; LF: array[0..6] of Integer; NT, NF, LNI, LNP, LNO, LE, K: Integer;
  procedure Cmp(V: TValue); begin MovRI(jCX, V); AluRR($39, jAX, jCX, True); end;
begin
  G := OB[B].TermGuard;
  NT := 0; NF := 0;
  MovRM(jAX, jBX, OB[B].TermX * 8);
  Cmp(VTrue); LT[NT] := JccFwd(ccE); Inc(NT);
  Cmp(VFalse); LF[NF] := JccFwd(ccE); Inc(NF);
  Cmp(VUndefined); LF[NF] := JccFwd(ccE); Inc(NF);
  Cmp(VNull); LF[NF] := JccFwd(ccE); Inc(NF);
  TagOf(jDX, jAX);
  AluRI(7, jDX, TAG_INT_HI, False);
  LNI := JccFwd(ccNE);
  AluRR($85, jAX, jAX, False);
  LT[NT] := JccFwd(ccNE); Inc(NT);
  LF[NF] := JmpFwd; Inc(NF);
  PatchHere(LNI);
  AluRI(7, jDX, TAG_PTR_HI, False);
  LNP := JccFwd(ccNE);
  MovRR(jCX, jAX);
  AluRR($31, jCX, jR13, True);
  CmpBMI(jCX, 4, ckObject);
  LNO := JccFwd(ccNE);
  TestBMI(jCX, 5, cfEmulatesUndefined);
  OJccGuardFb(ccNE, G, ofbTagCmp);
  LT[NT] := JmpFwd; Inc(NT);
  PatchHere(LNO);
  CmpBMI(jCX, 4, ckString);
  OJccGuardFb(ccNE, G, ofbTagCmp);
  Mov32RM(jDX, jCX, PtrInt(@PStr(nil)^.Len));
  AluRR($85, jDX, jDX, False);
  LT[NT] := JccFwd(ccNE); Inc(NT);
  LF[NF] := JmpFwd; Inc(NF);
  PatchHere(LNP);
  MovRI(jCX, DOUBLE_LIMIT);
  AluRR($39, jAX, jCX, True);
  OJccGuardFb(ccAE, G, ofbTagCmp);
  MovqXR(0, jAX);
  OSse($66, $57, 1, 1);
  OSse($66, $2E, 0, 1);
  LF[NF] := JccFwd(ccE); Inc(NF);
  LT[NT] := JmpFwd; Inc(NT);
  for K := 0 to NT - 1 do PatchHere(LT[K]);
  MovRI(jAX, 1);
  LE := JmpFwd;
  for K := 0 to NF - 1 do PatchHere(LF[K]);
  AluRR($31, jAX, jAX, False);
  PatchHere(LE);
  AluRR($85, jAX, jAX, False);
end;

{ block B's strict equality with a tagged operand into ecx 1 / 0 (then tested): the same bits are equal (but
  doubles: the deopt), two objects of different bits are not, a pointer against a non-pointer is not; two other
  pointers (strings, BigInts) and a double the deopt }
procedure OTagCmp(B: Integer);
var G, LNe, LDone, L1, L2: Integer;
  procedure Load(V: Integer);
  begin
    if V >= 0 then OBox(V) else MovRM(jAX, jBX, (-1 - V) * 8);
  end;
begin
  G := OB[B].TermGuard;
  Load(OB[B].TermX);
  MovRR(jDX, jAX);
  Load(OB[B].TermY);
  MovRI(jCX, DOUBLE_LIMIT);
  AluRR($39, jDX, jCX, True);
  OJccGuardFb(ccB, G, ofbTagCmp);
  AluRR($39, jAX, jCX, True);
  OJccGuardFb(ccB, G, ofbTagCmp);
  AluRR($39, jDX, jAX, True);
  LNe := JccFwd(ccNE);
  MovRI(jCX, 1);
  LDone := JmpFwd;
  PatchHere(LNe);
  { different bits: two pointers are equal only as the same string or BigInt (the deopt), objects not }
  MovRR(jCX, jDX);
  ShiftRI(5, jCX, 48, True);
  AluRI(7, jCX, TAG_PTR_HI, False);
  L1 := JccFwd(ccNE);
  MovRR(jCX, jAX);
  ShiftRI(5, jCX, 48, True);
  AluRI(7, jCX, TAG_PTR_HI, False);
  L2 := JccFwd(ccNE);
  AluRR($31, jDX, jR13, True);
  AluRR($31, jAX, jR13, True);
  CmpBMI(jDX, 4, ckObject);
  OJccGuardFb(ccNE, G, ofbTagCmp);
  CmpBMI(jAX, 4, ckObject);
  OJccGuardFb(ccNE, G, ofbTagCmp);
  PatchHere(L1); PatchHere(L2);
  AluRR($31, jCX, jCX, False);
  PatchHere(LDone);
  AluRR($85, jCX, jCX, False);
end;

{ G2_OPTDUMP=DIR (with G2_OPTLOG=1): each region's code in DIR/opt-ADDRESS.bin, for objdump -b binary }
procedure OptDump(Code: Pointer);
var F: File; Name: string;
begin
  Name := GetEnvironmentVariable('G2_OPTDUMP') + '/opt-' + IntToHex(PtrUInt(Code), 12) + '.bin';
  AssignFile(F, Name);
  Rewrite(F, 1);
  BlockWrite(F, JitBuf[0], JitLen);
  CloseFile(F);
  WriteLn(ErrOutput, 'opt dump ', Name, ' ', JitLen);
end;

{ the region's code; nil when it cannot be made }
function OEmit: Pointer;
var K, J, B, Epi, RestEpi, ThrowL, G, F: Integer; Labels: array of Integer; GuardAt, ExitAt: array of Integer;
  Code: PByte;
  D: Cardinal; Neg: Boolean; CC, T: Integer;
begin
  Result := nil;
  JitLen := 0;
  OGJCount := 0;
  OBranchFixCount := 0;
  SetLength(Labels, OBCount);
  { the baseline emitter's state, for the embedded instructions }
  JitFixCount := 0;
  JitDeferCount := 0;
  JitDeferJumpCount := 0;
  JitSlow := nil;
  JitSlowJump := -1;
  JitSlowCount := 0;
  JitTAEndCount := 0;
  JitInfo := nil;
  SetLength(JitNative, OT^.CodeLen + 1);
  for K := 0 to OT^.CodeLen do JitNative[K] := -1;
  { the entry: r15 saved, the spill frame }
  JB($41); JB($57);                                       { push r15 }
  AluRI(5, jSP, OFrame, True);
  for K := 0 to OOrderCount - 1 do
  begin
    B := OOrder[K];
    Labels[B] := JitLen;
    for J := 0 to High(OB[B].Nodes) do OCode(OB[B].Nodes[J]);
    case OB[B].Term of
      otJmp:
        begin
          OEdgeMoves(B, OB[B].Succ[0]);
          if not ((K + 1 < OOrderCount) and (OOrder[K + 1] = OB[B].Succ[0])) then OJmpBlock(-1, OB[B].Succ[0]);
        end;
      otExit: OJmpGuard(-1 - B);
      otBr:
        begin
          Neg := OB[B].TermImm <> 0;
          { the test's flags: CC holds when the test does }
          case OB[B].TermTest of
            otbCmpI:
              begin
                OLoadG(jCX, OB[B].TermX); OLoadG(jDX, OB[B].TermY);
                AluRR($39, jCX, jDX, False);
                CC := OIntCC(OB[B].TermCC);
              end;
            otbCmpD:
              begin
                OLoadX(0, OB[B].TermX); OLoadX(1, OB[B].TermY);
                CC := OUcomi(OB[B].TermCC);
                if (OB[B].TermCC = ocEq) then
                begin
                  { equal and ordered: setcc into al }
                  JB($0F); JB($94); JB($C0); JB($0F); JB($9B); JB($C1); JB($20); JB($C8);
                  JB($84); JB($C0);                       { test al, al }
                  CC := ccNE;
                end;
              end;
            otbTruthyI, otbTruthyB:
              begin
                OLoadG(jCX, OB[B].TermX);
                AluRR($85, jCX, jCX, False);
                CC := ccNE;
              end;
            otbTruthyD:
              begin
                OLoadX(0, OB[B].TermX);
                OSse($66, $57, 1, 1);
                OSse($66, $2E, 0, 1);
                CC := ccNE;
              end;
            otbTruthyTag:
              begin
                OTagTruthy(B);
                CC := ccNE;
              end;
            otbTagCmp:
              begin
                OTagCmp(B);
                CC := ccNE;
              end;
          else
            begin
              { a tagged register's undefined / null test }
              MovRM(jAX, jBX, OB[B].TermX * 8);
              case TOp(OB[B].TermCC) of
                opJUndef, opJNotUndef:
                  begin
                    MovRI(jCX, VUndefined);
                    AluRR($39, jAX, jCX, True);
                    CC := ccE;
                    Neg := TOp(OB[B].TermCC) = opJNotUndef;
                  end;
              else
                begin
                  { undefined or null: x or 1 = null (as the baseline tests) }
                  MovRI(jCX, VNull);
                  AluRI(1, jAX, 1, True);
                  AluRR($39, jAX, jCX, True);
                  CC := ccE;
                  Neg := TOp(OB[B].TermCC) = opJNotNullish;
                end;
              end;
            end;
          end;
          if Neg then CC := CC xor 1;
          { Succ[0] when CC holds, else Succ[1]; each edge's phi moves on its own path }
          if (Length(OB[OB[B].Succ[0]].Phis) = 0) then
          begin
            OJmpBlock(CC, OB[B].Succ[0]);
            OEdgeMoves(B, OB[B].Succ[1]);
            if not ((K + 1 < OOrderCount) and (OOrder[K + 1] = OB[B].Succ[1])) then OJmpBlock(-1, OB[B].Succ[1]);
          end
          else
          begin
            T := JccFwd(CC xor 1);
            OEdgeMoves(B, OB[B].Succ[0]);
            OJmpBlock(-1, OB[B].Succ[0]);
            PatchHere(T);
            OEdgeMoves(B, OB[B].Succ[1]);
            OJmpBlock(-1, OB[B].Succ[1]);
          end;
        end;
    end;
  end;
  { the embedded instructions' slow blocks }
  JitEmitDeferred(OT);
  { the epilogue (an exit to the interpreter: eax its PC) }
  Epi := JitLen;
  JB($41); JB($5E); JB($41); JB($5D); JB($5D); JB($41); JB($5C); JB($5B); JB($C3);
  { the embedded code's epilogue (eax its result) and throw exit: the frame and r15 first }
  RestEpi := JitLen;
  AluRI(0, jSP, OFrame, True);
  JB($41); JB($5F);
  JB($E9); JD(Cardinal(Epi - (JitLen + 4)));
  ThrowL := JitLen;
  MovRI(jAX, $FFFFFFFF);
  JB($E9); JD(Cardinal(RestEpi - (JitLen + 4)));
  { the cold stubs: one per guard node and per exit block }
  SetLength(GuardAt, ONCount + OBCount);
  for K := 0 to High(GuardAt) do GuardAt[K] := -1;
  for K := 0 to OGJCount - 1 do
  begin
    G := OGJ[K].G;
    if G >= 0 then J := G else J := ONCount + (-1 - G);
    if OGJ[K].Fb <> 0 then
    begin
      { a stub of its own: this jump's feedback }
      D := Cardinal(JitLen - (OGJ[K].At + 4));
      PCardinal(@JitBuf[OGJ[K].At])^ := D;
      OLeave(ON[G].FS, ON[G].FSN, ON[G].PC, OGJ[K].Fb, Epi);
      Continue;
    end;
    if GuardAt[J] < 0 then
    begin
      GuardAt[J] := JitLen;
      if G >= 0 then
      begin
        if ON[G].Op = onEntry then F := ofbEntry else F := ofbOverflow;
        OLeave(ON[G].FS, ON[G].FSN, ON[G].PC, F, Epi);
      end
      else
      begin
        B := -1 - G;
        OLeave(OB[B].TermFS, OB[B].TermFSN, OB[B].TermPC, 0, Epi);
      end;
    end;
    D := Cardinal(GuardAt[J] - (OGJ[K].At + 4));
    PCardinal(@JitBuf[OGJ[K].At])^ := D;
  end;
  { the embedded code's jumps: its exits (the frame state at the instruction written back), its epilogue and
    throw exits, its slow blocks' way back }
  SetLength(ExitAt, OT^.CodeLen + 1);
  for K := 0 to OT^.CodeLen do ExitAt[K] := -1;
  for K := 0 to JitFixCount - 1 do
  begin
    T := JitFixes[K].Target;
    if T = JIT_EPILOGUE then J := RestEpi
    else if T = JIT_THROW then J := ThrowL
    else if T < 0 then
    begin
      F := -1 - T;
      if (F > OT^.CodeLen) or (OEmbedNode[F] < 0) then Exit;
      if ExitAt[F] < 0 then
      begin
        ExitAt[F] := JitLen;
        G := OEmbedNode[F];
        OLeave(ON[G].FS, ON[G].FSN, F, 0, Epi);
      end;
      J := ExitAt[F];
    end
    else
    begin
      if (T > OT^.CodeLen) or (JitNative[T] < 0) then Exit;
      J := JitNative[T];
    end;
    D := Cardinal(J - (JitFixes[K].At + 4));
    PCardinal(@JitBuf[JitFixes[K].At])^ := D;
  end;
  for K := 0 to OBranchFixCount - 1 do
  begin
    D := Cardinal(Labels[OBranchFix[K].Blk] - (OBranchFix[K].At + 4));
    PCardinal(@JitBuf[OBranchFix[K].At])^ := D;
  end;
  Code := JitAlloc(JitLen);
  if Code = nil then Exit;
  Move(JitBuf[0], Code^, JitLen);
  Result := Code;
  if OptLog and (GetEnvironmentVariable('G2_OPTDUMP') <> '') then OptDump(Code);
end;

{ a first estimate before any analysis (most regions fail it): the instructions reachable from PC, the numeric
  ones (arithmetic, comparisons, numeric branches) at least as many as the others that do work }
function OQuickProfitable(PC: Integer): Boolean;
var Seen: array of Boolean; Work: array of Integer; NW, P, T, Num, Other: Integer; I: QWord; Op: TOp;
begin
  SetLength(Seen, OT^.CodeLen + 1);
  SetLength(Work, OT^.CodeLen + 1);
  NW := 0; Work[NW] := PC; Inc(NW);
  Num := 0; Other := 0;
  while NW > 0 do
  begin
    Dec(NW);
    P := Work[NW];
    while (P < OT^.CodeLen) and not Seen[P] do
    begin
      Seen[P] := True;
      I := OIns(P);
      Op := OOpOf(I);
      case Op of
        opAdd, opSub, opMul, opDiv, opMod, opBitAnd, opBitOr, opBitXor, opShl, opShr, opUShr, opAddI, opSubI, opIncI,
        opBitAndI, opBitOrI, opBitXorI, opShlI, opShrI, opUShrI, opNeg, opBitNot, opLt, opLe, opGt, opGe,
        opJNLt, opJNLe, opJNGt, opJNGe: Inc(Num);
        opNop, opMov, opLoadK, opLoadI, opLoadUndef, opLoadNull, opLoadTrue, opLoadFalse, opLoadHole, opJmp, opJT,
        opJF, opJNullish, opJNotNullish, opJUndef, opJNotUndef, opJNSEq, opJSEq, opSEq, opSNe, opNot, opRet,
        opRetUndef, opToNum, opToNumeric: ;
      else Inc(Other);
      end;
      if Op in [opRet, opRetUndef, opThrow, opThrowConst] then Break;
      if (Op = opJmp) or OIsCondJump(Op) then
      begin
        T := P + 1 + OC(I);
        if (T >= 0) and (T < OT^.CodeLen) and not Seen[T] then begin Work[NW] := T; Inc(NW); end;
        if Op = opJmp then Break;
      end;
      Inc(P);
    end;
  end;
  Result := (Num > 0) and (Num >= Other);
end;

{ whether any region of T could pass OQuickProfitable (from PC 0 or a loop's header): a template where none can gets
  no counters, so its calls and loops pay nothing for the tier }
function OptPromising(T: PFuncTemplate): Boolean;
var K, Tg: Integer; I: QWord;
begin
  OT := T;
  if T^.CodeLen = 0 then Exit(False);
  if OQuickProfitable(0) then Exit(True);
  for K := 0 to T^.CodeLen - 1 do
  begin
    I := T^.Code[K];
    if OOpOf(I) = opJmp then
    begin
      Tg := K + 1 + OC(I);
      if (Tg >= 0) and (Tg <= K) and OQuickProfitable(Tg) then Exit(True);
    end;
  end;
  Result := False;
end;

{ whether the region gains over the baseline code: its native number work (arithmetic, comparisons, numeric
  branches) at least its embedded instructions and unboxes, which cost about what the baseline's do plus their
  flushes; a region that is mostly the baseline's instructions is left to the baseline }
function OProfitable: Boolean;
var B, K, N, Gain, Cost: Integer;
begin
  Gain := 0; Cost := 0;
  for B := 0 to OBCount - 1 do
  begin
    for K := 0 to High(OB[B].Nodes) do
    begin
      N := OB[B].Nodes[K];
      case ON[N].Op of
        onAddI, onSubI, onMulI, onModI, onNegI, onAndI, onOrI, onXorI, onShlI, onShrI, onUShrI, onNotI,
        onAddD, onSubD, onMulD, onDivD, onNegD, onCmpI, onCmpD, onNotB: Inc(Gain);
        onEmbed, onUnbox, onFlush: Inc(Cost);
      end;
    end;
    if (OB[B].Term = otBr) and (OB[B].TermTest in [otbCmpI, otbCmpD, otbTruthyI, otbTruthyD, otbTruthyB]) then Inc(Gain);
  end;
  Result := (Gain > 0) and (Gain >= Cost);
end;

{ the region entered at PC of frame Fr's template, compiled and installed in its JitTable; False when it cannot be }
function OptCompile(Fr: PFrame; PC: Integer): Boolean;
var Kinds: array of Byte; R, Iter: Integer; V: TValue; Code: Pointer; Tbl: PPtrUInt; K: Integer;
begin
  Result := False;
  OT := Fr^.Tpl;
  OInfo := POptInfo(OT^.Opt);
  ORegs := OT^.MaxRegs;
  OEntry := PC;
  OFail := False;
  ONCount := 0; OFSCount := 0; OPhiArgCount := 0;
  SetLength(OExit, OT^.CodeLen + 1);
  SetLength(OEmbed, OT^.CodeLen + 1);
  SetLength(OEmbedNode, OT^.CodeLen + 1);
  for K := 0 to OT^.CodeLen do begin OExit[K] := False; OEmbed[K] := False; OEmbedNode[K] := -1; end;
  for K := 0 to OT^.CodeLen - 1 do
    if not OStaticOk(OOpOf(OIns(K))) then
    begin
      if OEmbeddable(K) then OEmbed[K] := True else OExit[K] := True;
    end;
  OExit[OT^.CodeLen] := True;
  { the entry's speculation: the frame's values now (a failed entry guard: ints as doubles) }
  SetLength(Kinds, ORegs);
  for R := 0 to ORegs - 1 do
  begin
    V := VStack[Fr^.Base + R];
    if IsInt(V) then
    begin
      if (OInfo^.Feedback[PC] and 2) <> 0 then Kinds[R] := vkDbl else Kinds[R] := vkInt;
    end
    else if V < DOUBLE_LIMIT then Kinds[R] := vkDbl
    else if (V = VTrue) or (V = VFalse) then Kinds[R] := vkBool
    else Kinds[R] := vkTag;
  end;
  if (OInfo^.Feedback[PC] and 4) <> 0 then Exit;
  if not OQuickProfitable(PC) then Exit;
  { PC 0 or a loop's header (a backward jump's target) }
  if PC <> 0 then
  begin
    R := -1;
    for K := PC to OT^.CodeLen - 1 do
      if (OOpOf(OIns(K)) = opJmp) and (K + 1 + OC(OIns(K)) = PC) then begin R := K; Break; end;
    if R < 0 then Exit;
  end;
  Iter := 0;
  repeat
    Inc(Iter);
    if Iter > 12 then Exit;
    OB := nil;
    if not OBuildBlocks then Exit;
  until OInferKinds(Kinds);
  OComputeLiveness;
  { an entry register live but of an unknown kind stays tagged }
  for R := 0 to ORegs - 1 do
    if OB[0].InKinds[R] = vkNone then OB[0].InKinds[R] := vkTag;
  if not OBuildSSA then Exit;
  if not OProfitable then Exit;
  OAllocate;
  Code := OEmit;
  if Code = nil then Exit;
  { installed: the template's table with this entry }
  if OInfo^.Table = nil then
  begin
    Tbl := GetMem(OT^.CodeLen * SizeOf(PtrUInt));
    Move(OInfo^.BaseTable^, Tbl^, OT^.CodeLen * SizeOf(PtrUInt));
    OInfo^.Table := Tbl;
  end;
  OInfo^.Table[PC] := PtrUInt(Code);
  OT^.JitTable := OInfo^.Table;
  Result := True;
end;

procedure OptReadNoEmbed;
var S: string; P, V: Integer;
begin
  S := GetEnvironmentVariable('G2_OPTNOEMBED') + ',';
  while S <> ',' do
  begin
    P := Pos(',', S);
    V := StrToIntDef(Copy(S, 1, P - 1), -1);
    if V = 999 then FillChar(OptNoEmbed, SizeOf(OptNoEmbed), 1)
    else if (V >= 0) and (V < 256) then OptNoEmbed[V] := True;
    Delete(S, 1, P);
    if S = '' then Break;
  end;
end;

function OptName(T: PFuncTemplate): string;
begin
  if T^.Name <> nil then Result := string(StrToUnicode(T^.Name)) else Result := '?';
end;

{ XJitRun's hook: the template's region at PC optimized when its counter ran out (PC 0 or a loop header); too
  many deopts first drop the optimized code }
procedure OptMaybe(Fr: PFrame; PC: Integer); noinline;
var Info: POptInfo; T: PFuncTemplate;
begin
  T := Fr^.Tpl;
  Info := POptInfo(T^.Opt);
  if Info^.Deopts > OPT_MAX_DEOPTS then
  begin
    { the speculation failed too often: back to the baseline, and each region may be tried again (with the
      recorded feedback) }
    T^.JitTable := Info^.BaseTable;
    Info^.Table := nil;
    Info^.Deopts := 0;
    Inc(Info^.Recompiles);
    FillChar(Info^.Tried[0], Length(Info^.Tried), 0);
    if OptLog then WriteLn(ErrOutput, 'opt ', PtrUInt(T) and $FFFFFF, ' dropped after deopts, recompiles ', Info^.Recompiles);
    if Info^.Recompiles > 4 then begin T^.Opt := nil; Exit; end;
  end;
  if Info^.Count > 0 then Exit;
  { the next run: longer after each region that was not optimized (a template without one stops asking) }
  Info^.Count := OPT_HOT shl Min(Info^.Fails, 20);
  if (Info^.Tried[PC] <> 0) or (GetCurrentThreadId <> JitMainThread) then
  begin
    Inc(Info^.Fails);
    Exit;
  end;
  if (OptMax >= 0) and (OptCompiles >= OptMax) then Exit;
  Inc(OptCompiles);
  if OptCompile(Fr, PC) then Info^.Tried[PC] := 1
  else
  begin
    Info^.Tried[PC] := 2;
    Inc(Info^.Fails);
    Info^.Count := OPT_HOT shl Min(Info^.Fails, 20);
  end;
  if OptLog then
    WriteLn(ErrOutput, 'opt ', OptName(T), ' pc ', PC, ' len ', T^.CodeLen, ' ok ', Info^.Tried[PC] = 1,
      ' nodes ', ONCount, ' blocks ', OBCount, ' spills ', OSpills, ' recompiles ', Info^.Recompiles, ' bytes ', JitLen);
  if OptLog then Flush(ErrOutput);
end;
