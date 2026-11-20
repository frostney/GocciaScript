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
  { element access on the speculated receiver in VStack register Reg (checked): X the index (an int node) }
  onTAGet = 73;    { a typed array's element of kind Imm, as the node's kind (int or double) }
  onTASet = 74;    { Y (a node of the element kind's number kind) into a typed array of kind Imm }
  onArrGet = 75;   { an elements store's value (not a hole) into VStack register Y }
  onArrSet = 76;   { Y (a node >= 0, else VStack register -1 - Y) over an elements store's value (not a hole) }
  { property access on the receiver in VStack register Reg, the shape its instruction's IC saw (OPropShape) a
    constant: Imm the slot (an inline one), bit 40 of Imm when the shape is already known to hold }
  onPropGet = 77;  { an own data property: of kind vkNone into VStack register Y, of a number kind unboxed }
  onPropSet = 78;  { Y (a node >= 0, else VStack register -1 - Y) over an own writable data property }
  onLength = 79;   { an array's, typed array's or string's length (Imm a LEN_ kind) as an int }

  onGlobalGet = 80;   { the global object's own property (shape OPropShape, slot Imm and 32 bits up the object's
                        inline capacity) into VStack register Y }
  onMath = 81;        { Math function Imm (OptMathFns) of X (a double), the callee in VStack register Reg checked }
  onUpvGet = 83;      { upvalue Imm of the frame's closure (open: its register's value) into VStack register Y }
  onElemRecv = 86;    { VStack register Imm, an element receiver no instruction of the region writes, checked once at
                        the entry to be an object (its element accesses then skip the check) }
  onCheckHole = 84;   { VStack register Imm checked not to be the hole (the baseline's code throws the ReferenceError) }
  onNewObj = 85;      { a plain object of inline capacity Imm and shape OPropShape allocated in the nursery into VStack
                        register Reg (the nursery full: an exit) }
  onInlGuard = 82;    { inlined call site Imm (OSites): its callee in VStack register Reg checked to be the inlined
                        template, room for its registers and a frame }

  OPT_INLINE_MAX = 60;   { an inlined callee's instructions at most }
  { the instructions an inlined callee's code may embed: the baseline's code of these uses neither its template's
    constants nor its inline caches nor its strictness (a call's frame, an operator's helper, typeof's atoms, an
    element read; not an element store, whose helper reads the frame's strictness) }
  OINL_EMBED = [opCall, opNew, opTypeof, opNot, opAdd, opSub, opMul, opDiv, opMod, opBitAnd, opBitOr, opBitXor, opShl,
    opShr, opUShr, opEq, opNe, opSEq, opSNe, opLt, opLe, opGt, opGe, opToNum, opToNumeric, opNeg, opBitNot, opAddI,
    opSubI, opIncI, opBitAndI, opBitOrI, opBitXorI, opShlI, opShrI, opUShrI, opGetElem];
  OPT_FB_NODES = 200;     { (OFbWeight) }
  OPT_FB_MIN = 20;
  { what an inlined call or allocation counts for in the profit estimates: its call (about 150 instructions of the
    shared call stub, and the callee's entry and return) or its helper gone }
  OPT_INL_GAIN = 6;
  OPT_INLINE_DEPTH = 4;  { inlined calls nested this deep at most (a callee's call inlined in turn) }
  OPT_INLINE_TOTAL = 160;   { the instructions of one call's callees, nested ones included, at most }

  PROP_KNOWN = Int64(1) shl 40;
  PROP_POLY = Int64(1) shl 41;   { a polymorphic IC's entries tried in turn (else entry Imm shr 44 and 7: its slot) }
  PROP_ADD = Int64(1) shl 42;    { a store that adds the property: the shape becomes OPropNew }
  PROP_ICCHK = Int64(1) shl 43;  { that add transition still in the store's IC (a prototype change forgets it) }
  PROP_EXT = Int64(1) shl 48;    { the receiver checked extensible (a class field's definition) }
  PROP_YOUNG = Int64(1) shl 47;  { the receiver allocated by the region since its last embed: no write barrier }
  LEN_ARRAY = 1; LEN_STR = 3;   { (a typed array's length is a prototype getter a script can replace) }

  ELEM_ARRAY = 100;   { OElemSpec: an object with an elements store (an array, a plain object's indices) }

  { feedback bits (TOptInfo.Feedback per instruction) }
  { a polymorphic call's dispatch (OBuildStream's, role 4): an opJNotUndef whose jump is taken unless VStack register
    A holds a function of template OVTpl (an opcode the tier takes as a jump: no case falls outside the enum) }
  otbTpl = 9;       { (a branch's test) that register's function not of that template }
  OFB_EXIT = 256;   { (OJccGuardFb) a plain exit: no feedback, no deopt counted }
  ofbOverflow = 1; ofbEntry = 2; ofbGiveUp = 4; ofbUnboxDbl = 8; ofbUnboxNo = 16; ofbTagCmp = 32; ofbElem = 64; ofbProp = 128;

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
    { a loop's back edge (a backward jump of the region's own code or an inlined callee's): it polls Interrupt,
      set leaving at TermPC with frame state TermFS (the baseline's poll there then ends the run) }
    TermPoll: Boolean;
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
  OElemSpec: array of Integer;       { per element instruction: the receiver speculated (a typed array kind,
                                       ELEM_ARRAY, -1 none), from the frame's values at compile time }
  OCaptured: array of Boolean;       { per register: a nested function's upvalue can be open on it, so its value
                                       lives in VStack only (never an SSA value) and only embedded code writes it }
  OCapList: array of Integer;
  OPropSlot: array of Integer;       { per GetProp / SetProp: the IC's slot (-1 none); per GetLength a LEN_ kind }
  OPropShape: array of PShape;       { per GetProp / SetProp: the IC's receiver shape }
  OPropKind: array of Byte;          { per GetProp: the IC's receiver object kind }
  OPropVK: array of Byte;            { per GetProp: the value's kind speculated (vkTag: kept tagged in VStack) }
  OPropNew: array of PShape;         { per add store (SetProp's transition, DefProp's): the shape after it }
  { per NewObj: its inline capacity (-1 not allocated inline); per DefProp of an object literal: the slot it
    stores (-1 not native), the receiver's shape expected before it and the one after (nil: an existing slot) }
  OAllocCap, ODefSlot: array of Integer;
  ODefShape, ODefNew: array of PShape;
  OFactCap: array of Word;           { per register: the inline capacity of an object the region allocated since its
                                       last embed (0 none known) }
  OEndCap: array of array of Word;
  OQClo: PObj;                       { the region's closure for OQuickProfitable (nil at baseline compile time) }
  OQStatic: Boolean;                 { OQuickProfitable for OptPromising: a callee not known yet may inline }
  OSpecFr: PFrame;                   { the frame the region compiles from (OSpecR's values; nil none) }
  OSpecMemo: array of record R: Integer; K: Byte; end;   { OSpecR's answers, two per virtual PC (R -1 none) }
  OPendR: Integer;                   { a fact OInstr leaves for its written register (-1 none) }
  OPendShape: PShape;
  OPendKind: Byte;
  OPendCap: Word;
  OPropIC: array of PPropIC;         { per GetProp: its IC (a prototype's property: Poly0.Depth and Holder) }
  OMathSpec: array of Integer;       { per call: the OptMathFns entry its inline caches name (-1 none) }
  OSse41: Integer = -1;              { whether the CPU has roundsd (-1 not asked yet) }
  { shapes known to hold while the SSA is built: per register, the shape its object was checked to have (nil
    none) and that object's kind (255 not checked); per block, the facts at its end }
  OElemChecked: array of Boolean;    { per register: an onElemRecv at the entry checked it (OElemReceiver skips it) }
  OFactShape: array of PShape;
  OFactKind: array of Byte;
  OEndShape: array of array of PShape;
  OEndKind: array of array of Byte;
  { the region's instructions, by virtual PC: the template's own (virtual PC = its PC, then a sentinel at
    its CodeLen), each with its origin: the template whose ICs and constants it uses, its PC there and the
    feedback byte its guards record into }
  OVLen: Integer;
  OVCode: array of QWord;
  OVTpl: array of PFuncTemplate;
  OVPC: array of Integer;
  OVFb: array of PByte;
  { per virtual PC: 0 the template's own, 1 an inlined call's guard (the call instruction, at the caller's PC),
    2 an inlined callee's (OVSite its call site) }
  OVRole: array of Byte;
  OVSite: array of Integer;
  { per virtual PC: the inlined call site whose callee frame a deopt there resumes in (-1 the template's own) }
  OVFrame: array of Integer;
  { the inlined call sites: the call at virtual PC VCall (callee register A of the region's frame, Argc
    arguments, Flag 1 no this; at PC Q of its caller's template) runs callee Tc, its guard at virtual PC Guard,
    its returns jumping to virtual PC Ret. Parent the site whose callee makes the call (-1 the template's own
    code), K the outermost such call's PC in the template, Depth the number of inlined callers, D the deopt
    descriptor (OInlDesc) }
  OSites: array of record K, VCall, A, Argc, Flag, Guard, Ret, Parent, Q, Depth: Integer; Tc: PFuncTemplate;
    D: Pointer; IsNew, IsFI: Boolean; Room, FiSite, FnCap: Integer; end;
  OSiteCount: Integer;
  OInlBan: array of Boolean;          { per caller PC: not inlined in this compilation }
  OInlBanned: Boolean;                { OInferKinds banned a site: the stream is built again }
  OptInline: Integer = -1;            { G2_OPTINLINE=0 turns inlining off (-1 not read yet) }
  OptAlloc: Integer = -1;             { G2_OPTALLOC=0 turns inline allocation and inlined constructors off }
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
  OTplLive: array of QWord;          { the same in the template's own code (the baseline's), per its PC }
  ONumUse: array of Boolean;         { OptCompile: the template's registers its code from the entry on reads as numbers }
  OMap: array of Integer;            { the building block's register map }
  OFail: Boolean;
  OSpills: Integer;
  OFrame: Integer;                   { bytes below the saved r15 }
  OScratch: Integer;                 { the scratch slot's offset from rsp }
  OEntry: Integer;
  OGuards: array of Integer;         { nodes and blocks needing a cold stub: >= 0 node, < 0 -1 - block (exit) }
  OGuardJumps: array of Integer;     { per guard: the first of its jumps in OGJ }
  OGJ: array of record G, At, Fb: Integer; end;
  { the code's cold fragments, emitted after its blocks: Kind 1 an overflow slot's load (Slot), 2 its store; At the
    hot code's jump to it, Back where it returns }
  OCold: array of record Kind, At, Back, Slot: Integer; end;
  OColdCount: Integer;
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
  Result := not (Op in [onConst, onEntry, onPhi, onStoreTag, onLoadKTag, onCopyTag, onUnbox, onEmbed, onUpvGet]);
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

{ (at the template's own instructions, also what its baseline code reads from there: a failed guard runs it) }
function OLiveAt(PC, R: Integer): Boolean; inline;
begin
  Result := ((OLive[PC * OLiveW + (R shr 6)] and (QWord(1) shl (R and 63))) <> 0) or
    ((PC < OT^.CodeLen) and (R < OT^.MaxRegs) and ((OTplLive[PC * OLiveW + (R shr 6)] and (QWord(1) shl (R and 63))) <> 0));
end;

function OIns(PC: Integer): QWord; inline; begin Result := OVCode[PC]; end;
function OFb(PC: Integer): Byte; inline; begin Result := OVFb[PC]^; end;
function OOpOf(I: QWord): TOp; inline; begin Result := TOp(I and $FF); end;
function OA(I: QWord): Integer; inline; begin Result := (I shr 8) and $FFFF; end;
function OB_(I: QWord): Integer; inline; begin Result := (I shr 24) and $FFFF; end;
function OC(I: QWord): Integer; inline; begin Result := Integer(SarInt64(Int64(I), 40)); end;
function OCU(I: QWord): Integer; inline; begin Result := Integer(I shr 40); end;

{ the instructions the tier can take at all (whether it takes one also depends on its operands' kinds) }
{ IC's entry E (0 Poly0, 1-3 More) }
function OICEntry(IC: PPropIC; E: Integer): PICEntry; inline;
begin
  if E = 0 then Result := @IC^.Poly0 else Result := @IC^.More^[E];
end;

{ whether get-IC entry E can be checked inline: a data property of a non-dictionary receiver shape, found through
  prototypes whose shapes alone tell (none a dictionary's) }
function OEntryOk(E: PICEntry): Boolean;
var D: Integer;
begin
  Result := False;
  if (E^.Shape = nil) or E^.Shape^.IsDict or (E^.Depth > IC_MAX_DEPTH) or (E^.Acc <> 0) or (E^.VerMask <> 0) or
     (E^.Slot < 0) then Exit;
  for D := 0 to E^.Depth - 1 do
    if (E^.Holder[D] = nil) or E^.Holder[D]^.IsDict then Exit;
  Result := True;
end;

{ whether every entry of get-IC IC (one to four) can be checked inline }
function OICOk(IC: PPropIC): Boolean;
var E: Integer;
begin
  Result := False;
  if (IC^.PolyCount < 1) or (IC^.PolyCount > 4) or ((IC^.PolyCount > 1) and (IC^.More = nil)) then Exit;
  for E := 0 to IC^.PolyCount - 1 do
    if not OEntryOk(OICEntry(IC, E)) then Exit;
  Result := True;
end;

{ IC's entry for a receiver of shape Sh (-1 none, or one not checked inline) }
function OEntryFor(IC: PPropIC; Sh: PShape): Integer;
var E: Integer;
begin
  Result := -1;
  if (IC^.PolyCount < 1) or (IC^.PolyCount > 4) or ((IC^.PolyCount > 1) and (IC^.More = nil)) then Exit;
  for E := 0 to IC^.PolyCount - 1 do
    if OICEntry(IC, E)^.Shape = Sh then
    begin
      if OEntryOk(OICEntry(IC, E)) then Result := E;
      Exit;
    end;
end;

function OStaticOk(Op: TOp): Boolean;
begin
  case Op of
    opNop, opMov, opLoadK, opLoadI, opLoadUndef, opLoadNull, opLoadTrue, opLoadFalse, opLoadHole,
    opAdd, opSub, opMul, opDiv, opMod, opBitAnd, opBitOr, opBitXor, opShl, opShr, opUShr,
    opAddI, opSubI, opIncI, opBitAndI, opBitOrI, opBitXorI, opShlI, opShrI, opUShrI,
    opNeg, opBitNot, opNot, opToNum, opToNumeric,
    opEq, opNe, opSEq, opSNe, opLt, opLe, opGt, opGe,
    opJmp, opJT, opJF, opJNullish, opJNotNullish, opJUndef, opJNotUndef,
    opJNLt, opJNLe, opJNGt, opJNGe, opJNSEq, opJSEq, opGetElem, opSetElem, opGetProp, opSetProp,
    opGetLength, opGetGlobal, opGetMethod, opGetUpv, opCheckHole, opDefField: Result := True;
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
    opMov: begin R(B); WFrom := A; WTo := A; end;
    opLoadK, opLoadI, opLoadUndef, opLoadNull, opLoadTrue, opLoadFalse, opClosure: begin WFrom := A; WTo := A; end;
    { (the registers it closes are captured ones, always current in VStack); B > A: it clears A .. B - 1 }
    opClose: if B > A then begin WFrom := A; WTo := B - 1; end;
    opGetElem, opInstanceof, opIn, opAdd, opSub, opMul, opDiv, opMod, opBitAnd, opBitOr, opBitXor, opShl, opShr,
    opUShr, opEq, opNe, opSEq, opSNe, opLt, opLe, opGt, opGe:
      begin R(B); R(C); WFrom := A; WTo := A; end;
    opSetElem: begin R(A); R(B); R(C); end;
    opGetGlobal, opTypeofGlobal, opGetUpv, opNewObj, opLoadHole: begin WFrom := A; WTo := A; end;
    opNewArr: begin for K := C to C + B - 1 do R(K); WFrom := A; WTo := A; end;
    opCall, opNew:
      begin
        { the callee, this and the arguments (new.target: the instruction's own); the callee's frame then
          overlaps A and up }
        for K := A to A + 1 + B do if (K <> A + 1) or (OOpOf(I) = opCall) then R(K);
        WFrom := A; WTo := ORegs - 1;
      end;
  else Result := False;
  end;
end;

function OEmbeddable(PC: Integer): Boolean;
var Rd: array[0..63] of Integer; NR, WF, WT: Integer;
begin
  { the template's own (the baseline's code there), or an inlined callee's call (OBuildStream let it be: no
    inline caches, its exits through XOptInlDeopt) }
  if OVRole[PC] in [2, 5] then Result := OOpOf(OIns(PC)) in OINL_EMBED
  else Result := (OVTpl[PC] = OT) and (PPtrUInt(OInfo^.BaseTable)[PC] <> 0);
  Result := Result and OEmbedSets(OIns(PC), Rd, NR, WF, WT) and not OptNoEmbed[OIns(PC) and $FF];
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
    opEq, opNe, opSEq, opSNe, opLt, opLe, opGt, opGe, opGetElem: begin R1 := OB_(I); R2 := OCU(I); end;
    opJT, opJF, opJNullish, opJNotNullish, opJUndef, opJNotUndef, opCheckHole: R1 := OA(I);
    opGetProp, opGetLength, opGetMethod: R1 := OB_(I);
    opCall: begin R1 := OA(I); R2 := OA(I) + 2; end;   { (an intrinsic Math call: the callee and its argument) }
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
    opEq, opNe, opSEq, opSNe, opLt, opLe, opGt, opGe, opGetElem, opGetProp, opGetLength, opGetGlobal,
    opCall, opGetMethod, opGetUpv, opNewObj: Result := OA(I);   { (GetMethod: A + 1 too, its this) }
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
  SetLength(Leader, OVLen + 1);
  SetLength(Seen, OVLen + 1);
  SetLength(Work, OVLen + 1);
  { the instructions reachable, and the leaders }
  NW := 0;
  Work[NW] := OEntry; Inc(NW);
  Leader[OEntry] := True;
  while NW > 0 do
  begin
    Dec(NW);
    PC := Work[NW];
    while (PC < OVLen) and not Seen[PC] do
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
        if PC + 1 < OVLen then Leader[PC + 1] := True;
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
  SetLength(OBlockOf, OVLen + 1);
  for PC := 0 to OVLen do OBlockOf[PC] := -1;
  OBCount := 1;
  SetLength(OB, 64);
  OB[0].StartPC := -1; OB[0].EndPC := -1; OB[0].TermPoll := False;
  for PC := 0 to OVLen - 1 do
    if Seen[PC] and (Leader[PC] or (PC = OEntry)) then
    begin
      if OBCount >= OPT_MAX_BLOCKS then Exit;
      if OBCount >= Length(OB) then SetLength(OB, OBCount * 2);
      OB[OBCount].StartPC := PC;
      OB[OBCount].TermPoll := False;
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
      if (PC + 1 >= OVLen) then begin OFail := True; Exit; end;
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

{ ---- liveness of the bytecode registers ---- }

{ the template's own liveness (what its baseline code reads from each PC on): an instruction not known here reads
  every register; the parameters and the registers closures capture are live throughout }
procedure OComputeTplLive;
var N, PC, W, R1, R2, NR, WF, WT, K: Integer; I: QWord; Op: TOp; Changed: Boolean; Cur, Always: array of QWord;
  ERd: array[0..63] of Integer;
  procedure SetBit(R: Integer); begin if (R >= 0) and (R < ORegs) then Cur[R shr 6] := Cur[R shr 6] or (QWord(1) shl (R and 63)); end;
  procedure ClrBit(R: Integer); begin if (R >= 0) and (R < ORegs) then Cur[R shr 6] := Cur[R shr 6] and not (QWord(1) shl (R and 63)); end;
  procedure OrFrom(P: Integer);
  var J: Integer;
  begin
    if (P < 0) or (P > N) then begin for J := 0 to OLiveW - 1 do Cur[J] := not QWord(0); Exit; end;
    for J := 0 to OLiveW - 1 do Cur[J] := Cur[J] or OTplLive[P * OLiveW + J];
  end;
begin
  N := OT^.CodeLen;
  SetLength(OTplLive, (N + 1) * OLiveW);
  FillChar(OTplLive[0], Length(OTplLive) * 8, 0);
  for W := 0 to OLiveW - 1 do OTplLive[N * OLiveW + W] := not QWord(0);
  SetLength(Cur, OLiveW);
  SetLength(Always, OLiveW);
  for W := 0 to OLiveW - 1 do Cur[W] := 0;
  for K := 0 to Min(OT^.NumParams, ORegs - 1) do SetBit(K);
  for K := 0 to ORegs - 1 do if OCaptured[K] then SetBit(K);
  for W := 0 to OLiveW - 1 do Always[W] := Cur[W];
  repeat
    Changed := False;
    for PC := N - 1 downto 0 do
    begin
      I := OT^.Code[PC];
      Op := OOpOf(I);
      for W := 0 to OLiveW - 1 do Cur[W] := 0;
      { live out }
      case Op of
        opRet, opRetUndef, opThrow, opThrowConst: ;
        opJmp: OrFrom(PC + 1 + OC(I));
      else
        begin
          OrFrom(PC + 1);
          if OIsCondJump(Op) then OrFrom(PC + 1 + OC(I));
        end;
      end;
      { live in }
      case Op of
        opRet, opThrow: SetBit(OA(I));
        opRetUndef, opThrowConst, opJmp: ;
        { (A the resumption's value, written when it resumes) }
        opYield: begin ClrBit(OA(I)); SetBit(OB_(I)); end;
      else
        if OIsCondJump(Op) then
        begin
          OReads(I, R1, R2);
          SetBit(R1); SetBit(R2);
        end
        else if OEmbedSets(I, ERd, NR, WF, WT) then
        begin
          if WF >= 0 then for K := WF to WT do ClrBit(K);
          for K := 0 to NR - 1 do SetBit(ERd[K]);
        end
        else for W := 0 to OLiveW - 1 do Cur[W] := not QWord(0);
      end;
      for W := 0 to OLiveW - 1 do
      begin
        Cur[W] := Cur[W] or Always[W];
        if OTplLive[PC * OLiveW + W] <> Cur[W] then
        begin
          OTplLive[PC * OLiveW + W] := Cur[W];
          Changed := True;
        end;
      end;
    end;
  until not Changed;
end;

procedure OComputeLiveness;
var Changed: Boolean; B, PC, K, W, R1, R2, Wr, S, NR, WF, WT: Integer; Cur: array of QWord; I: QWord;
  ERd: array[0..63] of Integer;
  procedure SetBit(R: Integer); begin if R >= 0 then Cur[R shr 6] := Cur[R shr 6] or (QWord(1) shl (R and 63)); end;
  procedure ClrBit(R: Integer); begin if R >= 0 then Cur[R shr 6] := Cur[R shr 6] and not (QWord(1) shl (R and 63)); end;
begin
  OLiveW := (ORegs + 63) shr 6;
  if OLiveW = 0 then OLiveW := 1;
  SetLength(OLive, (OVLen + 1) * OLiveW);
  FillChar(OLive[0], Length(OLive) * 8, 0);
  OComputeTplLive;
  SetLength(Cur, OLiveW);
  repeat
    Changed := False;
    for B := OBCount - 1 downto 1 do
    begin
      { live out: the successors' live in }
      for W := 0 to OLiveW - 1 do Cur[W] := 0;
      if OB[B].Term = otExit then
      begin
        { the baseline goes on from the exit: a return reads its value only, the template's own code what it
          reads from there, an inlined callee's (its frames the interpreter's, then its caller's) every register }
        I := OIns(OB[B].TermPC);
        if OOpOf(I) = opRetUndef then
        else if OOpOf(I) = opRet then SetBit(OA(I))
        else if (OB[B].TermPC < OT^.CodeLen) and (OVFrame[OB[B].TermPC] < 0) then
          for W := 0 to OLiveW - 1 do Cur[W] := OTplLive[OB[B].TermPC * OLiveW + W]
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
          else if OVRole[PC] = 1 then
          begin
            { an inlined call's guard: the call's operands (the baseline's call, a failed guard, reads them) }
            for K := OA(I) to OA(I) + 1 + OB_(I) do if (K <> OA(I) + 1) or (OOpOf(I) <> opNew) then SetBit(K);
          end
          else
          begin
            Wr := OWrites(I);
            ClrBit(Wr);
            OReads(I, R1, R2);
            SetBit(R1); SetBit(R2);
            if OOpOf(I) = opSetElem then begin SetBit(OA(I)); SetBit(OB_(I)); SetBit(OCU(I)); end;
            if OOpOf(I) in [opSetProp, opDefProp, opDefField] then begin SetBit(OA(I)); SetBit(OB_(I)); end;
            if OOpOf(I) = opGetMethod then begin ClrBit(OA(I) + 1); SetBit(OB_(I)); end;
            { an intrinsic call's this: the baseline's call (a failed guard) reads it }
            if OOpOf(I) = opCall then SetBit(OA(I) + 1);
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

function OSpecValue(Fr: PFrame; K, R, Depth: Integer): TValue; forward;
function ODblRead(K, R: Integer): Boolean; forward;

{ the kind a tagged value read as a number at PC is speculated to have from its guards' feedback alone }
function OSpec(PC: Integer): Integer;
begin
  if (OFb(PC) and ofbUnboxNo) <> 0 then Result := vkNone
  else if (OFb(PC) and ofbUnboxDbl) <> 0 then Result := vkDbl
  else Result := vkInt;
end;

{ the kind tagged register R read as a number at PC is speculated to have (vkNone: no speculation): what its
  guards there recorded, else a double when it holds one now, else an int }
function OSpecR(PC, R: Integer): Integer;
var V: TValue;
begin
  if (OFb(PC) and ofbUnboxNo) <> 0 then Result := vkNone
  else if (OFb(PC) and ofbUnboxDbl) <> 0 then Result := vkDbl
  else
  begin
    Result := vkInt;
    if (OSpecFr <> nil) and (R >= 0) and (R < ORegs) then
    begin
      { (asked again on each pass over the region: kept per instruction, for its first two registers) }
      if PC * 2 + 1 < Length(OSpecMemo) then
      begin
        if OSpecMemo[PC * 2].R = R then Exit(OSpecMemo[PC * 2].K);
        if OSpecMemo[PC * 2 + 1].R = R then Exit(OSpecMemo[PC * 2 + 1].K);
      end;
      V := OSpecValue(OSpecFr, PC, R, 0);
      { a double now, or a property read off shapes whose number properties have held doubles }
      if (not IsInt(V) and (V < DOUBLE_LIMIT)) or ODblRead(PC, R) then Result := vkDbl;
      if PC * 2 + 1 < Length(OSpecMemo) then
        if OSpecMemo[PC * 2].R < 0 then begin OSpecMemo[PC * 2].R := R; OSpecMemo[PC * 2].K := Result; end
        else if OSpecMemo[PC * 2 + 1].R < 0 then begin OSpecMemo[PC * 2 + 1].R := R; OSpecMemo[PC * 2 + 1].K := Result; end;
    end;
  end;
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
  Ovf := (OFb(PC) and 1) <> 0;
  KB := vkNone; KC := vkNone;
  case Op of
    opMov, opNeg, opBitNot, opNot, opToNum, opToNumeric, opAddI, opSubI, opIncI, opBitAndI, opBitOrI, opBitXorI,
    opShlI, opShrI, opUShrI: KB := K[OB_(I)];
    opAdd, opSub, opMul, opDiv, opMod, opBitAnd, opBitOr, opBitXor, opShl, opShr, opUShr, opEq, opNe, opSEq, opSNe,
    opLt, opLe, opGt, opGe: begin KB := K[OB_(I)]; KC := K[OCU(I)]; end;
  end;
  if ONumConsumer(Op) then
  begin
    if KB = vkTag then KB := OSpecR(PC, OB_(I));
    if KC = vkTag then KC := OSpecR(PC, OCU(I));
    if (KB = vkNone) and not (Op in [opMov]) and (Op <> opNop) and (Op in [opAdd, opSub, opMul, opDiv, opMod,
      opBitAnd, opBitOr, opBitXor, opShl, opShr, opUShr, opAddI, opSubI, opIncI, opBitAndI, opBitOrI, opBitXorI,
      opShlI, opShrI, opUShrI, opNeg, opBitNot, opToNum, opToNumeric, opLt, opLe, opGt, opGe]) then Exit(vkNone);
  end;
  case Op of
    opNop: Result := vkNone;
    opMov: if KB = vkNone then Result := vkTag else Result := KB;
    opLoadI: Result := vkInt;
    opLoadK: Result := OConstKind(OVTpl[PC]^.Consts[OCU(I)]);
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
    opGetElem:
      begin
        { a speculated receiver and an int index (a tagged one unboxed as an int): a double or a boolean is a
          property key, not an index }
        KC := K[OCU(I)];
        if KC = vkTag then KC := OSpecR(PC, OCU(I));
        if (KC <> vkInt) or (K[OB_(I)] <> vkTag) then Exit(vkNone);
        case OElemSpec[PC] of
          taInt8, taUint8, taUint8C, taInt16, taUint16, taInt32: Result := vkInt;
          taUint32: if Ovf then Result := vkNone else Result := vkInt;
          taFloat32, taFloat64: Result := vkDbl;
          ELEM_ARRAY: Result := vkTag;
        else Result := vkNone;
        end;
      end;
    opGetProp: if (K[OB_(I)] = vkTag) and (OPropSlot[PC] >= 0) then Result := OPropVK[PC] else Result := vkNone;
    opGetMethod: if (K[OB_(I)] = vkTag) and (OPropSlot[PC] >= 0) then Result := vkTag else Result := vkNone;
    opGetLength: if (K[OB_(I)] = vkTag) and (OPropSlot[PC] > 0) then Result := vkInt else Result := vkNone;
    opGetGlobal: if OPropSlot[PC] >= 0 then Result := vkTag else Result := vkNone;
    opGetUpv: Result := vkTag;
    opNewObj: if OAllocCap[PC] > 0 then Result := vkTag else Result := vkNone;
  else Result := vkNone;
  end;
end;

{ whether the region takes SetElem I with operand kinds K: a speculated receiver, an int index, and for a typed
  array a number to store }
function OSetElemOk(PC: Integer; I: QWord; const K: array of Byte): Boolean;
var KB, KC: Integer;
begin
  KB := K[OB_(I)];
  if KB = vkTag then KB := OSpecR(PC, OB_(I));
  if (KB <> vkInt) or (K[OA(I)] <> vkTag) then Exit(False);
  KC := K[OCU(I)];
  case OElemSpec[PC] of
    taInt8, taUint8, taInt16, taUint16, taInt32, taUint32, taFloat32, taFloat64:
      begin
        if KC = vkTag then KC := OSpecR(PC, OCU(I));
        Result := OIsNum(KC);
      end;
    ELEM_ARRAY: Result := KC <> vkNone;
  else Result := False;
  end;
end;

{ whether the tier takes the jump instruction I with operand kinds K }
function OJumpOk(PC: Integer; I: QWord; const K: array of Byte): Boolean;
var KA, KB: Integer;
begin
  KA := K[OA(I)];
  case OOpOf(I) of
    opJmp: Result := True;
    opJT, opJF: Result := (KA <> vkNone) and ((KA <> vkTag) or ((OFb(PC) and ofbTagCmp) = 0));
    opJNullish, opJNotNullish, opJUndef, opJNotUndef: Result := KA <> vkNone;
    opJNLt, opJNLe, opJNGt, opJNGe:
      begin
        KB := K[OB_(I)];
        if KA = vkTag then KA := OSpecR(PC, OA(I));
        if KB = vkTag then KB := OSpecR(PC, OB_(I));
        Result := OIsNum(KA) and OIsNum(KB);
      end;
    opJNSEq, opJSEq:
      begin
        KB := K[OB_(I)];
        if (KA = vkTag) or (KB = vkTag) then Result := (OFb(PC) and ofbTagCmp) = 0
        else Result := (OIsNum(KA) and OIsNum(KB)) or ((KA = vkBool) and (KB = vkBool));
      end;
  else Result := False;
  end;
end;

function OMk(Op: TOp; A, B: Integer; C: Int64): QWord; forward;

{ the atom typeof gives a value of kind K (VHole: not known) }
function OTypeofOfKind(K: Byte): TValue;
begin
  case K of
    vkInt, vkDbl: Result := TypeofValue(MkInt(0));
    vkBool: Result := TypeofValue(VTrue);
  else Result := VHole;
  end;
end;

{ the index of a string constant of T equal to V, -1 none }
function OStrConstIndex(T: PFuncTemplate; V: TValue): Integer;
var J: Integer;
begin
  for J := 0 to High(T^.Consts) do
    if IsString(T^.Consts[J]) and SEqFast(T^.Consts[J], V) then Exit(J);
  Result := -1;
end;

{ the kinds at each block's start; False when it found an instruction to make an exit (the blocks then change).
  Next to the kinds it follows string constants (opLoadK's, typeof's of a number or a boolean, moved) and folds
  what they decide: typeof of a known kind becomes opLoadK of its atom (when the template has that string), a
  comparison of two known strings opLoadTrue or opLoadFalse, and a strict-equality jump on them opJmp or opNop
  (False then: the blocks change). switch (typeof x) on a number keeps only its case }
function OInferKinds(const EntryKinds: array of Byte): Boolean;
var Work: array of Integer; InWork: array of Boolean; NW, B, PC, S, K, R, RK, NR, WF, WT: Integer; Cur: array of Byte;
  I: QWord; Op: TOp; Changed: Boolean; ERd: array[0..63] of Integer;
  InConst: array of array of TValue; CVis: array of Boolean; CurC: array of TValue; Folded, Eq: Boolean; TV: TValue;
begin
  Result := True;
  Folded := False;
  SetLength(InConst, OBCount);
  SetLength(CVis, OBCount);
  SetLength(CurC, ORegs);
  SetLength(InConst[0], ORegs);
  for R := 0 to ORegs - 1 do InConst[0][R] := VHole;
  CVis[0] := True;
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
    for R := 0 to ORegs - 1 do CurC[R] := InConst[B][R];
    if B > 0 then
      for PC := OB[B].StartPC to OB[B].EndPC do
      begin
        I := OIns(PC);
        Op := OOpOf(I);
        if OExit[PC] then Break;
        { the string constants: folds, then what the instruction writes }
        if Op = opTypeof then
        begin
          TV := OTypeofOfKind(Cur[OB_(I)]);
          if TV <> VHole then
          begin
            K := OStrConstIndex(OVTpl[PC], TV);
            if K >= 0 then
            begin
              OVCode[PC] := OMk(opLoadK, OA(I), 0, K);
              OEmbed[PC] := False;
              I := OIns(PC); Op := opLoadK;
            end;
          end;
        end;
        if (Op in [opSEq, opSNe, opEq, opNe]) and IsString(CurC[OB_(I)]) and IsString(CurC[OCU(I)]) then
        begin
          Eq := SEqFast(CurC[OB_(I)], CurC[OCU(I)]);
          if Eq = (Op in [opSEq, opEq]) then OVCode[PC] := OMk(opLoadTrue, OA(I), 0, 0)
          else OVCode[PC] := OMk(opLoadFalse, OA(I), 0, 0);
          OEmbed[PC] := False;
          I := OIns(PC); Op := OOpOf(I);
        end;
        if (Op in [opJSEq, opJNSEq]) and IsString(CurC[OA(I)]) and IsString(CurC[OB_(I)]) then
        begin
          Eq := SEqFast(CurC[OA(I)], CurC[OB_(I)]);
          if Eq = (Op = opJSEq) then OVCode[PC] := OMk(opJmp, 0, 0, OC(I)) else OVCode[PC] := OMk(opNop, 0, 0, 0);
          Folded := True;
          Break;
        end;
        if OEmbed[PC] then
        begin
          OEmbedSets(I, ERd, NR, WF, WT);
          if WF >= 0 then for R := WF to WT do CurC[R] := VHole;
        end;
        R := OWrites(I);
        if R >= 0 then CurC[R] := VHole;
        if Op = opGetMethod then CurC[OA(I) + 1] := VHole;
        if Op in [opCall, opNew] then for R := OA(I) to ORegs - 1 do CurC[R] := VHole;
        case Op of
          opLoadK: if IsString(OVTpl[PC]^.Consts[OCU(I)]) then CurC[OA(I)] := OVTpl[PC]^.Consts[OCU(I)];
          opMov: CurC[OA(I)] := CurC[OB_(I)];
          opTypeof: CurC[OA(I)] := OTypeofOfKind(Cur[OB_(I)]);
        end;
        for K := 0 to High(OCapList) do CurC[OCapList[K]] := VHole;
        if (Op = opJmp) or OIsCondJump(Op) then
        begin
          if not OJumpOk(PC, I, Cur) then begin OExit[PC] := True; Result := False; end
          else if Op in [opJNLt, opJNLe, opJNGt, opJNGe] then
          begin
            { the tagged operands unboxed }
            if Cur[OA(I)] = vkTag then Cur[OA(I)] := OSpecR(PC, OA(I));
            if Cur[OB_(I)] = vkTag then Cur[OB_(I)] := OSpecR(PC, OB_(I));
            for K := 0 to High(OCapList) do Cur[OCapList[K]] := vkTag;
          end;
          Break;
        end;
        if Op = opNop then Continue;
        if (OVRole[PC] = 1) and OSites[OVSite[PC]].IsFI then Continue;
        if OVRole[PC] = 1 then
        begin
          { an inlined call's guard: a function in the callee register }
          if Cur[OA(I)] <> vkTag then
          begin
            if OptLog then WriteLn(ErrOutput, 'ban guard ', PC, ' kind ', Cur[OA(I)]);
            OInlBan[OSites[OVSite[PC]].K] := True;
            OInlBanned := True;
            Result := False;
            Break;
          end;
          { a constructor's: its new object (this) }
          if OSites[OVSite[PC]].IsNew then Cur[OA(I) + 1] := vkTag;
          if OSites[OVSite[PC]].FiSite >= 0 then Cur[OSites[OSites[OVSite[PC]].FiSite].A] := vkTag;
          Continue;
        end;
        if OEmbed[PC] then
        begin
          OEmbedSets(I, ERd, NR, WF, WT);
          if WF >= 0 then for R := WF to WT do Cur[R] := vkTag;
          Continue;
        end;
        if Op = opSetElem then
        begin
          if not OSetElemOk(PC, I, Cur) then
          begin
            if OEmbeddable(PC) then OEmbed[PC] := True else OExit[PC] := True;
            Result := False;
            Break;
          end;
          { the tagged index (and a typed array's tagged value) unboxed }
          if Cur[OB_(I)] = vkTag then Cur[OB_(I)] := OSpecR(PC, OB_(I));
          if (OElemSpec[PC] <> ELEM_ARRAY) and (Cur[OCU(I)] = vkTag) then Cur[OCU(I)] := OSpecR(PC, OCU(I));
          for K := 0 to High(OCapList) do Cur[OCapList[K]] := vkTag;
          Continue;
        end;
        if Op = opCall then
        begin
          { an intrinsic Math call (not embedded): a function in the callee register, a number argument }
          RK := Cur[OA(I) + 2];
          if RK = vkTag then RK := OSpecR(PC, OA(I) + 2);
          if not OIsNum(RK) or (Cur[OA(I)] <> vkTag) or OCaptured[OA(I)] then
          begin
            OMathSpec[PC] := -1;
            if OEmbeddable(PC) then OEmbed[PC] := True else OExit[PC] := True;
            Result := False;
            Break;
          end;
          if Cur[OA(I) + 2] = vkTag then Cur[OA(I) + 2] := OSpecR(PC, OA(I) + 2);
          Cur[OA(I)] := vkDbl;
          for K := 0 to High(OCapList) do Cur[OCapList[K]] := vkTag;
          Continue;
        end;
        { a TDZ check writes nothing (a number or a boolean is no hole) }
        if Op = opCheckHole then Continue;
        if Op = opDefProp then
        begin
          { an object literal's receiver; any value }
          if (Cur[OA(I)] <> vkTag) or (OPropSlot[PC] < 0) or (Cur[OB_(I)] = vkNone) then
          begin
            if OEmbeddable(PC) then OEmbed[PC] := True else OExit[PC] := True;
            Result := False;
            Break;
          end;
          Continue;
        end;
        if Op in [opSetProp, opDefField] then
        begin
          { a speculated receiver; any value }
          if (Cur[OA(I)] <> vkTag) or (OPropSlot[PC] < 0) or (Cur[OB_(I)] = vkNone) then
          begin
            if OEmbeddable(PC) then OEmbed[PC] := True else OExit[PC] := True;
            Result := False;
            Break;
          end;
          Continue;
        end;
        if (Op = opGetElem) and (Cur[OCU(I)] = vkTag) and (OResultKind(PC, I, Cur) <> vkNone) then
          Cur[OCU(I)] := OSpecR(PC, OCU(I));
        RK := OResultKind(PC, I, Cur);
        if RK = vkNone then
        begin
          if OEmbeddable(PC) then OEmbed[PC] := True else OExit[PC] := True;
          Result := False;
          Break;
        end;
        if ((OWrites(I) >= 0) and OCaptured[OWrites(I)]) or ((Op = opGetMethod) and OCaptured[OA(I) + 1]) then
        begin
          { a captured register is written by the baseline's code only }
          if OEmbeddable(PC) then OEmbed[PC] := True else OExit[PC] := True;
          Result := False;
          Break;
        end;
        if ONumConsumer(Op) then
        begin
          { the tagged operands unboxed (as OGetAs does) }
          if not (Op in [opMov]) then
          begin
            if Cur[OB_(I)] = vkTag then Cur[OB_(I)] := OSpecR(PC, OB_(I));
            if not (Op in [opNeg, opBitNot, opToNum, opToNumeric, opAddI, opSubI, opIncI, opBitAndI, opBitOrI,
              opBitXorI, opShlI, opShrI, opUShrI]) then
              if Cur[OCU(I)] = vkTag then Cur[OCU(I)] := OSpecR(PC, OCU(I));
          end;
        end;
        Cur[OA(I)] := RK;
        if Op = opGetMethod then Cur[OA(I) + 1] := vkTag;
        { a captured register stays tagged (each read unboxes it again) }
        for K := 0 to High(OCapList) do Cur[OCapList[K]] := vkTag;
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
      { the constants: one where every way in agrees }
      if not CVis[S] then
      begin
        CVis[S] := True;
        SetLength(InConst[S], ORegs);
        for R := 0 to ORegs - 1 do InConst[S][R] := CurC[R];
        Changed := True;
      end
      else
        for R := 0 to ORegs - 1 do
          if (InConst[S][R] <> VHole) and (InConst[S][R] <> CurC[R]) then begin InConst[S][R] := VHole; Changed := True; end;
      if Changed and not InWork[S] then
      begin
        if NW >= Length(Work) then SetLength(Work, NW * 2);
        Work[NW] := S; Inc(NW); InWork[S] := True;
      end;
    end;
  end;
  if Folded then Result := False;
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
    SK := OSpecR(OCurPC, R);
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
var Rd: array[0..63] of Integer; NR, WF, WT, K, R, N, S: Integer; Flushed: array of Boolean;
  { the parameters of a non-strict ordinary function's frame at register Bs (its this) that changed since the
    region's entry flushed too: a legacy caller or arguments read while the instruction runs sees them
    (G2.ObjModel's LegacyCallerRead) }
  procedure FlushParams(T: PFuncTemplate; Bs: Integer);
  var R: Integer;
  begin
    if T^.IsStrict or T^.IsArrow or T^.IsMethod or T^.IsClassCtor or T^.IsGenerator or T^.IsAsync or T^.TopLevel then
      Exit;
    for R := Bs + 1 to Min(Bs + T^.NumParams, ORegs - 1) do
      if not Flushed[R] and (OMap[R] >= 0) and
         not ((ON[OMap[R]].Op = onEntry) and (Integer(ON[OMap[R]].Imm) = R)) then
      begin
        Flushed[R] := True;
        if OFSCount >= Length(OFS) then SetLength(OFS, OFSCount * 2 + 256);
        OFS[OFSCount].Reg := R; OFS[OFSCount].Node := OMap[R];
        Inc(OFSCount);
        Inc(ON[N].Y);
      end;
  end;
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
  { a call in an inlined callee: the inlined frames' registers below its frame valid in VStack (a collection
    during the call scans them) }
  if OVFrame[PC] >= 0 then
    for R := OT^.MaxRegs to Min(OA(I) + 1 + OB_(I), ORegs - 1) do
      if not Flushed[R] and (OMap[R] >= 0) then
      begin
        Flushed[R] := True;
        if OFSCount >= Length(OFS) then SetLength(OFS, OFSCount * 2 + 256);
        OFS[OFSCount].Reg := R; OFS[OFSCount].Node := OMap[R];
        Inc(OFSCount);
        Inc(ON[N].Y);
      end;
  FlushParams(OT, 0);
  S := OVFrame[PC];
  while S >= 0 do
  begin
    FlushParams(OSites[S].Tc, OSites[S].A + 1);
    S := OSites[S].Parent;
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

{ property get PC's node Imm for receiver register X: one shape's slot (PROP_KNOWN: the receiver's shape already
  known to be it), a polymorphic IC's entry the receiver's known shape picks, else PROP_POLY }
function OPropImm(PC, X: Integer): Int64;
var E: Integer; IC: PPropIC;
begin
  IC := OPropIC[PC];
  if OPropShape[PC] <> nil then
  begin
    Result := OPropSlot[PC];
    if (OFactShape[X] = OPropShape[PC]) and (OFactKind[X] = OPropKind[PC]) then Result := Result or PROP_KNOWN;
    Exit;
  end;
  E := -1;
  if OFactShape[X] <> nil then E := OEntryFor(IC, OFactShape[X]);
  if (E >= 0) and (OFactKind[X] = OICEntry(IC, E)^.Kind) then
    Result := OICEntry(IC, E)^.Slot or PROP_KNOWN or (Int64(E) shl 44)
  else Result := PROP_POLY;
end;

{ what the receiver register X of property get PC (node Imm) is known to be after it }
procedure OPropFact(PC, X: Integer; Imm: Int64);
begin
  if OPropShape[PC] <> nil then
  begin
    OFactShape[X] := OPropShape[PC];
    OFactKind[X] := OPropKind[PC];
  end
  else if (Imm and PROP_POLY) <> 0 then
  begin
    OFactShape[X] := nil;
    OFactKind[X] := 255;
  end;
end;

{ instruction PC (I) of block B into nodes }
procedure OInstr(B, PC: Integer; I: QWord);
var Op: TOp; A, X, Y, N: Integer; V: TValue; Ovf: Boolean; D: Double; Imm: Int64;
  function Arith(OpI, OpD: Integer): Integer;
  begin
    if OMap[OB_(I)] < 0 then OGetAs(B, OB_(I), OSpecR(PC, OB_(I)));
    if OMap[OCU(I)] < 0 then OGetAs(B, OCU(I), OSpecR(PC, OCU(I)));
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
    if OMap[OB_(I)] < 0 then OGetAs(B, OB_(I), OSpecR(PC, OB_(I)));
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
    if OMap[R1] < 0 then OGetAs(B, R1, OSpecR(PC, R1));
    if OMap[R2] < 0 then OGetAs(B, R2, OSpecR(PC, R2));
    if (OMap[R1] < 0) or (OMap[R2] < 0) then begin OFail := True; Exit(0); end;
    K1 := ON[OMap[R1]].Kind; K2 := ON[OMap[R2]].Kind;
    if ((K1 = vkInt) or (K1 = vkBool)) and (K1 = K2) then
      Result := OEmitNode(B, onCmpI, vkBool, OMap[R1], OMap[R2], CC)
    else Result := OEmitNode(B, onCmpD, vkBool, OGetAs(B, R1, vkDbl), OGetAs(B, R2, vkDbl), CC);
  end;
begin
  Op := OOpOf(I);
  A := OA(I);
  Ovf := (OFb(PC) and 1) <> 0;
  N := -1;
  if OEmbed[PC] then
  begin
    OEmbedInstr(B, PC, I);
    Exit;
  end;
  if OVRole[PC] = 1 then
  begin
    { a field initializer's: its constructor's guard checked it }
    if OSites[OVSite[PC]].IsFI then Exit;
    N := OEmitNode(B, onInlGuard, vkNone, -1, -1, OVSite[PC]);
    if OFail then Exit;
    ON[N].Reg := A;
    OGuard(N, PC);
    if OSites[OVSite[PC]].IsNew then
    begin
      { a constructor's this: allocated as XFastNew allocates it (the nursery full: the new instruction again) }
      N := OEmitNode(B, onNewObj, vkNone, -1, -1, OAllocCap[PC]);
      if OFail then Exit;
      ON[N].Reg := A + 1;
      OGuard(N, PC);
      OMap[A + 1] := -1;
      OFactShape[A + 1] := OPropShape[PC]; OFactKind[A + 1] := okPlain; OFactCap[A + 1] := OAllocCap[PC];
      { the field initializer's closure (the guard put it in VStack) }
      if OSites[OVSite[PC]].FiSite >= 0 then OMap[OSites[OSites[OVSite[PC]].FiSite].A] := -1;
    end;
    Exit;
  end;
  case Op of
    opNop: Exit;
    opGetElem:
      begin
        X := OGetAs(B, OCU(I), vkInt);
        if OFail then Exit;
        if OElemSpec[PC] = ELEM_ARRAY then
        begin
          N := OEmitNode(B, onArrGet, vkNone, X, A, 0);
          ON[N].Reg := OB_(I);
          OGuard(N, PC);
          OMap[A] := -1;
          Exit;
        end;
        if OElemSpec[PC] in [taFloat32, taFloat64] then Y := vkDbl else Y := vkInt;
        N := OEmitNode(B, onTAGet, Y, X, -1, OElemSpec[PC]);
        ON[N].Reg := OB_(I);
        OGuard(N, PC);
      end;
    opGetProp:
      begin
        X := OB_(I);
        if OPropVK[PC] = vkTag then Y := vkNone else Y := OPropVK[PC];
        N := OEmitNode(B, onPropGet, Y, -1, A, OPropImm(PC, X));
        if OFail then Exit;
        ON[N].Reg := X;
        OGuard(N, PC);
        OPropFact(PC, X, ON[N].Imm);
        { a number unboxed into the node; anything else into VStack }
        if Y = vkNone then OMap[A] := -1 else OMap[A] := N;
        Exit;
      end;
    opGetMethod:
      begin
        { this: the receiver copied into A + 1; the method: the property of that receiver into A }
        X := OB_(I);
        if OMap[X] >= 0 then begin OFail := True; Exit; end;
        if X <> A + 1 then
        begin
          OEmitNode(B, onCopyTag, vkNone, X, A + 1, 0);
          OMap[A + 1] := -1;
          OFactShape[A + 1] := OFactShape[X];
          OFactKind[A + 1] := OFactKind[X];
        end;
        X := A + 1;
        N := OEmitNode(B, onPropGet, vkNone, -1, A, OPropImm(PC, X));
        if OFail then Exit;
        ON[N].Reg := X;
        OGuard(N, PC);
        OPropFact(PC, X, ON[N].Imm);
        OMap[A] := -1;
        Exit;
      end;
    opSetProp, opDefProp, opDefField:
      begin
        X := A;
        if OMap[OB_(I)] >= 0 then Y := OMap[OB_(I)] else Y := -1 - OB_(I);
        Imm := OPropSlot[PC];
        if OPropNew[PC] <> nil then
        begin
          Imm := Imm or PROP_ADD;
          if Op <> opDefProp then Imm := Imm or PROP_ICCHK;
        end;
        if OFactCap[X] > OPropSlot[PC] then Imm := Imm or PROP_YOUNG
        else if Op = opDefField then Imm := Imm or PROP_EXT;
        if OFactShape[X] = OPropShape[PC] then Imm := Imm or PROP_KNOWN else OFactKind[X] := 255;
        N := OEmitNode(B, onPropSet, vkNone, -1, Y, Imm);
        if OFail then Exit;
        ON[N].Reg := X;
        OGuard(N, PC);
        if OPropNew[PC] <> nil then OFactShape[X] := OPropNew[PC] else OFactShape[X] := OPropShape[PC];
        Exit;
      end;
    opNewObj:
      begin
        N := OEmitNode(B, onNewObj, vkNone, -1, -1, OAllocCap[PC]);
        if OFail then Exit;
        ON[N].Reg := A;
        OGuard(N, PC);
        OMap[A] := -1;
        OPendR := A; OPendShape := OPropShape[PC]; OPendKind := okPlain; OPendCap := OAllocCap[PC];
        Exit;
      end;
    opCheckHole:
      begin
        if OMap[A] >= 0 then Exit;
        N := OEmitNode(B, onCheckHole, vkNone, -1, -1, A);
        if OFail then Exit;
        OGuard(N, PC);
        Exit;
      end;
    opGetUpv:
      begin
        { an inlined callee's: its own closure's (in its call's register, the guard checked) }
        if OVFrame[PC] >= 0 then X := OSites[OVFrame[PC]].A else X := -1;
        OEmitNode(B, onUpvGet, vkNone, X, A, OB_(I));
        OMap[A] := -1;
        Exit;
      end;
    opGetGlobal:
      begin
        N := OEmitNode(B, onGlobalGet, vkNone, -1, A,
          Int64(OPropSlot[PC]) or (Int64(ObjOf(GlobalObj)^.InlineCap) shl 32));
        if OFail then Exit;
        OGuard(N, PC);
        OMap[A] := -1;
        Exit;
      end;
    opCall:
      begin
        X := OGetAs(B, A + 2, vkDbl);
        if OFail then Exit;
        N := OEmitNode(B, onMath, vkDbl, X, -1, OMathSpec[PC]);
        if OFail then Exit;
        ON[N].Reg := A;
        OGuard(N, PC);
      end;
    opGetLength:
      begin
        N := OEmitNode(B, onLength, vkInt, -1, -1, OPropSlot[PC]);
        if OFail then Exit;
        ON[N].Reg := OB_(I);
        OGuard(N, PC);
      end;
    opSetElem:
      begin
        X := OGetAs(B, OB_(I), vkInt);
        if OFail then Exit;
        if OElemSpec[PC] = ELEM_ARRAY then
        begin
          if OMap[OCU(I)] >= 0 then Y := OMap[OCU(I)] else Y := -1 - OCU(I);
          N := OEmitNode(B, onArrSet, vkNone, X, Y, 0);
        end
        else
        begin
          if OElemSpec[PC] in [taFloat32, taFloat64] then Y := OGetAs(B, OCU(I), vkDbl)
          else Y := OGetAs(B, OCU(I), vkInt);
          if OFail then Exit;
          N := OEmitNode(B, onTASet, vkNone, X, Y, OElemSpec[PC]);
        end;
        ON[N].Reg := OA(I);
        OGuard(N, PC);
        Exit;
      end;
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
        V := OVTpl[PC]^.Consts[OCU(I)];
        case OConstKind(V) of
          vkInt: N := OConstI(B, IntOf(V));
          vkDbl: begin D := PDouble(@V)^; N := OConstD(B, D); end;
          vkBool: N := OEmitNode(B, onConst, vkBool, -1, -1, Ord(V = VTrue));
        else
          begin
            OEmitNode(B, onLoadKTag, vkNone, A, -1, Int64(PtrUInt(@OVTpl[PC]^.Consts[OCU(I)])));
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
        if OMap[OB_(I)] < 0 then OGetAs(B, OB_(I), OSpecR(PC, OB_(I)));
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
        if OMap[OB_(I)] < 0 then OGetAs(B, OB_(I), OSpecR(PC, OB_(I)));
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
    if OMap[R1] < 0 then OGetAs(B, R1, OSpecR(PC, R1));
    if OMap[R2] < 0 then OGetAs(B, R2, OSpecR(PC, R2));
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
  PC := OB[B].EndPC;
  { (the synthetic entry has no instruction) }
  if PC < 0 then Exit;
  { a back edge: its frame state for the poll's exit (a jump inlining made, roles 2, 4 and 5, is none) }
  I := OIns(PC);
  OB[B].TermPoll := ((OOpOf(I) = opJmp) or OIsCondJump(OOpOf(I))) and (OC(I) < 0) and (OVRole[PC] in [0, 1]);
  if OB[B].TermPoll then
  begin
    OB[B].TermPC := PC;
    OSnapshot(PC, OB[B].TermFS, OB[B].TermFSN);
  end;
  if OB[B].Term <> otBr then Exit;
  I := OIns(PC);
  Op := OOpOf(I);
  R1 := OA(I);
  R2 := OB_(I);
  { Succ[0] the jump target: taken when the test holds (TermImm 1: when it does not) }
  if OVRole[PC] = 4 then
  begin
    if OMap[R1] >= 0 then begin OFail := True; Exit; end;
    OB[B].TermTest := otbTpl;
    OB[B].TermX := R1;
    OB[B].TermImm := 0;
    Exit;
  end;
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
      onMulD, onDivD, onCmpI, onCmpD, onTASet, onArrSet, onPropSet] then ON[N].Y := ORep(ON[N].Y);
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

{ OElemChecked: the registers an element instruction of the region takes as its receiver (speculated: an
  elements store or a typed array) that are live at the entry, not captured, and that no instruction of the
  region writes (its own writes, an embedded instruction's, a call's from its register up; any other
  instruction counts as writing its A) }
procedure OElemCheckSet;
var Recv, Written: array of Boolean; B, PC, R, NR, WF, WT: Integer; I: QWord; Op: TOp; ERd: array[0..63] of Integer;
begin
  SetLength(OElemChecked, ORegs);
  SetLength(Recv, ORegs);
  SetLength(Written, ORegs);
  for R := 0 to ORegs - 1 do begin OElemChecked[R] := False; Recv[R] := False; Written[R] := False; end;
  for B := 1 to OBCount - 1 do
    for PC := OB[B].StartPC to OB[B].EndPC do
    begin
      I := OIns(PC);
      Op := OOpOf(I);
      if OExit[PC] then Break;
      if not OEmbed[PC] and (OElemSpec[PC] >= 0) then
        if Op = opGetElem then Recv[OB_(I)] := True
        else if Op = opSetElem then Recv[OA(I)] := True;
      if OEmbed[PC] then
      begin
        OEmbedSets(I, ERd, NR, WF, WT);
        if WF >= 0 then for R := WF to Min(WT, ORegs - 1) do Written[R] := True;
      end;
      R := OWrites(I);
      if (R >= 0) and (R < ORegs) then Written[R] := True;
      case Op of
        opCall, opNew: for R := OA(I) to ORegs - 1 do Written[R] := True;
        opGetMethod: if OA(I) + 1 < ORegs then Written[OA(I) + 1] := True;
        opNop, opJmp, opJT, opJF, opJNullish, opJNotNullish, opJUndef, opJNotUndef, opJNLt, opJNLe, opJNGt, opJNGe,
        opJNSEq, opJSEq, opSetProp, opSetElem, opDefProp, opDefField, opCheckHole, opRet, opRetUndef, opThrow,
        opThrowConst: ;
      else
        if (OWrites(I) < 0) and (OA(I) < ORegs) then Written[OA(I)] := True;
      end;
    end;
  for R := 0 to ORegs - 1 do
    OElemChecked[R] := Recv[R] and not Written[R] and not OCaptured[R] and OLiveAt(OEntry, R);
end;

function OBuildSSA: Boolean;
var Stack: array of Integer; Idx: array of Integer; NS, B, S, K, R, P, PC, N, V, Ph, L: Integer;
  Flushed: array of Integer;
  Phis: array of record B, R, N: Integer; end; NPhis: Integer; Visited: array of Boolean;
  FactS: PShape; FactK: Byte; FactC: Word;
begin
  Result := False;
  FactS := nil; FactK := 255;
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
  SetLength(OFactShape, ORegs);
  SetLength(OFactKind, ORegs);
  SetLength(OFactCap, ORegs);
  SetLength(OEndCap, OBCount);
  SetLength(OEndShape, OBCount);
  SetLength(OEndKind, OBCount);
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
      { the element receivers the region never writes: checked here once }
      OElemCheckSet;
      for R := 0 to ORegs - 1 do
        if OElemChecked[R] then
        begin
          N := OEmitNode(0, onElemRecv, vkNone, -1, -1, R);
          ON[N].PC := OEntry;
          ON[N].FS := OFSCount; ON[N].FSN := 0;
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
    { the shapes known: a single predecessor's at its end, else none }
    if (B > 0) and (Length(OB[B].Preds) = 1) and (OB[OB[B].Preds[0]].RPO < OB[B].RPO) and
       (OEndShape[OB[B].Preds[0]] <> nil) then
    begin
      Move(OEndShape[OB[B].Preds[0]][0], OFactShape[0], ORegs * SizeOf(PShape));
      Move(OEndKind[OB[B].Preds[0]][0], OFactKind[0], ORegs);
      Move(OEndCap[OB[B].Preds[0]][0], OFactCap[0], ORegs * SizeOf(Word));
    end
    else
    begin
      FillChar(OFactShape[0], ORegs * SizeOf(PShape), 0);
      FillChar(OFactKind[0], ORegs, 255);
      FillChar(OFactCap[0], ORegs * SizeOf(Word), 0);
    end;
    if B > 0 then
      for PC := OB[B].StartPC to OB[B].EndPC do
      begin
        if (PC = OB[B].EndPC) and ((OB[B].Term = otExit) or (OB[B].Term = otBr) or
           (OOpOf(OIns(PC)) = opJmp)) then Break;
        OCurPC := PC;
        if OOpOf(OIns(PC)) = opMov then
        begin
          FactS := OFactShape[OB_(OIns(PC))]; FactK := OFactKind[OB_(OIns(PC))]; FactC := OFactCap[OB_(OIns(PC))];
        end;
        OPendR := -1;
        OInstr(B, PC, OIns(PC));
        if OFail then Exit;
        { a captured register's unboxed value is the instruction's only: VStack holds it }
        for R := 0 to High(OCapList) do OMap[OCapList[R]] := -1;
        { an embedded instruction can change any object's shape; a write forgets its register's }
        if OEmbed[PC] then
        begin
          FillChar(OFactShape[0], ORegs * SizeOf(PShape), 0);
          FillChar(OFactKind[0], ORegs, 255);
          FillChar(OFactCap[0], ORegs * SizeOf(Word), 0);
        end
        else
        begin
          R := OWrites(OIns(PC));
          if R >= 0 then begin OFactShape[R] := nil; OFactKind[R] := 255; OFactCap[R] := 0; end;
          if OOpOf(OIns(PC)) = opGetMethod then OFactCap[OA(OIns(PC)) + 1] := 0;
          if OOpOf(OIns(PC)) = opMov then
          begin
            OFactShape[OA(OIns(PC))] := FactS; OFactKind[OA(OIns(PC))] := FactK; OFactCap[OA(OIns(PC))] := FactC;
          end;
          if OPendR >= 0 then
          begin
            OFactShape[OPendR] := OPendShape; OFactKind[OPendR] := OPendKind; OFactCap[OPendR] := OPendCap;
          end;
        end;
      end;
    SetLength(OEndShape[B], ORegs);
    SetLength(OEndKind[B], ORegs);
    Move(OFactShape[0], OEndShape[B][0], ORegs * SizeOf(PShape));
    Move(OFactKind[0], OEndKind[B][0], ORegs);
    SetLength(OEndCap[B], ORegs);
    Move(OFactCap[0], OEndCap[B][0], ORegs * SizeOf(Word));
    OCurPC := OB[B].EndPC;
    OTerminator(B);
    if OFail then Exit;
    for R := 0 to High(OCapList) do OMap[OCapList[R]] := -1;
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
  Result := (OB[B].Term = otExit) or OB[B].TermPoll or
    ((OB[B].Term = otBr) and (OB[B].TermTest in [otbTagCmp, otbTruthyTag]));
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
        onMulD, onDivD, onCmpI, onCmpD, onTASet, onArrSet, onPropSet] then Mark(ON[M].Y);
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
        onMulD, onDivD, onCmpI, onCmpD, onTASet, onArrSet, onPropSet] then Use(ON[N].Y, Pos);
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
  { the spill slots, then one scratch slot (an elements store's boxed value) }
  OScratch := OSpills * 8;
  OFrame := OSpills * 8 + 8;
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

{ cmp R, fs:[Disp] (64-bit) }
procedure OCmpRFs(R, Disp: Integer);
begin JB($64); JRex(True, R, 0, 0); JB($3B); JB($04 or ((R and 7) shl 3)); JB($25); JD(Cardinal(Disp)); end;

{ the receiver of element node N (VStack register Reg) as an object in rax, else N's deopt }
procedure OElemReceiver(N: Integer);
begin
  MovRM(jAX, jBX, ON[N].Reg * 8);
  AluRR($31, jAX, jR13, True);
  if (ON[N].Reg < Length(OElemChecked)) and OElemChecked[ON[N].Reg] then Exit;
  MovRR(jDX, jAX);
  ShiftRI(5, jDX, 48, True);
  OJccGuardFb(ccNE, N, ofbElem);
  CmpBMI(jAX, 4, ckObject);
  OJccGuardFb(ccNE, N, ofbElem);
end;

{ typed array element node N: rax the view (checked: its kind, the index in rcx below its length, an ordinary
  buffer), then rdx the bytes' address (the element at rdx + rcx << size + DataOff) }
procedure OTAAddress(N: Integer; Store: Boolean);
begin
  OElemReceiver(N);
  CmpBMI(jAX, PtrInt(@PObj(nil)^.OKind), okTypedArray);
  OJccGuardFb(ccNE, N, ofbElem);
  MovzxBRM(jDX, jAX, PtrInt(@PObj(nil)^.OFlags));
  ShiftRI(5, jDX, 4, False);
  AluRI(7, jDX, Integer(ON[N].Imm), False);
  OJccGuardFb(ccNE, N, ofbElem);
  OLoadG(jCX, ON[N].X);
  Mov32RM(jDX, jAX, PtrInt(@PObj(nil)^.Aux2));
  AluRR($85, jDX, jDX, False);
  OJccGuardFb(ccS, N, ofbElem);
  AluRR($39, jCX, jDX, False);
  OJccGuardFb(ccAE, N, ofbElem);
  JRex(False, jCX, 0, jCX); JB($8B); JModRR(jCX, jCX);   { mov ecx, ecx }
  MovRM(jDX, jAX, PtrInt(@PObj(nil)^.Aux1));
  AluRR($31, jDX, jR13, True);
  if Store then TestBMI(jDX, PtrInt(@PObj(nil)^.OFlags), bfShared or bfResizable or bfDetached or bfImmutable)
  else TestBMI(jDX, PtrInt(@PObj(nil)^.OFlags), bfShared or bfResizable or bfDetached);
  OJccGuardFb(ccNE, N, ofbElem);
  MovRM(jDX, jDX, PtrInt(@PObj(nil)^.Aux1));
  AluRR($31, jDX, jR13, True);
  Mov32RM(jAX, jAX, PtrInt(@PObj(nil)^.Pad));
  AluRR($01, jDX, jAX, True);
end;

{ the write barrier after rax went into the cell in rdx (rcx scratch): an old cell not yet remembered that now
  points into the nursery is remembered, every allocatable register kept around the call }
{ onArrSet's store of V into a hole at index Idx of array O's elements (below their length; no element guard on
  O): JH_SetElem's dense store when the prototypes leave it to one. False: not done (the region's exit) }
function OptHoleStore(O: PObj; Idx: Integer; V: TValue): Boolean;
var E: PValueArray;
begin
  Result := (O^.OKind = okArray) and ((O^.OFlags and 64) = 0) and (Idx < ArrayDenseMax) and   { 64: ofElemGuard }
            AppendProtosPlain(O);
  if not Result then Exit;
  E := O^.Elements;
  E^.Data[Idx] := V;
  WriteBarrier(@E^.Hdr, V);
end;

{ onArrGet's read of a hole at index Idx of O's elements (GetElemV's miss): undefined when O is a plain object or
  an array and no index-keyed property of it or its prototypes can answer it, else the hole (the region's exit) }
function OptHoleRead(O: PObj; Idx: Integer): TValue;
begin
  if ((O^.OKind = okPlain) or (O^.OKind = okArray)) and not O^.Shape^.HasIndexKeys and ElemMissUndefined(O, Idx) then
    Result := VUndefined
  else Result := VHole;
end;

procedure OBarrier;
var L1, L2, L3, L4, K: Integer;
begin
  MovzxBRM(jCX, jDX, 5);
  AluRI(4, jCX, cfOld or cfRemembered, False);
  AluRI(7, jCX, cfOld, False);
  L1 := JccFwd(ccNE);
  MovRR(jCX, jAX);
  ShiftRI(5, jCX, 48, True);
  AluRI(7, jCX, TAG_PTR_HI, False);
  L2 := JccFwd(ccNE);
  MovRR(jCX, jAX);
  AluRR($31, jCX, jR13, True);
  OCmpRFs(jCX, JitFsNurseryStart);
  L3 := JccFwd(ccB);
  OCmpRFs(jCX, JitFsNurseryLimit);
  L4 := JccFwd(ccAE);
  { push rsi, rdi, r8 - r11 (rsp stays aligned), xmm2 - xmm15 below them }
  JB($56); JB($57); JB($41); JB($50); JB($41); JB($51); JB($41); JB($52); JB($41); JB($53);
  AluRI(5, jSP, 112, True);
  for K := 0 to 13 do OSseM($F2, $11, 2 + K, jSP, K * 8);
  MovRR(jDI, jDX);
  CallAbs(@Remember);
  for K := 0 to 13 do OSseM($F2, $10, 2 + K, jSP, K * 8);
  AluRI(0, jSP, 112, True);
  JB($41); JB($5B); JB($41); JB($5A); JB($41); JB($59); JB($41); JB($58); JB($5F); JB($5E);
  PatchHere(L1); PatchHere(L2); PatchHere(L3); PatchHere(L4);
end;

{ property node N's receiver (VStack register Reg) in rax as an object: unless its shape is known to hold, checked
  to be an object of the IC's shape (and for a get, its object kind) }
procedure OPropReceiver(N: Integer; Get: Boolean);
begin
  MovRM(jAX, jBX, ON[N].Reg * 8);
  AluRR($31, jAX, jR13, True);
  if (ON[N].Imm and PROP_KNOWN) <> 0 then Exit;
  MovRR(jDX, jAX);
  ShiftRI(5, jDX, 48, True);
  OJccGuardFb(ccNE, N, ofbProp);
  CmpBMI(jAX, 4, ckObject);
  OJccGuardFb(ccNE, N, ofbProp);
  MovRI(jCX, QWord(PtrUInt(OPropShape[ON[N].PC])));
  CmpRM(jCX, jAX, PtrInt(@PObj(nil)^.Shape));
  OJccGuardFb(ccNE, N, ofbProp);
  if Get then
  begin
    CmpBMI(jAX, PtrInt(@PObj(nil)^.OKind), OPropKind[ON[N].PC]);
    OJccGuardFb(ccNE, N, ofbProp);
  end;
end;

{ the slot of property node N in rax's object inline (an overflow slot is the baseline's) }
procedure OPropInline(N: Integer);
begin
  MovzxWRM(jCX, jAX, PtrInt(@PObj(nil)^.InlineCap));
  AluRI(7, jCX, Integer(ON[N].Imm and $FFFFFFFF), False);
  OJccGuardFb(ccBE, N, ofbProp);
end;

{ a cold fragment of Kind for slot Slot: At the hot code's forward jump to it (its rel32 emitted), returning to the
  current position }
procedure OAddCold(Kind, At, Slot: Integer);
begin
  if OColdCount >= Length(OCold) then SetLength(OCold, OColdCount * 2 + 16);
  OCold[OColdCount].Kind := Kind; OCold[OColdCount].At := At; OCold[OColdCount].Slot := Slot;
  OCold[OColdCount].Back := JitLen;
  Inc(OColdCount);
end;

{ slot Slot of the object in rax (of a shape holding it) into rax: an inline slot, or past the object's inline
  capacity its overflow store's (a cold fragment) }
procedure OSlotLoad(Slot: Integer);
var L: Integer;
begin
  MovzxWRM(jCX, jAX, PtrInt(@PObj(nil)^.InlineCap));
  AluRI(7, jCX, Slot, False);
  L := JccFwd(ccBE);
  MovRM(jAX, jAX, PtrInt(@PObj(nil)^.Slots) + Slot * 8);
  OAddCold(1, L, Slot);
end;

{ the cold fragments }
procedure OEmitCold;
var K: Integer;
begin
  for K := 0 to OColdCount - 1 do
  begin
    PatchHere(OCold[K].At);
    case OCold[K].Kind of
      1:
        begin
          { rax the object, rcx its inline capacity }
          MovRI(jDX, Cardinal(OCold[K].Slot));
          AluRR($29, jDX, jCX, False);                          { sub edx, ecx }
          MovRM(jAX, jAX, PtrInt(@PObj(nil)^.Overflow));
          MovRMIdx(jAX, jAX, jDX, PtrInt(@PValueArray(nil)^.Data));
        end;
      2:
        begin
          { rax the object, rdx its inline capacity, rcx the value: rax becomes the overflow store (the barrier's
            cell) }
          JB($F7); JB($DA);                                     { neg edx }
          AluRI(0, jDX, OCold[K].Slot, False);
          MovRM(jAX, jAX, PtrInt(@PObj(nil)^.Overflow));
          MovMRIdx(jAX, jDX, PtrInt(@PValueArray(nil)^.Data), jCX);
        end;
    end;
    JB($E9); JD(Cardinal(OCold[K].Back - (JitLen + 4)));
  end;
end;

{ get-IC entry E's holder for the receiver in rax (of E's shape) into rax: from E's shape (a constant) along its
  prototypes, each checked to have E's holder shape }
procedure OPropHolder(N: Integer; E: PICEntry);
var D: Integer;
begin
  if E^.Depth = 0 then Exit;
  MovRI(jCX, QWord(PtrUInt(E^.Shape)));
  for D := 0 to E^.Depth - 1 do
  begin
    MovRM(jAX, jCX, PtrInt(@PShape(nil)^.Proto));
    AluRR($31, jAX, jR13, True);
    MovRI(jCX, QWord(PtrUInt(E^.Holder[D])));
    CmpRM(jCX, jAX, PtrInt(@PObj(nil)^.Shape));
    OJccGuardFb(ccNE, N, ofbProp);
  end;
end;

{ a polymorphic property get's value into rax: the receiver's shape against each IC entry's in turn (none: the
  deopt), then that entry's holder and inline slot }
procedure OPropPolyGet(N: Integer; IC: PPropIC);
var E, L, ND: Integer; En: PICEntry; Done: array[0..3] of Integer;
begin
  MovRM(jAX, jBX, ON[N].Reg * 8);
  AluRR($31, jAX, jR13, True);
  MovRR(jDX, jAX);
  ShiftRI(5, jDX, 48, True);
  OJccGuardFb(ccNE, N, ofbProp);
  CmpBMI(jAX, 4, ckObject);
  OJccGuardFb(ccNE, N, ofbProp);
  ND := 0;
  for E := 0 to IC^.PolyCount - 1 do
  begin
    En := OICEntry(IC, E);
    MovRI(jCX, QWord(PtrUInt(En^.Shape)));
    CmpRM(jCX, jAX, PtrInt(@PObj(nil)^.Shape));
    L := JccFwd(ccNE);
    CmpBMI(jAX, PtrInt(@PObj(nil)^.OKind), En^.Kind);
    OJccGuardFb(ccNE, N, ofbProp);
    OPropHolder(N, En);
    OSlotLoad(En^.Slot);
    Done[ND] := JmpFwd;
    Inc(ND);
    PatchHere(L);
  end;
  OJccGuardFb(-1, N, ofbProp);
  for E := 0 to ND - 1 do PatchHere(Done[E]);
end;

type
  { an inlined callee's deopt: its frames, outermost first, each callee register A (of the region's frame), Argc,
    template T, the PC it resumes at once the frame it calls returns; NextPC the region's frame's }
  POInlDesc = ^TOInlDesc;
  TOInlDesc = record
    N, NextPC: Integer;
    Lv: array[0..OPT_INLINE_DEPTH - 1] of record A, Argc, PC: Integer; T: PFuncTemplate; Kind: Byte; end;
  end;

function OInlDesc(S: Integer): POInlDesc; forward;

{ node N's code }
procedure OCode(N: Integer);
var L, CC, LH, KH: Integer; D: Int64; IC: PPropIC;
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
    onTAGet:
      begin
        OTAAddress(N, False);
        D := PtrInt(@PBytes(nil)^.Data);
        case ON[N].Imm of
          taUint8, taUint8C: SibOp(0, False, [$0F, $B6], jAX, jDX, jCX, 0, D);
          taInt8: SibOp(0, False, [$0F, $BE], jAX, jDX, jCX, 0, D);
          taInt16: SibOp(0, False, [$0F, $BF], jAX, jDX, jCX, 1, D);
          taUint16: SibOp(0, False, [$0F, $B7], jAX, jDX, jCX, 1, D);
          taInt32, taUint32: SibOp(0, False, [$8B], jAX, jDX, jCX, 2, D);
          taFloat64: SibOp($F2, False, [$0F, $10], 0, jDX, jCX, 3, D);
        else
          begin
            SibOp($F3, False, [$0F, $10], 0, jDX, jCX, 2, D);
            JB($F3); JB($0F); JB($5A); JB($C0);           { cvtss2sd xmm0, xmm0 }
          end;
        end;
        if ON[N].Imm = taUint32 then
        begin
          AluRR($85, jAX, jAX, False);
          OJccGuardFb(ccS, N, ofbOverflow);                { 2^31 and up: not an int }
        end;
        if ON[N].Kind = vkDbl then OStoreX(N, 0) else OStoreG(N, jAX);
      end;
    onTASet:
      begin
        OTAAddress(N, True);
        D := PtrInt(@PBytes(nil)^.Data);
        case ON[N].Imm of
          taFloat64:
            begin
              OLoadX(0, ON[N].Y);
              SibOp($F2, False, [$0F, $11], 0, jDX, jCX, 3, D);
            end;
          taFloat32:
            begin
              OLoadX(0, ON[N].Y);
              JB($F2); JB($0F); JB($5A); JB($C0);         { cvtsd2ss xmm0, xmm0 }
              SibOp($F3, False, [$0F, $11], 0, jDX, jCX, 2, D);
            end;
        else
          begin
            OLoadG(jAX, ON[N].Y);
            case ON[N].Imm of
              taUint8, taInt8: SibOp(0, False, [$88], jAX, jDX, jCX, 0, D);
              taInt16, taUint16: SibOp($66, False, [$89], jAX, jDX, jCX, 1, D);
            else SibOp(0, False, [$89], jAX, jDX, jCX, 2, D);
            end;
          end;
        end;
      end;
    onPropGet:
      begin
        IC := OPropIC[ON[N].PC];
        if (ON[N].Imm and PROP_POLY) <> 0 then OPropPolyGet(N, IC)
        else
        begin
          OPropReceiver(N, True);
          OPropHolder(N, OICEntry(IC, (ON[N].Imm shr 44) and 7));
          OSlotLoad(Integer(ON[N].Imm and $FFFFFFFF));
        end;
        if ON[N].Kind = vkNone then MovMR(jBX, ON[N].Y * 8, jAX)
        else
        begin
          { the value unboxed as the speculated kind (onUnbox's checks and feedback) }
          TagOf(jDX, jAX);
          AluRI(7, jDX, TAG_INT_HI, False);
          if ON[N].Kind = vkInt then
          begin
            L := JccFwd(ccE);
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
      end;
    onPropSet:
      begin
        if ON[N].Y >= 0 then
        begin
          OBox(ON[N].Y);
          MovMR(jSP, OScratch, jAX);
        end;
        OPropReceiver(N, False);
        if (ON[N].Imm and PROP_EXT) <> 0 then
        begin
          TestBMI(jAX, PtrInt(@PObj(nil)^.OFlags), 8);
          OJccGuardFb(ccNE, N, ofbProp);
        end;
        if (ON[N].Imm and PROP_ICCHK) <> 0 then
        begin
          { the store's IC still adds the property this way (G2.Core's ProtoChanged forgets add transitions) }
          MovRI(jCX, QWord(PtrUInt(@OPropIC[ON[N].PC]^.NewShape)));
          MovRM(jCX, jCX, 0);
          MovRI(jDX, QWord(PtrUInt(OPropNew[ON[N].PC])));
          AluRR($39, jCX, jDX, True);
          OJccGuardFb(ccNE, N, ofbProp);
        end;
        if ((ON[N].Imm and PROP_YOUNG) = 0) and ((ON[N].Imm and PROP_ADD) <> 0) then OPropInline(N);
        if (ON[N].Imm and PROP_ADD) <> 0 then
        begin
          MovRI(jCX, QWord(PtrUInt(OPropNew[ON[N].PC])));
          MovMR(jAX, PtrInt(@PObj(nil)^.Shape), jCX);
        end;
        if ON[N].Y >= 0 then MovRM(jCX, jSP, OScratch) else MovRM(jCX, jBX, (-1 - ON[N].Y) * 8);
        if ((ON[N].Imm and PROP_YOUNG) = 0) and ((ON[N].Imm and PROP_ADD) = 0) then
        begin
          { an own store: an inline slot, or past the inline capacity the overflow store's (a cold fragment) }
          MovzxWRM(jDX, jAX, PtrInt(@PObj(nil)^.InlineCap));
          AluRI(7, jDX, Integer(ON[N].Imm and $FFFFFFFF), False);
          L := JccFwd(ccBE);
          MovMR(jAX, PtrInt(@PObj(nil)^.Slots) + Integer(ON[N].Imm and $FFFFFFFF) * 8, jCX);
          OAddCold(2, L, Integer(ON[N].Imm and $FFFFFFFF));
        end
        else MovMR(jAX, PtrInt(@PObj(nil)^.Slots) + Integer(ON[N].Imm and $FFFFFFFF) * 8, jCX);
        if (ON[N].Y < 0) and ((ON[N].Imm and PROP_YOUNG) = 0) then
        begin
          MovRR(jDX, jAX);
          MovRR(jAX, jCX);
          OBarrier;
        end;
      end;
    onNewObj:
      begin
        { a bump of the nursery's top (full: the exit, whose baseline code collects), then the object's
          header and fields as G2.Core's NewObject writes them }
        L := 64 + Integer(ON[N].Imm) * 8;
        MovRFs(jAX, JitFsNurseryTop);
        LeaRM(jCX, jAX, L);
        MovRFs(jDX, JitFsNurseryLimit);
        AluRR($39, jCX, jDX, True);
        OJccGuardFb(ccA, N, OFB_EXIT);
        MovFsR(JitFsNurseryTop, jCX);
        MovRI(jDX, QWord(Cardinal(L)) or (QWord(ckObject) shl 32));
        MovMR(jAX, 0, jDX);
        MovRI(jDX, QWord(PtrUInt(OPropShape[ON[N].PC])));
        MovMR(jAX, PtrInt(@PObj(nil)^.Shape), jDX);
        AluRR($31, jDX, jDX, False);
        MovMR(jAX, PtrInt(@PObj(nil)^.Elements), jDX);
        MovMR(jAX, PtrInt(@PObj(nil)^.Overflow), jDX);
        MovMR(jAX, PtrInt(@PObj(nil)^.Native), jDX);
        MovRI(jDX, QWord(Cardinal(ON[N].Imm)) or (QWord(okPlain) shl 16));
        MovMR(jAX, PtrInt(@PObj(nil)^.InlineCap), jDX);
        MovRI(jDX, VUndefined);
        MovMR(jAX, PtrInt(@PObj(nil)^.Aux1), jDX);
        MovMR(jAX, PtrInt(@PObj(nil)^.Aux2), jDX);
        for L := 0 to Integer(ON[N].Imm) - 1 do MovMR(jAX, PtrInt(@PObj(nil)^.Slots) + L * 8, jDX);
        AluRR($31, jAX, jR13, True);
        MovMR(jBX, ON[N].Reg * 8, jAX);
      end;
    onElemRecv:
      begin
        MovRM(jAX, jBX, Integer(ON[N].Imm) * 8);
        AluRR($31, jAX, jR13, True);
        MovRR(jDX, jAX);
        ShiftRI(5, jDX, 48, True);
        OJccGuard(ccNE, N);
        CmpBMI(jAX, 4, ckObject);
        OJccGuard(ccNE, N);
      end;
    onCheckHole:
      begin
        MovRM(jAX, jBX, Integer(ON[N].Imm) * 8);
        MovRI(jCX, VHole);
        AluRR($39, jAX, jCX, True);
        OJccGuardFb(ccE, N, ofbProp);
      end;
    onUpvGet:
      begin
        { the closure's upvalue array (Aux2), its cell, then the register it is open on or its value (as the
          baseline's opGetUpv) }
        if ON[N].X >= 0 then MovRM(jAX, jBX, ON[N].X * 8) else MovRM(jAX, jR12, PtrInt(@PFrame(nil)^.Closure));
        MovRI(jDX, PTR_MASK);
        AluRR($21, jAX, jDX, True);
        MovRM(jAX, jAX, PtrInt(@PObj(nil)^.Aux2));
        AluRR($21, jAX, jDX, True);
        MovRM(jAX, jAX, PtrInt(@PValueArray(nil)^.Data) + Integer(ON[N].Imm) * 8);
        AluRR($21, jAX, jDX, True);
        MovRM(jCX, jAX, PtrInt(@PUpval(nil)^.StackIndex));
        AluRR($85, jCX, jCX, True);
        L := JccFwd(ccS);
        MovRFs(jDX, JitFsVStack);
        MovRMIdx(jAX, jDX, jCX, 0);
        CC := JmpFwd;
        PatchHere(L);
        MovRM(jAX, jAX, PtrInt(@PUpval(nil)^.Value));
        PatchHere(CC);
        MovMR(jBX, ON[N].Y * 8, jAX);
      end;
    onGlobalGet:
      begin
        MovRFs(jAX, JitFsGlobalObj);
        AluRR($31, jAX, jR13, True);
        MovRI(jCX, QWord(PtrUInt(OPropShape[ON[N].PC])));
        CmpRM(jCX, jAX, PtrInt(@PObj(nil)^.Shape));
        OJccGuardFb(ccNE, N, ofbProp);
        D := Integer(ON[N].Imm and $FFFFFFFF);
        if D < Integer(ON[N].Imm shr 32) then MovRM(jAX, jAX, PtrInt(@PObj(nil)^.Slots) + D * 8)
        else
        begin
          MovRM(jAX, jAX, PtrInt(@PObj(nil)^.Overflow));
          MovRM(jAX, jAX, PtrInt(@PValueArray(nil)^.Data) + (D - Integer(ON[N].Imm shr 32)) * 8);
        end;
        MovMR(jBX, ON[N].Y * 8, jAX);
      end;
    onInlGuard:
      begin
        { the callee: a function of the inlined template (as JitCallStub checks one) }
        MovRM(jAX, jBX, ON[N].Reg * 8);
        AluRR($31, jAX, jR13, True);
        MovRR(jDX, jAX);
        ShiftRI(5, jDX, 48, True);
        OJccGuardFb(ccNE, N, ofbProp);
        CmpBMI(jAX, 4, ckObject);
        OJccGuardFb(ccNE, N, ofbProp);
        CmpBMI(jAX, PtrInt(@PObj(nil)^.OKind), okFunction);
        OJccGuardFb(ccNE, N, ofbProp);
        { a class constructor (OFlags bit 0) only through new }
        if not OSites[ON[N].Imm].IsNew then
        begin
          TestBMI(jAX, PtrInt(@PObj(nil)^.OFlags), 1);
          OJccGuardFb(ccNE, N, ofbProp);
        end;
        MovRI(jCX, QWord(PtrUInt(OSites[ON[N].Imm].Tc)));
        CmpRM(jCX, jAX, PtrInt(@PObj(nil)^.Native));
        OJccGuardFb(ccNE, N, ofbProp);
        if OSites[ON[N].Imm].IsNew then
        begin
          { a constructor: XFastNew's caches hold (its shape, its prototype the new object's shape's) }
          MovRI(jCX, QWord(PtrUInt(OSites[ON[N].Imm].Tc^.CtorFnShape)));
          CmpRM(jCX, jAX, PtrInt(@PObj(nil)^.Shape));
          OJccGuardFb(ccNE, N, ofbProp);
          MovzxWRM(jCX, jAX, PtrInt(@PObj(nil)^.InlineCap));
          AluRI(7, jCX, OSites[ON[N].Imm].Tc^.CtorProtoSlot, False);
          OJccGuardFb(ccBE, N, ofbProp);
          MovRM(jCX, jAX, PtrInt(@PObj(nil)^.Slots) + OSites[ON[N].Imm].Tc^.CtorProtoSlot * 8);
          MovRI(jDX, QWord(PtrUInt(OSites[ON[N].Imm].Tc^.CtorRootShape)));
          CmpRM(jCX, jDX, PtrInt(@PShape(nil)^.Proto));
          OJccGuardFb(ccNE, N, ofbProp);
          if OSites[ON[N].Imm].FiSite >= 0 then
          begin
            { its field initializer's closure (an inline slot of the constructor's shape) into its register }
            MovzxWRM(jCX, jAX, PtrInt(@PObj(nil)^.InlineCap));
            AluRI(7, jCX, OSites[ON[N].Imm].FnCap, False);
            OJccGuardFb(ccNE, N, ofbProp);
            if OSites[ON[N].Imm].Tc^.FISlot < OSites[ON[N].Imm].FnCap then
              MovRM(jCX, jAX, PtrInt(@PObj(nil)^.Slots) + OSites[ON[N].Imm].Tc^.FISlot * 8)
            else
            begin
              { an overflow slot (the shape says the overflow store holds it) }
              MovRM(jCX, jAX, PtrInt(@PObj(nil)^.Overflow));
              MovRM(jCX, jCX, PtrInt(@PValueArray(nil)^.Data) +
                (OSites[ON[N].Imm].Tc^.FISlot - OSites[ON[N].Imm].FnCap) * 8);
            end;
            MovRR(jAX, jCX);
            AluRR($31, jAX, jR13, True);
            MovRR(jDX, jAX);
            ShiftRI(5, jDX, 48, True);
            OJccGuardFb(ccNE, N, ofbProp);
            CmpBMI(jAX, 4, ckObject);
            OJccGuardFb(ccNE, N, ofbProp);
            CmpBMI(jAX, PtrInt(@PObj(nil)^.OKind), okFunction);
            OJccGuardFb(ccNE, N, ofbProp);
            MovRI(jDX, QWord(PtrUInt(OSites[ON[N].Imm].Tc^.FieldInit)));
            CmpRM(jDX, jAX, PtrInt(@PObj(nil)^.Native));
            OJccGuardFb(ccNE, N, ofbProp);
            MovMR(jBX, OSites[OSites[ON[N].Imm].FiSite].A * 8, jCX);
          end;
        end
        { a sloppy callee's this: an object (given none, the callee does not read it: OInlTargetOf) }
        else if not OSites[ON[N].Imm].Tc^.IsStrict and (OSites[ON[N].Imm].Flag = 0) then
        begin
          MovRM(jAX, jBX, (ON[N].Reg + 1) * 8);
          AluRR($31, jAX, jR13, True);
          MovRR(jDX, jAX);
          ShiftRI(5, jDX, 48, True);
          OJccGuardFb(ccNE, N, ofbProp);
          CmpBMI(jAX, 4, ckObject);
          OJccGuardFb(ccNE, N, ofbProp);
        end;
        { room for the callee's registers, and for its frame should it deopt }
        Mov32RM(jAX, jR12, PtrInt(@PFrame(nil)^.Base));
        AluRI(7, jAX, STACK_MAX - (ON[N].Reg + 1 + OSites[ON[N].Imm].Room), False);
        OJccGuardFb(ccGE, N, ofbProp);
        Mov32RFs(jAX, JitFsFrameTop);
        AluRI(7, jAX, FRAME_MAX - 4 - OPT_INLINE_DEPTH, False);
        OJccGuardFb(ccGE, N, ofbProp);
      end;
    onMath:
      begin
        { the callee: the native function the feedback named }
        MovRM(jAX, jBX, ON[N].Reg * 8);
        AluRR($31, jAX, jR13, True);
        MovRR(jDX, jAX);
        ShiftRI(5, jDX, 48, True);
        OJccGuardFb(ccNE, N, ofbProp);
        CmpBMI(jAX, 4, ckObject);
        OJccGuardFb(ccNE, N, ofbProp);
        CmpBMI(jAX, PtrInt(@PObj(nil)^.OKind), okNative);
        OJccGuardFb(ccNE, N, ofbProp);
        MovRI(jCX, QWord(PtrUInt(OptMathFns[ON[N].Imm])));
        CmpRM(jCX, jAX, PtrInt(@PObj(nil)^.Native));
        OJccGuardFb(ccNE, N, ofbProp);
        OLoadX(0, ON[N].X);
        case ON[N].Imm of
          0: begin JB($F2); JB($0F); JB($51); JB($C0); end;                       { sqrtsd xmm0, xmm0 }
          1: begin JB($66); JB($0F); JB($3A); JB($0B); JB($C0); JB(9); end;       { roundsd xmm0, xmm0, floor }
          2: begin JB($66); JB($0F); JB($3A); JB($0B); JB($C0); JB(10); end;      { roundsd xmm0, xmm0, ceil }
          3:
            begin
              MovRI(jAX, QWord($7FFFFFFFFFFFFFFF));
              MovqXR(1, jAX);
              JB($66); JB($0F); JB($54); JB($C1);                                 { andpd xmm0, xmm1 }
            end;
        else begin JB($66); JB($0F); JB($3A); JB($0B); JB($C0); JB(11); end;      { roundsd xmm0, xmm0, trunc }
        end;
        OStoreX(N, 0);
      end;
    onLength:
      begin
        MovRM(jAX, jBX, ON[N].Reg * 8);
        AluRR($31, jAX, jR13, True);
        MovRR(jDX, jAX);
        ShiftRI(5, jDX, 48, True);
        OJccGuardFb(ccNE, N, ofbProp);
        if ON[N].Imm = LEN_STR then
        begin
          CmpBMI(jAX, 4, ckString);
          OJccGuardFb(ccNE, N, ofbProp);
          Mov32RM(jAX, jAX, PtrInt(@PStr(nil)^.Len));
        end
        else
        begin
          CmpBMI(jAX, 4, ckObject);
          OJccGuardFb(ccNE, N, ofbProp);
          CmpBMI(jAX, PtrInt(@PObj(nil)^.OKind), okArray);
          OJccGuardFb(ccNE, N, ofbProp);
          TestBMI(jAX, PtrInt(@PObj(nil)^.OFlags), ofBigLen);
          OJccGuardFb(ccNE, N, ofbProp);
          MovRM(jAX, jAX, PtrInt(@PObj(nil)^.Elements));
          Mov32RM(jAX, jAX, PtrInt(@PValueArray(nil)^.Len));
        end;
        OStoreG(N, jAX);
      end;
    onArrGet:
      begin
        OElemReceiver(N);
        MovRM(jDX, jAX, PtrInt(@PObj(nil)^.Elements));
        AluRR($85, jDX, jDX, True);
        OJccGuardFb(ccE, N, ofbElem);
        OLoadG(jCX, ON[N].X);
        Cmp32RM(jCX, jDX, PtrInt(@PValueArray(nil)^.Len));
        OJccGuardFb(ccAE, N, ofbElem);
        JRex(False, jCX, 0, jCX); JB($8B); JModRR(jCX, jCX);
        MovRMIdx(jAX, jDX, jCX, PtrInt(@PValueArray(nil)^.Data));
        MovRI(jDX, VHole);
        AluRR($39, jAX, jDX, True);
        LH := JccFwd(ccNE);
        { a hole: OptHoleRead's undefined when no index-keyed property on the way can answer it, else the exit
          (rsi, rdi, r8 - r11 and xmm2 - xmm15 kept as OBarrier keeps them; rcx still the index) }
        JB($56); JB($57); JB($41); JB($50); JB($41); JB($51); JB($41); JB($52); JB($41); JB($53);
        AluRI(5, jSP, 112, True);
        for KH := 0 to 13 do OSseM($F2, $11, 2 + KH, jSP, KH * 8);
        MovRM(jDI, jBX, ON[N].Reg * 8);
        AluRR($31, jDI, jR13, True);
        JRex(False, jSI, 0, jCX); JB($8B); JModRR(jSI, jCX);   { mov esi, ecx }
        CallAbs(@OptHoleRead);
        for KH := 0 to 13 do OSseM($F2, $10, 2 + KH, jSP, KH * 8);
        AluRI(0, jSP, 112, True);
        JB($41); JB($5B); JB($41); JB($5A); JB($41); JB($59); JB($41); JB($58); JB($5F); JB($5E);
        MovRI(jDX, VHole);
        AluRR($39, jAX, jDX, True);
        OJccGuardFb(ccE, N, ofbElem);
        PatchHere(LH);
        MovMR(jBX, ON[N].Y * 8, jAX);
      end;
    onArrSet:
      begin
        { the value boxed first (OBox uses rcx), parked in the scratch slot }
        if ON[N].Y >= 0 then
        begin
          OBox(ON[N].Y);
          MovMR(jSP, OScratch, jAX);
        end;
        OElemReceiver(N);
        MovRM(jDX, jAX, PtrInt(@PObj(nil)^.Elements));
        AluRR($85, jDX, jDX, True);
        OJccGuardFb(ccE, N, ofbElem);
        TestBMI(jAX, PtrInt(@PObj(nil)^.OFlags), 64);
        OJccGuardFb(ccNE, N, ofbElem);
        OLoadG(jCX, ON[N].X);
        Cmp32RM(jCX, jDX, PtrInt(@PValueArray(nil)^.Len));
        OJccGuardFb(ccAE, N, ofbElem);
        JRex(False, jCX, 0, jCX); JB($8B); JModRR(jCX, jCX);
        { rcx the cell's address }
        SibOp(0, True, [$8D], jCX, jDX, jCX, 3, PtrInt(@PValueArray(nil)^.Data));   { lea rcx, [rdx + rcx * 8 + Data] }
        MovRI(jAX, VHole);
        CmpRM(jAX, jCX, 0);
        LH := JccFwd(ccNE);
        { a hole: OptHoleStore's store when the array's prototypes leave it to a dense store, else the exit (rsi,
          rdi, r8 - r11 and xmm2 - xmm15 kept as OBarrier keeps them) }
        AluRR($29, jCX, jDX, True);                                { sub rcx, rdx }
        AluRI(5, jCX, PtrInt(@PValueArray(nil)^.Data), True);
        ShiftRI(5, jCX, 3, True);                                  { rcx: the index }
        if ON[N].Y >= 0 then MovRM(jDX, jSP, OScratch) else MovRM(jDX, jBX, (-1 - ON[N].Y) * 8);
        JB($56); JB($57); JB($41); JB($50); JB($41); JB($51); JB($41); JB($52); JB($41); JB($53);
        AluRI(5, jSP, 112, True);
        for KH := 0 to 13 do OSseM($F2, $11, 2 + KH, jSP, KH * 8);
        MovRM(jDI, jBX, ON[N].Reg * 8);
        AluRR($31, jDI, jR13, True);
        JRex(False, jSI, 0, jCX); JB($8B); JModRR(jSI, jCX);   { mov esi, ecx }
        CallAbs(@OptHoleStore);
        for KH := 0 to 13 do OSseM($F2, $10, 2 + KH, jSP, KH * 8);
        AluRI(0, jSP, 112, True);
        JB($41); JB($5B); JB($41); JB($5A); JB($41); JB($59); JB($41); JB($58); JB($5F); JB($5E);
        JB($84); JB($C0);                                          { test al, al }
        OJccGuardFb(ccE, N, ofbElem);
        L := JmpFwd;
        PatchHere(LH);
        if ON[N].Y >= 0 then MovRM(jAX, jSP, OScratch) else MovRM(jAX, jBX, (-1 - ON[N].Y) * 8);
        MovMR(jCX, 0, jAX);
        { a register's value may point into the nursery (a node's boxed number cannot) }
        if ON[N].Y < 0 then OBarrier;
        PatchHere(L);
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
        { in an inlined callee: the frame's inline descriptor its frames' (what it calls sees them, InlinedFrame) }
        if OVFrame[ON[N].PC] >= 0 then
        begin
          MovRI(jAX, QWord(PtrUInt(OInlDesc(OVFrame[ON[N].PC]))));
          MovMR(jR12, PtrInt(@PFrame(nil)^.Inl), jAX);
        end;
        JitInstr(OT, ON[N].PC, OIns(ON[N].PC));
        { the continuation: where its slow blocks come back to (JmpPC to the next instruction), the descriptor
          cleared }
        JitNative[ON[N].PC + 1] := JitLen;
        if OVFrame[ON[N].PC] >= 0 then Mov64MI(jR12, PtrInt(@PFrame(nil)^.Inl), 0);
      end;
    onLoadKTag:
      begin
        MovRI(jAX, QWord(ON[N].Imm));
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

function XOptInlDeopt(Fr: PFrame; D: POInlDesc; Q: Integer): Integer; forward;

{ G2_OPTLOG: each exit that records feedback, as it is taken (the template, the virtual and template PCs, the
  feedback bits) }
procedure XOptLogDeopt(T: PFuncTemplate; VPC, PC, Fb: Integer; Had: PByte);
begin
  if T^.Name <> nil then Write(ErrOutput, 'deopt ', UTF8Encode(StrToUnicode(T^.Name))) else Write(ErrOutput, 'deopt ?');
  WriteLn(ErrOutput, ' vpc ', VPC, ' pc ', PC, ' fb ', Fb, ' had ', Had^);
end;

{ the frame state written back, the stack restored, then the baseline code at PC (the interpreter when it has none);
  Feedback the bits recorded for PC (0 none: a planned exit) }
{ what new feedback weighs in deopts (past OPT_MAX_DEOPTS the regions go): all of them for a region of up to
  OPT_FB_NODES nodes, proportionally less for a bigger one, OPT_FB_MIN at least }
function OFbWeight: Integer;
begin
  if ONCount <= OPT_FB_NODES then Result := OPT_MAX_DEOPTS + 1
  else Result := Max(OPT_FB_MIN, ((OPT_MAX_DEOPTS + 1) * OPT_FB_NODES) div ONCount);
end;

procedure OLeave(FS, FSN, PC, Feedback, EpiAt: Integer);
var K, L1, L2, L3, S, RPC: Integer; Base: PtrUInt;
begin
  { (an exit from an inlined callee's embedded instruction: its descriptor cleared) }
  if (PC >= 0) and (PC <= High(OVFrame)) and (OVFrame[PC] >= 0) then Mov64MI(jR12, PtrInt(@PFrame(nil)^.Inl), 0);
  for K := FS to FS + FSN - 1 do
  begin
    OBox(OFS[K].Node);
    MovMR(jBX, OFS[K].Reg * 8, jAX);
  end;
  if OptLog and (Feedback <> 0) and (PC >= 0) and (PC < OVLen) then
  begin
    MovRI(jDI, QWord(PtrUInt(OVTpl[PC])));
    MovRI(jSI, Cardinal(PC));
    MovRI(jDX, Cardinal(OVPC[PC]));
    MovRI(jCX, Cardinal(Feedback));
    MovRI(jR8, QWord(PtrUInt(OVFb[PC])));
    CallAbs(@XOptLogDeopt);
  end;
  if Feedback <> 0 then
  begin
    { new feedback: recorded, and it weighs OFbWeight deopts (a small region compiles again at once; a big one,
      whose compilation costs far more than its deopts, learns several things first). Feedback already recorded
      weighs as much: the region was compiled before it was learned (its speculation reads the same bits), so it
      takes the same exit each time until it goes: when it was learned after this region was compiled (from
      another region, or an earlier exit of this one), the region goes at once }
    MovRI(jAX, QWord(PtrUInt(OVFb[PC])));
    if (OFb(PC) and Feedback) = 0 then
    begin
      JB($F6); JB($00); JB(Byte(Feedback));               { test byte [rax], Feedback }
      L1 := JccFwd(ccE);
      MovRI(jCX, QWord(PtrUInt(@OInfo^.Deopts)));
      JB($81); JB($01); JD(OPT_MAX_DEOPTS);               { add dword [rcx], OPT_MAX_DEOPTS }
      PatchHere(L1);
    end;
    JB($80); JB($08); JB(Byte(Feedback));                 { or byte [rax], Feedback }
    MovRI(jAX, QWord(PtrUInt(@OInfo^.FbDrop)));
    Mov32MI(jAX, 0, 1);
    MovRI(jAX, QWord(PtrUInt(@OInfo^.Deopts)));
    JB($81); JB($00); JD(OFbWeight - 1);                  { add dword [rax], OFbWeight - 1 }
    MovRI(jAX, QWord(PtrUInt(@OInfo^.Deopts)));
    JB($FF); JB($00);                                     { inc dword [rax] }
    { past the bound: the regions go }
    JB($81); JB($38); JD(OPT_MAX_DEOPTS);                 { cmp dword [rax], OPT_MAX_DEOPTS }
    L3 := JccFwd(ccLE);
    { the baseline's table back (a function entry's region is entered from it, with no count-down of its own) and
      its count at 1: the next count-down, a dec to zero, has OptMaybe drop the regions }
    MovRI(jAX, QWord(PtrUInt(@OInfo^.Count)));
    Mov32MI(jAX, 0, 1);
    MovRI(jAX, QWord(PtrUInt(@OT^.JitTable)));
    MovRI(jCX, QWord(PtrUInt(OInfo^.BaseTable)));
    MovMR(jAX, 0, jCX);
    PatchHere(L3);
  end;
  AluRI(0, jSP, OFrame, True);
  JB($41); JB($5F);                                       { pop r15 }
  if OVFrame[PC] >= 0 then
  begin
    { in an inlined callee: its frames run by the interpreter (XOptInlDeopt), then the caller after the call }
    S := OVFrame[PC];
    MovRR(jDI, jR12);
    MovRI(jSI, QWord(PtrUInt(OInlDesc(S))));
    MovRI(jDX, Cardinal(OVPC[PC]));
    CallAbs(@XOptInlDeopt);
    AluRR($85, jAX, jAX, False);
    L1 := JccFwd(ccE);
    MovRI(jAX, $FFFFFFFF);
    JB($E9); JD(Cardinal(EpiAt - (JitLen + 4)));
    PatchHere(L1);
    RPC := OSites[S].K + 1;
  end
  else RPC := OVPC[PC];
  Base := PPtrUInt(OInfo^.BaseTable)[RPC];
  if Base <> 0 then
  begin
    MovRI(jAX, QWord(Base));
    JB($FF); JB($E0);                                     { jmp rax }
  end
  else
  begin
    MovRI(jAX, QWord(RPC));
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

{ block B's strict equality with a tagged operand into ecx 1 / 0 (then tested): the same bits are equal, two
  objects of different bits are not, a pointer against a non-pointer is not; a double, or two other pointers
  (strings, BigInts), StrictEquals's (every allocatable register kept around the call) }
procedure OTagCmp(B: Integer);
var LNe, LDone, LDone2, L1, L2, LSlow1, LSlow2, LSlow3, LSlow4, K: Integer;
  procedure Load(V: Integer);
  begin
    if V >= 0 then OBox(V) else MovRM(jAX, jBX, (-1 - V) * 8);
  end;
begin
  Load(OB[B].TermX);
  MovRR(jDX, jAX);
  Load(OB[B].TermY);
  MovRI(jCX, DOUBLE_LIMIT);
  AluRR($39, jDX, jCX, True);
  LSlow1 := JccFwd(ccB);
  AluRR($39, jAX, jCX, True);
  LSlow2 := JccFwd(ccB);
  AluRR($39, jDX, jAX, True);
  LNe := JccFwd(ccNE);
  MovRI(jCX, 1);
  LDone := JmpFwd;
  PatchHere(LNe);
  { different bits (no doubles): unequal unless both are pointers to strings or BigInts with equal contents, so
    a side that is not a pointer, or is an object, decides it (the left checked first, then the right) }
  MovRR(jCX, jDX);
  ShiftRI(5, jCX, 48, True);
  AluRI(7, jCX, TAG_PTR_HI, False);
  L1 := JccFwd(ccNE);
  MovRR(jCX, jDX);
  AluRR($31, jCX, jR13, True);
  CmpBMI(jCX, 4, ckObject);
  LSlow3 := JccFwd(ccE);
  MovRR(jCX, jAX);
  ShiftRI(5, jCX, 48, True);
  AluRI(7, jCX, TAG_PTR_HI, False);
  L2 := JccFwd(ccNE);
  MovRR(jCX, jAX);
  AluRR($31, jCX, jR13, True);
  CmpBMI(jCX, 4, ckObject);
  LSlow4 := JccFwd(ccNE);
  PatchHere(L1); PatchHere(L2); PatchHere(LSlow3);
  AluRR($31, jCX, jCX, False);
  LDone2 := JmpFwd;
  PatchHere(LSlow1); PatchHere(LSlow2); PatchHere(LSlow4);
  { push rsi, rdi, r8 - r11 (rsp stays aligned), xmm2 - xmm15 below them }
  JB($56); JB($57); JB($41); JB($50); JB($41); JB($51); JB($41); JB($52); JB($41); JB($53);
  AluRI(5, jSP, 112, True);
  for K := 0 to 13 do OSseM($F2, $11, 2 + K, jSP, K * 8);
  MovRR(jDI, jDX);
  MovRR(jSI, jAX);
  CallAbs(@StrictEquals);
  JB($0F); JB($B6); JB($C8);                       { movzx ecx, al }
  for K := 0 to 13 do OSseM($F2, $10, 2 + K, jSP, K * 8);
  AluRI(0, jSP, 112, True);
  JB($41); JB($5B); JB($41); JB($5A); JB($41); JB($59); JB($41); JB($58); JB($5F); JB($5E);
  PatchHere(LDone); PatchHere(LDone2);
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
var K, J, B, Epi, RestEpi, ThrowL, G, F: Integer; Labels: array of Integer; GuardAt, ExitAt, FbAt: array of Integer;
  Code: PByte; L1, L2, L3, L4: Integer;
  D: Cardinal; Neg: Boolean; CC, T: Integer;
begin
  Result := nil;
  JitLen := 0;
  OGJCount := 0;
  OBranchFixCount := 0;
  OColdCount := 0;
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
  SetLength(JitNative, OVLen + 2);
  for K := 0 to OVLen + 1 do JitNative[K] := -1;
  { the entry: r15 saved, the spill frame }
  JB($41); JB($57);                                       { push r15 }
  AluRI(5, jSP, OFrame, True);
  for K := 0 to OOrderCount - 1 do
  begin
    B := OOrder[K];
    Labels[B] := JitLen;
    for J := 0 to High(OB[B].Nodes) do OCode(OB[B].Nodes[J]);
    if OB[B].TermPoll then
    begin
      { cmp dword fs:[Interrupt], 0; jne: the block's exit at its back edge }
      JB($64); JB($83); JB($3C); JB($25); JD(Cardinal(JitFsInterrupt)); JB(0);
      OJccGuard(ccNE, -1 - B);
    end;
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
            otbTpl:
              begin
                { ecx 1 unless the register holds a function of the template }
                MovRM(jAX, jBX, OB[B].TermX * 8);
                AluRR($31, jAX, jR13, True);
                MovRI(jCX, 1);
                MovRR(jDX, jAX);
                ShiftRI(5, jDX, 48, True);
                L1 := JccFwd(ccNE);
                CmpBMI(jAX, 4, ckObject);
                L2 := JccFwd(ccNE);
                CmpBMI(jAX, PtrInt(@PObj(nil)^.OKind), okFunction);
                L3 := JccFwd(ccNE);
                MovRI(jDX, QWord(PtrUInt(OVTpl[OB[B].EndPC])));
                CmpRM(jDX, jAX, PtrInt(@PObj(nil)^.Native));
                L4 := JccFwd(ccNE);
                AluRR($31, jCX, jCX, False);
                PatchHere(L1); PatchHere(L2); PatchHere(L3); PatchHere(L4);
                AluRR($85, jCX, jCX, False);
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
  OEmitCold;
  { the embedded instructions' slow blocks }
  JitEmitDeferred(OT);
  { the epilogue (an exit to the interpreter: eax its PC) }
  Epi := JitLen;
  JB($41); JB($5E); JB($41); JB($5D); JB($5D); JB($41); JB($5C); JB($5B); JB($C3);
  { the embedded code's epilogue (eax its result) and throw exit: the frame and r15 first }
  RestEpi := JitLen;
  if OSiteCount > 0 then Mov64MI(jR12, PtrInt(@PFrame(nil)^.Inl), 0);
  AluRI(0, jSP, OFrame, True);
  JB($41); JB($5F);
  JB($E9); JD(Cardinal(Epi - (JitLen + 4)));
  ThrowL := JitLen;
  MovRI(jAX, $FFFFFFFF);
  JB($E9); JD(Cardinal(RestEpi - (JitLen + 4)));
  { the cold stubs: one per guard node and per exit block }
  SetLength(GuardAt, ONCount + OBCount);
  for K := 0 to High(GuardAt) do GuardAt[K] := -1;
  SetLength(FbAt, OGJCount);
  for K := 0 to OGJCount - 1 do
  begin
    G := OGJ[K].G;
    if G >= 0 then J := G else J := ONCount + (-1 - G);
    if OGJ[K].Fb <> 0 then
    begin
      { a stub per guard and feedback (a plain exit: none): the node's other jumps with that feedback share it }
      FbAt[K] := -1;
      F := K - 1;
      while (F >= 0) and (F >= K - 64) do
      begin
        if (OGJ[F].G = G) and (OGJ[F].Fb = OGJ[K].Fb) then begin FbAt[K] := FbAt[F]; Break; end;
        Dec(F);
      end;
      if FbAt[K] < 0 then
      begin
        FbAt[K] := JitLen;
        if OGJ[K].Fb = OFB_EXIT then OLeave(ON[G].FS, ON[G].FSN, ON[G].PC, 0, Epi)
        else OLeave(ON[G].FS, ON[G].FSN, ON[G].PC, OGJ[K].Fb, Epi);
      end;
      D := Cardinal(FbAt[K] - (OGJ[K].At + 4));
      PCardinal(@JitBuf[OGJ[K].At])^ := D;
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
  SetLength(ExitAt, OVLen + 1);
  for K := 0 to OVLen do ExitAt[K] := -1;
  for K := 0 to JitFixCount - 1 do
  begin
    T := JitFixes[K].Target;
    if T = JIT_EPILOGUE then J := RestEpi
    else if T = JIT_THROW then J := ThrowL
    else if T < 0 then
    begin
      F := -1 - T;
      if (F > OVLen) or (OEmbedNode[F] < 0) then Exit;
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
      if (T > OVLen + 1) or (JitNative[T] < 0) then Exit;
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
  if (JitDebugFlags and 1) <> 0 then JitMapEntry(Code, JitLen, OT, 'opt');
  if OptLog and (GetEnvironmentVariable('G2_OPTDUMP') <> '') then OptDump(Code);
end;

function OInlEligible(T: PFuncTemplate; ForNew: Boolean): Boolean; forward;
function OInlOk(Op: TOp): Boolean; forward;
function OptName(T: PFuncTemplate): string; forward;

{ what register R holds at PC K of template T as its inline caches tell, with no frame (VUndefined unknown): a
  move's source, a cached global, a cached property of such a value, a prototype's cached property }
function OStaticValue(T: PFuncTemplate; Clo: PObj; K, R, Depth: Integer): TValue;
var P, E, D: Integer; I: QWord; Op: TOp; IC: PPropIC; En: PICEntry; O: PObj; V, Recv: TValue;
begin
  Result := VUndefined;
  if Depth > 4 then Exit;
  P := K - 1;
  while P >= 0 do
  begin
    I := T^.Code[P];
    Op := OOpOf(I);
    if (Op = opJmp) or OIsCondJump(Op) or (K - P > 16) then Exit;
    if ((Op = opGetMethod) and ((OA(I) = R) or (OA(I) + 1 = R))) or (OWrites(I) = R) then Break;
    Dec(P);
  end;
  if P < 0 then Exit;
  case Op of
    opMov: Result := OStaticValue(T, Clo, P, OB_(I), Depth + 1);
    opGetUpv:
      begin
        { the closure's upvalue (when the closure is known): open, its register's value }
        if (Clo = nil) or not IsPtr(Clo^.Aux2) then Exit;
        V := PValueArray(PtrOf(Clo^.Aux2))^.Data[OB_(I)];
        if not IsPtr(V) then Exit;
        if PUpval(PtrOf(V))^.StackIndex >= 0 then Result := VStack[PUpval(PtrOf(V))^.StackIndex]
        else Result := PUpval(PtrOf(V))^.Value;
      end;
    opGetGlobal:
      begin
        IC := @T^.ICs[OCU(I)];
        O := ObjOf(GlobalObj);
        if (IC^.Shape <> nil) and (IC^.Shape = O^.Shape) and (IC^.Slot >= 0) then Result := GetSlot(O, IC^.Slot);
      end;
    opGetProp, opGetMethod:
      begin
        if (Op = opGetMethod) and (R = OA(I) + 1) then Exit;
        IC := @T^.ICs[OCU(I)];
        if not OICOk(IC) then Exit;
        { every entry's value the same one }
        Recv := VUndefined;
        for E := 0 to IC^.PolyCount - 1 do
        begin
          En := OICEntry(IC, E);
          if En^.Depth = 0 then
          begin
            if Recv = VUndefined then Recv := OStaticValue(T, Clo, P, OB_(I), Depth + 1);
            if not IsObject(Recv) or (ObjOf(Recv)^.Shape <> En^.Shape) then Exit(VUndefined);
            V := GetSlot(ObjOf(Recv), En^.Slot);
          end
          else
          begin
            V := En^.Shape^.Proto;
            for D := 0 to En^.Depth - 1 do
            begin
              if not IsObject(V) or (ObjOf(V)^.Shape <> En^.Holder[D]) then Exit(VUndefined);
              if D < En^.Depth - 1 then V := ObjOf(V)^.Shape^.Proto;
            end;
            V := GetSlot(ObjOf(V), En^.Slot);
          end;
          if (E > 0) and (V <> Result) then Exit(VUndefined);
          Result := V;
        end;
      end;
  end;
end;

{ whether the call at PC K of template T leaves no call behind when the tier takes it: a Math native computed
  inline, or a callee that can be inlined whose own calls do the same (Depth inlined callers) }
function OCallGoes(T: PFuncTemplate; Clo: PObj; K, Depth: Integer): Boolean;
var V: TValue; O: PObj; R: Integer; C: PFuncTemplate;
begin
  Result := False;
  V := OStaticValue(T, Clo, K, OA(T^.Code[K]), 0);
  if not IsObject(V) then Exit;
  O := ObjOf(V);
  if O^.OKind = okNative then
  begin
    for R := 0 to High(OptMathFns) do
      if (OptMathFns[R] <> nil) and (O^.Native = OptMathFns[R]) and (OB_(T^.Code[K]) = 1) and
         (OOpOf(T^.Code[K]) = opCall) then Exit(True);
    Exit;
  end;
  if (O^.OKind <> okFunction) or (((O^.OFlags and 1) <> 0) and (OOpOf(T^.Code[K]) <> opNew)) or
     (Depth >= OPT_INLINE_DEPTH) then Exit;
  C := PFuncTemplate(O^.Native);
  if (C = OT) or (C = T) or not OInlEligible(C, OOpOf(T^.Code[K]) = opNew) then Exit;
  if (OOpOf(T^.Code[K]) = opNew) and (Pointer(O^.Shape) <> C^.CtorFnShape) then Exit;
  for R := 0 to C^.CodeLen - 1 do
    if (OOpOf(C^.Code[R]) in [opCall, opNew]) and not OCallGoes(C, O, R, Depth + 1) then Exit;
  Result := True;
end;

{ how the call at PC K of the template counts for OQuickProfitable: 2 one OCallGoes takes (its call gone), else -2
  (an embedded call clobbers every register the region holds values in) }
function OQuickCall(K: Integer): Integer;
begin
  if OCallGoes(OT, OQClo, K, 0) then Result := OPT_INL_GAIN
  else if OQStatic and not IsObject(OStaticValue(OT, OQClo, K, OA(OT^.Code[K]), 0)) then Result := OPT_INL_GAIN
  else Result := -2;
end;

{ a first estimate before any analysis (most regions fail it): the instructions reachable from PC, the numeric
  ones (arithmetic, comparisons, numeric branches) at least as many as the others that do work }
{ a loop header PC's loop: its last back edge's PC (-1: PC is no loop header) }
function OLoopEnd(T: PFuncTemplate; PC: Integer): Integer;
var K: Integer;
begin
  Result := -1;
  if PC <= 0 then Exit;
  for K := PC to T^.CodeLen - 1 do
    if (InsOp(T^.Code[K]) = opJmp) and (K + 1 + Integer(SarInt64(Int64(T^.Code[K]), 40)) = PC) then Result := K;
end;

{ whether template T's code from PC Lo to PC Hi suspends (a generator's yield, an await): the region exits there }
function OSuspendsIn(T: PFuncTemplate; Lo, Hi: Integer): Boolean;
var K: Integer;
begin
  Result := False;
  if not (T^.IsGenerator or T^.IsAsync) then Exit;
  for K := Lo to Hi do
    if InsOp(T^.Code[K]) in [opYield, opYieldStar, opYieldStarAsync, opAwait] then Exit(True);
end;

{ the code a loop header PC's region spans: the outermost loop around it that does not suspend, Lo its header ..
  Hi its last back edge (False: PC is no loop header) }
function OLoopSpan(T: PFuncTemplate; PC: Integer; out Lo, Hi: Integer): Boolean;
var K, H, E: Integer; Grew: Boolean;
begin
  Hi := OLoopEnd(T, PC);
  Lo := PC;
  Result := Hi >= 0;
  if not Result then Exit;
  repeat
    Grew := False;
    { a back edge past Hi to a header before Lo encloses the span }
    for K := Hi + 1 to T^.CodeLen - 1 do
      if InsOp(T^.Code[K]) = opJmp then
      begin
        H := K + 1 + Integer(SarInt64(Int64(T^.Code[K]), 40));
        if (H <= Lo) and (H > 0) then
        begin
          E := OLoopEnd(T, H);
          if (E > Hi) and not OSuspendsIn(T, H, E) then begin Lo := H; Hi := E; Grew := True; Break; end;
        end;
      end;
  until not Grew;
end;

function OQuickProfitable(PC: Integer): Boolean;
var Seen: array of Boolean; Work: array of Integer; NW, P, T, Num, Other, Props, R, LBeg, LEnd: Integer; I: QWord;
  Op: TOp; Loop: Boolean;
begin
  SetLength(Seen, OT^.CodeLen + 1);
  SetLength(Work, OT^.CodeLen + 1);
  NW := 0; Work[NW] := PC; Inc(NW);
  Num := 0; Other := 0; Props := 0; Loop := False;
  { a loop's region spans the outermost loop around it (its exits leave to the baseline code) }
  if not OLoopSpan(OT, PC, LBeg, LEnd) then begin LBeg := 0; LEnd := OT^.CodeLen - 1; end;
  while NW > 0 do
  begin
    Dec(NW);
    P := Work[NW];
    while (P < OT^.CodeLen) and (P >= LBeg) and (P <= LEnd) and not Seen[P] do
    begin
      Seen[P] := True;
      I := OT^.Code[P];
      Op := OOpOf(I);
      case Op of
        opAdd, opSub, opMul, opDiv, opMod, opBitAnd, opBitOr, opBitXor, opShl, opShr, opUShr, opAddI, opSubI, opIncI,
        opBitAndI, opBitOrI, opBitXorI, opShlI, opShrI, opUShrI, opNeg, opBitNot, opLt, opLe, opGt, opGe,
        opJNLt, opJNLe, opJNGt, opJNGe: Inc(Num);
        opNop, opMov, opLoadK, opLoadI, opLoadUndef, opLoadNull, opLoadTrue, opLoadFalse, opLoadHole, opJmp, opJT,
        opJF, opJNullish, opJNotNullish, opJUndef, opJNotUndef, opJNSEq, opJSEq, opSEq, opSNe, opNot, opRet,
        opRetUndef, opToNum, opToNumeric, opGetUpv, opCheckHole: ;
        opGetProp, opSetProp, opGetLength, opGetMethod: Inc(Props);
        { an embedded call clobbers every register the region holds values in; an inlined one gains its call }
        opCall: begin R := OQuickCall(P); if R > 0 then Inc(Num, R) else Inc(Other, -R); end;
        opNew: begin R := OQuickCall(P); if R > 0 then Inc(Num, R) else Inc(Other, -R); end;
        opNewObj: Inc(Num);
        opDefProp: ;
      else Inc(Other);
      end;
      if Op in [opRet, opRetUndef, opThrow, opThrowConst] then Break;
      if (Op = opJmp) or OIsCondJump(Op) then
      begin
        T := P + 1 + OC(I);
        if T <= P then Loop := True;
        if (T >= 0) and (T < OT^.CodeLen) and not Seen[T] then begin Work[NW] := T; Inc(NW); end;
        if Op = opJmp then Break;
      end;
      Inc(P);
    end;
  end;
  { native property access pays off over a loop's iterations; a short straight-line region's entry costs more
    than it saves (an inlining candidate instead) }
  if not Loop and not OQStatic then Inc(Other, Props);
  Result := (Num > 0) and (Num >= Other);
end;

{ whether T's registers are only ever captured by its nested functions' upvalues (no arguments object, field
  initializer, direct eval or top-level lexical cells): the tier keeps those registers in VStack }
function OptCapturesOk(T: PFuncTemplate): Boolean;
var I: Integer;
begin
  Result := (T^.FieldInit = nil) and (T^.ArgsReg < 0) and not T^.TopLevel;
  if Result then
    for I := 0 to T^.CodeLen - 1 do
      if InsOp(T^.Code[I]) in [opEvalHint, opMakeGLex, opMakeModCell] then Exit(False);
end;

{ whether any region of T could pass OQuickProfitable (from PC 0 or a loop's header): a template where none can gets
  no counters, so its calls and loops pay nothing for the tier }
function OptPromising(T: PFuncTemplate): Boolean;
var K, Tg: Integer; I: QWord;
begin
  OT := T;
  OQClo := nil;
  if T^.CodeLen = 0 then Exit(False);
  { its callees are not known yet (its ICs are empty): each counts as one that inlines, OptCompile's own
    estimate decides with them known }
  OQStatic := True;
  try
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
  finally
    OQStatic := False;
  end;
end;

{ whether the region gains over the baseline code: its native number work (arithmetic, comparisons, numeric
  branches) at least its embedded instructions and unboxes, which cost about what the baseline's do plus their
  flushes; a region that is mostly the baseline's instructions is left to the baseline }
function OProfitable: Boolean;
var B, K, N, Gain, Cost, Calls, Lo, Hi, P: Integer; Loop: Boolean;
begin
  Gain := 0; Cost := 0; Loop := False; Calls := 0;
  { a loop's region: the work in the outermost loop around its header counts (what runs once after it does not) }
  if not OLoopSpan(OT, OEntry, Lo, Hi) then begin Lo := 0; Hi := MaxInt; end;
  for B := 0 to OBCount - 1 do
  begin
    P := OB[B].StartPC;
    if (P >= 0) and (P < OVLen) and (OVRole[P] <> 0) and (OVSite[P] >= 0) then P := OSites[OVSite[P]].K
    else if (P >= 0) and (P < OVLen) and (OVFrame[P] >= 0) then P := OSites[OVFrame[P]].K;
    if (P >= 0) and (P <= OT^.CodeLen) and ((P < Lo) or (P > Hi)) then Continue;
    for K := 0 to High(OB[B].Nodes) do
    begin
      N := OB[B].Nodes[K];
      case ON[N].Op of
        onAddI, onSubI, onMulI, onModI, onNegI, onAndI, onOrI, onXorI, onShlI, onShrI, onUShrI, onNotI,
        onAddD, onSubD, onMulD, onDivD, onNegD, onCmpI, onCmpD, onNotB, onTAGet, onTASet, onArrGet, onArrSet, onLength, onMath: Inc(Gain);
        onUnbox, onFlush: Inc(Cost);
        { an inlined call: its call and return gone; an allocation or add without the baseline's helper }
        onInlGuard, onNewObj: Inc(Gain, OPT_INL_GAIN);
        onPropSet: if (ON[N].Imm and PROP_ADD) <> 0 then Inc(Gain);
        { an embedded call clobbers every register the region holds values in (they move to r15 or slots) }
        onEmbed:
          if OOpOf(OIns(ON[N].PC)) in [opCall, opNew, opGetMethod] then
          begin
            Inc(Cost, 2);
            if OOpOf(OIns(ON[N].PC)) <> opGetMethod then Inc(Calls);
          end
          else Inc(Cost);
      end;
    end;
    if (OB[B].Term = otBr) and (OB[B].TermTest in [otbCmpI, otbCmpD, otbTruthyI, otbTruthyD, otbTruthyB]) then Inc(Gain);
    if OB[B].Term <> otExit then
      for K := 0 to OB[B].NSucc - 1 do
        if OB[OB[B].Succ[K]].RPO <= OB[B].RPO then Loop := True;
  end;
  { a region without a loop that makes calls: its entry and the moves around each call are paid every time, so
    it must gain twice what it costs }
  if not Loop and (Calls > 0) then Cost := Cost * 2;
  Result := (Gain > 0) and (Gain >= Cost);
  if OptLog then WriteLn(ErrOutput, 'profit ', OptName(OT), ' pc ', OEntry, ' gain ', Gain, ' cost ', Cost, ' loop ', Loop);
end;

{ the slot get-IC entry E names of receiver V (of E's shape), along its prototypes as far as their shapes are E's
  holders (VUndefined when one is not) }
function OHolderSlot(V: TValue; E: PICEntry): TValue;
var O: PObj; D: Integer;
begin
  Result := VUndefined;
  O := ObjOf(V);
  for D := 0 to E^.Depth - 1 do
  begin
    if not IsObject(O^.Shape^.Proto) then Exit;
    O := ObjOf(O^.Shape^.Proto);
    if O^.Shape <> E^.Holder[D] then Exit;
  end;
  Result := GetSlot(O, E^.Slot);
end;

{ whether template T's code from PC W + 1 to PC K is entered only by falling through from W: no jump outside it
  (nor a handler) targets it }
function OSingleEntry(T: PFuncTemplate; W, K: Integer): Boolean;
var J, Tg: Integer; Op: TOp;
begin
  Result := False;
  if not T^.NoHandlers then Exit;
  for J := 0 to T^.CodeLen - 1 do
  begin
    Op := InsOp(T^.Code[J]);
    if (Op = opJmp) or OIsCondJump(Op) or (Op in [opIterNext, opIterUnpack, opAsyncIterReturn]) then
    begin
      Tg := J + 1 + Integer(SarInt64(Int64(T^.Code[J]), 40));
      if (Tg > W) and (Tg <= K) and not ((J > W) and (J < K)) then Exit;
    end;
  end;
  Result := True;
end;

{ the instruction before K that last wrote register R on every path to K (-1 none found): straight-line code, or
  past branches when the code from the writer to K is entered only through the writer }
function OWriterOf(K, R: Integer): Integer;
var P, N: Integer; I: QWord; Op: TOp; Rd: array[0..63] of Integer; NR, WF, WT: Integer; Crossed: Boolean;
  function Found(W: Integer): Integer;
  begin
    if Crossed and ((OVTpl[W] <> OVTpl[K]) or (OVFrame[W] <> OVFrame[K]) or
       not OSingleEntry(OVTpl[K], OVPC[W], OVPC[K])) then Result := -1
    else Result := W;
  end;
begin
  Result := -1;
  P := K - 1;
  N := 0;
  Crossed := False;
  while (P >= 0) and (N < 32) do
  begin
    I := OIns(P);
    Op := OOpOf(I);
    if OVRole[P] = 1 then
    begin
      { an inlined call's guard: the registers as the caller set them up before its call }
      if Crossed then Exit;
      P := OSites[OVSite[P]].VCall - 1;
      Inc(N);
      Continue;
    end;
    if OVRole[P] = 3 then
    begin
      { an inlined call: the callee writes its result register and its frame's }
      if R >= OSites[OVSite[P]].A then Exit;
      Dec(P);
      Inc(N);
      Continue;
    end;
    if (Op = opJmp) or OIsCondJump(Op) then
    begin
      Crossed := True;
      Dec(P);
      Inc(N);
      Continue;
    end;
    if (Op = opGetMethod) and ((OA(I) = R) or (OA(I) + 1 = R)) then Exit(Found(P));
    if OWrites(I) = R then Exit(Found(P));
    if OEmbedSets(I, Rd, NR, WF, WT) and (WF >= 0) and (R >= WF) and (R <= WT) then Exit;
    Dec(P);
    Inc(N);
  end;
end;

{ what register R holds at instruction K, as far as the frame now and the inline caches tell: the value of a
  move's source, of a cached own data property of such a value, of a cached global, else the register's value
  now (Depth bounds the walk). A speculation only: the code checks what it relies on }
function OSpecValue(Fr: PFrame; K, R, Depth: Integer): TValue;
var P, D: Integer; I: QWord; IC: PPropIC; O: PObj; V: TValue;
begin
  Result := VStack[Fr^.Base + R];
  if Depth > 4 then Exit;
  P := OWriterOf(K, R);
  if P < 0 then Exit;
  I := OIns(P);
  case OOpOf(I) of
    opMov: Result := OSpecValue(Fr, P, OB_(I), Depth + 1);
    opGetProp:
      begin
        IC := @OVTpl[P]^.ICs[OCU(I)];
        V := OSpecValue(Fr, P, OB_(I), Depth + 1);
        if not IsObject(V) then Exit;
        D := OEntryFor(IC, ObjOf(V)^.Shape);
        if D < 0 then Exit;
        Result := OHolderSlot(V, OICEntry(IC, D));
      end;
    opGetGlobal:
      begin
        IC := @OVTpl[P]^.ICs[OCU(I)];
        O := ObjOf(GlobalObj);
        if (IC^.Shape = nil) or (IC^.Shape <> O^.Shape) then Exit;
        Result := GetSlot(O, IC^.Slot);
      end;
    opGetUpv:
      begin
        { the frame's closure's upvalue now (open: its register's value); an inlined callee's closure as its call
          site speculates it }
        if OVFrame[P] >= 0 then
        begin
          if OSites[OVFrame[P]].VCall < 0 then Exit;
          V := OSpecValue(Fr, OSites[OVFrame[P]].VCall, OSites[OVFrame[P]].A, Depth + 1);
        end
        else V := Fr^.Closure;
        if not IsObject(V) or not IsPtr(ObjOf(V)^.Aux2) then Exit;
        V := PValueArray(PtrOf(ObjOf(V)^.Aux2))^.Data[OB_(I)];
        if not IsPtr(V) then Exit;
        if PUpval(PtrOf(V))^.StackIndex >= 0 then Result := VStack[PUpval(PtrOf(V))^.StackIndex]
        else Result := PUpval(PtrOf(V))^.Value;
      end;
    opGetMethod:
      begin
        { this: the receiver; the method: the cached data property along the receiver's prototypes }
        V := OSpecValue(Fr, P, OB_(I), Depth + 1);
        if R = OA(I) + 1 then begin Result := V; Exit; end;
        IC := @OVTpl[P]^.ICs[OCU(I)];
        if not IsObject(V) then Exit;
        D := OEntryFor(IC, ObjOf(V)^.Shape);
        if D < 0 then Exit;
        Result := OHolderSlot(V, OICEntry(IC, D));
      end;
  end;
end;

{ the native call instruction K calls as far as OSpecValue tells (nil none) }
function OCallNative(Fr: PFrame; K: Integer): Pointer;
var V: TValue;
begin
  Result := nil;
  V := OSpecValue(Fr, K, OA(OIns(K)), 0);
  if IsObject(V) and (ObjOf(V)^.OKind = okNative) then Result := ObjOf(V)^.Native;
end;

{ an instruction with the given fields }
function OMk(Op: TOp; A, B: Integer; C: Int64): QWord;
begin
  Result := QWord(Ord(Op)) or (QWord(A and $FFFF) shl 8) or (QWord(B and $FFFF) shl 24) or ((QWord(C) and $FFFFFF) shl 40);
end;

{ the instructions an inlined callee may have (each one the tier takes natively, or the call is not inlined; a
  call only inlined in turn or a Math native's) }
function OInlOk(Op: TOp): Boolean;
begin
  case Op of
    opNop, opMov, opLoadK, opLoadI, opLoadUndef, opLoadNull, opLoadTrue, opLoadFalse, opLoadHole,
    opAdd, opSub, opMul, opDiv, opMod, opBitAnd, opBitOr, opBitXor, opShl, opShr, opUShr,
    opAddI, opSubI, opIncI, opBitAndI, opBitOrI, opBitXorI, opShlI, opShrI, opUShrI,
    opNeg, opBitNot, opNot, opToNum, opToNumeric, opEq, opNe, opSEq, opSNe, opLt, opLe, opGt, opGe,
    opJmp, opJT, opJF, opJNullish, opJNotNullish, opJUndef, opJNotUndef, opJNLt, opJNLe, opJNGt, opJNGe,
    opJNSEq, opJSEq, opGetProp, opSetProp, opGetElem, opSetElem, opGetLength, opGetGlobal, opGetMethod, opRet,
    opRetUndef, opCall, opNew, opNewObj, opDefProp, opDefField, opGetUpv, opCheckHole, opTypeof, opThrow,
    opThrowConst: Result := True;
  else Result := False;
  end;
end;

{ callee instruction I inlined: its registers Off up (its frame's place), a jump's offset for the two virtual
  instructions each callee instruction takes }
function ORemap(I: QWord; Off: Integer): QWord;
var Op: TOp; A, B, CU, C: Integer;
begin
  Op := OOpOf(I); A := OA(I); B := OB_(I); CU := OCU(I); C := OC(I);
  case Op of
    opMov, opNeg, opBitNot, opNot, opToNum, opToNumeric, opAddI, opSubI, opIncI, opBitAndI, opBitOrI, opBitXorI,
    opShlI, opShrI, opUShrI, opGetProp, opGetLength, opSetProp, opGetMethod, opTypeof: Result := OMk(Op, A + Off, B + Off, CU);
    opThrow: Result := OMk(Op, A + Off, B, CU);
    opAdd, opSub, opMul, opDiv, opMod, opBitAnd, opBitOr, opBitXor, opShl, opShr, opUShr, opEq, opNe, opSEq, opSNe,
    opLt, opLe, opGt, opGe, opGetElem, opSetElem: Result := OMk(Op, A + Off, B + Off, CU + Off);
    opLoadK, opLoadI, opLoadUndef, opLoadNull, opLoadTrue, opLoadFalse, opLoadHole, opGetGlobal, opCall, opNew, opNewObj,
    opGetUpv:
      Result := OMk(Op, A + Off, B, CU);
    opDefProp, opDefField: Result := OMk(Op, A + Off, B + Off, CU);
    opJmp: Result := OMk(Op, A, B, 2 * C + 1);
    opJT, opJF, opJNullish, opJNotNullish, opJUndef, opJNotUndef: Result := OMk(Op, A + Off, B, 2 * C + 1);
    opCheckHole: Result := OMk(Op, A + Off, B, CU);
    opJNLt, opJNLe, opJNGt, opJNGe, opJNSEq, opJSEq: Result := OMk(Op, A + Off, B + Off, 2 * C + 1);
  else Result := I;
  end;
end;

{ cpuid leaf 1's ecx (bit 19: SSE4.1, roundsd) }
function OCpuidEcx1: Cardinal; assembler; nostackframe;
asm
  pushq %rbx
  movl $1, %eax
  cpuid
  movl %ecx, %eax
  popq %rbx
end;

{ the OptMathFns entry the call at virtual PC K calls, as its inline caches name it (-1 none) }
function OMathIndex(Fr: PFrame; K: Integer): Integer;
var Code: Pointer; R: Integer;
begin
  Result := -1;
  if (OOpOf(OIns(K)) <> opCall) or (OB_(OIns(K)) <> 1) or ((OFb(K) and ofbProp) <> 0) then Exit;
  Code := OCallNative(Fr, K);
  if Code = nil then Exit;
  for R := 0 to High(OptMathFns) do
    if (OptMathFns[R] <> nil) and (Code = OptMathFns[R]) and ((OSse41 = 1) or (R in [0, 3])) then Exit(R);
end;

{ whether template T can be inlined at all: a plain function small enough whose every instruction the tier
  takes natively }
function OInlEligible(T: PFuncTemplate; ForNew: Boolean): Boolean;
var Q: Integer;
begin
  Result := False;
  if (T^.CodeLen = 0) or (T^.CodeLen > OPT_INLINE_MAX) or not T^.FastCall or T^.IsGenerator or
     T^.IsAsync or T^.IsArrow or T^.IsDerived or not T^.NoCaptures or not T^.NoHandlers or
     (T^.ArgsReg >= 0) or (T^.RestReg >= 0) or T^.TopLevel then Exit;
  if (T^.FieldInit <> nil) and not ForNew then Exit;
  if ForNew and (T^.FieldInit <> nil) then
  begin
    { a field initializer inlined too: XPushFieldInit's cache of where the constructor holds it, no calls }
    if (T^.FIShape <> T^.CtorFnShape) or (T^.FISlot < 0) or (T^.FieldInit = T) or
       not OInlEligible(T^.FieldInit, False) then Exit;
    for Q := 0 to T^.FieldInit^.CodeLen - 1 do
      if InsOp(T^.FieldInit^.Code[Q]) in [opCall, opNew] then Exit;
  end;
  if ForNew then
  begin
    { a base constructor whose new object XFastNew's caches describe, returning that object }
    if (T^.IsMethod and not T^.IsClassCtor) or (T^.CtorRootShape = nil) or (T^.CtorFnShape = nil) or
       PShape(T^.CtorRootShape)^.IsDict or (Max(4, T^.ExpectedProps) > 16) then Exit;
    for Q := 0 to T^.CodeLen - 1 do
      if (InsOp(T^.Code[Q]) = opRet) and (InsA(T^.Code[Q]) <> 0) then Exit;
  end
  else if T^.IsClassCtor then Exit;
  for Q := 0 to T^.CodeLen - 1 do
    if not OInlOk(InsOp(T^.Code[Q])) then Exit;
  Result := True;
end;

{ the template the call at virtual PC K (made by inlined site Par's callee, -1 the template's own code) calls when
  it can be inlined there (nil none): the target OSpecValue names, a plain function small enough whose every
  instruction the tier takes natively, not one of the calls it is inlined in }
{ the template the call at virtual PC K (made by inlined site Par's callee, -1 the template's own code) runs when
  its callee is V and it can be inlined there (nil none) }
{ whether template T's code may read its this (register 0): the instructions an inlined callee has (OInlOk), each
  operand that is a register read checked; any other instruction counts as one that does }
function OReadsThis(T: PFuncTemplate): Boolean;
var K, J: Integer; I: QWord;
begin
  Result := True;
  for K := 0 to T^.CodeLen - 1 do
  begin
    I := T^.Code[K];
    case OOpOf(I) of
      opNop, opLoadK, opLoadI, opLoadUndef, opLoadNull, opLoadTrue, opLoadFalse, opLoadHole, opJmp, opGetGlobal,
      opGetUpv, opRetUndef, opThrowConst, opNewObj: ;
      opMov, opNeg, opBitNot, opNot, opToNum, opToNumeric, opAddI, opSubI, opIncI, opBitAndI, opBitOrI, opBitXorI,
      opShlI, opShrI, opUShrI, opGetProp, opGetLength, opGetMethod, opTypeof:
        if OB_(I) = 0 then Exit;
      opAdd, opSub, opMul, opDiv, opMod, opBitAnd, opBitOr, opBitXor, opShl, opShr, opUShr, opEq, opNe, opSEq, opSNe,
      opLt, opLe, opGt, opGe, opGetElem:
        if (OB_(I) = 0) or (OCU(I) = 0) then Exit;
      opJT, opJF, opJNullish, opJNotNullish, opJUndef, opJNotUndef, opCheckHole, opRet, opThrow:
        if OA(I) = 0 then Exit;
      opJNLt, opJNLe, opJNGt, opJNGe, opJNSEq, opJSEq, opSetProp, opDefProp, opDefField:
        if (OA(I) = 0) or (OB_(I) = 0) then Exit;
      opSetElem:
        if (OA(I) = 0) or (OB_(I) = 0) or (OCU(I) = 0) then Exit;
      opCall, opNew:
        for J := OA(I) to OA(I) + 1 + OB_(I) do if J = 0 then Exit;
    else Exit;
    end;
  end;
  Result := False;
end;

function OInlTargetOf(K, Par: Integer; V: TValue): PFuncTemplate;
var I: QWord; A, P: Integer; O: PObj; T: PFuncTemplate;
begin
  Result := nil;
  I := OIns(K);
  A := OA(I);
  if not IsObject(V) then Exit;
  O := ObjOf(V);
  { (a class constructor only through new) }
  if (O^.OKind <> okFunction) or (((O^.OFlags and 1) <> 0) and (OOpOf(I) <> opNew)) then Exit;
  T := PFuncTemplate(O^.Native);
  if (T = OT) or not OInlEligible(T, OOpOf(I) = opNew) or ((OOpOf(I) = opNew) and (OptAlloc = 0)) then Exit;
  if OOpOf(I) = opNew then
  begin
    { the constructor's prototype as its caches have it, in an inline slot }
    if (Pointer(O^.Shape) <> T^.CtorFnShape) or (T^.CtorProtoSlot < 0) or (T^.CtorProtoSlot >= O^.InlineCap) or
       (PShape(T^.CtorRootShape)^.Proto <> GetSlot(O, T^.CtorProtoSlot)) then Exit;
    { its field initializer's closure where XPushFieldInit's cache says, in an inline slot }
    if T^.FieldInit <> nil then
    begin
      if (T^.FISlot >= O^.InlineCap) and
         ((O^.Overflow = nil) or (T^.FISlot - O^.InlineCap >= Integer(O^.Overflow^.Len))) then Exit;
      V := GetSlot(O, T^.FISlot);
      if not IsObject(V) or (ObjOf(V)^.OKind <> okFunction) or (ObjOf(V)^.Native <> Pointer(T^.FieldInit)) then Exit;
    end;
  end;
  P := Par;
  while P >= 0 do
  begin
    if OSites[P].Tc = T then Exit;
    P := OSites[P].Parent;
  end;
  { a sloppy callee given no this would get the global object: only one that never reads its this }
  if (OOpOf(I) = opCall) and not T^.IsStrict and ((OCU(I) and 1) <> 0) and OReadsThis(T) then Exit;
  if A + 1 + T^.MaxRegs > 30000 then Exit;
  Result := T;
end;

function OInlTarget(Fr: PFrame; K, Par: Integer): PFuncTemplate;
begin
  Result := nil;
  if (OFb(K) and ofbProp) <> 0 then Exit;
  Result := OInlTargetOf(K, Par, OSpecValue(Fr, K, OA(OIns(K)), 0));
end;

{ the templates a method call at virtual PC K runs as its method's polymorphic inline cache tells, each from a
  prototype and inlinable (up to three, N 0 when the cache is not polymorphic or names none) }
procedure OInlTargets(K, Par: Integer; var Ts: array of PFuncTemplate; out N: Integer);
var W, E, D, J: Integer; I: QWord; IC: PPropIC; En: PICEntry; V: TValue; T: PFuncTemplate; Dup: Boolean;
begin
  N := 0;
  { (a failed guard here leaves its target in the cache: the dispatch takes it) }
  if OOpOf(OIns(K)) <> opCall then Exit;
  W := OWriterOf(K, OA(OIns(K)));
  if W < 0 then Exit;
  I := OIns(W);
  if not (OOpOf(I) in [opGetMethod, opGetProp]) or ((OOpOf(I) = opGetMethod) and (OA(I) <> OA(OIns(K)))) then Exit;
  IC := @OVTpl[W]^.ICs[OCU(I)];
  if (IC^.PolyCount < 2) or (IC^.PolyCount > 4) or (IC^.More = nil) then Exit;
  for E := 0 to IC^.PolyCount - 1 do
  begin
    En := OICEntry(IC, E);
    if (En^.Depth = 0) or not OEntryOk(En) then Continue;
    V := En^.Shape^.Proto;
    Dup := False;
    for D := 0 to En^.Depth - 1 do
    begin
      if not IsObject(V) or (ObjOf(V)^.Shape <> En^.Holder[D]) then begin Dup := True; Break; end;
      if D < En^.Depth - 1 then V := ObjOf(V)^.Shape^.Proto;
    end;
    if Dup then Continue;
    T := OInlTargetOf(K, Par, GetSlot(ObjOf(V), En^.Slot));
    if T = nil then Continue;
    for J := 0 to N - 1 do if Ts[J] = T then Dup := True;
    if Dup or (N > High(Ts)) then Continue;
    Ts[N] := T;
    Inc(N);
  end;
end;

{ whether instruction K uses an inline cache that has never been filled: it has not run (a cold path, an exit
  there a deopt only if it ever does) }
function OColdIC(K: Integer): Boolean;
var IC: PPropIC;
begin
  Result := False;
  case OOpOf(OIns(K)) of
    opGetProp, opGetMethod, opGetLength, opSetProp, opGetGlobal, opDefField:
      begin
        IC := @OVTpl[K]^.ICs[OCU(OIns(K))];
        Result := (IC^.Shape = nil) and (IC^.PolyCount = 0) and (IC^.Poly0.Shape = nil) and (IC^.NewShape = nil);
      end;
  end;
end;

{ the region's virtual instructions: the template's own, then each inlined call's (the call a jump to them): its
  guard, its callee's registers past its arguments undefined (as JitCallStub sets them), the callee's code (two
  virtual instructions each: a return a move into the call's register and a jump back after the call), then the
  calls that callee makes, inlined the same way (each one inlined, or a Math native's, or the outer call is
  not inlined) }
procedure OBuildStream(Fr: PFrame);
var K, Total: Integer; Sig: PtrUInt;
  procedure Add(Code: QWord; Tpl: PFuncTemplate; Pc: Integer; F: PByte; Role: Byte; Site, Frame: Integer);
  begin
    if OVLen >= Length(OVCode) then
    begin
      SetLength(OVCode, OVLen * 2 + 64); SetLength(OVTpl, OVLen * 2 + 64); SetLength(OVPC, OVLen * 2 + 64);
      SetLength(OVFb, OVLen * 2 + 64); SetLength(OVRole, OVLen * 2 + 64); SetLength(OVSite, OVLen * 2 + 64);
      SetLength(OVFrame, OVLen * 2 + 64);
    end;
    OVCode[OVLen] := Code; OVTpl[OVLen] := Tpl; OVPC[OVLen] := Pc; OVFb[OVLen] := F;
    OVRole[OVLen] := Role; OVSite[OVLen] := Site; OVFrame[OVLen] := Frame;
    Inc(OVLen);
  end;
  { the call at virtual PC VC (made by site Par's callee) inlined: True, or False with nothing added }
  function InlineAt(VC, Par: Integer): Boolean; forward;
  { the call at virtual PC VC (made by site Par's callee) inlined as a call of T: its site S, or False with nothing
    added }
  function InlineOne(VC, Par: Integer; T: PFuncTemplate; out S: Integer): Boolean;
  var R, S2, Off, Argc, Flag, Len0, Sites0, Total0, FbBase, FiFb, AFi, Ret, NFix, J: Integer; I: QWord;
    FI: PFuncTemplate; Sig0: PtrUInt; Fix: array of Integer;
    { site S's callee T's code at Off (two virtual instructions each): a return a move into register Off - 1 (a
      constructor's: its this) and a jump to Ret (-1: to a place patched later, listed in Fix), then the calls
      it makes inlined in turn (False: one that must be cannot) }
    function Body(S: Integer; T: PFuncTemplate; Off, Ret, FbBase: Integer): Boolean;
    var Q, Bb, V: Integer; I: QWord;
      procedure Back(Q, V: Integer);
      begin
        if Ret >= 0 then Add(OMk(opJmp, 0, 0, Ret - (V + 2)), T, Q, OTplFb(T) + Q, 2, S, S)
        else
        begin
          if NFix >= Length(Fix) then SetLength(Fix, NFix * 2 + 8);
          Fix[NFix] := OVLen;
          Inc(NFix);
          Add(OMk(opJmp, 0, 0, 0), T, Q, OTplFb(T) + Q, 2, S, S);
        end;
      end;
    begin
      Result := False;
      Bb := OVLen;
      for Q := 0 to T^.CodeLen - 1 do
      begin
        I := T^.Code[Q];
        V := Bb + 2 * Q;
        case OOpOf(I) of
          opRet:
            begin
              Add(OMk(opMov, Off - 1, OA(I) + Off, 0), T, Q, OTplFb(T) + Q, 2, S, S);
              Back(Q, V);
            end;
          opRetUndef:
            begin
              if OSites[S].IsNew then Add(OMk(opMov, Off - 1, Off, 0), T, Q, OTplFb(T) + Q, 2, S, S)
              else Add(OMk(opLoadUndef, Off - 1, 0, 0), T, Q, OTplFb(T) + Q, 2, S, S);
              Back(Q, V);
            end;
        else
          begin
            Add(ORemap(I, Off), T, Q, OTplFb(T) + Q, 2, S, S);
            Add(OMk(opNop, 0, 0, 0), T, Q, OTplFb(T) + Q, 2, S, S);
          end;
        end;
      end;
      if Off + T^.MaxRegs > ORegs then ORegs := Off + T^.MaxRegs;
      { the callee's calls: each inlined in turn, or a Math native's, or the baseline's call embedded (whose callee
        sees the inlined frames through the frame's inline descriptor, InlinedFrameCount) }
      for Q := 0 to T^.CodeLen - 1 do
        if (OOpOf(T^.Code[Q]) in [opCall, opNew]) and (OMathIndex(Fr, Bb + 2 * Q) < 0) then InlineAt(Bb + 2 * Q, S);
      Result := True;
    end;
    procedure NewSite(out S: Integer);
    begin
      if OSiteCount >= Length(OSites) then SetLength(OSites, OSiteCount * 2 + 8);
      S := OSiteCount;
      Inc(OSiteCount);
      OSites[S].D := nil; OSites[S].IsNew := False; OSites[S].IsFI := False; OSites[S].FiSite := -1;
    end;
    procedure Undo;
    begin
      OVLen := Len0; OSiteCount := Sites0; Total := Total0; Sig := Sig0;
    end;
  begin
    Result := False;
    NFix := 0;
    S := -1;
    if Total + T^.CodeLen + 1 > OPT_INLINE_TOTAL then Exit;
    Len0 := OVLen; Sites0 := OSiteCount; Total0 := Total; Sig0 := Sig;
    FbBase := Total;
    Inc(Total, T^.CodeLen + 1);
    Sig := Sig * 31 + PtrUInt(T) + PtrUInt(FbBase);
    I := OIns(VC);
    Argc := OB_(I);
    if OOpOf(I) = opNew then Flag := 0 else Flag := OCU(I) and 1;
    Off := OA(I) + 1;
    if Par < 0 then Ret := VC + 1 else Ret := VC + 2;
    NewSite(S);
    OSites[S].VCall := VC; OSites[S].A := OA(I); OSites[S].Argc := Argc; OSites[S].Flag := Flag;
    OSites[S].Tc := T; OSites[S].Parent := Par; OSites[S].Q := OVPC[VC]; OSites[S].Ret := Ret;
    OSites[S].IsNew := OOpOf(I) = opNew;
    OSites[S].Room := 1 + T^.MaxRegs;
    if Par < 0 then begin OSites[S].K := VC; OSites[S].Depth := 0; end
    else begin OSites[S].K := OSites[Par].K; OSites[S].Depth := OSites[Par].Depth + 1; end;
    OSites[S].Guard := OVLen;
    { the guard: the call as its caller makes it (a failed guard resumes there) }
    Add(I, OVTpl[VC], OVPC[VC], OVFb[VC], 1, S, OVFrame[VC]);
    if Flag = 1 then Add(OMk(opLoadUndef, Off, 0, 0), T, 0, OTplFb(T), 2, S, S);
    for R := 1 + Min(Argc, T^.NumParams) to T^.MaxRegs - 1 do
      Add(OMk(opLoadUndef, Off + R, 0, 0), T, 0, OTplFb(T), 2, S, S);
    if OSites[S].IsNew and (T^.FieldInit <> nil) then
    begin
      { (the constructor object's inline capacity: where its field initializer's slot is) }
      OSites[S].FnCap := ObjOf(OSpecValue(Fr, VC, OA(OIns(VC)), 0))^.InlineCap;
      { the class's field initializer first, on this (as XPushFieldInit runs it: a frame of its own above the
        constructor's registers and arguments, the constructor at PC 0 after it); its closure put in register
        AFi by the constructor's guard }
      FI := T^.FieldInit;
      if (OSites[S].Depth + 1 >= OPT_INLINE_DEPTH) or (Total + FI^.CodeLen + 1 > OPT_INLINE_TOTAL) then
      begin
        Undo;
        Exit;
      end;
      FiFb := Total;
      Inc(Total, FI^.CodeLen + 1);
      Sig := Sig * 31 + PtrUInt(FI) + PtrUInt(FiFb);
      AFi := Off + Max(T^.MaxRegs, Argc + 1);
      OSites[S].Room := (AFi - OSites[S].A) + 1 + FI^.MaxRegs;
      NewSite(S2);
      OSites[S2].VCall := -1; OSites[S2].A := AFi; OSites[S2].Argc := 0; OSites[S2].Flag := 0;
      OSites[S2].Tc := FI; OSites[S2].Parent := S; OSites[S2].Q := -1; OSites[S2].Ret := -1;
      OSites[S2].IsFI := True; OSites[S2].Room := 1 + FI^.MaxRegs;
      OSites[S2].K := OSites[S].K; OSites[S2].Depth := OSites[S].Depth + 1;
      OSites[S2].Guard := OVLen;
      OSites[S].FiSite := S2;
      Add(OMk(opNop, 0, 0, 0), T, 0, OTplFb(T), 1, S2, S);
      Add(OMk(opMov, AFi + 1, Off, 0), FI, 0, OTplFb(FI), 2, S2, S2);
      for R := 1 to FI^.MaxRegs - 1 do Add(OMk(opLoadUndef, AFi + 1 + R, 0, 0), FI, 0, OTplFb(FI), 2, S2, S2);
      if not Body(S2, FI, AFi + 1, -1, FiFb) then begin Undo; Exit; end;
      { its returns: to the constructor's code }
      for J := 0 to NFix - 1 do OVCode[Fix[J]] := OMk(opJmp, 0, 0, OVLen - (Fix[J] + 1));
    end;
    if not Body(S, T, Off, Ret, FbBase) then begin Undo; Exit; end;
    Result := True;
  end;
  { the call at virtual PC VC (made by site Par's callee) inlined: True, or False with nothing added. A polymorphic
    method call: each of its targets' code after a dispatch on the callee's template (the last one's guard the
    deopt) }
  function InlineAt(VC, Par: Integer): Boolean;
  var Ts: array[0..2] of PFuncTemplate; N, J, S, First, D, Len0, Sites0, Total0: Integer; T: PFuncTemplate;
    Sig0: PtrUInt;
  begin
    Result := False;
    if (Par >= 0) and (OSites[Par].Depth + 1 >= OPT_INLINE_DEPTH) then Exit;
    OInlTargets(VC, Par, Ts, N);
    First := OVLen;
    if N = 0 then
    begin
      T := OInlTarget(Fr, VC, Par);
      if (T = nil) or not InlineOne(VC, Par, T, S) then Exit;
    end
    else
    begin
      { each target's code after a test of the callee's template, then any other callee: the baseline's call
        (a copy of the call, role 5, embedded) }
      Len0 := OVLen; Sites0 := OSiteCount; Total0 := Total; Sig0 := Sig;
      S := -1;
      for J := 0 to N - 1 do
      begin
        D := OVLen;
        Add(OMk(opJNotUndef, OA(OIns(VC)), 0, 0), Ts[J], OVPC[VC], OVFb[VC], 4, -1, OVFrame[VC]);
        if not InlineOne(VC, Par, Ts[J], S) then
        begin
          OVLen := Len0; OSiteCount := Sites0; Total := Total0; Sig := Sig0;
          Exit;
        end;
        { not this target: the next one }
        OVCode[D] := OMk(opJNotUndef, OA(OIns(VC)), 0, OVLen - (D + 1));
      end;
      Add(OIns(VC), OVTpl[VC], OVPC[VC], OVFb[VC], 5, -1, OVFrame[VC]);
      if Par < 0 then Add(OMk(opJmp, 0, 0, (VC + 1) - (OVLen + 1)), OVTpl[VC], OVPC[VC], OVFb[VC], 5, -1, OVFrame[VC])
      else Add(OMk(opJmp, 0, 0, (VC + 2) - (OVLen + 1)), OVTpl[VC], OVPC[VC], OVFb[VC], 5, -1, OVFrame[VC]);
    end;
    { the call: a jump to the guard (or the dispatch) }
    OVCode[VC] := OMk(opJmp, 0, 0, First - (VC + 1));
    OVRole[VC] := 3;
    OVSite[VC] := S;
    Result := True;
  end;
begin
  if OptInline < 0 then
    if GetEnvironmentVariable('G2_OPTINLINE') = '0' then OptInline := 0 else OptInline := 1;
  if OptAlloc < 0 then
    if GetEnvironmentVariable('G2_OPTALLOC') = '0' then OptAlloc := 0 else OptAlloc := 1;
  OVLen := 0;
  OSiteCount := 0;
  for K := 0 to OT^.CodeLen do
    if K < OT^.CodeLen then Add(OT^.Code[K], OT, K, @OInfo^.Feedback[K], 0, -1, -1)
    else Add(0, OT, K, @OInfo^.Feedback[K], 0, -1, -1);
  if OptInline = 1 then
    for K := 0 to OT^.CodeLen - 1 do
    begin
      if not (OOpOf(OT^.Code[K]) in [opCall, opNew]) or OInlBan[K] then Continue;
      { (the callees' feedback is each callee template's own, OTplFb) }
      Total := 0;
      Sig := 0;
      if not InlineAt(K, -1) then Continue;
    end;
  { the end }
  Add(0, OT, OT^.CodeLen, @OInfo^.Feedback[OT^.CodeLen], 0, -1, -1);
  Dec(OVLen);
end;

{ site S's deopt descriptor (made once per compilation, kept with the code) }
function OInlDesc(S: Integer): POInlDesc;
var Ch: array[0..OPT_INLINE_DEPTH - 1] of Integer; N, P, J: Integer;
begin
  if OSites[S].D <> nil then Exit(POInlDesc(OSites[S].D));
  N := 0;
  P := S;
  while P >= 0 do
  begin
    Ch[N] := P;
    Inc(N);
    P := OSites[P].Parent;
  end;
  Result := AllocMem(SizeOf(TOInlDesc));
  Result^.N := N;
  Result^.NextPC := OSites[S].K + 1;
  for J := 0 to N - 1 do
  begin
    P := Ch[N - 1 - J];
    Result^.Lv[J].A := OSites[P].A;
    Result^.Lv[J].Argc := OSites[P].Argc;
    Result^.Lv[J].T := OSites[P].Tc;
    Result^.Lv[J].Kind := Ord(OSites[P].IsNew);
    if J < N - 1 then Result^.Lv[J].PC := OSites[Ch[N - 2 - J]].Q + 1 else Result^.Lv[J].PC := 0;
  end;
  OSites[S].D := Result;
end;

{ an inlined callee's guard failed or exit taken at its instruction Q (the frame state already in VStack): its
  frames, and those of the inlined callees that called it, made as JitCallStub makes them (each one's result
  written to its caller's register on return) and run by the interpreter to the outermost one's return (as
  XJitCallPost), the result in the call's register; 0 goes on (in the caller after the call), -1 a throw }
function XOptInlDeopt(Fr: PFrame; D: POInlDesc; Q: Integer): Integer;
var NF: PFrame; Base, J, Stop: Integer; V: TValue;
begin
  Fr^.PC := D^.NextPC;
  Stop := FrameTop;
  Base := 0;
  for J := 0 to D^.N - 1 do
  begin
    Base := Fr^.Base + D^.Lv[J].A + 1;
    NF := @Frames[FrameTop];
    NF^.Closure := VStack[Base - 1];
    { a constructor's frame (Kind 1): its new.target the constructor }
    if D^.Lv[J].Kind = 1 then NF^.NewTarget := NF^.Closure else NF^.NewTarget := VUndefined;
    NF^.Tpl := D^.Lv[J].T;
    NF^.Base := Base;
    if J = D^.N - 1 then NF^.PC := Q else NF^.PC := D^.Lv[J].PC;
    NF^.RetIdx := Base - 1;
    NF^.Argc := D^.Lv[J].Argc;
    NF^.Kind := D^.Lv[J].Kind;
    Inc(FrameTop);
  end;
  if StackTop < Base + D^.Lv[D^.N - 1].T^.MaxRegs then StackTop := Base + D^.Lv[D^.N - 1].T^.MaxRegs;   { (never below the caller's registers) }
  if not Execute(Stop, V) then
  begin
    StackTop := Fr^.Base + Fr^.Tpl^.MaxRegs;
    Exit(-1);
  end;
  VStack[Fr^.Base + D^.Lv[0].A] := V;
  StackTop := Fr^.Base + Fr^.Tpl^.MaxRegs;
  Result := 0;
end;

{ the log's list of the region's embedded opcodes (G2_OPTLOG) }
function OEmbedList: string;
var K: Integer; S: string;
begin
  Result := '';
  if OEmbed = nil then Exit;
  for K := 0 to High(OEmbed) do
    if OEmbed[K] and (K < OVLen) then
    begin
      WriteStr(S, OOpOf(OIns(K)));
      Result := Result + ' ' + S + '@' + IntToStr(K);
    end;
end;

{ (G2_OPTLOG) the inlined call sites: each callee's name and its depth }
function OSiteList: string;
var S: Integer;
begin
  Result := '';
  for S := 0 to OSiteCount - 1 do
    Result := Result + ' ' + OptName(OSites[S].Tc) + '/' + IntToStr(OSites[S].Depth);
end;

{ (G2_OPTLOG) the region's exits: each exit block's instruction and virtual PC }
function OExitList: string;
var B: Integer; S: string;
begin
  Result := '';
  for B := 0 to OBCount - 1 do
    if (OB[B].Term = otExit) and (OB[B].TermPC >= 0) and (OB[B].TermPC < OVLen) then
    begin
      WriteStr(S, OOpOf(OIns(OB[B].TermPC)));
      Result := Result + ' ' + S + '@' + IntToStr(OB[B].TermPC);
    end;
end;

{ NewObj at virtual PC K allocated inline (OAllocCap), the DefProps of its literal after it (straight-line code of
  its frame, on its register) native (ODefSlot, ODefShape, ODefNew) }
procedure OAllocSpec(K: Integer);
var Cap, A, P, CI: Integer; I: QWord; Sh, NS: PShape; Key: PStr;
begin
  Cap := Max(4, OCU(OIns(K)));
  if (Cap > 16) or (64 + Cap * 8 > (NurseryLimit - NurseryStart) div 16) then Exit;
  Sh := RootShapeFor(ObjectProto);
  if (Sh = nil) or Sh^.IsDict then Exit;
  OAllocCap[K] := Cap;
  ODefShape[K] := Sh;
  A := OA(OIns(K));
  P := K + 1;
  while (P < OVLen) and (P - K < 96) do
  begin
    I := OIns(P);
    if (OVFrame[P] <> OVFrame[K]) or (OVRole[P] in [1, 3]) then Exit;
    if (OOpOf(I) = opJmp) or OIsCondJump(OOpOf(I)) then Exit;
    if (OOpOf(I) = opDefProp) and (OA(I) = A) then
    begin
      if not IsString(OVTpl[P]^.Consts[OCU(I)]) then Exit;
      Key := PStr(PtrOf(OVTpl[P]^.Consts[OCU(I)]));
      CI := ShapeLookup(Sh, Key);
      ODefShape[P] := Sh;
      if CI >= 0 then
      begin
        if (Sh^.PFlags[CI] <> pfDefault) or (CI >= Cap) then Exit;
        ODefSlot[P] := CI;
      end
      else
      begin
        NS := ShapeAdd(Sh, Key, pfDefault);
        if (NS = nil) or NS^.IsDict or (NS^.Count - 1 >= Cap) or (NS^.Count - 1 <> Sh^.Count) then Exit;
        ODefSlot[P] := NS^.Count - 1;
        ODefNew[P] := NS;
        Sh := NS;
      end;
    end
    else if (OWrites(I) = A) or ((OOpOf(I) = opGetMethod) and (OA(I) + 1 = A)) then Exit;
    Inc(P);
  end;
end;

{ the region entered at PC of frame Fr's template, compiled and installed in its JitTable; False when it cannot be }
var
  ODblShapes: array of PShape;   { open addressing on the shape pointer (shapes live as long as the heap) }
  ODblShapeCount: Integer;

function ODblShapeSlot(Sh: PShape; Mask: Integer): Integer; inline;
begin
  Result := Integer((PtrUInt(Sh) shr 4) * 2654435761) and Mask;
end;

function ODblShapeHas(Sh: PShape): Boolean;
var J: Integer;
begin
  Result := False;
  if (Sh = nil) or (ODblShapes = nil) then Exit;
  J := ODblShapeSlot(Sh, High(ODblShapes));
  while ODblShapes[J] <> nil do
  begin
    if ODblShapes[J] = Sh then Exit(True);
    J := (J + 1) and High(ODblShapes);
  end;
end;

procedure ODblShapeAdd(Sh: PShape);
var Old: array of PShape; J, K: Integer;
begin
  if (Sh = nil) or ODblShapeHas(Sh) then Exit;
  if (ODblShapeCount + 1) * 2 > Length(ODblShapes) then
  begin
    Old := ODblShapes;
    ODblShapes := nil;
    SetLength(ODblShapes, Max(64, Length(Old) * 2));
    for K := 0 to High(Old) do
      if Old[K] <> nil then
      begin
        J := ODblShapeSlot(Old[K], High(ODblShapes));
        while ODblShapes[J] <> nil do J := (J + 1) and High(ODblShapes);
        ODblShapes[J] := Old[K];
      end;
  end;
  J := ODblShapeSlot(Sh, High(ODblShapes));
  while ODblShapes[J] <> nil do J := (J + 1) and High(ODblShapes);
  ODblShapes[J] := Sh;
  Inc(ODblShapeCount);
end;

{ whether register R at K was just read from a property of a shape whose number properties have held doubles }
function ODblRead(K, R: Integer): Boolean;
var P, E: Integer; IC: PPropIC;
begin
  Result := False;
  if ODblShapeCount = 0 then Exit;
  P := OWriterOf(K, R);
  if (P < 0) or not (OOpOf(OIns(P)) in [opGetProp, opGetMethod]) then Exit;
  IC := @OVTpl[P]^.ICs[OCU(OIns(P))];
  for E := 0 to IC^.PolyCount - 1 do
    if ODblShapeHas(OICEntry(IC, E)^.Shape) then Exit(True);
end;

{ marks the shapes of property read P's cache (nothing when P is not one) }
procedure ODblShapesOf(P: Integer);
var IC: PPropIC; R: Integer;
begin
  if (P < 0) or not (OOpOf(OIns(P)) in [opGetProp, opGetMethod]) then Exit;
  IC := @OVTpl[P]^.ICs[OCU(OIns(P))];
  for R := 0 to IC^.PolyCount - 1 do ODblShapeAdd(OICEntry(IC, R)^.Shape);
end;

function OptCompile(Fr: PFrame; PC: Integer): Boolean;
var Kinds: array of Byte; R, R1, R2, Iter, Attempt: Integer; Grew: Boolean; V: TValue; Code: Pointer; Tbl: PPtrUInt; K: Integer; IC: PPropIC; Sh: PShape;
  Again: Boolean;
begin
  Result := False;
  OT := Fr^.Tpl;
  OInfo := POptInfo(OT^.Opt);
  OEntry := PC;
  OSpecFr := Fr;
  { the first estimate, before anything is built (most regions fail it) }
  if IsObject(Fr^.Closure) then OQClo := ObjOf(Fr^.Closure) else OQClo := nil;
  if not OQuickProfitable(PC) then
  begin
    OQClo := nil;
    if OptLog then WriteLn(ErrOutput, 'opt ', OptName(OT), ' pc ', PC, ' not profitable (quick)');
    Exit;
  end;
  OQClo := nil;
  SetLength(OInlBan, OT^.CodeLen + 1);
  for K := 0 to OT^.CodeLen do OInlBan[K] := False;
  Attempt := 0;
  repeat
  Again := False;
  Inc(Attempt);
  if Attempt > 6 then
  begin
    if OptLog then WriteLn(ErrOutput, 'opt ', OptName(OT), ' pc ', PC, ' given up (inlining bans)');
    Exit;
  end;
  ORegs := OT^.MaxRegs;
  OFail := False;
  OInlBanned := False;
  ONCount := 0; OFSCount := 0; OPhiArgCount := 0;
  if OSse41 < 0 then
    if (OCpuidEcx1 and (1 shl 19)) <> 0 then OSse41 := 1 else OSse41 := 0;
  OBuildStream(Fr);
  SetLength(OSpecMemo, (OVLen + 1) * 2);
  for K := 0 to High(OSpecMemo) do OSpecMemo[K].R := -1;
  SetLength(OExit, OVLen + 1);
  SetLength(OEmbed, OVLen + 1);
  SetLength(OEmbedNode, OVLen + 1);
  for K := 0 to OVLen do begin OExit[K] := False; OEmbed[K] := False; OEmbedNode[K] := -1; end;
  { each call whose inline caches name a Math native (OCallNative), with one argument: computed inline }
  SetLength(OMathSpec, OVLen + 1);
  for K := 0 to OVLen do
  begin
    OMathSpec[K] := -1;
    if (K < OVLen) and (OVRole[K] in [0, 2]) then OMathSpec[K] := OMathIndex(Fr, K);
  end;
  { each NewObj's object allocated inline (a plain object of a small inline capacity), and the properties of its
    literal that follow it defined natively: their shapes as G2.Core's AddProperty makes them }
  SetLength(OAllocCap, OVLen + 1); SetLength(ODefSlot, OVLen + 1);
  SetLength(ODefShape, OVLen + 1); SetLength(ODefNew, OVLen + 1);
  for K := 0 to OVLen do begin OAllocCap[K] := -1; ODefSlot[K] := -1; ODefShape[K] := nil; ODefNew[K] := nil; end;
  if not GCStress and not MultiRealmAny and (OptAlloc <> 0) then
    for K := 0 to OVLen - 1 do
      if (OOpOf(OIns(K)) = opNewObj) and (OVRole[K] in [0, 2]) then OAllocSpec(K);
  for K := 0 to OVLen - 1 do
    if not (OVRole[K] in [1, 4]) and not OStaticOk(OOpOf(OIns(K))) and (OMathSpec[K] < 0) and
       not ((OOpOf(OIns(K)) = opNewObj) and (OAllocCap[K] > 0)) and
       not ((OOpOf(OIns(K)) = opDefProp) and (ODefSlot[K] >= 0)) then
    begin
      if OEmbeddable(K) then OEmbed[K] := True else OExit[K] := True;
    end;
  OExit[OT^.CodeLen] := True;
  OExit[OVLen] := True;
  { a generator's own suspensions and completion are the baseline's (the interpreter's) }
  if OT^.IsGenerator then
    for K := 0 to OVLen - 1 do
      if (OVRole[K] = 0) and (OVTpl[K] = OT) and
         (OOpOf(OIns(K)) in [opYield, opYieldStar, opAwait, opRet, opRetUndef]) then
      begin
        OExit[K] := True;
        OEmbed[K] := False;
      end;
  { the registers nested functions capture }
  SetLength(OCaptured, ORegs);
  for R := 0 to ORegs - 1 do OCaptured[R] := False;
  OCapList := nil;
  for K := 0 to High(OT^.Children) do
    for R := 0 to High(OT^.Children[K]^.Upvals) do
      if OT^.Children[K]^.Upvals[R].FromParentLocal and (OT^.Children[K]^.Upvals[R].Index < ORegs) and
         not OCaptured[OT^.Children[K]^.Upvals[R].Index] then
      begin
        OCaptured[OT^.Children[K]^.Upvals[R].Index] := True;
        SetLength(OCapList, Length(OCapList) + 1);
        OCapList[High(OCapList)] := OT^.Children[K]^.Upvals[R].Index;
      end;
  { the entry's speculation: the frame's values now (a failed entry guard: ints as doubles) }
  SetLength(Kinds, ORegs);
  for R := 0 to ORegs - 1 do
  begin
    V := VStack[Fr^.Base + R];
    if IsInt(V) then
    begin
      if (OFb(PC) and 2) <> 0 then Kinds[R] := vkDbl else Kinds[R] := vkInt;
    end
    else if V < DOUBLE_LIMIT then Kinds[R] := vkDbl
    else if (V = VTrue) or (V = VFalse) then Kinds[R] := vkBool
    else Kinds[R] := vkTag;
    if OCaptured[R] then Kinds[R] := vkTag;
  end;
  { only what the template reads from the entry on is speculated: the rest (an inlined callee's registers past the
    frame, a temporary written before it is read) holds whatever it held }
  OLiveW := (ORegs + 63) shr 6;
  if OLiveW = 0 then OLiveW := 1;
  OComputeTplLive;
  for R := 0 to ORegs - 1 do
    if (R >= OT^.MaxRegs) or ((OTplLive[PC * OLiveW + (R shr 6)] and (QWord(1) shl (R and 63))) = 0) then
      Kinds[R] := vkTag;
  { a register the template's code from PC on never reads as a number (only passes on, stores, or reads properties
    of) is not speculated: its guard would buy nothing, and a value whose kind differs between calls (babel's
    NodePath.get key, an index or a name) failed it on every other entry }
  SetLength(ONumUse, OT^.MaxRegs);
  for R := 0 to OT^.MaxRegs - 1 do ONumUse[R] := False;
  for K := PC to OT^.CodeLen - 1 do
  begin
    case OOpOf(OT^.Code[K]) of
      opGetProp, opGetLength, opGetMethod, opMov: Continue;
      opSetElem: begin R := OB_(OT^.Code[K]); if R < OT^.MaxRegs then ONumUse[R] := True; Continue; end;
    end;
    OReads(OT^.Code[K], R1, R2);
    if OOpOf(OT^.Code[K]) = opCall then R1 := -1;
    if (R1 >= 0) and (R1 < OT^.MaxRegs) then ONumUse[R1] := True;
    if (R2 >= 0) and (R2 < OT^.MaxRegs) then ONumUse[R2] := True;
  end;
  { a move's source is read as a number where its destination is }
  repeat
    Grew := False;
    for K := PC to OT^.CodeLen - 1 do
      if OOpOf(OT^.Code[K]) = opMov then
      begin
        R1 := OA(OT^.Code[K]); R2 := OB_(OT^.Code[K]);
        if (R1 < OT^.MaxRegs) and (R2 < OT^.MaxRegs) and ONumUse[R1] and not ONumUse[R2] then
        begin
          ONumUse[R2] := True;
          Grew := True;
        end;
      end;
  until not Grew;
  for R := 0 to OT^.MaxRegs - 1 do
    if (R < ORegs) and not ONumUse[R] then Kinds[R] := vkTag;
  if (OFb(PC) and 4) <> 0 then
  begin
    if OptLog then WriteLn(ErrOutput, 'opt ', OptName(OT), ' pc ', PC, ' given up (feedback)');
    Exit;
  end;
  { each element instruction's receiver speculated from the frame's value now: a typed array of a kind done
    natively, an object with an elements store, else none (an element guard that failed: none) }
  SetLength(OElemSpec, OVLen + 1);
  for K := 0 to OVLen do
  begin
    OElemSpec[K] := -1;
    if K = OVLen then Continue;
    case OOpOf(OIns(K)) of
      opGetElem: R := OB_(OIns(K));
      opSetElem: R := OA(OIns(K));
    else Continue;
    end;
    if (OFb(K) and ofbElem) <> 0 then Continue;
    V := OSpecValue(Fr, K, R, 0);
    if not IsObject(V) then Continue;
    if PObj(PtrOf(V))^.OKind = okTypedArray then
    begin
      if TAKind(PObj(PtrOf(V))) > taFloat64 then Continue;
      if (OOpOf(OIns(K)) = opSetElem) and (TAKind(PObj(PtrOf(V))) = taUint8C) then Continue;
      OElemSpec[K] := TAKind(PObj(PtrOf(V)));
    end
    else if PObj(PtrOf(V))^.Elements <> nil then OElemSpec[K] := ELEM_ARRAY;
  end;
  { each property instruction's receiver speculated from its IC: one own data property of one shape (not a
    dictionary's), its slot and shape constants in the code; a length from the receiver's value now }
  SetLength(OPropSlot, OVLen + 1);
  SetLength(OPropShape, OVLen + 1);
  SetLength(OPropKind, OVLen + 1);
  SetLength(OPropVK, OVLen + 1);
  SetLength(OPropIC, OVLen + 1);
  SetLength(OPropNew, OVLen + 1);
  { the receiver shapes whose number properties have held doubles: a property read whose unbox as an int failed
    marks the shapes of its cache (ODblShapes), and every number property of those shapes is read as a double }
  for K := 0 to OVLen - 1 do
    if (OFb(K) and ofbUnboxDbl) <> 0 then
    begin
      if OOpOf(OIns(K)) in [opGetProp, opGetMethod] then ODblShapesOf(K)
      else if ONumConsumer(OOpOf(OIns(K))) then
      begin
        { an arithmetic unbox that failed on a property just read: that read's shapes }
        if OOpOf(OIns(K)) in [opJNLt, opJNLe, opJNGt, opJNGe] then
        begin
          ODblShapesOf(OWriterOf(K, OA(OIns(K))));
          ODblShapesOf(OWriterOf(K, OB_(OIns(K))));
        end
        else
        begin
          ODblShapesOf(OWriterOf(K, OB_(OIns(K))));
          if not (OOpOf(OIns(K)) in [opAddI, opSubI, opIncI, opBitAndI, opBitOrI, opBitXorI, opShlI, opShrI, opUShrI,
          opNeg, opBitNot, opToNum, opToNumeric]) then ODblShapesOf(OWriterOf(K, OCU(OIns(K))));
        end;
      end;
    end;
  for K := 0 to OVLen do
  begin
    OPropSlot[K] := -1; OPropShape[K] := nil; OPropKind[K] := 0; OPropVK[K] := vkTag; OPropIC[K] := nil;
    OPropNew[K] := nil;
    if K = OVLen then Continue;
    { a guard that failed: not again, unless the cache has seen more shapes since (tried in turn). A full cache
      (four shapes) is a site with more shapes than it holds: its guards would fail again on each recompile }
    if ((OFb(K) and ofbProp) <> 0) and
       (not (OOpOf(OIns(K)) in [opGetProp, opGetMethod]) or (OVTpl[K]^.ICs[OCU(OIns(K))].PolyCount < 2) or
        (OVTpl[K]^.ICs[OCU(OIns(K))].PolyCount >= 4)) then Continue;
    case OOpOf(OIns(K)) of
      opGetProp, opGetMethod:
        begin
          IC := @OVTpl[K]^.ICs[OCU(OIns(K))];
          { one shape (its slot and shape constants in the code), or two to four (tried in turn) }
          if OICOk(IC) then
          begin
            OPropIC[K] := IC;
            if IC^.PolyCount = 1 then
            begin
              OPropSlot[K] := IC^.Poly0.Slot;
              OPropShape[K] := IC^.Poly0.Shape;
              OPropKind[K] := IC^.Poly0.Kind;
            end
            else OPropSlot[K] := 0;
            { a number now (the receiver as OSpecValue tells it): unboxed, as OSpec allows }
            V := OSpecValue(Fr, K, OB_(OIns(K)), 0);
            if IsObject(V) then
            begin
              R := OEntryFor(IC, ObjOf(V)^.Shape);
              if R >= 0 then
              begin
                Sh := OICEntry(IC, R)^.Shape;
                V := OHolderSlot(V, OICEntry(IC, R));
                if IsInt(V) and (OSpec(K) <> vkNone) then
                begin
                  if ODblShapeHas(Sh) then OPropVK[K] := vkDbl else OPropVK[K] := OSpec(K);
                end
                else if (V < DOUBLE_LIMIT) and (OSpec(K) <> vkNone) then OPropVK[K] := vkDbl;
              end;
            end;
          end;
        end;
      opNewObj: OPropShape[K] := ODefShape[K];
      opNew:
        if (OVRole[K] = 1) and OSites[OVSite[K]].IsNew then
        begin
          OPropShape[K] := PShape(OSites[OVSite[K]].Tc^.CtorRootShape);
          OAllocCap[K] := Max(4, OSites[OVSite[K]].Tc^.ExpectedProps);
        end;
      opDefProp:
        if ODefSlot[K] >= 0 then
        begin
          OPropSlot[K] := ODefSlot[K];
          OPropShape[K] := ODefShape[K];
          OPropNew[K] := ODefNew[K];
        end;
      opDefField:
        begin
          { a class field's cached add transition (as the baseline's) }
          IC := @OVTpl[K]^.ICs[OCU(OIns(K))];
          if (IC^.Shape <> nil) and not IC^.Shape^.IsDict and (IC^.NewShape <> nil) and not IC^.NewShape^.IsDict and
             (IC^.Slot >= 0) and (IC^.NewShape^.Count - 1 = IC^.Slot) then
          begin
            OPropSlot[K] := IC^.Slot;
            OPropShape[K] := IC^.Shape;
            OPropNew[K] := IC^.NewShape;
            OPropIC[K] := IC;
          end;
        end;
      opSetProp:
        begin
          { an own store, or an add transition (checked still cached: a prototype change forgets it) }
          IC := @OVTpl[K]^.ICs[OCU(OIns(K))];
          if (IC^.Shape <> nil) and not IC^.Shape^.IsDict and (IC^.Slot >= 0) and
             ((IC^.NewShape = nil) or (not IC^.NewShape^.IsDict and (IC^.NewShape^.Count - 1 = IC^.Slot))) then
          begin
            OPropSlot[K] := IC^.Slot;
            OPropShape[K] := IC^.Shape;
            OPropNew[K] := IC^.NewShape;
            OPropIC[K] := IC;
          end;
        end;
      opGetGlobal:
        begin
          { the global object's own data property (as JH_GetGlobal's cache) }
          IC := @OVTpl[K]^.ICs[OCU(OIns(K))];
          if (IC^.Shape <> nil) and (IC^.Slot >= 0) then
          begin
            OPropSlot[K] := IC^.Slot;
            OPropShape[K] := IC^.Shape;
          end;
        end;
      opGetLength:
        begin
          V := OSpecValue(Fr, K, OB_(OIns(K)), 0);
          if IsString(V) then OPropSlot[K] := LEN_STR
          else if IsObject(V) then
          begin
            if (PObj(PtrOf(V))^.OKind = okArray) and ((PObj(PtrOf(V))^.OFlags and ofBigLen) = 0) and
               (PObj(PtrOf(V))^.Elements <> nil) then OPropSlot[K] := LEN_ARRAY
          end;
        end;
    end;
  end;
  { PC 0 or a loop's header (a backward jump's target) }
  if PC <> 0 then
  begin
    R := -1;
    for K := PC to OT^.CodeLen - 1 do
      if (OOpOf(OT^.Code[K]) = opJmp) and (K + 1 + OC(OT^.Code[K]) = PC) then begin R := K; Break; end;
    if R < 0 then Exit;
  end;
  Iter := 0;
  repeat
    Inc(Iter);
    if Iter > 12 then
    begin
      if OptLog then WriteLn(ErrOutput, 'opt ', OptName(OT), ' pc ', PC, ' given up (kinds do not settle)');
      Exit;
    end;
    OB := nil;
    if not OBuildBlocks then Exit;
    if OInferKinds(Kinds) then Break;
    if OInlBanned then begin Again := True; Break; end;
  until False;
  if Again then Continue;
  { an inlined callee instruction the tier did not take: that call is not inlined }
  for K := OT^.CodeLen + 1 to OVLen - 1 do
    if (OVRole[K] = 2) and ((OExit[K] and not (OOpOf(OIns(K)) in [opThrow, opThrowConst]) and not OColdIC(K)) or
       (OEmbed[K] and not (OOpOf(OIns(K)) in OINL_EMBED))) and
       not OInlBan[OSites[OVSite[K]].K] then
    begin
      if OptLog then WriteLn(ErrOutput, 'ban at ', K, ' op ', OOpOf(OIns(K)), ' exit ', OExit[K]);
      OInlBan[OSites[OVSite[K]].K] := True;
      Again := True;
    end;
  until not Again;
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
{ whether a region can start at PC of T: its first instruction or a loop's header (a backward jump's target) }
function OIsEntry(T: PFuncTemplate; PC: Integer): Boolean;
var K: Integer;
begin
  if PC = 0 then Exit(True);
  for K := PC to T^.CodeLen - 1 do
    if (OOpOf(T^.Code[K]) = opJmp) and (K + 1 + OC(T^.Code[K]) = PC) then Exit(True);
  Result := False;
end;

{ the header of the innermost loop of T around PC (a backward jump at or past PC to at or before it), -1 none }
function OLoopAround(T: PFuncTemplate; PC: Integer): Integer;
var K, H: Integer;
begin
  Result := -1;
  for K := PC to T^.CodeLen - 1 do
    if (OOpOf(T^.Code[K]) = opJmp) and (OC(T^.Code[K]) < 0) then
    begin
      H := K + 1 + OC(T^.Code[K]);
      if (H <= PC) and (H > Result) then Result := H;
    end;
end;

{ whether the loop at header PC still has calls that have not run: a method whose inline cache is empty, or a
  callee not compiled yet (lazily compiled bodies: no code to inline) }
function OLoopCold(Fr: PFrame; T: PFuncTemplate; PC: Integer): Boolean;
var Lo, Hi, K, P: Integer; I, W: QWord; IC: PPropIC; V: TValue; Clo: PObj;
begin
  Result := False;
  if not OLoopSpan(T, PC, Lo, Hi) then Exit;
  if IsObject(Fr^.Closure) then Clo := ObjOf(Fr^.Closure) else Clo := nil;
  for K := Lo to Hi do
  begin
    I := T^.Code[K];
    if not (OOpOf(I) in [opCall, opNew]) then Continue;
    { the callee register's writer just before (a method or property read) }
    for P := K - 1 downto Max(Lo, K - 8) do
    begin
      W := T^.Code[P];
      if (OOpOf(W) in [opGetMethod, opGetProp]) and (OA(W) = OA(I)) then
      begin
        IC := @T^.ICs[OCU(W)];
        if (IC^.PolyCount = 0) and (IC^.Poly0.Shape = nil) then Exit(True);
        Break;
      end;
      if OWrites(W) = OA(I) then Break;
    end;
    V := OStaticValue(T, Clo, K, OA(I), 0);
    if IsObject(V) and (ObjOf(V)^.OKind = okFunction) and (PFuncTemplate(ObjOf(V)^.Native)^.CodeLen = 0) then
      Exit(True);
  end;
end;

procedure OptMaybe(Fr: PFrame; PC: Integer); noinline;
var Info: POptInfo; T: PFuncTemplate; Again: Boolean;
begin
  Again := False;
  T := Fr^.Tpl;
  Info := POptInfo(T^.Opt);
  if Info^.Deopts > OPT_MAX_DEOPTS then
  begin
    { the speculation failed too often: back to the baseline, and each region may be tried again (with the
      recorded feedback) }
    T^.JitTable := Info^.BaseTable;
    Info^.Table := nil;
    Info^.Deopts := 0;
    { new feedback (bounded: bits are only ever set) is not a failed recompile, but a template keeps a cap }
    if (Info^.FbDrop <> 0) and (Info^.FbDrops < 64) then Inc(Info^.FbDrops) else Inc(Info^.Recompiles);
    Info^.FbDrop := 0;
    FillChar(Info^.Tried[0], Length(Info^.Tried), 0);
    if OptLog then WriteLn(ErrOutput, 'opt ', PtrUInt(T) and $FFFFFF, ' dropped after deopts, recompiles ', Info^.Recompiles,
      ' feedback drops ', Info^.FbDrops);
    if Info^.Recompiles > 4 then begin T^.Opt := nil; Exit; end;
  end;
  if Info^.Count > 0 then Exit;
  { not a region's entry (the count ran out where the interpreter came back in, after a call): asked again at the
    next count-down point, no attempt counted }
  if not OIsEntry(T, PC) then
  begin
    { the innermost loop around PC (a call in its body brings the interpreter back in after the call): its region
      compiled from the frame now, entered at the next back edge's count-down }
    PC := OLoopAround(T, PC);
    if (PC < 0) or (Info^.Tried[PC] in [1, 2]) then
    begin
      Info^.Count := OPT_HOT shl Min(Info^.Fails, 20);
      Exit;
    end;
    Again := True;
  end;
  { the next run: longer after each region that was not optimized (a template without one stops asking) }
  Info^.Count := OPT_HOT shl Min(Info^.Fails, 20);
  if (Info^.Tried[PC] in [1, 2]) or (GetCurrentThreadId <> JitMainThread) then
  begin
    Inc(Info^.Fails);
    Exit;
  end;
  { a loop whose calls have not all run yet: asked again soon (Tried 10 .. 12 the times postponed) }
  if (Info^.Tried[PC] < 13) and OLoopCold(Fr, T, PC) then
  begin
    if Info^.Tried[PC] < 10 then Info^.Tried[PC] := 10 else Inc(Info^.Tried[PC]);
    Info^.Count := OPT_HOT div 4;
    if OptLog then WriteLn(ErrOutput, 'opt ', OptName(T), ' pc ', PC, ' postponed (calls not run yet)');
    Exit;
  end;
  if (OptMax >= 0) and (OptCompiles >= OptMax) then Exit;
  Inc(OptCompiles);
  if OptCompile(Fr, PC) then
  begin
    Info^.Tried[PC] := 1;
    if Again then Info^.Count := 1;
  end
  else
  begin
    Info^.Tried[PC] := 2;
    Inc(Info^.Fails);
    Info^.Count := OPT_HOT shl Min(Info^.Fails, 20);
  end;
  if OptLog then
    WriteLn(ErrOutput, 'opt ', OptName(T), ' pc ', PC, ' len ', T^.CodeLen, ' ok ', Info^.Tried[PC] = 1,
      ' nodes ', ONCount, ' blocks ', OBCount, ' spills ', OSpills, ' recompiles ', Info^.Recompiles, ' bytes ', JitLen,
      ' inlined ', OSiteCount, OSiteList, ' embeds', OEmbedList, ' exits', OExitList);
  if OptLog then Flush(ErrOutput);
end;

function InlinedFrameCount(I: Integer): Integer;
begin
  if Frames[I].Inl = nil then Result := 0 else Result := POInlDesc(Frames[I].Inl)^.N;
end;

procedure InlinedFrame(I, J: Integer; out Closure: TValue; out Tpl: PFuncTemplate; out Base, Argc: Integer);
var D: POInlDesc;
begin
  D := POInlDesc(Frames[I].Inl);
  Base := Frames[I].Base + D^.Lv[J].A + 1;
  Closure := VStack[Base - 1];
  Tpl := D^.Lv[J].T;
  Argc := D^.Lv[J].Argc;
end;
