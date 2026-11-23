{ G2.Artifact's instruction checks: every operand of every instruction against the template it is in, the control
  flow (jump targets, a terminator last, the exception handler depth at each instruction), and the instructions
  whose soundness rests on what only the compiler knows. The operand meanings are G2.VM's (Execute and FastLoop);
  the baseline JIT and the optimizing tier read them the same way and check nothing themselves, so the VM's bound
  is the binding one.

  What the code cannot show (a register holding an array, an object, a closure, a private name or a module's
  cell table where an instruction trusts it to) is checked at run time by those instructions. Two kinds of
  instruction are rewritten instead of trusted: a suspension's live register count becomes 0 (every register is
  kept), and a direct eval's caller description is parsed and bounded here. }

const
  { the instructions that trust a register's kind and are checked only in Execute (G2.VM's Opd* checks; opDefField
    is checked in every tier) }
  KIND_TRUSTING_OPS = [opArrPush, opArrHole, opArrSpread, opCallV, opNewV, opSuperCallV, opObjSpread, opCopyDataProps,
    opDefProp, opDefPropC, opDefMethod, opDefMethodC, opDefGetter, opDefSetter, opDefGetterC, opDefSetterC,
    opDefFieldC, opSetProtoLit, opSetFnName, opSetHome, opClassSetup, opClassField, opSuperCallUp,
    opGetPriv, opSetPriv, opDefPrivField, opAddPriv, opPrivIn, opModGet, opMakeModCell];
  { an object literal's inline slots (the compiler asks for at most 60) }
  MAX_NEWOBJ_HINT = 64;
  { a handler PC is packed with its kind into an int32 at a suspension (PC or Kind shl 28) }
  MAX_CODE_LEN = 1 shl 27;

procedure VerifyCode(T: PFuncTemplate);
var
  PC, A, B, C, CS, N, Target, I: Integer;
  Ins_: QWord;
  Op: TOp;
  Depth: array of Integer;
  Work: array of Integer;
  WorkTop, Prologue: Integer;
  DeclSeen: Boolean;

  procedure Bad(const Msg: string);
  begin
    Reject(T, PC, Msg);
  end;

  procedure Reg(R: Integer);
  begin
    if (R < 0) or (R >= T^.MaxRegs) then Bad(Format('register %d outside 0..%d', [R, T^.MaxRegs - 1]));
  end;

  { registers First .. First + Count - 1 }
  procedure Regs(First, Count: Integer);
  begin
    if Count <= 0 then Exit;
    if (First < 0) or (Int64(First) + Count > T^.MaxRegs) then
      Bad(Format('registers %d..%d outside 0..%d', [First, Int64(First) + Count - 1, T^.MaxRegs - 1]));
  end;

  procedure Konst(K: Integer);
  begin
    if (K < 0) or (K > High(T^.Consts)) then Bad(Format('constant %d outside 0..%d', [K, High(T^.Consts)]));
  end;

  { a constant the VM reads as a string atom without a tag check }
  procedure KonstStr(K: Integer);
  begin
    Konst(K);
    if not IsString(T^.Consts[K]) then Bad(Format('constant %d is not a string', [K]));
  end;

  procedure IC(K: Integer);
  begin
    if (K < 0) or (K > High(T^.ICs)) then Bad(Format('inline cache %d outside 0..%d', [K, High(T^.ICs)]));
    if T^.ICs[K].Key = nil then Bad(Format('inline cache %d has no key', [K]));
  end;

  procedure Upv(K: Integer);
  begin
    if (K < 0) or (K > High(T^.Upvals)) then Bad(Format('upvalue %d outside 0..%d', [K, High(T^.Upvals)]));
  end;

  procedure OneOf(V, Lo, Hi: Integer; const What: string);
  begin
    if (V < Lo) or (V > Hi) then Bad(Format('%s %d outside %d..%d', [What, V, Lo, Hi]));
  end;

  { a signed C that is an IC (>= 0) or the register -1 - C (< 0) }
  procedure ICOrNegReg(K: Integer);
  begin
    if K >= 0 then IC(K) else Reg(-1 - K);
  end;

  procedure Jump(Off: Integer);
  begin
    Target := PC + 1 + Off;
    if (Target < 0) or (Target >= T^.CodeLen) then Bad(Format('jump target %d outside the code', [Target]));
  end;

  function NameIn(const L: array of PStr; P: PStr): Boolean;
  var J: Integer;
  begin
    for J := 0 to High(L) do
      if L[J] = P then Exit(True);
    Result := False;
  end;

  { the caller description of a direct eval (G2.Compiler BuildEvalDesc): entries of five fields (name, kind,
    index, flags, depth) separated by #2 and each ended by #1. Kind 0 names a register of this template, kind 1
    an upvalue; G2.VM MakeEvalClosure opens them as the eval code's bindings }
  procedure EvalDesc(K: Integer);
  var S: UnicodeString; P, Q, F, Kind, Idx, V, Code: Integer; Fields: array[0..4] of UnicodeString;
  begin
    KonstStr(K);
    S := StrToUnicode(PStr(PtrOf(T^.Consts[K])));
    P := 1;
    while P <= Length(S) do
    begin
      Q := P;
      while (Q <= Length(S)) and (S[Q] <> #1) do Inc(Q);
      if Q > Length(S) then Bad('unterminated direct eval description');
      F := 0;
      Fields[0] := '';
      while P < Q do
      begin
        if S[P] = #2 then
        begin
          Inc(F);
          if F > 4 then Bad('direct eval description entry with more than five fields');
          Fields[F] := '';
        end
        else Fields[F] := Fields[F] + S[P];
        Inc(P);
      end;
      if F <> 4 then Bad('direct eval description entry without five fields');
      Val(string(Fields[1]), Kind, Code);
      if (Code <> 0) or (Kind < 0) or (Kind > 1) then Bad('direct eval description with an unknown binding kind');
      Val(string(Fields[2]), Idx, Code);
      if Code <> 0 then Bad('direct eval description with an invalid index');
      if Kind = 0 then Reg(Idx) else Upv(Idx);
      Val(string(Fields[3]), V, Code);
      if Code <> 0 then Bad('direct eval description with invalid flags');
      Val(string(Fields[4]), V, Code);
      if (Code <> 0) or (V < 0) then Bad('direct eval description with an invalid depth');
      P := Q + 1;
    end;
  end;

  procedure Visit(At, D: Integer);
  begin
    if Depth[At] = -1 then
    begin
      Depth[At] := D;
      Work[WorkTop] := At;
      Inc(WorkTop);
    end
    else if Depth[At] <> D then
      Reject(T, At, Format('reached with %d and with %d open exception handlers', [Depth[At], D]));
  end;

begin
  if T^.CodeLen > MAX_CODE_LEN then Reject(T, -1, 'code too long');
  if T^.EntryPC <> 0 then Reject(T, -1, 'entry not at the first instruction');
  { the baseline JIT's callee fill needs two registers past the parameters (the compiler leaves six) }
  if T^.NumParams + 2 > T^.MaxRegs then Reject(T, -1, Format('%d parameters in %d registers', [T^.NumParams, T^.MaxRegs]));
  if T^.Name = nil then Reject(T, -1, 'no name');
  for I := 0 to High(T^.CallSites) do
    if T^.CallSites[I].Text = nil then Reject(T, -1, 'call site without text');
  if T^.IsDerived and not T^.IsClassCtor then Reject(T, -1, 'derived but not a class constructor');
  if T^.IsClassCtor and (T^.IsArrow or T^.IsGenerator or T^.IsAsync) then
    Reject(T, -1, 'class constructor that is an arrow, a generator or async');
  if T^.IsAsync and not T^.IsGenerator and ((T^.AsyncReg < 0) or (T^.AsyncReg >= T^.MaxRegs)) then
    Reject(T, -1, 'async function without its promise register');
  T^.Loaded := True;
  T^.Trusting := False;
  N := 0;
  DeclSeen := False;
  for PC := 0 to T^.CodeLen - 1 do
  begin
    Ins_ := T^.Code[PC];
    Op := InsOp(Ins_);
    A := InsA(Ins_);
    B := InsB(Ins_);
    C := InsC(Ins_);
    CS := C;
    C := InsCU(Ins_);
    case Op of
      opNop, opLoadHome, opLoadNewTarget, opRetUndef, opImportMeta, opLoadCallee, opCovLine: ;
      opMov, opNeg, opBitNot, opNot, opTypeof, opToNum, opToNumeric, opToStr, opWithObj, opSuperBase:
        begin Reg(A); if not (Op in [opLoadHome, opLoadNewTarget, opWithObj]) then Reg(B); end;
      opLoadK: begin Reg(A); Konst(C); end;
      opLoadI, opLoadUndef, opLoadNull, opLoadTrue, opLoadFalse, opLoadHole, opRet, opThrow, opCheckThis,
      opIterClose, opIterCheckObj:
        Reg(A);
      opGetUpv, opSetUpv: begin Reg(A); Upv(B); end;
      opClose:
        begin
          OneOf(A, 0, T^.MaxRegs, 'first register to close');
          OneOf(B, 0, T^.MaxRegs, 'end of the registers to clear');
        end;
      opGetGlobal, opSetGlobal, opTypeofGlobal, opHasGlobalRef: begin Reg(A); IC(C); end;
      opGetProp, opGetLength: begin Reg(A); Reg(B); IC(C); end;
      opSetProp: begin Reg(A); Reg(B); IC(C); end;
      opGetMethod: begin Reg(A); Reg(A + 1); Reg(B); IC(C); end;
      opGetElem, opSetElem, opDelElem, opIn, opInstanceof, opAdd, opSub, opMul, opDiv, opMod, opPow, opBitAnd,
      opBitOr, opBitXor, opShl, opShr, opUShr, opEq, opNe, opSEq, opSNe, opLt, opLe, opGt, opGe:
        begin Reg(A); Reg(B); Reg(C); end;
      opDelProp: begin Reg(A); Reg(B); KonstStr(C); end;
      opGetSuper: begin Reg(A); Reg(B); Reg(0); ICOrNegReg(CS); end;
      opSetSuper: begin Reg(A); Reg(A + 1); Reg(B); ICOrNegReg(CS); end;
      opGetSuperB, opSetSuperB: begin Reg(A); Reg(B); Reg(B + 1); ICOrNegReg(CS); end;
      opAddI, opSubI, opIncI, opBitAndI, opBitOrI, opBitXorI, opShlI, opShrI, opUShrI: begin Reg(A); Reg(B); end;
      opToPropKey: begin Reg(A); Reg(B); if C <> 0 then Reg(C - 1); end;
      opJmp: Jump(CS);
      opJT, opJF, opJNullish, opJNotNullish, opJUndef, opJNotUndef: begin Reg(A); Jump(CS); end;
      opJNLt, opJNLe, opJNGt, opJNGe, opJNSEq, opJSEq: begin Reg(A); Reg(B); Jump(CS); end;
      opCall, opNew, opSuperCall:
        begin
          Regs(A, B + 2);
          OneOf(C, 0, 1, 'call flag');
          if Op <> opCall then Reg(0);
        end;
      opCallV, opNewV, opSuperCallV:
        begin
          Regs(A, 3);
          OneOf(C, 0, 1, 'call flag');
          if Op = opSuperCallV then Reg(0);
        end;
      opNewObj: begin Reg(A); OneOf(C, 0, MAX_NEWOBJ_HINT, 'object size hint'); end;
      opNewArr: begin Reg(A); Regs(C, B); end;
      opArrPush, opArrSpread: begin Reg(A); Reg(B); end;
      opArrHole: Reg(A);
      opDefProp, opDefMethod, opDefGetter, opDefSetter:
        begin
          Reg(A);
          if Op in [opDefGetter, opDefSetter] then Reg(B and $7FFF) else Reg(B);
          KonstStr(C);
        end;
      opDefPropC: begin Reg(A); Reg(B); Reg(C); end;
      opDefMethodC: begin Reg(A); Reg(B); Reg(C); end;
      opDefGetterC, opDefSetterC: begin Reg(A); Reg(B and $7FFF); Reg(C); end;
      opObjSpread, opCopyDataProps: begin Reg(A); Reg(B); if C <> 0 then Reg(C - 1); end;
      opClosure:
        begin
          Reg(A);
          if (C < 0) or (C > High(T^.Children)) then Bad(Format('function %d outside 0..%d', [C, High(T^.Children)]));
        end;
      opClassSetup: begin Reg(A); Reg(B); OneOf(C, 0, 1, 'derived flag'); end;
      opClassField: begin Reg(A); Reg(B); end;
      opGetIter, opGetAsyncIter, opForIn:
        begin
          Regs(A, 2);
          Reg(B);
          if Op = opForIn then Reg(C);
        end;
      opIterNext: begin Reg(A); Regs(B, 2); Jump(CS); end;
      opTry: begin Reg(A); OneOf(B, 0, 1, 'handler kind'); Jump(CS); end;
      opEndTry: ;
      opThrowConst:
        if B <> 1 then KonstStr(C);
      opTemplateObj: begin Reg(A); OneOf(B, 0, 1, 'template object mode'); end;
      opYield, opAwait: begin Reg(A); Reg(B); end;
      opRegExp: begin Reg(A); KonstStr(B); KonstStr(C); end;
      opDeclGlobals:
        begin
          if T^.GdiKind = 0 then Bad('global declarations outside a Script''s top level');
          if DeclSeen then Bad('global declarations twice');
          DeclSeen := True;
          { the lexical cells (opMakeGLex) and the function bindings (opDefGlobalFunc) rely on what it set and
            checked: it comes before them in the code, and nothing jumps back over it (checked below) }
        end;
      opDefGlobalFunc:
        begin
          Reg(A);
          OneOf(B, 0, 1, 'configurable flag');
          KonstStr(C);
          if not DeclSeen then Bad('global function binding before the global declarations');
          if not NameIn(T^.GdiFuncs, PStr(PtrOf(T^.Consts[C]))) then Bad('global function binding not declared');
        end;
      opMakeGLex:
        begin
          Reg(A);
          if not DeclSeen then Bad('global lexical binding before the global declarations');
          if (C < 0) or (C > High(T^.GdiLex)) then Bad(Format('global lexical binding %d outside 0..%d', [C, High(T^.GdiLex)]));
        end;
      opNewPrivName: begin Reg(A); KonstStr(C); end;
      opGetPriv: begin Reg(A); Reg(B); Reg(C); end;
      opSetPriv, opDefPrivField: begin Reg(A); Reg(B); Reg(C); end;
      opAddPriv:
        begin
          Reg(A);
          if B and $8000 <> 0 then Regs(B and $7FFF, 2) else Reg(B);
          Reg(C);
        end;
      opPrivIn: begin Reg(A); Reg(B); Reg(C); end;
      opDefField: begin Reg(A); Reg(B); IC(C); end;
      opDefFieldC: begin Reg(A); Reg(B); Reg(C); end;
      opSetHome: begin Reg(A); Reg(B); end;
      opYieldStar: begin Reg(A); Regs(B, 4); end;
      opYieldStarAsync: begin Reg(A); Regs(B, 5); end;
      opSuperCallUp: begin Regs(A, B + 2); Upv(C); end;
      opIterCallNext: begin Reg(A); Regs(B, 2); end;
      opIterUnpack: begin Reg(A); Reg(B); Jump(CS); end;
      opAsyncIterReturn: begin Reg(A); Reg(B); Jump(CS); end;
      opSetFnName: begin Reg(A); Reg(B); OneOf(C, 0, 2, 'name prefix'); end;
      opCheckHole: begin Reg(A); KonstStr(C); end;
      opGenStart: ;
      opIterCloseThrow: begin Reg(A); Reg(B); OneOf(C, 0, 1, 'completion flag'); end;
      opEvalHint:
        begin
          OneOf(A, 0, 2, 'eval goal');
          if C <> 0 then EvalDesc(C - 1);
          { the next instruction is the call it hints: the VM reads its callee register from there }
          if (PC + 1 >= T^.CodeLen) or not (InsOp(T^.Code[PC + 1]) in [opCall, opCallV]) then
            Bad('direct eval hint not followed by its call');
        end;
      opSetProtoLit: begin Reg(A); Reg(B); end;
      opMakeModCell, opModGet: begin Reg(A); Reg(B); end;
      opImport:
        begin
          Reg(A);
          Reg(B);
          if (C and $FFFF) <> $FFFF then Reg(C and $FFFF);
          if C shr 21 <> 0 then Bad('unknown import flags');
        end;
      opWithHas: begin Reg(A); Reg(B); KonstStr(C); end;
      opWithGet: begin Reg(A); Reg(B); KonstStr(C and $7FFFFF); end;
      opWithSet: begin Reg(A); Reg(B); KonstStr(C and $3FFFFF); end;
      opDelGlobal: begin Reg(A); KonstStr(C); end;
      opIntrinsic:
        begin
          Regs(A, B);
          Reg(A);
          if C = INTRINSIC_FORIN_VISIT then
          begin
            if B <> 2 then Bad('for-in visit with other than two operands');
          end
          else if (C < 0) or (C >= INTRINSIC_COUNT) then Bad(Format('intrinsic %d unknown', [C]));
        end;
      opCheckType: begin Reg(A); OneOf(C, TYPE_NUMBER, TYPE_OBJECT, 'type'); end;
    else
      { never emitted (opRest, opDebugger, opSwap, opInc, opDec, opSpreadCallArgs), or tied to the compiling
        engine's global lexical table (opGetGLex, opSetGLex: a file is compiled without them) }
      Bad(Format('instruction %s is not allowed in a bytecode file', [GetEnumName(TypeInfo(TOp), Ord(Op))]));
    end;
    case Op of
      opYield, opAwait, opYieldStar, opYieldStarAsync:
        if not T^.IsGenerator and not T^.IsAsync then Bad('suspension outside a generator or async function');
    end;
    if (Op in [opAwait, opYieldStarAsync]) and not T^.IsAsync then Bad('await outside an async function');
    if (Op in [opYield, opYieldStar]) and not T^.IsGenerator then Bad('yield outside a generator');
    if Op = opGenStart then
    begin
      Inc(N);
      if not T^.IsGenerator then Bad('generator start outside a generator');
    end;
    if Op in KIND_TRUSTING_OPS then T^.Trusting := True;
    { a suspension keeps every register (its live count, a compiler proof, becomes 0: all of them) }
    if Op in [opYield, opAwait, opYieldStar, opYieldStarAsync, opGenStart] then
      T^.Code[PC] := Ins(Op, A, B, 0);
  end;
  if T^.IsGenerator <> (N = 1) then Reject(T, -1, 'a generator has exactly one generator start');
  if (T^.GdiKind <> 0) and not DeclSeen and
     ((Length(T^.GdiLex) > 0) or (Length(T^.GdiVars) > 0) or (Length(T^.GdiFuncs) > 0)) then
    Reject(T, -1, 'global declarations never run');
  { a terminator last: no instruction runs off the end }
  PC := T^.CodeLen - 1;
  if not (InsOp(T^.Code[PC]) in [opRet, opRetUndef, opJmp, opThrow, opThrowConst]) then Bad('the code runs off its end');
  { the exception handler depth at each instruction, over the control flow from the entry: the same on every path
    to it, never below zero at an opEndTry. Every opEndTry then pops a handler its own frame pushed. The global
    declarations, the generator start and the opDefGlobalFunc / opMakeGLex instructions after them stay before
    every backward jump's target: nothing runs them again or skips the declarations. }
  SetLength(Depth, T^.CodeLen);
  SetLength(Work, T^.CodeLen);
  for I := 0 to T^.CodeLen - 1 do Depth[I] := -1;
  WorkTop := 0;
  Visit(0, 0);
  while WorkTop > 0 do
  begin
    Dec(WorkTop);
    PC := Work[WorkTop];
    Ins_ := T^.Code[PC];
    Op := InsOp(Ins_);
    CS := InsC(Ins_);
    N := Depth[PC];
    case Op of
      opRet, opRetUndef, opThrow, opThrowConst: ;
      opJmp: Visit(PC + 1 + CS, N);
      opTry:
        begin
          Visit(PC + 1 + CS, N);
          Visit(PC + 1, N + 1);
        end;
      opEndTry:
        begin
          if N < 1 then Bad('exception handler popped that this function did not push');
          Visit(PC + 1, N - 1);
        end;
      opJT, opJF, opJNullish, opJNotNullish, opJUndef, opJNotUndef, opJNLt, opJNLe, opJNGt, opJNGe, opJNSEq,
      opJSEq, opIterNext, opIterUnpack, opAsyncIterReturn:
        begin
          Visit(PC + 1 + CS, N);
          Visit(PC + 1, N);
        end;
    else
      Visit(PC + 1, N);
    end;
  end;
  { what must run once, at the start (the global declarations, a generator's start after its parameters'
    defaults): a jump before it stays before it (none skips it) and one after it stays after it (none runs it
    again), so every path runs it once and before what follows it }
  Prologue := -1;
  for PC := 0 to T^.CodeLen - 1 do
    if InsOp(T^.Code[PC]) in [opDeclGlobals, opGenStart] then Prologue := PC;
  if Prologue >= 0 then
    for PC := 0 to T^.CodeLen - 1 do
    begin
      Op := InsOp(T^.Code[PC]);
      if Op in [opJmp, opJT, opJF, opJNullish, opJNotNullish, opJUndef, opJNotUndef, opJNLt, opJNLe, opJNGt,
        opJNGe, opJNSEq, opJSEq, opIterNext, opIterUnpack, opAsyncIterReturn, opTry] then
        if (PC < Prologue) <> (PC + 1 + InsC(T^.Code[PC]) <= Prologue) then
          Reject(T, PC, 'jump into or around the function''s prologue');
    end;
end;
