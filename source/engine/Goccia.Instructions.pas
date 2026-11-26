unit Goccia.Instructions;
{ Instruction format: 64 bits = op (8) | A (16) | B (16) | C (24, signed where an offset). }
{$mode delphiunicode}{$H+}
{$I Goccia.Checks.inc}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}

interface

uses
  Goccia.Core, Goccia.SourceSpan;

const
  { opCheckType's types }
  TYPE_NUMBER = 1;
  TYPE_BOOLEAN = 2;
  TYPE_STRING = 3;
  TYPE_OBJECT = 4;
  { prototype hops a get IC's entry follows to the property's holder (VerMask has a bit for each) }
  IC_MAX_DEPTH = 8;

type
  TOp = (
    opNop, opMov, opLoadK, opLoadI, opLoadUndef, opLoadNull, opLoadTrue, opLoadFalse, opLoadHole,
    opGetUpv, opSetUpv, opClose,
    opGetGlobal, opSetGlobal, opTypeofGlobal,
    opGetProp, opSetProp, opGetElem, opSetElem, opDelProp, opDelElem, opIn, opInstanceof,
    opGetSuper, opSetSuper, opLoadHome, opLoadNewTarget,
    opAdd, opSub, opMul, opDiv, opMod, opPow,
    opBitAnd, opBitOr, opBitXor, opShl, opShr, opUShr,
    opAddI,                  { R[A] = R[B] + C (immediate): binary +, so a string concatenates, a BigInt throws }
    opSubI,                  { R[A] = R[B] - (-C): binary - of a Number constant, C holds the negated constant }
    opIncI,                  { R[A] = ToNumeric(R[B]) + C: ++ and --, a BigInt steps by C }
    opBitAndI, opBitOrI, opBitXorI, opShlI, opShrI, opUShrI,   { R[A] = R[B] op C (an int32 immediate) }
    opNeg, opBitNot, opNot, opTypeof, opToNum, opToNumeric, opToStr, opToPropKey,
    opEq, opNe, opSEq, opSNe, opLt, opLe, opGt, opGe,
    opJmp, opJT, opJF, opJNullish, opJNotNullish, opJUndef, opJNotUndef,
    opJNLt, opJNLe, opJNGt, opJNGe, opJNSEq, opJSEq,
    opCall, opCallV, opNew, opNewV, opSuperCall, opSuperCallV, opRet, opRetUndef,
    opNewObj, opNewArr, opArrPush, opArrSpread, opArrHole,
    opDefProp, opDefPropC, opDefMethod, opDefMethodC, opDefGetter, opDefSetter, opDefGetterC, opDefSetterC,
    opObjSpread, opCopyDataProps,
    opClosure, opClassSetup, opClassField,
    opGetIter, opIterNext, opIterClose, opForIn,
    opTry, opEndTry, opThrow, opThrowConst,
    opRest, opTemplateObj,
    opYield, opAwait, opRegExp, opDebugger, opSwap, opInc, opDec,
    opSpreadCallArgs, opGetLength,
    { global declarations (ES2026 16.1.7 GlobalDeclarationInstantiation, 19.2.1.3 EvalDeclarationInstantiation) }
    opDeclGlobals,           { the template's Gdi record: checks, then the lexical and var bindings }
    opDefGlobalFunc,         { CreateGlobalFunctionBinding(Consts[C], R[A]), configurable when B = 1 }
    opMakeGLex,              { global lexical binding Gdi.LexBase + C is the cell of this frame's register A }
    opGetGLex, opSetGLex,    { R[A] from / to global lexical binding C }
    { classes (15.7): private names are key atoms (kfPrivate) held in registers }
    opNewPrivName,           { R[A] := a fresh private name described by Consts[C] }
    opGetPriv,               { R[A] := R[B].#R[C] (PrivateGet) }
    opSetPriv,               { R[A].#R[C] := R[B] (PrivateSet) }
    opDefPrivField,          { PrivateFieldAdd(R[A], R[C], R[B]) }
    opAddPriv,               { PrivateMethodOrAccessorAdd(R[A], R[C], method R[B]; B or $8000: get R[B], set R[B + 1]) }
    opPrivIn,                { R[A] := #R[C] in R[B] }
    opDefField, opDefFieldC, { CreateDataPropertyOrThrow(R[A], ICs[C].Key / R[C], R[B]); opDefField caches the add transition }
    opSetHome,               { closure R[A]'s home object := R[B] }
    opCheckThis,             { ReferenceError if R[A] is a derived constructor's this before super() }
    opYieldStar,             { one step of yield* (15.5.5): R[B] iterator, R[B+1] next, R[B+2] received value,
                               R[B+3] received mode (0 next, 1 throw, 2 return); suspends and runs again, or
                               completes with R[A] := the inner result's value }
    opLoadCallee,            { R[A] := the running function (a derived constructor's, for super() in its arrows) }
    opSuperCallUp,           { super() in an arrow: R[A] constructor, R[A+1] new.target, B args from R[A+2]; this is
                               bound through upvalue C }
    opGetAsyncIter,          { 7.4.2 GetIterator(R[B], async): R[A] iterator, R[A + 1] next }
    opIterCallNext,          { R[A] := Call(R[B + 1], R[B]); an abrupt call leaves R[B] done }
    opIterUnpack,            { R[A] (an iterator result) not an object: TypeError; done: jump C; else R[A] := its value }
    opAsyncIterReturn,       { R[A] := Call(GetMethod(R[B], "return"), R[B]); no method (or R[B] done): jump C }
    opIterCheckObj,          { TypeError unless R[A] is an object }
    opYieldStarAsync,        { one step of yield* in an async generator: R[B] iterator, R[B+1] next, R[B+2] received
                               value, R[B+3] mode, R[B+4] phase }
    opSetFnName,             { SetFunctionName(R[A], key R[B]); C: 0 no prefix, 1 "get", 2 "set" }
    opCheckHole,             { ReferenceError (Consts[C] the name) if R[A] is a lexical binding in its TDZ }
    opGenStart,              { a generator after FunctionDeclarationInstantiation: made here, suspended at start and
                               returned to the caller (15.5.4, 27.6.3.2 order) }
    opIterCloseThrow,        { IteratorClose(R[A]) for a throw completion: return() runs, its errors are ignored }
    opEvalHint               { the next instruction is a direct eval call; A: 2 its var declarations are global
                               (non-strict code at a Script's top level), 0 they stay its own (until the engine has
                               direct eval's caller scope) }
    ,opSetProtoLit           { 13.2.5.5 __proto__: v in an object literal: R[A]'s prototype becomes R[B] if an object or null }
    ,opMakeModCell           { module: R[B] (the cell table)[C] := the cell of this frame's register A }
    ,opModGet                { module: R[A] := the value of cell R[B][C] (an import), ReferenceError before its initialization }
    ,opImport                { 13.3.10 import(R[B], R[C]): a promise (C = 0xFFFF: no options) }
    ,opImportMeta            { 13.3.12 import.meta of this code's module }
    ,opWithObj               { 14.11.2 with: R[A] := ToObject(R[A]) }
    ,opWithHas               { R[A] := HasBinding of object environment R[B] for Consts[C] (with @@unscopables) }
    ,opWithGet               { R[A] := Get(R[B], Consts[C]) }
    ,opWithSet               { Set(R[A], Consts[C], R[B], false) }
    ,opDelGlobal             { R[A] := delete of the global object's property Consts[C] (false for a global lexical) }
    ,opSuperBase             { 13.3.7 a super reference's GetSuperBase: R[A] := the prototype of home object R[B] }
    ,opGetSuperB             { R[A] := GetValue of the super reference with base R[B], this R[B + 1], key ICs[C] or
                               R[-1 - C] (ToPropertyKey'd in place) }
    ,opSetSuperB             { PutValue(that super reference, R[A]) }
    ,opHasGlobalRef          { R[A] := whether the global environment has a binding for ICs[C].Key (9.1.1.4.1
                               HasBinding: a strict assignment's reference, resolved before its value) }
    ,opGetMethod             { a method call's callee and this: R[A + 1] := R[B], then R[A] := R[A + 1].ICs[C] (as
                               opGetProp) }
    ,opIntrinsic             { R[A] := Intrinsics[C](R[A], ..., R[A + B - 1]): a helper of the builtins5 prelude for
                               a construct the compiler lowers to a call (INTRINSIC_* in Goccia.VM) }
    ,opCheckType             { a TypeError unless R[A] has type C (strict-types: TYPE_NUMBER, TYPE_BOOLEAN,
                               TYPE_STRING, TYPE_OBJECT) }
    ,opCovLine               { nothing: under coverage, the instruction of a source line that would have none (an
                               operand a register already holds), so the line counts as executed }
  );

  PFuncTemplate = ^TFuncTemplate;

  TUpvalDesc = record
    FromParentLocal: Boolean;   { True: parent register Index; False: parent upvalue Index }
    Index: Integer;
  end;

  { a get IC entry: a receiver of this shape and object kind finds Key at prototype depth Depth, slot Slot,
    while the prototypes on the way have the shapes in Holder }
  PICEntry = ^TICEntry;
  TICEntry = record
    Shape: PShape;
    Kind: Byte;
    Depth: Byte;
    VerMask: Byte;                   { bit d: the object at depth d (before the holder) is a dictionary }
    Acc: Byte;                       { 1: the slot holds an accessor (FastLoop calls a bytecode getter inline) }
    Slot: Integer;
    Holder: array[0..IC_MAX_DEPTH - 1] of PShape;
    Vers: array[0..IC_MAX_DEPTH - 1] of Cardinal;   { those dictionaries' versions when the entry was made }
  end;

  TICEntryMore = array[1..3] of TICEntry;
  PICEntryMore = ^TICEntryMore;

  PPropIC = ^TPropIC;
  TPropIC = record
    Key: PStr;
    Shape: PShape;          { receiver shape (stores and globals) }
    Slot: Integer;
    Depth: Integer;         { 0 own, 1.. prototype hops }
    Holder: array[0..3] of PShape;
    NewShape: PShape;       { store IC: transition target }
    Misses: Integer;
    Poly0: TICEntry;        { property gets: the first receiver shape, checked inline }
    More: PICEntryMore;     { up to three more shapes, allocated when a second one is seen: most sites
                              stay monomorphic, and four inline entries made an IC 336 bytes }
    PolyCount, PolyNext: Byte;
  end;

  TCallSite = record
    PC: Integer;
    Text: PStr;
  end;

  TFuncTemplate = record
    Name: PStr;
    Code: array of QWord;
    CodeLen: Integer;
    EntryPC: Integer;        { first instruction a call runs: the prologue's unused placeholders are skipped }
    { set by FinishTemplate once the arrays are final: the first constant and IC (nil when there are none), and
      whether a call can push the frame directly (not async, no rest parameter, no arguments object) }
    KBase: PValue;
    ICBase: PPropIC;
    FastCall: Boolean;
    { no upvalue can be open on its frame's registers: no nested function, no arguments object, no direct eval,
      not a Script's or module's top level (their lexical cells) }
    NoCaptures: Boolean;
    { no opTry: a return from its frame has no exception handlers of the frame's to pop }
    NoHandlers: Boolean;
    Consts: array of TValue;
    Children: array of PFuncTemplate;
    Upvals: array of TUpvalDesc;
    ICs: array of TPropIC;
    Lines: array of Integer;
    Cols: array of Integer;   { each instruction's source column (1-based, 0 unknown), beside Lines }
    { the callee of each call and construct site as its source wrote it (Goccia's CalleeDescriptorFor), by the
      instruction's index in ascending order: a callee that is not callable is named so in the TypeError }
    CallSites: array of TCallSite;
    NumParams: Integer;
    MaxRegs: Integer;
    RestReg: Integer;       { -1 none }
    ArgsReg: Integer;       { -1 none }
    HomeReg: Integer;       { register holding the home object, -1 none }
    NewTargetReg: Integer;
    IsArrow, IsClassCtor, IsDerived, IsMethod, IsGenerator, IsAsync, IsStrict: Boolean;
    SimpleParams: Boolean;  { no rest, default or pattern parameter: a sloppy one gets a mapped arguments object }
    { 10.4.4 the arguments object aliases the parameters (a sloppy function with simple parameters whose aliasing
      the compiler could not rule out: a parameter written, arguments escaping, or a direct eval) }
    MappedArgs: Boolean;
    ExpectedProps: Integer;
    { new F(...)'s caches (emptied when a shape they name is retired: ForgetRetiredShapes): F's shape and its
      prototype property's slot, the root shape of the last prototype, the field initializer's slot in F's shape }
    CtorFnShape: Pointer;
    CtorProtoSlot: Integer;
    CtorRootShape: Pointer;
    FIShape: Pointer;
    FISlot: Integer;
    NoHIShape: Pointer;   { instanceof: F's shape, known to have no own @@hasInstance under Function.prototype }
    FieldInit: PFuncTemplate;   { class field initializer, run on construction }
    SourceName: UnicodeString;
    SourceRange: TGocciaLazySourceText;   { the function's source for Function.prototype.toString: a range of the parse's source, copied only when read }
    Length: Integer;          { function.length }
    { the top level of a Script (GdiKind 1) or of non-strict eval code (2): its global declarations }
    GdiKind: Byte;
    TopLevel: Boolean;   { the code of a Script, a module or eval code (no function of its own) }
    GdiLex: array of PStr;        { let, const and class names, in register order of GdiLexRegs }
    GdiLexConst: array of Boolean;
    GdiVars: array of PStr;       { var names (functions excluded) }
    GdiFuncs: array of PStr;      { function declaration names, last declaration of a name only }
    GdiLexBase: Integer;          { first global lexical binding index, set when the declarations run }
    AsyncReg: Integer;            { an async function's register holding its promise, -1 otherwise }
    ModuleId: Integer;            { the Source Text Module Record (Goccia.Modules) whose code this is, 0 none }
    DeclLine, DeclCol: Integer;   { where the function's source starts (coverage locates the function there), 0 unknown,
                                    DeclLine -1 for one the source does not have (a class's implicit constructor) }
    { a lazily compiled function not compiled yet (Goccia.Lazy's PLazyStub): no code, FastCall False; its first call
      compiles it (LazyCompileHook) }
    Lazy: Pointer;
    { the baseline JIT (Goccia.Jit.inc): Jit the machine code (nil: none), JitTable its entry address for each
      instruction, JitState 0 not tried, 1 compiled, 2 not compiled (unsupported template) }
    Jit: Pointer;
    JitTable: Pointer;
    JitState: Byte;
    JitCallKind: Byte;   { set with JitState 1: bit 0 the shared call stub calls it inline (FastCall, not a
                           generator), bit 1 strict or an arrow (its this as given) }
    JitCount: Integer;   { entries before it is compiled (the JIT compiles hot templates only) }
    Opt: Pointer;        { the optimizing tier's state (Goccia.Opt.inc's POptInfo), nil when the template is not eligible }
    JitComplete: Boolean;   { the code takes every instruction but returns (XJitCall's callees) }
    JitCall: Pointer;    { set with JitState 1 and JitCallKind bit 0: the call and new stubs' entry into the code
                           (it sets the registers past the arguments undefined first) }
    JitEnter: Pointer;   { set with JitCall: compiled code's call sites' entry (rdi &R[A] of the caller, ecx A, esi
                           Argc | the this flag shl 16, r12 the caller's frame): the frame pushed with the template's
                           constants, then JitCall's work; eax -3 when it cannot (nothing changed: the call stub's) }
    OptFb: PByte;        { the optimizing tier's feedback, a byte per instruction (CodeLen + 1, nil until needed):
                           the guards that failed there, in the template's own regions or wherever it is inlined }
    ProfIdx: Integer;    { the profiler's (Goccia.Profile): 0 not seen yet, -1 not profiled (the host's code), else its
                           function profile's index + 1 }
    ProfSuspends: Byte;  { Goccia.Profile's: 0 not seen yet, 1 the code has an opGenStart (a call only creates the
                           generator or async state, its resumptions run the body), 2 not }
    { read from a bytecode file (Goccia.Artifact): its instructions that trust a register's kind check it (Goccia.VM's
      Opd* checks in Execute); FastLoop and the baseline JIT hand them to Execute, the optimizing tier leaves the
      template alone when it has one (Trusting) }
    Loaded: Boolean;
    Trusting: Boolean;
  end;

function Ins(Op: TOp; A, B: Integer; C: Integer): QWord; inline;
function InsOp(I: QWord): TOp; inline;
function InsA(I: QWord): Integer; inline;
function InsB(I: QWord): Integer; inline;
function InsC(I: QWord): Integer; inline;      { signed 24-bit }
function InsCU(I: QWord): Integer; inline;     { unsigned }

{ Every template this thread's engine made, so EngineDone can free them: templates live as long as the
  engine (closures and the code of running functions point at them). }
procedure RegisterTemplate(T: PFuncTemplate);
procedure FinishTemplate(T: PFuncTemplate);
procedure FreeAllTemplates;
var
  { Goccia.Lazy's: frees a template's lazy stub (TFuncTemplate.Lazy), one a function never called still holds }
  FreeLazyStubHook: procedure(P: Pointer) = nil;
  { Goccia.VM's: frees what the baseline JIT and the optimizing tier made for a template (its entry tables, its
    optimizing state); not its code, which stays in the JIT's chunks }
  FreeJitStateHook: procedure(T: PFuncTemplate) = nil;
{ clears every cached add transition of this thread's templates (Goccia.Core ProtoChanged) }
procedure ForgetAddTransitions;
{ forgets the retired shapes (Goccia.Core ShapeRetired) this thread's templates cache: an inline cache that names one
  starts over (its receiver part or its entries), a constructor cache is emptied }
procedure ForgetRetiredShapes;

implementation

uses
  Math, Goccia.ThreadCleanupRegistry;

function Ins(Op: TOp; A, B: Integer; C: Integer): QWord; inline;
begin
  Result := QWord(Ord(Op)) or (QWord(A and $FFFF) shl 8) or (QWord(B and $FFFF) shl 24) or
    (QWord(C and $FFFFFF) shl 40);
end;
function InsOp(I: QWord): TOp; inline; begin Result := TOp(I and $FF); end;
function InsA(I: QWord): Integer; inline; begin Result := (I shr 8) and $FFFF; end;
function InsB(I: QWord): Integer; inline; begin Result := (I shr 24) and $FFFF; end;
function InsC(I: QWord): Integer; inline; begin Result := SarInt64(Int64(I), 40); end;
function InsCU(I: QWord): Integer; inline; begin Result := I shr 40; end;


procedure FinishTemplate(T: PFuncTemplate);
var I: Integer;
begin
  if Length(T^.Consts) > 0 then T^.KBase := @T^.Consts[0] else T^.KBase := nil;
  if Length(T^.ICs) > 0 then T^.ICBase := @T^.ICs[0] else T^.ICBase := nil;
  T^.FastCall := not T^.IsAsync and (T^.RestReg < 0) and (T^.ArgsReg < 0);
  T^.NoHandlers := True;
  for I := 0 to T^.CodeLen - 1 do
    if InsOp(T^.Code[I]) = opTry then
    begin
      T^.NoHandlers := False;
      Break;
    end;
  T^.NoCaptures := (Length(T^.Children) = 0) and (T^.FieldInit = nil) and (T^.ArgsReg < 0) and not T^.TopLevel;
  if T^.NoCaptures then
    for I := 0 to T^.CodeLen - 1 do
      if InsOp(T^.Code[I]) in [opEvalHint, opMakeGLex, opMakeModCell, opClosure] then
      begin
        T^.NoCaptures := False;
        Break;
      end;
end;

threadvar
  AllTemplates: array of PFuncTemplate;
  TemplateCount: Integer;

procedure RegisterTemplate(T: PFuncTemplate);
begin
  if TemplateCount = Length(AllTemplates) then SetLength(AllTemplates, TemplateCount * 2 + 256);
  AllTemplates[TemplateCount] := T;
  Inc(TemplateCount);
end;

procedure ForgetAddTransitions;
var I, J: Integer; T: PFuncTemplate;
begin
  for I := 0 to TemplateCount - 1 do
  begin
    T := AllTemplates[I];
    for J := 0 to High(T^.ICs) do
      if T^.ICs[J].NewShape <> nil then
      begin
        T^.ICs[J].Shape := nil;
        T^.ICs[J].NewShape := nil;
      end;
  end;
end;

procedure ForgetRetiredShapes;
var I, J, D: Integer; T: PFuncTemplate; IC: PPropIC; Stale: Boolean;
  function Gone(S: Pointer): Boolean; inline;
  begin
    Result := (S <> nil) and ShapeRetired(PShape(S));
  end;
  { only the holders up to Depth: an entry is built on the stack, and those past it are whatever was there }
  function EntryGone(const E: TICEntry): Boolean;
  var K: Integer;
  begin
    Result := Gone(E.Shape);
    for K := 0 to Min(E.Depth, IC_MAX_DEPTH) - 1 do Result := Result or Gone(E.Holder[K]);
  end;
begin
  for I := 0 to TemplateCount - 1 do
  begin
    T := AllTemplates[I];
    if Gone(T^.CtorFnShape) then T^.CtorFnShape := nil;
    if Gone(T^.CtorRootShape) then T^.CtorRootShape := nil;
    if Gone(T^.FIShape) then begin T^.FIShape := nil; T^.FISlot := -1; end;
    if Gone(T^.NoHIShape) then T^.NoHIShape := nil;
    for J := 0 to High(T^.ICs) do
    begin
      IC := @T^.ICs[J];
      Stale := Gone(IC^.Shape) or Gone(IC^.NewShape);
      for D := 0 to Min(IC^.Depth, Length(IC^.Holder)) - 1 do Stale := Stale or Gone(IC^.Holder[D]);
      if Stale then
      begin
        IC^.Shape := nil; IC^.NewShape := nil;
        FillChar(IC^.Holder, SizeOf(IC^.Holder), 0);
        IC^.Depth := 0; IC^.Slot := -1;
      end;
      if IC^.PolyCount = 0 then Continue;
      Stale := EntryGone(IC^.Poly0);
      for D := 1 to IC^.PolyCount - 1 do Stale := Stale or EntryGone(IC^.More^[D]);
      if Stale then
      begin
        FillChar(IC^.Poly0, SizeOf(TICEntry), 0);
        if IC^.More <> nil then FillChar(IC^.More^, SizeOf(TICEntryMore), 0);
        IC^.PolyCount := 0; IC^.PolyNext := 0;
      end;
    end;
  end;
end;

procedure FreeAllTemplates;
var I, J: Integer; T: PFuncTemplate;
begin
  for I := 0 to TemplateCount - 1 do
  begin
    T := AllTemplates[I];
    for J := 0 to High(T^.ICs) do if T^.ICs[J].More <> nil then FreeMem(T^.ICs[J].More);
    if T^.OptFb <> nil then FreeMem(T^.OptFb);
    if (T^.Lazy <> nil) and Assigned(FreeLazyStubHook) then FreeLazyStubHook(T^.Lazy);
    if (T^.JitTable <> nil) or (T^.Opt <> nil) then
      if Assigned(FreeJitStateHook) then FreeJitStateHook(T);
    Dispose(T);
  end;
  AllTemplates := nil;
  TemplateCount := 0;
end;
{ the managed threadvars of this unit, released when the thread ends (Goccia.ThreadCleanupRegistry): FPC does not
  finalize a threadvar, and every engine thread leaves its strings, arrays and objects behind otherwise }
procedure GocciaInstructionsThreadvarCleanup;
begin
  AllTemplates := nil;
end;

initialization
  RegisterThreadvarCleanup(@GocciaInstructionsThreadvarCleanup);
  ProtoChangeHook := @ForgetAddTransitions;
end.
