unit G2.Bytecode;
{ Instruction format: 64 bits = op (8) | A (16) | B (16) | C (24, signed where an offset). }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}

interface

uses
  G2.Core, Goccia.SourceSpan;

type
  TOp = (
    opNop, opMov, opLoadK, opLoadI, opLoadUndef, opLoadNull, opLoadTrue, opLoadFalse, opLoadHole,
    opGetUpv, opSetUpv, opClose,
    opGetGlobal, opSetGlobal, opTypeofGlobal,
    opGetProp, opSetProp, opGetElem, opSetElem, opDelProp, opDelElem, opIn, opInstanceof,
    opGetSuper, opSetSuper, opLoadHome, opLoadNewTarget,
    opAdd, opSub, opMul, opDiv, opMod, opPow,
    opBitAnd, opBitOr, opBitXor, opShl, opShr, opUShr,
    opAddI,                  { R[A] = R[B] + C (immediate) }
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
                               (non-strict code at a Script's top level), 0 they stay its own (until G2 has
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
    Slot: Integer;
    Holder: array[0..3] of PShape;
    Vers: array[0..3] of Cardinal;   { those dictionaries' versions when the entry was made }
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

  { a function's source text (Function.prototype.toString), as the front end gives it }
  TG2SourceText = record
    Text: string;
    function Get: string; inline;
    procedure SetText(const AText: string); inline;
    function IsEmpty: Boolean; inline;
  end;

  TFuncTemplate = record
    Name: PStr;
    Code: array of QWord;
    CodeLen: Integer;
    EntryPC: Integer;        { first instruction a call runs: the prologue's unused placeholders are skipped }
    Consts: array of TValue;
    Children: array of PFuncTemplate;
    Upvals: array of TUpvalDesc;
    ICs: array of TPropIC;
    Lines: array of Integer;
    NumParams: Integer;
    MaxRegs: Integer;
    RestReg: Integer;       { -1 none }
    ArgsReg: Integer;       { -1 none }
    HomeReg: Integer;       { register holding the home object, -1 none }
    NewTargetReg: Integer;
    IsArrow, IsClassCtor, IsDerived, IsMethod, IsGenerator, IsAsync, IsStrict: Boolean;
    SimpleParams: Boolean;  { no rest, default or pattern parameter: a sloppy one gets a mapped arguments object }
    ExpectedProps: Integer;
    FieldInit: PFuncTemplate;   { class field initializer, run on construction }
    SourceName: UnicodeString;
    SourceRange: TG2SourceText;   { the function's source text, for Function.prototype.toString }
    Length: Integer;          { function.length }
    { the top level of a Script (GdiKind 1) or of non-strict eval code (2): its global declarations }
    GdiKind: Byte;
    GdiLex: array of PStr;        { let, const and class names, in register order of GdiLexRegs }
    GdiLexConst: array of Boolean;
    GdiVars: array of PStr;       { var names (functions excluded) }
    GdiFuncs: array of PStr;      { function declaration names, last declaration of a name only }
    GdiLexBase: Integer;          { first global lexical binding index, set when the declarations run }
    AsyncReg: Integer;            { an async function's register holding its promise, -1 otherwise }
    ModuleId: Integer;            { the Source Text Module Record (G2.Modules) whose code this is, 0 none }
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
procedure FreeAllTemplates;

implementation

function TG2SourceText.Get: string;
begin
  Result := Text;
end;

procedure TG2SourceText.SetText(const AText: string);
begin
  Text := AText;
end;

function TG2SourceText.IsEmpty: Boolean;
begin
  Result := Text = '';
end;

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


threadvar
  AllTemplates: array of PFuncTemplate;
  TemplateCount: Integer;

procedure RegisterTemplate(T: PFuncTemplate);
begin
  if TemplateCount = Length(AllTemplates) then SetLength(AllTemplates, TemplateCount * 2 + 256);
  AllTemplates[TemplateCount] := T;
  Inc(TemplateCount);
end;

procedure FreeAllTemplates;
var I, J: Integer; T: PFuncTemplate;
begin
  for I := 0 to TemplateCount - 1 do
  begin
    T := AllTemplates[I];
    for J := 0 to High(T^.ICs) do if T^.ICs[J].More <> nil then FreeMem(T^.ICs[J].More);
    Dispose(T);
  end;
  AllTemplates := nil;
  TemplateCount := 0;
end;
end.
