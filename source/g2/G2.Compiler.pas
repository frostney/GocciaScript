unit G2.Compiler;
{ Goccia AST -> G2 register bytecode. Lua-style: locals in registers, captured locals reached by closures
  through upvalues that are closed when their scope ends. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}

interface

uses
  SysUtils, Classes, Math, Generics.Collections,
  Goccia.AST.Node, Goccia.AST.Expressions, Goccia.AST.Statements, Goccia.Token,
  Goccia.Values.Primitives, Goccia.Values.BigIntValue,
  Goccia.ClassMask, Goccia.Modules.Phases, OrderedStringMap, G2.Core, G2.Bytecode, G2.BigIntV;

type
  ECompileError = class(Exception);
  { an early error the compiler finds (ParseAndCompile reports it as a SyntaxError, like the parser's) }
  EEarlySyntaxError = class(ECompileError);

{ Goal: 0 an isolated function body (preludes, the Function constructor, the embedding host), 1 a Script
  (16.1.7 GlobalDeclarationInstantiation), 2 eval code run in the global environment (19.2.1.3; strict eval
  code is goal 0) }
function CompileProgram(Prog: TGocciaProgram; const SourceName: UnicodeString; ReturnLast: Boolean = False;
  Goal: Integer = 0): PFuncTemplate;

threadvar
  { the eval code starts with a "use strict" directive the caller inserted to make it strict: not its value }
  EvalStrictPrefix: Boolean;

const
  ModNamespace = '*namespace*';   { an import or indirect export of a whole module namespace }
  ModDeferNamespace = '*defernamespace*';   { import defer * as ns: the deferred namespace (Deferring Module Evaluation) }

type
  TModEntry = record
    ExportName, LocalName, Request, ImportName: UnicodeString;
    Slot: Integer;
  end;
  { 16.2.1.6 the parts of a Source Text Module Record the compiler finds (goal 3): the cell table holds one slot
    per import (0 .. imports - 1) and one per exported local binding }
  TModuleInfo = class
  public
    Requests: array of UnicodeString;
    Imports: array of TModEntry;
    LocalExports: array of TModEntry;
    IndirectExports: array of TModEntry;
    StarExports: array of UnicodeString;
    { the module requests in source order, one entry per phase: an eager one evaluates its module, a deferred
      one only its asynchronous dependencies (the import defer proposal's [[RequestedModules]] order) }
    EvalOrder: array of record Req: UnicodeString; Eager: Boolean; end;
    HasTLA: Boolean;
    SlotCount: Integer;
    procedure AddRequest(const R: UnicodeString);
    procedure AddEager(const R: UnicodeString);
    procedure AddDeferred(const R: UnicodeString);
    function IsEager(const R: UnicodeString): Boolean;
  end;

{ a ModuleRequest Record as a string: the specifier, then #1 and its type attribute when it has one }
function ModuleRequestKey(const Spec, AType: UnicodeString): UnicodeString;

threadvar
  { goal 4 (a direct eval in function code): the caller's bindings, as G2.Compiler's direct-eval sites describe
    them (BuildEvalDesc): entries name #2 kind #2 index #2 flags, each ended by #1 }
  EvalOuterDesc: UnicodeString;
  { the next tagged template's site number (13.2.8.4: its template object is cached per site and realm) }
  TemplateSiteNext: Integer;
  { goal 3: the module info of the last CompileProgram (the caller owns it), and the module id its templates get }
  LastModuleInfo: TModuleInfo;
  CompilingModuleId: Integer;

implementation

uses
  TypInfo, G2.VM;

{ G2_DUMP=<name part>: the bytecode of each compiled function whose name contains it, on stderr (a debugging aid) }
procedure DumpTemplates(T: PFuncTemplate; const Pat: UnicodeString);
var I: Integer; Q: QWord; Nm: UnicodeString;
begin
  if T = nil then Exit;
  Nm := '';
  if T^.Name <> nil then Nm := StrToUnicode(T^.Name);
  if (Pat = '*') or (Pos(Pat, Nm) > 0) then
  begin
    WriteLn(StdErr, '== ', Nm, ' (params ', T^.NumParams, ', regs ', T^.MaxRegs, ')');
    for I := 0 to T^.CodeLen - 1 do
    begin
      Q := T^.Code[I];
      WriteLn(StdErr, Format('%4d  %-16s A=%d B=%d C=%d', [I, GetEnumName(TypeInfo(TOp), Q and $FF),
        (Q shr 8) and $FFFF, (Q shr 24) and $FFFF, SarInt64(Int64(Q), 40)]));
    end;
  end;
  for I := 0 to High(T^.Children) do DumpTemplates(T^.Children[I], Pat);
end;

procedure PatternNames(P: TGocciaDestructuringPattern; L: TStringList); forward;

{ the threadvar through a noinline accessor: FPC 3.3.1 b3 miscompiled its plain load in CompileFunction (an
  absolute relocation; t/threads/NOTES.md) }
function CompilingModuleIdOf: Integer; noinline;
begin
  Result := CompilingModuleId;
end;
procedure LexNamesOf(L: TList<TGocciaASTNode>; Out_: TStringList); forward;
function NewNameList: TStringList; forward;
procedure CheckStatementBody(S: TGocciaASTNode; IfClause, Strict: Boolean); forward;

{ Dispatch on an AST node's class. A chain of `is` tests costs a class-parent
  walk per test, and a node that matches late fails twenty of them. The cache
  maps an exact class to the index of the first entry it inherits from, which
  is what the chain computed, so subclasses keep their old branch. }
type
  TKindCache = record
    Keys: array[0..511] of TClass;
    Vals: array[0..511] of SmallInt;
    Count: Integer;
  end;

function KindOf(var C: TKindCache; const Classes: array of TClass; Obj: TObject): Integer;
var K: TClass; H, I: Integer;
begin
  K := Obj.ClassType;
  H := Integer((PtrUInt(K) shr 4) and 511);
  while C.Keys[H] <> nil do
  begin
    if C.Keys[H] = K then Exit(C.Vals[H]);
    H := (H + 1) and 511;
  end;
  Result := Length(Classes);
  for I := 0 to High(Classes) do
    if K.InheritsFrom(Classes[I]) then begin Result := I; Break; end;
  if C.Count < 256 then begin C.Keys[H] := K; C.Vals[H] := Result; Inc(C.Count); end;
end;

threadvar
  ExprKinds: TKindCache;
const
  ExprClasses: array[0..23] of TClass = (TGocciaLiteralExpression, TGocciaIdentifierExpression,
    TGocciaThisExpression, TGocciaBinaryExpression, TGocciaUnaryExpression, TGocciaCallExpression,
    TGocciaNewExpression, TGocciaMemberExpression, TGocciaObjectExpression, TGocciaArrayExpression,
    TGocciaFunctionExpression, TGocciaArrowFunctionExpression, TGocciaObjectMethodDefinition,
    TGocciaClassExpression, TGocciaTargetedAssignmentExpression, TGocciaDestructuringAssignmentExpression,
    TGocciaIncrementExpression, TGocciaTemplateLiteralExpression, TGocciaTemplateWithInterpolationExpression,
    TGocciaTaggedTemplateExpression, TGocciaConditionalExpression, TGocciaSequenceExpression,
    TGocciaNewTargetExpression, TGocciaRegexLiteralExpression);
  { TGocciaYieldExpression and TGocciaAwaitExpression come after these: kind 24 tests them as before }

const
  NO_REG = $FFFF;
  { flags of a direct eval's caller binding (BuildEvalDesc) }
  efConst = 1; efLexical = 2; efSameFunc = 4; efVarLevel = 8; efParamEnv = 16; efFnName = 32; efInitialized = 64;
  RESERVED_HOME = 1;   { offsets after the parameters for non-arrow functions }

type
  TBinding = record
    Name: UnicodeString;
    Reg: Integer;
    IsConst: Boolean;
    { let, const and class (9.1.1.1): uninitialized from the block's entry until the declaration runs.
      Initialized: the declaration has been compiled, so code compiled after it (closures included) runs after
      it; code before it gets a TDZ check, and HoleNop (the entry placeholder) then stores the hole. }
    IsLexical, Initialized: Boolean;
    HoleNop: Integer;
    IsFnName: Boolean;   { a named function expression's own name: immutable, but assigning it is a TypeError only in strict code }
  end;

  TScope = class
    Bindings: array of TBinding;
    Count: Integer;
    BaseReg: Integer;
    Captured: Boolean;
    IsFunctionTop: Boolean;
    IsSwitch: Boolean;   { a switch's case block: its declarations may be skipped, so reads are always checked }
    { past 8 bindings, Find uses a hash index: slot -> binding index + 1 (bundles have scopes of thousands) }
    Index: array of Integer;
    function Find(const N: UnicodeString): Integer;
    procedure Add(const N: UnicodeString; Reg: Integer; IsConst: Boolean);
  end;

  TJumpList = array of Integer;

  TLoopInfo = record
    Labels: TStringList;     { labels that name this statement }
    BreakJumps: TJumpList;
    ContJumps: TJumpList;
    IsLoop: Boolean;
    ScopeDepth: Integer;     { scope count when the loop/switch began }
    TryDepth: Integer;
  end;

  TFinallyInfo = record
    Block: TGocciaBlockStatement;   { finally body, nil for a catch-only try }
    ScopeDepth: Integer;
    IterReg: Integer;               { >= 0: a for-of loop's iterator, closed when a jump leaves the loop }
    UsingCap: Integer;              { >= 0: a using block's dispose capability, disposed when a jump leaves it }
    UsingAsync: Boolean;            { the block has an await using: its disposal awaits }
  end;

  TFuncState = class
    Parent: TFuncState;
    Tpl: PFuncTemplate;
    Scopes: array of TScope;
    ScopeCount: Integer;
    FreeReg: Integer;
    UpvalNames: array of UnicodeString;
    Loops: array of TLoopInfo;
    LoopCount: Integer;
    Tries: array of TFinallyInfo;
    TryCount: Integer;
    IsArrow: Boolean;
    NoArguments: Boolean;
    NoNewTarget: Boolean;   { goal 4: the caller has no non-arrow function around it (new.target a SyntaxError) }
    UsingCapReg: Integer;   { the innermost using block's dispose capability register, -1 none }
    IsStaticBlock: Boolean;   { ClassStaticBlockStatementList: await is reserved in it }   { a field initializer or static block: arguments is a SyntaxError in it and its arrows }
    HomeReg, NewTargetReg: Integer;   { reserved registers, -1 for arrows }
    HomeUsed, NewTargetUsed, ArgsUsed, CtorUsed: Boolean;
    CtorReg: Integer;   { a derived constructor's register for itself (super() in arrows), -1 otherwise }
    ArgsReg: Integer;
    PendingLabels: TStringList;
    CurLine: Integer;
    VarRegs: array of Integer;   { per register: bindings of the open scopes that use it }
    IsEvalOuter: Boolean;   { goal 4: the stand-in for a direct eval's caller, its bindings the caller's }
    CplP1: Integer;         { 1 + the register of the script's completion value (eval code, ReturnLast), 0 none }
    CplSkip: Boolean;       { the next expression statement is EvalStrictPrefix's directive }
    ParamAwait: Boolean;    { compiling an arrow's parameters in a class static block: ArrowParameters[+Await] }
    ParamTDZ: TStringList;  { while parameter initializers compile: the parameters not yet initialized }
  end;

  TCompiler = class
    FS: TFuncState;
    SourceName: UnicodeString;
    DefaultSuperForward: Boolean;   { next CompileFunction is a default derived constructor }
    PendingStrict: Integer;         { -1 inherit, 0 sloppy, 1 strict, for the next CompileFunction }
    ObjLiteralMethod: Boolean;   { the next CompileFunction is an object literal's method or accessor (not strict by itself) }
    NoIdentCheck: Boolean;
    PendingNoArguments: Boolean;
    PendingStaticBlock: Boolean;   { the next CompileFunction is a field initializer or static block (15.7.1: no arguments) }   { a function expression's own name: checked in its own context instead }
    ReturnLast: Boolean;            { the script returns the value of its last expression statement }
    Goal: Integer;                  { see CompileProgram; the top level of goals 1 and 2 declares globals }
    IsEvalCode: Boolean;
    ModImportNames: TStringList;    { goal 3: local names of imports (slot = index) }
    ModPublish: TStringList;        { goal 3: exported local bindings (slot = imports + index) }
    ModDefault: TGocciaExportDefaultDeclaration;
    { 14.11 the with statements being compiled, innermost last: their object's hidden local, function, scope }
    WithNames: array of UnicodeString;
    WithFuncs: array of TFuncState;
    WithScopes: array of Integer;
    { 0 a with statement; 1 the variable environment a non-strict direct eval adds its vars to (19.2.1.3 step 16),
      seen from its function; 2 the same seen from eval code, holding the names its caller's bindings at depth
      above WithDepths don't }
    WithKinds, WithDepths: array of Integer;
    WithCount, WithSerial: Integer;
    NextSloppyEval: Boolean;        { the next CompileFunction compiles a function with a non-strict direct eval }
    { 13.3.9 an OptionalChain short-circuits as a whole: ChainLink is set while a chain node's object or callee is
      compiled (that node is a link of the same chain), and a link's ?. jumps join ChainJumps, which the chain's
      outermost node patches to give undefined }
    ChainLink: Boolean;
    ChainJumps: TJumpList;
    { the value of the assignment or update being compiled is unused (an expression statement or a for head's
      init or update): its copy into the destination register can go. Set only for such a top-level node, and
      cleared by CompileAssign / CompileIncrement on entry }
    TopDiscard: Boolean;
    EvalDepth: array of Integer;    { goal 4: per caller binding, how many functions out of the caller it is }
    EvalVarEnv: UnicodeString;      { goal 4: the caller's eval variable environment binding ('' none) }
    EvalEnvNames: TStringList;      { goal 4: the var names the eval code adds to that environment }
    WithIdxScratch: array[0..63] of Integer;            { eval code (direct or indirect): its top level allows no using }
    GlobalTop: Boolean;             { compiling that top level's own statements (not nested functions) }
    AnnexBFuncs: TDictionary<Pointer, Boolean>;   { B.3.2 declarations: copied to their var binding when evaluated }
    InParamInit: Boolean;           { compiling parameter initializers (a direct eval there sees the parameter scope) }
    EvalFlags: array of Integer;    { goal 4: per caller binding (its register in the stand-in), the ef* flags }
    EvalSkipLocal: TStringList;     { goal 4: the eval code's var names its caller already binds (no local of its own) }
    PendingInitStatic: Boolean;     { the next class initializer CompileFunction makes is the static one }
    DeclaringLexical: Boolean;      { HoistBlock's lexical declarations: DeclareInScope marks them }
    { emission }
    function Emit(I: QWord): Integer;
    function EmitJ(Op: TOp; A, B: Integer): Integer;
    procedure PatchTo(PC, Target: Integer);
    procedure PatchHere(PC: Integer);
    procedure PatchListHere(const L: TJumpList);
    function Here: Integer;
    function AddConst(V: TValue): Integer;
    function AtomConst(const S: UnicodeString): Integer;
    function NewIC(const Key: UnicodeString): Integer;
    { registers }
    function AllocReg: Integer;
    function AllocRegs(N: Integer): Integer;
    procedure FreeTo(R: Integer);
    { scopes }
    procedure PushScope(IsTop: Boolean = False);
    procedure PopScope;
    function DeclareLocal(const N: UnicodeString; IsConst: Boolean): Integer;
    function IsVarReg(Dest: Integer): Boolean;
    function MaySideEffect(E: TGocciaExpression): Boolean;
    function DeclareInScope(S: TScope; const N: UnicodeString; IsConst: Boolean): Integer;
    function ResolveLocal(F: TFuncState; const N: UnicodeString; out Reg: Integer): Boolean;
    function ResolveUpval(F: TFuncState; const N: UnicodeString): Integer;
    procedure EmitCloseFrom(Depth: Integer);
    { functions }
    function CompileFunction(const Name: UnicodeString; const Params: TGocciaParameterArray; Body: TGocciaASTNode;
      IsArrow, IsMethod, IsClassCtor, IsDerived, IsGenerator, IsAsync: Boolean; FieldInit: TGocciaClassDefinition = nil;
      BodyNodes: TList<TGocciaASTNode> = nil): PFuncTemplate;
    function ChildIndex(T: PFuncTemplate): Integer;
    procedure EmitClosure(Dest: Integer; T: PFuncTemplate);
    { statements }
    procedure Stmt(S: TGocciaASTNode);
    procedure FinallyStmt(B: TGocciaASTNode);
    procedure StmtLabeled(St: TGocciaStatement);
    procedure CheckIdent(const N: UnicodeString);
    function WithsFor(const N: UnicodeString; var Idx: array of Integer): Integer;
    procedure EmitWithChain(const N: UnicodeString; Dest, ThisReg: Integer; var Ends: TJumpList; ForCall: Boolean = False);
    procedure PushWithEntry(const Nm: UnicodeString; F: TFuncState; Scope, Kind, Depth: Integer);
    procedure CompileWith(S: TGocciaWithStatement);
    procedure LoadIdentStatic(const N: UnicodeString; Dest: Integer);
    procedure StoreIdentStatic(const N: UnicodeString; Src: Integer; IsInit: Boolean);
    procedure LoadIdentThis(const N: UnicodeString; Dest, ThisReg: Integer; ForCall: Boolean = False);
    function ModImportSlot(const N: UnicodeString): Integer;
    procedure EmitModGet(Slot: Integer; Dest: Integer);
    procedure EmitModulePrologue;
    procedure EmitUsingDispose(Cap: Integer; Async: Boolean; ExcReg: Integer);
    procedure CompileUsingList(L: TList<TGocciaASTNode>; Stmts: TObjectList<TGocciaStatement>; Body: Boolean = False);
    procedure CompileUsingDecl(D: TGocciaUsingDeclaration);
    procedure CompileForOfUsingBody(S: TGocciaForOfStatement; V: Integer);
    procedure AddUsingResource(R: Integer; Async: Boolean);
    procedure BeginUsingRegion(Async: Boolean; out Cap, ExcReg, OuterTry, SavedCap: Integer);
    procedure EndUsingRegion(Cap, ExcReg, OuterTry, SavedCap: Integer; Async: Boolean);
    function NewICKey(Key: PStr): Integer;
    procedure ExprToNamed(E: TGocciaExpression; Dest: Integer; const Name: UnicodeString);
    procedure StmtCore(S: TGocciaASTNode);
    procedure StmtNodes(L: TList<TGocciaASTNode>);
    procedure StmtList(L: TObjectList<TGocciaASTNode>); overload;
    procedure StmtList(L: TObjectList<TGocciaStatement>); overload;
    procedure HoistBlock(L: TList<TGocciaASTNode>);
    procedure DeclareGlobals(T: PFuncTemplate; Nodes: TList<TGocciaASTNode>; VarNames: TStringList);
    procedure HoistFunctionsIn(Nodes: array of TGocciaASTNode);
    procedure CollectVarNames(N: TGocciaASTNode; Names: TStringList); noinline;   { inlined, its threadvar cache trips the FPC TLS miscompile }
    { B.3.2: the block-level function declarations of a non-strict body that also get a var binding (their names
      into Names, the declarations into AnnexBFuncs); TopLex the names a var could not share at the top }
    procedure CollectAnnexB(Nodes: TList<TGocciaASTNode>; TopLex, Names: TStringList);
    procedure AnnexBWalk(N: TGocciaASTNode; Encl, Names: TStringList);
    procedure AnnexBWalkList(L: TList<TGocciaASTNode>; Encl, Names: TStringList);
    procedure EmitAnnexBCopy(FD: TGocciaFunctionDeclaration);
    procedure EmitSuperSet(const Key: UnicodeString; KeyExpr, Value: TGocciaExpression; Dest: Integer);
    function LastChildUsesHome: Boolean;
    procedure CompileDeleteIdent(const N: UnicodeString; Dest: Integer);
    function BuildEvalDesc: UnicodeString;
    function EmitConstStore(const N: UnicodeString): Boolean;
    procedure EvalDeclarations(Nodes: TList<TGocciaASTNode>; Names: TStringList; Strict: Boolean);
    function EvalOuterFlags(const N: UnicodeString): Integer;
    procedure CollectVarNamesOf(N: TGocciaASTNode; Names: TStringList);
    procedure CompileBigIntLiteral(V: TGocciaBigIntValue; Dest: Integer);
    procedure CompileVarDecl(D: TGocciaVariableDeclaration);
    procedure CompileIf(S: TGocciaIfStatement);
    procedure CompileFor(S: TGocciaForStatement);
    procedure CompileWhile(S: TGocciaWhileStatement);
    procedure CompileDoWhile(S: TGocciaDoWhileStatement);
    procedure CompileForOf(S: TGocciaForOfStatement);
    procedure CompileForAwaitOf(S: TGocciaForOfStatement);
    procedure EmitAsyncIterClose(It, Tmp: Integer);
    procedure CompileForIn(S: TGocciaForInStatement);
    procedure ForHeadExpr(E: TGocciaExpression; Dest: Integer; IsVar: Boolean; const BName: UnicodeString;
      Pat: TGocciaDestructuringPattern);
    procedure CompileSwitch(S: TGocciaSwitchStatement);
    procedure CompileTry(S: TGocciaTryStatement);
    procedure CompileReturn(S: TGocciaReturnStatement);
    procedure CompileBreakContinue(IsBreak: Boolean; const Lbl: UnicodeString);
    procedure CompileClass(Def: TGocciaClassDefinition; Dest: Integer; const NameHint: UnicodeString = '');
    procedure CompileFieldValue(V: TGocciaExpression; Dest: Integer; const Name: UnicodeString);
    procedure EmitPrivateMethods(Def: TGocciaClassDefinition; Obj: Integer; Static: Boolean);
    procedure EmitClassInit(Def: TGocciaClassDefinition; Static: Boolean);
    procedure CompilePrivateGet(E: TGocciaPrivateMemberExpression; Dest: Integer; ThisDest: Integer = -1);
    function InDerivedCtor: Boolean;
    procedure LoadThis(Dest: Integer);
    procedure BeginLoop(IsLoop: Boolean);
    procedure EndLoop(ContTarget, BreakTarget: Integer);
    procedure EmitFinallyUnwind(ToTryDepth: Integer);
    { expressions }
    procedure ExprTo(E: TGocciaExpression; Dest: Integer);
    procedure CompileConditional(C: TGocciaConditionalExpression; Dest: Integer);
    procedure RaiseUnsupported(const AWhat: UnicodeString; N: TObject);
    function ExprAny(E: TGocciaExpression): Integer;
    function IsDiscardable(E: TGocciaExpression): Boolean;
    function PureRead(E: TGocciaExpression): Boolean;
    procedure BranchFalse(E: TGocciaExpression; var L: TJumpList);
    procedure BranchTrue(E: TGocciaExpression; var L: TJumpList);
    procedure LoadIdent(const N: UnicodeString; Dest: Integer);
    { the binding N resolves to (this function's scopes, then the enclosing functions') }
    function FindBinding(const N: UnicodeString; out F: TFuncState; out S: TScope; out J: Integer): Boolean;
    { N is a lexical binding whose declaration has not been compiled yet: a read or write here needs the TDZ
      check (its scope's entry then stores the hole) }
    function InTDZ(const N: UnicodeString): Boolean;
    function BindingIsConst(const N: UnicodeString): Boolean;
    procedure MarkInitialized(const N: UnicodeString);
    procedure StoreIdent(const N: UnicodeString; Src: Integer; IsInit: Boolean = False);
    procedure CompileBinary(E: TGocciaBinaryExpression; Dest: Integer);
    procedure CompileUnary(E: TGocciaUnaryExpression; Dest: Integer);
    function ArgumentsBound: Boolean;
    procedure CompileCall(E: TGocciaCallExpression; Dest: Integer);
    procedure CompileNew(E: TGocciaNewExpression; Dest: Integer);
    function CompileArgs(Args: TObjectList<TGocciaExpression>; Base: Integer; out Spread: Boolean): Integer;
    procedure CompileMember(E: TGocciaMemberExpression; Dest: Integer);
    function ChainBegin(var Saved: TJumpList): Boolean;
    procedure ChainEnd(IsLink: Boolean; var Saved: TJumpList; Dest: Integer);
    procedure ChainObject(E: TGocciaExpression);
    procedure ChainAdd(J: Integer);
    procedure CompileObject(E: TGocciaObjectExpression; Dest: Integer);
    procedure CompileArray(E: TGocciaArrayExpression; Dest: Integer);
    procedure CompileAssign(E: TGocciaExpression; Dest: Integer);
    procedure CompileIncrement(E: TGocciaIncrementExpression; Dest: Integer);
    procedure CompileTemplate(E: TGocciaTemplateWithInterpolationExpression; Dest: Integer);
    procedure CompileTagged(E: TGocciaTaggedTemplateExpression; Dest: Integer);
    procedure CompileLiteral(V: TGocciaValue; Dest: Integer);
    procedure CompileFuncExpr(E: TGocciaExpression; Dest: Integer; const NameHint: UnicodeString = '');
    procedure LoadNumber(D: Double; Dest: Integer);
    procedure LoadString(const S: UnicodeString; Dest: Integer);
    function BinOpFor(T: TGocciaTokenType): TOp;
    procedure CompileSuperMember(const Key: UnicodeString; KeyExpr: TGocciaExpression; Dest: Integer);
    procedure EvalSuperRef(const Key: UnicodeString; KeyExpr: TGocciaExpression; var Base, KeyOp: Integer);
    procedure StoreIdentRef(const N: UnicodeString; Env, Src: Integer);
    procedure ResolveWithEnv(const N: UnicodeString; Env: Integer);
    function HomeRegister: Integer;
    function NewTargetRegister: Integer;
    function NewTargetOutside: Boolean;
    function NewTargetFromArrow: Boolean;
    { patterns }
    procedure BindPattern(P: TGocciaDestructuringPattern; Src: Integer; Declare: Boolean; IsConst: Boolean);
    procedure AssignToTarget(T: TGocciaExpression; Src: Integer);
    procedure DeclarePatternNames(P: TGocciaDestructuringPattern; IsConst: Boolean);
  end;

function NameHash(const N: UnicodeString): Cardinal; inline;
var I: Integer;
begin
  Result := 2166136261;
  for I := 1 to Length(N) do Result := (Result xor Ord(N[I])) * 16777619;
end;

function TScope.Find(const N: UnicodeString): Integer;
var I, Mask, H: Integer;
begin
  if Length(Index) = 0 then
  begin
    for I := Count - 1 downto 0 do
      if Bindings[I].Name = N then Exit(I);
    Exit(-1);
  end;
  Mask := High(Index);
  H := Integer(NameHash(N) and Cardinal(Mask));
  while Index[H] <> 0 do
  begin
    if Bindings[Index[H] - 1].Name = N then Exit(Index[H] - 1);
    H := (H + 1) and Mask;
  end;
  Result := -1;
end;

procedure TScope.Add(const N: UnicodeString; Reg: Integer; IsConst: Boolean);
var I, Mask, H, Cap: Integer;
begin
  if Count >= Length(Bindings) then SetLength(Bindings, Count * 2 + 4);
  Bindings[Count].Name := N;
  Bindings[Count].Reg := Reg;
  Bindings[Count].IsConst := IsConst;
  Bindings[Count].IsLexical := False;
  Bindings[Count].Initialized := True;
  Bindings[Count].HoleNop := -1;
  Inc(Count);
  if Count <= 8 then Exit;
  if Count * 2 > Length(Index) then
  begin
    { (re)build at twice the size: names are unique within a scope }
    Cap := 32;
    while Cap < Count * 4 do Cap := Cap * 2;
    Index := nil;
    SetLength(Index, Cap);
    Mask := Cap - 1;
    for I := 0 to Count - 1 do
    begin
      H := Integer(NameHash(Bindings[I].Name) and Cardinal(Mask));
      while Index[H] <> 0 do H := (H + 1) and Mask;
      Index[H] := I + 1;
    end;
    Exit;
  end;
  Mask := High(Index);
  H := Integer(NameHash(N) and Cardinal(Mask));
  while Index[H] <> 0 do H := (H + 1) and Mask;
  Index[H] := Count;
end;

{ ---------------- emission ---------------- }

function TCompiler.Emit(I: QWord): Integer;
var T: PFuncTemplate; Live: Integer;
begin
  T := FS.Tpl;
  { A suspending instruction carries in C how many registers are live across it: those the allocator holds
    (it frees registers last in, first out, so none above FreeReg is read again before it is written) and those
    the instruction itself uses. A suspension saves only these. }
  case TOp(I and $FF) of
    opYield, opAwait, opYieldStar, opYieldStarAsync, opGenStart:
      if (I shr 40) = 0 then
      begin
        Live := Max(FS.FreeReg, Integer((I shr 8) and $FFFF) + 1);
        case TOp(I and $FF) of
          opYieldStar: Live := Max(Live, Integer((I shr 24) and $FFFF) + 4);
          opYieldStarAsync: Live := Max(Live, Integer((I shr 24) and $FFFF) + 5);
        else Live := Max(Live, Integer((I shr 24) and $FFFF) + 1);
        end;
        I := I or (QWord(Live) shl 40);
      end;
  end;
  if T^.CodeLen >= Length(T^.Code) then
  begin
    SetLength(T^.Code, T^.CodeLen * 2 + 64);
    SetLength(T^.Lines, Length(T^.Code));
  end;
  T^.Code[T^.CodeLen] := I;
  T^.Lines[T^.CodeLen] := FS.CurLine;
  Result := T^.CodeLen;
  Inc(T^.CodeLen);
end;

function TCompiler.EmitJ(Op: TOp; A, B: Integer): Integer;
begin
  Result := Emit(Ins(Op, A, B, 0));
end;

procedure TCompiler.PatchTo(PC, Target: Integer);
var I: QWord;
begin
  I := FS.Tpl^.Code[PC];
  FS.Tpl^.Code[PC] := (I and $000000FFFFFFFFFF) or (QWord((Target - (PC + 1)) and $FFFFFF) shl 40);
end;

procedure TCompiler.PatchHere(PC: Integer);
begin
  PatchTo(PC, FS.Tpl^.CodeLen);
end;

procedure TCompiler.PatchListHere(const L: TJumpList);
var I: Integer;
begin
  for I := 0 to High(L) do PatchHere(L[I]);
end;

procedure AddJump(var L: TJumpList; PC: Integer);
begin
  SetLength(L, Length(L) + 1);
  L[High(L)] := PC;
end;

function TCompiler.Here: Integer;
begin
  Result := FS.Tpl^.CodeLen;
end;

function TCompiler.AddConst(V: TValue): Integer;
var T: PFuncTemplate; I: Integer;
begin
  T := FS.Tpl;
  for I := 0 to High(T^.Consts) do
    if T^.Consts[I] = V then Exit(I);
  SetLength(T^.Consts, Length(T^.Consts) + 1);
  T^.Consts[High(T^.Consts)] := V;
  Result := High(T^.Consts);
end;

function TCompiler.AtomConst(const S: UnicodeString): Integer;
begin
  Result := AddConst(MkPtr(AtomOf(S)));
end;

function TCompiler.NewIC(const Key: UnicodeString): Integer;
var T: PFuncTemplate;
begin
  T := FS.Tpl;
  SetLength(T^.ICs, Length(T^.ICs) + 1);
  Result := High(T^.ICs);
  FillChar(T^.ICs[Result], SizeOf(TPropIC), 0);
  T^.ICs[Result].Key := AtomOf(Key);
  T^.ICs[Result].Slot := -1;
end;

{ ---------------- registers and scopes ---------------- }

function TCompiler.AllocReg: Integer;
begin
  Result := FS.FreeReg;
  Inc(FS.FreeReg);
  if FS.FreeReg > FS.Tpl^.MaxRegs then FS.Tpl^.MaxRegs := FS.FreeReg;
  if FS.FreeReg >= NO_REG then raise ECompileError.Create('function needs too many registers');
end;

function TCompiler.AllocRegs(N: Integer): Integer;
var I: Integer;
begin
  Result := FS.FreeReg;
  for I := 1 to N do AllocReg;
end;

procedure TCompiler.FreeTo(R: Integer);
begin
  FS.FreeReg := R;
end;

procedure TCompiler.PushScope(IsTop: Boolean);
var S: TScope;
begin
  S := TScope.Create;
  S.BaseReg := FS.FreeReg;
  S.IsFunctionTop := IsTop;
  if FS.ScopeCount >= Length(FS.Scopes) then SetLength(FS.Scopes, FS.ScopeCount * 2 + 8);
  FS.Scopes[FS.ScopeCount] := S;
  Inc(FS.ScopeCount);
end;

procedure TCompiler.PopScope;
var S: TScope; I: Integer;
begin
  Dec(FS.ScopeCount);
  S := FS.Scopes[FS.ScopeCount];
  for I := 0 to S.Count - 1 do
    if S.Bindings[I].Reg < Length(FS.VarRegs) then Dec(FS.VarRegs[S.Bindings[I].Reg]);
  if S.Captured then Emit(Ins(opClose, S.BaseReg, 0, 0));
  FS.FreeReg := S.BaseReg;
  S.Free;
end;

{ 13.1.1 static semantics: await is reserved in an async function's body and parameters (arrows inherit it),
  yield in a generator's (arrows do not inherit it) and, with the other strict-mode reserved words, in strict
  code; for identifier references, binding identifiers and labels alike (escaped spellings reach here decoded) }
{ 12.7.2 ReservedWord (the parser lets an escaped one through as an identifier: 13.1.1 makes it a SyntaxError) }
function IsReservedWord(const N: UnicodeString): Boolean;
begin
  Result := False;
  case Length(N) of
    2: Result := (N = 'do') or (N = 'if') or (N = 'in');
    3: Result := (N = 'for') or (N = 'new') or (N = 'try') or (N = 'var');
    4: Result := (N = 'case') or (N = 'else') or (N = 'enum') or (N = 'null') or (N = 'this') or (N = 'true') or
                 (N = 'void') or (N = 'with');
    5: Result := (N = 'break') or (N = 'catch') or (N = 'class') or (N = 'const') or (N = 'false') or
                 (N = 'super') or (N = 'throw') or (N = 'while');
    6: Result := (N = 'delete') or (N = 'export') or (N = 'import') or (N = 'return') or (N = 'switch') or
                 (N = 'typeof');
    7: Result := (N = 'default') or (N = 'extends') or (N = 'finally');
    8: Result := (N = 'continue') or (N = 'function');   { debugger: the parser makes debugger; an identifier statement }
    10: Result := N = 'instanceof';
  end;
end;

procedure TCompiler.CheckIdent(const N: UnicodeString);
var F: TFuncState;
begin
  if (N = '') or (N[1] = '%') or NoIdentCheck then Exit;
  if (N[1] >= 'b') and (N[1] <= 'w') and IsReservedWord(N) then
    raise EEarlySyntaxError.Create('Keyword must not contain escaped characters');
  if N = 'arguments' then
  begin
    F := FS;
    while (F <> nil) and F.IsArrow and not F.NoArguments do F := F.Parent;
    if (F <> nil) and F.NoArguments then raise EEarlySyntaxError.Create('''arguments'' is not allowed in class field initializer or static initialization block');
  end;
  if N = 'await' then
  begin
    { an arrow's concise body is [~Await]: only the async function's (or async arrow's) own code }
    F := FS;
    if F.Tpl^.IsAsync or F.IsStaticBlock or F.ParamAwait then
      raise EEarlySyntaxError.Create('Unexpected reserved word ''await''');
  end
  else if (N = 'yield') and (FS.Tpl^.IsStrict or (FS.Tpl^.IsGenerator and not FS.Tpl^.IsArrow)) then
    raise EEarlySyntaxError.Create('Unexpected reserved word ''yield''')
  else if FS.Tpl^.IsStrict and ((N = 'implements') or (N = 'interface') or (N = 'let') or (N = 'package') or
     (N = 'private') or (N = 'protected') or (N = 'public') or (N = 'static')) then
    raise EEarlySyntaxError.Create('Unexpected strict mode reserved word ''' + N + '''');
end;

function TCompiler.DeclareInScope(S: TScope; const N: UnicodeString; IsConst: Boolean): Integer;
var I: Integer;
begin
  CheckIdent(N);
  I := S.Find(N);
  if I >= 0 then Exit(S.Bindings[I].Reg);
  Result := AllocReg;
  S.Add(N, Result, IsConst);
  if DeclaringLexical then
  begin
    S.Bindings[S.Count - 1].IsLexical := True;
    S.Bindings[S.Count - 1].Initialized := False;
    S.Bindings[S.Count - 1].HoleNop := Emit(Ins(opNop, 0, 0, 0));
  end;
  if Result >= Length(FS.VarRegs) then SetLength(FS.VarRegs, Result * 2 + 16);
  Inc(FS.VarRegs[Result]);
end;

function TCompiler.DeclareLocal(const N: UnicodeString; IsConst: Boolean): Integer;
begin
  Result := DeclareInScope(FS.Scopes[FS.ScopeCount - 1], N, IsConst);
end;

function TCompiler.ResolveLocal(F: TFuncState; const N: UnicodeString; out Reg: Integer): Boolean;
var I, J: Integer;
begin
  for I := F.ScopeCount - 1 downto 0 do
  begin
    J := F.Scopes[I].Find(N);
    if J >= 0 then
    begin
      Reg := F.Scopes[I].Bindings[J].Reg;
      Exit(True);
    end;
  end;
  Result := False;
end;

function MarkCaptured(F: TFuncState; Reg: Integer): Boolean;
var I: Integer;
begin
  for I := F.ScopeCount - 1 downto 0 do
    if Reg >= F.Scopes[I].BaseReg then
    begin
      F.Scopes[I].Captured := True;
      Exit(True);
    end;
  Result := False;
end;

{ upvalue index of N in F (creating descriptors along the chain), -1 if N is global }
function TCompiler.ResolveUpval(F: TFuncState; const N: UnicodeString): Integer;
var I, Reg, PIdx: Integer; T: PFuncTemplate;
begin
  for I := 0 to High(F.UpvalNames) do
    if F.UpvalNames[I] = N then Exit(I);
  if F.Parent = nil then Exit(-1);
  T := F.Tpl;
  if ResolveLocal(F.Parent, N, Reg) then
  begin
    MarkCaptured(F.Parent, Reg);
    SetLength(F.UpvalNames, Length(F.UpvalNames) + 1);
    F.UpvalNames[High(F.UpvalNames)] := N;
    SetLength(T^.Upvals, Length(T^.Upvals) + 1);
    T^.Upvals[High(T^.Upvals)].FromParentLocal := True;
    T^.Upvals[High(T^.Upvals)].Index := Reg;
    Exit(High(T^.Upvals));
  end;
  PIdx := ResolveUpval(F.Parent, N);
  if PIdx < 0 then Exit(-1);
  SetLength(F.UpvalNames, Length(F.UpvalNames) + 1);
  F.UpvalNames[High(F.UpvalNames)] := N;
  SetLength(T^.Upvals, Length(T^.Upvals) + 1);
  T^.Upvals[High(T^.Upvals)].FromParentLocal := False;
  T^.Upvals[High(T^.Upvals)].Index := PIdx;
  Result := High(T^.Upvals);
end;

procedure TCompiler.EmitCloseFrom(Depth: Integer);
var I, Base: Integer; Any: Boolean;
begin
  Any := False; Base := 0;
  for I := FS.ScopeCount - 1 downto Depth do
    if FS.Scopes[I].Captured then begin Any := True; Base := FS.Scopes[I].BaseReg; end;
  if Any then Emit(Ins(opClose, Base, 0, 0));
end;

{ ---------------- identifiers ---------------- }

function TCompiler.FindBinding(const N: UnicodeString; out F: TFuncState; out S: TScope; out J: Integer): Boolean;
var I: Integer;
begin
  F := FS;
  while F <> nil do
  begin
    for I := F.ScopeCount - 1 downto 0 do
    begin
      J := F.Scopes[I].Find(N);
      if J >= 0 then begin S := F.Scopes[I]; Exit(True); end;
    end;
    F := F.Parent;
  end;
  Result := False;
end;

function TCompiler.InTDZ(const N: UnicodeString): Boolean;
var F: TFuncState; S: TScope; J: Integer;
begin
  Result := False;
  if not FindBinding(N, F, S, J) then Exit;
  if not S.Bindings[J].IsLexical then Exit;
  if S.Bindings[J].Initialized and not S.IsSwitch then Exit;
  Result := True;
  { the scope's entry stores the hole (F's code, still being compiled) }
  if S.Bindings[J].HoleNop >= 0 then F.Tpl^.Code[S.Bindings[J].HoleNop] := Ins(opLoadHole, S.Bindings[J].Reg, 0, 0);
end;

function TCompiler.BindingIsConst(const N: UnicodeString): Boolean;
var F: TFuncState; S: TScope; J: Integer;
begin
  Result := FindBinding(N, F, S, J) and S.Bindings[J].IsConst;
end;

{ 9.1.1.1.5 SetMutableBinding of an immutable binding: a TypeError in strict code; a named function expression's
  name (CreateImmutableBinding with strict false) ignores a non-strict assignment }
function TCompiler.EmitConstStore(const N: UnicodeString): Boolean;
var F: TFuncState; S: TScope; J: Integer;
begin
  Result := FindBinding(N, F, S, J) and S.Bindings[J].IsConst;
  if not Result then Exit;
  if S.Bindings[J].IsFnName and not FS.Tpl^.IsStrict then Exit;
  Emit(Ins(opThrowConst, 0, 0, AtomConst(N)));
end;

procedure TCompiler.MarkInitialized(const N: UnicodeString);
var I, J: Integer;
begin
  for I := FS.ScopeCount - 1 downto 0 do
  begin
    J := FS.Scopes[I].Find(N);
    if J >= 0 then
    begin
      if not FS.Scopes[I].IsSwitch then FS.Scopes[I].Bindings[J].Initialized := True;
      Exit;
    end;
  end;
end;

{ 14.11 identifiers in a with statement's body (and the functions in it) whose binding is outside the with:
  each applicable with object is asked first, innermost first (Idx gets their positions in WithNames) }
function TCompiler.WithsFor(const N: UnicodeString; var Idx: array of Integer): Integer;
var F, G: TFuncState; S, I: Integer; Found: Boolean; Inside: Boolean;
begin
  Result := 0;
  if (WithCount = 0) or (N = '') or (N[1] = '%') or (N[1] = '#') then Exit;
  { where the static binding is: function F, scope S (F nil: none, a global) }
  F := FS; S := -1; Found := False;
  while (F <> nil) and not Found do
  begin
    for S := F.ScopeCount - 1 downto 0 do
      if F.Scopes[S].Find(N) >= 0 then begin Found := True; Break; end;
    if not Found then F := F.Parent;
  end;
  for I := WithCount - 1 downto 0 do
  begin
    Inside := False;
    if Found then
    begin
      if (F = WithFuncs[I]) and (WithKinds[I] = 2) then
        Inside := EvalDepth[F.Scopes[S].Bindings[F.Scopes[S].Find(N)].Reg] <= WithDepths[I]
      else if (F = WithFuncs[I]) and (WithKinds[I] = 3) then
        { a with statement around the direct eval: the caller's bindings come innermost first, so one listed
          before the with's object is inside its body }
        Inside := F.Scopes[S].Find(N) < WithScopes[I]
      else if F = WithFuncs[I] then Inside := S >= WithScopes[I]
      else
      begin
        { a function nested in the with's function, compiled now, is inside the with's body }
        G := F.Parent;
        while (G <> nil) and (G <> WithFuncs[I]) do G := G.Parent;
        Inside := G <> nil;
      end;
    end;
    if Inside then Break;   { an inner declaration shadows this with and every outer one }
    Idx[Result] := I;
    Inc(Result);
  end;
end;

{ for each applicable with object: if it has the binding, Dest := its value (ThisReg >= 0: the object), then jump
  to the end; the static lookup follows the chain }
procedure TCompiler.EmitWithChain(const N: UnicodeString; Dest, ThisReg: Integer; var Ends: TJumpList; ForCall: Boolean);
var Idx: array[0..63] of Integer; C, I, T, J: Integer;
begin
  SetLength(Ends, 0);
  C := WithsFor(N, Idx);
  for I := 0 to C - 1 do
  begin
    T := AllocRegs(2);
    LoadIdentStatic(WithNames[Idx[I]], T);
    Emit(Ins(opWithHas, T + 1, T, AtomConst(N)));
    J := EmitJ(opJF, T + 1, 0);
    Emit(Ins(opWithGet, Dest, T, AtomConst(N)) or (QWord(Ord(FS.Tpl^.IsStrict)) shl 63));
    { ThisReg: the reference's environment; a call's this, where an eval's variable environment (declarative)
      gives undefined }
    if ThisReg >= 0 then
    begin
      if ForCall and (WithKinds[Idx[I]] in [1, 2]) then Emit(Ins(opLoadUndef, ThisReg, 0, 0))
      else Emit(Ins(opMov, ThisReg, T, 0));
    end;
    SetLength(Ends, Length(Ends) + 1);
    Ends[High(Ends)] := EmitJ(opJmp, 0, 0);
    PatchHere(J);
    FreeTo(T);
  end;
end;

procedure TCompiler.LoadIdent(const N: UnicodeString; Dest: Integer);
var Ends: TJumpList; I: Integer;
begin
  CheckIdent(N);
  if WithCount = 0 then begin LoadIdentStatic(N, Dest); Exit; end;
  EmitWithChain(N, Dest, -1, Ends);
  LoadIdentStatic(N, Dest);
  for I := 0 to High(Ends) do PatchHere(Ends[I]);
end;

{ PutValue to the reference LoadIdentThis resolved: Env the with object that held N, undefined for N's static
  binding (a compound assignment resolves its reference once, before its right-hand side runs) }
procedure TCompiler.StoreIdentRef(const N: UnicodeString; Env, Src: Integer);
var J, J2, T: Integer;
begin
  J := EmitJ(opJUndef, Env, 0);
  { 9.1.1.2.5 SetMutableBinding of an object environment }
  Emit(Ins(opWithSet, Env, Src, AtomConst(N)) or (QWord(2 + Ord(FS.Tpl^.IsStrict)) shl 62));
  J2 := EmitJ(opJmp, 0, 0);
  PatchHere(J);
  StoreIdentStatic(N, Src, False);
  PatchHere(J2);
end;

{ ResolveBinding(N) where with objects can hold it: Env the with object that has N, else undefined (N's static
  binding), for StoreIdentRef }
procedure TCompiler.ResolveWithEnv(const N: UnicodeString; Env: Integer);
var Idx: array[0..63] of Integer; C, I, T, J: Integer; Ends: TJumpList;
begin
  CheckIdent(N);
  SetLength(Ends, 0);
  C := WithsFor(N, Idx);
  for I := 0 to C - 1 do
  begin
    T := AllocRegs(2);
    LoadIdentStatic(WithNames[Idx[I]], T);
    Emit(Ins(opWithHas, T + 1, T, AtomConst(N)));
    J := EmitJ(opJF, T + 1, 0);
    Emit(Ins(opMov, Env, T, 0));
    SetLength(Ends, Length(Ends) + 1);
    Ends[High(Ends)] := EmitJ(opJmp, 0, 0);
    PatchHere(J);
    FreeTo(T);
  end;
  Emit(Ins(opLoadUndef, Env, 0, 0));
  for I := 0 to High(Ends) do PatchHere(Ends[I]);
end;

{ a call's callee by name: this is the with object it came from, else undefined }
procedure TCompiler.LoadIdentThis(const N: UnicodeString; Dest, ThisReg: Integer; ForCall: Boolean);
var Ends: TJumpList; I: Integer;
begin
  CheckIdent(N);
  EmitWithChain(N, Dest, ThisReg, Ends, ForCall);
  LoadIdentStatic(N, Dest);
  Emit(Ins(opLoadUndef, ThisReg, 0, 0));
  for I := 0 to High(Ends) do PatchHere(Ends[I]);
end;

procedure TCompiler.LoadIdentStatic(const N: UnicodeString; Dest: Integer);
var Reg, U: Integer; F: TFuncState;
begin
  if ResolveLocal(FS, N, Reg) then
  begin
    if (FS.ParamTDZ <> nil) and (FS.ParamTDZ.IndexOf(N) >= 0) and (FS.Scopes[0].Find(N) >= 0) and
       (FS.Scopes[0].Bindings[FS.Scopes[0].Find(N)].Reg = Reg) then
    begin
      { a parameter read before its own initialization: a ReferenceError }
      Emit(Ins(opLoadHole, Dest, 0, 0));
      Emit(Ins(opCheckHole, Dest, 0, AtomConst(N)));
      Exit;
    end;
    if Reg <> Dest then Emit(Ins(opMov, Dest, Reg, 0));
    if InTDZ(N) then Emit(Ins(opCheckHole, Dest, 0, AtomConst(N)));
    Exit;
  end;
  U := ResolveUpval(FS, N);
  if U >= 0 then
  begin
    Emit(Ins(opGetUpv, Dest, U, 0));
    if InTDZ(N) then Emit(Ins(opCheckHole, Dest, 0, AtomConst(N)));
    Exit;
  end;
  if (Goal = 3) and (ModImportSlot(N) >= 0) then begin EmitModGet(ModImportSlot(N), Dest); Exit; end;
  if N = 'arguments' then
  begin
    F := FS;
    while (F <> nil) and F.IsArrow do F := F.Parent;
    { goal 4: eval code (an arrow) uses its caller's arguments object }
    if (F <> nil) and F.IsEvalOuter then
    begin
      U := ResolveUpval(FS, '%arguments');
      if U >= 0 then begin Emit(Ins(opGetUpv, Dest, U, 0)); Exit; end;
    end;
    { a Script's or global eval code's top level has no arguments binding; isolated code (goal 0, including a
      direct eval inside a function until G2 gives it its caller's scope) keeps an arguments object of its own }
    if (F <> nil) and ((F.Parent <> nil) or (Goal = 0)) then
    begin
      F.ArgsUsed := True;
      if F = FS then Emit(Ins(opMov, Dest, F.ArgsReg, 0))
      else Emit(Ins(opGetUpv, Dest, ResolveUpval(FS, '%arguments'), 0));
      Exit;
    end;
  end;
  { 15.7.1 AllPrivateIdentifiersValid: a private name no enclosing class declares (eval code has no scope here) }
  if (N <> '') and (N[1] = '#') and (Goal <> 2) then
    raise EEarlySyntaxError.Create('Private field ''' + N + ''' must be declared in an enclosing class');
  if N = 'undefined' then begin Emit(Ins(opLoadUndef, Dest, 0, 0)); Exit; end;
  if N = 'NaN' then begin LoadNumber(NaN, Dest); Exit; end;
  if N = 'Infinity' then begin LoadNumber(Infinity, Dest); Exit; end;
  U := GLexFind(AtomOf(N));
  if U >= 0 then begin Emit(Ins(opGetGLex, Dest, 0, U)); Exit; end;
  Emit(Ins(opGetGlobal, Dest, 0, NewIC(N)));
end;

procedure TCompiler.StoreIdent(const N: UnicodeString; Src: Integer; IsInit: Boolean);
var Idx: array[0..63] of Integer; C, I, T, J: Integer; Ends: TJumpList;
begin
  CheckIdent(N);
  if WithCount = 0 then begin StoreIdentStatic(N, Src, IsInit); Exit; end;
  C := WithsFor(N, Idx);
  SetLength(Ends, 0);
  for I := 0 to C - 1 do
  begin
    T := AllocRegs(2);
    LoadIdentStatic(WithNames[Idx[I]], T);
    Emit(Ins(opWithHas, T + 1, T, AtomConst(N)));
    J := EmitJ(opJF, T + 1, 0);
    Emit(Ins(opWithSet, T, Src, AtomConst(N)) or (QWord(2 + Ord(FS.Tpl^.IsStrict)) shl 62));
    SetLength(Ends, Length(Ends) + 1);
    Ends[High(Ends)] := EmitJ(opJmp, 0, 0);
    PatchHere(J);
    FreeTo(T);
  end;
  StoreIdentStatic(N, Src, IsInit);
  for I := 0 to High(Ends) do PatchHere(Ends[I]);
end;

procedure TCompiler.PushWithEntry(const Nm: UnicodeString; F: TFuncState; Scope, Kind, Depth: Integer);
begin
  if WithCount >= Length(WithNames) then
  begin
    SetLength(WithNames, WithCount * 2 + 4); SetLength(WithFuncs, WithCount * 2 + 4); SetLength(WithScopes, WithCount * 2 + 4);
    SetLength(WithKinds, WithCount * 2 + 4); SetLength(WithDepths, WithCount * 2 + 4);
  end;
  WithNames[WithCount] := Nm; WithFuncs[WithCount] := F; WithScopes[WithCount] := Scope;
  WithKinds[WithCount] := Kind; WithDepths[WithCount] := Depth;
  Inc(WithCount);
end;

{ 14.11.2 with (Object) Statement: ToObject, an object environment for the body (a hidden local, captured by the
  body's functions like any other) }
procedure TCompiler.CompileWith(S: TGocciaWithStatement);
var R: Integer; Nm: UnicodeString;
begin
  if FS.Tpl^.IsStrict then raise EEarlySyntaxError.Create('Strict mode code may not include a with statement');
  CheckStatementBody(S.Body, False, False);
  PushScope;
  Nm := '%with' + IntToStr(WithSerial);
  Inc(WithSerial);
  R := DeclareLocal(Nm, True);
  ExprTo(S.ObjectExpression, R);
  Emit(Ins(opWithObj, R, 0, 0));
  PushWithEntry(Nm, FS, FS.ScopeCount, 0, 0);
  try
    Stmt(S.Body);
  finally
    Dec(WithCount);
  end;
  PopScope;
end;

procedure TCompiler.StoreIdentStatic(const N: UnicodeString; Src: Integer; IsInit: Boolean);
var Reg, U, T: Integer; F: TFuncState;
begin
  if ResolveLocal(FS, N, Reg) then
  begin
    if not IsInit then
    begin
      if InTDZ(N) then Emit(Ins(opCheckHole, Reg, 0, AtomConst(N)));
      if EmitConstStore(N) then Exit;
    end;
    if Reg <> Src then Emit(Ins(opMov, Reg, Src, 0));
    Exit;
  end;
  U := ResolveUpval(FS, N);
  if U >= 0 then
  begin
    if not IsInit then
    begin
      if InTDZ(N) then
      begin
        T := AllocReg;
        Emit(Ins(opGetUpv, T, U, 0));
        Emit(Ins(opCheckHole, T, 0, AtomConst(N)));
        FreeTo(T);
      end;
      if EmitConstStore(N) then Exit;
    end;
    Emit(Ins(opSetUpv, Src, U, 0));
    Exit;
  end;
  if (Goal = 3) and (ModImportSlot(N) >= 0) then
  begin
    { an import binding is immutable (9.1.1.5.5 CreateImportBinding): a ReferenceError while its target is
      uninitialized, otherwise a TypeError (module code is strict) }
    T := AllocReg;
    EmitModGet(ModImportSlot(N), T);
    FreeTo(T);
    Emit(Ins(opThrowConst, 0, 0, AtomConst(N)));
    Exit;
  end;
  if N = 'arguments' then
  begin
    { a non-strict function's arguments binding is mutable (10.2.11 step 22.f: CreateMutableBinding), as
      LoadIdentStatic resolves it }
    F := FS;
    while (F <> nil) and F.IsArrow do F := F.Parent;
    if (F <> nil) and F.IsEvalOuter then
    begin
      U := ResolveUpval(FS, '%arguments');
      if U >= 0 then begin Emit(Ins(opSetUpv, Src, U, 0)); Exit; end;
    end;
    if (F <> nil) and ((F.Parent <> nil) or (Goal = 0)) then
    begin
      F.ArgsUsed := True;
      if F = FS then begin if F.ArgsReg <> Src then Emit(Ins(opMov, F.ArgsReg, Src, 0)); end
      else Emit(Ins(opSetUpv, Src, ResolveUpval(FS, '%arguments'), 0));
      Exit;
    end;
  end;
  U := GLexFind(AtomOf(N));
  if U >= 0 then begin Emit(Ins(opSetGLex, Src, 0, U)); Exit; end;
  Emit(Ins(opSetGlobal, Src, 0, NewIC(N)));
end;

function TCompiler.HomeRegister: Integer;
var F: TFuncState;
begin
  { the nearest non-arrow function holds its home object in a reserved register; the program's top level (eval
    code outside a method: 19.2.1.1) has none }
  F := FS;
  while (F <> nil) and F.IsArrow do F := F.Parent;
  { goal 4: eval code in a method reaches its caller's home object }
  if (F <> nil) and F.IsEvalOuter then
  begin
    if F.Scopes[0].Find('%home') < 0 then raise EEarlySyntaxError.Create('''super'' keyword unexpected here');
    Exit(-1);
  end;
  if (F = nil) or (F.Parent = nil) then raise EEarlySyntaxError.Create('''super'' keyword unexpected here');
  if not FS.IsArrow then
  begin
    FS.HomeUsed := True;
    Exit(FS.HomeReg);
  end;
  F := FS;
  while (F <> nil) and F.IsArrow do F := F.Parent;
  if F = nil then raise ECompileError.Create('super outside a method');
  F.HomeUsed := True;
  Result := -1;
end;

function TCompiler.NewTargetRegister: Integer;
begin
  { an async function or a generator is never constructed: its new.target is undefined (and its async machinery
    reuses the register) }
  if not FS.IsArrow and (FS.Tpl^.IsAsync or FS.Tpl^.IsGenerator) then Exit(-1);
  if not FS.IsArrow then
  begin
    FS.NewTargetUsed := True;
    Exit(FS.NewTargetReg);
  end;
  Result := -1;
end;

{ an arrow's new.target: its nearest non-arrow function's, which must then fill its register }
function TCompiler.NewTargetFromArrow: Boolean;
var F: TFuncState;
begin
  F := FS;
  while (F <> nil) and F.IsArrow do F := F.Parent;
  if (F <> nil) and not F.IsEvalOuter and (F.Tpl^.IsAsync or F.Tpl^.IsGenerator) then Exit(False);
  if (F <> nil) and not F.IsEvalOuter then F.NewTargetUsed := True;
  Result := ResolveUpval(FS, '%newtarget') >= 0;
end;

{ new.target outside every non-arrow function: a SyntaxError (15.1.1 / 19.2.1.1), except in eval code whose caller
  is one (goal 4: its new.target an upvalue) }
function TCompiler.NewTargetOutside: Boolean;
var F: TFuncState;
begin
  F := FS;
  while (F <> nil) and F.IsArrow do F := F.Parent;
  Result := (F = nil) or ((F.Parent = nil) and not F.IsEvalOuter) or (F.IsEvalOuter and F.NoNewTarget);
end;

{ ---------------- literals ---------------- }

procedure TCompiler.LoadNumber(D: Double; Dest: Integer);
var V: TValue;
begin
  V := MkNumber(D);
  if IsInt(V) and (IntOf(V) >= -8000000) and (IntOf(V) <= 8000000) then
    Emit(Ins(opLoadI, Dest, 0, IntOf(V)))
  else
    Emit(Ins(opLoadK, Dest, 0, AddConst(V)));
end;

procedure TCompiler.LoadString(const S: UnicodeString; Dest: Integer);
begin
  Emit(Ins(opLoadK, Dest, 0, AtomConst(S)));
end;

procedure TCompiler.CompileLiteral(V: TGocciaValue; Dest: Integer);
begin
  if V is TGocciaNumberLiteralValue then LoadNumber(TGocciaNumberLiteralValue(V).Value, Dest)
  else if V is TGocciaStringLiteralValue then LoadString(TGocciaStringLiteralValue(V).Value, Dest)
  else if V is TGocciaBooleanLiteralValue then
  begin
    if TGocciaBooleanLiteralValue(V).Value then Emit(Ins(opLoadTrue, Dest, 0, 0))
    else Emit(Ins(opLoadFalse, Dest, 0, 0));
  end
  else if V is TGocciaNullLiteralValue then Emit(Ins(opLoadNull, Dest, 0, 0))
  else if V is TGocciaUndefinedLiteralValue then Emit(Ins(opLoadUndef, Dest, 0, 0))
  else if V is TGocciaBigIntValue then CompileBigIntLiteral(TGocciaBigIntValue(V), Dest)
  else RaiseUnsupported('literal', V);
end;

procedure TCompiler.CompileBigIntLiteral(V: TGocciaBigIntValue; Dest: Integer);
begin
  Emit(Ins(opLoadK, Dest, 0, AddConst(BigToValue(V.Value, True))));
end;

{ ---------------- functions ---------------- }

function TCompiler.ChildIndex(T: PFuncTemplate): Integer;
var P: PFuncTemplate;
begin
  P := FS.Tpl;
  SetLength(P^.Children, Length(P^.Children) + 1);
  P^.Children[High(P^.Children)] := T;
  Result := High(P^.Children);
end;

procedure TCompiler.EmitClosure(Dest: Integer; T: PFuncTemplate);
begin
  Emit(Ins(opClosure, Dest, 0, ChildIndex(T)));
end;

function BodyStatements(Body: TGocciaASTNode): TObjectList<TGocciaASTNode>;
begin
  if Body is TGocciaBlockStatement then Result := TGocciaBlockStatement(Body).Nodes else Result := nil;
end;

function TCompiler.CompileFunction(const Name: UnicodeString; const Params: TGocciaParameterArray; Body: TGocciaASTNode;
  IsArrow, IsMethod, IsClassCtor, IsDerived, IsGenerator, IsAsync: Boolean; FieldInit: TGocciaClassDefinition;
  BodyNodes: TList<TGocciaASTNode>): PFuncTemplate;
var Outer, F: TFuncState; T: PFuncTemplate; I, R, J, NP, HomeNop, NTNop, CtorNop: Integer; Names, TopLex: TStringList; ObjMeth, NoArgs, StaticBlk: Boolean;
  Nodes: TList<TGocciaASTNode>; Skip: Integer; Ret: Integer;
  Pair: TGocciaExpressionMap.TKeyValuePair; SavedGlobalTop, InitStatic, SavedParamInit: Boolean; PN: TStringList;
  K, SavedWith, Tmp: Integer; SloppyEval: Boolean; EnvName: UnicodeString;
begin
  SavedGlobalTop := GlobalTop;   { a nested function's declarations are its own }
  SavedParamInit := InParamInit;
  InParamInit := False;
  GlobalTop := False;
  InitStatic := PendingInitStatic;
  PendingInitStatic := False;
  SloppyEval := NextSloppyEval;
  NextSloppyEval := False;
  SavedWith := WithCount;
  New(T);
  RegisterTemplate(T);
  FillChar(T^, SizeOf(TFuncTemplate), 0);
  T^.Name := AtomOf(Name);
  ObjMeth := ObjLiteralMethod;
  ObjLiteralMethod := False;
  NoArgs := PendingNoArguments;
  PendingNoArguments := False;
  StaticBlk := PendingStaticBlock;
  PendingStaticBlock := False;
  T^.IsArrow := IsArrow; T^.IsMethod := IsMethod; T^.IsClassCtor := IsClassCtor; T^.IsDerived := IsDerived;
  T^.IsGenerator := IsGenerator; T^.IsAsync := IsAsync;
  T^.RestReg := -1; T^.ArgsReg := -1; T^.HomeReg := -1; T^.NewTargetReg := -1; T^.AsyncReg := -1;
  T^.ModuleId := CompilingModuleIdOf;
  T^.SourceName := SourceName;
  if PendingStrict >= 0 then T^.IsStrict := PendingStrict = 1
  else if FS <> nil then T^.IsStrict := FS.Tpl^.IsStrict
  else T^.IsStrict := False;
  T^.TopLevel := FS = nil;
  { class code is strict; an object literal's method is a method (no constructor, no prototype) but not strict }
  if (IsMethod and not ObjMeth) or IsClassCtor then T^.IsStrict := True;
  if ObjMeth then T^.IsMethod := True;
  PendingStrict := -1;
  Outer := FS;
  F := TFuncState.Create;
  F.Parent := Outer;
  F.Tpl := T;
  F.IsArrow := IsArrow;
  F.HomeReg := -1; F.NewTargetReg := -1;
  F.NoArguments := NoArgs;
  F.UsingCapReg := -1;
  F.IsStaticBlock := StaticBlk;
  if Outer <> nil then F.CurLine := Outer.CurLine;
  FS := F;
  try
    PushScope(True);
    { R0 = this }
    if IsArrow then AllocReg else DeclareInScope(FS.Scopes[0], '%this', False);
    NP := 0;
    for I := 0 to High(Params) do
      if not Params[I].IsRest then Inc(NP);
    T^.NumParams := NP;
    T^.SimpleParams := True;
    for I := 0 to High(Params) do
      if Params[I].IsRest or Params[I].IsPattern or Assigned(Params[I].DefaultValue) then T^.SimpleParams := False;
    T^.Length := 0;
    for I := 0 to High(Params) do
    begin
      if Params[I].IsRest or Assigned(Params[I].DefaultValue) then Break;
      Inc(T^.Length);
    end;
    { 15.3 ArrowParameters[?Await]: in a class static block (15.7.1 ContainsAwait) await names nothing }
    F.ParamAwait := IsArrow and (Outer <> nil) and (Outer.IsStaticBlock or Outer.ParamAwait);
    { parameter registers 1..NP, in order; patterns get a hidden name }
    J := 0;
    for I := 0 to High(Params) do
      if not Params[I].IsRest then
      begin
        Inc(J);
        { 15.2.1 / 15.3.1: eval and arguments are not parameter names in strict code }
        if T^.IsStrict and not Params[I].IsPattern and ((Params[I].Name = 'eval') or (Params[I].Name = 'arguments')) then
          raise EEarlySyntaxError.Create('Unexpected eval or arguments in strict mode');
        if Params[I].IsPattern then DeclareLocal('%param' + IntToStr(J), False)
        else DeclareLocal(Params[I].Name, False);
      end;
    if not IsArrow then
    begin
      F.HomeReg := DeclareInScope(FS.Scopes[0], '%home', True);
      F.NewTargetReg := DeclareInScope(FS.Scopes[0], '%newtarget', True);
      F.ArgsReg := DeclareInScope(FS.Scopes[0], '%arguments', False);
    end
    else F.ArgsReg := -1;
    for I := 0 to High(Params) do
      if Params[I].IsRest then
      begin
        if Params[I].IsPattern then T^.RestReg := DeclareLocal('%rest', False)
        else T^.RestReg := DeclareLocal(Params[I].Name, False);
      end;
    { an async function keeps its promise in a register: it is saved and restored with the others at each await }
    { an async function's promise, an async generator's request queue: saved with the other registers }
    if IsAsync then T^.AsyncReg := DeclareInScope(FS.Scopes[0], '%promise', False);
    HomeNop := Emit(Ins(opNop, 0, 0, 0));
    NTNop := Emit(Ins(opNop, 0, 0, 0));
    F.CtorReg := -1;
    if IsClassCtor and IsDerived then
    begin
      F.CtorReg := DeclareInScope(FS.Scopes[0], '%ctor', True);
      CtorNop := Emit(Ins(opNop, 0, 0, 0));
    end;
    if SloppyEval and not T^.IsStrict and not ((Outer <> nil) and Outer.IsEvalOuter) then
    begin
      { 10.2.11 / 19.2.1.3: the variable environment a non-strict direct eval in this function (its parameter
        expressions too) adds its var and function declarations to, made on entry; the function's own
        bindings shadow it, so it sits between them and the enclosing scopes }
      EnvName := '%evalenv' + IntToStr(WithSerial);
      Inc(WithSerial);
      R := DeclareLocal(EnvName, False);
      Tmp := AllocReg;
      Emit(Ins(opNewObj, R, 0, 4));
      Emit(Ins(opLoadNull, Tmp, 0, 0));
      Emit(Ins(opSetProtoLit, R, Tmp, 0));
      FreeTo(Tmp);
      PushWithEntry(EnvName, FS, 0, 1, 0);
    end;
    { default values and destructuring of parameters }
    J := 0;
    InParamInit := True;
    if not T^.SimpleParams then
    begin
      { 10.2.11 with parameter expressions: each parameter is uninitialized until its own turn }
      FS.ParamTDZ := NewNameList;
      for I := 0 to High(Params) do
        if Params[I].IsPattern then PatternNames(Params[I].Pattern, FS.ParamTDZ)
        else FS.ParamTDZ.Add(Params[I].Name);
    end;
    for I := 0 to High(Params) do
    begin
      if not Params[I].IsRest then Inc(J);
      if Params[I].IsRest then
      begin
        if Params[I].IsPattern then BindPattern(Params[I].Pattern, T^.RestReg, True, False);
        Continue;
      end;
      if Params[I].IsPattern then R := FS.Scopes[0].Bindings[FS.Scopes[0].Find('%param' + IntToStr(J))].Reg
      else R := J;
      if Assigned(Params[I].DefaultValue) then
      begin
        Skip := EmitJ(opJNotUndef, R, 0);
        { 10.2.11 / 8.4.5: an anonymous function default of a plain parameter takes its name }
        if Params[I].IsPattern then ExprTo(Params[I].DefaultValue, R)
        else ExprToNamed(Params[I].DefaultValue, R, Params[I].Name);
        PatchHere(Skip);
      end;
      if Params[I].IsPattern then BindPattern(Params[I].Pattern, R, True, False);
      if FS.ParamTDZ <> nil then
      begin
        if Params[I].IsPattern then
        begin
          PN := NewNameList;
          try
            PatternNames(Params[I].Pattern, PN);
            for K := 0 to PN.Count - 1 do
              if FS.ParamTDZ.IndexOf(PN[K]) >= 0 then FS.ParamTDZ.Delete(FS.ParamTDZ.IndexOf(PN[K]));
          finally
            PN.Free;
          end;
        end
        else if FS.ParamTDZ.IndexOf(Params[I].Name) >= 0 then FS.ParamTDZ.Delete(FS.ParamTDZ.IndexOf(Params[I].Name));
      end;
    end;
    FreeAndNil(FS.ParamTDZ);
    InParamInit := False;
    F.ParamAwait := False;
    if DefaultSuperForward then
    begin
      // constructor(...args) { super(...args); }
      DefaultSuperForward := False;
      R := AllocRegs(3);
      Emit(Ins(opMov, R + 2, T^.RestReg, 0));
      Emit(Ins(opSuperCallV, R, 0, 0));
      FreeTo(R);
    end;
    { a class's instance or static initializer (see CompileClass) }
    if Assigned(FieldInit) then EmitClassInit(FieldInit, InitStatic);
    { body }
    if Assigned(BodyNodes) then Nodes := BodyNodes
    else if Body is TGocciaBlockStatement then Nodes := TList<TGocciaASTNode>(TGocciaBlockStatement(Body).Nodes)
    else Nodes := nil;
    if Nodes <> nil then
    begin
      Names := NewNameList;
      try
        for I := 0 to Nodes.Count - 1 do CollectVarNames(Nodes[I], Names);
        if not T^.IsStrict and (Goal <> 3) then
        begin
          { B.3.2: block-level functions of non-strict code also get a var binding }
          TopLex := NewNameList;
          try
            LexNamesOf(Nodes, TopLex);
            for I := 0 to High(Params) do
              if Params[I].IsPattern then PatternNames(Params[I].Pattern, TopLex)
              else TopLex.Add(Params[I].Name);
            CollectAnnexB(Nodes, TopLex, Names);
          finally
            TopLex.Free;
          end;
        end;
        if (Outer <> nil) and Outer.IsEvalOuter then EvalDeclarations(Nodes, Names, T^.IsStrict);
        if (Outer = nil) and (Goal in [1, 2]) then
        begin
          GlobalTop := True;
          DeclareGlobals(T, Nodes, Names);
          Names.Clear;
        end;
        for I := 0 to Names.Count - 1 do
          if (Names[I] = 'arguments') and (F.ArgsReg >= 0) and (FS.Scopes[0].Find('arguments') < 0) then
          begin
            { 10.2.11 step 27: a var named arguments is the arguments object's binding, initialized to it }
            FS.Scopes[0].Add('arguments', F.ArgsReg, False);
            F.ArgsUsed := True;
          end
          else if FS.Scopes[0].Find(Names[I]) < 0 then
          begin
            R := DeclareLocal(Names[I], False);
            { a call fills the registers past the arguments with undefined: only code that ran before
              (parameter defaults, destructuring, fields) can have left something in this one }
            if T^.CodeLen > NTNop + 1 then Emit(Ins(opLoadUndef, R, 0, 0));
          end;
      finally
        Names.Free;
      end;
      if (EvalEnvNames <> nil) and (Outer <> nil) and Outer.IsEvalOuter then
      begin
        { 19.2.1.3 step 16.b: each var the caller's function does not bind becomes a deletable binding of its
          eval environment, undefined unless it is already there }
        Tmp := AllocRegs(2);
        LoadIdentStatic(EvalVarEnv, Tmp);
        for I := 0 to EvalEnvNames.Count - 1 do
        begin
          Emit(Ins(opWithHas, Tmp + 1, Tmp, AtomConst(EvalEnvNames[I])));
          J := EmitJ(opJT, Tmp + 1, 0);
          Emit(Ins(opLoadUndef, Tmp + 1, 0, 0));
          Emit(Ins(opDefField, Tmp, Tmp + 1, NewIC(EvalEnvNames[I])));
          PatchHere(J);
        end;
        FreeTo(Tmp);
        FreeAndNil(EvalEnvNames);
      end;
      HoistBlock(Nodes);
      FreeAndNil(EvalSkipLocal);
      if (Outer = nil) and (Goal = 3) then EmitModulePrologue;
      { a generator is made once its declarations are instantiated (its parameters evaluated) }
      if IsGenerator then Emit(Ins(opGenStart, 0, 0, 0));
      if GlobalTop and (Outer = nil) then
      begin
        GlobalTop := False;
        { each top-level let, const and class register becomes a global lexical binding's cell }
        for I := 0 to High(T^.GdiLex) do
        begin
          ResolveLocal(FS, StrToUnicode(T^.GdiLex[I]), R);
          Emit(Ins(opMakeGLex, R, 0, I));
        end;
      end;
      if ReturnLast and ((Outer = nil) or Outer.IsEvalOuter) and (Nodes.Count > 0) then
      begin
        { the script's value: its statement list's completion value }
        ReturnLast := False;
        Ret := AllocReg;
        Emit(Ins(opLoadUndef, Ret, 0, 0));
        F.CplP1 := Ret + 1;
        F.CplSkip := EvalStrictPrefix;
        EvalStrictPrefix := False;   { consumed: eval code this code runs is compiled on its own }
        StmtNodes(Nodes);
        F.CplP1 := 0;
        Emit(Ins(opRet, Ret, 0, 0));
      end
      else
        StmtNodes(Nodes);
      Emit(Ins(opRetUndef, 0, 0, 0));
    end
    else if Body is TGocciaExpression then
    begin
      Ret := AllocReg;
      ExprTo(TGocciaExpression(Body), Ret);
      Emit(Ins(opRet, Ret, 0, 0));
    end
    else
      Emit(Ins(opRetUndef, 0, 0, 0));
    if F.HomeUsed then
    begin
      T^.Code[HomeNop] := Ins(opLoadHome, F.HomeReg, 0, 0);
      T^.HomeReg := F.HomeReg;
    end;
    if F.ArgsUsed then T^.ArgsReg := F.ArgsReg;
    if F.CtorUsed then T^.Code[CtorNop] := Ins(opLoadCallee, F.CtorReg, 0, 0);
    if F.NewTargetUsed then
    begin
      T^.Code[NTNop] := Ins(opLoadNewTarget, F.NewTargetReg, 0, 0);
      T^.NewTargetReg := F.NewTargetReg;
    end;
    while FS.ScopeCount > 0 do
    begin
      Dec(FS.ScopeCount);
      FS.Scopes[FS.ScopeCount].Free;
    end;
    if T^.MaxRegs < NP + 4 then T^.MaxRegs := NP + 4;
    Inc(T^.MaxRegs, 2);   { scratch for natives }
    SetLength(T^.Code, T^.CodeLen);
    SetLength(T^.Lines, T^.CodeLen);
    { the home-object and new.target placeholders stay no-ops when unused: calls start after them
      (nothing jumps into the prologue) }
    T^.EntryPC := 0;
    while (T^.EntryPC < 2) and (T^.EntryPC < T^.CodeLen) and (InsOp(T^.Code[T^.EntryPC]) = opNop) do Inc(T^.EntryPC);
    Result := T;
  finally
    FS := Outer;
    F.Free;
    GlobalTop := SavedGlobalTop;
    InParamInit := SavedParamInit;
    WithCount := SavedWith;
  end;
end;

{ 8.4.5 NamedEvaluation where the target is a plain identifier: an anonymous function or class takes its name }
procedure TCompiler.ExprToNamed(E: TGocciaExpression; Dest: Integer; const Name: UnicodeString);
begin
  if (E is TGocciaFunctionExpression) or (E is TGocciaArrowFunctionExpression) then CompileFuncExpr(E, Dest, Name)
  else if E is TGocciaClassExpression then CompileClass(TGocciaClassExpression(E).ClassDefinition, Dest, Name)
  else ExprTo(E, Dest);
end;

procedure TCompiler.CompileFuncExpr(E: TGocciaExpression; Dest: Integer; const NameHint: UnicodeString);
var T: PFuncTemplate; FE: TGocciaFunctionExpression; AE: TGocciaArrowFunctionExpression; Self_: Integer;
  NameScope: Boolean;
begin
  if E is TGocciaArrowFunctionExpression then
  begin
    AE := TGocciaArrowFunctionExpression(E);
    NextSloppyEval := deuSloppyDirectEval in AE.DirectEvalUsage;
    T := CompileFunction(NameHint, AE.Parameters, AE.Body, True, False, False, False, False, AE.IsAsync);
    T^.SourceRange := AE.SourceRange;
    EmitClosure(Dest, T);
  end
  else if E is TGocciaFunctionExpression then
  begin
    FE := TGocciaFunctionExpression(E);
    { 15.2.1 / 15.5.1 / 15.8.1: the expression's own name is in its own [Await] / [Yield] context }
    if ((FE.Name = 'await') and FE.IsAsync) or ((FE.Name = 'yield') and (FE.IsGenerator or FE.ParsedInStrictMode)) then
      raise EEarlySyntaxError.Create('Unexpected reserved word ''' + FE.Name + '''');
    { a named function expression can call itself by name: bind it in a scope around the function }
    NameScope := (FE.Name <> '');
    if NameScope then
    begin
      PushScope;
      NoIdentCheck := True;
      try
        Self_ := DeclareLocal(FE.Name, True);
        with FS.Scopes[FS.ScopeCount - 1] do Bindings[Find(FE.Name)].IsFnName := True;
      finally
        NoIdentCheck := False;
      end;
    end
    else Self_ := -1;
    PendingStrict := Ord(FE.ParsedInStrictMode);
    NextSloppyEval := deuSloppyDirectEval in FE.DirectEvalUsage;
    if FE.Name <> '' then
      T := CompileFunction(FE.Name, FE.Parameters, FE.Body, False, False, False, False, FE.IsGenerator, FE.IsAsync)
    else
      T := CompileFunction(NameHint, FE.Parameters, FE.Body, False, False, False, False, FE.IsGenerator, FE.IsAsync);
    T^.SourceRange := FE.SourceRange;
    if NameScope then
    begin
      EmitClosure(Self_, T);
      Emit(Ins(opMov, Dest, Self_, 0));
      PopScope;
    end
    else
      EmitClosure(Dest, T);
  end
  else if E is TGocciaObjectMethodDefinition then
  begin
    ObjLiteralMethod := True;
    CompileFuncExpr(TGocciaObjectMethodDefinition(E).FunctionExpression, Dest, NameHint);
  end
  else
    raise ECompileError.Create('not a function: ' + E.ClassName);
end;

{ ---------------- hoisting ---------------- }

threadvar
  VarNameKinds: TKindCache;
const
  VarNameClasses: array[0..11] of TClass = (TGocciaVariableDeclaration, TGocciaDestructuringDeclaration,
    TGocciaBlockStatement, TGocciaIfStatement, TGocciaForStatement, TGocciaWhileStatement,
    TGocciaDoWhileStatement, TGocciaForOfStatement, TGocciaForInStatement, TGocciaTryStatement,
    TGocciaSwitchStatement, TGocciaWithStatement);

{ runs for every statement; most hold no var binding, and CollectVarNamesOf's
  string temporaries cost an exception frame per call }
{ the let / const / class names a statement list declares (its function declarations are not included) }
procedure LexNamesOf(L: TList<TGocciaASTNode>; Out_: TStringList);
var I, K: Integer; N: TGocciaASTNode; VD: TGocciaVariableDeclaration;
begin
  for I := 0 to L.Count - 1 do
  begin
    N := L[I];
    if (N is TGocciaVariableDeclaration) and not TGocciaVariableDeclaration(N).IsVar then
    begin
      VD := TGocciaVariableDeclaration(N);
      for K := 0 to High(VD.Variables) do
        if VD.Variables[K].IsPattern then PatternNames(VD.Variables[K].Pattern, Out_)
        else Out_.Add(VD.Variables[K].Name);
    end
    else if (N is TGocciaDestructuringDeclaration) and not TGocciaDestructuringDeclaration(N).IsVar then
      PatternNames(TGocciaDestructuringDeclaration(N).Pattern, Out_)
    else if N is TGocciaClassDeclaration then Out_.Add(TGocciaClassDeclaration(N).ClassDefinition.Name);
  end;
end;

function NewNameList: TStringList;
begin
  Result := TStringList.Create;
  Result.UseLocale := False;
  Result.CaseSensitive := True;
end;

procedure TCompiler.CollectAnnexB(Nodes: TList<TGocciaASTNode>; TopLex, Names: TStringList);
var I: Integer;
begin
  for I := 0 to Nodes.Count - 1 do AnnexBWalk(Nodes[I], TopLex, Names);
end;

{ B.3.2.1 step 29 / B.3.2.2 / B.3.2.3: a FunctionDeclaration in a block qualifies when a var of its name there
  would raise no early error: no enclosing block (nor the top level) declares the name lexically, its own block
  only by function declarations }
procedure TCompiler.AnnexBWalkList(L: TList<TGocciaASTNode>; Encl, Names: TStringList);
var I: Integer; Lex, E2: TStringList; FD: TGocciaFunctionDeclaration;
begin
  Lex := NewNameList;
  E2 := NewNameList;
  try
    LexNamesOf(L, Lex);
    E2.AddStrings(Encl);
    E2.AddStrings(Lex);
    for I := 0 to L.Count - 1 do
      if L[I] is TGocciaFunctionDeclaration then
      begin
        FD := TGocciaFunctionDeclaration(L[I]);
        E2.Add(FD.Name);
        if FD.FunctionExpression.IsGenerator or FD.FunctionExpression.IsAsync then Continue;
        if (Encl.IndexOf(FD.Name) >= 0) or (Lex.IndexOf(FD.Name) >= 0) or (FD.Name = 'arguments') then Continue;
        if AnnexBFuncs = nil then AnnexBFuncs := TDictionary<Pointer, Boolean>.Create;
        AnnexBFuncs.AddOrSetValue(FD, True);
        if Names.IndexOf(FD.Name) < 0 then Names.Add(FD.Name);
      end;
    for I := 0 to L.Count - 1 do AnnexBWalk(L[I], E2, Names);
  finally
    Lex.Free;
    E2.Free;
  end;
end;

procedure TCompiler.AnnexBWalk(N: TGocciaASTNode; Encl, Names: TStringList);
var L: TList<TGocciaASTNode>; E2: TStringList; I, K: Integer;
  procedure Clause(C: TGocciaASTNode);
  var One: TList<TGocciaASTNode>;
  begin
    if C is TGocciaFunctionDeclaration then
    begin
      { B.3.3: a function declaration as an if clause is one in a block of its own }
      One := TList<TGocciaASTNode>.Create;
      try
        One.Add(C);
        AnnexBWalkList(One, Encl, Names);
      finally
        One.Free;
      end;
    end
    else AnnexBWalk(C, Encl, Names);
  end;
begin
  if N = nil then Exit;
  if N is TGocciaBlockStatement then
    AnnexBWalkList(TList<TGocciaASTNode>(TGocciaBlockStatement(N).Nodes), Encl, Names)
  else if N is TGocciaIfStatement then
  begin
    Clause(TGocciaIfStatement(N).Consequent);
    if Assigned(TGocciaIfStatement(N).Alternate) then Clause(TGocciaIfStatement(N).Alternate);
  end
  else if (N is TGocciaForStatement) or (N is TGocciaForOfStatement) or (N is TGocciaForInStatement) then
  begin
    E2 := NewNameList;
    try
      E2.AddStrings(Encl);
      if N is TGocciaForStatement then
      begin
        L := TList<TGocciaASTNode>.Create;
        try
          if Assigned(TGocciaForStatement(N).Init) then L.Add(TGocciaForStatement(N).Init);
          LexNamesOf(L, E2);
        finally
          L.Free;
        end;
        AnnexBWalk(TGocciaForStatement(N).Body, E2, Names);
      end
      else if N is TGocciaForOfStatement then
      begin
        if not TGocciaForOfStatement(N).IsVar then
          if TGocciaForOfStatement(N).BindingName <> '' then E2.Add(TGocciaForOfStatement(N).BindingName)
          else PatternNames(TGocciaForOfStatement(N).BindingPattern, E2);
        AnnexBWalk(TGocciaForOfStatement(N).Body, E2, Names);
      end
      else
      begin
        if not TGocciaForInStatement(N).IsVar then
          if TGocciaForInStatement(N).BindingName <> '' then E2.Add(TGocciaForInStatement(N).BindingName)
          else PatternNames(TGocciaForInStatement(N).BindingPattern, E2);
        AnnexBWalk(TGocciaForInStatement(N).Body, E2, Names);
      end;
    finally
      E2.Free;
    end;
  end
  else if N is TGocciaWhileStatement then AnnexBWalk(TGocciaWhileStatement(N).Body, Encl, Names)
  else if N is TGocciaDoWhileStatement then AnnexBWalk(TGocciaDoWhileStatement(N).Body, Encl, Names)
  else if N is TGocciaWithStatement then AnnexBWalk(TGocciaWithStatement(N).Body, Encl, Names)
  else if N is TGocciaTryStatement then
  begin
    AnnexBWalk(TGocciaTryStatement(N).Block, Encl, Names);
    E2 := NewNameList;
    try
      E2.AddStrings(Encl);
      { B.3.4: a var may share a simple catch parameter's name, not a destructured one's }
      if Assigned(TGocciaTryStatement(N).CatchBindingPattern) then PatternNames(TGocciaTryStatement(N).CatchBindingPattern, E2);
      AnnexBWalk(TGocciaTryStatement(N).CatchBlock, E2, Names);
    finally
      E2.Free;
    end;
    AnnexBWalk(TGocciaTryStatement(N).FinallyBlock, Encl, Names);
  end
  else if N is TGocciaSwitchStatement then
  begin
    L := TList<TGocciaASTNode>.Create;
    try
      for I := 0 to TGocciaSwitchStatement(N).Cases.Count - 1 do
        for K := 0 to TGocciaSwitchStatement(N).Cases[I].Consequent.Count - 1 do
          L.Add(TGocciaSwitchStatement(N).Cases[I].Consequent[K]);
      AnnexBWalkList(L, Encl, Names);
    finally
      L.Free;
    end;
  end;
end;

{ B.3.2.1 step 29.a.ii.2 (and the global and eval forms): when the declaration is evaluated, the var binding
  gets the block binding's value }
procedure TCompiler.EmitAnnexBCopy(FD: TGocciaFunctionDeclaration);
var Src, J, U: Integer;
begin
  if not ResolveLocal(FS, FD.Name, Src) then Exit;
  J := FS.Scopes[0].Find(FD.Name);
  if J >= 0 then
  begin
    if FS.Scopes[0].Bindings[J].Reg <> Src then Emit(Ins(opMov, FS.Scopes[0].Bindings[J].Reg, Src, 0));
    Exit;
  end;
  { goal 4: the var binding is the caller's own, or one of its eval variable environment }
  if (FS.Parent <> nil) and FS.Parent.IsEvalOuter and (EvalVarEnv <> '') and
     ((EvalOuterFlags(FD.Name) < 0) or ((EvalOuterFlags(FD.Name) and efVarLevel) = 0)) then
  begin
    U := AllocReg;
    LoadIdentStatic(EvalVarEnv, U);
    Emit(Ins(opWithSet, U, Src, AtomConst(FD.Name)));
    FreeTo(U);
    Exit;
  end;
  U := ResolveUpval(FS, FD.Name);
  if U >= 0 then Emit(Ins(opSetUpv, Src, U, 0))
  else if (FS.Parent = nil) and (Goal in [1, 2]) then Emit(Ins(opSetGlobal, Src, 0, NewIC(FD.Name)));
end;

{ 19.2.1.1 PerformEval for a direct eval in function code: every binding the call site sees, innermost first, as a
  local register of this function or one of its upvalues (resolving an outer binding captures it), with what
  EvalDeclarationInstantiation needs to know about it }
function TCompiler.BuildEvalDesc: UnicodeString;
var Seen: TStringList; I, J, U, Fl, D: Integer; F: TFuncState; Sc: TScope;
  procedure Add(const N: UnicodeString; Kind, Idx, Flags: Integer; Depth: Integer = 0);
  begin
    Result := Result + N + #2 + IntToStr(Kind) + #2 + IntToStr(Idx) + #2 + IntToStr(Flags) + #2 + IntToStr(Depth) + #1;
  end;
begin
  Result := '';
  Seen := NewNameList;
  try
    { the nearest non-arrow function fills the registers eval code may read: home object, new.target, constructor }
    F := FS;
    while (F <> nil) and F.IsArrow do F := F.Parent;
    { a class field initializer or static block: its eval code may not contain arguments (19.2.1.1 step 5.b) }
    if (F <> nil) and F.NoArguments then
    begin
      Seen.Add('%noargs');
      Add('%noargs', 0, 0, 0);
    end;
    { an arrow at a Script's top level: its eval code may not contain new.target (19.2.1.1 step 5.a) }
    if NewTargetOutside then
    begin
      Seen.Add('%nonewtarget');
      Add('%nonewtarget', 0, 0, 0);
    end;
    if (F <> nil) and not F.IsEvalOuter and (F.Parent <> nil) then
    begin
      if F.HomeReg >= 0 then F.HomeUsed := True;
      if F.NewTargetReg >= 0 then F.NewTargetUsed := True;
      if F.CtorReg >= 0 then F.CtorUsed := True;
      if F.ArgsReg >= 0 then F.ArgsUsed := True;
    end;
    { the arguments object: the function's own, or (in an arrow) the nearest non-arrow function's }
    if not FS.IsArrow and (FS.ArgsReg >= 0) then
    begin
      FS.ArgsUsed := True;
      if FS.Scopes[0].Find('arguments') < 0 then
      begin
        { as %arguments: a function the eval code declares has an arguments object of its own }
        Fl := efVarLevel or efInitialized;
        if InParamInit then Fl := Fl or efParamEnv;
        Seen.Add('%arguments');
        Add('%arguments', 0, FS.ArgsReg, Fl);
      end;
    end;
    for I := FS.ScopeCount - 1 downto 0 do
    begin
      Sc := FS.Scopes[I];
      for J := Sc.Count - 1 downto 0 do
      begin
        if Seen.IndexOf(Sc.Bindings[J].Name) >= 0 then Continue;
        Seen.Add(Sc.Bindings[J].Name);
        { only a method has a home object (super) }
        if (Sc.Bindings[J].Name = '%home') and not (FS.Tpl^.IsMethod or FS.Tpl^.IsClassCtor) then Continue;
        Fl := efSameFunc;
        if Sc.Bindings[J].IsConst then Fl := Fl or efConst;
        if Sc.Bindings[J].IsFnName then Fl := Fl or efFnName;
        if Sc.Bindings[J].IsLexical then
        begin
          Fl := Fl or efLexical;
          if Sc.Bindings[J].Initialized and not Sc.IsSwitch then Fl := Fl or efInitialized
          else if Sc.Bindings[J].HoleNop >= 0 then
            FS.Tpl^.Code[Sc.Bindings[J].HoleNop] := Ins(opLoadHole, Sc.Bindings[J].Reg, 0, 0);
        end
        else Fl := Fl or efInitialized;
        if (I = 0) and not Sc.Bindings[J].IsLexical then
        begin
          Fl := Fl or efVarLevel;
          { the parameters, while their initializers run: a var of the eval may not share their names }
          if InParamInit and (Sc.Bindings[J].Name <> '') and (Sc.Bindings[J].Name[1] <> '%') then Fl := Fl or efParamEnv;
        end;
        MarkCaptured(FS, Sc.Bindings[J].Reg);
        Add(Sc.Bindings[J].Name, 0, Sc.Bindings[J].Reg, Fl);
      end;
    end;
    { the enclosing functions' bindings, as this function's upvalues; Depth: functions out from the caller }
    F := FS.Parent;
    D := 1;
    while F <> nil do
    begin
      for I := F.ScopeCount - 1 downto 0 do
      begin
        Sc := F.Scopes[I];
        for J := Sc.Count - 1 downto 0 do
        begin
          if Seen.IndexOf(Sc.Bindings[J].Name) >= 0 then Continue;
          Seen.Add(Sc.Bindings[J].Name);
          if (Sc.Bindings[J].Name = '%home') and not F.IsEvalOuter and not (F.Tpl^.IsMethod or F.Tpl^.IsClassCtor) then Continue;
          if (Sc.Bindings[J].Name = '%arguments') and not F.IsEvalOuter then F.ArgsUsed := True;
          U := ResolveUpval(FS, Sc.Bindings[J].Name);
          if U < 0 then Continue;
          Fl := efInitialized;
          if Sc.Bindings[J].IsConst then Fl := Fl or efConst;
          if Sc.Bindings[J].IsFnName then Fl := Fl or efFnName;
          if Sc.Bindings[J].IsLexical then
          begin
            Fl := efLexical;
            if Sc.Bindings[J].IsConst then Fl := Fl or efConst;
            if Sc.Bindings[J].HoleNop >= 0 then F.Tpl^.Code[Sc.Bindings[J].HoleNop] := Ins(opLoadHole, Sc.Bindings[J].Reg, 0, 0);
          end;
          if F.IsEvalOuter then Add(Sc.Bindings[J].Name, 1, U, Fl, D - 1 + EvalDepth[Sc.Bindings[J].Reg])
          else Add(Sc.Bindings[J].Name, 1, U, Fl, D);
        end;
      end;
      F := F.Parent;
      Inc(D);
    end;
  finally
    Seen.Free;
  end;
end;

{ goal 4: the ef* flags of the caller's binding N, -1 when the caller has none }
function TCompiler.EvalOuterFlags(const N: UnicodeString): Integer;
var F: TFuncState; J: Integer;
begin
  Result := -1;
  F := FS;
  while (F <> nil) and not F.IsEvalOuter do F := F.Parent;
  if F = nil then Exit;
  J := F.Scopes[0].Find(N);
  if (J < 0) and (N = 'arguments') then J := F.Scopes[0].Find('%arguments');
  if J >= 0 then Result := EvalFlags[F.Scopes[0].Bindings[J].Reg];
end;

{ 19.2.1.4 EvalDeclarationInstantiation for goal 4 (the variable environment the caller's function): a var name
  that a lexical binding between the eval and that environment holds, or (in parameter initializers) a parameter or
  arguments, is a SyntaxError; one the function itself binds is that binding (no local of the eval code's own) }
procedure TCompiler.EvalDeclarations(Nodes: TList<TGocciaASTNode>; Names: TStringList; Strict: Boolean);
var I, Fl: Integer; All, VarOnly: TStringList; N: UnicodeString;
begin
  if Strict then Exit;   { strict eval code: its declarations stay its own }
  All := NewNameList;
  VarOnly := NewNameList;
  try
    for I := 0 to Nodes.Count - 1 do CollectVarNames(Nodes[I], VarOnly);
    All.AddStrings(Names);
    for I := 0 to Nodes.Count - 1 do
      if Nodes[I] is TGocciaFunctionDeclaration then All.Add(TGocciaFunctionDeclaration(Nodes[I]).Name);
    EvalSkipLocal := NewNameList;
    for I := 0 to All.Count - 1 do
    begin
      N := All[I];
      Fl := EvalOuterFlags(N);
      if Fl < 0 then
      begin
        if EvalVarEnv <> '' then
        begin
          if Names.IndexOf(N) >= 0 then Names.Delete(Names.IndexOf(N));
          EvalSkipLocal.Add(N);
          if EvalEnvNames = nil then EvalEnvNames := NewNameList;
          if EvalEnvNames.IndexOf(N) < 0 then EvalEnvNames.Add(N);
        end;
        Continue;
      end;
      if (Fl and efParamEnv) <> 0 then
        raise EEarlySyntaxError.Create('Identifier ''' + N + ''' has already been declared');
      if ((Fl and efLexical) <> 0) and ((Fl and efSameFunc) <> 0) then
      begin
        { a B.3.2 function name only stays unhoisted }
        if (AnnexBFuncs <> nil) and (Names.IndexOf(N) >= 0) and (VarOnly.IndexOf(N) < 0) then
        begin
          Names.Delete(Names.IndexOf(N));
          Continue;
        end;
        raise EEarlySyntaxError.Create('Identifier ''' + N + ''' has already been declared');
      end;
      if (Fl and efVarLevel) <> 0 then
      begin
        if Names.IndexOf(N) >= 0 then Names.Delete(Names.IndexOf(N));
        EvalSkipLocal.Add(N);
      end
      else if EvalVarEnv <> '' then
      begin
        { not the caller function's own: a binding of its eval variable environment }
        if Names.IndexOf(N) >= 0 then Names.Delete(Names.IndexOf(N));
        EvalSkipLocal.Add(N);
        if EvalEnvNames = nil then EvalEnvNames := NewNameList;
        if EvalEnvNames.IndexOf(N) < 0 then EvalEnvNames.Add(N);
      end;
    end;
  finally
    All.Free;
    VarOnly.Free;
  end;
end;

procedure TCompiler.CollectVarNames(N: TGocciaASTNode; Names: TStringList);
begin
  if (N <> nil) and (KindOf(VarNameKinds, VarNameClasses, N) < Length(VarNameClasses)) then
    CollectVarNamesOf(N, Names);
end;

procedure TCompiler.CollectVarNamesOf(N: TGocciaASTNode; Names: TStringList);
var I, K: Integer; VD: TGocciaVariableDeclaration;
  procedure AddPatternNames(P: TGocciaDestructuringPattern);
  var K: Integer;
  begin
    if P = nil then Exit;
    if P is TGocciaIdentifierDestructuringPattern then Names.Add(TGocciaIdentifierDestructuringPattern(P).Name)
    else if P is TGocciaAssignmentDestructuringPattern then AddPatternNames(TGocciaAssignmentDestructuringPattern(P).Left)
    else if P is TGocciaRestDestructuringPattern then AddPatternNames(TGocciaRestDestructuringPattern(P).Argument)
    else if P is TGocciaArrayDestructuringPattern then
    begin
      for K := 0 to TGocciaArrayDestructuringPattern(P).Elements.Count - 1 do
        AddPatternNames(TGocciaArrayDestructuringPattern(P).Elements[K]);
    end
    else if P is TGocciaObjectDestructuringPattern then
      for K := 0 to TGocciaObjectDestructuringPattern(P).Properties.Count - 1 do
        AddPatternNames(TGocciaObjectDestructuringPattern(P).Properties[K].Pattern);
  end;
begin
  if N = nil then Exit;
  if N is TGocciaVariableDeclaration then
  begin
    VD := TGocciaVariableDeclaration(N);
    if VD.IsVar then
      for I := 0 to High(VD.Variables) do
        if VD.Variables[I].IsPattern then AddPatternNames(VD.Variables[I].Pattern)
        else Names.Add(VD.Variables[I].Name);
  end
  else if N is TGocciaDestructuringDeclaration then
  begin
    if TGocciaDestructuringDeclaration(N).IsVar then AddPatternNames(TGocciaDestructuringDeclaration(N).Pattern);
  end
  else if N is TGocciaBlockStatement then
  begin
    for I := 0 to TGocciaBlockStatement(N).Nodes.Count - 1 do CollectVarNames(TGocciaBlockStatement(N).Nodes[I], Names);
  end
  else if N is TGocciaIfStatement then
  begin
    CollectVarNames(TGocciaIfStatement(N).Consequent, Names);
    CollectVarNames(TGocciaIfStatement(N).Alternate, Names);
  end
  else if N is TGocciaForStatement then
  begin
    CollectVarNames(TGocciaForStatement(N).Init, Names);
    CollectVarNames(TGocciaForStatement(N).Body, Names);
  end
  else if N is TGocciaWhileStatement then CollectVarNames(TGocciaWhileStatement(N).Body, Names)
  else if N is TGocciaDoWhileStatement then CollectVarNames(TGocciaDoWhileStatement(N).Body, Names)
  else if N is TGocciaForOfStatement then
  begin
    if TGocciaForOfStatement(N).IsVar then
    begin
      if TGocciaForOfStatement(N).BindingName <> '' then Names.Add(TGocciaForOfStatement(N).BindingName)
      else AddPatternNames(TGocciaForOfStatement(N).BindingPattern);
    end;
    CollectVarNames(TGocciaForOfStatement(N).Body, Names);
  end
  else if N is TGocciaForInStatement then
  begin
    if TGocciaForInStatement(N).IsVar then
    begin
      if TGocciaForInStatement(N).BindingName <> '' then Names.Add(TGocciaForInStatement(N).BindingName)
      else AddPatternNames(TGocciaForInStatement(N).BindingPattern);
    end;
    CollectVarNames(TGocciaForInStatement(N).Body, Names);
  end
  else if N is TGocciaTryStatement then
  begin
    CollectVarNames(TGocciaTryStatement(N).Block, Names);
    CollectVarNames(TGocciaTryStatement(N).CatchBlock, Names);
    CollectVarNames(TGocciaTryStatement(N).FinallyBlock, Names);
  end
  else if N is TGocciaSwitchStatement then
  begin
    for I := 0 to TGocciaSwitchStatement(N).Cases.Count - 1 do
      for K := 0 to TGocciaSwitchStatement(N).Cases[I].Consequent.Count - 1 do
        CollectVarNames(TGocciaSwitchStatement(N).Cases[I].Consequent[K], Names);
  end
  else if N is TGocciaWithStatement then CollectVarNames(TGocciaWithStatement(N).Body, Names);
end;

procedure TCompiler.DeclarePatternNames(P: TGocciaDestructuringPattern; IsConst: Boolean);
var K: Integer;
begin
  if P = nil then Exit;
  if P is TGocciaIdentifierDestructuringPattern then DeclareLocal(TGocciaIdentifierDestructuringPattern(P).Name, IsConst)
  else if P is TGocciaAssignmentDestructuringPattern then DeclarePatternNames(TGocciaAssignmentDestructuringPattern(P).Left, IsConst)
  else if P is TGocciaRestDestructuringPattern then DeclarePatternNames(TGocciaRestDestructuringPattern(P).Argument, IsConst)
  else if P is TGocciaArrayDestructuringPattern then
  begin
    for K := 0 to TGocciaArrayDestructuringPattern(P).Elements.Count - 1 do
      DeclarePatternNames(TGocciaArrayDestructuringPattern(P).Elements[K], IsConst);
  end
  else if P is TGocciaObjectDestructuringPattern then
    for K := 0 to TGocciaObjectDestructuringPattern(P).Properties.Count - 1 do
      DeclarePatternNames(TGocciaObjectDestructuringPattern(P).Properties[K].Pattern, IsConst);
end;

{ declare the block's lexical names up front, so closures created earlier in the block resolve them,
  and create hoisted function declarations first }
threadvar
  HoistMaskCache: TClassMaskCache;
const
  HoistMaskClasses: array[0..4] of TClass = (
    TGocciaVariableDeclaration,
    TGocciaDestructuringDeclaration,
    TGocciaClassDeclaration,
    TGocciaFunctionDeclaration,
    TGocciaUsingDeclaration);

procedure TCompiler.HoistBlock(L: TList<TGocciaASTNode>);
var I: Integer; N: TGocciaASTNode; VD: TGocciaVariableDeclaration; K: Integer; FD: TGocciaFunctionDeclaration;
  T: PFuncTemplate; R: Integer; HoistMask: Cardinal;
begin
  for I := 0 to L.Count - 1 do
  begin
    N := L[I];
    HoistMask := ClassMaskOf(HoistMaskCache, HoistMaskClasses, N);
    DeclaringLexical := (HoistMask and Cardinal(7 or 16)) <> 0;
    if (((HoistMask and Cardinal(1)) <> 0)) and not TGocciaVariableDeclaration(N).IsVar then
    begin
      VD := TGocciaVariableDeclaration(N);
      for K := 0 to High(VD.Variables) do
        if VD.Variables[K].IsPattern then DeclarePatternNames(VD.Variables[K].Pattern, VD.IsConst)
        else DeclareLocal(VD.Variables[K].Name, VD.IsConst);
    end
    else if (((HoistMask and Cardinal(2)) <> 0)) and not TGocciaDestructuringDeclaration(N).IsVar then
      DeclarePatternNames(TGocciaDestructuringDeclaration(N).Pattern, TGocciaDestructuringDeclaration(N).IsConst)
    else if ((HoistMask and Cardinal(4)) <> 0) then
      DeclareLocal(TGocciaClassDeclaration(N).ClassDefinition.Name, False)
    else if ((HoistMask and Cardinal(16)) <> 0) then
    begin
      { using and await using bindings: const, uninitialized until their declaration runs }
      for K := 0 to High(TGocciaUsingDeclaration(N).Variables) do
        DeclareLocal(TGocciaUsingDeclaration(N).Variables[K].Name, True);
    end
    else if ((HoistMask and Cardinal(8)) <> 0) and not GlobalTop and
            ((EvalSkipLocal = nil) or (EvalSkipLocal.IndexOf(TGocciaFunctionDeclaration(N).Name) < 0)) then
      DeclareLocal(TGocciaFunctionDeclaration(N).Name, False);
  end;
  DeclaringLexical := False;
  for I := 0 to L.Count - 1 do
  begin
    N := L[I];
    HoistMask := ClassMaskOf(HoistMaskCache, HoistMaskClasses, N);
    if ((HoistMask and Cardinal(8)) <> 0) then
    begin
      FD := TGocciaFunctionDeclaration(N);
      PendingStrict := Ord(FD.FunctionExpression.ParsedInStrictMode);
      NextSloppyEval := deuSloppyDirectEval in FD.FunctionExpression.DirectEvalUsage;
      T := CompileFunction(FD.Name, FD.FunctionExpression.Parameters, FD.FunctionExpression.Body, False, False, False,
        False, FD.FunctionExpression.IsGenerator, FD.FunctionExpression.IsAsync);
      T^.SourceRange := FD.FunctionExpression.SourceRange;
      if GlobalTop then
      begin
        { 16.1.7 step 16: CreateGlobalFunctionBinding, in declaration order (the last one of a name wins) }
        R := FS.FreeReg; AllocReg;
        EmitClosure(R, T);
        Emit(Ins(opDefGlobalFunc, R, Ord(Goal = 2), AtomConst(FD.Name)));
        FreeTo(R);
        Continue;
      end;
      if not ResolveLocal(FS, FD.Name, R) then
      begin
        { goal 4: a direct eval's function bound in its caller's variable environment }
        R := FS.FreeReg; AllocReg;
        EmitClosure(R, T);
        StoreIdent(FD.Name, R, True);
        FreeTo(R);
        Continue;
      end;
      EmitClosure(R, T);
    end;
  end;
end;

{ The declarations of a Script's top level (Goal 1) or of non-strict global eval code (Goal 2), recorded on
  the template for opDeclGlobals (ES2026 16.1.7 GlobalDeclarationInstantiation, 19.2.1.3), which runs first.
  var and function names are bindings of the global object; a Script's let, const and class stay registers of
  its frame, whose cells opMakeGLex publishes as global lexical bindings, so its own code keeps register
  access. Function objects are created and bound by HoistBlock (GlobalTop). }
procedure TCompiler.DeclareGlobals(T: PFuncTemplate; Nodes: TList<TGocciaASTNode>; VarNames: TStringList);
var I, K: Integer; N: TGocciaASTNode; Funcs: TStringList; VD: TGocciaVariableDeclaration;
  procedure AddLex(const Name: UnicodeString; IsConst: Boolean);
  begin
    SetLength(T^.GdiLex, Length(T^.GdiLex) + 1);
    T^.GdiLex[High(T^.GdiLex)] := AtomOf(Name);
    SetLength(T^.GdiLexConst, Length(T^.GdiLexConst) + 1);
    T^.GdiLexConst[High(T^.GdiLexConst)] := IsConst;
  end;
  procedure AddPatternLex(P: TGocciaDestructuringPattern; IsConst: Boolean);
  var K: Integer;
  begin
    if P = nil then Exit;
    if P is TGocciaIdentifierDestructuringPattern then AddLex(TGocciaIdentifierDestructuringPattern(P).Name, IsConst)
    else if P is TGocciaAssignmentDestructuringPattern then AddPatternLex(TGocciaAssignmentDestructuringPattern(P).Left, IsConst)
    else if P is TGocciaRestDestructuringPattern then AddPatternLex(TGocciaRestDestructuringPattern(P).Argument, IsConst)
    else if P is TGocciaArrayDestructuringPattern then
    begin
      for K := 0 to TGocciaArrayDestructuringPattern(P).Elements.Count - 1 do
        AddPatternLex(TGocciaArrayDestructuringPattern(P).Elements[K], IsConst);
    end
    else if P is TGocciaObjectDestructuringPattern then
      for K := 0 to TGocciaObjectDestructuringPattern(P).Properties.Count - 1 do
        AddPatternLex(TGocciaObjectDestructuringPattern(P).Properties[K].Pattern, IsConst);
  end;
begin
  T^.GdiKind := Goal;
  Funcs := TStringList.Create;
  try
    Funcs.UseLocale := False;   { exact code-unit order: a locale collation equates distinct names }
    Funcs.Sorted := True;
    Funcs.Duplicates := dupIgnore;
    Funcs.CaseSensitive := True;
    for I := 0 to Nodes.Count - 1 do
    begin
      N := Nodes[I];
      if N is TGocciaFunctionDeclaration then
      begin
        if Funcs.IndexOf(TGocciaFunctionDeclaration(N).Name) < 0 then
        begin
          Funcs.Add(TGocciaFunctionDeclaration(N).Name);
          SetLength(T^.GdiFuncs, Length(T^.GdiFuncs) + 1);
          T^.GdiFuncs[High(T^.GdiFuncs)] := AtomOf(TGocciaFunctionDeclaration(N).Name);
        end;
      end
      else if Goal = 1 then
      begin
        if (N is TGocciaVariableDeclaration) and not TGocciaVariableDeclaration(N).IsVar then
        begin
          VD := TGocciaVariableDeclaration(N);
          for K := 0 to High(VD.Variables) do
            if VD.Variables[K].IsPattern then AddPatternLex(VD.Variables[K].Pattern, VD.IsConst)
            else AddLex(VD.Variables[K].Name, VD.IsConst);
        end
        else if (N is TGocciaDestructuringDeclaration) and not TGocciaDestructuringDeclaration(N).IsVar then
          AddPatternLex(TGocciaDestructuringDeclaration(N).Pattern, TGocciaDestructuringDeclaration(N).IsConst)
        else if N is TGocciaClassDeclaration then
          AddLex(TGocciaClassDeclaration(N).ClassDefinition.Name, False);
      end;
    end;
    for I := 0 to VarNames.Count - 1 do
      if Funcs.IndexOf(VarNames[I]) < 0 then
      begin
        Funcs.Add(VarNames[I]);   { also dedupes the var names }
        SetLength(T^.GdiVars, Length(T^.GdiVars) + 1);
        T^.GdiVars[High(T^.GdiVars)] := AtomOf(VarNames[I]);
      end;
  finally
    Funcs.Free;
  end;
  Emit(Ins(opDeclGlobals, 0, 0, 0));
end;

procedure TCompiler.HoistFunctionsIn(Nodes: array of TGocciaASTNode);
begin
end;

{ ---------------- statements ---------------- }

procedure TCompiler.StmtNodes(L: TList<TGocciaASTNode>);
var I: Integer;
begin
  for I := 0 to L.Count - 1 do
    if L[I] is TGocciaUsingDeclaration then begin CompileUsingList(L, nil, True); Exit; end;
  for I := 0 to L.Count - 1 do Stmt(L[I]);
end;

procedure TCompiler.StmtList(L: TObjectList<TGocciaASTNode>);
var I: Integer;
begin
  for I := 0 to L.Count - 1 do
    if L[I] is TGocciaUsingDeclaration then begin CompileUsingList(TList<TGocciaASTNode>(L), nil); Exit; end;
  for I := 0 to L.Count - 1 do Stmt(L[I]);
end;

procedure TCompiler.StmtList(L: TObjectList<TGocciaStatement>);
var I: Integer;
begin
  for I := 0 to L.Count - 1 do
    if L[I] is TGocciaUsingDeclaration then begin CompileUsingList(nil, L); Exit; end;
  for I := 0 to L.Count - 1 do Stmt(L[I]);
end;

function TCompiler.NewICKey(Key: PStr): Integer;
var T: PFuncTemplate;
begin
  T := FS.Tpl;
  SetLength(T^.ICs, Length(T^.ICs) + 1);
  Result := High(T^.ICs);
  FillChar(T^.ICs[Result], SizeOf(TPropIC), 0);
  T^.ICs[Result].Key := Key;
  T^.ICs[Result].Slot := -1;
end;

{ 27.? DisposeResources of a using block: the helper (G2.Disposal) disposes the capability's resources in reverse
  order; ExcReg >= 0 is the block's exception (rethrown, or wrapped in SuppressedErrors), -1 a normal completion }
procedure TCompiler.EmitUsingDispose(Cap: Integer; Async: Boolean; ExcReg: Integer);
var B: Integer;
begin
  B := AllocRegs(5);
  if Async then Emit(Ins(opGetGlobal, B, 0, NewICKey(UsingKey(2))))
  else Emit(Ins(opGetGlobal, B, 0, NewICKey(UsingKey(1))));
  Emit(Ins(opLoadUndef, B + 1, 0, 0));
  Emit(Ins(opMov, B + 2, Cap, 0));
  if ExcReg >= 0 then
  begin
    Emit(Ins(opMov, B + 3, ExcReg, 0));
    Emit(Ins(opLoadTrue, B + 4, 0, 0));
  end
  else
  begin
    Emit(Ins(opLoadUndef, B + 3, 0, 0));
    Emit(Ins(opLoadFalse, B + 4, 0, 0));
  end;
  Emit(Ins(opCall, B, 3, 0));
  if Async then Emit(Ins(opAwait, B, B, 0));
  FreeTo(B);
end;

{ a statement list with using declarations (14.2.2 BlockDeclarationInstantiation + DisposeResources): its body
  runs in a finally-kind try whose capability every exit disposes }
procedure TCompiler.CompileUsingList(L: TList<TGocciaASTNode>; Stmts: TObjectList<TGocciaStatement>; Body: Boolean);
var Cap, ExcReg, Outer, J, I, SavedCap: Integer; Async: Boolean;
begin
  { 16.1.1 / 19.2.1.1: using is a SyntaxError at the top level of a Script or of eval code (modules allow it) }
  if Body and ((FS.Parent = nil) or FS.Parent.IsEvalOuter) and ((Goal in [1, 2]) or IsEvalCode) then raise EEarlySyntaxError.Create('using declarations are not allowed at the top level of a script');
  Async := False;
  if Assigned(L) then
  begin
    for I := 0 to L.Count - 1 do
      if (L[I] is TGocciaUsingDeclaration) and TGocciaUsingDeclaration(L[I]).IsAwait then Async := True;
  end
  else
    for I := 0 to Stmts.Count - 1 do
      if (Stmts[I] is TGocciaUsingDeclaration) and TGocciaUsingDeclaration(Stmts[I]).IsAwait then Async := True;
  Cap := AllocReg;
  ExcReg := AllocReg;
  Emit(Ins(opNewArr, Cap, 0, 0));
  if FS.TryCount >= Length(FS.Tries) then SetLength(FS.Tries, FS.TryCount * 2 + 4);
  FS.Tries[FS.TryCount].Block := nil;
  FS.Tries[FS.TryCount].IterReg := -1;
  FS.Tries[FS.TryCount].UsingCap := Cap;
  FS.Tries[FS.TryCount].UsingAsync := Async;
  FS.Tries[FS.TryCount].ScopeDepth := FS.ScopeCount;
  Inc(FS.TryCount);
  Outer := Emit(Ins(opTry, ExcReg, 1, 0));
  SavedCap := FS.UsingCapReg;
  FS.UsingCapReg := Cap;
  if Assigned(L) then
    for I := 0 to L.Count - 1 do Stmt(L[I])
  else
    for I := 0 to Stmts.Count - 1 do Stmt(Stmts[I]);
  FS.UsingCapReg := SavedCap;
  Dec(FS.TryCount);
  Emit(Ins(opEndTry, 0, 0, 0));
  EmitUsingDispose(Cap, Async, -1);
  J := EmitJ(opJmp, 0, 0);
  PatchHere(Outer);
  EmitUsingDispose(Cap, Async, ExcReg);
  Emit(Ins(opThrow, ExcReg, 0, 0));
  PatchHere(J);
  FreeTo(Cap);
end;

{ 14.3.1 using / await using: each binding is const; its value joins the block's capability
  (AddDisposableResource) }
procedure TCompiler.CompileUsingDecl(D: TGocciaUsingDeclaration);
var I, R: Integer; V: TGocciaVariableInfo;
begin
  if FS.UsingCapReg < 0 then raise EEarlySyntaxError.Create('using declarations are not allowed here');
  for I := 0 to High(D.Variables) do
  begin
    V := D.Variables[I];
    R := DeclareLocal(V.Name, True);
    if V.HasInitializer then ExprToNamed(V.Initializer, R, V.Name)
    else Emit(Ins(opLoadUndef, R, 0, 0));
    MarkInitialized(V.Name);
    AddUsingResource(R, D.IsAwait);
  end;
end;

{ 14.7.5.7 ForIn/OfBodyEvaluation with a using ForDeclaration: each iteration's binding is a resource of that
  iteration's capability, disposed when the iteration completes (continue and break included) }
procedure TCompiler.CompileForOfUsingBody(S: TGocciaForOfStatement; V: Integer);
var Cap, ExcReg, OuterTry, SavedCap, R: Integer;
begin
  BeginLoop(True);
  BeginUsingRegion(S.IsAwaitUsing, Cap, ExcReg, OuterTry, SavedCap);
  R := DeclareLocal(S.BindingName, True);
  Emit(Ins(opMov, R, V, 0));
  AddUsingResource(R, S.IsAwaitUsing);
  Stmt(S.Body);
  EndUsingRegion(Cap, ExcReg, OuterTry, SavedCap, S.IsAwaitUsing);
end;

{ AddDisposableResource(the innermost capability, R[R], sync-dispose or async-dispose) }
procedure TCompiler.AddUsingResource(R: Integer; Async: Boolean);
var B: Integer;
begin
  B := AllocRegs(5);
  Emit(Ins(opGetGlobal, B, 0, NewICKey(UsingKey(0))));
  Emit(Ins(opLoadUndef, B + 1, 0, 0));
  Emit(Ins(opMov, B + 2, FS.UsingCapReg, 0));
  Emit(Ins(opMov, B + 3, R, 0));
  if Async then Emit(Ins(opLoadTrue, B + 4, 0, 0)) else Emit(Ins(opLoadFalse, B + 4, 0, 0));
  Emit(Ins(opCall, B, 3, 0));
  FreeTo(B);
end;

{ a region whose dispose capability DisposeResources empties when it completes: normally, by a jump out (the
  try entry's UsingCap) or by an exception (disposed with it, then rethrown) }
procedure TCompiler.BeginUsingRegion(Async: Boolean; out Cap, ExcReg, OuterTry, SavedCap: Integer);
begin
  Cap := AllocReg;
  ExcReg := AllocReg;
  Emit(Ins(opNewArr, Cap, 0, 0));
  if FS.TryCount >= Length(FS.Tries) then SetLength(FS.Tries, FS.TryCount * 2 + 4);
  FS.Tries[FS.TryCount].Block := nil;
  FS.Tries[FS.TryCount].IterReg := -1;
  FS.Tries[FS.TryCount].UsingCap := Cap;
  FS.Tries[FS.TryCount].UsingAsync := Async;
  FS.Tries[FS.TryCount].ScopeDepth := FS.ScopeCount;
  Inc(FS.TryCount);
  OuterTry := Emit(Ins(opTry, ExcReg, 1, 0));
  SavedCap := FS.UsingCapReg;
  FS.UsingCapReg := Cap;
end;

procedure TCompiler.EndUsingRegion(Cap, ExcReg, OuterTry, SavedCap: Integer; Async: Boolean);
var J: Integer;
begin
  FS.UsingCapReg := SavedCap;
  Dec(FS.TryCount);
  Emit(Ins(opEndTry, 0, 0, 0));
  EmitUsingDispose(Cap, Async, -1);
  J := EmitJ(opJmp, 0, 0);
  PatchHere(OuterTry);
  EmitUsingDispose(Cap, Async, ExcReg);
  Emit(Ins(opThrow, ExcReg, 0, 0));
  PatchHere(J);
end;

procedure TCompiler.BeginLoop(IsLoop: Boolean);
begin
  if FS.LoopCount >= Length(FS.Loops) then SetLength(FS.Loops, FS.LoopCount * 2 + 8);
  with FS.Loops[FS.LoopCount] do
  begin
    Labels := FS.PendingLabels;
    SetLength(BreakJumps, 0);
    SetLength(ContJumps, 0);
    ScopeDepth := FS.ScopeCount;
    TryDepth := FS.TryCount;
  end;
  FS.Loops[FS.LoopCount].IsLoop := IsLoop;
  FS.PendingLabels := nil;
  Inc(FS.LoopCount);
end;

procedure TCompiler.EndLoop(ContTarget, BreakTarget: Integer);
var I: Integer;
begin
  Dec(FS.LoopCount);
  with FS.Loops[FS.LoopCount] do
  begin
    for I := 0 to High(ContJumps) do PatchTo(ContJumps[I], ContTarget);
    for I := 0 to High(BreakJumps) do PatchTo(BreakJumps[I], BreakTarget);
    FreeAndNil(Labels);
  end;
end;

procedure TCompiler.EmitFinallyUnwind(ToTryDepth: Integer);
var I: Integer; Saved, T: Integer;
begin
  for I := FS.TryCount - 1 downto ToTryDepth do
  begin
    Emit(Ins(opEndTry, 0, 0, 0));
    { 14.7.5.7 ForIn/OfBodyEvaluation: a break, continue or return out of the loop closes its iterator }
    if FS.Tries[I].IterReg >= 0 then Emit(Ins(opIterClose, FS.Tries[I].IterReg, 0, 0))
    else if FS.Tries[I].IterReg <= -2 then
    begin
      T := AllocReg;
      EmitAsyncIterClose(-2 - FS.Tries[I].IterReg, T);
      FreeTo(T);
    end;
    if Assigned(FS.Tries[I].Block) then
    begin
      Saved := FS.TryCount;
      FS.TryCount := I;   { a jump inside the finally body must not re-run it }
      FinallyStmt(FS.Tries[I].Block);
      FS.TryCount := Saved;
    end;
    if FS.Tries[I].UsingCap >= 0 then
    begin
      Saved := FS.TryCount;
      FS.TryCount := I;
      EmitUsingDispose(FS.Tries[I].UsingCap, FS.Tries[I].UsingAsync, -1);
      FS.TryCount := Saved;
    end;
  end;
end;

procedure TCompiler.CompileBreakContinue(IsBreak: Boolean; const Lbl: UnicodeString);
var I, J: Integer; Found: Integer;
begin
  Found := -1;
  for I := FS.LoopCount - 1 downto 0 do
  begin
    if Lbl = '' then
    begin
      if IsBreak or FS.Loops[I].IsLoop then begin Found := I; Break; end;
    end
    else if Assigned(FS.Loops[I].Labels) and (FS.Loops[I].Labels.IndexOf(Lbl) >= 0) then
    begin
      Found := I; Break;
    end;
  end;
  { 14.8.1 / 14.9.1: a break or continue without a target (or across a function or static block) is an early
    error }
  if Found < 0 then raise EEarlySyntaxError.Create('Illegal break or continue statement: no such target');
  EmitFinallyUnwind(FS.Loops[Found].TryDepth);
  EmitCloseFrom(FS.Loops[Found].ScopeDepth);
  J := EmitJ(opJmp, 0, 0);
  if IsBreak then AddJump(FS.Loops[Found].BreakJumps, J) else AddJump(FS.Loops[Found].ContJumps, J);
end;

{ a finally block's normal completion leaves the statement's completion value as it was (14.15.3) }
procedure TCompiler.FinallyStmt(B: TGocciaASTNode);
var Sv: Integer;
begin
  if FS.CplP1 = 0 then begin Stmt(B); Exit; end;
  Sv := AllocReg;
  Emit(Ins(opMov, Sv, FS.CplP1 - 1, 0));
  Stmt(B);
  Emit(Ins(opMov, FS.CplP1 - 1, Sv, 0));
  FreeTo(Sv);
end;

procedure TCompiler.Stmt(S: TGocciaASTNode);
begin
  if S = nil then Exit;
  FS.CurLine := S.Line;
  if (S is TGocciaStatement) and (TGocciaStatement(S).LabelCount > 0) then
    StmtLabeled(TGocciaStatement(S))
  else
    StmtCore(S);
end;

{ out of line: reading the labels makes string temporaries, an exception
  frame for every statement if it sat in Stmt }
procedure TCompiler.StmtLabeled(St: TGocciaStatement);
var I: Integer; Lbls: TStringList;
begin
  { a label named arguments is not an IdentifierReference: only the reserved-word rules apply to labels }
  for I := 0 to St.LabelCount - 1 do
    if St.Labels[I] <> 'arguments' then
    begin
      CheckIdent(St.Labels[I]);
      { await is a reserved word in module code (13.1.1) }
      if (Goal = 3) and (St.Labels[I] = 'await') then raise EEarlySyntaxError.Create('Unexpected reserved word ''await''');
    end;
  { 14.13.1 / B.3.1: only a plain function declaration can be labelled, and only in non-strict code }
  if (St is TGocciaFunctionDeclaration) and (FS.Tpl^.IsStrict or TGocciaFunctionDeclaration(St).FunctionExpression.IsAsync or
     TGocciaFunctionDeclaration(St).FunctionExpression.IsGenerator) then
    raise EEarlySyntaxError.Create('Labelled function declarations are only allowed for plain functions in non-strict code');
  Lbls := NewNameList;
  for I := 0 to St.LabelCount - 1 do Lbls.Add(St.Labels[I]);
  FS.PendingLabels := Lbls;
  if (St is TGocciaForStatement) or (St is TGocciaWhileStatement) or (St is TGocciaDoWhileStatement) or
     (St is TGocciaForOfStatement) or (St is TGocciaForInStatement) then
    StmtCore(St)
  else
  begin
    BeginLoop(False);
    StmtCore(St);
    EndLoop(Here, Here);
  end;
end;

threadvar
  StmtKinds: TKindCache;
const
  StmtClasses: array[0..19] of TClass = (TGocciaExpressionStatement, TGocciaVariableDeclaration,
    TGocciaDestructuringDeclaration, TGocciaFunctionDeclaration, TGocciaBlockStatement, TGocciaIfStatement,
    TGocciaForStatement, TGocciaWhileStatement, TGocciaDoWhileStatement, TGocciaForOfStatement,
    TGocciaForInStatement, TGocciaReturnStatement, TGocciaThrowStatement, TGocciaTryStatement,
    TGocciaClassDeclaration, TGocciaSwitchStatement, TGocciaBreakStatement, TGocciaContinueStatement,
    TGocciaEmptyStatement, TGocciaASTNode);

procedure TCompiler.StmtCore(S: TGocciaASTNode);
var R, K: Integer; T: TGocciaThrowStatement;
begin
  K := KindOf(StmtKinds, StmtClasses, S);
  if FS.CplP1 <> 0 then
  begin
    { completion values (UpdateEmpty): an expression statement's value replaces the script's; if, loops, switch, try and
      with complete with UpdateEmpty(C, undefined), so their value is undefined until a statement in them sets it }
    if (K = 0) and FS.CplSkip then FS.CplSkip := False
    else if K = 0 then
    begin
      ExprTo(TGocciaExpressionStatement(S).Expression, FS.CplP1 - 1);
      Exit;
    end;
    if (K in [5..10, 13, 15]) or (S is TGocciaWithStatement) then Emit(Ins(opLoadUndef, FS.CplP1 - 1, 0, 0));
  end;
  case K of
    0: begin
         R := FS.FreeReg;
         TopDiscard := IsDiscardable(TGocciaExpressionStatement(S).Expression);
         ExprAny(TGocciaExpressionStatement(S).Expression);
         TopDiscard := False;
         FreeTo(R);
       end;
    1: CompileVarDecl(TGocciaVariableDeclaration(S));
    2: begin
         R := FS.FreeReg;
         AllocReg;
         ExprTo(TGocciaDestructuringDeclaration(S).Initializer, R);
         BindPattern(TGocciaDestructuringDeclaration(S).Pattern, R, True, TGocciaDestructuringDeclaration(S).IsConst);
         FreeTo(R);
       end;
    3: if (AnnexBFuncs <> nil) and AnnexBFuncs.ContainsKey(S) then EmitAnnexBCopy(TGocciaFunctionDeclaration(S));
    4: begin
         PushScope;
         HoistBlock(TList<TGocciaASTNode>(TGocciaBlockStatement(S).Nodes));
         StmtList(TGocciaBlockStatement(S).Nodes);
         PopScope;
       end;
    5: CompileIf(TGocciaIfStatement(S));
    6: CompileFor(TGocciaForStatement(S));
    7: CompileWhile(TGocciaWhileStatement(S));
    8: CompileDoWhile(TGocciaDoWhileStatement(S));
    9: CompileForOf(TGocciaForOfStatement(S));
    10: CompileForIn(TGocciaForInStatement(S));
    11: CompileReturn(TGocciaReturnStatement(S));
    12: begin
          T := TGocciaThrowStatement(S);
          R := FS.FreeReg;
          AllocReg;
          ExprTo(T.Value, R);
          Emit(Ins(opThrow, R, 0, 0));
          FreeTo(R);
        end;
    13: CompileTry(TGocciaTryStatement(S));
    14: begin
          ResolveLocal(FS, TGocciaClassDeclaration(S).ClassDefinition.Name, R);
          CompileClass(TGocciaClassDeclaration(S).ClassDefinition, R);
          MarkInitialized(TGocciaClassDeclaration(S).ClassDefinition.Name);
        end;
    15: CompileSwitch(TGocciaSwitchStatement(S));
    16: CompileBreakContinue(True, TGocciaBreakStatement(S).TargetLabel);
    17: CompileBreakContinue(False, TGocciaContinueStatement(S).TargetLabel);
    18: ;
  else
    if S is TGocciaUsingDeclaration then CompileUsingDecl(TGocciaUsingDeclaration(S))
    else if S is TGocciaWithStatement then CompileWith(TGocciaWithStatement(S))
    else if S is TGocciaExportDefaultDeclaration then
    begin
      { 16.2.3.7: export default: the binding *default* (or the declaration's name) gets the value; a function
        declaration was made by the prologue }
      with TGocciaExportDefaultDeclaration(S) do
        if not (IsDirectDeclaration and (Expression is TGocciaFunctionExpression)) then
        begin
          ResolveLocal(FS, LocalName, R);
          if IsDirectDeclaration and (Expression is TGocciaClassExpression) then
            CompileClass(TGocciaClassExpression(Expression).ClassDefinition, R, 'default')
          else ExprToNamed(Expression, R, 'default');
          MarkInitialized(LocalName);
        end;
    end
    else RaiseUnsupported('statement', S);
  end;
end;

procedure TCompiler.CompileVarDecl(D: TGocciaVariableDeclaration);
var I, R, T: Integer; V: TGocciaVariableInfo;
begin
  for I := 0 to High(D.Variables) do
  begin
    V := D.Variables[I];
    if V.IsPattern then
    begin
      T := FS.FreeReg; AllocReg;
      ExprTo(V.Initializer, T);
      BindPattern(V.Pattern, T, not D.IsVar, D.IsConst);
      FreeTo(T);
      Continue;
    end;
    if D.IsVar and V.HasInitializer and (WithsFor(V.Name, WithIdxScratch) > 0) then
    begin
      { 14.3.2.1: the initializer goes to ResolveBinding(name), which a with object around it can hold }
      T := FS.FreeReg; AllocReg;
      CompileFieldValue(V.Initializer, T, V.Name);
      StoreIdent(V.Name, T, False);
      FreeTo(T);
      Continue;
    end;
    if not D.IsVar then R := DeclareLocal(V.Name, D.IsConst)
    else if not ResolveLocal(FS, V.Name, R) then
    begin
      if D.IsVar then
      begin
        { a var of the global environment: `var x;` leaves the binding's value alone }
        if not V.HasInitializer then Continue;
        T := FS.FreeReg; AllocReg;
        if (V.Initializer is TGocciaFunctionExpression) or (V.Initializer is TGocciaArrowFunctionExpression) then
          CompileFuncExpr(V.Initializer, T, V.Name)
        else if V.Initializer is TGocciaClassExpression then
          CompileClass(TGocciaClassExpression(V.Initializer).ClassDefinition, T, V.Name)
        else
          ExprTo(V.Initializer, T);
        StoreIdent(V.Name, T, True);
        FreeTo(T);
        Continue;
      end;
      R := DeclareLocal(V.Name, D.IsConst);
    end;
    if V.HasInitializer then
    begin
      if (V.Initializer is TGocciaFunctionExpression) or (V.Initializer is TGocciaArrowFunctionExpression) then
        CompileFuncExpr(V.Initializer, R, V.Name)
      else if V.Initializer is TGocciaClassExpression then
      begin
        CompileClass(TGocciaClassExpression(V.Initializer).ClassDefinition, R, V.Name);
      end
      else
        ExprTo(V.Initializer, R);
    end
    else if not D.IsVar then
      Emit(Ins(opLoadUndef, R, 0, 0));
    if not D.IsVar then MarkInitialized(V.Name);
  end;
end;

procedure TCompiler.CompileIf(S: TGocciaIfStatement);
var FalseL: TJumpList; J: Integer;
  { B.3.3: a function declaration as a clause is a block of its own }
  procedure ClauseStmt(C: TGocciaASTNode);
  var One: TList<TGocciaASTNode>;
  begin
    if not (C is TGocciaFunctionDeclaration) then begin Stmt(C); Exit; end;
    One := TList<TGocciaASTNode>.Create;
    try
      One.Add(C);
      PushScope;
      HoistBlock(One);
      Stmt(C);
      PopScope;
    finally
      One.Free;
    end;
  end;
begin
  SetLength(FalseL, 0);
  CheckStatementBody(S.Consequent, True, FS.Tpl^.IsStrict);
  CheckStatementBody(S.Alternate, True, FS.Tpl^.IsStrict);
  BranchFalse(S.Condition, FalseL);
  ClauseStmt(S.Consequent);
  if Assigned(S.Alternate) then
  begin
    J := EmitJ(opJmp, 0, 0);
    PatchListHere(FalseL);
    ClauseStmt(S.Alternate);
    PatchHere(J);
  end
  else
    PatchListHere(FalseL);
end;

procedure TCompiler.CompileFor(S: TGocciaForStatement);
var Top, ContTarget, BreakTarget, R0, InitClose: Integer; FalseL: TJumpList; HeadDepth: Integer; HasCond: Boolean;
  HL: TList<TGocciaASTNode>; UsingFor, Async: Boolean; Cap, ExcReg, OuterTry, SavedCap: Integer;
begin
  CheckStatementBody(S.Body, False, FS.Tpl^.IsStrict);
  { 14.7.4.2 a using declaration in the head: the loop runs in a region whose capability DisposeResources
    empties when the for statement completes (the bindings are const: no per-iteration copies) }
  UsingFor := S.Init is TGocciaUsingDeclaration;
  Async := False; Cap := -1; ExcReg := -1; OuterTry := -1; SavedCap := -1;
  if UsingFor then
  begin
    Async := TGocciaUsingDeclaration(S.Init).IsAwait;
    BeginUsingRegion(Async, Cap, ExcReg, OuterTry, SavedCap);
  end;
  { the capability's registers sit below the head scope, whose update frees down to its bindings }
  PushScope;
  HeadDepth := FS.ScopeCount;
  { the head's let / const names first, as a block's are: a destructuring initializer's temporaries then sit
    above them }
  if Assigned(S.Init) and
     (((S.Init is TGocciaVariableDeclaration) and not TGocciaVariableDeclaration(S.Init).IsVar) or
      ((S.Init is TGocciaDestructuringDeclaration) and not TGocciaDestructuringDeclaration(S.Init).IsVar) or
      UsingFor) then
  begin
    HL := TList<TGocciaASTNode>.Create;
    try
      HL.Add(S.Init);
      HoistBlock(HL);
    finally
      HL.Free;
    end;
  end;
  { an init expression is not an expression statement: it has no completion value }
  if S.Init is TGocciaExpressionStatement then
  begin
    R0 := FS.FreeReg;
    TopDiscard := IsDiscardable(TGocciaExpressionStatement(S.Init).Expression);
    ExprAny(TGocciaExpressionStatement(S.Init).Expression);
    TopDiscard := False;
    FreeTo(R0);
  end
  else if Assigned(S.Init) then Stmt(S.Init);
  { 14.7.4.2 step 3 CreatePerIterationEnvironment before the first test: closures the head made keep its
    bindings, the first iteration gets copies (an opClose once the scope turns out captured) }
  InitClose := Emit(Ins(opNop, 0, 0, 0));
  Top := Here;
  SetLength(FalseL, 0);
  HasCond := Assigned(S.Condition);
  if HasCond then BranchFalse(S.Condition, FalseL);
  BeginLoop(True);
  Stmt(S.Body);
  ContTarget := Here;
  { per-iteration bindings: close the head scope's captures before the update }
  if FS.Scopes[HeadDepth - 1].Captured then
  begin
    Emit(Ins(opClose, FS.Scopes[HeadDepth - 1].BaseReg, 0, 0));
    FS.Tpl^.Code[InitClose] := Ins(opClose, FS.Scopes[HeadDepth - 1].BaseReg, 0, 0);
  end;
  if Assigned(S.Update) then
  begin
    TopDiscard := IsDiscardable(S.Update);
    ExprAny(S.Update);
    TopDiscard := False;
    FreeTo(FS.Scopes[FS.ScopeCount - 1].BaseReg + FS.Scopes[FS.ScopeCount - 1].Count);
  end;
  PatchTo(EmitJ(opJmp, 0, 0), Top);
  BreakTarget := Here;
  PatchListHere(FalseL);
  EndLoop(ContTarget, BreakTarget);
  if UsingFor then EndUsingRegion(Cap, ExcReg, OuterTry, SavedCap, Async);
  PopScope;
  if UsingFor then FreeTo(Cap);
end;

procedure TCompiler.CompileWhile(S: TGocciaWhileStatement);
var Top: Integer; FalseL: TJumpList;
begin
  CheckStatementBody(S.Body, False, FS.Tpl^.IsStrict);
  Top := Here;
  SetLength(FalseL, 0);
  BranchFalse(S.Condition, FalseL);
  BeginLoop(True);
  Stmt(S.Body);
  PatchTo(EmitJ(opJmp, 0, 0), Top);
  PatchListHere(FalseL);
  EndLoop(Top, Here);
end;

procedure TCompiler.CompileDoWhile(S: TGocciaDoWhileStatement);
var Top, Cont: Integer; TrueL: TJumpList; I: Integer;
begin
  CheckStatementBody(S.Body, False, FS.Tpl^.IsStrict);
  Top := Here;
  BeginLoop(True);
  Stmt(S.Body);
  Cont := Here;
  SetLength(TrueL, 0);
  BranchTrue(S.Condition, TrueL);
  for I := 0 to High(TrueL) do PatchTo(TrueL[I], Top);
  EndLoop(Cont, Here);
end;

{ The loop runs inside a handler: an exception from the binding or the body closes the iterator (return()
  runs, its errors ignored) and is rethrown; one from next() leaves the iterator done (opIterNext clears it),
  so nothing is closed. A break out of this loop reaches the close after the loop; a jump further out, or a
  return, closes it through the Tries entry (EmitFinallyUnwind). Exhaustion closes nothing. }
{ 14.7.5.6 ForIn/OfHeadEvaluation step 2: a lexical declaration's names are uninitialized bindings of the scope the
  expression runs in }
procedure TCompiler.ForHeadExpr(E: TGocciaExpression; Dest: Integer; IsVar: Boolean; const BName: UnicodeString;
  Pat: TGocciaDestructuringPattern);
var L: TStringList; I: Integer;
begin
  if IsVar or ((BName = '') and (Pat = nil)) then begin ExprTo(E, Dest); Exit; end;
  PushScope;
  DeclaringLexical := True;
  try
    if BName <> '' then DeclareLocal(BName, False)
    else
    begin
      L := NewNameList;
      try
        PatternNames(Pat, L);
        for I := 0 to L.Count - 1 do DeclareLocal(L[I], False);
      finally
        L.Free;
      end;
    end;
  finally
    DeclaringLexical := False;
  end;
  ExprTo(E, Dest);
  PopScope;
end;

procedure TCompiler.CompileForOf(S: TGocciaForOfStatement);
var It, V, Top, Exit_, R, Exc, H, JDone, JBreak: Integer;
begin
  CheckStatementBody(S.Body, False, FS.Tpl^.IsStrict);
  if S is TGocciaForAwaitOfStatement then begin CompileForAwaitOf(S); Exit; end;
  PushScope;
  It := AllocReg;
  AllocReg;   { the iterator record's next method }
  V := AllocReg;
  Exc := AllocReg;
  ForHeadExpr(S.Iterable, It, S.IsVar, S.BindingName, S.BindingPattern);
  Emit(Ins(opGetIter, It, It, 0));
  if FS.TryCount >= Length(FS.Tries) then SetLength(FS.Tries, FS.TryCount * 2 + 4);
  FS.Tries[FS.TryCount].Block := nil;
  FS.Tries[FS.TryCount].ScopeDepth := FS.ScopeCount;
  FS.Tries[FS.TryCount].IterReg := It;
  FS.Tries[FS.TryCount].UsingCap := -1;
  Inc(FS.TryCount);
  H := Emit(Ins(opTry, Exc, 1, 0));   { kind 1: cleanup, run by a generator's return completion too }
  Top := Here;
  Exit_ := Emit(Ins(opIterNext, V, It, 0));
  PushScope;
  if (S.IsUsing or S.IsAwaitUsing) and (S.BindingName <> '') then CompileForOfUsingBody(S, V)
  else
  begin
    if S.BindingName <> '' then
    begin
      if S.IsVar then StoreIdent(S.BindingName, V)
      else
      begin
        R := DeclareLocal(S.BindingName, S.IsConst);
        Emit(Ins(opMov, R, V, 0));
      end;
    end
    else if Assigned(S.BindingPattern) then
      BindPattern(S.BindingPattern, V, not S.IsVar, S.IsConst)
    else if Assigned(S.AssignmentTarget) then
      BindPattern(S.AssignmentTarget, V, False, False);
    BeginLoop(True);
    Stmt(S.Body);
  end;
  PopScope;
  PatchTo(EmitJ(opJmp, 0, 0), Top);
  Dec(FS.TryCount);
  { break: close, then leave }
  JBreak := Here;
  Emit(Ins(opEndTry, 0, 0, 0));
  Emit(Ins(opIterClose, It, 0, 0));
  JDone := EmitJ(opJmp, 0, 0);
  EndLoop(Top, JBreak);
  { exhausted }
  PatchHere(Exit_);
  Emit(Ins(opEndTry, 0, 0, 0));
  R := EmitJ(opJmp, 0, 0);
  { an exception in the loop }
  PatchHere(H);
  Emit(Ins(opIterCloseThrow, It, Exc, 1));
  Emit(Ins(opThrow, Exc, 0, 0));
  PatchHere(JDone);
  PatchHere(R);
  PopScope;
end;

{ 7.4.13 AsyncIteratorClose for a normal completion: return() called and awaited, its result an object }
procedure TCompiler.EmitAsyncIterClose(It, Tmp: Integer);
var J: Integer;
begin
  J := EmitJ(opAsyncIterReturn, Tmp, It);
  Emit(Ins(opAwait, Tmp, Tmp, 0));
  Emit(Ins(opIterCheckObj, Tmp, 0, 0));
  PatchHere(J);
  Emit(Ins(opLoadUndef, It, 0, 0));
end;

{ 14.7.5.6 ForIn/OfHeadEvaluation with async iteration, 14.7.5.7 ForIn/OfBodyEvaluation: each step calls
  next() and awaits the result; while that is under way the record sits in a scratch register, so an abrupt
  next() or a rejected await leaves nothing to close. A break closes the iterator (return() awaited); an
  exception from the body closes it quietly and is rethrown. }
procedure TCompiler.CompileForAwaitOf(S: TGocciaForOfStatement);
var It, Sv, V, Top, Exit_, R, Exc, Exc2, H, H2, JDone, JBreak, JQ: Integer;
begin
  PushScope;
  It := AllocReg;
  AllocReg;   { next }
  Sv := AllocReg;
  V := AllocReg;
  Exc := AllocReg;
  Exc2 := AllocReg;
  ForHeadExpr(S.Iterable, It, S.IsVar, S.BindingName, S.BindingPattern);
  Emit(Ins(opGetAsyncIter, It, It, 0));
  if FS.TryCount >= Length(FS.Tries) then SetLength(FS.Tries, FS.TryCount * 2 + 4);
  FS.Tries[FS.TryCount].Block := nil;
  FS.Tries[FS.TryCount].ScopeDepth := FS.ScopeCount;
  FS.Tries[FS.TryCount].IterReg := -2 - It;   { an async iterator: closed with EmitAsyncIterClose }
  FS.Tries[FS.TryCount].UsingCap := -1;
  Inc(FS.TryCount);
  H := Emit(Ins(opTry, Exc, 1, 0));
  Top := Here;
  Emit(Ins(opIterCallNext, V, It, 0));
  Emit(Ins(opMov, Sv, It, 0));
  Emit(Ins(opLoadUndef, It, 0, 0));
  Emit(Ins(opAwait, V, V, 0));
  Exit_ := EmitJ(opIterUnpack, V, Sv);
  Emit(Ins(opMov, It, Sv, 0));
  PushScope;
  if (S.IsUsing or S.IsAwaitUsing) and (S.BindingName <> '') then CompileForOfUsingBody(S, V)
  else
  begin
    if S.BindingName <> '' then
    begin
      if S.IsVar then StoreIdent(S.BindingName, V)
      else
      begin
        R := DeclareLocal(S.BindingName, S.IsConst);
        Emit(Ins(opMov, R, V, 0));
      end;
    end
    else if Assigned(S.BindingPattern) then
      BindPattern(S.BindingPattern, V, not S.IsVar, S.IsConst)
    else if Assigned(S.AssignmentTarget) then
      BindPattern(S.AssignmentTarget, V, False, False);
    BeginLoop(True);
    Stmt(S.Body);
  end;
  PopScope;
  PatchTo(EmitJ(opJmp, 0, 0), Top);
  Dec(FS.TryCount);
  JBreak := Here;
  Emit(Ins(opEndTry, 0, 0, 0));
  EmitAsyncIterClose(It, Sv);
  JDone := EmitJ(opJmp, 0, 0);
  EndLoop(Top, JBreak);
  PatchHere(Exit_);
  Emit(Ins(opEndTry, 0, 0, 0));
  R := EmitJ(opJmp, 0, 0);
  { an exception in the loop: return() called and awaited, its own failure ignored, then the rethrow }
  PatchHere(H);
  H2 := Emit(Ins(opTry, Exc2, 0, 0));
  JQ := EmitJ(opAsyncIterReturn, Sv, It);
  Emit(Ins(opAwait, Sv, Sv, 0));
  PatchHere(JQ);
  Emit(Ins(opEndTry, 0, 0, 0));
  PatchHere(H2);
  Emit(Ins(opThrow, Exc, 0, 0));
  PatchHere(JDone);
  PatchHere(R);
  PopScope;
end;

procedure TCompiler.CompileForIn(S: TGocciaForInStatement);
var It, V, Top, Exit_, R, ObjR, Chk: Integer;
begin
  CheckStatementBody(S.Body, False, FS.Tpl^.IsStrict);
  PushScope;
  It := AllocReg;
  AllocReg;   { opForIn's next method: the key iterator steps without a call }
  V := AllocReg;
  ObjR := AllocReg;
  Chk := AllocReg;
  ForHeadExpr(S.ObjectExpression, It, S.IsVar, S.BindingName, S.BindingPattern);
  Emit(Ins(opForIn, It, It, ObjR));
  Top := Here;
  Exit_ := Emit(Ins(opIterNext, V, It, 0));
  { 14.7.5.9: a property deleted before its turn is not visited }
  Emit(Ins(opIn, Chk, V, ObjR));
  PatchTo(EmitJ(opJF, Chk, 0), Top);
  PushScope;
  if S.BindingName <> '' then
  begin
    if S.IsVar then StoreIdent(S.BindingName, V)
    else
    begin
      R := DeclareLocal(S.BindingName, S.IsConst);
      Emit(Ins(opMov, R, V, 0));
    end;
  end
  else if Assigned(S.BindingPattern) then
    BindPattern(S.BindingPattern, V, not S.IsVar, S.IsConst)
  else if Assigned(S.AssignmentTarget) then
    BindPattern(S.AssignmentTarget, V, False, False);
  BeginLoop(True);
  Stmt(S.Body);
  PopScope;
  PatchTo(EmitJ(opJmp, 0, 0), Top);
  PatchHere(Exit_);
  EndLoop(Top, Here);
  PopScope;
end;

procedure TCompiler.CompileSwitch(S: TGocciaSwitchStatement);
var D, T, I, K, DefaultIdx: Integer; CaseJumps: array of Integer; JDefault: Integer; Bodies: array of Integer;
  Nodes: TList<TGocciaASTNode>;
begin
  PushScope;
  FS.Scopes[FS.ScopeCount - 1].IsSwitch := True;
  D := AllocReg;
  ExprTo(S.Discriminant, D);
  { 14.12.4: the case block's declarations are instantiated after the discriminant, before the case tests
    (which see them): lexical holes and function declarations come first }
  Nodes := TList<TGocciaASTNode>.Create;
  DefaultIdx := -1;
  for I := 0 to S.Cases.Count - 1 do
  begin
    { 14.12.1: at most one default clause; 14.12.1 (explicit resource management): no using declaration directly
      in a clause's statement list }
    if S.Cases[I].Test = nil then
    begin
      if DefaultIdx >= 0 then begin Nodes.Free; raise EEarlySyntaxError.Create('More than one default clause in switch statement'); end;
      DefaultIdx := I;
    end;
    for K := 0 to S.Cases[I].Consequent.Count - 1 do
    begin
      if S.Cases[I].Consequent[K] is TGocciaUsingDeclaration then
      begin
        Nodes.Free;
        raise EEarlySyntaxError.Create('using declarations are not allowed directly in a switch case clause');
      end;
      Nodes.Add(S.Cases[I].Consequent[K]);
    end;
  end;
  HoistBlock(Nodes);
  Nodes.Free;
  SetLength(CaseJumps, S.Cases.Count);
  DefaultIdx := -1;
  for I := 0 to S.Cases.Count - 1 do
  begin
    if S.Cases[I].Test = nil then begin DefaultIdx := I; CaseJumps[I] := -1; Continue; end;
    T := FS.FreeReg; AllocReg;
    ExprTo(S.Cases[I].Test, T);
    CaseJumps[I] := Emit(Ins(opJSEq, D, T, 0));
    FreeTo(T);
  end;
  JDefault := EmitJ(opJmp, 0, 0);
  BeginLoop(False);
  SetLength(Bodies, S.Cases.Count);
  for I := 0 to S.Cases.Count - 1 do
  begin
    Bodies[I] := Here;
    if CaseJumps[I] >= 0 then PatchHere(CaseJumps[I]);
    if I = DefaultIdx then PatchHere(JDefault);
    StmtList(S.Cases[I].Consequent);
  end;
  if DefaultIdx < 0 then PatchHere(JDefault);
  EndLoop(Here, Here);
  PopScope;
end;

procedure TCompiler.CompileTry(S: TGocciaTryStatement);
var Outer, Inner, ExcReg, JEnd, JEnd2, R: Integer; HasFinally: Boolean;
begin
  HasFinally := Assigned(S.FinallyBlock);
  ExcReg := AllocReg;   { kept for the whole statement }
  if HasFinally then
  begin
    if FS.TryCount >= Length(FS.Tries) then SetLength(FS.Tries, FS.TryCount * 2 + 4);
    FS.Tries[FS.TryCount].Block := S.FinallyBlock;
    FS.Tries[FS.TryCount].IterReg := -1;
    FS.Tries[FS.TryCount].UsingCap := -1;
    FS.Tries[FS.TryCount].ScopeDepth := FS.ScopeCount;
    Inc(FS.TryCount);
    Outer := Emit(Ins(opTry, ExcReg, 1, 0));   { kind 1: finally }
  end
  else Outer := -1;
  if Assigned(S.CatchBlock) then
  begin
    if FS.TryCount >= Length(FS.Tries) then SetLength(FS.Tries, FS.TryCount * 2 + 4);
    FS.Tries[FS.TryCount].Block := nil;
    FS.Tries[FS.TryCount].IterReg := -1;
    FS.Tries[FS.TryCount].UsingCap := -1;
    FS.Tries[FS.TryCount].ScopeDepth := FS.ScopeCount;
    Inc(FS.TryCount);
    Inner := Emit(Ins(opTry, ExcReg, 0, 0));
    Stmt(S.Block);
    Dec(FS.TryCount);
    Emit(Ins(opEndTry, 0, 0, 0));
    JEnd := EmitJ(opJmp, 0, 0);
    PatchHere(Inner);
    { a throw leaves the try block's scopes without their opClose: the closures made in them keep their
      variables, whose registers the catch block may reuse }
    Emit(Ins(opClose, ExcReg + 1, 0, 0));
    PushScope;
    if S.CatchParam <> '' then
    begin
      R := DeclareLocal(S.CatchParam, False);
      Emit(Ins(opMov, R, ExcReg, 0));
    end
    else if Assigned(S.CatchBindingPattern) then
      BindPattern(S.CatchBindingPattern, ExcReg, True, False);
    HoistBlock(TList<TGocciaASTNode>(S.CatchBlock.Nodes));
    StmtList(S.CatchBlock.Nodes);
    PopScope;
    PatchHere(JEnd);
  end
  else
    Stmt(S.Block);
  if HasFinally then
  begin
    Dec(FS.TryCount);
    Emit(Ins(opEndTry, 0, 0, 0));
    FinallyStmt(S.FinallyBlock);
    JEnd2 := EmitJ(opJmp, 0, 0);
    PatchHere(Outer);
    Emit(Ins(opClose, ExcReg + 1, 0, 0));
    FinallyStmt(S.FinallyBlock);
    Emit(Ins(opThrow, ExcReg, 0, 0));
    PatchHere(JEnd2);
  end;
  FreeTo(ExcReg);
end;

procedure TCompiler.CompileReturn(S: TGocciaReturnStatement);
var R: Integer;
begin
  if S.HasExpression and Assigned(S.Value) and FS.Tpl^.IsAsync and FS.Tpl^.IsGenerator then
  begin
    { 14.10.1: an async generator's return awaits its operand }
    R := FS.FreeReg; AllocReg;
    ExprTo(S.Value, R);
    Emit(Ins(opAwait, R, R, 0));
    if FS.TryCount > 0 then EmitFinallyUnwind(0);
    Emit(Ins(opRet, R, 0, 0));
    FreeTo(R);
    Exit;
  end;
  if S.HasExpression and Assigned(S.Value) then
  begin
    if FS.TryCount = 0 then
    begin
      { no finally can run between evaluating and returning: a variable's register is returned as is }
      R := FS.FreeReg;
      Emit(Ins(opRet, ExprAny(S.Value), 0, 0));
      FreeTo(R);
      Exit;
    end;
    R := FS.FreeReg; AllocReg;
    ExprTo(S.Value, R);
    if FS.TryCount > 0 then EmitFinallyUnwind(0);
    Emit(Ins(opRet, R, 0, 0));
    FreeTo(R);
  end
  else
  begin
    if FS.TryCount > 0 then EmitFinallyUnwind(0);
    Emit(Ins(opRetUndef, 0, 0, 0));
  end;
end;

{ ---------------- classes ---------------- }

{ ---------------- classes (ES2026 15.7.14 ClassDefinitionEvaluation) ----------------
  The class scope holds the class binding, one register per private name (its key, made fresh by each
  evaluation of the class: opNewPrivName), the keys of computed fields ('%k' + element index, evaluated in
  source order with the other elements) and the closures of private methods and accessors ('%m'/'%g'/'%s' +
  index). Methods and the two generated functions below reach them as upvalues.
  - The instance initializer (template FieldInit of the constructor, run by construction for a base class and
    after super() for a derived one) adds the private methods (the brand), then the fields in source order.
  - The static initializer runs once, this = the constructor, after the static private methods: static fields
    and static blocks in source order. }

{ 8.4.3 IsAnonymousFunctionDefinition }
function IsAnonymousFunctionDefinition(E: TGocciaExpression): Boolean;
begin
  if E is TGocciaArrowFunctionExpression then Exit(True);
  if E is TGocciaFunctionExpression then Exit(TGocciaFunctionExpression(E).Name = '');
  if E is TGocciaClassExpression then Exit(TGocciaClassExpression(E).ClassDefinition.Name = '');
  Result := False;
end;

function PrivKeyName(const N: string): UnicodeString; inline;
begin
  Result := '#' + UnicodeString(N);
end;

function ClassHasInstanceInit(Def: TGocciaClassDefinition): Boolean;
var I: Integer;
begin
  Result := Length(Def.FFieldOrder) > 0;
  if not Result then
    for I := 0 to High(Def.FElements) do
      if Def.FElements[I].IsPrivate and not Def.FElements[I].IsStatic and
         (Def.FElements[I].Kind in [cekMethod, cekGetter, cekSetter]) then Exit(True);
end;

function ClassHasStaticInit(Def: TGocciaClassDefinition): Boolean;
var I: Integer;
begin
  for I := 0 to High(Def.FElements) do
    if Def.FElements[I].IsStatic and (Def.FElements[I].Kind in [cekField, cekStaticBlock, cekAccessor]) then Exit(True);
  for I := 0 to High(Def.FElements) do
    if (Def.FElements[I].Kind = cekStaticBlock) then Exit(True);
  Result := False;
end;

{ a field's value: a named anonymous function gets the field's name }
{ ---------------- expressions ---------------- }

function TCompiler.BinOpFor(T: TGocciaTokenType): TOp;
begin
  case T of
    gttPlus, gttPlusAssign: Result := opAdd;
    gttMinus, gttMinusAssign: Result := opSub;
    gttStar, gttStarAssign: Result := opMul;
    gttSlash, gttSlashAssign: Result := opDiv;
    gttPercent, gttPercentAssign: Result := opMod;
    gttPower, gttPowerAssign: Result := opPow;
    gttBitwiseAnd, gttBitwiseAndAssign: Result := opBitAnd;
    gttBitwiseOr, gttBitwiseOrAssign: Result := opBitOr;
    gttBitwiseXor, gttBitwiseXorAssign: Result := opBitXor;
    gttLeftShift, gttLeftShiftAssign: Result := opShl;
    gttRightShift, gttRightShiftAssign: Result := opShr;
    gttUnsignedRightShift, gttUnsignedRightShiftAssign: Result := opUShr;
    gttEqual: Result := opSEq;
    gttNotEqual: Result := opSNe;
    gttLooseEqual: Result := opEq;
    gttLooseNotEqual: Result := opNe;
    gttLess: Result := opLt;
    gttLessEqual: Result := opLe;
    gttGreater: Result := opGt;
    gttGreaterEqual: Result := opGe;
    gttInstanceof: Result := opInstanceof;
    gttIn: Result := opIn;
  else
    raise ECompileError.CreateFmt('unsupported binary operator %d', [Ord(T)]);
  end;
end;

{ evaluating E runs no code (so cannot reassign a local): a literal, or a local read straight from its register }
function TCompiler.PureRead(E: TGocciaExpression): Boolean;
var R: Integer;
begin
  if E is TGocciaLiteralExpression then Exit(True);
  Result := (E is TGocciaIdentifierExpression) and ResolveLocal(FS, TGocciaIdentifierExpression(E).Name, R) and
    not InTDZ(TGocciaIdentifierExpression(E).Name) and
    ((WithCount = 0) or (WithsFor(TGocciaIdentifierExpression(E).Name, WithIdxScratch) = 0));
end;

{ an assignment or update whose value, when unused, need not be copied (TopDiscard) }
function TCompiler.IsDiscardable(E: TGocciaExpression): Boolean;
begin
  Result := (E is TGocciaAssignmentExpression) or (E is TGocciaIncrementExpression) or
    (E is TGocciaPropertyAssignmentExpression) or (E is TGocciaComputedPropertyAssignmentExpression) or
    (E is TGocciaCompoundAssignmentExpression);
end;

function TCompiler.ExprAny(E: TGocciaExpression): Integer;
var R, K: Integer;
begin
  if E <> nil then
  begin
    K := KindOf(ExprKinds, ExprClasses, E);   { 1 identifier, 2 this, as in ExprTo }
    { a local's register, unless a with object between the reference and the binding can hold the name }
    if (K = 1) and ResolveLocal(FS, TGocciaIdentifierExpression(E).Name, R) and
       not InTDZ(TGocciaIdentifierExpression(E).Name) and
       ((WithCount = 0) or (WithsFor(TGocciaIdentifierExpression(E).Name, WithIdxScratch) = 0)) then
      Exit(R);
    if (K = 2) and not FS.IsArrow then
    begin
      if InDerivedCtor then Emit(Ins(opCheckThis, 0, 0, 0));
      Exit(0);
    end;
  end;
  R := AllocReg;
  ExprTo(E, R);
  Result := R;
end;

{ conservative: False only when evaluating E cannot run code or assign }
function TCompiler.MaySideEffect(E: TGocciaExpression): Boolean;
begin
  if E = nil then Exit(False);
  if (E is TGocciaIdentifierExpression) or (E is TGocciaLiteralExpression) or (E is TGocciaThisExpression) or
     (E is TGocciaFunctionExpression) or (E is TGocciaArrowFunctionExpression) then Exit(False);
  if E is TGocciaBinaryExpression then
    Exit(MaySideEffect(TGocciaBinaryExpression(E).Left) or MaySideEffect(TGocciaBinaryExpression(E).Right));
  if E is TGocciaUnaryExpression then
    Exit((TGocciaUnaryExpression(E).Operator = gttDelete) or MaySideEffect(TGocciaUnaryExpression(E).Operand));
  if E is TGocciaMemberExpression then
    Exit(MaySideEffect(TGocciaMemberExpression(E).ObjectExpr) or
      (TGocciaMemberExpression(E).Computed and MaySideEffect(TGocciaMemberExpression(E).PropertyExpression)));
  if E is TGocciaConditionalExpression then
    Exit(MaySideEffect(TGocciaConditionalExpression(E).Condition) or
      MaySideEffect(TGocciaConditionalExpression(E).Consequent) or MaySideEffect(TGocciaConditionalExpression(E).Alternate));
  Result := True;
end;

{ Dest holds a declared variable: an expression that writes Dest before it has read all its operands
  (x = y || x, or a template literal that reads x) must build its value elsewhere }
function TCompiler.IsVarReg(Dest: Integer): Boolean;
begin
  { counts per register of the bindings in the open scopes (DeclareInScope / PopScope) }
  Result := (Dest >= 0) and (Dest < Length(FS.VarRegs)) and (FS.VarRegs[Dest] > 0);
end;

procedure TCompiler.CompilePrivateGet(E: TGocciaPrivateMemberExpression; Dest: Integer; ThisDest: Integer);
var Save, O, K: Integer; IsLink: Boolean; SavedJ: TJumpList;
begin
  IsLink := ChainBegin(SavedJ);
  Save := FS.FreeReg;
  O := AllocReg;
  ChainObject(E.ObjectExpr);
  ExprTo(E.ObjectExpr, O);
  ChainLink := False;
  if E.Optional then ChainAdd(EmitJ(opJNullish, O, 0));
  K := AllocReg;
  LoadIdent(PrivKeyName(E.PrivateName), K);
  Emit(Ins(opGetPriv, Dest, O, K));
  ChainEnd(IsLink, SavedJ, Dest);
  FreeTo(Save);
end;

{ the nearest non-arrow function is a derived constructor: its this is unbound until super() returns }
function TCompiler.InDerivedCtor: Boolean;
var F: TFuncState;
begin
  F := FS;
  while (F.Parent <> nil) and F.IsArrow do F := F.Parent;
  Result := F.Tpl^.IsClassCtor and F.Tpl^.IsDerived;
end;

procedure TCompiler.LoadThis(Dest: Integer);
begin
  if FS.IsArrow then LoadIdent('%this', Dest)
  else if Dest <> 0 then Emit(Ins(opMov, Dest, 0, 0));
  if InDerivedCtor then Emit(Ins(opCheckThis, Dest, 0, 0));
end;

procedure TCompiler.CompileFieldValue(V: TGocciaExpression; Dest: Integer; const Name: UnicodeString);
begin
  if V = nil then Emit(Ins(opLoadUndef, Dest, 0, 0))
  else if (Name <> '') and ((V is TGocciaFunctionExpression) or (V is TGocciaArrowFunctionExpression)) then
    CompileFuncExpr(V, Dest, Name)
  else if (Name <> '') and (V is TGocciaClassExpression) and (TGocciaClassExpression(V).ClassDefinition.Name = '') then
    CompileClass(TGocciaClassExpression(V).ClassDefinition, Dest, Name)
  else
    ExprTo(V, Dest);
end;

{ the private methods and accessors of one placement (instance or static) onto the object in register Obj }
procedure TCompiler.EmitPrivateMethods(Def: TGocciaClassDefinition; Obj: Integer; Static: Boolean);
var I, J, K, R: Integer; Done: array of Boolean; E: TGocciaClassElement;
begin
  SetLength(Done, Length(Def.FElements));
  for I := 0 to High(Def.FElements) do
  begin
    E := Def.FElements[I];
    if Done[I] or not E.IsPrivate or (E.IsStatic <> Static) or not (E.Kind in [cekMethod, cekGetter, cekSetter]) then
      Continue;
    Done[I] := True;
    K := AllocReg;
    LoadIdent(PrivKeyName(E.Name), K);
    if E.Kind = cekMethod then
    begin
      R := AllocReg;
      LoadIdent('%m' + IntToStr(I), R);
      Emit(Ins(opAddPriv, Obj, R, K));
    end
    else
    begin
      { a getter and a setter of one name are one private element }
      R := AllocRegs(2);
      Emit(Ins(opLoadUndef, R, 0, 0));
      Emit(Ins(opLoadUndef, R + 1, 0, 0));
      for J := I to High(Def.FElements) do
        if Def.FElements[J].IsPrivate and (Def.FElements[J].IsStatic = Static) and
           (Def.FElements[J].Name = E.Name) and (Def.FElements[J].Kind in [cekGetter, cekSetter]) then
        begin
          Done[J] := True;
          if Def.FElements[J].Kind = cekGetter then LoadIdent('%m' + IntToStr(J), R)
          else LoadIdent('%m' + IntToStr(J), R + 1);
        end;
      Emit(Ins(opAddPriv, Obj, R or $8000, K));
    end;
    FreeTo(K);
  end;
end;

{ the body of the instance (Static False) or static initializer; this (register 0) is the target }
procedure TCompiler.EmitClassInit(Def: TGocciaClassDefinition; Static: Boolean);
var I, V, K, C: Integer; F: TGocciaFieldOrderEntry; E: TGocciaClassElement; T: PFuncTemplate;
  NoParams: TGocciaParameterArray;
begin
  SetLength(NoParams, 0);
  if not Static then
  begin
    EmitPrivateMethods(Def, 0, False);
    for I := 0 to High(Def.FFieldOrder) do
    begin
      F := Def.FFieldOrder[I];
      V := AllocReg;
      K := AllocReg;
      if F.IsPrivate then
      begin
        CompileFieldValue(F.FieldInitializer, V, PrivKeyName(F.Name));
        LoadIdent(PrivKeyName(F.Name), K);
        Emit(Ins(opDefPrivField, 0, V, K));
      end
      else if F.IsComputed then
      begin
        CompileFieldValue(F.FieldInitializer, V, '');
        LoadIdent('%k' + IntToStr(F.ElementIndex), K);
        if IsAnonymousFunctionDefinition(F.FieldInitializer) then Emit(Ins(opSetFnName, V, K, 0));
        Emit(Ins(opDefFieldC, 0, V, K));
      end
      else
      begin
        CompileFieldValue(F.FieldInitializer, V, UnicodeString(F.Name));
        Emit(Ins(opDefField, 0, V, NewIC(F.Name)));
      end;
      FreeTo(V);
    end;
    Exit;
  end;
  for I := 0 to High(Def.FElements) do
  begin
    E := Def.FElements[I];
    if not (E.IsStatic or (E.Kind = cekStaticBlock)) then Continue;
    case E.Kind of
      cekField:
        begin
          V := AllocReg;
          K := AllocReg;
          if E.IsPrivate then
          begin
            CompileFieldValue(E.FieldInitializer, V, PrivKeyName(E.Name));
            LoadIdent(PrivKeyName(E.Name), K);
            Emit(Ins(opDefPrivField, 0, V, K));
          end
          else if E.IsComputed then
          begin
            CompileFieldValue(E.FieldInitializer, V, '');
            LoadIdent('%k' + IntToStr(I), K);
            if IsAnonymousFunctionDefinition(E.FieldInitializer) then Emit(Ins(opSetFnName, V, K, 0));
            Emit(Ins(opDefFieldC, 0, V, K));
          end
          else
          begin
            CompileFieldValue(E.FieldInitializer, V, UnicodeString(E.Name));
            Emit(Ins(opDefField, 0, V, NewIC(E.Name)));
          end;
          FreeTo(V);
        end;
      cekAccessor:
        begin
          { a static auto-accessor's storage }
          V := AllocReg;
          K := AllocReg;
          CompileFieldValue(E.FieldInitializer, V, '');
          LoadIdent(PrivKeyName(E.AccessorStorageName), K);
          Emit(Ins(opDefPrivField, 0, V, K));
          FreeTo(V);
        end;
      cekStaticBlock:
        begin
          { its own function (its own var scope), called with this = the class }
          PendingNoArguments := True;
          PendingStaticBlock := True;
          T := CompileFunction('', NoParams, E.StaticBlockBody, False, True, False, False, False, False);
          C := AllocRegs(2);
          EmitClosure(C, T);
          Emit(Ins(opSetHome, C, 0, 0));
          Emit(Ins(opMov, C + 1, 0, 0));
          Emit(Ins(opCall, C, 0, 0));
          FreeTo(C);
        end;
    end;
  end;
end;

{ 15.7.1 Static Semantics: Early Errors of a ClassBody: element names, one constructor, private names declared once
  (a getter and a setter of the same staticness may share one) }
procedure CheckClassElements(Def: TGocciaClassDefinition);
var I, J, Ctors: Integer; E, F: TGocciaClassElement;
begin
  Ctors := 0;
  for I := 0 to High(Def.FElements) do
  begin
    E := Def.FElements[I];
    if E.Kind = cekStaticBlock then Continue;
    if E.IsPrivate then
    begin
      if E.Name = 'constructor' then raise EEarlySyntaxError.Create('Classes may not have a private field named ''#constructor''');
      for J := 0 to I - 1 do
      begin
        F := Def.FElements[J];
        if (F.Kind = cekStaticBlock) or not F.IsPrivate or (F.Name <> E.Name) then Continue;
        if not (((E.Kind = cekGetter) and (F.Kind = cekSetter)) or ((E.Kind = cekSetter) and (F.Kind = cekGetter))) or
           (E.IsStatic <> F.IsStatic) then
          raise EEarlySyntaxError.Create('Identifier ''#' + E.Name + ''' has already been declared');
      end;
      Continue;
    end;
    if E.IsComputed then Continue;
    if (E.Kind = cekField) and (E.Name = 'constructor') then
      raise EEarlySyntaxError.Create('Classes may not have a field named ''constructor''');
    if E.IsStatic and (E.Name = 'prototype') and (E.Kind <> cekStaticBlock) then
      raise EEarlySyntaxError.Create('Classes may not have a static property named ''prototype''');
    if not E.IsStatic and (E.Name = 'constructor') then
    begin
      if (E.Kind in [cekGetter, cekSetter, cekAccessor]) or ((E.Kind = cekMethod) and (E.IsGenerator or E.IsAsync)) then
        raise EEarlySyntaxError.Create('Class constructor may not be an accessor, a generator or async');
      if E.Kind = cekMethod then Inc(Ctors);
    end;
  end;
  if Ctors > 1 then raise EEarlySyntaxError.Create('A class may only have one constructor');
  for I := 0 to High(Def.FFieldOrder) do
  begin
    if not Def.FFieldOrder[I].IsPrivate and not Def.FFieldOrder[I].IsComputed and
       not Def.FFieldOrder[I].IsAutoAccessorStorage and (Def.FFieldOrder[I].Name = 'constructor') then
      raise EEarlySyntaxError.Create('Classes may not have a field named ''constructor''');
    if Def.FFieldOrder[I].IsPrivate and (Def.FFieldOrder[I].Name = 'constructor') then
      raise EEarlySyntaxError.Create('Classes may not have a private field named ''#constructor''');
  end;
end;

procedure TCompiler.CompileClass(Def: TGocciaClassDefinition; Dest: Integer; const NameHint: UnicodeString);
var Base, Ctor, Proto, Parent, Tmp, I, R, KeyReg, Target: Integer; CtorMethod: TGocciaClassMethod; T: PFuncTemplate;
  Derived: Boolean; NoParams: TGocciaParameterArray; Self_: Integer; E: TGocciaClassElement;
  PrivNames: TStringList; Name, CName: UnicodeString; CtorT: PFuncTemplate;
  procedure AddPriv(const N: string);
  begin
    if (N <> '') and (PrivNames.IndexOf(N) < 0) then PrivNames.Add(N);
  end;
begin
  { 15.7.1: all of a class, its name included, is strict mode code }
  if (Def.Name = 'yield') or (Def.Name = 'let') or (Def.Name = 'static') or (Def.Name = 'implements') or
     (Def.Name = 'interface') or (Def.Name = 'package') or (Def.Name = 'private') or (Def.Name = 'protected') or
     (Def.Name = 'public') then
    raise EEarlySyntaxError.Create('Unexpected strict mode reserved word ''' + Def.Name + '''');
  Base := FS.FreeReg;
  PushScope;
  { 15.7.14 step 4: the class scope's binding of its name is uninitialized until the elements are defined (its
    heritage and computed keys see it in TDZ) }
  if Def.Name <> '' then
  begin
    DeclaringLexical := True;
    try
      Self_ := DeclareLocal(Def.Name, True);
    finally
      DeclaringLexical := False;
    end;
  end
  else Self_ := -1;
  Ctor := AllocReg;
  Proto := AllocReg;
  Parent := AllocReg;
  { the heritage sees the enclosing private names, not this class's }
  Derived := Assigned(Def.SuperClassExpression) or (Def.SuperClass <> '');
  if Derived then
  begin
    if Assigned(Def.SuperClassExpression) then ExprTo(Def.SuperClassExpression, Parent)
    else LoadIdent(Def.SuperClass, Parent);
  end;
  { 15.7.1 early errors; ClassHeritage is a LeftHandSideExpression: an arrow only in parentheses }
  if Assigned(Def.SuperClassExpression) and (Def.SuperClassExpression is TGocciaArrowFunctionExpression) and
     not Def.SuperClassExpression.Parenthesized then
    raise EEarlySyntaxError.Create('Unexpected token: an arrow function is not a class heritage');
  CheckClassElements(Def);
  { the private environment: a fresh key per private name }
  PrivNames := TStringList.Create;
  try
    PrivNames.CaseSensitive := True;
    PrivNames.UseLocale := False;   { exact code-unit comparison: a locale collation equates distinct names }
    for I := 0 to High(Def.FElements) do
    begin
      if Def.FElements[I].IsPrivate then AddPriv(Def.FElements[I].Name);
      if Def.FElements[I].Kind = cekAccessor then AddPriv(Def.FElements[I].AccessorStorageName);
    end;
    for I := 0 to High(Def.FFieldOrder) do
      if Def.FFieldOrder[I].IsPrivate then AddPriv(Def.FFieldOrder[I].Name);
    for I := 0 to PrivNames.Count - 1 do
    begin
      R := DeclareLocal(PrivKeyName(PrivNames[I]), True);
      Emit(Ins(opNewPrivName, R, 0, AtomConst('#' + PrivNames[I])));
    end;
  finally
    PrivNames.Free;
  end;
  { the constructor }
  CtorMethod := nil;
  if Def.Methods.TryGetValue('constructor', CtorMethod) then ;
  SetLength(NoParams, 0);
  { 15.7.15: an anonymous class is named by its binding (NamedEvaluation) }
  if Def.Name = '' then CName := NameHint else CName := UnicodeString(Def.Name);
  if Assigned(CtorMethod) then
    T := CompileFunction(CName, CtorMethod.Parameters, CtorMethod.Body, False, True, True, Derived, False, False,
      TGocciaClassDefinition(nil))
  else if Derived then
  begin
    SetLength(NoParams, 1);
    FillChar(NoParams[0], SizeOf(NoParams[0]), 0);
    NoParams[0].Name := '%args';
    NoParams[0].IsRest := True;
    DefaultSuperForward := True;
    T := CompileFunction(CName, NoParams, nil, False, True, True, Derived, False, False, nil);
    SetLength(NoParams, 0);
  end
  else
    T := CompileFunction(CName, NoParams, nil, False, True, True, Derived, False, False, nil);
  CtorT := T;
  EmitClosure(Ctor, T);
  if Derived then Emit(Ins(opClassSetup, Ctor, Parent, 1))
  else Emit(Ins(opClassSetup, Ctor, Parent, 0));
  Emit(Ins(opGetProp, Proto, Ctor, NewIC('prototype')));
  { the elements, in source order: methods and accessors are defined, computed keys evaluated }
  for I := 0 to High(Def.FElements) do
  begin
    E := Def.FElements[I];
    if E.IsStatic then Target := Ctor else Target := Proto;
    case E.Kind of
      cekMethod, cekGetter, cekSetter, cekAccessor:
        begin
          if (E.Kind = cekMethod) and not E.IsStatic and not E.IsComputed and not E.IsPrivate and
             (E.Name = 'constructor') then Continue;
          if (E.Kind = cekAccessor) and E.IsPrivate then Continue;   { a private auto-accessor is its storage }
          if E.IsPrivate then
          begin
            R := DeclareLocal('%m' + IntToStr(I), True);
            case E.Kind of
              cekMethod:
                begin
                  T := CompileFunction(PrivKeyName(E.Name), E.MethodNode.Parameters, E.MethodNode.Body, False,
                    True, False, False, E.MethodNode.IsGenerator, E.MethodNode.IsAsync);
                  T^.SourceRange := E.MethodNode.SourceRange;
                end;
              cekGetter:
                begin
                  T := CompileFunction('get ' + PrivKeyName(E.Name), NoParams, E.GetterNode.Body, False, True,
                    False, False, False, False);
                  T^.SourceRange := E.GetterNode.SourceRange;
                end;
            else
              T := CompileFunction('set ' + PrivKeyName(E.Name), E.SetterNode.Parameters, E.SetterNode.Body, False,
                True, False, False, False, False);
              T^.SourceRange := E.SetterNode.SourceRange;
            end;
            EmitClosure(R, T);
            Emit(Ins(opSetHome, R, Target, 0));
            Continue;
          end;
          KeyReg := -1;
          Name := UnicodeString(E.Name);
          if E.IsComputed then
          begin
            KeyReg := AllocReg;
            ExprTo(E.ComputedKeyExpression, KeyReg);
            Emit(Ins(opToPropKey, KeyReg, KeyReg, 0));
            Name := '';
          end;
          Tmp := AllocReg;
          if E.Kind = cekMethod then
          begin
            T := CompileFunction(Name, E.MethodNode.Parameters, E.MethodNode.Body, False, True, False, False,
              E.MethodNode.IsGenerator, E.MethodNode.IsAsync);
            T^.SourceRange := E.MethodNode.SourceRange;
            EmitClosure(Tmp, T);
            if KeyReg >= 0 then Emit(Ins(opSetFnName, Tmp, KeyReg, 0));
            if KeyReg >= 0 then Emit(Ins(opDefMethodC, Target, Tmp, KeyReg))
            else Emit(Ins(opDefMethod, Target, Tmp, AtomConst(Name)));
          end;
          if E.Kind in [cekGetter, cekAccessor] then
          begin
            if Name <> '' then T := CompileFunction('get ' + Name, NoParams, E.GetterNode.Body, False, True, False,
              False, False, False)
            else T := CompileFunction('', NoParams, E.GetterNode.Body, False, True, False, False, False, False);
            T^.SourceRange := E.GetterNode.SourceRange;
            EmitClosure(Tmp, T);
            if KeyReg >= 0 then Emit(Ins(opSetFnName, Tmp, KeyReg, 1));
            if KeyReg >= 0 then Emit(Ins(opDefGetterC, Target, Tmp, KeyReg))
            else Emit(Ins(opDefGetter, Target, Tmp, AtomConst(Name)));
          end;
          if E.Kind in [cekSetter, cekAccessor] then
          begin
            if Name <> '' then T := CompileFunction('set ' + Name, E.SetterNode.Parameters, E.SetterNode.Body, False,
              True, False, False, False, False)
            else T := CompileFunction('', E.SetterNode.Parameters, E.SetterNode.Body, False, True, False, False,
              False, False);
            T^.SourceRange := E.SetterNode.SourceRange;
            EmitClosure(Tmp, T);
            if KeyReg >= 0 then Emit(Ins(opSetFnName, Tmp, KeyReg, 2));
            if KeyReg >= 0 then Emit(Ins(opDefSetterC, Target, Tmp, KeyReg))
            else Emit(Ins(opDefSetter, Target, Tmp, AtomConst(Name)));
          end;
          if KeyReg >= 0 then FreeTo(KeyReg) else FreeTo(Tmp);
        end;
      cekField:
        if E.IsComputed then
        begin
          { ClassFieldDefinitionEvaluation: the key now, the value at initialization }
          R := DeclareLocal('%k' + IntToStr(I), True);
          ExprTo(E.ComputedKeyExpression, R);
          Emit(Ins(opToPropKey, R, R, 0));
        end;
    end;
  end;
  { the class binding is initialized once the elements are defined }
  if Self_ >= 0 then Emit(Ins(opMov, Self_, Ctor, 0));
  if Self_ >= 0 then MarkInitialized(Def.Name);
  if ClassHasInstanceInit(Def) then
  begin
    PendingInitStatic := False;
    PendingNoArguments := True;
    T := CompileFunction('<instance_members_initializer>', NoParams, nil, False, True, False, False, False, False, Def);
    CtorT^.FieldInit := T;
    Tmp := AllocReg;
    EmitClosure(Tmp, T);
    Emit(Ins(opClassField, Ctor, Tmp, 0));
    FreeTo(Tmp);
  end;
  { static private methods, then static fields and blocks }
  Tmp := AllocReg;
  EmitPrivateMethods(Def, Ctor, True);
  FreeTo(Tmp);
  if ClassHasStaticInit(Def) then
  begin
    PendingInitStatic := True;
    PendingNoArguments := True;
    T := CompileFunction('<static_initializer>', NoParams, nil, False, True, False, False, False, False, Def);
    Tmp := AllocRegs(2);
    EmitClosure(Tmp, T);
    Emit(Ins(opSetHome, Tmp, Ctor, 0));
    Emit(Ins(opMov, Tmp + 1, Ctor, 0));
    Emit(Ins(opCall, Tmp, 0, 0));
    FreeTo(Tmp);
  end;
  if Dest <> Ctor then Emit(Ins(opMov, Dest, Ctor, 0));
  { leave the class scope; Dest (below Base, or Ctor's own register when Dest = Ctor is impossible) keeps the class }
  FreeTo(Base);
  Dec(FS.ScopeCount);
  for I := 0 to FS.Scopes[FS.ScopeCount].Count - 1 do
    if FS.Scopes[FS.ScopeCount].Bindings[I].Reg < Length(FS.VarRegs) then
      Dec(FS.VarRegs[FS.Scopes[FS.ScopeCount].Bindings[I].Reg]);
  if FS.Scopes[FS.ScopeCount].Captured then Emit(Ins(opClose, FS.Scopes[FS.ScopeCount].BaseReg, 0, 0));
  FS.FreeReg := FS.Scopes[FS.ScopeCount].BaseReg;
  FS.Scopes[FS.ScopeCount].Free;
end;

{ 14.6 / 14.7: the body of an if or a loop is a Statement, not a declaration; B.3.3 lets a plain function
  declaration be an if clause in non-strict code (IfClause = True) }
procedure CheckStatementBody(S: TGocciaASTNode; IfClause, Strict: Boolean);
begin
  if S = nil then Exit;
  if (S is TGocciaClassDeclaration) or
     ((S is TGocciaVariableDeclaration) and not TGocciaVariableDeclaration(S).IsVar) or
     ((S is TGocciaDestructuringDeclaration) and not TGocciaDestructuringDeclaration(S).IsVar) or
     (S is TGocciaUsingDeclaration) then
    raise EEarlySyntaxError.Create('Lexical declaration cannot appear in a single-statement context');
  if S is TGocciaFunctionDeclaration then
    if not IfClause or Strict or TGocciaFunctionDeclaration(S).FunctionExpression.IsAsync or
       TGocciaFunctionDeclaration(S).FunctionExpression.IsGenerator or (TGocciaStatement(S).LabelCount > 0) then
      raise EEarlySyntaxError.Create('In strict mode code or a loop body, functions can only be declared at top level or inside a block');
end;

{ 15.5.1 a YieldExpression is an AssignmentExpression: as the operand of a unary or binary operator or as a
  conditional's test (unparenthesized) it is a SyntaxError (the parser builds it anyway) }
procedure NoBareYield(E: TGocciaExpression);
begin
  if (E is TGocciaYieldExpression) and not E.Parenthesized then
    raise EEarlySyntaxError.Create('Unexpected yield expression here');
end;

procedure TCompiler.CompileBinary(E: TGocciaBinaryExpression; Dest: Integer);
var Save, L, R: Integer; J: Integer; Op: TOp; V: TValue;
begin
  NoBareYield(E.Left);
  NoBareYield(E.Right);
  if (E.Operator = gttIn) and (E.Left is TGocciaPrivateMemberExpression) then
  begin
    { #x in obj (ergonomic brand check) }
    Save := FS.FreeReg;
    R := AllocReg;
    ExprTo(E.Right, R);
    L := AllocReg;
    LoadIdent(PrivKeyName(TGocciaPrivateMemberExpression(E.Left).PrivateName), L);
    Emit(Ins(opPrivIn, Dest, R, L));
    FreeTo(Save);
    Exit;
  end;
  if (E.Operator in [gttAnd, gttOr, gttNullishCoalescing]) and IsVarReg(Dest) then
  begin
    Save := FS.FreeReg;
    R := AllocReg;
    CompileBinary(E, R);
    Emit(Ins(opMov, Dest, R, 0));
    FreeTo(Save);
    Exit;
  end;
  case E.Operator of
    gttAnd:
      begin
        ExprTo(E.Left, Dest);
        J := EmitJ(opJF, Dest, 0);
        ExprTo(E.Right, Dest);
        PatchHere(J);
        Exit;
      end;
    gttOr:
      begin
        ExprTo(E.Left, Dest);
        J := EmitJ(opJT, Dest, 0);
        ExprTo(E.Right, Dest);
        PatchHere(J);
        Exit;
      end;
    gttNullishCoalescing:
      begin
        ExprTo(E.Left, Dest);
        J := EmitJ(opJNotNullish, Dest, 0);
        ExprTo(E.Right, Dest);
        PatchHere(J);
        Exit;
      end;
  end;
  Save := FS.FreeReg;
  Op := BinOpFor(E.Operator);
  { x + small int }
  if (Op in [opAdd, opSub]) and (E.Right is TGocciaLiteralExpression) and
     (TGocciaLiteralExpression(E.Right).Value is TGocciaNumberLiteralValue) then
  begin
    V := MkNumber(TGocciaNumberLiteralValue(TGocciaLiteralExpression(E.Right).Value).Value);
    if IsInt(V) and (Abs(IntOf(V)) < 4000000) then
    begin
      L := ExprAny(E.Left);
      if Op = opAdd then Emit(Ins(opAddI, Dest, L, IntOf(V))) else Emit(Ins(opSubI, Dest, L, -IntOf(V)));
      FreeTo(Save);
      Exit;
    end;
  end;
  { x op k for the bitwise and shift operators and an int32 literal k }
  if (Op in [opBitAnd, opBitOr, opBitXor, opShl, opShr, opUShr]) and (E.Right is TGocciaLiteralExpression) and
     (TGocciaLiteralExpression(E.Right).Value is TGocciaNumberLiteralValue) then
  begin
    V := MkNumber(TGocciaNumberLiteralValue(TGocciaLiteralExpression(E.Right).Value).Value);
    if IsInt(V) and (IntOf(V) >= -8388608) and (IntOf(V) < 8388608) then
    begin
      L := ExprAny(E.Left);
      case Op of
        opBitAnd: Emit(Ins(opBitAndI, Dest, L, IntOf(V)));
        opBitOr: Emit(Ins(opBitOrI, Dest, L, IntOf(V)));
        opBitXor: Emit(Ins(opBitXorI, Dest, L, IntOf(V)));
        opShl: Emit(Ins(opShlI, Dest, L, IntOf(V)));
        opShr: Emit(Ins(opShrI, Dest, L, IntOf(V)));
      else Emit(Ins(opUShrI, Dest, L, IntOf(V)));
      end;
      FreeTo(Save);
      Exit;
    end;
  end;
  L := ExprAny(E.Left);
  { a variable's register is read in place: copy it first when the right operand may change it
    (x + (x = 1), node !== (node = node.unwrap()), x + f() with f assigning x) }
  if (L < Save) and MaySideEffect(E.Right) then
  begin
    R := AllocReg;
    Emit(Ins(opMov, R, L, 0));
    L := R;
  end;
  R := ExprAny(E.Right);
  Emit(Ins(Op, Dest, L, R));
  FreeTo(Save);
end;

procedure TCompiler.CompileUnary(E: TGocciaUnaryExpression; Dest: Integer);
var Save, R, K, J: Integer; M: TGocciaMemberExpression; WEnds: TJumpList;
begin
  NoBareYield(E.Operand);
  Save := FS.FreeReg;
  case E.Operator of
    gttNot: begin R := ExprAny(E.Operand); Emit(Ins(opNot, Dest, R, 0)); end;
    gttMinus:
      begin
        if (E.Operand is TGocciaLiteralExpression) and (TGocciaLiteralExpression(E.Operand).Value is TGocciaNumberLiteralValue) then
          LoadNumber(-TGocciaNumberLiteralValue(TGocciaLiteralExpression(E.Operand).Value).Value, Dest)
        else begin R := ExprAny(E.Operand); Emit(Ins(opNeg, Dest, R, 0)); end;
      end;
    gttPlus: begin R := ExprAny(E.Operand); Emit(Ins(opToNum, Dest, R, 0)); end;
    gttBitwiseNot: begin R := ExprAny(E.Operand); Emit(Ins(opBitNot, Dest, R, 0)); end;
    gttTypeof:
      begin
        { typeof x reads the reference x: the same early errors (arguments in a field initializer, await, yield) }
        if E.Operand is TGocciaIdentifierExpression then CheckIdent(TGocciaIdentifierExpression(E.Operand).Name);
        if (E.Operand is TGocciaIdentifierExpression) and (WithCount > 0) and
           (WithsFor(TGocciaIdentifierExpression(E.Operand).Name, WithIdxScratch) > 0) then
        begin
          { typeof through with objects: the static fallback stays the unresolvable-safe typeof }
          EmitWithChain(TGocciaIdentifierExpression(E.Operand).Name, Dest, -1, WEnds);
          J := WithCount;
          WithCount := 0;
          try
            R := FS.FreeReg; AllocReg;
            if not ResolveLocal(FS, TGocciaIdentifierExpression(E.Operand).Name, K) and
               (ResolveUpval(FS, TGocciaIdentifierExpression(E.Operand).Name) < 0) and
               ((Goal <> 3) or (ModImportSlot(TGocciaIdentifierExpression(E.Operand).Name) < 0)) then
              Emit(Ins(opTypeofGlobal, Dest, 0, NewIC(TGocciaIdentifierExpression(E.Operand).Name)))
            else begin LoadIdentStatic(TGocciaIdentifierExpression(E.Operand).Name, R); Emit(Ins(opTypeof, Dest, R, 0)); end;
            FreeTo(R);
          finally
            WithCount := J;
          end;
          K := EmitJ(opJmp, 0, 0);
          for J := 0 to High(WEnds) do PatchHere(WEnds[J]);
          Emit(Ins(opTypeof, Dest, Dest, 0));
          PatchHere(K);
        end
        else if (E.Operand is TGocciaIdentifierExpression) and not ResolveLocal(FS, TGocciaIdentifierExpression(E.Operand).Name, R)
           and (ResolveUpval(FS, TGocciaIdentifierExpression(E.Operand).Name) < 0) and
           ((Goal <> 3) or (ModImportSlot(TGocciaIdentifierExpression(E.Operand).Name) < 0)) then
          Emit(Ins(opTypeofGlobal, Dest, 0, NewIC(TGocciaIdentifierExpression(E.Operand).Name)))
        else begin R := ExprAny(E.Operand); Emit(Ins(opTypeof, Dest, R, 0)); end;
      end;
    gttVoid: begin ExprAny(E.Operand); Emit(Ins(opLoadUndef, Dest, 0, 0)); end;
    gttDelete:
      if E.Operand is TGocciaPrivateMemberExpression then
        raise EEarlySyntaxError.Create('Private fields can not be deleted')
      else
      begin
        if (E.Operand is TGocciaMemberExpression) and (TGocciaMemberExpression(E.Operand).ObjectExpr is TGocciaSuperExpression) then
        begin
          { 13.5.1.2 step 5.a: the reference is evaluated, then a super reference is a ReferenceError }
          M := TGocciaMemberExpression(E.Operand);
          if M.Computed then EvalSuperRef('', M.PropertyExpression, R, K) else EvalSuperRef(M.PropertyName, nil, R, K);
          Emit(Ins(opThrowConst, 0, 1, 0));
        end
        else if E.Operand is TGocciaMemberExpression then
        begin
          M := TGocciaMemberExpression(E.Operand);
          R := ExprAny(M.ObjectExpr);
          if M.Computed then
          begin
            K := ExprAny(M.PropertyExpression);
            Emit(Ins(opDelElem, Dest, R, K));
          end
          else
            Emit(Ins(opDelProp, Dest, R, AtomConst(M.PropertyName)));
        end
        else if E.Operand is TGocciaIdentifierExpression then
          CompileDeleteIdent(TGocciaIdentifierExpression(E.Operand).Name, Dest)
        else
        begin
          { not a reference: evaluated for its effects, then true }
          ExprTo(E.Operand, Dest);
          Emit(Ins(opLoadTrue, Dest, 0, 0));
        end;
      end;
  else
    raise ECompileError.CreateFmt('unsupported unary operator %d', [Ord(E.Operator)]);
  end;
  FreeTo(Save);
end;

function TCompiler.CompileArgs(Args: TObjectList<TGocciaExpression>; Base: Integer; out Spread: Boolean): Integer;
var I, R: Integer;
begin
  Spread := False;
  for I := 0 to Args.Count - 1 do
    if Args[I] is TGocciaSpreadExpression then Spread := True;
  if not Spread then
  begin
    for I := 0 to Args.Count - 1 do
    begin
      R := AllocReg;
      ExprTo(Args[I], R);
    end;
    Exit(Args.Count);
  end;
  { spread: build one array in Base }
  R := AllocReg;
  Emit(Ins(opNewArr, R, 0, 0));
  for I := 0 to Args.Count - 1 do
  begin
    AllocReg;
    if Args[I] is TGocciaSpreadExpression then
    begin
      ExprTo(TGocciaSpreadExpression(Args[I]).Argument, R + 1);
      Emit(Ins(opArrSpread, R, R + 1, 0));
    end
    else
    begin
      ExprTo(Args[I], R + 1);
      Emit(Ins(opArrPush, R, R + 1, 0));
    end;
    FreeTo(R + 1);
  end;
  Result := -1;
end;

procedure TCompiler.CompileCall(E: TGocciaCallExpression; Dest: Integer);
var Base, Argc, K: Integer; M: TGocciaMemberExpression; Spread: Boolean; J: Integer; NullJ: Integer; UndefThis: Boolean;
  Reuse: Boolean; EvalHint, EvalDescConst: Integer; F: TFuncState; Callee: TGocciaExpression; ParenChain: Boolean;
  IsLink: Boolean; SavedJ: TJumpList;
begin
  IsLink := ChainBegin(SavedJ);
  UndefThis := False;
  EvalHint := 0;
  EvalDescConst := 0;
  { (a?.b)(...): the parser closes the parenthesized chain in a one-element sequence; the call keeps its
    reference (13.2.10.1 returns the reference) and so its this, but the parentheses end the chain: a nullish
    base gives an undefined callee, which the call rejects }
  Callee := E.Callee;
  ParenChain := False;
  if (Callee is TGocciaSequenceExpression) and (TGocciaSequenceExpression(Callee).Expressions.Count = 1) and
     ((TGocciaSequenceExpression(Callee).Expressions[0] is TGocciaMemberExpression) or
      (TGocciaSequenceExpression(Callee).Expressions[0] is TGocciaPrivateMemberExpression)) then
  begin
    Callee := TGocciaSequenceExpression(Callee).Expressions[0];
    ParenChain := True;
  end;
  { a destination that is the newest temporary can be the call's base: the result lands in place, without a
    move (no part of the call expression can read a temporary it did not allocate) }
  Reuse := (Dest >= 1) and (Dest = FS.FreeReg - 1) and not IsVarReg(Dest);
  if Reuse then
  begin
    Base := Dest;
    AllocRegs(1);  { this }
  end
  else
  begin
    Base := FS.FreeReg;
    AllocRegs(2);  { callee, this }
  end;
  NullJ := -1;
  if Callee is TGocciaMemberExpression then
  begin
    M := TGocciaMemberExpression(Callee);
    if M.ObjectExpr is TGocciaSuperExpression then
    begin
      LoadThis(Base + 1);
      if M.Computed then CompileSuperMember('', M.PropertyExpression, Base)
      else CompileSuperMember(M.PropertyName, nil, Base);
    end
    else
    begin
      if not ParenChain then ChainObject(M.ObjectExpr);
      ExprTo(M.ObjectExpr, Base + 1);
      ChainLink := False;
      if M.Optional then NullJ := EmitJ(opJNullish, Base + 1, 0);
      if M.Computed then
      begin
        K := AllocReg;
        ExprTo(M.PropertyExpression, K);
        Emit(Ins(opGetElem, Base, Base + 1, K));
        FreeTo(Base + 2);
      end
      else
        Emit(Ins(opGetProp, Base, Base + 1, NewIC(M.PropertyName)));
    end;
  end
  else if Callee is TGocciaPrivateMemberExpression then
  begin
    { obj.#m(...): this is obj }
    if not ParenChain then ChainObject(TGocciaPrivateMemberExpression(Callee).ObjectExpr);
    ExprTo(TGocciaPrivateMemberExpression(Callee).ObjectExpr, Base + 1);
    ChainLink := False;
    if TGocciaPrivateMemberExpression(Callee).Optional then NullJ := EmitJ(opJNullish, Base + 1, 0);
    K := AllocReg;
    LoadIdent(PrivKeyName(TGocciaPrivateMemberExpression(Callee).PrivateName), K);
    Emit(Ins(opGetPriv, Base, Base + 1, K));
    FreeTo(Base + 2);
  end
  else if (E.Callee is TGocciaSuperExpression) and FS.IsArrow then
  begin
    { super(...) in an arrow: the enclosing derived constructor, its new.target and its this binding }
    F := FS;
    while (F.Parent <> nil) and F.IsArrow do F := F.Parent;
    if (F.CtorReg < 0) and not (F.IsEvalOuter and (F.Scopes[0].Find('%ctor') >= 0)) then
      raise EEarlySyntaxError.Create('''super'' keyword unexpected here');
    F.CtorUsed := True;
    F.NewTargetUsed := True;
    LoadIdent('%ctor', Base);
    LoadIdent('%newtarget', Base + 1);
    Argc := CompileArgs(E.Arguments, Base + 2, Spread);
    if Spread then RaiseUnsupported('spread super call in an arrow', E);
    Emit(Ins(opSuperCallUp, Base, Argc, ResolveUpval(FS, '%this')));
    if Dest <> Base then Emit(Ins(opMov, Dest, Base, 0));
    if Reuse then FreeTo(Base + 1) else FreeTo(Base);
    ChainEnd(IsLink, SavedJ, Base);
    Exit;
  end
  else if E.Callee is TGocciaSuperExpression then
  begin
    { super(...): 15.7.1 a SyntaxError outside a derived class constructor (global eval code checks at run time) }
    if not (FS.Tpl^.IsClassCtor and FS.Tpl^.IsDerived) then
      raise EEarlySyntaxError.Create('''super'' keyword unexpected here');
    Argc := CompileArgs(E.Arguments, Base + 2, Spread);
    if Spread then Emit(Ins(opSuperCallV, Base, 0, 0)) else Emit(Ins(opSuperCall, Base, Argc, 0));
    if Dest <> Base then Emit(Ins(opMov, Dest, Base, 0));
    if Reuse then FreeTo(Base + 1) else FreeTo(Base);
    ChainEnd(IsLink, SavedJ, Base);
    Exit;
  end
  else if (E.Callee is TGocciaIdentifierExpression) and (WithCount > 0) and
          (WithsFor(TGocciaIdentifierExpression(E.Callee).Name, WithIdxScratch) > 0) then
  begin
    { a name resolved in a with object: the call's this is that object (14.11 / 9.1.1.2.10 WithBaseObject) }
    LoadIdentThis(TGocciaIdentifierExpression(E.Callee).Name, Base, Base + 1, True);
    UndefThis := False;
  end
  else
  begin
    ChainObject(E.Callee);
    ExprTo(E.Callee, Base);
    ChainLink := False;
    UndefThis := True;   { the call sets this to undefined (bit 40) }
  end;
  if ParenChain and (NullJ >= 0) then
  begin
    J := EmitJ(opJmp, 0, 0);
    PatchHere(NullJ);
    Emit(Ins(opLoadUndef, Base, 0, 0));
    Emit(Ins(opLoadUndef, Base + 1, 0, 0));
    PatchHere(J);
    NullJ := -1;
  end
  else if NullJ >= 0 then
  begin
    ChainAdd(NullJ);
    NullJ := -1;
  end;
  { a direct eval: eval named by an identifier that resolves to no local binding (a with object or an eval
    environment may hold it: the call is direct when the value is %eval%, 13.3.6.1 step 6) }
  if (E.Callee is TGocciaIdentifierExpression) and (TGocciaIdentifierExpression(E.Callee).Name = 'eval') and
     not E.Optional and not ResolveLocal(FS, 'eval', K) and (ResolveUpval(FS, 'eval') < 0) then
  begin
    { A: 2 global var declarations (non-strict Script top level), 1 a strict caller (19.2.1.1: the eval code is
      strict), 0 its declarations stay its own }
    if FS.Tpl^.IsStrict then EvalHint := 2
    else EvalHint := Ord((FS.Parent = nil) and (Goal in [1, 2])) * 2 + 1;
    { in function code the eval code gets the call site's bindings }
    if FS.Parent <> nil then EvalDescConst := AtomConst(BuildEvalDesc) + 1;
  end;
  if E.Optional then ChainAdd(EmitJ(opJNullish, Base, 0));
  Argc := CompileArgs(E.Arguments, Base + 2, Spread);
  if EvalHint > 0 then
  begin
    Emit(Ins(opEvalHint, EvalHint - 1, 0, EvalDescConst));
    EvalHint := 0;
  end;
  if Spread then Emit(Ins(opCallV, Base, 0, 0) or (QWord(Ord(UndefThis)) shl 40))
  else Emit(Ins(opCall, Base, Argc, 0) or (QWord(Ord(UndefThis)) shl 40));
  ChainEnd(IsLink, SavedJ, Base);
  if Dest <> Base then Emit(Ins(opMov, Dest, Base, 0));
  if Reuse then FreeTo(Base + 1) else FreeTo(Base);
end;

procedure TCompiler.CompileNew(E: TGocciaNewExpression; Dest: Integer);
var Base, Argc: Integer; Spread: Boolean; B: TGocciaExpression;
begin
  { 13.3.10: an ImportCall is not a MemberExpression: new import(...) and new import(...).x are SyntaxErrors
    (a parenthesized one is a primary expression) }
  B := E.Callee;
  while not B.Parenthesized do
  begin
    if B is TGocciaImportCallExpression then raise EEarlySyntaxError.Create('Cannot use new with import');
    if B is TGocciaMemberExpression then B := TGocciaMemberExpression(B).ObjectExpr
    else if B is TGocciaCallExpression then B := TGocciaCallExpression(B).Callee
    else Break;
  end;
  Base := FS.FreeReg;
  AllocRegs(2);
  ExprTo(E.Callee, Base);
  Argc := CompileArgs(E.Arguments, Base + 2, Spread);
  if Spread then Emit(Ins(opNewV, Base, 0, 0)) else Emit(Ins(opNew, Base, Argc, 0));
  if Dest <> Base then Emit(Ins(opMov, Dest, Base, 0));
  FreeTo(Base);
end;

procedure TCompiler.CompileSuperMember(const Key: UnicodeString; KeyExpr: TGocciaExpression; Dest: Integer);
var H, Save, K: Integer;
begin
  Save := FS.FreeReg;
  H := HomeRegister;
  if H < 0 then
  begin
    H := AllocReg;
    LoadIdent('%home', H);
  end;
  { 13.3.7.1 step 2: GetThisBinding (a ReferenceError before super()) comes before the key expression }
  if InDerivedCtor then
  begin
    K := AllocReg;
    LoadThis(K);
  end;
  if Assigned(KeyExpr) then
  begin
    { the key's ToPropertyKey comes after GetSuperBase (13.3.7.3, 6.2.5.5 GetValue): opGetSuper does both }
    K := ExprAny(KeyExpr);
    Emit(Ins(opGetSuper, Dest, H, -1 - K));
  end
  else
    Emit(Ins(opGetSuper, Dest, H, NewIC(Key)));
  FreeTo(Save);
end;

{ 13.3.7.1 a SuperProperty evaluated as a reference: this (GetThisBinding: a ReferenceError before super() in a
  derived constructor), then the key's value (ToPropertyKey comes with GetValue / PutValue), then GetSuperBase.
  Base and Base + 1 hold the base and this; KeyOp is the C operand of opGetSuperB / opSetSuperB (an IC index, or
  -1 - the key's register) }
procedure TCompiler.EvalSuperRef(const Key: UnicodeString; KeyExpr: TGocciaExpression; var Base, KeyOp: Integer);
var H, K: Integer;
begin
  Base := AllocRegs(2);
  LoadThis(Base + 1);
  if Assigned(KeyExpr) then
  begin
    K := AllocReg;
    ExprTo(KeyExpr, K);
    KeyOp := -1 - K;
  end
  else KeyOp := NewIC(Key);
  H := HomeRegister;
  if H < 0 then
  begin
    H := AllocReg;
    LoadIdent('%home', H);
  end;
  Emit(Ins(opSuperBase, Base, H, 0));
end;

{ the function just compiled (the newest child template) reads its home object (super) }
function TCompiler.LastChildUsesHome: Boolean;
var T: PFuncTemplate;
begin
  Result := False;
  if Length(FS.Tpl^.Children) = 0 then Exit;
  T := FS.Tpl^.Children[High(FS.Tpl^.Children)];
  Result := T^.HomeReg >= 0;
end;

{ 13.5.1.2 delete IdentifierReference (non-strict code): a with object's or the global object's property is
  deleted; a declarative binding is not (false); an unresolvable name gives true }
{ the name arguments is a function's arguments object binding here (as LoadIdentStatic resolves it) }
function TCompiler.ArgumentsBound: Boolean;
var F: TFuncState;
begin
  F := FS;
  while (F <> nil) and F.IsArrow do F := F.Parent;
  if (F <> nil) and F.IsEvalOuter then Exit(ResolveUpval(FS, '%arguments') >= 0);
  Result := (F <> nil) and ((F.Parent <> nil) or (Goal = 0));
end;

procedure TCompiler.CompileDeleteIdent(const N: UnicodeString; Dest: Integer);
var C, I, T, J, R: Integer; Ends: TJumpList;
begin
  SetLength(Ends, 0);
  if WithCount > 0 then
  begin
    C := WithsFor(N, WithIdxScratch);
    for I := 0 to C - 1 do
    begin
      T := AllocRegs(2);
      LoadIdentStatic(WithNames[WithIdxScratch[I]], T);
      Emit(Ins(opWithHas, T + 1, T, AtomConst(N)));
      J := EmitJ(opJF, T + 1, 0);
      Emit(Ins(opDelProp, Dest, T, AtomConst(N)));
      SetLength(Ends, Length(Ends) + 1);
      Ends[High(Ends)] := EmitJ(opJmp, 0, 0);
      PatchHere(J);
      FreeTo(T);
    end;
  end;
  if ResolveLocal(FS, N, R) or (ResolveUpval(FS, N) >= 0) or ((Goal = 3) and (ModImportSlot(N) >= 0)) or
     ((N = 'arguments') and ArgumentsBound) then
    Emit(Ins(opLoadFalse, Dest, 0, 0))
  else
    Emit(Ins(opDelGlobal, Dest, 0, AtomConst(N)));
  for I := 0 to High(Ends) do PatchHere(Ends[I]);
end;

procedure TCompiler.EmitSuperSet(const Key: UnicodeString; KeyExpr, Value: TGocciaExpression; Dest: Integer);
var H, Save, K, V: Integer;
begin
  Save := FS.FreeReg;
  H := HomeRegister;
  if H < 0 then
  begin
    H := AllocReg;
    LoadIdent('%home', H);
  end;
  V := AllocRegs(2);
  LoadThis(V + 1);
  if Assigned(KeyExpr) then
  begin
    K := ExprAny(KeyExpr);
    Emit(Ins(opToPropKey, K, K, 0));
  end
  else K := -1;
  ExprTo(Value, V);
  if K >= 0 then Emit(Ins(opSetSuper, V, H, -1 - K)) else Emit(Ins(opSetSuper, V, H, NewIC(Key)));
  if Dest <> V then Emit(Ins(opMov, Dest, V, 0));
  FreeTo(Save);
end;

{ a chain node starts: IsLink (the result) when it continues an enclosing node's chain, else it is the outermost
  and gets a fresh jump list (the enclosing one saved) }
function TCompiler.ChainBegin(var Saved: TJumpList): Boolean;
begin
  Result := ChainLink;
  ChainLink := False;
  if not Result then
  begin
    Saved := ChainJumps;
    SetLength(ChainJumps, 0);
  end;
end;

{ the outermost node of a chain: its short-circuits land here and give Dest undefined }
procedure TCompiler.ChainEnd(IsLink: Boolean; var Saved: TJumpList; Dest: Integer);
var J, I: Integer;
begin
  if IsLink then Exit;
  if Length(ChainJumps) > 0 then
  begin
    J := EmitJ(opJmp, 0, 0);
    for I := 0 to High(ChainJumps) do PatchHere(ChainJumps[I]);
    Emit(Ins(opLoadUndef, Dest, 0, 0));
    PatchHere(J);
  end;
  ChainJumps := Saved;
end;

{ the object or callee E of a chain node is a link of its chain when it is a member, call or private member
  expression (a parenthesized one is a sequence: it ends its own chain) }
procedure TCompiler.ChainObject(E: TGocciaExpression);
begin
  ChainLink := (E is TGocciaMemberExpression) or (E is TGocciaCallExpression) or (E is TGocciaPrivateMemberExpression);
end;

procedure TCompiler.ChainAdd(J: Integer);
begin
  SetLength(ChainJumps, Length(ChainJumps) + 1);
  ChainJumps[High(ChainJumps)] := J;
end;

procedure TCompiler.CompileMember(E: TGocciaMemberExpression; Dest: Integer);
var Save, O, K: Integer; IsLink: Boolean; SavedJ: TJumpList;
begin
  if E.ObjectExpr is TGocciaSuperExpression then
  begin
    ChainLink := False;
    if E.Computed then CompileSuperMember('', E.PropertyExpression, Dest)
    else CompileSuperMember(E.PropertyName, nil, Dest);
    Exit;
  end;
  IsLink := ChainBegin(SavedJ);
  Save := FS.FreeReg;
  ChainObject(E.ObjectExpr);
  O := ExprAny(E.ObjectExpr);
  ChainLink := False;
  if E.Optional then ChainAdd(EmitJ(opJNullish, O, 0));
  if E.Computed then
  begin
    K := ExprAny(E.PropertyExpression);
    Emit(Ins(opGetElem, Dest, O, K));
  end
  else if E.PropertyName = 'length' then
    Emit(Ins(opGetLength, Dest, O, NewIC('length')))
  else
    Emit(Ins(opGetProp, Dest, O, NewIC(E.PropertyName)));
  ChainEnd(IsLink, SavedJ, Dest);
  FreeTo(Save);
end;

procedure TCompiler.CompileObject(E: TGocciaObjectExpression; Dest: Integer);
var Obj, V, K, I, Save, N, ProtoCount: Integer; P: TGocciaPropertySourceOrder; T: PFuncTemplate; NoParams: TGocciaParameterArray; ProtoSetter: Boolean;
  Pair: TPair<TGocciaExpression, TGocciaExpression>;
begin
  Save := FS.FreeReg;
  Obj := AllocReg;
  N := Length(E.PropertySourceOrder);
  if N > 60 then N := 60;
  Emit(Ins(opNewObj, Obj, 0, N));
  V := AllocReg;
  K := AllocReg;
  SetLength(NoParams, 0);
  { 13.2.5.1 early errors of an object literal that is not an assignment pattern: a CoverInitializedName
    (a shorthand property with an initializer) and a second __proto__: v }
  ProtoCount := 0;
  for I := 0 to High(E.PropertySourceOrder) do
  begin
    P := E.PropertySourceOrder[I];
    if P.PropertyType <> pstStatic then Continue;
    if not P.UsesColonSyntax and (P.Expression is TGocciaAssignmentExpression) then
      raise EEarlySyntaxError.Create('Invalid shorthand property initializer');
    if (P.StaticKey = '__proto__') and P.UsesColonSyntax and not (P.Expression is TGocciaObjectMethodDefinition) then
    begin
      Inc(ProtoCount);
      if ProtoCount > 1 then raise EEarlySyntaxError.Create('Duplicate __proto__ fields are not allowed in object literals');
    end;
  end;
  for I := 0 to High(E.PropertySourceOrder) do
  begin
    P := E.PropertySourceOrder[I];
    case P.PropertyType of
      pstStatic:
        begin
          { 13.2.5.5: __proto__: v (not a shorthand, not a method) sets the prototype and names nothing }
          ProtoSetter := (P.StaticKey = '__proto__') and not (P.Expression is TGocciaObjectMethodDefinition) and
            not ((P.Expression is TGocciaIdentifierExpression) and (TGocciaIdentifierExpression(P.Expression).Name = '__proto__'));
          if ProtoSetter and not (P.Expression is TGocciaObjectMethodDefinition) then
            ExprTo(P.Expression, V)
          else if (P.Expression is TGocciaFunctionExpression) or (P.Expression is TGocciaArrowFunctionExpression) or
             (P.Expression is TGocciaObjectMethodDefinition) then
          begin
            CompileFuncExpr(P.Expression, V, P.StaticKey);
            { 15.4.4 DefineMethod: a method's [[HomeObject]] is the object (super reads through it) }
            if (P.Expression is TGocciaObjectMethodDefinition) and LastChildUsesHome then Emit(Ins(opSetHome, V, Obj, 0));
          end
          else if (P.Expression is TGocciaClassExpression) and (TGocciaClassExpression(P.Expression).ClassDefinition.Name = '') then
            CompileClass(TGocciaClassExpression(P.Expression).ClassDefinition, V, P.StaticKey)
          else
            ExprTo(P.Expression, V);
          if ProtoSetter then Emit(Ins(opSetProtoLit, Obj, V, 0))
          else Emit(Ins(opDefProp, Obj, V, AtomConst(P.StaticKey)));
        end;
      pstComputed:
        begin
          Pair := E.ComputedPropertiesInOrder[P.ComputedIndex];
          if Pair.Key is TGocciaSpreadExpression then
          begin
            ExprTo(TGocciaSpreadExpression(Pair.Key).Argument, V);
            Emit(Ins(opObjSpread, Obj, V, 0));
          end
          else
          begin
            ExprTo(Pair.Key, K);
            Emit(Ins(opToPropKey, K, K, 0));
            ExprTo(Pair.Value, V);
            if (Pair.Value is TGocciaObjectMethodDefinition) and LastChildUsesHome then Emit(Ins(opSetHome, V, Obj, 0));
            { 13.2.5.5: an anonymous function definition takes the key as its name }
            if IsAnonymousFunctionDefinition(Pair.Value) or (Pair.Value is TGocciaObjectMethodDefinition) then
              Emit(Ins(opSetFnName, V, K, 0));
            Emit(Ins(opDefPropC, Obj, K, V));
          end;
        end;
      pstGetter:
        begin
          ObjLiteralMethod := True;
          { an accessor's own "use strict" directive (10.2.1.1: its code is strict mode code) }
          if HasUseStrictDirective(TGocciaGetterExpression(P.Accessor).Body) then PendingStrict := 1;
          NextSloppyEval := deuSloppyDirectEval in TGocciaGetterExpression(P.Accessor).DirectEvalUsage;
          T := CompileFunction('get ' + P.StaticKey, NoParams, TGocciaGetterExpression(P.Accessor).Body,
            False, True, False, False, False, False);
          T^.SourceRange := TGocciaGetterExpression(P.Accessor).SourceRange;
          EmitClosure(V, T);
          Emit(Ins(opDefGetter, Obj, V or $8000, AtomConst(P.StaticKey)));
        end;
      pstSetter:
        begin
          ObjLiteralMethod := True;
          if HasUseStrictDirective(TGocciaSetterExpression(P.Accessor).Body) then PendingStrict := 1;
          NextSloppyEval := deuSloppyDirectEval in TGocciaSetterExpression(P.Accessor).DirectEvalUsage;
          T := CompileFunction('set ' + P.StaticKey, TGocciaSetterExpression(P.Accessor).Parameters,
            TGocciaSetterExpression(P.Accessor).Body, False, True, False, False, False, False);
          T^.SourceRange := TGocciaSetterExpression(P.Accessor).SourceRange;
          EmitClosure(V, T);
          Emit(Ins(opDefSetter, Obj, V or $8000, AtomConst(P.StaticKey)));
        end;
      pstComputedGetter, pstComputedSetter:
        begin
          Pair := E.ComputedPropertiesInOrder[P.ComputedIndex];
          ExprTo(Pair.Key, K);
          if P.PropertyType = pstComputedGetter then
          begin
            ObjLiteralMethod := True;
            if HasUseStrictDirective(TGocciaGetterExpression(Pair.Value).Body) then PendingStrict := 1;
            NextSloppyEval := deuSloppyDirectEval in TGocciaGetterExpression(Pair.Value).DirectEvalUsage;
            T := CompileFunction('get', NoParams, TGocciaGetterExpression(Pair.Value).Body,
              False, True, False, False, False, False);
            T^.SourceRange := TGocciaGetterExpression(Pair.Value).SourceRange;
            EmitClosure(V, T);
            Emit(Ins(opSetFnName, V, K, 1));
            Emit(Ins(opDefGetterC, Obj, V or $8000, K));
          end
          else
          begin
            ObjLiteralMethod := True;
            if HasUseStrictDirective(TGocciaSetterExpression(Pair.Value).Body) then PendingStrict := 1;
            NextSloppyEval := deuSloppyDirectEval in TGocciaSetterExpression(Pair.Value).DirectEvalUsage;
            T := CompileFunction('set', TGocciaSetterExpression(Pair.Value).Parameters,
              TGocciaSetterExpression(Pair.Value).Body, False, True, False, False, False, False);
            T^.SourceRange := TGocciaSetterExpression(Pair.Value).SourceRange;
            EmitClosure(V, T);
            Emit(Ins(opSetFnName, V, K, 2));
            Emit(Ins(opDefSetterC, Obj, V or $8000, K));
          end;
        end;
    else
      raise ECompileError.Create('unsupported object literal member');
    end;
  end;
  if Dest <> Obj then Emit(Ins(opMov, Dest, Obj, 0));
  FreeTo(Save);
end;

procedure TCompiler.CompileArray(E: TGocciaArrayExpression; Dest: Integer);
var Arr, V, I, Save: Integer; Simple: Boolean;
begin
  Save := FS.FreeReg;
  Simple := E.Elements.Count <= 64;
  for I := 0 to E.Elements.Count - 1 do
    if (E.Elements[I] is TGocciaSpreadExpression) or (E.Elements[I] is TGocciaHoleExpression) then Simple := False;
  if Simple and (E.Elements.Count > 0) then
  begin
    Arr := AllocReg;
    for I := 0 to E.Elements.Count - 1 do
    begin
      V := AllocReg;
      ExprTo(E.Elements[I], V);
    end;
    Emit(Ins(opNewArr, Arr, E.Elements.Count, Arr + 1));
    if Dest <> Arr then Emit(Ins(opMov, Dest, Arr, 0));
    FreeTo(Save);
    Exit;
  end;
  Arr := AllocReg;
  Emit(Ins(opNewArr, Arr, 0, 0));
  V := AllocReg;
  for I := 0 to E.Elements.Count - 1 do
  begin
    if E.Elements[I] is TGocciaSpreadExpression then
    begin
      ExprTo(TGocciaSpreadExpression(E.Elements[I]).Argument, V);
      Emit(Ins(opArrSpread, Arr, V, 0));
    end
    else if E.Elements[I] is TGocciaHoleExpression then
      Emit(Ins(opArrHole, Arr, 0, 0))
    else
    begin
      ExprTo(E.Elements[I], V);
      Emit(Ins(opArrPush, Arr, V, 0));
    end;
  end;
  if Dest <> Arr then Emit(Ins(opMov, Dest, Arr, 0));
  FreeTo(Save);
end;

procedure TCompiler.CompileTemplate(E: TGocciaTemplateWithInterpolationExpression; Dest: Integer);
var Save, T, I: Integer;
begin
  Save := FS.FreeReg;
  if E.Parts.Count = 0 then begin LoadString('', Dest); Exit; end;
  if IsVarReg(Dest) then
  begin
    T := AllocReg;
    CompileTemplate(E, T);
    Emit(Ins(opMov, Dest, T, 0));
    FreeTo(Save);
    Exit;
  end;
  T := AllocReg;
  LoadString('', Dest);
  for I := 0 to E.Parts.Count - 1 do
  begin
    ExprTo(E.Parts[I], T);
    Emit(Ins(opToStr, T, T, 0));
    Emit(Ins(opAdd, Dest, Dest, T));
  end;
  FreeTo(Save);
end;

// a tagged template calls the tag with (strings, ...values); strings has a raw property
procedure TCompiler.CompileTagged(E: TGocciaTaggedTemplateExpression; Dest: Integer);
var Base, I, Strs, Raw, V: Integer; M: TGocciaMemberExpression; K, Site, JMade: Integer;
begin
  Base := FS.FreeReg;
  AllocRegs(2);
  if E.Tag is TGocciaMemberExpression then
  begin
    M := TGocciaMemberExpression(E.Tag);
    ExprTo(M.ObjectExpr, Base + 1);
    if M.Computed then
    begin
      K := AllocReg;
      ExprTo(M.PropertyExpression, K);
      Emit(Ins(opGetElem, Base, Base + 1, K));
      FreeTo(Base + 2);
    end
    else
      Emit(Ins(opGetProp, Base, Base + 1, NewIC(M.PropertyName)));
  end
  else
  begin
    ExprTo(E.Tag, Base);
    Emit(Ins(opLoadUndef, Base + 1, 0, 0));
  end;
  Strs := AllocReg;
  { 13.2.8.4 GetTemplateObject: one frozen template object per site and realm }
  Site := TemplateSiteNext;
  Inc(TemplateSiteNext);
  Emit(Ins(opTemplateObj, Strs, 0, Site));
  JMade := EmitJ(opJNotUndef, Strs, 0);
  Emit(Ins(opNewArr, Strs, 0, 0));
  V := AllocReg;
  for I := 0 to High(E.CookedStrings) do
  begin
    if (I <= High(E.CookedValid)) and not E.CookedValid[I] then Emit(Ins(opLoadUndef, V, 0, 0))
    else LoadString(E.CookedStrings[I], V);
    Emit(Ins(opArrPush, Strs, V, 0));
  end;
  Raw := V;
  Emit(Ins(opNewArr, Raw, 0, 0));
  K := AllocReg;
  for I := 0 to High(E.RawStrings) do
  begin
    LoadString(E.RawStrings[I], K);
    Emit(Ins(opArrPush, Raw, K, 0));
  end;
  Emit(Ins(opDefMethod, Strs, Raw, AtomConst('raw')));
  FreeTo(Strs + 1);
  Emit(Ins(opTemplateObj, Strs, 1, Site));
  PatchHere(JMade);
  for I := 0 to E.Expressions.Count - 1 do
  begin
    V := AllocReg;
    ExprTo(E.Expressions[I], V);
  end;
  Emit(Ins(opCall, Base, 1 + E.Expressions.Count, 0));
  if Dest <> Base then Emit(Ins(opMov, Dest, Base, 0));
  FreeTo(Base);
end;

{ assignment target with the value in Src }
procedure TCompiler.AssignToTarget(T: TGocciaExpression; Src: Integer);
var Save, O, K: Integer; M: TGocciaMemberExpression;
begin
  Save := FS.FreeReg;
  if T is TGocciaIdentifierExpression then StoreIdent(TGocciaIdentifierExpression(T).Name, Src)
  else if (T is TGocciaMemberExpression) and (TGocciaMemberExpression(T).ObjectExpr is TGocciaSuperExpression) then
  begin
    M := TGocciaMemberExpression(T);
    if M.Computed then EvalSuperRef('', M.PropertyExpression, O, K) else EvalSuperRef(M.PropertyName, nil, O, K);
    Emit(Ins(opSetSuperB, Src, O, K));
  end
  else if T is TGocciaMemberExpression then
  begin
    M := TGocciaMemberExpression(T);
    O := ExprAny(M.ObjectExpr);
    if M.Computed then
    begin
      K := ExprAny(M.PropertyExpression);
      Emit(Ins(opSetElem, O, K, Src));
    end
    else
      Emit(Ins(opSetProp, O, Src, NewIC(M.PropertyName)));
  end
  else if T is TGocciaPrivateMemberExpression then
  begin
    O := ExprAny(TGocciaPrivateMemberExpression(T).ObjectExpr);
    K := AllocReg;
    LoadIdent(PrivKeyName(TGocciaPrivateMemberExpression(T).PrivateName), K);
    Emit(Ins(opSetPriv, O, Src, K));
  end
  else if T is TGocciaDestructuringPattern then
    BindPattern(TGocciaDestructuringPattern(T), Src, False, False)
  else
    raise ECompileError.Create('unsupported assignment target ' + T.ClassName);
  FreeTo(Save);
end;

procedure TCompiler.CompileAssign(E: TGocciaExpression; Dest: Integer);
var Save, O, K, V, R, J: Integer; A: TGocciaAssignmentExpression; PA: TGocciaPropertyAssignmentExpression;
  CA: TGocciaComputedPropertyAssignmentExpression; CO: TGocciaCompoundAssignmentExpression;
  PC: TGocciaPropertyCompoundAssignmentExpression; CC: TGocciaComputedPropertyCompoundAssignmentExpression;
  Op: TGocciaTokenType; IC: Integer; AName: UnicodeString; Discard: Boolean; BOp: TOp;
  { Name: the identifier a logical assignment assigns to, for the NamedEvaluation of an anonymous function or class
    (13.15.2: &&=, ||= and ??= only) }
  procedure Combine(Op: TGocciaTokenType; Cur, ValE: TGocciaExpression; CurReg, Dst: Integer; out ShortJ: Integer;
    const Name: UnicodeString = '');
  begin
    ShortJ := -1;
    case Op of
      gttLogicalAndAssign: begin ShortJ := EmitJ(opJF, CurReg, 0); CompileFieldValue(ValE, Dst, Name); end;
      gttLogicalOrAssign: begin ShortJ := EmitJ(opJT, CurReg, 0); CompileFieldValue(ValE, Dst, Name); end;
      gttNullishCoalescingAssign: begin ShortJ := EmitJ(opJNotNullish, CurReg, 0); CompileFieldValue(ValE, Dst, Name); end;
    else
      begin
        V := AllocReg;
        ExprTo(ValE, V);
        Emit(Ins(BinOpFor(Op), Dst, CurReg, V));
      end;
    end;
  end;
begin
  Save := FS.FreeReg;
  Discard := TopDiscard;
  TopDiscard := False;
  if E is TGocciaAssignmentExpression then
  begin
    A := TGocciaAssignmentExpression(E);
    { 13.15.2 step 1.c: NamedEvaluation only when the target IsIdentifierRef (a parenthesized one is not) }
    AName := A.Name;
    if Assigned(A.Target) and A.Target.Parenthesized then AName := '';
    if ResolveLocal(FS, A.Name, R) and not BindingIsConst(A.Name) and not InTDZ(A.Name) and
       ((WithCount = 0) or (WithsFor(A.Name, WithIdxScratch) = 0)) then
    begin
      if (A.Value is TGocciaFunctionExpression) or (A.Value is TGocciaArrowFunctionExpression) then CompileFuncExpr(A.Value, R, AName)
      else if (A.Value is TGocciaClassExpression) and (TGocciaClassExpression(A.Value).ClassDefinition.Name = '') then
        CompileClass(TGocciaClassExpression(A.Value).ClassDefinition, R, AName)
      else ExprTo(A.Value, R);
      if (Dest <> R) and not Discard then Emit(Ins(opMov, Dest, R, 0));
    end
    else if WithsFor(A.Name, WithIdxScratch) > 0 then
    begin
      { 13.15.2 step 1.a: the reference is resolved before the value is evaluated }
      K := AllocReg;
      ResolveWithEnv(A.Name, K);
      CompileFieldValue(A.Value, Dest, AName);
      StoreIdentRef(A.Name, K, Dest);
    end
    else
    begin
      CompileFieldValue(A.Value, Dest, AName);   { NamedEvaluation of an anonymous function or class }
      StoreIdent(A.Name, Dest);
    end;
  end
  else if (E is TGocciaPropertyAssignmentExpression) and (TGocciaPropertyAssignmentExpression(E).ObjectExpr is TGocciaSuperExpression) then
  begin
    { 13.15.2 super.name = v: this first (MakeSuperPropertyReference), then the value; [[Set]] on the home object's
      prototype with this as the receiver }
    PA := TGocciaPropertyAssignmentExpression(E);
    EmitSuperSet(PA.PropertyName, nil, PA.Value, Dest);
  end
  else if (E is TGocciaComputedPropertyAssignmentExpression) and (TGocciaComputedPropertyAssignmentExpression(E).ObjectExpr is TGocciaSuperExpression) then
  begin
    CA := TGocciaComputedPropertyAssignmentExpression(E);
    EmitSuperSet('', CA.PropertyExpression, CA.Value, Dest);
  end
  else if E is TGocciaPropertyAssignmentExpression then
  begin
    { the base is read before the value: a local's own register only when the value cannot run code that
      reassigns it }
    PA := TGocciaPropertyAssignmentExpression(E);
    if PureRead(PA.Value) then O := ExprAny(PA.ObjectExpr) else begin O := AllocReg; ExprTo(PA.ObjectExpr, O); end;
    if O = Dest then begin O := AllocReg; Emit(Ins(opMov, O, Dest, 0)); end;
    if Discard and PureRead(PA.Value) then V := ExprAny(PA.Value)
    else begin ExprTo(PA.Value, Dest); V := Dest; end;
    Emit(Ins(opSetProp, O, V, NewIC(PA.PropertyName)));
  end
  else if E is TGocciaComputedPropertyAssignmentExpression then
  begin
    { 13.15.2: base, then key, then value; locals' own registers where nothing evaluated later can reassign them }
    CA := TGocciaComputedPropertyAssignmentExpression(E);
    if PureRead(CA.Value) and PureRead(CA.PropertyExpression) then O := ExprAny(CA.ObjectExpr)
    else begin O := AllocReg; ExprTo(CA.ObjectExpr, O); end;
    if PureRead(CA.Value) then K := ExprAny(CA.PropertyExpression)
    else begin K := AllocReg; ExprTo(CA.PropertyExpression, K); end;
    if (O = Dest) or (K = Dest) then
    begin
      R := AllocReg;
      ExprTo(CA.Value, R);
      Emit(Ins(opSetElem, O, K, R));
      if not Discard then Emit(Ins(opMov, Dest, R, 0));
    end
    else
    begin
      if Discard and PureRead(CA.Value) then V := ExprAny(CA.Value)
      else begin ExprTo(CA.Value, Dest); V := Dest; end;
      Emit(Ins(opSetElem, O, K, V));
    end;
  end
  else if (E is TGocciaCompoundAssignmentExpression) and
          not (TGocciaCompoundAssignmentExpression(E).Operator in [gttLogicalAndAssign, gttLogicalOrAssign, gttNullishCoalescingAssign]) and
          PureRead(TGocciaCompoundAssignmentExpression(E).Value) and
          ResolveLocal(FS, TGocciaCompoundAssignmentExpression(E).Name, R) and
          not BindingIsConst(TGocciaCompoundAssignmentExpression(E).Name) and
          not InTDZ(TGocciaCompoundAssignmentExpression(E).Name) and
          ((WithCount = 0) or (WithsFor(TGocciaCompoundAssignmentExpression(E).Name, WithIdxScratch) = 0)) then
  begin
    { x op= v on a local, v a literal or a local: in place (nothing between the read of x and the operation can
      change it); x += k and x -= k for a small integer k as one immediate add }
    CO := TGocciaCompoundAssignmentExpression(E);
    BOp := BinOpFor(CO.Operator);
    if (BOp in [opAdd, opSub]) and (CO.Value is TGocciaLiteralExpression) and
       (TGocciaLiteralExpression(CO.Value).Value is TGocciaNumberLiteralValue) and
       IsInt(MkNumber(TGocciaNumberLiteralValue(TGocciaLiteralExpression(CO.Value).Value).Value)) and
       (Abs(IntOf(MkNumber(TGocciaNumberLiteralValue(TGocciaLiteralExpression(CO.Value).Value).Value))) < 4000000) then
    begin
      K := IntOf(MkNumber(TGocciaNumberLiteralValue(TGocciaLiteralExpression(CO.Value).Value).Value));
      if BOp = opAdd then Emit(Ins(opAddI, R, R, K)) else Emit(Ins(opSubI, R, R, -K));
    end
    else
    begin
      V := ExprAny(CO.Value);
      Emit(Ins(BOp, R, R, V));
    end;
    if (Dest <> R) and not Discard then Emit(Ins(opMov, Dest, R, 0));
  end
  else if E is TGocciaCompoundAssignmentExpression then
  begin
    CO := TGocciaCompoundAssignmentExpression(E);
    R := AllocReg;
    if WithsFor(CO.Name, WithIdxScratch) > 0 then
    begin
      K := AllocReg;
      LoadIdentThis(CO.Name, R, K);
      Combine(CO.Operator, nil, CO.Value, R, R, J, CO.Name);
      StoreIdentRef(CO.Name, K, R);
    end
    else
    begin
      LoadIdent(CO.Name, R);
      Combine(CO.Operator, nil, CO.Value, R, R, J, CO.Name);
      StoreIdent(CO.Name, R);
    end;
    if J >= 0 then PatchHere(J);
    Emit(Ins(opMov, Dest, R, 0));
  end
  else if (E is TGocciaPropertyCompoundAssignmentExpression) and
          (TGocciaPropertyCompoundAssignmentExpression(E).ObjectExpr is TGocciaSuperExpression) then
  begin
    PC := TGocciaPropertyCompoundAssignmentExpression(E);
    EvalSuperRef(PC.PropertyName, nil, O, K);
    R := AllocReg;
    Emit(Ins(opGetSuperB, R, O, K));
    Combine(PC.Operator, nil, PC.Value, R, R, J);
    Emit(Ins(opSetSuperB, R, O, K));
    if J >= 0 then PatchHere(J);
    Emit(Ins(opMov, Dest, R, 0));
  end
  else if (E is TGocciaComputedPropertyCompoundAssignmentExpression) and
          (TGocciaComputedPropertyCompoundAssignmentExpression(E).ObjectExpr is TGocciaSuperExpression) then
  begin
    CC := TGocciaComputedPropertyCompoundAssignmentExpression(E);
    EvalSuperRef('', CC.PropertyExpression, O, K);
    R := AllocReg;
    Emit(Ins(opGetSuperB, R, O, K));   { converts the key in place: PutValue does not repeat ToPropertyKey }
    Combine(CC.Operator, nil, CC.Value, R, R, J);
    Emit(Ins(opSetSuperB, R, O, K));
    if J >= 0 then PatchHere(J);
    Emit(Ins(opMov, Dest, R, 0));
  end
  else if E is TGocciaPropertyCompoundAssignmentExpression then
  begin
    PC := TGocciaPropertyCompoundAssignmentExpression(E);
    O := AllocReg; ExprTo(PC.ObjectExpr, O);
    R := AllocReg;
    IC := NewIC(PC.PropertyName);
    Emit(Ins(opGetProp, R, O, IC));
    Combine(PC.Operator, nil, PC.Value, R, R, J);
    Emit(Ins(opSetProp, O, R, NewIC(PC.PropertyName)));
    if J >= 0 then PatchHere(J);
    Emit(Ins(opMov, Dest, R, 0));
  end
  else if E is TGocciaComputedPropertyCompoundAssignmentExpression then
  begin
    CC := TGocciaComputedPropertyCompoundAssignmentExpression(E);
    O := AllocReg; ExprTo(CC.ObjectExpr, O);
    K := AllocReg; ExprTo(CC.PropertyExpression, K);
    Emit(Ins(opToPropKey, K, K, O + 1));   { C: the base, ToObject'd first (GetValue) }
    R := AllocReg;
    Emit(Ins(opGetElem, R, O, K));
    Combine(CC.Operator, nil, CC.Value, R, R, J);
    Emit(Ins(opSetElem, O, K, R));
    if J >= 0 then PatchHere(J);
    Emit(Ins(opMov, Dest, R, 0));
  end
  else if E is TGocciaPrivatePropertyAssignmentExpression then
  begin
    O := AllocReg; ExprTo(TGocciaPrivatePropertyAssignmentExpression(E).ObjectExpr, O);
    K := AllocReg; LoadIdent(PrivKeyName(TGocciaPrivatePropertyAssignmentExpression(E).PrivateName), K);
    CompileFieldValue(TGocciaPrivatePropertyAssignmentExpression(E).Value, Dest, '');
    Emit(Ins(opSetPriv, O, Dest, K));
  end
  else if E is TGocciaPrivatePropertyCompoundAssignmentExpression then
  begin
    O := AllocReg; ExprTo(TGocciaPrivatePropertyCompoundAssignmentExpression(E).ObjectExpr, O);
    K := AllocReg; LoadIdent(PrivKeyName(TGocciaPrivatePropertyCompoundAssignmentExpression(E).PrivateName), K);
    R := AllocReg;
    Emit(Ins(opGetPriv, R, O, K));
    Combine(TGocciaPrivatePropertyCompoundAssignmentExpression(E).Operator, nil,
      TGocciaPrivatePropertyCompoundAssignmentExpression(E).Value, R, R, J);
    Emit(Ins(opSetPriv, O, R, K));
    if J >= 0 then PatchHere(J);
    Emit(Ins(opMov, Dest, R, 0));
  end
  else if E is TGocciaDestructuringAssignmentExpression then
  begin
    ExprTo(TGocciaDestructuringAssignmentExpression(E).Right, Dest);
    R := AllocReg;
    Emit(Ins(opMov, R, Dest, 0));
    BindPattern(TGocciaDestructuringAssignmentExpression(E).Left, R, False, False);
  end;
  FreeTo(Save);
end;

procedure TCompiler.CompileIncrement(E: TGocciaIncrementExpression; Dest: Integer);
var Save, O, K, Cur, Nw: Integer; M: TGocciaMemberExpression; Delta: Integer; Name: UnicodeString; R: Integer;
  Discard: Boolean;
begin
  Save := FS.FreeReg;
  Discard := TopDiscard;
  TopDiscard := False;
  if E.Operator = gttIncrement then Delta := 1 else Delta := -1;
  if E.Operand is TGocciaIdentifierExpression then
  begin
    Name := TGocciaIdentifierExpression(E.Operand).Name;
    if ResolveLocal(FS, Name, R) and not BindingIsConst(Name) and not InTDZ(Name) and
       ((WithCount = 0) or (WithsFor(Name, WithIdxScratch) = 0)) then
    begin
      { an unused x++ is ++x: no copy of the old value }
      if E.IsPrefix or Discard then
      begin
        Emit(Ins(opIncI, R, R, Delta));
        if (Dest <> R) and not Discard then Emit(Ins(opMov, Dest, R, 0));
      end
      else
      begin
        Emit(Ins(opToNumeric, Dest, R, 0));
        Emit(Ins(opIncI, R, Dest, Delta));
      end;
      FreeTo(Save);
      Exit;
    end;
    Cur := AllocReg; Nw := AllocReg;
    if WithsFor(Name, WithIdxScratch) > 0 then
    begin
      K := AllocReg;
      LoadIdentThis(Name, Cur, K);
      Emit(Ins(opToNumeric, Cur, Cur, 0));
      Emit(Ins(opIncI, Nw, Cur, Delta));
      StoreIdentRef(Name, K, Nw);
    end
    else
    begin
      LoadIdent(Name, Cur);
      Emit(Ins(opToNumeric, Cur, Cur, 0));
      Emit(Ins(opIncI, Nw, Cur, Delta));
      StoreIdent(Name, Nw);
    end;
  end
  else if (E.Operand is TGocciaMemberExpression) and (TGocciaMemberExpression(E.Operand).ObjectExpr is TGocciaSuperExpression) then
  begin
    M := TGocciaMemberExpression(E.Operand);
    if M.Computed then EvalSuperRef('', M.PropertyExpression, O, K) else EvalSuperRef(M.PropertyName, nil, O, K);
    Cur := AllocReg; Nw := AllocReg;
    Emit(Ins(opGetSuperB, Cur, O, K));
    Emit(Ins(opToNumeric, Cur, Cur, 0));
    Emit(Ins(opIncI, Nw, Cur, Delta));
    Emit(Ins(opSetSuperB, Nw, O, K));
  end
  else if E.Operand is TGocciaMemberExpression then
  begin
    M := TGocciaMemberExpression(E.Operand);
    O := AllocReg; ExprTo(M.ObjectExpr, O);
    Cur := AllocReg; Nw := AllocReg;
    if M.Computed then
    begin
      K := AllocReg; ExprTo(M.PropertyExpression, K);
      Emit(Ins(opToPropKey, K, K, O + 1));
      Emit(Ins(opGetElem, Cur, O, K));
      Emit(Ins(opToNumeric, Cur, Cur, 0));
      Emit(Ins(opIncI, Nw, Cur, Delta));
      Emit(Ins(opSetElem, O, K, Nw));
    end
    else
    begin
      Emit(Ins(opGetProp, Cur, O, NewIC(M.PropertyName)));
      Emit(Ins(opToNumeric, Cur, Cur, 0));
      Emit(Ins(opIncI, Nw, Cur, Delta));
      Emit(Ins(opSetProp, O, Nw, NewIC(M.PropertyName)));
    end;
  end
  else if E.Operand is TGocciaPrivateMemberExpression then
  begin
    O := AllocReg; ExprTo(TGocciaPrivateMemberExpression(E.Operand).ObjectExpr, O);
    K := AllocReg; LoadIdent(PrivKeyName(TGocciaPrivateMemberExpression(E.Operand).PrivateName), K);
    Cur := AllocReg; Nw := AllocReg;
    Emit(Ins(opGetPriv, Cur, O, K));
    Emit(Ins(opToNumeric, Cur, Cur, 0));
    Emit(Ins(opIncI, Nw, Cur, Delta));
    Emit(Ins(opSetPriv, O, Nw, K));
  end
  else
    raise ECompileError.Create('unsupported increment target');
  if E.IsPrefix then Emit(Ins(opMov, Dest, Nw, 0)) else Emit(Ins(opMov, Dest, Cur, 0));
  FreeTo(Save);
end;

procedure TCompiler.CompileConditional(C: TGocciaConditionalExpression; Dest: Integer);
var J: Integer; FalseL: TJumpList;
begin
  NoBareYield(C.Condition);
  SetLength(FalseL, 0);
  BranchFalse(C.Condition, FalseL);
  ExprTo(C.Consequent, Dest);
  J := EmitJ(opJmp, 0, 0);
  PatchListHere(FalseL);
  ExprTo(C.Alternate, Dest);
  PatchHere(J);
end;

{ out of line: the message is a managed temporary, which would give the
  caller an implicit exception frame }
procedure TCompiler.RaiseUnsupported(const AWhat: UnicodeString; N: TObject);
begin
  raise ECompileError.Create('unsupported ' + AWhat + ' ' + N.ClassName);
end;


procedure TCompiler.ExprTo(E: TGocciaExpression; Dest: Integer);
var Save, R, J, J2, L: Integer; SE: TGocciaSequenceExpression;
  I: Integer; F: TFuncState; Msg: UnicodeString;
begin
  if E = nil then begin Emit(Ins(opLoadUndef, Dest, 0, 0)); Exit; end;
  L := E.Line;
  if L > 0 then FS.CurLine := L;
  case KindOf(ExprKinds, ExprClasses, E) of
    0: CompileLiteral(TGocciaLiteralExpression(E).Value, Dest);
    1: LoadIdent(TGocciaIdentifierExpression(E).Name, Dest);
    2: LoadThis(Dest);
    3: CompileBinary(TGocciaBinaryExpression(E), Dest);
    4: CompileUnary(TGocciaUnaryExpression(E), Dest);
    5: CompileCall(TGocciaCallExpression(E), Dest);
    6: CompileNew(TGocciaNewExpression(E), Dest);
    7: CompileMember(TGocciaMemberExpression(E), Dest);
    8: CompileObject(TGocciaObjectExpression(E), Dest);
    9: CompileArray(TGocciaArrayExpression(E), Dest);
    10, 11, 12: CompileFuncExpr(E, Dest);
    13: CompileClass(TGocciaClassExpression(E).ClassDefinition, Dest);
    14, 15: CompileAssign(E, Dest);
    16: CompileIncrement(TGocciaIncrementExpression(E), Dest);
    17: LoadString(TGocciaTemplateLiteralExpression(E).Value, Dest);
    18: CompileTemplate(TGocciaTemplateWithInterpolationExpression(E), Dest);
    19: CompileTagged(TGocciaTaggedTemplateExpression(E), Dest);
    20: CompileConditional(TGocciaConditionalExpression(E), Dest);
  else
  if E is TGocciaSequenceExpression then
  begin
    SE := TGocciaSequenceExpression(E);
    Save := FS.FreeReg;
    for I := 0 to SE.Expressions.Count - 2 do begin ExprAny(SE.Expressions[I]); FreeTo(Save); end;
    ExprTo(SE.Expressions[SE.Expressions.Count - 1], Dest);
  end
  else if E is TGocciaNewTargetExpression then
  begin
    if NewTargetOutside then raise EEarlySyntaxError.Create('new.target expression is not allowed here');
    if FS.IsArrow then
    begin
      { an arrow's new.target is its enclosing function's }
      F := FS;
      while (F.Parent <> nil) and F.IsArrow do F := F.Parent;
      if F.IsEvalOuter then
      begin
        { goal 4: eval code's new.target is its caller's }
        if ResolveUpval(FS, '%newtarget') >= 0 then LoadIdent('%newtarget', Dest)
        else Emit(Ins(opLoadUndef, Dest, 0, 0));
      end
      else if F.IsArrow or (F.Parent = nil) or F.Tpl^.IsAsync or F.Tpl^.IsGenerator then
        Emit(Ins(opLoadUndef, Dest, 0, 0))
      else
      begin
        F.NewTargetUsed := True;
        LoadIdent('%newtarget', Dest);
      end;
    end
    else
    begin
      if NewTargetOutside then raise EEarlySyntaxError.Create('new.target expression is not allowed here');
      R := NewTargetRegister;
      if R >= 0 then Emit(Ins(opMov, Dest, R, 0))
      else if NewTargetFromArrow then LoadIdent('%newtarget', Dest)
      else Emit(Ins(opLoadUndef, Dest, 0, 0));
    end;
  end
  else if E is TGocciaPrivateMemberExpression then
    CompilePrivateGet(TGocciaPrivateMemberExpression(E), Dest)
  else if E is TGocciaRegexLiteralExpression then
  begin
    { 13.2.7.2: the pattern and flags are early errors }
    if Assigned(RegExpValidateHook) then
    begin
      Msg := RegExpValidateHook(TGocciaRegexLiteralExpression(E).Pattern, TGocciaRegexLiteralExpression(E).Flags);
      if Msg <> '' then raise EEarlySyntaxError.Create(Msg);
    end;
    Emit(Ins(opRegExp, Dest, AtomConst(TGocciaRegexLiteralExpression(E).Pattern),
      AtomConst(TGocciaRegexLiteralExpression(E).Flags)));
  end
  else if E is TGocciaYieldExpression then
  begin
    { only a generator's own code yields: not its arrows, static blocks or field initializers }
    if not FS.Tpl^.IsGenerator or FS.Tpl^.IsArrow then raise EEarlySyntaxError.Create('Unexpected yield expression here');
    Save := FS.FreeReg;
    R := AllocReg;
    ExprTo(TGocciaYieldExpression(E).Operand, R);
    if TGocciaYieldExpression(E).IsDelegate and FS.Tpl^.IsAsync then
    begin
      { yield* in an async generator: R iterator, R+1 next, R+2 received value, R+3 mode, R+4 phase }
      AllocRegs(4);
      Emit(Ins(opGetAsyncIter, R, R, 0));
      Emit(Ins(opLoadUndef, R + 2, 0, 0));
      Emit(Ins(opLoadI, R + 3, 0, 0));
      Emit(Ins(opLoadI, R + 4, 0, 0));
      Emit(Ins(opYieldStarAsync, Dest, R, 0));
    end
    else if TGocciaYieldExpression(E).IsDelegate then
    begin
      { yield* (15.5.5): R iterator, R+1 next, R+2 received value, R+3 received mode }
      AllocRegs(3);
      Emit(Ins(opGetIter, R, R, 0));
      Emit(Ins(opLoadUndef, R + 2, 0, 0));
      Emit(Ins(opLoadI, R + 3, 0, 0));
      Emit(Ins(opYieldStar, Dest, R, 0));
    end
    else
    begin
      { 15.5.5: an async generator's yield awaits its operand first }
      if FS.Tpl^.IsAsync then Emit(Ins(opAwait, R, R, 0));
      Emit(Ins(opYield, Dest, R, 0));
    end;
    FreeTo(Save);
  end
  else if E is TGocciaAwaitExpression then
  begin
    { 15.7.1: a class static block's body is not async code, whatever encloses the class }
    F := FS;
    while (F <> nil) and F.IsArrow do F := F.Parent;
    if (F <> nil) and F.IsStaticBlock then raise EEarlySyntaxError.Create('await is not allowed in a class static block');
    Save := FS.FreeReg;
    R := AllocReg;
    ExprTo(TGocciaAwaitExpression(E).Operand, R);
    Emit(Ins(opAwait, Dest, R, 0));
    FreeTo(Save);
  end
  else
    if E is TGocciaImportCallExpression then
    begin
      Save := FS.FreeReg;
      R := AllocRegs(2);
      ExprTo(TGocciaImportCallExpression(E).Specifier, R);
      case TGocciaImportCallExpression(E).Phase of
        icpSource: L := 1;
        icpDefer: L := 2;
      else L := 0;
      end;
      if Assigned(TGocciaImportCallExpression(E).Options) then
      begin
        ExprTo(TGocciaImportCallExpression(E).Options, R + 1);
        Emit(Ins(opImport, Dest, R, R + 1) or (QWord(L) shl 56));
      end
      else Emit(Ins(opImport, Dest, R, $FFFF) or (QWord(L) shl 56));
      FreeTo(Save);
    end
    else if E is TGocciaImportMetaExpression then
    begin
      if Goal <> 3 then raise EEarlySyntaxError.Create('Cannot use ''import.meta'' outside a module');
      Emit(Ins(opImportMeta, Dest, 0, 0));
    end
    { super outside a property access or call (super.#x included): 15.7.1 / 13.3.7 early errors }
    else if E is TGocciaSuperExpression then
      raise EEarlySyntaxError.Create('''super'' keyword unexpected here')
    else
    RaiseUnsupported('expression', E);
  end;
end;

{ fall through when E is truthy; add jumps taken when falsy }
procedure TCompiler.BranchFalse(E: TGocciaExpression; var L: TJumpList);
var Save, A, B: Integer; BE: TGocciaBinaryExpression; TrueL: TJumpList; Op: TOp; V: TValue;
begin
  Save := FS.FreeReg;
  if (E is TGocciaUnaryExpression) and (TGocciaUnaryExpression(E).Operator = gttNot) then
  begin
    BranchTrue(TGocciaUnaryExpression(E).Operand, L);
    Exit;
  end;
  if E is TGocciaBinaryExpression then
  begin
    BE := TGocciaBinaryExpression(E);
    case BE.Operator of
      gttAnd:
        begin
          BranchFalse(BE.Left, L);
          BranchFalse(BE.Right, L);
          Exit;
        end;
      gttOr:
        begin
          SetLength(TrueL, 0);
          BranchTrue(BE.Left, TrueL);
          BranchFalse(BE.Right, L);
          PatchListHere(TrueL);
          Exit;
        end;
      gttLess, gttLessEqual, gttGreater, gttGreaterEqual, gttEqual, gttNotEqual:
        begin
          A := ExprAny(BE.Left);
          if (A < Save) and MaySideEffect(BE.Right) then
          begin
            B := AllocReg;
            Emit(Ins(opMov, B, A, 0));
            A := B;
          end;
          B := ExprAny(BE.Right);
          case BE.Operator of
            gttLess: Op := opJNLt;
            gttLessEqual: Op := opJNLe;
            gttGreater: Op := opJNGt;
            gttGreaterEqual: Op := opJNGe;
            gttEqual: Op := opJNSEq;
          else Op := opJSEq;
          end;
          AddJump(L, EmitJ(Op, A, B));
          FreeTo(Save);
          Exit;
        end;
    end;
  end;
  A := ExprAny(E);
  AddJump(L, EmitJ(opJF, A, 0));
  FreeTo(Save);
end;

procedure TCompiler.BranchTrue(E: TGocciaExpression; var L: TJumpList);
var Save, A: Integer; BE: TGocciaBinaryExpression; FalseL: TJumpList;
begin
  Save := FS.FreeReg;
  if (E is TGocciaUnaryExpression) and (TGocciaUnaryExpression(E).Operator = gttNot) then
  begin
    BranchFalse(TGocciaUnaryExpression(E).Operand, L);
    Exit;
  end;
  if E is TGocciaBinaryExpression then
  begin
    BE := TGocciaBinaryExpression(E);
    case BE.Operator of
      gttOr:
        begin
          BranchTrue(BE.Left, L);
          BranchTrue(BE.Right, L);
          Exit;
        end;
      gttAnd:
        begin
          SetLength(FalseL, 0);
          BranchFalse(BE.Left, FalseL);
          BranchTrue(BE.Right, L);
          PatchListHere(FalseL);
          Exit;
        end;
    end;
  end;
  A := ExprAny(E);
  AddJump(L, EmitJ(opJT, A, 0));
  FreeTo(Save);
end;

{ ---------------- destructuring ---------------- }

procedure TCompiler.BindPattern(P: TGocciaDestructuringPattern; Src: Integer; Declare: Boolean; IsConst: Boolean);
type
  TRef = record Kind, O, K: Integer; end;   { Kind 0: none, 1 named property (K the IC), 2 computed, 3 private,
                                               4 super (EvalSuperRef: O the base, K the key operand) }
var Save, V, It, I, J, K, Tmp, Exc, H, RS: Integer; OP: TGocciaObjectDestructuringPattern; AP: TGocciaArrayDestructuringPattern;
  Prop: TGocciaDestructuringProperty; Name: UnicodeString; Keys, CompKeys: array of Integer; R: Integer; HasRest: Boolean;
  Ref: TRef;
  { 13.15.5.5 / 13.15.5.6: an assignment target that is not a pattern is evaluated (its reference: the base and the
    key, not yet ToPropertyKey'd) before the value is read; PutValue comes after the initializer }
  function EvalRef(Pat: TGocciaDestructuringPattern): TRef;
  var X: TGocciaExpression; M: TGocciaMemberExpression;
  begin
    Result.Kind := 0;
    if Declare then Exit;
    if Pat is TGocciaRestDestructuringPattern then Pat := TGocciaRestDestructuringPattern(Pat).Argument;
    if Pat is TGocciaAssignmentDestructuringPattern then Pat := TGocciaAssignmentDestructuringPattern(Pat).Left;
    if Pat is TGocciaMemberExpressionDestructuringPattern then
    begin
      X := TGocciaMemberExpressionDestructuringPattern(Pat).Expression;
      if not (X is TGocciaMemberExpression) then Exit;
      M := TGocciaMemberExpression(X);
      if M.ObjectExpr is TGocciaSuperExpression then
      begin
        if M.Computed then EvalSuperRef('', M.PropertyExpression, Result.O, Result.K)
        else EvalSuperRef(M.PropertyName, nil, Result.O, Result.K);
        Result.Kind := 4;
        Exit;
      end;
      Result.O := AllocReg;
      ExprTo(M.ObjectExpr, Result.O);
      if M.Computed then
      begin
        Result.K := AllocReg;
        ExprTo(M.PropertyExpression, Result.K);
        Result.Kind := 2;
      end
      else
      begin
        Result.K := NewIC(M.PropertyName);
        Result.Kind := 1;
      end;
    end
    else if Pat is TGocciaPrivateMemberExpressionDestructuringPattern then
    begin
      X := TGocciaPrivateMemberExpressionDestructuringPattern(Pat).Expression;
      Result.O := AllocReg;
      ExprTo(TGocciaPrivateMemberExpression(X).ObjectExpr, Result.O);
      Result.K := AllocReg;
      LoadIdent(PrivKeyName(TGocciaPrivateMemberExpression(X).PrivateName), Result.K);
      Result.Kind := 3;
    end;
  end;
  function IsRefTarget(Pat: TGocciaDestructuringPattern): Boolean;
  begin
    if Pat is TGocciaAssignmentDestructuringPattern then Pat := TGocciaAssignmentDestructuringPattern(Pat).Left;
    Result := not Declare and ((Pat is TGocciaMemberExpressionDestructuringPattern) or
      (Pat is TGocciaPrivateMemberExpressionDestructuringPattern));
  end;
  procedure PutRef(const Rf: TRef; ValReg: Integer);
  begin
    case Rf.Kind of
      1: Emit(Ins(opSetProp, Rf.O, ValReg, Rf.K));
      2: Emit(Ins(opSetElem, Rf.O, Rf.K, ValReg));
      3: Emit(Ins(opSetPriv, Rf.O, ValReg, Rf.K));
      4: Emit(Ins(opSetSuperB, ValReg, Rf.O, Rf.K));
    end;
  end;
  procedure Target(Pat: TGocciaDestructuringPattern; ValReg: Integer; const Rf: TRef);
  var Sk: Integer; AD: TGocciaAssignmentDestructuringPattern; Rg: Integer;
  begin
    if Rf.Kind <> 0 then
    begin
      if Pat is TGocciaAssignmentDestructuringPattern then
      begin
        Sk := EmitJ(opJNotUndef, ValReg, 0);
        ExprTo(TGocciaAssignmentDestructuringPattern(Pat).Right, ValReg);
        PatchHere(Sk);
      end;
      PutRef(Rf, ValReg);
    end
    else if Pat is TGocciaAssignmentDestructuringPattern then
    begin
      AD := TGocciaAssignmentDestructuringPattern(Pat);
      Sk := EmitJ(opJNotUndef, ValReg, 0);
      if AD.Left is TGocciaIdentifierDestructuringPattern then
        ExprToNamed(AD.Right, ValReg, TGocciaIdentifierDestructuringPattern(AD.Left).Name)
      else ExprTo(AD.Right, ValReg);
      PatchHere(Sk);
      Target(AD.Left, ValReg, Rf);
    end
    else if Pat is TGocciaIdentifierDestructuringPattern then
    begin
      Name := TGocciaIdentifierDestructuringPattern(Pat).Name;
      if Declare then
      begin
        Rg := DeclareLocal(Name, IsConst);
        Emit(Ins(opMov, Rg, ValReg, 0));
      end
      else StoreIdent(Name, ValReg);
    end
    else if Pat is TGocciaMemberExpressionDestructuringPattern then
      AssignToTarget(TGocciaMemberExpressionDestructuringPattern(Pat).Expression, ValReg)
    else if Pat is TGocciaPrivateMemberExpressionDestructuringPattern then
      AssignToTarget(TGocciaPrivateMemberExpressionDestructuringPattern(Pat).Expression, ValReg)
    else
      BindPattern(Pat, ValReg, Declare, IsConst);
  end;
begin
  { declared names get their registers before any temporary is taken }
  if Declare then DeclarePatternNames(P, IsConst);
  Save := FS.FreeReg;
  if P is TGocciaObjectDestructuringPattern then
  begin
    OP := TGocciaObjectDestructuringPattern(P);
    { 14.3.3.1 / 13.15.5.1 early errors: a rest property is last and is a plain target, with no initializer }
    for I := 0 to OP.Properties.Count - 1 do
      if OP.Properties[I].Pattern is TGocciaRestDestructuringPattern then
      begin
        if I < OP.Properties.Count - 1 then raise EEarlySyntaxError.Create('Rest element must be last element');
        if (TGocciaRestDestructuringPattern(OP.Properties[I].Pattern).Argument is TGocciaObjectDestructuringPattern) or
           (TGocciaRestDestructuringPattern(OP.Properties[I].Pattern).Argument is TGocciaArrayDestructuringPattern) or
           (TGocciaRestDestructuringPattern(OP.Properties[I].Pattern).Argument is TGocciaAssignmentDestructuringPattern) then
          raise EEarlySyntaxError.Create('`...` must be followed by an assignable reference in assignment contexts');
      end;
    SetLength(Keys, 0);
    SetLength(CompKeys, 0);
    HasRest := (OP.Properties.Count > 0) and (OP.Properties[OP.Properties.Count - 1].Pattern is TGocciaRestDestructuringPattern);
    { the first property read does the check, unless a key expression or a reference is evaluated before it }
    if (OP.Properties.Count = 0) or OP.Properties[0].Computed or
       (OP.Properties[0].Pattern is TGocciaRestDestructuringPattern) or IsRefTarget(OP.Properties[0].Pattern) then
    begin
      { 14.3.3.1 / 8.6.2: RequireObjectCoercible, with no property read to do it }
      V := AllocReg;
      Emit(Ins(opMov, V, Src, 0));
      Emit(Ins(opWithObj, V, 0, 0));
      FreeTo(V);
    end;
    for I := 0 to OP.Properties.Count - 1 do
    begin
      Prop := OP.Properties[I];
      if Prop.Computed and HasRest and not (Prop.Pattern is TGocciaRestDestructuringPattern) then
      begin
        { 14.3.3.1 RestBindingInitialization excludes this key too: its register stays allocated }
        K := AllocReg;
        ExprTo(Prop.KeyExpression, K);
        Emit(Ins(opToPropKey, K, K, 0));
        SetLength(CompKeys, Length(CompKeys) + 1);
        CompKeys[High(CompKeys)] := K;
        RS := FS.FreeReg;
        Ref := EvalRef(Prop.Pattern);
        V := AllocReg;
        Emit(Ins(opGetElem, V, Src, K));
        Target(Prop.Pattern, V, Ref);
        FreeTo(RS);
        Continue;
      end;
      RS := FS.FreeReg;
      if Prop.Computed and not (Prop.Pattern is TGocciaRestDestructuringPattern) then
      begin
        K := AllocReg;
        ExprTo(Prop.KeyExpression, K);
        Emit(Ins(opToPropKey, K, K, 0));
      end;
      Ref := EvalRef(Prop.Pattern);
      V := AllocReg;
      if Prop.Pattern is TGocciaRestDestructuringPattern then
      begin
        Tmp := AllocReg;
        Emit(Ins(opNewObj, V, 0, 4));
        Emit(Ins(opCopyDataProps, V, Src, 0));
        { remove the keys already bound }
        for J := 0 to High(Keys) do Emit(Ins(opDelProp, Tmp, V, Keys[J]));
        for J := 0 to High(CompKeys) do Emit(Ins(opDelElem, Tmp, V, CompKeys[J]));
        Target(TGocciaRestDestructuringPattern(Prop.Pattern).Argument, V, Ref);
      end
      else
      begin
        if Prop.Computed then
          Emit(Ins(opGetElem, V, Src, K))
        else
        begin
          Emit(Ins(opGetProp, V, Src, NewIC(Prop.Key)));
          SetLength(Keys, Length(Keys) + 1);
          Keys[High(Keys)] := AtomConst(Prop.Key);
        end;
        Target(Prop.Pattern, V, Ref);
      end;
      FreeTo(RS);
    end;
  end
  else if P is TGocciaArrayDestructuringPattern then
  begin
    AP := TGocciaArrayDestructuringPattern(P);
    { 14.3.3 / 13.15.5 early errors: a rest element is last and has no initializer }
    for I := 0 to AP.Elements.Count - 1 do
      if AP.Elements[I] is TGocciaRestDestructuringPattern then
      begin
        if I < AP.Elements.Count - 1 then raise EEarlySyntaxError.Create('Rest element must be last element');
        if TGocciaRestDestructuringPattern(AP.Elements[I]).Argument is TGocciaAssignmentDestructuringPattern then
          raise EEarlySyntaxError.Create('Rest element may not have a default initializer');
      end;
    It := AllocReg;
    AllocReg;   { the iterator record's next method }
    Exc := AllocReg;
    Emit(Ins(opGetIter, It, Src, 0));
    { 8.6.2 BindingInitialization / 13.15.5 DestructuringAssignmentEvaluation: an abrupt element closes the
      iterator unless it is done }
    H := Emit(Ins(opTry, Exc, 1, 0));
    for I := 0 to AP.Elements.Count - 1 do
    begin
      RS := FS.FreeReg;
      Ref.Kind := 0;
      if AP.Elements[I] <> nil then Ref := EvalRef(AP.Elements[I]);
      V := AllocReg;
      if (AP.Elements[I] <> nil) and (AP.Elements[I] is TGocciaRestDestructuringPattern) then
      begin
        Emit(Ins(opNewArr, V, 0, 0));
        Tmp := AllocReg;
        J := Here;
        K := Emit(Ins(opIterNext, Tmp, It, 0));
        Emit(Ins(opArrPush, V, Tmp, 0));
        PatchTo(EmitJ(opJmp, 0, 0), J);
        PatchHere(K);
        Target(TGocciaRestDestructuringPattern(AP.Elements[I]).Argument, V, Ref);
      end
      else
      begin
        Emit(Ins(opLoadUndef, V, 0, 0));
        K := Emit(Ins(opIterNext, V, It, 0));
        PatchHere(K);   { exhausted: value stays undefined }
        if AP.Elements[I] <> nil then Target(AP.Elements[I], V, Ref);
      end;
      FreeTo(RS);
    end;
    Emit(Ins(opEndTry, 0, 0, 0));
    Emit(Ins(opIterClose, It, 0, 0));
    K := EmitJ(opJmp, 0, 0);
    PatchHere(H);
    Emit(Ins(opIterCloseThrow, It, Exc, 1));
    Emit(Ins(opThrow, Exc, 0, 0));
    PatchHere(K);
  end
  else if P is TGocciaIdentifierDestructuringPattern then
  begin
    Name := TGocciaIdentifierDestructuringPattern(P).Name;
    if Declare then
    begin
      R := DeclareLocal(Name, IsConst);
      Emit(Ins(opMov, R, Src, 0));
      MarkInitialized(Name);
    end
    else StoreIdent(Name, Src);
  end
  else if P is TGocciaAssignmentDestructuringPattern then
  begin
    V := AllocReg;
    Emit(Ins(opMov, V, Src, 0));
    J := EmitJ(opJNotUndef, V, 0);
    if TGocciaAssignmentDestructuringPattern(P).Left is TGocciaIdentifierDestructuringPattern then
      ExprToNamed(TGocciaAssignmentDestructuringPattern(P).Right, V,
        TGocciaIdentifierDestructuringPattern(TGocciaAssignmentDestructuringPattern(P).Left).Name)
    else ExprTo(TGocciaAssignmentDestructuringPattern(P).Right, V);
    PatchHere(J);
    BindPattern(TGocciaAssignmentDestructuringPattern(P).Left, V, Declare, IsConst);
  end
  else if P is TGocciaMemberExpressionDestructuringPattern then
    AssignToTarget(TGocciaMemberExpressionDestructuringPattern(P).Expression, Src)
  else if P is TGocciaPrivateMemberExpressionDestructuringPattern then
    AssignToTarget(TGocciaPrivateMemberExpressionDestructuringPattern(P).Expression, Src)
  else
    raise ECompileError.Create('unsupported pattern ' + P.ClassName);
  FreeTo(Save);
end;

{ ---------------- entry ---------------- }

{ ---------------- modules (goal 3) ---------------- }

procedure TModuleInfo.AddEager(const R: UnicodeString);
begin
  if IsEager(R) then Exit;
  SetLength(EvalOrder, Length(EvalOrder) + 1);
  EvalOrder[High(EvalOrder)].Req := R;
  EvalOrder[High(EvalOrder)].Eager := True;
end;

procedure TModuleInfo.AddDeferred(const R: UnicodeString);
var I: Integer;
begin
  for I := 0 to High(EvalOrder) do
    if (EvalOrder[I].Req = R) and not EvalOrder[I].Eager then Exit;
  SetLength(EvalOrder, Length(EvalOrder) + 1);
  EvalOrder[High(EvalOrder)].Req := R;
  EvalOrder[High(EvalOrder)].Eager := False;
end;

function TModuleInfo.IsEager(const R: UnicodeString): Boolean;
var I: Integer;
begin
  for I := 0 to High(EvalOrder) do
    if (EvalOrder[I].Req = R) and EvalOrder[I].Eager then Exit(True);
  Result := False;
end;

procedure TModuleInfo.AddRequest(const R: UnicodeString);
var I: Integer;
begin
  for I := 0 to High(Requests) do
    if Requests[I] = R then Exit;
  SetLength(Requests, Length(Requests) + 1);
  Requests[High(Requests)] := R;
end;

function TCompiler.ModImportSlot(const N: UnicodeString): Integer;
begin
  if ModImportNames = nil then Exit(-1);
  Result := ModImportNames.IndexOf(N);
end;

{ an import binding: the exporter's cell, read through this module's cell table (the %modcells parameter of the
  module function, an upvalue in nested functions) }
procedure TCompiler.EmitModGet(Slot: Integer; Dest: Integer);
var T: Integer;
begin
  T := AllocReg;
  LoadIdent('%modcells', T);
  Emit(Ins(opModGet, Dest, T, Slot));
  FreeTo(T);
end;

{ 16.2.1.6.4 InitializeEnvironment, the module's own part: after FunctionDeclarationInstantiation of its top level
  (HoistBlock), the default export's binding and every exported local's register become cells of the table }
procedure TCompiler.EmitModulePrologue;
var TR, R, I: Integer; D: TGocciaExportDefaultDeclaration; T: PFuncTemplate;
begin
  ResolveLocal(FS, '%modcells', TR);
  D := ModDefault;
  if Assigned(D) then
  begin
    if not ResolveLocal(FS, D.LocalName, R) then R := DeclareLocal(D.LocalName, False);
    if D.IsDirectDeclaration and (D.Expression is TGocciaFunctionExpression) then
    begin
      { a function declaration: its name is the (mutable) module binding, not a self binding }
      with TGocciaFunctionExpression(D.Expression) do
      begin
        if D.LocalName = GOCCIA_DEFAULT_EXPORT_BINDING then
          T := CompileFunction('default', Parameters, Body, False, False, False, False, IsGenerator, IsAsync)
        else T := CompileFunction(D.LocalName, Parameters, Body, False, False, False, False, IsGenerator, IsAsync);
        T^.SourceRange := SourceRange;
      end;
      EmitClosure(R, T);
    end
    else Emit(Ins(opLoadHole, R, 0, 0));
  end;
  for I := 0 to ModPublish.Count - 1 do
  begin
    if not ResolveLocal(FS, ModPublish[I], R) then
      raise EEarlySyntaxError.Create('Export ''' + ModPublish[I] + ''' is not defined in module');
    { the cell reaches other modules and namespaces before the declaration runs: its entry stores the hole }
    InTDZ(ModPublish[I]);
    Emit(Ins(opMakeModCell, R, TR, ModImportNames.Count + I));
  end;
end;

type
  { 16.2.1.? top-level await: an await (or for await, await using) outside every function }
  TTLAScan = class(TGocciaASTChildVisitor)
  public
    Found: Boolean;
    procedure VisitChild(const ANode: TGocciaASTNode); override;
    procedure EnterPart(const AKind: TGocciaASTPartKind; const AStartOffset, AEndOffset: Integer;
      const AFlags: TGocciaASTPartFlags); override;
    procedure LeavePart; override;
  end;

procedure TTLAScan.EnterPart(const AKind: TGocciaASTPartKind; const AStartOffset, AEndOffset: Integer;
  const AFlags: TGocciaASTPartFlags);
begin
end;

procedure TTLAScan.LeavePart;
begin
end;

procedure TTLAScan.VisitChild(const ANode: TGocciaASTNode);
begin
  if (ANode = nil) or Found then Exit;
  if (ANode is TGocciaFunctionExpression) or (ANode is TGocciaArrowFunctionExpression) or
     (ANode is TGocciaFunctionDeclaration) or (ANode is TGocciaObjectMethodDefinition) or
     (ANode is TGocciaClassExpression) or (ANode is TGocciaClassDeclaration) or
     (ANode is TGocciaGetterExpression) or (ANode is TGocciaSetterExpression) then Exit;
  if (ANode is TGocciaAwaitExpression) or (ANode is TGocciaForAwaitOfStatement) or
     ((ANode is TGocciaUsingDeclaration) and TGocciaUsingDeclaration(ANode).IsAwait) then
  begin
    Found := True;
    Exit;
  end;
  ANode.ForEachChild(Self);
end;

type
  { 16.2.1.1: an import or export declaration below a module's top level (one the parser let through) }
  TModDeclScan = class(TGocciaASTChildVisitor)
  public
    Found: Boolean;
    procedure VisitChild(const ANode: TGocciaASTNode); override;
    procedure EnterPart(const AKind: TGocciaASTPartKind; const AStartOffset, AEndOffset: Integer;
      const AFlags: TGocciaASTPartFlags); override;
    procedure LeavePart; override;
  end;

procedure TModDeclScan.VisitChild(const ANode: TGocciaASTNode);
begin
  if (ANode = nil) or Found then Exit;
  if (ANode is TGocciaImportDeclaration) or (ANode is TGocciaExportDeclaration) or
     (ANode is TGocciaExportDefaultDeclaration) or (ANode is TGocciaExportVariableDeclaration) or
     (ANode is TGocciaExportDestructuringDeclaration) or (ANode is TGocciaExportFunctionDeclaration) or
     (ANode is TGocciaExportClassDeclaration) or (ANode is TGocciaExportEnumDeclaration) then
  begin
    Found := True;
    Exit;
  end;
  ANode.ForEachChild(Self);
end;

procedure TModDeclScan.EnterPart(const AKind: TGocciaASTPartKind; const AStartOffset, AEndOffset: Integer;
  const AFlags: TGocciaASTPartFlags);
begin
end;

procedure TModDeclScan.LeavePart;
begin
end;

procedure PatternNames(P: TGocciaDestructuringPattern; L: TStringList);
var I: Integer;
begin
  if P is TGocciaIdentifierDestructuringPattern then L.Add(TGocciaIdentifierDestructuringPattern(P).Name)
  else if P is TGocciaAssignmentDestructuringPattern then PatternNames(TGocciaAssignmentDestructuringPattern(P).Left, L)
  else if P is TGocciaRestDestructuringPattern then PatternNames(TGocciaRestDestructuringPattern(P).Argument, L)
  else if P is TGocciaArrayDestructuringPattern then
  begin
    for I := 0 to TGocciaArrayDestructuringPattern(P).Elements.Count - 1 do
      if Assigned(TGocciaArrayDestructuringPattern(P).Elements[I]) then
        PatternNames(TGocciaArrayDestructuringPattern(P).Elements[I], L);
  end
  else if P is TGocciaObjectDestructuringPattern then
    for I := 0 to TGocciaObjectDestructuringPattern(P).Properties.Count - 1 do
      PatternNames(TGocciaObjectDestructuringPattern(P).Properties[I].Pattern, L);
end;

{ 16.2.1.7.1 ParseModule's entries, from the top level; Nodes gets the statements to compile (export wrappers
  unwrapped, imports and export lists dropped) }
function ModuleRequestKey(const Spec, AType: UnicodeString): UnicodeString;
begin
  if AType = '' then Result := Spec else Result := Spec + #1 + AType;
end;

procedure CollectModule(Prog: TGocciaProgram; MI: TModuleInfo; Nodes: TList<TGocciaASTNode>; C: TCompiler);
var I, J, K: Integer; N: TGocciaASTNode; Names, ExpNames, ExpLocals: TStringList; Pair: TStringStringMap.TKeyValuePair;
  E: TModEntry; Scan: TTLAScan; MScan: TModDeclScan; ReqKey: UnicodeString;
  procedure AddImport(const Local, Req, Imported: UnicodeString);
  begin
    if C.ModImportNames.IndexOf(Local) >= 0 then raise EEarlySyntaxError.Create('Identifier ''' + Local + ''' has already been declared');
    { module code is strict: eval and arguments are no BindingIdentifiers }
    if (Local = 'eval') or (Local = 'arguments') then
      raise EEarlySyntaxError.Create('Unexpected eval or arguments in strict mode');
    C.ModImportNames.Add(Local);
    SetLength(MI.Imports, Length(MI.Imports) + 1);
    with MI.Imports[High(MI.Imports)] do
    begin
      LocalName := Local; Request := Req; ImportName := Imported; ExportName := ''; Slot := C.ModImportNames.Count - 1;
    end;
  end;
  procedure AddExport(const Exp, Local: UnicodeString);
  begin
    if ExpNames.IndexOf(Exp) >= 0 then raise EEarlySyntaxError.Create('Duplicate export of ''' + Exp + '''');
    ExpNames.Add(Exp);
    ExpLocals.Add(Local);
  end;
  procedure AddIndirect(const Exp, Req, Imported: UnicodeString);
  begin
    if ExpNames.IndexOf(Exp) >= 0 then raise EEarlySyntaxError.Create('Duplicate export of ''' + Exp + '''');
    ExpNames.Add(Exp);
    ExpLocals.Add(#0);   { not a local }
    SetLength(MI.IndirectExports, Length(MI.IndirectExports) + 1);
    with MI.IndirectExports[High(MI.IndirectExports)] do
    begin
      ExportName := Exp; Request := Req; ImportName := Imported; LocalName := ''; Slot := -1;
    end;
  end;
begin
  Names := TStringList.Create; ExpNames := TStringList.Create; ExpLocals := TStringList.Create;
  Names.UseLocale := False; ExpNames.UseLocale := False; ExpLocals.UseLocale := False;
  Names.CaseSensitive := True; ExpNames.CaseSensitive := True; ExpLocals.CaseSensitive := True;
  try
    for I := 0 to Prog.Body.Count - 1 do
    begin
      N := Prog.Body[I];
      if N is TGocciaImportDeclaration then
      begin
        with TGocciaImportDeclaration(N) do
        begin
          if DuplicateBinding <> '' then raise EEarlySyntaxError.Create('Identifier ''' + DuplicateBinding + ''' has already been declared');
          ReqKey := ModuleRequestKey(ModulePath, AttributeType);
          MI.AddRequest(ReqKey);
          if Phase = icpDefer then
          begin
            MI.AddDeferred(ReqKey);
            AddImport(NamespaceName, ReqKey, ModDeferNamespace);
          end
          else
          begin
            MI.AddEager(ReqKey);
            if NamespaceName <> '' then AddImport(NamespaceName, ReqKey, ModNamespace);
          end;
          for Pair in Imports do AddImport(Pair.Key, ReqKey, Pair.Value);
        end;
        Continue;
      end;
      if N is TGocciaReExportDeclaration then
      begin
        with TGocciaReExportDeclaration(N) do
        begin
          ReqKey := ModuleRequestKey(ModulePath, AttributeType);
          MI.AddRequest(ReqKey);
          MI.AddEager(ReqKey);
          if IsStarExport and (NamespaceName = '') then
          begin
            SetLength(MI.StarExports, Length(MI.StarExports) + 1);
            MI.StarExports[High(MI.StarExports)] := ReqKey;
          end
          else if IsStarExport then AddIndirect(NamespaceName, ReqKey, ModNamespace)
          else for Pair in ExportsTable do AddIndirect(Pair.Key, ReqKey, Pair.Value);
        end;
        Continue;
      end;
      if N is TGocciaExportDeclaration then
      begin
        for Pair in TGocciaExportDeclaration(N).ExportsTable do AddExport(Pair.Key, Pair.Value);
        Continue;
      end;
      Names.Clear;
      if N is TGocciaExportVariableDeclaration then
      begin
        with TGocciaExportVariableDeclaration(N).Declaration do
          for J := 0 to High(Variables) do
            if Variables[J].IsPattern then PatternNames(Variables[J].Pattern, Names) else Names.Add(Variables[J].Name);
        Nodes.Add(TGocciaExportVariableDeclaration(N).Declaration);
      end
      else if N is TGocciaExportDestructuringDeclaration then
      begin
        PatternNames(TGocciaExportDestructuringDeclaration(N).Declaration.Pattern, Names);
        Nodes.Add(TGocciaExportDestructuringDeclaration(N).Declaration);
      end
      else if N is TGocciaExportFunctionDeclaration then
      begin
        Names.Add(TGocciaExportFunctionDeclaration(N).Declaration.Name);
        Nodes.Add(TGocciaExportFunctionDeclaration(N).Declaration);
      end
      else if N is TGocciaExportClassDeclaration then
      begin
        Names.Add(TGocciaExportClassDeclaration(N).Declaration.ClassDefinition.Name);
        Nodes.Add(TGocciaExportClassDeclaration(N).Declaration);
      end
      else if N is TGocciaExportDefaultDeclaration then
      begin
        if Assigned(C.ModDefault) then raise EEarlySyntaxError.Create('Duplicate export of ''default''');
        C.ModDefault := TGocciaExportDefaultDeclaration(N);
        AddExport('default', TGocciaExportDefaultDeclaration(N).LocalName);
        Nodes.Add(N);
      end
      else Nodes.Add(N);
      for J := 0 to Names.Count - 1 do AddExport(Names[J], Names[J]);
    end;
    { 16.2.1.7.1 step 10: a local export of an imported binding becomes an indirect export; a namespace import's
      resolves to the imported module's namespace (two of them through export * are the same binding) }
    for I := 0 to ExpNames.Count - 1 do
    begin
      if ExpLocals[I] = #0 then Continue;
      K := C.ModImportNames.IndexOf(ExpLocals[I]);
      if K >= 0 then
      begin
        SetLength(MI.IndirectExports, Length(MI.IndirectExports) + 1);
        with MI.IndirectExports[High(MI.IndirectExports)] do
        begin
          ExportName := ExpNames[I]; Request := MI.Imports[K].Request; ImportName := MI.Imports[K].ImportName;
          LocalName := ''; Slot := -1;
        end;
        Continue;
      end;
      E.ExportName := ExpNames[I]; E.LocalName := ExpLocals[I]; E.Request := ''; E.ImportName := '';
      if K >= 0 then E.Slot := K
      else
      begin
        J := C.ModPublish.IndexOf(ExpLocals[I]);
        if J < 0 then J := C.ModPublish.Add(ExpLocals[I]);
        E.Slot := -1 - J;   { fixed below, once the import count is known }
      end;
      SetLength(MI.LocalExports, Length(MI.LocalExports) + 1);
      MI.LocalExports[High(MI.LocalExports)] := E;
    end;
    for I := 0 to High(MI.LocalExports) do
      if MI.LocalExports[I].Slot < 0 then MI.LocalExports[I].Slot := C.ModImportNames.Count - 1 - MI.LocalExports[I].Slot;
    MI.SlotCount := C.ModImportNames.Count + C.ModPublish.Count;
    MScan := TModDeclScan.Create;
    try
      for I := 0 to Prog.Body.Count - 1 do Prog.Body[I].ForEachChild(MScan);
      if MScan.Found then raise EEarlySyntaxError.Create('An import or export declaration may only appear at the top level of a module');
    finally
      MScan.Free;
    end;
    Scan := TTLAScan.Create;
    try
      for I := 0 to Nodes.Count - 1 do Scan.VisitChild(Nodes[I]);
      MI.HasTLA := Scan.Found;
    finally
      Scan.Free;
    end;
  finally
    Names.Free; ExpNames.Free; ExpLocals.Free;
  end;
end;

function CompileProgram(Prog: TGocciaProgram; const SourceName: UnicodeString; ReturnLast: Boolean;
  Goal: Integer): PFuncTemplate;
var C: TCompiler; Params: TGocciaParameterArray; I: Integer; Nodes: TList<TGocciaASTNode>; Strict: Boolean;
  MI: TModuleInfo; Outer: TFuncState; OT: PFuncTemplate; Desc, Entry: UnicodeString; Fields: TUnicodeStringArray;
  K, P1, D, Ser: Integer;
begin
  C := TCompiler.Create;
  C.SourceName := SourceName;
  C.FS := nil;
  C.PendingStrict := -1;
  C.ReturnLast := ReturnLast;
  if Goal > 0 then
  begin
    Strict := HasUseStrictDirective(Prog);
    C.PendingStrict := Ord(Strict);
    if Strict and (Goal = 2) then Goal := 0;   { strict eval code: its declarations stay its own }
    if Goal = 4 then C.PendingStrict := Ord(Strict);
  end;
  C.Goal := Goal;
  C.IsEvalCode := SourceName = '<eval>';
  Nodes := TList<TGocciaASTNode>.Create;
  SetLength(Params, 0);
  MI := nil;
  try
    { 16.1.1 / 19.2.1.1: import and export declarations belong to modules only }
    if Goal <> 3 then
      for I := 0 to Prog.Body.Count - 1 do
        if (Prog.Body[I] is TGocciaImportDeclaration) or (Copy(Prog.Body[I].ClassName, 1, 13) = 'TGocciaExport') then
          raise EEarlySyntaxError.Create('Cannot use import or export outside a module');
    if Goal = 3 then
    begin
      { 16.2.1.7.1 ParseModule: strict, the imports and exports collected, a generator whose start is the module's
        instantiation (InitializeEnvironment) and whose resumption its evaluation }
      C.PendingStrict := 1;
      C.ModImportNames := TStringList.Create; C.ModImportNames.UseLocale := False; C.ModImportNames.CaseSensitive := True;
      C.ModPublish := TStringList.Create; C.ModPublish.UseLocale := False; C.ModPublish.CaseSensitive := True;
      MI := TModuleInfo.Create;
      CollectModule(Prog, MI, Nodes, C);
      SetLength(Params, 1);
      FillChar(Params[0], SizeOf(Params[0]), 0);
      Params[0].Name := '%modcells';
      Result := C.CompileFunction('<module>', Params, nil, False, False, False, False, True, MI.HasTLA, nil, Nodes);
      LastModuleInfo := MI;
      MI := nil;
    end
    else if Goal = 4 then
    begin
      { a direct eval in function code: an arrow (this, arguments, new.target its caller's) inside a stand-in for
        the caller whose bindings are the call site's, by index into the description (the eval's upvalues) }
      Outer := TFuncState.Create;
      New(OT);
      FillChar(OT^, SizeOf(TFuncTemplate), 0);
      Outer.Tpl := OT;
      Outer.IsEvalOuter := True;
      Outer.ArgsReg := -1; Outer.HomeReg := -1; Outer.NewTargetReg := -1; Outer.CtorReg := -1; Outer.UsingCapReg := -1;
      C.FS := Outer;
      C.PushScope(True);
      Desc := EvalOuterDesc;
      K := 0;
      while Desc <> '' do
      begin
        P1 := Pos(#1, Desc);
        Entry := Copy(Desc, 1, P1 - 1);
        Delete(Desc, 1, P1);
        Fields := Entry.Split([#2]);
        if Fields[0] = '%noargs' then Outer.NoArguments := True;
        if Fields[0] = '%nonewtarget' then Outer.NoNewTarget := True;
        { this code's own hidden with and eval environment names come after the caller's }
        if (Copy(Fields[0], 1, 8) = '%evalenv') and TryStrToInt(Copy(Fields[0], 9, 20), Ser) and (Ser >= C.WithSerial) then
          C.WithSerial := Ser + 1;
        if (Copy(Fields[0], 1, 5) = '%with') and TryStrToInt(Copy(Fields[0], 6, 20), Ser) and (Ser >= C.WithSerial) then
          C.WithSerial := Ser + 1;
        Outer.Scopes[0].Add(Fields[0], K, (StrToInt(Fields[3]) and efConst) <> 0);
        with Outer.Scopes[0].Bindings[Outer.Scopes[0].Count - 1] do
        begin
          IsLexical := (StrToInt(Fields[3]) and efLexical) <> 0;
          Initialized := (StrToInt(Fields[3]) and efInitialized) <> 0;
          IsFnName := (StrToInt(Fields[3]) and efFnName) <> 0;
          HoleNop := -1;
        end;
        SetLength(C.EvalFlags, K + 1);
        C.EvalFlags[K] := StrToInt(Fields[3]);
        SetLength(C.EvalDepth, K + 1);
        if Length(Fields) > 4 then C.EvalDepth[K] := StrToInt(Fields[4]) else C.EvalDepth[K] := 0;
        Inc(K);
      end;
      Outer.FreeReg := K;
      { the eval variable environments of the caller and the functions around it, outermost first: each holds
        the names no binding of its function (or one inside it) does }
      for D := 64 downto 0 do
      begin
        for I := 0 to Outer.Scopes[0].Count - 1 do
          if (Copy(Outer.Scopes[0].Bindings[I].Name, 1, 8) = '%evalenv') and (C.EvalDepth[Outer.Scopes[0].Bindings[I].Reg] = D) then
          begin
            C.PushWithEntry(Outer.Scopes[0].Bindings[I].Name, Outer, 0, 2, D);
            if D = 0 then C.EvalVarEnv := Outer.Scopes[0].Bindings[I].Name;
          end;
        { the with statements around the call, outermost first (the bindings come innermost first): kind 3,
          Scope the object's position }
        for I := Outer.Scopes[0].Count - 1 downto 0 do
          if (Copy(Outer.Scopes[0].Bindings[I].Name, 1, 5) = '%with') and (C.EvalDepth[Outer.Scopes[0].Bindings[I].Reg] = D) then
            C.PushWithEntry(Outer.Scopes[0].Bindings[I].Name, Outer, I, 3, D);
      end;
      for I := 0 to Prog.Body.Count - 1 do Nodes.Add(Prog.Body[I]);
      try
        Result := C.CompileFunction('<eval>', Params, nil, True, False, False, False, False, False, nil, Nodes);
      finally
        C.FS := nil;
        Outer.Scopes[0].Free;
        Outer.Free;
        Dispose(OT);
      end;
    end
    else
    begin
      for I := 0 to Prog.Body.Count - 1 do Nodes.Add(Prog.Body[I]);
      Result := C.CompileFunction('<script>', Params, nil, False, False, False, False, False, False, nil, Nodes);
      if GetEnvironmentVariable('G2_DUMP') <> '' then DumpTemplates(Result, UnicodeString(GetEnvironmentVariable('G2_DUMP')));
    end;
  finally
    MI.Free;
    C.ModImportNames.Free;
    C.ModPublish.Free;
    Nodes.Free;
    C.Free;
  end;
end;

end.
