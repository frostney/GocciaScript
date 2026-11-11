unit G2.PreParse;

{ G2's pre-parser for the single-pass front end: it reads a Script or Module with G2.Scan and checks the whole
  ES2026 syntactic grammar and every early error (§5.2.4, the Static Semantics: Early Errors clauses), without
  building a tree or emitting code.
  Along the way it resolves names: each scope's declarations, each reference to the scope that declares it,
  whether a binding is captured by an inner function, and per function a record (its source range, kind,
  flags and the free names it uses) that lets the code generator compile functions lazily, on their first
  call, without parsing the inner functions of the one it compiles.
  Cover grammars are handled by parsing again: a parenthesized expression or async call followed by => is
  read once more as arrow parameters, an array or object literal followed by = (or in / of in a for head) as
  an assignment pattern. What the first reading recorded (references, function records, private names) is
  dropped first. }

{$mode objfpc}{$H+}{$modeswitch advancedrecords}

interface

uses
  SysUtils, Math, G2.Core, G2.Scan;

type
  TParseGoal = (pgScript, pgModule);

  TFuncFlag = (ffArrow, ffAsync, ffGenerator, ffMethod, ffGetter, ffSetter, ffClassCtor, ffDerivedCtor,
    ffFieldInit, ffStaticBlock, ffStrict, ffSimpleParams, ffUsesThis, ffUsesArguments, ffDirectEval,
    ffSuperProp, ffSuperCall, ffNewTarget, ffInnerEval, ffExpression, ffDeclaration);
  TFuncFlags = set of TFuncFlag;

  { a function the pre-parser met: enough for a lazy stub, and to skip it when its parent is parsed again }
  TFuncRecord = record
    Start, Stop: Integer;     { its source text (Function.prototype.toString), Stop exclusive }
    Name: PStr;               { a declaration's or named expression's own name, else nil }
    ParamCount: Integer;      { ExpectedArgumentCount }
    Flags: TFuncFlags;
    FreeStart, FreeCount: Integer;   { the names it uses that its own scopes do not declare, in FreeNames }
  end;

  TDeclKind = (dkVar, dkFunction, dkParam, dkLet, dkConst, dkClass, dkUsing, dkLexFunction, dkCatchParam,
    dkCatchPattern, dkImport, dkFuncName, dkClassName, dkArguments);

  TScopeKind = (skScript, skModule, skFunction, skBlock, skCatch, skFor, skSwitch, skClass);

  TDecl = record
    Atom: PStr;
    Prev: Integer;      { the declaration of the same name this one hides, or -1 }
    Scope: Integer;     { the index of its scope on the scope stack }
    Pos: Integer;
    Kind: TDeclKind;
    Captured: Boolean;  { an inner function refers to it }
    Plain: Boolean;     { dkLexFunction: a plain function (B.3.2.4 allows duplicates of these in sloppy blocks) }
  end;

  TScopeRec = record
    Kind: TScopeKind;
    DeclStart, RefStart, VarStart: Integer;
    Func: Integer;      { the function context it belongs to }
  end;

  TRef = record
    Atom: PStr;
    Inner: Boolean;     { the reference is in an inner function }
  end;

  { a var-scoped name declared below the function's top-level scope, checked against each scope it hoists
    through when that scope ends }
  TVarEntry = record
    Atom: PStr;
    Pos: Integer;
    Decl: Integer;      { B.3.2.1 a sloppy block function's own declaration, else -1 }
    ForOf: Boolean;     { declared by for (var ... of ...) }
    AnnexB: Boolean;    { a block function's var binding: dropped, not an error, on a conflict }
    Dropped: Boolean;
  end;

  TFuncKind = (fkScript, fkModule, fkFunction, fkArrow, fkMethod, fkClassCtor, fkDerivedCtor, fkGetter,
    fkSetter, fkFieldInit, fkStaticBlock);

  TFuncCtx = record
    Kind: TFuncKind;
    IsStrict, IsAsync, IsGen: Boolean;
    YieldOp, AwaitOp: Boolean;   { yield / await are operators here }
    InParams: Boolean;           { parsing the parameters (YieldExpression / AwaitExpression are errors) }
    SuperProp, SuperCall, NewTarget, ArgsBanned, AwaitBanned: Boolean;
    Scope: Integer;
    LabelBase: Integer;
    Breakable, Loops: Integer;
    Rec: Integer;
    RefStart: Integer;
    UsesF: TFuncFlags;
    Name: PStr;
    NamePos: Integer;
    SimpleParams: Boolean;
    DupParamPos: Integer;        { a duplicate parameter name, or -1 }
  end;

  TLabel = record
    Atom: PStr;
    Loop: Boolean;
  end;

  TPrivName = record
    Atom: PStr;
    Pos: Integer;
    Kind: Byte;    { 0 field / method, 1 getter, 2 setter, 3 getter and setter }
    IsStatic: Boolean;
  end;

  TPrivClass = record
    NameStart, RefStart: Integer;
  end;

  TPrivRef = record
    Atom: PStr;
    Pos: Integer;
  end;

  { an expression's shape, as far as the early errors and cover grammars need it }
  TXKind = (xOther, xIdent, xMember, xPrivMember, xCall, xOptional, xOptPriv, xObject, xArray, xString, xArrow,
    xPrivName, xLiteral);

  TX = record
    K: TXKind;
    Paren: Boolean;    { parenthesized }
    Op: Byte;          { the unparenthesized operator at its top: opOrAnd, opCoalesce, opUnary }
    A: PStr;           { xIdent: the name }
  end;

  { the parser's position and what it has recorded, for parsing a stretch again }
  TMark = record
    St: TScanState;
    Kind: TTok;
    TokStart, TokEnd, TokLine, TokCol, PrevEnd: Integer;
    NewlineBefore, Escaped: Boolean;
    EscapedReserved: TTok;
    Atom: PStr;
    RefTop, FuncCount, FreeCount, PrivRefTop: Integer;
  end;

  { a map from atoms to integers (open addressing; keys are never removed) }
  TAtomMap = record
    Keys: array of PStr;
    Vals: array of Integer;
    Count, Mask: Integer;
    procedure Init(Bits: Integer);
    function Slot(A: PStr): Integer; inline;
    function Get(A: PStr): Integer; inline;
    procedure Put(A: PStr; V: Integer);
    procedure Grow;
  end;

  TStmtCtx = (scList, scIf, scLoop, scLabel, scOther);

  TPreParser = class
  private
    S: TScanner;
    FGoal: TParseGoal;
    FMap: TAtomMap;
    Decls: array of TDecl;
    DeclTop: Integer;
    Scopes: array of TScopeRec;
    ScopeTop: Integer;
    Refs: array of TRef;
    RefTop: Integer;
    VarLog: array of TVarEntry;
    VarTop: Integer;
    Ctxs: array of TFuncCtx;
    CtxTop: Integer;
    Labels: array of TLabel;
    LabelTop: Integer;
    FPendingLabels: Integer;
    PrivNames: array of TPrivName;
    PrivNameTop: Integer;
    PrivClasses: array of TPrivClass;
    PrivClassTop: Integer;
    PrivRefs: array of TPrivRef;
    PrivRefTop: Integer;
    FCoverErr: Integer;
    FCoverMsg: string;
    { the free-name dedup set: atom -> stamp }
    FSeen: TAtomMap;
    FStamp: Integer;
    FModuleExports: TAtomMap;
    FExportLocals: array of TPrivRef;
    FExportLocalTop: Integer;
    { errors }
    procedure Err(const Msg: string; Pos: Integer);
    procedure ErrHere(const Msg: string);
    procedure Unexpected;
    { tokens }
    function F: Integer; inline;
    function IsId(A: PStr): Boolean; inline;
    procedure Expect(K: TTok; G: TScanGoal);
    procedure Semi;
    function Mark: TMark;
    procedure Reset(const M: TMark);
    function TokName: string;
    function StartsExpression: Boolean;
    function IsIdentName: Boolean; inline;
    { scopes and names }
    procedure PushScope(K: TScopeKind);
    procedure PopScope;
    procedure CheckVarConflicts(SI: Integer);
    procedure ResolveRefs(SI: Integer);
    procedure PushDecl(A: PStr; K: TDeclKind; Pos: Integer; Plain: Boolean);
    procedure Declare(A: PStr; K: TDeclKind; Pos: Integer; Plain: Boolean);
    procedure DeclareVar(A: PStr; Pos: Integer; ForOf, AnnexB: Boolean; Decl: Integer);
    procedure AddRef(A: PStr); inline;
    procedure PushCtx(K: TFuncKind; IsAsync, IsGen: Boolean);
    procedure PopCtx;
    function NewRecord(Start: Integer): Integer;
    procedure FinishFunction(Stop: Integer);
    { identifiers }
    procedure CheckIdentRef(A: PStr; Pos: Integer);
    procedure CheckBinding(A: PStr; Pos: Integer);
    procedure CheckStrictBinding(A: PStr; Pos: Integer);
    function BindingIdent(K: TDeclKind): PStr;
    procedure CheckSimpleTarget(const X: TX; Kind: Integer; Pos: Integer);
    { expressions }
    function ParseExpression(Fl: Integer): TX;
    function ParseAssignment(Fl: Integer): TX;
    function ParseYield(Fl: Integer): TX;
    function ParseConditional(Fl: Integer): TX;
    function ParseBinary(MinPrec, Fl: Integer): TX;
    function ParseUnary(Fl: Integer): TX;
    function ParsePostfix(Fl: Integer): TX;
    function ParseLHS(Fl: Integer): TX;
    procedure CoverError(Pos: Integer; const Msg: string);
    procedure ParseCallTail(var X: TX; NewCallee: Boolean);
    function ParseNew: TX;
    function ParseSuper(InNew: Boolean): TX;
    function ParseImportExpr(InNew: Boolean): TX;
    function ParsePrimary(Fl: Integer): TX;
    function ParseIdentPrimary(Fl: Integer): TX;
    function ParseAsyncPrimary(Fl: Integer): TX;
    function ParseParenOrArrow(Fl: Integer): TX;
    function ParseArrayLiteral: TX;
    function ParseObjectLiteral: TX;
    procedure ParseArguments(Cover: Boolean);
    procedure ParseTemplate(Tagged: Boolean);
    procedure CheckRegExp;
    { patterns }
    procedure ParseBindingTarget(K: TDeclKind);
    procedure ParseBindingElement(K: TDeclKind);
    procedure ParseAssignPattern;
    procedure ParseAssignTarget;
    { functions and classes }
    procedure ParseArrow(Start: Integer; IsAsync: Boolean; Fl: Integer; Single: PStr; SinglePos: Integer);
    procedure ParseFunction(Start: Integer; IsAsync, IsExpr, DefaultExport: Boolean);
    procedure ParseFormalParams(Unique: Boolean);
    procedure ParseDirectives;
    procedure ParseFunctionBody;
    procedure CheckParamsStrict;
    procedure ParseMethod(Start: Integer; K: TFuncKind; IsAsync, IsGen: Boolean);
    function ParsePropertyName(out Name: PStr; out IsComputed: Boolean): Boolean;
    procedure ParseClass(Start: Integer; IsExpr: Boolean; DefaultExport: Boolean);
    procedure ParseClassBody(Derived: Boolean);
    procedure ParseDecorators;
    procedure DeclarePrivate(A: PStr; Pos: Integer; Kind: Byte; IsSt: Boolean);
    procedure UsePrivate(A: PStr; Pos: Integer);
    procedure PushPrivClass;
    procedure PopPrivClass;
    { statements }
    procedure ParseStatementList(EndTok: TTok);
    procedure ParseStatement(Ctx: TStmtCtx);
    procedure ParseBlock(G: TScanGoal);
    procedure ParseVarDecls(K: TDeclKind; ForHead: Boolean; out Count, MissingInit: Integer;
      out HasInit, IsPattern: Boolean);
    procedure ParseLexical(K: TDeclKind);
    procedure ParseIf;
    procedure ParseFor;
    procedure ParseWhile;
    procedure ParseDoWhile;
    procedure ParseBreakContinue(IsBreak: Boolean);
    procedure ParseReturn;
    procedure ParseWith;
    procedure ParseSwitch;
    procedure ParseThrow;
    procedure ParseTry;
    procedure ParseLabelledOrExpression(Ctx: TStmtCtx; Pending: Integer);
    procedure ParseExpressionStatement;
    function AtLetDeclaration(Ctx: TStmtCtx): Boolean;
    function AtUsingDeclaration(out IsAwait: Boolean): Boolean;
    procedure ParseUsing(IsAwait: Boolean);
    { modules }
    procedure ParseImportDecl;
    procedure ParseExportDecl;
    procedure ParseWithClause;
    function ParseModuleExportName(out IsString: Boolean): PStr;
    procedure AddExport(A: PStr; Pos: Integer);
    procedure ParseModuleSpecifier;
  public
    Funcs: array of TFuncRecord;
    FuncCount: Integer;
    FreeNames: array of PStr;
    FreeCount: Integer;
    constructor Create(ASrc: PWideChar; ALen: Integer; AGoal: TParseGoal; AStrict: Boolean);
    destructor Destroy; override;
    { the whole Script or Module; raises EScanError (Pos: the 0-based offset) on a syntax error }
    procedure ParseProgram;
    property Scanner: TScanner read S;
  end;

threadvar
  { checks a RegularExpressionLiteral's pattern and flags (§22.2.1.1 early errors): the error, or '' }
  PreParseRegExpCheck: function(const Pattern, Flags: UnicodeString): UnicodeString;

{ pre-parses Src; on a syntax error returns False with the message and its 1-based line and column }
function PreParseSource(const Src: UnicodeString; Goal: TParseGoal; Strict: Boolean; out Msg: string;
  out Line, Col: Integer): Boolean;

implementation

const
  { expression flags }
  afNoIn = 1;
  afAllowCover = 2;
  { TX.Op }
  opOrAnd = 1;
  opCoalesce = 2;
  opUnary = 3;
  { CheckSimpleTarget kinds }
  tgAssign = 0;
  tgCompound = 1;
  tgLogical = 2;
  tgUpdate = 3;
  tgForInOf = 4;
  tgPattern = 5;

  LexicalKinds = [dkLet, dkConst, dkClass, dkUsing, dkLexFunction, dkImport, dkClassName];

var
  AtomsReady: Boolean = False;
  AYield, AAwait, ALet, AStatic, AImplements, AInterface, APackage, APrivate, AProtected, APublic, AEval,
  AArguments, AAsync, AOf, AGet, ASet, AUsing, AAccessor, ATarget, AMeta, AFrom, AAs, AConstructor, APrototype,
  ASource, ADefer, AProto, ADefault: PStr;
  { special atoms by pointer: open addressing table }
  SpecKeys: array[0..255] of PStr;
  SpecVals: array[0..255] of Byte;

const
  spNone = 0; spYield = 1; spAwait = 2; spLet = 3; spStrictReserved = 4; spEval = 5; spArguments = 6;

function AtomOf(const W: UnicodeString): PStr;
begin
  Result := AtomOfChars(PWideChar(W), Length(W));
end;

function SpecSlot(A: PStr): Integer; inline;
begin
  Result := Integer((PtrUInt(A) * PtrUInt($9E3779B97F4A7C15)) shr 56);
end;

procedure AddSpec(A: PStr; V: Byte);
var I: Integer;
begin
  I := SpecSlot(A);
  while SpecKeys[I] <> nil do I := (I + 1) and 255;
  SpecKeys[I] := A;
  SpecVals[I] := V;
end;

function SpecOf(A: PStr): Byte; inline;
var I: Integer;
begin
  I := SpecSlot(A);
  while SpecKeys[I] <> nil do
  begin
    if SpecKeys[I] = A then Exit(SpecVals[I]);
    I := (I + 1) and 255;
  end;
  Result := spNone;
end;

procedure InitAtoms;
begin
  AYield := AtomOf('yield'); AAwait := AtomOf('await'); ALet := AtomOf('let'); AStatic := AtomOf('static');
  AImplements := AtomOf('implements'); AInterface := AtomOf('interface'); APackage := AtomOf('package');
  APrivate := AtomOf('private'); AProtected := AtomOf('protected'); APublic := AtomOf('public');
  AEval := AtomOf('eval'); AArguments := AtomOf('arguments'); AAsync := AtomOf('async'); AOf := AtomOf('of');
  AGet := AtomOf('get'); ASet := AtomOf('set'); AUsing := AtomOf('using'); AAccessor := AtomOf('accessor');
  ATarget := AtomOf('target'); AMeta := AtomOf('meta'); AFrom := AtomOf('from'); AAs := AtomOf('as');
  AConstructor := AtomOf('constructor'); APrototype := AtomOf('prototype'); ASource := AtomOf('source');
  ADefer := AtomOf('defer'); AProto := AtomOf('__proto__'); ADefault := AtomOf('default');
  FillChar(SpecKeys, SizeOf(SpecKeys), 0);
  AddSpec(AYield, spYield);
  AddSpec(AAwait, spAwait);
  AddSpec(ALet, spLet);
  AddSpec(AStatic, spStrictReserved);
  AddSpec(AImplements, spStrictReserved);
  AddSpec(AInterface, spStrictReserved);
  AddSpec(APackage, spStrictReserved);
  AddSpec(APrivate, spStrictReserved);
  AddSpec(AProtected, spStrictReserved);
  AddSpec(APublic, spStrictReserved);
  AddSpec(AEval, spEval);
  AddSpec(AArguments, spArguments);
  AtomsReady := True;
end;

{ TAtomMap }

procedure TAtomMap.Init(Bits: Integer);
begin
  SetLength(Keys, 1 shl Bits);
  SetLength(Vals, 1 shl Bits);
  FillChar(Keys[0], Length(Keys) * SizeOf(PStr), 0);
  Count := 0;
  Mask := (1 shl Bits) - 1;
end;

function TAtomMap.Slot(A: PStr): Integer;
begin
  Result := Integer((PtrUInt(A) * PtrUInt($9E3779B97F4A7C15)) shr 40) and Mask;
  while (Keys[Result] <> nil) and (Keys[Result] <> A) do Result := (Result + 1) and Mask;
end;

function TAtomMap.Get(A: PStr): Integer;
var I: Integer;
begin
  I := Slot(A);
  if Keys[I] = nil then Result := -1 else Result := Vals[I];
end;

procedure TAtomMap.Put(A: PStr; V: Integer);
var I: Integer;
begin
  I := Slot(A);
  if Keys[I] = nil then
  begin
    if (Count + 1) * 2 > Mask then
    begin
      Grow;
      I := Slot(A);
    end;
    Keys[I] := A;
    Inc(Count);
  end;
  Vals[I] := V;
end;

procedure TAtomMap.Grow;
var OK: array of PStr; OV: array of Integer; I, J, N: Integer;
begin
  OK := Keys;
  OV := Vals;
  N := Length(OK) * 2;
  Keys := nil;
  Vals := nil;
  SetLength(Keys, N);
  SetLength(Vals, N);
  FillChar(Keys[0], N * SizeOf(PStr), 0);
  Mask := N - 1;
  for I := 0 to High(OK) do
    if OK[I] <> nil then
    begin
      J := Slot(OK[I]);
      Keys[J] := OK[I];
      Vals[J] := OV[I];
    end;
end;

{ TPreParser: basics }

constructor TPreParser.Create(ASrc: PWideChar; ALen: Integer; AGoal: TParseGoal; AStrict: Boolean);
begin
  inherited Create;
  if not AtomsReady then InitAtoms;
  S := TScanner.Create(ASrc, ALen);
  S.HTMLComments := AGoal <> pgModule;
  FGoal := AGoal;
  FMap.Init(10);
  FSeen.Init(8);
  FModuleExports.Init(6);
  SetLength(Decls, 256);
  SetLength(Scopes, 64);
  SetLength(Refs, 1024);
  SetLength(VarLog, 64);
  SetLength(Ctxs, 32);
  SetLength(Labels, 16);
  SetLength(PrivNames, 16);
  SetLength(PrivClasses, 8);
  SetLength(PrivRefs, 16);
  SetLength(Funcs, 64);
  SetLength(FreeNames, 256);
  FCoverErr := -1;
  CtxTop := 0;
  if AGoal = pgModule then PushCtx(fkModule, False, False) else PushCtx(fkScript, False, False);
  Ctxs[0].IsStrict := AStrict or (AGoal = pgModule);
  Ctxs[0].AwaitOp := AGoal = pgModule;
end;

destructor TPreParser.Destroy;
begin
  S.Free;
  inherited Destroy;
end;

procedure TPreParser.Err(const Msg: string; Pos: Integer);
begin
  raise EScanError.CreatePos(Msg, Pos);
end;

procedure TPreParser.ErrHere(const Msg: string);
begin
  Err(Msg, S.TokStart);
end;

function TPreParser.TokName: string;
begin
  case S.Kind of
    tkEOF: Result := 'end of input';
    tkIdent, tkPrivateName: Result := '''' + UTF8Encode(S.TokText) + '''';
    tkNumber, tkBigInt: Result := 'number';
    tkString: Result := 'string';
    tkTemplate, tkTemplateHead, tkTemplateMiddle, tkTemplateTail: Result := 'template string';
    tkRegExp: Result := 'regular expression';
  else
    Result := '''' + UTF8Encode(S.TokText) + '''';
  end;
end;

procedure TPreParser.Unexpected;
begin
  if (S.Kind = tkIdent) and (S.EscapedReserved <> tkIdent) then ErrHere('Keyword must not contain escaped characters');
  if S.Kind = tkEOF then ErrHere('Unexpected end of input');
  ErrHere('Unexpected token ' + TokName);
end;

function TPreParser.F: Integer;
begin
  Result := CtxTop - 1;
end;

function TPreParser.IsId(A: PStr): Boolean;
begin
  Result := (S.Kind = tkIdent) and (S.Atom = A) and not S.Escaped;
end;

function TPreParser.IsIdentName: Boolean;
begin
  Result := (S.Kind = tkIdent) or (S.Kind >= tkBreak);
end;

procedure TPreParser.Expect(K: TTok; G: TScanGoal);
begin
  if S.Kind <> K then Unexpected;
  S.Next(G);
end;

procedure TPreParser.Semi;
begin
  if S.Kind = tkSemi then S.Next(sgRegExp)
  else if (S.Kind <> tkRBrace) and (S.Kind <> tkEOF) and not S.NewlineBefore then Unexpected;
end;

function TPreParser.Mark: TMark;
begin
  Result.St := S.Save;
  Result.Kind := S.Kind;
  Result.TokStart := S.TokStart;
  Result.TokEnd := S.TokEnd;
  Result.TokLine := S.TokLine;
  Result.TokCol := S.TokCol;
  Result.PrevEnd := S.PrevEnd;
  Result.NewlineBefore := S.NewlineBefore;
  Result.Escaped := S.Escaped;
  Result.EscapedReserved := S.EscapedReserved;
  Result.Atom := S.Atom;
  Result.RefTop := RefTop;
  Result.FuncCount := FuncCount;
  Result.FreeCount := FreeCount;
  Result.PrivRefTop := PrivRefTop;
end;

procedure TPreParser.Reset(const M: TMark);
begin
  S.Restore(M.St);
  S.Kind := M.Kind;
  S.TokStart := M.TokStart;
  S.TokEnd := M.TokEnd;
  S.TokLine := M.TokLine;
  S.TokCol := M.TokCol;
  S.PrevEnd := M.PrevEnd;
  S.NewlineBefore := M.NewlineBefore;
  S.Escaped := M.Escaped;
  S.EscapedReserved := M.EscapedReserved;
  S.Atom := M.Atom;
  RefTop := M.RefTop;
  FuncCount := M.FuncCount;
  FreeCount := M.FreeCount;
  PrivRefTop := M.PrivRefTop;
end;

{ whether the current token can begin an AssignmentExpression (for yield's optional operand) }
function TPreParser.StartsExpression: Boolean;
begin
  case S.Kind of
    tkEOF, tkRParen, tkRBracket, tkRBrace, tkComma, tkSemi, tkColon, tkQuestion, tkArrow, tkIn, tkInstanceof, tkLt,
    tkAssign, tkPlusAssign, tkMinusAssign, tkStarAssign, tkPercentAssign, tkStarStarAssign, tkShlAssign,
    tkSarAssign, tkShrAssign, tkAmpAssign, tkBarAssign, tkCaretAssign, tkAndAndAssign, tkOrOrAssign, tkQQAssign,
    tkEq, tkNe, tkSEq, tkSNe, tkLe, tkGe, tkGt, tkStarStar, tkPercent, tkAndAnd, tkOrOr, tkQQ, tkBar, tkCaret,
    tkAmp, tkShl, tkSar, tkShr, tkDot, tkQDot, tkTemplateMiddle, tkTemplateTail:
      Result := False;
  else
    Result := True;
  end;
end;

{ scopes and names }

procedure TPreParser.PushScope(K: TScopeKind);
begin
  if ScopeTop >= Length(Scopes) then SetLength(Scopes, ScopeTop * 2);
  Scopes[ScopeTop].Kind := K;
  Scopes[ScopeTop].DeclStart := DeclTop;
  Scopes[ScopeTop].RefStart := RefTop;
  Scopes[ScopeTop].VarStart := VarTop;
  Scopes[ScopeTop].Func := CtxTop - 1;
  Inc(ScopeTop);
  if K in [skScript, skModule, skFunction] then
  begin
    Ctxs[CtxTop - 1].Scope := ScopeTop - 1;
    Ctxs[CtxTop - 1].RefStart := RefTop;
  end;
end;

procedure TPreParser.PushDecl(A: PStr; K: TDeclKind; Pos: Integer; Plain: Boolean);
begin
  if DeclTop >= Length(Decls) then SetLength(Decls, DeclTop * 2);
  Decls[DeclTop].Atom := A;
  Decls[DeclTop].Prev := FMap.Get(A);
  Decls[DeclTop].Scope := ScopeTop - 1;
  Decls[DeclTop].Pos := Pos;
  Decls[DeclTop].Kind := K;
  Decls[DeclTop].Captured := False;
  Decls[DeclTop].Plain := Plain;
  FMap.Put(A, DeclTop);
  Inc(DeclTop);
end;

function NameOf(A: PStr): string;
begin
  Result := UTF8Encode(StrToUnicode(A));
end;

procedure TPreParser.Declare(A: PStr; K: TDeclKind; Pos: Integer; Plain: Boolean);
var D: Integer; DK: TDeclKind;
begin
  D := FMap.Get(A);
  if (D >= 0) and (Decls[D].Scope = ScopeTop - 1) then
  begin
    DK := Decls[D].Kind;
    case K of
      dkLet, dkConst, dkClass, dkUsing, dkImport, dkClassName:
        if DK <> dkFuncName then Err('Identifier ''' + NameOf(A) + ''' has already been declared', Pos);
      dkLexFunction:
        if DK = dkFuncName then
        else if (DK = dkLexFunction) and Decls[D].Plain and Plain and not Ctxs[F].IsStrict then
        else Err('Identifier ''' + NameOf(A) + ''' has already been declared', Pos);
      dkFunction:
        if DK in LexicalKinds then Err('Identifier ''' + NameOf(A) + ''' has already been declared', Pos);
      dkParam:
        if (DK = dkParam) and (Ctxs[F].DupParamPos < 0) then Ctxs[F].DupParamPos := Pos;
      dkCatchParam, dkCatchPattern:
        if DK in [dkCatchParam, dkCatchPattern] then
          Err('Identifier ''' + NameOf(A) + ''' has already been declared', Pos);
    end;
  end;
  PushDecl(A, K, Pos, Plain);
end;

procedure TPreParser.DeclareVar(A: PStr; Pos: Integer; ForOf, AnnexB: Boolean; Decl: Integer);
begin
  if VarTop >= Length(VarLog) then SetLength(VarLog, VarTop * 2);
  VarLog[VarTop].Atom := A;
  VarLog[VarTop].Pos := Pos;
  VarLog[VarTop].Decl := Decl;
  VarLog[VarTop].ForOf := ForOf;
  VarLog[VarTop].AnnexB := AnnexB;
  VarLog[VarTop].Dropped := False;
  Inc(VarTop);
end;

procedure TPreParser.AddRef(A: PStr);
begin
  if RefTop >= Length(Refs) then SetLength(Refs, RefTop * 2);
  Refs[RefTop].Atom := A;
  Refs[RefTop].Inner := False;
  Inc(RefTop);
end;

{ the var-scoped names hoisting through scope SI against its lexical declarations (§14.2.1, §15.2.1,
  §16.1.1 early errors; B.3.2 / B.3.4 exceptions) }
procedure TPreParser.CheckVarConflicts(SI: Integer);
var I, D: Integer;
begin
  for I := Scopes[SI].VarStart to VarTop - 1 do
  begin
    if VarLog[I].Dropped then Continue;
    D := FMap.Get(VarLog[I].Atom);
    if (D < 0) or (Decls[D].Scope <> SI) then Continue;
    case Decls[D].Kind of
      dkLet, dkConst, dkClass, dkUsing, dkLexFunction, dkImport, dkClassName:
        if VarLog[I].AnnexB then
        begin
          if D <> VarLog[I].Decl then VarLog[I].Dropped := True;
        end
        else Err('Identifier ''' + NameOf(VarLog[I].Atom) + ''' has already been declared', VarLog[I].Pos);
      dkCatchPattern:
        if VarLog[I].AnnexB then VarLog[I].Dropped := True
        else Err('Identifier ''' + NameOf(VarLog[I].Atom) + ''' has already been declared', VarLog[I].Pos);
      dkParam:
        if VarLog[I].AnnexB then VarLog[I].Dropped := True;
    end;
  end;
end;

procedure TPreParser.ResolveRefs(SI: Integer);
var R, W, D: Integer;
begin
  W := Scopes[SI].RefStart;
  for R := W to RefTop - 1 do
  begin
    D := FMap.Get(Refs[R].Atom);
    if (D >= 0) and (Decls[D].Scope = SI) then
    begin
      if Refs[R].Inner then Decls[D].Captured := True;
    end
    else
    begin
      if W <> R then Refs[W] := Refs[R];
      Inc(W);
    end;
  end;
  RefTop := W;
end;

procedure TPreParser.PopScope;
var SI, I, D: Integer; IsFunc: Boolean;
begin
  SI := ScopeTop - 1;
  IsFunc := Scopes[SI].Kind in [skScript, skModule, skFunction];
  if VarTop > Scopes[SI].VarStart then CheckVarConflicts(SI);
  if IsFunc then
  begin
    for I := Scopes[SI].VarStart to VarTop - 1 do
      if not VarLog[I].Dropped then
      begin
        D := FMap.Get(VarLog[I].Atom);
        if (D < 0) or (Decls[D].Scope <> SI) then PushDecl(VarLog[I].Atom, dkVar, VarLog[I].Pos, False);
      end;
    VarTop := Scopes[SI].VarStart;
    if (ffUsesArguments in Ctxs[F].UsesF) and
       (Ctxs[F].Kind in [fkFunction, fkMethod, fkClassCtor, fkDerivedCtor, fkGetter, fkSetter]) then
    begin
      D := FMap.Get(AArguments);
      if (D < 0) or (Decls[D].Scope <> SI) then PushDecl(AArguments, dkArguments, 0, False);
    end;
  end;
  if (RefTop > Scopes[SI].RefStart) and (DeclTop > Scopes[SI].DeclStart) then ResolveRefs(SI);
  for I := DeclTop - 1 downto Scopes[SI].DeclStart do FMap.Put(Decls[I].Atom, Decls[I].Prev);
  DeclTop := Scopes[SI].DeclStart;
  Dec(ScopeTop);
end;

procedure TPreParser.PushCtx(K: TFuncKind; IsAsync, IsGen: Boolean);
var P: Integer;
begin
  if CtxTop >= Length(Ctxs) then SetLength(Ctxs, CtxTop * 2);
  P := CtxTop - 1;
  FillChar(Ctxs[CtxTop], SizeOf(TFuncCtx), 0);
  Ctxs[CtxTop].Kind := K;
  Ctxs[CtxTop].IsAsync := IsAsync;
  Ctxs[CtxTop].IsGen := IsGen;
  Ctxs[CtxTop].Rec := -1;
  Ctxs[CtxTop].DupParamPos := -1;
  Ctxs[CtxTop].SimpleParams := True;
  Ctxs[CtxTop].LabelBase := LabelTop;
  if P >= 0 then Ctxs[CtxTop].IsStrict := Ctxs[P].IsStrict;
  case K of
    fkArrow:
      begin
        Ctxs[CtxTop].SuperProp := Ctxs[P].SuperProp;
        Ctxs[CtxTop].SuperCall := Ctxs[P].SuperCall;
        Ctxs[CtxTop].NewTarget := Ctxs[P].NewTarget;
        Ctxs[CtxTop].ArgsBanned := Ctxs[P].ArgsBanned;
      end;
    fkFunction:
      Ctxs[CtxTop].NewTarget := True;
    fkMethod, fkGetter, fkSetter, fkClassCtor:
      begin
        Ctxs[CtxTop].SuperProp := True;
        Ctxs[CtxTop].NewTarget := True;
      end;
    fkDerivedCtor:
      begin
        Ctxs[CtxTop].SuperProp := True;
        Ctxs[CtxTop].SuperCall := True;
        Ctxs[CtxTop].NewTarget := True;
      end;
    fkFieldInit:
      begin
        Ctxs[CtxTop].SuperProp := True;
        Ctxs[CtxTop].NewTarget := True;
        Ctxs[CtxTop].ArgsBanned := True;
      end;
    fkStaticBlock:
      begin
        Ctxs[CtxTop].SuperProp := True;
        Ctxs[CtxTop].NewTarget := True;
        Ctxs[CtxTop].ArgsBanned := True;
        Ctxs[CtxTop].AwaitBanned := True;
      end;
  end;
  if K <> fkArrow then
  begin
    Ctxs[CtxTop].YieldOp := IsGen;
    Ctxs[CtxTop].AwaitOp := IsAsync;
  end;
  Inc(CtxTop);
end;

procedure TPreParser.PopCtx;
var C, P: Integer;
begin
  C := CtxTop - 1;
  P := C - 1;
  if Ctxs[C].Kind = fkArrow then
    Ctxs[P].UsesF := Ctxs[P].UsesF + Ctxs[C].UsesF * [ffUsesThis, ffUsesArguments, ffSuperProp, ffSuperCall, ffNewTarget];
  if Ctxs[C].UsesF * [ffDirectEval, ffInnerEval] <> [] then Include(Ctxs[P].UsesF, ffInnerEval);
  LabelTop := Ctxs[C].LabelBase;
  Dec(CtxTop);
end;

function TPreParser.NewRecord(Start: Integer): Integer;
begin
  if FuncCount >= Length(Funcs) then SetLength(Funcs, FuncCount * 2);
  Result := FuncCount;
  Inc(FuncCount);
  FillChar(Funcs[Result], SizeOf(TFuncRecord), 0);
  Funcs[Result].Start := Start;
  Funcs[Result].FreeStart := -1;
end;

{ after the function's scope has ended: its unresolved references, once each, become its free names and
  references of an inner function for the enclosing scopes }
procedure TPreParser.FinishFunction(Stop: Integer);
var C, R, W, Rec: Integer; A: PStr;
begin
  C := CtxTop - 1;
  Inc(FStamp);
  W := Ctxs[C].RefStart;
  for R := W to RefTop - 1 do
  begin
    A := Refs[R].Atom;
    if FSeen.Get(A) <> FStamp then
    begin
      FSeen.Put(A, FStamp);
      Refs[W].Atom := A;
      Refs[W].Inner := True;
      Inc(W);
    end;
  end;
  RefTop := W;
  Rec := Ctxs[C].Rec;
  if Rec < 0 then Exit;
  Funcs[Rec].Stop := Stop;
  Funcs[Rec].Flags := Funcs[Rec].Flags + Ctxs[C].UsesF;
  if Ctxs[C].IsStrict then Include(Funcs[Rec].Flags, ffStrict);
  if Ctxs[C].SimpleParams then Include(Funcs[Rec].Flags, ffSimpleParams);
  Funcs[Rec].FreeStart := FreeCount;
  Funcs[Rec].FreeCount := RefTop - Ctxs[C].RefStart;
  if FreeCount + Funcs[Rec].FreeCount > Length(FreeNames) then
    SetLength(FreeNames, (FreeCount + Funcs[Rec].FreeCount) * 2);
  for R := Ctxs[C].RefStart to RefTop - 1 do
  begin
    FreeNames[FreeCount] := Refs[R].Atom;
    Inc(FreeCount);
  end;
end;

{ identifiers }

{ an IdentifierReference or LabelIdentifier (§13.1.1) }
procedure TPreParser.CheckIdentRef(A: PStr; Pos: Integer);
begin
  case SpecOf(A) of
    spNone: ;
    spYield:
      if Ctxs[F].IsStrict or Ctxs[F].YieldOp then Err('Unexpected identifier ''yield''', Pos);
    spAwait:
      if (FGoal = pgModule) or Ctxs[F].AwaitOp or Ctxs[F].AwaitBanned then Err('Unexpected reserved word ''await''', Pos);
    spLet, spStrictReserved:
      if Ctxs[F].IsStrict then Err('Unexpected strict mode reserved word ''' + NameOf(A) + '''', Pos);
    spArguments:
      begin
        if Ctxs[F].ArgsBanned then
          Err('''arguments'' is not allowed in class field initializer or static initialization block', Pos);
        Include(Ctxs[F].UsesF, ffUsesArguments);
      end;
  end;
end;

{ a BindingIdentifier (§13.1.1, §8.6.4) }
procedure TPreParser.CheckBinding(A: PStr; Pos: Integer);
begin
  case SpecOf(A) of
    spNone: ;
    spEval, spArguments:
      if Ctxs[F].IsStrict then Err('Unexpected eval or arguments in strict mode', Pos);
    spYield:
      if Ctxs[F].IsStrict or Ctxs[F].YieldOp then Err('Unexpected identifier ''yield''', Pos);
    spAwait:
      if (FGoal = pgModule) or Ctxs[F].AwaitOp or Ctxs[F].AwaitBanned then Err('Unexpected reserved word ''await''', Pos);
    spLet, spStrictReserved:
      if Ctxs[F].IsStrict then Err('Unexpected strict mode reserved word ''' + NameOf(A) + '''', Pos);
  end;
end;

{ a name that became strict mode code after it was read: a function's name and simple parameters, once its
  body has the Use Strict Directive }
procedure TPreParser.CheckStrictBinding(A: PStr; Pos: Integer);
begin
  case SpecOf(A) of
    spEval, spArguments: Err('Unexpected eval or arguments in strict mode', Pos);
    spYield: Err('Unexpected identifier ''yield''', Pos);
    spLet, spStrictReserved: Err('Unexpected strict mode reserved word ''' + NameOf(A) + '''', Pos);
  end;
end;

{ the current token as a BindingIdentifier of a K declaration }
function TPreParser.BindingIdent(K: TDeclKind): PStr;
var Pos: Integer;
begin
  if S.Kind <> tkIdent then Unexpected;
  if S.EscapedReserved <> tkIdent then ErrHere('Keyword must not contain escaped characters');
  Result := S.Atom;
  Pos := S.TokStart;
  CheckBinding(Result, Pos);
  if (Result = ALet) and (K in [dkLet, dkConst, dkUsing]) then
    ErrHere('let is disallowed as a lexically bound name');
  if K = dkVar then DeclareVar(Result, Pos, False, False, -1) else Declare(Result, K, Pos, False);
  S.Next(sgDiv);
end;

procedure TPreParser.CheckSimpleTarget(const X: TX; Kind: Integer; Pos: Integer);
begin
  case X.K of
    xIdent:
      if Ctxs[F].IsStrict and ((X.A = AEval) or (X.A = AArguments)) then
        Err('Unexpected eval or arguments in strict mode', Pos);
    xMember, xPrivMember: ;
    xCall:
      if Ctxs[F].IsStrict or not (Kind in [tgAssign, tgCompound, tgUpdate, tgForInOf]) then
        Err('Invalid left-hand side in assignment', Pos);
  else
    if Kind = tgUpdate then Err('Invalid left-hand side expression in prefix operation', Pos);
    Err('Invalid left-hand side in assignment', Pos);
  end;
end;

{ expressions }

function MakeX(K: TXKind): TX; inline;
begin
  Result.K := K;
  Result.Paren := False;
  Result.Op := 0;
  Result.A := nil;
end;

function BinPrec(K: TTok): Integer; inline;
begin
  case K of
    tkQQ: Result := 1;
    tkOrOr: Result := 2;
    tkAndAnd: Result := 3;
    tkBar: Result := 4;
    tkCaret: Result := 5;
    tkAmp: Result := 6;
    tkEq, tkNe, tkSEq, tkSNe: Result := 7;
    tkLt, tkGt, tkLe, tkGe, tkInstanceof, tkIn: Result := 8;
    tkShl, tkSar, tkShr: Result := 9;
    tkPlus, tkMinus: Result := 10;
    tkStar, tkSlash, tkPercent: Result := 11;
    tkStarStar: Result := 12;
  else
    Result := 0;
  end;
end;

procedure TPreParser.CoverError(Pos: Integer; const Msg: string);
begin
  if FCoverErr < 0 then
  begin
    FCoverErr := Pos;
    FCoverMsg := Msg;
  end;
end;

function TPreParser.ParseExpression(Fl: Integer): TX;
begin
  Result := ParseAssignment(Fl);
  while S.Kind = tkComma do
  begin
    if FCoverErr >= 0 then Err(FCoverMsg, FCoverErr);
    S.Next(sgRegExp);
    ParseAssignment(Fl);
    Result := MakeX(xOther);
  end;
end;

function TPreParser.ParseYield(Fl: Integer): TX;
begin
  if Ctxs[F].InParams then ErrHere('Yield expression not allowed in formal parameter');
  S.Next(sgRegExp);
  if not S.NewlineBefore then
  begin
    if S.Kind = tkStar then
    begin
      S.Next(sgRegExp);
      ParseAssignment(Fl and afNoIn);
    end
    else if StartsExpression then ParseAssignment(Fl and afNoIn);
  end;
  Result := MakeX(xOther);
end;

function TPreParser.ParseAssignment(Fl: Integer): TX;
var Saved, Pos: Integer; M: TMark; HaveMark: Boolean; K: TTok;
begin
  if Ctxs[F].YieldOp and IsId(AYield) then Exit(ParseYield(Fl));
  Saved := FCoverErr;
  FCoverErr := -1;
  HaveMark := (S.Kind = tkLBracket) or (S.Kind = tkLBrace);
  if HaveMark then M := Mark;
  Pos := S.TokStart;
  Result := ParseConditional(Fl);
  if Result.K <> xArrow then
  begin
    K := S.Kind;
    case K of
      tkAssign:
        begin
          if (Result.K in [xObject, xArray]) and not Result.Paren and HaveMark then
          begin
            Reset(M);
            FCoverErr := -1;
            ParseAssignPattern;
            if S.Kind <> tkAssign then Unexpected;
          end
          else CheckSimpleTarget(Result, tgAssign, Pos);
          FCoverErr := -1;
          S.Next(sgRegExp);
          ParseAssignment(Fl and afNoIn);
          Result := MakeX(xOther);
        end;
      tkPlusAssign, tkMinusAssign, tkStarAssign, tkSlashAssign, tkPercentAssign, tkStarStarAssign, tkShlAssign,
      tkSarAssign, tkShrAssign, tkAmpAssign, tkBarAssign, tkCaretAssign, tkAndAndAssign, tkOrOrAssign, tkQQAssign:
        begin
          if K in [tkAndAndAssign, tkOrOrAssign, tkQQAssign] then CheckSimpleTarget(Result, tgLogical, Pos)
          else CheckSimpleTarget(Result, tgCompound, Pos);
          S.Next(sgRegExp);
          ParseAssignment(Fl and afNoIn);
          Result := MakeX(xOther);
        end;
    end;
  end;
  if FCoverErr >= 0 then
  begin
    if ((Fl and afAllowCover) = 0) or not (Result.K in [xObject, xArray]) or Result.Paren then
      Err(FCoverMsg, FCoverErr);
  end
  else FCoverErr := Saved;
end;

function TPreParser.ParseConditional(Fl: Integer): TX;
begin
  Result := ParseBinary(0, Fl);
  if (Result.K <> xArrow) and (S.Kind = tkQuestion) then
  begin
    S.Next(sgRegExp);
    ParseAssignment(0);
    Expect(tkColon, sgRegExp);
    ParseAssignment(Fl and afNoIn);
    Result := MakeX(xOther);
  end;
end;

function TPreParser.ParseBinary(MinPrec, Fl: Integer): TX;
var K: TTok; P: Integer; R: TX;
begin
  if S.Kind = tkPrivateName then
  begin
    { RelationalExpression : PrivateIdentifier in ShiftExpression }
    if MinPrec > 8 then Unexpected;
    UsePrivate(S.Atom, S.TokStart);
    S.Next(sgDiv);
    if (S.Kind <> tkIn) or ((Fl and afNoIn) <> 0) then Unexpected;
    Result := MakeX(xPrivName);
  end
  else
  begin
    Result := ParseUnary(Fl);
    if Result.K = xArrow then Exit;
  end;
  while True do
  begin
    K := S.Kind;
    P := BinPrec(K);
    if (P = 0) or (P < MinPrec) then Break;
    if (K = tkIn) and ((Fl and afNoIn) <> 0) then Break;
    if (Result.K = xPrivName) and (K <> tkIn) then Unexpected;
    if (K = tkStarStar) and (Result.Op = opUnary) then
      ErrHere('Unary operator used immediately before exponentiation expression. Parenthesis must be used to ' +
        'disambiguate operator precedence');
    S.Next(sgRegExp);
    if K = tkStarStar then R := ParseBinary(P, Fl) else R := ParseBinary(P + 1, Fl);
    if R.K = xArrow then ErrHere('Malformed arrow function parameter list');
    if K = tkQQ then
    begin
      if (Result.Op = opOrAnd) or (R.Op = opOrAnd) then
        ErrHere('Unexpected token ''??'': || and && cannot be mixed with ?? without parentheses');
    end
    else if (K = tkOrOr) or (K = tkAndAnd) then
    begin
      if (Result.Op = opCoalesce) or (R.Op = opCoalesce) then
        ErrHere('|| and && cannot be mixed with ?? without parentheses');
    end;
    Result := MakeX(xOther);
    if K = tkQQ then Result.Op := opCoalesce
    else if (K = tkOrOr) or (K = tkAndAnd) then Result.Op := opOrAnd;
  end;
end;

function TPreParser.ParseUnary(Fl: Integer): TX;
var K: TTok; Pos: Integer;
begin
  K := S.Kind;
  case K of
    tkDelete, tkVoid, tkTypeof, tkPlus, tkMinus, tkTilde, tkBang:
      begin
        Pos := S.TokStart;
        S.Next(sgRegExp);
        Result := ParseUnary(Fl);
        if Result.K = xArrow then ErrHere('Malformed arrow function parameter list');
        if K = tkDelete then
        begin
          if (Result.K = xIdent) and Ctxs[F].IsStrict then Err('Delete of an unqualified identifier in strict mode.', Pos);
          if Result.K in [xPrivMember, xOptPriv] then Err('Private fields can not be deleted', Pos);
        end;
        Result := MakeX(xOther);
        Result.Op := opUnary;
      end;
    tkInc, tkDec:
      begin
        S.Next(sgRegExp);
        Pos := S.TokStart;
        Result := ParseUnary(Fl);
        if Result.K = xArrow then ErrHere('Malformed arrow function parameter list');
        CheckSimpleTarget(Result, tgUpdate, Pos);
        Result := MakeX(xOther);
      end;
    tkIdent:
      if Ctxs[F].AwaitOp and IsId(AAwait) then
      begin
        if Ctxs[F].InParams then ErrHere('Illegal await-expression in formal parameters of async function');
        S.Next(sgRegExp);
        Result := ParseUnary(Fl);
        if Result.K = xArrow then ErrHere('Malformed arrow function parameter list');
        Result := MakeX(xOther);
        Result.Op := opUnary;
      end
      else Result := ParsePostfix(Fl);
  else
    Result := ParsePostfix(Fl);
  end;
end;

function TPreParser.ParsePostfix(Fl: Integer): TX;
var Pos: Integer;
begin
  Pos := S.TokStart;
  Result := ParseLHS(Fl);
  if Result.K = xArrow then Exit;
  if ((S.Kind = tkInc) or (S.Kind = tkDec)) and not S.NewlineBefore then
  begin
    CheckSimpleTarget(Result, tgUpdate, Pos);
    S.Next(sgDiv);
    Result := MakeX(xOther);
  end;
end;

function TPreParser.ParseLHS(Fl: Integer): TX;
begin
  case S.Kind of
    tkNew: Result := ParseNew;
    tkSuper: Result := ParseSuper(False);
    tkImport: Result := ParseImportExpr(False);
  else
    Result := ParsePrimary(Fl);
    if Result.K = xArrow then Exit;
  end;
  ParseCallTail(Result, False);
end;

{ member accesses, calls, tagged templates and optional chains after X; a new expression's callee (NewCallee)
  stops before its arguments }
procedure TPreParser.ParseCallTail(var X: TX; NewCallee: Boolean);
var InChain: Boolean;
begin
  InChain := False;
  while True do
    case S.Kind of
      tkDot:
        begin
          S.Next(sgDiv);
          if S.Kind = tkPrivateName then
          begin
            UsePrivate(S.Atom, S.TokStart);
            if InChain then X.K := xOptPriv else X.K := xPrivMember;
          end
          else if IsIdentName then
          begin
            if InChain then X.K := xOptional else X.K := xMember;
          end
          else Unexpected;
          S.Next(sgDiv);
          X.Paren := False;
          X.Op := 0;
        end;
      tkLBracket:
        begin
          S.Next(sgRegExp);
          ParseExpression(0);
          Expect(tkRBracket, sgDiv);
          if InChain then X.K := xOptional else X.K := xMember;
          X.Paren := False;
          X.Op := 0;
        end;
      tkLParen:
        begin
          if NewCallee then Break;
          if (X.K = xIdent) and (X.A = AEval) then Include(Ctxs[F].UsesF, ffDirectEval);
          ParseArguments(False);
          if InChain then X.K := xOptional else X.K := xCall;
          X.Paren := False;
          X.Op := 0;
        end;
      tkTemplate, tkTemplateHead:
        begin
          if InChain then ErrHere('Invalid tagged template on optional chain');
          ParseTemplate(True);
          X := MakeX(xOther);
        end;
      tkQDot:
        begin
          if NewCallee then ErrHere('Invalid optional chain from new expression');
          InChain := True;
          S.Next(sgDiv);
          X := MakeX(xOptional);
          case S.Kind of
            tkLParen: ParseArguments(False);
            tkLBracket:
              begin
                S.Next(sgRegExp);
                ParseExpression(0);
                Expect(tkRBracket, sgDiv);
              end;
            tkPrivateName:
              begin
                UsePrivate(S.Atom, S.TokStart);
                S.Next(sgDiv);
                X.K := xOptPriv;
              end;
            tkTemplate, tkTemplateHead: ErrHere('Invalid tagged template on optional chain');
          else
            if not IsIdentName then Unexpected;
            S.Next(sgDiv);
          end;
        end;
    else
      Break;
    end;
end;

function TPreParser.ParseNew: TX;
var Pos: Integer;
begin
  Pos := S.TokStart;
  S.Next(sgRegExp);
  if S.Kind = tkDot then
  begin
    S.Next(sgDiv);
    if not IsId(ATarget) then
    begin
      if (S.Kind = tkIdent) and (S.Atom = ATarget) then ErrHere('Keyword must not contain escaped characters');
      Unexpected;
    end;
    if not Ctxs[F].NewTarget then Err('new.target expression is not allowed here', Pos);
    Include(Ctxs[F].UsesF, ffNewTarget);
    S.Next(sgDiv);
    Exit(MakeX(xOther));
  end;
  case S.Kind of
    tkNew: Result := ParseNew();
    tkSuper: Result := ParseSuper(True);
    tkImport: Result := ParseImportExpr(True);
  else
    Result := ParsePrimary(0);
    if Result.K = xArrow then ErrHere('Malformed arrow function parameter list');
  end;
  ParseCallTail(Result, True);
  if S.Kind = tkLParen then ParseArguments(False);
  Result := MakeX(xOther);
end;

function TPreParser.ParseSuper(InNew: Boolean): TX;
var Pos: Integer;
begin
  Pos := S.TokStart;
  S.Next(sgDiv);
  case S.Kind of
    tkDot:
      begin
        if not Ctxs[F].SuperProp then Err('''super'' keyword unexpected here', Pos);
        Include(Ctxs[F].UsesF, ffSuperProp);
        S.Next(sgDiv);
        if not IsIdentName then Unexpected;
        S.Next(sgDiv);
        Result := MakeX(xMember);
      end;
    tkLBracket:
      begin
        if not Ctxs[F].SuperProp then Err('''super'' keyword unexpected here', Pos);
        Include(Ctxs[F].UsesF, ffSuperProp);
        S.Next(sgRegExp);
        ParseExpression(0);
        Expect(tkRBracket, sgDiv);
        Result := MakeX(xMember);
      end;
    tkLParen:
      begin
        if InNew or not Ctxs[F].SuperCall then Err('''super'' keyword unexpected here', Pos);
        Include(Ctxs[F].UsesF, ffSuperCall);
        ParseArguments(False);
        Result := MakeX(xOther);
      end;
  else
    Err('''super'' keyword unexpected here', Pos);
  end;
end;

function TPreParser.ParseImportExpr(InNew: Boolean): TX;
var Pos: Integer;
begin
  Pos := S.TokStart;
  S.Next(sgDiv);
  Result := MakeX(xOther);
  if S.Kind = tkDot then
  begin
    S.Next(sgDiv);
    if IsId(AMeta) then
    begin
      if FGoal <> pgModule then Err('Cannot use ''import.meta'' outside a module', Pos);
      S.Next(sgDiv);
      Exit;
    end;
    if not (IsId(ASource) or IsId(ADefer)) then
    begin
      if (S.Kind = tkIdent) and S.Escaped then ErrHere('Keyword must not contain escaped characters');
      Unexpected;
    end;
    S.Next(sgDiv);
    if S.Kind <> tkLParen then Unexpected;
  end
  else if S.Kind <> tkLParen then Unexpected;
  if InNew then Err('Cannot use new with import', Pos);
  S.Next(sgRegExp);
  ParseAssignment(0);
  if S.Kind = tkComma then
  begin
    S.Next(sgRegExp);
    if S.Kind <> tkRParen then
    begin
      ParseAssignment(0);
      if S.Kind = tkComma then S.Next(sgRegExp);
    end;
  end;
  Expect(tkRParen, sgDiv);
end;

procedure TPreParser.ParseArguments(Cover: Boolean);
begin
  S.Next(sgRegExp);
  while S.Kind <> tkRParen do
  begin
    if S.Kind = tkEllipsis then S.Next(sgRegExp);
    if Cover then ParseAssignment(afAllowCover) else ParseAssignment(0);
    if S.Kind = tkComma then S.Next(sgRegExp)
    else if S.Kind <> tkRParen then Unexpected;
  end;
  S.Next(sgDiv);
end;

procedure TPreParser.ParseTemplate(Tagged: Boolean);
begin
  if not Tagged and not S.CookedValid then ErrHere('Invalid escape sequence in template');
  if S.Kind = tkTemplate then
  begin
    S.Next(sgDiv);
    Exit;
  end;
  repeat
    S.Next(sgRegExp);
    ParseExpression(0);
    if S.Kind <> tkRBrace then Unexpected;
    S.Rescan(sgTemplateTail);
    if not Tagged and not S.CookedValid then ErrHere('Invalid escape sequence in template');
  until S.Kind = tkTemplateTail;
  S.Next(sgDiv);
end;

procedure TPreParser.CheckRegExp;
var Pat, Fl, Msg: UnicodeString;
begin
  if not Assigned(PreParseRegExpCheck) then Exit;
  SetString(Pat, S.Src + S.RegexBodyStart, S.RegexBodyLen);
  SetString(Fl, S.Src + S.RegexFlagsStart, S.RegexFlagsLen);
  Msg := PreParseRegExpCheck(Pat, Fl);
  if Msg <> '' then ErrHere('Invalid regular expression: ' + UTF8Encode(Msg));
end;

function TPreParser.ParsePrimary(Fl: Integer): TX;
var Start: Integer;
begin
  Result := MakeX(xOther);
  case S.Kind of
    tkIdent: Result := ParseIdentPrimary(Fl);
    tkThis:
      begin
        Include(Ctxs[F].UsesF, ffUsesThis);
        S.Next(sgDiv);
      end;
    tkNumber:
      begin
        if S.LegacyOctal and Ctxs[F].IsStrict then ErrHere('Octal literals are not allowed in strict mode.');
        S.Next(sgDiv);
        Result.K := xLiteral;
      end;
    tkBigInt:
      begin
        S.Next(sgDiv);
        Result.K := xLiteral;
      end;
    tkString:
      begin
        if S.LegacyOctal and Ctxs[F].IsStrict then ErrHere('Octal escape sequences are not allowed in strict mode.');
        S.Next(sgDiv);
        Result.K := xString;
      end;
    tkTemplate, tkTemplateHead: ParseTemplate(False);
    tkRegExp:
      begin
        CheckRegExp;
        S.Next(sgDiv);
      end;
    tkSlash, tkSlashAssign:
      begin
        S.Rescan(sgRegExp);
        if S.Kind <> tkRegExp then Unexpected;
        CheckRegExp;
        S.Next(sgDiv);
      end;
    tkNull, tkTrue, tkFalse:
      begin
        S.Next(sgDiv);
        Result.K := xLiteral;
      end;
    tkLBracket: Result := ParseArrayLiteral;
    tkLBrace: Result := ParseObjectLiteral;
    tkLParen: Result := ParseParenOrArrow(Fl);
    tkFunction: ParseFunction(S.TokStart, False, True, False);
    tkClass: ParseClass(S.TokStart, True, False);
    tkAt:
      begin
        Start := S.TokStart;
        ParseDecorators;
        if S.Kind <> tkClass then Unexpected;
        ParseClass(Start, True, False);
      end;
  else
    Unexpected;
  end;
end;

function TPreParser.ParseIdentPrimary(Fl: Integer): TX;
var A: PStr; Pos: Integer;
begin
  if IsId(AAsync) then Exit(ParseAsyncPrimary(Fl));
  if S.EscapedReserved <> tkIdent then ErrHere('Keyword must not contain escaped characters');
  A := S.Atom;
  Pos := S.TokStart;
  CheckIdentRef(A, Pos);
  S.Next(sgDiv);
  if (S.Kind = tkArrow) and not S.NewlineBefore then
  begin
    ParseArrow(Pos, False, Fl, A, Pos);
    Exit(MakeX(xArrow));
  end;
  AddRef(A);
  Result := MakeX(xIdent);
  Result.A := A;
end;

function TPreParser.ParseAsyncPrimary(Fl: Integer): TX;
var M: TMark; Pos, P2, Saved: Integer; A: PStr;
begin
  Pos := S.TokStart;
  M := Mark;
  S.Next(sgDiv);
  if not S.NewlineBefore then
    case S.Kind of
      tkFunction:
        begin
          ParseFunction(Pos, True, True, False);
          Exit(MakeX(xOther));
        end;
      tkIdent:
        begin
          { async x => ... }
          if S.EscapedReserved <> tkIdent then ErrHere('Keyword must not contain escaped characters');
          A := S.Atom;
          P2 := S.TokStart;
          S.Next(sgDiv);
          if (S.Kind = tkArrow) and not S.NewlineBefore then
          begin
            ParseArrow(Pos, True, Fl, A, P2);
            Exit(MakeX(xArrow));
          end;
          { not an arrow: async is an identifier (for await (async of ...)); what follows is the caller's }
          Reset(M);
          S.Next(sgDiv);
        end;
      tkLParen:
        begin
          Saved := FCoverErr;
          FCoverErr := -1;
          ParseArguments(True);
          if (S.Kind = tkArrow) and not S.NewlineBefore then
          begin
            Reset(M);
            FCoverErr := Saved;
            S.Next(sgDiv);
            ParseArrow(Pos, True, Fl, nil, 0);
            Exit(MakeX(xArrow));
          end;
          if FCoverErr >= 0 then Err(FCoverMsg, FCoverErr);
          FCoverErr := Saved;
          AddRef(AAsync);
          Exit(MakeX(xCall));
        end;
    end;
  if (S.Kind = tkArrow) and not S.NewlineBefore then
  begin
    ParseArrow(Pos, False, Fl, AAsync, Pos);
    Exit(MakeX(xArrow));
  end;
  AddRef(AAsync);
  Result := MakeX(xIdent);
  Result.A := AAsync;
end;

function TPreParser.ParseParenOrArrow(Fl: Integer): TX;
var M: TMark; Pos, Saved, ArrowOnly, Count: Integer; X: TX;
begin
  M := Mark;
  Pos := S.TokStart;
  Saved := FCoverErr;
  FCoverErr := -1;
  S.Next(sgRegExp);
  ArrowOnly := -1;
  Count := 0;
  X := MakeX(xOther);
  if S.Kind = tkRParen then ArrowOnly := S.TokStart
  else
    while True do
    begin
      if S.Kind = tkEllipsis then
      begin
        ArrowOnly := S.TokStart;
        S.Next(sgRegExp);
        ParseAssignment(afAllowCover);
        Break;
      end;
      X := ParseAssignment(afAllowCover);
      if X.K = xArrow then X.K := xOther;
      Inc(Count);
      if S.Kind <> tkComma then Break;
      S.Next(sgRegExp);
      if S.Kind = tkRParen then
      begin
        ArrowOnly := S.TokStart;
        Break;
      end;
    end;
  if S.Kind <> tkRParen then Unexpected;
  S.Next(sgDiv);
  if (S.Kind = tkArrow) and not S.NewlineBefore then
  begin
    Reset(M);
    FCoverErr := Saved;
    ParseArrow(Pos, False, Fl, nil, 0);
    Exit(MakeX(xArrow));
  end;
  if ArrowOnly >= 0 then Err('Unexpected token', ArrowOnly);
  if FCoverErr >= 0 then Err(FCoverMsg, FCoverErr);
  FCoverErr := Saved;
  if Count = 1 then Result := X else Result := MakeX(xOther);
  Result.Paren := True;
  Result.Op := 0;
end;

function TPreParser.ParseArrayLiteral: TX;
begin
  S.Next(sgRegExp);
  while S.Kind <> tkRBracket do
  begin
    if S.Kind = tkComma then
    begin
      S.Next(sgRegExp);
      Continue;
    end;
    if S.Kind = tkEllipsis then S.Next(sgRegExp);
    ParseAssignment(afAllowCover);
    if S.Kind = tkComma then S.Next(sgRegExp)
    else if S.Kind <> tkRBracket then Unexpected;
  end;
  S.Next(sgDiv);
  Result := MakeX(xArray);
end;

{ a PropertyName (§13.2.5): Name is its atom when it is an identifier name, or a string literal spelling
  __proto__, constructor or prototype; IsComputed for [ ... ]. True when the name is a plain identifier (a
  possible shorthand); the token is consumed. }
function TPreParser.ParsePropertyName(out Name: PStr; out IsComputed: Boolean): Boolean;
begin
  Name := nil;
  IsComputed := False;
  Result := False;
  case S.Kind of
    tkIdent:
      begin
        Name := S.Atom;
        Result := S.EscapedReserved = tkIdent;
      end;
    tkString:
      begin
        if S.LegacyOctal and Ctxs[F].IsStrict then ErrHere('Octal escape sequences are not allowed in strict mode.');
        if (S.BufLen = 9) or (S.BufLen = 11) then Name := AtomOfChars(@S.Buf[0], S.BufLen);
      end;
    tkNumber:
      if S.LegacyOctal and Ctxs[F].IsStrict then ErrHere('Octal literals are not allowed in strict mode.');
    tkBigInt: ;
    tkLBracket:
      begin
        IsComputed := True;
        S.Next(sgRegExp);
        ParseAssignment(0);
        if S.Kind <> tkRBracket then Unexpected;
      end;
  else
    if S.Kind >= tkBreak then Name := S.TokAtom else Unexpected;
  end;
  S.Next(sgDiv);
end;

function TPreParser.ParseObjectLiteral: TX;
var ProtoCount, ProtoPos, PStart, KeyPos: Integer; IsAsync, IsGen, IsComputed, IsIdent, Named: Boolean;
  Acc: Integer; Name: PStr; FK: TFuncKind;
begin
  S.Next(sgDiv);
  ProtoCount := 0;
  ProtoPos := -1;
  while S.Kind <> tkRBrace do
  begin
    if S.Kind = tkEllipsis then
    begin
      S.Next(sgRegExp);
      ParseAssignment(afAllowCover);
    end
    else
    begin
      PStart := S.TokStart;
      IsAsync := False;
      IsGen := False;
      Acc := 0;
      Named := False;
      Name := nil;
      IsComputed := False;
      IsIdent := False;
      KeyPos := S.TokStart;
      if S.Kind = tkStar then
      begin
        IsGen := True;
        S.Next(sgDiv);
      end
      else if IsId(AAsync) or IsId(AGet) or IsId(ASet) then
      begin
        Name := S.Atom;
        S.Next(sgDiv);
        if S.Kind in [tkLParen, tkColon, tkComma, tkRBrace, tkAssign] then
        begin
          Named := True;
          IsIdent := True;
        end
        else if Name = AAsync then
        begin
          if S.NewlineBefore then Unexpected;
          IsAsync := True;
          if S.Kind = tkStar then
          begin
            IsGen := True;
            S.Next(sgDiv);
          end;
        end
        else if Name = AGet then Acc := 1
        else Acc := 2;
      end;
      if not Named then
      begin
        KeyPos := S.TokStart;
        IsIdent := ParsePropertyName(Name, IsComputed);
      end;
      if S.Kind = tkLParen then
      begin
        if Acc = 1 then FK := fkGetter else if Acc = 2 then FK := fkSetter else FK := fkMethod;
        ParseMethod(PStart, FK, IsAsync, IsGen);
      end
      else
      begin
        if IsAsync or IsGen or (Acc <> 0) then Unexpected;
        if S.Kind = tkColon then
        begin
          S.Next(sgRegExp);
          ParseAssignment(afAllowCover);
          if not IsComputed and (Name = AProto) then
          begin
            Inc(ProtoCount);
            if ProtoCount = 2 then ProtoPos := KeyPos;
          end;
        end
        else
        begin
          { shorthand: an IdentifierReference, perhaps with a CoverInitializedName initializer }
          if not IsIdent then
          begin
            if (Name <> nil) and (S.PrevEnd > KeyPos) and (S.Src[KeyPos] = '\') then
              Err('Keyword must not contain escaped characters', KeyPos);
            Err('Unexpected token', KeyPos);
          end;
          CheckIdentRef(Name, KeyPos);
          AddRef(Name);
          if S.Kind = tkAssign then
          begin
            CoverError(S.TokStart, 'Invalid shorthand property initializer');
            S.Next(sgRegExp);
            ParseAssignment(0);
          end;
        end;
      end;
    end;
    if S.Kind = tkComma then S.Next(sgDiv)
    else if S.Kind <> tkRBrace then Unexpected;
  end;
  if ProtoCount > 1 then CoverError(ProtoPos, 'Duplicate __proto__ fields are not allowed in object literals');
  S.Next(sgDiv);
  Result := MakeX(xObject);
end;

{ patterns }

procedure TPreParser.ParseBindingTarget(K: TDeclKind);
var A: PStr; Pos: Integer; Esc: TTok; Name: PStr; IsComputed: Boolean;
begin
  case S.Kind of
    tkIdent: BindingIdent(K);
    tkLBracket:
      begin
        S.Next(sgDiv);
        while S.Kind <> tkRBracket do
        begin
          if S.Kind = tkComma then
          begin
            S.Next(sgDiv);
            Continue;
          end;
          if S.Kind = tkEllipsis then
          begin
            S.Next(sgDiv);
            ParseBindingTarget(K);
            if S.Kind <> tkRBracket then ErrHere('Rest element must be last element');
            Break;
          end;
          ParseBindingElement(K);
          if S.Kind = tkComma then S.Next(sgDiv)
          else if S.Kind <> tkRBracket then Unexpected;
        end;
        S.Next(sgDiv);
      end;
    tkLBrace:
      begin
        S.Next(sgDiv);
        while S.Kind <> tkRBrace do
        begin
          if S.Kind = tkEllipsis then
          begin
            S.Next(sgDiv);
            if S.Kind <> tkIdent then Unexpected;
            BindingIdent(K);
            if S.Kind <> tkRBrace then ErrHere('Rest element must be last element');
            Break;
          end;
          if S.Kind = tkIdent then
          begin
            A := S.Atom;
            Pos := S.TokStart;
            Esc := S.EscapedReserved;
            S.Next(sgDiv);
            if S.Kind = tkColon then
            begin
              S.Next(sgDiv);
              ParseBindingElement(K);
            end
            else
            begin
              if Esc <> tkIdent then Err('Keyword must not contain escaped characters', Pos);
              CheckBinding(A, Pos);
              if (A = ALet) and (K in [dkLet, dkConst, dkUsing]) then
                Err('let is disallowed as a lexically bound name', Pos);
              if K = dkVar then DeclareVar(A, Pos, False, False, -1) else Declare(A, K, Pos, False);
              if S.Kind = tkAssign then
              begin
                S.Next(sgRegExp);
                ParseAssignment(0);
              end;
            end;
          end
          else
          begin
            ParsePropertyName(Name, IsComputed);
            Expect(tkColon, sgDiv);
            ParseBindingElement(K);
          end;
          if S.Kind = tkComma then S.Next(sgDiv)
          else if S.Kind <> tkRBrace then Unexpected;
        end;
        S.Next(sgDiv);
      end;
  else
    Unexpected;
  end;
end;

procedure TPreParser.ParseBindingElement(K: TDeclKind);
begin
  ParseBindingTarget(K);
  if S.Kind = tkAssign then
  begin
    S.Next(sgRegExp);
    ParseAssignment(0);
  end;
end;

{ an AssignmentPattern (§13.15.5), read again after the literal it covers was followed by = }
procedure TPreParser.ParseAssignPattern;
var A: PStr; Pos: Integer; Esc: TTok; Name: PStr; IsComputed: Boolean; X: TX;
begin
  if S.Kind = tkLBracket then
  begin
    S.Next(sgRegExp);
    while S.Kind <> tkRBracket do
    begin
      if S.Kind = tkComma then
      begin
        S.Next(sgRegExp);
        Continue;
      end;
      if S.Kind = tkEllipsis then
      begin
        S.Next(sgRegExp);
        ParseAssignTarget;
        if S.Kind = tkAssign then ErrHere('Rest element may not have a default initializer');
        if S.Kind <> tkRBracket then ErrHere('Rest element must be last element');
        Break;
      end;
      ParseAssignTarget;
      if S.Kind = tkAssign then
      begin
        S.Next(sgRegExp);
        ParseAssignment(0);
      end;
      if S.Kind = tkComma then S.Next(sgRegExp)
      else if S.Kind <> tkRBracket then Unexpected;
    end;
    S.Next(sgDiv);
    Exit;
  end;
  S.Next(sgDiv);
  while S.Kind <> tkRBrace do
  begin
    if S.Kind = tkEllipsis then
    begin
      S.Next(sgRegExp);
      Pos := S.TokStart;
      X := ParseLHS(0);
      if X.K = xArrow then ErrHere('Malformed arrow function parameter list');
      if FCoverErr >= 0 then Err(FCoverMsg, FCoverErr);
      if (X.K in [xObject, xArray]) and not X.Paren then
        Err('`...` must be followed by an assignable reference in assignment contexts', Pos);
      CheckSimpleTarget(X, tgPattern, Pos);
      if S.Kind <> tkRBrace then ErrHere('Rest element must be last element');
      Break;
    end;
    if S.Kind = tkIdent then
    begin
      A := S.Atom;
      Pos := S.TokStart;
      Esc := S.EscapedReserved;
      S.Next(sgDiv);
      if S.Kind = tkColon then
      begin
        S.Next(sgRegExp);
        ParseAssignTarget;
      end
      else
      begin
        if Esc <> tkIdent then Err('Keyword must not contain escaped characters', Pos);
        CheckIdentRef(A, Pos);
        if Ctxs[F].IsStrict and ((A = AEval) or (A = AArguments)) then
          Err('Unexpected eval or arguments in strict mode', Pos);
        AddRef(A);
      end;
    end
    else
    begin
      ParsePropertyName(Name, IsComputed);
      if S.Kind <> tkColon then Unexpected;
      S.Next(sgRegExp);
      ParseAssignTarget;
    end;
    if S.Kind = tkAssign then
    begin
      S.Next(sgRegExp);
      ParseAssignment(0);
    end;
    if S.Kind = tkComma then S.Next(sgDiv)
    else if S.Kind <> tkRBrace then Unexpected;
  end;
  S.Next(sgDiv);
end;

{ a DestructuringAssignmentTarget: a nested pattern, or a simple assignment target }
procedure TPreParser.ParseAssignTarget;
var M: TMark; Saved, Pos: Integer; X: TX;
begin
  Pos := S.TokStart;
  if (S.Kind = tkLBracket) or (S.Kind = tkLBrace) then
  begin
    M := Mark;
    Saved := FCoverErr;
    FCoverErr := -1;
    X := ParseLHS(0);
    if (X.K in [xObject, xArray]) and not X.Paren and
       (S.Kind in [tkAssign, tkComma, tkRBracket, tkRBrace]) then
    begin
      Reset(M);
      FCoverErr := -1;
      ParseAssignPattern;
      FCoverErr := Saved;
      Exit;
    end;
    if FCoverErr >= 0 then Err(FCoverMsg, FCoverErr);
    FCoverErr := Saved;
  end
  else X := ParseLHS(0);
  if X.K = xArrow then ErrHere('Malformed arrow function parameter list');
  CheckSimpleTarget(X, tgPattern, Pos);
end;

{ functions }

function SrcIs(P: PWideChar; const W: UnicodeString): Boolean;
var I: Integer;
begin
  for I := 1 to Length(W) do
    if P[I - 1] <> W[I] then Exit(False);
  Result := True;
end;

procedure TPreParser.CheckParamsStrict;
var I: Integer;
begin
  if Ctxs[F].Name <> nil then CheckStrictBinding(Ctxs[F].Name, Ctxs[F].NamePos);
  if Ctxs[F].Kind in [fkScript, fkModule] then Exit;
  for I := Scopes[Ctxs[F].Scope].DeclStart to DeclTop - 1 do
    if Decls[I].Kind = dkParam then CheckStrictBinding(Decls[I].Atom, Decls[I].Pos);
  if Ctxs[F].DupParamPos >= 0 then Err('Duplicate parameter name not allowed in this context', Ctxs[F].DupParamPos);
end;

{ the Directive Prologue (§11.2.1) of the current function or script }
procedure TPreParser.ParseDirectives;
var P, OctPos: Integer; UseStrict, Oct: Boolean; X: TX;
begin
  OctPos := -1;
  while S.Kind = tkString do
  begin
    P := S.TokStart;
    UseStrict := (S.TokEnd - P = 12) and SrcIs(S.Src + P + 1, 'use strict');
    Oct := S.LegacyOctal;
    X := ParseExpression(0);
    Semi;
    if (X.K <> xString) or X.Paren then Exit;
    if Oct and (OctPos < 0) then OctPos := P;
    if UseStrict then
    begin
      if not Ctxs[F].SimpleParams then
        Err('Illegal ''use strict'' directive in function with non-simple parameter list', P);
      if not Ctxs[F].IsStrict then
      begin
        Ctxs[F].IsStrict := True;
        CheckParamsStrict;
      end;
    end;
    if Ctxs[F].IsStrict and (OctPos >= 0) then Err('Octal escape sequences are not allowed in strict mode.', OctPos);
  end;
end;

procedure TPreParser.ParseFunctionBody;
begin
  Expect(tkLBrace, sgRegExp);
  ParseDirectives;
  ParseStatementList(tkRBrace);
end;

procedure TPreParser.ParseFormalParams(Unique: Boolean);
var Simple, HasDefault: Boolean; Count: Integer;
begin
  Expect(tkLParen, sgDiv);
  Simple := True;
  HasDefault := False;
  Count := 0;
  while S.Kind <> tkRParen do
  begin
    if S.Kind = tkEllipsis then
    begin
      S.Next(sgDiv);
      Simple := False;
      ParseBindingTarget(dkParam);
      if S.Kind = tkAssign then ErrHere('Rest parameter may not have a default initializer');
      if S.Kind <> tkRParen then ErrHere('Rest parameter must be last formal parameter');
      Break;
    end;
    if S.Kind <> tkIdent then Simple := False;
    ParseBindingTarget(dkParam);
    if S.Kind = tkAssign then
    begin
      Simple := False;
      HasDefault := True;
      S.Next(sgRegExp);
      ParseAssignment(0);
    end
    else if not HasDefault then Inc(Count);
    if S.Kind = tkComma then S.Next(sgDiv)
    else if S.Kind <> tkRParen then Unexpected;
  end;
  S.Next(sgDiv);
  Ctxs[F].SimpleParams := Simple;
  if Ctxs[F].Rec >= 0 then Funcs[Ctxs[F].Rec].ParamCount := Count;
  if (Ctxs[F].DupParamPos >= 0) and (Unique or not Simple or Ctxs[F].IsStrict) then
    Err('Duplicate parameter name not allowed in this context', Ctxs[F].DupParamPos);
end;

procedure TPreParser.ParseArrow(Start: Integer; IsAsync: Boolean; Fl: Integer; Single: PStr; SinglePos: Integer);
var Rec, P: Integer;
begin
  Rec := NewRecord(Start);
  Include(Funcs[Rec].Flags, ffArrow);
  if IsAsync then Include(Funcs[Rec].Flags, ffAsync);
  P := CtxTop - 1;
  PushCtx(fkArrow, IsAsync, False);
  Ctxs[F].Rec := Rec;
  Ctxs[F].YieldOp := Ctxs[P].YieldOp;
  Ctxs[F].AwaitOp := IsAsync or Ctxs[P].AwaitOp;
  Ctxs[F].InParams := True;
  PushScope(skFunction);
  if Single <> nil then
  begin
    CheckBinding(Single, SinglePos);
    Declare(Single, dkParam, SinglePos, False);
    Funcs[Rec].ParamCount := 1;
  end
  else ParseFormalParams(True);
  if (S.Kind <> tkArrow) or S.NewlineBefore then Unexpected;
  Ctxs[F].InParams := False;
  Ctxs[F].YieldOp := False;
  Ctxs[F].AwaitOp := IsAsync;
  S.Next(sgRegExp);
  if S.Kind = tkLBrace then
  begin
    ParseFunctionBody;
    Rec := S.TokEnd;
    PopScope;
    FinishFunction(Rec);
    PopCtx;
    S.Next(sgDiv);
  end
  else
  begin
    ParseAssignment(Fl and afNoIn);
    PopScope;
    FinishFunction(S.PrevEnd);
    PopCtx;
  end;
end;

procedure TPreParser.ParseFunction(Start: Integer; IsAsync, IsExpr, DefaultExport: Boolean);
var IsGen, Plain: Boolean; Name: PStr; NamePos, Rec: Integer;
begin
  S.Next(sgDiv);
  IsGen := False;
  if S.Kind = tkStar then
  begin
    IsGen := True;
    S.Next(sgDiv);
  end;
  Name := nil;
  NamePos := -1;
  if S.Kind = tkIdent then
  begin
    if S.EscapedReserved <> tkIdent then ErrHere('Keyword must not contain escaped characters');
    Name := S.Atom;
    NamePos := S.TokStart;
    if not IsExpr then
    begin
      CheckBinding(Name, NamePos);
      case Scopes[ScopeTop - 1].Kind of
        skScript, skFunction: Declare(Name, dkFunction, NamePos, False);
        skModule: Declare(Name, dkLexFunction, NamePos, False);
      else
        Plain := not IsAsync and not IsGen;
        Declare(Name, dkLexFunction, NamePos, Plain);
        if Plain and not Ctxs[F].IsStrict then DeclareVar(Name, NamePos, False, True, DeclTop - 1);
      end;
    end;
    S.Next(sgDiv);
  end
  else if not IsExpr and not DefaultExport then Unexpected;
  Rec := NewRecord(Start);
  if IsAsync then Include(Funcs[Rec].Flags, ffAsync);
  if IsGen then Include(Funcs[Rec].Flags, ffGenerator);
  if IsExpr then Include(Funcs[Rec].Flags, ffExpression) else Include(Funcs[Rec].Flags, ffDeclaration);
  Funcs[Rec].Name := Name;
  PushCtx(fkFunction, IsAsync, IsGen);
  Ctxs[F].Rec := Rec;
  Ctxs[F].Name := Name;
  Ctxs[F].NamePos := NamePos;
  if IsExpr and (Name <> nil) then CheckBinding(Name, NamePos);
  PushScope(skFunction);
  if IsExpr and (Name <> nil) then PushDecl(Name, dkFuncName, NamePos, False);
  Ctxs[F].InParams := True;
  ParseFormalParams(False);
  Ctxs[F].InParams := False;
  ParseFunctionBody;
  Rec := S.TokEnd;
  PopScope;
  FinishFunction(Rec);
  PopCtx;
  if IsExpr then S.Next(sgDiv) else S.Next(sgRegExp);
end;

procedure TPreParser.ParseMethod(Start: Integer; K: TFuncKind; IsAsync, IsGen: Boolean);
var Rec: Integer;
begin
  Rec := NewRecord(Start);
  case K of
    fkGetter: Include(Funcs[Rec].Flags, ffGetter);
    fkSetter: Include(Funcs[Rec].Flags, ffSetter);
    fkClassCtor: Include(Funcs[Rec].Flags, ffClassCtor);
    fkDerivedCtor: Funcs[Rec].Flags := Funcs[Rec].Flags + [ffClassCtor, ffDerivedCtor];
  end;
  Include(Funcs[Rec].Flags, ffMethod);
  if IsAsync then Include(Funcs[Rec].Flags, ffAsync);
  if IsGen then Include(Funcs[Rec].Flags, ffGenerator);
  PushCtx(K, IsAsync, IsGen);
  Ctxs[F].Rec := Rec;
  PushScope(skFunction);
  Ctxs[F].InParams := True;
  case K of
    fkGetter:
      begin
        Expect(tkLParen, sgDiv);
        if S.Kind <> tkRParen then ErrHere('Getter must not have any formal parameters.');
        S.Next(sgDiv);
      end;
    fkSetter:
      begin
        Expect(tkLParen, sgDiv);
        if S.Kind = tkEllipsis then ErrHere('Setter function argument must not be a rest parameter');
        if S.Kind = tkRParen then ErrHere('Setter must have exactly one formal parameter.');
        if S.Kind <> tkIdent then Ctxs[F].SimpleParams := False;
        ParseBindingTarget(dkParam);
        if S.Kind = tkAssign then
        begin
          Ctxs[F].SimpleParams := False;
          S.Next(sgRegExp);
          ParseAssignment(0);
        end;
        if S.Kind <> tkRParen then ErrHere('Setter must have exactly one formal parameter.');
        S.Next(sgDiv);
        if Ctxs[F].DupParamPos >= 0 then
          Err('Duplicate parameter name not allowed in this context', Ctxs[F].DupParamPos);
        Funcs[Rec].ParamCount := 1;
      end;
  else
    ParseFormalParams(True);
  end;
  Ctxs[F].InParams := False;
  ParseFunctionBody;
  Rec := S.TokEnd;
  PopScope;
  FinishFunction(Rec);
  PopCtx;
  S.Next(sgDiv);
end;

{ classes }

procedure TPreParser.DeclarePrivate(A: PStr; Pos: Integer; Kind: Byte; IsSt: Boolean);
var I: Integer;
begin
  for I := PrivClasses[PrivClassTop - 1].NameStart to PrivNameTop - 1 do
    if PrivNames[I].Atom = A then
    begin
      if (Kind in [1, 2]) and (PrivNames[I].Kind in [1, 2]) and (PrivNames[I].Kind <> Kind) and
         (PrivNames[I].IsStatic = IsSt) then
      begin
        PrivNames[I].Kind := 3;
        Exit;
      end;
      Err('Identifier ''#' + NameOf(A) + ''' has already been declared', Pos);
    end;
  if PrivNameTop >= Length(PrivNames) then SetLength(PrivNames, PrivNameTop * 2);
  PrivNames[PrivNameTop].Atom := A;
  PrivNames[PrivNameTop].Pos := Pos;
  PrivNames[PrivNameTop].Kind := Kind;
  PrivNames[PrivNameTop].IsStatic := IsSt;
  Inc(PrivNameTop);
end;

procedure TPreParser.UsePrivate(A: PStr; Pos: Integer);
begin
  if PrivClassTop = 0 then
    Err('Private field ''#' + NameOf(A) + ''' must be declared in an enclosing class', Pos);
  if PrivRefTop >= Length(PrivRefs) then SetLength(PrivRefs, PrivRefTop * 2);
  PrivRefs[PrivRefTop].Atom := A;
  PrivRefs[PrivRefTop].Pos := Pos;
  Inc(PrivRefTop);
end;

procedure TPreParser.PushPrivClass;
begin
  if PrivClassTop >= Length(PrivClasses) then SetLength(PrivClasses, PrivClassTop * 2);
  PrivClasses[PrivClassTop].NameStart := PrivNameTop;
  PrivClasses[PrivClassTop].RefStart := PrivRefTop;
  Inc(PrivClassTop);
end;

{ the class's private references against its private names (§15.7.1 AllPrivateIdentifiersValid); the rest
  belong to the enclosing class }
procedure TPreParser.PopPrivClass;
var CI, I, W: Integer;
begin
  CI := PrivClassTop - 1;
  Inc(FStamp);
  for I := PrivClasses[CI].NameStart to PrivNameTop - 1 do FSeen.Put(PrivNames[I].Atom, FStamp);
  W := PrivClasses[CI].RefStart;
  for I := W to PrivRefTop - 1 do
    if FSeen.Get(PrivRefs[I].Atom) <> FStamp then
    begin
      if CI = 0 then
        Err('Private field ''#' + NameOf(PrivRefs[I].Atom) + ''' must be declared in an enclosing class',
          PrivRefs[I].Pos);
      PrivRefs[W] := PrivRefs[I];
      Inc(W);
    end;
  PrivRefTop := W;
  PrivNameTop := PrivClasses[CI].NameStart;
  Dec(PrivClassTop);
end;

procedure TPreParser.ParseDecorators;
begin
  while S.Kind = tkAt do
  begin
    S.Next(sgRegExp);
    if S.Kind = tkLParen then
    begin
      S.Next(sgRegExp);
      ParseExpression(0);
      Expect(tkRParen, sgDiv);
      Continue;
    end;
    if S.Kind <> tkIdent then Unexpected;
    if S.EscapedReserved <> tkIdent then ErrHere('Keyword must not contain escaped characters');
    CheckIdentRef(S.Atom, S.TokStart);
    AddRef(S.Atom);
    S.Next(sgDiv);
    while S.Kind = tkDot do
    begin
      S.Next(sgDiv);
      if S.Kind = tkPrivateName then UsePrivate(S.Atom, S.TokStart)
      else if not IsIdentName then Unexpected;
      S.Next(sgDiv);
    end;
    if S.Kind = tkLParen then ParseArguments(False);
  end;
end;

procedure TPreParser.ParseClass(Start: Integer; IsExpr, DefaultExport: Boolean);
var OldStrict, Derived: Boolean; Name: PStr; NamePos: Integer; X: TX;
begin
  OldStrict := Ctxs[F].IsStrict;
  Ctxs[F].IsStrict := True;
  S.Next(sgDiv);
  Name := nil;
  NamePos := -1;
  if S.Kind = tkIdent then
  begin
    if S.EscapedReserved <> tkIdent then ErrHere('Keyword must not contain escaped characters');
    Name := S.Atom;
    NamePos := S.TokStart;
    CheckBinding(Name, NamePos);
    if not IsExpr then Declare(Name, dkClass, NamePos, False);
    S.Next(sgDiv);
  end
  else if not IsExpr and not DefaultExport then Unexpected;
  PushScope(skClass);
  if Name <> nil then PushDecl(Name, dkClassName, NamePos, False);
  Derived := False;
  if S.Kind = tkExtends then
  begin
    S.Next(sgRegExp);
    X := ParseLHS(0);
    if X.K = xArrow then ErrHere('Unexpected token: an arrow function is not a class heritage');
    Derived := True;
  end;
  ParseClassBody(Derived);
  PopScope;
  Ctxs[F].IsStrict := OldStrict;
  if IsExpr then S.Next(sgDiv) else S.Next(sgRegExp);
end;

procedure TPreParser.ParseClassBody(Derived: Boolean);
var PStart, KeyPos: Integer; IsStatic, IsAsync, IsGen, IsAccessor, IsComputed, IsPrivate, Named, HasCtor: Boolean;
  Acc: Integer; Name: PStr; K: TFuncKind;
begin
  Expect(tkLBrace, sgDiv);
  PushPrivClass;
  HasCtor := False;
  while S.Kind <> tkRBrace do
  begin
    if S.Kind = tkSemi then
    begin
      S.Next(sgDiv);
      Continue;
    end;
    if S.Kind = tkAt then ParseDecorators;
    PStart := S.TokStart;
    IsStatic := False;
    IsAsync := False;
    IsGen := False;
    IsAccessor := False;
    IsComputed := False;
    IsPrivate := False;
    Named := False;
    Acc := 0;
    Name := nil;
    KeyPos := S.TokStart;
    if IsId(AStatic) then
    begin
      S.Next(sgDiv);
      if S.Kind in [tkLParen, tkAssign, tkSemi, tkRBrace] then
      begin
        Named := True;
        Name := AStatic;
      end
      else if S.Kind = tkLBrace then
      begin
        { ClassStaticBlock }
        PushCtx(fkStaticBlock, False, False);
        PushScope(skFunction);
        S.Next(sgRegExp);
        ParseStatementList(tkRBrace);
        PopScope;
        FinishFunction(0);
        PopCtx;
        S.Next(sgDiv);
        Continue;
      end
      else IsStatic := True;
    end;
    if not Named then
    begin
      KeyPos := S.TokStart;
      if IsId(AAsync) then
      begin
        S.Next(sgDiv);
        if (S.Kind in [tkLParen, tkAssign, tkSemi, tkRBrace]) or S.NewlineBefore then
        begin
          Named := True;
          Name := AAsync;
        end
        else
        begin
          IsAsync := True;
          if S.Kind = tkStar then
          begin
            IsGen := True;
            S.Next(sgDiv);
          end;
        end;
      end
      else if S.Kind = tkStar then
      begin
        IsGen := True;
        S.Next(sgDiv);
      end
      else if IsId(AGet) or IsId(ASet) then
      begin
        Name := S.Atom;
        S.Next(sgDiv);
        if S.Kind in [tkLParen, tkAssign, tkSemi, tkRBrace] then Named := True
        else if (S.Kind = tkStar) and S.NewlineBefore then Named := True
        else if Name = AGet then Acc := 1
        else Acc := 2;
      end
      else if IsId(AAccessor) then
      begin
        S.Next(sgDiv);
        if (S.Kind in [tkLParen, tkAssign, tkSemi, tkRBrace]) or S.NewlineBefore then
        begin
          Named := True;
          Name := AAccessor;
        end
        else IsAccessor := True;
      end;
    end;
    if not Named then
    begin
      KeyPos := S.TokStart;
      if S.Kind = tkPrivateName then
      begin
        IsPrivate := True;
        Name := S.Atom;
        S.Next(sgDiv);
      end
      else ParsePropertyName(Name, IsComputed);
    end;
    if (S.Kind = tkLParen) and not IsAccessor then
    begin
      K := fkMethod;
      if Acc = 1 then K := fkGetter else if Acc = 2 then K := fkSetter;
      if IsPrivate then
      begin
        if Name = AConstructor then Err('Classes may not have a private field named ''#constructor''', KeyPos);
        DeclarePrivate(Name, KeyPos, Acc, IsStatic);
      end
      else if not IsComputed then
      begin
        if not IsStatic and (Name = AConstructor) then
        begin
          if (Acc <> 0) or IsGen or IsAsync then
            Err('Class constructor may not be an accessor, a generator or async', KeyPos);
          if HasCtor then Err('A class may only have one constructor', KeyPos);
          HasCtor := True;
          if Derived then K := fkDerivedCtor else K := fkClassCtor;
        end;
        if IsStatic and (Name = APrototype) then
          Err('Classes may not have a static property named ''prototype''', KeyPos);
      end;
      ParseMethod(PStart, K, IsAsync, IsGen);
    end
    else
    begin
      if IsAsync or IsGen or (Acc <> 0) then Unexpected;
      if IsPrivate then
      begin
        if Name = AConstructor then Err('Classes may not have a private field named ''#constructor''', KeyPos);
        DeclarePrivate(Name, KeyPos, 0, IsStatic);
      end
      else if not IsComputed then
      begin
        if Name = AConstructor then Err('Classes may not have a field named ''constructor''', KeyPos);
        if IsStatic and (Name = APrototype) then
          Err('Classes may not have a static property named ''prototype''', KeyPos);
      end;
      if S.Kind = tkAssign then
      begin
        PushCtx(fkFieldInit, False, False);
        PushScope(skFunction);
        S.Next(sgRegExp);
        ParseAssignment(0);
        PopScope;
        FinishFunction(0);
        PopCtx;
      end;
      if S.Kind = tkSemi then S.Next(sgDiv)
      else if (S.Kind <> tkRBrace) and not S.NewlineBefore then Unexpected;
    end;
  end;
  PopPrivClass;
end;

{ statements }

procedure TPreParser.ParseStatementList(EndTok: TTok);
begin
  while S.Kind <> EndTok do
  begin
    if S.Kind = tkEOF then Unexpected;
    ParseStatement(scList);
  end;
end;

procedure TPreParser.ParseBlock(G: TScanGoal);
begin
  Expect(tkLBrace, sgRegExp);
  PushScope(skBlock);
  ParseStatementList(tkRBrace);
  PopScope;
  S.Next(G);
end;

{ at let: whether it begins a LexicalDeclaration (the let is then consumed) }
function TPreParser.AtLetDeclaration(Ctx: TStmtCtx): Boolean;
var M: TMark;
begin
  M := Mark;
  S.Next(sgDiv);
  if S.Kind = tkLBracket then Exit(True);
  if (S.Kind = tkLBrace) or (S.Kind = tkIdent) then
    if (Ctx = scList) or not S.NewlineBefore then Exit(True);
  Reset(M);
  Result := False;
end;

{ at using or await: whether a UsingDeclaration / AwaitUsingDeclaration begins (consumed up to its first
  binding identifier) }
function TPreParser.AtUsingDeclaration(out IsAwait: Boolean): Boolean;
var M: TMark;
begin
  M := Mark;
  IsAwait := IsId(AAwait);
  S.Next(sgDiv);
  if IsAwait then
  begin
    if not IsId(AUsing) or S.NewlineBefore then
    begin
      Reset(M);
      Exit(False);
    end;
    S.Next(sgDiv);
  end;
  if (S.Kind = tkIdent) and not S.NewlineBefore then Exit(True);
  Reset(M);
  Result := False;
end;

procedure TPreParser.ParseUsing(IsAwait: Boolean);
begin
  case Scopes[ScopeTop - 1].Kind of
    skScript: ErrHere('using declarations are not allowed at the top level of a script');
    skSwitch: ErrHere('using declarations are not allowed directly in a switch case clause');
  end;
  while True do
  begin
    BindingIdent(dkUsing);
    if S.Kind <> tkAssign then ErrHere('Missing initializer in using declaration');
    S.Next(sgRegExp);
    ParseAssignment(0);
    if S.Kind <> tkComma then Break;
    S.Next(sgDiv);
  end;
  Semi;
end;

procedure TPreParser.ParseVarDecls(K: TDeclKind; ForHead: Boolean; out Count, MissingInit: Integer;
  out HasInit, IsPattern: Boolean);
var Pat: Boolean; P: Integer;
begin
  Count := 0;
  MissingInit := -1;
  HasInit := False;
  IsPattern := False;
  while True do
  begin
    P := S.TokStart;
    Pat := (S.Kind = tkLBracket) or (S.Kind = tkLBrace);
    if Count = 0 then IsPattern := Pat;
    ParseBindingTarget(K);
    Inc(Count);
    if S.Kind = tkAssign then
    begin
      HasInit := True;
      S.Next(sgRegExp);
      if ForHead then ParseAssignment(afNoIn) else ParseAssignment(0);
    end
    else if ((K = dkConst) or (K = dkUsing) or Pat) and (MissingInit < 0) then MissingInit := P;
    if S.Kind <> tkComma then Break;
    S.Next(sgDiv);
  end;
  if not ForHead and (MissingInit >= 0) then Err('Missing initializer in declaration', MissingInit);
end;

procedure TPreParser.ParseLexical(K: TDeclKind);
var Count, Missing: Integer; HasInit, IsPattern: Boolean;
begin
  ParseVarDecls(K, False, Count, Missing, HasInit, IsPattern);
  Semi;
end;

procedure TPreParser.ParseIf;
begin
  S.Next(sgDiv);
  Expect(tkLParen, sgRegExp);
  ParseExpression(0);
  Expect(tkRParen, sgRegExp);
  ParseStatement(scIf);
  if S.Kind = tkElse then
  begin
    S.Next(sgRegExp);
    ParseStatement(scIf);
  end;
end;

procedure TPreParser.ParseWhile;
begin
  S.Next(sgDiv);
  Expect(tkLParen, sgRegExp);
  ParseExpression(0);
  Expect(tkRParen, sgRegExp);
  ParseStatement(scLoop);
end;

procedure TPreParser.ParseDoWhile;
begin
  S.Next(sgRegExp);
  ParseStatement(scLoop);
  if S.Kind <> tkWhile then Unexpected;
  S.Next(sgDiv);
  Expect(tkLParen, sgRegExp);
  ParseExpression(0);
  Expect(tkRParen, sgRegExp);
  if S.Kind = tkSemi then S.Next(sgRegExp);
end;

procedure TPreParser.ParseFor;
var IsAwait, IsOf, Decl, IsPattern, HasInit, StartsLet, StartsAsync: Boolean; K: TDeclKind;
  VarStart, Count, Missing, Saved, Pos, I: Integer; M, M2: TMark; X: TX; UsingAwait: Boolean;
begin
  S.Next(sgDiv);
  IsAwait := False;
  if IsId(AAwait) and Ctxs[F].AwaitOp then
  begin
    if Ctxs[F].InParams then ErrHere('Illegal await-expression in formal parameters of async function');
    IsAwait := True;
    S.Next(sgDiv);
  end;
  Expect(tkLParen, sgRegExp);
  PushScope(skFor);
  VarStart := VarTop;
  Decl := False;
  K := dkVar;
  Count := 0;
  Missing := -1;
  HasInit := False;
  IsPattern := False;
  IsOf := False;
  if S.Kind = tkSemi then
  else
  begin
    if S.Kind = tkVar then
    begin
      Decl := True;
      K := dkVar;
      S.Next(sgDiv);
    end
    else if S.Kind = tkConst then
    begin
      Decl := True;
      K := dkConst;
      S.Next(sgDiv);
    end
    else if IsId(ALet) and AtLetDeclaration(scList) then
    begin
      Decl := True;
      K := dkLet;
    end
    else if IsId(AUsing) or (IsId(AAwait) and Ctxs[F].AwaitOp) then
    begin
      M := Mark;
      if AtUsingDeclaration(UsingAwait) then
      begin
        { for (using of ...): using is the LeftHandSideExpression, unless = follows (for (using of = x;;)) }
        IsOf := False;
        if IsId(AOf) and not UsingAwait then
        begin
          M2 := Mark;
          S.Next(sgDiv);
          IsOf := S.Kind <> tkAssign;
          Reset(M2);
        end;
        if IsOf then Reset(M)
        else
        begin
          Decl := True;
          K := dkUsing;
        end;
      end;
    end;
    if Decl then
      ParseVarDecls(K, True, Count, Missing, HasInit, IsPattern)
    else
    begin
      M := Mark;
      Pos := S.TokStart;
      StartsLet := IsId(ALet);
      StartsAsync := IsId(AAsync);
      Saved := FCoverErr;
      FCoverErr := -1;
      X := ParseExpression(afNoIn or afAllowCover);
      if (S.Kind = tkIn) or IsId(AOf) then
      begin
        IsOf := S.Kind <> tkIn;
        if IsOf and StartsLet then Err('The left-hand side of a for-of loop may not be ''let''.', Pos);
        if IsOf and not IsAwait and StartsAsync and (X.K = xIdent) and not X.Paren and (S.PrevEnd - Pos = 5) then
          Err('The left-hand side of a for-of loop may not be ''async''.', Pos);
        if (X.K in [xObject, xArray]) and not X.Paren then
        begin
          Reset(M);
          FCoverErr := -1;
          ParseAssignPattern;
        end
        else
        begin
          if FCoverErr >= 0 then Err(FCoverMsg, FCoverErr);
          if X.K = xArrow then Err('Invalid left-hand side in for-loop', Pos);
          CheckSimpleTarget(X, tgForInOf, Pos);
        end;
        FCoverErr := Saved;
        Decl := False;
        Count := -1;
      end
      else
      begin
        if FCoverErr >= 0 then Err(FCoverMsg, FCoverErr);
        FCoverErr := Saved;
      end;
    end;
  end;
  if (S.Kind = tkIn) or IsId(AOf) then
  begin
    IsOf := S.Kind <> tkIn;
    if Decl then
    begin
      if Count > 1 then ErrHere('Invalid left-hand side in for-in loop: Must have a single binding.');
      if HasInit and (IsOf or (K <> dkVar) or Ctxs[F].IsStrict or IsPattern) then
        ErrHere('for-in loop variable declaration may not have an initializer.');
      if (K = dkUsing) and not IsOf then ErrHere('using declarations are not allowed in for-in loops');
      if IsOf then
        for I := VarStart to VarTop - 1 do VarLog[I].ForOf := True;
    end
    else if Count = 0 then Unexpected;
    if IsAwait and not IsOf then Unexpected;
    S.Next(sgRegExp);
    if IsOf then ParseAssignment(0) else ParseExpression(0);
    Expect(tkRParen, sgRegExp);
  end
  else
  begin
    if IsAwait then Unexpected;
    if Missing >= 0 then Err('Missing initializer in declaration', Missing);
    Expect(tkSemi, sgRegExp);
    if S.Kind <> tkSemi then ParseExpression(0);
    Expect(tkSemi, sgRegExp);
    if S.Kind <> tkRParen then ParseExpression(0);
    Expect(tkRParen, sgRegExp);
  end;
  ParseStatement(scLoop);
  PopScope;
end;

procedure TPreParser.ParseBreakContinue(IsBreak: Boolean);
var I: Integer; Found: Boolean;
begin
  S.Next(sgDiv);
  if (S.Kind = tkIdent) and not S.NewlineBefore then
  begin
    if S.EscapedReserved <> tkIdent then ErrHere('Keyword must not contain escaped characters');
    Found := False;
    for I := LabelTop - 1 downto Ctxs[F].LabelBase do
      if Labels[I].Atom = S.Atom then
      begin
        if not IsBreak and not Labels[I].Loop then
          ErrHere('Illegal continue statement: ''' + NameOf(S.Atom) + ''' does not denote an iteration statement');
        Found := True;
        Break;
      end;
    if not Found then ErrHere('Undefined label ''' + NameOf(S.Atom) + '''');
    S.Next(sgDiv);
  end
  else if IsBreak then
  begin
    if Ctxs[F].Breakable = 0 then ErrHere('Illegal break statement');
  end
  else if Ctxs[F].Loops = 0 then ErrHere('Illegal continue statement: no surrounding iteration statement');
  Semi;
end;

procedure TPreParser.ParseReturn;
begin
  if Ctxs[F].Kind in [fkScript, fkModule, fkFieldInit, fkStaticBlock] then ErrHere('Illegal return statement');
  S.Next(sgRegExp);
  if not S.NewlineBefore and not (S.Kind in [tkSemi, tkRBrace, tkEOF]) then ParseExpression(0);
  Semi;
end;

procedure TPreParser.ParseWith;
begin
  if Ctxs[F].IsStrict then ErrHere('Strict mode code may not include a with statement');
  S.Next(sgDiv);
  Expect(tkLParen, sgRegExp);
  ParseExpression(0);
  Expect(tkRParen, sgRegExp);
  ParseStatement(scOther);
end;

procedure TPreParser.ParseSwitch;
var HasDefault: Boolean;
begin
  S.Next(sgDiv);
  Expect(tkLParen, sgRegExp);
  ParseExpression(0);
  Expect(tkRParen, sgDiv);
  Expect(tkLBrace, sgRegExp);
  PushScope(skSwitch);
  Inc(Ctxs[F].Breakable);
  HasDefault := False;
  while S.Kind <> tkRBrace do
  begin
    if S.Kind = tkCase then
    begin
      S.Next(sgRegExp);
      ParseExpression(0);
      Expect(tkColon, sgRegExp);
    end
    else if S.Kind = tkDefault then
    begin
      if HasDefault then ErrHere('More than one default clause in switch statement');
      HasDefault := True;
      S.Next(sgDiv);
      Expect(tkColon, sgRegExp);
    end
    else Unexpected;
    while not (S.Kind in [tkCase, tkDefault, tkRBrace]) do
    begin
      if S.Kind = tkEOF then Unexpected;
      ParseStatement(scList);
    end;
  end;
  Dec(Ctxs[F].Breakable);
  PopScope;
  S.Next(sgRegExp);
end;

procedure TPreParser.ParseThrow;
begin
  S.Next(sgRegExp);
  if S.NewlineBefore then ErrHere('Illegal newline after throw');
  ParseExpression(0);
  Semi;
end;

procedure TPreParser.ParseTry;
var HasCatch: Boolean;
begin
  S.Next(sgDiv);
  ParseBlock(sgDiv);
  HasCatch := False;
  if S.Kind = tkCatch then
  begin
    HasCatch := True;
    S.Next(sgDiv);
    PushScope(skCatch);
    if S.Kind = tkLParen then
    begin
      S.Next(sgDiv);
      if S.Kind = tkIdent then BindingIdent(dkCatchParam) else ParseBindingTarget(dkCatchPattern);
      Expect(tkRParen, sgDiv);
    end;
    Expect(tkLBrace, sgRegExp);
    ParseStatementList(tkRBrace);
    PopScope;
    S.Next(sgRegExp);
  end;
  if S.Kind = tkFinally then
  begin
    S.Next(sgDiv);
    ParseBlock(sgRegExp);
  end
  else if not HasCatch then ErrHere('Missing catch or finally after try');
end;

procedure TPreParser.ParseLabelledOrExpression(Ctx: TStmtCtx; Pending: Integer);
var X: TX; I: Integer; A: PStr; M: TMark; IsLabel: Boolean;
begin
  IsLabel := False;
  if IsId(AArguments) and Ctxs[F].ArgsBanned then
  begin
    { a label named arguments is no IdentifierReference (ContainsArguments) }
    M := Mark;
    S.Next(sgDiv);
    IsLabel := S.Kind = tkColon;
    Reset(M);
    if IsLabel then
    begin
      X := MakeX(xIdent);
      X.A := AArguments;
      S.Next(sgDiv);
    end;
  end;
  if not IsLabel then X := ParseExpression(0);
  if (X.K = xIdent) and not X.Paren and (S.Kind = tkColon) then
  begin
    A := X.A;
    if not IsLabel and (RefTop > 0) and (Refs[RefTop - 1].Atom = A) then Dec(RefTop);
    for I := Ctxs[F].LabelBase to LabelTop - 1 do
      if Labels[I].Atom = A then ErrHere('Label ''' + NameOf(A) + ''' has already been declared');
    if LabelTop >= Length(Labels) then SetLength(Labels, LabelTop * 2);
    Labels[LabelTop].Atom := A;
    Labels[LabelTop].Loop := False;
    Inc(LabelTop);
    S.Next(sgRegExp);
    FPendingLabels := Pending + 1;
    if Ctx in [scList, scLabel] then ParseStatement(scLabel) else ParseStatement(scOther);
    Dec(LabelTop);
    Exit;
  end;
  Semi;
end;

procedure TPreParser.ParseExpressionStatement;
begin
  ParseExpression(0);
  Semi;
end;

procedure TPreParser.ParseStatement(Ctx: TStmtCtx);
var Pending, I, Start: Integer; IsAwait, IsGen: Boolean; M: TMark; K: TTok;
begin
  Pending := FPendingLabels;
  FPendingLabels := 0;
  K := S.Kind;
  case K of
    tkLBrace: ParseBlock(sgRegExp);
    tkVar:
      begin
        S.Next(sgDiv);
        ParseLexical(dkVar);
      end;
    tkSemi: S.Next(sgRegExp);
    tkIf: ParseIf;
    tkFor, tkWhile, tkDo:
      begin
        for I := LabelTop - Pending to LabelTop - 1 do Labels[I].Loop := True;
        Inc(Ctxs[F].Breakable);
        Inc(Ctxs[F].Loops);
        if K = tkFor then ParseFor else if K = tkWhile then ParseWhile else ParseDoWhile;
        Dec(Ctxs[F].Breakable);
        Dec(Ctxs[F].Loops);
      end;
    tkContinue: ParseBreakContinue(False);
    tkBreak: ParseBreakContinue(True);
    tkReturn: ParseReturn;
    tkWith: ParseWith;
    tkSwitch: ParseSwitch;
    tkThrow: ParseThrow;
    tkTry: ParseTry;
    tkDebugger:
      begin
        S.Next(sgDiv);
        Semi;
      end;
    tkFunction:
      if Ctx = scList then ParseFunction(S.TokStart, False, False, False)
      else if (Ctx in [scIf, scLabel]) and not Ctxs[F].IsStrict then
      begin
        { B.3.3 / B.3.2: a plain function as an if clause or a labelled item in sloppy code }
        M := Mark;
        S.Next(sgDiv);
        IsGen := S.Kind = tkStar;
        Reset(M);
        if IsGen then ErrHere('Generators can only be declared at the top level or inside a block.');
        if Ctx = scIf then
        begin
          PushScope(skBlock);
          ParseFunction(S.TokStart, False, False, False);
          PopScope;
        end
        else ParseFunction(S.TokStart, False, False, False);
      end
      else if Ctxs[F].IsStrict then
        ErrHere('In strict mode code, functions can only be declared at top level or inside a block.')
      else ErrHere('In non-strict mode code, functions can only be declared at top level, inside a block, or as the body of an if statement.');
    tkClass:
      begin
        if Ctx <> scList then Unexpected;
        ParseClass(S.TokStart, False, False);
      end;
    tkConst:
      begin
        if Ctx <> scList then ErrHere('Lexical declaration cannot appear in a single-statement context');
        S.Next(sgDiv);
        ParseLexical(dkConst);
      end;
    tkAt:
      begin
        if Ctx <> scList then Unexpected;
        Start := S.TokStart;
        ParseDecorators;
        if (S.Kind = tkExport) and (Scopes[ScopeTop - 1].Kind = skModule) then ParseExportDecl
        else
        begin
          if S.Kind <> tkClass then Unexpected;
          ParseClass(Start, False, False);
        end;
      end;
    tkImport:
      begin
        M := Mark;
        S.Next(sgDiv);
        IsGen := (S.Kind = tkLParen) or (S.Kind = tkDot);
        Reset(M);
        if IsGen then ParseExpressionStatement
        else if (Ctx = scList) and (Scopes[ScopeTop - 1].Kind = skModule) then ParseImportDecl
        else if FGoal = pgModule then ErrHere('An import declaration may only appear at the top level of a module')
        else ErrHere('Cannot use import statement outside a module');
      end;
    tkExport:
      if (Ctx = scList) and (Scopes[ScopeTop - 1].Kind = skModule) then ParseExportDecl
      else if FGoal = pgModule then ErrHere('An export declaration may only appear at the top level of a module')
      else ErrHere('Unexpected token ''export''');
    tkIdent:
      if IsId(ALet) and AtLetDeclaration(Ctx) then
      begin
        if Ctx <> scList then ErrHere('Lexical declaration cannot appear in a single-statement context');
        ParseLexical(dkLet);
      end
      else if IsId(AAsync) then
      begin
        Start := S.TokStart;
        M := Mark;
        S.Next(sgDiv);
        if (S.Kind = tkFunction) and not S.NewlineBefore then
        begin
          if Ctx <> scList then Err('Async functions can only be declared at the top level or inside a block.', Start);
          ParseFunction(Start, True, False, False);
        end
        else
        begin
          Reset(M);
          ParseLabelledOrExpression(Ctx, Pending);
        end;
      end
      else if (IsId(AUsing) or (IsId(AAwait) and Ctxs[F].AwaitOp)) and AtUsingDeclaration(IsAwait) then
      begin
        if Ctx <> scList then ErrHere('Lexical declaration cannot appear in a single-statement context');
        ParseUsing(IsAwait);
      end
      else ParseLabelledOrExpression(Ctx, Pending);
  else
    ParseExpressionStatement;
  end;
end;

{ modules }

procedure TPreParser.ParseModuleSpecifier;
begin
  if S.Kind <> tkString then Unexpected;
  S.Next(sgDiv);
end;

procedure TPreParser.ParseWithClause;
var Keys: array of PStr; N, I: Integer; A: PStr;
begin
  if S.Kind <> tkWith then Exit;
  S.Next(sgDiv);
  Expect(tkLBrace, sgDiv);
  N := 0;
  SetLength(Keys, 4);
  while S.Kind <> tkRBrace do
  begin
    if S.Kind = tkString then A := AtomOfChars(PWideChar(S.Buf), S.BufLen)
    else if IsIdentName then A := S.TokAtom
    else Unexpected;
    for I := 0 to N - 1 do
      if Keys[I] = A then ErrHere('Import attribute has duplicate key ''' + NameOf(A) + '''');
    if N >= Length(Keys) then SetLength(Keys, N * 2);
    Keys[N] := A;
    Inc(N);
    S.Next(sgDiv);
    Expect(tkColon, sgDiv);
    if S.Kind <> tkString then Unexpected;
    S.Next(sgDiv);
    if S.Kind = tkComma then S.Next(sgDiv)
    else if S.Kind <> tkRBrace then Unexpected;
  end;
  S.Next(sgDiv);
end;

function WellFormed(P: PWideChar; N: Integer): Boolean;
var I: Integer;
begin
  I := 0;
  while I < N do
  begin
    if (Ord(P[I]) >= $D800) and (Ord(P[I]) <= $DBFF) then
    begin
      if (I + 1 >= N) or (Ord(P[I + 1]) < $DC00) or (Ord(P[I + 1]) > $DFFF) then Exit(False);
      Inc(I, 2);
    end
    else if (Ord(P[I]) >= $DC00) and (Ord(P[I]) <= $DFFF) then Exit(False)
    else Inc(I);
  end;
  Result := True;
end;

{ a ModuleExportName (IdentifierName or StringLiteral), consumed }
function TPreParser.ParseModuleExportName(out IsString: Boolean): PStr;
begin
  IsString := S.Kind = tkString;
  if IsString then
  begin
    if not WellFormed(PWideChar(S.Buf), S.BufLen) then ErrHere('Invalid module export name: contains unpaired surrogate');
    Result := AtomOfChars(PWideChar(S.Buf), S.BufLen);
  end
  else if IsIdentName then Result := S.TokAtom
  else Unexpected;
  S.Next(sgDiv);
end;

procedure TPreParser.AddExport(A: PStr; Pos: Integer);
begin
  if FModuleExports.Get(A) >= 0 then Err('Duplicate export of ''' + NameOf(A) + '''', Pos);
  FModuleExports.Put(A, 1);
end;

procedure TPreParser.ParseImportDecl;
var M, M2: TMark; IsDefer, IsSource, IsString: Boolean; A: PStr; Pos: Integer;
begin
  S.Next(sgDiv);
  if S.Kind = tkString then
  begin
    ParseModuleSpecifier;
    ParseWithClause;
    Semi;
    Exit;
  end;
  IsDefer := False;
  IsSource := False;
  if IsId(ADefer) then
  begin
    M := Mark;
    S.Next(sgDiv);
    if S.Kind = tkStar then IsDefer := True else Reset(M);
  end
  else if IsId(ASource) then
  begin
    M := Mark;
    S.Next(sgDiv);
    if S.Kind = tkIdent then
    begin
      if IsId(AFrom) then
      begin
        M2 := Mark;
        S.Next(sgDiv);
        IsSource := IsId(AFrom);
        Reset(M2);
      end
      else IsSource := True;
    end;
    if not IsSource then Reset(M);
  end;
  if IsSource then BindingIdent(dkImport)
  else if IsDefer then
  begin
    S.Next(sgDiv);
    if not IsId(AAs) then Unexpected;
    S.Next(sgDiv);
    BindingIdent(dkImport);
  end
  else
  begin
    if S.Kind = tkIdent then
    begin
      BindingIdent(dkImport);
      if S.Kind = tkComma then
      begin
        S.Next(sgDiv);
        if (S.Kind <> tkStar) and (S.Kind <> tkLBrace) then Unexpected;
      end;
    end;
    if S.Kind = tkStar then
    begin
      S.Next(sgDiv);
      if not IsId(AAs) then Unexpected;
      S.Next(sgDiv);
      BindingIdent(dkImport);
    end
    else if S.Kind = tkLBrace then
    begin
      S.Next(sgDiv);
      while S.Kind <> tkRBrace do
      begin
        if (S.Kind = tkIdent) and (S.EscapedReserved = tkIdent) then
        begin
          (* import x, or import x as y, between braces *)
          M := Mark;
          S.Next(sgDiv);
          if IsId(AAs) then
          begin
            S.Next(sgDiv);
            BindingIdent(dkImport);
          end
          else
          begin
            Reset(M);
            BindingIdent(dkImport);
          end;
        end
        else
        begin
          Pos := S.TokStart;
          A := ParseModuleExportName(IsString);
          if not IsId(AAs) then Err('Unexpected token', Pos);
          S.Next(sgDiv);
          BindingIdent(dkImport);
        end;
        if S.Kind = tkComma then S.Next(sgDiv)
        else if S.Kind <> tkRBrace then Unexpected;
      end;
      S.Next(sgDiv);
    end;
  end;
  if not IsId(AFrom) then Unexpected;
  S.Next(sgDiv);
  ParseModuleSpecifier;
  ParseWithClause;
  Semi;
end;

procedure TPreParser.ParseExportDecl;
var IsString: Boolean; A, L: PStr; Pos, D0, V0, I, Start, Count, Missing: Integer; HasInit, IsPattern: Boolean;
  M: TMark; LocalStart: Integer; LocalAtoms: array of PStr; LocalPos: array of Integer; LocalBad: array of Integer;
  N: Integer;
begin
  S.Next(sgRegExp);
  case S.Kind of
    tkStar:
      begin
        S.Next(sgDiv);
        if IsId(AAs) then
        begin
          S.Next(sgDiv);
          Pos := S.TokStart;
          AddExport(ParseModuleExportName(IsString), Pos);
        end;
        if not IsId(AFrom) then Unexpected;
        S.Next(sgDiv);
        ParseModuleSpecifier;
        ParseWithClause;
        Semi;
      end;
    tkLBrace:
      begin
        S.Next(sgDiv);
        N := 0;
        SetLength(LocalAtoms, 8);
        SetLength(LocalPos, 8);
        SetLength(LocalBad, 8);
        while S.Kind <> tkRBrace do
        begin
          Pos := S.TokStart;
          if N >= Length(LocalAtoms) then
          begin
            SetLength(LocalAtoms, N * 2);
            SetLength(LocalPos, N * 2);
            SetLength(LocalBad, N * 2);
          end;
          LocalBad[N] := 0;
          if S.Kind = tkString then LocalBad[N] := 1
          else if (S.Kind <> tkIdent) or (S.EscapedReserved <> tkIdent) then LocalBad[N] := 2
          else if SpecOf(S.Atom) in [spLet, spStrictReserved, spAwait] then LocalBad[N] := 2;
          L := ParseModuleExportName(IsString);
          LocalAtoms[N] := L;
          LocalPos[N] := Pos;
          Inc(N);
          if IsId(AAs) then
          begin
            S.Next(sgDiv);
            Pos := S.TokStart;
            A := ParseModuleExportName(IsString);
          end
          else A := L;
          AddExport(A, Pos);
          if S.Kind = tkComma then S.Next(sgDiv)
          else if S.Kind <> tkRBrace then Unexpected;
        end;
        S.Next(sgDiv);
        if IsId(AFrom) then
        begin
          S.Next(sgDiv);
          ParseModuleSpecifier;
          ParseWithClause;
        end
        else
          for I := 0 to N - 1 do
          begin
            if LocalBad[I] = 1 then Err('Unexpected string', LocalPos[I]);
            if LocalBad[I] = 2 then Err('Unexpected reserved word', LocalPos[I]);
            if FExportLocalTop >= Length(FExportLocals) then SetLength(FExportLocals, FExportLocalTop * 2 + 8);
            FExportLocals[FExportLocalTop].Atom := LocalAtoms[I];
            FExportLocals[FExportLocalTop].Pos := LocalPos[I];
            Inc(FExportLocalTop);
          end;
        Semi;
      end;
    tkVar, tkConst:
      begin
        V0 := VarTop;
        D0 := DeclTop;
        if S.Kind = tkVar then
        begin
          S.Next(sgDiv);
          ParseLexical(dkVar);
        end
        else
        begin
          S.Next(sgDiv);
          ParseLexical(dkConst);
        end;
        for I := V0 to VarTop - 1 do AddExport(VarLog[I].Atom, VarLog[I].Pos);
        for I := D0 to DeclTop - 1 do AddExport(Decls[I].Atom, Decls[I].Pos);
      end;
    tkFunction, tkClass, tkAt:
      begin
        D0 := DeclTop;
        Start := S.TokStart;
        if S.Kind = tkFunction then ParseFunction(Start, False, False, False)
        else
        begin
          if S.Kind = tkAt then ParseDecorators;
          if S.Kind <> tkClass then Unexpected;
          ParseClass(Start, False, False);
        end;
        AddExport(Decls[D0].Atom, Decls[D0].Pos);
      end;
    tkDefault:
      begin
        AddExport(ADefault, S.TokStart);
        S.Next(sgRegExp);
        Start := S.TokStart;
        if S.Kind = tkFunction then ParseFunction(Start, False, False, True)
        else if (S.Kind = tkClass) or (S.Kind = tkAt) then
        begin
          if S.Kind = tkAt then ParseDecorators;
          if S.Kind <> tkClass then Unexpected;
          ParseClass(Start, False, True);
        end
        else
        begin
          if IsId(AAsync) then
          begin
            M := Mark;
            S.Next(sgDiv);
            if (S.Kind = tkFunction) and not S.NewlineBefore then
            begin
              ParseFunction(Start, True, False, True);
              Exit;
            end;
            Reset(M);
          end;
          ParseAssignment(0);
          Semi;
        end;
      end;
    tkIdent:
      if IsId(ALet) then
      begin
        D0 := DeclTop;
        S.Next(sgDiv);
        ParseLexical(dkLet);
        for I := D0 to DeclTop - 1 do AddExport(Decls[I].Atom, Decls[I].Pos);
      end
      else if IsId(AAsync) then
      begin
        Start := S.TokStart;
        S.Next(sgDiv);
        if (S.Kind <> tkFunction) or S.NewlineBefore then Unexpected;
        D0 := DeclTop;
        ParseFunction(Start, True, False, False);
        AddExport(Decls[D0].Atom, Decls[D0].Pos);
      end
      else Unexpected;
  else
    Unexpected;
  end;
end;

procedure TPreParser.ParseProgram;
var I, D, SI: Integer;
begin
  S.Next(sgRegExp);
  if FGoal = pgModule then PushScope(skModule) else PushScope(skScript);
  if FGoal = pgScript then ParseDirectives;
  ParseStatementList(tkEOF);
  if FExportLocalTop > 0 then
  begin
    SI := ScopeTop - 1;
    Inc(FStamp);
    for I := Scopes[SI].VarStart to VarTop - 1 do FSeen.Put(VarLog[I].Atom, FStamp);
    for I := 0 to FExportLocalTop - 1 do
    begin
      D := FMap.Get(FExportLocals[I].Atom);
      if ((D < 0) or (Decls[D].Scope <> SI)) and (FSeen.Get(FExportLocals[I].Atom) <> FStamp) then
        Err('Export ''' + NameOf(FExportLocals[I].Atom) + ''' is not defined in module', FExportLocals[I].Pos);
    end;
  end;
  PopScope;
end;

function PreParseSource(const Src: UnicodeString; Goal: TParseGoal; Strict: Boolean; out Msg: string;
  out Line, Col: Integer): Boolean;
var P: TPreParser; I, LS: Integer;
begin
  Result := True;
  Msg := '';
  Line := 0;
  Col := 0;
  P := TPreParser.Create(PWideChar(Src), Length(Src), Goal, Strict);
  try
    try
      P.ParseProgram;
    except
      on E: EScanError do
      begin
        Result := False;
        Msg := E.Message;
        Line := 1;
        LS := 0;
        for I := 0 to Min(E.Pos, Length(Src)) - 1 do
          if (Src[I + 1] = #10) or (Src[I + 1] = #$2028) or (Src[I + 1] = #$2029) or
             ((Src[I + 1] = #13) and ((I + 1 >= Length(Src)) or (Src[I + 2] <> #10))) then
          begin
            Inc(Line);
            LS := I + 1;
          end;
        Col := E.Pos - LS + 1;
      end;
    end;
  finally
    P.Free;
  end;
end;

end.
