unit Goccia.Builtins.AST;

{$I Goccia.inc}

{ The `goccia:ast` namespace: one `parse` function that turns source text into
  a JavaScript-visible tree.

  What is exposed and why is [ADR 0117](../../docs/adr/0117-javascript-visible-ast-module.md)
  for the statement view and [ADR 0162](../../docs/adr/0162-the-parse-tree-reaches-expressions-on-request-through-one-child-enumeration.md)
  for the expression view `expressions: true` adds. By default the tree holds
  statements, the three kinds that own a statement list (`Program`,
  `BlockStatement`, `SwitchCase`), their offsets and positions, and comment
  trivia. It never holds names or literal values — a rule that needs those
  has the offsets and the source text the result carries.

  The tree is walked only through TGocciaASTNode.ForEachChild. Expressions are
  always walked, because that is the only way to reach the block of an arrow
  function assigned to a `const`; without the option they are walked without
  being emitted.

  With `tokens: true` the result also carries the committed token stream,
  comments included, as typed ranges; see
  [ADR 0163](../../docs/adr/0163-goccia-ast-reports-the-committed-token-stream.md).
  A type-level declaration is reported by the kind it was written as, and
  `typeOnly` marks the module statements that carry no code; see
  [ADR 0161](../../docs/adr/0161-goccia-ast-reports-type-level-declarations-by-kind.md). }

interface

uses
  Goccia.Values.ObjectValue;

function CreateASTNamespace: TGocciaObjectValue;

implementation

uses
  Classes,
  Generics.Collections,
  SysUtils,

  TextSemantics,

  Goccia.Arguments.Collection,
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Constants.ErrorNames,
  Goccia.Error,
  Goccia.GarbageCollector,
  Goccia.JSX.Transformer,
  Goccia.Keywords.Contextual,
  Goccia.Keywords.Reserved,
  Goccia.Lexer,
  Goccia.NestingLimit,
  Goccia.ObjectModel,
  Goccia.OriginMap,
  Goccia.SourcePipeline,
  Goccia.SourceSpan,
  Goccia.ThreadCleanupRegistry,
  Goccia.Token,
  Goccia.Utils.Arrays,
  Goccia.Values.ArrayValue,
  Goccia.Values.BigIntValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.Primitives;

const
  { The walker recurses once per level of the tree. The parser has already
    bounded the tree's height, and recursed deeper than this to build it, so
    past this the tree is truncated rather than the worker's stack. }
  MAX_WALK_DEPTH = MAX_TREE_HEIGHT;

  DEFAULT_FILE_NAME = '<source>';

  KIND_PROGRAM = 'Program';
  KIND_BLOCK = 'BlockStatement';
  KIND_SWITCH_CASE = 'SwitchCase';
  KIND_PARENTHESIZED = 'ParenthesizedExpression';
  TYPE_ASSERTION_KINDS: array[TGocciaTypeAssertionKind] of string = (
    'TSAsExpression', 'TSSatisfiesExpression', 'TSNonNullExpression');
  KIND_PROPERTY = 'Property';
  KIND_PARAMETER = 'Parameter';

  NO_OFFSET = -1;

type
  { The flags a layout rule dispatches on. Each kind carries the ones that
    mean something for it and no others (ADR 0162). }
  TASTNodeFlag = (anfComputed, anfOptional, anfAsync, anfGenerator,
    anfDelegate, anfPrefix, anfShorthand, anfMethod, anfRest, anfStatic);
  TASTNodeFlags = set of TASTNodeFlag;

  TASTFlatNode = record
    Kind: string;
    { Which flags this kind reports, and which of those are true. }
    FlagKinds: TASTNodeFlags;
    Flags: TASTNodeFlags;
    { '' when the kind has no operator. }
    OperatorText: string;
    { 'block' or 'expression' for an arrow function, '' otherwise. }
    BodyKind: string;
    { Measured in the text the parser was handed. }
    ParsedStart: Integer;
    ParsedEnd: Integer;
    { Measured in the caller's text. }
    StartOffset: Integer;
    EndOffset: Integer;
    FirstChild: Integer;
    LastChild: Integer;
    NextSibling: Integer;
    HasTypeOnly: Boolean;
    TypeOnly: Boolean;
  end;

  { The open nodes of the walk, innermost last. A frame is pushed for every
    node the walk passes through, reported or not, because a node that is not
    reported — a method wrapper, an optional chain's parentheses — still
    holds text that belongs to the range of the node around it. }
  TASTFrame = record
    Index: Integer;       // The node this frame reports, or -1.
    SavedParent: Integer;
    { The range so far, in parsed text. NO_OFFSET until something sets it. }
    Start: Integer;
    Finish: Integer;
    { A statement's range is the one the parser recorded (ADR 0118), and a
      parenthesized expression's is its parentheses: what is inside does not
      widen either. }
    Fixed: Boolean;
    { A statement is reported even when an edge of it falls in text a
      preprocessor wrote; anything else is not (see FinishNode). }
    Kept: Boolean;
  end;

  { Flattens a TGocciaProgram into the node records the JavaScript view is
    built from, through the tree's own child enumeration
    (TGocciaASTNode.ForEachChild). Emitting is separated from materializing
    so the whole walk runs without allocating a single managed value —
    nothing can be collected halfway through a traversal that has no roots
    yet. }
  TASTFlattener = class(TGocciaASTChildVisitor)
  private
    FNodes: array of TASTFlatNode;
    FCount: Integer;
    FFrames: array of TASTFrame;
    FFrameCount: Integer;
    FParent: Integer;
    FDepth: Integer;
    FExpressions: Boolean;
    { The node whose children are being enumerated. Whether a child is
      reported can depend on what it is a child of. }
    FEnumerating: TGocciaASTNode;
    FOriginMap: TGocciaOriginMap;
    { A node is reached once. The guard is for the parser sharing a node
      between two parents, which the arena allows (ADR 0143). }
    FSeen: TDictionary<Pointer, Boolean>;

    function MapOffset(const AOffset: Integer; const ABias: TGocciaOriginBias;
      out AExact: Boolean): Integer;
    function NewNode(const AKind: string): Integer;
    procedure PushFrame(const AIndex, AStart, AFinish: Integer;
      const AFixed, AKept: Boolean);
    procedure PopFrame;
    procedure Extend(const AStart, AFinish: Integer);
    procedure Link(const AParent, AIndex: Integer);
    procedure Dissolve(const AParent, AIndex: Integer);
    procedure FinishNode(const AFrame: TASTFrame);
    function MarkSeen(const ANode: TObject): Boolean;
    function ReportedKind(const ANode: TGocciaASTNode;
      out AFixed: Boolean): string;
    procedure Describe(const ANode: TGocciaASTNode; const AIndex: Integer);
    procedure VisitNode(const ANode: TGocciaASTNode);
    function IsWrittenByPreprocessor(const ANode: TGocciaASTNode): Boolean;
  public
    constructor Create(const AOriginMap: TGocciaOriginMap;
      const AExpressions: Boolean);
    destructor Destroy; override;
    procedure VisitChild(const ANode: TGocciaASTNode); override;
    procedure EnterPart(const AKind: TGocciaASTPartKind;
      const AStartOffset, AEndOffset: Integer;
      const AFlags: TGocciaASTPartFlags); override;
    procedure LeavePart; override;
    procedure Run(const AProgram: TGocciaProgram);
    function NodeAt(const AIndex: Integer): TASTFlatNode;
    property Count: Integer read FCount;
  end;

{ A type-level declaration reaches the tree as the empty statement the parser
  erased it into, and is reported as what it was written as. }
function ErasedTypeKindName(const AKind: TGocciaErasedTypeKind): string;
begin
  case AKind of
    etkTypeAlias: Result := 'TypeAliasDeclaration';
    etkInterface: Result := 'InterfaceDeclaration';
    etkExportTypeAlias: Result := 'ExportTypeAliasDeclaration';
    etkExportInterface: Result := 'ExportInterfaceDeclaration';
    etkImport: Result := 'ImportDeclaration';
    etkExport: Result := 'ExportDeclaration';
    etkReExport: Result := 'ReExportDeclaration';
  else
    Result := 'EmptyStatement';
  end;
end;

{ Whether a node of this statement's kind carries `typeOnly`, and its value.
  The module forms carry it either way, because the same kind is both a
  runtime and an erased statement; the type declarations always carry true. }
function StatementTypeOnly(const AStatement: TGocciaStatement;
  out ATypeOnly: Boolean): Boolean;
begin
  if AStatement is TGocciaEmptyStatement then
  begin
    ATypeOnly := TGocciaEmptyStatement(AStatement).ErasedTypeKind <> etkNone;
    Exit(ATypeOnly);
  end;
  ATypeOnly := False;
  Result := (AStatement is TGocciaImportDeclaration) or
    (AStatement is TGocciaExportDeclaration) or
    (AStatement is TGocciaReExportDeclaration);
end;

function StatementKind(const AStatement: TGocciaStatement): string;
begin
  // Ordered so a subclass is tested before its base.
  if AStatement is TGocciaBlockStatement then Result := KIND_BLOCK
  else if AStatement is TGocciaExpressionStatement then Result := 'ExpressionStatement'
  else if AStatement is TGocciaVariableDeclaration then Result := 'VariableDeclaration'
  else if AStatement is TGocciaFunctionDeclaration then Result := 'FunctionDeclaration'
  else if AStatement is TGocciaDestructuringDeclaration then Result := 'DestructuringDeclaration'
  else if AStatement is TGocciaIfStatement then Result := 'IfStatement'
  else if AStatement is TGocciaForStatement then Result := 'ForStatement'
  else if AStatement is TGocciaWhileStatement then Result := 'WhileStatement'
  else if AStatement is TGocciaDoWhileStatement then Result := 'DoWhileStatement'
  else if AStatement is TGocciaWithStatement then Result := 'WithStatement'
  else if AStatement is TGocciaForAwaitOfStatement then Result := 'ForAwaitOfStatement'
  else if AStatement is TGocciaForOfStatement then Result := 'ForOfStatement'
  else if AStatement is TGocciaForInStatement then Result := 'ForInStatement'
  else if AStatement is TGocciaReturnStatement then Result := 'ReturnStatement'
  else if AStatement is TGocciaThrowStatement then Result := 'ThrowStatement'
  else if AStatement is TGocciaTryStatement then Result := 'TryStatement'
  else if AStatement is TGocciaClassDeclaration then Result := 'ClassDeclaration'
  else if AStatement is TGocciaEnumDeclaration then Result := 'EnumDeclaration'
  else if AStatement is TGocciaExportEnumDeclaration then Result := 'ExportEnumDeclaration'
  else if AStatement is TGocciaImportDeclaration then Result := 'ImportDeclaration'
  else if AStatement is TGocciaExportDefaultDeclaration then Result := 'ExportDefaultDeclaration'
  else if AStatement is TGocciaExportVariableDeclaration then Result := 'ExportVariableDeclaration'
  else if AStatement is TGocciaExportDestructuringDeclaration then Result := 'ExportDestructuringDeclaration'
  else if AStatement is TGocciaExportFunctionDeclaration then Result := 'ExportFunctionDeclaration'
  else if AStatement is TGocciaExportClassDeclaration then Result := 'ExportClassDeclaration'
  else if AStatement is TGocciaReExportDeclaration then Result := 'ReExportDeclaration'
  else if AStatement is TGocciaExportDeclaration then Result := 'ExportDeclaration'
  else if AStatement is TGocciaSwitchStatement then Result := 'SwitchStatement'
  else if AStatement is TGocciaBreakStatement then Result := 'BreakStatement'
  else if AStatement is TGocciaContinueStatement then Result := 'ContinueStatement'
  else if AStatement is TGocciaUsingDeclaration then Result := 'UsingDeclaration'
  else if AStatement is TGocciaEmptyStatement then
    Result := ErasedTypeKindName(TGocciaEmptyStatement(AStatement).ErasedTypeKind)
  else Result := 'Statement';
end;

function OperatorText(const AOperator: TGocciaTokenType): string;
begin
  case AOperator of
    gttPlus: Result := '+';
    gttMinus: Result := '-';
    gttStar: Result := '*';
    gttSlash: Result := '/';
    gttPercent: Result := '%';
    gttPower: Result := '**';
    gttEqual: Result := '===';
    gttNotEqual: Result := '!==';
    gttLooseEqual: Result := '==';
    gttLooseNotEqual: Result := '!=';
    gttLess: Result := '<';
    gttGreater: Result := '>';
    gttLessEqual: Result := '<=';
    gttGreaterEqual: Result := '>=';
    gttAnd: Result := '&&';
    gttOr: Result := '||';
    gttNullishCoalescing: Result := '??';
    gttNot: Result := '!';
    gttTypeof: Result := 'typeof';
    gttVoid: Result := 'void';
    gttInstanceof: Result := 'instanceof';
    gttIn: Result := 'in';
    gttDelete: Result := 'delete';
    gttAssign: Result := '=';
    gttPlusAssign: Result := '+=';
    gttMinusAssign: Result := '-=';
    gttStarAssign: Result := '*=';
    gttSlashAssign: Result := '/=';
    gttPercentAssign: Result := '%=';
    gttPowerAssign: Result := '**=';
    gttNullishCoalescingAssign: Result := '??=';
    gttLogicalAndAssign: Result := '&&=';
    gttLogicalOrAssign: Result := '||=';
    gttIncrement: Result := '++';
    gttDecrement: Result := '--';
    gttBitwiseAnd: Result := '&';
    gttBitwiseOr: Result := '|';
    gttBitwiseXor: Result := '^';
    gttBitwiseNot: Result := '~';
    gttLeftShift: Result := '<<';
    gttRightShift: Result := '>>';
    gttUnsignedRightShift: Result := '>>>';
    gttBitwiseAndAssign: Result := '&=';
    gttBitwiseOrAssign: Result := '|=';
    gttBitwiseXorAssign: Result := '^=';
    gttLeftShiftAssign: Result := '<<=';
    gttRightShiftAssign: Result := '>>=';
    gttUnsignedRightShiftAssign: Result := '>>>=';
  else
    Result := '';
  end;
end;

function LiteralKind(const ALiteral: TGocciaLiteralExpression): string;
begin
  if ALiteral.Value is TGocciaStringLiteralValue then
    Result := 'StringLiteral'
  else if ALiteral.Value is TGocciaNumberLiteralValue then
    Result := 'NumericLiteral'
  else if ALiteral.Value is TGocciaBooleanLiteralValue then
    Result := 'BooleanLiteral'
  else if ALiteral.Value is TGocciaNullLiteralValue then
    Result := 'NullLiteral'
  else if ALiteral.Value is TGocciaBigIntValue then
    Result := 'BigIntLiteral'
  else
    { An `undefined` the parser put in place of a construct the host's
      profile turns off. It stands for text, not for a literal. }
    Result := '';
end;

{ The kind of an expression-level node, or '' for a node the view does not
  report: a node the parser builds that has no syntax of its own (a hole, a
  method wrapper, an optional chain's parentheses), or one whose syntax is
  reported by the node around it. }
function ExpressionKind(const ANode: TGocciaASTNode;
  const AParent: TGocciaASTNode): string;
begin
  // Ordered so a subclass is tested before its base.
  if ANode is TGocciaLiteralExpression then
    Result := LiteralKind(TGocciaLiteralExpression(ANode))
  else if (ANode is TGocciaTemplateLiteralExpression) or
          (ANode is TGocciaTemplateWithInterpolationExpression) then
    Result := 'TemplateLiteral'
  else if ANode is TGocciaTaggedTemplateExpression then
    Result := 'TaggedTemplateExpression'
  else if ANode is TGocciaRegexLiteralExpression then
    Result := 'RegExpLiteral'
  else if (ANode is TGocciaIdentifierExpression) or
          (ANode is TGocciaIdentifierDestructuringPattern) then
    Result := 'Identifier'
  else if ANode is TGocciaBinaryExpression then
  begin
    if TGocciaBinaryExpression(ANode).Operator in
       [gttAnd, gttOr, gttNullishCoalescing] then
      Result := 'LogicalExpression'
    else
      Result := 'BinaryExpression';
  end
  else if ANode is TGocciaSequenceExpression then
  begin
    { `(a?.b)` is kept as a one-element sequence so that the parentheses
      stop the chain. A sequence of one is not a sequence in the source. }
    if TGocciaSequenceExpression(ANode).Expressions.Count > 1 then
      Result := 'SequenceExpression'
    else
      Result := '';
  end
  else if ANode is TGocciaUnaryExpression then
    Result := 'UnaryExpression'
  else if (ANode is TGocciaTargetedAssignmentExpression) or
          (ANode is TGocciaDestructuringAssignmentExpression) then
    Result := 'AssignmentExpression'
  else if ANode is TGocciaIncrementExpression then
    Result := 'UpdateExpression'
  else if ANode is TGocciaCallExpression then
    Result := 'CallExpression'
  else if (ANode is TGocciaMemberExpression) or
          (ANode is TGocciaPrivateMemberExpression) then
    Result := 'MemberExpression'
  else if ANode is TGocciaArrayExpression then
    Result := 'ArrayExpression'
  else if ANode is TGocciaObjectExpression then
    Result := 'ObjectExpression'
  else if ANode is TGocciaArrowFunctionExpression then
    Result := 'ArrowFunctionExpression'
  else if ANode is TGocciaFunctionExpression then
  begin
    { A function declaration and an object method are each one piece of
      syntax that the parser builds as two nodes: the declaration or the
      property is reported, and the function inside it is that same text. }
    if (AParent is TGocciaFunctionDeclaration) or
       (AParent is TGocciaObjectMethodDefinition) then
      Result := ''
    else
      Result := 'FunctionExpression';
  end
  else if (ANode is TGocciaObjectMethodDefinition) or
          (ANode is TGocciaGetterExpression) or
          (ANode is TGocciaSetterExpression) or
          (ANode is TGocciaClassMethod) then
    { A method, getter or setter is the object Property or class member
      around it: the parser keeps the function separately, but in the source
      it is the same text. }
    Result := ''
  else if ANode is TGocciaClassExpression then
    Result := 'ClassExpression'
  else if ANode is TGocciaYieldExpression then
    Result := 'YieldExpression'
  else if ANode is TGocciaAwaitExpression then
    Result := 'AwaitExpression'
  else if ANode is TGocciaConditionalExpression then
    Result := 'ConditionalExpression'
  else if ANode is TGocciaNewExpression then
    Result := 'NewExpression'
  else if ANode is TGocciaThisExpression then
  begin
    { `#x in o` has no receiver in the source: the parser puts a `this` at
      the `#` so the brand check has an object to test. }
    if (AParent is TGocciaPrivateMemberExpression) and
       (ANode.Span.StartOffset = AParent.Span.StartOffset) then
      Result := ''
    else
      Result := 'ThisExpression';
  end
  else if ANode is TGocciaSuperExpression then
    Result := 'Super'
  else if (ANode is TGocciaImportMetaExpression) or
          (ANode is TGocciaNewTargetExpression) then
    Result := 'MetaProperty'
  else if ANode is TGocciaImportCallExpression then
    Result := 'ImportExpression'
  else if ANode is TGocciaSpreadExpression then
    Result := 'SpreadElement'
  else if ANode is TGocciaArrayDestructuringPattern then
    Result := 'ArrayPattern'
  else if ANode is TGocciaObjectDestructuringPattern then
    Result := 'ObjectPattern'
  else if ANode is TGocciaRestDestructuringPattern then
    Result := 'RestElement'
  else if ANode is TGocciaAssignmentDestructuringPattern then
    Result := 'AssignmentPattern'
  else if ANode is TGocciaIsExpression then
    Result := 'IsExpression'
  else if ANode is TGocciaMatchExpression then
    Result := 'MatchExpression'
  else
    { A hole, a member-expression destructuring target (the member inside
      is reported), and the patterns of `match`, which are not yet part of
      the view. }
    Result := '';
end;

{ TASTFlattener }

constructor TASTFlattener.Create(const AOriginMap: TGocciaOriginMap;
  const AExpressions: Boolean);
begin
  inherited Create;
  FOriginMap := AOriginMap;
  FExpressions := AExpressions;
  FParent := -1;
  FSeen := TDictionary<Pointer, Boolean>.Create;
end;

destructor TASTFlattener.Destroy;
begin
  FSeen.Free;
  inherited;
end;

function TASTFlattener.MarkSeen(const ANode: TObject): Boolean;
begin
  Result := not FSeen.ContainsKey(Pointer(ANode));
  if Result then
    FSeen.Add(Pointer(ANode), True);
end;

{ nil means no preprocessor ran, so the parsed text is the caller's own and
  every offset in it already is an original one. }
function TASTFlattener.MapOffset(const AOffset: Integer;
  const ABias: TGocciaOriginBias; out AExact: Boolean): Integer;
begin
  if not Assigned(FOriginMap) then
  begin
    AExact := True;
    Exit(AOffset);
  end;
  AExact := FOriginMap.Map(AOffset, ABias, Result);
end;

function TASTFlattener.NewNode(const AKind: string): Integer;
begin
  if FCount = Length(FNodes) then
    if FCount = 0 then
      SetLength(FNodes, 256)
    else
      SetLength(FNodes, FCount * 2);

  Result := FCount;
  Inc(FCount);
  FNodes[Result].Kind := AKind;
  FNodes[Result].FlagKinds := [];
  FNodes[Result].Flags := [];
  FNodes[Result].OperatorText := '';
  FNodes[Result].BodyKind := '';
  FNodes[Result].FirstChild := -1;
  FNodes[Result].LastChild := -1;
  FNodes[Result].NextSibling := -1;
  FNodes[Result].HasTypeOnly := False;
  FNodes[Result].TypeOnly := False;
end;

function TASTFlattener.NodeAt(const AIndex: Integer): TASTFlatNode;
begin
  Result := FNodes[AIndex];
end;

procedure TASTFlattener.PushFrame(const AIndex, AStart, AFinish: Integer;
  const AFixed, AKept: Boolean);
begin
  if FFrameCount = Length(FFrames) then
    if FFrameCount = 0 then
      SetLength(FFrames, 64)
    else
      SetLength(FFrames, FFrameCount * 2);
  FFrames[FFrameCount].Index := AIndex;
  FFrames[FFrameCount].SavedParent := FParent;
  FFrames[FFrameCount].Fixed := AFixed;
  FFrames[FFrameCount].Kept := AKept;
  { A kept node's range is the parser's even when it is empty. Anything else
    with no text of its own takes its range from what it holds. }
  if AKept or (AFinish > AStart) then
  begin
    FFrames[FFrameCount].Start := AStart;
    FFrames[FFrameCount].Finish := AFinish;
  end
  else
  begin
    FFrames[FFrameCount].Start := NO_OFFSET;
    FFrames[FFrameCount].Finish := NO_OFFSET;
  end;
  Inc(FFrameCount);
  if AIndex >= 0 then
    FParent := AIndex;
end;

{ Widens the innermost open frame to cover a range inside it. }
procedure TASTFlattener.Extend(const AStart, AFinish: Integer);
begin
  if (FFrameCount = 0) or (AFinish <= AStart) then
    Exit;
  with FFrames[FFrameCount - 1] do
  begin
    if Fixed then
      Exit;
    if (Start = NO_OFFSET) or (AStart < Start) then
      Start := AStart;
    if (Finish = NO_OFFSET) or (AFinish > Finish) then
      Finish := AFinish;
  end;
end;

procedure TASTFlattener.PopFrame;
var
  Frame: TASTFrame;
begin
  Dec(FFrameCount);
  Frame := FFrames[FFrameCount];
  FParent := Frame.SavedParent;
  if Frame.Index >= 0 then
    FinishNode(Frame);
  if FExpressions and (Frame.Start <> NO_OFFSET) then
    Extend(Frame.Start, Frame.Finish);
end;

{ Children are kept in source order. The enumeration already produces them
  in that order, so a child nearly always goes after the last one; the scan
  from the front is for the rest. }
procedure TASTFlattener.Link(const AParent, AIndex: Integer);
var
  Previous, Sibling, Last: Integer;
begin
  FNodes[AIndex].NextSibling := -1;
  Last := FNodes[AParent].LastChild;
  if Last < 0 then
  begin
    FNodes[AParent].FirstChild := AIndex;
    FNodes[AParent].LastChild := AIndex;
    Exit;
  end;
  if FNodes[Last].StartOffset <= FNodes[AIndex].StartOffset then
  begin
    FNodes[Last].NextSibling := AIndex;
    FNodes[AParent].LastChild := AIndex;
    Exit;
  end;

  Previous := -1;
  Sibling := FNodes[AParent].FirstChild;
  while (Sibling >= 0) and
        (FNodes[Sibling].StartOffset <= FNodes[AIndex].StartOffset) do
  begin
    Previous := Sibling;
    Sibling := FNodes[Sibling].NextSibling;
  end;
  FNodes[AIndex].NextSibling := Sibling;
  if Previous < 0 then
    FNodes[AParent].FirstChild := AIndex
  else
    FNodes[Previous].NextSibling := AIndex;
end;

{ Drops a node from the view and gives its children to its parent. }
procedure TASTFlattener.Dissolve(const AParent, AIndex: Integer);
var
  Child, Next: Integer;
begin
  Child := FNodes[AIndex].FirstChild;
  FNodes[AIndex].FirstChild := -1;
  FNodes[AIndex].LastChild := -1;
  while Child >= 0 do
  begin
    Next := FNodes[Child].NextSibling;
    if AParent >= 0 then
      Link(AParent, Child);
    Child := Next;
  end;
end;

procedure TASTFlattener.FinishNode(const AFrame: TASTFrame);
var
  StartExact, EndExact: Boolean;
begin
  with FNodes[AFrame.Index] do
  begin
    ParsedStart := AFrame.Start;
    ParsedEnd := AFrame.Finish;
    if (ParsedStart = NO_OFFSET) and not AFrame.Kept then
    begin
      { Nothing in the source: a part the parser did not measure and that
        holds no node either. }
      Dissolve(AFrame.SavedParent, AFrame.Index);
      Exit;
    end;
    { The parser measured this node in whatever text it was handed.
      Everything the result carries is measured in the file the caller
      passed, so the two offsets are mapped back here, once. }
    StartOffset := MapOffset(ParsedStart, obStart, StartExact);
    EndOffset := MapOffset(ParsedEnd, obEnd, EndExact);
  end;

  { A statement whose edge falls in text a preprocessor wrote is rounded
    outward to the construct that produced it (ADR 0118). Below the
    statement level that construct is usually the node itself — the call
    the JSX transformer writes for an element, the string it escapes an
    attribute into — so a node that does not begin and end in the caller's
    own text is not reported at all, and its children move up. }
  if not AFrame.Kept and not (StartExact and EndExact) then
  begin
    Dissolve(AFrame.SavedParent, AFrame.Index);
    Exit;
  end;

  if AFrame.SavedParent >= 0 then
    Link(AFrame.SavedParent, AFrame.Index);
end;

{ The kind a node is reported as, or '' when it is not reported. }
function TASTFlattener.ReportedKind(const ANode: TGocciaASTNode;
  out AFixed: Boolean): string;
begin
  AFixed := True;
  if ANode is TGocciaStatement then
  begin
    { An export and the declaration it exports are one statement in the
      source, which the parser builds as two nodes. The export is reported.
      An exported enum has always reported its declaration as well. }
    if (FEnumerating is TGocciaExportVariableDeclaration) or
       (FEnumerating is TGocciaExportDestructuringDeclaration) or
       (FEnumerating is TGocciaExportFunctionDeclaration) or
       (FEnumerating is TGocciaExportClassDeclaration) then
      Result := ''
    else
      Result := StatementKind(TGocciaStatement(ANode));
  end
  else if ANode is TGocciaCaseClause then
    Result := KIND_SWITCH_CASE
  else if FExpressions then
  begin
    AFixed := False;
    Result := ExpressionKind(ANode, FEnumerating);
  end
  else
    Result := '';
end;

procedure TASTFlattener.Describe(const ANode: TGocciaASTNode;
  const AIndex: Integer);

  procedure Report(const AFlag: TASTNodeFlag; const AValue: Boolean);
  begin
    Include(FNodes[AIndex].FlagKinds, AFlag);
    if AValue then
      Include(FNodes[AIndex].Flags, AFlag);
  end;

begin
  if ANode is TGocciaBinaryExpression then
    FNodes[AIndex].OperatorText :=
      OperatorText(TGocciaBinaryExpression(ANode).Operator)
  else if ANode is TGocciaUnaryExpression then
    FNodes[AIndex].OperatorText :=
      OperatorText(TGocciaUnaryExpression(ANode).Operator)
  else if ANode is TGocciaIncrementExpression then
  begin
    FNodes[AIndex].OperatorText :=
      OperatorText(TGocciaIncrementExpression(ANode).Operator);
    Report(anfPrefix, TGocciaIncrementExpression(ANode).IsPrefix);
  end
  else if ANode is TGocciaAssignmentExpression then
    FNodes[AIndex].OperatorText := '='
  else if ANode is TGocciaPropertyAssignmentExpression then
    FNodes[AIndex].OperatorText := '='
  else if ANode is TGocciaComputedPropertyAssignmentExpression then
    FNodes[AIndex].OperatorText := '='
  else if ANode is TGocciaPrivatePropertyAssignmentExpression then
    FNodes[AIndex].OperatorText := '='
  else if ANode is TGocciaDestructuringAssignmentExpression then
    FNodes[AIndex].OperatorText := '='
  else if ANode is TGocciaCompoundAssignmentExpression then
    FNodes[AIndex].OperatorText :=
      OperatorText(TGocciaCompoundAssignmentExpression(ANode).Operator)
  else if ANode is TGocciaPropertyCompoundAssignmentExpression then
    FNodes[AIndex].OperatorText := OperatorText(
      TGocciaPropertyCompoundAssignmentExpression(ANode).Operator)
  else if ANode is TGocciaComputedPropertyCompoundAssignmentExpression then
    FNodes[AIndex].OperatorText := OperatorText(
      TGocciaComputedPropertyCompoundAssignmentExpression(ANode).Operator)
  else if ANode is TGocciaPrivatePropertyCompoundAssignmentExpression then
    FNodes[AIndex].OperatorText := OperatorText(
      TGocciaPrivatePropertyCompoundAssignmentExpression(ANode).Operator)
  else if ANode is TGocciaMemberExpression then
  begin
    Report(anfComputed, TGocciaMemberExpression(ANode).Computed);
    Report(anfOptional, TGocciaMemberExpression(ANode).Optional);
  end
  else if ANode is TGocciaPrivateMemberExpression then
  begin
    Report(anfComputed, False);
    Report(anfOptional, TGocciaPrivateMemberExpression(ANode).Optional);
  end
  else if ANode is TGocciaCallExpression then
    Report(anfOptional, TGocciaCallExpression(ANode).Optional)
  else if ANode is TGocciaArrowFunctionExpression then
  begin
    Report(anfAsync, TGocciaArrowFunctionExpression(ANode).IsAsync);
    if TGocciaArrowFunctionExpression(ANode).Body is TGocciaBlockStatement then
      FNodes[AIndex].BodyKind := 'block'
    else
      FNodes[AIndex].BodyKind := 'expression';
  end
  else if ANode is TGocciaFunctionExpression then
  begin
    Report(anfAsync, TGocciaFunctionExpression(ANode).IsAsync);
    Report(anfGenerator, TGocciaFunctionExpression(ANode).IsGenerator);
  end
  else if ANode is TGocciaYieldExpression then
    Report(anfDelegate, TGocciaYieldExpression(ANode).IsDelegate);
end;

{ True when ANode is text a preprocessor wrote rather than copied. Its
  start alone is not enough: the transformer anchors what it writes to the
  construct it replaces, so its first character can map exactly. }
function TASTFlattener.IsWrittenByPreprocessor(
  const ANode: TGocciaASTNode): Boolean;
var
  StartExact, EndExact: Boolean;
begin
  if not Assigned(FOriginMap) or not Assigned(ANode) then
    Exit(False);
  MapOffset(ANode.Span.StartOffset, obStart, StartExact);
  MapOffset(ANode.Span.EndOffset, obEnd, EndExact);
  Result := not (StartExact and EndExact);
end;

procedure TASTFlattener.VisitNode(const ANode: TGocciaASTNode);
var
  Kind: string;
  Fixed, Parenthesized: Boolean;
  Index, Wrappers, I: Integer;
  Saved: TGocciaASTNode;
  Expression: TGocciaExpression;
  Assertion: TGocciaTypeAssertion;

  procedure PushParentheses;
  begin
    PushFrame(NewNode(KIND_PARENTHESIZED), Expression.ParenStartOffset,
      Expression.ParenEndOffset, True, False);
    Inc(Wrappers);
    Parenthesized := False;
  end;

begin
  Kind := ReportedKind(ANode, Fixed);

  { The parser keeps no node for parentheses or for a TypeScript assertion
    (`as T`, `satisfies T`, `x!`), only where they were. Each is reported as
    a node of its own around the expression, so that a rule can tell
    `(a + b) * c` from `a + b * c` by its tree, and so that the expression
    an assertion applies to ends where its text does. Each wrapper ends
    after the one inside it, so they are opened outermost first: by the
    offset they end at, latest first. An assertion's range covers what it
    wraps, so its frame is widened by its children rather than fixed. }
  Wrappers := 0;
  Expression := nil;
  Parenthesized := False;
  if FExpressions and (ANode is TGocciaExpression) then
  begin
    Expression := TGocciaExpression(ANode);
    Parenthesized := Expression.HasParenRange;
    for I := Expression.TypeAssertionCount - 1 downto 0 do
    begin
      Assertion := Expression.TypeAssertions[I];
      if Parenthesized and
         (Expression.ParenEndOffset > Assertion.EndOffset) then
        PushParentheses;
      PushFrame(NewNode(TYPE_ASSERTION_KINDS[Assertion.Kind]),
        Assertion.StartOffset, Assertion.EndOffset, False, False);
      Inc(Wrappers);
    end;
    if Parenthesized then
      PushParentheses;
  end;

  { The JSX transformer turns an element into a call it writes itself. The
    call's edges are anchored to the element's, so it survives FinishNode;
    its callee is the transformer's own text, which tells it apart from any
    call the file wrote. }
  if (Kind = 'CallExpression') and IsWrittenByPreprocessor(
    TGocciaCallExpression(ANode).Callee) then
    Kind := 'JSXElement';

  if Kind <> '' then
  begin
    Index := NewNode(Kind);
    if FExpressions and not Fixed and (Kind <> 'JSXElement') then
      Describe(ANode, Index)
    else if ANode is TGocciaStatement then
      FNodes[Index].HasTypeOnly := StatementTypeOnly(TGocciaStatement(ANode),
        FNodes[Index].TypeOnly);
  end
  else
    Index := -1;
  PushFrame(Index, ANode.Span.StartOffset, ANode.Span.EndOffset,
    Fixed and (Kind <> ''), Fixed and (Kind <> ''));

  Saved := FEnumerating;
  FEnumerating := ANode;
  try
    ANode.ForEachChild(Self);
  finally
    FEnumerating := Saved;
  end;

  PopFrame;
  for I := 1 to Wrappers do
    PopFrame;
end;

procedure TASTFlattener.VisitChild(const ANode: TGocciaASTNode);
begin
  if not Assigned(ANode) then
    Exit;
  if FDepth >= MAX_WALK_DEPTH then
    Exit;
  if not MarkSeen(ANode) then
    Exit;

  Inc(FDepth);
  try
    VisitNode(ANode);
  finally
    Dec(FDepth);
  end;
end;

procedure TASTFlattener.EnterPart(const AKind: TGocciaASTPartKind;
  const AStartOffset, AEndOffset: Integer;
  const AFlags: TGocciaASTPartFlags);

  procedure Report(const AIndex: Integer; const AFlag: TASTNodeFlag;
    const APartFlag: TGocciaASTPartFlag);
  begin
    Include(FNodes[AIndex].FlagKinds, AFlag);
    if APartFlag in AFlags then
      Include(FNodes[AIndex].Flags, AFlag);
  end;

var
  Index: Integer;
begin
  if not FExpressions then
  begin
    { Statements are reached through a part, but a part is not reported. }
    PushFrame(-1, NO_OFFSET, NO_OFFSET, False, False);
    Exit;
  end;

  case AKind of
    apkProperty:
      begin
        Index := NewNode(KIND_PROPERTY);
        Report(Index, anfComputed, apfComputed);
        Report(Index, anfShorthand, apfShorthand);
        Report(Index, anfMethod, apfMethod);
      end;
    apkParameter:
      begin
        Index := NewNode(KIND_PARAMETER);
        Report(Index, anfRest, apfRest);
      end;
    apkClassMethod, apkClassField, apkClassAccessor:
      begin
        case AKind of
          apkClassMethod: Index := NewNode('MethodDefinition');
          apkClassField: Index := NewNode('PropertyDefinition');
        else
          Index := NewNode('AccessorProperty');
        end;
        Report(Index, anfStatic, apfStatic);
        Report(Index, anfComputed, apfComputed);
      end;
    apkStaticBlock:
      Index := NewNode('StaticBlock');
  else
    // The module path of an import or a re-export is a string literal.
    Index := NewNode('StringLiteral');
  end;
  { A part the parser measured has that range, as a statement does; one it
    did not takes its range from what it holds. }
  PushFrame(Index, AStartOffset, AEndOffset, AEndOffset > AStartOffset, False);
end;

procedure TASTFlattener.LeavePart;
begin
  PopFrame;
end;

procedure TASTFlattener.Run(const AProgram: TGocciaProgram);
var
  Index: Integer;
  Saved: TGocciaASTNode;
  Exact: Boolean;
begin
  Index := NewNode(KIND_PROGRAM);
  PushFrame(Index, AProgram.Span.StartOffset, AProgram.Span.EndOffset, True,
    True);
  Saved := FEnumerating;
  FEnumerating := AProgram;
  try
    AProgram.ForEachChild(Self);
  finally
    FEnumerating := Saved;
  end;
  { The program is the root: it has no parent to link to, and its range is
    the parser's even when that is empty. }
  Dec(FFrameCount);
  FParent := -1;
  with FNodes[Index] do
  begin
    ParsedStart := AProgram.Span.StartOffset;
    ParsedEnd := AProgram.Span.EndOffset;
    StartOffset := MapOffset(ParsedStart, obStart, Exact);
    EndOffset := MapOffset(ParsedEnd, obEnd, Exact);
  end;
end;

{ Materializing }

{ Comment offsets come from the lexer, so they index the parsed text and need
  the same mapping back the tree's nodes get. }
function MappedOffset(const AOriginMap: TGocciaOriginMap;
  const AOffset: Integer; const ABias: TGocciaOriginBias): Integer;
begin
  if not Assigned(AOriginMap) then
    Exit(AOffset);
  AOriginMap.Map(AOffset, ABias, Result);
end;

const
  { What an assignment to a fresh key of an ordinary object creates. The
    result's objects are made here and their keys are distinct, so each is
    defined directly, without the lookups an assignment makes first. }
  RESULT_PROPERTY_FLAGS = [pfEnumerable, pfConfigurable, pfWritable];
  POSITION_PROPERTY_COUNT = 2;
  LOC_PROPERTY_COUNT = 2;
  { children, kind, start, end, loc. }
  NODE_BASE_PROPERTY_COUNT = 5;
  { kind, start, end, loc. }
  COMMENT_PROPERTY_COUNT = 4;

type
  { The numbers one result holds. Every offset, line and column in it lies
    between 0 and one past the source's length, and a file repeats them
    many times over — a node starts where its first child and first token
    do — so each is made once and shared: a number is a primitive, and which
    instance holds it is not observable. A value is attached to the result as
    soon as it is made, so every one the cache holds is reachable from the
    result while the cache lives. }
  TASTNumberValues = class
  private
    FValues: array of TGocciaNumberLiteralValue;
  public
    constructor Create(const ASourceLength: Integer);
    function ValueOf(const AValue: Integer): TGocciaNumberLiteralValue;
  end;

constructor TASTNumberValues.Create(const ASourceLength: Integer);
begin
  inherited Create;
  SetLength(FValues, ASourceLength + 2);
end;

function TASTNumberValues.ValueOf(
  const AValue: Integer): TGocciaNumberLiteralValue;
begin
  if (AValue < 0) or (AValue > High(FValues)) then
    Exit(TGocciaNumberLiteralValue.Create(AValue));
  Result := FValues[AValue];
  if Result = nil then
  begin
    Result := TGocciaNumberLiteralValue.Create(AValue);
    FValues[AValue] := Result;
  end;
end;

function CreatePositionObject(const ANumbers: TASTNumberValues;
  const ALine, AColumn: Integer): TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create(nil, POSITION_PROPERTY_COUNT);
  Result.DefineFreshDataProperty('line', ANumbers.ValueOf(ALine),
    RESULT_PROPERTY_FLAGS);
  Result.DefineFreshDataProperty('column', ANumbers.ValueOf(AColumn),
    RESULT_PROPERTY_FLAGS);
end;

{ The two positions are attached as they are made, so each is reachable from
  the loc object while the next allocates. }
function CreateLocObject(const ANumbers: TASTNumberValues;
  const AStartLine, AStartColumn, AEndLine,
  AEndColumn: Integer): TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create(nil, LOC_PROPERTY_COUNT);
  Result.DefineFreshDataProperty('start',
    CreatePositionObject(ANumbers, AStartLine, AStartColumn),
    RESULT_PROPERTY_FLAGS);
  Result.DefineFreshDataProperty('end',
    CreatePositionObject(ANumbers, AEndLine, AEndColumn),
    RESULT_PROPERTY_FLAGS);
end;

const
  FLAG_NAMES: array[TASTNodeFlag] of string = ('computed', 'optional',
    'async', 'generator', 'delegate', 'prefix', 'shorthand', 'method',
    'rest', 'static');

{ Every array in the result is filled in index order as it is made, so an
  element goes straight onto the end of its dense elements, where
  ArrayCreateDataProperty would spell the index as a property key and parse
  it back. }
procedure AppendElement(const AArray: TGocciaArrayValue; const AIndex: Integer;
  const AValue: TGocciaValue);
begin
  if not AArray.TryAppendDenseElementFast(AIndex, AValue) then
    ArrayCreateDataProperty(AArray, AIndex, AValue);
end;

{ Nodes are materialized parent-first. A node is held only by this call (its
  parent attaches it on return) while its kind string and its children are
  made, and each of those can collect, so it is rooted until it returns; every
  object it has attached by then is reachable from it. }
function MaterializeNode(const AFlattener: TASTFlattener;
  const ACoordinates: IGocciaSourceCoordinates;
  const ANumbers: TASTNumberValues;
  const AIndex: Integer): TGocciaObjectValue;
var
  Children: TGocciaArrayValue;
  Node: TASTFlatNode;
  ChildIndex, Position, PropertyCount: Integer;
  StartLine, StartColumn, EndLine, EndColumn: Integer;
  Flag: TASTNodeFlag;
  Roots: TGocciaActiveRootFrame;
begin
  Node := AFlattener.NodeAt(AIndex);
  { The line and column are read off the caller's text at the mapped
    offsets — deriving them rather than translating them separately is what
    keeps `loc` and the range the same answer. }
  ACoordinates.PositionAtOffset(Node.StartOffset, StartLine, StartColumn);
  ACoordinates.PositionAtOffset(Node.EndOffset, EndLine, EndColumn);
  Roots.Initialize;
  try
    PropertyCount := NODE_BASE_PROPERTY_COUNT + Ord(Node.OperatorText <> '') +
      Ord(Node.BodyKind <> '') + Ord(Node.HasTypeOnly);
    for Flag := Low(TASTNodeFlag) to High(TASTNodeFlag) do
      if Flag in Node.FlagKinds then
        Inc(PropertyCount);
    Result := TGocciaObjectValue.Create(nil, PropertyCount);
    Roots.Add(Result);
    Children := TGocciaArrayValue.Create;
    Result.DefineFreshDataProperty('children', Children, RESULT_PROPERTY_FLAGS);
    Result.DefineFreshDataProperty('kind',
      TGocciaStringLiteralValue.Create(Node.Kind), RESULT_PROPERTY_FLAGS);
    Result.DefineFreshDataProperty('start',
      ANumbers.ValueOf(Node.StartOffset), RESULT_PROPERTY_FLAGS);
    Result.DefineFreshDataProperty('end',
      ANumbers.ValueOf(Node.EndOffset), RESULT_PROPERTY_FLAGS);
    Result.DefineFreshDataProperty('loc', CreateLocObject(ANumbers, StartLine,
      StartColumn, EndLine, EndColumn), RESULT_PROPERTY_FLAGS);
    if Node.OperatorText <> '' then
      Result.DefineFreshDataProperty('operator',
        TGocciaStringLiteralValue.Create(Node.OperatorText),
        RESULT_PROPERTY_FLAGS);
    if Node.BodyKind <> '' then
      Result.DefineFreshDataProperty('bodyKind',
        TGocciaStringLiteralValue.Create(Node.BodyKind), RESULT_PROPERTY_FLAGS);
    for Flag := Low(TASTNodeFlag) to High(TASTNodeFlag) do
      if Flag in Node.FlagKinds then
        if Flag in Node.Flags then
          Result.DefineFreshDataProperty(FLAG_NAMES[Flag],
            TGocciaBooleanLiteralValue.TrueValue, RESULT_PROPERTY_FLAGS)
        else
          Result.DefineFreshDataProperty(FLAG_NAMES[Flag],
            TGocciaBooleanLiteralValue.FalseValue, RESULT_PROPERTY_FLAGS);
    if Node.HasTypeOnly then
      if Node.TypeOnly then
        Result.DefineFreshDataProperty('typeOnly',
          TGocciaBooleanLiteralValue.TrueValue, RESULT_PROPERTY_FLAGS)
      else
        Result.DefineFreshDataProperty('typeOnly',
          TGocciaBooleanLiteralValue.FalseValue, RESULT_PROPERTY_FLAGS);

    Position := 0;
    ChildIndex := Node.FirstChild;
    while ChildIndex >= 0 do
    begin
      AppendElement(Children, Position,
        MaterializeNode(AFlattener, ACoordinates, ANumbers, ChildIndex));
      Inc(Position);
      ChildIndex := AFlattener.NodeAt(ChildIndex).NextSibling;
    end;
  finally
    Roots.Clear;
  end;
end;

{ Tokens }

type
  { What a consumer dispatches on. The names are the lexical grammar's own
    (ES2026 §12.4-§12.9, and the JSX specification for the two JSX kinds), so
    a reader can look each one up; the lexer's finer distinctions — which
    operator, which keyword — are in the text the range selects. }
  TASTTokenType = (
    atIdentifier, atPrivateIdentifier, atKeyword, atPunctuator,
    atNumericLiteral, atStringLiteral, atRegularExpressionLiteral,
    atNoSubstitutionTemplate, atTemplateHead, atTemplateMiddle,
    atTemplateTail, atLineComment, atBlockComment, atHashbangComment,
    atJSXIdentifier, atJSXText
  );

  TASTFlatToken = record
    TokenType: TASTTokenType;
    StartOffset: Integer;
    EndOffset: Integer;
  end;

  TASTFlatTokenArray = array of TASTFlatToken;

const
  TOKEN_TYPE_NAMES: array[TASTTokenType] of string = (
    'Identifier', 'PrivateIdentifier', 'Keyword', 'Punctuator',
    'NumericLiteral', 'StringLiteral', 'RegularExpressionLiteral',
    'NoSubstitutionTemplate', 'TemplateHead', 'TemplateMiddle',
    'TemplateTail', 'LineComment', 'BlockComment', 'HashbangComment',
    'JSXIdentifier', 'JSXText'
  );

  OPTION_TOKENS = 'tokens';
  PROP_TOKENS = 'tokens';
  INITIAL_TOKEN_CAPACITY = 256;
  TOKEN_PROPERTY_COUNT = 3;
  PROP_TOKEN_TYPE = 'type';
  PROP_TOKEN_START = 'start';
  PROP_TOKEN_END = 'end';

  // ES2026 §12.7.2 ReservedWord. The engine's own reserved-word list answers
  // a different question — which names a binding may not use in this
  // language profile — so it is not the one a token is classified by.
  RESERVED_WORDS: array[0..37] of string = (
    KEYWORD_AWAIT, KEYWORD_BREAK, KEYWORD_CASE, KEYWORD_CATCH, KEYWORD_CLASS,
    KEYWORD_CONST, KEYWORD_CONTINUE, KEYWORD_DEBUGGER, KEYWORD_DEFAULT,
    KEYWORD_DELETE, KEYWORD_DO, KEYWORD_ELSE, KEYWORD_ENUM, KEYWORD_EXPORT,
    KEYWORD_EXTENDS, KEYWORD_FALSE, KEYWORD_FINALLY, KEYWORD_FOR,
    KEYWORD_FUNCTION, KEYWORD_IF, KEYWORD_IMPORT, KEYWORD_IN,
    KEYWORD_INSTANCEOF, KEYWORD_NEW, KEYWORD_NULL, KEYWORD_RETURN,
    KEYWORD_SUPER, KEYWORD_SWITCH, KEYWORD_THIS, KEYWORD_THROW, KEYWORD_TRUE,
    KEYWORD_TRY, KEYWORD_TYPEOF, KEYWORD_VAR, KEYWORD_VOID, KEYWORD_WHILE,
    KEYWORD_WITH, KEYWORD_YIELD
  );

function IsReservedWordSpelling(const AText: string): Boolean;
var
  I: Integer;
begin
  for I := Low(RESERVED_WORDS) to High(RESERVED_WORDS) do
    if AText = RESERVED_WORDS[I] then
      Exit(True);
  Result := False;
end;

{ Every token the lexer produces for an IdentifierName, whichever keyword
  token type it chose: the lexer promotes some contextual words (`let`,
  `static`, `as`, `from`) and not others, so its token type is not what a
  token is classified by. }
function IsIdentifierNameToken(const ATokenType: TGocciaTokenType): Boolean;
begin
  case ATokenType of
    gttIdentifier, gttTrue, gttFalse, gttNull, gttConst..gttFunction,
    gttTypeof, gttVoid, gttInstanceof, gttIn, gttDelete:
      Result := True;
  else
    Result := False;
  end;
end;

function LexerTokenType(const AToken: TGocciaSourceToken): TASTTokenType;
begin
  case AToken.TokenType of
    gttNumber, gttBigInt:
      Result := atNumericLiteral;
    gttString:
      Result := atStringLiteral;
    gttRegex:
      Result := atRegularExpressionLiteral;
    gttTemplate:
      Result := atNoSubstitutionTemplate;
    gttTemplateHead:
      Result := atTemplateHead;
    gttTemplateMiddle:
      Result := atTemplateMiddle;
    gttTemplateTail:
      Result := atTemplateTail;
  else
    if not IsIdentifierNameToken(AToken.TokenType) then
      Result := atPunctuator
    // ES2026 §12.7.2 Note 1: a keyword cannot be spelled with an escape.
    else if (not AToken.ContainsEscape) and
            IsReservedWordSpelling(AToken.Lexeme) then
      Result := atKeyword
    else
      Result := atIdentifier;
  end;
end;

function CommentTokenType(const AKind: TGocciaCommentKind): TASTTokenType;
begin
  case AKind of
    gckLine:
      Result := atLineComment;
    gckBlock:
      Result := atBlockComment;
  else
    Result := atHashbangComment;
  end;
end;

function JSXTokenType(const AKind: TGocciaJSXTokenKind): TASTTokenType;
begin
  case AKind of
    jtkPunctuator:
      Result := atPunctuator;
    jtkJSXIdentifier:
      Result := atJSXIdentifier;
    jtkIdentifier:
      Result := atIdentifier;
    jtkJSXText:
      Result := atJSXText;
    jtkString:
      Result := atStringLiteral;
    jtkLineComment:
      Result := atLineComment;
  else
    Result := atBlockComment;
  end;
end;

{ Collects the file's tokens from the three places they are decided, in the
  file's offsets: the lexer's committed stream (the goals the parser chose
  settle regex versus division and every template part), the lexer's comment
  trivia, and — when the JSX transformer rewrote the text — the JSX the lexer
  never saw. Allocates no managed value. }
type
  TASTTokenCollector = class
  private
    FTokens: TASTFlatTokenArray;
    FCount: Integer;
    procedure Add(const ATokenType: TASTTokenType;
      const AStartOffset, AEndOffset: Integer);
    procedure AddLexerTokens(const APipelineResult: TGocciaSourcePipelineResult);
    procedure AddComments(const APipelineResult: TGocciaSourcePipelineResult);
    procedure AddJSXTokens(const APipelineResult: TGocciaSourcePipelineResult);
    procedure SortByStart;
  public
    function Collect(
      const APipelineResult: TGocciaSourcePipelineResult): TASTFlatTokenArray;
  end;

procedure TASTTokenCollector.Add(const ATokenType: TASTTokenType;
  const AStartOffset, AEndOffset: Integer);
begin
  if FCount = Length(FTokens) then
    if FCount = 0 then
      SetLength(FTokens, INITIAL_TOKEN_CAPACITY)
    else
      SetLength(FTokens, FCount * 2);
  FTokens[FCount].TokenType := ATokenType;
  FTokens[FCount].StartOffset := AStartOffset;
  FTokens[FCount].EndOffset := AEndOffset;
  Inc(FCount);
end;

procedure TASTTokenCollector.AddLexerTokens(
  const APipelineResult: TGocciaSourcePipelineResult);
var
  Tokens: TGocciaSourceTokenArray;
  OriginMap: TGocciaOriginMap;
  TokenType: TASTTokenType;
  StartOffset, EndOffset, OriginalStart, I: Integer;
begin
  Tokens := APipelineResult.Tokens;
  OriginMap := APipelineResult.OriginMap;
  I := 0;
  while I <= High(Tokens) do
  begin
    if Tokens[I].TokenType = gttEOF then
    begin
      Inc(I);
      Continue;
    end;

    TokenType := LexerTokenType(Tokens[I]);
    StartOffset := Tokens[I].StartOffset;
    EndOffset := Tokens[I].EndOffset;

    // The lexer reads the brace that closes a substitution as a token of its
    // own and starts the continuation after it. In the grammar that brace
    // begins the continuation (ES2026 §12.9.6 TemplateMiddle, TemplateTail),
    // and keeping it there means a template's parts and its substitutions'
    // code never share a range. The continuation is scanned from the
    // character after the brace, so the two always touch.
    if (Tokens[I].TokenType = gttRightBrace) and (I < High(Tokens)) and
       (Tokens[I + 1].TokenType in [gttTemplateMiddle, gttTemplateTail]) and
       (Tokens[I + 1].StartOffset = EndOffset) then
    begin
      Inc(I);
      TokenType := LexerTokenType(Tokens[I]);
      EndOffset := Tokens[I].EndOffset;
    end
    // ES2026 §12.7 PrivateIdentifier :: # IdentifierName — one token, which
    // the lexer scans as two.
    else if (Tokens[I].TokenType = gttHash) and (I < High(Tokens)) and
            IsIdentifierNameToken(Tokens[I + 1].TokenType) and
            (Tokens[I + 1].StartOffset = EndOffset) then
    begin
      Inc(I);
      TokenType := atPrivateIdentifier;
      EndOffset := Tokens[I].EndOffset;
    end;
    Inc(I);

    { A preprocessor's output holds text the file does not, so a token is
      reported only when every character of it is a copy of the file's. What
      the JSX transformer synthesized — a `createElement`, the string a text
      child became — is dropped here, and the transformer reports the JSX it
      replaced. }
    if Assigned(OriginMap) then
    begin
      if not OriginMap.MapVerbatim(StartOffset, EndOffset, OriginalStart) then
        Continue;
      EndOffset := OriginalStart + (EndOffset - StartOffset);
      StartOffset := OriginalStart;
    end;
    Add(TokenType, StartOffset, EndOffset);
  end;
end;

procedure TASTTokenCollector.AddComments(
  const APipelineResult: TGocciaSourcePipelineResult);
var
  Comments: TGocciaCommentSpanArray;
  I: Integer;
begin
  Comments := APipelineResult.Comments;
  for I := 0 to High(Comments) do
    Add(CommentTokenType(Comments[I].Kind),
      MappedOffset(APipelineResult.OriginMap, Comments[I].StartOffset, obStart),
      MappedOffset(APipelineResult.OriginMap, Comments[I].EndOffset, obEnd));
end;

procedure TASTTokenCollector.AddJSXTokens(
  const APipelineResult: TGocciaSourcePipelineResult);
var
  JSXTokens: TGocciaJSXTokenArray;
  I: Integer;
begin
  JSXTokens := APipelineResult.JSXTokens;
  for I := 0 to High(JSXTokens) do
    Add(JSXTokenType(JSXTokens[I].Kind), JSXTokens[I].StartOffset,
      JSXTokens[I].EndOffset);
end;

{ Each of the three sources is in source order on its own; together they
  interleave. A stable bottom-up merge sort keeps that cheap whatever the
  interleaving, where an insertion sort would be quadratic on a file that is
  mostly JSX. }
procedure TASTTokenCollector.SortByStart;
var
  Source, Target, Swap: TASTFlatTokenArray;
  Width, Left, Middle, Right, I, J, K: Integer;
begin
  if FCount < 2 then
    Exit;
  Source := FTokens;
  SetLength(Target, FCount);
  Width := 1;
  while Width < FCount do
  begin
    Left := 0;
    while Left < FCount do
    begin
      Middle := Left + Width;
      if Middle > FCount then
        Middle := FCount;
      Right := Left + 2 * Width;
      if Right > FCount then
        Right := FCount;
      I := Left;
      J := Middle;
      K := Left;
      while (I < Middle) and (J < Right) do
      begin
        if Source[J].StartOffset < Source[I].StartOffset then
        begin
          Target[K] := Source[J];
          Inc(J);
        end
        else
        begin
          Target[K] := Source[I];
          Inc(I);
        end;
        Inc(K);
      end;
      while I < Middle do
      begin
        Target[K] := Source[I];
        Inc(I);
        Inc(K);
      end;
      while J < Right do
      begin
        Target[K] := Source[J];
        Inc(J);
        Inc(K);
      end;
      Inc(Left, 2 * Width);
    end;
    Swap := Source;
    Source := Target;
    Target := Swap;
    Width := Width * 2;
  end;
  FTokens := Source;
end;

function TASTTokenCollector.Collect(
  const APipelineResult: TGocciaSourcePipelineResult): TASTFlatTokenArray;
begin
  FCount := 0;
  AddLexerTokens(APipelineResult);
  AddComments(APipelineResult);
  AddJSXTokens(APipelineResult);
  SetLength(FTokens, FCount);
  SortByStart;
  Result := FTokens;
end;

{ One object per token, attached to the array before its properties are made
  so each is rooted through the result while they allocate. The type strings
  are immutable and shared: a file has tens of thousands of tokens and
  sixteen types. }
procedure MaterializeTokens(const ATarget: TGocciaArrayValue;
  const ATokens: TASTFlatTokenArray; const ANumbers: TASTNumberValues);
var
  TypeValues: array[TASTTokenType] of TGocciaStringLiteralValue;
  TokenType: TASTTokenType;
  TokenObject: TGocciaObjectValue;
  I: Integer;
begin
  for TokenType := Low(TASTTokenType) to High(TASTTokenType) do
    TypeValues[TokenType] := nil;

  for I := 0 to High(ATokens) do
  begin
    TokenObject := TGocciaObjectValue.Create(
      TGocciaObjectValue.SharedObjectPrototype, TOKEN_PROPERTY_COUNT);
    AppendElement(ATarget, I, TokenObject);
    TokenType := ATokens[I].TokenType;
    if TypeValues[TokenType] = nil then
      TypeValues[TokenType] :=
        TGocciaStringLiteralValue.Create(TOKEN_TYPE_NAMES[TokenType]);
    // The object was made just above, so each key is a fresh data property
    // and needs none of the checks an assignment makes.
    TokenObject.DefineFreshDataProperty(PROP_TOKEN_TYPE, TypeValues[TokenType],
      [pfEnumerable, pfConfigurable, pfWritable]);
    TokenObject.DefineFreshDataProperty(PROP_TOKEN_START,
      ANumbers.ValueOf(ATokens[I].StartOffset),
      [pfEnumerable, pfConfigurable, pfWritable]);
    TokenObject.DefineFreshDataProperty(PROP_TOKEN_END,
      ANumbers.ValueOf(ATokens[I].EndOffset),
      [pfEnumerable, pfConfigurable, pfWritable]);
  end;
end;

type
  TGocciaASTNamespaceHost = class
  published
    function Parse(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

{ The host carries no state, so every namespace on a thread shares one. It
  lives until thread teardown rather than being released with a namespace: a
  `parse` function the guest kept is bound to it and may still be called.
  Per thread because worker threads materialize the module concurrently. }
threadvar
  GASTHost: TGocciaASTNamespaceHost;

function ReadBooleanOption(const AOptions: TGocciaValue;
  const AName: string; const ADefault: Boolean): Boolean;
var
  Value: TGocciaValue;
begin
  Result := ADefault;
  if not (AOptions is TGocciaObjectValue) then
    Exit;
  Value := TGocciaObjectValue(AOptions).GetProperty(AName);
  if (Value = nil) or (Value is TGocciaUndefinedLiteralValue) then
    Exit;
  Result := Value.ToBooleanLiteral.Value;
end;

function ReadStringOption(const AOptions: TGocciaValue;
  const AName, ADefault: string): string;
var
  Value: TGocciaValue;
begin
  Result := ADefault;
  if not (AOptions is TGocciaObjectValue) then
    Exit;
  Value := TGocciaObjectValue(AOptions).GetProperty(AName);
  if not (Value is TGocciaStringLiteralValue) then
    Exit;
  Result := Value.ToStringLiteral.Value;
end;

function CommentKindName(const AKind: TGocciaCommentKind): string;
begin
  if AKind = gckLine then
    Result := 'Line'
  else
    Result := 'Block';
end;

function TGocciaASTNamespaceHost.Parse(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  SourceText: string;
  SourceLines: TStringList;
  Options: TGocciaSourcePipelineOptions;
  PipelineResult: TGocciaSourcePipelineResult;
  Flattener: TASTFlattener;
  ResultObject: TGocciaObjectValue;
  CommentObject: TGocciaObjectValue;
  Comments: TGocciaArrayValue;
  CommentSpans: TGocciaCommentSpanArray;
  Coordinates: IGocciaSourceCoordinates;
  StartOffset, EndOffset: Integer;
  StartLine, StartColumn, EndLine, EndColumn: Integer;
  ResultRoot: TGocciaTempRoot;
  I, CommentIndex: Integer;
  WantTokens: Boolean;
  TokenCollector: TASTTokenCollector;
  FlatTokens: TASTFlatTokenArray;
  Tokens: TGocciaArrayValue;
  Numbers: TASTNumberValues;
begin
  { Normalized up front so the text handed back is the text the offsets index:
    the pipeline normalizes line terminators before it lexes, and a CRLF file
    reported with its CRs would put every offset after the first line one byte
    out for anything slicing `source`. }
  if AArgs.GetElement(0) is TGocciaStringLiteralValue then
    SourceText := NormalizeNewlinesToLF(AArgs.GetElement(0).ToStringLiteral.Value)
  else
    ThrowTypeError('parse expects the source text as a string',
      'Pass the file contents as a string');

  { The language profile is the host's, not a second one configured here:
    `parse` should accept exactly the JavaScript this engine runs, and the
    compatibility flags that decides are already on the active scope. `eval`
    and the Function constructor inherit them the same way. The options
    argument therefore only says what is about the *text*, not about the
    language. }
  Options := TGocciaSourcePipeline.CurrentOptionsOrDefault;
  Options.CollectComments := True;
  Options.WarningUnsupportedFeatures := False;
  Options.InheritedStrictMode := False;
  if ReadBooleanOption(AArgs.GetElement(1), 'jsx', False) then
    Options.Preprocessors := Options.Preprocessors + [ppJSX]
  else
    Options.Preprocessors := Options.Preprocessors - [ppJSX];
  if ReadBooleanOption(AArgs.GetElement(1), 'module', True) then
    Options.SourceType := stModule
  else
    Options.SourceType := stScript;
  { Opt-in, so a caller that wants only the tree pays nothing for tokens:
    the pipeline copies none and the JSX transformer records none. }
  WantTokens := ReadBooleanOption(AArgs.GetElement(1), OPTION_TOKENS, False);
  Options.CollectTokens := WantTokens;

  { CreateTextLines keeps the exact text alongside the split lines, so the
    string the pipeline parses is character for character the one returned as
    `source` — a plain TStringList round trip loses a trailing newline, and
    every offset would then be into a string the caller never sees. }
  SourceLines := CreateTextLines(SourceText);
  try
    try
      // The file name decides whether the JSX preprocessor warns about the
      // extension, and appears in a syntax error's location.
      PipelineResult := TGocciaSourcePipeline.Parse(SourceLines,
        ReadStringOption(AArgs.GetElement(1), 'fileName', DEFAULT_FILE_NAME),
        Options);
    except
      on E: TGocciaError do
        ThrowSyntaxError(Format('%s (%d:%d)', [E.Message, E.Line, E.Column]));
    end;
  finally
    SourceLines.Free;
  end;

  Numbers := nil;
  InitializeTempRoot(ResultRoot);
  try
    Numbers := TASTNumberValues.Create(Length(SourceText));
    ResultObject := TGocciaObjectValue.Create;
    AddTempRootIfNeeded(ResultRoot, ResultObject);

    { The caller's own text, which is also the one every offset indexes. A
      preprocessor may have rewritten it on the way to the parser, but nothing
      the result carries is measured in that rewrite. }
    ResultObject.AssignProperty('source',
      TGocciaStringLiteralValue.Create(SourceText));

    Comments := TGocciaArrayValue.Create;
    ResultObject.AssignProperty('comments', Comments);

    Coordinates := TGocciaSourceCoordinates.Create(SourceText);

    Flattener := TASTFlattener.Create(PipelineResult.OriginMap,
      ReadBooleanOption(AArgs.GetElement(1), 'expressions', False));
    try
      Flattener.Run(PipelineResult.ProgramNode);
      ResultObject.AssignProperty('root',
        MaterializeNode(Flattener, Coordinates, Numbers, 0));
    finally
      Flattener.Free;
    end;

    CommentSpans := PipelineResult.Comments;
    CommentIndex := 0;
    for I := 0 to High(CommentSpans) do
    begin
      // A hashbang is reported as a token only; `comments` keeps the two
      // kinds it has always had.
      if CommentSpans[I].Kind = gckHashbang then
        Continue;
      CommentObject := TGocciaObjectValue.Create(nil, COMMENT_PROPERTY_COUNT);
      AppendElement(Comments, CommentIndex, CommentObject);
      Inc(CommentIndex);
      StartOffset := MappedOffset(PipelineResult.OriginMap,
        CommentSpans[I].StartOffset, obStart);
      EndOffset := MappedOffset(PipelineResult.OriginMap,
        CommentSpans[I].EndOffset, obEnd);
      Coordinates.PositionAtOffset(StartOffset, StartLine, StartColumn);
      Coordinates.PositionAtOffset(EndOffset, EndLine, EndColumn);
      CommentObject.DefineFreshDataProperty('kind',
        TGocciaStringLiteralValue.Create(CommentKindName(CommentSpans[I].Kind)),
        RESULT_PROPERTY_FLAGS);
      CommentObject.DefineFreshDataProperty('start',
        Numbers.ValueOf(StartOffset), RESULT_PROPERTY_FLAGS);
      CommentObject.DefineFreshDataProperty('end',
        Numbers.ValueOf(EndOffset), RESULT_PROPERTY_FLAGS);
      CommentObject.DefineFreshDataProperty('loc',
        CreateLocObject(Numbers, StartLine, StartColumn, EndLine, EndColumn),
        RESULT_PROPERTY_FLAGS);
    end;

    if WantTokens then
    begin
      TokenCollector := TASTTokenCollector.Create;
      try
        FlatTokens := TokenCollector.Collect(PipelineResult);
      finally
        TokenCollector.Free;
      end;
      Tokens := TGocciaArrayValue.Create;
      ResultObject.AssignProperty(PROP_TOKENS, Tokens);
      MaterializeTokens(Tokens, FlatTokens, Numbers);
    end;

    Result := ResultObject;
  finally
    RemoveTempRootIfNeeded(ResultRoot);
    Numbers.Free;
    PipelineResult.Free;
  end;
end;

function CreateASTNamespace: TGocciaObjectValue;
var
  Members: TGocciaMemberCollection;
begin
  if not Assigned(GASTHost) then
    GASTHost := TGocciaASTNamespaceHost.Create;

  Result := TGocciaObjectValue.Create;
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod('parse', GASTHost.Parse, 2, gmkStaticMethod);
    RegisterMemberDefinitions(Result, Members.ToDefinitions);
  finally
    Members.Free;
  end;
end;

procedure ClearASTHost;
begin
  FreeAndNil(GASTHost);
end;

initialization
  RegisterThreadvarCleanup(@ClearASTHost);

end.
