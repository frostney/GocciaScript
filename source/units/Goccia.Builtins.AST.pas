unit Goccia.Builtins.AST;

{$I Goccia.inc}

{ The `goccia:ast` namespace: one `parse` function that turns source text into
  a JavaScript-visible tree of statement structure.

  What is exposed and why is [ADR 0117](../../docs/adr/0117-javascript-visible-ast-module.md).
  In short: the tree holds statements, the three kinds that own a statement
  list (`Program`, `BlockStatement`, `SwitchCase`), their offsets and
  positions, and comment trivia. It does not hold expressions, names, or
  literal values — a rule that needs those has the offsets and the source
  text the result carries.

  Expressions are still *walked*, because that is the only way to reach the
  block of an arrow function assigned to a `const`. They are walked without
  being emitted. }

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
  Goccia.Lexer,
  Goccia.ObjectModel,
  Goccia.OriginMap,
  Goccia.SourcePipeline,
  Goccia.SourceSpan,
  Goccia.ThreadCleanupRegistry,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives;

const
  // The walker recurses over expression nesting, which a pathological source
  // can make arbitrarily deep. Past this the tree is truncated rather than
  // the worker's stack.
  MAX_WALK_DEPTH = 512;

  DEFAULT_FILE_NAME = '<source>';

  KIND_PROGRAM = 'Program';
  KIND_BLOCK = 'BlockStatement';
  KIND_SWITCH_CASE = 'SwitchCase';

type
  TASTFlatNode = record
    Kind: string;
    StartOffset: Integer;
    EndOffset: Integer;
    StartLine: Integer;
    StartColumn: Integer;
    EndLine: Integer;
    EndColumn: Integer;
    FirstChild: Integer;
    LastChild: Integer;
    NextSibling: Integer;
  end;

  { Flattens a TGocciaProgram into the node records the JavaScript view is
    built from. Emitting is separated from materializing so the whole walk
    runs without allocating a single managed value — nothing can be collected
    halfway through a traversal that has no roots yet. }
  TASTFlattener = class
  private
    FNodes: array of TASTFlatNode;
    FCount: Integer;
    FParent: Integer;
    FDepth: Integer;
    FOriginMap: TGocciaOriginMap;
    FCoordinates: IGocciaSourceCoordinates;
    { A class body reaches the same method through more than one of the
      definition's maps, and the unified element array overlaps them again.
      Walking all of them and refusing to emit a node twice is more robust
      than picking one path and being wrong when the parser fills another. }
    FSeen: TDictionary<Pointer, Boolean>;

    function MapOffset(const AOffset: Integer;
      const ABias: TGocciaOriginBias): Integer;
    function Emit(const AKind: string;
      const ASpan: TGocciaSourceSpan): Integer;
    procedure Push(const AIndex: Integer);
    procedure Pop(const APrevious: Integer);
    function MarkSeen(const ANode: TObject): Boolean;

    procedure VisitStatement(const AStatement: TGocciaStatement);
    procedure VisitNode(const ANode: TGocciaASTNode);
    procedure VisitExpression(const AExpression: TGocciaExpression);
    procedure VisitExpressionList(const AList: TObjectList<TGocciaExpression>);
    procedure VisitClassDefinition(const ADefinition: TGocciaClassDefinition);
    procedure VisitObjectExpression(const AObject: TGocciaObjectExpression);
    procedure VisitParameters(const AParameters: TGocciaParameterArray);
    procedure VisitVariables(const AVariables: TArray<TGocciaVariableInfo>);
    procedure VisitFunctionBody(const ABody: TGocciaASTNode);
  public
    constructor Create(const AOriginMap: TGocciaOriginMap;
      const ACoordinates: IGocciaSourceCoordinates);
    destructor Destroy; override;
    procedure Run(const AProgram: TGocciaProgram);
    function NodeAt(const AIndex: Integer): TASTFlatNode;
    property Count: Integer read FCount;
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
  else if AStatement is TGocciaEmptyStatement then Result := 'EmptyStatement'
  else Result := 'Statement';
end;

{ TASTFlattener }

constructor TASTFlattener.Create(const AOriginMap: TGocciaOriginMap;
  const ACoordinates: IGocciaSourceCoordinates);
begin
  inherited Create;
  FOriginMap := AOriginMap;
  FCoordinates := ACoordinates;
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
  const ABias: TGocciaOriginBias): Integer;
begin
  if not Assigned(FOriginMap) then
    Exit(AOffset);
  FOriginMap.Map(AOffset, ABias, Result);
end;

function TASTFlattener.Emit(const AKind: string;
  const ASpan: TGocciaSourceSpan): Integer;
var
  Previous, Sibling: Integer;
begin
  if FCount = Length(FNodes) then
    if FCount = 0 then
      SetLength(FNodes, 256)
    else
      SetLength(FNodes, FCount * 2);

  Result := FCount;
  Inc(FCount);

  FNodes[Result].Kind := AKind;
  { The parser measured this node in whatever text it was handed. Everything
    the result carries is measured in the file the caller passed, so the two
    offsets are mapped back here, once, and the line and column are then read
    off the original — deriving them rather than translating them separately
    is what keeps `loc` and the range the same answer. }
  FNodes[Result].StartOffset := MapOffset(ASpan.StartOffset, obStart);
  FNodes[Result].EndOffset := MapOffset(ASpan.EndOffset, obEnd);
  FCoordinates.PositionAtOffset(FNodes[Result].StartOffset,
    FNodes[Result].StartLine, FNodes[Result].StartColumn);
  FCoordinates.PositionAtOffset(FNodes[Result].EndOffset,
    FNodes[Result].EndLine, FNodes[Result].EndColumn);
  FNodes[Result].FirstChild := -1;
  FNodes[Result].LastChild := -1;
  FNodes[Result].NextSibling := -1;

  { Children are kept in source order, which for the three kinds that own a
    statement list is also the order the walk produces them in. It is not for
    a class: its elements are reached through several per-kind maps as well as
    the element array, and whichever one a given member turns up in first
    decides when it is emitted. Inserting by position costs a short scan over
    a sibling list that is nearly always already sorted, and spares every
    consumer from having to sort. }
  if FParent >= 0 then
  begin
    Previous := -1;
    Sibling := FNodes[FParent].FirstChild;
    while (Sibling >= 0) and
          (FNodes[Sibling].StartOffset <= FNodes[Result].StartOffset) do
    begin
      Previous := Sibling;
      Sibling := FNodes[Sibling].NextSibling;
    end;

    FNodes[Result].NextSibling := Sibling;
    if Previous < 0 then
      FNodes[FParent].FirstChild := Result
    else
      FNodes[Previous].NextSibling := Result;
    if Sibling < 0 then
      FNodes[FParent].LastChild := Result;
  end;
end;

function TASTFlattener.NodeAt(const AIndex: Integer): TASTFlatNode;
begin
  Result := FNodes[AIndex];
end;

procedure TASTFlattener.Push(const AIndex: Integer);
begin
  FParent := AIndex;
end;

procedure TASTFlattener.Pop(const APrevious: Integer);
begin
  FParent := APrevious;
end;

procedure TASTFlattener.VisitNode(const ANode: TGocciaASTNode);
begin
  if not Assigned(ANode) then
    Exit;
  if ANode is TGocciaStatement then
    VisitStatement(TGocciaStatement(ANode))
  else if ANode is TGocciaExpression then
    VisitExpression(TGocciaExpression(ANode));
end;

procedure TASTFlattener.VisitFunctionBody(const ABody: TGocciaASTNode);
begin
  // A concise arrow body is an expression, not a block.
  VisitNode(ABody);
end;

procedure TASTFlattener.VisitExpressionList(
  const AList: TObjectList<TGocciaExpression>);
var
  Item: TGocciaExpression;
begin
  if not Assigned(AList) then
    Exit;
  for Item in AList do
    VisitExpression(Item);
end;

procedure TASTFlattener.VisitParameters(
  const AParameters: TGocciaParameterArray);
var
  I: Integer;
begin
  for I := 0 to High(AParameters) do
  begin
    VisitExpression(AParameters[I].Pattern);
    VisitExpression(AParameters[I].DefaultValue);
  end;
end;

procedure TASTFlattener.VisitVariables(
  const AVariables: TArray<TGocciaVariableInfo>);
var
  I: Integer;
begin
  for I := 0 to High(AVariables) do
  begin
    VisitExpression(AVariables[I].Pattern);
    VisitExpression(AVariables[I].Initializer);
  end;
end;

procedure TASTFlattener.VisitObjectExpression(
  const AObject: TGocciaObjectExpression);
var
  I: Integer;
  Order: TGocciaPropertySourceOrder;
begin
  for I := 0 to High(AObject.PropertySourceOrder) do
  begin
    Order := AObject.PropertySourceOrder[I];
    VisitExpression(Order.Expression);
    if (Order.ComputedIndex >= 0) and
       (Order.ComputedIndex <= High(AObject.ComputedPropertiesInOrder)) then
    begin
      VisitExpression(AObject.ComputedPropertiesInOrder[Order.ComputedIndex].Key);
      VisitExpression(AObject.ComputedPropertiesInOrder[Order.ComputedIndex].Value);
    end;
  end;

  // The source-order table is the authority, but it is built by the parser
  // and the maps are what the evaluator reads; walking both and deduplicating
  // means a shape the table does not describe still gets its body walked.
  for I := 0 to AObject.Properties.CountFast - 1 do
    VisitExpression(AObject.Properties.EntryAt(I).Value);
  for I := 0 to AObject.Getters.CountFast - 1 do
    VisitExpression(AObject.Getters.EntryAt(I).Value);
  for I := 0 to AObject.Setters.CountFast - 1 do
    VisitExpression(AObject.Setters.EntryAt(I).Value);
end;

procedure TASTFlattener.VisitClassDefinition(
  const ADefinition: TGocciaClassDefinition);
var
  I: Integer;
  Element: TGocciaClassElement;
begin
  if not Assigned(ADefinition) then
    Exit;

  VisitExpression(ADefinition.SuperClassExpression);

  { The element array first, because it is the only one of these in source
    order — the per-kind maps below group by kind, and walking them first put
    a class's children in an order that has nothing to do with the file. The
    maps still run, because a parser that fills one without the other should
    lose a body rather than have this quietly decide which is canonical. }
  for I := 0 to High(ADefinition.FElements) do
  begin
    Element := ADefinition.FElements[I];
    VisitExpression(Element.ComputedKeyExpression);
    VisitExpression(Element.MethodNode);
    VisitExpression(Element.GetterNode);
    VisitExpression(Element.SetterNode);
    VisitExpression(Element.FieldInitializer);
    VisitStatement(Element.StaticBlockBody);
  end;

  for I := 0 to ADefinition.Methods.CountFast - 1 do
    VisitExpression(ADefinition.Methods.EntryAt(I).Value);
  for I := 0 to ADefinition.StaticMethods.CountFast - 1 do
    VisitExpression(ADefinition.StaticMethods.EntryAt(I).Value);
  for I := 0 to ADefinition.PrivateMethods.CountFast - 1 do
    VisitExpression(ADefinition.PrivateMethods.EntryAt(I).Value);
  for I := 0 to ADefinition.Getters.CountFast - 1 do
    VisitExpression(ADefinition.Getters.EntryAt(I).Value);
  for I := 0 to ADefinition.Setters.CountFast - 1 do
    VisitExpression(ADefinition.Setters.EntryAt(I).Value);
  for I := 0 to ADefinition.StaticGetters.CountFast - 1 do
    VisitExpression(ADefinition.StaticGetters.EntryAt(I).Value);
  for I := 0 to ADefinition.StaticSetters.CountFast - 1 do
    VisitExpression(ADefinition.StaticSetters.EntryAt(I).Value);
  for I := 0 to ADefinition.StaticProperties.CountFast - 1 do
    VisitExpression(ADefinition.StaticProperties.EntryAt(I).Value);
  for I := 0 to ADefinition.InstanceProperties.CountFast - 1 do
    VisitExpression(ADefinition.InstanceProperties.EntryAt(I).Value);
  for I := 0 to ADefinition.PrivateInstanceProperties.CountFast - 1 do
    VisitExpression(ADefinition.PrivateInstanceProperties.EntryAt(I).Value);
  for I := 0 to ADefinition.PrivateStaticProperties.CountFast - 1 do
    VisitExpression(ADefinition.PrivateStaticProperties.EntryAt(I).Value);

  for I := 0 to High(ADefinition.Decorators) do
    VisitExpression(ADefinition.Decorators[I]);
end;

procedure TASTFlattener.VisitExpression(const AExpression: TGocciaExpression);
var
  I: Integer;
begin
  if not Assigned(AExpression) then
    Exit;
  if FDepth >= MAX_WALK_DEPTH then
    Exit;
  if not MarkSeen(AExpression) then
    Exit;

  Inc(FDepth);
  try
    // The function-ish kinds: the only expressions that own a statement list.
    if AExpression is TGocciaArrowFunctionExpression then
    begin
      VisitParameters(TGocciaArrowFunctionExpression(AExpression).Parameters);
      VisitFunctionBody(TGocciaArrowFunctionExpression(AExpression).Body);
    end
    else if AExpression is TGocciaFunctionExpression then
    begin
      VisitParameters(TGocciaFunctionExpression(AExpression).Parameters);
      VisitFunctionBody(TGocciaFunctionExpression(AExpression).Body);
    end
    else if AExpression is TGocciaClassMethod then
      VisitFunctionBody(TGocciaClassMethod(AExpression).Body)
    else if AExpression is TGocciaGetterExpression then
      VisitFunctionBody(TGocciaGetterExpression(AExpression).Body)
    else if AExpression is TGocciaSetterExpression then
    begin
      VisitParameters(TGocciaSetterExpression(AExpression).Parameters);
      VisitFunctionBody(TGocciaSetterExpression(AExpression).Body);
    end
    else if AExpression is TGocciaObjectMethodDefinition then
      VisitExpression(TGocciaObjectMethodDefinition(AExpression).FunctionExpression)
    else if AExpression is TGocciaClassExpression then
      VisitClassDefinition(TGocciaClassExpression(AExpression).ClassDefinition)
    else if AExpression is TGocciaObjectExpression then
      VisitObjectExpression(TGocciaObjectExpression(AExpression))

    // Everything below owns no statement list, and is walked only to reach
    // one of the kinds above.
    else if AExpression is TGocciaBinaryExpression then
    begin
      VisitExpression(TGocciaBinaryExpression(AExpression).Left);
      VisitExpression(TGocciaBinaryExpression(AExpression).Right);
    end
    else if AExpression is TGocciaUnaryExpression then
      VisitExpression(TGocciaUnaryExpression(AExpression).Operand)
    else if AExpression is TGocciaSequenceExpression then
      VisitExpressionList(TGocciaSequenceExpression(AExpression).Expressions)
    else if AExpression is TGocciaConditionalExpression then
    begin
      VisitExpression(TGocciaConditionalExpression(AExpression).Condition);
      VisitExpression(TGocciaConditionalExpression(AExpression).Consequent);
      VisitExpression(TGocciaConditionalExpression(AExpression).Alternate);
    end
    else if AExpression is TGocciaCallExpression then
    begin
      VisitExpression(TGocciaCallExpression(AExpression).Callee);
      VisitExpressionList(TGocciaCallExpression(AExpression).Arguments);
    end
    else if AExpression is TGocciaNewExpression then
    begin
      VisitExpression(TGocciaNewExpression(AExpression).Callee);
      VisitExpressionList(TGocciaNewExpression(AExpression).Arguments);
    end
    else if AExpression is TGocciaMemberExpression then
    begin
      VisitExpression(TGocciaMemberExpression(AExpression).ObjectExpr);
      VisitExpression(TGocciaMemberExpression(AExpression).PropertyExpression);
    end
    else if AExpression is TGocciaArrayExpression then
      VisitExpressionList(TGocciaArrayExpression(AExpression).Elements)
    else if AExpression is TGocciaSpreadExpression then
      VisitExpression(TGocciaSpreadExpression(AExpression).Argument)
    else if AExpression is TGocciaAwaitExpression then
      VisitExpression(TGocciaAwaitExpression(AExpression).Operand)
    else if AExpression is TGocciaYieldExpression then
      VisitExpression(TGocciaYieldExpression(AExpression).Operand)
    else if AExpression is TGocciaAssignmentExpression then
      VisitExpression(TGocciaAssignmentExpression(AExpression).Value)
    else if AExpression is TGocciaCompoundAssignmentExpression then
      VisitExpression(TGocciaCompoundAssignmentExpression(AExpression).Value)
    else if AExpression is TGocciaPropertyAssignmentExpression then
    begin
      VisitExpression(TGocciaPropertyAssignmentExpression(AExpression).ObjectExpr);
      VisitExpression(TGocciaPropertyAssignmentExpression(AExpression).Value);
    end
    else if AExpression is TGocciaPropertyCompoundAssignmentExpression then
    begin
      VisitExpression(TGocciaPropertyCompoundAssignmentExpression(AExpression).ObjectExpr);
      VisitExpression(TGocciaPropertyCompoundAssignmentExpression(AExpression).Value);
    end
    else if AExpression is TGocciaComputedPropertyAssignmentExpression then
    begin
      VisitExpression(TGocciaComputedPropertyAssignmentExpression(AExpression).ObjectExpr);
      VisitExpression(TGocciaComputedPropertyAssignmentExpression(AExpression).PropertyExpression);
      VisitExpression(TGocciaComputedPropertyAssignmentExpression(AExpression).Value);
    end
    else if AExpression is TGocciaComputedPropertyCompoundAssignmentExpression then
    begin
      VisitExpression(TGocciaComputedPropertyCompoundAssignmentExpression(AExpression).ObjectExpr);
      VisitExpression(TGocciaComputedPropertyCompoundAssignmentExpression(AExpression).PropertyExpression);
      VisitExpression(TGocciaComputedPropertyCompoundAssignmentExpression(AExpression).Value);
    end
    else if AExpression is TGocciaPrivateMemberExpression then
      VisitExpression(TGocciaPrivateMemberExpression(AExpression).ObjectExpr)
    else if AExpression is TGocciaPrivatePropertyAssignmentExpression then
    begin
      VisitExpression(TGocciaPrivatePropertyAssignmentExpression(AExpression).ObjectExpr);
      VisitExpression(TGocciaPrivatePropertyAssignmentExpression(AExpression).Value);
    end
    else if AExpression is TGocciaPrivatePropertyCompoundAssignmentExpression then
    begin
      VisitExpression(TGocciaPrivatePropertyCompoundAssignmentExpression(AExpression).ObjectExpr);
      VisitExpression(TGocciaPrivatePropertyCompoundAssignmentExpression(AExpression).Value);
    end
    else if AExpression is TGocciaIncrementExpression then
      VisitExpression(TGocciaIncrementExpression(AExpression).Operand)
    else if AExpression is TGocciaTemplateWithInterpolationExpression then
      VisitExpressionList(TGocciaTemplateWithInterpolationExpression(AExpression).Parts)
    else if AExpression is TGocciaTaggedTemplateExpression then
    begin
      VisitExpression(TGocciaTaggedTemplateExpression(AExpression).Tag);
      VisitExpressionList(TGocciaTaggedTemplateExpression(AExpression).Expressions);
    end
    else if AExpression is TGocciaImportCallExpression then
    begin
      VisitExpression(TGocciaImportCallExpression(AExpression).Specifier);
      VisitExpression(TGocciaImportCallExpression(AExpression).Options);
    end
    else if AExpression is TGocciaDestructuringAssignmentExpression then
    begin
      VisitExpression(TGocciaDestructuringAssignmentExpression(AExpression).Left);
      VisitExpression(TGocciaDestructuringAssignmentExpression(AExpression).Right);
    end
    else if AExpression is TGocciaIsExpression then
      VisitExpression(TGocciaIsExpression(AExpression).Subject)
    else if AExpression is TGocciaMatchExpression then
    begin
      VisitExpression(TGocciaMatchExpression(AExpression).Subject);
      VisitExpression(TGocciaMatchExpression(AExpression).DefaultExpression);
      if Assigned(TGocciaMatchExpression(AExpression).Clauses) then
        for I := 0 to TGocciaMatchExpression(AExpression).Clauses.Count - 1 do
          VisitExpression(
            TGocciaMatchExpression(AExpression).Clauses[I].Expression);
    end

    // Destructuring patterns are expressions, and may carry defaults that in
    // turn carry functions.
    else if AExpression is TGocciaArrayDestructuringPattern then
    begin
      for I := 0 to TGocciaArrayDestructuringPattern(AExpression).Elements.Count - 1 do
        VisitExpression(TGocciaArrayDestructuringPattern(AExpression).Elements[I]);
    end
    else if AExpression is TGocciaObjectDestructuringPattern then
    begin
      for I := 0 to TGocciaObjectDestructuringPattern(AExpression).Properties.Count - 1 do
      begin
        VisitExpression(
          TGocciaObjectDestructuringPattern(AExpression).Properties[I].Pattern);
        VisitExpression(
          TGocciaObjectDestructuringPattern(AExpression).Properties[I].KeyExpression);
      end;
    end
    else if AExpression is TGocciaRestDestructuringPattern then
      VisitExpression(TGocciaRestDestructuringPattern(AExpression).Argument)
    else if AExpression is TGocciaAssignmentDestructuringPattern then
    begin
      VisitExpression(TGocciaAssignmentDestructuringPattern(AExpression).Left);
      VisitExpression(TGocciaAssignmentDestructuringPattern(AExpression).Right);
    end
    else if AExpression is TGocciaMemberExpressionDestructuringPattern then
      VisitExpression(TGocciaMemberExpressionDestructuringPattern(AExpression).Expression)
    else if AExpression is TGocciaPrivateMemberExpressionDestructuringPattern then
      VisitExpression(TGocciaPrivateMemberExpressionDestructuringPattern(AExpression).Expression);
  finally
    Dec(FDepth);
  end;
end;

procedure TASTFlattener.VisitStatement(const AStatement: TGocciaStatement);
var
  SavedParent, Index, CaseIndex, I: Integer;
  Child: TGocciaASTNode;
  Clause: TGocciaCaseClause;
begin
  if not Assigned(AStatement) then
    Exit;
  if FDepth >= MAX_WALK_DEPTH then
    Exit;
  if not MarkSeen(AStatement) then
    Exit;

  Inc(FDepth);
  Index := Emit(StatementKind(AStatement), AStatement.Span);
  SavedParent := FParent;
  Push(Index);
  try
    if AStatement is TGocciaBlockStatement then
    begin
      for Child in TGocciaBlockStatement(AStatement).Nodes do
        VisitNode(Child);
    end
    else if AStatement is TGocciaExpressionStatement then
      VisitExpression(TGocciaExpressionStatement(AStatement).Expression)
    else if AStatement is TGocciaVariableDeclaration then
      VisitVariables(TGocciaVariableDeclaration(AStatement).Variables)
    else if AStatement is TGocciaUsingDeclaration then
      VisitVariables(TGocciaUsingDeclaration(AStatement).Variables)
    else if AStatement is TGocciaDestructuringDeclaration then
    begin
      VisitExpression(TGocciaDestructuringDeclaration(AStatement).Pattern);
      VisitExpression(TGocciaDestructuringDeclaration(AStatement).Initializer);
    end
    else if AStatement is TGocciaFunctionDeclaration then
      VisitExpression(TGocciaFunctionDeclaration(AStatement).FunctionExpression)
    else if AStatement is TGocciaIfStatement then
    begin
      VisitExpression(TGocciaIfStatement(AStatement).Condition);
      VisitStatement(TGocciaIfStatement(AStatement).Consequent);
      VisitStatement(TGocciaIfStatement(AStatement).Alternate);
    end
    else if AStatement is TGocciaForStatement then
    begin
      VisitStatement(TGocciaForStatement(AStatement).Init);
      VisitExpression(TGocciaForStatement(AStatement).Condition);
      VisitExpression(TGocciaForStatement(AStatement).Update);
      VisitStatement(TGocciaForStatement(AStatement).Body);
    end
    else if AStatement is TGocciaWhileStatement then
    begin
      VisitExpression(TGocciaWhileStatement(AStatement).Condition);
      VisitStatement(TGocciaWhileStatement(AStatement).Body);
    end
    else if AStatement is TGocciaDoWhileStatement then
    begin
      VisitStatement(TGocciaDoWhileStatement(AStatement).Body);
      VisitExpression(TGocciaDoWhileStatement(AStatement).Condition);
    end
    else if AStatement is TGocciaWithStatement then
    begin
      VisitExpression(TGocciaWithStatement(AStatement).ObjectExpression);
      VisitStatement(TGocciaWithStatement(AStatement).Body);
    end
    else if AStatement is TGocciaForOfStatement then
    begin
      VisitExpression(TGocciaForOfStatement(AStatement).BindingPattern);
      VisitExpression(TGocciaForOfStatement(AStatement).AssignmentTarget);
      VisitExpression(TGocciaForOfStatement(AStatement).Iterable);
      VisitStatement(TGocciaForOfStatement(AStatement).Body);
    end
    else if AStatement is TGocciaForInStatement then
    begin
      VisitExpression(TGocciaForInStatement(AStatement).BindingPattern);
      VisitExpression(TGocciaForInStatement(AStatement).AssignmentTarget);
      VisitExpression(TGocciaForInStatement(AStatement).ObjectExpression);
      VisitStatement(TGocciaForInStatement(AStatement).Body);
    end
    else if AStatement is TGocciaReturnStatement then
      VisitExpression(TGocciaReturnStatement(AStatement).Value)
    else if AStatement is TGocciaThrowStatement then
      VisitExpression(TGocciaThrowStatement(AStatement).Value)
    else if AStatement is TGocciaTryStatement then
    begin
      VisitStatement(TGocciaTryStatement(AStatement).Block);
      VisitExpression(TGocciaTryStatement(AStatement).CatchBindingPattern);
      VisitStatement(TGocciaTryStatement(AStatement).CatchBlock);
      VisitStatement(TGocciaTryStatement(AStatement).FinallyBlock);
    end
    else if AStatement is TGocciaClassDeclaration then
      VisitClassDefinition(TGocciaClassDeclaration(AStatement).ClassDefinition)
    else if AStatement is TGocciaSwitchStatement then
    begin
      VisitExpression(TGocciaSwitchStatement(AStatement).Discriminant);
      for Clause in TGocciaSwitchStatement(AStatement).Cases do
      begin
        VisitExpression(Clause.Test);
        CaseIndex := Emit(KIND_SWITCH_CASE, Clause.Span);
        Push(CaseIndex);
        try
          for I := 0 to Clause.Consequent.Count - 1 do
            VisitStatement(Clause.Consequent[I]);
        finally
          Pop(Index);
        end;
      end;
    end
    else if AStatement is TGocciaEnumDeclaration then
    begin
      for I := 0 to High(TGocciaEnumDeclaration(AStatement).Members) do
        VisitExpression(TGocciaEnumDeclaration(AStatement).Members[I].Initializer);
    end
    else if AStatement is TGocciaExportEnumDeclaration then
      VisitStatement(TGocciaExportEnumDeclaration(AStatement).Declaration)
    else if AStatement is TGocciaExportDefaultDeclaration then
      VisitExpression(TGocciaExportDefaultDeclaration(AStatement).Expression)
    else if AStatement is TGocciaExportVariableDeclaration then
      VisitVariables(
        TGocciaExportVariableDeclaration(AStatement).Declaration.Variables)
    else if AStatement is TGocciaExportDestructuringDeclaration then
    begin
      VisitExpression(
        TGocciaExportDestructuringDeclaration(AStatement).Declaration.Pattern);
      VisitExpression(
        TGocciaExportDestructuringDeclaration(AStatement).Declaration.Initializer);
    end
    else if AStatement is TGocciaExportFunctionDeclaration then
      VisitExpression(TGocciaExportFunctionDeclaration(AStatement)
        .Declaration.FunctionExpression)
    else if AStatement is TGocciaExportClassDeclaration then
      VisitClassDefinition(TGocciaExportClassDeclaration(AStatement)
        .Declaration.ClassDefinition);
  finally
    Pop(SavedParent);
    Dec(FDepth);
  end;
end;

procedure TASTFlattener.Run(const AProgram: TGocciaProgram);
var
  Index, I: Integer;
begin
  Index := Emit(KIND_PROGRAM, AProgram.Span);
  Push(Index);
  for I := 0 to AProgram.Body.Count - 1 do
    VisitStatement(AProgram.Body[I]);
  Pop(-1);
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

function CreatePositionObject(const ALine, AColumn: Integer): TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create;
  Result.AssignProperty('line', TGocciaNumberLiteralValue.Create(ALine));
  Result.AssignProperty('column', TGocciaNumberLiteralValue.Create(AColumn));
end;

function CreateLocObject(const AStartLine, AStartColumn, AEndLine,
  AEndColumn: Integer): TGocciaObjectValue;
var
  Position: TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create;
  Position := CreatePositionObject(AStartLine, AStartColumn);
  Result.AssignProperty('start', Position);
  Position := CreatePositionObject(AEndLine, AEndColumn);
  Result.AssignProperty('end', Position);
end;

{ Nodes are materialized parent-first and attached to the parent's children
  array before their own properties are filled, so a collection triggered
  anywhere inside this walk still finds every object already built reachable
  from the rooted result. }
function MaterializeNode(const AFlattener: TASTFlattener;
  const AIndex: Integer): TGocciaObjectValue;
var
  Children: TGocciaArrayValue;
  Node: TASTFlatNode;
  ChildIndex, Position: Integer;
begin
  Node := AFlattener.NodeAt(AIndex);
  Result := TGocciaObjectValue.Create;
  Children := TGocciaArrayValue.Create;
  Result.AssignProperty('children', Children);
  Result.AssignProperty('kind', TGocciaStringLiteralValue.Create(Node.Kind));
  Result.AssignProperty('start',
    TGocciaNumberLiteralValue.Create(Node.StartOffset));
  Result.AssignProperty('end',
    TGocciaNumberLiteralValue.Create(Node.EndOffset));
  Result.AssignProperty('loc', CreateLocObject(Node.StartLine,
    Node.StartColumn, Node.EndLine, Node.EndColumn));

  Position := 0;
  ChildIndex := Node.FirstChild;
  while ChildIndex >= 0 do
  begin
    Children.SetElement(Position, MaterializeNode(AFlattener, ChildIndex));
    Inc(Position);
    ChildIndex := AFlattener.NodeAt(ChildIndex).NextSibling;
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
  I: Integer;
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

  InitializeTempRoot(ResultRoot);
  try
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

    Flattener := TASTFlattener.Create(PipelineResult.OriginMap, Coordinates);
    try
      Flattener.Run(PipelineResult.ProgramNode);
      ResultObject.AssignProperty('root', MaterializeNode(Flattener, 0));
    finally
      Flattener.Free;
    end;

    CommentSpans := PipelineResult.Comments;
    for I := 0 to High(CommentSpans) do
    begin
      CommentObject := TGocciaObjectValue.Create;
      Comments.SetElement(I, CommentObject);
      StartOffset := MappedOffset(PipelineResult.OriginMap,
        CommentSpans[I].StartOffset, obStart);
      EndOffset := MappedOffset(PipelineResult.OriginMap,
        CommentSpans[I].EndOffset, obEnd);
      Coordinates.PositionAtOffset(StartOffset, StartLine, StartColumn);
      Coordinates.PositionAtOffset(EndOffset, EndLine, EndColumn);
      CommentObject.AssignProperty('kind',
        TGocciaStringLiteralValue.Create(CommentKindName(CommentSpans[I].Kind)));
      CommentObject.AssignProperty('start',
        TGocciaNumberLiteralValue.Create(StartOffset));
      CommentObject.AssignProperty('end',
        TGocciaNumberLiteralValue.Create(EndOffset));
      CommentObject.AssignProperty('loc',
        CreateLocObject(StartLine, StartColumn, EndLine, EndColumn));
    end;

    Result := ResultObject;
  finally
    RemoveTempRootIfNeeded(ResultRoot);
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
