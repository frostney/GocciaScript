unit Goccia.Eval.EarlyErrors;

{$I Goccia.inc}

{ The static checks eval code gets before any of it runs: the early errors
  ES2026 §19.2.1.1 PerformEval step 11 adds for the eval call (`new.target`,
  `super` and `arguments` where the caller forbids them), the Script early
  errors of §16.1.1 that the parser leaves to the eval call (module and `using`
  declarations, a top-level `return`, `break` or `continue`), the strict-mode
  assignment restrictions on `eval` and `arguments`, and the name lists
  §19.2.1.3 EvalDeclarationInstantiation reads (VarDeclaredNames,
  LexicallyDeclaredNames, the top-level function declarations).

  They are AST passes that run before the eval code is compiled. }

interface

uses
  Generics.Collections,

  UnicodeStringList,

  Goccia.AST.BindingPatterns,
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements;

procedure AddUniqueEvalName(const ANames: TUnicodeStringList;
  const AName: string);
function EvalFunctionDeclarationFromNode(
  const ANode: TGocciaASTNode): TGocciaFunctionDeclaration;
procedure CollectTopLevelEvalLexicalNames(
  const ANodes: TObjectList<TGocciaStatement>;
  const ANames: TUnicodeStringList);

{ The names ES2026 §19.2.1.3 EvalDeclarationInstantiation binds when the
  variable environment is the global one. AVarNames is VarDeclaredNames of
  the body (step 1; with AMode = vbnNonStrictScriptCompatibility it also holds
  the block-level functions that get a var binding, ES2026 B.3.2 and
  §19.2.1.3 step 13). AFunctionNames
  are the names of the top-level function declarations, the last declaration
  of a name winning (functionsToInitialize, steps 8 and 10), and
  ADeclaredVarNames the other var names (declaredVarNames, steps 11-12), each once and in source order. }
procedure CollectGlobalEvalDeclarationNames(const AProgram: TGocciaProgram;
  const AMode: TGocciaVarBindingNameCollectionMode;
  const AVarNames, AFunctionNames, ADeclaredVarNames: TUnicodeStringList);
{ ES2026 §16.1.1 Static Semantics: Early Errors for ScriptBody: a duplicate
  name among the top-level LexicallyDeclaredNames, or a name both lexically
  declared and var-declared at the top level, is a SyntaxError. The parser
  leaves both to the eval call. AVarNames is VarDeclaredNames of the body, as
  CollectGlobalEvalDeclarationNames collects it. Raises TGocciaSyntaxError. }
procedure CheckEvalLexicalDeclarationConflicts(const AProgram: TGocciaProgram;
  const AVarNames: TUnicodeStringList);

procedure ValidateEvalScriptBody(const ANodes: TObjectList<TGocciaStatement>);
function EvalProgramContainsArgumentsReference(
  const AProgram: TGocciaProgram): Boolean;

{ Runs the eval Script's static early-error checks for forbidden references and
  assignments — `new.target` / `super` outside a permitting context and, under
  strict eval, assignment to `eval`/`arguments`. It does NOT cover
  EvalDeclarationInstantiation conflicts (e.g. duplicate lexical bindings).
  Raises a TGocciaSyntaxError on the first violation. Exposed so callers that
  must distinguish these early errors (SyntaxError) from runtime abrupt
  completions (TypeError) can validate up front. }
procedure ValidateEvalEarlyErrors(const AProgram: TGocciaProgram;
  const AStrictEval, AAllowNewTarget, AAllowSuperProperty,
  AAllowSuperCall: Boolean);
procedure ValidateStrictFunctionExpression(
  const AFunction: TGocciaFunctionExpression);
procedure ValidateStrictFunctionEarlyErrors(
  const AFunction: TGocciaFunctionExpression);
procedure ValidateEvalControlFlow(const AProgram: TGocciaProgram);
{ ES2026 §14.2.1 Block and §14.12.1 CaseBlock Static Semantics: Early Errors:
  a block's LexicallyDeclaredNames, its let, const, class and function
  declarations, may not repeat a name, except that non-strict code may
  declare a function twice (B.3.2.4). Checks every block of eval code, nested
  function bodies included, before the code is compiled. Raises
  TGocciaSyntaxError. }
procedure ValidateEvalBlockDeclarations(const AProgram: TGocciaProgram;
  const AStrictEval: Boolean);

implementation

uses
  SysUtils,

  Goccia.Constants,
  Goccia.Error,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Keywords.Reserved,
  Goccia.NestingLimit,
  Goccia.Token,
  Goccia.Values.ErrorHelper;

{ The passes below recurse once per level of the tree, and a member or call
  chain costs the parser no stack, so on an already deep native stack (eval
  reached from deep recursion) a long chain could run them off its end. They
  stop with the SyntaxError the parser and the compiler give when less than
  NATIVE_STACK_RESERVE of the stack is left. }
procedure RaiseEarlyErrorStackExhausted(const ANode: TGocciaASTNode);
var
  Line, Column: Integer;
begin
  Line := 0;
  Column := 0;
  if Assigned(ANode) then
  begin
    Line := ANode.Line;
    Column := ANode.Column;
  end;
  raise TGocciaSyntaxError.Create(SErrorNestingStackExhausted, Line, Column,
    '', nil, SSuggestNestingLimitExceeded);
end;

procedure CheckEarlyErrorStack(const ANode: TGocciaASTNode); {$IFDEF FPC}inline;{$ENDIF}
begin
  if NativeStackNearlyExhausted then
    RaiseEarlyErrorStackExhausted(ANode);
end;

procedure AddUniqueEvalName(const ANames: TUnicodeStringList;
  const AName: string);
begin
  if (AName <> '') and (not ANames.HasString(AName)) then
    ANames.Add(AName);
end;

function EvalFunctionDeclarationFromNode(
  const ANode: TGocciaASTNode): TGocciaFunctionDeclaration;
begin
  if ANode is TGocciaFunctionDeclaration then
    Result := TGocciaFunctionDeclaration(ANode)
  else if ANode is TGocciaExportFunctionDeclaration then
    Result := TGocciaExportFunctionDeclaration(ANode).Declaration
  else
    Result := nil;
end;

// ValidateEvalScriptBody has already rejected a top-level using declaration,
// so CollectLexicallyDeclaredNames finds none here.
procedure CollectTopLevelEvalLexicalNames(const ANodes: TObjectList<TGocciaStatement>;
  const ANames: TUnicodeStringList);
var
  Names: TUnicodeStringList;
  I, J: Integer;
begin
  Names := TUnicodeStringList.Create;
  try
    for I := 0 to ANodes.Count - 1 do
    begin
      Names.Clear;
      CollectLexicallyDeclaredNames(ANodes[I], Names);
      for J := 0 to Names.Count - 1 do
        AddUniqueEvalName(ANames, Names[J]);
    end;
  finally
    Names.Free;
  end;
end;

procedure ValidateEvalScriptBody(const ANodes: TObjectList<TGocciaStatement>);
var
  I: Integer;
begin
  for I := 0 to ANodes.Count - 1 do
  begin
    if (ANodes[I] is TGocciaImportDeclaration) or
       (ANodes[I] is TGocciaExportDeclaration) or
       (ANodes[I] is TGocciaExportDefaultDeclaration) or
       (ANodes[I] is TGocciaExportVariableDeclaration) or
       (ANodes[I] is TGocciaExportFunctionDeclaration) or
       (ANodes[I] is TGocciaExportEnumDeclaration) or
       (ANodes[I] is TGocciaReExportDeclaration) then
      ThrowSyntaxError('Module declarations are not allowed in eval code');
    if ANodes[I] is TGocciaUsingDeclaration then
      ThrowSyntaxError(
        'Using declarations are not allowed at the top level of eval');
  end;
end;

function EvalExpressionContainsArgumentsReference(
  const AExpr: TGocciaExpression): Boolean; forward;
function EvalPatternContainsArgumentsReference(
  const APattern: TGocciaDestructuringPattern): Boolean; forward;
function EvalStatementContainsArgumentsReference(
  const AStmt: TGocciaStatement): Boolean; forward;

function EvalParameterListContainsArgumentsReference(
  const AParameters: TGocciaParameterArray): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AParameters) do
  begin
    if AParameters[I].IsPattern and
       EvalPatternContainsArgumentsReference(AParameters[I].Pattern) then
      Exit(True);
    if EvalExpressionContainsArgumentsReference(AParameters[I].DefaultValue) then
      Exit(True);
  end;
  Result := False;
end;

function EvalASTNodeContainsArgumentsReference(
  const ANode: TGocciaASTNode): Boolean;
begin
  if ANode is TGocciaStatement then
    Exit(EvalStatementContainsArgumentsReference(TGocciaStatement(ANode)));
  if ANode is TGocciaExpression then
    Exit(EvalExpressionContainsArgumentsReference(TGocciaExpression(ANode)));
  Result := False;
end;

function EvalExpressionContainsArgumentsReference(
  const AExpr: TGocciaExpression): Boolean;
var
  CallExpr: TGocciaCallExpression;
  MemberExpr: TGocciaMemberExpression;
  ArrayExpr: TGocciaArrayExpression;
  ObjectExpr: TGocciaObjectExpression;
  ArrowExpr: TGocciaArrowFunctionExpression;
  NewExpr: TGocciaNewExpression;
  Pair: TPair<TGocciaExpression, TGocciaExpression>;
  I: Integer;
begin
  CheckEarlyErrorStack(AExpr);
  if not Assigned(AExpr) then
    Exit(False);

  if AExpr is TGocciaIdentifierExpression then
    Exit(TGocciaIdentifierExpression(AExpr).Name = IDENTIFIER_ARGUMENTS)
  else if AExpr is TGocciaArrowFunctionExpression then
  begin
    ArrowExpr := TGocciaArrowFunctionExpression(AExpr);
    if EvalParameterListContainsArgumentsReference(ArrowExpr.Parameters) then
      Exit(True);
    Exit(EvalASTNodeContainsArgumentsReference(ArrowExpr.Body));
  end
  else if AExpr is TGocciaFunctionExpression then
    Exit(False)
  else if AExpr is TGocciaCallExpression then
  begin
    CallExpr := TGocciaCallExpression(AExpr);
    if EvalExpressionContainsArgumentsReference(CallExpr.Callee) then
      Exit(True);
    for I := 0 to CallExpr.Arguments.Count - 1 do
      if EvalExpressionContainsArgumentsReference(CallExpr.Arguments[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(AExpr);
    if EvalExpressionContainsArgumentsReference(MemberExpr.ObjectExpr) then
      Exit(True);
    if MemberExpr.Computed and
       EvalExpressionContainsArgumentsReference(MemberExpr.PropertyExpression) then
      Exit(True);
  end
  else if AExpr is TGocciaBinaryExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaBinaryExpression(AExpr).Left) or
      EvalExpressionContainsArgumentsReference(
        TGocciaBinaryExpression(AExpr).Right))
  else if AExpr is TGocciaSequenceExpression then
  begin
    for I := 0 to TGocciaSequenceExpression(AExpr).Expressions.Count - 1 do
      if EvalExpressionContainsArgumentsReference(
        TGocciaSequenceExpression(AExpr).Expressions[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaUnaryExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaUnaryExpression(AExpr).Operand))
  // ES2026 §15.7.9 ContainsArguments: an assignment target is an
  // IdentifierReference too.
  else if AExpr is TGocciaAssignmentExpression then
    Exit((TGocciaAssignmentExpression(AExpr).Name = IDENTIFIER_ARGUMENTS) or
      EvalExpressionContainsArgumentsReference(
        TGocciaAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaPropertyAssignmentExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaPropertyAssignmentExpression(AExpr).ObjectExpr) or
      EvalExpressionContainsArgumentsReference(
        TGocciaPropertyAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaComputedPropertyAssignmentExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaComputedPropertyAssignmentExpression(AExpr).ObjectExpr) or
      EvalExpressionContainsArgumentsReference(
        TGocciaComputedPropertyAssignmentExpression(AExpr).PropertyExpression) or
      EvalExpressionContainsArgumentsReference(
        TGocciaComputedPropertyAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaCompoundAssignmentExpression then
    Exit((TGocciaCompoundAssignmentExpression(AExpr).Name =
      IDENTIFIER_ARGUMENTS) or EvalExpressionContainsArgumentsReference(
        TGocciaCompoundAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaPropertyCompoundAssignmentExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaPropertyCompoundAssignmentExpression(AExpr).ObjectExpr) or
      EvalExpressionContainsArgumentsReference(
        TGocciaPropertyCompoundAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaComputedPropertyCompoundAssignmentExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).ObjectExpr) or
      EvalExpressionContainsArgumentsReference(
        TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).PropertyExpression) or
      EvalExpressionContainsArgumentsReference(
        TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaIncrementExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaIncrementExpression(AExpr).Operand))
  else if AExpr is TGocciaArrayExpression then
  begin
    ArrayExpr := TGocciaArrayExpression(AExpr);
    for I := 0 to ArrayExpr.Elements.Count - 1 do
      if EvalExpressionContainsArgumentsReference(ArrayExpr.Elements[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaObjectExpression then
  begin
    ObjectExpr := TGocciaObjectExpression(AExpr);
    for I := 0 to High(ObjectExpr.PropertySourceOrder) do
      case ObjectExpr.PropertySourceOrder[I].PropertyType of
        pstStatic:
          if EvalExpressionContainsArgumentsReference(
              ObjectExpr.PropertySourceOrder[I].Expression) then
            Exit(True);
        pstComputed:
          begin
            Pair := ObjectExpr.ComputedPropertiesInOrder[
              ObjectExpr.PropertySourceOrder[I].ComputedIndex];
            if EvalExpressionContainsArgumentsReference(Pair.Key) or
               EvalExpressionContainsArgumentsReference(Pair.Value) then
              Exit(True);
          end;
        pstComputedGetter,
        pstComputedSetter:
          begin
            Pair := ObjectExpr.ComputedPropertiesInOrder[
              ObjectExpr.PropertySourceOrder[I].ComputedIndex];
            if EvalExpressionContainsArgumentsReference(Pair.Key) then
              Exit(True);
          end;
      end;
  end
  else if AExpr is TGocciaYieldExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaYieldExpression(AExpr).Operand))
  else if AExpr is TGocciaAwaitExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaAwaitExpression(AExpr).Operand))
  else if AExpr is TGocciaConditionalExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaConditionalExpression(AExpr).Condition) or
      EvalExpressionContainsArgumentsReference(
        TGocciaConditionalExpression(AExpr).Consequent) or
      EvalExpressionContainsArgumentsReference(
        TGocciaConditionalExpression(AExpr).Alternate))
  else if AExpr is TGocciaNewExpression then
  begin
    NewExpr := TGocciaNewExpression(AExpr);
    if EvalExpressionContainsArgumentsReference(NewExpr.Callee) then
      Exit(True);
    for I := 0 to NewExpr.Arguments.Count - 1 do
      if EvalExpressionContainsArgumentsReference(NewExpr.Arguments[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaSpreadExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaSpreadExpression(AExpr).Argument))
  else if AExpr is TGocciaTemplateWithInterpolationExpression then
  begin
    for I := 0 to TGocciaTemplateWithInterpolationExpression(AExpr).Parts.Count - 1 do
      if EvalExpressionContainsArgumentsReference(
        TGocciaTemplateWithInterpolationExpression(AExpr).Parts[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaTaggedTemplateExpression then
  begin
    if EvalExpressionContainsArgumentsReference(
        TGocciaTaggedTemplateExpression(AExpr).Tag) then
      Exit(True);
    for I := 0 to TGocciaTaggedTemplateExpression(AExpr).Expressions.Count - 1 do
      if EvalExpressionContainsArgumentsReference(
        TGocciaTaggedTemplateExpression(AExpr).Expressions[I]) then
        Exit(True);
  end
  // The target pattern's default initializers and computed keys are
  // expressions of the eval code as well.
  else if AExpr is TGocciaDestructuringAssignmentExpression then
    Exit(EvalPatternContainsArgumentsReference(
      TGocciaDestructuringAssignmentExpression(AExpr).Left) or
      EvalExpressionContainsArgumentsReference(
        TGocciaDestructuringAssignmentExpression(AExpr).Right))
  else if AExpr is TGocciaPrivateMemberExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaPrivateMemberExpression(AExpr).ObjectExpr))
  else if AExpr is TGocciaPrivatePropertyAssignmentExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaPrivatePropertyAssignmentExpression(AExpr).ObjectExpr) or
      EvalExpressionContainsArgumentsReference(
        TGocciaPrivatePropertyAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaPrivatePropertyCompoundAssignmentExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).ObjectExpr) or
      EvalExpressionContainsArgumentsReference(
        TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).Value));

  Result := False;
end;

function EvalPatternContainsArgumentsReference(
  const APattern: TGocciaDestructuringPattern): Boolean;
var
  ArrayPattern: TGocciaArrayDestructuringPattern;
  ObjectPattern: TGocciaObjectDestructuringPattern;
  AssignmentPattern: TGocciaAssignmentDestructuringPattern;
  Prop: TGocciaDestructuringProperty;
  I: Integer;
begin
  CheckEarlyErrorStack(APattern);
  if not Assigned(APattern) then
    Exit(False);

  if APattern is TGocciaArrayDestructuringPattern then
  begin
    ArrayPattern := TGocciaArrayDestructuringPattern(APattern);
    for I := 0 to ArrayPattern.Elements.Count - 1 do
      if EvalPatternContainsArgumentsReference(ArrayPattern.Elements[I]) then
        Exit(True);
  end
  else if APattern is TGocciaObjectDestructuringPattern then
  begin
    ObjectPattern := TGocciaObjectDestructuringPattern(APattern);
    for I := 0 to ObjectPattern.Properties.Count - 1 do
    begin
      Prop := ObjectPattern.Properties[I];
      if Prop.Computed and
         EvalExpressionContainsArgumentsReference(Prop.KeyExpression) then
        Exit(True);
      if EvalPatternContainsArgumentsReference(Prop.Pattern) then
        Exit(True);
    end;
  end
  else if APattern is TGocciaAssignmentDestructuringPattern then
  begin
    AssignmentPattern := TGocciaAssignmentDestructuringPattern(APattern);
    Exit(EvalPatternContainsArgumentsReference(AssignmentPattern.Left) or
      EvalExpressionContainsArgumentsReference(AssignmentPattern.Right));
  end
  else if APattern is TGocciaRestDestructuringPattern then
    Exit(EvalPatternContainsArgumentsReference(
      TGocciaRestDestructuringPattern(APattern).Argument))
  else if APattern is TGocciaMemberExpressionDestructuringPattern then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaMemberExpressionDestructuringPattern(APattern).Expression))
  else if APattern is TGocciaPrivateMemberExpressionDestructuringPattern then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaPrivateMemberExpressionDestructuringPattern(APattern).Expression));

  Result := False;
end;

function EvalStatementContainsArgumentsReference(
  const AStmt: TGocciaStatement): Boolean;
var
  VarDecl: TGocciaVariableDeclaration;
  DestructDecl: TGocciaDestructuringDeclaration;
  BlockStmt: TGocciaBlockStatement;
  IfStmt: TGocciaIfStatement;
  ForStmt: TGocciaForStatement;
  ForOfStmt: TGocciaForOfStatement;
  ForInStmt: TGocciaForInStatement;
  WhileStmt: TGocciaWhileStatement;
  DoWhileStmt: TGocciaDoWhileStatement;
  WithStmt: TGocciaWithStatement;
  ReturnStmt: TGocciaReturnStatement;
  ThrowStmt: TGocciaThrowStatement;
  TryStmt: TGocciaTryStatement;
  SwitchStmt: TGocciaSwitchStatement;
  UsingDecl: TGocciaUsingDeclaration;
  I, J: Integer;
begin
  CheckEarlyErrorStack(AStmt);
  if not Assigned(AStmt) then
    Exit(False);

  if AStmt is TGocciaExpressionStatement then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaExpressionStatement(AStmt).Expression))
  else if AStmt is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(AStmt);
    for I := 0 to High(VarDecl.Variables) do
      if EvalExpressionContainsArgumentsReference(
          VarDecl.Variables[I].Initializer) then
        Exit(True);
  end
  else if AStmt is TGocciaDestructuringDeclaration then
  begin
    DestructDecl := TGocciaDestructuringDeclaration(AStmt);
    Exit(EvalPatternContainsArgumentsReference(DestructDecl.Pattern) or
      EvalExpressionContainsArgumentsReference(DestructDecl.Initializer));
  end
  else if AStmt is TGocciaBlockStatement then
  begin
    BlockStmt := TGocciaBlockStatement(AStmt);
    for I := 0 to BlockStmt.Nodes.Count - 1 do
      if EvalASTNodeContainsArgumentsReference(BlockStmt.Nodes[I]) then
        Exit(True);
  end
  else if AStmt is TGocciaIfStatement then
  begin
    IfStmt := TGocciaIfStatement(AStmt);
    Exit(EvalExpressionContainsArgumentsReference(IfStmt.Condition) or
      EvalStatementContainsArgumentsReference(IfStmt.Consequent) or
      EvalStatementContainsArgumentsReference(IfStmt.Alternate));
  end
  else if AStmt is TGocciaForStatement then
  begin
    ForStmt := TGocciaForStatement(AStmt);
    Exit(EvalStatementContainsArgumentsReference(ForStmt.Init) or
      EvalExpressionContainsArgumentsReference(ForStmt.Condition) or
      EvalExpressionContainsArgumentsReference(ForStmt.Update) or
      EvalStatementContainsArgumentsReference(ForStmt.Body));
  end
  else if AStmt is TGocciaForOfStatement then
  begin
    ForOfStmt := TGocciaForOfStatement(AStmt);
    Exit(EvalExpressionContainsArgumentsReference(ForOfStmt.Iterable) or
      EvalStatementContainsArgumentsReference(ForOfStmt.Body));
  end
  else if AStmt is TGocciaForInStatement then
  begin
    ForInStmt := TGocciaForInStatement(AStmt);
    Exit(EvalExpressionContainsArgumentsReference(ForInStmt.ObjectExpression) or
      EvalStatementContainsArgumentsReference(ForInStmt.Body));
  end
  else if AStmt is TGocciaWhileStatement then
  begin
    WhileStmt := TGocciaWhileStatement(AStmt);
    Exit(EvalExpressionContainsArgumentsReference(WhileStmt.Condition) or
      EvalStatementContainsArgumentsReference(WhileStmt.Body));
  end
  else if AStmt is TGocciaDoWhileStatement then
  begin
    DoWhileStmt := TGocciaDoWhileStatement(AStmt);
    Exit(EvalStatementContainsArgumentsReference(DoWhileStmt.Body) or
      EvalExpressionContainsArgumentsReference(DoWhileStmt.Condition));
  end
  else if AStmt is TGocciaWithStatement then
  begin
    WithStmt := TGocciaWithStatement(AStmt);
    Exit(EvalExpressionContainsArgumentsReference(WithStmt.ObjectExpression) or
      EvalStatementContainsArgumentsReference(WithStmt.Body));
  end
  else if AStmt is TGocciaReturnStatement then
  begin
    ReturnStmt := TGocciaReturnStatement(AStmt);
    Exit(EvalExpressionContainsArgumentsReference(ReturnStmt.Value));
  end
  else if AStmt is TGocciaThrowStatement then
  begin
    ThrowStmt := TGocciaThrowStatement(AStmt);
    Exit(EvalExpressionContainsArgumentsReference(ThrowStmt.Value));
  end
  else if AStmt is TGocciaTryStatement then
  begin
    TryStmt := TGocciaTryStatement(AStmt);
    Exit(EvalStatementContainsArgumentsReference(TryStmt.Block) or
      EvalStatementContainsArgumentsReference(TryStmt.CatchBlock) or
      EvalStatementContainsArgumentsReference(TryStmt.FinallyBlock));
  end
  else if AStmt is TGocciaSwitchStatement then
  begin
    SwitchStmt := TGocciaSwitchStatement(AStmt);
    if EvalExpressionContainsArgumentsReference(SwitchStmt.Discriminant) then
      Exit(True);
    for I := 0 to SwitchStmt.Cases.Count - 1 do
    begin
      if EvalExpressionContainsArgumentsReference(SwitchStmt.Cases[I].Test) then
        Exit(True);
      for J := 0 to SwitchStmt.Cases[I].Consequent.Count - 1 do
        if EvalStatementContainsArgumentsReference(
            SwitchStmt.Cases[I].Consequent[J]) then
          Exit(True);
    end;
  end
  else if AStmt is TGocciaUsingDeclaration then
  begin
    UsingDecl := TGocciaUsingDeclaration(AStmt);
    for I := 0 to High(UsingDecl.Variables) do
      if EvalExpressionContainsArgumentsReference(
          UsingDecl.Variables[I].Initializer) then
        Exit(True);
  end;

  Result := False;
end;

function EvalProgramContainsArgumentsReference(
  const AProgram: TGocciaProgram): Boolean;
var
  I: Integer;
begin
  for I := 0 to AProgram.Body.Count - 1 do
    if EvalStatementContainsArgumentsReference(AProgram.Body[I]) then
      Exit(True);
  Result := False;
end;

procedure ValidateEvalEarlyErrorExpression(const AExpr: TGocciaExpression;
  const AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall: Boolean); forward;
procedure ValidateEvalEarlyErrorPattern(
  const APattern: TGocciaDestructuringPattern; const AAllowNewTarget,
  AAllowSuperProperty, AAllowSuperCall: Boolean); forward;

procedure ValidateEvalEarlyErrorStatement(const AStmt: TGocciaStatement;
  const AStrictEval, AAllowNewTarget, AAllowSuperProperty,
  AAllowSuperCall: Boolean);
var
  VarDecl: TGocciaVariableDeclaration;
  DestructDecl: TGocciaDestructuringDeclaration;
  BlockStmt: TGocciaBlockStatement;
  IfStmt: TGocciaIfStatement;
  ForStmt: TGocciaForStatement;
  ForOfStmt: TGocciaForOfStatement;
  ForInStmt: TGocciaForInStatement;
  WhileStmt: TGocciaWhileStatement;
  DoWhileStmt: TGocciaDoWhileStatement;
  WithStmt: TGocciaWithStatement;
  ReturnStmt: TGocciaReturnStatement;
  ThrowStmt: TGocciaThrowStatement;
  TryStmt: TGocciaTryStatement;
  SwitchStmt: TGocciaSwitchStatement;
  UsingDecl: TGocciaUsingDeclaration;
  I, J: Integer;
  function IsLabelledFunctionDeclaration(
    const AStatement: TGocciaStatement): Boolean;
  begin
    Result := Assigned(AStatement) and (AStatement.LabelCount > 0) and
      (AStatement is TGocciaFunctionDeclaration);
  end;
begin
  CheckEarlyErrorStack(AStmt);
  if not Assigned(AStmt) then
    Exit;

  if AStrictEval and IsLabelledFunctionDeclaration(AStmt) then
    ThrowSyntaxError('Invalid labelled function declaration');

  if AStmt is TGocciaExpressionStatement then
    ValidateEvalEarlyErrorExpression(
      TGocciaExpressionStatement(AStmt).Expression, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall)
  else if AStmt is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(AStmt);
    for I := 0 to High(VarDecl.Variables) do
    begin
      if VarDecl.Variables[I].IsPattern or Assigned(VarDecl.Variables[I].Pattern) then
        ValidateEvalEarlyErrorPattern(VarDecl.Variables[I].Pattern,
          AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
      ValidateEvalEarlyErrorExpression(VarDecl.Variables[I].Initializer,
        AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    end;
  end
  else if AStmt is TGocciaDestructuringDeclaration then
  begin
    DestructDecl := TGocciaDestructuringDeclaration(AStmt);
    ValidateEvalEarlyErrorPattern(DestructDecl.Pattern, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(DestructDecl.Initializer,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaBlockStatement then
  begin
    BlockStmt := TGocciaBlockStatement(AStmt);
    for I := 0 to BlockStmt.Nodes.Count - 1 do
      if BlockStmt.Nodes[I] is TGocciaStatement then
        ValidateEvalEarlyErrorStatement(TGocciaStatement(BlockStmt.Nodes[I]),
          AStrictEval, AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall)
      else if BlockStmt.Nodes[I] is TGocciaExpression then
        ValidateEvalEarlyErrorExpression(TGocciaExpression(BlockStmt.Nodes[I]),
          AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaIfStatement then
  begin
    IfStmt := TGocciaIfStatement(AStmt);
    if AStrictEval and (IsLabelledFunctionDeclaration(IfStmt.Consequent) or
       IsLabelledFunctionDeclaration(IfStmt.Alternate)) then
      ThrowSyntaxError('Invalid labelled function declaration');
    ValidateEvalEarlyErrorExpression(IfStmt.Condition, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorStatement(IfStmt.Consequent, AStrictEval,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorStatement(IfStmt.Alternate, AStrictEval,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaForStatement then
  begin
    ForStmt := TGocciaForStatement(AStmt);
    ValidateEvalEarlyErrorStatement(ForStmt.Init, AStrictEval, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(ForStmt.Condition, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(ForStmt.Update, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorStatement(ForStmt.Body, AStrictEval, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaForOfStatement then
  begin
    ForOfStmt := TGocciaForOfStatement(AStmt);
    ValidateEvalEarlyErrorExpression(ForOfStmt.Iterable, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorStatement(ForOfStmt.Body, AStrictEval,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaForInStatement then
  begin
    ForInStmt := TGocciaForInStatement(AStmt);
    ValidateEvalEarlyErrorExpression(ForInStmt.ObjectExpression,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorStatement(ForInStmt.Body, AStrictEval,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaWhileStatement then
  begin
    WhileStmt := TGocciaWhileStatement(AStmt);
    ValidateEvalEarlyErrorExpression(WhileStmt.Condition, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorStatement(WhileStmt.Body, AStrictEval,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaDoWhileStatement then
  begin
    DoWhileStmt := TGocciaDoWhileStatement(AStmt);
    ValidateEvalEarlyErrorStatement(DoWhileStmt.Body, AStrictEval,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(DoWhileStmt.Condition, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaWithStatement then
  begin
    WithStmt := TGocciaWithStatement(AStmt);
    ValidateEvalEarlyErrorExpression(WithStmt.ObjectExpression,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorStatement(WithStmt.Body, AStrictEval,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaReturnStatement then
  begin
    ReturnStmt := TGocciaReturnStatement(AStmt);
    ValidateEvalEarlyErrorExpression(ReturnStmt.Value, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaThrowStatement then
  begin
    ThrowStmt := TGocciaThrowStatement(AStmt);
    ValidateEvalEarlyErrorExpression(ThrowStmt.Value, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaTryStatement then
  begin
    TryStmt := TGocciaTryStatement(AStmt);
    ValidateEvalEarlyErrorStatement(TryStmt.Block, AStrictEval,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorStatement(TryStmt.CatchBlock, AStrictEval,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorStatement(TryStmt.FinallyBlock, AStrictEval,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaSwitchStatement then
  begin
    SwitchStmt := TGocciaSwitchStatement(AStmt);
    ValidateEvalEarlyErrorExpression(SwitchStmt.Discriminant, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    for I := 0 to SwitchStmt.Cases.Count - 1 do
    begin
      ValidateEvalEarlyErrorExpression(SwitchStmt.Cases[I].Test,
        AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
      for J := 0 to SwitchStmt.Cases[I].Consequent.Count - 1 do
        ValidateEvalEarlyErrorStatement(SwitchStmt.Cases[I].Consequent[J],
          AStrictEval, AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    end;
  end
  else if AStmt is TGocciaUsingDeclaration then
  begin
    UsingDecl := TGocciaUsingDeclaration(AStmt);
    for I := 0 to High(UsingDecl.Variables) do
      ValidateEvalEarlyErrorExpression(UsingDecl.Variables[I].Initializer,
        AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end;
end;

procedure ValidateEvalEarlyErrorExpression(const AExpr: TGocciaExpression;
  const AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall: Boolean);
var
  CallExpr: TGocciaCallExpression;
  MemberExpr: TGocciaMemberExpression;
  ArrayExpr: TGocciaArrayExpression;
  ObjectExpr: TGocciaObjectExpression;
  ArrowExpr: TGocciaArrowFunctionExpression;
  NewExpr: TGocciaNewExpression;
  Pair: TPair<TGocciaExpression, TGocciaExpression>;
  I: Integer;
begin
  CheckEarlyErrorStack(AExpr);
  if not Assigned(AExpr) then
    Exit;

  if AExpr is TGocciaNewTargetExpression then
  begin
    if not AAllowNewTarget then
      ThrowSyntaxError('new.target is not allowed in this eval context');
    Exit;
  end;

  if AExpr is TGocciaArrowFunctionExpression then
  begin
    ArrowExpr := TGocciaArrowFunctionExpression(AExpr);
    for I := 0 to High(ArrowExpr.Parameters) do
    begin
      if ArrowExpr.Parameters[I].IsPattern then
        ValidateEvalEarlyErrorPattern(ArrowExpr.Parameters[I].Pattern,
          AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
      ValidateEvalEarlyErrorExpression(ArrowExpr.Parameters[I].DefaultValue,
        AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    end;
    if ArrowExpr.Body is TGocciaStatement then
      ValidateEvalEarlyErrorStatement(TGocciaStatement(ArrowExpr.Body),
        False, AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall)
    else if ArrowExpr.Body is TGocciaExpression then
      ValidateEvalEarlyErrorExpression(TGocciaExpression(ArrowExpr.Body),
        AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaCallExpression then
  begin
    CallExpr := TGocciaCallExpression(AExpr);
    if CallExpr.Callee is TGocciaSuperExpression then
    begin
      if not AAllowSuperCall then
        ThrowSyntaxError('super() is not allowed in this eval context');
    end
    else
      ValidateEvalEarlyErrorExpression(CallExpr.Callee, AAllowNewTarget,
        AAllowSuperProperty, AAllowSuperCall);
    for I := 0 to CallExpr.Arguments.Count - 1 do
      ValidateEvalEarlyErrorExpression(CallExpr.Arguments[I], AAllowNewTarget,
        AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(AExpr);
    if MemberExpr.ObjectExpr is TGocciaSuperExpression then
    begin
      if not AAllowSuperProperty then
        ThrowSyntaxError('super property access is not allowed in this eval context');
    end
    else
      ValidateEvalEarlyErrorExpression(MemberExpr.ObjectExpr, AAllowNewTarget,
        AAllowSuperProperty, AAllowSuperCall);
    if MemberExpr.Computed then
      ValidateEvalEarlyErrorExpression(MemberExpr.PropertyExpression,
        AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaSuperExpression then
  begin
    if not AAllowSuperProperty then
      ThrowSyntaxError('super is not allowed in this eval context');
  end
  else if AExpr is TGocciaBinaryExpression then
  begin
    ValidateEvalEarlyErrorExpression(TGocciaBinaryExpression(AExpr).Left,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(TGocciaBinaryExpression(AExpr).Right,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaSequenceExpression then
  begin
    for I := 0 to TGocciaSequenceExpression(AExpr).Expressions.Count - 1 do
      ValidateEvalEarlyErrorExpression(
        TGocciaSequenceExpression(AExpr).Expressions[I], AAllowNewTarget,
        AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaUnaryExpression then
    ValidateEvalEarlyErrorExpression(TGocciaUnaryExpression(AExpr).Operand,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall)
  else if AExpr is TGocciaAssignmentExpression then
    ValidateEvalEarlyErrorExpression(TGocciaAssignmentExpression(AExpr).Value,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall)
  else if AExpr is TGocciaPropertyAssignmentExpression then
  begin
    ValidateEvalEarlyErrorExpression(
      TGocciaPropertyAssignmentExpression(AExpr).ObjectExpr, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(
      TGocciaPropertyAssignmentExpression(AExpr).Value, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaComputedPropertyAssignmentExpression then
  begin
    ValidateEvalEarlyErrorExpression(
      TGocciaComputedPropertyAssignmentExpression(AExpr).ObjectExpr,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(
      TGocciaComputedPropertyAssignmentExpression(AExpr).PropertyExpression,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(
      TGocciaComputedPropertyAssignmentExpression(AExpr).Value,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaCompoundAssignmentExpression then
    ValidateEvalEarlyErrorExpression(
      TGocciaCompoundAssignmentExpression(AExpr).Value, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall)
  else if AExpr is TGocciaPropertyCompoundAssignmentExpression then
  begin
    ValidateEvalEarlyErrorExpression(
      TGocciaPropertyCompoundAssignmentExpression(AExpr).ObjectExpr,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(
      TGocciaPropertyCompoundAssignmentExpression(AExpr).Value,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaComputedPropertyCompoundAssignmentExpression then
  begin
    ValidateEvalEarlyErrorExpression(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).ObjectExpr,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).PropertyExpression,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).Value,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaIncrementExpression then
    ValidateEvalEarlyErrorExpression(TGocciaIncrementExpression(AExpr).Operand,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall)
  else if AExpr is TGocciaArrayExpression then
  begin
    ArrayExpr := TGocciaArrayExpression(AExpr);
    for I := 0 to ArrayExpr.Elements.Count - 1 do
      ValidateEvalEarlyErrorExpression(ArrayExpr.Elements[I], AAllowNewTarget,
        AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaObjectExpression then
  begin
    ObjectExpr := TGocciaObjectExpression(AExpr);
    for I := 0 to High(ObjectExpr.PropertySourceOrder) do
      case ObjectExpr.PropertySourceOrder[I].PropertyType of
        pstStatic:
          ValidateEvalEarlyErrorExpression(
            ObjectExpr.PropertySourceOrder[I].Expression, AAllowNewTarget,
            AAllowSuperProperty, AAllowSuperCall);
        pstComputed:
          begin
            Pair := ObjectExpr.ComputedPropertiesInOrder[
              ObjectExpr.PropertySourceOrder[I].ComputedIndex];
            ValidateEvalEarlyErrorExpression(Pair.Key, AAllowNewTarget,
              AAllowSuperProperty, AAllowSuperCall);
            ValidateEvalEarlyErrorExpression(Pair.Value, AAllowNewTarget,
              AAllowSuperProperty, AAllowSuperCall);
          end;
        pstComputedGetter,
        pstComputedSetter:
          begin
            Pair := ObjectExpr.ComputedPropertiesInOrder[
              ObjectExpr.PropertySourceOrder[I].ComputedIndex];
            ValidateEvalEarlyErrorExpression(Pair.Key, AAllowNewTarget,
              AAllowSuperProperty, AAllowSuperCall);
          end;
      end;
  end
  else if AExpr is TGocciaYieldExpression then
    ValidateEvalEarlyErrorExpression(TGocciaYieldExpression(AExpr).Operand,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall)
  else if AExpr is TGocciaAwaitExpression then
    ValidateEvalEarlyErrorExpression(TGocciaAwaitExpression(AExpr).Operand,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall)
  else if AExpr is TGocciaConditionalExpression then
  begin
    ValidateEvalEarlyErrorExpression(TGocciaConditionalExpression(AExpr).Condition,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(TGocciaConditionalExpression(AExpr).Consequent,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(TGocciaConditionalExpression(AExpr).Alternate,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaNewExpression then
  begin
    NewExpr := TGocciaNewExpression(AExpr);
    ValidateEvalEarlyErrorExpression(NewExpr.Callee, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    for I := 0 to NewExpr.Arguments.Count - 1 do
      ValidateEvalEarlyErrorExpression(NewExpr.Arguments[I], AAllowNewTarget,
        AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaSpreadExpression then
    ValidateEvalEarlyErrorExpression(TGocciaSpreadExpression(AExpr).Argument,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall)
  else if AExpr is TGocciaTemplateWithInterpolationExpression then
  begin
    for I := 0 to TGocciaTemplateWithInterpolationExpression(AExpr).Parts.Count - 1 do
      ValidateEvalEarlyErrorExpression(
        TGocciaTemplateWithInterpolationExpression(AExpr).Parts[I],
        AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaTaggedTemplateExpression then
  begin
    ValidateEvalEarlyErrorExpression(TGocciaTaggedTemplateExpression(AExpr).Tag,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    for I := 0 to TGocciaTaggedTemplateExpression(AExpr).Expressions.Count - 1 do
      ValidateEvalEarlyErrorExpression(
        TGocciaTaggedTemplateExpression(AExpr).Expressions[I],
        AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaDestructuringAssignmentExpression then
  begin
    ValidateEvalEarlyErrorPattern(
      TGocciaDestructuringAssignmentExpression(AExpr).Left, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(
      TGocciaDestructuringAssignmentExpression(AExpr).Right, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaPrivateMemberExpression then
    ValidateEvalEarlyErrorExpression(
      TGocciaPrivateMemberExpression(AExpr).ObjectExpr, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall)
  else if AExpr is TGocciaPrivatePropertyAssignmentExpression then
  begin
    ValidateEvalEarlyErrorExpression(
      TGocciaPrivatePropertyAssignmentExpression(AExpr).ObjectExpr,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(
      TGocciaPrivatePropertyAssignmentExpression(AExpr).Value,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaPrivatePropertyCompoundAssignmentExpression then
  begin
    ValidateEvalEarlyErrorExpression(
      TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).ObjectExpr,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(
      TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).Value,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end;
end;

procedure ValidateEvalEarlyErrorPattern(
  const APattern: TGocciaDestructuringPattern; const AAllowNewTarget,
  AAllowSuperProperty, AAllowSuperCall: Boolean);
var
  ArrayPattern: TGocciaArrayDestructuringPattern;
  ObjectPattern: TGocciaObjectDestructuringPattern;
  AssignmentPattern: TGocciaAssignmentDestructuringPattern;
  RestPattern: TGocciaRestDestructuringPattern;
  Prop: TGocciaDestructuringProperty;
  I: Integer;
begin
  CheckEarlyErrorStack(APattern);
  if not Assigned(APattern) then
    Exit;

  if APattern is TGocciaArrayDestructuringPattern then
  begin
    ArrayPattern := TGocciaArrayDestructuringPattern(APattern);
    for I := 0 to ArrayPattern.Elements.Count - 1 do
      ValidateEvalEarlyErrorPattern(ArrayPattern.Elements[I], AAllowNewTarget,
        AAllowSuperProperty, AAllowSuperCall);
  end
  else if APattern is TGocciaObjectDestructuringPattern then
  begin
    ObjectPattern := TGocciaObjectDestructuringPattern(APattern);
    for I := 0 to ObjectPattern.Properties.Count - 1 do
    begin
      Prop := ObjectPattern.Properties[I];
      if Prop.Computed then
        ValidateEvalEarlyErrorExpression(Prop.KeyExpression, AAllowNewTarget,
          AAllowSuperProperty, AAllowSuperCall);
      ValidateEvalEarlyErrorPattern(Prop.Pattern, AAllowNewTarget,
        AAllowSuperProperty, AAllowSuperCall);
    end;
  end
  else if APattern is TGocciaAssignmentDestructuringPattern then
  begin
    AssignmentPattern := TGocciaAssignmentDestructuringPattern(APattern);
    ValidateEvalEarlyErrorPattern(AssignmentPattern.Left, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(AssignmentPattern.Right, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
  end
  else if APattern is TGocciaRestDestructuringPattern then
  begin
    RestPattern := TGocciaRestDestructuringPattern(APattern);
    ValidateEvalEarlyErrorPattern(RestPattern.Argument, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
  end
  else if APattern is TGocciaMemberExpressionDestructuringPattern then
    ValidateEvalEarlyErrorExpression(
      TGocciaMemberExpressionDestructuringPattern(APattern).Expression,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall)
  else if APattern is TGocciaPrivateMemberExpressionDestructuringPattern then
    ValidateEvalEarlyErrorExpression(
      TGocciaPrivateMemberExpressionDestructuringPattern(APattern).Expression,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
end;

function IsStrictEvalRestrictedBindingName(const AName: string): Boolean;
begin
  Result := (AName = 'eval') or (AName = IDENTIFIER_ARGUMENTS);
end;

procedure ValidateStrictEvalAssignmentExpression(
  const AExpr: TGocciaExpression); forward;
procedure ValidateStrictEvalAssignmentPattern(
  const APattern: TGocciaDestructuringPattern); forward;
procedure ValidateStrictEvalAssignmentStatement(
  const AStmt: TGocciaStatement); forward;

procedure RejectStrictEvalAssignmentName(const AName: string;
  const ALine, AColumn: Integer);
begin
  if IsStrictEvalRestrictedBindingName(AName) then
    raise TGocciaSyntaxError.Create(
      Format('Invalid assignment to ''%s'' in strict mode', [AName]),
      ALine, AColumn, '', nil);
end;

procedure ValidateStrictFunctionExpression(
  const AFunction: TGocciaFunctionExpression);
var
  BindingNames, PatternNames: TUnicodeStringList;
  I, J: Integer;
  procedure AddStrictParameterName(const AName: string);
  begin
    RejectStrictEvalAssignmentName(AName, AFunction.Line, AFunction.Column);
    if AName = '' then
      Exit;
    if BindingNames.HasString(AName) then
      raise TGocciaSyntaxError.Create(
        'Duplicate parameter name not allowed in strict mode',
        AFunction.Line, AFunction.Column, '', nil);
    BindingNames.Add(AName);
  end;
begin
  if not Assigned(AFunction) then
    Exit;
  RejectStrictEvalAssignmentName(AFunction.Name, AFunction.Line,
    AFunction.Column);
  BindingNames := TUnicodeStringList.Create;
  PatternNames := TUnicodeStringList.Create;
  try
    for I := 0 to High(AFunction.Parameters) do
    begin
      if AFunction.Parameters[I].IsPattern then
      begin
        ValidateStrictEvalAssignmentPattern(AFunction.Parameters[I].Pattern);
        PatternNames.Clear;
        CollectPatternBindingNames(AFunction.Parameters[I].Pattern,
          PatternNames);
        for J := 0 to PatternNames.Count - 1 do
          AddStrictParameterName(PatternNames[J]);
      end
      else
        AddStrictParameterName(AFunction.Parameters[I].Name);
      ValidateStrictEvalAssignmentExpression(
        AFunction.Parameters[I].DefaultValue);
    end;
  finally
    PatternNames.Free;
    BindingNames.Free;
  end;
  if AFunction.Body is TGocciaStatement then
    ValidateStrictEvalAssignmentStatement(TGocciaStatement(AFunction.Body))
  else if AFunction.Body is TGocciaExpression then
    ValidateStrictEvalAssignmentExpression(TGocciaExpression(AFunction.Body));
end;

procedure ValidateStrictFunctionEarlyErrors(
  const AFunction: TGocciaFunctionExpression);
begin
  ValidateStrictFunctionExpression(AFunction);
end;

procedure ValidateStrictEvalAssignmentStatement(
  const AStmt: TGocciaStatement);
var
  BlockStmt: TGocciaBlockStatement;
  DestructDecl: TGocciaDestructuringDeclaration;
  DoWhileStmt: TGocciaDoWhileStatement;
  FunctionDecl: TGocciaFunctionDeclaration;
  ForInStmt: TGocciaForInStatement;
  ForOfStmt: TGocciaForOfStatement;
  ForStmt: TGocciaForStatement;
  IfStmt: TGocciaIfStatement;
  SwitchStmt: TGocciaSwitchStatement;
  ThrowStmt: TGocciaThrowStatement;
  TryStmt: TGocciaTryStatement;
  UsingDecl: TGocciaUsingDeclaration;
  VarDecl: TGocciaVariableDeclaration;
  WhileStmt: TGocciaWhileStatement;
  WithStmt: TGocciaWithStatement;
  I, J: Integer;
begin
  CheckEarlyErrorStack(AStmt);
  if not Assigned(AStmt) then
    Exit;

  if AStmt is TGocciaExpressionStatement then
    ValidateStrictEvalAssignmentExpression(
      TGocciaExpressionStatement(AStmt).Expression)
  else if AStmt is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(AStmt);
    for I := 0 to High(VarDecl.Variables) do
    begin
      if VarDecl.Variables[I].IsPattern or Assigned(VarDecl.Variables[I].Pattern) then
        ValidateStrictEvalAssignmentPattern(VarDecl.Variables[I].Pattern)
      else
        RejectStrictEvalAssignmentName(VarDecl.Variables[I].Name, AStmt.Line,
          AStmt.Column);
      ValidateStrictEvalAssignmentExpression(VarDecl.Variables[I].Initializer);
    end;
  end
  else if AStmt is TGocciaDestructuringDeclaration then
  begin
    DestructDecl := TGocciaDestructuringDeclaration(AStmt);
    ValidateStrictEvalAssignmentPattern(DestructDecl.Pattern);
    ValidateStrictEvalAssignmentExpression(DestructDecl.Initializer);
  end
  else if AStmt is TGocciaFunctionDeclaration then
  begin
    FunctionDecl := TGocciaFunctionDeclaration(AStmt);
    RejectStrictEvalAssignmentName(FunctionDecl.Name, AStmt.Line, AStmt.Column);
    ValidateStrictFunctionExpression(FunctionDecl.FunctionExpression);
  end
  else if AStmt is TGocciaBlockStatement then
  begin
    BlockStmt := TGocciaBlockStatement(AStmt);
    for I := 0 to BlockStmt.Nodes.Count - 1 do
      if BlockStmt.Nodes[I] is TGocciaStatement then
        ValidateStrictEvalAssignmentStatement(TGocciaStatement(BlockStmt.Nodes[I]))
      else if BlockStmt.Nodes[I] is TGocciaExpression then
        ValidateStrictEvalAssignmentExpression(TGocciaExpression(BlockStmt.Nodes[I]));
  end
  else if AStmt is TGocciaIfStatement then
  begin
    IfStmt := TGocciaIfStatement(AStmt);
    ValidateStrictEvalAssignmentExpression(IfStmt.Condition);
    ValidateStrictEvalAssignmentStatement(IfStmt.Consequent);
    ValidateStrictEvalAssignmentStatement(IfStmt.Alternate);
  end
  else if AStmt is TGocciaForStatement then
  begin
    ForStmt := TGocciaForStatement(AStmt);
    ValidateStrictEvalAssignmentStatement(ForStmt.Init);
    ValidateStrictEvalAssignmentExpression(ForStmt.Condition);
    ValidateStrictEvalAssignmentExpression(ForStmt.Update);
    ValidateStrictEvalAssignmentStatement(ForStmt.Body);
  end
  else if AStmt is TGocciaForOfStatement then
  begin
    ForOfStmt := TGocciaForOfStatement(AStmt);
    RejectStrictEvalAssignmentName(ForOfStmt.BindingName, AStmt.Line,
      AStmt.Column);
    ValidateStrictEvalAssignmentPattern(ForOfStmt.BindingPattern);
    ValidateStrictEvalAssignmentPattern(ForOfStmt.AssignmentTarget);
    ValidateStrictEvalAssignmentExpression(ForOfStmt.Iterable);
    ValidateStrictEvalAssignmentStatement(ForOfStmt.Body);
  end
  else if AStmt is TGocciaForInStatement then
  begin
    ForInStmt := TGocciaForInStatement(AStmt);
    RejectStrictEvalAssignmentName(ForInStmt.BindingName, AStmt.Line,
      AStmt.Column);
    ValidateStrictEvalAssignmentPattern(ForInStmt.BindingPattern);
    ValidateStrictEvalAssignmentPattern(ForInStmt.AssignmentTarget);
    ValidateStrictEvalAssignmentExpression(ForInStmt.ObjectExpression);
    ValidateStrictEvalAssignmentStatement(ForInStmt.Body);
  end
  else if AStmt is TGocciaWhileStatement then
  begin
    WhileStmt := TGocciaWhileStatement(AStmt);
    ValidateStrictEvalAssignmentExpression(WhileStmt.Condition);
    ValidateStrictEvalAssignmentStatement(WhileStmt.Body);
  end
  else if AStmt is TGocciaDoWhileStatement then
  begin
    DoWhileStmt := TGocciaDoWhileStatement(AStmt);
    ValidateStrictEvalAssignmentStatement(DoWhileStmt.Body);
    ValidateStrictEvalAssignmentExpression(DoWhileStmt.Condition);
  end
  else if AStmt is TGocciaWithStatement then
  begin
    WithStmt := TGocciaWithStatement(AStmt);
    ValidateStrictEvalAssignmentExpression(WithStmt.ObjectExpression);
    ValidateStrictEvalAssignmentStatement(WithStmt.Body);
  end
  else if AStmt is TGocciaReturnStatement then
    ValidateStrictEvalAssignmentExpression(
      TGocciaReturnStatement(AStmt).Value)
  else if AStmt is TGocciaThrowStatement then
  begin
    ThrowStmt := TGocciaThrowStatement(AStmt);
    ValidateStrictEvalAssignmentExpression(ThrowStmt.Value);
  end
  else if AStmt is TGocciaTryStatement then
  begin
    TryStmt := TGocciaTryStatement(AStmt);
    ValidateStrictEvalAssignmentStatement(TryStmt.Block);
    RejectStrictEvalAssignmentName(TryStmt.CatchParam, AStmt.Line,
      AStmt.Column);
    ValidateStrictEvalAssignmentPattern(TryStmt.CatchBindingPattern);
    ValidateStrictEvalAssignmentStatement(TryStmt.CatchBlock);
    ValidateStrictEvalAssignmentStatement(TryStmt.FinallyBlock);
  end
  else if AStmt is TGocciaSwitchStatement then
  begin
    SwitchStmt := TGocciaSwitchStatement(AStmt);
    ValidateStrictEvalAssignmentExpression(SwitchStmt.Discriminant);
    for I := 0 to SwitchStmt.Cases.Count - 1 do
    begin
      ValidateStrictEvalAssignmentExpression(SwitchStmt.Cases[I].Test);
      for J := 0 to SwitchStmt.Cases[I].Consequent.Count - 1 do
        ValidateStrictEvalAssignmentStatement(SwitchStmt.Cases[I].Consequent[J]);
    end;
  end
  else if AStmt is TGocciaUsingDeclaration then
  begin
    UsingDecl := TGocciaUsingDeclaration(AStmt);
    for I := 0 to High(UsingDecl.Variables) do
      ValidateStrictEvalAssignmentExpression(UsingDecl.Variables[I].Initializer);
  end;
end;

procedure ValidateStrictEvalAssignmentExpression(
  const AExpr: TGocciaExpression);
var
  ArrayExpr: TGocciaArrayExpression;
  CallExpr: TGocciaCallExpression;
  MemberExpr: TGocciaMemberExpression;
  NewExpr: TGocciaNewExpression;
  ObjectExpr: TGocciaObjectExpression;
  Pair: TPair<TGocciaExpression, TGocciaExpression>;
  UnaryExpr: TGocciaUnaryExpression;
  I: Integer;
begin
  CheckEarlyErrorStack(AExpr);
  if not Assigned(AExpr) then
    Exit;

  if AExpr is TGocciaAssignmentExpression then
  begin
    RejectStrictEvalAssignmentName(TGocciaAssignmentExpression(AExpr).Name,
      AExpr.Line, AExpr.Column);
    ValidateStrictEvalAssignmentExpression(
      TGocciaAssignmentExpression(AExpr).Value);
  end
  else if AExpr is TGocciaCompoundAssignmentExpression then
  begin
    RejectStrictEvalAssignmentName(TGocciaCompoundAssignmentExpression(AExpr).Name,
      AExpr.Line, AExpr.Column);
    ValidateStrictEvalAssignmentExpression(
      TGocciaCompoundAssignmentExpression(AExpr).Value);
  end
  else if AExpr is TGocciaIncrementExpression then
  begin
    if TGocciaIncrementExpression(AExpr).Operand is TGocciaIdentifierExpression then
      RejectStrictEvalAssignmentName(
        TGocciaIdentifierExpression(TGocciaIncrementExpression(AExpr).Operand).Name,
        AExpr.Line, AExpr.Column);
    ValidateStrictEvalAssignmentExpression(
      TGocciaIncrementExpression(AExpr).Operand);
  end
  else if AExpr is TGocciaFunctionExpression then
    ValidateStrictFunctionExpression(TGocciaFunctionExpression(AExpr))
  else if AExpr is TGocciaArrowFunctionExpression then
  begin
    for I := 0 to High(TGocciaArrowFunctionExpression(AExpr).Parameters) do
    begin
      if TGocciaArrowFunctionExpression(AExpr).Parameters[I].IsPattern then
        ValidateStrictEvalAssignmentPattern(
          TGocciaArrowFunctionExpression(AExpr).Parameters[I].Pattern)
      else
        RejectStrictEvalAssignmentName(
          TGocciaArrowFunctionExpression(AExpr).Parameters[I].Name,
          AExpr.Line, AExpr.Column);
      ValidateStrictEvalAssignmentExpression(
        TGocciaArrowFunctionExpression(AExpr).Parameters[I].DefaultValue);
    end;
    if TGocciaArrowFunctionExpression(AExpr).Body is TGocciaStatement then
      ValidateStrictEvalAssignmentStatement(
        TGocciaStatement(TGocciaArrowFunctionExpression(AExpr).Body))
    else if TGocciaArrowFunctionExpression(AExpr).Body is TGocciaExpression then
      ValidateStrictEvalAssignmentExpression(
        TGocciaExpression(TGocciaArrowFunctionExpression(AExpr).Body));
  end
  else if AExpr is TGocciaDestructuringAssignmentExpression then
  begin
    ValidateStrictEvalAssignmentPattern(
      TGocciaDestructuringAssignmentExpression(AExpr).Left);
    ValidateStrictEvalAssignmentExpression(
      TGocciaDestructuringAssignmentExpression(AExpr).Right);
  end
  else if AExpr is TGocciaCallExpression then
  begin
    CallExpr := TGocciaCallExpression(AExpr);
    ValidateStrictEvalAssignmentExpression(CallExpr.Callee);
    for I := 0 to CallExpr.Arguments.Count - 1 do
      ValidateStrictEvalAssignmentExpression(CallExpr.Arguments[I]);
  end
  else if AExpr is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(AExpr);
    ValidateStrictEvalAssignmentExpression(MemberExpr.ObjectExpr);
    if MemberExpr.Computed then
      ValidateStrictEvalAssignmentExpression(MemberExpr.PropertyExpression);
  end
  else if AExpr is TGocciaPropertyAssignmentExpression then
  begin
    ValidateStrictEvalAssignmentExpression(
      TGocciaPropertyAssignmentExpression(AExpr).ObjectExpr);
    ValidateStrictEvalAssignmentExpression(
      TGocciaPropertyAssignmentExpression(AExpr).Value);
  end
  else if AExpr is TGocciaComputedPropertyAssignmentExpression then
  begin
    ValidateStrictEvalAssignmentExpression(
      TGocciaComputedPropertyAssignmentExpression(AExpr).ObjectExpr);
    ValidateStrictEvalAssignmentExpression(
      TGocciaComputedPropertyAssignmentExpression(AExpr).PropertyExpression);
    ValidateStrictEvalAssignmentExpression(
      TGocciaComputedPropertyAssignmentExpression(AExpr).Value);
  end
  else if AExpr is TGocciaPropertyCompoundAssignmentExpression then
  begin
    ValidateStrictEvalAssignmentExpression(
      TGocciaPropertyCompoundAssignmentExpression(AExpr).ObjectExpr);
    ValidateStrictEvalAssignmentExpression(
      TGocciaPropertyCompoundAssignmentExpression(AExpr).Value);
  end
  else if AExpr is TGocciaComputedPropertyCompoundAssignmentExpression then
  begin
    ValidateStrictEvalAssignmentExpression(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).ObjectExpr);
    ValidateStrictEvalAssignmentExpression(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).PropertyExpression);
    ValidateStrictEvalAssignmentExpression(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).Value);
  end
  else if AExpr is TGocciaBinaryExpression then
  begin
    ValidateStrictEvalAssignmentExpression(TGocciaBinaryExpression(AExpr).Left);
    ValidateStrictEvalAssignmentExpression(TGocciaBinaryExpression(AExpr).Right);
  end
  else if AExpr is TGocciaSequenceExpression then
  begin
    for I := 0 to TGocciaSequenceExpression(AExpr).Expressions.Count - 1 do
      ValidateStrictEvalAssignmentExpression(
        TGocciaSequenceExpression(AExpr).Expressions[I]);
  end
  else if AExpr is TGocciaUnaryExpression then
  begin
    UnaryExpr := TGocciaUnaryExpression(AExpr);
    if (UnaryExpr.Operator = gttDelete) and
       (UnaryExpr.Operand is TGocciaIdentifierExpression) then
      raise TGocciaSyntaxError.Create(
        'Delete of an unqualified identifier in strict mode',
        AExpr.Line, AExpr.Column, '', nil);
    ValidateStrictEvalAssignmentExpression(UnaryExpr.Operand);
  end
  else if AExpr is TGocciaArrayExpression then
  begin
    ArrayExpr := TGocciaArrayExpression(AExpr);
    for I := 0 to ArrayExpr.Elements.Count - 1 do
      ValidateStrictEvalAssignmentExpression(ArrayExpr.Elements[I]);
  end
  else if AExpr is TGocciaObjectExpression then
  begin
    ObjectExpr := TGocciaObjectExpression(AExpr);
    for I := 0 to High(ObjectExpr.PropertySourceOrder) do
      case ObjectExpr.PropertySourceOrder[I].PropertyType of
        pstStatic:
          ValidateStrictEvalAssignmentExpression(
            ObjectExpr.PropertySourceOrder[I].Expression);
        pstComputed:
          begin
            Pair := ObjectExpr.ComputedPropertiesInOrder[
              ObjectExpr.PropertySourceOrder[I].ComputedIndex];
            ValidateStrictEvalAssignmentExpression(Pair.Key);
            ValidateStrictEvalAssignmentExpression(Pair.Value);
          end;
        pstComputedGetter,
        pstComputedSetter:
          begin
            Pair := ObjectExpr.ComputedPropertiesInOrder[
              ObjectExpr.PropertySourceOrder[I].ComputedIndex];
            ValidateStrictEvalAssignmentExpression(Pair.Key);
          end;
      end;
  end
  else if AExpr is TGocciaYieldExpression then
    ValidateStrictEvalAssignmentExpression(TGocciaYieldExpression(AExpr).Operand)
  else if AExpr is TGocciaAwaitExpression then
    ValidateStrictEvalAssignmentExpression(TGocciaAwaitExpression(AExpr).Operand)
  else if AExpr is TGocciaConditionalExpression then
  begin
    ValidateStrictEvalAssignmentExpression(
      TGocciaConditionalExpression(AExpr).Condition);
    ValidateStrictEvalAssignmentExpression(
      TGocciaConditionalExpression(AExpr).Consequent);
    ValidateStrictEvalAssignmentExpression(
      TGocciaConditionalExpression(AExpr).Alternate);
  end
  else if AExpr is TGocciaNewExpression then
  begin
    NewExpr := TGocciaNewExpression(AExpr);
    ValidateStrictEvalAssignmentExpression(NewExpr.Callee);
    for I := 0 to NewExpr.Arguments.Count - 1 do
      ValidateStrictEvalAssignmentExpression(NewExpr.Arguments[I]);
  end
  else if AExpr is TGocciaSpreadExpression then
    ValidateStrictEvalAssignmentExpression(TGocciaSpreadExpression(AExpr).Argument)
  else if AExpr is TGocciaTemplateWithInterpolationExpression then
  begin
    for I := 0 to TGocciaTemplateWithInterpolationExpression(AExpr).Parts.Count - 1 do
      ValidateStrictEvalAssignmentExpression(
        TGocciaTemplateWithInterpolationExpression(AExpr).Parts[I]);
  end
  else if AExpr is TGocciaTaggedTemplateExpression then
  begin
    ValidateStrictEvalAssignmentExpression(
      TGocciaTaggedTemplateExpression(AExpr).Tag);
    for I := 0 to TGocciaTaggedTemplateExpression(AExpr).Expressions.Count - 1 do
      ValidateStrictEvalAssignmentExpression(
        TGocciaTaggedTemplateExpression(AExpr).Expressions[I]);
  end
  else if AExpr is TGocciaPrivateMemberExpression then
    ValidateStrictEvalAssignmentExpression(
      TGocciaPrivateMemberExpression(AExpr).ObjectExpr)
  else if AExpr is TGocciaPrivatePropertyAssignmentExpression then
  begin
    ValidateStrictEvalAssignmentExpression(
      TGocciaPrivatePropertyAssignmentExpression(AExpr).ObjectExpr);
    ValidateStrictEvalAssignmentExpression(
      TGocciaPrivatePropertyAssignmentExpression(AExpr).Value);
  end
  else if AExpr is TGocciaPrivatePropertyCompoundAssignmentExpression then
  begin
    ValidateStrictEvalAssignmentExpression(
      TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).ObjectExpr);
    ValidateStrictEvalAssignmentExpression(
      TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).Value);
  end;
end;

procedure ValidateStrictEvalAssignmentPattern(
  const APattern: TGocciaDestructuringPattern);
var
  ArrayPattern: TGocciaArrayDestructuringPattern;
  AssignmentPattern: TGocciaAssignmentDestructuringPattern;
  ObjectPattern: TGocciaObjectDestructuringPattern;
  Prop: TGocciaDestructuringProperty;
  RestPattern: TGocciaRestDestructuringPattern;
  I: Integer;
begin
  CheckEarlyErrorStack(APattern);
  if not Assigned(APattern) then
    Exit;

  if APattern is TGocciaIdentifierDestructuringPattern then
    RejectStrictEvalAssignmentName(
      TGocciaIdentifierDestructuringPattern(APattern).Name,
      APattern.Line, APattern.Column)
  else if APattern is TGocciaArrayDestructuringPattern then
  begin
    ArrayPattern := TGocciaArrayDestructuringPattern(APattern);
    for I := 0 to ArrayPattern.Elements.Count - 1 do
      ValidateStrictEvalAssignmentPattern(ArrayPattern.Elements[I]);
  end
  else if APattern is TGocciaObjectDestructuringPattern then
  begin
    ObjectPattern := TGocciaObjectDestructuringPattern(APattern);
    for I := 0 to ObjectPattern.Properties.Count - 1 do
    begin
      Prop := ObjectPattern.Properties[I];
      if Prop.Computed then
        ValidateStrictEvalAssignmentExpression(Prop.KeyExpression);
      ValidateStrictEvalAssignmentPattern(Prop.Pattern);
    end;
  end
  else if APattern is TGocciaAssignmentDestructuringPattern then
  begin
    AssignmentPattern := TGocciaAssignmentDestructuringPattern(APattern);
    ValidateStrictEvalAssignmentPattern(AssignmentPattern.Left);
    ValidateStrictEvalAssignmentExpression(AssignmentPattern.Right);
  end
  else if APattern is TGocciaRestDestructuringPattern then
  begin
    RestPattern := TGocciaRestDestructuringPattern(APattern);
    ValidateStrictEvalAssignmentPattern(RestPattern.Argument);
  end
  else if APattern is TGocciaMemberExpressionDestructuringPattern then
    ValidateStrictEvalAssignmentExpression(
      TGocciaMemberExpressionDestructuringPattern(APattern).Expression)
  else if APattern is TGocciaPrivateMemberExpressionDestructuringPattern then
    ValidateStrictEvalAssignmentExpression(
      TGocciaPrivateMemberExpressionDestructuringPattern(APattern).Expression);
end;

procedure ValidateEvalEarlyErrors(const AProgram: TGocciaProgram;
  const AStrictEval, AAllowNewTarget, AAllowSuperProperty,
  AAllowSuperCall: Boolean);
var
  I: Integer;
begin
  if AStrictEval then
    for I := 0 to AProgram.Body.Count - 1 do
      ValidateStrictEvalAssignmentStatement(AProgram.Body[I]);

  for I := 0 to AProgram.Body.Count - 1 do
    ValidateEvalEarlyErrorStatement(AProgram.Body[I], AStrictEval,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
end;

{ A `return`, or an unlabeled `break`/`continue` with no enclosing iteration or
  switch, is a Script early error (a SyntaxError) — eval code is never a function
  body, loop, or switch. Validating here, before any evaluation, makes them
  errors of the compile stage, which ShadowRealm.prototype.evaluate reports as a
  caller-realm SyntaxError rather than wrapping as a TypeError. Recursion walks
  only statements, so it stops at function/class boundaries (which are
  expressions) — a `return`/`break`/`continue` inside a nested function is never
  flagged. Labeled break/continue are left to the parser, which validates their
  targets and rejects undefined labels. }
procedure RejectEvalControlFlow(const AMessage: string;
  const AStmt: TGocciaStatement);
begin
  raise TGocciaSyntaxError.Create(AMessage, AStmt.Line, AStmt.Column, '', nil,
    SSuggestExpressionExpected);
end;

procedure ValidateEvalControlFlowStatement(const AStmt: TGocciaStatement;
  const AInIteration, AInSwitch: Boolean);
var
  I, J: Integer;
  BlockStmt: TGocciaBlockStatement;
  IfStmt: TGocciaIfStatement;
  TryStmt: TGocciaTryStatement;
  SwitchStmt: TGocciaSwitchStatement;
begin
  if not Assigned(AStmt) then
    Exit;

  if AStmt is TGocciaReturnStatement then
    RejectEvalControlFlow('Illegal return statement', AStmt)
  else if AStmt is TGocciaBreakStatement then
  begin
    if (TGocciaBreakStatement(AStmt).TargetLabel = '') and
       not (AInIteration or AInSwitch) then
      RejectEvalControlFlow(SErrorIllegalBreakStatement, AStmt);
  end
  else if AStmt is TGocciaContinueStatement then
  begin
    if (TGocciaContinueStatement(AStmt).TargetLabel = '') and
       not AInIteration then
      RejectEvalControlFlow(SErrorIllegalContinueStatement, AStmt);
  end
  else if AStmt is TGocciaBlockStatement then
  begin
    BlockStmt := TGocciaBlockStatement(AStmt);
    for I := 0 to BlockStmt.Nodes.Count - 1 do
      if BlockStmt.Nodes[I] is TGocciaStatement then
        ValidateEvalControlFlowStatement(
          TGocciaStatement(BlockStmt.Nodes[I]), AInIteration, AInSwitch);
  end
  else if AStmt is TGocciaIfStatement then
  begin
    IfStmt := TGocciaIfStatement(AStmt);
    ValidateEvalControlFlowStatement(IfStmt.Consequent, AInIteration, AInSwitch);
    ValidateEvalControlFlowStatement(IfStmt.Alternate, AInIteration, AInSwitch);
  end
  else if AStmt is TGocciaForStatement then
    ValidateEvalControlFlowStatement(
      TGocciaForStatement(AStmt).Body, True, AInSwitch)
  else if AStmt is TGocciaForOfStatement then
    ValidateEvalControlFlowStatement(
      TGocciaForOfStatement(AStmt).Body, True, AInSwitch)
  else if AStmt is TGocciaForInStatement then
    ValidateEvalControlFlowStatement(
      TGocciaForInStatement(AStmt).Body, True, AInSwitch)
  else if AStmt is TGocciaWhileStatement then
    ValidateEvalControlFlowStatement(
      TGocciaWhileStatement(AStmt).Body, True, AInSwitch)
  else if AStmt is TGocciaDoWhileStatement then
    ValidateEvalControlFlowStatement(
      TGocciaDoWhileStatement(AStmt).Body, True, AInSwitch)
  else if AStmt is TGocciaWithStatement then
    ValidateEvalControlFlowStatement(
      TGocciaWithStatement(AStmt).Body, AInIteration, AInSwitch)
  else if AStmt is TGocciaTryStatement then
  begin
    TryStmt := TGocciaTryStatement(AStmt);
    ValidateEvalControlFlowStatement(TryStmt.Block, AInIteration, AInSwitch);
    ValidateEvalControlFlowStatement(TryStmt.CatchBlock, AInIteration, AInSwitch);
    ValidateEvalControlFlowStatement(
      TryStmt.FinallyBlock, AInIteration, AInSwitch);
  end
  else if AStmt is TGocciaSwitchStatement then
  begin
    SwitchStmt := TGocciaSwitchStatement(AStmt);
    for I := 0 to SwitchStmt.Cases.Count - 1 do
      for J := 0 to SwitchStmt.Cases[I].Consequent.Count - 1 do
        if SwitchStmt.Cases[I].Consequent[J] is TGocciaStatement then
          ValidateEvalControlFlowStatement(
            TGocciaStatement(SwitchStmt.Cases[I].Consequent[J]),
            AInIteration, True);
  end;
end;

procedure ValidateEvalControlFlow(const AProgram: TGocciaProgram);
var
  I: Integer;
begin
  for I := 0 to AProgram.Body.Count - 1 do
    ValidateEvalControlFlowStatement(AProgram.Body[I], False, False);
end;

procedure ValidateEvalBlockStatement(const AStmt: TGocciaStatement;
  const AStrict: Boolean); forward;

// The declarations of one block or case block, ANodes.
procedure ValidateEvalBlockNodes(const ANodes: array of TGocciaASTNode;
  const AStrict: Boolean);
var
  Names, FunctionNames, StatementNames: TUnicodeStringList;
  Statement: TObjectList<TGocciaStatement>;
  Declaration: TGocciaFunctionDeclaration;
  I, J: Integer;

  procedure RaiseAlreadyDeclared(const AName: string;
    const ANode: TGocciaASTNode);
  begin
    raise TGocciaSyntaxError.Create(
      Format(SErrorIdentifierAlreadyDeclared, [AName]), ANode.Line,
      ANode.Column, '', nil, SSuggestAlreadyDeclared);
  end;

begin
  // A name needs two declarations to repeat; a block of fewer nodes only has
  // nested blocks to check.
  if Length(ANodes) < 2 then
  begin
    for I := 0 to High(ANodes) do
      if ANodes[I] is TGocciaStatement then
        ValidateEvalBlockStatement(TGocciaStatement(ANodes[I]), AStrict);
    Exit;
  end;
  Names := TUnicodeStringList.Create;
  FunctionNames := TUnicodeStringList.Create;
  StatementNames := TUnicodeStringList.Create;
  Statement := TObjectList<TGocciaStatement>.Create(False);
  try
    for I := 0 to High(ANodes) do
    begin
      if not (ANodes[I] is TGocciaStatement) then
        Continue;
      Declaration := EvalFunctionDeclarationFromNode(ANodes[I]);
      if Assigned(Declaration) then
      begin
        if Names.HasString(Declaration.Name) then
        begin
          if AStrict or (not FunctionNames.HasString(Declaration.Name)) then
            RaiseAlreadyDeclared(Declaration.Name, ANodes[I]);
        end
        else
        begin
          Names.Add(Declaration.Name);
          FunctionNames.Add(Declaration.Name);
        end;
        Continue;
      end;
      Statement.Clear;
      Statement.Add(TGocciaStatement(ANodes[I]));
      StatementNames.Clear;
      CollectTopLevelEvalLexicalNames(Statement, StatementNames);
      for J := 0 to StatementNames.Count - 1 do
      begin
        if Names.HasString(StatementNames[J]) then
          RaiseAlreadyDeclared(StatementNames[J], ANodes[I]);
        Names.Add(StatementNames[J]);
      end;
    end;
    for I := 0 to High(ANodes) do
      if ANodes[I] is TGocciaStatement then
        ValidateEvalBlockStatement(TGocciaStatement(ANodes[I]), AStrict);
  finally
    Statement.Free;
    StatementNames.Free;
    FunctionNames.Free;
    Names.Free;
  end;
end;

procedure ValidateEvalFunctionBody(const ABody: TGocciaASTNode;
  const AStrict: Boolean);
var
  Nodes: array of TGocciaASTNode;
  I: Integer;
begin
  if not (ABody is TGocciaBlockStatement) then
    Exit;
  SetLength(Nodes, TGocciaBlockStatement(ABody).Nodes.Count);
  for I := 0 to High(Nodes) do
    Nodes[I] := TGocciaBlockStatement(ABody).Nodes[I];
  // The body's own top level is checked by the function's declaration
  // instantiation; its blocks are checked here.
  for I := 0 to High(Nodes) do
    if Nodes[I] is TGocciaStatement then
      ValidateEvalBlockStatement(TGocciaStatement(Nodes[I]), AStrict);
end;

procedure ValidateEvalBlockStatement(const AStmt: TGocciaStatement;
  const AStrict: Boolean);
var
  Nodes: array of TGocciaASTNode;
  I, J, Count: Integer;
  SwitchStmt: TGocciaSwitchStatement;
  TryStmt: TGocciaTryStatement;
  Declaration: TGocciaFunctionDeclaration;
begin
  if not Assigned(AStmt) then
    Exit;
  Declaration := EvalFunctionDeclarationFromNode(AStmt);
  if Assigned(Declaration) then
  begin
    if Assigned(Declaration.FunctionExpression) then
      ValidateEvalFunctionBody(Declaration.FunctionExpression.Body, AStrict);
  end
  else if AStmt is TGocciaBlockStatement then
  begin
    SetLength(Nodes, TGocciaBlockStatement(AStmt).Nodes.Count);
    for I := 0 to High(Nodes) do
      Nodes[I] := TGocciaBlockStatement(AStmt).Nodes[I];
    ValidateEvalBlockNodes(Nodes, AStrict);
  end
  else if AStmt is TGocciaIfStatement then
  begin
    ValidateEvalBlockStatement(TGocciaIfStatement(AStmt).Consequent, AStrict);
    ValidateEvalBlockStatement(TGocciaIfStatement(AStmt).Alternate, AStrict);
  end
  else if AStmt is TGocciaForStatement then
    ValidateEvalBlockStatement(TGocciaForStatement(AStmt).Body, AStrict)
  else if AStmt is TGocciaForOfStatement then
    ValidateEvalBlockStatement(TGocciaForOfStatement(AStmt).Body, AStrict)
  else if AStmt is TGocciaForInStatement then
    ValidateEvalBlockStatement(TGocciaForInStatement(AStmt).Body, AStrict)
  else if AStmt is TGocciaWhileStatement then
    ValidateEvalBlockStatement(TGocciaWhileStatement(AStmt).Body, AStrict)
  else if AStmt is TGocciaDoWhileStatement then
    ValidateEvalBlockStatement(TGocciaDoWhileStatement(AStmt).Body, AStrict)
  else if AStmt is TGocciaWithStatement then
    ValidateEvalBlockStatement(TGocciaWithStatement(AStmt).Body, AStrict)
  else if AStmt is TGocciaTryStatement then
  begin
    TryStmt := TGocciaTryStatement(AStmt);
    ValidateEvalBlockStatement(TryStmt.Block, AStrict);
    ValidateEvalBlockStatement(TryStmt.CatchBlock, AStrict);
    ValidateEvalBlockStatement(TryStmt.FinallyBlock, AStrict);
  end
  else if AStmt is TGocciaSwitchStatement then
  begin
    // §14.12.1: the case clauses share one block.
    SwitchStmt := TGocciaSwitchStatement(AStmt);
    Count := 0;
    for I := 0 to SwitchStmt.Cases.Count - 1 do
      Inc(Count, SwitchStmt.Cases[I].Consequent.Count);
    SetLength(Nodes, Count);
    Count := 0;
    for I := 0 to SwitchStmt.Cases.Count - 1 do
      for J := 0 to SwitchStmt.Cases[I].Consequent.Count - 1 do
      begin
        Nodes[Count] := SwitchStmt.Cases[I].Consequent[J];
        Inc(Count);
      end;
    ValidateEvalBlockNodes(Nodes, AStrict);
  end;
end;

procedure ValidateEvalBlockDeclarations(const AProgram: TGocciaProgram;
  const AStrictEval: Boolean);
var
  I: Integer;
begin
  for I := 0 to AProgram.Body.Count - 1 do
    ValidateEvalBlockStatement(AProgram.Body[I], AStrictEval);
end;

procedure CollectGlobalEvalDeclarationNames(const AProgram: TGocciaProgram;
  const AMode: TGocciaVarBindingNameCollectionMode;
  const AVarNames, AFunctionNames, ADeclaredVarNames: TUnicodeStringList);
var
  Declaration: TGocciaFunctionDeclaration;
  I: Integer;
begin
  CollectVarBindingNamesFromStatements(AProgram.Body, AVarNames, AMode);
  for I := AProgram.Body.Count - 1 downto 0 do
  begin
    Declaration := EvalFunctionDeclarationFromNode(AProgram.Body[I]);
    if Assigned(Declaration) and
       (not AFunctionNames.HasString(Declaration.Name)) then
      AFunctionNames.Insert(0, Declaration.Name);
  end;
  for I := 0 to AVarNames.Count - 1 do
    if not AFunctionNames.HasString(AVarNames[I]) then
      AddUniqueEvalName(ADeclaredVarNames, AVarNames[I]);
end;

procedure CheckEvalLexicalDeclarationConflicts(const AProgram: TGocciaProgram;
  const AVarNames: TUnicodeStringList);
var
  LexicalNames, StatementNames: TUnicodeStringList;
  Statement: TObjectList<TGocciaStatement>;
  I, J: Integer;

  procedure RaiseAlreadyDeclared(const AName: string);
  begin
    raise TGocciaSyntaxError.Create(
      Format(SErrorIdentifierAlreadyDeclared, [AName]), 0, 0, '', nil,
      SSuggestAlreadyDeclared);
  end;

begin
  LexicalNames := TUnicodeStringList.Create;
  StatementNames := TUnicodeStringList.Create;
  Statement := TObjectList<TGocciaStatement>.Create(False);
  try
    for I := 0 to AProgram.Body.Count - 1 do
    begin
      Statement.Clear;
      Statement.Add(AProgram.Body[I]);
      StatementNames.Clear;
      CollectTopLevelEvalLexicalNames(Statement, StatementNames);
      for J := 0 to StatementNames.Count - 1 do
      begin
        if LexicalNames.HasString(StatementNames[J]) then
          RaiseAlreadyDeclared(StatementNames[J]);
        LexicalNames.Add(StatementNames[J]);
      end;
    end;
    for I := 0 to LexicalNames.Count - 1 do
      if AVarNames.HasString(LexicalNames[I]) then
        RaiseAlreadyDeclared(LexicalNames[I]);
  finally
    Statement.Free;
    StatementNames.Free;
    LexicalNames.Free;
  end;
end;

end.
