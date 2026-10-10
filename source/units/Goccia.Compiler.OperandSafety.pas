unit Goccia.Compiler.OperandSafety;

{$I Goccia.inc}

// Syntactic proofs behind reading a let binding or a parameter straight from
// its register (see TryResolveSettledLocalName in Goccia.Compiler.Expressions),
// and behind evaluating a parameter's initializer straight into the
// parameter's register (see EmitParameterInitialization).
//
// The proofs are allowlists. A node is accepted only when its class is listed
// here and every child that can hold code has been accepted too; a class that
// is not listed, including one added to the AST later, is rejected. Rejecting
// costs a register copy. Accepting wrongly would read a stale value, so the
// default has to be the copy.
//
// The compiler has older walkers that ask related questions as deny-lists
// (StatementNeedsPerIterationEnvironment, ExpressionCreatesClosureBoundary,
// ExpressionContainsDirectEval). They are not reused here: for them a node
// class nobody listed means "no closure, no eval", which is the unsafe answer
// for this purpose.

interface

uses
  Goccia.AST.Expressions,
  Goccia.AST.Node;

// True when running ANode cannot create a closure and cannot call direct eval.
// ANode is a loop statement. Function, method, accessor and class forms are
// the constructs that create closures; they are rejected by not being listed.
function StatementCreatesNoClosure(const ANode: TGocciaASTNode): Boolean;

// True when evaluating AExpr cannot write the register of a local of the
// function being compiled: it holds no assignment to an identifier, no
// increment or decrement of one, no destructuring assignment, no direct eval,
// and no suspension point. A call is accepted. The callee runs in its own
// register window, and a closure over this function's locals writes their
// cells, never their registers. nil, an operand that does not exist, is
// accepted.
function ExpressionKeepsLocalRegisters(const AExpr: TGocciaASTNode): Boolean;

// True when nothing evaluated up to the end of the initializer of the simple
// parameter AParams[AIndex] can read or write that parameter's binding: no
// expression in the parameters up to and including it names the parameter,
// as an identifier, an assignment target or a destructuring target, none calls
// direct eval, and every arrow or function expression there has source text
// in which the name, `eval` and a `\u` escape do not occur. Other function,
// method, accessor and class forms are rejected. Only then may the
// initializer use the parameter's register as its destination, where it
// writes intermediate values before it completes; otherwise one of them
// replaces the TDZ hole a read or a write of the binding has to find.
// Parameters before AIndex count because a closure created in their
// initializers reaches the binding when the initializer calls it.
function ParameterInitializerAvoidsOwnBinding(
  const AParams: TGocciaParameterArray; const AIndex: Integer): Boolean;

implementation

uses
  Generics.Collections,

  Goccia.AST.Statements;

const
  // An operand is checked once per enclosing operator, so a deeply nested
  // right-leaning expression would otherwise be walked a quadratic number of
  // times. A subtree larger than this keeps the copy.
  OPERAND_NODE_BUDGET = 64;

type
  TScan = record
    // False for the loop proof, True for the operand proof.
    RejectLocalWrites: Boolean;
    // Nodes left to visit; negative means unlimited.
    Budget: Integer;
    // Empty for the loop and operand proofs. For the parameter initializer
    // proof, the binding name no node may reach; an arrow or function
    // expression is then accepted when its source text cannot name the
    // binding.
    AvoidName: string;
  end;

function IsAsciiIdentifierPart(const AChar: Char): Boolean;
begin
  Result := AChar in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$'];
end;

// Over-approximates whether code with the source text ASourceText can reach
// the binding AName. Any occurrence of AName that does not continue an ASCII
// identifier counts, including one in a comment, a string or after a dot. A
// direct eval can name the binding at run time, and an IdentifierName may
// spell it with \u escapes. Empty source text is answered conservatively.
function SourceTextMayReachName(const ASourceText, AName: string): Boolean;
var
  Index, NameLength, SourceLength: Integer;
begin
  if (ASourceText = '') or (AName = '') or
     (Pos('eval', ASourceText) > 0) or (Pos('\u', ASourceText) > 0) then
    Exit(True);
  NameLength := Length(AName);
  SourceLength := Length(ASourceText);
  Index := Pos(AName, ASourceText);
  while Index > 0 do
  begin
    if ((Index = 1) or not IsAsciiIdentifierPart(ASourceText[Index - 1])) and
       ((Index + NameLength > SourceLength) or
        not IsAsciiIdentifierPart(ASourceText[Index + NameLength])) then
      Exit(True);
    Index := Pos(AName, ASourceText, Index + 1);
  end;
  Result := False;
end;

function ScanReachesName(const AScan: TScan; const AName: string): Boolean;
begin
  Result := (AScan.AvoidName <> '') and (AName = AScan.AvoidName);
end;

function ScanAcceptsFunctionSource(const AScan: TScan;
  const ASourceText: string): Boolean;
begin
  Result := (AScan.AvoidName <> '') and
    not SourceTextMayReachName(ASourceText, AScan.AvoidName);
end;

function ScanNode(var AScan: TScan; const ANode: TGocciaASTNode): Boolean;
  forward;

function ScanExpressions(var AScan: TScan;
  const AList: TObjectList<TGocciaExpression>): Boolean;
var
  I: Integer;
begin
  if Assigned(AList) then
    for I := 0 to AList.Count - 1 do
      if not ScanNode(AScan, AList[I]) then
        Exit(False);
  Result := True;
end;

function ScanPattern(var AScan: TScan;
  const APattern: TGocciaDestructuringPattern): Boolean;
var
  Kind: TClass;
  ArrayPattern: TGocciaArrayDestructuringPattern;
  ObjectPattern: TGocciaObjectDestructuringPattern;
  Prop: TGocciaDestructuringProperty;
  I: Integer;
begin
  if not Assigned(APattern) then
    Exit(True);

  Kind := APattern.ClassType;
  if Kind = TGocciaIdentifierDestructuringPattern then
    Result := not ScanReachesName(AScan,
      TGocciaIdentifierDestructuringPattern(APattern).Name)
  else if Kind = TGocciaArrayDestructuringPattern then
  begin
    ArrayPattern := TGocciaArrayDestructuringPattern(APattern);
    for I := 0 to ArrayPattern.Elements.Count - 1 do
      if not ScanPattern(AScan, ArrayPattern.Elements[I]) then
        Exit(False);
    Result := True;
  end
  else if Kind = TGocciaObjectDestructuringPattern then
  begin
    ObjectPattern := TGocciaObjectDestructuringPattern(APattern);
    for I := 0 to ObjectPattern.Properties.Count - 1 do
    begin
      Prop := ObjectPattern.Properties[I];
      if not ScanNode(AScan, Prop.KeyExpression) or
         not ScanPattern(AScan, Prop.Pattern) then
        Exit(False);
    end;
    Result := True;
  end
  else if Kind = TGocciaRestDestructuringPattern then
    Result := ScanPattern(AScan,
      TGocciaRestDestructuringPattern(APattern).Argument)
  else if Kind = TGocciaAssignmentDestructuringPattern then
    Result := ScanPattern(AScan,
      TGocciaAssignmentDestructuringPattern(APattern).Left) and
      ScanNode(AScan, TGocciaAssignmentDestructuringPattern(APattern).Right)
  else if Kind = TGocciaMemberExpressionDestructuringPattern then
    Result := ScanNode(AScan,
      TGocciaMemberExpressionDestructuringPattern(APattern).Expression)
  else if Kind = TGocciaPrivateMemberExpressionDestructuringPattern then
    Result := ScanNode(AScan,
      TGocciaPrivateMemberExpressionDestructuringPattern(APattern).Expression)
  else
    Result := False;
end;

function ScanObjectLiteral(var AScan: TScan;
  const AExpr: TGocciaObjectExpression): Boolean;
var
  Order: TArray<TGocciaPropertySourceOrder>;
  Pair: TPair<TGocciaExpression, TGocciaExpression>;
  Value: TGocciaExpression;
  I: Integer;
begin
  // The compiler reads properties through the source-order table and falls
  // back to the name table when that is empty. Only the first shape is proved
  // here, with every entry accounted for.
  Order := AExpr.PropertySourceOrder;
  if (Length(Order) = 0) and (AExpr.Properties.Count > 0) then
    Exit(False);

  for I := 0 to High(Order) do
    case Order[I].PropertyType of
      pstStatic:
      begin
        Value := Order[I].Expression;
        if not Assigned(Value) and
           not AExpr.Properties.TryGetValue(Order[I].StaticKey, Value) then
          Value := nil;
        if not ScanNode(AScan, Value) then
          Exit(False);
      end;
      pstComputed:
      begin
        if (Order[I].ComputedIndex < 0) or
           (Order[I].ComputedIndex > High(AExpr.ComputedPropertiesInOrder)) then
          Exit(False);
        Pair := AExpr.ComputedPropertiesInOrder[Order[I].ComputedIndex];
        if not ScanNode(AScan, Pair.Key) or not ScanNode(AScan, Pair.Value) then
          Exit(False);
      end;
    else
      // Getters and setters are closures.
      Exit(False);
    end;
  Result := True;
end;

function ScanExpression(var AScan: TScan;
  const AExpr: TGocciaExpression): Boolean;
var
  Kind: TClass;
  Call: TGocciaCallExpression;
  Member: TGocciaMemberExpression;
  Conditional: TGocciaConditionalExpression;
begin
  Kind := AExpr.ClassType;

  if Kind = TGocciaIdentifierExpression then
    Result := not ScanReachesName(AScan, TGocciaIdentifierExpression(AExpr).Name)
  else if (Kind = TGocciaLiteralExpression) or
     (Kind = TGocciaThisExpression) or
     (Kind = TGocciaTemplateLiteralExpression) or
     (Kind = TGocciaRegexLiteralExpression) or
     (Kind = TGocciaSuperExpression) or
     (Kind = TGocciaImportMetaExpression) or
     (Kind = TGocciaNewTargetExpression) or
     (Kind = TGocciaHoleExpression) then
    Result := True
  else if Kind = TGocciaBinaryExpression then
    Result := ScanNode(AScan, TGocciaBinaryExpression(AExpr).Left) and
      ScanNode(AScan, TGocciaBinaryExpression(AExpr).Right)
  else if Kind = TGocciaMemberExpression then
  begin
    Member := TGocciaMemberExpression(AExpr);
    Result := ScanNode(AScan, Member.ObjectExpr) and
      ScanNode(AScan, Member.PropertyExpression);
  end
  else if Kind = TGocciaCallExpression then
  begin
    Call := TGocciaCallExpression(AExpr);
    // Direct eval runs caller-visible code in the caller's own registers.
    if (Call.Callee is TGocciaIdentifierExpression) and
       (TGocciaIdentifierExpression(Call.Callee).Name = 'eval') then
      Exit(False);
    Result := ScanNode(AScan, Call.Callee) and
      ScanExpressions(AScan, Call.Arguments);
  end
  else if Kind = TGocciaUnaryExpression then
    Result := ScanNode(AScan, TGocciaUnaryExpression(AExpr).Operand)
  else if Kind = TGocciaConditionalExpression then
  begin
    Conditional := TGocciaConditionalExpression(AExpr);
    Result := ScanNode(AScan, Conditional.Condition) and
      ScanNode(AScan, Conditional.Consequent) and
      ScanNode(AScan, Conditional.Alternate);
  end
  else if Kind = TGocciaAssignmentExpression then
    Result := not AScan.RejectLocalWrites and
      not ScanReachesName(AScan, TGocciaAssignmentExpression(AExpr).Name) and
      ScanNode(AScan, TGocciaAssignmentExpression(AExpr).Value)
  else if Kind = TGocciaCompoundAssignmentExpression then
    Result := not AScan.RejectLocalWrites and
      not ScanReachesName(AScan,
        TGocciaCompoundAssignmentExpression(AExpr).Name) and
      ScanNode(AScan, TGocciaCompoundAssignmentExpression(AExpr).Value)
  else if Kind = TGocciaIncrementExpression then
    Result := not (AScan.RejectLocalWrites and
      (TGocciaIncrementExpression(AExpr).Operand is
        TGocciaIdentifierExpression)) and
      ScanNode(AScan, TGocciaIncrementExpression(AExpr).Operand)
  else if Kind = TGocciaPropertyAssignmentExpression then
    Result := ScanNode(AScan,
      TGocciaPropertyAssignmentExpression(AExpr).ObjectExpr) and
      ScanNode(AScan, TGocciaPropertyAssignmentExpression(AExpr).Value)
  else if Kind = TGocciaComputedPropertyAssignmentExpression then
    Result := ScanNode(AScan,
      TGocciaComputedPropertyAssignmentExpression(AExpr).ObjectExpr) and
      ScanNode(AScan,
        TGocciaComputedPropertyAssignmentExpression(AExpr).PropertyExpression) and
      ScanNode(AScan, TGocciaComputedPropertyAssignmentExpression(AExpr).Value)
  else if Kind = TGocciaPropertyCompoundAssignmentExpression then
    Result := ScanNode(AScan,
      TGocciaPropertyCompoundAssignmentExpression(AExpr).ObjectExpr) and
      ScanNode(AScan, TGocciaPropertyCompoundAssignmentExpression(AExpr).Value)
  else if Kind = TGocciaComputedPropertyCompoundAssignmentExpression then
    Result := ScanNode(AScan,
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).ObjectExpr) and
      ScanNode(AScan,
        TGocciaComputedPropertyCompoundAssignmentExpression(AExpr)
          .PropertyExpression) and
      ScanNode(AScan,
        TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).Value)
  else if Kind = TGocciaNewExpression then
    Result := ScanNode(AScan, TGocciaNewExpression(AExpr).Callee) and
      ScanExpressions(AScan, TGocciaNewExpression(AExpr).Arguments)
  else if Kind = TGocciaArrayExpression then
    Result := ScanExpressions(AScan, TGocciaArrayExpression(AExpr).Elements)
  else if Kind = TGocciaObjectExpression then
    Result := ScanObjectLiteral(AScan, TGocciaObjectExpression(AExpr))
  else if Kind = TGocciaSpreadExpression then
    Result := ScanNode(AScan, TGocciaSpreadExpression(AExpr).Argument)
  else if Kind = TGocciaSequenceExpression then
    Result := ScanExpressions(AScan,
      TGocciaSequenceExpression(AExpr).Expressions)
  else if Kind = TGocciaTemplateWithInterpolationExpression then
    Result := ScanExpressions(AScan,
      TGocciaTemplateWithInterpolationExpression(AExpr).Parts)
  else if Kind = TGocciaTaggedTemplateExpression then
    Result := ScanNode(AScan, TGocciaTaggedTemplateExpression(AExpr).Tag) and
      ScanExpressions(AScan, TGocciaTaggedTemplateExpression(AExpr).Expressions)
  else if Kind = TGocciaPrivateMemberExpression then
    Result := ScanNode(AScan, TGocciaPrivateMemberExpression(AExpr).ObjectExpr)
  else if Kind = TGocciaPrivatePropertyAssignmentExpression then
    Result := ScanNode(AScan,
      TGocciaPrivatePropertyAssignmentExpression(AExpr).ObjectExpr) and
      ScanNode(AScan, TGocciaPrivatePropertyAssignmentExpression(AExpr).Value)
  else if Kind = TGocciaPrivatePropertyCompoundAssignmentExpression then
    Result := ScanNode(AScan,
      TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).ObjectExpr) and
      ScanNode(AScan,
        TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).Value)
  else if Kind = TGocciaImportCallExpression then
    Result := ScanNode(AScan, TGocciaImportCallExpression(AExpr).Specifier) and
      ScanNode(AScan, TGocciaImportCallExpression(AExpr).Options)
  else if Kind = TGocciaAwaitExpression then
    Result := not AScan.RejectLocalWrites and
      ScanNode(AScan, TGocciaAwaitExpression(AExpr).Operand)
  else if Kind = TGocciaYieldExpression then
    Result := not AScan.RejectLocalWrites and
      ScanNode(AScan, TGocciaYieldExpression(AExpr).Operand)
  else if Kind = TGocciaDestructuringAssignmentExpression then
    Result := not AScan.RejectLocalWrites and
      ScanPattern(AScan, TGocciaDestructuringAssignmentExpression(AExpr).Left) and
      ScanNode(AScan, TGocciaDestructuringAssignmentExpression(AExpr).Right)
  else if Kind = TGocciaArrowFunctionExpression then
    Result := ScanAcceptsFunctionSource(AScan,
      TGocciaArrowFunctionExpression(AExpr).SourceText)
  else if Kind = TGocciaFunctionExpression then
    Result := ScanAcceptsFunctionSource(AScan,
      TGocciaFunctionExpression(AExpr).SourceText)
  else
    Result := False;
end;

function ScanVariables(var AScan: TScan;
  const AVariables: TArray<TGocciaVariableInfo>): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AVariables) do
    if not ScanPattern(AScan, AVariables[I].Pattern) or
       not ScanNode(AScan, AVariables[I].Initializer) then
      Exit(False);
  Result := True;
end;

function ScanStatement(var AScan: TScan;
  const AStmt: TGocciaStatement): Boolean;
var
  Kind: TClass;
  Block: TGocciaBlockStatement;
  IfStmt: TGocciaIfStatement;
  ForStmt: TGocciaForStatement;
  ForOf: TGocciaForOfStatement;
  ForIn: TGocciaForInStatement;
  TryStmt: TGocciaTryStatement;
  Switch: TGocciaSwitchStatement;
  I, J: Integer;
begin
  Kind := AStmt.ClassType;

  if Kind = TGocciaExpressionStatement then
    Result := ScanNode(AScan, TGocciaExpressionStatement(AStmt).Expression)
  else if Kind = TGocciaVariableDeclaration then
    Result := ScanVariables(AScan, TGocciaVariableDeclaration(AStmt).Variables)
  else if Kind = TGocciaBlockStatement then
  begin
    Block := TGocciaBlockStatement(AStmt);
    for I := 0 to Block.Nodes.Count - 1 do
      if not ScanNode(AScan, Block.Nodes[I]) then
        Exit(False);
    Result := True;
  end
  else if Kind = TGocciaIfStatement then
  begin
    IfStmt := TGocciaIfStatement(AStmt);
    Result := ScanNode(AScan, IfStmt.Condition) and
      ScanNode(AScan, IfStmt.Consequent) and ScanNode(AScan, IfStmt.Alternate);
  end
  else if Kind = TGocciaForStatement then
  begin
    ForStmt := TGocciaForStatement(AStmt);
    Result := ScanNode(AScan, ForStmt.Init) and
      ScanNode(AScan, ForStmt.Condition) and ScanNode(AScan, ForStmt.Update) and
      ScanNode(AScan, ForStmt.Body);
  end
  else if Kind = TGocciaWhileStatement then
    Result := ScanNode(AScan, TGocciaWhileStatement(AStmt).Condition) and
      ScanNode(AScan, TGocciaWhileStatement(AStmt).Body)
  else if Kind = TGocciaDoWhileStatement then
    Result := ScanNode(AScan, TGocciaDoWhileStatement(AStmt).Body) and
      ScanNode(AScan, TGocciaDoWhileStatement(AStmt).Condition)
  else if Kind = TGocciaForOfStatement then
  begin
    ForOf := TGocciaForOfStatement(AStmt);
    Result := not Assigned(ForOf.MatchPattern) and not ForOf.IsUsing and
      not ForOf.IsAwaitUsing and ScanPattern(AScan, ForOf.BindingPattern) and
      ScanPattern(AScan, ForOf.AssignmentTarget) and
      ScanNode(AScan, ForOf.Iterable) and ScanNode(AScan, ForOf.Body);
  end
  else if Kind = TGocciaForInStatement then
  begin
    ForIn := TGocciaForInStatement(AStmt);
    Result := ScanPattern(AScan, ForIn.BindingPattern) and
      ScanPattern(AScan, ForIn.AssignmentTarget) and
      ScanNode(AScan, ForIn.ObjectExpression) and ScanNode(AScan, ForIn.Body);
  end
  else if Kind = TGocciaReturnStatement then
    Result := ScanNode(AScan, TGocciaReturnStatement(AStmt).Value)
  else if Kind = TGocciaThrowStatement then
    Result := ScanNode(AScan, TGocciaThrowStatement(AStmt).Value)
  else if Kind = TGocciaTryStatement then
  begin
    TryStmt := TGocciaTryStatement(AStmt);
    Result := not Assigned(TryStmt.CatchPattern) and
      ScanPattern(AScan, TryStmt.CatchBindingPattern) and
      ScanNode(AScan, TryStmt.Block) and ScanNode(AScan, TryStmt.CatchBlock) and
      ScanNode(AScan, TryStmt.FinallyBlock);
  end
  else if Kind = TGocciaSwitchStatement then
  begin
    Switch := TGocciaSwitchStatement(AStmt);
    if not ScanNode(AScan, Switch.Discriminant) then
      Exit(False);
    for I := 0 to Switch.Cases.Count - 1 do
    begin
      if not ScanNode(AScan, Switch.Cases[I].Test) then
        Exit(False);
      for J := 0 to Switch.Cases[I].Consequent.Count - 1 do
        if not ScanNode(AScan, Switch.Cases[I].Consequent[J]) then
          Exit(False);
    end;
    Result := True;
  end
  else
    Result := (Kind = TGocciaBreakStatement) or
      (Kind = TGocciaContinueStatement) or (Kind = TGocciaEmptyStatement);
end;

function ScanNode(var AScan: TScan; const ANode: TGocciaASTNode): Boolean;
begin
  if not Assigned(ANode) then
    Exit(True);

  if AScan.Budget = 0 then
    Exit(False);
  if AScan.Budget > 0 then
    Dec(AScan.Budget);

  if ANode is TGocciaExpression then
    Result := ScanExpression(AScan, TGocciaExpression(ANode))
  else if ANode is TGocciaStatement then
    Result := ScanStatement(AScan, TGocciaStatement(ANode))
  else
    Result := False;
end;

function StatementCreatesNoClosure(const ANode: TGocciaASTNode): Boolean;
var
  Scan: TScan;
begin
  Scan.RejectLocalWrites := False;
  Scan.Budget := -1;
  Scan.AvoidName := '';
  Result := ScanNode(Scan, ANode);
end;

function ExpressionKeepsLocalRegisters(const AExpr: TGocciaASTNode): Boolean;
var
  Scan: TScan;
begin
  Scan.RejectLocalWrites := True;
  Scan.Budget := OPERAND_NODE_BUDGET;
  Scan.AvoidName := '';
  Result := ScanNode(Scan, AExpr);
end;

function ParameterInitializerAvoidsOwnBinding(
  const AParams: TGocciaParameterArray; const AIndex: Integer): Boolean;
var
  Scan: TScan;
  I: Integer;
begin
  Scan.RejectLocalWrites := False;
  Scan.Budget := -1;
  Scan.AvoidName := AParams[AIndex].Name;
  if Scan.AvoidName = '' then
    Exit(False);
  for I := 0 to AIndex do
  begin
    if AParams[I].IsPattern and not ScanPattern(Scan, AParams[I].Pattern) then
      Exit(False);
    if not ScanNode(Scan, AParams[I].DefaultValue) then
      Exit(False);
  end;
  Result := True;
end;

end.
