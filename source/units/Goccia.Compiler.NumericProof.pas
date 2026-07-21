unit Goccia.Compiler.NumericProof;

{$I Goccia.inc}

interface

uses
  Goccia.AST.Statements,
  Goccia.Compiler.Context;

// Records a proof only for a const-bound, non-escaping local arrow whose
// recursive body and every external call establish Number arguments.
procedure DiscoverClosedCallNumericProof(const ABlock: TGocciaBlockStatement;
  const AProofs: TNumericParameterProofMap);

implementation

uses
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.Token,
  Goccia.Values.Primitives;

function IsNumberLiteral(const AExpr: TGocciaExpression): Boolean;
begin
  Result := (AExpr is TGocciaLiteralExpression) and
    (TGocciaLiteralExpression(AExpr).Value is TGocciaNumberLiteralValue);
end;

function ParameterIndex(const AParameters: TGocciaParameterArray;
  const AName: string): Integer;
var
  I: Integer;
begin
  for I := 0 to High(AParameters) do
    if AParameters[I].Name = AName then
      Exit(I);
  Result := -1;
end;

function ProvesNumber(const AExpr: TGocciaExpression;
  const AFunctionName: string;
  const AParameters: TGocciaParameterArray): Boolean; forward;

function ProvesPredicate(const AExpr: TGocciaExpression;
  const AFunctionName: string;
  const AParameters: TGocciaParameterArray): Boolean;
var
  Binary: TGocciaBinaryExpression;
begin
  Result := False;
  if not (AExpr is TGocciaBinaryExpression) then
    Exit;
  Binary := TGocciaBinaryExpression(AExpr);
  case Binary.Operator of
    gttEqual, gttNotEqual, gttLooseEqual, gttLooseNotEqual,
    gttLess, gttGreater, gttLessEqual, gttGreaterEqual:
      Result := ProvesNumber(Binary.Left, AFunctionName, AParameters) and
        ProvesNumber(Binary.Right, AFunctionName, AParameters);
  end;
end;

function ProvesNumber(const AExpr: TGocciaExpression;
  const AFunctionName: string;
  const AParameters: TGocciaParameterArray): Boolean;
var
  I: Integer;
  Binary: TGocciaBinaryExpression;
  Conditional: TGocciaConditionalExpression;
  Call: TGocciaCallExpression;
  Callee: TGocciaIdentifierExpression;
begin
  if IsNumberLiteral(AExpr) then
    Exit(True);

  if AExpr is TGocciaIdentifierExpression then
    Exit(ParameterIndex(AParameters,
      TGocciaIdentifierExpression(AExpr).Name) >= 0);

  if AExpr is TGocciaBinaryExpression then
  begin
    Binary := TGocciaBinaryExpression(AExpr);
    case Binary.Operator of
      gttPlus, gttMinus, gttStar, gttSlash, gttPercent, gttPower:
        Exit(ProvesNumber(Binary.Left, AFunctionName, AParameters) and
          ProvesNumber(Binary.Right, AFunctionName, AParameters));
    else
      Exit(False);
    end;
  end;

  if AExpr is TGocciaConditionalExpression then
  begin
    Conditional := TGocciaConditionalExpression(AExpr);
    Exit(ProvesPredicate(Conditional.Condition, AFunctionName, AParameters) and
      ProvesNumber(Conditional.Consequent, AFunctionName, AParameters) and
      ProvesNumber(Conditional.Alternate, AFunctionName, AParameters));
  end;

  if AExpr is TGocciaCallExpression then
  begin
    Call := TGocciaCallExpression(AExpr);
    if Call.Optional or
       not (Call.Callee is TGocciaIdentifierExpression) then
      Exit(False);
    Callee := TGocciaIdentifierExpression(Call.Callee);
    if (Callee.Name <> AFunctionName) or
       (Call.Arguments.Count <> Length(AParameters)) then
      Exit(False);
    for I := 0 to Call.Arguments.Count - 1 do
      if not ProvesNumber(Call.Arguments[I], AFunctionName, AParameters) then
        Exit(False);
    Exit(True);
  end;

  Result := False;
end;

function ScanExpression(const AExpr: TGocciaExpression;
  const AFunctionName: string; const AParameterCount: Integer;
  var ACallCount: Integer): Boolean; forward;

function ScanCall(const ACall: TGocciaCallExpression;
  const AFunctionName: string; const AParameterCount: Integer;
  var ACallCount: Integer): Boolean;
var
  I: Integer;
begin
  if (ACall.Callee is TGocciaIdentifierExpression) and
     (TGocciaIdentifierExpression(ACall.Callee).Name = AFunctionName) then
  begin
    if ACall.Optional or (ACall.Arguments.Count <> AParameterCount) then
      Exit(False);
    for I := 0 to ACall.Arguments.Count - 1 do
      if not IsNumberLiteral(ACall.Arguments[I]) then
        Exit(False);
    Inc(ACallCount);
    Exit(True);
  end;

  if not ScanExpression(ACall.Callee, AFunctionName, AParameterCount,
    ACallCount) then
    Exit(False);
  for I := 0 to ACall.Arguments.Count - 1 do
    if not ScanExpression(ACall.Arguments[I], AFunctionName,
      AParameterCount, ACallCount) then
      Exit(False);
  Result := True;
end;

function ScanExpression(const AExpr: TGocciaExpression;
  const AFunctionName: string; const AParameterCount: Integer;
  var ACallCount: Integer): Boolean;
var
  I: Integer;
  Binary: TGocciaBinaryExpression;
  Sequence: TGocciaSequenceExpression;
  Conditional: TGocciaConditionalExpression;
  Assignment: TGocciaAssignmentExpression;
  Compound: TGocciaCompoundAssignmentExpression;
begin
  if not Assigned(AExpr) then
    Exit(True);
  if AExpr is TGocciaLiteralExpression then
    Exit(True);
  if AExpr is TGocciaIdentifierExpression then
    Exit(TGocciaIdentifierExpression(AExpr).Name <> AFunctionName);
  if AExpr is TGocciaBinaryExpression then
  begin
    Binary := TGocciaBinaryExpression(AExpr);
    Exit(ScanExpression(Binary.Left, AFunctionName, AParameterCount,
      ACallCount) and ScanExpression(Binary.Right, AFunctionName,
      AParameterCount, ACallCount));
  end;
  if AExpr is TGocciaSequenceExpression then
  begin
    Sequence := TGocciaSequenceExpression(AExpr);
    for I := 0 to Sequence.Expressions.Count - 1 do
      if not ScanExpression(Sequence.Expressions[I], AFunctionName,
        AParameterCount, ACallCount) then
        Exit(False);
    Exit(True);
  end;
  if AExpr is TGocciaUnaryExpression then
    Exit(ScanExpression(TGocciaUnaryExpression(AExpr).Operand,
      AFunctionName, AParameterCount, ACallCount));
  if AExpr is TGocciaConditionalExpression then
  begin
    Conditional := TGocciaConditionalExpression(AExpr);
    Exit(ScanExpression(Conditional.Condition, AFunctionName,
      AParameterCount, ACallCount) and
      ScanExpression(Conditional.Consequent, AFunctionName,
        AParameterCount, ACallCount) and
      ScanExpression(Conditional.Alternate, AFunctionName,
        AParameterCount, ACallCount));
  end;
  if AExpr is TGocciaCallExpression then
    Exit(ScanCall(TGocciaCallExpression(AExpr), AFunctionName,
      AParameterCount, ACallCount));
  if AExpr is TGocciaAssignmentExpression then
  begin
    Assignment := TGocciaAssignmentExpression(AExpr);
    Exit((Assignment.Name <> AFunctionName) and
      ScanExpression(Assignment.Value, AFunctionName, AParameterCount,
        ACallCount));
  end;
  if AExpr is TGocciaCompoundAssignmentExpression then
  begin
    Compound := TGocciaCompoundAssignmentExpression(AExpr);
    Exit((Compound.Name <> AFunctionName) and
      ScanExpression(Compound.Value, AFunctionName, AParameterCount,
        ACallCount));
  end;
  if AExpr is TGocciaIncrementExpression then
    Exit(ScanExpression(TGocciaIncrementExpression(AExpr).Operand,
      AFunctionName, AParameterCount, ACallCount));
  Result := False;
end;

function ScanStatement(const ANode: TGocciaASTNode;
  const AFunctionName: string; const AParameterCount: Integer;
  var ACallCount: Integer): Boolean;
var
  I: Integer;
  Block: TGocciaBlockStatement;
  Declaration: TGocciaVariableDeclaration;
  ForStatement: TGocciaForStatement;
  IfStatement: TGocciaIfStatement;
begin
  if ANode is TGocciaExpressionStatement then
    Exit(ScanExpression(TGocciaExpressionStatement(ANode).Expression,
      AFunctionName, AParameterCount, ACallCount));
  if ANode is TGocciaVariableDeclaration then
  begin
    Declaration := TGocciaVariableDeclaration(ANode);
    for I := 0 to High(Declaration.Variables) do
    begin
      if Declaration.Variables[I].Name = AFunctionName then
        Exit(False);
      if Declaration.Variables[I].HasInitializer and
         not ScanExpression(Declaration.Variables[I].Initializer,
           AFunctionName, AParameterCount, ACallCount) then
        Exit(False);
    end;
    Exit(True);
  end;
  if ANode is TGocciaBlockStatement then
  begin
    Block := TGocciaBlockStatement(ANode);
    for I := 0 to Block.Nodes.Count - 1 do
      if not ScanStatement(Block.Nodes[I], AFunctionName, AParameterCount,
        ACallCount) then
        Exit(False);
    Exit(True);
  end;
  if ANode is TGocciaForStatement then
  begin
    ForStatement := TGocciaForStatement(ANode);
    Exit(ScanStatement(ForStatement.Init, AFunctionName, AParameterCount,
      ACallCount) and ScanExpression(ForStatement.Condition, AFunctionName,
      AParameterCount, ACallCount) and ScanExpression(ForStatement.Update,
      AFunctionName, AParameterCount, ACallCount) and
      ScanStatement(ForStatement.Body, AFunctionName, AParameterCount,
        ACallCount));
  end;
  if ANode is TGocciaIfStatement then
  begin
    IfStatement := TGocciaIfStatement(ANode);
    Exit(ScanExpression(IfStatement.Condition, AFunctionName,
      AParameterCount, ACallCount) and ScanStatement(IfStatement.Consequent,
      AFunctionName, AParameterCount, ACallCount) and
      (not Assigned(IfStatement.Alternate) or
       ScanStatement(IfStatement.Alternate, AFunctionName, AParameterCount,
         ACallCount)));
  end;
  if ANode is TGocciaReturnStatement then
    Exit((not TGocciaReturnStatement(ANode).HasExpression) or
      ScanExpression(TGocciaReturnStatement(ANode).Value,
        AFunctionName, AParameterCount, ACallCount));
  if (ANode is TGocciaEmptyStatement) or
     (ANode is TGocciaBreakStatement) or
     (ANode is TGocciaContinueStatement) then
    Exit(True);
  Result := False;
end;

procedure DiscoverClosedCallNumericProof(const ABlock: TGocciaBlockStatement;
  const AProofs: TNumericParameterProofMap);
var
  I: Integer;
  CallCount: Integer;
  Declaration: TGocciaVariableDeclaration;
  Arrow: TGocciaArrowFunctionExpression;
  FunctionName: string;
  Mask: UInt64;
  Proof: TClosedNumericCallProof;
begin
  if not Assigned(AProofs) or (ABlock.Nodes.Count < 2) or
     not (ABlock.Nodes[0] is TGocciaVariableDeclaration) then
    Exit;
  Declaration := TGocciaVariableDeclaration(ABlock.Nodes[0]);
  if not Declaration.IsConst or (Length(Declaration.Variables) <> 1) or
     Declaration.Variables[0].IsPattern or
     not Declaration.Variables[0].HasInitializer or
     not (Declaration.Variables[0].Initializer is
       TGocciaArrowFunctionExpression) then
    Exit;

  FunctionName := Declaration.Variables[0].Name;
  Arrow := TGocciaArrowFunctionExpression(
    Declaration.Variables[0].Initializer);
  if Arrow.IsAsync or (Length(Arrow.Parameters) = 0) or
     (Length(Arrow.Parameters) > 64) or
     not (Arrow.Body is TGocciaExpression) then
    Exit;
  for I := 0 to High(Arrow.Parameters) do
    if Arrow.Parameters[I].IsPattern or Arrow.Parameters[I].IsRest or
       Arrow.Parameters[I].IsOptional or
       Assigned(Arrow.Parameters[I].DefaultValue) or
       (Arrow.Parameters[I].Name = FunctionName) then
      Exit;
  if not ProvesNumber(TGocciaExpression(Arrow.Body), FunctionName,
    Arrow.Parameters) then
    Exit;

  CallCount := 0;
  for I := 1 to ABlock.Nodes.Count - 1 do
    if not ScanStatement(ABlock.Nodes[I], FunctionName,
      Length(Arrow.Parameters), CallCount) then
      Exit;
  if CallCount = 0 then
    Exit;

  if Length(Arrow.Parameters) = 64 then
    Mask := High(UInt64)
  else
    Mask := (UInt64(1) shl Length(Arrow.Parameters)) - 1;
  Proof.ParameterMask := Mask;
  Proof.FunctionName := FunctionName;
  AProofs.AddOrSetValue(Arrow, Proof);
end;

end.
