unit Goccia.AST.BindingPatterns;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements;

procedure CollectPatternBindingNames(const APattern: TGocciaDestructuringPattern;
  const ANames: TStrings; const AUnique: Boolean = False);
function ParameterListBindsName(const AParameters: TGocciaParameterArray;
  const AName: string): Boolean;
procedure CollectVarBindingNamesFromNode(const ANode: TGocciaASTNode;
  const ANames: TStrings;
  const AIncludeAnnexBBlockFunctions: Boolean = True);
procedure CollectVarBindingNamesFromStatements(
  const AStatements: TObjectList<TGocciaStatement>; const ANames: TStrings;
  const AIncludeAnnexBBlockFunctions: Boolean = True);
procedure CollectVarBindingNamesFromNodes(
  const ANodes: TObjectList<TGocciaASTNode>; const ANames: TStrings;
  const AIncludeAnnexBBlockFunctions: Boolean = True);

implementation

procedure AddBindingName(const ANames: TStrings; const AName: string;
  const AUnique: Boolean);
begin
  if AName = '' then
    Exit;
  if (not AUnique) or (ANames.IndexOf(AName) < 0) then
    ANames.Add(AName);
end;

procedure CollectPatternBindingNames(const APattern: TGocciaDestructuringPattern;
  const ANames: TStrings; const AUnique: Boolean = False);
var
  ObjPat: TGocciaObjectDestructuringPattern;
  ArrPat: TGocciaArrayDestructuringPattern;
  I: Integer;
begin
  if not Assigned(APattern) then
    Exit;

  if APattern is TGocciaIdentifierDestructuringPattern then
    AddBindingName(ANames,
      TGocciaIdentifierDestructuringPattern(APattern).Name, AUnique)
  else if APattern is TGocciaObjectDestructuringPattern then
  begin
    ObjPat := TGocciaObjectDestructuringPattern(APattern);
    for I := 0 to ObjPat.Properties.Count - 1 do
      CollectPatternBindingNames(ObjPat.Properties[I].Pattern, ANames, AUnique);
  end
  else if APattern is TGocciaArrayDestructuringPattern then
  begin
    ArrPat := TGocciaArrayDestructuringPattern(APattern);
    for I := 0 to ArrPat.Elements.Count - 1 do
      CollectPatternBindingNames(ArrPat.Elements[I], ANames, AUnique);
  end
  else if APattern is TGocciaAssignmentDestructuringPattern then
    CollectPatternBindingNames(
      TGocciaAssignmentDestructuringPattern(APattern).Left, ANames, AUnique)
  else if APattern is TGocciaRestDestructuringPattern then
    CollectPatternBindingNames(
      TGocciaRestDestructuringPattern(APattern).Argument, ANames, AUnique);
end;

function ParameterListBindsName(const AParameters: TGocciaParameterArray;
  const AName: string): Boolean;
var
  I: Integer;
  Names: TStringList;
begin
  for I := 0 to High(AParameters) do
  begin
    if AParameters[I].IsPattern then
    begin
      Names := TStringList.Create;
      try
        Names.CaseSensitive := True;
        CollectPatternBindingNames(AParameters[I].Pattern, Names, True);
        if Names.IndexOf(AName) >= 0 then
          Exit(True);
      finally
        Names.Free;
      end;
    end
    else if AParameters[I].Name = AName then
      Exit(True);
  end;

  Result := False;
end;

procedure CollectVarBindingNamesFromNodeInternal(const ANode: TGocciaASTNode;
  const ANames: TStrings; const AIncludeAnnexBBlockFunctions: Boolean;
  const AAtVarScopedLevel: Boolean);
var
  Block: TGocciaBlockStatement;
  IfStmt: TGocciaIfStatement;
  ForOf: TGocciaForOfStatement;
  ForIn: TGocciaForInStatement;
  ForStmt: TGocciaForStatement;
  WhileStmt: TGocciaWhileStatement;
  DoWhileStmt: TGocciaDoWhileStatement;
  WithStmt: TGocciaWithStatement;
  TryStmt: TGocciaTryStatement;
  SwitchStmt: TGocciaSwitchStatement;
  VarDecl: TGocciaVariableDeclaration;
  DestructDecl: TGocciaDestructuringDeclaration;
  ExportDestructDecl: TGocciaExportDestructuringDeclaration;
  ExportVarDecl: TGocciaExportVariableDeclaration;
  I, J: Integer;
begin
  if not Assigned(ANode) then
    Exit;

  if ANode is TGocciaExportVariableDeclaration then
  begin
    ExportVarDecl := TGocciaExportVariableDeclaration(ANode);
    CollectVarBindingNamesFromNodeInternal(ExportVarDecl.Declaration, ANames,
      AIncludeAnnexBBlockFunctions, AAtVarScopedLevel);
  end
  else if ANode is TGocciaExportDestructuringDeclaration then
  begin
    ExportDestructDecl := TGocciaExportDestructuringDeclaration(ANode);
    CollectVarBindingNamesFromNodeInternal(ExportDestructDecl.Declaration,
      ANames, AIncludeAnnexBBlockFunctions, AAtVarScopedLevel);
  end
  else if ANode is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(ANode);
    if VarDecl.IsVar then
      for I := 0 to Length(VarDecl.Variables) - 1 do
        AddBindingName(ANames, VarDecl.Variables[I].Name, True);
  end
  else if (ANode is TGocciaFunctionDeclaration) and
          (AAtVarScopedLevel or
          (AIncludeAnnexBBlockFunctions and
          not TGocciaFunctionDeclaration(ANode).FunctionExpression.IsAsync and
          not TGocciaFunctionDeclaration(ANode).FunctionExpression.IsGenerator)) then
    AddBindingName(ANames, TGocciaFunctionDeclaration(ANode).Name, True)
  else if (ANode is TGocciaExportFunctionDeclaration) and
          (AAtVarScopedLevel or
          (AIncludeAnnexBBlockFunctions and
          not TGocciaExportFunctionDeclaration(ANode).Declaration.FunctionExpression.IsAsync and
          not TGocciaExportFunctionDeclaration(ANode).Declaration.FunctionExpression.IsGenerator)) then
    AddBindingName(ANames,
      TGocciaExportFunctionDeclaration(ANode).Declaration.Name, True)
  else if ANode is TGocciaDestructuringDeclaration then
  begin
    DestructDecl := TGocciaDestructuringDeclaration(ANode);
    if DestructDecl.IsVar then
      CollectPatternBindingNames(DestructDecl.Pattern, ANames, True);
  end
  else if ANode is TGocciaBlockStatement then
  begin
    Block := TGocciaBlockStatement(ANode);
    for I := 0 to Block.Nodes.Count - 1 do
      CollectVarBindingNamesFromNodeInternal(Block.Nodes[I], ANames,
        AIncludeAnnexBBlockFunctions, False);
  end
  else if ANode is TGocciaIfStatement then
  begin
    IfStmt := TGocciaIfStatement(ANode);
    CollectVarBindingNamesFromNodeInternal(IfStmt.Consequent, ANames,
      AIncludeAnnexBBlockFunctions, False);
    CollectVarBindingNamesFromNodeInternal(IfStmt.Alternate, ANames,
      AIncludeAnnexBBlockFunctions, False);
  end
  else if ANode is TGocciaForOfStatement then
  begin
    ForOf := TGocciaForOfStatement(ANode);
    if ForOf.IsVar then
    begin
      if Assigned(ForOf.BindingPattern) then
        CollectPatternBindingNames(ForOf.BindingPattern, ANames, True)
      else
        AddBindingName(ANames, ForOf.BindingName, True);
    end;
    CollectVarBindingNamesFromNodeInternal(ForOf.Body, ANames,
      AIncludeAnnexBBlockFunctions, False);
  end
  else if ANode is TGocciaForInStatement then
  begin
    ForIn := TGocciaForInStatement(ANode);
    if ForIn.IsVar then
    begin
      if Assigned(ForIn.BindingPattern) then
        CollectPatternBindingNames(ForIn.BindingPattern, ANames, True)
      else
        AddBindingName(ANames, ForIn.BindingName, True);
    end;
    CollectVarBindingNamesFromNodeInternal(ForIn.Body, ANames,
      AIncludeAnnexBBlockFunctions, False);
  end
  else if ANode is TGocciaForStatement then
  begin
    ForStmt := TGocciaForStatement(ANode);
    if Assigned(ForStmt.Init) then
      CollectVarBindingNamesFromNodeInternal(ForStmt.Init, ANames,
        AIncludeAnnexBBlockFunctions, AAtVarScopedLevel);
    CollectVarBindingNamesFromNodeInternal(ForStmt.Body, ANames,
      AIncludeAnnexBBlockFunctions, False);
  end
  else if ANode is TGocciaWhileStatement then
  begin
    WhileStmt := TGocciaWhileStatement(ANode);
    CollectVarBindingNamesFromNodeInternal(WhileStmt.Body, ANames,
      AIncludeAnnexBBlockFunctions, False);
  end
  else if ANode is TGocciaDoWhileStatement then
  begin
    DoWhileStmt := TGocciaDoWhileStatement(ANode);
    CollectVarBindingNamesFromNodeInternal(DoWhileStmt.Body, ANames,
      AIncludeAnnexBBlockFunctions, False);
  end
  else if ANode is TGocciaWithStatement then
  begin
    WithStmt := TGocciaWithStatement(ANode);
    CollectVarBindingNamesFromNodeInternal(WithStmt.Body, ANames,
      AIncludeAnnexBBlockFunctions, False);
  end
  else if ANode is TGocciaTryStatement then
  begin
    TryStmt := TGocciaTryStatement(ANode);
    CollectVarBindingNamesFromNodeInternal(TryStmt.Block, ANames,
      AIncludeAnnexBBlockFunctions, False);
    CollectVarBindingNamesFromNodeInternal(TryStmt.CatchBlock, ANames,
      AIncludeAnnexBBlockFunctions, False);
    CollectVarBindingNamesFromNodeInternal(TryStmt.FinallyBlock, ANames,
      AIncludeAnnexBBlockFunctions, False);
  end
  else if ANode is TGocciaSwitchStatement then
  begin
    SwitchStmt := TGocciaSwitchStatement(ANode);
    for I := 0 to SwitchStmt.Cases.Count - 1 do
      for J := 0 to SwitchStmt.Cases[I].Consequent.Count - 1 do
        CollectVarBindingNamesFromNodeInternal(
          SwitchStmt.Cases[I].Consequent[J], ANames,
          AIncludeAnnexBBlockFunctions, False);
  end;
end;

procedure CollectVarBindingNamesFromNode(const ANode: TGocciaASTNode;
  const ANames: TStrings;
  const AIncludeAnnexBBlockFunctions: Boolean);
begin
  CollectVarBindingNamesFromNodeInternal(ANode, ANames,
    AIncludeAnnexBBlockFunctions, True);
end;

procedure CollectVarBindingNamesFromStatements(
  const AStatements: TObjectList<TGocciaStatement>; const ANames: TStrings;
  const AIncludeAnnexBBlockFunctions: Boolean);
var
  I: Integer;
begin
  if not Assigned(AStatements) then
    Exit;
  for I := 0 to AStatements.Count - 1 do
    CollectVarBindingNamesFromNodeInternal(AStatements[I], ANames,
      AIncludeAnnexBBlockFunctions, True);
end;

procedure CollectVarBindingNamesFromNodes(
  const ANodes: TObjectList<TGocciaASTNode>; const ANames: TStrings;
  const AIncludeAnnexBBlockFunctions: Boolean);
var
  I: Integer;
begin
  if not Assigned(ANodes) then
    Exit;
  for I := 0 to ANodes.Count - 1 do
    CollectVarBindingNamesFromNodeInternal(ANodes[I], ANames,
      AIncludeAnnexBBlockFunctions, True);
end;

end.
