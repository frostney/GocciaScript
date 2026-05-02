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
procedure CollectVarBindingNamesFromNode(const ANode: TGocciaASTNode;
  const ANames: TStrings);
procedure CollectVarBindingNamesFromStatements(
  const AStatements: TObjectList<TGocciaStatement>; const ANames: TStrings);
procedure CollectVarBindingNamesFromNodes(
  const ANodes: TObjectList<TGocciaASTNode>; const ANames: TStrings);

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

procedure CollectVarBindingNamesFromNode(const ANode: TGocciaASTNode;
  const ANames: TStrings);
var
  Block: TGocciaBlockStatement;
  IfStmt: TGocciaIfStatement;
  ForOf: TGocciaForOfStatement;
  TryStmt: TGocciaTryStatement;
  SwitchStmt: TGocciaSwitchStatement;
  VarDecl: TGocciaVariableDeclaration;
  DestructDecl: TGocciaDestructuringDeclaration;
  ExportVarDecl: TGocciaExportVariableDeclaration;
  I, J: Integer;
begin
  if not Assigned(ANode) then
    Exit;

  if ANode is TGocciaExportVariableDeclaration then
  begin
    ExportVarDecl := TGocciaExportVariableDeclaration(ANode);
    CollectVarBindingNamesFromNode(ExportVarDecl.Declaration, ANames);
  end
  else if ANode is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(ANode);
    if VarDecl.IsVar then
      for I := 0 to Length(VarDecl.Variables) - 1 do
        AddBindingName(ANames, VarDecl.Variables[I].Name, True);
  end
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
      CollectVarBindingNamesFromNode(Block.Nodes[I], ANames);
  end
  else if ANode is TGocciaIfStatement then
  begin
    IfStmt := TGocciaIfStatement(ANode);
    CollectVarBindingNamesFromNode(IfStmt.Consequent, ANames);
    CollectVarBindingNamesFromNode(IfStmt.Alternate, ANames);
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
    CollectVarBindingNamesFromNode(ForOf.Body, ANames);
  end
  else if ANode is TGocciaTryStatement then
  begin
    TryStmt := TGocciaTryStatement(ANode);
    CollectVarBindingNamesFromNode(TryStmt.Block, ANames);
    CollectVarBindingNamesFromNode(TryStmt.CatchBlock, ANames);
    CollectVarBindingNamesFromNode(TryStmt.FinallyBlock, ANames);
  end
  else if ANode is TGocciaSwitchStatement then
  begin
    SwitchStmt := TGocciaSwitchStatement(ANode);
    for I := 0 to SwitchStmt.Cases.Count - 1 do
      for J := 0 to SwitchStmt.Cases[I].Consequent.Count - 1 do
        CollectVarBindingNamesFromNode(
          SwitchStmt.Cases[I].Consequent[J], ANames);
  end;
end;

procedure CollectVarBindingNamesFromStatements(
  const AStatements: TObjectList<TGocciaStatement>; const ANames: TStrings);
var
  I: Integer;
begin
  if not Assigned(AStatements) then
    Exit;
  for I := 0 to AStatements.Count - 1 do
    CollectVarBindingNamesFromNode(AStatements[I], ANames);
end;

procedure CollectVarBindingNamesFromNodes(
  const ANodes: TObjectList<TGocciaASTNode>; const ANames: TStrings);
var
  I: Integer;
begin
  if not Assigned(ANodes) then
    Exit;
  for I := 0 to ANodes.Count - 1 do
    CollectVarBindingNamesFromNode(ANodes[I], ANames);
end;

end.
