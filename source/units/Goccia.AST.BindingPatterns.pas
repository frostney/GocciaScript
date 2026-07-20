unit Goccia.AST.BindingPatterns;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  UnicodeStringList,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements;

type
  TGocciaVarBindingNameCollectionMode = (
    vbnStandard,
    vbnNonStrictScriptCompatibility
  );

procedure CollectPatternBindingNames(const APattern: TGocciaDestructuringPattern;
  const ANames: TUnicodeStringList; const AUnique: Boolean = False);
procedure CollectVariableInfoBindingNames(const AInfo: TGocciaVariableInfo;
  const ANames: TUnicodeStringList; const AUnique: Boolean = False);
procedure CollectVariableDeclarationBindingNames(
  const ADeclaration: TGocciaVariableDeclaration;
  const ANames: TUnicodeStringList;
  const AUnique: Boolean = False);
function ParameterListBindsName(const AParameters: TGocciaParameterArray;
  const AName: string): Boolean;
function ParameterListHasDuplicateBindingNames(
  const AParameters: TGocciaParameterArray): Boolean;
function ParameterListIsSimple(
  const AParameters: TGocciaParameterArray): Boolean;
function ParameterListContainsExpressionClass(
  const AParameters: TGocciaParameterArray;
  const AExpressionClass: TClass): Boolean;
procedure CollectVarBindingNamesFromNode(const ANode: TGocciaASTNode;
  const ANames: TUnicodeStringList;
  const AMode: TGocciaVarBindingNameCollectionMode = vbnStandard);
procedure CollectVarBindingNamesFromStatements(
  const AStatements: TObjectList<TGocciaStatement>;
  const ANames: TUnicodeStringList;
  const AMode: TGocciaVarBindingNameCollectionMode = vbnStandard);
procedure CollectVarBindingNamesFromNodes(
  const ANodes: TObjectList<TGocciaASTNode>;
  const ANames: TUnicodeStringList;
  const AMode: TGocciaVarBindingNameCollectionMode = vbnStandard);

implementation

procedure AddBindingName(const ANames: TUnicodeStringList;
  const AName: string;
  const AUnique: Boolean);
begin
  if AName = '' then
    Exit;
  if (not AUnique) or (ANames.IndexOf(AName) < 0) then
    ANames.Add(AName);
end;

procedure CollectPatternBindingNames(const APattern: TGocciaDestructuringPattern;
  const ANames: TUnicodeStringList; const AUnique: Boolean = False);
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

procedure CollectVariableInfoBindingNames(const AInfo: TGocciaVariableInfo;
  const ANames: TUnicodeStringList; const AUnique: Boolean = False);
begin
  if AInfo.IsPattern or Assigned(AInfo.Pattern) then
    CollectPatternBindingNames(AInfo.Pattern, ANames, AUnique)
  else
    AddBindingName(ANames, AInfo.Name, AUnique);
end;

procedure CollectVariableDeclarationBindingNames(
  const ADeclaration: TGocciaVariableDeclaration;
  const ANames: TUnicodeStringList;
  const AUnique: Boolean = False);
var
  I: Integer;
begin
  for I := 0 to High(ADeclaration.Variables) do
    CollectVariableInfoBindingNames(ADeclaration.Variables[I], ANames,
      AUnique);
end;

function ParameterListBindsName(const AParameters: TGocciaParameterArray;
  const AName: string): Boolean;
var
  I: Integer;
  Names: TUnicodeStringList;
begin
  for I := 0 to High(AParameters) do
  begin
    if AParameters[I].IsPattern then
    begin
      Names := TUnicodeStringList.Create;
      try
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

function ParameterListHasDuplicateBindingNames(
  const AParameters: TGocciaParameterArray): Boolean;
var
  I, J: Integer;
  Names, PatternNames: TUnicodeStringList;

  function AddBindingName(const AName: string): Boolean;
  begin
    if AName = '' then
      Exit(False);
    if Names.IndexOf(AName) >= 0 then
      Exit(True);
    Names.Add(AName);
    Result := False;
  end;
begin
  Names := TUnicodeStringList.Create;
  PatternNames := TUnicodeStringList.Create;
  try
    for I := 0 to High(AParameters) do
    begin
      if AParameters[I].IsPattern then
      begin
        PatternNames.Clear;
        CollectPatternBindingNames(AParameters[I].Pattern, PatternNames);
        for J := 0 to PatternNames.Count - 1 do
          if AddBindingName(PatternNames[J]) then
            Exit(True);
      end
      else if AddBindingName(AParameters[I].Name) then
        Exit(True);
    end;
  finally
    PatternNames.Free;
    Names.Free;
  end;
  Result := False;
end;

function ParameterListIsSimple(
  const AParameters: TGocciaParameterArray): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AParameters) do
    if AParameters[I].IsRest or AParameters[I].IsPattern or
       Assigned(AParameters[I].DefaultValue) or AParameters[I].IsOptional or
       (AParameters[I].TypeAnnotation <> '') then
      Exit(False);
  Result := True;
end;

function PatternContainsExpressionClass(
  const APattern: TGocciaDestructuringPattern;
  const AExpressionClass: TClass): Boolean; forward;

function ExpressionContainsClass(const AExpr: TGocciaExpression;
  const AExpressionClass: TClass): Boolean;
var
  ArrayExpr: TGocciaArrayExpression;
  ArrowExpr: TGocciaArrowFunctionExpression;
  CallExpr: TGocciaCallExpression;
  FuncExpr: TGocciaFunctionExpression;
  ImportExpr: TGocciaImportCallExpression;
  MatchClause: TGocciaMatchClause;
  MemberExpr: TGocciaMemberExpression;
  NewExpr: TGocciaNewExpression;
  ObjectExpr: TGocciaObjectExpression;
  Pair: TPair<TGocciaExpression, TGocciaExpression>;
  I: Integer;
begin
  if not Assigned(AExpr) then
    Exit(False);

  if AExpr.InheritsFrom(AExpressionClass) then
    Exit(True)
  else if AExpr is TGocciaCallExpression then
  begin
    CallExpr := TGocciaCallExpression(AExpr);
    if ExpressionContainsClass(CallExpr.Callee, AExpressionClass) then
      Exit(True);
    for I := 0 to CallExpr.Arguments.Count - 1 do
      if ExpressionContainsClass(CallExpr.Arguments[I], AExpressionClass) then
        Exit(True);
  end
  else if AExpr is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(AExpr);
    if ExpressionContainsClass(MemberExpr.ObjectExpr, AExpressionClass) then
      Exit(True);
    if MemberExpr.Computed and
       ExpressionContainsClass(MemberExpr.PropertyExpression,
         AExpressionClass) then
      Exit(True);
  end
  else if AExpr is TGocciaBinaryExpression then
    Exit(ExpressionContainsClass(TGocciaBinaryExpression(AExpr).Left,
      AExpressionClass) or ExpressionContainsClass(
      TGocciaBinaryExpression(AExpr).Right, AExpressionClass))
  else if AExpr is TGocciaSequenceExpression then
  begin
    for I := 0 to TGocciaSequenceExpression(AExpr).Expressions.Count - 1 do
      if ExpressionContainsClass(
        TGocciaSequenceExpression(AExpr).Expressions[I],
        AExpressionClass) then
        Exit(True);
  end
  else if AExpr is TGocciaUnaryExpression then
    Exit(ExpressionContainsClass(TGocciaUnaryExpression(AExpr).Operand,
      AExpressionClass))
  else if AExpr is TGocciaAssignmentExpression then
    Exit(ExpressionContainsClass(TGocciaAssignmentExpression(AExpr).Value,
      AExpressionClass))
  else if AExpr is TGocciaPropertyAssignmentExpression then
    Exit(ExpressionContainsClass(
      TGocciaPropertyAssignmentExpression(AExpr).ObjectExpr,
      AExpressionClass) or ExpressionContainsClass(
      TGocciaPropertyAssignmentExpression(AExpr).Value, AExpressionClass))
  else if AExpr is TGocciaComputedPropertyAssignmentExpression then
    Exit(ExpressionContainsClass(
      TGocciaComputedPropertyAssignmentExpression(AExpr).ObjectExpr,
      AExpressionClass) or ExpressionContainsClass(
      TGocciaComputedPropertyAssignmentExpression(AExpr).PropertyExpression,
      AExpressionClass) or ExpressionContainsClass(
      TGocciaComputedPropertyAssignmentExpression(AExpr).Value,
      AExpressionClass))
  else if AExpr is TGocciaCompoundAssignmentExpression then
    Exit(ExpressionContainsClass(TGocciaCompoundAssignmentExpression(AExpr).Value,
      AExpressionClass))
  else if AExpr is TGocciaPropertyCompoundAssignmentExpression then
    Exit(ExpressionContainsClass(
      TGocciaPropertyCompoundAssignmentExpression(AExpr).ObjectExpr,
      AExpressionClass) or ExpressionContainsClass(
      TGocciaPropertyCompoundAssignmentExpression(AExpr).Value,
      AExpressionClass))
  else if AExpr is TGocciaComputedPropertyCompoundAssignmentExpression then
    Exit(ExpressionContainsClass(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).ObjectExpr,
      AExpressionClass) or ExpressionContainsClass(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).PropertyExpression,
      AExpressionClass) or ExpressionContainsClass(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).Value,
      AExpressionClass))
  else if AExpr is TGocciaIncrementExpression then
    Exit(ExpressionContainsClass(TGocciaIncrementExpression(AExpr).Operand,
      AExpressionClass))
  else if AExpr is TGocciaArrayExpression then
  begin
    ArrayExpr := TGocciaArrayExpression(AExpr);
    for I := 0 to ArrayExpr.Elements.Count - 1 do
      if ExpressionContainsClass(ArrayExpr.Elements[I], AExpressionClass) then
        Exit(True);
  end
  else if AExpr is TGocciaObjectExpression then
  begin
    ObjectExpr := TGocciaObjectExpression(AExpr);
    for I := 0 to High(ObjectExpr.PropertySourceOrder) do
      case ObjectExpr.PropertySourceOrder[I].PropertyType of
        pstStatic:
          if ExpressionContainsClass(
            ObjectExpr.PropertySourceOrder[I].Expression,
            AExpressionClass) then
            Exit(True);
        pstComputed:
          begin
            Pair := ObjectExpr.ComputedPropertiesInOrder[
              ObjectExpr.PropertySourceOrder[I].ComputedIndex];
            if ExpressionContainsClass(Pair.Key, AExpressionClass) or
               ExpressionContainsClass(Pair.Value, AExpressionClass) then
              Exit(True);
          end;
        pstComputedGetter,
        pstComputedSetter:
          begin
            Pair := ObjectExpr.ComputedPropertiesInOrder[
              ObjectExpr.PropertySourceOrder[I].ComputedIndex];
            if ExpressionContainsClass(Pair.Key, AExpressionClass) then
              Exit(True);
          end;
      end;
  end
  else if AExpr is TGocciaYieldExpression then
    Exit(ExpressionContainsClass(TGocciaYieldExpression(AExpr).Operand,
      AExpressionClass))
  else if AExpr is TGocciaAwaitExpression then
    Exit(ExpressionContainsClass(TGocciaAwaitExpression(AExpr).Operand,
      AExpressionClass))
  else if AExpr is TGocciaConditionalExpression then
    Exit(ExpressionContainsClass(
      TGocciaConditionalExpression(AExpr).Condition, AExpressionClass) or
      ExpressionContainsClass(
        TGocciaConditionalExpression(AExpr).Consequent, AExpressionClass) or
      ExpressionContainsClass(
        TGocciaConditionalExpression(AExpr).Alternate, AExpressionClass))
  else if AExpr is TGocciaNewExpression then
  begin
    NewExpr := TGocciaNewExpression(AExpr);
    if ExpressionContainsClass(NewExpr.Callee, AExpressionClass) then
      Exit(True);
    for I := 0 to NewExpr.Arguments.Count - 1 do
      if ExpressionContainsClass(NewExpr.Arguments[I], AExpressionClass) then
        Exit(True);
  end
  else if AExpr is TGocciaImportCallExpression then
  begin
    ImportExpr := TGocciaImportCallExpression(AExpr);
    Exit(ExpressionContainsClass(ImportExpr.Specifier, AExpressionClass) or
      ExpressionContainsClass(ImportExpr.Options, AExpressionClass));
  end
  else if AExpr is TGocciaSpreadExpression then
    Exit(ExpressionContainsClass(TGocciaSpreadExpression(AExpr).Argument,
      AExpressionClass))
  else if AExpr is TGocciaTemplateWithInterpolationExpression then
  begin
    for I := 0 to TGocciaTemplateWithInterpolationExpression(AExpr).Parts.Count - 1 do
      if ExpressionContainsClass(
        TGocciaTemplateWithInterpolationExpression(AExpr).Parts[I],
        AExpressionClass) then
        Exit(True);
  end
  else if AExpr is TGocciaTaggedTemplateExpression then
  begin
    if ExpressionContainsClass(TGocciaTaggedTemplateExpression(AExpr).Tag,
       AExpressionClass) then
      Exit(True);
    for I := 0 to TGocciaTaggedTemplateExpression(AExpr).Expressions.Count - 1 do
      if ExpressionContainsClass(
        TGocciaTaggedTemplateExpression(AExpr).Expressions[I],
        AExpressionClass) then
        Exit(True);
  end
  else if AExpr is TGocciaDestructuringAssignmentExpression then
    Exit(PatternContainsExpressionClass(
      TGocciaDestructuringAssignmentExpression(AExpr).Left,
      AExpressionClass) or ExpressionContainsClass(
      TGocciaDestructuringAssignmentExpression(AExpr).Right,
      AExpressionClass))
  else if AExpr is TGocciaPrivateMemberExpression then
    Exit(ExpressionContainsClass(
      TGocciaPrivateMemberExpression(AExpr).ObjectExpr, AExpressionClass))
  else if AExpr is TGocciaPrivatePropertyAssignmentExpression then
    Exit(ExpressionContainsClass(
      TGocciaPrivatePropertyAssignmentExpression(AExpr).ObjectExpr,
      AExpressionClass) or ExpressionContainsClass(
      TGocciaPrivatePropertyAssignmentExpression(AExpr).Value,
      AExpressionClass))
  else if AExpr is TGocciaPrivatePropertyCompoundAssignmentExpression then
    Exit(ExpressionContainsClass(
      TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).ObjectExpr,
      AExpressionClass) or ExpressionContainsClass(
      TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).Value,
      AExpressionClass))
  else if AExpr is TGocciaArrowFunctionExpression then
  begin
    ArrowExpr := TGocciaArrowFunctionExpression(AExpr);
    Exit(ParameterListContainsExpressionClass(ArrowExpr.Parameters,
      AExpressionClass));
  end
  else if AExpr is TGocciaFunctionExpression then
  begin
    FuncExpr := TGocciaFunctionExpression(AExpr);
    Exit(ParameterListContainsExpressionClass(FuncExpr.Parameters,
      AExpressionClass));
  end
  else if AExpr is TGocciaObjectMethodDefinition then
    Exit(ParameterListContainsExpressionClass(
      TGocciaObjectMethodDefinition(AExpr).FunctionExpression.Parameters,
      AExpressionClass))
  else if AExpr is TGocciaIsExpression then
    Exit(ExpressionContainsClass(TGocciaIsExpression(AExpr).Subject,
      AExpressionClass))
  else if AExpr is TGocciaMatchExpression then
  begin
    if ExpressionContainsClass(TGocciaMatchExpression(AExpr).Subject,
       AExpressionClass) then
      Exit(True);
    for I := 0 to TGocciaMatchExpression(AExpr).Clauses.Count - 1 do
    begin
      MatchClause := TGocciaMatchExpression(AExpr).Clauses[I];
      if ExpressionContainsClass(MatchClause.Expression, AExpressionClass) then
        Exit(True);
    end;
    Exit(ExpressionContainsClass(
      TGocciaMatchExpression(AExpr).DefaultExpression, AExpressionClass));
  end;

  Result := False;
end;

function PatternContainsExpressionClass(
  const APattern: TGocciaDestructuringPattern;
  const AExpressionClass: TClass): Boolean;
var
  ArrPat: TGocciaArrayDestructuringPattern;
  AssignmentPattern: TGocciaAssignmentDestructuringPattern;
  ObjPat: TGocciaObjectDestructuringPattern;
  Prop: TGocciaDestructuringProperty;
  RestPattern: TGocciaRestDestructuringPattern;
  I: Integer;
begin
  if not Assigned(APattern) then
    Exit(False);

  if APattern is TGocciaArrayDestructuringPattern then
  begin
    ArrPat := TGocciaArrayDestructuringPattern(APattern);
    for I := 0 to ArrPat.Elements.Count - 1 do
      if PatternContainsExpressionClass(ArrPat.Elements[I],
        AExpressionClass) then
        Exit(True);
  end
  else if APattern is TGocciaObjectDestructuringPattern then
  begin
    ObjPat := TGocciaObjectDestructuringPattern(APattern);
    for I := 0 to ObjPat.Properties.Count - 1 do
    begin
      Prop := ObjPat.Properties[I];
      if (Prop.Computed and ExpressionContainsClass(
          Prop.KeyExpression, AExpressionClass)) or
         PatternContainsExpressionClass(Prop.Pattern, AExpressionClass) then
        Exit(True);
    end;
  end
  else if APattern is TGocciaAssignmentDestructuringPattern then
  begin
    AssignmentPattern := TGocciaAssignmentDestructuringPattern(APattern);
    Exit(PatternContainsExpressionClass(AssignmentPattern.Left,
      AExpressionClass) or ExpressionContainsClass(AssignmentPattern.Right,
      AExpressionClass));
  end
  else if APattern is TGocciaRestDestructuringPattern then
  begin
    RestPattern := TGocciaRestDestructuringPattern(APattern);
    Exit(PatternContainsExpressionClass(RestPattern.Argument,
      AExpressionClass));
  end
  else if APattern is TGocciaMemberExpressionDestructuringPattern then
    Exit(ExpressionContainsClass(
      TGocciaMemberExpressionDestructuringPattern(APattern).Expression,
      AExpressionClass))
  else if APattern is TGocciaPrivateMemberExpressionDestructuringPattern then
    Exit(ExpressionContainsClass(
      TGocciaPrivateMemberExpressionDestructuringPattern(APattern).Expression,
      AExpressionClass));

  Result := False;
end;

function ParameterListContainsExpressionClass(
  const AParameters: TGocciaParameterArray;
  const AExpressionClass: TClass): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AParameters) do
  begin
    if AParameters[I].IsPattern and
       PatternContainsExpressionClass(AParameters[I].Pattern,
         AExpressionClass) then
      Exit(True);
    if ExpressionContainsClass(AParameters[I].DefaultValue,
       AExpressionClass) then
      Exit(True);
  end;
  Result := False;
end;

procedure CollectVarBindingNamesFromNodeInternal(const ANode: TGocciaASTNode;
  const ANames: TUnicodeStringList;
  const AMode: TGocciaVarBindingNameCollectionMode;
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
      AMode, AAtVarScopedLevel);
  end
  else if ANode is TGocciaExportDestructuringDeclaration then
  begin
    ExportDestructDecl := TGocciaExportDestructuringDeclaration(ANode);
    CollectVarBindingNamesFromNodeInternal(ExportDestructDecl.Declaration,
      ANames, AMode, AAtVarScopedLevel);
  end
  else if ANode is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(ANode);
    if VarDecl.IsVar then
      CollectVariableDeclarationBindingNames(VarDecl, ANames, True);
  end
  else if (ANode is TGocciaFunctionDeclaration) and
          (AAtVarScopedLevel or
          ((AMode = vbnNonStrictScriptCompatibility) and
          not TGocciaFunctionDeclaration(ANode).FunctionExpression.IsAsync and
          not TGocciaFunctionDeclaration(ANode).FunctionExpression.IsGenerator)) then
    AddBindingName(ANames, TGocciaFunctionDeclaration(ANode).Name, True)
  else if (ANode is TGocciaExportFunctionDeclaration) and
          (AAtVarScopedLevel or
          ((AMode = vbnNonStrictScriptCompatibility) and
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
        AMode, False);
  end
  else if ANode is TGocciaIfStatement then
  begin
    IfStmt := TGocciaIfStatement(ANode);
    CollectVarBindingNamesFromNodeInternal(IfStmt.Consequent, ANames,
      AMode, False);
    CollectVarBindingNamesFromNodeInternal(IfStmt.Alternate, ANames,
      AMode, False);
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
      AMode, False);
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
      AMode, False);
  end
  else if ANode is TGocciaForStatement then
  begin
    ForStmt := TGocciaForStatement(ANode);
    if Assigned(ForStmt.Init) then
      CollectVarBindingNamesFromNodeInternal(ForStmt.Init, ANames,
        AMode, AAtVarScopedLevel);
    CollectVarBindingNamesFromNodeInternal(ForStmt.Body, ANames,
      AMode, False);
  end
  else if ANode is TGocciaWhileStatement then
  begin
    WhileStmt := TGocciaWhileStatement(ANode);
    CollectVarBindingNamesFromNodeInternal(WhileStmt.Body, ANames,
      AMode, False);
  end
  else if ANode is TGocciaDoWhileStatement then
  begin
    DoWhileStmt := TGocciaDoWhileStatement(ANode);
    CollectVarBindingNamesFromNodeInternal(DoWhileStmt.Body, ANames,
      AMode, False);
  end
  else if ANode is TGocciaWithStatement then
  begin
    WithStmt := TGocciaWithStatement(ANode);
    CollectVarBindingNamesFromNodeInternal(WithStmt.Body, ANames,
      AMode, False);
  end
  else if ANode is TGocciaTryStatement then
  begin
    TryStmt := TGocciaTryStatement(ANode);
    CollectVarBindingNamesFromNodeInternal(TryStmt.Block, ANames,
      AMode, False);
    CollectVarBindingNamesFromNodeInternal(TryStmt.CatchBlock, ANames,
      AMode, False);
    CollectVarBindingNamesFromNodeInternal(TryStmt.FinallyBlock, ANames,
      AMode, False);
  end
  else if ANode is TGocciaSwitchStatement then
  begin
    SwitchStmt := TGocciaSwitchStatement(ANode);
    for I := 0 to SwitchStmt.Cases.Count - 1 do
      for J := 0 to SwitchStmt.Cases[I].Consequent.Count - 1 do
        CollectVarBindingNamesFromNodeInternal(
          SwitchStmt.Cases[I].Consequent[J], ANames,
          AMode, False);
  end;
end;

procedure CollectVarBindingNamesFromNode(const ANode: TGocciaASTNode;
  const ANames: TUnicodeStringList;
  const AMode: TGocciaVarBindingNameCollectionMode = vbnStandard);
begin
  CollectVarBindingNamesFromNodeInternal(ANode, ANames, AMode, True);
end;

procedure CollectVarBindingNamesFromStatements(
  const AStatements: TObjectList<TGocciaStatement>;
  const ANames: TUnicodeStringList;
  const AMode: TGocciaVarBindingNameCollectionMode = vbnStandard);
var
  I: Integer;
begin
  if not Assigned(AStatements) then
    Exit;
  for I := 0 to AStatements.Count - 1 do
    CollectVarBindingNamesFromNodeInternal(AStatements[I], ANames, AMode,
      True);
end;

procedure CollectVarBindingNamesFromNodes(
  const ANodes: TObjectList<TGocciaASTNode>;
  const ANames: TUnicodeStringList;
  const AMode: TGocciaVarBindingNameCollectionMode = vbnStandard);
var
  I: Integer;
begin
  if not Assigned(ANodes) then
    Exit;
  for I := 0 to ANodes.Count - 1 do
    CollectVarBindingNamesFromNodeInternal(ANodes[I], ANames, AMode, True);
end;

end.
