unit Goccia.Scope.Declarations;

{ Declaration instantiation helpers that create the bindings of a scope from
  the declarations of its code, without evaluating any of it: lexical
  predeclaration (bindings left in their temporal dead zone) and var hoisting
  (bindings initialized to undefined). Module linking uses them in both
  execution modes (ES2026 §16.2.1.7.3.1 InitializeEnvironment), so they live
  outside the tree-walk evaluator. }

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Evaluator.Context,
  Goccia.Scope,
  Goccia.Scope.BindingMap;

procedure HoistVarDeclarations(const AStatements: TObjectList<TGocciaStatement>;
  const AScope: TGocciaScope;
  const AContext: TGocciaEvaluationContext); overload;
procedure HoistVarDeclarations(const ANodes: TObjectList<TGocciaASTNode>;
  const AScope: TGocciaScope;
  const AContext: TGocciaEvaluationContext); overload;

function BlockLexicalDeclarationType(const AIsConst: Boolean): TGocciaDeclarationType;
function IsNamedDefaultFunctionDeclaration(
  const ADecl: TGocciaExportDefaultDeclaration): Boolean;
function IsNamedDefaultClassDeclaration(
  const ADecl: TGocciaExportDefaultDeclaration): Boolean;

procedure PredeclareBlockLexicalName(const AScope: TGocciaScope; const AName: string;
  const ADeclarationType: TGocciaDeclarationType; const ALine, AColumn: Integer);
procedure PredeclareBlockLexicalBinding(const ANode: TGocciaASTNode;
  const AScope: TGocciaScope; const AIncludeFunctionDeclarations: Boolean = True);

procedure PredeclareModuleLexicalDeclarations(const AProgram: TGocciaProgram;
  const AScope: TGocciaScope);
procedure PredeclareScriptLexicalDeclarations(const AProgram: TGocciaProgram;
  const AScope: TGocciaScope);
procedure PredeclareFunctionBodyLexicalDeclarations(
  const ANodes: TObjectList<TGocciaASTNode>; const AScope: TGocciaScope);

implementation

uses
  OrderedStringMap,
  UnicodeStringList,

  Goccia.AST.BindingPatterns,
  Goccia.AST.Expressions,
  Goccia.Values.Primitives;

procedure HoistVarDeclarations(const AStatements: TObjectList<TGocciaStatement>;
  const AScope: TGocciaScope;
  const AContext: TGocciaEvaluationContext);
var
  Names: TUnicodeStringList;
  I: Integer;
begin
  Names := TUnicodeStringList.Create;
  try
    CollectVarBindingNamesFromStatements(AStatements, Names,
      VarBindingNameCollectionMode(AContext.NonStrictMode,
        AContext.CompatibilityNonStrictMode));
    for I := 0 to Names.Count - 1 do
      AScope.DefineVariableBinding(Names[I], TGocciaUndefinedLiteralValue.UndefinedValue, False);
  finally
    Names.Free;
  end;
end;

procedure HoistVarDeclarations(const ANodes: TObjectList<TGocciaASTNode>;
  const AScope: TGocciaScope;
  const AContext: TGocciaEvaluationContext);
var
  Names: TUnicodeStringList;
  I: Integer;
begin
  Names := TUnicodeStringList.Create;
  try
    CollectVarBindingNamesFromNodes(ANodes, Names,
      VarBindingNameCollectionMode(AContext.NonStrictMode,
        AContext.CompatibilityNonStrictMode));
    for I := 0 to Names.Count - 1 do
      AScope.DefineVariableBinding(Names[I], TGocciaUndefinedLiteralValue.UndefinedValue, False);
  finally
    Names.Free;
  end;
end;

function BlockLexicalDeclarationType(const AIsConst: Boolean): TGocciaDeclarationType;
begin
  if AIsConst then
    Result := dtConst
  else
    Result := dtLet;
end;

function IsNamedDefaultFunctionDeclaration(
  const ADecl: TGocciaExportDefaultDeclaration): Boolean;
begin
  Result := (ADecl.LocalName <> GOCCIA_DEFAULT_EXPORT_BINDING) and
    (ADecl.Expression is TGocciaFunctionExpression) and
    (TGocciaFunctionExpression(ADecl.Expression).Name = ADecl.LocalName);
end;

function IsNamedDefaultClassDeclaration(
  const ADecl: TGocciaExportDefaultDeclaration): Boolean;
begin
  Result := (ADecl.LocalName <> GOCCIA_DEFAULT_EXPORT_BINDING) and
    (ADecl.Expression is TGocciaClassExpression) and
    (TGocciaClassExpression(ADecl.Expression).ClassDefinition.Name =
    ADecl.LocalName);
end;

procedure PredeclareBlockLexicalName(const AScope: TGocciaScope; const AName: string;
  const ADeclarationType: TGocciaDeclarationType; const ALine, AColumn: Integer);
begin
  AScope.PredeclareLexicalBinding(AName, ADeclarationType, ALine, AColumn);
end;

procedure PredeclareBlockLexicalBinding(const ANode: TGocciaASTNode;
  const AScope: TGocciaScope; const AIncludeFunctionDeclarations: Boolean = True);
var
  ExportDefaultDecl: TGocciaExportDefaultDeclaration;
  VarDecl: TGocciaVariableDeclaration;
  DestructDecl: TGocciaDestructuringDeclaration;
  ImportDecl: TGocciaImportDeclaration;
  ImportPair: TStringStringMap.TKeyValuePair;
  UsingDecl: TGocciaUsingDeclaration;
  Names: TUnicodeStringList;
  I, J: Integer;
begin
  if ANode is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(ANode);
    if VarDecl.IsVar then
      Exit;
    for I := 0 to High(VarDecl.Variables) do
    begin
      Names := TUnicodeStringList.Create;
      try
        CollectVariableInfoBindingNames(VarDecl.Variables[I], Names, True);
        for J := 0 to Names.Count - 1 do
          PredeclareBlockLexicalName(AScope, Names[J],
            BlockLexicalDeclarationType(VarDecl.IsConst), VarDecl.Line,
            VarDecl.Column);
      finally
        Names.Free;
      end;
    end;
  end
  else if ANode is TGocciaExportVariableDeclaration then
  begin
    VarDecl := TGocciaExportVariableDeclaration(ANode).Declaration;
    if VarDecl.IsVar then
      Exit;
    for I := 0 to High(VarDecl.Variables) do
    begin
      Names := TUnicodeStringList.Create;
      try
        CollectVariableInfoBindingNames(VarDecl.Variables[I], Names, True);
        for J := 0 to Names.Count - 1 do
          PredeclareBlockLexicalName(AScope, Names[J],
            BlockLexicalDeclarationType(VarDecl.IsConst), VarDecl.Line,
            VarDecl.Column);
      finally
        Names.Free;
      end;
    end;
  end
  else if (ANode is TGocciaFunctionDeclaration) and
          AIncludeFunctionDeclarations then
    PredeclareBlockLexicalName(AScope,
      TGocciaFunctionDeclaration(ANode).Name, dtLet, ANode.Line,
      ANode.Column)
  else if (ANode is TGocciaExportFunctionDeclaration) and
          AIncludeFunctionDeclarations then
    PredeclareBlockLexicalName(AScope,
      TGocciaExportFunctionDeclaration(ANode).Declaration.Name, dtLet,
      ANode.Line, ANode.Column)
  else if ANode is TGocciaDestructuringDeclaration then
  begin
    DestructDecl := TGocciaDestructuringDeclaration(ANode);
    if DestructDecl.IsVar then
      Exit;
    Names := TUnicodeStringList.Create;
    try
      CollectPatternBindingNames(DestructDecl.Pattern, Names, True);
      for I := 0 to Names.Count - 1 do
        PredeclareBlockLexicalName(AScope, Names[I],
          BlockLexicalDeclarationType(DestructDecl.IsConst), DestructDecl.Line,
          DestructDecl.Column);
    finally
      Names.Free;
    end;
  end
  else if ANode is TGocciaExportDestructuringDeclaration then
  begin
    DestructDecl := TGocciaExportDestructuringDeclaration(ANode).Declaration;
    if DestructDecl.IsVar then
      Exit;
    Names := TUnicodeStringList.Create;
    try
      CollectPatternBindingNames(DestructDecl.Pattern, Names, True);
      for I := 0 to Names.Count - 1 do
        PredeclareBlockLexicalName(AScope, Names[I],
          BlockLexicalDeclarationType(DestructDecl.IsConst), DestructDecl.Line,
          DestructDecl.Column);
    finally
      Names.Free;
    end;
  end
  else if ANode is TGocciaUsingDeclaration then
  begin
    UsingDecl := TGocciaUsingDeclaration(ANode);
    for I := 0 to High(UsingDecl.Variables) do
      PredeclareBlockLexicalName(AScope, UsingDecl.Variables[I].Name, dtConst,
        UsingDecl.Line, UsingDecl.Column);
  end
  else if ANode is TGocciaImportDeclaration then
  begin
    ImportDecl := TGocciaImportDeclaration(ANode);
    if ImportDecl.NamespaceName <> '' then
      PredeclareBlockLexicalName(AScope, ImportDecl.NamespaceName, dtConst,
        ImportDecl.Line, ImportDecl.Column);
    for ImportPair in ImportDecl.Imports do
      PredeclareBlockLexicalName(AScope, ImportPair.Key, dtConst,
        ImportDecl.Line, ImportDecl.Column);
  end
  else if ANode is TGocciaClassDeclaration then
    PredeclareBlockLexicalName(AScope,
      TGocciaClassDeclaration(ANode).ClassDefinition.Name, dtLet, ANode.Line,
      ANode.Column)
  else if ANode is TGocciaExportClassDeclaration then
    PredeclareBlockLexicalName(AScope,
      TGocciaExportClassDeclaration(ANode).Declaration.ClassDefinition.Name,
      dtLet, ANode.Line, ANode.Column)
  else if ANode is TGocciaExportDefaultDeclaration then
  begin
    ExportDefaultDecl := TGocciaExportDefaultDeclaration(ANode);
    PredeclareBlockLexicalName(AScope,
      ExportDefaultDecl.LocalName,
      BlockLexicalDeclarationType(
        not (IsNamedDefaultFunctionDeclaration(ExportDefaultDecl) or
        IsNamedDefaultClassDeclaration(ExportDefaultDecl))),
      ANode.Line, ANode.Column);
  end
  else if ANode is TGocciaEnumDeclaration then
    PredeclareBlockLexicalName(AScope, TGocciaEnumDeclaration(ANode).Name,
      dtLet, ANode.Line, ANode.Column)
  else if ANode is TGocciaExportEnumDeclaration then
    PredeclareBlockLexicalName(AScope,
      TGocciaExportEnumDeclaration(ANode).Declaration.Name, dtLet,
      ANode.Line, ANode.Column);
end;

procedure PredeclareModuleLexicalDeclarations(const AProgram: TGocciaProgram;
  const AScope: TGocciaScope);
var
  I: Integer;
begin
  for I := 0 to AProgram.Body.Count - 1 do
    PredeclareBlockLexicalBinding(AProgram.Body[I], AScope, True);
end;

procedure PredeclareScriptLexicalDeclarations(const AProgram: TGocciaProgram;
  const AScope: TGocciaScope);
var
  I: Integer;
begin
  for I := 0 to AProgram.Body.Count - 1 do
    PredeclareBlockLexicalBinding(AProgram.Body[I], AScope, False);
end;

procedure PredeclareFunctionBodyLexicalDeclarations(
  const ANodes: TObjectList<TGocciaASTNode>; const AScope: TGocciaScope);
var
  I: Integer;
begin
  for I := 0 to ANodes.Count - 1 do
    PredeclareBlockLexicalBinding(ANodes[I], AScope, False);
end;

end.
