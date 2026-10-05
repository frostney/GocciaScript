unit Goccia.Compiler.BlockFunctions;

{$I Goccia.inc}

// Finds the block-level function declarations of one function body or script
// that also get a var binding in non-strict code.
//
// ES2026 §10.2.11 FunctionDeclarationInstantiation(func, argumentsList) and
// §16.1.7 GlobalDeclarationInstantiation(script, env), at their web-compat
// insertion points (Annex B.3.2.1 and B.3.2.2 up to ES2025): a
// FunctionDeclaration f directly in the StatementList of a Block, CaseClause or
// DefaultClause gets a var binding F, which f's evaluation sets to the function
// object, only if "replacing the FunctionDeclaration f with a VariableStatement
// that has F as a BindingIdentifier would not produce any Early Errors" and, in
// a function, "parameterNames does not contain F". Otherwise f is only a
// lexical binding of its block, and a parameter, `let`, `const` or `class`
// named F keeps its value.
//
// `var F` in f's place is an early error when F is lexically declared by a
// statement list f sits in, other than by f itself: a `let`, `const`, `class`,
// `using` or enum declaration in the function body or the script, in f's block
// or an enclosing block or case block, or another function declaration in f's
// block or an enclosing one. A `let` or `const` loop head that encloses f and
// a destructured catch parameter that binds F are early errors too (§14.7.4.1,
// §14.7.5.1, and Annex B.3.4 VariableStatements in Catch Blocks, which allows
// `var F` only under a catch parameter that is a plain identifier).
//
// Annex B.3.3 FunctionDeclarations in IfStatement Statement Clauses: a function
// declaration that is the whole body of an if or else clause behaves as if it
// were the only statement of a block. Async functions and generators never get
// a var binding. The scan does not enter nested functions or classes: each
// function body is scanned when it is compiled.

interface

uses
  Generics.Collections,

  Goccia.AST.Node,
  Goccia.Compiler.Context,
  Goccia.Compiler.Scope;

// Adds to AResult each function declaration in ABody, a function body, that
// gets a var binding. AScope is the scope the body is compiled in; its
// parameter names are parameterNames.
procedure DiscoverBlockFunctionVarBindings(const ABody: TGocciaASTNode;
  const AScope: TGocciaCompilerScope;
  const AResult: TBlockFunctionVarBindingSet);

// Adds to AResult each function declaration in AStatements, the statements of
// a script, that gets a var binding.
procedure DiscoverProgramBlockFunctionVarBindings(
  const AStatements: TObjectList<TGocciaStatement>;
  const AResult: TBlockFunctionVarBindingSet);

// True when AResult has ADeclaration, a function declaration in a block.
function BlockFunctionHasVarBinding(
  const AResult: TBlockFunctionVarBindingSet;
  const ADeclaration: TGocciaStatement): Boolean; {$IFDEF FPC}inline;{$ENDIF}

implementation

uses
  UnicodeStringList,

  Goccia.AST.BindingPatterns,
  Goccia.AST.Statements;

type
  TBlockFunctionScan = class
  private
    FScope: TGocciaCompilerScope;
    FResult: TBlockFunctionVarBindingSet;
    // One list of lexically declared names per statement list or binding
    // construct that encloses the statement being scanned, innermost last.
    FFrames: TObjectList<TUnicodeStringList>;

    function PushFrame: TUnicodeStringList;
    procedure PopFrame;
    procedure AddLexicalNames(const ANode: TGocciaASTNode;
      const AIncludeFunctions: Boolean; const ANames: TUnicodeStringList);
    procedure ConsiderDeclaration(const ADeclaration: TGocciaFunctionDeclaration);
    procedure ScanNestedList(const ANodes: TObjectList<TGocciaASTNode>);
    procedure ScanClause(const AStatement: TGocciaStatement);
    procedure ScanStatement(const ANode: TGocciaASTNode);
  public
    constructor Create(const AScope: TGocciaCompilerScope;
      const AResult: TBlockFunctionVarBindingSet);
    destructor Destroy; override;
    procedure ScanTopLevelNode(const ANode: TGocciaASTNode);
    procedure AddTopLevelLexicalNames(const ANode: TGocciaASTNode);
  end;

constructor TBlockFunctionScan.Create(const AScope: TGocciaCompilerScope;
  const AResult: TBlockFunctionVarBindingSet);
begin
  inherited Create;
  FScope := AScope;
  FResult := AResult;
  FFrames := TObjectList<TUnicodeStringList>.Create(True);
  // The frame of the function body's or the script's top-level declarations.
  PushFrame;
end;

destructor TBlockFunctionScan.Destroy;
begin
  FFrames.Free;
  inherited;
end;

function TBlockFunctionScan.PushFrame: TUnicodeStringList;
begin
  Result := TUnicodeStringList.Create;
  FFrames.Add(Result);
end;

procedure TBlockFunctionScan.PopFrame;
begin
  FFrames.Delete(FFrames.Count - 1);
end;

// ES2026 §8.2.6 Static Semantics: LexicallyDeclaredNames of one statement list
// item. At the top level of a function body or a script, function declarations
// are var scoped (§8.2.10 TopLevelLexicallyDeclaredNames), so AIncludeFunctions
// is False there.
procedure TBlockFunctionScan.AddLexicalNames(const ANode: TGocciaASTNode;
  const AIncludeFunctions: Boolean; const ANames: TUnicodeStringList);
var
  I: Integer;
begin
  if ANode is TGocciaVariableDeclaration then
  begin
    if not TGocciaVariableDeclaration(ANode).IsVar then
      CollectVariableDeclarationBindingNames(
        TGocciaVariableDeclaration(ANode), ANames);
  end
  else if ANode is TGocciaDestructuringDeclaration then
  begin
    if not TGocciaDestructuringDeclaration(ANode).IsVar then
      CollectPatternBindingNames(TGocciaDestructuringDeclaration(ANode).Pattern,
        ANames);
  end
  else if ANode is TGocciaClassDeclaration then
    ANames.Add(TGocciaClassDeclaration(ANode).ClassDefinition.Name)
  else if ANode is TGocciaEnumDeclaration then
    ANames.Add(TGocciaEnumDeclaration(ANode).Name)
  else if ANode is TGocciaUsingDeclaration then
  begin
    for I := 0 to High(TGocciaUsingDeclaration(ANode).Variables) do
      CollectVariableInfoBindingNames(
        TGocciaUsingDeclaration(ANode).Variables[I], ANames);
  end
  else if AIncludeFunctions and (ANode is TGocciaFunctionDeclaration) then
    ANames.Add(TGocciaFunctionDeclaration(ANode).Name);
end;

procedure TBlockFunctionScan.AddTopLevelLexicalNames(
  const ANode: TGocciaASTNode);
begin
  AddLexicalNames(ANode, False, FFrames[0]);
end;

// ADeclaration sits directly in the statement list of the innermost frame,
// which lists ADeclaration's own name once.
procedure TBlockFunctionScan.ConsiderDeclaration(
  const ADeclaration: TGocciaFunctionDeclaration);
var
  Name: string;
  Own: TUnicodeStringList;
  I, Count: Integer;
begin
  if ADeclaration.FunctionExpression.IsAsync or
     ADeclaration.FunctionExpression.IsGenerator then
    Exit;
  Name := ADeclaration.Name;
  if Assigned(FScope) and FScope.HasParameterName(Name) then
    Exit;

  // Another declaration of the name in the same statement list, a duplicate
  // function declaration that the §14.2.1 Block early errors allow in
  // non-strict code, would conflict with the var as well.
  Own := FFrames[FFrames.Count - 1];
  Count := 0;
  for I := 0 to Own.Count - 1 do
    if Own[I] = Name then
      Inc(Count);
  if Count > 1 then
    Exit;

  for I := 0 to FFrames.Count - 2 do
    if FFrames[I].IndexOf(Name) >= 0 then
      Exit;

  FResult.AddOrSetValue(ADeclaration, True);
end;

// A Block's StatementList.
procedure TBlockFunctionScan.ScanNestedList(
  const ANodes: TObjectList<TGocciaASTNode>);
var
  Names: TUnicodeStringList;
  I: Integer;
begin
  Names := PushFrame;
  try
    for I := 0 to ANodes.Count - 1 do
      AddLexicalNames(ANodes[I], True, Names);
    for I := 0 to ANodes.Count - 1 do
      if ANodes[I] is TGocciaFunctionDeclaration then
        ConsiderDeclaration(TGocciaFunctionDeclaration(ANodes[I]))
      else
        ScanStatement(ANodes[I]);
  finally
    PopFrame;
  end;
end;

// The body of an if or else clause.
procedure TBlockFunctionScan.ScanClause(const AStatement: TGocciaStatement);
begin
  if AStatement is TGocciaFunctionDeclaration then
  begin
    PushFrame.Add(TGocciaFunctionDeclaration(AStatement).Name);
    try
      ConsiderDeclaration(TGocciaFunctionDeclaration(AStatement));
    finally
      PopFrame;
    end;
  end
  else
    ScanStatement(AStatement);
end;

procedure TBlockFunctionScan.ScanStatement(const ANode: TGocciaASTNode);
var
  ForStmt: TGocciaForStatement;
  ForOf: TGocciaForOfStatement;
  ForIn: TGocciaForInStatement;
  TryStmt: TGocciaTryStatement;
  SwitchStmt: TGocciaSwitchStatement;
  Names: TUnicodeStringList;
  I, J: Integer;
begin
  if not Assigned(ANode) then
    Exit;

  if ANode is TGocciaBlockStatement then
    ScanNestedList(TGocciaBlockStatement(ANode).Nodes)
  else if ANode is TGocciaIfStatement then
  begin
    ScanClause(TGocciaIfStatement(ANode).Consequent);
    ScanClause(TGocciaIfStatement(ANode).Alternate);
  end
  else if ANode is TGocciaForStatement then
  begin
    ForStmt := TGocciaForStatement(ANode);
    Names := PushFrame;
    try
      AddLexicalNames(ForStmt.Init, False, Names);
      ScanStatement(ForStmt.Body);
    finally
      PopFrame;
    end;
  end
  else if ANode is TGocciaForOfStatement then
  begin
    ForOf := TGocciaForOfStatement(ANode);
    Names := PushFrame;
    try
      if not ForOf.IsVar and not Assigned(ForOf.AssignmentTarget) then
      begin
        if ForOf.BindingName <> '' then
          Names.Add(ForOf.BindingName);
        CollectPatternBindingNames(ForOf.BindingPattern, Names);
      end;
      ScanStatement(ForOf.Body);
    finally
      PopFrame;
    end;
  end
  else if ANode is TGocciaForInStatement then
  begin
    ForIn := TGocciaForInStatement(ANode);
    Names := PushFrame;
    try
      if not ForIn.IsVar and not Assigned(ForIn.AssignmentTarget) then
      begin
        if ForIn.BindingName <> '' then
          Names.Add(ForIn.BindingName);
        CollectPatternBindingNames(ForIn.BindingPattern, Names);
      end;
      ScanStatement(ForIn.Body);
    finally
      PopFrame;
    end;
  end
  else if ANode is TGocciaWhileStatement then
    ScanStatement(TGocciaWhileStatement(ANode).Body)
  else if ANode is TGocciaDoWhileStatement then
    ScanStatement(TGocciaDoWhileStatement(ANode).Body)
  else if ANode is TGocciaWithStatement then
    ScanStatement(TGocciaWithStatement(ANode).Body)
  else if ANode is TGocciaTryStatement then
  begin
    TryStmt := TGocciaTryStatement(ANode);
    ScanStatement(TryStmt.Block);
    Names := PushFrame;
    try
      CollectPatternBindingNames(TryStmt.CatchBindingPattern, Names);
      ScanStatement(TryStmt.CatchBlock);
    finally
      PopFrame;
    end;
    ScanStatement(TryStmt.FinallyBlock);
  end
  else if ANode is TGocciaSwitchStatement then
  begin
    // The case clauses share one CaseBlock, and so one statement list scope.
    SwitchStmt := TGocciaSwitchStatement(ANode);
    Names := PushFrame;
    try
      for I := 0 to SwitchStmt.Cases.Count - 1 do
        for J := 0 to SwitchStmt.Cases[I].Consequent.Count - 1 do
          AddLexicalNames(SwitchStmt.Cases[I].Consequent[J], True, Names);
      for I := 0 to SwitchStmt.Cases.Count - 1 do
        for J := 0 to SwitchStmt.Cases[I].Consequent.Count - 1 do
          if SwitchStmt.Cases[I].Consequent[J] is TGocciaFunctionDeclaration then
            ConsiderDeclaration(TGocciaFunctionDeclaration(
              SwitchStmt.Cases[I].Consequent[J]))
          else
            ScanStatement(SwitchStmt.Cases[I].Consequent[J]);
    finally
      PopFrame;
    end;
  end;
end;

procedure TBlockFunctionScan.ScanTopLevelNode(const ANode: TGocciaASTNode);
begin
  // A top-level function declaration is var scoped already.
  if not (ANode is TGocciaFunctionDeclaration) then
    ScanStatement(ANode);
end;

procedure DiscoverBlockFunctionVarBindings(const ABody: TGocciaASTNode;
  const AScope: TGocciaCompilerScope;
  const AResult: TBlockFunctionVarBindingSet);
var
  Scan: TBlockFunctionScan;
  Nodes: TObjectList<TGocciaASTNode>;
  I: Integer;
begin
  if not (ABody is TGocciaBlockStatement) then
    Exit;
  Nodes := TGocciaBlockStatement(ABody).Nodes;
  Scan := TBlockFunctionScan.Create(AScope, AResult);
  try
    for I := 0 to Nodes.Count - 1 do
      Scan.AddTopLevelLexicalNames(Nodes[I]);
    for I := 0 to Nodes.Count - 1 do
      Scan.ScanTopLevelNode(Nodes[I]);
  finally
    Scan.Free;
  end;
end;

procedure DiscoverProgramBlockFunctionVarBindings(
  const AStatements: TObjectList<TGocciaStatement>;
  const AResult: TBlockFunctionVarBindingSet);
var
  Scan: TBlockFunctionScan;
  I: Integer;
begin
  Scan := TBlockFunctionScan.Create(nil, AResult);
  try
    for I := 0 to AStatements.Count - 1 do
      Scan.AddTopLevelLexicalNames(AStatements[I]);
    for I := 0 to AStatements.Count - 1 do
      Scan.ScanTopLevelNode(AStatements[I]);
  finally
    Scan.Free;
  end;
end;

function BlockFunctionHasVarBinding(
  const AResult: TBlockFunctionVarBindingSet;
  const ADeclaration: TGocciaStatement): Boolean;
begin
  Result := Assigned(AResult) and AResult.ContainsKey(ADeclaration);
end;

end.
