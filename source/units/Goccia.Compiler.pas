unit Goccia.Compiler;

{$I Goccia.inc}

interface

uses
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Bytecode.Chunk,
  Goccia.Bytecode.Module,
  Goccia.Compiler.ConstantValue,
  Goccia.Compiler.Context,
  Goccia.Compiler.Scope;

type
  TGocciaCompiler = class
  private
    FModule: TGocciaBytecodeModule;
    FCurrentTemplate: TGocciaFunctionTemplate;
    FTopLevelTemplate: TGocciaFunctionTemplate;
    FCurrentScope: TGocciaCompilerScope;
    FSourcePath: string;
    FFormalParameterCounts: TFormalParameterCountMap;
    FGlobalBackedTopLevel: Boolean;
    FStrictTypes: Boolean;
    FOptimizationOptions: TGocciaCompilerOptimizationOptions;
    procedure DoCompileExpression(const AExpr: TGocciaExpression;
      const ADest: UInt8);
    function DoCompileStatement(const AStmt: TGocciaStatement): Boolean;
    procedure DoCompileFunctionBody(const ABody: TGocciaASTNode);
    procedure DoSwapState(const ATemplate: TGocciaFunctionTemplate;
      const AScope: TGocciaCompilerScope);
    function BuildContext: TGocciaCompilationContext;
  public
    constructor Create(const ASourcePath: string);
    destructor Destroy; override;

    function Compile(const AProgram: TGocciaProgram): TGocciaBytecodeModule;
    property FormalParameterCounts: TFormalParameterCountMap
      read FFormalParameterCounts;
    property GlobalBackedTopLevel: Boolean read FGlobalBackedTopLevel
      write FGlobalBackedTopLevel;
    property StrictTypes: Boolean read FStrictTypes write FStrictTypes;
    property OptimizationOptions: TGocciaCompilerOptimizationOptions
      read FOptimizationOptions write FOptimizationOptions;
  end;

const
  GOCCIA_RUNTIME_TAG = 'goccia-js';

implementation

uses
  Generics.Collections,
  SysUtils,

  Goccia.Bytecode,
  Goccia.Bytecode.Debug,
  Goccia.Compiler.ConstantFolding,
  Goccia.Compiler.Expressions,
  Goccia.Compiler.PatternMatching,
  Goccia.Compiler.Statements;

{ TGocciaCompiler }

constructor TGocciaCompiler.Create(const ASourcePath: string);
begin
  inherited Create;
  FSourcePath := ASourcePath;
  FFormalParameterCounts := TFormalParameterCountMap.Create;
  FModule := nil;
  FCurrentTemplate := nil;
  FTopLevelTemplate := nil;
  FCurrentScope := nil;
  FOptimizationOptions := DefaultCompilerOptimizationOptions;
end;

destructor TGocciaCompiler.Destroy;
begin
  FFormalParameterCounts.Free;
  inherited;
end;

function TGocciaCompiler.BuildContext: TGocciaCompilationContext;
begin
  Result.Template := FCurrentTemplate;
  Result.Scope := FCurrentScope;
  Result.SourcePath := FSourcePath;
  Result.FormalParameterCounts := FFormalParameterCounts;
  Result.GlobalBackedTopLevel := FGlobalBackedTopLevel and
    (FCurrentTemplate = FTopLevelTemplate);
  Result.StrictTypes := FStrictTypes;
  Result.OptimizationOptions := FOptimizationOptions;
  Result.CompileExpression := DoCompileExpression;
  Result.CompileStatement := DoCompileStatement;
  Result.CompileFunctionBody := DoCompileFunctionBody;
  Result.SwapState := DoSwapState;
end;

procedure TGocciaCompiler.DoSwapState(
  const ATemplate: TGocciaFunctionTemplate;
  const AScope: TGocciaCompilerScope);
begin
  FCurrentTemplate := ATemplate;
  FCurrentScope := AScope;
end;

procedure TGocciaCompiler.DoCompileExpression(
  const AExpr: TGocciaExpression; const ADest: UInt8);
var
  Ctx: TGocciaCompilationContext;
begin
  Ctx := BuildContext;
  EmitLineMapping(Ctx, AExpr.Line, AExpr.Column);

  if TryEmitConstantExpression(Ctx, AExpr, ADest) then
    Exit;

  if AExpr is TGocciaLiteralExpression then
    Goccia.Compiler.Expressions.CompileLiteral(Ctx, TGocciaLiteralExpression(AExpr), ADest)
  else if AExpr is TGocciaRegexLiteralExpression then
    Goccia.Compiler.Expressions.CompileRegexLiteral(Ctx, TGocciaRegexLiteralExpression(AExpr), ADest)
  else if AExpr is TGocciaIdentifierExpression then
    Goccia.Compiler.Expressions.CompileIdentifier(Ctx, TGocciaIdentifierExpression(AExpr), ADest)
  else if AExpr is TGocciaBinaryExpression then
    Goccia.Compiler.Expressions.CompileBinary(Ctx, TGocciaBinaryExpression(AExpr), ADest)
  else if AExpr is TGocciaSequenceExpression then
    Goccia.Compiler.Expressions.CompileSequence(Ctx, TGocciaSequenceExpression(AExpr), ADest)
  else if AExpr is TGocciaUnaryExpression then
    Goccia.Compiler.Expressions.CompileUnary(Ctx, TGocciaUnaryExpression(AExpr), ADest)
  else if AExpr is TGocciaPropertyCompoundAssignmentExpression then
    Goccia.Compiler.Expressions.CompilePropertyCompoundAssignment(Ctx,
      TGocciaPropertyCompoundAssignmentExpression(AExpr), ADest)
  else if AExpr is TGocciaComputedPropertyCompoundAssignmentExpression then
    Goccia.Compiler.Expressions.CompileComputedPropertyCompoundAssignment(Ctx,
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr), ADest)
  else if AExpr is TGocciaCompoundAssignmentExpression then
    Goccia.Compiler.Expressions.CompileCompoundAssignment(Ctx, TGocciaCompoundAssignmentExpression(AExpr), ADest)
  else if AExpr is TGocciaAssignmentExpression then
    Goccia.Compiler.Expressions.CompileAssignment(Ctx, TGocciaAssignmentExpression(AExpr), ADest)
  else if AExpr is TGocciaPropertyAssignmentExpression then
    Goccia.Compiler.Expressions.CompilePropertyAssignment(Ctx, TGocciaPropertyAssignmentExpression(AExpr), ADest)
  else if AExpr is TGocciaComputedPropertyAssignmentExpression then
    Goccia.Compiler.Expressions.CompileComputedPropertyAssignment(Ctx, TGocciaComputedPropertyAssignmentExpression(AExpr), ADest)
  else if AExpr is TGocciaMethodExpression then
    Goccia.Compiler.Expressions.CompileMethod(Ctx, TGocciaMethodExpression(AExpr), ADest)
  else if AExpr is TGocciaArrowFunctionExpression then
    Goccia.Compiler.Expressions.CompileArrowFunction(Ctx, TGocciaArrowFunctionExpression(AExpr), ADest)
  else if AExpr is TGocciaCallExpression then
    Goccia.Compiler.Expressions.CompileCall(Ctx, TGocciaCallExpression(AExpr), ADest)
  else if AExpr is TGocciaMemberExpression then
    Goccia.Compiler.Expressions.CompileMember(Ctx, TGocciaMemberExpression(AExpr), ADest)
  else if AExpr is TGocciaConditionalExpression then
    Goccia.Compiler.Expressions.CompileConditional(Ctx, TGocciaConditionalExpression(AExpr), ADest)
  else if AExpr is TGocciaArrayExpression then
    Goccia.Compiler.Expressions.CompileArray(Ctx, TGocciaArrayExpression(AExpr), ADest)
  else if AExpr is TGocciaObjectExpression then
    Goccia.Compiler.Expressions.CompileObject(Ctx, TGocciaObjectExpression(AExpr), ADest)
  else if AExpr is TGocciaTaggedTemplateExpression then
    Goccia.Compiler.Expressions.CompileTaggedTemplate(Ctx, TGocciaTaggedTemplateExpression(AExpr), ADest)
  else if AExpr is TGocciaTemplateWithInterpolationExpression then
    Goccia.Compiler.Expressions.CompileTemplateWithInterpolation(Ctx, TGocciaTemplateWithInterpolationExpression(AExpr), ADest)
  else if AExpr is TGocciaTemplateLiteralExpression then
    Goccia.Compiler.Expressions.CompileTemplateLiteral(Ctx, TGocciaTemplateLiteralExpression(AExpr), ADest)
  else if AExpr is TGocciaNewExpression then
    Goccia.Compiler.Expressions.CompileNewExpression(Ctx, TGocciaNewExpression(AExpr), ADest)
  else if AExpr is TGocciaIncrementExpression then
    Goccia.Compiler.Expressions.CompileIncrement(Ctx, TGocciaIncrementExpression(AExpr), ADest)
  else if AExpr is TGocciaThisExpression then
    Goccia.Compiler.Expressions.CompileThis(Ctx, ADest)
  else if AExpr is TGocciaSuperExpression then
  begin
    Goccia.Compiler.Expressions.CompileSuperAccess(Ctx, ADest);
  end
  else if AExpr is TGocciaImportMetaExpression then
    Goccia.Compiler.Expressions.CompileImportMeta(Ctx, ADest)
  else if AExpr is TGocciaImportCallExpression then
    Goccia.Compiler.Expressions.CompileDynamicImport(Ctx,
      TGocciaImportCallExpression(AExpr), ADest)
  else if AExpr is TGocciaPrivatePropertyCompoundAssignmentExpression then
    Goccia.Compiler.Expressions.CompilePrivatePropertyCompoundAssignment(Ctx,
      TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr), ADest)
  else if AExpr is TGocciaPrivatePropertyAssignmentExpression then
    Goccia.Compiler.Expressions.CompilePrivatePropertyAssignment(Ctx,
      TGocciaPrivatePropertyAssignmentExpression(AExpr), ADest)
  else if AExpr is TGocciaPrivateMemberExpression then
    Goccia.Compiler.Expressions.CompilePrivateMember(Ctx,
      TGocciaPrivateMemberExpression(AExpr), ADest)
  else if AExpr is TGocciaDestructuringAssignmentExpression then
    Goccia.Compiler.Expressions.CompileDestructuringAssignment(Ctx,
      TGocciaDestructuringAssignmentExpression(AExpr), ADest)
  else if AExpr is TGocciaClassExpression then
    Goccia.Compiler.Statements.CompileClassExpression(Ctx,
      TGocciaClassExpression(AExpr).ClassDefinition, ADest)
  else if AExpr is TGocciaAwaitExpression then
  begin
    DoCompileExpression(TGocciaAwaitExpression(AExpr).Operand, ADest);
    EmitInstruction(Ctx, EncodeABC(OP_AWAIT, ADest, ADest, 0));
  end
  else if AExpr is TGocciaYieldExpression then
    Goccia.Compiler.Expressions.CompileYield(Ctx,
      TGocciaYieldExpression(AExpr), ADest)
  else if AExpr is TGocciaIsExpression then
    Goccia.Compiler.PatternMatching.CompileIsExpression(Ctx,
      TGocciaIsExpression(AExpr), ADest)
  else if AExpr is TGocciaMatchExpression then
    Goccia.Compiler.PatternMatching.CompileMatchExpression(Ctx,
      TGocciaMatchExpression(AExpr), ADest)
  else if AExpr is TGocciaHoleExpression then
    EmitInstruction(Ctx, EncodeABC(OP_LOAD_HOLE, ADest, 0, 0))
  else
    EmitInstruction(Ctx, EncodeABC(OP_LOAD_UNDEFINED, ADest, 0, 0));
end;

function TGocciaCompiler.DoCompileStatement(const AStmt: TGocciaStatement): Boolean;
var
  Ctx: TGocciaCompilationContext;
begin
  Result := False;
  Ctx := BuildContext;
  EmitLineMapping(Ctx, AStmt.Line, AStmt.Column);

  if AStmt is TGocciaExpressionStatement then
    Goccia.Compiler.Statements.CompileExpressionStatement(Ctx, TGocciaExpressionStatement(AStmt))
  else if AStmt is TGocciaVariableDeclaration then
    Goccia.Compiler.Statements.CompileVariableDeclaration(Ctx, TGocciaVariableDeclaration(AStmt))
  else if AStmt is TGocciaBlockStatement then
    Result := Goccia.Compiler.Statements.CompileBlockStatement(Ctx,
      TGocciaBlockStatement(AStmt))
  else if AStmt is TGocciaIfStatement then
    Result := Goccia.Compiler.Statements.CompileIfStatement(Ctx,
      TGocciaIfStatement(AStmt))
  else if AStmt is TGocciaReturnStatement then
  begin
    Goccia.Compiler.Statements.CompileReturnStatement(Ctx,
      TGocciaReturnStatement(AStmt));
    Result := True;
  end
  else if AStmt is TGocciaThrowStatement then
  begin
    Goccia.Compiler.Statements.CompileThrowStatement(Ctx,
      TGocciaThrowStatement(AStmt));
    Result := True;
  end
  else if AStmt is TGocciaTryStatement then
    Goccia.Compiler.Statements.CompileTryStatement(Ctx, TGocciaTryStatement(AStmt))
  else if AStmt is TGocciaForAwaitOfStatement then
    Goccia.Compiler.Statements.CompileForAwaitOfStatement(Ctx, TGocciaForAwaitOfStatement(AStmt))
  else if AStmt is TGocciaForOfStatement then
    Goccia.Compiler.Statements.CompileForOfStatement(Ctx, TGocciaForOfStatement(AStmt))
  else if AStmt is TGocciaClassDeclaration then
    Goccia.Compiler.Statements.CompileClassDeclaration(Ctx,
      TGocciaClassDeclaration(AStmt))
  else if AStmt is TGocciaSwitchStatement then
    Goccia.Compiler.Statements.CompileSwitchStatement(Ctx, TGocciaSwitchStatement(AStmt))
  else if AStmt is TGocciaBreakStatement then
  begin
    Goccia.Compiler.Statements.CompileBreakStatement(Ctx);
    Result := True;
  end
  else if AStmt is TGocciaContinueStatement then
  begin
    Goccia.Compiler.Statements.CompileContinueStatement(Ctx);
    Result := True;
  end
  else if AStmt is TGocciaImportDeclaration then
    Goccia.Compiler.Statements.CompileImportDeclaration(Ctx, TGocciaImportDeclaration(AStmt))
  else if AStmt is TGocciaExportVariableDeclaration then
    Goccia.Compiler.Statements.CompileExportVariableDeclaration(Ctx, TGocciaExportVariableDeclaration(AStmt))
  else if AStmt is TGocciaExportDeclaration then
    Goccia.Compiler.Statements.CompileExportDeclaration(Ctx, TGocciaExportDeclaration(AStmt))
  else if AStmt is TGocciaReExportDeclaration then
    Goccia.Compiler.Statements.CompileReExportDeclaration(Ctx, TGocciaReExportDeclaration(AStmt))
  else if AStmt is TGocciaDestructuringDeclaration then
    Goccia.Compiler.Statements.CompileDestructuringDeclaration(Ctx, TGocciaDestructuringDeclaration(AStmt))
  else if AStmt is TGocciaEnumDeclaration then
    Goccia.Compiler.Statements.CompileEnumDeclaration(Ctx, TGocciaEnumDeclaration(AStmt))
  else if AStmt is TGocciaExportEnumDeclaration then
    Goccia.Compiler.Statements.CompileExportEnumDeclaration(Ctx,
      TGocciaExportEnumDeclaration(AStmt))
  else if AStmt is TGocciaUsingDeclaration then
    Goccia.Compiler.Statements.CompileUsingDeclaration(Ctx,
      TGocciaUsingDeclaration(AStmt));

  if not Result then
    Result := Goccia.Compiler.Statements.StatementAlwaysAbrupt(Ctx, AStmt);
end;

procedure HoistVarLocals(const ANode: TGocciaASTNode; const AScope: TGocciaCompilerScope); forward;

// Pre-declare all identifier bindings from a destructuring pattern so that
// hoisted function declarations can resolve upvalue captures.
procedure PredeclareDestructuringLocals(const APattern: TGocciaDestructuringPattern;
  const AScope: TGocciaCompilerScope; const AIsConst: Boolean);
var
  ObjPat: TGocciaObjectDestructuringPattern;
  ArrPat: TGocciaArrayDestructuringPattern;
  I: Integer;
begin
  if APattern is TGocciaIdentifierDestructuringPattern then
  begin
    if AScope.ResolveLocal(TGocciaIdentifierDestructuringPattern(APattern).Name) < 0 then
      AScope.DeclareLocal(TGocciaIdentifierDestructuringPattern(APattern).Name, AIsConst);
  end
  else if APattern is TGocciaObjectDestructuringPattern then
  begin
    ObjPat := TGocciaObjectDestructuringPattern(APattern);
    for I := 0 to ObjPat.Properties.Count - 1 do
      PredeclareDestructuringLocals(ObjPat.Properties[I].Pattern, AScope, AIsConst);
  end
  else if APattern is TGocciaArrayDestructuringPattern then
  begin
    ArrPat := TGocciaArrayDestructuringPattern(APattern);
    for I := 0 to ArrPat.Elements.Count - 1 do
      if Assigned(ArrPat.Elements[I]) then
        PredeclareDestructuringLocals(ArrPat.Elements[I], AScope, AIsConst);
  end
  else if APattern is TGocciaAssignmentDestructuringPattern then
    PredeclareDestructuringLocals(TGocciaAssignmentDestructuringPattern(APattern).Left, AScope, AIsConst)
  else if APattern is TGocciaRestDestructuringPattern then
    PredeclareDestructuringLocals(TGocciaRestDestructuringPattern(APattern).Argument, AScope, AIsConst);
end;

// Pre-declare all bindings from a TGocciaVariableDeclaration that are not
// already resolved, so hoisted function declarations can capture them.
procedure PredeclareVarDeclLocals(const AVarDecl: TGocciaVariableDeclaration;
  const AScope: TGocciaCompilerScope);
var
  J: Integer;
begin
  for J := 0 to High(AVarDecl.Variables) do
    if AScope.ResolveLocal(AVarDecl.Variables[J].Name) < 0 then
      AScope.DeclareLocal(AVarDecl.Variables[J].Name, AVarDecl.IsConst);
end;

// Pre-declare lexical locals from any statement node that introduces bindings,
// so that hoisted function declarations can resolve upvalue captures.
procedure PredeclareLexicalLocals(const ANode: TGocciaASTNode;
  const AScope: TGocciaCompilerScope);
var
  VarDecl: TGocciaVariableDeclaration;
begin
  if ANode is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(ANode);
    if (not VarDecl.IsVar) and (not VarDecl.IsFunctionDeclaration) then
      PredeclareVarDeclLocals(VarDecl, AScope);
  end
  else if ANode is TGocciaExportVariableDeclaration then
  begin
    VarDecl := TGocciaExportVariableDeclaration(ANode).Declaration;
    if (not VarDecl.IsVar) and (not VarDecl.IsFunctionDeclaration) then
      PredeclareVarDeclLocals(VarDecl, AScope);
  end
  else if (ANode is TGocciaDestructuringDeclaration) and
          not TGocciaDestructuringDeclaration(ANode).IsVar then
    PredeclareDestructuringLocals(
      TGocciaDestructuringDeclaration(ANode).Pattern, AScope,
      TGocciaDestructuringDeclaration(ANode).IsConst)
  else if ANode is TGocciaClassDeclaration then
  begin
    if AScope.ResolveLocal(TGocciaClassDeclaration(ANode).ClassDefinition.Name) < 0 then
      AScope.DeclareLocal(TGocciaClassDeclaration(ANode).ClassDefinition.Name, True);
  end
  else if ANode is TGocciaEnumDeclaration then
  begin
    if AScope.ResolveLocal(TGocciaEnumDeclaration(ANode).Name) < 0 then
      AScope.DeclareLocal(TGocciaEnumDeclaration(ANode).Name, False);
  end
  else if ANode is TGocciaExportEnumDeclaration then
  begin
    if AScope.ResolveLocal(TGocciaExportEnumDeclaration(ANode).Declaration.Name) < 0 then
      AScope.DeclareLocal(TGocciaExportEnumDeclaration(ANode).Declaration.Name, False);
  end;
end;

// Returns the inner TGocciaVariableDeclaration if the node is a function
// declaration (either directly or wrapped in TGocciaExportVariableDeclaration),
// or nil otherwise.
function GetFunctionDecl(const ANode: TGocciaASTNode): TGocciaVariableDeclaration;
begin
  if ANode is TGocciaVariableDeclaration then
    Result := TGocciaVariableDeclaration(ANode)
  else if ANode is TGocciaExportVariableDeclaration then
    Result := TGocciaExportVariableDeclaration(ANode).Declaration
  else
    Exit(nil);
  if not Result.IsFunctionDeclaration then
    Result := nil;
end;

procedure TGocciaCompiler.DoCompileFunctionBody(const ABody: TGocciaASTNode);
var
  Block: TGocciaBlockStatement;
  I: Integer;
  Node: TGocciaASTNode;
  Reg: UInt8;
  HasUsing, HasFunctionDecl: Boolean;
  StatementAbrupt: Boolean;
  SavedFinally: TObject;
begin
  SavedFinally := Goccia.Compiler.Statements.SavePendingFinally;
  try
    if ABody is TGocciaBlockStatement then
    begin
      Block := TGocciaBlockStatement(ABody);

      // Hoist var declarations to function scope
      for I := 0 to Block.Nodes.Count - 1 do
        HoistVarLocals(Block.Nodes[I], FCurrentScope);

      // Check if there are function declarations to hoist
      HasFunctionDecl := False;
      for I := 0 to Block.Nodes.Count - 1 do
        if GetFunctionDecl(Block.Nodes[I]) <> nil then
        begin
          HasFunctionDecl := True;
          Break;
        end;

      if HasFunctionDecl then
      begin
        // Pre-declare lexical locals so function declarations can resolve
        // upvalue captures for let/const variables declared later in the block
        for I := 0 to Block.Nodes.Count - 1 do
          PredeclareLexicalLocals(Block.Nodes[I], FCurrentScope);

        // Hoist function declarations: compile initializers before other statements
        for I := 0 to Block.Nodes.Count - 1 do
          if GetFunctionDecl(Block.Nodes[I]) <> nil then
            DoCompileStatement(TGocciaStatement(Block.Nodes[I]));
      end;

      // Check if the body contains using declarations — if so, delegate
      // to CompileBlockStatement which handles the try/finally disposal.
      HasUsing := False;
      for I := 0 to Block.Nodes.Count - 1 do
        if Block.Nodes[I] is TGocciaUsingDeclaration then
        begin
          HasUsing := True;
          Break;
        end;

      if HasUsing then
        StatementAbrupt := Goccia.Compiler.Statements.CompileBlockStatement(
          BuildContext, Block)
      else
      begin
        StatementAbrupt := False;
        for I := 0 to Block.Nodes.Count - 1 do
        begin
          Node := Block.Nodes[I];
          // Skip function declarations — already compiled during hoisting
          if GetFunctionDecl(Node) <> nil then
            Continue;
          if Node is TGocciaStatement then
          begin
            StatementAbrupt := DoCompileStatement(TGocciaStatement(Node));
            if FOptimizationOptions.EnableDeadBranchElimination and
               not FOptimizationOptions.PreserveCoverageShape and
               StatementAbrupt then
              Break;
          end
          else if Node is TGocciaExpression then
          begin
            Reg := FCurrentScope.AllocateRegister;
            DoCompileExpression(TGocciaExpression(Node), Reg);
            FCurrentScope.FreeRegister;
          end;
        end;
      end;
      Reg := FCurrentScope.AllocateRegister;
      EmitInstruction(BuildContext, EncodeABC(OP_LOAD_UNDEFINED, Reg, 0, 0));
      EmitInstruction(BuildContext, EncodeABC(OP_RETURN, Reg, 0, 0));
      FCurrentScope.FreeRegister;
    end
    else if ABody is TGocciaExpression then
    begin
      Reg := FCurrentScope.AllocateRegister;
      DoCompileExpression(TGocciaExpression(ABody), Reg);
      EmitInstruction(BuildContext, EncodeABC(OP_RETURN, Reg, 0, 0));
      FCurrentScope.FreeRegister;
    end
    else
    begin
      Reg := FCurrentScope.AllocateRegister;
      EmitInstruction(BuildContext, EncodeABC(OP_LOAD_UNDEFINED, Reg, 0, 0));
      EmitInstruction(BuildContext, EncodeABC(OP_RETURN, Reg, 0, 0));
      FCurrentScope.FreeRegister;
    end;
  finally
    Goccia.Compiler.Statements.RestorePendingFinally(SavedFinally);
  end;
end;

procedure HoistVarLocals(const ANode: TGocciaASTNode; const AScope: TGocciaCompilerScope);
var
  Block: TGocciaBlockStatement;
  IfStmt: TGocciaIfStatement;
  ForOf: TGocciaForOfStatement;
  TryStmt: TGocciaTryStatement;
  SwitchStmt: TGocciaSwitchStatement;
  VarDecl: TGocciaVariableDeclaration;
  DestructDecl: TGocciaDestructuringDeclaration;
  I, J: Integer;
begin
  if ANode is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(ANode);
    if VarDecl.IsVar then
      for I := 0 to High(VarDecl.Variables) do
        AScope.DeclareVarLocal(VarDecl.Variables[I].Name);
  end
  else if ANode is TGocciaDestructuringDeclaration then
  begin
    DestructDecl := TGocciaDestructuringDeclaration(ANode);
    if DestructDecl.IsVar then
      CollectDestructuringVarBindings(DestructDecl.Pattern, AScope);
  end
  else if ANode is TGocciaBlockStatement then
  begin
    Block := TGocciaBlockStatement(ANode);
    for I := 0 to Block.Nodes.Count - 1 do
      HoistVarLocals(Block.Nodes[I], AScope);
  end
  else if ANode is TGocciaIfStatement then
  begin
    IfStmt := TGocciaIfStatement(ANode);
    HoistVarLocals(IfStmt.Consequent, AScope);
    if Assigned(IfStmt.Alternate) then
      HoistVarLocals(IfStmt.Alternate, AScope);
  end
  else if ANode is TGocciaForOfStatement then
  begin
    ForOf := TGocciaForOfStatement(ANode);
    HoistVarLocals(ForOf.Body, AScope);
  end
  else if ANode is TGocciaTryStatement then
  begin
    TryStmt := TGocciaTryStatement(ANode);
    if Assigned(TryStmt.Block) then
      HoistVarLocals(TryStmt.Block, AScope);
    if Assigned(TryStmt.CatchBlock) then
      HoistVarLocals(TryStmt.CatchBlock, AScope);
    if Assigned(TryStmt.FinallyBlock) then
      HoistVarLocals(TryStmt.FinallyBlock, AScope);
  end
  else if ANode is TGocciaSwitchStatement then
  begin
    SwitchStmt := TGocciaSwitchStatement(ANode);
    for I := 0 to SwitchStmt.Cases.Count - 1 do
      for J := 0 to SwitchStmt.Cases[I].Consequent.Count - 1 do
        HoistVarLocals(SwitchStmt.Cases[I].Consequent[J], AScope);
  end;
end;

procedure HoistVarLocalsFromStatements(const AStatements: TObjectList<TGocciaStatement>;
  const AScope: TGocciaCompilerScope);
var
  I: Integer;
begin
  for I := 0 to AStatements.Count - 1 do
    HoistVarLocals(AStatements[I], AScope);
end;

procedure MarkHoistedVarsGlobalBacked(const AScope: TGocciaCompilerScope);
var
  I: Integer;
  Local: TGocciaCompilerLocal;
begin
  for I := 0 to AScope.LocalCount - 1 do
  begin
    Local := AScope.GetLocal(I);
    if (Local.Depth = 0) and (Local.Name <> '__receiver') then
      AScope.MarkGlobalBacked(I);
  end;
end;

procedure EmitHoistedGlobalVarDeclarations(const ACtx: TGocciaCompilationContext;
  const AScope: TGocciaCompilerScope);
const
  GLOBAL_DEFINE_VAR_DECL = 3;
var
  I: Integer;
  Local: TGocciaCompilerLocal;
  NameIdx: UInt16;
begin
  for I := 0 to AScope.LocalCount - 1 do
  begin
    Local := AScope.GetLocal(I);
    if (Local.Depth <> 0) or (Local.Name = '__receiver') then
      Continue;

    NameIdx := ACtx.Template.AddConstantString(Local.Name);
    if NameIdx > High(UInt8) then
      raise Exception.Create('Constant pool overflow: global name index exceeds 255');
    EmitInstruction(ACtx, EncodeABC(OP_DEFINE_GLOBAL_CONST, Local.Slot,
      GLOBAL_DEFINE_VAR_DECL, UInt8(NameIdx)));
  end;
end;

function TGocciaCompiler.Compile(
  const AProgram: TGocciaProgram): TGocciaBytecodeModule;
var
  I: Integer;
  LastStmt: TGocciaStatement;
  RetReg: UInt8;
  Ctx: TGocciaCompilationContext;
  HasFunctionDecl, BodyAbrupt, StatementAbrupt: Boolean;
begin
  FModule := TGocciaBytecodeModule.Create(GOCCIA_RUNTIME_TAG, FSourcePath);
  FCurrentTemplate := TGocciaFunctionTemplate.Create('<module>');
  FTopLevelTemplate := FCurrentTemplate;
  FCurrentTemplate.DebugInfo := TGocciaDebugInfo.Create(FSourcePath);
  FCurrentScope := TGocciaCompilerScope.Create(nil, 0);
  FCurrentScope.DeclareLocal('__receiver', False);

  try
    // Hoist var declarations to module scope
    HoistVarLocalsFromStatements(AProgram.Body, FCurrentScope);
    if FGlobalBackedTopLevel then
    begin
      MarkHoistedVarsGlobalBacked(FCurrentScope);
      EmitHoistedGlobalVarDeclarations(BuildContext, FCurrentScope);
    end;

    // Check if there are function declarations to hoist
    HasFunctionDecl := False;
    for I := 0 to AProgram.Body.Count - 1 do
      if GetFunctionDecl(AProgram.Body[I]) <> nil then
      begin
        HasFunctionDecl := True;
        Break;
      end;

    if HasFunctionDecl then
    begin
      // Pre-declare lexical locals so function declarations can resolve
      // upvalue captures for let/const variables declared later in the body
      for I := 0 to AProgram.Body.Count - 1 do
        PredeclareLexicalLocals(AProgram.Body[I], FCurrentScope);

      // Hoist function declarations: compile initializers before other statements
      for I := 0 to AProgram.Body.Count - 1 do
        if GetFunctionDecl(AProgram.Body[I]) <> nil then
          DoCompileStatement(AProgram.Body[I]);
    end;

    if AProgram.Body.Count > 0 then
    begin
      BodyAbrupt := False;
      for I := 0 to AProgram.Body.Count - 2 do
      begin
        // Skip function declarations — already compiled during hoisting
        if GetFunctionDecl(AProgram.Body[I]) <> nil then
          Continue;
        StatementAbrupt := DoCompileStatement(AProgram.Body[I]);
        if FOptimizationOptions.EnableDeadBranchElimination and
           not FOptimizationOptions.PreserveCoverageShape and
           StatementAbrupt then
        begin
          BodyAbrupt := True;
          Break;
        end;
      end;

      LastStmt := AProgram.Body[AProgram.Body.Count - 1];
      // Function declarations already compiled during hoisting — just emit return undefined
      if BodyAbrupt then
      begin
        EmitInstruction(BuildContext, EncodeABC(OP_LOAD_UNDEFINED, 0, 0, 0));
        EmitInstruction(BuildContext, EncodeABC(OP_RETURN, 0, 0, 0));
      end
      else if GetFunctionDecl(LastStmt) <> nil then
      begin
        EmitInstruction(BuildContext, EncodeABC(OP_LOAD_UNDEFINED, 0, 0, 0));
        EmitInstruction(BuildContext, EncodeABC(OP_RETURN, 0, 0, 0));
      end
      else if LastStmt is TGocciaExpressionStatement then
      begin
        RetReg := FCurrentScope.AllocateRegister;
        Ctx := BuildContext;
        EmitLineMapping(Ctx, LastStmt.Line, LastStmt.Column);
        DoCompileExpression(TGocciaExpressionStatement(LastStmt).Expression, RetReg);
        EmitInstruction(Ctx, EncodeABC(OP_RETURN, RetReg, 0, 0));
        FCurrentScope.FreeRegister;
      end
      else
      begin
        DoCompileStatement(LastStmt);
        EmitInstruction(BuildContext, EncodeABC(OP_LOAD_UNDEFINED, 0, 0, 0));
        EmitInstruction(BuildContext, EncodeABC(OP_RETURN, 0, 0, 0));
      end;
    end
    else
      EmitInstruction(BuildContext, EncodeABC(OP_LOAD_UNDEFINED, 0, 0, 0));
      EmitInstruction(BuildContext, EncodeABC(OP_RETURN, 0, 0, 0));

    FCurrentTemplate.MaxRegisters := FCurrentScope.MaxSlot;
    FModule.TopLevel := FCurrentTemplate;

    Result := FModule;
    FModule := nil;
    FCurrentTemplate := nil;
    FTopLevelTemplate := nil;
  finally
    FreeAndNil(FCurrentScope);
    FTopLevelTemplate := nil;
    if Assigned(FModule) then
    begin
      FreeAndNil(FCurrentTemplate);
      FreeAndNil(FModule);
    end;
  end;
end;

end.
