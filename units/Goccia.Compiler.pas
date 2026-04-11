unit Goccia.Compiler;

{$I Goccia.inc}

interface

uses
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Bytecode.Chunk,
  Goccia.Bytecode.Module,
  Goccia.Compiler.Context,
  Goccia.Compiler.Scope;

type
  TGocciaCompiler = class
  private
    FModule: TGocciaBytecodeModule;
    FCurrentTemplate: TGocciaFunctionTemplate;
    FCurrentScope: TGocciaCompilerScope;
    FSourcePath: string;
    FFormalParameterCounts: TFormalParameterCountMap;
    FGlobalBackedTopLevel: Boolean;
    procedure DoCompileExpression(const AExpr: TGocciaExpression;
      const ADest: UInt8);
    procedure DoCompileStatement(const AStmt: TGocciaStatement);
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
  end;

const
  GOCCIA_RUNTIME_TAG = 'goccia-js';

implementation

uses
  SysUtils,

  Goccia.Bytecode,
  Goccia.Bytecode.Debug,
  Goccia.Compiler.Expressions,
  Goccia.Compiler.Statements;

{ TGocciaCompiler }

constructor TGocciaCompiler.Create(const ASourcePath: string);
begin
  inherited Create;
  FSourcePath := ASourcePath;
  FFormalParameterCounts := TFormalParameterCountMap.Create;
  FModule := nil;
  FCurrentTemplate := nil;
  FCurrentScope := nil;
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
  Result.GlobalBackedTopLevel := FGlobalBackedTopLevel;
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

  if AExpr is TGocciaLiteralExpression then
    Goccia.Compiler.Expressions.CompileLiteral(Ctx, TGocciaLiteralExpression(AExpr), ADest)
  else if AExpr is TGocciaRegexLiteralExpression then
    Goccia.Compiler.Expressions.CompileRegexLiteral(Ctx, TGocciaRegexLiteralExpression(AExpr), ADest)
  else if AExpr is TGocciaIdentifierExpression then
    Goccia.Compiler.Expressions.CompileIdentifier(Ctx, TGocciaIdentifierExpression(AExpr), ADest)
  else if AExpr is TGocciaBinaryExpression then
    Goccia.Compiler.Expressions.CompileBinary(Ctx, TGocciaBinaryExpression(AExpr), ADest)
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
  else if AExpr is TGocciaHoleExpression then
    EmitInstruction(Ctx, EncodeABC(OP_LOAD_HOLE, ADest, 0, 0))
  else
    EmitInstruction(Ctx, EncodeABC(OP_LOAD_UNDEFINED, ADest, 0, 0));
end;

procedure TGocciaCompiler.DoCompileStatement(const AStmt: TGocciaStatement);
var
  Ctx: TGocciaCompilationContext;
begin
  Ctx := BuildContext;
  EmitLineMapping(Ctx, AStmt.Line, AStmt.Column);

  if AStmt is TGocciaExpressionStatement then
    Goccia.Compiler.Statements.CompileExpressionStatement(Ctx, TGocciaExpressionStatement(AStmt))
  else if AStmt is TGocciaVariableDeclaration then
    Goccia.Compiler.Statements.CompileVariableDeclaration(Ctx, TGocciaVariableDeclaration(AStmt))
  else if AStmt is TGocciaBlockStatement then
    Goccia.Compiler.Statements.CompileBlockStatement(Ctx, TGocciaBlockStatement(AStmt))
  else if AStmt is TGocciaIfStatement then
    Goccia.Compiler.Statements.CompileIfStatement(Ctx, TGocciaIfStatement(AStmt))
  else if AStmt is TGocciaReturnStatement then
    Goccia.Compiler.Statements.CompileReturnStatement(Ctx, TGocciaReturnStatement(AStmt))
  else if AStmt is TGocciaThrowStatement then
    Goccia.Compiler.Statements.CompileThrowStatement(Ctx, TGocciaThrowStatement(AStmt))
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
    Goccia.Compiler.Statements.CompileBreakStatement(Ctx)
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
      TGocciaExportEnumDeclaration(AStmt));
end;

procedure TGocciaCompiler.DoCompileFunctionBody(const ABody: TGocciaASTNode);
var
  Block: TGocciaBlockStatement;
  I: Integer;
  Node: TGocciaASTNode;
  Reg: UInt8;
  SavedFinally: TObject;
begin
  SavedFinally := Goccia.Compiler.Statements.SavePendingFinally;
  try
    if ABody is TGocciaBlockStatement then
    begin
      Block := TGocciaBlockStatement(ABody);
      for I := 0 to Block.Nodes.Count - 1 do
      begin
        Node := Block.Nodes[I];
        if Node is TGocciaStatement then
          DoCompileStatement(TGocciaStatement(Node))
        else if Node is TGocciaExpression then
        begin
          Reg := FCurrentScope.AllocateRegister;
          DoCompileExpression(TGocciaExpression(Node), Reg);
          FCurrentScope.FreeRegister;
        end;
      end;
      EmitInstruction(BuildContext, EncodeABC(OP_LOAD_UNDEFINED, 0, 0, 0));
      EmitInstruction(BuildContext, EncodeABC(OP_RETURN, 0, 0, 0));
    end
    else if ABody is TGocciaExpression then
    begin
      Reg := FCurrentScope.AllocateRegister;
      DoCompileExpression(TGocciaExpression(ABody), Reg);
      EmitInstruction(BuildContext, EncodeABC(OP_RETURN, Reg, 0, 0));
      FCurrentScope.FreeRegister;
    end
    else
      EmitInstruction(BuildContext, EncodeABC(OP_LOAD_UNDEFINED, 0, 0, 0));
      EmitInstruction(BuildContext, EncodeABC(OP_RETURN, 0, 0, 0));
  finally
    Goccia.Compiler.Statements.RestorePendingFinally(SavedFinally);
  end;
end;

function TGocciaCompiler.Compile(
  const AProgram: TGocciaProgram): TGocciaBytecodeModule;
var
  I: Integer;
  LastStmt: TGocciaStatement;
  RetReg: UInt8;
  Ctx: TGocciaCompilationContext;
begin
  FModule := TGocciaBytecodeModule.Create(GOCCIA_RUNTIME_TAG, FSourcePath);
  FCurrentTemplate := TGocciaFunctionTemplate.Create('<module>');
  FCurrentTemplate.DebugInfo := TGocciaDebugInfo.Create(FSourcePath);
  FCurrentScope := TGocciaCompilerScope.Create(nil, 0);
  FCurrentScope.DeclareLocal('__receiver', False);

  try
    if AProgram.Body.Count > 0 then
    begin
      for I := 0 to AProgram.Body.Count - 2 do
        DoCompileStatement(AProgram.Body[I]);

      LastStmt := AProgram.Body[AProgram.Body.Count - 1];
      if LastStmt is TGocciaExpressionStatement then
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
  finally
    FreeAndNil(FCurrentScope);
    if Assigned(FModule) then
    begin
      FreeAndNil(FCurrentTemplate);
      FreeAndNil(FModule);
    end;
  end;
end;

end.
