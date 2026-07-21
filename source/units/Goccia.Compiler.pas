unit Goccia.Compiler;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

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
    FNumericParameterProofs: TNumericParameterProofMap;
    FGlobalBackedTopLevel: Boolean;
    FAsyncTopLevel: Boolean;
    FPreinitializedTopLevelFunctions: Boolean;
    FStrictTypes: Boolean;
    FNonStrictMode: Boolean;
    FArgumentsObjectEnabled: Boolean;
    FLabelReentryStatement: TGocciaStatement;
    FOptimizationOptions: TGocciaCompilerOptimizationOptions;
    FDerivedConstructorThisGuard: Boolean;
    FTemplateDerivedConstructorThisGuards: TDictionary<TGocciaFunctionTemplate, Boolean>;
    procedure DoCompileExpression(const AExpr: TGocciaExpression;
      const ADest: UInt16);
    function DoCompileStatement(const AStmt: TGocciaStatement): Boolean;
    procedure DoCompileFunctionBody(const ABody: TGocciaASTNode);
    procedure DoSwapState(const ATemplate: TGocciaFunctionTemplate;
      const AScope: TGocciaCompilerScope);
    procedure DoSetDerivedConstructorThisGuard(const AGuard: Boolean);
    procedure DoSetNonStrictMode(const AEnabled: Boolean);
    function BuildContext: TGocciaCompilationContext;
    function NonStrictBlockFunctionVarBindingsEnabled: Boolean;
  public
    constructor Create(const ASourcePath: string);
    destructor Destroy; override;

    function Compile(const AProgram: TGocciaProgram): TGocciaBytecodeModule;
    property FormalParameterCounts: TFormalParameterCountMap
      read FFormalParameterCounts;
    property GlobalBackedTopLevel: Boolean read FGlobalBackedTopLevel
      write FGlobalBackedTopLevel;
    property AsyncTopLevel: Boolean read FAsyncTopLevel write FAsyncTopLevel;
    property PreinitializedTopLevelFunctions: Boolean
      read FPreinitializedTopLevelFunctions write FPreinitializedTopLevelFunctions;
    property StrictTypes: Boolean read FStrictTypes write FStrictTypes;
    property NonStrictMode: Boolean read FNonStrictMode write FNonStrictMode;
    property ArgumentsObjectEnabled: Boolean
      read FArgumentsObjectEnabled write FArgumentsObjectEnabled;
    property OptimizationOptions: TGocciaCompilerOptimizationOptions
      read FOptimizationOptions write FOptimizationOptions;
  end;

const
  GOCCIA_RUNTIME_TAG = 'goccia-js';

implementation

uses
  Classes,
  SysUtils,

  OrderedStringMap,
  UnicodeStringList,

  Goccia.AST.BindingPatterns,
  Goccia.Bytecode,
  Goccia.Bytecode.Debug,
  Goccia.Compiler.ConstantFolding,
  Goccia.Compiler.Expressions,
  Goccia.Compiler.NumericProof,
  Goccia.Compiler.PatternMatching,
  Goccia.Compiler.Statements,
  Goccia.Keywords.Reserved,
  Goccia.Modules;

{ TGocciaCompiler }

constructor TGocciaCompiler.Create(const ASourcePath: string);
begin
  inherited Create;
  FSourcePath := ASourcePath;
  FFormalParameterCounts := TFormalParameterCountMap.Create;
  FNumericParameterProofs := TNumericParameterProofMap.Create;
  FTemplateDerivedConstructorThisGuards :=
    TDictionary<TGocciaFunctionTemplate, Boolean>.Create;
  FDerivedConstructorThisGuard := False;
  FModule := nil;
  FCurrentTemplate := nil;
  FTopLevelTemplate := nil;
  FCurrentScope := nil;
  FOptimizationOptions := DefaultCompilerOptimizationOptions;
end;

destructor TGocciaCompiler.Destroy;
begin
  FNumericParameterProofs.Free;
  FTemplateDerivedConstructorThisGuards.Free;
  FFormalParameterCounts.Free;
  inherited;
end;

function TGocciaCompiler.BuildContext: TGocciaCompilationContext;
begin
  Result.Template := FCurrentTemplate;
  Result.Scope := FCurrentScope;
  Result.SourcePath := FSourcePath;
  Result.FormalParameterCounts := FFormalParameterCounts;
  Result.NumericParameterProofs := FNumericParameterProofs;
  Result.GlobalBackedTopLevel := FGlobalBackedTopLevel and
    (FCurrentTemplate = FTopLevelTemplate);
  Result.PreinitializedTopLevelFunctions := FPreinitializedTopLevelFunctions and
    (FCurrentTemplate = FTopLevelTemplate);
  Result.StrictTypes := FStrictTypes;
  Result.CompatibilityNonStrictMode := FNonStrictMode;
  Result.ArgumentsObjectEnabled := FArgumentsObjectEnabled;
  Result.NonStrictMode := FNonStrictMode and not FCurrentTemplate.StrictCode;
  Result.DerivedConstructorThisGuard := FDerivedConstructorThisGuard;
  Result.OptimizationOptions := FOptimizationOptions;
  Result.CompileExpression := DoCompileExpression;
  Result.CompileStatement := DoCompileStatement;
  Result.CompileFunctionBody := DoCompileFunctionBody;
  Result.SwapState := DoSwapState;
  Result.SetDerivedConstructorThisGuard := DoSetDerivedConstructorThisGuard;
  Result.SetNonStrictMode := DoSetNonStrictMode;
end;

function TGocciaCompiler.NonStrictBlockFunctionVarBindingsEnabled: Boolean;
begin
  Result := FNonStrictMode and Assigned(FCurrentTemplate) and
    not FCurrentTemplate.StrictCode;
end;

procedure TGocciaCompiler.DoSetDerivedConstructorThisGuard(
  const AGuard: Boolean);
begin
  FDerivedConstructorThisGuard := AGuard;
  if Assigned(FCurrentTemplate) then
    FTemplateDerivedConstructorThisGuards.AddOrSetValue(
      FCurrentTemplate, AGuard);
end;

procedure TGocciaCompiler.DoSetNonStrictMode(const AEnabled: Boolean);
begin
  FNonStrictMode := AEnabled;
end;

procedure TGocciaCompiler.DoSwapState(
  const ATemplate: TGocciaFunctionTemplate;
  const AScope: TGocciaCompilerScope);
begin
  if Assigned(FCurrentTemplate) then
    FTemplateDerivedConstructorThisGuards.AddOrSetValue(
      FCurrentTemplate, FDerivedConstructorThisGuard);
  FCurrentTemplate := ATemplate;
  FCurrentScope := AScope;
  if not Assigned(FCurrentTemplate) or
     not FTemplateDerivedConstructorThisGuards.TryGetValue(
       FCurrentTemplate, FDerivedConstructorThisGuard) then
    FDerivedConstructorThisGuard := False;
end;

procedure TGocciaCompiler.DoCompileExpression(
  const AExpr: TGocciaExpression; const ADest: UInt16);
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
  else if AExpr is TGocciaObjectMethodDefinition then
    Goccia.Compiler.Expressions.CompileFunctionExpression(Ctx,
      TGocciaObjectMethodDefinition(AExpr).FunctionExpression, ADest, '<method>')
  else if AExpr is TGocciaFunctionExpression then
    Goccia.Compiler.Expressions.CompileFunctionExpression(Ctx, TGocciaFunctionExpression(AExpr), ADest)
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
  else if AExpr is TGocciaNewTargetExpression then
    Goccia.Compiler.Expressions.CompileNewTarget(Ctx, ADest)
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
  PreviousLabelReentryStatement: TGocciaStatement;
begin
  Result := False;
  Ctx := BuildContext;
  if (AStmt.LabelCount > 0) and (FLabelReentryStatement <> AStmt) then
  begin
    PreviousLabelReentryStatement := FLabelReentryStatement;
    FLabelReentryStatement := AStmt;
    try
      Exit(Goccia.Compiler.Statements.CompileLabeledStatement(Ctx, AStmt));
    finally
      FLabelReentryStatement := PreviousLabelReentryStatement;
    end;
  end;

  EmitLineMapping(Ctx, AStmt.Line, AStmt.Column);

  if AStmt is TGocciaExpressionStatement then
    Goccia.Compiler.Statements.CompileExpressionStatement(Ctx, TGocciaExpressionStatement(AStmt))
  else if AStmt is TGocciaVariableDeclaration then
    Goccia.Compiler.Statements.CompileVariableDeclaration(Ctx, TGocciaVariableDeclaration(AStmt))
  else if AStmt is TGocciaFunctionDeclaration then
    Goccia.Compiler.Statements.CompileFunctionDeclaration(Ctx, TGocciaFunctionDeclaration(AStmt))
  else if AStmt is TGocciaBlockStatement then
    Result := Goccia.Compiler.Statements.CompileBlockStatement(Ctx,
      TGocciaBlockStatement(AStmt))
  else if AStmt is TGocciaIfStatement then
    Result := Goccia.Compiler.Statements.CompileIfStatement(Ctx,
      TGocciaIfStatement(AStmt))
  else if AStmt is TGocciaWithStatement then
    Result := Goccia.Compiler.Statements.CompileWithStatement(Ctx,
      TGocciaWithStatement(AStmt))
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
  else if AStmt is TGocciaForInStatement then
    Goccia.Compiler.Statements.CompileForInStatement(Ctx,
      TGocciaForInStatement(AStmt))
  else if AStmt is TGocciaForOfStatement then
    Goccia.Compiler.Statements.CompileForOfStatement(Ctx, TGocciaForOfStatement(AStmt))
  else if AStmt is TGocciaForStatement then
    Goccia.Compiler.Statements.CompileForStatement(Ctx, TGocciaForStatement(AStmt))
  else if AStmt is TGocciaWhileStatement then
    Goccia.Compiler.Statements.CompileWhileStatement(Ctx,
      TGocciaWhileStatement(AStmt))
  else if AStmt is TGocciaDoWhileStatement then
    Goccia.Compiler.Statements.CompileDoWhileStatement(Ctx,
      TGocciaDoWhileStatement(AStmt))
  else if AStmt is TGocciaClassDeclaration then
    Goccia.Compiler.Statements.CompileClassDeclaration(Ctx,
      TGocciaClassDeclaration(AStmt))
  else if AStmt is TGocciaSwitchStatement then
    Goccia.Compiler.Statements.CompileSwitchStatement(Ctx, TGocciaSwitchStatement(AStmt))
  else if AStmt is TGocciaBreakStatement then
  begin
    Goccia.Compiler.Statements.CompileBreakStatement(Ctx,
      TGocciaBreakStatement(AStmt));
    Result := True;
  end
  else if AStmt is TGocciaContinueStatement then
  begin
    Goccia.Compiler.Statements.CompileContinueStatement(Ctx,
      TGocciaContinueStatement(AStmt));
    Result := True;
  end
  else if AStmt is TGocciaImportDeclaration then
    Goccia.Compiler.Statements.CompileImportDeclaration(Ctx, TGocciaImportDeclaration(AStmt))
  else if AStmt is TGocciaExportVariableDeclaration then
    Goccia.Compiler.Statements.CompileExportVariableDeclaration(Ctx, TGocciaExportVariableDeclaration(AStmt))
  else if AStmt is TGocciaExportDestructuringDeclaration then
    Goccia.Compiler.Statements.CompileExportDestructuringDeclaration(Ctx,
      TGocciaExportDestructuringDeclaration(AStmt))
  else if AStmt is TGocciaExportFunctionDeclaration then
    Goccia.Compiler.Statements.CompileExportFunctionDeclaration(Ctx, TGocciaExportFunctionDeclaration(AStmt))
  else if AStmt is TGocciaExportClassDeclaration then
    Goccia.Compiler.Statements.CompileExportClassDeclaration(Ctx,
      TGocciaExportClassDeclaration(AStmt))
  else if AStmt is TGocciaExportDeclaration then
    Goccia.Compiler.Statements.CompileExportDeclaration(Ctx, TGocciaExportDeclaration(AStmt))
  else if AStmt is TGocciaExportDefaultDeclaration then
    Goccia.Compiler.Statements.CompileExportDefaultDeclaration(Ctx,
      TGocciaExportDefaultDeclaration(AStmt))
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

procedure HoistVarLocals(const ANode: TGocciaASTNode;
  const AScope: TGocciaCompilerScope;
  const AIncludeNonStrictBlockFunctionVarBindings: Boolean;
  const AAtVarScopedLevel: Boolean;
  const ASkipUninitializedVars: Boolean = False); forward;

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
  I: Integer;
  Names: TUnicodeStringList;
begin
  Names := TUnicodeStringList.Create;
  try
    CollectVariableDeclarationBindingNames(AVarDecl, Names, True);
    for I := 0 to Names.Count - 1 do
      if AScope.ResolveLocal(Names[I]) < 0 then
        AScope.DeclareLocal(Names[I], AVarDecl.IsConst);
  finally
    Names.Free;
  end;
end;

// Pre-declare lexical locals from any statement node that introduces bindings,
// so that hoisted function declarations can resolve upvalue captures.
procedure PredeclareLexicalLocals(const ANode: TGocciaASTNode;
  const AScope: TGocciaCompilerScope);
var
  ImportDecl: TGocciaImportDeclaration;
  ImportPair: TStringStringMap.TKeyValuePair;
  EncodedPath: string;
  LocalIdx: Integer;
  VarDecl: TGocciaVariableDeclaration;
  DestructDecl: TGocciaDestructuringDeclaration;
begin
  if ANode is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(ANode);
    if not VarDecl.IsVar then
      PredeclareVarDeclLocals(VarDecl, AScope);
  end
  else if ANode is TGocciaExportVariableDeclaration then
  begin
    VarDecl := TGocciaExportVariableDeclaration(ANode).Declaration;
    if not VarDecl.IsVar then
      PredeclareVarDeclLocals(VarDecl, AScope);
  end
  else if (ANode is TGocciaDestructuringDeclaration) and
          not TGocciaDestructuringDeclaration(ANode).IsVar then
    PredeclareDestructuringLocals(
      TGocciaDestructuringDeclaration(ANode).Pattern, AScope,
      TGocciaDestructuringDeclaration(ANode).IsConst)
  else if (ANode is TGocciaExportDestructuringDeclaration) and
          not TGocciaExportDestructuringDeclaration(ANode).Declaration.IsVar then
  begin
    DestructDecl := TGocciaExportDestructuringDeclaration(ANode).Declaration;
    PredeclareDestructuringLocals(DestructDecl.Pattern, AScope,
      DestructDecl.IsConst);
  end
  else if ANode is TGocciaClassDeclaration then
  begin
    if AScope.ResolveLocal(TGocciaClassDeclaration(ANode).ClassDefinition.Name) < 0 then
      AScope.DeclareLocal(TGocciaClassDeclaration(ANode).ClassDefinition.Name, False);
  end
  else if ANode is TGocciaExportClassDeclaration then
  begin
    if AScope.ResolveLocal(TGocciaExportClassDeclaration(ANode).Declaration.ClassDefinition.Name) < 0 then
      AScope.DeclareLocal(TGocciaExportClassDeclaration(ANode).Declaration.ClassDefinition.Name, False);
  end
  else if (ANode is TGocciaExportDefaultDeclaration) and
          (TGocciaExportDefaultDeclaration(ANode).LocalName <> GOCCIA_DEFAULT_EXPORT_BINDING) then
  begin
    if AScope.ResolveLocal(TGocciaExportDefaultDeclaration(ANode).LocalName) < 0 then
      AScope.DeclareLocal(TGocciaExportDefaultDeclaration(ANode).LocalName,
        not ((TGocciaExportDefaultDeclaration(ANode).Expression is
          TGocciaFunctionExpression) or
        ((TGocciaExportDefaultDeclaration(ANode).Expression is
          TGocciaClassExpression) and
        (TGocciaClassExpression(TGocciaExportDefaultDeclaration(ANode)
          .Expression).ClassDefinition.Name =
          TGocciaExportDefaultDeclaration(ANode).LocalName))));
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
  end
  else if ANode is TGocciaImportDeclaration then
  begin
    ImportDecl := TGocciaImportDeclaration(ANode);
    EncodedPath := EncodeImportSpecifierAttribute(ImportDecl.ModulePath,
      ImportDecl.AttributeType);
    if (ImportDecl.NamespaceName <> '') and
       (AScope.ResolveLocal(ImportDecl.NamespaceName) < 0) then
      AScope.DeclareLocal(ImportDecl.NamespaceName, True);
    if ImportDecl.NamespaceName <> '' then
    begin
      LocalIdx := AScope.ResolveLocal(ImportDecl.NamespaceName);
      if LocalIdx >= 0 then
        AScope.MarkImportBinding(LocalIdx, ImportDecl.Phase, EncodedPath, '');
    end;
    for ImportPair in ImportDecl.Imports do
    begin
      if AScope.ResolveLocal(ImportPair.Key) < 0 then
        AScope.DeclareLocal(ImportPair.Key, True);
      LocalIdx := AScope.ResolveLocal(ImportPair.Key);
      if LocalIdx >= 0 then
      begin
        if ImportDecl.Phase = icpEvaluation then
          AScope.MarkImportBinding(LocalIdx, ImportDecl.Phase, EncodedPath,
            ImportPair.Value)
        else
          AScope.MarkImportBinding(LocalIdx, ImportDecl.Phase, EncodedPath,
            '');
      end;
    end;
  end;
end;

procedure MarkLocalExport(const AScope: TGocciaCompilerScope;
  const ALocalName, AExportName: string);
var
  LocalIdx: Integer;
begin
  LocalIdx := AScope.ResolveLocal(ALocalName);
  if LocalIdx >= 0 then
    AScope.MarkExportBinding(LocalIdx, AExportName);
end;

procedure MarkModuleExportLocals(const AStatements: TObjectList<TGocciaStatement>;
  const AScope: TGocciaCompilerScope);
var
  ExportDecl: TGocciaExportDeclaration;
  ExportDefaultDecl: TGocciaExportDefaultDeclaration;
  ExportDestructuringDecl: TGocciaExportDestructuringDeclaration;
  ExportPair: TStringStringMap.TKeyValuePair;
  ExportVarDecl: TGocciaExportVariableDeclaration;
  I: Integer;
  Name: string;
  Names: TUnicodeStringList;
  Stmt: TGocciaStatement;
begin
  for I := 0 to AStatements.Count - 1 do
  begin
    Stmt := AStatements[I];
    if Stmt is TGocciaExportDeclaration then
    begin
      ExportDecl := TGocciaExportDeclaration(Stmt);
      for ExportPair in ExportDecl.ExportsTable do
        MarkLocalExport(AScope, ExportPair.Value, ExportPair.Key);
    end
    else if Stmt is TGocciaExportDefaultDeclaration then
    begin
      ExportDefaultDecl := TGocciaExportDefaultDeclaration(Stmt);
      MarkLocalExport(AScope, ExportDefaultDecl.LocalName, KEYWORD_DEFAULT);
    end
    else if Stmt is TGocciaExportVariableDeclaration then
    begin
      ExportVarDecl := TGocciaExportVariableDeclaration(Stmt);
      Names := TUnicodeStringList.Create;
      try
        CollectVariableDeclarationBindingNames(ExportVarDecl.Declaration,
          Names, True);
        for Name in Names do
          MarkLocalExport(AScope, Name, Name);
      finally
        Names.Free;
      end;
    end
    else if Stmt is TGocciaExportDestructuringDeclaration then
    begin
      ExportDestructuringDecl := TGocciaExportDestructuringDeclaration(Stmt);
      Names := TUnicodeStringList.Create;
      try
        CollectPatternBindingNames(ExportDestructuringDecl.Declaration.Pattern,
          Names, True);
        for Name in Names do
          MarkLocalExport(AScope, Name, Name);
      finally
        Names.Free;
      end;
    end
    else if Stmt is TGocciaExportFunctionDeclaration then
      MarkLocalExport(AScope,
        TGocciaExportFunctionDeclaration(Stmt).Declaration.Name,
        TGocciaExportFunctionDeclaration(Stmt).Declaration.Name)
    else if Stmt is TGocciaExportClassDeclaration then
      MarkLocalExport(AScope,
        TGocciaExportClassDeclaration(Stmt).Declaration.ClassDefinition.Name,
        TGocciaExportClassDeclaration(Stmt).Declaration.ClassDefinition.Name)
    else if Stmt is TGocciaExportEnumDeclaration then
      MarkLocalExport(AScope,
        TGocciaExportEnumDeclaration(Stmt).Declaration.Name,
        TGocciaExportEnumDeclaration(Stmt).Declaration.Name);
  end;
end;

procedure MarkTopLevelGlobalBackedLocals(const AScope: TGocciaCompilerScope);
var
  I: Integer;
  Local: TGocciaCompilerLocal;
begin
  for I := 0 to AScope.LocalCount - 1 do
  begin
    Local := AScope.GetLocal(I);
    if (Local.Depth = 0) and (Local.Name <> '__receiver') and
       not Local.IsImportBinding then
      AScope.MarkGlobalBacked(I);
  end;
end;

// Returns the inner TGocciaFunctionDeclaration if the node is a function
// declaration (either directly or wrapped in TGocciaExportFunctionDeclaration),
// or nil otherwise.
function GetFunctionDecl(const ANode: TGocciaASTNode): TGocciaFunctionDeclaration;
begin
  if ANode is TGocciaFunctionDeclaration then
    Result := TGocciaFunctionDeclaration(ANode)
  else if ANode is TGocciaExportFunctionDeclaration then
    Result := TGocciaExportFunctionDeclaration(ANode).Declaration
  else
    Result := nil;
end;

function IsNamedDefaultFunctionDeclaration(
  const ANode: TGocciaASTNode): Boolean;
var
  ExportDefault: TGocciaExportDefaultDeclaration;
begin
  Result := False;
  if not (ANode is TGocciaExportDefaultDeclaration) then
    Exit;

  ExportDefault := TGocciaExportDefaultDeclaration(ANode);
  Result := (ExportDefault.LocalName <> GOCCIA_DEFAULT_EXPORT_BINDING) and
    (ExportDefault.Expression is TGocciaFunctionExpression) and
    (TGocciaFunctionExpression(ExportDefault.Expression).Name =
    ExportDefault.LocalName);
end;

function IsHoistedFunctionDeclaration(const ANode: TGocciaASTNode): Boolean;
begin
  Result := (GetFunctionDecl(ANode) <> nil) or
    IsNamedDefaultFunctionDeclaration(ANode);
end;

procedure TGocciaCompiler.DoCompileFunctionBody(const ABody: TGocciaASTNode);
var
  Block: TGocciaBlockStatement;
  I: Integer;
  Node: TGocciaASTNode;
  Reg: UInt16;
  HasUsing, HasFunctionDecl: Boolean;
  StatementAbrupt: Boolean;
  SavedFinally: TObject;
  PredeclaredLexicalStart, PredeclaredLexicalIndex: Integer;
  PredeclaredLocal: TGocciaCompilerLocal;
begin
  SavedFinally := Goccia.Compiler.Statements.SavePendingFinally;
  try
    if ABody is TGocciaBlockStatement then
    begin
      Block := TGocciaBlockStatement(ABody);

      DiscoverClosedCallNumericProof(Block, FNumericParameterProofs);

      // Hoist var declarations to function scope
      for I := 0 to Block.Nodes.Count - 1 do
        HoistVarLocals(Block.Nodes[I], FCurrentScope,
          NonStrictBlockFunctionVarBindingsEnabled, True);

      // Check if there are function declarations to hoist
      HasFunctionDecl := False;
      for I := 0 to Block.Nodes.Count - 1 do
        if IsHoistedFunctionDeclaration(Block.Nodes[I]) then
        begin
          HasFunctionDecl := True;
          Break;
        end;

      // Function-body lexical declarations are instantiated before any body
      // statement executes, regardless of whether the body also contains a
      // hoisted function declaration. This establishes the TDZ for earlier
      // references and assignments.
      PredeclaredLexicalStart := FCurrentScope.LocalCount;
      for I := 0 to Block.Nodes.Count - 1 do
        PredeclareLexicalLocals(Block.Nodes[I], FCurrentScope);
      for PredeclaredLexicalIndex := PredeclaredLexicalStart to
        FCurrentScope.LocalCount - 1 do
      begin
        PredeclaredLocal := FCurrentScope.GetLocal(PredeclaredLexicalIndex);
        if not PredeclaredLocal.IsVar then
          EmitInstruction(BuildContext, EncodeABC(OP_LOAD_HOLE,
            PredeclaredLocal.Slot, 0, 0));
      end;

      if HasFunctionDecl then
      begin
        // Hoist function declarations: compile initializers before other statements
        for I := 0 to Block.Nodes.Count - 1 do
          if IsHoistedFunctionDeclaration(Block.Nodes[I]) then
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
          if IsHoistedFunctionDeclaration(Node) then
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

procedure HoistVarLocals(const ANode: TGocciaASTNode;
  const AScope: TGocciaCompilerScope;
  const AIncludeNonStrictBlockFunctionVarBindings: Boolean;
  const AAtVarScopedLevel: Boolean;
  const ASkipUninitializedVars: Boolean = False);
var
  Block: TGocciaBlockStatement;
  IfStmt: TGocciaIfStatement;
  ForOf: TGocciaForOfStatement;
  ForIn: TGocciaForInStatement;
  ForStmt: TGocciaForStatement;
  WhileStmt: TGocciaWhileStatement;
  DoWhileStmt: TGocciaDoWhileStatement;
  TryStmt: TGocciaTryStatement;
  SwitchStmt: TGocciaSwitchStatement;
  WithStmt: TGocciaWithStatement;
  VarDecl: TGocciaVariableDeclaration;
  DestructDecl: TGocciaDestructuringDeclaration;
  Names: TUnicodeStringList;
  I, J: Integer;
begin
  if ANode is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(ANode);
    if VarDecl.IsVar then
      for I := 0 to High(VarDecl.Variables) do
        if not (ASkipUninitializedVars and
           (not VarDecl.Variables[I].HasInitializer)) then
        begin
          Names := TUnicodeStringList.Create;
          try
            CollectVariableInfoBindingNames(VarDecl.Variables[I], Names, True);
            for J := 0 to Names.Count - 1 do
              AScope.DeclareVarLocal(Names[J]);
          finally
            Names.Free;
          end;
        end;
  end
  else if (ANode is TGocciaFunctionDeclaration) and
          (AAtVarScopedLevel or
          (AIncludeNonStrictBlockFunctionVarBindings and
          not TGocciaFunctionDeclaration(ANode).FunctionExpression.IsAsync and
          not TGocciaFunctionDeclaration(ANode).FunctionExpression.IsGenerator)) then
    AScope.DeclareVarLocal(TGocciaFunctionDeclaration(ANode).Name)
  else if ANode is TGocciaExportVariableDeclaration then
  begin
    VarDecl := TGocciaExportVariableDeclaration(ANode).Declaration;
    if VarDecl.IsVar then
      for I := 0 to High(VarDecl.Variables) do
        if not (ASkipUninitializedVars and
           (not VarDecl.Variables[I].HasInitializer)) then
        begin
          Names := TUnicodeStringList.Create;
          try
            CollectVariableInfoBindingNames(VarDecl.Variables[I], Names, True);
            for J := 0 to Names.Count - 1 do
              AScope.DeclareVarLocal(Names[J]);
          finally
            Names.Free;
          end;
        end;
  end
  else if (ANode is TGocciaExportFunctionDeclaration) and
          (AAtVarScopedLevel or
          (AIncludeNonStrictBlockFunctionVarBindings and
          not TGocciaExportFunctionDeclaration(ANode).Declaration.FunctionExpression.IsAsync and
          not TGocciaExportFunctionDeclaration(ANode).Declaration.FunctionExpression.IsGenerator)) then
    AScope.DeclareVarLocal(
      TGocciaExportFunctionDeclaration(ANode).Declaration.Name)
  else if ANode is TGocciaDestructuringDeclaration then
  begin
    DestructDecl := TGocciaDestructuringDeclaration(ANode);
    if DestructDecl.IsVar then
      CollectDestructuringVarBindings(DestructDecl.Pattern, AScope);
  end
  else if ANode is TGocciaExportDestructuringDeclaration then
  begin
    DestructDecl := TGocciaExportDestructuringDeclaration(ANode).Declaration;
    if DestructDecl.IsVar then
      CollectDestructuringVarBindings(DestructDecl.Pattern, AScope);
  end
  else if ANode is TGocciaBlockStatement then
  begin
    Block := TGocciaBlockStatement(ANode);
    for I := 0 to Block.Nodes.Count - 1 do
      HoistVarLocals(Block.Nodes[I], AScope,
        AIncludeNonStrictBlockFunctionVarBindings, False, ASkipUninitializedVars);
  end
  else if ANode is TGocciaIfStatement then
  begin
    IfStmt := TGocciaIfStatement(ANode);
    HoistVarLocals(IfStmt.Consequent, AScope,
      AIncludeNonStrictBlockFunctionVarBindings, False, ASkipUninitializedVars);
    if Assigned(IfStmt.Alternate) then
      HoistVarLocals(IfStmt.Alternate, AScope,
        AIncludeNonStrictBlockFunctionVarBindings, False, ASkipUninitializedVars);
  end
  else if ANode is TGocciaForOfStatement then
  begin
    ForOf := TGocciaForOfStatement(ANode);
    HoistVarLocals(ForOf.Body, AScope,
      AIncludeNonStrictBlockFunctionVarBindings, False, ASkipUninitializedVars);
  end
  else if ANode is TGocciaForInStatement then
  begin
    ForIn := TGocciaForInStatement(ANode);
    if ForIn.IsVar then
    begin
      if Assigned(ForIn.BindingPattern) then
        CollectDestructuringVarBindings(ForIn.BindingPattern, AScope)
      else if ForIn.BindingName <> '' then
        AScope.DeclareVarLocal(ForIn.BindingName);
    end;
    HoistVarLocals(ForIn.Body, AScope,
      AIncludeNonStrictBlockFunctionVarBindings, False, ASkipUninitializedVars);
  end
  else if ANode is TGocciaForStatement then
  begin
    ForStmt := TGocciaForStatement(ANode);
    if Assigned(ForStmt.Init) then
      HoistVarLocals(ForStmt.Init, AScope,
        AIncludeNonStrictBlockFunctionVarBindings, AAtVarScopedLevel,
        ASkipUninitializedVars);
    HoistVarLocals(ForStmt.Body, AScope,
      AIncludeNonStrictBlockFunctionVarBindings, False, ASkipUninitializedVars);
  end
  else if ANode is TGocciaWhileStatement then
  begin
    WhileStmt := TGocciaWhileStatement(ANode);
    HoistVarLocals(WhileStmt.Body, AScope,
      AIncludeNonStrictBlockFunctionVarBindings, False, ASkipUninitializedVars);
  end
  else if ANode is TGocciaDoWhileStatement then
  begin
    DoWhileStmt := TGocciaDoWhileStatement(ANode);
    HoistVarLocals(DoWhileStmt.Body, AScope,
      AIncludeNonStrictBlockFunctionVarBindings, False, ASkipUninitializedVars);
  end
  else if ANode is TGocciaWithStatement then
  begin
    WithStmt := TGocciaWithStatement(ANode);
    HoistVarLocals(WithStmt.Body, AScope,
      AIncludeNonStrictBlockFunctionVarBindings, False, ASkipUninitializedVars);
  end
  else if ANode is TGocciaTryStatement then
  begin
    TryStmt := TGocciaTryStatement(ANode);
    if Assigned(TryStmt.Block) then
      HoistVarLocals(TryStmt.Block, AScope,
        AIncludeNonStrictBlockFunctionVarBindings, False, ASkipUninitializedVars);
    if Assigned(TryStmt.CatchBlock) then
      HoistVarLocals(TryStmt.CatchBlock, AScope,
        AIncludeNonStrictBlockFunctionVarBindings, False, ASkipUninitializedVars);
    if Assigned(TryStmt.FinallyBlock) then
      HoistVarLocals(TryStmt.FinallyBlock, AScope,
        AIncludeNonStrictBlockFunctionVarBindings, False, ASkipUninitializedVars);
  end
  else if ANode is TGocciaSwitchStatement then
  begin
    SwitchStmt := TGocciaSwitchStatement(ANode);
    for I := 0 to SwitchStmt.Cases.Count - 1 do
      for J := 0 to SwitchStmt.Cases[I].Consequent.Count - 1 do
        HoistVarLocals(SwitchStmt.Cases[I].Consequent[J], AScope,
          AIncludeNonStrictBlockFunctionVarBindings, False, ASkipUninitializedVars);
  end;
end;

procedure HoistVarLocalsFromStatements(const AStatements: TObjectList<TGocciaStatement>;
  const AScope: TGocciaCompilerScope;
  const AIncludeNonStrictBlockFunctionVarBindings: Boolean;
  const ASkipUninitializedVars: Boolean = False);
var
  I: Integer;
begin
  for I := 0 to AStatements.Count - 1 do
    HoistVarLocals(AStatements[I], AScope,
      AIncludeNonStrictBlockFunctionVarBindings, True, ASkipUninitializedVars);
end;

procedure AddUniqueVarName(const ANames: TUnicodeStringList;
  const AName: string);
begin
  if (AName <> '') and (ANames.IndexOf(AName) < 0) then
    ANames.Add(AName);
end;

procedure CollectUninitializedVarDeclarations(const ANode: TGocciaASTNode;
  const ANames: TUnicodeStringList);
var
  Block: TGocciaBlockStatement;
  DoWhileStmt: TGocciaDoWhileStatement;
  ForOf: TGocciaForOfStatement;
  ForIn: TGocciaForInStatement;
  ForStmt: TGocciaForStatement;
  IfStmt: TGocciaIfStatement;
  I, J: Integer;
  SwitchStmt: TGocciaSwitchStatement;
  TryStmt: TGocciaTryStatement;
  VarDecl: TGocciaVariableDeclaration;
  WhileStmt: TGocciaWhileStatement;
  WithStmt: TGocciaWithStatement;
begin
  if ANode is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(ANode);
    if VarDecl.IsVar then
      for I := 0 to High(VarDecl.Variables) do
        if not VarDecl.Variables[I].HasInitializer then
          AddUniqueVarName(ANames, VarDecl.Variables[I].Name);
  end
  else if ANode is TGocciaExportVariableDeclaration then
  begin
    VarDecl := TGocciaExportVariableDeclaration(ANode).Declaration;
    if VarDecl.IsVar then
      for I := 0 to High(VarDecl.Variables) do
        if not VarDecl.Variables[I].HasInitializer then
          AddUniqueVarName(ANames, VarDecl.Variables[I].Name);
  end
  else if ANode is TGocciaBlockStatement then
  begin
    Block := TGocciaBlockStatement(ANode);
    for I := 0 to Block.Nodes.Count - 1 do
      CollectUninitializedVarDeclarations(Block.Nodes[I], ANames);
  end
  else if ANode is TGocciaIfStatement then
  begin
    IfStmt := TGocciaIfStatement(ANode);
    CollectUninitializedVarDeclarations(IfStmt.Consequent, ANames);
    if Assigned(IfStmt.Alternate) then
      CollectUninitializedVarDeclarations(IfStmt.Alternate, ANames);
  end
  else if ANode is TGocciaForOfStatement then
  begin
    ForOf := TGocciaForOfStatement(ANode);
    CollectUninitializedVarDeclarations(ForOf.Body, ANames);
  end
  else if ANode is TGocciaForInStatement then
  begin
    ForIn := TGocciaForInStatement(ANode);
    CollectUninitializedVarDeclarations(ForIn.Body, ANames);
  end
  else if ANode is TGocciaForStatement then
  begin
    ForStmt := TGocciaForStatement(ANode);
    if Assigned(ForStmt.Init) then
      CollectUninitializedVarDeclarations(ForStmt.Init, ANames);
    CollectUninitializedVarDeclarations(ForStmt.Body, ANames);
  end
  else if ANode is TGocciaWhileStatement then
  begin
    WhileStmt := TGocciaWhileStatement(ANode);
    CollectUninitializedVarDeclarations(WhileStmt.Body, ANames);
  end
  else if ANode is TGocciaDoWhileStatement then
  begin
    DoWhileStmt := TGocciaDoWhileStatement(ANode);
    CollectUninitializedVarDeclarations(DoWhileStmt.Body, ANames);
  end
  else if ANode is TGocciaWithStatement then
  begin
    WithStmt := TGocciaWithStatement(ANode);
    CollectUninitializedVarDeclarations(WithStmt.Body, ANames);
  end
  else if ANode is TGocciaTryStatement then
  begin
    TryStmt := TGocciaTryStatement(ANode);
    if Assigned(TryStmt.Block) then
      CollectUninitializedVarDeclarations(TryStmt.Block, ANames);
    if Assigned(TryStmt.CatchBlock) then
      CollectUninitializedVarDeclarations(TryStmt.CatchBlock, ANames);
    if Assigned(TryStmt.FinallyBlock) then
      CollectUninitializedVarDeclarations(TryStmt.FinallyBlock, ANames);
  end
  else if ANode is TGocciaSwitchStatement then
  begin
    SwitchStmt := TGocciaSwitchStatement(ANode);
    for I := 0 to SwitchStmt.Cases.Count - 1 do
      for J := 0 to SwitchStmt.Cases[I].Consequent.Count - 1 do
        CollectUninitializedVarDeclarations(
          SwitchStmt.Cases[I].Consequent[J], ANames);
  end;
end;

procedure EmitHoistedGlobalVarDeclarationsForNames(
  const ACtx: TGocciaCompilationContext;
  const ANames: TUnicodeStringList);
var
  I: Integer;
  NameIdx: UInt16;
begin
  for I := 0 to ANames.Count - 1 do
  begin
    NameIdx := ACtx.Template.AddConstantString(ANames[I]);
    EmitInstruction(ACtx, EncodeABx(OP_DEFINE_GLOBAL_VAR_DECL_LONG, 0,
      NameIdx));
  end;
end;

procedure EmitUninitializedTopLevelGlobalVarDeclarations(
  const ACtx: TGocciaCompilationContext;
  const AStatements: TObjectList<TGocciaStatement>);
var
  I: Integer;
  Names: TUnicodeStringList;
begin
  Names := TUnicodeStringList.Create;
  try
    for I := 0 to AStatements.Count - 1 do
      CollectUninitializedVarDeclarations(AStatements[I], Names);
    EmitHoistedGlobalVarDeclarationsForNames(ACtx, Names);
  finally
    Names.Free;
  end;
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
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, Local.Slot, 0, 0));
    EmitInstruction(ACtx, EncodeABx(OP_DEFINE_GLOBAL_VAR_DECL_LONG,
      Local.Slot, NameIdx));
  end;
end;

procedure EmitGlobalLexicalPredeclarations(const ACtx: TGocciaCompilationContext;
  const AScope: TGocciaCompilerScope; const AStartIndex: Integer);
var
  I: Integer;
  OpCode: TGocciaOpCode;
  Local: TGocciaCompilerLocal;
  NameIdx: UInt16;
begin
  for I := AStartIndex to AScope.LocalCount - 1 do
  begin
    Local := AScope.GetLocal(I);
    if (Local.Depth <> 0) or Local.IsVar or Local.IsImportBinding or
       (Local.Name = '__receiver') then
      Continue;

    if Local.IsConst then
      OpCode := OP_PREDECLARE_GLOBAL_CONST_LONG
    else
      OpCode := OP_PREDECLARE_GLOBAL_LET_LONG;
    NameIdx := ACtx.Template.AddConstantString(Local.Name);
    EmitInstruction(ACtx, EncodeABx(OpCode, Local.Slot, NameIdx));
  end;
end;

function TGocciaCompiler.Compile(
  const AProgram: TGocciaProgram): TGocciaBytecodeModule;
var
  I: Integer;
  RetReg: UInt16;
  Ctx: TGocciaCompilationContext;
  HasFunctionDecl, BodyAbrupt, StatementAbrupt: Boolean;
  PredeclaredLexicalStart, PredeclaredLexicalIndex: Integer;
  PredeclaredLocal: TGocciaCompilerLocal;
begin
  FNumericParameterProofs.Clear;
  FModule := TGocciaBytecodeModule.Create(GOCCIA_RUNTIME_TAG, FSourcePath);
  FCurrentTemplate := TGocciaFunctionTemplate.Create('<module>');
  FTopLevelTemplate := FCurrentTemplate;
  FCurrentTemplate.DebugInfo := TGocciaDebugInfo.Create(FSourcePath);
  FCurrentTemplate.IsAsync := FAsyncTopLevel;
  FCurrentTemplate.StrictCode := (not FNonStrictMode) or
    HasUseStrictDirective(AProgram);
  FCurrentScope := TGocciaCompilerScope.Create(nil, 0);
  FCurrentScope.DeclareLocal('__receiver', False);

  try
    // Hoist var declarations to module scope.
    HoistVarLocalsFromStatements(AProgram.Body, FCurrentScope,
      NonStrictBlockFunctionVarBindingsEnabled,
      FGlobalBackedTopLevel);
    if FGlobalBackedTopLevel then
    begin
      MarkHoistedVarsGlobalBacked(FCurrentScope);
      EmitHoistedGlobalVarDeclarations(BuildContext, FCurrentScope);
      EmitUninitializedTopLevelGlobalVarDeclarations(BuildContext,
        AProgram.Body);
    end;

    PredeclaredLexicalStart := FCurrentScope.LocalCount;
    for I := 0 to AProgram.Body.Count - 1 do
      PredeclareLexicalLocals(AProgram.Body[I], FCurrentScope);
    MarkModuleExportLocals(AProgram.Body, FCurrentScope);
    if FGlobalBackedTopLevel then
      MarkTopLevelGlobalBackedLocals(FCurrentScope);
    Ctx := BuildContext;
    if FGlobalBackedTopLevel then
      EmitGlobalLexicalPredeclarations(Ctx, FCurrentScope,
        PredeclaredLexicalStart);
    for PredeclaredLexicalIndex := PredeclaredLexicalStart to
      FCurrentScope.LocalCount - 1 do
    begin
      PredeclaredLocal := FCurrentScope.GetLocal(PredeclaredLexicalIndex);
      if not PredeclaredLocal.IsVar then
        EmitInstruction(Ctx, EncodeABC(OP_LOAD_HOLE,
          PredeclaredLocal.Slot, 0, 0));
    end;

    // Check if there are function declarations to hoist
    HasFunctionDecl := False;
    for I := 0 to AProgram.Body.Count - 1 do
      if IsHoistedFunctionDeclaration(AProgram.Body[I]) then
      begin
        HasFunctionDecl := True;
        Break;
      end;

    if HasFunctionDecl then
    begin
      // Hoist function declarations: compile initializers before other statements
      for I := 0 to AProgram.Body.Count - 1 do
        if IsHoistedFunctionDeclaration(AProgram.Body[I]) then
          DoCompileStatement(AProgram.Body[I]);
    end;

    RetReg := FCurrentScope.AllocateRegister;
    Ctx := BuildContext;
    EmitInstruction(Ctx, EncodeABC(OP_LOAD_UNDEFINED, RetReg, 0, 0));
    BodyAbrupt := False;

    for I := 0 to AProgram.Body.Count - 1 do
      if (AProgram.Body[I] is TGocciaImportDeclaration) or
         (AProgram.Body[I] is TGocciaReExportDeclaration) then
        DoCompileStatement(AProgram.Body[I]);

    for I := 0 to AProgram.Body.Count - 1 do
    begin
      // Skip function declarations — already compiled during hoisting.
      if IsHoistedFunctionDeclaration(AProgram.Body[I]) or
         (AProgram.Body[I] is TGocciaImportDeclaration) or
         (AProgram.Body[I] is TGocciaReExportDeclaration) then
        Continue;

      if AProgram.Body[I] is TGocciaExpressionStatement then
      begin
        EmitLineMapping(Ctx, AProgram.Body[I].Line, AProgram.Body[I].Column);
        DoCompileExpression(
          TGocciaExpressionStatement(AProgram.Body[I]).Expression, RetReg);
        StatementAbrupt := False;
      end
      else
        StatementAbrupt := DoCompileStatement(AProgram.Body[I]);

      if FOptimizationOptions.EnableDeadBranchElimination and
         not FOptimizationOptions.PreserveCoverageShape and
         StatementAbrupt then
      begin
        BodyAbrupt := True;
        Break;
      end;
    end;

    EmitInstruction(Ctx, EncodeABC(OP_RETURN, RetReg, 0, 0));
    FCurrentScope.FreeRegister;

    FCurrentTemplate.MaxRegisters := FCurrentScope.MaxSlot;
    FModule.TopLevel := FCurrentTemplate;

    Result := FModule;
    FModule := nil;
    FCurrentTemplate := nil;
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
