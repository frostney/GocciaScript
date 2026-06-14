unit Goccia.Compiler.Statements;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Bytecode.Chunk,
  Goccia.Compiler.Context,
  Goccia.Compiler.Scope,
  Goccia.Compiler.TypeRules;

procedure CompileExpressionStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExpressionStatement);
procedure CompileVariableDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaVariableDeclaration);
procedure CompileFunctionDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaFunctionDeclaration);
function CompileBlockStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaBlockStatement): Boolean;
function CompileIfStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaIfStatement): Boolean;
function CompileWithStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaWithStatement): Boolean;
procedure CompileReturnStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaReturnStatement);
procedure CompileThrowStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaThrowStatement);
procedure CompileTryStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaTryStatement);
procedure CompileForOfStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaForOfStatement);
procedure CompileForInStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaForInStatement);
procedure CompileForAwaitOfStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaForAwaitOfStatement);
procedure CompileForStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaForStatement);
procedure CompileWhileStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaWhileStatement);
procedure CompileDoWhileStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaDoWhileStatement);
procedure CompileImportDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaImportDeclaration);
procedure CompileExportDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExportDeclaration);
procedure CompileExportDefaultDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExportDefaultDeclaration);
procedure CompileExportVariableDeclaration(
  const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExportVariableDeclaration);
procedure CompileExportDestructuringDeclaration(
  const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExportDestructuringDeclaration);
procedure CompileExportFunctionDeclaration(
  const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExportFunctionDeclaration);
procedure CompileExportClassDeclaration(
  const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExportClassDeclaration);
procedure CompileReExportDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaReExportDeclaration);
procedure CompileSwitchStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaSwitchStatement);
function CompileLabeledStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaStatement): Boolean;
procedure CompileBreakStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaBreakStatement);
procedure CompileContinueStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaContinueStatement);
procedure CompileDestructuringDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaDestructuringDeclaration);
procedure CompileEnumDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaEnumDeclaration);
procedure CompileExportEnumDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExportEnumDeclaration);

procedure CompileUsingDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaUsingDeclaration);

procedure CompileClassDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaClassDeclaration);
procedure CompileClassExpression(const ACtx: TGocciaCompilationContext;
  const AClassDef: TGocciaClassDefinition; const ADest: UInt8;
  const AInferredName: string = '');

function IsArrayTypeAnnotation(const AAnnotation: string): Boolean;
function StripArrayLayer(const AAnnotation: string): string;
function ExpressionType(const AScope: TGocciaCompilerScope;
  const AExpr: TGocciaExpression): TGocciaLocalType;
function CharToLocalType(const ACh: Char): TGocciaLocalType;
function StatementAlwaysAbrupt(const AStmt: TGocciaStatement): Boolean; overload;
function StatementAlwaysAbrupt(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaStatement): Boolean; overload;

procedure CollectDestructuringVarBindings(const APattern: TGocciaDestructuringPattern;
  const AScope: TGocciaCompilerScope);

function SavePendingFinally: TObject;
procedure RestorePendingFinally(const ASaved: TObject);

implementation

uses
  Classes,
  SysUtils,

  OrderedStringMap,

  Goccia.AST.BindingPatterns,
  Goccia.Bytecode,
  Goccia.Bytecode.Debug,
  Goccia.Compiler.ConstantFolding,
  Goccia.Compiler.ConstantValue,
  Goccia.Compiler.Expressions,
  Goccia.Compiler.PatternMatching,
  Goccia.Constants.PropertyNames,
  Goccia.Constants.TypeNames,
  Goccia.Error,
  Goccia.Keywords.Reserved,
  Goccia.Modules,
  Goccia.Token,
  Goccia.Types.Enforcement,
  Goccia.Values.Primitives;

type
  TUsingResourceEntry = record
    ValueSlot: UInt8;
    DisposeSlot: UInt8;
    IsAwait: Boolean;
  end;

  TPreallocatedUsingDisposeSlot = record
    Declaration: TGocciaUsingDeclaration;
    VariableIndex: Integer;
    DisposeSlot: UInt8;
    ResourceRegistered: Boolean;
  end;

  TComputedFieldKeyLocal = record
    ElementIndex: Integer;
    Name: string;
  end;

  TComputedFieldKeyLocals = array of TComputedFieldKeyLocal;

  TPendingFinallyEntry = record
    FinallyBlock: TGocciaBlockStatement;
    // Non-nil when this entry represents a using block's disposal.
    // CompileReturnStatement emits the disposal sequence instead of
    // compiling a FinallyBlock when this array is populated.
    UsingResources: array of TUsingResourceEntry;
    UsingErrorReg: UInt8;
    IsIteratorClose: Boolean;
    IsAsyncIterator: Boolean;
    IteratorReg: UInt8;
  end;

  TLoopControlState = record
    BreakJumps: TList<Integer>;
    ContinueJumps: TList<Integer>;
    OldBreakJumps: TList<Integer>;
    OldContinueJumps: TList<Integer>;
    OldBreakFinallyBase: Integer;
    OldContinueFinallyBase: Integer;
    OldBreakScopeDepth: Integer;
    OldContinueScopeDepth: Integer;
  end;

  TLabelControlState = record
    LabelName: string;
    BreakJumps: TList<Integer>;
    ContinueJumps: TList<Integer>;
    BreakFinallyBase: Integer;
    ContinueFinallyBase: Integer;
    BreakScopeDepth: Integer;
    ContinueScopeDepth: Integer;
    IsIteration: Boolean;
  end;

procedure EmitGlobalDefinesForPattern(const ACtx: TGocciaCompilationContext;
  const APattern: TGocciaDestructuringPattern; const AIsConst: Boolean;
  const AIsVar: Boolean; const AHasInitializer: Boolean); forward;

threadvar
  GBreakJumps: TList<Integer>;
  GContinueJumps: TList<Integer>;
  GPendingFinally: TList<TPendingFinallyEntry>;
  GBreakFinallyBase: Integer;
  GContinueFinallyBase: Integer;
  GBreakScopeDepth: Integer;
  GContinueScopeDepth: Integer;
  GLabelControls: TList<TLabelControlState>;

function CurrentPendingFinallyBase: Integer;
begin
  if Assigned(GPendingFinally) then
    Result := GPendingFinally.Count
  else
    Result := 0;
end;

procedure BeginLoopControl(const ACtx: TGocciaCompilationContext;
  out AState: TLoopControlState);
var
  PendingBase: Integer;
begin
  AState.OldBreakJumps := GBreakJumps;
  AState.OldBreakFinallyBase := GBreakFinallyBase;
  AState.OldBreakScopeDepth := GBreakScopeDepth;
  AState.BreakJumps := TList<Integer>.Create;
  GBreakJumps := AState.BreakJumps;

  AState.OldContinueJumps := GContinueJumps;
  AState.OldContinueFinallyBase := GContinueFinallyBase;
  AState.OldContinueScopeDepth := GContinueScopeDepth;
  AState.ContinueJumps := TList<Integer>.Create;
  GContinueJumps := AState.ContinueJumps;

  GBreakScopeDepth := ACtx.Scope.Depth;
  GContinueScopeDepth := ACtx.Scope.Depth;
  PendingBase := CurrentPendingFinallyBase;
  GBreakFinallyBase := PendingBase;
  GContinueFinallyBase := PendingBase;
end;

procedure EndLoopControl(var AState: TLoopControlState);
begin
  AState.BreakJumps.Free;
  GBreakJumps := AState.OldBreakJumps;
  GBreakFinallyBase := AState.OldBreakFinallyBase;
  GBreakScopeDepth := AState.OldBreakScopeDepth;

  AState.ContinueJumps.Free;
  GContinueJumps := AState.OldContinueJumps;
  GContinueFinallyBase := AState.OldContinueFinallyBase;
  GContinueScopeDepth := AState.OldContinueScopeDepth;
end;

procedure SetLoopContinueScopeDepth(const ACtx: TGocciaCompilationContext);
begin
  GContinueScopeDepth := ACtx.Scope.Depth;
end;

procedure PatchJumpList(const ACtx: TGocciaCompilationContext;
  const AJumps: TList<Integer>);
var
  I: Integer;
begin
  for I := 0 to AJumps.Count - 1 do
    PatchJumpTarget(ACtx, AJumps[I]);
end;

function StatementIsIteration(const AStmt: TGocciaStatement): Boolean;
begin
  Result := (AStmt is TGocciaForStatement) or
    (AStmt is TGocciaForOfStatement) or
    (AStmt is TGocciaForInStatement) or
    (AStmt is TGocciaForAwaitOfStatement) or
    (AStmt is TGocciaWhileStatement) or
    (AStmt is TGocciaDoWhileStatement);
end;

function FindLabelControlIndex(const ALabelName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  if not Assigned(GLabelControls) then
    Exit;

  for I := GLabelControls.Count - 1 downto 0 do
    if GLabelControls[I].LabelName = ALabelName then
      Exit(I);
end;

procedure PatchLabeledContinueJumps(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaStatement);
var
  I, LabelIndex: Integer;
begin
  if not Assigned(GLabelControls) then
    Exit;

  for I := 0 to AStmt.LabelCount - 1 do
  begin
    LabelIndex := FindLabelControlIndex(AStmt.Labels[I]);
    if LabelIndex >= 0 then
      PatchJumpList(ACtx, GLabelControls[LabelIndex].ContinueJumps);
  end;
end;

procedure SetLabeledContinueCleanupBase(const AStmt: TGocciaStatement);
var
  I, LabelIndex: Integer;
  State: TLabelControlState;
begin
  if not Assigned(GLabelControls) then
    Exit;

  for I := 0 to AStmt.LabelCount - 1 do
  begin
    LabelIndex := FindLabelControlIndex(AStmt.Labels[I]);
    if LabelIndex >= 0 then
    begin
      State := GLabelControls[LabelIndex];
      State.ContinueFinallyBase := GContinueFinallyBase;
      State.ContinueScopeDepth := GContinueScopeDepth;
      GLabelControls[LabelIndex] := State;
    end;
  end;
end;

procedure CompileExpressionStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExpressionStatement);
var
  Reg: UInt8;
begin
  Reg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AStmt.Expression, Reg);
  ACtx.Scope.FreeRegister;
end;

procedure CompileDiscardedExpression(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaExpression);
var
  Reg: UInt8;
begin
  Reg := ACtx.Scope.AllocateRegister;
  try
    if AExpr is TGocciaIncrementExpression then
      CompileIncrement(ACtx, TGocciaIncrementExpression(AExpr), Reg, False)
    else
      ACtx.CompileExpression(AExpr, Reg);
  finally
    ACtx.Scope.FreeRegister;
  end;
end;

function IsArrayTypeAnnotation(const AAnnotation: string): Boolean;
var
  Trimmed: string;
  Len, Depth, I: Integer;
begin
  Result := False;
  Trimmed := Trim(AAnnotation);
  if Trimmed = '' then
    Exit;
  Len := Length(Trimmed);
  if (Len >= 2) and (Trimmed[Len - 1] = '[') and (Trimmed[Len] = ']') then
    Exit(True);
  if Pos('Array<', Trimmed) = 1 then
  begin
    Depth := 0;
    for I := 7 to Len do
    begin
      if Trimmed[I] = '<' then
        Inc(Depth)
      else if Trimmed[I] = '>' then
      begin
        if Depth = 0 then
          Exit(I = Len);
        Dec(Depth);
      end;
    end;
  end;
end;

function StripArrayLayer(const AAnnotation: string): string;
var
  Trimmed: string;
  Len, Depth, I: Integer;
begin
  Result := '';
  Trimmed := Trim(AAnnotation);
  if Trimmed = '' then
    Exit;
  Len := Length(Trimmed);
  if (Len >= 2) and (Trimmed[Len - 1] = '[') and (Trimmed[Len] = ']') then
  begin
    Result := Trim(Copy(Trimmed, 1, Len - 2));
    Exit;
  end;
  if Pos('Array<', Trimmed) = 1 then
  begin
    Depth := 0;
    for I := 7 to Len do
    begin
      if Trimmed[I] = '<' then
        Inc(Depth)
      else if Trimmed[I] = '>' then
      begin
        if (Depth = 0) and (I = Len) then
        begin
          Result := Trim(Copy(Trimmed, 7, I - 7));
          Exit;
        end;
        Dec(Depth);
      end;
    end;
  end;
end;

function IsArithmeticOp(const ATokenType: TGocciaTokenType): Boolean;
begin
  Result := ATokenType in [gttPlus, gttMinus, gttStar, gttSlash,
    gttPercent, gttPower];
end;

function IsComparisonOp(const ATokenType: TGocciaTokenType): Boolean;
begin
  Result := ATokenType in [gttLess, gttGreater, gttLessEqual, gttGreaterEqual,
    gttEqual, gttNotEqual, gttLooseEqual, gttLooseNotEqual];
end;

function ExpressionTypeAnnotation(const AScope: TGocciaCompilerScope;
  const AExpr: TGocciaExpression): string;
var
  LocalIdx: Integer;
  ObjAnnotation: string;
  Member: TGocciaMemberExpression;
begin
  Result := '';
  if AExpr is TGocciaIdentifierExpression then
  begin
    LocalIdx := AScope.ResolveLocal(TGocciaIdentifierExpression(AExpr).Name);
    if LocalIdx >= 0 then
      Result := AScope.GetLocal(LocalIdx).TypeAnnotation;
  end
  else if AExpr is TGocciaMemberExpression then
  begin
    Member := TGocciaMemberExpression(AExpr);
    if Member.Computed then
    begin
      ObjAnnotation := ExpressionTypeAnnotation(AScope, Member.ObjectExpr);
      if IsArrayTypeAnnotation(ObjAnnotation) then
        Result := StripArrayLayer(ObjAnnotation);
    end;
  end;
end;

function ExpressionType(const AScope: TGocciaCompilerScope;
  const AExpr: TGocciaExpression): TGocciaLocalType;
var
  Bin: TGocciaBinaryExpression;
  Sequence: TGocciaSequenceExpression;
  LocalIdx: Integer;
  Local: TGocciaCompilerLocal;
  LeftType, RightType: TGocciaLocalType;
  ObjAnnotation: string;
  Member: TGocciaMemberExpression;
begin
  Result := sltUntyped;
  if AExpr is TGocciaLiteralExpression then
  begin
    if TGocciaLiteralExpression(AExpr).Value is TGocciaNumberLiteralValue then
      Result := sltFloat
    else if TGocciaLiteralExpression(AExpr).Value is TGocciaBooleanLiteralValue then
      Result := sltBoolean
    else if TGocciaLiteralExpression(AExpr).Value is TGocciaStringLiteralValue then
      Result := sltString;
  end
  else if AExpr is TGocciaIdentifierExpression then
  begin
    LocalIdx := AScope.ResolveLocal(TGocciaIdentifierExpression(AExpr).Name);
    if LocalIdx >= 0 then
    begin
      Local := AScope.GetLocal(LocalIdx);
      if Local.IsConst or Local.IsStrictlyTyped or not Local.IsCaptured then
        Result := Local.TypeHint;
    end
    else
    begin
      LocalIdx := AScope.ResolveUpvalue(TGocciaIdentifierExpression(AExpr).Name);
      if LocalIdx >= 0 then
        if AScope.GetUpvalue(LocalIdx).IsConst or
           AScope.GetUpvalue(LocalIdx).IsStrictlyTyped then
          Result := AScope.GetUpvalue(LocalIdx).TypeHint;
    end;
  end
  else if AExpr is TGocciaBinaryExpression then
  begin
    Bin := TGocciaBinaryExpression(AExpr);
    if IsComparisonOp(Bin.Operator) then
      Result := sltBoolean
    else if IsArithmeticOp(Bin.Operator) then
    begin
      LeftType := ExpressionType(AScope, Bin.Left);
      RightType := ExpressionType(AScope, Bin.Right);
      if (LeftType = sltInteger) and (RightType = sltInteger) then
        Result := sltInteger
      else if IsKnownNumeric(LeftType) and IsKnownNumeric(RightType) then
        Result := sltFloat;
    end;
  end
  else if AExpr is TGocciaSequenceExpression then
  begin
    Sequence := TGocciaSequenceExpression(AExpr);
    if Sequence.Expressions.Count > 0 then
      Result := ExpressionType(AScope,
        Sequence.Expressions[Sequence.Expressions.Count - 1]);
  end
  else if AExpr is TGocciaCallExpression then
  begin
    if TGocciaCallExpression(AExpr).Callee is TGocciaIdentifierExpression then
    begin
      LocalIdx := AScope.ResolveLocal(
        TGocciaIdentifierExpression(TGocciaCallExpression(AExpr).Callee).Name);
      if LocalIdx >= 0 then
        Result := AScope.GetLocal(LocalIdx).ReturnTypeHint
      else
      begin
        LocalIdx := AScope.ResolveUpvalue(
          TGocciaIdentifierExpression(TGocciaCallExpression(AExpr).Callee).Name);
        if LocalIdx >= 0 then
          Result := AScope.GetUpvalue(LocalIdx).ReturnTypeHint;
      end;
    end;
  end
  else if AExpr is TGocciaConditionalExpression then
  begin
    LeftType := ExpressionType(AScope,
      TGocciaConditionalExpression(AExpr).Consequent);
    RightType := ExpressionType(AScope,
      TGocciaConditionalExpression(AExpr).Alternate);
    if LeftType = RightType then
      Result := LeftType
    else if IsKnownNumeric(LeftType) and IsKnownNumeric(RightType) then
      Result := sltFloat;
  end
  else if AExpr is TGocciaMemberExpression then
  begin
    Member := TGocciaMemberExpression(AExpr);
    if Member.Computed then
    begin
      ObjAnnotation := ExpressionTypeAnnotation(AScope, Member.ObjectExpr);
      if IsArrayTypeAnnotation(ObjAnnotation) then
        Result := TypeAnnotationToLocalType(StripArrayLayer(ObjAnnotation));
    end;
  end
  else if AExpr is TGocciaTemplateLiteralExpression then
    Result := sltString
  else if AExpr is TGocciaTemplateWithInterpolationExpression then
    Result := sltString
  else if AExpr is TGocciaObjectExpression then
    Result := sltReference
  else if AExpr is TGocciaArrayExpression then
    Result := sltReference
  else if AExpr is TGocciaNewExpression then
    Result := sltReference
  else if AExpr is TGocciaArrowFunctionExpression then
    Result := sltReference
  else if AExpr is TGocciaFunctionExpression then
    Result := sltReference;
end;

function InferredExpressionType(const AScope: TGocciaCompilerScope;
  const AExpr: TGocciaExpression): TGocciaLocalType;
begin
  Result := ExpressionType(AScope, AExpr);
  if Result = sltUntyped then
    Result := InferLocalType(AExpr);
end;

function IsUndefinedInitializer(const AExpr: TGocciaExpression): Boolean;
begin
  Result := (AExpr is TGocciaLiteralExpression) and
            (TGocciaLiteralExpression(AExpr).Value is TGocciaUndefinedLiteralValue);
end;

function LocalTypeToChar(const AType: TGocciaLocalType): Char;
begin
  case AType of
    sltInteger:   Result := 'I';
    sltFloat:     Result := 'F';
    sltBoolean:   Result := 'B';
    sltString:    Result := 'S';
    sltReference: Result := 'R';
  else
    Result := 'U';
  end;
end;

function CharToLocalType(const ACh: Char): TGocciaLocalType;
begin
  case ACh of
    'I': Result := sltInteger;
    'F': Result := sltFloat;
    'B': Result := sltBoolean;
    'S': Result := sltString;
    'R': Result := sltReference;
  else
    Result := sltUntyped;
  end;
end;

function BuildParamTypeSignature(
  const AParams: TGocciaParameterArray): string;
var
  I: Integer;
  ParamType: TGocciaLocalType;
begin
  Result := '';
  for I := 0 to High(AParams) do
  begin
    if AParams[I].IsRest then
      Break;
    if AParams[I].IsOptional or Assigned(AParams[I].DefaultValue) then
    begin
      Result := '';
      Exit;
    end;
    ParamType := TypeAnnotationToLocalType(AParams[I].TypeAnnotation);
    if ParamType = sltUntyped then
    begin
      Result := '';
      Exit;
    end;
    Result := Result + LocalTypeToChar(ParamType);
  end;
end;

function FindVarLocalIndex(const AScope: TGocciaCompilerScope;
  const AName: string): Integer; forward;
function FindLocalBySlot(const AScope: TGocciaCompilerScope;
  const AName: string; const ASlot: UInt8): Integer; forward;

procedure CopyLocalTypeMetadata(const ACtx: TGocciaCompilationContext;
  const ASourceIdx, ATargetIdx: Integer); forward;

procedure PrepareLexicalDeclarationLocals(
  const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaVariableDeclaration);
var
  I, LocalIdx: Integer;
  Info: TGocciaVariableInfo;
  Slot: UInt8;
begin
  if AStmt.IsVar then
    Exit;

  for I := 0 to High(AStmt.Variables) do
  begin
    Info := AStmt.Variables[I];
    LocalIdx := ACtx.Scope.ResolveLocal(Info.Name);
    if (LocalIdx >= 0) and
       (ACtx.Scope.GetLocal(LocalIdx).Depth = ACtx.Scope.Depth) and
       (ACtx.Scope.GetLocal(LocalIdx).Slot <>
        ACtx.Scope.DirectEvalSyntheticArgumentsSlot) then
      Slot := ACtx.Scope.GetLocal(LocalIdx).Slot
    else
      Slot := ACtx.Scope.DeclareLocal(Info.Name, AStmt.IsConst);
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_HOLE, Slot, 0, 0));
  end;
end;

procedure EmitGlobalDefine(const ACtx: TGocciaCompilationContext;
  const ASlot: UInt8; const AName: string; const AIsConst: Boolean;
  const AIsVar: Boolean = False; const AHasInitializer: Boolean = True);
var
  NameIdx: UInt16;
  DeclMode: UInt8;
begin
  if AIsVar and (not AHasInitializer) then
    DeclMode := GLOBAL_DEFINE_VAR_DECL
  else if AIsVar then
    DeclMode := GLOBAL_DEFINE_VAR
  else if AIsConst then
    DeclMode := GLOBAL_DEFINE_CONST
  else
    DeclMode := GLOBAL_DEFINE_LET;
  NameIdx := ACtx.Template.AddConstantString(AName);
  if NameIdx > High(UInt8) then
    raise Exception.Create('Constant pool overflow: global name index exceeds 255');
  EmitInstruction(ACtx, EncodeABC(OP_DEFINE_GLOBAL_CONST, ASlot,
    DeclMode, UInt8(NameIdx)));
end;

procedure CompileVariableDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaVariableDeclaration);
var
  I, FuncCount, LocalIdx: Integer;
  Info: TGocciaVariableInfo;
  Slot: UInt8;
  InferredTemplate: TGocciaFunctionTemplate;
  TypeHint, AnnotationType: TGocciaLocalType;
  ConstantValue: TGocciaCompileTimeValue;
  ConstantType: TGocciaLocalType;
  IsStrict, HasInitializer, HasRealInitializer, IsTopLevelGlobalBacked,
  IsVarRedeclaration: Boolean;
  CanTrackConstant: Boolean;
begin
  PrepareLexicalDeclarationLocals(ACtx, AStmt);

  for I := 0 to High(AStmt.Variables) do
  begin
    Info := AStmt.Variables[I];
    if AStmt.IsVar then
    begin
      // Track whether this is a redeclaration (slot already exists)
      LocalIdx := FindVarLocalIndex(ACtx.Scope, Info.Name);
      IsVarRedeclaration := LocalIdx >= 0;
      Slot := ACtx.Scope.DeclareVarLocal(Info.Name);
    end
    else
    begin
      // Check if the local was pre-declared for upvalue resolution and reuse
      // the existing slot, but only at the same scope depth to preserve
      // block-scoping and shadowing.
      LocalIdx := ACtx.Scope.ResolveLocal(Info.Name);
      if (LocalIdx >= 0) and
         (ACtx.Scope.GetLocal(LocalIdx).Depth = ACtx.Scope.Depth) then
        Slot := ACtx.Scope.GetLocal(LocalIdx).Slot
      else
        Slot := ACtx.Scope.DeclareLocal(Info.Name, AStmt.IsConst);
      IsVarRedeclaration := False;
    end;

    IsTopLevelGlobalBacked := ACtx.GlobalBackedTopLevel and
      (AStmt.IsVar or (ACtx.Scope.Depth = 0));
    if IsTopLevelGlobalBacked and AStmt.IsVar then
    begin
      LocalIdx := FindLocalBySlot(ACtx.Scope, Info.Name, Slot);
      if LocalIdx >= 0 then
        ACtx.Scope.MarkGlobalBacked(LocalIdx);
    end;

    HasInitializer := Info.HasInitializer;
    HasRealInitializer := Assigned(Info.Initializer) and
                          not IsUndefinedInitializer(Info.Initializer);

    AnnotationType := TypeAnnotationToLocalType(Info.TypeAnnotation);

    if (AnnotationType <> sltUntyped) and ACtx.StrictTypes then
      TypeHint := AnnotationType
    else if (Info.TypeAnnotation = '') and HasRealInitializer then
      TypeHint := InferredExpressionType(ACtx.Scope, Info.Initializer)
    else
      TypeHint := sltUntyped;

    { Strict-types enforcement is opt-in via --strict-types / config.
      When disabled, type annotations are parsed but not enforced. }
    IsStrict := ACtx.StrictTypes and (TypeHint <> sltUntyped);

    if TypeHint <> sltUntyped then
    begin
      LocalIdx := ACtx.Scope.ResolveLocal(Info.Name);
      if LocalIdx >= 0 then
      begin
        ACtx.Scope.SetLocalTypeHint(LocalIdx, TypeHint);
        ACtx.Template.SetLocalType(Slot, TypeHint);
        if IsStrict then
        begin
          ACtx.Scope.SetLocalStrictlyTyped(LocalIdx, True);
          ACtx.Template.SetLocalStrictFlag(Slot, True);
        end;
      end;
    end;

    if Info.TypeAnnotation <> '' then
    begin
      LocalIdx := ACtx.Scope.ResolveLocal(Info.Name);
      if LocalIdx >= 0 then
      begin
        ACtx.Scope.SetLocalTypeAnnotation(LocalIdx, Info.TypeAnnotation);
        if IsArrayTypeAnnotation(Info.TypeAnnotation) then
        begin
          ACtx.Scope.SetLocalArrayTyped(LocalIdx, True);
          ACtx.Scope.SetLocalElementTypeAnnotation(LocalIdx,
            StripArrayLayer(Info.TypeAnnotation));
        end;
      end;
    end;

    if AStmt.IsConst and (Info.Initializer is TGocciaArrowFunctionExpression) then
    begin
      LocalIdx := ACtx.Scope.ResolveLocal(Info.Name);
      if LocalIdx >= 0 then
      begin
        ACtx.Scope.SetLocalReturnTypeHint(LocalIdx,
          TypeAnnotationToLocalType(
            TGocciaArrowFunctionExpression(Info.Initializer).ReturnType));
        ACtx.Scope.SetLocalParamTypeSignature(LocalIdx,
          BuildParamTypeSignature(
            TGocciaArrowFunctionExpression(Info.Initializer).Parameters));
      end;
    end;

    if Assigned(Info.Initializer) and
       not (AStmt.IsVar and (not HasInitializer) and IsVarRedeclaration) then
    begin
      FuncCount := ACtx.Template.FunctionCount;

      if (Info.Initializer is TGocciaClassExpression) and
         (TGocciaClassExpression(Info.Initializer).ClassDefinition.Name = '') then
        CompileClassExpression(ACtx,
          TGocciaClassExpression(Info.Initializer).ClassDefinition, Slot, Info.Name)
      else
        ACtx.CompileExpression(Info.Initializer, Slot);

      if IsStrict and HasRealInitializer then
        if not TypesAreCompatible(InferLocalType(Info.Initializer), TypeHint) then
          EmitInstruction(ACtx, EncodeABC(OP_CHECK_TYPE, Slot,
            UInt8(Ord(TypeHint)), 0));

      if (Info.Initializer is TGocciaArrowFunctionExpression) or
         (Info.Initializer is TGocciaFunctionExpression) then
      begin
        if ACtx.Template.FunctionCount > FuncCount then
        begin
          InferredTemplate := ACtx.Template.GetFunction(
            ACtx.Template.FunctionCount - 1);
          if (InferredTemplate.Name = '<arrow>') or
             (InferredTemplate.Name = '<function>') or
             (InferredTemplate.Name = '<method>') then
            InferredTemplate.Name := Info.Name;
        end;
      end;
    end
    else if not (AStmt.IsVar and IsVarRedeclaration) then
      // Only emit OP_LOAD_UNDEFINED if not a var redeclaration (preserve prior value)
      EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, Slot, 0, 0));

    // When a local was pre-declared for upvalue resolution and subsequently
    // captured by a hoisted function closure, the closure's OP_CLOSURE created
    // a cell that copied the register's initial (undefined) value.  The
    // initializer wrote directly to the register (e.g. OP_LOAD_INT) bypassing
    // the cell.  Emit OP_SET_LOCAL to sync the cell with the register.
    if (not AStmt.IsVar) or HasInitializer then
    begin
      LocalIdx := ACtx.Scope.ResolveLocal(Info.Name);
      if (LocalIdx >= 0) and ACtx.Scope.GetLocal(LocalIdx).IsCaptured then
        EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, Slot, UInt16(Slot)));
    end;

    LocalIdx := ACtx.Scope.ResolveLocal(Info.Name);
    if LocalIdx >= 0 then
    begin
      ACtx.Scope.ClearLocalConstantValue(LocalIdx);
      CanTrackConstant := ACtx.OptimizationOptions.EnableConstPropagation and
        AStmt.IsConst and not AStmt.IsVar and
        not IsTopLevelGlobalBacked and HasRealInitializer and Assigned(Info.Initializer) and
        TryEvaluateConstantExpression(ACtx, Info.Initializer, ConstantValue);

      if CanTrackConstant and IsStrict then
      begin
        ConstantType := CompileTimeValueToLocalType(ConstantValue);
        CanTrackConstant := TypesAreCompatible(ConstantType, TypeHint);
      end;

      if CanTrackConstant then
        ACtx.Scope.SetLocalConstantValue(LocalIdx, ConstantValue);
    end;

    if IsTopLevelGlobalBacked then
    begin
      EmitGlobalDefine(ACtx, Slot, Info.Name, AStmt.IsConst, AStmt.IsVar,
        HasInitializer);
      LocalIdx := FindLocalBySlot(ACtx.Scope, Info.Name, Slot);
      if LocalIdx >= 0 then
        ACtx.Scope.MarkGlobalBacked(LocalIdx);
    end;
  end;
end;

procedure CompileFunctionDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaFunctionDeclaration);
var
  Slot: UInt8;
  LocalIdx, FuncCount: Integer;
  InferredTemplate: TGocciaFunctionTemplate;
  IsBlockScoped, IsTopLevelGlobalBacked, IsPreinitializedTopLevel,
  UsePreinitializedBinding: Boolean;
  NameIdx: UInt16;
begin
  IsBlockScoped := ACtx.Scope.Depth > 0;
  if IsBlockScoped then
  begin
    LocalIdx := ACtx.Scope.ResolveLocal(AStmt.Name);
    if (LocalIdx >= 0) and
       (ACtx.Scope.GetLocal(LocalIdx).Depth = ACtx.Scope.Depth) then
      Slot := ACtx.Scope.GetLocal(LocalIdx).Slot
    else
      Slot := ACtx.Scope.DeclareLocal(AStmt.Name, False);
  end
  else
  begin
    Slot := ACtx.Scope.DeclareVarLocal(AStmt.Name);
  end;

  IsTopLevelGlobalBacked := ACtx.GlobalBackedTopLevel and not IsBlockScoped;
  if IsTopLevelGlobalBacked then
  begin
    LocalIdx := FindLocalBySlot(ACtx.Scope, AStmt.Name, Slot);
    if LocalIdx >= 0 then
      ACtx.Scope.MarkGlobalBacked(LocalIdx);
  end;

  UsePreinitializedBinding := False;
  IsPreinitializedTopLevel := ACtx.PreinitializedTopLevelFunctions and
    not IsBlockScoped;

  if IsPreinitializedTopLevel and IsTopLevelGlobalBacked then
  begin
    LocalIdx := FindLocalBySlot(ACtx.Scope, AStmt.Name, Slot);
    UsePreinitializedBinding := (LocalIdx >= 0) and
      (ACtx.Scope.GetLocal(LocalIdx).ExportNameCount > 0);
  end;

  if UsePreinitializedBinding then
  begin
    NameIdx := ACtx.Template.AddConstantString(AStmt.Name);
    EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, Slot, NameIdx));
  end
  else
  begin
    FuncCount := ACtx.Template.FunctionCount;
    Goccia.Compiler.Expressions.CompileFunctionExpression(
      ACtx, AStmt.FunctionExpression, Slot);
    if ACtx.Template.FunctionCount > FuncCount then
    begin
      InferredTemplate := ACtx.Template.GetFunction(
        ACtx.Template.FunctionCount - 1);
      if InferredTemplate.Name = '<function>' then
        InferredTemplate.Name := AStmt.Name;
    end;
    if IsPreinitializedTopLevel and IsTopLevelGlobalBacked then
    begin
      NameIdx := ACtx.Template.AddConstantString(AStmt.Name);
      EmitInstruction(ACtx, EncodeABx(OP_SET_GLOBAL, Slot, NameIdx));
    end;
  end;

  LocalIdx := ACtx.Scope.ResolveLocal(AStmt.Name);
  if (LocalIdx >= 0) and ACtx.Scope.GetLocal(LocalIdx).IsCaptured then
    EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, Slot, UInt16(Slot)));

  if IsTopLevelGlobalBacked then
    EmitGlobalDefine(ACtx, Slot, AStmt.Name, False, True, True);
end;

procedure CompileAnnexBBlockFunctionActivation(
  const ACtx: TGocciaCompilationContext; const AStmt: TGocciaFunctionDeclaration);
var
  LocalIdx, VarLocalIdx: Integer;
  Slot: UInt8;
begin
  if (not ACtx.NonStrictMode) or (ACtx.Scope.Depth = 0) then
    Exit;
  if AStmt.FunctionExpression.IsAsync or AStmt.FunctionExpression.IsGenerator then
    Exit;

  LocalIdx := ACtx.Scope.ResolveLocal(AStmt.Name);
  if (LocalIdx < 0) or
     (ACtx.Scope.GetLocal(LocalIdx).Depth <> ACtx.Scope.Depth) then
    Exit;

  VarLocalIdx := FindVarLocalIndex(ACtx.Scope, AStmt.Name);
  if VarLocalIdx < 0 then
    Exit;

  Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
  if ACtx.Scope.GetLocal(VarLocalIdx).IsGlobalBacked then
    EmitGlobalDefine(ACtx, Slot, AStmt.Name, False, True, True)
  else if ACtx.Scope.GetLocal(VarLocalIdx).Slot <> Slot then
    EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, Slot,
      UInt16(ACtx.Scope.GetLocal(VarLocalIdx).Slot)));
end;

threadvar
  // Module-level stack of using resource entries for the current block.
  // Populated by CompileUsingDeclaration, consumed by CompileBlockStatement.
  GUsingResources: TList<TUsingResourceEntry>;
  GPreallocatedUsingDisposeSlots: TList<TPreallocatedUsingDisposeSlot>;

procedure RegisterUsingResourceForDisposal(const AEntry: TUsingResourceEntry);
var
  Len, PendingIdx: Integer;
  PendingEntry: TPendingFinallyEntry;
begin
  if not Assigned(GUsingResources) then
    GUsingResources := TList<TUsingResourceEntry>.Create;
  GUsingResources.Add(AEntry);

  if Assigned(GPendingFinally) and (GPendingFinally.Count > 0) then
  begin
    PendingIdx := GPendingFinally.Count - 1;
    PendingEntry := GPendingFinally[PendingIdx];
    if not Assigned(PendingEntry.FinallyBlock) then
    begin
      Len := Length(PendingEntry.UsingResources);
      SetLength(PendingEntry.UsingResources, Len + 1);
      PendingEntry.UsingResources[Len] := AEntry;
      GPendingFinally[PendingIdx] := PendingEntry;
    end;
  end;
end;

function TryUsePreallocatedUsingDisposeSlot(
  const ADeclaration: TGocciaUsingDeclaration; const AVariableIndex: Integer;
  out ADisposeSlot: UInt8; out AResourceRegistered: Boolean): Boolean;
var
  I: Integer;
  Preallocated: TPreallocatedUsingDisposeSlot;
begin
  Result := False;
  ADisposeSlot := 0;
  AResourceRegistered := False;
  if not Assigned(GPreallocatedUsingDisposeSlots) then
    Exit;

  for I := GPreallocatedUsingDisposeSlots.Count - 1 downto 0 do
  begin
    Preallocated := GPreallocatedUsingDisposeSlots[I];
    if (Preallocated.Declaration = ADeclaration) and
       (Preallocated.VariableIndex = AVariableIndex) then
    begin
      ADisposeSlot := Preallocated.DisposeSlot;
      AResourceRegistered := Preallocated.ResourceRegistered;
      GPreallocatedUsingDisposeSlots.Delete(I);
      Exit(True);
    end;
  end;
end;

// TC39 Explicit Resource Management: compile using / await using declaration.
// Emits OP_USING_INIT for each variable to validate and extract the dispose method.
// Also updates the topmost GPendingFinally entry so that CompileReturnStatement
// and CompileBreakStatement can emit the correct disposal sequence.
procedure CompileUsingDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaUsingDeclaration);
var
  I, FuncCount, LocalIdx: Integer;
  Info: TGocciaVariableInfo;
  ValueSlot, DisposeSlot: UInt8;
  Flags: UInt8;
  Entry: TUsingResourceEntry;
  InferredTemplate: TGocciaFunctionTemplate;
  ResourceRegistered: Boolean;
begin
  for I := 0 to High(AStmt.Variables) do
  begin
    Info := AStmt.Variables[I];

    // Reuse a block predeclaration when one was created for hoisted function
    // captures; otherwise declare the using variable at the current depth.
    LocalIdx := ACtx.Scope.ResolveLocal(Info.Name);
    if (LocalIdx >= 0) and
       (ACtx.Scope.GetLocal(LocalIdx).Depth = ACtx.Scope.Depth) then
      ValueSlot := ACtx.Scope.GetLocal(LocalIdx).Slot
    else
    begin
      ValueSlot := ACtx.Scope.DeclareLocal(Info.Name, True);
      LocalIdx := ACtx.Scope.ResolveLocal(Info.Name);
    end;

    // Allocate and clear the hidden dispose-method register before the
    // initializer runs. If a later initializer throws, the unwind path skips
    // this resource unless OP_USING_INIT actually stored a callable disposer.
    if not TryUsePreallocatedUsingDisposeSlot(AStmt, I, DisposeSlot,
      ResourceRegistered) then
    begin
      DisposeSlot := ACtx.Scope.AllocateRegister;
      ResourceRegistered := False;
      EmitInstruction(ACtx, EncodeABC(OP_LOAD_NULL, DisposeSlot, 0, 0));
    end;

    // Compile the initializer into the value slot, applying NamedEvaluation
    // for anonymous function/class definitions bound by `using`.
    FuncCount := ACtx.Template.FunctionCount;
    if (Info.Initializer is TGocciaClassExpression) and
       (TGocciaClassExpression(Info.Initializer).ClassDefinition.Name = '') then
      CompileClassExpression(ACtx,
        TGocciaClassExpression(Info.Initializer).ClassDefinition, ValueSlot,
        Info.Name)
    else
      ACtx.CompileExpression(Info.Initializer, ValueSlot);

    if (Info.Initializer is TGocciaArrowFunctionExpression) or
       (Info.Initializer is TGocciaFunctionExpression) then
    begin
      if ACtx.Template.FunctionCount > FuncCount then
      begin
        InferredTemplate := ACtx.Template.GetFunction(
          ACtx.Template.FunctionCount - 1);
        if (InferredTemplate.Name = '<arrow>') or
           (InferredTemplate.Name = '<function>') or
           (InferredTemplate.Name = '<method>') then
          InferredTemplate.Name := Info.Name;
      end;
    end;

    // OP_USING_INIT: A=disposeMethodDest, B=valueSource, C=flags (0=sync, 1=async)
    if AStmt.IsAwait then
      Flags := 1
    else
      Flags := 0;
    EmitInstruction(ACtx, EncodeABC(OP_USING_INIT, DisposeSlot, ValueSlot, Flags));
    if (LocalIdx >= 0) and
       (ACtx.Scope.GetLocal(LocalIdx).Slot = ValueSlot) and
       ACtx.Scope.GetLocal(LocalIdx).IsCaptured then
      EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, ValueSlot, UInt16(ValueSlot)));

    // Track for disposal at block exit
    Entry.ValueSlot := ValueSlot;
    Entry.DisposeSlot := DisposeSlot;
    Entry.IsAwait := AStmt.IsAwait;
    if not ResourceRegistered then
      RegisterUsingResourceForDisposal(Entry);
  end;
end;

// Emit disposal sequence for using resources in reverse order.
// Each OP_USING_DISPOSE: A=errorAccum, B=disposeMethod, C=resource
// OP_USING_DISPOSE writes the call result back into B, so for await using
// we emit OP_AWAIT on DisposeSlot to await the actual dispose Promise.
procedure EmitDisposalSequence(const ACtx: TGocciaCompilationContext;
  const AResources: array of TUsingResourceEntry;
  const AResourceCount: Integer; const AErrorReg: UInt8);
var
  I: Integer;
begin
  for I := AResourceCount - 1 downto 0 do
  begin
    EmitInstruction(ACtx, EncodeABC(OP_USING_DISPOSE,
      AErrorReg, AResources[I].DisposeSlot, AResources[I].ValueSlot));
    // Await the actual dispose result for async resources
    if AResources[I].IsAwait then
      EmitInstruction(ACtx, EncodeABC(OP_AWAIT,
        AResources[I].DisposeSlot, AResources[I].DisposeSlot, 0));
  end;
end;

function GetBlockFunctionDeclaration(const ANode: TGocciaASTNode): TGocciaFunctionDeclaration;
begin
  if ANode is TGocciaFunctionDeclaration then
    Result := TGocciaFunctionDeclaration(ANode)
  else if ANode is TGocciaExportFunctionDeclaration then
    Result := TGocciaExportFunctionDeclaration(ANode).Declaration
  else
    Result := nil;
end;

function FindVarLocalIndex(const AScope: TGocciaCompilerScope;
  const AName: string): Integer;
var
  I: Integer;
  Local: TGocciaCompilerLocal;
begin
  for I := 0 to AScope.LocalCount - 1 do
  begin
    Local := AScope.GetLocal(I);
    if (Local.Name = AName) and (Local.Depth = 0) then
      Exit(I);
  end;
  Result := -1;
end;

function FindLocalBySlot(const AScope: TGocciaCompilerScope;
  const AName: string; const ASlot: UInt8): Integer;
var
  I: Integer;
  Local: TGocciaCompilerLocal;
begin
  for I := 0 to AScope.LocalCount - 1 do
  begin
    Local := AScope.GetLocal(I);
    if (Local.Name = AName) and (Local.Slot = ASlot) then
      Exit(I);
  end;
  Result := -1;
end;

procedure CopyLocalTypeMetadata(const ACtx: TGocciaCompilationContext;
  const ASourceIdx, ATargetIdx: Integer);
var
  Source: TGocciaCompilerLocal;
  Target: TGocciaCompilerLocal;
begin
  if (ASourceIdx < 0) or (ATargetIdx < 0) then
    Exit;

  Source := ACtx.Scope.GetLocal(ASourceIdx);
  Target := ACtx.Scope.GetLocal(ATargetIdx);

  ACtx.Scope.SetLocalTypeHint(ATargetIdx, Source.TypeHint);
  ACtx.Scope.SetLocalStrictlyTyped(ATargetIdx, Source.IsStrictlyTyped);
  ACtx.Scope.SetLocalArrayTyped(ATargetIdx, Source.IsArrayTyped);
  ACtx.Scope.SetLocalReturnTypeHint(ATargetIdx, Source.ReturnTypeHint);
  ACtx.Scope.SetLocalParamTypeSignature(ATargetIdx,
    Source.ParamTypeSignature);
  ACtx.Scope.SetLocalTypeAnnotation(ATargetIdx, Source.TypeAnnotation);
  ACtx.Scope.SetLocalElementTypeAnnotation(ATargetIdx,
    Source.ElementTypeAnnotation);

  if Source.TypeHint <> sltUntyped then
    ACtx.Template.SetLocalType(Target.Slot, Source.TypeHint);
  if Source.IsStrictlyTyped then
    ACtx.Template.SetLocalStrictFlag(Target.Slot, True);
end;

procedure PredeclareBlockFunctionLocal(const AFunctionDecl: TGocciaFunctionDeclaration;
  const AScope: TGocciaCompilerScope);
var
  LocalIdx: Integer;
begin
  LocalIdx := AScope.ResolveLocal(AFunctionDecl.Name);
  if (LocalIdx < 0) or (AScope.GetLocal(LocalIdx).Depth <> AScope.Depth) or
     (AScope.GetLocal(LocalIdx).Slot =
      AScope.DirectEvalSyntheticArgumentsSlot) then
    AScope.DeclareLocal(AFunctionDecl.Name, False);
end;

procedure PredeclareBlockVariableLocals(const ACtx: TGocciaCompilationContext;
  const AVarDecl: TGocciaVariableDeclaration;
  const AEmitInitializers: Boolean = True);
var
  I, LocalIdx: Integer;
  Slot: UInt8;
begin
  for I := 0 to High(AVarDecl.Variables) do
  begin
    LocalIdx := ACtx.Scope.ResolveLocal(AVarDecl.Variables[I].Name);
    if (LocalIdx < 0) or
       (ACtx.Scope.GetLocal(LocalIdx).Depth <> ACtx.Scope.Depth) or
       (ACtx.Scope.GetLocal(LocalIdx).Slot =
        ACtx.Scope.DirectEvalSyntheticArgumentsSlot) then
    begin
      Slot := ACtx.Scope.DeclareLocal(AVarDecl.Variables[I].Name,
        AVarDecl.IsConst);
      if AEmitInitializers then
        EmitInstruction(ACtx, EncodeABC(OP_LOAD_HOLE, Slot, 0, 0));
    end;
  end;
end;

procedure PredeclareBlockPatternLocals(const ACtx: TGocciaCompilationContext;
  const APattern: TGocciaDestructuringPattern; const AIsConst: Boolean;
  const AEmitInitializers: Boolean = True);
var
  I, LocalIdx: Integer;
  Names: TStringList;
  Slot: UInt8;
begin
  Names := TStringList.Create;
  Names.CaseSensitive := True;
  try
    CollectPatternBindingNames(APattern, Names, True);
    for I := 0 to Names.Count - 1 do
    begin
      LocalIdx := ACtx.Scope.ResolveLocal(Names[I]);
      if (LocalIdx < 0) or
         (ACtx.Scope.GetLocal(LocalIdx).Depth <> ACtx.Scope.Depth) or
         (ACtx.Scope.GetLocal(LocalIdx).Slot =
          ACtx.Scope.DirectEvalSyntheticArgumentsSlot) then
      begin
        Slot := ACtx.Scope.DeclareLocal(Names[I], AIsConst);
        if AEmitInitializers then
          EmitInstruction(ACtx, EncodeABC(OP_LOAD_HOLE, Slot, 0, 0));
      end;
    end;
  finally
    Names.Free;
  end;
end;

procedure PredeclareBlockNamedLexicalLocal(
  const ACtx: TGocciaCompilationContext; const AName: string;
  const AIsConst: Boolean; const AEmitInitializer: Boolean = True);
var
  LocalIdx: Integer;
  Slot: UInt8;
begin
  LocalIdx := ACtx.Scope.ResolveLocal(AName);
  if (LocalIdx < 0) or
     (ACtx.Scope.GetLocal(LocalIdx).Depth <> ACtx.Scope.Depth) or
     (ACtx.Scope.GetLocal(LocalIdx).Slot =
      ACtx.Scope.DirectEvalSyntheticArgumentsSlot) then
  begin
    Slot := ACtx.Scope.DeclareLocal(AName, AIsConst);
    if AEmitInitializer then
      EmitInstruction(ACtx, EncodeABC(OP_LOAD_HOLE, Slot, 0, 0));
  end;
end;

procedure PredeclareBlockUsingLocals(const ACtx: TGocciaCompilationContext;
  const AUsingDecl: TGocciaUsingDeclaration;
  const AEmitInitializers: Boolean = True);
var
  I: Integer;
begin
  for I := 0 to High(AUsingDecl.Variables) do
    PredeclareBlockNamedLexicalLocal(ACtx, AUsingDecl.Variables[I].Name, True,
      AEmitInitializers);
end;

procedure PredeclareBlockLexicalLocals(const ANode: TGocciaASTNode;
  const ACtx: TGocciaCompilationContext;
  const AEmitInitializers: Boolean = True);
var
  VarDecl: TGocciaVariableDeclaration;
begin
  if ANode is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(ANode);
    if not VarDecl.IsVar then
      PredeclareBlockVariableLocals(ACtx, VarDecl, AEmitInitializers);
  end
  else if ANode is TGocciaExportVariableDeclaration then
  begin
    VarDecl := TGocciaExportVariableDeclaration(ANode).Declaration;
    if not VarDecl.IsVar then
      PredeclareBlockVariableLocals(ACtx, VarDecl, AEmitInitializers);
  end
  else if (ANode is TGocciaExportDestructuringDeclaration) and
          not TGocciaExportDestructuringDeclaration(ANode).Declaration.IsVar then
    PredeclareBlockPatternLocals(ACtx,
      TGocciaExportDestructuringDeclaration(ANode).Declaration.Pattern,
      TGocciaExportDestructuringDeclaration(ANode).Declaration.IsConst,
      AEmitInitializers)
  else if ANode is TGocciaFunctionDeclaration then
    PredeclareBlockFunctionLocal(TGocciaFunctionDeclaration(ANode), ACtx.Scope)
  else if ANode is TGocciaExportFunctionDeclaration then
    PredeclareBlockFunctionLocal(
      TGocciaExportFunctionDeclaration(ANode).Declaration, ACtx.Scope)
  else if ANode is TGocciaExportClassDeclaration then
    PredeclareBlockNamedLexicalLocal(ACtx,
      TGocciaExportClassDeclaration(ANode).Declaration.ClassDefinition.Name,
      False, AEmitInitializers)
  else if (ANode is TGocciaDestructuringDeclaration) and
          not TGocciaDestructuringDeclaration(ANode).IsVar then
    PredeclareBlockPatternLocals(ACtx,
      TGocciaDestructuringDeclaration(ANode).Pattern,
      TGocciaDestructuringDeclaration(ANode).IsConst, AEmitInitializers)
  else if ANode is TGocciaUsingDeclaration then
    PredeclareBlockUsingLocals(ACtx, TGocciaUsingDeclaration(ANode),
      AEmitInitializers)
  else if ANode is TGocciaClassDeclaration then
    PredeclareBlockNamedLexicalLocal(ACtx,
      TGocciaClassDeclaration(ANode).ClassDefinition.Name, False,
      AEmitInitializers)
  else if ANode is TGocciaEnumDeclaration then
    PredeclareBlockNamedLexicalLocal(ACtx,
      TGocciaEnumDeclaration(ANode).Name, False, AEmitInitializers)
  else if ANode is TGocciaExportEnumDeclaration then
    PredeclareBlockNamedLexicalLocal(ACtx,
      TGocciaExportEnumDeclaration(ANode).Declaration.Name, False,
      AEmitInitializers);
end;

procedure EmitBlockLexicalHoleInitializer(const ACtx: TGocciaCompilationContext;
  const AName: string);
var
  LocalIdx: Integer;
  Local: TGocciaCompilerLocal;
begin
  LocalIdx := ACtx.Scope.ResolveLocal(AName);
  if LocalIdx < 0 then
    Exit;
  Local := ACtx.Scope.GetLocal(LocalIdx);
  if Local.Depth = ACtx.Scope.Depth then
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_HOLE, Local.Slot, 0, 0));
end;

procedure EmitBlockPatternHoleInitializers(const ACtx: TGocciaCompilationContext;
  const APattern: TGocciaDestructuringPattern);
var
  I: Integer;
  Names: TStringList;
begin
  Names := TStringList.Create;
  Names.CaseSensitive := True;
  try
    CollectPatternBindingNames(APattern, Names, True);
    for I := 0 to Names.Count - 1 do
      EmitBlockLexicalHoleInitializer(ACtx, Names[I]);
  finally
    Names.Free;
  end;
end;

procedure EmitBlockLexicalHoleInitializers(const ANode: TGocciaASTNode;
  const ACtx: TGocciaCompilationContext);
var
  I: Integer;
  VarDecl: TGocciaVariableDeclaration;
  UsingDecl: TGocciaUsingDeclaration;
begin
  if ANode is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(ANode);
    if VarDecl.IsVar then
      Exit;
    for I := 0 to High(VarDecl.Variables) do
      EmitBlockLexicalHoleInitializer(ACtx, VarDecl.Variables[I].Name);
  end
  else if ANode is TGocciaExportVariableDeclaration then
  begin
    VarDecl := TGocciaExportVariableDeclaration(ANode).Declaration;
    if VarDecl.IsVar then
      Exit;
    for I := 0 to High(VarDecl.Variables) do
      EmitBlockLexicalHoleInitializer(ACtx, VarDecl.Variables[I].Name);
  end
  else if (ANode is TGocciaDestructuringDeclaration) and
          not TGocciaDestructuringDeclaration(ANode).IsVar then
    EmitBlockPatternHoleInitializers(ACtx,
      TGocciaDestructuringDeclaration(ANode).Pattern)
  else if (ANode is TGocciaExportDestructuringDeclaration) and
          not TGocciaExportDestructuringDeclaration(ANode).Declaration.IsVar then
    EmitBlockPatternHoleInitializers(ACtx,
      TGocciaExportDestructuringDeclaration(ANode).Declaration.Pattern)
  else if ANode is TGocciaUsingDeclaration then
  begin
    UsingDecl := TGocciaUsingDeclaration(ANode);
    for I := 0 to High(UsingDecl.Variables) do
      EmitBlockLexicalHoleInitializer(ACtx, UsingDecl.Variables[I].Name);
  end
  else if ANode is TGocciaClassDeclaration then
    EmitBlockLexicalHoleInitializer(ACtx,
      TGocciaClassDeclaration(ANode).ClassDefinition.Name)
  else if ANode is TGocciaExportClassDeclaration then
    EmitBlockLexicalHoleInitializer(ACtx,
      TGocciaExportClassDeclaration(ANode).Declaration.ClassDefinition.Name)
  else if ANode is TGocciaEnumDeclaration then
    EmitBlockLexicalHoleInitializer(ACtx, TGocciaEnumDeclaration(ANode).Name)
  else if ANode is TGocciaExportEnumDeclaration then
    EmitBlockLexicalHoleInitializer(ACtx,
      TGocciaExportEnumDeclaration(ANode).Declaration.Name);
end;

function NeedsBlockLexicalPredeclaration(const ANode: TGocciaASTNode): Boolean;
var
  VarDecl: TGocciaVariableDeclaration;
begin
  if ANode is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(ANode);
    Exit(not VarDecl.IsVar);
  end
  else if ANode is TGocciaExportVariableDeclaration then
  begin
    VarDecl := TGocciaExportVariableDeclaration(ANode).Declaration;
    Exit(not VarDecl.IsVar);
  end
  else if (ANode is TGocciaFunctionDeclaration) or
          (ANode is TGocciaExportFunctionDeclaration) then
    Exit(True)
  else if ANode is TGocciaDestructuringDeclaration then
    Exit(not TGocciaDestructuringDeclaration(ANode).IsVar)
  else if ANode is TGocciaExportDestructuringDeclaration then
    Exit(not TGocciaExportDestructuringDeclaration(ANode).Declaration.IsVar)
  else if (ANode is TGocciaUsingDeclaration) or
          (ANode is TGocciaClassDeclaration) or
          (ANode is TGocciaExportClassDeclaration) or
          (ANode is TGocciaEnumDeclaration) or
          (ANode is TGocciaExportEnumDeclaration) then
    Exit(True);

  Result := False;
end;

function StatementAlwaysAbrupt(const AStmt: TGocciaStatement): Boolean;
var
  IfStmt: TGocciaIfStatement;
  BlockStmt: TGocciaBlockStatement;
  I: Integer;
begin
  if (AStmt is TGocciaReturnStatement) or
     (AStmt is TGocciaThrowStatement) or
     (AStmt is TGocciaBreakStatement) or
     (AStmt is TGocciaContinueStatement) then
    Exit(True);

  if AStmt is TGocciaIfStatement then
  begin
    IfStmt := TGocciaIfStatement(AStmt);
    Exit(Assigned(IfStmt.Alternate) and
      StatementAlwaysAbrupt(IfStmt.Consequent) and
      StatementAlwaysAbrupt(IfStmt.Alternate));
  end;

  if AStmt is TGocciaWithStatement then
    Exit(StatementAlwaysAbrupt(TGocciaWithStatement(AStmt).Body));

  if AStmt is TGocciaBlockStatement then
  begin
    BlockStmt := TGocciaBlockStatement(AStmt);
    for I := 0 to BlockStmt.Nodes.Count - 1 do
      if (BlockStmt.Nodes[I] is TGocciaStatement) and
         StatementAlwaysAbrupt(TGocciaStatement(BlockStmt.Nodes[I])) then
        Exit(True);
  end;

  Result := False;
end;

function StatementAlwaysAbrupt(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaStatement): Boolean;
var
  ConditionValue: TGocciaCompileTimeValue;
  IfStmt: TGocciaIfStatement;
  BlockStmt: TGocciaBlockStatement;
  I: Integer;
begin
  if AStmt is TGocciaIfStatement then
  begin
    IfStmt := TGocciaIfStatement(AStmt);
    if ACtx.OptimizationOptions.EnableDeadBranchElimination and
       not ACtx.OptimizationOptions.PreserveCoverageShape and
       TryEvaluateConstantExpression(ACtx, IfStmt.Condition, ConditionValue) then
    begin
      if CompileTimeValueToBoolean(ConditionValue) then
        Exit(StatementAlwaysAbrupt(ACtx, IfStmt.Consequent));
      if Assigned(IfStmt.Alternate) then
        Exit(StatementAlwaysAbrupt(ACtx, IfStmt.Alternate));
      Exit(False);
    end;
  end;

  if AStmt is TGocciaBlockStatement then
  begin
    BlockStmt := TGocciaBlockStatement(AStmt);
    for I := 0 to BlockStmt.Nodes.Count - 1 do
      if (BlockStmt.Nodes[I] is TGocciaStatement) and
         StatementAlwaysAbrupt(ACtx, TGocciaStatement(BlockStmt.Nodes[I])) then
        Exit(True);
    Exit(False);
  end;

  if AStmt is TGocciaWithStatement then
    Exit(StatementAlwaysAbrupt(ACtx, TGocciaWithStatement(AStmt).Body));

  Result := StatementAlwaysAbrupt(AStmt);
end;

function StatementNeedsIteratorClose(const AStmt: TGocciaStatement): Boolean;
var
  BlockStmt: TGocciaBlockStatement;
  TryStmt: TGocciaTryStatement;
  I: Integer;
begin
  if not Assigned(AStmt) then
    Exit(False);

  if (AStmt is TGocciaBreakStatement) or
     (AStmt is TGocciaReturnStatement) or
     (AStmt is TGocciaThrowStatement) then
    Exit(True);

  if AStmt is TGocciaContinueStatement then
    Exit(TGocciaContinueStatement(AStmt).TargetLabel <> '');

  if (AStmt is TGocciaEmptyStatement) or
     (AStmt is TGocciaFunctionDeclaration) or
     (AStmt is TGocciaExportFunctionDeclaration) then
    Exit(False);

  if (AStmt is TGocciaExpressionStatement) or
     (AStmt is TGocciaVariableDeclaration) or
     (AStmt is TGocciaDestructuringDeclaration) or
     (AStmt is TGocciaUsingDeclaration) then
    Exit(True);

  if AStmt is TGocciaBlockStatement then
  begin
    BlockStmt := TGocciaBlockStatement(AStmt);
    for I := 0 to BlockStmt.Nodes.Count - 1 do
    begin
      if not (BlockStmt.Nodes[I] is TGocciaStatement) then
        Exit(True);
      if StatementNeedsIteratorClose(TGocciaStatement(BlockStmt.Nodes[I])) then
        Exit(True);
    end;
    Exit(False);
  end;

  if AStmt is TGocciaIfStatement then
    Exit(True);

  if AStmt is TGocciaWithStatement then
    Exit(True);

  if AStmt is TGocciaTryStatement then
  begin
    TryStmt := TGocciaTryStatement(AStmt);
    Exit(StatementNeedsIteratorClose(TryStmt.Block) or
      StatementNeedsIteratorClose(TryStmt.CatchBlock) or
      StatementNeedsIteratorClose(TryStmt.FinallyBlock));
  end;

  if AStmt is TGocciaSwitchStatement then
    Exit(True);

  if AStmt is TGocciaForOfStatement then
    Exit(True);

  if AStmt is TGocciaForInStatement then
    Exit(True);

  if AStmt is TGocciaForStatement then
    Exit(True);

  if AStmt is TGocciaWhileStatement then
    Exit(True);

  if AStmt is TGocciaDoWhileStatement then
    Exit(True);

  Result := True;
end;

procedure EmitPendingEntryCleanup(const ACtx: TGocciaCompilationContext;
  const AEntry: TPendingFinallyEntry; const ACloseIterator: Boolean);
var
  NullishJump: Integer;
begin
  EmitInstruction(ACtx, EncodeABC(OP_POP_HANDLER, 0, 0, 0));
  if Assigned(AEntry.FinallyBlock) then
    CompileBlockStatement(ACtx, AEntry.FinallyBlock)
  else if Length(AEntry.UsingResources) > 0 then
  begin
    EmitDisposalSequence(ACtx, AEntry.UsingResources,
      Length(AEntry.UsingResources), AEntry.UsingErrorReg);
    NullishJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH,
      AEntry.UsingErrorReg, GOCCIA_NULLISH_MATCH_HOLE);
    EmitInstruction(ACtx, EncodeABC(OP_THROW, AEntry.UsingErrorReg, 0, 0));
    PatchJumpTarget(ACtx, NullishJump);
  end
  else if AEntry.IsIteratorClose and ACloseIterator then
    EmitInstruction(ACtx, EncodeABC(OP_ITER_CLOSE, AEntry.IteratorReg,
      Ord(AEntry.IsAsyncIterator), 0));
end;

function CompileBlockStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaBlockStatement): Boolean;
var
  I: Integer;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount: Integer;
  Node: TGocciaASTNode;
  Reg: UInt8;
  HasUsing: Boolean;
  SavedResourceBase, ResourceCount: Integer;
  CatchReg, ErrorReg: UInt8;
  HandlerJump, EndJump, NullishJump: Integer;
  SavedResources: array of TUsingResourceEntry;
  PendingEntry: TPendingFinallyEntry;
  HasFunctionDecl, StatementAbrupt: Boolean;
begin
  Result := False;
  HasFunctionDecl := False;
  for I := 0 to AStmt.Nodes.Count - 1 do
    if GetBlockFunctionDeclaration(AStmt.Nodes[I]) <> nil then
    begin
      HasFunctionDecl := True;
      Break;
    end;

  // Check if this block contains any using declarations
  HasUsing := False;
  for I := 0 to AStmt.Nodes.Count - 1 do
    if AStmt.Nodes[I] is TGocciaUsingDeclaration then
    begin
      HasUsing := True;
      Break;
    end;

  // Fast path: no using declarations, original block compilation
  if not HasUsing then
  begin
    ACtx.Scope.BeginScope;
    for I := 0 to AStmt.Nodes.Count - 1 do
      PredeclareBlockLexicalLocals(AStmt.Nodes[I], ACtx);
    if HasFunctionDecl then
      for I := 0 to AStmt.Nodes.Count - 1 do
        if GetBlockFunctionDeclaration(AStmt.Nodes[I]) <> nil then
          ACtx.CompileStatement(TGocciaStatement(AStmt.Nodes[I]));
    for I := 0 to AStmt.Nodes.Count - 1 do
    begin
      Node := AStmt.Nodes[I];
      if GetBlockFunctionDeclaration(Node) <> nil then
      begin
        CompileAnnexBBlockFunctionActivation(ACtx,
          GetBlockFunctionDeclaration(Node));
        Continue;
      end;
      if Node is TGocciaStatement then
      begin
        StatementAbrupt := ACtx.CompileStatement(TGocciaStatement(Node));
        Result := Result or StatementAbrupt;
        if ACtx.OptimizationOptions.EnableDeadBranchElimination and
           not ACtx.OptimizationOptions.PreserveCoverageShape and
           StatementAbrupt then
          Break;
      end
      else if Node is TGocciaExpression then
      begin
        Reg := ACtx.Scope.AllocateRegister;
        ACtx.CompileExpression(TGocciaExpression(Node), Reg);
        ACtx.Scope.FreeRegister;
      end;
    end;
    ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
    for I := 0 to ClosedCount - 1 do
      EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));
    Exit;
  end;

  // Slow path: block with using declarations — compile as try/finally
  ACtx.Scope.BeginScope;
  for I := 0 to AStmt.Nodes.Count - 1 do
    PredeclareBlockLexicalLocals(AStmt.Nodes[I], ACtx);
  if HasFunctionDecl then
    for I := 0 to AStmt.Nodes.Count - 1 do
      if GetBlockFunctionDeclaration(AStmt.Nodes[I]) <> nil then
        ACtx.CompileStatement(TGocciaStatement(AStmt.Nodes[I]));

  // Remember the starting point of using resources for this block
  if not Assigned(GUsingResources) then
    GUsingResources := TList<TUsingResourceEntry>.Create;
  SavedResourceBase := GUsingResources.Count;

  // Allocate registers for exception handling
  CatchReg := ACtx.Scope.AllocateRegister;
  ErrorReg := ACtx.Scope.AllocateRegister;

  // Initialize error accumulator to the internal hole sentinel so user-thrown
  // null/undefined remain real disposal errors.
  EmitInstruction(ACtx, EncodeABC(OP_LOAD_HOLE, ErrorReg, 0, 0));

  // Set up exception handler — catches any throw from the block body
  HandlerJump := EmitJumpInstruction(ACtx, OP_PUSH_HANDLER, CatchReg);

  // Push a pending-finally entry so CompileReturnStatement knows to emit
  // disposal before returning from within this using block.
  if not Assigned(GPendingFinally) then
    GPendingFinally := TList<TPendingFinallyEntry>.Create;
  FillChar(PendingEntry, SizeOf(PendingEntry), 0);
  PendingEntry.FinallyBlock := nil;
  PendingEntry.UsingErrorReg := ErrorReg;
  // Resources not yet known (filled by CompileUsingDeclaration during body)
  GPendingFinally.Add(PendingEntry);

  // Compile all statements in the block (including using declarations)
  for I := 0 to AStmt.Nodes.Count - 1 do
  begin
    Node := AStmt.Nodes[I];
    if GetBlockFunctionDeclaration(Node) <> nil then
    begin
      CompileAnnexBBlockFunctionActivation(ACtx,
        GetBlockFunctionDeclaration(Node));
      Continue;
    end;
    if Node is TGocciaStatement then
    begin
      StatementAbrupt := ACtx.CompileStatement(TGocciaStatement(Node));
      Result := Result or StatementAbrupt;
      if ACtx.OptimizationOptions.EnableDeadBranchElimination and
         not ACtx.OptimizationOptions.PreserveCoverageShape and
         StatementAbrupt then
        Break;
    end
    else if Node is TGocciaExpression then
    begin
      Reg := ACtx.Scope.AllocateRegister;
      ACtx.CompileExpression(TGocciaExpression(Node), Reg);
      ACtx.Scope.FreeRegister;
    end;
  end;

  // Pop the pending-finally entry we pushed
  GPendingFinally.Delete(GPendingFinally.Count - 1);

  // Normal exit: pop handler, run disposal
  EmitInstruction(ACtx, EncodeABC(OP_POP_HANDLER, 0, 0, 0));

  // Collect resources registered by using declarations in this block
  ResourceCount := GUsingResources.Count - SavedResourceBase;
  SetLength(SavedResources, ResourceCount);
  for I := 0 to ResourceCount - 1 do
    SavedResources[I] := GUsingResources[SavedResourceBase + I];

  // Remove the entries we consumed
  for I := GUsingResources.Count - 1 downto SavedResourceBase do
    GUsingResources.Delete(I);

  // Normal path: dispose resources in reverse order
  EmitDisposalSequence(ACtx, SavedResources, ResourceCount, ErrorReg);

  // If any disposal error accumulated, throw it
  NullishJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH, ErrorReg,
    GOCCIA_NULLISH_MATCH_HOLE);
  EmitInstruction(ACtx, EncodeABC(OP_THROW, ErrorReg, 0, 0));
  PatchJumpTarget(ACtx, NullishJump);

  // Jump past the exception handler
  EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);

  // Exception handler: save the caught error, then dispose and re-throw
  PatchJumpTarget(ACtx, HandlerJump);

  // Move caught exception to error accumulator
  EmitInstruction(ACtx, EncodeABC(OP_MOVE, ErrorReg, CatchReg, 0));

  // Dispose resources (SuppressedError chaining handled by OP_USING_DISPOSE)
  EmitDisposalSequence(ACtx, SavedResources, ResourceCount, ErrorReg);

  // Re-throw the accumulated error
  EmitInstruction(ACtx, EncodeABC(OP_THROW, ErrorReg, 0, 0));

  PatchJumpTarget(ACtx, EndJump);

  // Free dispose method registers (in reverse order of allocation)
  for I := ResourceCount - 1 downto 0 do
    ACtx.Scope.FreeRegister;

  // Free exception handling registers
  ACtx.Scope.FreeRegister; // ErrorReg
  ACtx.Scope.FreeRegister; // CatchReg

  ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
  for I := 0 to ClosedCount - 1 do
    EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));
end;

function CompileWithStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaWithStatement): Boolean;
var
  HiddenName: string;
  HiddenSlot: UInt8;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount, I: Integer;
begin
  if not ACtx.NonStrictMode then
    raise TGocciaSyntaxError.Create(
      '''with'' statements are not allowed in strict mode',
      AStmt.Line, AStmt.Column, ACtx.SourcePath, nil);

  ACtx.Scope.BeginScope;
  HiddenName := '#with:' + IntToStr(ACtx.Template.CodeCount) + ':' +
    IntToStr(ACtx.Scope.Depth);
  HiddenSlot := ACtx.Scope.DeclareLocal(HiddenName, False);

  ACtx.CompileExpression(AStmt.ObjectExpression, HiddenSlot);
  EmitInstruction(ACtx, EncodeABC(OP_TO_OBJECT, HiddenSlot, HiddenSlot, 0));

  ACtx.Scope.PushWithBinding(HiddenName);
  try
    Result := ACtx.CompileStatement(AStmt.Body);
  finally
    ACtx.Scope.PopWithBinding;
  end;

  ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
  for I := 0 to ClosedCount - 1 do
    EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));
end;

function CompileIfStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaIfStatement): Boolean;
var
  CondReg, PatternSubjectReg: UInt8;
  ElseJump, EndJump: Integer;
  HasPatternBindings: Boolean;
  PatternFailJumps: TGocciaJumpArray;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount, I: Integer;
  ConditionValue: TGocciaCompileTimeValue;
  ConsequentAbrupt, AlternateAbrupt: Boolean;

  procedure PatchPatternFailureTarget;
  var
    J: Integer;
  begin
    if not HasPatternBindings then
      Exit;

    for J := 0 to High(PatternFailJumps) do
      PatchJumpTarget(ACtx, PatternFailJumps[J]);
    for J := 0 to ClosedCount - 1 do
      EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[J], 0, 0));
  end;
begin
  Result := False;

  if ACtx.OptimizationOptions.EnableDeadBranchElimination and
     not ACtx.OptimizationOptions.PreserveCoverageShape and
     TryEvaluateConstantExpression(ACtx, AStmt.Condition, ConditionValue) then
  begin
    if CompileTimeValueToBoolean(ConditionValue) then
      Result := ACtx.CompileStatement(AStmt.Consequent)
    else if Assigned(AStmt.Alternate) then
      Result := ACtx.CompileStatement(AStmt.Alternate);
    Exit;
  end;

  CondReg := ACtx.Scope.AllocateRegister;
  HasPatternBindings := False;
  PatternSubjectReg := 0;
  SetLength(PatternFailJumps, 0);
  try
    HasPatternBindings := CompileConditionWithPatternBindings(ACtx,
      AStmt.Condition, CondReg, PatternSubjectReg, PatternFailJumps);
    if not HasPatternBindings then
      ACtx.CompileExpression(AStmt.Condition, CondReg);

    ElseJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, CondReg);
    ConsequentAbrupt := ACtx.CompileStatement(AStmt.Consequent);

    if HasPatternBindings then
    begin
      ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
      for I := 0 to ClosedCount - 1 do
        EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));
    end;

    if Assigned(AStmt.Alternate) then
    begin
      EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
      PatchJumpTarget(ACtx, ElseJump);
      PatchPatternFailureTarget;
      AlternateAbrupt := ACtx.CompileStatement(AStmt.Alternate);
      PatchJumpTarget(ACtx, EndJump);
      Result := ConsequentAbrupt and AlternateAbrupt;
    end
    else
    begin
      PatchJumpTarget(ACtx, ElseJump);
      PatchPatternFailureTarget;
      Result := False;
    end;
  finally
    if HasPatternBindings then
      ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
  end;
end;

// Emit pending finally/disposal blocks before a return or abrupt exit.
// Handles both regular try/finally blocks and using-block disposal entries.
// Processes entries from innermost to outermost. Each entry is temporarily
// popped so that if a finally block contains a nested return, that return
// sees the remaining outer entries for its own cleanup.
procedure EmitPendingCleanup(const ACtx: TGocciaCompilationContext);
var
  Entry: TPendingFinallyEntry;
  Entries: array of TPendingFinallyEntry;
  I, Count: Integer;
begin
  if not Assigned(GPendingFinally) or (GPendingFinally.Count = 0) then
    Exit;
  // Snapshot and remove all entries; process innermost first
  Count := GPendingFinally.Count;
  SetLength(Entries, Count);
  for I := 0 to Count - 1 do
    Entries[I] := GPendingFinally[I];
  // Process from innermost to outermost. Before compiling each entry,
  // trim the list so only older entries remain visible to nested returns.
  for I := Count - 1 downto 0 do
  begin
    // Remove this entry (and any above it that were already processed)
    if GPendingFinally.Count > I then
      GPendingFinally.Delete(I);
    Entry := Entries[I];
    EmitPendingEntryCleanup(ACtx, Entry, True);
  end;
  // Restore all entries for the normal-path compilation
  for I := 0 to Count - 1 do
    GPendingFinally.Insert(I, Entries[I]);
end;

procedure CompileReturnStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaReturnStatement);
var
  Reg: UInt8;
begin
  if Assigned(AStmt.Value) then
  begin
    Reg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(AStmt.Value, Reg);
    EmitPendingCleanup(ACtx);
    EmitInstruction(ACtx, EncodeABC(OP_RETURN, Reg, 0, 0));
    ACtx.Scope.FreeRegister;
  end
  else
  begin
    EmitPendingCleanup(ACtx);
    Reg := ACtx.Scope.AllocateRegister;
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, Reg, 0, 0));
    EmitInstruction(ACtx, EncodeABC(OP_RETURN, Reg, 0, 0));
    ACtx.Scope.FreeRegister;
  end;
end;

procedure CompileThrowStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaThrowStatement);
var
  Reg: UInt8;
begin
  Reg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AStmt.Value, Reg);
  EmitInstruction(ACtx, EncodeABC(OP_THROW, Reg, 0, 0));
  ACtx.Scope.FreeRegister;
end;

procedure CompileTryStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaTryStatement);
var
  CatchReg, PatternTestReg, CatchInitErrorReg: UInt8;
  HandlerJump, EndJump, PatternMismatchJump, CatchSuccessJump: Integer;
  CatchInitHandlerJump, CatchInitSuccessJump: Integer;
  HasCatch, HasFinally: Boolean;
  HasCatchInitHandler: Boolean;
  I: Integer;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount: Integer;
  Entry: TPendingFinallyEntry;
begin
  HasCatch := Assigned(AStmt.CatchBlock);
  HasFinally := Assigned(AStmt.FinallyBlock);
  HasCatchInitHandler := Assigned(AStmt.CatchBindingPattern) and HasFinally;

  CatchReg := ACtx.Scope.AllocateRegister;
  CatchInitErrorReg := 0;
  CatchInitHandlerJump := -1;
  CatchInitSuccessJump := -1;
  PatternTestReg := 0;
  HandlerJump := EmitJumpInstruction(ACtx, OP_PUSH_HANDLER, CatchReg);

  if HasFinally then
  begin
    if not Assigned(GPendingFinally) then
      GPendingFinally := TList<TPendingFinallyEntry>.Create;
    Entry.FinallyBlock := AStmt.FinallyBlock;
    GPendingFinally.Add(Entry);
  end;

  CompileBlockStatement(ACtx, AStmt.Block);

  EmitInstruction(ACtx, EncodeABC(OP_POP_HANDLER, 0, 0, 0));

  if HasFinally then
    GPendingFinally.Delete(GPendingFinally.Count - 1);

  if HasFinally then
    CompileBlockStatement(ACtx, AStmt.FinallyBlock);

  EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);

  PatchJumpTarget(ACtx, HandlerJump);

  if HasCatch then
  begin
    if HasFinally then
      GPendingFinally.Add(Entry);

    PatternMismatchJump := -1;
    if Assigned(AStmt.CatchPattern) then
      PatternTestReg := ACtx.Scope.AllocateRegister;
    if HasCatchInitHandler then
      CatchInitErrorReg := ACtx.Scope.AllocateRegister;

    if (AStmt.CatchParam <> '') or Assigned(AStmt.CatchBindingPattern) then
    begin
      ACtx.Scope.BeginScope;
      if AStmt.CatchParam <> '' then
      begin
        ACtx.Scope.DeclareLocal(AStmt.CatchParam, False);
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, ACtx.Scope.NextSlot - 1, CatchReg, 0));
      end
      else
      begin
        CollectDestructuringBindings(AStmt.CatchBindingPattern, ACtx.Scope);
        EmitBlockPatternHoleInitializers(ACtx, AStmt.CatchBindingPattern);
        if HasCatchInitHandler then
          CatchInitHandlerJump := EmitJumpInstruction(ACtx, OP_PUSH_HANDLER,
            CatchInitErrorReg);
        EmitDestructuring(ACtx, AStmt.CatchBindingPattern, CatchReg);
        if HasCatchInitHandler then
          EmitInstruction(ACtx, EncodeABC(OP_POP_HANDLER, 0, 0, 0));
      end;
    end;

    if Assigned(AStmt.CatchPattern) then
    begin
      DeclarePatternBindings(ACtx, AStmt.CatchPattern);
      CompilePatternTest(ACtx, CatchReg, AStmt.CatchPattern, PatternTestReg);
      PatternMismatchJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE,
        PatternTestReg);
    end;

    CompileBlockStatement(ACtx, AStmt.CatchBlock);

    if (AStmt.CatchParam <> '') or Assigned(AStmt.CatchBindingPattern) then
    begin
      ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
      for I := 0 to ClosedCount - 1 do
        EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));
    end;

    if HasFinally then
    begin
      GPendingFinally.Delete(GPendingFinally.Count - 1);
      CompileBlockStatement(ACtx, AStmt.FinallyBlock);
    end;

    if PatternMismatchJump >= 0 then
    begin
      CatchSuccessJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
      PatchJumpTarget(ACtx, PatternMismatchJump);
      if (AStmt.CatchParam <> '') or Assigned(AStmt.CatchBindingPattern) then
        for I := 0 to ClosedCount - 1 do
          EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));
      if HasFinally then
        CompileBlockStatement(ACtx, AStmt.FinallyBlock);
      EmitInstruction(ACtx, EncodeABC(OP_THROW, CatchReg, 0, 0));
      PatchJumpTarget(ACtx, CatchSuccessJump);
      ACtx.Scope.FreeRegister;
    end;

    if HasCatchInitHandler then
    begin
      CatchInitSuccessJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
      PatchJumpTarget(ACtx, CatchInitHandlerJump);
      for I := 0 to ClosedCount - 1 do
        EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));
      CompileBlockStatement(ACtx, AStmt.FinallyBlock);
      EmitInstruction(ACtx, EncodeABC(OP_THROW, CatchInitErrorReg, 0, 0));
      PatchJumpTarget(ACtx, CatchInitSuccessJump);
      ACtx.Scope.FreeRegister;
    end;
  end
  else
  begin
    if HasFinally then
      CompileBlockStatement(ACtx, AStmt.FinallyBlock);
    EmitInstruction(ACtx, EncodeABC(OP_THROW, CatchReg, 0, 0));
  end;

  PatchJumpTarget(ACtx, EndJump);
  ACtx.Scope.FreeRegister;
end;

function IsConstArrayLocal(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaExpression; out ALocalIdx: Integer): Boolean;
var
  LocalIdx: Integer;
begin
  Result := False;
  if not (AExpr is TGocciaIdentifierExpression) then
    Exit;
  LocalIdx := ACtx.Scope.ResolveLocal(TGocciaIdentifierExpression(AExpr).Name);
  if LocalIdx < 0 then
    Exit;
  if not ACtx.Scope.GetLocal(LocalIdx).IsConst then
    Exit;
  if not ACtx.Scope.GetLocal(LocalIdx).IsArrayTyped then
    Exit;
  ALocalIdx := LocalIdx;
  Result := True;
end;

procedure CompileCountedForOf(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaForOfStatement; const AArrayLocalIdx: Integer);
var
  ArrReg, LenReg, IdxReg, OneReg, CmpReg, ValueReg: UInt8;
  LoopStart, ExitJump, MismatchJump, I, BindLocalIdx: Integer;
  Slot: UInt8;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount: Integer;
  LoopControl: TLoopControlState;
  ElemAnnotation: string;
  ElemType: TGocciaLocalType;
begin
  ArrReg := ACtx.Scope.AllocateRegister;
  LenReg := ACtx.Scope.AllocateRegister;
  IdxReg := ACtx.Scope.AllocateRegister;
  OneReg := ACtx.Scope.AllocateRegister;
  CmpReg := ACtx.Scope.AllocateRegister;
  ValueReg := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(AStmt.Iterable, ArrReg);
  EmitInstruction(ACtx, EncodeABC(OP_GET_LENGTH, LenReg, ArrReg, 0));
  EmitInstruction(ACtx, EncodeAsBx(OP_LOAD_INT, IdxReg, 0));
  EmitInstruction(ACtx, EncodeAsBx(OP_LOAD_INT, OneReg, 1));

  BeginLoopControl(ACtx, LoopControl);
  try
    LoopStart := CurrentCodePosition(ACtx);
    MismatchJump := -1;

    EmitInstruction(ACtx, EncodeABC(OP_GTE_INT, CmpReg, IdxReg, LenReg));
    ExitJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, CmpReg);

    EmitInstruction(ACtx, EncodeABC(OP_ARRAY_GET, ValueReg, ArrReg, IdxReg));

    ACtx.Scope.BeginScope;
    SetLoopContinueScopeDepth(ACtx);
    SetLabeledContinueCleanupBase(AStmt);

    if Assigned(AStmt.AssignmentTarget) then
      EmitDestructuring(ACtx, AStmt.AssignmentTarget, ValueReg, True)
    else if Assigned(AStmt.BindingPattern) then
    begin
      CollectDestructuringBindings(AStmt.BindingPattern, ACtx.Scope, AStmt.IsConst);
      EmitDestructuring(ACtx, AStmt.BindingPattern, ValueReg);
    end
    else if AStmt.BindingName <> '' then
    begin
      Slot := ACtx.Scope.DeclareLocal(AStmt.BindingName, AStmt.IsConst);
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, Slot, ValueReg, 0));

      ElemAnnotation := ACtx.Scope.GetLocal(AArrayLocalIdx).ElementTypeAnnotation;
      if ElemAnnotation <> '' then
      begin
        BindLocalIdx := ACtx.Scope.ResolveLocal(AStmt.BindingName);
        if BindLocalIdx >= 0 then
        begin
          ElemType := TypeAnnotationToLocalType(ElemAnnotation);
          if ACtx.StrictTypes and (ElemType <> sltUntyped) then
          begin
            EmitInstruction(ACtx, EncodeABC(OP_CHECK_TYPE, Slot,
              UInt8(Ord(ElemType)), 0));
            ACtx.Scope.SetLocalTypeHint(BindLocalIdx, ElemType);
            ACtx.Scope.SetLocalStrictlyTyped(BindLocalIdx, True);
            ACtx.Template.SetLocalType(Slot, ElemType);
            ACtx.Template.SetLocalStrictFlag(Slot, True);
          end;
          ACtx.Scope.SetLocalTypeAnnotation(BindLocalIdx, ElemAnnotation);
          if IsArrayTypeAnnotation(ElemAnnotation) then
          begin
            ACtx.Scope.SetLocalArrayTyped(BindLocalIdx, True);
            ACtx.Scope.SetLocalElementTypeAnnotation(BindLocalIdx,
              StripArrayLayer(ElemAnnotation));
          end;
        end;
      end;
    end;

    if Assigned(AStmt.MatchPattern) then
    begin
      DeclarePatternBindings(ACtx, AStmt.MatchPattern);
      CompilePatternTest(ACtx, ValueReg, AStmt.MatchPattern, CmpReg);
      MismatchJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, CmpReg);
    end;

    ACtx.CompileStatement(AStmt.Body);

    // Patch continue jumps before close-upvalue so closures see correct iteration values
    PatchJumpList(ACtx, LoopControl.ContinueJumps);
    PatchLabeledContinueJumps(ACtx, AStmt);
    if MismatchJump >= 0 then
      PatchJumpTarget(ACtx, MismatchJump);

    ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
    for I := 0 to ClosedCount - 1 do
      EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));

    EmitInstruction(ACtx, EncodeABC(OP_ADD_INT, IdxReg, IdxReg, OneReg));
    EmitInstruction(ACtx, EncodeAx(OP_JUMP, LoopStart - CurrentCodePosition(ACtx) - 1));

    PatchJumpTarget(ACtx, ExitJump);

    PatchJumpList(ACtx, LoopControl.BreakJumps);
  finally
    EndLoopControl(LoopControl);
  end;

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileForOfStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaForOfStatement);
var
  IterReg, ValueReg, DoneReg, CloseErrorReg, DisposeReg, UsingErrorReg,
    UsingCatchReg: UInt8;
  LoopStart, ExitJump, MismatchJump, HandlerJump, UsingHandlerJump,
    UsingEndJump, NullishJump, I: Integer;
  Slot: UInt8;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount: Integer;
  LoopControl: TLoopControlState;
  ArrayLocalIdx: Integer;
  PendingEntry, UsingPendingEntry: TPendingFinallyEntry;
  ResourceEntry: TUsingResourceEntry;
  NeedsIteratorClose: Boolean;
  UsingPendingActive: Boolean;
begin
  if (not AStmt.IsUsing) and (not Assigned(AStmt.AssignmentTarget)) and
     IsConstArrayLocal(ACtx, AStmt.Iterable, ArrayLocalIdx) then
  begin
    CompileCountedForOf(ACtx, AStmt, ArrayLocalIdx);
    Exit;
  end;

  IterReg := ACtx.Scope.AllocateRegister;
  ValueReg := ACtx.Scope.AllocateRegister;
  DoneReg := ACtx.Scope.AllocateRegister;
  CloseErrorReg := 0;
  DisposeReg := 0;
  UsingErrorReg := 0;
  UsingCatchReg := 0;

  NeedsIteratorClose := AStmt.IsUsing or Assigned(AStmt.AssignmentTarget) or
    Assigned(AStmt.BindingPattern) or Assigned(AStmt.MatchPattern) or
    StatementNeedsIteratorClose(AStmt.Body);
  if NeedsIteratorClose then
    CloseErrorReg := ACtx.Scope.AllocateRegister;
  if AStmt.IsUsing then
  begin
    DisposeReg := ACtx.Scope.AllocateRegister;
    UsingErrorReg := ACtx.Scope.AllocateRegister;
    UsingCatchReg := ACtx.Scope.AllocateRegister;
  end;

  ACtx.CompileExpression(AStmt.Iterable, IterReg);
  EmitInstruction(ACtx, EncodeABC(OP_GET_ITER, IterReg, IterReg, 0));

  if NeedsIteratorClose and not Assigned(GPendingFinally) then
    GPendingFinally := TList<TPendingFinallyEntry>.Create;
  BeginLoopControl(ACtx, LoopControl);
  try
    HandlerJump := -1;
    if NeedsIteratorClose then
    begin
      HandlerJump := EmitJumpInstruction(ACtx, OP_PUSH_HANDLER, CloseErrorReg);
      FillChar(PendingEntry, SizeOf(PendingEntry), 0);
      PendingEntry.IsIteratorClose := True;
      PendingEntry.IteratorReg := IterReg;
      GPendingFinally.Add(PendingEntry);
      GContinueFinallyBase := GPendingFinally.Count;
    end;

    LoopStart := CurrentCodePosition(ACtx);
    MismatchJump := -1;

    EmitInstruction(ACtx, EncodeABC(OP_ITER_NEXT, ValueReg, DoneReg, IterReg));
    ExitJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, DoneReg);

    ACtx.Scope.BeginScope;
    SetLoopContinueScopeDepth(ACtx);
    SetLabeledContinueCleanupBase(AStmt);

    UsingPendingActive := False;
    UsingHandlerJump := -1;
    if AStmt.IsUsing then
    begin
      EmitInstruction(ACtx, EncodeABC(OP_LOAD_HOLE, UsingErrorReg, 0, 0));
      EmitInstruction(ACtx, EncodeABC(OP_LOAD_NULL, DisposeReg, 0, 0));
      UsingHandlerJump := EmitJumpInstruction(ACtx, OP_PUSH_HANDLER,
        UsingCatchReg);
    end;

    if Assigned(AStmt.AssignmentTarget) then
      EmitDestructuring(ACtx, AStmt.AssignmentTarget, ValueReg, True)
    else if Assigned(AStmt.BindingPattern) then
    begin
      CollectDestructuringBindings(AStmt.BindingPattern, ACtx.Scope, AStmt.IsConst);
      EmitDestructuring(ACtx, AStmt.BindingPattern, ValueReg);
    end
    else if AStmt.BindingName <> '' then
    begin
      Slot := ACtx.Scope.DeclareLocal(AStmt.BindingName, AStmt.IsConst);
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, Slot, ValueReg, 0));
    end;

    if AStmt.IsUsing then
    begin
      ResourceEntry.ValueSlot := ValueReg;
      if AStmt.BindingName <> '' then
        ResourceEntry.ValueSlot := Slot;
      ResourceEntry.DisposeSlot := DisposeReg;
      ResourceEntry.IsAwait := AStmt.IsAwaitUsing;
      EmitInstruction(ACtx, EncodeABC(OP_USING_INIT, DisposeReg,
        ResourceEntry.ValueSlot, Ord(AStmt.IsAwaitUsing)));

      if not Assigned(GPendingFinally) then
        GPendingFinally := TList<TPendingFinallyEntry>.Create;
      FillChar(UsingPendingEntry, SizeOf(UsingPendingEntry), 0);
      UsingPendingEntry.UsingErrorReg := UsingErrorReg;
      SetLength(UsingPendingEntry.UsingResources, 1);
      UsingPendingEntry.UsingResources[0] := ResourceEntry;
      GPendingFinally.Add(UsingPendingEntry);
      UsingPendingActive := True;
      GContinueFinallyBase := GPendingFinally.Count;
      SetLabeledContinueCleanupBase(AStmt);
    end;

    if Assigned(AStmt.MatchPattern) then
    begin
      DeclarePatternBindings(ACtx, AStmt.MatchPattern);
      CompilePatternTest(ACtx, ValueReg, AStmt.MatchPattern, DoneReg);
      MismatchJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, DoneReg);
    end;

    ACtx.CompileStatement(AStmt.Body);

    if MismatchJump >= 0 then
      PatchJumpTarget(ACtx, MismatchJump);

    // Continue targets run the normal per-iteration disposal path.
    PatchJumpList(ACtx, LoopControl.ContinueJumps);
    PatchLabeledContinueJumps(ACtx, AStmt);

    if AStmt.IsUsing then
    begin
      if UsingPendingActive then
      begin
        GPendingFinally.Delete(GPendingFinally.Count - 1);
        UsingPendingActive := False;
      end;

      EmitInstruction(ACtx, EncodeABC(OP_POP_HANDLER, 0, 0, 0));
      EmitDisposalSequence(ACtx, [ResourceEntry], 1, UsingErrorReg);
      NullishJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH,
        UsingErrorReg, GOCCIA_NULLISH_MATCH_HOLE);
      EmitInstruction(ACtx, EncodeABC(OP_THROW, UsingErrorReg, 0, 0));
      PatchJumpTarget(ACtx, NullishJump);

      UsingEndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);

      PatchJumpTarget(ACtx, UsingHandlerJump);
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, UsingErrorReg,
        UsingCatchReg, 0));
      EmitDisposalSequence(ACtx, [ResourceEntry], 1, UsingErrorReg);
      EmitInstruction(ACtx, EncodeABC(OP_THROW, UsingErrorReg, 0, 0));

      PatchJumpTarget(ACtx, UsingEndJump);
    end;

    ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
    for I := 0 to ClosedCount - 1 do
      EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));

    EmitInstruction(ACtx, EncodeAx(OP_JUMP,
      LoopStart - CurrentCodePosition(ACtx) - 1));

    if NeedsIteratorClose then
    begin
      PatchJumpTarget(ACtx, HandlerJump);
      EmitInstruction(ACtx, EncodeABC(OP_ITER_CLOSE, IterReg, 0, 1));
      EmitInstruction(ACtx, EncodeABC(OP_THROW, CloseErrorReg, 0, 0));
    end;

    PatchJumpTarget(ACtx, ExitJump);
    if NeedsIteratorClose then
    begin
      EmitInstruction(ACtx, EncodeABC(OP_POP_HANDLER, 0, 0, 0));
      GPendingFinally.Delete(GPendingFinally.Count - 1);
    end;

    PatchJumpList(ACtx, LoopControl.BreakJumps);
  finally
    EndLoopControl(LoopControl);
  end;

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  if NeedsIteratorClose then
    ACtx.Scope.FreeRegister;
  if AStmt.IsUsing then
  begin
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
  end;
end;

procedure CompileForInStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaForInStatement);
var
  EntriesReg, LenReg, IdxReg, OneReg, CmpReg, EntryReg, KeyReg,
    ValidReg: UInt8;
  LoopStart, ExitJump, I: Integer;
  Slot: UInt8;
  LocalIdx: Integer;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount: Integer;
  LoopControl: TLoopControlState;
begin
  EntriesReg := ACtx.Scope.AllocateRegister;
  LenReg := ACtx.Scope.AllocateRegister;
  IdxReg := ACtx.Scope.AllocateRegister;
  OneReg := ACtx.Scope.AllocateRegister;
  CmpReg := ACtx.Scope.AllocateRegister;
  EntryReg := ACtx.Scope.AllocateRegister;
  KeyReg := ACtx.Scope.AllocateRegister;
  ValidReg := ACtx.Scope.AllocateRegister;

  if AStmt.IsVar and ACtx.GlobalBackedTopLevel then
  begin
    if Assigned(AStmt.BindingPattern) then
    begin
      CollectDestructuringVarBindings(AStmt.BindingPattern, ACtx.Scope);
      EmitGlobalDefinesForPattern(ACtx, AStmt.BindingPattern, False, True,
        False);
    end
    else if AStmt.BindingName <> '' then
    begin
      Slot := ACtx.Scope.DeclareVarLocal(AStmt.BindingName);
      LocalIdx := FindLocalBySlot(ACtx.Scope, AStmt.BindingName, Slot);
      if LocalIdx >= 0 then
        ACtx.Scope.MarkGlobalBacked(LocalIdx);
      EmitGlobalDefine(ACtx, Slot, AStmt.BindingName, False, True, False);
    end;
  end;

  ACtx.CompileExpression(AStmt.ObjectExpression, EntriesReg);
  EmitInstruction(ACtx, EncodeABC(OP_ENUM_KEYS, EntriesReg, EntriesReg, 0));
  EmitInstruction(ACtx, EncodeABC(OP_GET_LENGTH, LenReg, EntriesReg, 0));
  EmitInstruction(ACtx, EncodeAsBx(OP_LOAD_INT, IdxReg, 0));
  EmitInstruction(ACtx, EncodeAsBx(OP_LOAD_INT, OneReg, 1));

  BeginLoopControl(ACtx, LoopControl);
  try
    LoopStart := CurrentCodePosition(ACtx);
    EmitInstruction(ACtx, EncodeABC(OP_GTE_INT, CmpReg, IdxReg, LenReg));
    ExitJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, CmpReg);
    EmitInstruction(ACtx, EncodeABC(OP_ARRAY_GET, EntryReg, EntriesReg, IdxReg));
    EmitInstruction(ACtx, EncodeABC(OP_ADD_INT, IdxReg, IdxReg, OneReg));
    EmitInstruction(ACtx, EncodeABC(OP_ENUM_ENTRY, KeyReg, ValidReg, EntryReg));
    EmitInstruction(ACtx,
      EncodeAsBx(OP_JUMP_IF_FALSE, ValidReg,
        LoopStart - CurrentCodePosition(ACtx) - 1));

    ACtx.Scope.BeginScope;
    SetLoopContinueScopeDepth(ACtx);
    SetLabeledContinueCleanupBase(AStmt);

    if Assigned(AStmt.AssignmentTarget) then
      EmitDestructuring(ACtx, AStmt.AssignmentTarget, KeyReg, True)
    else if Assigned(AStmt.BindingPattern) then
    begin
      if AStmt.IsVar then
        CollectDestructuringVarBindings(AStmt.BindingPattern, ACtx.Scope)
      else
        CollectDestructuringBindings(AStmt.BindingPattern, ACtx.Scope,
          AStmt.IsConst);
      EmitDestructuring(ACtx, AStmt.BindingPattern, KeyReg,
        AStmt.IsVar and ACtx.GlobalBackedTopLevel);
    end
    else if AStmt.BindingName <> '' then
    begin
      if AStmt.IsVar then
        Slot := ACtx.Scope.DeclareVarLocal(AStmt.BindingName)
      else
        Slot := ACtx.Scope.DeclareLocal(AStmt.BindingName, AStmt.IsConst);
      EmitBindingAssignmentFromRegister(ACtx, AStmt.BindingName, KeyReg,
        AStmt.IsVar and ACtx.GlobalBackedTopLevel);
    end;

    ACtx.CompileStatement(AStmt.Body);

    PatchJumpList(ACtx, LoopControl.ContinueJumps);
    PatchLabeledContinueJumps(ACtx, AStmt);

    ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
    for I := 0 to ClosedCount - 1 do
      EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));

    EmitInstruction(ACtx,
      EncodeAx(OP_JUMP, LoopStart - CurrentCodePosition(ACtx) - 1));

    PatchJumpTarget(ACtx, ExitJump);
    PatchJumpList(ACtx, LoopControl.BreakJumps);
  finally
    EndLoopControl(LoopControl);
  end;

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileForAwaitOfStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaForAwaitOfStatement);
var
  IterReg, ValueReg, DoneReg, CloseErrorReg: UInt8;
  LoopStart, ExitJump, MismatchJump, HandlerJump, I: Integer;
  Slot: UInt8;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount: Integer;
  LoopControl: TLoopControlState;
  PendingEntry: TPendingFinallyEntry;
  NeedsIteratorClose: Boolean;
begin
  IterReg := ACtx.Scope.AllocateRegister;
  ValueReg := ACtx.Scope.AllocateRegister;
  DoneReg := ACtx.Scope.AllocateRegister;
  CloseErrorReg := 0;

  NeedsIteratorClose := Assigned(AStmt.AssignmentTarget) or
    Assigned(AStmt.BindingPattern) or Assigned(AStmt.MatchPattern) or
    StatementNeedsIteratorClose(AStmt.Body);
  if NeedsIteratorClose then
    CloseErrorReg := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(AStmt.Iterable, IterReg);
  EmitInstruction(ACtx, EncodeABC(OP_GET_ITER, IterReg, IterReg, 1));

  if NeedsIteratorClose and not Assigned(GPendingFinally) then
    GPendingFinally := TList<TPendingFinallyEntry>.Create;
  BeginLoopControl(ACtx, LoopControl);
  try
    HandlerJump := -1;
    if NeedsIteratorClose then
    begin
      HandlerJump := EmitJumpInstruction(ACtx, OP_PUSH_HANDLER, CloseErrorReg);
      FillChar(PendingEntry, SizeOf(PendingEntry), 0);
      PendingEntry.IsIteratorClose := True;
      PendingEntry.IsAsyncIterator := True;
      PendingEntry.IteratorReg := IterReg;
      GPendingFinally.Add(PendingEntry);
      GContinueFinallyBase := GPendingFinally.Count;
    end;

    LoopStart := CurrentCodePosition(ACtx);
    MismatchJump := -1;

    EmitInstruction(ACtx, EncodeABC(OP_ASYNC_ITER_NEXT, ValueReg, 0, IterReg));
    EmitInstruction(ACtx, EncodeABC(OP_AWAIT, ValueReg, ValueReg, 0));
    EmitInstruction(ACtx, EncodeABC(OP_ITER_UNPACK, ValueReg, DoneReg, ValueReg));
    ExitJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, DoneReg);

    ACtx.Scope.BeginScope;
    SetLoopContinueScopeDepth(ACtx);
    SetLabeledContinueCleanupBase(AStmt);

    if Assigned(AStmt.AssignmentTarget) then
      EmitDestructuring(ACtx, AStmt.AssignmentTarget, ValueReg, True)
    else if Assigned(AStmt.BindingPattern) then
    begin
      CollectDestructuringBindings(AStmt.BindingPattern, ACtx.Scope, AStmt.IsConst);
      EmitDestructuring(ACtx, AStmt.BindingPattern, ValueReg);
    end
    else if AStmt.BindingName <> '' then
    begin
      Slot := ACtx.Scope.DeclareLocal(AStmt.BindingName, AStmt.IsConst);
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, Slot, ValueReg, 0));
    end;

    if Assigned(AStmt.MatchPattern) then
    begin
      DeclarePatternBindings(ACtx, AStmt.MatchPattern);
      CompilePatternTest(ACtx, ValueReg, AStmt.MatchPattern, DoneReg);
      MismatchJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, DoneReg);
    end;

    ACtx.CompileStatement(AStmt.Body);

    // Patch continue jumps before close-upvalue so closures see correct iteration values
    PatchJumpList(ACtx, LoopControl.ContinueJumps);
    PatchLabeledContinueJumps(ACtx, AStmt);
    if MismatchJump >= 0 then
      PatchJumpTarget(ACtx, MismatchJump);

    ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
    for I := 0 to ClosedCount - 1 do
      EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));

    EmitInstruction(ACtx, EncodeAx(OP_JUMP, LoopStart - CurrentCodePosition(ACtx) - 1));

    if NeedsIteratorClose then
    begin
      PatchJumpTarget(ACtx, HandlerJump);
      EmitInstruction(ACtx, EncodeABC(OP_ITER_CLOSE, IterReg, 1, 1));
      EmitInstruction(ACtx, EncodeABC(OP_THROW, CloseErrorReg, 0, 0));
    end;

    PatchJumpTarget(ACtx, ExitJump);
    if NeedsIteratorClose then
    begin
      EmitInstruction(ACtx, EncodeABC(OP_POP_HANDLER, 0, 0, 0));
      GPendingFinally.Delete(GPendingFinally.Count - 1);
    end;

    PatchJumpList(ACtx, LoopControl.BreakJumps);
  finally
    EndLoopControl(LoopControl);
  end;

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  if NeedsIteratorClose then
    ACtx.Scope.FreeRegister;
end;

function ForBodyAssignsIdentifier(const ANode: TGocciaASTNode;
  const AName: string): Boolean; forward;
function StatementNeedsPerIterationEnvironment(
  const ANode: TGocciaASTNode): Boolean; forward;

function ExpressionNeedsPerIterationEnvironment(
  const AExpr: TGocciaExpression): Boolean;
begin
  Result := ExpressionContainsDirectEval(AExpr) or
    ExpressionContainsSuspension(AExpr) or
    ExpressionCreatesClosureBoundary(AExpr);
end;

function TryCompileCountedFor(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaForStatement): Boolean;
var
  VarDecl: TGocciaVariableDeclaration;
  IsConst, IsVarKeyword: Boolean;
  LoopName: string;
  StartLit: TGocciaLiteralExpression;
  StartIntDouble: Double;
  StartInt: Integer;
  CondExpr: TGocciaBinaryExpression;
  CondLeftIdent: TGocciaIdentifierExpression;
  IncExpr: TGocciaIncrementExpression;
  IncOperandIdent: TGocciaIdentifierExpression;
  StartReg, LimitReg, OneReg, CmpReg: UInt8;
  Slot, OuterSlot: UInt8;
  LoopStart, ExitJump, I: Integer;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount: Integer;
  LoopControl: TLoopControlState;
  IsAscending: Boolean;
  ExitOpcode, StepOpcode: TGocciaOpCode;
begin
  Result := False;

  if not Assigned(AStmt.Init) or not (AStmt.Init is TGocciaVariableDeclaration) then
    Exit;
  VarDecl := TGocciaVariableDeclaration(AStmt.Init);
  if Length(VarDecl.Variables) <> 1 then
    Exit;
  if not VarDecl.Variables[0].HasInitializer then
    Exit;
  if not (VarDecl.Variables[0].Initializer is TGocciaLiteralExpression) then
    Exit;
  StartLit := TGocciaLiteralExpression(VarDecl.Variables[0].Initializer);
  if not (StartLit.Value is TGocciaNumberLiteralValue) then
    Exit;
  StartIntDouble := TGocciaNumberLiteralValue(StartLit.Value).Value;
  if (StartIntDouble <> Trunc(StartIntDouble)) or
     (StartIntDouble < Low(Int16)) or (StartIntDouble > High(Int16)) then
    Exit;
  StartInt := Trunc(StartIntDouble);
  IsConst := VarDecl.IsConst;
  IsVarKeyword := VarDecl.IsVar;
  // const can't be incremented anyway. var has shared (not per-iteration)
  // binding and may be global-backed at top level — both make the fast path's
  // per-iteration slot copy wrong; defer to the general path.
  if IsConst or IsVarKeyword then
    Exit;
  // Type annotations (e.g. `for (let i: number = 0; ...)`) carry hints the
  // fast path can't propagate onto the synthetic per-iteration slot; route
  // through the general path so CompileVariableDeclaration installs them.
  if VarDecl.Variables[0].TypeAnnotation <> '' then
    Exit;

  LoopName := VarDecl.Variables[0].Name;

  if not Assigned(AStmt.Condition) or
     not (AStmt.Condition is TGocciaBinaryExpression) then
    Exit;
  CondExpr := TGocciaBinaryExpression(AStmt.Condition);
  if not (CondExpr.Left is TGocciaIdentifierExpression) then
    Exit;
  CondLeftIdent := TGocciaIdentifierExpression(CondExpr.Left);
  if CondLeftIdent.Name <> LoopName then
    Exit;
  // ES2026 §14.7.4.4 evaluates the test expression each iteration. The fast
  // path snapshots LimitReg once before the loop, so anything that can change
  // between iterations would diverge from the spec. Restrict to integer-valued
  // numeric literals only — `ForBodyAssignsIdentifier` doesn't see writes
  // through IIFEs/callbacks/property setters, so a bare-identifier RHS is
  // unsafe; and the emitted compare uses OP_GTE_INT/OP_LTE_INT, so a
  // non-integer literal like `i < 3.5` would round in surprising ways
  // relative to the spec's IEEE 754 compare.
  if not (CondExpr.Right is TGocciaLiteralExpression) then
    Exit;
  if not (TGocciaLiteralExpression(CondExpr.Right).Value is TGocciaNumberLiteralValue) then
    Exit;
  if Frac(TGocciaNumberLiteralValue(
       TGocciaLiteralExpression(CondExpr.Right).Value).Value) <> 0 then
    Exit;

  case CondExpr.Operator of
    gttLess: begin IsAscending := True; ExitOpcode := OP_GTE_INT; end;
    gttGreater: begin IsAscending := False; ExitOpcode := OP_LTE_INT; end;
  else
    Exit;
  end;

  if not Assigned(AStmt.Update) or
     not (AStmt.Update is TGocciaIncrementExpression) then
    Exit;
  IncExpr := TGocciaIncrementExpression(AStmt.Update);
  if not (IncExpr.Operand is TGocciaIdentifierExpression) then
    Exit;
  IncOperandIdent := TGocciaIdentifierExpression(IncExpr.Operand);
  if IncOperandIdent.Name <> LoopName then
    Exit;
  if IsAscending and (IncExpr.Operator <> gttIncrement) then
    Exit;
  if (not IsAscending) and (IncExpr.Operator <> gttDecrement) then
    Exit;
  if IsAscending then
    StepOpcode := OP_ADD_INT
  else
    StepOpcode := OP_SUB_INT;

  if ForBodyAssignsIdentifier(AStmt.Body, LoopName) then
    Exit;

  // Outer scope owns the canonical loop var slot; per-iteration scope
  // declares a fresh slot and copies in/out so closures pin per iteration.
  ACtx.Scope.BeginScope;
  StartReg := ACtx.Scope.DeclareLocal(LoopName, False);
  EmitInstruction(ACtx, EncodeAsBx(OP_LOAD_INT, StartReg, Int16(StartInt)));

  if IsVarKeyword then
    ; // var doesn't need per-iteration treatment but the fast path still
      // works the same; we only hit this branch when it's a let.
  // (IsConst already excluded above; IsVar excluded by the path above as
  // well — keep the guard simple by just rejecting var.)

  LimitReg := ACtx.Scope.AllocateRegister;
  OneReg := ACtx.Scope.AllocateRegister;
  CmpReg := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(CondExpr.Right, LimitReg);
  EmitInstruction(ACtx, EncodeAsBx(OP_LOAD_INT, OneReg, 1));

  BeginLoopControl(ACtx, LoopControl);
  try
    LoopStart := CurrentCodePosition(ACtx);

    EmitInstruction(ACtx, EncodeABC(ExitOpcode, CmpReg, StartReg, LimitReg));
    ExitJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, CmpReg);

    OuterSlot := StartReg;
    ACtx.Scope.BeginScope;
    SetLoopContinueScopeDepth(ACtx);
    SetLabeledContinueCleanupBase(AStmt);
    Slot := ACtx.Scope.DeclareLocal(LoopName, False);
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, Slot, OuterSlot, 0));

    ACtx.CompileStatement(AStmt.Body);

    PatchJumpList(ACtx, LoopControl.ContinueJumps);
    PatchLabeledContinueJumps(ACtx, AStmt);

    EmitInstruction(ACtx, EncodeABC(OP_MOVE, OuterSlot, Slot, 0));
    ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
    for I := 0 to ClosedCount - 1 do
      EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));

    EmitInstruction(ACtx, EncodeABC(StepOpcode, OuterSlot, OuterSlot, OneReg));
    EmitInstruction(ACtx, EncodeAx(OP_JUMP,
      LoopStart - CurrentCodePosition(ACtx) - 1));

    PatchJumpTarget(ACtx, ExitJump);

    PatchJumpList(ACtx, LoopControl.BreakJumps);
  finally
    EndLoopControl(LoopControl);
  end;

  ACtx.Scope.FreeRegister; // CmpReg
  ACtx.Scope.FreeRegister; // OneReg
  ACtx.Scope.FreeRegister; // LimitReg
  // StartReg is popped by EndScope of the outer for-scope below.

  ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
  for I := 0 to ClosedCount - 1 do
    EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));

  Result := True;
end;

function PatternAssignsIdentifier(const APattern: TGocciaDestructuringPattern;
  const AName: string): Boolean;
var
  Names: TStringList;
begin
  Result := False;
  if not Assigned(APattern) then
    Exit;

  Names := TStringList.Create;
  Names.CaseSensitive := True;
  try
    CollectPatternBindingNames(APattern, Names);
    Result := Names.IndexOf(AName) >= 0;
  finally
    Names.Free;
  end;
end;

function ForBodyAssignsIdentifier(const ANode: TGocciaASTNode;
  const AName: string): Boolean;
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
  ExprStmt: TGocciaExpressionStatement;
  Assign: TGocciaAssignmentExpression;
  Compound: TGocciaCompoundAssignmentExpression;
  IncExpr: TGocciaIncrementExpression;
  ReturnStmt: TGocciaReturnStatement;
  ThrowStmt: TGocciaThrowStatement;
  I, J: Integer;
begin
  Result := False;
  if not Assigned(ANode) then
    Exit;

  if ANode is TGocciaExpressionStatement then
  begin
    ExprStmt := TGocciaExpressionStatement(ANode);
    Result := ForBodyAssignsIdentifier(ExprStmt.Expression, AName);
  end
  else if ANode is TGocciaAssignmentExpression then
  begin
    Assign := TGocciaAssignmentExpression(ANode);
    if Assign.Name = AName then
      Exit(True);
    Result := ForBodyAssignsIdentifier(Assign.Value, AName);
  end
  else if ANode is TGocciaCompoundAssignmentExpression then
  begin
    Compound := TGocciaCompoundAssignmentExpression(ANode);
    if Compound.Name = AName then
      Exit(True);
    Result := ForBodyAssignsIdentifier(Compound.Value, AName);
  end
  else if ANode is TGocciaIncrementExpression then
  begin
    IncExpr := TGocciaIncrementExpression(ANode);
    if (IncExpr.Operand is TGocciaIdentifierExpression) and
       (TGocciaIdentifierExpression(IncExpr.Operand).Name = AName) then
      Exit(True);
    Result := ForBodyAssignsIdentifier(IncExpr.Operand, AName);
  end
  else if ANode is TGocciaBlockStatement then
  begin
    Block := TGocciaBlockStatement(ANode);
    for I := 0 to Block.Nodes.Count - 1 do
      if ForBodyAssignsIdentifier(Block.Nodes[I], AName) then
        Exit(True);
  end
  else if ANode is TGocciaIfStatement then
  begin
    IfStmt := TGocciaIfStatement(ANode);
    if ForBodyAssignsIdentifier(IfStmt.Condition, AName) then Exit(True);
    if ForBodyAssignsIdentifier(IfStmt.Consequent, AName) then Exit(True);
    Result := ForBodyAssignsIdentifier(IfStmt.Alternate, AName);
  end
  else if ANode is TGocciaForOfStatement then
  begin
    ForOf := TGocciaForOfStatement(ANode);
    if ForOf.IsVar then
    begin
      if ForOf.BindingName = AName then
        Exit(True);
      if PatternAssignsIdentifier(ForOf.BindingPattern, AName) then
        Exit(True);
    end;
    if ForBodyAssignsIdentifier(ForOf.Iterable, AName) then Exit(True);
    Result := ForBodyAssignsIdentifier(ForOf.Body, AName);
  end
  else if ANode is TGocciaForInStatement then
  begin
    ForIn := TGocciaForInStatement(ANode);
    if PatternAssignsIdentifier(ForIn.AssignmentTarget, AName) then
      Exit(True);
    if ForIn.IsVar then
    begin
      if ForIn.BindingName = AName then
        Exit(True);
      if PatternAssignsIdentifier(ForIn.BindingPattern, AName) then
        Exit(True);
    end;
    if ForBodyAssignsIdentifier(ForIn.ObjectExpression, AName) then Exit(True);
    Result := ForBodyAssignsIdentifier(ForIn.Body, AName);
  end
  else if ANode is TGocciaForStatement then
  begin
    ForStmt := TGocciaForStatement(ANode);
    if ForBodyAssignsIdentifier(ForStmt.Init, AName) then Exit(True);
    if ForBodyAssignsIdentifier(ForStmt.Condition, AName) then Exit(True);
    if ForBodyAssignsIdentifier(ForStmt.Update, AName) then Exit(True);
    Result := ForBodyAssignsIdentifier(ForStmt.Body, AName);
  end
  else if ANode is TGocciaWhileStatement then
  begin
    WhileStmt := TGocciaWhileStatement(ANode);
    if ForBodyAssignsIdentifier(WhileStmt.Condition, AName) then Exit(True);
    Result := ForBodyAssignsIdentifier(WhileStmt.Body, AName);
  end
  else if ANode is TGocciaDoWhileStatement then
  begin
    DoWhileStmt := TGocciaDoWhileStatement(ANode);
    if ForBodyAssignsIdentifier(DoWhileStmt.Body, AName) then Exit(True);
    Result := ForBodyAssignsIdentifier(DoWhileStmt.Condition, AName);
  end
  else if ANode is TGocciaTryStatement then
  begin
    TryStmt := TGocciaTryStatement(ANode);
    if ForBodyAssignsIdentifier(TryStmt.Block, AName) then Exit(True);
    if ForBodyAssignsIdentifier(TryStmt.CatchBlock, AName) then Exit(True);
    Result := ForBodyAssignsIdentifier(TryStmt.FinallyBlock, AName);
  end
  else if ANode is TGocciaSwitchStatement then
  begin
    SwitchStmt := TGocciaSwitchStatement(ANode);
    for I := 0 to SwitchStmt.Cases.Count - 1 do
      for J := 0 to SwitchStmt.Cases[I].Consequent.Count - 1 do
        if ForBodyAssignsIdentifier(SwitchStmt.Cases[I].Consequent[J], AName) then
          Exit(True);
  end
  else if ANode is TGocciaReturnStatement then
  begin
    ReturnStmt := TGocciaReturnStatement(ANode);
    Result := ForBodyAssignsIdentifier(ReturnStmt.Value, AName);
  end
  else if ANode is TGocciaThrowStatement then
  begin
    ThrowStmt := TGocciaThrowStatement(ANode);
    Result := ForBodyAssignsIdentifier(ThrowStmt.Value, AName);
  end;
  // Conservative: only reports DEFINITE writes to a top-level identifier.
  // Closures captured in the body that reassign `i` are NOT flagged here, but
  // they would target a per-iteration slot in the slow path anyway. The fast
  // path's assumption is that direct synchronous writes to `i` from the body
  // are absent; deeper analysis would be unsound.
end;

function StatementNeedsPerIterationEnvironment(
  const ANode: TGocciaASTNode): Boolean;
var
  Block: TGocciaBlockStatement;
  ExprStmt: TGocciaExpressionStatement;
  VarDecl: TGocciaVariableDeclaration;
  IfStmt: TGocciaIfStatement;
  ForStmt: TGocciaForStatement;
  ForOf: TGocciaForOfStatement;
  ForIn: TGocciaForInStatement;
  WhileStmt: TGocciaWhileStatement;
  DoWhileStmt: TGocciaDoWhileStatement;
  TryStmt: TGocciaTryStatement;
  ReturnStmt: TGocciaReturnStatement;
  ThrowStmt: TGocciaThrowStatement;
  SwitchStmt: TGocciaSwitchStatement;
  EnumStmt: TGocciaEnumDeclaration;
  ExportDefault: TGocciaExportDefaultDeclaration;
  ExportVar: TGocciaExportVariableDeclaration;
  ExportDestruct: TGocciaExportDestructuringDeclaration;
  ExportEnum: TGocciaExportEnumDeclaration;
  I, J: Integer;
begin
  Result := False;
  if not Assigned(ANode) then
    Exit;

  if ANode is TGocciaExpression then
    Exit(ExpressionNeedsPerIterationEnvironment(TGocciaExpression(ANode)));

  if (ANode is TGocciaFunctionDeclaration) or
     (ANode is TGocciaClassDeclaration) or
     (ANode is TGocciaExportFunctionDeclaration) or
     (ANode is TGocciaExportClassDeclaration) or
     (ANode is TGocciaUsingDeclaration) or
     (ANode is TGocciaWithStatement) or
     (ANode is TGocciaForAwaitOfStatement) then
    Exit(True);

  if ANode is TGocciaExpressionStatement then
  begin
    ExprStmt := TGocciaExpressionStatement(ANode);
    Result := ExpressionNeedsPerIterationEnvironment(ExprStmt.Expression);
  end
  else if ANode is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(ANode);
    for I := 0 to High(VarDecl.Variables) do
      if VarDecl.Variables[I].HasInitializer and
         ExpressionNeedsPerIterationEnvironment(VarDecl.Variables[I].Initializer) then
        Exit(True);
  end
  else if ANode is TGocciaDestructuringDeclaration then
    Exit(True)
  else if ANode is TGocciaBlockStatement then
  begin
    Block := TGocciaBlockStatement(ANode);
    for I := 0 to Block.Nodes.Count - 1 do
      if StatementNeedsPerIterationEnvironment(Block.Nodes[I]) then
        Exit(True);
  end
  else if ANode is TGocciaIfStatement then
  begin
    IfStmt := TGocciaIfStatement(ANode);
    Result := ExpressionNeedsPerIterationEnvironment(IfStmt.Condition) or
      StatementNeedsPerIterationEnvironment(IfStmt.Consequent) or
      StatementNeedsPerIterationEnvironment(IfStmt.Alternate);
  end
  else if ANode is TGocciaForStatement then
  begin
    ForStmt := TGocciaForStatement(ANode);
    Result := StatementNeedsPerIterationEnvironment(ForStmt.Init) or
      ExpressionNeedsPerIterationEnvironment(ForStmt.Condition) or
      ExpressionNeedsPerIterationEnvironment(ForStmt.Update) or
      StatementNeedsPerIterationEnvironment(ForStmt.Body);
  end
  else if ANode is TGocciaForOfStatement then
  begin
    ForOf := TGocciaForOfStatement(ANode);
    Result := Assigned(ForOf.AssignmentTarget) or
      Assigned(ForOf.BindingPattern) or Assigned(ForOf.MatchPattern) or
      ExpressionNeedsPerIterationEnvironment(ForOf.Iterable) or
      StatementNeedsPerIterationEnvironment(ForOf.Body);
  end
  else if ANode is TGocciaForInStatement then
  begin
    ForIn := TGocciaForInStatement(ANode);
    Result := Assigned(ForIn.AssignmentTarget) or
      Assigned(ForIn.BindingPattern) or
      ExpressionNeedsPerIterationEnvironment(ForIn.ObjectExpression) or
      StatementNeedsPerIterationEnvironment(ForIn.Body);
  end
  else if ANode is TGocciaWhileStatement then
  begin
    WhileStmt := TGocciaWhileStatement(ANode);
    Result := ExpressionNeedsPerIterationEnvironment(WhileStmt.Condition) or
      StatementNeedsPerIterationEnvironment(WhileStmt.Body);
  end
  else if ANode is TGocciaDoWhileStatement then
  begin
    DoWhileStmt := TGocciaDoWhileStatement(ANode);
    Result := StatementNeedsPerIterationEnvironment(DoWhileStmt.Body) or
      ExpressionNeedsPerIterationEnvironment(DoWhileStmt.Condition);
  end
  else if ANode is TGocciaTryStatement then
  begin
    TryStmt := TGocciaTryStatement(ANode);
    Result := Assigned(TryStmt.CatchBindingPattern) or
      Assigned(TryStmt.CatchPattern) or
      StatementNeedsPerIterationEnvironment(TryStmt.Block) or
      StatementNeedsPerIterationEnvironment(TryStmt.CatchBlock) or
      StatementNeedsPerIterationEnvironment(TryStmt.FinallyBlock);
  end
  else if ANode is TGocciaReturnStatement then
  begin
    ReturnStmt := TGocciaReturnStatement(ANode);
    Result := ExpressionNeedsPerIterationEnvironment(ReturnStmt.Value);
  end
  else if ANode is TGocciaThrowStatement then
  begin
    ThrowStmt := TGocciaThrowStatement(ANode);
    Result := ExpressionNeedsPerIterationEnvironment(ThrowStmt.Value);
  end
  else if ANode is TGocciaSwitchStatement then
  begin
    SwitchStmt := TGocciaSwitchStatement(ANode);
    if ExpressionNeedsPerIterationEnvironment(SwitchStmt.Discriminant) then
      Exit(True);
    for I := 0 to SwitchStmt.Cases.Count - 1 do
    begin
      if ExpressionNeedsPerIterationEnvironment(SwitchStmt.Cases[I].Test) then
        Exit(True);
      for J := 0 to SwitchStmt.Cases[I].Consequent.Count - 1 do
        if StatementNeedsPerIterationEnvironment(
          SwitchStmt.Cases[I].Consequent[J]) then
          Exit(True);
    end;
  end
  else if ANode is TGocciaEnumDeclaration then
  begin
    EnumStmt := TGocciaEnumDeclaration(ANode);
    for I := 0 to High(EnumStmt.Members) do
      if ExpressionNeedsPerIterationEnvironment(EnumStmt.Members[I].Initializer) then
        Exit(True);
  end
  else if ANode is TGocciaExportDefaultDeclaration then
  begin
    ExportDefault := TGocciaExportDefaultDeclaration(ANode);
    Result := ExpressionNeedsPerIterationEnvironment(ExportDefault.Expression);
  end
  else if ANode is TGocciaExportVariableDeclaration then
  begin
    ExportVar := TGocciaExportVariableDeclaration(ANode);
    Result := StatementNeedsPerIterationEnvironment(ExportVar.Declaration);
  end
  else if ANode is TGocciaExportDestructuringDeclaration then
  begin
    ExportDestruct := TGocciaExportDestructuringDeclaration(ANode);
    Result := StatementNeedsPerIterationEnvironment(ExportDestruct.Declaration);
  end
  else if ANode is TGocciaExportEnumDeclaration then
  begin
    ExportEnum := TGocciaExportEnumDeclaration(ANode);
    Result := StatementNeedsPerIterationEnvironment(ExportEnum.Declaration);
  end;
end;

function ForStatementCanShareLexicalEnvironment(
  const AStmt: TGocciaForStatement): Boolean;
begin
  Result := Assigned(AStmt.Init) and
    (AStmt.Init is TGocciaVariableDeclaration) and
    not TGocciaVariableDeclaration(AStmt.Init).IsVar and
    not StatementNeedsPerIterationEnvironment(AStmt.Init) and
    not ExpressionNeedsPerIterationEnvironment(AStmt.Condition) and
    not ExpressionNeedsPerIterationEnvironment(AStmt.Update) and
    not StatementNeedsPerIterationEnvironment(AStmt.Body);
end;

procedure CompileForStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaForStatement);
var
  CondReg: UInt8;
  LoopStart, ExitJump, I: Integer;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount: Integer;
  BodyClosedLocals: array[0..255] of UInt8;
  BodyClosedCount: Integer;
  UpdateClosedLocals: array[0..255] of UInt8;
  UpdateClosedCount: Integer;
  LoopControl: TLoopControlState;
  HasLexicalInit: Boolean;
  UseSharedLexicalFor: Boolean;
  PerIterNames: TStringList;
  PerIterIsConst: Boolean;
  OuterSlots, BodySlots, UpdateSlots: array of UInt8;
  OuterLocalIdxs: array of Integer;
  Name: string;
  VarDecl: TGocciaVariableDeclaration;
  DestructDecl: TGocciaDestructuringDeclaration;
  LocalIdx: Integer;
  HasUsingInit: Boolean;
  SavedResourceBase, ResourceCount: Integer;
  CatchReg, ErrorReg: UInt8;
  HandlerJump, EndJump, NullishJump: Integer;
  PendingEntry: TPendingFinallyEntry;
  SavedResources: array of TUsingResourceEntry;
  UsingDecl: TGocciaUsingDeclaration;
begin
  if TryCompileCountedFor(ACtx, AStmt) then
    Exit;

  HasLexicalInit := False;
  HasUsingInit := AStmt.Init is TGocciaUsingDeclaration;
  UseSharedLexicalFor := False;
  PerIterIsConst := False;
  PerIterNames := nil;

  if Assigned(AStmt.Init) then
  begin
    if (AStmt.Init is TGocciaVariableDeclaration) and
       not TGocciaVariableDeclaration(AStmt.Init).IsVar then
    begin
      HasLexicalInit := True;
      VarDecl := TGocciaVariableDeclaration(AStmt.Init);
      PerIterIsConst := VarDecl.IsConst;
      PerIterNames := TStringList.Create;
      for I := 0 to High(VarDecl.Variables) do
        PerIterNames.Add(VarDecl.Variables[I].Name);
    end
    else if (AStmt.Init is TGocciaDestructuringDeclaration)
            and not TGocciaDestructuringDeclaration(AStmt.Init).IsVar then
    begin
      HasLexicalInit := True;
      DestructDecl := TGocciaDestructuringDeclaration(AStmt.Init);
      PerIterIsConst := DestructDecl.IsConst;
      PerIterNames := TStringList.Create;
      CollectPatternBindingNames(DestructDecl.Pattern, PerIterNames, True);
    end
    else if HasUsingInit then
    begin
      HasLexicalInit := True;
      PerIterIsConst := True;
      UsingDecl := TGocciaUsingDeclaration(AStmt.Init);
      PerIterNames := TStringList.Create;
      for I := 0 to High(UsingDecl.Variables) do
        PerIterNames.Add(UsingDecl.Variables[I].Name);
    end;
  end;

  if HasLexicalInit then
    UseSharedLexicalFor := ForStatementCanShareLexicalEnvironment(AStmt);

  try
    if HasLexicalInit then
      ACtx.Scope.BeginScope;

    SavedResourceBase := 0;
    CatchReg := 0;
    ErrorReg := 0;
    HandlerJump := -1;
    if HasUsingInit then
    begin
      if not Assigned(GUsingResources) then
        GUsingResources := TList<TUsingResourceEntry>.Create;
      SavedResourceBase := GUsingResources.Count;
      CatchReg := ACtx.Scope.AllocateRegister;
      ErrorReg := ACtx.Scope.AllocateRegister;
      EmitInstruction(ACtx, EncodeABC(OP_LOAD_HOLE, ErrorReg, 0, 0));
      HandlerJump := EmitJumpInstruction(ACtx, OP_PUSH_HANDLER, CatchReg);

      if not Assigned(GPendingFinally) then
        GPendingFinally := TList<TPendingFinallyEntry>.Create;
      FillChar(PendingEntry, SizeOf(PendingEntry), 0);
      PendingEntry.FinallyBlock := nil;
      PendingEntry.UsingErrorReg := ErrorReg;
      GPendingFinally.Add(PendingEntry);
    end;

    try
      if Assigned(AStmt.Init) then
        ACtx.CompileStatement(AStmt.Init);

      BeginLoopControl(ACtx, LoopControl);

      try
        if HasLexicalInit and not UseSharedLexicalFor then
      begin
        SetLength(OuterSlots, PerIterNames.Count);
        SetLength(BodySlots, PerIterNames.Count);
        SetLength(UpdateSlots, PerIterNames.Count);
        SetLength(OuterLocalIdxs, PerIterNames.Count);
        for I := 0 to PerIterNames.Count - 1 do
        begin
          Name := PerIterNames[I];
          OuterLocalIdxs[I] := ACtx.Scope.ResolveLocal(Name);
          OuterSlots[I] := ACtx.Scope.GetLocal(OuterLocalIdxs[I]).Slot;
        end;

        LoopStart := CurrentCodePosition(ACtx);
        ExitJump := -1;

        // ES2026 §14.7.4.4 step 2: create the per-iteration environment
        // before evaluating the test and body.
        ACtx.Scope.BeginScope;
        SetLoopContinueScopeDepth(ACtx);
        SetLabeledContinueCleanupBase(AStmt);
        for I := 0 to PerIterNames.Count - 1 do
        begin
          Name := PerIterNames[I];
          BodySlots[I] := ACtx.Scope.DeclareLocal(Name, PerIterIsConst);
          LocalIdx := ACtx.Scope.ResolveLocal(Name);
          CopyLocalTypeMetadata(ACtx, OuterLocalIdxs[I], LocalIdx);
          EmitInstruction(ACtx,
            EncodeABC(OP_MOVE, BodySlots[I], OuterSlots[I], 0));
        end;

        if Assigned(AStmt.Condition) then
        begin
          CondReg := ACtx.Scope.AllocateRegister;
          try
            ACtx.CompileExpression(AStmt.Condition, CondReg);
            ExitJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, CondReg);
          finally
            ACtx.Scope.FreeRegister;
          end;
        end;

        ACtx.CompileStatement(AStmt.Body);

        PatchJumpList(ACtx, LoopControl.ContinueJumps);
        PatchLabeledContinueJumps(ACtx, AStmt);

        // BodySlots are no longer visible to name resolution after EndScope,
        // but their registers still hold the body iteration values for the
        // update environment snapshot.
        ACtx.Scope.EndScope(BodyClosedLocals, BodyClosedCount);
        for I := 0 to BodyClosedCount - 1 do
          EmitInstruction(ACtx,
            EncodeABC(OP_CLOSE_UPVALUE, BodyClosedLocals[I], 0, 0));

        // ES2026 §14.7.4.4 step 3.e: create a fresh per-iteration
        // environment for the update expression, distinct from the body
        // iteration environment that closures in the body captured.
        ACtx.Scope.BeginScope;
        for I := 0 to PerIterNames.Count - 1 do
        begin
          Name := PerIterNames[I];
          UpdateSlots[I] := ACtx.Scope.DeclareLocal(Name, PerIterIsConst);
          LocalIdx := ACtx.Scope.ResolveLocal(Name);
          CopyLocalTypeMetadata(ACtx, OuterLocalIdxs[I], LocalIdx);
          EmitInstruction(ACtx,
            EncodeABC(OP_MOVE, UpdateSlots[I], BodySlots[I], 0));
        end;

        if Assigned(AStmt.Update) then
          CompileDiscardedExpression(ACtx, AStmt.Update);

        for I := 0 to PerIterNames.Count - 1 do
          EmitInstruction(ACtx,
            EncodeABC(OP_MOVE, OuterSlots[I], UpdateSlots[I], 0));

        ACtx.Scope.EndScope(UpdateClosedLocals, UpdateClosedCount);
        for I := 0 to UpdateClosedCount - 1 do
          EmitInstruction(ACtx,
            EncodeABC(OP_CLOSE_UPVALUE, UpdateClosedLocals[I], 0, 0));

        EmitInstruction(ACtx,
          EncodeAx(OP_JUMP, LoopStart - CurrentCodePosition(ACtx) - 1));

        if ExitJump >= 0 then
        begin
          PatchJumpTarget(ACtx, ExitJump);
          for I := 0 to BodyClosedCount - 1 do
            EmitInstruction(ACtx,
              EncodeABC(OP_CLOSE_UPVALUE, BodyClosedLocals[I], 0, 0));
        end;

        PatchJumpList(ACtx, LoopControl.BreakJumps);
      end
      else
      begin
        LoopStart := CurrentCodePosition(ACtx);
        ExitJump := -1;

        if Assigned(AStmt.Condition) then
        begin
          CondReg := ACtx.Scope.AllocateRegister;
          try
            ACtx.CompileExpression(AStmt.Condition, CondReg);
            ExitJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, CondReg);
          finally
            ACtx.Scope.FreeRegister;
          end;
        end;

        ACtx.Scope.BeginScope;
        SetLoopContinueScopeDepth(ACtx);
        SetLabeledContinueCleanupBase(AStmt);

        ACtx.CompileStatement(AStmt.Body);

        PatchJumpList(ACtx, LoopControl.ContinueJumps);
        PatchLabeledContinueJumps(ACtx, AStmt);

        ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
        for I := 0 to ClosedCount - 1 do
          EmitInstruction(ACtx,
            EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));

        if Assigned(AStmt.Update) then
          CompileDiscardedExpression(ACtx, AStmt.Update);

        EmitInstruction(ACtx,
          EncodeAx(OP_JUMP, LoopStart - CurrentCodePosition(ACtx) - 1));

        if ExitJump >= 0 then
          PatchJumpTarget(ACtx, ExitJump);

        PatchJumpList(ACtx, LoopControl.BreakJumps);
      end;
    finally
      EndLoopControl(LoopControl);
    end;

      if HasUsingInit then
      begin
        GPendingFinally.Delete(GPendingFinally.Count - 1);

        EmitInstruction(ACtx, EncodeABC(OP_POP_HANDLER, 0, 0, 0));

        ResourceCount := GUsingResources.Count - SavedResourceBase;
        SetLength(SavedResources, ResourceCount);
        for I := 0 to ResourceCount - 1 do
          SavedResources[I] := GUsingResources[SavedResourceBase + I];

        for I := GUsingResources.Count - 1 downto SavedResourceBase do
          GUsingResources.Delete(I);

        EmitDisposalSequence(ACtx, SavedResources, ResourceCount, ErrorReg);
        NullishJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH,
          ErrorReg, GOCCIA_NULLISH_MATCH_HOLE);
        EmitInstruction(ACtx, EncodeABC(OP_THROW, ErrorReg, 0, 0));
        PatchJumpTarget(ACtx, NullishJump);

        EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);

        PatchJumpTarget(ACtx, HandlerJump);
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, ErrorReg, CatchReg, 0));
        EmitDisposalSequence(ACtx, SavedResources, ResourceCount, ErrorReg);
        EmitInstruction(ACtx, EncodeABC(OP_THROW, ErrorReg, 0, 0));

        PatchJumpTarget(ACtx, EndJump);

        for I := ResourceCount - 1 downto 0 do
          ACtx.Scope.FreeRegister;
        ACtx.Scope.FreeRegister; // ErrorReg
        ACtx.Scope.FreeRegister; // CatchReg
      end;
    except
      if HasUsingInit and Assigned(GPendingFinally) and
         (GPendingFinally.Count > 0) then
        GPendingFinally.Delete(GPendingFinally.Count - 1);
      raise;
    end;

    if HasLexicalInit then
    begin
      ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
      for I := 0 to ClosedCount - 1 do
        EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));
    end;
  finally
    if Assigned(PerIterNames) then
      PerIterNames.Free;
  end;
end;

// ES2026 §14.7.3.2 Runtime Semantics: WhileLoopEvaluation
procedure CompileWhileStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaWhileStatement);
var
  CondReg: UInt8;
  LoopStart, ExitJump: Integer;
  LoopControl: TLoopControlState;
begin
  BeginLoopControl(ACtx, LoopControl);
  try
    SetLabeledContinueCleanupBase(AStmt);
    LoopStart := CurrentCodePosition(ACtx);
    CondReg := ACtx.Scope.AllocateRegister;
    try
      ACtx.CompileExpression(AStmt.Condition, CondReg);
      ExitJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, CondReg);
    finally
      ACtx.Scope.FreeRegister;
    end;

    ACtx.CompileStatement(AStmt.Body);

    PatchJumpList(ACtx, LoopControl.ContinueJumps);
    PatchLabeledContinueJumps(ACtx, AStmt);

    EmitInstruction(ACtx,
      EncodeAx(OP_JUMP, LoopStart - CurrentCodePosition(ACtx) - 1));

    PatchJumpTarget(ACtx, ExitJump);

    PatchJumpList(ACtx, LoopControl.BreakJumps);
  finally
    EndLoopControl(LoopControl);
  end;
end;

// ES2026 §14.7.2.2 Runtime Semantics: DoWhileLoopEvaluation
procedure CompileDoWhileStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaDoWhileStatement);
var
  CondReg: UInt8;
  LoopStart: Integer;
  LoopControl: TLoopControlState;
begin
  BeginLoopControl(ACtx, LoopControl);
  try
    SetLabeledContinueCleanupBase(AStmt);
    LoopStart := CurrentCodePosition(ACtx);

    ACtx.CompileStatement(AStmt.Body);

    PatchJumpList(ACtx, LoopControl.ContinueJumps);
    PatchLabeledContinueJumps(ACtx, AStmt);

    CondReg := ACtx.Scope.AllocateRegister;
    try
      ACtx.CompileExpression(AStmt.Condition, CondReg);
      EmitInstruction(ACtx,
        EncodeAsBx(OP_JUMP_IF_TRUE, CondReg,
          LoopStart - CurrentCodePosition(ACtx) - 1));
    finally
      ACtx.Scope.FreeRegister;
    end;

    PatchJumpList(ACtx, LoopControl.BreakJumps);
  finally
    EndLoopControl(LoopControl);
  end;
end;

procedure CompileImportDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaImportDeclaration);
var
  ModReg: UInt8;
  NamespaceSlot: UInt8;
  PathIdx, NameIdx: UInt16;
  Pair: TStringStringMap.TKeyValuePair;
  Slots: array of UInt8;
  Names: array of string;
  EncodedPath: string;
  HasNamespace: Boolean;
  I, Count: Integer;

  function ImportSlot(const AName: string): UInt8;
  var
    LocalIdx: Integer;
  begin
    LocalIdx := ACtx.Scope.ResolveLocal(AName);
    if (LocalIdx >= 0) and
       (ACtx.Scope.GetLocal(LocalIdx).Depth = ACtx.Scope.Depth) then
      Exit(ACtx.Scope.GetLocal(LocalIdx).Slot);
    Result := ACtx.Scope.DeclareLocal(AName, True);
  end;

  procedure MarkImportSlot(const AName: string; const AExportName: string);
  var
    LocalIdx: Integer;
  begin
    LocalIdx := ACtx.Scope.ResolveLocal(AName);
    if LocalIdx >= 0 then
      ACtx.Scope.MarkImportBinding(LocalIdx, AStmt.Phase, EncodedPath,
        AExportName);
  end;
begin
  HasNamespace := AStmt.NamespaceName <> '';
  Count := AStmt.Imports.Count;
  SetLength(Slots, Count);
  SetLength(Names, Count);
  EncodedPath := EncodeImportSpecifierAttribute(AStmt.ModulePath,
    AStmt.AttributeType);

  if HasNamespace then
  begin
    NamespaceSlot := ImportSlot(AStmt.NamespaceName);
    MarkImportSlot(AStmt.NamespaceName, '');
  end;

  I := 0;
  for Pair in AStmt.Imports do
  begin
    Slots[I] := ImportSlot(Pair.Key);
    Names[I] := Pair.Value;
    if AStmt.Phase = icpEvaluation then
      MarkImportSlot(Pair.Key, Pair.Value)
    else
      MarkImportSlot(Pair.Key, '');
    Inc(I);
  end;

  ModReg := ACtx.Scope.AllocateRegister;
  PathIdx := ACtx.Template.AddConstantString(EncodedPath);
  if AStmt.Phase = icpSource then
    EmitInstruction(ACtx, EncodeABx(OP_IMPORT_SOURCE, ModReg, PathIdx))
  else if AStmt.Phase = icpDefer then
    EmitInstruction(ACtx, EncodeABx(OP_IMPORT_DEFER, ModReg, PathIdx))
  else
    EmitInstruction(ACtx, EncodeABx(OP_IMPORT, ModReg, PathIdx));

  if HasNamespace then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, NamespaceSlot, ModReg, 0));

  for I := 0 to Count - 1 do
  begin
    if AStmt.Phase = icpSource then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, Slots[I], ModReg, 0))
    else if AStmt.Phase <> icpEvaluation then
    begin
      NameIdx := ACtx.Template.AddConstantString(Names[I]);
      if NameIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: import name index exceeds 255');
      EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, Slots[I], ModReg,
        UInt8(NameIdx)));
    end;
  end;

  ACtx.Scope.FreeRegister;
end;

procedure CompileExportDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExportDeclaration);
var
  Pair: TStringStringMap.TKeyValuePair;
  LocalIdx: Integer;
  Local: TGocciaCompilerLocal;
  Reg: UInt8;
  NameIdx, SourceNameIdx: UInt16;
begin
  for Pair in AStmt.ExportsTable do
  begin
    LocalIdx := ACtx.Scope.ResolveLocal(Pair.Value);
    if LocalIdx >= 0 then
    begin
      Local := ACtx.Scope.GetLocal(LocalIdx);
      if Local.IsImportBinding then
        Continue;
      if Local.IsGlobalBacked then
      begin
        Reg := ACtx.Scope.AllocateRegister;
        SourceNameIdx := ACtx.Template.AddConstantString(Pair.Value);
        EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, Reg, SourceNameIdx));
      end
      else
        Reg := Local.Slot;
      NameIdx := ACtx.Template.AddConstantString(Pair.Key);
      EmitInstruction(ACtx, EncodeABx(OP_EXPORT, Reg, NameIdx));
      if Local.IsGlobalBacked then
        ACtx.Scope.FreeRegister;
    end;
  end;
end;

procedure CompileExportDefaultDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExportDefaultDeclaration);
var
  Slot: UInt8;
  LocalIdx, FuncCount: Integer;
  InferredTemplate: TGocciaFunctionTemplate;
  NameIdx: UInt16;
  BindingName: string;
  IsNamedDefaultFunction: Boolean;
  IsNamedDefaultClass: Boolean;
begin
  BindingName := AStmt.LocalName;
  IsNamedDefaultFunction := (BindingName <> GOCCIA_DEFAULT_EXPORT_BINDING) and
    (AStmt.Expression is TGocciaFunctionExpression) and
    (TGocciaFunctionExpression(AStmt.Expression).Name = BindingName);
  IsNamedDefaultClass := (BindingName <> GOCCIA_DEFAULT_EXPORT_BINDING) and
    (AStmt.Expression is TGocciaClassExpression) and
    (TGocciaClassExpression(AStmt.Expression).ClassDefinition.Name =
    BindingName);
  LocalIdx := ACtx.Scope.ResolveLocal(BindingName);
  if (LocalIdx >= 0) and
     (ACtx.Scope.GetLocal(LocalIdx).Depth = ACtx.Scope.Depth) then
    Slot := ACtx.Scope.GetLocal(LocalIdx).Slot
  else
    Slot := ACtx.Scope.DeclareLocal(BindingName,
      not (IsNamedDefaultFunction or IsNamedDefaultClass));

  if ACtx.GlobalBackedTopLevel and (ACtx.Scope.Depth = 0) then
  begin
    LocalIdx := ACtx.Scope.ResolveLocal(BindingName);
    if LocalIdx >= 0 then
      ACtx.Scope.MarkGlobalBacked(LocalIdx);
  end;

  FuncCount := ACtx.Template.FunctionCount;
  if (AStmt.Expression is TGocciaClassExpression) and
     (TGocciaClassExpression(AStmt.Expression).ClassDefinition.Name = '') then
    CompileClassExpression(ACtx,
      TGocciaClassExpression(AStmt.Expression).ClassDefinition, Slot,
      KEYWORD_DEFAULT)
  else if IsNamedDefaultFunction then
    CompileFunctionExpression(ACtx, TGocciaFunctionExpression(AStmt.Expression),
      Slot, BindingName, False)
  else
    ACtx.CompileExpression(AStmt.Expression, Slot);

  if (BindingName = GOCCIA_DEFAULT_EXPORT_BINDING) and
     ((AStmt.Expression is TGocciaArrowFunctionExpression) or
     ((AStmt.Expression is TGocciaFunctionExpression) and
     (TGocciaFunctionExpression(AStmt.Expression).Name = ''))) then
  begin
    if ACtx.Template.FunctionCount > FuncCount then
    begin
      InferredTemplate := ACtx.Template.GetFunction(
        ACtx.Template.FunctionCount - 1);
      if (InferredTemplate.Name = '<arrow>') or
         (InferredTemplate.Name = '<function>') or
         (InferredTemplate.Name = '<method>') then
        InferredTemplate.Name := KEYWORD_DEFAULT;
    end;
  end;

  LocalIdx := ACtx.Scope.ResolveLocal(BindingName);
  if (LocalIdx >= 0) and ACtx.Scope.GetLocal(LocalIdx).IsCaptured then
    EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, Slot, UInt16(Slot)));

  if ACtx.GlobalBackedTopLevel and (ACtx.Scope.Depth = 0) then
    EmitGlobalDefine(ACtx, Slot, BindingName,
      not (IsNamedDefaultFunction or IsNamedDefaultClass));

  NameIdx := ACtx.Template.AddConstantString(KEYWORD_DEFAULT);
  EmitInstruction(ACtx, EncodeABx(OP_EXPORT, Slot, NameIdx));
end;

procedure CompileExportVariableDeclaration(
  const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExportVariableDeclaration);
var
  I: Integer;
  VarInfo: TGocciaVariableInfo;
  LocalIdx: Integer;
  Reg: UInt8;
  NameIdx: UInt16;
begin
  CompileVariableDeclaration(ACtx, AStmt.Declaration);

  for I := 0 to Length(AStmt.Declaration.Variables) - 1 do
  begin
    VarInfo := AStmt.Declaration.Variables[I];
    LocalIdx := ACtx.Scope.ResolveLocal(VarInfo.Name);
    if LocalIdx >= 0 then
    begin
      Reg := ACtx.Scope.GetLocal(LocalIdx).Slot;
      NameIdx := ACtx.Template.AddConstantString(VarInfo.Name);
      EmitInstruction(ACtx, EncodeABx(OP_EXPORT, Reg, NameIdx));
    end;
  end;
end;

procedure CompileExportDestructuringDeclaration(
  const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExportDestructuringDeclaration);
var
  LocalIdx: Integer;
  Name: string;
  NameIdx: UInt16;
  Names: TStringList;
  Reg: UInt8;
begin
  CompileDestructuringDeclaration(ACtx, AStmt.Declaration);

  Names := TStringList.Create;
  try
    Names.CaseSensitive := True;
    CollectPatternBindingNames(AStmt.Declaration.Pattern, Names, True);
    for Name in Names do
    begin
      LocalIdx := ACtx.Scope.ResolveLocal(Name);
      if LocalIdx >= 0 then
      begin
        Reg := ACtx.Scope.GetLocal(LocalIdx).Slot;
        NameIdx := ACtx.Template.AddConstantString(Name);
        EmitInstruction(ACtx, EncodeABx(OP_EXPORT, Reg, NameIdx));
      end;
    end;
  finally
    Names.Free;
  end;
end;

procedure CompileExportFunctionDeclaration(
  const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExportFunctionDeclaration);
var
  LocalIdx: Integer;
  Reg: UInt8;
  NameIdx: UInt16;
begin
  CompileFunctionDeclaration(ACtx, AStmt.Declaration);

  LocalIdx := ACtx.Scope.ResolveLocal(AStmt.Declaration.Name);
  if LocalIdx >= 0 then
  begin
    Reg := ACtx.Scope.GetLocal(LocalIdx).Slot;
    NameIdx := ACtx.Template.AddConstantString(AStmt.Declaration.Name);
    EmitInstruction(ACtx, EncodeABx(OP_EXPORT, Reg, NameIdx));
  end;
end;

procedure CompileExportClassDeclaration(
  const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExportClassDeclaration);
var
  LocalIdx: Integer;
  Reg: UInt8;
  NameIdx: UInt16;
begin
  CompileClassDeclaration(ACtx, AStmt.Declaration);

  LocalIdx := ACtx.Scope.ResolveLocal(AStmt.Declaration.ClassDefinition.Name);
  if LocalIdx >= 0 then
  begin
    Reg := ACtx.Scope.GetLocal(LocalIdx).Slot;
    NameIdx := ACtx.Template.AddConstantString(
      AStmt.Declaration.ClassDefinition.Name);
    EmitInstruction(ACtx, EncodeABx(OP_EXPORT, Reg, NameIdx));
  end;
end;

procedure CompileReExportDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaReExportDeclaration);
begin
  // Re-exports are link-time module graph declarations. The module loader
  // registers their forwardings before evaluation, so bytecode must not
  // snapshot them with OP_EXPORT during execution.
end;

procedure CompileSwitchStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaSwitchStatement);
var
  DiscReg, TestReg, CmpReg: UInt8;
  I, J, DefaultIndex: Integer;
  CaseClause: TGocciaCaseClause;
  CaseBodyJumps: array of Integer;
  DefaultJump, EndJump: Integer;
  OldBreakJumps: TList<Integer>;
  OldBreakFinallyBase: Integer;
  OldBreakScopeDepth: Integer;
  BreakJumps: TList<Integer>;
  Node: TGocciaASTNode;
  Reg: UInt8;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount: Integer;
  HasFunctionDecl, NeedsPrelude, StatementAbrupt, HasUsing: Boolean;
  SavedResourceBase, ResourceCount: Integer;
  CatchReg, ErrorReg: UInt8;
  HandlerJump, DisposalEndJump, NullishJump: Integer;
  SavedResources: array of TUsingResourceEntry;
  PendingEntry: TPendingFinallyEntry;
  PreallocatedUsingDisposeSlots, OldPreallocatedUsingDisposeSlots:
    TList<TPreallocatedUsingDisposeSlot>;

  procedure BeginSwitchUsingDisposalRegion;
  begin
    if not HasUsing then
      Exit;

    if not Assigned(GUsingResources) then
      GUsingResources := TList<TUsingResourceEntry>.Create;
    SavedResourceBase := GUsingResources.Count;

    CatchReg := ACtx.Scope.AllocateRegister;
    ErrorReg := ACtx.Scope.AllocateRegister;
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_HOLE, ErrorReg, 0, 0));
    HandlerJump := EmitJumpInstruction(ACtx, OP_PUSH_HANDLER, CatchReg);

    if not Assigned(GPendingFinally) then
      GPendingFinally := TList<TPendingFinallyEntry>.Create;
    FillChar(PendingEntry, SizeOf(PendingEntry), 0);
    PendingEntry.FinallyBlock := nil;
    PendingEntry.UsingErrorReg := ErrorReg;
    GPendingFinally.Add(PendingEntry);
  end;

  procedure RegisterSwitchUsingResources;
  var
    CaseIdx, ConsequentIdx, VarIdx, LocalIdx: Integer;
    UsingDecl: TGocciaUsingDeclaration;
    Entry: TUsingResourceEntry;
    Preallocated: TPreallocatedUsingDisposeSlot;
  begin
    if not HasUsing then
      Exit;

    PreallocatedUsingDisposeSlots := TList<TPreallocatedUsingDisposeSlot>.Create;
    OldPreallocatedUsingDisposeSlots := GPreallocatedUsingDisposeSlots;
    GPreallocatedUsingDisposeSlots := PreallocatedUsingDisposeSlots;

    for CaseIdx := 0 to AStmt.Cases.Count - 1 do
    begin
      CaseClause := AStmt.Cases[CaseIdx];
      for ConsequentIdx := 0 to CaseClause.Consequent.Count - 1 do
      begin
        if not (CaseClause.Consequent[ConsequentIdx] is TGocciaUsingDeclaration) then
          Continue;

        UsingDecl := TGocciaUsingDeclaration(CaseClause.Consequent[ConsequentIdx]);
        for VarIdx := 0 to High(UsingDecl.Variables) do
        begin
          LocalIdx := ACtx.Scope.ResolveLocal(UsingDecl.Variables[VarIdx].Name);
          if LocalIdx < 0 then
            Continue;

          Entry.ValueSlot := ACtx.Scope.GetLocal(LocalIdx).Slot;
          Entry.DisposeSlot := ACtx.Scope.AllocateRegister;
          Entry.IsAwait := UsingDecl.IsAwait;
          EmitInstruction(ACtx, EncodeABC(OP_LOAD_NULL, Entry.DisposeSlot, 0, 0));
          RegisterUsingResourceForDisposal(Entry);

          Preallocated.Declaration := UsingDecl;
          Preallocated.VariableIndex := VarIdx;
          Preallocated.DisposeSlot := Entry.DisposeSlot;
          Preallocated.ResourceRegistered := True;
          PreallocatedUsingDisposeSlots.Add(Preallocated);
        end;
      end;
    end;
  end;
begin
  PreallocatedUsingDisposeSlots := nil;
  OldPreallocatedUsingDisposeSlots := nil;
  DiscReg := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(AStmt.Discriminant, DiscReg);

  SetLength(CaseBodyJumps, AStmt.Cases.Count);
  DefaultIndex := -1;
  DefaultJump := -1;
  EndJump := -1;

  OldBreakJumps := GBreakJumps;
  OldBreakFinallyBase := GBreakFinallyBase;
  OldBreakScopeDepth := GBreakScopeDepth;
  BreakJumps := TList<Integer>.Create;
  GBreakJumps := BreakJumps;
  if Assigned(GPendingFinally) then
    GBreakFinallyBase := GPendingFinally.Count
  else
    GBreakFinallyBase := 0;
  try
    HasFunctionDecl := False;
    NeedsPrelude := False;
    HasUsing := False;
    for I := 0 to AStmt.Cases.Count - 1 do
    begin
      CaseClause := AStmt.Cases[I];
      for J := 0 to CaseClause.Consequent.Count - 1 do
      begin
        Node := CaseClause.Consequent[J];
        if NeedsBlockLexicalPredeclaration(Node) then
          NeedsPrelude := True;
        if Node is TGocciaUsingDeclaration then
          HasUsing := True;
        if GetBlockFunctionDeclaration(Node) <> nil then
          HasFunctionDecl := True;
      end;
    end;

    ACtx.Scope.BeginScope;
    GBreakScopeDepth := ACtx.Scope.Depth;

    if NeedsPrelude then
    begin
      for I := 0 to AStmt.Cases.Count - 1 do
      begin
        CaseClause := AStmt.Cases[I];
        for J := 0 to CaseClause.Consequent.Count - 1 do
          PredeclareBlockLexicalLocals(CaseClause.Consequent[J], ACtx, False);
      end;

      for I := 0 to AStmt.Cases.Count - 1 do
      begin
        CaseClause := AStmt.Cases[I];
        for J := 0 to CaseClause.Consequent.Count - 1 do
          EmitBlockLexicalHoleInitializers(CaseClause.Consequent[J], ACtx);
      end;

      for I := 0 to AStmt.Cases.Count - 1 do
      begin
        CaseClause := AStmt.Cases[I];
        for J := 0 to CaseClause.Consequent.Count - 1 do
          if HasFunctionDecl and
             (GetBlockFunctionDeclaration(CaseClause.Consequent[J]) <> nil) then
            ACtx.CompileStatement(CaseClause.Consequent[J]);
      end;
    end;

    if HasUsing then
    begin
      BeginSwitchUsingDisposalRegion;
      RegisterSwitchUsingResources;
    end;

    TestReg := ACtx.Scope.AllocateRegister;
    CmpReg := ACtx.Scope.AllocateRegister;
    for I := 0 to AStmt.Cases.Count - 1 do
    begin
      CaseClause := AStmt.Cases[I];
      if not Assigned(CaseClause.Test) then
      begin
        DefaultIndex := I;
        CaseBodyJumps[I] := -1;
        Continue;
      end;
      ACtx.CompileExpression(CaseClause.Test, TestReg);
      EmitInstruction(ACtx, EncodeABC(OP_EQ, CmpReg, DiscReg, TestReg));
      CaseBodyJumps[I] := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, CmpReg);
    end;

    if DefaultIndex >= 0 then
      DefaultJump := EmitJumpInstruction(ACtx, OP_JUMP, 0)
    else
      EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;

    for I := 0 to AStmt.Cases.Count - 1 do
    begin
      CaseClause := AStmt.Cases[I];

      if I = DefaultIndex then
      begin
        if DefaultJump >= 0 then
          PatchJumpTarget(ACtx, DefaultJump);
      end
      else
      begin
        if CaseBodyJumps[I] >= 0 then
          PatchJumpTarget(ACtx, CaseBodyJumps[I]);
      end;

      for J := 0 to CaseClause.Consequent.Count - 1 do
      begin
        Node := CaseClause.Consequent[J];
        if GetBlockFunctionDeclaration(Node) <> nil then
        begin
          CompileAnnexBBlockFunctionActivation(ACtx,
            GetBlockFunctionDeclaration(Node));
          Continue;
        end;
        if Node is TGocciaStatement then
        begin
          StatementAbrupt := ACtx.CompileStatement(TGocciaStatement(Node));
          if ACtx.OptimizationOptions.EnableDeadBranchElimination and
             not ACtx.OptimizationOptions.PreserveCoverageShape and
             StatementAbrupt then
            Break;
        end
        else if Node is TGocciaExpression then
        begin
          Reg := ACtx.Scope.AllocateRegister;
          ACtx.CompileExpression(TGocciaExpression(Node), Reg);
          ACtx.Scope.FreeRegister;
        end;
      end;
    end;

    if HasUsing and (EndJump >= 0) then
    begin
      PatchJumpTarget(ACtx, EndJump);
      EndJump := -1;
    end;

    if HasUsing then
    begin
      GPendingFinally.Delete(GPendingFinally.Count - 1);
      EmitInstruction(ACtx, EncodeABC(OP_POP_HANDLER, 0, 0, 0));

      ResourceCount := GUsingResources.Count - SavedResourceBase;
      SetLength(SavedResources, ResourceCount);
      for I := 0 to ResourceCount - 1 do
        SavedResources[I] := GUsingResources[SavedResourceBase + I];

      for I := GUsingResources.Count - 1 downto SavedResourceBase do
        GUsingResources.Delete(I);

      EmitDisposalSequence(ACtx, SavedResources, ResourceCount, ErrorReg);
      NullishJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH, ErrorReg,
        GOCCIA_NULLISH_MATCH_HOLE);
      EmitInstruction(ACtx, EncodeABC(OP_THROW, ErrorReg, 0, 0));
      PatchJumpTarget(ACtx, NullishJump);

      DisposalEndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);

      PatchJumpTarget(ACtx, HandlerJump);
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ErrorReg, CatchReg, 0));
      EmitDisposalSequence(ACtx, SavedResources, ResourceCount, ErrorReg);
      EmitInstruction(ACtx, EncodeABC(OP_THROW, ErrorReg, 0, 0));

      PatchJumpTarget(ACtx, DisposalEndJump);

      for I := ResourceCount - 1 downto 0 do
        ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
    end;
    if EndJump >= 0 then
      PatchJumpTarget(ACtx, EndJump);

    for I := 0 to BreakJumps.Count - 1 do
      PatchJumpTarget(ACtx, BreakJumps[I]);

    ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
    for J := 0 to ClosedCount - 1 do
      EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[J], 0, 0));
  finally
    if Assigned(PreallocatedUsingDisposeSlots) then
    begin
      if GPreallocatedUsingDisposeSlots = PreallocatedUsingDisposeSlots then
        GPreallocatedUsingDisposeSlots := OldPreallocatedUsingDisposeSlots;
      PreallocatedUsingDisposeSlots.Free;
    end;
    BreakJumps.Free;
    GBreakJumps := OldBreakJumps;
    GBreakFinallyBase := OldBreakFinallyBase;
    GBreakScopeDepth := OldBreakScopeDepth;
  end;
  ACtx.Scope.FreeRegister;
end;

procedure EmitLoopControlCleanup(const ACtx: TGocciaCompilationContext;
  const AFinallyBase, AScopeDepth: Integer; const AIsBreak: Boolean);
var
  I, Count, Base: Integer;
  Entries: array of TPendingFinallyEntry;
  Entry: TPendingFinallyEntry;
  Local: TGocciaCompilerLocal;
begin
  if Assigned(GPendingFinally) and (GPendingFinally.Count > AFinallyBase) then
  begin
    Count := GPendingFinally.Count;
    Base := AFinallyBase;
    SetLength(Entries, Count - Base);
    for I := Base to Count - 1 do
      Entries[I - Base] := GPendingFinally[I];
    for I := Count - 1 downto Base do
    begin
      if GPendingFinally.Count > I then
        GPendingFinally.Delete(I);
      Entry := Entries[I - Base];
      EmitPendingEntryCleanup(ACtx, Entry, AIsBreak);
    end;
    for I := 0 to Length(Entries) - 1 do
      GPendingFinally.Insert(Base + I, Entries[I]);
  end;

  for I := ACtx.Scope.LocalCount - 1 downto 0 do
  begin
    Local := ACtx.Scope.GetLocal(I);
    if Local.Depth <= AScopeDepth then
      Break;
    if Local.IsCaptured then
      EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, Local.Slot, 0, 0));
  end;
end;

function CompileLabeledStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaStatement): Boolean;
var
  CreatedControls: array of TLabelControlState;
  State: TLabelControlState;
  StartIndex, I: Integer;
begin
  if not Assigned(GLabelControls) then
    GLabelControls := TList<TLabelControlState>.Create;

  StartIndex := GLabelControls.Count;
  SetLength(CreatedControls, AStmt.LabelCount);
  for I := 0 to AStmt.LabelCount - 1 do
  begin
    State.LabelName := AStmt.Labels[I];
    State.BreakJumps := TList<Integer>.Create;
    State.ContinueJumps := TList<Integer>.Create;
    State.BreakFinallyBase := CurrentPendingFinallyBase;
    State.ContinueFinallyBase := CurrentPendingFinallyBase;
    State.BreakScopeDepth := ACtx.Scope.Depth;
    State.ContinueScopeDepth := ACtx.Scope.Depth;
    State.IsIteration := StatementIsIteration(AStmt);
    CreatedControls[I] := State;
    GLabelControls.Add(State);
  end;

  try
    ACtx.CompileStatement(AStmt);
    for I := StartIndex to GLabelControls.Count - 1 do
      PatchJumpList(ACtx, GLabelControls[I].BreakJumps);
    Result := False;
  finally
    for I := GLabelControls.Count - 1 downto StartIndex do
      GLabelControls.Delete(I);
    for I := 0 to Length(CreatedControls) - 1 do
    begin
      CreatedControls[I].BreakJumps.Free;
      CreatedControls[I].ContinueJumps.Free;
    end;
  end;
end;

procedure CompileBreakStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaBreakStatement);
var
  LabelIndex: Integer;
  State: TLabelControlState;
begin
  if AStmt.TargetLabel <> '' then
  begin
    LabelIndex := FindLabelControlIndex(AStmt.TargetLabel);
    if LabelIndex < 0 then
      Exit;
    State := GLabelControls[LabelIndex];
    EmitLoopControlCleanup(ACtx, State.BreakFinallyBase,
      State.BreakScopeDepth, True);
    State.BreakJumps.Add(EmitJumpInstruction(ACtx, OP_JUMP, 0));
    GLabelControls[LabelIndex] := State;
    Exit;
  end;

  if not Assigned(GBreakJumps) then
    Exit;

  EmitLoopControlCleanup(ACtx, GBreakFinallyBase, GBreakScopeDepth, True);
  GBreakJumps.Add(EmitJumpInstruction(ACtx, OP_JUMP, 0));
end;

procedure CompileContinueStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaContinueStatement);
var
  LabelIndex: Integer;
  State: TLabelControlState;
begin
  if AStmt.TargetLabel <> '' then
  begin
    LabelIndex := FindLabelControlIndex(AStmt.TargetLabel);
    if (LabelIndex < 0) or not GLabelControls[LabelIndex].IsIteration then
      Exit;
    State := GLabelControls[LabelIndex];
    EmitLoopControlCleanup(ACtx, State.ContinueFinallyBase,
      State.ContinueScopeDepth, True);
    State.ContinueJumps.Add(EmitJumpInstruction(ACtx, OP_JUMP, 0));
    GLabelControls[LabelIndex] := State;
    Exit;
  end;

  if not Assigned(GContinueJumps) then
    Exit;

  EmitLoopControlCleanup(ACtx, GContinueFinallyBase, GContinueScopeDepth, False);
  GContinueJumps.Add(EmitJumpInstruction(ACtx, OP_JUMP, 0));
end;

function DisplayClassElementName(const AStorageName: string): string;
var
  I, SeparatorIndex: Integer;
begin
  if Pos('#slot:', AStorageName) = 1 then
  begin
    SeparatorIndex := 0;
    for I := Length('#slot:') + 1 to Length(AStorageName) do
      if AStorageName[I] = '$' then
      begin
        SeparatorIndex := I;
        Break;
      end;
    if SeparatorIndex > 0 then
      Exit('#' + Copy(AStorageName, SeparatorIndex + 1, MaxInt));
  end;
  Result := AStorageName;
end;

procedure CompileMethodBody(const ACtx: TGocciaCompilationContext;
  const AClassReg: UInt8; const AMethodName: string;
  const AMethod: TGocciaClassMethod; const AStoreOpcode: TGocciaOpCode;
  const AGuardDerivedThis: Boolean = False);
var
  OldTemplate: TGocciaFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TGocciaFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  ChildCtx: TGocciaCompilationContext;
  FuncIdx: UInt16;
  MethodReg: UInt8;
  MethodNameIdx: UInt16;
  FormalCount, RestParamIndex, I: Integer;
  ArgumentsSlot: Integer;
  DisplayName: string;
  OldDerivedGuard: Boolean;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;
  OldDerivedGuard := ACtx.DerivedConstructorThisGuard;
  DisplayName := DisplayClassElementName(AMethodName);

  ChildTemplate := TGocciaFunctionTemplate.Create(DisplayName);
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.IsAsync := AMethod.IsAsync;
  ChildTemplate.IsGenerator := AMethod.IsGenerator;
  ChildTemplate.SourceText := AMethod.SourceText;
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);

  ChildScope.DeclareLocal(KEYWORD_THIS, False);
  ChildTemplate.ParameterCount := Length(AMethod.Parameters);

  FormalCount := -1;
  RestParamIndex := -1;
  for I := 0 to High(AMethod.Parameters) do
  begin
    if AMethod.Parameters[I].IsRest or
       Assigned(AMethod.Parameters[I].DefaultValue) then
    begin
      if FormalCount < 0 then
        FormalCount := I;
      if AMethod.Parameters[I].IsRest then
        RestParamIndex := I;
    end;
    if AMethod.Parameters[I].IsPattern then
      ChildScope.DeclareLocal(SyntheticParamLocalName(I), False)
    else
      ChildScope.DeclareLocal(AMethod.Parameters[I].Name, False);
  end;
  for I := 0 to High(AMethod.Parameters) do
    if AMethod.Parameters[I].IsPattern and Assigned(AMethod.Parameters[I].Pattern) then
      CollectDestructuringBindings(AMethod.Parameters[I].Pattern, ChildScope);
  ArgumentsSlot := DeclareArgumentsObjectLocal(ACtx, ChildScope, AMethod.Parameters);
  if FormalCount < 0 then
    FormalCount := Length(AMethod.Parameters);
  ChildTemplate.FormalParameterCount := UInt8(FormalCount);
  if Assigned(ACtx.FormalParameterCounts) then
    ACtx.FormalParameterCounts.AddOrSetValue(ChildTemplate, FormalCount);

  ACtx.SwapState(ChildTemplate, ChildScope);
  if Assigned(ACtx.SetDerivedConstructorThisGuard) then
    ACtx.SetDerivedConstructorThisGuard(AGuardDerivedThis);
  EmitLineMapping(ACtx, AMethod.Line, AMethod.Column);

  ChildCtx := ACtx;
  ChildCtx.Template := ChildTemplate;
  ChildCtx.Scope := ChildScope;
  ChildCtx.NonStrictMode := ACtx.NonStrictMode and
    not ChildTemplate.StrictCode;
  ChildCtx.DerivedConstructorThisGuard := AGuardDerivedThis;

  EmitCreateArgumentsObject(ChildCtx, ArgumentsSlot,
    ChildCtx.NonStrictMode and ParameterListIsSimple(AMethod.Parameters),
    Length(AMethod.Parameters));

  if (RestParamIndex >= 0) and
     not ParameterListHasDefaultValues(AMethod.Parameters) then
    EmitInstruction(ChildCtx, EncodeABC(OP_PACK_ARGS,
      UInt8(ChildScope.ResolveLocal(
        AMethod.Parameters[RestParamIndex].Name)),
      UInt8(RestParamIndex), 0));

  EmitDefaultParameters(ChildCtx, AMethod.Parameters);
  EmitDestructuringParameters(ChildCtx, AMethod.Parameters);
  if ChildTemplate.CodeCount > High(UInt16) then
    raise Exception.Create('Parameter preamble is too large to encode');
  ChildTemplate.ParameterPreambleSize := UInt16(ChildTemplate.CodeCount);

  ACtx.CompileFunctionBody(AMethod.Body);
  ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

  for I := 0 to ChildScope.UpvalueCount - 1 do
    ChildTemplate.AddUpvalueDescriptor(
      ChildScope.GetUpvalue(I).IsLocal,
      ChildScope.GetUpvalue(I).Index,
      ChildScope.GetUpvalue(I).Name);

  if Assigned(ACtx.SetDerivedConstructorThisGuard) then
    ACtx.SetDerivedConstructorThisGuard(OldDerivedGuard);
  ACtx.SwapState(OldTemplate, OldScope);
  ChildScope.Free;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  MethodReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, MethodReg, FuncIdx));

  MethodNameIdx := ACtx.Template.AddConstantString(AMethodName);
  if MethodNameIdx > High(UInt8) then
    raise Exception.Create('Constant pool overflow: method name index exceeds 255');
  EmitInstruction(ACtx, EncodeABC(AStoreOpcode,
    AClassReg, UInt8(MethodNameIdx), MethodReg));
  ACtx.Scope.FreeRegister;
end;

procedure CompileGetterBody(const ACtx: TGocciaCompilationContext;
  const ATargetReg: UInt8; const AName: string;
  const AGetter: TGocciaGetterExpression; const AOpcode: TGocciaOpCode;
  const AFlags: UInt8);
var
  OldTemplate: TGocciaFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TGocciaFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  ChildCtx: TGocciaCompilationContext;
  FuncIdx: UInt16;
  FnReg, TargetReg, AccessorReg: UInt8;
  NameIdx: UInt16;
  EmptyParams: TGocciaParameterArray;
  ArgumentsSlot: Integer;
  I: Integer;
  DisplayName: string;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;
  DisplayName := DisplayClassElementName(AName);

  ChildTemplate := TGocciaFunctionTemplate.Create('get ' + DisplayName);
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.SourceText := AGetter.SourceText;
  ChildTemplate.ParameterCount := 0;
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);
  ChildScope.DeclareLocal(KEYWORD_THIS, False);
  SetLength(EmptyParams, 0);
  ArgumentsSlot := DeclareArgumentsObjectLocal(ACtx, ChildScope, EmptyParams);

  ACtx.SwapState(ChildTemplate, ChildScope);
  ChildCtx := ACtx;
  ChildCtx.Template := ChildTemplate;
  ChildCtx.Scope := ChildScope;
  ChildCtx.NonStrictMode := ACtx.NonStrictMode and
    not ChildTemplate.StrictCode;
  ChildCtx.DerivedConstructorThisGuard := False;
  EmitLineMapping(ChildCtx, AGetter.Line, AGetter.Column);
  EmitCreateArgumentsObject(ChildCtx, ArgumentsSlot,
    ChildCtx.NonStrictMode and ParameterListIsSimple(EmptyParams),
    Length(EmptyParams));
  ACtx.CompileFunctionBody(AGetter.Body);
  ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

  for I := 0 to ChildScope.UpvalueCount - 1 do
    ChildTemplate.AddUpvalueDescriptor(
      ChildScope.GetUpvalue(I).IsLocal,
      ChildScope.GetUpvalue(I).Index,
      ChildScope.GetUpvalue(I).Name);

  ACtx.SwapState(OldTemplate, OldScope);
  ChildScope.Free;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  FnReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, FnReg, FuncIdx));

  NameIdx := ACtx.Template.AddConstantString(AName);
  if NameIdx > High(UInt8) then
    raise Exception.Create('Constant pool overflow: getter name index exceeds 255');
  TargetReg := ACtx.Scope.AllocateRegister;
  AccessorReg := ACtx.Scope.AllocateRegister;
  Assert(AccessorReg = TargetReg + 1,
    'OP_DEFINE_ACCESSOR_* expects accessor register at target + 1');
  EmitInstruction(ACtx, EncodeABC(OP_MOVE, TargetReg, ATargetReg, 0));
  EmitInstruction(ACtx, EncodeABC(OP_MOVE, AccessorReg, FnReg, 0));
  EmitInstruction(ACtx, EncodeABC(AOpcode, TargetReg, AFlags, UInt8(NameIdx)));
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileSetterBody(const ACtx: TGocciaCompilationContext;
  const ATargetReg: UInt8; const AName: string;
  const ASetter: TGocciaSetterExpression; const AOpcode: TGocciaOpCode;
  const AFlags: UInt8);
var
  OldTemplate: TGocciaFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TGocciaFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  ChildCtx: TGocciaCompilationContext;
  FuncIdx: UInt16;
  FnReg, TargetReg, AccessorReg: UInt8;
  NameIdx: UInt16;
  SetterParams: TGocciaParameterArray;
  ArgumentsSlot: Integer;
  FormalCount, I: Integer;
  DisplayName: string;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;
  DisplayName := DisplayClassElementName(AName);

  ChildTemplate := TGocciaFunctionTemplate.Create('set ' + DisplayName);
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.SourceText := ASetter.SourceText;
  SetterParams := ASetter.Parameters;
  if Length(SetterParams) = 0 then
  begin
    SetLength(SetterParams, 1);
    SetterParams[0].Name := ASetter.Parameter;
  end;
  ChildTemplate.ParameterCount := Length(SetterParams);
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);
  ChildScope.DeclareLocal(KEYWORD_THIS, False);
  for I := 0 to High(SetterParams) do
    if SetterParams[I].IsPattern then
      ChildScope.DeclareLocal(SyntheticParamLocalName(I), False)
    else
      ChildScope.DeclareLocal(SetterParams[I].Name, False);
  for I := 0 to High(SetterParams) do
    if SetterParams[I].IsPattern and Assigned(SetterParams[I].Pattern) then
      CollectDestructuringBindings(SetterParams[I].Pattern, ChildScope);
  FormalCount := Length(SetterParams);
  if (Length(SetterParams) > 0) and
     (SetterParams[0].IsRest or Assigned(SetterParams[0].DefaultValue)) then
    FormalCount := 0;
  ChildTemplate.FormalParameterCount := UInt8(FormalCount);
  if Assigned(ACtx.FormalParameterCounts) then
    ACtx.FormalParameterCounts.AddOrSetValue(ChildTemplate, FormalCount);
  ArgumentsSlot := DeclareArgumentsObjectLocal(ACtx, ChildScope, SetterParams);

  ACtx.SwapState(ChildTemplate, ChildScope);
  ChildCtx := ACtx;
  ChildCtx.Template := ChildTemplate;
  ChildCtx.Scope := ChildScope;
  ChildCtx.NonStrictMode := ACtx.NonStrictMode and
    not ChildTemplate.StrictCode;
  ChildCtx.DerivedConstructorThisGuard := False;
  EmitLineMapping(ChildCtx, ASetter.Line, ASetter.Column);
  EmitCreateArgumentsObject(ChildCtx, ArgumentsSlot,
    ChildCtx.NonStrictMode and ParameterListIsSimple(SetterParams),
    Length(SetterParams));
  EmitDefaultParameters(ChildCtx, SetterParams);
  EmitDestructuringParameters(ChildCtx, SetterParams);
  if ChildTemplate.CodeCount > High(UInt16) then
    raise Exception.Create('Parameter preamble is too large to encode');
  ChildTemplate.ParameterPreambleSize := UInt16(ChildTemplate.CodeCount);
  ACtx.CompileFunctionBody(ASetter.Body);
  ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

  for I := 0 to ChildScope.UpvalueCount - 1 do
    ChildTemplate.AddUpvalueDescriptor(
      ChildScope.GetUpvalue(I).IsLocal,
      ChildScope.GetUpvalue(I).Index,
      ChildScope.GetUpvalue(I).Name);

  ACtx.SwapState(OldTemplate, OldScope);
  ChildScope.Free;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  FnReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, FnReg, FuncIdx));

  NameIdx := ACtx.Template.AddConstantString(AName);
  if NameIdx > High(UInt8) then
    raise Exception.Create('Constant pool overflow: setter name index exceeds 255');
  TargetReg := ACtx.Scope.AllocateRegister;
  AccessorReg := ACtx.Scope.AllocateRegister;
  Assert(AccessorReg = TargetReg + 1,
    'OP_DEFINE_ACCESSOR_* expects accessor register at target + 1');
  EmitInstruction(ACtx, EncodeABC(OP_MOVE, TargetReg, ATargetReg, 0));
  EmitInstruction(ACtx, EncodeABC(OP_MOVE, AccessorReg, FnReg, 0));
  EmitInstruction(ACtx, EncodeABC(AOpcode, TargetReg, AFlags, UInt8(NameIdx)));
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileComputedGetterBody(const ACtx: TGocciaCompilationContext;
  const ATargetReg: UInt8; const AKeyReg: UInt8;
  const AGetter: TGocciaGetterExpression; const AOpcode: TGocciaOpCode;
  const AFlags: UInt8);
var
  OldTemplate: TGocciaFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TGocciaFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  ChildCtx: TGocciaCompilationContext;
  FuncIdx: UInt16;
  FnReg, TargetReg, AccessorReg: UInt8;
  EmptyParams: TGocciaParameterArray;
  ArgumentsSlot: Integer;
  I: Integer;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TGocciaFunctionTemplate.Create('<get [computed]>');
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.ParameterCount := 0;
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);
  ChildScope.DeclareLocal(KEYWORD_THIS, False);
  SetLength(EmptyParams, 0);
  ArgumentsSlot := DeclareArgumentsObjectLocal(ACtx, ChildScope, EmptyParams);

  ACtx.SwapState(ChildTemplate, ChildScope);
  ChildCtx := ACtx;
  ChildCtx.Template := ChildTemplate;
  ChildCtx.Scope := ChildScope;
  ChildCtx.NonStrictMode := ACtx.NonStrictMode and
    not ChildTemplate.StrictCode;
  ChildCtx.DerivedConstructorThisGuard := False;
  EmitLineMapping(ChildCtx, AGetter.Line, AGetter.Column);
  EmitCreateArgumentsObject(ChildCtx, ArgumentsSlot,
    ChildCtx.NonStrictMode and ParameterListIsSimple(EmptyParams),
    Length(EmptyParams));
  ACtx.CompileFunctionBody(AGetter.Body);
  ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

  for I := 0 to ChildScope.UpvalueCount - 1 do
    ChildTemplate.AddUpvalueDescriptor(
      ChildScope.GetUpvalue(I).IsLocal,
      ChildScope.GetUpvalue(I).Index,
      ChildScope.GetUpvalue(I).Name);

  ACtx.SwapState(OldTemplate, OldScope);
  ChildScope.Free;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  FnReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, FnReg, FuncIdx));
  EmitInstruction(ACtx, EncodeABC(OP_SET_FUNCTION_NAME, FnReg, AKeyReg,
    FUNCTION_NAME_PREFIX_GET));

  TargetReg := ACtx.Scope.AllocateRegister;
  AccessorReg := ACtx.Scope.AllocateRegister;
  Assert(AccessorReg = TargetReg + 1,
    'OP_DEFINE_ACCESSOR_* expects accessor register at target + 1');
  EmitInstruction(ACtx, EncodeABC(OP_MOVE, TargetReg, ATargetReg, 0));
  EmitInstruction(ACtx, EncodeABC(OP_MOVE, AccessorReg, FnReg, 0));
  EmitInstruction(ACtx, EncodeABC(AOpcode, TargetReg, AFlags, AKeyReg));
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileComputedSetterBody(const ACtx: TGocciaCompilationContext;
  const ATargetReg: UInt8; const AKeyReg: UInt8;
  const ASetter: TGocciaSetterExpression; const AOpcode: TGocciaOpCode;
  const AFlags: UInt8);
var
  OldTemplate: TGocciaFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TGocciaFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  ChildCtx: TGocciaCompilationContext;
  FuncIdx: UInt16;
  FnReg, TargetReg, AccessorReg: UInt8;
  SetterParams: TGocciaParameterArray;
  ArgumentsSlot: Integer;
  FormalCount, I: Integer;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TGocciaFunctionTemplate.Create('<set [computed]>');
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  SetterParams := ASetter.Parameters;
  if Length(SetterParams) = 0 then
  begin
    SetLength(SetterParams, 1);
    SetterParams[0].Name := ASetter.Parameter;
  end;
  ChildTemplate.ParameterCount := Length(SetterParams);
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);
  ChildScope.DeclareLocal(KEYWORD_THIS, False);
  for I := 0 to High(SetterParams) do
    if SetterParams[I].IsPattern then
      ChildScope.DeclareLocal(SyntheticParamLocalName(I), False)
    else
      ChildScope.DeclareLocal(SetterParams[I].Name, False);
  for I := 0 to High(SetterParams) do
    if SetterParams[I].IsPattern and Assigned(SetterParams[I].Pattern) then
      CollectDestructuringBindings(SetterParams[I].Pattern, ChildScope);
  FormalCount := Length(SetterParams);
  if (Length(SetterParams) > 0) and
     (SetterParams[0].IsRest or Assigned(SetterParams[0].DefaultValue)) then
    FormalCount := 0;
  ChildTemplate.FormalParameterCount := UInt8(FormalCount);
  if Assigned(ACtx.FormalParameterCounts) then
    ACtx.FormalParameterCounts.AddOrSetValue(ChildTemplate, FormalCount);
  ArgumentsSlot := DeclareArgumentsObjectLocal(ACtx, ChildScope, SetterParams);

  ACtx.SwapState(ChildTemplate, ChildScope);
  ChildCtx := ACtx;
  ChildCtx.Template := ChildTemplate;
  ChildCtx.Scope := ChildScope;
  ChildCtx.NonStrictMode := ACtx.NonStrictMode and
    not ChildTemplate.StrictCode;
  ChildCtx.DerivedConstructorThisGuard := False;
  EmitLineMapping(ChildCtx, ASetter.Line, ASetter.Column);
  EmitCreateArgumentsObject(ChildCtx, ArgumentsSlot,
    ChildCtx.NonStrictMode and ParameterListIsSimple(SetterParams),
    Length(SetterParams));
  EmitDefaultParameters(ChildCtx, SetterParams);
  EmitDestructuringParameters(ChildCtx, SetterParams);
  if ChildTemplate.CodeCount > High(UInt16) then
    raise Exception.Create('Parameter preamble is too large to encode');
  ChildTemplate.ParameterPreambleSize := UInt16(ChildTemplate.CodeCount);
  ACtx.CompileFunctionBody(ASetter.Body);
  ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

  for I := 0 to ChildScope.UpvalueCount - 1 do
    ChildTemplate.AddUpvalueDescriptor(
      ChildScope.GetUpvalue(I).IsLocal,
      ChildScope.GetUpvalue(I).Index,
      ChildScope.GetUpvalue(I).Name);

  ACtx.SwapState(OldTemplate, OldScope);
  ChildScope.Free;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  FnReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, FnReg, FuncIdx));
  EmitInstruction(ACtx, EncodeABC(OP_SET_FUNCTION_NAME, FnReg, AKeyReg,
    FUNCTION_NAME_PREFIX_SET));

  TargetReg := ACtx.Scope.AllocateRegister;
  AccessorReg := ACtx.Scope.AllocateRegister;
  Assert(AccessorReg = TargetReg + 1,
    'OP_DEFINE_ACCESSOR_* expects accessor register at target + 1');
  EmitInstruction(ACtx, EncodeABC(OP_MOVE, TargetReg, ATargetReg, 0));
  EmitInstruction(ACtx, EncodeABC(OP_MOVE, AccessorReg, FnReg, 0));
  EmitInstruction(ACtx, EncodeABC(AOpcode, TargetReg, AFlags, AKeyReg));
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileComputedMethodBody(const ACtx: TGocciaCompilationContext;
  const AClassReg: UInt8; const AKeyReg: UInt8;
  const AMethod: TGocciaClassMethod; const AIsStatic: Boolean);
var
  OldTemplate: TGocciaFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TGocciaFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  ChildCtx: TGocciaCompilationContext;
  FuncIdx: UInt16;
  FnReg, TargetReg: UInt8;
  ProtoNameIdx: UInt16;
  FormalCount, RestParamIndex, I: Integer;
  ArgumentsSlot: Integer;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TGocciaFunctionTemplate.Create('<method [computed]>');
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.IsAsync := AMethod.IsAsync;
  ChildTemplate.IsGenerator := AMethod.IsGenerator;
  ChildTemplate.SourceText := AMethod.SourceText;
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);

  ChildScope.DeclareLocal(KEYWORD_THIS, False);
  ChildTemplate.ParameterCount := Length(AMethod.Parameters);

  FormalCount := -1;
  RestParamIndex := -1;
  for I := 0 to High(AMethod.Parameters) do
  begin
    if AMethod.Parameters[I].IsRest or
       Assigned(AMethod.Parameters[I].DefaultValue) then
    begin
      if FormalCount < 0 then
        FormalCount := I;
      if AMethod.Parameters[I].IsRest then
        RestParamIndex := I;
    end;
    if AMethod.Parameters[I].IsPattern then
      ChildScope.DeclareLocal(SyntheticParamLocalName(I), False)
    else
      ChildScope.DeclareLocal(AMethod.Parameters[I].Name, False);
  end;
  for I := 0 to High(AMethod.Parameters) do
    if AMethod.Parameters[I].IsPattern and Assigned(AMethod.Parameters[I].Pattern) then
      CollectDestructuringBindings(AMethod.Parameters[I].Pattern, ChildScope);
  ArgumentsSlot := DeclareArgumentsObjectLocal(ACtx, ChildScope, AMethod.Parameters);
  if FormalCount < 0 then
    FormalCount := Length(AMethod.Parameters);
  ChildTemplate.FormalParameterCount := UInt8(FormalCount);
  if Assigned(ACtx.FormalParameterCounts) then
    ACtx.FormalParameterCounts.AddOrSetValue(ChildTemplate, FormalCount);

  ACtx.SwapState(ChildTemplate, ChildScope);
  EmitLineMapping(ACtx, AMethod.Line, AMethod.Column);

  ChildCtx := ACtx;
  ChildCtx.Template := ChildTemplate;
  ChildCtx.Scope := ChildScope;
  ChildCtx.NonStrictMode := ACtx.NonStrictMode and
    not ChildTemplate.StrictCode;
  ChildCtx.DerivedConstructorThisGuard := False;

  EmitCreateArgumentsObject(ChildCtx, ArgumentsSlot,
    ChildCtx.NonStrictMode and ParameterListIsSimple(AMethod.Parameters),
    Length(AMethod.Parameters));

  if (RestParamIndex >= 0) and
     not ParameterListHasDefaultValues(AMethod.Parameters) then
    EmitInstruction(ChildCtx, EncodeABC(OP_PACK_ARGS,
      UInt8(ChildScope.ResolveLocal(
        AMethod.Parameters[RestParamIndex].Name)),
      UInt8(RestParamIndex), 0));

  EmitDefaultParameters(ChildCtx, AMethod.Parameters);
  EmitDestructuringParameters(ChildCtx, AMethod.Parameters);
  if ChildTemplate.CodeCount > High(UInt16) then
    raise Exception.Create('Parameter preamble is too large to encode');
  ChildTemplate.ParameterPreambleSize := UInt16(ChildTemplate.CodeCount);

  ACtx.CompileFunctionBody(AMethod.Body);
  ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

  for I := 0 to ChildScope.UpvalueCount - 1 do
    ChildTemplate.AddUpvalueDescriptor(
      ChildScope.GetUpvalue(I).IsLocal,
      ChildScope.GetUpvalue(I).Index,
      ChildScope.GetUpvalue(I).Name);

  ACtx.SwapState(OldTemplate, OldScope);
  ChildScope.Free;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  FnReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, FnReg, FuncIdx));
  EmitInstruction(ACtx, EncodeABC(OP_SET_FUNCTION_NAME, FnReg, AKeyReg,
    FUNCTION_NAME_PREFIX_NONE));

  if AIsStatic then
    // Static: class[key] = method
    EmitInstruction(ACtx, EncodeABC(OP_SET_INDEX, AClassReg, AKeyReg, FnReg))
  else
  begin
    // Instance: class.prototype[key] = method
    TargetReg := ACtx.Scope.AllocateRegister;
    ProtoNameIdx := ACtx.Template.AddConstantString(PROP_PROTOTYPE);
    EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, TargetReg,
      AClassReg, UInt8(ProtoNameIdx)));
    EmitInstruction(ACtx, EncodeABC(OP_SET_INDEX, TargetReg, AKeyReg, FnReg));
    ACtx.Scope.FreeRegister;
  end;
  ACtx.Scope.FreeRegister;
end;

function FindComputedFieldKeyLocalName(
  const ALocals: TComputedFieldKeyLocals;
  const AElementIndex: Integer): string;
var
  LocalIndex: Integer;
begin
  for LocalIndex := 0 to High(ALocals) do
    if ALocals[LocalIndex].ElementIndex = AElementIndex then
      Exit(ALocals[LocalIndex].Name);
  Result := '';
end;

procedure CompileComputedElements(const ACtx: TGocciaCompilationContext;
  const ATargetReg: UInt8; const AClassDef: TGocciaClassDefinition;
  var AComputedFieldKeyLocals: TComputedFieldKeyLocals);
var
  I: Integer;
  Elem: TGocciaClassElement;
  KeyReg: UInt8;
  ComputedKeyName: string;
  ClassKeyPrefix: string;
  NeedsKeyLocal: Boolean;
  KeyIsLocal: Boolean;
begin
  SetLength(AComputedFieldKeyLocals, 0);
  ClassKeyPrefix := IntToHex(PtrUInt(AClassDef), SizeOf(PtrUInt) * 2);
  for I := 0 to High(AClassDef.FElements) do
  begin
    Elem := AClassDef.FElements[I];
    if not Elem.IsComputed then
      Continue;
    if not (Elem.Kind in [cekGetter, cekSetter, cekMethod, cekField, cekAccessor]) then
      Continue;

    NeedsKeyLocal := (Elem.Kind in [cekField, cekAccessor]) or
      (Length(Elem.Decorators) > 0);
    KeyIsLocal := False;
    if NeedsKeyLocal then
    begin
      SetLength(AComputedFieldKeyLocals, Length(AComputedFieldKeyLocals) + 1);
      ComputedKeyName := Format('#computed-element-key:%s:%d',
        [ClassKeyPrefix, I]);
      AComputedFieldKeyLocals[High(AComputedFieldKeyLocals)].ElementIndex := I;
      AComputedFieldKeyLocals[High(AComputedFieldKeyLocals)].Name :=
        ComputedKeyName;
      KeyReg := ACtx.Scope.DeclareLocal(ComputedKeyName, False);
      KeyIsLocal := True;
    end
    else
      KeyReg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(Elem.ComputedKeyExpression, KeyReg);
    if (Elem.Kind in [cekField, cekAccessor]) or
       (Length(Elem.Decorators) > 0) then
      EmitInstruction(ACtx, EncodeABC(OP_TO_PROPERTY_KEY, KeyReg, KeyReg, 0));

    case Elem.Kind of
      cekField:
      begin
        Continue;
      end;
      cekGetter:
        if Elem.IsStatic then
          CompileComputedGetterBody(ACtx, ATargetReg, KeyReg,
            Elem.GetterNode, OP_DEFINE_ACCESSOR_DYNAMIC, ACCESSOR_FLAG_STATIC)
        else
          CompileComputedGetterBody(ACtx, ATargetReg, KeyReg,
            Elem.GetterNode, OP_DEFINE_ACCESSOR_DYNAMIC, 0);
      cekSetter:
        if Elem.IsStatic then
          CompileComputedSetterBody(ACtx, ATargetReg, KeyReg,
            Elem.SetterNode, OP_DEFINE_ACCESSOR_DYNAMIC,
            ACCESSOR_FLAG_STATIC or ACCESSOR_FLAG_SETTER)
        else
          CompileComputedSetterBody(ACtx, ATargetReg, KeyReg,
            Elem.SetterNode, OP_DEFINE_ACCESSOR_DYNAMIC, ACCESSOR_FLAG_SETTER);
      cekMethod:
        CompileComputedMethodBody(ACtx, ATargetReg, KeyReg,
          Elem.MethodNode, Elem.IsStatic);
      cekAccessor:
        ; // Auto-accessor installation consumes the captured property key later.
    end;

    if not KeyIsLocal then
      ACtx.Scope.FreeRegister;
  end;
end;

function HasAccessorInitializers(
  const AClassDef: TGocciaClassDefinition): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AClassDef.FElements) do
    if (AClassDef.FElements[I].Kind = cekAccessor) and
       Assigned(AClassDef.FElements[I].FieldInitializer) then
      Exit(True);
  Result := False;
end;

function HasComputedInstanceFields(
  const AClassDef: TGocciaClassDefinition): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AClassDef.FFieldOrder) do
    if AClassDef.FFieldOrder[I].IsComputed then
      Exit(True);
  Result := False;
end;

function IsAnonymousFunctionNameInitializer(
  const AExpression: TGocciaExpression): Boolean;
begin
  Result := (AExpression is TGocciaArrowFunctionExpression) or
    ((AExpression is TGocciaFunctionExpression) and
     (TGocciaFunctionExpression(AExpression).Name = '')) or
    ((AExpression is TGocciaClassExpression) and
     (TGocciaClassExpression(AExpression).ClassDefinition.Name = ''));
end;

function ClassFieldInferredName(const AElement: TGocciaClassElement): string;
begin
  if AElement.IsComputed then
    Exit('');
  if AElement.IsPrivate then
    Exit('#' + AElement.Name);
  Result := AElement.Name;
end;

procedure CompileFieldValueWithInferredName(
  const ACtx: TGocciaCompilationContext; const AExpression: TGocciaExpression;
  const ADest: UInt8; const AInferredName: string);
begin
  if not Assigned(AExpression) then
  begin
    EmitInstruction(ACtx, EncodeABx(OP_LOAD_UNDEFINED, ADest, 0));
    Exit;
  end;

  Goccia.Compiler.Expressions.CompileExpressionWithInferredName(
    ACtx, AExpression, ADest, AInferredName);
end;

procedure CompileStaticFieldInitializerExpression(
  const ACtx: TGocciaCompilationContext; const AClassReg: UInt8;
  const AExpression: TGocciaExpression; const ADest: UInt8;
  const AInferredName: string = '');
var
  ClosedLocals: array[0..0] of UInt8;
  ClosedCount, I: Integer;
  ThisReg: UInt8;
  OldRejectArgumentsInDirectEval: Boolean;
begin
  OldRejectArgumentsInDirectEval := ACtx.Template.RejectArgumentsInDirectEval;
  ACtx.Template.RejectArgumentsInDirectEval := True;
  ACtx.Scope.BeginScope;
  try
    ThisReg := ACtx.Scope.DeclareLocal(KEYWORD_THIS, False);
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ThisReg, AClassReg, 0));
    CompileFieldValueWithInferredName(ACtx, AExpression, ADest,
      AInferredName);
    ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
    for I := 0 to ClosedCount - 1 do
      EmitInstruction(ACtx,
        EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));
  finally
    ACtx.Template.RejectArgumentsInDirectEval :=
      OldRejectArgumentsInDirectEval;
  end;
end;

procedure RegisterPrivateName(const AScope: TGocciaCompilerScope;
  const AName, APrefix: string);
begin
  if AName = '' then
    Exit;
  if AName[1] = '#' then
    AScope.DeclarePrivateNamePrefix(Copy(AName, 2, MaxInt), APrefix)
  else
    AScope.DeclarePrivateNamePrefix(AName, APrefix);
end;

procedure RegisterClassPrivateNames(const AScope: TGocciaCompilerScope;
  const AClassDef: TGocciaClassDefinition; const APrefix: string);
var
  I: Integer;
  MethodPair: TGocciaClassMethodMap.TKeyValuePair;
  ExprPair: TGocciaExpressionMap.TKeyValuePair;
  GetterPair: TGocciaGetterExpressionMap.TKeyValuePair;
  SetterPair: TGocciaSetterExpressionMap.TKeyValuePair;
begin
  for I := 0 to High(AClassDef.FElements) do
    if AClassDef.FElements[I].IsPrivate then
      RegisterPrivateName(AScope, AClassDef.FElements[I].Name, APrefix);

  for ExprPair in AClassDef.PrivateInstanceProperties do
    RegisterPrivateName(AScope, ExprPair.Key, APrefix);
  for ExprPair in AClassDef.PrivateStaticProperties do
    RegisterPrivateName(AScope, ExprPair.Key, APrefix);
  for MethodPair in AClassDef.PrivateMethods do
    RegisterPrivateName(AScope, MethodPair.Key, APrefix);
  for GetterPair in AClassDef.Getters do
    if (GetterPair.Key <> '') and (GetterPair.Key[1] = '#') then
      RegisterPrivateName(AScope, GetterPair.Key, APrefix);
  for SetterPair in AClassDef.Setters do
    if (SetterPair.Key <> '') and (SetterPair.Key[1] = '#') then
      RegisterPrivateName(AScope, SetterPair.Key, APrefix);
  for GetterPair in AClassDef.StaticGetters do
    if (GetterPair.Key <> '') and (GetterPair.Key[1] = '#') then
      RegisterPrivateName(AScope, GetterPair.Key, APrefix);
  for SetterPair in AClassDef.StaticSetters do
    if (SetterPair.Key <> '') and (SetterPair.Key[1] = '#') then
      RegisterPrivateName(AScope, SetterPair.Key, APrefix);
end;

procedure CompileFieldInitializer(const ACtx: TGocciaCompilationContext;
  const AClassReg: UInt8; const AClassDef: TGocciaClassDefinition;
  const AComputedFieldKeyLocals: TComputedFieldKeyLocals);
var
  OldTemplate: TGocciaFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TGocciaFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  ChildCtx: TGocciaCompilationContext;
  FuncIdx: UInt16;
  FnReg: UInt8;
  ValReg, ThisReg, KeyReg: UInt8;
  KeyIdx: UInt16;
  I, UpvalueIdx: Integer;
  Entry: TGocciaExpressionMap.TKeyValuePair;
  Elem: TGocciaClassElement;
  ComputedKeyName: string;
  AccessorBackingName: string;
  StoreOpcode: TGocciaOpCode;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TGocciaFunctionTemplate.Create('<fields>');
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.ParameterCount := 0;
  ChildTemplate.RejectArgumentsInDirectEval := True;
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);

  ThisReg := ChildScope.DeclareLocal(KEYWORD_THIS, False);

  ACtx.SwapState(ChildTemplate, ChildScope);

  ChildCtx := ACtx;
  ChildCtx.Template := ChildTemplate;
  ChildCtx.Scope := ChildScope;
  ChildCtx.NonStrictMode := ACtx.NonStrictMode and
    not ChildTemplate.StrictCode;
  ChildCtx.DerivedConstructorThisGuard := False;

  if Length(AClassDef.FFieldOrder) > 0 then
  begin
    for I := 0 to High(AClassDef.FFieldOrder) do
    begin
      ValReg := ChildScope.AllocateRegister;
      if AClassDef.FFieldOrder[I].IsComputed then
      begin
        CompileFieldValueWithInferredName(ChildCtx,
          AClassDef.FFieldOrder[I].FieldInitializer, ValReg, '');
        KeyReg := ChildScope.AllocateRegister;
        ComputedKeyName := FindComputedFieldKeyLocalName(
          AComputedFieldKeyLocals, AClassDef.FFieldOrder[I].ElementIndex);
        UpvalueIdx := ChildScope.ResolveUpvalue(ComputedKeyName);
        if UpvalueIdx < 0 then
          raise Exception.Create('Compiler error: computed class field key was not captured');
        EmitInstruction(ChildCtx, EncodeABx(OP_GET_UPVALUE, KeyReg,
          UInt16(UpvalueIdx)));
        if IsAnonymousFunctionNameInitializer(
           AClassDef.FFieldOrder[I].FieldInitializer) then
          EmitInstruction(ChildCtx, EncodeABC(OP_SET_FUNCTION_NAME, ValReg,
            KeyReg, 0));
        EmitInstruction(ChildCtx, EncodeABC(OP_DEFINE_PROP_DYNAMIC, ThisReg,
          KeyReg, ValReg));
        ChildScope.FreeRegister;
        ChildScope.FreeRegister;
        Continue;
      end
      else if AClassDef.FFieldOrder[I].IsPrivate then
      begin
        CompileFieldValueWithInferredName(ChildCtx,
          AClassDef.FFieldOrder[I].FieldInitializer, ValReg,
          '#' + AClassDef.FFieldOrder[I].Name);
        KeyIdx := ChildTemplate.AddConstantString(
          '#slot:' + ChildScope.ResolvePrivatePrefix + AClassDef.FFieldOrder[I].Name);
        StoreOpcode := OP_DEFINE_STATIC_PROP_CONST;
      end
      else
      begin
        CompileFieldValueWithInferredName(ChildCtx,
          AClassDef.FFieldOrder[I].FieldInitializer, ValReg,
          AClassDef.FFieldOrder[I].Name);
        KeyIdx := ChildTemplate.AddConstantString(AClassDef.FFieldOrder[I].Name);
        StoreOpcode := OP_DEFINE_STATIC_PROP_CONST;
      end;
      if KeyIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: field name index exceeds 255');
      EmitInstruction(ChildCtx, EncodeABC(StoreOpcode, ThisReg,
        UInt8(KeyIdx), ValReg));
      ChildScope.FreeRegister;
    end;
  end
  else
  begin
    for I := 0 to AClassDef.InstanceProperties.Count - 1 do
    begin
      Entry := AClassDef.InstanceProperties.EntryAt(I);
      ValReg := ChildScope.AllocateRegister;
      ACtx.CompileExpression(Entry.Value, ValReg);
      KeyIdx := ChildTemplate.AddConstantString(Entry.Key);
      if KeyIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: field name index exceeds 255');
      EmitInstruction(ChildCtx, EncodeABC(OP_DEFINE_STATIC_PROP_CONST, ThisReg,
        UInt8(KeyIdx), ValReg));
      ChildScope.FreeRegister;
    end;

    for I := 0 to AClassDef.PrivateInstanceProperties.Count - 1 do
    begin
      Entry := AClassDef.PrivateInstanceProperties.EntryAt(I);
      ValReg := ChildScope.AllocateRegister;
      ACtx.CompileExpression(Entry.Value, ValReg);
      KeyIdx := ChildTemplate.AddConstantString('#slot:' + ChildScope.ResolvePrivatePrefix + Entry.Key);
      if KeyIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: field name index exceeds 255');
      EmitInstruction(ChildCtx, EncodeABC(OP_DEFINE_STATIC_PROP_CONST, ThisReg,
        UInt8(KeyIdx), ValReg));
      ChildScope.FreeRegister;
    end;
  end;

  for I := 0 to High(AClassDef.FElements) do
  begin
    Elem := AClassDef.FElements[I];
    if (Elem.Kind <> cekAccessor) or not Assigned(Elem.FieldInitializer) then
      Continue;
    ValReg := ChildScope.AllocateRegister;
    ACtx.CompileExpression(Elem.FieldInitializer, ValReg);
    if Elem.IsComputed then
      AccessorBackingName := '__accessor_computed_' + IntToStr(I)
    else
      AccessorBackingName := '__accessor_' + Elem.Name;
    KeyIdx := ChildTemplate.AddConstantString(AccessorBackingName);
    if KeyIdx > High(UInt8) then
      raise Exception.Create('Constant pool overflow: accessor backing field name index exceeds 255');
    EmitInstruction(ChildCtx, EncodeABC(OP_SET_PROP_CONST, ThisReg,
      UInt8(KeyIdx), ValReg));
    ChildScope.FreeRegister;
  end;

  ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

  for I := 0 to ChildScope.UpvalueCount - 1 do
    ChildTemplate.AddUpvalueDescriptor(
      ChildScope.GetUpvalue(I).IsLocal,
      ChildScope.GetUpvalue(I).Index,
      ChildScope.GetUpvalue(I).Name);

  ACtx.SwapState(OldTemplate, OldScope);
  ChildScope.Free;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  FnReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, FnReg, FuncIdx));

  EmitInstruction(ACtx, EncodeABC(OP_CLASS_SET_FIELD_INITIALIZER,
    AClassReg, FnReg, 0));
  ACtx.Scope.FreeRegister;
end;

// ES2022 §15.7.14 ClassStaticBlockDefinition
procedure CompileStaticBlock(const ACtx: TGocciaCompilationContext;
  const AClassReg: UInt8; const ABody: TGocciaBlockStatement);
var
  OldTemplate: TGocciaFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TGocciaFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  ChildCtx: TGocciaCompilationContext;
  FuncIdx: UInt16;
  FnReg: UInt8;
  I: Integer;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TGocciaFunctionTemplate.Create('<static block>');
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.ParameterCount := 0;
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);

  ChildScope.DeclareLocal(KEYWORD_THIS, False);

  ACtx.SwapState(ChildTemplate, ChildScope);

  ChildCtx := ACtx;
  ChildCtx.Template := ChildTemplate;
  ChildCtx.Scope := ChildScope;
  ChildCtx.NonStrictMode := ACtx.NonStrictMode and
    not ChildTemplate.StrictCode;
  ChildCtx.DerivedConstructorThisGuard := False;

  CompileBlockStatement(ChildCtx, ABody);

  EmitInstruction(ChildCtx, EncodeABx(OP_LOAD_UNDEFINED, 0, 0));
  EmitInstruction(ChildCtx, EncodeABC(OP_RETURN, 0, 0, 0));

  ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

  for I := 0 to ChildScope.UpvalueCount - 1 do
    ChildTemplate.AddUpvalueDescriptor(
      ChildScope.GetUpvalue(I).IsLocal,
      ChildScope.GetUpvalue(I).Index,
      ChildScope.GetUpvalue(I).Name);

  ACtx.SwapState(OldTemplate, OldScope);
  ChildScope.Free;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  FnReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, FnReg, FuncIdx));

  EmitInstruction(ACtx, EncodeABC(OP_CLASS_EXEC_STATIC_BLOCK,
    AClassReg, FnReg, 0));
  ACtx.Scope.FreeRegister;
end;

function ElementDescriptor(const AKind: TGocciaClassElementKind;
  const AName: string; const AIsStatic, AIsPrivate: Boolean): string;
var
  KindChar: Char;
  Flags: Integer;
begin
  case AKind of
    cekMethod: KindChar := 'm';
    cekGetter: KindChar := 'g';
    cekSetter: KindChar := 's';
    cekField: KindChar := 'f';
    cekAccessor: KindChar := 'a';
    cekStaticBlock: KindChar := 'b';
  else
    KindChar := 'm';
  end;
  Flags := 0;
  if AIsStatic then Flags := Flags or 1;
  if AIsPrivate then Flags := Flags or 2;
  Result := KindChar + ':' + AName + ':' + IntToStr(Flags);
end;

function HasDecoratorsOrAccessors(
  const AClassDef: TGocciaClassDefinition): Boolean;
var
  I: Integer;
begin
  if Length(AClassDef.FDecorators) > 0 then
    Exit(True);
  for I := 0 to High(AClassDef.FElements) do
  begin
    if Length(AClassDef.FElements[I].Decorators) > 0 then
      Exit(True);
    if AClassDef.FElements[I].Kind = cekAccessor then
      Exit(True);
  end;
  Result := False;
end;

procedure CompileAutoAccessors(const ACtx: TGocciaCompilationContext;
  const AClassReg: UInt8; const AClassDef: TGocciaClassDefinition;
  const AComputedFieldKeyLocals: TComputedFieldKeyLocals);
var
  I: Integer;
  Elem: TGocciaClassElement;
  NameIdx: UInt16;
  KeyReg: UInt8;
  LocalIdx: Integer;
  ComputedKeyName: string;
  BackingName: string;
begin
  for I := 0 to High(AClassDef.FElements) do
  begin
    Elem := AClassDef.FElements[I];
    if Elem.Kind <> cekAccessor then
      Continue;

    if Elem.IsComputed then
      BackingName := '__accessor_computed_' + IntToStr(I)
    else
      BackingName := Elem.Name;

    NameIdx := ACtx.Template.AddConstantString(BackingName);
    if NameIdx > High(UInt8) then
      raise Exception.Create('Constant pool overflow: accessor name index exceeds 255');

    if Elem.IsComputed then
    begin
      ComputedKeyName := FindComputedFieldKeyLocalName(
        AComputedFieldKeyLocals, I);
      LocalIdx := ACtx.Scope.ResolveLocal(ComputedKeyName);
      if LocalIdx < 0 then
        raise Exception.Create('Compiler error: computed auto-accessor key was not captured');
      KeyReg := ACtx.Scope.GetLocal(LocalIdx).Slot;
      EmitInstruction(ACtx, EncodeABC(OP_SETUP_AUTO_ACCESSOR_DYNAMIC,
        KeyReg, Ord(Elem.IsStatic), UInt8(NameIdx)));
    end
    else
      EmitInstruction(ACtx, EncodeABC(OP_SETUP_AUTO_ACCESSOR_CONST,
        0, Ord(Elem.IsStatic), UInt8(NameIdx)));
  end;
end;

procedure CompileDecoratorOrchestration(
  const ACtx: TGocciaCompilationContext;
  const AClassReg: UInt8; const AClassDef: TGocciaClassDefinition;
  const AComputedFieldKeyLocals: TComputedFieldKeyLocals);
var
  I, J: Integer;
  Elem: TGocciaClassElement;
  DecoRegs: array of array of UInt8;
  ClassDecoRegs: array of UInt8;
  DescIdx: UInt16;
  Desc: string;
  PairReg, ExtraReg: UInt8;
  HasElementDecorators: Boolean;
  ComputedKeyName: string;
  LocalIdx: Integer;
begin
  HasElementDecorators := False;
  for I := 0 to High(AClassDef.FElements) do
    if Length(AClassDef.FElements[I].Decorators) > 0 then
    begin
      HasElementDecorators := True;
      Break;
    end;

  if not HasElementDecorators and (Length(AClassDef.FDecorators) = 0) then
    Exit;

  SetLength(DecoRegs, Length(AClassDef.FElements));
  for I := 0 to High(AClassDef.FElements) do
  begin
    Elem := AClassDef.FElements[I];
    SetLength(DecoRegs[I], Length(Elem.Decorators));
    for J := 0 to High(Elem.Decorators) do
    begin
      DecoRegs[I][J] := ACtx.Scope.AllocateRegister;
      ACtx.CompileExpression(Elem.Decorators[J], DecoRegs[I][J]);
    end;
  end;

  SetLength(ClassDecoRegs, Length(AClassDef.FDecorators));
  for I := 0 to High(AClassDef.FDecorators) do
  begin
    ClassDecoRegs[I] := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(AClassDef.FDecorators[I], ClassDecoRegs[I]);
  end;

  for I := 0 to High(AClassDef.FElements) do
  begin
    Elem := AClassDef.FElements[I];
    if Length(DecoRegs[I]) = 0 then
      Continue;

    for J := High(DecoRegs[I]) downto 0 do
    begin
      Desc := ElementDescriptor(Elem.Kind, Elem.Name,
        Elem.IsStatic, Elem.IsPrivate);
      DescIdx := ACtx.Template.AddConstantString(Desc);
      if DescIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: descriptor index exceeds 255');

      PairReg := ACtx.Scope.AllocateRegister;
      ExtraReg := ACtx.Scope.AllocateRegister;
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, PairReg, DecoRegs[I][J], 0));
      if Elem.IsComputed then
      begin
        ComputedKeyName := FindComputedFieldKeyLocalName(
          AComputedFieldKeyLocals, I);
        LocalIdx := ACtx.Scope.ResolveLocal(ComputedKeyName);
        if LocalIdx < 0 then
          raise Exception.Create('Compiler error: computed decorator element key was not captured');
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, ExtraReg,
          ACtx.Scope.GetLocal(LocalIdx).Slot, 0));
        EmitInstruction(ACtx, EncodeABC(OP_APPLY_ELEMENT_DECORATOR_CONST,
          PairReg, ExtraReg, UInt8(DescIdx)));
      end
      else
      begin
        EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, ExtraReg, 0, 0));
        EmitInstruction(ACtx, EncodeABC(OP_APPLY_ELEMENT_DECORATOR_CONST,
          PairReg, 0, UInt8(DescIdx)));
      end;
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
    end;
  end;

  for I := High(ClassDecoRegs) downto 0 do
  begin
    PairReg := ACtx.Scope.AllocateRegister;
    ExtraReg := ACtx.Scope.AllocateRegister;
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, PairReg, ClassDecoRegs[I], 0));
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, ExtraReg, 0, 0));
    EmitInstruction(ACtx, EncodeABC(OP_APPLY_CLASS_DECORATOR, PairReg, 0, 0));
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
  end;

  for I := High(ClassDecoRegs) downto 0 do
    ACtx.Scope.FreeRegister;
  for I := High(AClassDef.FElements) downto 0 do
    for J := High(DecoRegs[I]) downto 0 do
      ACtx.Scope.FreeRegister;
end;

procedure CompileDecoratorAndAccessorPass(
  const ACtx: TGocciaCompilationContext;
  const AClassReg: UInt8; const AClassDef: TGocciaClassDefinition;
  const ASuperReg: Integer;
  const AComputedFieldKeyLocals: TComputedFieldKeyLocals);
var
  PairReg, ExtraReg: UInt8;
begin
  if not HasDecoratorsOrAccessors(AClassDef) then
    Exit;

  PairReg := ACtx.Scope.AllocateRegister;
  ExtraReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABC(OP_MOVE, PairReg, AClassReg, 0));
  if ASuperReg >= 0 then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ExtraReg, UInt8(ASuperReg), 0))
  else
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, ExtraReg, 0, 0));
  EmitInstruction(ACtx, EncodeABC(OP_BEGIN_DECORATORS, PairReg, 0, 0));
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;

  CompileAutoAccessors(ACtx, AClassReg, AClassDef,
    AComputedFieldKeyLocals);
  CompileDecoratorOrchestration(ACtx, AClassReg, AClassDef,
    AComputedFieldKeyLocals);

  PairReg := ACtx.Scope.AllocateRegister;
  ExtraReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABC(OP_MOVE, PairReg, AClassReg, 0));
  EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, ExtraReg, 0, 0));
  EmitInstruction(ACtx, EncodeABC(OP_FINISH_DECORATORS, PairReg, 0, 0));
  EmitInstruction(ACtx, EncodeABC(OP_MOVE, AClassReg, PairReg, 0));
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileClassDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaClassDeclaration);
var
  ClassDef: TGocciaClassDefinition;
  ClassReg, SuperReg, ValReg, KeyReg: UInt8;
  NameIdx, KeyIdx: UInt16;
  MethodPair: TGocciaClassMethodMap.TKeyValuePair;
  GetterPair: TGocciaGetterExpressionMap.TKeyValuePair;
  SetterPair: TGocciaSetterExpressionMap.TKeyValuePair;
  StaticPropPair: TGocciaExpressionMap.TKeyValuePair;
  I, ClassLocalIdx, LocalIdx, UpvalIdx: Integer;
  HasSuper: Boolean;
  IsTopLevelGlobalBacked: Boolean;
  PrivPrefix: string;
  PrivateNameMark: Integer;
  ComputedFieldKeyLocals: TComputedFieldKeyLocals;
  ComputedKeyName: string;
  InnerNameSlot: UInt8;
  HasInnerNameBinding: Boolean;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount: Integer;
begin
  ClassDef := AStmt.ClassDefinition;
  HasSuper := Assigned(ClassDef.SuperClassExpression) or
    (ClassDef.SuperClass <> '');
  IsTopLevelGlobalBacked := ACtx.GlobalBackedTopLevel and
    (ACtx.Scope.Depth = 0);

  PrivPrefix := NextClassPrivatePrefix;
  PrivateNameMark := ACtx.Scope.PrivateNameMark;
  ACtx.Scope.PrivatePrefix := PrivPrefix;
  RegisterClassPrivateNames(ACtx.Scope, ClassDef, PrivPrefix);

  // Reuse pre-declared slot if it exists at the same scope depth
  // (for function declaration upvalue resolution)
  LocalIdx := ACtx.Scope.ResolveLocal(ClassDef.Name);
  if (LocalIdx >= 0) and
     (ACtx.Scope.GetLocal(LocalIdx).Depth = ACtx.Scope.Depth) then
    ClassReg := ACtx.Scope.GetLocal(LocalIdx).Slot
  else
  begin
    ClassReg := ACtx.Scope.DeclareLocal(ClassDef.Name, False);
    LocalIdx := ACtx.Scope.ResolveLocal(ClassDef.Name);
  end;
  ClassLocalIdx := LocalIdx;
  if IsTopLevelGlobalBacked and (ClassLocalIdx >= 0) then
    ACtx.Scope.MarkGlobalBacked(ClassLocalIdx);
  NameIdx := ACtx.Template.AddConstantString(ClassDef.Name);
  EmitInstruction(ACtx, EncodeABx(OP_NEW_CLASS, ClassReg, NameIdx));

  HasInnerNameBinding := ClassDef.Name <> '';
  InnerNameSlot := 0;
  if HasInnerNameBinding then
  begin
    ACtx.Scope.BeginScope;
    InnerNameSlot := ACtx.Scope.DeclareLocal(ClassDef.Name, True);
    if HasSuper then
      EmitInstruction(ACtx, EncodeABC(OP_LOAD_HOLE, InnerNameSlot, 0, 0))
    else
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, InnerNameSlot, ClassReg, 0));
  end;

  if HasSuper then
  begin
    SuperReg := ACtx.Scope.DeclareLocal('__super__', False);

    if Assigned(ClassDef.SuperClassExpression) then
      ACtx.CompileExpression(ClassDef.SuperClassExpression, SuperReg)
    else
    begin
      LocalIdx := ACtx.Scope.ResolveLocal(ClassDef.SuperClass);
      if LocalIdx >= 0 then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, SuperReg,
          ACtx.Scope.GetLocal(LocalIdx).Slot, 0))
      else
      begin
        UpvalIdx := ACtx.Scope.ResolveUpvalue(ClassDef.SuperClass);
        if UpvalIdx >= 0 then
          EmitInstruction(ACtx, EncodeABx(OP_GET_UPVALUE, SuperReg,
            UInt16(UpvalIdx)))
        else
          EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, SuperReg,
            ACtx.Template.AddConstantString(ClassDef.SuperClass)));
      end;
    end;

    EmitInstruction(ACtx, EncodeABC(OP_CLASS_SET_SUPER, ClassReg, SuperReg, 0));
  end;

  if HasInnerNameBinding and HasSuper then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, InnerNameSlot, ClassReg, 0));

  for MethodPair in ClassDef.Methods do
    CompileMethodBody(ACtx, ClassReg, MethodPair.Key,
      MethodPair.Value, OP_CLASS_ADD_METHOD_CONST,
      HasSuper and (MethodPair.Key = PROP_CONSTRUCTOR));

  for MethodPair in ClassDef.StaticMethods do
    CompileMethodBody(ACtx, ClassReg, MethodPair.Key,
      MethodPair.Value, OP_DEFINE_STATIC_METHOD_CONST);

  for MethodPair in ClassDef.PrivateMethods do
  begin
    if MethodPair.Value.IsStatic then
      CompileMethodBody(ACtx, ClassReg, '#slot:' + PrivPrefix + MethodPair.Key,
        MethodPair.Value, OP_DEFINE_STATIC_METHOD_CONST)
    else
      CompileMethodBody(ACtx, ClassReg, '#slot:' + PrivPrefix + MethodPair.Key,
        MethodPair.Value, OP_CLASS_ADD_METHOD_CONST);
  end;

  for GetterPair in ClassDef.Getters do
  begin
    if (GetterPair.Key <> '') and (GetterPair.Key[1] = '#') then
      CompileGetterBody(ACtx, ClassReg,
        '#slot:' + PrivPrefix + Copy(GetterPair.Key, 2, MaxInt),
        GetterPair.Value, OP_DEFINE_ACCESSOR_CONST, 0)
    else
      CompileGetterBody(ACtx, ClassReg, GetterPair.Key,
        GetterPair.Value, OP_DEFINE_ACCESSOR_CONST, 0);
  end;

  for SetterPair in ClassDef.Setters do
  begin
    if (SetterPair.Key <> '') and (SetterPair.Key[1] = '#') then
      CompileSetterBody(ACtx, ClassReg,
        '#slot:' + PrivPrefix + Copy(SetterPair.Key, 2, MaxInt),
        SetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_SETTER)
    else
      CompileSetterBody(ACtx, ClassReg, SetterPair.Key,
        SetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_SETTER);
  end;

  for GetterPair in ClassDef.StaticGetters do
  begin
    if (GetterPair.Key <> '') and (GetterPair.Key[1] = '#') then
      CompileGetterBody(ACtx, ClassReg,
        '#slot:' + PrivPrefix + Copy(GetterPair.Key, 2, MaxInt),
        GetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_STATIC)
    else
      CompileGetterBody(ACtx, ClassReg, GetterPair.Key,
        GetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_STATIC);
  end;

  for SetterPair in ClassDef.StaticSetters do
  begin
    if (SetterPair.Key <> '') and (SetterPair.Key[1] = '#') then
      CompileSetterBody(ACtx, ClassReg,
        '#slot:' + PrivPrefix + Copy(SetterPair.Key, 2, MaxInt),
        SetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_STATIC or ACCESSOR_FLAG_SETTER)
    else
      CompileSetterBody(ACtx, ClassReg, SetterPair.Key,
        SetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_STATIC or ACCESSOR_FLAG_SETTER);
  end;

  CompileComputedElements(ACtx, ClassReg, ClassDef, ComputedFieldKeyLocals);

  if (ClassDef.InstanceProperties.Count > 0) or
     (ClassDef.PrivateInstanceProperties.Count > 0) or
     HasComputedInstanceFields(ClassDef) or
     HasAccessorInitializers(ClassDef) then
    CompileFieldInitializer(ACtx, ClassReg, ClassDef, ComputedFieldKeyLocals);

  // Static fields without FElements entries (legacy / no static blocks)
  if Length(ClassDef.FElements) = 0 then
  begin
    for StaticPropPair in ClassDef.StaticProperties do
    begin
      ValReg := ACtx.Scope.AllocateRegister;
      CompileStaticFieldInitializerExpression(
        ACtx, ClassReg, StaticPropPair.Value, ValReg, StaticPropPair.Key);
      KeyIdx := ACtx.Template.AddConstantString(StaticPropPair.Key);
      if KeyIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: static property name index exceeds 255');
      EmitInstruction(ACtx, EncodeABC(OP_DEFINE_STATIC_PROP_CONST, ClassReg,
        UInt8(KeyIdx), ValReg));
      ACtx.Scope.FreeRegister;
    end;

    for StaticPropPair in ClassDef.PrivateStaticProperties do
    begin
      ValReg := ACtx.Scope.AllocateRegister;
      CompileStaticFieldInitializerExpression(
        ACtx, ClassReg, StaticPropPair.Value, ValReg,
        '#' + StaticPropPair.Key);
      KeyIdx := ACtx.Template.AddConstantString('#slot:' + PrivPrefix + StaticPropPair.Key);
      if KeyIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: static property name index exceeds 255');
      EmitInstruction(ACtx, EncodeABC(OP_CLASS_DECLARE_PRIVATE_STATIC_CONST,
        ClassReg, UInt8(KeyIdx), 0));
      EmitInstruction(ACtx, EncodeABC(OP_DEFINE_STATIC_PROP_CONST, ClassReg,
        UInt8(KeyIdx), ValReg));
      ACtx.Scope.FreeRegister;
    end;
  end;

  // ES2022 §15.7.14: compile static fields and static blocks in source order
  for I := 0 to High(ClassDef.FElements) do
  begin
    if ClassDef.FElements[I].Kind = cekStaticBlock then
      CompileStaticBlock(ACtx, ClassReg, ClassDef.FElements[I].StaticBlockBody)
    else if (ClassDef.FElements[I].Kind = cekField) and ClassDef.FElements[I].IsStatic then
    begin
      ValReg := ACtx.Scope.AllocateRegister;
      if ClassDef.FElements[I].IsComputed then
      begin
        ComputedKeyName := FindComputedFieldKeyLocalName(
          ComputedFieldKeyLocals, I);
        LocalIdx := ACtx.Scope.ResolveLocal(ComputedKeyName);
        if LocalIdx < 0 then
          raise Exception.Create('Compiler error: computed static field key was not captured');
        KeyReg := ACtx.Scope.GetLocal(LocalIdx).Slot;
      end
      else
        KeyReg := 0;
      if Assigned(ClassDef.FElements[I].FieldInitializer) then
        CompileStaticFieldInitializerExpression(
          ACtx, ClassReg, ClassDef.FElements[I].FieldInitializer, ValReg,
          ClassFieldInferredName(ClassDef.FElements[I]))
      else
        EmitInstruction(ACtx, EncodeABx(OP_LOAD_UNDEFINED, ValReg, 0));
      if ClassDef.FElements[I].IsComputed and
         IsAnonymousFunctionNameInitializer(
           ClassDef.FElements[I].FieldInitializer) then
        EmitInstruction(ACtx, EncodeABC(OP_SET_FUNCTION_NAME, ValReg,
          KeyReg, 0));
      if ClassDef.FElements[I].IsPrivate then
      begin
        KeyIdx := ACtx.Template.AddConstantString(
          '#slot:' + PrivPrefix + ClassDef.FElements[I].Name);
        if KeyIdx > High(UInt8) then
          raise Exception.Create('Constant pool overflow: static property name index exceeds 255');
        EmitInstruction(ACtx, EncodeABC(OP_CLASS_DECLARE_PRIVATE_STATIC_CONST,
          ClassReg, UInt8(KeyIdx), 0));
        EmitInstruction(ACtx, EncodeABC(OP_DEFINE_STATIC_PROP_CONST, ClassReg,
          UInt8(KeyIdx), ValReg));
      end
      else if ClassDef.FElements[I].IsComputed then
        EmitInstruction(ACtx, EncodeABC(OP_DEFINE_PROP_DYNAMIC,
          ClassReg, KeyReg, ValReg))
      else
      begin
        KeyIdx := ACtx.Template.AddConstantString(ClassDef.FElements[I].Name);
        if KeyIdx > High(UInt8) then
          raise Exception.Create('Constant pool overflow: static property name index exceeds 255');
        EmitInstruction(ACtx, EncodeABC(OP_DEFINE_STATIC_PROP_CONST, ClassReg,
          UInt8(KeyIdx), ValReg));
      end;
      ACtx.Scope.FreeRegister;
    end;
  end;

  if HasSuper then
    CompileDecoratorAndAccessorPass(ACtx, ClassReg, ClassDef, SuperReg,
      ComputedFieldKeyLocals)
  else
    CompileDecoratorAndAccessorPass(ACtx, ClassReg, ClassDef, -1,
      ComputedFieldKeyLocals);

  // Sync cell if the class local was pre-declared and captured by a hoisted
  // function (see CompileVariableDeclaration for the full explanation)
  if (ClassLocalIdx >= 0) and
     ACtx.Scope.GetLocal(ClassLocalIdx).IsCaptured then
    EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, ClassReg, UInt16(ClassReg)));

  if IsTopLevelGlobalBacked then
    EmitGlobalDefine(ACtx, ClassReg, ClassDef.Name, False);

  if HasInnerNameBinding then
  begin
    ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
    for I := 0 to ClosedCount - 1 do
      EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));
  end;

  ACtx.Scope.PrivatePrefix := '';
  ACtx.Scope.RestorePrivateNameMark(PrivateNameMark);
end;

procedure CompileClassExpression(const ACtx: TGocciaCompilationContext;
  const AClassDef: TGocciaClassDefinition; const ADest: UInt8;
  const AInferredName: string = '');
var
  ClassDef: TGocciaClassDefinition;
  SuperReg, ValReg, KeyReg: UInt8;
  NameIdx, KeyIdx: UInt16;
  MethodPair: TGocciaClassMethodMap.TKeyValuePair;
  GetterPair: TGocciaGetterExpressionMap.TKeyValuePair;
  SetterPair: TGocciaSetterExpressionMap.TKeyValuePair;
  StaticPropPair: TGocciaExpressionMap.TKeyValuePair;
  LocalIdx, UpvalIdx: Integer;
  HasSuper: Boolean;
  PrivPrefix: string;
  PrivateNameMark: Integer;
  HasNameBinding: Boolean;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount, I: Integer;
  ComputedFieldKeyLocals: TComputedFieldKeyLocals;
  ComputedKeyName: string;
  NameSlot: UInt8;
begin
  ClassDef := AClassDef;
  HasSuper := Assigned(ClassDef.SuperClassExpression) or
    (ClassDef.SuperClass <> '');
  HasNameBinding := ClassDef.Name <> '';

  PrivPrefix := NextClassPrivatePrefix;
  PrivateNameMark := ACtx.Scope.PrivateNameMark;
  ACtx.Scope.PrivatePrefix := PrivPrefix;
  RegisterClassPrivateNames(ACtx.Scope, ClassDef, PrivPrefix);

  if HasNameBinding then
    NameIdx := ACtx.Template.AddConstantString(ClassDef.Name)
  else if AInferredName <> '' then
    NameIdx := ACtx.Template.AddConstantString(AInferredName)
  else
    NameIdx := ACtx.Template.AddConstantString('<anonymous>');
  EmitInstruction(ACtx, EncodeABx(OP_NEW_CLASS, ADest, NameIdx));

  // ES2026 §15.7.14: Named class expressions bind the name in an inner
  // block scope visible to methods/static initializers via upvalue capture
  if HasNameBinding then
  begin
    ACtx.Scope.BeginScope;
    NameSlot := ACtx.Scope.DeclareLocal(ClassDef.Name, True);
    if HasSuper then
      EmitInstruction(ACtx, EncodeABC(OP_LOAD_HOLE, NameSlot, 0, 0))
    else
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, NameSlot, ADest, 0));
  end;

  if HasSuper then
  begin
    SuperReg := ACtx.Scope.DeclareLocal('__super__', False);

    if Assigned(ClassDef.SuperClassExpression) then
      ACtx.CompileExpression(ClassDef.SuperClassExpression, SuperReg)
    else
    begin
      LocalIdx := ACtx.Scope.ResolveLocal(ClassDef.SuperClass);
      if LocalIdx >= 0 then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, SuperReg,
          ACtx.Scope.GetLocal(LocalIdx).Slot, 0))
      else
      begin
        UpvalIdx := ACtx.Scope.ResolveUpvalue(ClassDef.SuperClass);
        if UpvalIdx >= 0 then
          EmitInstruction(ACtx, EncodeABx(OP_GET_UPVALUE, SuperReg,
            UInt16(UpvalIdx)))
        else
          EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, SuperReg,
            ACtx.Template.AddConstantString(ClassDef.SuperClass)));
      end;
    end;

    EmitInstruction(ACtx, EncodeABC(OP_CLASS_SET_SUPER, ADest, SuperReg, 0));
  end;

  if HasNameBinding and HasSuper then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, NameSlot, ADest, 0));

  for MethodPair in ClassDef.Methods do
    CompileMethodBody(ACtx, ADest, MethodPair.Key,
      MethodPair.Value, OP_CLASS_ADD_METHOD_CONST,
      HasSuper and (MethodPair.Key = PROP_CONSTRUCTOR));

  for MethodPair in ClassDef.StaticMethods do
    CompileMethodBody(ACtx, ADest, MethodPair.Key,
      MethodPair.Value, OP_DEFINE_STATIC_METHOD_CONST);

  for MethodPair in ClassDef.PrivateMethods do
  begin
    if MethodPair.Value.IsStatic then
      CompileMethodBody(ACtx, ADest, '#slot:' + PrivPrefix + MethodPair.Key,
        MethodPair.Value, OP_DEFINE_STATIC_METHOD_CONST)
    else
      CompileMethodBody(ACtx, ADest, '#slot:' + PrivPrefix + MethodPair.Key,
        MethodPair.Value, OP_CLASS_ADD_METHOD_CONST);
  end;

  for GetterPair in ClassDef.Getters do
  begin
    if (GetterPair.Key <> '') and (GetterPair.Key[1] = '#') then
      CompileGetterBody(ACtx, ADest,
        '#slot:' + PrivPrefix + Copy(GetterPair.Key, 2, MaxInt),
        GetterPair.Value, OP_DEFINE_ACCESSOR_CONST, 0)
    else
      CompileGetterBody(ACtx, ADest, GetterPair.Key,
        GetterPair.Value, OP_DEFINE_ACCESSOR_CONST, 0);
  end;

  for SetterPair in ClassDef.Setters do
  begin
    if (SetterPair.Key <> '') and (SetterPair.Key[1] = '#') then
      CompileSetterBody(ACtx, ADest,
        '#slot:' + PrivPrefix + Copy(SetterPair.Key, 2, MaxInt),
        SetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_SETTER)
    else
      CompileSetterBody(ACtx, ADest, SetterPair.Key,
        SetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_SETTER);
  end;

  for GetterPair in ClassDef.StaticGetters do
  begin
    if (GetterPair.Key <> '') and (GetterPair.Key[1] = '#') then
      CompileGetterBody(ACtx, ADest,
        '#slot:' + PrivPrefix + Copy(GetterPair.Key, 2, MaxInt),
        GetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_STATIC)
    else
      CompileGetterBody(ACtx, ADest, GetterPair.Key,
        GetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_STATIC);
  end;

  for SetterPair in ClassDef.StaticSetters do
  begin
    if (SetterPair.Key <> '') and (SetterPair.Key[1] = '#') then
      CompileSetterBody(ACtx, ADest,
        '#slot:' + PrivPrefix + Copy(SetterPair.Key, 2, MaxInt),
        SetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_STATIC or ACCESSOR_FLAG_SETTER)
    else
      CompileSetterBody(ACtx, ADest, SetterPair.Key,
        SetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_STATIC or ACCESSOR_FLAG_SETTER);
  end;

  CompileComputedElements(ACtx, ADest, ClassDef, ComputedFieldKeyLocals);

  if (ClassDef.InstanceProperties.Count > 0) or
     (ClassDef.PrivateInstanceProperties.Count > 0) or
     HasComputedInstanceFields(ClassDef) or
     HasAccessorInitializers(ClassDef) then
    CompileFieldInitializer(ACtx, ADest, ClassDef, ComputedFieldKeyLocals);

  // Static fields without FElements entries (legacy / no static blocks)
  if Length(ClassDef.FElements) = 0 then
  begin
    for StaticPropPair in ClassDef.StaticProperties do
    begin
      ValReg := ACtx.Scope.AllocateRegister;
      CompileStaticFieldInitializerExpression(
        ACtx, ADest, StaticPropPair.Value, ValReg, StaticPropPair.Key);
      KeyIdx := ACtx.Template.AddConstantString(StaticPropPair.Key);
      if KeyIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: static property name index exceeds 255');
      EmitInstruction(ACtx, EncodeABC(OP_DEFINE_STATIC_PROP_CONST, ADest,
        UInt8(KeyIdx), ValReg));
      ACtx.Scope.FreeRegister;
    end;

    for StaticPropPair in ClassDef.PrivateStaticProperties do
    begin
      ValReg := ACtx.Scope.AllocateRegister;
      CompileStaticFieldInitializerExpression(
        ACtx, ADest, StaticPropPair.Value, ValReg,
        '#' + StaticPropPair.Key);
      KeyIdx := ACtx.Template.AddConstantString('#slot:' + PrivPrefix + StaticPropPair.Key);
      if KeyIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: static property name index exceeds 255');
      EmitInstruction(ACtx, EncodeABC(OP_CLASS_DECLARE_PRIVATE_STATIC_CONST,
        ADest, UInt8(KeyIdx), 0));
      EmitInstruction(ACtx, EncodeABC(OP_DEFINE_STATIC_PROP_CONST, ADest,
        UInt8(KeyIdx), ValReg));
      ACtx.Scope.FreeRegister;
    end;
  end;

  // ES2022 §15.7.14: compile static fields and static blocks in source order
  for I := 0 to High(ClassDef.FElements) do
  begin
    if ClassDef.FElements[I].Kind = cekStaticBlock then
      CompileStaticBlock(ACtx, ADest, ClassDef.FElements[I].StaticBlockBody)
    else if (ClassDef.FElements[I].Kind = cekField) and ClassDef.FElements[I].IsStatic then
    begin
      ValReg := ACtx.Scope.AllocateRegister;
      if ClassDef.FElements[I].IsComputed then
      begin
        ComputedKeyName := FindComputedFieldKeyLocalName(
          ComputedFieldKeyLocals, I);
        LocalIdx := ACtx.Scope.ResolveLocal(ComputedKeyName);
        if LocalIdx < 0 then
          raise Exception.Create('Compiler error: computed static field key was not captured');
        KeyReg := ACtx.Scope.GetLocal(LocalIdx).Slot;
      end
      else
        KeyReg := 0;
      if Assigned(ClassDef.FElements[I].FieldInitializer) then
        CompileStaticFieldInitializerExpression(
          ACtx, ADest, ClassDef.FElements[I].FieldInitializer, ValReg,
          ClassFieldInferredName(ClassDef.FElements[I]))
      else
        EmitInstruction(ACtx, EncodeABx(OP_LOAD_UNDEFINED, ValReg, 0));
      if ClassDef.FElements[I].IsComputed and
         IsAnonymousFunctionNameInitializer(
           ClassDef.FElements[I].FieldInitializer) then
        EmitInstruction(ACtx, EncodeABC(OP_SET_FUNCTION_NAME, ValReg,
          KeyReg, 0));
      if ClassDef.FElements[I].IsPrivate then
      begin
        KeyIdx := ACtx.Template.AddConstantString(
          '#slot:' + PrivPrefix + ClassDef.FElements[I].Name);
        if KeyIdx > High(UInt8) then
          raise Exception.Create('Constant pool overflow: static property name index exceeds 255');
        EmitInstruction(ACtx, EncodeABC(OP_CLASS_DECLARE_PRIVATE_STATIC_CONST,
          ADest, UInt8(KeyIdx), 0));
        EmitInstruction(ACtx, EncodeABC(OP_DEFINE_STATIC_PROP_CONST, ADest,
          UInt8(KeyIdx), ValReg));
      end
      else if ClassDef.FElements[I].IsComputed then
        EmitInstruction(ACtx, EncodeABC(OP_DEFINE_PROP_DYNAMIC,
          ADest, KeyReg, ValReg))
      else
      begin
        KeyIdx := ACtx.Template.AddConstantString(ClassDef.FElements[I].Name);
        if KeyIdx > High(UInt8) then
          raise Exception.Create('Constant pool overflow: static property name index exceeds 255');
        EmitInstruction(ACtx, EncodeABC(OP_DEFINE_STATIC_PROP_CONST, ADest,
          UInt8(KeyIdx), ValReg));
      end;
      ACtx.Scope.FreeRegister;
    end;
  end;

  if HasSuper then
  begin
    CompileDecoratorAndAccessorPass(ACtx, ADest, ClassDef, SuperReg,
      ComputedFieldKeyLocals);
    // Only free __super__ manually when there is no name binding scope —
    // when HasNameBinding is true, __super__ lives inside the inner scope
    // and EndScope below will free it together with the name binding local.
    if not HasNameBinding then
    begin
      EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, SuperReg, 0, 0));
      ACtx.Scope.FreeRegister;
    end;
  end
  else
    CompileDecoratorAndAccessorPass(ACtx, ADest, ClassDef, -1,
      ComputedFieldKeyLocals);

  if HasNameBinding then
  begin
    ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
    for I := 0 to ClosedCount - 1 do
      EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));
  end;

  ACtx.Scope.PrivatePrefix := '';
  ACtx.Scope.RestorePrivateNameMark(PrivateNameMark);
end;

procedure CollectDestructuringVarBindings(const APattern: TGocciaDestructuringPattern;
  const AScope: TGocciaCompilerScope);
var
  Names: TStringList;
  I: Integer;
begin
  Names := TStringList.Create;
  Names.CaseSensitive := True;
  try
    CollectPatternBindingNames(APattern, Names);
    for I := 0 to Names.Count - 1 do
      AScope.DeclareVarLocal(Names[I]);
  finally
    Names.Free;
  end;
end;

procedure EmitGlobalDefinesForPattern(const ACtx: TGocciaCompilationContext;
  const APattern: TGocciaDestructuringPattern; const AIsConst: Boolean;
  const AIsVar: Boolean; const AHasInitializer: Boolean);
var
  ObjPat: TGocciaObjectDestructuringPattern;
  ArrPat: TGocciaArrayDestructuringPattern;
  AssignPat: TGocciaAssignmentDestructuringPattern;
  LocalIdx: Integer;
  I: Integer;
begin
  if APattern is TGocciaIdentifierDestructuringPattern then
  begin
    LocalIdx := ACtx.Scope.ResolveLocal(TGocciaIdentifierDestructuringPattern(APattern).Name);
    if LocalIdx >= 0 then
    begin
      ACtx.Scope.MarkGlobalBacked(LocalIdx);
      EmitGlobalDefine(ACtx, ACtx.Scope.GetLocal(LocalIdx).Slot,
        ACtx.Scope.GetLocal(LocalIdx).Name, AIsConst, AIsVar,
        AHasInitializer);
    end;
  end
  else if APattern is TGocciaObjectDestructuringPattern then
  begin
    ObjPat := TGocciaObjectDestructuringPattern(APattern);
    for I := 0 to ObjPat.Properties.Count - 1 do
      EmitGlobalDefinesForPattern(ACtx, ObjPat.Properties[I].Pattern,
        AIsConst, AIsVar, AHasInitializer);
  end
  else if APattern is TGocciaArrayDestructuringPattern then
  begin
    ArrPat := TGocciaArrayDestructuringPattern(APattern);
    for I := 0 to ArrPat.Elements.Count - 1 do
      if Assigned(ArrPat.Elements[I]) then
        EmitGlobalDefinesForPattern(ACtx, ArrPat.Elements[I], AIsConst,
          AIsVar, AHasInitializer);
  end
  else if APattern is TGocciaAssignmentDestructuringPattern then
  begin
    AssignPat := TGocciaAssignmentDestructuringPattern(APattern);
    EmitGlobalDefinesForPattern(ACtx, AssignPat.Left, AIsConst, AIsVar,
      AHasInitializer);
  end
  else if APattern is TGocciaRestDestructuringPattern then
    EmitGlobalDefinesForPattern(ACtx,
      TGocciaRestDestructuringPattern(APattern).Argument, AIsConst, AIsVar,
      AHasInitializer);
end;

procedure CompileDestructuringDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaDestructuringDeclaration);
var
  SrcReg: UInt8;
  IsTopLevelGlobalBacked: Boolean;
begin
  IsTopLevelGlobalBacked := ACtx.GlobalBackedTopLevel and
    (AStmt.IsVar or (ACtx.Scope.Depth = 0));

  if AStmt.IsVar then
    CollectDestructuringVarBindings(AStmt.Pattern, ACtx.Scope)
  else
    CollectDestructuringBindings(AStmt.Pattern, ACtx.Scope, AStmt.IsConst);

  if IsTopLevelGlobalBacked and AStmt.IsVar then
    EmitGlobalDefinesForPattern(ACtx, AStmt.Pattern, False, True, False);

  SrcReg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AStmt.Initializer, SrcReg);
  EmitDestructuring(ACtx, AStmt.Pattern, SrcReg,
    IsTopLevelGlobalBacked and AStmt.IsVar);
  ACtx.Scope.FreeRegister;

  if IsTopLevelGlobalBacked then
  begin
    if not AStmt.IsVar then
      EmitGlobalDefinesForPattern(ACtx, AStmt.Pattern, AStmt.IsConst,
        False, True);
  end;
end;

procedure CompileEnumDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaEnumDeclaration);
var
  EnumSlot, InnerSlot, MemberSlot: UInt8;
  I: Integer;
  KeyIdx: UInt16;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount, J: Integer;
  LocalIdx: Integer;
begin
  // Reuse pre-declared slot if it exists at the same scope depth
  // (for function declaration upvalue resolution)
  LocalIdx := ACtx.Scope.ResolveLocal(AStmt.Name);
  if (LocalIdx >= 0) and
     (ACtx.Scope.GetLocal(LocalIdx).Depth = ACtx.Scope.Depth) then
    EnumSlot := ACtx.Scope.GetLocal(LocalIdx).Slot
  else
    EnumSlot := ACtx.Scope.DeclareLocal(AStmt.Name, False);
  if ACtx.GlobalBackedTopLevel and (ACtx.Scope.Depth = 0) then
  begin
    LocalIdx := ACtx.Scope.ResolveLocal(AStmt.Name);
    if LocalIdx >= 0 then
      ACtx.Scope.MarkGlobalBacked(LocalIdx);
  end;
  EmitInstruction(ACtx, EncodeABx(OP_NEW_OBJECT, EnumSlot,
    Length(AStmt.Members)));

  ACtx.Scope.BeginScope;

  InnerSlot := ACtx.Scope.DeclareLocal(AStmt.Name, False);
  EmitInstruction(ACtx, EncodeABC(OP_MOVE, InnerSlot, EnumSlot, 0));

  for I := 0 to High(AStmt.Members) do
  begin
    MemberSlot := ACtx.Scope.DeclareLocal(AStmt.Members[I].Name, False);
    ACtx.CompileExpression(AStmt.Members[I].Initializer, MemberSlot);
    KeyIdx := ACtx.Template.AddConstantString(AStmt.Members[I].Name);
    if KeyIdx > High(UInt8) then
      raise Exception.Create('Constant pool overflow: enum member name index exceeds 255');
    EmitInstruction(ACtx, EncodeABC(OP_SET_PROP_CONST, EnumSlot, UInt8(KeyIdx), MemberSlot));
  end;

  KeyIdx := ACtx.Template.AddConstantString(AStmt.Name);
  if KeyIdx > High(UInt8) then
    raise Exception.Create('Constant pool overflow: enum name index exceeds 255');
  EmitInstruction(ACtx, EncodeABC(OP_FINALIZE_ENUM, EnumSlot,
    0, UInt8(KeyIdx)));

  ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
  for J := 0 to ClosedCount - 1 do
    EmitInstruction(ACtx, EncodeABx(OP_CLOSE_UPVALUE, ClosedLocals[J], 0));

  // Sync cell if the enum local was pre-declared and captured by a hoisted
  // function (see CompileVariableDeclaration for the full explanation)
  LocalIdx := ACtx.Scope.ResolveLocal(AStmt.Name);
  if (LocalIdx >= 0) and ACtx.Scope.GetLocal(LocalIdx).IsCaptured then
    EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, EnumSlot, UInt16(EnumSlot)));

  if ACtx.GlobalBackedTopLevel and (ACtx.Scope.Depth = 0) then
    EmitGlobalDefine(ACtx, EnumSlot, AStmt.Name, False);
end;

procedure CompileExportEnumDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExportEnumDeclaration);
var
  LocalIdx: Integer;
  Reg: UInt8;
  NameIdx: UInt16;
begin
  CompileEnumDeclaration(ACtx, AStmt.Declaration);
  LocalIdx := ACtx.Scope.ResolveLocal(AStmt.Declaration.Name);
  if LocalIdx >= 0 then
  begin
    Reg := ACtx.Scope.GetLocal(LocalIdx).Slot;
    NameIdx := ACtx.Template.AddConstantString(AStmt.Declaration.Name);
    EmitInstruction(ACtx, EncodeABx(OP_EXPORT, Reg, NameIdx));
  end;
end;

function SavePendingFinally: TObject;
begin
  Result := TObject(GPendingFinally);
  GPendingFinally := nil;
end;

procedure RestorePendingFinally(const ASaved: TObject);
begin
  GPendingFinally.Free;
  GPendingFinally := TList<TPendingFinallyEntry>(ASaved);
end;

end.
