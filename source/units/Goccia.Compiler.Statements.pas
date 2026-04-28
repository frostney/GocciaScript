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
procedure CompileBlockStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaBlockStatement);
procedure CompileIfStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaIfStatement);
procedure CompileReturnStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaReturnStatement);
procedure CompileThrowStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaThrowStatement);
procedure CompileTryStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaTryStatement);
procedure CompileForOfStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaForOfStatement);
procedure CompileForAwaitOfStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaForAwaitOfStatement);
procedure CompileImportDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaImportDeclaration);
procedure CompileExportDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExportDeclaration);
procedure CompileExportVariableDeclaration(
  const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExportVariableDeclaration);
procedure CompileReExportDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaReExportDeclaration);
procedure CompileSwitchStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaSwitchStatement);
procedure CompileBreakStatement(const ACtx: TGocciaCompilationContext);
procedure CompileContinueStatement(const ACtx: TGocciaCompilationContext);
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

function TypeAnnotationToLocalType(const AAnnotation: string): TGocciaLocalType;
function IsArrayTypeAnnotation(const AAnnotation: string): Boolean;
function StripArrayLayer(const AAnnotation: string): string;
function InferLocalType(const AExpr: TGocciaExpression): TGocciaLocalType;
function ExpressionType(const AScope: TGocciaCompilerScope;
  const AExpr: TGocciaExpression): TGocciaLocalType;
function CharToLocalType(const ACh: Char): TGocciaLocalType;

procedure CollectDestructuringVarBindings(const APattern: TGocciaDestructuringPattern;
  const AScope: TGocciaCompilerScope);

function SavePendingFinally: TObject;
procedure RestorePendingFinally(const ASaved: TObject);

implementation

uses
  SysUtils,

  OrderedStringMap,

  Goccia.Bytecode,
  Goccia.Bytecode.Debug,
  Goccia.Compiler.Expressions,
  Goccia.Compiler.PatternMatching,
  Goccia.Constants.PropertyNames,
  Goccia.Constants.TypeNames,
  Goccia.Keywords.Reserved,
  Goccia.Token,
  Goccia.Values.Primitives;

type
  TUsingResourceEntry = record
    ValueSlot: UInt8;
    DisposeSlot: UInt8;
    IsAwait: Boolean;
  end;

  TPendingFinallyEntry = record
    FinallyBlock: TGocciaBlockStatement;
    // Non-nil when this entry represents a using block's disposal.
    // CompileReturnStatement emits the disposal sequence instead of
    // compiling a FinallyBlock when this array is populated.
    UsingResources: array of TUsingResourceEntry;
    UsingErrorReg: UInt8;
  end;

threadvar
  GBreakJumps: TList<Integer>;
  GContinueJumps: TList<Integer>;
  GPendingFinally: TList<TPendingFinallyEntry>;
  GBreakFinallyBase: Integer;
  GContinueFinallyBase: Integer;

procedure CompileExpressionStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExpressionStatement);
var
  Reg: UInt8;
begin
  Reg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AStmt.Expression, Reg);
  ACtx.Scope.FreeRegister;
end;

function InferLocalType(const AExpr: TGocciaExpression): TGocciaLocalType;
var
  Lit: TGocciaLiteralExpression;
begin
  Result := sltUntyped;
  if AExpr is TGocciaLiteralExpression then
  begin
    Lit := TGocciaLiteralExpression(AExpr);
    if Lit.Value is TGocciaNumberLiteralValue then
      Result := sltFloat
    else if Lit.Value is TGocciaBooleanLiteralValue then
      Result := sltBoolean
    else if Lit.Value is TGocciaStringLiteralValue then
      Result := sltString;
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
  else if AExpr is TGocciaMethodExpression then
    Result := sltReference;
end;

function TypeAnnotationToLocalType(const AAnnotation: string): TGocciaLocalType;
begin
  Result := sltUntyped;
  if AAnnotation = '' then
    Exit;
  if Pos('|', AAnnotation) > 0 then
    Exit;
  if AAnnotation = NUMBER_TYPE_NAME then
    Result := sltFloat
  else if AAnnotation = STRING_TYPE_NAME then
    Result := sltString
  else if AAnnotation = BOOLEAN_TYPE_NAME then
    Result := sltBoolean
  else if (AAnnotation = OBJECT_TYPE_NAME) or (AAnnotation = 'Object')
       or (AAnnotation = 'Function')
       or (Pos('<', AAnnotation) > 0)
       or (Pos('[', AAnnotation) > 0)
       or (Pos('{', AAnnotation) > 0)
       or (Pos('=>', AAnnotation) > 0) then
    Result := sltReference;
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
  Result := ATokenType in [gttMinus, gttStar, gttSlash, gttPercent, gttPower];
end;

function IsComparisonOp(const ATokenType: TGocciaTokenType): Boolean;
begin
  Result := ATokenType in [gttLess, gttGreater, gttLessEqual, gttGreaterEqual,
    gttEqual, gttNotEqual];
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
  LocalIdx: Integer;
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
      if AScope.GetLocal(LocalIdx).IsConst or
         AScope.GetLocal(LocalIdx).IsStrictlyTyped then
        Result := AScope.GetLocal(LocalIdx).TypeHint;
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
  else if AExpr is TGocciaMethodExpression then
    Result := sltReference;
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

{ B field encodes the declaration mode for OP_DEFINE_GLOBAL_CONST:
  1 = let declaration (throws on re-declaration)
  2 = const declaration (throws on re-declaration, non-writable) }
const
  GLOBAL_DEFINE_LET = 1;
  GLOBAL_DEFINE_CONST = 2;

procedure EmitGlobalDefine(const ACtx: TGocciaCompilationContext;
  const ASlot: UInt8; const AName: string; const AIsConst: Boolean);
var
  NameIdx: UInt16;
  DeclMode: UInt8;
begin
  if AIsConst then
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
  IsStrict, HasRealInitializer, IsTopLevelGlobalBacked, IsVarRedeclaration: Boolean;
begin
  for I := 0 to High(AStmt.Variables) do
  begin
    Info := AStmt.Variables[I];
    if AStmt.IsVar then
    begin
      // Track whether this is a redeclaration (slot already exists)
      LocalIdx := ACtx.Scope.ResolveLocal(Info.Name);
      IsVarRedeclaration := LocalIdx >= 0;
      Slot := ACtx.Scope.DeclareVarLocal(Info.Name);
    end
    else
    begin
      // Check if the local was pre-declared (for function declaration
      // upvalue resolution) and reuse the existing slot, but only at the
      // same scope depth to preserve block-scoping and shadowing
      LocalIdx := ACtx.Scope.ResolveLocal(Info.Name);
      if (LocalIdx >= 0) and
         (ACtx.Scope.GetLocal(LocalIdx).Depth = ACtx.Scope.Depth) then
        Slot := ACtx.Scope.GetLocal(LocalIdx).Slot
      else
        Slot := ACtx.Scope.DeclareLocal(Info.Name, AStmt.IsConst);
      IsVarRedeclaration := False;
    end;

    IsTopLevelGlobalBacked := ACtx.GlobalBackedTopLevel and
      (ACtx.Scope.Depth = 0);
    if IsTopLevelGlobalBacked then
    begin
      LocalIdx := ACtx.Scope.ResolveLocal(Info.Name);
      if LocalIdx >= 0 then
        ACtx.Scope.MarkGlobalBacked(LocalIdx);
    end;

    HasRealInitializer := Assigned(Info.Initializer) and
                          not IsUndefinedInitializer(Info.Initializer);

    AnnotationType := TypeAnnotationToLocalType(Info.TypeAnnotation);

    if AnnotationType <> sltUntyped then
      TypeHint := AnnotationType
    else if (Info.TypeAnnotation = '') and HasRealInitializer then
      TypeHint := InferLocalType(Info.Initializer)
    else
      TypeHint := sltUntyped;

    IsStrict := TypeHint <> sltUntyped;

    if IsStrict then
    begin
      LocalIdx := ACtx.Scope.ResolveLocal(Info.Name);
      if LocalIdx >= 0 then
      begin
        ACtx.Scope.SetLocalTypeHint(LocalIdx, TypeHint);
        ACtx.Template.SetLocalType(Slot, TypeHint);
        ACtx.Scope.SetLocalStrictlyTyped(LocalIdx, True);
        ACtx.Template.SetLocalStrictFlag(Slot, True);
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
       not (AStmt.IsVar and (not HasRealInitializer) and IsVarRedeclaration) then
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
         (Info.Initializer is TGocciaMethodExpression) then
      begin
        if ACtx.Template.FunctionCount > FuncCount then
        begin
          InferredTemplate := ACtx.Template.GetFunction(
            ACtx.Template.FunctionCount - 1);
          if (InferredTemplate.Name = '<arrow>') or
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
    if not AStmt.IsVar then
    begin
      LocalIdx := ACtx.Scope.ResolveLocal(Info.Name);
      if (LocalIdx >= 0) and ACtx.Scope.GetLocal(LocalIdx).IsCaptured then
        EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, Slot, UInt16(Slot)));
    end;

    if IsTopLevelGlobalBacked then
      EmitGlobalDefine(ACtx, Slot, Info.Name, AStmt.IsConst);
  end;
end;

threadvar
  // Module-level stack of using resource entries for the current block.
  // Populated by CompileUsingDeclaration, consumed by CompileBlockStatement.
  GUsingResources: TList<TUsingResourceEntry>;

// TC39 Explicit Resource Management: compile using / await using declaration.
// Emits OP_USING_INIT for each variable to validate and extract the dispose method.
// Also updates the topmost GPendingFinally entry so that CompileReturnStatement
// and CompileBreakStatement can emit the correct disposal sequence.
procedure CompileUsingDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaUsingDeclaration);
var
  I, Len: Integer;
  Info: TGocciaVariableInfo;
  ValueSlot, DisposeSlot: UInt8;
  Flags: UInt8;
  Entry: TUsingResourceEntry;
  PendingIdx: Integer;
  PendingEntry: TPendingFinallyEntry;
begin
  for I := 0 to High(AStmt.Variables) do
  begin
    Info := AStmt.Variables[I];

    // Declare the using variable as const
    ValueSlot := ACtx.Scope.DeclareLocal(Info.Name, True);

    // Compile the initializer into the value slot
    ACtx.CompileExpression(Info.Initializer, ValueSlot);

    // Allocate a hidden register for the dispose method
    DisposeSlot := ACtx.Scope.AllocateRegister;

    // OP_USING_INIT: A=disposeMethodDest, B=valueSource, C=flags (0=sync, 1=async)
    if AStmt.IsAwait then
      Flags := 1
    else
      Flags := 0;
    EmitInstruction(ACtx, EncodeABC(OP_USING_INIT, DisposeSlot, ValueSlot, Flags));

    // Track for disposal at block exit
    Entry.ValueSlot := ValueSlot;
    Entry.DisposeSlot := DisposeSlot;
    Entry.IsAwait := AStmt.IsAwait;
    if not Assigned(GUsingResources) then
      GUsingResources := TList<TUsingResourceEntry>.Create;
    GUsingResources.Add(Entry);

    // Update the topmost pending-finally entry so return/break can find resources
    if Assigned(GPendingFinally) and (GPendingFinally.Count > 0) then
    begin
      PendingIdx := GPendingFinally.Count - 1;
      PendingEntry := GPendingFinally[PendingIdx];
      if not Assigned(PendingEntry.FinallyBlock) then
      begin
        Len := Length(PendingEntry.UsingResources);
        SetLength(PendingEntry.UsingResources, Len + 1);
        PendingEntry.UsingResources[Len] := Entry;
        GPendingFinally[PendingIdx] := PendingEntry;
      end;
    end;
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

procedure CompileBlockStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaBlockStatement);
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
begin
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
    begin
      Node := AStmt.Nodes[I];
      if Node is TGocciaStatement then
        ACtx.CompileStatement(TGocciaStatement(Node))
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

  // Remember the starting point of using resources for this block
  if not Assigned(GUsingResources) then
    GUsingResources := TList<TUsingResourceEntry>.Create;
  SavedResourceBase := GUsingResources.Count;

  // Allocate registers for exception handling
  CatchReg := ACtx.Scope.AllocateRegister;
  ErrorReg := ACtx.Scope.AllocateRegister;

  // Initialize error accumulator to null (no error yet)
  EmitInstruction(ACtx, EncodeABC(OP_LOAD_NULL, ErrorReg, 0, 0));

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
    if Node is TGocciaStatement then
      ACtx.CompileStatement(TGocciaStatement(Node))
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
  NullishJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH, ErrorReg);
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

procedure CompileIfStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaIfStatement);
var
  CondReg, PatternSubjectReg: UInt8;
  ElseJump, EndJump: Integer;
  HasPatternBindings: Boolean;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount, I: Integer;
begin
  CondReg := ACtx.Scope.AllocateRegister;
  HasPatternBindings := CompileConditionWithPatternBindings(ACtx,
    AStmt.Condition, CondReg, PatternSubjectReg);
  if not HasPatternBindings then
    ACtx.CompileExpression(AStmt.Condition, CondReg);

  ElseJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, CondReg);
  ACtx.CompileStatement(AStmt.Consequent);

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
    ACtx.CompileStatement(AStmt.Alternate);
    PatchJumpTarget(ACtx, EndJump);
  end
  else
    PatchJumpTarget(ACtx, ElseJump);

  ACtx.Scope.FreeRegister;
end;

// Emit pending finally/disposal blocks before a return or abrupt exit.
// Handles both regular try/finally blocks and using-block disposal entries.
// Processes entries from innermost to outermost. Each entry is temporarily
// popped so that if a finally block contains a nested return, that return
// sees the remaining outer entries for its own cleanup.
procedure EmitPendingCleanup(const ACtx: TGocciaCompilationContext);
var
  Entry: TPendingFinallyEntry;
  NullishJump: Integer;
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
    EmitInstruction(ACtx, EncodeABC(OP_POP_HANDLER, 0, 0, 0));
    if Assigned(Entry.FinallyBlock) then
      CompileBlockStatement(ACtx, Entry.FinallyBlock)
    else if Length(Entry.UsingResources) > 0 then
    begin
      EmitDisposalSequence(ACtx, Entry.UsingResources,
        Length(Entry.UsingResources), Entry.UsingErrorReg);
      NullishJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH,
        Entry.UsingErrorReg);
      EmitInstruction(ACtx, EncodeABC(OP_THROW, Entry.UsingErrorReg, 0, 0));
      PatchJumpTarget(ACtx, NullishJump);
    end;
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
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, 0, 0, 0));
    EmitInstruction(ACtx, EncodeABC(OP_RETURN, 0, 0, 0));
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
  CatchReg, PatternTestReg: UInt8;
  HandlerJump, EndJump, PatternMismatchJump, CatchSuccessJump: Integer;
  HasCatch, HasFinally: Boolean;
  I: Integer;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount: Integer;
  Entry: TPendingFinallyEntry;
begin
  HasCatch := Assigned(AStmt.CatchBlock);
  HasFinally := Assigned(AStmt.FinallyBlock);

  CatchReg := ACtx.Scope.AllocateRegister;
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

    if AStmt.CatchParam <> '' then
    begin
      ACtx.Scope.BeginScope;
      ACtx.Scope.DeclareLocal(AStmt.CatchParam, False);
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ACtx.Scope.NextSlot - 1, CatchReg, 0));
    end;

    if Assigned(AStmt.CatchPattern) then
    begin
      DeclarePatternBindings(ACtx, AStmt.CatchPattern);
      CompilePatternTest(ACtx, CatchReg, AStmt.CatchPattern, PatternTestReg);
      PatternMismatchJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE,
        PatternTestReg);
    end;

    CompileBlockStatement(ACtx, AStmt.CatchBlock);

    if AStmt.CatchParam <> '' then
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
      if HasFinally then
        CompileBlockStatement(ACtx, AStmt.FinallyBlock);
      EmitInstruction(ACtx, EncodeABC(OP_THROW, CatchReg, 0, 0));
      PatchJumpTarget(ACtx, CatchSuccessJump);
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
  OldBreakJumps: TList<Integer>;
  OldBreakFinallyBase: Integer;
  BreakJumps: TList<Integer>;
  OldContinueJumps: TList<Integer>;
  OldContinueFinallyBase: Integer;
  ContinueJumps: TList<Integer>;
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

  OldBreakJumps := GBreakJumps;
  OldBreakFinallyBase := GBreakFinallyBase;
  BreakJumps := TList<Integer>.Create;
  GBreakJumps := BreakJumps;
  OldContinueJumps := GContinueJumps;
  OldContinueFinallyBase := GContinueFinallyBase;
  ContinueJumps := TList<Integer>.Create;
  GContinueJumps := ContinueJumps;
  if Assigned(GPendingFinally) then
  begin
    GBreakFinallyBase := GPendingFinally.Count;
    GContinueFinallyBase := GPendingFinally.Count;
  end
  else
  begin
    GBreakFinallyBase := 0;
    GContinueFinallyBase := 0;
  end;
  try
    LoopStart := CurrentCodePosition(ACtx);
    MismatchJump := -1;

    EmitInstruction(ACtx, EncodeABC(OP_GTE_INT, CmpReg, IdxReg, LenReg));
    ExitJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, CmpReg);

    EmitInstruction(ACtx, EncodeABC(OP_ARRAY_GET, ValueReg, ArrReg, IdxReg));

    ACtx.Scope.BeginScope;

    if Assigned(AStmt.BindingPattern) then
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
          if ElemType <> sltUntyped then
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
    for I := 0 to ContinueJumps.Count - 1 do
      PatchJumpTarget(ACtx, ContinueJumps[I]);
    if MismatchJump >= 0 then
      PatchJumpTarget(ACtx, MismatchJump);

    ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
    for I := 0 to ClosedCount - 1 do
      EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));

    EmitInstruction(ACtx, EncodeABC(OP_ADD_INT, IdxReg, IdxReg, OneReg));
    EmitInstruction(ACtx, EncodeAx(OP_JUMP, LoopStart - CurrentCodePosition(ACtx) - 1));

    PatchJumpTarget(ACtx, ExitJump);

    for I := 0 to BreakJumps.Count - 1 do
      PatchJumpTarget(ACtx, BreakJumps[I]);
  finally
    BreakJumps.Free;
    GBreakJumps := OldBreakJumps;
    GBreakFinallyBase := OldBreakFinallyBase;
    ContinueJumps.Free;
    GContinueJumps := OldContinueJumps;
    GContinueFinallyBase := OldContinueFinallyBase;
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
  IterReg, ValueReg, DoneReg: UInt8;
  LoopStart, ExitJump, MismatchJump, I: Integer;
  Slot: UInt8;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount: Integer;
  OldBreakJumps: TList<Integer>;
  OldBreakFinallyBase: Integer;
  BreakJumps: TList<Integer>;
  OldContinueJumps: TList<Integer>;
  OldContinueFinallyBase: Integer;
  ContinueJumps: TList<Integer>;
  ArrayLocalIdx: Integer;
begin
  if IsConstArrayLocal(ACtx, AStmt.Iterable, ArrayLocalIdx) then
  begin
    CompileCountedForOf(ACtx, AStmt, ArrayLocalIdx);
    Exit;
  end;

  IterReg := ACtx.Scope.AllocateRegister;
  ValueReg := ACtx.Scope.AllocateRegister;
  DoneReg := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(AStmt.Iterable, IterReg);
  EmitInstruction(ACtx, EncodeABC(OP_GET_ITER, IterReg, IterReg, 0));

  OldBreakJumps := GBreakJumps;
  OldBreakFinallyBase := GBreakFinallyBase;
  BreakJumps := TList<Integer>.Create;
  GBreakJumps := BreakJumps;
  OldContinueJumps := GContinueJumps;
  OldContinueFinallyBase := GContinueFinallyBase;
  ContinueJumps := TList<Integer>.Create;
  GContinueJumps := ContinueJumps;
  if Assigned(GPendingFinally) then
  begin
    GBreakFinallyBase := GPendingFinally.Count;
    GContinueFinallyBase := GPendingFinally.Count;
  end
  else
  begin
    GBreakFinallyBase := 0;
    GContinueFinallyBase := 0;
  end;
  try
    LoopStart := CurrentCodePosition(ACtx);
    MismatchJump := -1;

    EmitInstruction(ACtx, EncodeABC(OP_ITER_NEXT, ValueReg, DoneReg, IterReg));
    ExitJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, DoneReg);

    ACtx.Scope.BeginScope;

    if Assigned(AStmt.BindingPattern) then
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
    for I := 0 to ContinueJumps.Count - 1 do
      PatchJumpTarget(ACtx, ContinueJumps[I]);
    if MismatchJump >= 0 then
      PatchJumpTarget(ACtx, MismatchJump);

    ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
    for I := 0 to ClosedCount - 1 do
      EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));

    EmitInstruction(ACtx, EncodeAx(OP_JUMP, LoopStart - CurrentCodePosition(ACtx) - 1));

    PatchJumpTarget(ACtx, ExitJump);

    for I := 0 to BreakJumps.Count - 1 do
      PatchJumpTarget(ACtx, BreakJumps[I]);
  finally
    BreakJumps.Free;
    GBreakJumps := OldBreakJumps;
    GBreakFinallyBase := OldBreakFinallyBase;
    ContinueJumps.Free;
    GContinueJumps := OldContinueJumps;
    GContinueFinallyBase := OldContinueFinallyBase;
  end;

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileForAwaitOfStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaForAwaitOfStatement);
var
  IterReg, ValueReg, DoneReg: UInt8;
  LoopStart, ExitJump, MismatchJump, I: Integer;
  Slot: UInt8;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount: Integer;
  OldBreakJumps: TList<Integer>;
  OldBreakFinallyBase: Integer;
  BreakJumps: TList<Integer>;
  OldContinueJumps: TList<Integer>;
  OldContinueFinallyBase: Integer;
  ContinueJumps: TList<Integer>;
begin
  IterReg := ACtx.Scope.AllocateRegister;
  ValueReg := ACtx.Scope.AllocateRegister;
  DoneReg := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(AStmt.Iterable, IterReg);
  EmitInstruction(ACtx, EncodeABC(OP_GET_ITER, IterReg, IterReg, 1));

  OldBreakJumps := GBreakJumps;
  OldBreakFinallyBase := GBreakFinallyBase;
  BreakJumps := TList<Integer>.Create;
  GBreakJumps := BreakJumps;
  OldContinueJumps := GContinueJumps;
  OldContinueFinallyBase := GContinueFinallyBase;
  ContinueJumps := TList<Integer>.Create;
  GContinueJumps := ContinueJumps;
  if Assigned(GPendingFinally) then
  begin
    GBreakFinallyBase := GPendingFinally.Count;
    GContinueFinallyBase := GPendingFinally.Count;
  end
  else
  begin
    GBreakFinallyBase := 0;
    GContinueFinallyBase := 0;
  end;
  try
    LoopStart := CurrentCodePosition(ACtx);
    MismatchJump := -1;

    EmitInstruction(ACtx, EncodeABC(OP_ITER_NEXT, ValueReg, DoneReg, IterReg));
    ExitJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, DoneReg);
    EmitInstruction(ACtx, EncodeABC(OP_AWAIT, ValueReg, ValueReg, 0));

    ACtx.Scope.BeginScope;

    if Assigned(AStmt.BindingPattern) then
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
    for I := 0 to ContinueJumps.Count - 1 do
      PatchJumpTarget(ACtx, ContinueJumps[I]);
    if MismatchJump >= 0 then
      PatchJumpTarget(ACtx, MismatchJump);

    ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
    for I := 0 to ClosedCount - 1 do
      EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));

    EmitInstruction(ACtx, EncodeAx(OP_JUMP, LoopStart - CurrentCodePosition(ACtx) - 1));

    PatchJumpTarget(ACtx, ExitJump);

    for I := 0 to BreakJumps.Count - 1 do
      PatchJumpTarget(ACtx, BreakJumps[I]);
  finally
    BreakJumps.Free;
    GBreakJumps := OldBreakJumps;
    GBreakFinallyBase := OldBreakFinallyBase;
    ContinueJumps.Free;
    GContinueJumps := OldContinueJumps;
    GContinueFinallyBase := OldContinueFinallyBase;
  end;

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
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
  HasNamespace: Boolean;
  I, Count: Integer;
begin
  HasNamespace := AStmt.NamespaceName <> '';
  Count := AStmt.Imports.Count;
  SetLength(Slots, Count);
  SetLength(Names, Count);

  if HasNamespace then
    NamespaceSlot := ACtx.Scope.DeclareLocal(AStmt.NamespaceName, True);

  I := 0;
  for Pair in AStmt.Imports do
  begin
    Slots[I] := ACtx.Scope.DeclareLocal(Pair.Key, True);
    Names[I] := Pair.Value;
    Inc(I);
  end;

  ModReg := ACtx.Scope.AllocateRegister;
  PathIdx := ACtx.Template.AddConstantString(AStmt.ModulePath);
  EmitInstruction(ACtx, EncodeABx(OP_IMPORT, ModReg, PathIdx));

  if HasNamespace then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, NamespaceSlot, ModReg, 0));

  for I := 0 to Count - 1 do
  begin
    NameIdx := ACtx.Template.AddConstantString(Names[I]);
    if NameIdx > High(UInt8) then
      raise Exception.Create('Constant pool overflow: import name index exceeds 255');
    EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, Slots[I], ModReg,
      UInt8(NameIdx)));
  end;

  ACtx.Scope.FreeRegister;
end;

procedure CompileExportDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExportDeclaration);
var
  Pair: TStringStringMap.TKeyValuePair;
  LocalIdx: Integer;
  Reg: UInt8;
  NameIdx: UInt16;
begin
  for Pair in AStmt.ExportsTable do
  begin
    LocalIdx := ACtx.Scope.ResolveLocal(Pair.Value);
    if LocalIdx >= 0 then
    begin
      Reg := ACtx.Scope.GetLocal(LocalIdx).Slot;
      NameIdx := ACtx.Template.AddConstantString(Pair.Key);
      EmitInstruction(ACtx, EncodeABx(OP_EXPORT, Reg, NameIdx));
    end;
  end;
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

procedure CompileReExportDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaReExportDeclaration);
var
  ModReg, ValReg: UInt8;
  PathIdx, SrcNameIdx, ExportNameIdx: UInt16;
  Pair: TStringStringMap.TKeyValuePair;
begin
  ModReg := ACtx.Scope.AllocateRegister;
  ValReg := ACtx.Scope.AllocateRegister;
  PathIdx := ACtx.Template.AddConstantString(AStmt.ModulePath);
  EmitInstruction(ACtx, EncodeABx(OP_IMPORT, ModReg, PathIdx));

  for Pair in AStmt.ExportsTable do
  begin
    SrcNameIdx := ACtx.Template.AddConstantString(Pair.Value);
    if SrcNameIdx > High(UInt8) then
      raise Exception.Create('Constant pool overflow: re-export source name index exceeds 255');
    EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, ValReg, ModReg,
      UInt8(SrcNameIdx)));
    ExportNameIdx := ACtx.Template.AddConstantString(Pair.Key);
    EmitInstruction(ACtx, EncodeABx(OP_EXPORT, ValReg, ExportNameIdx));
  end;

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
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
  BreakJumps: TList<Integer>;
  Node: TGocciaASTNode;
  Reg: UInt8;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount: Integer;
begin
  DiscReg := ACtx.Scope.AllocateRegister;
  TestReg := ACtx.Scope.AllocateRegister;
  CmpReg := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(AStmt.Discriminant, DiscReg);

  SetLength(CaseBodyJumps, AStmt.Cases.Count);
  DefaultIndex := -1;
  DefaultJump := -1;
  EndJump := -1;

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
  ACtx.Scope.FreeRegister;

  OldBreakJumps := GBreakJumps;
  OldBreakFinallyBase := GBreakFinallyBase;
  BreakJumps := TList<Integer>.Create;
  GBreakJumps := BreakJumps;
  if Assigned(GPendingFinally) then
    GBreakFinallyBase := GPendingFinally.Count
  else
    GBreakFinallyBase := 0;
  try
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

      ACtx.Scope.BeginScope;
      for J := 0 to CaseClause.Consequent.Count - 1 do
      begin
        Node := CaseClause.Consequent[J];
        if Node is TGocciaStatement then
          ACtx.CompileStatement(TGocciaStatement(Node))
        else if Node is TGocciaExpression then
        begin
          Reg := ACtx.Scope.AllocateRegister;
          ACtx.CompileExpression(TGocciaExpression(Node), Reg);
          ACtx.Scope.FreeRegister;
        end;
      end;
      ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
      for J := 0 to ClosedCount - 1 do
        EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[J], 0, 0));
    end;

    if EndJump >= 0 then
      PatchJumpTarget(ACtx, EndJump);

    for I := 0 to BreakJumps.Count - 1 do
      PatchJumpTarget(ACtx, BreakJumps[I]);
  finally
    BreakJumps.Free;
    GBreakJumps := OldBreakJumps;
    GBreakFinallyBase := OldBreakFinallyBase;
  end;
end;

procedure CompileBreakStatement(const ACtx: TGocciaCompilationContext);
var
  I: Integer;
  Entry: TPendingFinallyEntry;
  NullishJump: Integer;
begin
  if not Assigned(GBreakJumps) then
    Exit;

  if Assigned(GPendingFinally) and (GPendingFinally.Count > GBreakFinallyBase) then
    for I := GPendingFinally.Count - 1 downto GBreakFinallyBase do
    begin
      Entry := GPendingFinally[I];
      EmitInstruction(ACtx, EncodeABC(OP_POP_HANDLER, 0, 0, 0));
      if Assigned(Entry.FinallyBlock) then
        CompileBlockStatement(ACtx, Entry.FinallyBlock)
      else if Length(Entry.UsingResources) > 0 then
      begin
        EmitDisposalSequence(ACtx, Entry.UsingResources,
          Length(Entry.UsingResources), Entry.UsingErrorReg);
        NullishJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH,
          Entry.UsingErrorReg);
        EmitInstruction(ACtx, EncodeABC(OP_THROW, Entry.UsingErrorReg, 0, 0));
        PatchJumpTarget(ACtx, NullishJump);
      end;
    end;

  GBreakJumps.Add(EmitJumpInstruction(ACtx, OP_JUMP, 0));
end;

procedure CompileContinueStatement(const ACtx: TGocciaCompilationContext);
var
  I, Count, Base: Integer;
  Entries: array of TPendingFinallyEntry;
  Entry: TPendingFinallyEntry;
  NullishJump: Integer;
begin
  if not Assigned(GContinueJumps) then
    Exit;

  if Assigned(GPendingFinally) and (GPendingFinally.Count > GContinueFinallyBase) then
  begin
    Count := GPendingFinally.Count;
    Base := GContinueFinallyBase;
    // Snapshot entries above the continue-finally base
    SetLength(Entries, Count - Base);
    for I := Base to Count - 1 do
      Entries[I - Base] := GPendingFinally[I];
    // Process from innermost to outermost, removing each before compiling
    // its cleanup so nested abrupt completions do not see the same entry.
    for I := Count - 1 downto Base do
    begin
      if GPendingFinally.Count > I then
        GPendingFinally.Delete(I);
      Entry := Entries[I - Base];
      EmitInstruction(ACtx, EncodeABC(OP_POP_HANDLER, 0, 0, 0));
      if Assigned(Entry.FinallyBlock) then
        CompileBlockStatement(ACtx, Entry.FinallyBlock)
      else if Length(Entry.UsingResources) > 0 then
      begin
        EmitDisposalSequence(ACtx, Entry.UsingResources,
          Length(Entry.UsingResources), Entry.UsingErrorReg);
        NullishJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH,
          Entry.UsingErrorReg);
        EmitInstruction(ACtx, EncodeABC(OP_THROW, Entry.UsingErrorReg, 0, 0));
        PatchJumpTarget(ACtx, NullishJump);
      end;
    end;
    // Restore entries so normal-path compilation still sees them
    for I := 0 to Length(Entries) - 1 do
      GPendingFinally.Insert(Base + I, Entries[I]);
  end;

  GContinueJumps.Add(EmitJumpInstruction(ACtx, OP_JUMP, 0));
end;

procedure CompileMethodBody(const ACtx: TGocciaCompilationContext;
  const AClassReg: UInt8; const AMethodName: string;
  const AMethod: TGocciaClassMethod; const AStoreOpcode: TGocciaOpCode);
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
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TGocciaFunctionTemplate.Create(
    '<method ' + AMethodName + '>');
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
    ChildScope.DeclareLocal(AMethod.Parameters[I].Name, False);
  end;
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

  if RestParamIndex >= 0 then
    EmitInstruction(ChildCtx, EncodeABC(OP_PACK_ARGS,
      UInt8(ChildScope.ResolveLocal(
        AMethod.Parameters[RestParamIndex].Name)),
      UInt8(RestParamIndex), 0));

  EmitDefaultParameters(ChildCtx, AMethod.Parameters);

  ACtx.CompileFunctionBody(AMethod.Body);
  ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

  for I := 0 to ChildScope.UpvalueCount - 1 do
    ChildTemplate.AddUpvalueDescriptor(
      ChildScope.GetUpvalue(I).IsLocal,
      ChildScope.GetUpvalue(I).Index);

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
  FuncIdx: UInt16;
  FnReg: UInt8;
  NameIdx: UInt16;
  I: Integer;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TGocciaFunctionTemplate.Create('<get ' + AName + '>');
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.SourceText := AGetter.SourceText;
  ChildTemplate.ParameterCount := 0;
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);
  ChildScope.DeclareLocal(KEYWORD_THIS, False);

  ACtx.SwapState(ChildTemplate, ChildScope);
  EmitLineMapping(ACtx, AGetter.Line, AGetter.Column);
  ACtx.CompileFunctionBody(AGetter.Body);
  ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

  for I := 0 to ChildScope.UpvalueCount - 1 do
    ChildTemplate.AddUpvalueDescriptor(
      ChildScope.GetUpvalue(I).IsLocal,
      ChildScope.GetUpvalue(I).Index);

  ACtx.SwapState(OldTemplate, OldScope);
  ChildScope.Free;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  FnReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, FnReg, FuncIdx));

  NameIdx := ACtx.Template.AddConstantString(AName);
  if NameIdx > High(UInt8) then
    raise Exception.Create('Constant pool overflow: getter name index exceeds 255');
  if FnReg <> ATargetReg + 1 then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ATargetReg + 1, FnReg, 0));
  EmitInstruction(ACtx, EncodeABC(AOpcode, ATargetReg, AFlags, UInt8(NameIdx)));
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
  FuncIdx: UInt16;
  FnReg: UInt8;
  NameIdx: UInt16;
  I: Integer;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TGocciaFunctionTemplate.Create('<set ' + AName + '>');
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.SourceText := ASetter.SourceText;
  ChildTemplate.ParameterCount := 1;
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);
  ChildScope.DeclareLocal(KEYWORD_THIS, False);
  ChildScope.DeclareLocal(ASetter.Parameter, False);

  ACtx.SwapState(ChildTemplate, ChildScope);
  EmitLineMapping(ACtx, ASetter.Line, ASetter.Column);
  ACtx.CompileFunctionBody(ASetter.Body);
  ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

  for I := 0 to ChildScope.UpvalueCount - 1 do
    ChildTemplate.AddUpvalueDescriptor(
      ChildScope.GetUpvalue(I).IsLocal,
      ChildScope.GetUpvalue(I).Index);

  ACtx.SwapState(OldTemplate, OldScope);
  ChildScope.Free;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  FnReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, FnReg, FuncIdx));

  NameIdx := ACtx.Template.AddConstantString(AName);
  if NameIdx > High(UInt8) then
    raise Exception.Create('Constant pool overflow: setter name index exceeds 255');
  if FnReg <> ATargetReg + 1 then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ATargetReg + 1, FnReg, 0));
  EmitInstruction(ACtx, EncodeABC(AOpcode, ATargetReg, AFlags, UInt8(NameIdx)));
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
  FuncIdx: UInt16;
  FnReg, SafeKeyReg: UInt8;
  I: Integer;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TGocciaFunctionTemplate.Create('<get [computed]>');
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.ParameterCount := 0;
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);
  ChildScope.DeclareLocal(KEYWORD_THIS, False);

  ACtx.SwapState(ChildTemplate, ChildScope);
  EmitLineMapping(ACtx, AGetter.Line, AGetter.Column);
  ACtx.CompileFunctionBody(AGetter.Body);
  ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

  for I := 0 to ChildScope.UpvalueCount - 1 do
    ChildTemplate.AddUpvalueDescriptor(
      ChildScope.GetUpvalue(I).IsLocal,
      ChildScope.GetUpvalue(I).Index);

  ACtx.SwapState(OldTemplate, OldScope);
  ChildScope.Free;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  FnReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, FnReg, FuncIdx));

  if AKeyReg = ATargetReg + 1 then
  begin
    SafeKeyReg := ACtx.Scope.AllocateRegister;
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, SafeKeyReg, AKeyReg, 0));
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ATargetReg + 1, FnReg, 0));
    EmitInstruction(ACtx, EncodeABC(AOpcode, ATargetReg, AFlags, SafeKeyReg));
    ACtx.Scope.FreeRegister;
  end
  else
  begin
    if FnReg <> ATargetReg + 1 then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ATargetReg + 1, FnReg, 0));
    EmitInstruction(ACtx, EncodeABC(AOpcode, ATargetReg, AFlags, AKeyReg));
  end;
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
  FuncIdx: UInt16;
  FnReg, SafeKeyReg: UInt8;
  I: Integer;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TGocciaFunctionTemplate.Create('<set [computed]>');
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.ParameterCount := 1;
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);
  ChildScope.DeclareLocal(KEYWORD_THIS, False);
  ChildScope.DeclareLocal(ASetter.Parameter, False);

  ACtx.SwapState(ChildTemplate, ChildScope);
  EmitLineMapping(ACtx, ASetter.Line, ASetter.Column);
  ACtx.CompileFunctionBody(ASetter.Body);
  ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

  for I := 0 to ChildScope.UpvalueCount - 1 do
    ChildTemplate.AddUpvalueDescriptor(
      ChildScope.GetUpvalue(I).IsLocal,
      ChildScope.GetUpvalue(I).Index);

  ACtx.SwapState(OldTemplate, OldScope);
  ChildScope.Free;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  FnReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, FnReg, FuncIdx));

  if AKeyReg = ATargetReg + 1 then
  begin
    SafeKeyReg := ACtx.Scope.AllocateRegister;
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, SafeKeyReg, AKeyReg, 0));
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ATargetReg + 1, FnReg, 0));
    EmitInstruction(ACtx, EncodeABC(AOpcode, ATargetReg, AFlags, SafeKeyReg));
    ACtx.Scope.FreeRegister;
  end
  else
  begin
    if FnReg <> ATargetReg + 1 then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ATargetReg + 1, FnReg, 0));
    EmitInstruction(ACtx, EncodeABC(AOpcode, ATargetReg, AFlags, AKeyReg));
  end;
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
    ChildScope.DeclareLocal(AMethod.Parameters[I].Name, False);
  end;
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

  if RestParamIndex >= 0 then
    EmitInstruction(ChildCtx, EncodeABC(OP_PACK_ARGS,
      UInt8(ChildScope.ResolveLocal(
        AMethod.Parameters[RestParamIndex].Name)),
      UInt8(RestParamIndex), 0));

  EmitDefaultParameters(ChildCtx, AMethod.Parameters);

  ACtx.CompileFunctionBody(AMethod.Body);
  ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

  for I := 0 to ChildScope.UpvalueCount - 1 do
    ChildTemplate.AddUpvalueDescriptor(
      ChildScope.GetUpvalue(I).IsLocal,
      ChildScope.GetUpvalue(I).Index);

  ACtx.SwapState(OldTemplate, OldScope);
  ChildScope.Free;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  FnReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, FnReg, FuncIdx));

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

procedure CompileComputedElements(const ACtx: TGocciaCompilationContext;
  const ATargetReg: UInt8; const AClassDef: TGocciaClassDefinition);
var
  I: Integer;
  Elem: TGocciaClassElement;
  KeyReg: UInt8;
begin
  for I := 0 to High(AClassDef.FElements) do
  begin
    Elem := AClassDef.FElements[I];
    if not Elem.IsComputed then
      Continue;

    KeyReg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(Elem.ComputedKeyExpression, KeyReg);

    case Elem.Kind of
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
    end;

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

procedure CompileFieldInitializer(const ACtx: TGocciaCompilationContext;
  const AClassReg: UInt8; const AClassDef: TGocciaClassDefinition);
var
  OldTemplate: TGocciaFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TGocciaFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  ChildCtx: TGocciaCompilationContext;
  FuncIdx: UInt16;
  FnReg: UInt8;
  ValReg, ThisReg: UInt8;
  KeyIdx: UInt16;
  I: Integer;
  Entry: TGocciaExpressionMap.TKeyValuePair;
  Elem: TGocciaClassElement;
  FieldExpr: TGocciaExpression;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TGocciaFunctionTemplate.Create('<fields>');
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.ParameterCount := 0;
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);

  ThisReg := ChildScope.DeclareLocal(KEYWORD_THIS, False);

  ACtx.SwapState(ChildTemplate, ChildScope);

  ChildCtx := ACtx;
  ChildCtx.Template := ChildTemplate;
  ChildCtx.Scope := ChildScope;

  if Length(AClassDef.FFieldOrder) > 0 then
  begin
    for I := 0 to High(AClassDef.FFieldOrder) do
    begin
      ValReg := ChildScope.AllocateRegister;
      if AClassDef.FFieldOrder[I].IsPrivate then
      begin
        if AClassDef.PrivateInstanceProperties.TryGetValue(
            AClassDef.FFieldOrder[I].Name, FieldExpr) then
          ACtx.CompileExpression(FieldExpr, ValReg);
        KeyIdx := ChildTemplate.AddConstantString(
          '#' + ChildScope.ResolvePrivatePrefix + AClassDef.FFieldOrder[I].Name);
      end
      else
      begin
        if AClassDef.InstanceProperties.TryGetValue(
            AClassDef.FFieldOrder[I].Name, FieldExpr) then
          ACtx.CompileExpression(FieldExpr, ValReg);
        KeyIdx := ChildTemplate.AddConstantString(AClassDef.FFieldOrder[I].Name);
      end;
      if KeyIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: field name index exceeds 255');
      EmitInstruction(ChildCtx, EncodeABC(OP_SET_PROP_CONST, ThisReg,
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
      EmitInstruction(ChildCtx, EncodeABC(OP_SET_PROP_CONST, ThisReg,
        UInt8(KeyIdx), ValReg));
      ChildScope.FreeRegister;
    end;

    for I := 0 to AClassDef.PrivateInstanceProperties.Count - 1 do
    begin
      Entry := AClassDef.PrivateInstanceProperties.EntryAt(I);
      ValReg := ChildScope.AllocateRegister;
      ACtx.CompileExpression(Entry.Value, ValReg);
      KeyIdx := ChildTemplate.AddConstantString('#' + ChildScope.ResolvePrivatePrefix + Entry.Key);
      if KeyIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: field name index exceeds 255');
      EmitInstruction(ChildCtx, EncodeABC(OP_SET_PROP_CONST, ThisReg,
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
    KeyIdx := ChildTemplate.AddConstantString('__accessor_' + Elem.Name);
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
      ChildScope.GetUpvalue(I).Index);

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
  Node: TGocciaASTNode;
  Reg: UInt8;
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

  for I := 0 to ABody.Nodes.Count - 1 do
  begin
    Node := ABody.Nodes[I];
    if Node is TGocciaStatement then
      ACtx.CompileStatement(TGocciaStatement(Node))
    else if Node is TGocciaExpression then
    begin
      Reg := ACtx.Scope.AllocateRegister;
      ACtx.CompileExpression(TGocciaExpression(Node), Reg);
      ACtx.Scope.FreeRegister;
    end;
  end;

  EmitInstruction(ChildCtx, EncodeABx(OP_LOAD_UNDEFINED, 0, 0));
  EmitInstruction(ChildCtx, EncodeABC(OP_RETURN, 0, 0, 0));

  ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

  for I := 0 to ChildScope.UpvalueCount - 1 do
    ChildTemplate.AddUpvalueDescriptor(
      ChildScope.GetUpvalue(I).IsLocal,
      ChildScope.GetUpvalue(I).Index);

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
  const AClassReg: UInt8; const AClassDef: TGocciaClassDefinition);
var
  I: Integer;
  Elem: TGocciaClassElement;
  NameIdx: UInt16;
  PairReg, InitReg: UInt8;
begin
  for I := 0 to High(AClassDef.FElements) do
  begin
    Elem := AClassDef.FElements[I];
    if Elem.Kind <> cekAccessor then
      Continue;

    NameIdx := ACtx.Template.AddConstantString(Elem.Name);
    if NameIdx > High(UInt8) then
      raise Exception.Create('Constant pool overflow: accessor name index exceeds 255');

    PairReg := ACtx.Scope.AllocateRegister;
    InitReg := ACtx.Scope.AllocateRegister;
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, InitReg, 0, 0));
    EmitInstruction(ACtx, EncodeABC(OP_SETUP_AUTO_ACCESSOR_CONST, PairReg, 0,
      UInt8(NameIdx)));
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
  end;
end;

procedure CompileDecoratorOrchestration(
  const ACtx: TGocciaCompilationContext;
  const AClassReg: UInt8; const AClassDef: TGocciaClassDefinition);
var
  I, J: Integer;
  Elem: TGocciaClassElement;
  DecoRegs: array of array of UInt8;
  ClassDecoRegs: array of UInt8;
  DescIdx: UInt16;
  Desc: string;
  PairReg, ExtraReg: UInt8;
  HasElementDecorators: Boolean;
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
      EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, ExtraReg, 0, 0));
      EmitInstruction(ACtx, EncodeABC(OP_APPLY_ELEMENT_DECORATOR_CONST, PairReg,
        0, UInt8(DescIdx)));
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
  const ASuperReg: Integer);
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

  CompileAutoAccessors(ACtx, AClassReg, AClassDef);
  CompileDecoratorOrchestration(ACtx, AClassReg, AClassDef);

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
  ClassReg, SuperReg, ValReg: UInt8;
  NameIdx, KeyIdx: UInt16;
  MethodPair: TGocciaClassMethodMap.TKeyValuePair;
  GetterPair: TGocciaGetterExpressionMap.TKeyValuePair;
  SetterPair: TGocciaSetterExpressionMap.TKeyValuePair;
  StaticPropPair: TGocciaExpressionMap.TKeyValuePair;
  I, LocalIdx, UpvalIdx: Integer;
  HasSuper: Boolean;
  PrivPrefix: string;
begin
  ClassDef := AStmt.ClassDefinition;
  HasSuper := ClassDef.SuperClass <> '';

  PrivPrefix := NextClassPrivatePrefix;
  ACtx.Scope.PrivatePrefix := PrivPrefix;

  // Reuse pre-declared slot if it exists at the same scope depth
  // (for function declaration upvalue resolution)
  LocalIdx := ACtx.Scope.ResolveLocal(ClassDef.Name);
  if (LocalIdx >= 0) and
     (ACtx.Scope.GetLocal(LocalIdx).Depth = ACtx.Scope.Depth) then
    ClassReg := ACtx.Scope.GetLocal(LocalIdx).Slot
  else
    ClassReg := ACtx.Scope.DeclareLocal(ClassDef.Name, True);
  if ACtx.GlobalBackedTopLevel and (ACtx.Scope.Depth = 0) then
  begin
    LocalIdx := ACtx.Scope.ResolveLocal(ClassDef.Name);
    if LocalIdx >= 0 then
      ACtx.Scope.MarkGlobalBacked(LocalIdx);
  end;
  NameIdx := ACtx.Template.AddConstantString(ClassDef.Name);
  EmitInstruction(ACtx, EncodeABx(OP_NEW_CLASS, ClassReg, NameIdx));

  if HasSuper then
  begin
    SuperReg := ACtx.Scope.DeclareLocal('__super__', False);

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

    EmitInstruction(ACtx, EncodeABC(OP_CLASS_SET_SUPER, ClassReg, SuperReg, 0));
  end;

  for MethodPair in ClassDef.Methods do
  begin
    if MethodPair.Value.IsStatic then
      CompileMethodBody(ACtx, ClassReg, MethodPair.Key,
        MethodPair.Value, OP_SET_PROP_CONST)
    else
      CompileMethodBody(ACtx, ClassReg, MethodPair.Key,
        MethodPair.Value, OP_CLASS_ADD_METHOD_CONST);
  end;

  for MethodPair in ClassDef.PrivateMethods do
    CompileMethodBody(ACtx, ClassReg, '#' + PrivPrefix + MethodPair.Key,
      MethodPair.Value, OP_CLASS_ADD_METHOD_CONST);

  for GetterPair in ClassDef.Getters do
  begin
    if (GetterPair.Key <> '') and (GetterPair.Key[1] = '#') then
      CompileGetterBody(ACtx, ClassReg,
        '#' + PrivPrefix + Copy(GetterPair.Key, 2, MaxInt),
        GetterPair.Value, OP_DEFINE_ACCESSOR_CONST, 0)
    else
      CompileGetterBody(ACtx, ClassReg, GetterPair.Key,
        GetterPair.Value, OP_DEFINE_ACCESSOR_CONST, 0);
  end;

  for SetterPair in ClassDef.Setters do
  begin
    if (SetterPair.Key <> '') and (SetterPair.Key[1] = '#') then
      CompileSetterBody(ACtx, ClassReg,
        '#' + PrivPrefix + Copy(SetterPair.Key, 2, MaxInt),
        SetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_SETTER)
    else
      CompileSetterBody(ACtx, ClassReg, SetterPair.Key,
        SetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_SETTER);
  end;

  for GetterPair in ClassDef.StaticGetters do
  begin
    if (GetterPair.Key <> '') and (GetterPair.Key[1] = '#') then
      CompileGetterBody(ACtx, ClassReg,
        '#' + PrivPrefix + Copy(GetterPair.Key, 2, MaxInt),
        GetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_STATIC)
    else
      CompileGetterBody(ACtx, ClassReg, GetterPair.Key,
        GetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_STATIC);
  end;

  for SetterPair in ClassDef.StaticSetters do
  begin
    if (SetterPair.Key <> '') and (SetterPair.Key[1] = '#') then
      CompileSetterBody(ACtx, ClassReg,
        '#' + PrivPrefix + Copy(SetterPair.Key, 2, MaxInt),
        SetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_STATIC or ACCESSOR_FLAG_SETTER)
    else
      CompileSetterBody(ACtx, ClassReg, SetterPair.Key,
        SetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_STATIC or ACCESSOR_FLAG_SETTER);
  end;

  if (ClassDef.InstanceProperties.Count > 0) or
     (ClassDef.PrivateInstanceProperties.Count > 0) or
     HasAccessorInitializers(ClassDef) then
    CompileFieldInitializer(ACtx, ClassReg, ClassDef);

  // Static fields without FElements entries (legacy / no static blocks)
  if Length(ClassDef.FElements) = 0 then
  begin
    for StaticPropPair in ClassDef.StaticProperties do
    begin
      ValReg := ACtx.Scope.AllocateRegister;
      ACtx.CompileExpression(StaticPropPair.Value, ValReg);
      KeyIdx := ACtx.Template.AddConstantString(StaticPropPair.Key);
      if KeyIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: static property name index exceeds 255');
      EmitInstruction(ACtx, EncodeABC(OP_SET_PROP_CONST, ClassReg,
        UInt8(KeyIdx), ValReg));
      ACtx.Scope.FreeRegister;
    end;

    for StaticPropPair in ClassDef.PrivateStaticProperties do
    begin
      ValReg := ACtx.Scope.AllocateRegister;
      ACtx.CompileExpression(StaticPropPair.Value, ValReg);
      KeyIdx := ACtx.Template.AddConstantString('#' + PrivPrefix + StaticPropPair.Key);
      if KeyIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: static property name index exceeds 255');
      EmitInstruction(ACtx, EncodeABC(OP_CLASS_DECLARE_PRIVATE_STATIC_CONST,
        ClassReg, UInt8(KeyIdx), 0));
      EmitInstruction(ACtx, EncodeABC(OP_SET_PROP_CONST, ClassReg,
        UInt8(KeyIdx), ValReg));
      ACtx.Scope.FreeRegister;
    end;
  end;

  CompileComputedElements(ACtx, ClassReg, ClassDef);

  // ES2022 §15.7.14: compile static fields and static blocks in source order
  for I := 0 to High(ClassDef.FElements) do
  begin
    if ClassDef.FElements[I].Kind = cekStaticBlock then
      CompileStaticBlock(ACtx, ClassReg, ClassDef.FElements[I].StaticBlockBody)
    else if (ClassDef.FElements[I].Kind = cekField) and ClassDef.FElements[I].IsStatic then
    begin
      ValReg := ACtx.Scope.AllocateRegister;
      if Assigned(ClassDef.FElements[I].FieldInitializer) then
        ACtx.CompileExpression(ClassDef.FElements[I].FieldInitializer, ValReg)
      else
        EmitInstruction(ACtx, EncodeABx(OP_LOAD_UNDEFINED, ValReg, 0));
      if ClassDef.FElements[I].IsPrivate then
      begin
        KeyIdx := ACtx.Template.AddConstantString(
          '#' + PrivPrefix + ClassDef.FElements[I].Name);
        if KeyIdx > High(UInt8) then
          raise Exception.Create('Constant pool overflow: static property name index exceeds 255');
        EmitInstruction(ACtx, EncodeABC(OP_CLASS_DECLARE_PRIVATE_STATIC_CONST,
          ClassReg, UInt8(KeyIdx), 0));
        EmitInstruction(ACtx, EncodeABC(OP_SET_PROP_CONST, ClassReg,
          UInt8(KeyIdx), ValReg));
      end
      else
      begin
        KeyIdx := ACtx.Template.AddConstantString(ClassDef.FElements[I].Name);
        if KeyIdx > High(UInt8) then
          raise Exception.Create('Constant pool overflow: static property name index exceeds 255');
        EmitInstruction(ACtx, EncodeABC(OP_SET_PROP_CONST, ClassReg,
          UInt8(KeyIdx), ValReg));
      end;
      ACtx.Scope.FreeRegister;
    end;
  end;

  if HasSuper then
    CompileDecoratorAndAccessorPass(ACtx, ClassReg, ClassDef, SuperReg)
  else
    CompileDecoratorAndAccessorPass(ACtx, ClassReg, ClassDef, -1);

  // Sync cell if the class local was pre-declared and captured by a hoisted
  // function (see CompileVariableDeclaration for the full explanation)
  LocalIdx := ACtx.Scope.ResolveLocal(ClassDef.Name);
  if (LocalIdx >= 0) and ACtx.Scope.GetLocal(LocalIdx).IsCaptured then
    EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, ClassReg, UInt16(ClassReg)));

  if ACtx.GlobalBackedTopLevel and (ACtx.Scope.Depth = 0) then
    EmitGlobalDefine(ACtx, ClassReg, ClassDef.Name, True);

  ACtx.Scope.PrivatePrefix := '';
end;

procedure CompileClassExpression(const ACtx: TGocciaCompilationContext;
  const AClassDef: TGocciaClassDefinition; const ADest: UInt8;
  const AInferredName: string = '');
var
  ClassDef: TGocciaClassDefinition;
  SuperReg, ValReg: UInt8;
  NameIdx, KeyIdx: UInt16;
  MethodPair: TGocciaClassMethodMap.TKeyValuePair;
  GetterPair: TGocciaGetterExpressionMap.TKeyValuePair;
  SetterPair: TGocciaSetterExpressionMap.TKeyValuePair;
  StaticPropPair: TGocciaExpressionMap.TKeyValuePair;
  LocalIdx, UpvalIdx: Integer;
  HasSuper: Boolean;
  PrivPrefix: string;
  HasNameBinding: Boolean;
  ClosedLocals: array[0..0] of UInt8;
  ClosedCount, I: Integer;
begin
  ClassDef := AClassDef;
  HasSuper := ClassDef.SuperClass <> '';
  HasNameBinding := ClassDef.Name <> '';

  PrivPrefix := NextClassPrivatePrefix;
  ACtx.Scope.PrivatePrefix := PrivPrefix;

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
    ACtx.Scope.DeclareLocal(ClassDef.Name, True);
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ACtx.Scope.NextSlot - 1, ADest, 0));
  end;

  if HasSuper then
  begin
    SuperReg := ACtx.Scope.DeclareLocal('__super__', False);

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

    EmitInstruction(ACtx, EncodeABC(OP_CLASS_SET_SUPER, ADest, SuperReg, 0));
  end;

  for MethodPair in ClassDef.Methods do
  begin
    if MethodPair.Value.IsStatic then
      CompileMethodBody(ACtx, ADest, MethodPair.Key,
        MethodPair.Value, OP_SET_PROP_CONST)
    else
      CompileMethodBody(ACtx, ADest, MethodPair.Key,
        MethodPair.Value, OP_CLASS_ADD_METHOD_CONST);
  end;

  for MethodPair in ClassDef.PrivateMethods do
    CompileMethodBody(ACtx, ADest, '#' + PrivPrefix + MethodPair.Key,
      MethodPair.Value, OP_CLASS_ADD_METHOD_CONST);

  for GetterPair in ClassDef.Getters do
  begin
    if (GetterPair.Key <> '') and (GetterPair.Key[1] = '#') then
      CompileGetterBody(ACtx, ADest,
        '#' + PrivPrefix + Copy(GetterPair.Key, 2, MaxInt),
        GetterPair.Value, OP_DEFINE_ACCESSOR_CONST, 0)
    else
      CompileGetterBody(ACtx, ADest, GetterPair.Key,
        GetterPair.Value, OP_DEFINE_ACCESSOR_CONST, 0);
  end;

  for SetterPair in ClassDef.Setters do
  begin
    if (SetterPair.Key <> '') and (SetterPair.Key[1] = '#') then
      CompileSetterBody(ACtx, ADest,
        '#' + PrivPrefix + Copy(SetterPair.Key, 2, MaxInt),
        SetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_SETTER)
    else
      CompileSetterBody(ACtx, ADest, SetterPair.Key,
        SetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_SETTER);
  end;

  for GetterPair in ClassDef.StaticGetters do
  begin
    if (GetterPair.Key <> '') and (GetterPair.Key[1] = '#') then
      CompileGetterBody(ACtx, ADest,
        '#' + PrivPrefix + Copy(GetterPair.Key, 2, MaxInt),
        GetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_STATIC)
    else
      CompileGetterBody(ACtx, ADest, GetterPair.Key,
        GetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_STATIC);
  end;

  for SetterPair in ClassDef.StaticSetters do
  begin
    if (SetterPair.Key <> '') and (SetterPair.Key[1] = '#') then
      CompileSetterBody(ACtx, ADest,
        '#' + PrivPrefix + Copy(SetterPair.Key, 2, MaxInt),
        SetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_STATIC or ACCESSOR_FLAG_SETTER)
    else
      CompileSetterBody(ACtx, ADest, SetterPair.Key,
        SetterPair.Value, OP_DEFINE_ACCESSOR_CONST, ACCESSOR_FLAG_STATIC or ACCESSOR_FLAG_SETTER);
  end;

  if (ClassDef.InstanceProperties.Count > 0) or
     (ClassDef.PrivateInstanceProperties.Count > 0) or
     HasAccessorInitializers(ClassDef) then
    CompileFieldInitializer(ACtx, ADest, ClassDef);

  // Static fields without FElements entries (legacy / no static blocks)
  if Length(ClassDef.FElements) = 0 then
  begin
    for StaticPropPair in ClassDef.StaticProperties do
    begin
      ValReg := ACtx.Scope.AllocateRegister;
      ACtx.CompileExpression(StaticPropPair.Value, ValReg);
      KeyIdx := ACtx.Template.AddConstantString(StaticPropPair.Key);
      if KeyIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: static property name index exceeds 255');
      EmitInstruction(ACtx, EncodeABC(OP_SET_PROP_CONST, ADest,
        UInt8(KeyIdx), ValReg));
      ACtx.Scope.FreeRegister;
    end;

    for StaticPropPair in ClassDef.PrivateStaticProperties do
    begin
      ValReg := ACtx.Scope.AllocateRegister;
      ACtx.CompileExpression(StaticPropPair.Value, ValReg);
      KeyIdx := ACtx.Template.AddConstantString('#' + PrivPrefix + StaticPropPair.Key);
      if KeyIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: static property name index exceeds 255');
      EmitInstruction(ACtx, EncodeABC(OP_CLASS_DECLARE_PRIVATE_STATIC_CONST,
        ADest, UInt8(KeyIdx), 0));
      EmitInstruction(ACtx, EncodeABC(OP_SET_PROP_CONST, ADest,
        UInt8(KeyIdx), ValReg));
      ACtx.Scope.FreeRegister;
    end;
  end;

  CompileComputedElements(ACtx, ADest, ClassDef);

  // ES2022 §15.7.14: compile static fields and static blocks in source order
  for I := 0 to High(ClassDef.FElements) do
  begin
    if ClassDef.FElements[I].Kind = cekStaticBlock then
      CompileStaticBlock(ACtx, ADest, ClassDef.FElements[I].StaticBlockBody)
    else if (ClassDef.FElements[I].Kind = cekField) and ClassDef.FElements[I].IsStatic then
    begin
      ValReg := ACtx.Scope.AllocateRegister;
      if Assigned(ClassDef.FElements[I].FieldInitializer) then
        ACtx.CompileExpression(ClassDef.FElements[I].FieldInitializer, ValReg)
      else
        EmitInstruction(ACtx, EncodeABx(OP_LOAD_UNDEFINED, ValReg, 0));
      if ClassDef.FElements[I].IsPrivate then
      begin
        KeyIdx := ACtx.Template.AddConstantString(
          '#' + PrivPrefix + ClassDef.FElements[I].Name);
        if KeyIdx > High(UInt8) then
          raise Exception.Create('Constant pool overflow: static property name index exceeds 255');
        EmitInstruction(ACtx, EncodeABC(OP_CLASS_DECLARE_PRIVATE_STATIC_CONST,
          ADest, UInt8(KeyIdx), 0));
        EmitInstruction(ACtx, EncodeABC(OP_SET_PROP_CONST, ADest,
          UInt8(KeyIdx), ValReg));
      end
      else
      begin
        KeyIdx := ACtx.Template.AddConstantString(ClassDef.FElements[I].Name);
        if KeyIdx > High(UInt8) then
          raise Exception.Create('Constant pool overflow: static property name index exceeds 255');
        EmitInstruction(ACtx, EncodeABC(OP_SET_PROP_CONST, ADest,
          UInt8(KeyIdx), ValReg));
      end;
      ACtx.Scope.FreeRegister;
    end;
  end;

  if HasSuper then
  begin
    CompileDecoratorAndAccessorPass(ACtx, ADest, ClassDef, SuperReg);
    // Only free __super__ manually when there is no name binding scope —
    // when HasNameBinding is true, __super__ lives inside the inner scope
    // and EndScope below will free it together with the name binding local.
    if not HasNameBinding then
      ACtx.Scope.FreeRegister;
  end
  else
    CompileDecoratorAndAccessorPass(ACtx, ADest, ClassDef, -1);

  if HasNameBinding then
  begin
    ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
    for I := 0 to ClosedCount - 1 do
      EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));
  end;

  ACtx.Scope.PrivatePrefix := '';
end;

procedure CollectDestructuringVarBindings(const APattern: TGocciaDestructuringPattern;
  const AScope: TGocciaCompilerScope);
var
  ObjPat: TGocciaObjectDestructuringPattern;
  ArrPat: TGocciaArrayDestructuringPattern;
  AssignPat: TGocciaAssignmentDestructuringPattern;
  I: Integer;
begin
  if APattern is TGocciaIdentifierDestructuringPattern then
    AScope.DeclareVarLocal(TGocciaIdentifierDestructuringPattern(APattern).Name)
  else if APattern is TGocciaObjectDestructuringPattern then
  begin
    ObjPat := TGocciaObjectDestructuringPattern(APattern);
    for I := 0 to ObjPat.Properties.Count - 1 do
      CollectDestructuringVarBindings(ObjPat.Properties[I].Pattern, AScope);
  end
  else if APattern is TGocciaArrayDestructuringPattern then
  begin
    ArrPat := TGocciaArrayDestructuringPattern(APattern);
    for I := 0 to ArrPat.Elements.Count - 1 do
      if Assigned(ArrPat.Elements[I]) then
        CollectDestructuringVarBindings(ArrPat.Elements[I], AScope);
  end
  else if APattern is TGocciaAssignmentDestructuringPattern then
  begin
    AssignPat := TGocciaAssignmentDestructuringPattern(APattern);
    CollectDestructuringVarBindings(AssignPat.Left, AScope);
  end
  else if APattern is TGocciaRestDestructuringPattern then
    CollectDestructuringVarBindings(
      TGocciaRestDestructuringPattern(APattern).Argument, AScope);
end;

procedure EmitGlobalDefinesForPattern(const ACtx: TGocciaCompilationContext;
  const APattern: TGocciaDestructuringPattern; const AIsConst: Boolean);
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
        ACtx.Scope.GetLocal(LocalIdx).Name, AIsConst);
    end;
  end
  else if APattern is TGocciaObjectDestructuringPattern then
  begin
    ObjPat := TGocciaObjectDestructuringPattern(APattern);
    for I := 0 to ObjPat.Properties.Count - 1 do
      EmitGlobalDefinesForPattern(ACtx, ObjPat.Properties[I].Pattern, AIsConst);
  end
  else if APattern is TGocciaArrayDestructuringPattern then
  begin
    ArrPat := TGocciaArrayDestructuringPattern(APattern);
    for I := 0 to ArrPat.Elements.Count - 1 do
      if Assigned(ArrPat.Elements[I]) then
        EmitGlobalDefinesForPattern(ACtx, ArrPat.Elements[I], AIsConst);
  end
  else if APattern is TGocciaAssignmentDestructuringPattern then
  begin
    AssignPat := TGocciaAssignmentDestructuringPattern(APattern);
    EmitGlobalDefinesForPattern(ACtx, AssignPat.Left, AIsConst);
  end
  else if APattern is TGocciaRestDestructuringPattern then
    EmitGlobalDefinesForPattern(ACtx,
      TGocciaRestDestructuringPattern(APattern).Argument, AIsConst);
end;

procedure CompileDestructuringDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaDestructuringDeclaration);
var
  SrcReg: UInt8;
  IsTopLevelGlobalBacked: Boolean;
  LocalCountBefore, I: Integer;
  Local: TGocciaCompilerLocal;
begin
  IsTopLevelGlobalBacked := ACtx.GlobalBackedTopLevel and
    (ACtx.Scope.Depth = 0);
  LocalCountBefore := ACtx.Scope.LocalCount;

  if AStmt.IsVar then
    CollectDestructuringVarBindings(AStmt.Pattern, ACtx.Scope)
  else
    CollectDestructuringBindings(AStmt.Pattern, ACtx.Scope, AStmt.IsConst);

  if IsTopLevelGlobalBacked and (not AStmt.IsVar) then
    for I := LocalCountBefore to ACtx.Scope.LocalCount - 1 do
      ACtx.Scope.MarkGlobalBacked(I);

  SrcReg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AStmt.Initializer, SrcReg);
  EmitDestructuring(ACtx, AStmt.Pattern, SrcReg);
  ACtx.Scope.FreeRegister;

  if IsTopLevelGlobalBacked then
  begin
    if AStmt.IsVar then
      EmitGlobalDefinesForPattern(ACtx, AStmt.Pattern, False)
    else
      for I := LocalCountBefore to ACtx.Scope.LocalCount - 1 do
      begin
        Local := ACtx.Scope.GetLocal(I);
        EmitGlobalDefine(ACtx, Local.Slot, Local.Name, AStmt.IsConst);
      end;
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
