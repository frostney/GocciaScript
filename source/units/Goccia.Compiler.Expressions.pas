unit Goccia.Compiler.Expressions;

{$I Goccia.inc}

interface

uses
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Bytecode,
  Goccia.Bytecode.Chunk,
  Goccia.Compiler.Context,
  Goccia.Compiler.Scope;

procedure CompileLiteral(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaLiteralExpression; const ADest: UInt8);
procedure CompileIdentifier(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIdentifierExpression; const ADest: UInt8);
procedure CompileIdentifierAccess(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIdentifierExpression; const ADest: UInt8;
  const ASafe: Boolean);
procedure CompileBinary(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaBinaryExpression; const ADest: UInt8);
procedure CompileSequence(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaSequenceExpression; const ADest: UInt8);
procedure CompileUnary(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaUnaryExpression; const ADest: UInt8);
procedure CompileAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaAssignmentExpression; const ADest: UInt8);
procedure CompilePropertyAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaPropertyAssignmentExpression; const ADest: UInt8);
procedure CompileArrowFunction(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaArrowFunctionExpression; const ADest: UInt8);
procedure CompileCall(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaCallExpression; const ADest: UInt8);
procedure CompileMember(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaMemberExpression; const ADest: UInt8);
procedure CompileConditional(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaConditionalExpression; const ADest: UInt8);
procedure CompileArray(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaArrayExpression; const ADest: UInt8);
procedure CompileObject(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaObjectExpression; const ADest: UInt8);
procedure CompileTemplateLiteral(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaTemplateLiteralExpression; const ADest: UInt8);
procedure CompileRegexLiteral(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaRegexLiteralExpression; const ADest: UInt8);
procedure CompileTemplateWithInterpolation(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaTemplateWithInterpolationExpression; const ADest: UInt8);
procedure CompileTaggedTemplate(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaTaggedTemplateExpression; const ADest: UInt8);
procedure CompileNewExpression(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaNewExpression; const ADest: UInt8);
procedure CompileComputedPropertyAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaComputedPropertyAssignmentExpression; const ADest: UInt8);
procedure CompileCompoundAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaCompoundAssignmentExpression; const ADest: UInt8);
procedure CompilePropertyCompoundAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaPropertyCompoundAssignmentExpression; const ADest: UInt8);
procedure CompileComputedPropertyCompoundAssignment(
  const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaComputedPropertyCompoundAssignmentExpression;
  const ADest: UInt8);
procedure CompileFunctionExpression(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaFunctionExpression; const ADest: UInt8;
  const ATemplateName: string = '<function>');
procedure CompileIncrement(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIncrementExpression; const ADest: UInt8);
procedure CompilePrivateMember(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaPrivateMemberExpression; const ADest: UInt8);
procedure CompilePrivatePropertyAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaPrivatePropertyAssignmentExpression; const ADest: UInt8);
procedure CompilePrivatePropertyCompoundAssignment(
  const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaPrivatePropertyCompoundAssignmentExpression;
  const ADest: UInt8);
procedure CompileThis(const ACtx: TGocciaCompilationContext;
  const ADest: UInt8);
procedure CompileSuperAccess(const ACtx: TGocciaCompilationContext;
  const ADest: UInt8);
procedure CompileImportMeta(const ACtx: TGocciaCompilationContext;
  const ADest: UInt8);
procedure CompileNewTarget(const ACtx: TGocciaCompilationContext;
  const ADest: UInt8);
procedure CompileDynamicImport(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaImportCallExpression; const ADest: UInt8);
procedure CompileYield(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaYieldExpression; const ADest: UInt8);

procedure EmitDefaultParameters(const ACtx: TGocciaCompilationContext;
  const AParams: TGocciaParameterArray);
function DeclareArgumentsObjectLocal(const ACtx: TGocciaCompilationContext;
  const AScope: TGocciaCompilerScope; const AParams: TGocciaParameterArray): Integer;
procedure EmitCreateArgumentsObject(const ACtx: TGocciaCompilationContext;
  const AArgumentsSlot: Integer);
procedure CollectDestructuringBindings(const APattern: TGocciaDestructuringPattern;
  const AScope: TGocciaCompilerScope; const AIsConst: Boolean = False);
procedure EmitDestructuring(const ACtx: TGocciaCompilationContext;
  const APattern: TGocciaDestructuringPattern; const ASrcReg: UInt8;
  const AAssignmentMode: Boolean = False);
procedure CompileDestructuringAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaDestructuringAssignmentExpression; const ADest: UInt8);

implementation

uses
  Classes,
  Generics.Collections,
  Math,
  SysUtils,

  Goccia.AST.BindingPatterns,
  Goccia.Bytecode.Debug,
  Goccia.Compiler.ConstantFolding,
  Goccia.Compiler.Statements,
  Goccia.Compiler.TypeRules,
  Goccia.Constants,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Keywords.Reserved,
  Goccia.Token,
  Goccia.Types.Enforcement,
  Goccia.Values.BigIntValue,
  Goccia.Values.Primitives;

function PrivateKey(const AScope: TGocciaCompilerScope;
  const AName: string): string;
var
  Prefix: string;
begin
  Prefix := AScope.ResolvePrivatePrefix;
  Result := '#slot:' + Prefix + AName;
end;

function InferredExpressionType(const AScope: TGocciaCompilerScope;
  const AExpr: TGocciaExpression): TGocciaLocalType;
begin
  Result := ExpressionType(AScope, AExpr);
  if Result = sltUntyped then
    Result := InferLocalType(AExpr);
end;

procedure SetNonStrictLocalTypeHint(const ACtx: TGocciaCompilationContext;
  const ALocalIdx: Integer; const ATypeHint: TGocciaLocalType);
var
  Local: TGocciaCompilerLocal;
begin
  if ALocalIdx < 0 then
    Exit;

  Local := ACtx.Scope.GetLocal(ALocalIdx);
  if Local.IsStrictlyTyped then
    Exit;

  ACtx.Scope.SetLocalTypeHint(ALocalIdx, ATypeHint);
  ACtx.Template.SetLocalType(Local.Slot, ATypeHint);
end;

procedure RefreshNonStrictLocalTypeHint(const ACtx: TGocciaCompilationContext;
  const ALocalIdx: Integer; const AExpr: TGocciaExpression);
begin
  SetNonStrictLocalTypeHint(ACtx, ALocalIdx,
    InferredExpressionType(ACtx.Scope, AExpr));
end;

procedure EmitStrictLocalTypeCheck(const ACtx: TGocciaCompilationContext;
  const ALocalIdx: Integer; const AValueReg: UInt8;
  const AProducedType: TGocciaLocalType);
var
  Local: TGocciaCompilerLocal;
begin
  if ALocalIdx < 0 then
    Exit;

  Local := ACtx.Scope.GetLocal(ALocalIdx);
  if Local.IsStrictlyTyped and (Local.TypeHint <> sltUntyped) and
     not TypesAreCompatible(AProducedType, Local.TypeHint) then
    EmitInstruction(ACtx, EncodeABC(OP_CHECK_TYPE, AValueReg,
      UInt8(Ord(Local.TypeHint)), 0));
end;

procedure EmitLoadPropertyByName(const ACtx: TGocciaCompilationContext;
  const ADest, AObjReg: UInt8; const APropertyName: string);
var
  PropIdx: UInt16;
  KeyReg: UInt8;
begin
  PropIdx := ACtx.Template.AddConstantString(APropertyName);
  if PropIdx <= High(UInt8) then
    EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, ADest, AObjReg,
      UInt8(PropIdx)))
  else
  begin
    KeyReg := ACtx.Scope.AllocateRegister;
    try
      EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, KeyReg, PropIdx));
      EmitInstruction(ACtx, EncodeABC(OP_GET_INDEX, ADest, AObjReg, KeyReg));
    finally
      ACtx.Scope.FreeRegister;
    end;
  end;
end;

procedure EmitStorePropertyByName(const ACtx: TGocciaCompilationContext;
  const AObjReg: UInt8; const APropertyName: string; const AValueReg: UInt8);
var
  PropIdx: UInt16;
  KeyReg: UInt8;
  ConstOp, IndexOp: TGocciaOpCode;
begin
  if ACtx.NonStrictMode then
  begin
    ConstOp := OP_SET_PROP_CONST_LOOSE;
    IndexOp := OP_SET_INDEX_LOOSE;
  end
  else
  begin
    ConstOp := OP_SET_PROP_CONST;
    IndexOp := OP_SET_INDEX;
  end;

  PropIdx := ACtx.Template.AddConstantString(APropertyName);
  if PropIdx <= High(UInt8) then
    EmitInstruction(ACtx, EncodeABC(ConstOp, AObjReg,
      UInt8(PropIdx), AValueReg))
  else
  begin
    KeyReg := ACtx.Scope.AllocateRegister;
    try
      EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, KeyReg, PropIdx));
      EmitInstruction(ACtx, EncodeABC(IndexOp, AObjReg, KeyReg,
        AValueReg));
    finally
      ACtx.Scope.FreeRegister;
    end;
  end;
end;

procedure EmitDefineDataPropertyByName(const ACtx: TGocciaCompilationContext;
  const AObjReg: UInt8; const APropertyName: string; const AValueReg: UInt8;
  const AOpcode: TGocciaOpCode);
var
  PropIdx: UInt16;
  KeyReg: UInt8;
begin
  PropIdx := ACtx.Template.AddConstantString(APropertyName);
  KeyReg := ACtx.Scope.AllocateRegister;
  try
    EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, KeyReg, PropIdx));
    EmitInstruction(ACtx, EncodeABC(AOpcode, AObjReg, KeyReg, AValueReg));
  finally
    ACtx.Scope.FreeRegister;
  end;
end;

function StoreByKeyOpcode(const ACtx: TGocciaCompilationContext): TGocciaOpCode;
begin
  if ACtx.NonStrictMode then
    Result := OP_SET_INDEX_LOOSE
  else
    Result := OP_ARRAY_SET;
end;

function ChildBodyIsStrictCode(const ACtx: TGocciaCompilationContext;
  const ABody: TGocciaASTNode): Boolean; inline;
begin
  Result := (not ACtx.NonStrictMode) or
    (Assigned(ACtx.Template) and ACtx.Template.StrictCode) or
    HasUseStrictDirective(ABody);
end;

function SetGlobalOpcode(const ACtx: TGocciaCompilationContext): TGocciaOpCode;
begin
  if ACtx.NonStrictMode then
    Result := OP_SET_GLOBAL_LOOSE
  else
    Result := OP_SET_GLOBAL;
end;

procedure EmitSetGlobalByIndex(const ACtx: TGocciaCompilationContext;
  const AValueReg: UInt8; const ANameIdx: UInt16);
begin
  EmitInstruction(ACtx, EncodeABx(SetGlobalOpcode(ACtx), AValueReg,
    ANameIdx));
end;

procedure EmitSetGlobalByName(const ACtx: TGocciaCompilationContext;
  const AValueReg: UInt8; const AName: string);
begin
  EmitSetGlobalByIndex(ACtx, AValueReg,
    ACtx.Template.AddConstantString(AName));
end;

procedure EmitLoadSuperPropertyByName(const ACtx: TGocciaCompilationContext;
  const ABaseReg, AMode: UInt8; const APropertyName: string);
var
  PropIdx: UInt16;
  KeyReg: UInt8;
begin
  PropIdx := ACtx.Template.AddConstantString(APropertyName);
  if PropIdx <= High(UInt8) then
    EmitInstruction(ACtx, EncodeABC(OP_SUPER_GET_CONST, ABaseReg, AMode,
      UInt8(PropIdx)))
  else
  begin
    KeyReg := ACtx.Scope.AllocateRegister;
    try
      EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, KeyReg, PropIdx));
      EmitInstruction(ACtx, EncodeABC(OP_SUPER_GET, ABaseReg, AMode, KeyReg));
    finally
      ACtx.Scope.FreeRegister;
    end;
  end;
end;

procedure EmitConstAssignmentError(const ACtx: TGocciaCompilationContext);
var
  MsgIdx: UInt16;
begin
  MsgIdx := ACtx.Template.AddConstantString('Assignment to constant variable.');
  if MsgIdx <= High(UInt8) then
    EmitInstruction(ACtx, EncodeABC(OP_THROW_TYPE_ERROR_CONST, 0, 0,
      UInt8(MsgIdx)))
  else
    EmitInstruction(ACtx, EncodeABx(OP_THROW_TYPE_ERROR_CONST_LONG, 0, MsgIdx));
end;

procedure CompileYield(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaYieldExpression; const ADest: UInt8);
begin
  if Assigned(AExpr.Operand) then
    ACtx.CompileExpression(AExpr.Operand, ADest)
  else
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, ADest, 0, 0));
  if AExpr.IsDelegate then
    EmitInstruction(ACtx, EncodeABC(OP_YIELD, ADest, ADest, 1))
  else
    EmitInstruction(ACtx, EncodeABC(OP_YIELD, ADest, ADest, 0));
end;

procedure CompileLiteral(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaLiteralExpression; const ADest: UInt8);
var
  Value: TGocciaValue;
  Idx: UInt16;
begin
  Value := AExpr.Value;

  if Value is TGocciaNullLiteralValue then
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_NULL, ADest, 0, 0))
  else if Value is TGocciaUndefinedLiteralValue then
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, ADest, 0, 0))
  else if Value is TGocciaBooleanLiteralValue then
  begin
    if TGocciaBooleanLiteralValue(Value).Value then
      EmitInstruction(ACtx, EncodeABC(OP_LOAD_TRUE, ADest, 0, 0))
    else
      EmitInstruction(ACtx, EncodeABC(OP_LOAD_FALSE, ADest, 0, 0));
  end
  else if Value is TGocciaNumberLiteralValue then
  begin
    if not TGocciaNumberLiteralValue(Value).IsNaN
       and not TGocciaNumberLiteralValue(Value).IsInfinite
       and (Frac(TGocciaNumberLiteralValue(Value).Value) = 0.0)
       and (TGocciaNumberLiteralValue(Value).Value >= MIN_SBX)
       and (TGocciaNumberLiteralValue(Value).Value <= MAX_SBX) then
      EmitInstruction(ACtx, EncodeAsBx(OP_LOAD_INT, ADest,
        Int16(Trunc(TGocciaNumberLiteralValue(Value).Value))))
    else
    begin
      Idx := ACtx.Template.AddConstantFloat(
        TGocciaNumberLiteralValue(Value).Value);
      EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, ADest, Idx));
    end;
  end
  else if Value is TGocciaStringLiteralValue then
  begin
    Idx := ACtx.Template.AddConstantString(
      TGocciaStringLiteralValue(Value).Value);
    EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, ADest, Idx));
  end
  else if Value is TGocciaBigIntValue then
  begin
    Idx := ACtx.Template.AddConstantBigInt(
      TGocciaBigIntValue(Value).Value.ToString);
    EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, ADest, Idx));
  end
  else
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, ADest, 0, 0));
end;

procedure CompileRegexLiteral(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaRegexLiteralExpression; const ADest: UInt8);
var
  BaseReg, PatternReg, FlagsReg: UInt8;
  NameIdx: UInt16;
begin
  BaseReg := ACtx.Scope.AllocateRegister;

  NameIdx := ACtx.Template.AddConstantString(CONSTRUCTOR_REGEXP);
  EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, BaseReg, NameIdx));

  PatternReg := ACtx.Scope.AllocateRegister;
  FlagsReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, PatternReg,
    ACtx.Template.AddConstantString(AExpr.Pattern)));
  EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, FlagsReg,
    ACtx.Template.AddConstantString(AExpr.Flags)));
  EmitInstruction(ACtx, EncodeABC(OP_CALL, BaseReg, 2, 0));
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;

  if BaseReg <> ADest then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, BaseReg, 0));
  ACtx.Scope.FreeRegister;
end;

procedure CompileIdentifier(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIdentifierExpression; const ADest: UInt8);
begin
  CompileIdentifierAccess(ACtx, AExpr, ADest, False);
end;

procedure EmitBindingAssignmentFromRegister(const ACtx: TGocciaCompilationContext;
  const AName: string; const AValueReg: UInt8;
  const AAssignmentMode: Boolean); forward;

function HiddenWithBindingName(const AName: string): Boolean;
begin
  Result := Pos('#with:', AName) = 1;
end;

function CapturedLocalDeclaredInsideParentWith(const AScope: TGocciaCompilerScope;
  const AName: string): Boolean;
var
  Scope: TGocciaCompilerScope;
  LocalIdx: Integer;
  Local: TGocciaCompilerLocal;
  WithDepth: Integer;
begin
  Scope := AScope.Parent;
  while Assigned(Scope) do
  begin
    LocalIdx := Scope.ResolveLocal(AName);
    if LocalIdx >= 0 then
    begin
      if Scope.WithBindingCount = 0 then
        Exit(False);

      Local := Scope.GetLocal(LocalIdx);
      WithDepth := Scope.GetWithBindingDepth(Scope.WithBindingCount - 1);
      Exit((WithDepth >= 0) and (Local.Depth > WithDepth));
    end;

    Scope := Scope.Parent;
  end;

  Result := False;
end;

function ShouldTryWithBinding(const AScope: TGocciaCompilerScope;
  const AName: string): Boolean;
var
  LocalIdx: Integer;
  Local: TGocciaCompilerLocal;
  WithDepth: Integer;
begin
  if (AScope.WithBindingCount = 0) or (AName = KEYWORD_THIS) or
     HiddenWithBindingName(AName) then
    Exit(False);

  WithDepth := AScope.GetWithBindingDepth(AScope.WithBindingCount - 1);
  LocalIdx := AScope.ResolveLocal(AName);
  if LocalIdx >= 0 then
  begin
    Local := AScope.GetLocal(LocalIdx);
    if Local.Depth > WithDepth then
      Exit(False);
  end;

  if CapturedLocalDeclaredInsideParentWith(AScope, AName) then
    Exit(False);

  Result := True;
end;

procedure EmitLoadHiddenWithObject(const ACtx: TGocciaCompilationContext;
  const AHiddenName: string; const ADest: UInt8);
var
  LocalIdx, UpvalIdx: Integer;
begin
  LocalIdx := ACtx.Scope.ResolveLocal(AHiddenName);
  if LocalIdx >= 0 then
  begin
    EmitInstruction(ACtx, EncodeABx(OP_GET_LOCAL, ADest,
      ACtx.Scope.GetLocal(LocalIdx).Slot));
    Exit;
  end;

  UpvalIdx := ACtx.Scope.ResolveUpvalue(AHiddenName);
  if UpvalIdx >= 0 then
  begin
    EmitInstruction(ACtx, EncodeABx(OP_GET_UPVALUE, ADest,
      UInt16(UpvalIdx)));
    Exit;
  end;

  EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, ADest, 0, 0));
end;

procedure CompileIdentifierAccessNoWith(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIdentifierExpression; const ADest: UInt8;
  const ASafe: Boolean);
var
  LocalIdx, UpvalIdx: Integer;
  Local: TGocciaCompilerLocal;
  Slot: UInt8;
  NameIdx: UInt16;
  CondReg, ArgReg: UInt8;
  OkJump: Integer;
begin
  LocalIdx := ACtx.Scope.ResolveLocal(AExpr.Name);
  if LocalIdx >= 0 then
  begin
    Local := ACtx.Scope.GetLocal(LocalIdx);
    if Local.IsGlobalBacked then
    begin
      NameIdx := ACtx.Template.AddConstantString(AExpr.Name);
      EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, ADest, NameIdx));
      Exit;
    end;
    Slot := Local.Slot;
    if Slot <> ADest then
      EmitInstruction(ACtx, EncodeABx(OP_GET_LOCAL, ADest, Slot));
    Exit;
  end;

  UpvalIdx := ACtx.Scope.ResolveUpvalue(AExpr.Name);
  if UpvalIdx >= 0 then
  begin
    if ACtx.Scope.GetUpvalue(UpvalIdx).IsGlobalBacked then
    begin
      NameIdx := ACtx.Template.AddConstantString(AExpr.Name);
      EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, ADest, NameIdx));
      Exit;
    end;
    EmitInstruction(ACtx, EncodeABx(OP_GET_UPVALUE, ADest, UInt16(UpvalIdx)));
    Exit;
  end;

  NameIdx := ACtx.Template.AddConstantString(AExpr.Name);

  if ASafe then
  begin
    EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, ADest, NameIdx));
    Exit;
  end;

  CondReg := ACtx.Scope.AllocateRegister;
  ArgReg := ACtx.Scope.AllocateRegister;

  EmitInstruction(ACtx, EncodeABx(OP_HAS_GLOBAL, CondReg, NameIdx));
  OkJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, CondReg);

  EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, CondReg,
    ACtx.Template.AddConstantString(REFERENCE_ERROR_NAME)));
  EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, ArgReg,
    ACtx.Template.AddConstantString(AExpr.Name + ' is not defined')));
  EmitInstruction(ACtx, EncodeABC(OP_CONSTRUCT, CondReg, CondReg, 1));
  EmitInstruction(ACtx, EncodeABC(OP_THROW, CondReg, 0, 0));

  PatchJumpTarget(ACtx, OkJump);

  EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, ADest, NameIdx));

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileIdentifierAccess(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIdentifierExpression; const ADest: UInt8;
  const ASafe: Boolean);
var
  ObjReg, KeyReg, CondReg: UInt8;
  NameIdx: UInt16;
  I, EndCount: Integer;
  MissJump: Integer;
  EndJumps: array of Integer;
begin
  if not ShouldTryWithBinding(ACtx.Scope, AExpr.Name) then
  begin
    CompileIdentifierAccessNoWith(ACtx, AExpr, ADest, ASafe);
    Exit;
  end;

  ObjReg := ACtx.Scope.AllocateRegister;
  KeyReg := ACtx.Scope.AllocateRegister;
  CondReg := ACtx.Scope.AllocateRegister;
  EndCount := 0;
  SetLength(EndJumps, ACtx.Scope.WithBindingCount);
  try
    NameIdx := ACtx.Template.AddConstantString(AExpr.Name);
    EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, KeyReg, NameIdx));

    for I := ACtx.Scope.WithBindingCount - 1 downto 0 do
    begin
      EmitLoadHiddenWithObject(ACtx, ACtx.Scope.GetWithBindingName(I), ObjReg);
      EmitInstruction(ACtx, EncodeABC(OP_HAS_WITH_BINDING, CondReg, ObjReg,
        KeyReg));
      MissJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, CondReg);
      EmitInstruction(ACtx, EncodeABC(OP_GET_INDEX, ADest, ObjReg, KeyReg));
      EndJumps[EndCount] := EmitJumpInstruction(ACtx, OP_JUMP, 0);
      Inc(EndCount);
      PatchJumpTarget(ACtx, MissJump);
    end;

    CompileIdentifierAccessNoWith(ACtx, AExpr, ADest, ASafe);

    for I := 0 to EndCount - 1 do
      PatchJumpTarget(ACtx, EndJumps[I]);
  finally
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
  end;
end;

procedure EmitLoadBindingByName(const ACtx: TGocciaCompilationContext;
  const AName: string; const ADest: UInt8; const ASafe: Boolean);
var
  Ident: TGocciaIdentifierExpression;
begin
  Ident := TGocciaIdentifierExpression.Create(AName, 0, 0);
  try
    CompileIdentifierAccess(ACtx, Ident, ADest, ASafe);
  finally
    Ident.Free;
  end;
end;

procedure EmitWithAssignmentOrFallback(const ACtx: TGocciaCompilationContext;
  const AName: string; const AValueReg: UInt8);
var
  ObjReg, KeyReg, CondReg: UInt8;
  NameIdx: UInt16;
  I, EndCount: Integer;
  MissJump: Integer;
  EndJumps: array of Integer;
begin
  ObjReg := ACtx.Scope.AllocateRegister;
  KeyReg := ACtx.Scope.AllocateRegister;
  CondReg := ACtx.Scope.AllocateRegister;
  EndCount := 0;
  SetLength(EndJumps, ACtx.Scope.WithBindingCount);
  try
    NameIdx := ACtx.Template.AddConstantString(AName);
    EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, KeyReg, NameIdx));

    for I := ACtx.Scope.WithBindingCount - 1 downto 0 do
    begin
      EmitLoadHiddenWithObject(ACtx, ACtx.Scope.GetWithBindingName(I), ObjReg);
      EmitInstruction(ACtx, EncodeABC(OP_HAS_WITH_BINDING, CondReg, ObjReg,
        KeyReg));
      MissJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, CondReg);
      EmitInstruction(ACtx, EncodeABC(StoreByKeyOpcode(ACtx), ObjReg, KeyReg,
        AValueReg));
      EndJumps[EndCount] := EmitJumpInstruction(ACtx, OP_JUMP, 0);
      Inc(EndCount);
      PatchJumpTarget(ACtx, MissJump);
    end;

    EmitBindingAssignmentFromRegister(ACtx, AName, AValueReg, True);

    for I := 0 to EndCount - 1 do
      PatchJumpTarget(ACtx, EndJumps[I]);
  finally
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
  end;
end;

function CallTrustedFlag(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaCallExpression): UInt8;
var
  Sig: string;
  LocalIdx, I: Integer;
  ArgType, ParamType: TGocciaLocalType;
  Local: TGocciaCompilerLocal;
  UV: TGocciaCompilerUpvalue;
begin
  Result := 0;
  if not (AExpr.Callee is TGocciaIdentifierExpression) then
    Exit;

  Sig := '';
  LocalIdx := ACtx.Scope.ResolveLocal(
    TGocciaIdentifierExpression(AExpr.Callee).Name);
  if LocalIdx >= 0 then
  begin
    Local := ACtx.Scope.GetLocal(LocalIdx);
    if not Local.IsConst then
      Exit;
    if Local.IsGlobalBacked then
      Exit;
    Sig := Local.ParamTypeSignature;
  end
  else
  begin
    LocalIdx := ACtx.Scope.ResolveUpvalue(
      TGocciaIdentifierExpression(AExpr.Callee).Name);
    if LocalIdx >= 0 then
    begin
      UV := ACtx.Scope.GetUpvalue(LocalIdx);
      if not UV.IsConst then
        Exit;
      if UV.IsGlobalBacked then
        Exit;
      Sig := UV.ParamTypeSignature;
    end;
  end;

  if (Sig = '') or (Length(Sig) > AExpr.Arguments.Count) then
    Exit;

  for I := 1 to Length(Sig) do
  begin
    ParamType := CharToLocalType(Sig[I]);
    if ParamType = sltUntyped then
      Exit;
    ArgType := ExpressionType(ACtx.Scope, AExpr.Arguments[I - 1]);
    if not TypesAreCompatible(ArgType, ParamType) then
      Exit;
  end;

  Result := 2;
end;

function TryIntegerOp(const AOperator: TGocciaTokenType;
  out AIntOp: TGocciaOpCode): Boolean;
begin
  Result := True;
  case AOperator of
    gttPlus:         AIntOp := OP_ADD_INT;
    gttMinus:        AIntOp := OP_SUB_INT;
    gttStar:         AIntOp := OP_MUL_INT;
    gttPercent:      AIntOp := OP_MOD_INT;
    gttLess:         AIntOp := OP_LT_INT;
    gttGreater:      AIntOp := OP_GT_INT;
    gttLessEqual:    AIntOp := OP_LTE_INT;
    gttGreaterEqual: AIntOp := OP_GTE_INT;
    gttEqual:        AIntOp := OP_EQ_INT;
    gttNotEqual:     AIntOp := OP_NEQ_INT;
    gttLooseEqual:   AIntOp := OP_EQ_INT;
    gttLooseNotEqual: AIntOp := OP_NEQ_INT;
  else
    Result := False;
  end;
end;


function IsArithmeticCompoundAssign(const ATokenType: TGocciaTokenType;
  out AArithOp: TGocciaTokenType): Boolean;
begin
  Result := True;
  case ATokenType of
    gttPlusAssign:    AArithOp := gttPlus;
    gttMinusAssign:   AArithOp := gttMinus;
    gttStarAssign:    AArithOp := gttStar;
    gttSlashAssign:   AArithOp := gttSlash;
    gttPercentAssign: AArithOp := gttPercent;
  else
    Result := False;
  end;
end;

function TryFloatOp(const AOperator: TGocciaTokenType;
  out AFloatOp: TGocciaOpCode): Boolean;
begin
  Result := True;
  case AOperator of
    gttPlus:         AFloatOp := OP_ADD_FLOAT;
    gttMinus:        AFloatOp := OP_SUB_FLOAT;
    gttStar:         AFloatOp := OP_MUL_FLOAT;
    gttSlash:        AFloatOp := OP_DIV_FLOAT;
    gttPercent:      AFloatOp := OP_MOD_FLOAT;
    gttLess:         AFloatOp := OP_LT_FLOAT;
    gttGreater:      AFloatOp := OP_GT_FLOAT;
    gttLessEqual:    AFloatOp := OP_LTE_FLOAT;
    gttGreaterEqual: AFloatOp := OP_GTE_FLOAT;
    gttEqual:        AFloatOp := OP_EQ_FLOAT;
    gttNotEqual:     AFloatOp := OP_NEQ_FLOAT;
    gttLooseEqual:   AFloatOp := OP_EQ_FLOAT;
    gttLooseNotEqual: AFloatOp := OP_NEQ_FLOAT;
  else
    Result := False;
  end;
end;

procedure CompileBinary(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaBinaryExpression; const ADest: UInt8);
var
  RegB, RegC: UInt8;
  Op, IntOp, FloatOp: TGocciaOpCode;
  JumpIdx, JumpIdx2: Integer;
  LeftType, RightType: TGocciaLocalType;
begin
  if TryFoldBinary(ACtx, AExpr, ADest) then
    Exit;

  if AExpr.Operator = gttAnd then
  begin
    ACtx.CompileExpression(AExpr.Left, ADest);
    JumpIdx := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, ADest);
    ACtx.CompileExpression(AExpr.Right, ADest);
    PatchJumpTarget(ACtx, JumpIdx);
    Exit;
  end;

  if AExpr.Operator = gttOr then
  begin
    ACtx.CompileExpression(AExpr.Left, ADest);
    JumpIdx := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, ADest);
    ACtx.CompileExpression(AExpr.Right, ADest);
    PatchJumpTarget(ACtx, JumpIdx);
    Exit;
  end;

  if AExpr.Operator = gttNullishCoalescing then
  begin
    ACtx.CompileExpression(AExpr.Left, ADest);
    JumpIdx := EmitJumpInstruction(ACtx, OP_JUMP_IF_NOT_NULLISH, ADest);
    ACtx.CompileExpression(AExpr.Right, ADest);
    PatchJumpTarget(ACtx, JumpIdx);
    Exit;
  end;

  RegB := ACtx.Scope.AllocateRegister;
  RegC := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(AExpr.Left, RegB);
  ACtx.CompileExpression(AExpr.Right, RegC);

  LeftType := ExpressionType(ACtx.Scope, AExpr.Left);
  RightType := ExpressionType(ACtx.Scope, AExpr.Right);

  if (LeftType = sltInteger) and (RightType = sltInteger) and
     TryIntegerOp(AExpr.Operator, IntOp) then
    EmitInstruction(ACtx, EncodeABC(IntOp, ADest, RegB, RegC))
  else if IsKnownNumeric(LeftType) and IsKnownNumeric(RightType) and
          TryFloatOp(AExpr.Operator, FloatOp) then
    EmitInstruction(ACtx, EncodeABC(FloatOp, ADest, RegB, RegC))
  else
  begin
    Op := TokenTypeToRuntimeOp(AExpr.Operator);
    if (AExpr.Operator = gttPlus) and
       not (IsKnownNumeric(LeftType) and IsKnownNumeric(RightType)) then
    begin
      EmitInstruction(ACtx, EncodeABx(OP_TO_PRIMITIVE, RegB, RegB));
      EmitInstruction(ACtx, EncodeABx(OP_TO_PRIMITIVE, RegC, RegC));
    end;
    if AExpr.Operator = gttIn then
      EmitInstruction(ACtx, EncodeABC(Op, ADest, RegC, RegB))
    else
      EmitInstruction(ACtx, EncodeABC(Op, ADest, RegB, RegC));
  end;

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileDelete(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaUnaryExpression; const ADest: UInt8);
var
  MemberExpr: TGocciaMemberExpression;
  IdentExpr: TGocciaIdentifierExpression;
  ObjReg, KeyReg: UInt8;
  PropIdx: UInt16;
  NilJump, EndJump: Integer;

  procedure EmitGlobalDeleteIdentifierResult(const AName: string);
  var
    NameIdx: UInt16;
  begin
    NameIdx := ACtx.Template.AddConstantString(AName);
    EmitInstruction(ACtx, EncodeABx(OP_DELETE_GLOBAL, ADest, NameIdx));
  end;

  procedure CompileDeleteIdentifierNoWith(const AName: string);
  var
    LocalIdx, UpvalIdx: Integer;
  begin
    LocalIdx := ACtx.Scope.ResolveLocal(AName);
    if LocalIdx >= 0 then
    begin
      if ACtx.Scope.GetLocal(LocalIdx).IsGlobalBacked then
        EmitGlobalDeleteIdentifierResult(AName)
      else
        EmitInstruction(ACtx, EncodeABC(OP_LOAD_FALSE, ADest, 0, 0));
      Exit;
    end;

    UpvalIdx := ACtx.Scope.ResolveUpvalue(AName);
    if UpvalIdx >= 0 then
    begin
      if ACtx.Scope.GetUpvalue(UpvalIdx).IsGlobalBacked then
        EmitGlobalDeleteIdentifierResult(AName)
      else
        EmitInstruction(ACtx, EncodeABC(OP_LOAD_FALSE, ADest, 0, 0));
      Exit;
    end;

    EmitGlobalDeleteIdentifierResult(AName);
  end;

  procedure CompileDeleteIdentifier(const AName: string);
  var
    WithObjReg, WithKeyReg, CondReg: UInt8;
    NameIdx: UInt16;
    I, EndCount: Integer;
    MissJump: Integer;
    EndJumps: array of Integer;
  begin
    if not ShouldTryWithBinding(ACtx.Scope, AName) then
    begin
      CompileDeleteIdentifierNoWith(AName);
      Exit;
    end;

    WithObjReg := ACtx.Scope.AllocateRegister;
    WithKeyReg := ACtx.Scope.AllocateRegister;
    CondReg := ACtx.Scope.AllocateRegister;
    EndCount := 0;
    SetLength(EndJumps, ACtx.Scope.WithBindingCount);
    try
      NameIdx := ACtx.Template.AddConstantString(AName);
      EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, WithKeyReg, NameIdx));

      for I := ACtx.Scope.WithBindingCount - 1 downto 0 do
      begin
        EmitLoadHiddenWithObject(ACtx, ACtx.Scope.GetWithBindingName(I), WithObjReg);
        EmitInstruction(ACtx, EncodeABC(OP_HAS_WITH_BINDING, CondReg, WithObjReg,
          WithKeyReg));
        MissJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, CondReg);
        EmitInstruction(ACtx, EncodeABC(OP_DEL_INDEX_LOOSE, ADest, WithObjReg,
          WithKeyReg));
        EndJumps[EndCount] := EmitJumpInstruction(ACtx, OP_JUMP, 0);
        Inc(EndCount);
        PatchJumpTarget(ACtx, MissJump);
      end;

      CompileDeleteIdentifierNoWith(AName);

      for I := 0 to EndCount - 1 do
        PatchJumpTarget(ACtx, EndJumps[I]);
    finally
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
    end;
  end;
begin
  if AExpr.Operand is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(AExpr.Operand);
    ObjReg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(MemberExpr.ObjectExpr, ObjReg);

    if MemberExpr.Optional then
      NilJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH, ObjReg)
    else
      NilJump := -1;

    if MemberExpr.Computed then
    begin
      KeyReg := ACtx.Scope.AllocateRegister;
      ACtx.CompileExpression(MemberExpr.PropertyExpression, KeyReg);
      if ACtx.NonStrictMode then
        EmitInstruction(ACtx, EncodeABC(OP_DEL_INDEX_LOOSE, ADest, ObjReg, KeyReg))
      else
        EmitInstruction(ACtx, EncodeABC(OP_DEL_INDEX, ADest, ObjReg, KeyReg));
      ACtx.Scope.FreeRegister;
    end
    else
    begin
      PropIdx := ACtx.Template.AddConstantString(MemberExpr.PropertyName);
      if ACtx.NonStrictMode then
        EmitInstruction(ACtx, EncodeABx(OP_DELETE_PROP_CONST_LOOSE, ObjReg, PropIdx))
      else
        EmitInstruction(ACtx, EncodeABx(OP_DELETE_PROP_CONST, ObjReg, PropIdx));
      if ADest <> ObjReg then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, ObjReg, 0));
    end;

    if NilJump >= 0 then
    begin
      EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
      PatchJumpTarget(ACtx, NilJump);
      EmitInstruction(ACtx, EncodeABC(OP_LOAD_TRUE, ADest, 0, 0));
      PatchJumpTarget(ACtx, EndJump);
    end;

    ACtx.Scope.FreeRegister;
  end
  else if AExpr.Operand is TGocciaIdentifierExpression then
  begin
    if not ACtx.NonStrictMode then
      raise Exception.Create('Delete of an unqualified identifier in strict mode');
    IdentExpr := TGocciaIdentifierExpression(AExpr.Operand);
    CompileDeleteIdentifier(IdentExpr.Name);
  end
  else
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_TRUE, ADest, 0, 0));
end;

procedure CompileUnary(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaUnaryExpression; const ADest: UInt8);
var
  RegB: UInt8;
begin
  if AExpr.Operator = gttDelete then
  begin
    CompileDelete(ACtx, AExpr, ADest);
    Exit;
  end;

  if TryFoldUnary(ACtx, AExpr, ADest) then
    Exit;

  RegB := ACtx.Scope.AllocateRegister;

  if (AExpr.Operator = gttTypeof) and (AExpr.Operand is TGocciaIdentifierExpression) then
    CompileIdentifierAccess(ACtx, TGocciaIdentifierExpression(AExpr.Operand), RegB, True)
  else
    ACtx.CompileExpression(AExpr.Operand, RegB);

  case AExpr.Operator of
    gttNot:        EmitInstruction(ACtx, EncodeABC(OP_NOT, ADest, RegB, 0));
    gttMinus:      EmitInstruction(ACtx, EncodeABC(OP_NEG, ADest, RegB, 0));
    gttPlus:       EmitInstruction(ACtx, EncodeABC(OP_TO_NUMBER, ADest, RegB, 0));
    gttTypeof:     EmitInstruction(ACtx, EncodeABC(OP_TYPEOF, ADest, RegB, 0));
    gttBitwiseNot: EmitInstruction(ACtx, EncodeABC(OP_BNOT, ADest, RegB, 0));
    gttVoid:       EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, ADest, 0, 0));
  else
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, ADest, 0, 0));
  end;

  ACtx.Scope.FreeRegister;
end;

// ES2026 §13.16 Comma Operator (,)
procedure CompileSequence(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaSequenceExpression; const ADest: UInt8);
var
  I: Integer;
  TempReg: UInt8;
begin
  if AExpr.Expressions.Count = 0 then
  begin
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, ADest, 0, 0));
    Exit;
  end;

  if AExpr.Expressions.Count = 1 then
  begin
    ACtx.CompileExpression(AExpr.Expressions[0], ADest);
    Exit;
  end;

  TempReg := ACtx.Scope.AllocateRegister;
  try
    for I := 0 to AExpr.Expressions.Count - 2 do
      ACtx.CompileExpression(AExpr.Expressions[I], TempReg);

    ACtx.CompileExpression(AExpr.Expressions[AExpr.Expressions.Count - 1], ADest);
  finally
    ACtx.Scope.FreeRegister;
  end;
end;

procedure CompileAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaAssignmentExpression; const ADest: UInt8);
var
  LocalIdx, UpvalIdx: Integer;
  Local: TGocciaCompilerLocal;
  Slot, GlobalExistsReg, ErrorReg, MessageReg: UInt8;
  ObjReg, KeyReg, CondReg, TargetReg: UInt8;
  NameIdx: UInt16;
  ValueType: TGocciaLocalType;
  GlobalExistsJump, MissJump, EndJump: Integer;
  I, EndCount: Integer;
  EndJumps: array of Integer;
begin
  if ShouldTryWithBinding(ACtx.Scope, AExpr.Name) then
  begin
    ObjReg := ACtx.Scope.AllocateRegister;
    KeyReg := ACtx.Scope.AllocateRegister;
    CondReg := ACtx.Scope.AllocateRegister;
    TargetReg := ACtx.Scope.AllocateRegister;
    EndCount := 0;
    SetLength(EndJumps, ACtx.Scope.WithBindingCount);
    try
      NameIdx := ACtx.Template.AddConstantString(AExpr.Name);
      EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, KeyReg, NameIdx));
      EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, TargetReg, 0, 0));

      for I := ACtx.Scope.WithBindingCount - 1 downto 0 do
      begin
        EmitLoadHiddenWithObject(ACtx, ACtx.Scope.GetWithBindingName(I),
          ObjReg);
        EmitInstruction(ACtx, EncodeABC(OP_HAS_WITH_BINDING, CondReg, ObjReg,
          KeyReg));
        MissJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, CondReg);
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, TargetReg, ObjReg, 0));
        EndJumps[EndCount] := EmitJumpInstruction(ACtx, OP_JUMP, 0);
        Inc(EndCount);
        PatchJumpTarget(ACtx, MissJump);
      end;

      for I := 0 to EndCount - 1 do
        PatchJumpTarget(ACtx, EndJumps[I]);

      ACtx.CompileExpression(AExpr.Value, ADest);
      MissJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH, TargetReg,
        GOCCIA_NULLISH_MATCH_UNDEFINED);
      EmitInstruction(ACtx, EncodeABC(StoreByKeyOpcode(ACtx), TargetReg,
        KeyReg, ADest));
      EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
      PatchJumpTarget(ACtx, MissJump);
      EmitBindingAssignmentFromRegister(ACtx, AExpr.Name, ADest, True);
      PatchJumpTarget(ACtx, EndJump);
    finally
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
    end;
    Exit;
  end;

  LocalIdx := ACtx.Scope.ResolveLocal(AExpr.Name);
  UpvalIdx := -1;
  GlobalExistsReg := 0;
  NameIdx := 0;

  if LocalIdx < 0 then
  begin
    UpvalIdx := ACtx.Scope.ResolveUpvalue(AExpr.Name);
    if UpvalIdx < 0 then
    begin
      NameIdx := ACtx.Template.AddConstantString(AExpr.Name);
      if ACtx.NonStrictMode then
      begin
        ACtx.CompileExpression(AExpr.Value, ADest);
        EmitSetGlobalByIndex(ACtx, ADest, NameIdx);
        Exit;
      end;
      GlobalExistsReg := ACtx.Scope.AllocateRegister;
      EmitInstruction(ACtx, EncodeABx(OP_HAS_GLOBAL, GlobalExistsReg, NameIdx));
    end;
  end;

  ACtx.CompileExpression(AExpr.Value, ADest);

  if LocalIdx >= 0 then
  begin
    Local := ACtx.Scope.GetLocal(LocalIdx);
    if Local.IsConst then
    begin
      EmitConstAssignmentError(ACtx);
      Exit;
    end;
    if Local.IsGlobalBacked then
    begin
      EmitStrictLocalTypeCheck(ACtx, LocalIdx, ADest,
        InferLocalType(AExpr.Value));
      EmitSetGlobalByName(ACtx, ADest, AExpr.Name);
      RefreshNonStrictLocalTypeHint(ACtx, LocalIdx, AExpr.Value);
      Exit;
    end;
    Slot := Local.Slot;
    EmitStrictLocalTypeCheck(ACtx, LocalIdx, ADest,
      InferLocalType(AExpr.Value));
    if ADest <> Slot then
    begin
      EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, ADest, Slot));
    end;
    if not Local.IsStrictlyTyped then
    begin
      ValueType := InferredExpressionType(ACtx.Scope, AExpr.Value);
      SetNonStrictLocalTypeHint(ACtx, LocalIdx, ValueType);
    end;
    Exit;
  end;

  if UpvalIdx >= 0 then
  begin
    if ACtx.Scope.GetUpvalue(UpvalIdx).IsConst then
    begin
      EmitConstAssignmentError(ACtx);
      Exit;
    end;
    if ACtx.Scope.GetUpvalue(UpvalIdx).IsGlobalBacked then
    begin
      EmitSetGlobalByName(ACtx, ADest, AExpr.Name);
      Exit;
    end;
    if ACtx.Scope.GetUpvalue(UpvalIdx).IsStrictlyTyped then
      if not TypesAreCompatible(InferLocalType(AExpr.Value), ACtx.Scope.GetUpvalue(UpvalIdx).TypeHint) then
        EmitInstruction(ACtx, EncodeABC(OP_CHECK_TYPE, ADest,
          UInt8(Ord(ACtx.Scope.GetUpvalue(UpvalIdx).TypeHint)), 0));
    EmitInstruction(ACtx, EncodeABx(OP_SET_UPVALUE, ADest, UInt16(UpvalIdx)));
    Exit;
  end;

  GlobalExistsJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, GlobalExistsReg);

  ErrorReg := GlobalExistsReg;
  MessageReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, ErrorReg,
    ACtx.Template.AddConstantString(REFERENCE_ERROR_NAME)));
  EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, MessageReg,
    ACtx.Template.AddConstantString(AExpr.Name + ' is not defined')));
  EmitInstruction(ACtx, EncodeABC(OP_CONSTRUCT, ErrorReg, ErrorReg, 1));
  EmitInstruction(ACtx, EncodeABC(OP_THROW, ErrorReg, 0, 0));
  ACtx.Scope.FreeRegister;

  PatchJumpTarget(ACtx, GlobalExistsJump);
  EmitSetGlobalByIndex(ACtx, ADest, NameIdx);
  ACtx.Scope.FreeRegister;
end;

procedure CompilePropertyAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaPropertyAssignmentExpression; const ADest: UInt8);
var
  ObjReg, ValReg: UInt8;
  KeyIdx: UInt16;
begin
  ObjReg := ACtx.Scope.AllocateRegister;
  ValReg := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(AExpr.ObjectExpr, ObjReg);
  ACtx.CompileExpression(AExpr.Value, ValReg);

  EmitStorePropertyByName(ACtx, ObjReg, AExpr.PropertyName, ValReg);

  if ADest <> ValReg then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, ValReg, 0));

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure EmitUndefinedCheck(const ACtx: TGocciaCompilationContext;
  const ASlot: UInt8; out AJumpIdx: Integer);
begin
  AJumpIdx := EmitJumpInstruction(ACtx, OP_JUMP_IF_NOT_NULLISH, ASlot,
    GOCCIA_NULLISH_MATCH_UNDEFINED);
end;

function DeclareArgumentsObjectLocal(const ACtx: TGocciaCompilationContext;
  const AScope: TGocciaCompilerScope; const AParams: TGocciaParameterArray): Integer;
begin
  if not ACtx.CompatibilityNonStrictMode then
    Exit(-1);

  if ParameterListBindsName(AParams, IDENTIFIER_ARGUMENTS) then
    Exit(-1);

  Result := AScope.DeclareLocal(IDENTIFIER_ARGUMENTS, False);
end;

procedure EmitCreateArgumentsObject(const ACtx: TGocciaCompilationContext;
  const AArgumentsSlot: Integer);
begin
  if AArgumentsSlot >= 0 then
    EmitInstruction(ACtx, EncodeABC(OP_CREATE_ARGUMENTS,
      UInt8(AArgumentsSlot), 0, 0));
end;

procedure EmitDefaultParameters(const ACtx: TGocciaCompilationContext;
  const AParams: TGocciaParameterArray);
var
  I, LocalIdx: Integer;
  Slot: UInt8;
  JumpIdx: Integer;
begin
  for I := 0 to High(AParams) do
  begin
    if not Assigned(AParams[I].DefaultValue) then
      Continue;
    if AParams[I].IsPattern then
    begin
      LocalIdx := ACtx.Scope.ResolveLocal('__param' + IntToStr(I));
      if LocalIdx < 0 then
        Continue;
      Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
      EmitUndefinedCheck(ACtx, Slot, JumpIdx);
      ACtx.CompileExpression(AParams[I].DefaultValue, Slot);
      PatchJumpTarget(ACtx, JumpIdx);
      Continue;
    end;

    LocalIdx := ACtx.Scope.ResolveLocal(AParams[I].Name);
    if LocalIdx < 0 then
      Continue;

    Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
    EmitUndefinedCheck(ACtx, Slot, JumpIdx);
    ACtx.CompileExpression(AParams[I].DefaultValue, Slot);
    PatchJumpTarget(ACtx, JumpIdx);
  end;
end;

procedure CollectDestructuringBindings(const APattern: TGocciaDestructuringPattern;
  const AScope: TGocciaCompilerScope; const AIsConst: Boolean);
var
  Names: TStringList;
  I, LocalIdx: Integer;
begin
  Names := TStringList.Create;
  Names.CaseSensitive := True;
  try
    CollectPatternBindingNames(APattern, Names);
    for I := 0 to Names.Count - 1 do
    begin
      // Reuse a pre-declared local (from function hoisting upvalue resolution)
      // if it exists at the same scope depth — mirrors CompileVariableDeclaration
      LocalIdx := AScope.ResolveLocal(Names[I]);
      if (LocalIdx < 0) or
         (AScope.GetLocal(LocalIdx).Depth <> AScope.Depth) then
        AScope.DeclareLocal(Names[I], AIsConst);
    end;
  finally
    Names.Free;
  end;
end;

procedure EmitBindingAssignmentFromRegister(const ACtx: TGocciaCompilationContext;
  const AName: string; const AValueReg: UInt8;
  const AAssignmentMode: Boolean);
var
  LocalIdx, UpvalIdx: Integer;
  Local: TGocciaCompilerLocal;
  Upvalue: TGocciaCompilerUpvalue;
  Slot: UInt8;
begin
  LocalIdx := ACtx.Scope.ResolveLocal(AName);
  if LocalIdx >= 0 then
  begin
    Local := ACtx.Scope.GetLocal(LocalIdx);
    if AAssignmentMode and Local.IsConst then
    begin
      EmitConstAssignmentError(ACtx);
      Exit;
    end;
    if AAssignmentMode and Local.IsStrictlyTyped and
       (Local.TypeHint <> sltUntyped) then
      EmitInstruction(ACtx, EncodeABC(OP_CHECK_TYPE, AValueReg,
        UInt8(Ord(Local.TypeHint)), 0));
    if Local.IsGlobalBacked then
    begin
      EmitSetGlobalByName(ACtx, AValueReg, AName);
      Exit;
    end;
    Slot := Local.Slot;
    if AAssignmentMode then
    begin
      if Slot <> AValueReg then
        EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, AValueReg, UInt16(Slot)))
      else if Local.IsCaptured then
        EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, Slot, UInt16(Slot)));
      Exit;
    end;
    if Slot <> AValueReg then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, Slot, AValueReg, 0));
    if Local.IsCaptured then
      EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, Slot, UInt16(Slot)));
    Exit;
  end;

  UpvalIdx := ACtx.Scope.ResolveUpvalue(AName);
  if UpvalIdx >= 0 then
  begin
    Upvalue := ACtx.Scope.GetUpvalue(UpvalIdx);
    if AAssignmentMode and Upvalue.IsConst then
    begin
      EmitConstAssignmentError(ACtx);
      Exit;
    end;
    if AAssignmentMode and Upvalue.IsStrictlyTyped and
       (Upvalue.TypeHint <> sltUntyped) then
      EmitInstruction(ACtx, EncodeABC(OP_CHECK_TYPE, AValueReg,
        UInt8(Ord(Upvalue.TypeHint)), 0));
    if Upvalue.IsGlobalBacked then
      EmitSetGlobalByName(ACtx, AValueReg, AName)
    else
      EmitInstruction(ACtx, EncodeABx(OP_SET_UPVALUE, AValueReg,
        UInt16(UpvalIdx)));
    Exit;
  end;

  EmitSetGlobalByName(ACtx, AValueReg, AName);
end;

procedure EmitDestructuring(const ACtx: TGocciaCompilationContext;
  const APattern: TGocciaDestructuringPattern; const ASrcReg: UInt8;
  const AAssignmentMode: Boolean);
var
  ObjPat: TGocciaObjectDestructuringPattern;
  ArrPat: TGocciaArrayDestructuringPattern;
  AssignPat: TGocciaAssignmentDestructuringPattern;
  IdentPat: TGocciaIdentifierDestructuringPattern;
  Prop: TGocciaDestructuringProperty;
  DestSlot, IdxReg: UInt8;
  PropIdx: UInt16;
  JumpIdx, I: Integer;
  HasRest: Boolean;
  Limit: Integer;
  RestIndex: Integer;
begin
  if APattern is TGocciaIdentifierDestructuringPattern then
  begin
    IdentPat := TGocciaIdentifierDestructuringPattern(APattern);
    EmitBindingAssignmentFromRegister(ACtx, IdentPat.Name, ASrcReg,
      AAssignmentMode);
  end
  else if APattern is TGocciaObjectDestructuringPattern then
  begin
    EmitInstruction(ACtx, EncodeABC(OP_VALIDATE_VALUE, ASrcReg,
      VALIDATE_OP_REQUIRE_OBJECT, 0));
    ObjPat := TGocciaObjectDestructuringPattern(APattern);
    for I := 0 to ObjPat.Properties.Count - 1 do
    begin
      Prop := ObjPat.Properties[I];

      if Prop.Pattern is TGocciaRestDestructuringPattern then
      begin
        IdxReg := ACtx.Scope.AllocateRegister;
        EmitInstruction(ACtx, EncodeABC(OP_NEW_ARRAY, IdxReg, UInt8(I), 0));
        for JumpIdx := 0 to I - 1 do
        begin
          DestSlot := ACtx.Scope.AllocateRegister;
          PropIdx := ACtx.Template.AddConstantString(ObjPat.Properties[JumpIdx].Key);
          EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, DestSlot, PropIdx));
          EmitInstruction(ACtx, EncodeABC(OP_ARRAY_PUSH, IdxReg, DestSlot, 0));
          ACtx.Scope.FreeRegister;
        end;
        DestSlot := ACtx.Scope.AllocateRegister;
        if IdxReg <> DestSlot + 1 then
          EmitInstruction(ACtx, EncodeABC(OP_MOVE, DestSlot + 1, IdxReg, 0));
        EmitInstruction(ACtx, EncodeABC(OP_COLLECTION_OP, DestSlot,
          COLLECTION_OP_OBJECT_REST, ASrcReg));
        EmitDestructuring(ACtx,
          TGocciaRestDestructuringPattern(Prop.Pattern).Argument, DestSlot,
          AAssignmentMode);
        ACtx.Scope.FreeRegister;
        ACtx.Scope.FreeRegister;
        Break;
      end;

      DestSlot := ACtx.Scope.AllocateRegister;
      if Prop.Computed and Assigned(Prop.KeyExpression) then
      begin
        IdxReg := ACtx.Scope.AllocateRegister;
        ACtx.CompileExpression(Prop.KeyExpression, IdxReg);
        EmitInstruction(ACtx, EncodeABC(OP_GET_INDEX, DestSlot, ASrcReg,
          IdxReg));
        ACtx.Scope.FreeRegister;
      end
      else
      begin
        EmitLoadPropertyByName(ACtx, DestSlot, ASrcReg, Prop.Key);
      end;

      EmitDestructuring(ACtx, Prop.Pattern, DestSlot, AAssignmentMode);
      ACtx.Scope.FreeRegister;
    end;
  end
  else if APattern is TGocciaArrayDestructuringPattern then
  begin
    ArrPat := TGocciaArrayDestructuringPattern(APattern);
    // Compute the iteration bound for OP_VALIDATE_VALUE.  Without a rest
    // element the spec consumes exactly N elements then closes the
    // iterator (ES2024 §8.5.3 IteratorBindingInitialization step 4).
    // With a rest element, full iteration is required.  We pass that
    // bound as operand C so the VM can stop calling next() once the
    // pattern is satisfied — otherwise an iterator whose next() always
    // returns done:false (e.g. test262's named-dflt-ary-init-iter-close
    // pattern) allocates unboundedly during destructuring.
    //
    // Operand C encoding (UInt8):
    //   0..254   = exact bound (0 means "consume zero elements" — empty
    //              array pattern `const [] = iter` per spec);
    //   255      = unbounded (rest pattern present, or pattern length
    //              exceeds what C can represent).
    // The 255 sentinel separates "empty fixed pattern" from "unbounded";
    // collapsing both to 0 would silently drain the iterator on
    // `const [] = iter` instead of consuming nothing then closing.
    HasRest := False;
    RestIndex := -1;
    for I := 0 to ArrPat.Elements.Count - 1 do
      if Assigned(ArrPat.Elements[I]) and
         (ArrPat.Elements[I] is TGocciaRestDestructuringPattern) then
      begin
        HasRest := True;
        RestIndex := I;
        Break;
      end;
    // Only HasRest collapses to the unbounded sentinel.  A fixed-size
    // pattern with >= 255 elements would otherwise silently change
    // semantics from "consume exactly N then close" to "drain the whole
    // iterator", because the operand-C encoding can't represent N.
    // Refuse to compile rather than miscompile — patterns this large are
    // pathological in practice (no real codebase array-destructures 255
    // bindings at once), and a clear compile-time error is better than
    // a silent infinite-iteration trap.
    if HasRest then
    begin
      // OP_UNPACK encodes the rest start offset in a UInt8 operand
      // (`EncodeABC(OP_UNPACK, DestSlot, ASrcReg, UInt8(I))` below), so
      // rest patterns whose start exceeds 255 elements would be
      // miscompiled — the truncated index points at the wrong slice
      // start.  Reject those at compile time too, paralleling the
      // fixed-pattern guard.  When the operand width is widened, this
      // limit can be relaxed.
      if RestIndex > High(UInt8) then
        raise Exception.CreateFmt(
          'Array destructuring rest pattern starts past the encodable '
          + 'offset (%d; max %d)',
          [RestIndex, High(UInt8)]);
      Limit := ITERABLE_LIMIT_UNBOUNDED;
    end
    else
    begin
      if ArrPat.Elements.Count >= ITERABLE_LIMIT_UNBOUNDED then
        raise Exception.CreateFmt(
          'Array destructuring pattern is too large to encode exactly '
          + '(%d elements; max %d without a rest element)',
          [ArrPat.Elements.Count, ITERABLE_LIMIT_UNBOUNDED - 1]);
      Limit := ArrPat.Elements.Count;
    end;

    EmitInstruction(ACtx, EncodeABC(OP_VALIDATE_VALUE, ASrcReg,
      VALIDATE_OP_REQUIRE_ITERABLE, UInt8(Limit)));
    for I := 0 to ArrPat.Elements.Count - 1 do
    begin
      if not Assigned(ArrPat.Elements[I]) then
        Continue;

      if ArrPat.Elements[I] is TGocciaRestDestructuringPattern then
      begin
        DestSlot := ACtx.Scope.AllocateRegister;
        EmitInstruction(ACtx, EncodeABC(OP_UNPACK, DestSlot, ASrcReg,
          UInt8(I)));
        EmitDestructuring(ACtx,
          TGocciaRestDestructuringPattern(ArrPat.Elements[I]).Argument, DestSlot,
          AAssignmentMode);
        ACtx.Scope.FreeRegister;
        Break;
      end;

      DestSlot := ACtx.Scope.AllocateRegister;
      IdxReg := ACtx.Scope.AllocateRegister;
      EmitInstruction(ACtx, EncodeAsBx(OP_LOAD_INT, IdxReg, Int16(I)));
      EmitInstruction(ACtx, EncodeABC(OP_ARRAY_GET, DestSlot, ASrcReg, IdxReg));
      ACtx.Scope.FreeRegister;

      EmitDestructuring(ACtx, ArrPat.Elements[I], DestSlot, AAssignmentMode);
      ACtx.Scope.FreeRegister;
    end;
  end
  else if APattern is TGocciaAssignmentDestructuringPattern then
  begin
    AssignPat := TGocciaAssignmentDestructuringPattern(APattern);
    EmitUndefinedCheck(ACtx, ASrcReg, JumpIdx);
    ACtx.CompileExpression(AssignPat.Right, ASrcReg);
    PatchJumpTarget(ACtx, JumpIdx);
    EmitDestructuring(ACtx, AssignPat.Left, ASrcReg, AAssignmentMode);
  end
  else if APattern is TGocciaMemberExpressionDestructuringPattern then
  begin
    DestSlot := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(
      TGocciaMemberExpressionDestructuringPattern(APattern).Expression.ObjectExpr,
      DestSlot);
    if TGocciaMemberExpressionDestructuringPattern(APattern).Expression.Computed then
    begin
      IdxReg := ACtx.Scope.AllocateRegister;
      ACtx.CompileExpression(
        TGocciaMemberExpressionDestructuringPattern(APattern).Expression.PropertyExpression,
        IdxReg);
      EmitInstruction(ACtx, EncodeABC(StoreByKeyOpcode(ACtx), DestSlot,
        IdxReg, ASrcReg));
      ACtx.Scope.FreeRegister;
    end
    else
    begin
      EmitStorePropertyByName(ACtx, DestSlot,
        TGocciaMemberExpressionDestructuringPattern(APattern).Expression.PropertyName,
        ASrcReg);
    end;
    ACtx.Scope.FreeRegister;
  end;
end;

procedure CompileDestructuringAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaDestructuringAssignmentExpression; const ADest: UInt8);
var
  SrcReg: UInt8;
begin
  SrcReg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AExpr.Right, SrcReg);
  EmitDestructuring(ACtx, AExpr.Left, SrcReg, True);
  if ADest <> SrcReg then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, SrcReg, 0));
  ACtx.Scope.FreeRegister;
end;

procedure EmitDestructuringParameters(const ACtx: TGocciaCompilationContext;
  const AParams: TGocciaParameterArray);
var
  I, LocalIdx: Integer;
  ParamSlot: UInt8;
begin
  for I := 0 to High(AParams) do
  begin
    if not AParams[I].IsPattern then
      Continue;
    if not Assigned(AParams[I].Pattern) then
      Continue;

    LocalIdx := ACtx.Scope.ResolveLocal('__param' + IntToStr(I));
    if LocalIdx < 0 then
      Continue;
    ParamSlot := ACtx.Scope.GetLocal(LocalIdx).Slot;
    EmitDestructuring(ACtx, AParams[I].Pattern, ParamSlot);
  end;
end;

procedure ApplyParameterTypeAnnotations(
  const AScope: TGocciaCompilerScope;
  const ATemplate: TGocciaFunctionTemplate;
  const AParameters: TGocciaParameterArray;
  const AStrictTypes: Boolean);
var
  I, LocalIdx: Integer;
  AnnotationType: TGocciaLocalType;
  ParamName: string;
begin
  if not AStrictTypes then
    Exit;

  for I := 0 to High(AParameters) do
  begin
    if AParameters[I].TypeAnnotation = '' then
      Continue;
    if AParameters[I].IsOptional then
      Continue;
    if Assigned(AParameters[I].DefaultValue) then
      Continue;
    AnnotationType := TypeAnnotationToLocalType(AParameters[I].TypeAnnotation);
    if AnnotationType = sltUntyped then
      Continue;
    if AParameters[I].IsPattern then
      ParamName := '__param' + IntToStr(I)
    else
      ParamName := AParameters[I].Name;
    LocalIdx := AScope.ResolveLocal(ParamName);
    if LocalIdx < 0 then
      Continue;
    // Rest parameter: record the type hint so subsequent reassignments are
    // guarded, but EmitParameterTypeChecks skips IsRest so no OP_CHECK_TYPE
    // fires at function entry against the rest array itself.
    AScope.SetLocalTypeHint(LocalIdx, AnnotationType);
    ATemplate.SetLocalType(AScope.GetLocal(LocalIdx).Slot, AnnotationType);
    AScope.SetLocalStrictlyTyped(LocalIdx, True);
    ATemplate.SetLocalStrictFlag(AScope.GetLocal(LocalIdx).Slot, True);
  end;
end;

procedure EmitParameterTypeChecks(const ACtx: TGocciaCompilationContext;
  const AParameters: TGocciaParameterArray);
var
  I, LocalIdx, CheckCount, CodeBefore: Integer;
  Local: TGocciaCompilerLocal;
  ParamName: string;
begin
  CodeBefore := ACtx.Template.CodeCount;
  for I := 0 to High(AParameters) do
  begin
    if AParameters[I].TypeAnnotation = '' then
      Continue;
    if AParameters[I].IsRest then
      Continue;
    if AParameters[I].IsPattern then
      ParamName := '__param' + IntToStr(I)
    else
      ParamName := AParameters[I].Name;
    LocalIdx := ACtx.Scope.ResolveLocal(ParamName);
    if LocalIdx < 0 then
      Continue;
    Local := ACtx.Scope.GetLocal(LocalIdx);
    if not Local.IsStrictlyTyped then
      Continue;
    EmitInstruction(ACtx, EncodeABC(OP_CHECK_TYPE, Local.Slot,
      UInt8(Ord(Local.TypeHint)), 0));
  end;
  CheckCount := ACtx.Template.CodeCount - CodeBefore;
  if (CheckCount > 0) and (CodeBefore = 0) then
    ACtx.Template.TypeCheckPreambleSize := UInt8(CheckCount);
end;

procedure CompileArrowFunction(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaArrowFunctionExpression; const ADest: UInt8);
var
  OldTemplate: TGocciaFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TGocciaFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  ChildCtx: TGocciaCompilationContext;
  FuncIdx: UInt16;
  FormalCount: Integer;
  RestParamIndex: Integer;
  RestReg: Integer;
  I: Integer;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TGocciaFunctionTemplate.Create('<arrow>');
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.IsAsync := AExpr.IsAsync;
  ChildTemplate.IsArrow := True;
  ChildTemplate.StrictCode := ChildBodyIsStrictCode(ACtx, AExpr.Body);
  ChildTemplate.SourceText := AExpr.SourceText;
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);
  ChildScope.IsArrow := True;

  ChildScope.DeclareLocal('__receiver', False);
  ChildTemplate.ParameterCount := Length(AExpr.Parameters);

  FormalCount := -1;
  RestParamIndex := -1;
  for I := 0 to High(AExpr.Parameters) do
  begin
    if AExpr.Parameters[I].IsRest or Assigned(AExpr.Parameters[I].DefaultValue) then
    begin
      if FormalCount < 0 then
        FormalCount := I;
      if AExpr.Parameters[I].IsRest then
        RestParamIndex := I;
    end;
    if AExpr.Parameters[I].IsPattern then
      ChildScope.DeclareLocal('__param' + IntToStr(I), False)
    else
      ChildScope.DeclareLocal(AExpr.Parameters[I].Name, False);
  end;
  for I := 0 to High(AExpr.Parameters) do
    if AExpr.Parameters[I].IsPattern and Assigned(AExpr.Parameters[I].Pattern) then
      CollectDestructuringBindings(AExpr.Parameters[I].Pattern, ChildScope);

  ApplyParameterTypeAnnotations(ChildScope, ChildTemplate, AExpr.Parameters,
    ACtx.StrictTypes);

  if FormalCount < 0 then
    FormalCount := Length(AExpr.Parameters);
  ChildTemplate.FormalParameterCount := UInt8(FormalCount);
  if Assigned(ACtx.FormalParameterCounts) then
    ACtx.FormalParameterCounts.AddOrSetValue(ChildTemplate, FormalCount);

  ACtx.SwapState(ChildTemplate, ChildScope);
  try
    ChildCtx := ACtx;
    ChildCtx.Template := ChildTemplate;
    ChildCtx.Scope := ChildScope;
    ChildCtx.NonStrictMode := ACtx.NonStrictMode and
      not ChildTemplate.StrictCode;

    if RestParamIndex >= 0 then
    begin
      RestReg := ChildScope.ResolveLocal(AExpr.Parameters[RestParamIndex].Name);
      EmitInstruction(ChildCtx,
        EncodeABC(OP_PACK_ARGS, UInt8(RestReg), UInt8(RestParamIndex), 0));
    end;

    EmitDefaultParameters(ChildCtx, AExpr.Parameters);
    EmitDestructuringParameters(ChildCtx, AExpr.Parameters);
    EmitParameterTypeChecks(ChildCtx, AExpr.Parameters);

    ACtx.CompileFunctionBody(AExpr.Body);

    ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

    for I := 0 to ChildScope.UpvalueCount - 1 do
      ChildTemplate.AddUpvalueDescriptor(
        ChildScope.GetUpvalue(I).IsLocal,
        ChildScope.GetUpvalue(I).Index);
  finally
    ACtx.SwapState(OldTemplate, OldScope);
    ChildScope.Free;
  end;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, ADest, FuncIdx));
end;

function HasSpreadArgument(const AExpr: TGocciaCallExpression): Boolean;
var
  I: Integer;
begin
  for I := 0 to AExpr.Arguments.Count - 1 do
    if AExpr.Arguments[I] is TGocciaSpreadExpression then
      Exit(True);
  Result := False;
end;

function IsLocalSlot(const AScope: TGocciaCompilerScope;
  const ASlot: UInt8): Boolean;
var
  I: Integer;
begin
  for I := 0 to AScope.LocalCount - 1 do
    if AScope.GetLocal(I).Slot = ASlot then
      Exit(True);
  Result := False;
end;

procedure CompileSpreadArgsArrayFromList(const ACtx: TGocciaCompilationContext;
  const AArgs: TObjectList<TGocciaExpression>; const AArrayReg: UInt8);
var
  I: Integer;
  ElemReg: UInt8;
begin
  EmitInstruction(ACtx, EncodeABC(OP_NEW_ARRAY, AArrayReg, 0, 0));
  for I := 0 to AArgs.Count - 1 do
  begin
    ElemReg := ACtx.Scope.AllocateRegister;
    if AArgs[I] is TGocciaSpreadExpression then
    begin
      ACtx.CompileExpression(
        TGocciaSpreadExpression(AArgs[I]).Argument, ElemReg);
      EmitInstruction(ACtx, EncodeABC(OP_COLLECTION_OP, AArrayReg,
        COLLECTION_OP_SPREAD_ITERABLE_INTO_ARRAY, ElemReg));
    end
    else
    begin
      ACtx.CompileExpression(AArgs[I], ElemReg);
      EmitInstruction(ACtx, EncodeABC(OP_ARRAY_PUSH, AArrayReg, ElemReg, 0));
    end;
    ACtx.Scope.FreeRegister;
  end;
end;

procedure CompileSpreadArgsArray(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaCallExpression; const AArrayReg: UInt8);
begin
  CompileSpreadArgsArrayFromList(ACtx, AExpr.Arguments, AArrayReg);
end;

function TryCompileWithIdentifierCall(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaCallExpression; const ADest: UInt8;
  const AArgCount: Integer; const AUseSpread: Boolean): Boolean;
var
  Ident: TGocciaIdentifierExpression;
  ObjReg, BaseReg, ArgsReg, KeyReg, CondReg: UInt8;
  NameIdx: UInt16;
  I, EndCount: Integer;
  MissJump, CallNilJump, BranchEndJump: Integer;
  EndJumps: array of Integer;

  procedure EmitBranchCall(const AIsMethodCall, AJumpAfter: Boolean);
  var
    ArgIndex: Integer;
  begin
    if AExpr.Optional then
      CallNilJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH, BaseReg)
    else
      CallNilJump := -1;

    if AUseSpread then
    begin
      ArgsReg := ACtx.Scope.AllocateRegister;
      CompileSpreadArgsArray(ACtx, AExpr, ArgsReg);
      if AIsMethodCall then
        EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, BaseReg, ArgsReg, 1))
      else
        EmitInstruction(ACtx, EncodeABC(OP_CALL, BaseReg, ArgsReg, 1));
      ACtx.Scope.FreeRegister;
    end
    else
    begin
      for ArgIndex := 0 to AArgCount - 1 do
        ACtx.CompileExpression(AExpr.Arguments[ArgIndex],
          ACtx.Scope.AllocateRegister);
      if AIsMethodCall then
        EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, BaseReg,
          UInt8(AArgCount), 0))
      else
        EmitInstruction(ACtx, EncodeABC(OP_CALL, BaseReg, UInt8(AArgCount),
          CallTrustedFlag(ACtx, AExpr)));
      for ArgIndex := 0 to AArgCount - 1 do
        ACtx.Scope.FreeRegister;
    end;

    if AExpr.Optional then
    begin
      BranchEndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
      PatchJumpTarget(ACtx, CallNilJump);
      EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, BaseReg, 0, 0));
      PatchJumpTarget(ACtx, BranchEndJump);
    end;

    if ADest <> BaseReg then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, BaseReg, 0));

    if AJumpAfter then
    begin
      EndJumps[EndCount] := EmitJumpInstruction(ACtx, OP_JUMP, 0);
      Inc(EndCount);
    end;
  end;

begin
  if not (AExpr.Callee is TGocciaIdentifierExpression) then
    Exit(False);

  Ident := TGocciaIdentifierExpression(AExpr.Callee);
  if not ShouldTryWithBinding(ACtx.Scope, Ident.Name) then
    Exit(False);

  ObjReg := ACtx.Scope.AllocateRegister;
  BaseReg := ACtx.Scope.AllocateRegister;
  NameIdx := ACtx.Template.AddConstantString(Ident.Name);
  EndCount := 0;
  SetLength(EndJumps, ACtx.Scope.WithBindingCount);
  try
    for I := ACtx.Scope.WithBindingCount - 1 downto 0 do
    begin
      EmitLoadHiddenWithObject(ACtx, ACtx.Scope.GetWithBindingName(I), ObjReg);
      KeyReg := ACtx.Scope.AllocateRegister;
      CondReg := ACtx.Scope.AllocateRegister;
      EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, KeyReg, NameIdx));
      EmitInstruction(ACtx, EncodeABC(OP_HAS_WITH_BINDING, CondReg, ObjReg,
        KeyReg));
      MissJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, CondReg);
      EmitInstruction(ACtx, EncodeABC(OP_GET_INDEX, BaseReg, ObjReg, KeyReg));
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
      EmitBranchCall(True, True);
      PatchJumpTarget(ACtx, MissJump);
    end;

    CompileIdentifierAccessNoWith(ACtx, Ident, BaseReg, False);
    EmitBranchCall(False, False);

    for I := 0 to EndCount - 1 do
      PatchJumpTarget(ACtx, EndJumps[I]);
  finally
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
  end;

  Result := True;
end;

procedure CompileCall(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaCallExpression; const ADest: UInt8);
var
  ArgCount, I: Integer;
  BaseReg, ObjReg, ArgsReg, SuperReg, KeyReg: UInt8;
  ThisLocalIdx: Integer;
  ThisUpvalIdx: Integer;
  ThisLocal: TGocciaCompilerLocal;
  ThisSlot: UInt8;
  MemberExpr: TGocciaMemberExpression;
  PropIdx: UInt16;
  UseSpread: Boolean;
  NilJump, CallNilJump, EndJump: Integer;
begin
  ArgCount := AExpr.Arguments.Count;
  if ArgCount > High(UInt8) then
    raise Exception.Create('Compiler error: too many arguments (>255)');
  UseSpread := HasSpreadArgument(AExpr);
  CallNilJump := -1;

  if AExpr.Callee is TGocciaSuperExpression then
  begin
    ObjReg := ACtx.Scope.AllocateRegister;
    CompileThis(ACtx, ObjReg);
    BaseReg := ACtx.Scope.AllocateRegister;
    SuperReg := ACtx.Scope.AllocateRegister;
    CompileSuperAccess(ACtx, SuperReg);
    if SuperReg <> BaseReg + 1 then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, BaseReg + 1, SuperReg, 0));
    EmitLoadSuperPropertyByName(ACtx, BaseReg, 1, PROP_CONSTRUCTOR);
    ACtx.Scope.FreeRegister;

    if AExpr.Optional then
      CallNilJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH, BaseReg);

    if UseSpread then
    begin
      ArgsReg := ACtx.Scope.AllocateRegister;
      CompileSpreadArgsArray(ACtx, AExpr, ArgsReg);
      EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, BaseReg, ArgsReg, 1));
      ACtx.Scope.FreeRegister;
    end
    else
    begin
      for I := 0 to ArgCount - 1 do
        ACtx.CompileExpression(AExpr.Arguments[I], ACtx.Scope.AllocateRegister);
      EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, BaseReg, UInt8(ArgCount), 0));
      for I := 0 to ArgCount - 1 do
        ACtx.Scope.FreeRegister;
    end;

    ThisLocalIdx := ACtx.Scope.ResolveLocal(KEYWORD_THIS);
    if ThisLocalIdx >= 0 then
    begin
      ThisLocal := ACtx.Scope.GetLocal(ThisLocalIdx);
      ThisSlot := ThisLocal.Slot;
      if ThisSlot <> BaseReg then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, ThisSlot, BaseReg, 0));
      if ThisLocal.IsCaptured then
        EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, ThisSlot,
          UInt16(ThisSlot)));
    end
    else
    begin
      ThisUpvalIdx := ACtx.Scope.ResolveUpvalue(KEYWORD_THIS);
      if ThisUpvalIdx >= 0 then
        EmitInstruction(ACtx, EncodeABx(OP_SET_UPVALUE, BaseReg,
          UInt16(ThisUpvalIdx)))
      else
      begin
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, 0, BaseReg, 0));
        EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, 0, 0));
      end;
    end;

    if AExpr.Optional then
    begin
      EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
      PatchJumpTarget(ACtx, CallNilJump);
      EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, BaseReg, 0, 0));
      PatchJumpTarget(ACtx, EndJump);
    end;

    if ADest <> BaseReg then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, BaseReg, 0));

    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
  end
  else if AExpr.Callee is TGocciaPrivateMemberExpression then
  begin
    ObjReg := ACtx.Scope.AllocateRegister;
    BaseReg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(
      TGocciaPrivateMemberExpression(AExpr.Callee).ObjectExpr, ObjReg);
    PropIdx := ACtx.Template.AddConstantString(
      PrivateKey(ACtx.Scope,
        TGocciaPrivateMemberExpression(AExpr.Callee).PrivateName));
    if PropIdx <= High(UInt8) then
      EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, BaseReg, ObjReg,
        UInt8(PropIdx)))
    else
    begin
      KeyReg := ACtx.Scope.AllocateRegister;
      try
        EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, KeyReg, PropIdx));
        EmitInstruction(ACtx, EncodeABC(OP_GET_INDEX, BaseReg, ObjReg, KeyReg));
      finally
        ACtx.Scope.FreeRegister;
      end;
    end;

    if AExpr.Optional then
      CallNilJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH, BaseReg);

    if UseSpread then
    begin
      ArgsReg := ACtx.Scope.AllocateRegister;
      CompileSpreadArgsArray(ACtx, AExpr, ArgsReg);
      EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, BaseReg, ArgsReg, 1));
      ACtx.Scope.FreeRegister;
    end
    else
    begin
      for I := 0 to ArgCount - 1 do
        ACtx.CompileExpression(AExpr.Arguments[I], ACtx.Scope.AllocateRegister);
      EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, BaseReg, UInt8(ArgCount), 0));
      for I := 0 to ArgCount - 1 do
        ACtx.Scope.FreeRegister;
    end;

    if AExpr.Optional then
    begin
      EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
      PatchJumpTarget(ACtx, CallNilJump);
      EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, BaseReg, 0, 0));
      PatchJumpTarget(ACtx, EndJump);
    end;

    if ADest <> BaseReg then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, BaseReg, 0));
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
  end
  else if AExpr.Callee is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(AExpr.Callee);

    if MemberExpr.ObjectExpr is TGocciaSuperExpression then
    begin
      ObjReg := ACtx.Scope.AllocateRegister;
      CompileThis(ACtx, ObjReg);
      BaseReg := ACtx.Scope.AllocateRegister;
      SuperReg := ACtx.Scope.AllocateRegister;
      CompileSuperAccess(ACtx, SuperReg);
      if MemberExpr.Computed then
      begin
        KeyReg := ACtx.Scope.AllocateRegister;
        ACtx.CompileExpression(MemberExpr.PropertyExpression, KeyReg);
      end
      else
        PropIdx := ACtx.Template.AddConstantString(MemberExpr.PropertyName);
      if SuperReg <> BaseReg + 1 then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, BaseReg + 1, SuperReg, 0));
      if MemberExpr.Computed then
      begin
        EmitInstruction(ACtx, EncodeABC(OP_SUPER_GET, BaseReg, 0, KeyReg));
        ACtx.Scope.FreeRegister;
      end
      else
      begin
        if PropIdx <= High(UInt8) then
          EmitInstruction(ACtx, EncodeABC(OP_SUPER_GET_CONST, BaseReg, 0,
            UInt8(PropIdx)))
        else
        begin
          KeyReg := ACtx.Scope.AllocateRegister;
          try
            EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, KeyReg, PropIdx));
            EmitInstruction(ACtx, EncodeABC(OP_SUPER_GET, BaseReg, 0, KeyReg));
          finally
            ACtx.Scope.FreeRegister;
          end;
        end;
      end;
      ACtx.Scope.FreeRegister;

      if AExpr.Optional then
        CallNilJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH, BaseReg);

      if UseSpread then
      begin
        ArgsReg := ACtx.Scope.AllocateRegister;
        CompileSpreadArgsArray(ACtx, AExpr, ArgsReg);
        EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, BaseReg, ArgsReg, 1));
        ACtx.Scope.FreeRegister;
      end
      else
      begin
        for I := 0 to ArgCount - 1 do
          ACtx.CompileExpression(AExpr.Arguments[I], ACtx.Scope.AllocateRegister);
        EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, BaseReg, UInt8(ArgCount), 0));
        for I := 0 to ArgCount - 1 do
          ACtx.Scope.FreeRegister;
      end;

      if AExpr.Optional then
      begin
        EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
        PatchJumpTarget(ACtx, CallNilJump);
        EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, BaseReg, 0, 0));
        PatchJumpTarget(ACtx, EndJump);
      end;

      if ADest <> BaseReg then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, BaseReg, 0));

      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
    end
    else
    begin
      ObjReg := ACtx.Scope.AllocateRegister;
      BaseReg := ACtx.Scope.AllocateRegister;

      ACtx.CompileExpression(MemberExpr.ObjectExpr, ObjReg);

      if MemberExpr.Optional then
        NilJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH, ObjReg)
      else
        NilJump := -1;

      if MemberExpr.Computed then
      begin
        ACtx.CompileExpression(MemberExpr.PropertyExpression, BaseReg);
        EmitInstruction(ACtx, EncodeABC(OP_ARRAY_GET, BaseReg, ObjReg, BaseReg));
      end
      else
        EmitLoadPropertyByName(ACtx, BaseReg, ObjReg, MemberExpr.PropertyName);

      if AExpr.Optional then
        CallNilJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH, BaseReg);

      if UseSpread then
      begin
        ArgsReg := ACtx.Scope.AllocateRegister;
        CompileSpreadArgsArray(ACtx, AExpr, ArgsReg);
        EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, BaseReg, ArgsReg, 1));
        ACtx.Scope.FreeRegister;
      end
      else
      begin
        for I := 0 to ArgCount - 1 do
          ACtx.CompileExpression(AExpr.Arguments[I], ACtx.Scope.AllocateRegister);
        EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, BaseReg, UInt8(ArgCount), 0));
        for I := 0 to ArgCount - 1 do
          ACtx.Scope.FreeRegister;
      end;

      if MemberExpr.Optional or AExpr.Optional then
      begin
        EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
        if MemberExpr.Optional then
          PatchJumpTarget(ACtx, NilJump);
        if AExpr.Optional then
          PatchJumpTarget(ACtx, CallNilJump);
        EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, BaseReg, 0, 0));
        PatchJumpTarget(ACtx, EndJump);
      end;

      if ADest <> BaseReg then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, BaseReg, 0));

      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
    end;
  end
  else
  begin
    if TryCompileWithIdentifierCall(ACtx, AExpr, ADest, ArgCount,
       UseSpread) then
      Exit;

    if (ADest + 1 = ACtx.Scope.NextSlot) and
       not IsLocalSlot(ACtx.Scope, ADest) then
      BaseReg := ADest
    else
      BaseReg := ACtx.Scope.AllocateRegister;

    ACtx.CompileExpression(AExpr.Callee, BaseReg);

    if AExpr.Optional then
      CallNilJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH, BaseReg);

    if UseSpread then
    begin
      ArgsReg := ACtx.Scope.AllocateRegister;
      CompileSpreadArgsArray(ACtx, AExpr, ArgsReg);
      EmitInstruction(ACtx, EncodeABC(OP_CALL, BaseReg, ArgsReg, 1));
      ACtx.Scope.FreeRegister;
    end
    else
    begin
      for I := 0 to ArgCount - 1 do
        ACtx.CompileExpression(AExpr.Arguments[I], ACtx.Scope.AllocateRegister);
      EmitInstruction(ACtx, EncodeABC(OP_CALL, BaseReg, UInt8(ArgCount),
        CallTrustedFlag(ACtx, AExpr)));
      for I := 0 to ArgCount - 1 do
        ACtx.Scope.FreeRegister;
    end;

    if AExpr.Optional then
    begin
      EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
      PatchJumpTarget(ACtx, CallNilJump);
      EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, BaseReg, 0, 0));
      PatchJumpTarget(ACtx, EndJump);
    end;

    if BaseReg <> ADest then
    begin
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, BaseReg, 0));
      ACtx.Scope.FreeRegister;
    end;
  end;
end;

procedure CompileMember(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaMemberExpression; const ADest: UInt8);
var
  ObjReg, IdxReg, BaseReg, SuperReg, KeyReg: UInt8;
  PropIdx: UInt16;
  NilJump, EndJump: Integer;
begin
  if AExpr.ObjectExpr is TGocciaSuperExpression then
  begin
    ObjReg := ACtx.Scope.AllocateRegister;
    CompileThis(ACtx, ObjReg);
    BaseReg := ACtx.Scope.AllocateRegister;
    SuperReg := ACtx.Scope.AllocateRegister;
    CompileSuperAccess(ACtx, SuperReg);
    if AExpr.Computed then
    begin
      KeyReg := ACtx.Scope.AllocateRegister;
      ACtx.CompileExpression(AExpr.PropertyExpression, KeyReg);
    end
    else
    begin
      PropIdx := ACtx.Template.AddConstantString(AExpr.PropertyName);
    end;
    if SuperReg <> BaseReg + 1 then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, BaseReg + 1, SuperReg, 0));
    if AExpr.Computed then
    begin
      EmitInstruction(ACtx, EncodeABC(OP_SUPER_GET, BaseReg, 0, KeyReg));
      ACtx.Scope.FreeRegister;
    end
    else
    begin
      if PropIdx <= High(UInt8) then
        EmitInstruction(ACtx, EncodeABC(OP_SUPER_GET_CONST, BaseReg, 0,
          UInt8(PropIdx)))
      else
      begin
        KeyReg := ACtx.Scope.AllocateRegister;
        EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, KeyReg, PropIdx));
        EmitInstruction(ACtx, EncodeABC(OP_SUPER_GET, BaseReg, 0, KeyReg));
        ACtx.Scope.FreeRegister;
      end;
    end;
    if ADest <> BaseReg then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, BaseReg, 0));
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    Exit;
  end;

  ObjReg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AExpr.ObjectExpr, ObjReg);

  if AExpr.Optional then
  begin
    NilJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH, ObjReg);

    if AExpr.Computed then
    begin
      IdxReg := ACtx.Scope.AllocateRegister;
      ACtx.CompileExpression(AExpr.PropertyExpression, IdxReg);
      EmitInstruction(ACtx, EncodeABC(OP_ARRAY_GET, ADest, ObjReg, IdxReg));
      ACtx.Scope.FreeRegister;
    end
    else
      EmitLoadPropertyByName(ACtx, ADest, ObjReg, AExpr.PropertyName);

    EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
    PatchJumpTarget(ACtx, NilJump);
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, ADest, 0, 0));
    PatchJumpTarget(ACtx, EndJump);
  end
  else
  begin
    if AExpr.Computed then
    begin
      IdxReg := ACtx.Scope.AllocateRegister;
      ACtx.CompileExpression(AExpr.PropertyExpression, IdxReg);
      EmitInstruction(ACtx, EncodeABC(OP_ARRAY_GET, ADest, ObjReg, IdxReg));
      ACtx.Scope.FreeRegister;
    end
    else
      EmitLoadPropertyByName(ACtx, ADest, ObjReg, AExpr.PropertyName);
  end;

  ACtx.Scope.FreeRegister;
end;

procedure CompileConditional(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaConditionalExpression; const ADest: UInt8);
var
  ElseJump, EndJump: Integer;
begin
  ACtx.CompileExpression(AExpr.Condition, ADest);
  ElseJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, ADest);
  ACtx.CompileExpression(AExpr.Consequent, ADest);
  EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
  PatchJumpTarget(ACtx, ElseJump);
  ACtx.CompileExpression(AExpr.Alternate, ADest);
  PatchJumpTarget(ACtx, EndJump);
end;

procedure CompileArray(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaArrayExpression; const ADest: UInt8);
var
  I: Integer;
  ElemReg: UInt8;
begin
  EmitInstruction(ACtx, EncodeABC(OP_NEW_ARRAY, ADest,
    UInt8(Min(AExpr.Elements.Count, 255)), 0));

  for I := 0 to AExpr.Elements.Count - 1 do
  begin
    ElemReg := ACtx.Scope.AllocateRegister;
    if AExpr.Elements[I] is TGocciaSpreadExpression then
    begin
      ACtx.CompileExpression(TGocciaSpreadExpression(AExpr.Elements[I]).Argument, ElemReg);
      EmitInstruction(ACtx, EncodeABC(OP_COLLECTION_OP, ADest,
        COLLECTION_OP_SPREAD_ITERABLE_INTO_ARRAY, ElemReg));
    end
    else
    begin
      ACtx.CompileExpression(AExpr.Elements[I], ElemReg);
      EmitInstruction(ACtx, EncodeABC(OP_ARRAY_PUSH, ADest, ElemReg, 0));
    end;
    ACtx.Scope.FreeRegister;
  end;
end;

procedure CompileObjectProperty(const ACtx: TGocciaCompilationContext;
  const ADest: UInt8; const AKey: string; const AValExpr: TGocciaExpression);
var
  ValReg: UInt8;
  DefineOp: TGocciaOpCode;
  FuncCount: Integer;
  InferredTemplate: TGocciaFunctionTemplate;
  ValueExpr: TGocciaExpression;
begin
  ValReg := ACtx.Scope.AllocateRegister;
  FuncCount := ACtx.Template.FunctionCount;
  ValueExpr := AValExpr;
  DefineOp := OP_DEFINE_DATA_PROP;

  if AValExpr is TGocciaObjectMethodDefinition then
  begin
    ValueExpr := TGocciaObjectMethodDefinition(AValExpr).FunctionExpression;
    DefineOp := OP_DEFINE_METHOD_PROP;
    CompileFunctionExpression(ACtx,
      TGocciaObjectMethodDefinition(AValExpr).FunctionExpression,
      ValReg, '<method>');
  end
  else if (ValueExpr is TGocciaClassExpression) and
     (TGocciaClassExpression(ValueExpr).ClassDefinition.Name = '') then
    Goccia.Compiler.Statements.CompileClassExpression(ACtx,
      TGocciaClassExpression(ValueExpr).ClassDefinition, ValReg, AKey)
  else
    ACtx.CompileExpression(ValueExpr, ValReg);

  if (ValueExpr is TGocciaArrowFunctionExpression) or
     (ValueExpr is TGocciaFunctionExpression) then
  begin
    if ACtx.Template.FunctionCount > FuncCount then
    begin
      InferredTemplate := ACtx.Template.GetFunction(
        ACtx.Template.FunctionCount - 1);
      if (InferredTemplate.Name = '<arrow>') or
         (InferredTemplate.Name = '<function>') or
         (InferredTemplate.Name = '<method>') then
        InferredTemplate.Name := AKey;
    end;
  end;

  EmitDefineDataPropertyByName(ACtx, ADest, AKey, ValReg, DefineOp);
  ACtx.Scope.FreeRegister;
end;

procedure CompileGetterProperty(const ACtx: TGocciaCompilationContext;
  const ADest: UInt8; const AKey: string;
  const AGetter: TGocciaGetterExpression);
var
  OldTemplate: TGocciaFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TGocciaFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  ChildCtx: TGocciaCompilationContext;
  FuncIdx: UInt16;
  TargetReg, AccessorReg: UInt8;
  KeyIdx: UInt16;
  EmptyParams: TGocciaParameterArray;
  ArgumentsSlot: Integer;
  I: Integer;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TGocciaFunctionTemplate.Create('get ' + AKey);
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.StrictCode := ChildBodyIsStrictCode(ACtx, AGetter.Body);
  ChildTemplate.StrictThis := ChildTemplate.StrictCode;
  ChildTemplate.SourceText := AGetter.SourceText;
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);
  ChildScope.DeclareLocal(KEYWORD_THIS, False);
  ChildTemplate.ParameterCount := 0;
  SetLength(EmptyParams, 0);
  ArgumentsSlot := DeclareArgumentsObjectLocal(ACtx, ChildScope, EmptyParams);

  ACtx.SwapState(ChildTemplate, ChildScope);
  try
    ChildCtx := ACtx;
    ChildCtx.Template := ChildTemplate;
    ChildCtx.Scope := ChildScope;
    ChildCtx.NonStrictMode := ACtx.NonStrictMode and
      not ChildTemplate.StrictCode;
    EmitLineMapping(ChildCtx, AGetter.Line, AGetter.Column);
    EmitCreateArgumentsObject(ChildCtx, ArgumentsSlot);
    ACtx.CompileFunctionBody(AGetter.Body);
    ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

    for I := 0 to ChildScope.UpvalueCount - 1 do
      ChildTemplate.AddUpvalueDescriptor(
        ChildScope.GetUpvalue(I).IsLocal,
        ChildScope.GetUpvalue(I).Index);
  finally
    ACtx.SwapState(OldTemplate, OldScope);
    ChildScope.Free;
  end;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  KeyIdx := ACtx.Template.AddConstantString(AKey);
  if KeyIdx > High(UInt8) then
    raise Exception.Create('Constant pool overflow: property name index exceeds 255');
  TargetReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABC(OP_MOVE, TargetReg, ADest, 0));
  AccessorReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, AccessorReg, FuncIdx));
  EmitInstruction(ACtx, EncodeABC(OP_DEFINE_ACCESSOR_CONST, TargetReg, 0, UInt8(KeyIdx)));
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileSetterProperty(const ACtx: TGocciaCompilationContext;
  const ADest: UInt8; const AKey: string;
  const ASetter: TGocciaSetterExpression);
var
  OldTemplate: TGocciaFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TGocciaFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  ChildCtx: TGocciaCompilationContext;
  FuncIdx: UInt16;
  TargetReg, AccessorReg: UInt8;
  KeyIdx: UInt16;
  SetterParams: TGocciaParameterArray;
  ArgumentsSlot: Integer;
  I: Integer;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TGocciaFunctionTemplate.Create('set ' + AKey);
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.StrictCode := ChildBodyIsStrictCode(ACtx, ASetter.Body);
  ChildTemplate.StrictThis := ChildTemplate.StrictCode;
  ChildTemplate.SourceText := ASetter.SourceText;
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);
  ChildScope.DeclareLocal(KEYWORD_THIS, False);
  ChildScope.DeclareLocal(ASetter.Parameter, False);
  ChildTemplate.ParameterCount := 1;
  SetLength(SetterParams, 1);
  SetterParams[0].Name := ASetter.Parameter;
  ArgumentsSlot := DeclareArgumentsObjectLocal(ACtx, ChildScope, SetterParams);

  ACtx.SwapState(ChildTemplate, ChildScope);
  try
    ChildCtx := ACtx;
    ChildCtx.Template := ChildTemplate;
    ChildCtx.Scope := ChildScope;
    ChildCtx.NonStrictMode := ACtx.NonStrictMode and
      not ChildTemplate.StrictCode;
    EmitLineMapping(ChildCtx, ASetter.Line, ASetter.Column);
    EmitCreateArgumentsObject(ChildCtx, ArgumentsSlot);
    ACtx.CompileFunctionBody(ASetter.Body);
    ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

    for I := 0 to ChildScope.UpvalueCount - 1 do
      ChildTemplate.AddUpvalueDescriptor(
        ChildScope.GetUpvalue(I).IsLocal,
        ChildScope.GetUpvalue(I).Index);
  finally
    ACtx.SwapState(OldTemplate, OldScope);
    ChildScope.Free;
  end;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  KeyIdx := ACtx.Template.AddConstantString(AKey);
  if KeyIdx > High(UInt8) then
    raise Exception.Create('Constant pool overflow: property name index exceeds 255');
  TargetReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABC(OP_MOVE, TargetReg, ADest, 0));
  AccessorReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, AccessorReg, FuncIdx));
  EmitInstruction(ACtx, EncodeABC(OP_DEFINE_ACCESSOR_CONST, TargetReg, ACCESSOR_FLAG_SETTER, UInt8(KeyIdx)));
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileObject(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaObjectExpression; const ADest: UInt8);
var
  I: Integer;
  Key: string;
  ValExpr: TGocciaExpression;
  KeyReg, ValReg: UInt8;
  DefineOp: TGocciaOpCode;
  Names: TStringList;
  Order: TArray<TGocciaPropertySourceOrder>;
  Pair: TPair<TGocciaExpression, TGocciaExpression>;
  GetterExpr: TGocciaGetterExpression;
  SetterExpr: TGocciaSetterExpression;
begin
  EmitInstruction(ACtx, EncodeABx(OP_NEW_OBJECT, ADest, AExpr.Properties.Count));

  Order := AExpr.PropertySourceOrder;
  if Length(Order) > 0 then
  begin
    for I := 0 to High(Order) do
    begin
      if Order[I].Skip then
        Continue;

      Key := Order[I].StaticKey;
      case Order[I].PropertyType of
        pstStatic:
          if AExpr.Properties.TryGetValue(Key, ValExpr) then
            CompileObjectProperty(ACtx, ADest, Key, ValExpr);
        pstComputed:
        begin
          if (Order[I].ComputedIndex >= 0) and
             (Order[I].ComputedIndex <= High(AExpr.ComputedPropertiesInOrder)) then
          begin
            Pair := AExpr.ComputedPropertiesInOrder[Order[I].ComputedIndex];
            if Pair.Key is TGocciaSpreadExpression then
            begin
              ValReg := ACtx.Scope.AllocateRegister;
              ACtx.CompileExpression(TGocciaSpreadExpression(Pair.Key).Argument, ValReg);
              EmitInstruction(ACtx, EncodeABC(OP_COLLECTION_OP, ADest,
                COLLECTION_OP_SPREAD_OBJECT, ValReg));
              ACtx.Scope.FreeRegister;
            end
            else
            begin
              KeyReg := ACtx.Scope.AllocateRegister;
              ValReg := ACtx.Scope.AllocateRegister;
              ACtx.CompileExpression(Pair.Key, KeyReg);
              if Pair.Value is TGocciaObjectMethodDefinition then
              begin
                DefineOp := OP_DEFINE_METHOD_PROP;
                CompileFunctionExpression(ACtx,
                  TGocciaObjectMethodDefinition(Pair.Value).FunctionExpression,
                  ValReg, '<method>');
              end
              else
              begin
                DefineOp := OP_DEFINE_DATA_PROP;
                ACtx.CompileExpression(Pair.Value, ValReg);
              end;
              EmitInstruction(ACtx, EncodeABC(DefineOp, ADest, KeyReg, ValReg));
              ACtx.Scope.FreeRegister;
              ACtx.Scope.FreeRegister;
            end;
          end;
        end;
        pstGetter:
          if Assigned(AExpr.Getters) and AExpr.Getters.TryGetValue(Key, GetterExpr) then
            CompileGetterProperty(ACtx, ADest, Key, GetterExpr);
        pstSetter:
          if Assigned(AExpr.Setters) and AExpr.Setters.TryGetValue(Key, SetterExpr) then
            CompileSetterProperty(ACtx, ADest, Key, SetterExpr);
      end;
    end;
  end
  else
  begin
    Names := AExpr.GetPropertyNamesInOrder;
    for I := 0 to Names.Count - 1 do
    begin
      Key := Names[I];
      if AExpr.Properties.TryGetValue(Key, ValExpr) then
        CompileObjectProperty(ACtx, ADest, Key, ValExpr);
    end;
  end;
end;

// Template literals without real interpolations are emitted as constant strings.
// The parser pre-segments templates with interpolations into
// TGocciaTemplateWithInterpolationExpression, so this function only handles
// the no-interpolation case.
procedure CompileTemplateLiteral(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaTemplateLiteralExpression; const ADest: UInt8);
begin
  EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, ADest,
    ACtx.Template.AddConstantString(AExpr.Value)));
end;

// ES2026 §13.15.5.1 Template Literals — compile pre-segmented template.
// Expression parts must use OP_TO_STRING to apply the "string" ToPrimitive
// hint (preferring toString() over valueOf()), matching the spec's ToString
// semantics rather than OP_ADD's "default" hint.
procedure CompileTemplateWithInterpolation(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaTemplateWithInterpolationExpression; const ADest: UInt8);
var
  I: Integer;
  PartReg: UInt8;
begin
  if AExpr.Parts.Count = 0 then
  begin
    EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, ADest,
      ACtx.Template.AddConstantString('')));
    Exit;
  end;

  ACtx.CompileExpression(AExpr.Parts[0], ADest);

  for I := 1 to AExpr.Parts.Count - 1 do
  begin
    PartReg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(AExpr.Parts[I], PartReg);
    // ES2026 §13.15.5.1 step 5e: ToString(value) on each substitution
    if not (AExpr.Parts[I] is TGocciaLiteralExpression) then
      EmitInstruction(ACtx, EncodeABC(OP_TO_STRING, PartReg, PartReg, 0));
    EmitInstruction(ACtx, EncodeABC(OP_ADD, ADest, ADest, PartReg));
    ACtx.Scope.FreeRegister;
  end;
end;

// ES2026 §13.2.8.3 GetTemplateObject — step 8: DefinePropertyOrThrow(template, "raw", {...})
procedure EmitDefineNonEnumerableProperty(const ACtx: TGocciaCompilationContext;
  const AObjReg, AValueReg: UInt8; const APropName: string);
var
  GlobalObjReg, DefPropReg, ObjArgReg, NameArgReg, DescReg: UInt8;
  ObjectIdx, DefPropIdx, ValuePropIdx, PropNameIdx: UInt16;
begin
  GlobalObjReg := ACtx.Scope.AllocateRegister;
  DefPropReg   := ACtx.Scope.AllocateRegister;
  ObjArgReg    := ACtx.Scope.AllocateRegister;
  NameArgReg   := ACtx.Scope.AllocateRegister;
  DescReg      := ACtx.Scope.AllocateRegister;

  ObjectIdx   := ACtx.Template.AddConstantString(CONSTRUCTOR_OBJECT);
  DefPropIdx  := ACtx.Template.AddConstantString(PROP_DEFINE_PROPERTY);
  PropNameIdx := ACtx.Template.AddConstantString(APropName);
  ValuePropIdx := ACtx.Template.AddConstantString(PROP_VALUE);
  if (ObjectIdx > High(UInt16)) or (DefPropIdx > High(UInt8)) or
     (PropNameIdx > High(UInt16)) or (ValuePropIdx > High(UInt8)) then
    raise Exception.Create('Constant pool overflow');

  // Look up Object.defineProperty
  EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, GlobalObjReg, ObjectIdx));
  EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, DefPropReg, GlobalObjReg, UInt8(DefPropIdx)));

  // Argument 1: the target object
  EmitInstruction(ACtx, EncodeABC(OP_MOVE, ObjArgReg, AObjReg, 0));

  // Argument 2: the property name string
  EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, NameArgReg, PropNameIdx));

  // Argument 3: the descriptor {value: AValueReg}
  // Omitting enumerable/writable/configurable → all default to false per spec
  EmitInstruction(ACtx, EncodeABC(OP_NEW_OBJECT, DescReg, 0, 0));
  EmitInstruction(ACtx, EncodeABC(OP_SET_PROP_CONST, DescReg, UInt8(ValuePropIdx), AValueReg));

  // Object.defineProperty(obj, propName, descriptor) — 3 args
  EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, DefPropReg, 3, 0));

  ACtx.Scope.FreeRegister; // DescReg
  ACtx.Scope.FreeRegister; // NameArgReg
  ACtx.Scope.FreeRegister; // ObjArgReg
  ACtx.Scope.FreeRegister; // DefPropReg
  ACtx.Scope.FreeRegister; // GlobalObjReg
end;

// ES2026 §13.2.8.3 GetTemplateObject — step 12: SetIntegrityLevel(template, frozen)
procedure EmitObjectFreeze(const ACtx: TGocciaCompilationContext; const AReg: UInt8);
var
  GlobalObjReg, FreezeReg, FreezeArgReg: UInt8;
  ObjectIdx, FreezeIdx: UInt16;
begin
  GlobalObjReg := ACtx.Scope.AllocateRegister;
  FreezeReg := ACtx.Scope.AllocateRegister;
  FreezeArgReg := ACtx.Scope.AllocateRegister;

  ObjectIdx := ACtx.Template.AddConstantString(CONSTRUCTOR_OBJECT);
  FreezeIdx := ACtx.Template.AddConstantString(PROP_FREEZE);
  if (ObjectIdx > High(UInt16)) or (FreezeIdx > High(UInt8)) then
    raise Exception.Create('Constant pool overflow');

  EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, GlobalObjReg, ObjectIdx));
  EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, FreezeReg, GlobalObjReg, UInt8(FreezeIdx)));
  // Move the target array into the argument register
  EmitInstruction(ACtx, EncodeABC(OP_MOVE, FreezeArgReg, AReg, 0));
  // OP_CALL_METHOD: function in FreezeReg, this = GlobalObjReg (FreezeReg - 1), 1 arg
  EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, FreezeReg, 1, 0));
  // Result lands in FreezeReg but we discard it; the array is frozen in place

  ACtx.Scope.FreeRegister; // FreezeArgReg
  ACtx.Scope.FreeRegister; // FreezeReg
  ACtx.Scope.FreeRegister; // GlobalObjReg
end;

// Resolves the tag callee into a register pair ready for OP_CALL or OP_CALL_METHOD.
// For a member-expression tag (obj.method`...`):
//   allocates [AObjReg, ABaseReg] in that order so the VM finds the receiver at BaseReg-1,
//   compiles the object into AObjReg, and fetches the property into ABaseReg.
// For any other tag expression: allocates only ABaseReg and compiles the callee into it.
// AIsMethodCall tells the caller which opcode to emit and whether to free AObjReg afterward.
procedure CompileTagCalleeRegisters(const ACtx: TGocciaCompilationContext;
  const ATagExpr: TGocciaExpression;
  out ABaseReg, AObjReg: UInt8;
  out AIsMethodCall: Boolean);
var
  MemberExpr: TGocciaMemberExpression;
  IdentExpr: TGocciaIdentifierExpression;
  KeyReg, CondReg: UInt8;
  NameIdx: UInt16;
  I, EndCount: Integer;
  MissJump: Integer;
  EndJumps: array of Integer;
begin
  if ATagExpr is TGocciaMemberExpression then
  begin
    AIsMethodCall := True;
    MemberExpr := TGocciaMemberExpression(ATagExpr);
    AObjReg  := ACtx.Scope.AllocateRegister;
    ABaseReg := ACtx.Scope.AllocateRegister;

    ACtx.CompileExpression(MemberExpr.ObjectExpr, AObjReg);
    if MemberExpr.Computed then
    begin
      ACtx.CompileExpression(MemberExpr.PropertyExpression, ABaseReg);
      EmitInstruction(ACtx, EncodeABC(OP_ARRAY_GET, ABaseReg, AObjReg, ABaseReg));
    end
    else
      EmitLoadPropertyByName(ACtx, ABaseReg, AObjReg,
        MemberExpr.PropertyName);
  end
  else if (ATagExpr is TGocciaIdentifierExpression) and
          ShouldTryWithBinding(ACtx.Scope,
            TGocciaIdentifierExpression(ATagExpr).Name) then
  begin
    AIsMethodCall := True;
    IdentExpr := TGocciaIdentifierExpression(ATagExpr);
    AObjReg := ACtx.Scope.AllocateRegister;
    ABaseReg := ACtx.Scope.AllocateRegister;
    KeyReg := ACtx.Scope.AllocateRegister;
    CondReg := ACtx.Scope.AllocateRegister;
    EndCount := 0;
    SetLength(EndJumps, ACtx.Scope.WithBindingCount);
    try
      NameIdx := ACtx.Template.AddConstantString(IdentExpr.Name);
      EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, AObjReg, 0, 0));
      EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, KeyReg, NameIdx));

      for I := ACtx.Scope.WithBindingCount - 1 downto 0 do
      begin
        EmitLoadHiddenWithObject(ACtx, ACtx.Scope.GetWithBindingName(I),
          ABaseReg);
        EmitInstruction(ACtx, EncodeABC(OP_HAS_WITH_BINDING, CondReg,
          ABaseReg, KeyReg));
        MissJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, CondReg);
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, AObjReg, ABaseReg, 0));
        EmitInstruction(ACtx, EncodeABC(OP_GET_INDEX, ABaseReg, AObjReg,
          KeyReg));
        EndJumps[EndCount] := EmitJumpInstruction(ACtx, OP_JUMP, 0);
        Inc(EndCount);
        PatchJumpTarget(ACtx, MissJump);
      end;

      CompileIdentifierAccessNoWith(ACtx, IdentExpr, ABaseReg, False);

      for I := 0 to EndCount - 1 do
        PatchJumpTarget(ACtx, EndJumps[I]);
    finally
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
    end;
  end
  else
  begin
    AIsMethodCall := False;
    ABaseReg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(ATagExpr, ABaseReg);
    AObjReg := 0; // unused — caller must not free when AIsMethodCall = false
  end;
end;

// ES2026 §13.3.11 TaggedTemplate
procedure CompileTaggedTemplate(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaTaggedTemplateExpression; const ADest: UInt8);
var
  ObjReg, BaseReg, Arg0Reg: UInt8;
  ArgCount, I: Integer;
  IsMethodCall: Boolean;
begin
  ArgCount := 1 + AExpr.Expressions.Count; // template object + substitution values
  if ArgCount > High(UInt8) then
    raise Exception.Create('Compiler error: too many tagged template substitutions (>254)');

  CompileTagCalleeRegisters(ACtx, AExpr.Tag, BaseReg, ObjReg, IsMethodCall);

  // ES2026 §13.2.8.3 GetTemplateObject — emit a single OP_LOAD_CONST with a
  // bckTemplateObject constant.  The VM lazily builds, freezes, and pins the
  // template object on first execution of this instruction, then returns the
  // identical cached object on every subsequent call to the same call site,
  // satisfying the per-Parse-Node identity requirement without a new opcode.
  Arg0Reg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, Arg0Reg,
    ACtx.Template.AddConstantTemplateObject(AExpr.CookedStrings, AExpr.RawStrings,
      AExpr.CookedValid)));

  // ES2026 §13.3.11 step 3: evaluate substitutions into argument registers
  for I := 0 to AExpr.Expressions.Count - 1 do
    ACtx.CompileExpression(AExpr.Expressions[I], ACtx.Scope.AllocateRegister);

  if IsMethodCall then
    EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, BaseReg, UInt8(ArgCount), 0))
  else
    EmitInstruction(ACtx, EncodeABC(OP_CALL, BaseReg, UInt8(ArgCount), 0));

  for I := 0 to AExpr.Expressions.Count - 1 do
    ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister; // Arg0Reg

  if ADest <> BaseReg then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, BaseReg, 0));

  ACtx.Scope.FreeRegister; // BaseReg
  if IsMethodCall then
    ACtx.Scope.FreeRegister; // ObjReg
end;

function NewExprHasSpread(const AExpr: TGocciaNewExpression): Boolean;
var
  I: Integer;
begin
  for I := 0 to AExpr.Arguments.Count - 1 do
    if AExpr.Arguments[I] is TGocciaSpreadExpression then
      Exit(True);
  Result := False;
end;

procedure CompileNewExpression(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaNewExpression; const ADest: UInt8);
var
  CtorReg, ArgsReg: UInt8;
  ArgCount, I: Integer;
begin
  CtorReg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AExpr.Callee, CtorReg);

  if NewExprHasSpread(AExpr) then
  begin
    ArgsReg := ACtx.Scope.AllocateRegister;
    CompileSpreadArgsArrayFromList(ACtx, AExpr.Arguments, ArgsReg);
    EmitInstruction(ACtx, EncodeABC(OP_CONSTRUCT, ADest, CtorReg, ArgsReg or $80));
    ACtx.Scope.FreeRegister; // ArgsReg
  end
  else
  begin
    ArgCount := AExpr.Arguments.Count;
    if ArgCount > High(UInt8) then
      raise Exception.Create('Compiler error: too many constructor arguments (>255)');
    for I := 0 to ArgCount - 1 do
      ACtx.CompileExpression(AExpr.Arguments[I], ACtx.Scope.AllocateRegister);
    EmitInstruction(ACtx, EncodeABC(OP_CONSTRUCT, ADest, CtorReg, UInt8(ArgCount)));
    for I := 0 to ArgCount - 1 do
      ACtx.Scope.FreeRegister;
  end;

  ACtx.Scope.FreeRegister; // CtorReg
end;

procedure CompileComputedPropertyAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaComputedPropertyAssignmentExpression; const ADest: UInt8);
var
  ObjReg, KeyReg, ValReg: UInt8;
begin
  ObjReg := ACtx.Scope.AllocateRegister;
  KeyReg := ACtx.Scope.AllocateRegister;
  ValReg := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(AExpr.ObjectExpr, ObjReg);
  ACtx.CompileExpression(AExpr.PropertyExpression, KeyReg);
  ACtx.CompileExpression(AExpr.Value, ValReg);

  EmitInstruction(ACtx, EncodeABC(StoreByKeyOpcode(ACtx), ObjReg, KeyReg,
    ValReg));

  if ADest <> ValReg then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, ValReg, 0));

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileFunctionExpression(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaFunctionExpression; const ADest: UInt8;
  const ATemplateName: string);
var
  OldTemplate: TGocciaFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TGocciaFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  ChildCtx: TGocciaCompilationContext;
  FuncIdx: UInt16;
  FormalCount: Integer;
  RestParamIndex: Integer;
  RestReg: Integer;
  I: Integer;
  ArgumentsSlot: Integer;
  HasNameBinding: Boolean;
  NameSlot: UInt8;
  ClosedLocals: array[0..0] of UInt8;
  ClosedCount: Integer;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  // ES2026 §15.2.5: Named function expressions bind the name in an inner
  // block scope visible inside the body via upvalue capture
  HasNameBinding := AExpr.Name <> '';
  if HasNameBinding then
  begin
    OldScope.BeginScope;
    NameSlot := OldScope.DeclareLocal(AExpr.Name, True);
  end;

  ChildTemplate := TGocciaFunctionTemplate.Create(ATemplateName);
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.IsAsync := AExpr.IsAsync;
  ChildTemplate.IsGenerator := AExpr.IsGenerator;
  ChildTemplate.HasOwnPrototype := AExpr.HasOwnPrototype;
  ChildTemplate.StrictCode := ChildBodyIsStrictCode(ACtx, AExpr.Body);
  ChildTemplate.StrictThis := ChildTemplate.StrictCode;
  ChildTemplate.SourceText := AExpr.SourceText;
  if HasNameBinding then
    ChildTemplate.Name := AExpr.Name;
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);

  ChildScope.DeclareLocal(KEYWORD_THIS, False);
  ChildTemplate.ParameterCount := Length(AExpr.Parameters);

  FormalCount := -1;
  RestParamIndex := -1;
  for I := 0 to High(AExpr.Parameters) do
  begin
    if AExpr.Parameters[I].IsRest or Assigned(AExpr.Parameters[I].DefaultValue) then
    begin
      if FormalCount < 0 then
        FormalCount := I;
      if AExpr.Parameters[I].IsRest then
        RestParamIndex := I;
    end;
    if AExpr.Parameters[I].IsPattern then
      ChildScope.DeclareLocal('__param' + IntToStr(I), False)
    else
      ChildScope.DeclareLocal(AExpr.Parameters[I].Name, False);
  end;
  for I := 0 to High(AExpr.Parameters) do
    if AExpr.Parameters[I].IsPattern and Assigned(AExpr.Parameters[I].Pattern) then
      CollectDestructuringBindings(AExpr.Parameters[I].Pattern, ChildScope);

  ArgumentsSlot := DeclareArgumentsObjectLocal(ACtx, ChildScope, AExpr.Parameters);

  ApplyParameterTypeAnnotations(ChildScope, ChildTemplate, AExpr.Parameters,
    ACtx.StrictTypes);

  if FormalCount < 0 then
    FormalCount := Length(AExpr.Parameters);
  ChildTemplate.FormalParameterCount := UInt8(FormalCount);
  if Assigned(ACtx.FormalParameterCounts) then
    ACtx.FormalParameterCounts.AddOrSetValue(ChildTemplate, FormalCount);

  ACtx.SwapState(ChildTemplate, ChildScope);
  try
    ChildCtx := ACtx;
    ChildCtx.Template := ChildTemplate;
    ChildCtx.Scope := ChildScope;
    ChildCtx.NonStrictMode := ACtx.NonStrictMode and
      not ChildTemplate.StrictCode;

    EmitCreateArgumentsObject(ChildCtx, ArgumentsSlot);

    if RestParamIndex >= 0 then
    begin
      RestReg := ChildScope.ResolveLocal(AExpr.Parameters[RestParamIndex].Name);
      EmitInstruction(ChildCtx,
        EncodeABC(OP_PACK_ARGS, UInt8(RestReg), UInt8(RestParamIndex), 0));
    end;

    EmitDefaultParameters(ChildCtx, AExpr.Parameters);
    EmitDestructuringParameters(ChildCtx, AExpr.Parameters);
    EmitParameterTypeChecks(ChildCtx, AExpr.Parameters);

    ACtx.CompileFunctionBody(AExpr.Body);

    ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

    for I := 0 to ChildScope.UpvalueCount - 1 do
      ChildTemplate.AddUpvalueDescriptor(
        ChildScope.GetUpvalue(I).IsLocal,
        ChildScope.GetUpvalue(I).Index);
  finally
    ACtx.SwapState(OldTemplate, OldScope);
    ChildScope.Free;
  end;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, ADest, FuncIdx));

  if HasNameBinding then
  begin
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, NameSlot, ADest, 0));
    OldScope.EndScope(ClosedLocals, ClosedCount);
    for I := 0 to ClosedCount - 1 do
      EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));
  end;
end;

procedure CompileCompoundAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaCompoundAssignmentExpression; const ADest: UInt8);
var
  LocalIdx, UpvalIdx: Integer;
  Slot, RegVal, RegResult, RegTemp, CondReg, ArgReg: UInt8;
  NameIdx: UInt16;
  Op, FloatOp: TGocciaOpCode;
  LocalType, ValType, ResultType: TGocciaLocalType;
  ArithOp: TGocciaTokenType;
  JumpIdx, OkJump: Integer;
begin
  // ES2026 §13.15.2 AssignmentExpression : LeftHandSideExpression ??=/&&=/||= AssignmentExpression
  if IsShortCircuitAssignment(AExpr.Operator) then
  begin
    if ShouldTryWithBinding(ACtx.Scope, AExpr.Name) then
    begin
      Op := ShortCircuitJumpOp(AExpr.Operator);
      EmitLoadBindingByName(ACtx, AExpr.Name, ADest, False);
      JumpIdx := EmitJumpInstruction(ACtx, Op, ADest);
      ACtx.CompileExpression(AExpr.Value, ADest);
      EmitWithAssignmentOrFallback(ACtx, AExpr.Name, ADest);
      PatchJumpTarget(ACtx, JumpIdx);
      Exit;
    end;

    Op := ShortCircuitJumpOp(AExpr.Operator);
    LocalIdx := ACtx.Scope.ResolveLocal(AExpr.Name);
    if LocalIdx >= 0 then
    begin
      if ACtx.Scope.GetLocal(LocalIdx).IsGlobalBacked then
      begin
        EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, ADest,
          ACtx.Template.AddConstantString(AExpr.Name)));
        JumpIdx := EmitJumpInstruction(ACtx, Op, ADest);
        ACtx.CompileExpression(AExpr.Value, ADest);
        LocalType := ACtx.Scope.GetLocal(LocalIdx).TypeHint;
        if ACtx.Scope.GetLocal(LocalIdx).IsStrictlyTyped and
           (LocalType <> sltUntyped) and
           not TypesAreCompatible(InferLocalType(AExpr.Value), LocalType) then
          EmitInstruction(ACtx, EncodeABC(OP_CHECK_TYPE, ADest,
            UInt8(Ord(LocalType)), 0));
        if ACtx.Scope.GetLocal(LocalIdx).IsConst then
          EmitConstAssignmentError(ACtx)
        else
        begin
          EmitSetGlobalByName(ACtx, ADest, AExpr.Name);
          SetNonStrictLocalTypeHint(ACtx, LocalIdx, sltUntyped);
        end;
        PatchJumpTarget(ACtx, JumpIdx);
        Exit;
      end;

      Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
      if ADest <> Slot then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, Slot, 0));
      JumpIdx := EmitJumpInstruction(ACtx, Op, ADest);
      LocalType := ACtx.Scope.GetLocal(LocalIdx).TypeHint;
      if ACtx.Scope.GetLocal(LocalIdx).IsConst then
      begin
        RegVal := ACtx.Scope.AllocateRegister;
        ACtx.CompileExpression(AExpr.Value, RegVal);
        if ACtx.Scope.GetLocal(LocalIdx).IsStrictlyTyped and
           (LocalType <> sltUntyped) and
           not TypesAreCompatible(InferLocalType(AExpr.Value), LocalType) then
          EmitInstruction(ACtx, EncodeABC(OP_CHECK_TYPE, RegVal,
            UInt8(Ord(LocalType)), 0));
        EmitConstAssignmentError(ACtx);
        ACtx.Scope.FreeRegister;
      end
      else
      begin
        ACtx.CompileExpression(AExpr.Value, ADest);
        EmitStrictLocalTypeCheck(ACtx, LocalIdx, ADest,
          InferLocalType(AExpr.Value));
        if ADest <> Slot then
          EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, ADest, Slot));
        SetNonStrictLocalTypeHint(ACtx, LocalIdx, sltUntyped);
      end;
      PatchJumpTarget(ACtx, JumpIdx);
      Exit;
    end;

    UpvalIdx := ACtx.Scope.ResolveUpvalue(AExpr.Name);
    if UpvalIdx >= 0 then
    begin
      if ACtx.Scope.GetUpvalue(UpvalIdx).IsGlobalBacked then
        EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, ADest,
          ACtx.Template.AddConstantString(AExpr.Name)))
      else
        EmitInstruction(ACtx, EncodeABx(OP_GET_UPVALUE, ADest, UInt16(UpvalIdx)));
      JumpIdx := EmitJumpInstruction(ACtx, Op, ADest);
      ACtx.CompileExpression(AExpr.Value, ADest);
      if ACtx.Scope.GetUpvalue(UpvalIdx).IsStrictlyTyped and
         not TypesAreCompatible(InferLocalType(AExpr.Value),
           ACtx.Scope.GetUpvalue(UpvalIdx).TypeHint) then
        EmitInstruction(ACtx, EncodeABC(OP_CHECK_TYPE, ADest,
          UInt8(Ord(ACtx.Scope.GetUpvalue(UpvalIdx).TypeHint)), 0));
      if ACtx.Scope.GetUpvalue(UpvalIdx).IsConst then
        EmitConstAssignmentError(ACtx)
      else
      begin
        if ACtx.Scope.GetUpvalue(UpvalIdx).IsGlobalBacked then
          EmitSetGlobalByName(ACtx, ADest, AExpr.Name)
        else
          EmitInstruction(ACtx, EncodeABx(OP_SET_UPVALUE, ADest, UInt16(UpvalIdx)));
      end;
      PatchJumpTarget(ACtx, JumpIdx);
      Exit;
    end;

    NameIdx := ACtx.Template.AddConstantString(AExpr.Name);
    CondReg := ACtx.Scope.AllocateRegister;
    ArgReg := ACtx.Scope.AllocateRegister;
    EmitInstruction(ACtx, EncodeABx(OP_HAS_GLOBAL, CondReg, NameIdx));
    OkJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, CondReg);
    EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, CondReg,
      ACtx.Template.AddConstantString(REFERENCE_ERROR_NAME)));
    EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, ArgReg,
      ACtx.Template.AddConstantString(AExpr.Name + ' is not defined')));
    EmitInstruction(ACtx, EncodeABC(OP_CONSTRUCT, CondReg, CondReg, 1));
    EmitInstruction(ACtx, EncodeABC(OP_THROW, CondReg, 0, 0));
    PatchJumpTarget(ACtx, OkJump);
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;

    EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, ADest, NameIdx));
    JumpIdx := EmitJumpInstruction(ACtx, Op, ADest);
    ACtx.CompileExpression(AExpr.Value, ADest);
    EmitSetGlobalByIndex(ACtx, ADest, NameIdx);
    PatchJumpTarget(ACtx, JumpIdx);
    Exit;
  end;

  Op := CompoundOpToRuntimeOp(AExpr.Operator);

  if ShouldTryWithBinding(ACtx.Scope, AExpr.Name) then
  begin
    RegVal := ACtx.Scope.AllocateRegister;
    RegResult := ACtx.Scope.AllocateRegister;
    EmitLoadBindingByName(ACtx, AExpr.Name, RegResult, False);
    ACtx.CompileExpression(AExpr.Value, RegVal);
    EmitInstruction(ACtx, EncodeABC(Op, ADest, RegResult, RegVal));
    EmitWithAssignmentOrFallback(ACtx, AExpr.Name, ADest);
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    Exit;
  end;

  LocalIdx := ACtx.Scope.ResolveLocal(AExpr.Name);
  if LocalIdx >= 0 then
  begin
    if ACtx.Scope.GetLocal(LocalIdx).IsGlobalBacked then
    begin
      RegVal := ACtx.Scope.AllocateRegister;
      RegResult := ACtx.Scope.AllocateRegister;
      EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, RegResult,
        ACtx.Template.AddConstantString(AExpr.Name)));
      ACtx.CompileExpression(AExpr.Value, RegVal);
      LocalType := ACtx.Scope.GetLocal(LocalIdx).TypeHint;
      ValType := ExpressionType(ACtx.Scope, AExpr.Value);
      ResultType := sltUntyped;
      if IsArithmeticCompoundAssign(AExpr.Operator, ArithOp) and
         IsKnownNumeric(LocalType) and IsKnownNumeric(ValType) then
        ResultType := sltFloat;
      if ACtx.Scope.GetLocal(LocalIdx).IsConst then
        EmitConstAssignmentError(ACtx)
      else
      begin
        EmitInstruction(ACtx, EncodeABC(Op, ADest, RegResult, RegVal));
        EmitStrictLocalTypeCheck(ACtx, LocalIdx, ADest, ResultType);
        EmitSetGlobalByName(ACtx, ADest, AExpr.Name);
        SetNonStrictLocalTypeHint(ACtx, LocalIdx, sltUntyped);
      end;
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
      Exit;
    end;
    Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
    RegVal := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(AExpr.Value, RegVal);

    LocalType := ACtx.Scope.GetLocal(LocalIdx).TypeHint;
    if ACtx.Scope.GetLocal(LocalIdx).IsConst then
    begin
      EmitConstAssignmentError(ACtx);
      ACtx.Scope.FreeRegister;
      Exit;
    end;
    ValType := ExpressionType(ACtx.Scope, AExpr.Value);
    RegTemp := ACtx.Scope.AllocateRegister;
    if IsKnownNumeric(LocalType) and
       IsKnownNumeric(ValType) and
       IsArithmeticCompoundAssign(AExpr.Operator, ArithOp) and
       TryFloatOp(ArithOp, FloatOp) then
    begin
      EmitInstruction(ACtx, EncodeABC(FloatOp, RegTemp, Slot, RegVal))
    end
    else
      EmitInstruction(ACtx, EncodeABC(Op, RegTemp, Slot, RegVal));
    ResultType := sltUntyped;
    if IsArithmeticCompoundAssign(AExpr.Operator, ArithOp) and
       IsKnownNumeric(LocalType) and IsKnownNumeric(ValType) then
      ResultType := sltFloat;
    EmitStrictLocalTypeCheck(ACtx, LocalIdx, RegTemp, ResultType);
    EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, RegTemp, Slot));
    if not ACtx.Scope.GetLocal(LocalIdx).IsStrictlyTyped then
    begin
      SetNonStrictLocalTypeHint(ACtx, LocalIdx, ResultType);
    end;
    if ADest <> Slot then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, RegTemp, 0));
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    Exit;
  end;

  UpvalIdx := ACtx.Scope.ResolveUpvalue(AExpr.Name);
  if UpvalIdx >= 0 then
  begin
    RegVal := ACtx.Scope.AllocateRegister;
    RegResult := ACtx.Scope.AllocateRegister;
    if ACtx.Scope.GetUpvalue(UpvalIdx).IsGlobalBacked then
      EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, RegResult,
        ACtx.Template.AddConstantString(AExpr.Name)))
    else
      EmitInstruction(ACtx, EncodeABx(OP_GET_UPVALUE, RegResult, UInt16(UpvalIdx)));
    ACtx.CompileExpression(AExpr.Value, RegVal);
    if ACtx.Scope.GetUpvalue(UpvalIdx).IsStrictlyTyped and
       not TypesAreCompatible(InferLocalType(AExpr.Value),
         ACtx.Scope.GetUpvalue(UpvalIdx).TypeHint) then
      EmitInstruction(ACtx, EncodeABC(OP_CHECK_TYPE, RegVal,
        UInt8(Ord(ACtx.Scope.GetUpvalue(UpvalIdx).TypeHint)), 0));
    if ACtx.Scope.GetUpvalue(UpvalIdx).IsConst then
      EmitConstAssignmentError(ACtx)
    else
    begin
      EmitInstruction(ACtx, EncodeABC(Op, RegResult, RegResult, RegVal));
      if ACtx.Scope.GetUpvalue(UpvalIdx).IsGlobalBacked then
        EmitSetGlobalByName(ACtx, RegResult, AExpr.Name)
      else
        EmitInstruction(ACtx, EncodeABx(OP_SET_UPVALUE, RegResult,
          UInt16(UpvalIdx)));
      if ADest <> RegResult then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, RegResult, 0));
    end;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    Exit;
  end;

  RegVal := ACtx.Scope.AllocateRegister;
  RegResult := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, RegResult,
    ACtx.Template.AddConstantString(AExpr.Name)));
  ACtx.CompileExpression(AExpr.Value, RegVal);
  EmitInstruction(ACtx, EncodeABC(Op, ADest, RegResult, RegVal));
  EmitSetGlobalByName(ACtx, ADest, AExpr.Name);
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompilePropertyCompoundAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaPropertyCompoundAssignmentExpression; const ADest: UInt8);
var
  ObjReg, CurReg, ValReg: UInt8;
  Op: TGocciaOpCode;
  PropIdx: UInt16;
  JumpIdx: Integer;
begin
  // ES2026 §13.15.2 AssignmentExpression : LeftHandSideExpression ??=/&&=/||= AssignmentExpression
  if IsShortCircuitAssignment(AExpr.Operator) then
  begin
    ObjReg := ACtx.Scope.AllocateRegister;
    CurReg := ACtx.Scope.AllocateRegister;

    ACtx.CompileExpression(AExpr.ObjectExpr, ObjReg);
    EmitLoadPropertyByName(ACtx, CurReg, ObjReg, AExpr.PropertyName);
    JumpIdx := EmitJumpInstruction(ACtx, ShortCircuitJumpOp(AExpr.Operator), CurReg);
    ACtx.CompileExpression(AExpr.Value, CurReg);
    EmitStorePropertyByName(ACtx, ObjReg, AExpr.PropertyName, CurReg);
    PatchJumpTarget(ACtx, JumpIdx);

    if ADest <> CurReg then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, CurReg, 0));

    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    Exit;
  end;

  Op := CompoundOpToRuntimeOp(AExpr.Operator);
  ObjReg := ACtx.Scope.AllocateRegister;
  CurReg := ACtx.Scope.AllocateRegister;
  ValReg := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(AExpr.ObjectExpr, ObjReg);
  EmitLoadPropertyByName(ACtx, CurReg, ObjReg, AExpr.PropertyName);
  ACtx.CompileExpression(AExpr.Value, ValReg);
  EmitInstruction(ACtx, EncodeABC(Op, CurReg, CurReg, ValReg));
  EmitStorePropertyByName(ACtx, ObjReg, AExpr.PropertyName, CurReg);

  if ADest <> CurReg then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, CurReg, 0));

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileComputedPropertyCompoundAssignment(
  const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaComputedPropertyCompoundAssignmentExpression;
  const ADest: UInt8);
var
  ObjReg, KeyReg, CurReg, ValReg: UInt8;
  Op: TGocciaOpCode;
  JumpIdx: Integer;
begin
  // ES2026 §13.15.2 AssignmentExpression : LeftHandSideExpression ??=/&&=/||= AssignmentExpression
  if IsShortCircuitAssignment(AExpr.Operator) then
  begin
    ObjReg := ACtx.Scope.AllocateRegister;
    KeyReg := ACtx.Scope.AllocateRegister;
    CurReg := ACtx.Scope.AllocateRegister;

    ACtx.CompileExpression(AExpr.ObjectExpr, ObjReg);
    ACtx.CompileExpression(AExpr.PropertyExpression, KeyReg);
    EmitInstruction(ACtx, EncodeABC(OP_ARRAY_GET, CurReg, ObjReg, KeyReg));
    JumpIdx := EmitJumpInstruction(ACtx, ShortCircuitJumpOp(AExpr.Operator), CurReg);
    ACtx.CompileExpression(AExpr.Value, CurReg);
    EmitInstruction(ACtx, EncodeABC(StoreByKeyOpcode(ACtx), ObjReg, KeyReg,
      CurReg));
    PatchJumpTarget(ACtx, JumpIdx);

    if ADest <> CurReg then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, CurReg, 0));

    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    Exit;
  end;

  Op := CompoundOpToRuntimeOp(AExpr.Operator);
  ObjReg := ACtx.Scope.AllocateRegister;
  KeyReg := ACtx.Scope.AllocateRegister;
  CurReg := ACtx.Scope.AllocateRegister;
  ValReg := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(AExpr.ObjectExpr, ObjReg);
  ACtx.CompileExpression(AExpr.PropertyExpression, KeyReg);
  EmitInstruction(ACtx, EncodeABC(OP_ARRAY_GET, CurReg, ObjReg, KeyReg));
  ACtx.CompileExpression(AExpr.Value, ValReg);
  EmitInstruction(ACtx, EncodeABC(Op, CurReg, CurReg, ValReg));
  EmitInstruction(ACtx, EncodeABC(StoreByKeyOpcode(ACtx), ObjReg, KeyReg,
    CurReg));

  if ADest <> CurReg then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, CurReg, 0));

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileIncrementMember(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIncrementExpression; const AMember: TGocciaMemberExpression;
  const ADest: UInt8; const AOp: TGocciaOpCode);
var
  ObjReg, CurReg: UInt8;
begin
  ObjReg := ACtx.Scope.AllocateRegister;
  CurReg := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(AMember.ObjectExpr, ObjReg);
  EmitLoadPropertyByName(ACtx, CurReg, ObjReg, AMember.PropertyName);
  EmitInstruction(ACtx, EncodeABC(OP_TO_NUMERIC, CurReg, CurReg, 0));
  if not AExpr.IsPrefix then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, CurReg, 0));
  EmitInstruction(ACtx, EncodeABC(AOp, CurReg, CurReg, 0));
  EmitStorePropertyByName(ACtx, ObjReg, AMember.PropertyName, CurReg);
  if AExpr.IsPrefix then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, CurReg, 0));

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileIncrementComputedMember(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIncrementExpression; const AMember: TGocciaMemberExpression;
  const ADest: UInt8; const AOp: TGocciaOpCode);
var
  ObjReg, KeyReg, CurReg: UInt8;
begin
  ObjReg := ACtx.Scope.AllocateRegister;
  KeyReg := ACtx.Scope.AllocateRegister;
  CurReg := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(AMember.ObjectExpr, ObjReg);
  ACtx.CompileExpression(AMember.PropertyExpression, KeyReg);
  EmitInstruction(ACtx, EncodeABC(OP_ARRAY_GET, CurReg, ObjReg, KeyReg));
  EmitInstruction(ACtx, EncodeABC(OP_TO_NUMERIC, CurReg, CurReg, 0));
  if not AExpr.IsPrefix then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, CurReg, 0));
  EmitInstruction(ACtx, EncodeABC(AOp, CurReg, CurReg, 0));
  EmitInstruction(ACtx, EncodeABC(StoreByKeyOpcode(ACtx), ObjReg, KeyReg,
    CurReg));
  if AExpr.IsPrefix then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, CurReg, 0));

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileIncrement(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIncrementExpression; const ADest: UInt8);
var
  LocalIdx, UpvalIdx: Integer;
  Slot, RegResult: UInt8;
  Op: TGocciaOpCode;
  Ident: TGocciaIdentifierExpression;
  MemberExpr: TGocciaMemberExpression;
begin
  if AExpr.Operator = gttIncrement then
    Op := OP_INC
  else
    Op := OP_DEC;

  if AExpr.Operand is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(AExpr.Operand);
    if MemberExpr.Computed then
      CompileIncrementComputedMember(ACtx, AExpr, MemberExpr, ADest, Op)
    else
      CompileIncrementMember(ACtx, AExpr, MemberExpr, ADest, Op);
    Exit;
  end;

  if not (AExpr.Operand is TGocciaIdentifierExpression) then
  begin
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, ADest, 0, 0));
    Exit;
  end;

  Ident := TGocciaIdentifierExpression(AExpr.Operand);

  if ShouldTryWithBinding(ACtx.Scope, Ident.Name) then
  begin
    RegResult := ACtx.Scope.AllocateRegister;
    EmitLoadBindingByName(ACtx, Ident.Name, RegResult, False);
    EmitInstruction(ACtx, EncodeABC(OP_TO_NUMERIC, RegResult, RegResult, 0));
    if not AExpr.IsPrefix then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, RegResult, 0));
    EmitInstruction(ACtx, EncodeABC(Op, RegResult, RegResult, 0));
    EmitWithAssignmentOrFallback(ACtx, Ident.Name, RegResult);
    if AExpr.IsPrefix then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, RegResult, 0));
    ACtx.Scope.FreeRegister;
    Exit;
  end;

  LocalIdx := ACtx.Scope.ResolveLocal(Ident.Name);
  if LocalIdx >= 0 then
  begin
    if ACtx.Scope.GetLocal(LocalIdx).IsConst then
    begin
      EmitConstAssignmentError(ACtx);
      Exit;
    end;
    if ACtx.Scope.GetLocal(LocalIdx).IsGlobalBacked then
    begin
      RegResult := ACtx.Scope.AllocateRegister;
      EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, RegResult,
        ACtx.Template.AddConstantString(Ident.Name)));
      EmitInstruction(ACtx, EncodeABC(OP_TO_NUMERIC, RegResult, RegResult, 0));
      if not AExpr.IsPrefix then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, RegResult, 0));
      EmitInstruction(ACtx, EncodeABC(Op, RegResult, RegResult, 0));
      EmitSetGlobalByName(ACtx, RegResult, Ident.Name);
      if AExpr.IsPrefix then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, RegResult, 0));
      ACtx.Scope.FreeRegister;
      Exit;
    end;
    Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
    EmitInstruction(ACtx, EncodeABC(OP_TO_NUMERIC, Slot, Slot, 0));
    if not AExpr.IsPrefix then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, Slot, 0));
    EmitInstruction(ACtx, EncodeABC(Op, Slot, Slot, 0));
    if ACtx.Scope.GetLocal(LocalIdx).IsCaptured then
      EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, Slot, UInt16(Slot)));
    if AExpr.IsPrefix then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, Slot, 0));
    Exit;
  end;

  UpvalIdx := ACtx.Scope.ResolveUpvalue(Ident.Name);
  if UpvalIdx >= 0 then
  begin
    if ACtx.Scope.GetUpvalue(UpvalIdx).IsConst then
    begin
      EmitConstAssignmentError(ACtx);
      Exit;
    end;
    RegResult := ACtx.Scope.AllocateRegister;
    if ACtx.Scope.GetUpvalue(UpvalIdx).IsGlobalBacked then
      EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, RegResult,
        ACtx.Template.AddConstantString(Ident.Name)))
    else
      EmitInstruction(ACtx, EncodeABx(OP_GET_UPVALUE, RegResult, UInt16(UpvalIdx)));
    EmitInstruction(ACtx, EncodeABC(OP_TO_NUMERIC, RegResult, RegResult, 0));
    if not AExpr.IsPrefix then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, RegResult, 0));
    EmitInstruction(ACtx, EncodeABC(Op, RegResult, RegResult, 0));
    if ACtx.Scope.GetUpvalue(UpvalIdx).IsGlobalBacked then
      EmitSetGlobalByName(ACtx, RegResult, Ident.Name)
    else
      EmitInstruction(ACtx, EncodeABx(OP_SET_UPVALUE, RegResult, UInt16(UpvalIdx)));
    if AExpr.IsPrefix then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, RegResult, 0));
    ACtx.Scope.FreeRegister;
    Exit;
  end;

  RegResult := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, RegResult,
    ACtx.Template.AddConstantString(Ident.Name)));
  EmitInstruction(ACtx, EncodeABC(OP_TO_NUMERIC, RegResult, RegResult, 0));
  if not AExpr.IsPrefix then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, RegResult, 0));
  EmitInstruction(ACtx, EncodeABC(Op, RegResult, RegResult, 0));
  EmitSetGlobalByName(ACtx, RegResult, Ident.Name);
  if AExpr.IsPrefix then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, RegResult, 0));
  ACtx.Scope.FreeRegister;
end;

procedure CompilePrivateMember(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaPrivateMemberExpression; const ADest: UInt8);
var
  ObjReg: UInt8;
begin
  ObjReg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AExpr.ObjectExpr, ObjReg);
  EmitLoadPropertyByName(ACtx, ADest, ObjReg,
    PrivateKey(ACtx.Scope, AExpr.PrivateName));
  ACtx.Scope.FreeRegister;
end;

procedure CompilePrivatePropertyAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaPrivatePropertyAssignmentExpression; const ADest: UInt8);
var
  ObjReg, ValReg: UInt8;
begin
  ObjReg := ACtx.Scope.AllocateRegister;
  ValReg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AExpr.ObjectExpr, ObjReg);
  ACtx.CompileExpression(AExpr.Value, ValReg);
  EmitStorePropertyByName(ACtx, ObjReg,
    PrivateKey(ACtx.Scope, AExpr.PrivateName), ValReg);
  if ADest <> ValReg then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, ValReg, 0));
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompilePrivatePropertyCompoundAssignment(
  const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaPrivatePropertyCompoundAssignmentExpression;
  const ADest: UInt8);
var
  ObjReg, CurReg, ValReg: UInt8;
  Op: TGocciaOpCode;
  PrivatePropKey: string;
  JumpIdx: Integer;
begin
  // ES2026 §13.15.2 AssignmentExpression : LeftHandSideExpression ??=/&&=/||= AssignmentExpression
  if IsShortCircuitAssignment(AExpr.Operator) then
  begin
    ObjReg := ACtx.Scope.AllocateRegister;
    CurReg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(AExpr.ObjectExpr, ObjReg);
    PrivatePropKey := PrivateKey(ACtx.Scope, AExpr.PrivateName);
    EmitLoadPropertyByName(ACtx, CurReg, ObjReg, PrivatePropKey);
    JumpIdx := EmitJumpInstruction(ACtx, ShortCircuitJumpOp(AExpr.Operator), CurReg);
    ACtx.CompileExpression(AExpr.Value, CurReg);
    EmitStorePropertyByName(ACtx, ObjReg, PrivatePropKey, CurReg);
    PatchJumpTarget(ACtx, JumpIdx);
    if ADest <> CurReg then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, CurReg, 0));
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    Exit;
  end;

  Op := CompoundOpToRuntimeOp(AExpr.Operator);
  ObjReg := ACtx.Scope.AllocateRegister;
  CurReg := ACtx.Scope.AllocateRegister;
  ValReg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AExpr.ObjectExpr, ObjReg);
  PrivatePropKey := PrivateKey(ACtx.Scope, AExpr.PrivateName);
  EmitLoadPropertyByName(ACtx, CurReg, ObjReg, PrivatePropKey);
  ACtx.CompileExpression(AExpr.Value, ValReg);
  EmitInstruction(ACtx, EncodeABC(Op, CurReg, CurReg, ValReg));
  EmitStorePropertyByName(ACtx, ObjReg, PrivatePropKey, CurReg);
  if ADest <> CurReg then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, CurReg, 0));
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileThis(const ACtx: TGocciaCompilationContext;
  const ADest: UInt8);
var
  LocalIdx, UpvalIdx: Integer;
  Slot: UInt8;
begin
  LocalIdx := ACtx.Scope.ResolveLocal(KEYWORD_THIS);
  if LocalIdx >= 0 then
  begin
    Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
    if Slot <> ADest then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, Slot, 0));
    Exit;
  end;

  UpvalIdx := ACtx.Scope.ResolveUpvalue(KEYWORD_THIS);
  if UpvalIdx >= 0 then
  begin
    EmitInstruction(ACtx, EncodeABx(OP_GET_UPVALUE, ADest, UInt16(UpvalIdx)));
    Exit;
  end;

  // ES2026 §13.2.1.1 / §9.4.3 ResolveThisBinding: when no enclosing
  // function or class establishes a `this` binding, fall back to the
  // surrounding environment's GetThisBinding.  For Scripts that yields
  // the realm's global object; for Modules it yields undefined.  The
  // OP_GET_THIS_BINDING handler reads FGlobalScope.ThisValue, which the
  // runtime sets up correctly for both kinds.
  EmitInstruction(ACtx, EncodeABC(OP_GET_THIS_BINDING, ADest, 0, 0));
end;

procedure CompileSuperAccess(const ACtx: TGocciaCompilationContext;
  const ADest: UInt8);
var
  LocalIdx, UpvalIdx: Integer;
  Slot: UInt8;
begin
  LocalIdx := ACtx.Scope.ResolveLocal('__super__');
  if LocalIdx >= 0 then
  begin
    Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
    if Slot <> ADest then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, Slot, 0));
    Exit;
  end;

  UpvalIdx := ACtx.Scope.ResolveUpvalue('__super__');
  if UpvalIdx >= 0 then
  begin
    EmitInstruction(ACtx, EncodeABx(OP_GET_UPVALUE, ADest, UInt16(UpvalIdx)));
    Exit;
  end;

  EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, ADest, 0, 0));
end;

// ES2026 §13.3.12 MetaProperty — import.meta
procedure CompileImportMeta(const ACtx: TGocciaCompilationContext;
  const ADest: UInt8);
begin
  EmitInstruction(ACtx, EncodeABC(OP_IMPORT_META, ADest, 0, 0));
end;

// ES2026 §13.3.12 MetaProperty — new.target
procedure CompileNewTarget(const ACtx: TGocciaCompilationContext;
  const ADest: UInt8);
begin
  EmitInstruction(ACtx, EncodeABC(OP_NEW_TARGET, ADest, 0, 0));
end;

// ES2026 §13.3.10 ImportCall — import(specifier)
procedure CompileDynamicImport(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaImportCallExpression; const ADest: UInt8);
var
  SpecReg: UInt8;
begin
  SpecReg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AExpr.Specifier, SpecReg);
  EmitInstruction(ACtx, EncodeABC(OP_DYNAMIC_IMPORT, ADest, SpecReg, 0));
  ACtx.Scope.FreeRegister;
end;

end.
