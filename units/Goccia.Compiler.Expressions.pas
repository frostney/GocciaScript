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
procedure CompileMethod(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaMethodExpression; const ADest: UInt8);
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

procedure EmitDefaultParameters(const ACtx: TGocciaCompilationContext;
  const AParams: TGocciaParameterArray);
procedure CollectDestructuringBindings(const APattern: TGocciaDestructuringPattern;
  const AScope: TGocciaCompilerScope; const AIsConst: Boolean = False);
procedure EmitDestructuring(const ACtx: TGocciaCompilationContext;
  const APattern: TGocciaDestructuringPattern; const ASrcReg: UInt8);
procedure CompileDestructuringAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaDestructuringAssignmentExpression; const ADest: UInt8);

implementation

uses
  Classes,
  Generics.Collections,
  Math,
  SysUtils,

  Goccia.Bytecode.Debug,
  Goccia.Compiler.ConstantFolding,
  Goccia.Compiler.Statements,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Keywords.Reserved,
  Goccia.Lexer,
  Goccia.Parser,
  Goccia.Token,
  Goccia.Values.Primitives;

function PrivateKey(const AScope: TGocciaCompilerScope;
  const AName: string): string;
var
  Prefix: string;
begin
  Prefix := AScope.ResolvePrivatePrefix;
  Result := '#' + Prefix + AName;
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

procedure CompileIdentifierAccess(const ACtx: TGocciaCompilationContext;
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
  else
    Result := False;
  end;
end;


function IsKnownNumeric(const AType: TGocciaLocalType): Boolean; inline;
begin
  Result := AType in [sltInteger, sltFloat];
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
  ObjReg, KeyReg: UInt8;
  PropIdx: UInt16;
begin
  if AExpr.Operand is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(AExpr.Operand);
    ObjReg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(MemberExpr.ObjectExpr, ObjReg);

    if MemberExpr.Computed then
    begin
      KeyReg := ACtx.Scope.AllocateRegister;
      ACtx.CompileExpression(MemberExpr.PropertyExpression, KeyReg);
      EmitInstruction(ACtx, EncodeABC(OP_DEL_INDEX, ADest, ObjReg, KeyReg));
      ACtx.Scope.FreeRegister;
    end
    else
    begin
      PropIdx := ACtx.Template.AddConstantString(MemberExpr.PropertyName);
      if PropIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: property name index exceeds 255');
      EmitInstruction(ACtx, EncodeABx(OP_DELETE_PROP_CONST, ObjReg, PropIdx));
      if ADest <> ObjReg then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, ObjReg, 0));
    end;
    ACtx.Scope.FreeRegister;
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
  else
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, ADest, 0, 0));
  end;

  ACtx.Scope.FreeRegister;
end;

procedure CompileAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaAssignmentExpression; const ADest: UInt8);
var
  LocalIdx, UpvalIdx: Integer;
  Local: TGocciaCompilerLocal;
  Slot: UInt8;
  MsgIdx: UInt16;
  Hint: TGocciaLocalType;
begin
  ACtx.CompileExpression(AExpr.Value, ADest);

  LocalIdx := ACtx.Scope.ResolveLocal(AExpr.Name);
  if LocalIdx >= 0 then
  begin
    Local := ACtx.Scope.GetLocal(LocalIdx);
    if Local.IsConst then
    begin
      MsgIdx := ACtx.Template.AddConstantString(
        'Assignment to constant variable.');
      if MsgIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: error message index exceeds 255');
      EmitInstruction(ACtx, EncodeABC(OP_THROW_TYPE_ERROR_CONST, 0, 0, UInt8(MsgIdx)));
      Exit;
    end;
    if Local.IsGlobalBacked then
    begin
      EmitInstruction(ACtx, EncodeABx(OP_SET_GLOBAL, ADest,
        ACtx.Template.AddConstantString(AExpr.Name)));
      Exit;
    end;
    Slot := Local.Slot;
    Hint := Local.TypeHint;
    if Local.IsStrictlyTyped and (Hint <> sltUntyped) then
      if not TypesAreCompatible(InferLocalType(AExpr.Value), Hint) then
        EmitInstruction(ACtx, EncodeABC(OP_CHECK_TYPE, ADest,
          UInt8(Ord(Hint)), 0));
    if ADest <> Slot then
    begin
      EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, ADest, Slot));
    end;
    Exit;
  end;

  UpvalIdx := ACtx.Scope.ResolveUpvalue(AExpr.Name);
  if UpvalIdx >= 0 then
  begin
    if ACtx.Scope.GetUpvalue(UpvalIdx).IsConst then
    begin
      MsgIdx := ACtx.Template.AddConstantString(
        'Assignment to constant variable.');
      if MsgIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: error message index exceeds 255');
      EmitInstruction(ACtx, EncodeABC(OP_THROW_TYPE_ERROR_CONST, 0, 0, UInt8(MsgIdx)));
      Exit;
    end;
    if ACtx.Scope.GetUpvalue(UpvalIdx).IsStrictlyTyped then
      if not TypesAreCompatible(InferLocalType(AExpr.Value), ACtx.Scope.GetUpvalue(UpvalIdx).TypeHint) then
        EmitInstruction(ACtx, EncodeABC(OP_CHECK_TYPE, ADest,
          UInt8(Ord(ACtx.Scope.GetUpvalue(UpvalIdx).TypeHint)), 0));
    EmitInstruction(ACtx, EncodeABx(OP_SET_UPVALUE, ADest, UInt16(UpvalIdx)));
    Exit;
  end;

  EmitInstruction(ACtx, EncodeABx(OP_SET_GLOBAL, ADest,
    ACtx.Template.AddConstantString(AExpr.Name)));
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

  KeyIdx := ACtx.Template.AddConstantString(AExpr.PropertyName);
  if KeyIdx > High(UInt8) then
    raise Exception.Create('Constant pool overflow: property name index exceeds 255');
  EmitInstruction(ACtx, EncodeABC(OP_SET_PROP_CONST, ObjReg, UInt8(KeyIdx), ValReg));

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
  ObjPat: TGocciaObjectDestructuringPattern;
  ArrPat: TGocciaArrayDestructuringPattern;
  AssignPat: TGocciaAssignmentDestructuringPattern;
  I: Integer;
begin
  if APattern is TGocciaIdentifierDestructuringPattern then
    AScope.DeclareLocal(TGocciaIdentifierDestructuringPattern(APattern).Name, AIsConst)
  else if APattern is TGocciaObjectDestructuringPattern then
  begin
    ObjPat := TGocciaObjectDestructuringPattern(APattern);
    for I := 0 to ObjPat.Properties.Count - 1 do
      CollectDestructuringBindings(ObjPat.Properties[I].Pattern, AScope, AIsConst);
  end
  else if APattern is TGocciaArrayDestructuringPattern then
  begin
    ArrPat := TGocciaArrayDestructuringPattern(APattern);
    for I := 0 to ArrPat.Elements.Count - 1 do
      if Assigned(ArrPat.Elements[I]) then
        CollectDestructuringBindings(ArrPat.Elements[I], AScope, AIsConst);
  end
  else if APattern is TGocciaAssignmentDestructuringPattern then
  begin
    AssignPat := TGocciaAssignmentDestructuringPattern(APattern);
    CollectDestructuringBindings(AssignPat.Left, AScope, AIsConst);
  end
  else if APattern is TGocciaRestDestructuringPattern then
    CollectDestructuringBindings(
      TGocciaRestDestructuringPattern(APattern).Argument, AScope, AIsConst);
end;

procedure EmitDestructuring(const ACtx: TGocciaCompilationContext;
  const APattern: TGocciaDestructuringPattern; const ASrcReg: UInt8);
var
  ObjPat: TGocciaObjectDestructuringPattern;
  ArrPat: TGocciaArrayDestructuringPattern;
  AssignPat: TGocciaAssignmentDestructuringPattern;
  IdentPat: TGocciaIdentifierDestructuringPattern;
  Prop: TGocciaDestructuringProperty;
  LocalIdx: Integer;
  DestSlot, IdxReg: UInt8;
  PropIdx: UInt16;
  JumpIdx, I: Integer;
begin
  if APattern is TGocciaIdentifierDestructuringPattern then
  begin
    IdentPat := TGocciaIdentifierDestructuringPattern(APattern);
    LocalIdx := ACtx.Scope.ResolveLocal(IdentPat.Name);
    if LocalIdx >= 0 then
    begin
      DestSlot := ACtx.Scope.GetLocal(LocalIdx).Slot;
      if DestSlot <> ASrcReg then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, DestSlot, ASrcReg, 0));
    end
    else
    begin
      LocalIdx := ACtx.Scope.ResolveUpvalue(IdentPat.Name);
      if LocalIdx >= 0 then
        EmitInstruction(ACtx, EncodeABx(OP_SET_UPVALUE, ASrcReg, UInt16(LocalIdx)))
      else
        EmitInstruction(ACtx, EncodeABx(OP_SET_GLOBAL, ASrcReg,
          ACtx.Template.AddConstantString(IdentPat.Name)));
    end;
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
          TGocciaRestDestructuringPattern(Prop.Pattern).Argument, DestSlot);
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
        PropIdx := ACtx.Template.AddConstantString(Prop.Key);
        if PropIdx > High(UInt8) then
          raise Exception.Create('Constant pool overflow: property name index exceeds 255');
        EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, DestSlot, ASrcReg,
          UInt8(PropIdx)));
      end;

      EmitDestructuring(ACtx, Prop.Pattern, DestSlot);
      ACtx.Scope.FreeRegister;
    end;
  end
  else if APattern is TGocciaArrayDestructuringPattern then
  begin
    EmitInstruction(ACtx, EncodeABC(OP_VALIDATE_VALUE, ASrcReg,
      VALIDATE_OP_REQUIRE_ITERABLE, 0));
    ArrPat := TGocciaArrayDestructuringPattern(APattern);
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
          TGocciaRestDestructuringPattern(ArrPat.Elements[I]).Argument, DestSlot);
        ACtx.Scope.FreeRegister;
        Break;
      end;

      DestSlot := ACtx.Scope.AllocateRegister;
      IdxReg := ACtx.Scope.AllocateRegister;
      EmitInstruction(ACtx, EncodeAsBx(OP_LOAD_INT, IdxReg, Int16(I)));
      EmitInstruction(ACtx, EncodeABC(OP_ARRAY_GET, DestSlot, ASrcReg, IdxReg));
      ACtx.Scope.FreeRegister;

      EmitDestructuring(ACtx, ArrPat.Elements[I], DestSlot);
      ACtx.Scope.FreeRegister;
    end;
  end
  else if APattern is TGocciaAssignmentDestructuringPattern then
  begin
    AssignPat := TGocciaAssignmentDestructuringPattern(APattern);
    EmitUndefinedCheck(ACtx, ASrcReg, JumpIdx);
    ACtx.CompileExpression(AssignPat.Right, ASrcReg);
    PatchJumpTarget(ACtx, JumpIdx);
    EmitDestructuring(ACtx, AssignPat.Left, ASrcReg);
  end;
end;

procedure CompileDestructuringAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaDestructuringAssignmentExpression; const ADest: UInt8);
var
  SrcReg: UInt8;
begin
  SrcReg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AExpr.Right, SrcReg);
  EmitDestructuring(ACtx, AExpr.Left, SrcReg);
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
  const AParameters: TGocciaParameterArray);
var
  I, LocalIdx: Integer;
  AnnotationType: TGocciaLocalType;
  ParamName: string;
begin
  for I := 0 to High(AParameters) do
  begin
    if AParameters[I].TypeAnnotation = '' then
      Continue;
    if AParameters[I].IsRest then
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
    AScope.SetLocalTypeHint(LocalIdx, AnnotationType);
    AScope.SetLocalStrictlyTyped(LocalIdx, True);
    ATemplate.SetLocalType(AScope.GetLocal(LocalIdx).Slot, AnnotationType);
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
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);

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

  ApplyParameterTypeAnnotations(ChildScope, ChildTemplate, AExpr.Parameters);

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

procedure CompileSpreadArgsArray(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaCallExpression; const AArrayReg: UInt8);
var
  I: Integer;
  ElemReg: UInt8;
begin
  EmitInstruction(ACtx, EncodeABC(OP_NEW_ARRAY, AArrayReg, 0, 0));
  for I := 0 to AExpr.Arguments.Count - 1 do
  begin
    ElemReg := ACtx.Scope.AllocateRegister;
    if AExpr.Arguments[I] is TGocciaSpreadExpression then
    begin
      ACtx.CompileExpression(
        TGocciaSpreadExpression(AExpr.Arguments[I]).Argument, ElemReg);
      EmitInstruction(ACtx, EncodeABC(OP_COLLECTION_OP, AArrayReg,
        COLLECTION_OP_SPREAD_ITERABLE_INTO_ARRAY, ElemReg));
    end
    else
    begin
      ACtx.CompileExpression(AExpr.Arguments[I], ElemReg);
      EmitInstruction(ACtx, EncodeABC(OP_ARRAY_PUSH, AArrayReg, ElemReg, 0));
    end;
    ACtx.Scope.FreeRegister;
  end;
end;

procedure CompileCall(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaCallExpression; const ADest: UInt8);
var
  ArgCount, I: Integer;
  BaseReg, ObjReg, ArgsReg, SuperReg: UInt8;
  MemberExpr: TGocciaMemberExpression;
  PropIdx: UInt16;
  UseSpread: Boolean;
  NilJump, EndJump: Integer;
begin
  ArgCount := AExpr.Arguments.Count;
  if ArgCount > High(UInt8) then
    raise Exception.Create('Compiler error: too many arguments (>255)');
  UseSpread := HasSpreadArgument(AExpr);

  if AExpr.Callee is TGocciaSuperExpression then
  begin
    ObjReg := ACtx.Scope.AllocateRegister;
    CompileThis(ACtx, ObjReg);
    BaseReg := ACtx.Scope.AllocateRegister;
    SuperReg := ACtx.Scope.AllocateRegister;
    CompileSuperAccess(ACtx, SuperReg);
    PropIdx := ACtx.Template.AddConstantString(PROP_CONSTRUCTOR);
    if PropIdx > High(UInt8) then
      raise Exception.Create('Constant pool overflow');
    if SuperReg <> BaseReg + 1 then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, BaseReg + 1, SuperReg, 0));
    EmitInstruction(ACtx, EncodeABC(OP_SUPER_GET_CONST, BaseReg, 0, UInt8(PropIdx)));
    ACtx.Scope.FreeRegister;

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
    if PropIdx > High(UInt8) then
      raise Exception.Create('Constant pool overflow: private method name index exceeds 255');
    EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, BaseReg, ObjReg, UInt8(PropIdx)));

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
      PropIdx := ACtx.Template.AddConstantString(MemberExpr.PropertyName);
      if PropIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow');
      if SuperReg <> BaseReg + 1 then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, BaseReg + 1, SuperReg, 0));
      EmitInstruction(ACtx, EncodeABC(OP_SUPER_GET_CONST, BaseReg, 0, UInt8(PropIdx)));
      ACtx.Scope.FreeRegister;

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
      begin
        PropIdx := ACtx.Template.AddConstantString(MemberExpr.PropertyName);
        if PropIdx > High(UInt8) then
          raise Exception.Create('Constant pool overflow: property name index exceeds 255');
        EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, BaseReg, ObjReg, UInt8(PropIdx)));
      end;

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

      if MemberExpr.Optional then
      begin
        EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
        PatchJumpTarget(ACtx, NilJump);
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
    if (ADest + 1 = ACtx.Scope.NextSlot) and
       not IsLocalSlot(ACtx.Scope, ADest) then
      BaseReg := ADest
    else
      BaseReg := ACtx.Scope.AllocateRegister;

    ACtx.CompileExpression(AExpr.Callee, BaseReg);

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
  ObjReg, IdxReg: UInt8;
  PropIdx: UInt16;
  NilJump, EndJump: Integer;
begin
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
    begin
      PropIdx := ACtx.Template.AddConstantString(AExpr.PropertyName);
      if PropIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: property name index exceeds 255');
      EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, ADest, ObjReg, UInt8(PropIdx)));
    end;

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
    begin
      PropIdx := ACtx.Template.AddConstantString(AExpr.PropertyName);
      if PropIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: property name index exceeds 255');
      EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, ADest, ObjReg, UInt8(PropIdx)));
    end;
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
  const AExpr: TGocciaObjectExpression; const ADest: UInt8;
  const AKey: string; const AValExpr: TGocciaExpression);
var
  ValReg: UInt8;
  KeyIdx: UInt16;
begin
  ValReg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AValExpr, ValReg);
  KeyIdx := ACtx.Template.AddConstantString(AKey);
  if KeyIdx > High(UInt8) then
    raise Exception.Create('Constant pool overflow: property name index exceeds 255');
  EmitInstruction(ACtx, EncodeABC(OP_SET_PROP_CONST, ADest, KeyIdx, ValReg));
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
  FuncIdx: UInt16;
  GetterReg: UInt8;
  KeyIdx: UInt16;
  I: Integer;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TGocciaFunctionTemplate.Create('<get ' + AKey + '>');
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);
  ChildScope.DeclareLocal(KEYWORD_THIS, False);
  ChildTemplate.ParameterCount := 0;

  ACtx.SwapState(ChildTemplate, ChildScope);
  try
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
  GetterReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, GetterReg, FuncIdx));
  KeyIdx := ACtx.Template.AddConstantString(AKey);
  if KeyIdx > High(UInt8) then
    raise Exception.Create('Constant pool overflow: property name index exceeds 255');
  if GetterReg <> ADest + 1 then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest + 1, GetterReg, 0));
  EmitInstruction(ACtx, EncodeABC(OP_DEFINE_ACCESSOR_CONST, ADest, 0, UInt8(KeyIdx)));
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
  FuncIdx: UInt16;
  SetterReg: UInt8;
  KeyIdx: UInt16;
  I: Integer;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TGocciaFunctionTemplate.Create('<set ' + AKey + '>');
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);
  ChildScope.DeclareLocal(KEYWORD_THIS, False);
  ChildScope.DeclareLocal(ASetter.Parameter, False);
  ChildTemplate.ParameterCount := 1;

  ACtx.SwapState(ChildTemplate, ChildScope);
  try
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
  SetterReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, SetterReg, FuncIdx));
  KeyIdx := ACtx.Template.AddConstantString(AKey);
  if KeyIdx > High(UInt8) then
    raise Exception.Create('Constant pool overflow: property name index exceeds 255');
  if SetterReg <> ADest + 1 then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest + 1, SetterReg, 0));
  EmitInstruction(ACtx, EncodeABC(OP_DEFINE_ACCESSOR_CONST, ADest, ACCESSOR_FLAG_SETTER, UInt8(KeyIdx)));
  ACtx.Scope.FreeRegister;
end;

procedure CompileObject(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaObjectExpression; const ADest: UInt8);
var
  I: Integer;
  Key: string;
  ValExpr: TGocciaExpression;
  KeyReg, ValReg: UInt8;
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
      Key := Order[I].StaticKey;
      case Order[I].PropertyType of
        pstStatic:
          if AExpr.Properties.TryGetValue(Key, ValExpr) then
            CompileObjectProperty(ACtx, AExpr, ADest, Key, ValExpr);
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
              ACtx.CompileExpression(Pair.Value, ValReg);
              EmitInstruction(ACtx, EncodeABC(OP_SET_INDEX, ADest, KeyReg, ValReg));
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
        CompileObjectProperty(ACtx, AExpr, ADest, Key, ValExpr);
    end;
  end;
end;

procedure CompileTemplateLiteral(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaTemplateLiteralExpression; const ADest: UInt8);
var
  Template: string;
  I, Start, BraceCount: Integer;
  ExprText, StaticPart: string;
  PartReg: UInt8;
  HasParts: Boolean;
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  Tokens: TObjectList<TGocciaToken>;
  SourceLines: TStringList;
  ParsedExpr: TGocciaExpression;
begin
  Template := AExpr.Value;

  if Pos('${', Template) = 0 then
  begin
    EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, ADest,
      ACtx.Template.AddConstantString(Template)));
    Exit;
  end;

  HasParts := False;
  I := 1;
  while I <= Length(Template) do
  begin
    if (I < Length(Template)) and (Template[I] = '$') and (Template[I + 1] = '{') then
    begin
      Inc(I, 2);
      Start := I;
      BraceCount := 1;
      while (I <= Length(Template)) and (BraceCount > 0) do
      begin
        if Template[I] = '{' then
          Inc(BraceCount)
        else if Template[I] = '}' then
          Dec(BraceCount);
        if BraceCount > 0 then
          Inc(I);
      end;

      ExprText := Trim(Copy(Template, Start, I - Start));
      Inc(I);

      if ExprText <> '' then
      begin
        Lexer := TGocciaLexer.Create('(' + ExprText + ');', ACtx.SourcePath);
        try
          Tokens := Lexer.ScanTokens;
          SourceLines := TStringList.Create;
          try
            SourceLines.Text := ExprText;
            Parser := TGocciaParser.Create(Tokens, ACtx.SourcePath, SourceLines);
            try
              ProgramNode := Parser.ParseUnchecked;
              try
                ParsedExpr := nil;
                if (ProgramNode.Body.Count > 0) and
                   (ProgramNode.Body[0] is TGocciaExpressionStatement) then
                  ParsedExpr := TGocciaExpressionStatement(
                    ProgramNode.Body[0]).Expression;

                if Assigned(ParsedExpr) then
                begin
                  if not HasParts then
                  begin
                    ACtx.CompileExpression(ParsedExpr, ADest);
                    EmitInstruction(ACtx, EncodeABC(OP_TO_STRING, ADest, ADest, 0));
                    HasParts := True;
                  end
                  else
                  begin
                    PartReg := ACtx.Scope.AllocateRegister;
                    ACtx.CompileExpression(ParsedExpr, PartReg);
                    EmitInstruction(ACtx, EncodeABC(OP_TO_STRING, PartReg, PartReg, 0));
                    EmitInstruction(ACtx, EncodeABC(OP_ADD, ADest, ADest, PartReg));
                    ACtx.Scope.FreeRegister;
                  end;
                end;
              finally
                ProgramNode.Free;
              end;
            finally
              Parser.Free;
            end;
          finally
            SourceLines.Free;
          end;
        finally
          Lexer.Free;
        end;
      end;
    end
    else
    begin
      Start := I;
      while (I <= Length(Template)) and
        not ((I < Length(Template)) and (Template[I] = '$') and (Template[I + 1] = '{')) do
        Inc(I);

      StaticPart := Copy(Template, Start, I - Start);
      if StaticPart <> '' then
      begin
        if not HasParts then
        begin
          EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, ADest,
            ACtx.Template.AddConstantString(StaticPart)));
          HasParts := True;
        end
        else
        begin
          PartReg := ACtx.Scope.AllocateRegister;
          EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, PartReg,
            ACtx.Template.AddConstantString(StaticPart)));
          EmitInstruction(ACtx, EncodeABC(OP_ADD, ADest, ADest, PartReg));
          ACtx.Scope.FreeRegister;
        end;
      end;
    end;
  end;

  if not HasParts then
    EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, ADest,
      ACtx.Template.AddConstantString('')));
end;

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
    EmitInstruction(ACtx, EncodeABC(OP_ADD, ADest, ADest, PartReg));
    ACtx.Scope.FreeRegister;
  end;
end;

procedure CompileNewExpression(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaNewExpression; const ADest: UInt8);
var
  CtorReg: UInt8;
  NameIdx: UInt16;
  ArgCount, I: Integer;
  Local: TGocciaCompilerLocal;
begin
  CtorReg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AExpr.Callee, CtorReg);

  ArgCount := AExpr.Arguments.Count;
  if ArgCount > High(UInt8) then
    raise Exception.Create('Compiler error: too many constructor arguments (>255)');
  for I := 0 to ArgCount - 1 do
    ACtx.CompileExpression(AExpr.Arguments[I], ACtx.Scope.AllocateRegister);

  EmitInstruction(ACtx, EncodeABC(OP_CONSTRUCT, ADest, CtorReg, UInt8(ArgCount)));

  for I := 0 to ArgCount - 1 do
    ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;

  for I := 0 to ACtx.Scope.LocalCount - 1 do
  begin
    Local := ACtx.Scope.GetLocal(I);
    if Local.IsGlobalBacked then
    begin
      NameIdx := ACtx.Template.AddConstantString(Local.Name);
      if NameIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: global name index exceeds 255');
      EmitInstruction(ACtx, EncodeABC(OP_DEFINE_GLOBAL_CONST, Local.Slot,
        0, UInt8(NameIdx)));
    end;
  end;
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

  EmitInstruction(ACtx, EncodeABC(OP_ARRAY_SET, ObjReg, KeyReg, ValReg));

  if ADest <> ValReg then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, ValReg, 0));

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileMethod(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaMethodExpression; const ADest: UInt8);
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

  ChildTemplate := TGocciaFunctionTemplate.Create('<method>');
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.IsAsync := AExpr.IsAsync;
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

  ApplyParameterTypeAnnotations(ChildScope, ChildTemplate, AExpr.Parameters);

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

procedure CompileCompoundAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaCompoundAssignmentExpression; const ADest: UInt8);
var
  LocalIdx, UpvalIdx: Integer;
  Slot, RegVal, RegResult, CondReg, ArgReg: UInt8;
  MsgIdx, NameIdx: UInt16;
  Op, FloatOp: TGocciaOpCode;
  LocalType, ValType: TGocciaLocalType;
  ArithOp: TGocciaTokenType;
  JumpIdx, OkJump: Integer;
begin
  // ES2026 §13.15.2 AssignmentExpression : LeftHandSideExpression ??= AssignmentExpression
  if AExpr.Operator = gttNullishCoalescingAssign then
  begin
    LocalIdx := ACtx.Scope.ResolveLocal(AExpr.Name);
    if LocalIdx >= 0 then
    begin
      if ACtx.Scope.GetLocal(LocalIdx).IsGlobalBacked then
      begin
        EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, ADest,
          ACtx.Template.AddConstantString(AExpr.Name)));
        JumpIdx := EmitJumpInstruction(ACtx, OP_JUMP_IF_NOT_NULLISH, ADest);
        if ACtx.Scope.GetLocal(LocalIdx).IsConst then
        begin
          MsgIdx := ACtx.Template.AddConstantString(
            'Assignment to constant variable.');
          if MsgIdx > High(UInt8) then
            raise Exception.Create('Constant pool overflow: error message index exceeds 255');
          EmitInstruction(ACtx, EncodeABC(OP_THROW_TYPE_ERROR_CONST, 0, 0, UInt8(MsgIdx)));
        end
        else
        begin
          ACtx.CompileExpression(AExpr.Value, ADest);
          EmitInstruction(ACtx, EncodeABx(OP_SET_GLOBAL, ADest,
            ACtx.Template.AddConstantString(AExpr.Name)));
        end;
        PatchJumpTarget(ACtx, JumpIdx);
        Exit;
      end;

      Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
      if ADest <> Slot then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, Slot, 0));
      JumpIdx := EmitJumpInstruction(ACtx, OP_JUMP_IF_NOT_NULLISH, ADest);
      if ACtx.Scope.GetLocal(LocalIdx).IsConst then
      begin
        MsgIdx := ACtx.Template.AddConstantString(
          'Assignment to constant variable.');
        if MsgIdx > High(UInt8) then
          raise Exception.Create('Constant pool overflow: error message index exceeds 255');
        EmitInstruction(ACtx, EncodeABC(OP_THROW_TYPE_ERROR_CONST, 0, 0, UInt8(MsgIdx)));
      end
      else
      begin
        ACtx.CompileExpression(AExpr.Value, ADest);
        LocalType := ACtx.Scope.GetLocal(LocalIdx).TypeHint;
        if ACtx.Scope.GetLocal(LocalIdx).IsStrictlyTyped and
           (LocalType <> sltUntyped) and
           not TypesAreCompatible(InferLocalType(AExpr.Value), LocalType) then
          EmitInstruction(ACtx, EncodeABC(OP_CHECK_TYPE, ADest,
            UInt8(Ord(LocalType)), 0));
        if ADest <> Slot then
          EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, ADest, Slot));
      end;
      PatchJumpTarget(ACtx, JumpIdx);
      Exit;
    end;

    UpvalIdx := ACtx.Scope.ResolveUpvalue(AExpr.Name);
    if UpvalIdx >= 0 then
    begin
      EmitInstruction(ACtx, EncodeABx(OP_GET_UPVALUE, ADest, UInt16(UpvalIdx)));
      JumpIdx := EmitJumpInstruction(ACtx, OP_JUMP_IF_NOT_NULLISH, ADest);
      if ACtx.Scope.GetUpvalue(UpvalIdx).IsConst then
      begin
        MsgIdx := ACtx.Template.AddConstantString(
          'Assignment to constant variable.');
        if MsgIdx > High(UInt8) then
          raise Exception.Create('Constant pool overflow: error message index exceeds 255');
        EmitInstruction(ACtx, EncodeABC(OP_THROW_TYPE_ERROR_CONST, 0, 0, UInt8(MsgIdx)));
      end
      else
      begin
        ACtx.CompileExpression(AExpr.Value, ADest);
        if ACtx.Scope.GetUpvalue(UpvalIdx).IsStrictlyTyped and
           not TypesAreCompatible(InferLocalType(AExpr.Value),
             ACtx.Scope.GetUpvalue(UpvalIdx).TypeHint) then
          EmitInstruction(ACtx, EncodeABC(OP_CHECK_TYPE, ADest,
            UInt8(Ord(ACtx.Scope.GetUpvalue(UpvalIdx).TypeHint)), 0));
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
    JumpIdx := EmitJumpInstruction(ACtx, OP_JUMP_IF_NOT_NULLISH, ADest);
    ACtx.CompileExpression(AExpr.Value, ADest);
    EmitInstruction(ACtx, EncodeABx(OP_SET_GLOBAL, ADest, NameIdx));
    PatchJumpTarget(ACtx, JumpIdx);
    Exit;
  end;

  Op := CompoundOpToRuntimeOp(AExpr.Operator);

  LocalIdx := ACtx.Scope.ResolveLocal(AExpr.Name);
  if LocalIdx >= 0 then
  begin
    if ACtx.Scope.GetLocal(LocalIdx).IsConst then
    begin
      MsgIdx := ACtx.Template.AddConstantString(
        'Assignment to constant variable.');
      if MsgIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: error message index exceeds 255');
      EmitInstruction(ACtx, EncodeABC(OP_THROW_TYPE_ERROR_CONST, 0, 0, UInt8(MsgIdx)));
      Exit;
    end;
    if ACtx.Scope.GetLocal(LocalIdx).IsGlobalBacked then
    begin
      RegVal := ACtx.Scope.AllocateRegister;
      RegResult := ACtx.Scope.AllocateRegister;
      EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, RegResult,
        ACtx.Template.AddConstantString(AExpr.Name)));
      ACtx.CompileExpression(AExpr.Value, RegVal);
      EmitInstruction(ACtx, EncodeABC(Op, ADest, RegResult, RegVal));
      EmitInstruction(ACtx, EncodeABx(OP_SET_GLOBAL, ADest,
        ACtx.Template.AddConstantString(AExpr.Name)));
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
      Exit;
    end;
    Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
    RegVal := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(AExpr.Value, RegVal);

    LocalType := ACtx.Scope.GetLocal(LocalIdx).TypeHint;
    ValType := ExpressionType(ACtx.Scope, AExpr.Value);
    if (LocalType in [sltInteger, sltFloat]) and
       (ValType in [sltInteger, sltFloat]) and
       IsArithmeticCompoundAssign(AExpr.Operator, ArithOp) and
       TryFloatOp(ArithOp, FloatOp) then
      EmitInstruction(ACtx, EncodeABC(FloatOp, Slot, Slot, RegVal))
    else
      EmitInstruction(ACtx, EncodeABC(Op, Slot, Slot, RegVal));
    if ADest <> Slot then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, Slot, 0));
    ACtx.Scope.FreeRegister;
    Exit;
  end;

  UpvalIdx := ACtx.Scope.ResolveUpvalue(AExpr.Name);
  if UpvalIdx >= 0 then
  begin
    RegVal := ACtx.Scope.AllocateRegister;
    RegResult := ACtx.Scope.AllocateRegister;
    EmitInstruction(ACtx, EncodeABx(OP_GET_UPVALUE, RegResult, UInt16(UpvalIdx)));
    ACtx.CompileExpression(AExpr.Value, RegVal);
    EmitInstruction(ACtx, EncodeABC(Op, RegResult, RegResult, RegVal));
    EmitInstruction(ACtx, EncodeABx(OP_SET_UPVALUE, RegResult, UInt16(UpvalIdx)));
    if ADest <> RegResult then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, RegResult, 0));
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
  EmitInstruction(ACtx, EncodeABx(OP_SET_GLOBAL, ADest,
    ACtx.Template.AddConstantString(AExpr.Name)));
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
  // ES2026 §13.15.2 AssignmentExpression : LeftHandSideExpression ??= AssignmentExpression
  if AExpr.Operator = gttNullishCoalescingAssign then
  begin
    ObjReg := ACtx.Scope.AllocateRegister;
    CurReg := ACtx.Scope.AllocateRegister;

    ACtx.CompileExpression(AExpr.ObjectExpr, ObjReg);
    PropIdx := ACtx.Template.AddConstantString(AExpr.PropertyName);
    if PropIdx > High(UInt8) then
      raise Exception.Create('Constant pool overflow: property name index exceeds 255');
    EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, CurReg, ObjReg,
      UInt8(PropIdx)));
    JumpIdx := EmitJumpInstruction(ACtx, OP_JUMP_IF_NOT_NULLISH, CurReg);
    ACtx.CompileExpression(AExpr.Value, CurReg);
    EmitInstruction(ACtx, EncodeABC(OP_SET_PROP_CONST, ObjReg, UInt8(PropIdx),
      CurReg));
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
  PropIdx := ACtx.Template.AddConstantString(AExpr.PropertyName);
  if PropIdx > High(UInt8) then
    raise Exception.Create('Constant pool overflow: property name index exceeds 255');
  EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, CurReg, ObjReg,
    UInt8(PropIdx)));
  ACtx.CompileExpression(AExpr.Value, ValReg);
  EmitInstruction(ACtx, EncodeABC(Op, CurReg, CurReg, ValReg));
  EmitInstruction(ACtx, EncodeABC(OP_SET_PROP_CONST, ObjReg, UInt8(PropIdx),
    CurReg));

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
  // ES2026 §13.15.2 AssignmentExpression : LeftHandSideExpression ??= AssignmentExpression
  if AExpr.Operator = gttNullishCoalescingAssign then
  begin
    ObjReg := ACtx.Scope.AllocateRegister;
    KeyReg := ACtx.Scope.AllocateRegister;
    CurReg := ACtx.Scope.AllocateRegister;

    ACtx.CompileExpression(AExpr.ObjectExpr, ObjReg);
    ACtx.CompileExpression(AExpr.PropertyExpression, KeyReg);
    EmitInstruction(ACtx, EncodeABC(OP_ARRAY_GET, CurReg, ObjReg, KeyReg));
    JumpIdx := EmitJumpInstruction(ACtx, OP_JUMP_IF_NOT_NULLISH, CurReg);
    ACtx.CompileExpression(AExpr.Value, CurReg);
    EmitInstruction(ACtx, EncodeABC(OP_ARRAY_SET, ObjReg, KeyReg, CurReg));
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
  EmitInstruction(ACtx, EncodeABC(OP_ARRAY_SET, ObjReg, KeyReg, CurReg));

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
  ObjReg, CurReg, RegOne: UInt8;
  PropIdx: UInt16;
begin
  ObjReg := ACtx.Scope.AllocateRegister;
  CurReg := ACtx.Scope.AllocateRegister;
  RegOne := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(AMember.ObjectExpr, ObjReg);
  PropIdx := ACtx.Template.AddConstantString(AMember.PropertyName);
  if PropIdx > High(UInt8) then
    raise Exception.Create('Constant pool overflow: property name index exceeds 255');
  EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, CurReg, ObjReg, UInt8(PropIdx)));
  EmitInstruction(ACtx, EncodeABC(OP_TO_NUMBER, CurReg, CurReg, 0));
  EmitInstruction(ACtx, EncodeAsBx(OP_LOAD_INT, RegOne, 1));
  if not AExpr.IsPrefix then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, CurReg, 0));
  EmitInstruction(ACtx, EncodeABC(AOp, CurReg, CurReg, RegOne));
  EmitInstruction(ACtx, EncodeABC(OP_SET_PROP_CONST, ObjReg, UInt8(PropIdx), CurReg));
  if AExpr.IsPrefix then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, CurReg, 0));

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileIncrementComputedMember(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIncrementExpression; const AMember: TGocciaMemberExpression;
  const ADest: UInt8; const AOp: TGocciaOpCode);
var
  ObjReg, KeyReg, CurReg, RegOne: UInt8;
begin
  ObjReg := ACtx.Scope.AllocateRegister;
  KeyReg := ACtx.Scope.AllocateRegister;
  CurReg := ACtx.Scope.AllocateRegister;
  RegOne := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(AMember.ObjectExpr, ObjReg);
  ACtx.CompileExpression(AMember.PropertyExpression, KeyReg);
  EmitInstruction(ACtx, EncodeABC(OP_ARRAY_GET, CurReg, ObjReg, KeyReg));
  EmitInstruction(ACtx, EncodeABC(OP_TO_NUMBER, CurReg, CurReg, 0));
  EmitInstruction(ACtx, EncodeAsBx(OP_LOAD_INT, RegOne, 1));
  if not AExpr.IsPrefix then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, CurReg, 0));
  EmitInstruction(ACtx, EncodeABC(AOp, CurReg, CurReg, RegOne));
  EmitInstruction(ACtx, EncodeABC(OP_ARRAY_SET, ObjReg, KeyReg, CurReg));
  if AExpr.IsPrefix then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, CurReg, 0));

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileIncrement(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIncrementExpression; const ADest: UInt8);
var
  LocalIdx, UpvalIdx: Integer;
  Slot, RegOne, RegResult: UInt8;
  MsgIdx: UInt16;
  Op: TGocciaOpCode;
  Ident: TGocciaIdentifierExpression;
  MemberExpr: TGocciaMemberExpression;
begin
  if AExpr.Operator = gttIncrement then
    Op := OP_ADD
  else
    Op := OP_SUB;

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

  LocalIdx := ACtx.Scope.ResolveLocal(Ident.Name);
  if LocalIdx >= 0 then
  begin
    if ACtx.Scope.GetLocal(LocalIdx).IsConst then
    begin
      MsgIdx := ACtx.Template.AddConstantString(
        'Assignment to constant variable.');
      if MsgIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: error message index exceeds 255');
      EmitInstruction(ACtx, EncodeABC(OP_THROW_TYPE_ERROR_CONST, 0, 0, UInt8(MsgIdx)));
      Exit;
    end;
    if ACtx.Scope.GetLocal(LocalIdx).IsGlobalBacked then
    begin
      RegResult := ACtx.Scope.AllocateRegister;
      RegOne := ACtx.Scope.AllocateRegister;
      EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, RegResult,
        ACtx.Template.AddConstantString(Ident.Name)));
      EmitInstruction(ACtx, EncodeABC(OP_TO_NUMBER, RegResult, RegResult, 0));
      EmitInstruction(ACtx, EncodeAsBx(OP_LOAD_INT, RegOne, 1));
      if not AExpr.IsPrefix then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, RegResult, 0));
      EmitInstruction(ACtx, EncodeABC(Op, RegResult, RegResult, RegOne));
      EmitInstruction(ACtx, EncodeABx(OP_SET_GLOBAL, RegResult,
        ACtx.Template.AddConstantString(Ident.Name)));
      if AExpr.IsPrefix then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, RegResult, 0));
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
      Exit;
    end;
    Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
    EmitInstruction(ACtx, EncodeABC(OP_TO_NUMBER, Slot, Slot, 0));
    RegOne := ACtx.Scope.AllocateRegister;
    EmitInstruction(ACtx, EncodeAsBx(OP_LOAD_INT, RegOne, 1));
    if not AExpr.IsPrefix then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, Slot, 0));
    EmitInstruction(ACtx, EncodeABC(Op, Slot, Slot, RegOne));
    if AExpr.IsPrefix then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, Slot, 0));
    ACtx.Scope.FreeRegister;
    Exit;
  end;

  UpvalIdx := ACtx.Scope.ResolveUpvalue(Ident.Name);
  if UpvalIdx >= 0 then
  begin
    RegResult := ACtx.Scope.AllocateRegister;
    RegOne := ACtx.Scope.AllocateRegister;
    EmitInstruction(ACtx, EncodeABx(OP_GET_UPVALUE, RegResult, UInt16(UpvalIdx)));
    EmitInstruction(ACtx, EncodeABC(OP_TO_NUMBER, RegResult, RegResult, 0));
    EmitInstruction(ACtx, EncodeAsBx(OP_LOAD_INT, RegOne, 1));
    if not AExpr.IsPrefix then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, RegResult, 0));
    EmitInstruction(ACtx, EncodeABC(Op, RegResult, RegResult, RegOne));
    EmitInstruction(ACtx, EncodeABx(OP_SET_UPVALUE, RegResult, UInt16(UpvalIdx)));
    if AExpr.IsPrefix then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, RegResult, 0));
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    Exit;
  end;

  RegResult := ACtx.Scope.AllocateRegister;
  RegOne := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, RegResult,
    ACtx.Template.AddConstantString(Ident.Name)));
  EmitInstruction(ACtx, EncodeABC(OP_TO_NUMBER, RegResult, RegResult, 0));
  EmitInstruction(ACtx, EncodeAsBx(OP_LOAD_INT, RegOne, 1));
  if not AExpr.IsPrefix then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, RegResult, 0));
  EmitInstruction(ACtx, EncodeABC(Op, RegResult, RegResult, RegOne));
  EmitInstruction(ACtx, EncodeABx(OP_SET_GLOBAL, RegResult,
    ACtx.Template.AddConstantString(Ident.Name)));
  if AExpr.IsPrefix then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, RegResult, 0));
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompilePrivateMember(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaPrivateMemberExpression; const ADest: UInt8);
var
  ObjReg: UInt8;
  PropIdx: UInt16;
begin
  ObjReg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AExpr.ObjectExpr, ObjReg);
  PropIdx := ACtx.Template.AddConstantString(
    PrivateKey(ACtx.Scope, AExpr.PrivateName));
  if PropIdx > High(UInt8) then
    raise Exception.Create('Constant pool overflow: private property name index exceeds 255');
  EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, ADest, ObjReg, UInt8(PropIdx)));
  ACtx.Scope.FreeRegister;
end;

procedure CompilePrivatePropertyAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaPrivatePropertyAssignmentExpression; const ADest: UInt8);
var
  ObjReg, ValReg: UInt8;
  KeyIdx: UInt16;
begin
  ObjReg := ACtx.Scope.AllocateRegister;
  ValReg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AExpr.ObjectExpr, ObjReg);
  ACtx.CompileExpression(AExpr.Value, ValReg);
  KeyIdx := ACtx.Template.AddConstantString(
    PrivateKey(ACtx.Scope, AExpr.PrivateName));
  if KeyIdx > High(UInt8) then
    raise Exception.Create('Constant pool overflow: private property name index exceeds 255');
  EmitInstruction(ACtx, EncodeABC(OP_SET_PROP_CONST, ObjReg, UInt8(KeyIdx), ValReg));
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
  PropIdx: UInt16;
  JumpIdx: Integer;
begin
  // ES2026 §13.15.2 AssignmentExpression : LeftHandSideExpression ??= AssignmentExpression
  if AExpr.Operator = gttNullishCoalescingAssign then
  begin
    ObjReg := ACtx.Scope.AllocateRegister;
    CurReg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(AExpr.ObjectExpr, ObjReg);
    PropIdx := ACtx.Template.AddConstantString(
      PrivateKey(ACtx.Scope, AExpr.PrivateName));
    if PropIdx > High(UInt8) then
      raise Exception.Create('Constant pool overflow: private property name index exceeds 255');
    EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, CurReg, ObjReg, UInt8(PropIdx)));
    JumpIdx := EmitJumpInstruction(ACtx, OP_JUMP_IF_NOT_NULLISH, CurReg);
    ACtx.CompileExpression(AExpr.Value, CurReg);
    EmitInstruction(ACtx, EncodeABC(OP_SET_PROP_CONST, ObjReg, UInt8(PropIdx), CurReg));
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
  PropIdx := ACtx.Template.AddConstantString(
    PrivateKey(ACtx.Scope, AExpr.PrivateName));
  if PropIdx > High(UInt8) then
    raise Exception.Create('Constant pool overflow: private property name index exceeds 255');
  EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, CurReg, ObjReg, UInt8(PropIdx)));
  ACtx.CompileExpression(AExpr.Value, ValReg);
  EmitInstruction(ACtx, EncodeABC(Op, CurReg, CurReg, ValReg));
  EmitInstruction(ACtx, EncodeABC(OP_SET_PROP_CONST, ObjReg, UInt8(PropIdx), CurReg));
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

  EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, ADest, 0, 0));
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

end.
