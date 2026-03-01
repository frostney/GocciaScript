unit Goccia.Compiler.Expressions;

{$I Goccia.inc}

interface

uses
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
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
procedure CompileThis(const ACtx: TGocciaCompilationContext;
  const ADest: UInt8);

procedure CollectDestructuringBindings(const APattern: TGocciaDestructuringPattern;
  const AScope: TGocciaCompilerScope);
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

  Souffle.Bytecode,
  Souffle.Bytecode.Chunk,
  Souffle.Bytecode.Debug,
  Souffle.Value,

  Goccia.Compiler.ConstantFolding,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.ErrorNames,
  Goccia.Lexer,
  Goccia.Parser,
  Goccia.Token,
  Goccia.Values.Primitives;

function TypedGetLocalOp(const AHint: TSouffleLocalType): TSouffleOpCode;
begin
  case AHint of
    sltInteger:   Result := OP_GET_LOCAL_INT;
    sltFloat:     Result := OP_GET_LOCAL_FLOAT;
    sltBoolean:   Result := OP_GET_LOCAL_BOOL;
    sltString:    Result := OP_GET_LOCAL_STRING;
    sltReference: Result := OP_GET_LOCAL_REF;
  else
    Result := OP_GET_LOCAL;
  end;
end;

function TypedSetLocalOp(const AHint: TSouffleLocalType): TSouffleOpCode;
begin
  case AHint of
    sltInteger:   Result := OP_SET_LOCAL_INT;
    sltFloat:     Result := OP_SET_LOCAL_FLOAT;
    sltBoolean:   Result := OP_SET_LOCAL_BOOL;
    sltString:    Result := OP_SET_LOCAL_STRING;
    sltReference: Result := OP_SET_LOCAL_REF;
  else
    Result := OP_SET_LOCAL;
  end;
end;

procedure CompileLiteral(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaLiteralExpression; const ADest: UInt8);
var
  Value: TGocciaValue;
  Idx: UInt16;
begin
  Value := AExpr.Value;

  if Value is TGocciaNullLiteralValue then
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_NIL, ADest, 1, 0))
  else if Value is TGocciaUndefinedLiteralValue then
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_NIL, ADest, 0, 0))
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
       and (TGocciaNumberLiteralValue(Value).Value >= -32768)
       and (TGocciaNumberLiteralValue(Value).Value <= 32767) then
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
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_NIL, ADest, 0, 0));
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
  Slot: UInt8;
  NameIdx: UInt16;
  CondReg, ArgReg: UInt8;
  OkJump: Integer;
  Hint: TSouffleLocalType;
begin
  LocalIdx := ACtx.Scope.ResolveLocal(AExpr.Name);
  if LocalIdx >= 0 then
  begin
    Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
    Hint := ACtx.Scope.GetLocal(LocalIdx).TypeHint;
    if (Hint <> sltUntyped) and (Slot <> ADest) then
      EmitInstruction(ACtx, EncodeABx(TypedGetLocalOp(Hint), ADest, Slot))
    else if Slot <> ADest then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, Slot, 0));
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
    EmitInstruction(ACtx, EncodeABx(OP_RT_GET_GLOBAL, ADest, NameIdx));
    Exit;
  end;

  CondReg := ACtx.Scope.AllocateRegister;
  ArgReg := ACtx.Scope.AllocateRegister;

  EmitInstruction(ACtx, EncodeABx(OP_RT_HAS_GLOBAL, CondReg, NameIdx));
  OkJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, CondReg);

  EmitInstruction(ACtx, EncodeABx(OP_RT_GET_GLOBAL, CondReg,
    ACtx.Template.AddConstantString(REFERENCE_ERROR_NAME)));
  EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, ArgReg,
    ACtx.Template.AddConstantString(AExpr.Name + ' is not defined')));
  EmitInstruction(ACtx, EncodeABC(OP_RT_CONSTRUCT, CondReg, CondReg, 1));
  EmitInstruction(ACtx, EncodeABC(OP_THROW, CondReg, 0, 0));

  PatchJumpTarget(ACtx, OkJump);

  EmitInstruction(ACtx, EncodeABx(OP_RT_GET_GLOBAL, ADest, NameIdx));

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileBinary(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaBinaryExpression; const ADest: UInt8);
var
  RegB, RegC: UInt8;
  Op: TSouffleOpCode;
  JumpIdx, JumpIdx2: Integer;
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
    JumpIdx := EmitJumpInstruction(ACtx, OP_JUMP_IF_NOT_NIL, ADest);
    ACtx.CompileExpression(AExpr.Right, ADest);
    PatchJumpTarget(ACtx, JumpIdx);
    Exit;
  end;

  RegB := ACtx.Scope.AllocateRegister;
  RegC := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(AExpr.Left, RegB);
  ACtx.CompileExpression(AExpr.Right, RegC);

  Op := TokenTypeToRuntimeOp(AExpr.Operator);
  if AExpr.Operator = gttPlus then
  begin
    EmitInstruction(ACtx, EncodeABx(OP_TO_PRIMITIVE, RegB, RegB));
    EmitInstruction(ACtx, EncodeABx(OP_TO_PRIMITIVE, RegC, RegC));
  end;
  if AExpr.Operator = gttIn then
    EmitInstruction(ACtx, EncodeABC(Op, ADest, RegC, RegB))
  else
    EmitInstruction(ACtx, EncodeABC(Op, ADest, RegB, RegC));

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
      EmitInstruction(ACtx, EncodeABC(OP_RT_DEL_INDEX, ADest, ObjReg, KeyReg));
      ACtx.Scope.FreeRegister;
    end
    else
    begin
      PropIdx := ACtx.Template.AddConstantString(MemberExpr.PropertyName);
      if PropIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: property name index exceeds 255');
      EmitInstruction(ACtx, EncodeABC(OP_RT_DEL_PROP, ADest, ObjReg, UInt8(PropIdx)));
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
    gttNot:        EmitInstruction(ACtx, EncodeABC(OP_RT_NOT, ADest, RegB, 0));
    gttMinus:      EmitInstruction(ACtx, EncodeABC(OP_RT_NEG, ADest, RegB, 0));
    gttPlus:
    begin
      EmitInstruction(ACtx, EncodeABC(OP_RT_NEG, ADest, RegB, 0));
      EmitInstruction(ACtx, EncodeABC(OP_RT_NEG, ADest, ADest, 0));
    end;
    gttTypeof:     EmitInstruction(ACtx, EncodeABC(OP_RT_TYPEOF, ADest, RegB, 0));
    gttBitwiseNot: EmitInstruction(ACtx, EncodeABC(OP_RT_BNOT, ADest, RegB, 0));
  else
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_NIL, ADest, 0, 0));
  end;

  ACtx.Scope.FreeRegister;
end;

procedure CompileAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaAssignmentExpression; const ADest: UInt8);
var
  LocalIdx, UpvalIdx: Integer;
  Slot: UInt8;
  Hint: TSouffleLocalType;
begin
  ACtx.CompileExpression(AExpr.Value, ADest);

  LocalIdx := ACtx.Scope.ResolveLocal(AExpr.Name);
  if LocalIdx >= 0 then
  begin
    Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
    Hint := ACtx.Scope.GetLocal(LocalIdx).TypeHint;
    if ADest <> Slot then
    begin
      if Hint <> sltUntyped then
        EmitInstruction(ACtx, EncodeABx(TypedSetLocalOp(Hint), ADest, Slot))
      else
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, Slot, ADest, 0));
    end;
    Exit;
  end;

  UpvalIdx := ACtx.Scope.ResolveUpvalue(AExpr.Name);
  if UpvalIdx >= 0 then
  begin
    EmitInstruction(ACtx, EncodeABx(OP_SET_UPVALUE, ADest, UInt16(UpvalIdx)));
    Exit;
  end;

  EmitInstruction(ACtx, EncodeABx(OP_RT_SET_GLOBAL, ADest,
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
  EmitInstruction(ACtx, EncodeABC(OP_RECORD_SET, ObjReg, KeyIdx, ValReg));

  if ADest <> ValReg then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, ValReg, 0));

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure EmitUndefinedCheck(const ACtx: TGocciaCompilationContext;
  const ASlot: UInt8; out AJumpIdx: Integer);
begin
  AJumpIdx := EmitJumpInstruction(ACtx, OP_JUMP_IF_NOT_NIL, ASlot,
    SOUFFLE_NIL_DEFAULT);
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
  const AScope: TGocciaCompilerScope);
var
  ObjPat: TGocciaObjectDestructuringPattern;
  ArrPat: TGocciaArrayDestructuringPattern;
  AssignPat: TGocciaAssignmentDestructuringPattern;
  I: Integer;
begin
  if APattern is TGocciaIdentifierDestructuringPattern then
    AScope.DeclareLocal(TGocciaIdentifierDestructuringPattern(APattern).Name, False)
  else if APattern is TGocciaObjectDestructuringPattern then
  begin
    ObjPat := TGocciaObjectDestructuringPattern(APattern);
    for I := 0 to ObjPat.Properties.Count - 1 do
      CollectDestructuringBindings(ObjPat.Properties[I].Pattern, AScope);
  end
  else if APattern is TGocciaArrayDestructuringPattern then
  begin
    ArrPat := TGocciaArrayDestructuringPattern(APattern);
    for I := 0 to ArrPat.Elements.Count - 1 do
      if Assigned(ArrPat.Elements[I]) then
        CollectDestructuringBindings(ArrPat.Elements[I], AScope);
  end
  else if APattern is TGocciaAssignmentDestructuringPattern then
  begin
    AssignPat := TGocciaAssignmentDestructuringPattern(APattern);
    CollectDestructuringBindings(AssignPat.Left, AScope);
  end
  else if APattern is TGocciaRestDestructuringPattern then
    CollectDestructuringBindings(
      TGocciaRestDestructuringPattern(APattern).Argument, AScope);
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
        EmitInstruction(ACtx, EncodeABx(OP_RT_SET_GLOBAL, ASrcReg,
          ACtx.Template.AddConstantString(IdentPat.Name)));
    end;
  end
  else if APattern is TGocciaObjectDestructuringPattern then
  begin
    EmitInstruction(ACtx, EncodeABC(OP_RT_REQUIRE_OBJECT, ASrcReg, 0, 0));
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
        EmitInstruction(ACtx, EncodeABC(OP_RT_OBJ_REST, DestSlot, ASrcReg,
          IdxReg));
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
        EmitInstruction(ACtx, EncodeABC(OP_RT_GET_INDEX, DestSlot, ASrcReg,
          IdxReg));
        ACtx.Scope.FreeRegister;
      end
      else
      begin
        PropIdx := ACtx.Template.AddConstantString(Prop.Key);
        EmitInstruction(ACtx, EncodeABC(OP_RT_GET_PROP, DestSlot, ASrcReg,
          UInt8(PropIdx)));
      end;

      EmitDestructuring(ACtx, Prop.Pattern, DestSlot);
      ACtx.Scope.FreeRegister;
    end;
  end
  else if APattern is TGocciaArrayDestructuringPattern then
  begin
    EmitInstruction(ACtx, EncodeABC(OP_RT_REQUIRE_ITERABLE, ASrcReg, 0, 0));
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

procedure CompileArrowFunction(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaArrowFunctionExpression; const ADest: UInt8);
var
  OldTemplate: TSouffleFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TSouffleFunctionTemplate;
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

  ChildTemplate := TSouffleFunctionTemplate.Create('<arrow>');
  ChildTemplate.DebugInfo := TSouffleDebugInfo.Create(ACtx.SourcePath);
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

  if FormalCount < 0 then
    FormalCount := Length(AExpr.Parameters);
  if Assigned(ACtx.FormalParameterCounts) then
    ACtx.FormalParameterCounts.AddOrSetValue(ChildTemplate, FormalCount);

  ACtx.SwapState(ChildTemplate, ChildScope);

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

  ACtx.CompileFunctionBody(AExpr.Body);

  ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

  for I := 0 to ChildScope.UpvalueCount - 1 do
    ChildTemplate.AddUpvalueDescriptor(
      ChildScope.GetUpvalue(I).IsLocal,
      ChildScope.GetUpvalue(I).Index);

  ACtx.SwapState(OldTemplate, OldScope);
  ChildScope.Free;

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
      EmitInstruction(ACtx, EncodeABC(OP_RT_SPREAD, AArrayReg, ElemReg, 0));
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
  BaseReg, ObjReg, ArgsReg: UInt8;
  MemberExpr: TGocciaMemberExpression;
  PropIdx: UInt16;
  UseSpread: Boolean;
begin
  ArgCount := AExpr.Arguments.Count;
  UseSpread := HasSpreadArgument(AExpr);

  if AExpr.Callee is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(AExpr.Callee);
    ObjReg := ACtx.Scope.AllocateRegister;
    BaseReg := ACtx.Scope.AllocateRegister;

    ACtx.CompileExpression(MemberExpr.ObjectExpr, ObjReg);

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
      EmitInstruction(ACtx, EncodeABC(OP_RECORD_GET, BaseReg, ObjReg, UInt8(PropIdx)));
    end;

    if UseSpread then
    begin
      ArgsReg := ACtx.Scope.AllocateRegister;
      CompileSpreadArgsArray(ACtx, AExpr, ArgsReg);
      EmitInstruction(ACtx, EncodeABC(OP_RT_CALL_METHOD_SPREAD, BaseReg, ArgsReg, 0));
      ACtx.Scope.FreeRegister;
    end
    else
    begin
      for I := 0 to ArgCount - 1 do
        ACtx.CompileExpression(AExpr.Arguments[I], ACtx.Scope.AllocateRegister);
      EmitInstruction(ACtx, EncodeABC(OP_RT_CALL_METHOD, BaseReg, UInt8(ArgCount), 0));
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
    BaseReg := ACtx.Scope.AllocateRegister;

    ACtx.CompileExpression(AExpr.Callee, BaseReg);

    if UseSpread then
    begin
      ArgsReg := ACtx.Scope.AllocateRegister;
      CompileSpreadArgsArray(ACtx, AExpr, ArgsReg);
      EmitInstruction(ACtx, EncodeABC(OP_RT_CALL_SPREAD, BaseReg, ArgsReg, 0));
      ACtx.Scope.FreeRegister;
    end
    else
    begin
      for I := 0 to ArgCount - 1 do
        ACtx.CompileExpression(AExpr.Arguments[I], ACtx.Scope.AllocateRegister);
      EmitInstruction(ACtx, EncodeABC(OP_RT_CALL, BaseReg, UInt8(ArgCount), 0));
      for I := 0 to ArgCount - 1 do
        ACtx.Scope.FreeRegister;
    end;

    if ADest <> BaseReg then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, BaseReg, 0));

    ACtx.Scope.FreeRegister;
  end;
end;

procedure CompileMember(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaMemberExpression; const ADest: UInt8);
var
  ObjReg, IdxReg: UInt8;
  PropIdx: UInt16;
begin
  ObjReg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AExpr.ObjectExpr, ObjReg);

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
    EmitInstruction(ACtx, EncodeABC(OP_RECORD_GET, ADest, ObjReg, UInt8(PropIdx)));
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
      EmitInstruction(ACtx, EncodeABC(OP_RT_SPREAD, ADest, ElemReg, 0));
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
  EmitInstruction(ACtx, EncodeABC(OP_RT_SET_PROP, ADest, KeyIdx, ValReg));
  ACtx.Scope.FreeRegister;
end;

procedure CompileGetterProperty(const ACtx: TGocciaCompilationContext;
  const ADest: UInt8; const AKey: string;
  const AGetter: TGocciaGetterExpression);
var
  OldTemplate: TSouffleFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TSouffleFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  FuncIdx: UInt16;
  GetterReg: UInt8;
  KeyIdx: UInt16;
  I: Integer;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TSouffleFunctionTemplate.Create('<get ' + AKey + '>');
  ChildTemplate.DebugInfo := TSouffleDebugInfo.Create(ACtx.SourcePath);
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);
  ChildScope.DeclareLocal('this', False);
  ChildTemplate.ParameterCount := 0;

  ACtx.SwapState(ChildTemplate, ChildScope);
  ACtx.CompileFunctionBody(AGetter.Body);
  ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

  for I := 0 to ChildScope.UpvalueCount - 1 do
    ChildTemplate.AddUpvalueDescriptor(
      ChildScope.GetUpvalue(I).IsLocal,
      ChildScope.GetUpvalue(I).Index);

  ACtx.SwapState(OldTemplate, OldScope);
  ChildScope.Free;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  GetterReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, GetterReg, FuncIdx));
  KeyIdx := ACtx.Template.AddConstantString(AKey);
  if KeyIdx > High(UInt8) then
    raise Exception.Create('Constant pool overflow: property name index exceeds 255');
  EmitInstruction(ACtx, EncodeABC(OP_RT_DEF_GETTER, ADest, KeyIdx, GetterReg));
  ACtx.Scope.FreeRegister;
end;

procedure CompileSetterProperty(const ACtx: TGocciaCompilationContext;
  const ADest: UInt8; const AKey: string;
  const ASetter: TGocciaSetterExpression);
var
  OldTemplate: TSouffleFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TSouffleFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  FuncIdx: UInt16;
  SetterReg: UInt8;
  KeyIdx: UInt16;
  I: Integer;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TSouffleFunctionTemplate.Create('<set ' + AKey + '>');
  ChildTemplate.DebugInfo := TSouffleDebugInfo.Create(ACtx.SourcePath);
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);
  ChildScope.DeclareLocal('this', False);
  ChildScope.DeclareLocal(ASetter.Parameter, False);
  ChildTemplate.ParameterCount := 1;

  ACtx.SwapState(ChildTemplate, ChildScope);
  ACtx.CompileFunctionBody(ASetter.Body);
  ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

  for I := 0 to ChildScope.UpvalueCount - 1 do
    ChildTemplate.AddUpvalueDescriptor(
      ChildScope.GetUpvalue(I).IsLocal,
      ChildScope.GetUpvalue(I).Index);

  ACtx.SwapState(OldTemplate, OldScope);
  ChildScope.Free;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  SetterReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, SetterReg, FuncIdx));
  KeyIdx := ACtx.Template.AddConstantString(AKey);
  if KeyIdx > High(UInt8) then
    raise Exception.Create('Constant pool overflow: property name index exceeds 255');
  EmitInstruction(ACtx, EncodeABC(OP_RT_DEF_SETTER, ADest, KeyIdx, SetterReg));
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
  EmitInstruction(ACtx, EncodeABx(OP_RT_GET_GLOBAL, ADest,
    ACtx.Template.AddConstantString(CONSTRUCTOR_OBJECT)));
  EmitInstruction(ACtx, EncodeABC(OP_RT_CONSTRUCT, ADest, ADest, 0));

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
              EmitInstruction(ACtx, EncodeABC(OP_RT_SPREAD_OBJ, ADest, ValReg, 0));
              ACtx.Scope.FreeRegister;
            end
            else
            begin
              KeyReg := ACtx.Scope.AllocateRegister;
              ValReg := ACtx.Scope.AllocateRegister;
              ACtx.CompileExpression(Pair.Key, KeyReg);
              ACtx.CompileExpression(Pair.Value, ValReg);
              EmitInstruction(ACtx, EncodeABC(OP_RT_SET_INDEX, ADest, KeyReg, ValReg));
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
              ProgramNode := Parser.Parse;
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
                    EmitInstruction(ACtx, EncodeABC(OP_RT_TO_STRING, ADest, ADest, 0));
                    HasParts := True;
                  end
                  else
                  begin
                    PartReg := ACtx.Scope.AllocateRegister;
                    ACtx.CompileExpression(ParsedExpr, PartReg);
                    EmitInstruction(ACtx, EncodeABC(OP_RT_TO_STRING, PartReg, PartReg, 0));
                    EmitInstruction(ACtx, EncodeABC(OP_RT_ADD, ADest, ADest, PartReg));
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
          EmitInstruction(ACtx, EncodeABC(OP_RT_ADD, ADest, ADest, PartReg));
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
    EmitInstruction(ACtx, EncodeABC(OP_RT_ADD, ADest, ADest, PartReg));
    ACtx.Scope.FreeRegister;
  end;
end;

procedure CompileNewExpression(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaNewExpression; const ADest: UInt8);
var
  CtorReg: UInt8;
  ArgCount, I: Integer;
begin
  CtorReg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AExpr.Callee, CtorReg);

  ArgCount := AExpr.Arguments.Count;
  for I := 0 to ArgCount - 1 do
    ACtx.CompileExpression(AExpr.Arguments[I], ACtx.Scope.AllocateRegister);

  EmitInstruction(ACtx, EncodeABC(OP_RT_CONSTRUCT, ADest, CtorReg, UInt8(ArgCount)));

  for I := 0 to ArgCount - 1 do
    ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
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
  OldTemplate: TSouffleFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TSouffleFunctionTemplate;
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

  ChildTemplate := TSouffleFunctionTemplate.Create('<method>');
  ChildTemplate.DebugInfo := TSouffleDebugInfo.Create(ACtx.SourcePath);
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);

  ChildScope.DeclareLocal('this', False);
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

  if FormalCount < 0 then
    FormalCount := Length(AExpr.Parameters);
  if Assigned(ACtx.FormalParameterCounts) then
    ACtx.FormalParameterCounts.AddOrSetValue(ChildTemplate, FormalCount);

  ACtx.SwapState(ChildTemplate, ChildScope);

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

  ACtx.CompileFunctionBody(AExpr.Body);

  ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

  for I := 0 to ChildScope.UpvalueCount - 1 do
    ChildTemplate.AddUpvalueDescriptor(
      ChildScope.GetUpvalue(I).IsLocal,
      ChildScope.GetUpvalue(I).Index);

  ACtx.SwapState(OldTemplate, OldScope);
  ChildScope.Free;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, ADest, FuncIdx));
end;

procedure CompileCompoundAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaCompoundAssignmentExpression; const ADest: UInt8);
var
  LocalIdx, UpvalIdx: Integer;
  Slot, RegVal, RegResult: UInt8;
  Op: TSouffleOpCode;
begin
  Op := CompoundOpToRuntimeOp(AExpr.Operator);

  LocalIdx := ACtx.Scope.ResolveLocal(AExpr.Name);
  if LocalIdx >= 0 then
  begin
    Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
    RegVal := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(AExpr.Value, RegVal);
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
  EmitInstruction(ACtx, EncodeABx(OP_RT_GET_GLOBAL, RegResult,
    ACtx.Template.AddConstantString(AExpr.Name)));
  ACtx.CompileExpression(AExpr.Value, RegVal);
  EmitInstruction(ACtx, EncodeABC(Op, ADest, RegResult, RegVal));
  EmitInstruction(ACtx, EncodeABx(OP_RT_SET_GLOBAL, ADest,
    ACtx.Template.AddConstantString(AExpr.Name)));
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompilePropertyCompoundAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaPropertyCompoundAssignmentExpression; const ADest: UInt8);
var
  ObjReg, CurReg, ValReg: UInt8;
  Op: TSouffleOpCode;
  PropIdx: UInt16;
begin
  Op := CompoundOpToRuntimeOp(AExpr.Operator);
  ObjReg := ACtx.Scope.AllocateRegister;
  CurReg := ACtx.Scope.AllocateRegister;
  ValReg := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(AExpr.ObjectExpr, ObjReg);
  PropIdx := ACtx.Template.AddConstantString(AExpr.PropertyName);
  EmitInstruction(ACtx, EncodeABC(OP_RT_GET_PROP, CurReg, ObjReg,
    UInt8(PropIdx)));
  ACtx.CompileExpression(AExpr.Value, ValReg);
  EmitInstruction(ACtx, EncodeABC(Op, CurReg, CurReg, ValReg));
  EmitInstruction(ACtx, EncodeABC(OP_RT_SET_PROP, ObjReg, UInt8(PropIdx),
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
  Op: TSouffleOpCode;
begin
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

procedure CompileIncrement(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIncrementExpression; const ADest: UInt8);
var
  LocalIdx, UpvalIdx: Integer;
  Slot, RegOne, RegResult: UInt8;
  Op: TSouffleOpCode;
  Ident: TGocciaIdentifierExpression;
begin
  if AExpr.Operator = gttIncrement then
    Op := OP_ADD_INT
  else
    Op := OP_SUB_INT;

  if not (AExpr.Operand is TGocciaIdentifierExpression) then
  begin
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_NIL, ADest, 0, 0));
    Exit;
  end;

  Ident := TGocciaIdentifierExpression(AExpr.Operand);

  LocalIdx := ACtx.Scope.ResolveLocal(Ident.Name);
  if LocalIdx >= 0 then
  begin
    Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
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
  EmitInstruction(ACtx, EncodeABx(OP_RT_GET_GLOBAL, RegResult,
    ACtx.Template.AddConstantString(Ident.Name)));
  EmitInstruction(ACtx, EncodeAsBx(OP_LOAD_INT, RegOne, 1));
  if not AExpr.IsPrefix then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, RegResult, 0));
  EmitInstruction(ACtx, EncodeABC(Op, RegResult, RegResult, RegOne));
  EmitInstruction(ACtx, EncodeABx(OP_RT_SET_GLOBAL, RegResult,
    ACtx.Template.AddConstantString(Ident.Name)));
  if AExpr.IsPrefix then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, RegResult, 0));
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileThis(const ACtx: TGocciaCompilationContext;
  const ADest: UInt8);
var
  LocalIdx, UpvalIdx: Integer;
  Slot: UInt8;
begin
  LocalIdx := ACtx.Scope.ResolveLocal('this');
  if LocalIdx >= 0 then
  begin
    Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
    if Slot <> ADest then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, Slot, 0));
    Exit;
  end;

  UpvalIdx := ACtx.Scope.ResolveUpvalue('this');
  if UpvalIdx >= 0 then
  begin
    EmitInstruction(ACtx, EncodeABx(OP_GET_UPVALUE, ADest, UInt16(UpvalIdx)));
    Exit;
  end;

  EmitInstruction(ACtx, EncodeABC(OP_LOAD_NIL, ADest, 0, 0));
end;

end.
