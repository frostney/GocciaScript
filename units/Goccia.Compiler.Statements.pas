unit Goccia.Compiler.Statements;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Compiler.Context;

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
procedure CompileImportDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaImportDeclaration);
procedure CompileExportDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExportDeclaration);
procedure CompileSwitchStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaSwitchStatement);
procedure CompileBreakStatement(const ACtx: TGocciaCompilationContext);
procedure CompileDestructuringDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaDestructuringDeclaration);

function IsSimpleClass(const AClassDef: TGocciaClassDefinition): Boolean;
procedure CompileClassDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaClassDeclaration);

function SavePendingFinally: TObject;
procedure RestorePendingFinally(const ASaved: TObject);

implementation

uses
  SysUtils,

  Souffle.Bytecode,
  Souffle.Bytecode.Chunk,
  Souffle.Bytecode.Debug,

  Goccia.Compiler.Expressions,
  Goccia.Compiler.Scope,
  Goccia.Values.Primitives;

type
  TPendingFinallyEntry = record
    FinallyBlock: TGocciaBlockStatement;
  end;

var
  GBreakJumps: TList<Integer> = nil;
  GPendingFinally: TList<TPendingFinallyEntry>;

procedure CompileExpressionStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExpressionStatement);
var
  Reg: UInt8;
begin
  Reg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AStmt.Expression, Reg);
  ACtx.Scope.FreeRegister;
end;

function InferLocalType(const AExpr: TGocciaExpression): TSouffleLocalType;
var
  Lit: TGocciaLiteralExpression;
begin
  Result := sltUntyped;
  if AExpr is TGocciaLiteralExpression then
  begin
    Lit := TGocciaLiteralExpression(AExpr);
    if Lit.Value is TGocciaNumberLiteralValue then
    begin
      if (Frac(TGocciaNumberLiteralValue(Lit.Value).Value) = 0) and
         (Abs(TGocciaNumberLiteralValue(Lit.Value).Value) < MaxInt) then
        Result := sltInteger
      else
        Result := sltFloat;
    end
    else if Lit.Value is TGocciaBooleanLiteralValue then
      Result := sltBoolean
    else if Lit.Value is TGocciaStringLiteralValue then
      Result := sltString;
  end
  else if AExpr is TGocciaTemplateLiteralExpression then
    Result := sltString
  else if AExpr is TGocciaTemplateWithInterpolationExpression then
    Result := sltString;
end;

procedure CompileVariableDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaVariableDeclaration);
var
  I, FuncCount, LocalIdx: Integer;
  Info: TGocciaVariableInfo;
  Slot: UInt8;
  InferredTemplate: TSouffleFunctionTemplate;
  TypeHint: TSouffleLocalType;
begin
  for I := 0 to High(AStmt.Variables) do
  begin
    Info := AStmt.Variables[I];
    Slot := ACtx.Scope.DeclareLocal(Info.Name, AStmt.IsConst);
    if Assigned(Info.Initializer) then
    begin
      TypeHint := InferLocalType(Info.Initializer);
      if TypeHint <> sltUntyped then
      begin
        LocalIdx := ACtx.Scope.ResolveLocal(Info.Name);
        if LocalIdx >= 0 then
        begin
          ACtx.Scope.SetLocalTypeHint(LocalIdx, TypeHint);
          ACtx.Template.SetLocalType(Slot, TypeHint);
        end;
      end;

      FuncCount := ACtx.Template.FunctionCount;
      ACtx.CompileExpression(Info.Initializer, Slot);

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
    else
      EmitInstruction(ACtx, EncodeABC(OP_LOAD_NIL, Slot, 0, 0));
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
end;

procedure CompileIfStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaIfStatement);
var
  CondReg: UInt8;
  ElseJump, EndJump: Integer;
begin
  CondReg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AStmt.Condition, CondReg);
  ACtx.Scope.FreeRegister;

  ElseJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, CondReg);
  ACtx.CompileStatement(AStmt.Consequent);

  if Assigned(AStmt.Alternate) then
  begin
    EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
    PatchJumpTarget(ACtx, ElseJump);
    ACtx.CompileStatement(AStmt.Alternate);
    PatchJumpTarget(ACtx, EndJump);
  end
  else
    PatchJumpTarget(ACtx, ElseJump);
end;

procedure CompileReturnStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaReturnStatement);
var
  Reg: UInt8;
  I: Integer;
begin
  if Assigned(AStmt.Value) then
  begin
    Reg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(AStmt.Value, Reg);

    if Assigned(GPendingFinally) then
      for I := GPendingFinally.Count - 1 downto 0 do
      begin
        EmitInstruction(ACtx, EncodeABC(OP_POP_HANDLER, 0, 0, 0));
        CompileBlockStatement(ACtx, GPendingFinally[I].FinallyBlock);
      end;

    EmitInstruction(ACtx, EncodeABC(OP_RETURN, Reg, 0, 0));
    ACtx.Scope.FreeRegister;
  end
  else
  begin
    if Assigned(GPendingFinally) then
      for I := GPendingFinally.Count - 1 downto 0 do
      begin
        EmitInstruction(ACtx, EncodeABC(OP_POP_HANDLER, 0, 0, 0));
        CompileBlockStatement(ACtx, GPendingFinally[I].FinallyBlock);
      end;

    EmitInstruction(ACtx, EncodeABC(OP_RETURN_NIL, 0, 0, 0));
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
  CatchReg: UInt8;
  HandlerJump, EndJump: Integer;
  HasCatch, HasFinally: Boolean;
  I: Integer;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount: Integer;
  Entry: TPendingFinallyEntry;
begin
  HasCatch := Assigned(AStmt.CatchBlock);
  HasFinally := Assigned(AStmt.FinallyBlock);

  CatchReg := ACtx.Scope.AllocateRegister;
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
    if AStmt.CatchParam <> '' then
    begin
      ACtx.Scope.BeginScope;
      ACtx.Scope.DeclareLocal(AStmt.CatchParam, False);
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ACtx.Scope.NextSlot - 1, CatchReg, 0));
    end;

    CompileBlockStatement(ACtx, AStmt.CatchBlock);

    if AStmt.CatchParam <> '' then
    begin
      ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
      for I := 0 to ClosedCount - 1 do
        EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));
    end;

    if HasFinally then
      CompileBlockStatement(ACtx, AStmt.FinallyBlock);
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

procedure CompileForOfStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaForOfStatement);
var
  IterReg, ValueReg, DoneReg: UInt8;
  LoopStart, ExitJump, I: Integer;
  Slot: UInt8;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount: Integer;
begin
  IterReg := ACtx.Scope.AllocateRegister;
  ValueReg := ACtx.Scope.AllocateRegister;
  DoneReg := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(AStmt.Iterable, IterReg);
  EmitInstruction(ACtx, EncodeABC(OP_RT_GET_ITER, IterReg, IterReg, 0));

  LoopStart := CurrentCodePosition(ACtx);

  EmitInstruction(ACtx, EncodeABC(OP_RT_ITER_NEXT, ValueReg, DoneReg, IterReg));
  ExitJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, DoneReg);

  ACtx.Scope.BeginScope;

  if AStmt.BindingName <> '' then
  begin
    Slot := ACtx.Scope.DeclareLocal(AStmt.BindingName, AStmt.IsConst);
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, Slot, ValueReg, 0));
  end;

  ACtx.CompileStatement(AStmt.Body);

  ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
  for I := 0 to ClosedCount - 1 do
    EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));

  EmitInstruction(ACtx, EncodeAx(OP_JUMP, LoopStart - CurrentCodePosition(ACtx) - 1));

  PatchJumpTarget(ACtx, ExitJump);

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileImportDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaImportDeclaration);
var
  ModReg: UInt8;
  PathIdx, NameIdx: UInt16;
  Pair: TPair<string, string>;
  Slot: UInt8;
begin
  ModReg := ACtx.Scope.AllocateRegister;
  PathIdx := ACtx.Template.AddConstantString(AStmt.ModulePath);
  EmitInstruction(ACtx, EncodeABx(OP_RT_IMPORT, ModReg, PathIdx));

  for Pair in AStmt.Imports do
  begin
    Slot := ACtx.Scope.DeclareLocal(Pair.Key, True);
    NameIdx := ACtx.Template.AddConstantString(Pair.Value);
    if NameIdx > High(UInt8) then
      raise Exception.Create('Constant pool overflow: import name index exceeds 255');
    EmitInstruction(ACtx, EncodeABC(OP_RECORD_GET, Slot, ModReg, UInt8(NameIdx)));
  end;

  ACtx.Scope.FreeRegister;
end;

procedure CompileExportDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExportDeclaration);
var
  Pair: TPair<string, string>;
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
      EmitInstruction(ACtx, EncodeABx(OP_RT_EXPORT, Reg, NameIdx));
    end;
  end;
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
    EmitInstruction(ACtx, EncodeABC(OP_RT_EQ, CmpReg, DiscReg, TestReg));
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
  BreakJumps := TList<Integer>.Create;
  GBreakJumps := BreakJumps;
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
  end;
end;

procedure CompileBreakStatement(const ACtx: TGocciaCompilationContext);
begin
  if Assigned(GBreakJumps) then
    GBreakJumps.Add(EmitJumpInstruction(ACtx, OP_JUMP, 0));
end;

function IsSimpleClass(const AClassDef: TGocciaClassDefinition): Boolean;
begin
  Result := (AClassDef.Getters.Count = 0) and
    (AClassDef.Setters.Count = 0) and
    (AClassDef.StaticGetters.Count = 0) and
    (AClassDef.StaticSetters.Count = 0) and
    (AClassDef.StaticProperties.Count = 0) and
    (AClassDef.InstanceProperties.Count = 0) and
    (AClassDef.PrivateInstanceProperties.Count = 0) and
    (AClassDef.PrivateMethods.Count = 0) and
    (Length(AClassDef.Decorators) = 0) and
    (Length(AClassDef.FComputedStaticGetters) = 0) and
    (Length(AClassDef.FComputedStaticSetters) = 0) and
    (Length(AClassDef.FComputedInstanceGetters) = 0) and
    (Length(AClassDef.FComputedInstanceSetters) = 0);
end;

procedure CompileClassDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaClassDeclaration);
var
  ClassDef: TGocciaClassDefinition;
  ClassReg, MethodReg, SuperReg: UInt8;
  NameIdx, MethodNameIdx: UInt16;
  MethodPair: TPair<string, TGocciaClassMethod>;
  OldTemplate: TSouffleFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TSouffleFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  ChildCtx: TGocciaCompilationContext;
  FuncIdx: UInt16;
  FormalCount, RestParamIndex, I: Integer;
begin
  ClassDef := AStmt.ClassDefinition;
  ClassReg := ACtx.Scope.AllocateRegister;
  NameIdx := ACtx.Template.AddConstantString(ClassDef.Name);
  EmitInstruction(ACtx, EncodeABx(OP_NEW_BLUEPRINT, ClassReg, NameIdx));

  if ClassDef.SuperClass <> '' then
  begin
    SuperReg := ACtx.Scope.AllocateRegister;
    EmitInstruction(ACtx, EncodeABx(OP_RT_GET_GLOBAL, SuperReg,
      ACtx.Template.AddConstantString(ClassDef.SuperClass)));
    EmitInstruction(ACtx, EncodeABC(OP_INHERIT, ClassReg, SuperReg, 0));
    ACtx.Scope.FreeRegister;
  end;

  for MethodPair in ClassDef.Methods do
  begin
    OldTemplate := ACtx.Template;
    OldScope := ACtx.Scope;

    ChildTemplate := TSouffleFunctionTemplate.Create('<method ' + MethodPair.Key + '>');
    ChildTemplate.DebugInfo := TSouffleDebugInfo.Create(ACtx.SourcePath);
    ChildScope := TGocciaCompilerScope.Create(OldScope, 0);

    ChildScope.DeclareLocal('this', False);
    ChildTemplate.ParameterCount := Length(MethodPair.Value.Parameters);

    FormalCount := -1;
    RestParamIndex := -1;
    for I := 0 to High(MethodPair.Value.Parameters) do
    begin
      if MethodPair.Value.Parameters[I].IsRest or
         Assigned(MethodPair.Value.Parameters[I].DefaultValue) then
      begin
        if FormalCount < 0 then
          FormalCount := I;
        if MethodPair.Value.Parameters[I].IsRest then
          RestParamIndex := I;
      end;
      ChildScope.DeclareLocal(MethodPair.Value.Parameters[I].Name, False);
    end;
    if FormalCount < 0 then
      FormalCount := Length(MethodPair.Value.Parameters);
    if Assigned(ACtx.FormalParameterCounts) then
      ACtx.FormalParameterCounts.AddOrSetValue(ChildTemplate, FormalCount);

    ACtx.SwapState(ChildTemplate, ChildScope);

    ChildCtx := ACtx;
    ChildCtx.Template := ChildTemplate;
    ChildCtx.Scope := ChildScope;

    if RestParamIndex >= 0 then
      EmitInstruction(ChildCtx, EncodeABC(OP_PACK_ARGS,
        UInt8(ChildScope.ResolveLocal(
          MethodPair.Value.Parameters[RestParamIndex].Name)),
        UInt8(RestParamIndex), 0));

    ACtx.CompileFunctionBody(MethodPair.Value.Body);
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

    MethodNameIdx := ACtx.Template.AddConstantString(MethodPair.Key);
    if MethodNameIdx > High(UInt8) then
      raise Exception.Create('Constant pool overflow: method name index exceeds 255');
    EmitInstruction(ACtx, EncodeABC(OP_RECORD_SET,
      ClassReg, UInt8(MethodNameIdx), MethodReg));
    ACtx.Scope.FreeRegister;
  end;

  EmitInstruction(ACtx, EncodeABx(OP_RT_SET_GLOBAL, ClassReg,
    ACtx.Template.AddConstantString(ClassDef.Name)));
  ACtx.Scope.FreeRegister;
end;

procedure CompileDestructuringDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaDestructuringDeclaration);
var
  SrcReg: UInt8;
begin
  CollectDestructuringBindings(AStmt.Pattern, ACtx.Scope);

  SrcReg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AStmt.Initializer, SrcReg);
  EmitDestructuring(ACtx, AStmt.Pattern, SrcReg);
  ACtx.Scope.FreeRegister;
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
