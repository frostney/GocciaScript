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
procedure CompileDestructuringDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaDestructuringDeclaration);
procedure CompileEnumDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaEnumDeclaration);

function IsSimpleClass(const AClassDef: TGocciaClassDefinition): Boolean;
procedure CompileClassDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaClassDeclaration);
procedure CompileComplexClassDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaClassDeclaration; const APendingIndex: Integer);

function SavePendingFinally: TObject;
procedure RestorePendingFinally(const ASaved: TObject);

implementation

uses
  SysUtils,

  Souffle.Bytecode,
  Souffle.Bytecode.Chunk,
  Souffle.Bytecode.Debug,

  Goccia.Compiler.Expressions,
  Goccia.Compiler.ExtOps,
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
  SavedFinally: TList<TPendingFinallyEntry>;
begin
  if Assigned(AStmt.Value) then
  begin
    Reg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(AStmt.Value, Reg);

    if Assigned(GPendingFinally) then
    begin
      SavedFinally := GPendingFinally;
      GPendingFinally := nil;
      for I := SavedFinally.Count - 1 downto 0 do
      begin
        EmitInstruction(ACtx, EncodeABC(OP_POP_HANDLER, 0, 0, 0));
        CompileBlockStatement(ACtx, SavedFinally[I].FinallyBlock);
      end;
      GPendingFinally := SavedFinally;
    end;

    EmitInstruction(ACtx, EncodeABC(OP_RETURN, Reg, 0, 0));
    ACtx.Scope.FreeRegister;
  end
  else
  begin
    if Assigned(GPendingFinally) then
    begin
      SavedFinally := GPendingFinally;
      GPendingFinally := nil;
      for I := SavedFinally.Count - 1 downto 0 do
      begin
        EmitInstruction(ACtx, EncodeABC(OP_POP_HANDLER, 0, 0, 0));
        CompileBlockStatement(ACtx, SavedFinally[I].FinallyBlock);
      end;
      GPendingFinally := SavedFinally;
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
    if HasFinally then
      GPendingFinally.Add(Entry);

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
    begin
      GPendingFinally.Delete(GPendingFinally.Count - 1);
      CompileBlockStatement(ACtx, AStmt.FinallyBlock);
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

procedure CompileForOfStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaForOfStatement);
var
  IterReg, ValueReg, DoneReg: UInt8;
  LoopStart, ExitJump, I: Integer;
  Slot: UInt8;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount: Integer;
  OldBreakJumps: TList<Integer>;
  BreakJumps: TList<Integer>;
begin
  IterReg := ACtx.Scope.AllocateRegister;
  ValueReg := ACtx.Scope.AllocateRegister;
  DoneReg := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(AStmt.Iterable, IterReg);
  EmitInstruction(ACtx, EncodeABC(OP_RT_GET_ITER, IterReg, IterReg, 0));

  OldBreakJumps := GBreakJumps;
  BreakJumps := TList<Integer>.Create;
  GBreakJumps := BreakJumps;
  try
    LoopStart := CurrentCodePosition(ACtx);

    EmitInstruction(ACtx, EncodeABC(OP_RT_ITER_NEXT, ValueReg, DoneReg, IterReg));
    ExitJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, DoneReg);

    ACtx.Scope.BeginScope;

    if Assigned(AStmt.BindingPattern) then
    begin
      CollectDestructuringBindings(AStmt.BindingPattern, ACtx.Scope);
      EmitDestructuring(ACtx, AStmt.BindingPattern, ValueReg);
    end
    else if AStmt.BindingName <> '' then
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

    for I := 0 to BreakJumps.Count - 1 do
      PatchJumpTarget(ACtx, BreakJumps[I]);
  finally
    BreakJumps.Free;
    GBreakJumps := OldBreakJumps;
  end;

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileForAwaitOfStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaForAwaitOfStatement);
var
  IterReg, ValueReg, DoneReg: UInt8;
  LoopStart, ExitJump, I: Integer;
  Slot: UInt8;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount: Integer;
  OldBreakJumps: TList<Integer>;
  BreakJumps: TList<Integer>;
begin
  IterReg := ACtx.Scope.AllocateRegister;
  ValueReg := ACtx.Scope.AllocateRegister;
  DoneReg := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(AStmt.Iterable, IterReg);
  EmitInstruction(ACtx, EncodeABC(OP_RT_GET_ITER, IterReg, IterReg, 1));

  OldBreakJumps := GBreakJumps;
  BreakJumps := TList<Integer>.Create;
  GBreakJumps := BreakJumps;
  try
    LoopStart := CurrentCodePosition(ACtx);

    EmitInstruction(ACtx, EncodeABC(OP_RT_ITER_NEXT, ValueReg, DoneReg, IterReg));
    ExitJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, DoneReg);
    EmitInstruction(ACtx, EncodeABC(OP_RT_AWAIT, ValueReg, ValueReg, 0));

    ACtx.Scope.BeginScope;

    if Assigned(AStmt.BindingPattern) then
    begin
      CollectDestructuringBindings(AStmt.BindingPattern, ACtx.Scope);
      EmitDestructuring(ACtx, AStmt.BindingPattern, ValueReg);
    end
    else if AStmt.BindingName <> '' then
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

    for I := 0 to BreakJumps.Count - 1 do
      PatchJumpTarget(ACtx, BreakJumps[I]);
  finally
    BreakJumps.Free;
    GBreakJumps := OldBreakJumps;
  end;

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
  Slots: array of UInt8;
  Names: array of string;
  I, Count: Integer;
begin
  Count := AStmt.Imports.Count;
  SetLength(Slots, Count);
  SetLength(Names, Count);

  I := 0;
  for Pair in AStmt.Imports do
  begin
    Slots[I] := ACtx.Scope.DeclareLocal(Pair.Key, True);
    Names[I] := Pair.Value;
    Inc(I);
  end;

  ModReg := ACtx.Scope.AllocateRegister;
  PathIdx := ACtx.Template.AddConstantString(AStmt.ModulePath);
  EmitInstruction(ACtx, EncodeABx(OP_RT_IMPORT, ModReg, PathIdx));

  for I := 0 to Count - 1 do
  begin
    NameIdx := ACtx.Template.AddConstantString(Names[I]);
    if NameIdx > High(UInt8) then
      raise Exception.Create('Constant pool overflow: import name index exceeds 255');
    EmitInstruction(ACtx, EncodeABC(OP_RECORD_GET, Slots[I], ModReg,
      UInt8(NameIdx)));
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
      EmitInstruction(ACtx, EncodeABx(OP_RT_EXPORT, Reg, NameIdx));
    end;
  end;
end;

procedure CompileReExportDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaReExportDeclaration);
var
  ModReg, ValReg: UInt8;
  PathIdx, SrcNameIdx, ExportNameIdx: UInt16;
  Pair: TPair<string, string>;
begin
  ModReg := ACtx.Scope.AllocateRegister;
  ValReg := ACtx.Scope.AllocateRegister;
  PathIdx := ACtx.Template.AddConstantString(AStmt.ModulePath);
  EmitInstruction(ACtx, EncodeABx(OP_RT_IMPORT, ModReg, PathIdx));

  for Pair in AStmt.ExportsTable do
  begin
    SrcNameIdx := ACtx.Template.AddConstantString(Pair.Value);
    if SrcNameIdx > High(UInt8) then
      raise Exception.Create('Constant pool overflow: re-export source name index exceeds 255');
    EmitInstruction(ACtx, EncodeABC(OP_RECORD_GET, ValReg, ModReg,
      UInt8(SrcNameIdx)));
    ExportNameIdx := ACtx.Template.AddConstantString(Pair.Key);
    EmitInstruction(ACtx, EncodeABx(OP_RT_EXPORT, ValReg, ExportNameIdx));
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
  Result := (AClassDef.InstanceProperties.Count = 0) and
    (AClassDef.PrivateInstanceProperties.Count = 0) and
    (Length(AClassDef.Decorators) = 0) and
    (Length(AClassDef.FElements) = 0) and
    (Length(AClassDef.FComputedStaticGetters) = 0) and
    (Length(AClassDef.FComputedStaticSetters) = 0) and
    (Length(AClassDef.FComputedInstanceGetters) = 0) and
    (Length(AClassDef.FComputedInstanceSetters) = 0);
end;

procedure CompileMethodBody(const ACtx: TGocciaCompilationContext;
  const AClassReg: UInt8; const AMethodName: string;
  const AMethod: TGocciaClassMethod; const AStoreOpcode: TSouffleOpCode);
var
  OldTemplate: TSouffleFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TSouffleFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  ChildCtx: TGocciaCompilationContext;
  FuncIdx: UInt16;
  MethodReg: UInt8;
  MethodNameIdx: UInt16;
  FormalCount, RestParamIndex, I: Integer;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TSouffleFunctionTemplate.Create(
    '<method ' + AMethodName + '>');
  ChildTemplate.DebugInfo := TSouffleDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.IsAsync := AMethod.IsAsync;
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);

  ChildScope.DeclareLocal('this', False);
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
  if Assigned(ACtx.FormalParameterCounts) then
    ACtx.FormalParameterCounts.AddOrSetValue(ChildTemplate, FormalCount);

  ACtx.SwapState(ChildTemplate, ChildScope);

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
  const AGetter: TGocciaGetterExpression; const AExtOp: UInt8);
var
  OldTemplate: TSouffleFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TSouffleFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  FuncIdx: UInt16;
  FnReg: UInt8;
  NameIdx: UInt16;
  I: Integer;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TSouffleFunctionTemplate.Create('<get ' + AName + '>');
  ChildTemplate.DebugInfo := TSouffleDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.ParameterCount := 0;
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);
  ChildScope.DeclareLocal('this', False);

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
  FnReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, FnReg, FuncIdx));

  NameIdx := ACtx.Template.AddConstantString(AName);
  if NameIdx > High(UInt8) then
    raise Exception.Create('Constant pool overflow: getter name index exceeds 255');
  if FnReg <> ATargetReg + 1 then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ATargetReg + 1, FnReg, 0));
  EmitInstruction(ACtx, EncodeABC(OP_RT_EXT, ATargetReg, AExtOp, UInt8(NameIdx)));
  ACtx.Scope.FreeRegister;
end;

procedure CompileSetterBody(const ACtx: TGocciaCompilationContext;
  const ATargetReg: UInt8; const AName: string;
  const ASetter: TGocciaSetterExpression; const AExtOp: UInt8);
var
  OldTemplate: TSouffleFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TSouffleFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  FuncIdx: UInt16;
  FnReg: UInt8;
  NameIdx: UInt16;
  I: Integer;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TSouffleFunctionTemplate.Create('<set ' + AName + '>');
  ChildTemplate.DebugInfo := TSouffleDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.ParameterCount := 1;
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);
  ChildScope.DeclareLocal('this', False);
  ChildScope.DeclareLocal(ASetter.Parameter, False);

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
  FnReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, FnReg, FuncIdx));

  NameIdx := ACtx.Template.AddConstantString(AName);
  if NameIdx > High(UInt8) then
    raise Exception.Create('Constant pool overflow: setter name index exceeds 255');
  if FnReg <> ATargetReg + 1 then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ATargetReg + 1, FnReg, 0));
  EmitInstruction(ACtx, EncodeABC(OP_RT_EXT, ATargetReg, AExtOp, UInt8(NameIdx)));
  ACtx.Scope.FreeRegister;
end;

procedure CompileClassDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaClassDeclaration);
var
  ClassDef: TGocciaClassDefinition;
  ClassReg, SuperReg, ValReg: UInt8;
  NameIdx, KeyIdx: UInt16;
  MethodPair: TPair<string, TGocciaClassMethod>;
  GetterPair: TPair<string, TGocciaGetterExpression>;
  SetterPair: TPair<string, TGocciaSetterExpression>;
  StaticPropPair: TPair<string, TGocciaExpression>;
  LocalIdx, UpvalIdx: Integer;
  HasSuper: Boolean;
begin
  ClassDef := AStmt.ClassDefinition;
  HasSuper := ClassDef.SuperClass <> '';

  ClassReg := ACtx.Scope.DeclareLocal(ClassDef.Name, True);
  NameIdx := ACtx.Template.AddConstantString(ClassDef.Name);
  EmitInstruction(ACtx, EncodeABx(OP_NEW_BLUEPRINT, ClassReg, NameIdx));

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
        EmitInstruction(ACtx, EncodeABx(OP_RT_GET_GLOBAL, SuperReg,
          ACtx.Template.AddConstantString(ClassDef.SuperClass)));
    end;

    EmitInstruction(ACtx, EncodeABC(OP_INHERIT, ClassReg, SuperReg, 0));
  end;

  for MethodPair in ClassDef.Methods do
  begin
    if MethodPair.Value.IsStatic then
      CompileMethodBody(ACtx, ClassReg, MethodPair.Key,
        MethodPair.Value, OP_RT_SET_PROP)
    else
      CompileMethodBody(ACtx, ClassReg, MethodPair.Key,
        MethodPair.Value, OP_RECORD_SET);
  end;

  for MethodPair in ClassDef.PrivateMethods do
    CompileMethodBody(ACtx, ClassReg, '#' + MethodPair.Key,
      MethodPair.Value, OP_RECORD_SET);

  for GetterPair in ClassDef.Getters do
    CompileGetterBody(ACtx, ClassReg, GetterPair.Key,
      GetterPair.Value, GOCCIA_EXT_DEF_GETTER);

  for SetterPair in ClassDef.Setters do
    CompileSetterBody(ACtx, ClassReg, SetterPair.Key,
      SetterPair.Value, GOCCIA_EXT_DEF_SETTER);

  for GetterPair in ClassDef.StaticGetters do
    CompileGetterBody(ACtx, ClassReg, GetterPair.Key,
      GetterPair.Value, GOCCIA_EXT_DEF_STATIC_GETTER);

  for SetterPair in ClassDef.StaticSetters do
    CompileSetterBody(ACtx, ClassReg, SetterPair.Key,
      SetterPair.Value, GOCCIA_EXT_DEF_STATIC_SETTER);

  for StaticPropPair in ClassDef.StaticProperties do
  begin
    ValReg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(StaticPropPair.Value, ValReg);
    KeyIdx := ACtx.Template.AddConstantString(StaticPropPair.Key);
    if KeyIdx > High(UInt8) then
      raise Exception.Create('Constant pool overflow: static property name index exceeds 255');
    EmitInstruction(ACtx, EncodeABC(OP_RT_SET_PROP, ClassReg,
      UInt8(KeyIdx), ValReg));
    ACtx.Scope.FreeRegister;
  end;

  for StaticPropPair in ClassDef.PrivateStaticProperties do
  begin
    ValReg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(StaticPropPair.Value, ValReg);
    KeyIdx := ACtx.Template.AddConstantString('#' + StaticPropPair.Key);
    if KeyIdx > High(UInt8) then
      raise Exception.Create('Constant pool overflow: static property name index exceeds 255');
    EmitInstruction(ACtx, EncodeABC(OP_RT_SET_PROP, ClassReg,
      UInt8(KeyIdx), ValReg));
    ACtx.Scope.FreeRegister;
  end;
end;

procedure CompileComplexClassDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaClassDeclaration; const APendingIndex: Integer);
var
  ClassDef: TGocciaClassDefinition;
  ClassReg: UInt8;
  NameIdx: UInt16;
  I: Integer;
  Local: TGocciaCompilerLocal;
begin
  ClassDef := AStmt.ClassDefinition;

  for I := 0 to ACtx.Scope.LocalCount - 1 do
  begin
    Local := ACtx.Scope.GetLocal(I);
    if (Local.Name <> '') and (Local.Name <> '__receiver') and
       (Local.Name <> 'this') then
    begin
      ACtx.Scope.MarkGlobalBacked(I);
      NameIdx := ACtx.Template.AddConstantString(Local.Name);
      EmitInstruction(ACtx, EncodeABC(OP_RT_EXT, Local.Slot,
        GOCCIA_EXT_DEFINE_GLOBAL, UInt8(NameIdx)));
    end;
  end;

  ClassReg := ACtx.Scope.DeclareLocal(ClassDef.Name, True);
  EmitInstruction(ACtx, EncodeABC(OP_RT_EXT, ClassReg,
    GOCCIA_EXT_EVAL_CLASS, UInt8(APendingIndex)));

  NameIdx := ACtx.Template.AddConstantString(ClassDef.Name);
  EmitInstruction(ACtx, EncodeABC(OP_RT_EXT, ClassReg,
    GOCCIA_EXT_DEFINE_GLOBAL, UInt8(NameIdx)));

  for I := 0 to ACtx.Scope.LocalCount - 1 do
  begin
    Local := ACtx.Scope.GetLocal(I);
    if Local.IsGlobalBacked then
    begin
      NameIdx := ACtx.Template.AddConstantString(Local.Name);
      EmitInstruction(ACtx, EncodeABC(OP_RT_EXT, Local.Slot,
        GOCCIA_EXT_DEFINE_GLOBAL, UInt8(NameIdx)));
    end;
  end;
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

procedure CompileEnumDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaEnumDeclaration);
var
  EnumSlot, MemberSlot: UInt8;
  I: Integer;
  KeyIdx: UInt16;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount, J: Integer;
begin
  EnumSlot := ACtx.Scope.DeclareLocal(AStmt.Name, False);
  EmitInstruction(ACtx, EncodeABx(OP_NEW_RECORD, EnumSlot,
    Length(AStmt.Members)));

  ACtx.Scope.BeginScope;

  ACtx.Scope.DeclareLocal(AStmt.Name, False);
  EmitInstruction(ACtx, EncodeABC(OP_MOVE,
    ACtx.Scope.ResolveLocal(AStmt.Name), EnumSlot, 0));

  for I := 0 to High(AStmt.Members) do
  begin
    MemberSlot := ACtx.Scope.DeclareLocal(AStmt.Members[I].Name, False);
    ACtx.CompileExpression(AStmt.Members[I].Initializer, MemberSlot);
    KeyIdx := ACtx.Template.AddConstantString(AStmt.Members[I].Name);
    EmitInstruction(ACtx, EncodeABC(OP_RECORD_SET, EnumSlot, KeyIdx, MemberSlot));
  end;

  EmitInstruction(ACtx, EncodeABC(OP_RT_EXT, EnumSlot,
    GOCCIA_EXT_FINALIZE_ENUM,
    ACtx.Template.AddConstantString(AStmt.Name)));

  ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
  for J := 0 to ClosedCount - 1 do
    EmitInstruction(ACtx, EncodeABx(OP_CLOSE_UPVALUE, ClosedLocals[J], 0));
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
