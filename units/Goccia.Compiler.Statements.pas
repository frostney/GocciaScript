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

implementation

uses
  SysUtils,

  Souffle.Bytecode,

  Goccia.Compiler.Scope;

procedure CompileExpressionStatement(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaExpressionStatement);
var
  Reg: UInt8;
begin
  Reg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AStmt.Expression, Reg);
  ACtx.Scope.FreeRegister;
end;

procedure CompileVariableDeclaration(const ACtx: TGocciaCompilationContext;
  const AStmt: TGocciaVariableDeclaration);
var
  I: Integer;
  Info: TGocciaVariableInfo;
  Slot: UInt8;
begin
  for I := 0 to High(AStmt.Variables) do
  begin
    Info := AStmt.Variables[I];
    Slot := ACtx.Scope.DeclareLocal(Info.Name, AStmt.IsConst);
    if Assigned(Info.Initializer) then
      ACtx.CompileExpression(Info.Initializer, Slot)
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
begin
  if Assigned(AStmt.Value) then
  begin
    Reg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(AStmt.Value, Reg);
    EmitInstruction(ACtx, EncodeABC(OP_RETURN, Reg, 0, 0));
    ACtx.Scope.FreeRegister;
  end
  else
    EmitInstruction(ACtx, EncodeABC(OP_RETURN_NIL, 0, 0, 0));
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
  HandlerJump, EndJump, I: Integer;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount: Integer;
begin
  CatchReg := ACtx.Scope.AllocateRegister;
  HandlerJump := EmitJumpInstruction(ACtx, OP_PUSH_HANDLER, CatchReg);

  CompileBlockStatement(ACtx, AStmt.Block);

  EmitInstruction(ACtx, EncodeABC(OP_POP_HANDLER, 0, 0, 0));
  EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);

  PatchJumpTarget(ACtx, HandlerJump);

  if AStmt.CatchParam <> '' then
  begin
    ACtx.Scope.BeginScope;
    ACtx.Scope.DeclareLocal(AStmt.CatchParam, False);
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ACtx.Scope.NextSlot - 1, CatchReg, 0));
  end;

  if Assigned(AStmt.CatchBlock) then
    CompileBlockStatement(ACtx, AStmt.CatchBlock);

  if AStmt.CatchParam <> '' then
  begin
    ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
    for I := 0 to ClosedCount - 1 do
      EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));
  end;

  PatchJumpTarget(ACtx, EndJump);

  if Assigned(AStmt.FinallyBlock) then
    CompileBlockStatement(ACtx, AStmt.FinallyBlock);

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
    EmitInstruction(ACtx, EncodeABC(OP_RT_GET_PROP, Slot, ModReg, UInt8(NameIdx)));
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

end.
