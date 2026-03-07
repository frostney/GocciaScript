unit Goccia.Compiler.Context;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Souffle.Bytecode,
  Souffle.Bytecode.Chunk,
  Souffle.Bytecode.Debug,
  Souffle.Value,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Compiler.Scope,
  Goccia.Token;

type
  TCompileExpressionProc = procedure(const AExpr: TGocciaExpression;
    const ADest: UInt8) of object;
  TCompileStatementProc = procedure(const AStmt: TGocciaStatement) of object;
  TCompileFunctionBodyProc = procedure(const ABody: TGocciaASTNode) of object;
  TSwapStateProc = procedure(const ATemplate: TSouffleFunctionTemplate;
    const AScope: TGocciaCompilerScope) of object;

  TFormalParameterCountMap = TDictionary<TSouffleFunctionTemplate, Integer>;

  TGocciaCompilationContext = record
    Template: TSouffleFunctionTemplate;
    Scope: TGocciaCompilerScope;
    SourcePath: string;
    FormalParameterCounts: TFormalParameterCountMap;
    CompileExpression: TCompileExpressionProc;
    CompileStatement: TCompileStatementProc;
    CompileFunctionBody: TCompileFunctionBodyProc;
    SwapState: TSwapStateProc;
  end;

function EmitInstruction(const ACtx: TGocciaCompilationContext;
  const AInstruction: UInt32): Integer; inline;
procedure EmitLineMapping(const ACtx: TGocciaCompilationContext;
  const ALine, AColumn: Integer);
function EmitJumpInstruction(const ACtx: TGocciaCompilationContext;
  const AOp: TSouffleOpCode; const AReg: UInt8): Integer; overload;
function EmitJumpInstruction(const ACtx: TGocciaCompilationContext;
  const AOp: TSouffleOpCode; const AReg, AFlags: UInt8): Integer; overload;
procedure PatchJumpTarget(const ACtx: TGocciaCompilationContext;
  const AIndex: Integer);
function CurrentCodePosition(const ACtx: TGocciaCompilationContext): Integer;

function TokenTypeToRuntimeOp(
  const ATokenType: TGocciaTokenType): TSouffleOpCode;
function CompoundOpToRuntimeOp(
  const ATokenType: TGocciaTokenType): TSouffleOpCode;

implementation

function EmitInstruction(const ACtx: TGocciaCompilationContext;
  const AInstruction: UInt32): Integer;
begin
  Result := ACtx.Template.EmitInstruction(AInstruction);
end;

procedure EmitLineMapping(const ACtx: TGocciaCompilationContext;
  const ALine, AColumn: Integer);
begin
  if Assigned(ACtx.Template.DebugInfo) then
    ACtx.Template.DebugInfo.AddLineMapping(
      UInt32(ACtx.Template.CodeCount), UInt32(ALine), UInt16(AColumn));
end;

function EmitJumpInstruction(const ACtx: TGocciaCompilationContext;
  const AOp: TSouffleOpCode; const AReg: UInt8): Integer;
begin
  if AOp = OP_JUMP then
    Result := EmitInstruction(ACtx, EncodeAx(AOp, 0))
  else if AOp = OP_PUSH_HANDLER then
    Result := EmitInstruction(ACtx, EncodeABx(AOp, AReg, 0))
  else if (AOp = OP_JUMP_IF_NIL) or (AOp = OP_JUMP_IF_NOT_NIL) then
    Result := EmitInstruction(ACtx, EncodeABC(AOp, AReg, SOUFFLE_NIL_MATCH_ANY, 0))
  else
    Result := EmitInstruction(ACtx, EncodeAsBx(AOp, AReg, 0));
end;

function EmitJumpInstruction(const ACtx: TGocciaCompilationContext;
  const AOp: TSouffleOpCode; const AReg, AFlags: UInt8): Integer;
begin
  Result := EmitInstruction(ACtx, EncodeABC(AOp, AReg, AFlags, 0));
end;

procedure PatchJumpTarget(const ACtx: TGocciaCompilationContext;
  const AIndex: Integer);
var
  Offset: Integer;
  Instruction: UInt32;
  Op, A, B: UInt8;
begin
  Offset := CurrentCodePosition(ACtx) - AIndex - 1;
  Instruction := ACtx.Template.GetInstruction(AIndex);
  Op := DecodeOp(Instruction);

  if TSouffleOpCode(Op) = OP_JUMP then
    ACtx.Template.PatchInstruction(AIndex, EncodeAx(OP_JUMP, Offset))
  else if TSouffleOpCode(Op) = OP_PUSH_HANDLER then
  begin
    A := DecodeA(Instruction);
    ACtx.Template.PatchInstruction(AIndex,
      EncodeABx(OP_PUSH_HANDLER, A, UInt16(Offset)));
  end
  else if (TSouffleOpCode(Op) = OP_JUMP_IF_NIL) or
          (TSouffleOpCode(Op) = OP_JUMP_IF_NOT_NIL) then
  begin
    A := DecodeA(Instruction);
    B := DecodeB(Instruction);
    Assert(Offset <= 255, 'Nil jump offset exceeds 8-bit range');
    ACtx.Template.PatchInstruction(AIndex,
      EncodeABC(TSouffleOpCode(Op), A, B, UInt8(Offset)));
  end
  else
  begin
    A := DecodeA(Instruction);
    ACtx.Template.PatchInstruction(AIndex,
      EncodeAsBx(TSouffleOpCode(Op), A, Int16(Offset)));
  end;
end;

function CurrentCodePosition(const ACtx: TGocciaCompilationContext): Integer;
begin
  Result := ACtx.Template.CodeCount;
end;

function TokenTypeToRuntimeOp(
  const ATokenType: TGocciaTokenType): TSouffleOpCode;
begin
  case ATokenType of
    gttPlus:               Result := OP_RT_ADD;
    gttMinus:              Result := OP_RT_SUB;
    gttStar:               Result := OP_RT_MUL;
    gttSlash:              Result := OP_RT_DIV;
    gttPercent:            Result := OP_RT_MOD;
    gttPower:              Result := OP_RT_POW;
    gttEqual:              Result := OP_RT_EQ;
    gttNotEqual:           Result := OP_RT_NEQ;
    gttLess:               Result := OP_RT_LT;
    gttGreater:            Result := OP_RT_GT;
    gttLessEqual:          Result := OP_RT_LTE;
    gttGreaterEqual:       Result := OP_RT_GTE;
    gttInstanceof:         Result := OP_RT_IS_INSTANCE;
    gttIn:                 Result := OP_RT_HAS_PROPERTY;
    gttBitwiseAnd:         Result := OP_RT_BAND;
    gttBitwiseOr:          Result := OP_RT_BOR;
    gttBitwiseXor:         Result := OP_RT_BXOR;
    gttLeftShift:          Result := OP_RT_SHL;
    gttRightShift:         Result := OP_RT_SHR;
    gttUnsignedRightShift: Result := OP_RT_USHR;
  else
    Result := OP_RT_ADD;
  end;
end;

function CompoundOpToRuntimeOp(
  const ATokenType: TGocciaTokenType): TSouffleOpCode;
begin
  case ATokenType of
    gttPlusAssign:               Result := OP_RT_ADD;
    gttMinusAssign:              Result := OP_RT_SUB;
    gttStarAssign:               Result := OP_RT_MUL;
    gttSlashAssign:              Result := OP_RT_DIV;
    gttPercentAssign:            Result := OP_RT_MOD;
    gttPowerAssign:              Result := OP_RT_POW;
    gttBitwiseAndAssign:         Result := OP_RT_BAND;
    gttBitwiseOrAssign:          Result := OP_RT_BOR;
    gttBitwiseXorAssign:         Result := OP_RT_BXOR;
    gttLeftShiftAssign:          Result := OP_RT_SHL;
    gttRightShiftAssign:         Result := OP_RT_SHR;
    gttUnsignedRightShiftAssign: Result := OP_RT_USHR;
  else
    Result := OP_RT_ADD;
  end;
end;

end.
