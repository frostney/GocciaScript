unit Goccia.Compiler.Context;

{$I Goccia.inc}

interface

uses
  SysUtils,

  HashMap,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Bytecode,
  Goccia.Bytecode.Chunk,
  Goccia.Bytecode.Debug,
  Goccia.Compiler.Scope,
  Goccia.Token;

type
  TCompileExpressionProc = procedure(const AExpr: TGocciaExpression;
    const ADest: UInt8) of object;
  TCompileStatementProc = procedure(const AStmt: TGocciaStatement) of object;
  TCompileFunctionBodyProc = procedure(const ABody: TGocciaASTNode) of object;
  TSwapStateProc = procedure(const ATemplate: TGocciaFunctionTemplate;
    const AScope: TGocciaCompilerScope) of object;

  TFormalParameterCountMap = THashMap<TGocciaFunctionTemplate, Integer>;

  TGocciaCompilationContext = record
    Template: TGocciaFunctionTemplate;
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
  const AOp: TGocciaOpCode; const AReg: UInt8): Integer; overload;
function EmitJumpInstruction(const ACtx: TGocciaCompilationContext;
  const AOp: TGocciaOpCode; const AReg, AFlags: UInt8): Integer; overload;
procedure PatchJumpTarget(const ACtx: TGocciaCompilationContext;
  const AIndex: Integer);
function CurrentCodePosition(const ACtx: TGocciaCompilationContext): Integer;

function TokenTypeToRuntimeOp(
  const ATokenType: TGocciaTokenType): TGocciaOpCode;
function CompoundOpToRuntimeOp(
  const ATokenType: TGocciaTokenType): TGocciaOpCode;

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
  const AOp: TGocciaOpCode; const AReg: UInt8): Integer;
begin
  if AOp = OP_JUMP then
    Result := EmitInstruction(ACtx, EncodeAx(AOp, 0))
  else if AOp = OP_PUSH_HANDLER then
    Result := EmitInstruction(ACtx, EncodeABx(AOp, AReg, 0))
  else if (AOp = OP_JUMP_IF_NULLISH) or (AOp = OP_JUMP_IF_NOT_NULLISH) then
    Result := EmitInstruction(ACtx, EncodeABC(AOp, AReg, GOCCIA_NULLISH_MATCH_ANY, 0))
  else
    Result := EmitInstruction(ACtx, EncodeAsBx(AOp, AReg, 0));
end;

function EmitJumpInstruction(const ACtx: TGocciaCompilationContext;
  const AOp: TGocciaOpCode; const AReg, AFlags: UInt8): Integer;
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

  if TGocciaOpCode(Op) = OP_JUMP then
    ACtx.Template.PatchInstruction(AIndex, EncodeAx(OP_JUMP, Offset))
  else if TGocciaOpCode(Op) = OP_PUSH_HANDLER then
  begin
    A := DecodeA(Instruction);
    ACtx.Template.PatchInstruction(AIndex,
      EncodeABx(OP_PUSH_HANDLER, A, UInt16(Offset)));
  end
  else if (TGocciaOpCode(Op) = OP_JUMP_IF_NULLISH) or
          (TGocciaOpCode(Op) = OP_JUMP_IF_NOT_NULLISH) then
  begin
    A := DecodeA(Instruction);
    B := DecodeB(Instruction);
    if Offset > High(UInt8) then
      raise Exception.Create('Nullish jump offset exceeds 8-bit range');
    ACtx.Template.PatchInstruction(AIndex,
      EncodeABC(TGocciaOpCode(Op), A, B, UInt8(Offset)));
  end
  else
  begin
    A := DecodeA(Instruction);
    ACtx.Template.PatchInstruction(AIndex,
      EncodeAsBx(TGocciaOpCode(Op), A, Int16(Offset)));
  end;
end;

function CurrentCodePosition(const ACtx: TGocciaCompilationContext): Integer;
begin
  Result := ACtx.Template.CodeCount;
end;

function TokenTypeToRuntimeOp(
  const ATokenType: TGocciaTokenType): TGocciaOpCode;
begin
  case ATokenType of
    gttPlus:               Result := OP_ADD;
    gttMinus:              Result := OP_SUB;
    gttStar:               Result := OP_MUL;
    gttSlash:              Result := OP_DIV;
    gttPercent:            Result := OP_MOD;
    gttPower:              Result := OP_POW;
    gttEqual:              Result := OP_EQ;
    gttNotEqual:           Result := OP_NEQ;
    gttLess:               Result := OP_LT;
    gttGreater:            Result := OP_GT;
    gttLessEqual:          Result := OP_LTE;
    gttGreaterEqual:       Result := OP_GTE;
    gttInstanceof:         Result := OP_IS_INSTANCE;
    gttIn:                 Result := OP_HAS_PROPERTY;
    gttBitwiseAnd:         Result := OP_BAND;
    gttBitwiseOr:          Result := OP_BOR;
    gttBitwiseXor:         Result := OP_BXOR;
    gttLeftShift:          Result := OP_SHL;
    gttRightShift:         Result := OP_SHR;
    gttUnsignedRightShift: Result := OP_USHR;
  else
    Result := OP_ADD;
  end;
end;

function CompoundOpToRuntimeOp(
  const ATokenType: TGocciaTokenType): TGocciaOpCode;
begin
  case ATokenType of
    gttPlusAssign:               Result := OP_ADD;
    gttMinusAssign:              Result := OP_SUB;
    gttStarAssign:               Result := OP_MUL;
    gttSlashAssign:              Result := OP_DIV;
    gttPercentAssign:            Result := OP_MOD;
    gttPowerAssign:              Result := OP_POW;
    gttBitwiseAndAssign:         Result := OP_BAND;
    gttBitwiseOrAssign:          Result := OP_BOR;
    gttBitwiseXorAssign:         Result := OP_BXOR;
    gttLeftShiftAssign:          Result := OP_SHL;
    gttRightShiftAssign:         Result := OP_SHR;
    gttUnsignedRightShiftAssign: Result := OP_USHR;
  else
    Result := OP_ADD;
  end;
end;

end.
