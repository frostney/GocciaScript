unit Goccia.Compiler;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  Souffle.Bytecode,
  Souffle.Bytecode.Chunk,
  Souffle.Bytecode.Debug,
  Souffle.Bytecode.Module,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Compiler.Scope,
  Goccia.Token,
  Goccia.Values.Primitives;

type
  TGocciaCompilerClassEntry = record
    ClassDeclaration: TGocciaClassDeclaration;
    Line: Integer;
    Column: Integer;
  end;

  TGocciaCompiler = class
  private
    FModule: TSouffleBytecodeModule;
    FCurrentPrototype: TSouffleFunctionPrototype;
    FCurrentScope: TGocciaCompilerScope;
    FSourcePath: string;
    FPendingClasses: array of TGocciaCompilerClassEntry;

    procedure CompileExpression(const AExpr: TGocciaExpression; const ADest: UInt8);
    procedure CompileStatement(const AStmt: TGocciaStatement);

    procedure CompileLiteral(const AExpr: TGocciaLiteralExpression; const ADest: UInt8);
    procedure CompileIdentifier(const AExpr: TGocciaIdentifierExpression; const ADest: UInt8);
    procedure CompileBinary(const AExpr: TGocciaBinaryExpression; const ADest: UInt8);
    procedure CompileUnary(const AExpr: TGocciaUnaryExpression; const ADest: UInt8);
    procedure CompileAssignment(const AExpr: TGocciaAssignmentExpression; const ADest: UInt8);
    procedure CompilePropertyAssignment(const AExpr: TGocciaPropertyAssignmentExpression; const ADest: UInt8);
    procedure CompileArrowFunction(const AExpr: TGocciaArrowFunctionExpression; const ADest: UInt8);
    procedure CompileCall(const AExpr: TGocciaCallExpression; const ADest: UInt8);
    procedure CompileMember(const AExpr: TGocciaMemberExpression; const ADest: UInt8);
    procedure CompileConditional(const AExpr: TGocciaConditionalExpression; const ADest: UInt8);
    procedure CompileArray(const AExpr: TGocciaArrayExpression; const ADest: UInt8);
    procedure CompileObject(const AExpr: TGocciaObjectExpression; const ADest: UInt8);
    procedure CompileTemplateLiteral(const AExpr: TGocciaTemplateLiteralExpression; const ADest: UInt8);
    procedure CompileTemplateWithInterpolation(const AExpr: TGocciaTemplateWithInterpolationExpression; const ADest: UInt8);
    procedure CompileNewExpression(const AExpr: TGocciaNewExpression; const ADest: UInt8);

    procedure CompileExpressionStatement(const AStmt: TGocciaExpressionStatement);
    procedure CompileVariableDeclaration(const AStmt: TGocciaVariableDeclaration);
    procedure CompileBlockStatement(const AStmt: TGocciaBlockStatement);
    procedure CompileIfStatement(const AStmt: TGocciaIfStatement);
    procedure CompileReturnStatement(const AStmt: TGocciaReturnStatement);
    procedure CompileThrowStatement(const AStmt: TGocciaThrowStatement);
    procedure CompileTryStatement(const AStmt: TGocciaTryStatement);
    procedure CompileForOfStatement(const AStmt: TGocciaForOfStatement);
    procedure CompileImportDeclaration(const AStmt: TGocciaImportDeclaration);
    procedure CompileExportDeclaration(const AStmt: TGocciaExportDeclaration);

    function Emit(const AInstruction: UInt32): Integer;
    procedure EmitLine(const ALine, AColumn: Integer);
    function EmitJump(const AOp: TSouffleOpCode; const AReg: UInt8): Integer;
    procedure PatchJump(const AIndex: Integer);
    function CurrentCodeCount: Integer;

    procedure CompileFunctionBody(const ABody: TGocciaASTNode);
    function TokenTypeToRTOp(const ATokenType: TGocciaTokenType): TSouffleOpCode;
  public
    constructor Create(const ASourcePath: string);
    destructor Destroy; override;

    function Compile(const AProgram: TGocciaProgram): TSouffleBytecodeModule;
    function PendingClassCount: Integer;
    function GetPendingClass(const AIndex: Integer): TGocciaCompilerClassEntry;
  end;

const
  GOCCIA_RUNTIME_TAG = 'goccia-js';
  COMPOUND_TAG_OBJECT = 0;
  COMPOUND_TAG_ARRAY  = 1;

implementation

uses
  SysUtils,

  Goccia.Lexer,
  Goccia.Parser;

{ TGocciaCompiler }

constructor TGocciaCompiler.Create(const ASourcePath: string);
begin
  inherited Create;
  FSourcePath := ASourcePath;
  FModule := nil;
  FCurrentPrototype := nil;
  FCurrentScope := nil;
end;

destructor TGocciaCompiler.Destroy;
begin
  inherited;
end;

function TGocciaCompiler.Compile(
  const AProgram: TGocciaProgram): TSouffleBytecodeModule;
var
  I: Integer;
begin
  FModule := TSouffleBytecodeModule.Create(GOCCIA_RUNTIME_TAG, FSourcePath);
  FCurrentPrototype := TSouffleFunctionPrototype.Create('<module>');
  FCurrentPrototype.DebugInfo := TSouffleDebugInfo.Create(FSourcePath);
  FCurrentScope := TGocciaCompilerScope.Create(nil, 0);

  for I := 0 to AProgram.Body.Count - 1 do
    CompileStatement(AProgram.Body[I]);

  Emit(EncodeABC(OP_RETURN_NIL, 0, 0, 0));

  FCurrentPrototype.MaxRegisters := FCurrentScope.MaxSlot;
  FModule.TopLevel := FCurrentPrototype;

  FCurrentScope.Free;
  FCurrentScope := nil;
  FCurrentPrototype := nil;

  Result := FModule;
  FModule := nil;
end;

{ Expression dispatch }

procedure TGocciaCompiler.CompileExpression(const AExpr: TGocciaExpression;
  const ADest: UInt8);
begin
  EmitLine(AExpr.Line, AExpr.Column);

  if AExpr is TGocciaLiteralExpression then
    CompileLiteral(TGocciaLiteralExpression(AExpr), ADest)
  else if AExpr is TGocciaIdentifierExpression then
    CompileIdentifier(TGocciaIdentifierExpression(AExpr), ADest)
  else if AExpr is TGocciaBinaryExpression then
    CompileBinary(TGocciaBinaryExpression(AExpr), ADest)
  else if AExpr is TGocciaUnaryExpression then
    CompileUnary(TGocciaUnaryExpression(AExpr), ADest)
  else if AExpr is TGocciaAssignmentExpression then
    CompileAssignment(TGocciaAssignmentExpression(AExpr), ADest)
  else if AExpr is TGocciaPropertyAssignmentExpression then
    CompilePropertyAssignment(TGocciaPropertyAssignmentExpression(AExpr), ADest)
  else if AExpr is TGocciaArrowFunctionExpression then
    CompileArrowFunction(TGocciaArrowFunctionExpression(AExpr), ADest)
  else if AExpr is TGocciaCallExpression then
    CompileCall(TGocciaCallExpression(AExpr), ADest)
  else if AExpr is TGocciaMemberExpression then
    CompileMember(TGocciaMemberExpression(AExpr), ADest)
  else if AExpr is TGocciaConditionalExpression then
    CompileConditional(TGocciaConditionalExpression(AExpr), ADest)
  else if AExpr is TGocciaArrayExpression then
    CompileArray(TGocciaArrayExpression(AExpr), ADest)
  else if AExpr is TGocciaObjectExpression then
    CompileObject(TGocciaObjectExpression(AExpr), ADest)
  else if AExpr is TGocciaTemplateWithInterpolationExpression then
    CompileTemplateWithInterpolation(TGocciaTemplateWithInterpolationExpression(AExpr), ADest)
  else if AExpr is TGocciaTemplateLiteralExpression then
    CompileTemplateLiteral(TGocciaTemplateLiteralExpression(AExpr), ADest)
  else if AExpr is TGocciaNewExpression then
    CompileNewExpression(TGocciaNewExpression(AExpr), ADest)
  else
    Emit(EncodeABC(OP_LOAD_NIL, ADest, 0, 0));
end;

{ Statement dispatch }

procedure TGocciaCompiler.CompileStatement(const AStmt: TGocciaStatement);
begin
  EmitLine(AStmt.Line, AStmt.Column);

  if AStmt is TGocciaExpressionStatement then
    CompileExpressionStatement(TGocciaExpressionStatement(AStmt))
  else if AStmt is TGocciaVariableDeclaration then
    CompileVariableDeclaration(TGocciaVariableDeclaration(AStmt))
  else if AStmt is TGocciaBlockStatement then
    CompileBlockStatement(TGocciaBlockStatement(AStmt))
  else if AStmt is TGocciaIfStatement then
    CompileIfStatement(TGocciaIfStatement(AStmt))
  else if AStmt is TGocciaReturnStatement then
    CompileReturnStatement(TGocciaReturnStatement(AStmt))
  else if AStmt is TGocciaThrowStatement then
    CompileThrowStatement(TGocciaThrowStatement(AStmt))
  else if AStmt is TGocciaTryStatement then
    CompileTryStatement(TGocciaTryStatement(AStmt))
  else if AStmt is TGocciaForOfStatement then
    CompileForOfStatement(TGocciaForOfStatement(AStmt))
  else if AStmt is TGocciaClassDeclaration then
  begin
    SetLength(FPendingClasses, Length(FPendingClasses) + 1);
    FPendingClasses[High(FPendingClasses)].ClassDeclaration :=
      TGocciaClassDeclaration(AStmt);
    FPendingClasses[High(FPendingClasses)].Line := AStmt.Line;
    FPendingClasses[High(FPendingClasses)].Column := AStmt.Column;
  end
  else if AStmt is TGocciaImportDeclaration then
    CompileImportDeclaration(TGocciaImportDeclaration(AStmt))
  else if AStmt is TGocciaExportDeclaration then
    CompileExportDeclaration(TGocciaExportDeclaration(AStmt));
end;

{ Expression compilation }

procedure TGocciaCompiler.CompileLiteral(
  const AExpr: TGocciaLiteralExpression; const ADest: UInt8);
var
  Value: TGocciaValue;
  Idx: UInt16;
begin
  Value := AExpr.Value;

  if Value is TGocciaUndefinedLiteralValue then
    Emit(EncodeABC(OP_LOAD_NIL, ADest, 0, 0))
  else if Value is TGocciaNullLiteralValue then
    Emit(EncodeABx(OP_RT_GET_GLOBAL, ADest,
      FCurrentPrototype.AddConstantString('null')))
  else if Value is TGocciaBooleanLiteralValue then
  begin
    if TGocciaBooleanLiteralValue(Value).Value then
      Emit(EncodeABC(OP_LOAD_TRUE, ADest, 0, 0))
    else
      Emit(EncodeABC(OP_LOAD_FALSE, ADest, 0, 0));
  end
  else if Value is TGocciaNumberLiteralValue then
  begin
    if not TGocciaNumberLiteralValue(Value).IsNaN
       and not TGocciaNumberLiteralValue(Value).IsInfinite
       and (Frac(TGocciaNumberLiteralValue(Value).Value) = 0.0)
       and (TGocciaNumberLiteralValue(Value).Value >= -32768)
       and (TGocciaNumberLiteralValue(Value).Value <= 32767) then
      Emit(EncodeAsBx(OP_LOAD_INT, ADest,
        Int16(Trunc(TGocciaNumberLiteralValue(Value).Value))))
    else
    begin
      Idx := FCurrentPrototype.AddConstantFloat(
        TGocciaNumberLiteralValue(Value).Value);
      Emit(EncodeABx(OP_LOAD_CONST, ADest, Idx));
    end;
  end
  else if Value is TGocciaStringLiteralValue then
  begin
    Idx := FCurrentPrototype.AddConstantString(
      TGocciaStringLiteralValue(Value).Value);
    Emit(EncodeABx(OP_LOAD_CONST, ADest, Idx));
  end
  else
    Emit(EncodeABC(OP_LOAD_NIL, ADest, 0, 0));
end;

procedure TGocciaCompiler.CompileIdentifier(
  const AExpr: TGocciaIdentifierExpression; const ADest: UInt8);
var
  LocalIdx, UpvalIdx: Integer;
  Slot: UInt8;
begin
  LocalIdx := FCurrentScope.ResolveLocal(AExpr.Name);
  if LocalIdx >= 0 then
  begin
    Slot := FCurrentScope.GetLocal(LocalIdx).Slot;
    if Slot <> ADest then
      Emit(EncodeABC(OP_MOVE, ADest, Slot, 0));
    Exit;
  end;

  UpvalIdx := FCurrentScope.ResolveUpvalue(AExpr.Name);
  if UpvalIdx >= 0 then
  begin
    Emit(EncodeABx(OP_GET_UPVALUE, ADest, UInt16(UpvalIdx)));
    Exit;
  end;

  Emit(EncodeABx(OP_RT_GET_GLOBAL, ADest,
    FCurrentPrototype.AddConstantString(AExpr.Name)));
end;

procedure TGocciaCompiler.CompileBinary(
  const AExpr: TGocciaBinaryExpression; const ADest: UInt8);
var
  RegB, RegC: UInt8;
  Op: TSouffleOpCode;
  JumpIdx, JumpIdx2: Integer;
begin
  if AExpr.Operator = gttAnd then
  begin
    CompileExpression(AExpr.Left, ADest);
    JumpIdx := EmitJump(OP_JUMP_IF_FALSE, ADest);
    CompileExpression(AExpr.Right, ADest);
    PatchJump(JumpIdx);
    Exit;
  end;

  if AExpr.Operator = gttOr then
  begin
    CompileExpression(AExpr.Left, ADest);
    JumpIdx := EmitJump(OP_JUMP_IF_TRUE, ADest);
    CompileExpression(AExpr.Right, ADest);
    PatchJump(JumpIdx);
    Exit;
  end;

  if AExpr.Operator = gttNullishCoalescing then
  begin
    CompileExpression(AExpr.Left, ADest);

    JumpIdx := EmitJump(OP_JUMP_IF_NOT_NIL, ADest);
    JumpIdx2 := EmitJump(OP_JUMP, 0);

    PatchJump(JumpIdx);
    RegB := FCurrentScope.AllocateRegister;
    RegC := FCurrentScope.AllocateRegister;
    Emit(EncodeABx(OP_RT_GET_GLOBAL, RegB,
      FCurrentPrototype.AddConstantString('null')));
    Emit(EncodeABC(OP_RT_EQ, RegC, ADest, RegB));
    FCurrentScope.FreeRegister;
    FCurrentScope.FreeRegister;
    JumpIdx := EmitJump(OP_JUMP_IF_FALSE, RegC);

    PatchJump(JumpIdx2);
    CompileExpression(AExpr.Right, ADest);

    PatchJump(JumpIdx);
    Exit;
  end;

  RegB := FCurrentScope.AllocateRegister;
  RegC := FCurrentScope.AllocateRegister;

  CompileExpression(AExpr.Left, RegB);
  CompileExpression(AExpr.Right, RegC);

  Op := TokenTypeToRTOp(AExpr.Operator);
  Emit(EncodeABC(Op, ADest, RegB, RegC));

  FCurrentScope.FreeRegister;
  FCurrentScope.FreeRegister;
end;

procedure TGocciaCompiler.CompileUnary(
  const AExpr: TGocciaUnaryExpression; const ADest: UInt8);
var
  RegB: UInt8;
begin
  RegB := FCurrentScope.AllocateRegister;
  CompileExpression(AExpr.Operand, RegB);

  case AExpr.Operator of
    gttNot:        Emit(EncodeABC(OP_RT_NOT, ADest, RegB, 0));
    gttMinus:      Emit(EncodeABC(OP_RT_NEG, ADest, RegB, 0));
    gttTypeof:     Emit(EncodeABC(OP_RT_TYPEOF, ADest, RegB, 0));
    gttBitwiseNot: Emit(EncodeABC(OP_RT_BNOT, ADest, RegB, 0));
  else
    Emit(EncodeABC(OP_LOAD_NIL, ADest, 0, 0));
  end;

  FCurrentScope.FreeRegister;
end;

procedure TGocciaCompiler.CompileAssignment(
  const AExpr: TGocciaAssignmentExpression; const ADest: UInt8);
var
  LocalIdx, UpvalIdx: Integer;
  Slot: UInt8;
begin
  CompileExpression(AExpr.Value, ADest);

  LocalIdx := FCurrentScope.ResolveLocal(AExpr.Name);
  if LocalIdx >= 0 then
  begin
    Slot := FCurrentScope.GetLocal(LocalIdx).Slot;
    if ADest <> Slot then
      Emit(EncodeABC(OP_MOVE, Slot, ADest, 0));
    Exit;
  end;

  UpvalIdx := FCurrentScope.ResolveUpvalue(AExpr.Name);
  if UpvalIdx >= 0 then
  begin
    Emit(EncodeABx(OP_SET_UPVALUE, ADest, UInt16(UpvalIdx)));
    Exit;
  end;

  Emit(EncodeABx(OP_RT_SET_GLOBAL, ADest,
    FCurrentPrototype.AddConstantString(AExpr.Name)));
end;

procedure TGocciaCompiler.CompilePropertyAssignment(
  const AExpr: TGocciaPropertyAssignmentExpression; const ADest: UInt8);
var
  ObjReg, ValReg: UInt8;
  KeyIdx: UInt16;
begin
  ObjReg := FCurrentScope.AllocateRegister;
  ValReg := FCurrentScope.AllocateRegister;

  CompileExpression(AExpr.ObjectExpr, ObjReg);
  CompileExpression(AExpr.Value, ValReg);

  KeyIdx := FCurrentPrototype.AddConstantString(AExpr.PropertyName);
  if KeyIdx > High(UInt8) then
    raise Exception.Create('Constant pool overflow: property name index exceeds 255');
  Emit(EncodeABC(OP_RT_SET_PROP, ObjReg, KeyIdx, ValReg));

  if ADest <> ValReg then
    Emit(EncodeABC(OP_MOVE, ADest, ValReg, 0));

  FCurrentScope.FreeRegister;
  FCurrentScope.FreeRegister;
end;

procedure TGocciaCompiler.CompileArrowFunction(
  const AExpr: TGocciaArrowFunctionExpression; const ADest: UInt8);
var
  OldProto: TSouffleFunctionPrototype;
  OldScope: TGocciaCompilerScope;
  ChildProto: TSouffleFunctionPrototype;
  ChildScope: TGocciaCompilerScope;
  FuncIdx: UInt16;
  I: Integer;
begin
  OldProto := FCurrentPrototype;
  OldScope := FCurrentScope;

  ChildProto := TSouffleFunctionPrototype.Create('<arrow>');
  ChildProto.DebugInfo := TSouffleDebugInfo.Create(FSourcePath);
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);

  ChildProto.ParameterCount := Length(AExpr.Parameters);
  for I := 0 to High(AExpr.Parameters) do
    if not AExpr.Parameters[I].IsPattern then
      ChildScope.DeclareLocal(AExpr.Parameters[I].Name, False);

  FCurrentPrototype := ChildProto;
  FCurrentScope := ChildScope;

  CompileFunctionBody(AExpr.Body);

  ChildProto.MaxRegisters := ChildScope.MaxSlot;

  for I := 0 to ChildScope.UpvalueCount - 1 do
    ChildProto.AddUpvalueDescriptor(
      ChildScope.GetUpvalue(I).IsLocal,
      ChildScope.GetUpvalue(I).Index);

  FCurrentPrototype := OldProto;
  FCurrentScope := OldScope;
  ChildScope.Free;

  FuncIdx := FCurrentPrototype.AddFunction(ChildProto);
  Emit(EncodeABx(OP_CLOSURE, ADest, FuncIdx));
end;

procedure TGocciaCompiler.CompileFunctionBody(const ABody: TGocciaASTNode);
var
  Block: TGocciaBlockStatement;
  I: Integer;
  Node: TGocciaASTNode;
  Reg: UInt8;
begin
  if ABody is TGocciaBlockStatement then
  begin
    Block := TGocciaBlockStatement(ABody);
    for I := 0 to Block.Nodes.Count - 1 do
    begin
      Node := Block.Nodes[I];
      if Node is TGocciaStatement then
        CompileStatement(TGocciaStatement(Node))
      else if Node is TGocciaExpression then
      begin
        Reg := FCurrentScope.AllocateRegister;
        CompileExpression(TGocciaExpression(Node), Reg);
        FCurrentScope.FreeRegister;
      end;
    end;
    Emit(EncodeABC(OP_RETURN_NIL, 0, 0, 0));
  end
  else if ABody is TGocciaExpression then
  begin
    Reg := FCurrentScope.AllocateRegister;
    CompileExpression(TGocciaExpression(ABody), Reg);
    Emit(EncodeABC(OP_RETURN, Reg, 0, 0));
    FCurrentScope.FreeRegister;
  end
  else
    Emit(EncodeABC(OP_RETURN_NIL, 0, 0, 0));
end;

procedure TGocciaCompiler.CompileCall(
  const AExpr: TGocciaCallExpression; const ADest: UInt8);
var
  ArgCount, I: Integer;
  BaseReg, ObjReg: UInt8;
  MemberExpr: TGocciaMemberExpression;
  PropIdx: UInt16;
begin
  ArgCount := AExpr.Arguments.Count;

  if AExpr.Callee is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(AExpr.Callee);
    ObjReg := FCurrentScope.AllocateRegister;
    BaseReg := FCurrentScope.AllocateRegister;

    CompileExpression(MemberExpr.ObjectExpr, ObjReg);

    if MemberExpr.Computed then
    begin
      CompileExpression(MemberExpr.PropertyExpression, BaseReg);
      Emit(EncodeABC(OP_RT_GET_INDEX, BaseReg, ObjReg, BaseReg));
    end
    else
    begin
      PropIdx := FCurrentPrototype.AddConstantString(MemberExpr.PropertyName);
      if PropIdx > High(UInt8) then
        raise Exception.Create('Constant pool overflow: property name index exceeds 255');
      Emit(EncodeABC(OP_RT_GET_PROP, BaseReg, ObjReg, UInt8(PropIdx)));
    end;

    for I := 0 to ArgCount - 1 do
      CompileExpression(AExpr.Arguments[I], FCurrentScope.AllocateRegister);

    Emit(EncodeABC(OP_RT_CALL_METHOD, BaseReg, UInt8(ArgCount), 0));

    for I := 0 to ArgCount - 1 do
      FCurrentScope.FreeRegister;

    if ADest <> BaseReg then
      Emit(EncodeABC(OP_MOVE, ADest, BaseReg, 0));

    FCurrentScope.FreeRegister;
    FCurrentScope.FreeRegister;
  end
  else
  begin
    BaseReg := FCurrentScope.AllocateRegister;

    CompileExpression(AExpr.Callee, BaseReg);

    for I := 0 to ArgCount - 1 do
      CompileExpression(AExpr.Arguments[I], FCurrentScope.AllocateRegister);

    Emit(EncodeABC(OP_RT_CALL, BaseReg, UInt8(ArgCount), 0));

    for I := 0 to ArgCount - 1 do
      FCurrentScope.FreeRegister;

    if ADest <> BaseReg then
      Emit(EncodeABC(OP_MOVE, ADest, BaseReg, 0));

    FCurrentScope.FreeRegister;
  end;
end;

procedure TGocciaCompiler.CompileMember(
  const AExpr: TGocciaMemberExpression; const ADest: UInt8);
var
  ObjReg: UInt8;
  PropIdx: UInt16;
begin
  ObjReg := FCurrentScope.AllocateRegister;
  CompileExpression(AExpr.ObjectExpr, ObjReg);

  if AExpr.Computed then
  begin
    CompileExpression(AExpr.PropertyExpression, ADest);
    Emit(EncodeABC(OP_RT_GET_INDEX, ADest, ObjReg, ADest));
  end
  else
  begin
    PropIdx := FCurrentPrototype.AddConstantString(AExpr.PropertyName);
    if PropIdx > High(UInt8) then
      raise Exception.Create('Constant pool overflow: property name index exceeds 255');
    Emit(EncodeABC(OP_RT_GET_PROP, ADest, ObjReg, UInt8(PropIdx)));
  end;

  FCurrentScope.FreeRegister;
end;

procedure TGocciaCompiler.CompileConditional(
  const AExpr: TGocciaConditionalExpression; const ADest: UInt8);
var
  ElseJump, EndJump: Integer;
begin
  CompileExpression(AExpr.Condition, ADest);
  ElseJump := EmitJump(OP_JUMP_IF_FALSE, ADest);
  CompileExpression(AExpr.Consequent, ADest);
  EndJump := EmitJump(OP_JUMP, 0);
  PatchJump(ElseJump);
  CompileExpression(AExpr.Alternate, ADest);
  PatchJump(EndJump);
end;

procedure TGocciaCompiler.CompileArray(
  const AExpr: TGocciaArrayExpression; const ADest: UInt8);
var
  I: Integer;
  ElemReg, IdxReg: UInt8;
begin
  Emit(EncodeABC(OP_RT_NEW_COMPOUND, ADest, COMPOUND_TAG_ARRAY, 0));

  for I := 0 to AExpr.Elements.Count - 1 do
  begin
    ElemReg := FCurrentScope.AllocateRegister;
    IdxReg := FCurrentScope.AllocateRegister;
    CompileExpression(AExpr.Elements[I], ElemReg);
    Emit(EncodeAsBx(OP_LOAD_INT, IdxReg, Int16(I)));
    Emit(EncodeABC(OP_RT_INIT_INDEX, ADest, IdxReg, ElemReg));
    FCurrentScope.FreeRegister;
    FCurrentScope.FreeRegister;
  end;
end;

procedure TGocciaCompiler.CompileObject(
  const AExpr: TGocciaObjectExpression; const ADest: UInt8);
var
  I: Integer;
  Key: string;
  ValExpr: TGocciaExpression;
  ValReg: UInt8;
  KeyIdx: UInt16;
  Names: TStringList;
begin
  Emit(EncodeABC(OP_RT_NEW_COMPOUND, ADest, COMPOUND_TAG_OBJECT, 0));

  Names := AExpr.GetPropertyNamesInOrder;
  try
    for I := 0 to Names.Count - 1 do
    begin
      Key := Names[I];
      if AExpr.Properties.TryGetValue(Key, ValExpr) then
      begin
        ValReg := FCurrentScope.AllocateRegister;
        CompileExpression(ValExpr, ValReg);
        KeyIdx := FCurrentPrototype.AddConstantString(Key);
        if KeyIdx > High(UInt8) then
          raise Exception.Create('Constant pool overflow: property name index exceeds 255');
        Emit(EncodeABC(OP_RT_INIT_FIELD, ADest, KeyIdx, ValReg));
        FCurrentScope.FreeRegister;
      end;
    end;
  finally
    Names.Free;
  end;
end;

procedure TGocciaCompiler.CompileTemplateLiteral(
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
    Emit(EncodeABx(OP_LOAD_CONST, ADest,
      FCurrentPrototype.AddConstantString(Template)));
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
        Lexer := TGocciaLexer.Create(ExprText + ';', FSourcePath);
        try
          Tokens := Lexer.ScanTokens;
          SourceLines := TStringList.Create;
          try
            SourceLines.Text := ExprText;
            Parser := TGocciaParser.Create(Tokens, FSourcePath, SourceLines);
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
                    CompileExpression(ParsedExpr, ADest);
                    HasParts := True;
                  end
                  else
                  begin
                    PartReg := FCurrentScope.AllocateRegister;
                    CompileExpression(ParsedExpr, PartReg);
                    Emit(EncodeABC(OP_RT_ADD, ADest, ADest, PartReg));
                    FCurrentScope.FreeRegister;
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
          Emit(EncodeABx(OP_LOAD_CONST, ADest,
            FCurrentPrototype.AddConstantString(StaticPart)));
          HasParts := True;
        end
        else
        begin
          PartReg := FCurrentScope.AllocateRegister;
          Emit(EncodeABx(OP_LOAD_CONST, PartReg,
            FCurrentPrototype.AddConstantString(StaticPart)));
          Emit(EncodeABC(OP_RT_ADD, ADest, ADest, PartReg));
          FCurrentScope.FreeRegister;
        end;
      end;
    end;
  end;

  if not HasParts then
    Emit(EncodeABx(OP_LOAD_CONST, ADest,
      FCurrentPrototype.AddConstantString('')));
end;

procedure TGocciaCompiler.CompileTemplateWithInterpolation(
  const AExpr: TGocciaTemplateWithInterpolationExpression; const ADest: UInt8);
var
  I: Integer;
  PartReg: UInt8;
begin
  if AExpr.Parts.Count = 0 then
  begin
    Emit(EncodeABx(OP_LOAD_CONST, ADest,
      FCurrentPrototype.AddConstantString('')));
    Exit;
  end;

  CompileExpression(AExpr.Parts[0], ADest);

  for I := 1 to AExpr.Parts.Count - 1 do
  begin
    PartReg := FCurrentScope.AllocateRegister;
    CompileExpression(AExpr.Parts[I], PartReg);
    Emit(EncodeABC(OP_RT_ADD, ADest, ADest, PartReg));
    FCurrentScope.FreeRegister;
  end;
end;

procedure TGocciaCompiler.CompileNewExpression(
  const AExpr: TGocciaNewExpression; const ADest: UInt8);
var
  CtorReg: UInt8;
  ArgCount, I: Integer;
begin
  CtorReg := FCurrentScope.AllocateRegister;
  CompileExpression(AExpr.Callee, CtorReg);

  ArgCount := AExpr.Arguments.Count;
  for I := 0 to ArgCount - 1 do
    CompileExpression(AExpr.Arguments[I], FCurrentScope.AllocateRegister);

  Emit(EncodeABC(OP_RT_CONSTRUCT, ADest, CtorReg, UInt8(ArgCount)));

  for I := 0 to ArgCount - 1 do
    FCurrentScope.FreeRegister;
  FCurrentScope.FreeRegister;
end;

{ Statement compilation }

procedure TGocciaCompiler.CompileExpressionStatement(
  const AStmt: TGocciaExpressionStatement);
var
  Reg: UInt8;
begin
  Reg := FCurrentScope.AllocateRegister;
  CompileExpression(AStmt.Expression, Reg);
  FCurrentScope.FreeRegister;
end;

procedure TGocciaCompiler.CompileVariableDeclaration(
  const AStmt: TGocciaVariableDeclaration);
var
  I: Integer;
  Info: TGocciaVariableInfo;
  Slot: UInt8;
begin
  for I := 0 to High(AStmt.Variables) do
  begin
    Info := AStmt.Variables[I];
    Slot := FCurrentScope.DeclareLocal(Info.Name, AStmt.IsConst);
    if Assigned(Info.Initializer) then
      CompileExpression(Info.Initializer, Slot)
    else
      Emit(EncodeABC(OP_LOAD_NIL, Slot, 0, 0));
  end;
end;

procedure TGocciaCompiler.CompileBlockStatement(
  const AStmt: TGocciaBlockStatement);
var
  I: Integer;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount: Integer;
  Node: TGocciaASTNode;
  Reg: UInt8;
begin
  FCurrentScope.BeginScope;

  for I := 0 to AStmt.Nodes.Count - 1 do
  begin
    Node := AStmt.Nodes[I];
    if Node is TGocciaStatement then
      CompileStatement(TGocciaStatement(Node))
    else if Node is TGocciaExpression then
    begin
      Reg := FCurrentScope.AllocateRegister;
      CompileExpression(TGocciaExpression(Node), Reg);
      FCurrentScope.FreeRegister;
    end;
  end;

  FCurrentScope.EndScope(ClosedLocals, ClosedCount);
  for I := 0 to ClosedCount - 1 do
    Emit(EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));
end;

procedure TGocciaCompiler.CompileIfStatement(
  const AStmt: TGocciaIfStatement);
var
  CondReg: UInt8;
  ElseJump, EndJump: Integer;
begin
  CondReg := FCurrentScope.AllocateRegister;
  CompileExpression(AStmt.Condition, CondReg);
  FCurrentScope.FreeRegister;

  ElseJump := EmitJump(OP_JUMP_IF_FALSE, CondReg);
  CompileStatement(AStmt.Consequent);

  if Assigned(AStmt.Alternate) then
  begin
    EndJump := EmitJump(OP_JUMP, 0);
    PatchJump(ElseJump);
    CompileStatement(AStmt.Alternate);
    PatchJump(EndJump);
  end
  else
    PatchJump(ElseJump);
end;

procedure TGocciaCompiler.CompileReturnStatement(
  const AStmt: TGocciaReturnStatement);
var
  Reg: UInt8;
begin
  if Assigned(AStmt.Value) then
  begin
    Reg := FCurrentScope.AllocateRegister;
    CompileExpression(AStmt.Value, Reg);
    Emit(EncodeABC(OP_RETURN, Reg, 0, 0));
    FCurrentScope.FreeRegister;
  end
  else
    Emit(EncodeABC(OP_RETURN_NIL, 0, 0, 0));
end;

procedure TGocciaCompiler.CompileThrowStatement(
  const AStmt: TGocciaThrowStatement);
var
  Reg: UInt8;
begin
  Reg := FCurrentScope.AllocateRegister;
  CompileExpression(AStmt.Value, Reg);
  Emit(EncodeABC(OP_THROW, Reg, 0, 0));
  FCurrentScope.FreeRegister;
end;

procedure TGocciaCompiler.CompileTryStatement(
  const AStmt: TGocciaTryStatement);
var
  CatchReg: UInt8;
  HandlerJump, EndJump: Integer;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount: Integer;
begin
  CatchReg := FCurrentScope.AllocateRegister;
  HandlerJump := EmitJump(OP_PUSH_HANDLER, CatchReg);

  CompileBlockStatement(AStmt.Block);

  Emit(EncodeABC(OP_POP_HANDLER, 0, 0, 0));
  EndJump := EmitJump(OP_JUMP, 0);

  PatchJump(HandlerJump);

  if AStmt.CatchParam <> '' then
  begin
    FCurrentScope.BeginScope;
    FCurrentScope.DeclareLocal(AStmt.CatchParam, False);
    Emit(EncodeABC(OP_MOVE, FCurrentScope.NextSlot - 1, CatchReg, 0));
  end;

  if Assigned(AStmt.CatchBlock) then
    CompileBlockStatement(AStmt.CatchBlock);

  if AStmt.CatchParam <> '' then
    FCurrentScope.EndScope(ClosedLocals, ClosedCount);

  PatchJump(EndJump);

  if Assigned(AStmt.FinallyBlock) then
    CompileBlockStatement(AStmt.FinallyBlock);

  FCurrentScope.FreeRegister;
end;

procedure TGocciaCompiler.CompileForOfStatement(
  const AStmt: TGocciaForOfStatement);
var
  IterReg, ValueReg, DoneReg: UInt8;
  LoopStart, ExitJump: Integer;
  Slot: UInt8;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount: Integer;
begin
  IterReg := FCurrentScope.AllocateRegister;
  ValueReg := FCurrentScope.AllocateRegister;
  DoneReg := FCurrentScope.AllocateRegister;

  CompileExpression(AStmt.Iterable, IterReg);
  Emit(EncodeABC(OP_RT_GET_ITER, IterReg, IterReg, 0));

  LoopStart := CurrentCodeCount;

  Emit(EncodeABC(OP_RT_ITER_NEXT, ValueReg, DoneReg, IterReg));
  ExitJump := EmitJump(OP_JUMP_IF_TRUE, DoneReg);

  FCurrentScope.BeginScope;

  if AStmt.BindingName <> '' then
  begin
    Slot := FCurrentScope.DeclareLocal(AStmt.BindingName, AStmt.IsConst);
    Emit(EncodeABC(OP_MOVE, Slot, ValueReg, 0));
  end;

  CompileStatement(AStmt.Body);

  FCurrentScope.EndScope(ClosedLocals, ClosedCount);

  Emit(EncodeAx(OP_JUMP, LoopStart - CurrentCodeCount - 1));

  PatchJump(ExitJump);

  FCurrentScope.FreeRegister;
  FCurrentScope.FreeRegister;
  FCurrentScope.FreeRegister;
end;

procedure TGocciaCompiler.CompileImportDeclaration(
  const AStmt: TGocciaImportDeclaration);
var
  ModReg: UInt8;
  PathIdx, NameIdx: UInt16;
  Pair: TPair<string, string>;
  Slot: UInt8;
begin
  ModReg := FCurrentScope.AllocateRegister;
  PathIdx := FCurrentPrototype.AddConstantString(AStmt.ModulePath);
  Emit(EncodeABx(OP_RT_IMPORT, ModReg, PathIdx));

  for Pair in AStmt.Imports do
  begin
    Slot := FCurrentScope.DeclareLocal(Pair.Key, True);
    NameIdx := FCurrentPrototype.AddConstantString(Pair.Value);
    if NameIdx > High(UInt8) then
      raise Exception.Create('Constant pool overflow: import name index exceeds 255');
    Emit(EncodeABC(OP_RT_GET_PROP, Slot, ModReg, UInt8(NameIdx)));
  end;

  FCurrentScope.FreeRegister;
end;

procedure TGocciaCompiler.CompileExportDeclaration(
  const AStmt: TGocciaExportDeclaration);
var
  Pair: TPair<string, string>;
  LocalIdx: Integer;
  Reg: UInt8;
  NameIdx: UInt16;
begin
  for Pair in AStmt.ExportsTable do
  begin
    LocalIdx := FCurrentScope.ResolveLocal(Pair.Value);
    if LocalIdx >= 0 then
    begin
      Reg := FCurrentScope.GetLocal(LocalIdx).Slot;
      NameIdx := FCurrentPrototype.AddConstantString(Pair.Key);
      Emit(EncodeABx(OP_RT_EXPORT, Reg, NameIdx));
    end;
  end;
end;

{ Helpers }

function TGocciaCompiler.Emit(const AInstruction: UInt32): Integer;
begin
  Result := FCurrentPrototype.EmitInstruction(AInstruction);
end;

procedure TGocciaCompiler.EmitLine(const ALine, AColumn: Integer);
begin
  if Assigned(FCurrentPrototype.DebugInfo) then
    FCurrentPrototype.DebugInfo.AddLineMapping(
      UInt32(FCurrentPrototype.CodeCount), UInt32(ALine), UInt16(AColumn));
end;

function TGocciaCompiler.EmitJump(const AOp: TSouffleOpCode;
  const AReg: UInt8): Integer;
begin
  if AOp = OP_JUMP then
    Result := Emit(EncodeAx(AOp, 0))
  else
    Result := Emit(EncodeAsBx(AOp, AReg, 0));
end;

procedure TGocciaCompiler.PatchJump(const AIndex: Integer);
var
  Offset: Integer;
  Instruction: UInt32;
  Op: UInt8;
  A: UInt8;
begin
  Offset := CurrentCodeCount - AIndex - 1;
  Instruction := FCurrentPrototype.GetInstruction(AIndex);
  Op := DecodeOp(Instruction);

  if TSouffleOpCode(Op) = OP_JUMP then
    FCurrentPrototype.PatchInstruction(AIndex, EncodeAx(OP_JUMP, Offset))
  else
  begin
    A := DecodeA(Instruction);
    FCurrentPrototype.PatchInstruction(AIndex,
      EncodeAsBx(TSouffleOpCode(Op), A, Int16(Offset)));
  end;
end;

function TGocciaCompiler.CurrentCodeCount: Integer;
begin
  Result := FCurrentPrototype.CodeCount;
end;

function TGocciaCompiler.PendingClassCount: Integer;
begin
  Result := Length(FPendingClasses);
end;

function TGocciaCompiler.GetPendingClass(
  const AIndex: Integer): TGocciaCompilerClassEntry;
begin
  Result := FPendingClasses[AIndex];
end;

function TGocciaCompiler.TokenTypeToRTOp(
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

end.
