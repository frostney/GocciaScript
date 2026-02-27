unit Goccia.Compiler.Expressions;

{$I Goccia.inc}

interface

uses
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Compiler.Context;

procedure CompileLiteral(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaLiteralExpression; const ADest: UInt8);
procedure CompileIdentifier(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIdentifierExpression; const ADest: UInt8);
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
procedure CompileCompoundAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaCompoundAssignmentExpression; const ADest: UInt8);

implementation

uses
  Classes,
  Generics.Collections,
  Math,
  SysUtils,

  Souffle.Bytecode,
  Souffle.Bytecode.Chunk,
  Souffle.Bytecode.Debug,

  Goccia.Compiler.ConstantFolding,
  Goccia.Compiler.Scope,
  Goccia.Lexer,
  Goccia.Parser,
  Goccia.Token,
  Goccia.Values.Primitives;

procedure CompileLiteral(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaLiteralExpression; const ADest: UInt8);
var
  Value: TGocciaValue;
  Idx: UInt16;
begin
  Value := AExpr.Value;

  if Value is TGocciaUndefinedLiteralValue then
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_NIL, ADest, 0, 0))
  else if Value is TGocciaNullLiteralValue then
    EmitInstruction(ACtx, EncodeABx(OP_RT_GET_GLOBAL, ADest,
      ACtx.Template.AddConstantString('null')))
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
var
  LocalIdx, UpvalIdx: Integer;
  Slot: UInt8;
begin
  LocalIdx := ACtx.Scope.ResolveLocal(AExpr.Name);
  if LocalIdx >= 0 then
  begin
    Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
    if Slot <> ADest then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, Slot, 0));
    Exit;
  end;

  UpvalIdx := ACtx.Scope.ResolveUpvalue(AExpr.Name);
  if UpvalIdx >= 0 then
  begin
    EmitInstruction(ACtx, EncodeABx(OP_GET_UPVALUE, ADest, UInt16(UpvalIdx)));
    Exit;
  end;

  EmitInstruction(ACtx, EncodeABx(OP_RT_GET_GLOBAL, ADest,
    ACtx.Template.AddConstantString(AExpr.Name)));
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
    JumpIdx2 := EmitJumpInstruction(ACtx, OP_JUMP, 0);

    PatchJumpTarget(ACtx, JumpIdx);
    RegB := ACtx.Scope.AllocateRegister;
    RegC := ACtx.Scope.AllocateRegister;
    EmitInstruction(ACtx, EncodeABx(OP_RT_GET_GLOBAL, RegB,
      ACtx.Template.AddConstantString('null')));
    EmitInstruction(ACtx, EncodeABC(OP_RT_EQ, RegC, ADest, RegB));
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    JumpIdx := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, RegC);

    PatchJumpTarget(ACtx, JumpIdx2);
    ACtx.CompileExpression(AExpr.Right, ADest);

    PatchJumpTarget(ACtx, JumpIdx);
    Exit;
  end;

  RegB := ACtx.Scope.AllocateRegister;
  RegC := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(AExpr.Left, RegB);
  ACtx.CompileExpression(AExpr.Right, RegC);

  Op := TokenTypeToRuntimeOp(AExpr.Operator);
  EmitInstruction(ACtx, EncodeABC(Op, ADest, RegB, RegC));

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileUnary(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaUnaryExpression; const ADest: UInt8);
var
  RegB: UInt8;
begin
  if TryFoldUnary(ACtx, AExpr, ADest) then
    Exit;

  RegB := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AExpr.Operand, RegB);

  case AExpr.Operator of
    gttNot:        EmitInstruction(ACtx, EncodeABC(OP_RT_NOT, ADest, RegB, 0));
    gttMinus:      EmitInstruction(ACtx, EncodeABC(OP_RT_NEG, ADest, RegB, 0));
    gttPlus:
    begin
      if ADest <> RegB then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, RegB, 0));
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
begin
  ACtx.CompileExpression(AExpr.Value, ADest);

  LocalIdx := ACtx.Scope.ResolveLocal(AExpr.Name);
  if LocalIdx >= 0 then
  begin
    Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
    if ADest <> Slot then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, Slot, ADest, 0));
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
  EmitInstruction(ACtx, EncodeABC(OP_TABLE_SET, ObjReg, KeyIdx, ValReg));

  if ADest <> ValReg then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, ValReg, 0));

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileArrowFunction(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaArrowFunctionExpression; const ADest: UInt8);
var
  OldTemplate: TSouffleFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TSouffleFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  FuncIdx: UInt16;
  I: Integer;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TSouffleFunctionTemplate.Create('<arrow>');
  ChildTemplate.DebugInfo := TSouffleDebugInfo.Create(ACtx.SourcePath);
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);

  ChildTemplate.ParameterCount := Length(AExpr.Parameters);
  for I := 0 to High(AExpr.Parameters) do
    if not AExpr.Parameters[I].IsPattern then
      ChildScope.DeclareLocal(AExpr.Parameters[I].Name, False);

  ACtx.SwapState(ChildTemplate, ChildScope);
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

procedure CompileCall(const ACtx: TGocciaCompilationContext;
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
      EmitInstruction(ACtx, EncodeABC(OP_TABLE_GET, BaseReg, ObjReg, UInt8(PropIdx)));
    end;

    for I := 0 to ArgCount - 1 do
      ACtx.CompileExpression(AExpr.Arguments[I], ACtx.Scope.AllocateRegister);

    EmitInstruction(ACtx, EncodeABC(OP_RT_CALL_METHOD, BaseReg, UInt8(ArgCount), 0));

    for I := 0 to ArgCount - 1 do
      ACtx.Scope.FreeRegister;

    if ADest <> BaseReg then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, BaseReg, 0));

    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
  end
  else
  begin
    BaseReg := ACtx.Scope.AllocateRegister;

    ACtx.CompileExpression(AExpr.Callee, BaseReg);

    for I := 0 to ArgCount - 1 do
      ACtx.CompileExpression(AExpr.Arguments[I], ACtx.Scope.AllocateRegister);

    EmitInstruction(ACtx, EncodeABC(OP_RT_CALL, BaseReg, UInt8(ArgCount), 0));

    for I := 0 to ArgCount - 1 do
      ACtx.Scope.FreeRegister;

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
    EmitInstruction(ACtx, EncodeABC(OP_TABLE_GET, ADest, ObjReg, UInt8(PropIdx)));
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
    ACtx.CompileExpression(AExpr.Elements[I], ElemReg);
    EmitInstruction(ACtx, EncodeABC(OP_ARRAY_PUSH, ADest, ElemReg, 0));
    ACtx.Scope.FreeRegister;
  end;
end;

procedure CompileObject(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaObjectExpression; const ADest: UInt8);
var
  I: Integer;
  Key: string;
  ValExpr: TGocciaExpression;
  ValReg: UInt8;
  KeyIdx: UInt16;
  Names: TStringList;
begin
  EmitInstruction(ACtx, EncodeABC(OP_NEW_TABLE, ADest,
    UInt8(Min(AExpr.Properties.Count, 255)), 0));

  Names := AExpr.GetPropertyNamesInOrder;
  try
    for I := 0 to Names.Count - 1 do
    begin
      Key := Names[I];
      if AExpr.Properties.TryGetValue(Key, ValExpr) then
      begin
        ValReg := ACtx.Scope.AllocateRegister;
        ACtx.CompileExpression(ValExpr, ValReg);
        KeyIdx := ACtx.Template.AddConstantString(Key);
        if KeyIdx > High(UInt8) then
          raise Exception.Create('Constant pool overflow: property name index exceeds 255');
        EmitInstruction(ACtx, EncodeABC(OP_TABLE_SET, ADest, KeyIdx, ValReg));
        ACtx.Scope.FreeRegister;
      end;
    end;
  finally
    Names.Free;
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
        Lexer := TGocciaLexer.Create(ExprText + ';', ACtx.SourcePath);
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
                    HasParts := True;
                  end
                  else
                  begin
                    PartReg := ACtx.Scope.AllocateRegister;
                    ACtx.CompileExpression(ParsedExpr, PartReg);
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

end.
