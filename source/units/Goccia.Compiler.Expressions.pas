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
  Goccia.Compiler.Scope,
  Goccia.SourceSpan;

procedure CompileLiteral(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaLiteralExpression; const ADest: UInt16);
procedure CompileIdentifier(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIdentifierExpression; const ADest: UInt16);
procedure CompileIdentifierAccess(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIdentifierExpression; const ADest: UInt16;
  const ASafe: Boolean);
procedure CompileBinary(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaBinaryExpression; const ADest: UInt16);
procedure CompileSequence(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaSequenceExpression; const ADest: UInt16);
procedure CompileUnary(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaUnaryExpression; const ADest: UInt16);
procedure CompileAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaAssignmentExpression; const ADest: UInt16);
procedure CompilePropertyAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaPropertyAssignmentExpression; const ADest: UInt16);
procedure CompileArrowFunction(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaArrowFunctionExpression; const ADest: UInt16);
procedure CompileCall(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaCallExpression; const ADest: UInt16;
  const ATail: Boolean = False);
procedure CompileMember(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaMemberExpression; const ADest: UInt16);
procedure CompileConditional(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaConditionalExpression; const ADest: UInt16);
procedure CompileArray(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaArrayExpression; const ADest: UInt16);
procedure CompileObject(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaObjectExpression; const ADest: UInt16);
procedure CompileTemplateLiteral(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaTemplateLiteralExpression; const ADest: UInt16);
procedure CompileRegexLiteral(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaRegexLiteralExpression; const ADest: UInt16);
procedure CompileTemplateWithInterpolation(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaTemplateWithInterpolationExpression; const ADest: UInt16);
procedure CompileTaggedTemplate(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaTaggedTemplateExpression; const ADest: UInt16;
  const ATail: Boolean = False);
// Compiles AExpr into ADest, marking any call that the static semantics place
// in tail position (ES2026 §15.10.2 HasCallInTailPosition) with CALL_FLAG_TAIL
// so the VM can reuse the current call frame.  The marked call is still emitted
// as an ordinary call producing a value in ADest; the caller emits OP_RETURN as
// usual.  When the VM honors the tail flag it returns directly and that trailing
// store/return is simply never reached.  Descends only through the
// tail-position-preserving forms (conditional, &&/||/??, comma); every other
// form just compiles normally.
procedure CompileReturnValueTailAware(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaExpression; const ADest: UInt16);
procedure CompileNewExpression(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaNewExpression; const ADest: UInt16);
procedure CompileComputedPropertyAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaComputedPropertyAssignmentExpression; const ADest: UInt16);
procedure CompileCompoundAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaCompoundAssignmentExpression; const ADest: UInt16);
procedure CompilePropertyCompoundAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaPropertyCompoundAssignmentExpression; const ADest: UInt16);
procedure CompileComputedPropertyCompoundAssignment(
  const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaComputedPropertyCompoundAssignmentExpression;
  const ADest: UInt16);
procedure CompileFunctionExpression(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaFunctionExpression; const ADest: UInt16;
  const ATemplateName: string = '<function>';
  const ABindOwnName: Boolean = True);
procedure CompileExpressionWithInferredName(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaExpression; const ADest: UInt16;
  const AInferredName: string);
procedure CompileIncrement(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIncrementExpression; const ADest: UInt16;
  const AKeepResult: Boolean = True);
procedure CompilePrivateMember(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaPrivateMemberExpression; const ADest: UInt16);
procedure CompilePrivatePropertyAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaPrivatePropertyAssignmentExpression; const ADest: UInt16);
procedure CompilePrivatePropertyCompoundAssignment(
  const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaPrivatePropertyCompoundAssignmentExpression;
  const ADest: UInt16);
procedure CompileThis(const ACtx: TGocciaCompilationContext;
  const ADest: UInt16);
procedure CompileSuperAccess(const ACtx: TGocciaCompilationContext;
  const ADest: UInt16);
procedure CompileImportMeta(const ACtx: TGocciaCompilationContext;
  const ADest: UInt16);
procedure CompileNewTarget(const ACtx: TGocciaCompilationContext;
  const ADest: UInt16);
procedure CompileDynamicImport(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaImportCallExpression; const ADest: UInt16);
procedure CompileYield(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaYieldExpression; const ADest: UInt16);
function ExpressionContainsDirectEval(const AExpr: TGocciaExpression): Boolean;
function ExpressionContainsSuspension(const AExpr: TGocciaExpression): Boolean;
function ExpressionCreatesClosureBoundary(const AExpr: TGocciaExpression): Boolean;

procedure EmitDefaultParameters(const ACtx: TGocciaCompilationContext;
  const AParams: TGocciaParameterArray);
function ParameterListHasDefaultValues(
  const AParams: TGocciaParameterArray): Boolean;
function ParameterListIsSimple(const AParams: TGocciaParameterArray): Boolean;
function SyntheticParamLocalName(const AIndex: Integer): string;
function DeclareArgumentsObjectLocal(const ACtx: TGocciaCompilationContext;
  const AScope: TGocciaCompilerScope; const AParams: TGocciaParameterArray;
  const ASourceText: string): Integer;
procedure EmitCreateArgumentsObject(const ACtx: TGocciaCompilationContext;
  const AArgumentsSlot: Integer; const AUseMappedArguments: Boolean;
  const AFormalParameterCount: Integer);
procedure CollectDestructuringBindings(const APattern: TGocciaDestructuringPattern;
  const AScope: TGocciaCompilerScope; const AIsConst: Boolean = False);
procedure EmitBindingAssignmentFromRegister(const ACtx: TGocciaCompilationContext;
  const AName: string; const AValueReg: UInt16;
  const AAssignmentMode: Boolean);
procedure EmitLoadHiddenWithObject(const ACtx: TGocciaCompilationContext;
  const AHiddenName: string; const ADest: UInt16);
procedure EmitWithAssignmentOrFallback(const ACtx: TGocciaCompilationContext;
  const AName: string; const AValueReg: UInt16);
procedure EmitDestructuring(const ACtx: TGocciaCompilationContext;
  const APattern: TGocciaDestructuringPattern; const ASrcReg: UInt16;
  const AAssignmentMode: Boolean = False);
procedure EmitDestructuringParameters(const ACtx: TGocciaCompilationContext;
  const AParams: TGocciaParameterArray);
procedure CompileDestructuringAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaDestructuringAssignmentExpression; const ADest: UInt16);

implementation

uses
  Classes,
  Generics.Collections,
  Math,
  SysUtils,

  UnicodeStringList,

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
  Goccia.Modules,
  Goccia.Token,
  Goccia.Types.Enforcement,
  Goccia.Values.BigIntValue,
  Goccia.Values.Primitives;

const
  DERIVED_THIS_INITIALIZED_LOCAL = '__derived_this_initialized';

type
  TGocciaCompilerJumpArray = array of Integer;
  TPreparedDestructuringTargetKind = (
    pdtNone,
    pdtIdentifierWithBinding,
    pdtStringProperty,
    pdtComputedProperty,
    pdtSuperStringProperty,
    pdtSuperComputedProperty,
    pdtPrivateProperty
  );
  TPreparedDestructuringTarget = record
    Kind: TPreparedDestructuringTargetKind;
    ObjectReg: Integer;
    KeyReg: Integer;
    ThisReg: Integer;
    SuperReg: Integer;
    PropertyName: string;
  end;

function ExpressionContainsOptionalChain(const AExpr: TGocciaExpression): Boolean; forward;
procedure AddOptionalChainJump(const ACtx: TGocciaCompilationContext;
  var AJumps: TGocciaCompilerJumpArray; var AJumpCount: Integer;
  const AReg: UInt16); forward;
procedure CompileExpressionWithOptionalChainJumps(
  const ACtx: TGocciaCompilationContext; const AExpr: TGocciaExpression;
  const ADest: UInt16; var AJumps: TGocciaCompilerJumpArray;
  var AJumpCount: Integer); forward;

function PrivateKey(const AScope: TGocciaCompilerScope;
  const AName: string): string;
var
  Prefix: string;
begin
  Prefix := AScope.ResolvePrivatePrefixForName(AName);
  Result := '#slot:' + Prefix + AName;
end;

function InferredExpressionType(const AScope: TGocciaCompilerScope;
  const AExpr: TGocciaExpression): TGocciaLocalType;
begin
  Result := ExpressionType(AScope, AExpr);
  if Result = sltUntyped then
    Result := InferLocalType(AExpr);
end;

function IsAnonymousFunctionNameExpression(
  const AExpr: TGocciaExpression): Boolean;
begin
  Result := (AExpr is TGocciaObjectMethodDefinition) or
    (AExpr is TGocciaArrowFunctionExpression) or
    ((AExpr is TGocciaFunctionExpression) and
     (TGocciaFunctionExpression(AExpr).Name = '')) or
    ((AExpr is TGocciaClassExpression) and
     (TGocciaClassExpression(AExpr).ClassDefinition.Name = ''));
end;

procedure CompileExpressionWithInferredName(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaExpression; const ADest: UInt16;
  const AInferredName: string);
var
  FuncCount: Integer;
  InferredTemplate: TGocciaFunctionTemplate;
begin
  if AInferredName = '' then
  begin
    ACtx.CompileExpression(AExpr, ADest);
    Exit;
  end;

  FuncCount := ACtx.Template.FunctionCount;
  if (AExpr is TGocciaClassExpression) and
     (TGocciaClassExpression(AExpr).ClassDefinition.Name = '') then
  begin
    Goccia.Compiler.Statements.CompileClassExpression(ACtx,
      TGocciaClassExpression(AExpr).ClassDefinition, ADest, AInferredName);
    Exit;
  end;

  ACtx.CompileExpression(AExpr, ADest);
  if ((AExpr is TGocciaArrowFunctionExpression) or
      (AExpr is TGocciaFunctionExpression)) and
     (ACtx.Template.FunctionCount > FuncCount) then
  begin
    InferredTemplate := ACtx.Template.GetFunction(
      ACtx.Template.FunctionCount - 1);
    if (InferredTemplate.Name = '<arrow>') or
       (InferredTemplate.Name = '<function>') or
       (InferredTemplate.Name = '<method>') then
      InferredTemplate.Name := AInferredName;
  end;
end;

procedure CompileAssignmentValue(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaExpression; const ADest: UInt16;
  const AInferredName: string; const AInferName: Boolean);
begin
  if AInferName then
    CompileExpressionWithInferredName(ACtx, AExpr, ADest, AInferredName)
  else
    ACtx.CompileExpression(AExpr, ADest);
end;

function SingleIdentifierPatternName(
  const APattern: TGocciaDestructuringPattern): string;
begin
  if APattern is TGocciaIdentifierDestructuringPattern then
    Exit(TGocciaIdentifierDestructuringPattern(APattern).Name);
  Result := '';
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
  const ALocalIdx: Integer; const AValueReg: UInt16;
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
  const ADest, AObjReg: UInt16; const APropertyName: string);
var
  PropIdx: UInt16;
  KeyReg: UInt16;
begin
  PropIdx := ACtx.Template.AddConstantString(APropertyName);
  if PropIdx <= High(UInt8) then
    EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, ADest, AObjReg,
      UInt16(PropIdx)))
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
  const AObjReg: UInt16; const APropertyName: string; const AValueReg: UInt16);
var
  PropIdx: UInt16;
  KeyReg: UInt16;
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
      UInt16(PropIdx), AValueReg))
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
  const AObjReg: UInt16; const APropertyName: string; const AValueReg: UInt16;
  const AOpcode: TGocciaOpCode);
var
  PropIdx: UInt16;
  KeyReg: UInt16;
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

function GetWithBindingOpcode(const ACtx: TGocciaCompilationContext): TGocciaOpCode;
begin
  if ACtx.NonStrictMode then
    Result := OP_GET_WITH_BINDING
  else
    Result := OP_GET_WITH_BINDING_STRICT;
end;

function SetWithBindingOpcode(const ACtx: TGocciaCompilationContext): TGocciaOpCode;
begin
  if ACtx.NonStrictMode then
    Result := OP_SET_WITH_BINDING_LOOSE
  else
    Result := OP_SET_WITH_BINDING;
end;

function ChildBodyIsStrictCode(const ACtx: TGocciaCompilationContext;
  const ABody: TGocciaASTNode): Boolean;
  {$IFDEF FPC}inline;{$ENDIF}
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
  const AValueReg: UInt16; const ANameIdx: UInt16);
begin
  EmitInstruction(ACtx, EncodeABx(SetGlobalOpcode(ACtx), AValueReg,
    ANameIdx));
end;

procedure EmitSetGlobalByName(const ACtx: TGocciaCompilationContext;
  const AValueReg: UInt16; const AName: string);
begin
  EmitSetGlobalByIndex(ACtx, AValueReg,
    ACtx.Template.AddConstantString(AName));
end;

procedure EmitLoadSuperPropertyByName(const ACtx: TGocciaCompilationContext;
  const ABaseReg, AMode: UInt16; const APropertyName: string);
var
  PropIdx: UInt16;
  KeyReg: UInt16;
begin
  PropIdx := ACtx.Template.AddConstantString(APropertyName);
  if PropIdx <= High(UInt8) then
    EmitInstruction(ACtx, EncodeABC(OP_SUPER_GET_CONST, ABaseReg, AMode,
      UInt16(PropIdx)))
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

procedure PrepareSuperPropertyBase(const ACtx: TGocciaCompilationContext;
  out AThisReg, ABaseReg, ASuperReg: UInt16);
begin
  AThisReg := ACtx.Scope.AllocateRegister;
  CompileThis(ACtx, AThisReg);
  ABaseReg := ACtx.Scope.AllocateRegister;
  ASuperReg := ACtx.Scope.AllocateRegister;
  CompileSuperAccess(ACtx, ASuperReg);
  if ASuperReg <> ABaseReg + 1 then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ABaseReg + 1, ASuperReg, 0));
end;

procedure EmitConstAssignmentError(const ACtx: TGocciaCompilationContext);
var
  MsgIdx: UInt16;
begin
  MsgIdx := ACtx.Template.AddConstantString('Assignment to constant variable.');
  if MsgIdx <= High(UInt8) then
    EmitInstruction(ACtx, EncodeABC(OP_THROW_TYPE_ERROR_CONST, 0, 0,
      UInt16(MsgIdx)))
  else
    EmitInstruction(ACtx, EncodeABx(OP_THROW_TYPE_ERROR_CONST_LONG, 0, MsgIdx));
end;

function ShouldIgnoreNonStrictImmutableLocalAssignment(
  const ACtx: TGocciaCompilationContext;
  const ALocal: TGocciaCompilerLocal): Boolean;
  {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := ACtx.NonStrictMode and ALocal.IsConst and
    ALocal.IsNonStrictImmutable;
end;

function ShouldIgnoreNonStrictImmutableUpvalueAssignment(
  const ACtx: TGocciaCompilationContext;
  const AUpvalue: TGocciaCompilerUpvalue): Boolean;
  {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := ACtx.NonStrictMode and AUpvalue.IsConst and
    AUpvalue.IsNonStrictImmutable;
end;

procedure EmitReferenceError(const ACtx: TGocciaCompilationContext;
  const AMessage: string);
var
  ErrorReg, MessageReg: UInt16;
begin
  ErrorReg := ACtx.Scope.AllocateRegister;
  MessageReg := ACtx.Scope.AllocateRegister;
  try
    EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, ErrorReg,
      ACtx.Template.AddConstantString(REFERENCE_ERROR_NAME)));
    EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, MessageReg,
      ACtx.Template.AddConstantString(AMessage)));
    EmitInstruction(ACtx, EncodeABC(OP_CONSTRUCT, ErrorReg, ErrorReg, 1));
    EmitInstruction(ACtx, EncodeABC(OP_THROW, ErrorReg, 0, 0));
  finally
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
  end;
end;

procedure CompileYield(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaYieldExpression; const ADest: UInt16);
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
  const AExpr: TGocciaLiteralExpression; const ADest: UInt16);
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
    EmitLoadStringLiteral(ACtx, ADest, TGocciaStringLiteralValue(Value).Value)
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
  const AExpr: TGocciaRegexLiteralExpression; const ADest: UInt16);
var
  LiteralIdx: UInt16;
begin
  LiteralIdx := ACtx.Template.AddConstantRegExpLiteral(AExpr.Pattern, AExpr.Flags);
  EmitInstruction(ACtx, EncodeABx(OP_LOAD_REGEXP, ADest, LiteralIdx));
end;

procedure CompileIdentifier(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIdentifierExpression; const ADest: UInt16);
begin
  CompileIdentifierAccess(ACtx, AExpr, ADest, False);
end;

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
  const AHiddenName: string; const ADest: UInt16);
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

procedure EmitImportBindingAccess(const ACtx: TGocciaCompilationContext;
  const APhase: TGocciaImportCallPhase; const AModulePath, AExportName: string;
  const ADest: UInt16);
var
  PathIdx, NameIdx: UInt16;
begin
  PathIdx := ACtx.Template.AddConstantString(AModulePath);
  if APhase = icpSource then
    EmitInstruction(ACtx, EncodeABx(OP_IMPORT_SOURCE, ADest, PathIdx))
  else if APhase = icpDefer then
    EmitInstruction(ACtx, EncodeABx(OP_IMPORT_DEFER, ADest, PathIdx))
  else
    EmitInstruction(ACtx, EncodeABx(OP_IMPORT, ADest, PathIdx));

  // ES2026 §16.2.1.7.3.1: a named import of a missing or ambiguous export is a
  // SyntaxError. OP_GET_IMPORT_BINDING loads the binding from the module
  // namespace already in ADest and rejects names the module does not export,
  // so the bytecode entry path (which bypasses link-time validation) still
  // matches the interpreter (ADR 0014).
  if (APhase = icpEvaluation) and (AExportName <> '') then
  begin
    NameIdx := ACtx.Template.AddConstantString(AExportName);
    EmitInstruction(ACtx, EncodeABx(OP_GET_IMPORT_BINDING, ADest, NameIdx));
  end;
end;

procedure EmitExportBindingUpdates(const ACtx: TGocciaCompilationContext;
  const AExportNames: array of string; const AExportNameCount: Integer;
  const AValueReg: UInt16);
var
  I: Integer;
  NameIdx: UInt16;
begin
  for I := 0 to AExportNameCount - 1 do
  begin
    NameIdx := ACtx.Template.AddConstantString(AExportNames[I]);
    EmitInstruction(ACtx, EncodeABx(OP_EXPORT, AValueReg, NameIdx));
  end;
end;

procedure CompileIdentifierAccessNoWith(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIdentifierExpression; const ADest: UInt16;
  const ASafe: Boolean);
var
  LocalIdx, UpvalIdx: Integer;
  Local: TGocciaCompilerLocal;
  Slot: UInt16;
  NameIdx: UInt16;
  CondReg, ArgReg: UInt16;
  OkJump: Integer;
begin
  LocalIdx := ACtx.Scope.ResolveLocal(AExpr.Name);
  if LocalIdx >= 0 then
  begin
    Local := ACtx.Scope.GetLocal(LocalIdx);
    if Local.IsImportBinding then
    begin
      EmitImportBindingAccess(ACtx, Local.ImportPhase, Local.ImportModulePath,
        Local.ImportExportName, ADest);
      Exit;
    end;
    if Local.IsGlobalBacked then
    begin
      NameIdx := ACtx.Template.AddConstantString(AExpr.Name);
      EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, ADest, NameIdx));
      Exit;
    end;
    Slot := Local.Slot;
    EmitInstruction(ACtx, EncodeABx(OP_GET_LOCAL, ADest, Slot));
    Exit;
  end;

  UpvalIdx := ACtx.Scope.ResolveUpvalue(AExpr.Name);
  if UpvalIdx >= 0 then
  begin
    if ACtx.Scope.GetUpvalue(UpvalIdx).IsImportBinding then
    begin
      EmitImportBindingAccess(ACtx, ACtx.Scope.GetUpvalue(UpvalIdx).ImportPhase,
        ACtx.Scope.GetUpvalue(UpvalIdx).ImportModulePath,
        ACtx.Scope.GetUpvalue(UpvalIdx).ImportExportName, ADest);
      Exit;
    end;
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
  const AExpr: TGocciaIdentifierExpression; const ADest: UInt16;
  const ASafe: Boolean);
var
  ObjReg, KeyReg, CondReg: UInt16;
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
      EmitInstruction(ACtx, EncodeABC(GetWithBindingOpcode(ACtx), ADest,
        ObjReg, KeyReg));
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

procedure PrepareIdentifierWithBindingTarget(
  const ACtx: TGocciaCompilationContext; const AName: string;
  var ATarget: TPreparedDestructuringTarget);
var
  ObjReg, CondReg, ThisReg, BaseReg, SuperReg: UInt16;
  NameIdx: UInt16;
  I, EndCount: Integer;
  MissJump: Integer;
  EndJumps: array of Integer;
begin
  ATarget.Kind := pdtIdentifierWithBinding;
  ATarget.PropertyName := AName;
  ATarget.ObjectReg := ACtx.Scope.AllocateRegister;
  ATarget.KeyReg := ACtx.Scope.AllocateRegister;
  ObjReg := ACtx.Scope.AllocateRegister;
  CondReg := ACtx.Scope.AllocateRegister;
  EndCount := 0;
  SetLength(EndJumps, ACtx.Scope.WithBindingCount);
  try
    NameIdx := ACtx.Template.AddConstantString(AName);
    EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, UInt16(ATarget.KeyReg),
      NameIdx));
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED,
      UInt16(ATarget.ObjectReg), 0, 0));

    for I := ACtx.Scope.WithBindingCount - 1 downto 0 do
    begin
      EmitLoadHiddenWithObject(ACtx, ACtx.Scope.GetWithBindingName(I), ObjReg);
      EmitInstruction(ACtx, EncodeABC(OP_HAS_WITH_BINDING, CondReg, ObjReg,
        UInt16(ATarget.KeyReg)));
      MissJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, CondReg);
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, UInt16(ATarget.ObjectReg),
        ObjReg, 0));
      EndJumps[EndCount] := EmitJumpInstruction(ACtx, OP_JUMP, 0);
      Inc(EndCount);
      PatchJumpTarget(ACtx, MissJump);
    end;

    for I := 0 to EndCount - 1 do
      PatchJumpTarget(ACtx, EndJumps[I]);
  finally
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
  end;
end;

procedure EmitPreparedIdentifierWithBindingLoad(
  const ACtx: TGocciaCompilationContext;
  const ATarget: TPreparedDestructuringTarget; const ADest: UInt16);
var
  Ident: TGocciaIdentifierExpression;
  MissJump, EndJump: Integer;
begin
  MissJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH,
    UInt16(ATarget.ObjectReg), GOCCIA_NULLISH_MATCH_UNDEFINED);
  EmitInstruction(ACtx, EncodeABC(GetWithBindingOpcode(ACtx), ADest,
    UInt16(ATarget.ObjectReg), UInt16(ATarget.KeyReg)));
  EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
  PatchJumpTarget(ACtx, MissJump);
  Ident := TGocciaIdentifierExpression.Create(ATarget.PropertyName,
    TGocciaSourceSpan.Empty);
  try
    CompileIdentifierAccessNoWith(ACtx, Ident, ADest, False);
  finally
    Ident.Free;
  end;
  PatchJumpTarget(ACtx, EndJump);
end;

procedure EmitLoadBindingByName(const ACtx: TGocciaCompilationContext;
  const AName: string; const ADest: UInt16; const ASafe: Boolean);
var
  Ident: TGocciaIdentifierExpression;
begin
  Ident := TGocciaIdentifierExpression.Create(AName,
    TGocciaSourceSpan.Empty);
  try
    CompileIdentifierAccess(ACtx, Ident, ADest, ASafe);
  finally
    Ident.Free;
  end;
end;

procedure EmitWithAssignmentOrFallback(const ACtx: TGocciaCompilationContext;
  const AName: string; const AValueReg: UInt16);
var
  ObjReg, KeyReg, CondReg: UInt16;
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
      EmitInstruction(ACtx, EncodeABC(SetWithBindingOpcode(ACtx), ObjReg,
        KeyReg, AValueReg));
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

procedure EmitWithBindingProbe(const ACtx: TGocciaCompilationContext;
  const AName: string);
var
  ObjReg, KeyReg, CondReg: UInt16;
  NameIdx: UInt16;
  I, EndCount: Integer;
  MissJump: Integer;
  EndJumps: array of Integer;
begin
  if (ACtx.Scope.WithBindingCount = 0) or HiddenWithBindingName(AName) then
    Exit;

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
      EndJumps[EndCount] := EmitJumpInstruction(ACtx, OP_JUMP, 0);
      Inc(EndCount);
      PatchJumpTarget(ACtx, MissJump);
    end;

    for I := 0 to EndCount - 1 do
      PatchJumpTarget(ACtx, EndJumps[I]);
  finally
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
  end;
end;

procedure EmitDestructuringBindingResolutionProbe(
  const ACtx: TGocciaCompilationContext;
  const APattern: TGocciaDestructuringPattern);
var
  AssignPat: TGocciaAssignmentDestructuringPattern;
begin
  if APattern is TGocciaAssignmentDestructuringPattern then
  begin
    AssignPat := TGocciaAssignmentDestructuringPattern(APattern);
    EmitDestructuringBindingResolutionProbe(ACtx, AssignPat.Left);
  end
  else if APattern is TGocciaIdentifierDestructuringPattern then
    EmitWithBindingProbe(ACtx,
      TGocciaIdentifierDestructuringPattern(APattern).Name);
end;

function CallTrustedFlag(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaCallExpression): UInt16;
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

  Result := CALL_FLAG_TRUSTED;
end;

function IsDirectEvalCall(const AExpr: TGocciaCallExpression): Boolean;
begin
  Result := (not AExpr.Optional) and
    (AExpr.Callee is TGocciaIdentifierExpression) and
    (TGocciaIdentifierExpression(AExpr.Callee).Name = 'eval');
end;

function ExpressionContainsDirectEval(const AExpr: TGocciaExpression): Boolean;
var
  CallExpr: TGocciaCallExpression;
  MemberExpr: TGocciaMemberExpression;
  ArrayExpr: TGocciaArrayExpression;
  ObjectExpr: TGocciaObjectExpression;
  NewExpr: TGocciaNewExpression;
  ImportExpr: TGocciaImportCallExpression;
  Pair: TPair<TGocciaExpression, TGocciaExpression>;
  I: Integer;
begin
  if not Assigned(AExpr) then
    Exit(False);

  if AExpr is TGocciaCallExpression then
  begin
    CallExpr := TGocciaCallExpression(AExpr);
    if IsDirectEvalCall(CallExpr) then
      Exit(True);
    if ExpressionContainsDirectEval(CallExpr.Callee) then
      Exit(True);
    for I := 0 to CallExpr.Arguments.Count - 1 do
      if ExpressionContainsDirectEval(CallExpr.Arguments[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(AExpr);
    if ExpressionContainsDirectEval(MemberExpr.ObjectExpr) then
      Exit(True);
    if MemberExpr.Computed and
       ExpressionContainsDirectEval(MemberExpr.PropertyExpression) then
      Exit(True);
  end
  else if AExpr is TGocciaBinaryExpression then
    Exit(ExpressionContainsDirectEval(TGocciaBinaryExpression(AExpr).Left) or
      ExpressionContainsDirectEval(TGocciaBinaryExpression(AExpr).Right))
  else if AExpr is TGocciaSequenceExpression then
  begin
    for I := 0 to TGocciaSequenceExpression(AExpr).Expressions.Count - 1 do
      if ExpressionContainsDirectEval(
        TGocciaSequenceExpression(AExpr).Expressions[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaUnaryExpression then
    Exit(ExpressionContainsDirectEval(TGocciaUnaryExpression(AExpr).Operand))
  else if AExpr is TGocciaAssignmentExpression then
    Exit(ExpressionContainsDirectEval(TGocciaAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaPropertyAssignmentExpression then
    Exit(ExpressionContainsDirectEval(
      TGocciaPropertyAssignmentExpression(AExpr).ObjectExpr) or
      ExpressionContainsDirectEval(
        TGocciaPropertyAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaComputedPropertyAssignmentExpression then
    Exit(ExpressionContainsDirectEval(
      TGocciaComputedPropertyAssignmentExpression(AExpr).ObjectExpr) or
      ExpressionContainsDirectEval(
        TGocciaComputedPropertyAssignmentExpression(AExpr).PropertyExpression) or
      ExpressionContainsDirectEval(
        TGocciaComputedPropertyAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaCompoundAssignmentExpression then
    Exit(ExpressionContainsDirectEval(
      TGocciaCompoundAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaPropertyCompoundAssignmentExpression then
    Exit(ExpressionContainsDirectEval(
      TGocciaPropertyCompoundAssignmentExpression(AExpr).ObjectExpr) or
      ExpressionContainsDirectEval(
        TGocciaPropertyCompoundAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaComputedPropertyCompoundAssignmentExpression then
    Exit(ExpressionContainsDirectEval(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).ObjectExpr) or
      ExpressionContainsDirectEval(
        TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).PropertyExpression) or
      ExpressionContainsDirectEval(
        TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaIncrementExpression then
    Exit(ExpressionContainsDirectEval(TGocciaIncrementExpression(AExpr).Operand))
  else if AExpr is TGocciaArrayExpression then
  begin
    ArrayExpr := TGocciaArrayExpression(AExpr);
    for I := 0 to ArrayExpr.Elements.Count - 1 do
      if ExpressionContainsDirectEval(ArrayExpr.Elements[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaObjectExpression then
  begin
    ObjectExpr := TGocciaObjectExpression(AExpr);
    for I := 0 to High(ObjectExpr.PropertySourceOrder) do
      case ObjectExpr.PropertySourceOrder[I].PropertyType of
        pstStatic:
          if ExpressionContainsDirectEval(
            ObjectExpr.PropertySourceOrder[I].Expression) then
            Exit(True);
        pstComputed:
          begin
            Pair := ObjectExpr.ComputedPropertiesInOrder[
              ObjectExpr.PropertySourceOrder[I].ComputedIndex];
            if ExpressionContainsDirectEval(Pair.Key) or
               ExpressionContainsDirectEval(Pair.Value) then
              Exit(True);
          end;
        pstComputedGetter,
        pstComputedSetter:
          begin
            Pair := ObjectExpr.ComputedPropertiesInOrder[
              ObjectExpr.PropertySourceOrder[I].ComputedIndex];
            if ExpressionContainsDirectEval(Pair.Key) then
              Exit(True);
          end;
      end;
  end
  else if AExpr is TGocciaYieldExpression then
    Exit(ExpressionContainsDirectEval(TGocciaYieldExpression(AExpr).Operand))
  else if AExpr is TGocciaAwaitExpression then
    Exit(ExpressionContainsDirectEval(TGocciaAwaitExpression(AExpr).Operand))
  else if AExpr is TGocciaConditionalExpression then
    Exit(ExpressionContainsDirectEval(TGocciaConditionalExpression(AExpr).Condition) or
      ExpressionContainsDirectEval(TGocciaConditionalExpression(AExpr).Consequent) or
      ExpressionContainsDirectEval(TGocciaConditionalExpression(AExpr).Alternate))
  else if AExpr is TGocciaNewExpression then
  begin
    NewExpr := TGocciaNewExpression(AExpr);
    if ExpressionContainsDirectEval(NewExpr.Callee) then
      Exit(True);
    for I := 0 to NewExpr.Arguments.Count - 1 do
      if ExpressionContainsDirectEval(NewExpr.Arguments[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaImportCallExpression then
  begin
    ImportExpr := TGocciaImportCallExpression(AExpr);
    Exit(ExpressionContainsDirectEval(ImportExpr.Specifier) or
      ExpressionContainsDirectEval(ImportExpr.Options));
  end
  else if AExpr is TGocciaSpreadExpression then
    Exit(ExpressionContainsDirectEval(TGocciaSpreadExpression(AExpr).Argument))
  else if AExpr is TGocciaTemplateWithInterpolationExpression then
  begin
    for I := 0 to TGocciaTemplateWithInterpolationExpression(AExpr).Parts.Count - 1 do
      if ExpressionContainsDirectEval(
        TGocciaTemplateWithInterpolationExpression(AExpr).Parts[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaTaggedTemplateExpression then
  begin
    if ExpressionContainsDirectEval(TGocciaTaggedTemplateExpression(AExpr).Tag) then
      Exit(True);
    for I := 0 to TGocciaTaggedTemplateExpression(AExpr).Expressions.Count - 1 do
      if ExpressionContainsDirectEval(
        TGocciaTaggedTemplateExpression(AExpr).Expressions[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaDestructuringAssignmentExpression then
    Exit(ExpressionContainsDirectEval(
      TGocciaDestructuringAssignmentExpression(AExpr).Right))
  else if AExpr is TGocciaPrivateMemberExpression then
    Exit(ExpressionContainsDirectEval(
      TGocciaPrivateMemberExpression(AExpr).ObjectExpr))
  else if AExpr is TGocciaPrivatePropertyAssignmentExpression then
    Exit(ExpressionContainsDirectEval(
      TGocciaPrivatePropertyAssignmentExpression(AExpr).ObjectExpr) or
      ExpressionContainsDirectEval(
        TGocciaPrivatePropertyAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaPrivatePropertyCompoundAssignmentExpression then
    Exit(ExpressionContainsDirectEval(
      TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).ObjectExpr) or
      ExpressionContainsDirectEval(
        TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).Value));

  Result := False;
end;

function ExpressionContainsSuspension(const AExpr: TGocciaExpression): Boolean;
var
  CallExpr: TGocciaCallExpression;
  MemberExpr: TGocciaMemberExpression;
  ArrayExpr: TGocciaArrayExpression;
  ObjectExpr: TGocciaObjectExpression;
  NewExpr: TGocciaNewExpression;
  ImportExpr: TGocciaImportCallExpression;
  Pair: TPair<TGocciaExpression, TGocciaExpression>;
  I: Integer;
begin
  if not Assigned(AExpr) then
    Exit(False);

  if (AExpr is TGocciaYieldExpression) or
     (AExpr is TGocciaAwaitExpression) then
    Exit(True);

  if (AExpr is TGocciaFunctionExpression) or
     (AExpr is TGocciaArrowFunctionExpression) then
    Exit(False);

  if AExpr is TGocciaCallExpression then
  begin
    CallExpr := TGocciaCallExpression(AExpr);
    if ExpressionContainsSuspension(CallExpr.Callee) then
      Exit(True);
    for I := 0 to CallExpr.Arguments.Count - 1 do
      if ExpressionContainsSuspension(CallExpr.Arguments[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(AExpr);
    if ExpressionContainsSuspension(MemberExpr.ObjectExpr) then
      Exit(True);
    if MemberExpr.Computed and
       ExpressionContainsSuspension(MemberExpr.PropertyExpression) then
      Exit(True);
  end
  else if AExpr is TGocciaBinaryExpression then
    Exit(ExpressionContainsSuspension(TGocciaBinaryExpression(AExpr).Left) or
      ExpressionContainsSuspension(TGocciaBinaryExpression(AExpr).Right))
  else if AExpr is TGocciaSequenceExpression then
  begin
    for I := 0 to TGocciaSequenceExpression(AExpr).Expressions.Count - 1 do
      if ExpressionContainsSuspension(
        TGocciaSequenceExpression(AExpr).Expressions[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaUnaryExpression then
    Exit(ExpressionContainsSuspension(TGocciaUnaryExpression(AExpr).Operand))
  else if AExpr is TGocciaAssignmentExpression then
    Exit(ExpressionContainsSuspension(TGocciaAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaPropertyAssignmentExpression then
    Exit(ExpressionContainsSuspension(
      TGocciaPropertyAssignmentExpression(AExpr).ObjectExpr) or
      ExpressionContainsSuspension(
        TGocciaPropertyAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaComputedPropertyAssignmentExpression then
    Exit(ExpressionContainsSuspension(
      TGocciaComputedPropertyAssignmentExpression(AExpr).ObjectExpr) or
      ExpressionContainsSuspension(
        TGocciaComputedPropertyAssignmentExpression(AExpr).PropertyExpression) or
      ExpressionContainsSuspension(
        TGocciaComputedPropertyAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaCompoundAssignmentExpression then
    Exit(ExpressionContainsSuspension(
      TGocciaCompoundAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaPropertyCompoundAssignmentExpression then
    Exit(ExpressionContainsSuspension(
      TGocciaPropertyCompoundAssignmentExpression(AExpr).ObjectExpr) or
      ExpressionContainsSuspension(
        TGocciaPropertyCompoundAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaComputedPropertyCompoundAssignmentExpression then
    Exit(ExpressionContainsSuspension(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).ObjectExpr) or
      ExpressionContainsSuspension(
        TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).PropertyExpression) or
      ExpressionContainsSuspension(
        TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaIncrementExpression then
    Exit(ExpressionContainsSuspension(TGocciaIncrementExpression(AExpr).Operand))
  else if AExpr is TGocciaArrayExpression then
  begin
    ArrayExpr := TGocciaArrayExpression(AExpr);
    for I := 0 to ArrayExpr.Elements.Count - 1 do
      if ExpressionContainsSuspension(ArrayExpr.Elements[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaObjectExpression then
  begin
    ObjectExpr := TGocciaObjectExpression(AExpr);
    for I := 0 to High(ObjectExpr.PropertySourceOrder) do
      case ObjectExpr.PropertySourceOrder[I].PropertyType of
        pstStatic:
          if ExpressionContainsSuspension(
            ObjectExpr.PropertySourceOrder[I].Expression) then
            Exit(True);
        pstComputed:
          begin
            Pair := ObjectExpr.ComputedPropertiesInOrder[
              ObjectExpr.PropertySourceOrder[I].ComputedIndex];
            if ExpressionContainsSuspension(Pair.Key) or
               ExpressionContainsSuspension(Pair.Value) then
              Exit(True);
          end;
        pstComputedGetter,
        pstComputedSetter:
          begin
            Pair := ObjectExpr.ComputedPropertiesInOrder[
              ObjectExpr.PropertySourceOrder[I].ComputedIndex];
            if ExpressionContainsSuspension(Pair.Key) then
              Exit(True);
          end;
      end;
  end
  else if AExpr is TGocciaConditionalExpression then
    Exit(ExpressionContainsSuspension(TGocciaConditionalExpression(AExpr).Condition) or
      ExpressionContainsSuspension(TGocciaConditionalExpression(AExpr).Consequent) or
      ExpressionContainsSuspension(TGocciaConditionalExpression(AExpr).Alternate))
  else if AExpr is TGocciaNewExpression then
  begin
    NewExpr := TGocciaNewExpression(AExpr);
    if ExpressionContainsSuspension(NewExpr.Callee) then
      Exit(True);
    for I := 0 to NewExpr.Arguments.Count - 1 do
      if ExpressionContainsSuspension(NewExpr.Arguments[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaImportCallExpression then
  begin
    ImportExpr := TGocciaImportCallExpression(AExpr);
    Exit(ExpressionContainsSuspension(ImportExpr.Specifier) or
      ExpressionContainsSuspension(ImportExpr.Options));
  end
  else if AExpr is TGocciaSpreadExpression then
    Exit(ExpressionContainsSuspension(TGocciaSpreadExpression(AExpr).Argument))
  else if AExpr is TGocciaTemplateWithInterpolationExpression then
  begin
    for I := 0 to TGocciaTemplateWithInterpolationExpression(AExpr).Parts.Count - 1 do
      if ExpressionContainsSuspension(
        TGocciaTemplateWithInterpolationExpression(AExpr).Parts[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaTaggedTemplateExpression then
  begin
    if ExpressionContainsSuspension(TGocciaTaggedTemplateExpression(AExpr).Tag) then
      Exit(True);
    for I := 0 to TGocciaTaggedTemplateExpression(AExpr).Expressions.Count - 1 do
      if ExpressionContainsSuspension(
        TGocciaTaggedTemplateExpression(AExpr).Expressions[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaDestructuringAssignmentExpression then
    Exit(ExpressionContainsSuspension(
      TGocciaDestructuringAssignmentExpression(AExpr).Right))
  else if AExpr is TGocciaPrivateMemberExpression then
    Exit(ExpressionContainsSuspension(
      TGocciaPrivateMemberExpression(AExpr).ObjectExpr))
  else if AExpr is TGocciaPrivatePropertyAssignmentExpression then
    Exit(ExpressionContainsSuspension(
      TGocciaPrivatePropertyAssignmentExpression(AExpr).ObjectExpr) or
      ExpressionContainsSuspension(
        TGocciaPrivatePropertyAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaPrivatePropertyCompoundAssignmentExpression then
    Exit(ExpressionContainsSuspension(
      TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).ObjectExpr) or
      ExpressionContainsSuspension(
        TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).Value));

  Result := False;
end;

function ExpressionCreatesClosureBoundary(const AExpr: TGocciaExpression): Boolean;
var
  CallExpr: TGocciaCallExpression;
  MemberExpr: TGocciaMemberExpression;
  ArrayExpr: TGocciaArrayExpression;
  ObjectExpr: TGocciaObjectExpression;
  NewExpr: TGocciaNewExpression;
  ImportExpr: TGocciaImportCallExpression;
  Pair: TPair<TGocciaExpression, TGocciaExpression>;
  I: Integer;
begin
  if not Assigned(AExpr) then
    Exit(False);

  if (AExpr is TGocciaFunctionExpression) or
     (AExpr is TGocciaArrowFunctionExpression) or
     (AExpr is TGocciaObjectMethodDefinition) or
     (AExpr is TGocciaClassExpression) or
     (AExpr is TGocciaGetterExpression) or
     (AExpr is TGocciaSetterExpression) or
     (AExpr is TGocciaIsExpression) or
     (AExpr is TGocciaMatchExpression) then
    Exit(True);

  if AExpr is TGocciaCallExpression then
  begin
    CallExpr := TGocciaCallExpression(AExpr);
    if ExpressionCreatesClosureBoundary(CallExpr.Callee) then
      Exit(True);
    for I := 0 to CallExpr.Arguments.Count - 1 do
      if ExpressionCreatesClosureBoundary(CallExpr.Arguments[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(AExpr);
    if ExpressionCreatesClosureBoundary(MemberExpr.ObjectExpr) then
      Exit(True);
    if MemberExpr.Computed and
       ExpressionCreatesClosureBoundary(MemberExpr.PropertyExpression) then
      Exit(True);
  end
  else if AExpr is TGocciaBinaryExpression then
    Exit(ExpressionCreatesClosureBoundary(TGocciaBinaryExpression(AExpr).Left) or
      ExpressionCreatesClosureBoundary(TGocciaBinaryExpression(AExpr).Right))
  else if AExpr is TGocciaSequenceExpression then
  begin
    for I := 0 to TGocciaSequenceExpression(AExpr).Expressions.Count - 1 do
      if ExpressionCreatesClosureBoundary(
        TGocciaSequenceExpression(AExpr).Expressions[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaUnaryExpression then
    Exit(ExpressionCreatesClosureBoundary(TGocciaUnaryExpression(AExpr).Operand))
  else if AExpr is TGocciaAssignmentExpression then
    Exit(ExpressionCreatesClosureBoundary(TGocciaAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaPropertyAssignmentExpression then
    Exit(ExpressionCreatesClosureBoundary(
      TGocciaPropertyAssignmentExpression(AExpr).ObjectExpr) or
      ExpressionCreatesClosureBoundary(
        TGocciaPropertyAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaComputedPropertyAssignmentExpression then
    Exit(ExpressionCreatesClosureBoundary(
      TGocciaComputedPropertyAssignmentExpression(AExpr).ObjectExpr) or
      ExpressionCreatesClosureBoundary(
        TGocciaComputedPropertyAssignmentExpression(AExpr).PropertyExpression) or
      ExpressionCreatesClosureBoundary(
        TGocciaComputedPropertyAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaCompoundAssignmentExpression then
    Exit(ExpressionCreatesClosureBoundary(
      TGocciaCompoundAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaPropertyCompoundAssignmentExpression then
    Exit(ExpressionCreatesClosureBoundary(
      TGocciaPropertyCompoundAssignmentExpression(AExpr).ObjectExpr) or
      ExpressionCreatesClosureBoundary(
        TGocciaPropertyCompoundAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaComputedPropertyCompoundAssignmentExpression then
    Exit(ExpressionCreatesClosureBoundary(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).ObjectExpr) or
      ExpressionCreatesClosureBoundary(
        TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).PropertyExpression) or
      ExpressionCreatesClosureBoundary(
        TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaIncrementExpression then
    Exit(ExpressionCreatesClosureBoundary(TGocciaIncrementExpression(AExpr).Operand))
  else if AExpr is TGocciaArrayExpression then
  begin
    ArrayExpr := TGocciaArrayExpression(AExpr);
    for I := 0 to ArrayExpr.Elements.Count - 1 do
      if ExpressionCreatesClosureBoundary(ArrayExpr.Elements[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaObjectExpression then
  begin
    ObjectExpr := TGocciaObjectExpression(AExpr);
    for I := 0 to High(ObjectExpr.PropertySourceOrder) do
      case ObjectExpr.PropertySourceOrder[I].PropertyType of
        pstStatic:
          if ExpressionCreatesClosureBoundary(
            ObjectExpr.PropertySourceOrder[I].Expression) then
            Exit(True);
        pstComputed:
          begin
            Pair := ObjectExpr.ComputedPropertiesInOrder[
              ObjectExpr.PropertySourceOrder[I].ComputedIndex];
            if ExpressionCreatesClosureBoundary(Pair.Key) or
               ExpressionCreatesClosureBoundary(Pair.Value) then
              Exit(True);
          end;
        pstGetter,
        pstSetter,
        pstComputedGetter,
        pstComputedSetter:
          Exit(True);
      end;
  end
  else if AExpr is TGocciaYieldExpression then
    Exit(ExpressionCreatesClosureBoundary(TGocciaYieldExpression(AExpr).Operand))
  else if AExpr is TGocciaAwaitExpression then
    Exit(ExpressionCreatesClosureBoundary(TGocciaAwaitExpression(AExpr).Operand))
  else if AExpr is TGocciaConditionalExpression then
    Exit(ExpressionCreatesClosureBoundary(TGocciaConditionalExpression(AExpr).Condition) or
      ExpressionCreatesClosureBoundary(TGocciaConditionalExpression(AExpr).Consequent) or
      ExpressionCreatesClosureBoundary(TGocciaConditionalExpression(AExpr).Alternate))
  else if AExpr is TGocciaNewExpression then
  begin
    NewExpr := TGocciaNewExpression(AExpr);
    if ExpressionCreatesClosureBoundary(NewExpr.Callee) then
      Exit(True);
    for I := 0 to NewExpr.Arguments.Count - 1 do
      if ExpressionCreatesClosureBoundary(NewExpr.Arguments[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaImportCallExpression then
  begin
    ImportExpr := TGocciaImportCallExpression(AExpr);
    Exit(ExpressionCreatesClosureBoundary(ImportExpr.Specifier) or
      ExpressionCreatesClosureBoundary(ImportExpr.Options));
  end
  else if AExpr is TGocciaSpreadExpression then
    Exit(ExpressionCreatesClosureBoundary(TGocciaSpreadExpression(AExpr).Argument))
  else if AExpr is TGocciaTemplateWithInterpolationExpression then
  begin
    for I := 0 to TGocciaTemplateWithInterpolationExpression(AExpr).Parts.Count - 1 do
      if ExpressionCreatesClosureBoundary(
        TGocciaTemplateWithInterpolationExpression(AExpr).Parts[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaTaggedTemplateExpression then
  begin
    if ExpressionCreatesClosureBoundary(TGocciaTaggedTemplateExpression(AExpr).Tag) then
      Exit(True);
    for I := 0 to TGocciaTaggedTemplateExpression(AExpr).Expressions.Count - 1 do
      if ExpressionCreatesClosureBoundary(
        TGocciaTaggedTemplateExpression(AExpr).Expressions[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaDestructuringAssignmentExpression then
    Exit(ExpressionCreatesClosureBoundary(
      TGocciaDestructuringAssignmentExpression(AExpr).Right))
  else if AExpr is TGocciaPrivateMemberExpression then
    Exit(ExpressionCreatesClosureBoundary(
      TGocciaPrivateMemberExpression(AExpr).ObjectExpr))
  else if AExpr is TGocciaPrivatePropertyAssignmentExpression then
    Exit(ExpressionCreatesClosureBoundary(
      TGocciaPrivatePropertyAssignmentExpression(AExpr).ObjectExpr) or
      ExpressionCreatesClosureBoundary(
        TGocciaPrivatePropertyAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaPrivatePropertyCompoundAssignmentExpression then
    Exit(ExpressionCreatesClosureBoundary(
      TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).ObjectExpr) or
      ExpressionCreatesClosureBoundary(
        TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).Value));

  Result := False;
end;

function ParameterDefaultsContainDirectEval(
  const AParams: TGocciaParameterArray): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AParams) do
    if Assigned(AParams[I].DefaultValue) and
       ExpressionContainsDirectEval(AParams[I].DefaultValue) then
      Exit(True);
  Result := False;
end;

function ParameterListHasDefaultValues(
  const AParams: TGocciaParameterArray): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AParams) do
    if Assigned(AParams[I].DefaultValue) then
      Exit(True);
  Result := False;
end;

function IsDirectEvalVisibleCompilerName(const AName: string): Boolean;
begin
  Result := (AName <> '') and
    (AName <> KEYWORD_THIS) and
    (AName <> '__receiver') and
    (AName <> '__super__') and
    (AName <> DERIVED_THIS_INITIALIZED_LOCAL) and
    (Copy(AName, 1, 1) <> '#') and
    (Copy(AName, 1, 7) <> '__param') and
    (Copy(AName, 1, 19) <> '__accessor_computed');
end;

function DirectEvalBindingNameSeen(const ANames: TUnicodeStringList;
  const AName: string): Boolean;
begin
  Result := ANames.IndexOf(AName) >= 0;
end;

procedure AddDirectEvalBinding(var ABindings: TGocciaDirectEvalBindingArray;
  const ANames: TUnicodeStringList; const AName: string;
  const AKind: TGocciaDirectEvalBindingKind; const AIndex: UInt16;
  const AIsConst: Boolean; const AIsVarEnvironmentBinding: Boolean = False;
  const AIsEvalSyntheticArguments: Boolean = False);
var
  BindingIndex: Integer;
begin
  if (AName <> DERIVED_THIS_INITIALIZED_LOCAL) and
     (AKind <> debWithLocal) and (AKind <> debWithUpvalue) and
     (AName <> KEYWORD_THIS) and
     not IsDirectEvalVisibleCompilerName(AName) then
    Exit;
  if DirectEvalBindingNameSeen(ANames, AName) then
    Exit;

  ANames.Add(AName);
  BindingIndex := Length(ABindings);
  SetLength(ABindings, BindingIndex + 1);
  ABindings[BindingIndex].Name := AName;
  ABindings[BindingIndex].Kind := AKind;
  ABindings[BindingIndex].Index := AIndex;
  ABindings[BindingIndex].IsConst := AIsConst;
  ABindings[BindingIndex].IsVarEnvironmentBinding :=
    AIsVarEnvironmentBinding;
  ABindings[BindingIndex].IsEvalSyntheticArguments :=
    AIsEvalSyntheticArguments;
end;

procedure EmitDerivedThisInitializedCheck(
  const ACtx: TGocciaCompilationContext);
var
  FlagReg: UInt16;
  LocalIdx, UpvalueIdx: Integer;
  Local: TGocciaCompilerLocal;
  OkJump: Integer;
begin
  if not ACtx.DerivedConstructorThisGuard then
    Exit;

  UpvalueIdx := -1;
  FlagReg := ACtx.Scope.AllocateRegister;
  try
    LocalIdx := ACtx.Scope.ResolveLocal(DERIVED_THIS_INITIALIZED_LOCAL);
    if LocalIdx >= 0 then
    begin
      Local := ACtx.Scope.GetLocal(LocalIdx);
      if Local.IsCaptured then
        EmitInstruction(ACtx, EncodeABx(OP_GET_LOCAL, FlagReg,
          UInt16(Local.Slot)))
      else if Local.Slot <> FlagReg then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, FlagReg, Local.Slot, 0));
    end
    else
    begin
      UpvalueIdx := ACtx.Scope.ResolveUpvalue(DERIVED_THIS_INITIALIZED_LOCAL);
      if UpvalueIdx >= 0 then
        EmitInstruction(ACtx, EncodeABx(OP_GET_UPVALUE, FlagReg,
          UInt16(UpvalueIdx)))
      else
        EmitInstruction(ACtx, EncodeABC(OP_CHECK_DERIVED_THIS, 0, 0, 0));
    end;

    if (LocalIdx >= 0) or (UpvalueIdx >= 0) then
    begin
      OkJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, FlagReg);
      EmitReferenceError(ACtx,
        'Must call super constructor before accessing this');
      PatchJumpTarget(ACtx, OkJump);
    end;
  finally
    ACtx.Scope.FreeRegister;
  end;
end;

procedure EmitSetDerivedThisInitialized(
  const ACtx: TGocciaCompilationContext);
var
  FlagReg: UInt16;
  LocalIdx, UpvalueIdx: Integer;
  Local: TGocciaCompilerLocal;
begin
  LocalIdx := ACtx.Scope.ResolveLocal(DERIVED_THIS_INITIALIZED_LOCAL);
  UpvalueIdx := -1;
  if LocalIdx < 0 then
    UpvalueIdx := ACtx.Scope.ResolveUpvalue(DERIVED_THIS_INITIALIZED_LOCAL);
  if (LocalIdx < 0) and (UpvalueIdx < 0) then
    Exit;

  FlagReg := ACtx.Scope.AllocateRegister;
  try
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_TRUE, FlagReg, 0, 0));
    if LocalIdx >= 0 then
    begin
      Local := ACtx.Scope.GetLocal(LocalIdx);
      if Local.Slot <> FlagReg then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, Local.Slot, FlagReg, 0));
      if Local.IsCaptured then
        EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, Local.Slot,
          UInt16(Local.Slot)));
    end
    else
      EmitInstruction(ACtx, EncodeABx(OP_SET_UPVALUE, FlagReg,
        UInt16(UpvalueIdx)));
  finally
    ACtx.Scope.FreeRegister;
  end;
end;

procedure CaptureDirectEvalEnvironment(const ACtx: TGocciaCompilationContext;
  const APC: UInt32);
var
  Bindings: TGocciaDirectEvalBindingArray;
  Names: TUnicodeStringList;
  ScopeCursor: TGocciaCompilerScope;
  Local: TGocciaCompilerLocal;
  UV: TGocciaCompilerUpvalue;
  LocalIdx, UpvalueIdx: Integer;
  IsEvalSensitiveArguments: Boolean;
begin
  Names := TUnicodeStringList.Create;
  try
    for LocalIdx := ACtx.Scope.LocalCount - 1 downto 0 do
    begin
      Local := ACtx.Scope.GetLocal(LocalIdx);
      if HiddenWithBindingName(Local.Name) then
        AddDirectEvalBinding(Bindings, Names, Local.Name, debWithLocal,
          Local.Slot, False)
      else if Local.IsGlobalBacked then
        AddDirectEvalBinding(Bindings, Names, Local.Name, debGlobal, 0,
          Local.IsConst, Local.IsVar)
      else
      begin
        IsEvalSensitiveArguments := (Local.Name = IDENTIFIER_ARGUMENTS) and
          ((ACtx.Template.DirectEvalSyntheticArgumentsSlot = Local.Slot) or
           (not ACtx.Template.IsArrow));
        AddDirectEvalBinding(Bindings, Names, Local.Name, debLocal,
          Local.Slot, Local.IsConst, Local.IsVar or IsEvalSensitiveArguments,
          IsEvalSensitiveArguments or (Local.IsConst and
          Local.IsNonStrictImmutable));
      end;
    end;

    ScopeCursor := ACtx.Scope.Parent;
    while Assigned(ScopeCursor) do
    begin
      for LocalIdx := ScopeCursor.LocalCount - 1 downto 0 do
      begin
        Local := ScopeCursor.GetLocal(LocalIdx);
        if DirectEvalBindingNameSeen(Names, Local.Name) then
          Continue;
        if Local.Name = DERIVED_THIS_INITIALIZED_LOCAL then
        begin
          UpvalueIdx := ACtx.Scope.ResolveUpvalue(Local.Name);
          if UpvalueIdx >= 0 then
          begin
            UV := ACtx.Scope.GetUpvalue(UpvalueIdx);
            AddDirectEvalBinding(Bindings, Names, Local.Name, debUpvalue,
              UInt16(UpvalueIdx), UV.IsConst, UV.IsVar,
              UV.IsNonStrictImmutable);
          end;
          Continue;
        end;
        if HiddenWithBindingName(Local.Name) then
        begin
          UpvalueIdx := ACtx.Scope.ResolveUpvalue(Local.Name);
          if UpvalueIdx >= 0 then
            AddDirectEvalBinding(Bindings, Names, Local.Name, debWithUpvalue,
              UInt16(UpvalueIdx), False);
          Continue;
        end;
        if (Local.Name <> KEYWORD_THIS) and
           not IsDirectEvalVisibleCompilerName(Local.Name) then
          Continue;
        if Local.IsGlobalBacked then
          AddDirectEvalBinding(Bindings, Names, Local.Name, debGlobal, 0,
            Local.IsConst, Local.IsVar)
        else
        begin
          UpvalueIdx := ACtx.Scope.ResolveUpvalue(Local.Name);
          if UpvalueIdx >= 0 then
          begin
            UV := ACtx.Scope.GetUpvalue(UpvalueIdx);
            AddDirectEvalBinding(Bindings, Names, Local.Name, debUpvalue,
              UInt16(UpvalueIdx), UV.IsConst, UV.IsVar,
              UV.IsNonStrictImmutable);
          end;
        end;
      end;
      ScopeCursor := ScopeCursor.Parent;
    end;

    ACtx.Template.AddDirectEvalEnvironment(APC,
      ACtx.Template.RejectArgumentsInDirectEval, Bindings);
  finally
    Names.Free;
  end;
end;

function CallFlags(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaCallExpression; const ACallPC: UInt32): UInt16;
begin
  Result := CallTrustedFlag(ACtx, AExpr);
  if IsDirectEvalCall(AExpr) then
  begin
    CaptureDirectEvalEnvironment(ACtx, ACallPC);
    Result := Result or CALL_FLAG_DIRECT_EVAL;
  end;
end;

function SpreadCallFlags(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaCallExpression; const ACallPC: UInt32): UInt16;
begin
  Result := CALL_FLAG_SPREAD;
  if IsDirectEvalCall(AExpr) then
  begin
    CaptureDirectEvalEnvironment(ACtx, ACallPC);
    Result := Result or CALL_FLAG_DIRECT_EVAL;
  end;
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
  const AExpr: TGocciaBinaryExpression; const ADest: UInt16);
var
  RegB, RegC: UInt16;
  Op, IntOp, FloatOp: TGocciaOpCode;
  JumpIdx, JumpIdx2: Integer;
  LeftType, RightType: TGocciaLocalType;
  PrivateExpr: TGocciaPrivateMemberExpression;
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

  if (AExpr.Operator = gttIn) and
     (AExpr.Left is TGocciaPrivateMemberExpression) then
  begin
    PrivateExpr := TGocciaPrivateMemberExpression(AExpr.Left);
    RegB := ACtx.Scope.AllocateRegister;
    RegC := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(AExpr.Right, RegB);
    EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, RegC,
      ACtx.Template.AddConstantString(PrivateKey(
      ACtx.Scope, PrivateExpr.PrivateName))));
    EmitInstruction(ACtx, EncodeABC(OP_HAS_PROPERTY, ADest, RegB, RegC));
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
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
    if AExpr.Operator = gttIn then
      EmitInstruction(ACtx, EncodeABC(Op, ADest, RegC, RegB))
    else
      EmitInstruction(ACtx, EncodeABC(Op, ADest, RegB, RegC));
  end;

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileDelete(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaUnaryExpression; const ADest: UInt16);
var
  MemberExpr: TGocciaMemberExpression;
  IdentExpr: TGocciaIdentifierExpression;
  ObjReg, KeyReg: UInt16;
  PropIdx: UInt16;
  EndJump, JumpIndex: Integer;
  NullishJumps: TGocciaCompilerJumpArray;
  NullishJumpCount: Integer;

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
    WithObjReg, WithKeyReg, CondReg: UInt16;
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
    if MemberExpr.ObjectExpr is TGocciaSuperExpression then
    begin
      ObjReg := ACtx.Scope.AllocateRegister;
      try
        CompileThis(ACtx, ObjReg);
        if MemberExpr.Computed and Assigned(MemberExpr.PropertyExpression) then
          ACtx.CompileExpression(MemberExpr.PropertyExpression, ObjReg);
        EmitReferenceError(ACtx, 'Cannot delete super property');
        EmitInstruction(ACtx, EncodeABC(OP_LOAD_TRUE, ADest, 0, 0));
      finally
        ACtx.Scope.FreeRegister;
      end;
      Exit;
    end;

    ObjReg := ACtx.Scope.AllocateRegister;
    NullishJumpCount := 0;
    CompileExpressionWithOptionalChainJumps(ACtx, MemberExpr.ObjectExpr,
      ObjReg, NullishJumps, NullishJumpCount);

    if MemberExpr.Optional then
      AddOptionalChainJump(ACtx, NullishJumps, NullishJumpCount, ObjReg);

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

    if NullishJumpCount > 0 then
    begin
      EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
      for JumpIndex := 0 to NullishJumpCount - 1 do
        PatchJumpTarget(ACtx, NullishJumps[JumpIndex]);
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
  begin
    ObjReg := ACtx.Scope.AllocateRegister;
    try
      ACtx.CompileExpression(AExpr.Operand, ObjReg);
      EmitInstruction(ACtx, EncodeABC(OP_LOAD_TRUE, ADest, 0, 0));
    finally
      ACtx.Scope.FreeRegister;
    end;
  end;
end;

procedure CompileUnary(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaUnaryExpression; const ADest: UInt16);
var
  RegB: UInt16;
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
  const AExpr: TGocciaSequenceExpression; const ADest: UInt16);
var
  I: Integer;
  TempReg: UInt16;
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

function PrepareUpvalueAssignmentReference(
  const ACtx: TGocciaCompilationContext; const AUpvalueIndex: Integer): UInt16;
begin
  Result := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABC(OP_RESOLVE_UPVALUE_REF, Result,
    UInt16(AUpvalueIndex), UInt16(Ord(not ACtx.NonStrictMode))));
end;

procedure EmitPreparedUpvalueLoad(const ACtx: TGocciaCompilationContext;
  const AUpvalue: TGocciaCompilerUpvalue; const AUpvalueIndex: Integer;
  const AReferenceReg, ADest: UInt16);
var
  StaticJump, EndJump: Integer;
begin
  if not AUpvalue.IsGlobalBacked then
  begin
    EmitInstruction(ACtx, EncodeABx(OP_GET_UPVALUE, ADest,
      UInt16(AUpvalueIndex)));
    Exit;
  end;

  StaticJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH, AReferenceReg,
    GOCCIA_NULLISH_MATCH_UNDEFINED);
  EmitInstruction(ACtx, EncodeABx(OP_GET_UPVALUE, ADest,
    UInt16(AUpvalueIndex)));
  EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
  PatchJumpTarget(ACtx, StaticJump);
  EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, ADest,
    ACtx.Template.AddConstantString(AUpvalue.Name)));
  PatchJumpTarget(ACtx, EndJump);
end;

procedure EmitPreparedUpvalueAssignment(
  const ACtx: TGocciaCompilationContext;
  const AUpvalue: TGocciaCompilerUpvalue; const AUpvalueIndex: Integer;
  const AReferenceReg, AValueReg, ATypeCheckReg: UInt16;
  const AValueType: TGocciaLocalType);
var
  StaticJump, EndJump: Integer;
begin
  // ES2026 §13.15.2: PutValue uses the LeftHandSideExpression Reference
  // captured before the right-hand side is evaluated.
  StaticJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH, AReferenceReg,
    GOCCIA_NULLISH_MATCH_UNDEFINED);
  EmitInstruction(ACtx, EncodeABC(OP_SET_UPVALUE_REF, AValueReg,
    AReferenceReg, UInt16(AUpvalueIndex)));
  EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
  PatchJumpTarget(ACtx, StaticJump);

  if AUpvalue.IsConst then
  begin
    if not ShouldIgnoreNonStrictImmutableUpvalueAssignment(ACtx, AUpvalue) then
      EmitConstAssignmentError(ACtx);
  end
  else
  begin
    if AUpvalue.IsStrictlyTyped and
       not TypesAreCompatible(AValueType, AUpvalue.TypeHint) then
      EmitInstruction(ACtx, EncodeABC(OP_CHECK_TYPE, ATypeCheckReg,
        UInt8(Ord(AUpvalue.TypeHint)), 0));
    if AUpvalue.IsGlobalBacked then
      EmitInstruction(ACtx, EncodeABx(OP_SET_GLOBAL_STATIC, AValueReg,
        ACtx.Template.AddConstantString(AUpvalue.Name)))
    else
      EmitInstruction(ACtx, EncodeABC(OP_SET_UPVALUE_REF, AValueReg,
        AReferenceReg, UInt16(AUpvalueIndex)));
    EmitExportBindingUpdates(ACtx, AUpvalue.ExportNames,
      AUpvalue.ExportNameCount, AValueReg);
  end;

  PatchJumpTarget(ACtx, EndJump);
end;

procedure EmitCurrentUpvalueAssignment(
  const ACtx: TGocciaCompilationContext;
  const AUpvalue: TGocciaCompilerUpvalue; const AUpvalueIndex: Integer;
  const AValueReg, ATypeCheckReg: UInt16;
  const AValueType: TGocciaLocalType);
var
  ReferenceReg: UInt16;
begin
  if AUpvalue.IsConst or AUpvalue.IsStrictlyTyped or
     (AUpvalue.ExportNameCount > 0) then
  begin
    ReferenceReg := PrepareUpvalueAssignmentReference(ACtx, AUpvalueIndex);
    EmitPreparedUpvalueAssignment(ACtx, AUpvalue, AUpvalueIndex,
      ReferenceReg, AValueReg, ATypeCheckReg, AValueType);
    ACtx.Scope.FreeRegister;
  end
  else if AUpvalue.IsGlobalBacked then
    EmitSetGlobalByName(ACtx, AValueReg, AUpvalue.Name)
  else
    EmitInstruction(ACtx, EncodeABx(OP_SET_UPVALUE_DYNAMIC, AValueReg,
      UInt16(AUpvalueIndex)));
end;

procedure CompileAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaAssignmentExpression; const ADest: UInt16);
var
  LocalIdx, UpvalIdx: Integer;
  Local: TGocciaCompilerLocal;
  Upvalue: TGocciaCompilerUpvalue;
  Slot, GlobalExistsReg, ErrorReg, MessageReg: UInt16;
  ObjReg, KeyReg, CondReg: UInt16;
  TargetReg: Integer;
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

      CompileAssignmentValue(ACtx, AExpr.Value, ADest, AExpr.Name,
        AExpr.InferName);
      MissJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH, TargetReg,
        GOCCIA_NULLISH_MATCH_UNDEFINED);
      EmitInstruction(ACtx, EncodeABC(SetWithBindingOpcode(ACtx), TargetReg,
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
  TargetReg := -1;

  if LocalIdx < 0 then
  begin
    UpvalIdx := ACtx.Scope.ResolveUpvalue(AExpr.Name);
    if UpvalIdx < 0 then
    begin
      NameIdx := ACtx.Template.AddConstantString(AExpr.Name);
      if ACtx.NonStrictMode then
      begin
        CompileAssignmentValue(ACtx, AExpr.Value, ADest, AExpr.Name,
          AExpr.InferName);
        EmitSetGlobalByIndex(ACtx, ADest, NameIdx);
        Exit;
      end;
      GlobalExistsReg := ACtx.Scope.AllocateRegister;
      EmitInstruction(ACtx, EncodeABx(OP_HAS_GLOBAL, GlobalExistsReg, NameIdx));
    end;
  end;

  if (UpvalIdx >= 0) and ExpressionContainsDirectEval(AExpr.Value) then
    TargetReg := PrepareUpvalueAssignmentReference(ACtx, UpvalIdx);

  CompileAssignmentValue(ACtx, AExpr.Value, ADest, AExpr.Name,
    AExpr.InferName);

  if LocalIdx >= 0 then
  begin
    Local := ACtx.Scope.GetLocal(LocalIdx);
    if not Local.IsGlobalBacked then
    begin
      // PutValue on an uninitialized lexical binding throws after the RHS has
      // been evaluated. Probe the destination without disturbing ADest.
      ErrorReg := ACtx.Scope.AllocateRegister;
      EmitInstruction(ACtx, EncodeABx(OP_GET_LOCAL, ErrorReg, Local.Slot));
      ACtx.Scope.FreeRegister;
    end;
    if Local.IsConst then
    begin
      if ShouldIgnoreNonStrictImmutableLocalAssignment(ACtx, Local) then
        Exit;
      EmitConstAssignmentError(ACtx);
      Exit;
    end;
    if Local.IsGlobalBacked then
    begin
      EmitStrictLocalTypeCheck(ACtx, LocalIdx, ADest,
        InferLocalType(AExpr.Value));
      EmitSetGlobalByName(ACtx, ADest, AExpr.Name);
      EmitExportBindingUpdates(ACtx, Local.ExportNames,
        Local.ExportNameCount, ADest);
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
    EmitExportBindingUpdates(ACtx, Local.ExportNames,
      Local.ExportNameCount, ADest);
    if not Local.IsStrictlyTyped then
    begin
      ValueType := InferredExpressionType(ACtx.Scope, AExpr.Value);
      SetNonStrictLocalTypeHint(ACtx, LocalIdx, ValueType);
    end;
    Exit;
  end;

  if UpvalIdx >= 0 then
  begin
    Upvalue := ACtx.Scope.GetUpvalue(UpvalIdx);
    if TargetReg >= 0 then
    begin
      EmitPreparedUpvalueAssignment(ACtx, Upvalue, UpvalIdx,
        UInt16(TargetReg), ADest, ADest, InferLocalType(AExpr.Value));
      ACtx.Scope.FreeRegister;
    end
    else
      EmitCurrentUpvalueAssignment(ACtx, Upvalue, UpvalIdx, ADest, ADest,
        InferLocalType(AExpr.Value));
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
  const AExpr: TGocciaPropertyAssignmentExpression; const ADest: UInt16);
var
  BaseReg, KeyReg, ObjReg, SuperReg, ThisReg, ValReg: UInt16;
  KeyIdx: UInt16;
begin
  if AExpr.ObjectExpr is TGocciaSuperExpression then
  begin
    PrepareSuperPropertyBase(ACtx, ThisReg, BaseReg, SuperReg);
    KeyReg := ACtx.Scope.AllocateRegister;
    ValReg := ACtx.Scope.AllocateRegister;
    KeyIdx := ACtx.Template.AddConstantString(AExpr.PropertyName);
    EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, KeyReg, KeyIdx));
    // PutValue(super reference, RHS): RHS is evaluated before a null super base
    // throws, so tests observe RHS side effects exactly once.
    ACtx.CompileExpression(AExpr.Value, ValReg);
    EmitInstruction(ACtx, EncodeABC(OP_SUPER_SET, BaseReg, KeyReg, ValReg));

    if ADest <> ValReg then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, ValReg, 0));

    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    Exit;
  end;

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
  const ASlot: UInt16; out AJumpIdx: Integer);
begin
  AJumpIdx := EmitJumpInstruction(ACtx, OP_JUMP_IF_NOT_NULLISH, ASlot,
    GOCCIA_NULLISH_MATCH_UNDEFINED);
end;

function DeclareArgumentsObjectLocal(const ACtx: TGocciaCompilationContext;
  const AScope: TGocciaCompilerScope; const AParams: TGocciaParameterArray;
  const ASourceText: string): Integer;
begin
  if not ACtx.ArgumentsObjectEnabled then
    Exit(-1);

  if ParameterListBindsName(AParams, IDENTIFIER_ARGUMENTS) then
    Exit(-1);

  // Skip the arguments local (and the OP_CREATE_ARGUMENTS it would trigger)
  // when the function body provably never references it; see
  // FunctionSourceMayReferenceArgumentsObject.
  if not FunctionSourceMayReferenceArgumentsObject(ASourceText) then
    Exit(-1);

  Result := AScope.DeclareVarLocal(IDENTIFIER_ARGUMENTS);
end;

function ParameterListIsSimple(const AParams: TGocciaParameterArray): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AParams) do
    if AParams[I].IsRest or AParams[I].IsPattern or
       Assigned(AParams[I].DefaultValue) then
      Exit(False);
  Result := True;
end;

function SyntheticParamLocalName(const AIndex: Integer): string;
begin
  Result := '`#param`:' + IntToStr(AIndex);
end;

procedure EmitCreateArgumentsObject(const ACtx: TGocciaCompilationContext;
  const AArgumentsSlot: Integer; const AUseMappedArguments: Boolean;
  const AFormalParameterCount: Integer);
var
  MappedFlag: UInt16;
begin
  if AArgumentsSlot >= 0 then
  begin
    if AUseMappedArguments then
      MappedFlag := 1
    else
      MappedFlag := 0;
    EmitInstruction(ACtx, EncodeABC(OP_CREATE_ARGUMENTS,
      UInt16(AArgumentsSlot), MappedFlag, UInt16(AFormalParameterCount)));
  end;
end;

procedure EmitDefaultParameters(const ACtx: TGocciaCompilationContext;
  const AParams: TGocciaParameterArray);
var
  I, LocalIdx: Integer;
  Slot: UInt16;
  JumpIdx: Integer;
  IsCaptured: Boolean;
begin
  if not ParameterListHasDefaultValues(AParams) then
    Exit;

  for I := 0 to High(AParams) do
  begin
    if AParams[I].IsPattern then
      LocalIdx := ACtx.Scope.ResolveLocal(SyntheticParamLocalName(I))
    else
      LocalIdx := ACtx.Scope.ResolveLocal(AParams[I].Name);
    if LocalIdx < 0 then
      Continue;
    Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_HOLE, Slot, 0, 0));
    if ACtx.Scope.GetLocal(LocalIdx).IsCaptured then
      EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, Slot, UInt16(Slot)));
  end;

  for I := 0 to High(AParams) do
  begin
    if AParams[I].IsPattern then
      LocalIdx := ACtx.Scope.ResolveLocal(SyntheticParamLocalName(I))
    else
      LocalIdx := ACtx.Scope.ResolveLocal(AParams[I].Name);
    if LocalIdx < 0 then
      Continue;
    Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
    IsCaptured := ACtx.Scope.GetLocal(LocalIdx).IsCaptured;

    if AParams[I].IsRest then
      EmitInstruction(ACtx, EncodeABC(OP_PACK_ARGS, Slot, UInt16(I), 0))
    else
      EmitInstruction(ACtx, EncodeABC(OP_LOAD_ARGUMENT, Slot, UInt16(I), 0));
    if IsCaptured then
      EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, Slot, UInt16(Slot)));

    if not Assigned(AParams[I].DefaultValue) then
      Continue;

    EmitUndefinedCheck(ACtx, Slot, JumpIdx);
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_HOLE, Slot, 0, 0));
    if IsCaptured then
      EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, Slot, UInt16(Slot)));
    if AParams[I].IsPattern then
      ACtx.CompileExpression(AParams[I].DefaultValue, Slot)
    else
      CompileExpressionWithInferredName(ACtx, AParams[I].DefaultValue, Slot,
        AParams[I].Name);
    if IsCaptured then
      EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, Slot, UInt16(Slot)));
    PatchJumpTarget(ACtx, JumpIdx);
  end;
end;

procedure CollectDestructuringBindings(const APattern: TGocciaDestructuringPattern;
  const AScope: TGocciaCompilerScope; const AIsConst: Boolean);
var
  Names: TUnicodeStringList;
  I, LocalIdx: Integer;
begin
  Names := TUnicodeStringList.Create;
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
  const AName: string; const AValueReg: UInt16;
  const AAssignmentMode: Boolean);
var
  LocalIdx, UpvalIdx: Integer;
  Local: TGocciaCompilerLocal;
  Upvalue: TGocciaCompilerUpvalue;
  Slot, CheckReg: UInt16;
begin
  LocalIdx := ACtx.Scope.ResolveLocal(AName);
  if LocalIdx >= 0 then
  begin
    Local := ACtx.Scope.GetLocal(LocalIdx);
    if AAssignmentMode and not Local.IsGlobalBacked then
    begin
      CheckReg := ACtx.Scope.AllocateRegister;
      EmitInstruction(ACtx, EncodeABx(OP_GET_LOCAL, CheckReg, Local.Slot));
      ACtx.Scope.FreeRegister;
    end;
    if AAssignmentMode and Local.IsConst then
    begin
      if ShouldIgnoreNonStrictImmutableLocalAssignment(ACtx, Local) then
        Exit;
      EmitConstAssignmentError(ACtx);
      Exit;
    end;
    if AAssignmentMode and Local.IsStrictlyTyped and
       (Local.TypeHint <> sltUntyped) then
      EmitInstruction(ACtx, EncodeABC(OP_CHECK_TYPE, AValueReg,
        UInt8(Ord(Local.TypeHint)), 0));
    if Local.IsGlobalBacked and AAssignmentMode then
    begin
      EmitSetGlobalByName(ACtx, AValueReg, AName);
      EmitExportBindingUpdates(ACtx, Local.ExportNames,
        Local.ExportNameCount, AValueReg);
      Exit;
    end;
    Slot := Local.Slot;
    if AAssignmentMode then
    begin
      if Slot <> AValueReg then
        EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, AValueReg, UInt16(Slot)))
      else if Local.IsCaptured then
        EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, Slot, UInt16(Slot)));
      EmitExportBindingUpdates(ACtx, Local.ExportNames,
        Local.ExportNameCount, AValueReg);
      Exit;
    end;
    if Slot <> AValueReg then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, Slot, AValueReg, 0));
    if Local.IsCaptured then
      EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, Slot, UInt16(Slot)));
    EmitExportBindingUpdates(ACtx, Local.ExportNames,
      Local.ExportNameCount, Slot);
    Exit;
  end;

  UpvalIdx := ACtx.Scope.ResolveUpvalue(AName);
  if UpvalIdx >= 0 then
  begin
    Upvalue := ACtx.Scope.GetUpvalue(UpvalIdx);
    if AAssignmentMode then
      EmitCurrentUpvalueAssignment(ACtx, Upvalue, UpvalIdx, AValueReg,
        AValueReg, sltUntyped)
    else
    begin
      if Upvalue.IsGlobalBacked then
        EmitSetGlobalByName(ACtx, AValueReg, AName)
      else
        EmitInstruction(ACtx, EncodeABx(OP_SET_UPVALUE, AValueReg,
          UInt16(UpvalIdx)));
      EmitExportBindingUpdates(ACtx, Upvalue.ExportNames,
        Upvalue.ExportNameCount, AValueReg);
    end;
    Exit;
  end;

  EmitSetGlobalByName(ACtx, AValueReg, AName);
end;

function DestructuringPatternHasSuspendingDefault(
  const APattern: TGocciaDestructuringPattern): Boolean;
var
  ArrPat: TGocciaArrayDestructuringPattern;
  ObjPat: TGocciaObjectDestructuringPattern;
  AssignPat: TGocciaAssignmentDestructuringPattern;
  I: Integer;
begin
  Result := False;
  if not Assigned(APattern) then
    Exit;

  if APattern is TGocciaAssignmentDestructuringPattern then
  begin
    AssignPat := TGocciaAssignmentDestructuringPattern(APattern);
    if ExpressionContainsSuspension(AssignPat.Right) then
      Exit(True);
    Exit(DestructuringPatternHasSuspendingDefault(AssignPat.Left));
  end;

  if APattern is TGocciaArrayDestructuringPattern then
  begin
    ArrPat := TGocciaArrayDestructuringPattern(APattern);
    for I := 0 to ArrPat.Elements.Count - 1 do
      if DestructuringPatternHasSuspendingDefault(ArrPat.Elements[I]) then
        Exit(True);
    Exit;
  end;

  if APattern is TGocciaObjectDestructuringPattern then
  begin
    ObjPat := TGocciaObjectDestructuringPattern(APattern);
    for I := 0 to ObjPat.Properties.Count - 1 do
      if DestructuringPatternHasSuspendingDefault(ObjPat.Properties[I].Pattern) then
        Exit(True);
  end;
end;

procedure InitPreparedDestructuringTarget(
  var ATarget: TPreparedDestructuringTarget);
begin
  ATarget.Kind := pdtNone;
  ATarget.ObjectReg := -1;
  ATarget.KeyReg := -1;
  ATarget.ThisReg := -1;
  ATarget.SuperReg := -1;
  ATarget.PropertyName := '';
end;

procedure ReleasePreparedDestructuringTarget(
  const ACtx: TGocciaCompilationContext;
  var ATarget: TPreparedDestructuringTarget);
begin
  if ATarget.KeyReg >= 0 then
    ACtx.Scope.FreeRegister;
  if ATarget.SuperReg >= 0 then
    ACtx.Scope.FreeRegister;
  if ATarget.ObjectReg >= 0 then
    ACtx.Scope.FreeRegister;
  if ATarget.ThisReg >= 0 then
    ACtx.Scope.FreeRegister;
  InitPreparedDestructuringTarget(ATarget);
end;

procedure PrepareDestructuringAssignmentTarget(
  const ACtx: TGocciaCompilationContext;
  const APattern: TGocciaDestructuringPattern;
  var ATarget: TPreparedDestructuringTarget);
var
  AssignPat: TGocciaAssignmentDestructuringPattern;
  RestPat: TGocciaRestDestructuringPattern;
  IdentPat: TGocciaIdentifierDestructuringPattern;
  MemberPat: TGocciaMemberExpressionDestructuringPattern;
  PrivatePat: TGocciaPrivateMemberExpressionDestructuringPattern;
  ObjReg, CondReg, ThisReg, BaseReg, SuperReg: UInt16;
  NameIdx: UInt16;
  I, EndCount: Integer;
  MissJump: Integer;
  EndJumps: array of Integer;
begin
  if APattern is TGocciaAssignmentDestructuringPattern then
  begin
    AssignPat := TGocciaAssignmentDestructuringPattern(APattern);
    PrepareDestructuringAssignmentTarget(ACtx, AssignPat.Left, ATarget);
    Exit;
  end;

  if APattern is TGocciaRestDestructuringPattern then
  begin
    RestPat := TGocciaRestDestructuringPattern(APattern);
    PrepareDestructuringAssignmentTarget(ACtx, RestPat.Argument, ATarget);
    Exit;
  end;

  if APattern is TGocciaIdentifierDestructuringPattern then
  begin
    IdentPat := TGocciaIdentifierDestructuringPattern(APattern);
    if ShouldTryWithBinding(ACtx.Scope, IdentPat.Name) then
    begin
      ATarget.Kind := pdtIdentifierWithBinding;
      ATarget.PropertyName := IdentPat.Name;
      ATarget.ObjectReg := ACtx.Scope.AllocateRegister;
      ATarget.KeyReg := ACtx.Scope.AllocateRegister;
      ObjReg := ACtx.Scope.AllocateRegister;
      CondReg := ACtx.Scope.AllocateRegister;
      EndCount := 0;
      SetLength(EndJumps, ACtx.Scope.WithBindingCount);
      try
        NameIdx := ACtx.Template.AddConstantString(IdentPat.Name);
        EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST,
          UInt16(ATarget.KeyReg), NameIdx));
        EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED,
          UInt16(ATarget.ObjectReg), 0, 0));

        for I := ACtx.Scope.WithBindingCount - 1 downto 0 do
        begin
          EmitLoadHiddenWithObject(ACtx, ACtx.Scope.GetWithBindingName(I),
            ObjReg);
          EmitInstruction(ACtx, EncodeABC(OP_HAS_WITH_BINDING, CondReg,
            ObjReg, UInt16(ATarget.KeyReg)));
          MissJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, CondReg);
          EmitInstruction(ACtx, EncodeABC(OP_MOVE, UInt16(ATarget.ObjectReg),
            ObjReg, 0));
          EndJumps[EndCount] := EmitJumpInstruction(ACtx, OP_JUMP, 0);
          Inc(EndCount);
          PatchJumpTarget(ACtx, MissJump);
        end;

        for I := 0 to EndCount - 1 do
          PatchJumpTarget(ACtx, EndJumps[I]);
      finally
        ACtx.Scope.FreeRegister;
        ACtx.Scope.FreeRegister;
      end;
    end;
    Exit;
  end;

  if APattern is TGocciaMemberExpressionDestructuringPattern then
  begin
    MemberPat := TGocciaMemberExpressionDestructuringPattern(APattern);
    if MemberPat.Expression.ObjectExpr is TGocciaSuperExpression then
    begin
      PrepareSuperPropertyBase(ACtx, ThisReg, BaseReg, SuperReg);
      ATarget.ThisReg := ThisReg;
      ATarget.ObjectReg := BaseReg;
      ATarget.SuperReg := SuperReg;
      ATarget.KeyReg := ACtx.Scope.AllocateRegister;
      if MemberPat.Expression.Computed then
      begin
        ATarget.Kind := pdtSuperComputedProperty;
        ACtx.CompileExpression(MemberPat.Expression.PropertyExpression,
          UInt16(ATarget.KeyReg));
        EmitInstruction(ACtx, EncodeABC(OP_SUPER_BASE, SuperReg, SuperReg,
          ThisReg));
      end
      else
      begin
        ATarget.Kind := pdtSuperStringProperty;
        ATarget.PropertyName := MemberPat.Expression.PropertyName;
        EmitInstruction(ACtx, EncodeABC(OP_SUPER_BASE, SuperReg, SuperReg,
          ThisReg));
        NameIdx := ACtx.Template.AddConstantString(ATarget.PropertyName);
        EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST,
          UInt16(ATarget.KeyReg), NameIdx));
      end;
      Exit;
    end;

    ATarget.ObjectReg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(MemberPat.Expression.ObjectExpr,
      UInt16(ATarget.ObjectReg));
    if MemberPat.Expression.Computed then
    begin
      ATarget.Kind := pdtComputedProperty;
      ATarget.KeyReg := ACtx.Scope.AllocateRegister;
      ACtx.CompileExpression(MemberPat.Expression.PropertyExpression,
        UInt16(ATarget.KeyReg));
    end
    else
    begin
      ATarget.Kind := pdtStringProperty;
      ATarget.PropertyName := MemberPat.Expression.PropertyName;
    end;
    Exit;
  end;

  if APattern is TGocciaPrivateMemberExpressionDestructuringPattern then
  begin
    PrivatePat := TGocciaPrivateMemberExpressionDestructuringPattern(APattern);
    ATarget.Kind := pdtPrivateProperty;
    ATarget.ObjectReg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(PrivatePat.Expression.ObjectExpr,
      UInt16(ATarget.ObjectReg));
    ATarget.PropertyName := PrivateKey(ACtx.Scope,
      PrivatePat.Expression.PrivateName);
  end;
end;

procedure EmitPreparedDestructuringTargetStore(
  const ACtx: TGocciaCompilationContext;
  const ATarget: TPreparedDestructuringTarget; const AValueReg: UInt16);
var
  MissJump, EndJump: Integer;
begin
  case ATarget.Kind of
    pdtIdentifierWithBinding:
      begin
        MissJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH,
          UInt16(ATarget.ObjectReg), GOCCIA_NULLISH_MATCH_UNDEFINED);
        EmitInstruction(ACtx, EncodeABC(SetWithBindingOpcode(ACtx),
          UInt16(ATarget.ObjectReg), UInt16(ATarget.KeyReg), AValueReg));
        EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
        PatchJumpTarget(ACtx, MissJump);
        EmitBindingAssignmentFromRegister(ACtx, ATarget.PropertyName,
          AValueReg, True);
        PatchJumpTarget(ACtx, EndJump);
      end;
    pdtStringProperty, pdtPrivateProperty:
      EmitStorePropertyByName(ACtx, UInt16(ATarget.ObjectReg),
        ATarget.PropertyName, AValueReg);
    pdtComputedProperty:
      EmitInstruction(ACtx, EncodeABC(StoreByKeyOpcode(ACtx),
        UInt16(ATarget.ObjectReg), UInt16(ATarget.KeyReg), AValueReg));
    pdtSuperStringProperty, pdtSuperComputedProperty:
      EmitInstruction(ACtx, EncodeABC(OP_SUPER_SET_BASE,
        UInt16(ATarget.ObjectReg), UInt16(ATarget.KeyReg), AValueReg));
  end;
end;

procedure EmitDestructuringWithPreparedTarget(
  const ACtx: TGocciaCompilationContext;
  const APattern: TGocciaDestructuringPattern; const ASrcReg: UInt16;
  const AAssignmentMode: Boolean;
  const ATarget: TPreparedDestructuringTarget);
var
  AssignPat: TGocciaAssignmentDestructuringPattern;
  RestPat: TGocciaRestDestructuringPattern;
  JumpIdx: Integer;
begin
  if APattern is TGocciaAssignmentDestructuringPattern then
  begin
    AssignPat := TGocciaAssignmentDestructuringPattern(APattern);
    EmitUndefinedCheck(ACtx, ASrcReg, JumpIdx);
    CompileExpressionWithInferredName(ACtx, AssignPat.Right, ASrcReg,
      SingleIdentifierPatternName(AssignPat.Left));
    PatchJumpTarget(ACtx, JumpIdx);
    EmitDestructuringWithPreparedTarget(ACtx, AssignPat.Left, ASrcReg,
      AAssignmentMode, ATarget);
    Exit;
  end;

  if APattern is TGocciaRestDestructuringPattern then
  begin
    RestPat := TGocciaRestDestructuringPattern(APattern);
    EmitDestructuringWithPreparedTarget(ACtx, RestPat.Argument, ASrcReg,
      AAssignmentMode, ATarget);
    Exit;
  end;

  if (ATarget.Kind <> pdtNone) and
     ((APattern is TGocciaIdentifierDestructuringPattern) or
      (APattern is TGocciaMemberExpressionDestructuringPattern) or
      (APattern is TGocciaPrivateMemberExpressionDestructuringPattern)) then
  begin
    EmitPreparedDestructuringTargetStore(ACtx, ATarget, ASrcReg);
    Exit;
  end;

  EmitDestructuring(ACtx, APattern, ASrcReg, AAssignmentMode);
end;

procedure EmitStreamingArrayDestructuring(const ACtx: TGocciaCompilationContext;
  const APattern: TGocciaArrayDestructuringPattern; const ASrcReg: UInt16;
  const AAssignmentMode: Boolean);
var
  IterReg, ValueReg, DoneReg, ErrorReg: UInt16;
  I: Integer;
  HandlerJump, AfterHandlerJump, SkipCloseJump, SkipNextJump, AfterNextJump,
  RestLoop, RestDoneJump: Integer;
  RestReg: UInt16;
  PreparedTarget: TPreparedDestructuringTarget;

  procedure EmitProtectedIterNext;
  var
    NextHandlerJump, AfterNextHandlerJump: Integer;
  begin
    NextHandlerJump := EmitJumpInstruction(ACtx, OP_PUSH_FINALLY_HANDLER,
      ErrorReg);
    EmitInstruction(ACtx, EncodeABC(OP_ITER_NEXT, ValueReg, DoneReg,
      IterReg));
    EmitInstruction(ACtx, EncodeABC(OP_POP_HANDLER, 0, 0, 0));
    AfterNextHandlerJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);

    PatchJumpTarget(ACtx, NextHandlerJump);
    EmitInstruction(ACtx, EncodeABC(OP_THROW, ErrorReg, 0, 0));

    PatchJumpTarget(ACtx, AfterNextHandlerJump);
  end;
begin
  IterReg := ACtx.Scope.AllocateRegister;
  ValueReg := ACtx.Scope.AllocateRegister;
  DoneReg := ACtx.Scope.AllocateRegister;
  ErrorReg := ACtx.Scope.AllocateRegister;
  try
    EmitInstruction(ACtx, EncodeABC(OP_GET_ITER, IterReg, ASrcReg, 0));
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_FALSE, DoneReg, 0, 0));

    for I := 0 to APattern.Elements.Count - 1 do
    begin
      if not Assigned(APattern.Elements[I]) then
      begin
        SkipNextJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, DoneReg);
        EmitProtectedIterNext;
        PatchJumpTarget(ACtx, SkipNextJump);
        Continue;
      end;

      InitPreparedDestructuringTarget(PreparedTarget);
      try
        HandlerJump := -1;
        if AAssignmentMode then
        begin
          HandlerJump := EmitJumpInstruction(ACtx, OP_PUSH_FINALLY_HANDLER,
            ErrorReg);
          PrepareDestructuringAssignmentTarget(ACtx, APattern.Elements[I],
            PreparedTarget);
          EmitInstruction(ACtx, EncodeABC(OP_POP_HANDLER, 0, 0, 0));
          AfterHandlerJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);

          PatchJumpTarget(ACtx, HandlerJump);
          SkipCloseJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, DoneReg);
          EmitInstruction(ACtx, EncodeABC(OP_ITER_CLOSE, IterReg, ErrorReg,
            ITER_CLOSE_PRESERVE_UNLESS_GENERATOR_RETURN));
          PatchJumpTarget(ACtx, SkipCloseJump);
          EmitInstruction(ACtx, EncodeABC(OP_THROW, ErrorReg, 0, 0));

          PatchJumpTarget(ACtx, AfterHandlerJump);
        end;

        if APattern.Elements[I] is TGocciaRestDestructuringPattern then
        begin
          RestReg := ACtx.Scope.AllocateRegister;
          try
            EmitInstruction(ACtx, EncodeABC(OP_NEW_ARRAY, RestReg, 0, 0));
            RestLoop := CurrentCodePosition(ACtx);
            RestDoneJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE,
              DoneReg);
            EmitProtectedIterNext;
            SkipNextJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE,
              DoneReg);
            EmitInstruction(ACtx, EncodeABC(OP_ARRAY_PUSH, RestReg, ValueReg,
              0));
            EmitInstruction(ACtx, EncodeAx(OP_JUMP,
              RestLoop - CurrentCodePosition(ACtx) - 1));
            PatchJumpTarget(ACtx, SkipNextJump);
            PatchJumpTarget(ACtx, RestDoneJump);
            EmitDestructuringWithPreparedTarget(ACtx, APattern.Elements[I],
              UInt16(RestReg), AAssignmentMode, PreparedTarget);
          finally
            ACtx.Scope.FreeRegister;
          end;
          Break;
        end;

        SkipNextJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, DoneReg);
        EmitProtectedIterNext;
        AfterNextJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
        PatchJumpTarget(ACtx, SkipNextJump);
        EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, ValueReg, 0, 0));
        PatchJumpTarget(ACtx, AfterNextJump);

        HandlerJump := EmitJumpInstruction(ACtx, OP_PUSH_FINALLY_HANDLER,
          ErrorReg);
        EmitDestructuringWithPreparedTarget(ACtx, APattern.Elements[I],
          ValueReg, AAssignmentMode, PreparedTarget);
        EmitInstruction(ACtx, EncodeABC(OP_POP_HANDLER, 0, 0, 0));
        AfterHandlerJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);

        PatchJumpTarget(ACtx, HandlerJump);
        SkipCloseJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, DoneReg);
        EmitInstruction(ACtx, EncodeABC(OP_ITER_CLOSE, IterReg, ErrorReg,
          ITER_CLOSE_PRESERVE_UNLESS_GENERATOR_RETURN));
        PatchJumpTarget(ACtx, SkipCloseJump);
        EmitInstruction(ACtx, EncodeABC(OP_THROW, ErrorReg, 0, 0));

        PatchJumpTarget(ACtx, AfterHandlerJump);
      finally
        ReleasePreparedDestructuringTarget(ACtx, PreparedTarget);
      end;
    end;

    SkipCloseJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, DoneReg);
    EmitInstruction(ACtx, EncodeABC(OP_ITER_CLOSE, IterReg, 0, 0));
    PatchJumpTarget(ACtx, SkipCloseJump);
  finally
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
  end;
end;

procedure EmitDestructuring(const ACtx: TGocciaCompilationContext;
  const APattern: TGocciaDestructuringPattern; const ASrcReg: UInt16;
  const AAssignmentMode: Boolean);
var
  ObjPat: TGocciaObjectDestructuringPattern;
  ArrPat: TGocciaArrayDestructuringPattern;
  AssignPat: TGocciaAssignmentDestructuringPattern;
  IdentPat: TGocciaIdentifierDestructuringPattern;
  Prop: TGocciaDestructuringProperty;
  DestSlot, IdxReg, ThisReg, BaseReg, SuperReg: UInt16;
  PropIdx: UInt16;
  JumpIdx, I: Integer;
  HasRest: Boolean;
  Limit: Integer;
  RestIndex: Integer;
  ComputedKeySlot: Integer;
  ComputedKeySlots: array of Integer;
  PreparedTarget: TPreparedDestructuringTarget;
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
    SetLength(ComputedKeySlots, ObjPat.Properties.Count);
    for I := 0 to High(ComputedKeySlots) do
      ComputedKeySlots[I] := -1;
    RestIndex := -1;
    for I := 0 to ObjPat.Properties.Count - 1 do
      if ObjPat.Properties[I].Pattern is TGocciaRestDestructuringPattern then
      begin
        RestIndex := I;
        Break;
      end;

    for I := 0 to ObjPat.Properties.Count - 1 do
    begin
      Prop := ObjPat.Properties[I];

      if Prop.Pattern is TGocciaRestDestructuringPattern then
      begin
        IdxReg := ACtx.Scope.AllocateRegister;
        EmitInstruction(ACtx, EncodeABC(OP_NEW_ARRAY, IdxReg, UInt16(I), 0));
        for JumpIdx := 0 to I - 1 do
        begin
          if ObjPat.Properties[JumpIdx].Computed and
             Assigned(ObjPat.Properties[JumpIdx].KeyExpression) then
          begin
            if ComputedKeySlots[JumpIdx] < 0 then
              raise Exception.Create('Compiler error: computed object rest key was not preserved');
            EmitInstruction(ACtx, EncodeABC(OP_ARRAY_PUSH, IdxReg,
              UInt16(ComputedKeySlots[JumpIdx]), 0));
          end
          else
          begin
            DestSlot := ACtx.Scope.AllocateRegister;
            PropIdx := ACtx.Template.AddConstantString(ObjPat.Properties[JumpIdx].Key);
            EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, DestSlot, PropIdx));
            EmitInstruction(ACtx, EncodeABC(OP_ARRAY_PUSH, IdxReg, DestSlot, 0));
            ACtx.Scope.FreeRegister;
          end;
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

      InitPreparedDestructuringTarget(PreparedTarget);
      IdxReg := 0;
      if Prop.Computed and Assigned(Prop.KeyExpression) then
      begin
        if (RestIndex >= 0) and (I < RestIndex) then
        begin
          ComputedKeySlot := ACtx.Scope.DeclareLocal(
            '#object-rest-key:' + IntToStr(ACtx.Template.CodeCount) + ':' +
            IntToStr(ACtx.Scope.NextSlot) + ':' + IntToStr(I), False);
          ACtx.CompileExpression(Prop.KeyExpression, UInt16(ComputedKeySlot));
          EmitInstruction(ACtx, EncodeABC(OP_TO_PROPERTY_KEY,
            UInt16(ComputedKeySlot), UInt16(ComputedKeySlot), 0));
          ComputedKeySlots[I] := ComputedKeySlot;
          IdxReg := UInt16(ComputedKeySlot);
        end
        else
        begin
          IdxReg := ACtx.Scope.AllocateRegister;
          ACtx.CompileExpression(Prop.KeyExpression, IdxReg);
          EmitInstruction(ACtx, EncodeABC(OP_TO_PROPERTY_KEY, IdxReg, IdxReg,
            0));
        end;
      end;

      try
        if AAssignmentMode then
          PrepareDestructuringAssignmentTarget(ACtx, Prop.Pattern,
            PreparedTarget)
        else
          EmitDestructuringBindingResolutionProbe(ACtx, Prop.Pattern);

        DestSlot := ACtx.Scope.AllocateRegister;
        if Prop.Computed and Assigned(Prop.KeyExpression) then
          EmitInstruction(ACtx, EncodeABC(OP_GET_INDEX, DestSlot, ASrcReg,
            IdxReg))
        else
          EmitLoadPropertyByName(ACtx, DestSlot, ASrcReg, Prop.Key);

        EmitDestructuringWithPreparedTarget(ACtx, Prop.Pattern, DestSlot,
          AAssignmentMode, PreparedTarget);
        ACtx.Scope.FreeRegister;
      finally
        ReleasePreparedDestructuringTarget(ACtx, PreparedTarget);
        if Prop.Computed and Assigned(Prop.KeyExpression) and
           not ((RestIndex >= 0) and (I < RestIndex)) then
          ACtx.Scope.FreeRegister;
      end;
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
    if AAssignmentMode or
       ((not HasRest) and DestructuringPatternHasSuspendingDefault(ArrPat)) then
    begin
      EmitStreamingArrayDestructuring(ACtx, ArrPat, ASrcReg, AAssignmentMode);
      Exit;
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
    CompileExpressionWithInferredName(ACtx, AssignPat.Right, ASrcReg,
      SingleIdentifierPatternName(AssignPat.Left));
    PatchJumpTarget(ACtx, JumpIdx);
    EmitDestructuring(ACtx, AssignPat.Left, ASrcReg, AAssignmentMode);
  end
  else if APattern is TGocciaMemberExpressionDestructuringPattern then
  begin
    if TGocciaMemberExpressionDestructuringPattern(APattern)
       .Expression.ObjectExpr is TGocciaSuperExpression then
    begin
      PrepareSuperPropertyBase(ACtx, ThisReg, BaseReg, SuperReg);
      IdxReg := ACtx.Scope.AllocateRegister;
      if TGocciaMemberExpressionDestructuringPattern(APattern).Expression.Computed then
      begin
        ACtx.CompileExpression(
          TGocciaMemberExpressionDestructuringPattern(APattern)
            .Expression.PropertyExpression,
          IdxReg);
        EmitInstruction(ACtx, EncodeABC(OP_TO_PROPERTY_KEY, IdxReg,
          IdxReg, 0));
      end
      else
      begin
        PropIdx := ACtx.Template.AddConstantString(
          TGocciaMemberExpressionDestructuringPattern(APattern)
            .Expression.PropertyName);
        EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, IdxReg, PropIdx));
      end;
      EmitInstruction(ACtx, EncodeABC(OP_SUPER_SET, BaseReg, IdxReg, ASrcReg));
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
      Exit;
    end;

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
  end
  else if APattern is TGocciaPrivateMemberExpressionDestructuringPattern then
  begin
    DestSlot := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(
      TGocciaPrivateMemberExpressionDestructuringPattern(APattern).Expression.ObjectExpr,
      DestSlot);
    EmitStorePropertyByName(ACtx, DestSlot,
      PrivateKey(ACtx.Scope,
        TGocciaPrivateMemberExpressionDestructuringPattern(APattern).Expression.PrivateName),
      ASrcReg);
    ACtx.Scope.FreeRegister;
  end;
end;

procedure CompileDestructuringAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaDestructuringAssignmentExpression; const ADest: UInt16);
var
  SrcReg: UInt16;
begin
  SrcReg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AExpr.Right, SrcReg);
  if ADest <> SrcReg then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, SrcReg, 0));
  EmitDestructuring(ACtx, AExpr.Left, SrcReg, True);
  ACtx.Scope.FreeRegister;
end;

procedure EmitDestructuringParameters(const ACtx: TGocciaCompilationContext;
  const AParams: TGocciaParameterArray);
var
  I, LocalIdx: Integer;
  ParamSlot: UInt16;
begin
  for I := 0 to High(AParams) do
  begin
    if not AParams[I].IsPattern then
      Continue;
    if not Assigned(AParams[I].Pattern) then
      Continue;

    LocalIdx := ACtx.Scope.ResolveLocal(SyntheticParamLocalName(I));
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
      ParamName := SyntheticParamLocalName(I)
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
      ParamName := SyntheticParamLocalName(I)
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
    ACtx.Template.TypeCheckPreambleSize := UInt16(CheckCount);
end;

procedure CompileArrowFunction(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaArrowFunctionExpression; const ADest: UInt16);
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
  OldDerivedGuard: Boolean;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;
  OldDerivedGuard := ACtx.DerivedConstructorThisGuard;

  ChildTemplate := TGocciaFunctionTemplate.Create('<arrow>');
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.IsAsync := AExpr.IsAsync;
  ChildTemplate.IsArrow := True;
  ChildTemplate.StrictCode := ChildBodyIsStrictCode(ACtx, AExpr.Body);
  ChildTemplate.RejectArgumentsInDirectEval :=
    OldTemplate.RejectArgumentsInDirectEval;
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
      ChildScope.DeclareLocal(SyntheticParamLocalName(I), False)
    else
      ChildScope.DeclareLocal(AExpr.Parameters[I].Name, False);
  end;
  for I := 0 to High(AExpr.Parameters) do
    if AExpr.Parameters[I].IsPattern and Assigned(AExpr.Parameters[I].Pattern) then
      CollectDestructuringBindings(AExpr.Parameters[I].Pattern, ChildScope);

  if ParameterDefaultsContainDirectEval(AExpr.Parameters) and
     not ParameterListBindsName(AExpr.Parameters, IDENTIFIER_ARGUMENTS) then
  begin
    ChildTemplate.DirectEvalSyntheticArgumentsSlot :=
      ChildScope.DeclareLocal(IDENTIFIER_ARGUMENTS, False);
    ChildScope.DirectEvalSyntheticArgumentsSlot :=
      ChildTemplate.DirectEvalSyntheticArgumentsSlot;
  end;

  ApplyParameterTypeAnnotations(ChildScope, ChildTemplate, AExpr.Parameters,
    ACtx.StrictTypes);

  if FormalCount < 0 then
    FormalCount := Length(AExpr.Parameters);
  ChildTemplate.FormalParameterCount := UInt16(FormalCount);
  if Assigned(ACtx.FormalParameterCounts) then
    ACtx.FormalParameterCounts.AddOrSetValue(ChildTemplate, FormalCount);

  ACtx.SwapState(ChildTemplate, ChildScope);
  if Assigned(ACtx.SetDerivedConstructorThisGuard) then
    ACtx.SetDerivedConstructorThisGuard(OldDerivedGuard);
  try
    ChildCtx := ACtx;
    ChildCtx.Template := ChildTemplate;
    ChildCtx.Scope := ChildScope;
    ChildCtx.NonStrictMode := ACtx.NonStrictMode and
      not ChildTemplate.StrictCode;
    ChildCtx.DerivedConstructorThisGuard := OldDerivedGuard;

    if (RestParamIndex >= 0) and
       not ParameterListHasDefaultValues(AExpr.Parameters) then
    begin
      if AExpr.Parameters[RestParamIndex].IsPattern then
        RestReg := ChildScope.ResolveLocal(SyntheticParamLocalName(RestParamIndex))
      else
        RestReg := ChildScope.ResolveLocal(AExpr.Parameters[RestParamIndex].Name);
      EmitInstruction(ChildCtx,
        EncodeABC(OP_PACK_ARGS, UInt16(RestReg), UInt16(RestParamIndex), 0));
    end;

    EmitDefaultParameters(ChildCtx, AExpr.Parameters);
    EmitDestructuringParameters(ChildCtx, AExpr.Parameters);
    EmitParameterTypeChecks(ChildCtx, AExpr.Parameters);
    if ChildTemplate.CodeCount > High(UInt16) then
      raise Exception.Create('Parameter preamble is too large to encode');
    ChildTemplate.ParameterPreambleSize := UInt16(ChildTemplate.CodeCount);

    ACtx.CompileFunctionBody(AExpr.Body);

    ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

    for I := 0 to ChildScope.UpvalueCount - 1 do
      ChildTemplate.AddUpvalueDescriptor(
        ChildScope.GetUpvalue(I).IsLocal,
        ChildScope.GetUpvalue(I).Index,
        ChildScope.GetUpvalue(I).Name);
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
  const ASlot: UInt16): Boolean;
var
  I: Integer;
begin
  for I := 0 to AScope.LocalCount - 1 do
    if AScope.GetLocal(I).Slot = ASlot then
      Exit(True);
  Result := False;
end;

procedure CompileSpreadArgsArrayFromList(const ACtx: TGocciaCompilationContext;
  const AArgs: TObjectList<TGocciaExpression>; const AArrayReg: UInt16);
var
  I: Integer;
  ElemReg: UInt16;
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
  const AExpr: TGocciaCallExpression; const AArrayReg: UInt16);
begin
  CompileSpreadArgsArrayFromList(ACtx, AExpr.Arguments, AArrayReg);
end;

function TryCompileWithIdentifierCall(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaCallExpression; const ADest: UInt16;
  const AArgCount: Integer; const AUseSpread: Boolean;
  var AJumps: TGocciaCompilerJumpArray; var AJumpCount: Integer;
  const ATail: Boolean = False): Boolean;
var
  Ident: TGocciaIdentifierExpression;
  ObjReg, BaseReg, ArgsReg, KeyReg, CondReg: UInt16;
  NameIdx: UInt16;
  I, EndCount: Integer;
  MissJump: Integer;
  EndJumps: array of Integer;
  TailFlag: UInt16;

  procedure EmitBranchCall(const AIsMethodCall, AJumpAfter: Boolean);
  var
    ArgIndex: Integer;
  begin
    // An optional call is not in tail position (the nullish guard runs after).
    if ATail and not AExpr.Optional then
      TailFlag := CALL_FLAG_TAIL
    else
      TailFlag := 0;
    if AExpr.Optional then
      AddOptionalChainJump(ACtx, AJumps, AJumpCount, BaseReg);

    if AUseSpread then
    begin
      ArgsReg := ACtx.Scope.AllocateRegister;
      CompileSpreadArgsArray(ACtx, AExpr, ArgsReg);
      if AIsMethodCall then
        EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, BaseReg, ArgsReg,
          CALL_FLAG_SPREAD or TailFlag))
      else
        EmitInstruction(ACtx, EncodeABC(OP_CALL, BaseReg, ArgsReg,
          SpreadCallFlags(ACtx, AExpr, CurrentCodePosition(ACtx)) or TailFlag));
      ACtx.Scope.FreeRegister;
    end
    else
    begin
      for ArgIndex := 0 to AArgCount - 1 do
        ACtx.CompileExpression(AExpr.Arguments[ArgIndex],
          ACtx.Scope.AllocateRegister);
      if AIsMethodCall then
        EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, BaseReg,
          UInt16(AArgCount), TailFlag))
      else
        EmitInstruction(ACtx, EncodeABC(OP_CALL, BaseReg, UInt16(AArgCount),
        CallFlags(ACtx, AExpr, CurrentCodePosition(ACtx)) or TailFlag));
      for ArgIndex := 0 to AArgCount - 1 do
        ACtx.Scope.FreeRegister;
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
      EmitInstruction(ACtx, EncodeABC(GetWithBindingOpcode(ACtx), BaseReg,
        ObjReg, KeyReg));
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

function CompileCallWithOptionalChainJumps(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaCallExpression; const ADest: UInt16;
  var AJumps: TGocciaCompilerJumpArray; var AJumpCount: Integer): Boolean;
var
  ArgCount: Integer;
  ObjReg, BaseReg, ArgsReg, KeyReg: UInt16;
  MemberExpr: TGocciaMemberExpression;
  PrivateExpr: TGocciaPrivateMemberExpression;
  Ident: TGocciaIdentifierExpression;
  SequenceExpr: TGocciaSequenceExpression;
  PropIdx: UInt16;
  UseSpread: Boolean;
  EndJump, JumpIndex: Integer;
  LocalJumps: TGocciaCompilerJumpArray;
  LocalJumpCount: Integer;

  procedure EmitArgumentsAndCall(const AIsMethodCall: Boolean);
  var
    ArgIndex: Integer;
  begin
    if AExpr.Optional then
      AddOptionalChainJump(ACtx, AJumps, AJumpCount, BaseReg);

    if UseSpread then
    begin
      ArgsReg := ACtx.Scope.AllocateRegister;
      CompileSpreadArgsArray(ACtx, AExpr, ArgsReg);
      if AIsMethodCall then
        EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, BaseReg, ArgsReg, 1))
      else
        EmitInstruction(ACtx, EncodeABC(OP_CALL, BaseReg, ArgsReg,
          SpreadCallFlags(ACtx, AExpr, CurrentCodePosition(ACtx))));
      ACtx.Scope.FreeRegister;
    end
    else
    begin
      for ArgIndex := 0 to ArgCount - 1 do
        ACtx.CompileExpression(AExpr.Arguments[ArgIndex],
          ACtx.Scope.AllocateRegister);
      if AIsMethodCall then
        EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, BaseReg,
          UInt16(ArgCount), 0))
      else
        EmitInstruction(ACtx, EncodeABC(OP_CALL, BaseReg, UInt16(ArgCount),
          CallFlags(ACtx, AExpr, CurrentCodePosition(ACtx))));
      for ArgIndex := 0 to ArgCount - 1 do
        ACtx.Scope.FreeRegister;
    end;
  end;

begin
  if not (AExpr.Optional or ExpressionContainsOptionalChain(AExpr.Callee)) then
    Exit(False);

  ArgCount := AExpr.Arguments.Count;
  if ArgCount > High(UInt16) then
    raise Exception.Create('Compiler error: too many arguments (>65535)');
  UseSpread := HasSpreadArgument(AExpr);

  if AExpr.Callee is TGocciaIdentifierExpression then
  begin
    Ident := TGocciaIdentifierExpression(AExpr.Callee);
    if ShouldTryWithBinding(ACtx.Scope, Ident.Name) then
      Exit(TryCompileWithIdentifierCall(ACtx, AExpr, ADest, ArgCount,
        UseSpread, AJumps, AJumpCount));
  end;

  if AExpr.Callee is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(AExpr.Callee);
    if MemberExpr.ObjectExpr is TGocciaSuperExpression then
      Exit(False);

    ObjReg := ACtx.Scope.AllocateRegister;
    BaseReg := ACtx.Scope.AllocateRegister;
    try
      CompileExpressionWithOptionalChainJumps(ACtx, MemberExpr.ObjectExpr,
        ObjReg, AJumps, AJumpCount);
      if MemberExpr.Optional then
        AddOptionalChainJump(ACtx, AJumps, AJumpCount, ObjReg);

      if MemberExpr.Computed then
      begin
        KeyReg := ACtx.Scope.AllocateRegister;
        try
          ACtx.CompileExpression(MemberExpr.PropertyExpression, KeyReg);
          EmitInstruction(ACtx, EncodeABC(OP_ARRAY_GET, BaseReg, ObjReg,
            KeyReg));
        finally
          ACtx.Scope.FreeRegister;
        end;
      end
      else
        EmitLoadPropertyByName(ACtx, BaseReg, ObjReg, MemberExpr.PropertyName);

      EmitArgumentsAndCall(True);

      if ADest <> BaseReg then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, BaseReg, 0));
    finally
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
    end;
    Exit(True);
  end;

  if AExpr.Callee is TGocciaPrivateMemberExpression then
  begin
    PrivateExpr := TGocciaPrivateMemberExpression(AExpr.Callee);
    ObjReg := ACtx.Scope.AllocateRegister;
    BaseReg := ACtx.Scope.AllocateRegister;
    try
      CompileExpressionWithOptionalChainJumps(ACtx, PrivateExpr.ObjectExpr,
        ObjReg, AJumps, AJumpCount);
      if PrivateExpr.Optional then
        AddOptionalChainJump(ACtx, AJumps, AJumpCount, ObjReg);
      PropIdx := ACtx.Template.AddConstantString(
        PrivateKey(ACtx.Scope, PrivateExpr.PrivateName));
      if PropIdx <= High(UInt8) then
        EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, BaseReg, ObjReg,
          UInt16(PropIdx)))
      else
      begin
        KeyReg := ACtx.Scope.AllocateRegister;
        try
          EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, KeyReg, PropIdx));
          EmitInstruction(ACtx, EncodeABC(OP_GET_INDEX, BaseReg, ObjReg,
            KeyReg));
        finally
          ACtx.Scope.FreeRegister;
        end;
      end;

      EmitArgumentsAndCall(True);

      if ADest <> BaseReg then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, BaseReg, 0));
    finally
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
    end;
    Exit(True);
  end;

  if (AExpr.Callee is TGocciaSequenceExpression) then
  begin
    SequenceExpr := TGocciaSequenceExpression(AExpr.Callee);
    if (SequenceExpr.Expressions.Count = 1) and
       (SequenceExpr.Expressions[0] is TGocciaMemberExpression) then
    begin
      MemberExpr := TGocciaMemberExpression(SequenceExpr.Expressions[0]);
      if MemberExpr.ObjectExpr is TGocciaSuperExpression then
        Exit(False);

      ObjReg := ACtx.Scope.AllocateRegister;
      BaseReg := ACtx.Scope.AllocateRegister;
      LocalJumpCount := 0;
      try
        CompileExpressionWithOptionalChainJumps(ACtx, MemberExpr.ObjectExpr,
          ObjReg, LocalJumps, LocalJumpCount);
        if MemberExpr.Optional then
          AddOptionalChainJump(ACtx, LocalJumps, LocalJumpCount, ObjReg);

        if MemberExpr.Computed then
        begin
          KeyReg := ACtx.Scope.AllocateRegister;
          try
            ACtx.CompileExpression(MemberExpr.PropertyExpression, KeyReg);
            EmitInstruction(ACtx, EncodeABC(OP_ARRAY_GET, BaseReg, ObjReg,
              KeyReg));
          finally
            ACtx.Scope.FreeRegister;
          end;
        end
        else
          EmitLoadPropertyByName(ACtx, BaseReg, ObjReg,
            MemberExpr.PropertyName);

        if LocalJumpCount > 0 then
        begin
          EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
          for JumpIndex := 0 to LocalJumpCount - 1 do
            PatchJumpTarget(ACtx, LocalJumps[JumpIndex]);
          EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, BaseReg, 0, 0));
          PatchJumpTarget(ACtx, EndJump);
        end;

        EmitArgumentsAndCall(True);

        if ADest <> BaseReg then
          EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, BaseReg, 0));
      finally
        ACtx.Scope.FreeRegister;
        ACtx.Scope.FreeRegister;
      end;
      Exit(True);
    end;

    if (SequenceExpr.Expressions.Count = 1) and
       (SequenceExpr.Expressions[0] is TGocciaPrivateMemberExpression) then
    begin
      PrivateExpr := TGocciaPrivateMemberExpression(
        SequenceExpr.Expressions[0]);
      ObjReg := ACtx.Scope.AllocateRegister;
      BaseReg := ACtx.Scope.AllocateRegister;
      LocalJumpCount := 0;
      try
        CompileExpressionWithOptionalChainJumps(ACtx, PrivateExpr.ObjectExpr,
          ObjReg, LocalJumps, LocalJumpCount);
        if PrivateExpr.Optional then
          AddOptionalChainJump(ACtx, LocalJumps, LocalJumpCount, ObjReg);
        PropIdx := ACtx.Template.AddConstantString(
          PrivateKey(ACtx.Scope, PrivateExpr.PrivateName));
        if PropIdx <= High(UInt8) then
          EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, BaseReg, ObjReg,
            UInt16(PropIdx)))
        else
        begin
          KeyReg := ACtx.Scope.AllocateRegister;
          try
            EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, KeyReg, PropIdx));
            EmitInstruction(ACtx, EncodeABC(OP_GET_INDEX, BaseReg, ObjReg,
              KeyReg));
          finally
            ACtx.Scope.FreeRegister;
          end;
        end;

        if LocalJumpCount > 0 then
        begin
          EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
          for JumpIndex := 0 to LocalJumpCount - 1 do
            PatchJumpTarget(ACtx, LocalJumps[JumpIndex]);
          EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, BaseReg, 0, 0));
          PatchJumpTarget(ACtx, EndJump);
        end;

        EmitArgumentsAndCall(True);

        if ADest <> BaseReg then
          EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, BaseReg, 0));
      finally
        ACtx.Scope.FreeRegister;
        ACtx.Scope.FreeRegister;
      end;
      Exit(True);
    end;
  end;

  if (ADest + 1 = ACtx.Scope.NextSlot) and
     not IsLocalSlot(ACtx.Scope, ADest) then
    BaseReg := ADest
  else
    BaseReg := ACtx.Scope.AllocateRegister;
  try
    CompileExpressionWithOptionalChainJumps(ACtx, AExpr.Callee, BaseReg,
      AJumps, AJumpCount);
    EmitArgumentsAndCall(False);

    if BaseReg <> ADest then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, BaseReg, 0));
  finally
    if BaseReg <> ADest then
      ACtx.Scope.FreeRegister;
  end;

  Result := True;
end;

function TryCompileOptionalChainCall(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaCallExpression; const ADest: UInt16): Boolean;
var
  EndJump, JumpIndex: Integer;
  NullishJumps: TGocciaCompilerJumpArray;
  NullishJumpCount: Integer;
begin
  NullishJumpCount := 0;
  if not CompileCallWithOptionalChainJumps(ACtx, AExpr, ADest, NullishJumps,
     NullishJumpCount) then
    Exit(False);

  if NullishJumpCount > 0 then
  begin
    EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
    for JumpIndex := 0 to NullishJumpCount - 1 do
      PatchJumpTarget(ACtx, NullishJumps[JumpIndex]);
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, ADest, 0, 0));
    PatchJumpTarget(ACtx, EndJump);
  end;

  Result := True;
end;

procedure CompileCall(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaCallExpression; const ADest: UInt16;
  const ATail: Boolean = False);
var
  ArgCount, I: Integer;
  BaseReg, ObjReg, ArgsReg, SuperReg, KeyReg: UInt16;
  ThisLocalIdx: Integer;
  ThisUpvalIdx: Integer;
  ThisLocal: TGocciaCompilerLocal;
  ThisSlot: UInt16;
  MemberExpr: TGocciaMemberExpression;
  PropIdx: UInt16;
  UseSpread: Boolean;
  NilJump, CallNilJump, EndJump: Integer;
  IgnoredJumps: TGocciaCompilerJumpArray;
  IgnoredJumpCount: Integer;
  MethodTailFlag: UInt16;
begin
  if TryCompileOptionalChainCall(ACtx, AExpr, ADest) then
    Exit;

  ArgCount := AExpr.Arguments.Count;
  if ArgCount > High(UInt16) then
    raise Exception.Create('Compiler error: too many arguments (>65535)');
  UseSpread := HasSpreadArgument(AExpr);
  CallNilJump := -1;
  IgnoredJumpCount := 0;
  // ES2026 §15.10.2: an optional call cannot be in tail position here because
  // the nullish guard runs after the call, and super() writes `this` back after
  // the call, so neither is the last operation.  Only mark the plain and direct
  // member call forms.
  if ATail and not AExpr.Optional and
     not (AExpr.Callee is TGocciaSuperExpression) then
    MethodTailFlag := CALL_FLAG_TAIL
  else
    MethodTailFlag := 0;

  if AExpr.Callee is TGocciaSuperExpression then
  begin
    ObjReg := ACtx.Scope.AllocateRegister;
    ThisLocalIdx := ACtx.Scope.ResolveLocal(KEYWORD_THIS);
    if ThisLocalIdx >= 0 then
    begin
      ThisLocal := ACtx.Scope.GetLocal(ThisLocalIdx);
      if ThisLocal.Slot <> ObjReg then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, ObjReg, ThisLocal.Slot, 0));
    end
    else
    begin
      ThisUpvalIdx := ACtx.Scope.ResolveUpvalue(KEYWORD_THIS);
      if ThisUpvalIdx >= 0 then
        EmitInstruction(ACtx, EncodeABx(OP_GET_UPVALUE, ObjReg,
          UInt16(ThisUpvalIdx)))
      else
        CompileThis(ACtx, ObjReg);
    end;
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
      EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, BaseReg, UInt16(ArgCount), 0));
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
    EmitSetDerivedThisInitialized(ACtx);
  end
  else
  begin
      ThisUpvalIdx := ACtx.Scope.ResolveUpvalue(KEYWORD_THIS);
      if ThisUpvalIdx >= 0 then
      begin
        EmitInstruction(ACtx, EncodeABx(OP_SET_UPVALUE, BaseReg,
          UInt16(ThisUpvalIdx)));
        EmitSetDerivedThisInitialized(ACtx);
      end
      else
      begin
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, 0, BaseReg, 0));
        EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, 0, 0));
        EmitSetDerivedThisInitialized(ACtx);
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
        UInt16(PropIdx)))
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
      EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, BaseReg, ArgsReg,
        CALL_FLAG_SPREAD or MethodTailFlag));
      ACtx.Scope.FreeRegister;
    end
    else
    begin
      for I := 0 to ArgCount - 1 do
        ACtx.CompileExpression(AExpr.Arguments[I], ACtx.Scope.AllocateRegister);
      EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, BaseReg, UInt16(ArgCount),
        MethodTailFlag));
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
            UInt16(PropIdx)))
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
        EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, BaseReg, UInt16(ArgCount), 0));
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
        EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, BaseReg, ArgsReg,
          CALL_FLAG_SPREAD or MethodTailFlag));
        ACtx.Scope.FreeRegister;
      end
      else
      begin
        for I := 0 to ArgCount - 1 do
          ACtx.CompileExpression(AExpr.Arguments[I], ACtx.Scope.AllocateRegister);
        EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, BaseReg, UInt16(ArgCount),
          MethodTailFlag));
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
       UseSpread, IgnoredJumps, IgnoredJumpCount, MethodTailFlag <> 0) then
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
      EmitInstruction(ACtx, EncodeABC(OP_CALL, BaseReg, ArgsReg,
        SpreadCallFlags(ACtx, AExpr, CurrentCodePosition(ACtx)) or MethodTailFlag));
      ACtx.Scope.FreeRegister;
    end
    else
    begin
      for I := 0 to ArgCount - 1 do
        ACtx.CompileExpression(AExpr.Arguments[I], ACtx.Scope.AllocateRegister);
      EmitInstruction(ACtx, EncodeABC(OP_CALL, BaseReg, UInt16(ArgCount),
        CallFlags(ACtx, AExpr, CurrentCodePosition(ACtx)) or MethodTailFlag));
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
  const AExpr: TGocciaMemberExpression; const ADest: UInt16);
var
  ObjReg, IdxReg, BaseReg, SuperReg, KeyReg: UInt16;
  PropIdx: UInt16;
  EndJump, JumpIndex: Integer;
  NullishJumps: TGocciaCompilerJumpArray;
  NullishJumpCount: Integer;
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
          UInt16(PropIdx)))
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
  NullishJumpCount := 0;
  CompileExpressionWithOptionalChainJumps(ACtx, AExpr.ObjectExpr, ObjReg,
    NullishJumps, NullishJumpCount);

  if AExpr.Optional then
    AddOptionalChainJump(ACtx, NullishJumps, NullishJumpCount, ObjReg);

  if AExpr.Computed then
  begin
    IdxReg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(AExpr.PropertyExpression, IdxReg);
    EmitInstruction(ACtx, EncodeABC(OP_ARRAY_GET, ADest, ObjReg, IdxReg));
    ACtx.Scope.FreeRegister;
  end
  else
    EmitLoadPropertyByName(ACtx, ADest, ObjReg, AExpr.PropertyName);

  if NullishJumpCount > 0 then
  begin
    EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
    for JumpIndex := 0 to NullishJumpCount - 1 do
      PatchJumpTarget(ACtx, NullishJumps[JumpIndex]);
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, ADest, 0, 0));
    PatchJumpTarget(ACtx, EndJump);
  end;

  ACtx.Scope.FreeRegister;
end;

procedure CompileConditional(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaConditionalExpression; const ADest: UInt16);
var
  ElseJump, EndJump: Integer;
  ConditionReg: UInt16;
begin
  ConditionReg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AExpr.Condition, ConditionReg);
  ElseJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, ConditionReg);
  ACtx.Scope.FreeRegister;
  ACtx.CompileExpression(AExpr.Consequent, ADest);
  EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
  PatchJumpTarget(ACtx, ElseJump);
  ACtx.CompileExpression(AExpr.Alternate, ADest);
  PatchJumpTarget(ACtx, EndJump);
end;

procedure CompileArray(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaArrayExpression; const ADest: UInt16);
var
  I: Integer;
  ElemReg: UInt16;
begin
  EmitInstruction(ACtx, EncodeABC(OP_NEW_ARRAY, ADest,
    UInt16(Min(AExpr.Elements.Count, High(UInt16))), 0));

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
  const ADest: UInt16; const AKey: string; const AValExpr: TGocciaExpression;
  const AUsesColonSyntax: Boolean = True);
var
  ValReg: UInt16;
  DefineOp: TGocciaOpCode;
  FuncCount: Integer;
  InferredTemplate: TGocciaFunctionTemplate;
  ValueExpr: TGocciaExpression;
  IsProtoSetter: Boolean;
begin
  ValReg := ACtx.Scope.AllocateRegister;
  FuncCount := ACtx.Template.FunctionCount;
  ValueExpr := AValExpr;
  DefineOp := OP_DEFINE_DATA_PROP;
  IsProtoSetter := False;

  if AValExpr is TGocciaObjectMethodDefinition then
  begin
    ValueExpr := TGocciaObjectMethodDefinition(AValExpr).FunctionExpression;
    DefineOp := OP_DEFINE_METHOD_PROP;
    CompileFunctionExpression(ACtx,
      TGocciaObjectMethodDefinition(AValExpr).FunctionExpression,
      ValReg, '<method>');
  end
  else
  begin
    IsProtoSetter := AUsesColonSyntax and (AKey = PROP_PROTO);
    if (not IsProtoSetter) and
       (ValueExpr is TGocciaClassExpression) and
       (TGocciaClassExpression(ValueExpr).ClassDefinition.Name = '') then
      Goccia.Compiler.Statements.CompileClassExpression(ACtx,
        TGocciaClassExpression(ValueExpr).ClassDefinition, ValReg, AKey)
    else
      ACtx.CompileExpression(ValueExpr, ValReg);
  end;

  if (not IsProtoSetter) and
     ((ValueExpr is TGocciaArrowFunctionExpression) or
      (ValueExpr is TGocciaFunctionExpression)) then
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

  if IsProtoSetter then
    EmitInstruction(ACtx, EncodeABC(OP_SET_OBJECT_PROTO, ADest, ValReg, 0))
  else
    EmitDefineDataPropertyByName(ACtx, ADest, AKey, ValReg, DefineOp);
  ACtx.Scope.FreeRegister;
end;

procedure CompileGetterProperty(const ACtx: TGocciaCompilationContext;
  const ADest: UInt16; const AKey: string;
  const AGetter: TGocciaGetterExpression);
var
  OldTemplate: TGocciaFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TGocciaFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  ChildCtx: TGocciaCompilationContext;
  FuncIdx: UInt16;
  TargetReg, AccessorReg: UInt16;
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
  ArgumentsSlot := DeclareArgumentsObjectLocal(ACtx, ChildScope, EmptyParams,
    ChildTemplate.SourceText);

  ACtx.SwapState(ChildTemplate, ChildScope);
  try
    ChildCtx := ACtx;
    ChildCtx.Template := ChildTemplate;
    ChildCtx.Scope := ChildScope;
    ChildCtx.NonStrictMode := ACtx.NonStrictMode and
      not ChildTemplate.StrictCode;
    ChildCtx.DerivedConstructorThisGuard := False;
    EmitLineMapping(ChildCtx, AGetter.Line, AGetter.Column);
    EmitCreateArgumentsObject(ChildCtx, ArgumentsSlot,
      ChildCtx.NonStrictMode and ParameterListIsSimple(EmptyParams),
      Length(EmptyParams));
    ACtx.CompileFunctionBody(AGetter.Body);
    ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

    for I := 0 to ChildScope.UpvalueCount - 1 do
      ChildTemplate.AddUpvalueDescriptor(
        ChildScope.GetUpvalue(I).IsLocal,
        ChildScope.GetUpvalue(I).Index,
        ChildScope.GetUpvalue(I).Name);
  finally
    ACtx.SwapState(OldTemplate, OldScope);
    ChildScope.Free;
  end;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  KeyIdx := ACtx.Template.AddConstantString(AKey);
  TargetReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABC(OP_MOVE, TargetReg, ADest, 0));
  AccessorReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, AccessorReg, FuncIdx));
  EmitInstruction(ACtx, EncodeABC(OP_DEFINE_ACCESSOR_CONST, TargetReg, 0, UInt16(KeyIdx)));
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileSetterProperty(const ACtx: TGocciaCompilationContext;
  const ADest: UInt16; const AKey: string;
  const ASetter: TGocciaSetterExpression);
var
  OldTemplate: TGocciaFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TGocciaFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  ChildCtx: TGocciaCompilationContext;
  FuncIdx: UInt16;
  TargetReg, AccessorReg: UInt16;
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
  SetterParams := ASetter.Parameters;
  ChildTemplate.ParameterCount := Length(SetterParams);
  if (Length(SetterParams) > 0) and
     (SetterParams[0].IsRest or Assigned(SetterParams[0].DefaultValue)) then
    ChildTemplate.FormalParameterCount := 0
  else
    ChildTemplate.FormalParameterCount := 1;
  for I := 0 to High(SetterParams) do
  begin
    if SetterParams[I].IsPattern then
      ChildScope.DeclareLocal(SyntheticParamLocalName(I), False)
    else
      ChildScope.DeclareLocal(SetterParams[I].Name, False);
  end;
  for I := 0 to High(SetterParams) do
    if SetterParams[I].IsPattern and Assigned(SetterParams[I].Pattern) then
      CollectDestructuringBindings(SetterParams[I].Pattern, ChildScope);
  ArgumentsSlot := DeclareArgumentsObjectLocal(ACtx, ChildScope, SetterParams,
    ChildTemplate.SourceText);
  ApplyParameterTypeAnnotations(ChildScope, ChildTemplate, SetterParams,
    ACtx.StrictTypes);

  ACtx.SwapState(ChildTemplate, ChildScope);
  try
    ChildCtx := ACtx;
    ChildCtx.Template := ChildTemplate;
    ChildCtx.Scope := ChildScope;
    ChildCtx.NonStrictMode := ACtx.NonStrictMode and
      not ChildTemplate.StrictCode;
    ChildCtx.DerivedConstructorThisGuard := False;
    EmitLineMapping(ChildCtx, ASetter.Line, ASetter.Column);
    EmitCreateArgumentsObject(ChildCtx, ArgumentsSlot,
      ChildCtx.NonStrictMode and ParameterListIsSimple(SetterParams),
      Length(SetterParams));
    EmitDefaultParameters(ChildCtx, SetterParams);
    EmitDestructuringParameters(ChildCtx, SetterParams);
    EmitParameterTypeChecks(ChildCtx, SetterParams);
    if ChildTemplate.CodeCount > High(UInt16) then
      raise Exception.Create('Parameter preamble is too large to encode');
    ChildTemplate.ParameterPreambleSize := UInt16(ChildTemplate.CodeCount);
    ACtx.CompileFunctionBody(ASetter.Body);
    ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

    for I := 0 to ChildScope.UpvalueCount - 1 do
      ChildTemplate.AddUpvalueDescriptor(
        ChildScope.GetUpvalue(I).IsLocal,
        ChildScope.GetUpvalue(I).Index,
        ChildScope.GetUpvalue(I).Name);
  finally
    ACtx.SwapState(OldTemplate, OldScope);
    ChildScope.Free;
  end;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  KeyIdx := ACtx.Template.AddConstantString(AKey);
  TargetReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABC(OP_MOVE, TargetReg, ADest, 0));
  AccessorReg := ACtx.Scope.AllocateRegister;
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, AccessorReg, FuncIdx));
  EmitInstruction(ACtx, EncodeABC(OP_DEFINE_ACCESSOR_CONST, TargetReg, ACCESSOR_FLAG_SETTER, UInt16(KeyIdx)));
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileComputedGetterProperty(const ACtx: TGocciaCompilationContext;
  const ADest: UInt16; const AKeyReg: UInt16;
  const AGetter: TGocciaGetterExpression);
var
  OldTemplate: TGocciaFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TGocciaFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  ChildCtx: TGocciaCompilationContext;
  FuncIdx: UInt16;
  TargetReg, AccessorReg: UInt16;
  EmptyParams: TGocciaParameterArray;
  ArgumentsSlot: Integer;
  I: Integer;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TGocciaFunctionTemplate.Create('get [computed]');
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.StrictCode := ChildBodyIsStrictCode(ACtx, AGetter.Body);
  ChildTemplate.StrictThis := ChildTemplate.StrictCode;
  ChildTemplate.SourceText := AGetter.SourceText;
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);
  ChildScope.DeclareLocal(KEYWORD_THIS, False);
  ChildTemplate.ParameterCount := 0;
  SetLength(EmptyParams, 0);
  ArgumentsSlot := DeclareArgumentsObjectLocal(ACtx, ChildScope, EmptyParams,
    ChildTemplate.SourceText);

  ACtx.SwapState(ChildTemplate, ChildScope);
  try
    ChildCtx := ACtx;
    ChildCtx.Template := ChildTemplate;
    ChildCtx.Scope := ChildScope;
    ChildCtx.NonStrictMode := ACtx.NonStrictMode and
      not ChildTemplate.StrictCode;
    ChildCtx.DerivedConstructorThisGuard := False;
    EmitLineMapping(ChildCtx, AGetter.Line, AGetter.Column);
    EmitCreateArgumentsObject(ChildCtx, ArgumentsSlot,
      ChildCtx.NonStrictMode and ParameterListIsSimple(EmptyParams),
      Length(EmptyParams));
    ACtx.CompileFunctionBody(AGetter.Body);
    ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

    for I := 0 to ChildScope.UpvalueCount - 1 do
      ChildTemplate.AddUpvalueDescriptor(
        ChildScope.GetUpvalue(I).IsLocal,
        ChildScope.GetUpvalue(I).Index,
        ChildScope.GetUpvalue(I).Name);
  finally
    ACtx.SwapState(OldTemplate, OldScope);
    ChildScope.Free;
  end;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  TargetReg := ACtx.Scope.AllocateRegister;
  AccessorReg := ACtx.Scope.AllocateRegister;
  Assert(AccessorReg = TargetReg + 1,
    'OP_DEFINE_ACCESSOR_* expects accessor register at target + 1');
  EmitInstruction(ACtx, EncodeABC(OP_MOVE, TargetReg, ADest, 0));
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, AccessorReg, FuncIdx));
  EmitInstruction(ACtx, EncodeABC(OP_SET_FUNCTION_NAME, AccessorReg,
    AKeyReg, FUNCTION_NAME_PREFIX_GET));
  EmitInstruction(ACtx, EncodeABC(OP_DEFINE_ACCESSOR_DYNAMIC, TargetReg, 0,
    AKeyReg));
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileComputedSetterProperty(const ACtx: TGocciaCompilationContext;
  const ADest: UInt16; const AKeyReg: UInt16;
  const ASetter: TGocciaSetterExpression);
var
  OldTemplate: TGocciaFunctionTemplate;
  OldScope: TGocciaCompilerScope;
  ChildTemplate: TGocciaFunctionTemplate;
  ChildScope: TGocciaCompilerScope;
  ChildCtx: TGocciaCompilationContext;
  FuncIdx: UInt16;
  TargetReg, AccessorReg: UInt16;
  SetterParams: TGocciaParameterArray;
  ArgumentsSlot: Integer;
  I: Integer;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  ChildTemplate := TGocciaFunctionTemplate.Create('set [computed]');
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.StrictCode := ChildBodyIsStrictCode(ACtx, ASetter.Body);
  ChildTemplate.StrictThis := ChildTemplate.StrictCode;
  ChildTemplate.SourceText := ASetter.SourceText;
  ChildScope := TGocciaCompilerScope.Create(OldScope, 0);
  ChildScope.DeclareLocal(KEYWORD_THIS, False);
  SetterParams := ASetter.Parameters;
  ChildTemplate.ParameterCount := Length(SetterParams);
  if (Length(SetterParams) > 0) and
     (SetterParams[0].IsRest or Assigned(SetterParams[0].DefaultValue)) then
    ChildTemplate.FormalParameterCount := 0
  else
    ChildTemplate.FormalParameterCount := 1;
  for I := 0 to High(SetterParams) do
  begin
    if SetterParams[I].IsPattern then
      ChildScope.DeclareLocal(SyntheticParamLocalName(I), False)
    else
      ChildScope.DeclareLocal(SetterParams[I].Name, False);
  end;
  for I := 0 to High(SetterParams) do
    if SetterParams[I].IsPattern and Assigned(SetterParams[I].Pattern) then
      CollectDestructuringBindings(SetterParams[I].Pattern, ChildScope);
  ArgumentsSlot := DeclareArgumentsObjectLocal(ACtx, ChildScope, SetterParams,
    ChildTemplate.SourceText);
  ApplyParameterTypeAnnotations(ChildScope, ChildTemplate, SetterParams,
    ACtx.StrictTypes);

  ACtx.SwapState(ChildTemplate, ChildScope);
  try
    ChildCtx := ACtx;
    ChildCtx.Template := ChildTemplate;
    ChildCtx.Scope := ChildScope;
    ChildCtx.NonStrictMode := ACtx.NonStrictMode and
      not ChildTemplate.StrictCode;
    ChildCtx.DerivedConstructorThisGuard := False;
    EmitLineMapping(ChildCtx, ASetter.Line, ASetter.Column);
    EmitCreateArgumentsObject(ChildCtx, ArgumentsSlot,
      ChildCtx.NonStrictMode and ParameterListIsSimple(SetterParams),
      Length(SetterParams));
    EmitDefaultParameters(ChildCtx, SetterParams);
    EmitDestructuringParameters(ChildCtx, SetterParams);
    EmitParameterTypeChecks(ChildCtx, SetterParams);
    if ChildTemplate.CodeCount > High(UInt16) then
      raise Exception.Create('Parameter preamble is too large to encode');
    ChildTemplate.ParameterPreambleSize := UInt16(ChildTemplate.CodeCount);
    ACtx.CompileFunctionBody(ASetter.Body);
    ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

    for I := 0 to ChildScope.UpvalueCount - 1 do
      ChildTemplate.AddUpvalueDescriptor(
        ChildScope.GetUpvalue(I).IsLocal,
        ChildScope.GetUpvalue(I).Index,
        ChildScope.GetUpvalue(I).Name);
  finally
    ACtx.SwapState(OldTemplate, OldScope);
    ChildScope.Free;
  end;

  FuncIdx := OldTemplate.AddFunction(ChildTemplate);
  TargetReg := ACtx.Scope.AllocateRegister;
  AccessorReg := ACtx.Scope.AllocateRegister;
  Assert(AccessorReg = TargetReg + 1,
    'OP_DEFINE_ACCESSOR_* expects accessor register at target + 1');
  EmitInstruction(ACtx, EncodeABC(OP_MOVE, TargetReg, ADest, 0));
  EmitInstruction(ACtx, EncodeABx(OP_CLOSURE, AccessorReg, FuncIdx));
  EmitInstruction(ACtx, EncodeABC(OP_SET_FUNCTION_NAME, AccessorReg,
    AKeyReg, FUNCTION_NAME_PREFIX_SET));
  EmitInstruction(ACtx, EncodeABC(OP_DEFINE_ACCESSOR_DYNAMIC, TargetReg,
    ACCESSOR_FLAG_SETTER, AKeyReg));
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileObject(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaObjectExpression; const ADest: UInt16);
var
  I: Integer;
  Key: string;
  ValExpr: TGocciaExpression;
  FinalExpr: TGocciaExpression;
  KeyReg, ValReg: UInt16;
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
      Key := Order[I].StaticKey;
      case Order[I].PropertyType of
        pstStatic:
        begin
          ValExpr := Order[I].Expression;
          if (not Assigned(ValExpr)) and
             AExpr.Properties.TryGetValue(Key, FinalExpr) then
            ValExpr := FinalExpr;

          if Assigned(ValExpr) then
            CompileObjectProperty(ACtx, ADest, Key, ValExpr,
              Order[I].UsesColonSyntax);
        end;
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
              EmitInstruction(ACtx, EncodeABC(OP_TO_PROPERTY_KEY, KeyReg,
                KeyReg, 0));
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
              if IsAnonymousFunctionNameExpression(Pair.Value) then
                EmitInstruction(ACtx, EncodeABC(OP_SET_FUNCTION_NAME, ValReg,
                  KeyReg, FUNCTION_NAME_PREFIX_NONE));
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
        pstComputedGetter:
        begin
          if (Order[I].ComputedIndex >= 0) and
             (Order[I].ComputedIndex <= High(AExpr.ComputedPropertiesInOrder)) then
          begin
            Pair := AExpr.ComputedPropertiesInOrder[Order[I].ComputedIndex];
            KeyReg := ACtx.Scope.AllocateRegister;
            ACtx.CompileExpression(Pair.Key, KeyReg);
            EmitInstruction(ACtx, EncodeABC(OP_TO_PROPERTY_KEY, KeyReg,
              KeyReg, 0));
            CompileComputedGetterProperty(ACtx, ADest, KeyReg,
              TGocciaGetterExpression(Pair.Value));
            ACtx.Scope.FreeRegister;
          end;
        end;
        pstComputedSetter:
        begin
          if (Order[I].ComputedIndex >= 0) and
             (Order[I].ComputedIndex <= High(AExpr.ComputedPropertiesInOrder)) then
          begin
            Pair := AExpr.ComputedPropertiesInOrder[Order[I].ComputedIndex];
            KeyReg := ACtx.Scope.AllocateRegister;
            ACtx.CompileExpression(Pair.Key, KeyReg);
            EmitInstruction(ACtx, EncodeABC(OP_TO_PROPERTY_KEY, KeyReg,
              KeyReg, 0));
            CompileComputedSetterProperty(ACtx, ADest, KeyReg,
              TGocciaSetterExpression(Pair.Value));
            ACtx.Scope.FreeRegister;
          end;
        end;
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
  const AExpr: TGocciaTemplateLiteralExpression; const ADest: UInt16);
begin
  EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, ADest,
    ACtx.Template.AddConstantString(AExpr.Value)));
end;

// ES2026 §13.15.5.1 Template Literals — compile pre-segmented template.
// Expression parts must use OP_TO_STRING to apply the "string" ToPrimitive
// hint (preferring toString() over valueOf()), matching the spec's ToString
// semantics rather than OP_ADD's "default" hint.
procedure CompileTemplateWithInterpolation(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaTemplateWithInterpolationExpression; const ADest: UInt16);
var
  I: Integer;
  PartReg: UInt16;
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
  const AObjReg, AValueReg: UInt16; const APropName: string);
var
  GlobalObjReg, DefPropReg, ObjArgReg, NameArgReg, DescReg: UInt16;
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
  // Look up Object.defineProperty
  EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, GlobalObjReg, ObjectIdx));
  EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, DefPropReg, GlobalObjReg, UInt16(DefPropIdx)));

  // Argument 1: the target object
  EmitInstruction(ACtx, EncodeABC(OP_MOVE, ObjArgReg, AObjReg, 0));

  // Argument 2: the property name string
  EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, NameArgReg, PropNameIdx));

  // Argument 3: the descriptor {value: AValueReg}
  // Omitting enumerable/writable/configurable → all default to false per spec
  EmitInstruction(ACtx, EncodeABC(OP_NEW_OBJECT, DescReg, 0, 0));
  EmitInstruction(ACtx, EncodeABC(OP_SET_PROP_CONST, DescReg, UInt16(ValuePropIdx), AValueReg));

  // Object.defineProperty(obj, propName, descriptor) — 3 args
  EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, DefPropReg, 3, 0));

  ACtx.Scope.FreeRegister; // DescReg
  ACtx.Scope.FreeRegister; // NameArgReg
  ACtx.Scope.FreeRegister; // ObjArgReg
  ACtx.Scope.FreeRegister; // DefPropReg
  ACtx.Scope.FreeRegister; // GlobalObjReg
end;

// ES2026 §13.2.8.3 GetTemplateObject — step 12: SetIntegrityLevel(template, frozen)
procedure EmitObjectFreeze(const ACtx: TGocciaCompilationContext; const AReg: UInt16);
var
  GlobalObjReg, FreezeReg, FreezeArgReg: UInt16;
  ObjectIdx, FreezeIdx: UInt16;
begin
  GlobalObjReg := ACtx.Scope.AllocateRegister;
  FreezeReg := ACtx.Scope.AllocateRegister;
  FreezeArgReg := ACtx.Scope.AllocateRegister;

  ObjectIdx := ACtx.Template.AddConstantString(CONSTRUCTOR_OBJECT);
  FreezeIdx := ACtx.Template.AddConstantString(PROP_FREEZE);
  EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, GlobalObjReg, ObjectIdx));
  EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, FreezeReg, GlobalObjReg, UInt16(FreezeIdx)));
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
  out ABaseReg, AObjReg: UInt16;
  out AIsMethodCall: Boolean);
var
  MemberExpr: TGocciaMemberExpression;
  IdentExpr: TGocciaIdentifierExpression;
  KeyReg, CondReg: UInt16;
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
        EmitInstruction(ACtx, EncodeABC(GetWithBindingOpcode(ACtx), ABaseReg,
          AObjReg, KeyReg));
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
  const AExpr: TGocciaTaggedTemplateExpression; const ADest: UInt16;
  const ATail: Boolean = False);
var
  ObjReg, BaseReg, Arg0Reg: UInt16;
  ArgCount, I: Integer;
  IsMethodCall: Boolean;
  TailFlag: UInt16;
begin
  ArgCount := 1 + AExpr.Expressions.Count; // template object + substitution values
  if ArgCount > High(UInt16) then
    raise Exception.Create('Compiler error: too many tagged template substitutions (>65534)');

  if ATail then
    TailFlag := CALL_FLAG_TAIL
  else
    TailFlag := 0;

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
    EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, BaseReg, UInt16(ArgCount),
      TailFlag))
  else
    EmitInstruction(ACtx, EncodeABC(OP_CALL, BaseReg, UInt16(ArgCount), TailFlag));

  for I := 0 to AExpr.Expressions.Count - 1 do
    ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister; // Arg0Reg

  if ADest <> BaseReg then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, BaseReg, 0));

  ACtx.Scope.FreeRegister; // BaseReg
  if IsMethodCall then
    ACtx.Scope.FreeRegister; // ObjReg
end;

// ES2026 §15.10.2 HasCallInTailPosition.  Compiles AExpr into ADest, tagging the
// call(s) the static semantics place in tail position so the VM may reuse the
// current frame.  Only the tail-position-preserving expression forms recurse;
// every other shape (including a call argument, the callee, or a logical
// short-circuit operand that is returned directly) is compiled normally and is
// therefore NOT a tail call.
procedure CompileReturnValueTailAware(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaExpression; const ADest: UInt16);
var
  Conditional: TGocciaConditionalExpression;
  Binary: TGocciaBinaryExpression;
  Sequence: TGocciaSequenceExpression;
  TempReg: UInt16;
  ElseJump, EndJump, ShortCircuitJump, I: Integer;
begin
  // CallExpression Arguments / CallExpression TemplateLiteral: the call itself is
  // in tail position.
  if AExpr is TGocciaCallExpression then
  begin
    CompileCall(ACtx, TGocciaCallExpression(AExpr), ADest, True);
    Exit;
  end;
  if AExpr is TGocciaTaggedTemplateExpression then
  begin
    CompileTaggedTemplate(ACtx, TGocciaTaggedTemplateExpression(AExpr), ADest,
      True);
    Exit;
  end;

  // ConditionalExpression: both branches inherit tail position.
  if AExpr is TGocciaConditionalExpression then
  begin
    Conditional := TGocciaConditionalExpression(AExpr);
    ACtx.CompileExpression(Conditional.Condition, ADest);
    ElseJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, ADest);
    CompileReturnValueTailAware(ACtx, Conditional.Consequent, ADest);
    EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
    PatchJumpTarget(ACtx, ElseJump);
    CompileReturnValueTailAware(ACtx, Conditional.Alternate, ADest);
    PatchJumpTarget(ACtx, EndJump);
    Exit;
  end;

  // LogicalANDExpression / LogicalORExpression / CoalesceExpression: only the
  // right operand inherits tail position.  The short-circuit (left) result is
  // produced as an ordinary value.
  if AExpr is TGocciaBinaryExpression then
  begin
    Binary := TGocciaBinaryExpression(AExpr);
    if (Binary.Operator = gttAnd) or (Binary.Operator = gttOr) or
       (Binary.Operator = gttNullishCoalescing) then
    begin
      ACtx.CompileExpression(Binary.Left, ADest);
      case Binary.Operator of
        gttAnd:
          ShortCircuitJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, ADest);
        gttOr:
          ShortCircuitJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, ADest);
      else
        ShortCircuitJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_NOT_NULLISH,
          ADest);
      end;
      CompileReturnValueTailAware(ACtx, Binary.Right, ADest);
      PatchJumpTarget(ACtx, ShortCircuitJump);
      Exit;
    end;
  end;

  // Expression `,` AssignmentExpression: only the final operand inherits tail
  // position.
  if AExpr is TGocciaSequenceExpression then
  begin
    Sequence := TGocciaSequenceExpression(AExpr);
    if Sequence.Expressions.Count >= 2 then
    begin
      TempReg := ACtx.Scope.AllocateRegister;
      try
        for I := 0 to Sequence.Expressions.Count - 2 do
          ACtx.CompileExpression(Sequence.Expressions[I], TempReg);
      finally
        ACtx.Scope.FreeRegister;
      end;
      CompileReturnValueTailAware(ACtx,
        Sequence.Expressions[Sequence.Expressions.Count - 1], ADest);
      Exit;
    end;
  end;

  // Any other production terminates tail position: compile as an ordinary value.
  ACtx.CompileExpression(AExpr, ADest);
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
  const AExpr: TGocciaNewExpression; const ADest: UInt16);
var
  CtorReg, ArgsReg: UInt16;
  ArgCount, I: Integer;
begin
  CtorReg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AExpr.Callee, CtorReg);

  if NewExprHasSpread(AExpr) then
  begin
    ArgsReg := ACtx.Scope.AllocateRegister;
    CompileSpreadArgsArrayFromList(ACtx, AExpr.Arguments, ArgsReg);
    EmitInstruction(ACtx, EncodeABC(OP_CONSTRUCT_SPREAD, ADest, CtorReg,
      ArgsReg));
    ACtx.Scope.FreeRegister; // ArgsReg
  end
  else
  begin
    ArgCount := AExpr.Arguments.Count;
    if ArgCount > High(UInt16) then
      raise Exception.Create('Compiler error: too many constructor arguments (>65535)');
    for I := 0 to ArgCount - 1 do
      ACtx.CompileExpression(AExpr.Arguments[I], ACtx.Scope.AllocateRegister);
    EmitInstruction(ACtx, EncodeABC(OP_CONSTRUCT, ADest, CtorReg, UInt16(ArgCount)));
    for I := 0 to ArgCount - 1 do
      ACtx.Scope.FreeRegister;
  end;

  ACtx.Scope.FreeRegister; // CtorReg
end;

procedure CompileComputedPropertyAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaComputedPropertyAssignmentExpression; const ADest: UInt16);
var
  BaseReg, KeyReg, ObjReg, SuperReg, ThisReg, ValReg: UInt16;
begin
  if AExpr.ObjectExpr is TGocciaSuperExpression then
  begin
    PrepareSuperPropertyBase(ACtx, ThisReg, BaseReg, SuperReg);
    KeyReg := ACtx.Scope.AllocateRegister;
    ValReg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(AExpr.PropertyExpression, KeyReg);
    EmitInstruction(ACtx, EncodeABC(OP_SUPER_BASE, SuperReg, SuperReg,
      ThisReg));
    ACtx.CompileExpression(AExpr.Value, ValReg);
    EmitInstruction(ACtx, EncodeABC(OP_TO_PROPERTY_KEY, KeyReg, KeyReg, 0));
    EmitInstruction(ACtx, EncodeABC(OP_SUPER_SET_BASE, BaseReg, KeyReg,
      ValReg));

    if ADest <> ValReg then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, ValReg, 0));

    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    Exit;
  end;

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
  const AExpr: TGocciaFunctionExpression; const ADest: UInt16;
  const ATemplateName: string; const ABindOwnName: Boolean);
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
  NameSlot: UInt16;
  ClosedLocals: TArray<UInt16>;
  ClosedCount: Integer;
begin
  OldTemplate := ACtx.Template;
  OldScope := ACtx.Scope;

  // ES2026 §15.2.5: Named function expressions bind the name in an inner
  // block scope visible inside the body via upvalue capture
  HasNameBinding := ABindOwnName and (AExpr.Name <> '');
  if HasNameBinding then
  begin
    OldScope.BeginScope;
    NameSlot := OldScope.DeclareLocal(AExpr.Name, True);
    OldScope.MarkNonStrictImmutable(OldScope.ResolveLocal(AExpr.Name));
  end;

  ChildTemplate := TGocciaFunctionTemplate.Create(ATemplateName);
  ChildTemplate.DebugInfo := TGocciaDebugInfo.Create(ACtx.SourcePath);
  ChildTemplate.IsAsync := AExpr.IsAsync;
  ChildTemplate.IsGenerator := AExpr.IsGenerator;
  ChildTemplate.HasOwnPrototype := AExpr.HasOwnPrototype;
  ChildTemplate.StrictCode := AExpr.ParsedInStrictMode or
    ChildBodyIsStrictCode(ACtx, AExpr.Body);
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
      ChildScope.DeclareLocal(SyntheticParamLocalName(I), False)
    else
      ChildScope.DeclareLocal(AExpr.Parameters[I].Name, False);
  end;
  for I := 0 to High(AExpr.Parameters) do
    if AExpr.Parameters[I].IsPattern and Assigned(AExpr.Parameters[I].Pattern) then
      CollectDestructuringBindings(AExpr.Parameters[I].Pattern, ChildScope);

  ArgumentsSlot := DeclareArgumentsObjectLocal(ACtx, ChildScope, AExpr.Parameters,
    ChildTemplate.SourceText);

  ApplyParameterTypeAnnotations(ChildScope, ChildTemplate, AExpr.Parameters,
    ACtx.StrictTypes);

  if FormalCount < 0 then
    FormalCount := Length(AExpr.Parameters);
  ChildTemplate.FormalParameterCount := UInt16(FormalCount);
  if Assigned(ACtx.FormalParameterCounts) then
    ACtx.FormalParameterCounts.AddOrSetValue(ChildTemplate, FormalCount);

  ACtx.SwapState(ChildTemplate, ChildScope);
  try
    ChildCtx := ACtx;
    ChildCtx.Template := ChildTemplate;
    ChildCtx.Scope := ChildScope;
    ChildCtx.NonStrictMode := ACtx.NonStrictMode and
      not ChildTemplate.StrictCode;
    ChildCtx.DerivedConstructorThisGuard := False;

    EmitCreateArgumentsObject(ChildCtx, ArgumentsSlot,
      ChildCtx.NonStrictMode and ParameterListIsSimple(AExpr.Parameters),
      Length(AExpr.Parameters));

    if (RestParamIndex >= 0) and
       not ParameterListHasDefaultValues(AExpr.Parameters) then
    begin
      if AExpr.Parameters[RestParamIndex].IsPattern then
        RestReg := ChildScope.ResolveLocal(SyntheticParamLocalName(RestParamIndex))
      else
        RestReg := ChildScope.ResolveLocal(AExpr.Parameters[RestParamIndex].Name);
      EmitInstruction(ChildCtx,
        EncodeABC(OP_PACK_ARGS, UInt16(RestReg), UInt16(RestParamIndex), 0));
    end;

    EmitDefaultParameters(ChildCtx, AExpr.Parameters);
    EmitDestructuringParameters(ChildCtx, AExpr.Parameters);
    EmitParameterTypeChecks(ChildCtx, AExpr.Parameters);
    if ChildTemplate.CodeCount > High(UInt16) then
      raise Exception.Create('Parameter preamble is too large to encode');
    ChildTemplate.ParameterPreambleSize := UInt16(ChildTemplate.CodeCount);

    ACtx.CompileFunctionBody(AExpr.Body);

    ChildTemplate.MaxRegisters := ChildScope.MaxSlot;

    for I := 0 to ChildScope.UpvalueCount - 1 do
      ChildTemplate.AddUpvalueDescriptor(
        ChildScope.GetUpvalue(I).IsLocal,
        ChildScope.GetUpvalue(I).Index,
        ChildScope.GetUpvalue(I).Name);
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
      EmitInstruction(ACtx, EncodeABx(OP_CLOSE_UPVALUE, 0, UInt16(ClosedLocals[I])));
  end;
end;

procedure CompileCompoundAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaCompoundAssignmentExpression; const ADest: UInt16);
var
  LocalIdx, UpvalIdx: Integer;
  Slot, RegVal, RegResult, RegTemp, CondReg, ArgReg, ObjReg, KeyReg: UInt16;
  ReferenceReg: Integer;
  NameIdx: UInt16;
  Op, FloatOp: TGocciaOpCode;
  Upvalue: TGocciaCompilerUpvalue;
  LocalType, ValType, ResultType: TGocciaLocalType;
  ArithOp: TGocciaTokenType;
  I, EndCount, JumpIdx, MissJump, OkJump: Integer;
  EndJumps: array of Integer;
  PreparedTarget: TPreparedDestructuringTarget;
begin
  // ES2026 §13.15.2 AssignmentExpression : LeftHandSideExpression ??=/&&=/||= AssignmentExpression
  if IsShortCircuitAssignment(AExpr.Operator) then
  begin
    if ShouldTryWithBinding(ACtx.Scope, AExpr.Name) then
    begin
      InitPreparedDestructuringTarget(PreparedTarget);
      try
        Op := ShortCircuitJumpOp(AExpr.Operator);
        PrepareIdentifierWithBindingTarget(ACtx, AExpr.Name, PreparedTarget);
        EmitPreparedIdentifierWithBindingLoad(ACtx, PreparedTarget, ADest);
        JumpIdx := EmitJumpInstruction(ACtx, Op, ADest);
        CompileAssignmentValue(ACtx, AExpr.Value, ADest, AExpr.Name,
          AExpr.InferName);
        EmitPreparedDestructuringTargetStore(ACtx, PreparedTarget, ADest);
        PatchJumpTarget(ACtx, JumpIdx);
      finally
        ReleasePreparedDestructuringTarget(ACtx, PreparedTarget);
      end;
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
        CompileAssignmentValue(ACtx, AExpr.Value, ADest, AExpr.Name,
          AExpr.InferName);
        LocalType := ACtx.Scope.GetLocal(LocalIdx).TypeHint;
        if ACtx.Scope.GetLocal(LocalIdx).IsStrictlyTyped and
           (LocalType <> sltUntyped) and
           not TypesAreCompatible(InferLocalType(AExpr.Value), LocalType) then
          EmitInstruction(ACtx, EncodeABC(OP_CHECK_TYPE, ADest,
            UInt8(Ord(LocalType)), 0));
        if ACtx.Scope.GetLocal(LocalIdx).IsConst then
        begin
          if not ShouldIgnoreNonStrictImmutableLocalAssignment(ACtx,
             ACtx.Scope.GetLocal(LocalIdx)) then
            EmitConstAssignmentError(ACtx);
        end
        else
        begin
          EmitSetGlobalByName(ACtx, ADest, AExpr.Name);
          EmitExportBindingUpdates(ACtx,
            ACtx.Scope.GetLocal(LocalIdx).ExportNames,
            ACtx.Scope.GetLocal(LocalIdx).ExportNameCount, ADest);
          SetNonStrictLocalTypeHint(ACtx, LocalIdx, sltUntyped);
        end;
        PatchJumpTarget(ACtx, JumpIdx);
        Exit;
      end;

      Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
      EmitInstruction(ACtx, EncodeABx(OP_GET_LOCAL, ADest, Slot));
      JumpIdx := EmitJumpInstruction(ACtx, Op, ADest);
      LocalType := ACtx.Scope.GetLocal(LocalIdx).TypeHint;
      if ACtx.Scope.GetLocal(LocalIdx).IsConst then
      begin
        RegVal := ACtx.Scope.AllocateRegister;
        CompileAssignmentValue(ACtx, AExpr.Value, RegVal, AExpr.Name,
          AExpr.InferName);
        if ACtx.Scope.GetLocal(LocalIdx).IsStrictlyTyped and
           (LocalType <> sltUntyped) and
           not TypesAreCompatible(InferLocalType(AExpr.Value), LocalType) then
          EmitInstruction(ACtx, EncodeABC(OP_CHECK_TYPE, RegVal,
            UInt8(Ord(LocalType)), 0));
        if ShouldIgnoreNonStrictImmutableLocalAssignment(ACtx,
           ACtx.Scope.GetLocal(LocalIdx)) then
        begin
          if ADest <> RegVal then
            EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, RegVal, 0));
        end
        else
          EmitConstAssignmentError(ACtx);
        ACtx.Scope.FreeRegister;
      end
      else
      begin
        CompileAssignmentValue(ACtx, AExpr.Value, ADest, AExpr.Name,
          AExpr.InferName);
        EmitStrictLocalTypeCheck(ACtx, LocalIdx, ADest,
          InferLocalType(AExpr.Value));
        if ADest <> Slot then
          EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, ADest, Slot));
        EmitExportBindingUpdates(ACtx,
          ACtx.Scope.GetLocal(LocalIdx).ExportNames,
          ACtx.Scope.GetLocal(LocalIdx).ExportNameCount, ADest);
        SetNonStrictLocalTypeHint(ACtx, LocalIdx, sltUntyped);
      end;
      PatchJumpTarget(ACtx, JumpIdx);
      Exit;
    end;

    UpvalIdx := ACtx.Scope.ResolveUpvalue(AExpr.Name);
    if UpvalIdx >= 0 then
    begin
      Upvalue := ACtx.Scope.GetUpvalue(UpvalIdx);
      ReferenceReg := -1;
      if ExpressionContainsDirectEval(AExpr.Value) then
        ReferenceReg := PrepareUpvalueAssignmentReference(ACtx, UpvalIdx);
      if ReferenceReg >= 0 then
        EmitPreparedUpvalueLoad(ACtx, Upvalue, UpvalIdx,
          UInt16(ReferenceReg), ADest)
      else if Upvalue.IsGlobalBacked then
        EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, ADest,
          ACtx.Template.AddConstantString(AExpr.Name)))
      else
        EmitInstruction(ACtx, EncodeABx(OP_GET_UPVALUE, ADest,
          UInt16(UpvalIdx)));
      JumpIdx := EmitJumpInstruction(ACtx, Op, ADest);
      CompileAssignmentValue(ACtx, AExpr.Value, ADest, AExpr.Name,
        AExpr.InferName);
      if ReferenceReg >= 0 then
        EmitPreparedUpvalueAssignment(ACtx, Upvalue, UpvalIdx,
          UInt16(ReferenceReg), ADest, ADest, InferLocalType(AExpr.Value))
      else
        EmitCurrentUpvalueAssignment(ACtx, Upvalue, UpvalIdx, ADest, ADest,
          InferLocalType(AExpr.Value));
      PatchJumpTarget(ACtx, JumpIdx);
      if ReferenceReg >= 0 then
        ACtx.Scope.FreeRegister;
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
    CompileAssignmentValue(ACtx, AExpr.Value, ADest, AExpr.Name,
      AExpr.InferName);
    EmitSetGlobalByIndex(ACtx, ADest, NameIdx);
    PatchJumpTarget(ACtx, JumpIdx);
    Exit;
  end;

  Op := CompoundOpToRuntimeOp(AExpr.Operator);

  if ShouldTryWithBinding(ACtx.Scope, AExpr.Name) then
  begin
    InitPreparedDestructuringTarget(PreparedTarget);
    RegVal := ACtx.Scope.AllocateRegister;
    RegResult := ACtx.Scope.AllocateRegister;
    try
      PrepareIdentifierWithBindingTarget(ACtx, AExpr.Name, PreparedTarget);
      EmitPreparedIdentifierWithBindingLoad(ACtx, PreparedTarget, RegResult);
      ACtx.CompileExpression(AExpr.Value, RegVal);
      EmitInstruction(ACtx, EncodeABC(Op, ADest, RegResult, RegVal));
      EmitPreparedDestructuringTargetStore(ACtx, PreparedTarget, ADest);
    finally
      ReleasePreparedDestructuringTarget(ACtx, PreparedTarget);
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
    end;
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
      begin
        if ShouldIgnoreNonStrictImmutableLocalAssignment(ACtx,
           ACtx.Scope.GetLocal(LocalIdx)) then
          EmitInstruction(ACtx, EncodeABC(Op, ADest, RegResult, RegVal))
        else
          EmitConstAssignmentError(ACtx);
      end
      else
      begin
        EmitInstruction(ACtx, EncodeABC(Op, ADest, RegResult, RegVal));
        EmitStrictLocalTypeCheck(ACtx, LocalIdx, ADest, ResultType);
        EmitSetGlobalByName(ACtx, ADest, AExpr.Name);
        EmitExportBindingUpdates(ACtx,
          ACtx.Scope.GetLocal(LocalIdx).ExportNames,
          ACtx.Scope.GetLocal(LocalIdx).ExportNameCount, ADest);
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
      if ShouldIgnoreNonStrictImmutableLocalAssignment(ACtx,
         ACtx.Scope.GetLocal(LocalIdx)) then
        EmitInstruction(ACtx, EncodeABC(Op, ADest, Slot, RegVal))
      else
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
    EmitExportBindingUpdates(ACtx,
      ACtx.Scope.GetLocal(LocalIdx).ExportNames,
      ACtx.Scope.GetLocal(LocalIdx).ExportNameCount, RegTemp);
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
    Upvalue := ACtx.Scope.GetUpvalue(UpvalIdx);
    RegVal := ACtx.Scope.AllocateRegister;
    RegResult := ACtx.Scope.AllocateRegister;
    ReferenceReg := -1;
    if ExpressionContainsDirectEval(AExpr.Value) then
      ReferenceReg := PrepareUpvalueAssignmentReference(ACtx, UpvalIdx);
    if ReferenceReg >= 0 then
      EmitPreparedUpvalueLoad(ACtx, Upvalue, UpvalIdx,
        UInt16(ReferenceReg), RegResult)
    else if Upvalue.IsGlobalBacked then
      EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, RegResult,
        ACtx.Template.AddConstantString(AExpr.Name)))
    else
      EmitInstruction(ACtx, EncodeABx(OP_GET_UPVALUE, RegResult,
        UInt16(UpvalIdx)));
    ACtx.CompileExpression(AExpr.Value, RegVal);
    EmitInstruction(ACtx, EncodeABC(Op, RegResult, RegResult, RegVal));
    if ReferenceReg >= 0 then
      EmitPreparedUpvalueAssignment(ACtx, Upvalue, UpvalIdx,
        UInt16(ReferenceReg), RegResult, RegResult,
        InferLocalType(AExpr.Value))
    else
      EmitCurrentUpvalueAssignment(ACtx, Upvalue, UpvalIdx, RegResult,
        RegResult, InferLocalType(AExpr.Value));
    if ADest <> RegResult then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, RegResult, 0));
    if ReferenceReg >= 0 then
      ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    Exit;
  end;

  RegVal := ACtx.Scope.AllocateRegister;
  RegResult := ACtx.Scope.AllocateRegister;
  EmitLoadBindingByName(ACtx, AExpr.Name, RegResult, False);
  ACtx.CompileExpression(AExpr.Value, RegVal);
  EmitInstruction(ACtx, EncodeABC(Op, ADest, RegResult, RegVal));
  EmitSetGlobalByName(ACtx, ADest, AExpr.Name);
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompilePropertyCompoundAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaPropertyCompoundAssignmentExpression; const ADest: UInt16);
var
  BaseReg, CurReg, KeyReg, ObjReg, SuperReg, ThisReg, ValReg: UInt16;
  Op: TGocciaOpCode;
  PropIdx: UInt16;
  JumpIdx: Integer;
begin
  if AExpr.ObjectExpr is TGocciaSuperExpression then
  begin
    PrepareSuperPropertyBase(ACtx, ThisReg, BaseReg, SuperReg);
    KeyReg := ACtx.Scope.AllocateRegister;
    PropIdx := ACtx.Template.AddConstantString(AExpr.PropertyName);
    EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, KeyReg, PropIdx));
    EmitInstruction(ACtx, EncodeABC(OP_SUPER_GET, BaseReg, 0, KeyReg));

    if IsShortCircuitAssignment(AExpr.Operator) then
    begin
      JumpIdx := EmitJumpInstruction(ACtx,
        ShortCircuitJumpOp(AExpr.Operator), BaseReg);
      ACtx.CompileExpression(AExpr.Value, BaseReg);
      EmitInstruction(ACtx, EncodeABC(OP_SUPER_SET, BaseReg, KeyReg,
        BaseReg));
      PatchJumpTarget(ACtx, JumpIdx);

      if ADest <> BaseReg then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, BaseReg, 0));

      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
      Exit;
    end;

    Op := CompoundOpToRuntimeOp(AExpr.Operator);
    ValReg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(AExpr.Value, ValReg);
    EmitInstruction(ACtx, EncodeABC(Op, BaseReg, BaseReg, ValReg));
    EmitInstruction(ACtx, EncodeABC(OP_SUPER_SET, BaseReg, KeyReg, BaseReg));

    if ADest <> BaseReg then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, BaseReg, 0));

    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    Exit;
  end;

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
  const ADest: UInt16);
var
  BaseReg, ObjReg, KeyReg, CurReg, SuperReg, ThisReg, ValReg: UInt16;
  Op: TGocciaOpCode;
  JumpIdx: Integer;
begin
  if AExpr.ObjectExpr is TGocciaSuperExpression then
  begin
    PrepareSuperPropertyBase(ACtx, ThisReg, BaseReg, SuperReg);
    KeyReg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(AExpr.PropertyExpression, KeyReg);
    EmitInstruction(ACtx, EncodeABC(OP_SUPER_BASE, SuperReg, SuperReg,
      ThisReg));
    EmitInstruction(ACtx, EncodeABC(OP_TO_PROPERTY_KEY, KeyReg, KeyReg, 0));
    EmitInstruction(ACtx, EncodeABC(OP_SUPER_GET_BASE, BaseReg, 0, KeyReg));

    if IsShortCircuitAssignment(AExpr.Operator) then
    begin
      JumpIdx := EmitJumpInstruction(ACtx,
        ShortCircuitJumpOp(AExpr.Operator), BaseReg);
      ACtx.CompileExpression(AExpr.Value, BaseReg);
      EmitInstruction(ACtx, EncodeABC(OP_SUPER_SET_BASE, BaseReg, KeyReg,
        BaseReg));
      PatchJumpTarget(ACtx, JumpIdx);

      if ADest <> BaseReg then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, BaseReg, 0));

      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
      Exit;
    end;

    Op := CompoundOpToRuntimeOp(AExpr.Operator);
    ValReg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(AExpr.Value, ValReg);
    EmitInstruction(ACtx, EncodeABC(Op, BaseReg, BaseReg, ValReg));
    EmitInstruction(ACtx, EncodeABC(OP_SUPER_SET_BASE, BaseReg, KeyReg,
      BaseReg));

    if ADest <> BaseReg then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, BaseReg, 0));

    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    Exit;
  end;

  // ES2026 §13.15.2 AssignmentExpression : LeftHandSideExpression ??=/&&=/||= AssignmentExpression
  if IsShortCircuitAssignment(AExpr.Operator) then
  begin
    ObjReg := ACtx.Scope.AllocateRegister;
    KeyReg := ACtx.Scope.AllocateRegister;
    CurReg := ACtx.Scope.AllocateRegister;

    ACtx.CompileExpression(AExpr.ObjectExpr, ObjReg);
    ACtx.CompileExpression(AExpr.PropertyExpression, KeyReg);
    EmitInstruction(ACtx, EncodeABC(OP_VALIDATE_VALUE, ObjReg,
      VALIDATE_OP_REQUIRE_OBJECT, 0));
    EmitInstruction(ACtx, EncodeABC(OP_TO_PROPERTY_KEY, KeyReg, KeyReg, 0));
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
  EmitInstruction(ACtx, EncodeABC(OP_VALIDATE_VALUE, ObjReg,
    VALIDATE_OP_REQUIRE_OBJECT, 0));
  EmitInstruction(ACtx, EncodeABC(OP_TO_PROPERTY_KEY, KeyReg, KeyReg, 0));
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

procedure EmitIncrementStep(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIncrementExpression; const ADest, AValueReg: UInt16;
  const AOp, ANumericOp, APostNumericOp: TGocciaOpCode;
  const AKeepResult: Boolean);
begin
  if AKeepResult and not AExpr.IsPrefix and (ADest <> AValueReg) then
  begin
    EmitInstruction(ACtx, EncodeABC(APostNumericOp, ADest, AValueReg, 0));
    Exit;
  end;

  if AKeepResult and not AExpr.IsPrefix then
    raise Exception.Create(
      'Compiler error: postfix increment requires a distinct result register');

  EmitInstruction(ACtx, EncodeABC(ANumericOp, AValueReg, AValueReg, 0));
  if AKeepResult and AExpr.IsPrefix and (ADest <> AValueReg) then
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, AValueReg, 0));
end;

procedure CompileIncrementMember(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIncrementExpression; const AMember: TGocciaMemberExpression;
  const ADest: UInt16; const AOp, ANumericOp, APostNumericOp: TGocciaOpCode;
  const AKeepResult: Boolean);
var
  ObjReg, CurReg, KeyReg, SuperReg, ThisReg: UInt16;
  PropIdx: UInt16;
begin
  if AMember.ObjectExpr is TGocciaSuperExpression then
  begin
    PrepareSuperPropertyBase(ACtx, ThisReg, CurReg, SuperReg);
    KeyReg := ACtx.Scope.AllocateRegister;
    PropIdx := ACtx.Template.AddConstantString(AMember.PropertyName);
    EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, KeyReg, PropIdx));
    EmitInstruction(ACtx, EncodeABC(OP_SUPER_GET, CurReg, 0, KeyReg));
    EmitIncrementStep(ACtx, AExpr, ADest, CurReg, AOp, ANumericOp,
      APostNumericOp, AKeepResult);
    EmitInstruction(ACtx, EncodeABC(OP_SUPER_SET, CurReg, KeyReg, CurReg));

    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    Exit;
  end;

  ObjReg := ACtx.Scope.AllocateRegister;
  CurReg := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(AMember.ObjectExpr, ObjReg);
  EmitLoadPropertyByName(ACtx, CurReg, ObjReg, AMember.PropertyName);
  EmitIncrementStep(ACtx, AExpr, ADest, CurReg, AOp, ANumericOp,
    APostNumericOp, AKeepResult);
  EmitStorePropertyByName(ACtx, ObjReg, AMember.PropertyName, CurReg);

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileIncrementComputedMember(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIncrementExpression; const AMember: TGocciaMemberExpression;
  const ADest: UInt16; const AOp, ANumericOp, APostNumericOp: TGocciaOpCode;
  const AKeepResult: Boolean);
var
  ObjReg, KeyReg, CurReg, SuperReg, ThisReg: UInt16;
begin
  if AMember.ObjectExpr is TGocciaSuperExpression then
  begin
    PrepareSuperPropertyBase(ACtx, ThisReg, CurReg, SuperReg);
    KeyReg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(AMember.PropertyExpression, KeyReg);
    EmitInstruction(ACtx, EncodeABC(OP_SUPER_BASE, SuperReg, SuperReg,
      ThisReg));
    EmitInstruction(ACtx, EncodeABC(OP_TO_PROPERTY_KEY, KeyReg, KeyReg, 0));
    EmitInstruction(ACtx, EncodeABC(OP_SUPER_GET_BASE, CurReg, 0, KeyReg));
    EmitIncrementStep(ACtx, AExpr, ADest, CurReg, AOp, ANumericOp,
      APostNumericOp, AKeepResult);
    EmitInstruction(ACtx, EncodeABC(OP_SUPER_SET_BASE, CurReg, KeyReg,
      CurReg));

    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    Exit;
  end;

  ObjReg := ACtx.Scope.AllocateRegister;
  KeyReg := ACtx.Scope.AllocateRegister;
  CurReg := ACtx.Scope.AllocateRegister;

  ACtx.CompileExpression(AMember.ObjectExpr, ObjReg);
  ACtx.CompileExpression(AMember.PropertyExpression, KeyReg);
  EmitInstruction(ACtx, EncodeABC(OP_VALIDATE_VALUE, ObjReg,
    VALIDATE_OP_REQUIRE_OBJECT, 0));
  EmitInstruction(ACtx, EncodeABC(OP_TO_PROPERTY_KEY, KeyReg, KeyReg, 0));
  EmitInstruction(ACtx, EncodeABC(OP_ARRAY_GET, CurReg, ObjReg, KeyReg));
  EmitIncrementStep(ACtx, AExpr, ADest, CurReg, AOp, ANumericOp,
    APostNumericOp, AKeepResult);
  EmitInstruction(ACtx, EncodeABC(StoreByKeyOpcode(ACtx), ObjReg, KeyReg,
    CurReg));

  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
  ACtx.Scope.FreeRegister;
end;

procedure CompileIncrement(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIncrementExpression; const ADest: UInt16;
  const AKeepResult: Boolean = True);
var
  LocalIdx, UpvalIdx: Integer;
  Slot, RegResult, ObjReg, KeyReg, CondReg: UInt16;
  Op, NumericOp, PostNumericOp: TGocciaOpCode;
  Ident: TGocciaIdentifierExpression;
  MemberExpr: TGocciaMemberExpression;
  PrivateExpr: TGocciaPrivateMemberExpression;
  Upvalue: TGocciaCompilerUpvalue;
  ReservedDestRegister: Boolean;
  NameIdx: UInt16;
  I, EndCount: Integer;
  MissJump: Integer;
  EndJumps: array of Integer;
begin
  ReservedDestRegister := False;
  if AKeepResult and not AExpr.IsPrefix and
     (ADest = ACtx.Scope.NextSlot) and not IsLocalSlot(ACtx.Scope, ADest) then
  begin
    ACtx.Scope.AllocateRegister;
    ReservedDestRegister := True;
  end;

  try
    if AExpr.Operator = gttIncrement then
    begin
      Op := OP_INC;
      NumericOp := OP_INC_NUMERIC;
      PostNumericOp := OP_POST_INC_NUMERIC;
    end
    else
    begin
      Op := OP_DEC;
      NumericOp := OP_DEC_NUMERIC;
      PostNumericOp := OP_POST_DEC_NUMERIC;
    end;

    if AExpr.Operand is TGocciaMemberExpression then
    begin
      MemberExpr := TGocciaMemberExpression(AExpr.Operand);
      if MemberExpr.Computed then
        CompileIncrementComputedMember(ACtx, AExpr, MemberExpr, ADest, Op,
          NumericOp, PostNumericOp, AKeepResult)
      else
        CompileIncrementMember(ACtx, AExpr, MemberExpr, ADest, Op,
          NumericOp, PostNumericOp, AKeepResult);
      Exit;
    end;

    if AExpr.Operand is TGocciaPrivateMemberExpression then
    begin
      PrivateExpr := TGocciaPrivateMemberExpression(AExpr.Operand);
      ObjReg := ACtx.Scope.AllocateRegister;
      RegResult := ACtx.Scope.AllocateRegister;
      ACtx.CompileExpression(PrivateExpr.ObjectExpr, ObjReg);
      EmitLoadPropertyByName(ACtx, RegResult, ObjReg,
        PrivateKey(ACtx.Scope, PrivateExpr.PrivateName));
      EmitIncrementStep(ACtx, AExpr, ADest, RegResult, Op, NumericOp,
        PostNumericOp, AKeepResult);
      EmitStorePropertyByName(ACtx, ObjReg,
        PrivateKey(ACtx.Scope, PrivateExpr.PrivateName), RegResult);
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
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
      ObjReg := ACtx.Scope.AllocateRegister;
      KeyReg := ACtx.Scope.AllocateRegister;
      CondReg := ACtx.Scope.AllocateRegister;
      RegResult := ACtx.Scope.AllocateRegister;
      EndCount := 0;
      SetLength(EndJumps, ACtx.Scope.WithBindingCount);
      try
        NameIdx := ACtx.Template.AddConstantString(Ident.Name);
        EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, KeyReg, NameIdx));

        for I := ACtx.Scope.WithBindingCount - 1 downto 0 do
        begin
          EmitLoadHiddenWithObject(ACtx, ACtx.Scope.GetWithBindingName(I),
            ObjReg);
          EmitInstruction(ACtx, EncodeABC(OP_HAS_WITH_BINDING, CondReg,
            ObjReg, KeyReg));
          MissJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, CondReg);
          EmitInstruction(ACtx, EncodeABC(GetWithBindingOpcode(ACtx),
            RegResult, ObjReg, KeyReg));
          EmitIncrementStep(ACtx, AExpr, ADest, RegResult, Op, NumericOp,
            PostNumericOp, AKeepResult);
          EmitInstruction(ACtx, EncodeABC(SetWithBindingOpcode(ACtx),
            ObjReg, KeyReg, RegResult));
          EndJumps[EndCount] := EmitJumpInstruction(ACtx, OP_JUMP, 0);
          Inc(EndCount);
          PatchJumpTarget(ACtx, MissJump);
        end;

        CompileIdentifierAccessNoWith(ACtx, Ident, RegResult, False);
        EmitIncrementStep(ACtx, AExpr, ADest, RegResult, Op, NumericOp,
          PostNumericOp, AKeepResult);
        EmitBindingAssignmentFromRegister(ACtx, Ident.Name, RegResult, True);

        for I := 0 to EndCount - 1 do
          PatchJumpTarget(ACtx, EndJumps[I]);
      finally
        ACtx.Scope.FreeRegister;
        ACtx.Scope.FreeRegister;
        ACtx.Scope.FreeRegister;
        ACtx.Scope.FreeRegister;
      end;
      Exit;
    end;

    LocalIdx := ACtx.Scope.ResolveLocal(Ident.Name);
    if LocalIdx >= 0 then
    begin
      if ACtx.Scope.GetLocal(LocalIdx).IsConst then
      begin
        if ShouldIgnoreNonStrictImmutableLocalAssignment(ACtx,
           ACtx.Scope.GetLocal(LocalIdx)) then
        begin
          Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
          if ACtx.Scope.GetLocal(LocalIdx).IsCaptured then
            EmitInstruction(ACtx, EncodeABx(OP_GET_LOCAL, Slot, UInt16(Slot)));
          EmitIncrementStep(ACtx, AExpr, ADest, Slot, Op, NumericOp,
            PostNumericOp, AKeepResult);
          Exit;
        end;
        EmitConstAssignmentError(ACtx);
        Exit;
      end;
      if ACtx.Scope.GetLocal(LocalIdx).IsGlobalBacked then
      begin
        RegResult := ACtx.Scope.AllocateRegister;
        EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, RegResult,
          ACtx.Template.AddConstantString(Ident.Name)));
        EmitIncrementStep(ACtx, AExpr, ADest, RegResult, Op, NumericOp,
          PostNumericOp, AKeepResult);
        EmitSetGlobalByName(ACtx, RegResult, Ident.Name);
        EmitExportBindingUpdates(ACtx,
          ACtx.Scope.GetLocal(LocalIdx).ExportNames,
          ACtx.Scope.GetLocal(LocalIdx).ExportNameCount, RegResult);
        ACtx.Scope.FreeRegister;
        Exit;
      end;
      Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
      if ACtx.Scope.GetLocal(LocalIdx).IsCaptured then
        EmitInstruction(ACtx, EncodeABx(OP_GET_LOCAL, Slot, UInt16(Slot)));
      EmitIncrementStep(ACtx, AExpr, ADest, Slot, Op, NumericOp,
        PostNumericOp, AKeepResult);
      if ACtx.Scope.GetLocal(LocalIdx).IsCaptured then
        EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, Slot, UInt16(Slot)));
      EmitExportBindingUpdates(ACtx,
        ACtx.Scope.GetLocal(LocalIdx).ExportNames,
        ACtx.Scope.GetLocal(LocalIdx).ExportNameCount, Slot);
      Exit;
    end;

    UpvalIdx := ACtx.Scope.ResolveUpvalue(Ident.Name);
    if UpvalIdx >= 0 then
    begin
      Upvalue := ACtx.Scope.GetUpvalue(UpvalIdx);
      RegResult := ACtx.Scope.AllocateRegister;
      if Upvalue.IsGlobalBacked then
        EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, RegResult,
          ACtx.Template.AddConstantString(Ident.Name)))
      else
        EmitInstruction(ACtx, EncodeABx(OP_GET_UPVALUE, RegResult,
          UInt16(UpvalIdx)));
      EmitIncrementStep(ACtx, AExpr, ADest, RegResult, Op, NumericOp,
        PostNumericOp, AKeepResult);
      EmitCurrentUpvalueAssignment(ACtx, Upvalue, UpvalIdx, RegResult,
        RegResult, sltUntyped);
      ACtx.Scope.FreeRegister;
      Exit;
    end;

    RegResult := ACtx.Scope.AllocateRegister;
    CompileIdentifierAccessNoWith(ACtx, Ident, RegResult, False);
    EmitIncrementStep(ACtx, AExpr, ADest, RegResult, Op, NumericOp,
      PostNumericOp, AKeepResult);
    EmitSetGlobalByName(ACtx, RegResult, Ident.Name);
    ACtx.Scope.FreeRegister;
  finally
    if ReservedDestRegister then
      ACtx.Scope.FreeRegister;
  end;
end;

procedure AddOptionalChainJump(const ACtx: TGocciaCompilationContext;
  var AJumps: TGocciaCompilerJumpArray; var AJumpCount: Integer;
  const AReg: UInt16);
begin
  if AJumpCount >= Length(AJumps) then
    SetLength(AJumps, AJumpCount * 2 + 4);
  AJumps[AJumpCount] := EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH, AReg);
  Inc(AJumpCount);
end;

function ExpressionContainsOptionalChain(const AExpr: TGocciaExpression): Boolean;
var
  CallExpr: TGocciaCallExpression;
  MemberExpr: TGocciaMemberExpression;
  PrivateExpr: TGocciaPrivateMemberExpression;
  I: Integer;
begin
  if not Assigned(AExpr) then
    Exit(False);

  if AExpr is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(AExpr);
    if MemberExpr.Optional or
       ExpressionContainsOptionalChain(MemberExpr.ObjectExpr) or
       (MemberExpr.Computed and
        ExpressionContainsOptionalChain(MemberExpr.PropertyExpression)) then
      Exit(True);
  end
  else if AExpr is TGocciaPrivateMemberExpression then
  begin
    PrivateExpr := TGocciaPrivateMemberExpression(AExpr);
    if PrivateExpr.Optional or
       ExpressionContainsOptionalChain(PrivateExpr.ObjectExpr) then
      Exit(True);
  end
  else if AExpr is TGocciaCallExpression then
  begin
    CallExpr := TGocciaCallExpression(AExpr);
    if CallExpr.Optional or ExpressionContainsOptionalChain(CallExpr.Callee) then
      Exit(True);
    for I := 0 to CallExpr.Arguments.Count - 1 do
      if ExpressionContainsOptionalChain(CallExpr.Arguments[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaSequenceExpression then
  begin
    for I := 0 to TGocciaSequenceExpression(AExpr).Expressions.Count - 1 do
      if ExpressionContainsOptionalChain(
        TGocciaSequenceExpression(AExpr).Expressions[I]) then
        Exit(True);
  end;

  Result := False;
end;

procedure CompileExpressionWithOptionalChainJumps(
  const ACtx: TGocciaCompilationContext; const AExpr: TGocciaExpression;
  const ADest: UInt16; var AJumps: TGocciaCompilerJumpArray;
  var AJumpCount: Integer);
var
  CallExpr: TGocciaCallExpression;
  MemberExpr: TGocciaMemberExpression;
  PrivateExpr: TGocciaPrivateMemberExpression;
  ObjReg, IdxReg: UInt16;
begin
  if AExpr is TGocciaCallExpression then
  begin
    CallExpr := TGocciaCallExpression(AExpr);
    if CompileCallWithOptionalChainJumps(ACtx, CallExpr, ADest, AJumps,
       AJumpCount) then
      Exit;
  end;

  if AExpr is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(AExpr);
    if MemberExpr.ObjectExpr is TGocciaSuperExpression then
    begin
      CompileMember(ACtx, MemberExpr, ADest);
      if MemberExpr.Optional then
        AddOptionalChainJump(ACtx, AJumps, AJumpCount, ADest);
      Exit;
    end;

    ObjReg := ACtx.Scope.AllocateRegister;
    CompileExpressionWithOptionalChainJumps(ACtx, MemberExpr.ObjectExpr,
      ObjReg, AJumps, AJumpCount);
    if MemberExpr.Optional then
      AddOptionalChainJump(ACtx, AJumps, AJumpCount, ObjReg);

    if MemberExpr.Computed then
    begin
      IdxReg := ACtx.Scope.AllocateRegister;
      ACtx.CompileExpression(MemberExpr.PropertyExpression, IdxReg);
      EmitInstruction(ACtx, EncodeABC(OP_ARRAY_GET, ADest, ObjReg, IdxReg));
      ACtx.Scope.FreeRegister;
    end
    else
      EmitLoadPropertyByName(ACtx, ADest, ObjReg, MemberExpr.PropertyName);
    ACtx.Scope.FreeRegister;
    Exit;
  end;

  if AExpr is TGocciaPrivateMemberExpression then
  begin
    PrivateExpr := TGocciaPrivateMemberExpression(AExpr);
    ObjReg := ACtx.Scope.AllocateRegister;
    CompileExpressionWithOptionalChainJumps(ACtx, PrivateExpr.ObjectExpr,
      ObjReg, AJumps, AJumpCount);
    if PrivateExpr.Optional then
      AddOptionalChainJump(ACtx, AJumps, AJumpCount, ObjReg);
    EmitLoadPropertyByName(ACtx, ADest, ObjReg,
      PrivateKey(ACtx.Scope, PrivateExpr.PrivateName));
    ACtx.Scope.FreeRegister;
    Exit;
  end;

  ACtx.CompileExpression(AExpr, ADest);
end;

procedure CompilePrivateMember(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaPrivateMemberExpression; const ADest: UInt16);
var
  ObjReg: UInt16;
  EndJump: Integer;
  NullishJumps: TGocciaCompilerJumpArray;
  NullishJumpCount: Integer;
  I: Integer;
begin
  ObjReg := ACtx.Scope.AllocateRegister;
  NullishJumpCount := 0;
  CompileExpressionWithOptionalChainJumps(ACtx, AExpr.ObjectExpr, ObjReg,
    NullishJumps, NullishJumpCount);
  if AExpr.Optional then
    AddOptionalChainJump(ACtx, NullishJumps, NullishJumpCount, ObjReg);

  EmitLoadPropertyByName(ACtx, ADest, ObjReg,
    PrivateKey(ACtx.Scope, AExpr.PrivateName));
  if NullishJumpCount > 0 then
  begin
    EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
    for I := 0 to NullishJumpCount - 1 do
      PatchJumpTarget(ACtx, NullishJumps[I]);
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, ADest, 0, 0));
    PatchJumpTarget(ACtx, EndJump);
  end
  else
    SetLength(NullishJumps, 0);
  ACtx.Scope.FreeRegister;
end;

procedure CompilePrivatePropertyAssignment(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaPrivatePropertyAssignmentExpression; const ADest: UInt16);
var
  ObjReg, ValReg: UInt16;
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
  const ADest: UInt16);
var
  ObjReg, CurReg, ValReg: UInt16;
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
  const ADest: UInt16);
var
  LocalIdx, UpvalIdx: Integer;
  Local: TGocciaCompilerLocal;
  Slot: UInt16;
begin
  LocalIdx := ACtx.Scope.ResolveLocal(KEYWORD_THIS);
  if LocalIdx >= 0 then
  begin
    Local := ACtx.Scope.GetLocal(LocalIdx);
    Slot := Local.Slot;
    EmitDerivedThisInitializedCheck(ACtx);
    if Local.IsCaptured then
      EmitInstruction(ACtx, EncodeABx(OP_GET_LOCAL, ADest, UInt16(Slot)))
    else if Slot <> ADest then
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, ADest, Slot, 0));
    Exit;
  end;

  UpvalIdx := ACtx.Scope.ResolveUpvalue(KEYWORD_THIS);
  if UpvalIdx >= 0 then
  begin
    EmitDerivedThisInitializedCheck(ACtx);
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
  const ADest: UInt16);
var
  LocalIdx, UpvalIdx: Integer;
  Slot: UInt16;
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
  const ADest: UInt16);
begin
  EmitInstruction(ACtx, EncodeABC(OP_IMPORT_META, ADest, 0, 0));
end;

// ES2026 §13.3.12 MetaProperty — new.target
procedure CompileNewTarget(const ACtx: TGocciaCompilationContext;
  const ADest: UInt16);
begin
  EmitInstruction(ACtx, EncodeABC(OP_NEW_TARGET, ADest, 0, 0));
end;

// ES2026 §13.3.10 ImportCall — import(specifier [, options])
procedure CompileDynamicImport(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaImportCallExpression; const ADest: UInt16);
var
  SpecReg: UInt16;
  OptionsReg: UInt16;
begin
  SpecReg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AExpr.Specifier, SpecReg);
  if Assigned(AExpr.Options) then
  begin
    OptionsReg := ACtx.Scope.AllocateRegister;
    ACtx.CompileExpression(AExpr.Options, OptionsReg);
    case AExpr.Phase of
      icpEvaluation:
        EmitInstruction(ACtx, EncodeABC(OP_DYNAMIC_IMPORT_OPTIONS, ADest,
          SpecReg, OptionsReg));
      icpSource:
        EmitInstruction(ACtx, EncodeABC(OP_DYNAMIC_IMPORT_SOURCE_OPTIONS,
          ADest, SpecReg, OptionsReg));
      icpDefer:
        EmitInstruction(ACtx, EncodeABC(OP_DYNAMIC_IMPORT_DEFER_OPTIONS,
          ADest, SpecReg, OptionsReg));
    end;
    ACtx.Scope.FreeRegister;
  end;
  if not Assigned(AExpr.Options) then
    EmitInstruction(ACtx, EncodeABC(OP_DYNAMIC_IMPORT, ADest, SpecReg,
      Ord(AExpr.Phase)));
  ACtx.Scope.FreeRegister;
end;

end.
