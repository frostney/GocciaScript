unit Goccia.Evaluator;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.ControlFlow,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Evaluator.Context,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
  Goccia.Values.ClassValue,
  Goccia.Values.HoleValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  PGocciaValue = ^TGocciaValue;

function Evaluate(const ANode: TGocciaASTNode; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
function EvaluateExpression(const AExpression: TGocciaExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateStatement(const AStatement: TGocciaStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
function EvaluateStatements(const ANodes: TObjectList<TGocciaASTNode>; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
function EvaluateBinary(const ABinaryExpression: TGocciaBinaryExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateUnary(const AUnaryExpression: TGocciaUnaryExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateDelete(const AOperand: TGocciaExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateCall(const ACallExpression: TGocciaCallExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateMember(const AMemberExpression: TGocciaMemberExpression; const AContext: TGocciaEvaluationContext): TGocciaValue; overload;
function EvaluateMember(const AMemberExpression: TGocciaMemberExpression; const AContext: TGocciaEvaluationContext; out AObjectValue: TGocciaValue): TGocciaValue; overload;
function EvaluateArray(const AArrayExpression: TGocciaArrayExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateObject(const AObjectExpression: TGocciaObjectExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateGetter(const AGetterExpression: TGocciaGetterExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateSetter(const ASetterExpression: TGocciaSetterExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateArrowFunction(const AArrowFunctionExpression: TGocciaArrowFunctionExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateMethodExpression(const AMethodExpression: TGocciaMethodExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateBlock(const ABlockStatement: TGocciaBlockStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
function EvaluateIf(const AIfStatement: TGocciaIfStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
function EvaluateTry(const ATryStatement: TGocciaTryStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
function EvaluateSwitch(const ASwitchStatement: TGocciaSwitchStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
function EvaluateClassMethod(const AClassMethod: TGocciaClassMethod; const AContext: TGocciaEvaluationContext; const ASuperClass: TGocciaValue = nil): TGocciaValue;
function EvaluateClass(const AClassDeclaration: TGocciaClassDeclaration; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateClassExpression(const AClassExpression: TGocciaClassExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateClassDefinition(const AClassDef: TGocciaClassDefinition; const AContext: TGocciaEvaluationContext; const ALine, AColumn: Integer): TGocciaClassValue;
function EvaluateNewExpression(const ANewExpression: TGocciaNewExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluatePrivateMember(const APrivateMemberExpression: TGocciaPrivateMemberExpression; const AContext: TGocciaEvaluationContext): TGocciaValue; overload;
function EvaluatePrivateMember(const APrivateMemberExpression: TGocciaPrivateMemberExpression; const AContext: TGocciaEvaluationContext; out AObjectValue: TGocciaValue): TGocciaValue; overload;
function EvaluatePrivatePropertyAssignment(const APrivatePropertyAssignmentExpression: TGocciaPrivatePropertyAssignmentExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluatePrivatePropertyCompoundAssignment(const APrivatePropertyCompoundAssignmentExpression: TGocciaPrivatePropertyCompoundAssignmentExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateDestructuringAssignment(const ADestructuringAssignmentExpression: TGocciaDestructuringAssignmentExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateEnumDeclaration(const AEnumDeclaration: TGocciaEnumDeclaration; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateDestructuringDeclaration(const ADestructuringDeclaration: TGocciaDestructuringDeclaration; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateTemplateLiteral(const ATemplateLiteralExpression: TGocciaTemplateLiteralExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateTemplateWithInterpolation(const ATemplateWithInterpolationExpression: TGocciaTemplateWithInterpolationExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateTaggedTemplate(const ATaggedTemplateExpression: TGocciaTaggedTemplateExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateTemplateExpression(const AExpressionText: string; const AContext: TGocciaEvaluationContext; const ALine, AColumn: Integer): TGocciaValue;
function EvaluateAwait(const AAwaitExpression: TGocciaAwaitExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function AwaitValue(const AValue: TGocciaValue): TGocciaValue;
function EvaluateUsingDeclaration(const AUsingDeclaration: TGocciaUsingDeclaration; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
function EvaluateForOf(const AForOfStatement: TGocciaForOfStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
function EvaluateForAwaitOf(const AForAwaitOfStatement: TGocciaForAwaitOfStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;

// Destructuring pattern assignment procedures
procedure AssignPattern(const APattern: TGocciaDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
procedure AssignIdentifierPattern(const APattern: TGocciaIdentifierDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
procedure AssignArrayPattern(const APattern: TGocciaArrayDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
procedure AssignObjectPattern(const APattern: TGocciaObjectDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
procedure AssignAssignmentPattern(const APattern: TGocciaAssignmentDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
procedure AssignRestPattern(const APattern: TGocciaRestDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
procedure InitializeInstanceProperties(const AInstance: TGocciaInstanceValue; const AClassValue: TGocciaClassValue; const AContext: TGocciaEvaluationContext);
procedure InitializePrivateInstanceProperties(const AInstance: TGocciaInstanceValue; const AClassValue: TGocciaClassValue; const AContext: TGocciaEvaluationContext);
function InstantiateClass(const AClassValue: TGocciaClassValue; const AArguments: TGocciaArgumentsCollection; const AContext: TGocciaEvaluationContext): TGocciaValue;

function IsObjectInstanceOfClass(const AObj: TGocciaObjectValue; const AClassValue: TGocciaClassValue): Boolean;

implementation

uses
  Classes,
  SysUtils,

  OrderedStringMap,
  StringBuffer,

  Goccia.CallStack,
  Goccia.Constants,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Coverage,
  Goccia.DisposalTracker,
  Goccia.Error,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Evaluator.Arithmetic,
  Goccia.Evaluator.Assignment,
  Goccia.Evaluator.Bitwise,
  Goccia.Evaluator.Comparison,
  Goccia.Evaluator.Decorators,
  Goccia.Evaluator.TypeOperations,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.Keywords.Reserved,
  Goccia.Lexer,
  Goccia.MicrotaskQueue,
  Goccia.Parser,
  Goccia.StackLimit,
  Goccia.Timeout,
  Goccia.Token,
  Goccia.Values.ArrayValue,
  Goccia.Values.AsyncFunctionValue,
  Goccia.Values.BigIntValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.EnumValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.FunctionValue,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.IteratorValue,
  Goccia.Values.MapValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.PromiseValue,
  Goccia.Values.ProxyValue,
  Goccia.Values.SetValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToPrimitive;

// Helper: create a non-owning copy of a statement list (AST owns the nodes)
function CopyStatementList(const ASource: TObjectList<TGocciaASTNode>): TObjectList<TGocciaASTNode>;
var
  I: Integer;
begin
  Result := TObjectList<TGocciaASTNode>.Create(False);
  for I := 0 to ASource.Count - 1 do
    Result.Add(ASource[I]);
end;

function EvaluateStatements(const ANodes: TObjectList<TGocciaASTNode>; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
var
  I: Integer;
begin
  if Assigned(AContext.OnError) and not Assigned(AContext.Scope.OnError) then
    AContext.Scope.OnError := AContext.OnError;

  Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
  for I := 0 to ANodes.Count - 1 do
  begin
    if ANodes[I] is TGocciaExpression then
      Result := TGocciaControlFlow.Normal(EvaluateExpression(TGocciaExpression(ANodes[I]), AContext))
    else
      Result := EvaluateStatement(TGocciaStatement(ANodes[I]), AContext);
    if Result.Kind <> cfkNormal then Exit;
  end;
end;

function GetIteratorFromValue(const AValue: TGocciaValue): TGocciaIteratorValue;
var
  IteratorMethod, IteratorObj, NextMethod: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  WasAlreadyRooted: Boolean;
  GC: TGarbageCollector;
begin
  if AValue is TGocciaIteratorValue then
  begin
    Result := TGocciaIteratorValue(AValue);
    Exit;
  end;

  if AValue is TGocciaObjectValue then
  begin
    IteratorMethod := TGocciaObjectValue(AValue).GetSymbolProperty(TGocciaSymbolValue.WellKnownIterator);
    if Assigned(IteratorMethod) and not (IteratorMethod is TGocciaUndefinedLiteralValue) and IteratorMethod.IsCallable then
    begin
      GC := TGarbageCollector.Instance;
      WasAlreadyRooted := Assigned(GC) and GC.IsTempRoot(AValue);
      if Assigned(GC) and not WasAlreadyRooted then
        GC.AddTempRoot(AValue);
      try
        CallArgs := TGocciaArgumentsCollection.Create;
        try
          IteratorObj := TGocciaFunctionBase(IteratorMethod).Call(CallArgs, AValue);
        finally
          CallArgs.Free;
        end;
      finally
        if Assigned(GC) and not WasAlreadyRooted then
          GC.RemoveTempRoot(AValue);
      end;
      if IteratorObj is TGocciaIteratorValue then
      begin
        Result := TGocciaIteratorValue(IteratorObj);
        Exit;
      end;
      if IteratorObj is TGocciaObjectValue then
      begin
        NextMethod := IteratorObj.GetProperty(PROP_NEXT);
        if Assigned(NextMethod) and not (NextMethod is TGocciaUndefinedLiteralValue) and NextMethod.IsCallable then
        begin
          Result := TGocciaGenericIteratorValue.Create(IteratorObj);
          Exit;
        end;
      end;
    end;
  end;

  if AValue is TGocciaStringLiteralValue then
  begin
    Result := TGocciaStringIteratorValue.Create(AValue);
    Exit;
  end;

  Result := nil;
end;

procedure SpreadIterableInto(const ASpreadValue: TGocciaValue; const ATarget: TGocciaValueList);
var
  SpreadArray: TGocciaArrayValue;
  Iterator: TGocciaIteratorValue;
  IterResult: TGocciaObjectValue;
  J: Integer;
begin
  if ASpreadValue is TGocciaArrayValue then
  begin
    SpreadArray := TGocciaArrayValue(ASpreadValue);
    for J := 0 to SpreadArray.Elements.Count - 1 do
    begin
      if SpreadArray.Elements[J] = TGocciaHoleValue.HoleValue then
        ATarget.Add(TGocciaUndefinedLiteralValue.UndefinedValue)
      else
        ATarget.Add(SpreadArray.Elements[J]);
    end;
  end
  else
  begin
    Iterator := GetIteratorFromValue(ASpreadValue);
    if Assigned(Iterator) then
    begin
      TGarbageCollector.Instance.AddTempRoot(Iterator);
      try
        IterResult := Iterator.AdvanceNext;
        while not IterResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value do
        begin
          ATarget.Add(IterResult.GetProperty(PROP_VALUE));
          IterResult := Iterator.AdvanceNext;
        end;
      finally
        TGarbageCollector.Instance.RemoveTempRoot(Iterator);
      end;
    end
    else
      ThrowTypeError(
        SErrorSpreadRequiresIterable,
        SSuggestSpreadRequiresIterable);
  end;
end;

procedure SpreadIterableIntoArgs(const ASpreadValue: TGocciaValue; const AArgs: TGocciaArgumentsCollection);
begin
  SpreadIterableInto(ASpreadValue, AArgs.Items);
end;

function Evaluate(const ANode: TGocciaASTNode; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
begin
  // Propagate OnError onto the scope so closures inherit it
  if Assigned(AContext.OnError) and not Assigned(AContext.Scope.OnError) then
    AContext.Scope.OnError := AContext.OnError;

  if ANode is TGocciaExpression then
    Result := TGocciaControlFlow.Normal(EvaluateExpression(TGocciaExpression(ANode), AContext))
  else if ANode is TGocciaStatement then
    Result := EvaluateStatement(TGocciaStatement(ANode), AContext)
  else
    Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

function EvaluateExpression(const AExpression: TGocciaExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  if AContext.CoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
    TGocciaCoverageTracker.Instance.RecordLineHit(
      AContext.CurrentFilePath, AExpression.Line);
  Result := AExpression.Evaluate(AContext);
end;

function EvaluateStatement(const AStatement: TGocciaStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
begin
  if AContext.CoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
    TGocciaCoverageTracker.Instance.RecordLineHit(
      AContext.CurrentFilePath, AStatement.Line);
  Result := AStatement.Execute(AContext);
end;

function EvaluateBinary(const ABinaryExpression: TGocciaBinaryExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Left, Right: TGocciaValue;
begin
  // Handle short-circuiting logical operators first
  if ABinaryExpression.Operator = gttAnd then
  begin
    Left := EvaluateExpression(ABinaryExpression.Left, AContext);
    if not Left.ToBooleanLiteral.Value then
    begin
      if AContext.CoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
        TGocciaCoverageTracker.Instance.RecordBranchHit(
          AContext.CurrentFilePath, ABinaryExpression.Line,
          ABinaryExpression.Column, 0);
      Result := Left  // Short-circuit: return left operand if falsy
    end
    else
    begin
      if AContext.CoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
        TGocciaCoverageTracker.Instance.RecordBranchHit(
          AContext.CurrentFilePath, ABinaryExpression.Line,
          ABinaryExpression.Column, 1);
      Right := EvaluateExpression(ABinaryExpression.Right, AContext);
      Result := Right;  // Return right operand
    end;
    Exit;
  end
  else if ABinaryExpression.Operator = gttOr then
  begin
    Left := EvaluateExpression(ABinaryExpression.Left, AContext);
    if Left.ToBooleanLiteral.Value then
    begin
      if AContext.CoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
        TGocciaCoverageTracker.Instance.RecordBranchHit(
          AContext.CurrentFilePath, ABinaryExpression.Line,
          ABinaryExpression.Column, 0);
      Result := Left  // Short-circuit: return left operand if truthy
    end
    else
    begin
      if AContext.CoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
        TGocciaCoverageTracker.Instance.RecordBranchHit(
          AContext.CurrentFilePath, ABinaryExpression.Line,
          ABinaryExpression.Column, 1);
      Right := EvaluateExpression(ABinaryExpression.Right, AContext);
      Result := Right;  // Return right operand
    end;
    Exit;
  end
  else if ABinaryExpression.Operator = gttNullishCoalescing then
  begin
    Left := EvaluateExpression(ABinaryExpression.Left, AContext);
    // Return right operand only if left is null or undefined
    if (Left is TGocciaNullLiteralValue) or (Left is TGocciaUndefinedLiteralValue) then
    begin
      if AContext.CoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
        TGocciaCoverageTracker.Instance.RecordBranchHit(
          AContext.CurrentFilePath, ABinaryExpression.Line,
          ABinaryExpression.Column, 0);
      Right := EvaluateExpression(ABinaryExpression.Right, AContext);
      Result := Right;
    end
    else
    begin
      if AContext.CoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
        TGocciaCoverageTracker.Instance.RecordBranchHit(
          AContext.CurrentFilePath, ABinaryExpression.Line,
          ABinaryExpression.Column, 1);
      Result := Left;  // Return left operand for all other values (including falsy ones)
    end;
    Exit;
  end;

  // For all other operators, evaluate both operands
  Left := EvaluateExpression(ABinaryExpression.Left, AContext);
  Right := EvaluateExpression(ABinaryExpression.Right, AContext);

  case ABinaryExpression.Operator of
    gttPlus:
      Result := EvaluateAddition(Left, Right);
    gttMinus:
      Result := EvaluateSubtraction(Left, Right);
    gttStar:
      Result := EvaluateMultiplication(Left, Right);
    gttSlash:
      Result := EvaluateDivision(Left, Right);
    gttPercent:
      Result := EvaluateModulo(Left, Right);
    gttPower:
      Result := EvaluateExponentiation(Left, Right);
    gttEqual:
      if IsStrictEqual(Left, Right) then Result := TGocciaBooleanLiteralValue.TrueValue else Result := TGocciaBooleanLiteralValue.FalseValue;
    gttNotEqual:
      if IsNotStrictEqual(Left, Right) then Result := TGocciaBooleanLiteralValue.TrueValue else Result := TGocciaBooleanLiteralValue.FalseValue;
    gttLess:
      if LessThan(Left, Right) then Result := TGocciaBooleanLiteralValue.TrueValue else Result := TGocciaBooleanLiteralValue.FalseValue;
    gttGreater:
      if GreaterThan(Left, Right) then Result := TGocciaBooleanLiteralValue.TrueValue else Result := TGocciaBooleanLiteralValue.FalseValue;
    gttLessEqual:
      if LessThanOrEqual(Left, Right) then Result := TGocciaBooleanLiteralValue.TrueValue else Result := TGocciaBooleanLiteralValue.FalseValue;
    gttGreaterEqual:
      if GreaterThanOrEqual(Left, Right) then Result := TGocciaBooleanLiteralValue.TrueValue else Result := TGocciaBooleanLiteralValue.FalseValue;
    gttInstanceof:
      Result := EvaluateInstanceof(Left, Right, IsObjectInstanceOfClass);
    gttIn:
      Result := EvaluateInOperator(Left, Right);
    // Bitwise operators
    gttBitwiseAnd:
      Result := EvaluateBitwiseAnd(Left, Right);
    gttBitwiseOr:
      Result := EvaluateBitwiseOr(Left, Right);
    gttBitwiseXor:
      Result := EvaluateBitwiseXor(Left, Right);
    gttLeftShift:
      Result := EvaluateLeftShift(Left, Right);
    gttRightShift:
      Result := EvaluateRightShift(Left, Right);
    gttUnsignedRightShift:
      Result := EvaluateUnsignedRightShift(Left, Right);
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function EvaluateUnary(const AUnaryExpression: TGocciaUnaryExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Operand: TGocciaValue;
begin
  // Special handling for delete operator
  if AUnaryExpression.Operator = gttDelete then
  begin
    Result := EvaluateDelete(AUnaryExpression.Operand, AContext);
    Exit;
  end;

  // Special handling for typeof: must not throw for undeclared variables
  if AUnaryExpression.Operator = gttTypeof then
  begin
    try
      Operand := EvaluateExpression(AUnaryExpression.Operand, AContext);
    except
      on E: TGocciaReferenceError do
      begin
        // typeof on undeclared variable returns "undefined" per ECMAScript spec
        Result := TGocciaStringLiteralValue.Create('undefined');
        Exit;
      end;
    end;
    Result := EvaluateTypeof(Operand);
    Exit;
  end;

  Operand := EvaluateExpression(AUnaryExpression.Operand, AContext);

  case AUnaryExpression.Operator of
    gttNot:
      if Operand.ToBooleanLiteral.Value then Result := TGocciaBooleanLiteralValue.FalseValue else Result := TGocciaBooleanLiteralValue.TrueValue;
    gttMinus:
      begin
        if Operand is TGocciaSymbolValue then
          ThrowTypeError(SErrorSymbolToNumber, SSuggestSymbolNoImplicitConversion);
        // ES2026 §6.1.6.2.1 BigInt::unaryMinus
        if Operand is TGocciaBigIntValue then
        begin
          Result := TGocciaBigIntValue.Create(
            TGocciaBigIntValue(Operand).Value.Negate);
        end
        else if (Operand is TGocciaNumberLiteralValue) then
        begin
          if TGocciaNumberLiteralValue(Operand).IsInfinity then
            Result := TGocciaNumberLiteralValue.NegativeInfinityValue  // -Infinity = -Infinity
          else if TGocciaNumberLiteralValue(Operand).IsNegativeInfinity then
            Result := TGocciaNumberLiteralValue.InfinityValue  // -(-Infinity) = Infinity
          // Handle signed zero: -0 should create negative zero, -(negative zero) should create positive zero
          else if TGocciaNumberLiteralValue(Operand).Value = 0 then
          begin
            if TGocciaNumberLiteralValue(Operand).IsNegativeZero then
              Result := TGocciaNumberLiteralValue.ZeroValue  // -(-0) = +0
            else
              Result := TGocciaNumberLiteralValue.NegativeZeroValue;  // -0 = -0
          end
          else
            Result := TGocciaNumberLiteralValue.Create(-Operand.ToNumberLiteral.Value);
        end
        else
          Result := TGocciaNumberLiteralValue.Create(-Operand.ToNumberLiteral.Value);
      end;
    gttPlus:
    begin
      if Operand is TGocciaSymbolValue then
        ThrowTypeError(SErrorSymbolToNumber, SSuggestSymbolNoImplicitConversion);
      // ES2026 §7.1.4: unary + on BigInt throws TypeError
      if Operand is TGocciaBigIntValue then
        ThrowTypeError(SErrorBigIntUnaryPlus, SSuggestBigIntNoImplicitConversion);
      Result := Operand.ToNumberLiteral;
    end;
    gttTypeof:
      Result := EvaluateTypeof(Operand);
    gttBitwiseNot:
      Result := EvaluateBitwiseNot(Operand);
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function EvaluateCall(const ACallExpression: TGocciaCallExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Callee: TGocciaValue;
  Arguments: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue;
  ArgumentExpr: TGocciaExpression;
  SuperClass: TGocciaClassValue;
  MemberExpr: TGocciaMemberExpression;
  SpreadValue: TGocciaValue;
  CalleeName: string;
  I: Integer;
begin
  CheckExecutionTimeout;
  IncrementInstructionCounter;
  CheckInstructionLimit;
  // Handle super() calls specially
  if ACallExpression.Callee is TGocciaSuperExpression then
  begin
    SuperClass := TGocciaClassValue(EvaluateExpression(ACallExpression.Callee, AContext));
    if not (SuperClass is TGocciaClassValue) then
    begin
      AContext.OnError('super() can only be called within a method with a superclass',
        ACallExpression.Line, ACallExpression.Column);
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;

    Arguments := TGocciaArgumentsCollection.Create;

    try
      for ArgumentExpr in ACallExpression.Arguments do
      begin
        if ArgumentExpr is TGocciaSpreadExpression then
        begin
          SpreadValue := EvaluateExpression(TGocciaSpreadExpression(ArgumentExpr).Argument, AContext);
          SpreadIterableIntoArgs(SpreadValue, Arguments);
        end
        else
          Arguments.Add(EvaluateExpression(ArgumentExpr, AContext));
      end;

      if Assigned(SuperClass.ConstructorMethod) then
      begin
        Result := SuperClass.ConstructorMethod.Call(Arguments, AContext.Scope.ThisValue);
      end
      else
      begin
        if AContext.Scope.ThisValue is TGocciaInstanceValue then
          TGocciaInstanceValue(AContext.Scope.ThisValue).InitializeNativeFromArguments(Arguments);
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      end;
    finally
      Arguments.Free;
    end;
    Exit;
  end;

  // Handle method calls vs regular function calls
  if ACallExpression.Callee is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(ACallExpression.Callee);
    if MemberExpr.ObjectExpr is TGocciaSuperExpression then
    begin
      // Super method calls: evaluate normally
      Callee := EvaluateExpression(ACallExpression.Callee, AContext);
      ThisValue := AContext.Scope.ThisValue;  // Use current instance's 'this'
    end
    else
    begin
      // Regular method calls: use overloaded function to get both method and object
      Callee := EvaluateMember(MemberExpr, AContext, ThisValue);
      if MemberExpr.Optional and
         ((ThisValue is TGocciaNullLiteralValue) or (ThisValue is TGocciaUndefinedLiteralValue)) then
      begin
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
        Exit;
      end;
    end;
  end
  else if ACallExpression.Callee is TGocciaPrivateMemberExpression then
  begin
    // Private method calls: use overloaded function to get both method and object
    Callee := EvaluatePrivateMember(TGocciaPrivateMemberExpression(ACallExpression.Callee), AContext, ThisValue);
  end
  else
  begin
    // Regular function calls
    Callee := EvaluateExpression(ACallExpression.Callee, AContext);
    ThisValue := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;

  Arguments := TGocciaArgumentsCollection.Create;
  try
    for ArgumentExpr in ACallExpression.Arguments do
    begin
      if ArgumentExpr is TGocciaSpreadExpression then
      begin
        SpreadValue := EvaluateExpression(TGocciaSpreadExpression(ArgumentExpr).Argument, AContext);
        SpreadIterableIntoArgs(SpreadValue, Arguments);
      end
      else
      begin
        Arguments.Add(EvaluateExpression(ArgumentExpr, AContext));
      end;
    end;

    if Callee is TGocciaNativeFunctionValue then
      CalleeName := TGocciaNativeFunctionValue(Callee).Name
    else if Callee is TGocciaFunctionValue then
      CalleeName := TGocciaFunctionValue(Callee).Name
    else if Callee is TGocciaClassValue then
      CalleeName := TGocciaClassValue(Callee).Name
    else
      CalleeName := '';

    if Assigned(TGocciaCallStack.Instance) then
    begin
      TGocciaCallStack.Instance.Push(CalleeName, AContext.CurrentFilePath,
        ACallExpression.Line, ACallExpression.Column);
      CheckStackDepth(TGocciaCallStack.Instance.Count);
    end;
    try
      if Callee is TGocciaProxyValue then
        Result := TGocciaProxyValue(Callee).ApplyTrap(Arguments, ThisValue)
      else if Callee is TGocciaNativeFunctionValue then
        Result := TGocciaNativeFunctionValue(Callee).Call(Arguments, ThisValue)
      else if Callee is TGocciaFunctionValue then
        Result := TGocciaFunctionValue(Callee).Call(Arguments, ThisValue)
      else if Callee is TGocciaBoundFunctionValue then
        Result := TGocciaBoundFunctionValue(Callee).Call(Arguments, ThisValue)
      else if Callee is TGocciaClassValue then
        Result := TGocciaClassValue(Callee).Call(Arguments, ThisValue)
      else if Callee is TGocciaFunctionBase then
        Result := TGocciaFunctionBase(Callee).Call(Arguments, ThisValue)
      else
      begin
        MemberExpr := nil;
        if ACallExpression.Callee is TGocciaMemberExpression then
          MemberExpr := TGocciaMemberExpression(ACallExpression.Callee);

        if Assigned(MemberExpr) and (MemberExpr.ObjectExpr is TGocciaIdentifierExpression) then
          ThrowTypeError(
            Format(SErrorMemberNotFunction,
              [TGocciaIdentifierExpression(MemberExpr.ObjectExpr).Name,
               MemberExpr.PropertyName]),
            Format('''%s'' is of type ''%s'' which does not have method ''%s''',
              [TGocciaIdentifierExpression(MemberExpr.ObjectExpr).Name,
               ThisValue.TypeName,
               MemberExpr.PropertyName]))
        else if Assigned(MemberExpr) then
          ThrowTypeError(
            Format(SErrorMemberNotFunction,
              [ThisValue.TypeName, MemberExpr.PropertyName]),
            Format('''%s'' is of type ''%s'' which does not have method ''%s''',
              [ThisValue.TypeName, ThisValue.TypeName, MemberExpr.PropertyName]))
        else if ACallExpression.Callee is TGocciaIdentifierExpression then
          ThrowTypeError(
            Format(SErrorNotFunction,
              [TGocciaIdentifierExpression(ACallExpression.Callee).Name]),
            Format('''%s'' is of type ''%s'' and cannot be called as a function',
              [TGocciaIdentifierExpression(ACallExpression.Callee).Name,
               Callee.TypeName]))
        else
          ThrowTypeError(Format(SErrorValueNotFunction, [Callee.TypeName]),
            SSuggestNotFunctionType);
      end;
    finally
      if Assigned(TGocciaCallStack.Instance) then
        TGocciaCallStack.Instance.Pop;
    end;

  finally
    Arguments.Free;
  end;
end;

function EvaluateMemberCore(const AMemberExpression: TGocciaMemberExpression; const AContext: TGocciaEvaluationContext; const AOutObjectValue: PGocciaValue): TGocciaValue; forward;

function EvaluateMember(const AMemberExpression: TGocciaMemberExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateMemberCore(AMemberExpression, AContext, nil);
end;

function EvaluateMember(const AMemberExpression: TGocciaMemberExpression; const AContext: TGocciaEvaluationContext; out AObjectValue: TGocciaValue): TGocciaValue;
begin
  Result := EvaluateMemberCore(AMemberExpression, AContext, @AObjectValue);
end;

function EvaluateMemberCore(const AMemberExpression: TGocciaMemberExpression; const AContext: TGocciaEvaluationContext; const AOutObjectValue: PGocciaValue): TGocciaValue;
var
  Obj: TGocciaValue;
  PropertyName: string;
  PropertyValue: TGocciaValue;
  SuperClass: TGocciaClassValue;
  BoxedValue: TGocciaObjectValue;
  ObjectEvaluated: Boolean;
begin
  ObjectEvaluated := False;

  // Handle optional chaining: obj?.prop returns undefined if obj is null/undefined
  if AMemberExpression.Optional then
  begin
    Obj := EvaluateExpression(AMemberExpression.ObjectExpr, AContext);
    ObjectEvaluated := True;
    if Assigned(AOutObjectValue) then
      AOutObjectValue^ := Obj;
    if (Obj is TGocciaNullLiteralValue) or (Obj is TGocciaUndefinedLiteralValue) then
    begin
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;
  end;

  // Handle super.method() specially
  if AMemberExpression.ObjectExpr is TGocciaSuperExpression then
  begin
    SuperClass := TGocciaClassValue(EvaluateExpression(AMemberExpression.ObjectExpr, AContext));
    if not (SuperClass is TGocciaClassValue) then
    begin
      AContext.OnError('super can only be used within a method with a superclass',
        AMemberExpression.Line, AMemberExpression.Column);
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      if Assigned(AOutObjectValue) then
        AOutObjectValue^ := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;

    if Assigned(AOutObjectValue) then
      AOutObjectValue^ := SuperClass;

    // Get the property name
    if AMemberExpression.Computed and Assigned(AMemberExpression.PropertyExpression) then
    begin
      PropertyValue := EvaluateExpression(AMemberExpression.PropertyExpression, AContext);
      PropertyName := PropertyValue.ToStringLiteral.Value;
    end
    else
    begin
      PropertyName := AMemberExpression.PropertyName;
    end;

    // Check if we're in a static method context or instance method context
    if AContext.Scope.ThisValue is TGocciaClassValue then
    begin
      // Static method context: look for static methods using GetProperty
      Result := SuperClass.GetProperty(PropertyName);
    end
    else
    begin
      // Instance method context: look for instance methods using GetMethod
      Result := SuperClass.GetMethod(PropertyName);
      if not Assigned(Result) then
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    end;

    Exit;
  end;

  if not ObjectEvaluated then
  begin
    Obj := EvaluateExpression(AMemberExpression.ObjectExpr, AContext);
    if Assigned(AOutObjectValue) then
      AOutObjectValue^ := Obj;
  end;

  // Determine the property name
  if AMemberExpression.Computed and Assigned(AMemberExpression.PropertyExpression) then
  begin
    // Computed access: evaluate the property expression to get the property name
    PropertyValue := EvaluateExpression(AMemberExpression.PropertyExpression, AContext);

    // Symbol property access
    if PropertyValue is TGocciaSymbolValue then
    begin
      if (Obj is TGocciaNullLiteralValue) or (Obj is TGocciaUndefinedLiteralValue) then
        ThrowTypeError(Format(SErrorCannotReadPropertiesOf, [Obj.ToStringLiteral.Value, 'Symbol()']),
          SSuggestCheckNullBeforeAccess);

      if Obj is TGocciaClassValue then
      begin
        Result := TGocciaClassValue(Obj).GetSymbolProperty(TGocciaSymbolValue(PropertyValue));
        Exit;
      end
      else if Obj is TGocciaObjectValue then
      begin
        Result := TGocciaObjectValue(Obj).GetSymbolProperty(TGocciaSymbolValue(PropertyValue));
        Exit;
      end
      else
      begin
        BoxedValue := Obj.Box;
        if Assigned(BoxedValue) then
        begin
          Result := BoxedValue.GetSymbolProperty(TGocciaSymbolValue(PropertyValue));
          Exit;
        end;
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
        Exit;
      end;
    end;

    PropertyName := PropertyValue.ToStringLiteral.Value;
  end
  else
  begin
    // Static access: use the property name directly
    PropertyName := AMemberExpression.PropertyName;
  end;

  Result := Obj.GetProperty(PropertyName);
  if Result = nil then
  begin
    // Handle primitive boxing for property access
    BoxedValue := Obj.Box;
    if Assigned(BoxedValue) then
    begin
      Result := BoxedValue.GetProperty(PropertyName);
    end
    else if (Obj is TGocciaNullLiteralValue) or (Obj is TGocciaUndefinedLiteralValue) then
    begin
      if AMemberExpression.ObjectExpr is TGocciaMemberExpression then
        ThrowTypeError(
          Format(SErrorCannotReadPropertyOf,
            [PropertyName, Obj.ToStringLiteral.Value]),
          Format('''%s'' evaluated to %s and does not have property ''%s''',
            [TGocciaMemberExpression(AMemberExpression.ObjectExpr).PropertyName,
             Obj.ToStringLiteral.Value, PropertyName]))
      else if AMemberExpression.ObjectExpr is TGocciaIdentifierExpression then
        ThrowTypeError(
          Format(SErrorCannotReadPropertyOf,
            [PropertyName, Obj.ToStringLiteral.Value]),
          Format('''%s'' is %s and does not have property ''%s''',
            [TGocciaIdentifierExpression(AMemberExpression.ObjectExpr).Name,
             Obj.ToStringLiteral.Value, PropertyName]))
      else
        ThrowTypeError(Format(
          SErrorCannotReadPropertyOf,
          [PropertyName, Obj.ToStringLiteral.Value]),
          SSuggestCheckNullBeforeAccess);
    end
    else
    begin
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    end;
  end;
end;

function EvaluateArray(const AArrayExpression: TGocciaArrayExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  I: Integer;
  ElementValue: TGocciaValue;
  SpreadValue: TGocciaValue;
begin
  Arr := TGocciaArrayValue.Create;
  TGarbageCollector.Instance.AddTempRoot(Arr);
  try
    for I := 0 to AArrayExpression.Elements.Count - 1 do
    begin
      if AArrayExpression.Elements[I] is TGocciaHoleExpression then
        Arr.Elements.Add(TGocciaHoleValue.HoleValue)
      else if AArrayExpression.Elements[I] is TGocciaSpreadExpression then
      begin
        SpreadValue := EvaluateExpression(TGocciaSpreadExpression(AArrayExpression.Elements[I]).Argument, AContext);
        SpreadIterableInto(SpreadValue, Arr.Elements);
      end
      else
      begin
        ElementValue := EvaluateExpression(AArrayExpression.Elements[I], AContext);
        Arr.Elements.Add(ElementValue);
      end;
    end;
    Result := Arr;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(Arr);
  end;
end;

function EvaluateObject(const AObjectExpression: TGocciaObjectExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  ComputedPair: TPair<TGocciaExpression, TGocciaExpression>;
  ComputedKey: string;
  SpreadValue: TGocciaValue;
  SpreadObj: TGocciaObjectValue;
  SpreadArray: TGocciaArrayValue;
  Key: string;
  I, J: Integer;
  GetterFunction: TGocciaValue;
  SetterFunction: TGocciaValue;
  PropertyName: string;
  PropertyExpression: TGocciaExpression;
  PropertyValue: TGocciaValue;
  ExistingDescriptor: TGocciaPropertyDescriptor;
  ExistingSetter: TGocciaValue;
  ExistingGetter: TGocciaValue;
  SymbolEntry: TPair<TGocciaSymbolValue, TGocciaValue>;
begin
  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AddTempRoot(Obj);

  try
  // Process all properties in source order
  for I := 0 to High(AObjectExpression.PropertySourceOrder) do
    begin
      case AObjectExpression.PropertySourceOrder[I].PropertyType of
        pstStatic:
          begin
            // Static property: {key: value}
            PropertyName := AObjectExpression.PropertySourceOrder[I].StaticKey;
            if AObjectExpression.Properties.TryGetValue(PropertyName, PropertyExpression) then
              Obj.DefineProperty(PropertyName, TGocciaPropertyDescriptorData.Create(EvaluateExpression(PropertyExpression, AContext), [pfEnumerable, pfConfigurable, pfWritable]));
          end;

        pstComputed:
          begin
            // Computed property or spread: {[expr]: value} or {...obj}
            ComputedPair := AObjectExpression.ComputedPropertiesInOrder[AObjectExpression.PropertySourceOrder[I].ComputedIndex];

            if ComputedPair.Key is TGocciaSpreadExpression then
            begin
              // Spread expression: copy all enumerable properties
              SpreadValue := EvaluateExpression(TGocciaSpreadExpression(ComputedPair.Key).Argument, AContext);

              if SpreadValue is TGocciaObjectValue then
              begin
                // Spread object string properties
                SpreadObj := TGocciaObjectValue(SpreadValue);
                for Key in SpreadObj.GetEnumerablePropertyNames do
                  Obj.DefineProperty(Key, TGocciaPropertyDescriptorData.Create(SpreadObj.GetProperty(Key), [pfEnumerable, pfConfigurable, pfWritable]));
                for SymbolEntry in SpreadObj.GetEnumerableSymbolProperties do
                  Obj.DefineSymbolProperty(SymbolEntry.Key, TGocciaPropertyDescriptorData.Create(SymbolEntry.Value, [pfEnumerable, pfConfigurable, pfWritable]));
              end
                             else if SpreadValue is TGocciaArrayValue then
               begin
                 // Spread array as indexed properties
                 SpreadArray := TGocciaArrayValue(SpreadValue);
                 for J := 0 to SpreadArray.Elements.Count - 1 do
                 begin
                   if SpreadArray.Elements[J] <> TGocciaHoleValue.HoleValue then
                     Obj.DefineProperty(IntToStr(J), TGocciaPropertyDescriptorData.Create(SpreadArray.Elements[J], [pfEnumerable, pfConfigurable, pfWritable]));
                 end;
               end
               else if SpreadValue is TGocciaStringLiteralValue then
               begin
                 // Spread string as indexed properties
                 for J := 1 to Length(SpreadValue.ToStringLiteral.Value) do
                   Obj.DefineProperty(IntToStr(J - 1), TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create(SpreadValue.ToStringLiteral.Value[J]), [pfEnumerable, pfConfigurable, pfWritable]));
               end;
              // null/undefined/other primitives are ignored in spread context
            end
            else
            begin
              // Regular computed property: {[expr]: value}
              PropertyValue := EvaluateExpression(ComputedPair.Key, AContext);
              if PropertyValue is TGocciaSymbolValue then
                Obj.DefineSymbolProperty(TGocciaSymbolValue(PropertyValue), TGocciaPropertyDescriptorData.Create(EvaluateExpression(ComputedPair.Value, AContext), [pfEnumerable, pfConfigurable, pfWritable]))
              else
              begin
                ComputedKey := PropertyValue.ToStringLiteral.Value;
                Obj.DefineProperty(ComputedKey, TGocciaPropertyDescriptorData.Create(EvaluateExpression(ComputedPair.Value, AContext), [pfEnumerable, pfConfigurable, pfWritable]));
              end;
            end;
          end;

        pstGetter:
          begin
            // Getter: {get prop() {...}}
            GetterFunction := EvaluateGetter(AObjectExpression.Getters[AObjectExpression.PropertySourceOrder[I].StaticKey], AContext);
            // Check if there's already a setter for this property
            ExistingDescriptor := Obj.GetOwnPropertyDescriptor(AObjectExpression.PropertySourceOrder[I].StaticKey);
            if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
               Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter) then
            begin
              // Merge with existing setter
              ExistingSetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter;
              Obj.DefineProperty(AObjectExpression.PropertySourceOrder[I].StaticKey, TGocciaPropertyDescriptorAccessor.Create(GetterFunction, ExistingSetter, [pfEnumerable, pfConfigurable]));
            end
            else
            begin
              // No existing setter, create getter-only descriptor
              Obj.DefineProperty(AObjectExpression.PropertySourceOrder[I].StaticKey, TGocciaPropertyDescriptorAccessor.Create(GetterFunction, nil, [pfEnumerable, pfConfigurable]));
            end;
          end;

        pstSetter:
          begin
            // Setter: {set prop(val) {...}}
            SetterFunction := EvaluateSetter(AObjectExpression.Setters[AObjectExpression.PropertySourceOrder[I].StaticKey], AContext);
            // Check if there's already a getter for this property
            ExistingDescriptor := Obj.GetOwnPropertyDescriptor(AObjectExpression.PropertySourceOrder[I].StaticKey);
            if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
               Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter) then
            begin
              // Merge with existing getter
              ExistingGetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter;
              Obj.DefineProperty(AObjectExpression.PropertySourceOrder[I].StaticKey, TGocciaPropertyDescriptorAccessor.Create(ExistingGetter, SetterFunction, [pfEnumerable, pfConfigurable]));
            end
            else
            begin
              // No existing getter, create setter-only descriptor
              Obj.DefineProperty(AObjectExpression.PropertySourceOrder[I].StaticKey, TGocciaPropertyDescriptorAccessor.Create(nil, SetterFunction, [pfEnumerable, pfConfigurable]));
            end;
          end;
      end;
    end;

  Result := Obj;
  finally
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveTempRoot(Obj);
  end;
end;

function EvaluateGetter(const AGetterExpression: TGocciaGetterExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Statements: TObjectList<TGocciaASTNode>;
  EmptyParameters: TGocciaParameterArray;
begin
  // Getter has no parameters
  SetLength(EmptyParameters, 0);

  Statements := CopyStatementList(TGocciaBlockStatement(AGetterExpression.Body).Nodes);

  // Create function with closure scope
  Result := TGocciaFunctionValue.Create(EmptyParameters, Statements, AContext.Scope.CreateChild);
  TGocciaFunctionValue(Result).SourceFilePath := AContext.CurrentFilePath;
  TGocciaFunctionValue(Result).SourceLine := AGetterExpression.Line;
end;

function EvaluateSetter(const ASetterExpression: TGocciaSetterExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Statements: TObjectList<TGocciaASTNode>;
  Parameters: TGocciaParameterArray;
begin
  // Setter has one parameter
  SetLength(Parameters, 1);
  Parameters[0].Name := ASetterExpression.Parameter;
  Parameters[0].DefaultValue := nil;

  Statements := CopyStatementList(TGocciaBlockStatement(ASetterExpression.Body).Nodes);

  // Create function with closure scope
  Result := TGocciaFunctionValue.Create(Parameters, Statements, AContext.Scope.CreateChild);
  TGocciaFunctionValue(Result).SourceFilePath := AContext.CurrentFilePath;
  TGocciaFunctionValue(Result).SourceLine := ASetterExpression.Line;
end;

// ES2026 §27.7.5.3 Await(value)
function AwaitValue(const AValue: TGocciaValue): TGocciaValue;
var
  ThenMethod: TGocciaValue;
  Promise: TGocciaPromiseValue;
  ThenArgs: TGocciaArgumentsCollection;
begin
  if AValue is TGocciaPromiseValue then
  begin
    Promise := TGocciaPromiseValue(AValue);
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AddTempRoot(Promise);
  end
  // ES2026 §25.6.4.6.1 PromiseResolve: If value is a thenable (object with
  // callable .then), wrap it in a Promise via resolve
  else if AValue is TGocciaObjectValue then
  begin
    ThenMethod := AValue.GetProperty(PROP_THEN);
    if Assigned(ThenMethod) and not (ThenMethod is TGocciaUndefinedLiteralValue) and ThenMethod.IsCallable then
    begin
      Promise := TGocciaPromiseValue.Create;
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.AddTempRoot(Promise);
      ThenArgs := TGocciaArgumentsCollection.Create([
        TGocciaNativeFunctionValue.Create(Promise.DoResolve, 'resolve', 1),
        TGocciaNativeFunctionValue.Create(Promise.DoReject, 'reject', 1)
      ]);
      try
        // ES2026 §25.6.4.6.1 step 9: If Call(then, ...) throws, reject promise
        try
          TGocciaFunctionBase(ThenMethod).Call(ThenArgs, AValue);
        except
          on E: TGocciaThrowValue do
            Promise.Reject(E.Value);
        end;
      finally
        ThenArgs.Free;
      end;
    end
    else
    begin
      // ES2026 §27.7.5.3 step 2: Await wraps the value in Promise.resolve(),
      // introducing a microtask boundary even for non-thenable objects
      if Assigned(TGocciaMicrotaskQueue.Instance) then
        TGocciaMicrotaskQueue.Instance.DrainQueue;
      Result := AValue;
      Exit;
    end;
  end
  else
  begin
    // ES2026 §27.7.5.3 step 2: Await wraps the value in Promise.resolve(),
    // introducing a microtask boundary even for primitive values
    if Assigned(TGocciaMicrotaskQueue.Instance) then
      TGocciaMicrotaskQueue.Instance.DrainQueue;
    Result := AValue;
    Exit;
  end;

  try
    if Promise.State <> gpsPending then
    begin
      // ES2026 §27.7.5.3 step 3-4: Even already-settled promises introduce
      // a microtask boundary (the continuation is a PromiseReactionJob)
      if Assigned(TGocciaMicrotaskQueue.Instance) then
        TGocciaMicrotaskQueue.Instance.DrainQueue;
      if Promise.State = gpsFulfilled then
        Result := Promise.PromiseResult
      else
        raise TGocciaThrowValue.Create(Promise.PromiseResult);
      Exit;
    end;

    if Assigned(TGocciaMicrotaskQueue.Instance) then
      TGocciaMicrotaskQueue.Instance.DrainQueue;

    if Promise.State = gpsFulfilled then
      Result := Promise.PromiseResult
    else if Promise.State = gpsRejected then
      raise TGocciaThrowValue.Create(Promise.PromiseResult)
    else
      ThrowTypeError(SErrorAwaitPromiseUnsettled, SSuggestAwaitMicrotaskDrain);
  finally
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveTempRoot(Promise);
  end;
end;

// ES2026 §27.7.5.3 Await(value)
function EvaluateAwait(const AAwaitExpression: TGocciaAwaitExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := AwaitValue(EvaluateExpression(AAwaitExpression.Operand, AContext));
end;

// ES2026 §14.7.5.6 ForIn/OfBodyEvaluation(lhs, stmt, iteratorRecord, iterationKind, lhsKind)
function EvaluateForOf(const AForOfStatement: TGocciaForOfStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
var
  IterableValue: TGocciaValue;
  Iterator: TGocciaIteratorValue;
  IterResult: TGocciaObjectValue;
  CurrentValue: TGocciaValue;
  CF: TGocciaControlFlow;
  IterScope: TGocciaScope;
  IterContext: TGocciaEvaluationContext;
  DeclarationType: TGocciaDeclarationType;
begin
  Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);

  IterableValue := EvaluateExpression(AForOfStatement.Iterable, AContext);
  Iterator := GetIteratorFromValue(IterableValue);
  if Iterator = nil then
  begin
    if AForOfStatement.Iterable is TGocciaIdentifierExpression then
      ThrowTypeError(
        Format(SErrorNotIterable,
          [TGocciaIdentifierExpression(AForOfStatement.Iterable).Name]),
        Format('''%s'' is of type ''%s'' which does not implement the iterator protocol',
          [TGocciaIdentifierExpression(AForOfStatement.Iterable).Name,
           IterableValue.TypeName]))
    else
      ThrowTypeError(
        Format(SErrorNotIterable, [IterableValue.TypeName]),
        SSuggestIteratorProtocol);
  end;

  if AForOfStatement.IsConst then
    DeclarationType := dtConst
  else
    DeclarationType := dtLet;

  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AddTempRoot(Iterator);
  try
    IterResult := Iterator.AdvanceNext;
    while not IterResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value do
    begin
      CheckExecutionTimeout;
      IncrementInstructionCounter;
      CheckInstructionLimit;
      CurrentValue := IterResult.GetProperty(PROP_VALUE);

      IterScope := AContext.Scope.CreateChild(skBlock);
      IterContext := AContext;
      IterContext.Scope := IterScope;

      if AForOfStatement.BindingPattern <> nil then
        AssignPattern(AForOfStatement.BindingPattern, CurrentValue, IterContext, True, DeclarationType)
      else
        IterScope.DefineLexicalBinding(AForOfStatement.BindingName, CurrentValue, DeclarationType);

      CF := EvaluateStatement(AForOfStatement.Body, IterContext);
      if CF.Kind = cfkBreak then Break;
      if CF.Kind = cfkReturn then begin Result := CF; Exit; end;

      IterResult := Iterator.AdvanceNext;
    end;
  finally
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveTempRoot(Iterator);
  end;
end;

// ES2026 §14.7.5.6 ForIn/OfBodyEvaluation — for-await-of variant
function EvaluateForAwaitOf(const AForAwaitOfStatement: TGocciaForAwaitOfStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
var
  IterableValue, IteratorMethod, IteratorObj, NextMethod, NextResult, DoneValue, CurrentValue: TGocciaValue;
  Iterator: TGocciaIteratorValue;
  GenericNextResult: TGocciaObjectValue;
  CF: TGocciaControlFlow;
  IterScope: TGocciaScope;
  IterContext: TGocciaEvaluationContext;
  DeclarationType: TGocciaDeclarationType;
  EmptyArgs: TGocciaArgumentsCollection;
begin
  Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);

  IterableValue := EvaluateExpression(AForAwaitOfStatement.Iterable, AContext);

  if AForAwaitOfStatement.IsConst then
    DeclarationType := dtConst
  else
    DeclarationType := dtLet;

  IteratorMethod := nil;
  if IterableValue is TGocciaObjectValue then
    IteratorMethod := TGocciaObjectValue(IterableValue).GetSymbolProperty(TGocciaSymbolValue.WellKnownAsyncIterator);

  if Assigned(IteratorMethod) and IteratorMethod.IsCallable then
  begin
    EmptyArgs := TGocciaArgumentsCollection.Create;
    try
      IteratorObj := TGocciaFunctionBase(IteratorMethod).Call(EmptyArgs, IterableValue);
    finally
      EmptyArgs.Free;
    end;

    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AddTempRoot(IteratorObj);
    try
      EmptyArgs := TGocciaArgumentsCollection.Create;
      try
        NextMethod := IteratorObj.GetProperty(PROP_NEXT);
        if not Assigned(NextMethod) or not NextMethod.IsCallable then
          ThrowTypeError(SErrorAsyncIteratorNextNotCallable, SSuggestAsyncIteratorProtocol);

        while True do
        begin
          CheckExecutionTimeout;
          IncrementInstructionCounter;
          CheckInstructionLimit;
          NextResult := TGocciaFunctionBase(NextMethod).Call(EmptyArgs, IteratorObj);
          NextResult := AwaitValue(NextResult);

          // ES2026 §7.4.2 step 5: If nextResult is not an Object, throw a TypeError
          if NextResult.IsPrimitive then
            ThrowTypeError(Format(SErrorIteratorResultNotObject, [NextResult.ToStringLiteral.Value]),
              SSuggestIteratorResultObject);

          DoneValue := NextResult.GetProperty(PROP_DONE);
          if Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value then
            Break;

          CurrentValue := NextResult.GetProperty(PROP_VALUE);
          if not Assigned(CurrentValue) then
            CurrentValue := TGocciaUndefinedLiteralValue.UndefinedValue;

          IterScope := AContext.Scope.CreateChild(skBlock);
          IterContext := AContext;
          IterContext.Scope := IterScope;

          if AForAwaitOfStatement.BindingPattern <> nil then
            AssignPattern(AForAwaitOfStatement.BindingPattern, CurrentValue, IterContext, True, DeclarationType)
          else
            IterScope.DefineLexicalBinding(AForAwaitOfStatement.BindingName, CurrentValue, DeclarationType);

          CF := EvaluateStatement(AForAwaitOfStatement.Body, IterContext);
          if CF.Kind = cfkBreak then Break;
          if CF.Kind = cfkReturn then begin Result := CF; Exit; end;
        end;
      finally
        EmptyArgs.Free;
      end;
    finally
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.RemoveTempRoot(IteratorObj);
    end;
  end
  else
  begin
    Iterator := GetIteratorFromValue(IterableValue);
    if Iterator = nil then
      ThrowTypeError(
        Format(SErrorNotIterable, [IterableValue.TypeName]),
        SSuggestIteratorProtocol);

    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AddTempRoot(Iterator);
    try
      GenericNextResult := Iterator.AdvanceNext;
      while not GenericNextResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value do
      begin
        CheckExecutionTimeout;
        IncrementInstructionCounter;
        CheckInstructionLimit;
        CurrentValue := GenericNextResult.GetProperty(PROP_VALUE);
        CurrentValue := AwaitValue(CurrentValue);

        IterScope := AContext.Scope.CreateChild(skBlock);
        IterContext := AContext;
        IterContext.Scope := IterScope;

        if AForAwaitOfStatement.BindingPattern <> nil then
          AssignPattern(AForAwaitOfStatement.BindingPattern, CurrentValue, IterContext, True, DeclarationType)
        else
          IterScope.DefineLexicalBinding(AForAwaitOfStatement.BindingName, CurrentValue, DeclarationType);

        CF := EvaluateStatement(AForAwaitOfStatement.Body, IterContext);
        if CF.Kind = cfkBreak then Break;
        if CF.Kind = cfkReturn then begin Result := CF; Exit; end;

        GenericNextResult := Iterator.AdvanceNext;
      end;
    finally
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.RemoveTempRoot(Iterator);
    end;
  end;
end;

function EvaluateArrowFunction(const AArrowFunctionExpression: TGocciaArrowFunctionExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Statements: TObjectList<TGocciaASTNode>;
begin
  if AArrowFunctionExpression.Body is TGocciaBlockStatement then
    Statements := CopyStatementList(TGocciaBlockStatement(AArrowFunctionExpression.Body).Nodes)
  else
  begin
    // Body is a single expression: (n) => n * 2
    Statements := TObjectList<TGocciaASTNode>.Create(False);
    Statements.Add(AArrowFunctionExpression.Body);
  end;

  if AArrowFunctionExpression.IsAsync then
    Result := TGocciaAsyncArrowFunctionValue.Create(AArrowFunctionExpression.Parameters, Statements, AContext.Scope.CreateChild)
  else
    Result := TGocciaArrowFunctionValue.Create(AArrowFunctionExpression.Parameters, Statements, AContext.Scope.CreateChild);
  TGocciaFunctionValue(Result).IsExpressionBody := not (AArrowFunctionExpression.Body is TGocciaBlockStatement);
  TGocciaFunctionValue(Result).SourceFilePath := AContext.CurrentFilePath;
  TGocciaFunctionValue(Result).SourceLine := AArrowFunctionExpression.Line;
end;

function EvaluateMethodExpression(const AMethodExpression: TGocciaMethodExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Statements: TObjectList<TGocciaASTNode>;
begin
  if AMethodExpression.Body is TGocciaBlockStatement then
    Statements := CopyStatementList(TGocciaBlockStatement(AMethodExpression.Body).Nodes)
  else
  begin
    Statements := TObjectList<TGocciaASTNode>.Create(False);
    Statements.Add(AMethodExpression.Body);
  end;

  if AMethodExpression.IsAsync then
    Result := TGocciaAsyncFunctionValue.Create(AMethodExpression.Parameters, Statements, AContext.Scope.CreateChild)
  else
    Result := TGocciaFunctionValue.Create(AMethodExpression.Parameters, Statements, AContext.Scope.CreateChild);
  TGocciaFunctionValue(Result).SourceFilePath := AContext.CurrentFilePath;
  TGocciaFunctionValue(Result).SourceLine := AMethodExpression.Line;
end;

// TC39 Explicit Resource Management §3.6 DisposeResources — sync disposal
function DisposeTrackedResources(const ATracker: TGocciaDisposalTracker;
  const AExistingError: TGocciaValue): TGocciaValue;
var
  I: Integer;
  Resource: TGocciaDisposableResource;
  CurrentError: TGocciaValue;
  HasError: Boolean;
begin
  CurrentError := AExistingError;
  HasError := Assigned(AExistingError);

  // Dispose in reverse order (LIFO)
  for I := ATracker.Count - 1 downto 0 do
  begin
    Resource := ATracker.GetResource(I);
    if Assigned(Resource.DisposeMethod) and Resource.DisposeMethod.IsCallable then
    begin
      try
        TGocciaFunctionBase(Resource.DisposeMethod).CallNoArgs(Resource.ResourceValue);
      except
        on E: TGocciaThrowValue do
        begin
          if HasError then
            CurrentError := CreateSuppressedErrorObject(E.Value, CurrentError)
          else
            CurrentError := E.Value;
          HasError := True;
        end;
      end;
    end;
  end;

  if HasError then
    Result := CurrentError
  else
    Result := nil;
end;

// TC39 Explicit Resource Management §3.6 DisposeResources — async disposal
function DisposeTrackedResourcesAsync(const ATracker: TGocciaDisposalTracker;
  const AExistingError: TGocciaValue): TGocciaValue;
var
  I: Integer;
  Resource: TGocciaDisposableResource;
  CurrentError: TGocciaValue;
  HasError: Boolean;
  CallResult: TGocciaValue;
begin
  CurrentError := AExistingError;
  HasError := Assigned(AExistingError);

  // Dispose in reverse order (LIFO)
  for I := ATracker.Count - 1 downto 0 do
  begin
    Resource := ATracker.GetResource(I);
    if Assigned(Resource.DisposeMethod) and Resource.DisposeMethod.IsCallable then
    begin
      try
        CallResult := TGocciaFunctionBase(Resource.DisposeMethod)
          .CallNoArgs(Resource.ResourceValue);
        // For async disposal, await the result
        if Resource.Hint = dhAsyncDispose then
        begin
          if Assigned(CallResult) then
            AwaitValue(CallResult);
        end;
      except
        on E: TGocciaThrowValue do
        begin
          if HasError then
            CurrentError := CreateSuppressedErrorObject(E.Value, CurrentError)
          else
            CurrentError := E.Value;
          HasError := True;
        end;
      end;
    end
    else if Resource.Hint = dhAsyncDispose then
    begin
      // Null/undefined value in await using — ensure at least one await point
      AwaitValue(TGocciaUndefinedLiteralValue.UndefinedValue);
    end;
  end;

  if HasError then
    Result := CurrentError
  else
    Result := nil;
end;

function HasAsyncDisposals(const ATracker: TGocciaDisposalTracker): Boolean;
var
  I: Integer;
begin
  for I := 0 to ATracker.Count - 1 do
    if ATracker.GetResource(I).Hint = dhAsyncDispose then
      Exit(True);
  Result := False;
end;

function EvaluateBlock(const ABlockStatement: TGocciaBlockStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
var
  I: Integer;
  BlockContext: TGocciaEvaluationContext;
  NeedsChildScope: Boolean;
  HasUsingDeclarations: Boolean;
  Tracker: TGocciaDisposalTracker;
  DisposalError: TGocciaValue;
  CaughtError: TGocciaValue;
  HasCaughtError: Boolean;
  GC: TGarbageCollector;
begin
  NeedsChildScope := False;
  HasUsingDeclarations := False;
  for I := 0 to ABlockStatement.Nodes.Count - 1 do
  begin
    if (ABlockStatement.Nodes[I] is TGocciaVariableDeclaration) or
       (ABlockStatement.Nodes[I] is TGocciaDestructuringDeclaration) or
       (ABlockStatement.Nodes[I] is TGocciaUsingDeclaration) then
    begin
      NeedsChildScope := True;
      if ABlockStatement.Nodes[I] is TGocciaUsingDeclaration then
        HasUsingDeclarations := True;
    end;
  end;

  // Fast path: no using declarations — original block evaluation
  if not HasUsingDeclarations then
  begin
    BlockContext := AContext;
    if NeedsChildScope then
      BlockContext.Scope := AContext.Scope.CreateChild(skBlock, 'BlockScope')
    else
      BlockContext.Scope := AContext.Scope;
    try
      Result := EvaluateStatements(ABlockStatement.Nodes, BlockContext);
      if (Result.Kind = cfkNormal) and (Result.Value = nil) then
        Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      if NeedsChildScope then
        BlockContext.Scope.Free;
    end;
    Exit;
  end;

  // Slow path: block has using declarations — need disposal at exit
  BlockContext := AContext;
  BlockContext.Scope := AContext.Scope.CreateChild(skBlock, 'BlockScope');

  Tracker := TGocciaDisposalTracker.Create;
  BlockContext.DisposalTracker := Tracker;

  CaughtError := nil;
  HasCaughtError := False;
  GC := TGarbageCollector.Instance;
  try
    try
      Result := EvaluateStatements(ABlockStatement.Nodes, BlockContext);
      if (Result.Kind = cfkNormal) and (Result.Value = nil) then
        Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
    except
      on E: TGocciaThrowValue do
      begin
        CaughtError := E.Value;
        HasCaughtError := True;
        // Protect the caught error value from GC during disposal
        if Assigned(GC) and Assigned(CaughtError) then
          GC.AddTempRoot(CaughtError);
        Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
      end;
    end;
  finally
    // TC39 Explicit Resource Management: dispose resources at block exit
    // Route through async path when tracker contains await using entries
    if HasCaughtError then
    begin
      if HasAsyncDisposals(Tracker) then
        DisposalError := DisposeTrackedResourcesAsync(Tracker, CaughtError)
      else
        DisposalError := DisposeTrackedResources(Tracker, CaughtError);
    end
    else
    begin
      if HasAsyncDisposals(Tracker) then
        DisposalError := DisposeTrackedResourcesAsync(Tracker, nil)
      else
        DisposalError := DisposeTrackedResources(Tracker, nil);
    end;
    Tracker.Free;
    BlockContext.DisposalTracker := nil;

    // Free the block scope before re-raising
    BlockContext.Scope.Free;

    // Remove GC temp root before re-raising
    if HasCaughtError and Assigned(GC) and Assigned(CaughtError) then
      GC.RemoveTempRoot(CaughtError);

    // If disposal produced an error, throw it (may be SuppressedError)
    if Assigned(DisposalError) then
      raise TGocciaThrowValue.Create(DisposalError)
    else if HasCaughtError then
      raise TGocciaThrowValue.Create(CaughtError);
  end;
end;

// TC39 Explicit Resource Management: using / await using declaration evaluation
function EvaluateUsingDeclaration(const AUsingDeclaration: TGocciaUsingDeclaration; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
var
  I: Integer;
  Value: TGocciaValue;
  DisposeMethod: TGocciaValue;
  Tracker: TGocciaDisposalTracker;
  Hint: TGocciaDisposalHint;
begin
  Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);

  // The disposal tracker should have been set by EvaluateBlock
  if (AContext.DisposalTracker = nil) or
     not (AContext.DisposalTracker is TGocciaDisposalTracker) then
    ThrowTypeError(SErrorUsingOutsideBlock, SSuggestUsingInsideBlock);
  Tracker := TGocciaDisposalTracker(AContext.DisposalTracker);

  if AUsingDeclaration.IsAwait then
    Hint := dhAsyncDispose
  else
    Hint := dhSyncDispose;

  for I := 0 to Length(AUsingDeclaration.Variables) - 1 do
  begin
    // TC39 Explicit Resource Management §3.4 CreateDisposableResource
    Value := EvaluateExpression(AUsingDeclaration.Variables[I].Initializer, AContext);

    // null and undefined are silently skipped (no error, no disposal)
    if (Value is TGocciaUndefinedLiteralValue) or (Value is TGocciaNullLiteralValue) then
    begin
      // Bind the variable but don't track for disposal
      AContext.Scope.DefineLexicalBinding(AUsingDeclaration.Variables[I].Name, Value, dtConst);

      // For await using with null/undefined, still record to ensure an await point
      if Hint = dhAsyncDispose then
        Tracker.AddResource(nil, nil, dhAsyncDispose);
    end
    else
    begin
      // Value must be an object with the appropriate @@dispose/@@asyncDispose method
      DisposeMethod := GetDisposeMethod(Value, Hint);
      if not Assigned(DisposeMethod) then
      begin
        if Hint = dhAsyncDispose then
          ThrowTypeError(SErrorNotAsyncDisposable, SSuggestDisposable)
        else
          ThrowTypeError(SErrorNotDisposable, SSuggestDisposable);
      end;

      // Bind the variable as const (using bindings are not reassignable)
      AContext.Scope.DefineLexicalBinding(AUsingDeclaration.Variables[I].Name, Value, dtConst);

      // Track for disposal at block exit
      Tracker.AddResource(Value, DisposeMethod, Hint);
    end;
  end;
end;

function EvaluateIf(const AIfStatement: TGocciaIfStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
var
  ConditionResult: Boolean;
begin
  ConditionResult := EvaluateExpression(AIfStatement.Condition, AContext)
    .ToBooleanLiteral.Value;
  if AContext.CoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
  begin
    if ConditionResult then
      TGocciaCoverageTracker.Instance.RecordBranchHit(
        AContext.CurrentFilePath, AIfStatement.Line, AIfStatement.Column, 0)
    else
      TGocciaCoverageTracker.Instance.RecordBranchHit(
        AContext.CurrentFilePath, AIfStatement.Line, AIfStatement.Column, 1);
  end;
  if ConditionResult then
    Result := EvaluateStatement(AIfStatement.Consequent, AContext)
  else if Assigned(AIfStatement.Alternate) then
    Result := EvaluateStatement(AIfStatement.Alternate, AContext)
  else
    Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

function ExecuteCatchBlock(const ATryStatement: TGocciaTryStatement; const AErrorValue: TGocciaValue; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
var
  CatchScope: TGocciaScope;
  CatchContext: TGocciaEvaluationContext;
begin
  if ATryStatement.CatchParam <> '' then
  begin
    CatchScope := TGocciaCatchScope.Create(AContext.Scope, ATryStatement.CatchParam);
    try
      CatchScope.DefineLexicalBinding(ATryStatement.CatchParam, AErrorValue, dtParameter);
      CatchContext := AContext;
      CatchContext.Scope := CatchScope;
      Result := EvaluateStatements(ATryStatement.CatchBlock.Nodes, CatchContext);
    finally
      CatchScope.Free;
    end;
  end
  else
    Result := EvaluateStatements(ATryStatement.CatchBlock.Nodes, AContext);
end;

function PascalExceptionToErrorObject(const E: Exception): TGocciaValue;
begin
  if E is TGocciaTypeError then
    Result := CreateErrorObject(TYPE_ERROR_NAME, E.Message)
  else if E is TGocciaReferenceError then
    Result := CreateErrorObject(REFERENCE_ERROR_NAME, E.Message)
  else if E is TGocciaSyntaxError then
    Result := CreateErrorObject(SYNTAX_ERROR_NAME, E.Message)
  else if E is TGocciaRuntimeError then
    Result := CreateErrorObject(ERROR_NAME, E.Message)
  else
    Result := CreateErrorObject(ERROR_NAME, E.Message);
end;

function EvaluateTry(const ATryStatement: TGocciaTryStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
var
  ThrownValue: TGocciaValue;
  HasUnhandledThrow: Boolean;
  FinallyCF: TGocciaControlFlow;
begin
  HasUnhandledThrow := False;
  ThrownValue := nil;
  Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);

  // Phase 1: Execute try block, capturing throws
  try
    Result := EvaluateStatements(ATryStatement.Block.Nodes, AContext);
  except
    on E: TGocciaThrowValue do
    begin
      if Assigned(ATryStatement.CatchBlock) then
      begin
        try
          Result := ExecuteCatchBlock(ATryStatement, E.Value, AContext);
        except
          on E2: TGocciaThrowValue do
          begin
            HasUnhandledThrow := True;
            ThrownValue := E2.Value;
          end;
        end;
      end
      else
      begin
        HasUnhandledThrow := True;
        ThrownValue := E.Value;
      end;
    end;
    on E: TGocciaTimeoutError do
      raise;
    on E: TGocciaInstructionLimitError do
      raise;
    on E: Exception do
    begin
      if Assigned(ATryStatement.CatchBlock) then
      begin
        try
          Result := ExecuteCatchBlock(ATryStatement, PascalExceptionToErrorObject(E), AContext);
        except
          on E2: TGocciaThrowValue do
          begin
            HasUnhandledThrow := True;
            ThrownValue := E2.Value;
          end;
        end;
      end
      else
      begin
        HasUnhandledThrow := True;
        ThrownValue := PascalExceptionToErrorObject(E);
      end;
    end;
  end;

  // Phase 2: Execute finally block (always runs)
  if Assigned(ATryStatement.FinallyBlock) then
  begin
    if HasUnhandledThrow and Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AddTempRoot(ThrownValue);
    try
      FinallyCF := EvaluateStatements(ATryStatement.FinallyBlock.Nodes, AContext);
      // Per JS semantics: finally's control flow overrides try/catch result AND pending throw
      if FinallyCF.Kind <> cfkNormal then
      begin
        Result := FinallyCF;
        Exit;
      end;
      // If finally throws (TGocciaThrowValue), it propagates naturally and overrides everything
    finally
      if HasUnhandledThrow and Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.RemoveTempRoot(ThrownValue);
    end;
  end;

  // Phase 3: Re-raise unhandled throw (if not overridden by finally)
  if HasUnhandledThrow then
    raise TGocciaThrowValue.Create(ThrownValue);
end;

function EvaluateClassMethod(const AClassMethod: TGocciaClassMethod; const AContext: TGocciaEvaluationContext; const ASuperClass: TGocciaValue = nil): TGocciaValue;
var
  Statements: TObjectList<TGocciaASTNode>;
begin
  Statements := CopyStatementList(TGocciaBlockStatement(AClassMethod.Body).Nodes);

  if AClassMethod.IsAsync then
    Result := TGocciaAsyncMethodValue.Create(AClassMethod.Parameters, Statements, AContext.Scope.CreateChild, AClassMethod.Name, ASuperClass)
  else
    Result := TGocciaMethodValue.Create(AClassMethod.Parameters, Statements, AContext.Scope.CreateChild, AClassMethod.Name, ASuperClass);
  TGocciaFunctionValue(Result).SourceFilePath := AContext.CurrentFilePath;
  TGocciaFunctionValue(Result).SourceLine := AClassMethod.Line;
end;

function EvaluateClass(const AClassDeclaration: TGocciaClassDeclaration; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  ClassDef: TGocciaClassDefinition;
begin
  ClassDef := AClassDeclaration.ClassDefinition;
  Result := EvaluateClassDefinition(ClassDef, AContext, AClassDeclaration.Line, AClassDeclaration.Column);

  // For class declarations, bind the class name to the scope
  AContext.Scope.DefineLexicalBinding(ClassDef.Name, Result, dtLet);
end;

// TC39 proposal-enum
function EvaluateEnumDeclaration(const AEnumDeclaration: TGocciaEnumDeclaration; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  EnumValue: TGocciaEnumValue;
  EnumEntries: TGocciaArrayValue;
  ChildScope: TGocciaScope;
  ChildContext: TGocciaEvaluationContext;
  I: Integer;
  MemberValue: TGocciaValue;
  EntryPair: TGocciaArrayValue;
begin
  EnumValue := TGocciaEnumValue.Create(AEnumDeclaration.Name);
  EnumEntries := TGocciaArrayValue.Create;

  ChildScope := AContext.Scope.CreateChild(skBlock);
  ChildContext := AContext;
  ChildContext.Scope := ChildScope;

  ChildScope.DefineLexicalBinding(AEnumDeclaration.Name, EnumValue, dtLet);

  TGarbageCollector.Instance.AddTempRoot(EnumEntries);
  try
    for I := 0 to Length(AEnumDeclaration.Members) - 1 do
    begin
      MemberValue := EvaluateExpression(AEnumDeclaration.Members[I].Initializer, ChildContext);

      if not ((MemberValue is TGocciaNumberLiteralValue) or
              (MemberValue is TGocciaStringLiteralValue) or
              (MemberValue is TGocciaSymbolValue)) then
        ThrowTypeError(SErrorEnumInitializer, SSuggestEnumValueType);

      EnumValue.DefineProperty(AEnumDeclaration.Members[I].Name,
        TGocciaPropertyDescriptorData.Create(MemberValue, [pfEnumerable]));

      ChildScope.DefineLexicalBinding(AEnumDeclaration.Members[I].Name, MemberValue, dtLet);

      EntryPair := TGocciaArrayValue.Create;
      EntryPair.Elements.Add(TGocciaStringLiteralValue.Create(AEnumDeclaration.Members[I].Name));
      EntryPair.Elements.Add(MemberValue);
      EnumEntries.Elements.Add(EntryPair);
    end;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(EnumEntries);
  end;

  EnumValue.Entries := EnumEntries;
  InitializeEnumSymbols(EnumValue);
  EnumValue.PreventExtensions;

  AContext.Scope.DefineLexicalBinding(AEnumDeclaration.Name, EnumValue, dtLet);
  Result := EnumValue;
end;

procedure InitializeInstanceProperties(const AInstance: TGocciaInstanceValue; const AClassValue: TGocciaClassValue; const AContext: TGocciaEvaluationContext);
var
  PropertyValue: TGocciaValue;
  Entry: TGocciaExpressionMap.TKeyValuePair;
  I: Integer;
  FOEntry: TGocciaClassFieldOrderEntry;
  Expr: TGocciaExpression;
begin
  if Assigned(AClassValue.SuperClass) then
    InitializeInstanceProperties(AInstance, AClassValue.SuperClass, AContext);

  if AClassValue.FieldOrderCount > 0 then
  begin
    for I := 0 to AClassValue.FieldOrderCount - 1 do
    begin
      FOEntry := AClassValue.FieldOrderEntry(I);
      Expr := nil;
      if FOEntry.IsPrivate then
      begin
        if AClassValue.PrivateInstancePropertyDefs.TryGetValue(FOEntry.Name, Expr) and Assigned(Expr) then
        begin
          PropertyValue := EvaluateExpression(Expr, AContext);
          AInstance.SetPrivateProperty(FOEntry.Name, PropertyValue, AClassValue);
        end;
      end
      else
      begin
        if AClassValue.InstancePropertyDefs.TryGetValue(FOEntry.Name, Expr) and Assigned(Expr) then
        begin
          PropertyValue := EvaluateExpression(Expr, AContext);
          AInstance.AssignProperty(FOEntry.Name, PropertyValue);
        end;
      end;
    end;
  end
  else
  begin
    for I := 0 to AClassValue.InstancePropertyDefs.Count - 1 do
    begin
      Entry := AClassValue.InstancePropertyDefs.EntryAt(I);
      PropertyValue := EvaluateExpression(Entry.Value, AContext);
      AInstance.AssignProperty(Entry.Key, PropertyValue);
    end;
  end;
end;

function EvaluateSwitch(const ASwitchStatement: TGocciaSwitchStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
var
  Discriminant: TGocciaValue;
  CaseClause: TGocciaCaseClause;
  CaseTest: TGocciaValue;
  CF: TGocciaControlFlow;
  I, J: Integer;
  Matched: Boolean;
  DefaultIndex: Integer;
  Done: Boolean;
begin
  Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
  Discriminant := EvaluateExpression(ASwitchStatement.Discriminant, AContext);

  Matched := False;
  DefaultIndex := -1;
  Done := False;

  for I := 0 to ASwitchStatement.Cases.Count - 1 do
  begin
    CaseClause := ASwitchStatement.Cases[I];

    if not Assigned(CaseClause.Test) then
    begin
      DefaultIndex := I;
      if not Matched then
        Continue;
    end;

    if not Matched then
    begin
      CaseTest := EvaluateExpression(CaseClause.Test, AContext);
      if IsStrictEqual(Discriminant, CaseTest) then
      begin
        Matched := True;
        if AContext.CoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
          TGocciaCoverageTracker.Instance.RecordBranchHit(
            AContext.CurrentFilePath, ASwitchStatement.Line,
            ASwitchStatement.Column, I);
      end;
    end;

    if Matched then
    begin
      for J := 0 to CaseClause.Consequent.Count - 1 do
      begin
        CF := EvaluateStatement(CaseClause.Consequent[J], AContext);
        if CF.Kind = cfkBreak then begin Done := True; Break; end;
        if CF.Kind = cfkReturn then begin Result := CF; Exit; end;
        Result := CF;
      end;
      if Done then Break;
    end;
  end;

  if not Matched and not Done and (DefaultIndex >= 0) then
  begin
    if AContext.CoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
      TGocciaCoverageTracker.Instance.RecordBranchHit(
        AContext.CurrentFilePath, ASwitchStatement.Line,
        ASwitchStatement.Column, DefaultIndex);
    for I := DefaultIndex to ASwitchStatement.Cases.Count - 1 do
    begin
      CaseClause := ASwitchStatement.Cases[I];
      for J := 0 to CaseClause.Consequent.Count - 1 do
      begin
        CF := EvaluateStatement(CaseClause.Consequent[J], AContext);
        if CF.Kind = cfkBreak then begin Done := True; Break; end;
        if CF.Kind = cfkReturn then begin Result := CF; Exit; end;
        Result := CF;
      end;
      if Done then Break;
    end;
  end;
end;

function EvaluateNewExpression(const ANewExpression: TGocciaNewExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Callee: TGocciaValue;
  Arguments: TGocciaArgumentsCollection;
  SpreadValue: TGocciaValue;
  CalleeName: string;
  I: Integer;
  Instance: TGocciaInstanceValue;
  NativeInstance: TGocciaObjectValue;
  WalkClass, ClassValue: TGocciaClassValue;
  InitContext, SuperInitContext: TGocciaEvaluationContext;
  InitScope, SuperInitScope: TGocciaScope;
begin
  CheckExecutionTimeout;
  IncrementInstructionCounter;
  CheckInstructionLimit;
  Callee := EvaluateExpression(ANewExpression.Callee, AContext);

  Arguments := TGocciaArgumentsCollection.Create;
  try
    for I := 0 to ANewExpression.Arguments.Count - 1 do
    begin
      if ANewExpression.Arguments[I] is TGocciaSpreadExpression then
      begin
        SpreadValue := EvaluateExpression(TGocciaSpreadExpression(ANewExpression.Arguments[I]).Argument, AContext);
        SpreadIterableIntoArgs(SpreadValue, Arguments);
      end
      else
        Arguments.Add(EvaluateExpression(ANewExpression.Arguments[I], AContext));
    end;

    if Callee is TGocciaClassValue then
      CalleeName := TGocciaClassValue(Callee).Name
    else if Callee is TGocciaNativeFunctionValue then
      CalleeName := TGocciaNativeFunctionValue(Callee).Name
    else
      CalleeName := '';

    if Assigned(TGocciaCallStack.Instance) then
      TGocciaCallStack.Instance.Push(CalleeName, AContext.CurrentFilePath,
        ANewExpression.Line, ANewExpression.Column);
    try
      if Callee is TGocciaProxyValue then
      begin
        Result := TGocciaProxyValue(Callee).ConstructTrap(Arguments);
      end
      else if Callee is TGocciaClassValue then
      begin
        Result := InstantiateClass(TGocciaClassValue(Callee), Arguments, AContext);
      end
      else if Callee is TGocciaNativeFunctionValue then
      begin
        Result := TGocciaNativeFunctionValue(Callee).Call(Arguments,
          TGocciaHoleValue.HoleValue);
      end
      else
      begin
        if ANewExpression.Callee is TGocciaIdentifierExpression then
          ThrowTypeError(
            Format(SErrorNotConstructor,
              [TGocciaIdentifierExpression(ANewExpression.Callee).Name]),
            Format('''%s'' is of type ''%s'' and cannot be used with ''new''',
              [TGocciaIdentifierExpression(ANewExpression.Callee).Name,
               Callee.TypeName]))
        else
          ThrowTypeError(
            Format(SErrorValueNotConstructor, [Callee.TypeName]),
            Format('values of type ''%s'' cannot be used with ''new''',
              [Callee.TypeName]));
      end;
    finally
      if Assigned(TGocciaCallStack.Instance) then
        TGocciaCallStack.Instance.Pop;
    end;
  finally
    Arguments.Free;
  end;
end;

function EvaluateClassExpression(const AClassExpression: TGocciaClassExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  ClassDef: TGocciaClassDefinition;
  InnerContext: TGocciaEvaluationContext;
  InnerScope: TGocciaScope;
begin
  ClassDef := AClassExpression.ClassDefinition;

  if ClassDef.Name <> '' then
  begin
    // ES2026 §15.7.14: Named class expressions create an inner binding
    // visible only inside the class body
    InnerScope := AContext.Scope.CreateChild(skBlock);
    InnerScope.DefineLexicalBinding(ClassDef.Name,
      TGocciaUndefinedLiteralValue.UndefinedValue, dtConst);
    InnerContext := AContext;
    InnerContext.Scope := InnerScope;
    Result := EvaluateClassDefinition(ClassDef, InnerContext,
      AClassExpression.Line, AClassExpression.Column);
    InnerScope.ForceUpdateBinding(ClassDef.Name, Result);
  end
  else
    Result := EvaluateClassDefinition(ClassDef, AContext,
      AClassExpression.Line, AClassExpression.Column);
end;

// ES2022 §15.7.14 ClassStaticBlockDefinition: execute static block body
procedure ExecuteStaticBlock(const ABody: TGocciaBlockStatement;
  const AContext: TGocciaEvaluationContext; const AClassValue: TGocciaClassValue);
var
  BlockScope: TGocciaClassInitScope;
  BlockContext: TGocciaEvaluationContext;
begin
  BlockScope := TGocciaClassInitScope.Create(AContext.Scope, AClassValue);
  BlockScope.ThisValue := AClassValue;
  BlockContext := AContext;
  BlockContext.Scope := BlockScope;
  EvaluateStatements(ABody.Nodes, BlockContext);
end;

function EvaluateClassDefinition(const AClassDef: TGocciaClassDefinition; const AContext: TGocciaEvaluationContext; const ALine, AColumn: Integer): TGocciaClassValue;
var
  SuperClass: TGocciaClassValue;
  SuperClassValue: TGocciaValue;
  ClassValue: TGocciaClassValue;
  MethodPair: TGocciaClassMethodMap.TKeyValuePair;
  PropertyPair: TGocciaExpressionMap.TKeyValuePair;
  PropertyEntry: TGocciaExpressionMap.TKeyValuePair;
  GetterPair: TGocciaGetterExpressionMap.TKeyValuePair;
  SetterPair: TGocciaSetterExpressionMap.TKeyValuePair;
  Method: TGocciaMethodValue;
  ComputedKey: TGocciaValue;
  PropertyValue: TGocciaValue;
  PropertyExpr: TGocciaExpression;
  GetterFunction, SetterFunction: TGocciaFunctionValue;
  ClassName: string;
  I, J: Integer;
  HasDecorators: Boolean;
  FieldOrderEntries: array of TGocciaClassFieldOrderEntry;
  MetadataObject: TGocciaObjectValue;
  SuperMetadata: TGocciaValue;
  EvaluatedElementDecorators: array of array of TGocciaValue;
  EvaluatedClassDecorators: array of TGocciaValue;
  DecoratorFn, DecoratorResult: TGocciaValue;
  DecoratorArgs, InitArgs: TGocciaArgumentsCollection;
  ContextObject, AccessObject, AutoAccessorValue, DecResultObj: TGocciaObjectValue;
  Elem: TGocciaClassElement;
  ElementName: string;
  CurrentMethod, GetterFnValue, SetterFnValue: TGocciaValue;
  NewGetter, NewSetter, NewInit: TGocciaValue;
  MethodCollector, FieldCollector, StaticFieldCollector, ClassCollector: TGocciaInitializerCollector;
  AccessGetterHelper: TGocciaAccessGetter;
  AccessSetterHelper: TGocciaAccessSetter;
  InitializerResults: TArray<TGocciaValue>;
  AccessorBackingName: string;
  ExistingDescriptor: TGocciaPropertyDescriptor;
begin
  SuperClass := nil;
  if AClassDef.SuperClass <> '' then
  begin
    SuperClassValue := AContext.Scope.GetValue(AClassDef.SuperClass);
    if SuperClassValue = nil then
      AContext.OnError(Format('Superclass "%s" not found', [AClassDef.SuperClass]), ALine, AColumn)
    else if not (SuperClassValue is TGocciaClassValue) then
      AContext.OnError(Format('Superclass "%s" is not a class (found %s)', [AClassDef.SuperClass, SuperClassValue.TypeName]), ALine, AColumn)
    else
      SuperClass := TGocciaClassValue(SuperClassValue);
  end;

  // Use the class name if provided, otherwise create an anonymous class
  if AClassDef.Name <> '' then
    ClassName := AClassDef.Name
  else
    ClassName := '<anonymous>';

  ClassValue := TGocciaClassValue.Create(ClassName, SuperClass);
  ClassValue.Prototype.AssignProperty(PROP_CONSTRUCTOR, ClassValue);

  for MethodPair in AClassDef.Methods do
  begin
    Method := TGocciaMethodValue(EvaluateClassMethod(MethodPair.Value, AContext, SuperClass));
    Method.OwningClass := ClassValue;

    if MethodPair.Value.IsStatic then
      ClassValue.SetProperty(MethodPair.Key, Method)
    else
      ClassValue.AddMethod(MethodPair.Key, Method);
  end;

  // Static fields without FElements entries (legacy / no static blocks)
  if Length(AClassDef.FElements) = 0 then
  begin
    for PropertyPair in AClassDef.StaticProperties do
    begin
      PropertyValue := EvaluateExpression(PropertyPair.Value, AContext);
      ClassValue.SetProperty(PropertyPair.Key, PropertyValue);
    end;

    for PropertyPair in AClassDef.PrivateStaticProperties do
    begin
      PropertyValue := EvaluateExpression(PropertyPair.Value, AContext);
      ClassValue.AddPrivateStaticProperty(PropertyPair.Key, PropertyValue);
    end;
  end;

  // Store instance property definitions on the class in declaration order
  for I := 0 to AClassDef.InstanceProperties.Count - 1 do
  begin
    PropertyEntry := AClassDef.InstanceProperties.EntryAt(I);
    ClassValue.AddInstanceProperty(PropertyEntry.Key, PropertyEntry.Value);
  end;

  // Store private instance property definitions on the class in declaration order
  for I := 0 to AClassDef.PrivateInstanceProperties.Count - 1 do
  begin
    PropertyEntry := AClassDef.PrivateInstanceProperties.EntryAt(I);
    ClassValue.AddPrivateInstanceProperty(PropertyEntry.Key, PropertyEntry.Value);
  end;

  // Copy field order for source-order initialization
  if Length(AClassDef.FFieldOrder) > 0 then
  begin
    SetLength(FieldOrderEntries, Length(AClassDef.FFieldOrder));
    for I := 0 to High(AClassDef.FFieldOrder) do
    begin
      FieldOrderEntries[I].Name := AClassDef.FFieldOrder[I].Name;
      FieldOrderEntries[I].IsPrivate := AClassDef.FFieldOrder[I].IsPrivate;
    end;
    ClassValue.SetFieldOrder(FieldOrderEntries);
  end;

  for MethodPair in AClassDef.PrivateMethods do
  begin
    Method := TGocciaMethodValue(EvaluateClassMethod(MethodPair.Value, AContext, SuperClass));
    Method.OwningClass := ClassValue;
    ClassValue.AddPrivateMethod(MethodPair.Key, Method);
  end;

  // Handle getters and setters

  for GetterPair in AClassDef.Getters do
  begin
    GetterFunction := TGocciaFunctionValue(EvaluateGetter(GetterPair.Value, AContext));
    if (Length(GetterPair.Key) > 0) and (GetterPair.Key[1] = '#') then
      ClassValue.AddPrivateGetter(Copy(GetterPair.Key, 2, Length(GetterPair.Key) - 1), GetterFunction)
    else
      ClassValue.AddGetter(GetterPair.Key, GetterFunction);
  end;

  for SetterPair in AClassDef.Setters do
  begin
    SetterFunction := TGocciaFunctionValue(EvaluateSetter(SetterPair.Value, AContext));
    if (Length(SetterPair.Key) > 0) and (SetterPair.Key[1] = '#') then
      ClassValue.AddPrivateSetter(Copy(SetterPair.Key, 2, Length(SetterPair.Key) - 1), SetterFunction)
    else
      ClassValue.AddSetter(SetterPair.Key, SetterFunction);
  end;

  for GetterPair in AClassDef.FStaticGetters do
  begin
    GetterFunction := TGocciaFunctionValue(EvaluateGetter(GetterPair.Value, AContext));
    if (GetterPair.Key <> '') and (GetterPair.Key[1] = '#') then
      ClassValue.AddPrivateGetter(Copy(GetterPair.Key, 2, Length(GetterPair.Key) - 1), GetterFunction)
    else
      ClassValue.AddStaticGetter(GetterPair.Key, GetterFunction);
  end;

  for SetterPair in AClassDef.FStaticSetters do
  begin
    SetterFunction := TGocciaFunctionValue(EvaluateSetter(SetterPair.Value, AContext));
    if (SetterPair.Key <> '') and (SetterPair.Key[1] = '#') then
      ClassValue.AddPrivateSetter(Copy(SetterPair.Key, 2, Length(SetterPair.Key) - 1), SetterFunction)
    else
      ClassValue.AddStaticSetter(SetterPair.Key, SetterFunction);
  end;

  // Handle computed members (methods, getters, setters) in source order via FElements
  for I := 0 to High(AClassDef.FElements) do
  begin
    Elem := AClassDef.FElements[I];
    if not Elem.IsComputed then
      Continue;
    if not (Elem.Kind in [cekMethod, cekGetter, cekSetter]) then
      Continue;

    ComputedKey := EvaluateExpression(Elem.ComputedKeyExpression, AContext);

    case Elem.Kind of
      cekMethod:
      begin
        Method := TGocciaMethodValue(EvaluateClassMethod(Elem.MethodNode, AContext, SuperClass));
        Method.OwningClass := ClassValue;
        if ComputedKey is TGocciaSymbolValue then
        begin
          if Elem.IsStatic then
            ClassValue.DefineSymbolProperty(
              TGocciaSymbolValue(ComputedKey),
              TGocciaPropertyDescriptorData.Create(Method, [pfWritable, pfConfigurable]))
          else
            ClassValue.Prototype.DefineSymbolProperty(
              TGocciaSymbolValue(ComputedKey),
              TGocciaPropertyDescriptorData.Create(Method, [pfWritable, pfConfigurable]));
        end
        else
        begin
          if Elem.IsStatic then
            ClassValue.SetProperty(ComputedKey.ToStringLiteral.Value, Method)
          else
            ClassValue.AddMethod(ComputedKey.ToStringLiteral.Value, Method);
        end;
      end;
      cekGetter:
      begin
        GetterFunction := TGocciaFunctionValue(EvaluateGetter(Elem.GetterNode, AContext));
        if ComputedKey is TGocciaSymbolValue then
        begin
          if Elem.IsStatic then
            ClassValue.DefineSymbolProperty(
              TGocciaSymbolValue(ComputedKey),
              TGocciaPropertyDescriptorAccessor.Create(GetterFunction, nil, [pfConfigurable]))
          else
            ClassValue.Prototype.DefineSymbolProperty(
              TGocciaSymbolValue(ComputedKey),
              TGocciaPropertyDescriptorAccessor.Create(GetterFunction, nil, [pfConfigurable]));
        end
        else
        begin
          if Elem.IsStatic then
            ClassValue.AddStaticGetter(ComputedKey.ToStringLiteral.Value, GetterFunction)
          else
            ClassValue.AddGetter(ComputedKey.ToStringLiteral.Value, GetterFunction);
        end;
      end;
      cekSetter:
      begin
        SetterFunction := TGocciaFunctionValue(EvaluateSetter(Elem.SetterNode, AContext));
        if ComputedKey is TGocciaSymbolValue then
        begin
          if Elem.IsStatic then
            ExistingDescriptor := ClassValue.GetOwnStaticSymbolDescriptor(TGocciaSymbolValue(ComputedKey))
          else
            ExistingDescriptor := ClassValue.Prototype.GetOwnSymbolPropertyDescriptor(TGocciaSymbolValue(ComputedKey));

          if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
             Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter) then
          begin
            if Elem.IsStatic then
              ClassValue.DefineSymbolProperty(
                TGocciaSymbolValue(ComputedKey),
                TGocciaPropertyDescriptorAccessor.Create(
                  TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter,
                  SetterFunction, [pfConfigurable]))
            else
              ClassValue.Prototype.DefineSymbolProperty(
                TGocciaSymbolValue(ComputedKey),
                TGocciaPropertyDescriptorAccessor.Create(
                  TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter,
                  SetterFunction, [pfConfigurable]));
          end
          else
          begin
            if Elem.IsStatic then
              ClassValue.DefineSymbolProperty(
                TGocciaSymbolValue(ComputedKey),
                TGocciaPropertyDescriptorAccessor.Create(nil, SetterFunction, [pfConfigurable]))
            else
              ClassValue.Prototype.DefineSymbolProperty(
                TGocciaSymbolValue(ComputedKey),
                TGocciaPropertyDescriptorAccessor.Create(nil, SetterFunction, [pfConfigurable]));
          end;
        end
        else
        begin
          if Elem.IsStatic then
            ClassValue.AddStaticSetter(ComputedKey.ToStringLiteral.Value, SetterFunction)
          else
            ClassValue.AddSetter(ComputedKey.ToStringLiteral.Value, SetterFunction);
        end;
      end;
    end;
  end;

  // TC39 proposal-decorators §3.1 ClassDefinitionEvaluation — auto-accessor setup
  for I := 0 to High(AClassDef.FElements) do
  begin
    if AClassDef.FElements[I].Kind = cekAccessor then
    begin
      Elem := AClassDef.FElements[I];
      AccessorBackingName := '__accessor_' + Elem.Name;

      if Assigned(Elem.FieldInitializer) then
        ClassValue.AddInstanceProperty(AccessorBackingName, Elem.FieldInitializer);

      ClassValue.AddAutoAccessor(Elem.Name, AccessorBackingName, Elem.IsStatic);
    end;
  end;

  // ES2022 §15.7.14: evaluate static fields and static blocks in source order
  for I := 0 to High(AClassDef.FElements) do
  begin
    Elem := AClassDef.FElements[I];
    if Elem.Kind = cekStaticBlock then
      ExecuteStaticBlock(Elem.StaticBlockBody, AContext, ClassValue)
    else if (Elem.Kind = cekField) and Elem.IsStatic then
    begin
      if Assigned(Elem.FieldInitializer) then
        PropertyValue := EvaluateExpression(Elem.FieldInitializer, AContext)
      else
        PropertyValue := TGocciaUndefinedLiteralValue.UndefinedValue;
      if Elem.IsPrivate then
        ClassValue.AddPrivateStaticProperty(Elem.Name, PropertyValue)
      else
        ClassValue.SetProperty(Elem.Name, PropertyValue);
    end;
  end;

  // TC39 proposal-decorators §3.2 ApplyDecoratorsToClassDefinition
  HasDecorators := Length(AClassDef.FDecorators) > 0;
  if not HasDecorators then
    for I := 0 to High(AClassDef.FElements) do
      if (Length(AClassDef.FElements[I].Decorators) > 0) or
         (AClassDef.FElements[I].Kind = cekAccessor) then
      begin
        HasDecorators := True;
        Break;
      end;

  if HasDecorators then
  begin
    // Create metadata object; inherit from superclass metadata if present
    SuperMetadata := nil;
    if Assigned(SuperClass) then
      SuperMetadata := SuperClass.GetSymbolProperty(TGocciaSymbolValue.WellKnownMetadata);
    if (SuperMetadata <> nil) and (SuperMetadata is TGocciaObjectValue) then
      MetadataObject := TGocciaObjectValue.Create(TGocciaObjectValue(SuperMetadata))
    else
      MetadataObject := TGocciaObjectValue.Create;
    TGarbageCollector.Instance.AddTempRoot(MetadataObject);

    MethodCollector := TGocciaInitializerCollector.Create;
    FieldCollector := TGocciaInitializerCollector.Create;
    StaticFieldCollector := TGocciaInitializerCollector.Create;
    ClassCollector := TGocciaInitializerCollector.Create;
    try

    // Phase 1: Evaluate all decorator expressions in source order
    SetLength(EvaluatedElementDecorators, Length(AClassDef.FElements));
    for I := 0 to High(AClassDef.FElements) do
    begin
      SetLength(EvaluatedElementDecorators[I], Length(AClassDef.FElements[I].Decorators));
      for J := 0 to High(AClassDef.FElements[I].Decorators) do
        EvaluatedElementDecorators[I][J] := EvaluateExpression(AClassDef.FElements[I].Decorators[J], AContext);
    end;

    SetLength(EvaluatedClassDecorators, Length(AClassDef.FDecorators));
    for I := 0 to High(AClassDef.FDecorators) do
      EvaluatedClassDecorators[I] := EvaluateExpression(AClassDef.FDecorators[I], AContext);

    // Phase 2: Call element decorators (applied per-element, bottom-up within each element)

    for I := 0 to High(AClassDef.FElements) do
    begin
      Elem := AClassDef.FElements[I];
      if Length(EvaluatedElementDecorators[I]) = 0 then
        Continue;

      for J := High(EvaluatedElementDecorators[I]) downto 0 do
      begin
        DecoratorFn := EvaluatedElementDecorators[I][J];
        if not DecoratorFn.IsCallable then
          ThrowTypeError(SErrorDecoratorMustBeFunction, SSuggestDecoratorFunction);

        // Build context object
        ContextObject := TGocciaObjectValue.Create;

        case Elem.Kind of
          cekMethod: ContextObject.AssignProperty(PROP_KIND, TGocciaStringLiteralValue.Create('method'));
          cekGetter: ContextObject.AssignProperty(PROP_KIND, TGocciaStringLiteralValue.Create('getter'));
          cekSetter: ContextObject.AssignProperty(PROP_KIND, TGocciaStringLiteralValue.Create('setter'));
          cekField: ContextObject.AssignProperty(PROP_KIND, TGocciaStringLiteralValue.Create('field'));
          cekAccessor: ContextObject.AssignProperty(PROP_KIND, TGocciaStringLiteralValue.Create('accessor'));
        end;

        if Elem.IsPrivate then
          ContextObject.AssignProperty(PROP_NAME, TGocciaStringLiteralValue.Create('#' + Elem.Name))
        else
          ContextObject.AssignProperty(PROP_NAME, TGocciaStringLiteralValue.Create(Elem.Name));

        if Elem.IsStatic then
          ContextObject.AssignProperty(PROP_STATIC, TGocciaBooleanLiteralValue.TrueValue)
        else
          ContextObject.AssignProperty(PROP_STATIC, TGocciaBooleanLiteralValue.FalseValue);
        if Elem.IsPrivate then
          ContextObject.AssignProperty(PROP_PRIVATE, TGocciaBooleanLiteralValue.TrueValue)
        else
          ContextObject.AssignProperty(PROP_PRIVATE, TGocciaBooleanLiteralValue.FalseValue);
        ContextObject.AssignProperty(PROP_METADATA, MetadataObject);

        // Build access object
        ElementName := Elem.Name;
        AccessObject := TGocciaObjectValue.Create;

        case Elem.Kind of
          cekMethod:
          begin
            if Elem.IsPrivate then
              CurrentMethod := ClassValue.GetPrivateMethod(ElementName)
            else if Elem.IsStatic then
              CurrentMethod := ClassValue.GetProperty(ElementName)
            else
              CurrentMethod := ClassValue.Prototype.GetProperty(ElementName);

            AccessGetterHelper := TGocciaAccessGetter.Create(CurrentMethod, ElementName);
            AccessObject.AssignProperty(PROP_GET,
              TGocciaNativeFunctionValue.CreateWithoutPrototype(AccessGetterHelper.Get, PROP_GET, 0));
            ContextObject.AssignProperty(PROP_ACCESS, AccessObject);
          end;
          cekField, cekAccessor:
          begin
            AccessGetterHelper := TGocciaAccessGetter.Create(nil, ElementName);
            AccessSetterHelper := TGocciaAccessSetter.Create(ElementName);
            AccessObject.AssignProperty(PROP_GET,
              TGocciaNativeFunctionValue.CreateWithoutPrototype(AccessGetterHelper.Get, PROP_GET, 0));
            AccessObject.AssignProperty(PROP_SET,
              TGocciaNativeFunctionValue.CreateWithoutPrototype(AccessSetterHelper.SetValue, PROP_SET, 1));
            ContextObject.AssignProperty(PROP_ACCESS, AccessObject);
          end;
        end;

        // addInitializer - use appropriate collector based on element kind
        if Elem.IsStatic then
          ContextObject.AssignProperty(PROP_ADD_INITIALIZER,
            TGocciaNativeFunctionValue.CreateWithoutPrototype(StaticFieldCollector.AddInitializer, PROP_ADD_INITIALIZER, 1))
        else if Elem.Kind in [cekField, cekAccessor] then
          ContextObject.AssignProperty(PROP_ADD_INITIALIZER,
            TGocciaNativeFunctionValue.CreateWithoutPrototype(FieldCollector.AddInitializer, PROP_ADD_INITIALIZER, 1))
        else
          ContextObject.AssignProperty(PROP_ADD_INITIALIZER,
            TGocciaNativeFunctionValue.CreateWithoutPrototype(MethodCollector.AddInitializer, PROP_ADD_INITIALIZER, 1));

        // Call the decorator
        DecoratorArgs := TGocciaArgumentsCollection.Create;
        try
          case Elem.Kind of
            cekMethod:
            begin
              if Elem.IsPrivate then
                DecoratorArgs.Add(ClassValue.GetPrivateMethod(ElementName))
              else if Elem.IsStatic then
                DecoratorArgs.Add(ClassValue.GetProperty(ElementName))
              else
                DecoratorArgs.Add(ClassValue.Prototype.GetProperty(ElementName));
              DecoratorArgs.Add(ContextObject);

              DecoratorResult := TGocciaFunctionBase(DecoratorFn).Call(DecoratorArgs, TGocciaUndefinedLiteralValue.UndefinedValue);

              if (DecoratorResult <> nil) and not (DecoratorResult is TGocciaUndefinedLiteralValue) then
              begin
                if not DecoratorResult.IsCallable then
                  AContext.OnError('Method decorator must return a function or undefined', ALine, AColumn);

                if Elem.IsPrivate then
                begin
                  if DecoratorResult is TGocciaMethodValue then
                    ClassValue.AddPrivateMethod(ElementName, TGocciaMethodValue(DecoratorResult));
                end
                else if Elem.IsStatic then
                  ClassValue.SetProperty(ElementName, DecoratorResult)
                else
                  ClassValue.Prototype.AssignProperty(ElementName, DecoratorResult);
              end;
            end;

            cekGetter:
            begin
              if Elem.IsPrivate then
                GetterFnValue := ClassValue.PrivatePropertyGetter[ElementName]
              else if Elem.IsStatic then
                GetterFnValue := ClassValue.StaticPropertyGetter[ElementName]
              else
                GetterFnValue := ClassValue.PropertyGetter[ElementName];

              DecoratorArgs.Add(GetterFnValue);
              DecoratorArgs.Add(ContextObject);

              DecoratorResult := TGocciaFunctionBase(DecoratorFn).Call(DecoratorArgs, TGocciaUndefinedLiteralValue.UndefinedValue);

              if (DecoratorResult <> nil) and not (DecoratorResult is TGocciaUndefinedLiteralValue) then
              begin
                if not DecoratorResult.IsCallable then
                  AContext.OnError('Getter decorator must return a function or undefined', ALine, AColumn);

                if Elem.IsPrivate then
                  ClassValue.AddPrivateGetter(ElementName, TGocciaFunctionValue(DecoratorResult))
                else if Elem.IsStatic then
                  ClassValue.AddStaticGetter(ElementName, TGocciaFunctionValue(DecoratorResult))
                else
                  ClassValue.AddGetter(ElementName, TGocciaFunctionValue(DecoratorResult));
              end;
            end;

            cekSetter:
            begin
              if Elem.IsPrivate then
                SetterFnValue := ClassValue.PrivatePropertySetter[ElementName]
              else if Elem.IsStatic then
                SetterFnValue := ClassValue.StaticPropertySetter[ElementName]
              else
                SetterFnValue := ClassValue.PropertySetter[ElementName];

              DecoratorArgs.Add(SetterFnValue);
              DecoratorArgs.Add(ContextObject);

              DecoratorResult := TGocciaFunctionBase(DecoratorFn).Call(DecoratorArgs, TGocciaUndefinedLiteralValue.UndefinedValue);

              if (DecoratorResult <> nil) and not (DecoratorResult is TGocciaUndefinedLiteralValue) then
              begin
                if not DecoratorResult.IsCallable then
                  AContext.OnError('Setter decorator must return a function or undefined', ALine, AColumn);

                if Elem.IsPrivate then
                  ClassValue.AddPrivateSetter(ElementName, TGocciaFunctionValue(DecoratorResult))
                else if Elem.IsStatic then
                  ClassValue.AddStaticSetter(ElementName, TGocciaFunctionValue(DecoratorResult))
                else
                  ClassValue.AddSetter(ElementName, TGocciaFunctionValue(DecoratorResult));
              end;
            end;

            cekField:
            begin
              DecoratorArgs.Add(TGocciaUndefinedLiteralValue.UndefinedValue);
              DecoratorArgs.Add(ContextObject);

              DecoratorResult := TGocciaFunctionBase(DecoratorFn).Call(DecoratorArgs, TGocciaUndefinedLiteralValue.UndefinedValue);

              if (DecoratorResult <> nil) and not (DecoratorResult is TGocciaUndefinedLiteralValue) then
              begin
                if not DecoratorResult.IsCallable then
                  AContext.OnError('Field decorator must return a function or undefined', ALine, AColumn);
                ClassValue.AddFieldInitializer(ElementName, DecoratorResult, Elem.IsPrivate, Elem.IsStatic);
              end;
            end;

            cekAccessor:
            begin
              AutoAccessorValue := TGocciaObjectValue.Create;
              AutoAccessorValue.AssignProperty(PROP_GET, ClassValue.PropertyGetter[ElementName]);
              AutoAccessorValue.AssignProperty(PROP_SET, ClassValue.PropertySetter[ElementName]);

              DecoratorArgs.Add(AutoAccessorValue);
              DecoratorArgs.Add(ContextObject);

              DecoratorResult := TGocciaFunctionBase(DecoratorFn).Call(DecoratorArgs, TGocciaUndefinedLiteralValue.UndefinedValue);

              if (DecoratorResult <> nil) and not (DecoratorResult is TGocciaUndefinedLiteralValue) then
              begin
                if not (DecoratorResult is TGocciaObjectValue) then
                  AContext.OnError('Accessor decorator must return an object or undefined', ALine, AColumn);
                DecResultObj := TGocciaObjectValue(DecoratorResult);
                NewGetter := DecResultObj.GetProperty(PROP_GET);
                NewSetter := DecResultObj.GetProperty(PROP_SET);
                NewInit := DecResultObj.GetProperty(PROP_INIT);

                if Assigned(NewGetter) and not (NewGetter is TGocciaUndefinedLiteralValue) then
                  ClassValue.AddGetter(ElementName, TGocciaFunctionValue(NewGetter));
                if Assigned(NewSetter) and not (NewSetter is TGocciaUndefinedLiteralValue) then
                  ClassValue.AddSetter(ElementName, TGocciaFunctionValue(NewSetter));
                if Assigned(NewInit) and not (NewInit is TGocciaUndefinedLiteralValue) and NewInit.IsCallable then
                  ClassValue.AddFieldInitializer(ElementName, NewInit, Elem.IsPrivate, Elem.IsStatic);
              end;
            end;
          end;
        finally
          DecoratorArgs.Free;
        end;
      end;
    end;

    // Phase 3: Call class decorators (bottom-up)
    for I := High(EvaluatedClassDecorators) downto 0 do
    begin
      DecoratorFn := EvaluatedClassDecorators[I];
      if not DecoratorFn.IsCallable then
        ThrowTypeError(SErrorDecoratorMustBeFunction, SSuggestDecoratorFunction);

      ContextObject := TGocciaObjectValue.Create;
      ContextObject.AssignProperty(PROP_KIND, TGocciaStringLiteralValue.Create('class'));
      if AClassDef.Name <> '' then
        ContextObject.AssignProperty(PROP_NAME, TGocciaStringLiteralValue.Create(AClassDef.Name))
      else
        ContextObject.AssignProperty(PROP_NAME, TGocciaUndefinedLiteralValue.UndefinedValue);
      ContextObject.AssignProperty(PROP_METADATA, MetadataObject);
      ContextObject.AssignProperty(PROP_ADD_INITIALIZER,
        TGocciaNativeFunctionValue.CreateWithoutPrototype(ClassCollector.AddInitializer, PROP_ADD_INITIALIZER, 1));

      DecoratorArgs := TGocciaArgumentsCollection.Create([ClassValue, ContextObject]);
      try
        DecoratorResult := TGocciaFunctionBase(DecoratorFn).Call(DecoratorArgs, TGocciaUndefinedLiteralValue.UndefinedValue);

        if (DecoratorResult <> nil) and not (DecoratorResult is TGocciaUndefinedLiteralValue) then
        begin
          if not (DecoratorResult is TGocciaClassValue) then
            AContext.OnError('Class decorator must return a class or undefined', ALine, AColumn);
          ClassValue := TGocciaClassValue(DecoratorResult);
        end;
      finally
        DecoratorArgs.Free;
      end;
    end;

    // Assign Symbol.metadata on the class
    ClassValue.DefineSymbolProperty(
      TGocciaSymbolValue.WellKnownMetadata,
      TGocciaPropertyDescriptorData.Create(MetadataObject, [pfConfigurable]));

    // Store initializer lists on the class for execution during instantiation
    InitializerResults := MethodCollector.GetInitializers;
    ClassValue.SetMethodInitializers(InitializerResults);
    InitializerResults := FieldCollector.GetInitializers;
    ClassValue.SetFieldInitializers(InitializerResults);

    // Run class-level initializers after static fields
    InitializerResults := ClassCollector.GetInitializers;
    for I := 0 to High(InitializerResults) do
    begin
      InitArgs := TGocciaArgumentsCollection.Create;
      try
        TGocciaFunctionBase(InitializerResults[I]).Call(InitArgs, ClassValue);
      finally
        InitArgs.Free;
      end;
    end;

    // Run static field initializers
    InitializerResults := StaticFieldCollector.GetInitializers;
    for I := 0 to High(InitializerResults) do
    begin
      InitArgs := TGocciaArgumentsCollection.Create;
      try
        TGocciaFunctionBase(InitializerResults[I]).Call(InitArgs, ClassValue);
      finally
        InitArgs.Free;
      end;
    end;

    finally
      TGarbageCollector.Instance.RemoveTempRoot(MetadataObject);
      MethodCollector.Free;
      FieldCollector.Free;
      StaticFieldCollector.Free;
      ClassCollector.Free;
    end;
  end;

  Result := ClassValue;
end;

function ResolveOwningClass(const AInstance: TGocciaInstanceValue; const AContext: TGocciaEvaluationContext): TGocciaClassValue;
var
  OwningClassValue: TGocciaValue;
begin
  // Walk the scope chain to find the owning class (set on TGocciaMethodCallScope or TGocciaClassInitScope)
  OwningClassValue := AContext.Scope.FindOwningClass;
  if (OwningClassValue is TGocciaClassValue) then
  begin
    Result := TGocciaClassValue(OwningClassValue);
    Exit;
  end;
  // Fallback: use the instance's class
  Result := AInstance.ClassValue;
end;

function ResolveLexicalOwningClass(
  const AContext: TGocciaEvaluationContext): TGocciaClassValue;
var
  OwningClassValue: TGocciaValue;
begin
  Result := nil;
  OwningClassValue := AContext.Scope.FindOwningClass;
  if OwningClassValue is TGocciaClassValue then
    Result := TGocciaClassValue(OwningClassValue);
end;

function CollectDeclaredPrivateNames(const AContext: TGocciaEvaluationContext): TStringList;
var
  OwningClassValue: TGocciaClassValue;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := True;
  Result.Sorted := False;
  Result.Duplicates := dupIgnore;

  OwningClassValue := ResolveLexicalOwningClass(AContext);
  if Assigned(OwningClassValue) then
    OwningClassValue.AppendOwnPrivateNames(Result);
end;

procedure ThrowPrivateGetterMissingError(const APrivateName: string);
begin
  ThrowTypeError(Format(SErrorPrivateGetterMissing,
    [APrivateName]), SSuggestPrivateFieldAccess);
end;

procedure ThrowPrivateSetterMissingError(const APrivateName: string);
begin
  ThrowTypeError(Format(SErrorPrivateSetterMissing,
    [APrivateName]), SSuggestPrivateFieldAccess);
end;

procedure ThrowPrivateBrandError(const APrivateName: string);
begin
  ThrowTypeError(Format(SErrorPrivateFieldInaccessible, [APrivateName]),
    SSuggestPrivateFieldAccess);
end;

procedure EnsurePrivateStaticBrand(const AReceiver,
  AAccessClass: TGocciaClassValue; const APrivateName: string);
begin
  if AReceiver <> AAccessClass then
    ThrowPrivateBrandError(APrivateName);
end;

// ES2026 §7.3.30 PrivateGet ( O, P )
function EvaluatePrivateMemberOnInstance(const AInstance: TGocciaInstanceValue; const APrivateName: string; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  AccessClass: TGocciaClassValue;
  GetterFn: TGocciaFunctionBase;
  EmptyArgs: TGocciaArgumentsCollection;
begin
  // Determine the access class from the owning class of the current method
  AccessClass := ResolveOwningClass(AInstance, AContext);

  // Check if this is a private getter
  if AccessClass.HasPrivateGetter(APrivateName) then
  begin
    GetterFn := AccessClass.PrivatePropertyGetter[APrivateName];
    EmptyArgs := TGocciaArgumentsCollection.Create;
    try
      Result := GetterFn.Call(EmptyArgs, AInstance);
    finally
      EmptyArgs.Free;
    end;
    Exit;
  end;

  if AccessClass.HasPrivateSetter(APrivateName) then
    ThrowPrivateGetterMissingError(APrivateName);

  // Check if this is a private method call
  if AInstance.ClassValue.PrivateMethods.ContainsKey(APrivateName) then
  begin
    Result := AInstance.ClassValue.GetPrivateMethod(APrivateName);
    if Result = nil then
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    // It's a private property access
    Result := AInstance.GetPrivateProperty(APrivateName, AccessClass);
  end;
end;

function EvaluatePrivateMemberOnClass(const AClassValue: TGocciaClassValue;
  const APrivateName: string; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  AccessClass: TGocciaClassValue;
  GetterFn: TGocciaFunctionBase;
  EmptyArgs: TGocciaArgumentsCollection;
begin
  AccessClass := ResolveLexicalOwningClass(AContext);
  if not Assigned(AccessClass) then
    AccessClass := AClassValue;

  EnsurePrivateStaticBrand(AClassValue, AccessClass, APrivateName);

  if AccessClass.HasOwnPrivateGetter(APrivateName) then
  begin
    GetterFn := AccessClass.GetOwnPrivatePropertyGetter(APrivateName);
    EmptyArgs := TGocciaArgumentsCollection.Create;
    try
      Result := GetterFn.Call(EmptyArgs, AClassValue);
    finally
      EmptyArgs.Free;
    end;
    Exit;
  end;

  if AccessClass.HasOwnPrivateSetter(APrivateName) then
    ThrowPrivateGetterMissingError(APrivateName);

  if not AccessClass.PrivateStaticProperties.TryGetValue(APrivateName, Result) then
    ThrowPrivateBrandError(APrivateName);
end;

procedure AssignPrivateMemberOnClass(const AClassValue: TGocciaClassValue;
  const APrivateName: string; const AValue: TGocciaValue;
  const AContext: TGocciaEvaluationContext);
var
  AccessClass: TGocciaClassValue;
  SetterFn: TGocciaFunctionBase;
  SetterArgs: TGocciaArgumentsCollection;
begin
  AccessClass := ResolveLexicalOwningClass(AContext);
  if not Assigned(AccessClass) then
    AccessClass := AClassValue;

  EnsurePrivateStaticBrand(AClassValue, AccessClass, APrivateName);

  if AccessClass.HasOwnPrivateSetter(APrivateName) then
  begin
    SetterFn := AccessClass.GetOwnPrivatePropertySetter(APrivateName);
    SetterArgs := TGocciaArgumentsCollection.Create;
    try
      SetterArgs.Add(AValue);
      SetterFn.Call(SetterArgs, AClassValue);
    finally
      SetterArgs.Free;
    end;
    Exit;
  end;

  if AccessClass.HasOwnPrivateGetter(APrivateName) then
    ThrowPrivateSetterMissingError(APrivateName);

  if not AccessClass.HasOwnPrivateStaticProperty(APrivateName) then
    ThrowPrivateBrandError(APrivateName);

  AccessClass.AddPrivateStaticProperty(APrivateName, AValue);
end;

function EvaluatePrivateMember(const APrivateMemberExpression: TGocciaPrivateMemberExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  ObjectValue: TGocciaValue;
  Instance: TGocciaInstanceValue;
  ClassValue: TGocciaClassValue;
begin
  // Evaluate the object expression
  ObjectValue := EvaluateExpression(APrivateMemberExpression.ObjectExpr, AContext);

  if ObjectValue is TGocciaInstanceValue then
  begin
    Instance := TGocciaInstanceValue(ObjectValue);
    Result := EvaluatePrivateMemberOnInstance(Instance, APrivateMemberExpression.PrivateName, AContext);
  end
  else if ObjectValue is TGocciaClassValue then
  begin
    ClassValue := TGocciaClassValue(ObjectValue);
    Result := EvaluatePrivateMemberOnClass(
      ClassValue, APrivateMemberExpression.PrivateName, AContext);
  end
  else
  begin
    AContext.OnError(Format('Private fields can only be accessed on class instances or classes, not %s', [ObjectValue.TypeName]),
      APrivateMemberExpression.Line, APrivateMemberExpression.Column);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function EvaluatePrivateMember(const APrivateMemberExpression: TGocciaPrivateMemberExpression; const AContext: TGocciaEvaluationContext; out AObjectValue: TGocciaValue): TGocciaValue;
var
  Instance: TGocciaInstanceValue;
  ClassValue: TGocciaClassValue;
begin
  // Evaluate the object expression and store it for this binding
  AObjectValue := EvaluateExpression(APrivateMemberExpression.ObjectExpr, AContext);

  if AObjectValue is TGocciaInstanceValue then
  begin
    Instance := TGocciaInstanceValue(AObjectValue);
    Result := EvaluatePrivateMemberOnInstance(Instance, APrivateMemberExpression.PrivateName, AContext);
  end
  else if AObjectValue is TGocciaClassValue then
  begin
    ClassValue := TGocciaClassValue(AObjectValue);
    Result := EvaluatePrivateMemberOnClass(
      ClassValue, APrivateMemberExpression.PrivateName, AContext);
  end
  else
  begin
    AContext.OnError(Format('Private fields can only be accessed on class instances or classes, not %s', [AObjectValue.TypeName]),
      APrivateMemberExpression.Line, APrivateMemberExpression.Column);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

// ES2026 §7.3.31 PrivateSet ( O, P, value )
function EvaluatePrivatePropertyAssignment(const APrivatePropertyAssignmentExpression: TGocciaPrivatePropertyAssignmentExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  ObjectValue: TGocciaValue;
  Instance: TGocciaInstanceValue;
  ClassValue: TGocciaClassValue;
  AccessClass: TGocciaClassValue;
  Value: TGocciaValue;
  SetterFn: TGocciaFunctionBase;
  SetterArgs: TGocciaArgumentsCollection;
begin
  // Evaluate the object expression
  ObjectValue := EvaluateExpression(APrivatePropertyAssignmentExpression.ObjectExpr, AContext);

  // Evaluate the value to assign
  Value := EvaluateExpression(APrivatePropertyAssignmentExpression.Value, AContext);

  if ObjectValue is TGocciaInstanceValue then
  begin
    // Private field assignment on instance
    Instance := TGocciaInstanceValue(ObjectValue);

    // Determine the access class from the owning class of the current method
    AccessClass := ResolveOwningClass(Instance, AContext);

    // Check if this is a private setter
    if AccessClass.HasPrivateSetter(APrivatePropertyAssignmentExpression.PrivateName) then
    begin
      SetterFn := AccessClass.PrivatePropertySetter[APrivatePropertyAssignmentExpression.PrivateName];
      SetterArgs := TGocciaArgumentsCollection.Create;
      try
        SetterArgs.Add(Value);
        SetterFn.Call(SetterArgs, Instance);
      finally
        SetterArgs.Free;
      end;
    end
    else if AccessClass.HasPrivateGetter(APrivatePropertyAssignmentExpression.PrivateName) then
      ThrowPrivateSetterMissingError(APrivatePropertyAssignmentExpression.PrivateName)
    else
    begin
      // Set the private property
      Instance.SetPrivateProperty(APrivatePropertyAssignmentExpression.PrivateName, Value, AccessClass);
    end;
  end
  else if ObjectValue is TGocciaClassValue then
  begin
    ClassValue := TGocciaClassValue(ObjectValue);
    AssignPrivateMemberOnClass(
      ClassValue, APrivatePropertyAssignmentExpression.PrivateName, Value,
      AContext);
  end
  else
  begin
    AContext.OnError(Format('Private fields can only be assigned on class instances or classes, not %s', [ObjectValue.TypeName]),
      APrivatePropertyAssignmentExpression.Line, APrivatePropertyAssignmentExpression.Column);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  Result := Value;
end;

// ES2026 §13.15.2 AssignmentExpression : LeftHandSideExpression ??= AssignmentExpression
function EvaluatePrivatePropertyCompoundAssignment(const APrivatePropertyCompoundAssignmentExpression: TGocciaPrivatePropertyCompoundAssignmentExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  ObjectValue: TGocciaValue;
  Instance: TGocciaInstanceValue;
  ClassValue: TGocciaClassValue;
  AccessClass: TGocciaClassValue;
  CurrentValue: TGocciaValue;
  Value: TGocciaValue;
  SetterFn: TGocciaFunctionBase;
  SetterArgs: TGocciaArgumentsCollection;
begin
  // Evaluate the object expression
  ObjectValue := EvaluateExpression(APrivatePropertyCompoundAssignmentExpression.ObjectExpr, AContext);

  if ObjectValue is TGocciaInstanceValue then
  begin
    Instance := TGocciaInstanceValue(ObjectValue);
    AccessClass := ResolveOwningClass(Instance, AContext);
    CurrentValue := EvaluatePrivateMemberOnInstance(
      Instance, APrivatePropertyCompoundAssignmentExpression.PrivateName, AContext);
  end
  else if ObjectValue is TGocciaClassValue then
  begin
    ClassValue := TGocciaClassValue(ObjectValue);
    CurrentValue := EvaluatePrivateMemberOnClass(
      ClassValue, APrivatePropertyCompoundAssignmentExpression.PrivateName,
      AContext);
  end
  else
  begin
    AContext.OnError(Format('Private fields can only be accessed on class instances or classes, not %s', [ObjectValue.TypeName]),
      APrivatePropertyCompoundAssignmentExpression.Line, APrivatePropertyCompoundAssignmentExpression.Column);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  // ES2026 §13.15.2 step 3: short-circuit logical/nullish assignment
  if APrivatePropertyCompoundAssignmentExpression.Operator in
     [gttNullishCoalescingAssign, gttLogicalAndAssign, gttLogicalOrAssign] then
  begin
    case APrivatePropertyCompoundAssignmentExpression.Operator of
      gttNullishCoalescingAssign:
        if not ((CurrentValue is TGocciaUndefinedLiteralValue) or
                (CurrentValue is TGocciaNullLiteralValue)) then
        begin
          Result := CurrentValue;
          Exit;
        end;
      gttLogicalAndAssign:
        if not CurrentValue.ToBooleanLiteral.Value then
        begin
          Result := CurrentValue;
          Exit;
        end;
      gttLogicalOrAssign:
        if CurrentValue.ToBooleanLiteral.Value then
        begin
          Result := CurrentValue;
          Exit;
        end;
    end;

    Value := EvaluateExpression(APrivatePropertyCompoundAssignmentExpression.Value, AContext);
    Result := Value;

    if ObjectValue is TGocciaInstanceValue then
    begin
      if AccessClass.HasPrivateSetter(APrivatePropertyCompoundAssignmentExpression.PrivateName) then
      begin
        SetterFn := AccessClass.PrivatePropertySetter[APrivatePropertyCompoundAssignmentExpression.PrivateName];
        SetterArgs := TGocciaArgumentsCollection.Create;
        try
          SetterArgs.Add(Value);
          SetterFn.Call(SetterArgs, Instance);
        finally
          SetterArgs.Free;
        end;
      end
      else if AccessClass.HasPrivateGetter(APrivatePropertyCompoundAssignmentExpression.PrivateName) then
        ThrowPrivateSetterMissingError(APrivatePropertyCompoundAssignmentExpression.PrivateName)
      else
        Instance.SetPrivateProperty(APrivatePropertyCompoundAssignmentExpression.PrivateName, Result, AccessClass);
    end
    else if ObjectValue is TGocciaClassValue then
      AssignPrivateMemberOnClass(
        ClassValue, APrivatePropertyCompoundAssignmentExpression.PrivateName,
        Result, AContext);
    Exit;
  end;

  // Evaluate the value to operate with
  Value := EvaluateExpression(APrivatePropertyCompoundAssignmentExpression.Value, AContext);

  // Use shared compound operation function
  Result := PerformCompoundOperation(CurrentValue, Value, APrivatePropertyCompoundAssignmentExpression.Operator);

  // Set the new value
  if ObjectValue is TGocciaInstanceValue then
  begin
    if AccessClass.HasPrivateSetter(APrivatePropertyCompoundAssignmentExpression.PrivateName) then
    begin
      SetterFn := AccessClass.PrivatePropertySetter[
        APrivatePropertyCompoundAssignmentExpression.PrivateName];
      SetterArgs := TGocciaArgumentsCollection.Create;
      try
        SetterArgs.Add(Result);
        SetterFn.Call(SetterArgs, Instance);
      finally
        SetterArgs.Free;
      end;
    end
    else if AccessClass.HasPrivateGetter(APrivatePropertyCompoundAssignmentExpression.PrivateName) then
      ThrowPrivateSetterMissingError(APrivatePropertyCompoundAssignmentExpression.PrivateName)
    else
      Instance.SetPrivateProperty(
        APrivatePropertyCompoundAssignmentExpression.PrivateName, Result, AccessClass);
  end
  else if ObjectValue is TGocciaClassValue then
    AssignPrivateMemberOnClass(
      ClassValue, APrivatePropertyCompoundAssignmentExpression.PrivateName,
      Result, AContext);
end;

procedure InitializePrivateInstanceProperties(const AInstance: TGocciaInstanceValue; const AClassValue: TGocciaClassValue; const AContext: TGocciaEvaluationContext);
var
  PropertyValue: TGocciaValue;
  Entry: TGocciaExpressionMap.TKeyValuePair;
  I: Integer;
begin
  for I := 0 to AClassValue.PrivateInstancePropertyDefs.Count - 1 do
  begin
    Entry := AClassValue.PrivateInstancePropertyDefs.EntryAt(I);
    PropertyValue := EvaluateExpression(Entry.Value, AContext);
    AInstance.SetPrivateProperty(Entry.Key, PropertyValue, AClassValue);
  end;
end;

function InstantiateClass(const AClassValue: TGocciaClassValue;
  const AArguments: TGocciaArgumentsCollection;
  const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Instance: TGocciaObjectValue;
  WalkClass: TGocciaClassValue;
  NativeInstance: TGocciaObjectValue;
  InitContext, SuperInitContext: TGocciaEvaluationContext;
  InitScope, SuperInitScope: TGocciaScope;
begin
  CheckExecutionTimeout;
  IncrementInstructionCounter;
  CheckInstructionLimit;
  NativeInstance := nil;
  WalkClass := AClassValue;
  while Assigned(WalkClass) do
  begin
    CheckExecutionTimeout;
    IncrementInstructionCounter;
    CheckInstructionLimit;
    NativeInstance := WalkClass.CreateNativeInstance(AArguments);
    if Assigned(NativeInstance) then
      Break;
    WalkClass := WalkClass.SuperClass;
  end;

  if Assigned(NativeInstance) then
  begin
    Instance := NativeInstance;
    Instance.Prototype := AClassValue.Prototype;
    if NativeInstance is TGocciaInstanceValue then
      TGocciaInstanceValue(NativeInstance).ClassValue := AClassValue;
  end
  else
  begin
    Instance := TGocciaInstanceValue.Create(AClassValue);
    Instance.Prototype := AClassValue.Prototype;
  end;

  TGarbageCollector.Instance.AddTempRoot(Instance);
  try
    InitContext := AContext;
    InitScope := TGocciaClassInitScope.Create(AContext.Scope, AClassValue);
    InitScope.ThisValue := Instance;
    InitContext.Scope := InitScope;

    if Instance is TGocciaInstanceValue then
    begin
      InitializeInstanceProperties(TGocciaInstanceValue(Instance), AClassValue, InitContext);

      if AClassValue.FieldOrderCount = 0 then
      begin
        WalkClass := AClassValue.SuperClass;
        while Assigned(WalkClass) do
        begin
          CheckExecutionTimeout;
          IncrementInstructionCounter;
          CheckInstructionLimit;
          SuperInitContext := AContext;
          SuperInitScope := TGocciaClassInitScope.Create(AContext.Scope, WalkClass);
          SuperInitScope.ThisValue := Instance;
          SuperInitContext.Scope := SuperInitScope;
          if WalkClass.FieldOrderCount = 0 then
            InitializePrivateInstanceProperties(TGocciaInstanceValue(Instance), WalkClass, SuperInitContext);
          WalkClass := WalkClass.SuperClass;
        end;

        InitializePrivateInstanceProperties(TGocciaInstanceValue(Instance), AClassValue, InitContext);
      end;
    end;

    AClassValue.RunMethodInitializers(Instance);
    AClassValue.RunFieldInitializers(Instance);
    AClassValue.RunDecoratorFieldInitializers(Instance);

    if Assigned(AClassValue.ConstructorMethod) then
      AClassValue.ConstructorMethod.Call(AArguments, Instance)
    else if Assigned(AClassValue.SuperClass) and Assigned(AClassValue.SuperClass.ConstructorMethod) then
      AClassValue.SuperClass.ConstructorMethod.Call(AArguments, Instance)
    else if Assigned(NativeInstance) and (NativeInstance is TGocciaInstanceValue) then
      TGocciaInstanceValue(NativeInstance).InitializeNativeFromArguments(AArguments);
  finally
    TGarbageCollector.Instance.RemoveTempRoot(Instance);
  end;

  Result := Instance;
end;

// Template literals without real interpolations are returned as static strings.
// The parser pre-segments templates with interpolations into
// TGocciaTemplateWithInterpolationExpression, so this function only handles
// the no-interpolation case.
function EvaluateTemplateLiteral(const ATemplateLiteralExpression: TGocciaTemplateLiteralExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(ATemplateLiteralExpression.Value);
end;

// ES2026 §13.2.8 Template Literals — evaluate a pre-segmented template with
// interpolation expressions. Parts alternate between string literal nodes
// (static text) and expression nodes (interpolated values).
function EvaluateTemplateWithInterpolation(const ATemplateWithInterpolationExpression: TGocciaTemplateWithInterpolationExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  SB: TStringBuffer;
  I: Integer;
  PartValue: TGocciaValue;
begin
  SB := TStringBuffer.Create;
  for I := 0 to ATemplateWithInterpolationExpression.Parts.Count - 1 do
  begin
    PartValue := EvaluateExpression(ATemplateWithInterpolationExpression.Parts[I], AContext);
    if PartValue = nil then
      SB.Append('undefined')
    else
    begin
      if PartValue is TGocciaSymbolValue then
        ThrowTypeError(SErrorSymbolToString, SSuggestSymbolNoImplicitConversion);
      // ES2026 §13.15.5.1 step 5e: ToString(value) on each substitution
      SB.Append(ToECMAString(PartValue).Value);
    end;
  end;
  Result := TGocciaStringLiteralValue.Create(SB.ToString);
end;

// ES2026 §13.3.11 Runtime Semantics: Evaluation — Tagged Templates
function EvaluateTaggedTemplate(const ATaggedTemplateExpression: TGocciaTaggedTemplateExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Callee, ThisValue, ExprValue, TemplateObject: TGocciaValue;
  MemberExpr: TGocciaMemberExpression;
  CookedArray, RawArray: TGocciaArrayValue;
  Arguments: TGocciaArgumentsCollection;
  I: Integer;
  CalleeName: string;
begin
  CheckExecutionTimeout;
  IncrementInstructionCounter;
  CheckInstructionLimit;

  // ES2026 §13.3.11 step 1: Evaluate the tag expression
  if ATaggedTemplateExpression.Tag is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(ATaggedTemplateExpression.Tag);
    Callee := EvaluateMember(MemberExpr, AContext, ThisValue);
  end
  else
  begin
    Callee := EvaluateExpression(ATaggedTemplateExpression.Tag, AContext);
    ThisValue := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;

  // ES2026 §13.2.8.3 GetTemplateObject — return the cached template object for
  // this Parse Node, or build it on first evaluation and pin it for reuse.
  if Assigned(ATaggedTemplateExpression.TemplateObject) then
    TemplateObject := ATaggedTemplateExpression.TemplateObject
  else
  begin
    // Build the raw array
    RawArray := TGocciaArrayValue.Create;
    TGarbageCollector.Instance.AddTempRoot(RawArray);
    try
      for I := 0 to Length(ATaggedTemplateExpression.RawStrings) - 1 do
        RawArray.Elements.Add(TGocciaStringLiteralValue.Create(ATaggedTemplateExpression.RawStrings[I]));
      RawArray.Freeze;

      // Build the cooked array (the template object)
      CookedArray := TGocciaArrayValue.Create;
      TGarbageCollector.Instance.AddTempRoot(CookedArray);
      try
        // TC39 Template Literal Revision: segments with malformed escapes get
        // cooked=undefined; valid segments get the resolved string value.
        for I := 0 to Length(ATaggedTemplateExpression.CookedStrings) - 1 do
        begin
          if ATaggedTemplateExpression.CookedValid[I] then
            CookedArray.Elements.Add(TGocciaStringLiteralValue.Create(ATaggedTemplateExpression.CookedStrings[I]))
          else
            CookedArray.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue);
        end;
        // ES2026 §13.2.8.3 step 8: Define raw as non-enumerable, non-writable, non-configurable
        CookedArray.DefineProperty(PROP_RAW,
          TGocciaPropertyDescriptorData.Create(RawArray, []));
        // ES2026 §13.2.8.3 step 12: Freeze the template object
        CookedArray.Freeze;
        // ES2026 §13.2.8.3 step 13: Store in realm [[TemplateMap]] keyed by this
        // Parse Node so subsequent evaluations return the identical object.
        ATaggedTemplateExpression.SetCachedTemplateObject(CookedArray);
        TemplateObject := CookedArray;
      finally
        TGarbageCollector.Instance.RemoveTempRoot(CookedArray);
      end;
    finally
      TGarbageCollector.Instance.RemoveTempRoot(RawArray);
    end;
  end;

  // ES2026 §13.3.11 step 3: Evaluate substitution expressions and build arguments
  Arguments := TGocciaArgumentsCollection.Create;
  try
    Arguments.Add(TemplateObject);
    for I := 0 to ATaggedTemplateExpression.Expressions.Count - 1 do
    begin
      ExprValue := EvaluateExpression(ATaggedTemplateExpression.Expressions[I], AContext);
      Arguments.Add(ExprValue);
    end;

    // ES2026 §13.3.11 step 4: Call the tag function
    if Callee is TGocciaNativeFunctionValue then
      CalleeName := TGocciaNativeFunctionValue(Callee).Name
    else if Callee is TGocciaFunctionValue then
      CalleeName := TGocciaFunctionValue(Callee).Name
    else
      CalleeName := '';

    if Assigned(TGocciaCallStack.Instance) then
      TGocciaCallStack.Instance.Push(CalleeName, AContext.CurrentFilePath,
        ATaggedTemplateExpression.Line, ATaggedTemplateExpression.Column);
    try
      if Callee is TGocciaProxyValue then
        Result := TGocciaProxyValue(Callee).ApplyTrap(Arguments, ThisValue)
      else if Callee is TGocciaNativeFunctionValue then
        Result := TGocciaNativeFunctionValue(Callee).Call(Arguments, ThisValue)
      else if Callee is TGocciaFunctionValue then
        Result := TGocciaFunctionValue(Callee).Call(Arguments, ThisValue)
      else if Callee is TGocciaBoundFunctionValue then
        Result := TGocciaBoundFunctionValue(Callee).Call(Arguments, ThisValue)
      else if Callee is TGocciaFunctionBase then
        Result := TGocciaFunctionBase(Callee).Call(Arguments, ThisValue)
      else
        ThrowTypeError(
          Format(SErrorValueNotFunction, [Callee.TypeName]),
          SSuggestTaggedTemplateCallable);
    finally
      if Assigned(TGocciaCallStack.Instance) then
        TGocciaCallStack.Instance.Pop;
    end;
  finally
    Arguments.Free;
  end;
end;

// Lightweight template expression evaluator - handles 95% of common cases without full parsing
function EvaluateTemplateExpression(const AExpressionText: string; const AContext: TGocciaEvaluationContext; const ALine, AColumn: Integer): TGocciaValue;
var
  Trimmed: string;
  PlusPos, MinusPos, StarPos, SlashPos, DotPos: Integer;
  Left, Right, PropName: string;
  LeftVal, RightVal, ObjVal: TGocciaValue;
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  Expression: TGocciaExpression;
  Tokens: TObjectList<TGocciaToken>;
  SourceLines: TStringList;
  DeclaredPrivateNames: TStringList;
  I: Integer;
  IsSimpleIdentifier: Boolean;
begin
  Trimmed := Trim(AExpressionText);

  // Handle empty expression
  if Trimmed = '' then
  begin
    Result := TGocciaStringLiteralValue.Create('');
    Exit;
  end;

  // Check for complex expressions that need full parsing
  if (Pos('"', Trimmed) > 0) or (Pos('''', Trimmed) > 0) or  // String literals
     (Pos('(', Trimmed) > 0) or (Pos(')', Trimmed) > 0) or   // Parentheses
     (Pos('?', Trimmed) > 0) or (Pos(':', Trimmed) > 0) or   // Ternary operator
     (Pos('[', Trimmed) > 0) or (Pos(']', Trimmed) > 0) or   // Array access
     (Pos('#', Trimmed) > 0) or                               // Private field access
     (Pos('>=', Trimmed) > 0) or (Pos('<=', Trimmed) > 0) or // Comparison operators
     (Pos('==', Trimmed) > 0) or (Pos('!=', Trimmed) > 0) or
     (Pos('>', Trimmed) > 0) or (Pos('<', Trimmed) > 0) then
  begin
    // Complex expression - use full parser
  end
  else
  begin
    // Try simple variable access first (most common case)
    // Check if it's a valid identifier (letters, digits, underscore, dollar)
    IsSimpleIdentifier := True;
    if Length(Trimmed) > 0 then
    begin
      if not (Trimmed[1] in ['a'..'z', 'A'..'Z', '_', '$']) then
        IsSimpleIdentifier := False
      else
      begin
        for I := 2 to Length(Trimmed) do
        begin
          if not (Trimmed[I] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '$']) then
          begin
            IsSimpleIdentifier := False;
            Break;
          end;
        end;
      end;
    end
    else
      IsSimpleIdentifier := False;

    if IsSimpleIdentifier then
    begin
      // Handle keyword literals before scope lookup
      if Trimmed = KEYWORD_TRUE then
      begin
        Result := TGocciaBooleanLiteralValue.TrueValue;
        Exit;
      end
      else if Trimmed = KEYWORD_FALSE then
      begin
        Result := TGocciaBooleanLiteralValue.FalseValue;
        Exit;
      end
      else if Trimmed = KEYWORD_NULL then
      begin
        Result := TGocciaNullLiteralValue.NullValue;
        Exit;
      end
      else if Trimmed = NAN_LITERAL then
      begin
        Result := TGocciaNumberLiteralValue.NaNValue;
        Exit;
      end
      else if Trimmed = INFINITY_LITERAL then
      begin
        Result := TGocciaNumberLiteralValue.InfinityValue;
        Exit;
      end;

      Result := AContext.Scope.ResolveIdentifier(Trimmed);
      if Result = nil then
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;

    // Try simple property access (obj.prop)
    DotPos := Pos('.', Trimmed);
    if (DotPos > 1) and (DotPos < Length(Trimmed)) and
       (Pos(' ', Trimmed) = 0) and (Pos('+', Trimmed) = 0) and
       (Pos('-', Trimmed) = 0) and (Pos('*', Trimmed) = 0) and
       (Pos('/', Trimmed) = 0) then
    begin
      Left := Copy(Trimmed, 1, DotPos - 1);
      PropName := Copy(Trimmed, DotPos + 1, Length(Trimmed));

      ObjVal := AContext.Scope.ResolveIdentifier(Left);
      if (ObjVal <> nil) and (ObjVal is TGocciaObjectValue) then
      begin
        Result := TGocciaObjectValue(ObjVal).GetProperty(PropName);
        Exit;
      end;
    end;

    // Try simple binary arithmetic (x + y, a - b, etc.) - ONLY if it looks simple
    PlusPos := Pos(' + ', Trimmed);
    MinusPos := Pos(' - ', Trimmed);
    StarPos := Pos(' * ', Trimmed);
    SlashPos := Pos(' / ', Trimmed);

    // Only handle binary operations if there's exactly one operator and no other complexity
    if (PlusPos > 0) and (PlusPos < Length(Trimmed) - 2) and
       (Pos(' + ', Copy(Trimmed, PlusPos + 3, Length(Trimmed))) = 0) then // No second +
    begin
      Left := Trim(Copy(Trimmed, 1, PlusPos - 1));
      Right := Trim(Copy(Trimmed, PlusPos + 3, Length(Trimmed)));

      // Only proceed if both sides are simple identifiers
      if (Left <> '') and (Right <> '') and
         (Pos(' ', Left) = 0) and (Pos(' ', Right) = 0) and
         (Pos('.', Left) = 0) and (Pos('.', Right) = 0) then
      begin
        LeftVal := AContext.Scope.GetValue(Left);
        RightVal := AContext.Scope.GetValue(Right);

        if (LeftVal <> nil) and (RightVal <> nil) then
        begin
          if (LeftVal is TGocciaSymbolValue) or (RightVal is TGocciaSymbolValue) then
            ThrowTypeError(SErrorSymbolToString, SSuggestSymbolNoImplicitConversion);
          if (LeftVal is TGocciaStringLiteralValue) or (RightVal is TGocciaStringLiteralValue) then
            Result := TGocciaStringLiteralValue.Create(LeftVal.ToStringLiteral.Value + RightVal.ToStringLiteral.Value)
          else
            Result := TGocciaNumberLiteralValue.Create(LeftVal.ToNumberLiteral.Value + RightVal.ToNumberLiteral.Value);
          Exit;
        end;
      end;
    end;

    if (MinusPos > 0) and (MinusPos < Length(Trimmed) - 2) and
       (Pos(' - ', Copy(Trimmed, MinusPos + 3, Length(Trimmed))) = 0) then // No second -
    begin
      Left := Trim(Copy(Trimmed, 1, MinusPos - 1));
      Right := Trim(Copy(Trimmed, MinusPos + 3, Length(Trimmed)));

      if (Left <> '') and (Right <> '') and
         (Pos(' ', Left) = 0) and (Pos(' ', Right) = 0) and
         (Pos('.', Left) = 0) and (Pos('.', Right) = 0) then
      begin
        LeftVal := AContext.Scope.GetValue(Left);
        RightVal := AContext.Scope.GetValue(Right);

        if (LeftVal <> nil) and (RightVal <> nil) then
        begin
          Result := TGocciaNumberLiteralValue.Create(LeftVal.ToNumberLiteral.Value - RightVal.ToNumberLiteral.Value);
          Exit;
        end;
      end;
    end;

    if (StarPos > 0) and (StarPos < Length(Trimmed) - 2) and
       (Pos(' * ', Copy(Trimmed, StarPos + 3, Length(Trimmed))) = 0) then // No second *
    begin
      Left := Trim(Copy(Trimmed, 1, StarPos - 1));
      Right := Trim(Copy(Trimmed, StarPos + 3, Length(Trimmed)));

      if (Left <> '') and (Right <> '') and
         (Pos(' ', Left) = 0) and (Pos(' ', Right) = 0) and
         (Pos('.', Left) = 0) and (Pos('.', Right) = 0) then
      begin
        LeftVal := AContext.Scope.GetValue(Left);
        RightVal := AContext.Scope.GetValue(Right);

        if (LeftVal <> nil) and (RightVal <> nil) then
        begin
          Result := TGocciaNumberLiteralValue.Create(LeftVal.ToNumberLiteral.Value * RightVal.ToNumberLiteral.Value);
          Exit;
        end;
      end;
    end;

    if (SlashPos > 0) and (SlashPos < Length(Trimmed) - 2) and
       (Pos(' / ', Copy(Trimmed, SlashPos + 3, Length(Trimmed))) = 0) then // No second /
    begin
      Left := Trim(Copy(Trimmed, 1, SlashPos - 1));
      Right := Trim(Copy(Trimmed, SlashPos + 3, Length(Trimmed)));

      if (Left <> '') and (Right <> '') and
         (Pos(' ', Left) = 0) and (Pos(' ', Right) = 0) and
         (Pos('.', Left) = 0) and (Pos('.', Right) = 0) then
      begin
        LeftVal := AContext.Scope.GetValue(Left);
        RightVal := AContext.Scope.GetValue(Right);

        if (LeftVal <> nil) and (RightVal <> nil) then
        begin
          if RightVal.ToNumberLiteral.Value = 0 then
            Result := TGocciaNumberLiteralValue.InfinityValue
          else
            Result := TGocciaNumberLiteralValue.Create(LeftVal.ToNumberLiteral.Value / RightVal.ToNumberLiteral.Value);
          Exit;
        end;
      end;
    end;
  end;

  // Fall back to full parsing for complex expressions
  // This handles complex cases like nested expressions, function calls, string literals, etc.
  Lexer := nil;
  Parser := nil;
  Tokens := nil;
  SourceLines := nil;
  Expression := nil;
  DeclaredPrivateNames := nil;

  try
    SourceLines := TStringList.Create;
    SourceLines.Add(AExpressionText);

    Lexer := TGocciaLexer.Create(AExpressionText, 'template-expression');
    Tokens := Lexer.ScanTokens;
    if Tokens = nil then
    begin
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;

    Parser := TGocciaParser.Create(Tokens, 'template-expression', SourceLines);
    Tokens := nil; // Parser owns it

    if Pos('#', Trimmed) > 0 then
    begin
      DeclaredPrivateNames := CollectDeclaredPrivateNames(AContext);
      Expression := Parser.ParseExpressionWithPrivateNames(DeclaredPrivateNames);
    end
    else
      Expression := Parser.Expression;

    if Expression = nil then
    begin
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;

    Result := EvaluateExpression(Expression, AContext);

  except
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;

  // Cleanup
  if Assigned(Expression) then Expression.Free;
  if Assigned(Parser) then Parser.Free;
  if Assigned(Tokens) then Tokens.Free;
  if Assigned(Lexer) then Lexer.Free;
  if Assigned(SourceLines) then SourceLines.Free;
  if Assigned(DeclaredPrivateNames) then DeclaredPrivateNames.Free;
end;

function EvaluateDestructuringAssignment(const ADestructuringAssignmentExpression: TGocciaDestructuringAssignmentExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Value: TGocciaValue;
begin
  // Evaluate the right-hand side
  Value := EvaluateExpression(ADestructuringAssignmentExpression.Right, AContext);

  // Apply the destructuring pattern
  AssignPattern(ADestructuringAssignmentExpression.Left, Value, AContext);

  Result := Value;
end;

function EvaluateDestructuringDeclaration(const ADestructuringDeclaration: TGocciaDestructuringDeclaration; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Value: TGocciaValue;
begin
  // Evaluate the initializer
  Value := EvaluateExpression(ADestructuringDeclaration.Initializer, AContext);

  // Apply the destructuring pattern to declare variables
  if ADestructuringDeclaration.IsConst then
    AssignPattern(ADestructuringDeclaration.Pattern, Value, AContext, True, dtConst)
  else
    AssignPattern(ADestructuringDeclaration.Pattern, Value, AContext, True, dtLet);

  Result := Value;
end;

procedure AssignPattern(const APattern: TGocciaDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
begin
  if APattern is TGocciaIdentifierDestructuringPattern then
    AssignIdentifierPattern(TGocciaIdentifierDestructuringPattern(APattern), AValue, AContext, AIsDeclaration, ADeclarationType)
  else if APattern is TGocciaArrayDestructuringPattern then
    AssignArrayPattern(TGocciaArrayDestructuringPattern(APattern), AValue, AContext, AIsDeclaration, ADeclarationType)
  else if APattern is TGocciaObjectDestructuringPattern then
    AssignObjectPattern(TGocciaObjectDestructuringPattern(APattern), AValue, AContext, AIsDeclaration, ADeclarationType)
  else if APattern is TGocciaAssignmentDestructuringPattern then
    AssignAssignmentPattern(TGocciaAssignmentDestructuringPattern(APattern), AValue, AContext, AIsDeclaration, ADeclarationType)
  else if APattern is TGocciaRestDestructuringPattern then
    AssignRestPattern(TGocciaRestDestructuringPattern(APattern), AValue, AContext, AIsDeclaration, ADeclarationType);
end;

procedure AssignIdentifierPattern(const APattern: TGocciaIdentifierDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
begin
  if AIsDeclaration then
    AContext.Scope.DefineLexicalBinding(APattern.Name, AValue, ADeclarationType)
  else
    AContext.Scope.AssignLexicalBinding(APattern.Name, AValue);
end;

procedure AssignArrayPattern(const APattern: TGocciaArrayDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
var
  ArrayValue: TGocciaArrayValue;
  Iterator: TGocciaIteratorValue;
  IterResult: TGocciaObjectValue;
  I: Integer;
  ElementValue: TGocciaValue;
  RestElements: TGocciaArrayValue;
  J: Integer;
begin
  if (AValue is TGocciaNullLiteralValue) or (AValue is TGocciaUndefinedLiteralValue) then
    ThrowTypeError(
      Format(SErrorCannotDestructure, [AValue.ToStringLiteral.Value]),
      SSuggestDestructureRequiresIterable);

  if AValue is TGocciaArrayValue then
  begin
    ArrayValue := TGocciaArrayValue(AValue);

    for I := 0 to APattern.Elements.Count - 1 do
    begin
      if APattern.Elements[I] = nil then
        Continue;

      if APattern.Elements[I] is TGocciaRestDestructuringPattern then
      begin
        RestElements := TGocciaArrayValue.Create;
        for J := I to ArrayValue.Elements.Count - 1 do
        begin
          if ArrayValue.Elements[J] <> TGocciaHoleValue.HoleValue then
            RestElements.Elements.Add(ArrayValue.Elements[J])
          else
            RestElements.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue);
        end;
        AssignPattern(TGocciaRestDestructuringPattern(APattern.Elements[I]).Argument, RestElements, AContext, AIsDeclaration, ADeclarationType);
        Break;
      end
      else
      begin
        if (I < ArrayValue.Elements.Count) and (ArrayValue.Elements[I] <> TGocciaHoleValue.HoleValue) then
          ElementValue := ArrayValue.Elements[I]
        else
          ElementValue := TGocciaUndefinedLiteralValue.UndefinedValue;

        AssignPattern(APattern.Elements[I], ElementValue, AContext, AIsDeclaration, ADeclarationType);
      end;
    end;
  end
  else if AValue is TGocciaStringLiteralValue then
  begin
    for I := 0 to APattern.Elements.Count - 1 do
    begin
      if APattern.Elements[I] = nil then
        Continue;

      if APattern.Elements[I] is TGocciaRestDestructuringPattern then
      begin
        RestElements := TGocciaArrayValue.Create;
        for J := I + 1 to Length(AValue.ToStringLiteral.Value) do
          RestElements.Elements.Add(TGocciaStringLiteralValue.Create(AValue.ToStringLiteral.Value[J]));
        AssignPattern(TGocciaRestDestructuringPattern(APattern.Elements[I]).Argument, RestElements, AContext, AIsDeclaration, ADeclarationType);
        Break;
      end
      else
      begin
        if I + 1 <= Length(AValue.ToStringLiteral.Value) then
          ElementValue := TGocciaStringLiteralValue.Create(AValue.ToStringLiteral.Value[I + 1])
        else
          ElementValue := TGocciaUndefinedLiteralValue.UndefinedValue;

        AssignPattern(APattern.Elements[I], ElementValue, AContext, AIsDeclaration, ADeclarationType);
      end;
    end;
  end
  else
  begin
    Iterator := GetIteratorFromValue(AValue);
    if not Assigned(Iterator) then
      ThrowTypeError(
        Format(SErrorNotIterable, [AValue.TypeName]),
        SSuggestDestructureRequiresIterable);

    TGarbageCollector.Instance.AddTempRoot(Iterator);
    try
      for I := 0 to APattern.Elements.Count - 1 do
      begin
        if APattern.Elements[I] = nil then
        begin
          Iterator.AdvanceNext;
          Continue;
        end;

        if APattern.Elements[I] is TGocciaRestDestructuringPattern then
        begin
          RestElements := TGocciaArrayValue.Create;
          IterResult := Iterator.AdvanceNext;
          while not IterResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value do
          begin
            RestElements.Elements.Add(IterResult.GetProperty(PROP_VALUE));
            IterResult := Iterator.AdvanceNext;
          end;
          AssignPattern(TGocciaRestDestructuringPattern(APattern.Elements[I]).Argument, RestElements, AContext, AIsDeclaration, ADeclarationType);
          Break;
        end
        else
        begin
          IterResult := Iterator.AdvanceNext;
          if IterResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value then
            ElementValue := TGocciaUndefinedLiteralValue.UndefinedValue
          else
            ElementValue := IterResult.GetProperty(PROP_VALUE);

          AssignPattern(APattern.Elements[I], ElementValue, AContext, AIsDeclaration, ADeclarationType);
        end;
      end;
    finally
      TGarbageCollector.Instance.RemoveTempRoot(Iterator);
    end;
  end;
end;

procedure AssignObjectPattern(const APattern: TGocciaObjectDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
var
  ObjectValue: TGocciaObjectValue;
  Prop: TGocciaDestructuringProperty;
  PropValue: TGocciaValue;
  Key: string;
  RestObject: TGocciaObjectValue;
  UsedKeys: TStringList;
  ObjectPair: TPair<string, TGocciaValue>;
  I: Integer;
begin
  // Check if value is an object
  if not (AValue is TGocciaObjectValue) then
  begin
    if (AValue is TGocciaNullLiteralValue) or (AValue is TGocciaUndefinedLiteralValue) then
      ThrowTypeError(
        Format(SErrorCannotDestructure, [AValue.ToStringLiteral.Value]),
        SSuggestDestructureRequiresObject)
    else
      ThrowTypeError(
        Format(SErrorCannotDestructureType, [AValue.TypeName]),
        SSuggestDestructureRequiresObject);
  end;

  ObjectValue := TGocciaObjectValue(AValue);
  UsedKeys := TStringList.Create;

  try
    // Use indexed for loop to ensure properties are processed in source order
    for I := 0 to APattern.Properties.Count - 1 do
    begin
      Prop := APattern.Properties[I];
      if Prop.Pattern is TGocciaRestDestructuringPattern then
      begin
        // Rest pattern: collect remaining properties
        RestObject := TGocciaObjectValue.Create;
        for Key in ObjectValue.GetEnumerablePropertyNames do
        begin
          if UsedKeys.IndexOf(Key) = -1 then
            RestObject.AssignProperty(Key, ObjectValue.GetProperty(Key));
        end;
        AssignPattern(TGocciaRestDestructuringPattern(Prop.Pattern).Argument, RestObject, AContext, AIsDeclaration, ADeclarationType);
      end
      else
      begin
        // Regular property
        if Prop.Computed then
        begin
          // Computed property key (may be a symbol)
          PropValue := EvaluateExpression(Prop.KeyExpression, AContext);
          if PropValue is TGocciaSymbolValue then
          begin
            PropValue := ObjectValue.GetSymbolProperty(TGocciaSymbolValue(PropValue));
            AssignPattern(Prop.Pattern, PropValue, AContext, AIsDeclaration, ADeclarationType);
            Continue;
          end;
          Key := PropValue.ToStringLiteral.Value;
        end
        else
        begin
          Key := Prop.Key;
        end;

        UsedKeys.Add(Key);
        PropValue := ObjectValue.GetProperty(Key);
        AssignPattern(Prop.Pattern, PropValue, AContext, AIsDeclaration, ADeclarationType);
      end;
    end;
  finally
    UsedKeys.Free;
  end;
end;

procedure AssignAssignmentPattern(const APattern: TGocciaAssignmentDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
var
  DefaultValue: TGocciaValue;
begin
  // Use default value if the value is undefined
  if AValue is TGocciaUndefinedLiteralValue then
  begin
    DefaultValue := EvaluateExpression(APattern.Right, AContext);
    AssignPattern(APattern.Left, DefaultValue, AContext, AIsDeclaration, ADeclarationType);
  end
  else
  begin
    AssignPattern(APattern.Left, AValue, AContext, AIsDeclaration, ADeclarationType);
  end;
end;

procedure AssignRestPattern(const APattern: TGocciaRestDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
begin
  AssignPattern(APattern.Argument, AValue, AContext, AIsDeclaration, ADeclarationType);
end;

function IsObjectInstanceOfClass(const AObj: TGocciaObjectValue; const AClassValue: TGocciaClassValue): Boolean;
var
  CurrentPrototype: TGocciaObjectValue;
  TargetPrototype: TGocciaObjectValue;
begin
  Result := False;

  // Get the target prototype we're looking for
  TargetPrototype := AClassValue.Prototype;
  if not Assigned(TargetPrototype) then
    Exit;

  // Walk up the prototype chain of the object
  CurrentPrototype := AObj.Prototype;
  while Assigned(CurrentPrototype) do
  begin
    // Check if the current prototype is the target prototype
    if CurrentPrototype = TargetPrototype then
    begin
      Result := True;
      Exit;
    end;

    // Move up the prototype chain
    CurrentPrototype := CurrentPrototype.Prototype;
  end;
end;

function EvaluateDelete(const AOperand: TGocciaExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  MemberExpr: TGocciaMemberExpression;
  ObjValue: TGocciaValue;
  PropertyName: string;
  Value: TGocciaValue;
  ArrayValue: TGocciaArrayValue;
  ObjectValue: TGocciaObjectValue;
  Index: Integer;
begin
  // Handle member expressions (property deletion)
  if AOperand is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(AOperand);
    ObjValue := EvaluateExpression(MemberExpr.ObjectExpr, AContext);

    // Get property name
    if MemberExpr.Computed and Assigned(MemberExpr.PropertyExpression) then
    begin
      Value := EvaluateExpression(MemberExpr.PropertyExpression, AContext);
      PropertyName := Value.ToStringLiteral.Value;
    end
    else
    begin
      PropertyName := MemberExpr.PropertyName;
    end;

    // Handle array element deletion
    if ObjValue is TGocciaArrayValue then
    begin
      ArrayValue := TGocciaArrayValue(ObjValue);
      if TryStrToInt(PropertyName, Index) and (Index >= 0) and (Index < ArrayValue.Elements.Count) then
      begin
        ArrayValue.Elements[Index] := TGocciaHoleValue.HoleValue;
        Result := TGocciaBooleanLiteralValue.TrueValue;
      end
      else
      begin
        if not ArrayValue.DeleteProperty(PropertyName) then
          ThrowTypeError(Format(SErrorCannotDeletePropertyOf, [PropertyName, '[object Array]']),
            SSuggestCannotDeleteNonConfigurable);
        Result := TGocciaBooleanLiteralValue.TrueValue;
      end;
    end
    else if ObjValue is TGocciaObjectValue then
    begin
      ObjectValue := TGocciaObjectValue(ObjValue);
      if not ObjectValue.DeleteProperty(PropertyName) then
        ThrowTypeError(Format(SErrorCannotDeletePropertyOf, [PropertyName, '[object Object]']),
          SSuggestCannotDeleteNonConfigurable);
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end
    else
    begin
      // Cannot delete properties from primitives, null, or undefined
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end;
  end
  else
  begin
    // Handle other expressions according to strict mode semantics
    if AOperand is TGocciaIdentifierExpression then
    begin
      // In strict mode, attempting to delete variables/identifiers throws TypeError
      AContext.OnError('Delete of an unqualified identifier in strict mode',
        AOperand.Line, AOperand.Column);
      Result := TGocciaBooleanLiteralValue.TrueValue; // Fallback if OnError doesn't throw
    end
    else
    begin
      // For literals and other non-reference expressions, return true
      // (e.g., delete 5, delete "string", delete (1+2), etc.)
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end;
  end;
end;

end.
