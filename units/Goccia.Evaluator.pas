unit Goccia.Evaluator;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Modules,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  PGocciaValue = ^TGocciaValue;

  TGocciaEvaluationContext = record
    Scope: TGocciaScope;
    OnError: TGocciaThrowErrorCallback;
    LoadModule: TLoadModuleCallback;
    CurrentFilePath: string;
  end;

function Evaluate(const ANode: TGocciaASTNode; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateExpression(const AExpression: TGocciaExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateStatement(const AStatement: TGocciaStatement; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateStatementsSafe(const ANodes: TObjectList<TGocciaASTNode>; const AContext: TGocciaEvaluationContext): TGocciaValue;
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
function EvaluateBlock(const ABlockStatement: TGocciaBlockStatement; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateIf(const AIfStatement: TGocciaIfStatement; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateTry(const ATryStatement: TGocciaTryStatement; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateSwitch(const ASwitchStatement: TGocciaSwitchStatement; const AContext: TGocciaEvaluationContext): TGocciaValue;
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
function EvaluateTemplateExpression(const AExpressionText: string; const AContext: TGocciaEvaluationContext; const ALine, AColumn: Integer): TGocciaValue;
function EvaluateAwait(const AAwaitExpression: TGocciaAwaitExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function AwaitValue(const AValue: TGocciaValue): TGocciaValue;
function EvaluateForOf(const AForOfStatement: TGocciaForOfStatement; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateForAwaitOf(const AForAwaitOfStatement: TGocciaForAwaitOfStatement; const AContext: TGocciaEvaluationContext): TGocciaValue;

// Destructuring pattern assignment procedures
procedure AssignPattern(const APattern: TGocciaDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
procedure AssignIdentifierPattern(const APattern: TGocciaIdentifierDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
procedure AssignArrayPattern(const APattern: TGocciaArrayDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
procedure AssignObjectPattern(const APattern: TGocciaObjectDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
procedure AssignAssignmentPattern(const APattern: TGocciaAssignmentDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
procedure AssignRestPattern(const APattern: TGocciaRestDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
procedure InitializeInstanceProperties(const AInstance: TGocciaInstanceValue; const AClassValue: TGocciaClassValue; const AContext: TGocciaEvaluationContext);
procedure InitializePrivateInstanceProperties(const AInstance: TGocciaInstanceValue; const AClassValue: TGocciaClassValue; const AContext: TGocciaEvaluationContext);

function IsObjectInstanceOfClass(const AObj: TGocciaObjectValue; const AClassValue: TGocciaClassValue): Boolean;

// Helper function to safely call OnError with proper error handling
procedure SafeOnError(const AContext: TGocciaEvaluationContext; const AMessage: string; const ALine, AColumn: Integer);

implementation

uses
  Classes,
  SysUtils,

  OrderedMap,

  Goccia.Arguments.Collection,
  Goccia.CallStack,
  Goccia.Constants,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.Evaluator.Arithmetic,
  Goccia.Evaluator.Assignment,
  Goccia.Evaluator.Bitwise,
  Goccia.Evaluator.Comparison,
  Goccia.Evaluator.Decorators,
  Goccia.Evaluator.TypeOperations,
  Goccia.GarbageCollector,
  Goccia.Keywords.Reserved,
  Goccia.Lexer,
  Goccia.MicrotaskQueue,
  Goccia.Parser,
  Goccia.Token,
  Goccia.Values.ArrayValue,
  Goccia.Values.AsyncFunctionValue,
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
  Goccia.Values.SetValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToPrimitive;

// Helper function to safely call OnError with proper error handling
procedure SafeOnError(const AContext: TGocciaEvaluationContext; const AMessage: string; const ALine, AColumn: Integer);
begin
  if Assigned(AContext.OnError) then
    AContext.OnError(AMessage, ALine, AColumn);
end;

// Helper: create a non-owning copy of a statement list (AST owns the nodes)
function CopyStatementList(const ASource: TObjectList<TGocciaASTNode>): TObjectList<TGocciaASTNode>;
var
  I: Integer;
begin
  Result := TObjectList<TGocciaASTNode>.Create(False);
  for I := 0 to ASource.Count - 1 do
    Result.Add(ASource[I]);
end;

function EvaluateStatementsSafe(const ANodes: TObjectList<TGocciaASTNode>; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  I: Integer;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := 0 to ANodes.Count - 1 do
  begin
    try
      Result := Evaluate(ANodes[I], AContext);
    except
      on E: TGocciaReturnValue do raise;
      on E: TGocciaThrowValue do raise;
      on E: TGocciaBreakSignal do raise;
      on E: TGocciaTypeError do raise;
      on E: TGocciaReferenceError do raise;
      on E: TGocciaRuntimeError do raise;
      on E: Exception do
        raise TGocciaError.Create('Error executing statement: ' + E.Message, 0, 0, '', nil);
    end;
  end;
end;

function GetIteratorFromValue(const AValue: TGocciaValue): TGocciaIteratorValue;
var
  IteratorMethod, IteratorObj, NextMethod: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  WasAlreadyRooted: Boolean;
  GC: TGocciaGarbageCollector;
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
      GC := TGocciaGarbageCollector.Instance;
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
      if SpreadArray.Elements[J] = nil then
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
      TGocciaGarbageCollector.Instance.AddTempRoot(Iterator);
      try
        IterResult := Iterator.AdvanceNext;
        while not IterResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value do
        begin
          ATarget.Add(IterResult.GetProperty(PROP_VALUE));
          IterResult := Iterator.AdvanceNext;
        end;
      finally
        TGocciaGarbageCollector.Instance.RemoveTempRoot(Iterator);
      end;
    end
    else
      ThrowTypeError('Spread syntax requires an iterable');
  end;
end;

procedure SpreadIterableIntoArgs(const ASpreadValue: TGocciaValue; const AArgs: TGocciaArgumentsCollection);
begin
  SpreadIterableInto(ASpreadValue, AArgs.Items);
end;

function Evaluate(const ANode: TGocciaASTNode; const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  // Propagate OnError onto the scope so closures inherit it
  if Assigned(AContext.OnError) and not Assigned(AContext.Scope.OnError) then
    AContext.Scope.OnError := AContext.OnError;

  if ANode is TGocciaExpression then
  begin
    Result := EvaluateExpression(TGocciaExpression(ANode), AContext);
  end
  else if ANode is TGocciaStatement then
  begin
    Result := EvaluateStatement(TGocciaStatement(ANode), AContext);
  end
  else
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function EvaluateExpression(const AExpression: TGocciaExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Value: TGocciaValue;
  Obj: TGocciaValue;
  PropName: string;
  PropertyValue: TGocciaValue;
  OldValue: TGocciaValue;
begin
  if AExpression is TGocciaLiteralExpression then
  begin
    Result := TGocciaLiteralExpression(AExpression).Value.RuntimeCopy;
  end
  else if AExpression is TGocciaTemplateLiteralExpression then
    Result := EvaluateTemplateLiteral(TGocciaTemplateLiteralExpression(AExpression), AContext)
  else if AExpression is TGocciaIdentifierExpression then
  begin
    // In strict mode, GetValue throws ReferenceError for undefined variables
    Result := AContext.Scope.GetValue(TGocciaIdentifierExpression(AExpression).Name);
  end
  else if AExpression is TGocciaBinaryExpression then
    Result := EvaluateBinary(TGocciaBinaryExpression(AExpression), AContext)
  else if AExpression is TGocciaUnaryExpression then
    Result := EvaluateUnary(TGocciaUnaryExpression(AExpression), AContext)
  else if AExpression is TGocciaAssignmentExpression then
  begin
    // Variable assignment
    Result := EvaluateExpression(TGocciaAssignmentExpression(AExpression).Value, AContext);
    AContext.Scope.AssignLexicalBinding(TGocciaAssignmentExpression(AExpression).Name, Result);
  end
  else if AExpression is TGocciaPropertyAssignmentExpression then
  begin
    // Property assignment
    Obj := EvaluateExpression(TGocciaPropertyAssignmentExpression(AExpression).ObjectExpr, AContext);
    Value := EvaluateExpression(TGocciaPropertyAssignmentExpression(AExpression).Value, AContext);
    AssignProperty(Obj, TGocciaPropertyAssignmentExpression(AExpression).PropertyName, Value, AContext.OnError, AExpression.Line, AExpression.Column);
    Result := Value;
  end
  else if AExpression is TGocciaComputedPropertyAssignmentExpression then
  begin
    // Computed property assignment (e.g., obj[expr] = value)
    Obj := EvaluateExpression(TGocciaComputedPropertyAssignmentExpression(AExpression).ObjectExpr, AContext);
    PropertyValue := EvaluateExpression(TGocciaComputedPropertyAssignmentExpression(AExpression).PropertyExpression, AContext);
    Value := EvaluateExpression(TGocciaComputedPropertyAssignmentExpression(AExpression).Value, AContext);
    if (PropertyValue is TGocciaSymbolValue) and (Obj is TGocciaClassValue) then
      TGocciaClassValue(Obj).AssignSymbolProperty(TGocciaSymbolValue(PropertyValue), Value)
    else if (PropertyValue is TGocciaSymbolValue) and (Obj is TGocciaObjectValue) then
      TGocciaObjectValue(Obj).AssignSymbolProperty(TGocciaSymbolValue(PropertyValue), Value)
    else
    begin
      PropName := PropertyValue.ToStringLiteral.Value;
      AssignProperty(Obj, PropName, Value, AContext.OnError, AExpression.Line, AExpression.Column);
    end;
    Result := Value;
  end
  else if AExpression is TGocciaCompoundAssignmentExpression then
  begin
    // Variable compound assignment (e.g., count += 5)
    Result := AContext.Scope.GetValue(TGocciaCompoundAssignmentExpression(AExpression).Name);
    Value := EvaluateExpression(TGocciaCompoundAssignmentExpression(AExpression).Value, AContext);

    // Use shared compound operation function
    Result := PerformCompoundOperation(Result, Value, TGocciaCompoundAssignmentExpression(AExpression).Operator);

    AContext.Scope.AssignLexicalBinding(TGocciaCompoundAssignmentExpression(AExpression).Name, Result);
  end
  else if AExpression is TGocciaPropertyCompoundAssignmentExpression then
  begin
    // Property compound assignment (e.g., obj.count += 5)
    Obj := EvaluateExpression(TGocciaPropertyCompoundAssignmentExpression(AExpression).ObjectExpr, AContext);
    Value := EvaluateExpression(TGocciaPropertyCompoundAssignmentExpression(AExpression).Value, AContext);
    PropName := TGocciaPropertyCompoundAssignmentExpression(AExpression).PropertyName;
    PerformPropertyCompoundAssignment(Obj, PropName, Value, TGocciaPropertyCompoundAssignmentExpression(AExpression).Operator, AContext.OnError, AExpression.Line, AExpression.Column);

    // Get the final result
    Result := Obj.GetProperty(PropName);
    if Result = nil then
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else if AExpression is TGocciaComputedPropertyCompoundAssignmentExpression then
  begin
    // Computed property compound assignment (e.g., obj[expr] += value)
    Obj := EvaluateExpression(TGocciaComputedPropertyCompoundAssignmentExpression(AExpression).ObjectExpr, AContext);
    PropertyValue := EvaluateExpression(TGocciaComputedPropertyCompoundAssignmentExpression(AExpression).PropertyExpression, AContext);
    Value := EvaluateExpression(TGocciaComputedPropertyCompoundAssignmentExpression(AExpression).Value, AContext);
    if (PropertyValue is TGocciaSymbolValue) and ((Obj is TGocciaClassValue) or (Obj is TGocciaObjectValue)) then
    begin
      PerformSymbolPropertyCompoundAssignment(Obj, TGocciaSymbolValue(PropertyValue), Value, TGocciaComputedPropertyCompoundAssignmentExpression(AExpression).Operator, AContext.OnError, AExpression.Line, AExpression.Column);
      if Obj is TGocciaClassValue then
        Result := TGocciaClassValue(Obj).GetSymbolProperty(TGocciaSymbolValue(PropertyValue))
      else
        Result := TGocciaObjectValue(Obj).GetSymbolProperty(TGocciaSymbolValue(PropertyValue));
    end
    else
    begin
      PropName := PropertyValue.ToStringLiteral.Value;
      PerformPropertyCompoundAssignment(Obj, PropName, Value, TGocciaComputedPropertyCompoundAssignmentExpression(AExpression).Operator, AContext.OnError, AExpression.Line, AExpression.Column);
      Result := Obj.GetProperty(PropName);
    end;
    if Result = nil then
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else if AExpression is TGocciaIncrementExpression then
  begin
    // Increment/decrement expressions (++x, --x, x++, x--)
    if TGocciaIncrementExpression(AExpression).Operand is TGocciaIdentifierExpression then
    begin
            // Variable increment/decrement
      PropName := TGocciaIdentifierExpression(TGocciaIncrementExpression(AExpression).Operand).Name;
      OldValue := AContext.Scope.GetValue(PropName);

      // Calculate new value
      Value := PerformIncrement(OldValue, TGocciaIncrementExpression(AExpression).Operator = gttIncrement);

      // Set the new value
      AContext.Scope.AssignLexicalBinding(PropName, Value);

      // Return value depends on prefix/postfix
      if TGocciaIncrementExpression(AExpression).IsPrefix then
        Result := Value  // Prefix: return new value (++x)
      else
        Result := OldValue;  // Postfix: return old value (x++)
    end
    else if TGocciaIncrementExpression(AExpression).Operand is TGocciaMemberExpression then
    begin
      // Property increment/decrement
      Obj := EvaluateExpression(TGocciaMemberExpression(TGocciaIncrementExpression(AExpression).Operand).ObjectExpr, AContext);
      PropName := TGocciaMemberExpression(TGocciaIncrementExpression(AExpression).Operand).PropertyName;

      // Get current property value
      OldValue := Obj.GetProperty(PropName);
      if OldValue = nil then
      begin
        AContext.OnError('Cannot access property on non-object', AExpression.Line, AExpression.Column);
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
        Exit;
      end;

      // Calculate new value
      Value := PerformIncrement(OldValue, TGocciaIncrementExpression(AExpression).Operator = gttIncrement);

      // Set the new value (create property if it doesn't exist, like JavaScript)
      DefinePropertyOnValue(Obj, PropName, Value);

      // Return value depends on prefix/postfix
      if TGocciaIncrementExpression(AExpression).IsPrefix then
        Result := Value  // Prefix: return new value (++obj.prop)
      else
        Result := OldValue;  // Postfix: return old value (obj.prop++)
    end
    else
    begin
      AContext.OnError('Invalid target for increment/decrement', AExpression.Line, AExpression.Column);
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    end;
  end
  else if AExpression is TGocciaCallExpression then
  begin
    Result := EvaluateCall(TGocciaCallExpression(AExpression), AContext);
  end
  else if AExpression is TGocciaMemberExpression then
    Result := EvaluateMember(TGocciaMemberExpression(AExpression), AContext)
  else if AExpression is TGocciaArrayExpression then
    Result := EvaluateArray(TGocciaArrayExpression(AExpression), AContext)
  else if AExpression is TGocciaObjectExpression then
    Result := EvaluateObject(TGocciaObjectExpression(AExpression), AContext)
  else if AExpression is TGocciaMethodExpression then
    Result := EvaluateMethodExpression(TGocciaMethodExpression(AExpression), AContext)
  else if AExpression is TGocciaAwaitExpression then
    Result := EvaluateAwait(TGocciaAwaitExpression(AExpression), AContext)
  else if AExpression is TGocciaArrowFunctionExpression then
    Result := EvaluateArrowFunction(TGocciaArrowFunctionExpression(AExpression), AContext)
  else if AExpression is TGocciaConditionalExpression then
  begin
    if EvaluateExpression(TGocciaConditionalExpression(AExpression).Condition, AContext).ToBooleanLiteral.Value then
      Result := EvaluateExpression(TGocciaConditionalExpression(AExpression).Consequent, AContext)
    else
      Result := EvaluateExpression(TGocciaConditionalExpression(AExpression).Alternate, AContext);
  end
  else if AExpression is TGocciaNewExpression then
  begin
    // Handle new expression - create instance
    Result := EvaluateNewExpression(TGocciaNewExpression(AExpression), AContext);
  end
  else if AExpression is TGocciaThisExpression then
  begin
    Result := AContext.Scope.ThisValue;
  end
  else if AExpression is TGocciaClassExpression then
  begin
    Result := EvaluateClassExpression(TGocciaClassExpression(AExpression), AContext);
  end
  else if AExpression is TGocciaSuperExpression then
  begin
    // Walk the scope chain via VMT dispatch to find the super class set by
    // TGocciaMethodCallScope. All class methods go through TGocciaMethodValue.CreateCallScope
    // which always creates a TGocciaMethodCallScope with the correct SuperClass.
    Result := AContext.Scope.FindSuperClass;
    if not Assigned(Result) then
      AContext.OnError('super can only be used in a class method', AExpression.Line, AExpression.Column);
  end
  else if AExpression is TGocciaPrivateMemberExpression then
  begin
    Result := EvaluatePrivateMember(TGocciaPrivateMemberExpression(AExpression), AContext);
  end
  else if AExpression is TGocciaPrivatePropertyAssignmentExpression then
  begin
    Result := EvaluatePrivatePropertyAssignment(TGocciaPrivatePropertyAssignmentExpression(AExpression), AContext);
  end
  else if AExpression is TGocciaPrivatePropertyCompoundAssignmentExpression then
  begin
    Result := EvaluatePrivatePropertyCompoundAssignment(TGocciaPrivatePropertyCompoundAssignmentExpression(AExpression), AContext);
  end
  else if AExpression is TGocciaMemberExpression then
  begin
    if TGocciaMemberExpression(AExpression).ObjectExpr is TGocciaThisExpression then
    begin
      // When accessing a property through this, use the current scope's this value
      Result := AContext.Scope.GetThisProperty(TGocciaMemberExpression(AExpression).PropertyName);
    end
    else
    begin
      // For normal member access, evaluate the object first
      Result := EvaluateMember(TGocciaMemberExpression(AExpression), AContext);
    end;
  end
  else if AExpression is TGocciaHoleExpression then
  begin
    // Return nil for holes - special case for sparse arrays
    Result := nil;
  end
  else if AExpression is TGocciaSpreadExpression then
  begin
    // Evaluate the spread expression - this should not be called directly
    // Spread expressions are handled specially in array/object/call contexts
    AContext.OnError('Unexpected spread syntax', AExpression.Line, AExpression.Column);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else if AExpression is TGocciaDestructuringAssignmentExpression then
  begin
    Result := EvaluateDestructuringAssignment(TGocciaDestructuringAssignmentExpression(AExpression), AContext);
  end
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function EvaluateStatement(const AStatement: TGocciaStatement; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Decl: TGocciaVariableDeclaration;
  ImportDecl: TGocciaImportDeclaration;
  Module: TGocciaModule;
  ImportPair: TPair<string, string>;
  Value: TGocciaValue;
  I: Integer;
begin

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  if AStatement is TGocciaExpressionStatement then
  begin
    Result := EvaluateExpression(TGocciaExpressionStatement(AStatement).Expression, AContext);
  end
      else if AStatement is TGocciaVariableDeclaration then
  begin
    Decl := TGocciaVariableDeclaration(AStatement);

    // Process each variable in the declaration
    for I := 0 to Length(Decl.Variables) - 1 do
    begin

      Value := EvaluateExpression(Decl.Variables[I].Initializer, AContext);

      // Name inference for anonymous functions: const add = (a, b) => a + b; => add.name === "add"
      if (Value is TGocciaFunctionValue) and (TGocciaFunctionValue(Value).Name = '') then
        TGocciaFunctionValue(Value).Name := Decl.Variables[I].Name;

      // Use the new Define/Assign pattern
      if Decl.IsConst then
        AContext.Scope.DefineFromToken(Decl.Variables[I].Name, Value, gttConst)
      else
        AContext.Scope.DefineFromToken(Decl.Variables[I].Name, Value, gttLet);

    end;
  end
  else if AStatement is TGocciaDestructuringDeclaration then
  begin
    Result := EvaluateDestructuringDeclaration(TGocciaDestructuringDeclaration(AStatement), AContext);
  end
  else if AStatement is TGocciaBlockStatement then
  begin
    Result := EvaluateBlock(TGocciaBlockStatement(AStatement), AContext);
  end
  else if AStatement is TGocciaIfStatement then
  begin
    Result := EvaluateIf(TGocciaIfStatement(AStatement), AContext);
  end
  else if AStatement is TGocciaSwitchStatement then
  begin
    Result := EvaluateSwitch(TGocciaSwitchStatement(AStatement), AContext);
  end
  else if AStatement is TGocciaBreakStatement then
  begin
    raise TGocciaBreakSignal.Create;
  end
  else if AStatement is TGocciaReturnStatement then
  begin
    if Assigned(TGocciaReturnStatement(AStatement).Value) then
    begin
      Value := EvaluateExpression(TGocciaReturnStatement(AStatement).Value, AContext);
      if Value = nil then
      begin
        Value := TGocciaUndefinedLiteralValue.UndefinedValue;
      end;
    end
    else
    begin
      Value := TGocciaUndefinedLiteralValue.UndefinedValue;
    end;
    raise TGocciaReturnValue.Create(Value);
  end
  else if AStatement is TGocciaThrowStatement then
  begin
    Value := EvaluateExpression(TGocciaThrowStatement(AStatement).Value, AContext);
    raise TGocciaThrowValue.Create(Value);
  end
  else if AStatement is TGocciaTryStatement then
  begin
    Result := EvaluateTry(TGocciaTryStatement(AStatement), AContext);
  end
  else if AStatement is TGocciaClassDeclaration then
  begin
    Result := EvaluateClass(TGocciaClassDeclaration(AStatement), AContext);
  end
  else if AStatement is TGocciaEnumDeclaration then
  begin
    Result := EvaluateEnumDeclaration(TGocciaEnumDeclaration(AStatement), AContext);
  end
  else if AStatement is TGocciaExportEnumDeclaration then
  begin
    Result := EvaluateEnumDeclaration(TGocciaExportEnumDeclaration(AStatement).Declaration, AContext);
  end
  else if AStatement is TGocciaImportDeclaration then
  begin
    ImportDecl := TGocciaImportDeclaration(AStatement);
    Module := AContext.LoadModule(ImportDecl.ModulePath, AContext.CurrentFilePath);

    for ImportPair in ImportDecl.Imports do
    begin
      if Module.ExportsTable.TryGetValue(ImportPair.Value, Value) then
      begin
        AContext.Scope.DefineLexicalBinding(ImportPair.Key, Value, dtLet);
      end
      else
      begin
        AContext.OnError(Format('Module "%s" has no export named "%s"',
          [ImportDecl.ModulePath, ImportPair.Value]), AStatement.Line, AStatement.Column);
      end;
    end;
  end
  else if AStatement is TGocciaExportVariableDeclaration then
  begin
    Result := EvaluateStatement(TGocciaExportVariableDeclaration(AStatement).Declaration, AContext);
  end
  else if AStatement is TGocciaReExportDeclaration then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else if AStatement is TGocciaForAwaitOfStatement then
  begin
    Result := EvaluateForAwaitOf(TGocciaForAwaitOfStatement(AStatement), AContext);
  end
  else if AStatement is TGocciaForOfStatement then
  begin
    Result := EvaluateForOf(TGocciaForOfStatement(AStatement), AContext);
  end
  else if AStatement is TGocciaEmptyStatement then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;

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
      Result := Left  // Short-circuit: return left operand if falsy
    else
    begin
      Right := EvaluateExpression(ABinaryExpression.Right, AContext);
      Result := Right;  // Return right operand
    end;
    Exit;
  end
  else if ABinaryExpression.Operator = gttOr then
  begin
    Left := EvaluateExpression(ABinaryExpression.Left, AContext);
    if Left.ToBooleanLiteral.Value then
      Result := Left  // Short-circuit: return left operand if truthy
    else
    begin
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
      Right := EvaluateExpression(ABinaryExpression.Right, AContext);
      Result := Right;
    end
    else
      Result := Left;  // Return left operand for all other values (including falsy ones)
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
          ThrowTypeError('Cannot convert a Symbol value to a number');
        if (Operand is TGocciaNumberLiteralValue) then
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
        ThrowTypeError('Cannot convert a Symbol value to a number');
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
      TGocciaCallStack.Instance.Push(CalleeName, AContext.CurrentFilePath,
        ACallExpression.Line, ACallExpression.Column);
    try
      if Callee is TGocciaNativeFunctionValue then
        Result := TGocciaNativeFunctionValue(Callee).Call(Arguments, ThisValue)
      else if Callee is TGocciaFunctionValue then
        Result := TGocciaFunctionValue(Callee).Call(Arguments, ThisValue)
      else if Callee is TGocciaBoundFunctionValue then
        Result := TGocciaBoundFunctionValue(Callee).Call(Arguments, ThisValue)
      else if Callee is TGocciaClassValue then
        Result := TGocciaClassValue(Callee).Call(Arguments, ThisValue)
      else
      begin
        ThrowTypeError(Format('%s is not a function', [Callee.TypeName]));
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
begin

  // Handle optional chaining: obj?.prop returns undefined if obj is null/undefined
  if AMemberExpression.Optional then
  begin
    Obj := EvaluateExpression(AMemberExpression.ObjectExpr, AContext);
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

  Obj := EvaluateExpression(AMemberExpression.ObjectExpr, AContext);
  if Assigned(AOutObjectValue) then
    AOutObjectValue^ := Obj;

  // Determine the property name
  if AMemberExpression.Computed and Assigned(AMemberExpression.PropertyExpression) then
  begin
    // Computed access: evaluate the property expression to get the property name
    PropertyValue := EvaluateExpression(AMemberExpression.PropertyExpression, AContext);

    // Symbol property access
    if PropertyValue is TGocciaSymbolValue then
    begin
      if (Obj is TGocciaNullLiteralValue) or (Obj is TGocciaUndefinedLiteralValue) then
        ThrowTypeError('Cannot read properties of ' + Obj.ToStringLiteral.Value + ' (reading ''Symbol()'')');

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
      ThrowTypeError('Cannot read properties of ' + Obj.ToStringLiteral.Value + ' (reading ''' + PropertyName + ''')')
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
  TGocciaGarbageCollector.Instance.AddTempRoot(Arr);
  try
    for I := 0 to AArrayExpression.Elements.Count - 1 do
    begin
      if AArrayExpression.Elements[I] is TGocciaHoleExpression then
        Arr.Elements.Add(nil)
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
    TGocciaGarbageCollector.Instance.RemoveTempRoot(Arr);
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
  if Assigned(TGocciaGarbageCollector.Instance) then
    TGocciaGarbageCollector.Instance.AddTempRoot(Obj);

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
                   if SpreadArray.Elements[J] <> nil then
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
    if Assigned(TGocciaGarbageCollector.Instance) then
      TGocciaGarbageCollector.Instance.RemoveTempRoot(Obj);
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
    if Assigned(TGocciaGarbageCollector.Instance) then
      TGocciaGarbageCollector.Instance.AddTempRoot(Promise);
  end
  // ES2026 §25.6.4.6.1 PromiseResolve: If value is a thenable (object with
  // callable .then), wrap it in a Promise via resolve
  else if AValue is TGocciaObjectValue then
  begin
    ThenMethod := AValue.GetProperty(PROP_THEN);
    if Assigned(ThenMethod) and not (ThenMethod is TGocciaUndefinedLiteralValue) and ThenMethod.IsCallable then
    begin
      Promise := TGocciaPromiseValue.Create;
      if Assigned(TGocciaGarbageCollector.Instance) then
        TGocciaGarbageCollector.Instance.AddTempRoot(Promise);
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
      Result := AValue;
      Exit;
    end;
  end
  else
  begin
    Result := AValue;
    Exit;
  end;

  try
    if Promise.State <> gpsPending then
    begin
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
      ThrowTypeError('await: Promise did not settle after microtask drain');
  finally
    if Assigned(TGocciaGarbageCollector.Instance) then
      TGocciaGarbageCollector.Instance.RemoveTempRoot(Promise);
  end;
end;

// ES2026 §27.7.5.3 Await(value)
function EvaluateAwait(const AAwaitExpression: TGocciaAwaitExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := AwaitValue(EvaluateExpression(AAwaitExpression.Operand, AContext));
end;

// ES2026 §14.7.5.6 ForIn/OfBodyEvaluation(lhs, stmt, iteratorRecord, iterationKind, lhsKind)
function EvaluateForOf(const AForOfStatement: TGocciaForOfStatement; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  IterableValue: TGocciaValue;
  Iterator: TGocciaIteratorValue;
  IterResult: TGocciaObjectValue;
  CurrentValue: TGocciaValue;
  IterScope: TGocciaScope;
  IterContext: TGocciaEvaluationContext;
  DeclarationType: TGocciaDeclarationType;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  IterableValue := EvaluateExpression(AForOfStatement.Iterable, AContext);
  Iterator := GetIteratorFromValue(IterableValue);
  if Iterator = nil then
    raise TGocciaTypeError.Create('Value is not iterable', AForOfStatement.Line, AForOfStatement.Column, '', nil);

  if AForOfStatement.IsConst then
    DeclarationType := dtConst
  else
    DeclarationType := dtLet;

  if Assigned(TGocciaGarbageCollector.Instance) then
    TGocciaGarbageCollector.Instance.AddTempRoot(Iterator);
  try
    try
      IterResult := Iterator.AdvanceNext;
      while not IterResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value do
      begin
        CurrentValue := IterResult.GetProperty(PROP_VALUE);

        IterScope := AContext.Scope.CreateChild(skBlock);
        IterContext := AContext;
        IterContext.Scope := IterScope;

        if AForOfStatement.BindingPattern <> nil then
          AssignPattern(AForOfStatement.BindingPattern, CurrentValue, IterContext, True, DeclarationType)
        else
          IterScope.DefineLexicalBinding(AForOfStatement.BindingName, CurrentValue, DeclarationType);

        try
          Evaluate(AForOfStatement.Body, IterContext);
        except
          on E: TGocciaBreakSignal do
            Exit;
        end;

        IterResult := Iterator.AdvanceNext;
      end;
    except
      on E: TGocciaBreakSignal do
        ;
    end;
  finally
    if Assigned(TGocciaGarbageCollector.Instance) then
      TGocciaGarbageCollector.Instance.RemoveTempRoot(Iterator);
  end;
end;

// ES2026 §14.7.5.6 ForIn/OfBodyEvaluation — for-await-of variant
function EvaluateForAwaitOf(const AForAwaitOfStatement: TGocciaForAwaitOfStatement; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  IterableValue, IteratorMethod, IteratorObj, NextMethod, NextResult, DoneValue, CurrentValue: TGocciaValue;
  Iterator: TGocciaIteratorValue;
  GenericNextResult: TGocciaObjectValue;
  IterScope: TGocciaScope;
  IterContext: TGocciaEvaluationContext;
  DeclarationType: TGocciaDeclarationType;
  EmptyArgs: TGocciaArgumentsCollection;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

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

    if Assigned(TGocciaGarbageCollector.Instance) then
      TGocciaGarbageCollector.Instance.AddTempRoot(IteratorObj);
    try
      EmptyArgs := TGocciaArgumentsCollection.Create;
      try
        NextMethod := IteratorObj.GetProperty(PROP_NEXT);
        if not Assigned(NextMethod) or not NextMethod.IsCallable then
          ThrowTypeError('Async iterator .next is not callable');

          while True do
          begin
            NextResult := TGocciaFunctionBase(NextMethod).Call(EmptyArgs, IteratorObj);
            NextResult := AwaitValue(NextResult);

            // ES2026 §7.4.2 step 5: If nextResult is not an Object, throw a TypeError
            if NextResult.IsPrimitive then
              ThrowTypeError('Iterator result ' + NextResult.ToStringLiteral.Value + ' is not an object');

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

          try
            Evaluate(AForAwaitOfStatement.Body, IterContext);
          except
            on E: TGocciaBreakSignal do
              Exit;
          end;
        end;
      finally
        EmptyArgs.Free;
      end;
    finally
      if Assigned(TGocciaGarbageCollector.Instance) then
        TGocciaGarbageCollector.Instance.RemoveTempRoot(IteratorObj);
    end;
  end
  else
  begin
    Iterator := GetIteratorFromValue(IterableValue);
    if Iterator = nil then
      ThrowTypeError('Value is not iterable');

    if Assigned(TGocciaGarbageCollector.Instance) then
      TGocciaGarbageCollector.Instance.AddTempRoot(Iterator);
    try
      try
        GenericNextResult := Iterator.AdvanceNext;
        while not GenericNextResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value do
        begin
          CurrentValue := GenericNextResult.GetProperty(PROP_VALUE);
          CurrentValue := AwaitValue(CurrentValue);

          IterScope := AContext.Scope.CreateChild(skBlock);
          IterContext := AContext;
          IterContext.Scope := IterScope;

          if AForAwaitOfStatement.BindingPattern <> nil then
            AssignPattern(AForAwaitOfStatement.BindingPattern, CurrentValue, IterContext, True, DeclarationType)
          else
            IterScope.DefineLexicalBinding(AForAwaitOfStatement.BindingName, CurrentValue, DeclarationType);

          try
            Evaluate(AForAwaitOfStatement.Body, IterContext);
          except
            on E: TGocciaBreakSignal do
              Exit;
          end;

          GenericNextResult := Iterator.AdvanceNext;
        end;
      except
        on E: TGocciaBreakSignal do
          ;
      end;
    finally
      if Assigned(TGocciaGarbageCollector.Instance) then
        TGocciaGarbageCollector.Instance.RemoveTempRoot(Iterator);
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
end;

function EvaluateBlock(const ABlockStatement: TGocciaBlockStatement; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  I: Integer;
  LastValue: TGocciaValue;
  BlockContext: TGocciaEvaluationContext;
  NeedsChildScope: Boolean;
begin
  // Check if we need a child scope (only if there are variable declarations)
  NeedsChildScope := False;
  for I := 0 to ABlockStatement.Nodes.Count - 1 do
  begin
    if (ABlockStatement.Nodes[I] is TGocciaVariableDeclaration) or
       (ABlockStatement.Nodes[I] is TGocciaDestructuringDeclaration) then
    begin
      NeedsChildScope := True;
      Break;
    end;
  end;

  BlockContext := AContext;
  if NeedsChildScope then
    BlockContext.Scope := AContext.Scope.CreateChild(skBlock, 'BlockScope')
  else
    BlockContext.Scope := AContext.Scope; // Use same scope - no isolation needed

  try
    LastValue := EvaluateStatementsSafe(ABlockStatement.Nodes, BlockContext);
    if LastValue = nil then
      LastValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    Result := LastValue;
  finally
    // Only free the scope if we created a child scope
    if NeedsChildScope then
      BlockContext.Scope.Free;
  end;
end;

function EvaluateIf(const AIfStatement: TGocciaIfStatement; const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  if EvaluateExpression(AIfStatement.Condition, AContext).ToBooleanLiteral.Value then
    Result := EvaluateStatement(AIfStatement.Consequent, AContext)
  else if Assigned(AIfStatement.Alternate) then
    Result := EvaluateStatement(AIfStatement.Alternate, AContext)
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function ExecuteCatchBlock(const ATryStatement: TGocciaTryStatement; const AErrorValue: TGocciaValue; const AContext: TGocciaEvaluationContext): TGocciaValue;
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
      Result := EvaluateStatementsSafe(ATryStatement.CatchBlock.Nodes, CatchContext);
    finally
      CatchScope.Free;
    end;
  end
  else
    Result := EvaluateStatementsSafe(ATryStatement.CatchBlock.Nodes, AContext);
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

function EvaluateTry(const ATryStatement: TGocciaTryStatement; const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  try
    try
      Result := EvaluateStatementsSafe(ATryStatement.Block.Nodes, AContext);
    except
      on E: TGocciaThrowValue do
      begin
        if Assigned(ATryStatement.CatchBlock) then
          Result := ExecuteCatchBlock(ATryStatement, E.Value, AContext)
        else
          raise;
      end;
      on E: TGocciaReturnValue do raise;
      on E: TGocciaBreakSignal do raise;
      on E: Exception do
      begin
        if Assigned(ATryStatement.CatchBlock) then
          Result := ExecuteCatchBlock(ATryStatement, PascalExceptionToErrorObject(E), AContext)
        else
          raise TGocciaThrowValue.Create(PascalExceptionToErrorObject(E));
      end;
    end;
  finally
    if Assigned(ATryStatement.FinallyBlock) then
      EvaluateStatementsSafe(ATryStatement.FinallyBlock.Nodes, AContext);
  end;
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

  TGocciaGarbageCollector.Instance.AddTempRoot(EnumEntries);
  try
    for I := 0 to Length(AEnumDeclaration.Members) - 1 do
    begin
      MemberValue := EvaluateExpression(AEnumDeclaration.Members[I].Initializer, ChildContext);

      if not ((MemberValue is TGocciaNumberLiteralValue) or
              (MemberValue is TGocciaStringLiteralValue) or
              (MemberValue is TGocciaSymbolValue)) then
        ThrowTypeError('Enum member initializer must evaluate to a Number, String, or Symbol value');

      EnumValue.DefineProperty(AEnumDeclaration.Members[I].Name,
        TGocciaPropertyDescriptorData.Create(MemberValue, [pfEnumerable]));

      ChildScope.DefineLexicalBinding(AEnumDeclaration.Members[I].Name, MemberValue, dtLet);

      EntryPair := TGocciaArrayValue.Create;
      EntryPair.Elements.Add(TGocciaStringLiteralValue.Create(AEnumDeclaration.Members[I].Name));
      EntryPair.Elements.Add(MemberValue);
      EnumEntries.Elements.Add(EntryPair);
    end;
  finally
    TGocciaGarbageCollector.Instance.RemoveTempRoot(EnumEntries);
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
  Entry: TOrderedMap<TGocciaExpression>.TKeyValuePair;
  I: Integer;
begin
  if Assigned(AClassValue.SuperClass) then
    InitializeInstanceProperties(AInstance, AClassValue.SuperClass, AContext);

  for I := 0 to AClassValue.InstancePropertyDefs.Count - 1 do
  begin
    Entry := AClassValue.InstancePropertyDefs.EntryAt(I);
    PropertyValue := EvaluateExpression(Entry.Value, AContext);
    AInstance.AssignProperty(Entry.Key, PropertyValue);
  end;
end;

function EvaluateSwitch(const ASwitchStatement: TGocciaSwitchStatement; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Discriminant: TGocciaValue;
  CaseClause: TGocciaCaseClause;
  CaseTest: TGocciaValue;
  I, J: Integer;
  Matched: Boolean;
  DefaultIndex: Integer;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  // Evaluate the discriminant expression
  Discriminant := EvaluateExpression(ASwitchStatement.Discriminant, AContext);

  Matched := False;
  DefaultIndex := -1;

  try
    // First pass: find matching case or default
    for I := 0 to ASwitchStatement.Cases.Count - 1 do
    begin
      CaseClause := ASwitchStatement.Cases[I];

      if not Assigned(CaseClause.Test) then
      begin
        // Remember default case index for later
        DefaultIndex := I;
        Continue;
      end;

      if not Matched then
      begin
        CaseTest := EvaluateExpression(CaseClause.Test, AContext);
        if IsStrictEqual(Discriminant, CaseTest) then
          Matched := True;
      end;

      // Once matched, execute this case's statements (fall-through)
      if Matched then
      begin
        for J := 0 to CaseClause.Consequent.Count - 1 do
          Result := Evaluate(CaseClause.Consequent[J], AContext);
      end;
    end;

    // If no case matched, execute from default and fall through
    if not Matched and (DefaultIndex >= 0) then
    begin
      for I := DefaultIndex to ASwitchStatement.Cases.Count - 1 do
      begin
        CaseClause := ASwitchStatement.Cases[I];
        for J := 0 to CaseClause.Consequent.Count - 1 do
          Result := Evaluate(CaseClause.Consequent[J], AContext);
      end;
    end;
  except
    on E: TGocciaBreakSignal do
    begin
      // Break exits the switch — swallow the signal
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
      if Callee is TGocciaClassValue then
      begin
        ClassValue := TGocciaClassValue(Callee);

        NativeInstance := nil;
        WalkClass := ClassValue;
        while Assigned(WalkClass) do
        begin
          NativeInstance := WalkClass.CreateNativeInstance(Arguments);
          if Assigned(NativeInstance) then
            Break;
          WalkClass := WalkClass.SuperClass;
        end;

        if Assigned(NativeInstance) then
        begin
          NativeInstance.Prototype := ClassValue.Prototype;
          if NativeInstance is TGocciaInstanceValue then
            TGocciaInstanceValue(NativeInstance).ClassValue := ClassValue;

          InitContext := AContext;
          InitScope := TGocciaClassInitScope.Create(AContext.Scope, ClassValue);
          InitScope.ThisValue := NativeInstance;
          InitContext.Scope := InitScope;

          InitializeInstanceProperties(TGocciaInstanceValue(NativeInstance), ClassValue, InitContext);

          if Assigned(ClassValue.SuperClass) then
          begin
            SuperInitContext := AContext;
            SuperInitScope := TGocciaClassInitScope.Create(AContext.Scope, ClassValue.SuperClass);
            SuperInitScope.ThisValue := NativeInstance;
            SuperInitContext.Scope := SuperInitScope;
            InitializePrivateInstanceProperties(TGocciaInstanceValue(NativeInstance), ClassValue.SuperClass, SuperInitContext);
          end;

          InitializePrivateInstanceProperties(TGocciaInstanceValue(NativeInstance), ClassValue, InitContext);

          ClassValue.RunMethodInitializers(NativeInstance);
          ClassValue.RunFieldInitializers(NativeInstance);
          ClassValue.RunDecoratorFieldInitializers(NativeInstance);

          if Assigned(ClassValue.ConstructorMethod) then
            ClassValue.ConstructorMethod.Call(Arguments, NativeInstance)
          else if Assigned(ClassValue.SuperClass) and Assigned(ClassValue.SuperClass.ConstructorMethod) then
            ClassValue.SuperClass.ConstructorMethod.Call(Arguments, NativeInstance)
          else
            TGocciaInstanceValue(NativeInstance).InitializeNativeFromArguments(Arguments);

          Result := NativeInstance;
        end
        else
        begin
          Instance := TGocciaInstanceValue.Create(ClassValue);
          Instance.Prototype := ClassValue.Prototype;

          InitContext := AContext;
          InitScope := TGocciaClassInitScope.Create(AContext.Scope, ClassValue);
          InitScope.ThisValue := Instance;
          InitContext.Scope := InitScope;

          InitializeInstanceProperties(Instance, ClassValue, InitContext);

          if Assigned(ClassValue.SuperClass) then
          begin
            SuperInitContext := AContext;
            SuperInitScope := TGocciaClassInitScope.Create(AContext.Scope, ClassValue.SuperClass);
            SuperInitScope.ThisValue := Instance;
            SuperInitContext.Scope := SuperInitScope;
            InitializePrivateInstanceProperties(Instance, ClassValue.SuperClass, SuperInitContext);
          end;

          InitializePrivateInstanceProperties(Instance, ClassValue, InitContext);

          ClassValue.RunMethodInitializers(Instance);
          ClassValue.RunFieldInitializers(Instance);
          ClassValue.RunDecoratorFieldInitializers(Instance);

          if Assigned(ClassValue.ConstructorMethod) then
            ClassValue.ConstructorMethod.Call(Arguments, Instance)
          else if Assigned(ClassValue.SuperClass) and Assigned(ClassValue.SuperClass.ConstructorMethod) then
            ClassValue.SuperClass.ConstructorMethod.Call(Arguments, Instance);

          Result := Instance;
        end;
      end
      else if Callee is TGocciaNativeFunctionValue then
      begin
        Result := TGocciaNativeFunctionValue(Callee).Call(Arguments, TGocciaUndefinedLiteralValue.UndefinedValue);
      end
      else
      begin
        ThrowTypeError(Callee.TypeName + ' is not a constructor');
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
begin
  ClassDef := AClassExpression.ClassDefinition;
  Result := EvaluateClassDefinition(ClassDef, AContext, AClassExpression.Line, AClassExpression.Column);

  // For class expressions, don't automatically bind to scope - just return the class value
  // If it's a named class expression, the name is only visible inside the class methods
end;

function EvaluateClassDefinition(const AClassDef: TGocciaClassDefinition; const AContext: TGocciaEvaluationContext; const ALine, AColumn: Integer): TGocciaClassValue;
var
  SuperClass: TGocciaClassValue;
  SuperClassValue: TGocciaValue;
  ClassValue: TGocciaClassValue;
  MethodPair: TPair<string, TGocciaClassMethod>;
  PropertyPair: TPair<string, TGocciaExpression>;
  PropertyEntry: TOrderedMap<TGocciaExpression>.TKeyValuePair;
  GetterPair: TPair<string, TGocciaGetterExpression>;
  SetterPair: TPair<string, TGocciaSetterExpression>;
  Method: TGocciaMethodValue;
  ComputedKey: TGocciaValue;
  PropertyValue: TGocciaValue;
  PropertyExpr: TGocciaExpression;
  GetterFunction, SetterFunction: TGocciaFunctionValue;
  ClassName: string;
  I, J: Integer;
  HasDecorators: Boolean;
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

  // Handle methods
  for MethodPair in AClassDef.Methods do
  begin
    // Pass superclass directly to method creation
    Method := TGocciaMethodValue(EvaluateClassMethod(MethodPair.Value, AContext, SuperClass));
    Method.OwningClass := ClassValue;

    if MethodPair.Value.IsStatic then
    begin
      // Static methods are added as properties on the class constructor itself
      ClassValue.SetProperty(MethodPair.Key, Method);
    end
    else
    begin
      // Instance methods are added to the class prototype
      ClassValue.AddMethod(MethodPair.Key, Method);
    end;
  end;

  // Handle static properties
  for PropertyPair in AClassDef.StaticProperties do
  begin
    // Evaluate the property value and set it on the class constructor
    PropertyValue := EvaluateExpression(PropertyPair.Value, AContext);
    ClassValue.SetProperty(PropertyPair.Key, PropertyValue);
  end;

  // Handle private static properties
  for PropertyPair in AClassDef.PrivateStaticProperties do
  begin
    // Evaluate the property value and set it on the class as private static property
    PropertyValue := EvaluateExpression(PropertyPair.Value, AContext);
    ClassValue.AddPrivateStaticProperty(PropertyPair.Key, PropertyValue);
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

  // Store private methods on the class
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
    // Private getters are prefixed with '#' by the parser
    if (Length(GetterPair.Key) > 0) and (GetterPair.Key[1] = '#') then
      ClassValue.AddPrivateGetter(Copy(GetterPair.Key, 2, Length(GetterPair.Key) - 1), GetterFunction)
    else
      ClassValue.AddGetter(GetterPair.Key, GetterFunction);
  end;

  for SetterPair in AClassDef.Setters do
  begin
    SetterFunction := TGocciaFunctionValue(EvaluateSetter(SetterPair.Value, AContext));
    // Private setters are prefixed with '#' by the parser
    if (Length(SetterPair.Key) > 0) and (SetterPair.Key[1] = '#') then
      ClassValue.AddPrivateSetter(Copy(SetterPair.Key, 2, Length(SetterPair.Key) - 1), SetterFunction)
    else
      ClassValue.AddSetter(SetterPair.Key, SetterFunction);
  end;

  // Handle named static getters
  for GetterPair in AClassDef.FStaticGetters do
  begin
    GetterFunction := TGocciaFunctionValue(EvaluateGetter(GetterPair.Value, AContext));
    ClassValue.AddStaticGetter(GetterPair.Key, GetterFunction);
  end;

  // Handle named static setters
  for SetterPair in AClassDef.FStaticSetters do
  begin
    SetterFunction := TGocciaFunctionValue(EvaluateSetter(SetterPair.Value, AContext));
    ClassValue.AddStaticSetter(SetterPair.Key, SetterFunction);
  end;

  // Handle computed accessors in source declaration order via FElements
  for I := 0 to High(AClassDef.FElements) do
  begin
    Elem := AClassDef.FElements[I];
    if not Elem.IsComputed then
      Continue;
    if not (Elem.Kind in [cekGetter, cekSetter]) then
      Continue;

    ComputedKey := EvaluateExpression(Elem.ComputedKeyExpression, AContext);

    case Elem.Kind of
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

  // TC39 proposal-decorators §3.2 ApplyDecoratorsToClassDefinition
  HasDecorators := (Length(AClassDef.FDecorators) > 0) or (Length(AClassDef.FElements) > 0);

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
    TGocciaGarbageCollector.Instance.AddTempRoot(MetadataObject);

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
          ThrowTypeError('Decorator must be a function');

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
        ThrowTypeError('Decorator must be a function');

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
      TGocciaGarbageCollector.Instance.RemoveTempRoot(MetadataObject);
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

function EvaluatePrivateMemberOnInstance(const AInstance: TGocciaInstanceValue; const APrivateName: string; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  AccessClass: TGocciaClassValue;
  GetterFn: TGocciaFunctionValue;
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
    // Private static field access on class
    ClassValue := TGocciaClassValue(ObjectValue);

    // Access private static property
    Result := ClassValue.GetPrivateStaticProperty(APrivateMemberExpression.PrivateName);
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
    // Private static field access on class
    ClassValue := TGocciaClassValue(AObjectValue);

    // Access private static property
    Result := ClassValue.GetPrivateStaticProperty(APrivateMemberExpression.PrivateName);
  end
  else
  begin
    AContext.OnError(Format('Private fields can only be accessed on class instances or classes, not %s', [AObjectValue.TypeName]),
      APrivateMemberExpression.Line, APrivateMemberExpression.Column);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function EvaluatePrivatePropertyAssignment(const APrivatePropertyAssignmentExpression: TGocciaPrivatePropertyAssignmentExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  ObjectValue: TGocciaValue;
  Instance: TGocciaInstanceValue;
  ClassValue: TGocciaClassValue;
  AccessClass: TGocciaClassValue;
  Value: TGocciaValue;
  SetterFn: TGocciaFunctionValue;
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
    else
    begin
      // Set the private property
      Instance.SetPrivateProperty(APrivatePropertyAssignmentExpression.PrivateName, Value, AccessClass);
    end;
  end
  else if ObjectValue is TGocciaClassValue then
  begin
    // Private static field assignment on class
    ClassValue := TGocciaClassValue(ObjectValue);

    // Set the private static property
    ClassValue.AddPrivateStaticProperty(APrivatePropertyAssignmentExpression.PrivateName, Value);
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

function EvaluatePrivatePropertyCompoundAssignment(const APrivatePropertyCompoundAssignmentExpression: TGocciaPrivatePropertyCompoundAssignmentExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  ObjectValue: TGocciaValue;
  Instance: TGocciaInstanceValue;
  ClassValue: TGocciaClassValue;
  AccessClass: TGocciaClassValue;
  Value: TGocciaValue;
  CurrentValue: TGocciaValue;
begin
  // Evaluate the object expression
  ObjectValue := EvaluateExpression(APrivatePropertyCompoundAssignmentExpression.ObjectExpr, AContext);

  // Evaluate the value to operate with
  Value := EvaluateExpression(APrivatePropertyCompoundAssignmentExpression.Value, AContext);

  if ObjectValue is TGocciaInstanceValue then
  begin
    // Private field compound assignment on instance
    Instance := TGocciaInstanceValue(ObjectValue);

    // Determine the access class from the owning class of the current method
    AccessClass := ResolveOwningClass(Instance, AContext);

    // Get the current value of the private property
    CurrentValue := Instance.GetPrivateProperty(APrivatePropertyCompoundAssignmentExpression.PrivateName, AccessClass);
  end
  else if ObjectValue is TGocciaClassValue then
  begin
    // Private static field compound assignment on class
    ClassValue := TGocciaClassValue(ObjectValue);

    // Get the current value of the private static property
    CurrentValue := ClassValue.GetPrivateStaticProperty(APrivatePropertyCompoundAssignmentExpression.PrivateName);
  end
  else
  begin
    AContext.OnError(Format('Private fields can only be accessed on class instances or classes, not %s', [ObjectValue.TypeName]),
      APrivatePropertyCompoundAssignmentExpression.Line, APrivatePropertyCompoundAssignmentExpression.Column);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  // Use shared compound operation function
  Result := PerformCompoundOperation(CurrentValue, Value, APrivatePropertyCompoundAssignmentExpression.Operator);

  // Set the new value
  if ObjectValue is TGocciaInstanceValue then
    Instance.SetPrivateProperty(APrivatePropertyCompoundAssignmentExpression.PrivateName, Result, AccessClass)
  else if ObjectValue is TGocciaClassValue then
    ClassValue.AddPrivateStaticProperty(APrivatePropertyCompoundAssignmentExpression.PrivateName, Result);
end;

procedure InitializePrivateInstanceProperties(const AInstance: TGocciaInstanceValue; const AClassValue: TGocciaClassValue; const AContext: TGocciaEvaluationContext);
var
  PropertyValue: TGocciaValue;
  Entry: TOrderedMap<TGocciaExpression>.TKeyValuePair;
  I: Integer;
begin
  for I := 0 to AClassValue.PrivateInstancePropertyDefs.Count - 1 do
  begin
    Entry := AClassValue.PrivateInstancePropertyDefs.EntryAt(I);
    PropertyValue := EvaluateExpression(Entry.Value, AContext);
    AInstance.SetPrivateProperty(Entry.Key, PropertyValue, AClassValue);
  end;
end;

function EvaluateTemplateLiteral(const ATemplateLiteralExpression: TGocciaTemplateLiteralExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Template: string;
  SB: TStringBuilder;
  I, Start: Integer;
  BraceCount: Integer;
  ExpressionText: string;
  ExpressionValue: TGocciaValue;
begin
  Template := ATemplateLiteralExpression.Value;
  SB := TStringBuilder.Create;
  try
    I := 1;
    while I <= Length(Template) do
    begin
      if (I < Length(Template)) and (Template[I] = '$') and (Template[I + 1] = '{') then
      begin
        I := I + 2;
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

        if BraceCount > 0 then
        begin
          AContext.OnError('Unterminated template expression', ATemplateLiteralExpression.Line, ATemplateLiteralExpression.Column);
          Result := TGocciaStringLiteralValue.Create(Template);
          Exit;
        end;

        ExpressionText := Trim(Copy(Template, Start, I - Start));
        if ExpressionText <> '' then
        begin
          ExpressionValue := EvaluateTemplateExpression(ExpressionText, AContext, ATemplateLiteralExpression.Line, ATemplateLiteralExpression.Column);
          if ExpressionValue <> nil then
          begin
            if ExpressionValue is TGocciaSymbolValue then
              ThrowTypeError('Cannot convert a Symbol value to a string');
            // ES2026 §13.15.5.1 step 5e: ToString(value) on each substitution
            SB.Append(ToECMAString(ExpressionValue).Value);
          end
          else
            SB.Append('undefined');
        end;

        Inc(I);
      end
      else
      begin
        SB.Append(Template[I]);
        Inc(I);
      end;
    end;

    Result := TGocciaStringLiteralValue.Create(SB.ToString);
  finally
    SB.Free;
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
        Result := TGocciaNullLiteralValue.Create;
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
            ThrowTypeError('Cannot convert a Symbol value to a string');
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
    ThrowTypeError('Cannot destructure null or undefined');

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
          if ArrayValue.Elements[J] <> nil then
            RestElements.Elements.Add(ArrayValue.Elements[J])
          else
            RestElements.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue);
        end;
        AssignPattern(TGocciaRestDestructuringPattern(APattern.Elements[I]).Argument, RestElements, AContext, AIsDeclaration, ADeclarationType);
        Break;
      end
      else
      begin
        if (I < ArrayValue.Elements.Count) and (ArrayValue.Elements[I] <> nil) then
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
      ThrowTypeError('Value is not iterable');

    TGocciaGarbageCollector.Instance.AddTempRoot(Iterator);
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
      TGocciaGarbageCollector.Instance.RemoveTempRoot(Iterator);
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
      ThrowTypeError('Cannot destructure null or undefined')
    else
      ThrowTypeError('Cannot destructure non-object value');
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
        ArrayValue.Elements[Index] := nil;
        Result := TGocciaBooleanLiteralValue.TrueValue;
      end
      else
      begin
        if not ArrayValue.DeleteProperty(PropertyName) then
          ThrowTypeError('Cannot delete property ''' + PropertyName + ''' of [object Array]');
        Result := TGocciaBooleanLiteralValue.TrueValue;
      end;
    end
    else if ObjValue is TGocciaObjectValue then
    begin
      ObjectValue := TGocciaObjectValue(ObjValue);
      if not ObjectValue.DeleteProperty(PropertyName) then
        ThrowTypeError('Cannot delete property ''' + PropertyName + ''' of [object Object]');
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
