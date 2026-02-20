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
function EvaluateDestructuringDeclaration(const ADestructuringDeclaration: TGocciaDestructuringDeclaration; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateTemplateLiteral(const ATemplateLiteralExpression: TGocciaTemplateLiteralExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateTemplateExpression(const AExpressionText: string; const AContext: TGocciaEvaluationContext; const ALine, AColumn: Integer): TGocciaValue;

// Destructuring pattern assignment procedures
procedure AssignPattern(const APattern: TGocciaDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False);
procedure AssignIdentifierPattern(const APattern: TGocciaIdentifierDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False);
procedure AssignArrayPattern(const APattern: TGocciaArrayDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False);
procedure AssignObjectPattern(const APattern: TGocciaObjectDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False);
procedure AssignAssignmentPattern(const APattern: TGocciaAssignmentDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False);
procedure AssignRestPattern(const APattern: TGocciaRestDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False);
procedure InitializeInstanceProperties(const AInstance: TGocciaInstanceValue; const AClassValue: TGocciaClassValue; const AContext: TGocciaEvaluationContext);
procedure InitializePrivateInstanceProperties(const AInstance: TGocciaInstanceValue; const AClassValue: TGocciaClassValue; const AContext: TGocciaEvaluationContext);

function IsObjectInstanceOfClass(const AObj: TGocciaObjectValue; const AClassValue: TGocciaClassValue): Boolean;

// Helper function to safely call OnError with proper error handling
procedure SafeOnError(const AContext: TGocciaEvaluationContext; const AMessage: string; const ALine, AColumn: Integer);

implementation

uses
  Classes,
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Error,
  Goccia.Evaluator.Arithmetic,
  Goccia.Evaluator.Assignment,
  Goccia.Evaluator.Bitwise,
  Goccia.Evaluator.Comparison,
  Goccia.Evaluator.TypeOperations,
  Goccia.GarbageCollector,
  Goccia.Keywords.Reserved,
  Goccia.Lexer,
  Goccia.Parser,
  Goccia.Token,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.Constants,
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
  Goccia.Values.SetValue,
  Goccia.Values.SymbolValue;

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
      TGocciaGarbageCollector.Instance.AddTempRoot(AValue);
      try
        CallArgs := TGocciaArgumentsCollection.Create;
        try
          IteratorObj := TGocciaFunctionBase(IteratorMethod).Call(CallArgs, AValue);
        finally
          CallArgs.Free;
        end;
      finally
        TGocciaGarbageCollector.Instance.RemoveTempRoot(AValue);
      end;
      if IteratorObj is TGocciaIteratorValue then
      begin
        Result := TGocciaIteratorValue(IteratorObj);
        Exit;
      end;
      if IteratorObj is TGocciaObjectValue then
      begin
        NextMethod := IteratorObj.GetProperty('next');
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

procedure SpreadIterableInto(const ASpreadValue: TGocciaValue; const ATarget: TList<TGocciaValue>);
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
        while not IterResult.GetProperty('done').ToBooleanLiteral.Value do
        begin
          ATarget.Add(IterResult.GetProperty('value'));
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
        AArgs.Add(TGocciaUndefinedLiteralValue.UndefinedValue)
      else
        AArgs.Add(SpreadArray.Elements[J]);
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
        while not IterResult.GetProperty('done').ToBooleanLiteral.Value do
        begin
          AArgs.Add(IterResult.GetProperty('value'));
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
  LeftExpr: TGocciaExpression;
  Value: TGocciaValue;
  Obj: TGocciaValue;
  PropName: string;
  PropertyValue: TGocciaValue;
  I: Integer;
  Callee: TGocciaValue;
  Arguments: TObjectList<TGocciaValue>;
  OldValue: TGocciaValue; // For increment/decrement operations
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
    if (PropertyValue is TGocciaSymbolValue) and (Obj is TGocciaObjectValue) then
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
    PropName := EvaluateExpression(TGocciaComputedPropertyCompoundAssignmentExpression(AExpression).PropertyExpression, AContext).ToStringLiteral.Value;
    Value := EvaluateExpression(TGocciaComputedPropertyCompoundAssignmentExpression(AExpression).Value, AContext);
    PerformPropertyCompoundAssignment(Obj, PropName, Value, TGocciaComputedPropertyCompoundAssignmentExpression(AExpression).Operator, AContext.OnError, AExpression.Line, AExpression.Column);

    // Get the final result
    Result := Obj.GetProperty(PropName);
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
      Result := TGocciaNativeFunctionValue(Callee).Call(Arguments, ThisValue)
    else if Callee is TGocciaFunctionValue then
      Result := TGocciaFunctionValue(Callee).Call(Arguments, ThisValue)
    else if Callee is TGocciaBoundFunctionValue then
      Result := TGocciaBoundFunctionValue(Callee).Call(Arguments, ThisValue)
    else if Callee is TGocciaClassValue then
      Result := TGocciaClassValue(Callee).Call(Arguments, ThisValue)
    else
    begin
      SafeOnError(AContext, Format('%s is not a function', [Callee.TypeName]), ACallExpression.Line, ACallExpression.Column);
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
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
      begin
        AContext.OnError('Cannot read property ''Symbol()'' of ' + Obj.ToStringLiteral.Value,
          AMemberExpression.Line, AMemberExpression.Column);
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
        Exit;
      end;
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
      AContext.OnError('Cannot read property ''' + PropertyName + ''' of ' + Obj.ToStringLiteral.Value,
        AMemberExpression.Line, AMemberExpression.Column);
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
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
  Obj := TGocciaObjectValue.Create;

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
                // Also spread symbol properties
                for SymbolEntry in SpreadObj.GetEnumerableSymbolProperties do
                  Obj.AssignSymbolProperty(SymbolEntry.Key, SymbolEntry.Value);
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
                Obj.AssignSymbolProperty(TGocciaSymbolValue(PropertyValue), EvaluateExpression(ComputedPair.Value, AContext))
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

  // Arrow functions always use lexical this per ECMAScript spec
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

  // Shorthand methods use call-site this like regular functions
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
    Result := CreateErrorObject('TypeError', E.Message)
  else if E is TGocciaReferenceError then
    Result := CreateErrorObject('ReferenceError', E.Message)
  else
    Result := CreateErrorObject('Error', E.Message);
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

  // Always create a unique child scope for the closure - now with default parameter support
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

procedure InitializeInstanceProperties(const AInstance: TGocciaInstanceValue; const AClassValue: TGocciaClassValue; const AContext: TGocciaEvaluationContext);
var
  PropertyValue: TGocciaValue;
  PropertyExpr: TGocciaExpression;
  I: Integer;
  PropName: string;
begin
  // Initialize inherited properties first (parent to child order)
  if Assigned(AClassValue.SuperClass) then
    InitializeInstanceProperties(AInstance, AClassValue.SuperClass, AContext);

  // Then initialize this class's properties in declaration order (child properties can shadow parent properties)
  for I := 0 to AClassValue.InstancePropertyOrder.Count - 1 do
  begin
    PropName := AClassValue.InstancePropertyOrder[I];
    if AClassValue.InstancePropertyDefs.TryGetValue(PropName, PropertyExpr) then
    begin
      // Evaluate the property expression in the current context
      PropertyValue := EvaluateExpression(PropertyExpr, AContext);
      AInstance.AssignProperty(PropName, PropertyValue);
    end;
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

        if Assigned(ClassValue.ConstructorMethod) then
          ClassValue.ConstructorMethod.Call(Arguments, Instance)
        else if Assigned(ClassValue.SuperClass) and Assigned(ClassValue.SuperClass.ConstructorMethod) then
          ClassValue.SuperClass.ConstructorMethod.Call(Arguments, Instance);

        Result := Instance;
      end;
    end
    else if Callee is TGocciaNativeFunctionValue then
    begin
      // Native function constructors (e.g. Error, TypeError, etc.)
      Result := TGocciaNativeFunctionValue(Callee).Call(Arguments, TGocciaUndefinedLiteralValue.UndefinedValue);
    end
    else
    begin
      // Throw proper TypeError for non-constructors
      ThrowTypeError(Callee.TypeName + ' is not a constructor');
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
  GetterPair: TPair<string, TGocciaGetterExpression>;
  SetterPair: TPair<string, TGocciaSetterExpression>;
  Method: TGocciaMethodValue;
  ComputedKey: TGocciaValue;
  PropertyValue: TGocciaValue;
  PropertyExpr: TGocciaExpression;
  GetterFunction, SetterFunction: TGocciaFunctionValue;
  ClassName: string;
  I: Integer;
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
  ClassValue.Prototype.AssignProperty('constructor', ClassValue);

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
  for I := 0 to AClassDef.InstancePropertyOrder.Count - 1 do
  begin
    if AClassDef.InstanceProperties.TryGetValue(AClassDef.InstancePropertyOrder[I], PropertyExpr) then
      ClassValue.AddInstanceProperty(AClassDef.InstancePropertyOrder[I], PropertyExpr);
  end;

  // Store private instance property definitions on the class in declaration order
  for I := 0 to AClassDef.PrivateInstancePropertyOrder.Count - 1 do
  begin
    if AClassDef.PrivateInstanceProperties.TryGetValue(AClassDef.PrivateInstancePropertyOrder[I], PropertyExpr) then
      ClassValue.AddPrivateInstanceProperty(AClassDef.PrivateInstancePropertyOrder[I], PropertyExpr);
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

  // Handle computed static getters (e.g., static get [Symbol.species]())
  for I := 0 to Length(AClassDef.FComputedStaticGetters) - 1 do
  begin
    ComputedKey := EvaluateExpression(AClassDef.FComputedStaticGetters[I].KeyExpression, AContext);
    GetterFunction := TGocciaFunctionValue(EvaluateGetter(AClassDef.FComputedStaticGetters[I].GetterExpression, AContext));
    if ComputedKey is TGocciaSymbolValue then
      ClassValue.DefineSymbolProperty(
        TGocciaSymbolValue(ComputedKey),
        TGocciaPropertyDescriptorAccessor.Create(GetterFunction, nil, [pfConfigurable]))
    else
      ClassValue.AddStaticGetter(ComputedKey.ToStringLiteral.Value, GetterFunction);
  end;

  // For class expressions, don't automatically bind to scope - just return the class value
  // If it's a named class expression, the name is only visible inside the class methods
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
    GetterFn := AccessClass.GetPrivateGetter(APrivateName);
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
      SetterFn := AccessClass.GetPrivateSetter(APrivatePropertyAssignmentExpression.PrivateName);
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
  PropertyExpr: TGocciaExpression;
  I: Integer;
  PropName: string;
begin
  // Initialize private instance properties in declaration order (only from the exact class, not inherited)
  for I := 0 to AClassValue.PrivateInstancePropertyOrder.Count - 1 do
  begin
    PropName := AClassValue.PrivateInstancePropertyOrder[I];
    if AClassValue.PrivateInstancePropertyDefs.TryGetValue(PropName, PropertyExpr) then
    begin
      // Evaluate the property expression in the current context
      PropertyValue := EvaluateExpression(PropertyExpr, AContext);
      AInstance.SetPrivateProperty(PropName, PropertyValue, AClassValue);
    end;
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
            SB.Append(ExpressionValue.ToStringLiteral.Value);
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
  AssignPattern(ADestructuringDeclaration.Pattern, Value, AContext, True); // IsDeclaration = True

  Result := Value;
end;

procedure AssignPattern(const APattern: TGocciaDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False);
begin
  if APattern is TGocciaIdentifierDestructuringPattern then
    AssignIdentifierPattern(TGocciaIdentifierDestructuringPattern(APattern), AValue, AContext, AIsDeclaration)
  else if APattern is TGocciaArrayDestructuringPattern then
    AssignArrayPattern(TGocciaArrayDestructuringPattern(APattern), AValue, AContext, AIsDeclaration)
  else if APattern is TGocciaObjectDestructuringPattern then
    AssignObjectPattern(TGocciaObjectDestructuringPattern(APattern), AValue, AContext, AIsDeclaration)
  else if APattern is TGocciaAssignmentDestructuringPattern then
    AssignAssignmentPattern(TGocciaAssignmentDestructuringPattern(APattern), AValue, AContext, AIsDeclaration)
  else if APattern is TGocciaRestDestructuringPattern then
    AssignRestPattern(TGocciaRestDestructuringPattern(APattern), AValue, AContext, AIsDeclaration);
end;

procedure AssignIdentifierPattern(const APattern: TGocciaIdentifierDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False);
begin
  if AIsDeclaration then
    AContext.Scope.DefineLexicalBinding(APattern.Name, AValue, dtLet)
  else
    AContext.Scope.AssignLexicalBinding(APattern.Name, AValue);
end;

procedure AssignArrayPattern(const APattern: TGocciaArrayDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False);
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
        AssignPattern(TGocciaRestDestructuringPattern(APattern.Elements[I]).Argument, RestElements, AContext, AIsDeclaration);
        Break;
      end
      else
      begin
        if (I < ArrayValue.Elements.Count) and (ArrayValue.Elements[I] <> nil) then
          ElementValue := ArrayValue.Elements[I]
        else
          ElementValue := TGocciaUndefinedLiteralValue.UndefinedValue;

        AssignPattern(APattern.Elements[I], ElementValue, AContext, AIsDeclaration);
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
        AssignPattern(TGocciaRestDestructuringPattern(APattern.Elements[I]).Argument, RestElements, AContext, AIsDeclaration);
        Break;
      end
      else
      begin
        if I + 1 <= Length(AValue.ToStringLiteral.Value) then
          ElementValue := TGocciaStringLiteralValue.Create(AValue.ToStringLiteral.Value[I + 1])
        else
          ElementValue := TGocciaUndefinedLiteralValue.UndefinedValue;

        AssignPattern(APattern.Elements[I], ElementValue, AContext, AIsDeclaration);
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
          while not IterResult.GetProperty('done').ToBooleanLiteral.Value do
          begin
            RestElements.Elements.Add(IterResult.GetProperty('value'));
            IterResult := Iterator.AdvanceNext;
          end;
          AssignPattern(TGocciaRestDestructuringPattern(APattern.Elements[I]).Argument, RestElements, AContext, AIsDeclaration);
          Break;
        end
        else
        begin
          IterResult := Iterator.AdvanceNext;
          if IterResult.GetProperty('done').ToBooleanLiteral.Value then
            ElementValue := TGocciaUndefinedLiteralValue.UndefinedValue
          else
            ElementValue := IterResult.GetProperty('value');

          AssignPattern(APattern.Elements[I], ElementValue, AContext, AIsDeclaration);
        end;
      end;
    finally
      TGocciaGarbageCollector.Instance.RemoveTempRoot(Iterator);
    end;
  end;
end;

procedure AssignObjectPattern(const APattern: TGocciaObjectDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False);
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
        AssignPattern(TGocciaRestDestructuringPattern(Prop.Pattern).Argument, RestObject, AContext, AIsDeclaration);
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
            AssignPattern(Prop.Pattern, PropValue, AContext, AIsDeclaration);
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
        AssignPattern(Prop.Pattern, PropValue, AContext, AIsDeclaration);
      end;
    end;
  finally
    UsedKeys.Free;
  end;
end;

procedure AssignAssignmentPattern(const APattern: TGocciaAssignmentDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False);
var
  DefaultValue: TGocciaValue;
begin
  // Use default value if the value is undefined
  if AValue is TGocciaUndefinedLiteralValue then
  begin
    DefaultValue := EvaluateExpression(APattern.Right, AContext);
    AssignPattern(APattern.Left, DefaultValue, AContext, AIsDeclaration);
  end
  else
  begin
    AssignPattern(APattern.Left, AValue, AContext, AIsDeclaration);
  end;
end;

procedure AssignRestPattern(const APattern: TGocciaRestDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False);
begin
  // Rest patterns are handled by their containing array/object patterns
  AssignPattern(APattern.Argument, AValue, AContext, AIsDeclaration);
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
