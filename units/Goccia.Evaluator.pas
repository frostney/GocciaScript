unit Goccia.Evaluator;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Goccia.Interfaces, Goccia.Token, Goccia.Scope, Goccia.Error, Goccia.Error.ThrowErrorCallback, Goccia.Modules, Goccia.Values.NativeFunction, Goccia.Values.ObjectValue, Goccia.Values.FunctionValue, Goccia.Values.FunctionBase, Goccia.Values.ClassValue, Goccia.Values.ArrayValue, Goccia.Values.Error, Goccia.Values.SymbolValue, Goccia.Values.SetValue, Goccia.Values.MapValue, Goccia.AST.Node, Goccia.AST.Expressions, Goccia.AST.Statements, Goccia.Utils, Goccia.Lexer, Goccia.Parser,
  Goccia.Evaluator.Arithmetic, Goccia.Evaluator.Bitwise, Goccia.Evaluator.Comparison, Goccia.Evaluator.TypeOperations, Goccia.Evaluator.Assignment, Goccia.Arguments.Collection, Goccia.Values.ErrorHelper,
  Generics.Collections, SysUtils, Math, Classes;

type
  PGocciaValue = ^TGocciaValue;

  TGocciaEvaluationContext = record
    Scope: TGocciaScope;
    OnError: TGocciaThrowErrorCallback;
    LoadModule: TLoadModuleCallback;
  end;

function Evaluate(Node: TGocciaASTNode; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateExpression(Expression: TGocciaExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateStatement(Statement: TGocciaStatement; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateStatementsSafe(Nodes: TObjectList<TGocciaASTNode>; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateBinary(BinaryExpression: TGocciaBinaryExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateUnary(UnaryExpression: TGocciaUnaryExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateDelete(Operand: TGocciaExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateCall(CallExpression: TGocciaCallExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateMember(MemberExpression: TGocciaMemberExpression; Context: TGocciaEvaluationContext): TGocciaValue; overload;
function EvaluateMember(MemberExpression: TGocciaMemberExpression; Context: TGocciaEvaluationContext; out ObjectValue: TGocciaValue): TGocciaValue; overload;
function EvaluateArray(ArrayExpression: TGocciaArrayExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateObject(ObjectExpression: TGocciaObjectExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateGetter(GetterExpression: TGocciaGetterExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateSetter(SetterExpression: TGocciaSetterExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateArrowFunction(ArrowFunctionExpression: TGocciaArrowFunctionExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateBlock(BlockStatement: TGocciaBlockStatement; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateIf(IfStatement: TGocciaIfStatement; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateTry(TryStatement: TGocciaTryStatement; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateSwitch(SwitchStatement: TGocciaSwitchStatement; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateClassMethod(ClassMethod: TGocciaClassMethod; Context: TGocciaEvaluationContext; SuperClass: TGocciaValue = nil): TGocciaValue;
function EvaluateClass(ClassDeclaration: TGocciaClassDeclaration; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateClassExpression(ClassExpression: TGocciaClassExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateClassDefinition(ClassDef: TGocciaClassDefinition; Context: TGocciaEvaluationContext; Line, Column: Integer): TGocciaClassValue;
function EvaluateNewExpression(NewExpression: TGocciaNewExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluatePrivateMember(PrivateMemberExpression: TGocciaPrivateMemberExpression; Context: TGocciaEvaluationContext): TGocciaValue; overload;
function EvaluatePrivateMember(PrivateMemberExpression: TGocciaPrivateMemberExpression; Context: TGocciaEvaluationContext; out ObjectValue: TGocciaValue): TGocciaValue; overload;
function EvaluatePrivatePropertyAssignment(PrivatePropertyAssignmentExpression: TGocciaPrivatePropertyAssignmentExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluatePrivatePropertyCompoundAssignment(PrivatePropertyCompoundAssignmentExpression: TGocciaPrivatePropertyCompoundAssignmentExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateDestructuringAssignment(DestructuringAssignmentExpression: TGocciaDestructuringAssignmentExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateDestructuringDeclaration(DestructuringDeclaration: TGocciaDestructuringDeclaration; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateTemplateLiteral(TemplateLiteralExpression: TGocciaTemplateLiteralExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateTemplateExpression(const ExpressionText: string; Context: TGocciaEvaluationContext; Line, Column: Integer): TGocciaValue;

// Destructuring pattern assignment procedures
procedure AssignPattern(Pattern: TGocciaDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext; IsDeclaration: Boolean = False);
procedure AssignIdentifierPattern(Pattern: TGocciaIdentifierDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext; IsDeclaration: Boolean = False);
procedure AssignArrayPattern(Pattern: TGocciaArrayDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext; IsDeclaration: Boolean = False);
procedure AssignObjectPattern(Pattern: TGocciaObjectDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext; IsDeclaration: Boolean = False);
procedure AssignAssignmentPattern(Pattern: TGocciaAssignmentDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext; IsDeclaration: Boolean = False);
procedure AssignRestPattern(Pattern: TGocciaRestDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext; IsDeclaration: Boolean = False);
procedure InitializeInstanceProperties(Instance: TGocciaInstanceValue; ClassValue: TGocciaClassValue; Context: TGocciaEvaluationContext);
procedure InitializePrivateInstanceProperties(Instance: TGocciaInstanceValue; ClassValue: TGocciaClassValue; Context: TGocciaEvaluationContext);

function IsObjectInstanceOfClass(Obj: TGocciaObjectValue; ClassValue: TGocciaClassValue): Boolean;

// Helper function to safely call OnError with proper error handling
procedure SafeOnError(Context: TGocciaEvaluationContext; const Message: string; Line, Column: Integer);

implementation

  uses
    Goccia.Values.ObjectPropertyDescriptor,
    Goccia.Values.ClassHelper,
    Goccia.Values.Constants;

// Helper function to safely call OnError with proper error handling
procedure SafeOnError(Context: TGocciaEvaluationContext; const Message: string; Line, Column: Integer);
begin
  if Assigned(Context.OnError) then
    Context.OnError(Message, Line, Column);
end;

// Helper: create a non-owning copy of a statement list (AST owns the nodes)
function CopyStatementList(Source: TObjectList<TGocciaASTNode>): TObjectList<TGocciaASTNode>;
var
  I: Integer;
begin
  Result := TObjectList<TGocciaASTNode>.Create(False);
  for I := 0 to Source.Count - 1 do
    Result.Add(Source[I]);
end;

function EvaluateStatementsSafe(Nodes: TObjectList<TGocciaASTNode>; Context: TGocciaEvaluationContext): TGocciaValue;
var
  I: Integer;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := 0 to Nodes.Count - 1 do
  begin
    try
      Result := Evaluate(Nodes[I], Context);
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

procedure SpreadIterableInto(SpreadValue: TGocciaValue; Target: TList<TGocciaValue>);
var
  SpreadArray: TGocciaArrayValue;
  J: Integer;
begin
  if SpreadValue is TGocciaArrayValue then
  begin
    SpreadArray := TGocciaArrayValue(SpreadValue);
    for J := 0 to SpreadArray.Elements.Count - 1 do
    begin
      if SpreadArray.Elements[J] = nil then
        Target.Add(TGocciaUndefinedLiteralValue.UndefinedValue)
      else
        Target.Add(SpreadArray.Elements[J]);
    end;
  end
  else if SpreadValue is TGocciaStringLiteralValue then
  begin
    for J := 1 to Length(SpreadValue.ToStringLiteral.Value) do
      Target.Add(TGocciaStringLiteralValue.Create(SpreadValue.ToStringLiteral.Value[J]));
  end
  else if SpreadValue is TGocciaSetValue then
  begin
    SpreadArray := TGocciaSetValue(SpreadValue).ToArray;
    for J := 0 to SpreadArray.Elements.Count - 1 do
      Target.Add(SpreadArray.Elements[J]);
  end
  else if SpreadValue is TGocciaMapValue then
  begin
    SpreadArray := TGocciaMapValue(SpreadValue).ToArray;
    for J := 0 to SpreadArray.Elements.Count - 1 do
      Target.Add(SpreadArray.Elements[J]);
  end
  else
    ThrowTypeError('Spread syntax requires an iterable');
end;

procedure SpreadIterableIntoArgs(SpreadValue: TGocciaValue; Args: TGocciaArgumentsCollection);
var
  SpreadArray: TGocciaArrayValue;
  J: Integer;
begin
  if SpreadValue is TGocciaArrayValue then
  begin
    SpreadArray := TGocciaArrayValue(SpreadValue);
    for J := 0 to SpreadArray.Elements.Count - 1 do
    begin
      if SpreadArray.Elements[J] = nil then
        Args.Add(TGocciaUndefinedLiteralValue.UndefinedValue)
      else
        Args.Add(SpreadArray.Elements[J]);
    end;
  end
  else if SpreadValue is TGocciaStringLiteralValue then
  begin
    for J := 1 to Length(SpreadValue.ToStringLiteral.Value) do
      Args.Add(TGocciaStringLiteralValue.Create(SpreadValue.ToStringLiteral.Value[J]));
  end
  else
    ThrowTypeError('Spread syntax requires an iterable');
end;

function Evaluate(Node: TGocciaASTNode; Context: TGocciaEvaluationContext): TGocciaValue;
begin
  // Propagate OnError onto the scope so closures inherit it
  if Assigned(Context.OnError) and not Assigned(Context.Scope.OnError) then
    Context.Scope.OnError := Context.OnError;

  if Node is TGocciaExpression then
  begin
    Result := EvaluateExpression(TGocciaExpression(Node), Context);
  end
  else if Node is TGocciaStatement then
  begin
    Result := EvaluateStatement(TGocciaStatement(Node), Context);
  end
  else
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function EvaluateExpression(Expression: TGocciaExpression; Context: TGocciaEvaluationContext): TGocciaValue;
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
  if Expression is TGocciaLiteralExpression then
  begin
    Result := TGocciaLiteralExpression(Expression).Value.RuntimeCopy;
  end
  else if Expression is TGocciaTemplateLiteralExpression then
    Result := EvaluateTemplateLiteral(TGocciaTemplateLiteralExpression(Expression), Context)
  else if Expression is TGocciaIdentifierExpression then
  begin
    // In strict mode, GetValue throws ReferenceError for undefined variables
    Result := Context.Scope.GetValue(TGocciaIdentifierExpression(Expression).Name);
  end
  else if Expression is TGocciaBinaryExpression then
    Result := EvaluateBinary(TGocciaBinaryExpression(Expression), Context)
  else if Expression is TGocciaUnaryExpression then
    Result := EvaluateUnary(TGocciaUnaryExpression(Expression), Context)
  else if Expression is TGocciaAssignmentExpression then
  begin
    // Variable assignment
    Result := EvaluateExpression(TGocciaAssignmentExpression(Expression).Value, Context);
    Context.Scope.AssignLexicalBinding(TGocciaAssignmentExpression(Expression).Name, Result);
  end
  else if Expression is TGocciaPropertyAssignmentExpression then
  begin
    // Property assignment
    Obj := EvaluateExpression(TGocciaPropertyAssignmentExpression(Expression).ObjectExpr, Context);
    Value := EvaluateExpression(TGocciaPropertyAssignmentExpression(Expression).Value, Context);
    AssignProperty(Obj, TGocciaPropertyAssignmentExpression(Expression).PropertyName, Value, Context.OnError, Expression.Line, Expression.Column);
    Result := Value;
  end
  else if Expression is TGocciaComputedPropertyAssignmentExpression then
  begin
    // Computed property assignment (e.g., obj[expr] = value)
    Obj := EvaluateExpression(TGocciaComputedPropertyAssignmentExpression(Expression).ObjectExpr, Context);
    PropertyValue := EvaluateExpression(TGocciaComputedPropertyAssignmentExpression(Expression).PropertyExpression, Context);
    Value := EvaluateExpression(TGocciaComputedPropertyAssignmentExpression(Expression).Value, Context);
    if (PropertyValue is TGocciaSymbolValue) and (Obj is TGocciaObjectValue) then
      TGocciaObjectValue(Obj).AssignSymbolProperty(TGocciaSymbolValue(PropertyValue), Value)
    else
    begin
      PropName := PropertyValue.ToStringLiteral.Value;
      AssignProperty(Obj, PropName, Value, Context.OnError, Expression.Line, Expression.Column);
    end;
    Result := Value;
  end
  else if Expression is TGocciaCompoundAssignmentExpression then
  begin
    // Variable compound assignment (e.g., count += 5)
    Result := Context.Scope.GetValue(TGocciaCompoundAssignmentExpression(Expression).Name);
    Value := EvaluateExpression(TGocciaCompoundAssignmentExpression(Expression).Value, Context);

    // Use shared compound operation function
    Result := PerformCompoundOperation(Result, Value, TGocciaCompoundAssignmentExpression(Expression).Operator);

    Context.Scope.AssignLexicalBinding(TGocciaCompoundAssignmentExpression(Expression).Name, Result);
  end
  else if Expression is TGocciaPropertyCompoundAssignmentExpression then
  begin
    // Property compound assignment (e.g., obj.count += 5)
    Obj := EvaluateExpression(TGocciaPropertyCompoundAssignmentExpression(Expression).ObjectExpr, Context);
    Value := EvaluateExpression(TGocciaPropertyCompoundAssignmentExpression(Expression).Value, Context);
    PropName := TGocciaPropertyCompoundAssignmentExpression(Expression).PropertyName;
    PerformPropertyCompoundAssignment(Obj, PropName, Value, TGocciaPropertyCompoundAssignmentExpression(Expression).Operator, Context.OnError, Expression.Line, Expression.Column);

    // Get the final result
    Result := Obj.GetProperty(PropName);
    if Result = nil then
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else if Expression is TGocciaComputedPropertyCompoundAssignmentExpression then
  begin
    // Computed property compound assignment (e.g., obj[expr] += value)
    Obj := EvaluateExpression(TGocciaComputedPropertyCompoundAssignmentExpression(Expression).ObjectExpr, Context);
    PropName := EvaluateExpression(TGocciaComputedPropertyCompoundAssignmentExpression(Expression).PropertyExpression, Context).ToStringLiteral.Value;
    Value := EvaluateExpression(TGocciaComputedPropertyCompoundAssignmentExpression(Expression).Value, Context);
    PerformPropertyCompoundAssignment(Obj, PropName, Value, TGocciaComputedPropertyCompoundAssignmentExpression(Expression).Operator, Context.OnError, Expression.Line, Expression.Column);

    // Get the final result
    Result := Obj.GetProperty(PropName);
    if Result = nil then
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else if Expression is TGocciaIncrementExpression then
  begin
    // Increment/decrement expressions (++x, --x, x++, x--)
    if TGocciaIncrementExpression(Expression).Operand is TGocciaIdentifierExpression then
    begin
            // Variable increment/decrement
      PropName := TGocciaIdentifierExpression(TGocciaIncrementExpression(Expression).Operand).Name;
      OldValue := Context.Scope.GetValue(PropName);

      // Calculate new value
      Value := PerformIncrement(OldValue, TGocciaIncrementExpression(Expression).Operator = gttIncrement);

      // Set the new value
      Context.Scope.AssignLexicalBinding(PropName, Value);

      // Return value depends on prefix/postfix
      if TGocciaIncrementExpression(Expression).IsPrefix then
        Result := Value  // Prefix: return new value (++x)
      else
        Result := OldValue;  // Postfix: return old value (x++)
    end
    else if TGocciaIncrementExpression(Expression).Operand is TGocciaMemberExpression then
    begin
      // Property increment/decrement
      Obj := EvaluateExpression(TGocciaMemberExpression(TGocciaIncrementExpression(Expression).Operand).ObjectExpr, Context);
      PropName := TGocciaMemberExpression(TGocciaIncrementExpression(Expression).Operand).PropertyName;

      // Get current property value
      OldValue := Obj.GetProperty(PropName);
      if OldValue = nil then
      begin
        Context.OnError('Cannot access property on non-object', Expression.Line, Expression.Column);
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
        Exit;
      end;

      // Calculate new value
      Value := PerformIncrement(OldValue, TGocciaIncrementExpression(Expression).Operator = gttIncrement);

      // Set the new value (create property if it doesn't exist, like JavaScript)
      DefinePropertyOnValue(Obj, PropName, Value);

      // Return value depends on prefix/postfix
      if TGocciaIncrementExpression(Expression).IsPrefix then
        Result := Value  // Prefix: return new value (++obj.prop)
      else
        Result := OldValue;  // Postfix: return old value (obj.prop++)
    end
    else
    begin
      Context.OnError('Invalid target for increment/decrement', Expression.Line, Expression.Column);
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    end;
  end
  else if Expression is TGocciaCallExpression then
  begin
    Result := EvaluateCall(TGocciaCallExpression(Expression), Context);
  end
  else if Expression is TGocciaMemberExpression then
    Result := EvaluateMember(TGocciaMemberExpression(Expression), Context)
  else if Expression is TGocciaArrayExpression then
    Result := EvaluateArray(TGocciaArrayExpression(Expression), Context)
  else if Expression is TGocciaObjectExpression then
    Result := EvaluateObject(TGocciaObjectExpression(Expression), Context)
  else if Expression is TGocciaArrowFunctionExpression then
    Result := EvaluateArrowFunction(TGocciaArrowFunctionExpression(Expression), Context)
  else if Expression is TGocciaConditionalExpression then
  begin
    if EvaluateExpression(TGocciaConditionalExpression(Expression).Condition, Context).ToBooleanLiteral.Value then
      Result := EvaluateExpression(TGocciaConditionalExpression(Expression).Consequent, Context)
    else
      Result := EvaluateExpression(TGocciaConditionalExpression(Expression).Alternate, Context);
  end
  else if Expression is TGocciaNewExpression then
  begin
    // Handle new expression - create instance
    Result := EvaluateNewExpression(TGocciaNewExpression(Expression), Context);
  end
  else if Expression is TGocciaThisExpression then
  begin
    Result := Context.Scope.ThisValue;
  end
  else if Expression is TGocciaClassExpression then
  begin
    Result := EvaluateClassExpression(TGocciaClassExpression(Expression), Context);
  end
  else if Expression is TGocciaSuperExpression then
  begin
    // Use __super__ from the scope (set by TGocciaMethodValue.Call to the correct
    // superclass for the currently executing method). This is essential for multi-level
    // inheritance: when B extends A and C extends B, calling super() inside B's
    // constructor must resolve to A, not to B (which would happen if we used the
    // instance's class, since the instance is of type C).
    if Context.Scope.Contains('__super__') then
      Result := Context.Scope.GetValue('__super__')
    else if Context.Scope.ThisValue is TGocciaInstanceValue then
    begin
      if Assigned(TGocciaInstanceValue(Context.Scope.ThisValue).ClassValue.SuperClass) then
        Result := TGocciaInstanceValue(Context.Scope.ThisValue).ClassValue.SuperClass
      else
        Context.OnError('No superclass found', Expression.Line, Expression.Column);
    end
    else if Context.Scope.ThisValue is TGocciaClassValue then
    begin
      if Assigned(TGocciaClassValue(Context.Scope.ThisValue).SuperClass) then
        Result := TGocciaClassValue(Context.Scope.ThisValue).SuperClass
      else
        Context.OnError('No superclass found', Expression.Line, Expression.Column);
    end
    else
      Context.OnError('super can only be used in a class method', Expression.Line, Expression.Column);
  end
  else if Expression is TGocciaPrivateMemberExpression then
  begin
    Result := EvaluatePrivateMember(TGocciaPrivateMemberExpression(Expression), Context);
  end
  else if Expression is TGocciaPrivatePropertyAssignmentExpression then
  begin
    Result := EvaluatePrivatePropertyAssignment(TGocciaPrivatePropertyAssignmentExpression(Expression), Context);
  end
  else if Expression is TGocciaPrivatePropertyCompoundAssignmentExpression then
  begin
    Result := EvaluatePrivatePropertyCompoundAssignment(TGocciaPrivatePropertyCompoundAssignmentExpression(Expression), Context);
  end
  else if Expression is TGocciaMemberExpression then
  begin
    if TGocciaMemberExpression(Expression).ObjectExpr is TGocciaThisExpression then
    begin
      // When accessing a property through this, use the current scope's this value
      Result := Context.Scope.GetThisProperty(TGocciaMemberExpression(Expression).PropertyName);
    end
    else
    begin
      // For normal member access, evaluate the object first
      Result := EvaluateMember(TGocciaMemberExpression(Expression), Context);
    end;
  end
  else if Expression is TGocciaHoleExpression then
  begin
    // Return nil for holes - special case for sparse arrays
    Result := nil;
  end
  else if Expression is TGocciaSpreadExpression then
  begin
    // Evaluate the spread expression - this should not be called directly
    // Spread expressions are handled specially in array/object/call contexts
    Context.OnError('Unexpected spread syntax', Expression.Line, Expression.Column);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else if Expression is TGocciaDestructuringAssignmentExpression then
  begin
    Result := EvaluateDestructuringAssignment(TGocciaDestructuringAssignmentExpression(Expression), Context);
  end
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function EvaluateStatement(Statement: TGocciaStatement; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Decl: TGocciaVariableDeclaration;
  ImportDecl: TGocciaImportDeclaration;
  Module: TGocciaModule;
  ImportPair: TPair<string, string>;
  Value: TGocciaValue;
  I: Integer;
begin

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  if Statement is TGocciaExpressionStatement then
  begin
    Result := EvaluateExpression(TGocciaExpressionStatement(Statement).Expression, Context);
  end
      else if Statement is TGocciaVariableDeclaration then
  begin
    Decl := TGocciaVariableDeclaration(Statement);

    // Process each variable in the declaration
    for I := 0 to Length(Decl.Variables) - 1 do
    begin

      Value := EvaluateExpression(Decl.Variables[I].Initializer, Context);

      // Name inference for anonymous functions: const add = (a, b) => a + b; => add.name === "add"
      if (Value is TGocciaFunctionValue) and (TGocciaFunctionValue(Value).Name = '') then
        TGocciaFunctionValue(Value).Name := Decl.Variables[I].Name;

      // Use the new Define/Assign pattern
      if Decl.IsConst then
        Context.Scope.DefineFromToken(Decl.Variables[I].Name, Value, gttConst)
      else
        Context.Scope.DefineFromToken(Decl.Variables[I].Name, Value, gttLet);

    end;
  end
  else if Statement is TGocciaDestructuringDeclaration then
  begin
    Result := EvaluateDestructuringDeclaration(TGocciaDestructuringDeclaration(Statement), Context);
  end
  else if Statement is TGocciaBlockStatement then
  begin
    Result := EvaluateBlock(TGocciaBlockStatement(Statement), Context);
  end
  else if Statement is TGocciaIfStatement then
  begin
    Result := EvaluateIf(TGocciaIfStatement(Statement), Context);
  end
  else if Statement is TGocciaForStatement then
  begin
    // For now, just return undefined since we only want parsing support
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else if Statement is TGocciaWhileStatement then
  begin
    // For now, just return undefined since we only want parsing support
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else if Statement is TGocciaDoWhileStatement then
  begin
    // For now, just return undefined since we only want parsing support
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else if Statement is TGocciaSwitchStatement then
  begin
    Result := EvaluateSwitch(TGocciaSwitchStatement(Statement), Context);
  end
  else if Statement is TGocciaBreakStatement then
  begin
    raise TGocciaBreakSignal.Create;
  end
  else if Statement is TGocciaReturnStatement then
  begin
    if Assigned(TGocciaReturnStatement(Statement).Value) then
    begin
      Value := EvaluateExpression(TGocciaReturnStatement(Statement).Value, Context);
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
  else if Statement is TGocciaThrowStatement then
  begin
    Value := EvaluateExpression(TGocciaThrowStatement(Statement).Value, Context);
    raise TGocciaThrowValue.Create(Value);
  end
  else if Statement is TGocciaTryStatement then
  begin
    Result := EvaluateTry(TGocciaTryStatement(Statement), Context);
  end
  else if Statement is TGocciaClassDeclaration then
  begin
    Result := EvaluateClass(TGocciaClassDeclaration(Statement), Context);
  end
  else if Statement is TGocciaImportDeclaration then
  begin
    ImportDecl := TGocciaImportDeclaration(Statement);
    Module := Context.LoadModule(ImportDecl.ModulePath);

    for ImportPair in ImportDecl.Imports do
    begin
      if Module.ExportsTable.TryGetValue(ImportPair.Value, Value) then
      begin
        Context.Scope.DefineLexicalBinding(ImportPair.Key, Value, dtLet);
      end
      else
      begin
        Context.OnError(Format('Module "%s" has no export named "%s"',
          [ImportDecl.ModulePath, ImportPair.Value]), Statement.Line, Statement.Column);
      end;
    end;
  end;

end;

function EvaluateBinary(BinaryExpression: TGocciaBinaryExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Left, Right: TGocciaValue;
begin
  // Handle short-circuiting logical operators first
  if BinaryExpression.Operator = gttAnd then
  begin
    Left := EvaluateExpression(BinaryExpression.Left, Context);
    if not Left.ToBooleanLiteral.Value then
      Result := Left  // Short-circuit: return left operand if falsy
    else
    begin
      Right := EvaluateExpression(BinaryExpression.Right, Context);
      Result := Right;  // Return right operand
    end;
    Exit;
  end
  else if BinaryExpression.Operator = gttOr then
  begin
    Left := EvaluateExpression(BinaryExpression.Left, Context);
    if Left.ToBooleanLiteral.Value then
      Result := Left  // Short-circuit: return left operand if truthy
    else
    begin
      Right := EvaluateExpression(BinaryExpression.Right, Context);
      Result := Right;  // Return right operand
    end;
    Exit;
  end
  else if BinaryExpression.Operator = gttNullishCoalescing then
  begin
    Left := EvaluateExpression(BinaryExpression.Left, Context);
    // Return right operand only if left is null or undefined
    if (Left is TGocciaNullLiteralValue) or (Left is TGocciaUndefinedLiteralValue) then
    begin
      Right := EvaluateExpression(BinaryExpression.Right, Context);
      Result := Right;
    end
    else
      Result := Left;  // Return left operand for all other values (including falsy ones)
    Exit;
  end;

  // For all other operators, evaluate both operands
  Left := EvaluateExpression(BinaryExpression.Left, Context);
  Right := EvaluateExpression(BinaryExpression.Right, Context);

  case BinaryExpression.Operator of
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
      Result := TGocciaBooleanLiteralValue.Create(IsStrictEqual(Left, Right));
    gttNotEqual:
      Result := TGocciaBooleanLiteralValue.Create(IsNotStrictEqual(Left, Right));
    gttLess:
      Result := TGocciaBooleanLiteralValue.Create(LessThan(Left, Right));
    gttGreater:
      Result := TGocciaBooleanLiteralValue.Create(GreaterThan(Left, Right));
    gttLessEqual:
      Result := TGocciaBooleanLiteralValue.Create(LessThanOrEqual(Left, Right));
    gttGreaterEqual:
      Result := TGocciaBooleanLiteralValue.Create(GreaterThanOrEqual(Left, Right));
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

function EvaluateUnary(UnaryExpression: TGocciaUnaryExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Operand: TGocciaValue;
begin
  // Special handling for delete operator
  if UnaryExpression.Operator = gttDelete then
  begin
    Result := EvaluateDelete(UnaryExpression.Operand, Context);
    Exit;
  end;

  // Special handling for typeof: must not throw for undeclared variables
  if UnaryExpression.Operator = gttTypeof then
  begin
    try
      Operand := EvaluateExpression(UnaryExpression.Operand, Context);
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

  Operand := EvaluateExpression(UnaryExpression.Operand, Context);

  case UnaryExpression.Operator of
    gttNot:
      Result := TGocciaBooleanLiteralValue.Create(not Operand.ToBooleanLiteral.Value);
    gttMinus:
      begin
        // Handle infinity cases first
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
              Result := TGocciaNumberLiteralValue.Create(0.0)  // -(-0) = +0
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
      Result := Operand.ToNumberLiteral;
    gttTypeof:
      Result := EvaluateTypeof(Operand);
    gttBitwiseNot:
      Result := EvaluateBitwiseNot(Operand);
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function EvaluateCall(CallExpression: TGocciaCallExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Callee: TGocciaValue;
  Arguments: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue;
  ArgumentExpr: TGocciaExpression;
  SuperClass: TGocciaClassValue;
  MemberExpr: TGocciaMemberExpression;
  SpreadValue: TGocciaValue;
  CallableIntf: IGocciaCallable;
  I: Integer;
begin

        // Handle super() calls specially
  if CallExpression.Callee is TGocciaSuperExpression then
  begin
    SuperClass := TGocciaClassValue(EvaluateExpression(CallExpression.Callee, Context));
    if not (SuperClass is TGocciaClassValue) then
    begin
      Context.OnError('super() can only be called within a method with a superclass',
        CallExpression.Line, CallExpression.Column);
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;

    Arguments := TGocciaArgumentsCollection.Create;

    try
      for ArgumentExpr in CallExpression.Arguments do
        Arguments.Add(EvaluateExpression(ArgumentExpr, Context));

      // Call the superclass constructor with the current instance as 'this'
      if Assigned(SuperClass.ConstructorMethod) then
      begin
        Result := SuperClass.ConstructorMethod.Call(Arguments, Context.Scope.ThisValue);
      end
      else
      begin
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      end;
    finally
      Arguments.Free;
    end;
    Exit;
  end;

  // Handle method calls vs regular function calls
  if CallExpression.Callee is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(CallExpression.Callee);
    if MemberExpr.ObjectExpr is TGocciaSuperExpression then
    begin
      // Super method calls: evaluate normally
      Callee := EvaluateExpression(CallExpression.Callee, Context);
      ThisValue := Context.Scope.ThisValue;  // Use current instance's 'this'
    end
    else
    begin
      // Regular method calls: use overloaded function to get both method and object
      Callee := EvaluateMember(MemberExpr, Context, ThisValue);
    end;
  end
  else if CallExpression.Callee is TGocciaPrivateMemberExpression then
  begin
    // Private method calls: use overloaded function to get both method and object
    Callee := EvaluatePrivateMember(TGocciaPrivateMemberExpression(CallExpression.Callee), Context, ThisValue);
  end
  else
  begin
    // Regular function calls
    Callee := EvaluateExpression(CallExpression.Callee, Context);
    ThisValue := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;

  Arguments := TGocciaArgumentsCollection.Create;
  try
    for ArgumentExpr in CallExpression.Arguments do
    begin
      if ArgumentExpr is TGocciaSpreadExpression then
      begin
        SpreadValue := EvaluateExpression(TGocciaSpreadExpression(ArgumentExpr).Argument, Context);
        SpreadIterableIntoArgs(SpreadValue, Arguments);
      end
      else
      begin
        Arguments.Add(EvaluateExpression(ArgumentExpr, Context));
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
    else if Supports(Callee, IGocciaCallable, CallableIntf) then
      Result := CallableIntf.Call(Arguments, ThisValue)
    else
    begin
      SafeOnError(Context, Format('%s is not a function', [Callee.TypeName]), CallExpression.Line, CallExpression.Column);
      // If OnError doesn't throw, return undefined
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    end;

  finally
    Arguments.Free;
  end;
end;

function EvaluateMemberCore(MemberExpression: TGocciaMemberExpression; Context: TGocciaEvaluationContext; OutObjectValue: PGocciaValue): TGocciaValue; forward;

function EvaluateMember(MemberExpression: TGocciaMemberExpression; Context: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateMemberCore(MemberExpression, Context, nil);
end;

function EvaluateMember(MemberExpression: TGocciaMemberExpression; Context: TGocciaEvaluationContext; out ObjectValue: TGocciaValue): TGocciaValue;
begin
  Result := EvaluateMemberCore(MemberExpression, Context, @ObjectValue);
end;

function EvaluateMemberCore(MemberExpression: TGocciaMemberExpression; Context: TGocciaEvaluationContext; OutObjectValue: PGocciaValue): TGocciaValue;
var
  Obj: TGocciaValue;
  PropertyName: string;
  PropertyValue: TGocciaValue;
  SuperClass: TGocciaClassValue;
  BoxedValue: TGocciaObjectValue;
begin

  // Handle optional chaining: obj?.prop returns undefined if obj is null/undefined
  if MemberExpression.Optional then
  begin
    Obj := EvaluateExpression(MemberExpression.ObjectExpr, Context);
    if Assigned(OutObjectValue) then
      OutObjectValue^ := Obj;
    if (Obj is TGocciaNullLiteralValue) or (Obj is TGocciaUndefinedLiteralValue) then
    begin
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;
  end;

  // Handle super.method() specially
  if MemberExpression.ObjectExpr is TGocciaSuperExpression then
  begin
    SuperClass := TGocciaClassValue(EvaluateExpression(MemberExpression.ObjectExpr, Context));
    if not (SuperClass is TGocciaClassValue) then
    begin
      Context.OnError('super can only be used within a method with a superclass',
        MemberExpression.Line, MemberExpression.Column);
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      if Assigned(OutObjectValue) then
        OutObjectValue^ := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;

    if Assigned(OutObjectValue) then
      OutObjectValue^ := SuperClass;

    // Get the property name
    if MemberExpression.Computed and Assigned(MemberExpression.PropertyExpression) then
    begin
      PropertyValue := EvaluateExpression(MemberExpression.PropertyExpression, Context);
      PropertyName := PropertyValue.ToStringLiteral.Value;
    end
    else
    begin
      PropertyName := MemberExpression.PropertyName;
    end;

    // Check if we're in a static method context or instance method context
    if Context.Scope.ThisValue is TGocciaClassValue then
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

  Obj := EvaluateExpression(MemberExpression.ObjectExpr, Context);
  if Assigned(OutObjectValue) then
    OutObjectValue^ := Obj;

  // Determine the property name
  if MemberExpression.Computed and Assigned(MemberExpression.PropertyExpression) then
  begin
    // Computed access: evaluate the property expression to get the property name
    PropertyValue := EvaluateExpression(MemberExpression.PropertyExpression, Context);

    // Symbol property access
    if (PropertyValue is TGocciaSymbolValue) and (Obj is TGocciaObjectValue) then
    begin
      Result := TGocciaObjectValue(Obj).GetSymbolProperty(TGocciaSymbolValue(PropertyValue));
      Exit;
    end;

    PropertyName := PropertyValue.ToStringLiteral.Value;
  end
  else
  begin
    // Static access: use the property name directly
    PropertyName := MemberExpression.PropertyName;
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
      Context.OnError('Cannot read property ''' + PropertyName + ''' of ' + Obj.ToStringLiteral.Value,
        MemberExpression.Line, MemberExpression.Column);
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    end
    else
    begin
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    end;
  end;
end;

function EvaluateArray(ArrayExpression: TGocciaArrayExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  I: Integer;
  ElementValue: TGocciaValue;
  SpreadValue: TGocciaValue;
begin
  Arr := TGocciaArrayValue.Create;
  for I := 0 to ArrayExpression.Elements.Count - 1 do
  begin
    if ArrayExpression.Elements[I] is TGocciaHoleExpression then
    begin
      // For holes, add nil to represent the hole
      Arr.Elements.Add(nil);
    end
    else if ArrayExpression.Elements[I] is TGocciaSpreadExpression then
    begin
      SpreadValue := EvaluateExpression(TGocciaSpreadExpression(ArrayExpression.Elements[I]).Argument, Context);
      SpreadIterableInto(SpreadValue, Arr.Elements);
    end
    else
    begin
      ElementValue := EvaluateExpression(ArrayExpression.Elements[I], Context);
      Arr.Elements.Add(ElementValue);
    end;
  end;
  Result := Arr;
end;

function EvaluateObject(ObjectExpression: TGocciaObjectExpression; Context: TGocciaEvaluationContext): TGocciaValue;
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
  for I := 0 to High(ObjectExpression.PropertySourceOrder) do
    begin
      case ObjectExpression.PropertySourceOrder[I].PropertyType of
        pstStatic:
          begin
            // Static property: {key: value}
            PropertyName := ObjectExpression.PropertySourceOrder[I].StaticKey;
            if ObjectExpression.Properties.TryGetValue(PropertyName, PropertyExpression) then
              Obj.DefineProperty(PropertyName, TGocciaPropertyDescriptorData.Create(EvaluateExpression(PropertyExpression, Context), [pfEnumerable, pfConfigurable, pfWritable]));
          end;

        pstComputed:
          begin
            // Computed property or spread: {[expr]: value} or {...obj}
            ComputedPair := ObjectExpression.ComputedPropertiesInOrder[ObjectExpression.PropertySourceOrder[I].ComputedIndex];

            if ComputedPair.Key is TGocciaSpreadExpression then
            begin
              // Spread expression: copy all enumerable properties
              SpreadValue := EvaluateExpression(TGocciaSpreadExpression(ComputedPair.Key).Argument, Context);

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
              PropertyValue := EvaluateExpression(ComputedPair.Key, Context);
              if PropertyValue is TGocciaSymbolValue then
                Obj.AssignSymbolProperty(TGocciaSymbolValue(PropertyValue), EvaluateExpression(ComputedPair.Value, Context))
              else
              begin
                ComputedKey := PropertyValue.ToStringLiteral.Value;
                Obj.DefineProperty(ComputedKey, TGocciaPropertyDescriptorData.Create(EvaluateExpression(ComputedPair.Value, Context), [pfEnumerable, pfConfigurable, pfWritable]));
              end;
            end;
          end;

        pstGetter:
          begin
            // Getter: {get prop() {...}}
            GetterFunction := EvaluateGetter(ObjectExpression.Getters[ObjectExpression.PropertySourceOrder[I].StaticKey], Context);
            // Check if there's already a setter for this property
            ExistingDescriptor := Obj.GetOwnPropertyDescriptor(ObjectExpression.PropertySourceOrder[I].StaticKey);
            if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
               Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter) then
            begin
              // Merge with existing setter
              ExistingSetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter;
              Obj.DefineProperty(ObjectExpression.PropertySourceOrder[I].StaticKey, TGocciaPropertyDescriptorAccessor.Create(GetterFunction, ExistingSetter, [pfEnumerable, pfConfigurable]));
            end
            else
            begin
              // No existing setter, create getter-only descriptor
              Obj.DefineProperty(ObjectExpression.PropertySourceOrder[I].StaticKey, TGocciaPropertyDescriptorAccessor.Create(GetterFunction, nil, [pfEnumerable, pfConfigurable]));
            end;
          end;

        pstSetter:
          begin
            // Setter: {set prop(val) {...}}
            SetterFunction := EvaluateSetter(ObjectExpression.Setters[ObjectExpression.PropertySourceOrder[I].StaticKey], Context);
            // Check if there's already a getter for this property
            ExistingDescriptor := Obj.GetOwnPropertyDescriptor(ObjectExpression.PropertySourceOrder[I].StaticKey);
            if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
               Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter) then
            begin
              // Merge with existing getter
              ExistingGetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter;
              Obj.DefineProperty(ObjectExpression.PropertySourceOrder[I].StaticKey, TGocciaPropertyDescriptorAccessor.Create(ExistingGetter, SetterFunction, [pfEnumerable, pfConfigurable]));
            end
            else
            begin
              // No existing getter, create setter-only descriptor
              Obj.DefineProperty(ObjectExpression.PropertySourceOrder[I].StaticKey, TGocciaPropertyDescriptorAccessor.Create(nil, SetterFunction, [pfEnumerable, pfConfigurable]));
            end;
          end;
      end;
    end;

  Result := Obj;
end;

function EvaluateGetter(GetterExpression: TGocciaGetterExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Statements: TObjectList<TGocciaASTNode>;
  EmptyParameters: TGocciaParameterArray;
begin
  // Getter has no parameters
  SetLength(EmptyParameters, 0);

  Statements := CopyStatementList(TGocciaBlockStatement(GetterExpression.Body).Nodes);

  // Create function with closure scope
  Result := TGocciaFunctionValue.Create(EmptyParameters, Statements, Context.Scope.CreateChild);
end;

function EvaluateSetter(SetterExpression: TGocciaSetterExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Statements: TObjectList<TGocciaASTNode>;
  Parameters: TGocciaParameterArray;
begin
  // Setter has one parameter
  SetLength(Parameters, 1);
  Parameters[0].Name := SetterExpression.Parameter;
  Parameters[0].DefaultValue := nil;

  Statements := CopyStatementList(TGocciaBlockStatement(SetterExpression.Body).Nodes);

  // Create function with closure scope
  Result := TGocciaFunctionValue.Create(Parameters, Statements, Context.Scope.CreateChild);
end;

function EvaluateArrowFunction(ArrowFunctionExpression: TGocciaArrowFunctionExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Statements: TObjectList<TGocciaASTNode>;
begin
  if ArrowFunctionExpression.Body is TGocciaBlockStatement then
    Statements := CopyStatementList(TGocciaBlockStatement(ArrowFunctionExpression.Body).Nodes)
  else
  begin
    // Body is a single expression: (n) => n * 2
    Statements := TObjectList<TGocciaASTNode>.Create(False);
    Statements.Add(ArrowFunctionExpression.Body);
  end;

  // Create function with closure scope - we need to avoid dangling references to temporary call scopes
  // But we also need to preserve access to the current scope's variables
  // The simplest approach: create a child scope that will be persistent
  Result := TGocciaFunctionValue.Create(ArrowFunctionExpression.Parameters, Statements, Context.Scope.CreateChild);
  TGocciaFunctionValue(Result).IsArrow := True;
  TGocciaFunctionValue(Result).IsExpressionBody := not (ArrowFunctionExpression.Body is TGocciaBlockStatement);
end;

function EvaluateBlock(BlockStatement: TGocciaBlockStatement; Context: TGocciaEvaluationContext): TGocciaValue;
var
  I: Integer;
  LastValue: TGocciaValue;
  BlockContext: TGocciaEvaluationContext;
  NeedsChildScope: Boolean;
begin
  // Check if we need a child scope (only if there are variable declarations)
  NeedsChildScope := False;
  for I := 0 to BlockStatement.Nodes.Count - 1 do
  begin
    if (BlockStatement.Nodes[I] is TGocciaVariableDeclaration) or
       (BlockStatement.Nodes[I] is TGocciaDestructuringDeclaration) then
    begin
      NeedsChildScope := True;
      Break;
    end;
  end;

  BlockContext := Context;
  if NeedsChildScope then
    BlockContext.Scope := Context.Scope.CreateChild(skBlock, 'BlockScope')
  else
    BlockContext.Scope := Context.Scope; // Use same scope - no isolation needed

  try
    LastValue := EvaluateStatementsSafe(BlockStatement.Nodes, BlockContext);
    if LastValue = nil then
      LastValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    Result := LastValue;
  finally
    // Only free the scope if we created a child scope
    if NeedsChildScope then
      BlockContext.Scope.Free;
  end;
end;

function EvaluateIf(IfStatement: TGocciaIfStatement; Context: TGocciaEvaluationContext): TGocciaValue;
begin
  if EvaluateExpression(IfStatement.Condition, Context).ToBooleanLiteral.Value then
    Result := EvaluateStatement(IfStatement.Consequent, Context)
  else if Assigned(IfStatement.Alternate) then
    Result := EvaluateStatement(IfStatement.Alternate, Context)
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function ExecuteCatchBlock(TryStatement: TGocciaTryStatement; ErrorValue: TGocciaValue; Context: TGocciaEvaluationContext): TGocciaValue;
var
  CatchScope: TGocciaScope;
  CatchContext: TGocciaEvaluationContext;
begin
  if TryStatement.CatchParam <> '' then
  begin
    CatchScope := TGocciaCatchScope.Create(Context.Scope, TryStatement.CatchParam);
    try
      CatchScope.DefineLexicalBinding(TryStatement.CatchParam, ErrorValue, dtParameter);
      CatchContext := Context;
      CatchContext.Scope := CatchScope;
      Result := EvaluateStatementsSafe(TryStatement.CatchBlock.Nodes, CatchContext);
    finally
      CatchScope.Free;
    end;
  end
  else
    Result := EvaluateStatementsSafe(TryStatement.CatchBlock.Nodes, Context);
end;

function PascalExceptionToErrorObject(E: Exception): TGocciaValue;
begin
  if E is TGocciaTypeError then
    Result := CreateErrorObject('TypeError', E.Message)
  else if E is TGocciaReferenceError then
    Result := CreateErrorObject('ReferenceError', E.Message)
  else
    Result := CreateErrorObject('Error', E.Message);
end;

function EvaluateTry(TryStatement: TGocciaTryStatement; Context: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  try
    try
      Result := EvaluateStatementsSafe(TryStatement.Block.Nodes, Context);
    except
      on E: TGocciaThrowValue do
      begin
        if Assigned(TryStatement.CatchBlock) then
          Result := ExecuteCatchBlock(TryStatement, E.Value, Context)
        else
          raise;
      end;
      on E: TGocciaReturnValue do raise;
      on E: TGocciaBreakSignal do raise;
      on E: Exception do
      begin
        if Assigned(TryStatement.CatchBlock) then
          Result := ExecuteCatchBlock(TryStatement, PascalExceptionToErrorObject(E), Context)
        else
          raise TGocciaThrowValue.Create(PascalExceptionToErrorObject(E));
      end;
    end;
  finally
    if Assigned(TryStatement.FinallyBlock) then
      EvaluateStatementsSafe(TryStatement.FinallyBlock.Nodes, Context);
  end;
end;

function EvaluateClassMethod(ClassMethod: TGocciaClassMethod; Context: TGocciaEvaluationContext; SuperClass: TGocciaValue = nil): TGocciaValue;
var
  Statements: TObjectList<TGocciaASTNode>;
begin
  Statements := CopyStatementList(TGocciaBlockStatement(ClassMethod.Body).Nodes);

  // Always create a unique child scope for the closure - now with default parameter support
  Result := TGocciaMethodValue.Create(ClassMethod.Parameters, Statements, Context.Scope.CreateChild, ClassMethod.Name, SuperClass);
end;

function EvaluateClass(ClassDeclaration: TGocciaClassDeclaration; Context: TGocciaEvaluationContext): TGocciaValue;
var
  ClassDef: TGocciaClassDefinition;
begin
  ClassDef := ClassDeclaration.ClassDefinition;
  Result := EvaluateClassDefinition(ClassDef, Context, ClassDeclaration.Line, ClassDeclaration.Column);

  // For class declarations, bind the class name to the scope
  Context.Scope.DefineLexicalBinding(ClassDef.Name, Result, dtLet);
end;

procedure InitializeInstanceProperties(Instance: TGocciaInstanceValue; ClassValue: TGocciaClassValue; Context: TGocciaEvaluationContext);
var
  PropertyValue: TGocciaValue;
  PropertyExpr: TGocciaExpression;
  I: Integer;
  PropName: string;
begin
  // Initialize inherited properties first (parent to child order)
  if Assigned(ClassValue.SuperClass) then
    InitializeInstanceProperties(Instance, ClassValue.SuperClass, Context);

  // Then initialize this class's properties in declaration order (child properties can shadow parent properties)
  for I := 0 to ClassValue.InstancePropertyOrder.Count - 1 do
  begin
    PropName := ClassValue.InstancePropertyOrder[I];
    if ClassValue.InstancePropertyDefs.TryGetValue(PropName, PropertyExpr) then
    begin
      // Evaluate the property expression in the current context
      PropertyValue := EvaluateExpression(PropertyExpr, Context);
      Instance.AssignProperty(PropName, PropertyValue);
    end;
  end;
end;

function EvaluateSwitch(SwitchStatement: TGocciaSwitchStatement; Context: TGocciaEvaluationContext): TGocciaValue;
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
  Discriminant := EvaluateExpression(SwitchStatement.Discriminant, Context);

  Matched := False;
  DefaultIndex := -1;

  try
    // First pass: find matching case or default
    for I := 0 to SwitchStatement.Cases.Count - 1 do
    begin
      CaseClause := SwitchStatement.Cases[I];

      if not Assigned(CaseClause.Test) then
      begin
        // Remember default case index for later
        DefaultIndex := I;
        Continue;
      end;

      if not Matched then
      begin
        CaseTest := EvaluateExpression(CaseClause.Test, Context);
        if IsStrictEqual(Discriminant, CaseTest) then
          Matched := True;
      end;

      // Once matched, execute this case's statements (fall-through)
      if Matched then
      begin
        for J := 0 to CaseClause.Consequent.Count - 1 do
          Result := Evaluate(CaseClause.Consequent[J], Context);
      end;
    end;

    // If no case matched, execute from default and fall through
    if not Matched and (DefaultIndex >= 0) then
    begin
      for I := DefaultIndex to SwitchStatement.Cases.Count - 1 do
      begin
        CaseClause := SwitchStatement.Cases[I];
        for J := 0 to CaseClause.Consequent.Count - 1 do
          Result := Evaluate(CaseClause.Consequent[J], Context);
      end;
    end;
  except
    on E: TGocciaBreakSignal do
    begin
      // Break exits the switch — swallow the signal
    end;
  end;

end;

function EvaluateNewExpression(NewExpression: TGocciaNewExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Callee: TGocciaValue;
  Arguments: TGocciaArgumentsCollection;
  I: Integer;
  Instance: TGocciaInstanceValue;
  ClassValue: TGocciaClassValue;
  InitContext, SuperInitContext: TGocciaEvaluationContext;
  InitScope, SuperInitScope: TGocciaScope;
begin
  // Evaluate the callee (class)
  Callee := EvaluateExpression(NewExpression.Callee, Context);

  // Evaluate arguments
  Arguments := TGocciaArgumentsCollection.Create;
  try
    for I := 0 to NewExpression.Arguments.Count - 1 do
      Arguments.Add(EvaluateExpression(NewExpression.Arguments[I], Context));

    // Create new instance with proper timing
    if Callee is TGocciaClassValue then
    begin
      ClassValue := TGocciaClassValue(Callee);

      // Step 1: Create basic instance and set prototype
      Instance := TGocciaInstanceValue.Create(ClassValue);
      Instance.Prototype := ClassValue.Prototype;

      // Step 2: Initialize properties BEFORE calling constructor
      // Create an initialization context with `this` bound to the new instance
      InitContext := Context;
      InitScope := Context.Scope.CreateChild(skBlock, 'InstanceInit');
      InitScope.ThisValue := Instance;
      InitScope.DefineLexicalBinding('this', Instance, dtUnknown);
      InitScope.DefineLexicalBinding('__owning_class__', ClassValue, dtUnknown);
      InitContext.Scope := InitScope;

      InitializeInstanceProperties(Instance, ClassValue, InitContext);

      // Step 2.5: Initialize private properties from inheritance chain (simple iteration)
      // Start from the topmost superclass and work down
      if Assigned(ClassValue.SuperClass) then
      begin
        // Create a context with the superclass as owning class for correct composite key resolution
        SuperInitContext := Context;
        SuperInitScope := Context.Scope.CreateChild(skBlock, 'SuperPrivateInit');
        SuperInitScope.ThisValue := Instance;
        SuperInitScope.DefineLexicalBinding('this', Instance, dtUnknown);
        SuperInitScope.DefineLexicalBinding('__owning_class__', ClassValue.SuperClass, dtUnknown);
        SuperInitContext.Scope := SuperInitScope;
        InitializePrivateInstanceProperties(Instance, ClassValue.SuperClass, SuperInitContext);
      end;

      // Step 2.6: Initialize private properties from current class
      InitializePrivateInstanceProperties(Instance, ClassValue, InitContext);

      // Step 3: Call constructor (properties are now available)
      if Assigned(ClassValue.ConstructorMethod) then
        ClassValue.ConstructorMethod.Call(Arguments, Instance)
      else if Assigned(ClassValue.SuperClass) and Assigned(ClassValue.SuperClass.ConstructorMethod) then
        ClassValue.SuperClass.ConstructorMethod.Call(Arguments, Instance);

      Result := Instance;
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

function EvaluateClassExpression(ClassExpression: TGocciaClassExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  ClassDef: TGocciaClassDefinition;
begin
  ClassDef := ClassExpression.ClassDefinition;
  Result := EvaluateClassDefinition(ClassDef, Context, ClassExpression.Line, ClassExpression.Column);

  // For class expressions, don't automatically bind to scope - just return the class value
  // If it's a named class expression, the name is only visible inside the class methods
end;

function EvaluateClassDefinition(ClassDef: TGocciaClassDefinition; Context: TGocciaEvaluationContext; Line, Column: Integer): TGocciaClassValue;
var
  SuperClass: TGocciaClassValue;
  SuperClassValue: TGocciaValue;
  ClassValue: TGocciaClassValue;
  MethodPair: TPair<string, TGocciaClassMethod>;
  PropertyPair: TPair<string, TGocciaExpression>;
  GetterPair: TPair<string, TGocciaGetterExpression>;
  SetterPair: TPair<string, TGocciaSetterExpression>;
  Method: TGocciaMethodValue;
  PropertyValue: TGocciaValue;
  PropertyExpr: TGocciaExpression;
  GetterFunction, SetterFunction: TGocciaFunctionValue;
  ClassName: string;
  I: Integer;
begin
  SuperClass := nil;
  if ClassDef.SuperClass <> '' then
  begin
    SuperClassValue := Context.Scope.GetValue(ClassDef.SuperClass);
    if SuperClassValue = nil then
      Context.OnError(Format('Superclass "%s" not found', [ClassDef.SuperClass]), Line, Column)
    else if not (SuperClassValue is TGocciaClassValue) then
      Context.OnError(Format('Superclass "%s" is not a class (found %s)', [ClassDef.SuperClass, SuperClassValue.TypeName]), Line, Column)
    else
      SuperClass := TGocciaClassValue(SuperClassValue);
  end;

  // Use the class name if provided, otherwise create an anonymous class
  if ClassDef.Name <> '' then
    ClassName := ClassDef.Name
  else
    ClassName := '<anonymous>';

  ClassValue := TGocciaClassValue.Create(ClassName, SuperClass);

  // Handle methods
  for MethodPair in ClassDef.Methods do
  begin
    // Pass superclass directly to method creation
    Method := TGocciaMethodValue(EvaluateClassMethod(MethodPair.Value, Context, SuperClass));
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
  for PropertyPair in ClassDef.StaticProperties do
  begin
    // Evaluate the property value and set it on the class constructor
    PropertyValue := EvaluateExpression(PropertyPair.Value, Context);
    ClassValue.SetProperty(PropertyPair.Key, PropertyValue);
  end;

  // Handle private static properties
  for PropertyPair in ClassDef.PrivateStaticProperties do
  begin
    // Evaluate the property value and set it on the class as private static property
    PropertyValue := EvaluateExpression(PropertyPair.Value, Context);
    ClassValue.AddPrivateStaticProperty(PropertyPair.Key, PropertyValue);
  end;

  // Store instance property definitions on the class in declaration order
  for I := 0 to ClassDef.InstancePropertyOrder.Count - 1 do
  begin
    if ClassDef.InstanceProperties.TryGetValue(ClassDef.InstancePropertyOrder[I], PropertyExpr) then
      ClassValue.AddInstanceProperty(ClassDef.InstancePropertyOrder[I], PropertyExpr);
  end;

  // Store private instance property definitions on the class in declaration order
  for I := 0 to ClassDef.PrivateInstancePropertyOrder.Count - 1 do
  begin
    if ClassDef.PrivateInstanceProperties.TryGetValue(ClassDef.PrivateInstancePropertyOrder[I], PropertyExpr) then
      ClassValue.AddPrivateInstanceProperty(ClassDef.PrivateInstancePropertyOrder[I], PropertyExpr);
  end;

  // Store private methods on the class
  for MethodPair in ClassDef.PrivateMethods do
  begin
    Method := TGocciaMethodValue(EvaluateClassMethod(MethodPair.Value, Context, SuperClass));
    Method.OwningClass := ClassValue;
    ClassValue.AddPrivateMethod(MethodPair.Key, Method);
  end;

  // Handle getters and setters

  for GetterPair in ClassDef.Getters do
  begin
    GetterFunction := TGocciaFunctionValue(EvaluateGetter(GetterPair.Value, Context));
    // Private getters are prefixed with '#' by the parser
    if (Length(GetterPair.Key) > 0) and (GetterPair.Key[1] = '#') then
      ClassValue.AddPrivateGetter(Copy(GetterPair.Key, 2, Length(GetterPair.Key) - 1), GetterFunction)
    else
      ClassValue.AddGetter(GetterPair.Key, GetterFunction);
  end;

  for SetterPair in ClassDef.Setters do
  begin
    SetterFunction := TGocciaFunctionValue(EvaluateSetter(SetterPair.Value, Context));
    // Private setters are prefixed with '#' by the parser
    if (Length(SetterPair.Key) > 0) and (SetterPair.Key[1] = '#') then
      ClassValue.AddPrivateSetter(Copy(SetterPair.Key, 2, Length(SetterPair.Key) - 1), SetterFunction)
    else
      ClassValue.AddSetter(SetterPair.Key, SetterFunction);
  end;

  // For class expressions, don't automatically bind to scope - just return the class value
  // If it's a named class expression, the name is only visible inside the class methods
  Result := ClassValue;
end;

function ResolveOwningClass(Instance: TGocciaInstanceValue; Context: TGocciaEvaluationContext): TGocciaClassValue;
var
  OwningClassValue: TGocciaValue;
begin
  // Try to get the owning class from the scope (set when a method is called or during initialization)
  if Context.Scope.Contains('__owning_class__') then
  begin
    OwningClassValue := Context.Scope.GetValue('__owning_class__');
    if OwningClassValue is TGocciaClassValue then
    begin
      Result := TGocciaClassValue(OwningClassValue);
      Exit;
    end;
  end;
  // Fallback: use the instance's class
  Result := Instance.ClassValue;
end;

function EvaluatePrivateMemberOnInstance(Instance: TGocciaInstanceValue; const PrivateName: string; Context: TGocciaEvaluationContext): TGocciaValue;
var
  AccessClass: TGocciaClassValue;
  GetterFn: TGocciaFunctionValue;
  EmptyArgs: TGocciaArgumentsCollection;
begin
  // Determine the access class from the owning class of the current method
  AccessClass := ResolveOwningClass(Instance, Context);

  // Check if this is a private getter
  if AccessClass.HasPrivateGetter(PrivateName) then
  begin
    GetterFn := AccessClass.GetPrivateGetter(PrivateName);
    EmptyArgs := TGocciaArgumentsCollection.Create;
    try
      Result := GetterFn.Call(EmptyArgs, Instance);
    finally
      EmptyArgs.Free;
    end;
    Exit;
  end;

  // Check if this is a private method call
  if Instance.ClassValue.PrivateMethods.ContainsKey(PrivateName) then
  begin
    Result := Instance.ClassValue.GetPrivateMethod(PrivateName);
    if Result = nil then
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    // It's a private property access
    Result := Instance.GetPrivateProperty(PrivateName, AccessClass);
  end;
end;

function EvaluatePrivateMember(PrivateMemberExpression: TGocciaPrivateMemberExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  ObjectValue: TGocciaValue;
  Instance: TGocciaInstanceValue;
  ClassValue: TGocciaClassValue;
begin
  // Evaluate the object expression
  ObjectValue := EvaluateExpression(PrivateMemberExpression.ObjectExpr, Context);

  if ObjectValue is TGocciaInstanceValue then
  begin
    Instance := TGocciaInstanceValue(ObjectValue);
    Result := EvaluatePrivateMemberOnInstance(Instance, PrivateMemberExpression.PrivateName, Context);
  end
  else if ObjectValue is TGocciaClassValue then
  begin
    // Private static field access on class
    ClassValue := TGocciaClassValue(ObjectValue);

    // Access private static property
    Result := ClassValue.GetPrivateStaticProperty(PrivateMemberExpression.PrivateName);
  end
  else
  begin
    Context.OnError(Format('Private fields can only be accessed on class instances or classes, not %s', [ObjectValue.TypeName]),
      PrivateMemberExpression.Line, PrivateMemberExpression.Column);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function EvaluatePrivateMember(PrivateMemberExpression: TGocciaPrivateMemberExpression; Context: TGocciaEvaluationContext; out ObjectValue: TGocciaValue): TGocciaValue;
var
  Instance: TGocciaInstanceValue;
  ClassValue: TGocciaClassValue;
begin
  // Evaluate the object expression and store it for this binding
  ObjectValue := EvaluateExpression(PrivateMemberExpression.ObjectExpr, Context);

  if ObjectValue is TGocciaInstanceValue then
  begin
    Instance := TGocciaInstanceValue(ObjectValue);
    Result := EvaluatePrivateMemberOnInstance(Instance, PrivateMemberExpression.PrivateName, Context);
  end
  else if ObjectValue is TGocciaClassValue then
  begin
    // Private static field access on class
    ClassValue := TGocciaClassValue(ObjectValue);

    // Access private static property
    Result := ClassValue.GetPrivateStaticProperty(PrivateMemberExpression.PrivateName);
  end
  else
  begin
    Context.OnError(Format('Private fields can only be accessed on class instances or classes, not %s', [ObjectValue.TypeName]),
      PrivateMemberExpression.Line, PrivateMemberExpression.Column);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function EvaluatePrivatePropertyAssignment(PrivatePropertyAssignmentExpression: TGocciaPrivatePropertyAssignmentExpression; Context: TGocciaEvaluationContext): TGocciaValue;
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
  ObjectValue := EvaluateExpression(PrivatePropertyAssignmentExpression.ObjectExpr, Context);

  // Evaluate the value to assign
  Value := EvaluateExpression(PrivatePropertyAssignmentExpression.Value, Context);

  if ObjectValue is TGocciaInstanceValue then
  begin
    // Private field assignment on instance
    Instance := TGocciaInstanceValue(ObjectValue);

    // Determine the access class from the owning class of the current method
    AccessClass := ResolveOwningClass(Instance, Context);

    // Check if this is a private setter
    if AccessClass.HasPrivateSetter(PrivatePropertyAssignmentExpression.PrivateName) then
    begin
      SetterFn := AccessClass.GetPrivateSetter(PrivatePropertyAssignmentExpression.PrivateName);
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
      Instance.SetPrivateProperty(PrivatePropertyAssignmentExpression.PrivateName, Value, AccessClass);
    end;
  end
  else if ObjectValue is TGocciaClassValue then
  begin
    // Private static field assignment on class
    ClassValue := TGocciaClassValue(ObjectValue);

    // Set the private static property
    ClassValue.AddPrivateStaticProperty(PrivatePropertyAssignmentExpression.PrivateName, Value);
  end
  else
  begin
    Context.OnError(Format('Private fields can only be assigned on class instances or classes, not %s', [ObjectValue.TypeName]),
      PrivatePropertyAssignmentExpression.Line, PrivatePropertyAssignmentExpression.Column);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  Result := Value;
end;

function EvaluatePrivatePropertyCompoundAssignment(PrivatePropertyCompoundAssignmentExpression: TGocciaPrivatePropertyCompoundAssignmentExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  ObjectValue: TGocciaValue;
  Instance: TGocciaInstanceValue;
  ClassValue: TGocciaClassValue;
  AccessClass: TGocciaClassValue;
  Value: TGocciaValue;
  CurrentValue: TGocciaValue;
begin
  // Evaluate the object expression
  ObjectValue := EvaluateExpression(PrivatePropertyCompoundAssignmentExpression.ObjectExpr, Context);

  // Evaluate the value to operate with
  Value := EvaluateExpression(PrivatePropertyCompoundAssignmentExpression.Value, Context);

  if ObjectValue is TGocciaInstanceValue then
  begin
    // Private field compound assignment on instance
    Instance := TGocciaInstanceValue(ObjectValue);

    // Determine the access class from the owning class of the current method
    AccessClass := ResolveOwningClass(Instance, Context);

    // Get the current value of the private property
    CurrentValue := Instance.GetPrivateProperty(PrivatePropertyCompoundAssignmentExpression.PrivateName, AccessClass);
  end
  else if ObjectValue is TGocciaClassValue then
  begin
    // Private static field compound assignment on class
    ClassValue := TGocciaClassValue(ObjectValue);

    // Get the current value of the private static property
    CurrentValue := ClassValue.GetPrivateStaticProperty(PrivatePropertyCompoundAssignmentExpression.PrivateName);
  end
  else
  begin
    Context.OnError(Format('Private fields can only be accessed on class instances or classes, not %s', [ObjectValue.TypeName]),
      PrivatePropertyCompoundAssignmentExpression.Line, PrivatePropertyCompoundAssignmentExpression.Column);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  // Use shared compound operation function
  Result := PerformCompoundOperation(CurrentValue, Value, PrivatePropertyCompoundAssignmentExpression.Operator);

  // Set the new value
  if ObjectValue is TGocciaInstanceValue then
    Instance.SetPrivateProperty(PrivatePropertyCompoundAssignmentExpression.PrivateName, Result, AccessClass)
  else if ObjectValue is TGocciaClassValue then
    ClassValue.AddPrivateStaticProperty(PrivatePropertyCompoundAssignmentExpression.PrivateName, Result);
end;

procedure InitializePrivateInstanceProperties(Instance: TGocciaInstanceValue; ClassValue: TGocciaClassValue; Context: TGocciaEvaluationContext);
var
  PropertyValue: TGocciaValue;
  PropertyExpr: TGocciaExpression;
  I: Integer;
  PropName: string;
begin
  // Initialize private instance properties in declaration order (only from the exact class, not inherited)
  for I := 0 to ClassValue.PrivateInstancePropertyOrder.Count - 1 do
  begin
    PropName := ClassValue.PrivateInstancePropertyOrder[I];
    if ClassValue.PrivateInstancePropertyDefs.TryGetValue(PropName, PropertyExpr) then
    begin
      // Evaluate the property expression in the current context
      PropertyValue := EvaluateExpression(PropertyExpr, Context);
      Instance.SetPrivateProperty(PropName, PropertyValue, ClassValue);
    end;
  end;
end;

function EvaluateTemplateLiteral(TemplateLiteralExpression: TGocciaTemplateLiteralExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Template: string;
  SB: TStringBuilder;
  I, Start: Integer;
  BraceCount: Integer;
  ExpressionText: string;
  ExpressionValue: TGocciaValue;
begin
  Template := TemplateLiteralExpression.Value;
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
          Context.OnError('Unterminated template expression', TemplateLiteralExpression.Line, TemplateLiteralExpression.Column);
          Result := TGocciaStringLiteralValue.Create(Template);
          Exit;
        end;

        ExpressionText := Trim(Copy(Template, Start, I - Start));
        if ExpressionText <> '' then
        begin
          ExpressionValue := EvaluateTemplateExpression(ExpressionText, Context, TemplateLiteralExpression.Line, TemplateLiteralExpression.Column);
          if ExpressionValue <> nil then
            SB.Append(ExpressionValue.ToStringLiteral.Value)
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
function EvaluateTemplateExpression(const ExpressionText: string; Context: TGocciaEvaluationContext; Line, Column: Integer): TGocciaValue;
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
  Trimmed := Trim(ExpressionText);

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
      if Trimmed = 'true' then
      begin
        Result := TGocciaBooleanLiteralValue.TrueValue;
        Exit;
      end
      else if Trimmed = 'false' then
      begin
        Result := TGocciaBooleanLiteralValue.FalseValue;
        Exit;
      end
      else if Trimmed = 'null' then
      begin
        Result := TGocciaNullLiteralValue.Create;
        Exit;
      end
      else if Trimmed = 'undefined' then
      begin
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
        Exit;
      end
      else if Trimmed = 'NaN' then
      begin
        Result := TGocciaNumberLiteralValue.NaNValue;
        Exit;
      end
      else if Trimmed = 'Infinity' then
      begin
        Result := TGocciaNumberLiteralValue.InfinityValue;
        Exit;
      end;

      Result := Context.Scope.GetValue(Trimmed);
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

      ObjVal := Context.Scope.GetValue(Left);
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
        LeftVal := Context.Scope.GetValue(Left);
        RightVal := Context.Scope.GetValue(Right);

        if (LeftVal <> nil) and (RightVal <> nil) then
        begin
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
        LeftVal := Context.Scope.GetValue(Left);
        RightVal := Context.Scope.GetValue(Right);

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
        LeftVal := Context.Scope.GetValue(Left);
        RightVal := Context.Scope.GetValue(Right);

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
        LeftVal := Context.Scope.GetValue(Left);
        RightVal := Context.Scope.GetValue(Right);

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
    SourceLines.Add(ExpressionText);

    Lexer := TGocciaLexer.Create(ExpressionText, 'template-expression');
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

    Result := EvaluateExpression(Expression, Context);

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

function EvaluateDestructuringAssignment(DestructuringAssignmentExpression: TGocciaDestructuringAssignmentExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Value: TGocciaValue;
begin
  // Evaluate the right-hand side
  Value := EvaluateExpression(DestructuringAssignmentExpression.Right, Context);

  // Apply the destructuring pattern
  AssignPattern(DestructuringAssignmentExpression.Left, Value, Context);

  Result := Value;
end;

function EvaluateDestructuringDeclaration(DestructuringDeclaration: TGocciaDestructuringDeclaration; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Value: TGocciaValue;
begin
  // Evaluate the initializer
  Value := EvaluateExpression(DestructuringDeclaration.Initializer, Context);

  // Apply the destructuring pattern to declare variables
  AssignPattern(DestructuringDeclaration.Pattern, Value, Context, True); // IsDeclaration = True

  Result := Value;
end;

procedure AssignPattern(Pattern: TGocciaDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext; IsDeclaration: Boolean = False);
begin
  if Pattern is TGocciaIdentifierDestructuringPattern then
    AssignIdentifierPattern(TGocciaIdentifierDestructuringPattern(Pattern), Value, Context, IsDeclaration)
  else if Pattern is TGocciaArrayDestructuringPattern then
    AssignArrayPattern(TGocciaArrayDestructuringPattern(Pattern), Value, Context, IsDeclaration)
  else if Pattern is TGocciaObjectDestructuringPattern then
    AssignObjectPattern(TGocciaObjectDestructuringPattern(Pattern), Value, Context, IsDeclaration)
  else if Pattern is TGocciaAssignmentDestructuringPattern then
    AssignAssignmentPattern(TGocciaAssignmentDestructuringPattern(Pattern), Value, Context, IsDeclaration)
  else if Pattern is TGocciaRestDestructuringPattern then
    AssignRestPattern(TGocciaRestDestructuringPattern(Pattern), Value, Context, IsDeclaration);
end;

procedure AssignIdentifierPattern(Pattern: TGocciaIdentifierDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext; IsDeclaration: Boolean = False);
begin
  if IsDeclaration then
    Context.Scope.DefineLexicalBinding(Pattern.Name, Value, dtLet)
  else
    Context.Scope.AssignLexicalBinding(Pattern.Name, Value);
end;

procedure AssignArrayPattern(Pattern: TGocciaArrayDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext; IsDeclaration: Boolean = False);
var
  ArrayValue: TGocciaArrayValue;
  I: Integer;
  ElementValue: TGocciaValue;
  RestElements: TGocciaArrayValue;
  J: Integer;
begin
  // Check if value is iterable (array or string)
  if Value is TGocciaArrayValue then
  begin
    ArrayValue := TGocciaArrayValue(Value);

    for I := 0 to Pattern.Elements.Count - 1 do
    begin
      if Pattern.Elements[I] = nil then
        Continue; // Skip holes

      if Pattern.Elements[I] is TGocciaRestDestructuringPattern then
      begin
        // Rest pattern: collect remaining elements
        RestElements := TGocciaArrayValue.Create;
        for J := I to ArrayValue.Elements.Count - 1 do
        begin
          if ArrayValue.Elements[J] <> nil then
            RestElements.Elements.Add(ArrayValue.Elements[J])
          else
            RestElements.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue);
        end;
        AssignPattern(TGocciaRestDestructuringPattern(Pattern.Elements[I]).Argument, RestElements, Context, IsDeclaration);
        Break;
      end
      else
      begin
        // Regular element
        if (I < ArrayValue.Elements.Count) and (ArrayValue.Elements[I] <> nil) then
          ElementValue := ArrayValue.Elements[I]
        else
          ElementValue := TGocciaUndefinedLiteralValue.UndefinedValue;

        AssignPattern(Pattern.Elements[I], ElementValue, Context, IsDeclaration);
      end;
    end;
  end
  else if Value is TGocciaStringLiteralValue then
  begin
    // String destructuring
    for I := 0 to Pattern.Elements.Count - 1 do
    begin
      if Pattern.Elements[I] = nil then
        Continue; // Skip holes

      if Pattern.Elements[I] is TGocciaRestDestructuringPattern then
      begin
        // Rest pattern: collect remaining characters
        RestElements := TGocciaArrayValue.Create;
        for J := I + 1 to Length(Value.ToStringLiteral.Value) do
          RestElements.Elements.Add(TGocciaStringLiteralValue.Create(Value.ToStringLiteral.Value[J]));
        AssignPattern(TGocciaRestDestructuringPattern(Pattern.Elements[I]).Argument, RestElements, Context, IsDeclaration);
        Break;
      end
      else
      begin
        // Regular character
        if I + 1 <= Length(Value.ToStringLiteral.Value) then
          ElementValue := TGocciaStringLiteralValue.Create(Value.ToStringLiteral.Value[I + 1])
        else
          ElementValue := TGocciaUndefinedLiteralValue.UndefinedValue;

        AssignPattern(Pattern.Elements[I], ElementValue, Context, IsDeclaration);
      end;
    end;
  end
  else if (Value is TGocciaNullLiteralValue) or (Value is TGocciaUndefinedLiteralValue) then
  begin
    ThrowTypeError('Cannot destructure null or undefined');
  end
  else
  begin
    ThrowTypeError('Value is not iterable');
  end;
end;

procedure AssignObjectPattern(Pattern: TGocciaObjectDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext; IsDeclaration: Boolean = False);
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
  if not (Value is TGocciaObjectValue) then
  begin
    if (Value is TGocciaNullLiteralValue) or (Value is TGocciaUndefinedLiteralValue) then
      ThrowTypeError('Cannot destructure null or undefined')
    else
      ThrowTypeError('Cannot destructure non-object value');
  end;

  ObjectValue := TGocciaObjectValue(Value);
  UsedKeys := TStringList.Create;

  try
    // Use indexed for loop to ensure properties are processed in source order
    for I := 0 to Pattern.Properties.Count - 1 do
    begin
      Prop := Pattern.Properties[I];
      if Prop.Pattern is TGocciaRestDestructuringPattern then
      begin
        // Rest pattern: collect remaining properties
        RestObject := TGocciaObjectValue.Create;
        for Key in ObjectValue.GetEnumerablePropertyNames do
        begin
          if UsedKeys.IndexOf(Key) = -1 then
            RestObject.AssignProperty(Key, ObjectValue.GetProperty(Key));
        end;
        AssignPattern(TGocciaRestDestructuringPattern(Prop.Pattern).Argument, RestObject, Context, IsDeclaration);
      end
      else
      begin
        // Regular property
        if Prop.Computed then
        begin
          // Computed property key (may be a symbol)
          PropValue := EvaluateExpression(Prop.KeyExpression, Context);
          if PropValue is TGocciaSymbolValue then
          begin
            PropValue := ObjectValue.GetSymbolProperty(TGocciaSymbolValue(PropValue));
            AssignPattern(Prop.Pattern, PropValue, Context, IsDeclaration);
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
        AssignPattern(Prop.Pattern, PropValue, Context, IsDeclaration);
      end;
    end;
  finally
    UsedKeys.Free;
  end;
end;

procedure AssignAssignmentPattern(Pattern: TGocciaAssignmentDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext; IsDeclaration: Boolean = False);
var
  DefaultValue: TGocciaValue;
begin
  // Use default value if the value is undefined
  if Value is TGocciaUndefinedLiteralValue then
  begin
    DefaultValue := EvaluateExpression(Pattern.Right, Context);
    AssignPattern(Pattern.Left, DefaultValue, Context, IsDeclaration);
  end
  else
  begin
    AssignPattern(Pattern.Left, Value, Context, IsDeclaration);
  end;
end;

procedure AssignRestPattern(Pattern: TGocciaRestDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext; IsDeclaration: Boolean = False);
begin
  // Rest patterns are handled by their containing array/object patterns
  AssignPattern(Pattern.Argument, Value, Context, IsDeclaration);
end;

function IsObjectInstanceOfClass(Obj: TGocciaObjectValue; ClassValue: TGocciaClassValue): Boolean;
var
  CurrentPrototype: TGocciaObjectValue;
  TargetPrototype: TGocciaObjectValue;
begin
  Result := False;

  // Get the target prototype we're looking for
  TargetPrototype := ClassValue.Prototype;
  if not Assigned(TargetPrototype) then
    Exit;

  // Walk up the prototype chain of the object
  CurrentPrototype := Obj.Prototype;
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

function EvaluateDelete(Operand: TGocciaExpression; Context: TGocciaEvaluationContext): TGocciaValue;
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
  if Operand is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(Operand);
    ObjValue := EvaluateExpression(MemberExpr.ObjectExpr, Context);

    // Get property name
    if MemberExpr.Computed and Assigned(MemberExpr.PropertyExpression) then
    begin
      Value := EvaluateExpression(MemberExpr.PropertyExpression, Context);
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
        // Set array element to nil (creates a hole/sparse array)
        ArrayValue.Elements[Index] := nil;
        Result := TGocciaBooleanLiteralValue.Create(True);
      end
      else
      begin
        // Try to delete as a regular property on the array object
        ArrayValue.DeleteProperty(PropertyName);
        Result := TGocciaBooleanLiteralValue.Create(True);
      end;
    end
    // Handle object property deletion
    else if ObjValue is TGocciaObjectValue then
    begin
      ObjectValue := TGocciaObjectValue(ObjValue);
      // DeleteProperty returns False for non-configurable properties, True otherwise
      Result := TGocciaBooleanLiteralValue.Create(ObjectValue.DeleteProperty(PropertyName));
    end
    else
    begin
      // Cannot delete properties from primitives, null, or undefined
      Result := TGocciaBooleanLiteralValue.Create(True);
    end;
  end
  else
  begin
    // Handle other expressions according to strict mode semantics
    if Operand is TGocciaIdentifierExpression then
    begin
      // In strict mode, attempting to delete variables/identifiers throws TypeError
      Context.OnError('Delete of an unqualified identifier in strict mode',
        Operand.Line, Operand.Column);
      Result := TGocciaBooleanLiteralValue.Create(True); // Fallback if OnError doesn't throw
    end
    else
    begin
      // For literals and other non-reference expressions, return true
      // (e.g., delete 5, delete "string", delete (1+2), etc.)
      Result := TGocciaBooleanLiteralValue.Create(True);
    end;
  end;
end;

end.
